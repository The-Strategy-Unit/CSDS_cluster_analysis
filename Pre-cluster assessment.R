# Pre-cluster assessment

library(tidyverse)
library(janitor)
library(sparklyr)
library(dplyr)
library(dbplyr)
library(DBI)
library(sconn)

# Connect to databricks ----
sc <- sc()

# Read in key measures data ----

# representative sample
key_measures <-
  read_csv("key_measures_rep_sub_sample.csv") |>
  pivot_longer(-person_id) |>
  mutate(value = as.numeric(value)) |>
  pivot_wider(id_cols = person_id)



# Remove date fields and replace NA's where appropriate

key_measures_reduced <-
  key_measures |>
  mutate(
    intermittent_care_periods = ifelse(is.na(intermittent_care_periods), 0, intermittent_care_periods),
    acute_admissions_2223 = ifelse(is.na(acute_admissions_2223), 0, acute_admissions_2223)
  ) |>
  anti_join(
    key_measures |>
      group_by(person_id) |>
      summarise(n = n()) |>
      filter(n > 1),
    by = "person_id"
  )


# Clean names for graphs
key_measure_lookup <-
  tribble(
    ~variable, ~variable_clean,
    "person_id"                 , "0. Person ID",
    "care_contacts"             , "A. Care contacts",
    "referrals"                 , "B. Referrals",
    "length_care"               , "C. Care length",
    "average_daily_contacts"    , "D. Average daily contacts",
    "contact_prop_aft_midpoint" , "E. Contact proportion after midpoint",
    "team_input_count"          , "F. Team input count",
    "intermittent_care_periods" , "G. Intermittent care periods",
    "specialist_contact_prop"   , "H. Specialist care proportion",
    "remote_contact_prop"       , "I. Remote care proportion",
    "avg_contact_duration"      , "J. Average contact duration",
    "acute_admissions_2223"     , "K. Acute admission count"
  )


#1. Check for Missing Values ----
## Identify and handle missing values, as they can affect the clustering process.
sum(is.na(key_measures_reduced))

skimr::skim(key_measures_reduced)

# Calculate the probability distribution
## Team input
prob_distribution_team <-
  key_measures_reduced |>
  group_by(org_id_provider, team_input_count) |>
  summarise(n = n()) |>
  filter(!is.na(team_input_count)) |>
  mutate(probability = n / sum(n),
         team_count = max(team_input_count)) |>
  rename(field = team_input_count)

## Overall probability distribution for team input (ungrouped by org_id)
overall_prob_distribution_team <-
  key_measures_reduced |>
  group_by(team_input_count) |>
  summarise(n = n()) |>
  filter(!is.na(team_input_count)) |>
  mutate(probability = n / sum(n)) |>
  rename(field = team_input_count)

## Avg contact duration
prob_distribution_duration <-
  key_measures_reduced |>
  group_by(avg_contact_duration) |>
  summarise(n = n()) |>
  filter(!is.na(avg_contact_duration)) |>
  mutate(probability = n / sum(n)) |>
  rename(field = 1)

# Function to impute missing values for avg_contact_duration
impute_avg_contact_duration <- function(series, distribution_df) {
  na_indices <- which(is.na(series))
  sampled_values <- sample(distribution_df$field, size = length(na_indices), replace = TRUE, prob = distribution_df$probability)
  series[na_indices] <- sampled_values
  return(series)
}

# Function to impute missing values for team_input_count
impute_team_input_count <- function(series, distribution_df, overall_distribution_df, org_id_provider) {
  na_indices <- which(is.na(series))
  sampled_values <- vector("numeric", length(na_indices))

  for (i in seq_along(na_indices)) {
    org_id <- org_id_provider[na_indices[i]]
    subset_df <- distribution_df[distribution_df$org_id_provider == org_id, ]

    sampled_values[i] <- if (nrow(subset_df) > 1) {
      sample(subset_df$field, size = 1, replace = TRUE, prob = subset_df$probability)
    } else if (nrow(subset_df) == 1) {
      subset_df$field
    } else {
      sample(overall_distribution_df$field, size = 1, replace = TRUE, prob = overall_distribution_df$probability)
    }
  }

  series[na_indices] <- sampled_values
  return(series)
}

# Impute NA values
key_measures_reduced_imp <-
  key_measures_reduced |>
  mutate(
    team_input_count = impute_team_input_count(team_input_count, prob_distribution_team, overall_prob_distribution_team, org_id_provider),
    avg_contact_duration = impute_avg_contact_duration(avg_contact_duration, prob_distribution_duration)
  )

# Check:
sum(is.na(key_measures_reduced_imp))

#write_csv(key_measures_reduced_imp, "key_measures_reduced_imp.csv")


#2. Examine Data Distribution ----
## Understand the distribution of each variable. Skewed distributions might need transformation.

### Read in from files if wanting to skip above spark connect stuff
key_measures_reduced_imp <- read_csv("key_measures_reduced_imp.csv")

summary(key_measures_reduced_imp)

skimr::skim(key_measures_reduced_imp)


key_measures_reduced_imp |>
#key_measures_reduced |>
  #select(-org_id_provider) |>
  pivot_longer(cols = -person_id) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>

  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~variable_clean, scales = "free") +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = NA, colour = "grey"),
    strip.text = element_text(face = "bold")
    ) +
  labs(title = "Distribution of key measures",
       subtitle = "CSDS Pre-cluster analysis assessment")

# Do I need to transform skewed variables?

#3. Outlier Detection ----
## Identify and decide how to handle outliers, as they can distort cluster formation.

remove_outliers <-
  key_measures_reduced_imp |>
  #key_measures_reduced |>
  #select(-org_id_provider) |>
  pivot_longer(cols = -person_id) |>
  group_by(name) |>
  mutate(quantile_99 = quantile(value, 0.99)) |>
  ungroup() |>
  filter(value > quantile_99) |>  # remove values above 99th percentile
  select(person_id) |>
  distinct()


key_measures_reduced_imp_ex_outlier <-
  key_measures_reduced_imp |>
  #select(-org_id_provider) |>
  anti_join(remove_outliers, by = "person_id")

# write for cluster analysis
write.csv(
  key_measures_reduced_imp_ex_outlier,
  "key_measures_sample_cluster.csv"
)


key_measures_reduced_imp_ex_outlier |>
  pivot_longer(cols = -person_id) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~variable_clean, scales = "free") +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = NA, colour = "grey"),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "Distribution of key measures",
       subtitle = "CSDS Pre-cluster analysis assessment")


#4. Correlation Analysis ----
## Check for highly correlated variables. Highly correlated variables might need to be removed or combined.

cor_matrix <- cor(key_measures_reduced_imp_ex_outlier[, -1])  # Exclude person_id

corrplot::corrplot(
  cor_matrix,
  method = "square",
  type = "upper",
  tl.col = "black"
)


# pull in clean variable names
cor_matrix_2 <-
  cor(
    key_measures_reduced_imp_ex_outlier |>
      pivot_longer(-person_id) |>
      left_join(key_measure_lookup, by = c("name" = "variable")) |>
      select(-name) |>
      pivot_wider(id_cols = "person_id",
                  names_from = "variable_clean",
                  values_from = "value") |>
      select(-person_id)
    )  # Exclude person_id

corrplot::corrplot(
  cor_matrix_2,
  method = "square",
  type = "lower",
  tl.col = "black"
)


#5. Standardize/Normalize Data ----
## Standardize variables to have a mean of 0 and a standard deviation of 1, especially if they are on different scales.
key_measures_scaled <- scale(key_measures_reduced_imp_ex_outlier[, -1])  # Exclude person_id

key_measures_scaled_long <-
  key_measures_reduced_imp_ex_outlier |>
  pivot_longer(-person_id) |>
  group_by(name) |>
  mutate(value_scale = scale(value)) |>
  select(person_id, name, value_scale) |>
  mutate(value_scale = as.numeric(value_scale))

key_measures_scaled_wide <-
  key_measures_scaled_long |>
  pivot_wider(id_cols = person_id,
              names_from = name,
              values_from = value_scale)


write_csv(key_measures_scaled_long, "key_measures_scaled.csv")
write_csv(key_measures_scaled_wide, "key_measures_scaled_wide.csv")

key_measures_reduced_imp_ex_outlier |>
  pivot_longer(-person_id) |>
  group_by(name) |>
  mutate(value_scale = scale(value)) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>

  ggplot(aes(x = value_scale)) +
  geom_density() +
  facet_wrap(~variable_clean, scales = "free") +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = NA, colour = "grey"),
    strip.text = element_text(face = "bold")
  ) +
  labs(title = "Distribution of key measures - scaled",
       subtitle = "CSDS Pre-cluster analysis assessment")


#6. Assess Multicollinearity ----
## Check for multicollinearity, which can affect the clustering results.

vif_results <- car::vif(lm(care_contacts ~ ., data = key_measures_reduced_imp_ex_outlier[, -1]))  # Example using VIF

#7. Dimensionality Reduction ----
## Consider using PCA (Principal Component Analysis) to reduce dimensionality if you have many variables.
pca_result <- prcomp(key_measures_reduced_imp_ex_outlier |> select(-person_id), center = TRUE, scale. = TRUE)
summary(pca_result)

screeplot(pca_result, type = "lines")

print(pca_result$rotation)

biplot(pca_result)



# Save the data table to Databricks & disconnect ----

# Write imputed data to databricks
dbGetQuery(sc, "USE strategyunit.csds_al") # Set the schema context
spark_key_measures_reduced_imp <- copy_to(sc, key_measures_reduced_imp, "key_measures_reduced_imp", overwrite = TRUE) # Copy the data frame to Spark
spark_write_table(spark_key_measures_reduced_imp, "strategyunit.csds_al.key_measures_reduced_imp")

# Write scaled long data to databricks
dbGetQuery(sc, "USE strategyunit.csds_al")
spark_key_measures_scaled_wide <- copy_to(sc, key_measures_scaled_wide, "key_measures_scaled_wide", overwrite = TRUE)
spark_write_table(spark_key_measures_scaled_wide, "strategyunit.csds_al.key_measures_scaled_wide")


# Disconnect
sc_disconnect()

