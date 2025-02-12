# Perform cluster analysis

library(tidyverse)
library(GGally)
library(factoextra)
library(plotly)
library(tidyLPA)
library(mclust)
library(cluster)
library(clusterSim) # For Davies-Bouldin index

# Approach: ----

1. **Determine the Number of Clusters**:
   - Use the **Elbow Method**: Plot the total within-cluster sum of squares against the number of clusters and look for an "elbow" point.
   - Use the **Silhouette Method**: Calculate the silhouette coefficient for different numbers of clusters and choose the number that maximizes the silhouette score.
   - Use the **Gap Statistic**: Compare the total within intra-cluster variation for different numbers of clusters with their expected values under null reference distribution of the data.

2. **Choose the Clustering Algorithm**:
   - **K-means**: Suitable for large datasets and when clusters are spherical.
   - **K-medoids**: More robust to noise and outliers compared to K-means.
   - **Hierarchical Clustering**: Useful for smaller datasets and when you want a dendrogram to visualize the clustering process.
   - **Model-Based Clustering**: Assumes data is generated from a mixture of distributions and can handle more complex cluster shapes.

# Data Transformation and Normalization ----

key_measures_scaled <-
  read_csv("key_measures_scaled.csv") |>
  #data.table::as.data.table()
  pivot_wider(id_cols = person_id,
              names_from = name,
              values_from = value_scale)

key_measures_scaled_head <-
  key_measures_scaled |>
  head(10000) |>
  select(-person_id)

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


# Step 1: Performing K-means Clustering in R ----

## 1. **Determine the Optimal Number of Clusters**:
fviz_nbclust(key_measures_scaled_head, kmeans, method = "wss")  # Elbow Method
fviz_nbclust(key_measures_scaled_head, kmeans, method = "silhouette")  # Silhouette Method


## 2. **Run K-means Clustering**:
set.seed(123)  # For reproducibility
kmeans_result_4 <-  kmeans(key_measures_scaled_head, centers = 4, nstart = 100, iter.max = 100)   # Assuming 4 clusters
kmeans_result_5 <-  kmeans(key_measures_scaled_head, centers = 5, nstart = 100, iter.max = 100)   # Assuming 5 clusters
kmeans_result_6 <-  kmeans(key_measures_scaled_head, centers = 6, nstart = 100, iter.max = 100)   # Assuming 6 clusters
kmeans_result_7 <-  kmeans(key_measures_scaled_head, centers = 7, nstart = 100, iter.max = 1000)   # Assuming 7 clusters
kmeans_result_8 <-  kmeans(key_measures_scaled_head, centers = 8, nstart = 100, iter.max = 100)   # Assuming 8 clusters
kmeans_result_9 <-  kmeans(key_measures_scaled_head, centers = 9, nstart = 100, iter.max = 100)   # Assuming 9 clusters
kmeans_result_10 <- kmeans(key_measures_scaled_head, centers = 10, nstart = 100, iter.max = 100)  # Assuming 10 clusters
kmeans_result_11 <- kmeans(key_measures_scaled_head, centers = 11, nstart = 100, iter.max = 100)  # Assuming 11 clusters
kmeans_result_12 <- kmeans(key_measures_scaled_head, centers = 12, nstart = 100, iter.max = 100)  # Assuming 12 clusters


# K-means goodness of fit
## The total within-cluster sum of squares (WSS) measures the compactness of the clusters. Lower values indicate better fit.
## The more clusters derived, the lower the sum of squares
## Trade off between fit and model simplicity?

link_model_output <- function(model, model_id) {

  model$tot.withinss |>
    tibble() |>
    rename(tot.withinss = 1) |>
    mutate(id = model_id)
}

link_model_output(kmeans_result_4, "clusters_4") |>
  union_all(link_model_output(kmeans_result_5 , "clusters_5")) |>
  union_all(link_model_output(kmeans_result_6 , "clusters_6")) |>
  union_all(link_model_output(kmeans_result_7 , "clusters_7")) |>
  union_all(link_model_output(kmeans_result_8 , "clusters_8")) |>
  union_all(link_model_output(kmeans_result_9 , "clusters_9")) |>
  union_all(link_model_output(kmeans_result_10, "clusters_10")) |>
  union_all(link_model_output(kmeans_result_11, "clusters_11")) |>
  union_all(link_model_output(kmeans_result_12, "clusters_12")) |>
  mutate(rn = row_number()) |>

  ggplot(aes(x = reorder(id, rn), y = tot.withinss)) +
  geom_col(width = 0.01) +
  geom_point(size = 5) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "n_clusters",
       y = "Total within-cluster sum of squares (WSS)",
       title = "Comparing sum of squares Goodness of Fit measure",
       subtitle = "K-means cluster models"
       )

## 3. **Visualize the Clusters**:
fviz_cluster(kmeans_result_7,
             data = key_measures_scaled_head,
             geom = "point",
             #ellipse.level = 99,
             ellipse.alpha = 0.1,
             pointsize = 0.5
             ) +
  #geom_point(aes(shape = cluster), size = 1, alpha = 0.2) +
  labs(title = "K-means Clustering Results",
     x = "Variable 1",
     y = "Variable 2") +
  theme_minimal()


## 4. Pairwise Scatter Plot Matrix
# Perform Principal Component Analysis (PCA)
pca_result <-
  prcomp(key_measures_scaled_head, scale. = TRUE)

# Create a dataframe with the principal components
pca_data <-
  as.data.frame(pca_result$x) |>
  mutate(cluster = as.factor(kmeans_result_7$cluster)) # Add cluster assignments to the dataframe

# Create pairwise scatter plot matrix
ggpairs(pca_data, aes(color = cluster, alpha = 0.5))


## 5. 3D plot of top 3 principle components
# Create a dataframe with the first three principal components
pca_data_3d <-
  pca_data[, 1:3] |>
  mutate(cluster = as.factor(kmeans_result_7$cluster))

pca_data_3d$cluster <- as.factor(kmeans_result_7$cluster)

# Create 3D plot
plot_ly(pca_data_3d,
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        color = ~cluster, colors = c('#1f77b4', '#ff7f0e', '#2ca02c')) %>%
  add_markers(size =) %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = '3D Plot of Principal Components')



# Plot classifications
kmeans_classification_means <-
  bind_cols(key_measures_scaled_head, classification = kmeans_result$cluster) |>
  group_by(classification) |>
  summarise_all(funs(mean)) |>
  mutate(classification = paste0("Profile ", 1:7)) |>
  mutate_at(vars(-classification), function(x) round(x, 3)) |>
  rename(profile = classification)

kmeans_classification_means |>
  pivot_longer(cols = -profile) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>
  ggplot(aes(x = variable_clean, y = value, fill = variable_clean)) +
  geom_col() +
  facet_wrap(~profile, scale = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Variable",
       y = "Mean Zscore",
       title = "Mean Zscore by variable and cluster/profile",
       subtitle = "K-means cluster analysis - 7 clusters")


# Step 2: Performing Model-Based Clustering ----

#library(forcats)


https://jihongzhang.org/posts/2017-11-23-latent-profile-analysis/

https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html


lpa_model_lookup <-
  tribble(
    ~model, ~model_name,

    "EII", "spherical, equal volume",
    "VII", "spherical, unequal volume",
    "EEI", "diagonal, equal volume and shape",
    "VEI", "diagonal, varying volume, equal shape",
    "EVI", "diagonal, equal volume, varying shape",
    "VVI", "diagonal, varying volume and shape",
    "EEE", "ellipsoidal, equal volume, shape, and orientation",
    "VEE", "ellipsoidal, equal shape and orientation",
    "EVE", "ellipsoidal, equal volume and orientation",
    "VVE", "ellipsoidal, equal orientation",
    "EEV", "ellipsoidal, equal volume and equal shape",
    "VEV", "ellipsoidal, equal shape",
    "EVV", "ellipsoidal, equal volume",
    "VVV", "ellipsoidal, varying volume, shape, and orientation",
    "X"  , "univariate normal",
    "XII", "spherical multivariate normal",
    "XXI", "diagonal multivariate normal",
    "XXX", "ellipsoidal multivariate normal"
    )


explore_model_fit <- function(df,
                              n_profiles_range = 1:12,
                              model_names = c("EII","VII","EEI","VEI","EVI","VVI","EEE",
                                              "VEE","EVE","VVE","EEV","VEV","EVV","VVV"
                                              )) {
  set.seed(123)
  x <- mclustBIC(df, G = n_profiles_range, modelNames = model_names)
  y <- x |>
    as.data.frame.matrix() |>
    rownames_to_column("n_profiles") |>
    rename(
      `EII: spherical, equal volume`                             = EII,
      `VII: spherical, unequal volume`                           = VII,
      `EEI: diagonal, equal volume and shape`                    = EEI,
      `VEI: diagonal, varying volume, equal shape`               = VEI,
      `EVI: diagonal, equal volume, varying shape`               = EVI,
      `VVI: diagonal, varying volume and shape`                  = VVI,
      `EEE: ellipsoidal, equal volume, shape, and orientation`   = EEE,
      `VEE: ellipsoidal, equal shape and orientation`            = VEE,
      `EVE: ellipsoidal, equal volume and orientation`           = EVE,
      `VVE: ellipsoidal, equal orientation`                      = VVE,
      `EEV: ellipsoidal, equal volume and equal shape`           = EEV,
      `VEV: ellipsoidal, equal shape`                            = VEV,
      `EVV: ellipsoidal, equal volume`                           = EVV,
      `VVV: ellipsoidal, varying volume, shape, and orientation` = VVV
      )
  y
}

fit_output      <- explore_model_fit(key_measures_scaled_head, n_profiles_range = 1:12)
#fit_output_full <- explore_model_fit(key_measures_scaled,      n_profiles_range = 1:12)

# Elbow plot
to_plot <-
  fit_output |>
  gather(`Covariance matrix structure`, val, -n_profiles) |>
  mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
         val = abs(val)) # this is to make the BIC values positive (to align with more common formula / interpretation of BIC)

to_plot |>
  tibble() |>
  mutate(n_profiles = as.numeric(n_profiles)) |>

  ggplot(aes(x = n_profiles,
             y = val,
             color = `Covariance matrix structure`,
             group = `Covariance matrix structure`,
             #label = `Covariance matrix structure`
             )) +
  geom_line() +
  geom_point() +
  #geom_label() +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_minimal() +
  labs(x = "No. clusters",
       y = "BIC (smaller value is better)",
       title = "Comparing LPA model and number of cluster selection",
       subtitle = "Model-based clustering - LPA")




# Write model manually
selected_model_eev_6 <- Mclust(key_measures_scaled_head, G = 6, modelNames = "EEV")
print(summary(selected_model_eev_6))

selected_model_eev_6_full <- Mclust(key_measures_scaled, G = 6, modelNames = "EEV")
print(summary(selected_model_eev_6_full))

# Plot model classifications
dff <- bind_cols(key_measures_scaled_head, classification = selected_model_eev_6$classification)

proc_df <-
  dff |>
  mutate_at(vars(-classification), scale) |>
  group_by(classification) |>
  summarise_all(funs(mean)) |>
  mutate(classification = paste0("Profile ", 1:6)) |>
  mutate_at(vars(-classification), function(x) round(x, 3)) |>
  rename(profile = classification)


proc_df |>
  gather(key, val, -profile) |>
  ggplot(aes(x = profile, y = val, fill = key, group = key)) +
  geom_col(position = "dodge") +
  ylab("Z-score") +
  xlab("") +
  scale_fill_discrete("") +
  theme_minimal()

# Facet option
proc_df |>
  pivot_longer(cols = -profile) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>
  ggplot(aes(x = variable_clean, y = value, fill = variable_clean)) +
  geom_col() +
  facet_wrap(~profile#, scales = "free_y"
             ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey")
        ) +
  labs(x = "Variable",
       y = "Mean Zscore",
       title = "Plotting LPA profiles by variable and average Zscore",
       subtitle = "Model-based clustering - LPA")


# LPA goodness of fit
lpa_bic <- lpa_model$bic

# Step 3: Perform Density-Based Clustering ----

# https://www.sthda.com/english/wiki/wiki.php?id_contents=7940

library(fpc)
library(dbscan)


# Optimal value of “eps” parameter
dbscan::kNNdistplot(key_measures_scaled_head, k =  5)
abline(h = 2.4, lty = 2)

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(key_measures_scaled_head, eps = 2.4, MinPts = 5)

# Plot DBSCAN results
plot(db, key_measures_scaled_head, main = "DBSCAN", frame = FALSE)

fviz_cluster(db, key_measures_scaled_head, stand = FALSE, frame = FALSE, geom = "point")

# Print DBSCAN
print(db)


# Step x... : Evaluating and Comparing Clustering Results ----

# Assuming you have your data in a dataframe called 'data'

# K-means clustering
kmeans_clusters <- kmeans_result_7$cluster

# LPA clustering (using Mclust for Gaussian Mixture Models)
lpa_clusters <- selected_model_eev_6$classification

# DBSCAN clustering
dbscan_clusters <- db$cluster

# Evaluate clustering performance
evaluate_clustering <- function(data, clusters) {
  silhouette_score <- mean(silhouette(clusters, dist(data))[, 3])
  db_index <- clusterSim::davies.bouldin(data, clusters)
  wcss <- sum(kmeans(data, centers = length(unique(clusters)))$withinss)
  return(list(silhouette_score = silhouette_score, db_index = db_index, wcss = wcss))
}


# Evaluate clustering performance
evaluate_clustering <- function(data, clusters) {
  # Ensure clusters are numeric
  clusters <- as.numeric(clusters)

  silhouette_score <- mean(silhouette(clusters, dist(data))[, 3])
  db_index <- index.DB(data, clusters)$DB # Using clusterSim package for Davies-Bouldin index
  wcss <- sum(kmeans(data, centers = length(unique(clusters)))$withinss)

  return(list(silhouette_score = silhouette_score, db_index = db_index, wcss = wcss))
}


# Calculate evaluation metrics for each model
kmeans_eval <- evaluate_clustering(key_measures_scaled_head, kmeans_clusters)
lpa_eval    <- evaluate_clustering(key_measures_scaled_head, lpa_clusters)
dbscan_eval <- evaluate_clustering(key_measures_scaled_head, dbscan_clusters)


# Print evaluation metrics
print("K-means Evaluation:")
print(kmeans_eval)

print("LPA Evaluation:")
print(lpa_eval)

print("DBSCAN Evaluation:")
print(dbscan_eval)

# Select the best model based on silhouette score (higher is better)
best_model <- ifelse(kmeans_eval$silhouette_score >
                       lpa_eval$silhouette_score & kmeans_eval$silhouette_score >
                       dbscan_eval$silhouette_score, "K-means",
                     ifelse(lpa_eval$silhouette_score >
                              dbscan_eval$silhouette_score, "LPA", "DBSCAN"))

print(paste("The best model is:", best_model))





## 1. **Compare Clustering Solutions**:
### Use **Adjusted Rand Index (ARI)** to compare different clustering solutions.
library(clue)
adjustedRandIndex(kmeans_result$cluster, selected_model_eev_6$classification)

# ? interpreation


## 2. **Interpret the Clusters**:
### Analyze the characteristics of each cluster by examining the mean values of the original variables within each cluster.
aggregate(key_measures_scaled_head, by = list(cluster = kmeans_result$cluster), mean)

