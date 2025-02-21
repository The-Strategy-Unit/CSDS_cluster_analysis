# Perform cluster analysis

library(tidyverse)
library(factoextra)
library(GGally)
library(plotly)
library(mclust)
library(cluster)
library(clusterSim)
library(sconn)

#library(sparklyr)
#library(dplyr)
#library(dbplyr)
#library(DBI)

# Connect to databricks ----

sc <- sc()

sc_disconnect()

# Approach: ----

# 1. **Determine the Number of Clusters**:
#    - Use the **Elbow Method**: Plot the total within-cluster sum of squares against the number of clusters and look for an "elbow" point.
#    - Use the **Silhouette Method**: Calculate the silhouette coefficient for different numbers of clusters and choose the number that maximizes the silhouette score.
#    - Use the **Gap Statistic**: Compare the total within intra-cluster variation for different numbers of clusters with their expected values under null reference distribution of the data.
#
# 2. **Choose the Clustering Algorithm**:
#    - **K-means**: Suitable for large datasets and when clusters are spherical.
#    - **K-medoids**: More robust to noise and outliers compared to K-means.
#    - **Hierarchical Clustering**: Useful for smaller datasets and when you want a dendrogram to visualize the clustering process.
#    - **Model-Based Clustering**: Assumes data is generated from a mixture of distributions and can handle more complex cluster shapes.


# Read in look ups ----

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

# Read in data ----
## Read in scaled data from data bricks ----
#sp_key_measures_scaled_wide <-
#  dplyr::tbl(
#    sc,
#    dbplyr::in_catalog("strategyunit", "csds_al", "key_measures_scaled_wide")
#    )
#
#
sp_key_measures_scaled_wide <- tbl(sc, dbplyr::in_catalog("strategyunit", "csds_al", "key_measures_scaled_wide"))


sp_km_strat_sample_head <- tbl(sc, dbplyr::in_catalog("strategyunit", "csds_al", "key_measures_scaled_wide_strat_sample_head"))


#sp_key_measures_scaled_wide <-
#  sp_key_measures_scaled_wide %>%
#  sdf_register("sp_key_measures_scaled_wide")

## Read in representative sample - from file directory ----
key_measures_rep_sub_sample <-
  #read_csv("key_measures_rep_sub_sample.csv") |>
  read_csv("key_measures_sample_cluster.csv") |>
  select(-person_id) |>
  as.data.frame()

# Step 1: Performing Model-Based Clustering ----
## 1. Determine the Optimal Number of Clusters: ----
#plot <- fviz_nbclust(key_measures_rep_sub_sample, kmeans, method = "wss")

gc()

set.seed(123)
kmeans_result_4  <-  kmeans(key_measures_rep_sub_sample, centers = 4)   # Assuming 4 clusters
kmeans_result_5  <-  kmeans(key_measures_rep_sub_sample, centers = 5)   # Assuming 5 clusters
kmeans_result_6  <-  kmeans(key_measures_rep_sub_sample, centers = 6)   # Assuming 6 clusters
kmeans_result_7  <-  kmeans(key_measures_rep_sub_sample, centers = 7)   # Assuming 7 clusters
kmeans_result_8  <-  kmeans(key_measures_rep_sub_sample, centers = 8)   # Assuming 8 clusters
kmeans_result_9  <-  kmeans(key_measures_rep_sub_sample, centers = 9)   # Assuming 9 clusters
kmeans_result_10 <- kmeans(key_measures_rep_sub_sample, centers = 10)  # Assuming 10 clusters
kmeans_result_11 <- kmeans(key_measures_rep_sub_sample, centers = 11)  # Assuming 11 clusters
kmeans_result_12 <- kmeans(key_measures_rep_sub_sample, centers = 12)  # Assuming 12 clusters

# Plot sum of squares
wssplot <- function(data, nc = 10, seed = 1234, max_iter = 100) {
  wss <- numeric(nc)
  wss[1] <- (nrow(data) - 1) * sum(apply(data, 2, var))
  set.seed(seed)
  for (i in 2:nc) {
    wss[i] <- sum(kmeans(data, centers = i, nstart = 25, iter.max = max_iter)$withinss)
  }
  plot(1:nc, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within Group Sum of Squares',
       main = 'Elbow Method Plot to Find Optimal Number of Clusters', frame.plot = TRUE,
       col = 'blue', lwd = 1.5)
}

wssplot(key_measures_rep_sub_sample)


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

## 2. Visualize the Clusters: ----
fviz_cluster(kmeans_result_6,
             data = key_measures_rep_sub_sample,
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


## 3. Pairwise Scatter Plot Matrix ----
# Perform Principal Component Analysis (PCA)
pca_result <-
  prcomp(key_measures_rep_sub_sample, scale. = TRUE)

# Create a dataframe with the principal components
pca_data <-
  as.data.frame(pca_result$x) |>
  mutate(cluster = as.factor(kmeans_result_6$cluster)) # Add cluster assignments to the dataframe

# Create pairwise scatter plot matrix
ggpairs_plot <- ggpairs(pca_data, aes(color = cluster, alpha = 0.5))


## 4. 3D plot of top 3 principle components ----
# Create a dataframe with the first three principal components
pca_data_3d <-
  pca_data[, 1:3] |>
  mutate(cluster = as.factor(kmeans_result_6$cluster))

pca_data_3d$cluster <- as.factor(kmeans_result_6$cluster)

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
  bind_cols(key_measures_rep_sub_sample, classification = kmeans_result_6$cluster) |>
  group_by(classification) |>
  summarise_all(funs(mean)) |>
  mutate(classification = paste0("Profile ", 1:6)) |>
  mutate_at(vars(-classification), function(x) round(x, 3)) |>
  rename(profile = classification)

kmeans_classification_means_plot_free <-
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
       subtitle = "K-means cluster analysis - 6 clusters")


kmeans_classification_means_plot_fixed <-
  kmeans_classification_means |>
  pivot_longer(cols = -profile) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>
  ggplot(aes(x = variable_clean, y = value, fill = variable_clean)) +
  geom_col() +
  facet_wrap(~profile) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Variable",
       y = "Mean Zscore",
       title = "Mean Zscore by variable and cluster/profile",
       subtitle = "K-means cluster analysis - 6 clusters")



# Step 2: Performing Model-Based Clustering ----

#library(forcats)


# https://jihongzhang.org/posts/2017-11-23-latent-profile-analysis/

# https://cran.r-project.org/web/packages/tidyLPA/vignettes/Introduction_to_tidyLPA.html


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

fit_output      <- explore_model_fit(key_measures_rep_sub_sample, n_profiles_range = 1:12)

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
selected_model_eev_6 <- Mclust(key_measures_rep_sub_sample, G = 6, modelNames = "EEV")
print(summary(selected_model_eev_6))

# Plot model classifications
dff <- bind_cols(key_measures_rep_sub_sample, classification = selected_model_eev_6$classification)

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
lpa_profiles_mean_fixed <-
  proc_df |>
  pivot_longer(cols = -profile) |>
  left_join(key_measure_lookup, by = c("name" = "variable")) |>
  ggplot(aes(x = variable_clean, y = value, fill = variable_clean)) +
  geom_col() +
  facet_wrap(~profile, scales = "free_y"
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

lpa_profiles_mean_free <-
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


# Step 3: Perform Density-Based Clustering ----

# https://www.sthda.com/english/wiki/wiki.php?id_contents=7940
# https://cran.r-project.org/web/packages/dbscan/vignettes/dbscan.pdf

library(fpc)
library(dbscan)

# Optimal value of “eps” parameter
dbscan::kNNdistplot(key_measures_rep_sub_sample, k =  5)
abline(h = 1.8, lty = 2)

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(key_measures_rep_sub_sample, eps = 1.8, MinPts = 5)

# Plot DBSCAN results
dbscan_plot <- plot(db, key_measures_rep_sub_sample, main = "DBSCAN", frame = FALSE)

dbscan_plot_fviz <- fviz_cluster(db, key_measures_rep_sub_sample, stand = FALSE, frame = FALSE, geom = "point")

# Print DBSCAN
print(db)

summary(db)

# Visualise DBscan clusters
# Calculate the average variable scores for each cluster
db_average_scores <- aggregate(key_measures_rep_sub_sample, by = list(cluster = db$cluster), FUN = mean)

library(reshape2)
library(ggplot2)

# Melt the data for ggplot2
db_melted_scores <- reshape2::melt(db_average_scores, id.vars = "cluster")

# Create a bar plot
db_clusters_avg_plot_fixed <-
  db_melted_scores |>
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Variable Scores by Cluster", x = "Variable", y = "Average Score", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_rect(fill = NA, colour = "grey"),
        legend.position = "none"
        ) +
  facet_wrap(~factor(cluster))

db_clusters_avg_plot_free <-
  db_melted_scores |>
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Variable Scores by Cluster", x = "Variable", y = "Average Score", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.background = element_rect(fill = NA, colour = "grey"),
        legend.position = "none"
        ) +
  facet_wrap(~factor(cluster), scales = "free_y")


# Set row names to cluster numbers
row.names(db_average_scores) <- db_average_scores$cluster
db_average_scores <- db_average_scores[, -1] # Remove the cluster column

# Create a heatmap
pheatmap::pheatmap(db_average_scores,
                   main = "Average Variable Scores by Cluster",
                   cluster_rows = FALSE,
                   cluster_cols = FALSE,
                   scale = "column")


# Step x... : Evaluating and Comparing Clustering Results ----

gc()

# Clean environment to free up disk space
# Specify the names of the objects you want to keep
keep_objects <- c("key_measures_rep_sub_sample",
                  "kmeans_result_6",
                  "selected_model_eev_6",
                  "db")

# Get the names of all objects in the workspace
all_objects <- ls()

# Remove all objects except the specified ones
rm(list = setdiff(all_objects, keep_objects))


# Isolate cluster vectors from each model
kmeans_clusters <- kmeans_result_6$cluster
lpa_clusters    <- selected_model_eev_6$classification
dbscan_clusters <- db$cluster


# Function to subsample data
subsample_data <- function(data, fraction = 0.25) {
  set.seed(123) # For reproducibility
  sample_indices <- sample(1:nrow(data), size = floor(fraction * nrow(data)))
  return(data[sample_indices, ])
}

# Evaluate clustering performance on a subsample
evaluate_clustering_subsample <- function(data, clusters, fraction = 0.25) {
  # Subsample the data
  subsample <- subsample_data(data, fraction)
  subsample_clusters <- clusters[1:nrow(subsample)]

  # Ensure clusters are numeric
  subsample_clusters <- as.numeric(subsample_clusters)

  # Calculate silhouette score
  dist_matrix <- dist(subsample)
  sil <- silhouette(subsample_clusters, dist_matrix)
  silhouette_score <- mean(sil[, 3])

  # Calculate Davies-Bouldin index
  db_index <- index.DB(subsample, subsample_clusters)$DB

  # Calculate within-cluster sum of squares (WCSS)
  wcss <- sum(kmeans(subsample, centers = length(unique(subsample_clusters)))$withinss)

  return(list(silhouette_score = silhouette_score, db_index = db_index, wcss = wcss))
}

# Calculate evaluation metrics for each model using a subsample
kmeans_eval <- evaluate_clustering_subsample(key_measures_rep_sub_sample, kmeans_clusters)
lpa_eval    <- evaluate_clustering_subsample(key_measures_rep_sub_sample, lpa_clusters)
dbscan_eval <- evaluate_clustering_subsample(key_measures_rep_sub_sample, dbscan_clusters)

# Function to identify the optimum model
select_optimum_model <- function(kmeans_eval, lpa_eval, dbscan_eval) {
  # Combine evaluation metrics into a data frame
  eval_metrics <- data.frame(
    model = c("K-means", "LPA", "DBSCAN"),
    silhouette_score = c(kmeans_eval$silhouette_score, lpa_eval$silhouette_score, dbscan_eval$silhouette_score),
    db_index = c(kmeans_eval$db_index, lpa_eval$db_index, dbscan_eval$db_index),
    wcss = c(kmeans_eval$wcss, lpa_eval$wcss, dbscan_eval$wcss)
  )

  # Normalize the metrics for comparison (higher silhouette score is better, lower DB index and WCSS are better)
  eval_metrics$normalized_silhouette <- eval_metrics$silhouette_score / max(eval_metrics$silhouette_score)
  eval_metrics$normalized_db_index <- min(eval_metrics$db_index) / eval_metrics$db_index
  eval_metrics$normalized_wcss <- min(eval_metrics$wcss) / eval_metrics$wcss

  # Calculate a combined score (you can adjust the weights as needed)
  eval_metrics$combined_score <- eval_metrics$normalized_silhouette + eval_metrics$normalized_db_index + eval_metrics$normalized_wcss

  # Identify the model with the highest combined score
  optimum_model <- eval_metrics[which.max(eval_metrics$combined_score), "model"]

  return(optimum_model)
}

# Identify the optimum model
optimum_model <- select_optimum_model(kmeans_eval, lpa_eval, dbscan_eval)
print(paste("The optimum model is:", optimum_model))





## 1. **Compare Clustering Solutions**:
### Use **Adjusted Rand Index (ARI)** to compare different clustering solutions.
library(clue)
adjustedRandIndex(kmeans_result$cluster, selected_model_eev_6$classification)

# ? interpreation


## 2. **Interpret the Clusters**:
### Analyze the characteristics of each cluster by examining the mean values of the original variables within each cluster.
aggregate(key_measures_scaled_head, by = list(cluster = kmeans_result$cluster), mean)


# Step y...: Save objects and outputs to read into quarto doc ----

# Assuming your cluster analysis objects are named kmeans_model and lpa_model
save(
  # kmeans
  kmeans_result_4 ,
  kmeans_result_5 ,
  kmeans_result_6 ,
  kmeans_result_7 ,
  kmeans_result_8 ,
  kmeans_result_9 ,
  kmeans_result_10,
  kmeans_result_11,
  kmeans_result_12,
  pca_result,
  pca_data,
  ggpairs_plot,
  pca_data_3d,
  kmeans_classification_means,
  kmeans_classification_means_plot_fixed,
  kmeans_classification_means_plot_free,

  # LPA
  lpa_model_lookup,
  fit_output,
  to_plot,
  selected_model_eev_6,
  dff,
  proc_df,
  lpa_profiles_mean_fixed,
  lpa_profiles_mean_free,

  # density based
  db,
  #dbscan_plot,
  dbscan_plot_fviz,
  db_average_scores,
  db_melted_scores,
  db_clusters_avg_plot_fixed,
  db_clusters_avg_plot_free,


  file = "cluster_analysis.RData"
  )

# Save model outputs only for evaluation code:
save(
  # kmeans
  kmeans_result_6 ,

  # LPA
  selected_model_eev_6,

  # density based
  db,

  file = "cluster_analysis_selected_models.RData"
)

save(
  # evaluation
  kmeans_clusters,
  lpa_clusters   ,
  dbscan_clusters,
  kmeans_eval,
  lpa_eval   ,
  dbscan_eval,
  file = "cluster_analysis_evaluations.RData"
)
