# Notes/ overflow

# Parrallel processing
# Step 1: Performing K-means Clustering in R ----
library(readr)
library(data.table)
library(factoextra)
library(bigmemory)
library(parallel)

# Step 1: Read the CSV file in chunks using data.table for efficiency
key_measures_rep_sub_sample <-
  fread("key_measures_rep_sub_sample.csv") |>
  select( -person_id)

# Step 2: Convert to a matrix
sample_matrix <- as.matrix(key_measures_rep_sub_sample)

# Step 3: Remove the data frame to free up memory
rm(key_measures_rep_sub_sample)
gc()

# Step 4: Create the elbow plot using parallel processing
cl <- makeCluster(detectCores() - 2)  # Use fewer cores to reduce memory load
clusterEvalQ(cl, library(factoextra))
clusterExport(cl, "sample_matrix")

# Ensure k.max is set correctly and handle errors gracefully
elbow_plot <- tryCatch({
  parLapply(cl, 2:10, function(k) {
    fviz_nbclust(sample_matrix, kmeans, method = "wss", k.max = k)
  })
}, error = function(e) {
  stopCluster(cl)
  stop(e)
})

stopCluster(cl)


## 1. **Determine the Optimal Number of Clusters**:
fviz_nbclust(key_measures_scaled_head |> head(10000), kmeans, method = "wss")  # Elbow Method
#fviz_nbclust(key_measures_scaled_head, kmeans, method = "silhouette")  # Silhouette Method


## 2. **Run K-means Clustering**:
set.seed(123)  # For reproducibility
kmeans_result_4 <-  kmeans(key_measures_rep_sub_sample, centers = 4, nstart = 100, iter.max = 100)   # Assuming 4 clusters
kmeans_result_5 <-  kmeans(key_measures_rep_sub_sample, centers = 5, nstart = 100, iter.max = 100)   # Assuming 5 clusters
kmeans_result_6 <-  kmeans(key_measures_rep_sub_sample, centers = 6, nstart = 10, iter.max = 1000)   # Assuming 6 clusters
kmeans_result_7 <-  kmeans(key_measures_rep_sub_sample, centers = 7, nstart = 100, iter.max = 1000)   # Assuming 7 clusters
kmeans_result_8 <-  kmeans(key_measures_rep_sub_sample, centers = 8, nstart = 100, iter.max = 100)   # Assuming 8 clusters
kmeans_result_9 <-  kmeans(key_measures_rep_sub_sample, centers = 9, nstart = 100, iter.max = 100)   # Assuming 9 clusters
kmeans_result_10 <- kmeans(key_measures_rep_sub_sample, centers = 10, nstart = 100, iter.max = 100)  # Assuming 10 clusters
kmeans_result_11 <- kmeans(key_measures_rep_sub_sample, centers = 11, nstart = 100, iter.max = 100)  # Assuming 11 clusters
kmeans_result_12 <- kmeans(key_measures_rep_sub_sample, centers = 12, nstart = 100, iter.max = 100)  # Assuming 12 clusters
