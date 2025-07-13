summarize_kmeans_clusters <- function(df_scaled, kmeans) {
  clusters <- kmeans$cluster
  df_with_clusters <- as.data.frame(df_scaled)
  df_with_clusters$cluster <- clusters
  aggregate(. ~ cluster, data = df_with_clusters, mean)
}
