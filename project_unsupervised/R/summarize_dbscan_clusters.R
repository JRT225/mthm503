summarize_dbscan_clusters <- function(df_scaled, db) {
  clusters <- db$cluster
  df_with_clusters <- as.data.frame(df_scaled)
  df_with_clusters$cluster <- clusters
  aggregate(. ~ cluster, data = df_with_clusters, mean)
}
