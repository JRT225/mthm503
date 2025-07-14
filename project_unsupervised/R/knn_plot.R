plot_knn_dist <- function(df_scaled, k = 5) {
  library(dbscan)
  dists <- kNNdist(df_scaled, k = k)
  dists_sorted <- sort(dists)
  df_plot <- data.frame(Distance = dists_sorted, Index = seq_along(dists_sorted))
  ggplot(df_plot, aes(x = Index, y = Distance)) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    labs(title = paste("5-NN Distance"), x = "Points by distance", y = "kNN distance") +
    theme_minimal()
}
