library(dplyr)
library(ggplot2)
library(dbscan)
library(stats) 

#PCA plot
plot_pca_scree <- function(pca) {
  scree_df <- data.frame(
    PC = paste0("PC", 1:length(pca$sdev)),
    Variance = pca$sdev^2 / sum(pca$sdev^2)
  )
  ggplot(scree_df, aes(x = PC, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(aes(group = 1), color = "darkred") +
    geom_point(color = "darkred") +
    theme_minimal() +
    labs(title = "Scree Plot of PCA", y = "Proportion of Variance")
}

#Biplot
plot_pca_biplot <- function(pca, df_scaled) {
  biplot(pca, scale = 0, main = "PCA Biplot")
}

# Elbow plot
plot_elbow <- function(df_scaled, max_k = 10) {
  elbow <- sapply(1:max_k, function(k) {
    kmeans(df_scaled, centers = k, nstart = 30)$tot.withinss
  })
  qplot(1:max_k, elbow, geom = c("line", "point")) +
    labs(title = "Elbow Plot for K-means", x = "Number of clusters (k)", y = "Within-cluster Sum of Squares") +
    theme_minimal()
}

# Kmeans PCA cluster
plot_kmeans_pca <- function(pca, kmeans) {
  df_pca <- as.data.frame(pca$x)
  df_pca$cluster <- factor(kmeans$cluster)
  ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(title = "K-means Clusters in PCA Space", color = "Cluster")
}

#DBSCAN PCA cluster
plot_dbscan_pca <- function(pca, db) {
  df_pca <- as.data.frame(pca$x)
  df_pca$cluster <- factor(db$cluster)
  ggplot(df_pca, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(title = "DBSCAN Clusters in PCA Space", color = "DBSCAN Cluster")
}