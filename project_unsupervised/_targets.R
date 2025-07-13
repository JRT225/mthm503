library(targets)
library(tarchetypes)
library(here)

source("R/load_olive_oil_data.R")
source("R/clean_data.R")
source("R/olive_eda.R")
source("R/summarize_kmeans_clusters.R")
source("R/summarize_dbscan_clusters.R")

tar_option_set(packages = c(
  "dplyr", "DBI", "RPostgres", "tidyr", "ggplot2", "corrplot", "reshape2",
  "GGally", "cluster", "dbscan"
))

list(
  tar_target(
    raw_data,
    load_olive_oil_data()
  ),
  tar_target(
    cleaned_data,
    clean_data(raw_data)
  ),
  tar_target(
    eda_results,
    olive_eda(cleaned_data)
  ),
  tar_target(
    df_scaled,
    scale(select(cleaned_data, -id))
  ),
  tar_target(
    pca,
    prcomp(df_scaled, center = TRUE, scale. = TRUE)
  ),
  tar_target(
    kmeans,
    kmeans(df_scaled, centers = 3, nstart = 30)
  ),
  tar_target(
    silhouette_kmeans,
    {
      sil <- cluster::silhouette(kmeans$cluster, dist(df_scaled))
      mean(sil[, 3])
    }
  ),
  tar_target(
    db,
    dbscan::dbscan(df_scaled, eps = 1, minPts = 5)
  ),
  tar_target(
    kmeans_summary,
    summarize_kmeans_clusters(df_scaled, kmeans)
  ),
  tar_target(
    dbscan_summary,
    summarize_dbscan_clusters(df_scaled, db)
  ),
  tar_render(
    report,
    "vignettes/Unsupervised_Report.Rmd"
  )
)
