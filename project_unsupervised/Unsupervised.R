library(DBI)
library(RPostgres)
library(tidyr)
library(ggplot2)
library(corrplot)
library(reshape2)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = "aws-0-eu-west-2.pooler.supabase.com",
  user = "pgstudent.rvdwflidqvcvffdccwrh",
  password = "0%jkXK^tjMZwuG",
  port=5432
)

dbListTables(con)

oil <- tbl(con, "olive_oil" ) %>% collect()

dbDisconnect(con)

str(oil)

write_csv(oil, "olive.csv")

df <- oil

head(df)
str(df)

colSums(is.na(df))

summary(df)

# Making df into long format
df_reshape <- melt(df, id.vars="id")

ggplot(df_reshape, aes(value)) +
  geom_histogram(bins=30, fill="blue", color="black") +
  facet_wrap(~variable, scales="free", ncol=4) +
  theme_minimal() +
  labs(title= "Distributions of fatty acids in Olive oil")

df_num <- df %>% select(-id)
corr_matrix <- cor(df_num)
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")

ggplot(df_reshape, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Boxplots of Fatty Acid Features", x = "Fatty Acid", y = "Value")

library(GGally)
GGally::ggpairs(df %>% select(-id))

# PCA Dimensionality reduction
df_scaled <- as.data.frame(scale(df %>%select(-id)))

pca <- prcomp(df_scaled, center=TRUE, scale. = TRUE)
summary(pca)

plot(pca, type="lines")
biplot(pca, scale=0)


#K means clustering
#Elbow method
elbow <- sapply(1:10, function(k) {
  kmeans(df_scaled, centers=k, nstart=30)$tot.withinss
})

plot(1:10, elbow, type="b",
     xlab = "Number of k",
     ylab="Within Group Sum of Squares")

#setting K=3
set.seed(42)
kmeans <- kmeans(df_scaled, centers=3, nstart=30)
table(kmeans$cluster)

#Visualize clusters in PCA
df_pca <-as.data.frame(pca$x)
df_pca$cluster <- factor(kmeans$cluster)
ggplot(df_pca, aes(PC1, PC2, color=cluster)) + 
  geom_point(size=2) + 
  theme_minimal() +
  labs(title = "K Means Cluster in PCA space")

library(cluster)
sil <- silhouette(kmeans$cluster, dist(df_scaled))
mean(sil[, 3]) # average silhouette width, higher is better (max 1)

plot(sil, main = "Silhouette plot for k-means clustering (k=3)")

install.packages("dbscan")
library(dbscan)

#Finding best knn
kNNdistplot(df_scaled, k=5)
abline(h=1.5, lty=2)

#Setting eps =1.5
db <- dbscan(df_scaled, eps = 1, minPts = 5)
table(db$cluster)

df_pca <- as.data.frame(pca$x)
df_pca$dbscan_cluster <- factor(db$cluster)
library(ggplot2)
ggplot(df_pca, aes(PC1, PC2, color = dbscan_cluster)) + 
  geom_point(size = 2) + 
  theme_minimal() + 
  labs(title = "DBSCAN clusters in PCA space")

# For k-means:
df_kmeans <- cbind(df_num, kmeans_cluster = kmeans$cluster)
aggregate(. ~ kmeans_cluster, data = df_kmeans, FUN = mean)

# For DBSCAN:
df$dbscan_cluster <- db$cluster
aggregate(. ~ dbscan_cluster, data = df %>% select(-id), FUN = mean)
