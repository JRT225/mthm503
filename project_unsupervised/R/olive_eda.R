olive_eda <- function(df) {
  library(ggplot2)
  library(reshape2)
  # Histogram by variable
  df_long <- melt(df, id.vars = "id")
  p1 <- ggplot(df_long, aes(value)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    facet_wrap(~variable, scales = "free", ncol = 4) +
    theme_minimal() +
    labs(title = "Distributions of fatty acids in Olive oil")
  ggsave("olive_histograms.png", plot = p1)
  
  # Boxplots
  p2 <- ggplot(df_long, aes(x = variable, y = value)) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = "Boxplots of Fatty Acid Features", x = "Fatty Acid", y = "Value")
  ggsave("olive_boxplots.png", plot = p2)
  
  # Correlation plot
  df_num <- df %>% select(-id)
  corr_matrix <- cor(df_num)
  corrplot::corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")
  # Save summary
  summary(df)
}
