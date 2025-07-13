perform_eda <- function(data_selected) {
  library(ggplot2)
  library(corrplot)
  library(gridExtra)
  plots <- list()
  
  # Distribution of casualty_severity
  p1 <- ggplot(data_selected, aes(speed_zone, fill=casualty_severity)) + 
    geom_bar(position='dodge') + ggtitle("Speed Zone vs Severity")
  p2 <- ggplot(data_selected, aes(as.factor(is_night), fill=casualty_severity)) + 
    geom_bar(position='dodge') + scale_x_discrete(labels=c("Day","Night")) + ggtitle("Night vs Severity")
  p3 <- ggplot(data_selected, aes(engine_size_group, fill=casualty_severity)) + 
    geom_bar(position='dodge') + ggtitle("Engine Size vs Severity")
  p4 <- ggplot(data_selected, aes(casualty_age_group, fill=casualty_severity)) + 
    geom_bar(position='dodge') + ggtitle("Casualty Age Group vs Severity")
  
  plots$main_grid <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)
  
  # Correlation plot (if numerics exist)
  num_vars <- dplyr::select(data_selected, where(is.numeric))
  if (ncol(num_vars) > 1) {
    corr_matrix <- cor(num_vars)
    plots$corr <- corrplot::corrplot(corr_matrix, method = "color")
  }
  plots
}
