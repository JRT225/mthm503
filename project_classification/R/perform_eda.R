perform_eda <- function(data_selected) {
  library(ggplot2)
  library(corrplot)
  
  plots <- list()
  
  # Individual plots
  plots$severity_dist <- ggplot(data_selected, aes(as.factor(casualty_severity), fill=as.factor(casualty_severity))) +
    geom_bar() +
    labs(title="Casualty Severity Distribution", x="Severity", y="Count") +
    theme_minimal()
  
  plots$speed_vs_severity <- ggplot(data_selected, aes(speed_zone, fill=casualty_severity)) + 
    geom_bar(position='dodge') + 
    ggtitle("Speed Zone vs Severity")
  
  plots$night_vs_severity <- ggplot(data_selected, aes(as.factor(is_night), fill=casualty_severity)) + 
    geom_bar(position='dodge') + 
    scale_x_discrete(labels=c("Day","Night")) + 
    ggtitle("Night vs Severity")
  
  plots$engine_vs_severity <- ggplot(data_selected, aes(engine_size_group, fill=casualty_severity)) + 
    geom_bar(position='dodge') + 
    ggtitle("Engine Size vs Severity")
  
  plots$age_vs_severity <- ggplot(data_selected, aes(casualty_age_group, fill=casualty_severity)) + 
    geom_bar(position='dodge') + 
    ggtitle("Casualty Age Group vs Severity")
  
  # Correlation matrix
  num_vars <- dplyr::select(data_selected, where(is.numeric))
  if (ncol(num_vars) > 1) {
    plots$corr_matrix <- cor(num_vars)
  } else {
    plots$corr_matrix <- NULL
  }
  
  return(plots)
}
