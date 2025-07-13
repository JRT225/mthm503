clean_data <- function(df) {
  library(dplyr)
  # Remove NAs
  df <- df %>% filter(complete.cases(.))
  df
}
