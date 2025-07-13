clean_data <- function(df) {
  library(dplyr)
  df <- df %>%
    mutate(across(c(extrication, sex, age_band, casualty_severity, financial_year), as.factor)) %>%
    filter(sex != "Unknown", age_band != "Unknown", extrication != "Unknown") %>%
    droplevels() %>%
    mutate(extrication_rate = n_casualties / number_of_stat19_reported_casualties)
  df
}
