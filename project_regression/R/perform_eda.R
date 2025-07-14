perform_eda <- function(df) {
  library(ggplot2)
  library(dplyr)
  library(scales)
  
  # Data summaries
  rate_summary <- df %>%
    group_by(age_band, sex) %>%
    summarise(mean_rate = mean(extrication_rate, na.rm = TRUE), .groups="drop")
  
  rate_severity_summary <- df %>%
    group_by(age_band, sex, casualty_severity) %>%
    summarise(mean_rate = mean(extrication_rate, na.rm = TRUE), .groups="drop")
  
  yearly_summary <- df %>%
    group_by(financial_year, extrication) %>%
    summarise(mean_rate = mean(extrication_rate, na.rm = TRUE), .groups="drop")
  
  sex <- ggplot(df, aes(sex)) + geom_bar() + labs(title = "Sex Counts")
  age <- ggplot(df, aes(age_band)) + geom_bar() + labs(title= "Age Band Counts")
  ex_sex <- ggplot(df, aes(x = sex, fill = extrication)) + 
    geom_bar(position = "fill") + labs(y = "Proportion",
                                       title = "Extrication Method by Sex")
  ex_age <- ggplot(df, aes(x = age_band, fill = extrication)) +
    geom_bar(position = "fill") + labs(y = "Proportion", 
                                       title = "Extrication Method by Age Band")
  ex_age_sex <- ggplot(rate_summary, aes(x = age_band, y = mean_rate, fill = sex)) +
    geom_col(position = "dodge") +
    labs(title = "Mean Extrication Rate by Age Band and Sex",
         x = "Age Band", y = "Mean Extrication Rate") +
    scale_y_continuous(labels = scales::percent)
  ex_age_sev <- ggplot(df, aes(x = age_band, fill = extrication)) +
    geom_bar(position = "fill") +
    facet_wrap(~casualty_severity) +
    labs(title = "Extrication by Age Band & Severity", y = "Proportion", x = "Age Band")
  ex_mean <- ggplot(rate_severity_summary, aes(x = age_band, y = mean_rate, fill = sex)) +
    geom_col(position = "dodge") +
    facet_wrap(~casualty_severity) +
    labs(title = "Mean Extrication Rate by Age, Sex, and Severity",
         y = "Mean Extrication Rate", x = "Age Band") +
    scale_y_continuous(labels = scales::percent)
  
  ex_fin <- ggplot(yearly_summary, aes(x = financial_year, y = mean_rate, color = extrication, group = extrication)) +
    geom_line() +
    labs(title = "Extrication Rate Over Time", y = "Mean Extrication Rate", x = "Financial Year") +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  list(
    plots = list(
      sex = sex, 
      age = age, 
      ex_sex = ex_sex,
      ex_age = ex_age, 
      ex_age_sex = ex_age_sex, 
      ex_age_sev = ex_age_sev, 
      ex_mean = ex_mean, 
      ex_fin = ex_fin
    ),
    summaries = list(
      rate_summary = rate_summary,
      rate_severity_summary = rate_severity_summary,
      yearly_summary = yearly_summary
    ),
    summary_table = summary(df)
  )
}
