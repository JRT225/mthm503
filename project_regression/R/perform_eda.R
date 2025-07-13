perform_eda <- function(df) {
  library(ggplot2)
  p1 <- ggplot(df, aes(sex)) + geom_bar() + labs(title = "Sex Counts")
  p2 <- ggplot(df, aes(age_band)) + geom_bar() + labs(title= "Age Band Counts")
  p3 <- ggplot(df, aes(x = sex, fill = extrication)) + 
    geom_bar(position = "fill") + labs(y = "Proportion", title = "Extrication Method by Sex")
  p4 <- ggplot(df, aes(x = age_band, fill = extrication)) +
    geom_bar(position = "fill") + labs(y = "Proportion", title = "Extrication Method by Age Band")
  ggsave("eda_sex_counts.png", p1)
  ggsave("eda_age_band_counts.png", p2)
  ggsave("eda_extrication_by_sex.png", p3)
  ggsave("eda_extrication_by_age_band.png", p4)
  summary(df)
}
