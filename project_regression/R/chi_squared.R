chi_squared <- function(df) {
  library(knitr)
  library(broom)
  tab_sex <- table(df$extrication, df$sex)
  tab_age <- table(df$extrication, df$age_band)
  tab_severity <- table(df$extrication, df$casualty_severity)
  
  list(
    chisq_sex = chisq.test(tab_sex),
    chisq_age = chisq.test(tab_age),
    chisq_severity = chisq.test(tab_severity)
  )
}