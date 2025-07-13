train_glm_model <- function(train_data) {
  glm(
    cbind(n_casualties, number_of_stat19_reported_casualties - n_casualties) ~ sex + age_band,
    family = binomial,
    data = train_data
  )
}
