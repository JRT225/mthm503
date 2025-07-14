train_multinom_regression <- function(train_data) {
  library(nnet)
  #Sex and Age
  model <- multinom(extrication ~ sex + age_band, data = train_data)
  # Multinomial with severity
  model_sev <- multinom(extrication ~ sex + age_band + casualty_severity, data = train_data)
  list(model = model, model_sev = model_sev)
}
