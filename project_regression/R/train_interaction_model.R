train_interaction_model <- function(train_data) {
  library(nnet)
  model_int <- multinom(extrication ~ sex * age_band, data = train_data)
  model_sev_int <- multinom(extrication ~ sex * age_band * casualty_severity, data = train_data)
  list(model_int = model_int, model_sev_int = model_sev_int)
}
