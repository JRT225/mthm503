evaluate_regression_models <- function(model_multinom, model_multinom_sev, model_int, model_sev_int, test_data) {
  list(
    aic = AIC(model_multinom, model_multinom_sev, model_int, model_sev_int),
    summary_multinom = summary(model_multinom),
    summary_multinom_sev = summary(model_multinom_sev),
    summary_int = summary(model_int),
    summary_sev_int = summary(model_sev_int)

  )
}
