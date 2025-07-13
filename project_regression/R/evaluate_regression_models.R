evaluate_regression_models <- function(model_multinom, model_glm, model_interaction, test_data) {
  # Example: Return AIC and summaries for each model
  list(
    aic = AIC(model_multinom, model_interaction, model_glm),
    summary_multinom = summary(model_multinom),
    summary_interaction = summary(model_interaction),
    summary_glm = summary(model_glm)
  )
}
