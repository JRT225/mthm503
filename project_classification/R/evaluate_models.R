evaluate_models <- function(model_multinom, model_rf, model_xgb, test_data) {
  library(caret)
  library(pROC)
  
  # Predictions
  pred_multinom <- predict(model_multinom, test_data)
  pred_rf <- predict(model_rf, test_data)
  pred_xgb <- predict(model_xgb, test_data)
  
  # Confusion Matrices
  cm_multinom <- confusionMatrix(pred_multinom, test_data$casualty_severity)
  cm_rf <- confusionMatrix(pred_rf, test_data$casualty_severity)
  cm_xgb <- confusionMatrix(pred_xgb, test_data$casualty_severity)
  
  # Probabilities for AUC
  prob_multinom <- predict(model_multinom, test_data, type = "prob")
  prob_rf <- predict(model_rf, test_data, type = "prob")
  prob_xgb <- predict(model_xgb, test_data, type = "prob")
  
  auc_multinom <- pROC::multiclass.roc(test_data$casualty_severity, prob_multinom)$auc
  auc_rf <- pROC::multiclass.roc(test_data$casualty_severity, prob_rf)$auc
  auc_xgb <- pROC::multiclass.roc(test_data$casualty_severity, prob_xgb)$auc
  
  list(
    cm_multinom = cm_multinom,
    cm_rf = cm_rf,
    cm_xgb = cm_xgb,
    auc_multinom = auc_multinom,
    auc_rf = auc_rf,
    auc_xgb = auc_xgb
  )
}
