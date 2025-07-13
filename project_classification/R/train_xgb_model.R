train_xgb_model <- function(train_data) {
  library(caret)
  library(xgboost)
  set.seed(42)
  cv_ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = multiClassSummary,
    savePredictions = TRUE
  )
  train(
    casualty_severity ~ .,
    data = train_data,
    method = "xgbTree",
    trControl = cv_ctrl
  )
}
