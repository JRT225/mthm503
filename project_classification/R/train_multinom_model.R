train_multinom_model <- function(train_data) {
  library(caret)
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
    method = "multinom",
    trControl = cv_ctrl,
    trace = FALSE
  )
}
