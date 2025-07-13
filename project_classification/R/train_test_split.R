train_test_split <- function(data_selected) {
  library(caret)
  set.seed(42)
  train_idx <- createDataPartition(data_selected$casualty_severity, p = 0.8, list = FALSE)
  list(
    train = data_selected[train_idx, ],
    test  = data_selected[-train_idx, ]
  )
}