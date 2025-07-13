train_test_split <- function(df) {
  library(caret)
  set.seed(42)
  idx <- createDataPartition(df$extrication, p = 0.8, list = FALSE)
  train <- df[idx, ]
  test <- df[-idx, ]
  list(train = train, test = test)
}
