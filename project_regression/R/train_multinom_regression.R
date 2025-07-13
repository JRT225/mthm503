train_multinom_regression <- function(train_data) {
  library(nnet)
  multinom(extrication ~ sex + age_band, data = train_data)
}
