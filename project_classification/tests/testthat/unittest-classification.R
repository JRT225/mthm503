library(testthat)
library(readr)
library(caret)

test_that("train/test split keeps 80% in training set", {
  df <- readr::read_csv("project_classification/data/pedestrian.csv")
  df$casualty_severity <- as.factor(df$casualty_severity)
  set.seed(42)
  train_idx <- createDataPartition(df$casualty_severity, p=0.8, list=FALSE)
  expect_true(abs(length(train_idx) / nrow(df) - 0.8) < 0.02)
})

test_that("dataset reports missing values but does not fail", {
  df <- readr::read_csv("project_classification/data/pedestrian.csv")
  expect_gt(sum(is.na(df)), 0)
})