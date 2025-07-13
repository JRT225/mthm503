#install.packages("testthat")
library(testthat)

test_that("no missing values in olive oil data", {
  df <- readr::read_csv("project_unsupervised/data/olive.csv")
  expect_true(all(!is.na(df)))
})

test_that("kmeans produces 3 clusters", {
  df <- readr::read_csv("project_unsupervised/data/olive.csv")
  df_num <- df[, -1]
  km <- kmeans(scale(df_num), centers=3, nstart=30)
  expect_equal(length(unique(km$cluster)), 3)
})
