library(testthat)
library(readr)
library(dplyr)

test_that("cleaned regression data has no 'Unknown' values in sex, age_band, extrication", {
  df <- read_csv("project_regression/data/fire.csv") %>%
    filter(sex != "Unknown", age_band != "Unknown", extrication != "Unknown") %>%
    mutate(across(c(sex, age_band, extrication), as.character))
  expect_false(any(df$sex == "Unknown" | df$age_band == "Unknown" | df$extrication == "Unknown"))
})

test_that("extrication_rate is between 0 and 1", {
  df <- read_csv("project_regression/data/fire.csv")
  df <- df %>%
    filter(sex != "Unknown", age_band != "Unknown", extrication != "Unknown") %>%
    mutate(extrication_rate = n_casualties / number_of_stat19_reported_casualties)
  expect_true(all(df$extrication_rate >= 0 & df$extrication_rate <= 1, na.rm = TRUE))
})