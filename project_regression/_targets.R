library(targets)
library(tarchetypes)
library(here)

source("R/load_fire_finance_data.R")
source("R/clean_data.R")
source("R/perform_eda.R")
source("R/chi_squared.R")
source("R/train_multinom_regression.R")
source("R/train_interaction_model.R")
source("R/evaluate_regression_models.R")

tar_option_set(packages = c(
  "dplyr", "DBI", "RPostgres", "tidyverse", "yardstick", "vip", "readr", "tune",
  "tidyr", "workflows", "parsnip", "rsample", "themis", "corrplot", "caret", "nnet", "knitr", "broom"
))

list(
  tar_target(
    raw_data,
    load_fire_finance_data()
  ),
  tar_target(
    cleaned_data,
    clean_data(raw_data)
  ),
  tar_target(
    eda_results,
    perform_eda(cleaned_data)
  ),
  tar_target(
    chi_sq_results,
    chi_squared(cleaned_data)
  ),
  tar_target(
    model_multinom,
    train_multinom_regression(cleaned_data)$model
  ),
  tar_target(
    model_multinom_sev,
    train_multinom_regression(cleaned_data)$model_sev
  ),
  tar_target(
    model_int,
    train_interaction_model(cleaned_data)$model_int
  ),
  tar_target(
    model_sev_int,
    train_interaction_model(cleaned_data)$model_sev_int
  ),
  tar_target(
    evaluation,
    evaluate_regression_models(
      model_multinom,
      model_multinom_sev,
      model_int,
      model_sev_int,
      test_data
    )
  ),
  tar_render(
    report,
    "vignettes/Regression_Report.Rmd"
  )
)
