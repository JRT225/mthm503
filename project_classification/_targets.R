library(targets)
library(tarchetypes)
library(here)

source("R/load_pedestrian_data.R")
source("R/clean_and_engineer_features.R")
source("R/train_test_split.R")
source("R/perform_eda.R")
source("R/train_xgb_model.R")
source("R/train_rf_model.R")
source("R/train_multinom_model.R")
source("R/evaluate_models.R")

tar_option_set(packages = c(
  "dplyr", "DBI", "RPostgres", "tidyverse", "caret", "lubridate", "corrplot",
  "randomForest", "nnet", "pROC", "VIM", "gridExtra", "xgboost"
))

list(
  # Load and join data
  tar_target(
    raw_data,
    load_pedestrian_data()
  ),
  # Data cleaning and feature engineering
  tar_target(
    cleaned_data,
    clean_and_engineer_features(raw_data)
  ),
  # Exploratory Data Analysis
  tar_target(
    eda_results,
    perform_eda(cleaned_data)
  ),
  # Train/Test split
  tar_target(
    split,
    train_test_split(cleaned_data)
  ),
  tar_target(
    train_data,
    split$train
  ),
  tar_target(
    test_data,
    split$test
  ),
  # Train Models
  tar_target(
    model_multinom,
    train_multinom_model(train_data)
  ),
  tar_target(
    model_rf,
    train_rf_model(train_data)
  ),
  tar_target(
    model_xgb,
    train_xgb_model(train_data)
  ),
  # Model Evaluation
  tar_target(
    evaluation,
    evaluate_models(
      model_multinom,
      model_rf,
      model_xgb,
      test_data
    )
  ),
  # Render Report
  tar_render(
    report,
    "vignettes/Classification_Report.Rmd"
  )
)