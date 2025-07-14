library(DBI)
library(RPostgres)
library(tidyverse)
library(caret)
library(lubridate)
library(corrplot)
library(randomForest)
library(nnet)
library(pROC)
library(car)
library(VIM)
library(gridExtra)
library(tibble)
library(knitr)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres",
  host = "aws-0-eu-west-2.pooler.supabase.com",
  user = "pgstudent.rvdwflidqvcvffdccwrh",
  password = "0%jkXK^tjMZwuG",
  port=5432
)

dbListTables(con)

#Pull the three tables
casualties_df <- tbl(con, "stats19_casualties") %>% collect()
accidents_df  <- tbl(con, "stats19_accidents")  %>% collect()
vehicles_df   <- tbl(con, "stats19_vehicles")   %>% collect()

dbDisconnect(con)
#Join the three tables
ped_df <- casualties_df %>%
  filter(casualty_class == "Pedestrian") %>%
  left_join(accidents_df, by = "accident_index") %>%
  left_join(vehicles_df, by = c("accident_index", "vehicle_reference"))

summary(ped_df)

write_csv(ped_df, "file.csv")

# Load Data
data <- ped_df
str(data)
head(data, 10)
summary(data)
#EDA

colSums(is.na(data))

ggplot(data, aes(as.factor(casualty_severity), fill=as.factor(casualty_severity))) +
  geom_bar() +
  labs(title="Casualty Severity Distribution", x="Severity", y="Count") +
  theme_minimal()

num_vars <- select_if(data, is.numeric)
if (ncol(num_vars) > 1) {
  corr_matrix <- cor(num_vars, use="pairwise.complete.obs")
  corrplot::corrplot(corr_matrix, method = "color", tl.cex=0.7)
}

# Remove all-NA columns and constant columns
data <- data[, colSums(is.na(data)) < nrow(data)]
data <- data[, sapply(data, function(x) length(unique(x[!is.na(x)])) > 1)]

# Pick all columns except IDs and other potential leakage
exclude_cols <- c("accident_index", "vehicle_reference", "enhanced_casualty_severity", "enhanced_collision_severity" ,"casualty_reference", "geom", "from_pt", "to_pt", "generic_make_model", "lsoa_of_casualty", "lsoa_of_driver", "lsoa_of_accident_location", "lad_ons", "lah_ons") 
# Remove them
exclude_cols <- intersect(exclude_cols, names(data))
all_cols <- setdiff(names(data), exclude_cols)
# Ensure target
all_cols <- c("casualty_severity", setdiff(all_cols, "casualty_severity"))

data_imp <- data[, all_cols]

# Impute or remove rows with NA
Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
data_imp <- data_imp %>%
  mutate(
    across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm=TRUE), .)),
    across(where(is.character), ~ifelse(is.na(.), Mode(.), .))
  ) %>%
  mutate(across(where(is.character), as.factor))

data_imp$casualty_severity <- as.factor(data_imp$casualty_severity)
rf_imp <- randomForest(casualty_severity ~ ., data = data_imp, importance = TRUE, ntree = 200)
varImpPlot(rf_imp)
importance(rf_imp)

# Choosing top 10 features based on MeanDecreaseGini
imp_tbl <- as.data.frame(importance(rf_imp))
imp_tbl$Feature <- rownames(imp_tbl)
imp_tbl <- imp_tbl[order(-imp_tbl$MeanDecreaseGini), ] # sort descending
head(imp_tbl, 10)


# Impute missing values, aggregation
Mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm=TRUE), .))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), Mode(.), .))) %>%
  mutate(across(where(is.factor), ~ifelse(is.na(.), Mode(.), .)))

# Feature engineering
data <- data %>%
  mutate(
    obs_date_parsed = ymd_hms(obs_date, tz = "UTC"),
    obs_weekday = wday(obs_date_parsed, label = TRUE),
    obs_hour = hour(obs_date_parsed),
    obs_month = month(obs_date_parsed, label = TRUE),
    obs_year = year(obs_date_parsed),
    casualty_age_group = cut(
      age_of_casualty,
      breaks = c(-Inf, 12, 21, 60, Inf),
      labels = c("Child", "Youth", "Adult", "Elderly")
    ),
    driver_age_group = if ("age_of_driver" %in% names(.)) {
      cut(
        age_of_driver,
        breaks = c(-Inf, 18, 30, 65, Inf),
        labels = c("Young", "Adult", "Middle", "Senior")
      )
    } else { NA },
    speed_zone = cut(
      speed_limit_mph,
      breaks = c(-Inf, 20, 40, Inf),
      labels = c("Low", "Medium", "High")
    ),
    is_night = ifelse(grepl("Dark", as.character(light_conditions)), 1, 0),
    is_urban = ifelse(casualty_home_area_type == "Urban area", 1, 0),
    engine_size_group = cut(
      engine_capacity_cc,
      breaks = c(-Inf, 1200, 2000, Inf),
      labels = c("Small", "Medium", "Large")
    ),
    vulnerable_pedestrian = ifelse(casualty_age_group %in% c("Child", "Elderly"), 1, 0),
    on_crossing = ifelse(grepl("crossing facility", tolower(as.character(pedestrian_location))), 1, 0)
  )

# Remove data with potential leakage/high-cardinality columns
leakage_vars <- c(
  "accident_severity", "enhanced_casualty_severity", "enhanced_collision_severity",
  "casualty_reference", "accident_index", "vehicle_reference"
)
high_card_cols <- names(Filter(function(x) is.factor(x) && nlevels(x) > 30, data))
drop_vars <- c(
  high_card_cols, 
  "from_pt", "to_pt", "generic_make_model", "geom", 
  "lsoa_of_casualty", "lsoa_of_driver", "lsoa_of_accident_location", "lad_ons", "lah_ons"
)
model_vars <- c(
  "casualty_severity", "casualty_age_group", "driver_age_group", "speed_zone", "is_night", "is_urban",
  "engine_size_group", "vulnerable_pedestrian", "on_crossing", "sex_of_casualty", "pedestrian_location", 
  "pedestrian_movement", "vehicle_type", "sex_of_driver", "engine_capacity_cc", "light_conditions", 
  "casualty_home_area_type", "road_type", "speed_limit_mph", "obs_weekday", "obs_hour", "obs_month", 
  "obs_year", "did_police_officer_attend_scene_of_accident"
)
model_vars <- intersect(model_vars, names(data))
data_selected <- data[, model_vars]
str(data_selected)

#Convert all characters to factors
data_selected <- data_selected %>% mutate(across(where(is.character), as.factor))
str(data_selected)

summary(data_selected)

p1 <- ggplot(data_selected, aes(speed_zone, fill=casualty_severity)) + 
  geom_bar(position='dodge') + 
  ggtitle("Speed Zone vs Severity")

p2 <- ggplot(data_selected, aes(as.factor(is_night), fill=casualty_severity)) + 
  geom_bar(position='dodge') + 
  scale_x_discrete(labels=c("Day","Night")) + 
  ggtitle("Night vs Severity")

p3 <- ggplot(data_selected, aes(engine_size_group, fill=casualty_severity)) + 
  geom_bar(position='dodge') + 
  ggtitle("Engine Size vs Severity")

p4 <- ggplot(data_selected, aes(casualty_age_group, fill=casualty_severity)) + 
  geom_bar(position='dodge') + 
  ggtitle("Casualty Age Group vs Severity")

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

num_vars <- select(data_selected, where(is.numeric))
if (ncol(num_vars) > 1) {
  corr_matrix <- cor(num_vars)
  corrplot::corrplot(corr_matrix, method = "color")
}

# Now use data_selected for modeling
# DataPartitiion is used for proportions of classes
set.seed(42)
train_idx <- createDataPartition(data_selected$casualty_severity, p = 0.8, list = FALSE)
train_data <- data_selected[train_idx, ]
test_data  <- data_selected[-train_idx, ]

# 5-fold CV
cv_ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = TRUE,
  sampling = "smote"
)

# Train multinomial logistic regression
set.seed(42)
model_multinom <- train(
  casualty_severity ~ .,
  data = train_data,
  method = "multinom",
  trControl = cv_ctrl,
  trace = FALSE
)

# Train random forest
model_rf <- train(
  casualty_severity ~ .,
  data = train_data,
  method = "rf",
  trControl = cv_ctrl,
  importance = TRUE,
  ntree = 100
)

#Train XGBoost
library(xgboost)
model_xgb <- train(
  casualty_severity ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = cv_ctrl
)

# Prediction of models
pred_multinom <- predict(model_multinom, test_data)
pred_rf <- predict(model_rf, test_data)
pred_xgb <- predict(model_xgb, test_data)

#Confusion Matrix
cm_multinom <- confusionMatrix(pred_multinom, test_data$casualty_severity)
cm_rf <- confusionMatrix(pred_rf, test_data$casualty_severity)
cm_xgb <- confusionMatrix(pred_xgb, test_data$casualty_severity)

print(cm_multinom)
print(cm_rf)
print(cm_xgb)

# Filter macro metrics and showing in tables
get_macro <- function(cm, metric) {
  vals <- cm$byClass[, metric]
  mean(vals, na.rm=TRUE)
}
model_names <- c("Multinom", "RandomForest", "XGBoost")
accuracy <- c(
  cm_multinom$overall["Accuracy"],
  cm_rf$overall["Accuracy"],
  cm_xgb$overall["Accuracy"]
)
kappa <- c(
  cm_multinom$overall["Kappa"],
  cm_rf$overall["Kappa"],
  cm_xgb$overall["Kappa"]
)
precision <- c(
  get_macro(cm_multinom, "Pos Pred Value"),
  get_macro(cm_rf, "Pos Pred Value"),
  get_macro(cm_xgb, "Pos Pred Value")
)
recall <- c(
  get_macro(cm_multinom, "Sensitivity"),
  get_macro(cm_rf, "Sensitivity"),
  get_macro(cm_xgb, "Sensitivity")
)
balanced_acc <- c(
  get_macro(cm_multinom, "Balanced Accuracy"),
  get_macro(cm_rf, "Balanced Accuracy"),
  get_macro(cm_xgb, "Balanced Accuracy")
)
tbl <- data.frame(
  Model = model_names,
  Accuracy = accuracy,
  Kappa = kappa,
  Precision = precision,
  Recall = recall,
  Balanced_Accuracy = balanced_acc
)
knitr::kable(tbl, digits=3, caption = "Overall Model Performance (Macro Averages)")

get_class_metric <- function(cm, metric) {
  vals <- cm$byClass[, metric]
  # Name the vector by class, for clarity
  names(vals) <- rownames(cm$byClass)
  return(vals)
}

# Set classes
classes <- rownames(cm_multinom$byClass)

# Make the table for classes
class_metrics <- data.frame(
  Class = rep(classes, each = 3),
  Model = rep(c("Multinom", "RandomForest", "XGBoost"), times = length(classes)),
  Precision = c(get_class_metric(cm_multinom, "Pos Pred Value"),
                get_class_metric(cm_rf, "Pos Pred Value"),
                get_class_metric(cm_xgb, "Pos Pred Value")),
  Recall = c(get_class_metric(cm_multinom, "Sensitivity"),
             get_class_metric(cm_rf, "Sensitivity"),
             get_class_metric(cm_xgb, "Sensitivity")),
  Balanced_Accuracy = c(get_class_metric(cm_multinom, "Balanced Accuracy"),
                        get_class_metric(cm_rf, "Balanced Accuracy"),
                        get_class_metric(cm_xgb, "Balanced Accuracy"))
)

class_metrics_wide <- pivot_wider(
  class_metrics,
  id_cols = Class,
  names_from = Model,
  values_from = c(Precision, Recall, Balanced_Accuracy),
  names_glue = "{Model}_{.value}"
)
knitr::kable(class_metrics_wide, digits = 3, caption = "Per-class Model Metrics")

# Multiclass ROC/AUC
prob_multinom <- predict(model_multinom, test_data, type = "prob")
prob_rf <- predict(model_rf, test_data, type = "prob")
prob_xgb <- predict(model_xgb, test_data, type = "prob")

auc_multinom <- multiclass.roc(test_data$casualty_severity, prob_multinom)
auc_rf <- multiclass.roc(test_data$casualty_severity, prob_rf)
auc_xgb <- multiclass.roc(test_data$casualty_severity, prob_xgb)

print(auc_multinom$auc)
print(auc_rf$auc)
print(auc_xgb$auc)