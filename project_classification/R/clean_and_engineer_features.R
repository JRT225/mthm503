clean_and_engineer_features <- function(df) {
  library(dplyr)
  library(lubridate)
  
  # Remove all-NA and constants
  df <- df[, colSums(is.na(df)) < nrow(df)]
  df <- df[, sapply(df, function(x) length(unique(x[!is.na(x)])) > 1)]
  
  # Impute missing values, aggregation
  Mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    ux[which.max(tabulate(match(x, ux)))]
  }
  df <- df %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm=TRUE), .))) %>%
    mutate(across(where(is.character), ~ifelse(is.na(.), Mode(.), .))) %>%
    mutate(across(where(is.factor), ~ifelse(is.na(.), Mode(.), .)))
  
  # Feature engineering
  df <- df %>%
    mutate(
      obs_date_parsed = ymd_hms(obs_date, tz = "UTC"),
      obs_weekday = wday(obs_date_parsed, label = TRUE),
      obs_hour = hour(obs_date_parsed),
      obs_month = month(obs_date_parsed, label = TRUE),
      obs_year = year(obs_date_parsed),
      casualty_age_group = cut(
        age_of_casualty,
        breaks = c(-Inf, 12, 18, 65, Inf),
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
  # Remove leakage/high-cardinality columns
  leakage_vars <- c(
    "accident_severity", "enhanced_casualty_severity", "enhanced_collision_severity",
    "casualty_reference", "accident_index", "vehicle_reference"
  )
  high_card_cols <- names(Filter(function(x) is.factor(x) && nlevels(x) > 30, df))
  drop_vars <- c(
    high_card_cols, 
    "from_pt", "to_pt", "generic_make_model", "geom", 
    "lsoa_of_casualty", "lsoa_of_driver", "lsoa_of_accident_location", "lad_ons", "lah_ons"
  )
  
  #Removing the unwanted features
  model_vars <- c(
    "casualty_severity", "casualty_age_group", "driver_age_group", "speed_zone", "is_night", "is_urban",
    "engine_size_group", "vulnerable_pedestrian", "on_crossing", "sex_of_casualty", "pedestrian_location", 
    "pedestrian_movement", "vehicle_type", "sex_of_driver", "engine_capacity_cc", "light_conditions", 
    "casualty_home_area_type", "road_type", "speed_limit_mph", "obs_weekday", "obs_hour", "obs_month", 
    "obs_year", "did_police_officer_attend_scene_of_accident"
  )
  model_vars <- intersect(model_vars, names(df))
  data_selected <- df[, model_vars, drop = FALSE]
  
  # Convert all character columns to factor
  data_selected <- data_selected %>% mutate(across(where(is.character), as.factor))
  
  data_selected
}
