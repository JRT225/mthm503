# Loading casualties, accidents and vehicles data from DB
load_pedestrian_data <- function() {
  library(DBI)
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "aws-0-eu-west-2.pooler.supabase.com",
    user = "pgstudent.rvdwflidqvcvffdccwrh",
    password = "0%jkXK^tjMZwuG",
    port=5432
  )
  casualties_df <- tbl(con, "stats19_casualties")
  accidents_df  <- tbl(con, "stats19_accidents")
  vehicles_df   <- tbl(con, "stats19_vehicles")
  
  #Join the three tables
  pedestrian <- casualties_df %>%
    filter(casualty_class == "Pedestrian") %>%
    left_join(accidents_df, by = "accident_index") %>%
    left_join(vehicles_df, by = c("accident_index", "vehicle_reference")) %>%
    collect()
  
  dbDisconnect(con)
  pedestrian
}
