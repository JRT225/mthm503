# Loading fire and finance data from DB
load_fire_finance_data <- function() {
  library(DBI)
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "aws-0-eu-west-2.pooler.supabase.com",
    user = "pgstudent.rvdwflidqvcvffdccwrh",
    password = "0%jkXK^tjMZwuG",
    port=5432
  )
  fire <- tbl(con, "fire_rescue_extrication_casualties")
  finance <- tbl(con, "stats19_by_financial_year")
  
  df <- fire %>%
    left_join(finance, by = "financial_year") %>%
    collect()
  
  dbDisconnect(con)
  df
}