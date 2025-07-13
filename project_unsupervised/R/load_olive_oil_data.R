# Loading olive oil data from DB
load_olive_oil_data <- function() {
  library(DBI)
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "postgres",
    host = "aws-0-eu-west-2.pooler.supabase.com",
    user = "pgstudent.rvdwflidqvcvffdccwrh",
    password = "0%jkXK^tjMZwuG",
    port=5432
  )
  oil <- tbl(con, "olive_oil") %>% 
    collect()
  dbDisconnect(con)
  oil
}