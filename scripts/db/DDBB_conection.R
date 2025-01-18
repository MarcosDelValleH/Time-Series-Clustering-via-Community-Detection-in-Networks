library(DBI)
library(RPostgres)
library(dplyr)
library(dotenv)  # Para gestionar variables de entorno

# load .env
dotenv::load_dot_env()

# Load credentials from environment
db_config <- list(
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = Sys.getenv("DB_NAME")
)

# Create a connection
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = db_config$dbname,
  host = db_config$host,
  port = db_config$port,
  user = db_config$user,
  password = db_config$password
)

# Lets load the necessary tables
athletes <- tbl(con, 'athletes') |>
  collect()



laps <- tbl(con, 'laps') |>
  collect()

race_athlete <-  tbl(con, 'race_athlete') |>
  collect()


races <-  tbl(con, 'races') |>
  collect()

