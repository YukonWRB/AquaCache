library(shiny)
library(shinyjs)
library(HydroMetDB)

# Establish database connection
if (!exists("pool")) {
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "hydromet",
    host = Sys.getenv("hydrometHost"),
    port = Sys.getenv("hydrometPort"),
    user = Sys.getenv("hydrometAdminUser"),
    password = Sys.getenv("hydrometAdminPass")
  )
}
