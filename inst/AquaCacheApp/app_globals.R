library(shiny)
library(shinyjs)
library(shinyWidgets)
library(AquaCache)

# Establish database connection
if (!exists("pool")) {
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "aquacache",
    host = Sys.getenv("aquacacheHost"),
    port = Sys.getenv("aquacachePort"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacachedminPass")
  )
}
