library(shiny)
library(shinyjs)
library(shinyWidgets)
library(AquaCache)

# Establish database connection
if (!exists("pool")) {
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "AquaCache",
    host = Sys.getenv("AquaCacheHost"),
    port = Sys.getenv("AquaCachePort"),
    user = Sys.getenv("AquaCacheAdminUser"),
    password = Sys.getenv("AquaCacheAdminPass")
  )
}
