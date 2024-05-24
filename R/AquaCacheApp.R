#' Shiny Interface to Hydromet DB
#'
#'@description
#'`r lifecycle::badge("experimental")`
#'
#' Loads a Shiny app that facilitates the addition of information to the database.
#' Currently supports uploading of documents, one-off images, vectors, and raster, with provisions to support adding timeseries (data, images, and rasters).
#' 
#' Requires write privileges to the database.
#' 
#'
#' @return Opens the Shiny application.
#' @export

AquaCacheApp <- function() {
  
  Sys.getenv()
  
  rlang::check_installed("shiny", reason = "to run this app.")
  rlang::check_installed("DT", reason = "to create interactive tables within the app.")
  rlang::check_installed("shinyFiles", reason = "to allow file uploads within the app.")
  rlang::check_installed("shinyjs", reason = "to allow for hiding and showing of elements within the app.")
  rlang::check_installed("shinyWidgets", reason = "to allow for the use of widgets within the app.")
  
  appDir <- system.file("hydrometAdd-app", package = "HydroMetDB")
  if (appDir == "") {
    stop("Could not find hydromet Shiny app directory. Try re-installing `HydroMetDB`.", call. = FALSE)
  }
  
  
  # Load the global variables. Contains modules as well as call to pool::pool() for connection to WRB database, library calls, and loads the translations data.table.
  source(system.file("hydrometAdd-app/app_globals.R", package = "HydroMetDB"))

  shiny::runApp(appDir, display.mode = "normal")
}
