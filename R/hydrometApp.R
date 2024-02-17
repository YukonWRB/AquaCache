#' Shiny Interface to Hydromet DB
#'
#'@description
#'`r lifecycle::badge("experimental")`
#'
#' Loads a Shiny app that facilitates the addition of timeseries (data, images, and rasters) and uploading of documents, one-off images and rasters.
#'
#' @return Opens the Shiny application.
#' @export

hydrometApp <- function() {
  
  rlang::check_installed("shiny", reason = "to run this app.")
  rlang::check_installed("DT", reason = "to create interactive tables within the app.")
  rlang::check_installed("shinyFiles", reason = "to allow file uploads within the app.")
  rlang::check_installed("shinyjs", reason = "to allow for hiding and showing of elements within the app.")
  
  
  appDir <- system.file("hydrometAdd-app", package = "HydroMetDB")
  if (appDir == "") {
    stop("Could not find hydromet Shiny app directory. Try re-installing `HydroMetDB`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
