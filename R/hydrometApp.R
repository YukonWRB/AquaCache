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
  appDir <- system.file("hydrometAdd-app", package = "HydroMetDB")
  if (appDir == "") {
    stop("Could not find hydromet Shiny app directory. Try re-installing `HydroMetDB`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
