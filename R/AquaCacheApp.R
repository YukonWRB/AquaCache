#' Shiny Interface to Hydromet DB
#'
#'@description
#'`r lifecycle::badge("experimental")`
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' @param browser Open the application in a browser window right away (TRUE), or in a R window (FALSE). Default is TRUE.
#' @param display.mode The display mode for the application. Default is "normal". See `shiny::runApp()` for more information.
#'
#' @return Opens a Shiny application.
#' @export
#'

AquaCacheApp <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port"), browser = TRUE, display.mode = "normal") {
  
  rlang::check_installed("shiny", reason = "to run this app.")
  rlang::check_installed("DT", reason = "to create interactive tables within the app.")
  rlang::check_installed("shinyFiles", reason = "to allow file uploads within the app.")
  rlang::check_installed("shinyjs", reason = "to allow for hiding and showing of elements within the app.")
  rlang::check_installed("shinyWidgets", reason = "to allow for the use of widgets within the app.")
  rlang::check_installed("pool", reason = "to connect to the WRB database.")
  
  appDir <- system.file("AquaCacheApp", package = "HydroMetDB")
  if (appDir == "") {
    stop("Could not find hydromet Shiny app directory. Try re-installing `HydroMetDB`.", call. = FALSE)
  }
  
  
  # Load the global variables. Contains modules as well as call to pool::pool() for connection to WRB database, library calls, and loads the translations data.table.
  source(system.file("AquaCacheApp/app_globals.R", package = "HydroMetDB"))

  shiny::runApp(appDir, display.mode = display.mode, host = host, port = port, launch.browser = browser)
}
