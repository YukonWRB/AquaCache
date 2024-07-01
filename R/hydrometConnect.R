#' Connect to the hydromet (dev) database
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by function [AquaConnect()]. Still exported by this package as old database 'hydromet' is now a development environment.

#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form hydrometHost:"hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form hydrometPort:"1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form hydrometAdminUser:"username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form hydrometAdminPass:"password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

hydrometConnect <- function(name = "hydromet", host = Sys.getenv("hydrometHost"), port = Sys.getenv("hydrometPort"), username = Sys.getenv("hydrometAdminUser"), password = Sys.getenv("hydrometAdminPass"), silent = FALSE){

  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(hydro)
  }, error = function(e) {
    stop("Connection failed.")
  })
}
