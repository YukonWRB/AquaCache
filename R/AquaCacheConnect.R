#' Connect to the AquaCache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the hydrology database, especially if the database type and connection method changes in the future.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form AquaCacheHost:"hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form AquaCachePort:"1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form AquaCacheAdminUser:"username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form AquaCacheAdminPass:"password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

AquaCacheCon <- function(name = "AquaCache", host = Sys.getenv("AquaCacheHost"), port = Sys.getenv("AquaCachePort"), username = Sys.getenv("AquaCacheAdminUser"), password = Sys.getenv("AquaCacheAdminPass"), silent = FALSE){

  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    # Explicitly set time zone, just in case
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(hydro)
  }, error = function(e) {
    stop("Connection failed.")
  })
}
