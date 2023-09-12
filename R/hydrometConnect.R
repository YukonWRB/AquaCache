#' Connect to the hydromet database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the hydrology database, especially if the database type and connection method changes in the future.
#'
#' @param name Database name.
#' @param host Database host address.
#' @param port Connection port.
#' @param username Username. Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @seealso [DB_browse_ts()], [DB_browse_spatial()]to browse the database contents; [DB_get_meta()] to extract metadata; [DB_get_ts()] to extract timeseries information; and [DB_get_spatial()] to extract spatial content.
#' @export
#'

hydrometConnect <- function(name = "hydromet", host = "localhost", port = "5432", username = "postgres", password = "SnowFa11ing", silent = FALSE){

  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    if (!silent){
      print("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(hydro)
  }, error = function(e){
    stop("Connection failed.")
  })
}
