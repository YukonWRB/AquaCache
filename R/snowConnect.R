#' Connect to the snow database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function exists to facilitate connecting to the snow database, especially if the database type and connection method changes in the future.
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
#' @export
#'



snowConnect_pg <- function(name = "snowDB", host = "localhost", port = "5432", username = "postgres", password = Sys.getenv("SnowPass"), silent = FALSE){



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
