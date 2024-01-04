#' Connect to the EQWin database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the EQWin Access database.
#'
#' @param path Full path to the database including extension.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

EQConnect <- function(path = "X:/EQWin/WR/DB/Water_Resources.mdb", silent = FALSE){

  tryCatch({
    EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
    if (!silent){
      print("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(EQWin)
  }, error = function(e) {
    stop("EQWin connection failed.")
  })

}
