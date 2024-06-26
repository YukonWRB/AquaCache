#' Connect to the EQWin database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the EQWin Access database. Requires installation of Microsoft Access redistributable of the right bitness for the version of R you're running. See https://www.microsoft.com/en-us/download/details.aspx?id=54920 for installation. Note that 64 bit R needs a 64 bit installation.
#'
#' @param path Full path to the database including extension.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

EQConnect <- function(path = "//carver/infosys/EQWin/WR/DB/Water_Resources.mdb;", silent = FALSE){

  warning("This function is causing R to crash as of 2024-06-25. Unsure what the issue is. Contact the Data Scientist if you require programmatic access to the EQWin database in the meantime.")
  # tryCatch({
  #   EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
  #   DBI::dbCanConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
  #   if (!silent) {
  #     print("Remember to disconnect using DBI::dbDisconnect() when finished.")
  #   }
  #   return(EQWin)
  # }, error = function(e) {
  #   stop("EQWin connection failed. Do you need to install the Access database engine? Check the help file.")
  # })

}
