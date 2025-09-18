#' Connect to the EQWin database
#'
#' @description
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

EQConnect <- function(
  path = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb",
  silent = FALSE
) {
  # Check if the file exists
  if (!file.exists(path)) {
    stop("The specified database file does not exist.")
  }

  tryCatch(
    {
      EQWin <- DBI::dbConnect(
        drv = odbc::odbc(),
        .connection_string = paste0(
          "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
          path
        )
      )
      if (!silent) {
        message(
          "Remember to disconnect using DBI::dbDisconnect() when finished."
        )
      }
      return(EQWin)
    },
    error = function(e) {
      stop(
        "EQWin connection failed: ",
        e$message,
        ". Do you need to install the Access database engine? Check the help file."
      )
    }
  )
}
