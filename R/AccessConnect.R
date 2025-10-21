#' Connect to an Access database
#'
#' First checks that the proper driver exists, then attempts to connect to the database. A missing driver could trigger a crash of R, so it is important to check for it first. Can be used for all sorts of Access database connections, including EQWin.
#'
#' @param path The path to the Access database file
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#' @export
#'

AccessConnect <- function(path, silent = FALSE) {
  rlang::check_installed(
    "odbc",
    reason = "Package odbc is required for Access database connections"
  )

  # Attempt to load the ODBC driver (if not found R will crash, so stop)
  odbc_drivers <- tryCatch(odbc::odbcListDrivers(), error = function(e) {
    return(NULL)
  })

  # Check if the Microsoft Access Driver is available
  if (!any(grepl("Microsoft Access Driver", odbc_drivers$name))) {
    stop(
      "You need to install the Microsoft Access Driver to connect to an Access database. Go to https://www.microsoft.com/en-us/download/details.aspx?id=54920 and install the version that matches your Microsoft Office installation bit-ness."
    )
  }

  # Check that the file can actually be accessed
  if (!file.exists(path)) {
    stop(
      "The file path you provided does not exist or can't be connected to at the moment."
    )
  }

  # Proceed with the connection if the driver is available
  tryCatch(
    {
      con <- odbc::dbConnect(
        odbc::odbc(),
        .connection_string = paste0(
          "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
          path
        )
      )
    },
    error = function(e) {
      message("Failed to connect to the database: ", e$message)
      return(NULL)
    }
  )

  if (!DBI::dbIsValid(con)) {
    warning("Connection was initially established but returns as invalid.")
    return(con)
  } else {
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(con)
  }
}
