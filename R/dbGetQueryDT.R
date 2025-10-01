#' Retrieve a data.table from a database connection
#'
#' @description
#'
#' A simple wrapper function around [DBI::dbGetQuery()] that returns a data.table object instead of a data.frame. Can be used as a direct replacement for [DBI::dbGetQuery()], but be aware of the downstream effects of using a data.table object instead of a data.frame.
#'
#' @param con A connection object as returned by a [DBI::dbConnect()] call or, in the context of this package, a [AquaConnect()] or [snowConnect()] call.
#' @param statement A SQL statement to be passed to the database.
#' @param sqlite_date_convert Logical. If TRUE AND `con` is to an sqlite DB, the function will convert character strings to date or datetime objects with UTC 0 offset. This is useful for sqlite databases where dates are stored as text. If FALSE, no conversion will be done. Defaults to TRUE.
#' @param ... Other parameters passed to the ... parameter of [DBI::dbGetQuery()].
#'
#' @return A data.table object.
#' @export
#'
#' @import data.table
#'

dbGetQueryDT <- function(con, statement, sqlite_date_convert = TRUE, ...) {
  .datatable.aware <- TRUE
  df <- DBI::dbGetQuery(con, statement, ...)
  data.table::setDT(df)

  if (sqlite_date_convert && inherits(con, "SQLiteConnection")) {
    cn <- names(df)

    # find exact "date" columns and convert in one go
    date_cols <- grep("^date$", cn, ignore.case = TRUE, value = TRUE)
    if (length(date_cols)) {
      # if stored as text "YYYY-MM-DD" → as.Date;
      # if numeric → as.Date(..., origin="1970-01-01")
      df[,
        (date_cols) := lapply(
          .SD,
          function(x) {
            if (is.character(x)) {
              as.Date(x)
            } else {
              as.Date(x, origin = "1970-01-01")
            }
          }
        ),
        .SDcols = date_cols
      ]
    }

    # find exact "datetime" columns and convert
    datetime_cols <- grep("^datetime$", cn, ignore.case = TRUE, value = TRUE)
    if (length(datetime_cols)) {
      df[,
        (datetime_cols) := lapply(
          .SD,
          function(x) {
            if (is.character(x)) {
              as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
            } else {
              as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
            }
          }
        ),
        .SDcols = datetime_cols
      ]
    }

    # any "_dt" suffix (e.g., start_dt, end_dt) → POSIXct
    other_dt <- grep("_dt$", cn, ignore.case = TRUE, value = TRUE)
    other_dt <- setdiff(other_dt, datetime_cols)
    if (length(other_dt)) {
      df[,
        (other_dt) := lapply(
          .SD,
          function(x) {
            if (is.character(x)) {
              as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
            } else {
              as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
            }
          }
        ),
        .SDcols = other_dt
      ]
    }
  }

  return(df)
}
