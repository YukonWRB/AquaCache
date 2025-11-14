#' @title Format Date-Time to pass to SQL
#' @description
#' Format a date-time object to a string that can be passed to SQL queries. Works on POSIXct objects.
#'
#' @param x A date-time object (POSIXct).
#' @return A string formatted as "YYYY-MM-DD HH:MM:SS" in UTC time zone.
#' @export
fmt <- function(x) format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")


#' @title Begin a transaction
#' @description
#' Begin a transaction on a database connection. This function creates a temporary table to ensure the transaction is active, which DBI::dbBegin does not do. The transaction must be committed or rolled back using `DBI::dbExecute(con, "COMMIT;")` or `DBI::dbExecute(con, "ROLLBACK;")`. Note that you should not use DBI's built-in transaction functions to commit or rollback a transaction started with this function.
#'
#' @param con A database connection object.
#' @param silent A boolean indicating whether to suppress messages about transaction status.
#' @return A boolean indicating whether a transaction was started
#' @export
dbTransBegin <- function(con, silent = TRUE) {
  # Check if already in a transaction
  if (dbTransCheck(con)) {
    if (!silent) {
      message("dbTransBegin: Transaction already active.")
    }
    return(FALSE)
  }

  # Begin transaction
  DBI::dbExecute(con, "BEGIN;")
  # Create a temporary table that auto-destructs so the xact_id is assigned (otherwise nothing happens until something is pushed)
  DBI::dbExecute(con, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;") # Of course, also destroyed on rollback

  # Confirm transaction is active
  active <- DBI::dbGetQuery(
    con,
    "SELECT pg_current_xact_id_if_assigned() IS NOT NULL AS is_transaction;"
  )[1, 1]
  if (active) {
    return(active)
  } else {
    stop(
      "dbTransBegin: Transaction could not be started or verified as started."
    )
  }
}


#' @title Check if a transaction is active
#' @description
#' Check if a database connection is in an active transaction. Does not work on a transaction begun with `DBI::dbBegin` until some modification is made to the database; function [dbTransBegin()] should be used to start a transaction instead.
#'
#' @param con A database connection object.
#' @return A boolean indicating whether a transaction is active (TRUE for active)
#' @export
dbTransCheck <- function(con) {
  active <- DBI::dbGetQuery(
    con,
    "SELECT pg_current_xact_id_if_assigned() IS NOT NULL;"
  )[1, 1]

  if (!active) {
    # If transaction was started and nothing done yet, active would still be FALSE. Code below handles that possibility
    # Create a temporary table that auto-destructs so the xact_id is assigned (otherwise nothing happens until something is pushed)
    DBI::dbExecute(con, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;") # Of course, also destroyed on rollback (if not in a transaction, this will just create and drop the table immediately)
    # Check again
    active <- DBI::dbGetQuery(
      con,
      "SELECT pg_current_xact_id_if_assigned() IS NOT NULL;"
    )[1, 1]
  }

  return(active)
}


#' Replace infinite or NaN values with NA
#'
#' Utility function to replace `Inf`, `-Inf`, and `NaN` values with `NA`.
#'
#' @param x Numeric vector, data.frame, or data.table. If data.frame or data.table, will only work on 'numeric' class columns.
#' @return Numeric vector with infinite values converted to `NA`.
#' @export
inf_to_na <- function(x) {
  # data.table
  if (data.table::is.data.table(x)) {
    num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(num_cols)) {
      x[,
        (num_cols) := lapply(.SD, function(col) {
          idx <- which(!is.finite(col))
          if (length(idx)) {
            col[idx] <- NA
          }
          col
        }),
        .SDcols = num_cols
      ]
    }
    return(x)
  }

  # data.frame / tibble
  if (is.data.frame(x)) {
    # TRUE for tibbles or data.frames (and data.tables, but these are dealt with differently)
    numeric_cols <- sapply(x, is.numeric)
    x[numeric_cols] <- lapply(x[numeric_cols], function(col) {
      idx <- which(!is.finite(col))
      if (length(idx)) {
        col[idx] <- NA
      }
      col
    })
    return(x)
  }

  # vector
  if (is.numeric(x)) {
    idx <- which(!is.finite(x))
    if (length(idx)) {
      x[idx] <- NA
    }
    return(x)
  }

  # If x is not numeric, return it unchanged
  warning("Input is not numeric. Returning unchanged.")
  return(x)
}
