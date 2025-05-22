#' @title Format Date-Time to pass to SQL
#' @description
#' Format a date-time object to a string that can be passed to SQL queries. Works on POSIXct objects.
#' 
#' @noRd
fmt <- function(x) format(x, tz = "UTC", "%Y-%m-%d %H:%M:%S")

#' @title DB transaction check
#' @description
#' Check if a database connection is in an active transaction. If not, start a new transaction.
#' 
#' @param con A database connection object.
#' @return A boolean indicating whether a new transaction was started
#' @noRd

dbTransBegin <- function(con) {
  active <- attr(con, "active_transaction") 
  if ((is.null(active) || !active) && start) {
    DBI::dbBegin(con)
    attr(con, "active_transaction") <- TRUE
    active <- TRUE  # For a new transaction
  } else {
    active <- FALSE  # No new transaction started
  }
  return(active)
}
