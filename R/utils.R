#' @title Format Date-Time to pass to SQL
#' @description
#' Format a date-time object to a string that can be passed to SQL queries. Works on POSIXct objects.
#' 
#' @param x A date-time object (POSIXct).
#' @return A string formatted as "YYYY-MM-DD HH:MM:SS" in UTC time zone.
#' @export
fmt <- function(x) format(x, tz = "UTC", "%Y-%m-%d %H:%M:%S")


#' @title Begin a transaction
#' @description
#' Begin a transaction on a database connection. This function creates a temporary table to ensure the transaction is active.
#' 
#' @param con A database connection object.
#' @param silent A boolean indicating whether to suppress messages about transaction status.
#' @return A boolean indicating whether a transaction was started
#' @export
dbTransBegin <- function(con, silent = TRUE) {
  
  # Check if already in a transaction
  if (dbTransCheck(con)) {
    if (!silent) message("dbTransBegin: Transaction already active.")
    return(FALSE)
  }
  
  # Begin transaction
  DBI::dbExecute(con, "BEGIN;")
  # Create a temporary table that auto-destructs so the xact_id is assigned (otherwise nothing happens until something is pushed)
  DBI::dbExecute(con, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;") # Of course, also destroyed on rollback
  
  # Confirm transaction is active
  active <- DBI::dbGetQuery(con, "SELECT pg_current_xact_id_if_assigned() IS NOT NULL AS is_transaction;")[1,1]
  if (active) {
    return(active)
  } else {
    stop("dbTransBegin: Transaction could not be started or verified as started.")
  }
}


#' @title Check if a transaction is active
#' @description
#' Check if a database connection is in an active transaction.
#' 
#' @param con A database connection object.
#' @return A boolean indicating whether a transaction is active (TRUE for active)
#' @export
dbTransCheck <- function(con) {
  
  active <- DBI::dbGetQuery(con, "SELECT pg_current_xact_id_if_assigned() IS NOT NULL AS is_transaction;")[1,1]
  
  if (!active) { # If transaction was started and nothing done yet, active would still be FALSE. Code below handles that possibility
    # Create a temporary table that auto-destructs so the xact_id is assigned (otherwise nothing happens until something is pushed)
    DBI::dbExecute(con, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;") # Of course, also destroyed on rollback
    # Check again
    active <- DBI::dbGetQuery(con, "SELECT pg_current_xact_id_if_assigned() IS NOT NULL AS is_transaction;")[1,1]
  }

  return(active)
}
