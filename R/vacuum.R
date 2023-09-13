#' Maintenance (vacuum) function for hydrometric database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Performs a VACUUM (ANALYZE) operation on the database, re-organizing and compacting tables.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return A vacuumed database.
#' @export
#'


vacuum <- function(con = hydometConnect(silent = TRUE))

{
  DBI::dbWithTransaction(
    con,
    {
      DBI::dbExecute(con, "VACUUM (ANALYZE)")
      DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_vacuum';"))
    }
  )
}
