#' Maintenance (vacuum) function for hydrometric database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Performs a VACUUM operation on the database, re-organizing and compacting tables.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [WRBtools::hydroConnect()].
#'
#' @return A vacuumed database.
#' @export
#'


vacuum <- function(con)

{
  on.exit(DBI::dbDisconnect(con))

  #TODO: operations below should be atomic
  DBI::dbExecute(con, "VACUUM (ANALYZE)")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_vacuum';"))
}
