#' Maintenance (vacuum) function for AquaCache database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Performs a VACUUM (ANALYZE) operation on the database, re-organizing and compacting tables.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#'
#' @return A vacuumed database.
#' @export
#'

vacuum <- function(con = AquaConnect(silent = TRUE))

{
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "VACUUM (ANALYZE)")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_vacuum';"))
}
