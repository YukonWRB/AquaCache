#' Maintenance (vacuum) function for AquaCache database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Performs a VACUUM (ANALYZE) operation on the database, re-organizing and compacting tables.
#'
#' @param con A connection to the database. If left NULL will use function AquaConnect and automatically disconnect when finished.
#' @param full If TRUE, performs a full vacuum. This takes longer and requires an exclusive lock, but can reclaim more space as tables are re-written without dead space.
#'
#' @return A vacuumed database.
#' @export
#'

vacuum <- function(con = NULL, full = FALSE)

{
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  DBI::dbExecute(con, "VACUUM (ANALYZE)")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_vacuum';"))
}
