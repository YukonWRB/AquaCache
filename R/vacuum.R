#' Maintenance (vacuum) function for AquaCache database.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Performs a VACUUM (ANALYZE) operation on the database, re-organizing and compacting tables.
#'
#' @param con A connection to the database. If left NULL will use function AquaConnect and automatically disconnect when finished.
#'
#' @return A vacuumed database.
#' @export
#'

vacuum <- function(con = NULL)

{
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  DBI::dbExecute(con, "VACUUM (ANALYZE)")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_vacuum';"))
}
