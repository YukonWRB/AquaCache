#' Retrieve a data.table from a database connection
#' 
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' A simple wrapper function around [DBI::dbGetQuery()] that returns a data.table object instead of a data.frame. Can be used as a direct replacement for [DBI::dbGetQuery()], but be aware of the downstream effects of using a data.table object instead of a data.frame.
#'
#' @param con A connection object as returned by a [DBI::dbConnect()] call or, in the context of this package, a [AquaConnect()] or [snowConnect()] call.
#' @param statement A SQL statement to be passed to the database.
#' @param ... Other parameters passed to the ... parameter of [DBI::dbGetQuery()].
#'
#' @return A data.table object.
#' @export
#' 
#' @import data.table
#'

dbGetQueryDT <- function(con, statement, ...) {
  .datatable.aware <- TRUE
  res <- DBI::dbGetQuery(con, statement, ...)
  data.table::setDT(res)
  return(res)
}
