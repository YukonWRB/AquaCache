#' Get new discrete-type data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new discrete data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the discrete table and that have a proper entry in the timeseries table; refer to [addHydrometTimeseries()] for how to add new stations. Does not work on any timeseries of category "continuous": for that, use [getNewContinuous()]. Timeseries with no specified souce_fx will be ignored.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location as entered in the 'timeseries' table, 'param_code' gets the parameter code defined in the 'settings' table, and start_datetime defaults to the instant after the last point already existing in the DB. Each of these can however be set using the "source_fx_args" column in the "timeseries" table; refer to [addHydrometTimeseries()] for a description of how to formulate these arguments.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'discrete'.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export
#'

getNewDiscrete <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all") {
  warning("Function getNewDiscrete is not complete yet!!!")
}
