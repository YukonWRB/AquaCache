#' Get new discrete-category data
#'
#' @description
#' `r lifecycle::badge("experimental")`
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

#Notes for Emilie:
# 1. Don't forget about necessary updates to the timeseries table, namely for start_datetime, end_datetime, and last_new_data. last_daily_calculation can be left blank.
# 2. Note that timeseries.param_type is restricted to specific strings for consistency. See hydrometInit.

getNewDiscrete <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all") {
  warning("Function getNewDiscrete is not complete yet!!!")

  # Get settings
  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")

  # Create table of timeseries
  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, period_type FROM timeseries WHERE category = 'discrete' AND source_fx IS NOT NULL;")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, period_type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND category = 'discrete' AND source_fx IS NOT NULL;"))
    if (length(timeseries_id) != nrow(all_timeseries)){
      warning("At least one of the timeseries IDs you called for cannot be found in the database, is not of category 'discrete', or has no function specified in column source_fx.")
    }
  }

  # For each timeseries id, pulls data from snow DB and adds to hydrometdb

}
