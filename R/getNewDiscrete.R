#' Get new discrete-category data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Retrieves new discrete data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the discrete table and that have a proper entry in the timeseries table; refer to [addHydrometTimeseries()] for how to add new stations. Does not work on any timeseries of category "continuous": for that, use [getNewContinuous()]. Timeseries with no specified souce_fx will be ignored.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location as entered in the 'timeseries' table, 'param_code' gets the parameter code defined in the 'settings' table, and start_datetime defaults to the instant after the last point already existing in the DB. Additional parameters can be passed using the "source_fx_args" column in the "timeseries" table; refer to [addHydrometTimeseries()] for a description of how to formulate these arguments.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'discrete'.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export
#'

getNewDiscrete <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all") {

  # Get settings
  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")

  # Create table of timeseries
  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime FROM timeseries WHERE category = 'discrete' AND source_fx IS NOT NULL;")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND category = 'discrete' AND source_fx IS NOT NULL;"))
    if (length(timeseries_id) != nrow(all_timeseries)){
      warning("At least one of the timeseries IDs you called for cannot be found in the database, is not of category 'discrete', or has no function specified in column source_fx.")
    }
  }

  count <- 0 #counter for number of successful new pulls
  success <- data.frame("location" = NULL, "parameter" = NULL, "timeseries" = NULL)

  # Run for loop over timeseries rows
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    param_code <- settings[settings$parameter == parameter & settings$source_fx == source_fx , "remote_param_name"]
    last_data_point <- all_timeseries$end_datetime[i] + 1 #one second after the last data point

    tryCatch({
      args_list <- list(location = loc, param_code = param_code, start_datetime = last_data_point)
      if (!is.na(source_fx_args)){ #add some arguments if they are specified
        args <- strsplit(source_fx_args, "\\},\\s*\\{")
        pairs <- lapply(args, function(pair){
          gsub("[{}]", "", pair)
        })
        pairs <- lapply(pairs, function(pair){
          gsub("\"", "", pair)
        })
        pairs <- lapply(pairs, function(pair){
          gsub("'", "", pair)
        })
        pairs <- strsplit(unlist(pairs), "=")
        pairs <- lapply(pairs, function(pair){
          trimws(pair)
        })
        for (j in 1:length(pairs)){
          args_list[[pairs[[j]][1]]] <- pairs[[j]][[2]]
        }
      }
      ts <- do.call(source_fx, args_list) #Get the data using the args_list
      ts <- ts[!is.na(ts$value) , ]

      if (nrow(ts) > 0){
        ts$timeseries_id <- tsid
        DBI::dbWithTransaction(
          con, {
            if (min(ts$datetime) < last_data_point - 1){ #This might happen because a source_fx is feeding in data before the requested datetime. Example: getSnowCourse if a new station is run in parallel with an old station, and the offset between the two used to adjust "old" measurements to the new measurements.
              DBI::dbExecute(con, paste0("DELETE FROM measurements_discrete WHERE datetime >= '", min(ts$datetime), "' AND timeseries_id = ", tsid, ";"))
            }
            DBI::dbAppendTable(con, "measurements_discrete", ts)
            #make the new entry into table timeseries
            DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            count <- count + 1
            success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "timeseries_id" = tsid))
          }
        )
      }
    }, error = function(e) {
      warning("getNewDiscrete: Failed to get new data or to append new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], ").")
    }) #End of tryCatch
  } #End of iteration over each location + param

  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_discretes'"))

  if (nrow(success) > 0){
    return(success)
  }
}
