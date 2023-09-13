#' Hourly update of real-time data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new real-time data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the realtime tables; refer to [add_timeseries()] for how to add new stations. Does not work on any timeseries of category "discrete".
#'
#' ## Measurement periods:
#' With the exception of "instanteneous" timeseries which automatically receive a period of "00:00:00" (0 time), the period associated with measurements (ex: 1 hour precipitation sum) is derived directly from the output of the function associated with each timeseries in the column source_fx of the timeseries table. If the output function does not return a column with name "period" with a value that the database can coerce to a data type "interval", the period will be entered as NULL. If any of the period values cannot be interpreted by lubridate::period as time periods, then they will also be entered as NULL.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export

 getNewRealtime <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all")
{
  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")
  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, type FROM timeseries WHERE category = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND category = 'continuous'"))
    if (length(timeseries_id) != nrow(all_timeseries)){
      warning("At least one of the timeseries IDs you called for cannot be found in the database.")
    }
  }

  count <- 0 #counter for number of successful new pulls
  success <- data.frame("location" = NULL, "parameter" = NULL, "timeseries" = NULL)
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    param_code <- settings[settings$parameter == parameter & settings$source_fx == source_fx , "remote_param_name"]
    last_data_point <- all_timeseries$end_datetime[i] + 1 #one second after the last data point
    type <- all_timeseries$type[i]

    tryCatch({
      #TODO: find a way to incorporate the source_fx_args in do.call below
      ts <- do.call(source_fx, list(location = loc, param_code = param_code, start_datetime = last_data_point))
      if (nrow(ts) > 0){
        if (all_timeseries$type[i] == "instantaneous"){
          ts$period <- "00:00:00"
        } else if (!("period" %in% names(ts))){
          ts$period <- NA
        } else {
          check <- lubridate::period(unique(ts$period))
          if (NA %in% check){
            ts$period <- NA
          }
        }
        ts$timeseries_id <- tsid
        DBI::dbWithTransaction(
          con, {
            DBI::dbAppendTable(con, "measurements_continuous", ts)
            #make the new entry into table timeseries
            DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            count <- count + 1
            success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "timeseries_id" = tsid))
          }
        )
      }
    }, error = function(e) {
      warning("getNewRealtime: Failed to get new data or to append new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], ").")
    }) #End of tryCatch
  } #End of iteration over each location + param
  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_realtime'"))
  if (nrow(success) > 0){
    return(success)
  }
} #End of function

