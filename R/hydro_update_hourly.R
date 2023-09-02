#' Hourly update of real-time data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new real-time data starting from the last data point in the local database, using the function speficied in the timeseries table column "source_fx" Only works on stations that are ALREADY in the realtime tables; refer to [add_timeseries()] for how to add new stations.
#'
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [WRBtools::hydroConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export

hydro_update_hourly <- function(con, timeseries_id = "all")
{
  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")
  if (timeseries_id == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx , end_datetime FROM timeseries WHERE category = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, end_datetime FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
  }

  count <- 0 #counter for number of
  success <- data.frame("location" = NULL, "parameter" = NULL, "timeseries" = NULL)
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    param_code <- settings[settings$parameter == parameter & settings$source_fx == source_fx , "remote_param_name"]
    last_data_point <- all_timeseries$end_datetime[i] + 1 #one second after the last data point

    tryCatch({
      ts <- do.call(source_fx, list(location = loc, param_code = param_code, start_datetime = last_data_point))
      if (nrow(ts) > 0){
        ts$timeseries_id <- tsid
        DBI::dbAppendTable(con, "realtime", ts)
        #make the new entry into table timeseries
        DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
        count <- count + 1
        success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "timeseries_id" = tsid))
      }
    }, error = function(e) {
      warning("hydro_update_hourly: Failed to check for new data or to append discovered new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], ").")
    }) #End of tryCatch
  } #End of iteration over each location + param
  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_realtime'"))
  if (nrow(success) > 0){
    return(success)
  }
} #End of function

