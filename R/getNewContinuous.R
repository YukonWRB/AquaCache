#' Get new continuous-category data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new real-time data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the measurements_continuous table and that have a proper entry in the timeseries table; refer to [addACTimeseries()] for how to add new stations. Does not work on any timeseries of category "discrete": for that, use [getNewDiscrete()]. Timeseries with no specified souce_fx will be ignored.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location as entered in the 'timeseries' table, 'param_code' gets the parameter code defined in the 'settings' table, and start_datetime defaults to the instant after the last point already existing in the DB. Each of these can however be set using the "source_fx_args" column in the "timeseries" table; refer to [addACTimeseries()] for a description of how to formulate these arguments.
#'
#' ## Assigning measurement periods:
#' With the exception of "instantaneous" timeseries which automatically receive a period of "00:00:00" (0 time), the period associated with measurements (ex: 1 hour precipitation sum) is derived from the interval between measurements UNLESS a period column is provided by the source function (column source_fx, may also depend on source_fx_args). This function typically fetches only a few hours of measurements at a time, so if the interval cannot be conclusively determined from the new data (i.e. hourly measurements over four hours with two measurements missed) then additional data points will be pulled from the database.
#'
#' If a period supplied by any data fetch function cannot be coerced to an period object acceptable to "duration" data type, NULL values will be entered to differentiate from instantaneous periods of "00:00:00".
#'
#' ## Sharing privileges and ownership
#' The parameters of column share_with of table timeseries will be used to determine which users will have access to the new data and the owner column will be used to determine the owner of the new data.
#' 
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'continuous'.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export

getNewContinuous <- function(con = AquaConnect(silent = TRUE), timeseries_id = "all", active = 'default')
{
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  # Create table of timeseries
  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, period_type, record_rate, share_with, owner, active FROM timeseries WHERE category = 'continuous' AND source_fx IS NOT NULL;")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, period_type, record_rate, share_with, owner, active FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND category = 'continuous' AND source_fx IS NOT NULL;"))
    if (length(timeseries_id) != nrow(all_timeseries)) {
      warning("At least one of the timeseries IDs you called for cannot be found in the database, is not of category 'continuous', or has no function specified in column source_fx.")
    }
  }
  
  if (active == 'default') {
    all_timeseries <- all_timeseries[all_timeseries$active == TRUE, ]
  }
  
  if (nrow(all_timeseries) == 0) {
    stop("Could not find any timeseries matching your input parameters.")
  }
  
  
  count <- 0 #counter for number of successful new pulls
  success <- data.frame("location" = NULL, "parameter" = NULL, "timeseries" = NULL)
  
  # Run for loop over timeseries rows
  for (i in 1:nrow(all_timeseries)) {
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    period_type <- all_timeseries$period_type[i]
    record_rate <- all_timeseries$record_rate[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    share_with <- all_timeseries$share_with[i]
    owner <- all_timeseries$owner[i]
    if (is.na(record_rate)) {
      param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = ", parameter, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate IS NULL;"))[1,1]
    } else {
      param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = ", parameter, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate = '", record_rate, "';"))[1,1]
    }


    last_data_point <- all_timeseries$end_datetime[i] + 1 #one second after the last data point

    tryCatch({
      args_list <- list(location = loc, param_code = param_code, start_datetime = last_data_point)
      if (!is.na(source_fx_args)) { #add some arguments if they are specified
        args <- strsplit(source_fx_args, "\\},\\s*\\{")
        pairs <- lapply(args, function(pair) {
          gsub("[{}]", "", pair)
          })
        pairs <- lapply(pairs, function(pair) {
          gsub("\"", "", pair)
        })
        pairs <- lapply(pairs, function(pair) {
          gsub("'", "", pair)
        })
        pairs <- strsplit(unlist(pairs), "=")
        pairs <- lapply(pairs, function(pair) {
          trimws(pair)
        })
        for (j in 1:length(pairs)) {
          args_list[[pairs[[j]][1]]] <- pairs[[j]][[2]]
        }
      }

      ts <- do.call(source_fx, args_list) #Get the data using the args_list
      ts <- ts[!is.na(ts$value) , ]

      if (nrow(ts) > 0) {
        #assign a period to the data
        if (period_type == "instantaneous") { #Period is always 0 for instantaneous data
          ts$period <- "00:00:00"
        } else if ((period_type != "instantaneous") & !("period" %in% names(ts))) { #period_types of mean, median, min, max should all have a period
          ts <- calculate_period(data = ts, timeseries_id = tsid, con = con)
        } else { #Check to make sure that the supplied period can actually be coerced to a period
          check <- lubridate::period(unique(ts$period))
          if (NA %in% check) {
            ts$period <- NA
          }
        }
        
        ts$timeseries_id <- tsid
        ts$imputed <- FALSE
        # The column for "imputed" defaults to FALSE in the DB, so even though it is NOT NULL it doesn't need to be specified UNLESS this function gets modified to impute values.
        if (!is.na(owner)) {
          ts$owner <- owner
        }
        ts$share_with <- share_with
        
        DBI::dbWithTransaction(
          con, {
            if (min(ts$datetime) < last_data_point - 1) {
              DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE datetime >= '", min(ts$datetime), "' AND timeseries_id = ", tsid, ";"))
            }
            DBI::dbAppendTable(con, "measurements_continuous", ts)
            #make the new entry into table timeseries
            DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            count <- count + 1
            success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "timeseries_id" = tsid))
          }
        )
      }
    }, error = function(e) {
      warning("getNewContinuous: Failed to get new data or to append new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], "). Returned error '", e$message, "'.")
    }) #End of tryCatch
  } #End of iteration over each location + param

  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_continuous'"))
  if (nrow(success) > 0) {
    return(success)
  }
} #End of function

