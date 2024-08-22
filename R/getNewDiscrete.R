#' Get new discrete-category data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new discrete data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the discrete table and that have a proper entry in the timeseries table; refer to [addACTimeseries()] for how to add new stations. Does not work on any timeseries of category "continuous": for that, use [getNewContinuous()]. Timeseries with no specified souce_fx will be ignored.
#'
#' ## Making functions called by getNewDiscrete:
#' Each timeseries in the database has a source_fx column that specifies the function to be called to get new data and, optionally, function arguments specified in source_fx_args. Source functions must return a data.frame with the following mandatory columns:
#' - 'datetime': a POSIXct datetime object in UTC 0 time zone
#' - 'value': the value of the parameter at that datetime, as a numeric. Negative and positive values are allowed, and NA values are allowed if column result_condition is not NULL/NA. Values such as < DL or > DL should appear as NA with corresponding entries in the result_condition column.
#' 'sample_type': a numeric specifying the sample_type_id of the data point from table 'sample_types', such as 1 ('field msr/obs'), or 34 ('sample-routine').
#' 'result_value_type':  a numeric specifying the result_value_type_id of the data point from table 'result_value_types', such as 1 ('actual'), 2 ('blank corrected'), or 3 ('calculated').
#' 
#' Conditional columns are:
#' - 'result_condition': a numeric specifying the result condition of the data point from table 'result_conditions', such as "< DL" or "> DL". Only necessary if there are NA values in the 'value' column that should be interpreted as a specific condition. If not provided, rows with NA values will be dropped.
#' - 'result_condition_value': a numeric specifying the value of the result condition, such as 0.1 for "< DL 0.1". Necessary if column 'result_condition' is provided AND contains values of 1 or 2, i.e. 'Below Detection/Quantification Limit' or 'Above Detection/Quantification Limit'.
#' - 'sample_fraction': a numeric specifying the sample_fraction_id of the data point from table 'sample_fractions', such as 19 ('total'), 5 ('dissolved'), or 18 ('suspended'). Required if the column 'sample_fraction' in table 'parameters' is TRUE for the parameter in question.
#' - 'result_speciation': a numeric specifying the result_speciation_id of the data point from table 'result_speciations', such as 3 (as CaCO3), 5 (as CN), or 44 (of S). Required if the column 'result_speciation' in table 'parameters' is TRUE for the parameter in question.
#' 
#' Optional columns are:
#' - 'target_datetime': a POSIXct datetime object in UTC 0 time zone, specifying an artificial datetime for the data point which can be used for data analysis or plotting purposes.
#' 'collection_method': a numeric specifying the collection_method_id of the data point from table 'collection_methods', such as 1 (observation), 27 (water bottle), or 14 (pump).
#' -'lab': 
#' - 'protocol': 
#' - 'note': a character string with a note about the data point(s).
#' - 'contributor' the name of the person or organization that contributed the data, as a character string. This should match entries in the 'owners_contributors' table and an error will be thrown if it does not.
#' - 'owner': the owner of the data, as a character string. If not specified, the owner will be the owner of the timeseries. This should match entries in the 'owners_contributors' table and an error will be thrown if it does not.
#' - 'share_with': the user groups with which the data should be shared, as a character string. If not specified, the data will be shared with the same groups as the timeseries.
#' 
#' Additionally, functions must be able to handle the case where no new data is available and return an empty data.frame.
#' 
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location as entered in the 'timeseries' table, 'param_code' gets the parameter code defined in the 'settings' table, and start_datetime defaults to the instant after the last point already existing in the DB. Additional parameters can be passed using the "source_fx_args" column in the "timeseries" table; refer to [addACTimeseries()] for a description of how to formulate these arguments.
#' 
#' ## Sharing privileges and ownership
#' The parameters of column share_with of table timeseries will be used to determine which users will have access to the new data and the owner column will be used to determine the owner of the new data, unless the source function returns populated columns for owner and share_with.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'discrete'.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.

#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export
#'

getNewDiscrete <- function(con = AquaConnect(silent = TRUE), timeseries_id = "all", active = 'default') {
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  # Create table of timeseries
  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, period_type, record_rate, share_with, owner, active FROM timeseries WHERE category = 'discrete' AND source_fx IS NOT NULL;")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, period_type, record_rate, share_with, owner, active FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND category = 'discrete' AND source_fx IS NOT NULL;"))
    if (length(timeseries_id) != nrow(all_timeseries)) {
      warning("At least one of the timeseries IDs you called for cannot be found in the database, is not of category 'discrete', or has no function specified in column source_fx.")
    }
  }
  
  if (active == 'default') {
    all_timeseries <- all_timeseries[all_timeseries$active == TRUE, ]
  }

  count <- 0 #counter for number of successful new pulls
  success <- data.frame("location" = NULL, "parameter" = NULL, "timeseries" = NULL)

  # Run for loop over timeseries rows
  EQcon <- NULL #This prevents multiple connections to EQcon...
  snowCon <- NULL
  for (i in 1:nrow(all_timeseries)) {
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    period_type <- all_timeseries$period_type[i]
    record_rate <- all_timeseries$record_rate[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    share_with <- all_timeseries$share_with[i]
    owner <- all_timeseries$owner[i]
    
    if (source_fx == "downloadEQWin" & is.null(EQcon)) {
      EQcon <- EQConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(EQcon), add = TRUE)
    }
    if (source_fx == "downloadSnowCourse" & is.null(snowCon)) {
      snowCon <- snowConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
    }
    source_fx_args <- all_timeseries$source_fx_args[i]
    if (is.na(record_rate)) {
      param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = '", parameter, "' AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate IS NULL;"))[1,1]
    } else {
      param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = '", parameter, "' AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate = '", record_rate, "';"))[1,1]
    }
    last_data_point <- all_timeseries$end_datetime[i] + 1 #one second after the last data point

    tryCatch({
      args_list <- list(location = loc, param_code = param_code, start_datetime = last_data_point)
      # Connections to snow and eqwin are set before the source_fx_args are made, that way source_fx_args will override the same named param.
      if (source_fx == "downloadEQWin") {
        args_list[["EQcon"]] <- EQcon
      }
      if (source_fx == "downloadSnowCourse") {
        args_list[["snowCon"]] <- snowCon
        args_list[["ACCon"]] <- con
      }
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
      if (nrow(ts) > 0) {
        
        # Make sure that columns called 'sample_type' 'result_value_type' 'datetime' 'value' are present at minimum and that there are no NAs in all columns
        if (!("sample_type" %in% names(ts))) {
          stop("The source function did not return a column 'sample_type'.")
        } else {
          if (any(is.na(ts$sample_type))) {
            stop("The source function returned NA values in the column 'sample_type'.")
          }
        }
        if (!("result_value_type" %in% names(ts))) {
          stop("The source function did not return a column 'result_value_type'.")
        } else {
          if (any(is.na(ts$result_value_type))) {
            stop("The source function returned NA values in the column 'result_value_type'.")
          }
        }
        if (!("datetime" %in% names(ts))) {
          stop("The source function did not return a column 'datetime'.")
        } else {
          if (any(is.na(ts$datetime))) {
            stop("The source function returned NA values in the column 'datetime'.")
          }
        }
        
        # More complex check on 'value' column as it can contain NAs if there is a 'result_condition' column
        if (!("value" %in% names(ts))) {
          stop("The source function did not return a column 'value'.")
        }
        # if there are NAs in the 'value' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
        if (any(is.na(ts$value))) {
          if (!("result_condition" %in% names(ts))) {
            stop("The source function returned NA values in the column 'value' but did not return a column 'result_condition'.")
          } else { # Check that each NA in 'value' has a corresponding entry in 'result_condition'
            sub.ts <- ts[is.na(ts$value), ]
            check_result_condition <- FALSE # prevents repeatedly checking for the same thing
            
            for (j in 1:nrow(sub.ts)) {
              if (is.na(sub.ts$value[j]) & is.na(sub.ts$result_condition[j])) {
                stop("The source function returned at least one NA value in the column 'value' but did not return a corresponding entry in the column 'result_condition'.")
              } else {
                if (!check_result_condition) {
                  if (any(sub.ts$result_condition %in% c(1, 2))) {
                    if (!("result_condition_value" %in% names(ts))) {
                      stop("The source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value.")
                    }
                  }
                  check_result_condition <- TRUE
                }
                
                if (sub.ts$result_condition[j] %in% c(1, 2)) {
                  if (is.na(sub.ts$result_condition_value[j])) {
                    stop("The source function returned a value of 1 or 2 in the column 'result_condition' but did not return a corresponding entry in the column 'result_condition_value'.")
                  }
                }
              }
            } # End of loop over each row with NA in value column
          }
        } # End of additional checks is any NA values in 'value' column are returned
        
        
        # Check if the source function returned the owner and share_with columns. If not, use the ones from the timeseries table if they are not NA. If yes, use the ones from the source function, replacing NAs in these columns with the timeseries table values if they are not NA.
        if ("owner" %in% names(ts)) {
          if (!is.na(owner)) {
            ts$owner[is.na(ts$owner)] <- owner
          }
        } else {
          if (!is.na(owner)) {
            ts$owner <- owner
          }
        }
        if ("share_with" %in% names(ts)) {
          if (!is.na(share_with)) {
            ts$share_with[is.na(ts$share_with)] <- share_with
          }
        } else {
          if (!is.na(share_with)) {
            ts$share_with <- share_with
          }
        }
        
        # Get the result_speciation and sample_fraction boolean values for the parameter. If TRUE then ts must contain columns result_speciation and sample_fraction.
        result_speciation <- DBI::dbGetQuery(con, paste0("SELECT result_speciation FROM parameters WHERE param_code = '", parameter, "';"))[1,1]
        sample_fraction <- DBI::dbGetQuery(con, paste0("SELECT sample_fraction FROM parameters WHERE param_code = '", parameter, "';"))[1,1]
        if (result_speciation == TRUE) {
          if (!("result_speciation" %in% names(ts))) {
            stop("The source function did not return a column 'result_speciation' but the parameter in the database has result_speciation set to TRUE.")
          } else { # Check that all values in the result_speciation column are not NA
            if (any(is.na(ts$result_speciation))) {
              stop("The source function returned NA values in the column 'result_speciation' when the parameters table specifies that this should be populated.")
            }
          }
        }
        if (sample_fraction == TRUE) {
          if (!("sample_fraction" %in% names(ts))) {
            stop("The source function did not return a column 'sample_fraction' but the parameter in the database has sample_fraction set to TRUE.")
          } else { # Check that all values in the sample_fraction column are not NA
            if (any(is.na(ts$sample_fraction))) {
              stop("The source function returned NA values in the column 'sample_fraction' when the parameters table specifies that this should be populated.")
            }
          }
        }
        
        ts$timeseries_id <- tsid
        DBI::dbWithTransaction(
          con, {
            if (min(ts$datetime) < last_data_point - 1) { #This might happen because a source_fx is feeding in data before the requested datetime. Example: downloadSnowCourse if a new station is run in parallel with an old station, and the offset between the two used to adjust "old" measurements to the new measurements.
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
      warning("getNewDiscrete: Failed to get new data or to append new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], "). Error message: ", e$message)
    }) #End of tryCatch
  } #End of iteration over each location + param

  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_discrete'"))

  if (nrow(success) > 0) {
    return(success)
  }
}
