#' Get new discrete-category data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new discrete data starting from the last data point in the local database, using the function specified in the sample_series table column "source_fx".
#'
#' ## Making functions called by getNewDiscrete:
#' Each timeseries in the database has a source_fx column that specifies the function to be called to get new data and, optionally, function arguments specified in source_fx_args. Source functions must return a data.frame with the following mandatory columns:
#' - 'datetime': a POSIXct datetime object in UTC 0 time zone
#' - 'value': the value of the parameter at that datetime, as a numeric. Negative and positive values are allowed, and NA values are allowed if column result_condition is not NULL/NA. Values such as < DL or > DL should appear as NA with corresponding entries in the result_condition column.
#' 'sample_type': a numeric specifying the sample_type_id of the data point from table 'sample_types', such as 1 ('sample-field msr/obs'), or 34 ('sample-routine').
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
#' - 'contributor' the name of the person or organization that contributed the data, as a character string. This should match entries in the 'organizations' table and an error will be thrown if it does not.
#' - 'owner': the owner of the data, as a character string. If not specified, the owner will be the owner of the timeseries. This should match entries in the 'organizations' table and an error will be thrown if it does not.
#' - 'approval': the approval status of the data, as a character string. This should match entries in the 'approvals' table and an error will be thrown if it does not.
#' - 'grade': the grade of the data, as a character string. This should match entries in the 'grades' table and an error will be thrown if it does not.
#' - 'qualifier': the qualifier of the data, as a character string. This should match entries in the 'qualifiers' table and an error will be thrown if it does not.
#' - 'share_with': the user groups with which the data should be shared, as a character string. If not specified, the data will be shared with the same groups as the sample_series
#' 
#' Additionally, functions must be able to handle the case where no new data is available and return an empty data.frame.
#' 
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location as entered in the 'sample_series' table, 'parameter_id' gets the parameter code defined in the 'fetch_settings' table, and start_datetime defaults to the instant after the last point already existing in the DB. Additional parameters can be passed using the "source_fx_args" column in the "sample_series" table; refer to [addACTimeseries()] for a description of how to formulate these arguments.
#' 
#' ## Sharing privileges and ownership
#' The parameters of column share_with of table sample_series will be used to determine which users will have access to the new data and the owner column will be used to determine the owner of the new data, unless the source function returns populated columns for owner and share_with.
#'
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param location_id The location_ids you wish to have updated, as character or numeric vector. Defaults to "all" which will fetch data from all location_ids in the 'sample_series' table for all corresponding time ranges using the associated source functions (if more than one per location).
#' @param sub_location_id The sub_location_ids you wish to have updated, as character or numeric vector. Defaults to "all" which will fetch data from all sub_location_ids in the 'sample_series' table for all corresponding time ranges using the associated source functions (if more than one per location).
#' @param sample_series_id The sample_series_ids you wish to have updated, as character or numeric vector. Defaults to NULL, giving precedence to 'location_id'. This can be useful when wanting to synch all time ranges for a location that may have different sample_series_ids.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'sample_series' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export
#'

getNewDiscrete <- function(con = NULL, location_id = "all", sub_location_id = "all", sample_series_id = NULL, active = 'default') {
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  # Make sure that location_id and sample_series_id are not both NULL or both not NULL
  if (is.null(location_id) & is.null(sample_series_id)) {
    stop("location_id and sample_series_id cannot both be NULL")
  }
  if (!is.null(location_id) & !is.null(sample_series_id)) {
    stop("location_id and sample_series_id cannot both be specified (not NULL)")
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  if (is.null(location_id)) {
    if (sample_series_id[1] == "all") {
      all_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series WHERE (synch_to IS NULL OR synch_to < now())")
    } else {
      all_series <- DBI::dbExecute(con, "SELECT * FROM sample_series WHERE sample_series_id IN ('", paste(sample_series_id, collapse = "', '"), "') AND (synch_to IS NULL OR synch_to < now())")
      if (length(unique(sample_series_id)) != nrow(all_series)) {
        fail <- sample_series_id[!sample_series_id %in% all_series$sample_series_id]
        ifelse((length(fail) == 1),
               warning("Could not find one of the sample_series_ids that you specified: ID ", fail, " is missing from the database."),
               warning("Could not find some of the sample_series_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
        )
      }
    }
  } else {
    if (location_id[1] == "all") {
      all_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series WHERE (synch_to IS NULL OR synch_to < now())")
    } else {
      all_series <- DBI::dbExecute(con, "SELECT * FROM sample_series WHERE location_id_id IN ('", paste(location_id, collapse = "', '"), "') AND (synch_to IS NULL OR synch_to < now())")
      if (length(unique(location_id)) != nrow(all_series)) {
        fail <- location_id[!location_id %in% all_series$location_id]
        ifelse((length(fail) == 1),
               warning("Could not find one of the sample_series_ids that you specified: ID ", fail, " is missing from the database."),
               warning("Could not find some of the sample_series_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
        )
      }
    }
  }

  
  if (active == 'default') {
    all_series <- all_series[all_series$active, ]
  }

  count <- 0 #counter for number of successful new pulls (samples - not individual results)
  success <- data.frame("location" = NULL, "parameter" = NULL, "timeseries" = NULL)

  # Run for loop over timeseries rows
  message("Fetching new discrete data with getNewDiscrete...")
  
  EQcon <- NULL #This prevents multiple connections to EQcon...
  snowCon <- NULL
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_series), style = 3)
  }
  for (i in 1:nrow(all_series)) {
    loc <- all_series$location_id[i]
    sub_loc <- all_series$sub_location_id[i]
    sid <- all_series$sample_series_id[i]
    source_fx <- all_series$source_fx[i]
    source_fx_args <- all_series$source_fx_args[i]
    share_with <- all_series$share_with[i]
    owner <- all_series$default_owner[i]
    contributor <- all_series$default_contributor[i]
    range_start <- all_series$synch_from[i]
    range_end <- all_series$synch_to[i]
    last_data_point <- DBI::dbGetQuery(con, paste0("SELECT MAX(datetime) FROM samples WHERE location_id = ", loc, " AND sub_location_id = ", sub_loc, ";"))[1,1] + 1
    
    if (source_fx == "downloadEQWin" & is.null(EQcon)) {
      EQcon <- EQConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(EQcon), add = TRUE)
    }
    if (source_fx == "downloadSnowCourse" & is.null(snowCon)) {
      snowCon <- snowConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
    }
    source_fx_args <- all_series$source_fx_args[i]

    tryCatch({
      args_list <- list(con = con, location = loc, sub_location = sub_loc, start_datetime = last_data_point)
      # Connections to snow and eqwin are set before the source_fx_args are made, that way source_fx_args will override the same named param.
      if (source_fx == "downloadEQWin") {
        args_list[["EQcon"]] <- EQcon
      }
      if (source_fx == "downloadSnowCourse") {
        args_list[["snowCon"]] <- snowCon
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
      
      data <- do.call(source_fx, args_list) #Get the data using the args_list
      if (nrow(data) > 0) {
        
        # Make sure that columns called 'sample_type' 'result_value_type' 'datetime' 'parameter_id' 'result' are present at minimum and that there are no NAs in all columns except for result (but then result_condition must be present)
        if (!("sample_type" %in% names(data))) {
          stop("The source function did not return a column 'sample_type'.")
        } else {
          if (any(is.na(data$sample_type))) {
            stop("The source function returned NA values in the column 'sample_type'.")
          }
        }
        if (!("result_value_type" %in% names(data))) {
          stop("The source function did not return a column 'result_value_type'.")
        } else {
          if (any(is.na(data$result_value_type))) {
            stop("The source function returned NA values in the column 'result_value_type'.")
          }
        }
        if (!("datetime" %in% names(data))) {
          stop("The source function did not return a column 'datetime'.")
        } else {
          if (any(is.na(data$datetime))) {
            stop("The source function returned NA values in the column 'datetime'.")
          }
        }
        if (!("parameter_id" %in% names(data))) {
          stop("The source function did not return a column 'parameter_id'.")
        } else {
          if (any(is.na(data$parameter_id))) {
            stop("The source function returned NA values in the column 'parameter_id'.")
          }
        }
        
        # More complex check on 'value' column as it can contain NAs if there is a 'result_condition' column
        if (!("value" %in% names(data))) {
          stop("The source function did not return a column 'value'.")
        }
        # if there are NAs in the 'value' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
        if (any(is.na(data$value))) {
          if (!("result_condition" %in% names(data))) {
            stop("The source function returned NA values in the column 'value' but did not return a column 'result_condition'.")
          } else { # Check that each NA in 'value' has a corresponding entry in 'result_condition'
            sub.data <- data[is.na(data$value), ]
            check_result_condition <- FALSE # prevents repeatedly checking for the same thing
            
            for (j in 1:nrow(sub.data)) {
              if (is.na(sub.data$value[j]) & is.na(sub.data$result_condition[j])) {
                stop("The source function returned at least one NA value in the column 'value' but did not return a corresponding entry in the column 'result_condition'.")
              } else {
                if (!check_result_condition) {
                  if (any(sub.data$result_condition %in% c(1, 2))) {
                    if (!("result_condition_value" %in% names(data))) {
                      stop("The source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value.")
                    }
                  }
                  check_result_condition <- TRUE
                }
                
                if (sub.data$result_condition[j] %in% c(1, 2)) {
                  if (is.na(sub.data$result_condition_value[j])) {
                    stop("The source function returned a value of 1 or 2 in the column 'result_condition' (indicating above or below detection limit) but did not return a corresponding entry in the column 'result_condition_value'.")
                  }
                }
              }
            } # End of looping over each row with NA in value column
          }
        } # End of additional checks is any NA values in 'value' column are returned
        
        
        # Get the result_speciation and sample_fraction boolean values for the parameter. If TRUE then data must contain columns result_speciation and sample_fraction.
        result_speciation <- DBI::dbGetQuery(con, paste0("SELECT result_speciation FROM parameters WHERE parameter_id = '", parameter, "';"))[1,1]
        sample_fraction <- DBI::dbGetQuery(con, paste0("SELECT sample_fraction FROM parameters WHERE parameter_id = '", parameter, "';"))[1,1]
        if (result_speciation) {
          if (!("result_speciation" %in% names(data))) {
            stop("The source function did not return a column 'result_speciation' but the parameter in the database has result_speciation set to TRUE.")
          } else { # Check that all values in the result_speciation column are not NA
            if (any(is.na(data$result_speciation))) {
              stop("The source function returned NA values in the column 'result_speciation' when the parameters table specifies that this should be populated.")
            }
          }
        }
        if (sample_fraction) {
          if (!("sample_fraction" %in% names(data))) {
            stop("The source function did not return a column 'sample_fraction' but the parameter in the database has sample_fraction set to TRUE.")
          } else { # Check that all values in the sample_fraction column are not NA
            if (any(is.na(data$sample_fraction))) {
              stop("The source function returned NA values in the column 'sample_fraction' when the parameters table specifies that this should be populated.")
            }
          }
        }
        
        data$timeseries_id <- sid
        
        # Adjust owner, contributor, approval information
        adjust_owner(con = con, timeseries_id = tsid, data = data[, c("datetime", "owner")])  # Owner is always present, defaulting to the timeseries table value if not in the data
        if ("contributor" %in% names(data)) {
          adjust_contributor(con = con, timeseries_id = tsid, data = data[, c("datetime", "contributor")])
        }
        if ("approval" %in% names(data)) {
          adjust_approval(con = con, timeseries_id = tsid, data = data[, c("datetime", "approval")])
        }
        if ("grade" %in% names(data)) {
          adjust_grade(con = con, timeseries_id = tsid, data = data[, c("datetime", "grade")])
        }
        if ("qualifier" %in% names(data)) {
          adjust_qualifier(con = con, timeseries_id = tsid, data = data[, c("datetime", "qualifier")])
        }
        data <- data[ , -which(names(data) %in% c("owner", "contributor", "approval", "grade", "qualifier"))]
        
        # Now commit the changes to the database
        commit_fx <- function(con, data, last_data_point, tsid) {
          
          if (min(data$datetime) < last_data_point - 1) { #This might happen because a source_fx is feeding in data before the requested datetime. Example: downloadSnowCourse if a new station is run in parallel with an old station, and the offset between the two used to adjust "old" measurements to the new measurements.
            DBI::dbExecute(con, paste0("DELETE FROM measurements_discrete WHERE datetime >= '", min(data$datetime), "' AND timeseries_id = ", tsid, ";"))
          }
          DBI::dbAppendTable(con, "measurements_discrete", data)
          #make the new entry into table timeseries
          DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(data$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
        }
        
        if (!attr(con, "active_transaction")) {
          DBI::dbBegin(con)
          attr(con, "active_transaction") <- TRUE
          tryCatch({
            commit_fx(con, data, last_data_point, tsid)
            DBI::dbCommit(con)
            attr(con, "active_transaction") <- FALSE
            count <- count + 1
            success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "timeseries_id" = tsid))
          }, error = function(e) {
            DBI::dbRollback(con)
            attr(con, "active_transaction") <<- FALSE
            warning("getNewDiscrete: Failed to commit new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], "). Error message: ", e$message)
          })
        } else { # we're already in a transaction
          commit_fx(con, data, last_data_point, tsid)
          count <- count + 1
          success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "timeseries_id" = tsid))
        }
        
      }
    }, error = function(e) {
      warning("getNewDiscrete: Failed to get new data or to append new data at location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], "). Error message: ", e$message)
    }) #End of tryCatch
    
    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
    
  } # End of for loop
  
  if (interactive()) {
    close(pb)
  }

  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_discrete'"))

  if (nrow(success) > 0) {
    return(success)
  }
}
