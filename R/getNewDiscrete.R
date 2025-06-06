#' Get new discrete-category data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new discrete data starting from the last data point in the local database, using the function specified in the sample_series table column "source_fx". Each sample series can also have a specified time range, allowing a certain location/sub-location to have multiple sample_series with different source functions and time ranges. The function will update the database in-place with the new data.
#'
#' ## Making functions called by getNewDiscrete:
#' Each sample_series_id in the database has a source_fx column that specifies the function to be called to get new data and, optionally, function arguments specified in source_fx_args. Source functions must return list of lists, with each list element containing two data.frames: one for sample metadata and one for associated results. The sample metadata data.frame must contain the following columns:
#' - 'location_id': a numeric specifying the location_id of the data point from table 'locations'.
#' - 'media_id': a numeric specifying the media_id of the data point from table 'medias'.
#' - 'datetime': a POSIXct datetime object in UTC 0 time zone, specifying the datetime of the data point.
#' - 'collection_method': a numeric specifying the collection_method_id of the data point from table 'collection_methods', such as 1 (observation), 27 (water bottle), or 14 (pump).
#' - 'sample_type': a numeric specifying the sample_type_id of the data point from table 'sample_types', such as 1 (grab), 2 (composite), or 3 (integrated).
#' - 'owner': the owner of the data, as a character string. This should match entries in the 'organizations' table and an error will be thrown if it does not.
#' - 'import_source_id': a numeric specifying the import_source_id of the data point from table 'import_sources' (use for tracking purposes).
#' Optional columns are:
#' - 'target_datetime': a POSIXct datetime object in UTC 0 time zone, specifying an artificial datetime for the data point which can be used for data analysis or plotting purposes.
#' - 'note': a character string with a note about the data point(s).
#' - 'contributor' the name of the person or organization that contributed the data, as a character string. This should match entries in the 'organizations' table and an error will be thrown if it does not.
#' - 'approval': the approval status of the data, as a character string. This should match entries in the 'approvals' table and an error will be thrown if it does not.
#' - 'grade': the grade of the data, as a character string. This should match entries in the 'grades' table and an error will be thrown if it does not.
#' - 'qualifier': the qualifier of the data, as a character string. This should match entries in the 'qualifiers' table and an error will be thrown if it does not.
#' 
#' 
#' The results data.frame must contain the following columns:
#' - 'parameter_id': a numeric specifying the parameter_id of the data point from table 'parameters'.
#' - 'result': a numeric specifying the sample's results, matched to the parameters
#' - 'result_condition': a numeric specifying the result condition of the data point from table 'result_conditions', such as "< DL" or "> DL". Only necessary if there are NA values in the 'result' column that should be interpreted as a specific condition. If not provided, rows with NA values will be dropped.
#' - 'result_condition_value': a numeric specifying the value of the result condition, such as 0.1 for "< DL 0.1". Necessary if column 'result_condition' is provided AND contains values of 1 or 2, i.e. 'Below Detection/Quantification Limit' or 'Above Detection/Quantification Limit'.
#' - 'sample_fraction': a numeric specifying the sample_fraction_id of the data point from table 'sample_fractions', such as 19 ('total'), 5 ('dissolved'), or 18 ('suspended'). Required if the column 'sample_fraction' in table 'parameters' is TRUE for the parameter in question.
#' - 'result_speciation': a numeric specifying the result_speciation_id of the data point from table 'result_speciations', such as 3 (as CaCO3), 5 (as CN), or 44 (of S). Required if the column 'result_speciation' in table 'parameters' is TRUE for the parameter in question.
#' 
#' Additionally, functions must be able to handle the case where no new data is available and return an empty list.
#' 
#'
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param location_id The location_ids you wish to have updated, as character or numeric vector. Defaults to NULL which will fetch data from all location_ids in the 'sample_series' table for all corresponding time ranges using the associated source functions (if more than one per location).
#' @param sub_location_id The sub_location_ids you wish to have updated, as character or numeric vector. Defaults to NULL which will fetch data from all sub_location_ids in the 'sample_series' table for all corresponding time ranges using the associated source functions (if more than one per location).
#' @param sample_series_id The sample_series_ids you wish to have updated, as character or numeric vector. Defaults to NULL, giving precedence to 'location_id'. This can be useful when wanting to synch all time ranges for a location that may have different sample_series_ids.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'sample_series' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#' @param snowCon A connection to the snow course database, created with [snowConnect()]. NULL will create a connection using the same connection host and port as the 'con' connection object and close it afterwards. Not used if no data is pulled from the snow database.
#' @param EQCon A connection to the EQWin database, created with [EQConnect()]. NULL will create a connection and close it afterwards. Not used if no data is pulled from the EQWin database.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export
#'

getNewDiscrete <- function(con = NULL, location_id = NULL, sub_location_id = NULL, sample_series_id = NULL, active = 'default', snowCon = NULL, EQCon = NULL) {
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  # Make sure that location_id and sample_series_id are not both NULL or both not NULL
  if (!is.null(location_id) & !is.null(sample_series_id)) {
    stop("location_id and sample_series_id cannot both be specified (not NULL)")
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  if (is.null(location_id)) {
    if (is.null(sample_series_id)) {
      all_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series WHERE (synch_to IS NULL OR synch_to < now())")
    } else {
      all_series <- DBI::dbGetQuery(con, paste0("SELECT * FROM sample_series WHERE sample_series_id IN (", paste(sample_series_id, collapse = ", "), ") AND (synch_to IS NULL OR synch_to < now())"))
      if (length(unique(sample_series_id)) != nrow(all_series)) {
        fail <- sample_series_id[!sample_series_id %in% all_series$sample_series_id]
        ifelse((length(fail) == 1),
               warning("Could not find one of the sample_series_ids that you specified: ID ", fail, " is missing from the database."),
               warning("Could not find some of the sample_series_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
        )
      }
    }
  } else {
    if (is.null(sub_location_id)) {
      all_series <- DBI::dbGetQuery(con, paste0("SELECT * FROM sample_series WHERE location_id IN (", paste(location_id, collapse = ", "), ") AND (synch_to IS NULL OR synch_to < now())"))
    } else {
      all_series <- DBI::dbGetQuery(con, paste0("SELECT * FROM sample_series WHERE location_id IN (", paste(location_id, collapse = ", "), ") AND sub_location_id IN (", paste(sub_location_id, collapse = ", "), ") AND (synch_to IS NULL OR synch_to < now())"))
    }
    if (length(unique(location_id)) != nrow(all_series)) {
      fail <- location_id[!location_id %in% all_series$location_id]
      ifelse((length(fail) == 1),
             warning("Could not find one of the sample_series_ids that you specified: ID ", fail, " is missing from the database."),
             warning("Could not find some of the sample_series_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
  }
  
  
  if (active == 'default') {
    all_series <- all_series[all_series$active, ]
  }

  count <- 0 #counter for number of successful new pulls (samples - not individual results)

  # Run for loop over timeseries rows
  message("Fetching new discrete data with getNewDiscrete...")
  
  # Define a function to commit the data to the database, used later for each sample
  commit_fx <- function(con, sample, results) {
    
    # Insert the sample data
    DBI::dbAppendTable(con, "samples", sample)
    
    # Get the sample_id
    sample_id <- DBI::dbGetQuery(con, paste0("SELECT sample_id FROM samples WHERE location_id = ", sample$location_id, " AND datetime = '", sample$datetime, " UTC';"))[1,1]
    
    # Insert the results data
    results$sample_id <- sample_id
    DBI::dbAppendTable(con, "results", results)
    
    return(sample_id)
  }
  
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_series), style = 3)
  }
  for (i in 1:nrow(all_series)) {
    loc_id <- all_series$location_id[i]
    sub_loc_id <- all_series$sub_location_id[i]
    sid <- all_series$sample_series_id[i]
    source_fx <- all_series$source_fx[i]
    source_fx_args <- all_series$source_fx_args[i]
    share_with <- all_series$share_with[i]
    owner <- all_series$default_owner[i]
    contributor <- all_series$default_contributor[i]
    range_start <- all_series$synch_from[i]
    range_end <- all_series$synch_to[i]
    
    # Find the last data point
    query <- paste0("SELECT MAX(datetime) FROM samples WHERE location_id = ", loc_id)
    if (!is.na(sub_loc_id)) query <- paste0(query, " AND sub_location_id = ", sub_loc_id)
    if (!is.na(range_start)) query <- paste0(query, " AND datetime > '", as.character(range_start), " UTC'")
    last_data_point <- DBI::dbGetQuery(con, paste0(query, ";"))[1, 1] + 1
    
    if (is.na(last_data_point)) { # Means we're dealing with a location that has no samples in yet - probably just created
      last_data_point <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
    }
    
    if (source_fx == "downloadEQWin" & is.null(EQCon)) {
      EQCon <- EQConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(EQCon), add = TRUE)
    }
    if (source_fx == "downloadSnowCourse" & is.null(snowCon)) {
      # Try with the same host and port as the AquaCache connection
      dets <-  DBI::dbGetQuery(con, "SELECT inet_server_addr() AS ip, inet_server_port() AS port")
      snowCon <- snowConnect(host = dets$ip, port = dets$port, silent = TRUE)
      on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
    }

    tryCatch({
      args_list <- list(con = con, start_datetime = last_data_point, end_datetime = if (is.na(range_end)) Sys.time() else range_end)
      # Connections to snow and eqwin are set before the source_fx_args are made, that way source_fx_args will override the same named param.
      if (source_fx == "downloadEQWin") {
        args_list[["EQCon"]] <- EQCon
      }
      if (source_fx == "downloadSnowCourse") {
        args_list[["snowCon"]] <- snowCon
      }
      if (!is.na(source_fx_args)) { #add some arguments if they are specified
        args <- jsonlite::fromJSON(source_fx_args)
        args_list <- c(args_list, lapply(args, as.character))
      }
      
      ## Get the data ##############
      data <- do.call(source_fx, args_list) #Get the data using the args_list
      
      if (length(data) == 0) {
        next
      }
      
      if (!inherits(data, "list")) {
        stop("For sample_series_id ", sid, " the source function did not return a list.")
      } else if (!inherits(data[[1]], "list")) {
        stop("For sample_series_id ", sid, " the source function did not return a list of lists (one element per sample, with two data.frames: one for sample metadata, the other for associated results).")
      }
      
        # Work on each list element to populate the 'samples' and 'results' tables
        for (j in 1:length(data)) {
          if (!("sample" %in% names(data[[j]])) | !("results" %in% names(data[[j]]))) {
            warning("For sample_series_id ", sid, " the source function did not return a list with elements named 'sample' and 'results'. Failed on list element ", j, ". Skipping to next element.")
            next
          }
          
          # Make sure that results element has at least one row
          if (nrow(data[[j]][["results"]]) == 0) {
            next
          }
          
          
          ## Checks on sample metadata ###########
          # Ensure the sample data has required minimum columns
          sample <- data[[j]][["sample"]]
          
          # Functions may pass the location code instead of location_id, change it
          names_samp <- names(sample)
          if ("location" %in% names_samp) {
            sample$location_id <- loc_id
            sample$location <- NULL
            names_samp <- names(sample)
          }
          if ("sub_location" %in% names_samp) {
            sample$sub_location_id <- sub_loc_id
            sample$sub_location <- NULL
            names_samp <- names(sample)
          }
          # Check that the sample data has the required columns at minimum: c("location_id", "media_id", "datetime", "collection_method", "sample_type", "owner", "import_source_id"). Note that import_source_id is only mandatory because this function pulls data in from a remote source
          mandatory_samp <- c("location_id", "media_id", "datetime", "collection_method", "sample_type", "owner", "import_source_id")
          if (!all(c(mandatory_samp) %in% names_samp)) {
            # Make an error message stating which column is missing
            missing <- c(mandatory_samp)[!c(mandatory_samp) %in% names_samp]
            warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function did not return one or more mandatory column(s) for the sample metadata: '", paste(missing, collapse = "', '"), "' Skipping to next sample.")
            next
          }
          
          sample$import_source <- source_fx
          
          # Checks on sample results ############
          # Ensure the results have required minimum columns
          results <- data[[j]][["results"]]
          names_res <- names(results)
          # Check that the results have the mandatory columns
          mandatory_res <- c("result", "result_type", "parameter_id")
          if (!all(c(mandatory_res) %in% names_res)) {
            # Make an error message stating which column is missing
            missing <- c(mandatory_res)[!c(mandatory_res) %in% names_res]
            warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function did not return one or more mandatory column(s) for the results: '", paste(missing, collapse = "', '"), "'. Skipping to next sample.")
            next
          }
          
          # More complex checks if 'result' is NA
          # if there are NAs in the 'result' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
          if (any(is.na(results$result))) {
            if (!("result_condition" %in% names_res)) {
              warning("For sample_series_id ", sid, ", sample ", j, " (sample_datetime ", sample$datetime, ") the source function returned NA values in the column 'result' but did not return a column called 'result_condition'. Skipping to next sample.")
              next
            } else { # Check that each NA in 'result' has a corresponding entry in 'result_condition'
              sub.results <- results[is.na(results$result), ]
              check_result_condition <- FALSE # prevents repeatedly checking for the same thing
              
              next_flag <- FALSE
              for (k in 1:nrow(sub.results)) {
                if (is.na(sub.results$result[k]) & is.na(sub.results$result_condition[k])) {
                  warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function returned at least one NA result in the column 'result' but did not return a corresponding entry in the column 'result_condition'. Skipping to next sample.")
                  next_flag <- TRUE
                } else {
                  if (!check_result_condition) {
                    if (any(sub.results$result_condition %in% c(1, 2))) {
                      if (!("result_condition_value" %in% names(results))) {
                        warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value. Skipping to next sample.")
                        next_flag <- TRUE
                      }
                    }
                    check_result_condition <- TRUE
                  }
                  
                  if (sub.results$result_condition[k] %in% c(1, 2)) {
                    if (is.na(sub.results$result_condition_value[k])) {
                      warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function returned a value of 1 or 2 in the column 'result_condition' (indicating above or below detection limit) but did not return a corresponding entry in the column 'result_condition_value'. Skipping to the next sample.")
                      next_flag <- TRUE
                    }
                  }
                }
              } # End of looping over each row with NA in result column
            }
            if (next_flag) {
              next
            }
          } # End of additional checks fs any NA values in 'result' column are returned
          
          
          # Get the result_speciation and sample_fraction boolean values for the parameters. If at least one TRUE then data must contain columns result_speciation and sample_fraction.
          result_speciation <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, result_speciation AS result_speciation_bool FROM parameters WHERE parameter_id IN (", paste(unique(results$parameter_id), collapse = ", "), ");"))
          sample_fraction <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM parameters WHERE parameter_id IN (", paste(unique(results$parameter_id), collapse = ", "), ");"))
          if (any(result_speciation$result_speciation_bool)) {
            if (!("result_speciation" %in% names(data))) {
              warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function did not return a column 'result_speciation' but the database mandates this for at least one of the parameters. Skipping to next sample.")
              next
            } else { # Check that values in the result_speciation column are not NA where necessary
              merge <- merge(results, result_speciation, by = "parameter_id")
              # For rows where result_speciation_bool is TRUE, check that the corresponding result_speciation column is not NA
              chk <- with(merge, result_speciation_bool & is.na(result_speciation))
              if (any(chk)) {
                warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function returned NA values in the column 'result_speciation' for at least one parameter where the database mandates this value. Skipping to next sample.")
                next
              }
            }
          }
          if (any(sample_fraction$sample_fraction_bool)) {
            if (!("sample_fraction" %in% names(data))) {
              warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function did not return a column 'sample_fraction' but the database mandates this for at least one of the parameters. Skipping to next sample.")
              next
            } else { # Check that all values in the sample_fraction column are not NA where necessary
              merge <- merge(results, sample_fraction, by = "parameter_id")
              # For rows where sample_fraction_bool is TRUE, check that the corresponding sample_fraction column is not NA
              chk <- with(merge, sample_fraction_bool & is.na(sample_fraction))
              if (any(chk)) {
                warning("For sample_series_id ", sid, " element ", j, " (sample_datetime ", sample$datetime, ") the source function returned NA values in the column 'sample_fraction' for at least one parameter where the database mandates this value. Skipping to next sample.")
                next
              }
            }
          }
          
          
          # Append values in a transaction block ##########
          activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
          if (activeTrans) {
            tryCatch({
              commit_fx(con, sample, results)
              DBI::dbExecute(con, "COMMIT;")
              count <- count + 1
            }, error = function(e) {
              DBI::dbExecute(con, "ROLLBACK;")
              warning("getNewDiscrete: Failed to commit new data for sample_series_id, ", sid, ". Error message: ", e$message)
            })
          } else { # we're already in a transaction
            commit_fx(con, sample, results)
            count <- count + 1
          }
          
        } # End of looping over each list element (sample) for a sample_series_id
      DBI::dbExecute(con, paste0("UPDATE sample_series SET last_new_data = now() WHERE sample_series_id = ", sid, ";")) # Update the last new data column
    }, error = function(e) {
      warning("getNewDiscrete: Failed to get new data or to append new data for sample_series_id ", sid, ". Error message: ", e$message)
    }) #End of tryCatch
    
    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
    
  } # End of for loop
  
  if (interactive()) {
    close(pb)
  }

  message(count, " samples were found for the ", nrow(all_series), " sample_series specified.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_discrete'"))

}
