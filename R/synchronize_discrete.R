#' Synchronize hydro DB with remote sources
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This synchronize function pulls and replaces data referenced in table 'sample_series' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence.
#' 
#' @details
#' Deleting sample data found in AquaCache but not in the remote sources is done with the following logic: each sample_series_id is checked for any data found on the remote using the source_fx and the synch_from and synch_to datetimes assigned in table 'sample_series'. Any samples not found in the remote are deleted from the local database if the 'import_source' of the new and existing samples match. If the 'import_source' of the new and existing samples do not match, the sample is not deleted.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param sample_series_id The sample_series_id you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `sample_series_id`, or one per element of `sample_series_id`
#' @param active Sets behavior for checking sample_series_ids or not. If set to 'default', the function will look to the column 'active' in the 'sample_series_id' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all sample_series_id
#' @param delete If TRUE, the function will delete any samples and/or results that are not found in the remote source IF these samples are labelled in column 'import_source' as having the same import source. If FALSE, the function will not delete any data. See details for more info.
#' @param snowCon A connection to the snow course database, created with [snowConnect()]. NULL will create a connection using the same connection host and port as the 'con' connection object and close it afterwards. Not used if no data is pulled from the snow database.
#' @param EQCon A connection to the EQWin database, created with [EQConnect()]. NULL will create a connection and close it afterwards. Not used if no data is pulled from the EQWin database.
#'
#' @return Updated entries in the hydro database.
#' @export
#'

synchronize_discrete <- function(con = NULL, sample_series_id = "all", start_datetime, active = 'default', delete = FALSE, snowCon = NULL, EQCon = NULL)
{
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  if (inherits(start_datetime, "Date")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else if (inherits(start_datetime, "character")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else if (!inherits(start_datetime, "POSIXct")) {
    stop("start_datetime must be a Date, character, or POSIXct object.")
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  start <- Sys.time()
  
  message("Synchronizing sample series with synchronize_discrete...")
  
  #Check length of start_datetime is either 1 of same as sample_series_id
  if (length(start_datetime) != 1) {
    if (length(start_datetime) != length(sample_series_id)) {
      stop("There is not exactly one element to start_datetime per valid sample_series_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for sample_series_id that doesn't exist.")
    }
  } else {
    sample_series_id <- unique(sample_series_id)
  }
  
  
  if (sample_series_id[1] == "all") {
    all_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series;")
  } else {
    all_series <- DBI::dbGetQuery(con, paste0("SELECT * FROM sample_series WHERE sample_series_id IN (", paste(sample_series_id, collapse = ", "), ");"))
    if (length(unique(sample_series_id)) != nrow(all_series)) {
      fail <- sample_series_id[!sample_series_id %in% all_series$sample_series_id]
      ifelse((length(fail) == 1),
             warning("Could not find one of the sample_series_ids that you specified: ID ", fail, " is missing from the database."),
             warning("Could not find some of the sample_series_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
  }
  
  if (active == 'default') {
    all_series <- all_series[all_series$active, ]
  }
  
  grade_unknown <- DBI::dbGetQuery(con, "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNK';")[1,1]
  if (is.na(grade_unknown)) {
    stop("synchronize: Could not find grade type 'Unknown' in the database.")
  }
  approval_unknown <- DBI::dbGetQuery(con, "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNK';")[1,1]
  if (is.na(approval_unknown)) {
    stop("synchronize: Could not find approval type 'Unknown' in the database.")
  }
  qualifier_unknown <- DBI::dbGetQuery(con, "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNK';")[1,1]
  if (is.na(qualifier_unknown)) {
    stop("synchronize: Could not find qualifier type 'Unknown' in the database.")
  }
  
  updated <- 0 #Counter for number of updated timeseries
  EQCon <- NULL #This prevents multiple connections to EQCon...
  snowCon <- NULL # ...and snowCon
  valid_sample_names <- DBI::dbGetQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_schema = 'discrete' AND table_name = 'samples';")[,1]
  valid_result_names <- DBI::dbGetQuery(con, "SELECT column_name FROM information_schema.columns WHERE table_schema = 'discrete' AND table_name = 'results';")[,1]
  
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
    sid <- all_series$sample_series_id[i]
    loc_id <- all_series$location_id[i]
    sub_loc_id <- all_series$sub_location_id[i]
    synch_from <- all_series$synch_from[i]
    synch_to <- all_series$synch_to[i]
    source_fx <- all_series$source_fx[i]
    source_fx_args <- all_series$source_fx_args[i]
    default_owner <- all_series$default_owner[i]
    default_contributor <- all_series$default_contributor[i]
    
    # Location codes (not numeric IDs) are needed for the source functions
    loc_code <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location_id = ", loc_id, ";"))[1,1]
    sub_loc_code <- if (!is.na(sub_loc_id)) {DBI::dbGetQuery(con, paste0("SELECT sub_location_name FROM sub_locations WHERE sub_location_id = ", sub_loc_id, ";"))}[1,1]
    
    # start/end datetime for the sample series
    start_i <- if (!is.na(synch_from)) min(start_datetime, synch_from) else start_datetime
    end_i <- if (!is.na(synch_to)) synch_to else Sys.time()
    
    # both functions downloadEQWin and downloadSnowCourse can establish their own connections, but this is repetitive and inefficient. Instead, we make the connection once and pass the connection to the function.
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
      args_list <- list(location = loc_code, sub_location = sub_loc_code, start_datetime = start_i, end_datetime = end_i, con = con)
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
      
      if (source_fx == "downloadEQWin") {
        args_list[["EQCon"]] <- EQCon
      }
      if (source_fx == "downloadSnowCourse") {
        args_list[["snowCon"]] <- snowCon
      }
      
      inRemote <- do.call(source_fx, args_list) #Get the data using the args_list
      
      if (!inherits(inRemote, "list")) {
        stop("For sample_series_id ", sid, " the source function did not return a list.")
      } else if (!inherits(inRemote[[1]], "list")) {
        stop("For sample_series_id ", sid, " the source function did not return a list of lists (one element per sample, with two data.frames: one for sample metadata, the other for associated results).")
      }
      
      if (delete) {
        # Extract the 'datetime' of each sample in the list (make a blank element if it's not found)
        inRemote_datetimes <- lapply(inRemote, function(x) if ("sample" %in% names(x)) x$sample$datetime else NA)
      }
      
      if (length(inRemote) > 0) {
        for (j in 1:length(inRemote)) {
          
          if (!("sample" %in% names(inRemote[[j]])) | !("results" %in% names(inRemote[[j]]))) {
            warning("For sample_series_id ", sid, " the source function did not return a list with elements named 'sample' and 'results'. Failed on list element ", j, ", moving on to next element.")
            next
          }
          
          inRemote_sample <- inRemote[[j]][["sample"]]
          inRemote_results <- inRemote[[j]][["results"]]
          names_inRemote_samp <- names(inRemote_sample)
          names_inRemote_res <- names(inRemote_results)
          
          if (delete) {
            if (j == 1) {
              # Delete any samples between the start of the series and the first sample in the remote data, if any. Cascades to results.
              DBI::dbExecute(con, paste0("DELETE FROM samples WHERE datetime > '", start_i, "' AND datetime < '", inRemote_datetimes[[j]], "' AND location_id = ", loc_id, " AND sub_location_id ", if (!is.na(sub_loc_id)) paste0("= ", sub_loc_id) else "IS NULL", " AND z ", if (!is.null(inRemote_sample$z)) paste0("= ", inRemote_sample$z) else "IS NULL", " AND media_id = ", inRemote_sample$media_id, " AND sample_type = ", inRemote_sample$sample_type, " AND collection_method = ", inRemote_sample$collection_method, " AND import_source = '", source_fx, "' AND no_update IS FALSE;"))
            } else if (j == length(inRemote)) {
              DBI::dbExecute(con, paste0("DELETE FROM samples WHERE datetime < '", end_i, "' AND datetime > '", inRemote_datetimes[[j]], "' AND location_id = ", loc_id, " AND sub_location_id ", if (!is.na(sub_loc_id)) paste0("= ", sub_loc_id) else "IS NULL", " AND z ", if (!is.null(inRemote_sample$z)) paste0("= ", inRemote_sample$z) else "IS NULL", " AND media_id = ", inRemote_sample$media_id, " AND sample_type = ", inRemote_sample$sample_type, " AND collection_method = ", inRemote_sample$collection_method, " AND import_source = '", source_fx, "' AND no_update IS FALSE;"))
            } else {
              DBI::dbExecute(con, paste0("DELETE FROM samples WHERE datetime BETWEEN '", inRemote_datetimes[[j - 1]] + 1, "' AND '", inRemote_datetimes[[j]] - 1, "' AND location_id = ", loc_id, " AND sub_location_id ", if (!is.na(sub_loc_id)) paste0("= ", sub_loc_id) else "IS NULL", " AND z ", if (!is.null(inRemote_sample$z)) paste0("= ", inRemote_sample$z) else "IS NULL", " AND media_id = ", inRemote_sample$media_id, " AND sample_type = ", inRemote_sample$sample_type, " AND collection_method = ", inRemote_sample$collection_method, " AND import_source = '", source_fx, "' AND no_update IS FALSE;"))
            }
          }
          
          if (nrow(inRemote_results) == 0) {
            next
          }
          
          inDB_sample <- DBI::dbGetQuery(con, paste0("SELECT * FROM samples WHERE datetime = '", inRemote_sample$datetime, "' AND location_id = ", loc_id, " AND sub_location_id ", if (!is.na(sub_loc_id)) paste0("= ", sub_loc_id) else "IS NULL", " AND z ", if (!is.null(inRemote_sample$z)) paste0("= ", inRemote_sample$z) else "IS NULL", " AND media_id = ", inRemote_sample$media_id, " AND sample_type = ", inRemote_sample$sample_type, " AND collection_method = ", inRemote_sample$collection_method, ";"))
          
          
          # Check for any changes/additions/subtractions to the sample metadata
          # If changes are detected, update the sample metadata
          if (nrow(inDB_sample) > 0) { # Check existing DB sample and results. If no sample is found, add the sample and corresponding results in else section
            if (inDB_sample$no_update) { # If no_update is TRUE, skip to the next sample
              next
            }
            # Check existing DB sample and results ##################
            ## Check sample metadata ##############
            for (k in names_inRemote_samp) {
              # Ensure the name is a valid DB one
              if (k %in% valid_sample_names) { # If TRUE, check for differences
                inDB_k <- inDB_sample[[k]]
                inRemote_k <- inRemote_sample[[k]]
                # If the relevant columns in the two data.frames are all numbers, convert to numeric
                if (!inherits(inDB_k, "numeric")) {
                  if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", inDB_k)) {
                    inDB_k <- as.numeric(inDB_k)
                  }
                }
                if (!inherits(inRemote_k, "numeric")) {
                  if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", inRemote_k)) {
                    inRemote_k <- as.numeric(inRemote_k)
                  }
                }
                if (!identical(inDB_k, inRemote_k)) { # If TRUE, update the DB
                  # message("discrepancy found in ", k)
                  to_insert <- if (!is.na(inRemote_k)) inRemote_k else NULL
                  if (is.null(to_insert)) {
                    DBI::dbExecute(con, paste0("UPDATE samples SET ", k, " = NULL WHERE sample_id = ", inDB_sample$sample_id, ";"))
                  } else {
                    DBI::dbExecute(con, paste0("UPDATE samples SET ", k, " = '", to_insert, "' WHERE sample_id = ", inDB_sample$sample_id, ";"))
                  }
                }
              }
            }
            
            # Get the results for the sample
            inDB_results <- DBI::dbGetQuery(con, paste0("SELECT * FROM results WHERE sample_id = ", inDB_sample$sample_id, ";"))
            
            inDB_results$checked <- FALSE # This will be used to track which rows have been checked
            
            for (k in 1:nrow(inRemote_results)) {
              sub <- inRemote_results[k, ]
              names_inRemote_sub <- names(sub)
              # Sort out if there's an equivalent row in inDB_result. There could be new results! Results are unique on result_type, parameter_id, sample_fraction, result_value_type, result_speciation, protocol_method, laboratory, analysis_datetime, but not all columns might be populated in 'sub'
              
              inDB_sub <- inDB_results[
                inDB_results$result_type == sub$result_type & 
                  inDB_results$parameter_id == sub$parameter_id & 
                  inDB_results$result_value_type == if (!is.null(sub$result_value_type)) sub$result_value_type else NA & 
                  inDB_results$result_speciation == if (!is.null(sub$result_speciation)) sub$result_speciation else NA & 
                  inDB_results$protocol_method == if (!is.null(sub$protocol_method)) sub$protocol_method else NA & 
                  inDB_results$laboratory == if (!is.null(sub$laboratory)) sub$laboratory else NA & 
                  inDB_results$analysis_datetime == if (!is.null(sub$analysis_datetime)) sub$analysis_datetime else NA &
                  inDB_results$sample_fraction == if (!is.null(sub$sample_fraction)) sub$sample_fraction else NA
                , ]
              
              if (nrow(inDB_sub) == 0) { # looks like a new result, add it (actually it might match an existing one but there's no way to know because some of the unique key columns were changed. If that's the case the 'old' one will be removed later)
                ## Checks on results ###########
                # Check that the results have the mandatory columns
                mandatory_res <- c("result", "result_type", "parameter_id")
                if (!all(c(mandatory_res) %in% names_inRemote_sub)) {
                  # Make an error message stating which column is missing
                  missing <- c(mandatory_res)[!c(mandatory_res) %in% names_inRemote_sub]
                  stop("For sample_series_id ", sid, " the source function did not return one or more mandatory column(s) for the sample metadata: '", paste(missing, collapse = "', '"), "'.")
                }
                
                # More complex checks if 'result' is NA
                # if there are NAs in the 'result' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
                if (is.na(sub$result)) {
                  if (!("result_condition" %in% names_inRemote_sub)) {
                    warning("For sample_series_id ", sid, " the source function returned at least one NA result in the column 'result' but did not return a corresponding entry in the column 'result_condition'.")
                  } else { # check that 'result_condition' is not NA.
                    if (is.na(sub$result_condition)) {
                      warning("For sample_series_id ", sid, " the source function returned a value of NA in the column 'result' but did not return a corresponding entry in the column 'result_condition'.")
                    } else { # check that 'result_condition' is not NA.
                      
                      if (sub$result_condition %in% c(1, 2)) {
                        if (!("result_condition_value" %in% names_inRemote_sub)) {
                          warning("For sample_series_id ", sid, " the source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value.")
                        }
                      }
                    }
                    
                    if (sub$result_condition %in% c(1, 2)) {
                      if (is.na(sub$result_condition_value)) {
                        stop("For sample_series_id ", sid, " the source function returned a value of 1 or 2 in the column 'result_condition' (indicating above or below detection limit) but did not return a corresponding entry in the column 'result_condition_value'.")
                      }
                    }
                  }
                } # End of additional checks if any NA values in 'result' column are returned
                
                # Get the result_speciation and sample_fraction boolean values for the parameters. If at least one TRUE then data must contain columns result_speciation and sample_fraction.
                result_speciation <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, result_speciation AS result_speciation_bool FROM parameters WHERE parameter_id IN (", paste(unique(sub$parameter_id), collapse = ", "), ");"))
                sample_fraction <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM parameters WHERE parameter_id IN (", paste(unique(sub$parameter_id), collapse = ", "), ");"))
                if (any(result_speciation$result_speciation_bool)) {
                  if (!("result_speciation" %in% names_inRemote_sub)) {
                    warning("For sample_series_id ", sid, " the source function did not return a column 'result_speciation' but the database mandates this for at least one of the parameters.")
                  } else { # Check that values in the result_speciation column are not NA where necessary
                    merge <- merge(sub, result_speciation, by = "parameter_id")
                    # For rows where result_speciation_bool is TRUE, check that the corresponding result_speciation column is not NA
                    chk <- with(merge, result_speciation_bool & is.na(result_speciation))
                    if (any(chk)) {
                      warning("For sample_series_id ", sid, " the source function returned NA values in the column 'result_speciation' for at least one parameter where the database mandates this value.")
                    }
                  }
                }
                if (any(sample_fraction$sample_fraction_bool)) {
                  if (!("sample_fraction" %in% names_inRemote_sub)) {
                    stop("The source function did not return a column 'sample_fraction' but the database mandates this for at least one of the parameters.")
                  } else { # Check that all values in the sample_fraction column are not NA where necessary
                    merge <- merge(names_inRemote_sub, sample_fraction, by = "parameter_id")
                    # For rows where sample_fraction_bool is TRUE, check that the corresponding sample_fraction column is not NA
                    chk <- with(merge, sample_fraction_bool & is.na(sample_fraction))
                    if (any(chk)) {
                      stop("For sample_series_id ", sid, " the source function returned NA values in the column 'sample_fraction' for at least one parameter where the database mandates this value.")
                    }
                  }
                }
                
                # Append new values
                sub$sample_id <- inDB_sample$sample_id
                DBI::dbAppendTable(con, "results", sub)
                
              } else if (nrow(inDB_sub) == 1) { # matching result found, check and adjust if necessary
                # Check for differences in the results
                for (l in names_inRemote_sub) {
                  if (l %in% valid_result_names) {
                    inDB_l <- inDB_sub[[l]]
                    sub_l <- sub[[l]]
                    # If the relevant columns in the two data.frames are all numbers, convert to numeric
                    if (!inherits(inDB_l, "numeric")) {
                      if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", inDB_l)) {
                        inDB_l <- as.numeric(inDB_l)
                      }
                    }
                    if (!inherits(sub_l, "numeric")) {
                      if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", sub_l)) {
                        sub_l <- as.numeric(sub_l)
                      }
                    }
                    if (!identical(inDB_l, sub_l)) {
                      to_insert <- if (!is.na(sub_l)) sub_l else "NULL"
                      # message("Found discrepancy in ", l, " for sample_series_id ", sid, ". Updating the database.")
                      DBI::dbExecute(con, paste0("UPDATE results SET ", l, " = '", to_insert, "' WHERE result_id = ", inDB_sub$result_id, ";"))
                    }
                  }
                }
                inDB_results[inDB_results$result_id == inDB_sub$result_id, "checked"] <- TRUE # result entry will not be deleted
                
              } else {
                warning("For sample_series_id ", sid, " the source function returned a result that matched more than one result in the database. This should not happen.")
                inDB_results[inDB_results$result_id == inDB_sub$result_id, "checked"] <- TRUE # result entry will not be deleted
              }
              
            }
            # Remove any results that were not checked
            to_delete <- inDB_results[!inDB_results$checked, "result_id"]
            if (length(to_delete) > 0) {
              DBI::dbExecute(con, paste0("DELETE FROM results WHERE result_id IN (", paste(to_delete, collapse = ", "), ");"))
            }
            
          } else { # No database sample was found, add the sample and corresponding results (follow same process as getNewDiscrete)
            # Add a new sample and results to the DB  ########
            ## Checks on sample metadata ###########
            # Functions may pass the location code instead of location_id, change it
            if ("location" %in% names_inRemote_samp) {
              inRemote_sample$location_id <- loc_id
              inRemote_sample$location <- NULL
              names_inRemote_samp <- names(inRemote_sample)
            }
            if ("sub_location" %in% names_inRemote_samp) {
              inRemote_sample$sub_location_id <- sub_loc_id
              inRemote_sample$sub_location <- NULL
              names_inRemote_samp <- names(inRemote_sample)
            }
            
            # Check that the sample data has the required columns at minimum: c("location_id", "media_id", "datetime", "collection_method", "sample_type", "owner", "import_source_id"). Note that import_source_id is only mandatory because this function pulls data in from a remote source
            mandatory_samp <- c("location_id", "media_id", "datetime", "collection_method", "sample_type", "owner", "import_source_id")
            if (!all(c(mandatory_samp) %in% names_inRemote_samp)) {
              # Make an error message stating which column is missing
              missing <- c(mandatory_samp)[!c(mandatory_samp) %in% names_inRemote_samp]
              stop("For sample_series_id ", sid, " the source function did not return one or more mandatory column(s) for the sample metadata to enable the addition of new samples found in the remote: '", paste(missing, collapse = "', '"), "'.")
            }
            
            inRemote_sample$import_source <- source_fx
            
            
            ## Checks on results ###########
            # Check that the results have the mandatory columns
            mandatory_res <- c("result", "result_type", "parameter_id")
            if (!all(c(mandatory_res) %in% names_inRemote_res)) {
              # Make an error message stating which column is missing
              missing <- c(mandatory_res)[!c(mandatory_res) %in% names_inRemote_res]
              stop("For sample_series_id ", sid, " the source function did not return one or more mandatory column(s) for the sample metadata: '", paste(missing, collapse = "', '"), "'.")
            }
            
            # More complex checks if 'result' is NA
            # if there are NAs in the 'result' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
            if (any(is.na(inRemote_results$result))) {
              if (!("result_condition" %in% names_inRemote_res)) {
                stop("For sample_series_id ", sid, " the source function returned NA values in the column 'result' but did not return a column called 'result_condition'.")
              } else { # Check that each NA in 'result' has a corresponding entry in 'result_condition'
                sub.results <- inRemote_results[is.na(inRemote_results$result), ]
                check_result_condition <- FALSE # prevents repeatedly checking for the same thing
                
                for (l in 1:nrow(sub.results)) {
                  if (is.na(sub.results$result[l]) & is.na(sub.results$result_condition[l])) {
                    stop("For sample_series_id ", sid, " the source function returned at least one NA result in the column 'result' but did not return a corresponding entry in the column 'result_condition'.")
                  } else {
                    if (!check_result_condition) {
                      if (any(sub.results$result_condition %in% c(1, 2))) {
                        if (!("result_condition_value" %in% names(inRemote_results))) {
                          stop("For sample_series_id ", sid, " the source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value.")
                        }
                      }
                      check_result_condition <- TRUE
                    }
                    
                    if (sub.results$result_condition[l] %in% c(1, 2)) {
                      if (is.na(sub.results$result_condition_value[l])) {
                        stop("For sample_series_id ", sid, " the source function returned a value of 1 or 2 in the column 'result_condition' (indicating above or below detection limit) but did not return a corresponding entry in the column 'result_condition_value'.")
                      }
                    }
                  }
                } # End of looping over each row with NA in result column
              }
            } # End of additional checks fs any NA values in 'result' column are returned
            
            # Get the result_speciation and sample_fraction boolean values for the parameters. If at least one TRUE then data must contain columns result_speciation and sample_fraction.
            result_speciation <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, result_speciation AS result_speciation_bool FROM parameters WHERE parameter_id IN (", paste(unique(inRemote_results$parameter_id), collapse = ", "), ");"))
            sample_fraction <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM parameters WHERE parameter_id IN (", paste(unique(inRemote_results$parameter_id), collapse = ", "), ");"))
            if (any(result_speciation$result_speciation_bool)) {
              if (!("result_speciation" %in% names(inRemote_results))) {
                stop("For sample_series_id ", sid, " the source function did not return a column 'result_speciation' but the database mandates this for at least one of the parameters.")
              } else { # Check that values in the result_speciation column are not NA where necessary
                merge <- merge(inRemote_results, result_speciation, by = "parameter_id")
                # For rows where result_speciation_bool is TRUE, check that the corresponding result_speciation column is not NA
                chk <- with(merge, result_speciation_bool & is.na(result_speciation))
                if (any(chk)) {
                  stop("For sample_series_id ", sid, " the source function returned NA values in the column 'result_speciation' for at least one parameter where the database mandates this value.")
                }
              }
            }
            if (any(sample_fraction$sample_fraction_bool)) {
              if (!("sample_fraction" %in% names(inRemote_results))) {
                stop("The source function did not return a column 'sample_fraction' but the database mandates this for at least one of the parameters.")
              } else { # Check that all values in the sample_fraction column are not NA where necessary
                merge <- merge(inRemote_results, sample_fraction, by = "parameter_id")
                # For rows where sample_fraction_bool is TRUE, check that the corresponding sample_fraction column is not NA
                chk <- with(merge, sample_fraction_bool & is.na(sample_fraction))
                if (any(chk)) {
                  stop("For sample_series_id ", sid, " the source function returned NA values in the column 'sample_fraction' for at least one parameter where the database mandates this value.")
                }
              }
            }
            
            # Append values in a transaction block ##########
            if (!attr(con, "active_transaction")) {
              DBI::dbBegin(con)
              attr(con, "active_transaction") <- TRUE
              tryCatch({
                commit_fx(con, inRemote_sample, inRemote_results)
                DBI::dbCommit(con)
                attr(con, "active_transaction") <- FALSE
              }, error = function(e) {
                DBI::dbRollback(con)
                attr(con, "active_transaction") <<- FALSE
                warning("getNewDiscrete: Failed to commit new data for sample_series_id ", sid, " and list element ", j, " . Error message: ", e$message)
              })
            } else { # we're already in a transaction
              commit_fx(con, inRemote_sample, inRemote_results)
            }
          } # End of if no sample is found (making a new one)
        } # End of loop over inRemote
        
        
      } else { # There was no data in remote for the date range specified
        DBI::dbExecute(con, paste0("UPDATE sample_series SET last_synchronize = '", .POSIXct(Sys.time(), "UTC"), "' WHERE sample_series_id = ", sid, ";"))
      }
    }, error = function(e) {
      warning("synchronize failed on sample_series_id ", sid, "  with message: ", e$message)
    }
    ) # End of tryCatch
    
    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
    
  } # End of for loop
  
  if (interactive()) {
    close(pb)
  }
  
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_synchronize_discrete';"))
  message("Found ", updated, " timeseries to refresh out of the ", nrow(all_series), " unique numbers provided.")
  diff <- Sys.time() - start
  message("Total elapsed time for synchronize: ", round(diff[[1]], 2), " ", units(diff), ". End of function.")
  
} #End of function
