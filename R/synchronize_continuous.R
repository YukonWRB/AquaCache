#' Synchronize hydro DB with remote sources
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This synchronize function pulls and replaces data referenced in table 'timeseries' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence. New data is also brought in, if any exists on the remote. Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables (Water Survey of Canada).
#' 
#' If you leave the 'con' object as NULL the function will try to run in parallel, which dramatically speeds things up. You can pass connection parameters to the function if you want to run it in parallel, but you must leave 'con' as NULL. If you don't want to run in parallel, you can pass a connection object to the function and it will run in sequence.
#' 
#' In addition, grades, qualifiers, and approvals are always updated as it's computationally cheaper to do so than to check if they need updating.
#'
#' NOTE that any data point labelled as imputed = TRUE is only replaced if a value is found in the remote exactly matching the datetime of the imputed entry, and any data point labelled as no_update = TRUE is not replaced by the remote dat (imputed or not).
#'
#'Any timeseries labelled as 'downloadAquarius' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [downloadAquarius()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after. If you wish to run this function in parallel you MUST leave this argument NULL. If you also specify connection parameters in later arguments they will be used, otherwise the function will use the AquaConnect defaults.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `timeseries_id`, or one per element of `timeseries_id.`
#' @param active Sets behavior for checking timeseries or not. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all timeseries
#' @param dbName The name of the database to connect to. If left NULL, the function will use the default database name from the .Renviron file as per [AquaConnect()].
#' @param dbHost The host address of the database. If left NULL, the function will use the default host address from the .Renviron file as per [AquaConnect()].
#' @param dbPort The port of the database. If left NULL, the function will use the default port from the .Renviron file as per [AquaConnect()].
#' @param dbUser The username for the database. If left NULL, the function will use the default username from the .Renviron file as per [AquaConnect()].
#' @param dbPass The password for the database. If left NULL, the function will use the default password from the .Renviron file as per [AquaConnect()].
#'
#' @return Updated entries in the hydro database.
#' @export
#'
#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

synchronize_continuous <- function(con = NULL, timeseries_id = "all", start_datetime, active = 'default', dbName = NULL, dbHost = NULL, dbPort = NULL, dbUser = NULL, dbPass = NULL) {
  
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
    if (any(is.null(c(dbName, dbHost, dbPort, dbUser, dbPass)))) {
      con <- AquaConnect(silent = TRUE)
      con_params <- FALSE
      parallel <- TRUE  # TRUE because the connection parameters can be passed on to parallel instances
    } else {
      con <- AquaConnect(name = dbName, host = dbHost, port = dbPort, username = dbUser, password = dbPass, silent = TRUE)
      con_params <- TRUE
      parallel <- TRUE  # TRUE because the connection parameters can be passed on to parallel instances
    }
    on.exit(DBI::dbDisconnect(con))
    rlang::check_installed("foreach", reason = "to run this function in parallel")
    rlang::check_installed("doSNOW", reason = "to run this function in parallel")
    
    `%dopar%` <- foreach::`%dopar%`
  } else {
    parallel <- FALSE # FALSE because the connection parameters are already set and can't be passed on to parallel instances
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  start <- Sys.time()
  
  message("Synchronizing timeseries with synchronize_continuous...")
  
  #Check length of start_datetime is either 1 of same as timeseries_id
  if (length(start_datetime) != 1) {
    if (length(start_datetime) != length(timeseries_id)) {
      stop("There is not exactly one element to start_datetime per valid timeseries_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for timeseries_id that doesn't exist.")
    }
  } else {
    timeseries_id <- unique(timeseries_id)
  }
  
  
  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter_id, timeseries_id, source_fx, source_fx_args, last_daily_calculation, period_type, record_rate, default_owner, active FROM timeseries WHERE source_fx IS NOT NULL")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter_id, timeseries_id, source_fx, source_fx_args, last_daily_calculation, period_type, record_rate, default_owner, active FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND source_fx IS NOT NULL;"))
    if (length(unique(timeseries_id)) != nrow(all_timeseries)) {
      fail <- timeseries_id[!timeseries_id %in% all_timeseries$timeseries_id]
      ifelse((length(fail) == 1),
             warning("Could not find one of the timeseries_ids that you specified: ID ", fail, " is missing from the database."),
             warning("Could not find some of the timeseries_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
  }
  
  if (active == 'default') {
    all_timeseries <- all_timeseries[all_timeseries$active, ]
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
  
  # Define a worker function that either gets passed to parallel or sequential for loops over rows in all_timeseries
  worker <- function(i, all_timeseries, approval_unknown, grade_unknown, qualifier_unknown, start_datetime, parallel, con) {
    
    success <- FALSE # So that reporting of successful updates can work
    
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter_id[i]
    period_type <- all_timeseries$period_type[i]
    record_rate <- all_timeseries$record_rate[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    owner <- all_timeseries$default_owner[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    
    start_dt <- if (length(start_datetime) > 1) start_datetime[i] else start_datetime
    
    tryCatch({
      if (!is.na(source_fx_args)) { #add some arguments if they are specified
        args <- jsonlite::fromJSON(source_fx_args)
        args_list <- c(args_list, lapply(args, as.character))
      }
      
      inRemote <- do.call(source_fx, args_list) #Get the data using the args_list
      
      inRemote <- inRemote[!is.na(inRemote$value) , ]
      
      if (nrow(inRemote) > 0) {
        inDB <- DBI::dbGetQuery(con, paste0("SELECT no_update, datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime),"';"))
        # Set aside any rows where no_update == TRUE
        no_update <- inDB[inDB$no_update, ]
        inDB <- inDB[!inDB$no_update, ]
        # Drop no_update columns
        inDB$no_update <- NULL
        no_update$no_update <- NULL
        #Check if any imputed data points are present in the new data; replace the imputed value if TRUE and a non-imputed value now exists
        imputed <- inDB[inDB$imputed, ]
        imputed.remains <- data.frame()
        if (nrow(imputed) > 0) {
          for (i in 1:nrow(imputed)) {
            if (!(imputed[i, "datetime"] %in% inRemote$datetime)) {
              imputed.remains <- rbind(imputed.remains, imputed[i , ])
            }
          }
        }
        no_update <- rbind(no_update, imputed.remains)
        
        # Adjust parameters
        if (!("approval" %in% names(inRemote))) {
          inRemote$approval <- approval_unknown
        }
        if (!("grade" %in% names(inRemote))) {
          inRemote$grade <- grade_unknown
        }
        if (!("qualifier" %in% names(inRemote))) {
          inRemote$qualifier <- qualifier_unknown
        }
        if (!is.null(owner)) {  # There may not be an owner assigned in table timeseries
          if (!("owner" %in% names(inRemote))) {
            inRemote$owner <- owner
          }
        }
        
        # Check changes in attributes
        if ("owner" %in% names(inRemote)) {
          adjust_owner(con, tsid, inRemote[, c("datetime", "owner")])
        }
        if ("contributor" %in% names(inRemote)) {
          adjust_contributor(con, tsid, inRemote[, c("datetime", "contributor")])
        }
        if ("grade" %in% names(inRemote)) {
          adjust_grade(con, tsid, inRemote[, c("datetime", "grade")])
        }
        if ("approval" %in% names(inRemote)) {
          adjust_approval(con, tsid, inRemote[, c("datetime", "approval")])
        }
        if ("qualifier" %in% names(inRemote)) {
          adjust_qualifier(con, tsid, inRemote[, c("datetime", "qualifier")])
        }
        
        # Drop columns owner, contributor, grade, approval, qualifier as these are already taken care of
        inRemote$owner <- NULL
        inRemote$contributor <- NULL
        inRemote$grade <- NULL
        inRemote$approval <- NULL
        inRemote$qualifier <- NULL
        
        if (nrow(inDB) > 0) { # If nothing inDB it's an automatic mismatch so this is skipped
          if (min(inRemote$datetime) > min(inDB$datetime)) { #if TRUE means that the DB has older data than the remote, which happens notably for the WSC. This older data can't be compared and is thus discarded.
            inDB <- inDB[inDB$datetime >= min(inRemote$datetime) , ]
          }
          
          if (min(inRemote$datetime) < min(inDB$datetime)) { #if TRUE means that the remote has older data than the DB, so immediately declare mismatch = TRUE.
            mismatch <- TRUE
            datetime <- min(inRemote$datetime)
          } else {
            #order both timeseries to compare them
            inDB <- inDB[order(inDB$datetime) , ]
            inRemote <- inRemote[order(inRemote$datetime) , ]
            
            # Create a unique datetime key for both data frames
            # Check if there is remote data that completely overlaps with rows in no_update. If so, remove those rows from inRemote.
            if (nrow(no_update) > 0) {
              inRemote <- inRemote[!(inRemote$datetime %in% no_update$datetime), ]
              inDB <- inDB[!(inDB$datetime %in% no_update$datetime), ]
            }
            
            # Make keys
            inRemote$key <- paste(substr(as.character(inRemote$datetime), 1, 22), inRemote$value, sep = "|")
            inDB$key <- paste(substr(as.character(inDB$datetime), 1, 22), inDB$value, sep = "|")
            
            # Check for mismatches using set operations. 
            mismatch_keys_remote <- inRemote$key[!(inRemote$key %in% inDB$key)]
            # Check the inverse as well, in case there are points in the DB that are not in the remote
            mismatch_keys_db <- inDB$key[!(inDB$key %in% inRemote$key)]
            
            # This DOES NOT catch if there are points inDB at dates in the future from what is inRemote, unless a mismatch happens before. Check for that later.
            
            # Check where the discrepancy is in both data frames
            if (length(mismatch_keys_remote) > 0 | length(mismatch_keys_db) > 0) {
              mismatch <- TRUE
              # Find the most recent datetime in the remote data that is not in the DB. This will be the datetime to start replacing from in the local.
              datetime <- min(inRemote[inRemote$key %in% mismatch_keys_remote, "datetime"], inDB[inDB$key %in% mismatch_keys_db, "datetime"])
            } else {
              if (max(inDB$datetime) > max(inRemote$datetime)) { # Check if the remote misses data that should be deleted from the DB
                mismatch <- TRUE
                datetime <- max(inRemote$datetime)
              } else {
                mismatch <- FALSE
              }
            }
            inRemote$key <- NULL
          }
        } else { # There's no data in the DB but there is some in the remote. Automatic mismatch.
          mismatch <- TRUE
          datetime <- min(inRemote$datetime)
        }
        
        if (mismatch) { # mismatch is TRUE: there was a mismatch between the remote and the local data
          inRemote <- inRemote[inRemote$datetime >= datetime , ]
          #assign a period to the data
          if (period_type == "instantaneous") { #Period is always 0 for instantaneous data
            inRemote$period <- "00:00:00"
          } else if ((period_type != "instantaneous") & !("period" %in% names(inRemote))) { #period_types of mean, median, min, max should all have a period
            period <- calculate_period(data = inRemote[ , c("datetime")], timeseries_id = tsid, con = con)
            inRemote <- merge(inRemote, period, by = "datetime", all.x = TRUE)
          } else { #Check to make sure that the supplied period can actually be coerced to a period
            check <- lubridate::period(unique(inRemote$period))
            if (NA %in% check) {
              inRemote$period <- NA
            }
          }
          inRemote$imputed <- FALSE
          inRemote$timeseries_id <- tsid
          
          
          # Now commit the changes to the database
          commit_fx <- function(con, no_update, tsid, inRemote, inDB) {
            # Now delete entries in measurements_continuous and measurements_calculated_daily that are no longer in the remote data and/or that need to be replaced
            if (nrow(no_update) > 0) { # Don't delete imputed data points unless there's new data to replace it!
              DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", datetime, "' AND datetime NOT IN ('", paste(no_update$datetime, collapse = "', '"), "');"))
              DBI::dbExecute(con, paste0("DELETE FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", datetime, "';"))
            } else {
              DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", datetime, "';"))
              DBI::dbExecute(con, paste0("DELETE FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", datetime, "';"))
            }
            
            DBI::dbAppendTable(con, "measurements_continuous", inRemote[, c("datetime", "value", "period", "timeseries_id", "imputed")])
            
            #Recalculate daily means and statistics
            calculate_stats(timeseries_id = tsid,
                            con = con,
                            start_recalc = as.Date(substr(datetime, 1, 10)))
            
            # adjust entries in table 'timeseries' to reflect the new data
            end <- max(inRemote$datetime)
            DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", end, "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "', last_synchronize = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            earliest <- min(inRemote$datetime, 
                            DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, ";"))[[1]], 
                            DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, ";"))[[1]])
            DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", earliest, "' WHERE timeseries_id = ", tsid, ";"))
          }
          
          if (!attr(con, "active_transaction")) {
            DBI::dbBegin(con)
            attr(con, "active_transaction") <- TRUE
            tryCatch({
              commit_fx(con, no_update, tsid, inRemote, inDB)
              DBI::dbCommit(con)
              attr(con, "active_transaction") <- FALSE
              
              success <- TRUE
              
            }, error = function(e) {
              DBI::dbRollback(con)
              attr(con, "active_transaction") <<- FALSE
              warning("synchronize failed to make database changes for ", loc, " and parameter code ", parameter, " (timeseries_id ", tsid, ") with message: ", e$message, ".")
            })
          } else { # we're already in a transaction
            commit_fx(con, no_update, tsid, inRemote, inDB)
            
            success <- TRUE
            
          }
          
        } else { # mismatch is FALSE: there was data in the remote but no mismatch. Do basic checks and update the last_synchronize date.
          DBI::dbExecute(con, paste0("UPDATE timeseries SET last_synchronize = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
          # Check to make sure start_datetime in the timeseries table is accurate based on what's in the DB (this isn't regularly done otherwise and is quick to do). This doesn't deal with HYDAT historical means, but that's done by the HYDAT sync/update functions.
          start_dt <- DBI::dbGetQuery(con, paste0("SELECT start_datetime FROM timeseries WHERE timeseries_id = ", tsid, ";"))[[1]]
          
          # double check the earliest time in DB in case there's an error in the timeseries table
          
          earliest <- min(inRemote$datetime, 
                          DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, ";"))[[1]], 
                          DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, ";"))[[1]])
          
          
          DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", earliest, "' WHERE timeseries_id = ", tsid, ";"))
        }
      } else { # There was no data in remote for the date range specified
        DBI::dbExecute(con, paste0("UPDATE timeseries SET last_synchronize = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
      }
    }, error = function(e) {
      warning("synchronize failed on location ", loc, " and parameter code ", parameter, " (timeseries_id ", tsid, ") with message: ", e$message, ".")
    }
    ) # End of tryCatch
    
    return(success)
    
  } # End of worker function
  
  
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_timeseries), style = 3)
  }
  
  if (parallel) {
    n.cores <- parallel::detectCores() - 2
    cl <- parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    if (con_params) {
      parallel::clusterExport(cl, c("all_timeseries", "approval_unknown", "grade_unknown", "qualifier_unknown", "start_datetime", "dbName", "dbHost", "dbPort", "dbUser", "dbPass"), envir = environment())
    } else {
      parallel::clusterExport(cl, c("all_timeseries", "approval_unknown", "grade_unknown", "qualifier_unknown", "start_datetime"), envir = environment())
    }
    doSNOW::registerDoSNOW(cl)
    if (interactive()) {
      progress <- function(n) {
        utils::setTxtProgressBar(pb, n)
      }
      opts <- list(progress = progress)
      
      updated <- foreach::foreach(i = 1:nrow(all_timeseries), .packages = c("DBI", "lubridate", "AquaCache"), .options.snow = opts, .combine = sum) %dopar% {
        if (con_params) {
          parcon <- AquaCache::AquaConnect(name = dbName, host = dbHost, port = dbPort, username = dbUser, password = dbPass, silent = TRUE)
        } else {
          parcon <- AquaCache::AquaConnect(silent = TRUE)
        }
        success <- worker(i, all_timeseries, approval_unknown, grade_unknown, qualifier_unknown, start_datetime, parallel = TRUE, con = parcon)
        DBI::dbDisconnect(parcon)
        if (success) 1 else 0 # Will be summed up to get the total number of updated timeseries in object 'updated'
      }
    } else {
      updated <- foreach::foreach(i = 1:nrow(all_timeseries), .packages = c("DBI", "lubridate", "AquaCache"), .combine = sum) %dopar% {
        if (con_params) {
          parcon <- AquaCache::AquaConnect(name = dbName, host = dbHost, port = dbPort, username = dbUser, password = dbPass, silent = TRUE)
        } else {
          parcon <- AquaCache::AquaConnect(silent = TRUE)
        }
        success <- worker(i, all_timeseries, approval_unknown, grade_unknown, qualifier_unknown, start_datetime, parallel = TRUE, con = parcon)
        DBI::dbDisconnect(parcon)
        if (success) 1 else 0 # Will be summed up to get the total number of updated timeseries in object 'updated'
      }
    }
  } else { # Not parallel
    updated <- 0 # Counter for number of updated timeseries
    
    for (i in 1:nrow(all_timeseries)) {
      success <- worker(i, all_timeseries, approval_unknown, grade_unknown, qualifier_unknown, start_datetime, parallel = FALSE, con = con)
      if (success) updated <- updated + 1
      if (interactive()) {
        utils::setTxtProgressBar(pb, i)
      }
    } # End of for loop
  } # End of not parallel block
  
  if (interactive()) {
    close(pb)
  }
  
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_sync_continuous';"))
  message("Found ", updated, " timeseries to refresh or add to out of the ", nrow(all_timeseries), " unique timeseries provided.")
  diff <- Sys.time() - start
  message("Total elapsed time for synchronize continuous: ", round(diff[[1]], 2), " ", units(diff), ". End of function.")
  
} #End of function
