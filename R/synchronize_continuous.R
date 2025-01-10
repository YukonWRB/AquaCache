#' Synchronize hydro DB with remote sources
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This synchronize function pulls and replaces data references in table 'timeseries' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence. Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables (Water Survey of Canada).
#'
#' NOTE that any data point labelled as imputed = TRUE is only replaced if a value is found in the remote, and any data point labelled as no_update = TRUE is not replaced by the remote data.
#'
#'Any timeseries labelled as 'downloadAquarius' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [downloadAquarius()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `timeseries_id`, or one per element of `timeseries_id.`
#' @param active Sets behavior for checking timeseries or not. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all timeseries
#'
#' @return Updated entries in the hydro database.
#' @export
#'
#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

synchronize_continuous <- function(con = NULL, timeseries_id = "all", start_datetime, active = 'default')
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
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter_id, timeseries_id, source_fx, source_fx_args, last_daily_calculation, category, period_type, record_rate, owner, active FROM timeseries WHERE source_fx IS NOT NULL")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter_id, timeseries_id, source_fx, source_fx_args, last_daily_calculation, category, period_type, record_rate, owner, active FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND source_fx IS NOT NULL;"))
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

  updated <- 0 #Counter for number of updated timeseries
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_timeseries), style = 3)
  }
  for (i in 1:nrow(all_timeseries)) {
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter_id[i]
    period_type <- all_timeseries$period_type[i]
    record_rate <- all_timeseries$record_rate[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    owner <- all_timeseries$owner[i]
    
    source_fx_args <- all_timeseries$source_fx_args[i]
    if (is.na(record_rate)) {
      parameter_id <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM fetch_settings WHERE parameter_id = ", parameter, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate IS NULL;"))[1,1]
    } else {
      parameter_id <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM fetch_settings WHERE parameter_id = ", parameter, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate = '", record_rate, "';"))[1,1]
    }
    start_dt <- if (length(start_datetime) > 1) start_datetime[i] else start_datetime

    tryCatch({
      args_list <- list(location = loc, parameter_id = parameter_id, start_datetime = start_dt, con = con)
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
          
          # Adjust parameters only used for continuous timeseries
          if (!("approval" %in% names(inRemote))) {
            inRemote$approval <- approval_unknown
          }
          if (!("grade" %in% names(inRemote))) {
            inRemote$grade <- grade_unknown
          }
          if (!("qualifier" %in% names(inRemote))) {
            inRemote$qualifier <- qualifier_unknown
          }
        
        # Check changes in attributes
        if (!is.null(owner)) {  # There may not be an owner assigned in table timeseries
          if (!("owner" %in% names(inRemote))) {
            inRemote$owner <- owner
          }
        }

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
            mismatch_keys <- inRemote$key[!(inRemote$key %in% inDB$key)]
            # This DOES NOT catch if there are points inDB at dates in the future from what is inRemote, unless a mismatch happens before. Check for that later.
            
            # Check where the discrepancy is in both data frames
            if (length(mismatch_keys) > 0) {
              mismatch <- TRUE
              # Find the most recent datetime in the remote data that is not in the DB. This will be the datetime to start replacing from in the local.
              datetime <- min(inRemote[inRemote$key %in% mismatch_keys, "datetime"])
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

        if (mismatch) {
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
                DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime), "' AND datetime NOT IN ('", paste(no_update$datetime, collapse = "', '"), "');"))
                DBI::dbExecute(con, paste0("DELETE FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", min(inRemote$datetime), "';"))
              } else {
                DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime), "';"))
                DBI::dbExecute(con, paste0("DELETE FROM measurements_calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", min(inRemote$datetime), "';"))
              }
              
              DBI::dbAppendTable(con, "measurements_continuous", inRemote[, c("datetime", "value", "period", "timeseries_id", "imputed")])
              
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
              updated <- updated + 1
            }, error = function(e) {
              DBI::dbRollback(con)
              attr(con, "active_transaction") <<- FALSE
              warning("synchronize failed to make database changes for ", loc, " and parameter code ", parameter, " (timeseries_id ", tsid, ") with message: ", e$message, ".")
            })
          } else { # we're already in a transaction
            commit_fx(con, imputed.remains, tsid, inRemote, inDB)
            updated <- updated + 1
          }
          
            #Recalculate daily means and statistics
            calculate_stats(timeseries_id = tsid,
                            con = con,
                            start_recalc = as.Date(substr(datetime, 1, 10)))
        } else { # mismatch is FALSE: there was data in the remote but no mismatch
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
    
    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
    
  } # End of for loop
  
  if (interactive()) {
    close(pb)
  }

  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_sync_continuous';"))
  message("Found ", updated, " timeseries to refresh out of the ", nrow(all_timeseries), " unique numbers provided.")
  diff <- Sys.time() - start
  message("Total elapsed time for synchronize: ", round(diff[[1]], 2), " ", units(diff), ". End of function.")

} #End of function
