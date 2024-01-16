#' Synchronize hydro DB with remote sources
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The synchronize function pulls and replaces data of category 'continuous' and 'discrete' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence. For continuous data daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables.
#'
#' NOTE that any data point labelled as imputed = TRUE is only replaced if a value is found in the remote.
#'
#'Any timeseries labelled as 'getRealtimeAquarius' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [getRealtimeAquarius()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct) from which to look for possible new data. You can specify a single start_datetime to apply to all `timeseries_id`, or one per element of `timeseries_id.`
#' @param discrete Should discrete data also be synchronized?
#'
#' @return Updated entries in the hydro database.
#' @export
#'

#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

synchronize <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all", start_datetime, discrete = FALSE)
{

  on.exit(DBI::dbDisconnect(con))
  start <- Sys.time()

  message("Synchronizing timeseries with synchronize...")

  if (!inherits(start_datetime, "POSIXct")){
    stop("Parameter start_datetime must be supplied as a POSIXct object.")
  }

  #Check length of start_datetime is either 1 of same as timeseries_id
  if (length(start_datetime) != 1){
    if (length(start_datetime) != nrow(timeseries_id)){
      stop("There is not exactly one element to start_datetime per valid timeseries_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for timeseries_id that doesn't exist.")
    }
  }

  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")

  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, last_daily_calculation, category, period_type FROM timeseries WHERE source_fx IS NOT NULL")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, end_datetime, last_daily_calculation, category, period_type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
    if (length(timeseries_id) != nrow(all_timeseries)){
      fail <- timeseries_id[!(timeseries_id %in% all_timeseries$timeseries_id)]
      ifelse ((length(fail) == 1),
              warning("Could not find one of the timeseries_ids that you specified: ID ", fail, " is missing from the database."),
              warning("Could not find some of the timeseries_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
  }

  updated <- 0 #Counter for number of updated timeseries
  EQcon <- "unset"
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    category <- all_timeseries$category[i]
    if (category == "discrete" & !discrete){
      next()
    }
    period_type <- all_timeseries$period_type[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    if (source_fx == "getNewEQWin" & EQcon == "unset"){
      EQcon <- EQConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(EQcon), add = TRUE)
    }
    source_fx_args <- all_timeseries$source_fx_args[i]
    param_code <- settings[settings$parameter == parameter & settings$source_fx == source_fx , "remote_param_name"]
    start_dt <- if (length(start_datetime) > 1) start_datetime[i] else start_datetime

    tryCatch({
      args_list <- list(location = loc, param_code = param_code, start_datetime = start_dt)
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
      if (source_fx == "getNewEQWin"){
        args_list[["EQcon"]] <- EQcon
      }
      inRemote <- do.call(source_fx, args_list) #Get the data using the args_list
      inRemote <- inRemote[!is.na(inRemote$value) , ]

      if (nrow(inRemote) > 0){
        if (category == "continuous"){
          inDB <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, grade, approval, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime),"';"))
          #Check if any imputed data points are present in the new data; replace the imputed value if TRUE and a non-imputed value now exists
          imputed <- inDB[inDB$imputed == TRUE , ]
          imputed.remains <- data.frame()
          if (nrow(imputed) > 0){
            for (i in 1:nrow(imputed)){
              if (!(imputed[i, "datetime"] %in% inRemote)){
                imputed.remains <- rbind(imputed.remains, imputed[i , ])
              }
            }
          }
        } else if (category == "discrete"){
          inDB <- DBI::dbGetQuery(con, paste0("SELECT target_datetime, datetime, value, sample_class FROM measurements_discrete WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime),"';"))
        }

        if (min(inRemote$datetime) > min(inDB$datetime)){ #if TRUE means that the DB has older data than the remote, which happens notably for the WSC. This older data can't be compared and is thus discarded.
          inDB <- inDB[inDB$datetime >= min(inRemote$datetime) , ]
        }

        #order both timeseries to compare them
        inDB <- inDB[order(inDB$datetime) , ]
        inRemote <- inRemote[order(inRemote$datetime) , ]

        # Create a unique datetime key for both data frames
        if (category == "continuous"){
          inRemote$key <- paste(inRemote$datetime, inRemote$value, inRemote$grade, inRemote$approval, sep = "|")
          inDB$key <- paste(inDB$datetime, inDB$value, inDB$grade, inDB$approval, sep = "|")
        } else if (category == "discrete"){
          inRemote$key <- paste(inRemote$target_datetime, inRemote$datetime, inRemote$value, inRemote$sample_class, sep = "|")
          inDB$key <- paste(inDB$target_datetime, inDB$datetime, inDB$value, inDB$sample_class, sep = "|")
        }

        # Check for mismatches using set operations
        mismatch_keys <- setdiff(inRemote$key, inDB$key)

        # Check where the discrepancy is
        if (length(mismatch_keys) > 0) {
          mismatch <- TRUE
          datetime <- inRemote[inRemote$key %in% mismatch_keys, "datetime"]
          datetime <- min(datetime)
        } else {
          mismatch <- FALSE
        }
        inRemote$key <- NULL

        if (mismatch){
          inRemote <- inRemote[inRemote$datetime >= datetime , ]
          if (category == "continuous"){
            #assign a period to the data
            if (period_type == "instantaneous"){ #Period is always 0 for instantaneous data
              inRemote$period <- "00:00:00"
            } else if ((period_type != "instantaneous") & !("period" %in% names(inRemote))) { #period_types of mean, median, min, max should all have a period
              inRemote <- calculate_period(data = inRemote, timeseries_id = tsid, con = con)
            } else { #Check to make sure that the supplied period can actually be coerced to a period
              check <- lubridate::period(unique(inRemote$period))
              if (NA %in% check){
                inRemote$period <- NA
              }
            }
            inRemote$imputed <- FALSE
          }
          inRemote$timeseries_id <- tsid

          DBI::dbWithTransaction(
            con,
            {
              updated <- updated + 1
              if (category == "continuous"){
                if (nrow(imputed.remains) > 0){
                  DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime), "' AND datetime NOT IN ('", paste(imputed.remains$datetime, collapse = "', '"), "');"))
                } else {
                  DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime), "';"))
                }
                DBI::dbAppendTable(con, "measurements_continuous", inRemote)
              } else if (category == "discrete"){
                DBI::dbExecute(con, paste0("DELETE FROM measurements_discrete WHERE timeseries_id = ", tsid, " AND datetime >= '", min(inRemote$datetime), "';"))
                DBI::dbAppendTable(con, "measurements_discrete", inRemote)
              }
              #make the new entry into table timeseries
              end <- max(max(inDB$datetime), max(inRemote$datetime))
              DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", end, "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "', last_synchronize = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            }
          )
          if (category == "continuous"){
            #Recalculate daily means and statistics
            calculate_stats(timeseries_id = tsid,
                            con = con,
                            start_recalc = as.Date(substr(datetime, 1, 10)))
          }
        }
      }
    }, error = function(e) {
      warning("synchronize failed on location ", loc, " and parameter ", parameter, " (timeseries_id ", tsid, ").")
    }
    )
  }

  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_sync';"))
  message("Found ", updated, " timeseries to refresh out of the ", nrow(all_timeseries), " provided.")
  diff <- Sys.time() - start
  message("Total elapsed time for synchronize: ", round(diff[[1]], 2), " ", units(diff), ". End of function.")

} #End of function
