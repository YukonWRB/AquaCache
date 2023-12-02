#' Update of hysro DB
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The weekly update function pulls and replaces data of category 'continuous' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence. Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables.
#'
#' NOTE that any data point labelled as imputed = TRUE is ignored, as this implies it is missing from the remote and thus cannot be checked.
#'
#'Any timeseries labelled as 'getRealtimeAquarius' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [getRealtimeAquarius()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct) from which to look for possible new data. You can specify a single start_datetime to apply to all `timeseries_id`, or one per element of `timeseries_id.`
#'
#' @return Updated entries in the hydro database.
#' @export
#'

#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

synchronizeContinuous <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all", start_datetime)
{

  message("Synchronizing continuous category timeseries with synchronizeContinuous...")
  if (!inherits(start_datetime, "POSIXct")){
    stop("Parameter start_datetime must be supplied as a POSIXct object.")
  }
  start <- Sys.time()

  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")
  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, source_fx_args, period_type FROM timeseries WHERE category = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, source_fx_args, period_type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
    if (length(timeseries_id) != nrow(all_timeseries)){
      warning("At least one of the timeseries IDs you called for cannot be found in the database.")
    }
  }

  #Check length of start_datetime is either 1 of same as timeseries_id
  if (length(start_datetime) != 1){
    if (length(start_datetime) != nrow(all_timeseries)){
      stop("There is not exactly one element to start_datetime per valid timeseries_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for timeseries_id that doesn't exist.")
    }
  }

  updated <- 0 #Counter for number of updated timeseries
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    period_type <- all_timeseries$period_type[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
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
      ts <- do.call(source_fx, args_list) #Get the data using the args_list
      ts <- ts[!is.na(ts$value) , ]

      if (nrow(ts) > 0){
        realtime <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, grade, approval, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", min(ts$datetime),"';"))
        #Check if any imputed data points are present in the new data; replace the imputed value if TRUE and a non-imputed value now exists
        imputed <- realtime[realtime$imputed == TRUE , ]
        imputed.remains <- data.frame()
        if (nrow(imputed) > 0){
          for (i in 1:nrow(imputed)){
            if (!(imputed[i, "datetime"] %in% ts)){
              imputed.remains <- rbind(imputed.remains, imputed[i , ])
            }
          }
        }

        if (min(ts$datetime) > min(realtime$datetime)){ #if TRUE means that the DB has older data than the remote, which happens notably for the WSC. This older data can't be compared and is thus discarded.
          realtime <- realtime[realtime$datetime >= min(ts$datetime) , ]
        }

        #order both timeseries to compare them
        realtime <- realtime[order(realtime$datetime) , ]
        ts <- ts[order(ts$datetime) , ]

        # Create a unique datetime key for both data frames
        ts$key <- paste(ts$datetime, ts$value, ts$grade, ts$approval, sep = "|")
        realtime$key <- paste(realtime$datetime, realtime$value, realtime$grade, realtime$approval, sep = "|")

        # Check for mismatches using set operations
        mismatch_keys <- setdiff(ts$key, realtime$key)

        # Check where the discrepancy is
        if (length(mismatch_keys) > 0) {
          mismatch <- TRUE
          datetime <- ts[ts$key %in% mismatch_keys, "datetime"]
          datetime <- min(datetime)
        } else {
          mismatch <- FALSE
        }
        ts$key <- NULL

        if (mismatch){
          ts <- ts[ts$datetime >= datetime , ]
          #assign a period to the data
          if (period_type == "instantaneous"){ #Period is always 0 for instantaneous data
            ts$period <- "00:00:00"
          } else if ((period_type != "instantaneous") & !("period" %in% names(ts))) { #period_types of mean, median, min, max should all have a period
            period_res <- calculate_period(data = ts, timeseries_id = tsid, con = con)
            ts <- period_res$ts
          } else { #Check to make sure that the supplied period can actually be coerced to a period
            check <- lubridate::period(unique(ts$period))
            if (NA %in% check){
              ts$period <- NA
            }
          }
          ts$timeseries_id <- tsid
          ts$imputed <- FALSE
          DBI::dbWithTransaction(
            con,
            {
              updated <- updated + 1
              if (nrow(imputed.remains) > 0){
                DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", min(ts$datetime), "' AND '", max(ts$datetime), "' AND datetime NOT IN ('", paste(imputed.remains$datetime, collapse = "', '"), "');"))
              } else {
                DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", min(ts$datetime), "' AND '", max(ts$datetime), "';"))
              }

              DBI::dbAppendTable(con, "measurements_continuous", ts)
              #make the new entry into table timeseries
              end <- max(max(realtime$datetime), ts$datetime)
              DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", end, "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "', last_synchronize = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            }
          )

          #Recalculate daily means and statistics
          calculate_stats(timeseries_id = tsid,
                          con = con,
                          start_recalc = as.Date(substr(datetime, 1, 10)))
        }
      }
    }, error = function(e) {
      warning("synchronizeContinuous failed on location ", loc, " and parameter ", parameter, " (timeseries_id ", tsid, ").")
    }
    )
  }

  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_sync_continuous';"))
  message("Found ", updated, " timeseries to refresh out of the ", nrow(all_timeseries), " provided.")
  diff <- Sys.time() - start
  message("Total elapsed time for synchronizeContinuous: ", round(diff[[1]], 2), " ", units(diff), ". End of function.")

} #End of function
