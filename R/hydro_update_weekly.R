#' Weekly update of hydro database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved". Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables.
#'
#'Any timeseries labelled as 'getRealtimeAQ' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [WRBtools::aq_download()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct) from which to look for possible new data. You can specify a single start_datetime to apply to all `timeseries_id`, or one per element of `timeseries_id.`
#'
#' @return Updated entries in the hydro database.
#' @export
#'

#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

hydro_update_weekly <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all", start_datetime)
{

  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")
  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, type FROM timeseries WHERE category = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
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

  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    param_code <- settings[settings$parameter == parameter & settings$source_fx == source_fx , "remote_param_name"]
    start_dt <- if (length(start_datetime) > 1) start_datetime[i] else start_datetime

    tryCatch({
      ts <- do.call(source_fx, list(location = loc, param_code = param_code, start_datetime = start_dt))
      if (nrow(ts) > 0){
        if ("period" %in% check){
          if (all_timeseries$type[i] == "instantaneous"){
            ts$period <- "00:00:00"
          } else if (!("period" %in% names(ts))){
            ts$period <- NA
          } else {
            check_period <- lubridate::period(unique(ts$period))
            if (NA %in% check_period){
              ts$period <- NA
            }
          }
        }

        realtime <- DBI::dbGetQuery(con, paste0("SELECT * FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start_dt, "';"))

        # Create a unique datetime key for both data frames
        ts$key <- paste(ts$datetime, ts$value, ts$grade, ts$approval, ts$period, sep = "|")
        realtime$key <- paste(realtime$datetime, realtime$value, realtime$grade, realtime$approval, realtime$period, sep = "|")

        # Check for mismatches using set operations
        mismatch_keys <- setdiff(ts$key, realtime$key)

        # Check if there are any discrepancies
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
          ts$timeseries_id <- tsid
          DBI::dbWithTransaction(
            con,
            {
              DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", min(ts$datetime), "' AND '", max(ts$datetime), "';"))
              DBI::dbAppendTable(con, "measurements_continuous", ts)
              #make the new entry into table timeseries
              end <- max(max(realtime$datetime), ts$datetime)
              DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", end, "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))
            }
          )

          #Recalculate daily means and statistics
          calculate_stats(timeseries = tsid,
                          con = con,
                          start_recalc = as.Date(substr(datetime, 1, 10)))
        }
      }
    }, error = function(e) {
      warning("Hydro_update_weekly failed on location ", loc, " and parameter ", parameter, " (location_id ", tsid, ").")
    }
    )
  }

  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_weekly';"))

} #End of function
