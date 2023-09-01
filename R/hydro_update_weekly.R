#' Weekly update of hydro database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved". Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables.
#'
#'Any timeseries labelled as 'getRealtimeAQ' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [WRBtools::aq_download()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [WRBtools::hydroConnect()].
#' @param tsid The timeseries_id you wish to have updated. Defaults to "all". Will search for all timeseries with the specified location codes, so level + flow, snow depth + SWE, etc.
#' @param start_datetime The datetime (as a POSIXct) from which to look for possible new data.
#'
#' @return Updated entries in the hydro database.
#' @export
#'

#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

hydro_update_weekly <- function(con, tsid = "all", start_datetime)
{

  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")
  if (tsid == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx FROM timeseries WHERE category = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx FROM timeseries WHERE timeseries_id IN ('", paste(tsid, collapse = "', '"), "')"))
  }

  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    param_code <- settings[settings$parameter == parameter & settings$source_fx == source_fx , "remote_param_name"]

    tryCatch({
        ts <- do.call(source_fx, list(location = loc, param_code = param_code, start_datetime = start_datetime))
      if (nrow(ts) > 0){
        realtime <- DBI::dbGetQuery(con, paste0("SELECT * FROM realtime WHERE timeseries_id = ", tsid, " AND datetime >= '", start_datetime, "';"))
        mismatch <- FALSE
        done <- FALSE
        row <- 1
        while (!mismatch & !done){
          datetime <- ts$datetime[row]
          if (datetime %in% realtime$datetime){ # check that the corresponding time exists in realtime. If not, mismatch is automatically TRUE
            if (!(ts[ts$datetime == datetime, "value"] == realtime[realtime$datetime == datetime, "value"]) | !(ts[ts$datetime == datetime, "grade"] == realtime[realtime$datetime == datetime, "grade"]) | !(ts[ts$datetime == datetime, "approval"] == realtime[realtime$datetime == datetime, "approval"])) { #check that values are the same
              mismatch <- TRUE
              if (row > 1) { #Go back to the last known good point if not the first row
                datetime <- ts$datetime[row-100]
              }
            } else {
              row <- row + 100 #Go up by increments of 100 for the sake of speed
            }
          } else {
            mismatch <- TRUE
            if (row > 1) { #Go back to the last known good point if not the first row
              datetime <- ts$datetime[row-100]
            }
          }
          if (row == nrow(ts)){
            done <- TRUE
          }
        }
        if (mismatch){
          ts <- ts[ts$datetime >= datetime , ]
          ts$timeseries_id <- tsid
          DBI::dbExecute(con, paste0("DELETE FROM realtime WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", min(ts$datetime), "' AND '", max(ts$datetime), "';"))
          DBI::dbAppendTable(con, "realtime", ts)
          #make the new entry into table timeseries
          end <- max(max(realtime$datetime), ts$datetime)
          DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", end, "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid, ";"))

          #Recalculate daily means and statistics
          calculate_stats(timeseries = data.frame("location" = loc,
                                                  "parameter" = parameter),
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
