#' Update HYDAT-related timeseries
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' First checks the local version of HYDAT using function [WRBtools::hydat_check()] and updates if needed, then checks the local copy against the one last used by the database. If needed or if force_update == TRUE, proceeds to checking each location specified for new data and replaced old data wherever a discrepancy is noted. If all WSC timeseries in the WRB hydro database are in timeseries, will also update the internal_status table with the HYDAT version used for the update.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id Character vector of timeseries_ids for which to look for updates. "all" will attempt to update all timeseries from operator 'WSC'.
#' @param force_update Set TRUE if you want to force a check of each location against the local copy of HYDAT.
#'
#' @return Updated daily means where HYDAT values were replaced by updated values.
#' @export
#'

update_hydat <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all", force_update = FALSE){

  #Check if the local copy of HYDAT needs an update
  WRBtools::hydat_check(silent = FALSE)
  hydat_path <- tidyhydat::hy_downloaded_db()

  #Check now if the DB should be updated
  local_hydat <- tidyhydat::hy_version(hydat_path)$Date
  DB_hydat <- DBI::dbGetQuery(con, "SELECT value FROM internal_status WHERE event = 'HYDAT_version'")[1,1]
  if (!is.na(DB_hydat)){
    if (DB_hydat != local_hydat){
      new_hydat <- TRUE
    } else {
      new_hydat <- FALSE
    }
  } else {
    new_hydat <- TRUE
  }
  if (!new_hydat & force_update ){
    message("Local version of HYDAT is current, updating related timeseries because force_update is set to TRUE.")
  }

  if (new_hydat | force_update){
    #Get the required timeseries_ids
    if (timeseries_id[1] == "all"){
      all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, type FROM timeseries WHERE category = 'continuous' AND operator = 'WSC' AND parameter IN ('flow', 'level')")
    } else {
      all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND category = 'continuous' AND operator = 'WSC' AND parameter IN ('flow', 'level')"))
      if (length(timeseries_id) != nrow(all_timeseries)){
        fail <- timeseries_id[!(timeseries_id %in% all_timeseries$timeseries_id)]
        if ((length(fail) == 1)) {
          warning("update_hydat: Could not find one of the timeseries_ids that you specified: ID ", fail, " is missing from the database.")
        } else {
          warning("update_hydat: Could not find some of the timeseries_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
        }
      }
    }

    #Now update historical HYDAT timeseries.
    message("Updating database with information in HYDAT due to new HYDAT version or request to force update...")
    for (i in unique(all_timeseries$location)) {
      new_flow <- as.data.frame(tidyhydat::hy_daily_flows(i))
      new_flow <- new_flow[ , c("Date", "Value", "Symbol")]
      names(new_flow) <- c("date", "value", "grade")
      new_flow <- new_flow[!is.na(new_flow$value) , ]
      new_level <- as.data.frame(tidyhydat::hy_daily_levels(i))
      new_level <- new_level[ , c("Date", "Value", "Symbol")]
      names(new_level) <- c("date", "value", "grade")
      new_level<- new_level[!is.na(new_level$value) , ]

      if (nrow(new_flow) > 0) {
        tryCatch({
          tsid_flow <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE parameter = 'flow' AND location = '", i, "' AND operator = 'WSC' AND category = 'continuous'"))[1,1]
          if (length(tsid_flow) == 0 | is.na(tsid_flow)){ #There is no realtime or daily data yet, and no corresponding tsid.
            new_entry <- data.frame("location" = i,
                                    "parameter" = "flow",
                                    "unit" = "m3/s",
                                    "category" = "continuous",
                                    "type" = "instantaneous",
                                    "start_datetime" = min(new_flow$date),
                                    "end_datetime" = max(new_flow$date),
                                    "last_new_data" = .POSIXct(Sys.time(), tz = "UTC"),
                                    "operator" = "WSC",
                                    "network" = "Canada Yukon Hydrometric Network",
                                    "public" = TRUE,
                                    "source_fx" = "getRealtimeWSC"
            )
            DBI::dbAppendTable(con, "timeseries", new_entry)
            tsid_flow <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND operator = 'WSC';"))[1,1]

            new_flow$type <- "mean"
            new_flow$approval <- "approved"
            new_flow$timeseries_id <- tsid_flow
            DBI::dbAppendTable(con, "calculated_daily", new_flow)
            calculate_stats(timeseries_id = tsid_flow,
                            con = con,
                            start_recalc = min(new_flow$date))
            message("Found historical flow daily means for a location that didn't yet exist in the local database. Added an entry to table 'timeseries' and calculated new daily stats.")
          } else { #There is a corresponding tsid in the database
            existing <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade FROM calculated_daily WHERE timeseries_id = ", tsid_flow))
            if (nrow(existing) > 0){ #There is an entry in timeseries table AND existing data in calculated_daily
              # Create a unique key for both data frames
              new_flow$key <- paste(new_flow$date, new_flow$value, new_flow$grade, sep = "|")
              existing$key <- paste(existing$date, existing$value, existing$grade, sep = "|")

              # Check for mismatches using set operations
              mismatch_keys <- setdiff(new_flow$key, existing$key)

              # Check if there are any discrepancies
              if (length(mismatch_keys) > 0) {
                mismatch <- TRUE
                date <- new_flow[new_flow$key %in% mismatch_keys, "date"]
                date <- min(date)
              } else {
                mismatch <- FALSE
              }

              if (mismatch){
                new_flow$key <- NULL
                new_flow <- new_flow[new_flow$date >= date , ]
                new_flow$type <- "mean"
                new_flow$approval <- "approved"
                new_flow$timeseries_id <- tsid_flow
                DBI::dbWithTransaction(
                  con,
                  {
                    DBI::dbExecute(con, paste0("DELETE FROM calculated_daily WHERE timeseries_id = ", tsid_flow, " AND date BETWEEN '", min(new_flow$date), "' AND '", max(new_flow$date), "';"))
                    DBI::dbAppendTable(con, "calculated_daily", new_flow)
                    start <- min(existing$date, new_flow$date)
                    DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start, "'WHERE timeseries_id = ", tsid_flow, ";"))
                  }
                )
                calculate_stats(timeseries_id = tsid_flow,
                                con = con,
                                start_recalc = start)
              }
            } else { #There is an entry in timeseries table, but no daily data
              new_flow$type <- "mean"
              new_flow$approval <- "approved"
              new_flow$timeseries_id <- tsid_flow
              DBI::dbWithTransaction(
                con, {
                  DBI::dbAppendTable(con, "calculated_daily", new_flow)
                  DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(new_flow$date), "'WHERE timeseries_id = ", tsid_flow, ";"))
                }
              )
              calculate_stats(timeseries_id = tsid_flow,
                              con = con,
                              start_recalc = min(new_flow$date))
              message("Found historical flow daily means for a location that only had realtime data. Added new entries to calculated_daily and calculated daily stats.")
            }
          }

        }, error = function(e){
          warning("Something went wrong when trying to add new flow data for location ", i)
        })
      } #End of for flow loop

      if (nrow(new_level) > 0){
        tryCatch({
          tsid_level <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE parameter = 'level' AND location = '", i, "' AND operator = 'WSC' AND category = 'continuous'"))[1,1]
          if (length(tsid_level) == 0 | is.na(tsid_level)){ #There is no realtime or daily data yet, and no corresponding tsid.
            new_entry <- data.frame("location" = i,
                                    "parameter" = "level",
                                    "unit" = "m",
                                    "category" = "continuous",
                                    "type" = "instantaneous",
                                    "start_datetime" = min(new_level$date),
                                    "end_datetime" = max(new_level$date),
                                    "last_new_data" = .POSIXct(Sys.time(), tz = "UTC"),
                                    "operator" = "WSC",
                                    "network" = "Canada Yukon Hydrometric Network",
                                    "public" = TRUE,
                                    "source_fx" = "getRealtimeWSC"
            )
            DBI::dbAppendTable(con, "timeseries", new_entry)
            tsid_level <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND operator = 'WSC';"))[1,1]

            new_level$type <- "mean"
            new_level$approval <- "approved"
            new_level$timeseries$id <- tsid_level
            DBI::dbAppendTable(con, "calculated_daily", new_level)
            calculate_stats(timeseries_id = tsid_level,
                            con = con,
                            start_recalc = min(new_level$date))
            message("Found historical level daily means for a location that didn't yet exist in the local database. Added an entry to table 'timeseries' and calculated new daily stats.")
          } else {
            existing <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade FROM calculated_daily WHERE timeseries_id = ", tsid_level))
            if (nrow(existing) > 0){ #There is an entry in timeseries table AND existing data
              # Create a unique key for both data frames
              new_level$key <- paste(new_level$date, new_level$value, new_level$grade, sep = "|")
              existing$key <- paste(existing$date, existing$value, existing$grade, sep = "|")

              # Check for mismatches using set operations
              mismatch_keys <- setdiff(new_level$key, existing$key)

              # Check if there are any discrepancies
              if (length(mismatch_keys) > 0) {
                mismatch <- TRUE
                date <- new_level[new_level$key %in% mismatch_keys, "date"]
                date <- min(date)
              } else {
                mismatch <- FALSE
              }

              if (mismatch){
                new_level$key <- NULL
                new_level <- new_level[new_level$date >= date , ]
                new_level$type <- "mean"
                new_level$approval <- "approved"
                new_level$timeseries_id <- tsid_level
                DBI::dbWithTransaction(
                  con,
                  {
                    DBI::dbExecute(con, paste0("DELETE FROM calculated_daily WHERE timeseries_id = ", tsid_level, " AND date BETWEEN '", min(new_level$date), "' AND '", max(new_level$date), "';"))
                    DBI::dbAppendTable(con, "calculated_daily", new_level)
                    start <- min(min(existing$date), new_level$date)
                    DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start, "'WHERE timeseries_id = ", tsid_level, ";"))
                  }
                )
                calculate_stats(timeseries_id = tsid_level,
                                con = con,
                                start_recalc = start)
              }
            } else { #There is an entry in timeseries table, but no daily data
              new_level$type <- "mean"
              new_level$approval <- "approved"
              new_level$timeseries_id <- tsid_level
              DBI::dbWithTransaction(
                con, {
                  DBI::dbAppendTable(con, "calculated_daily", new_level)
                  DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(new_level$date), "'WHERE timeseries_id = ", tsid_level, ";"))
                }
              )
              calculate_stats(timeseries_id = tsid_level,
                              con = con,
                              start_recalc = min(new_level$date))
              message("Found historical level daily means for a location that only had realtime data. Added new entries to calculated_daily and calculated daily stats.")
            }
          }

        }, error = function(e){
          warning("Something went wrong when trying to add new level data for location ", i)
        })
      } #End of for level loop
    } #End of for loop iterating over locations
    DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", local_hydat, "' WHERE event = 'HYDAT_version'"))
    message("Completed update of HYDAT related data.")
  } else {
    message("No updates were made because the last HYDAT version referenced in the database is the same as the current HYDAT, and you didn't specify force_update = TRUE")
    }#End of function portion that seeks to update HYDAT related data
} #End of function
