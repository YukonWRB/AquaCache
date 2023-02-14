#' Update HYDAT-related timeseries
#'
#' First checks and updates the local version of HYDAT if needed, then checks the local copy against the one last used by the database. If needed or if force_update == TRUE, proceeds to checking each location specified for new data and replaced old data wherever a discrepancy is noted. If all WSC timeseries in the WRB hydro database are in timeseries, will also update the internal_status table with the HYDAT version used for the update.
#'
#' @param timeseries Character vector of timeseries for which to look for updates.
#' @param path The path to the hydrometric database, passed to WRBtools::hydroConnect.
#' @param force_update Set TRUE if you want to force a check of each location against the local copy of HYDAT.
#'
#' @return Updated daily means where HYDAT values exist, and a boolean indicating if the HYDAT database was in fact updated. Note: if force_update is TRUE but there is no new HYDAT database, FALSE will be returned even though database timeseries are updated.
#' @export
#'

update_hydat <- function(timeseries, path, force_update = FALSE){

  hydro <- WRBtools::hydroConnect(path = path)
  on.exit(DBI::dbDisconnect(hydro))

  #Check if the local copy of HYDAT needs an update
  WRBtools::hydat_check(silent = FALSE)
  hydat_path <- tidyhydat::hy_downloaded_db()

  #Check now if the DB should be updated
  if (!force_update){ #Check if HYDAT last used with the DB is older than the new hydat
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    DB_hydat <- as.character(DBI::dbGetQuery(hydro, "SELECT value FROM internal_status WHERE event = 'HYDAT_version'"))
    if (DB_hydat != local_hydat){
      new_hydat <- TRUE
    } else {
      new_hydat <- FALSE
    }
  }
  if (force_update){
    if (!new_hydat){
      print("Local version of HYDAT is current, updating related timeseries because force_update is set to TRUE.")
    }
  }

  #Now update historical HYDAT timeseries if new_hydat == TRUE. At the same time check for new flow or level entries at existing stations.
  if (new_hydat | force_update){
    print("Updating historical information in HYDAT due to new database or request to force update...")
    for (i in unique(timeseries)) {
      #Work with flows first
      tryCatch({
        flow_historical <- tidyhydat::hy_daily_flows(i)[,-c(3,5)]
        colnames(flow_historical) <- c("location", "date", "value")
        flow_historical$parameter <- "flow"
        flow_historical$approval <- "approved"
        flow_historical$units <- "m3/s"
        flow_historical$date <- as.character(flow_historical$date)

        existing <- DBI::dbGetQuery(hydro, paste0("SELECT datetime_UTC, value FROM daily WHERE location = '", i, "' AND parameter = 'flow'"))
        if (nrow(existing) > 0){
          mismatch <- FALSE
          done <- FALSE
          while (!mismatch & !done){
            for (j in 1:nrow(flow_historical)){
              date <- flow_historical$date[i]
              if (date %in% existing$date){ # check that the corresponding date exists in existing. If not, mismatch is TRUE
                if (!(flow_historical[flow_historical$date == date, "value"] == existing[existing$date == date, "value"])){
                  mismatch <- TRUE
                }
              } else {
                mismatch <- TRUE
              }
              if (j == nrow(ts)){
                done <- TRUE
              }
            }
          }
          if (mismatch){ #only need to append new if mismatch == TRUE, otherwise the TS was not yet updated in HYDAT.
            flow_historical <- flow_historical[flow_historical$date >= new_hydat_start , ]
            DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE date >= '", min(flow_historical$date), "' AND location = '", i, "' AND parameter = 'flow'")) #Deletes everything after the first HYDAT entry that is not in or different from the database.
            DBI::dbAppendTable(hydro, "daily", flow_historical)
            DBI::dbExecute(hydro, paste0("UPDATE timeseries SET last_daily_calculation = NULL WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous' AND operator = 'WSC'"))
          }
        } else {
          DBI::dbAppendTable(hydro, "daily", flow_historical)
        }

        #check if it already exists in timeseries table
        ts <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND type = 'continuous'"))
        if (nrow(ts) == 0){ #It is a new TS at an existing location: add entry to timeseries table from scratch
          timeseries_info <- data.frame("location" = i)
          location_info <- data.frame("location" = i)
          timeseries_info$parameter <- "flow"
          timeseries_info$units <- "m3/s"
          timeseries_info$type <- "continuous"
          timeseries_info$start_datetime_UTC <- paste0(min(flow_historical$date), " 00:00:00")
          timeseries_info$end_datetime_UTC <- paste0(max(flow_historical$date), " 00:00:00")
          timeseries_info$last_new_data_UTC <- paste0(max(flow_historical$date), " 00:00:00")
          timeseries_info$last_daily_calculation_UTC <- paste0(max(flow_historical$date), " 00:00:00")
          timeseries_info$operator <- "WSC"
          timeseries_info$network <- "Canada Yukon Hydrometric Network"
          DBI::dbAppendTable(hydro, "timeseries", timeseries_info)

          tryCatch({latitude <- tidyhydat::hy_stations(i)$LATITUDE}, error = function(e) {latitude <- NULL})
          if(length(latitude) < 1) {latitude <- NULL}
          tryCatch({longitude <- tidyhydat::hy_stations(i)$LONGITUDE}, error = function(e) {longitude <- NULL})
          if(length(longitude) < 1) {longitude <- NULL}
          location_info$latitude <- latitude
          location_info$longitude <- longitude
          location_info$name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
          DBI::dbAppendTable(hydro, "locations", location_info)
        } else { #The time-series already exists and the timeseries table entry exists. Re-input start time in case it changed and end time if it is after entry in realtime table
          start_datetime <- paste0(min(flow_historical$date), " 00:00:00")
          end_datetime_historical <- paste0(max(flow_historical$date), " 00:00:00")
          end_datetime_realtime <- ts$end_datetime
          end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))
          DBI::dbExecute(hydro, paste0("UPDATE timeseries SET start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "' WHERE location = '", i, "' AND parameter = 'flow' AND type = 'continuous'"))
        }

        #Recalculate daily means and stats
        calculate_stats(timeseries = data.frame("location" = i,
                                               "parameter" = "flow"),
                        path = path)

      }, error = function(e){}
      ) #End of flow tryCatch

      #Do the same thing for levels
      tryCatch({
        level_historical <- tidyhydat::hy_daily_levels(i)[,-c(3,5)]
        colnames(level_historical) <- c("location", "date", "value")
        level_historical$parameter <- "level"
        level_historical$approval <- "approved"
        level_historical$units <- "m"
        level_historical$date <- as.character(level_historical$date)

        existing <- DBI::dbGetQuery(hydro, paste0("SELECT datetime_UTC, value FROM daily WHERE location = '", i, "' AND parameter = 'level'"))
        if (nrow(existing) > 0){
          mismatch <- FALSE
          done <- FALSE
          while (!mismatch & !done){
            for (j in 1:nrow(level_historical)){
              date <- level_historical$date[i]
              if (date %in% existing$date){ # check that the corresponding date exists in existing. If not, mismatch is TRUE
                if (!(level_historical[level_historical$date == date, "value"] == existing[existing$date == date, "value"])){
                  mismatch <- TRUE
                }
              } else {
                mismatch <- TRUE
              }
              if (j == nrow(ts)){
                done <- TRUE
              }
            }
          }
          if (mismatch){ #only need to append new if mismatch == TRUE, otherwise the TS was not yet updated in HYDAT.
            level_historical <- level_historical[level_historical$date >= new_hydat_start , ]
            DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE date >= '", min(level_historical$date), "' AND location = '", i, "' AND parameter = 'level'"))
            DBI::dbAppendTable(hydro, "daily", level_historical)
            DBI::dbExecute(hydro, paste0("UPDATE timeseries SET last_daily_calculation = 'NULL' WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous' AND operator = 'WSC'"))
          }
        } else {
          DBI::dbAppendTable(hydro, "daily", level_historical)
        }
        #check if it already exists in timeseries table
        ts <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous'"))
        if (nrow(ts) == 0){ #It is a new TS at an existing location: add entry to timeseries table from scratch, update locations table
          timeseries_info <- data.frame("location" = i)
          location_info <- data.frame("location" = i)
          timeseries_info$parameter <- "level"
          timeseries_info$units <- "m"
          timeseries_info$type <- "continuous"
          timeseries_info$start_datetime_UTC <- paste0(min(level_historical$date), " 00:00:00")
          timeseries_info$end_datetime_UTC <- paste0(max(level_historical$date), " 00:00:00")
          timeseries_info$last_new_data_UTC <- paste0(max(level_historical$date), " 00:00:00")
          timeseries_info$last_daily_calculation_UTC <- paste0(max(level_historical$date), " 00:00:00")
          timeseries_info$operator <- "WSC"
          timeseries_info$network <- "Canada Yukon Hydrometric Network"
          DBI::dbAppendTable(hydro, "timeseries", timeseries_info)

          tryCatch({latitude <- tidyhydat::hy_stations(i)$LATITUDE}, error = function(e) {latitude <- NULL})
          if(length(latitude) < 1) {latitude <- NULL}
          tryCatch({longitude <- tidyhydat::hy_stations(i)$LONGITUDE}, error = function(e) {longitude <- NULL})
          if(length(longitude) < 1) {longitude <- NULL}
          location_info$latitude <- latitude
          location_info$longitude <- longitude
          location_info$name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
          DBI::dbAppendTable(hydro, "locations", location_info)

        } else { #The time-series already exists and the timeseries and locations table entries exists. Re-input start time in case it changed and end time if it is after entry in realtime table
          start_datetime <- paste0(min(level_historical$date), " 00:00:00")
          end_datetime_historical <- paste0(max(level_historical$date), " 00:00:00")
          end_datetime_realtime <- ts$end_datetime
          end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))
          DBI::dbExecute(hydro, paste0("UPDATE timeseries SET start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "' WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous'"))
        }
        #Recalculate daily means and stats
        calculate_stats(timeseries = data.frame("location" = i,
                                               "parameter" = "level"),
                        path = path)

      }, error = function(e){}
      ) #End of level section
    } #End of for loop updating information contained in HYDAT for each timeseries

    timeseries_WSC <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM timeseries WHERE operator = 'WSC'")
    if (length(timeseries_WSC$location) == length(timeseries)){
      DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", local_hydat, "' WHERE event = 'HYDAT_version'"))
    }
  } #End of section updating information contained in HYDAT if HYDAT is new

  return(new_hydat)

} #End of function
