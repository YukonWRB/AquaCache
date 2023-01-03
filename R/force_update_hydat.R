
#' Force update of information contained in HYDAT
#'
#' By default, the database update functions only update historical information for WSC locations when there is an update to the HYDAT database. However, it may be necessary to refresh this information from time-to-time, for example if any records were accidentally deleted. This function first checks for updates to the HYDAT database on the remote, then replaces locally held information with the HYDAT copy. The function hydro_update_daily is then called to update the daily statistics, as these will be removed in the refresh process. Timeseries held in Aquarius are not affected.
#' The function also cross-checks any WSC locations present in realtime tables and attempts to find a corresponding historical time-series. Lastly, the table locations will be updated if any new information is found.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#'
#' @return The database is updated in-place.
#' @export
#'

force_update_hydat <- function(path)

{

  #Check hydat version, update if needed.
  tryCatch({hydat_path <- tidyhydat::hy_downloaded_db() #Attempts to get the hydat path, in case it's downloaded already.
  local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
  }, error = function(e) {hydat_path <- NULL})

  new_hydat <- FALSE
  if (!is.null(hydat_path) & exists("local_hydat")){ #If hydat already exists, compare version numbers
    local_hydat <- gsub("-", "", as.character(local_hydat))
    remote_hydat <- tidyhydat::hy_remote()
    if (local_hydat != remote_hydat){ #if remote version is more recent, download new version
      tidyhydat::download_hydat(ask=FALSE)
      hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
      new_hydat <- TRUE
      print("The local WSC HYDAT database was updated.")
    }
  } else if (is.null(hydat_path) | !exists("local_hydat")) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask=FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
    new_hydat <- TRUE
    print("A local copy of the WSC HYDAT database was installed.")
  }

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))
  DBI::dbExecute(hydro, "PRAGMA busy_timeout=10000")

  ### Now update historical HYDAT timeseries. At the same time check for new flow or level entries at existing stations, and update the locations table if needed.
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
  for (i in 1:nrow(locations)) { #working with object locations here and not one containing new stations because the new stations will already be up to date.
    #Deal with flows
    tryCatch({
      flow_historical <- tidyhydat::hy_daily_flows(locations$location[1])[,-c(3,5)]
      colnames(flow_historical) <- c("location", "date", "value")
      flow_historical$approval <- "approved"
      flow_historical$units <- "m3/s"
      flow_historical$date <- as.character(flow_historical$date)
      delete_bracket <- c(min(flow_historical$date), max(flow_historical$date))
      DBI::dbExecute(hydro, paste0("DELETE FROM flow_daily WHERE date BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "' AND location = '", locations$location[i], "'"))
      DBI::dbAppendTable(hydro, "flow_daily", flow_historical)
      #check if it already exists in locations table
      ts <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", locations$location[i], "' AND data_type = 'flow'"))
      if (nrow(ts) == 0){ #It is a new TS at an existing location: add entry to locations table from scratch
        info <- locations[i,]
        info$data_type <- "flow"
        info$start_datetime <- paste0(min(flow_historical$date), " 00:00:00")
        info$end_datetime <- paste0(max(flow_historical$date), " 00:00:00")
        tryCatch({latitude <- tidyhydat::hy_stations(locations$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
        if(length(latitude) < 1) {latitude <- NULL}
        tryCatch({longitude <- tidyhydat::hy_stations(locations$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
        if(length(longitude) < 1) {longitude <- NULL}
        info$latitude <- latitude
        info$longitude <- longitude
        info$operator <- "WSC"
        info$network <- "Canada Yukon Hydrometric Network"
        DBI::dbAppendTable(hydro, "locations", info)
      } else { #The time-series already exists and the locations table entry exists. Re-input start time in case it changed and end time if it is after entry in realtime table
        start_datetime <- paste0(min(flow_historical$date), " 00:00:00")
        end_datetime_historical <- paste0(max(flow_historical$date), " 00:00:00")
        end_datetime_realtime <- ts$end_datetime
        end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))
        DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE location = '", locations$location[i], "' AND data_type = 'flow'"))
      }
    }, error = function(e){}
    )
    tryCatch({
      level_historical <- tidyhydat::hy_daily_levels(locations$location[1])[,-c(3,5)]
      colnames(level_historical) <- c("location", "date", "value")
      level_historical$approval <- "approved"
      level$historical$units <- "m"
      level_historical$date <- as.character(level_historical$date)
      delete_bracket <- c(min(level_historical$date), max(level_historical$date))
      DBI::dbExecute(hydro, paste0("DELETE FROM level_daily WHERE date BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "' AND location = '", locations$location[i], "'"))
      DBI::dbAppendTable(hydro, "level_daily", level_historical)
      #check if it already exists in locations table
      ts <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", locations$location[i], "' AND data_type = 'level'"))
      if (nrow(ts) == 0){ #It is a new TS at an existing location: add entry to locations table from scratch
        info <- locations[i,]
        info$data_type <- "level"
        info$start_datetime <- paste0(min(level_historical$date), " 00:00:00")
        info$end_datetime <- paste0(max(level_historical$date), " 00:00:00")
        tryCatch({latitude <- tidyhydat::hy_stations(locations$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
        if(length(latitude) < 1) {latitude <- NULL}
        tryCatch({longitude <- tidyhydat::hy_stations(locations$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
        if(length(longitude) < 1) {longitude <- NULL}
        info$latitude <- latitude
        info$longitude <- longitude
        info$operator <- "WSC"
        info$network <- "Canada Yukon Hydrometric Network"
        DBI::dbAppendTable(hydro, "locations", info)
      } else { #The time-series already exists and the locations table entry exists. Re-input start time in case it changed and end time if it is after entry in realtime table
        start_datetime <- paste0(min(level_historical$date), " 00:00:00")
        end_datetime_historical <- paste0(max(level_historical$date), " 00:00:00")
        end_datetime_realtime <- ts$end_datetime
        end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))
        DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE location = '", locations$location[i], "' AND data_type = 'level'"))
      }

    }, error = function(e){
      level_historical <- NULL
    }
    )
  } #End of for loop updating information contained in HYDAT

} #End of function.
