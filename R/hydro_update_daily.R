#' Daily update of hydro database
#'
#' Daily update of hydro database, with multiple aims: 1. Any station and data_type added to table 'locations' is added to its relevant table, and table 'locations' is filled out; 2. Updating of daily means and datum tables if a new HYDAT database version exists; 3. Calculation of daily means from realtime data and addition to relevant daily tables; 4. Calculation of daily statistics for new days since last run AND/OR for all days that may have been modified with a HYDAT update.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#'
#' @return The database is updated in-place.
#' @export
#'

hydro_update_daily <- function(path){

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  #location cross-check to catch new entries
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
  #realtime stations
  realtime_stns <- data.frame("location" = NULL, "data_type" = NULL)
  WSC_flow_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WSC_flow_realtime")
  if (nrow(WSC_flow_stns) > 0){
    WSC_flow_stns$data_type <- "flow"
    realtime_stns <- rbind(realtime_stns, WSC_flow_stns)
  }
  WSC_level_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WSC_level_realtime")
  if (nrow(WSC_level_stns) > 0){
    WSC_level_stns$data_type <- "level"
    realtime_stns <- rbind(realtime_stns, WSC_level_stns)
  }
  WRB_flow_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_flow_realtime")
  if (nrow(WRB_flow_stns) > 0){
    WRB_flow_stns$data_type <- "flow"
    realtime_stns <- rbind(realtime_stns, WRB_flow_stns)
  }
  WRB_level_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_level_realtime")
  if (nrow(WRB_level_stns) > 0){
    WRB_level_stns$data_type <- "level"
    realtime_stns <- rbind(realtime_stns, WRB_level_stns)
  }
  WRB_snow_pillow_depth_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_snow_pillow_depth_realtime")
  if (nrow(WRB_snow_pillow_depth_stns) > 0){
    WRB_snow_pillow_depth_stns$data_type <- "depth"
    realtime_stns <- rbind(realtime_stns, WRB_snow_pillow_depth_stns)
  }
  WRB_snow_pillow_SWE_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_snow_pillow_SWE_realtime")
  if (nrow(WRB_snow_pillow_SWE_stns) > 0){
    WRB_snow_pillow_SWE_stns$data_type <- "SWE"
    realtime_stns <- rbind(realtime_stns, WRB_snow_pillow_SWE_stns)
  }

  if (nrow(realtime_stns) < nrow(locations)){ #if TRUE, some new station or data type for an existing station has been added to the data set
    new_stns <- TRUE
    #find the new station
    new <- dplyr::anti_join(locations, realtime_stns)
    for (i in nrow(new)){
      tryCatch({
        if (new$data_type[i] == "SWE"){ #only option is an aquarius station
          data <- WRBtools::aq_download(new$location[i], "SWE.Corrected")
          #add new information to the realtime table
          ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "SWE" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "WRB_snow_pillow_SWE_realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'meteorology' WHERE location = '", new$location[i], "' AND data_type = 'SWE'"))

        } else if (new$data_type[i] == "depth"){ #only option is an aquarius station
          data <- WRBtools::aq_download(new$location[i], "Snow Depth.TempCompensated.Corrected")
          #add new information to the realtime table
          ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "depth" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "WRB_snow_pillow_depth_realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'meteorology' WHERE location = '", new$location[i], "' AND data_type = 'depth'"))

        } else if(new$data_type[i] == "flow"){#check for a WSC station first, then an Aquarius station
          WSC_fail <- FALSE
          tryCatch({
            token <- tidyhydat.ws::token_ws()
            data <- tidyhydat.ws::realtime_ws(new$location[i], 47, start_date = Sys.Date()-577, token = token)
            data <- data[,c(2,4,1)]
            names(data) <- c("datetime_UTC", "flow", "location")
            data$datetime_UTC <- as.character(data$datetime_UTC)
            data$approval <- "preliminary"
            #add new information to the realtime table
            DBI::dbAppendTable(hydro, "WSC_flow_realtime", data)
            #make the new entry into the locations table
            start_datetime <- as.character(min(data$datetime_UTC))
            end_datetime <- as.character(max(data$datetime_UTC))
            tryCatch({latitude <- tidyhydat::hy_stations(new$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
            if(length(latitude) < 1) {latitude <- NULL}
            tryCatch({longitude <- tidyhydat::hy_stations(new$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
            if(length(longitude) < 1) {longitude <- NULL}

            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "', latitude = '", latitude, "' longitude = '", longitude, "' operator = 'WSC', network = 'Canada Yukon Hydrometric Network'"))

          }, error= function(e) {
            WSC_fail <- TRUE
          })
          if (WSC_fail| is.null(nrow(data))){ #try for a WRB station
            data <- WRBtools::aq_download(new$location[i], "Discharge.Publish")
            #add new information to the realtime table
            ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "flow" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
            DBI::dbAppendTable(hydro, "WRB_flow_realtime", ts)
            #make the new entry into table locations
            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'Small Stream Network' WHERE location = '", new$location[i], "' AND data_type = 'flow'"))
          }

        } else if (new$data_type[i] == "level") {#check for a WSC station first, then an Aquarius station
          WSC_fail <- FALSE
          tryCatch({
            token <- tidyhydat.ws::token_ws()
            rm(data)
            data <- tidyhydat.ws::realtime_ws(new$location[i], 46, start_date = Sys.Date()-577, token = token)
            data <- data[,c(2,4,1)]
            names(data) <- c("datetime_UTC", "level", "location")
            data$datetime_UTC <- as.character(data$datetime_UTC)
            data$approval <- "preliminary"
            #add new information to the realtime table
            DBI::dbAppendTable(hydro, "WSC_level_realtime", data)
            #make the new entry into the locations table
            start_datetime <- as.character(min(data$datetime_UTC))
            end_datetime <- as.character(max(data$datetime_UTC))
            tryCatch({latitude <- tidyhydat::hy_stations(new$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
            if(length(latitude) < 1) {latitude <- NULL}
            tryCatch({longitude <- tidyhydat::hy_stations(new$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
            if(length(longitude) < 1) {longitude <- NULL}

            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "', latitude = '", latitude, "' longitude = '", longitude, "' operator = 'WSC', network = 'Canada Yukon Hydrometric Network'"))

          }, error= function(e) {
            WSC_fail <- TRUE
          })
          if (WSC_fail | is.null(nrow(data))){ #try for a WRB station
            data <- WRBtools::aq_download(new$location[i], "Stage.Publish")
            #add new information to the realtime table
            ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "level" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
            DBI::dbAppendTable(hydro, "WRB_level_realtime", ts)
            #make the new entry into table locations
            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'Small Stream Network' WHERE location = '", new$location[i], "' AND data_type = 'level'"))
          }

        }
      }, error = function(e) {
      })
    } #End of for loop that works on every new station
  } #End of if loop to deal with new stations


  #Now deal with the daily data: update HYDAT database first, then update both datum tables  if necessary, then calculate daily means for non WSC locations and for realtime-covered days, then calculate stats
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
    }
  } else if (is.null(hydat_path) | !exists("local_hydat")) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask=FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
    new_hydat <- TRUE
  }
  hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
  on.exit(DBI::dbDisconnect(hydat))

  #Update datum table
  if (new_hydat){
    datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
    names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
    RSQLite::dbWriteTable(hydro, "datum_list", datum_list, overwrite = TRUE)
  }
  #Update datum conversions
  if (new_hydat | new_stns){
    locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
    datum_conversions <- data.frame()
    #WSC stations first
    locations_WSC <- dplyr::filter(locations, operator == "WSC")
    locations_WRB <- dplyr::filter(locations, operator == "WRB")
    if (nrow(locations_WSC) > 0){
      all_datums <- dplyr::filter(DBI::dbReadTable(hydat, "STN_DATUM_CONVERSION"), STATION_NUMBER %in% locations_WSC$location)
      all_datums$current <- NA
      names(all_datums) <- c("location", "datum_id_from", "datum_id_to", "conversion_m", "current")
      for (i in unique(all_datums$location)){
        subset <- dplyr::arrange(dplyr::filter(all_datums, location == i), -datum_id_to) #order most recent datum first
        subset$current[1] <- TRUE
        if (nrow(subset) > 1) {
          subset$current[2:nrow(subset)] <- FALSE
        }
        datum_conversions <- rbind(datum_conversions, subset)
      }
    }
    if(nrow(locations_WRB) > 0){
      all_datums <- data.frame(location = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA)
      for (i in 1:length(unique(locations_WRB$location))){
        #find a corresponding entry in table locations to pick a data_type
        data_type <- DBI::dbGetQuery(hydro, paste0("SELECT data_type FROM locations WHERE location = '", unique(locations_WRB$location)[i], "'"))[1,]
        ts_name <- if (data_type == "SWE") "SWE.Corrected" else if (data_type == "depth") "Snow Depth.TempCompensated.Corrected" else if (data_type == "flow") "Discharge.Publish" else if (data_type == "level") "Stage.Publish"
        conversion <- WRBtools::aq_download(unique(locations_WRB$location)[i], ts_name, start = Sys.Date()-1)$metadata
        conversion <- conversion[7,2]
        all_datums[i, ] <- c(unique(locations_WRB$location), datum_id_from = 10, datum_id_to = 110, conversion_m = conversion, current = TRUE)
        datum_conversions <- rbind(datum_conversions, all_datums)
      }
    }

    RSQLite::dbWriteTable(hydro, "datum_conversions", datum_conversions, overwrite=TRUE)
  }

  #Now calculate daily means and stats where necessary
  if (new_hydat){ #all daily means need to be updated, stats recalculated for updated days.

  }
  if (new_stns & !(new_hydat)){ #only new stations need daily means and stats calculated

  }

} #End of function
