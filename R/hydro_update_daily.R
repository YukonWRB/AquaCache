#' Daily update of hydro database
#'
#' Daily update of hydro database, with multiple aims: 1. Any station and data_type added to table 'locations' is added to its relevant table, and table 'locations' is filled out; 2. Updating of datum tables if a new HYDAT database version exists or if new stations are added; 3. Calculation of daily means from realtime data and addition to relevant daily tables; 4. Calculation of daily statistics for new days since last run AND/OR for all days that may have been modified with a HYDAT update.
#'
#' The function checks for an existing HYDAT database, and will download it if it is missing or can be updated.
#'
#' Calculating daily statistics for February 29 is complicated: due to a paucity of data, this day's statistics are liable to be very mismatched from those of the preceding and succeeding days if calculated based only on Feb 29 data. Consequently, statistics for these days are computed by averaging those Feb 28 and March 1, ensuring a smooth line when graphing mean/min/max/quantile parameters.
#'
#' Timeseries that have an identical location name in WSC real-time/historical data and Aquarius will only pull from WSC information. For initial setup incorporating mirored stations in Aquarius, see function 'initial_WSC'.
#'
#' Note that this function calls hydro_update_hourly to update the realtime WSC and WRB tables; stations that were added to the table 'locations' since the last run are initialized using a separate process.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return The database is updated in-place.
#' @export
#'

hydro_update_daily <- function(path, stage = "Stage.Publish", discharge = "Discharge.Publish", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS")

{
  if (is.null(Sys.getenv("AQPASS"))){
    stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
  }
  if (is.null(Sys.getenv("AQUSER"))){
    stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
  }
  if (is.null(Sys.getenv("WS_USRNM"))){
    stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
  }
  if (is.null(Sys.getenv("WS_PWD"))){
    stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
  }

  #Ensure that existing realtime data is up-to-date from WSC and Aquarius
  hydro_update_hourly(path = path, stage = stage, discharge = discharge, server = server)

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

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  #location cross-check to catch new entries
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
  #realtime stations
  existing_locations <- data.frame("location" = NULL, "data_type" = NULL)
  WSC_flow_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WSC_flow_realtime")
  if (nrow(WSC_flow_stns) > 0){
    WSC_flow_stns$data_type <- "flow"
    existing_locations <- rbind(existing_locations, WSC_flow_stns)
  }
  WSC_level_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WSC_level_realtime")
  if (nrow(WSC_level_stns) > 0){
    WSC_level_stns$data_type <- "level"
    existing_locations <- rbind(existing_locations, WSC_level_stns)
  }
  WRB_flow_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_flow_realtime")
  if (nrow(WRB_flow_stns) > 0){
    WRB_flow_stns$data_type <- "flow"
    existing_locations <- rbind(existing_locations, WRB_flow_stns)
  }
  WRB_level_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_level_realtime")
  if (nrow(WRB_level_stns) > 0){
    WRB_level_stns$data_type <- "level"
    existing_locations <- rbind(existing_locations, WRB_level_stns)
  }
  WRB_snow_pillow_depth_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_snow_pillow_depth_realtime")
  if (nrow(WRB_snow_pillow_depth_stns) > 0){
    WRB_snow_pillow_depth_stns$data_type <- "depth"
    existing_locations <- rbind(existing_locations, WRB_snow_pillow_depth_stns)
  }
  WRB_snow_pillow_SWE_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WRB_snow_pillow_SWE_realtime")
  if (nrow(WRB_snow_pillow_SWE_stns) > 0){
    WRB_snow_pillow_SWE_stns$data_type <- "SWE"
    existing_locations <- rbind(existing_locations, WRB_snow_pillow_SWE_stns)
  }
  #daily WSC stations (daily WRB stations invariable have a real-time equivalent)
  WSC_daily_flow_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WSC_flow_daily")
  if (nrow(WSC_daily_flow_stns) > 0){
    WSC_daily_flow_stns$data_type <- "flow"
    existing_locations <- rbind(existing_locations, WSC_daily_flow_stns)
  }
  WSC_daily_level_stns <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM WSC_level_daily")
  if (nrow(WSC_daily_level_stns) > 0){
    WSC_daily_level_stns$data_type <- "level"
    existing_locations <- rbind(existing_locations, WSC_daily_level_stns)
  }
  existing_locations <- unique(existing_locations)

  new_stns <- FALSE
  if (nrow(existing_locations) < nrow(locations)){ #if TRUE, some new station or data type for an existing station has been added to the data set
    new_stns <- TRUE
    library(tidyhydat.ws) #necessary because package does not deal with data properly
    #find the new station
    new <- dplyr::anti_join(locations, existing_locations)
    for (i in 1:nrow(new)){
      tryCatch({
        if (new$data_type[i] == "SWE"){ #only option is an aquarius station
          data <- WRBtools::aq_download(new$location[i], SWE)
          #add new information to the realtime table
          ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "SWE" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "WRB_snow_pillow_SWE_realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'meteorology' WHERE location = '", new$location[i], "' AND data_type = 'SWE'"))

        } else if (new$data_type[i] == "depth"){ #only option is an aquarius station
          data <- WRBtools::aq_download(new$location[i], depth)
          #add new information to the realtime table
          ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "depth" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "WRB_snow_pillow_depth_realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'meteorology' WHERE location = '", new$location[i], "' AND data_type = 'depth'"))

        } else if(new$data_type[i] == "flow"){#check for a WSC station first, then an Aquarius station
          WSC_fail <- TRUE
          tryCatch({
          #add new information to the realtime table
          tryCatch({
            token <- tidyhydat.ws::token_ws()
            data_realtime <- NULL
            data_realtime <- tidyhydat.ws::realtime_ws(new$location[i], 47, start_date = Sys.Date()-577, token = token)
            data_realtime <- data_realtime[,c(2,4,1)]
            names(data_realtime) <- c("datetime_UTC", "flow", "location")
            data_realtime$datetime_UTC <- as.character(data_realtime$datetime_UTC)
            data_realtime$approval <- "preliminary"
            DBI::dbAppendTable(hydro, "WSC_flow_realtime", data_realtime)
            WSC_fail <- FALSE
          }, error = function(e) {
            data_realtime <- NULL
          }
          )

          #Add new information to the historical table if possible
          tryCatch({
            data_historical <- tidyhydat::hy_daily_flows(i)[,-c(3,5)]
            colnames(data_historical) <- c("location", "date", "flow")
            data_historical$approval <- "approved"
            data_historical$date <- as.character(data_historical$date)
            DBI::dbAppendTable(hydro, "WSC_flow_daily", data_historical)
            WSC_fail <- FALSE
          }, error = function(e){
            data_historical <- NULL
          }
          )

            #make the new entry into the locations table if possible
            if (!is.null(data_realtime)){
              start_datetime_realtime <- min(data_realtime$datetime_UTC)
            } else {
              start_datetime_realtime <- Sys.time()
              attr(start_datetime_realtime, "tzone") <- "UTC"
              start_datetime_realtime <- as.character(start_datetime_realtime)
            }
            if (!is.null(data_historical)){
              start_datetime_historical <- min(data_historical$date)
            } else {
              start_datetime_historical <- Sys.time()
              attr(start_datetime_historical, "tzone") <- "UTC"
              start_datetime_historical <- as.character(start_datetime_realtime)
            }
            start_datetime <- min(c(start_datetime_realtime, start_datetime_historical))

            if (!is.null(data_realtime)){
              end_datetime_realtime <- max(data_realtime$datetime_UTC)
            } else {
              end_datetime_realtime <- "1700-01-01 00:00:00"
            }
            if (!is.null(data_historical)){
              end_datetime_historical <- max(data_historical$date)
            } else {
              end_datetime_historical <- "1700-01-01 00:00:00"
            }
            end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))

            tryCatch({latitude <- tidyhydat::hy_stations(new$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
            if(length(latitude) < 1) {latitude <- NULL}
            tryCatch({longitude <- tidyhydat::hy_stations(new$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
            if(length(longitude) < 1) {longitude <- NULL}

            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "', latitude = '", latitude, "' longitude = '", longitude, "' operator = 'WSC', network = 'Canada Yukon Hydrometric Network'"))

          }, error= function(e) {
            data_realtime <- NULL
          })

          if (WSC_fail| is.null(nrow(data_realtime))){ #try for a WRB station
            data <- WRBtools::aq_download(new$location[i], discharge)
            #add new information to the realtime table
            ts <- data.frame("location" = new$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "flow" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
            DBI::dbAppendTable(hydro, "WRB_flow_realtime", ts)
            #make the new entry into table locations
            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'Small Stream Network' WHERE location = '", new$location[i], "' AND data_type = 'flow'"))
          }

        } else if (new$data_type[i] == "level") {#check for a WSC station first, then an Aquarius station
          WSC_fail <- TRUE
          tryCatch({
            #add new information to the realtime table if possible
            tryCatch({
              token <- tidyhydat.ws::token_ws()
              data_realtime <- NULL
              data_realtime <- tidyhydat.ws::realtime_ws(new$location[i], 46, start_date = Sys.Date()-577, token = token)
              data_realtime <- data_realtime[,c(2,4,1)]
              names(data_realtime) <- c("datetime_UTC", "level", "location")
              data_realtime$datetime_UTC <- as.character(data_realtime$datetime_UTC)
              data_realtime$approval <- "preliminary"
              DBI::dbAppendTable(hydro, "WSC_level_realtime", data_realtime)
              WSC_fail <- FALSE
            }, error = function(e){
              data_realtime <- NULL
            }
            )

            #add new information to the historical table if possible
            tryCatch({
              data_historical <- tidyhydat::hy_daily_levels(i)[,-c(3,5)]
              colnames(data_historical) <- c("location", "date", "level")
              data_historical$approval <- "approved"
              data_historical$date <- as.character(data_historical$date)
              DBI::dbAppendTable(hydro, "WSC_level_daily", data_historical)
              WSC_fail <- FALSE
            }, error = function(e){
              data_historical <- NULL
            }
            )

            #make the new entry into the locations table if possible
            if (!is.null(data_realtime)){
              start_datetime_realtime <- min(data_realtime$datetime_UTC)
            } else {
              start_datetime_realtime <- Sys.time()
              attr(start_datetime_realtime, "tzone") <- "UTC"
              start_datetime_realtime <- as.character(start_datetime_realtime)
            }
            if (!is.null(data_historical)){
              start_datetime_historical <- min(data_historical$date)
            } else {
              start_datetime_historical <- Sys.time()
              attr(start_datetime_historical, "tzone") <- "UTC"
              start_datetime_historical <- as.character(start_datetime_realtime)
            }
            start_datetime <- min(c(start_datetime_realtime, start_datetime_historical))

            if (!is.null(data_realtime)){
              end_datetime_realtime <- max(data_realtime$datetime_UTC)
            } else {
              end_datetime_realtime <- "1700-01-01 00:00:00"
            }
            if (!is.null(data_historical)){
              end_datetime_historical <- max(data_historical$date)
            } else {
              end_datetime_historical <- "1700-01-01 00:00:00"
            }
            end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))

            tryCatch({latitude <- tidyhydat::hy_stations(new$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
            if(length(latitude) < 1) {latitude <- NULL}
            tryCatch({longitude <- tidyhydat::hy_stations(new$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
            if(length(longitude) < 1) {longitude <- NULL}

            DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "', latitude = '", latitude, "' longitude = '", longitude, "' operator = 'WSC', network = 'Canada Yukon Hydrometric Network'"))

          }, error= function(e) {
            data_realtime <- NULL
          })
          if (WSC_fail | is.null(nrow(data_realtime))){ #try for a WRB station
            data <- WRBtools::aq_download(new$location[i], stage)
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


  ### Now deal with datums if hydat is updated or if stations were added, or if entries are missing
  datums <- DBI::dbGetQuery(hydro, "SELECT location FROM datum_conversions") #pull the existing datums
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations") #refresh of locations in case any where added
  missing_datums <- setdiff(unique(locations$location), datums$location)
  if (length(missing_datums) > 1) missing_datums <- TRUE else missing_datums <- FALSE

  #Update datum table
  if (new_hydat){
    hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
    on.exit(DBI::dbDisconnect(hydat))
    datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
    names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
    RSQLite::dbWriteTable(hydro, "datum_list", datum_list, overwrite = TRUE)
  }
  #Update datum conversions
  if (new_hydat | new_stns | missing_datums){
    hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
    on.exit(DBI::dbDisconnect(hydat))
    datum_conversions <- data.frame()
    locations_WSC <- dplyr::filter(locations, operator == "WSC")
    locations_WRB <- dplyr::filter(locations, operator == "WRB")
    if (nrow(locations_WSC) > 0){
      all_datums <- dplyr::filter(DBI::dbReadTable(hydat, "STN_DATUM_CONVERSION"), STATION_NUMBER %in% locations_WSC$location)
      all_datums$current <- NA
      names(all_datums) <- c("location", "datum_id_from", "datum_id_to", "conversion_m", "current")
      for (i in unique(locations_WSC$location)){
        subset <- dplyr::arrange(dplyr::filter(all_datums, location == i), -datum_id_to) #order most recent datum first
        if (nrow(subset) == 0){
          subset <- data.frame("location" = i, "datum_id_from" = 10, "datum_id_to" = 10, "conversion_m" = 0,  "current" = TRUE)
        } else if (nrow(subset) > 0){
          subset$current[1] <- TRUE
        }

        if (nrow(subset) > 1) {
          subset$current[2:nrow(subset)] <- FALSE
        }
        datum_conversions <- rbind(datum_conversions, subset)
      }
    }
    if (nrow(locations_WRB) > 0){
      all_datums <- data.frame(location = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA)
      for (i in 1:length(unique(locations_WRB$location))){
        #find a corresponding entry in table locations to pick a data_type
        data_type <- DBI::dbGetQuery(hydro, paste0("SELECT data_type FROM locations WHERE location = '", unique(locations_WRB$location)[i], "'"))[1,]
        ts_name <- if (data_type == "SWE") SWE else if (data_type == "depth") depth else if (data_type == "flow") discharge else if (data_type == "level") stage
        conversion <- WRBtools::aq_download(unique(locations_WRB$location)[i], ts_name, start = Sys.Date()-1)$metadata
        conversion <- conversion[7,2]
        all_datums[i, ] <- c(unique(locations_WRB$location), datum_id_from = 10, datum_id_to = 110, conversion_m = conversion, current = TRUE)
        datum_conversions <- rbind(datum_conversions, all_datums)
      }
    }
    RSQLite::dbWriteTable(hydro, "datum_conversions", datum_conversions, overwrite=TRUE)
  }


  ### Now update historical HYDAT timeseries if new_hydat == TRUE. At the same time check for new flow or level entries at existing stations.
  if (new_hydat){
    for (i in 1:nrow(locations)) { #working with object locations here and not one containing new stations because the new stations will already be up to date.
      #Deal with flows
      tryCatch({
        flow_historical <- tidyhydat::hy_daily_flows(locations$location[1])[,-c(3,5)]
        colnames(flow_historical) <- c("location", "date", "flow")
        flow_historical$approval <- "approved"
        flow_historical$date <- as.character(flow_historical$date)
        delete_bracket <- c(min(flow_historical$date), max(flow_historical$date))
        DBI::dbExecute(hydro, paste0("DELETE FROM WSC_flow_daily WHERE date BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "' AND location = '", locations$location[i], "'"))
        DBI::dbAppendTable(hydro, "WSC_flow_daily", flow_historical)
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
        colnames(level_historical) <- c("location", "date", "level")
        level_historical$approval <- "approved"
        level_historical$date <- as.character(level_historical$date)
        delete_bracket <- c(min(level_historical$date), max(level_historical$date))
        DBI::dbExecute(hydro, paste0("DELETE FROM WSC_level_daily WHERE date BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "' AND location = '", locations$location[i], "'"))
        DBI::dbAppendTable(hydro, "WSC_level_daily", level_historical)
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
  } #End of section updating information contained in HYDAT if HYDAT is new


  ### Calculate new daily means from realtime data, followed by stats
  #Get list of locations again in case it's changed.
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
  #calculate daily means for any days without them
  leap_list <- (seq(1800, 2100, by = 4))
  for (i in 1:nrow(locations)){
    loc <- locations$location[i]
    type <- locations$data_type[i]
    table_name <- if (type == "SWE") "snow_pillow_SWE" else if (type == "depth") "snow_pillow_depth" else type
    operator <- locations$operator[i]

    last_day_historic <- DBI::dbGetQuery(hydro, paste0("SELECT MAX(date) FROM ", operator, "_", table_name, "_daily WHERE location = '", loc, "'"))[1,]
    last_day_historic <- as.character(as.Date(last_day_historic) - 2) #recalculate last two days in case realtime data hasn't yet come in. This will also wipe the stats for those two days just in case.
    gap_realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM ", operator, "_", table_name, "_realtime WHERE location = '", loc, "' AND datetime_UTC BETWEEN '", last_day_historic, " 23:59:59.99' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
    if (nrow(gap_realtime) > 0){
      gap_realtime <- gap_realtime %>%
        dplyr::group_by(lubridate::year(.data$datetime_UTC), lubridate::yday(.data$datetime_UTC)) %>%
        dplyr::summarize(date = mean(lubridate::date(.data$datetime_UTC)),
                         value = mean(.data[[type]]),
                         .groups = "drop")
      gap_realtime <- gap_realtime[,c(3,4)]
      names(gap_realtime) <- c("date", type)
      gap_realtime$approval <- "preliminary"
      gap_realtime$location <- loc
      gap_realtime$date <- as.character(gap_realtime$date)
      DBI::dbExecute(hydro, paste0("DELETE FROM ", operator, "_", table_name, "_daily WHERE date > '", last_day_historic, "'"))
      DBI::dbAppendTable(hydro, paste0(operator, "_", table_name, "_daily"), gap_realtime)
    }

    # Now calculate stats where they are missing
    missing_stats <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM ", operator, "_", table_name, "_daily WHERE location = '", loc, "' AND max IS NULL"))
    all_stats <- DBI::dbGetQuery(hydro, paste0("SELECT date, ", type, " FROM ", operator, "_", table_name, "_daily WHERE location = '", loc, "'"))
    # Remove Feb. 29 data as it would mess with the percentiles; save the missing_stats ones and add them back in later. This is also important as it prevents deleting Feb 29 data in the daily table without replacing it.
    feb_29 <- missing_stats[(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
    missing_stats <- missing_stats[!(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
    all_stats <- all_stats[!(lubridate::month(all_stats$date) == "2" & lubridate::mday(all_stats$date) == "29"), , drop = FALSE]
    # Create a dayofyear column that pretends Feb 29 doesn't exist; all years have 365 days
    missing_stats <- missing_stats %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$date) %in% leap_list,
                                                                        ifelse(lubridate::month(.data$date) <= 2,
                                                                               lubridate::yday(.data$date),
                                                                               lubridate::yday(.data$date) - 1),
                                                                        lubridate::yday(.data$date)))
    all_stats <- all_stats %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$date) %in% leap_list,
                                                                ifelse(lubridate::month(.data$date) <= 2,
                                                                       lubridate::yday(.data$date),
                                                                       lubridate::yday(.data$date) - 1),
                                                                lubridate::yday(.data$date)))

    #selects only records beginning with the second dayofyear in all_stats (those for which stats can be calculated)
    missing_stats <- missing_stats[order(missing_stats[ , "date"]) , ]
    all_stats <- all_stats[order(all_stats[ , "date"]) , ]
    duplicated <- all_stats[duplicated(all_stats$dayofyear),]
    missing_stats <- missing_stats[missing_stats$date %in% duplicated$date , ]

    if (nrow(missing_stats) > 0){
    for (j in 1:nrow(missing_stats)){
      date <- missing_stats$date[j]
      doy <- missing_stats$dayofyear[j]
      current <- missing_stats[[type]][j]
      past <- all_stats[all_stats$dayofyear == doy & all_stats$date < date , ][[type]] #Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
      past <- past[!is.na(past)]
      if (length(past) >= 1){
        missing_stats$max[j] <- max(past) #again, NOT including current measurement
        missing_stats$min[j] <- min(past)
        missing_stats$QP90[j] <- stats::quantile(past, 0.90)
        missing_stats$QP75[j] <- stats::quantile(past, 0.75)
        missing_stats$QP50[j] <- stats::quantile(past, 0.50)
        missing_stats$QP25[j] <- stats::quantile(past, 0.25)
        missing_stats$QP10[j] <- stats::quantile(past, 0.10)
        if (length(past) > 1 & !is.na(current)){ #need at least 2 measurements to calculate a percent historic!
          missing_stats$percent_historic_range[j] <- ((current - min(past)) / (max(past) - min(past))) * 100
        }
      }
    }
    missing_stats <- subset(missing_stats, select=-c(dayofyear)) #remove column not in database table

      #Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st.
      if (nrow(feb_29) > 0 & !(substr(as.character(as.Date(.POSIXct(Sys.time(), "UTC"))), 6, 10) %in% c("02-29", "03-01,", "03-02"))) {
        for (k in 1:nrow(feb_29)){
          date <- as.Date(feb_29$date[k])
          before <- missing_stats[missing_stats$date == date - 1 , ]
          after <- missing_stats[missing_stats$date == date + 1 , ]
          feb_29$percent_historic_range[k] <- mean(c(before$percent_historic_range, after$percent_historic_range))
          feb_29$max[k] <- mean(c(before$max, after$max))
          feb_29$min[k] <- mean(c(before$min, after$min))
          feb_29$QP90[k] <- mean(c(before$QP90, after$QP90))
          feb_29$QP75[k] <- mean(c(before$QP75, after$QP75))
          feb_29$QP50[k] <- mean(c(before$QP50, after$QP50))
          feb_29$QP25[k] <- mean(c(before$QP25, after$QP25))
          feb_29$QP10[k] <- mean(c(before$QP10, after$QP10))
        }
      }

      missing_stats <- rbind(missing_stats, feb_29)

      DBI::dbExecute(hydro, paste0("DELETE FROM ", operator, "_", table_name, "_daily WHERE location = '", loc, "' AND date BETWEEN '", min(missing_stats$date), "' AND '", max(missing_stats$date), "'"))
      DBI::dbAppendTable(hydro, paste0(operator, "_", table_name, "_daily"), missing_stats)
    }

  } # End of for loop calculating means and stats for each station in locations table

} #End of function
