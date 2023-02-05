#' Daily update of hydro database
#'
#' Daily update of hydro database, with multiple aims: 1. Incorporation of new realtime information; 2. Any station and parameter added to table 'locations' is added to its relevant table, and table 'locations' is filled out; 3. Updating of datum tables and related data if a new HYDAT database version exists or if new stations are added; 4. Calculation of daily means from realtime data and addition to relevant daily tables; 5. Calculation of daily statistics for new days since last run AND/OR for all days that may have been modified with a HYDAT update. In addition, if tables already exist for watershed polygons or discrete snow survey measurements, these will be populated upon addition of new WSC locations or, for snow surveys, updated twice a month during the winter season with new data.
#'
#' The function checks for an existing HYDAT database, and will download it if it is missing or can be updated. At the same time any affected daily timeseries are recalculated (using HYDAT daily means and calculated means) starting from the first day where the new HYDAT information diverges from the existing daily means.
#'
#' Calculating daily statistics for February 29 is complicated: due to a paucity of data, this day's statistics are liable to be very mismatched from those of the preceding and succeeding days if calculated based only on Feb 29 data. Consequently, statistics for these days are computed by averaging those of Feb 28 and March 1, ensuring a smooth line when graphing mean/min/max/quantile parameters. This necessitates waiting for complete March 1st data, so Feb 29 means and stats will be delayed until March 2nd.
#'
#' Timeseries that have an identical location name in WSC real-time/historical data and Aquarius will only pull from WSC information. For initial setup incorporating mirrored stations in Aquarius, see function 'initial_WSC'.
#'
#' Note that this function calls hydro_update_hourly to update the realtime tables; stations that were added to the table 'locations' since the last run are initialized using a separate process.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param aquarius TRUE if you are fetching new realtime data from Aquarius, in which case you should also check the next five parameters. FALSE will only populate with WSC station data.
#' @param stage The name of the stage (level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge (flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label.
#' @param distance The name of the distance timeseries as it appears in Aquarius if it exists, in the form Parameter.Label. All stations must have the same parameter and label. Usually used for distance from bridge girders to water surface.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#' @param snow_db_path The path to the snow survey database.
#'
#' @return The database is updated in-place.
#' @import tidyhydat.ws
#' @export
#'

hydro_update_daily <- function(path, aquarius = TRUE, stage = "Stage.Corrected", discharge = "Discharge.Master", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", distance = "Distance.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS", snow_db_path = "//carver/infosys/Snow/DB/SnowDB.mdb")

{

  #TODO: This entire function needs to be adapted to work with to-be-defined parameters, like temperature or precip. right now lots of steps rely on hard-coded parameter names, but this could be more flexible. Two options: get new parameter names from the 'locations' table when new entries are made, or have a single list of defined parameters and associated units here.
  #Settings table now contains value pairs for parameter and TS name in Aquarius. Needs to be adapted throughout all functions.

  function_start <- Sys.time()

  if (aquarius){
    if (is.null(Sys.getenv("AQPASS"))){
      stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
    }
    if (is.null(Sys.getenv("AQUSER"))){
      stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
    }
  }
  if (is.null(Sys.getenv("WS_USRNM"))){
    stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
  }
  if (is.null(Sys.getenv("WS_PWD"))){
    stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
  }

  hydro <- WRBtools::hydroConnect(path = path) #Connect to the hydro database
  on.exit(DBI::dbDisconnect(hydro), add=TRUE)

  #Ensure that existing realtime data is up-to-date from WSC and Aquarius
  print("Getting realtime information up to date with hydro_update_hourly...")
  hourly_start <- Sys.time()
  hydro_update_hourly(path = path, aquarius = aquarius, stage = stage, discharge = discharge, SWE = SWE, depth = depth, server = server)
  hourly_duration <- Sys.time() - hourly_start
  print(paste0("Hydro_update_hourly executed in ", round(hourly_duration[[1]], 2), " ", units(hourly_duration), "."))

  # Get updated snow course measurements if in season, only if the table exists
  tables <- DBI::dbListTables(hydro)
  if ("discrete" %in% tables) { #Doesn't run if not there!
    if ((1 < lubridate::month(Sys.Date())) & (lubridate::month(Sys.Date()) < 7)){ #only from Feb to June inclusively
      tryCatch({
        print("Looking for new discrete measurements...")
        if (lubridate::day(Sys.Date()) %in% c(1, 15)){ #overwrite twice a month to capture any changes
          getSnowCourse(hydro_db_path = path, snow_db_path = snow_db_path, inactive = FALSE, overwrite = TRUE)
        } else {
          getSnowCourse(hydro_db_path = path, snow_db_path = snow_db_path, inactive = FALSE, overwrite = FALSE)
        }
      }, error = function(e) {
        print("Failed to get new snow survey measurements.")
      })
    }
  }

  #Check for a new version of HYDAT, update timeseries in the database if needed.
  print("Checking for new HYDAT database...")
  locations_WSC <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM locations WHERE operator = 'WSC'")
  new_hydat <- update_hydat(locations = locations_WSC$location, path = path, force_update = FALSE) #This function is run for flow and level for each station, even if one is not currently in the HYDAT database. This allows for new data streams to be incorporated seamlessly, either because HYDAT covers a station already reporting or because a flow/level only station is reporting the other param.


  #TODO: This next step, checking for new entries, needs to be simplified. In particular it needs to work with yet-to-be defined parameters, like temperature, precip, etc.
  print("Checking tables to see if there are new entries of type 'continuous'...")
  new_stns <- FALSE
  new_locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE start_datetime_UTC IS NULL AND name IS NOT 'FAILED' AND parameter IN ('flow', 'level', 'SWE', 'snow depth', 'distance') AND type = 'continuous'")
  if (nrow(new_locations) > 0){ #if TRUE, some new station or data type for an existing station has been added to the locations table
    print("New station(s) detected in locations table.")
    locations_check_before <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name = 'FAILED' AND parameter IN ('level', 'flow', 'distance', 'SWE', 'snow depth') AND type = 'continuous'")
    #find the new station
    for (i in 1:nrow(new_locations)){
      print(paste0("Attempting to add location ", new_locations$location[i], " for parameter ", new_locations$parameter[i], " and type continuous. Locations table as well as measurement tables will be populated if successful."))
      tryCatch({
        if (new_locations$parameter[i] == "SWE" & aquarius){ #only option is an aquarius station
          data <- WRBtools::aq_download(loc_id = new_locations$location[i], ts_name = SWE, server = server)
          name <- data$metadata[1,2]
          #add new information to the realtime table
          ts <- data.frame("location" = new_locations$location[i], "parameter" = "SWE", "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = "mm SWE", "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime_UTC = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'meteorology' WHERE location = '", new_locations$location[i], "' AND parameter = 'SWE' AND type = 'continuous'"))

        } else if (new_locations$parameter[i] == "snow depth" & aquarius){ #only option is an aquarius station
          data <- WRBtools::aq_download(loc_id = new_locations$location[i], ts_name = depth, server = server)
          name <- data$metadata[1,2]
          #add new information to the realtime table
          ts <- data.frame("location" = new_locations$location[i], "parameter" = "snow depth", "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = "cm", "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime_UTC = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'meteorology' WHERE location = '", new_locations$location[i], "' AND parameter = 'snow depth' AND type = 'continuous'"))

        } else if (new_locations$parameter[i] == "distance" & aquarius){ #only option is an aquarius station
          data <- WRBtools::aq_download(loc_id = new_locations$location[i], ts_name = distance, server = server)
          name <- data$metadata[1,2]
          #add new information to the realtime table
          ts <- data.frame("location" = new_locations$location[i], "parameter" = "distance", "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = "m", "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
          DBI::dbAppendTable(hydro, "realtime", ts)
          #make the new entry into table locations
          DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime_UTC = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'highways' WHERE location = '", new_locations$location[i], "' AND parameter = 'distance' AND type = 'continuous'"))

        } else if(new_locations$parameter[i] == "flow"){#check for a WSC station first, then an Aquarius station
          WSC_fail <- TRUE
          tryCatch({
            #add new information to the realtime table
            data_realtime <- NULL
            tryCatch({
              token <- suppressMessages(tidyhydat.ws::token_ws())
              data_realtime <- NULL
              data_realtime <- suppressMessages(tidyhydat.ws::realtime_ws(new_locations$location[i], 47, start_date = Sys.Date()-577, token = token))
              data_realtime <- data_realtime[,c(2,4,1)]
              names(data_realtime) <- c("datetime_UTC", "value", "location")
              data_realtime$datetime_UTC <- as.character(data_realtime$datetime_UTC)
              data_realtime$parameter <- "flow"
              data_realtime$approval <- "preliminary"
              data_realtime$units <- "m3/s"
              DBI::dbAppendTable(hydro, "realtime", data_realtime)
              WSC_fail <- FALSE
            }, error = function(e) {
              data_realtime <- NULL
            }
            )

            #Add new information to the historical table if possible
            data_historical <- NULL
            tryCatch({
              data_historical <- tidyhydat::hy_daily_flows(new_locations$location[i])[,-c(3,5)]
              colnames(data_historical) <- c("location", "date", "value")
              data_historical$approval <- "approved"
              data_historical$units <- "m3/s"
              data_historical$parameter <- "flow"
              data_historical$date <- as.character(data_historical$date)
              DBI::dbAppendTable(hydro, "daily", data_historical)
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

            tryCatch({latitude <- tidyhydat::hy_stations(new_locations$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
            if(length(latitude) < 1) {latitude <- NULL}
            tryCatch({longitude <- tidyhydat::hy_stations(new_locations$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
            if(length(longitude) < 1) {longitude <- NULL}
            name <- stringr::str_to_title(tidyhydat::hy_stations(new_locations$location[i])$STATION_NAME)

            DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "', latitude = ", latitude, ", longitude = ", longitude, ", operator = 'WSC', network = 'Canada Yukon Hydrometric Network' WHERE location = '", new_locations$location[i], "' AND parameter = 'flow' AND type = 'continuous'"))

          }, error= function(e) {
          })

          if (WSC_fail & aquarius){ #try for a WRB station
            data <- WRBtools::aq_download(loc_id = new_locations$location[i], ts_name = discharge, server = server)
            name <- data$metadata[1,2]
            #add new information to the realtime table
            ts <- data.frame("location" = new_locations$location[i], "parameter" = "flow", "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = "m3/s", "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
            DBI::dbAppendTable(hydro, "realtime", ts)
            #make the new entry into table locations
            DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime_UTC = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'Small Stream Network' WHERE location = '", new_locations$location[i], "' AND parameter = 'flow' AND type = 'continuous'"))
          }

        } else if (new_locations$parameter[i] == "level") {#check for a WSC station first, then an Aquarius station
          WSC_fail <- TRUE
          tryCatch({
            #add new information to the realtime table if possible
            data_realtime <- NULL
            tryCatch({
              token <- suppressMessages(tidyhydat.ws::token_ws())
              data_realtime <- NULL
              data_realtime <- suppressMessages(tidyhydat.ws::realtime_ws(new_locations$location[i], 46, start_date = Sys.Date()-577, token = token))
              data_realtime <- data_realtime[,c(2,4,1)]
              names(data_realtime) <- c("datetime_UTC", "value", "location")
              data_realtime$datetime_UTC <- as.character(data_realtime$datetime_UTC)
              data_realtime$parameter <- "level"
              data_realtime$approval <- "preliminary"
              data_realtime$units <- "m"
              DBI::dbAppendTable(hydro, "realtime", data_realtime)
              WSC_fail <- FALSE
            }, error = function(e){
              print(paste0("No WSC realtime data for ", new_locations$location[i]))
              data_realtime <- NULL
            }
            )

            #add new information to the historical table if possible
            data_historical <- NULL
            tryCatch({
              data_historical <- tidyhydat::hy_daily_levels(new_locations$location[i])[,-c(3,5)]
              colnames(data_historical) <- c("location", "date", "value")
              data_historical$approval <- "approved"
              data_historical$units <- "m"
              data_historical$parameter <- "level"
              data_historical$date <- as.character(data_historical$date)
              DBI::dbAppendTable(hydro, "daily", data_historical)
              WSC_fail <- FALSE
            }, error = function(e){
              print(paste0("No WSC historical data for ", new_locations$location[i]))
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

            tryCatch({latitude <- tidyhydat::hy_stations(new_locations$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
            if(length(latitude) < 1) {latitude <- NULL}
            tryCatch({longitude <- tidyhydat::hy_stations(new_locations$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
            if(length(longitude) < 1) {longitude <- NULL}
            name <- stringr::str_to_title(tidyhydat::hy_stations(new_locations$location[i])$STATION_NAME)

            DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "', latitude = ", latitude, ", longitude = ", longitude, ", operator = 'WSC', network = 'Canada Yukon Hydrometric Network' WHERE location = '", new_locations$location[i], "' AND parameter = 'level' AND type = 'continuous'"))

          }, error= function(e) {
          })

          if (WSC_fail & aquarius){ #try for a WRB station
            data <- WRBtools::aq_download(loc_id = new_locations$location[i], ts_name = stage, server = server)
            name <- data$metadata[1,2]
            #add new information to the realtime table
            ts <- data.frame("location" = new_locations$location[i], "parameter" = "level", "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = "m", "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
            DBI::dbAppendTable(hydro, "realtime", ts)
            #make the new entry into table locations
            DBI::dbExecute(hydro, paste0("UPDATE locations SET name = '", name, "', start_datetime_UTC = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime_UTC = '", as.character(max(data$timeseries$timestamp_UTC)),"', latitude = ", data$metadata$value[5], ", longitude = ", data$metadata$value[6], ", operator = 'WRB', network = 'Small Stream Network' WHERE location = '", new_locations$location[i], "' AND parameter = 'level' AND type = 'continuous'"))
          }
        }
        print(paste0("Successfully added station ", new_locations$location[i], " for parameter ", new_locations$parameter[i], " and type continuous"))
      }, error = function(e) {
        DBI::dbExecute(hydro, paste0("UPDATE locations SET name = 'FAILED' WHERE location = '", new_locations$location[i], "' AND parameter = '", new_locations$parameter[i], "' AND type = 'continuous'"))
        print(paste0("Failed to retrieve data from location ", new_locations$location[i], " for parameter ", new_locations$parameter[i], ". The location was flagged as 'FAILED' in the locations table, clear this flag to try again."))
      })
    }
    #End of for loop that works on every new station
    locations_check_after <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name = 'FAILED' AND parameter IN ('level', 'flow', 'distance', 'SWE', 'snow depth') AND type = 'continuous'")
    new_failed <- nrow(locations_check_after) - nrow(locations_check_before)
    if (new_failed < nrow(new_locations)){
      new_stns <- TRUE
    }
  } #End of if loop to deal with new stations

  if (new_stns){
    new_watersheds <- DBI::dbGetQuery(hydro, "SELECT location FROM locations WHERE operator = 'WSC' AND name IS NOT 'FAILED'")$location
    tryCatch({
      getWatersheds(locations = "WSC", path = path) #Add a watershed polygon to the polygons table if possible
      print("Added watershed polygons for new stations and replaced existing polygons.")
    }, error = function(e) {
      print("Failed to add new waterwheds after adding new locations. You could troubleshoot using function getWatersheds in isolation.")
    })
    }

  print("Checking datum tables...")
  ### Now deal with datums if hydat is updated or if stations were added, or if entries are missing
  datums <- DBI::dbGetQuery(hydro, "SELECT location FROM datum_conversions") #pull the existing datums
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name IS NOT 'FAILED' AND type = 'continuous'") #refresh of locations in case any where added
  missing_datums <- setdiff(unique(locations$location), datums$location)
  if (length(missing_datums) > 1) missing_datums <- TRUE else missing_datums <- FALSE

  #Update datum table
  if (new_hydat){
    hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
    on.exit(DBI::dbDisconnect(hydat), add = TRUE)
    DBI::dbExecute(hydat, "PRAGMA busy_timeout=10000")
    datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
    names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
    RSQLite::dbWriteTable(hydro, "datum_list", datum_list, overwrite = TRUE)
    print("Table datum_list was updated to reflect new copy of HYDAT.")
  }
  #Update datum conversions
  if (new_hydat | new_stns | missing_datums){
    hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
    on.exit(DBI::dbDisconnect(hydat), add = TRUE)
    datum_conversions <- data.frame()
    locations_WSC <- dplyr::filter(locations, operator == "WSC")
    locations_WRB <- dplyr::filter(locations, operator == "WRB")
    locations_WRB <- locations_WRB[!(locations_WRB$location %in% locations_WSC$locations) , ] #There are rare instances where a WSC location and infrastructure is used to report information for the WRB network. This selection avoids having two locations with different datums, one from WSC and one from Aquarius.
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
    if (nrow(locations_WRB) > 0 & aquarius){
      all_datums <- data.frame(location = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA)
      for (i in 1:length(unique(locations_WRB$location))){
        #find a corresponding entry in table locations to pick a parameter
        parameter <- locations_WRB[locations_WRB$location == unique(locations_WRB$location)[i],]$parameter[1]

        #TODO: ts_name should be a little function of its own. Maybe?
        #ts_name <- aq_ts(parameter)
        ts_name <- if (parameter == "SWE") SWE else if (parameter == "snow depth") depth else if (parameter == "flow") discharge else if (parameter == "level") stage else if (parameter == "distance") distance
        conversion <- WRBtools::aq_download(loc_id = unique(locations_WRB$location)[i], ts_name = ts_name, start = Sys.Date()-1, server = server)$metadata
        conversion <- conversion[7,2]
        all_datums[i, ] <- c(unique(locations_WRB$location)[i], datum_id_from = 10, datum_id_to = 110, conversion_m = conversion, current = TRUE)
      }
      datum_conversions <- rbind(datum_conversions, all_datums)
    }
    RSQLite::dbWriteTable(hydro, "datum_conversions", datum_conversions, overwrite=TRUE)
    print("Table datum_conversions was updated because of either a new copy of HYDAT, addition of new stations, or detection of datums missing from a/some stations.")
  }

  ### Calculate new daily means and stats from realtime data where necessary
  #Get list of locations again in case it's changed.
  print("Calculating daily means and statistics...")
  stat_start <- Sys.time()
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name IS NOT 'FAILED' AND type = 'continuous'")
  needs_calc <- locations[is.na(locations$last_daily_calculation_UTC) , ] #All of these need a new calculation.
  has_last_new_data <- locations[!is.na(locations$last_new_data_UTC) & !is.na(locations$last_daily_calculation_UTC) , ] #only a subset of these need new calculation. Those that have a calculation and don't have an entry for new data don't need calculations.
  if (nrow(has_last_new_data) > 0){ #Take subset of has_last_new_data where the last calculation was before new data being added.
    needs_new_calc <- has_last_new_data[(as.POSIXct(has_last_new_data$last_new_data_UTC)) > as.POSIXct(has_last_new_data$last_daily_calculation_UTC) , ]
    needs_calc <- rbind(needs_calc, needs_new_calc)
  }
  calculate_stats(locations = needs_calc)

  stats_diff <- Sys.time() - stat_start
  total_diff <- Sys.time() - function_start

  print(paste0("Daily means and statistics calculated in ", round(stats_diff[[1]], 2), " ", units(stats_diff)))

  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_daily'"))

  print(paste0("Total elapsed time for hydro_update_daily: ", round(total_diff[[1]], 2), " ", units(total_diff), ". End of function."))

} #End of function
