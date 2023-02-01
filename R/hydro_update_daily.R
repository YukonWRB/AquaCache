#' Daily update of hydro database
#'
#' Daily update of hydro database, with multiple aims: 1. Any station and parameter added to table 'locations' is added to its relevant table, and table 'locations' is filled out; 2. Updating of datum tables if a new HYDAT database version exists or if new stations are added; 3. Calculation of daily means from realtime data and addition to relevant daily tables; 4. Calculation of daily statistics for new days since last run AND/OR for all days that may have been modified with a HYDAT update.
#'
#' The function checks for an existing HYDAT database, and will download it if it is missing or can be updated.
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

hydro_update_daily <- function(path, aquarius = TRUE, stage = "Stage.Corrected", discharge = "Discharge.Master", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", distance = "Distance.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS", snow_db_path = "X:/Snow/DB/SnowDB.mdb")

{

  #TODO: This entire function needs to be adapted to work with to-be-defined parameters, like temperature or precip. right now lots of steps rely on hard-coded parameter names, but this could be more flexible. Two options: get new parameter names from the 'locations' table when new entries are made, or have a single list of defined parameters and associated units here.
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

  print("Checking the local HYDAT database...")
  #TODO: checking and updating hydat should probably be made into a WRBtools function...
  #Check hydat version, update if needed.
  tryCatch({hydat_path <- tidyhydat::hy_downloaded_db() #Attempts to get the hydat path, in case it's downloaded already.
  }, error = function(e) {hydat_path <- NULL})

  new_hydat <- FALSE
  if (!is.null(hydat_path) & exists("local_hydat")){ #If hydat already exists, compare version numbers
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    remote_hydat <- tidyhydat::hy_remote()
    if (local_hydat != remote_hydat){ #if remote version is more recent, download new version
      try(tidyhydat::download_hydat(ask=FALSE))
      hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
      local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date) #check the HYDAT version again just in case. It can fail to update without actually creating an error and stopping.
      local_hydat <- gsub("-", "", as.character(local_hydat))
      if (local_hydat == remote_hydat){
        new_hydat <- TRUE
        print("The local WSC HYDAT database was updated.")
      } else {
        print("Failed to update the local HYDAT database. There is probably an active connection to the database preventing an overwrite, this function will try again at next run.")
      }
    }
  } else if (is.null(hydat_path) | !exists("local_hydat")) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask=FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
    new_hydat <- TRUE
    local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
    local_hydat <- gsub("-", "", as.character(local_hydat))
    print("A local copy of the WSC HYDAT database was installed.")
  }

  #Check if HYDAT has been updated outside of this function.
  local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
  local_hydat <- gsub("-", "", as.character(local_hydat))
  DB_hydat <- as.character(DBI::dbGetQuery(hydro, "SELECT value FROM internal_status WHERE event = 'HYDAT_version'"))
  if (DB_hydat != local_hydat){
    new_hydat <- TRUE
  }

  # Get updated snow course measurements if in season, only if the table exists
  if ((1 < lubridate::month(Sys.Date())) & (lubridate::month(Sys.Date()) < 7)){ #only from Feb to June inclusively
    tables <- DBI::dbListTables(hydro)
    if ("discrete" %in% tables) {#Doesn't run if not there!
      print("Looking for new discrete measurements...")
      if (lubridate::day(Sys.Date()) %in% c(1, 15)){ #overwrite twice a month to capture any changes
        getSnowCourse(hydro_db_path = path, snow_db_path = snow_db_path, inactive = FALSE, overwrite = TRUE)
      } else {
        getSnowCourse(hydro_db_path = path, snow_db_path = snow_db_path, inactive = FALSE, overwrite = FALSE)
      }
    }
  }

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
            name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)

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

      #TODO: line below currently runs on each and every location starting with 01-11, even if they're WRB stations. Needs to run only if operator == WSC for this to be viable.
      #try(getWatersheds(locations = new_locations$location[i], path = path)) #Add a watershed polygon to the polygons table if possible

    } #End of for loop that works on every new station
    locations_check_after <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name = 'FAILED' AND parameter IN ('level', 'flow', 'distance', 'SWE', 'snow depth') AND type = 'continuous'")
    new_failed <- nrow(locations_check_after) - nrow(locations_check_before)
    if (new_failed < nrow(new_locations)){
      new_stns <- TRUE
    }
  } #End of if loop to deal with new stations


  print("Checking datum tables...")
  ### Now deal with datums if hydat is updated or if stations were added, or if entries are missing
  datums <- DBI::dbGetQuery(hydro, "SELECT location FROM datum_conversions") #pull the existing datums
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name IS NOT 'FAILED' AND parameter IN ('level', 'flow', 'distance', 'SWE', 'snow depth') AND type = 'continuous'") #refresh of locations in case any where added
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


  #TODO: This next portion and force_update_hydat should probably be one and the same. Also, stats should be calculated for each station just before every DELETE/INSERT to minimize time with blank tables.

  #Now update historical HYDAT timeseries if new_hydat == TRUE. At the same time check for new flow or level entries at existing stations.
  if (new_hydat){
    print("Updating historical information in HYDAT due to new database...")
    for (i in unique(locations_WSC$location)) { #This loop is run for flow and level for each station, even if one is not currently in the HYDAT database. This allows for new data streams to be incorporated seamlessly, either because HYDAT covers a station already reporting or because a flow/level only station is reportion the other param.
      #Deal with flows
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
          while (!mismatch){
            for (j in 1:nrow(flow_historical)){
              if (!((flow_historical$value[j] == existing$value[j]) & (flow_historical$date[j] == existing$date[j]))){
                new_hydat_start <- flow_historical$date[j]
                mismatch <- TRUE
              }
            }
          }
          if (mismatch){ #only need to append new if mismatch == TRUE, otherwise the TS was not yet updated in HYDAT.
            flow_historical <- flow_historical[flow_historical$date >= new_hydat_start , ]
            delete_from <- min(flow_historical$date)
            DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE date >= '", delete_from, "' AND location = '", i, "' AND parameter = 'flow'")) #Deletes everything after the first HDAT entry that is not in or different from the database.
            DBI::dbAppendTable(hydro, "daily", flow_historical)
            DBI::dbExecute(hydro, paste0("UPDATE locations SET last_daily_calculation = 'NULL' WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous' AND operator = 'WSC'"))
          }
        } else {
          DBI::dbAppendTable(hydro, "daily", flow_historical)
        }

        #check if it already exists in locations table
        ts <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", i, "' AND parameter = 'flow' AND type = 'continuous'"))
        if (nrow(ts) == 0){ #It is a new TS at an existing location: add entry to locations table from scratch
          info <- locations[locations$location == i ,]
          info <- info[1,]
          info$parameter <- "flow"
          info$type <- "continuous"
          info$start_datetime <- paste0(min(flow_historical$date), " 00:00:00")
          info$end_datetime <- paste0(max(flow_historical$date), " 00:00:00")
          tryCatch({latitude <- tidyhydat::hy_stations(i)$LATITUDE}, error = function(e) {latitude <- NULL})
          if(length(latitude) < 1) {latitude <- NULL}
          tryCatch({longitude <- tidyhydat::hy_stations(i)$LONGITUDE}, error = function(e) {longitude <- NULL})
          if(length(longitude) < 1) {longitude <- NULL}
          info$latitude <- latitude
          info$longitude <- longitude
          info$operator <- "WSC"
          info$network <- "Canada Yukon Hydrometric Network"
          info$name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
          DBI::dbAppendTable(hydro, "locations", info)
        } else { #The time-series already exists and the locations table entry exists. Re-input start time in case it changed and end time if it is after entry in realtime table
          start_datetime <- paste0(min(flow_historical$date), " 00:00:00")
          end_datetime_historical <- paste0(max(flow_historical$date), " 00:00:00")
          end_datetime_realtime <- ts$end_datetime
          end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))
          DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "' WHERE location = '", i, "' AND parameter = 'flow' AND type = 'continuous'"))
        }
      }, error = function(e){}
      )
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
          while (!mismatch){
            for (j in 1:nrow(level_historical)){
              if (!((level_historical$value[j] == existing$value[j]) & (level_historical$date[j] == existing$date[j]))){
                new_hydat_start <- level_historical$date[j]
                mismatch <- TRUE
              }
            }
          }
          if (mismatch){ #only need to append new if mismatch == TRUE, otherwise the TS was not yet updated in HYDAT.
            level_historical <- level_historical[level_historical$date <= new_hydat_start , ]
            delete_from <- min(level_historical$date)
            DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE date >= '", delete_from, "' AND location = '", i, "' AND parameter = 'level'"))
            DBI::dbAppendTable(hydro, "daily", level_historical)
            DBI::dbExecute(hydro, paste0("UPDATE locations SET last_daily_calculation = 'NULL' WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous' AND operator = 'WSC'"))
          }
        } else {
          DBI::dbAppendTable(hydro, "daily", level_historical)
        }
        #check if it already exists in locations table
        ts <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous'"))
        if (nrow(ts) == 0){ #It is a new TS at an existing location: add entry to locations table from scratch
          info <- locations[locations$location == i ,]
          info <- info[1,]
          info$parameter <- "level"
          info$type <- "continuous"
          info$start_datetime <- paste0(min(level_historical$date), " 00:00:00")
          info$end_datetime <- paste0(max(level_historical$date), " 00:00:00")
          tryCatch({latitude <- tidyhydat::hy_stations(i)$LATITUDE}, error = function(e) {latitude <- NULL})
          if(length(latitude) < 1) {latitude <- NULL}
          tryCatch({longitude <- tidyhydat::hy_stations(i)$LONGITUDE}, error = function(e) {longitude <- NULL})
          if(length(longitude) < 1) {longitude <- NULL}
          info$latitude <- latitude
          info$longitude <- longitude
          info$operator <- "WSC"
          info$network <- "Canada Yukon Hydrometric Network"
          info$name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
          DBI::dbAppendTable(hydro, "locations", info)
        } else { #The time-series already exists and the locations table entry exists. Re-input start time in case it changed and end time if it is after entry in realtime table
          start_datetime <- paste0(min(level_historical$date), " 00:00:00")
          end_datetime_historical <- paste0(max(level_historical$date), " 00:00:00")
          end_datetime_realtime <- ts$end_datetime
          end_datetime <- max(c(end_datetime_realtime, end_datetime_historical))
          DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "' WHERE location = '", i, "' AND parameter = 'level' AND type = 'continuous'"))
        }
      }, error = function(e){}
      )
    } #End of for loop updating information contained in HYDAT
  } #End of section updating information contained in HYDAT if HYDAT is new


  ### Calculate new daily means from realtime data, followed by stats
  #Get list of locations again in case it's changed.
  print("Calculating daily means and statistics...")
  stat_start <- Sys.time()
  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name IS NOT 'FAILED' AND parameter IN ('level', 'flow', 'distance', 'SWE', 'snow depth') AND type = 'continuous'")
    no_calc <- locations[is.na(locations$last_daily_calculation_UTC) , ] #All of these need a new calculation.
    has_last_new_data <- locations[!is.na(locations$last_new_data_UTC) & !is.na(locations$last_daily_calculation_UTC) , ] #only a subset of these need new calculation. Those that have a calculation and don't have an entry for new data don't need calculations.
    if (nrow(has_last_new_data) > 0){
      needs_new_calc <- has_last_new_data[(as.POSIXct(has_last_new_data$last_new_data_UTC) + 6*60*60) > as.POSIXct(has_last_new_data$last_daily_calculation_UTC) , ]
      no_calc <- rbind(no_calc, needs_new_calc)
    }
    locations <- no_calc

  #calculate daily means for any days without them
  leap_list <- (seq(1800, 2100, by = 4))
  for (i in 1:nrow(locations)){
    loc <- locations$location[i]
    parameter <- locations$parameter[i]
    operator <- locations$operator[i]

    last_day_historic <- DBI::dbGetQuery(hydro, paste0("SELECT MAX(date) FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]
    #TODO: the step below is slow, needs to query a very large table. Can it be done another way?
    earliest_day_realtime <- as.character(as.Date(DBI::dbGetQuery(hydro, paste0("SELECT MIN(datetime_UTC) FROM realtime WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]))
    if (!is.na(last_day_historic) & !is.na(earliest_day_realtime)){
      if (last_day_historic > as.character(as.Date(earliest_day_realtime) + 2)) {
        last_day_historic <- as.character(as.Date(last_day_historic) - 2) #if the two days before last_day_historic are in the realtime data, recalculate last two days in case realtime data hadn't yet come in. This will also wipe the stats for those two days just in case.
      }
    } else if (is.na(last_day_historic) & !is.na(earliest_day_realtime)){
      last_day_historic <- as.character(as.Date(earliest_day_realtime) - 2)
    }

    gap_realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM realtime WHERE parameter = '", parameter, "' AND location = '", loc, "' AND datetime_UTC BETWEEN '", last_day_historic, " 00:00:00' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
    if (nrow(gap_realtime) > 0){
      gap_realtime <- gap_realtime %>%
        dplyr::group_by(lubridate::year(.data$datetime_UTC), lubridate::yday(.data$datetime_UTC)) %>%
        dplyr::summarize(date = mean(lubridate::date(.data$datetime_UTC)),
                         value = mean(.data$value),
                         grade = sort(.data$grade,decreasing=TRUE)[1],
                         approval = sort(.data$approval, decreasing=TRUE)[1],
                         .groups = "drop")
      gap_realtime <- gap_realtime[,c(3:6)]
      names(gap_realtime) <- c("date", "value", "grade", "approval")
      if (min(gap_realtime$date) > last_day_historic){ #Makes a row if there is no data for that day, this way stats will be calculated for that day later.
        gap_realtime <- rbind(gap_realtime, data.frame("date" = last_day_historic, "value" = NA, "grade" = NA, "approval" = NA))
      }
      gap_realtime <- fasstr::fill_missing_dates(gap_realtime, "date", pad_ends = FALSE) #fills any missing dates with NAs, which will let them be filled later on when calculating stats.
      gap_realtime$units <- if (parameter == "level") "m" else if (parameter == "flow") "m3/s" else if (parameter == "SWE") "mm SWE" else if (parameter == "snow depth") "cm" else if (parameter == "distance") "m"
      gap_realtime$location <- loc
      gap_realtime$parameter <- parameter
      gap_realtime$date <- as.character(gap_realtime$date)
      DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE parameter = '", parameter, "' AND date >= '", min(gap_realtime$date), "' AND location = '", loc, "'"))
      DBI::dbAppendTable(hydro, "daily", gap_realtime)
    }
    #TODO: surely there's a way to remove the DELETE and APPEND operations above, and only do it once after stats are calculated?

    # Now calculate stats where they are missing
    all_stats <- DBI::dbGetQuery(hydro, paste0("SELECT date, value FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "'"))
    missing_stats <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "' AND max IS NULL"))
    #TODO: the step above selects rows that get dropped later, and does this each time. How about just working with gap_realtime from above?

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

    #selects only records beginning with the second dayofyear and having values for the second time from all_stats (those for which stats can be calculated). Selects valid rows even if there is no current value, ensuring complete plotting parameters.
    missing_stats <- missing_stats[order(missing_stats[ , "date"]) , ]
    all_stats <- all_stats[order(all_stats[ , "date"]) , ]
    temp <- data.frame()
    for (j in unique(missing_stats$dayofyear)){
      earliest <- all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]$date[2]
      if (!is.na(earliest)){
        missing <- missing_stats[missing_stats$dayofyear == j & missing_stats$date >= earliest , ]
        temp <- rbind(temp, missing)
      }
    }
    missing_stats <- temp

    if (nrow(missing_stats) > 0){
    for (k in 1:nrow(missing_stats)){
      date <- missing_stats$date[k]
      doy <- missing_stats$dayofyear[k]
      current <- missing_stats$value[k]
      past <- all_stats[all_stats$dayofyear == doy & all_stats$date < date , ]$value #Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
      past <- past[!is.na(past)]
      if (length(past) >= 1){
        missing_stats$max[k] <- max(past) #again, NOT including current measurement
        missing_stats$min[k] <- min(past)
        missing_stats$QP90[k] <- stats::quantile(past, 0.90)
        missing_stats$QP75[k] <- stats::quantile(past, 0.75)
        missing_stats$QP50[k] <- stats::quantile(past, 0.50)
        missing_stats$QP25[k] <- stats::quantile(past, 0.25)
        missing_stats$QP10[k] <- stats::quantile(past, 0.10)
        if (length(past) > 1 & !is.na(current)){ #need at least 2 measurements to calculate a percent historic!
          missing_stats$percent_historic_range[k] <- ((current - min(past)) / (max(past) - min(past))) * 100
        }
      }
    }
    missing_stats <- subset(missing_stats, select=-c(dayofyear)) #remove column not in database table

      #Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st. Unfortunately this means that initial setups done on those days will not calculate Feb 29!
      if (nrow(feb_29) > 0 & !(substr(as.character(as.Date(.POSIXct(Sys.time(), "UTC"))), 6, 10) %in% c("02-29", "03-01,", "03-02"))) {
        for (l in 1:nrow(feb_29)){
          date <- as.Date(feb_29$date[l])
          before <- missing_stats[missing_stats$date == date - 1 , ]
          after <- missing_stats[missing_stats$date == date + 1 , ]
          feb_29$percent_historic_range[l] <- mean(c(before$percent_historic_range, after$percent_historic_range))
          feb_29$max[l] <- mean(c(before$max, after$max))
          feb_29$min[l] <- mean(c(before$min, after$min))
          feb_29$QP90[l] <- mean(c(before$QP90, after$QP90))
          feb_29$QP75[l] <- mean(c(before$QP75, after$QP75))
          feb_29$QP50[l] <- mean(c(before$QP50, after$QP50))
          feb_29$QP25[l] <- mean(c(before$QP25, after$QP25))
          feb_29$QP10[l] <- mean(c(before$QP10, after$QP10))
        }
      }
      missing_stats <- rbind(missing_stats, feb_29)

      #TODO: line below needs to become an UPDATE instead
      DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "' AND date BETWEEN '", min(missing_stats$date), "' AND '", max(missing_stats$date), "' AND max IS NULL")) #The AND max IS NULL part prevents deleting entries within the time range that have not been recalculated, as would happen if, say, a Feb 29 is calculated on March 3rd. Without that condition, March 1 and 2 would also be deleted but are not part of missing_stats due to initial selection criteria of missing_stats.
      DBI::dbAppendTable(hydro, "daily", missing_stats)
      DBI::dbExecute(hydro, paste0("UPDATE locations SET last_daily_calculation_UTC = '", .POSIXct(Sys.time(), "UTC"), "' WHERE location= '", loc, "' AND parameter = '", parameter, "' AND operator = '", operator, "' AND type = 'continuous'"))
    }
  } # End of for loop calculating means and stats for each station in locations table

  if (new_hydat){
    DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", local_hydat, "' WHERE event = 'HYDAT_version'")) #new_hydat was set during the HYDAT checks above, but the value in the table is only changed here in case something did not run in the script.
  }

  stats_diff <- Sys.time() - stat_start
  total_diff <- Sys.time() - function_start
  print(paste0("Daily means and statistics calculated in ", round(stats_diff[[1]], 2), " ", units(stats_diff)))
  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_daily'"))
  print(paste0("Total elapsed time for hydro_update_daily: ", round(total_diff[[1]], 2), " ", units(total_diff), ". End of function."))

} #End of function
