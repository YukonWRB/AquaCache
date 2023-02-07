#' Daily update of hydro database
#'
#' Daily update of hydro database, with multiple aims: 1. Incorporation of new realtime information; 2. Any station and parameter added to table 'timeseries' is added to its relevant table, and table 'timeseries' is filled out; 3. Updating of datum tables and related data if a new HYDAT database version exists or if new stations are added; 4. Calculation of daily means from realtime data and addition to relevant daily tables; 5. Calculation of daily statistics for new days since last run AND/OR for all days that may have been modified with a HYDAT update. In addition, if tables already exist for watershed polygons or discrete snow survey measurements, these will be populated upon addition of new WSC timeseries or, for snow surveys, updated twice a month during the winter season with new data.
#'
#' The function checks for an existing HYDAT database, and will download it if it is missing or can be updated. At the same time any affected daily timeseries are recalculated (using HYDAT daily means and calculated means) starting from the first day where the new HYDAT information diverges from the existing daily means.
#'
#' Calculating daily statistics for February 29 is complicated: due to a paucity of data, this day's statistics are liable to be very mismatched from those of the preceding and succeeding days if calculated based only on Feb 29 data. Consequently, statistics for these days are computed by averaging those of Feb 28 and March 1, ensuring a smooth line when graphing mean/min/max/quantile parameters. This necessitates waiting for complete March 1st data, so Feb 29 means and stats will be delayed until March 2nd.
#'
#' Timeseries that have an identical location name in WSC real-time/historical data and Aquarius will only pull from WSC information. For initial setup incorporating mirrored stations in Aquarius, see function 'initial_WSC'.
#'
#' Note that this function calls hydro_update_hourly to update the realtime tables; stations that were added to the table 'timeseries' since the last run are initialized using a separate process.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param aquarius TRUE if you are fetching new realtime data from Aquarius. FALSE will only populate with WSC station data. Any newly added location will try to pull from Aquarius regardless of this parameter.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#' @param snow_db_path The path to the snow survey database.
#'
#' @return The database is updated in-place.
#' @import tidyhydat.ws
#' @export
#'

hydro_update_daily <- function(path, aquarius = TRUE, server = "https://yukon.aquaticinformatics.net/AQUARIUS", snow_db_path = "//carver/infosys/Snow/DB/SnowDB.mdb")

{
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

  aq_names <- DBI::dbGetQuery(hydro, "SELECT parameter, value FROM settings WHERE application  = 'aquarius'")

  #Ensure that existing realtime data is up-to-date from WSC and Aquarius
  print("Getting realtime information up to date with hydro_update_hourly...")
  hourly_start <- Sys.time()
  hydro_update_hourly(path = path, aquarius = aquarius, server = server)
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
  timeseries_WSC <- DBI::dbGetQuery(hydro, "SELECT DISTINCT location FROM timeseries WHERE operator = 'WSC'")
  new_hydat <- update_hydat(timeseries = timeseries_WSC$location, path = path, force_update = FALSE) #This function is run for flow and level for each station, even if one is not currently in the HYDAT database. This allows for new data streams to be incorporated seamlessly, either because HYDAT covers a station already reporting or because a flow/level only station is reporting the other param.


  #TODO: This next step, checking for new entries, needs to be simplified. In particular it needs to work with yet-to-be defined parameters, like temperature, precip, etc.
  print("Checking tables to see if there are new entries of type 'continuous'...")
  new_stns <- FALSE
  new_timeseries <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE start_datetime_UTC IS NULL AND type = 'continuous'")
  if (nrow(new_timeseries) > 0){ #if TRUE, some new station or data type for an existing station has been added to the timeseries table
    print("New station(s) detected in timeseries table.")
    timeseries_check_before <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'")
    #find the new station
    for (i in 1:nrow(new_timeseries)){
      loc <- new_timeseries$location[i]
      parameter <- new_timeseries$parameter[i]
      units <- new_timeseries$units[i]
      network <- new_timeseries$network[i]
      print(paste0("Attempting to add location ", loc, " for parameter ", parameter, " and type continuous. Timeseries table, locations table, and measurement tables will be populated if successful."))
      tryCatch({
        WSC_fail <- TRUE
        if (parameter %in% c("level", "flow")){
          tryCatch({ #Try for a WSC station first
            #add new information to the realtime table
            data_realtime <- NULL
            tryCatch({
              token <- suppressMessages(tidyhydat.ws::token_ws())
              data_realtime <- NULL
              if (parameter == "flow"){
                data_realtime <- suppressMessages(tidyhydat.ws::realtime_ws(new_timeseries$location[i], 47, start_date = Sys.Date()-577, token = token))
              } else if (parameter == "level"){
                data_realtime <- suppressMessages(tidyhydat.ws::realtime_ws(new_timeseries$location[i], 46, start_date = Sys.Date()-577, token = token))
              }
              data_realtime <- data_realtime[,c(2,4,1)]
              names(data_realtime) <- c("datetime_UTC", "value", "location")
              data_realtime$datetime_UTC <- as.character(data_realtime$datetime_UTC)
              data_realtime$parameter <- parameter
              data_realtime$approval <- "preliminary"
              DBI::dbAppendTable(hydro, "realtime", data_realtime)
              WSC_fail <- FALSE
            }, error = function(e) {
              data_realtime <- NULL
            }
            )

            #Add new information to the historical table if possible
            data_historical <- NULL
            tryCatch({
              if (parameter == "flow"){
                data_historical <- tidyhydat::hy_daily_flows(new_timeseries$location[i])[,-c(3,5)]
              } else if (parameter == "level"){
                data_historical <- tidyhydat::hy_daily_levels(new_timeseries$location[i])[,-c(3,5)]
              }
              colnames(data_historical) <- c("location", "date", "value")
              data_historical$approval <- "approved"
              data_historical$parameter <- parameter
              data_historical$date <- as.character(data_historical$date)
              DBI::dbAppendTable(hydro, "daily", data_historical)
              WSC_fail <- FALSE
            }, error = function(e){
              data_historical <- NULL
            }
            )

            #make the new entry into the timeseries table if possible
            if (!is.null(data_realtime) & !is.null(data_historical)){
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

              tryCatch({latitude <- tidyhydat::hy_stations(new_timeseries$location[i])$LATITUDE}, error = function(e) {latitude <- NULL})
              if(length(latitude) < 1) {latitude <- NULL}
              tryCatch({longitude <- tidyhydat::hy_stations(new_timeseries$location[i])$LONGITUDE}, error = function(e) {longitude <- NULL})
              if(length(longitude) < 1) {longitude <- NULL}
              name <- stringr::str_to_title(tidyhydat::hy_stations(new_timeseries$location[i])$STATION_NAME)

              if (!is.null(data_realtime)){
                DBI::dbExecute(hydro, paste0("UPDATE timeseries SET start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "', last_new_data_UTC = '", .POSIXct(Sys.time(), "UTC"), "', operator = 'WSC', network = 'Canada Yukon Hydrometric Network' WHERE location = '", new_timeseries$location[i], "' AND parameter = '", parameter, "' AND type = 'continuous'"))
              } else { #Set last_new_data so calculate_stats doesn't include it in calculations later on
                DBI::dbExecute(hydro, paste0("UPDATE timeseries SET start_datetime_UTC = '", start_datetime, "', end_datetime_UTC = '", end_datetime, "', last_new_data_UTC = '", end_datetime, "', last_daily_calculation_UTC = '", end_datetime, "', operator = 'WSC', network = 'Canada Yukon Hydrometric Network' WHERE location = '", new_timeseries$location[i], "' AND parameter = '", parameter, "' AND type = 'continuous'"))
              }
              DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO locations (location, name, latitude, longitude) VALUES ('", new_timeseries$location[i], "', '", name, "', '", latitude, "', '", longitude, "')"))
            }
          }, error= function(e) {
          })
        }

        if (WSC_fail | !(parameter %in% c("level", "flow"))){ #try for a WRB station
          tryCatch({
            ts_name <- aq_names[aq_names$parameter == new_timeseries$parameter[i] , 2]
            data <- WRBtools::aq_download(loc_id = new_timeseries$location[i], ts_name = ts_name, server = server)
            name <- data$metadata[1,2]
            #add new information to the realtime table
            ts <- data.frame("location" = loc, "parameter" = parameter, "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
            DBI::dbAppendTable(hydro, "realtime", ts)
            #make the new entry into table timeseries
            DBI::dbExecute(hydro, paste0("UPDATE timeseries SET start_datetime_UTC = '", as.character(min(data$timeseries$timestamp_UTC)),"', end_datetime_UTC = '", as.character(max(data$timeseries$timestamp_UTC)),"', last_new_data_UTC = '", .POSIXct(Sys.time(), "UTC"), "', operator = 'WRB', network = '", network, "' WHERE location = '", loc, "' AND parameter = '", parameter, "' AND type = 'continuous'"))
            DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO locations (location, name, latitude, longitude) VALUES ('", new_timeseries$location[i], "', '", name, "', '", data$metadata$value[5], "', '", data$metadata$value[6], "')"))
          }, error = function(e) {
            print(paste0("Failed to retrieve data from location ", loc, " for parameter ", parameter, " and from Aquarius. The location type was flagged as 'FAILED' in the timeseries table, clear this flag to try again. You may also want to check the timeseries parameter and label in Aquarius."))
          })
        }

        print(paste0("Successfully added station ", loc, " for parameter ", parameter, " and type continuous"))
      }, error = function(e) {
        DBI::dbExecute(hydro, paste0("UPDATE timeseries SET type = 'FAILED' WHERE location = '", new_timeseries$location[i], "' AND parameter = '", parameter, "' AND type = 'continuous'"))
        print(paste0("Failed to retrieve data from location ", loc, " for parameter ", parameter, ". The location type was flagged as 'FAILED' in the timeseries table, clear this flag to try again."))
      })
    }#End of for loop that works on every new station

    timeseries_check_after <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'")
    new_failed <- nrow(timeseries_check_after) - nrow(timeseries_check_before)
    if (new_failed < nrow(new_timeseries)){
      new_stns <- TRUE
    }
  } #End of if loop to deal with new stations

  #TODO: only get new polygons for locations what have just been added.
  #TODO: Add polygons for WRB stations here too, though these will have to be created first.
  if (new_stns){
    tryCatch({
      getWatersheds(locations = "WSC", path = path) #Add a watershed polygon to the polygons table if possible
      print("Added watershed polygons for new stations and replaced existing polygons.")
    }, error = function(e) {
      print("Failed to add new watersheds after adding new timeseries. You could troubleshoot using function getWatersheds in isolation.")
    })
  }

  print("Checking datum tables...")
  ### Now deal with datums if hydat is updated or if stations were added, or if entries are missing
  datums <- DBI::dbGetQuery(hydro, "SELECT location FROM datum_conversions") #pull the existing datums
  timeseries <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'") #refresh of timeseries in case any where added
  missing_datums <- setdiff(unique(timeseries$location), datums$location)
  if (length(missing_datums) > 1) missing_datums <- TRUE else missing_datums <- FALSE

  #Update datum table
  hydat_path <- tidyhydat::hy_downloaded_db()
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
    timeseries_WSC <- dplyr::filter(timeseries, operator == "WSC")
    timeseries_WRB <- dplyr::filter(timeseries, operator == "WRB")
    timeseries_WRB <- timeseries_WRB[!(timeseries_WRB$location %in% timeseries_WSC$location) , ] #There are rare instances where a WSC location and infrastructure is used to report information for the WRB network. This selection avoids having two locations with different datums, one from WSC and one from Aquarius.
    if (nrow(timeseries_WSC) > 0){
      all_datums <- dplyr::filter(DBI::dbReadTable(hydat, "STN_DATUM_CONVERSION"), STATION_NUMBER %in% timeseries_WSC$location)
      all_datums$current <- NA
      names(all_datums) <- c("location", "datum_id_from", "datum_id_to", "conversion_m", "current")
      for (i in unique(timeseries_WSC$location)){
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
    if (nrow(timeseries_WRB) > 0 & aquarius){
      all_datums <- data.frame(location = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA)
      for (i in 1:length(unique(timeseries_WRB$location))){
        #find a corresponding entry in table timeseries to pick a parameter
        parameter <- timeseries_WRB[timeseries_WRB$location == unique(timeseries_WRB$location)[i],]$parameter[1]
        ts_name <- aq_names[aq_names$parameter == parameter, 2]
        conversion <- WRBtools::aq_download(loc_id = unique(timeseries_WRB$location)[i], ts_name = ts_name, start = Sys.Date()-1, server = server)$metadata
        conversion <- conversion[7,2]
        all_datums[i, ] <- c(unique(timeseries_WRB$location)[i], datum_id_from = 10, datum_id_to = 110, conversion_m = conversion, current = TRUE)
      }
      datum_conversions <- rbind(datum_conversions, all_datums)
    }
    for (i in 1:nrow(datum_conversions)){
      DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO datum_conversions (location, datum_id_from, datum_id_to, conversion_m, current) VALUES ('", datum_conversions$location[i], "', '", datum_conversions$datum_id_from[i], "', '", datum_conversions$datum_id_to[i], "', '", datum_conversions$conversion_m[i], "', '", datum_conversions$current[i], "')"))
      DBI::dbExecute(hydro, paste0("UPDATE datum_conversions SET datum_id_from = '", datum_conversions$datum_id_from[i], "', datum_id_to = '", datum_conversions$datum_id_to[i], "', conversion_m = '", datum_conversions$conversion_m[i], "', current = '", datum_conversions$current[i], "' WHERE location = '", datum_conversions$location[i], "'"))
    }
    print("Table datum_conversions was updated because of either a new copy of HYDAT, addition of new stations, or detection of datums missing from a/some stations.")
  }

  ### Calculate new daily means and stats from realtime data where necessary
  #Get list of timeseries again in case it's changed.
  print("Calculating daily means and statistics...")
  stat_start <- Sys.time()
  timeseries <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'")
  needs_calc <- timeseries[is.na(timeseries$last_daily_calculation_UTC) , ] #All of these need a new calculation.
  has_last_new_data <- timeseries[!is.na(timeseries$last_new_data_UTC) & !is.na(timeseries$last_daily_calculation_UTC) , ] #only a subset of these need new calculation. Those that have a calculation and don't have an entry for new data don't need calculations.
  if (nrow(has_last_new_data) > 0){ #Take subset of has_last_new_data where the last calculation was before new data being added.
    needs_new_calc <- has_last_new_data[(as.POSIXct(has_last_new_data$last_new_data_UTC)) > as.POSIXct(has_last_new_data$last_daily_calculation_UTC) , ]
    needs_calc <- rbind(needs_calc, needs_new_calc)
  }

  calculate_stats(timeseries = needs_calc, path = path)

  stats_diff <- Sys.time() - stat_start
  total_diff <- Sys.time() - function_start

  print(paste0("Daily means and statistics calculated in ", round(stats_diff[[1]], 2), " ", units(stats_diff)))

  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_daily'"))

  print(paste0("Total elapsed time for hydro_update_daily: ", round(total_diff[[1]], 2), " ", units(total_diff), ". End of function."))

} #End of function
