#' Daily update of hydro database
#'
#' @description
#'`r lifecycle::badge("stable")`
#'
#' This function is intended to be run on a daily or near-daily basis to ensure data integrity. Pulls in new data, calculates statistics where necessary, and performs cross-checks on several tables (see details for more information).
#'
#' @details
#'Calls several functions in sequence: [getNewRealtime()] to pull in new data into the measurements_continuous table, [getNewDiscrete()] to pull in new data to the measurements_discrete table, [update_hydat()] to check for a new HYDAT database version (hydrometric data from the WSC) and incorporate daily means which differ from those already calculated in the dabatabase, [update_hydat_datums()] to update the datums table with any new datums present in HYDAT, and [calculate_stats()] to calculate new statistics where necessary.
#'
#' Note that new timeseries should be added using function [add_timeseries()].
#'
#' Any timeseries labelled as 'WRBtools::aq_download()' in the source_fx column in the timeseries table will need your Aquarius username, password, and server URL present in your .Renviron profile, or those three parameters entered in the column source_fx_args: see [WRBtools::aq_download()] for more information about that function, and [add_ts_template()] for details on how to format your parameters.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#'
#' @return The database is updated in-place, and diagnostic messages are printed to the console.
#' @export

#TODO: snow_db_path should instead be a path or connection identifiers living in the .Renviron file.
hydro_update_daily <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all")

{
  function_start <- Sys.time()

  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")

  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, end_datetime, type FROM timeseries WHERE category = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, end_datetime, type FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
    if (length(timeseries_id) != nrow(all_timeseries)){
      fail <- timeseries_id[!(timeseries_id %in% all_timeseries$timeseries_id)]
      ifelse ((length(fail) == 1),
      warning("Could not find one of the timeseries_ids that you specified: ID ", fail, " is missing from the database."),
      warning("Could not find some of the timeseries_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
  }

  #Ensure that existing realtime data is up-to-date from WSC and Aquarius
  message("Getting realtime information up to date with getNewRealtime...")
  rt_start <- Sys.time()
  getNewRealtime(con)
  rt_duration <- Sys.time() - rt_start
  message(paste0("getNewRealtime executed in ", round(rt_duration[[1]], 2), " ", units(rt_duration), "."))


#   # Get updated snow course measurements if in season, only if the table exists
#   message("Getting discrete data up to date with getNewDiscrete...")
#   tables <- DBI::dbListTables(con)
#   if ("discrete" %in% tables) { #Doesn't run if not there!
#     if ((1 < lubridate::month(Sys.Date())) & (lubridate::month(Sys.Date()) < 7)){ #only from Feb to June inclusively
#       tryCatch({
#         print("Looking for new discrete measurements...")
#         if (lubridate::day(Sys.Date()) %in% c(1, 15)){ #overwrite twice a month to capture any changes
#           getSnowCourse(hydro_db_path = path, snow_db_path = snow_db_path, inactive = FALSE, overwrite = TRUE)
#         } else {
#           getSnowCourse(hydro_db_path = path, snow_db_path = snow_db_path, inactive = FALSE, overwrite = FALSE)
#         }
#       }, error = function(e) {
#         print("Failed to get new snow survey measurements.")
#       })
#     }
#   }

  #Check for a new version of HYDAT, update timeseries in the database if needed.
  message("Checking for new HYDAT database...")
  new_hydat <- update_hydat(con = con) #This function is run for flow and level for each station, even if one of the two is not currently in the HYDAT database. This allows for new data streams to be incorporated seamlessly, either because HYDAT covers a station already reporting but only in realtime or because a flow/level only station is reporting the other param.

  #TODO: cross-check polygons against the flow and level stations that should have polygons. Try to calculate or get them, alert user if not possible.


  print("Checking datum tables...")
  ### Now deal with datums if hydat is updated or if stations were added, or if entries are missing
  datums <- DBI::dbGetQuery(con, "SELECT location FROM datum_conversions") #pull the existing datums
  timeseries <- DBI::dbGetQuery(con, "SELECT * FROM timeseries WHERE category = 'continuous'") #refresh of timeseries in case any where added
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
    RSQLite::dbWriteTable(con, "datum_list", datum_list, overwrite = TRUE)
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
        ts_name <- aq_names[aq_names$parameter == parameter, "remote_param_name"]
        conversion <- WRBtools::aq_download(loc_id = unique(timeseries_WRB$location)[i], ts_name = ts_name, start = Sys.Date()-1)$metadata
        conversion <- conversion[7,2]
        all_datums[i, ] <- c(unique(timeseries_WRB$location)[i], datum_id_from = 10, datum_id_to = 110, conversion_m = conversion, current = TRUE)
      }
      datum_conversions <- rbind(datum_conversions, all_datums)
    }
    for (i in 1:nrow(datum_conversions)){
      DBI::dbExecute(con, paste0("INSERT OR IGNORE INTO datum_conversions (location, datum_id_from, datum_id_to, conversion_m, current) VALUES ('", datum_conversions$location[i], "', '", datum_conversions$datum_id_from[i], "', '", datum_conversions$datum_id_to[i], "', '", datum_conversions$conversion_m[i], "', '", datum_conversions$current[i], "')"))
      DBI::dbExecute(con, paste0("UPDATE datum_conversions SET current = '", datum_conversions$current[i], "' WHERE location = '", datum_conversions$location[i], "' AND datum_id_from = '", datum_conversions$datum_id_from[i], "' AND datum_id_to = '", datum_conversions$datum_id_to[i], "'"))
    }
    print("Table datum_conversions was updated because of either a new copy of HYDAT, addition of new stations, or detection of datums missing from a/some stations.")
  }

  ### Calculate new daily means and stats from realtime data where necessary
  #Get list of timeseries again in case it's changed.
  print("Calculating daily means and statistics...")
  stat_start <- Sys.time()
  timeseries <- DBI::dbGetQuery(con, "SELECT * FROM timeseries WHERE category = 'continuous'")
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

  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_daily'"))

  print(paste0("Total elapsed time for hydro_update_daily: ", round(total_diff[[1]], 2), " ", units(total_diff), ". End of function."))

} #End of function
