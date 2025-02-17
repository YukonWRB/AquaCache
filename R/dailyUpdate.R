#' Daily update of hydro database
#'
#' @description
#'`r lifecycle::badge("stable")`
#'
#' This function is intended to be run on a daily or near-daily basis to ensure data integrity. Pulls in new data, calculates statistics where necessary, and performs cross-checks on several tables (see details for more information).
#'
#' @details
#'Calls several functions in sequence: [getNewContinuous()] to pull in new data into the measurements_continuous table, [getNewDiscrete()] to pull in new data to the measurements_discrete table, [getNewImages()] to get new images in a series, [getNewRasters()] to get new model rasters, [update_hydat()] to check for a new HYDAT database version (hydrometric data from the WSC) and incorporate daily means which differ from those already calculated in the dabatabase, [update_hydat_datums()] to update the datums table with any new datums present in HYDAT, and [calculate_stats()] to calculate new statistics where necessary.
#'
#' Note that new timeseries should be added using function [addACTimeseries()].
#'
#' Any timeseries labelled as downloadAquarius in the source_fx column in the timeseries table will need your Aquarius username, password, and server URL present in your .Renviron profile, or those three parameters entered in the column source_fx_args: see downloadAquarius for more information about that function.
#'
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param sample_series_id The sample_series_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'timeseries', 'images_index', or 'raster_series_index' tables to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#' @param continuous If TRUE, will update continuous data. Default is TRUE.
#' @param discrete If TRUE, will update discrete data. Default is TRUE.
#' @param hydat If TRUE, will check for new HYDAT data and update timeseries in the database if needed. Default is TRUE.
#' @param images If TRUE, will fetch new images. Default is TRUE.
#' @param rasters If TRUE, will fetch new rasters. Default is TRUE.
#'
#' @return The database is updated in-place, and diagnostic messages are printed to the console.
#' @export

dailyUpdate <- function(con = NULL, timeseries_id = "all", sample_series_id = "all", active = 'default', continuous = TRUE, discrete = TRUE, hydat = TRUE, images = TRUE, rasters = TRUE)
{
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  function_start <- Sys.time()
  message(" ")
  message("dailyUpdate start at ", Sys.time())

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  if (timeseries_id[1] == "all") {
    continuous_ts <- DBI::dbGetQuery(con, "SELECT location, timeseries_id, last_daily_calculation, active FROM timeseries WHERE source_fx IS NOT NULL")
  } else {
    continuous_ts <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id, last_daily_calculation, active FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
    if (length(timeseries_id) != nrow(continuous_ts)) {
      fail <- timeseries_id[!(timeseries_id %in% continuous_ts$timeseries_id)]
      ifelse((length(fail) == 1),
              warning("Could not find one of the timeseries_ids that you specified: ID ", fail, " is missing from the database."),
              warning("Could not find some of the timeseries_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
  }
  
  if (active == 'default') {
    continuous_ts <- continuous_ts[continuous_ts$active, ]
  }

  #Get new data ################
  if (continuous) {
    if (nrow(continuous_ts) > 0) {
      message("Getting continuous information up to date with getNewContinuous...")
      tryCatch({
        rt_start <- Sys.time()
        getNewContinuous(con = con, timeseries_id = continuous_ts$timeseries_id)
        rt_duration <- Sys.time() - rt_start
        message("getNewContinuous executed in ", round(rt_duration[[1]], 2), " ", units(rt_duration), ".")
      }, error = function(e) {
        warning("dailyUpdate: error fetching new continuous data. Returned message: ", e$message)
      })
    }
  }

  if (discrete) {
    if (sample_series_id[1] == "all") {
      discrete_ids <- DBI::dbGetQuery(con, "SELECT sample_series_id FROM sample_series WHERE source_fx IS NOT NULL")
    } else {
      all_sample_series <- DBI::dbGetQuery(con, paste0("SELECT sample_series_id FROM sample_series WHERE sample_series_id IN ('", paste(sample_series_id, collapse = "', '"), "')"))
      if (length(sample_series_id) != nrow(all_sample_series)) {
        fail <- sample_series_id[!(sample_series_id %in% all_sample_series$sample_series_id)]
        ifelse((length(fail) == 1),
               warning("Could not find one of the sample_series_ids that you specified: ID ", fail, " is missing from the database."),
               warning("Could not find some of the sample_series_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
        )
      }
    }
    if (nrow(discrete_ids) > 0) {
      message("Getting discrete information up to date with getNewDiscrete...")
      tryCatch({
        disc_start <- Sys.time()
        getNewDiscrete(con = con, sample_series_id = discrete_ids$sample_series_id, location_id = NULL, sub_location_id = NULL)
        disc_duration <- Sys.time() - disc_start
        message("getNewDiscrete executed in ", round(disc_duration[[1]], 2), " ", units(disc_duration), ".")
      }, error = function(e) {
        warning("dailyUpdate: error fetching new discrete data. Returned message: ", e$message)
      })
    }
  }
  
  if (images) {
    message("Getting new images with getNewImages...")
    tryCatch({
      img_start <- Sys.time()
      getNewImages(con = con, active = active)
      img_duration <- Sys.time() - img_start
      message("getNewImages executed in ", round(img_duration[[1]], 2), " ", units(img_duration), ".")
    }, error = function(e) {
      warning("dailyUpdate: error fetching new images. Returned message: ", e$message)
    })
  }
  
  if (rasters) {
    message("Getting new rasters with getNewRasters...")
    tryCatch({
      ras_start <- Sys.time()
      getNewRasters(con = con, active = active)
      ras_duration <- Sys.time() - ras_start
      message("getNewRasters executed in ", round(ras_duration[[1]], 2), " ", units(ras_duration), ".")
    }, error = function(e) {
      warning("dailyUpdate: error fetching new rasters. Returned message: ", e$message)
    })
  }
  
  ### Check for a new version of HYDAT, update timeseries in the database if needed. #####
  if (hydat) {
    message("Checking for new HYDAT database on this computer and determining the version last used for updating timeseries with update_hydat...")
    tryCatch({
      hy_start <- Sys.time()
      suppressMessages({new_hydat <- update_hydat(con = con)}) #This function will run for flow and level for each station, even if one of the two is not currently in the HYDAT database. This allows for new data streams to be incorporated seamlessly, either because HYDAT covers a station already reporting but only in realtime or because a flow/level only station is reporting the other param.
      if (new_hydat) {
        hy_duration <- Sys.time() - hy_start
        message("A new version of HYDAT was detected. Timeseries were updated in ", round(hy_duration[[1]], 2), " ", units(hy_duration), ".")
        # check WSC stations for new datums and check datums table for new entries
        message("Checking if latest version of HYDAT has new datums...")
        update_hydat_datums(con = con)
      } else {
        message("HYDAT database is already up to date")
      }
    }, error = function(e) {
      warning("dailyUpdate: error when checking for new HYDAT database or when updating datums. Returned message: ", e$message)
    })
  }


  ### Calculate new daily means and stats from realtime data where necessary #######
  if (nrow(continuous_ts) > 0) {
    message("Calculating daily means and statistics where necessary...")
    tryCatch({
      stat_start <- Sys.time()
      calc_ts <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, last_daily_calculation, last_new_data FROM timeseries WHERE timeseries_id IN ('", paste(continuous_ts$timeseries_id, collapse = "', '"), "') AND record_rate IN ('1 day', '< 1 day');"))
      needs_calc <- calc_ts[is.na(calc_ts$last_daily_calculation) , ] #All of these need a new calculation.
      has_last_new_data <- calc_ts[!is.na(calc_ts$last_new_data) & !is.na(calc_ts$last_daily_calculation) , ] #only a subset of these need new calculation. Those that have a calculation and don't have an entry for new data don't need calculations.
      if (nrow(has_last_new_data) > 0) { #Take subset of has_last_new_data where the last calculation was before new data being added.
        needs_new_calc <- has_last_new_data[(has_last_new_data$last_new_data) > has_last_new_data$last_daily_calculation , ]
        needs_calc <- rbind(needs_calc, needs_new_calc)
      }
      if (nrow(needs_calc) > 0) {
        calculate_stats(timeseries_id = needs_calc$timeseries_id, con = con)
        stats_diff <- Sys.time() - stat_start
        message("Daily means and statistics calculated in ", round(stats_diff[[1]], 2), " ", units(stats_diff))
      } else {
        message("No daily means and stats to calculate, skipping.")
      }
    }, error = function(e) {
      warning("dailyUpdate: error when trying to calculate new daily means and statistics. Returned message: ", e$message)
    })
  }

  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_daily'"))

  total_diff <- Sys.time() - function_start
  message("Total elapsed time for dailyUpdate: ", round(total_diff[[1]], 2), " ", units(total_diff), ". End of function.")

} #End of function
