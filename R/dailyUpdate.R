#' Daily update of hydro database
#'
#' @description
#'`r lifecycle::badge("stable")`
#'
#' This function is intended to be run on a daily or near-daily basis to ensure data integrity. Pulls in new data, calculates statistics where necessary, and performs cross-checks on several tables (see details for more information).
#'
#' @details
#'Calls several functions in sequence: [getNewContinuous()] to pull in new data into the measurements_continuous table, [getNewDiscrete()] to pull in new data to the measurements_discrete table, [update_hydat()] to check for a new HYDAT database version (hydrometric data from the WSC) and incorporate daily means which differ from those already calculated in the dabatabase, [update_hydat_datums()] to update the datums table with any new datums present in HYDAT, and [calculate_stats()] to calculate new statistics where necessary.
#'
#' Note that new timeseries should be added using function [addHydrometTimeseries()].
#'
#' Any timeseries labelled as getRealtimeAquarius in the source_fx column in the timeseries table will need your Aquarius username, password, and server URL present in your .Renviron profile, or those three parameters entered in the column source_fx_args: see getRealtimeAquarius for more information about that function, and [addHydrometTemplate()] for details on how to format the parameters to pass to [addHydrometTimeseries()].
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#'
#' @return The database is updated in-place, and diagnostic messages are printed to the console.
#' @export

#TODO: snow_db_path should instead be a path or connection identifiers living in the .Renviron file.
dailyUpdate <- function(con = hydrometConnect(silent=TRUE), timeseries_id = "all")

{
  function_start <- Sys.time()

  settings <- DBI::dbGetQuery(con,  "SELECT source_fx, parameter, remote_param_name FROM settings;")

  if (timeseries_id[1] == "all"){
    continuous_ts <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, end_datetime, last_daily_calculation FROM timeseries WHERE category = 'continuous'")
    discrete_ts <- DBI::dbGetQuery(con, "SELECT location, parameter, timeseries_id, source_fx, end_datetime, last_daily_calculation FROM timeseries WHERE category = 'discrete'")

  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, timeseries_id, source_fx, end_datetime, last_daily_calculation, category FROM timeseries WHERE timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "')"))
    if (length(timeseries_id) != nrow(continuous_ts)){
      fail <- timeseries_id[!(timeseries_id %in% continuous_ts$timeseries_id)]
      ifelse ((length(fail) == 1),
      warning("Could not find one of the timeseries_ids that you specified: ID ", fail, " is missing from the database."),
      warning("Could not find some of the timeseries_ids that you specified: IDs ", paste(fail, collapse = ", "), " are missing from the database.")
      )
    }
    continuous_ts <- all_timeseries[all_timeseries$category == "continuous" , ]
    discrete_ts <- all_timeseries[all_timeseries$category == "discrete" , ]
  }

  if (nrow(continuous_ts) > 0){
    message("Getting continuous information up to date with getNewContinuous...")
    rt_start <- Sys.time()
    getNewContinuous(con = con, timeseries_id = continuous_ts$timeseries_id)
    rt_duration <- Sys.time() - rt_start
    message(paste0("getNewContinuous executed in ", round(rt_duration[[1]], 2), " ", units(rt_duration), "."))
  }

  if (nrow(discrete_ts) > 0){
    message("Getting discrete information up to date with getNewDiscrete...")
    disc_start <- Sys.time()
    getNewDiscrete(con = con, timeseries_id = discrete_ts$timeseries_id)
    disc_duration <- Sys.time() - disc_start
    message(paste0("getNewDiscrete executed in ", round(disc_duration[[1]], 2), " ", units(disc_duration), "."))
  }


  ### Check for a new version of HYDAT, update timeseries in the database if needed. #####
  message("Checking for new HYDAT database with update_hydat...")
  hy_start <- Sys.time()
  suppressMessages(new_hydat <- update_hydat(con = con)) #This function is run for flow and level for each station, even if one of the two is not currently in the HYDAT database. This allows for new data streams to be incorporated seamlessly, either because HYDAT covers a station already reporting but only in realtime or because a flow/level only station is reporting the other param.
  if (new_hydat){
    hy_duration <- Sys.time() - hy_start
    message(paste0("A new version of HYDAT was detected. Timeseries were updated in ", round(hy_duration[[1]], 2), " ", units(hy_duration), "."))
  } else {
    message("HYDAT database is already up to date")
  }

  # if new HYDAT, check WSC stations for new datums and check datums table for new entries
  if (new_hydat){
    message("Checking if latest version of HYDAT has new datums...")
    update_hydat_datums(con = con)
  }


  ### Calculate new daily means and stats from realtime data where necessary #######
  if (nrow(continuous_ts) > 0){
    message("Calculating daily means and statistics where necessary...")
    stat_start <- Sys.time()
    calc_ts <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, last_daily_calculation, last_new_data FROM timeseries WHERE timeseries_id IN ('", paste(continuous_ts$timeseries_id, collapse = "', '"), "') AND category = 'continuous';"))
    needs_calc <- calc_ts[is.na(calc_ts$last_daily_calculation) , ] #All of these need a new calculation.
    has_last_new_data <- calc_ts[!is.na(calc_ts$last_new_data) & !is.na(calc_ts$last_daily_calculation) , ] #only a subset of these need new calculation. Those that have a calculation and don't have an entry for new data don't need calculations.
    if (nrow(has_last_new_data) > 0){ #Take subset of has_last_new_data where the last calculation was before new data being added.
      needs_new_calc <- has_last_new_data[(has_last_new_data$last_new_data) > has_last_new_data$last_daily_calculation , ]
      needs_calc <- rbind(needs_calc, needs_new_calc)
    }
    calculate_stats(timeseries_id = needs_calc$timeseries_id, con = con)

    stats_diff <- Sys.time() - stat_start
    total_diff <- Sys.time() - function_start

    message(paste0("Daily means and statistics calculated in ", round(stats_diff[[1]], 2), " ", units(stats_diff)))
  }


  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_daily'"))

  message("Total elapsed time for hydro_update_daily: ", round(total_diff[[1]], 2), " ", units(total_diff), ". End of function.")

} #End of function
