#' Bring snow course data into the AquaCache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Brings in pared-down snow course data to the database. Can automatically calculate an offset value where locations have operated in parallel in anticipation of replacing the old location with a nearby new one, updating the calculation with each new data point (see parameter old_loc).
#'
#' @param location The location code associated with the snow course.
#' @param parameter_id For snow courses, one of "SWE" or "depth" corresponding to the desired parameter.
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param old_loc In some cases the measurement location has moved slightly over the years, but not enough for the new location to be distinct from the old location. In this case you can specify the old location name which will be searched for in the snowDB. If found, the timeseries from the old location will be treated as if they are the new location. An offset will be calculated whenever possible putting the old location in-line with the new location. New location data takes precedence when both were measured.
#' @param con A connection to the AquaCache database, only used if an offset is calculated for an old_loc. If not provided, a connection will be attempted using AquaConnect().
#' @param snowCon A connection to the snow database.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export


downloadSnowCourse <- function(location, parameter_id, start_datetime, end_datetime = Sys.time(), old_loc = NULL, con = NULL, snowCon = snowConnect())
  {

  # Checking start_datetime parameter
  tryCatch({
    if (inherits(start_datetime, "character") & nchar(start_datetime) > 10) { #Does not necessarily default to 0 hour.
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else if (inherits(start_datetime, "POSIXct")) {
      attr(start_datetime, "tzone") <- "UTC"
    } else if (inherits(start_datetime, "Date") | (inherits(start_datetime, "character") & nchar(start_datetime) == 10)) { #defaults to 0 hour
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else {
      stop("Parameter start_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter start_datetime to POSIXct.")
  })

  # Checking end_datetime parameter
  tryCatch({
    if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) { #Does not necessarily default to 0 hour.
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "POSIXct")) {
      attr(end_datetime, "tzone") <- "UTC"
    } else if (inherits(end_datetime, "Date") | (inherits(end_datetime, "character") & nchar(end_datetime) == 10)) { #defaults to very end of day
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      end_datetime <- end_datetime + 60*60*23.9999
    } else {
      stop("Parameter end_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter end_datetime to POSIXct.")
  })

  start_date <- as.Date(start_datetime)
  end_date <- as.Date(end_datetime)
  
  if (!is.null(old_loc)) {
    #Check if there are new measurements at the old station
    old_meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, survey_date, ", parameter_id, ", estimate_flag FROM means WHERE location = '", old_loc, "' AND survey_date > '", start_date, "';"))
  } else {
    old_meas <- data.frame()
  }

  if (nrow(old_meas) > 0) { #There's some new data at the old location, so recalculate an offset and apply backwards.
    #Get all old and new data
    old_meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, survey_date, ", parameter_id, ", estimate_flag FROM means WHERE location = '", old_loc, "';"))
    names(old_meas) <- c("target_datetime", "datetime", "value", "note")
    # Adjust the plain date to middle of the day MST
    old_meas$target_datetime <- as.POSIXct(old_meas$target_datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    old_meas$datetime <- as.POSIXct(old_meas$datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    
    meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, survey_date, ", parameter_id, ", estimate_flag FROM means WHERE location = '", location, "';"))
    names(meas) <- c("target_datetime", "datetime", "value", "note")
    # Adjust the plain date to middle of the day MST
    meas$target_datetime <- as.POSIXct(meas$target_datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    meas$datetime <- as.POSIXct(meas$datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    
    common_datetimes <- as.POSIXct(intersect(old_meas$target_datetime, meas$target_datetime))
    # Calculate the offset as a percentage of the new data, apply to the old data
    offset <- mean(meas$value[meas$target_datetime %in% common_datetimes] / old_meas$value[old_meas$target_datetime %in% common_datetimes])
    old_meas$value <- old_meas$value * offset #apply offset to old data
    old_meas <- old_meas[!(old_meas$target_datetime %in% meas$target_datetime),] #discard overlapping old data
    meas <- rbind(meas, old_meas) #combine
    
    #Discard any measurements that are outside the requested time range
    meas <- meas[meas$datetime >= start_datetime & meas$datetime <= end_datetime, ]
    
    try({
      # Update the timeseries table of AquaCache DB with the offset values
      if (is.null(con)) {
        con <- AquaConnect(silent = TRUE)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
      }
      hydro_param <- DBI::dbGetQuery(con, paste0("SELECT parameter_id FROM parameters WHERE param_name = '", if (parameter_id == "swe") "snow water equivalent" else if (parameter_id == "depth") "snow depth", "';"))[1,1]
      media_id <- DBI::dbGetQuery(con, "SELECT media_id FROM media_types WHERE media_type = 'atmospheric'")[1,1]
      DBI::dbExecute(con, paste0("UPDATE timeseries SET note = 'Compound timeseries incorporating measurements from ", old_loc, ". Measurements at the old location adjusted using a multiplier of ", round(offset, 4), ", calculated from ", length(common_datetimes), " data points. New location measurements take precedence over old for overlap period.' WHERE location = '", location, "' AND category = 'discrete' AND media_id = ", media_id, " AND parameter_id = ", hydro_param, ";"))
    })
  } else {
    #Get measurements for that location beginning after the start_datetime
    meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, survey_date, ", parameter_id, ", estimate_flag FROM means WHERE location = '", location, "' AND survey_date > '", start_date, "' AND survey_date <= '", end_date, "';"))
    
  }
  if (nrow(meas) > 0) {
    names(meas) <- c("target_datetime", "datetime", "value", "result_value_type")
    # Adjust the plain date to middle of the day MST
    meas$target_datetime <- as.POSIXct(meas$target_datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    meas$datetime <- as.POSIXct(meas$datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    
    # Change estimated or actual values to the database values
    meas$result_value_type[meas$result_value_type] <- DBI::dbGetQuery(con, "SELECT result_value_type_id FROM result_value_types WHERE LOWER(result_value_type) = 'estimated'")[1,1]
    meas$result_value_type[!meas$result_value_type] <- DBI::dbGetQuery(con, "SELECT result_value_type_id FROM result_value_types WHERE LOWER(result_value_type) = 'actual'")[1,1]
    meas$sample_type <- DBI::dbGetQuery(con, "SELECT sample_type_id FROM sample_types WHERE LOWER(sample_type) = 'field msr/obs'")[1,1]
    meas$collection_method <- DBI::dbGetQuery(con, "SELECT collection_method_id FROM collection_methods WHERE LOWER(collection_method) = 'observation'")[1,1]
    meas$protocol <- DBI::dbGetQuery(con, "SELECT protocol_id FROM analysis_protocols WHERE LOWER(protocol_name) = 'bc snow survey sampling guide'")[1,1]
    meas$owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE LOWER(name) = 'yukon department of environment, water resources branch';")[1,1]
    meas$contributor <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE LOWER(name) = 'yukon department of environment, water resources branch';")[1,1]
    
    meas <- meas[!is.na(meas$value), ]  # Some measurements in table 'measurements_discrete' can have NA values, but this is used to represent values that are below/above detection limits. Not applicable for snow survey measurements.
  } else {
    meas <- data.frame()
  }
  return(meas)
}
