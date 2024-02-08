#' Bring snow course data into the hydromet database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Brings in pared-down snow course data to the database. Can automatically calculate an offset value where locations have operated in parallel in anticipation of replacing the old location with a nearby new one, updating the calculation with each new data point (see parameter old_loc).
#'
#' @param location The location code associated with the snow course.
#' @param param_code For snow courses, one of "SWE" or "depth" corresponding to the desired parameter.
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param old_loc In some cases the measurement location has moved slightly over the years, but not enough for the new location to be distinct from the old location. In this case you can specify the old location name which will be searched for in the snowDB. If found, the timeseries from the old location will be treated as if they are the new location. An offset will be calculated whenever possible putting the old location in-line with the new location. New location data takes precedence when both were measured.
#' @param snowCon A connection to the snow database.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export


downloadSnowCourse <- function(location, param_code, start_datetime, end_datetime = Sys.time(), old_loc = NULL, snowCon = snowConnect())
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

  if (!is.null(old_loc)) {
    #Check if there are new measurements at the old station
    old_meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, sample_datetime, ", param_code, ", estimate_flag FROM means WHERE location = '", old_loc, "' AND sample_datetime > '", start_datetime, "';"))
  } else {
    old_meas <- data.frame()
  }

  if (nrow(old_meas) > 0) { #There's some new data at the old location, so recalculate an offset and apply backwards.
    #Get all old and new data
    old_meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, sample_datetime, ", param_code, ", estimate_flag FROM means WHERE location = '", old_loc, "';"))
    names(old_meas) <- c("target_datetime", "datetime", "value", "note")
    meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, sample_datetime, ", param_code, ", estimate_flag FROM means WHERE location = '", location, "';"))
    names(meas) <- c("target_datetime", "datetime", "value", "note")
    common_datetimes <- as.Date(intersect(old_meas$target_datetime, meas$target_datetime), origin = "1970-01-01")
    offset <- mean(meas$value[meas$target_datetime %in% common_datetimes] - old_meas$value[old_meas$target_datetime %in% common_datetimes])
    old_meas$value <- old_meas$value + round(offset, 1) #apply offset to old data
    old_meas <- old_meas[!(old_meas$target_datetime %in% meas$target_datetime),] #discard overlapping old data
    meas <- rbind(meas, old_meas) #combine
  } else {
    #Get measurements for that location beginning after the start_datetime
    meas <- DBI::dbGetQuery(snowCon, paste0("SELECT target_date, sample_datetime, ", param_code, ", estimate_flag FROM means WHERE location = '", location, "' AND sample_datetime > '", start_datetime, "';"))
    names(meas) <- c("target_datetime", "datetime", "value", "note")
  }
  if (nrow(meas) > 0) {
    meas$target_datetime <- as.POSIXct(meas$target_datetime, tz = "UTC")
    meas$note <- as.character(meas$note)
    meas$note[meas$note == "TRUE"] <- "estimated"
    meas$note[meas$note == "FALSE"] <- NA
    meas$sample_class <- "M"
  } else {
    meas <- data.frame()
  }
  return(meas)
}
