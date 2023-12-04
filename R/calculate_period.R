#' Calculate periodicity of data and add a column
#'
#' Calculates a period for continuous=type temporal data and prepares a column named 'period' with ISO8601 formatted periods for import to postgreSQL database. Will identify changes to periodicity within data, for example moving from 1-hour intervals to 6-hour intervals.
#'
#' @param data The data.frame for which to calculate periodicity. Must contain, at minimum, columns named 'datetime', 'value', 'grade', 'approval', but only 'datetime' needs to have no NAs.
#' @param timeseries_id The ID of the timeseries for which to calculate periodicity. Used to fetch any data points lacking a period, as well as to search for additional data points if there are too few to calculate a period in the provided `data`.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return A list of two objects: a data.frame with calculated periods and a Boolean delete flag, true if rows were fetched from the database (and thus need to be deleted prior to appending) or false if no data needs to be replaced
#' @export

calculate_period <- function(data, timeseries_id, con = hydrometConnect())
{
  # Get datetimes from the earliest missing period to calculate necessary values, as some might be missing
  no_period <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, grade, approval FROM measurements_continuous WHERE timeseries_id = ", timeseries_id, " AND datetime >= (SELECT MIN(datetime) FROM measurements_continuous WHERE period IS NULL AND timeseries_id = ", timeseries_id, ");"))
  if (nrow(no_period) > 0){
    ts <- rbind(ts, no_period)
  }
  ts <- ts[order(ts$datetime) ,] #Sort ascending
  diffs <- as.numeric(diff(ts$datetime), units = "hours")
  smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
  # Initialize variables to track changes
  consecutive_count <- 0
  changes <- data.frame()
  last_diff <- 0
  if (length(smoothed_diffs) > 0) {
    for (j in 1:length(smoothed_diffs)) {
      if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] < 25 && smoothed_diffs[j] != last_diff) { # Check if smoothed interval is less than threshold, which is set to more than a whole day (greatest interval possible is 24 hours) as well as not the same as the last recorded diff
        consecutive_count <- consecutive_count + 1
        if (consecutive_count == 3) { # At three consecutive new measurements it's starting to look like a pattern
          last_diff <- smoothed_diffs[j]
          change <- data.frame(datetime = ts$datetime[j-3],
                               period = last_diff)
          changes <- rbind(changes, change)
          consecutive_count <- 0
        }
      } else {
        consecutive_count <- 0
      }
    }
  }

  # Calculate the duration in days, hours, minutes, and seconds and assign to the right location in ts
  ts$period <- NA
  if (nrow(changes) > 0){
    for (j in 1:nrow(changes)){
      days <- floor(changes$period[j] / 24)
      remaining_hours <- changes$period[j] %% 24
      minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
      seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
      ts[ts$datetime == changes$datetime[j], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
    }
    #carry non-na's forward and backwards, if applicable
    ts$period <- zoo::na.locf(zoo::na.locf(ts$period, na.rm = FALSE), fromLast=TRUE)

  } else { #In this case there were too few measurements to conclusively determine a period so pull a few from the DB and redo the calculation
    no_period <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, grade, approval FROM measurements_continuous WHERE timeseries_id = ", timeseries_id, " ORDER BY datetime DESC LIMIT 10;"))
    no_period$period <- NA
    ts <- rbind(ts, no_period)
    ts <- ts[order(ts$datetime), ]
    diffs <- as.numeric(diff(ts$datetime), units = "hours")
    smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
    consecutive_count <- 0
    changes <- data.frame()
    last_diff <- 0
    if (length(smoothed_diffs) > 0){
      for (j in 1:length(smoothed_diffs)) {
        if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] < 25 && smoothed_diffs[j] != last_diff) {
          consecutive_count <- consecutive_count + 1
          if (consecutive_count == 3) {
            last_diff <- smoothed_diffs[j]
            change <- data.frame(datetime = ts$datetime[j-3],
                                 period = last_diff)
            changes <- rbind(changes, change)
            consecutive_count <- 0
          }
        } else {
          consecutive_count <- 0
        }
      }
    }
    if (nrow(changes) > 0){
      for (k in 1:nrow(changes)){
        days <- floor(changes$period[k] / 24)
        remaining_hours <- changes$period[k] %% 24
        minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
        seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
        ts[ts$datetime == changes$datetime[k], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
      }
      #carry non-na's forward and backwards, if applicable
      ts$period <- zoo::na.locf(zoo::na.locf(ts$period, na.rm = FALSE), fromLast=TRUE)
    } else {
      ts$period <- NULL
    }
  }
  return(ts)
} # End of function
