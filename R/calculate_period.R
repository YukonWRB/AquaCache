#' Calculate periodicity of data and add a column
#'
#' Calculates a period for continuous-type temporal data and prepares a column named 'period' with ISO8601 formatted periods for import to postgreSQL database. Will identify changes to periodicity within data, for example moving from 1-hour intervals to 6-hour intervals. MUST be able to connect to the aquacache DB to fetch missing data points or to pull additional data in case of ambiguity.
#'
#' @param data The data.frame or data.table for which to calculate periodicity. Must contain at minimum a column named 'datetime' (in POSIXct format) with no missing values, can also contain a column for 'value' and 'timeseries'. Other columns will be ignored as these are not found in the database.
#' @param timeseries_id The ID of the timeseries for which to calculate periodicity. Used to fetch any data points lacking a period, as well as to search for additional data points if there are too few to calculate a period in the provided `data`. This CAN be NA for the edge use case of creating a new timeseries.
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after. Used to fetch any rows that don't have a period calculated yet, or to fetch additional rows if too few exist to conclusively calculate a period.
#'
#' @return A data.frame with calculated periods as ISO8601 formatted strings in a column named 'period'.
#' @export

calculate_period <- function(data, timeseries_id, con = NULL)
{
  if (!inherits(data, "data.frame")) { # Then it might be a vector
    if (!inherits(data, "POSIXct")) {
      stop("The 'data' parameter must be a data.frame with a column named 'datetime' in POSIXct format OR a POSIXct vector.")
    } else {
      data <- data.frame(datetime = data)
    }
  }
  
  if (!("datetime" %in% names(data))) {
    stop("The 'data' parameter must contain a column named 'datetime'.")
  }
  
  # Drop columns not called 'datetime' 'value', 'timeseries_id' as these can't be found in the database
  cols <- intersect(names(data), c("datetime","value","timeseries_id"))
  if (data.table::is.data.table(data)) {
    data <- data[, cols, with = FALSE]
  } else {
    data <- data[, cols, drop = FALSE]
  }
  
  
  # Get datetimes from the earliest missing period to calculate necessary values, as some might be missing
  col_names <- names(data) # Get all columns in data so as to return a data.frame with the same columns as input
  if (!is.na(timeseries_id)) {
    no_period <- dbGetQueryDT(con, paste0("SELECT ", paste(col_names, collapse = ', '), " FROM measurements_continuous WHERE timeseries_id = ", timeseries_id, " AND datetime >= (SELECT MIN(datetime) FROM measurements_continuous WHERE period IS NULL AND timeseries_id = ", timeseries_id, ") AND datetime NOT IN ('", paste(data$datetime, collapse = "', '"), "');"))
    if (nrow(no_period) > 0) {
      if (data.table::is.data.table(data)) {
        data <- data.table::rbindlist(list(data, no_period), use.names = TRUE)
      } else {
        data <- rbind(data, no_period)
      }    
    }
  }
  if (data.table::is.data.table(data)) {
    data.table::setorder(data, datetime)
  } else {
    data <- data[order(data$datetime), , drop = FALSE]
  }
  diffs <- as.numeric(diff(data$datetime), units = "hours")
  smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
  # Initialize variables to track changes
  consecutive_count <- 0
  changes <- data.frame()
  last_diff <- 0
  if (length(smoothed_diffs) > 0) {
    for (j in 1:length(smoothed_diffs)) {
      if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] != last_diff) { # Check if smoothed interval is not the same as the last recorded diff
        consecutive_count <- consecutive_count + 1
        if (consecutive_count == 3) { # At three consecutive new measurements it's starting to look like a pattern
          last_diff <- smoothed_diffs[j]
          change <- data.frame(datetime = data$datetime[j - 2],
                               period = last_diff)
          changes <- rbind(changes, change)
          consecutive_count <- 0
        }
      } else {
        consecutive_count <- 0
      }
    }
  }
  
  # Calculate the duration in days, hours, minutes, and seconds and assign to the right location in data
  if (nrow(changes) > 0) {
    for (j in 1:nrow(changes)) {
      days <- floor(changes$period[j] / 24)
      remaining_hours <- changes$period[j] %% 24
      minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
      seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
      data[data$datetime == changes$datetime[j], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
    }
    #carry non-na's forward and backwards, if applicable
    data$period <- zoo::na.locf(zoo::na.locf(data$period, na.rm = FALSE), fromLast = TRUE)
    
  } else { #In this case there were too few measurements to conclusively determine a period so pull a few from the DB and redo the calculation
    if (is.na(timeseries_id)) {
      stop("There were too few measurements to calculate a period and no timeseries_id was provided to fetch additional data.")
    }
    extra <- dbGetQueryDT(con, paste0("SELECT ", paste(col_names, collapse = ', '), " FROM measurements_continuous WHERE timeseries_id = ", timeseries_id, " ORDER BY datetime DESC LIMIT 10;"))
    if (data.table::is.data.table(data)) {
      data <- data.table::rbindlist(list(data, extra), use.names = TRUE)
      data.table::setorder(data, datetime)
    } else {
      data <- rbind(data, extra)
      data <- data[order(data$datetime), , drop = FALSE]
    }
    diffs <- as.numeric(diff(data$datetime), units = "hours")
    smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
    consecutive_count <- 0
    changes <- data.frame()
    last_diff <- 0
    if (length(smoothed_diffs) > 0) {
      for (j in 1:length(smoothed_diffs)) {
        if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] != last_diff) {
          consecutive_count <- consecutive_count + 1
          if (consecutive_count == 3) {
            last_diff <- smoothed_diffs[j]
            change <- data.frame(datetime = data$datetime[j - 2],
                                 period = last_diff)
            changes <- rbind(changes, change)
            consecutive_count <- 0
          }
        } else {
          consecutive_count <- 0
        }
      }
    }
    if (nrow(changes) > 0) {
      for (k in 1:nrow(changes)) {
        days <- floor(changes$period[k] / 24)
        remaining_hours <- changes$period[k] %% 24
        minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
        seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
        data[data$datetime == changes$datetime[k], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
      }
      #carry non-na's forward and backwards, if applicable
      data$period <- zoo::na.locf(zoo::na.locf(data$period, na.rm = FALSE), fromLast = TRUE)
    } else {
      data$period <- NULL
    }
  }
  return(data)
} # End of function
