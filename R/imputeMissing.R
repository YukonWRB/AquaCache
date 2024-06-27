#' Impute missing values
#'
#' @description
#' ## Note
#' See function predictMissing() (not finished yet!) for machine learning predictions, especially useful if there are no timeseries than be be used with a direct relationship such as predicting flow using level, or level using multiple other level stations.
#' 
#'Impute missing values using either a nearby timeseries or using linear or spline interpolation on the time-series itself. If using nearby timeseries, they can be of same or of other parameters, as specified with parameter 'extra_params'. User interaction is necessary to select the most appropriate timeseries and to review the result of imputation before updating the database.
#'
#' Imputing long periods of missing data using spline or linear interpolation on the timeseries itself is not recommended. If this is the only feasible option presented by this function because no nearby timeseries can be used for imputation, we recommend using [predictMissing()] instead.
#'
#' @param tsid The target timeseries_id.
#' @param radius The radius in kilometers within which to search for similar timeseries.
#' @param start The start datetime (as POSIXct, Date, or character) from which to fill in missing values. If specifying a POSIXct the time zone will be respected.
#' @param end The end datetime (as POSIXct, Date, or character) to which to fill in missing values. If specifying a POSIXct the time zone will be respected.
#' @param extra_params Optional extra parameters (by name, which must match exactly an entry in the database) to consider when imputing values. For example, you may choose to use wind speeds at 1 or 10 meters to impute speeds at 5 meters.
#' @param imputed Should already imputed data be imputed again?
#' @param daily Should the imputation be done on the daily table? Even if set to TRUE this will only apply if there are no entries in table measurements_continuous to modify.
#' @param min_gap An optional integer specifying the minimum number of missing points to interpolate. This can be useful when you want to use a certain method to impute only short gaps, and use another method for longer gaps.
#' @param max_gap An optional integer specifying the maximum number of missing points to interpolate. This can be useful when you want to use a certain method to impute only short gaps, and use another method for longer gaps.
#' @param con A connection to the database.
#'
#' @return Imputed values added to the database.
#' @export


imputeMissing <- function(tsid, radius, start, end, extra_params = NULL, imputed = TRUE, daily = FALSE, min_gap = 1, max_gap = Inf, con = AquaCacheCon(silent = TRUE)) {

  on.exit(DBI::dbDisconnect(con))
  
  rlang::check_installed("plotly", reason = "to make an interactive plot of imputed data.")

  returns <- list() #holds the objects to return
  
  if (inherits(start, "Date") || inherits(start, "character")) {
    start <- as.POSIXct(start, tz = "UTC")
  } else if (inherits(start, "POSIXct")) {
    attr(start, "tzone") <- "UTC"
  } else {
    stop("start must be a date, character, or posixct.")
  }
  if (inherits(end, "Date") || inherits(end, "character")) {
    end <- as.POSIXct(end, tz = "UTC")
  } else if (inherits(end, "POSIXct")) {
    attr(end, "tzone") <- "UTC"
  } else {
    stop("end must be a date, character, or posixct.")
  }
  
  
  entry <- DBI::dbGetQuery(con, paste0("SELECT t.location, p.param_name AS parameter, t.category, t.period_type, t.record_rate FROM timeseries AS t JOIN parameters AS p on T.parameter = p.param_code WHERE t.timeseries_id = ", tsid, ";"))
  if (entry$category != "continuous") {
    stop("This function is not designed to work with discrete category timeseries.")
  }
  returns[["target_timeseries"]] <- entry
  
  # The interval between start and end must contain at least 50 data points (otherwise standard deviation doesn't work well to assess goodness of fit)
  if (!daily) {
    exist.values <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start, "' AND datetime <= '", end, "';"))
  } else { # daily is TRUE
    exist.cont <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start, "' AND datetime <= '", end, "';"))
    if (nrow(exist.cont) > 0) {  # Now see if there are entries in calculated_daily that are NOT in the datetime range of exist.cont
      exist.values <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(max(exist.cont$datetime)), "' AND date <= '", as.Date(min(exist.cont$datetime)), "';"))
      exist.values$datetime <- as.POSIXct(exist.values$date, tz = "UTC")
      exist.values$date <- NULL
      if (nrow(exist.values) == 0) {
        exist.values <- exist.cont
        daily <- FALSE
        warning("There are no values in the datetime range you provided in the calculated_daily table. Working with entries to table measurements_continuous instead.")
      }
    } else {
      exist.values <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", as.Date(start), "' AND date <= '", as.Date(end), "';"))
      exist.values$datetime <- as.POSIXct(exist.values$date, tz = "UTC")
      exist.values$date <- NULL
    }
    if (nrow(exist.values) == 0) {
      exist.values <- exist.cont
      daily <- FALSE
      warning("There are no values in the datetime range you provided in the calculated_daily table. Working with entries to table measurements_continuous instead.")
    }
  }
  
  if (nrow(exist.values) == 0) {
    stop("There are no values at all in the datetime range you provided. Perhaps you need to expand your range, or you need to set parameter daily to TRUE.")
  }
  
  if (!daily) {
    exist.values <- calculate_period(exist.values, tsid, con = con) #This is used to determine by how far to expand the datetime range.
  } else {
    exist.values$period <- "P1D"
  }
  
  if (nrow(exist.values) < 50) {
    message("There aren't enough data points for the timeseries between the start and end times you specified. Expanding the selection until there are at least 50 points.")
    if (!daily) {
      periods <- unique(lubridate::period_to_seconds(lubridate::period(exist.values$period)))
      if (length(periods) > 1) {
        period <- min(periods)
      } else {
        period <- periods
      }
      
      missing <- 50 - nrow(exist.values)
      
      #Expand the start and end times by the same amount on each side until there is enough data
      while (nrow(exist.values) < 50) {
        start <- start - missing*period/2
        end <- end + missing*period/2
        exist.values <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", start, "' AND datetime <= '", end, "';"))
      }
      message("Expanded the time range: start is now ", start, " and end is now ", end, ".")
    } else { #daily is TRUE
      missing <- 50 - nrow(exist.values)
      day.start <- as.Date(start)
      day.end <- as.Date(end)
      while (nrow(exist.values) < 50) {
        day.start <- day.start - missing
        day.end <- day.end + missing
        exist.values <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", day.start, "' AND date <= '", day.end, "';"))
        exist.values$datetime <- as.POSIXct(exist.values$date, tz = "UTC")
        exist.values$date <- NULL
      }
      start <- as.POSIXct(day.start)
      end <- as.POSIXct(day.end)
      period <- 86400
      message("Expanded the time range: start is now ", start, " and end is now ", end, ".")
    }
  } else {
    if (!daily) {
      periods <- unique(lubridate::period_to_seconds(lubridate::period(exist.values$period)))
      if (length(periods) > 1) {
        period <- min(periods)
      } else {
        period <- periods
      }
    } else {
      period <- 86400
    }
  }
  
  exist.values[is.na(exist.values$imputed), "imputed"] <- FALSE
  exist.values$period <- NULL

  # Now keep/remove imputed values depending on the user preference, and check if data exists at the start and end of the imputation period. If not, expand the range again if possible.
  if (imputed) {
    exist.values[exist.values$imputed == TRUE, "value"] <- NA
    # Check to make sure the last value is not NA, since it must be anchored somehow.
    if (is.na(exist.values[exist.values$datetime == max(exist.values$datetime), "value"])) {
      if (daily) {
        # Find the next non-NA value and add data to that point
        goto_date <- DBI::dbGetQuery(con, paste0("SELECT min(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date > '", as.Date(end), "' AND value IS NOT NULL AND imputed IS FALSE;"))[1,1]
        goto_data <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date <= '", goto_date, "' AND date > '", as.Date(end), "' AND imputed IS FALSE;"))
        goto_data$datetime <- as.POSIXct(goto_data$date, tz = "UTC")
        goto_data$date <- NULL
        rbind(exist.values, goto_data)
      } else { # Daily is FALSE
        # Find the next non-NA value and add data to that point
        goto_date <- DBI::dbGetQuery(con, paste0("SELECT min(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime > '", end, "' AND value IS NOT NULL AND imputed IS FALSE;"))[1,1]
        goto_data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime <= '", goto_date, "' AND datetime > '", end, "' AND imputed IS FALSE;"))
        rbind(exist.values, goto_data)
      }
    }
    # Check to make sure the first value is not NA UNLESS the start_predict is prior to the entry in timeseries table
    if (is.na(exist.values[exist.values$datetime == min(exist.values$datetime), "value"])) {
      if (daily) {
        gofrom_date <- DBI::dbGetQuery(con, paste0("SELECT max(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND value IS NOT NULL AND date < '", as.Date(start), "' AND imputed IS FALSE;"))[1,1]
        # Find the next non-NA value and add data to that point
        gofrom_data <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", gofrom_date, "' AND date < '", as.Date(start), "';"))
        gofrom_data$datetime <- as.POSIXct(gofrom_data$date, tz = "UTC")
        gofrom_data$date <- NULL
        exist.values <- rbind(gofrom_data, exist.values)
      } else {
        gofrom_date <- DBI::dbGetQuery(con, paste0("SELECT max(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND value IS NOT NULL AND datetime < '", start, "' AND imputed IS FALSE;"))[1,1]
        # Find the next non-NA value and add data to that point
        gofrom_data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", gofrom_date, "' AND datetime < '", start, "';"))
        exist.values <- rbind(gofrom_data, exist.values)
      }
    }
  } else { # imputed is false, so do the same as above but allow imputed values
    # Check to make sure the last value is not NA, since it must be anchored somehow.
    if (is.na(exist.values[exist.values$datetime == max(exist.values$datetime), "value"])) {
      if (daily) {
        # Find the next non-NA value and add data to that point
        goto_date <- DBI::dbGetQuery(con, paste0("SELECT min(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date > '", as.Date(end), "' AND value IS NOT NULL;"))[1,1]
        goto_data <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date <= '", goto_date, "' AND date > '", as.Date(end), "';"))
        goto_data$datetime <- as.POSIXct(goto_data$date, tz = "UTC")
        goto_data$date <- NULL
        exist.values <- rbind(exist.values, goto_data)
      } else {
        # Find the next non-NA value and add data to that point
        goto_date <- DBI::dbGetQuery(con, paste0("SELECT min(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime > '", end, "' AND value IS NOT NULL;"))[1,1]
        goto_data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime <= '", goto_date, "' AND datetime > '", end, "';"))
        exist.values <- rbind(exist.values, goto_data)
      }
    }
    # Check to make sure the first value is not NA UNLESS the start_predict is prior to the entry in timeseries table
    if (is.na(exist.values[exist.values$datetime == min(exist.values$datetime), "value"])) {
      if (daily) {
        gofrom_date <- DBI::dbGetQuery(con, paste0("SELECT max(date) FROM calculated_daily WHERE timeseries_id = ", tsid, " AND value IS NOT NULL AND date < '", as.Date(start), "';"))[1,1]
        # Find the next non-NA value and add data to that point
        gofrom_data <- DBI::dbGetQuery(con, paste0("SELECT date, value, imputed FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", gofrom_date, "' AND date < '", as.Date(start), "';"))
        gofrom_data$datetime <- as.POSIXct(gofrom_data$date, tz = "UTC")
        gofrom_data$date <- NULL
        exist.values <- rbind(gofrom_data, exist.values)
      } else {
        gofrom_date <- DBI::dbGetQuery(con, paste0("SELECT max(datetime) FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND value IS NOT NULL AND datetime < '", start, "';"))[1,1]
        # Find the next non-NA value and add data to that point
        gofrom_data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime >= '", gofrom_date, "' AND datetime < '", start, "';"))
        exist.values <- rbind(gofrom_data, exist.values)
      }
    }
  }
  
  exist.values$imputed <- NULL
  
  if (daily) {
    exist.values$datetime <- as.POSIXct(exist.values$date, tz = "UTC")
    exist.values$date <- NULL
    entry$period_type <- "mean"
    entry$record_rate <- "1 day"
  }

  start <- min(exist.values$datetime)
  end <- max(exist.values$datetime)

  #if mean, min, max then measurements over 'period' prior to the recorded point are used for the point. Capture those measurements too.
  if (entry$period_type != "instantaneous") {
    start <- min(exist.values$datetime) - period
  }

  # Fill in the missing data points with NA
  full_dt <- data.frame("datetime" = seq.POSIXt(min(exist.values$datetime), max(exist.values$datetime), by = period))
  full_dt <- merge(full_dt, exist.values, by = "datetime", all.x = TRUE)

  returns[["db_extract"]] <- exist.values
  returns[["db_extract_w_NAs"]] <- full_dt

  if (nrow(full_dt[is.na(full_dt$value) , ]) == 0) {
    return("There are no missing values in the datetime range (or expanded datetime range) under consideration. If you wanted to recalculate previously imputed values, leave parameter 'imputed' to the default TRUE. If you want to impute data that's only in the calculated_daily table, set parameter 'daily' to TRUE.")
  }

  # Find the same parameter at nearby locations, or at same location but with different record_rate
  nrby <- DBI::dbGetQuery(con, paste0("SELECT l.location, l.name, ST_Distance(ST_Transform(v.geom, 3857), ST_Transform((SELECT v2.geom FROM vectors v2 JOIN locations l2 ON v2.geom_id = l2.geom_id WHERE l2.location = '", entry$location, "'), 3857)) AS distance_meters FROM locations l JOIN vectors v ON l.geom_id = v.geom_id WHERE ST_DWithin(ST_Transform(v.geom, 3857), ST_Transform((SELECT v3.geom FROM vectors v3 JOIN locations l3 ON v3.geom_id = l3.geom_id WHERE l3.location = '", entry$location, "' AND v3.geom_type = 'ST_Point'), 3857), ", radius * 1000, ") AND v.geom_type = 'ST_Point';"))

  # look for timeseries within the radius (in table nrby) that might have data that can be used to impute the missing values
  if (!is.null(extra_params)) { # if there are extra parameters, look for those as well
    similar <- DBI::dbGetQuery(con, paste0("SELECT t.location, t.timeseries_id, p.param_name AS parameter, t.period_type, t.record_rate FROM timeseries AS t JOIN parameters AS p on t.parameter = p.param_code WHERE t.location IN ('", paste(nrby$location, collapse = "', '"), "') AND p.param_name IN ('", paste(entry$parameter, "', '", paste(extra_params, collapse = "', '"), collapse = "', '", sep = ""), "') AND t.timeseries_id != ", tsid, " AND t.start_datetime < '", min(exist.values$datetime), "';"))
    
  } else { # if there are no extra parameters, look for the same parameter at nearby locations
    similar <- DBI::dbGetQuery(con, paste0("SELECT t.location, t.timeseries_id, p.param_name AS parameter, t.period_type, t.record_rate FROM timeseries AS t JOIN parameters AS p on t.parameter = p.param_code WHERE t.location IN ('", paste(nrby$location, collapse = "', '"), "') AND p.param_name = '", entry$parameter, "' AND t.timeseries_id != ", tsid, " AND t.start_datetime < '", min(exist.values$datetime), "';"))
  }
  

  message("Working with location ", entry$location, ", parameter ", entry$parameter, ", period_type ", entry$period_type, ", and record rate of ", entry$record_rate, ". Look right to see what it looks like. You can zoom and pan the graph.")
  
  # Run-length encoding to identify stretches of NAs
  lengths <- rle(is.na(full_dt$value))
  positions <- cumsum(lengths$lengths)  # Positions of changes
  bot <- min(full_dt$value, na.rm = TRUE)
  top <- max(full_dt$value, na.rm = TRUE)
  
  p <- plotly::plot_ly()
  first_impute <- TRUE
  first_no_impute <- TRUE
  for (i in seq_along(lengths$lengths)) {
    if (lengths$values[i]) {  # If stretch is NA
      length_na <- lengths$lengths[i]
      start_pos <- positions[i] - lengths$lengths[i] + 1
      end_pos <- positions[i + 1] - lengths$lengths[i + 1]
      
      # Determine color based on imputation criteria
      leg_imputing <- FALSE
      leg_no_impute <- FALSE
      color <- if (length_na >= min_gap && length_na <= max_gap) "darkorange2" else "green4"
      if (color == "darkorange2" & first_impute) {
        leg_imputing <- TRUE
        first_impute <- FALSE
      } else if (color == "green4" & first_no_impute) {
        leg_no_impute <- TRUE
        first_no_impute <- FALSE
      }
      
      if (start_pos > 1 && end_pos < nrow(full_dt)) {
        p <- p %>%
          plotly::add_ribbons(data = full_dt[start_pos:end_pos , ], x = ~datetime, ymin = bot, ymax = top, name = if (leg_imputing) "Missing - imputing" else if (leg_no_impute) "Missing - no impute", color = I(color), line = list(width = 0.2), showlegend = if (leg_imputing || leg_no_impute) TRUE else FALSE)
      } else if (start_pos == 1) {  # Handling for start of data
        p <- p %>%
          plotly::add_ribbons(data = full_dt[start_pos:end_pos , ], x = ~datetime, ymin = bot, ymax = top, name = if (leg_imputing) "Missing - imputing" else if (leg_no_impute) "Missing - no impute", color = I(color), line = list(width = 0.2), showlegend = if (leg_imputing || leg_no_impute) TRUE else FALSE)
      } else if (end_pos == nrow(full_dt)) {  # Handling for end of data
        p <- p %>%
          plotly::add_ribbons(data = full_dt[start_pos:end_pos , ], x = ~datetime, ymin = bot, ymax = top, name = if (leg_imputing) "Missing - imputing" else if (leg_no_impute) "Missing - no impute", color = I(color), line = list(width = 0.2), showlegend = if (leg_imputing || leg_no_impute) TRUE else FALSE)
      }
      
    }
  }
  p <- p %>%
    plotly::add_trace(data = full_dt, x = ~datetime, y = ~value, type = "scatter", mode = "lines+markers", name = "Existing", line = list(color = "blue"), marker = list(color = "blue", size = 2), showlegend = TRUE) %>%
    plotly::layout(xaxis = list(title = "Date", showgrid = FALSE, showline = TRUE), yaxis = list(title = "value", showgrid = FALSE, showline = TRUE), legend = list(x = mean(top, bot), y = 0.5))
  print(p)

  no_similar <- FALSE
  if (nrow(similar) > 0) {
    similar <- merge(similar, nrby, by = "location")

    # Retain only entries with equal or more frequent record rates
    # Replace the text strings with numeric values for record rate
    duration_mapping <- c('< 1 day' = 1, '1 day' = 2, '1 week' = 3, '4 weeks' = 4, '1 month' = 5, 'year' = 6)
    entry$record_rate_numeric <- duration_mapping[entry$record_rate]
    similar$record_rate_numeric <- duration_mapping[similar$record_rate]
    #retain only those with a more granular or equal recording rate
    similar <- similar[similar$record_rate_numeric <= entry$record_rate_numeric , ]
    similar <- similar[, -which(names(similar) %in% c("record_rate_numeric"))]

    if (nrow(similar) > 0) {
      # Check suitability for each entry (i.e. does the data all exist, rank how well it tracks normally)
      #   Will need to turn the recording rate and mean/max/min into matching the missing data
      similar$avg_offset <- NA
      similar$sd_on_offset <- NA
      similar$missing_data_for_impute <- FALSE
      data <- list()
      suppressWarnings( #otherwise it gives warnings every time a value can't be calculated
        for (i in 1:nrow(similar)) {
          #Get data from start to end
          df <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", similar[i, "timeseries_id"], " AND datetime >= '", start, "' AND datetime <= '", end, "';"))
          if (daily) {
            to_add <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", similar[i, "timeseries_id"], " AND date >= '", as.Date(start), "' AND date <= '", if (nrow(df) > 0) as.Date(min(df$datetime)) else as.Date(end), "';"))
            if (nrow(to_add) > 0) {
              to_add$datetime <- as.POSIXct(to_add$date)
              to_add$date <- NULL
              df <- rbind(df, to_add)
            }
          }
          if (nrow(df) == 0) {
            similar[i, "missing_data_for_impute"] <- TRUE
            next
          }
          df <- calculate_period(df, similar[i, "timeseries_id"], con = con)
          
          # Check if the timeseries is missing data
          df.periods <- unique(lubridate::period_to_seconds(lubridate::period(df$period)))
          if (length(df.periods) > 1) {
            df.period <- min(df.periods)
          } else {
            df.period <- df.periods
          }
          
          full <- data.frame("datetime" = seq.POSIXt(min(df$datetime), max(df$datetime), by = df.period))
          
          if (nrow(full) != nrow(df)) {
            similar[i, "missing_data_for_impute"] <- TRUE
          }
          
          #Make into mean/max/min if necessary and match the period of the target ts
          calculated <- data.frame(datetime = full_dt$datetime,
                                   value = NA)
          if (entry$period_type == "instantaneous") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(mean(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
            }
          } else if (entry$period_type == "sum") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(sum(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
            }
          } else if (entry$period_type == "min") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(min(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
            }
          } else if (entry$period_type == "max") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(max(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
            }
          } else if (entry$period_type == "mean") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(mean(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
            }
          } else if (entry$period_type == "median") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(stats::median(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
            }
          } else if (entry$period_type == "(min+max)/2") {
            for (j in 1:nrow(full_dt)) {
              calculated[j, "value"] <- hablar::rationalize(mean(c(min(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE), max(df[df$datetime > (full_dt[j, "datetime"] - period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))))
            }
          }
          
          diff <- calculated$value - full_dt$value
          
          similar[i, "avg_offset"] <- mean(diff, na.rm = TRUE)
          similar[i, "sd_on_offset"] <- stats::sd(diff, na.rm = TRUE)
          
          if (is.na(similar[i, "avg_offset"])) {
            next
          }
          data[[as.character(similar[i, "timeseries_id"])]] <- calculated
        }
      )
      similar <- similar[!is.na(similar$avg_offset),]
      similar <- similar[order(similar$distance_meters), ]
      
      select_for_impute <- function() {
        message("Timeseries that can be used for imputation:")
        print(similar[ , -which(names(similar) == "avg_offset")], row.names = FALSE)
        
        choice <- readline(prompt =
                             writeLines(paste("\nChoose a timeseries by selecting its timeseries_id",
                                              "\nEnter 0 to impute without other data (you can choose from several other options)",
                                              "\nHit Escape to restart."
                             )))
        choice <- as.numeric(choice)
        
        if (choice == 0) {
          res <- data.frame()
        } else if (!(choice %in% similar$timeseries_id)) {
          while (!(choice %in% similar$timeseries_id)) {
            choice <- readline(prompt =
                                 writeLines(paste("\nThat isn't an acceptable choice. Try again."
                                 )))
            choice <- as.numeric(choice)
          }
        }
        
        res <- similar[similar$timeseries_id == choice, ]
        return(res)
      }
      
      selected <- select_for_impute()
      if (nrow(selected) == 0) {
        message("Which other method would you like to try?")
        other_impute <- readline(prompt = writeLines(paste("\n1: Linear inerpolation",
                                                           "\n2: Cubic spline interpolation",
                                                           "\n3: Stop! I'd like to exit"
        )))
        other_impute <- as.numeric(other_impute)
        
        if (other_impute == 3) {
          return(returns)
        }
        if (!(other_impute %in% 1:2)) {
          while (!(other_impute %in% 1:2)) {
            other_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
            other_impute <- as.numeric(other_impute)
          }
        }
        other_method <- TRUE
      } else {
        missing_for_impute <- function(new_tsid = selected$timeseries_id, new_start = start, new_end = end, con = con) {
          exit <- FALSE
          message("The timeseries you selected is missing data during the imputation period. What would you like to do?")
          add_impute <- readline(prompt = writeLines(paste("\n1: Exit: I'll impute data for that timeseries and come back",
                                                           "\n2: Select another timeseries (or linear/cubic interpolation)",
                                                           "\n3: Use the time series as-is",
                                                           "\n4: Exit this function")))
          add_impute <- as.numeric(add_impute)
          if (!(add_impute %in% 1:3)) {
            while (!(add_impute %in% 1:3)) {
              add_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
              add_impute <- as.numeric(add_impute)
            }
          }
          if (add_impute == 3) {
            similar[similar$timeseries_id == new_tsid, "missing_data_for_impute"] <- FALSE
            res <- list(exit = exit, selected = similar[similar$timeseries_id == new_tsid,])
          }
          else if (add_impute == 2) {
            res <- list(exit = exit, selected = select_for_impute())
          } else if (add_impute == 1) {
            exit <- TRUE
            res <- list(exit = exit, selected = similar[similar$timeseries_id == new_tsid,])
          }
          return(res)
        } #End of function missing_for_impute
        
        if (selected$missing_data_for_impute) {
          other_method <- FALSE
          while (selected$missing_data_for_impute) {
            missing_for_impute_res <- missing_for_impute()
            selected <- missing_for_impute_res$selected
            if (nrow(missing_for_impute_res$selected) == 0) {
              message("Which other method would you like to try?")
              other_impute <- readline(prompt = writeLines(paste("\n1: Linear inerpolation",
                                                                 "\n2: Cubic spline interpolation",
                                                                 "\n3: Stop! I'd like to exit"
              )))
              other_impute <- as.numeric(other_impute)
              
              if (other_impute == 3) {
                return(returns)
              }
              if (!(other_impute %in% 1:2)) {
                while (!(other_impute %in% 1:2)) {
                  other_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
                  other_impute <- as.numeric(other_impute)
                }
              }
              other_method <- TRUE
              break()
            }
            if (missing_for_impute_res$exit) {
              message("Reminder to now go and impute data for timeseries_id ", selected$timeseries_id, "!")
              return()
            }
          } #End of while loop: user either selected a timeseries with no missing data or opted to go on with the missing data.
        } else {
          other_method <- FALSE
        }
      }
    } else {
      no_similar <- TRUE
    }
  } else {
    no_similar <- TRUE
  }
  
  if (no_similar) {
    # There are no locations within the radius specified! Ask the user if they want linear or cubic.
    message("There were no suitable locations within the radius you specified. Do you want to use another method instead?")
    other_impute <- readline(prompt = writeLines(paste("\n1: Yes, linear inerpolation",
                                                       "\n2: Yes, cubic spline interpolation",
                                                       "\n3: No, I'd like to exit"
    )))
    other_impute <- as.numeric(other_impute)
    
    if (other_impute == 3) {
      return(returns)
    }
    
    if (!(other_impute %in% 1:2)) {
      while (!(other_impute %in% 1:2)) {
        other_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
        other_impute <- as.numeric(other_impute)
      }
    }
    other_method <- TRUE
  }

  
  
  impute_function <- function(df, min_gap, max_gap, method, selected = NULL, list = NULL) { #selected and list are only used for the direct interpolation method
    df$imputed <- FALSE  # Initialize the 'imputed' column
    lengths <- rle(is.na(df$value))  # Run-length encoding to find consecutive NAs
    positions <- cumsum(lengths$lengths)  # Positions of changes
    
    if (method == "direct") {
      to_use <- list[[as.character(selected$timeseries_id)]]
      offset <- selected$avg_offset
      to_use$value <- to_use$value - offset
    }
    
    for (i in seq_along(lengths$lengths)) {
      if (lengths$values[i] && lengths$lengths[i] <= max_gap && lengths$lengths[i] >= min_gap) {  # Check for NA stretch less than max_gap
        start_pos <- positions[i] - lengths$lengths[i] + 1
        end_pos <- positions[i + 1] - lengths$lengths[i + 1]
        if (end_pos < nrow(df)) {  # Avoid out-of-bounds
          if (method == "linear") { # Linear interpolation for the stretch of NAs
            y_values <- stats::approx(x = c(start_pos - 1, end_pos + 1), 
                                      y = df$value[c(start_pos - 1, end_pos + 1)], 
                                      xout = seq(start_pos, end_pos), 
                                      method = "linear")$y
          } else if (method == "spline") {
            if (start_pos - 20 < 1) {
              start_pos_spline <- 1
            } else {
              start_pos_spline <- start_pos - 20
            }
            if (end_pos + 20 > nrow(df)) {
              end_pos_spline <- nrow(df)
            } else {
              end_pos_spline <- end_pos + 20
            }
            y_values <- stats::spline(x = c(start_pos_spline:end_pos_spline), 
                                      y = df$value[c(start_pos_spline:end_pos_spline)], 
                                      xout = seq(start_pos, end_pos))$y
          } else if (method == "direct") {
            dt_start <- df[start_pos, "datetime"]
            dt_end <- df[end_pos, "datetime"]
            y_values <- to_use[to_use$datetime >= dt_start & to_use$datetime <= dt_end, "value"]
          }
        }
        df$value[start_pos:end_pos] <- y_values
        df$imputed[start_pos:end_pos] <- TRUE
      }
    }
    return(df)
  }
  
  
  if (other_method) {
    #Now make the interpolation according to add_impute. This is only using the target tsid!
    if (other_impute == 1) { # Linear interpolation
      imputed <- impute_function(full_dt, min_gap, max_gap, "linear")
      returns[["imputed_data"]] <- imputed
    }  else if (other_impute == 2) { # spline interpolation
      imputed <- impute_function(full_dt, min_gap, max_gap, "spline")
    }
    returns[["imputed_data"]] <- imputed
  } else {
    # Now ask user if they want to impute directly (with an offset) between the two timeseries or if a model should be used.
    message("Would you like to impute directly between the two timeseries or use a model to impute the data?")
    impute_type <- readline(prompt = writeLines(paste("\n1: Impute directly between the two timeseries. An average offset will be calculated between the two timeseries and used to impute the data.",
                                                      "\n2: Use a random forest model to impute the data.",
                                                      "\n3: Stop! I'd like to exit."
    )))
    if (impute_type == 2) { # Use a model to impute the data 
      message("Use function predictMissing() to impute using a model (must exit this function).")
      while (impute_type == 2) {
        impute_type <- readline(prompt = writeLines(paste("\nTry again with 1 (Impute directly using the selected timeseries) or 3, exit.")))
        impute_type <- as.numeric(impute_type)
      }
    }
    if (!(impute_type %in% c(1,3))) {
      while (!(impute_type %in% c(1,3))) {
        impute_type <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
        impute_type <- as.numeric(impute_type)
      }
    }
    if (impute_type == 3) {
      return(returns)
    }
    if (impute_type == 1) {
      imputed <- impute_function(full_dt, min_gap, max_gap, "direct", selected, data)
      returns[["imputed_data"]] <- imputed
    }
  } 

  #plot and ask the user to confirm ok before modifying the DB
  plot_fx <- function(data) {
    without <- data # without will be data without imputed values
    without[without$imputed == TRUE, "value"] <- NA
    with <- data # with will be data with imputed values
    indices <- which(with$imputed) # indices of imputed values
    with[-c(indices, indices - 1, indices + 1), "value"] <- NA # Leave only the imputed values plus a point before and after, so the lines plot
     p <- plotly::plot_ly(data = without, x = ~datetime, y = ~value, type = "scatter", mode = "lines+markers", line = list(color = "blue"), marker = list(color = "blue", size = 3), name = c("Existing")) %>%
      plotly::add_trace(data = with, x = ~datetime, y = ~value, type = "scatter", mode = "lines+markers", line = list(color = "red", size = 0.1), marker = list(color = "red", size = 6), name = "Imputed")
     return(p)
  }
  
  print(plot_fx(data = imputed))
  
  message("Look right for the result. Does it look ok?")
  commit <- readline(prompt = writeLines(paste("\n1: Yes, and please modify the timeseries in the database",
                                               "\n2: Yes, but please ONLY return the result",
                                               "\n3: No, I'd like to try something different"
  )))
  commit <- as.numeric(commit)

  if (commit == 2) {
    return(returns)
  }

  if (!(commit %in% c(1:3))) {
    while (!(commit %in% c(1:3))) {
      commit <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
      commit <- as.numeric(commit)
    }
  }
  
  if (commit == 1) {
    message("Commiting the results to the DB and modifying tables. Please be patient. A message will be shown when finished.")
    to_push <- imputed[imputed$imputed == TRUE, ]
    to_push$timeseries_id <- tsid
    to_push$grade <- "U"
    to_push$approval <- "U"

    if (daily) {
      to_push$date <- as.Date(to_push$datetime)
      to_push$datetime <- NULL
      DBI::dbExecute(con, paste0("DELETE FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date IN ('", paste(to_push$date, collapse = "', '"), "')")) #delete is here in case previously imputed values are being over-written
      DBI::dbAppendTable(con, "calculated_daily", to_push)
      calculate_stats(con = con, timeseries_id = tsid, start_recalc = min(to_push$date))
    } else {
      if (entry$period_type != "instantaneous") {
        #re-enter the period as ISO8601
        days <- floor(period / 86400)
        remainder <- period %% 86400
        hours <- floor(remainder / 3600)
        remainder <- remainder %% 3600
        minutes <- floor(remainder / 60)
        seconds <- remainder %% 60
        to_push$period <- paste("P", days, "DT", hours, "H", minutes, "M", seconds, "S", sep = "")
        } else {
        to_push$period <- "PT0S"
      }
      
      DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime IN ('", paste(to_push$datetime, collapse = "', '"), "')")) #delete is here in case previously imputed values are being over-written
      DBI::dbAppendTable(con, "measurements_continuous", to_push)
      calculate_stats(con = con, timeseries_id = tsid, start_recalc = min(to_push$datetime))
    }
    
    message("Timeseries_id ", tsid, " has been updated in the database and daily stats recalculated if necessary.")
  }
  
  if (commit == 3) {
    message("This part of the function doesn't work yet.")
  }
}
