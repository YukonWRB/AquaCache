#' Impute missing values
#'
#' Impute missing values using either nearby timeseries or using linear or spline interpollation. If using nearby timeseries, they can be of same or of other parameters, as specified with parameter 'extra_params'. User interaction is necessary to select the most appropriate timeseries and to review the result of imputation before updating the database.
#'
#' @param tsid The target timeseries_id.
#' @param radius The radius in kilometers within which to search for similar timeseries.
#' @param start The start datetime (as POSIXct object) from which to fill in missing values (can use local or other time zone).
#' @param end The end datetime (as POSIXct object) to which to fill in missing values (can use local or other time zone).
#' @param extra_params Optional extra parameters to consider when imputing values. For example, you may choose to use wind speeds at 1 or 10 meters to impute speeds at 5 meters.
#' @param imputed Should already imputed data be imputed again?
#' @param con A connection to the database.
#'
#' @return Imputed values added to the database.
#' @export
#'

imputeMissing <- function(tsid, radius, start, end, extra_params = NULL, imputed = TRUE, con = hydrometConnect(silent=TRUE)) {

  on.exit(DBI::dbDisconnect(con))

  returns <- list() #holds the objects to return

  attr(start, "tzone") <- "UTC"
  attr(end, "tzone") <- "UTC"

  entry <- DBI::dbGetQuery(con, paste0("SELECT location, parameter, category, period_type, record_rate FROM timeseries WHERE timeseries_id = ", tsid, ";"))
  if (entry$category != "continuous"){
    stop("This function is not designed to work with discrete category timeseries.")
  }
  returns[["target_timeseries"]] <- entry

  # The interval between start and end must contain at least 50 data points (otherwise standard deviation doesn't work well to assess goodness of fit)
  exist.values <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime > '", start, "' AND datetime < '", end, "';"))

  if (nrow(exist.values) == 0){
    stop("There are no values at all in the datetime range you provided. Perhaps you need to set parameter imputed = TRUE to recalculate previously imputed values, or you need to expand your range.")
  }

  exist.values <- calculate_period(exist.values, tsid, con = con) #This is used to determine by how far to expand the datetime range.

  if (nrow(exist.values) < 50){
    message("There aren't enough data points for the timeseries between the start and end times you specified. Expanding the selection until there are at least 50 points.")
    periods <- unique(lubridate::period_to_seconds(lubridate::period(exist.values$period)))
    if (length(periods) > 1){
      period <- min(periods)
    } else {
      period <- periods
    }

    missing <- 50 - nrow(exist.values)

    #Expand the start and end times by the same amount on each side until there is enough data
    while (nrow(exist.values) < 50){
      start <- start - missing*period/2
      end <- end + missing*period/2
      exist.values <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime > '", start, "' AND datetime < '", end, "';"))
    }
    message("Expanded the time range: start is now ", start, " and end is now ", end, ".")
  } else {
    periods <- unique(lubridate::period_to_seconds(lubridate::period(exist.values$period)))
    if (length(periods) > 1){
      period <- min(periods)
    } else {
      period <- periods
    }
  }

  if (imputed){
    exist.values[exist.values$imputed == TRUE, "value"] <- NA
  }
  exist.values$imputed <- NULL

  start <- min(exist.values$datetime)
  end <- max(exist.values$datetime)

  #if mean, min, max then measurements over 'period' prior to the recorded point are used for the point. Capture those measurements too.
  if (entry$period_type != "instantaneous"){
    start <- min(exist.values$datetime) - period
  }

  # Fill in the missing data points with NA
  full_dt <- data.frame("datetime" = seq.POSIXt(min(exist.values$datetime), max(exist.values$datetime), by = period))
  exist.values <- exist.values[, -which(names(exist.values) == "period")]
  full_dt <- merge(full_dt, exist.values, by = "datetime", all.x = TRUE)

  returns[["db_extract"]] <- exist.values
  returns[["db_extract_w_NAs"]] <- full_dt

  if (nrow(full_dt[is.na(full_dt$value) , ]) == 0){
    return("There are no missing values in the datetime range (or expanded datetime range) under consideration. If you wanted to recalculate previously imputed values, leave parameter 'imputed' to the default TRUE.")
  }


  # Find the same parameter at nearby locations, or at same location but with different record_rate
  nrby <- DBI::dbGetQuery(con, paste0("SELECT l.location, l.name, ST_Distance(ST_Transform(v.geom, 3857), ST_Transform((SELECT v2.geom FROM vectors v2 JOIN locations l2 ON v2.geom_id = l2.geom_id WHERE l2.location = '", entry$location, "'), 3857)) AS distance_meters FROM locations l JOIN vectors v ON l.geom_id = v.geom_id WHERE ST_DWithin(ST_Transform(v.geom, 3857), ST_Transform((SELECT v3.geom FROM vectors v3 JOIN locations l3 ON v3.geom_id = l3.geom_id WHERE l3.location = '", entry$location, "' AND v3.geom_type = 'ST_Point'), 3857), ", radius * 1000, ") AND v.geom_type = 'ST_Point';"))

  if (!is.null(extra_params)){
    similar <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, parameter, period_type, operator, record_rate FROM timeseries WHERE location IN ('", paste(nrby$location, collapse = "', '"), "') AND parameter IN ('", paste(entry$parameter, "', '", paste(extra_params, collapse = "', '"), collapse = "', '", sep = ""), "') AND timeseries_id != ", tsid, ";"))
  } else {
    similar <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, location, parameter, period_type, operator, record_rate FROM timeseries WHERE location IN ('", paste(nrby$location, collapse = "', '"), "') AND parameter = '", entry$parameter, "' AND timeseries_id != ", tsid, ";"))
  }


  if (nrow(similar) > 0){
    similar <- merge(similar, nrby, by = "location")

    # Retain only entries with equal or more frequent record rates
    # Replace the text strings with numeric values for record rate
    duration_mapping <- c('< 1 day' = 1, '1 day' = 2, '1 week' = 3, '4 weeks' = 4, '1 month' = 5, 'year' = 6)
    entry$record_rate_numeric <- duration_mapping[entry$record_rate]
    similar$record_rate_numeric <- duration_mapping[similar$record_rate]
    #retain only those with a more granular or equal recording rate
    similar <- similar[similar$record_rate_numeric <= entry$record_rate_numeric , ]
    similar <- similar[, -which(names(similar) %in% c("record_rate_numeric"))]

    # Check for suitability for each entry (i.e. does the data exist, rank how well does it track normally)
    # Turn the recording rate and mean/max/min into something matching the missing data

    calculate_values <- function(similar, start, end, conn = con) {
      data <- list()
      similar$sd_on_existing <- NA
      similar$missing_data_for_impute <- FALSE
      similar$avg_offset <- NA

      for (i in 1:nrow(similar)){
        #Get data from start to end
        df <- DBI::dbGetQuery(conn, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", similar[i, "timeseries_id"], " AND datetime > '", start, "' AND datetime < '", end, "';"))
        df <- calculate_period(df, similar[i, "timeseries_id"], con = conn)

        # Check if the timeseries is missing data
        df.periods <- unique(lubridate::period_to_seconds(lubridate::period(df$period)))
        if (length(df.periods) > 1){
          df.period <- min(df.periods)
        } else {
          df.period <- df.periods
        }

        full <- data.frame("datetime" = seq.POSIXt(min(df$datetime), max(df$datetime), by = df.period))

        if (nrow(full) != nrow(df)){
          similar[i, "missing_data_for_impute"] <- TRUE
        }

        #Make into mean/max/min if necessary and match the period of the target ts
        calculated <- data.frame(datetime = full_dt$datetime,
                                 value = NA)
        if (entry$period_type == "instantaneous"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(mean(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
          }
        } else if (entry$period_type == "sum"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(sum(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
          }
        } else if (entry$period_type == "min"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(min(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
          }
        } else if (entry$period_type == "max"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(max(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
          }
        } else if (entry$period_type == "mean"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(mean(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
          }
        } else if (entry$period_type == "median"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(stats::median(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))
          }
        } else if (entry$period_type == "(min+max)/2"){
          for (j in 1:nrow(full_dt)){
            calculated[j, "value"] <- hablar::rationalize(mean(c(min(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE), max(df[df$datetime > (full_dt[j, "datetime"]-period) & df$datetime <= full_dt[j, "datetime"] , "value"], na.rm = TRUE))))
          }
        }
        data[[as.character(similar[i, "timeseries_id"])]] <- calculated

        diff <- calculated$value - full_dt$value

        similar[i, "avg_offset"] <- mean(diff, na.rm=TRUE)
        similar[i, "sd_on_existing"] <- stats::sd(diff, na.rm = TRUE)
      }

      return(list(data = data, similar = similar))
    } #end of function calculate_values

    suppressWarnings( #otherwise it gives warnings every time a value can't be calculated
      res <- calculate_values(similar, start = start, end = end)
    )
    data <- res$data
    similar <- res$similar[order(similar$distance_meters), ]

    returns[["matched_data"]] <- data

    message("Working with location ", entry$location, ", parameter ", entry$parameter, ", period_type ", entry$period_type, ", and record rate of ", entry$record_rate, ". Look right to see what it looks like.")
    grey_indices <- which(is.na(full_dt$value))
    bot <- min(full_dt$value, na.rm = TRUE)
    top <- max(full_dt$value, na.rm = TRUE)
    plot(full_dt$datetime, full_dt$value, type = "l", col = "blue")
    for (i in grey_indices){
      graphics::rect(full_dt[i - 1, "datetime"], bot, full_dt[i + 1, "datetime"], top, col = 'grey', border = NA)
    }

    select_for_impute <- function(){
      message("Timeseries that can be used for imputation:")
      print(similar, row.names = FALSE)

      choice <- readline(prompt =
                           writeLines(paste("\nChoose a timeseries by selecting its timeseries_id",
                                            "\nEnter 0 to impute without other data (you can choose from several other options)",
                                            "\nHit Escape to restart."
                           )))
      choice <- as.numeric(choice)

      if (choice == 0){
        res <- data.frame()
      } else if (!(choice %in% similar$timeseries_id)){
        while(!(choice %in% similar$timeseries_id)) {
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
    if (nrow(selected) == 0){
      message("Which other method would you like to try?")
      other_impute <- readline(prompt = writeLines(paste("\n1: Linear inerpolation",
                                                       "\n2: Cubic spline interpolation",
                                                       "\n3: Stop! I'd like to exit"
      )))
      other_impute <- as.numeric(other_impute)

      if (other_impute == 3){
        return(returns)
      }
      if (!(other_impute %in% 1:2)){
        while(!(other_impute %in% 1:2)) {
          other_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
          other_impute <- as.numeric(other_impute)
        }
      }
      other_method <- TRUE
    } else {
      missing_for_impute <- function(new_tsid = selected$timeseries_id, new_start = start, new_end = end, con = con){
        exit <- "false"
        message("The timeseries you selected is missing data during the imputation period. What would you like to do?")
        add_impute <- readline(prompt = writeLines(paste("\n1: Impute the missing data now",
                                                         "\n2: Select another timeseries (or linear/cubic interpolation)",
                                                         "\n3: Use the time series as-is",
                                                         "\n4: Exit this function")))
        add_impute <- as.numeric(add_impute)
        if (!(add_impute %in% 1:4)){
          while(!(add_impute %in% 1:4)) {
            add_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
            add_impute <- as.numeric(add_impute)
          }
        }
        if (add_impute == 4){
          res <- list(exit = "exit")
        }
        if (add_impute == 3){
          similar[similar$timeseries_id == new_tsid, "missing_data_for_impute"] <- FALSE
          res <- list(exit = exit, selected = similar[similar$timeseries_id == new_tsid,])
        }
        else if (add_impute == 2){
          res <- list(exit = exit, selected = select_for_impute())
        } else if (add_impute == 1){
          new_radius <- readline(prompt = writeLines(paste("\nImputing data for timeseries_id ", new_tsid,
                                                           "\n  \nHow big should the search radius be (km)?")))
          new_radius <- as.numeric(new_radius)
          imputeMissing(tsid = new_tsid, radius = new_radius, start = new_start, end = new_end, imputed = FALSE, con = con)
          similar[similar$timeseries_id == new_tsid, "missing_data_for_impute"] <- FALSE
          res <- list(exit = "new_impute", selected = similar[similar$timeseries_id == new_tsid,], new_impute = new_tsid)
        }
        return(res)
      } #End of function missing_for_impute

      if (selected$missing_data_for_impute){
        other_method <- FALSE
        while(selected$missing_data_for_impute){
          missing_for_impute_res <- missing_for_impute()
          selected <- missing_for_impute_res$selected
          if (nrow(missing_for_impute_res$selected) == 0){
            message("Which other method would you like to try?")
            other_impute <- readline(prompt = writeLines(paste("\n1: Linear inerpolation",
                                                               "\n2: Cubic spline interpolation",
                                                               "\n3: Stop! I'd like to exit"
            )))
            other_impute <- as.numeric(other_impute)

            if (other_impute == 3){
              return(returns)
            }
            if (!(other_impute %in% 1:2)){
              while(!(other_impute %in% 1:2)) {
                other_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
                other_impute <- as.numeric(other_impute)
              }
            }
            other_method <- TRUE
            break()
          }
          if (missing_for_impute_res$exit == "exit"){
            message("Goodby!")
            return(returns)
          }
          if (missing_for_impute_res$exit == "new_impute"){
            replace_tsid <- missing_for_impute_res$new_impute
            new_calc <- calculate_values(similar[similar$timeseries_id == replace_tsid,], con = con)
            data[[as.character(replace_tsid)]] <- new_calc$data[[as.character(replace_tsid)]]
          }
        } #End of while loop: user either selected a timeseries with no missing data or opted to go on with the missing data.
      }
    }

  } else {
    # There are no locations within the radius specified! Ask the user if they want linear or cubic.
    message("There were no suitable locations within the radius you specified. Do you want to use another method instead?")
    other_impute <- readline(prompt = writeLines(paste("\n1: Yes, linear inerpolation",
                                                     "\n2: Yes, cubic spline interpolation",
                                                     "\n3: No, I'd like to exit"
    )))
    other_impute <- as.numeric(other_impute)

    if (other_impute == 3){
      return(returns)
    }

    if (!(other_impute %in% 1:2)){
      while(!(other_impute %in% 1:2)) {
        other_impute <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
        other_impute <- as.numeric(other_impute)
      }
    }
    other_method <- TRUE
  }


  if (other_method){
    #Now make the interpolation according to add_impute. This is only using the target tsid!
    imputed <- full_dt
    imputed$imputed <- FALSE

    if (other_impute == 1){ # Linear interpolation
      values <- stats::approx(full_dt$datetime, full_dt$value, method = "linear", n = nrow(full_dt))$y
      for(i in 1:nrow(imputed)){
        if (is.na(imputed[i, "value"])){
          datetime <- imputed[i, "datetime"]
          imputed[i, "value"] <- values[i]
          imputed[i, "imputed"] <- TRUE
        }
      }
      returns[["imputed_data"]] <- imputed
    }  else if (other_impute == 2){ # spline interpolation
      values <- stats::spline(full_dt$datetime, full_dt$value, n = nrow(full_dt))$y
      for(i in 1:nrow(imputed)){
        if (is.na(imputed[i, "value"])){
          datetime <- imputed[i, "datetime"]
          imputed[i, "value"] <- values[i]
          imputed[i, "imputed"] <- TRUE
        }
      }
    }
    returns[["imputed_data"]] <- imputed
  } else {
    #Sub in the values from the selected timeseries wherever possible
    to_use <- data[[as.character(selected$timeseries_id)]]
    offset <- selected$avg_offset
    to_use$value <- to_use$value - offset
    imputed <- full_dt
    imputed$imputed <- FALSE
    for (i in 1:nrow(imputed)){
      if (is.na(imputed[i, "value"])){
        datetime <- imputed[i, "datetime"]
        imputed[i, "value"] <- to_use[to_use$datetime == datetime, "value"]
        imputed[i, "imputed"] <- TRUE
      }
    }
    returns[["imputed_data"]] <- imputed
  }


  #plot and ask the user to confirm ok before modifying the DB
  plot_fx <- function(data){
    without <- data
    without[without$imputed == TRUE, "value"] <- NA
    with <- imputed
    indices <- which(with$imputed)
    with[-c(indices, indices-1, indices+1), "value"] <- NA

    plot(without$datetime, without$value, type = "l", col = "blue", xlab = "datetime", ylab = "value")
    graphics::lines(with$datetime, with$value, col = "red", lwd = 3)
  }

  plot_fx(data = imputed)
  message("Look right for the result. Does it look ok?")
  commit <- readline(prompt = writeLines(paste("\n1: Yes, and please modify the timeseries in the database",
                                               "\n2: Yes, but please ONLY return the result",
                                               "\n3: No, I'd like to try something different"
  )))
  commit <- as.numeric(commit)

  if (commit == 2){
    return(returns)
  }

  if (!(commit %in% c(1:3))){
    while(!(commit %in% c(1:3))) {
      commit <- readline(prompt = writeLines(paste("\nThat isn't an acceptable number. Try again.")))
      commit <- as.numeric(commit)
    }
  }

  if (commit == 1) {
    to_push <- imputed[imputed$imputed == TRUE, ]
    to_push$timeseries_id <- tsid
    #re-enter the period as ISO8601
    days <- floor(period / (24 *3600))
    remaining_hours <- period %% (24 *3600)
    minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
    seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
    to_push$period <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")

    DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime IN ('", paste(to_push$datetime, collapse = "', '"), "')")) #delete is here in case previously imputed values are being over-written
    DBI::dbAppendTable(con, "measurements_continuous", to_push)
    calculate_stats(con = con, timeseries_id = tsid, start_recalc = min(to_push$datetime))
    message("Timeseries_id ", tsid, " has been updated in the database and daily stats recalculated if necessary.")
  }

  if (commit == 3){
    message("This part of the function doesn't work yet.")
  }

  return(returns)
}
