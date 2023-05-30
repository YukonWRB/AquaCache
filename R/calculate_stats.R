#' Calculate daily means and statistics
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function is meant to be called from within hydro_update_daily, but is exported just in case a need arises to calculate daily means and statistics in isolation. It *must* be used with a database created by this package, or one with identical table and column names.
#'
#' @param timeseries A data.frame containing, at a minimum, columns named 'location', and 'parameter', necessary to identify the exact records in need of updating.
#' @param path The path to the hydrometric database, passed to [WRBtools::hydroConnect()].
#' @param start_recalc The day on which to start daily calculations, one vector element per row of the dataset passed as argument to timeseries. If NULL will only recalculate necessary days.
#'
#' @return Updated entries in the 'daily' table
#' @export
#'

calculate_stats <- function(timeseries = NULL, path = NULL, start_recalc = NULL) {

  if (is.null(timeseries) | is.null(path)){
    stop("You must specify parameters 'timeseries' and 'path.'")
  }

  if (!is.null(start_recalc)){
    if (nrow(timeseries) != length(start_recalc)){
      stop("It looks like you're trying to specify a start date for recalculations, but there isn't exactly one vector element per row in the parameter timeseries.")
    }
  }

  on.exit(DBI::dbDisconnect(hydro))

  #calculate daily means for any days without them
  leap_list <- (seq(1800, 2100, by = 4))
  for (i in 1:nrow(timeseries)){
    loc <- timeseries$location[i]
    parameter <- timeseries$parameter[i]
    hydro <- WRBtools::hydroConnect(path = path, silent = TRUE)
    units <- DBI::dbGetQuery(hydro, paste0("SELECT units FROM timeseries WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]
    last_day_historic <- DBI::dbGetQuery(hydro, paste0("SELECT MAX(date) FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]
    #TODO: the step below is slow, needs to query a very large table. Can it be done another way?
    earliest_day_realtime <- as.character(as.Date(DBI::dbGetQuery(hydro, paste0("SELECT MIN(datetime_UTC) FROM realtime WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]))
    if (!is.na(last_day_historic) & !is.na(earliest_day_realtime)){
      if (last_day_historic > as.character(as.Date(earliest_day_realtime) + 2)) {
        last_day_historic <- as.character(as.Date(last_day_historic) - 2) #if the two days before last_day_historic are in the realtime data, recalculate last two days in case realtime data hadn't yet come in. This will also wipe the stats for those two days just in case.
      }
    } else if (is.na(last_day_historic) & !is.na(earliest_day_realtime)){
      last_day_historic <- as.character(as.Date(earliest_day_realtime) - 2)
    }
    if (!is.null(start_recalc)){
      last_day_historic <- min(last_day_historic, start_recalc[i])
    }

    gap_realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM realtime WHERE parameter = '", parameter, "' AND location = '", loc, "' AND datetime_UTC BETWEEN '", last_day_historic, " 00:00:00' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
    if (nrow(gap_realtime) > 0){
      gap_realtime <- gap_realtime %>%
        dplyr::group_by(lubridate::year(.data$datetime_UTC), lubridate::yday(.data$datetime_UTC)) %>%
        dplyr::summarize(date = mean(lubridate::date(.data$datetime_UTC)),
                         value = mean(.data$value),
                         grade = sort(.data$grade,decreasing=TRUE)[1],
                         approval = sort(.data$approval, decreasing=TRUE)[1],
                         .groups = "drop")
      gap_realtime <- gap_realtime[,c(3:6)]
      names(gap_realtime) <- c("date", "value", "grade", "approval")
      if (min(gap_realtime$date) > last_day_historic){ #Makes a row if there is no data for that day, this way stats will be calculated for that day later.
        gap_realtime <- rbind(gap_realtime, data.frame("date" = last_day_historic, "value" = NA, "grade" = NA, "approval" = NA))
      }
      gap_realtime <- fasstr::fill_missing_dates(gap_realtime, "date", pad_ends = FALSE) #fills any missing dates with NAs, which will let them be filled later on when calculating stats.
      gap_realtime$location <- loc
      gap_realtime$parameter <- parameter
      gap_realtime$date <- as.character(gap_realtime$date)
      DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE parameter = '", parameter, "' AND date >= '", min(gap_realtime$date), "' AND location = '", loc, "'"))
      DBI::dbAppendTable(hydro, "daily", gap_realtime)
      DBI::dbExecute(hydro, paste0("UPDATE timeseries SET last_daily_calculation_UTC = '", as.character(.POSIXct(Sys.time(), "UTC")), "' WHERE location= '", loc, "' AND parameter = '", parameter, "' AND type = 'continuous'"))
    }
    #TODO: surely there's a way to remove the DELETE and APPEND operations above, and only do it once after stats are calculated?

    # Now calculate stats where they are missing
    all_stats <- DBI::dbGetQuery(hydro, paste0("SELECT date, value FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "'"))
    missing_stats <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "' AND max IS NULL"))
    DBI::dbDisconnect(hydro)
    #TODO: the step above selects rows that get dropped later, and does this each time. How about just working with gap_realtime from above?

    # Remove Feb. 29 data as it would mess with the percentiles; save the missing_stats ones and add them back in later. This is also important as it prevents deleting Feb 29 data in the daily table without replacing it.
    feb_29 <- missing_stats[(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
    missing_stats <- missing_stats[!(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
    all_stats <- all_stats[!(lubridate::month(all_stats$date) == "2" & lubridate::mday(all_stats$date) == "29"), , drop = FALSE]
    # Create a dayofyear column that pretends Feb 29 doesn't exist; all years have 365 days
    missing_stats <- missing_stats %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$date) %in% leap_list,
                                                                        ifelse(lubridate::month(.data$date) <= 2,
                                                                               lubridate::yday(.data$date),
                                                                               lubridate::yday(.data$date) - 1),
                                                                        lubridate::yday(.data$date)))
    all_stats <- all_stats %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$date) %in% leap_list,
                                                                ifelse(lubridate::month(.data$date) <= 2,
                                                                       lubridate::yday(.data$date),
                                                                       lubridate::yday(.data$date) - 1),
                                                                lubridate::yday(.data$date)))

    #selects only records beginning with the second dayofyear and having values for the second time from all_stats (those for which stats can be calculated). Selects valid rows even if there is no current value, ensuring complete plotting parameters.
    missing_stats <- missing_stats[order(missing_stats[ , "date"]) , ]
    all_stats <- all_stats[order(all_stats[ , "date"]) , ]
    temp <- data.frame()
    for (j in unique(missing_stats$dayofyear)){
      earliest <- all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]$date[2]
      if (!is.na(earliest)){
        missing <- missing_stats[missing_stats$dayofyear == j & missing_stats$date >= earliest , ]
        temp <- rbind(temp, missing)
      }
    }
    missing_stats <- temp

    if (nrow(missing_stats) > 0){
      for (k in 1:nrow(missing_stats)){
        date <- missing_stats$date[k]
        doy <- missing_stats$dayofyear[k]
        current <- missing_stats$value[k]
        past <- all_stats[all_stats$dayofyear == doy & all_stats$date < date , ]$value #Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
        past <- past[!is.na(past)]
        if (length(past) >= 1){
          missing_stats$max[k] <- max(past) #again, NOT including current measurement
          missing_stats$min[k] <- min(past)
          missing_stats$QP90[k] <- stats::quantile(past, 0.90)
          missing_stats$QP75[k] <- stats::quantile(past, 0.75)
          missing_stats$QP50[k] <- stats::quantile(past, 0.50)
          missing_stats$QP25[k] <- stats::quantile(past, 0.25)
          missing_stats$QP10[k] <- stats::quantile(past, 0.10)
          if (length(past) > 1 & !is.na(current)){ #need at least 2 measurements to calculate a percent historic!
            missing_stats$percent_historic_range[k] <- ((current - min(past)) / (max(past) - min(past))) * 100
          }
        }
      }
      missing_stats <- subset(missing_stats, select=-c(dayofyear)) #remove column not in database table

      #Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st. Unfortunately this means that initial setups done on those days will not calculate Feb 29!
      if (nrow(feb_29) > 0 & !(substr(as.character(as.Date(.POSIXct(Sys.time(), "UTC"))), 6, 10) %in% c("02-29", "03-01,", "03-02"))) {
        for (l in 1:nrow(feb_29)){
          date <- as.Date(feb_29$date[l])
          before <- missing_stats[missing_stats$date == date - 1 , ]
          after <- missing_stats[missing_stats$date == date + 1 , ]
          feb_29$percent_historic_range[l] <- mean(c(before$percent_historic_range, after$percent_historic_range))
          feb_29$max[l] <- mean(c(before$max, after$max))
          feb_29$min[l] <- mean(c(before$min, after$min))
          feb_29$QP90[l] <- mean(c(before$QP90, after$QP90))
          feb_29$QP75[l] <- mean(c(before$QP75, after$QP75))
          feb_29$QP50[l] <- mean(c(before$QP50, after$QP50))
          feb_29$QP25[l] <- mean(c(before$QP25, after$QP25))
          feb_29$QP10[l] <- mean(c(before$QP10, after$QP10))
        }
      }
      missing_stats <- rbind(missing_stats, feb_29)

      #TODO: line below needs to become an UPDATE instead
      hydro <- WRBtools::hydroConnect(path = path, silent = TRUE)
      DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "' AND date BETWEEN '", min(missing_stats$date), "' AND '", max(missing_stats$date), "' AND max IS NULL")) #The AND max IS NULL part prevents deleting entries within the time range that have not been recalculated, as would happen if, say, a Feb 29 is calculated on March 3rd. Without that condition, March 1 and 2 would also be deleted but are not part of missing_stats due to initial selection criteria of missing_stats.
      DBI::dbAppendTable(hydro, "daily", missing_stats)
      DBI::dbDisconnect(hydro)
    }
  } # End of for loop calculating means and stats for each station in timeseries table
}
