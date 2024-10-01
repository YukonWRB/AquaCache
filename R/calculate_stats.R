#' Calculate daily means and statistics
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates daily means from data in the measurements_continuous table as well as derived statistics for each day (historical min, max, q10, q25, q50 (mean), q75, q90). Derived daily statistics are for each day of year **prior** to the current date for historical context, with the exception of the first day of record for which only the min/max are populated with the day's value.  February 29 calculations are handled differently: see details.
#'
#' Some continuous measurement data may have a period of greater than 1 day. In these cases it would be impossible to calculate daily statistics, so this function explicitly excludes data points with a period greater than P1D.
#'
#' This function is meant to be called from within hydro_update_daily, but is exported just in case a need arises to calculate daily means and statistics in isolation or in another function. It *must* be used with a database created by this package, or one with identical table and column names.
#'
#' @details
#' Calculating daily statistics for February 29 is complicated: due to a paucity of data, this day's statistics are liable to be very mismatched from those of the preceding and succeeding days if calculated based only on Feb 29 data. Consequently, statistics for these days are computed by averaging those of Feb 28 and March 1, ensuring a smooth line when graphing mean/min/max/quantile parameters. This necessitates waiting for complete March 1st data, so Feb 29 means and stats will be delayed until March 2nd.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Only works on data of category 'continuous'. Specifying 'all' will work on all continuous category timeseries.
#' @param start_recalc The day on which to start daily calculations, as a vector of one element OR as NULL. If NULL will only calculate days for which there is realtime data but no daily data yet, plus two days in the past to account for possible past calculations with incomplete data.
#'
#' @return Updated entries in the 'measurements_calculated_daily' table.
#' @export
#'

calculate_stats <- function(con = AquaConnect(silent = TRUE), timeseries_id, start_recalc = NULL) {

  if (!is.null(start_recalc)) {
    if (length(start_recalc) != 1) {
      stop("It looks like you're trying to specify a start date for recalculations, this has to be a vector of length 1.")
    }
    if (!inherits(start_recalc, "Date")) start_recalc <- as.Date(start_recalc)
  }

  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE category = 'continuous' AND record_rate IN ('1 day', '< 1 day');"))
    timeseries_id <- all_timeseries$timeseries_id
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE category = 'continuous' AND timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "') AND record_rate IN ('1 day', '< 1 day');"))
    if (nrow(all_timeseries) == 0) {
      stop("Calculations are not possible. Perhaps the timeseries_id you specified are not of category continuous or have a record_rate of greater than 1 day.")
    }
    if (length(timeseries_id) != length(all_timeseries$timeseries_id)) {
      warning("At least one of the timeseries_id you specified was not of category 'continuous', had a recording rate greater than 1 day, or could not be found in the database.")
    }
    timeseries_id <- all_timeseries$timeseries_id
  }


  #calculate daily means or sums for any days without them
  leap_list <- (seq(1800, 2100, by = 4))
  hydat_checked <- FALSE
  for (i in timeseries_id) {
    start_recalc_i <- start_recalc
    skip <- FALSE
    tryCatch({ #error catching for calculating stats; another one later for appending to the DB
      last_day_historic <- DBI::dbGetQuery(con, paste0("SELECT MAX(date) FROM measurements_calculated_daily WHERE timeseries_id = ", i, ";"))[1,]
      earliest_day_historic <-  as.Date(DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", i, ";"))[1,])
      earliest_day_measurements <- as.Date(DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", i, " AND period <= 'P1D';"))[1,])
      
      # Below lines deal with timeseries that don't have daily values but should, or don't have them far enough in the past. We check if it's necessary to calculate further back in time than start_recalc_i.
      if (is.na(earliest_day_historic)) { # This means that no daily values have been calculated yet, or that they've been completely deleted from the database. In that case we start recalculation straight from the earliest measurement date.
        earliest_day_historic <- earliest_day_measurements
        last_day_historic <- earliest_day_measurements
        start_recalc_i <- earliest_day_measurements
      } else if (is.na(earliest_day_measurements)) { # In this case there are no realtime measurements but there are calculated daily values
        if (last_day_historic < Sys.Date() - 30) { # If this is the case there should be no data to recalculate anymore unless it's been a long time since the last calc or they've been deleted for some reason.
          start_recalc_i <- last_day_historic
        } else { 
          start_recalc_i <- Sys.Date() - 2
        }
        start_recalc_i <- last_day_historic
      } else if (earliest_day_measurements < earliest_day_historic) { # If we got to here there are measurements and daily values. Check if measurements start earlier than daily values.
        start_recalc_i <- earliest_day_measurements
      }
      
      tmp <- DBI::dbGetQuery(con, paste0("SELECT period_type, source_fx FROM timeseries WHERE timeseries_id = ", i, ";"))
      period_type <- tmp[1,1] # Daily values are calculated differently depending on the period type
      source_fx <- tmp[1,2]  #source_fx is necessary to deal differently with WSC locations, since HYDAT daily means take precedence over calculated ones.

      if (!is.null(start_recalc_i)) { #start_recalc_i is specified (not NULL)
        if (!is.na(earliest_day_measurements)) { # If there are measurements, then we can calculate from the earliest measurement date
          if (earliest_day_historic < earliest_day_measurements) {
            last_day_historic <- max(earliest_day_historic, start_recalc_i)
          } else {
            last_day_historic <- if (length(last_day_historic) > 0) max(earliest_day_measurements, start_recalc_i) else earliest_day_measurements #in case the user asked for a start prior to the actual record start, or if there is no record in measurements_calculated_daily yet
          }
        } else { # There are no measurements, so we can only calculate from the earliest day in the measurements_calculated_daily table
          if (start_recalc_i < earliest_day_historic) {
            last_day_historic <- earliest_day_historic
          } else {
            last_day_historic <- NA
          }
        }
      } else { #start_recalc_i is NULL so let's find out when to start recalculating
        if (!is.na(last_day_historic) & !is.na(earliest_day_measurements)) {
          last_day_historic <- last_day_historic - 2 # recalculate the last two days of historic data in case new data has come in
        } else if (is.na(last_day_historic) & !is.na(earliest_day_measurements)) { #say, a new timeseries that isn't in hydat yet or one that's just being added and has no calculations yet
          last_day_historic <- earliest_day_measurements
        } else { # a timeseries that is only in HYDAT, has no realtime measurements
          last_day_historic <- DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND max IS NULL;"))[1,]
        }
      }

      if (is.na(last_day_historic)) {
        skip <- TRUE
      }
      
      if (!skip) {
        missing_stats <- data.frame()
        flag <- FALSE  #This flag is set to TRUE in cases where there isn't an entry in hydat for the station yet. Rare case but it happens! Also is set TRUE if the timeseries recalculation isn't far enough in the past to overlap with HYDAT daily means, or if it's WSC data that's not level or flow.
        if ((source_fx == "downloadWSC") & (last_day_historic < Sys.Date() - 30)) { #this will check to make sure that we're not overwriting HYDAT daily means with calculated realtime means
          # Check to make sure HYDAT is installed and up to date
          if (!hydat_checked) {
            hydat_check(silent = TRUE)
            hydat_checked <- TRUE
          }
          tmp <- DBI::dbGetQuery(con, paste0("SELECT t.location, t.parameter_id, p.param_name FROM timeseries AS t JOIN parameters AS p ON t.parameter_id = p.parameter_id WHERE t.timeseries_id = ", i, ";"))
          hydat_con <- DBI::dbConnect(RSQLite::SQLite(), tidyhydat::hy_downloaded_db())
          if (tmp[, "param_name"] == "discharge, river/stream") {
            last_hydat_year <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (year) FROM DLY_FLOWS WHERE STATION_NUMBER = '", tmp[, "location"], "';"))[1,1]
            if (is.na(last_hydat_year)) { # There is no data in hydat yet
              flag <- TRUE
            }
            if (!flag) {
              max_mth <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (month) FROM DLY_FLOWS WHERE STATION_NUMBER = '", tmp[, "location"], "' AND year = ", last_hydat_year, ";"))[1,1]
              last_hydat <- as.Date(paste0(last_hydat_year, "-", max_mth, if (max_mth %in% c(1,3,5,7,8,10,12)) "-31" else if (max_mth %in% c(4,6,9,11)) "-30" else "28"), format = "%Y-%m-%d")
            }
            if (last_day_historic > last_hydat) {
              flag <- TRUE
            }
          } else if (tmp[, "param_name"] == "water level") {
            last_hydat_year <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (year) FROM DLY_LEVELS WHERE STATION_NUMBER = '", tmp[, "location"], "';"))[1,1]
            if (is.na(last_hydat_year)) {
              flag <- TRUE
            }
            if (!flag) {
              max_mth <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (month) FROM DLY_LEVELS WHERE STATION_NUMBER = '", tmp[, "location"], "' AND year = ", last_hydat_year, ";"))[1,1]
              last_hydat <- as.Date(paste0(last_hydat_year, "-", max_mth, if (max_mth %in% c(1,3,5,7,8,10,12)) "-31" else if (max_mth %in% c(4,6,9,11)) "-30" else "28"), format = "%Y-%m-%d")
              if (last_day_historic > last_hydat) {
                flag <- TRUE
              }
            }
          } else {
            flag <- TRUE
          }
          DBI::dbDisconnect(hydat_con)
          
          if (!flag) {
            gap_measurements <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, grade, approval, imputed, share_with, owner, contributor FROM measurements_continuous WHERE timeseries_id = ", i, " AND datetime >= '", last_hydat + 1, " 00:00:00' AND period <= 'P1D'"))
            
            if (nrow(gap_measurements) > 0) { #Then there is new measurements data, or we're force-recalculating from an earlier date
              gap_measurements <- gap_measurements %>%
                dplyr::group_by(lubridate::year(.data$datetime), lubridate::yday(.data$datetime)) %>%
                dplyr::summarize(date = mean(lubridate::date(.data$datetime)),
                                 value = if (period_type == "sum") sum(.data$value) else if (period_type == "median") stats::median(.data$value) else if (period_type == "min") min(.data$value) else if (period_type == "max") max(.data$value) else if (period_type == "mean") mean(.data$value) else if (period_type == "(min+max)/2") mean(c(min(.data$value), max(.data$value))) else if (period_type == "instantaneous") mean(.data$value),
                                 grade = unique(.data$grade,decreasing = TRUE)[1],
                                 approval = unique(.data$approval, decreasing = TRUE)[1],
                                 imputed = sort(.data$imputed, decreasing = TRUE)[1],
                                 owner = unique(.data$owner)[1],
                                 contributor = unique(.data$contributor)[1],
                                 share_with = sort(.data$share_with)[1],
                                 .groups = "drop")
              gap_measurements <- gap_measurements[,c(3:10)]
              names(gap_measurements) <- c("date", "value", "grade", "approval", "imputed", "owner", "contributor", "share_with")
              
              if (!((last_hydat + 1) %in% gap_measurements$date)) { #Makes a row if there is no data for that day, this way stats will be calculated for that day later.
                owner <- if (nrow(gap_measurements[gap_measurements$date == last_day_historic, "owner"]) > 0) gap_measurements[gap_measurements$date == last_day_historic, "owner"] else unique(gap_measurements$owner)[1]
                contributor <- if (nrow(gap_measurements[gap_measurements$date == last_day_historic, "contributor"]) > 0) gap_measurements[gap_measurements$date == last_day_historic, "contributor"] else unique(gap_measurements$contributor)[1]
                share <- if (nrow(gap_measurements[gap_measurements$date == last_day_historic, "share_with"]) > 0) gap_measurements[gap_measurements$date == last_day_historic, "share_with"] else unique(gap_measurements$share_with)[1]
                gap_measurements <- rbind(gap_measurements, data.frame("date" = last_hydat + 1, "value" = NA, "grade" = NA, "approval" = NA, "imputed" = FALSE, "owner" = owner, "contributor" = contributor, "share_with" = share))
              }
              
              if (last_day_historic < min(gap_measurements$date)) { #Because of the frequent gap between historical HYDAT database and realtime data and the fact that HYDAT daily means are directly appended to the measurements_calculated_daily table, it's possible that no realtime measurements exist between last_day_historic and the earliest measurement. In that case infill with HYDAT values where they exist, taking from the database first for any imputed values and then directly from HYDAT.
                
                backfill_imputed  <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade, approval, imputed, owner, contributor, share_with FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(gap_measurements$date), "' AND date >= '", last_day_historic, "' AND imputed IS TRUE AND value IS NOT NULL;"))
                
                grade_mapping <- c("-1" = "U",
                                   "10" = "I",
                                   "20" = "E",
                                   "30" = "D",
                                   "40" = "N",
                                   "50" = "U")
                
                backfill <- if (tmp[, "param_name"] == "discharge, river/stream") as.data.frame(tidyhydat::hy_daily_flows(tmp[, "location"], start_date = last_day_historic, end_date = min(gap_measurements$date) - 1)) else as.data.frame(tidyhydat::hy_daily_levels(tmp[, "location"], start_date = last_day_historic, end_date = min(gap_measurements$date) - 1))
                backfill <- backfill[ , c("Date", "Value", "Symbol")]
                names(backfill) <- c("date", "value", "grade")
                backfill <- backfill[!is.na(backfill$value) , ]
                backfill$grade <- ifelse(backfill$grade %in% names(grade_mapping),
                                         grade_mapping[backfill$grade],
                                         "Z")
                backfill$approval  <- "A"
                backfill$imputed  <- FALSE
                
                #Remove any entries with values that are already in backfill and not NA, even if they've been imputed
                backfill_imputed <- backfill_imputed[!backfill_imputed$date %in% backfill[!is.na(backfill$value), "date"], ] 
                # Remove any entries in backfill_imputed that are already in backfill but are NA
                backfill <- backfill[!backfill$date %in% backfill_imputed$date, ]
                backfill <- rbind(backfill, backfill_imputed)
                
                gap_measurements <- rbind(gap_measurements, backfill)
              }
              
              #Fill in any missing dates so that they get calculated values where possible
              full_dates <- data.frame("date" = seq.Date(min(gap_measurements$date), max(gap_measurements$date), by = "1 day"))
              gap_measurements <- merge(gap_measurements, full_dates, by = "date", all = TRUE)
              
              # Fill in owner, contributor, share_with with the last known value
              columns_to_fill <- c("owner", "contributor", "share_with")
              gap_measurements[columns_to_fill] <- lapply(gap_measurements[columns_to_fill], function(x) zoo::na.locf(x, na.rm = FALSE))
              
              gap_measurements[is.na(gap_measurements$imputed) , "imputed"] <- FALSE
              gap_measurements[is.na(gap_measurements$grade) , "grade"] <- "U"
              gap_measurements[is.na(gap_measurements$approval) , "approval"] <- "U"
              
              all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date < '", last_hydat, "';"))
              #Need to rbind only the calculated daily means AFTER last_hydat
              all_stats <- rbind(all_stats, gap_measurements[gap_measurements$date >= last_hydat, c("date", "value")])
              missing_stats <- gap_measurements
            } else { #There is no new measurement data, but stats may still need to be calculated because of new HYDAT data
              
              all_imputed  <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade, approval, imputed, owner, contributor, share_with FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND imputed IS TRUE AND value IS NOT NULL;"))
              
              grade_mapping <- c("-1" = "U",
                                 "10" = "I",
                                 "20" = "E",
                                 "30" = "D",
                                 "40" = "N",
                                 "50" = "U")
              
              all_hydat <- if (tmp[, "param_name"] == "discharge, river/stream") as.data.frame(tidyhydat::hy_daily_flows(tmp[, "location"])) else as.data.frame(tidyhydat::hy_daily_levels(tmp[, "location"]))
              all_hydat <- all_hydat[ , c("Date", "Value", "Symbol")]
              names(all_hydat) <- c("date", "value", "grade")
              all_hydat <- all_hydat[!is.na(all_hydat$value) , ]
              all_hydat$grade <- ifelse(all_hydat$grade %in% names(grade_mapping),
                                        grade_mapping[all_hydat$grade],
                                        "Z")
              all_hydat$approval  <- "A"
              all_hydat$imputed  <- FALSE
              
              #Remove any entries with values that are already in all_hydat and not NA, even if they've been imputed
              all_imputed <- all_imputed[!all_imputed$date %in% all_hydat[!is.na(all_hydat$value), "date"], ]
              # Remove any entries in all_imputed that are already in all_hydat but are NA
              all_hydat <- all_hydat[!all_hydat$date %in% all_imputed$date, ]
              
              all <- rbind(all_hydat, all_imputed)
              
              missing_stats <- all[all$date >= last_day_historic , ]
              
              if (nrow(missing_stats) > 0) {
                all_stats <- all[, c("date", "value")]
              }
            } 
          }
        } else {
          flag <- TRUE
        }
        
        if (!(source_fx == "downloadWSC") || flag) { #All timeseries where: operator is not WSC and therefore lacks superseding daily means; isn't recalculating past enough to overlap HYDAT daily means; operator is WSC but there's no entry in HYDAT
          gap_measurements <- DBI::dbGetQuery(con, paste0("SELECT datetime, value, grade, approval, owner, contributor, share_with, imputed FROM measurements_continuous WHERE timeseries_id = ", i, " AND datetime >= '", last_day_historic, " 00:00:00' AND period <= 'P1D'"))
          
          if (nrow(gap_measurements) > 0) { #Then there is new measurements data, or we're force-recalculating from an earlier date perhaps due to updated HYDAT
            gap_measurements <- gap_measurements %>%
              dplyr::group_by(lubridate::year(.data$datetime), lubridate::yday(.data$datetime)) %>%
              dplyr::summarize(date = mean(lubridate::date(.data$datetime)),
                               value = if (period_type == "sum") sum(.data$value) else if (period_type == "median") stats::median(.data$value) else if (period_type == "min") min(.data$value) else if (period_type == "max") max(.data$value) else if (period_type == "mean") mean(.data$value) else if (period_type == "(min+max)/2") mean(c(min(.data$value), max(.data$value))) else if (period_type == "instantaneous") mean(.data$value),
                               grade = unique(.data$grade)[1],
                               approval = unique(.data$approval)[1],
                               imputed = sort(.data$imputed, decreasing = TRUE)[1], # Ensures that if there is even 1 imputed point in a day, the whole day is marked as imputed
                               owner = unique(.data$owner)[1],
                               contributor = unique(.data$contributor)[1],
                               share_with = sort(.data$share_with)[1],
                               .groups = "drop")
            gap_measurements <- gap_measurements[,c(3:10)]
            names(gap_measurements) <- c("date", "value", "grade", "approval", "imputed", "owner", "contributor", "share_with")
            
            if (!((last_day_historic + 1) %in% gap_measurements$date)) { #Makes a row if there is no data for that day, this way stats will be calculated for that day later. Reminder that last_day_historic is 2 days *prior* to the last day for which there is a daily mean.
              owner <- if (nrow(gap_measurements[gap_measurements$date == last_day_historic, "owner"]) > 0) gap_measurements[gap_measurements$date == last_day_historic, "owner"] else unique(gap_measurements$owner)[1]
              contributor <- if (nrow(gap_measurements[gap_measurements$date == last_day_historic, "contributor"]) > 0) gap_measurements[gap_measurements$date == last_day_historic, "contributor"] else unique(gap_measurements$contributor)[1]
              share <- if (nrow(gap_measurements[gap_measurements$date == last_day_historic, "share_with"]) > 0) gap_measurements[gap_measurements$date == last_day_historic, "share_with"] else unique(gap_measurements$share_with)[1]
              gap_measurements <- rbind(gap_measurements, data.frame("date" = last_day_historic + 1, "value" = NA, "grade" = NA, "approval" = NA, "imputed" = FALSE, "owner" = owner, "contributor" = contributor, share_with = share))
            }
            #Fill in any missing dates so that they get calculated values where possible
            full_dates <- data.frame("date" = seq.Date(min(gap_measurements$date), max(gap_measurements$date), by = "1 day"))
            gap_measurements <- merge(gap_measurements, full_dates, by = "date", all = TRUE)
            
            # Fill in owner, contributor, share_with with the last known value
            columns_to_fill <- c("owner", "contributor", "share_with")
            gap_measurements[columns_to_fill] <- lapply(gap_measurements[columns_to_fill], function(x) zoo::na.locf(x, na.rm = FALSE))
            
            gap_measurements[is.na(gap_measurements$imputed) , "imputed"] <- FALSE
            gap_measurements[is.na(gap_measurements$grade) , "grade"] <- "U"
            gap_measurements[is.na(gap_measurements$approval) , "approval"] <- "U"
            
            all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(gap_measurements$date), "';"))
            all_stats <- rbind(all_stats, gap_measurements[, c("date", "value")])
            missing_stats <- gap_measurements
          } else { #There is no new measurement data, but stats may still need to be calculated
            missing_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade, approval, owner, contributor, share_with, imputed FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date >= '", last_day_historic, "';"))
            if (nrow(missing_stats) > 0) {
              all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM measurements_calculated_daily WHERE timeseries_id = ", i, ";"))
            }
          }
        }
        
        # Now calculate stats where they are missing
        if (nrow(missing_stats) > 0) {
          # Remove Feb. 29 data as it would mess with the percentiles; save the missing_stats ones and add them back in later. This is also important as it prevents deleting Feb 29 data in the measurements_calculated_daily table without replacing it.
          feb_29 <- missing_stats[(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
          missing_stats <- missing_stats[!(lubridate::month(missing_stats$date) == "2" & lubridate::mday(missing_stats$date) == "29"), , drop = FALSE]
          all_stats <- all_stats[!(lubridate::month(all_stats$date) == "2" & lubridate::mday(all_stats$date) == "29"), , drop = FALSE]
          # Create a dayofyear column that pretends Feb 29 doesn't exist; all years have 365 days
          missing_stats$dayofyear <- ifelse(lubridate::year(missing_stats$date) %in% leap_list,
                                            ifelse(lubridate::month(missing_stats$date) <= 2,
                                                   lubridate::yday(missing_stats$date),
                                                   lubridate::yday(missing_stats$date) - 1),
                                            lubridate::yday(missing_stats$date))
          all_stats$dayofyear <- ifelse(lubridate::year(all_stats$date) %in% leap_list,
                                        ifelse(lubridate::month(all_stats$date) <= 2,
                                               lubridate::yday(all_stats$date),
                                               lubridate::yday(all_stats$date) - 1),
                                        lubridate::yday(all_stats$date))
          #select only records beginning with the second dayofyear and having values for the second time from all_stats (those for which stats can be calculated). Selects valid rows even if there is no current value, ensuring complete plotting parameters.
          temp <- data.frame()
          first_instance_no_stats <- data.frame()
          for (j in unique(missing_stats$dayofyear)) {
            if (nrow(all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]) > 0) { #Check that there is at least some data for that doy. If not, no need to manipulate that doy further.
              earliest_full_stats <- lubridate::add_with_rollback(min(all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]$date), lubridate::years(1))
              first_instance <- min(all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]$date)
              missing <- missing_stats[missing_stats$dayofyear == j & missing_stats$date >= earliest_full_stats , ]
              temp <- rbind(temp, missing)
              missing_first <- missing_stats[missing_stats$dayofyear == j  & missing_stats$date == first_instance , ]
              first_instance_no_stats <- rbind(first_instance_no_stats, missing_first)
            }
          }
          missing_stats <- temp
          
          #Find the first Feb 29, and add it to first_instance_no_stats if there are no adjacent values that will get added in later
          if (nrow(feb_29) > 0) {
            first_feb_29 <- feb_29[feb_29$date == min(feb_29$date) , ]
            if (!all(c((first_feb_29$date  - 1), (first_feb_29$date  + 1)) %in% missing_stats$date) & !is.na(first_feb_29$value)) { #if statement is FALSE, feb 29 will be dealt with later by getting the mean of the surrounding samples so don't add it to first_instance_no_stats so it isn't dealt with here
              feb_29 <- feb_29[!feb_29$date == first_feb_29$date , ]
              first_feb_29$dayofyear <- NA
              first_instance_no_stats <- rbind(first_instance_no_stats, first_feb_29[, c("date", "value", "grade", "approval", "dayofyear", "imputed", "owner", "contributor", "share_with")])
            }
          }
          
          if (nrow(first_instance_no_stats) > 0) { #Add a min and max for the first instance, delete + append, then remove it from missing_stats for calculations
            missing_stats <- missing_stats[!(missing_stats$date %in% first_instance_no_stats$date) , ]
            first_instance_no_stats <- first_instance_no_stats[!is.na(first_instance_no_stats$value) , ]
            first_instance_no_stats <- first_instance_no_stats[ , !(names(first_instance_no_stats) == "dayofyear")]
            first_instance_no_stats$timeseries_id <- i
            first_instance_no_stats$max <- first_instance_no_stats$min <- first_instance_no_stats$value
            first_instance_no_stats$doy_count <- 1
            DBI::dbWithTransaction(
              con,
              {
                DBI::dbExecute(con, paste0("DELETE FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date IN ('", paste(first_instance_no_stats$date, collapse = "', '"), "')"))
                DBI::dbAppendTable(con, "measurements_calculated_daily", first_instance_no_stats)
                if (nrow(missing_stats) == 0) {  #If < 1 year of data exists, there might not be anything left in missing_stats but first instance data is still being appended.
                  DBI::dbExecute(con, paste0("UPDATE timeseries SET last_daily_calculation = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", i, ";"))
                }
              }
            )
          }
          
          if (nrow(missing_stats) > 0) {
            # Calculate statistics for each day
            missing_stats <- data.table::setDT(missing_stats)
            for (k in 1:nrow(missing_stats)) {
              date <- missing_stats$date[k]
              doy <- missing_stats$dayofyear[k]
              past <- all_stats[all_stats$dayofyear == doy & all_stats$date < date, "value"] #Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
              past <- past[!is.na(past)]
              if (length(past) >= 1) {
                current <- missing_stats$value[k]
                min <- min(past)
                max <- max(past)
                values <- c(list("max" = max, "min" = min, "mean" = mean(past)), as.list(stats::quantile(past, c(0.90, 0.75, 0.50, 0.25, 0.10), names = FALSE)), "doy_count" = if (!is.na(current)) length(past) + 1 else length(past))
                data.table::set(missing_stats, i = k, j = c("max", "min", "mean", "q90", "q75", "q50", "q25", "q10", "doy_count"), value = values)
                if (length(past) > 1 & !is.na(current)) { #need at least 2 measurements to calculate a percent historic, plus a current measurement!
                  data.table::set(missing_stats, i = k, j = "percent_historic_range", value = (((current - min) / (max - min)) * 100))
                }
              }
            }
            data.table::set(missing_stats, j = "dayofyear", value = NULL)
            
            #Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st.
            if (nrow(feb_29) > 0 & !(substr(as.character(as.Date(.POSIXct(Sys.time(), "UTC"))), 6, 10) %in% c("02-29", "03-01,", "03-02"))) {
              for (l in feb_29$date) {
                before <- missing_stats[missing_stats$date == l - 1 , ]
                after <- missing_stats[missing_stats$date == l + 1 , ]
                if (nrow(before) == 0 & nrow(after) == 0) { #If TRUE then can't do anything except for passing the value forward
                  if (is.na(feb_29[feb_29$date == l, "value"])) { #If there's no value and can't do anything else then drop the row
                    feb_29 <- feb_29[!feb_29$date == l ,]
                  }
                } else {
                  feb_29[feb_29$date == l, c("percent_historic_range", "max", "min", "q90", "q75", "q50", "q25", "q10", "mean", "doy_count")] <- suppressWarnings(c(mean(c(before$percent_historic_range, after$percent_historic_range)), mean(c(before$max, after$max)), mean(c(before$min, after$min)), mean(c(before$q90, after$q90)), mean(c(before$q75, after$q75)), mean(c(before$q50, after$q50)), mean(c(before$q25, after$q25)), mean(c(before$q10, after$q10)), mean(c(before$mean, after$mean)), min(c(before$doy_count, after$doy_count)))) # warnings suppressed because of the possibility of NA values
                }
              }
              feb_29 <- hablar::rationalize(feb_29)
              missing_stats <- rbind(missing_stats, feb_29, fill = TRUE)
            }
            missing_stats$timeseries_id <- i
          }
        }
      } else {
        # Nothing here as it's taken care of outside the tryCatch loop
      }
    }, error = function(e) {
      warning("calculate_stats: failed to calculate stats for timeseries_id ", i, ". Returned error: ", e$message)
    }) #End of tryCatch for stats calculation
    
    if (skip) {
      message("Skipping calculations for timeseries ", i, " as it looks like nothing needs to be calculated.")
      next
    }

    
    if (nrow(missing_stats) > 0) { #This is separated from the calculation portion to allow for a tryCatch for calculation and appending, separately.
      tryCatch({
        missing_stats <- missing_stats[order(missing_stats$date), ]
        missing_stats <- hablar::rationalize(missing_stats)  # Occasionally % historic range is dividing by zero (snowpack), so this replaces Inf, -Inf with NAs
        # Construct the SQL DELETE query. This is done in a manner that can't delete rows where there are no calculated stats even if they are between the start and end date of missing_stats.
        delete_query <- paste0("DELETE FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date BETWEEN '", min(missing_stats$date), "' AND '", max(missing_stats$date), "'")
        remaining_dates <- as.Date(setdiff(seq.Date(min(as.Date(missing_stats$date)), max(as.Date(missing_stats$date)), by = "day"), as.Date(missing_stats$date)), origin = "1970-01-01")
        if (length(remaining_dates) > 0) {
          delete_query <- paste0(delete_query, " AND date NOT IN ('", paste(remaining_dates, collapse = "','"), "')")
        }

        DBI::dbWithTransaction(
          con,
          {
            DBI::dbExecute(con, delete_query)
            DBI::dbAppendTable(con, "measurements_calculated_daily", missing_stats) # Append the missing_stats data to the measurements_calculated_daily table
            DBI::dbExecute(con, paste0("UPDATE timeseries SET last_daily_calculation = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", i, ";"))
          }
        )
      }, error = function(e) {
        warning("calculate_stats: failed to append new statistics for timeseries_id ", i, ". Returned error: ", e$message)
      }) #End of tryCatch for removing/adding to DB
    }
  } # End of for loop calculating means and stats for each station in timeseries table
}
