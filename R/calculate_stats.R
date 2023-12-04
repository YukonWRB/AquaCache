#' Calculate daily means and statistics
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Calculates daily means from data in the measurements_continuous table as well as derived statistics for each day (historical min, max, q10, q25, q50 (mean), q75, q90). Derived daily use values for each day of year **prior** to the current date for historical context, with the exception of the first day of record for which only the min/max are populated with the day's value.  February 29 calculations are handled differently: see details.
#'
#' This function is meant to be called from within hydro_update_daily, but is exported just in case a need arises to calculate daily means and statistics in isolation. It *must* be used with a database created by this package, or one with identical table and column names.
#'
#' @details
#' Calculating daily statistics for February 29 is complicated: due to a paucity of data, this day's statistics are liable to be very mismatched from those of the preceding and succeeding days if calculated based only on Feb 29 data. Consequently, statistics for these days are computed by averaging those of Feb 28 and March 1, ensuring a smooth line when graphing mean/min/max/quantile parameters. This necessitates waiting for complete March 1st data, so Feb 29 means and stats will be delayed until March 2nd.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Only works on data of category 'continuous'
#' @param start_recalc The day on which to start daily calculations, as a vector of one element OR one vector element per element in `tsid` OR as NULL. If NULL will only calculate days for which there is realtime data but no daily data yet, plus two days in the past to account for possible past calculations with incomplete data.
#'
#' @return Updated entries in the 'calculated_daily' table.
#' @export
#'

calculate_stats <- function(con = hydrometConnect(silent = TRUE), timeseries_id, start_recalc = NULL) {

  if (!is.null(start_recalc)){
    if ((length(timeseries_id) != length(start_recalc)) & (length(start_recalc) != 1)){
      stop("It looks like you're trying to specify a start date for recalculations, but there isn't exactly one vector element OR one element per elment in the parameter tsid")
    }
    if (!inherits(start_recalc, "Date")) start_recalc <- as.Date(start_recalc)
  }

  if (timeseries_id[1] == "all"){
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE category = 'continuous';"))
    timeseries_id <- all_timeseries$timeseries_id
  } else {
    all_timeseries <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE category = 'continuous' AND timeseries_id IN ('", paste(timeseries_id, collapse = "', '"), "');"))
    if (length(timeseries_id) != length(all_timeseries$timeseries_id)) {
      #TODO: improve this warning message with which tsid exactly was missing
      warning("At least one of the timeseries_id you specified was not of category 'continuous' or could not be found in the database.")
    }
    timeseries_id <- all_timeseries$timeseries_id
  }


  #calculate daily means or sums for any days without them
  leap_list <- (seq(1800, 2100, by = 4))
  for (i in timeseries_id){
    tryCatch({ #error catching for calculating stats; another one later for appending to the DB
      last_day_historic <- DBI::dbGetQuery(con, paste0("SELECT MAX(date) FROM calculated_daily WHERE timeseries_id = ", i, ";"))[1,]
      earliest_day_measurements <- as.Date(DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", i, ";"))[1,])
      tmp <- DBI::dbGetQuery(con, paste0("SELECT period_type, operator FROM timeseries WHERE timeseries_id = ", i, ";"))
      period_type <- tmp[1,1]
      operator <- tmp[1,2]  #operator is necessary to deal differently with WSC locations, since HYDAT daily means take precedence over calculated ones.

      if (!is.null(start_recalc)){ #start_recalc is specified (not NULL)
        if (!is.na(earliest_day_measurements)){
          last_day_historic <- if (length(last_day_historic) > 0) min(earliest_day_measurements, (if (length(start_recalc) == 1) start_recalc else start_recalc[i])) else earliest_day_measurements #in case the user asked for a start prior to the actual record start, or if there is no record in calculated_daily yet
        } else {
          earliest_day_historic <-  as.Date(DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM calculated_daily WHERE timeseries_id = ", i, ";"))[1,])
          if (start_recalc < earliest_day_historic) {
            last_day_historic <- earliest_day_historic
          } else {
            last_day_historic <- start_recalc
          }
        }
      } else { #start_recalc is NULL
        if (!is.na(last_day_historic) & !is.na(earliest_day_measurements)){
          last_day_measurements <- as.Date(DBI::dbGetQuery(con, paste0("SELECT MAX(datetime) FROM measurements_continuous WHERE timeseries_id = ", i, ";"))[1,])
          last_day_historic <- last_day_historic - 2 # recalculate the last two days of historic data in case new data has come in
        } else if (is.na(last_day_historic) & !is.na(earliest_day_measurements)){ #say, a new timeseries that isn't in hydat yet or one that's just being added and has no calculations yet
          last_day_historic <- earliest_day_measurements
        } else { # a timeseries that is only in HYDAT, has no realtime measurements
          last_day_historic <- DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM calculated_daily WHERE timeseries_id = ", i, " AND max IS NULL;"))[1,]
        }
      }

      if (is.na(last_day_historic)){
        message("Skipping calculations for timeseries ", i, " as it looks like nothing needs to be calculated.")
        next()
      }

      missing_stats <- data.frame()
      flag <- FALSE  #This flag is in case there actually isn't an entry in hydat for the station yet. Rare case but it happens! Also is set if the timeseries recalculation isn't far enough in the past to overlap with HYDAT daily means.
      if (operator == "WSC" & (last_day_historic < Sys.Date()-30)){ #this will check to make sure that we're not overwriting HYDAT daily means with calculated realtime means
        tmp <- DBI::dbGetQuery(con, paste0("SELECT location, parameter FROM timeseries WHERE timeseries_id = ", i, ";"))
        hydat_con <- DBI::dbConnect(RSQLite::SQLite(), tidyhydat::hy_downloaded_db())
        if (tmp[, "parameter"] == "flow"){
          last_hydat_year <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (year) FROM DLY_FLOWS WHERE STATION_NUMBER = '", tmp[, "location"], "';"))[1,1]
          if (is.na(last_hydat_year)) {
            flag <- TRUE
          }
          if (!flag) {
            max_mth <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (month) FROM DLY_FLOWS WHERE STATION_NUMBER = '", tmp[, "location"], "' AND year = ", last_hydat_year, ";"))[1,1]
            last_hydat <- as.Date(paste0(last_hydat_year, "-", max_mth, if (max_mth %in% c(1,3,5,7,8,10,12))"-31" else if (max_mth %in% c(4,6,9,11)) "-30" else "28"), format = "%Y-%m-%d")
          }
        } else if (tmp[, "parameter"] == "level") {
          last_hydat_year <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (year) FROM DLY_LEVELS WHERE STATION_NUMBER = '", tmp[, "location"], "';"))[1,1]
          if (is.na(last_hydat_year)) {
            flag <- TRUE
          }
          if (!flag) {
            max_mth <- DBI::dbGetQuery(hydat_con, paste0("SELECT MAX (month) FROM DLY_LEVELS WHERE STATION_NUMBER = '", tmp[, "location"], "' AND year = ", last_hydat_year, ";"))[1,1]
            last_hydat <- as.Date(paste0(last_hydat_year, "-", max_mth, if (max_mth %in% c(1,3,5,7,8,10,12))"-31" else if (max_mth %in% c(4,6,9,11)) "-30" else "28"), format = "%Y-%m-%d")
          }
        }
        DBI::dbDisconnect(hydat_con)

        if (!flag){
          gap_measurements <- DBI::dbGetQuery(con, paste0("SELECT * FROM measurements_continuous WHERE timeseries_id = ", i, " AND datetime > '", last_hydat + 1, " 00:00:00'"))

          if (nrow(gap_measurements) > 0){ #Then there is new measurements data, or we're force-recalculating from an earlier date
            gap_measurements <- gap_measurements %>%
              dplyr::group_by(lubridate::year(.data$datetime), lubridate::yday(.data$datetime)) %>%
              dplyr::summarize(date = mean(lubridate::date(.data$datetime)),
                               value = if (period_type == "sum") sum(.data$value) else if (period_type == "median") stats::median(.data$value) else if (period_type == "min") min(.data$value) else if (period_type == "max") max(.data$value) else mean(.data$value),
                               grade = sort(.data$grade,decreasing=TRUE)[1],
                               approval = sort(.data$approval, decreasing=TRUE)[1],
                               imputed = sort(.data$imputed, decreasing = TRUE)[1],
                               .groups = "drop")
            gap_measurements <- gap_measurements[,c(3:7)]
            names(gap_measurements) <- c("date", "value", "grade", "approval", "imputed")

            if (!(last_hydat %in% gap_measurements$date)){ #Makes a row if there is no data for that day, this way stats will be calculated for that day later.
              gap_measurements <- rbind(gap_measurements, data.frame("date" = last_hydat, "value" = NA, "grade" = NA, "approval" = NA, "imputed" = FALSE))
            }

            if (last_day_historic < min(gap_measurements$date)){ #Because of the frequent gap between historical HYDAT database and realtime data and the fact that HYDAT daily means are directly appended to the calculated_daily table, it's possible that no realtime measurements exist between last_day_historic and the earliest measurement. In that case infill with calculated_daily values.
              backfill <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade, approval, imputed FROM calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(gap_measurements$date), "' AND date > '", last_day_historic, "';"))
              gap_measurements <- rbind(gap_measurements, backfill)
            }
            gap_measurements <- as.data.frame(fasstr::fill_missing_dates(gap_measurements, "date", pad_ends = FALSE)) #fills any missing dates with NAs, which will let them be filled later on when calculating stats.

            all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", i, " AND date <= '", last_hydat, "';"))
            #Need to rbind only the calculated daily means AFTER last_hydat
            all_stats <- rbind(all_stats, gap_measurements[gap_measurements$date >= last_hydat, c("date", "value")])
            missing_stats <- gap_measurements
          } else { #There is no new measurement data, but stats may still need to be calculated because of new HYDAT data
            missing_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade, approval, imputed FROM calculated_daily WHERE timeseries_id = ", i, " AND date >= '", last_day_historic, "';"))
            if (nrow(missing_stats) > 0){
              all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", i, ";"))
            }
          }
        }
      } else {
        flag <- TRUE
      }

      if (operator != "WSC" || flag) { #All timeseries where: operator is not WSC and therefore lack superseding daily means; isn't recalculating past enough to overlap HYDAT daily means; operator is WSC but there's no entry in HYDAT
        gap_measurements <- DBI::dbGetQuery(con, paste0("SELECT * FROM measurements_continuous WHERE timeseries_id = ", i, " AND datetime >= '", last_day_historic, " 00:00:00'"))

        if (nrow(gap_measurements) > 0){ #Then there is new measurements data, or we're force-recalculating from an earlier date perhaps due to updated HYDAT
          gap_measurements <- gap_measurements %>%
            dplyr::group_by(lubridate::year(.data$datetime), lubridate::yday(.data$datetime)) %>%
            dplyr::summarize(date = mean(lubridate::date(.data$datetime)),
                             value = if (period_type == "sum") sum(.data$value) else if (period_type == "median") stats::median(.data$value) else if (period_type == "min") min(.data$value) else if (period_type == "max") max(.data$value) else mean(.data$value),
                             grade = sort(.data$grade, decreasing = TRUE)[1],
                             approval = sort(.data$approval, decreasing = TRUE)[1],
                             imputed = sort(.data$imputed, decreasing = TRUE)[1],
                             .groups = "drop")
          gap_measurements <- gap_measurements[,c(3:7)]
          names(gap_measurements) <- c("date", "value", "grade", "approval", "imputed")

          if (!((last_day_historic) %in% gap_measurements$date)){ #Makes a row if there is no data for that day, this way stats will be calculated for that day later. Reminder that last_day_historic is 2 days *prior* to the last day for which there is a daily mean.
            gap_measurements <- rbind(gap_measurements, data.frame("date" = last_day_historic, "value" = NA, "grade" = NA, "approval" = NA, "imputed" = FALSE))
          }
          gap_measurements <- as.data.frame(fasstr::fill_missing_dates(gap_measurements, "date", pad_ends = FALSE)) #fills any missing dates with NAs, which will let them be filled later on when calculating stats.
          gap_measurements[is.na(gap_measurements$imputed) , "imputed"] <- FALSE

          all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(gap_measurements$date), "';"))
          all_stats <- rbind(all_stats, gap_measurements[, c("date", "value")])
          missing_stats <- gap_measurements
        } else { #There is no new measurement data, but stats may still need to be calculated
          missing_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value, grade, approval FROM calculated_daily WHERE timeseries_id = ", i, " AND date >= '", last_day_historic, "';"))
          if (nrow(missing_stats) > 0){
            all_stats <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", i, ";"))
          }
        }
      }


      # Now calculate stats where they are missing
      if (nrow(missing_stats) > 0){
        # Remove Feb. 29 data as it would mess with the percentiles; save the missing_stats ones and add them back in later. This is also important as it prevents deleting Feb 29 data in the calculated_daily table without replacing it.
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
        for (j in unique(missing_stats$dayofyear)){
          if (nrow(all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]) > 0){ #Check that there is at least some data for that doy. If not, no need to manipulate that doy further.
            earliest_full_stats <- lubridate::add_with_rollback(min(all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]$date), lubridate::years(1))
            first_instance <- min(all_stats[all_stats$dayofyear == j & !is.na(all_stats$value), ]$date)
            missing <- missing_stats[missing_stats$dayofyear == j & missing_stats$date >= earliest_full_stats , ]
            temp <- rbind(temp, missing)
            missing_first <- missing_stats[missing_stats$dayofyear ==j  & missing_stats$date == first_instance , ]
            first_instance_no_stats <- rbind(first_instance_no_stats, missing_first)
          }
        }
        missing_stats <- temp

        #Find the first Feb 29, and add it to first_instance_no_stats if there are no adjacent values that will get added in later
        if (nrow(feb_29) > 0){
          first_feb_29 <- feb_29[feb_29$date == min(feb_29$date) , ]
          if (!all(c((first_feb_29$date  - 1), (first_feb_29$date  + 1)) %in% missing_stats$date) & !is.na(first_feb_29$value)){ #if statement is FALSE, feb 29 will be dealt with later by getting the mean of the surrounding samples so don't add it to first_instance_no_stats so it isn't dealt with here
            feb_29 <- feb_29[!feb_29$date == first_feb_29$date , ]
            first_feb_29$dayofyear <- NA
            first_instance_no_stats <- rbind(first_instance_no_stats, first_feb_29[, c("date", "value", "grade", "approval", "dayofyear", "imputed")])
          }
        }

        if (nrow(first_instance_no_stats) > 0){ #Add a min and max for the first instance, delete + append, then remove it from missing_stats for calculations
          missing_stats <- missing_stats[!(missing_stats$date %in% first_instance_no_stats$date) , ]
          first_instance_no_stats <- first_instance_no_stats[ , !(names(first_instance_no_stats) == "dayofyear")]
          first_instance_no_stats$timeseries_id <- i
          first_instance_no_stats$max <- first_instance_no_stats$value
          first_instance_no_stats$min <- first_instance_no_stats$value
          first_instance_no_stats <- first_instance_no_stats[!is.na(first_instance_no_stats$value) , ]
          DBI::dbWithTransaction(
            con,
            {
              DBI::dbExecute(con, paste0("DELETE FROM calculated_daily WHERE timeseries_id = ", i, " AND date IN ('", paste(first_instance_no_stats$date, collapse = "', '"), "')"))
              DBI::dbAppendTable(con, "calculated_daily", first_instance_no_stats)
              if (nrow(missing_stats) == 0){  #If < 1 year of data exists, there might not be anything left in missing_stats but first instance data is still being appended.
                DBI::dbExecute(con, paste0("UPDATE timeseries SET last_daily_calculation = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", i, ";"))
              }
            }
          )
        }

        if (nrow(missing_stats) > 0){
          missing_stats$max <- missing_stats$min <- missing_stats$q90 <- missing_stats$q75 <- missing_stats$q50 <- missing_stats$q25 <- missing_stats$q10 <- missing_stats$percent_historic_range <- NA
          for (k in 1:nrow(missing_stats)){
            date <- missing_stats$date[k]
            doy <- missing_stats$dayofyear[k]
            current <- missing_stats$value[k]
            past <- all_stats[all_stats$dayofyear == doy & all_stats$date < date, "value"] #Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
            past <- past[!is.na(past)]
            if (length(past) >= 1){
              missing_stats[k, c("max", "min", "q90", "q75", "q50", "q25", "q10")] <- c(max(past), min(past), stats::quantile(past, c(0.90, 0.75, 0.50, 0.25, 0.10)))
              if (length(past) > 1 & !is.na(current)){ #need at least 2 measurements to calculate a percent historic, plus a current measurement!
                missing_stats[k, "percent_historic_range"] <- ((current - min(past)) / (max(past) - min(past))) * 100
              }
            }
          }
          missing_stats <- missing_stats[ , !(names(missing_stats) == "dayofyear")]
          missing_stats <- hablar::rationalize(missing_stats)

          #Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st.
          if (nrow(feb_29) > 0 & !(substr(as.character(as.Date(.POSIXct(Sys.time(), "UTC"))), 6, 10) %in% c("02-29", "03-01,", "03-02"))) {
            for (l in feb_29$date){
              before <- missing_stats[missing_stats$date == l - 1 , ]
              after <- missing_stats[missing_stats$date == l + 1 , ]
              if (nrow(before) == 0 | nrow(after) == 0) { #If TRUE then can't do anything except for passing the value forward
                if (is.na(feb_29[feb_29$date == l, "value"])){ #If there's no value and can't do anything else then drop the row
                  feb_29 <- feb_29[!feb_29$date == l ,]
                }
              } else {
                feb_29[feb_29$date == l, c("percent_historic_range", "max", "min", "q90", "q75", "q50", "q25", "q10")] <- c(mean(c(before$percent_historic_range, after$percent_historic_range)), mean(c(before$max, after$max)), mean(c(before$min, after$min)), mean(c(before$q90, after$q90)), mean(c(before$q75, after$q75)), mean(c(before$q50, after$q50)), mean(c(before$q25, after$q25)), mean(c(before$q10, after$q10)))
              }
            }
            feb_29 <- hablar::rationalize(feb_29)
            missing_stats <- rbind(missing_stats, feb_29)
          }
          missing_stats$timeseries_id <- i
        }
      }
    }, error = function(e) {
      warning("calculate_stats: failed to calculate stats for timeseries_id ", i, ".")
    }) #End of tryCatch for stats calculation

    if (nrow(missing_stats) > 0){ #This is separated from the calculation portion to allow for a tryCatch for calculation and appending, separately.
      tryCatch({
        # Construct the SQL DELETE query. This is done in a manner that can't delete rows where there are no stats even if they are between the start and end date of missing_stats.
        delete_query <- paste0("DELETE FROM calculated_daily WHERE timeseries_id = ", i, " AND date BETWEEN '", min(missing_stats$date), "' AND '", max(missing_stats$date), "'")
        remaining_dates <- as.Date(setdiff(seq.Date(min(as.Date(missing_stats$date)), max(as.Date(missing_stats$date)), by = "day"), as.Date(missing_stats$date)), origin = "1970-01-01")
        if (length(remaining_dates) > 0) {
          delete_query <- paste0(delete_query, " AND date NOT IN ('", paste(remaining_dates, collapse = "','"), "')")
        }

        DBI::dbWithTransaction(
          con,
          {
            DBI::dbExecute(con, delete_query)
            DBI::dbAppendTable(con, "calculated_daily", missing_stats) # Append the missing_stats data to the calculated_daily table
            DBI::dbExecute(con, paste0("UPDATE timeseries SET last_daily_calculation = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", i, ";"))
          }
        )
      }, error = function(e){
        warning("calculate_stats: failed to append new statistics for timeseries_id ", i, ".")
      }) #End of tryCatch for removing/adding to DB
    }

  } # End of for loop calculating means and stats for each station in timeseries table
}
