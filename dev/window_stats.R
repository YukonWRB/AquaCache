# Add columns to measurements_calculated_daily table to hold 30 year window stats
message(
  "Adding 30 year window statistics columns to measurements_calculated_daily table."
)

DBI::dbExecute(
  con,
  "ALTER TABLE continuous.timeseries ADD COLUMN IF NOT EXISTS historic_window_years INTEGER DEFAULT 30;"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.timeseries.historic_window_years IS 'Optional number of years to use when calculating windowed historical statistics (window_* columns) alongside the full-history metrics.';"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_percent_historic_range NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_max NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_min NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_mean NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q90 NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q75 NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q50 NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q25 NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q10 NUMERIC;"
)
DBI::dbExecute(
  con,
  "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_doy_count INTEGER;"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_percent_historic_range IS 'Percent of historical range for the same day of year using only the most recent years defined by timeseries.historic_window_years. Values are populated when a window is configured and enough historic points exist within that window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_max IS 'Historical max for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_min IS 'Historical min for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_mean IS 'Historical mean for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q50 IS 'Historical 50th quantile for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q75 IS 'Historical 75th quantile for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q90 IS 'Historical 90th quantile for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q25 IS 'Historical 25th quantile for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q10 IS 'Historical 10th quantile for the day of year within the configured recent-year window.';"
)
DBI::dbExecute(
  con,
  "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_doy_count IS 'Number of historical points available for the day of year within the configured recent-year window.';"
)


# calculate_stats function that had been modified to do window stats (not quite worked through)

#' Calculate daily means and statistics
#'
#' @description
#'
#' Calculates daily means from data in the measurements_continuous table as well as derived statistics for each day (historical min, max, q10, q25, q50 (mean), q75, q90), using the timezone specified in table 'timeseries' to define the start/end of days. Derived daily statistics are for each day of year **prior** to the current date for historical context, with the exception of the first day of record for which only the min/max are populated with the day's value. Any data graded as 'unusable' is excluded from calculations. February 29 calculations are handled differently: see details.
#'
#' When a timeseries includes a value for `historic_window_years`, the function also calculates "window" statistics (window_*) using only data within that many years of each date, while still maintaining the full-history statistics.
#'
#' Water Survey of Canada daily means are dealt with differently than other timeseries: daily means provided in the HYDAT database take precedence over means calculated from data in table 'measurements_continuous'.
#'
#' Some continuous measurement data may have a period of greater than 1 day. In these cases it would be impossible to calculate daily statistics, so this function explicitly excludes data points with a period greater than P1D.
#'
#' This function is meant to be called from within [dailyUpdate()], but is exported in case a need arises to calculate daily means and statistics in isolation or in another function. It *must* be used with a database created by this package, or one with identical tables.
#'
#' @details
#' Calculating daily statistics for February 29 is complicated: due to a paucity of data, this day's statistics are liable to be very mismatched from those of the preceding and succeeding days if calculated based only on Feb 29 data. Consequently, statistics for these days are computed by averaging those of Feb 28 and March 1, ensuring a smooth line when graphing mean/min/max/quantile parameters. This necessitates waiting for complete March 1st data, so Feb 29 means and stats will be delayed until March 2nd.
#'
#' @param con A connection to the database. If NULL, a connection will be created and closed by the function.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Specifying 'all' will work on all timeseries.
#' @param start_recalc The day on which to start daily calculations, as a vector of one element OR as NULL. If NULL will recalculate the last two days in case there is new realtime data, plus two days in the past to account for possible past calculations with incomplete data.
#'
#' @return Updated entries in the 'measurements_calculated_daily' table.
#' @export
#'

calculate_stats <- function(con = NULL, timeseries_id, start_recalc = NULL) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  # Date/datetime conversion functions
  # Convert a date at local midnight to UTC datetime string
  # @param day A Date object.
  # @param offset The timezone offset in hours from UTC.
  midnight_to_utc <- function(day, offset) {
    if (is.null(day) || length(day) == 0 || is.na(day)) {
      return(NA_character_)
    }
    utc_midnight <- as.POSIXct(
      paste0(as.character(day), " 00:00:00"),
      tz = "UTC"
    )
    if (is.na(utc_midnight)) {
      return(paste0(as.character(day), " 00:00:00"))
    }
    format(utc_midnight - lubridate::dhours(offset), "%Y-%m-%d %H:%M:%S")
  }

  # Convert a UTC datetime to a local date
  # @param datetime A POSIXct or POSIXlt object.
  # @param offset The timezone offset in hours from UTC.
  to_local_date <- function(datetime, offset) {
    if (is.null(datetime) || length(datetime) == 0) {
      return(as.Date(NA))
    }
    if (all(is.na(datetime))) {
      return(rep(as.Date(NA), length(datetime)))
    }
    if (!inherits(datetime, "POSIXt")) {
      datetime <- as.POSIXct(datetime, tz = "UTC")
    }
    as.Date(datetime + lubridate::dhours(offset))
  }

  # daily stats summary functions
  calc_daily_value <- function(values, aggregation_type) {
    if (all(is.na(values))) {
      return(NA_real_)
    }
    if (aggregation_type == "sum") {
      sum(values, na.rm = TRUE)
    } else if (aggregation_type == "median") {
      stats::median(values, na.rm = TRUE)
    } else if (aggregation_type == "min") {
      min(values, na.rm = TRUE)
    } else if (aggregation_type == "max") {
      max(values, na.rm = TRUE)
    } else if (aggregation_type == "mean") {
      mean(values, na.rm = TRUE)
    } else if (aggregation_type == "(min+max)/2") {
      mean(
        c(
          min(values, na.rm = TRUE),
          max(values, na.rm = TRUE)
        ),
        na.rm = TRUE
      )
    } else if (aggregation_type == "instantaneous") {
      mean(values, na.rm = TRUE)
    } else {
      mean(values, na.rm = TRUE)
    }
  }
  summarize_measurements <- function(measurements, aggregation_type, offset) {
    if (nrow(measurements) == 0) {
      return(data.frame(
        date = as.Date(character()),
        value = numeric(),
        imputed = logical()
      ))
    }
    localised <- measurements %>%
      dplyr::mutate(
        datetime = as.POSIXct(.data$datetime, tz = "UTC"),
        .local_datetime = .data$datetime + lubridate::dhours(offset),
        .local_date = as.Date(.data$.local_datetime)
      )
    aggregated <- localised %>%
      dplyr::group_by(.data$.local_date) %>%
      dplyr::summarize(
        date = dplyr::first(.data$.local_date),
        value = calc_daily_value(.data$value, aggregation_type),
        imputed = any(.data$imputed, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::select("date", "value", "imputed") %>%
      dplyr::arrange(.data$date)
    aggregated$imputed[is.na(aggregated$imputed)] <- FALSE
    as.data.frame(aggregated)
  }

  # helper to determine the last date available in HYDAT for a station
  get_last_hydat_date <- function(hcon, station, param) {
    if (param == "flow") {
      last_year <- DBI::dbGetQuery(
        hcon,
        sprintf(
          "SELECT MAX(year) FROM DLY_FLOWS WHERE STATION_NUMBER = '%s';",
          station
        )
      )[1, 1]
      if (is.na(last_year)) {
        return(NA)
      }
      dat <- tidyhydat::hy_daily_flows(
        station,
        start_date = paste0(last_year, "-01-01"),
        end_date = paste0(last_year, "-12-31")
      )
    } else {
      last_year <- DBI::dbGetQuery(
        hcon,
        sprintf(
          "SELECT MAX(year) FROM DLY_LEVELS WHERE STATION_NUMBER = '%s';",
          station
        )
      )[1, 1]
      if (is.na(last_year)) {
        return(NA)
      }
      dat <- tidyhydat::hy_daily_levels(
        station,
        start_date = paste0(last_year, "-01-01"),
        end_date = paste0(last_year, "-12-31")
      )
    }

    if (nrow(dat) == 0) {
      return(NA)
    }
    max(as.Date(dat$Date))
  }

  if (!is.null(start_recalc)) {
    if (length(start_recalc) != 1) {
      stop(
        "It looks like you're trying to specify a start date for recalculations, this has to be a vector of length 1."
      )
    }
    if (!inherits(start_recalc, "Date")) start_recalc <- as.Date(start_recalc)
  }

  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT timeseries_id FROM timeseries WHERE record_rate <= '1 day';"
      )
    )
    timeseries_id <- all_timeseries$timeseries_id
  } else {
    all_timeseries <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT timeseries_id FROM timeseries WHERE timeseries_id IN ('",
        paste(timeseries_id, collapse = "', '"),
        "') AND record_rate <= '1 day';"
      )
    )
    if (nrow(all_timeseries) == 0) {
      stop(
        "Calculations are not possible. Perhaps the timeseries_id you specified are not in table timeseries or have a record_rate of greater than 1 day."
      )
    }
    if (length(timeseries_id) != length(all_timeseries$timeseries_id)) {
      warning(
        "At least one of the timeseries_id you specified had a recording rate greater than 1 day or could not be found in the database."
      )
    }
    timeseries_id <- all_timeseries$timeseries_id
  }

  # calculate daily means or sums for any days without them
  years <- 1800:2100
  leap_list <- years[
    (years %% 400 == 0) | (years %% 4 == 0 & years %% 100 != 0)
  ]
  hydat_checked <- FALSE
  for (i in timeseries_id) {
    # Fetch the grades to disregard 'unusable' data later
    grades_dt <- dbGetQueryDT(
      con,
      paste0(
        "SELECT g.start_dt, g.end_dt FROM grades g LEFT JOIN grade_types gt ON g.grade_type_id = gt.grade_type_id WHERE g.timeseries_id = ",
        i,
        " AND gt.grade_type_code = 'N' ORDER BY start_dt;"
      )
    )
    # Drop rows where start_dt and end_dt are the same
    grades_dt <- grades_dt[!(grades_dt$start_dt == grades_dt$end_dt), ]

    if (nrow(grades_dt) != 0) {
      unusable <- TRUE
      # Make the SQL for the exclusions as it's reused later
      exclusions <- sprintf(
        "datetime NOT BETWEEN '%s' AND '%s'",
        grades_dt$start_dt,
        grades_dt$end_dt
      )
      exclusions <- paste(exclusions, collapse = " AND ")
    } else {
      unusable <- FALSE
      exclusions <- "1=1" # Dummy condition that is always TRUE
    }

    start_recalc_i <- start_recalc
    skip <- FALSE
    tryCatch(
      {
        tmp <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT at.aggregation_type, t.source_fx, t.timezone_daily_calc, t.historic_window_years FROM timeseries t JOIN aggregation_types at ON t.aggregation_type_id = at.aggregation_type_id WHERE timeseries_id = ",
            i,
            ";"
          )
        )
        aggregation_type <- tmp[1, 1] # Daily values are calculated differently depending on the period type
        source_fx <- tmp[1, 2] # source_fx is necessary to deal differently with WSC locations, since HYDAT daily means take precedence over calculated ones.
        daily_offset <- tmp[1, 3]
        historic_window_years <- suppressWarnings(as.numeric(tmp[1, 4]))
        window_configured <- !is.na(historic_window_years) &&
          historic_window_years > 0

        # error catching for calculating stats; another one later for appending to the DB
        last_day_historic <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT MAX(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
            i,
            " AND max IS NOT NULL;"
          )
        )[1, 1]
        if (is.na(last_day_historic)) {
          last_day_historic <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
              i,
              ";"
            )
          )[1, 1]
        }
        earliest_day_historic <- as.Date(DBI::dbGetQuery(
          con,
          paste0(
            "SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
            i,
            " AND max IS NOT NULL;"
          )
        )[1, 1])
        if (is.na(earliest_day_historic)) {
          earliest_day_historic <- as.Date(DBI::dbGetQuery(
            con,
            paste0(
              "SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
              i,
              ";"
            )
          )[1, 1])
        }
        # Find the earliest datetime in the measurements_continuous table, without considering unusable data
        if (unusable) {
          # There is unusable data to exclude
          query <- sprintf(
            "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = %s AND period <= 'P1D' AND %s",
            i,
            exclusions
          )
          earliest_day_measurements <- to_local_date(
            DBI::dbGetQuery(con, query)[1, 1],
            daily_offset
          )
        } else {
          # No unusable data to exclude
          earliest_day_measurements <- to_local_date(
            DBI::dbGetQuery(
              con,
              paste0(
                "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ",
                i,
                " AND period <= 'P1D';"
              )
            )[1, 1],
            daily_offset
          )
        }

        # Below lines deal with timeseries that don't have daily values but should, or don't have them far enough in the past. We check if it's necessary to calculate further back in time than start_recalc_i.
        if (is.na(earliest_day_historic)) {
          # This means that no daily values have been calculated yet, or that they've been completely deleted from the database. In that case we start recalculation straight from the earliest measurement date.
          earliest_day_historic <- earliest_day_measurements
          last_day_historic <- earliest_day_measurements
          start_recalc_i <- min(earliest_day_measurements, start_recalc_i)
        } else if (is.na(earliest_day_measurements)) {
          # In this case there are no realtime measurements but there are calculated daily values
          if (last_day_historic < Sys.Date() - 30) {
            # If this is the case there should be no data to recalculate anymore unless it's been a long time since the last calc or they've been deleted for some reason.
            start_recalc_i <- min(last_day_historic, start_recalc_i)
          } else {
            start_recalc_i <- min(Sys.Date() - 2, start_recalc_i) # recalculate the last two days of historic data in case new data has come in
          }
        } else if (earliest_day_measurements < earliest_day_historic) {
          # If we got to here there are measurements and daily values. Check if measurements start earlier than daily values.
          start_recalc_i <- earliest_day_measurements
        }

        if (!is.null(start_recalc_i)) {
          #start_recalc_i is specified (not NULL)
          if (!is.na(earliest_day_measurements)) {
            # If there are measurements, then we can calculate from the earliest measurement date
            if (earliest_day_historic < earliest_day_measurements) {
              last_day_historic <- max(earliest_day_historic, start_recalc_i)
            } else {
              last_day_historic <- if (length(last_day_historic) > 0) {
                max(earliest_day_measurements, start_recalc_i)
              } else {
                earliest_day_measurements
              } # in case the user asked for a start prior to the actual record start, or if there is no record in measurements_calculated_daily yet
            }
          } else {
            # There are no measurements, so we can only calculate from the earliest day in the measurements_calculated_daily table
            last_day_historic <- earliest_day_historic
          }
        } else {
          # start_recalc_i is NULL so let's find out when to start recalculating
          if (!is.na(last_day_historic) & !is.na(earliest_day_measurements)) {
            last_day_historic <- last_day_historic - 2 # recalculate the last two days of historic data in case new data has come in
          } else if (
            is.na(last_day_historic) & !is.na(earliest_day_measurements)
          ) {
            # say, a new timeseries that isn't in hydat yet or one that's just being added and has no calculations yet
            last_day_historic <- earliest_day_measurements
          } else {
            # a timeseries that is only in HYDAT, has no realtime measurements
            last_day_historic <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
                i,
                " AND max IS NULL;"
              )
            )[1, 1]
          }
        }

        if (is.na(last_day_historic)) {
          skip <- TRUE
        }

        if (!skip) {
          # Check if any corrections have been made to the timeseries during the computation time. If not, save time and computations by getting values straight from measurements_continuous instead of the corrected tables.
          correction_cutoff <- midnight_to_utc(last_day_historic, daily_offset)
          if (is.na(correction_cutoff)) {
            correction_cutoff <- paste0(last_day_historic, " 00:00:00")
          }
          corrections_apply <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT correction_id FROM corrections WHERE timeseries_id = ",
              i,
              " AND end_dt > '",
              correction_cutoff,
              "' LIMIT 1;"
            )
          )
          if (nrow(corrections_apply) == 1) {
            corrections_apply <- TRUE
          } else {
            corrections_apply <- FALSE
          }

          missing_stats <- data.frame()
          flag <- FALSE # This flag is set to TRUE in cases where there isn't an entry in hydat for the station yet. Rare case but it happens! Also is set TRUE if the timeseries recalculation isn't far enough in the past to overlap with HYDAT daily means, or if it's WSC data that's not level or flow.
          if (is.na(source_fx)) {
            source_fx <- "NA"
          }
          if (
            (source_fx == "downloadWSC") & (last_day_historic < Sys.Date() - 30)
          ) {
            # this will check to make sure that we're not overwriting HYDAT daily means with calculated realtime means
            # Check to make sure HYDAT is installed and up to date
            if (!hydat_checked) {
              hydat_check(silent = TRUE)
              hydat_checked <- TRUE
            }
            tmp <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT l.location, t.parameter_id, p.param_name FROM timeseries AS t JOIN locations AS l ON t.location_id = l.location_id JOIN parameters AS p ON t.parameter_id = p.parameter_id WHERE t.timeseries_id = ",
                i,
                ";"
              )
            )
            hydat_con <- DBI::dbConnect(
              RSQLite::SQLite(),
              tidyhydat::hy_downloaded_db()
            )
            if (tmp[, "param_name"] %in% c("flow", "water level")) {
              last_hydat <- get_last_hydat_date(
                hydat_con,
                tmp[, "location"],
                tmp[, "param_name"]
              )
              if (is.na(last_hydat) || last_day_historic > last_hydat) {
                flag <- TRUE
              }
            } else {
              flag <- TRUE
            }
            DBI::dbDisconnect(hydat_con)

            if (!flag) {
              start_hydat_date <- last_hydat + 1
              start_hydat_ts <- midnight_to_utc(start_hydat_date, daily_offset)
              if (is.na(start_hydat_ts)) {
                start_hydat_ts <- paste0(start_hydat_date, " 00:00:00")
              }
              if (corrections_apply) {
                if (unusable) {
                  query <- paste0(
                    "SELECT datetime, value_corrected AS value, imputed FROM measurements_continuous_corrected WHERE timeseries_id = ",
                    i,
                    " AND datetime >= '",
                    start_hydat_ts,
                    "' AND period <= 'P1D' AND ",
                    exclusions,
                    ";"
                  )
                  gap_measurements <- DBI::dbGetQuery(con, query)
                } else {
                  gap_measurements <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT datetime, value_corrected AS value, imputed FROM measurements_continuous_corrected WHERE timeseries_id = ",
                      i,
                      " AND datetime >= '",
                      start_hydat_ts,
                      "' AND period <= 'P1D'"
                    )
                  )
                }
              } else {
                if (unusable) {
                  query <- paste0(
                    "SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ",
                    i,
                    " AND datetime >= '",
                    start_hydat_ts,
                    "' AND period <= 'P1D' AND ",
                    exclusions,
                    ";"
                  )
                  gap_measurements <- DBI::dbGetQuery(con, query)
                } else {
                  gap_measurements <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ",
                      i,
                      " AND datetime >= '",
                      start_hydat_ts,
                      "' AND period <= 'P1D'"
                    )
                  )
                }
              }

              if (nrow(gap_measurements) > 0) {
                # Then there is new measurements data, or we're force-recalculating from an earlier date
                gap_measurements <- summarize_measurements(
                  gap_measurements,
                  aggregation_type,
                  daily_offset
                )

                if (!((last_hydat + 1) %in% gap_measurements$date)) {
                  # Makes a row if there is no data for that day, this way stats will be calculated for that day later.
                  gap_measurements <- rbind(
                    gap_measurements,
                    data.frame(
                      "date" = last_hydat + 1,
                      "value" = NA,
                      "imputed" = FALSE
                    )
                  )
                }

                if (last_day_historic < min(gap_measurements$date)) {
                  # Because of the frequent gap between historical HYDAT database and realtime data and the fact that HYDAT daily means are directly appended to the measurements_calculated_daily table, it's possible that no realtime measurements exist between last_day_historic and the earliest measurement. In that case infill with HYDAT values where they exist, taking from the database first for any imputed values and then directly from HYDAT.

                  backfill_imputed <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT date, value, imputed FROM measurements_calculated_daily WHERE timeseries_id = ",
                      i,
                      " AND date < '",
                      min(gap_measurements$date),
                      "' AND date >= '",
                      last_day_historic,
                      "' AND imputed IS TRUE AND value IS NOT NULL;"
                    )
                  )

                  backfill <- if (tmp[, "param_name"] == "flow") {
                    as.data.frame(tidyhydat::hy_daily_flows(
                      tmp[, "location"],
                      start_date = last_day_historic,
                      end_date = min(gap_measurements$date) - 1
                    ))
                  } else {
                    as.data.frame(tidyhydat::hy_daily_levels(
                      tmp[, "location"],
                      start_date = last_day_historic,
                      end_date = min(gap_measurements$date) - 1
                    ))
                  }
                  backfill <- backfill[, c("Date", "Value")]
                  names(backfill) <- c("date", "value")
                  backfill <- backfill[!is.na(backfill$value), ]
                  backfill$imputed <- FALSE

                  # Remove any entries with values that are already in backfill and not NA, even if they've been imputed
                  backfill_imputed <- backfill_imputed[
                    !backfill_imputed$date %in%
                      backfill[!is.na(backfill$value), "date"],
                  ]
                  # Remove any entries in backfill_imputed that are already in backfill but are NA
                  backfill <- backfill[
                    !backfill$date %in% backfill_imputed$date,
                  ]
                  backfill <- rbind(backfill, backfill_imputed)

                  gap_measurements <- rbind(gap_measurements, backfill)
                }

                # Fill in any missing dates so that they get calculated values where possible
                full_dates <- data.frame(
                  "date" = seq.Date(
                    min(gap_measurements$date),
                    max(gap_measurements$date),
                    by = "1 day"
                  )
                )
                gap_measurements <- merge(
                  gap_measurements,
                  full_dates,
                  by = "date",
                  all = TRUE
                )

                gap_measurements[
                  is.na(gap_measurements$imputed),
                  "imputed"
                ] <- FALSE

                all_stats <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT date, value FROM measurements_calculated_daily WHERE timeseries_id = ",
                    i,
                    " AND date < '",
                    last_hydat,
                    "';"
                  )
                )
                # Need to rbind only the calculated daily means AFTER last_hydat
                all_stats <- rbind(
                  all_stats,
                  gap_measurements[
                    gap_measurements$date >= last_hydat,
                    c("date", "value")
                  ]
                )
                missing_stats <- gap_measurements
              } else {
                # There is no new measurement data, but stats may still need to be calculated because of new HYDAT data

                all_imputed <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT date, value, imputed FROM measurements_calculated_daily WHERE timeseries_id = ",
                    i,
                    " AND imputed IS TRUE AND value IS NOT NULL;"
                  )
                )

                all_hydat <- if (tmp[, "param_name"] == "flow") {
                  as.data.frame(tidyhydat::hy_daily_flows(tmp[, "location"]))
                } else {
                  as.data.frame(tidyhydat::hy_daily_levels(tmp[, "location"]))
                }
                all_hydat <- all_hydat[, c("Date", "Value")]
                names(all_hydat) <- c("date", "value")
                all_hydat <- all_hydat[!is.na(all_hydat$value), ]
                all_hydat$imputed <- FALSE

                # Remove any entries with values that are already in all_hydat and not NA, even if they've been imputed
                all_imputed <- all_imputed[
                  !all_imputed$date %in%
                    all_hydat[!is.na(all_hydat$value), "date"],
                ]
                # Remove any entries in all_imputed that are already in all_hydat but are NA
                all_hydat <- all_hydat[!all_hydat$date %in% all_imputed$date, ]

                all <- rbind(all_hydat, all_imputed)

                missing_stats <- all[all$date >= last_day_historic, ]

                if (nrow(missing_stats) > 0) {
                  all_stats <- all[, c("date", "value")]
                }
              }
            }
          } else {
            flag <- TRUE
          }

          if (!(source_fx == "downloadWSC") || flag) {
            # All timeseries where: operator is not WSC and therefore lacks superseding daily means; isn't recalculating past enough to overlap HYDAT daily means; operator is WSC but there's no entry in HYDAT
            recalc_start_ts <- midnight_to_utc(last_day_historic, daily_offset)
            if (is.na(recalc_start_ts)) {
              recalc_start_ts <- paste0(last_day_historic, " 00:00:00")
            }
            if (corrections_apply) {
              if (unusable) {
                query <- paste0(
                  "SELECT datetime, value_corrected AS value, imputed FROM measurements_continuous_corrected WHERE timeseries_id = ",
                  i,
                  " AND datetime >= '",
                  recalc_start_ts,
                  "' AND period <= 'P1D' AND ",
                  exclusions,
                  ";"
                )
                gap_measurements <- DBI::dbGetQuery(con, query)
              } else {
                gap_measurements <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT datetime, value_corrected AS value, imputed FROM measurements_continuous_corrected WHERE timeseries_id = ",
                    i,
                    " AND datetime >= '",
                    recalc_start_ts,
                    "' AND period <= 'P1D'"
                  )
                )
              }
            } else {
              if (unusable) {
                query <- paste0(
                  "SELECT datetime, value AS value, imputed FROM measurements_continuous WHERE timeseries_id = ",
                  i,
                  " AND datetime >= '",
                  recalc_start_ts,
                  "' AND period <= 'P1D' AND ",
                  exclusions,
                  ";"
                )
                gap_measurements <- DBI::dbGetQuery(con, query)
              } else {
                gap_measurements <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT datetime, value, imputed FROM measurements_continuous WHERE timeseries_id = ",
                    i,
                    " AND datetime >= '",
                    recalc_start_ts,
                    "' AND period <= 'P1D'"
                  )
                )
              }
            }

            if (nrow(gap_measurements) > 0) {
              # Then there is new measurements data, or we're force-recalculating from an earlier date perhaps due to updated HYDAT
              gap_measurements <- summarize_measurements(
                gap_measurements,
                aggregation_type,
                daily_offset
              )

              if (!((last_day_historic + 1) %in% gap_measurements$date)) {
                # Makes a row if there is no data for that day, this way stats will be calculated for that day later. Reminder that last_day_historic is 2 days *prior* to the last day for which there is a daily mean.
                gap_measurements <- rbind(
                  gap_measurements,
                  data.frame(
                    "date" = last_day_historic + 1,
                    "value" = NA,
                    "imputed" = FALSE
                  )
                )
              }
              # Fill in any missing dates so that they get calculated values where possible
              full_dates <- data.frame(
                "date" = seq.Date(
                  min(gap_measurements$date),
                  max(gap_measurements$date),
                  by = "1 day"
                )
              )
              gap_measurements <- merge(
                gap_measurements,
                full_dates,
                by = "date",
                all = TRUE
              )

              gap_measurements[
                is.na(gap_measurements$imputed),
                "imputed"
              ] <- FALSE

              all_stats <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT date, value FROM measurements_calculated_daily WHERE timeseries_id = ",
                  i,
                  " AND date < '",
                  min(gap_measurements$date),
                  "';"
                )
              )
              all_stats <- rbind(
                all_stats,
                gap_measurements[, c("date", "value")]
              )
              missing_stats <- gap_measurements
            } else {
              # There is no new measurement data, but stats may still need to be calculated
              missing_stats <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT date, value, imputed FROM measurements_calculated_daily WHERE timeseries_id = ",
                  i,
                  " AND date >= '",
                  last_day_historic,
                  "';"
                )
              )
              if (nrow(missing_stats) > 0) {
                all_stats <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT date, value FROM measurements_calculated_daily WHERE timeseries_id = ",
                    i,
                    ";"
                  )
                )
              }
            }
          }

          # Now calculate stats where they are missing
          if (nrow(missing_stats) > 0) {
            # Remove Feb. 29 data as it would mess with the percentiles; save the missing_stats ones and add them back in later. This is also important as it prevents deleting Feb 29 data in the measurements_calculated_daily table without replacing it.
            feb_29 <- missing_stats[
              (lubridate::month(missing_stats$date) == "2" &
                lubridate::mday(missing_stats$date) == "29"),
              ,
              drop = FALSE
            ]
            missing_stats <- missing_stats[
              !(lubridate::month(missing_stats$date) == "2" &
                lubridate::mday(missing_stats$date) == "29"),
              ,
              drop = FALSE
            ]
            all_stats <- all_stats[
              !(lubridate::month(all_stats$date) == "2" &
                lubridate::mday(all_stats$date) == "29"),
              ,
              drop = FALSE
            ]
            # Create a dayofyear column that pretends Feb 29 doesn't exist; all years have 365 days
            missing_stats$dayofyear <- ifelse(
              lubridate::year(missing_stats$date) %in% leap_list,
              ifelse(
                lubridate::month(missing_stats$date) <= 2,
                lubridate::yday(missing_stats$date),
                lubridate::yday(missing_stats$date) - 1
              ),
              lubridate::yday(missing_stats$date)
            )
            all_stats$dayofyear <- ifelse(
              lubridate::year(all_stats$date) %in% leap_list,
              ifelse(
                lubridate::month(all_stats$date) <= 2,
                lubridate::yday(all_stats$date),
                lubridate::yday(all_stats$date) - 1
              ),
              lubridate::yday(all_stats$date)
            )
            # select only records beginning with the second dayofyear and having values for the second time from all_stats (those for which stats can be calculated). Selects valid rows even if there is no current value, ensuring complete plotting parameters.
            temp <- data.frame()
            first_instance_no_stats <- data.frame()
            for (j in unique(missing_stats$dayofyear)) {
              if (
                nrow(all_stats[
                  all_stats$dayofyear == j & !is.na(all_stats$value),
                ]) >
                  0
              ) {
                # Check that there is at least some data for that doy. If not, no need to manipulate that doy further.
                earliest_full_stats <- lubridate::add_with_rollback(
                  min(
                    all_stats[
                      all_stats$dayofyear == j & !is.na(all_stats$value),
                    ]$date
                  ),
                  lubridate::years(1)
                )
                first_instance <- min(
                  all_stats[
                    all_stats$dayofyear == j & !is.na(all_stats$value),
                  ]$date
                )
                missing <- missing_stats[
                  missing_stats$dayofyear == j &
                    missing_stats$date >= earliest_full_stats,
                ]
                temp <- rbind(temp, missing)
                missing_first <- missing_stats[
                  missing_stats$dayofyear == j &
                    missing_stats$date == first_instance,
                ]
                first_instance_no_stats <- rbind(
                  first_instance_no_stats,
                  missing_first
                )
              }
            }
            missing_stats <- temp

            # Find the first Feb 29, and add it to first_instance_no_stats if there are no adjacent values that will get added in later
            if (nrow(feb_29) > 0) {
              first_feb_29 <- feb_29[feb_29$date == min(feb_29$date), ]
              if (
                !all(
                  c((first_feb_29$date - 1), (first_feb_29$date + 1)) %in%
                    missing_stats$date
                ) &
                  !is.na(first_feb_29$value)
              ) {
                # if statement is FALSE, feb 29 will be dealt with later by getting the mean of the surrounding samples so don't add it to first_instance_no_stats so it isn't dealt with here
                feb_29 <- feb_29[feb_29$date != first_feb_29$date, ]
                first_feb_29$dayofyear <- NA
                first_instance_no_stats <- rbind(
                  first_instance_no_stats,
                  first_feb_29[, c("date", "value", "dayofyear", "imputed")]
                )
              }
            }

            if (nrow(first_instance_no_stats) > 0) {
              # Add a min and max for the first instance, delete + append, then remove it from missing_stats for calculations
              missing_stats <- missing_stats[
                !(missing_stats$date %in% first_instance_no_stats$date),
              ]
              first_instance_no_stats <- first_instance_no_stats[
                !is.na(first_instance_no_stats$value),
              ]
              first_instance_no_stats <- first_instance_no_stats[,
                !(names(first_instance_no_stats) == "dayofyear")
              ]
              first_instance_no_stats$timeseries_id <- i
              first_instance_no_stats$max <- first_instance_no_stats$min <- first_instance_no_stats$value
              first_instance_no_stats$doy_count <- 1

              first_instance_no_stats$window_percent_historic_range <- NA_real_
              first_instance_no_stats$window_max <- NA_real_
              first_instance_no_stats$window_min <- NA_real_
              first_instance_no_stats$window_mean <- NA_real_
              first_instance_no_stats$window_q90 <- NA_real_
              first_instance_no_stats$window_q75 <- NA_real_
              first_instance_no_stats$window_q50 <- NA_real_
              first_instance_no_stats$window_q25 <- NA_real_
              first_instance_no_stats$window_q10 <- NA_real_
              first_instance_no_stats$window_doy_count <- NA_integer_

              # Now commit the changes to the database
              commit_fx1 <- function(
                con,
                i,
                first_instance_no_stats,
                missing_stats
              ) {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM measurements_calculated_daily WHERE timeseries_id = ",
                    i,
                    " AND date IN ('",
                    paste(first_instance_no_stats$date, collapse = "', '"),
                    "')"
                  )
                )
                DBI::dbAppendTable(
                  con,
                  "measurements_calculated_daily",
                  first_instance_no_stats
                )
                if (nrow(missing_stats) == 0) {
                  # If < 1 year of data exists, there might not be anything left in missing_stats but first instance data is still being appended.
                  DBI::dbExecute(
                    con,
                    paste0(
                      "UPDATE timeseries SET last_daily_calculation = '",
                      .POSIXct(Sys.time(), "UTC"),
                      "' WHERE timeseries_id = ",
                      i,
                      ";"
                    )
                  )
                }
              }

              active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function
              if (active) {
                tryCatch(
                  {
                    commit_fx1(con, i, first_instance_no_stats, missing_stats)
                    DBI::dbExecute(con, "COMMIT;")
                  },
                  error = function(e) {
                    DBI::dbExecute(con, "ROLLBACK;")
                  }
                )
              } else {
                # we're already in a transaction
                commit_fx1(con, i, first_instance_no_stats, missing_stats)
              }
            } # End of dealing with first instance data

            if (nrow(missing_stats) > 0) {
              # Calculate statistics for each day
              missing_stats <- data.table::setDT(missing_stats)
              for (k in 1:nrow(missing_stats)) {
                int <- as.integer(k)
                date <- missing_stats$date[int]
                doy <- missing_stats$dayofyear[int]
                past <- all_stats[
                  all_stats$dayofyear == doy & all_stats$date < date,
                  "value"
                ] # Importantly, does NOT include the current measurement. A current measure greater than past maximum will rank > 100%
                past <- past[!is.na(past)]

                past_window <- past
                if (window_configured) {
                  window_start <- lubridate::add_with_rollback(
                    # Using add_with_rollback to avoid issues with leap years
                    date,
                    lubridate::years(-historic_window_years)
                  )
                  past_window <- all_stats[
                    all_stats$dayofyear == doy &
                      all_stats$date < date &
                      all_stats$date >= window_start,
                    "value"
                  ]
                  past_window <- past_window[!is.na(past_window)]
                }

                if (length(past) >= 1) {
                  current <- missing_stats$value[int]
                  min <- min(past)
                  max <- max(past)
                  values <- c(
                    list("max" = max, "min" = min, "mean" = mean(past)),
                    as.list(stats::quantile(
                      past,
                      c(0.90, 0.75, 0.50, 0.25, 0.10),
                      names = FALSE
                    )),
                    "doy_count" = if (!is.na(current)) {
                      length(past) + 1
                    } else {
                      length(past)
                    }
                  )

                  window_values <- rep(NA_real_, 9)
                  if (window_configured && length(past_window) >= 1) {
                    window_current <- missing_stats$value[int]
                    window_min <- min(past_window)
                    window_max <- max(past_window)
                    window_values <- c(
                      list(
                        "window_max" = window_max,
                        "window_min" = window_min,
                        "window_mean" = mean(past_window)
                      ),
                      as.list(stats::quantile(
                        past_window,
                        c(0.90, 0.75, 0.50, 0.25, 0.10),
                        names = FALSE
                      )),
                      "window_doy_count" = if (!is.na(window_current)) {
                        length(past_window) + 1
                      } else {
                        length(past_window)
                      }
                    )
                  }

                  data.table::set(
                    missing_stats,
                    i = int,
                    j = c(
                      "max",
                      "min",
                      "mean",
                      "q90",
                      "q75",
                      "q50",
                      "q25",
                      "q10",
                      "doy_count",
                      "window_max",
                      "window_min",
                      "window_mean",
                      "window_q90",
                      "window_q75",
                      "window_q50",
                      "window_q25",
                      "window_q10",
                      "window_doy_count"
                    ),
                    value = c(values, window_values)
                  )
                  if (length(past) > 1 & !is.na(current)) {
                    # need at least 2 measurements to calculate a percent historic, plus a current measurement!
                    data.table::set(
                      missing_stats,
                      i = int,
                      j = "percent_historic_range",
                      value = ((current - min) / (max - min)) * 100
                    )
                  }
                  if (
                    window_configured &&
                      length(past_window) > 1 &
                      !is.na(current)
                  ) {
                    data.table::set(
                      missing_stats,
                      i = int,
                      j = "window_percent_historic_range",
                      value = ((current - window_min) /
                        (window_max - window_min)) *
                        100
                    )
                  }
                }
              }
              data.table::set(missing_stats, j = "dayofyear", value = NULL)

              # Assign values to Feb 29 that are between Feb 28 and March 1. Doesn't run on the 29, 1st, or 2nd to wait for complete stats on the 1st.
              if (
                nrow(feb_29) > 0 &
                  !(substr(
                    as.character(as.Date(.POSIXct(Sys.time(), "UTC"))),
                    6,
                    10
                  ) %in%
                    c("02-29", "03-01", "03-02"))
              ) {
                for (l in feb_29$date) {
                  before <- missing_stats[missing_stats$date == l - 1, ]
                  after <- missing_stats[missing_stats$date == l + 1, ]
                  if (nrow(before) == 0 & nrow(after) == 0) {
                    # If TRUE then can't do anything except for passing the value forward
                    if (is.na(feb_29[feb_29$date == l, "value"])) {
                      # If there's no value and can't do anything else then drop the row
                      feb_29 <- feb_29[feb_29$date != l, ]
                    }
                  } else {
                    feb_29[
                      feb_29$date == l,
                      c(
                        "percent_historic_range",
                        "max",
                        "min",
                        "q90",
                        "q75",
                        "q50",
                        "q25",
                        "q10",
                        "mean",
                        "doy_count",
                        "window_percent_historic_range",
                        "window_max",
                        "window_min",
                        "window_q90",
                        "window_q75",
                        "window_q50",
                        "window_q25",
                        "window_q10",
                        "window_mean",
                        "window_doy_count"
                      )
                    ] <- suppressWarnings(c(
                      mean(c(
                        before$percent_historic_range,
                        after$percent_historic_range
                      )),
                      mean(c(before$max, after$max)),
                      mean(c(before$min, after$min)),
                      mean(c(before$q90, after$q90)),
                      mean(c(before$q75, after$q75)),
                      mean(c(before$q50, after$q50)),
                      mean(c(before$q25, after$q25)),
                      mean(c(before$q10, after$q10)),
                      mean(c(before$mean, after$mean)),
                      min(c(before$doy_count, after$doy_count)),
                      mean(c(
                        before$window_percent_historic_range,
                        after$window_percent_historic_range
                      )),
                      mean(c(before$window_max, after$window_max)),
                      mean(c(before$window_min, after$window_min)),
                      mean(c(before$window_q90, after$window_q90)),
                      mean(c(before$window_q75, after$window_q75)),
                      mean(c(before$window_q50, after$window_q50)),
                      mean(c(before$window_q25, after$window_q25)),
                      mean(c(before$window_q10, after$window_q10)),
                      mean(c(before$window_mean, after$window_mean)),
                      min(c(before$window_doy_count, after$window_doy_count))
                    )) # warnings suppressed because of the possibility of NA values
                  }
                }
                feb_29 <- inf_to_na(feb_29)
                missing_stats <- rbind(missing_stats, feb_29, fill = TRUE)
              }
              data.table::set(missing_stats, j = "timeseries_id", value = i)
            }
          }
        } else {
          # Nothing here as it's taken care of outside the tryCatch loop
        }
      },
      error = function(e) {
        warning(
          "calculate_stats: failed to calculate stats for timeseries_id ",
          i,
          ". Returned error: ",
          e$message
        )
      }
    ) # End of tryCatch for stats calculation

    if (skip) {
      message(
        "Skipping calculations for timeseries ",
        i,
        " as it looks like nothing needs to be calculated."
      )
      next
    }

    if (nrow(missing_stats) > 0) {
      # This is separated from the calculation portion to allow for a tryCatch for calculation and appending, separately.
      tryCatch(
        {
          missing_stats <- missing_stats[order(missing_stats$date), ]
          missing_stats <- inf_to_na(missing_stats) # Occasionally % historic range is dividing by zero (snowpack), so this replaces Inf, -Inf with NAs
          # Construct the SQL DELETE query. This is done in a manner that can't delete rows where there are no calculated stats even if they are between the start and end date of missing_stats.
          delete_query <- paste0(
            "DELETE FROM measurements_calculated_daily WHERE timeseries_id = ",
            i,
            " AND date BETWEEN '",
            min(missing_stats$date),
            "' AND '",
            max(missing_stats$date),
            "'"
          )
          remaining_dates <- as.Date(
            setdiff(
              seq.Date(
                min(as.Date(missing_stats$date)),
                max(as.Date(missing_stats$date)),
                by = "day"
              ),
              as.Date(missing_stats$date)
            ),
            origin = "1970-01-01"
          )
          if (length(remaining_dates) > 0) {
            delete_query <- paste0(
              delete_query,
              " AND date NOT IN ('",
              paste(remaining_dates, collapse = "','"),
              "')"
            )
          }

          # Now commit the changes to the database
          commit_fx2 <- function(con, delete_query, missing_stats, i) {
            DBI::dbExecute(con, delete_query)
            DBI::dbAppendTable(
              con,
              "measurements_calculated_daily",
              missing_stats
            ) # Append the missing_stats data to the measurements_calculated_daily table
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE timeseries SET last_daily_calculation = '",
                .POSIXct(Sys.time(), "UTC"),
                "' WHERE timeseries_id = ",
                i,
                ";"
              )
            )
          }

          active <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.

          if (active) {
            tryCatch(
              {
                commit_fx2(con, delete_query, missing_stats, i)
                DBI::dbExecute(con, "COMMIT;")
              },
              error = function(e) {
                DBI::dbExecute(con, "ROLLBACK;")
              }
            )
          } else {
            # we're already in a transaction
            commit_fx2(con, delete_query, missing_stats, i)
          }
        },
        error = function(e) {
          warning(
            "calculate_stats: failed to append new statistics for timeseries_id ",
            i,
            ". Returned error: ",
            e$message
          )
        }
      ) # End of tryCatch for removing/adding to DB
    }
  } # End of for loop calculating means and stats for each station in timeseries table
} # End of calculate_stats function
