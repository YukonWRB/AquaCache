#' Add new data to the measurements_continuous or measurements_calculated_daily tables
#'
#' This function can be used to append new contiuous type data directly to the database without downloading it from a remote source. Differs from [getNewContinuous()] as the later is used to pull data from a remote before append.
#'
#' @param tsid The timeseries_id to which the data will be appended. This is a required parameter.
#' @param df A data.frame containing the data. Must have columns named 'datetime' and 'value' at minimum. Other optional columns are 'owner', 'contributor', 'approval', 'grade', 'qualifier', 'imputed'; see the `adjust_` series of functions to see how these are used.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. If left NULL, a connection will be attempted using AquaConnect() and closed afterwards.
#' @param target One of 'realtime' or 'daily'. Default is 'realtime'. You would only want to append directly to the 'daily' table if adding pre-calculated daily means with the aim of adding higher frequency data to the 'measurements_continuous' table later. As an extra check, the data.frame passed in argument 'df' must contain a column named 'datetime' or 'date' to match this parameter.
#' @param overwrite Select one of "no", "all", "conflict". Default is "no", which will not overwrite any existing data (and fail if there is a conflict). "all" will wipe and replace all database entries for the target timeseries in the temporal range of `df`, while "conflict" will overwrite only those entries that conflict with the data in `df` (i.e. have the same datetime or date).
#'
#' @return Nothing; data is added to the database silently.
#' @export
#'

addNewContinuous <- function(
  tsid,
  df,
  con = NULL,
  target = "realtime",
  overwrite = "no"
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  # Check that the timeseries_id is valid
  check <- DBI::dbGetQuery(
    con,
    paste0("SELECT timeseries_id FROM timeseries WHERE timeseries_id = ", tsid)
  )[1, 1]
  if (is.na(check)) {
    stop("addNewContinuous: The timeseries_id you specified does not exist.")
  }

  if (!overwrite %in% c("no", "all", "conflict")) {
    stop(
      "addNewContinuous: Parameter 'overwrite' must be one of 'no', 'all', or 'conflict'."
    )
  }

  if (!target %in% c("realtime", "daily")) {
    stop(
      "addNewContinuous: Parameter 'target' must be one of 'realtime' or 'daily'"
    )
  }

  if (target == "realtime") {
    # Ensure there's a 'datetime' column
    if (!("datetime" %in% names(df))) {
      stop(
        "addNewContinuous: The data.frame must contain a column named 'datetime' if the parameter 'target' is set to 'realtime'."
      )
    }
  } else {
    if (!"date" %in% names(df)) {
      stop(
        "addNewContinuous: The data.frame must contain a column named 'date' if the parameter 'target' is set to 'daily'."
      )
    }
  }

  if (!("value" %in% names(df))) {
    stop(
      "addNewContinuous: The data.frame must contain a column named 'value'."
    )
  }

  grade_unknown <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNK';"
  )[1, 1]
  if (is.na(grade_unknown)) {
    stop(
      "addNewContinuous: Could not find grade type 'Unknown' in the database."
    )
  }
  approval_unknown <- DBI::dbGetQuery(
    con,
    "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNK';"
  )[1, 1]
  if (is.na(approval_unknown)) {
    stop(
      "addNewContinuous: Could not find approval type 'Unknown' in the database."
    )
  }
  qualifier_unknown <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNK';"
  )[1, 1]
  if (is.na(qualifier_unknown)) {
    stop(
      "addNewContinuous: Could not find qualifier type 'Unknown' in the database."
    )
  }

  info <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT at.aggregation_type, t.default_owner AS owner FROM timeseries AS t JOIN aggregation_types at ON at.aggregation_type_id = t.aggregation_type_id WHERE timeseries_id = ",
      tsid,
      ";"
    )
  )
  end_datetime_realtime <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT MAX(datetime) FROM measurements_continuous WHERE timeseries_id = ",
      tsid,
      ";"
    )
  )[[1]]
  end_datetime_daily <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT MAX(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
      tsid,
      ";"
    )
  )[[1]]

  if (is.na(end_datetime_realtime) && is.na(end_datetime_daily)) {
    last_data_point <- NA
  } else {
    last_data_point = max(
      end_datetime_realtime,
      end_datetime_daily,
      na.rm = TRUE
    ) +
      1
  }
  df <- df[!is.na(df$value), ]

  if ("owner" %in% names(df)) {
    if (!is.na(info$owner)) {
      df$owner[is.na(df$owner)] <- info$owner
    }
  } else {
    if (!is.na(info$owner)) {
      df$owner <- info$owner
    }
  }

  if (!("approval" %in% names(df))) {
    df$approval <- approval_unknown
  }

  if (!("grade" %in% names(df))) {
    df$grade <- grade_unknown
  }

  if (!("qualifier" %in% names(df))) {
    df$qualifier <- qualifier_unknown
  }

  if (!("imputed" %in% names(df))) {
    df$imputed <- FALSE
  }

  if (!("no_update" %in% names(df))) {
    df$no_update <- FALSE
  }

  # Append the data ##########################################################
  if (target == "realtime") {
    if (!inherits(df$datetime, "POSIXct")) {
      if (inherits(df$datetime, "character")) {
        df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
      } else if (inherits(df$datetime, "Date")) {
        df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
      } else {
        stop(
          "addNewContinuous: The 'datetime' column must be in POSIXct format or convertible to it (time zone will be assumed as UTC)."
        )
      }
    }

    if (is.na(last_data_point)) {
      last_data_point <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MAX(datetime) FROM measurements_continuous WHERE timeseries_id = ",
          tsid,
          ";"
        )
      )[1, 1]
    }
    if (is.na(last_data_point)) {
      last_data_point <- min(df$datetime) + 1
    }

    commit_fx <- function(con, df, last_data_point, tsid) {
      adjust_grade(con, tsid, df[, c("datetime", "grade")])
      adjust_approval(con, tsid, df[, c("datetime", "approval")])
      adjust_qualifier(con, tsid, df[, c("datetime", "qualifier")])
      if ("owner" %in% names(df)) {
        adjust_owner(con, tsid, df[, c("datetime", "owner")])
      }
      if ("contributor" %in% names(df)) {
        adjust_contributor(con, tsid, df[, c("datetime", "contributor")])
      }
      df$timeseries_id <- tsid
      # Drop columns no longer necessary
      df <- df[, c(
        "datetime",
        "value",
        "timeseries_id",
        "imputed",
        "no_update"
      )]

      #assign a period to the data
      if (info$aggregation_type == "instantaneous") {
        #Period is always 0 for instantaneous data
        df$period <- "00:00:00"
      } else if (
        (info$aggregation_type != "instantaneous") & !("period" %in% names(df))
      ) {
        # aggregation_types of mean, median, min, max should all have a period
        df_period <- calculate_period(
          data = df,
          timeseries_id = tsid,
          con = con
        )
        no_period <- dbGetQueryDT(
          con,
          paste0(
            "SELECT datetime FROM measurements_continuous WHERE timeseries_id = ",
            tsid,
            " AND datetime >= (SELECT MIN(datetime) FROM measurements_continuous WHERE period IS NULL AND timeseries_id = ",
            tsid,
            ") AND datetime NOT IN ('",
            paste(df$datetime, collapse = "', '"),
            "');"
          )
        )

        # Update the period column in the database with the calculated periods from 'df_period' where there is a match with datetimes in no_period
        if (nrow(no_period) > 0) {
          no_period$period <- df_period$period[match(
            no_period$datetime,
            df_period$datetime
          )]
          for (i in seq_len(nrow(no_period))) {
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE measurements_continuous SET period = '",
                no_period$period[i],
                "' WHERE datetime = '",
                no_period$datetime[i],
                "' AND timeseries_id = ",
                tsid,
                ";"
              )
            )
          }
        }

        # Only retain rows in df_period that were in df (calculate_period could have added rows if it needed to pull extra data to calculate the period)
        df <- df_period[df$datetime %in% df_period$datetime, ]
      } else {
        #Check to make sure that the supplied period can actually be coerced to a period
        check <- lubridate::period(unique(df$period))
        if (NA %in% check) {
          df$period <- NA
        }
      }

      if (overwrite == "all") {
        DBI::dbExecute(
          con,
          paste0(
            "DELETE FROM measurements_continuous WHERE datetime BETWEEN '",
            min(df$datetime),
            "' AND '",
            max(df$datetime),
            "' AND timeseries_id = ",
            tsid,
            ";"
          )
        )
      } else if (overwrite == "conflict") {
        DBI::dbExecute(
          con,
          paste0(
            "DELETE FROM measurements_continuous WHERE datetime IN ('",
            paste(df$datetime, collapse = "', '"),
            "') AND timeseries_id = ",
            tsid,
            ";"
          )
        )
      } else {
        # If overwrite is "no", we remove any rows in df that conflict with existing data
        existing_datetimes <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT datetime FROM measurements_continuous WHERE datetime IN ('",
            paste(df$datetime, collapse = "', '"),
            "') AND timeseries_id = ",
            tsid,
            ";"
          )
        )$datetime
        if (length(existing_datetimes) > 0) {
          df <- df[!df$datetime %in% existing_datetimes, ]
        }
      }
      DBI::dbAppendTable(con, "measurements_continuous", df)

      tryCatch(
        {
          calculate_stats(
            con = con,
            timeseries_id = tsid,
            start_recalc = min(df$datetime)
          )
        },
        error = function(e) {
          message(
            "addNewContinuous: Failed to recalculate statistics for timeseries_id ",
            tsid,
            ". Error message: '",
            e$message,
            "'."
          )
        }
      )

      # make the new entry into table timeseries
      exist_times <- DBI::dbGetQuery(
        con,
        "SELECT start_datetime, end_datetime FROM timeseries WHERE timeseries_id =  $1;",
        params = list(tsid)
      )
      new_start <- min(c(exist_times$start_datetime, df$datetime), na.rm = TRUE)
      new_end <- max(c(exist_times$end_datetime, df$datetime), na.rm = TRUE)
      DBI::dbExecute(
        con,
        "UPDATE timeseries SET end_datetime = $1, start_datetime = $2, last_new_data = $3 WHERE timeseries_id = $4",
        params = list(
          new_end,
          new_start,
          .POSIXct(Sys.time(), "UTC"),
          tsid
        )
      )
    } # end commit_fx

    activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
    if (activeTrans) {
      tryCatch(
        {
          commit_fx(con, df, last_data_point, tsid)
          DBI::dbExecute(con, "COMMIT;")
        },
        error = function(e) {
          DBI::dbExecute(con, "ROLLBACK;")
          warning(
            "addNewContinuous: Failed to append new data. Returned error: ",
            e$message,
            "."
          )
        }
      )
    } else {
      commit_fx(con, df, last_data_point, tsid)
    }
  } else {
    if (!inherits(df$date, "Date")) {
      stop("addNewContinuous: The 'date' column must be in Date format.")
    }

    if (is.na(last_data_point)) {
      last_data_point <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MAX(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
          tsid,
          ";"
        )
      )[1, 1]
    }
    if (is.na(last_data_point)) {
      last_data_point <- min(df$date) + 1
    }

    commit_fx <- function(con, df, last_data_point, tsid) {
      adjust_grade(con, tsid, df[, c("date", "grade")])
      adjust_approval(con, tsid, df[, c("date", "approval")])
      adjust_qualifier(con, tsid, df[, c("date", "qualifier")])
      if ("owner" %in% names(df)) {
        adjust_owner(con, tsid, df[, c("date", "owner")])
      }
      if ("contributor" %in% names(df)) {
        adjust_contributor(con, tsid, df[, c("date", "contributor")])
      }

      df$timeseries_id <- tsid
      # Drop columns no longer necessary
      df <- df[, c("date", "value", "timeseries_id", "imputed", "no_update")]

      if (overwrite == "all") {
        DBI::dbExecute(
          con,
          paste0(
            "DELETE FROM measurements_calculated_daily WHERE date BETWEEN ('",
            min(df$date),
            "' AND '",
            max(df$date),
            "') AND timeseries_id = ",
            tsid,
            ";"
          )
        )
      } else if (overwrite == "conflict") {
        DBI::dbExecute(
          con,
          paste0(
            "DELETE FROM measurements_calculated_daily WHERE date IN ('",
            paste(df$date, collapse = "', '"),
            "') AND timeseries_id = ",
            tsid,
            ";"
          )
        )
      } else {
        # If overwrite is "no", we do not delete any existing data, so we just append
      }
      DBI::dbAppendTable(con, "measurements_calculated_daily", df)
      #make the new entry into table timeseries
      if (max(df$date) > last_data_point) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE timeseries SET end_datetime = '",
            max(df$date),
            "', last_new_data = '",
            .POSIXct(Sys.time(), "UTC"),
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
      } else {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE timeseries SET last_new_data = '",
            .POSIXct(Sys.time(), "UTC"),
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
      }
    }

    activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
    if (activeTrans) {
      tryCatch(
        {
          commit_fx(con, df, last_data_point, tsid)
          DBI::dbExecute(con, "COMMIT;")
        },
        error = function(e) {
          DBI::dbExecute(con, "ROLLBACK;")
          warning(
            "addNewContinuous: Failed to append new data. Returned error '",
            e$message,
            "'."
          )
        }
      )
    } else {
      commit_fx(con, df, last_data_point, tsid)
    }
  }
}
