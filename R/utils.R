#' @title Format Date-Time to pass to SQL
#' @description
#' Format a date-time object to a string that can be passed to SQL queries. Works on POSIXct objects.
#'
#' @param x A date-time object (POSIXct).
#' @return A string formatted as "YYYY-MM-DD HH:MM:SS" in UTC time zone.
#' @export
fmt <- function(x) format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")


#' @title Begin a transaction
#' @description
#' Begin a transaction on a database connection. This function creates a temporary table to ensure the transaction is active, which DBI::dbBegin does not do. The transaction must be committed or rolled back using `DBI::dbExecute(con, "COMMIT;")` or `DBI::dbExecute(con, "ROLLBACK;")`. Note that you should not use DBI's built-in transaction functions to commit or rollback a transaction started with this function.
#'
#' @param con A database connection object.
#' @param silent A boolean indicating whether to suppress messages about transaction status.
#' @return A boolean indicating whether a transaction was started
#' @export
dbTransBegin <- function(con, silent = TRUE) {
  # Check if already in a transaction
  if (dbTransCheck(con)) {
    if (!silent) {
      message("dbTransBegin: Transaction already active.")
    }
    return(FALSE)
  }

  # Begin transaction
  DBI::dbExecute(con, "BEGIN;")
  # Create a temporary table that auto-destructs so the xact_id is assigned (otherwise nothing happens until something is pushed)
  DBI::dbExecute(con, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;") # Of course, also destroyed on rollback

  # Confirm transaction is active
  active <- DBI::dbGetQuery(
    con,
    "SELECT pg_current_xact_id_if_assigned() IS NOT NULL AS is_transaction;"
  )[1, 1]
  if (active) {
    return(active)
  } else {
    stop(
      "dbTransBegin: Transaction could not be started or verified as started."
    )
  }
}


#' @title Check if a transaction is active
#' @description
#' Check if a database connection is in an active transaction. Does not work on a transaction begun with `DBI::dbBegin` until some modification is made to the database; function [dbTransBegin()] should be used to start a transaction instead.
#'
#' @param con A database connection object.
#' @return A boolean indicating whether a transaction is active (TRUE for active)
#' @export
dbTransCheck <- function(con) {
  active <- DBI::dbGetQuery(
    con,
    "SELECT pg_current_xact_id_if_assigned() IS NOT NULL;"
  )[1, 1]

  if (!active) {
    # If transaction was started and nothing done yet, active would still be FALSE. Code below handles that possibility
    # Create a temporary table that auto-destructs so the xact_id is assigned (otherwise nothing happens until something is pushed)
    DBI::dbExecute(con, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;") # Of course, also destroyed on rollback (if not in a transaction, this will just create and drop the table immediately)
    # Check again
    active <- DBI::dbGetQuery(
      con,
      "SELECT pg_current_xact_id_if_assigned() IS NOT NULL;"
    )[1, 1]
  }

  return(active)
}


#' @title Acquire a PostgreSQL advisory lock for a namespaced key.
#' @description
#' Attempts to acquire a PostgreSQL advisory lock for a given namespace and key. If `wait` is TRUE, will block until the lock is acquired. If `wait` is FALSE, will return immediately with a boolean indicating whether the lock was acquired.
#' @param con A database connection object.
#' @param namespace The namespace for the lock.
#' @param key The key for the lock.
#' @param wait A boolean indicating whether to wait for the lock (default: TRUE).
#' @return A boolean indicating whether the lock was acquired.
#' @export

advisory_lock_acquire <- function(con, namespace, key, wait = TRUE) {
  if (wait) {
    DBI::dbExecute(
      con,
      "SELECT pg_advisory_lock(hashtext($1), $2);",
      params = list(namespace, key)
    )
    return(TRUE)
  }

  isTRUE(
    DBI::dbGetQuery(
      con,
      "SELECT pg_try_advisory_lock(hashtext($1), $2) AS locked;",
      params = list(namespace, key)
    )[[1]]
  )
}


#' @title Release a PostgreSQL advisory lock for a namespaced key.
#' @description
#' Includes a recovery path for aborted transactions and a final fallback to unlock all session advisory locks.
#' @param con A database connection object.
#' @param namespace The namespace for the lock.
#' @param key The key for the lock.
#' @return A boolean indicating whether the lock was released.
#' @export

advisory_lock_release <- function(con, namespace, key) {
  unlock_query <- "SELECT pg_advisory_unlock(hashtext($1), $2) AS unlocked;"
  unlock_once <- function() {
    DBI::dbGetQuery(
      con,
      unlock_query,
      params = list(namespace, key)
    )[[1]]
  }

  unlock_error <- NULL
  unlocked <- tryCatch(
    unlock_once(),
    error = function(e) {
      unlock_error <<- e
      NA
    }
  )

  if (
    !is.null(unlock_error) &&
      grepl(
        "current transaction is aborted",
        conditionMessage(unlock_error),
        fixed = TRUE
      )
  ) {
    # Clear aborted transaction state before trying unlock again.
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    unlock_error <- NULL
    unlocked <- tryCatch(
      unlock_once(),
      error = function(e) {
        unlock_error <<- e
        NA
      }
    )
  }

  if (isTRUE(unlocked)) {
    return(TRUE)
  }

  if (isFALSE(unlocked)) {
    warning(
      "advisory_lock_release: Lock was not held for namespace '",
      namespace,
      "' and key ",
      key,
      ".",
      call. = FALSE
    )
    return(FALSE)
  }

  err_text <- if (is.null(unlock_error)) {
    "Unknown unlock failure."
  } else {
    conditionMessage(unlock_error)
  }

  warning(
    "advisory_lock_release: Failed to release lock for namespace '",
    namespace,
    "' and key ",
    key,
    ". Error: ",
    err_text,
    ". Attempting fallback pg_advisory_unlock_all().",
    call. = FALSE
  )

  tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT pg_advisory_unlock_all();")
    },
    error = function(e) {
      warning(
        "advisory_lock_release: Fallback pg_advisory_unlock_all() also failed. Error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  FALSE
}


#' Replace infinite or NaN values with NA
#'
#' Utility function to replace `Inf`, `-Inf`, and `NaN` values with `NA`.
#'
#' @param x Numeric vector, data.frame, or data.table. If data.frame or data.table, will only work on 'numeric' class columns.
#' @return Numeric vector with infinite values converted to `NA`.
#' @export
inf_to_na <- function(x) {
  # data.table
  if (data.table::is.data.table(x)) {
    num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(num_cols)) {
      x[,
        (num_cols) := lapply(.SD, function(col) {
          idx <- which(!is.finite(col))
          if (length(idx)) {
            col[idx] <- NA
          }
          col
        }),
        .SDcols = num_cols
      ]
    }
    return(x)
  }

  # data.frame / tibble
  if (is.data.frame(x)) {
    # TRUE for tibbles or data.frames (and data.tables, but these are dealt with differently)
    numeric_cols <- sapply(x, is.numeric)
    x[numeric_cols] <- lapply(x[numeric_cols], function(col) {
      idx <- which(!is.finite(col))
      if (length(idx)) {
        col[idx] <- NA
      }
      col
    })
    return(x)
  }

  # vector
  if (is.numeric(x)) {
    idx <- which(!is.finite(x))
    if (length(idx)) {
      x[idx] <- NA
    }
    return(x)
  }

  # If x is not numeric, return it unchanged
  warning("Input is not numeric. Returning unchanged.")
  return(x)
}

#' @title Create an empty daily stats data frame with the correct structure
#' @description
#' Utility function to create an empty daily stats data frame with the correct structure
#' @return An empty data frame with the correct columns and types for daily stats
#' @noRd
#' @keywords internal
empty_daily_stats <- function() {
  data.frame(
    timeseries_id = integer(),
    date = as.Date(character()),
    value = numeric(),
    imputed = logical(),
    percent_historic_range = numeric(),
    max = numeric(),
    min = numeric(),
    q90 = numeric(),
    q75 = numeric(),
    q50 = numeric(),
    q25 = numeric(),
    q10 = numeric(),
    mean = numeric(),
    doy_count = integer()
  )
}

#' @title Normalize daily stats data frame
#' @description
#' Utility function to normalize a daily stats data frame to ensure it has the correct structure and types, and to fill in any missing columns with NA values. Also ensures the data frame is ordered by date.
#' @param df A data frame containing daily stats data, which may have missing columns or incorrect types.
#' @param timeseries_id The timeseries_id to fill in for any missing timeseries_id column.
#' @return A normalized data frame with the correct structure and types for daily stats, with missing columns filled in with NA and ordered by date.
#' @noRd
#' @keywords internal

normalize_daily_stats <- function(df, timeseries_id) {
  if (nrow(df) == 0) {
    return(empty_daily_stats())
  }

  cols <- c(
    "timeseries_id",
    "date",
    "value",
    "imputed",
    "percent_historic_range",
    "max",
    "min",
    "q90",
    "q75",
    "q50",
    "q25",
    "q10",
    "mean",
    "doy_count"
  )
  col_defaults <- list(
    timeseries_id = as.integer(NA),
    date = as.Date(NA),
    value = as.numeric(NA),
    imputed = as.logical(NA),
    percent_historic_range = as.numeric(NA),
    max = as.numeric(NA),
    min = as.numeric(NA),
    q90 = as.numeric(NA),
    q75 = as.numeric(NA),
    q50 = as.numeric(NA),
    q25 = as.numeric(NA),
    q10 = as.numeric(NA),
    mean = as.numeric(NA),
    doy_count = as.integer(NA)
  )
  numeric_cols <- c(
    "value",
    "percent_historic_range",
    "max",
    "min",
    "q90",
    "q75",
    "q50",
    "q25",
    "q10",
    "mean"
  )

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!("timeseries_id" %in% names(df))) {
    df$timeseries_id <- timeseries_id
  }

  for (col in setdiff(cols, names(df))) {
    df[[col]] <- rep(col_defaults[[col]], nrow(df))
  }

  df <- df[, cols, drop = FALSE]
  df$timeseries_id <- as.integer(df$timeseries_id)
  df$date <- as.Date(df$date)
  for (col in numeric_cols) {
    df[[col]] <- as.numeric(df[[col]])
  }
  df$imputed <- as.logical(df$imputed)
  df$doy_count <- as.integer(df$doy_count)
  df[order(df$date), , drop = FALSE]
}

#' @title Generate a unique key for daily stats rows
#' @description
#' Utility function to generate a unique key for daily stats rows based on the values of all columns. This is used to compare rows and determine if they have changed.
#' @param df A data frame containing daily stats data, which must have the columns: timeseries_id, date, value, imputed, percent_historic_range, max, min, q90, q75, q50, q25, q10, mean, doy_count.
#' @return A character vector of unique keys for each row, generated by concatenating the values of all columns with a separator. NA values are represented as "NA" in the key.
#' @noRd
#' @keywords internal

daily_stats_row_key <- function(df) {
  if (nrow(df) == 0) {
    return(character())
  }

  paste(
    ifelse(is.na(df$timeseries_id), "NA", as.character(df$timeseries_id)),
    as.character(df$date),
    ifelse(is.na(df$value), "NA", as.character(df$value)),
    ifelse(is.na(df$imputed), "NA", as.character(df$imputed)),
    ifelse(
      is.na(df$percent_historic_range),
      "NA",
      as.character(df$percent_historic_range)
    ),
    ifelse(is.na(df$max), "NA", as.character(df$max)),
    ifelse(is.na(df$min), "NA", as.character(df$min)),
    ifelse(is.na(df$q90), "NA", as.character(df$q90)),
    ifelse(is.na(df$q75), "NA", as.character(df$q75)),
    ifelse(is.na(df$q50), "NA", as.character(df$q50)),
    ifelse(is.na(df$q25), "NA", as.character(df$q25)),
    ifelse(is.na(df$q10), "NA", as.character(df$q10)),
    ifelse(is.na(df$mean), "NA", as.character(df$mean)),
    ifelse(is.na(df$doy_count), "NA", as.character(df$doy_count)),
    sep = "|"
  )
}

#' @title Retain only rows with changed daily stats
#' @description
#' Compares a data frame of daily stats rows to existing rows in the database for the same timeseries_id and date, and retains only the rows that have different values in any of the daily stats columns (value, imputed, percent_historic_range, max, min, q90, q75, q50, q25, q10, mean, doy_count). If there are no existing rows for the same timeseries_id and date, all rows are treated as changed and retained. This function is used to minimize the number of rows that need to be updated in the database by only updating rows where the daily stats have actually changed.
#' @param con A database connection object.
#' @param timeseries_id The timeseries_id for which to compare daily stats rows.
#' @param rows A data frame of daily stats rows to compare, which must have the columns: timeseries_id, date, value, imputed, percent_historic_range, max, min, q90, q75, q50, q25, q10, mean, doy_count.
#' @return A data frame containing only the rows from `rows` that have different values in any of the daily stats columns compared to the existing rows in the database for the same timeseries_id and date. If there are no existing rows for the same timeseries_id and date, all rows from `rows` are returned.
#' @noRd
#' @keywords internal
select_changed_daily_stats <- function(con, timeseries_id, rows) {
  rows <- normalize_daily_stats(rows, timeseries_id)
  if (nrow(rows) == 0) {
    return(rows)
  }

  existing <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT timeseries_id, date, value, imputed, percent_historic_range, ",
      "max, min, q90, q75, q50, q25, q10, mean, doy_count ",
      "FROM measurements_calculated_daily WHERE timeseries_id = ",
      timeseries_id,
      " AND date IN ('",
      paste(rows$date, collapse = "', '"),
      "');"
    )
  )
  existing <- normalize_daily_stats(existing, timeseries_id)

  existing_keys <- setNames(
    daily_stats_row_key(existing),
    as.character(existing$date)
  )
  row_keys <- daily_stats_row_key(rows)
  changed <- vapply(
    seq_len(nrow(rows)),
    function(idx) {
      existing_key <- unname(existing_keys[as.character(rows$date[idx])])
      if (length(existing_key) == 0) {
        return(TRUE)
      }
      !identical(
        existing_key,
        row_keys[idx]
      )
    },
    logical(1)
  )

  rows[changed, , drop = FALSE]
}
