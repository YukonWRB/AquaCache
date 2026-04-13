#' @title Format Date-Time to pass to SQL
#' @description
#' Format a date-time object to a string that can be passed to SQL queries. Works on POSIXct objects.
#'
#' @param x A date-time object (POSIXct).
#' @return A string formatted as "YYYY-MM-DD HH:MM:SS" in UTC time zone.
#' @export
fmt <- function(x) format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

#' @title Temporarily clear PostgreSQL spatial environment variables
#' @description
#' Internal helper used around raster workflows to avoid `terra` picking up
#' PostgreSQL-installed `PROJ_LIB` or `GDAL_DATA` paths that can break raster
#' reads and writes. Returns a restore function when any variables are unset.
#' @return Either `NULL` when no PostgreSQL spatial environment variables were
#' detected, or a function that restores the original values.
#' @noRd
#' @keywords internal
unset_postgres_spatial_env <- function() {
  env_vars <- c("PROJ_LIB", "GDAL_DATA")
  old_env <- Sys.getenv(env_vars, unset = NA_character_)
  postgres_vars <- !is.na(old_env) &
    grepl("PostgreSQL", old_env, ignore.case = TRUE)

  if (!any(postgres_vars)) {
    return(NULL)
  }

  vars_to_unset <- env_vars[postgres_vars]
  old_vals <- old_env[postgres_vars]
  Sys.unsetenv(vars_to_unset)

  function() {
    for (nm in names(old_vals)) {
      if (is.na(old_vals[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, setNames(list(old_vals[[nm]]), nm))
      }
    }
  }
}

#' @title Find a PostgreSQL command line utility
#' @description
#' Internal helper that first checks `Sys.which()` and, on Windows, falls back
#' to common PostgreSQL installation directories under Program Files.
#' @param name The base utility name, such as `"psql"` or `"raster2pgsql"`.
#' @return A single path to the requested utility, or `""` if it could not be
#' found.
#' @noRd
#' @keywords internal
find_postgres_utility <- function(name) {
  utility_path <- Sys.which(name)
  if (nzchar(utility_path) || .Platform$OS.type != "windows") {
    return(utility_path)
  }

  patterns <- c(
    file.path("C:/Program Files/PostgreSQL", "*", "bin", paste0(name, ".exe")),
    file.path(
      "C:/Program Files (x86)/PostgreSQL",
      "*",
      "bin",
      paste0(name, ".exe")
    )
  )
  candidates <- unique(unlist(lapply(patterns, Sys.glob), use.names = FALSE))
  if (!length(candidates)) {
    return("")
  }

  normalizePath(
    sort(candidates, decreasing = TRUE)[1],
    winslash = "\\",
    mustWork = FALSE
  )
}


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

  existing_keys <- stats::setNames(
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

#' @title Resolve a matrix state identifier
#' @description
#' Accepts a matrix state identifier as an integer id, numeric string, or a
#' matrix state code/name and returns the corresponding matrix_state_id.
#' @param con A database connection object.
#' @param matrix_state A single matrix state identifier or label.
#' @return An integer matrix_state_id, or `NA_integer_` if no value was
#' supplied.
#' @noRd
#' @keywords internal
resolve_matrix_state_identifier <- function(con, matrix_state = NA) {
  if (is.null(matrix_state) || length(matrix_state) == 0) {
    return(NA_integer_)
  }

  matrix_state <- matrix_state[1]

  if (is.na(matrix_state)) {
    return(NA_integer_)
  }

  if (is.factor(matrix_state)) {
    matrix_state <- as.character(matrix_state)
  }

  if (inherits(matrix_state, "integer")) {
    return(as.integer(matrix_state))
  }

  if (is.numeric(matrix_state)) {
    return(as.integer(matrix_state))
  }

  matrix_state <- trimws(as.character(matrix_state))
  if (!nzchar(matrix_state) || toupper(matrix_state) %in% c("NA", "NULL")) {
    return(NA_integer_)
  }

  if (grepl("^[+-]?[0-9]+$", matrix_state)) {
    return(as.integer(matrix_state))
  }

  hits <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT matrix_state_id",
      "FROM public.matrix_states",
      "WHERE LOWER(matrix_state_code) = LOWER($1)",
      "   OR LOWER(matrix_state_name) = LOWER($1)",
      "   OR LOWER(matrix_state_name_fr) = LOWER($1)",
      "ORDER BY matrix_state_id;"
    ),
    params = list(matrix_state)
  )

  if (nrow(hits) == 0) {
    stop(
      "Unknown matrix_state value '",
      matrix_state,
      "'. Supply a valid matrix_state_id or a value from public.matrix_states."
    )
  }

  if (nrow(hits) > 1) {
    stop(
      "Matrix state value '",
      matrix_state,
      "' matched multiple rows in public.matrix_states."
    )
  }

  as.integer(hits$matrix_state_id[[1]])
}

#' @title Normalize matrix state columns on discrete result rows
#' @description
#' Resolves `matrix_state` text labels or `matrix_state_id` values on a results
#' data.frame and returns rows ready for insert into `discrete.results`.
#' @param con A database connection object.
#' @param sample_media_id The parent sample media_id for the results.
#' @param results A discrete results data.frame.
#' @return The input `results` with a resolved `matrix_state_id` column and any
#' `matrix_state` helper column removed.
#' @noRd
#' @keywords internal
normalize_discrete_result_matrix_states <- function(
  con,
  sample_media_id,
  results
) {
  if (nrow(results) == 0) {
    return(results)
  }

  matrix_state_input <- if ("matrix_state" %in% names(results)) {
    results$matrix_state
  } else if ("matrix_state_id" %in% names(results)) {
    results$matrix_state_id
  } else {
    rep(NA_integer_, nrow(results))
  }

  if (!("matrix_state_id" %in% names(results))) {
    results$matrix_state_id <- NA_integer_
  }

  results$matrix_state_id <- vapply(
    seq_len(nrow(results)),
    function(i) {
      resolve_discrete_result_matrix_state(
        con = con,
        sample_media_id = sample_media_id,
        parameter_id = results$parameter_id[[i]],
        matrix_state_id = results$matrix_state_id[[i]],
        matrix_state = matrix_state_input[[i]]
      )
    },
    integer(1)
  )

  if ("matrix_state" %in% names(results)) {
    results$matrix_state <- NULL
  }

  results
}

#' @title Find a discrete sample id from its unique key
#' @description
#' Looks up a discrete sample using the actual uniqueness constraint on
#' `discrete.samples`.
#' @param con A database connection object.
#' @param sample A one-row sample data.frame.
#' @return The matching sample_id as an integer.
#' @noRd
#' @keywords internal
find_discrete_sample_id <- function(con, sample) {
  sample_id <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT sample_id",
      "FROM samples",
      "WHERE location_id = $1",
      "  AND sub_location_id IS NOT DISTINCT FROM $2",
      "  AND media_id = $3",
      "  AND z IS NOT DISTINCT FROM $4",
      "  AND datetime = $5",
      "  AND sample_type = $6",
      "  AND collection_method = $7",
      "LIMIT 1;"
    ),
    params = list(
      suppressWarnings(as.integer(sample$location_id[1])),
      if ("sub_location_id" %in% names(sample)) {
        suppressWarnings(as.integer(sample$sub_location_id[1]))
      } else {
        NA_integer_
      },
      suppressWarnings(as.integer(sample$media_id[1])),
      if ("z" %in% names(sample)) {
        suppressWarnings(as.numeric(sample$z[1]))
      } else {
        NA_real_
      },
      as.POSIXct(sample$datetime[1], tz = "UTC"),
      suppressWarnings(as.integer(sample$sample_type[1])),
      suppressWarnings(as.integer(sample$collection_method[1]))
    )
  )[1, 1]

  if (is.na(sample_id)) {
    stop(
      "Could not determine sample_id for the inserted discrete sample. ",
      "The sample insert may have failed or the sample uniqueness key may be inconsistent."
    )
  }

  as.integer(sample_id)
}

#' @title Resolve matrix state ID for parameter/media combinations
#' @description
#' Utility function to resolve the matrix state ID for a parameter/media
#' combination using the database function `public.resolve_matrix_state_id()`.
#' If `matrix_state_id` or `matrix_state` is already supplied, that value is
#' resolved and returned unchanged.
#' @param con A database connection object.
#' @param media_id The media_id used to infer a default matrix state when one is not supplied explicitly.
#' @param parameter_id The parameter_id for the discrete result.
#' @param matrix_state_id An optional existing matrix_state_id for the discrete
#' result. If provided and not NA, this value will be returned as is.
#' @param matrix_state An optional matrix state code or label to resolve when
#' `matrix_state_id` is not provided.
#' @return An integer matrix_state_id for the parameter/media combination.
#' @noRd
#' @keywords internal
resolve_parameter_matrix_state <- function(
  con,
  media_id,
  parameter_id,
  matrix_state_id = NA_integer_,
  matrix_state = matrix_state_id
) {
  media_id <- suppressWarnings(as.integer(media_id))
  parameter_id <- suppressWarnings(as.integer(parameter_id))
  matrix_state_id <- resolve_matrix_state_identifier(con, matrix_state_id)

  if (!is.na(matrix_state_id)) {
    return(matrix_state_id)
  }

  matrix_state_id <- resolve_matrix_state_identifier(con, matrix_state)
  if (!is.na(matrix_state_id)) {
    return(matrix_state_id)
  }

  DBI::dbGetQuery(
    con,
    "SELECT public.resolve_matrix_state_id($1, $2, $3) AS matrix_state_id;",
    params = list(media_id, parameter_id, matrix_state_id)
  )[1, 1]
}

#' @title Resolve matrix state ID for discrete results
#' @description
#' Utility function to resolve the matrix state ID for discrete results based on
#' the sample_media_id, parameter_id, and optionally an existing
#' matrix_state_id. If the matrix_state_id is provided and not NA, it is
#' returned as is. If the matrix_state_id is NA, the function calls a database
#' function `public.resolve_matrix_state_id` to determine the appropriate
#' matrix_state_id based on the sample_media_id and parameter_id. This is used
#' to ensure that discrete results have the correct matrix state ID assigned,
#' which may be necessary for certain calculations or analyses.
#' @param con A database connection object.
#' @param sample_media_id The sample_media_id for the discrete result.
#' @param parameter_id The parameter_id for the discrete result.
#' @param matrix_state_id An optional existing matrix_state_id for the discrete
#' result. If provided and not NA, this value will be returned as is.
#' @param matrix_state An optional matrix state code or label to resolve when
#' `matrix_state_id` is not provided.
#' @return An integer matrix_state_id for the discrete result, either the
#' provided matrix_state_id if it was not NA, or the resolved matrix_state_id
#' from the database function if the provided matrix_state_id was NA.
#' @noRd
#' @keywords internal
resolve_discrete_result_matrix_state <- function(
  con,
  sample_media_id,
  parameter_id,
  matrix_state_id = NA_integer_,
  matrix_state = matrix_state_id
) {
  sample_media_id <- suppressWarnings(as.integer(sample_media_id))
  parameter_id <- suppressWarnings(as.integer(parameter_id))
  matrix_state_id <- resolve_matrix_state_identifier(con, matrix_state_id)

  if (!is.na(matrix_state_id)) {
    return(matrix_state_id)
  }

  matrix_state_id <- resolve_matrix_state_identifier(con, matrix_state)
  if (!is.na(matrix_state_id)) {
    return(matrix_state_id)
  }

  DBI::dbGetQuery(
    con,
    "SELECT public.resolve_matrix_state_id($1, $2, $3) AS matrix_state_id;",
    params = list(sample_media_id, parameter_id, matrix_state_id)
  )[1, 1]
}
