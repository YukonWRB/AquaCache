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
        do.call(Sys.setenv, stats::setNames(list(old_vals[[nm]]), nm))
      }
    }
  }
}

#' @title Find a PostgreSQL command line utility
#' @description
#' Finds a PostgreSQL/PostGIS command line utility by checking PATH and common
#' installation directories, then choosing the newest detected version.
#' @param name The base utility name, such as `"psql"` or `"raster2pgsql"`.
#' @param version_ok Optional predicate used to filter candidate versions.
#'   It is called as `version_ok(major, minor, patch, label, path)`.
#' @param prefer_newest If `TRUE`, choose the newest version found. If `FALSE`,
#'   choose the first discovered candidate.
#' @return A single normalized path, or `""` if no suitable utility is found.
#' @noRd
#' @keywords internal
find_postgres_utility <- function(
  name,
  version_ok = NULL,
  prefer_newest = TRUE
) {
  candidates <- aquacache_postgres_utility_candidates(name)

  if (!length(candidates)) {
    return("")
  }

  if (!is.null(version_ok) && !is.function(version_ok)) {
    stop("`version_ok` must be NULL or a function.", call. = FALSE)
  }

  info <- aquacache_postgres_utility_info(candidates)
  info <- info[info$available, , drop = FALSE]

  if (!nrow(info)) {
    return("")
  }

  if (!is.null(version_ok)) {
    keep <- vapply(
      seq_len(nrow(info)),
      function(i) {
        isTRUE(version_ok(
          major = info$major[[i]],
          minor = info$minor[[i]],
          patch = info$patch[[i]],
          label = info$version_label[[i]],
          path = info$path[[i]]
        ))
      },
      logical(1)
    )

    info <- info[keep, , drop = FALSE]

    if (!nrow(info)) {
      return("")
    }
  }

  if (!prefer_newest) {
    return(info$path[[1L]])
  }

  parsed <- !is.na(info$major)

  if (any(parsed)) {
    ranked <- info[parsed, , drop = FALSE]

    ord <- order(
      -ranked$major,
      -ranked$minor,
      -ranked$patch,
      ranked$source_rank
    )

    return(ranked$path[[ord[[1L]]]])
  }

  # Fallback for utilities whose version cannot be parsed.
  info$path[[1L]]
}


#' @keywords internal
#' @noRd
aquacache_postgres_utility_candidates <- function(name) {
  candidates <- character()

  path_hit <- unname(Sys.which(name))
  if (nzchar(path_hit)) {
    candidates <- c(candidates, path_hit)
  }

  path_dirs <- strsplit(
    Sys.getenv("PATH", unset = ""),
    .Platform$path.sep,
    fixed = TRUE
  )[[1]]

  path_dirs <- gsub('^"|"$', "", path_dirs)
  path_dirs <- path_dirs[nzchar(path_dirs)]

  if (.Platform$OS.type == "windows") {
    has_extension <- grepl("\\.[A-Za-z0-9]+$", basename(name))

    if (has_extension) {
      path_names <- name
    } else {
      path_ext <- strsplit(
        Sys.getenv("PATHEXT", unset = ".COM;.EXE;.BAT;.CMD"),
        ";",
        fixed = TRUE
      )[[1]]

      path_ext <- unique(tolower(path_ext[nzchar(path_ext)]))
      path_names <- unique(c(
        paste0(name, path_ext),
        paste0(name, ".exe"),
        name
      ))
    }

    if (length(path_dirs)) {
      candidates <- c(
        candidates,
        unlist(
          lapply(path_dirs, function(d) file.path(d, path_names)),
          use.names = FALSE
        )
      )
    }

    exe_name <- if (grepl("\\.exe$", name, ignore.case = TRUE)) {
      name
    } else {
      paste0(name, ".exe")
    }

    program_dirs <- unique(c(
      Sys.getenv("ProgramFiles", unset = ""),
      "C:/Program Files",
      Sys.getenv("ProgramFiles(x86)", unset = ""),
      "C:/Program Files (x86)"
    ))

    program_dirs <- program_dirs[nzchar(program_dirs)]

    patterns <- file.path(
      program_dirs,
      "PostgreSQL",
      "*",
      "bin",
      exe_name
    )

    candidates <- c(
      candidates,
      unlist(lapply(patterns, Sys.glob), use.names = FALSE)
    )
  } else {
    if (length(path_dirs)) {
      candidates <- c(candidates, file.path(path_dirs, name))
    }

    candidates <- c(
      candidates,
      file.path(
        c("/usr/bin", "/usr/local/bin", "/opt/homebrew/bin", "/opt/local/bin"),
        name
      )
    )

    patterns <- c(
      file.path("/usr/lib/postgresql", "*", "bin", name),
      file.path("/usr/pgsql-*", "bin", name),
      file.path("/Library/PostgreSQL", "*", "bin", name),
      file.path(
        "/Applications/Postgres.app/Contents/Versions",
        "*",
        "bin",
        name
      )
    )

    candidates <- c(
      candidates,
      unlist(lapply(patterns, Sys.glob), use.names = FALSE)
    )
  }

  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]

  if (.Platform$OS.type != "windows") {
    access <- file.access(candidates, mode = 1L)
    candidates <- candidates[!is.na(access) & access == 0L]
  }

  if (!length(candidates)) {
    return(character())
  }

  unique(normalizePath(
    candidates,
    winslash = "\\",
    mustWork = FALSE
  ))
}


#' @title Find a GDAL command line utility
#' @description
#' Finds a GDAL/OGR command line utility by checking PATH and common GDAL,
#' QGIS, OSGeo4W, PostgreSQL, Homebrew, and Linux installation directories.
#' @param name The base utility name, such as `"ogr2ogr"`.
#' @return A single normalized path, or `""` if no suitable utility is found.
#' @noRd
#' @keywords internal
find_gdal_utility <- function(name) {
  candidates <- aquacache_gdal_utility_candidates(name)

  if (!length(candidates)) {
    return("")
  }

  info <- aquacache_postgres_utility_info(candidates)
  parsed <- !is.na(info$major)

  if (any(parsed)) {
    ranked <- info[parsed, , drop = FALSE]
    ord <- order(
      -ranked$major,
      -ranked$minor,
      -ranked$patch,
      ranked$source_rank
    )
    return(ranked$path[[ord[[1L]]]])
  }

  info$path[[1L]]
}


#' @keywords internal
#' @noRd
aquacache_gdal_utility_candidates <- function(name) {
  candidates <- aquacache_postgres_utility_candidates(name)

  path_dirs <- strsplit(
    Sys.getenv("PATH", unset = ""),
    .Platform$path.sep,
    fixed = TRUE
  )[[1]]
  path_dirs <- gsub('^"|"$', "", path_dirs)
  path_dirs <- path_dirs[nzchar(path_dirs)]

  if (.Platform$OS.type == "windows") {
    exe_name <- if (grepl("\\.exe$", name, ignore.case = TRUE)) {
      name
    } else {
      paste0(name, ".exe")
    }

    program_dirs <- unique(c(
      Sys.getenv("ProgramFiles", unset = ""),
      "C:/Program Files",
      Sys.getenv("ProgramFiles(x86)", unset = ""),
      "C:/Program Files (x86)"
    ))
    program_dirs <- program_dirs[nzchar(program_dirs)]

    patterns <- c(
      file.path(program_dirs, "QGIS*", "bin", exe_name),
      file.path(program_dirs, "QGIS*", "apps", "gdal", "bin", exe_name),
      file.path(program_dirs, "OSGeo4W", "bin", exe_name),
      file.path(program_dirs, "OSGeo4W64", "bin", exe_name),
      file.path("C:/OSGeo4W", "bin", exe_name),
      file.path("C:/OSGeo4W64", "bin", exe_name)
    )

    candidates <- c(
      candidates,
      unlist(lapply(patterns, Sys.glob), use.names = FALSE)
    )
  } else {
    candidates <- c(
      candidates,
      file.path(path_dirs, name),
      file.path(
        c(
          "/usr/bin",
          "/usr/local/bin",
          "/opt/homebrew/bin",
          "/opt/local/bin",
          "/usr/local/opt/gdal/bin",
          "/opt/qgis/bin"
        ),
        name
      )
    )
  }

  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]

  if (.Platform$OS.type != "windows") {
    access <- file.access(candidates, mode = 1L)
    candidates <- candidates[!is.na(access) & access == 0L]
  }

  if (!length(candidates)) {
    return(character())
  }

  unique(normalizePath(
    candidates,
    winslash = "\\",
    mustWork = FALSE
  ))
}


#' @keywords internal
#' @noRd
aquacache_try_install_ogr2ogr <- function(ask = interactive()) {
  existing <- find_gdal_utility("ogr2ogr")
  if (nzchar(existing)) {
    return(existing)
  }

  if (!isTRUE(ask)) {
    return("")
  }

  install_cmd <- NULL
  install_args <- NULL
  install_label <- NULL

  if (.Platform$OS.type == "windows") {
    winget <- unname(Sys.which("winget"))
    choco <- unname(Sys.which("choco"))

    if (nzchar(winget)) {
      install_cmd <- winget
      install_args <- c(
        "install",
        "--id", "OSGeo.QGIS_LTR",
        "--exact",
        "--silent",
        "--accept-package-agreements",
        "--accept-source-agreements"
      )
      install_label <- "QGIS LTR with GDAL/OGR using winget"
    } else if (nzchar(choco)) {
      install_cmd <- choco
      install_args <- c("install", "qgis-ltr", "-y")
      install_label <- "QGIS LTR with GDAL/OGR using Chocolatey"
    }
  } else {
    brew <- unname(Sys.which("brew"))
    apt_get <- unname(Sys.which("apt-get"))
    dnf <- unname(Sys.which("dnf"))
    yum <- unname(Sys.which("yum"))
    pacman <- unname(Sys.which("pacman"))
    zypper <- unname(Sys.which("zypper"))
    apk <- unname(Sys.which("apk"))

    if (nzchar(brew)) {
      install_cmd <- brew
      install_args <- c("install", "gdal")
      install_label <- "GDAL using Homebrew"
    } else if (nzchar(apt_get)) {
      install_cmd <- "sudo"
      install_args <- c(apt_get, "install", "-y", "gdal-bin")
      install_label <- "gdal-bin using apt-get"
    } else if (nzchar(dnf)) {
      install_cmd <- "sudo"
      install_args <- c(dnf, "install", "-y", "gdal")
      install_label <- "GDAL using dnf"
    } else if (nzchar(yum)) {
      install_cmd <- "sudo"
      install_args <- c(yum, "install", "-y", "gdal")
      install_label <- "GDAL using yum"
    } else if (nzchar(pacman)) {
      install_cmd <- "sudo"
      install_args <- c(pacman, "-S", "--noconfirm", "gdal")
      install_label <- "GDAL using pacman"
    } else if (nzchar(zypper)) {
      install_cmd <- "sudo"
      install_args <- c(zypper, "--non-interactive", "install", "gdal")
      install_label <- "GDAL using zypper"
    } else if (nzchar(apk)) {
      install_cmd <- "sudo"
      install_args <- c(apk, "add", "gdal-tools")
      install_label <- "GDAL tools using apk"
    }
  }

  if (is.null(install_cmd)) {
    message(
      "Could not find a supported package manager to install ogr2ogr automatically. ",
      "Install GDAL/QGIS/OSGeo4W and ensure ogr2ogr is on PATH."
    )
    return("")
  }

  message(
    "ogr2ogr was not found. AquaCache can try to install ",
    install_label,
    ". This may require administrator privileges and may take several minutes."
  )
  commit <- readline(prompt = "Install now? 1 = yes, 2 = no: ")
  if (!identical(as.numeric(commit), 1)) {
    return("")
  }

  status <- tryCatch(
    system2(install_cmd, install_args),
    error = function(e) {
      warning("Failed to run ogr2ogr installer: ", conditionMessage(e))
      1L
    }
  )

  if (!identical(status, 0L)) {
    warning(
      "The ogr2ogr installation command did not complete successfully. ",
      "Falling back to the R-only vector insert method."
    )
    return("")
  }

  find_gdal_utility("ogr2ogr")
}


#' @keywords internal
#' @noRd
aquacache_postgres_utility_info <- function(paths) {
  versions <- lapply(paths, aquacache_postgres_utility_version)

  data.frame(
    path = paths,
    version_label = vapply(versions, `[[`, character(1), "label"),
    available = vapply(versions, `[[`, logical(1), "available"),
    major = vapply(versions, `[[`, integer(1), "major"),
    minor = vapply(versions, `[[`, integer(1), "minor"),
    patch = vapply(versions, `[[`, integer(1), "patch"),
    source_rank = seq_along(paths),
    stringsAsFactors = FALSE
  )
}


#' @keywords internal
#' @noRd
aquacache_postgres_utility_version <- function(path) {
  unknown <- function(label = NA_character_, available = FALSE) {
    list(
      label = label,
      available = available,
      major = NA_integer_,
      minor = NA_integer_,
      patch = NA_integer_
    )
  }

  out <- tryCatch(
    suppressWarnings(system2(path, "--version", stdout = TRUE, stderr = TRUE)),
    error = function(e) character()
  )

  status <- attr(out, "status")
  available <- is.null(status) || identical(as.integer(status), 0L)

  if (!length(out) || !available) {
    return(unknown(paste(out, collapse = " "), available = available))
  }

  label <- paste(out, collapse = " ")

  matches <- regmatches(
    label,
    gregexpr("[0-9]+(\\.[0-9]+){1,3}", label, perl = TRUE)
  )[[1]]

  if (!length(matches) || identical(matches, character(0))) {
    return(unknown(label, available = TRUE))
  }

  # Use the last version-like token. This avoids reading the "2" in
  # utility names like raster2pgsql as the version.
  version <- utils::tail(matches, 1L)
  parts <- suppressWarnings(as.integer(strsplit(version, ".", fixed = TRUE)[[
    1
  ]]))

  if (length(parts) < 2L || anyNA(parts)) {
    return(unknown(label, available = TRUE))
  }

  parts <- c(parts, 0L, 0L, 0L)[1:3]

  list(
    label = label,
    available = TRUE,
    major = parts[[1L]],
    minor = parts[[2L]],
    patch = parts[[3L]]
  )
}

#' Check if a given PostgreSQL version supports the RESTRICT option
#' @keywords internal
#' @noRd
aquacache_psql_supports_restrict <- function(
  major,
  minor,
  patch = 0L,
  label = NULL,
  path = NULL
) {
  if (is.na(major) || is.na(minor)) {
    return(FALSE)
  }

  if (major >= 18L) {
    return(TRUE)
  }

  fixed_minor <- c(
    `13` = 22L,
    `14` = 19L,
    `15` = 14L,
    `16` = 10L,
    `17` = 6L
  )

  major_chr <- as.character(major)

  major_chr %in% names(fixed_minor) && minor >= fixed_minor[[major_chr]]
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

#' @title Convert a date to a daily datetime in UTC
#' @description
#' Utility function to convert a date to a daily datetime in UTC, with an optional timezone offset in hours. The resulting datetime will be at noon UTC by default, or adjusted based on the provided timezone offset.
#' @param date A date object or a string that can be converted to a date.
#' @param timezone_offset_hours An optional integer representing the timezone offset in hours from UTC.
#' @return A POSIXct datetime object in UTC corresponding to the input date at noon UTC, adjusted for the timezone offset if provided.
#' @noRd
#' @keywords internal

daily_datetime_utc <- function(date, timezone_offset_hours = 0L) {
  timezone_offset_hours <- suppressWarnings(as.integer(timezone_offset_hours))
  if (length(timezone_offset_hours) == 0 || is.na(timezone_offset_hours)) {
    timezone_offset_hours <- 0L
  }

  as.POSIXct(as.Date(date), tz = "UTC") +
    (12L - timezone_offset_hours) * 3600
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
      "FROM discrete.samples",
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


#' @title Get free disk space in gigabytes
#' @description
#' Utility function to get the amount of free disk space in gigabytes for a given path. If no path is provided, it defaults to the root directory ("/") on Unix-like systems or the C: drive on Windows. The function uses platform-specific commands to determine free disk space and returns the result in gigabytes.
#' @param path An optional path to check for free disk space. Defaults to "/" on Unix-like systems and "C:" on Windows.
#' @return The amount of free disk space in gigabytes for the specified path.
#' @export

get_free_space_gb <- function(path = NULL) {
  if (.Platform$OS.type == "windows") {
    if (is.null(path)) {
      path <- "C:"
    }

    # WMIC is deprecated on newer Windows but still commonly available
    cmd <- sprintf(
      'wmic logicaldisk where "DeviceID=\'%s\'" get FreeSpace /value',
      gsub("/", "", path)
    )

    x <- system(cmd, intern = TRUE)

    free <- sub("FreeSpace=", "", grep("FreeSpace=", x, value = TRUE))

    as.numeric(free) / 1024^3
  } else {
    if (is.null(path)) {
      path <- "/"
    }

    x <- system2(
      "df",
      args = c("-Pk", shQuote(path)),
      stdout = TRUE
    )

    vals <- strsplit(x[2], "[[:space:]]+")[[1]]
    vals <- vals[vals != ""]

    # "Available" column in KB
    available_kb <- as.numeric(vals[4])

    available_kb / 1024^2
  }
}
