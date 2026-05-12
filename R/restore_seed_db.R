#' Restore an AquaCache seed database from a SQL dump
#'
#' Restores a plain `.sql` or gzipped SQL AquaCache seed dump into a new
#' PostgreSQL database. The target database must be fresh unless `replace = TRUE`
#' is supplied. System databases such as `postgres`, `template0`, and `template1`
#' are always refused as targets. The restore also ensures the cluster-level
#' `public_reader` login role exists with password `"aquacache"` because
#' AquaCache row-level security policies and grants can reference it during
#' restore, and that login is useful for read-only applications.
#'
#' The restore is run through the PostgreSQL `psql` command line utility because
#' dumps created by `pg_dump` can contain `COPY` data, functions, triggers, and
#' other SQL that should be interpreted by PostgreSQL's own client.
#'
#' @param file Path to a plain `.sql` dump or gzipped SQL dump.
#' @param name Target database name to create and restore into. Defaults to
#'   `aquacacheName` from the user's `.Renviron`.
#' @param host Database host address. Defaults to `aquacacheHost`.
#' @param port Database port. Defaults to `aquacachePort`.
#' @param username PostgreSQL user with permission to create and drop the target
#'   database. Defaults to `aquacacheAdminUser`.
#' @param password Password for `username`. Defaults to `aquacacheAdminPass`.
#' @param maintenance_db Existing database used for the administrative
#'   connection that creates/drops `name`. Defaults to `"postgres"`.
#' @param replace If `TRUE`, drop and recreate an existing target database.
#'   `postgres`, `template0`, and `template1` are not allowed.
#' @param require_current_patch If `TRUE`, the restored database must end at the
#'   latest AquaCache patch available in the installed package.
#' @param apply_patches If `TRUE`, apply package patches after restore when the
#'   dump is older than the installed package.
#' @param psql Path to the `psql` utility. If `NULL`, the package searches the
#'   system PATH and common PostgreSQL installation directories.
#' @param cleanup_on_error If `TRUE`, drop the newly created target database if
#'   restore or validation fails.
#'
#' @return Invisibly returns a list with the restored database name, connection
#'   host/port, and final patch number.
#' @export
restore_seed_db <- function(
  file,
  name = Sys.getenv("aquacacheName", "aquacache"),
  host = Sys.getenv("aquacacheHost", "localhost"),
  port = Sys.getenv("aquacachePort", "5432"),
  username = Sys.getenv("aquacacheAdminUser", "postgres"),
  password = Sys.getenv("aquacacheAdminPass", "postgres"),
  maintenance_db = "postgres",
  replace = FALSE,
  require_current_patch = TRUE,
  apply_patches = TRUE,
  psql = NULL,
  cleanup_on_error = TRUE
) {
  file <- aquacache_validate_seed_dump_file(file)
  name <- aquacache_validate_restore_target_name(name)
  maintenance_db <- aquacache_validate_restore_maintenance_db(
    maintenance_db,
    name
  )

  aquacache_validate_restore_connection_arg(host, "host")
  aquacache_validate_restore_connection_arg(port, "port")
  aquacache_validate_restore_connection_arg(username, "username")

  replace <- isTRUE(replace)
  require_current_patch <- isTRUE(require_current_patch)
  apply_patches <- isTRUE(apply_patches)
  cleanup_on_error <- isTRUE(cleanup_on_error)

  dump_uses_restrict <- aquacache_sql_dump_uses_restrict(file)

  if (is.null(psql)) {
    psql <- find_postgres_utility(
      "psql",
      version_ok = if (dump_uses_restrict) {
        aquacache_psql_supports_restrict
      } else {
        NULL
      }
    )
  }

  if (!nzchar(psql) || !file.exists(psql)) {
    if (isTRUE(dump_uses_restrict)) {
      stop(
        "The SQL dump contains psql \\restrict/\\unrestrict commands, ",
        "but no compatible psql client was found. Install PostgreSQL client ",
        "tools 13.22+, 14.19+, 15.14+, 16.10+, 17.6+, or 18+, or pass ",
        "`psql =` explicitly.",
        call. = FALSE
      )
    }

    stop(
      "The psql utility could not be found. Install PostgreSQL client tools ",
      "or pass `psql` explicitly.",
      call. = FALSE
    )
  }

  latest_patch <- aquacache_latest_patch_number()
  metadata <- aquacache_read_seed_dump_metadata(file)
  metadata_patch <- aquacache_seed_dump_metadata_patch(metadata)

  if (!is.na(metadata_patch)) {
    if (metadata_patch > latest_patch) {
      stop(
        "The seed dump was created at AquaCache patch ",
        metadata_patch,
        ", but this package only has patches through ",
        latest_patch,
        ". Install a newer AquaCache package before restoring this dump."
      )
    }
    if (
      require_current_patch &&
        !apply_patches &&
        metadata_patch < latest_patch
    ) {
      stop(
        "The seed dump was created at AquaCache patch ",
        metadata_patch,
        ", but this package expects patch ",
        latest_patch,
        ". Use a current dump or set `apply_patches = TRUE`."
      )
    }
  }

  sql_dump <- aquacache_materialize_sql_dump(file)
  admin_con <- NULL
  target_con <- NULL
  target_identifier <- NULL
  created_target <- FALSE
  restore_complete <- FALSE

  on.exit(
    {
      if (!is.null(target_con) && DBI::dbIsValid(target_con)) {
        try(DBI::dbDisconnect(target_con), silent = TRUE)
      }

      if (!is.null(admin_con) && DBI::dbIsValid(admin_con)) {
        if (
          cleanup_on_error &&
            created_target &&
            !restore_complete &&
            !is.null(target_identifier)
        ) {
          try(
            DBI::dbExecute(
              admin_con,
              sprintf(
                "DROP DATABASE IF EXISTS %s WITH (FORCE);",
                target_identifier
              )
            ),
            silent = TRUE
          )
        }
        try(DBI::dbDisconnect(admin_con), silent = TRUE)
      }

      sql_dump$cleanup()
    },
    add = TRUE
  )

  admin_con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = maintenance_db,
    host = host,
    port = port,
    user = username,
    password = password
  )
  DBI::dbExecute(admin_con, "SET timezone = 'UTC'")
  aquacache_ensure_restore_roles(admin_con)
  target_identifier <- as.character(DBI::dbQuoteIdentifier(admin_con, name))

  existing <- DBI::dbGetQuery(
    admin_con,
    "SELECT datname FROM pg_database WHERE datname = $1;",
    params = list(name)
  )

  if (nrow(existing) > 0) {
    if (!replace) {
      stop(
        "Database '",
        name,
        "' already exists. Choose a new name or set `replace = TRUE`."
      )
    }

    DBI::dbExecute(
      admin_con,
      sprintf("DROP DATABASE %s WITH (FORCE);", target_identifier)
    )
  }

  DBI::dbExecute(
    admin_con,
    sprintf("CREATE DATABASE %s;", target_identifier)
  )
  created_target <- TRUE

  aquacache_run_psql_file(
    psql = psql,
    file = sql_dump$path,
    name = name,
    host = host,
    port = port,
    username = username,
    password = password
  )

  target_con <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = name,
    host = host,
    port = port,
    user = username,
    password = password
  )
  DBI::dbExecute(target_con, "SET timezone = 'UTC'")

  patch_number <- aquacache_db_patch_number(target_con)
  if (is.na(patch_number)) {
    stop(
      "The restored database does not contain information.version_info with ",
      "a 'Last patch number' row. This does not look like an AquaCache seed dump."
    )
  }

  if (!is.na(metadata_patch) && patch_number != metadata_patch) {
    stop(
      "The dump metadata reports AquaCache patch ",
      metadata_patch,
      ", but the restored database reports patch ",
      patch_number,
      ". The dump metadata and database contents are inconsistent."
    )
  }

  if (patch_number > latest_patch) {
    stop(
      "The restored database is at AquaCache patch ",
      patch_number,
      ", but this package only has patches through ",
      latest_patch,
      ". Install a newer AquaCache package before using this database."
    )
  }

  if (patch_number < latest_patch) {
    if (apply_patches) {
      aquacache_apply_patches(
        con = target_con,
        current_patch = patch_number,
        latest_patch = latest_patch
      )
      patch_number <- aquacache_db_patch_number(target_con)
    } else if (require_current_patch) {
      stop(
        "The restored database is at AquaCache patch ",
        patch_number,
        ", but this package expects patch ",
        latest_patch,
        ". Re-run with `apply_patches = TRUE` or use a current dump."
      )
    } else {
      warning(
        "The restored database is at AquaCache patch ",
        patch_number,
        ", but this package has patches through ",
        latest_patch,
        ".",
        call. = FALSE
      )
    }
  }

  if (require_current_patch && patch_number != latest_patch) {
    stop(
      "Patch validation failed. The restored database ended at patch ",
      patch_number,
      ", but patch ",
      latest_patch,
      " is required."
    )
  }

  DBI::dbExecute(
    target_con,
    aquacache_public_reader_grants_sql(
      con = target_con,
      excluded_tables = c(
        "continuous.measurements_continuous",
        "application.api_requests",
        "application.feedback",
        "application.images",
        "application.notifications"
      ),
      excluded_functions = NULL,
      excluded_schemas = c(
        "audit"
      )
    )
  )

  restore_complete <- TRUE
  message(
    "Restored AquaCache seed database '",
    name,
    "' at patch ",
    patch_number,
    "."
  )

  invisible(list(
    database = name,
    host = host,
    port = port,
    patch_number = patch_number,
    file = file,
    psql = psql
  ))
}

#' @keywords internal
#' @noRd
aquacache_validate_seed_dump_file <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop("`file` must be a single path to a .sql or gzipped SQL dump.")
  }

  file <- normalizePath(file, winslash = "\\", mustWork = TRUE)
  lower_file <- tolower(file)
  if (!grepl("\\.sql$", lower_file) && !grepl("\\.gz$", lower_file)) {
    stop("`file` must point to a plain .sql dump or gzipped SQL dump.")
  }

  file
}

#' @keywords internal
#' @noRd
aquacache_validate_restore_target_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a single target database name.")
  }

  name <- trimws(name)
  if (!nzchar(name)) {
    stop("`name` must not be empty.")
  }
  if (nchar(name, type = "bytes") > 63L) {
    stop(
      "`name` must be 63 bytes or fewer, matching PostgreSQL's identifier limit."
    )
  }

  blocked <- c("postgres", "template0", "template1")
  if (tolower(name) %in% blocked) {
    stop(
      "Refusing to restore into system PostgreSQL database '",
      name,
      "'. Choose a new target database name."
    )
  }

  name
}

#' @keywords internal
#' @noRd
aquacache_validate_restore_maintenance_db <- function(maintenance_db, name) {
  if (
    !is.character(maintenance_db) ||
      length(maintenance_db) != 1L ||
      is.na(maintenance_db)
  ) {
    stop("`maintenance_db` must be a single existing database name.")
  }

  maintenance_db <- trimws(maintenance_db)
  if (!nzchar(maintenance_db)) {
    stop("`maintenance_db` must not be empty.")
  }

  if (tolower(maintenance_db) == tolower(name)) {
    stop("`maintenance_db` must be different from the target database `name`.")
  }

  maintenance_db
}

#' @keywords internal
#' @noRd
aquacache_validate_restore_connection_arg <- function(value, label) {
  if (identical(label, "port")) {
    if (
      !(is.character(value) || is.numeric(value) || is.integer(value)) ||
        length(value) != 1L ||
        is.na(value)
    ) {
      stop("`port` must be a single non-empty value.")
    }
    if (!nzchar(trimws(as.character(value)))) {
      stop("`port` must be a single non-empty value.")
    }
    return(invisible(TRUE))
  }

  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("`", label, "` must be a single non-empty value.")
  }
  if (!nzchar(trimws(value))) {
    stop("`", label, "` must be a single non-empty value.")
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
aquacache_patch_dir <- function() {
  local_dir <- file.path(getwd(), "inst", "patches")
  if (dir.exists(local_dir)) {
    return(local_dir)
  }

  patch_dir <- system.file("patches", package = "AquaCache")
  if (!nzchar(patch_dir) || !dir.exists(patch_dir)) {
    stop("Could not find the AquaCache patch directory.")
  }

  patch_dir
}

#' @keywords internal
#' @noRd
aquacache_latest_patch_number <- function() {
  patch_files <- list.files(
    aquacache_patch_dir(),
    pattern = "^patch_[0-9]+\\.R$",
    full.names = FALSE
  )

  if (!length(patch_files)) {
    stop("No AquaCache patch files were found.")
  }

  max(as.integer(gsub("^patch_([0-9]+)\\.R$", "\\1", patch_files)))
}

#' @keywords internal
#' @noRd
aquacache_patch_file <- function(patch_number) {
  patch_file <- file.path(
    aquacache_patch_dir(),
    paste0("patch_", patch_number, ".R")
  )
  if (!file.exists(patch_file)) {
    stop("Patch file not found for AquaCache patch ", patch_number, ".")
  }
  patch_file
}

#' @keywords internal
#' @noRd
aquacache_db_patch_number <- function(con) {
  if (
    !DBI::dbExistsTable(
      con,
      DBI::Id(schema = "information", table = "version_info")
    )
  ) {
    return(NA_integer_)
  }

  patch <- DBI::dbGetQuery(
    con,
    "SELECT version
     FROM information.version_info
     WHERE item = 'Last patch number';"
  )

  if (nrow(patch) == 0) {
    return(NA_integer_)
  }

  patch_number <- suppressWarnings(as.integer(patch$version[[1]]))
  if (is.na(patch_number)) {
    stop(
      "The database patch number in information.version_info is not an integer."
    )
  }

  patch_number
}

#' @keywords internal
#' @noRd
aquacache_apply_patches <- function(con, current_patch, latest_patch) {
  current_patch <- as.integer(current_patch)
  latest_patch <- as.integer(latest_patch)

  if (is.na(current_patch) || is.na(latest_patch)) {
    stop("Patch numbers must be available before applying patches.")
  }
  if (current_patch >= latest_patch) {
    return(invisible(TRUE))
  }

  for (patch in seq.int(current_patch + 1L, latest_patch)) {
    tryCatch(
      {
        source(aquacache_patch_file(patch), local = TRUE)
      },
      error = function(e) {
        stop(
          "An error occurred while applying AquaCache patch ",
          patch,
          ": ",
          e$message
        )
      }
    )
  }

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
aquacache_public_reader_role_sql <- function() {
  "DO $aquacache$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_catalog.pg_roles
    WHERE rolname = 'public_reader'
  ) THEN
    EXECUTE 'CREATE ROLE public_reader LOGIN PASSWORD ''aquacache''';
  ELSE
    EXECUTE 'ALTER ROLE public_reader LOGIN PASSWORD ''aquacache''';
  END IF;
END
$aquacache$;
"
}

#' @keywords internal
#' @noRd
aquacache_public_reader_grants_sql <- function(
  con,
  excluded_tables = NULL,
  excluded_functions = NULL,
  excluded_schemas = NULL
) {
  excluded_tables_sql <- aquacache_sql_text_array(con, excluded_tables)
  excluded_functions_sql <- aquacache_sql_text_array(con, excluded_functions)
  excluded_schemas_sql <- aquacache_sql_text_array(con, excluded_schemas)

  paste0(
    "DO $aquacache$
DECLARE
  r record;
  excluded_tables text[] := ",
    excluded_tables_sql,
    ";
  excluded_functions text[] := ",
    excluded_functions_sql,
    ";
  excluded_schemas text[] := ",
    excluded_schemas_sql,
    ";
BEGIN
  -- Schema USAGE
  FOR r IN
    SELECT n.nspname AS schema_name
    FROM pg_catalog.pg_namespace n
    WHERE n.nspname NOT LIKE 'pg_%'
      AND n.nspname <> 'information_schema'
      AND n.nspname <> ALL(excluded_schemas)
  LOOP
    EXECUTE format(
      'GRANT USAGE ON SCHEMA %I TO public_reader',
      r.schema_name
    );
  END LOOP;

  -- Table / view SELECT
  FOR r IN
    SELECT
      n.nspname AS schema_name,
      c.relname AS object_name
    FROM pg_catalog.pg_class c
    JOIN pg_catalog.pg_namespace n
      ON n.oid = c.relnamespace
    WHERE c.relkind IN ('r', 'p', 'v', 'm', 'f')
      AND n.nspname NOT LIKE 'pg_%'
      AND n.nspname <> 'information_schema'
      AND n.nspname <> ALL(excluded_schemas)
      AND format('%I.%I', n.nspname, c.relname) <> ALL(excluded_tables)
  LOOP
    EXECUTE format(
      'GRANT SELECT ON TABLE %I.%I TO public_reader',
      r.schema_name,
      r.object_name
    );
  END LOOP;

  -- Sequence USAGE/SELECT, useful if any views/functions expose currval/nextval patterns
  FOR r IN
    SELECT
      n.nspname AS schema_name,
      c.relname AS object_name
    FROM pg_catalog.pg_class c
    JOIN pg_catalog.pg_namespace n
      ON n.oid = c.relnamespace
    WHERE c.relkind = 'S'
      AND n.nspname NOT LIKE 'pg_%'
      AND n.nspname <> 'information_schema'
      AND n.nspname <> ALL(excluded_schemas)
  LOOP
    EXECUTE format(
      'GRANT USAGE, SELECT ON SEQUENCE %I.%I TO public_reader',
      r.schema_name,
      r.object_name
    );
  END LOOP;

  -- Function / procedure EXECUTE
  FOR r IN
    SELECT
      n.nspname AS schema_name,
      p.proname AS function_name,
      pg_catalog.pg_get_function_identity_arguments(p.oid) AS identity_args
    FROM pg_catalog.pg_proc p
    JOIN pg_catalog.pg_namespace n
      ON n.oid = p.pronamespace
    WHERE n.nspname NOT LIKE 'pg_%'
      AND n.nspname <> 'information_schema'
      AND n.nspname <> ALL(excluded_schemas)
      AND format(
            '%I.%I(%s)',
            n.nspname,
            p.proname,
            pg_catalog.pg_get_function_identity_arguments(p.oid)
          ) <> ALL(excluded_functions)
  LOOP
    EXECUTE format(
      'GRANT EXECUTE ON FUNCTION %I.%I(%s) TO public_reader',
      r.schema_name,
      r.function_name,
      r.identity_args
    );
  END LOOP;
END
$aquacache$;
"
  )
}

#' @keywords internal
#' @noRd
aquacache_sql_text_array <- function(con, x) {
  if (is.null(x) || length(x) == 0L) {
    return("ARRAY[]::text[]")
  }

  if (!is.character(x) || anyNA(x)) {
    stop("Grant exclusion lists must be character vectors without NA values.")
  }

  paste0(
    "ARRAY[",
    paste(DBI::dbQuoteString(con, x), collapse = ", "),
    "]::text[]"
  )
}

#' @keywords internal
#' @noRd
aquacache_ensure_restore_roles <- function(con) {
  tryCatch(
    {
      DBI::dbExecute(con, aquacache_public_reader_role_sql())
    },
    error = function(e) {
      stop(
        "Role 'public_reader' could not be created or updated. ",
        "Restore the seed database as a PostgreSQL user with CREATEROLE ",
        "privileges, or create/update role 'public_reader' before restoring. ",
        "Original error: ",
        e$message
      )
    }
  )

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
aquacache_seed_dump_metadata_lines <- function(
  source_database,
  patch_number,
  package_version = as.character(utils::packageVersion("AquaCache"))
) {
  if (is.na(patch_number)) {
    patch_number <- ""
  }

  c(
    "-- AquaCache seed database dump",
    paste0("-- AquaCache package version: ", package_version),
    paste0("-- AquaCache patch number: ", patch_number),
    paste0("-- AquaCache source database: ", source_database),
    paste0(
      "-- AquaCache created at UTC: ",
      format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    "--"
  )
}

#' @keywords internal
#' @noRd
aquacache_prepend_lines_to_file <- function(file, lines) {
  tmp <- tempfile(fileext = ".sql")
  in_con <- file(file, open = "rb")
  out_con <- file(tmp, open = "wb")
  on.exit(
    {
      try(close(in_con), silent = TRUE)
      try(close(out_con), silent = TRUE)
      unlink(tmp)
    },
    add = TRUE
  )

  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), out_con)

  repeat {
    chunk <- readBin(in_con, what = "raw", n = 1024L * 1024L)
    if (!length(chunk)) {
      break
    }
    writeBin(chunk, out_con)
  }

  close(in_con)
  close(out_con)

  if (!file.copy(tmp, file, overwrite = TRUE)) {
    stop("Failed to write AquaCache metadata to dump file: ", file)
  }

  invisible(file)
}

#' @keywords internal
#' @noRd
aquacache_read_seed_dump_metadata <- function(file, max_lines = 200L) {
  con <- if (grepl("\\.gz$", tolower(file))) {
    gzfile(file, open = "rt")
  } else {
    file(file, open = "rt")
  }
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, n = max_lines, warn = FALSE)
  if (!length(lines)) {
    return(list())
  }

  matches <- regmatches(
    lines,
    regexec("^--\\s*AquaCache\\s+([^:]+):\\s*(.*)$", lines)
  )
  matches <- matches[lengths(matches) == 3L]
  if (!length(matches)) {
    return(list())
  }

  keys <- vapply(
    matches,
    function(x) {
      key <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(x[[2]])))
      gsub("^_|_$", "", key)
    },
    character(1)
  )
  values <- lapply(matches, function(x) trimws(x[[3]]))
  stats::setNames(values, keys)
}


#' @keywords internal
#' @noRd
aquacache_seed_dump_metadata_patch <- function(metadata) {
  patch <- metadata[["patch_number"]]
  if (is.null(patch) || length(patch) == 0L || !nzchar(patch[[1]])) {
    return(NA_integer_)
  }

  patch_number <- suppressWarnings(as.integer(patch[[1]]))
  if (is.na(patch_number)) {
    stop("The AquaCache patch number in the dump metadata is not an integer.")
  }

  patch_number
}


#' @keywords internal
#' @noRd
aquacache_materialize_sql_dump <- function(file) {
  if (!grepl("\\.gz$", tolower(file))) {
    return(list(
      path = file,
      cleanup = function() invisible(TRUE)
    ))
  }

  tmp <- tempfile(fileext = ".sql")
  in_con <- gzfile(file, open = "rb")
  out_con <- file(tmp, open = "wb")
  on.exit(
    {
      try(close(in_con), silent = TRUE)
      try(close(out_con), silent = TRUE)
    },
    add = TRUE
  )

  repeat {
    chunk <- readBin(in_con, what = "raw", n = 1024L * 1024L)
    if (!length(chunk)) {
      break
    }
    writeBin(chunk, out_con)
  }

  close(in_con)
  close(out_con)

  list(
    path = tmp,
    cleanup = function() unlink(tmp)
  )
}


#' @keywords internal
#' @noRd
aquacache_run_psql_file <- function(
  psql,
  file,
  name,
  host,
  port,
  username,
  password
) {
  stdout_file <- tempfile(fileext = ".log")
  stderr_file <- tempfile(fileext = ".log")
  old_password <- Sys.getenv("PGPASSWORD", unset = NA_character_)

  on.exit(
    {
      if (is.na(old_password)) {
        Sys.unsetenv("PGPASSWORD")
      } else {
        Sys.setenv(PGPASSWORD = old_password)
      }
      unlink(c(stdout_file, stderr_file))
    },
    add = TRUE
  )

  if (!is.null(password) && nzchar(password)) {
    Sys.setenv(PGPASSWORD = password)
  } else {
    Sys.unsetenv("PGPASSWORD")
  }

  status <- system2(
    command = psql,
    args = c(
      "-X",
      "--no-psqlrc",
      "-v",
      "ON_ERROR_STOP=1",
      "-w",
      "-U",
      username,
      "-h",
      host,
      "-p",
      as.character(port),
      "-d",
      name,
      "-f",
      file
    ),
    stdout = stdout_file,
    stderr = stderr_file
  )

  if (is.null(status)) {
    status <- 0L
  }

  if (!identical(as.integer(status), 0L)) {
    out <- aquacache_tail_file(stdout_file)
    err <- aquacache_tail_file(stderr_file)
    stop(
      "psql failed while restoring the seed database (exit ",
      status,
      ").\nstdout:\n",
      out,
      "\nstderr:\n",
      err
    )
  }

  invisible(TRUE)
}


#' @keywords internal
#' @noRd
aquacache_tail_file <- function(file, n = 80L) {
  if (!file.exists(file) || file.info(file)$size == 0) {
    return("")
  }

  paste(tail(readLines(file, warn = FALSE), n), collapse = "\n")
}

#' @keywords internal
#' @noRd
aquacache_sql_dump_uses_restrict <- function(file) {
  con <- if (grepl("\\.gz$", tolower(file))) {
    gzfile(file, open = "rt")
  } else {
    file(file, open = "rt")
  }

  on.exit(close(con), add = TRUE)

  repeat {
    lines <- readLines(con, n = 1000L, warn = FALSE)

    if (!length(lines)) {
      return(FALSE)
    }

    if (any(grepl("^\\\\(un)?restrict(\\s|$)", lines))) {
      return(TRUE)
    }
  }
}
