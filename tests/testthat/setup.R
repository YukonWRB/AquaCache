# If on CI, set environment variables which would otherwise be found in the user's .Renviron file.
# We're working with a micro postgres database here installed on the CI environment; see the .github/workflows/R-CMD-check.yaml file.
# This DB is created with AquaCache::create_test_db() and default parameters, expect for the username which uses 'postgres'
if (Sys.getenv("CI") == "true") {
  Sys.setenv(
    aquacacheName = "testdb",
    aquacacheHost = "localhost",
    aquacachePort = "5432",
    aquacacheUser = "runner",
    aquacachePass = "runner",
    aquacacheAdminUser = "runner",
    aquacacheAdminPass = "runner",
    AQUSER = "readonly",
    AQPASS = "WaterIsLife",
    AQSERVER = "https://yukon.aquaticinformatics.net/AQUARIUS"
  )
  message("Running on CI, setting environment accordingly.")
}

set.seed(123) # Set seed for reproducibility in tests


# Check for Postgres credentials; skip tests if not available
skip_if_no_postgres <- function() {
  testthat::skip_if_not_installed("RPostgres")
  required <- c(
    "aquacacheName",
    "aquacacheHost",
    "aquacachePort",
    "aquacacheAdminUser",
    "aquacacheAdminPass"
  )
  missing <- required[Sys.getenv(required, unset = "") == ""]
  if (length(missing) > 0) {
    testthat::skip(
      paste(
        "Postgres test database credentials not available:",
        paste(missing, collapse = ", ")
      )
    )
  }
}


# Helper function to connect to the test database; will skip tests if connection fails
connect_test <- function() {
  skip_if_no_postgres()
  tryCatch(
    {
      con <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("aquacacheName"),
        host = Sys.getenv("aquacacheHost"),
        port = Sys.getenv("aquacachePort"),
        user = Sys.getenv("aquacacheAdminUser"),
        password = Sys.getenv("aquacacheAdminPass")
      )
      DBI::dbExecute(con, "SET timezone = 'UTC'")
      con
    },
    error = function(err) {
      testthat::skip(paste(
        "Unable to connect to Postgres test database:",
        err$message
      ))
    }
  )
}


cleanup_postgres_session <- function(con) {
  suppressMessages(try((DBI::dbExecute(con, "ROLLBACK;")), silent = TRUE))
  DBI::dbDisconnect(con)
}


db_contracts_enabled <- function() {
  tolower(Sys.getenv("AQUACACHE_RUN_DB_CONTRACTS", unset = "false")) %in%
    c("1", "true", "yes", "y")
}


normalize_function_signature <- function(x) {
  x <- trimws(tolower(x))
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\s*,\\s*", ", ", x)
  x <- gsub("\\(\\s*", "(", x)
  x <- gsub("\\s*\\)", ")", x)
  x
}


skip_if_no_db_contracts <- function() {
  if (!db_contracts_enabled()) {
    testthat::skip(
      "DB contract tests are disabled. Set AQUACACHE_RUN_DB_CONTRACTS=true to enable."
    )
  }
  skip_if_no_postgres()
}


ensure_pgtap <- function(con) {
  installed <- DBI::dbGetQuery(
    con,
    "SELECT 1 AS ok FROM pg_extension WHERE extname = 'pgtap' LIMIT 1"
  )
  if (nrow(installed) == 0) {
    ok <- tryCatch(
      {
        DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS pgtap;")
        TRUE
      },
      error = function(e) FALSE
    )
    if (!ok) {
      testthat::skip(
        "pgTAP is not available. Install extension pgtap in the test database."
      )
    }
  }
}


db_function_inventory <- function(con) {
  out <- DBI::dbGetQuery(
    con,
    "
    SELECT
      lower(
        n.nspname || '.' || p.proname || '(' ||
          pg_get_function_identity_arguments(p.oid) || ')'
      ) AS signature,
      CASE
        WHEN p.prorettype = 'pg_catalog.trigger'::regtype THEN 'trigger'
        ELSE 'callable'
      END AS function_kind
    FROM pg_proc p
    JOIN pg_namespace n
      ON n.oid = p.pronamespace
    LEFT JOIN pg_depend d
      ON d.classid = 'pg_proc'::regclass
     AND d.objid = p.oid
     AND d.deptype = 'e'
    WHERE p.prokind = 'f'
      AND n.nspname IN (
        'application',
        'boreholes',
        'continuous',
        'discrete',
        'files',
        'public',
        'spatial'
      )
      AND d.objid IS NULL
    ORDER BY signature
    "
  )
  out$signature <- normalize_function_signature(out$signature)
  out$function_kind <- tolower(trimws(out$function_kind))
  out
}


read_db_function_manifest <- function() {
  out <- read.csv(
    testthat::test_path("data", "db_function_coverage_manifest.csv"),
    stringsAsFactors = FALSE
  )
  out$signature <- normalize_function_signature(out$signature)
  out$function_kind <- tolower(trimws(out$function_kind))
  out
}


db_contract_files <- function() {
  dir <- testthat::test_path("fixtures", "sql", "contracts")
  list.files(dir, pattern = "\\.sql$", full.names = TRUE)
}


read_db_contract_coverage <- function() {
  files <- db_contract_files()
  rows <- lapply(files, function(file) {
    lines <- readLines(file, warn = FALSE)
    covered <- trimws(sub(
      "^\\s*--\\s*@covers\\s+",
      "",
      lines[grepl(
        "^\\s*--\\s*@covers\\s+",
        lines
      )]
    ))
    if (length(covered) == 0) {
      return(NULL)
    }
    data.frame(
      signature = normalize_function_signature(covered),
      contract_file = basename(file),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(
      signature = character(),
      contract_file = character(),
      stringsAsFactors = FALSE
    ))
  }
  out
}


split_contract_statements <- function(lines) {
  marker_idx <- which(grepl("^\\s*--\\s*@statement\\s*$", lines))
  if (length(marker_idx) == 0) {
    stop("Contract SQL file is missing -- @statement markers.")
  }

  blocks <- vector("list", length(marker_idx))
  for (i in seq_along(marker_idx)) {
    start <- marker_idx[i] + 1
    end <- if (i < length(marker_idx)) marker_idx[i + 1] - 1 else length(lines)
    block <- paste(lines[start:end], collapse = "\n")
    blocks[[i]] <- trimws(block)
  }

  Filter(nzchar, blocks)
}


assert_pgtap_output <- function(tap_lines, contract_name) {
  tap_lines <- tap_lines[!is.na(tap_lines)]
  tap_lines <- trimws(tap_lines)
  tap_lines <- tap_lines[nzchar(tap_lines)]

  has_fail <- any(grepl("^not ok\\b", tap_lines))
  has_bail <- any(grepl("^Bail out!", tap_lines, ignore.case = TRUE))

  if (has_fail || has_bail) {
    failing <- tap_lines[
      grepl("^not ok\\b", tap_lines) |
        grepl("^Bail out!", tap_lines, ignore.case = TRUE) |
        grepl("^#", tap_lines)
    ]
    stop(
      "pgTAP contract failed: ",
      contract_name,
      "\n",
      paste(failing, collapse = "\n")
    )
  }
}


run_pgtap_contract_file <- function(con, file, statement_timeout = "20s") {
  lines <- readLines(file, warn = FALSE)
  statements <- split_contract_statements(lines)

  DBI::dbExecute(con, "BEGIN;")
  on.exit(
    suppressMessages(try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)),
    add = TRUE
  )
  DBI::dbExecute(
    con,
    paste0("SET LOCAL statement_timeout = '", statement_timeout, "';")
  )

  for (stmt in statements) {
    is_select <- grepl("^\\s*(SELECT|WITH)\\b", stmt, ignore.case = TRUE)
    is_finish <- grepl("\\bfinish\\s*\\(\\s*\\)", stmt, ignore.case = TRUE)

    if (is_select) {
      out <- DBI::dbGetQuery(con, stmt)
      if (is_finish) {
        tap_lines <- as.character(out[[1]])
        assert_pgtap_output(tap_lines, basename(file))
      }
    } else {
      DBI::dbExecute(con, stmt)
    }
  }
}
