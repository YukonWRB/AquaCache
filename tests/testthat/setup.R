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

# The SQL fixture can contain explicit primary key values with stale backing sequences.
test_db_state <- new.env(parent = emptyenv())
test_db_state$sequences_synced <- FALSE


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


sync_test_sequences <- function(con) {
  DBI::dbExecute(
    con,
    "
    DO $$
    DECLARE
      rec record;
      max_id bigint;
    BEGIN
      FOR rec IN
        WITH sequence_columns AS (
          SELECT
            cols.table_schema,
            cols.table_name,
            cols.column_name,
            pg_get_serial_sequence(
              format('%I.%I', cols.table_schema, cols.table_name),
              cols.column_name
            ) AS sequence_name
          FROM information_schema.columns cols
          WHERE cols.table_schema NOT IN ('pg_catalog', 'information_schema')
        )
        SELECT
          table_schema,
          table_name,
          column_name,
          sequence_name
        FROM sequence_columns
        WHERE sequence_name IS NOT NULL
          AND has_sequence_privilege(sequence_name, 'UPDATE')
      LOOP
        EXECUTE format(
          'SELECT COALESCE(MAX(%1$I), 0) FROM %2$I.%3$I',
          rec.column_name,
          rec.table_schema,
          rec.table_name
        ) INTO max_id;

        EXECUTE format(
          'SELECT setval(%L::regclass, %s, %s);',
          rec.sequence_name,
          GREATEST(max_id, 1),
          CASE WHEN max_id > 0 THEN 'true' ELSE 'false' END
        );
      END LOOP;
    END
    $$;
    "
  )
}


# Helper function to connect to the test database; will skip tests if connection fails
connect_test <- function() {
  skip_if_no_postgres()
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("aquacacheName"),
      host = Sys.getenv("aquacacheHost"),
      port = Sys.getenv("aquacachePort"),
      user = Sys.getenv("aquacacheAdminUser"),
      password = Sys.getenv("aquacacheAdminPass")
    ),
    error = function(err) {
      testthat::skip(paste(
        "Unable to connect to Postgres test database:",
        err$message
      ))
    }
  )
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  if (!isTRUE(test_db_state$sequences_synced)) {
    sync_test_sequences(con)
    test_db_state$sequences_synced <- TRUE
  }
  con
}


cleanup_postgres_session <- function(con) {
  suppressMessages(try((DBI::dbExecute(con, "ROLLBACK;")), silent = TRUE))
  DBI::dbDisconnect(con)
}
