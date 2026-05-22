# Patch 46
# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 46."
)

message(
  "Working outside of a transaction so that indices can be created concurrently, without locking the tables for other users."
)

tryCatch(
  {
    message(
      "Adding a GIN index on the attributes column of spatial.vectors to speed up queries that filter on attributes."
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS attributes_idx
       ON spatial.vectors USING GIN (attributes);"
    )

    message(
      "Adding BTREE indices on results and samples tables to speed up queries.."
    )
    # Location-first date-range and location/date -> parameter lookups.
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS samples_location_datetime_sample_id_idx
ON discrete.samples (location_id, datetime, sample_id);"
    )

    # Location + sample media + date-range lookups.
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS samples_location_media_datetime_sample_id_idx
ON discrete.samples (location_id, media_id, datetime, sample_id);"
    )

    # Parameter-first result lookups used by plotting and selected-parameter
    # availability checks.
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS results_parameter_sample_id_idx
ON discrete.results (parameter_id, sample_id);"
    )

    # Speeds up advanced selector availability once samples have been narrowed.
    # This has the same leading keys as idx_results_sample_parameter, but includes
    # result qualifier dimensions so Postgres can answer selector queries with
    # fewer heap visits on large result tables.
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS results_sample_parameter_selector_values_idx
ON discrete.results (sample_id, parameter_id)
INCLUDE (
  result_type,
  sample_fraction_id,
  result_value_type,
  result_speciation_id
);"
    )

    # If we got here, everything worked and we can commit the transaction

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '46'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    message(
      "Patch 46 applied successfully."
    )
  },
  error = function(e) {
    stop(
      "Patch 46 failed on index creation. ",
      e$message
    )
  }
)
