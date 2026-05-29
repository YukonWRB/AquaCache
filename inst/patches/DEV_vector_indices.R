# DEV vector index tuning
# This is intentionally a DEV patch, not a numbered production patch.
# It can be sourced with an admin/postgres connection object named `con`.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (!check$session_user %in% c("postgres", "admin")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work."
  )
}

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

message(
  "Working on DEV_vector_indices. Indexes are changed concurrently where possible, so this script must run outside a transaction."
)

tryCatch(
  {
    message(
      "Dropping duplicate spatial.vectors(layer_name, feature_name) index. The unique index on (layer_name, feature_name, geom_type) can serve the same leading-column lookups."
    )
    DBI::dbExecute(
      con,
      "DROP INDEX CONCURRENTLY IF EXISTS spatial.idx_vectors_layer_feature_name;"
    )

    message(
      "Adding btree index for common layer_name + geom_type filters."
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS vectors_layer_geom_type_idx
       ON spatial.vectors (layer_name, geom_type);"
    )

    message(
      "Adding partial spatial indexes for heavily queried named vector layers."
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS vectors_geom_nhn_basins_idx
       ON spatial.vectors USING GIST (geom)
       WHERE layer_name = 'National Hydro Network - Basins';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS vectors_geom_drainage_basins_idx
       ON spatial.vectors USING GIST (geom)
       WHERE layer_name = 'Drainage basins';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS vectors_geom_roads_idx
       ON spatial.vectors USING GIST (geom)
       WHERE layer_name = 'Roads';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS vectors_geom_regions_idx
       ON spatial.vectors USING GIST (geom)
       WHERE layer_name IN (
         'Yukon regions - broad',
         'Provincial/Territorial Boundaries'
       );"
    )

    message("Refreshing planner statistics for spatial.vectors.")
    DBI::dbExecute(con, "ANALYZE spatial.vectors;")

    message("DEV_vector_indices completed successfully.")
  },
  error = function(e) {
    stop("DEV_vector_indices failed: ", e$message)
  }
)
