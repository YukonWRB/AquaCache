check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (!check$session_user %in% c("postgres", "admin")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work."
  )
}

message(
  "Working on adding new vector indices. Indexes are changed concurrently where possible, so this script runs outside of a transaction."
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

    message("Refreshing planner statistics for spatial.vectors.")
    DBI::dbExecute(con, "ANALYZE spatial.vectors;")

    message("New vector indices created successfully.")

    # If we got here, everything worked and we can commit the transaction

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '47'
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
      "Patch 47 applied successfully."
    )
  },
  error = function(e) {
    stop("patch 47 failed: ", e$message)
  }
)
