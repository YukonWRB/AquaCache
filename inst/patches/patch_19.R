# Patch 19

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 19. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

message("This patch adds indices to several tables for faster queries.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    DBI::dbExecute(con, "CREATE INDEX ON measurements_continuous (datetime);")
    DBI::dbExecute(con, "CREATE INDEX ON measurements_calculated_daily (date);")

    DBI::dbExecute(con, "CREATE INDEX ON images (datetime);")
    DBI::dbExecute(con, "CREATE INDEX ON images (location_id);")

    DBI::dbExecute(con, "CREATE INDEX ON documents (type);")

    DBI::dbExecute(con, "CREATE INDEX ON rasters_reference (valid_from);")
    DBI::dbExecute(con, "CREATE INDEX ON rasters_reference (valid_to);")
    DBI::dbExecute(con, "CREATE INDEX ON rasters_reference (raster_series_id);")

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '19' WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion("AquaCache")),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")
    attr(con, "active_transaction") <- FALSE

    message("Patch 19 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 19 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
