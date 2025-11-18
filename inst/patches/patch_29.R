# Patch 29

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 29. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    message(
      "Adding columns to control if a timeseries or sample series should be synchronized with a remote data store."
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries ADD COLUMN sync_remote BOOLEAN NOT NULL DEFAULT TRUE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD COLUMN sync_remote BOOLEAN NOT NULL DEFAULT TRUE;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes ADD COLUMN depth_to_bedrock_m NUMERIC;"
    )

    # Modify the spatial.vectors table so that it can store attributes beyond the current columns. Use JSONB format.
    message(
      "Modifying spatial.vectors table to add attributes column in JSONB format."
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.vectors ADD COLUMN attributes JSONB;"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '29' WHERE item = 'Last patch number';"
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

    message("Patch 29 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 29 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
