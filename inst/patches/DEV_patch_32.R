# Patch 32

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 32. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # Add a column to location_types so that there can be a class abbreviation for each type
    type_suffix <- data.frame(
      type = c(
        "river/stream",
        "lake/pond",
        "estuary",
        "wetland",
        "ocean",
        "stormwater storage",
        "sewer sanitary",
        "sewer storm",
        "reservoir",
        "canal/ditch drainage",
        "canal/ditch irrigation",
        "well",
        "spring/seep",
        "water well",
        "meteorological station",
        "meteorological",
        "snowpack",
        "atmosphere"
      ),
      suffix = c(
        "SW",
        "SW",
        "OC",
        "SW",
        "OC",
        "WW",
        "WW",
        "WW",
        "SW",
        "SW",
        "SW",
        "GW",
        "GW",
        "GW",
        "MET",
        "MET",
        "MET",
        "MET"
      )
    )

    # Check if column exists already (in case patch is re-run)
    column_check <- DBI::dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns WHERE table_name = 'location_types' AND column_name = 'type_suffix';"
    )

    if (nrow(column_check) == 0) {
      DBI::dbExecute(
        con,
        "ALTER TABLE location_types ADD COLUMN type_suffix TEXT;"
      )
    }

    for (i in 1:nrow(type_suffix)) {
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE location_types SET type_suffix = '",
          type_suffix$suffix[i],
          "' WHERE type = '",
          type_suffix$type[i],
          "';"
        )
      )
    }

    # Check if any type_suffix is NULL, warn user if so
    null_check <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS null_count FROM location_types WHERE type_suffix IS NULL;"
    )

    if (null_check$null_count > 0) {
      warning(
        "There are ",
        null_check$null_count,
        " location_types with NULL type_suffix. You MUST review and update these manually."
      )
    }

    # Re-jig location ownership and data sharing agreements
    DBI::dbExecute(
      con,
      "DROP TABLE IF EXISTS locations_metadata_owners_operators;"
    )

    # Add a column 'data_sharing_agreement_id' to timeseries table
    DBI::dbExecute(
      con,
      "ALTER TABLE timeseries ADD COLUMN data_sharing_agreement_id INTEGER REFERENCES files.documents(document_id);"
    )
    # Re-use a function to check document type for data sharing agreement
    DBI::dbExecute(
      con,
      "
    create trigger trg_check_data_sharing_agreement before insert or update on continuous.timeseries for each row execute function check_data_sharing_agreement()
    ;"
    )

    # Drop column 'data_sharing_agreement_id' from locations table
    DBI::dbExecute(
      con,
      "ALTER TABLE locations DROP COLUMN IF EXISTS data_sharing_agreement_id;"
    )

    # Now the big changes to location codes.
    # Drop 'location' from table 'timeseries'

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '32' WHERE item = 'Last patch number';"
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

    message("Patch 32 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 32 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
