# Patch 12
# Adjustments to view tables

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}


message(
  "Working on Patch 12. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    message(
      "Modifying the measurements_continuous view table so that deleted data is not shown."
    )
    DBI::dbExecute(
      con,
      "
                      CREATE OR REPLACE VIEW continuous.measurements_continuous_corrected AS
                      SELECT 
                        mc.timeseries_id,
                        mc.datetime,
                        mc.value AS value_raw,
                        CASE 
                          WHEN EXISTS (
                            SELECT 1
                            FROM corrections c
                            WHERE c.timeseries_id = mc.timeseries_id
                              AND c.start_dt <= mc.datetime
                              AND c.end_dt >= mc.datetime
                          )
                          THEN continuous.apply_corrections(mc.timeseries_id, mc.datetime, mc.value)
                          ELSE mc.value
                        END AS value_corrected,
                        mc.period,
                        mc.imputed
                      FROM measurements_continuous mc
                      JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id
                      WHERE 'public_reader'::text = ANY (ts.share_with)
                         OR EXISTS (
                               SELECT 1
                               FROM unnest(ts.share_with) role(role)
                               WHERE pg_has_role(CURRENT_USER, role.role::name, 'MEMBER'::text)
                                 AND role.role::name IN (SELECT pg_roles.rolname FROM pg_roles)
                             );
                      "
    )

    message(
      "Adding column 'jurisdictional_relevance' to the locations table, useful for filtering data for targeted applications."
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE locations ADD COLUMN IF NOT EXISTS jurisdictional_relevance BOOLEAN DEFAULT TRUE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN locations.jurisdictional_relevance IS 'Whether the location is publicly relevant to the jursdiction operating this database. Can be used for filtering results from public-facing applications if desired without labelling the location or associated timeseries as non-public.';"
    )

    #  Wrap up ###########
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '12' WHERE item = 'Last patch number';"
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

    message("Success! Patch 12 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 12 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
