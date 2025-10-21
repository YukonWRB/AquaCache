# Patch 16

# Initial checks #################
# # Ensure the user is postgres OR admin as this patch requires it
# check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
#
# if (!check$session_user %in% c("postgres", "admin")) {
#   stop("You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work.")
# }
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 16. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

message(
  "This patch modifies the checks on table 'qualifiers', allow for qualifiers which overlap in time."
)

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.check_qualifiers_overlap()
                         RETURNS trigger
                         LANGUAGE plpgsql
                        AS $function$
                                        BEGIN
                                            IF EXISTS (
                                                SELECT 1
                                                FROM qualifiers
                                                WHERE timeseries_id = NEW.timeseries_id
                                                AND qualifier_type_id = NEW.qualifier_type_id -- exclude qualifiers of different types
                                                AND qualifier_id IS DISTINCT FROM NEW.qualifier_id  -- Exclude the current row in an UPDATE
                                                AND (
                                                    (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                                                )
                                            ) THEN
                                                RAISE EXCEPTION 'Qualifiers of type % cannot overlap in time for the same timeseries_id. Failed on: %', NEW.qualifier_type_id, NEW.timeseries_id;
                                            END IF;
                                            RETURN NEW;
                                        END;
                                        $function$
                ;"
    )

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '16' WHERE item = 'Last patch number';"
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

    message("Patch 16 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 16 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
