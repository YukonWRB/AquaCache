# Patch 36

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 36. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # Fix the instrument deployment check function (now checks z_id, instrument_id, and correct start/end column names)
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_instrument_meta_overlap()
      RETURNS trigger
      LANGUAGE plpgsql
      AS $function$
                 BEGIN
                     IF EXISTS (
                         SELECT 1
                         FROM locations_metadata_instruments
                         WHERE location_id = NEW.location_id
                         AND sub_location_id = NEW.sub_location_id -- often NULL
                         AND z_id = NEW.z_id -- often NULL
                         AND instrument_id = NEW.instrument_id -- more than one instrument can be deployed at the same location, but not the same instrument
                         AND metadata_id != NEW.metadata_id  -- Exclude the current row in an UPDATE
                         AND (
                             NEW.start_datetime < end_datetime AND (NEW.end_datetime IS NULL OR NEW.end_datetime > start_datetime)
                         )
                     ) THEN
                         RAISE EXCEPTION 'Time range overlap detected for location_id: %, sub_location_id: %, z_id: %, instrument_id: %', NEW.location_id, NEW.sub_location_id, NEW.z_id, NEW.instrument_id;
                     END IF;
                     RETURN NEW;
                 END;
                 $function$
   ;
   "
    )

    # Extend session table with close metadata
    DBI::dbExecute(
      con,
      "ALTER TABLE application.shiny_app_usage
   ADD COLUMN IF NOT EXISTS session_end_source TEXT;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE application.shiny_app_usage
   ADD COLUMN IF NOT EXISTS session_end_note TEXT;"
    )
    DBI::dbExecute(
      con,
      "GRANT UPDATE (session_end, session_end_source, session_end_note, error_message, login_to)
   ON application.shiny_app_usage TO PUBLIC;"
    )

    # Event table for page-level and action-level usage tracking
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.shiny_app_usage_event (
     id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
     usage_id INTEGER NOT NULL REFERENCES application.shiny_app_usage(id) ON DELETE CASCADE,
     event_ts TIMESTAMPTZ NOT NULL DEFAULT NOW(),
     event_type TEXT NOT NULL,
     page_id TEXT NULL,
     app_side TEXT NOT NULL CHECK (app_side IN ('public', 'admin', 'system')),
     duration_ms INTEGER NULL CHECK (duration_ms >= 0),
     payload JSONB NOT NULL DEFAULT '{}'::jsonb
   );"
    )

    # Comments
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.shiny_app_usage_event IS
   'Append-only usage events for YGwater sessions (page enters/leaves, downloads, plot requests, map filter activity, and session close diagnostics).';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.shiny_app_usage_event.usage_id IS
   'Foreign key to application.shiny_app_usage.id for the parent session record.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.shiny_app_usage_event.app_side IS
   'Application side classification: public, admin, or system.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.shiny_app_usage_event.payload IS
   'Redacted event metadata captured as JSONB (ids, ranges, counts, and flags).';"
    )

    # Indexes for reporting queries
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_usage_ts_idx
   ON application.shiny_app_usage_event (usage_id, event_ts);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_type_ts_idx
   ON application.shiny_app_usage_event (event_type, event_ts);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_page_ts_idx
   ON application.shiny_app_usage_event (page_id, event_ts);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_side_ts_idx
   ON application.shiny_app_usage_event (app_side, event_ts);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS shiny_app_usage_event_payload_gin_idx
   ON application.shiny_app_usage_event USING GIN (payload jsonb_path_ops);"
    )

    # Permissions
    DBI::dbExecute(
      con,
      "GRANT INSERT ON TABLE application.shiny_app_usage_event TO PUBLIC;"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '36' WHERE item = 'Last patch number';"
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

    message("Patch 36 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 36 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
