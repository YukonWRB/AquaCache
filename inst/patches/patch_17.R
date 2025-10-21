# Patch 17

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 17. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

message(
  "This patch adds columns to many tables to identify a record's creator and modifier, and creates automatic triggers to populate these fields based on the logged in user."
)

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    # Create a function to update the modified_by field
    DBI::dbExecute(
      con,
      "
    CREATE OR REPLACE FUNCTION public.user_modified()
    RETURNS trigger AS $$
    BEGIN
      IF TG_OP = 'UPDATE' THEN
        NEW.modified_by := current_user;
      END IF;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
  "
    )

    # Drop the existing created_by and modified_by columns from the 'continuous.corrections' table
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.corrections DROP COLUMN IF EXISTS created_by;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.corrections DROP COLUMN IF EXISTS modified_by;"
    )

    # Define the tables to be modified
    tables <- c(
      "application.images",
      "application.page_content",
      "application.text",
      "continuous.thresholds",
      "continuous.extrema",
      "continuous.corrections",
      "discrete.collection_methods",
      "discrete.laboratories",
      "discrete.protocols_methods",
      "discrete.results",
      "discrete.result_conditions",
      "discrete.result_speciations",
      "discrete.result_types",
      "discrete.result_value_types",
      "discrete.samples",
      "discrete.sample_fractions",
      "discrete.sample_series",
      "discrete.sample_types",
      "files.documents",
      "files.documents_spatial",
      "files.document_types",
      "files.images",
      "files.image_series",
      "files.image_types",
      "public.approval_types",
      "public.correction_types",
      "public.datum_conversions",
      "public.datum_list",
      "public.grade_types",
      "public.locations",
      "public.locations_metadata_access",
      "public.locations_metadata_infrastructure",
      "public.locations_metadata_infrastructure_groundwater",
      "public.locations_metadata_infrastructure_hydromet",
      "public.locations_metadata_instruments",
      "public.locations_metadata_maintenance",
      "public.locations_metadata_owners_operators",
      "public.locations_metadata_xsections",
      "public.locations_networks",
      "public.locations_projects",
      "public.location_types",
      "public.media_types",
      "public.networks",
      "public.network_project_types",
      "public.organizations",
      "public.parameters",
      "public.parameter_groups",
      "public.parameter_sub_groups",
      "public.projects",
      "public.qualifier_types",
      "public.sub_locations",
      "instruments.array_maintenance_changes",
      "instruments.calibrate_depth",
      "instruments.calibrate_dissolved_oxygen",
      "instruments.calibrate_orp",
      "instruments.calibrate_pH",
      "instruments.calibrate_specific_conductance",
      "instruments.calibrate_temperature",
      "instruments.calibrate_turbidity",
      "instruments.calibrations",
      "instruments.instruments",
      "instruments.instrument_maintenance",
      "instruments.instrument_make",
      "instruments.instrument_model",
      "instruments.instrument_type",
      "instruments.observers",
      "instruments.sensors",
      "instruments.sensor_types"
    )

    for (tbl in tables) {
      DBI::dbExecute(
        con,
        sprintf(
          "
    ALTER TABLE %s
      ADD COLUMN created_by text NOT NULL DEFAULT current_user,
      ADD COLUMN modified_by text;
  ",
          tbl
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "
    CREATE TRIGGER trg_user_audit
      BEFORE UPDATE ON %s
      FOR EACH ROW
      EXECUTE FUNCTION public.user_modified();
  ",
          tbl
        )
      )
    }

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '17' WHERE item = 'Last patch number';"
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

    # Run calculate_stats and synchronize_continuous on all timeseries with source_fx = 'downloadAquarius' because historic ranges will be modified by 'unusable' data and some qualifiers can now overlap.
    try({
      timeseriesids <- DBI::dbGetQuery(
        con,
        "SELECT timeseries_id FROM timeseries WHERE source_fx = 'downloadAquarius';"
      )[, 1]
      synchronize_continuous(
        con = con,
        timeseries_id = timeseriesids,
        start_datetime = "1900-01-01"
      )
      calculate_stats(
        con = con,
        timeseries_id = timeseriesids,
        start_recalc = "1900-01-01"
      )
    })

    message("Patch 17 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 17 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
