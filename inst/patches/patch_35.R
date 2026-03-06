# Patch 35

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 35. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # Drop + re-create timeseries views with more information
    DBI::dbExecute(con, "DROP VIEW IF EXISTS timeseries_metadata_en;")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS timeseries_metadata_fr;")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW continuous.timeseries_metadata_en
    WITH(security_invoker=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name AS location_name,
        loc.alias AS alias_name,
        lz.z_meters AS depth_height_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS location_elevation,
        array_agg(DISTINCT proj.name) AS projects,
        array_agg(DISTINCT net.name) AS networks,
        mtypes.media_type,
        params.param_name AS parameter_name,
        params.unit_default AS units,
        at.aggregation_type,
        ts.record_rate AS recording_rate,
        ts.sensor_priority,
        ts.start_datetime,
        ts.end_datetime,
        ts.note
       FROM timeseries ts
         JOIN locations loc ON ts.location_id = loc.location_id
         LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
         LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
         LEFT JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
         LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
         LEFT JOIN locations_projects loc_proj ON loc.location_id = loc_proj.location_id
         LEFT JOIN projects proj ON loc_proj.project_id = proj.project_id
         LEFT JOIN locations_networks loc_net ON loc.location_id = loc_net.location_id
         LEFT JOIN networks net ON loc_net.network_id = net.network_id
         LEFT JOIN datum_conversions dc ON loc.location_id = dc.location_id AND dc.current = true
      GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type, params.param_name, params.unit_default, at.aggregation_type, lz.z_meters, dc.conversion_m;"
    )

    DBI::dbExecute(
      con,
      '
    CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
    WITH(security_invoker=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name_fr AS nom_endroit,
        loc.alias AS nom_alias,
        lz.z_meters AS profondeur_hauteur_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS "élévation_endroit",
        array_agg(DISTINCT proj.name_fr) AS projets,
        array_agg(DISTINCT net.name_fr) AS "réseaux",
        mtypes.media_type_fr AS "type_de_média",
        params.param_name_fr AS "nom_paramètre",
        params.unit_default AS "unités",
        ag.aggregation_type_fr AS "type_agrégation",
        ts.record_rate AS "fréquence_enregistrement",
        ts.sensor_priority AS "priorité_capteur",
        ts.start_datetime AS "début",
        ts.end_datetime AS fin,
        ts.note
      FROM timeseries ts
        JOIN locations loc ON ts.location_id = loc.location_id
        LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
        LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
        LEFT JOIN aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id
        LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
        LEFT JOIN locations_projects loc_proj ON loc.location_id = loc_proj.location_id
        LEFT JOIN projects proj ON loc_proj.project_id = proj.project_id
        LEFT JOIN locations_networks loc_net ON loc.location_id = loc_net.location_id
        LEFT JOIN networks net ON loc_net.network_id = net.network_id
        LEFT JOIN datum_conversions dc ON loc.location_id = dc.location_id AND dc.current = true
      GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type_fr, params.param_name_fr, params.unit_default, ag.aggregation_type_fr, lz.z_meters, dc.conversion_m;'
    )

    # Add some columns to 'discrete.guidelines' and further build out ways of distinguishing guidelines

    # Start by wiping the guidelines table, there should be nothing in there anyways at this point and it will make it easier to work with the new structure. If there are guidelines in there, they should be re-added after the patch is applied.
    DBI::dbExecute(con, "DELETE FROM discrete.guidelines;")

    # Create a new table for publisher information for guidelines
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_publishers (
        publisher_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        publisher_name TEXT UNIQUE,
        publisher_name_fr TEXT UNIQUE,
        country TEXT,
        prov_terr_state TEXT
      );"
    )

    # Alter guidelines.publisher to reference guideline_publishers
    exist <- DBI::dbGetQuery(
      con,
      "SELECT data_type
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'guidelines'
         AND column_name = 'publisher';"
    )

    if (nrow(exist) > 0) {
      if (!identical(exist$data_type[[1]], "integer")) {
        DBI::dbExecute(
          con,
          "ALTER TABLE discrete.guidelines
           ALTER COLUMN publisher TYPE INTEGER USING (publisher::INTEGER);"
        )
      }

      fk_exist <- DBI::dbGetQuery(
        con,
        "SELECT 1
         FROM pg_constraint c
         JOIN pg_class t ON t.oid = c.conrelid
         JOIN pg_namespace n ON n.oid = t.relnamespace
         WHERE n.nspname = 'discrete'
           AND t.relname = 'guidelines'
           AND c.conname = 'fk_guidelines_publisher';"
      )

      if (nrow(fk_exist) == 0) {
        DBI::dbExecute(
          con,
          "ALTER TABLE discrete.guidelines
           ADD CONSTRAINT fk_guidelines_publisher
           FOREIGN KEY (publisher)
           REFERENCES discrete.guideline_publishers(publisher_id)
           ON UPDATE CASCADE
           ON DELETE CASCADE;"
        )
      }
    }

    # Create a new table to hold series information for guidelines
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_series (
        series_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        series_name TEXT UNIQUE NOT NULL,
        series_name_fr TEXT UNIQUE,
        publisher_id INTEGER NOT NULL REFERENCES discrete.guideline_publishers(publisher_id) ON UPDATE CASCADE ON DELETE CASCADE
      );"
    )

    # Add the new FK column to guidelines to link to the series table (a guideline can belong to one series, but a series can have multiple guidelines)
    check <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'guidelines'
         AND column_name = 'series';"
    )
    if (nrow(check) == 0) {
      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.guidelines
         ADD COLUMN IF NOT EXISTS series INTEGER
         REFERENCES discrete.guideline_series(series_id)
         ON UPDATE CASCADE
         ON DELETE CASCADE;"
      )
    }

    # Guidelines can apply to multiple media types
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guidelines_media_types (
        guideline_id INTEGER REFERENCES discrete.guidelines(guideline_id) ON UPDATE CASCADE ON DELETE CASCADE,
        media_id INTEGER REFERENCES public.media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE,
        PRIMARY KEY (guideline_id, media_id)
      );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guidelines_media_types IS 'Linking table to associate guidelines with media types. A guideline can apply to multiple media types, and a media type can have multiple guidelines.';"
    )

    # Guidelines can also apply to multiple fractions
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guidelines_fractions (
        guideline_id INTEGER REFERENCES discrete.guidelines(guideline_id) ON UPDATE CASCADE ON DELETE CASCADE,
        fraction_id INTEGER REFERENCES discrete.sample_fractions(sample_fraction_id) ON UPDATE CASCADE ON DELETE CASCADE,
        PRIMARY KEY (guideline_id, fraction_id)
      );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guidelines_fractions IS 'Linking table to associate guidelines with sample fractions. A guideline can apply to multiple fractions, and a fraction can have multiple guidelines.';"
    )

    # Drop guidelines.sample_fraction_id column if present (now handled by linking table)
    check <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'guidelines'
         AND column_name = 'sample_fraction_id';"
    )
    if (nrow(check) > 0) {
      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.guidelines DROP COLUMN IF EXISTS sample_fraction_id;"
      )
    }

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_enforce_sample_fraction ON discrete.guidelines;"
    )

    # Add a column specifically for notes on guideline applicability (e.g. if a guideline only applies under certain conditions, that can be noted here)
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       ADD COLUMN IF NOT EXISTS applicability_notes TEXT;"
    )
    #Rename the old 'notes' column to 'general_notes' to make it clear that it's for general notes about the guideline, not specific to applicability
    exists <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns       
       WHERE table_schema = 'discrete'
         AND table_name = 'guidelines'
         AND column_name = 'note';"
    )
    if (nrow(exists) > 0) {
      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.guidelines
       RENAME COLUMN note TO general_notes;"
      )
    }

    # Grants #################
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION discrete.get_guideline_value(INTEGER, INTEGER) TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE discrete.guidelines, discrete.guideline_publishers, discrete.guideline_series TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE discrete.guidelines_media_types, discrete.guidelines_fractions TO PUBLIC;"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '35' WHERE item = 'Last patch number';"
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

    message("Patch 35 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 35 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
