# Patch 30

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 30. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # modify sample_series fkeys so that they cascade on delete and update
    message(
      "Modifying foreign key constraints on discrete.sample_series to cascade on delete and update."
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_sub_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_default_owner_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_default_owner_fkey FOREIGN KEY (default_owner) REFERENCES public.organizations(organization_id) ON DELETE SET NULL ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_default_contributor_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_default_contributor_fkey FOREIGN KEY (default_contributor) REFERENCES public.organizations(organization_id) ON DELETE SET NULL ON UPDATE CASCADE;"
    )

    # Do same for images
    message(
      "Modifying foreign key constraints on files.images to cascade on delete and update."
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images DROP CONSTRAINT images_image_type_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images ADD CONSTRAINT images_image_type_fkey FOREIGN KEY (image_type) REFERENCES files.image_types(image_type_id) ON DELETE SET NULL ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images DROP CONSTRAINT images_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images ADD CONSTRAINT images_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )

    # Drop a few unused function and triggers
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS documents_spatial_after_delete ON files.documents_spatial;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS documents_spatial_after_insert ON files.documents_spatial;"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_document_flags_after_insert();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_document_flags_after_delete();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_line_flag();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_location_flag();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_polygon_flag();"
    )

    # Add columns to measurements_calculated_daily table to hold 30 year window stats
    message(
      "Adding 30 year window statistics columns to measurements_calculated_daily table."
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries ADD COLUMN IF NOT EXISTS historic_window_years INTEGER DEFAULT 30;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.historic_window_years IS 'Optional number of years to use when calculating windowed historical statistics (window_* columns) alongside the full-history metrics.';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_percent_historic_range NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_max NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_min NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_mean NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q90 NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q75 NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q50 NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q25 NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_q10 NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
      ADD COLUMN IF NOT EXISTS window_doy_count INTEGER;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_percent_historic_range IS 'Percent of historical range for the same day of year using only the most recent years defined by timeseries.historic_window_years. Values are populated when a window is configured and enough historic points exist within that window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_max IS 'Historical max for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_min IS 'Historical min for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_mean IS 'Historical mean for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q50 IS 'Historical 50th quantile for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q75 IS 'Historical 75th quantile for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q90 IS 'Historical 90th quantile for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q25 IS 'Historical 25th quantile for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_q10 IS 'Historical 10th quantile for the day of year within the configured recent-year window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.window_doy_count IS 'Number of historical points available for the day of year within the configured recent-year window.';"
    )

    # Apply modifications to borehole purposes and bring in data from old SQL Server DB
    # First ammend the purpose table to add a French translation
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.borehole_well_purposes ADD COLUMN IF NOT EXISTS purpose_name_fr TEXT;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.borehole_well_purposes ADD COLUMN IF NOT EXISTS description_fr TEXT;"
    )

    # Populate with some common purposes
    df <- data.frame(
      purpose_name = c(
        # This should already be in the table
        "monitoring",
        "drinking water, residential",
        "drinking water, municipal/commercial",
        "irrigation",
        "observation",
        "dewatering",
        "injection",
        "mineral exploration"
      ),
      # Some of these are modified sligltly from the original purposes
      description = c(
        "Borehole or well installed for monitoring purposes, typically with a small diameter and screen.",
        "Borehole or well installed for private residence drinking water supply.",
        "Borehole or well installed for municipal or commercial drinking water supply.",
        "Borehole or well installed to provide water for agricultural irrigation.",
        "Borehole or well installed to observe groundwater levels or quality changes over time.",
        "Borehole or well installed to lower the groundwater table temporarily or permanently.",
        "Borehole or well installed to inject water or other fluids into the ground.",
        "Borehole installed as part of mineral exploration activities."
      ),
      purpose_name_fr = c(
        # This will be added, matched to the existing purpose_name
        "surveillance",
        "eau potable, résidentiel",
        "eau potable, municipal/commercial",
        "irrigation",
        "observation",
        "désembouage",
        "injection",
        "exploration minérale"
      ),
      description_fr = c(
        # Also added, matched to the existing purpose_name
        "Forage ou puits installé à des fins de surveillance, généralement de petit diamètre et équipé d'un écran.",
        "Forage ou puits installé pour l'approvisionnement en eau potable d'une résidence privée.",
        "Forage ou puits installé pour l'approvisionnement en eau potable municipal ou commercial.",
        "Forage ou puits installé pour fournir de l'eau à l'irrigation agricole.",
        "Forage ou puits installé pour observer les niveaux d'eau souterraine ou les changements de qualité au fil du temps.",
        "Forage ou puits installé pour abaisser temporairement ou définitivement la nappe phréatique.",
        "Forage ou puits installé pour injecter de l'eau ou d'autres fluides dans le sol.",
        "Forage installé dans le cadre d'activités d'exploration minérale."
      )
    )
    for (i in 1:nrow(df)) {
      DBI::dbExecute(
        con,
        "UPDATE boreholes.borehole_well_purposes SET purpose_name_fr = $1, description_fr = $2 WHERE purpose_name = $3;",
        params = list(
          df$purpose_name_fr[i],
          df$description_fr[i],
          df$purpose_name[i]
        )
      )
    }

    # Bring in the well purpose from the SQL Server database (was not done the first time around)
    # Try to connect to the current SQL Server database to fetch records (won't work outside of YG networks)
    sql_server <- FALSE
    tryCatch(
      {
        sql <- DBI::dbConnect(
          odbc::odbc(),
          Driver = "SQL Server",
          Server = "sql-apps2-prd",
          Database = "YWWR",
          Trusted_Connection = "True"
        )
        sql_server <- TRUE
      },
      error = function(e) {}
    )

    if (sql_server) {
      message(
        "Looks like you're on the YG network, I'll fetch the missing well purposes from the SQL Server database."
      )
      boreholes_sql <- DBI::dbGetQuery(
        sql,
        "SELECT BoreholeId, Purpose FROM WellRecords;"
      )

      # TODO: Make a key to match random text from SQL server to our purpose_name in AquaCache
    } else {
      message(
        "Looks like you're not on the YG network, so I can't fetch the missing well purposes from the SQL Server database. You can update them manually later if needed."
      )
    }

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '30' WHERE item = 'Last patch number';"
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

    message("Patch 30 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 30 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
