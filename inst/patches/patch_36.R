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
                         FROM public.locations_metadata_instruments
                         WHERE location_id = NEW.location_id
                         AND sub_location_id IS NOT DISTINCT FROM NEW.sub_location_id
                         AND z_id IS NOT DISTINCT FROM NEW.z_id
                         AND instrument_id IS NOT DISTINCT FROM NEW.instrument_id
                         AND metadata_id != NEW.metadata_id  -- Exclude the current row in an UPDATE
                         AND NEW.start_datetime <
                           COALESCE(end_datetime, 'infinity'::timestamptz)
                         AND COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
                           start_datetime
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
    DBI::dbExecute(
      con,
      "DO $$
       DECLARE seq_name text;
       BEGIN
         seq_name := pg_get_serial_sequence('application.shiny_app_usage_event', 'id');
         IF seq_name IS NOT NULL THEN
           EXECUTE format('GRANT USAGE, SELECT ON SEQUENCE %s TO PUBLIC', seq_name);
         END IF;
       END $$;"
    )

    # Add column to locations_metadata_instruments to associate instruments with a timeseries_id
    DBI::dbExecute(
      con,
      "ALTER TABLE locations_metadata_instruments
       ADD COLUMN IF NOT EXISTS timeseries_id INTEGER REFERENCES continuous.timeseries ON DELETE SET NULL ON UPDATE CASCADE;"
    )

    # Create a table 'suppliers' to hold information about suppliers
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.suppliers (
      supplier_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      supplier_name TEXT NOT NULL,
      contact_name TEXT,
      contact_phone TEXT,
      contact_email TEXT,
      note TEXT
      )"
    )

    # Add some extra columns to table 'instruments' to capture more metadata
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
      ADD COLUMN IF NOT EXISTS supplier_id INTEGER REFERENCES instruments.suppliers(supplier_id) ON DELETE SET NULL ON UPDATE CASCADE"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
    ADD COLUMN IF NOT EXISTS purchase_price NUMERIC"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
    ADD COLUMN IF NOT EXISTS takes_measurements BOOLEAN"
    )
    # Now populate takes_measurements based on what's already in the DB
    types <- DBI::dbGetQuery(
      con,
      "SELECT type_id, type FROM instruments.instrument_type;"
    )
    for (i in unique(types$type_id)) {
      type_name <- types$type[types$type_id == i]
      if (
        grepl(
          "sonde|logger|probe|sensor|buoy|meter",
          type_name,
          ignore.case = TRUE
        )
      ) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE instruments.instruments SET takes_measurements = TRUE WHERE type = ",
            i,
            ";"
          )
        )
      } else {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE instruments.instruments SET takes_measurements = FALSE WHERE type = ",
            i,
            ";"
          )
        )
      }
    }

    # Make takes_measurements NOT NULL
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
    ALTER COLUMN takes_measurements SET NOT NULL"
    )

    # Make a few new columns in instruments.instruments
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ADD COLUMN IF NOT EXISTS cable_length_m NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ADD COLUMN IF NOT EXISTS firmware_version TEXT;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ADD COLUMN IF NOT EXISTS voltage NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ADD COLUMN IF NOT EXISTS power_active_mA NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ADD COLUMN IF NOT EXISTS power_quiescent_mA NUMERIC;"
    )

    # Define a first-pass conversion map for common units already present in the DB plus a few closely related units used elsewhere in the project.
    conversions <- data.frame(
      from_unit = c(
        "°C",
        "°F",
        "°C",
        "K",
        "°F",
        "K",
        "m",
        "ft",
        "m",
        "cm",
        "m",
        "mm",
        "cm",
        "mm",
        "ft",
        "in",
        "in",
        "mm",
        "km/h",
        "m/s",
        "US gal/min",
        "L/s",
        "m3/s",
        "L/s",
        "mg/l",
        "mg/L",
        "mg/L",
        "µg/L",
        "mg/l",
        "µg/L"
      ),
      to_unit = c(
        "°F",
        "°C",
        "K",
        "°C",
        "K",
        "°F",
        "ft",
        "m",
        "cm",
        "m",
        "mm",
        "m",
        "mm",
        "cm",
        "in",
        "ft",
        "mm",
        "in",
        "m/s",
        "km/h",
        "L/s",
        "US gal/min",
        "L/s",
        "m3/s",
        "mg/L",
        "mg/l",
        "µg/L",
        "mg/L",
        "µg/L",
        "mg/l"
      ),
      conversion_type = c(
        rep("affine", 6),
        rep("factor", 24)
      ),
      scale_a = c(
        9 / 5,
        5 / 9,
        1,
        1,
        5 / 9,
        9 / 5,
        3.28083989501312,
        0.3048,
        100,
        0.01,
        1000,
        0.001,
        10,
        0.1,
        12,
        1 / 12,
        25.4,
        1 / 25.4,
        1 / 3.6,
        3.6,
        0.0630901964,
        1 / 0.0630901964,
        1000,
        0.001,
        1,
        1,
        1000,
        0.001,
        1000,
        0.001
      ),
      scale_b = c(
        32,
        -32 * 5 / 9,
        273.15,
        -273.15,
        273.15 - 32 * 5 / 9,
        -459.67,
        rep(0, 24)
      ),
      stringsAsFactors = FALSE
    )

    # Make a new table for 'units'. Start with the units already referenced by
    # parameters, then add any units needed by the conversion map.

    # DBI::dbExecute(con, "ALTER TABLE public.parameters RENAME COLUMN unit_default to unit_liquid;")
    # DBI::dbExecute(con, "ALTER TABLE public.parameters ADD COLUMN IF NOT EXISTS unit_gas TEXT;")
    existing_parameter_units <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT unit_default, unit_solid FROM public.parameters;"
    )
    existing_parameter_units <- unique(trimws(c(
      existing_parameter_units$unit_default,
      existing_parameter_units$unit_solid
    )))
    # Drop NAs and empty strings
    existing_parameter_units <- existing_parameter_units[
      !is.na(existing_parameter_units) & existing_parameter_units != ""
    ]
    all_units <- unique(c(
      existing_parameter_units,
      conversions$from_unit,
      conversions$to_unit
    ))
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.units (
      unit_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      unit_name TEXT NOT NULL UNIQUE
      );"
    )
    # Insert the unique units into the new 'units' table.
    for (unit in all_units) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.units (unit_name) VALUES ($1)
         ON CONFLICT (unit_name) DO NOTHING;",
        params = list(unit)
      )
    }

    # Make a new table 'unit_conversions' to hold conversion factors between units
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.unit_conversions (
      conversion_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      from_unit_id INTEGER NOT NULL REFERENCES public.units(unit_id) ON DELETE CASCADE ON UPDATE CASCADE,
      to_unit_id INTEGER NOT NULL REFERENCES public.units(unit_id) ON DELETE CASCADE ON UPDATE CASCADE,
      conversion_type TEXT NOT NULL CHECK (conversion_type IN ('affine', 'factor')),
      scale_a NUMERIC NOT NULL,
      scale_b NUMERIC NOT NULL DEFAULT 0,
      UNIQUE (from_unit_id, to_unit_id)
      );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.unit_conversions IS 'Conversion factors between units. The conversion_factor is the number you multiply a value in the from_unit to get the equivalent value in the to_unit.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.unit_conversions.from_unit_id IS 'Foreign key to public.units for the original unit.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.unit_conversions.to_unit_id IS 'Foreign key to public.units for the target unit.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.unit_conversions.conversion_type IS 'Type of conversion (affine, i.e. y = mx + b, or factor, i.e. y = kx).';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.unit_conversions.scale_a IS 'The scale factor (m or k) for the conversion.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.unit_conversions.scale_b IS 'The offset (b) for affine conversions (default 0).';"
    )

    # Populate the unit conversions table with common direct conversions.
    for (i in seq_len(nrow(conversions))) {
      row <- conversions[i, , drop = FALSE]
      DBI::dbExecute(
        con,
        "INSERT INTO public.unit_conversions (
           from_unit_id,
           to_unit_id,
           conversion_type,
           scale_a,
           scale_b
         )
         SELECT
           from_u.unit_id,
           to_u.unit_id,
           $3,
           $4,
           $5
         FROM public.units from_u
         JOIN public.units to_u
           ON to_u.unit_name = $2
         WHERE from_u.unit_name = $1
         ON CONFLICT (from_unit_id, to_unit_id) DO UPDATE
         SET conversion_type = EXCLUDED.conversion_type,
             scale_a = EXCLUDED.scale_a,
             scale_b = EXCLUDED.scale_b;",
        params = list(
          row$from_unit[[1]],
          row$to_unit[[1]],
          row$conversion_type[[1]],
          row$scale_a[[1]],
          row$scale_b[[1]]
        )
      )
    }

    # Make a few new columns in public.locations_metadata_instruments
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ADD COLUMN IF NOT EXISTS record_units INTEGER REFERENCES public.units(unit_id) ON DELETE SET NULL ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.record_units IS 'Units of the recorded data (e.g., m, °C, etc.)';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ADD COLUMN IF NOT EXISTS record_interval_seconds NUMERIC;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.record_interval_seconds IS 'Interval between recorded data points in seconds';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ADD COLUMN IF NOT EXISTS record_duration_seconds NUMERIC DEFAULT 0;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.record_duration_seconds IS 'Duration of the recording in seconds (instantaneous = 0)';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ADD COLUMN IF NOT EXISTS record_notes TEXT;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.record_notes IS 'Additional notes about the recording';"
    )
    # The statistic used for the record (mean, max, min, etc.)
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ADD COLUMN IF NOT EXISTS record_statistic INTEGER REFERENCES continuous.aggregation_types(aggregation_type_id) ON DELETE SET NULL ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.record_statistic IS 'Statistic used for the record (instantaneous, mean, max, min, etc.)';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ADD COLUMN IF NOT EXISTS record_notes TEXT;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.record_notes IS 'Additional notes about the recording';"
    )
    # Backfill the new recording metadata where this instrument deployment is already linked to a continuous timeseries.
    DBI::dbExecute(
      con,
      "UPDATE public.locations_metadata_instruments lmi
       SET record_units = u.unit_id
       FROM continuous.timeseries ts
       JOIN public.parameters p
         ON p.parameter_id = ts.parameter_id
       JOIN public.units u
         ON u.unit_name = COALESCE(
           NULLIF(p.unit_default, ''),
           NULLIF(p.unit_solid, '')
         )
       WHERE lmi.timeseries_id = ts.timeseries_id
         AND lmi.record_units IS NULL;"
    )
    DBI::dbExecute(
      con,
      "UPDATE public.locations_metadata_instruments lmi
       SET record_interval_seconds = EXTRACT(EPOCH FROM ts.record_rate)
       FROM continuous.timeseries ts
       WHERE lmi.timeseries_id = ts.timeseries_id
         AND lmi.record_interval_seconds IS NULL
         AND ts.record_rate IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "UPDATE public.locations_metadata_instruments lmi
       SET record_statistic = at.aggregation_type_id
       FROM continuous.timeseries ts
       JOIN continuous.aggregation_types at
         ON at.aggregation_type_id = ts.aggregation_type_id
       WHERE lmi.timeseries_id = ts.timeseries_id
         AND lmi.record_statistic IS NULL;"
    )
    DBI::dbExecute(
      con,
      "UPDATE public.locations_metadata_instruments lmi
       SET record_notes = ts.note
       FROM continuous.timeseries ts
       WHERE lmi.timeseries_id = ts.timeseries_id
         AND (lmi.record_notes IS NULL OR lmi.record_notes = '')
         AND ts.note IS NOT NULL
         AND ts.note <> '';"
    )

    # Add columns to instruments_maintenance
    # ...to be completed

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
