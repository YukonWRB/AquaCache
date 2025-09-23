# Patch 27

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 27. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
      "Modifying tables to better handle instrument data and location depth/height"
    )

    # Modify unique key on sub_locations table
    # delete the old one
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations
      DROP CONSTRAINT IF EXISTS sub_locations_location_id_sub_location_name_key;"
    )
    # Make a new one on sub_location_id and location_id
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations
      ADD CONSTRAINT unique_subloc_per_location UNIQUE(location_id, sub_location_id);"
    )

    # Add column to 'timeseries' table to hold the time zone used for daily mean and stats calculations
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
      ADD COLUMN IF NOT EXISTS timezone_daily_calc INTEGER DEFAULT 0 NOT NULL;"
    )
    # Add a CHECK constraint to ensure timezone is between -12 and +14
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
      ADD CONSTRAINT timezone_daily_calc_valid CHECK (timezone_daily_calc >= -12 AND timezone_daily_calc <= 14);"
    )
    # Add useful comment
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.timezone_daily_calc IS 'Timezone offset in hours from UTC used for daily mean and statistics calculations. Range is -12 to +14. Default is 0 (UTC).';"
    )
    # Make them all -8 to align with the WSC's defaults
    DBI::dbExecute(
      con,
      "UPDATE continuous.timeseries SET timezone_daily_calc = -8;"
    )

    # Recalculate all daily means and stats for all timeseries
    message(
      "Recalculating all daily means and stats for all timeseries. Be patient, this will take a while."
    )
    calculate_stats(con, timeseries_id = "all", start_recalc = "1900-01-01")

    message(
      "Please review the timezone associated with daily calculations on each timeseries. They've all been set to -8 to align with the WSC's defaults, but you may want to change some of them."
    )

    # Modify instruments.instruments table so that the 'owner' field is an FK to organizations table
    wrb_owner_id <- DBI::dbGetQuery(
      con,
      "SELECT organization_id FROM public.organizations WHERE name = 'Yukon Department of Environment, Water Resources Branch';"
    )[1, 1]
    DBI::dbExecute(
      con,
      "UPDATE instruments.instruments SET owner = $1;",
      params = list(wrb_owner_id)
    )
    # Make column instruments.owner INTEGER NOT NULL and add FK constraint to organizations table
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ALTER COLUMN owner TYPE INTEGER USING owner::INTEGER;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments ALTER COLUMN owner SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
      ADD CONSTRAINT fk_instrument_owner
      FOREIGN KEY (owner)
      REFERENCES public.organizations(organization_id)
      ON DELETE CASCADE ON UPDATE CASCADE;"
    )

    # Change the 'instruments' column in the 'locations_metadata_instruments' table to be INTEGER,
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments ALTER COLUMN instruments TYPE INTEGER USING instruments[1]::INTEGER;"
    )
    # rename it to instrument_id for clarity
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments RENAME COLUMN instruments TO instrument_id;"
    )
    #  add FK constraint to 'instruments.instruments' table
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT fk_loc_meta_instrument
      FOREIGN KEY (instrument_id)
      REFERENCES instruments.instruments(instrument_id)
      ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    # Drop the trigger check_instruments_trigger
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_instruments_trigger ON public.locations_metadata_instruments;"
    )
    # Drop function check_instruments_reference()
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS public.check_instruments_reference();"
    )

    # Drop the old unique key
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      DROP CONSTRAINT IF EXISTS locations_metadata_instruments_location_id_sub_location_id_key;"
    )

    # Make a new, better unique key
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT unique_location_instrument_period UNIQUE NULLS NOT DISTINCT(location_id, sub_location_id, instrument_id, start_dt, end_dt);"
    )
    # Guard against inverted ranges when end is present
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT instrument_period_valid
      CHECK (end_dt IS NULL OR start_dt < end_dt);"
    )
    # Prevent overlaps for the same location_id and instrument_id; allow touching; allow open-ended
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT no_overlap_location_instrument
      EXCLUDE USING gist (
        location_id WITH =,
        instrument_id WITH =,
        tstzrange(start_dt, COALESCE(end_dt,'infinity'::timestamptz), '[)') WITH &&
      )
      WHERE (sub_location_id IS NULL)
      DEFERRABLE INITIALLY IMMEDIATE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT no_overlap_sub_location_instrument
      EXCLUDE USING gist (
        sub_location_id WITH =,
        instrument_id WITH =,
        tstzrange(start_dt, COALESCE(end_dt,'infinity'::timestamptz), '[)') WITH &&
      )
      WHERE (sub_location_id IS NOT NULL)
      DEFERRABLE INITIALLY IMMEDIATE;"
    )
    # Add a 'note' column to public.locations_metadata_instruments table
    DBI::dbExecute(
      con,
      "
    ALTER TABLE public.locations_metadata_instruments
      ADD COLUMN IF NOT EXISTS note TEXT;"
    )

    # Rename columns start_dt and end_dt to start_datetime and end_datetime for consistency
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      RENAME COLUMN start_dt TO start_datetime;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      RENAME COLUMN end_dt TO end_datetime;"
    )

    # Modify locations_metadata_owners_operators table
    # Make a compound FK to sub_locations table (location_id, sub_location_id), ensuring sub_location_id always matches a valid sub_location_id for the given location_id
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_owners_operators
    ADD CONSTRAINT fk_subloc_composite
    FOREIGN KEY (location_id, sub_location_id)
    REFERENCES public.sub_locations (location_id, sub_location_id);"
    )

    # Drop the sub_location_id simple FK:
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_owners_operators DROP CONSTRAINT locations_metadata_owners_operators_sub_location_id_fkey;"
    )

    # Drop old unique key
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_owners_operators
      DROP CONSTRAINT IF EXISTS locations_metadata_owners_operators_unique;"
    )
    # Ensure that public.locations_metadata_owners_operators table is unique on location_id, start_datetime, end_datetime
    DBI::dbExecute(
      con,
      "
    ALTER TABLE public.locations_metadata_owners_operators
      ADD CONSTRAINT unique_location_ownership_period UNIQUE NULLS NOT DISTINCT(location_id, sub_location_id, start_datetime, end_datetime);
      "
    )

    # Guard against inverted ranges when end is present
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_owners_operators
      ADD CONSTRAINT ownership_period_valid
      CHECK (end_datetime IS NULL OR start_datetime < end_datetime);"
    )

    # Prevent overlaps for the same location_id; allow touching; allow open-ended
    DBI::dbExecute(
      con,
      "-- LOCATION-ONLY overlap prevention (rows w/ no sub_location)
      ALTER TABLE public.locations_metadata_owners_operators
        ADD CONSTRAINT no_overlap_location_only
        EXCLUDE USING gist (
          location_id WITH =,
          tstzrange(start_datetime, COALESCE(end_datetime,'infinity'::timestamptz), '[)') WITH &&
        )
        WHERE (sub_location_id IS NULL)
        DEFERRABLE INITIALLY IMMEDIATE;
      "
    )
    DBI::dbExecute(
      con,
      "
      -- SUB-LOCATION overlap prevention (rows w/ a sub_location)
      ALTER TABLE public.locations_metadata_owners_operators
        ADD CONSTRAINT no_overlap_sub_location
        EXCLUDE USING gist (
          sub_location_id WITH =,
          tstzrange(start_datetime, COALESCE(end_datetime,'infinity'::timestamptz), '[)') WITH &&
        )
        WHERE (sub_location_id IS NOT NULL)
        DEFERRABLE INITIALLY IMMEDIATE;
      "
    )
    # Add a 'note' column to public.locations_metadata_owners_operators table
    DBI::dbExecute(
      con,
      "
    ALTER TABLE public.locations_metadata_owners_operators
      ADD COLUMN IF NOT EXISTS note TEXT;"
    )

    # Add a new table for depth/height of monitoring locations, reference columns 'z' in timeseries to this new table. Samples will continue to have a numeric z column.
    DBI::dbExecute(
      con,
      "
    CREATE TABLE IF NOT EXISTS public.locations_z (
      z_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      location_id INTEGER NOT NULL REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE,
      sub_location_id INTEGER,
      z_meters NUMERIC NOT NULL,
      note TEXT,
    created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by TEXT DEFAULT CURRENT_USER NOT NULL,
    modified TIMESTAMP WITH TIME ZONE,
    modified_by TEXT,
      CONSTRAINT fk_loc_z_loc FOREIGN KEY (location_id, sub_location_id) REFERENCES public.sub_locations(location_id, sub_location_id) ON DELETE CASCADE ON UPDATE CASCADE,
    UNIQUE NULLS NOT DISTINCT(location_id, sub_location_id, z_meters)
    );"
    )

    # Add index on location_id for faster queries
    DBI::dbExecute(
      con,
      "CREATE INDEX locations_z_location_id_idx ON public.locations_z(location_id);"
    )

    # Add a comment on the new table clarifying that it's not used for samples
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_z IS 'Table to hold depth/height (z) information for monitoring locations. This is referenced for timeseries as well as instrument deployment purposes, but note that sample data elevations/depths are not linked to this table.'
    ;"
    )

    # Migrate existing data from timeseries.z to locations_z table
    ts_z <- DBI::dbGetQuery(
      con,
      "
      SELECT DISTINCT timeseries_id, location_id, sub_location_id, z AS z_meters
      FROM continuous.timeseries
      WHERE z IS NOT NULL;
    "
    )
    if (nrow(ts_z) > 0) {
      for (i in 1:nrow(ts_z)) {
        row <- ts_z[i, ]
        new_z_id <- DBI::dbGetQuery(
          con,
          "
        INSERT INTO public.locations_z (location_id, sub_location_id, z_meters)
        VALUES ($1, $2, $3) RETURNING z_id;
      ",
          params = list(row$location_id, row$sub_location_id, row$z_meters)
        )[1, 1]
      }
      # Use the timeseries_id to update the timeseries.z column to reference the new locations_z table
      DBI::dbExecute(
        con,
        "UPDATE continuous.timeseries SET z = $1 WHERE timeseries_id = $2;",
        params = list(new_z_id, row$timeseries_id)
      )
    }

    # Drop the views timeseries_metadata_fr and timeseries_metadata_en if they exist; recreate later while considering the new z_id column
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.timeseries_metadata_fr;"
    )
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.timeseries_metadata_en;"
    )

    # Convert timeseries.z to INTEGER and add FK constraint to locations_z table
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries ALTER COLUMN z TYPE INTEGER USING z::INTEGER;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
      ADD CONSTRAINT fk_timeseries_z
      FOREIGN KEY (z)
      REFERENCES public.locations_z(z_id)
      ON DELETE SET NULL ON UPDATE CASCADE;"
    )
    # rename the column to z_id for clarity
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries RENAME COLUMN z TO z_id;"
    )

    # Add new column for z (depth/height) in meters to locations_metadata_instruments table
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD COLUMN IF NOT EXISTS z_id INTEGER REFERENCES public.locations_z(z_id) ON DELETE SET NULL ON UPDATE CASCADE;"
    )

    # Re-create the views timeseries_metadata_fr and timeseries_metadata_en considering the new z_id column
    DBI::dbExecute(
      con,
      '
    CREATE VIEW continuous.timeseries_metadata_fr
    WITH(security_invoker=true)
    AS SELECT 
    ts.timeseries_id,
        loc.location_id,
      loc.name_fr AS nom_endroit,
      lz.z_meters AS "profondeur_hauteur_m",
      mtypes.media_type_fr AS "type_de_média",
      params.param_name_fr AS "nom_paramètre",
      params.unit_default AS "unités",
      ag.aggregation_type_fr AS "type_agrégation",
      ts.record_rate AS "fréquence_enregistrement",
      ts.start_datetime AS "début",
      ts.end_datetime AS fin,
      ts.note
     FROM timeseries ts
       JOIN locations loc ON ts.location_id = loc.location_id
       LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
       LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
       LEFT JOIN aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id
       LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id
    ORDER BY ts.timeseries_id;
    '
    )
    # Now update the English version
    DBI::dbExecute(
      con,
      "
    CREATE VIEW continuous.timeseries_metadata_en
    WITH(security_invoker=true)
    AS SELECT 
    ts.timeseries_id,
    loc.location_id,
    loc.name AS location_name,
    lz.z_meters AS depth_height_m,
    mtypes.media_type,
    params.param_name AS parameter_name,
    params.unit_default AS units,
    at.aggregation_type,
    ts.record_rate AS recording_rate,
    ts.start_datetime,
    ts.end_datetime,
    ts.note
    FROM timeseries ts
    JOIN locations loc ON ts.location_id = loc.location_id
    LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
    LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
    LEFT JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
    LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id
    ORDER BY ts.timeseries_id;
    "
    )

    # Field visit infrastructure ##################
    # Create infrastructure to hold field visit metadata. Will allow linking instruments to a field visit. Samples will be linked to a field visit.
    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS field;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA field TO public;")

    DBI::dbExecute(
      con,
      "
    CREATE TABLE IF NOT EXISTS field.field_visits (
      field_visit_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      start_datetime TIMESTAMPTZ NOT NULL,
      end_datetime TIMESTAMPTZ,
      location_id INTEGER NOT NULL REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE,
      sub_location_id INTEGER,
      purpose TEXT,
      cloud_cover_percent NUMERIC CHECK (cloud_cover_percent >= 0 AND cloud_cover_percent <= 100),
      precip_current_type TEXT CHECK (precip_current_type IN ('None', 'Rain', 'Snow', 'Mixed', 'Hail', 'Freezing rain')),
      precip_current_rate TEXT CHECK (precip_current_rate IN ('None', 'Light', 'Moderate', 'Heavy')),
      precip_24h_mm NUMERIC CHECK (precip_24h_mm >= 0),
      precip_48h_mm NUMERIC CHECK (precip_48h_mm >= 0),
      air_temp_c NUMERIC CHECK (air_temp_c >= -40 AND air_temp_c <= 40),
      wind TEXT, -- e.g., calm, light, etc.
      note TEXT,
      share_with TEXT[] DEFAULT ARRAY['public_reader']::text[],
      created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT DEFAULT CURRENT_USER NOT NULL,
      modified TIMESTAMP WITH TIME ZONE,
      modified_by TEXT,
      -- compound FK to sub_locations table to ensure sub_location_id always matches a valid sub_location_id for the given location_id
      CONSTRAINT fk_field_visit_subloc FOREIGN KEY (location_id, sub_location_id) REFERENCES public.sub_locations(location_id, sub_location_id) ON DELETE CASCADE ON UPDATE CASCADE,
      -- ensure that end_datetime is after start_datetime when end_datetime is present
      CONSTRAINT field_visit_period_valid CHECK (end_datetime IS NULL OR end_datetime > start_datetime),
      CONSTRAINT unique_visit UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, start_datetime)
    );"
    )
    # Create GIN index on share_with column to speed up RLS queries
    DBI::dbExecute(
      con,
      "CREATE INDEX timeseries_share_with_gin_idx ON field.field_visits USING gin (share_with);"
    )
    # Create index on location_id for faster queries
    DBI::dbExecute(
      con,
      "CREATE INDEX field_visits_location_id_idx ON field.field_visits(location_id);"
    )
    # Create index on start_datetime for faster queries
    DBI::dbExecute(
      con,
      "CREATE INDEX field_visits_start_datetime_idx ON field.field_visits(start_datetime);"
    )

    # Enanble RLS and create a policy to allow users to see only their own field visits or those shared with public_reader role
    DBI::dbExecute(
      con,
      "ALTER TABLE field.field_visits ENABLE ROW LEVEL SECURITY;"
    )
    # Create the new SELECT policy
    DBI::dbExecute(
      con,
      "
    CREATE POLICY rls ON field.field_visits
      FOR SELECT
      TO public
    USING (
      share_with @> ARRAY['public_reader']
      OR share_with && public.current_user_roles()
    );"
    )
    # Triggers to track who created/modified records and when
    DBI::dbExecute(
      con,
      "create trigger trg_user_audit before update on field.field_visits for each row execute function public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "create trigger update_modify_time before update on field.field_visits for each row execute function public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE field.field_visits IS 'Table to hold metadata about field visits to monitoring locations. Samples can be linked to field visits and field visits to instruments used during the visit.';"
    )

    # Allow many-to-many relationship between field visits and instruments via a junction table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS field.field_visit_instruments (
      field_visit_id int NOT NULL REFERENCES field.field_visits(field_visit_id) ON DELETE CASCADE,
      instrument_id  int NOT NULL REFERENCES instruments.instruments(instrument_id) ON DELETE SET NULL ON UPDATE CASCADE,
      created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT DEFAULT CURRENT_USER NOT NULL,
      modified TIMESTAMP WITH TIME ZONE,
      modified_by TEXT,
      PRIMARY KEY (field_visit_id, instrument_id)
    );"
    )
    # Triggers to track who created/modified records and when
    DBI::dbExecute(
      con,
      "create trigger trg_user_audit before update on field.field_visit_instruments for each row execute function public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "create trigger update_modify_time before update on field.field_visit_instruments for each row execute function public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE field.field_visit_instruments IS 'Junction table to allow many-to-many relationship between field visits and instruments used during the visit.';"
    )

    # Allow many-to-many relationship between field visits and images via a junction table
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS field.field_visit_images (
      field_visit_id int NOT NULL REFERENCES field.field_visits(field_visit_id) ON DELETE CASCADE,
      image_id  int NOT NULL REFERENCES files.images(image_id) ON DELETE CASCADE,
      created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT DEFAULT CURRENT_USER NOT NULL,
      modified TIMESTAMP WITH TIME ZONE,
      modified_by TEXT,
      PRIMARY KEY (field_visit_id, image_id)
    );"
    )
    # Triggers to track who created/modified records and when
    DBI::dbExecute(
      con,
      "create trigger trg_user_audit before update on field.field_visit_images for each row execute function public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "create trigger update_modify_time before update on field.field_visit_images for each row execute function public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE field.field_visit_images IS 'Junction table to allow many-to-many relationship between field visits and images taken during the visit.';"
    )

    # Add a field for field_visit_id to discrete.samples table
    DBI::dbExecute(
      con,
      "
    ALTER TABLE discrete.samples
      ADD COLUMN IF NOT EXISTS field_visit_id INTEGER 
      REFERENCES field.field_visits(field_visit_id) ON DELETE SET NULL ON UPDATE CASCADE
      ;"
    )

    # Find all user groups that have access to discrete.samples and grant them privileges to the new tables
    groups <- DBI::dbGetQuery(
      con,
      "SELECT public.get_shareable_principals_for('discrete.samples') AS groups;"
    )
    tbls <- c(
      "field.field_visits",
      "field.field_visit_instruments",
      "field.field_visit_images"
    )
    for (grp in groups$groups) {
      for (tbl in tbls) {
        DBI::dbExecute(
          con,
          paste0(
            "GRANT SELECT, INSERT, UPDATE, DELETE ON ",
            tbl,
            " TO ",
            grp,
            ";"
          )
        )
      }
    }

    # Clean up some pieces ###########
    DBI::dbExecute(
      con,
      "ALTER TABLE files.documents ALTER COLUMN file_hash SET NOT NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images ALTER COLUMN file_hash SET NOT NULL"
    )
    # Delete columns has_points, has_lines, has_polygons from files.documents table
    DBI::dbExecute(
      con,
      "ALTER TABLE files.documents
      DROP COLUMN IF EXISTS has_points,
      DROP COLUMN IF EXISTS has_lines,
      DROP COLUMN IF EXISTS has_polygons;"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '27' WHERE item = 'Last patch number';"
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

    message("Patch 27 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 27 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
