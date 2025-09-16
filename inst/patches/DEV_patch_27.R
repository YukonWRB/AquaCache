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
      "ALTER TABLE public.locations_metadata_instruments ALTER COLUMN instruments TYPE INTEGER;"
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

    # DRop the old UK
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      DROP CONSTRAINT IF EXISTS locations_metadata_instruments_location_id_sub_location_id_key;"
    )
    # Make a new, better UK
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT unique_location_instrument_period UNIQUE NULLS NOT DISTINCT(location_id, sub_location_id, instrument_id, start_datetime, end_datetime);"
    )
    # Guard against inverted ranges when end is present
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT instrument_period_valid
      CHECK (end_datetime IS NULL OR start_datetime < end_datetime);"
    )
    # Prevent overlaps for the same location_id and instrument_id; allow touching; allow open-ended
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
      ADD CONSTRAINT no_overlap_location_instrument
      EXCLUDE USING gist (
        location_id WITH =,
        instrument_id WITH =,
        tstzrange(start_datetime, COALESCE(end_datetime,'infinity'::timestamptz), '[)') WITH &&
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
        tstzrange(start_datetime, COALESCE(end_datetime,'infinity'::timestamptz), '[)') WITH &&
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
      "ALTER TABLE public.locations_owners_operators
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

    # Add a new table for depth/height of monitoring locations, reference columns 'z' in timeseries and samples tables to this new table
    DBI::dbExecute(
      con,
      "
    CREATE TABLE IF NOT EXISTS public.locations_z (
      location_z_id SERIAL PRIMARY KEY,
      location_id INTEGER NOT NULL REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE,
      sub_location_id INTEGER,
      z_meters NUMERIC NOT NULL,
      note TEXT,
      CONSTRAINT fk_loc_z_loc FOREIGN KEY (location_id, sub_location_id) REFERENCES public.sub_locations(location_id, sub_location_id) ON DELETE CASCADE ON UPDATE CASCADE,
    );"
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
        VALUES ($1, $2, $3) RETURNING location_z_id;
      ",
          params = as.list(row$c("location_id", "sub_location_id", "z_meters"))
        )
      }
      # Use the timeseries_id to update the timeseries.z column to reference the new locations_z table
      DBI::dbExecute(
        con,
        "UPDATE continuous.timeseries SET z = $1 WHERE timeseries_id = $2;",
        params = list(new_z_id, row$timeseries_id)
      )
    }

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
      REFERENCES public.locations_z(location_z_id)
      ON DELETE SET NULL ON UPDATE CASCADE;"
    )
    # rename the column to location_z_id for clarity
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries RENAME COLUMN z TO location_z_id;"
    )

    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")

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
