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

    DBI::dbExecute(
      con,
      "ALTER TABLE public.location_types ADD COLUMN IF NOT EXISTS type_suffix TEXT;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.location_types.type_suffix IS 'A short suffix to use in constructing location codes, e.g., SW for surface water, GW for groundwater, WW for wastewater, OC for oceanic, MET for meteorological.';"
    )

    for (i in 1:nrow(type_suffix)) {
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE public.location_types SET type_suffix = '",
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
      "DROP TABLE IF EXISTS public.locations_metadata_owners_operators;"
    )

    # Create a table to hold data sharing agreements for timeseries with temporal ranges.
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.timeseries_data_sharing_agreements (
        timeseries_data_sharing_agreement_id SERIAL PRIMARY KEY,
        timeseries_id INTEGER NOT NULL REFERENCES continuous.timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
        data_sharing_agreement_id INTEGER NOT NULL REFERENCES files.documents(document_id),
        start_dt TIMESTAMPTZ NOT NULL,
        end_dt TIMESTAMPTZ NOT NULL DEFAULT 'infinity',
        created TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.timeseries_data_sharing_agreements IS 'Temporal data sharing agreements for timeseries. Use start_dt/end_dt to define applicability windows.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.timeseries_data_sharing_agreements.data_sharing_agreement_id IS 'References documents.document_id where document type is data sharing agreement.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS idx_timeseries_data_sharing_agreements_range
       ON public.timeseries_data_sharing_agreements (timeseries_id, start_dt, end_dt);"
    )
    # Drop old triggers if they exist
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_timeseries_data_sharing_agreements_updated ON public.timeseries_data_sharing_agreements;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_timeseries_data_sharing_agreements_updated
       BEFORE UPDATE
       ON public.timeseries_data_sharing_agreements
       FOR EACH ROW
       EXECUTE FUNCTION public.update_updated();"
    )
    # Drop the trigger if it exists
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_check_timeseries_data_sharing_agreement_type ON public.timeseries_data_sharing_agreements;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_check_timeseries_data_sharing_agreement_type
       BEFORE INSERT OR UPDATE ON public.timeseries_data_sharing_agreements
       FOR EACH ROW
       EXECUTE FUNCTION files.check_data_sharing_agreement();"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_timeseries_data_sharing_agreements_overlap()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.timeseries_data_sharing_agreements
           WHERE timeseries_id = NEW.timeseries_id
             AND timeseries_data_sharing_agreement_id != NEW.timeseries_data_sharing_agreement_id
             AND (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
         ) THEN
           RAISE EXCEPTION 'Data sharing agreements cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
         END IF;
         RETURN NEW;
       END;
       $$;"
    )
    # Drop the trigger if it exists
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_timeseries_data_sharing_agreements_overlap ON public.timeseries_data_sharing_agreements;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_timeseries_data_sharing_agreements_overlap
       AFTER INSERT OR UPDATE ON public.timeseries_data_sharing_agreements
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_timeseries_data_sharing_agreements_overlap();"
    )

    # Add a column on timeseries for default data sharing agreements.
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries ADD COLUMN IF NOT EXISTS default_data_sharing_agreement_id INTEGER REFERENCES files.documents(document_id);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.default_data_sharing_agreement_id IS 'Default data sharing agreement for this timeseries when data does not specify otherwise. Must match to a document_type of data sharing agreement, enforced by trigger and function.';"
    )

    # Create a function and trigger to enforce document type
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION files.check_default_data_sharing_agreement()
          RETURNS TRIGGER
          LANGUAGE plpgsql
        AS $$
        BEGIN
          IF NEW.default_data_sharing_agreement_id IS NOT NULL THEN
            IF NOT EXISTS (
              SELECT 1
              FROM files.documents d
              JOIN files.document_types dt ON d.document_type_id = dt.document_type_id
              WHERE d.document_id = NEW.default_data_sharing_agreement_id
                AND dt.type = 'data sharing agreement'
            ) THEN
              RAISE EXCEPTION 'Invalid document type: default_data_sharing_agreement_id must reference a document of type ''data sharing agreement''';
            END IF;
          END IF;
          RETURN NEW;
        END;
        $$;
        "
    )
    # Drop the trigger if it exists and create it anew
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_check_default_data_sharing_agreement ON continuous.timeseries;"
    )
    DBI::dbExecute(
      con,
      "
        CREATE TRIGGER trg_check_default_data_sharing_agreement
        BEFORE INSERT OR UPDATE ON continuous.timeseries
        FOR EACH ROW
        EXECUTE FUNCTION files.check_default_data_sharing_agreement();
        "
    )

    # Migrate existing timeseries data_sharing_agreement_id values into the new table if present.
    column_check <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'continuous'
         AND table_name = 'timeseries'
         AND column_name = 'data_sharing_agreement_id';"
    )$column_name
    if (length(column_check) > 0) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.timeseries_data_sharing_agreements
           (timeseries_id, data_sharing_agreement_id, start_dt, end_dt)
         SELECT timeseries_id,
                data_sharing_agreement_id,
                COALESCE(start_datetime, NOW()),
                COALESCE(end_datetime, 'infinity'::timestamptz)
         FROM continuous.timeseries
         WHERE data_sharing_agreement_id IS NOT NULL;"
      )
      DBI::dbExecute(
        con,
        "UPDATE continuous.timeseries
         SET default_data_sharing_agreement_id = data_sharing_agreement_id
         WHERE data_sharing_agreement_id IS NOT NULL;"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE continuous.timeseries DROP COLUMN IF EXISTS data_sharing_agreement_id;"
      )
      DBI::dbExecute(
        con,
        "DROP TRIGGER IF EXISTS trg_check_data_sharing_agreement ON continuous.timeseries;"
      )
    }

    # Add a column 'data_sharing_agreement_id' to discrete.samples table
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples ADD COLUMN IF NOT EXISTS data_sharing_agreement_id INTEGER REFERENCES files.documents(document_id);"
    )
    # Re-use a function to check document type for data sharing agreement
    try(
      {
        DBI::dbExecute(
          con,
          "
    create trigger trg_check_data_sharing_agreement before insert or update on discrete.samples for each row execute function files.check_data_sharing_agreement()
    ;"
        )
      },
      silent = TRUE
    )

    # Drop column 'data_sharing_agreement_id' from locations table
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations DROP COLUMN IF EXISTS data_sharing_agreement_id;"
    )

    # Drop the trigger trg_check_data_sharing_agreement from locations table if it exists
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_check_data_sharing_agreement ON locations;"
    )

    # Now the big changes to location codes.
    # Drop 'location' from table 'timeseries'
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries DROP COLUMN IF EXISTS location;"
    )

    # Rename 'location' in table 'locations' to 'location_code' and add 'alias' column
    # First check if 'location_code' column already exists
    column_check <- DBI::dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns WHERE table_name = 'locations' AND column_name = 'location_code';"
    )$column_name
    if (length(column_check) == 0) {
      DBI::dbExecute(
        con,
        "ALTER TABLE public.locations RENAME COLUMN location TO location_code;"
      )
    }
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations.location_code IS 'A unique code for the location, ideally constructed from the location National Hydro Network polygon in which the location is situated, using the first 2 digits and 2-3 letters of the polygon name, location type suffix, and a unique number.';"
    )

    # Create a new column 'alias' in table 'locations'
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations ADD COLUMN IF NOT EXISTS alias TEXT;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations.alias IS 'An alternate name or code for the location.';"
    )

    # Now we'll create location codes for all existing locations
    # Search for the layer_name 'National Hydro Network - Basins' in spatial.vectors. If not found, prompt user to download NHN basins and load to database.
    check <- DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS count
      FROM spatial.vectors
      WHERE layer_name = 'National Hydro Network - Basins';
      "
    )[1, 1]

    # If not found, download and load the NHN basins
    if (check < 1338) {
      message(
        "National Hydro Network - Basins layer not found or incomplete in spatial.vectors. Downloading and loading the layer now. This will take a few minutes..."
      )
      # Fetch the file. terra::vect will auto-unzip.
      vect <- terra::vect(
        "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_decoupage.gpkg.zip"
      )
      vect <- vect[, c("validity_date", "dataset_name", "edition", "version")]
      vect$description <- paste0(
        "Edition: ",
        vect$edition,
        ", Version: ",
        vect$version
      )

      AquaCache::insertACVector(
        geom = vect,
        layer_name = "National Hydro Network - Basins",
        feature_name_col = "dataset_name",
        description_col = "description",
        con = con,
        overwrite = FALSE,
        ask = FALSE
      )
    }

    # Iterate through all locations with location_codes that are null, that don't begin with (two numbers, 2-3 letters), that don't begin with 'YOWN-' and generate codes
    exist_locs <- DBI::dbGetQuery(
      con,
      "SELECT location_id, latitude, longitude, location_type, location_code
      FROM locations
      WHERE location_code IS NULL
        OR location_code = ''
        OR location_code !~ '^(?:[0-9]{2}[A-Za-z]{2,3}|YOWN)';
      "
    )
    location_types <- DBI::dbGetQuery(
      con,
      "SELECT * FROM location_types;"
    )
    for (i in 1:nrow(exist_locs)) {
      # See if there's a corresponding polygon. Do a spatial intersection with the lat/lon
      poly <- DBI::dbGetQuery(
        con,
        "
        WITH p AS (
          SELECT ST_Transform(ST_SetSRID(ST_MakePoint($1,$2), 4326), ST_SRID(geom)) AS pt
          FROM spatial.vectors
          WHERE layer_name = 'National Hydro Network - Basins'
          LIMIT 1
        )
        SELECT feature_name
        FROM spatial.vectors v, p
        WHERE v.layer_name = 'National Hydro Network - Basins'
          AND ST_Intersects(v.geom, p.pt)
        ORDER BY ST_Area(v.geom::geography) ASC
        LIMIT 1;
        ",
        params = list(exist_locs$longitude[i], exist_locs$latitude[i])
      )[1, 1]

      # Take the first 2 numbers and subsequent 2 or 3 letters from the polygon name
      code <- sub("^([0-9]{2})([A-Za-z]{2,3}).*$", "\\1\\2", poly)

      # Generate a location type suffix
      type_suffix <- location_types[
        location_types$type_id == exist_locs$location_type[i],
        "type_suffix"
      ]

      if (is.na(type_suffix)) {
        type_suffix <- "OT"
        warning("Location type suffix is NA. Using 'OT' (other) as the suffix.")
      }

      code <- paste0(code, "-", type_suffix)

      # Find the database's greatest existing code with that prefix in column 'location_code'
      existing_code <- DBI::dbGetQuery(
        con,
        "
      SELECT MAX(location_code) AS max_code
      FROM public.locations
      WHERE location_code LIKE $1
      ",
        params = list(paste0(code, "%"))
      )[1, 1]

      if (is.na(existing_code)) {
        code <- paste0(code, "-00001")
      } else {
        # Increment the numeric suffix by 1
        # Find the last numbers with no in-between characters (maay not be a hyphen)
        suffix_num <- as.integer(sub(
          "^.*?(\\d+)$",
          "\\1",
          existing_code
        ))
        suffix_num <- suffix_num + 1
        code <- paste0(code, "-", sprintf("%05d", suffix_num))
      }

      # Update the location_code in the database (move the old code to 'alias' first)
      DBI::dbExecute(
        con,
        "
        UPDATE locations
        SET alias = location_code
        WHERE location_id = $1
        AND alias IS NULL;",
        params = list(exist_locs$location_id[i])
      )
      DBI::dbExecute(
        con,
        "
        UPDATE locations
        SET location_code = $1
        WHERE location_id = $2;
      ",
        params = list(code, exist_locs$location_id[i])
      )
    } # End of generating location codes loop

    # Add a table to hold first nation language names
    DBI::dbExecute(con, "DROP TABLE IF EXISTS public.languages;")
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.languages (
        language_code INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        language_name_en TEXT NOT NULL,
        language_name_fr TEXT
      );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.languages IS 'Table of First Nation language names.';"
    )

    languages_df <- data.frame(
      language_name_en = c(
        "Gwich’in",
        "Hän",
        "Upper Tanana",
        "Northern Tutchone",
        "Southern Tutchone",
        "Kaska",
        "Tagish",
        "Tlingit"
      ),
      language_name_fr = c(
        "Gwich’in",
        "Hän",
        "Tanana supérieur",
        "Tutchone du Nord",
        "Tutchone du Sud",
        "Kaska",
        "Tagish",
        "Tlingit"
      ),
      stringsAsFactors = FALSE
    )
    DBI::dbAppendTable(
      con,
      "languages",
      languages_df
    )

    # Now let's add a table to hold First Nation language names for locations, linked on location_id. One column per language.
    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS public.location_names (
        location_id INTEGER NOT NULL REFERENCES public.locations (location_id) ON DELETE CASCADE ON UPDATE CASCADE,
        language_code INTEGER NOT NULL REFERENCES public.languages (language_code) ON DELETE CASCADE ON UPDATE CASCADE,
        name TEXT NOT NULL,
        PRIMARY KEY (location_id, language_code)
      );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS idx_location_names_location_id ON public.location_names (location_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS idx_location_names_language_code ON public.location_names (language_code);"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.location_names IS 'Table of First Nation language names for locations, one row per language:location association.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.location_names.name IS 'The name of the location in the specified First Nation language.';"
    )

    # Deal with views
    # Drop view and recreate in English
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS public.location_metadata_en;"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW public.location_metadata_en WITH (security_invoker='true') AS
      SELECT 
        loc.location_id,
        loc.name,
        loc.alias AS alias,
        loc.location_code,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS elevation,
        dl.datum_name_en AS datum,
        loc.note,
        array_agg(DISTINCT proj.name) AS projects,
        array_agg(DISTINCT net.name) AS networks,
        COALESCE(
          jsonb_agg(
            DISTINCT jsonb_build_object(
              'language_code', lng.language_code,
              'language_name_en', lng.language_name_en,
              'name', ln.name
            )
          ) FILTER (WHERE ln.location_id IS NOT NULL),
          '[]'::jsonb
        ) AS fn_names
      FROM ((((((public.locations loc
        LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
        LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
        LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
        LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
        LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
        LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
        LEFT JOIN public.location_names ln ON loc.location_id = ln.location_id
        LEFT JOIN public.languages lng ON ln.language_code = lng.language_code
      GROUP BY loc.location_id, loc.location_code, loc.name, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_en;"
    )

    DBI::dbExecute(
      con,
      "ALTER VIEW public.location_metadata_en OWNER TO admin;"
    )

    # Drop view and recreate in French
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS public.location_metadata_fr;"
    )
    DBI::dbExecute(
      con,
      "CREATE VIEW public.location_metadata_fr WITH (security_invoker='true') AS
      SELECT loc.location_id,
      loc.name_fr AS nom,
          loc.alias AS alias,
          loc.location_code AS code_de_site,
          loc.latitude,
          loc.longitude,
          dc.conversion_m AS altitude,
          dl.datum_name_fr AS datum,
          loc.note,
          array_agg(DISTINCT proj.name_fr) AS projets,
          array_agg(DISTINCT net.name_fr) AS réseaux,
          COALESCE(
            jsonb_agg(
              DISTINCT jsonb_build_object(
                'language_code', lng.language_code,
                'language_name_fr', lng.language_name_fr,
                'name', ln.name
              )
            ) FILTER (WHERE ln.location_id IS NOT NULL),
            '[]'::jsonb
          ) AS noms_premières_nations
        FROM ((((((public.locations loc
          LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
          LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
          LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
          LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
          LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
          LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
          LEFT JOIN public.location_names ln ON loc.location_id = ln.location_id
          LEFT JOIN public.languages lng ON ln.language_code = lng.language_code
        GROUP BY loc.location_id, loc.location_code, loc.name_fr, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_fr;"
    )

    DBI::dbExecute(
      con,
      "ALTER VIEW public.location_metadata_fr OWNER TO admin;"
    )

    # Grant select on views to public role
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE public.location_metadata_en TO public;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE public.location_metadata_fr TO public;"
    )

    # Create a new table with many-to-many relationship linking organizations to data sharing agreements.
    DBI::dbExecute(
      con,
      "
    CREATE TABLE IF NOT EXISTS public.organization_data_sharing_agreements (
      organization_id INT NOT NULL,
      data_sharing_agreement_id INT NOT NULL,
      PRIMARY KEY (organization_id, data_sharing_agreement_id),
      FOREIGN KEY (organization_id) REFERENCES public.organizations (organization_id),
      FOREIGN KEY (data_sharing_agreement_id) REFERENCES files.documents(document_id)
    );
    "
    )

    # Use the function files.check_data_sharing_agreement() to create a trigger on organization_data_sharing that enforces document type
    DBI::dbExecute(
      con,
      "
    CREATE TRIGGER trg_check_data_sharing_agreement_type
    BEFORE INSERT OR UPDATE ON public.organization_data_sharing_agreements
    FOR EACH ROW
    EXECUTE FUNCTION files.check_data_sharing_agreement();
    "
    )

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
