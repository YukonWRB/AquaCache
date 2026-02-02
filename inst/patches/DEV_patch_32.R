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
      "ALTER TABLE location_types ADD COLUMN IF NOT EXISTS type_suffix TEXT;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN location_types.type_suffix IS 'A short suffix to use in constructing location codes, e.g., SW for surface water, GW for groundwater, WW for wastewater, OC for oceanic, MET for meteorological.';"
    )

    for (i in 1:nrow(type_suffix)) {
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE location_types SET type_suffix = '",
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
      "DROP TABLE IF EXISTS locations_metadata_owners_operators;"
    )

    # Add a column 'data_sharing_agreement_id' to timeseries table
    DBI::dbExecute(
      con,
      "ALTER TABLE timeseries ADD COLUMN IF NOT EXISTS data_sharing_agreement_id INTEGER REFERENCES files.documents(document_id);"
    )
    # Re-use a function to check document type for data sharing agreement
    try(
      {
        DBI::dbExecute(
          con,
          "
    create trigger trg_check_data_sharing_agreement before insert or update on continuous.timeseries for each row execute function files.check_data_sharing_agreement()
    ;"
        )
      },
      silent = TRUE
    )

    # Drop column 'data_sharing_agreement_id' from locations table
    DBI::dbExecute(
      con,
      "ALTER TABLE locations DROP COLUMN IF EXISTS data_sharing_agreement_id;"
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
      "ALTER TABLE timeseries DROP COLUMN IF EXISTS location;"
    )

    # Rename 'location' in table 'locations' to 'alias' and drop not null
    # Find the name of the unique constraint on column 'location' in table 'locations'
    # Check if the column is present first (in case patch is re-run)
    column_check <- DBI::dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns WHERE table_name = 'locations' AND column_name = 'location';"
    )$column_name
    if (length(column_check) > 0) {
      unique_constraint_name <- DBI::dbGetQuery(
        con,
        "SELECT conname FROM pg_constraint WHERE conrelid = 'locations'::regclass AND contype = 'u' AND conkey = (SELECT array_agg(attnum) FROM pg_attribute WHERE attrelid = 'locations'::regclass AND attname = 'location');"
      )$conname
      if (length(unique_constraint_name) > 0) {
        # Drop the unique constraint if it exists
        DBI::dbExecute(
          con,
          paste0(
            "ALTER TABLE locations DROP CONSTRAINT ",
            unique_constraint_name,
            ";"
          )
        )
      }
      DBI::dbExecute(
        con,
        "ALTER TABLE locations ALTER COLUMN location DROP NOT NULL;"
      )

      DBI::dbExecute(
        con,
        "ALTER TABLE locations RENAME COLUMN location TO alias;"
      )
    }

    # Create a new column 'location_code' in table 'locations'
    DBI::dbExecute(
      con,
      "ALTER TABLE locations ADD COLUMN IF NOT EXISTS location_code TEXT;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN locations.location_code IS 'A unique code for the location, ideally constructed from the location National Hydro Network polygon in which the location is situated, using the first 2 digits and 2-3 letters of the polygon name, location type suffix, and a unique number.';"
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

    # Iterate through all locations with NULL location_code and generate codes
    exist_locs <- DBI::dbGetQuery(
      con,
      "SELECT location_id, latitude, longitude, location_type FROM locations WHERE location_code IS NULL;"
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

      # Update the location_code in the database
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

    # Make location_code NOT NULL
    DBI::dbExecute(
      con,
      "ALTER TABLE locations ALTER COLUMN location_code SET NOT NULL;"
    )
    # Make location_code UNIQUE
    DBI::dbExecute(
      con,
      "ALTER TABLE locations ADD CONSTRAINT unique_location_code UNIQUE (location_code);"
    )

    # Deal with views
    DBI::dbExecute(
      con,
      "CREATE VIEW public.location_metadata_en WITH (security_invoker='true') AS
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
        array_agg(DISTINCT net.name) AS networks
      FROM ((((((public.locations loc
        LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
        LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
        LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
        LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
        LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
        LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
      GROUP BY loc.location_id, loc.location_code, loc.name, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_en;"
    )

    DBI::dbExecute(
      con,
      "ALTER VIEW public.location_metadata_en OWNER TO admin;"
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
          array_agg(DISTINCT net.name_fr) AS réseaux
        FROM ((((((public.locations loc
          LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
          LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
          LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
          LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
          LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
          LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
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
      document_id INT NOT NULL,
      PRIMARY KEY (organization_id, document_id),
      FOREIGN KEY (organization_id) REFERENCES public.organizations (organization_id),
      FOREIGN KEY (document_id) REFERENCES files.documents(document_id)
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
