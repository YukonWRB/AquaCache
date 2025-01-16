# Patch 11
# Evolution of discrete data storage and of location metadata storage

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}


message("Working on Patch 11. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Lock the entire database to prevent writes while we make changes
  DBI::dbExecute(con, "LOCK pg_database IN ACCESS EXCLUSIVE MODE;")
  
  try({
    DBI::dbExecute(con, "CREATE ROLE discrete_editor WITH BYPASSRLS;")
  })
  try({
    DBI::dbExecute(con, "CREATE ROLE continuous_editor WITH BYPASSRLS;")
  })
  
  
  # Move tables to new schemas to clean things up
  DBI::dbExecute(con, "CREATE SCHEMA continuous;")
  DBI::dbExecute(con, "COMMENT ON SCHEMA continuous IS 'Schema to hold continuous data and associated metadata.';")
  
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT SELECT ON TABLES TO public_reader;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT SELECT ON SEQUENCES TO public_reader; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT EXECUTE ON FUNCTIONS TO public_reader;
")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT SELECT ON TABLES TO discrete_editor;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT SELECT ON SEQUENCES TO discrete_editor; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT EXECUTE ON FUNCTIONS TO discrete_editor;
")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT SELECT ON TABLES TO continuous_editor;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT SELECT ON SEQUENCES TO continuous_editor; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA continuous
GRANT EXECUTE ON FUNCTIONS TO continuous_editor;
")
  
  # Move tables and views to this schema:
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE forecasts SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE timeseries SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE thresholds SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE extrema SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE grades SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE approvals SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE qualifiers SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE owners SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE contributors SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE corrections SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE rating_curve_points SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE rating_curve_shifts SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE rating_curves SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER TABLE settings RENAME TO fetch_settings;")
  DBI::dbExecute(con, "ALTER TABLE fetch_settings SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER VIEW measurements_calculated_daily_corrected SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER VIEW measurements_continuous_corrected SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER VIEW measurements_hourly_corrected SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION apply_corrections SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION check_approvals_overlap SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION check_contributors_overlap SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION check_grades_overlap SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION check_owners_overlap SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION check_qualifiers_overlap SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION delete_old_forecasts SET SCHEMA continuous;")
  DBI::dbExecute(con, "ALTER FUNCTION validate_corrections SET SCHEMA continuous;")
  
  
  DBI::dbExecute(con, "ALTER TABLE owners_contributors RENAME TO organizations;")
  DBI::dbExecute(con, "ALTER TABLE organizations RENAME COLUMN owner_contributor_id TO organization_id")
  DBI::dbExecute(con, "ALTER TABLE continuous.owners RENAME COLUMN owner_contributor_id TO organization_id")
  DBI::dbExecute(con, "ALTER TABLE continuous.contributors RENAME COLUMN owner_contributor_id TO organization_id")
  
  
  DBI::dbExecute(con, "CREATE SCHEMA files;")
  DBI::dbExecute(con, "COMMENT ON SCHEMA files IS 'Schema to hold files and associated metadata.';")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT SELECT ON TABLES TO public_reader;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT SELECT ON SEQUENCES TO public_reader; -- Needed for serial/bigserial columns
")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT SELECT ON TABLES TO discrete_editor;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT SELECT ON SEQUENCES TO discrete_editor; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT EXECUTE ON FUNCTIONS TO discrete_editor;
")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT SELECT ON TABLES TO continuous_editor;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT SELECT ON SEQUENCES TO continuous_editor; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA files
GRANT EXECUTE ON FUNCTIONS TO continuous_editor;
")
  
  DBI::dbExecute(con, "ALTER TABLE documents SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER TABLE images SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER TABLE images_index SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER TABLE document_types SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER TABLE documents_spatial SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION check_location_images SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION check_data_sharing_agreement SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION enforce_lat_lon_constraints SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION enforce_share_with_restriction SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION update_document_flags_after_delete SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION update_document_flags_after_insert SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION update_line_flag SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION update_location_flag SET SCHEMA files;")
  DBI::dbExecute(con, "ALTER FUNCTION update_polygon_flag SET SCHEMA files;")
  
  DBI::dbExecute(con, "ALTER FUNCTION update_geom_type SET SCHEMA spatial;")
  
  
  DBI::dbExecute(con, "ALTER SCHEMA measurements_discrete RENAME TO discrete;")
  DBI::dbExecute(con, "COMMENT ON SCHEMA discrete IS 'Schema to hold discrete data and associated metadata.';")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT SELECT ON TABLES TO public_reader;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT SELECT ON SEQUENCES TO public_reader; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT EXECUTE ON FUNCTIONS TO public_reader;
")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT SELECT ON TABLES TO discrete_editor;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT SELECT ON SEQUENCES TO discrete_editor; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT EXECUTE ON FUNCTIONS TO discrete_editor;
")
  
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT SELECT ON TABLES TO continuous_editor;
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT SELECT ON SEQUENCES TO continuous_editor; -- Needed for serial/bigserial columns
")
  DBI::dbExecute(con, "
ALTER DEFAULT PRIVILEGES IN SCHEMA discrete
GRANT EXECUTE ON FUNCTIONS TO continuous_editor;
")
  
  DBI::dbExecute(con, "DROP FUNCTION check_sample_class_exists;")
  DBI::dbExecute(con, "DROP FUNCTION prevent_delete_public_group")
  DBI::dbExecute(con, "DROP FUNCTION remove_group_id_from_share_with")
  DBI::dbExecute(con, "DROP FUNCTION remove_group_id_from_users")
  DBI::dbExecute(con, "DROP FUNCTION validate_user_groups")
  DBI::dbExecute(con, "DROP FUNCTION update_modify_datetime")
  
  
  # Update the search path (takes effect at next connection)
  DBI::dbExecute(con, "ALTER DATABASE aquacache SET search_path TO public, continuous, discrete, spatial, files, instruments, information;")
  # Update the search path for this session
  DBI::dbExecute(con, "SET search_path TO public, continuous, discrete, spatial, files, instruments, information;")
  
  DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public, continuous, discrete, spatial, files, instruments, information TO public_reader, discrete_editor, continuous_editor, yg;")
  
  
  # 1. Make modifications for location metadata storage #################
  # General plan: 
  # Create a new table to hold sub-locations. 
  # Modify timeseries table to include sub locations in unique key. 
  # Create a table to hold instrument information that is unique on location AND sub-locations.
  # No modification to instrument table for now, but it'll need a look to make sure it is flexible enough for all types of instruments.
  
  ## Create new table 'sub_locations' #################
  DBI::dbExecute(con, "
  CREATE TABLE sub_locations (
    sub_location_id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    sub_location_name TEXT NOT NULL,
    sub_location_name_fr TEXT,
    latitude NUMERIC,
    longitude NUMERIC,
    note TEXT,
    share_with TEXT[], -- referential integrity to database users enforced via function/trigger
    UNIQUE (location_id, sub_location_name)
  );
  ")
  DBI::dbExecute(con, "ALTER TABLE sub_locations OWNER TO admin;")
  
  
  
  
  # Create trigger for existing function validate_share_with to ensure that all users in share_with column exist in the database
  DBI::dbExecute(con, "
  CREATE TRIGGER validate_share_with_trigger
  BEFORE INSERT OR UPDATE ON sub_locations
  FOR EACH ROW
  EXECUTE FUNCTION public.validate_share_with();
  ")
  
  # Make this table RLS-enabled, apply same policy as elsewhere in DB
  DBI::dbExecute(con, paste0("CREATE INDEX sub_locations_share_with_gin_idx ON sub_locations USING GIN (share_with);"))
  DBI::dbExecute(con, "ALTER TABLE sub_locations ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, paste0("CREATE POLICY rls ON sub_locations
                              FOR ALL
                              USING (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              )
                              WITH CHECK (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              );
                 "))
  
  # add column and fk to new table in 'timeseries'
  DBI::dbExecute(con, "
  ALTER TABLE timeseries 
  ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id); -- can be null
  ")
  
  # Unique key on timeseries table is adjusted lower down!
  
  
  ## Create table to store instrument arrays at locations/sub_locations ###########
  # Create table 'locations_metadata_instruments'
  DBI::dbExecute(con, "
  CREATE TABLE locations_metadata_instruments (
    metadata_id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    sub_location_id INTEGER REFERENCES sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    instruments INTEGER[],  --array, nullable to allow for a break in usage at a location. Reference to 'instruments' table must be done via a function/trigger because FKs are not supported for arrays.
    start_dt TIMESTAMPTZ NOT NULL,
    end_dt TIMESTAMPTZ,
    created TIMESTAMPTZ DEFAULT now() NOT NULL,
    modified TIMESTAMPTZ,
    UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id)  -- the not distinct part means that you can't have two null sub_location_ids
);
")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_instruments OWNER TO admin;")
  
  
  # Trigger to update the modified timestamp
  DBI::dbExecute(con, "
  create trigger update_locations_metadata_instruments_modified before
  update on
  locations_metadata_instruments for each row execute function update_modified()
  ")
  
  # Function and trigger to check reference to table instruments.instrument_id
  DBI::dbExecute(con, "
                 CREATE OR REPLACE FUNCTION check_instruments_reference()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if all instrument IDs in the instruments array exist in the instruments table
    IF EXISTS (
        SELECT 1
        FROM UNNEST(NEW.instruments) AS instrument_id
        WHERE NOT EXISTS (
            SELECT 1 FROM instruments WHERE instrument_id = instrument_id
        )
    ) THEN
        RAISE EXCEPTION 'Invalid Instrument ID: %', instrument_id;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "
  CREATE TRIGGER check_instruments_trigger
BEFORE INSERT OR UPDATE ON locations_metadata_instruments
FOR EACH ROW
EXECUTE FUNCTION check_instruments_reference();
")
  
  # Trigger and function to make sure that records do not overlap in time
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION public.check_instrument_meta_overlap()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
              BEGIN
                  IF EXISTS (
                      SELECT 1
                      FROM locations_metadata_instruments
                      WHERE location_id = NEW.location_id
                      AND sub_location_id = NEW.sub_location_id
                      AND metadata_id != NEW.metadata_id  -- Exclude the current row in an UPDATE
                      AND (
                          NEW.start_dt < end_dt AND (NEW.end_dt IS NULL OR NEW.end_dt > start_dt)
                      )
                  ) THEN
                      RAISE EXCEPTION 'Time range overlap detected for location_id: %, sub_location_id: %', NEW.location_id, NEW.sub_location_id;
                  END IF;
                  RETURN NEW;
              END;
              $function$
;
")
  
  DBI::dbExecute(con, "
                 create constraint trigger 
                 check_instrument_meta_overlap 
                 after insert or update on locations_metadata_instruments 
                 deferrable initially deferred 
                 for each row execute function check_instrument_meta_overlap()
                 ")
  
  
  # Then, dispense with existing tables as their data is taken care of by locations_metadata_instruments'
  DBI::dbExecute(con, "
    DROP TABLE locations_metadata_acquisition CASCADE;
    ")
  DBI::dbExecute(con, "
    DROP TABLE locations_metadata_transmission CASCADE;
    ")
  
  ### Modify existing tables to work with sub_locations ##############
  # Modify existing tables locations_metadata_access, locations_metadata_infrastructure, locations_metadata_infrastructure_groundwater, locations_metadata_infrastructure_hydromet, locations_metadata_maintenance, locations_metadata_owners_operators, locations_metadata_xsections to also (optionally) use sub_location_id, with the same nulls not distinct unique key.
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_access ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure_groundwater ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure_hydromet ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_maintenance ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_owners_operators ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_xsections ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id);")
  
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_access DROP CONSTRAINT IF EXISTS locations_metadata_access_unique;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure DROP CONSTRAINT IF EXISTS locations_metadata_infrastructure_unique;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure_groundwater DROP CONSTRAINT IF EXISTS locations_metadata_infrastructure_groundwater_unique;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure_hydromet DROP CONSTRAINT IF EXISTS locations_metadata_infrastructure_hydromet_unique;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_maintenance DROP CONSTRAINT IF EXISTS locations_metadata_maintenance_unique;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_owners_operators DROP CONSTRAINT IF EXISTS locations_metadata_owners_operators_unique;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_xsections DROP CONSTRAINT IF EXISTS locations_metadata_xsections_unique;")
  
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_access ADD CONSTRAINT locations_metadata_access_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure ADD CONSTRAINT locations_metadata_infrastructure_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure_groundwater ADD CONSTRAINT locations_metadata_infrastructure_groundwater_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_infrastructure_hydromet ADD CONSTRAINT locations_metadata_infrastructure_hydromet_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_maintenance ADD CONSTRAINT locations_metadata_maintenance_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_owners_operators ADD CONSTRAINT locations_metadata_owners_operators_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_xsections ADD CONSTRAINT locations_metadata_xsections_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);")
  
  
  # Populate ownership and operator metadata for locations and sub-locations in table 'locations_metadata_owners_operators' from table 'locations'
  # Some entries to location table with a 'location' starting with YOWN need to be updated to have an owner of '2'\
  DBI::dbExecute(con, "UPDATE locations SET owner = 2 WHERE location LIKE 'YOWN%';")
  
  tmp <- DBI::dbGetQuery(con, "SELECT location_id, owner FROM locations WHERE owner IS NOT NULL")
  
  for (i in unique(tmp$location_id)) {
    # Fetch the earliest datetime for this location from table 'timeseries'
    start_dt <- DBI::dbGetQuery(con, paste0("SELECT MIN(start_datetime) FROM timeseries WHERE location_id = ", i))[1,1]
    if (is.na(start_dt)) {
      next
    }
    df <- data.frame(location_id = i, owner = tmp[tmp$location_id == i, "owner"], operator = tmp[tmp$location_id == i, "owner"], start_datetime = start_dt)
    DBI::dbAppendTable(con, "locations_metadata_owners_operators", df)
  }
  
  
  
  
  
  
  # 2. Make modifications for discrete data storage #################
  
  ## Create 'sample_series' table ###########
  DBI::dbExecute(con, "
                 CREATE TABLE discrete.sample_series (
                      sample_series_id SERIAL PRIMARY KEY,
                      location_id INTEGER NOT NULL REFERENCES locations(location_id),
                      sub_location_id INTEGER REFERENCES sub_locations(sub_location_id),
                      synch_from TIMESTAMPTZ, -- start of the period to synchronize data from
                      synch_to TIMESTAMPTZ, -- end of the period to synchronize data from
                      default_owner INTEGER NOT NULL REFERENCES organizations(organization_id),
                      default_contributor INTEGER REFERENCES organizations(organization_id),
                      last_new_data TIMESTAMPTZ,
                      last_synchronize TIMESTAMPTZ,
                      active BOOLEAN NOT NULL DEFAULT TRUE,
                      source_fx TEXT NOT NULL,
                      source_fx_args TEXT,
                      note TEXT,
                      UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, synch_from, synch_to)
                      );
                ")
  DBI::dbExecute(con, "ALTER TABLE sample_series OWNER TO admin;")
  
  # Ensure that synch_from is before synch_to, and that for each location_id, sub_location_id unique pair, records do not overlap in time using synch_from and synch_to
  DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION discrete.check_sample_series_overlap()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
    -- Ensure synch_from is before synch_to if both are NOT NULL
    IF NEW.synch_from IS NOT NULL AND NEW.synch_to IS NOT NULL AND NEW.synch_from >= NEW.synch_to THEN
        RAISE EXCEPTION 'The start of the new synchronization period must be before the end of the new synchronization period.';
    END IF;

    -- Check for overlaps with existing records
    IF EXISTS (
        SELECT 1
        FROM discrete.sample_series
        WHERE location_id = NEW.location_id
          AND sub_location_id = NEW.sub_location_id
          AND sample_series_id != NEW.sample_series_id -- Exclude the current row in an UPDATE
          AND (
              -- Case 1: Both synch_from and synch_to are defined
              (NEW.synch_from IS NOT NULL AND NEW.synch_to IS NOT NULL
               AND (synch_from, synch_to) OVERLAPS (NEW.synch_from, NEW.synch_to))
              
              -- Case 2: Existing synch_from IS NULL, synch_to IS NOT NULL
              OR (synch_from IS NULL AND synch_to IS NOT NULL
                  AND NEW.synch_from IS NOT NULL AND NEW.synch_from < synch_to)
              
              -- Case 3: Existing synch_to IS NULL, synch_from IS NOT NULL
              OR (synch_to IS NULL AND synch_from IS NOT NULL
                  AND NEW.synch_to IS NOT NULL AND NEW.synch_to > synch_from)
              
              -- Case 4: Both synch_from and synch_to are NULL (unbounded time range)
              OR (NEW.synch_from IS NULL AND NEW.synch_to IS NULL
                  AND synch_from IS NULL AND synch_to IS NULL)
          )
    ) THEN
        RAISE EXCEPTION 'Time range overlap detected for location_id: %, sub_location_id: %', NEW.location_id, NEW.sub_location_id;
    END IF;

    RETURN NEW;
END;
$function$;
")
  
  DBI::dbExecute(con, "
CREATE TRIGGER trigger_check_sample_series_overlap
BEFORE INSERT OR UPDATE ON discrete.sample_series
FOR EACH ROW
EXECUTE FUNCTION discrete.check_sample_series_overlap();
")
  
  
  
  
  
  
  DBI::dbExecute(con, "COMMENT ON TABLE continuous.timeseries IS 
                 'Provides a record of every continuous-type timeseries in the database. Each timeseries is unique by its combination of location, sub_location, parameter, media, period_type, record_rate, sensor priority, and z (elevation or depth). Continuous data is data gathered at regular and usually frequent intervals by automatic means. Refer to table sample_series for lab and manual measurement.'
                 ")
  
  DBI::dbExecute(con, "COMMENT ON TABLE discrete.sample_series IS
                 'Provides a means of automating the import of discrete data (lab or field measurements) via the source_fx and optional arguments, designed to work with the AquaCache R package. Actual sample metadata is stored in the samples table, measurements in the results table.'
                 ")
  
  ## Create 'samples' table ########
  DBI::dbExecute(con, "
                 CREATE TABLE discrete.samples (
                 sample_id SERIAL PRIMARY KEY,
                 location_id INTEGER NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
                 sub_location_id INTEGER REFERENCES sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 media_id INTEGER NOT NULL REFERENCES media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE,
                 z NUMERIC,
                 datetime TIMESTAMPTZ NOT NULL,
                 target_datetime TIMESTAMPTZ,
                 collection_method INTEGER NOT NULL REFERENCES collection_methods(collection_method_id) ON UPDATE CASCADE ON DELETE CASCADE,
                 sample_type INTEGER NOT NULL REFERENCES sample_types(sample_type_id) ON UPDATE CASCADE ON DELETE CASCADE,
                 linked_with INTEGER REFERENCES samples(sample_id) ON UPDATE CASCADE ON DELETE SET NULL, -- (to associate samples with each other)
                 sample_volume_ml NUMERIC,
                 purge_volume_l NUMERIC,
                 purge_time_min NUMERIC,
                 flow_rate_l_min NUMERIC,
                 wave_hgt_m NUMERIC,
                 sample_grade INTEGER REFERENCES grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 sample_approval INTEGER REFERENCES approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 sample_qualifier INTEGER REFERENCES qualifier_types(qualifier_type_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 owner INTEGER NOT NULL REFERENCES organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE,
                 contributor INTEGER REFERENCES organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 comissioning_org INTEGER REFERENCES organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 sampling_org INTEGER REFERENCES organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL,
                 documents INTEGER[], -- array of document_ids, existence of documents enforced via function/trigger
                 share_with TEXT[] DEFAULT '{public_reader}', -- referential integrity to database users enforced via function/trigger
                 import_source TEXT,
                 import_source_id TEXT,
                 no_update BOOLEAN DEFAULT FALSE,
                 note TEXT,
                 created TIMESTAMPTZ DEFAULT now() NOT NULL,
                 modified TIMESTAMPTZ,
                 UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, media_id, z, datetime, sample_type, collection_method) --nulls not distinct is to enforce uniqueness on null sub locations
                 )
                 ")
  DBI::dbExecute(con, "ALTER TABLE samples OWNER TO admin;")
  
  DBI::dbExecute(con, "CREATE INDEX idx_samples_location_id ON discrete.samples(location_id);")
  DBI::dbExecute(con, "CREATE INDEX idx_samples_datetime ON discrete.samples(datetime);")
  DBI::dbExecute(con, "CREATE INDEX idx_samples_media_id ON discrete.samples(media_id);")
  
  # Make this table RLS-enabled, apply same policy as elsewhere in DB
  DBI::dbExecute(con, paste0("CREATE INDEX samples_share_with_gin_idx ON samples USING GIN (share_with);"))
  DBI::dbExecute(con, "ALTER TABLE samples ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, paste0("CREATE POLICY rls ON samples
                              FOR ALL
                              USING (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              )
                              WITH CHECK (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              );
                 "))
  
  # Trigger to update the modified timestamp
  DBI::dbExecute(con, "
  create trigger update_samples_modified 
  before update
  on
  discrete.samples for each row execute function update_modified()
  ")
  
  # Create trigger for existing function validate_share_with to ensure that all users in share_with column exist in the database
  DBI::dbExecute(con, "
  CREATE TRIGGER validate_share_with_trigger
  BEFORE INSERT OR UPDATE ON discrete.samples
  FOR EACH ROW
  EXECUTE FUNCTION public.validate_share_with();
  ")
  
  # Create function and trigger to ensure that documents in the 'documents' array exist in the 'documents' table, column 'document_id'
  DBI::dbExecute(con, "
        CREATE OR REPLACE FUNCTION public.validate_documents_array()
        RETURNS TRIGGER AS $$
        BEGIN
        -- skip validation if the documents column is NULL
        IF NEW.documents IS NOT NULL THEN
            -- Check if every document ID in the array exists in the documents table
            IF EXISTS (
                SELECT 1
                FROM unnest(NEW.documents) AS doc_id
                LEFT JOIN documents ON documents.document_id = doc_id
                WHERE documents.document_id IS NULL
            ) THEN
                RAISE EXCEPTION 'One or more document IDs in the documents array do not exist in the documents table.';
            END IF;
        END IF;
        
            RETURN NEW;
        END;
        $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
                CREATE TRIGGER validate_documents_trigger
                BEFORE INSERT OR UPDATE ON discrete.samples
                FOR EACH ROW
                EXECUTE FUNCTION public.validate_documents_array();
")
  
  ## Modify some existing tables and create new ones to work with 'results' table ###############
  # Modify sample_types.sample_type = field msr/obs to sample-field msr/obs, and add 'unknown'
  DBI::dbExecute(con, "UPDATE sample_types SET sample_type = 'sample-field msr/obs - no lab results expected' WHERE sample_type = 'field msr/obs';")
  DBI::dbExecute(con, "INSERT INTO sample_types (sample_type) VALUES ('unknown');")
  
  # Make a table to differentiate between field and lab values
  DBI::dbExecute(con, "
                 CREATE TABLE discrete.result_types (
                 result_type_id SERIAL PRIMARY KEY,
                 result_type TEXT NOT NULL
                 );
                 ")
  DBI::dbExecute(con, "ALTER TABLE result_types OWNER TO admin;")
  
  result_types <- data.frame(result_type = c("field", "lab", "unknown"))
  DBI::dbAppendTable(con, "result_types", result_types)
  
  # Make a new fraction called 'unknown'
  new_fraction <- data.frame(sample_fraction = "unknown")
  DBI::dbAppendTable(con, "sample_fractions", new_fraction
  )
  
  # rename 'analysis_protocols' to 'protocols_methods'
  DBI::dbExecute(con, "ALTER TABLE analysis_protocols RENAME TO protocols_methods;")
  
  
  ## Create table 'results' #################
  DBI::dbExecute(con, "
                 CREATE TABLE discrete.results (
                 result_id SERIAL PRIMARY KEY,
                 sample_id INTEGER NOT NULL REFERENCES samples(sample_id) ON UPDATE CASCADE ON DELETE CASCADE,
                 result_type INTEGER NOT NULL REFERENCES discrete.result_types(result_type_id) ON UPDATE CASCADE ON DELETE CASCADE,  -- lab, field, unknown
                 parameter_id INTEGER NOT NULL REFERENCES parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE, -- also dictates the units to use
                 sample_fraction INTEGER REFERENCES sample_fractions(sample_fraction_id) ON UPDATE CASCADE ON DELETE SET NULL,  -- WAD, SAD, total, dissolved, etc
                 result NUMERIC,  -- if null then a function/trigger will ensure that result_condition is also populated
                 result_condition INTEGER REFERENCES result_conditions(result_condition_id) ON UPDATE CASCADE ON DELETE CASCADE, -- below, above, etc. Mandatory if result is null
                 result_condition_value NUMERIC,
                 result_value_type INTEGER REFERENCES result_value_types(result_value_type_id) ON UPDATE CASCADE ON DELETE SET NULL, -- actual, estimated, blank corrected, calculated, etc
                 result_speciation INTEGER REFERENCES result_speciations(result_speciation_id) ON UPDATE CASCADE ON DELETE SET NULL, -- as CaCO3, as N, etc. Mandatory in some cases, based on parameter
                 protocol_method INTEGER REFERENCES protocols_methods(protocol_id) ON UPDATE CASCADE ON DELETE SET NULL, -- the method used to obtain the result
                 laboratory INTEGER REFERENCES laboratories(lab_id) ON UPDATE CASCADE ON DELETE SET NULL, -- the lab that did the analysis
                 analysis_datetime TIMESTAMPTZ, -- when the analysis was done
                 share_with TEXT[] DEFAULT '{public_reader}', -- referential integrity to database users enforced via function/trigger
                 no_update BOOLEAN DEFAULT FALSE,
                 created TIMESTAMPTZ DEFAULT now() NOT NULL,
                 modified TIMESTAMPTZ,
                 UNIQUE NULLS NOT DISTINCT (sample_id, parameter_id, sample_fraction, result_value_type, result_speciation, protocol_method, laboratory, analysis_datetime)
                 )
                 ")
  DBI::dbExecute(con, "ALTER TABLE results OWNER TO admin;")
  
  
  # Indices to speed things up
  DBI::dbExecute(con, "CREATE INDEX idx_results_sample_id ON discrete.results(sample_id);")
  DBI::dbExecute(con, "CREATE INDEX idx_results_parameter_id ON discrete.results(parameter_id);")
  DBI::dbExecute(con, "CREATE INDEX idx_results_sample_parameter ON discrete.results(sample_id, parameter_id);")
  DBI::dbExecute(con, "CREATE INDEX idx_results_result_condition ON discrete.results(result_condition);")
  
  # Make this table RLS-enabled, apply same policy as elsewhere in DB
  DBI::dbExecute(con, paste0("CREATE INDEX results_share_with_gin_idx ON results USING GIN (share_with);"))
  DBI::dbExecute(con, "ALTER TABLE results ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, paste0("CREATE POLICY rls ON results
                              FOR ALL
                              USING (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              )
                              WITH CHECK (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              );
                 "))
  
  # Trigger to update the modified timestamp
  DBI::dbExecute(con, "
  create trigger update_results_modified 
  before update
  on
  discrete.results for each row execute function update_modified()
  ")
  
  # Create trigger for existing function validate_share_with to ensure that all users in share_with column exist in the database
  DBI::dbExecute(con, "
  CREATE TRIGGER validate_share_with_trigger
  BEFORE INSERT OR UPDATE ON discrete.results
  FOR EACH ROW
  EXECUTE FUNCTION public.validate_share_with();
  ")
  
  # Add some constraints on table 'results'
  # if result is null, result_condition must be populated
  DBI::dbExecute(con, "ALTER TABLE discrete.results ADD CONSTRAINT chk_result_condition CHECK (((result_condition IS NULL) OR (result IS NULL)))")
  
  # if result_condition is 1 or 2, result_condition_value must be populated
  DBI::dbExecute(con, "ALTER TABLE discrete.results ADD CONSTRAINT chk_result_condition_value CHECK (((result_condition_value IS NULL) OR (result_condition = ANY (ARRAY[1, 2]))))")
  
  # if the parameter_id in table 'parameters' has TRUE in result_speciation column, result_speciation must be populated
  DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION discrete.enforce_result_speciation()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if the associated parameter_id requires result_speciation
    IF EXISTS (
        SELECT 1
        FROM parameters
        WHERE parameter_id = NEW.parameter_id
          AND result_speciation = TRUE
          AND NEW.result_speciation IS NULL
    ) THEN
        RAISE EXCEPTION 'Result speciation must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "
CREATE TRIGGER trg_enforce_result_speciation
BEFORE INSERT OR UPDATE ON discrete.results
FOR EACH ROW
EXECUTE FUNCTION enforce_result_speciation();
")
  
  # if the parameter_id in table 'sample_fraction' has TRUE in sample_fraction column, sample_fraction must be populated
  DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION discrete.enforce_sample_fraction()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if the associated parameter_id requires a sample fraction
    IF EXISTS (
        SELECT 1
        FROM parameters
        WHERE parameter_id = NEW.parameter_id
          AND sample_fraction = TRUE
          AND NEW.sample_fraction IS NULL
    ) THEN
        RAISE EXCEPTION 'Sample fraction must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "
CREATE TRIGGER trg_enforce_sample_fraction
BEFORE INSERT OR UPDATE ON discrete.results
FOR EACH ROW
EXECUTE FUNCTION enforce_sample_fraction();
")
  
  
  # 3. Move data. discrete data in 'timeseries' to 'samples' and 'sample_series', 'measurements' to 'results'. ############
  # Pull from timeseries WHERE category  = 'discrete'. Drop irrelevant columns: period_type, category, last_daily_calculation, record_rate, sensor_priority
  sample_series <- DBI::dbGetQuery(con, "SELECT timeseries_id, location, location_id, sub_location_id, parameter_id, media_id, z, last_new_data, last_synchronize, source_fx, source_fx_args, note, active FROM timeseries WHERE category = 'discrete'")
  
  # Find parameter_id for definite field parameters: SWE, snow depth
  field <- DBI::dbGetQuery(con, "SELECT parameter_id FROM parameters WHERE param_name IN ('snow depth', 'snow water equivalent', 'water level', 'discharge, river/stream')")
  
  ## Move the data into sample_series, samples, and results
  message("Moving discrete data to new table structure. This will take a while (I didn't code it efficiently at all, sorry).")
  all_sample_series <- data.frame()
  for (i in unique(sample_series$location_id)) {
    if (!is.null(sample_series[sample_series$location_id == i, "source_fx"])) { # Make entry in sample_series
      series <- data.frame(location_id = i,
                           last_new_data = min(sample_series[sample_series$location_id == i, "last_new_data"]),
                           active = any(sample_series[sample_series$location_id == i, "active"]),
                           source_fx = sample_series[sample_series$location_id == i, "source_fx"][[1]],
                           source_fx_args = if (all(is.na(sample_series[sample_series$location_id == i, "source_fx_args"]))) NA else paste(sample_series[sample_series$location_id == i, "source_fx_args"], collapse = " AND "),
                           note = if (all(is.na(sample_series[sample_series$location_id == i, "note"]))) NA else paste(sample_series[sample_series$location_id == i, "note"], collapse = " AND "),
                           default_owner = DBI::dbGetQuery(con, paste0("SELECT DISTINCT owner FROM timeseries WHERE location_id = ", i, ";"))[,1],
                           default_contributor = DBI::dbGetQuery(con, paste0("SELECT DISTINCT owner FROM timeseries WHERE location_id = ", i, ";"))[,1])
      all_sample_series <- rbind(all_sample_series, series)
    } # else no source_fx, so no need to make an entry in sample_series
  }
  DBI::dbAppendTable(con, "sample_series", all_sample_series)
  
  measurements <- DBI::dbGetQuery(con, "SELECT * FROM measurements_discrete;")
  
  # Merge columns from 'sample_series' on column 'timeseries': parameter_id, media_id, z
  measurements <- merge(measurements, sample_series[, c("timeseries_id", "parameter_id", "media_id", "z", "location_id")], by = "timeseries_id", all.x = TRUE)
  measurements <- measurements[!duplicated(measurements),]
  measurements <- measurements[order(measurements$datetime),]
    
    # Get the unique datetimes - these are the samples
  all_meas <- data.frame()
  count <- 1
  for (i in unique(measurements$location_id)) {  # Create a series of entries to the 'sample' table for each sampling event
    message("Processing location ", count, " of ", length(unique(measurements$location_id)))
    count <- count + 1
    sub <- measurements[measurements$location_id == i,] # Isolate data specific to this location
    for (j in 1:length(unique(sub$datetime))) { # Make a record per sample datetime
      datetime <- unique(sub$datetime)[j]
      sample <- data.frame(
        location_id = i,
        media_id = unique(sub[sub$datetime == datetime, "media_id"]),
        z = unique(sub[sub$datetime == datetime, "z"]),
        datetime = datetime,
        target_datetime = unique(sub[sub$datetime == datetime, "target_datetime"]),
        collection_method = unique(sub[sub$datetime == datetime, "collection_method"]),
        sample_type = unique(sub[sub$datetime == datetime, "sample_type"]),
        owner = DBI::dbGetQuery(con, paste0("SELECT DISTINCT owner FROM timeseries WHERE location_id = ", unique(sub[sub$datetime == datetime, "location_id"]), ";"))[,1],
        contributor = DBI::dbGetQuery(con, paste0("SELECT DISTINCT owner FROM timeseries WHERE location_id = ", unique(sub[sub$datetime == datetime, "location_id"]), ";"))[,1],
        share_with = if (all(is.na(sub[sub$datetime == datetime, "share_with"]))) NA else unique(sub[sub$datetime == datetime, "share_with"]),
        import_source = "AquaCache patch 11 - transfer from old measurements_continuous table.",
        no_update = unique(sub[sub$datetime == datetime, "no_update"]),
        note = if (all(is.na(sub[sub$datetime == datetime, "note"]))) NA else paste(sub[sub$datetime == datetime, "note"], collapse = " AND ")
      )
      DBI::dbAppendTable(con, "samples", sample)
      
      # Get the sample_id for this sample
      new_sample_id <- DBI::dbGetQuery(con, paste0("SELECT sample_id FROM samples WHERE location_id = ", unique(sub[sub$datetime == datetime, "location_id"]), " AND datetime = '", as.character(datetime), " UTC';"))[1,1]
      
      # Add in the measurements taken during this sample
      new_meas <- data.frame(sample_id = new_sample_id,
                             result_type = ifelse(sub[sub$datetime == datetime, "parameter_id"] %in% field$parameter_id, 1, 3),
                             parameter_id = sub[sub$datetime == datetime, "parameter_id"],
                             sample_fraction = sub[sub$datetime == datetime, "sample_fraction"],
                             result = sub[sub$datetime == datetime, "value"],
                             result_condition = sub[sub$datetime == datetime, "result_condition"],
                             result_condition_value = sub[sub$datetime == datetime, "result_condition_value"],
                             result_value_type = sub[sub$datetime == datetime, "result_value_type"],
                             result_speciation = sub[sub$datetime == datetime, "result_speciation"],
                             protocol_method = sub[sub$datetime == datetime, "protocol"],
                             laboratory = sub[sub$datetime == datetime, "lab"],
                             analysis_datetime = sub[sub$datetime == datetime, "analysis_datetime"],
                             share_with = sub[sub$datetime == datetime, "share_with"],
                             no_update = sub[sub$datetime == datetime, "no_update"],
                             created = sub[sub$datetime == datetime, "created_modified"]
      )
      all_meas <- rbind(all_meas, new_meas)
    }
  }
  DBI::dbAppendTable(con, "results", all_meas)

  
  # Delete measurements_discrete with cascade
  DBI::dbExecute(con, "DROP TABLE measurements_discrete CASCADE;")
  
  
  # 4. Do the final modifications to 'timeseries' and 'locations' #########
  DBI::dbExecute(con, "DELETE FROM timeseries WHERE category = 'discrete'")
  DBI::dbExecute(con, "ALTER TABLE timeseries DROP COLUMN category CASCADE;")
  DBI::dbExecute(con, "ALTER TABLE timeseries RENAME COLUMN owner TO default_owner;")
  
  # Redo timeseries_metadata_en and fr views because of the cascade
  message("Re-creating views...")
  DBI::dbExecute(con, '
  CREATE OR REPLACE VIEW continuous.timeseries_metadata_en
  WITH(security_invoker=true)
  AS SELECT ts.timeseries_id,
  mtypes.media_type,
  params.param_name AS parameter_name,
  params.unit_default AS default_units,
  params.unit_solid AS units_solid_medium,
  ts.period_type,
  ts.record_rate AS recording_rate,
  ts.start_datetime,
  ts.end_datetime,
  ts.note,
  loc.location_id,
  loc.location AS location_code,
  loc.name AS location_name,
  loc.latitude,
  loc.longitude
  FROM timeseries ts
  JOIN locations loc ON ts.location_id = loc.location_id
  LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
  LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
  ORDER BY ts.timeseries_id;
  ')
  DBI::dbExecute(con, "ALTER VIEW timeseries_metadata_en OWNER TO admin;")
  
  DBI::dbExecute(con, '
                 CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
WITH(security_invoker=true)
AS SELECT ts.timeseries_id,
    mtypes.media_type_fr AS "type_de_média",
    params.param_name_fr AS "nom_paramètre",
    params.unit_default AS "unités_par_défaut",
    params.unit_solid AS "unités_media_solide",
    ts.period_type,
    ts.record_rate AS recording_rate,
    ts.start_datetime AS "début",
    ts.end_datetime AS fin,
    ts.note,
    loc.location_id,
    loc.location AS location_code,
    loc.name_fr AS nom_endroit,
    loc.latitude,
    loc.longitude
   FROM timeseries ts
     JOIN locations loc ON ts.location_id = loc.location_id
     LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
     LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
  ORDER BY ts.timeseries_id;
                 ')
  DBI::dbExecute(con, "ALTER VIEW timeseries_metadata_fr OWNER TO admin;")
  
  unique_nm <- DBI::dbGetQuery(con, "SELECT conname
FROM pg_constraint
WHERE conrelid = 'timeseries'::regclass
  AND contype = 'u';")
  
  if (nrow(unique_nm) > 0) {
    DBI::dbExecute(con, paste0("
  ALTER TABLE timeseries
  DROP CONSTRAINT ", unique_nm[1,1], ";
  "))
  }
  
  
  DBI::dbExecute(con, "
  ALTER TABLE timeseries
  ADD CONSTRAINT timeseries_unique UNIQUE NULLS NOT DISTINCT (
    location_id,
    parameter_id, 
    period_type, 
    media_id, 
    record_rate, 
    z, 
    sensor_priority, 
    sub_location_id
  );
  ")
  
  # Delete the 'owner' column from the 'locations' table as it is now redundant. Ownership information can still be held at the timeseries level via table 'owners'
  DBI::dbExecute(con, "ALTER TABLE locations DROP COLUMN owner;")
  
  DBI::dbExecute(con, "UPDATE internal_status SET event = 'last_sync_continuous' WHERE event = 'last_sync'")
  DBI::dbExecute(con, "INSERT INTO internal_status (event) VALUES ('last_sync_discrete')")
  
  
  
  # 5. Create new roles/groups and modify user/group permissions ########
  
  try({
    DBI::dbExecute(con, "REVOKE ALL ON SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ac_editor;")
    DBI::dbExecute(con, "REVOKE ALL ON ALL TABLES IN SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ac_editor;")
    DBI::dbExecute(con, "REVOKE ALL ON ALL SEQUENCES IN SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ac_editor;")
    DBI::dbExecute(con, "REVOKE ALL ON ALL FUNCTIONS IN SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ac_editor;")
    
    # Revoke default privileges for ac_editor
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous REVOKE ALL ON TABLES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete REVOKE ALL ON TABLES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial REVOKE ALL ON TABLES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files REVOKE ALL ON TABLES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments REVOKE ALL ON TABLES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information REVOKE ALL ON TABLES FROM ac_editor;")
    
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON SEQUENCES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous REVOKE ALL ON SEQUENCES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete REVOKE ALL ON SEQUENCES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial REVOKE ALL ON SEQUENCES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files REVOKE ALL ON SEQUENCES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments REVOKE ALL ON SEQUENCES FROM ac_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information REVOKE ALL ON SEQUENCES FROM ac_editor;")
    
    # Reassign ownership
    DBI::dbExecute(con, "REASSIGN OWNED BY ac_editor TO postgres;")
    
    # Drop all privileges
    DBI::dbExecute(con, "DROP OWNED BY ac_editor;")
    
    # Terminate active connections (if necessary)
    DBI::dbExecute(con, "
SELECT pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.usename = 'ac_editor'
  AND pid <> pg_backend_pid();
")
    
    # Drop the role
    DBI::dbExecute(con, "DROP ROLE ac_editor;")
  })
  
  
  
  # Add extra privileges for discrete_editor
  extra_tbls <- c("locations", "sub_locations", "networks", "organizations", "projects", "locations_networks", "locations_projects", "locations_metadata_access", "locations_metadata_infrastructure", "locations_metadata_infrastructure_groundwater", "locations_metadata_infrastructure_hydromet", "locations_metadata_instruments", "locations_metadata_maintenance", "locations_metadata_owners_operators", "locations_metadata_xsections")
  files_tbls <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'files'")[,1]
  # Remove 'images_index' and 'document_types'
  files_tbls <- files_tbls[files_tbls != "images_index" & files_tbls != "document_types"]
  instruments_tbls <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'instruments'")[,1]
  spatial_tbls <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'spatial'")[,1]
  
  # Get all tables in schema 'discrete'
  discrete_schema_tbls <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'discrete'")[,1]
  # Remove 'sample_series'
  discrete_schema_tbls <- discrete_schema_tbls[discrete_schema_tbls != "sample_series"]
  discrete_editor_tbls <- c(extra_tbls, discrete_schema_tbls, instruments_tbls, files_tbls, spatial_tbls)
  
  for (i in discrete_editor_tbls) {
    DBI::dbExecute(con, paste0("GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE ", i, " TO discrete_editor;"))
  }
  
  # Add extra privileges for continuous_editor
  # Get all tables in schema 'continuous'
  continuous_schema_tbls <- DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'continuous'")[,1]
  # Remove 'fetch_settings'
  continuous_schema_tbls <- continuous_schema_tbls[continuous_schema_tbls != "fetch_settings"]
  continuous_editor_tbls <- c(extra_tbls, continuous_schema_tbls, instruments_tbls, files_tbls, spatial_tbls)
  for (i in continuous_editor_tbls) {
    DBI::dbExecute(con, paste0("GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE ", i, " TO continuous_editor;"))
  }
  
  # Grant privileges on sequences as well
  discrete_schema_seqs <- DBI::dbGetQuery(con, "
  SELECT sequence_name
  FROM information_schema.sequences
  WHERE sequence_schema = 'discrete';
")[,1]
  continuous_schema_seqs <- DBI::dbGetQuery(con, "
  SELECT sequence_name
  FROM information_schema.sequences
  WHERE sequence_schema = 'continuous';
")[,1]
  public_schema_seqs <- DBI::dbGetQuery(con, "
  SELECT sequence_name
  FROM information_schema.sequences
  WHERE sequence_schema = 'public';
")[,1]
  instrument_schema_seqs <- DBI::dbGetQuery(con, "
  SELECT sequence_name
  FROM information_schema.sequences
  WHERE sequence_schema = 'instruments';
")[,1]
  spatial_schema_seqs <- DBI::dbGetQuery(con, "
  SELECT sequence_name
  FROM information_schema.sequences
  WHERE sequence_schema = 'spatial';
")[,1]
  files_schema_seqs <- DBI::dbGetQuery(con, "
  SELECT sequence_name
  FROM information_schema.sequences
  WHERE sequence_schema = 'files';
")[,1]
  
  sequences_cont <- c(continuous_schema_seqs, public_schema_seqs, instrument_schema_seqs, files_schema_seqs)
  sequences_disc <- c(discrete_schema_seqs, public_schema_seqs, instrument_schema_seqs, files_schema_seqs)
  for (seq in sequences_cont) {
    DBI::dbExecute(con, paste0("GRANT USAGE, SELECT, UPDATE ON SEQUENCE ", seq, " TO continuous_editor;"))
  }
  for (seq in sequences_disc) {
    DBI::dbExecute(con, paste0("GRANT USAGE, SELECT, UPDATE ON SEQUENCE ", seq, " TO discrete_editor;"))
  }
  
  
  # Modify role 'yg' to be called 'yg_reader' and give it BYPASSRLS
  try({
    DBI::dbExecute(con, "ALTER ROLE yg RENAME TO yg_reader;")
    # Change references to 'yg' in table 'timeseries' and 'locations' to 'yg_reader'
    DBI::dbExecute(con, "UPDATE timeseries SET share_with = '{yg_reader}' WHERE share_with = '{yg}';")
    DBI::dbExecute(con, "UPDATE locations SET share_with = '{yg_reader}' WHERE share_with = '{yg}';")
    
  })
  
  # Check if 'yg_reader' exists before proceeding:
  if (DBI::dbGetQuery(con, "SELECT 1 FROM pg_roles WHERE rolname = 'yg_reader';")[1,1] == 1) {
    DBI::dbExecute(con, "ALTER ROLE yg_reader WITH BYPASSRLS;")
    
    # it should see all tables, functions, view, etc in all schemas but not edit them
    DBI::dbExecute(con, "
                 DO $$
DECLARE
    tbl RECORD;
BEGIN
    FOR tbl IN
        SELECT schemaname, tablename
        FROM pg_tables
        WHERE schemaname NOT IN ('pg_catalog', 'information_schema') -- Exclude system schemas
    LOOP
        EXECUTE format('GRANT SELECT ON TABLE %I.%I TO yg_reader;', tbl.schemaname, tbl.tablename);
    END LOOP;
END $$;
")
    
    DBI::dbExecute(con, "
DO $$
DECLARE
    vw RECORD;
BEGIN
    FOR vw IN
        SELECT schemaname, viewname
        FROM pg_views
        WHERE schemaname NOT IN ('pg_catalog', 'information_schema') -- Exclude system schemas
    LOOP
        EXECUTE format('GRANT SELECT ON TABLE %I.%I TO yg_reader;', vw.schemaname, vw.viewname);
    END LOOP;
END $$;
")
    
    DBI::dbExecute(con, "
               DO $$
DECLARE
    seq RECORD;
BEGIN
    FOR seq IN
        SELECT schemaname, sequencename
        FROM pg_sequences
        WHERE schemaname NOT IN ('pg_catalog', 'information_schema') -- Exclude system schemas
    LOOP
        EXECUTE format('GRANT SELECT, USAGE ON SEQUENCE %I.%I TO yg_reader;', seq.schemaname, seq.sequencename);
    END LOOP;
END $$;
")
  }
  
  
  # Clean up a few old functions...
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION continuous.check_owners_overlap()
  RETURNS trigger
  LANGUAGE plpgsql
  AS $function$
    BEGIN
  IF EXISTS (
    SELECT 1
    FROM owners
    WHERE timeseries_id = NEW.timeseries_id
    AND organization_id != NEW.organization_id  -- Exclude the current row in an UPDATE
    AND (
      (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
    )
  ) THEN
  RAISE EXCEPTION 'Owners cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
  END IF;
  RETURN NEW;
  END;
  $function$
    ;
    ")
  
  DBI::dbExecute(con, "
  ALTER FUNCTION continuous.check_owners_overlap() OWNER TO postgres;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_owners_overlap() TO public;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_owners_overlap() TO postgres;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_owners_overlap() TO admin;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_owners_overlap() TO yg_reader;
  ")
  
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION continuous.check_contributors_overlap()
  RETURNS trigger
  LANGUAGE plpgsql
  AS $function$
    BEGIN
  IF EXISTS (
    SELECT 1
    FROM contributors
    WHERE timeseries_id = NEW.timeseries_id
    AND organization_id != NEW.organization_id  -- Exclude the current row in an UPDATE
    AND (
      (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
    )
  ) THEN
  RAISE EXCEPTION 'Contributors cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
  END IF;
  RETURN NEW;
  END;
  $function$
    ;
    ")
  
  DBI::dbExecute(con, "
  ALTER FUNCTION continuous.check_contributors_overlap() OWNER TO postgres;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_contributors_overlap() TO public;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_contributors_overlap() TO postgres;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_contributors_overlap() TO admin;
  ")
  DBI::dbExecute(con, "
  GRANT ALL ON FUNCTION continuous.check_contributors_overlap() TO yg_reader;
  ")
  
  #  Wrap up ###########
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '11' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  
  message("Success! Patch 11 applied successfully: discrete data tables have been modified for better organization and location + sub-location metadata storage was improved.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 11 failed and the DB has been rolled back to its earlier state. ", e$message)
})

message("Performing a vacuum (analyze) on the database. Please be patient.")
DBI::dbExecute(con, "VACUUM ANALYZE;")
