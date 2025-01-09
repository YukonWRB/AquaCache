# Patch 10
# Evolution of discrete data storage and of location metadata storage

# Initial checks #################
# Ensure the user is postgres Oas this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user == "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work.")
}


message("Working on Patch 11. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE

tryCatch({
  
  # Move tables to new schemas to clean things up
  DBI::dbExecute(con, "CREATE SCHEMA continuous;")
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
  
  
  DBI::dbExecute(con, "ALTER TABLE owners_contributors RENAME TO owners_contributors_operators;")
  
  
  DBI::dbExecute(con, "CREATE SCHEMA files;")
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
  DBI::dbExecute(con, "ALTER FUNCTION check_sample_class_exists SET SCHEMA discrete;")
  
  
  DBI::dbExecute(con, "DROP FUNCTION prevent_delete_public_group")
  DBI::dbExecute(con, "DROP FUNCTION remove_group_id_from_share_with")
  DBI::dbExecute(con, "DROP FUNCTION remove_group_id_from_users")
  DBI::dbExecute(con, "DROP FUNCTION validate_user_groups")
  DBI::dbExecute(con, "DROP FUNCTION update_modify_datetime")
  
  
  # Update the search path (takes effect at next connection)
  DBI::dbExecute(con, "ALTER DATABASE aquacache SET search_path TO public, continuous, discrete, spatial, files, instruments, information;")
  # Update the search path for this session
  DBI::dbExecute(con, "SET search_path TO public, continuous, discrete, spatial, files, instruments, information;")
  
  # 1. Make modifications for location metadata storage #################
  # General plan: 
  # Create a new table to hold sub-locations. 
  # Modify timeseries table to include sub locations in unique key. 
  # Create a table to hold instrument information that is unique on location AND sub-locations.
  # No modification to instrument table for now, but it'll need a look to make sure it is flexible enough for all types of instruments.
  
  ## 1.1 Create new table 'sub_locations' #################
  DBI::dbExecute(con, "
  CREATE TABLE sub_locations (
    sub_location_id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    sub_location_name TEXT NOT NULL,
    sub_location_name_fr TEXT,
    share_with TEXT[], -- referential integrity to database users enforced via function/trigger
    UNIQUE (location_id, sub_location_name)
  );
  ")
  
  # Create trigger for existing function validate_share_with to ensure that all users in share_with column exist in the database
  DBI::dbExecute(con, "
  CREATE TRIGGER validate_share_with_trigger
  BEFORE INSERT OR UPDATE ON sub_locations
  FOR EACH ROW
  EXECUTE FUNCTION public.validate_share_with();
  ")
  
  ## 1.2 Modify timeseries table to allow uniqueness on sub_locations #################
  DBI::dbExecute(con, "
  ALTER TABLE timeseries 
  ADD COLUMN sub_location_id INTEGER REFERENCES sub_locations(sub_location_id); -- can be null
  ")
  
 # Unique key on timeseries table is adjusted lower down!
  
  
  # Create table 'locations_metadata_instruments'
  DBI::dbExecute(con, "
  CREATE TABLE locations_metadata_instruments (
    metadata_id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    sub_location_id INTEGER REFERENCES sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    instruments INTEGER[],  --array, nullable to allow for a break in usage of a location. Reference to 'instruments' table must be done via a function/trigger because FKs are not supported for arrays.
    start_dt TIMESTAMPTZ NOT NULL,
    end_dt TIMESTAMPTZ,
    created TIMESTAMPTZ DEFAULT now() NOT NULL,
    modified TIMESTAMPTZ,
    UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id)  -- the not distinct part means that you can't have two null sub_location_ids
);
")
  
  # Trigger to update the modified timestamp
  DBI::dbExecute(con, "
  create trigger update_locations_metadata_instruments_modified before
  update
  on
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
  
  # Delete the 'owner' column from the 'locations' table as it is now redundant. Ownership information can still be held at the timeseries level via table 'owners'
  DBI::dbExecute(con, "ALTER TABLE locations DROP COLUMN owner;")
  
  
  
  
  
  
  
  # 2. Make modifications for discrete data storage #################
  # Split 'timeseries' table in two: 
  # Anything labelled as 'continuous' in 'timeseries_type' stays in 'timeseries'
  # Anything labelled as 'discrete' goes to a new table called 'sample_series'. This is then linked to table 'samples'. Each sampling event gets an entry in this table, and the measured values go in table 'measurements_discrete' (which could be renamed to 'results' instead for clarity - and for that matter 'measurements_continuous' could be renamed 'measurements').
  # The idea here is that 'sample_series' holds metadata about the location's sampling history, and most importantly facilitates the use of import functions for a specific location, sub-location, media, depth, etc. The downside is that this adds a layer that isn't present if we can directly link a location to a sample and to a specific result
    
    # Create 'sample_series'
  DBI::dbExecute(con, "
                 CREATE TABLE sample_series (
                      sample_series_id SERIAL PRIMARY KEY,
                      timeseries_id INTEGER, --Removed after this table is linked to measurements_discrete
                      location TEXT NOT NULL REFERENCES locations(location),
                      location_id INTEGER NOT NULL REFERENCES locations(location_id),
                      sub_location_id INTEGER REFERENCES sub_locations(sub_location_id),
                      parameter_id INTEGER NOT NULL REFERENCES parameters(parameter_id),
                      media_id INTEGER REFERENCES media_types(media_id),
                      z NUMERIC, --(to consider if z should actually be held in 'samples' table)
                      start_datetime TIMESTAMPTZ ,
                      end_datetime TIMESTAMPTZ,
                      last_new_data TIMESTAMPTZ,
                      last_synchronize TIMESTAMPTZ,
                      source_fx TEXT,
                      source_fx_args TEXT,
                      note TEXT,
                      active BOOLEAN NOT NULL DEFAULT TRUE
                  );
                ")
  
  DBI::dbExecute(con, "
  ALTER TABLE sample_series
  ADD CONSTRAINT sample_series_unique UNIQUE NULLS NOT DISTINCT (
    location_id,
    sub_location_id,
    parameter_id,
    media_id,
    z
  );
  ")
  
  DBI::dbExecute(con, "COMMENT ON TABLE timeseries IS 
                 'Provides a record of every continuous-type timeseries in the database. Each timeseries is unique by its combination of location, sub_location, parameter, media, period_type, record_rate, sensor priority, and z (elevation). Continuous data is data gathered at regular and usually frequent intervals by automatic means. Refer to table sample_series for lab and manual measurement.'
                 ")
  
  DBI::dbExecute(con, "COMMENT ON TABLE sample_series IS
                 'Provides a record of every discrete sample in the database. Each sample series is unique by its combination of location, sub_location, parameter, period_type, record_rate, and z (elevation). Discrete data includes infrequent, often manual measurements of values such as snow depth or dissolved element parameters. For more continuously gathered data, usually acquired by automatic means, refer to table timeseries.'
                 ")
  
  # Pull from timeseries WHERE category  = 'discrete'. Drop irrelevant columns: period_type, category, last_daily_calculation, record_rate, sensor_priority
  sample_series <- DBI::dbGetQuery(con, "SELECT timeseries_id, location, location_id, sub_location_id, parameter_id, media_id, z, start_datetime, end_datetime, last_new_data, last_synchronize, source_fx, source_fx_args, note, active FROM timeseries WHERE category = 'discrete'")
  
  # Add discrete data to new table sample_series
  DBI::dbAppendTable(con, "sample_series", sample_series)
  
  # Pull sample_series data to get the match between sample_series_id and timeseries_id
  sample_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series")
  
  # Remove any entries labelled 'discrete' from table 'timeseries', then drop column 'category'. Requires remove foreign key to 'timeseries_id' from measurements_discrete table.
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete DROP CONSTRAINT fk_timeseries_id")
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete DROP CONSTRAINT measurements_discrete_result_condition_fkey1")  # Somehow this duplicate fk got in there
  
  # Modify entries in measurements_discrete to use sample_series_id instead of timeseries_id
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD COLUMN sample_series_id INTEGER REFERENCES sample_series(sample_series_id);")
  for (i in 1:nrow(sample_series)) {
    DBI::dbExecute(con, paste0("UPDATE measurements_discrete SET sample_series_id = ", sample_series$sample_series_id[i], " WHERE timeseries_id = ", sample_series$timeseries_id[i], ";"))
  }
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete DROP COLUMN timeseries_id;")
  # Make a not null constraint on sample_series_id
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete ALTER COLUMN sample_series_id SET NOT NULL")
  
  
  DBI::dbExecute(con, "DELETE FROM timeseries WHERE category = 'discrete'")
  DBI::dbExecute(con, "ALTER TABLE timeseries DROP COLUMN category")
  
  DBI::dbExecute(con, "
  ALTER TABLE timeseries
  DROP CONSTRAINT timeseries_unique;
  ")
  
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
  

    # Another table called 'samples' gets additional data about each distinct sample:
  DBI::dbExecute(con, "CREATE TABLE samples (
    sample_id SERIAL PRIMARY KEY,
    sample_series_id INTEGER REFERENCES sample_series(sample_series_id),
    replicate_status,
    linked INTEGER REFERENCES samples(sample_id), --(to associate samples - should be not null if replicate status indicates)
    flow_m3s NUMERIC,
    wx TEXT,
    sample_equipment 
    sample_volume NUMERIC,
    notes TEXT
  );
    ")
      
      
      # 'measurements_discrete' now gets linked to table 'samples', on column 'sample_id'
      # Still keeps columns identifying the lab and protocol, which allows for a single sample that gets split between labs or within a lab to different protocols/methods.
      # Question remains... what about lab replicates? These aren't distinct samples until they're split by the lab. Is there another similar use case? Do we just add a column for 'lab_dup' and add this to the unique key?
      # 'measurements_discrete' will require column for 'parameter' as this was previously held in 'timeseries'. Dissolved, total, speciation, etc should already be taken care of.
      
      # Q: do we need a table for 'replicate status'? This would apply to table 'samples' as well as to 'measurements_discrete'/'results' table
      
      
      # Relationships between 'samples' table and other tables need to be investigated. For example:
      # grades
      # approvals
      # qualifiers
      # owner (reminder that this column in 'locations' needs to be removed)
      # contributor
      # corrections
      # ???
      
      # It may be preferable to hold these attributes for 'discrete' data in entirely new tables, using the same basic functions to automatically apply the attributes. Otherwise a new column will be necessary in these tables as we no longer have a consistent 'timeseries_id'.
      
      
      
      # Consider also adding tables for station and parameter groups. Concept:
      'parameter_groups'
    param_group_id
    param_group_name
    param_group_description
    parameters []
    
    


# Update the version_info table
DBI::dbExecute(con, "UPDATE information.version_info SET version = '10' WHERE item = 'Last patch number';")
DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))

# Commit the transaction
DBI::dbExecute(con, "COMMIT;")
attr(con, "active_transaction") <- FALSE


message("Patch 10 applied successfully: discrete data tables have been modified for better organization and location + sub-location metadata storage was improved.")

}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 10 failed and the DB has been rolled back to its earlier state. ", e$message)
})
