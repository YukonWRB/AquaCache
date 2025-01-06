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
  
  DBI::dbExecute(con, "CREATE SCHEMA modifiers;")
  
  
  DBI::dbExecute(con, "CREATE SCHEMA metadata;")
  DBI::dbExecute(con, "RENAME SCHEMA measurements_discrete TO discrete;")
  
  
  
  DBI::dbExecute(con, "ALTER DATABASE aquacache SET search_path TO public, metadata, modifiers, continuous, discrete, ;")
  
    
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
    share_with TEXT[] -- referential integrity to database users enforced via function/trigger
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
  
  
  
  ## 1.2Modify timeseries table to allow uniqueness on sub_locations #################
  DBI::dbExecute(con, "
  ALTER TABLE timeseries 
  ADD COLUMN sub_location INTEGER REFERENCES sub_locations(sub_location_id); -- can be null
  ")
  
  DBI::dbExecute(con, "
  ALTER TABLE timeseries
  DROP CONSTRAINT timeseries_unique;
  ")
  
  DBI::dbExecute(con, "
  ALTER TABLE timeseries
  ADD CONSTRAINT timeseries_unique UNIQUE NULLS NOT DISTINCT (
    location, 
    parameter_id, 
    category, 
    period_type, 
    media_id, 
    record_rate, 
    z, 
    sensor_priority, 
    sub_location_id
  );
  ")
  
  
  # Create table 'locations_metadata_instruments'
  
  CREATE TABLE location_instrument_metadata (
    metadata_id SERIAL PRIMARY KEY,
    location_id INT NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    sub_location_id INT REFERENCES sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE,
    instruments INTEGER[],  --array, nullable to allow for a break in usage of a location. Reference to 'instruments' table must be done via a function/trigger because FKs are not supported for arrays.
    start_dt TIMESTAMPTZ NOT NULL,
    end_dt TIMESTAMPTZ,
    created TIMESTAMPTZ DEFAULT now() NOT NULL,
    modified TIMESTAMPTZ,
    UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id)  -- the not distinct part means that you can't have two null sub_location_ids with the same location. You could have a location with a null sub_location AND the same location 								 with a not null sub location though.
);



# Then, dispense with existing tables as their data is taken care of by locations_metadata_instruments'
    
    DROP TABLE locations_metadata_acquisition CASCADE;
    
    DROP TABLE locations_metadata_transmission CASCADE;
    
    
    
    # Modify existing tables 'locations_metadata_infrastructure', locations_metadata_infrastructure_groundwater, locations_metadata_infrastructure_hydromet, locations_metadata_maintenance, locations_metadata_owners_operators to also (optionally) use sub_location_id, with the same nulls not distinct unique key.
    
    # Use of the unique nulls not distinct key makes it possible to hold information for the location writ large AND location/sublocation combinations in the same tables.
    
    
    # 2. Make modifications for discrete data storage #################
    # Split 'timeseries' table in two: 
    # Anything labelled as 'continuous' in 'timeseries_type' stays in 'timeseries'
    # Anything labelled as 'discrete' goes to a new table called 'sample_series'. This is then linked to table 'samples'. Each sampling event gets an entry in this table, and the measured values go in table 'measurements_discrete' (which could be renamed to 'results' instead for clarity - and for that matter 'measurements_continuous' could be renamed 'measurements').
    # The idea here is that 'sample_series' holds metadata about the location's sampling history, and most importantly facilitates the use of import functions for a specific location, sub-location, media, depth, etc. The downside is that this adds a layer that isn't present if we can directly link a location to a sample and to a specific result.
    # Another option is to merge sample_series and samples, and to somehow record the 'synch' functions applicable for each location, sub location, media, etc combination differently. We do however still end up with this extra table!
    
    
    # Table 'sample_series' looks like this:
    sample_series_id
    location
    sub_location
    media
    z (to consider if z should actually be held in 'samples' table)
    start_dt
    end_dt
    last_new_data
    last_synchronize
    source_fx
    source_fx_args
    
    # Another table called 'samples' gets additional data about each distinct sample:
    sample_id
    sample_series_id
    replicate_status
    linked_to (to associate samples - should be not null if replicate status indicates)
    flow
    wx
    parameters (optional array column listing the parameters, might be useful for 'synchronize')
    sample_equipment
    sample_volume
    notes
    ???
      
      
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
