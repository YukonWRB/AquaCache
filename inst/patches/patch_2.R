# Patch 2.
# Fixes an improper function reference in a triger that updates the modify_datetime field in the database for tables in the instruments schema.

# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")

if (!check$can_create) {
  stop("You do not have the necessary privileges to create a new schema in this database.")
}


# Begin a transaction
message("Applying patch 2.")
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # List the tables to update
  tables_to_update <- c('instrument_type', 'instrument_make', 'instrument_model', 'observers', 'instruments', 'sensor_types', 'sensors', 'instrument_maintenance', 'array_maintenance_changes', 'calibrations', 'calibrate_temperature', 'calibrate_specific_conductance', 'calibrate_pH', 'calibrate_ORP', 'calibrate_turbidity', 'calibrate_dissolved_oxygen', 'calibrate_depth')
  
  # Remove instruments.instrument_deployment table because this is captured in the location metadata tables
  DBI::dbExecute(con, "DROP TABLE IF EXISTS instruments.instrument_deployment;")
  
  # Check existence of function instruments.update_modify_datetime
  check <- DBI::dbGetQuery(con, "
  SELECT proname 
  FROM pg_proc 
  JOIN pg_namespace ON pg_proc.pronamespace = pg_namespace.oid 
  WHERE proname = 'update_modify_datetime' 
    AND nspname = 'instruments';
")
  
  if (nrow(check) == 0) {
    # Create the function anew
    DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION instruments.update_modify_datetime()
    RETURNS TRIGGER AS $$
    BEGIN
      NEW.modify_datetime = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
  ")
  }
  
  # drop the problematic triggers and recreate them
  for (table in tables_to_update) {
    #Delete the trigger if it exists
    DBI::dbExecute(con, paste0("
      DROP TRIGGER IF EXISTS update_", table, "_modify_datetime ON instruments.", table, ";"))
    
    DBI::dbExecute(con, paste0("
      CREATE TRIGGER update_", table, "_modify_datetime
      BEFORE UPDATE ON instruments.", table, "
      FOR EACH ROW
      EXECUTE FUNCTION instruments.update_modify_datetime();
    "))
  }
  
  # If that worked now update the version number in the database
  DBI::dbExecute(con, "UPDATE information.version_info SET version = 2 WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  # Give user a message
  message("Patch 2 applied successfully: dropped table instruments.instrument_deployment and fixed trigger functions for instrument-realted tables.")
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 2 failed and the DB has been rolled back to its earlier state. ", e$message)
})

