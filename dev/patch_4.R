# Patch 4.
# Creates new schema to hold Shiny app data along with a table specific to app ygwater.

# Initial checks #################
# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")

if (!check$can_create) {
  stop("You do not have the necessary privileges to create a new schema in this database.")
}

message("Working on Patch 4. This update should be quite fast and consists of a new schema and table for storing Shiny app data. \n  Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  DBI::dbExecute(con, "CREATE SCHEMA web;")
  
  DBI::dbExecute(con, "
    CREATE TABLE web.url_states_ygwater (
        token TEXT PRIMARY KEY,             -- Unconstrained length for maximum flexibility
        state_data JSONB NOT NULL,          -- JSONB to store serialized state data
        ip_address INET,                    -- IP address of the user
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),  -- Auto-populate timestamp
        permanent BOOLEAN DEFAULT FALSE     -- Whether the state is permanent or temporary (can be deleted automatically)
    );
  ")
  
  DBI::dbExecute(con, "CREATE INDEX idx_url_states_ygwater_created_at ON web.url_states_ygwater (created_at);")
  
  # Create a function and trigger to delete old records if the table exceeds a certain threshold
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION web.cleanup_if_url_states_ygwater_exceeds_threshold() RETURNS VOID AS $$
DECLARE
    record_count INT;
BEGIN
    -- Get the current number of rows in the table
    SELECT COUNT(*) INTO record_count FROM web.url_states_ygwater;
    
    -- Check if the row count exceeds 100
    IF record_count > 100 THEN
        -- Delete records older than 30 days that are not marked as permanent
        DELETE FROM web.url_states_ygwater
        WHERE created_at < NOW() - INTERVAL '30 days'
          AND permanent = FALSE;
    END IF;
END;
$$ LANGUAGE plpgsql;
  ")
  
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION web.insert_trigger_cleanup_url_states_ygwater() RETURNS TRIGGER AS $$
BEGIN
    -- Call the cleanup function after each insert
    PERFORM web.cleanup_if_url_states_ygwater_exceeds_threshold();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")

DBI::dbExecute(con, "
-- Attach the trigger to the web.url_states_ygwater table
CREATE TRIGGER trigger_insert_cleanup_url_states_ygwater
AFTER INSERT ON web.url_states_ygwater
FOR EACH ROW
EXECUTE FUNCTION web.insert_trigger_cleanup_url_states_ygwater();
  ")

# Grant SELECT, INSERT, UPDATE, DELETE on the table to the ac_editor, hydromet_read, snow_read roles.
DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON web.url_states_ygwater TO ac_editor;")
DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON web.url_states_ygwater TO hydromet_read;")
DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON web.url_states_ygwater TO snow_read;")

  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '4' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  message("Patch 4 applied successfully: schema 'web' and table 'web.url_states_ygwater' created.")
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 4 failed and the DB has been rolled back to its earlier state. ", e$message)
})
