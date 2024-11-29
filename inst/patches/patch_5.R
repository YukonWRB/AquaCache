# Patch 5.
# Creates new table 'network_types' ....

# Initial checks #################
# No special checks needed as this patch can work with either 'admin' or 'postgres' user

message("Working on Patch 5. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Create the network_project_types table and populate it
  DBI::dbExecute(con, "CREATE TABLE network_project_types (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  name_fr TEXT,
  description TEXT,
  description_fr TEXT)
  ;")
  
  tbl <- data.frame(name = c("monitoring", "research"),
                    name_fr = c("surveillance", "recherche"),
                    description = c("A network/project for monitoring purposes of background or human-influenced conditions.", "A network/project for research purposes, including experimental or observational studies."),
                    description_fr = c("Un r\u00E9seau/projet \u00E0 des fins de surveillance des conditions de fond ou influenc\u00E9es par l'humain", "Un r\u00E9seau/projet \u00E0 des fins de recherche, y compris des \u00E9tudes exp\u00E9rimentales ou observationnelles."))
  
  DBI::dbAppendTable(con, "network_project_types", tbl)
  
  # modify 'networks' table. Column 'type' needs to become numeric and reference the new table. All fields in there right now are 'monitoring' so that should be easy.
  DBI::dbExecute(con, "ALTER TABLE networks DROP COLUMN type;")
  DBI::dbExecute(con, "ALTER TABLE networks ADD COLUMN type INTEGER REFERENCES network_project_types(id);")
  DBI::dbExecute(con, "UPDATE networks SET type = 1;")
  # Make not null
  DBI::dbExecute(con, "ALTER TABLE networks ALTER COLUMN type SET NOT NULL;")
  
  # Do the same for 'projects' table.
  DBI::dbExecute(con, "ALTER TABLE projects DROP COLUMN type;")
  DBI::dbExecute(con, "ALTER TABLE projects ADD COLUMN type INTEGER REFERENCES network_project_types(id);")
  DBI::dbExecute(con, "UPDATE projects SET type = 1;")
  # Make not null
  DBI::dbExecute(con, "ALTER TABLE projects ALTER COLUMN type SET NOT NULL;")
  
  
  # Grant privileges: SELECT, INSERT, UPDATE, DELETE to ac_editor, SELECT to hydromet_read
  DBI::dbExecute(con, "GRANT SELECT, INSERT, UPDATE, DELETE ON network_project_types TO ac_editor;")
  DBI::dbExecute(con, "GRANT SELECT ON network_project_types TO hydromet_read;")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '5' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Create new users so that postgres stops being used for everything. Read-only user is hydromet_read, change that to ac_reader.
  # Admin user (can do everything except for creating new databases)
  
  # Give the user a prompt to enter the password using readline
  message("Creating new users so that postgres stops being used. ENTER YOUR DESIRED PASSWORD BELOW.")
  password <- readline("Password for the admin user: ")
  
  
  DBI::dbExecute(con, paste0("CREATE ROLE admin WITH LOGIN PASSWORD '", password, "';"))
  DBI::dbExecute(con, "GRANT CONNECT ON DATABASE aquacache TO admin;")
  

  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  
  message("Patch 5 applied successfully: created new table for network types and added foreign keys from 'networks' and 'projects' to this new table.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 5 failed and the DB has been rolled back to its earlier state. ", e$message)
})
