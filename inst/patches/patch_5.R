# Patch 5.
# Creates new table 'network_types' ....

# Initial checks #################
# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")

if (!check$can_create) {
  stop("You do not have the necessary privileges to create a new schema in this database.")
}

message("Working on Patch 5. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Create the network_types table and populate it
 DBI::dbExecute(con, "CREATE TABLE network_types (
  network_type_id SERIAL PRIMARY KEY,
  network_type_name TEXT NOT NULL,
  network_type_name_fr TEXT,
  network_type_description TEXT,
  network_type_description_fr TEXT)
  ;")
  
  tbl <- data.frame(network_type_name = c("monitoring", "research"),
                    network_type_name_fr = c("surveillance", "recherche"),
                    network_type_description = c("A network for monitoring purposes of background or human-influenced conditions.", "A network for research purposes, including experimental or observational studies."),
                    network_type_description_fr = c("Un réseau à des fins de surveillance des conditions de fond ou influencées par l'humain", "Un réseau à des fins de recherche, y compris des études expérimentales ou observationnelles."))
  
  DBI::dbAppendTable(con, "network_types", tbl)
  
  # modify 'networks' table. Column 'type' needs to become numeric and reference the new table. All fields in there right now are 'monitoring' so that should be easy.
  DBI::dbExecute(con, "ALTER TABLE networks DROP COLUMN type;")
  DBI::dbExecute(con, "ALTER TABLE networks ADD COLUMN type INTEGER REFERENCES network_types(network_type_id);")
  DBI::dbExecute(con, "UPDATE networks SET type = 1;")
  # Make not null
  DBI::dbExecute(con, "ALTER TABLE networks ALTER COLUMN type SET NOT NULL;")

  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '5' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))

  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  
  message("Patch 5 applied successfully: created new table for network types and added foreign keys from 'networks' to this new table.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 5 failed and the DB has been rolled back to its earlier state. ", e$message)
})
