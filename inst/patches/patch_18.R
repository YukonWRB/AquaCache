# Patch 18

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 18. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

message("This patch adds a default category to images in table 'images' so that they can be more easily identified later.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  DBI::dbExecute(con, "UPDATE image_types SET image_type = 'Ice observation' WHERE image_type = 'Ice observation flight'")
  
  # Get the image_type for 'auto'
  auto_type <- DBI::dbGetQuery(con, "SELECT image_type_id FROM image_types WHERE image_type = 'Auto';")[1,1]
  ice_type <- DBI::dbGetQuery(con, "SELECT image_type_id FROM image_types WHERE image_type = 'Ice observation';")[1,1]
  
  # Update column image_type to be 'image_type' for all images in the images table where where column img_meta_id is NOT NULL
  DBI::dbExecute(con, paste0("UPDATE images SET image_type = ", auto_type, " WHERE img_meta_id IS NOT NULL;"))
  DBI::dbExecute(con, paste0("UPDATE images SET image_type = ", ice_type, " WHERE img_meta_id IS NULL;"))
  
  # Make column image_type_id NOT NULL
  DBI::dbExecute(con, "ALTER TABLE images ALTER COLUMN image_type SET NOT NULL;")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '18' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  message("Patch 18 applied successfully.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 18 failed and the DB has been rolled back to its earlier state. ", e$message)
})
