# Patch 13

# Initial checks #################
# # Ensure the user is postgres OR admin as this patch requires it
# check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
# 
# if (!check$session_user %in% c("postgres", "admin")) {
#   stop("You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work.")
# }
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 13. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

message("This patch modifies the 'images' and 'documents' tables to add a 'tags' column (text array, indexed), fills in columns not already populated in the 'images' table and makes then NOT NULL, and modifies the 'locations' table to add columns to flag locations as having anthropogenic influence and as 'sentinel' locations for climate change analyses and reports.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Add columns to locations table to facilitate climate change analyses and reports
  DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN anthropogenic_influence BOOLEAN DEFAULT FALSE;")
  # Comment on the column
  DBI::dbExecute(con, "COMMENT ON COLUMN locations.anthropogenic_influence IS 'Flag to indicate if the location has anthropogenic influence';")
  DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN sentinel_location BOOLEAN DEFAULT FALSE;")
  # Comment on the column
  DBI::dbExecute(con, "COMMENT ON COLUMN locations.sentinel_location IS 'Flag to indicate if the location is a sentinel location for climate change analyses and reports';")
  
  # Add columns to the 'images' table and modify the existing NULL/NOT NULL columns so that more details are stored
  DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN tags TEXT[];")
  DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN location_id INTEGER REFERENCES locations(location_id)")
  # Make a GIN index on the tags column
  DBI::dbExecute(con, "CREATE INDEX idx_images_tags ON images USING GIN(tags);")
  
  # Rename columns images.altitude_asl to images.elevation_msl and images.altitude_agl to images.elevation_agl
  DBI::dbExecute(con, "ALTER TABLE images RENAME COLUMN altitude_asl_m TO elevation_msl_m;")
  DBI::dbExecute(con, "ALTER TABLE images RENAME COLUMN altitude_agl_m TO elevation_agl_m;")
  # Add comments to both newly named columns
  DBI::dbExecute(con, "COMMENT ON COLUMN images.elevation_msl_m IS 'Elevation in meters above mean sea level';")
  DBI::dbExecute(con, "COMMENT ON COLUMN images.elevation_agl_m IS 'Elevation in meters above ground level';")
  
  # Drop trigger trigger_enforce_lat_lon_constraints and function enforce_lat_lon_constraints
  DBI::dbExecute(con, "DROP TRIGGER trigger_enforce_lat_lon_constraints ON images;")
  DBI::dbExecute(con, "DROP FUNCTION enforce_lat_lon_constraints();")
  
  # Many entries don't have a location_id, latitude, and longitude associated. We'll make these columns NOT NULL so need to populate them first. table images_index has a column for 'location_id' that references locations.location_id, which is where latitude and longitude are stored.
  exists <- DBI::dbGetQuery(con, "SELECT img_meta_id FROM images")
  for (i in unique(exists$img_meta_id)) {
    loc_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM images_index WHERE img_meta_id = ", i, ";"))[1,1]
    lat <- DBI::dbGetQuery(con, paste0("SELECT latitude FROM locations WHERE location_id = ", loc_id, ";"))[1,1]
    long <- DBI::dbGetQuery(con, paste0("SELECT longitude FROM locations WHERE location_id = ", loc_id, ";"))[1,1]
    
    DBI::dbExecute(con, paste0("UPDATE images SET location_id = ", loc_id, ", latitude = ", lat, ", longitude = ", long, " WHERE img_meta_id = ", i, ";"))
    
    elev <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", loc_id, " AND current IS TRUE"))[1,1]
    if (!is.na(elev)) {
      DBI::dbExecute(con, paste0("UPDATE images SET elevation_msl_m = ", elev, " WHERE img_meta_id = ", i, ";"))
    }
    
    # Also set the owner and contributor of the images. If source_fx in images_index IN downloadWSCImages or downloadNupointImages, then both are the WSC
    wsc <- DBI::dbGetQuery(con, "SELECT organization_id FROM organizations WHERE name = 'Water Survey of Canada'")[1,1]
    if (is.na(wsc)) {
      next()
    } else {
      if (DBI::dbGetQuery(con, paste0("SELECT source_fx FROM images_index WHERE img_meta_id = ", i, ";"))[1,1] %in% c("downloadWSCImages", "downloadNupointImages")) {
        DBI::dbExecute(con, paste0("UPDATE images SET owner = ", wsc, ", contributor = ", wsc, " WHERE img_meta_id = ", i, ";"))
      }
    }
  }
    
    # Make 'latitude', 'longitude' NOT NULL
    DBI::dbExecute(con, "ALTER TABLE images ALTER COLUMN latitude SET NOT NULL;")
    DBI::dbExecute(con, "ALTER TABLE images ALTER COLUMN longitude SET NOT NULL;")
    
    # Ensure that img_meta_id CAN be NULL
    DBI::dbExecute(con, "ALTER TABLE images ALTER COLUMN img_meta_id DROP NOT NULL;")
    
    # Find the name of the unique key constraint and delete it
    key <- DBI::dbGetQuery(con, "SELECT constraint_name FROM information_schema.table_constraints WHERE table_name = 'images' AND constraint_type = 'UNIQUE'")[1,1]
    DBI::dbExecute(con, paste0("ALTER TABLE images DROP CONSTRAINT ", key, ";"))

    # Delete any files with a size of less than 10 kb as these are almost certainly empty or corrupted
    DBI::dbExecute(con, "DELETE FROM images WHERE octet_length(file) < 10 * 1024;")
    
    # Add a new unique key constraint on the 'file' column so that no two images can be identical
    DBI::dbExecute(con, "ALTER TABLE images 
  ADD COLUMN file_hash text GENERATED ALWAYS AS (md5(encode(file, 'hex'))) STORED;
  ")
    # Add a comment to this column so its purpose is clear
    DBI::dbExecute(con, "COMMENT ON COLUMN images.file_hash IS 'MD5 hash of the image file, used to ensure that no two images are identical';")
    
    # Find all duplicate images.
    duplicates <- DBI::dbGetQuery(con, "SELECT i.* FROM images i 
                                  JOIN (
                                      SELECT file_hash
                                      FROM images
                                      GROUP BY file_hash
                                      HAVING COUNT(*) > 1
                                    ) d ON i.file_hash = d.file_hash;
")
    # Delete the one(s) with the earliest datetime and keep the one(s) with the latest datetime
    for (i in unique(duplicates$file_hash)) {
      all <- DBI::dbGetQuery(con, paste0("SELECT image_id FROM images WHERE file_hash = '", i, "' ORDER BY datetime ASC;"))
      to_keep <- DBI::dbGetQuery(con, paste0("SELECT image_id FROM images WHERE file_hash = '", i, "' ORDER BY datetime DESC LIMIT 1;"))
      to_delete <- all[-which(all$image_id %in% to_keep$image_id),]
      if (length(to_delete) > 0) {
        DBI::dbExecute(con, paste0("DELETE FROM images WHERE image_id IN (", paste(to_delete, collapse = ", "), ");"))
      }
    }

    DBI::dbExecute(con, "ALTER TABLE images 
  ADD CONSTRAINT unique_image_hash UNIQUE (file_hash);
  ")

    # Rename table 'images_index' to 'image_series', which aligns with how this is done for discrete data
    DBI::dbExecute(con, "ALTER TABLE images_index RENAME TO image_series;")

    
    
    
    # Now also modify documents to have a 'tags' column
    DBI::dbExecute(con, "ALTER TABLE documents ADD COLUMN tags TEXT[];")
    # Make a GIN index on the tags column
    DBI::dbExecute(con, "CREATE INDEX idx_documents_tags ON documents USING GIN(tags);")
    
    # Also add a file_hash column to documents, and a unique constraint on it
    DBI::dbExecute(con, "ALTER TABLE documents
  ADD COLUMN file_hash text GENERATED ALWAYS AS (md5(encode(document, 'hex'))) STORED;
  ")
    # Add a comment to this column so its purpose is clear
    DBI::dbExecute(con, "COMMENT ON COLUMN documents.file_hash IS 'MD5 hash of the document file, used for ensuring that no two documents are identical.';")
    
    # Find all duplicate documents.
    duplicates <- DBI::dbGetQuery(con, "SELECT doc.* FROM documents doc
                                  JOIN (
                                      SELECT file_hash
                                      FROM documents
                                      GROUP BY file_hash
                                      HAVING COUNT(*) > 1
                                    ) d ON doc.file_hash = d.file_hash;
")
    
    # Fix foreign key CASCADES from documents to documents_spatial: should be ON DELETE CASCADE ON UPDATE CASCADE but is not
    DBI::dbExecute(con, "ALTER TABLE documents_spatial DROP CONSTRAINT documents_spatial_document_id_fkey;")
    DBI::dbExecute(con, "ALTER TABLE documents_spatial DROP CONSTRAINT documents_spatial_geom_id_fkey")
    DBI::dbExecute(con, "ALTER TABLE documents_spatial ADD CONSTRAINT documents_spatial_document_id_fkey FOREIGN KEY (document_id) REFERENCES documents(document_id) ON DELETE CASCADE ON UPDATE CASCADE;")
    DBI::dbExecute(con, "ALTER TABLE documents_spatial ADD CONSTRAINT documents_spatial_geom_id_fkey FOREIGN KEY (geom_id) REFERENCES vectors(geom_id) ON DELETE CASCADE ON UPDATE CASCADE;")
    
    # Delete the one(s) with the later publish_date and keep the one(s) with the latest
    for (i in unique(duplicates$file_hash)) {
      all <- DBI::dbGetQuery(con, paste0("SELECT document_id FROM documents WHERE file_hash = '", i, "' ORDER BY publish_date ASC;"))
      to_keep <- DBI::dbGetQuery(con, paste0("SELECT document_id FROM documents WHERE file_hash = '", i, "' ORDER BY publish_date DESC LIMIT 1;"))
      to_delete <- all[-which(all$document_id %in% to_keep$document_id),]
      if (length(to_delete) > 0) {
        DBI::dbExecute(con, paste0("DELETE FROM documents WHERE document_id IN (", paste(to_delete, collapse = ", "), ");"))
      }
    }
    
    DBI::dbExecute(con, "ALTER TABLE documents
  ADD CONSTRAINT unique_document_hash UNIQUE (file_hash);
  ")

    
    # Update the version_info table
    DBI::dbExecute(con, "UPDATE information.version_info SET version = '13' WHERE item = 'Last patch number';")
    DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
    
    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")
    attr(con, "active_transaction") <- FALSE
    
    message("Patch 13 applied successfully.")
    
  }, error = function(e) {
    
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop("Patch 13 failed and the DB has been rolled back to its earlier state. ", e$message)
  })
