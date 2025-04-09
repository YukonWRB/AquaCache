# Patch 14

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

message("Working on Patch 14. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

message("This patch modifies the 'sample_series', 'timeseries', 'raster_series_index', and 'image_series' tables. These tables are used to programmatically fetch data from remote sources, with parameters for the fetch functions taken both from the tables and from a column valled 'source_fx_args'. Now, all fetch parameters are stored in a new JSONB column, giving improved flexibility for fetch operations. This also dispenses with the need for table 'fetch_settings'.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # sample_series ##################
  # fetch the current table
  sample_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series;")
  
  # Delete the contents of column 'source_fx_args'
  DBI::dbExecute(con, "UPDATE sample_series SET source_fx_args = NULL;")
  # Modify the column type to JSONB
  DBI::dbExecute(con, "ALTER TABLE sample_series ALTER COLUMN source_fx_args TYPE JSONB USING source_fx_args::JSONB;")
  
  for (i in 1:nrow(sample_series)) {
    # Get the current row
    row <- sample_series[i, ]
    
    loc_id <- row$location_id
    loc_code <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location_id = ", loc_id, ";"))[1,1]
    old_loc <- sub(".*'([^']+)'.*", "\\1", row$source_fx_args)
    
    if (!is.na(old_loc)) {
      new_args <- data.frame(location = loc_code,
                             old_loc = old_loc)
    } else {
      new_args <- data.frame(location = loc_code)
    }
    
    # Convert the new arguments to JSON
    new_args_json <- jsonlite::toJSON(new_args, auto_unbox = TRUE)
    
    # Update the row in the database
    DBI::dbExecute(con, paste0("UPDATE sample_series SET source_fx_args = '", new_args_json, "' WHERE sample_series_id = ", row$sample_series_id, ";"))
  }
  
  # timeseries ##################
  # fetch the current table
  timeseries <- DBI::dbGetQuery(con, "SELECT * FROM timeseries;")
  # Delete the contents of column 'source_fx_args'
  DBI::dbExecute(con, "UPDATE timeseries SET source_fx_args = NULL;")
  # Modify the column type to JSONB
  DBI::dbExecute(con, "ALTER TABLE timeseries ALTER COLUMN source_fx_args TYPE JSONB USING source_fx_args::JSONB;")
  for (i in 1:nrow(timeseries)) {
    # Get the current row
    row <- timeseries[i, ]
    
    loc_id <- row$location_id
    loc_code <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location_id = ", loc_id, ";"))[1,1]
    
    parameter <- row$parameter_id
    record_rate <- row$record_rate
    extra_args <- row$source_fx_args
    source_fx <- row$source_fx
    period_type <- row$period_type
    
    
    if (is.na(record_rate)) {
      remote_param <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM fetch_settings WHERE parameter_id = ", parameter, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate IS NULL;"))[1,1]
    } else {
      remote_param <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM fetch_settings WHERE parameter_id = ", parameter, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate = '", record_rate, "';"))[1,1]
    }
    
    new_args <- data.frame(location = loc_code,
                           parameter = remote_param)
    
    extra_args <- row$source_fx_args
    if (!is.na(extra_args)) {
      split <- strsplit(extra_args, "=")
      split <- lapply(split, function(x) {
        x <- gsub("[{}]", "", x)
        x <- gsub("\"", "", x)
        x <- gsub("'", "", x)
        return(trimws(x))
      })
      new_args[[split[[1]][1]]] <- split[[1]][2]
    }
    
    # Convert the new arguments to JSONB
    new_args_json <- jsonlite::toJSON(new_args, auto_unbox = TRUE)
    
    # Update the row in the database
    DBI::dbExecute(con, paste0("UPDATE timeseries SET source_fx_args = '", new_args_json, "' WHERE timeseries_id = ", row$timeseries_id, ";"))
  }
  
  
  
  # raster_series_index ##################
  # fetch the current table
  raster_series_index <- DBI::dbGetQuery(con, "SELECT * FROM raster_series_index;")
  # Delete the contents of column 'source_fx_args'
  DBI::dbExecute(con, "UPDATE raster_series_index SET source_fx_args = NULL;")
  # Modify the column type to JSONB
  DBI::dbExecute(con, "ALTER TABLE raster_series_index ALTER COLUMN source_fx_args TYPE JSONB USING source_fx_args::JSONB;")
  
  for (i in 1:nrow(raster_series_index)) {
    # Get the current row
    row <- raster_series_index[i, ]
    
    
    new_args <- data.frame(parameter = row$parameter)
    
    extra_args <- row$source_fx_args
    if (!is.na(extra_args)) {
      split <- strsplit(extra_args, "=")
      split <- lapply(split, function(x) {
        x <- gsub("[{}]", "", x)
        x <- gsub("\"", "", x)
        x <- gsub("'", "", x)
        return(trimws(x))
      })
      new_args[[split[[1]][1]]] <- split[[1]][2]
    }
    
    # Convert the new arguments to JSONB
    new_args_json <- jsonlite::toJSON(new_args, auto_unbox = TRUE)
    
    # Update the row in the database
    DBI::dbExecute(con, paste0("UPDATE raster_series_index SET source_fx_args = '", new_args_json, "' WHERE raster_series_id = ", row$raster_series_id, ";"))
  }
  
  
  
  
  # image_series ##################
  # fetch the current table
  image_series <- DBI::dbGetQuery(con, "SELECT * FROM image_series;")
  # Delete the contents of column 'source_fx_args'
  DBI::dbExecute(con, "UPDATE image_series SET source_fx_args = NULL;")
  # Modify the column type to JSONB
  DBI::dbExecute(con, "ALTER TABLE image_series ALTER COLUMN source_fx_args TYPE JSONB USING source_fx_args::JSONB;")
  
  for (i in i:nrow(image_series)) {
    # Get the current row
    row <- image_series[i, ]
    
    loc_id <- row$location_id
    loc_code <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location_id = ", loc_id, ";"))[1,1]
    
    new_args <- data.frame(location = loc_code)
    
    extra_args <- row$source_fx_args
    if (!is.na(extra_args)) {
      split <- strsplit(extra_args, "=")
      split <- lapply(split, function(x) {
        x <- gsub("[{}]", "", x)
        x <- gsub("\"", "", x)
        x <- gsub("'", "", x)
        return(trimws(x))
      })
      new_args[[split[[1]][1]]] <- split[[1]][2]
    }
    
    # Convert the new arguments to JSONB
    new_args_json <- jsonlite::toJSON(new_args, auto_unbox = TRUE)
    
    # Update the row in the database
    DBI::dbExecute(con, paste0("UPDATE image_series SET source_fx_args = '", new_args_json, "' WHERE img_meta_id = ", row$img_meta_id, ";"))
  }
  
  
  # Add a new table for 'image_types' with default tag options as a character array
  DBI::dbExecute(con, "CREATE TABLE files.image_types (
    image_type_id SERIAL PRIMARY KEY,
    image_type TEXT NOT NULL,
    description TEXT,
    default_tag_options TEXT[]);")
  
  img_types <- data.frame(image_type = c("Sampling event", "Ice observation flight", "Site audit", "Spill response", "Documentation", "Auto", "Other"),
                          description = c("Regular sampling event (not audit)",
                                          "Observation flight for river ice conditions monitoring",
                                          "Audit of conditions at a water licence holder site",
                                          "Monitoring of a spill or incident involving water quality",
                                          "Photo documentation of a site, instrument setup, etc.",
                                          "Image automatically taken from a web or server location",
                                          "Other"),
                          default_tag_options = c("{sample site}",
                                   "{head, toe, smooth ice, in flood}",
                                   "{sample site}",
                                   "{sample site}",
                                   "{instruments, set up, take down, maintenance}",
                                   "{auto}",
                                   "{undefined}"))
  DBI::dbAppendTable(con, "image_types", img_types)
  
  # Add a new column to the 'images' table for the image type and 'create_datetime'
  DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN image_type INTEGER REFERENCES files.image_types(image_type_id);")
  DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN create_datetime TIMESTAMP DEFAULT NOW();")
  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '14' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  message("Patch 14 applied successfully.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 14 failed and the DB has been rolled back to its earlier state. ", e$message)
})
