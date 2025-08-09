# Patch 23

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 23. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

message("This patch creates a new schema to hold borehole and water well data.")

# Ask the user if they want to populate the boreholes schema with data from the Access and SQL Server databases
message("Would you like to populate the boreholes schema with data from the Access and SQL Server databases? \n 1 = Yes \n 2 = No")
choice <- readline(prompt = "Enter 1 or 2: ")
if (choice != "1" && choice != "2") {
  stop("Invalid choice. Please enter 1 or 2.")
}

if (choice == "2") {
  message("The boreholes schema will be created but will be empty.")
}

if (choice == "1") {
  message("The boreholes schema will be populated with data from the Access and SQL Server databases.")
}

if (choice == "1") {
  # Ask the user to confirm the path to the Access database
  message("Is 'X:/YWWR/App/Database27.mdb' the correct path to the Access database? Enter 1 for yes or type in the correct path.")
  choice <- readline(prompt = "Enter 1 or the correct path: ")
  if (choice != "1") {
    accessPath <- choice
    message("Using the provided path: ", path)
  } else {
    accessPath <- "X:/YWWR/App/Database27.mdb"
    message("Using the default path: X:/YWWR/App/Database27.mdb")
  }
  
  message("Using the SQL Server database 'YWWR' on server 'sql-apps2-prd'. Ensure you have access to this database and the necessary permissions to create tables and schemas.")
  
  # Try to connect to the old Access database to fetch relevant tables
  tryCatch({
    acc <- AccessConnect(path = accessPath, silent = TRUE)
    access <- TRUE
  }, error = function(e) {
    stop("Failed to connect to the Access database. Error: ", e$message)
    access <- FALSE
  })
  
  # Try to connect to the current SQL Server database to fetch records
  tryCatch({
    sql <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "sql-apps2-prd", 
                          Database = "YWWR", Trusted_Connection = "True")
    sql_server <- TRUE
  }, error = function(e) {
    stop("Failed to connect to the SQL Server database. Error: ", e$message)
    sql_server <- FALSE
  })
  
} else {
  # If the user does not want to populate the boreholes schema, set access and sql_server to FALSE
  access <- FALSE
  sql_server <- FALSE
}



# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop("A transaction is already in progress. Please commit or rollback the current transaction before applying this patch.")
}
active <- dbTransBegin(con)


tryCatch({
  
  # Delete the old boreholes schema if it exists
  DBI::dbExecute(con, "DROP SCHEMA IF EXISTS boreholes CASCADE;")
  
  # Create the boreholes schema and update the search path
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS boreholes;")
  # Get the name of the database to which connected
  db_name <- DBI::dbGetQuery(con, "SELECT current_database();")[1,1]
  DBI::dbExecute(con, paste0("ALTER DATABASE ", db_name, " SET search_path TO public, continuous, discrete, spatial, files, instruments, boreholes, information, application;"))
  DBI::dbExecute(con, "SET search_path TO public, continuous, discrete, spatial, files, instruments, boreholes, information, application;")
  
  
  
  # Create the 'drillers' table in schema 'boreholes'
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS boreholes.drillers (
      driller_id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      address TEXT,
      phone TEXT,
      email TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );")
  
  # Create the boreholes table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS boreholes.boreholes (
      borehole_id SERIAL PRIMARY KEY,
      location_id INTEGER REFERENCES locations(location_id) ON DELETE SET NULL ON UPDATE CASCADE,
      borehole_name TEXT,
      completion_date DATE,
      drilled_by INTEGER REFERENCES drillers(driller_id) ON DELETE SET NULL ON UPDATE CASCADE,
      drill_method TEXT,
      comissioned_by TEXT,
      latitude NUMERIC NOT NULL,
      longitude NUMERIC NOT NULL,
      location_source TEXT,
      ground_elevation_m NUMERIC,
      elevation_source TEXT,
      depth_m NUMERIC,
      import_borehole_id TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );")
  # Create an index on the borehole_name column for faster searches
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_borehole_name ON boreholes.boreholes(borehole_name);")
  # Create an index on latitude and longitude for faster spatial queries
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_borehole_lat_lon ON boreholes.boreholes(latitude, longitude);")
  
  # Create a join table for documents
  DBI::dbExecute(con, "
                 CREATE TABLE IF NOT EXISTS boreholes.boreholes_documents (
                  borehole_id INTEGER NOT NULL REFERENCES boreholes.boreholes(borehole_id) ON DELETE CASCADE ON UPDATE CASCADE,
                  document_id INTEGER NOT NULL REFERENCES files.documents(document_id) ON DELETE CASCADE ON UPDATE CASCADE,
                  PRIMARY KEY (borehole_id, document_id)
    );")
  
  # Create the table for permafrost, referencing borehole_id
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS boreholes.permafrost (
      permafrost_record_id SERIAL PRIMARY KEY,
      borehole_id INTEGER NOT NULL REFERENCES boreholes.boreholes(borehole_id) ON DELETE CASCADE ON UPDATE CASCADE,
      depth_from_m NUMERIC,
      depth_to_m NUMERIC,
      ice_description TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );")
  
  # Create function and trigger to prevent overlapping permafrost records
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION boreholes.prevent_permafrost_overlap()
    RETURNS TRIGGER AS $$
      DECLARE
    conflict_count INTEGER;
    BEGIN
    SELECT 1
    INTO conflict_count
    FROM boreholes.permafrost p
    WHERE p.borehole_id    = NEW.borehole_id
    AND NEW.depth_from_m < p.depth_to_m
    AND NEW.depth_to_m   > p.depth_from_m
    -- on UPDATE skip comparing against itself
    AND (TG_OP = 'INSERT' OR p.permafrost_record_id <> NEW.permafrost_record_id)
    LIMIT 1;
    
    IF FOUND THEN
    RAISE EXCEPTION
    'Cannot add permafrost [% – %] for borehole %: overlaps existing interval',
    NEW.depth_from_m, NEW.depth_to_m, NEW.borehole_id;
    END IF;
    
    RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    ")
  
  DBI::dbExecute(con, "
    CREATE TRIGGER trg_permafrost_no_overlap
    BEFORE INSERT OR UPDATE ON boreholes.permafrost
    FOR EACH ROW
    EXECUTE FUNCTION boreholes.prevent_permafrost_overlap();
    ")
  
  # Create the table for geology, referencing borehole_id
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS boreholes.geology (
      geo_record_id SERIAL PRIMARY KEY,
      borehole_id INTEGER NOT NULL REFERENCES boreholes.boreholes(borehole_id) ON DELETE CASCADE ON UPDATE CASCADE,
      depth_from_m NUMERIC NOT NULL,
      depth_to_m NUMERIC NOT NULL,
      color TEXT,
      consolidated BOOLEAN NOT NULL,
      primary_material TEXT,
      secondary_material TEXT,
      texture TEXT,
      description TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );")
  
  # Create function and trigger to prevent overlapping geology records
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION boreholes.prevent_geology_overlap()
    RETURNS TRIGGER AS $$
      DECLARE
    conflict_count INTEGER;
    BEGIN
    SELECT 1
    INTO conflict_count
    FROM boreholes.geology g
    WHERE g.borehole_id    = NEW.borehole_id
    AND NEW.depth_from_m < g.depth_to_m
    AND NEW.depth_to_m   > g.depth_from_m
    -- on UPDATE skip comparing against itself
    AND (TG_OP = 'INSERT' OR g.geo_record_id <> NEW.geo_record_id)
    LIMIT 1;
    
    IF FOUND THEN
    RAISE EXCEPTION
    'Cannot add geology [% – %] for borehole %: overlaps existing interval',
    NEW.depth_from_m, NEW.depth_to_m, NEW.borehole_id;
    END IF;
    
    RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    ")
  
  DBI::dbExecute(con, "
    CREATE TRIGGER trg_geology_no_overlap
    BEFORE INSERT OR UPDATE ON boreholes.geology
    FOR EACH ROW
    EXECUTE FUNCTION boreholes.prevent_geology_overlap();
    ")
  
  # Create a table for well casing material
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS boreholes.casing_materials (
      casing_material_id SERIAL PRIMARY KEY,
      material_name TEXT NOT NULL,
      material_name_fr TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );")
  
  casings <- data.frame(material_name = c("none", "ABS", "PVC", "steel", "other plastic", "concrete", "other", "unknown"),
                        material_name_fr = c("aucun", "ABS", "PVC", "acier", "autre plastique", "béton", "autre", "inconnu"))
  DBI::dbAppendTable(con, "casing_materials", casings)
  
  # Create the table for water wells
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS boreholes.wells (
      well_id SERIAL PRIMARY KEY,
      borehole_id INTEGER NOT NULL REFERENCES boreholes.boreholes(borehole_id) ON DELETE CASCADE ON UPDATE CASCADE,      
      casing_material INTEGER REFERENCES boreholes.casing_materials(casing_material_id) ON DELETE SET NULL ON UPDATE CASCADE,
      casing_diameter_mm NUMERIC,
      casing_depth_to_m NUMERIC,
      stick_up_height_m NUMERIC,
      casing_comment TEXT,
      screen_top_depth_m NUMERIC,
      screen_bottom_depth_m NUMERIC,
      screen_comment TEXT,
      static_water_level_m NUMERIC,
      estimated_yield_lps NUMERIC,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    );")
  
  
  # Add new document types
  new_types <- data.frame(document_type_en = c("borehole log", "pumping test", "water quality test", "geological profile", "water well record"),
                          document_type_fr = c("journal de forage", "essai de pompage", "test de qualité de l'eau", "profil géologique", "enregistrement de puits d'eau"))
  
  # The document types might exist already, so only insert if they don't
  for (i in 1:nrow(new_types)) {
    type_exists <- DBI::dbGetQuery(con, paste0("SELECT document_type_id FROM document_types WHERE document_type_en = '", new_types$document_type_en[i], "';"))[1,1]
    if (is.na(type_exists)) {
      DBI::dbExecute(con, paste0("INSERT INTO document_types (document_type_en, document_type_fr) VALUES ('", new_types$document_type_en[i], "', '", new_types$document_type_fr[i], "');"))
    }
  }
  
  # Add a new column 'private_expiry' to all tables with column 'share_with'
  # Will be used to search the private_expiry column for a date now in the past and change share_with entry to public_reader for those records
  
  # Find all tables containing a column called 'share_with'
  tbls <- DBI::dbGetQuery(con, "SELECT DISTINCT table_name FROM information_schema.columns WHERE column_name = 'share_with';")
  for (i in 1:nrow(tbls)) {
    tbl <- tbls$table_name[i]
    # Check if the table already has a column called 'private_expiry'
    col_exists <- DBI::dbGetQuery(con, paste0("SELECT column_name FROM information_schema.columns WHERE table_name = '", tbl, "' AND column_name = 'private_expiry';"))
    if (nrow(col_exists) == 0) {
      # Add the new column 'private_expiry' to the table
      DBI::dbExecute(con, paste0("ALTER TABLE ", tbl, " ADD COLUMN private_expiry DATE;"))
      message("Added column 'private_expiry' to table ", tbl)
    } else {
      message("Column 'private_expiry' already exists in table ", tbl)
    }
  }
  
  # Add new data if Access and SQL Server connections are available   #############
  # Bring in data from the 'R_Driller' Access table
  if (access && sql_server) {
    drillers <- DBI::dbGetQuery(acc, "SELECT * FROM R_Driller;")
    if (nrow(drillers) > 0) {
      # Make a data.frame mapped to the new drillers table
      drillers_import <- data.frame(
        name = drillers$`Drilling Company Names`,
        address = drillers$Driller_Address
      )
      # Append the drillers data to the drillers table
      DBI::dbAppendTable(con, "drillers", drillers_import)
      message("Imported drillers from the Access database.")
    } else {
      message("No drillers found in the Access database.")
    }
    
    
    # Bring in the borehole data from the Access database. There may or may not be any way to match to the SQL Server data, so this is a best effort.
    boreholes_acc <- DBI::dbGetQuery(acc, "SELECT BoreholeID AS _import_borehole_id_Access, Permafrost, PFDepthFrm, PFDepthTo, Depth, DrillerCode, DrillerNames FROM 1_BOREHOLE;")
    boreholes_sql <- DBI::dbGetQuery(sql, "SELECT BoreholeId AS _import_borehole_id_SQL, WellName, UTMZone, Easting, Northing, LocationSource, Purpose, DepthToBedrock, DrillYear, DrillMonth, DrillDay, WellDepth, StaticWaterLevel, EstimatedYield, TopOfScreen, BottomOfScreen, WellHeadStickUp FROM WellRecords;")
    boreholes <- merge(boreholes_sql, boreholes_acc, by.y = "_import_borehole_id_Access", by.x = "_import_borehole_id_SQL", all.x = TRUE) # We are purposely dropping any boreholes that are only in Access as these don't have a water well associated and are 'orphan' records
    
    
    # convert the easting and northing (plus UTM zones) to latitude and longitude
    boreholes$latitude <- NA
    boreholes$longitude <- NA
    rlang::check_installed("oce", "for converting UTM coordinates to latitude and longitude")
    for (i in 1:nrow(boreholes)) {
      res <- oce::utm2lonlat(boreholes$Easting[i], boreholes$Northing[i], zone = boreholes$UTMZone[i], hemisphere = "N")
      boreholes$latitude[i] <- res$latitude
      boreholes$longitude[i] <- res$longitude
    }
    
    # Convert the completion date to a date format
    boreholes$completion_date <- as.Date(paste(boreholes$DrillYear, boreholes$DrillMonth, boreholes$DrillDay, sep = "-"))
    # Make any date before 1900 NA
    boreholes$completion_Date[boreholes$completion_date < as.Date("1900-01-01")] <- NA
    
    
    # Deal with elevations. We'll extract elevation from a web service as they're all shit
    rlang::check_installed("elevatr", "for fetching elevations from a web service")
    rlang::check_installed("sf", "for handling spatial data")
    for (i in 1:nrow(boreholes)) {
      pts <- data.frame(x = boreholes$longitude[i], y = boreholes$latitude[i])
      # If x or y is NA, skip this point
      if (is.na(pts$x) || is.na(pts$y)) {
        boreholes$ground_elevation_m[i] <- NA
        next
      }
      sf_pts <- sf::st_as_sf(pts, coords = c("x","y"), crs = 4326)
      elev_data <- elevatr::get_elev_point(locations = sf_pts, src = "aws", z = 14)
      boreholes$ground_elevation_m[i] <- elev_data$elevation[1]
    }
    
    # Deal with well depth. Use column from SQL Server, convert from feet to meters
    boreholes$depth_m <- boreholes$WellDepth * 0.3048 # Convert feet to meters
    
    # Find the driller information. Access table R_Driller column Driller_Code matches boreholes_acc$DrillerCode, but have to find the match for 'R_Driller.Drilling Company Names' to the new Postres table 'drillers' in order to match on id
    new_drillers <- DBI::dbGetQuery(con, "SELECT driller_id, name FROM boreholes.drillers;")
    # Match the table 'drillers', fetched earlier from Access, to the new_drillers table on drillers$Drilling Company Names = new_drillers$name to find drillers$Driller_code
    new_drillers <- merge(new_drillers, drillers[, c("Driller_code", "Drilling Company Names")], by.x = 'name', by.y = 'Drilling Company Names')
    boreholes$drilled_by <- NA 
    for (i in 1:nrow(boreholes)) {
      new_id <- NA
      new_id <- new_drillers[new_drillers$Driller_code == boreholes$DrillerCode[i], "driller_id"]
      if (is.na(new_id[1]))  { # Also check for a match on column 'DrillerNames'
        new_id <- new_drillers[new_drillers$Driller_code == boreholes$DrillerNames[i], "driller_id"]
      }
      if (!is.na(new_id[1])) {
        boreholes$drilled_by[i] <- new_id
      } else {
        boreholes$drilled_by[i] <- NA
      }
    }
    
    # Make a data.frame mapped to the new boreholes table
    boreholes_import <- data.frame(
      borehole_name = boreholes$WellName,
      completion_date = boreholes$completion_date,
      latitude = boreholes$latitude,
      longitude = boreholes$longitude,
      location_source = boreholes$LocationSource,
      ground_elevation_m = boreholes$ground_elevation_m,
      elevation_source = "web service, NRCAN",
      import_borehole_id = boreholes$`_import_borehole_id_SQL`,
      depth_m = boreholes$depth_m,
      drilled_by = boreholes$drilled_by
    )
    no_coords <- boreholes_import[is.na(boreholes_import$latitude) | is.na(boreholes_import$longitude), ]
    
    # Drop the no_coords from the main boreholes_import data frame
    boreholes_import <- boreholes_import[!is.na(boreholes_import$latitude) & !is.na(boreholes_import$longitude), ]
    
    # Append the borehole data to the boreholes table
    DBI::dbAppendTable(con, "boreholes", boreholes_import)
    message("Imported borehole data from the Access and SQL databases.")
    
    
    # Make a new postgres table for the boreholes with no coordinates (which may or may not ever be fixed)
    DBI::dbExecute(con, "
          CREATE TABLE IF NOT EXISTS boreholes.boreholes_no_coords (
            borehole_id SERIAL PRIMARY KEY,
            borehole_name TEXT,
            completion_date DATE,
            drilled_by INTEGER REFERENCES boreholes.drillers(driller_id) ON DELETE SET NULL ON UPDATE CASCADE,
            drill_method TEXT,
            comissioned_by TEXT,
            latitude NUMERIC,
            longitude NUMERIC,
            location_source TEXT,
            ground_elevation_m NUMERIC,
            elevation_source TEXT,
            depth_m NUMERIC,
            import_borehole_id TEXT,
            created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
          );")
    # Create an index on the borehole_name column for faster searches
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_borehole_name ON boreholes.boreholes(borehole_name);")
    # Create an index on latitude and longitude for faster spatial queries
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_borehole_lat_lon ON boreholes.boreholes(latitude, longitude);")
    
    
    # Create a join table for documents
    DBI::dbExecute(con, "
                 CREATE TABLE IF NOT EXISTS boreholes.boreholes_no_coords_documents (
                  borehole_id INTEGER NOT NULL REFERENCES boreholes.boreholes(borehole_id) ON DELETE CASCADE ON UPDATE CASCADE,
                  document_id INTEGER NOT NULL REFERENCES files.documents(document_id) ON DELETE CASCADE ON UPDATE CASCADE,
                  PRIMARY KEY (borehole_id, document_id)
    );")
    
    DBI::dbAppendTable(con, "boreholes_no_coords", no_coords)
    message("Imported boreholes with no coordinates into the boreholes.boreholes_no_coords table.")
    
    
    
    # For each borehole, fetch the document .ftp link (from SQL Server), fetch the document(s), append them to the files.documents table, and link them to the borehole in the boreholes.boreholes table.
    new_boreholes <- DBI::dbGetQuery(con, "SELECT borehole_id, import_borehole_id, borehole_name FROM boreholes.boreholes;")
    for (i in 1:nrow(new_boreholes)) {
      ftp <- DBI::dbGetQuery(sql, paste0("SELECT WellLog FROM WellRecords WHERE BoreholeId = ", new_boreholes$import_borehole_id[i], ";"))[1,1]
      additional_ftp <- DBI::dbGetQuery(sql, paste0("SELECT DocumentLinks FROM WellRecords WHERE BoreholeId = ", new_boreholes$import_borehole_id[i], ";"))[1,1]
      if (!is.na(ftp)) {
        # Get the file extension from the FTP link
        file_ext <- tools::file_ext(ftp)
        path <- tempfile(fileext = paste0(".", file_ext))
        # Fetch the document from the FTP server
        tryCatch({
          curl::curl_download(ftp, path)
        }, error = function(e) {
          warning("Failed to download document for borehole ", new_boreholes$import_borehole_id[i], ": ", e$message)
        })
        
        if (file.exists(path)) {
          name <- new_boreholes$borehole_name[i]
          if (!is.na(name)) {
            name <- paste0("Well record for ", name)
          } else {
            id <- new_boreholes$borehole_id[i]
            name <- paste0("Well record for well ID ", id)
          }
          # Append the document to the files.documents table
          tryCatch({
            # Check if the name already exists in the files.documents table
            exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
            if (exists > 0) {
              name <- paste0(name, " (2)")
              exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
              if (exists > 0) {
                name <- paste0(name, " (3)")
                exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                if (exists > 0) {
                  name <- paste0(name, " (4)")
                  exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                  if (exists > 0) {
                    name <- paste0(name, " (5)")
                    exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                    if (exists > 0) {
                      name <- paste0(name, " (6)")
                      exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                      if (exists > 0) {
                        stop("Too many documents with the same name exist. Please rename the document manually and try again.")
                      }
                    }
                  }
                }
              }
            }
            res <- insertACDocument(path = path, name = name, type = "water well record", description = "Water well or borehole drilling record.", tags = c("water well", "well", "borehole"), con = con)
            new_doc <- res$new_document_id
          }, error = function(e) {
            if (stringr::str_detect(e$message, stringr::fixed("Key (file_hash)="))) {
              # If the document already exists, get the hash from the error message and find the document_id
              hash <- stringr::str_match(e$message, "Key \\(file_hash\\)=\\(([0-9a-f]+)\\)")[,2]
              # Get the document_id for this hash
              new_doc <<- DBI::dbGetQuery(con, paste0("SELECT document_id FROM files.documents WHERE file_hash = '", hash, "';"))[1,1]
            } else {
              new_doc <<- NA
            }
          })
          
          if (!is.na(new_doc)) {
            # Check if the document is already linked to the borehole
            check <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM boreholes.boreholes_documents WHERE borehole_id = ", new_boreholes$borehole_id[i], " AND document_id = ", new_doc, ";"))[1,1]
            if (check == 0) {
              DBI::dbExecute(con, paste0("INSERT INTO boreholes.boreholes_documents (borehole_id, document_id) VALUES (", new_boreholes$borehole_id[i], ",", new_doc, ");"))
            }
          }
        }
      }
      
      if (!is.na(additional_ftp)) {
        # Get the file extension from the FTP link
        file_ext <- tools::file_ext(additional_ftp)
        path <- tempfile(fileext = paste0(".", file_ext))
        # Fetch the document from the FTP server
        tryCatch({
          curl::curl_download(additional_ftp, path)
        }, error = function(e) {
          warning("Failed to download document for borehole ", new_boreholes$import_borehole_id[i], ": ", e$message)
        })
        
        if (file.exists(path)) {
          
          borehole_name <- new_boreholes$borehole_name[i]
          if (is.na(borehole_name)) {
            borehole_name <- new_boreholes$borehole_id[i]
          }
          
          if (file_ext == "zip") { # unzip the files to a tempdir
            dir <- paste0(tempdir(), "/", new_boreholes$import_borehole_id[i], "_additional")
            # Delete the directory if it exists so we start clean
            if (dir.exists(dir)) {
              unlink(dir, recursive = TRUE)
            }
            dir.create(dir)
            utils::unzip(path, exdir = dir)
            dir <- list.files(dir, full.names = TRUE)
            files <- list.files(dir, full.names = TRUE)
            # Create a name for each file
            names <- character(0)
            additional_increment <- 1
            for (j in 1:length(files)) {
              name <- tools::file_path_sans_ext(basename(files[j]))
              
              if (tolower(name) != "report") {
                names[j] <- paste0(name, " for borehole ", borehole_name)
              } else {
                if (additional_increment > 1) {
                  names[j] <- paste0("Additional document (", additional_increment, ") for borehole ", borehole_name)
                  additional_increment <- additional_increment + 1
                } else {
                  names[j] <- paste0("Additional document for borehole ", borehole_name)
                }
              }
            }
          }
        } else {  # not a zip file (probably a pdf)
          files <- path
          if (tolower(name) != "report") {
            names <- paste0(tools::file_path_sans_ext(basename(files)), " for borehole ", borehole_name)
          } else {
            names <- paste0("Additional document for borehole ", borehole_name)
          }
        }
        
        # Append the document(s) to the files.documents table
        for (j in 1:length(files)) {
          
          f <- files[j]
          n <- names[j]
          # Append the document to the files.documents table
          tryCatch({
            # Check if the name already exists in the files.documents table
            exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
            if (exists > 0) {
              n <- paste0(n, " (2)")
              exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
              if (exists > 0) {
                n <- paste0(n, " (3)")
                exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                if (exists > 0) {
                  n <- paste0(n, " (4)")
                  exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                  if (exists > 0) {
                    n <- paste0(n, " (5)")
                    exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                    if (exists > 0) {
                      n <- paste0(n, " (6)")
                      exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                      if (exists > 0) {
                        stop("Too many documents with the same name exist. Please rename the document manually and try again.")
                      }
                    }
                  }
                }
              }
            }
            res <- insertACDocument(path = f, name = n, type = "water well record", description = "Water well or borehole drilling record.", tags = c("water well", "well", "borehole"), con = con)
            new_doc <- res$new_document_id
          }, error = function(e) {
            if (stringr::str_detect(e$message, stringr::fixed("Key (file_hash)="))) {
              # If the document already exists, get the hash from the error message and find the document_id
              hash <- stringr::str_match(e$message, "Key \\(file_hash\\)=\\(([0-9a-f]+)\\)")[,2]
              # Get the document_id for this hash
              new_doc <<- DBI::dbGetQuery(con, paste0("SELECT document_id FROM files.documents WHERE file_hash = '", hash, "';"))[1,1]
            } else {
              new_doc <<- NA
            }
          })
          
          if (!is.na(new_doc)) {
            # Check if the document is already linked to the borehole
            check <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM boreholes.boreholes_documents WHERE borehole_id = ", new_boreholes$borehole_id[i], " AND document_id = ", new_doc, ";"))[1,1]
            if (check == 0) {
              DBI::dbExecute(con, paste0("INSERT INTO boreholes.boreholes_documents (borehole_id, document_id) VALUES (", new_boreholes$borehole_id[i], ",", new_doc, ");"))
            }
          }
        }
      }
    } # End of loop over new_boreholes
    
    
    # Do the same thing for boreholes_no_coords
    new_boreholes_no_coords <- DBI::dbGetQuery(con, "SELECT borehole_id, import_borehole_id, borehole_name FROM boreholes.boreholes_no_coords;")
    for (i in 1:nrow(new_boreholes_no_coords)) {
      ftp <- DBI::dbGetQuery(sql, paste0("SELECT WellLog FROM WellRecords WHERE BoreholeId = ", new_boreholes_no_coords$import_borehole_id[i], ";"))[1,1]
      if (!is.na(ftp)) {
        # Get the file extension from the FTP link
        file_ext <- tools::file_ext(ftp)
        path <- tempfile(fileext = paste0(".", file_ext))
        # Fetch the document from the FTP server
        tryCatch({
          curl::curl_download(ftp, path)
        }, error = function(e) {
          warning("Failed to download document for borehole ", new_boreholes_no_coords$import_borehole_id[i], ": ", e$message)
        })
        
        if (file.exists(path)) {
          name <- new_boreholes_no_coords$borehole_name[i]
          if (!is.na(name)) {
            name <- paste0("Well record for ", name)
          } else {
            id <- new_boreholes_no_coords$borehole_id[i]
            name <- paste0("Well record for well ID ", id)
          }
          # Append the document to the files.documents table
          tryCatch({
            # Check if the name already exists in the files.documents table
            exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
            if (exists > 0) {
              name <- paste0(name, " (2)")
              exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
              if (exists > 0) {
                name <- paste0(name, " (3)")
                exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                if (exists > 0) {
                  name <- paste0(name, " (4)")
                  exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                  if (exists > 0) {
                    name <- paste0(name, " (5)")
                    exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                    if (exists > 0) {
                      name <- paste0(name, " (6)")
                      exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", name, "';"))[1,1]
                      if (exists > 0) {
                        stop("Too many documents with the same name exist. Please rename the document manually and try again.")
                      }
                    }
                  }
                }
              }
            }
            res <- insertACDocument(path = path, name = name, type = "water well record", description = "Water well or borehole drilling record.", tags = c("water well", "well", "borehole"), con = con)
            new_doc <- res$new_document_id
          }, error = function(e) {
            if (stringr::str_detect(e$message, stringr::fixed("Key (file_hash)="))) {
              # If the document already exists, get the hash from the error message and find the document_id
              hash <- stringr::str_match(e$message, "Key \\(file_hash\\)=\\(([0-9a-f]+)\\)")[,2]
              # Get the document_id for this hash
              new_doc <<- DBI::dbGetQuery(con, paste0("SELECT document_id FROM files.documents WHERE file_hash = '", hash, "';"))[1,1]
            } else {
              new_doc <<- NA
            }
          })
          
          if (!is.na(new_doc)) {
            # Check if the document is already linked to the borehole
            check <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM boreholes.boreholes_no_coords_documents WHERE borehole_id = ", new_boreholes_no_coords$borehole_id[i], " AND document_id = ", new_doc, ";"))[1,1]
            if (check == 0) {
              DBI::dbExecute(con, paste0("INSERT INTO boreholes.boreholes_no_coords_documents (borehole_id, document_id) VALUES (", new_boreholes_no_coords$borehole_id[i], ",", new_doc, ");"))
            }
          }
          
          
          if (!is.na(additional_ftp)) {
            # Get the file extension from the FTP link
            file_ext <- tools::file_ext(additional_ftp)
            path <- tempfile(fileext = paste0(".", file_ext))
            # Fetch the document from the FTP server
            tryCatch({
              curl::curl_download(additional_ftp, path)
            }, error = function(e) {
              warning("Failed to download document for borehole ", new_boreholes_no_coords$import_borehole_id[i], ": ", e$message)
            })
            
            if (file.exists(path)) {
              
              borehole_name <- new_boreholes_no_coords$borehole_name[i]
              if (is.na(borehole_name)) {
                borehole_name <- new_boreholes_no_coords$borehole_id[i]
              }
              
              if (file_ext == "zip") { # unzip the files to a tempdir
                dir <- paste0(tempdir(), "/", new_boreholes_no_coords$import_borehole_id[i], "_additional")
                # Delete the directory if it exists so we start clean
                if (dir.exists(dir)) {
                  unlink(dir, recursive = TRUE)
                }
                dir.create(dir)
                utils::unzip(path, exdir = dir)
                dir <- list.files(dir, full.names = TRUE)
                files <- list.files(dir, full.names = TRUE)
                # Create a name for each file
                names <- character(0)
                additional_increment <- 1
                for (j in 1:length(files)) {
                  name <- tools::file_path_sans_ext(basename(files[j]))
                  
                  if (tolower(name) != "report") {
                    names[j] <- paste0(name, " for borehole ", borehole_name)
                  } else {
                    if (additional_increment > 1) {
                      names[j] <- paste0("Additional document (", additional_increment, ") for borehole ", borehole_name)
                      additional_increment <- additional_increment + 1
                    } else {
                      names[j] <- paste0("Additional document for borehole ", borehole_name)
                    }
                  }
                }
              }
            } else {  # not a zip file (probably a pdf)
              files <- path
              if (tolower(name) != "report") {
                names <- paste0(tools::file_path_sans_ext(basename(files)), " for borehole ", borehole_name)
              } else {
                names <- paste0("Additional document for borehole ", borehole_name)
              }
            }
            
            # Append the document(s) to the files.documents table
            for (j in 1:length(files)) {
              
              f <- files[j]
              n <- names[j]
              # Append the document to the files.documents table
              tryCatch({
                # Check if the name already exists in the files.documents table
                exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                if (exists > 0) {
                  n <- paste0(n, " (2)")
                  exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                  if (exists > 0) {
                    n <- paste0(n, " (3)")
                    exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                    if (exists > 0) {
                      n <- paste0(n, " (4)")
                      exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                      if (exists > 0) {
                        n <- paste0(n, " (5)")
                        exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                        if (exists > 0) {
                          n <- paste0(n, " (6)")
                          exists <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM files.documents WHERE name = '", n, "';"))[1,1]
                          if (exists > 0) {
                            stop("Too many documents with the same name exist. Please rename the document manually and try again.")
                          }
                        }
                      }
                    }
                  }
                }
                res <- insertACDocument(path = f, name = n, type = "water well record", description = "Water well or borehole drilling record.", tags = c("water well", "well", "borehole"), con = con)
                new_doc <- res$new_document_id              }, error = function(e) {
                  if (stringr::str_detect(e$message, stringr::fixed("Key (file_hash)="))) {
                    # If the document already exists, get the hash from the error message and find the document_id
                    hash <- stringr::str_match(e$message, "Key \\(file_hash\\)=\\(([0-9a-f]+)\\)")[,2]
                    # Get the document_id for this hash
                    new_doc <<- DBI::dbGetQuery(con, paste0("SELECT document_id FROM files.documents WHERE file_hash = '", hash, "';"))[1,1]
                  } else {
                    new_doc <<- NA
                  }
                })
              
              if (!is.na(new_doc)) {
                # Check if the document is already linked to the borehole
                check <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) FROM boreholes.boreholes_no_coords_documents WHERE borehole_id = ", new_boreholes_no_coords$borehole_id[i], " AND document_id = ", new_doc, ";"))[1,1]
                if (check == 0) {
                  DBI::dbExecute(con, paste0("INSERT INTO boreholes.boreholes_no_coords_documents (borehole_id, document_id) VALUES (", new_boreholes_no_coords$borehole_id[i], ",", new_doc, ");"))
                }
              }
            }
          }
        }
      }
    }
    
    
    # For each borehole, fetch the permafrost data from the Access database and insert it into the boreholes.permafrost table
    boreholes_pf <- boreholes[boreholes$Permafrost & !is.na(boreholes$Permafrost) & !is.na(boreholes$latitude),]
    for (i in 1:nrow(boreholes_pf)) {
      # Find the new borehole_id using _import_borehole_id_SQL to the database's new 'import_borehole_id' column
      new_borehole_id <- DBI::dbGetQuery(con, paste0("SELECT borehole_id FROM boreholes.boreholes WHERE import_borehole_id = '", boreholes_pf$`_import_borehole_id_SQL`[i], "';"))[1,1]
      if (!is.na(new_borehole_id)) {
        # Insert the permafrost data into the boreholes.permafrost table
        DBI::dbExecute(con, paste0("INSERT INTO boreholes.permafrost (borehole_id, depth_from_m, depth_to_m) VALUES (", new_borehole_id, ", ", if (!is.na(boreholes_pf$PFDepthFrm[i])) boreholes_pf$PFDepthFrm[i] else "NULL", ", ", if (!is.na(boreholes_pf$PFDepthTo[i])) boreholes_pf$PFDepthTo[i] else "NULL", ");"))
      } else {
        message("No matching borehole found for permafrost record with import_borehole_id ", boreholes_pf$`_import_borehole_id_SQL`[i])
      }
    }
    
    # For each borehole, populate the new 'wells' table with the well data
    casing_materials <- DBI::dbGetQuery(con, "SELECT casing_material_id, material_name FROM boreholes.casing_materials;")
    
    # function to automatically retry Access calls that fail
    retry <- function(expr, max_attempts = 10, pause = 1) {
      attempt <- 1
      while (attempt <= max_attempts) {
        result <- try(expr, silent = TRUE)
        if (!inherits(result, "try-error")) {
          return(result)
        }
        Sys.sleep(pause)
        attempt <- attempt + 1
      }
      stop("All ", max_attempts, " attempts failed. MS Access is garbage.")
    }
    
    for (i in 1:nrow(boreholes)) {
      # Find the new borehole_id using _import_borehole_id_SQL to the database's new 'import_borehole_id' column
      new_borehole_id <- DBI::dbGetQuery(con, paste0("SELECT borehole_id FROM boreholes.boreholes WHERE import_borehole_id = '", boreholes$`_import_borehole_id_SQL`[i], "';"))[1,1]  # there could be no id if the borehole had no lat/long
      if (!is.na(new_borehole_id)) {
        # If possible find casing material from Access table 2_WELL, matching on boreholes$`_import_borehole_id_SQL`
        casing_material <- retry(DBI::dbGetQuery(acc, paste0("SELECT CasingMatl FROM 2_WELL WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1])
        # Map the casing_material to the new casing_materials table's casing_material_id
        casing_material_id <- casing_materials[tolower(casing_materials$material_name) == tolower(casing_material), "casing_material_id"]
        casing_diameter_mm <- retry(DBI::dbGetQuery(acc, paste0("SELECT CasingDia FROM 2_WELL WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1])
        casing_diameter_in <- DBI::dbGetQuery(sql, paste0("SELECT CasingOutsideDiameter FROM WellRecords WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1]
        casing_depth_to <- retry(DBI::dbGetQuery(acc, paste0("SELECT C_DepthTo FROM 2_WELL WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1])
        stick_up_height <- DBI::dbGetQuery(sql, paste0("SELECT WellHeadStickUp FROM WellRecords WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1]
        casing_comment <- retry(DBI::dbGetQuery(acc, paste0("SELECT Case_Comm FROM 2_WELL WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1])
        screen_top <- DBI::dbGetQuery(sql, paste0("SELECT TopOfScreen FROM WellRecords WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1]
        screen_bottom <- DBI::dbGetQuery(sql, paste0("SELECT BottomOfScreen FROM WellRecords WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1]
        screen_comment <- retry(DBI::dbGetQuery(acc, paste0("SELECT ScreenComm FROM 2_WELL WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1])
        static_wl <- DBI::dbGetQuery(sql, paste0("SELECT StaticWaterLevel FROM WellRecords WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1]
        est_yield <- DBI::dbGetQuery(sql, paste0("SELECT EstimatedYield FROM WellRecords WHERE BoreholeID = ", boreholes$`_import_borehole_id_SQL`[i], ";"))[1,1]
        
        df <- data.frame(
          borehole_id = new_borehole_id,
          casing_material = if (!is.na(casing_material_id[1])) casing_material_id[1] else NA,
          casing_diameter_mm = if (!is.na(casing_diameter_mm)) casing_diameter_mm else if (!is.na(casing_diameter_in)) casing_diameter_in * 25.4 else NA, # Convert inches to mm
          casing_depth_to_m = if (!is.na(casing_depth_to)) casing_depth_to else NA,
          stick_up_height_m = if (!is.na(stick_up_height)) stick_up_height * 0.3048 else NA, # Convert feet to meters
          casing_comment = if (!is.na(casing_comment)) casing_comment else NA,
          screen_top_depth_m = if (!is.na(screen_top)) screen_top * 0.3048 else NA, # Convert feet to meters
          screen_bottom_depth_m = if (!is.na(screen_bottom)) screen_bottom * 0.3048 else NA, # Convert feet to meters
          screen_comment = if (!is.na(screen_comment)) screen_comment else NA,
          static_water_level_m = if (!is.na(static_wl)) static_wl * 0.3048 else NA, # Convert feet to meters
          estimated_yield_lps = if (!is.na(est_yield)) est_yield * 0.0630902 else NA # Convert gallons per minute to liters per second
        )
        
        DBI::dbAppendTable(con, "wells", df)
      }
    }
    
    
    # Now let's see if we can associate boreholes with existing locations in the database, using the borehole latitude and longitude and a 200m buffer around the locations table latitude/longitude
    locations <- DBI::dbGetQuery(con, "SELECT location_id, name, latitude, longitude FROM locations WHERE latitude IS NOT NULL AND longitude IS NOT NULL AND location LIKE 'YOWN%';")
    bohole_locs <- DBI::dbGetQuery(con, "SELECT borehole_id, borehole_name, latitude, longitude FROM boreholes.boreholes;")
    
    # Ensure geosphere package is installed
    rlang::check_installed("geosphere", "calculate distances between points")
    for (i in 1:nrow(bohole_locs)) {
      # Calculate the distance from this borehole to all locations
      dists <- geosphere::distHaversine(matrix(c(bohole_locs$longitude[i], bohole_locs$latitude[i]), ncol = 2),
                                        matrix(c(locations$longitude, locations$latitude), ncol = 2))
      # Find the minimum distance
      min_dist <- min(dists, na.rm = TRUE)
      if (min_dist <= 200) { # If the minimum distance is less than or equal to 200m, associate the borehole with that location
        loc_id <- locations$location_id[which.min(dists)]
        # Ask the user if they want to associate the borehole with this location
        borehole_name <- bohole_locs$borehole_name[i]
        location_name <- locations$name[which.min(dists)]
        response <- readline(prompt = paste0("Borehole '", borehole_name, "' is ", round(min_dist), "m from location '", location_name, "'. Associate with this location? (y/n): "))
        if (tolower(response) == "y") {
          DBI::dbExecute(con, paste0("UPDATE boreholes.boreholes SET location_id = ", loc_id, " WHERE borehole_id = ", bohole_locs$borehole_id[i], ";"))
          message("Associated borehole '", borehole_name, "' with location '", location_name, "'.")
        } else {
          message("Did not associate borehole '", borehole_name, "' with any location.")
        }
      }
    }
    
  }  # End of Access import
  
  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '23' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  message("Patch 23 applied successfully.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  stop("Patch 23 failed and the DB has been rolled back to its earlier state. ", e$message)
})
