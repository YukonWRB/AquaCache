# Patch 3.
# Creates new table 'qualifier' to hold information such as 'ice affected', 'estimated', 'draw-down', etc. for data quality control.

# Initial checks #################
# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")

if (!check$can_create) {
  stop("You do not have the necessary privileges to create a new schema in this database.")
}

message("Working on Patch 3. This update will take a while, please be patient! It's possible that your R console appears frozen but is not.\n  Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  # Add column name_fr to owners_contributors
  DBI::dbExecute(con, "ALTER TABLE owners_contributors ADD COLUMN name_fr TEXT;")
  
  tbls <- DBI::dbListTables(con)
  if ("qualifiers" %in% tbls) {
    DBI::dbExecute(con, "DROP TABLE qualifiers;")
  }
  
  if ("qualifier_types" %in% tbls) {
    DBI::dbExecute(con, "DROP TABLE qualifier_types;")
  }
  
  message("Working on qualifier tables")
  # Make new qualifiers and qualifier_types tables #################
  DBI::dbExecute(con, "CREATE TABLE public.qualifier_types (
  qualifier_type_id SERIAL PRIMARY KEY,
  qualifier_type_code TEXT NOT NULL UNIQUE,
  qualifier_type_description TEXT,
  qualifier_type_description_fr TEXT
);")
  
  qualifiers <- data.frame(
    qualifier_type_code = c("ICE", "ICE-EST", "DRY", "OOW", "SUS", "EST", "DD", "BW", "INT", "HW-MISS", "LW-MISS", "PMMAX", "PMMIN", "PYMAX", "PYMIN", "REL", "UNS", "UNK"),
    qualifier_type_description = c("Ice present", "Ice interpollation/estimation", "Dry well/stream/lake (not only sensor out of water)", "Sensor out of water", "Suspect measurements", "Estimated", "Draw-down after pumping", "Backwater affecting measurements", "Interpolated data", "High water missed (peak not recorded)", "Low water missed (trough not recorded)", "Peak montly maximum", "Peak monthly minimum", "Peak yearly maximum", "Peak yearly minimum", "Release of water, ex. beaver dam breaking", "Unspecified", "Unknown"),
    qualifier_type_description_fr = c("Glace présente", "Interpolation/estimation due à la glace", "Asséché (non seulement capteur hors de l'eau)", "Capteur hors de l'eau", "Mesures suspectes", "Estimé", "Abaissement après pompage", "Refoulement affectant les mesures", "Données interpolées", "Hautes eaux manquées (creux non enregistré)", "Basses eaux manquées (pic non enregistré)", "Pic mensuel maximum", "Pic mensuel minimum", "Pic annuel maximum", "Pic annuel minimum", "Libération d'eau, ex. rupture d'un barrage de castor", "Non spécifié", "Inconnu")
  )
  
  DBI::dbAppendTable(con, "qualifier_types", qualifiers)
  
  # Check if the table now exists with correct columns and entries
  tbls <- DBI::dbListTables(con)
  if (!"qualifier_types" %in% tbls) {
    stop("Table 'qualifier_types' was not created. Please check the database and try again.")
  } else {
    cols <- DBI::dbListFields(con, "qualifier_types")
    if (!all(c("qualifier_type_id", "qualifier_type_code", "qualifier_type_description") %in% cols)) {
      stop("Table 'qualifier_types' was created but does not contain the expected columns. Please check the database and try again.")
    } else {
      codes <- DBI::dbGetQuery(con, "SELECT qualifier_type_code FROM qualifier_types;")
      if (!all(c("ICE", "DRY", "SUS", "EST", "DD", "BW", "UNS", "UNK") %in% codes$qualifier_type_code)) {
        stop("Table 'qualifier_types' was created but does not contain the expected entries in qualifier_type_code. Please check the database and try again.")
      }
    }
  }
  
  # Now make a table 'qualifiers' and check functions to apply qualifiers for timeseries and time ranges, similar to table 'corrections'
  DBI::dbExecute(con,
                 "CREATE TABLE public.qualifiers (
               qualifier_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               qualifier_type_id INTEGER NOT NULL REFERENCES qualifier_types(qualifier_type_id) ON DELETE CASCADE ON UPDATE CASCADE,
               start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
               updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
               );")
  # Make an index to speed things up later on as the table grows
  DBI::dbExecute(con, 
                 "CREATE INDEX idx_qualifiers_timeseries_time
ON public.qualifiers (timeseries_id, start_dt, end_dt);
")
  
  # Make a function and trigger that ensures that qualifiers do not overlap in time for a given timeseries_id
  DBI::dbExecute(con,
                 "CREATE OR REPLACE FUNCTION public.check_qualifiers_overlap()
                RETURNS TRIGGER
                LANGUAGE plpgsql
                AS $$
                BEGIN
                    IF EXISTS (
                        SELECT 1
                        FROM qualifiers
                        WHERE timeseries_id = NEW.timeseries_id
                        AND qualifier_id != NEW.qualifier_id  -- Exclude the current row in an UPDATE
                        AND (
                            (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                        )
                    ) THEN
                        RAISE EXCEPTION 'Qualifiers cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                    END IF;
                    RETURN NEW;
                END;
                $$;
              ")
  
  DBI::dbExecute(con,
                 "CREATE CONSTRAINT TRIGGER check_qualifiers_overlap
                AFTER INSERT OR UPDATE ON qualifiers
                DEFERRABLE INITIALLY DEFERRED
                FOR EACH ROW
                EXECUTE FUNCTION public.check_qualifiers_overlap();
                ")
  
  # Make a function and trigger that updates the 'updated' column when a qualifiers entry is updated
  DBI::dbExecute(con,
                 "CREATE OR REPLACE FUNCTION public.update_updated()
               RETURNS TRIGGER
               LANGUAGE plpgsql
               AS $$
               BEGIN
               NEW.updated = NOW();
               RETURN NEW;
               END;
               $$;")
  
  DBI::dbExecute(con,
                 "CREATE OR REPLACE TRIGGER update_qualifiers_updated
               BEFORE UPDATE
               ON public.qualifiers
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")
  
  # While we're at it let's do the same thing for table 'corrections' since it isn't done already...
  DBI::dbExecute(con,
                 "CREATE OR REPLACE TRIGGER update_corrections_updated
               BEFORE UPDATE
               ON public.corrections
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")
  
  
  
  # Now let's apply the same principle to the 'approvals' and 'grades' tables.
  
  # Fold old 'approvals', 'grades' tables into new structures #################
  # modify some old names for consistency
  DBI::dbExecute(con, "UPDATE approvals SET description = 'Unspecified', description_fr = 'Non spécifié' WHERE description = 'Undefined';")
  DBI::dbExecute(con, "UPDATE grades SET description = 'Unspecified', description_fr = 'Non spécifié' WHERE description = 'Undefined';")
  
  message("Working on approvals tables")
  # Create new table 'approval_types'. This is existing table 'approvals' but with a new '_id' column.
  exist_table_approvals <- DBI::dbGetQuery(con, "SELECT * FROM approvals")
  names(exist_table_approvals) <- c("approval_type_code", "approval_type_description", "approval_type_description_fr")
  exist_table_approvals[exist_table_approvals$approval_type_code == "Z" , "approval_type_code"] <- "UNK"
  exist_table_approvals[exist_table_approvals$approval_type_code == "U" , "approval_type_code"] <- "UNS"
  
  DBI::dbExecute(con,
                 "CREATE TABLE public.approval_types (
               approval_type_id SERIAL PRIMARY KEY,
               approval_type_code TEXT NOT NULL UNIQUE,
               approval_type_description TEXT NOT NULL,
               approval_type_description_fr TEXT
               );")
  DBI::dbAppendTable(con, "approval_types", exist_table_approvals)
  
  # Create new table 'grade_types'. This is existing table 'grades' but with a new '_id' column.
  exist_table_grades <- DBI::dbGetQuery(con, "SELECT * FROM grades")
  names(exist_table_grades) <- c("grade_type_code", "grade_type_description", "grade_type_description_fr")
  
  exist_table_grades <- exist_table_grades[exist_table_grades$grade_type_code %in% c("A", "B", "C", "D", "N", "Z", "U"), ]
  exist_table_grades[exist_table_grades$grade_type_code == "Z" , "grade_type_code"] <- "UNK"
  exist_table_grades[exist_table_grades$grade_type_code == "U" , "grade_type_code"] <- "UNS"
  exist_table <- rbind(exist_table_grades, data.frame(grade_type_code = c("E", "MISS"), grade_type_description = c("Estimated", "Missing data"), grade_type_description_fr = c("Estimé", "Données manquantes")))
  
  DBI::dbExecute(con,
                 "CREATE TABLE public.grade_types (
               grade_type_id SERIAL PRIMARY KEY,
               grade_type_code TEXT NOT NULL UNIQUE,
               grade_type_description TEXT NOT NULL,
               grade_type_description_fr TEXT
               );")
  DBI::dbAppendTable(con, "grade_types", exist_table_grades)
  
  # Do a full replacement of 'approvals' column with the new 'approvals' table ########
  # Drop foreign key constraints on approvals for tables 'measurements_calculated_daily', 'rating_curves', 'measurements_continuous' so that CASCADE statements don't take effect
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP CONSTRAINT IF EXISTS fk_approval;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP CONSTRAINT IF EXISTS fk_approval;")
  DBI::dbExecute(con, "ALTER TABLE rating_curves DROP CONSTRAINT IF EXISTS rating_curves_approval_fkey;")
  DBI::dbExecute(con, "DROP VIEW measurements_continuous_corrected;")   # Will have to be recreated at some point
  DBI::dbExecute(con, "DROP VIEW measurements_hourly;")   # Will have to be recreated at some point
  DBI::dbExecute(con, "DROP FUNCTION IF EXISTS check_approval_exists_daily CASCADE;")
  DBI::dbExecute(con, "DROP FUNCTION IF EXISTS check_approval_exists_continuous CASCADE;")
  
  # Drop the existing table 'approvals'. Don't worry, its contents are now in approval_types!
  DBI::dbExecute(con, "DROP TABLE approvals;")
  
  # Create new table 'approvals' which will overwrite the existing 'approvals' table
  DBI::dbExecute(con,
                 "CREATE TABLE public.approvals (
               approval_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               approval_type_id INTEGER NOT NULL REFERENCES approval_types(approval_type_id) ON DELETE CASCADE ON UPDATE CASCADE,
               start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
               updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
               );")
  
  # Trigger to update the 'updated' column when a new approval is added
  DBI::dbExecute(con,
                 "CREATE TRIGGER update_approvals_updated
               BEFORE UPDATE
               ON public.approvals
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")
  
  # Make an index to speed things up later on as the table grows
  DBI::dbExecute(con, 
                 "CREATE INDEX idx_approvals_timeseries_time
               ON public.approvals (timeseries_id, start_dt, end_dt);")
  
  # Make a function and trigger that ensures that approvals do not overlap in time for a given timeseries_id
  DBI::dbExecute(con,
                 "CREATE OR REPLACE FUNCTION public.check_approvals_overlap()
                RETURNS TRIGGER
                LANGUAGE plpgsql
                AS $$
                BEGIN
                    IF EXISTS (
                        SELECT 1
                        FROM approvals
                        WHERE timeseries_id = NEW.timeseries_id
                        AND approval_id != NEW.approval_id  -- Exclude the current row in an UPDATE
                        AND (
                            (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                        )
                    ) THEN
                        RAISE EXCEPTION 'Approvals cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                    END IF;
                    RETURN NEW;
                END;
                $$;
              ")
  
  DBI::dbExecute(con,
                 "CREATE CONSTRAINT TRIGGER check_approvals_overlap
                AFTER INSERT OR UPDATE ON approvals
                DEFERRABLE INITIALLY DEFERRED
                FOR EACH ROW
                EXECUTE FUNCTION public.check_approvals_overlap();
              ")
  
  
  ## Now scan the tables 'measurements_continuous', 'measurements_calculated_daily'. These tables have a column called 'approval' that was linked to the old table 'approvals' - the letter in the 'approval' column now corresponds to the 'approval_type_code' in the new 'approval_types' table.
  approval_types <- DBI::dbGetQuery(con, "SELECT * FROM approval_types")
  
  
  tsids <- DBI::dbGetQuery(con, "SELECT DISTINCT timeseries_id FROM measurements_continuous;")[,1]
  for (i in tsids) {
    exist <- DBI::dbGetQuery(con, paste0("SELECT datetime, approval FROM measurements_continuous WHERE timeseries_id = ", i, " ORDER BY datetime;"))
    earliest_daily <- DBI::dbGetQuery(con, paste0("SELECT date AS datetime, approval FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date = (SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", i, ") AND value IS NOT NULL;"))
    earliest_daily$datetime <- as.POSIXct(earliest_daily$datetime, tz = "UTC")
    
    exist <- rbind(exist, earliest_daily[earliest_daily$datetime < min(exist$datetime),])
    exist[exist$approval == "U", "approval"] <- "UNS"
    exist[exist$approval == "Z", "approval"] <- "UNK"
    
    adjust_approval(con, i, exist[, c("datetime", "approval")])
  }
  
  # Now that all approvals live in a separate table, delete the approval column from 'measurements_continuous' and 'measurements_calculated_daily'
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN approval CASCADE;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN approval CASCADE;")
  
  
  ## The table 'rating_curves' also has a column called 'approval' that was linked to the old table 'approvals'. This column now needs to reference the 'approval_types' table, column 'approval_type_id'. This table might be empty!
  exist_table_rc <- DBI::dbGetQuery(con, "SELECT * FROM rating_curves")
  
  if (nrow(exist_table_rc) > 0) {
    if (length(unique(exist_table_rc$approval)) > 0) {
      for (i in unique(exist_table_rc$approval)) {
        new_id <- approval_types[approval_types$approval_type_code == i, "approval_type_id"]
        DBI::dbExecute(con, paste0("UPDATE rating_curves SET approval = ", new_id, " WHERE approval = '", i, "';"))
      }
    }
  }
  # change type of column 'approval' to integer and add foreign key constraint
  DBI::dbExecute(con, "ALTER TABLE rating_curves ALTER COLUMN approval TYPE INTEGER USING approval::INTEGER;")
  DBI::dbExecute(con, "ALTER TABLE rating_curves ADD CONSTRAINT rating_curves_approval_fkey FOREIGN KEY (approval) REFERENCES approval_types(approval_type_id) ON DELETE CASCADE ON UPDATE CASCADE;")
  
  
  message("Working on grades tables")
  # Do a full replacement of 'grades' column with the new 'grades' table ########
  # Drop foreign key constraints on grades for tables 'measurements_calculated_daily', 'measurements_continuous' so that CASCADE statements don't take effect
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP CONSTRAINT IF EXISTS fk_grade;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP CONSTRAINT IF EXISTS fk_grade;")
  DBI::dbExecute(con, "DROP FUNCTION IF EXISTS check_grade_exists_daily CASCADE;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_xsections DROP CONSTRAINT IF EXISTS locations_metadata_xsections_measurement_grade_fkey;")
  DBI::dbExecute(con, "DROP FUNCTION IF EXISTS check_grade_exists_daily CASCADE;")
  DBI::dbExecute(con, "DROP FUNCTION IF EXISTS check_grade_exists_continuous CASCADE;")
  
  # add new foreign key to locations_metadata_xsections linking measurement_grade to table grade_types
  # Change column type to integer
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_xsections ALTER COLUMN measurement_grade TYPE INTEGER USING measurement_grade::INTEGER;")
  DBI::dbExecute(con, "ALTER TABLE locations_metadata_xsections ADD CONSTRAINT locations_metadata_xsections_grade_fkey FOREIGN KEY (measurement_grade) REFERENCES grade_types(grade_type_id) ON DELETE CASCADE ON UPDATE CASCADE;")
  
  
  # Drop the existing table 'grades'
  DBI::dbExecute(con, "DROP TABLE grades;")
  
  # Create new table 'grades' which will overwrite the existing 'grades' table
  DBI::dbExecute(con,
                 "CREATE TABLE public.grades (
               grade_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               grade_type_id INTEGER NOT NULL REFERENCES grade_types(grade_type_id) ON DELETE CASCADE ON UPDATE CASCADE,
               start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
               updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
               );")
  # Trigger to update the 'updated' column when a new grade is added
  DBI::dbExecute(con,
                 "CREATE TRIGGER update_grades_updated
               BEFORE UPDATE
               ON public.grades
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")
  
  # Make an index to speed things up later on as the table grows
  DBI::dbExecute(con, 
                 "CREATE INDEX idx_grades_timeseries_time
                ON public.grades (timeseries_id, start_dt, end_dt);
                ")
  
  # Make a function and trigger that ensures that grades do not overlap in time for a given timeseries_id
  DBI::dbExecute(con,
                 "CREATE OR REPLACE FUNCTION public.check_grades_overlap()
              RETURNS TRIGGER
              LANGUAGE plpgsql
              AS $$
              BEGIN
                  IF EXISTS (
                      SELECT 1
                      FROM grades
                      WHERE timeseries_id = NEW.timeseries_id
                      AND grade_id != NEW.grade_id  -- Exclude the current row in an UPDATE
                      AND (
                          (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                      )
                  ) THEN
                      RAISE EXCEPTION 'Grades cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                  END IF;
                  RETURN NEW;
              END;
              $$;
              ")
  DBI::dbExecute(con,
                 "CREATE CONSTRAINT TRIGGER check_grades_overlap
                AFTER INSERT OR UPDATE ON grades
                DEFERRABLE INITIALLY DEFERRED
                FOR EACH ROW
                EXECUTE FUNCTION public.check_grades_overlap();
              ")
  
  
  tsids <- DBI::dbGetQuery(con, "SELECT DISTINCT timeseries_id FROM measurements_continuous;")[,1]
  grade_types <- DBI::dbGetQuery(con, "SELECT * FROM grade_types")
  for (i in tsids) {
    exist <- DBI::dbGetQuery(con, paste0("SELECT datetime, grade FROM measurements_continuous WHERE timeseries_id = ", i, " ORDER BY datetime;"))
    exist_daily <- DBI::dbGetQuery(con, paste0("SELECT date AS datetime, grade FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(exist$datetime), "' AND value IS NOT NULL;"))
    exist_daily$datetime <- as.POSIXct(exist_daily$datetime, tz = "UTC")
    
    exist <- rbind(exist, exist_daily)
    
    # Some grades no longer have an equivalent! In particular, E, I, R, S are now qualifiers
    qualifiers_to_port <- exist[exist$grade %in% c("E", "I", "R", "S"),]
    names(qualifiers_to_port) <- c("datetime", "qualifier")
    qualifiers_to_port[qualifiers_to_port$qualifier == "E", "qualifier"] <- "EST"
    qualifiers_to_port[qualifiers_to_port$qualifier == "I", "qualifier"] <- "ICE"
    qualifiers_to_port[qualifiers_to_port$qualifier == "R", "qualifier"] <- "DD"
    qualifiers_to_port[qualifiers_to_port$qualifier == "S", "qualifier"] <- "SUS"
    
    exist[exist$grade %in% c("E", "I", "R", "S"), "grade"] <- "UNS"
    
    if (nrow(qualifiers_to_port) > 0) {
      adjust_qualifier(con, i, qualifiers_to_port[, c("datetime", "qualifier")])
    }
    
    source_fx <- DBI::dbGetQuery(con, paste0("SELECT source_fx FROM timeseries WHERE timeseries_id = ", i, ";"))[1,1]
    if (!is.na(source_fx)) {
      if (source_fx == "downloadWSC") {
        exist$grade <- exist[exist$grade == "U", "grade"] <- "UNS"
      }
    }

    exist[is.na(exist$grade), "grade"] <- "UNK"
    exist[exist$grade == "U", "grade"] <- "UNS"
    exist[exist$grade == "Z", "grade"] <- "UNK"
    
    adjust_grade(con, i, exist[, c("datetime", "grade")])
  }
  
  # Now that all grades live in a separate table, delete the grade column from 'measurements_continuous' and 'measurements_calculated_daily'
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN grade CASCADE;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN grade CASCADE;")
  
  
  # Do a full replacement of 'owner' column with the owners_contributors table ########
  message("Working on owners tables")
  # Drop foreign key constraints on owner for tables 'measurements_calculated_daily', 'measurements_continuous' so that CASCADE statements don't take effect
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP CONSTRAINT IF EXISTS measurements_continuous_owner_fkey;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP CONSTRAINT IF EXISTS calculated_daily_owner_fkey;")
  
  # Create new table 'owners'
  DBI::dbExecute(con,
                 "CREATE TABLE public.owners (
               owner_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               owner_contributor_id INTEGER NOT NULL REFERENCES owners_contributors(owner_contributor_id) ON DELETE CASCADE ON UPDATE CASCADE,
               start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
               updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
               );")
  # Trigger to update the 'updated' column when a new owner is added
  DBI::dbExecute(con,
                 "CREATE TRIGGER update_owners_updated
               BEFORE UPDATE
               ON public.owners
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")
  
  # Make an index to speed things up later on as the table grows
  DBI::dbExecute(con, 
                 "CREATE INDEX idx_owners_timeseries_time
                ON public.owners (timeseries_id, start_dt, end_dt);
                ")
  
  # Make a function and trigger that ensures that owners do not overlap in time for a given timeseries_id
  DBI::dbExecute(con,
                 "CREATE OR REPLACE FUNCTION public.check_owners_overlap()
              RETURNS TRIGGER
              LANGUAGE plpgsql
              AS $$
              BEGIN
                  IF EXISTS (
                      SELECT 1
                      FROM owners
                      WHERE timeseries_id = NEW.timeseries_id
                      AND owner_contributor_id != NEW.owner_contributor_id  -- Exclude the current row in an UPDATE
                      AND (
                          (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                      )
                  ) THEN
                      RAISE EXCEPTION 'Owners cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                  END IF;
                  RETURN NEW;
              END;
              $$;
              ")
  DBI::dbExecute(con,
                 "CREATE CONSTRAINT TRIGGER check_owners_overlap
                AFTER INSERT OR UPDATE ON owners
                DEFERRABLE INITIALLY DEFERRED
                FOR EACH ROW
                EXECUTE FUNCTION public.check_owners_overlap();
              ")
  
  
  wsc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada';")[1,1]
  if (is.na(wsc_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "Water Survey of Canada"))
    wsc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada';")[1,1]
  }
  wrb_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Yukon Department of Environment, Water Resources Branch';")[1,1]
  if (is.na(wrb_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "Yukon Department of Environment, Water Resources Branch"))
    wrb_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Yukon Department of Environment, Water Resources Branch';")[1,1]
  }
  eccc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Environment and Climate Change Canada';")[1,1]
  if (is.na(eccc_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "Environment and Climate Change Canada"))
    eccc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Environment and Climate Change Canada';")[1,1]
  }
  usgs_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'United States Geological Survey';")[1,1]
  if (is.na(usgs_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "United States Geological Survey"))
    usgs_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'United States Geological Survey';")[1,1]
  }
  
  tsids <- DBI::dbGetQuery(con, "SELECT DISTINCT timeseries_id FROM measurements_continuous;")[,1]
  for (i in tsids) {
    exist <- DBI::dbGetQuery(con, paste0("SELECT datetime, owner FROM measurements_continuous WHERE timeseries_id = ", i, " ORDER BY datetime;"))
    earliest_daily <- DBI::dbGetQuery(con, paste0("SELECT date AS datetime, owner FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(exist$datetime), "' AND value IS NOT NULL;"))
    earliest_daily$datetime <- as.POSIXct(earliest_daily$datetime, tz = "UTC")
    
    exist <- rbind(exist, earliest_daily)
    
    
    source_fx <- DBI::dbGetQuery(con, paste0("SELECT source_fx FROM timeseries WHERE timeseries_id = ", i, ";"))[1,1]
    if (is.na(source_fx)) {
      next
    }
    owner <- DBI::dbGetQuery(con, paste0("SELECT owner FROM timeseries WHERE timeseries_id = ", i, ";"))[1,1]
    if (is.na(source_fx)) {
      next
    }
    if (source_fx == "downloadWSC") {
      exist$contributor <- wsc_owner
      if (is.na(owner)) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET owner = ", wsc_owner, " WHERE timeseries_id = ", i, ";"))
      }
    } else if (source_fx == "downloadAquarius") {
      exist$contributor <- wrb_owner
      if (is.na(owner)) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET owner = ", wrb_owner, " WHERE timeseries_id = ", i, ";"))
      }
    } else if (source_fx == "downloadECCCwx") {
      exist$contributor <- eccc_owner
      if (is.na(owner)) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET owner = ", eccc_owner, " WHERE timeseries_id = ", i, ";"))
      }
    } else if (source_fx == "downloadNWIS") {
      exist$contributor <- usgs_owner
      if (is.na(owner)) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET owner = ", usgs_owner, " WHERE timeseries_id = ", i, ";"))
      }
    }
    
    adjust_owner(con, i, exist[, c("datetime", "owner")])
  }
  
  # Now that all owners live in a separate table, delete the owner column from 'measurements_continuous' and 'measurements_calculated_daily'
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN owner CASCADE;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN owner CASCADE;")
  
  
  # Do a full replacement of 'contributor' column with the owners_contributors table ########
  message("Working on contributors tables")
  # Drop foreign key constraints on contributors for tables 'measurements_calculated_daily', 'measurements_continuous' so that CASCADE statements don't take effect
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP CONSTRAINT IF EXISTS measurements_continuous_contributor_fkey;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP CONSTRAINT IF EXISTS calculated_daily_contributor_fkey;")
  
  # Create new table 'contributors'
  DBI::dbExecute(con,
                 "CREATE TABLE public.contributors (
               contributor_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               owner_contributor_id INTEGER NOT NULL REFERENCES owners_contributors(owner_contributor_id) ON DELETE CASCADE ON UPDATE CASCADE,
               start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
               updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
               );")
  # Trigger to update the 'updated' column when a new contributor is added
  DBI::dbExecute(con,
                 "CREATE TRIGGER update_contributors_updated
               BEFORE UPDATE
               ON public.contributors
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")
  
  # Make an index to speed things up later on as the table grows
  DBI::dbExecute(con, 
                 "CREATE INDEX idx_contributors_timeseries_time
                ON public.contributors (timeseries_id, start_dt, end_dt);
                ")
  
  # Make a function and trigger that ensures that contributors do not overlap in time for a given timeseries_id
  DBI::dbExecute(con,
                 "CREATE OR REPLACE FUNCTION public.check_contributors_overlap()
              RETURNS TRIGGER
              LANGUAGE plpgsql
              AS $$
              BEGIN
                  IF EXISTS (
                      SELECT 1
                      FROM contributors
                      WHERE timeseries_id = NEW.timeseries_id
                      AND owner_contributor_id != NEW.owner_contributor_id  -- Exclude the current row in an UPDATE
                      AND (
                          (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                      )
                  ) THEN
                      RAISE EXCEPTION 'Contributors cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                  END IF;
                  RETURN NEW;
              END;
              $$;
              ")
  DBI::dbExecute(con,
                 "CREATE CONSTRAINT TRIGGER check_contributors_overlap
                AFTER INSERT OR UPDATE ON contributors
                DEFERRABLE INITIALLY DEFERRED
                FOR EACH ROW
                EXECUTE FUNCTION public.check_contributors_overlap();
              ")
  
  wsc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada';")[1,1]
  if (is.na(wsc_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "Water Survey of Canada"))
    wsc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada';")[1,1]
  }
  wrb_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Yukon Department of Environment, Water Resources Branch';")[1,1]
  if (is.na(wrb_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "Yukon Department of Environment, Water Resources Branch"))
    wrb_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Yukon Department of Environment, Water Resources Branch';")[1,1]
  }
  eccc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Environment and Climate Change Canada';")[1,1]
  if (is.na(eccc_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "Environment and Climate Change Canada"))
    eccc_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Environment and Climate Change Canada';")[1,1]
  }
  usgs_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'United States Geological Survey';")[1,1]
  if (is.na(usgs_owner)) {
    DBI::dbAppendTable(con, "owners_contributors", data.frame(name = "United States Geological Survey"))
    usgs_owner <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'United States Geological Survey';")[1,1]
  }
  
  tsids <- DBI::dbGetQuery(con, "SELECT DISTINCT timeseries_id FROM measurements_continuous;")[,1]
  for (i in tsids) {
    exist <- DBI::dbGetQuery(con, paste0("SELECT datetime, contributor FROM measurements_continuous WHERE timeseries_id = ", i, " ORDER BY datetime;"))
    earliest_daily <- DBI::dbGetQuery(con, paste0("SELECT date AS datetime, contributor FROM measurements_calculated_daily WHERE timeseries_id = ", i, " AND date < '", min(exist$datetime), "' AND value IS NOT NULL;"))
    earliest_daily$datetime <- as.POSIXct(earliest_daily$datetime, tz = "UTC")
    
    exist <- rbind(exist, earliest_daily)
    source_fx <- DBI::dbGetQuery(con, paste0("SELECT source_fx FROM timeseries WHERE timeseries_id = ", i, ";"))[1,1]
    if (is.na(source_fx)) {
      next
    }
    if (source_fx == "downloadWSC") {
      exist$contributor <- wsc_owner
    } else if (source_fx == "downloadAquarius") {
      exist$contributor <- wrb_owner
    } else if (source_fx == "downloadECCCwx") {
      exist$contributor <- eccc_owner
    } else if (source_fx == "downloadNWIS") {
      exist$contributor <- usgs_owner
    }
    
    adjust_contributor(con, i, exist[, c("datetime", "contributor")])
  }
  
  # Now that all contributors live in a separate table, delete the contributor column from 'measurements_continuous' and 'measurements_calculated_daily'
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN contributor CASCADE;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN contributor CASCADE;")
  
  
#   # Re-create the view tables that were dropped earlier and make some new ones #################
#   message("Re-creating view tables (and adding a few new ones)")
#   
  # measurements_continuous_corrected table
  DBI::dbExecute(con, "
CREATE OR REPLACE VIEW measurements_continuous_corrected AS
SELECT
    mc.timeseries_id,
    mc.datetime,
    mc.value AS value_raw,
    ac.value_corrected,
    mc.period,
    mc.imputed,
    mc.share_with
FROM
    measurements_continuous mc
CROSS JOIN LATERAL (
    SELECT apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
) ac
WHERE
    ac.value_corrected IS NOT NULL;
")

  # measurements_continuous_corrected_hourly table
  DBI::dbExecute(con, "
  CREATE OR REPLACE VIEW measurements_continuous_corrected_hourly AS
SELECT
    mcc.timeseries_id,
    date_trunc('hour', mcc.datetime) AS datetime,
    AVG(mcc.value_raw) AS value_raw,
    AVG(mcc.value_corrected) AS value_corrected,
    BOOL_OR(mcc.imputed) AS imputed
FROM
    measurements_continuous_corrected mcc
GROUP BY
    mcc.timeseries_id,
    date_trunc('hour', mcc.datetime)
ORDER BY
    datetime;
")
  
  
  # Wrap up #################
  message("Wrapping up with a few tweaks...")
  
  # Fix some functions in the metadata tables that cause problems...
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_infrastructure_missing_trigger
              BEFORE INSERT ON locations_metadata_infrastructure
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_infrastructure_missing();
")
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_infrastructure_hydromet_trigger
              BEFORE INSERT ON locations_metadata_infrastructure_hydromet
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_infrastructure_hydromet();
")
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_infrastructure_groundwater_trigger
              BEFORE INSERT ON locations_metadata_infrastructure_groundwater
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_infrastructure_groundwater();
")
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_owners_operators_missing_trigger
              BEFORE INSERT ON locations_metadata_owners_operators
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_owners_operators_missing();
")
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_access_missing_trigger
              BEFORE INSERT ON locations_metadata_access
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_access_missing();
")
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_transmission_missing_trigger
              BEFORE INSERT ON locations_metadata_transmission
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_transmission_missing();
")
  DBI::dbExecute(con, "
              CREATE OR REPLACE TRIGGER fill_locations_metadata_acquisition_missing_trigger
              BEFORE INSERT ON locations_metadata_acquisition
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_acquisition_missing();
")
  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '3' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  message("Patch 3 applied successfully: qualifiers, grades, approvals, owners, and contributors are now stored in their own tables. A full VACUUM ANALYZE is recommended: from R, use function vacuum() from this package.")
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 3 failed and the DB has been rolled back to its earlier state. ", e$message)
})
