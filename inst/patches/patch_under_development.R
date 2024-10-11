# Patch 3.
# Creates new table 'qualifier' to hold information such as 'ice affected', 'estimated', 'draw-down', etc. for data quality control.

# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")

if (!check$can_create) {
  stop("You do not have the necessary privileges to create a new schema in this database.")
}

tbls <- DBI::dbListTables(con)
needs_update <- TRUE
if ("qualifier" %in% tbls) {
  # Check to see if the table contains the expected columns
  cols <- DBI::dbListFields(con, "qualifier")
  if (!all(c("qualifier_id", "qualifier_code", "qualifier_description") %in% cols)) {
    warning("Table 'qualifier' exists but does not contain the expected columns. Replacing it with the correct table structure.")
    DBI::dbExecute(con, "DROP TABLE qualifier;")
  } else {
    # Check to see if the table contains at minimum entries in qualifier_code for 'ice', 'dry', 'susp', 'est', 'dd', 'bw' 
    codes <- DBI::dbGetQuery(con, "SELECT qualifier_code FROM qualifier;")
    if (!all(c("ice", "dry", "susp", "est", "dd", "bw") %in% codes$qualifier_code)) {
      warning("Table 'qualifier' exists but does not contain the expected entries in qualifier_code. Replacing it with the correct table structure.")
      DBI::dbExecute(con, "DROP TABLE qualifier;")
    }
  }
}

if (needs_update) {
  DBI::dbExecute(con, "CREATE TABLE public.qualifier_types (
  qualifier_type_id SERIAL PRIMARY KEY,
  qualifier_type_code TEXT NOT NULL UNIQUE,
  qualifier_type_description TEXT
);")
  
  qualifiers <- data.frame(
    qualifier_type_code = c("ice", "dry", "susp", "est", "dd", "bw", "uns", "unk"),
    qualifier_type_description = c("Ice affected", "Sensor out of water or intermitendly out of water", "Suspect measurements", "Estimated", "Draw-down after pumping", "Backwater affecting measurements", "Unspecified", "Unknown")
  )
  DBI::dbAppendTable(con, "qualifier_types", qualifiers)
}

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
    if (!all(c("ice", "dry", "susp", "est", "dd", "bw") %in% codes$qualifier_type_code)) {
      stop("Table 'qualifier_types' was created but does not contain the expected entries in qualifier_type_code. Please check the database and try again.")
    }
  }
}

# Now make a table 'qualifiers' and check functions to apply qualifiers for timeseries and time ranges, similar to table 'corrections'
DBI::dbExecute(con,
               "CREATE TABLE public.qualifiers (
               qualifier_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               qualifier_type_id INTEGER NOT NULL REFERENCES qualifier_types(qualifier_type_id) ON DELETE SET NULL ON UPDATE CASCADE,
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
        RAISE EXCEPTION 'Qualifiers cannot overlap in time for the same timeseries_id.';
    END IF;
    RETURN NEW;
END;
$$;
")

DBI::dbExecute(con,
"CREATE CONSTRAINT TRIGGER check_qualifiers_overlap
AFTER INSERT OR UPDATE ON qualifiers
DEFERRABLE INITIALLY IMMEDIATE
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
               "CREATE TRIGGER update_qualifiers_updated
               BEFORE UPDATE
               ON public.qualifiers
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")

# While we're at it let's do the same thing for table 'corrections' since it isn't done already...
DBI::dbExecute(con,
               "CREATE TRIGGER update_corrections_updated
               BEFORE UPDATE
               ON public.corrections
               FOR EACH ROW
               EXECUTE FUNCTION public.update_updated();")



# Now let's apply the same principle to the 'approvals' and 'grades' tables.
## Approvals
# Create new table 'approval_types'. This is existing table 'approvals' but with a new '_id' column.
exist_table_approvals <- DBI::dbGetQuery(con, "SELECT * FROM approvals")
names(exist_table_approvals) <- c("approval_type_code", "approval_type_description", "approval_type_description_fr")

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

DBI::dbExecute(con,
               "CREATE TABLE public.grade_types (
               grade_type_id SERIAL PRIMARY KEY,
               grade_type_code TEXT NOT NULL UNIQUE,
               grade_type_description TEXT NOT NULL,
               grade_type_description_fr TEXT
               );")
DBI::dbAppendTable(con, "grade_types", exist_table_grades)


#####################################################
# Stopped here!!!

# Do a full replacement of 'approvals' column with the new 'approvals' table ########
# Drop foreign key constraints on approvals for tables 'measurements_calculated_daily', 'rating_curves', 'measurements_continuous' so that CASCADE statements don't take effect
DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP CONSTRAINT IF EXISTS fk_approval;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP CONSTRAINT IF EXISTS fk_approval;")
DBI::dbExecute(con, "ALTER TABLE rating_curves DROP CONSTRAINT IF EXISTS rating_curves_approval_fkey;")

# Drop the existing table 'approvals'
DBI::dbExecute(con, "DROP TABLE approvals;")

# Create new table 'approvals' which will overwrite the existing 'approvals' table
DBI::dbExecute(con,
               "CREATE TABLE public.approvals (
               approval_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               approval_type_id INTEGER NOT NULL REFERENCES approval_types(approval_type_id) ON DELETE SET NULL ON UPDATE CASCADE,
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
ON public.approvals (timeseries_id, start_dt, end_dt);
")

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
        AND qualifier_id != NEW.qualifier_id  -- Exclude the current row in an UPDATE
        AND (
            (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
        )
    ) THEN
        RAISE EXCEPTION 'Approvals cannot overlap in time for the same timeseries_id.';
    END IF;
    RETURN NEW;
END;
$$;
")

DBI::dbExecute(con,
"CREATE CONSTRAINT TRIGGER check_approvals_overlap
AFTER INSERT OR UPDATE ON approvals
DEFERRABLE INITIALLY IMMEDIATE
FOR EACH ROW
EXECUTE FUNCTION public.check_approvals_overlap();
")


## Now scan the tables 'measurements_continuous', 'measurements_calculated_daily'. These tables have a column called 'approval' that was linked to the old table 'approvals' - the letter in the 'approval' column now corresponds to the 'approval_type_code' in the new 'approval_types' table.
# For each timeseries_id we need to make entries to table 'approvals' with start + end times of each approval change.
approval_types <- DBI::dbGetQuery(con, "SELECT * FROM approval_types")

tsids <- DBI::dbGetQuery(con, "SELECT DISTINCT timeseries_id FROM measurements_continuous;")[,1]
for (i in tsids) {
  exist <- DBI::dbGetQuery(con, paste0("SELECT datetime, approval, value FROM measurements_continuous WHERE timeseries_id = ", i, " ORDER BY datetime;"))
  earliest_hydat <- as.POSIXct(DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", i, ";"))[1,1], tz = "UTC")
  
  # Initialize a data frame to hold approval ranges for insertion into 'approvals'
  approvals <- data.frame(
    timeseries_id = integer(),
    approval_type_id = integer(),
    start_dt = as.POSIXct(character(), tzone = "UTC"),
    end_dt = as.POSIXct(character(), tzone = "UTC")
  )
  
  # If earliest_hydat is earlier than the first datetime in exist, assign approval 'A' for that period
  if (!is.na(earliest_hydat) && earliest_hydat < exist$datetime[1]) {
    approvals <- rbind(approvals, data.frame(
      timeseries_id = i,
      approval_type_id = "A",
      start_dt = earliest_hydat,
      end_dt = as.POSIXct(NA, tzone = "UTC")
    ))
    attr(approvals$end_dt, "tzone") <- "UTC"
  } else {
    approvals <- rbind(approvals, data.frame(
      timeseries_id = i,
      approval_type_id = exist$approval[1], 
      start_dt = exist$datetime[1],
      end_dt = as.POSIXct(NA, tzone = "UTC")
    ))
    attr(approvals$end_dt, "tzone") <- "UTC"
  }
  current_approval <- approvals$approval_type_id[1]
  
  for (j in 1:nrow(exist)) {
    if (exist$approval[j] != current_approval) {
      # print(j)
      # break()
      # There is a change in approval
      current_approval <- exist$approval[j]
      # End datetime is the last datetime before the change
      end_dt <- exist$datetime[j - 1]
      approvals[nrow(approvals), "end_dt"] <- end_dt
      
      # Make a new entry to 'approvals' for the new approval
      new <- data.frame(
        timeseries_id = i,
        approval_type_id = current_approval, 
        start_dt = exist$datetime[j],
        end_dt = as.POSIXct(NA)
      )
      attr(new$end_dt, "tzone") <- "UTC"
      approvals <- rbind(approvals, new)
    }
    if (j == nrow(exist)) {
      approvals[nrow(approvals), "end_dt"] <- exist$datetime[j]
    }
  }
  
  # Map 'approval_type_id' to the actual id in the 'approval_types' table
  for (k in 1:nrow(approvals)) {
    new_id <- approval_types[approval_types$approval_type_code == approvals$approval_type_id[k], "approval_type_id"]
    approvals$approval_type_id[k] <- new_id
  }
  approvals$approval_type_id <- as.integer(approvals$approval_type_id)
  
  # Insert approvals into the 'approvals' table in the database
  DBI::dbWriteTable(con, 'approvals', approvals, append = TRUE, row.names = FALSE)
}

# Now that all approvals live in a separate table, delete the approval column from 'measurements_continuous' and 'measurements_calculated_daily'
DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN approval;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN approval;")


## The table 'rating_curves' also has a column called 'approval' that was linked to the old table 'approvals'. This column now needs to reference the 'approval_types' table, column 'approval_type_id'. This table might be empty!
exist_table_rc <- DBI::dbGetQuery(con, "SELECT * FROM rating_curves")
approval_types <- DBI::dbGetQuery(con, "SELECT * FROM approval_types")

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
DBI::dbExecute(con, "ALTER TABLE rating_curves ADD CONSTRAINT rating_curves_approval_fkey FOREIGN KEY (approval) REFERENCES approval_types(approval_type_id) ON DELETE SET NULL ON UPDATE CASCADE;")



# Do a full replacement of 'grades' column with the new 'grades' table ########
# Drop foreign key constraints on approvals for tables 'measurements_calculated_daily', 'rating_curves', 'measurements_continuous' so that CASCADE statements don't take effect
DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP CONSTRAINT IF EXISTS fk_grade;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP CONSTRAINT IF EXISTS fk_grade;")

# Drop the existing table 'grades'
DBI::dbExecute(con, "DROP TABLE grades;")

# Create new table 'grades' which will overwrite the existing 'grades' table
DBI::dbExecute(con,
               "CREATE TABLE public.grades (
               grade_id SERIAL PRIMARY KEY,
               timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
               grade_type_id INTEGER NOT NULL REFERENCES grade_types(grade_type_id) ON DELETE SET NULL ON UPDATE CASCADE,
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
        AND qualifier_id != NEW.qualifier_id  -- Exclude the current row in an UPDATE
        AND (
            (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
        )
    ) THEN
        RAISE EXCEPTION 'grades cannot overlap in time for the same timeseries_id.';
    END IF;
    RETURN NEW;
END;
$$;
")
DBI::dbExecute(con,
               "CREATE CONSTRAINT TRIGGER check_grades_overlap
AFTER INSERT OR UPDATE ON grades
DEFERRABLE INITIALLY IMMEDIATE
FOR EACH ROW
EXECUTE FUNCTION public.check_grades_overlap();
")


## Now scan the tables 'measurements_continuous', 'measurements_calculated_daily'. These tables have a column called 'grade' that was linked to the old table 'grades' - the letter in the 'grade' column now corresponds to the 'grade_type_code' in the new 'grade_types' table.
# For each timeseries_id we need to make entries to table 'grades' with start + end times of each grade change.
grade_types <- DBI::dbGetQuery(con, "SELECT * FROM grade_types")

tsids <- DBI::dbGetQuery(con, "SELECT DISTINCT timeseries_id FROM measurements_continuous;")[,1]
for (i in tsids) {
  exist <- DBI::dbGetQuery(con, paste0("SELECT datetime, grade, value FROM measurements_continuous WHERE timeseries_id = ", i, " ORDER BY datetime;"))
  earliest_hydat <- as.POSIXct(DBI::dbGetQuery(con, paste0("SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", i, ";"))[1,1], tz = "UTC")
  is_wsc <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE timeseries_id = ", i, " AND source+fx = 'downloadWSC';"))[1,1]
  
  # Initialize a data frame to hold grade ranges for insertion into 'grades'
  grades <- data.frame(
    timeseries_id = integer(),
    grade_type_id = integer(),
    start_dt = as.POSIXct(character(), tzone = "UTC"),
    end_dt = as.POSIXct(character(), tzone = "UTC")
  )
  
  if (is.na(is_wsc)) { # Then no grades are applied by the WSC, just approvals and qualifiers.
    # If earliest_hydat is earlier than the first datetime in exist, assign grade 'A' for that period
    if (!is.na(earliest_hydat) && earliest_hydat < exist$datetime[1]) {
      grades <- rbind(grades, data.frame(
        timeseries_id = i,
        grade_type_id = "A",
        start_dt = earliest_hydat,
        end_dt = as.POSIXct(NA, tzone = "UTC")
      ))
      attr(grades$end_dt, "tzone") <- "UTC"
    } else {
      grades <- rbind(grades, data.frame(
        timeseries_id = i,
        grade_type_id = exist$grade[1], 
        start_dt = exist$datetime[1],
        end_dt = as.POSIXct(NA, tzone = "UTC")
      ))
      attr(grades$end_dt, "tzone") <- "UTC"
    }
    current_grade <- grades$grade_type_id[1]
    
    for (j in 1:nrow(exist)) {
      if (exist$grade[j] != current_grade) {
        # print(j)
        # break()
        # There is a change in grade
        current_grade <- exist$grade[j]
        # End datetime is the last datetime before the change
        end_dt <- exist$datetime[j - 1]
        grades[nrow(grades), "end_dt"] <- end_dt
        
        # Make a new entry to 'grades' for the new grade
        new <- data.frame(
          timeseries_id = i,
          grade_type_id = current_grade, 
          start_dt = exist$datetime[j],
          end_dt = as.POSIXct(NA)
        )
        attr(new$end_dt, "tzone") <- "UTC"
        grades <- rbind(grades, new)
      }
      if (j == nrow(exist)) {
        grades[nrow(grades), "end_dt"] <- exist$datetime[j]
      }
    }
  } else { # WSC timeseries have no defined gardes
    grades <- rbind(grades, data.frame(
      timeseries_id = i,
      grade_type_id = "A",
      start_dt = earliest_hydat,
      end_dt = max(exist$datetime)
    ))
  }
  
  # Map 'grade_type_id' to the actual id in the 'grade_types' table
  for (k in 1:nrow(grades)) {
    new_id <- grade_types[grade_types$grade_type_code == grades$grade_type_id[k], "grade_type_id"]
    grades$grade_type_id[k] <- new_id
  }
  grades$grade_type_id <- as.integer(grades$grade_type_id)
  
  # Insert grades into the 'grades' table in the database
  DBI::dbWriteTable(con, 'grades', grades, append = TRUE, row.names = FALSE)
}

# Now that all grades live in a separate table, delete the grade column from 'measurements_continuous' and 'measurements_calculated_daily'
DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN grade;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN grade;")




# Do the same for owner, contributor #################


# Wrap up #################
# Update the version_info table
DBI::dbExecute(con, "UPDATE information.version_info SET version = '3' WHERE item = 'Last patch number';")
DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))

message("Patch 3 applied successfully: new table for qualifiers created.")
