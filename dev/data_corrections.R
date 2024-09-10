# Add table to hold corrections. These can apply to anything with a timeseries_id. A view table will be created to apply these corrections to the data and to let users extract corrected data

DBI::dbExecute(con, "CREATE TABLE correction_types (
  correction_type_id SERIAL PRIMARY KEY,
  correction_type TEXT NOT NULL,
  description TEXT NOT NULL,
  priority INTEGER NOT NULL,
  value1 BOOLEAN NOT NULL,
  value2 BOOLEAN NOT NULL,
  timestep_window BOOLEAN NOT NULL,
  equation TEXT,
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (priority)
)")

correction_types <- data.frame(
  correction_type = c("trim",
                      "delete",
                      "drift linear",
                      "drift equation",
                      "offset linear",
                      "offset two-point",
                      "rolling average smoothing",
                      "scale"),
  description = c("Remove data points outside of a specified value range",
                  "Remove data points within a specified datetime range",
                  "Apply a linear drift correction",
                  "Apply a drift correction based on an equation",
                  "Apply a linear offset correction",
                  "Apply a two-point offset correction",
                  "Apply a rolling average smoothing correction",
                  "Apply a percent scaling correction"),
  priority = c(2, 1, 5, 6, 3, 4, 8, 7),
  value1 = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
  value1_description = c("Value below which to discard data", 
                         NA, 
                         "Slope for drift linear (depends on timestep_window)", 
                         "Equation for drift (depends on timestep_window)", 
                         "Shift for linear offset, first point for two point offset", 
                         "Second point for two point offset",
                         NA,
                         "Scaling factor (%) to apply to data"),
  value2 = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
  value2_description = c("Value above which to discard data", 
                         NA, 
                         NA, 
                         NA, 
                         NA, 
                         "Second point for two point offset",
                         NA,
                         NA),
  timestep_window = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
)

DBI::dbAppendTable(con, "correction_types", correction_types)

DBI::dbExecute(con, "CREATE TABLE measurements_corrections (
  correction_id SERIAL PRIMARY KEY,
  timeseries_id INTEGER NOT NULL REFERENCES timeseries (timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE,
  start_dt DATE NOT NULL,
  end_dt DATE NOT NULL,
  correction_type INTEGER NOT NULL REFERENCES correction_types(correction_type_id) ON UPDATE CASCADE ON DELETE CASCADE
  value1 NUMERIC, # Only optional for 'delete' correction type
  value2 NUMERIC,  # Optional second value for two-point offset correction
  timestep_window INTERVAL,  # Applies to rolling average smoothing (window) as well as drift corrections (time step)
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_by TEXT NOT NULL,
  modified_by TEXT,
  UNIQUE (timeseries_id, start_dt, end_dt, correction_type)
)")

# Make function and trigger to ensure value1, value2, timestep_window are appropriately populated based on correction_type
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION validate_measurements_corrections()
RETURNS TRIGGER AS $$
BEGIN
  -- Fetch the correction type definition from correction_types
  DECLARE
    v1_required BOOLEAN;
    v2_required BOOLEAN;
    timestep_required BOOLEAN;
  BEGIN
    SELECT value1, value2, timestep_window
    INTO v1_required, v2_required, timestep_required
    FROM correction_types
    WHERE correction_type_id = NEW.correction_type;

    -- Check value1
    IF v1_required AND NEW.value1 IS NULL THEN
      RAISE EXCEPTION 'value1 cannot be NULL for correction_type_id %', NEW.correction_type;
    ELSIF NOT v1_required AND NEW.value1 IS NOT NULL THEN
      RAISE EXCEPTION 'value1 must be NULL for correction_type_id %', NEW.correction_type;
    END IF;

    -- Check value2
    IF v2_required AND NEW.value2 IS NULL THEN
      RAISE EXCEPTION 'value2 cannot be NULL for correction_type_id %', NEW.correction_type;
    ELSIF NOT v2_required AND NEW.value2 IS NOT NULL THEN
      RAISE EXCEPTION 'value2 must be NULL for correction_type_id %', NEW.correction_type;
    END IF;

    -- Check timestep_window
    IF timestep_required AND NEW.timestep_window IS NULL THEN
      RAISE EXCEPTION 'timestep_window cannot be NULL for correction_type_id %', NEW.correction_type;
    ELSIF NOT timestep_required AND NEW.timestep_window IS NOT NULL THEN
      RAISE EXCEPTION 'timestep_window must be NULL for correction_type_id %', NEW.correction_type;
    END IF;

    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;
")

DBI::dbExecute(con, "CREATE TRIGGER validate_measurements_corrections_trigger
BEFORE INSERT OR UPDATE ON measurements_corrections
FOR EACH ROW
EXECUTE FUNCTION validate_measurements_corrections();
")







# TODO: Create views table that incorporates corrections. Should make use of 'priority' column in correction_types table to determine order of operations, and apply corrections one after the other to data where they overlap.
 
# !!!! function calculate_stats takes values from measurements_continuous. Consider changing this to use the views table instead
