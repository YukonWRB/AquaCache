# Commented code below has already been integrated into databases and int AquaCacheInit. 

# # Add table to hold corrections. These can apply to anything with a timeseries_id. A view table will be created to apply these corrections to the data and to let users extract corrected data
# 
# DBI::dbExecute(con, "CREATE TABLE correction_types (
#   correction_type_id SERIAL PRIMARY KEY,
#   correction_type TEXT NOT NULL,
#   description TEXT NOT NULL,
#   priority INTEGER NOT NULL,
#   value1 BOOLEAN DEFAULT FALSE,
#   value1_description TEXT,
#   value2 BOOLEAN DEFAULT FALSE,
#   value2_description TEXT,
#   timestep_window BOOLEAN DEFAULT FALSE,
#   equation BOOLEAN NOT NULL DEFAULT FALSE,
#   created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
#   updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
#   UNIQUE (priority)
# )")
# 
# correction_types <- data.frame(
#   correction_type = c("trim",
#                       "delete",
#                       "drift linear",
#                       "drift equation",
#                       "offset linear",
#                       "scale"),
#   description = c("Remove data points outside of a specified value range",
#                   "Remove data points within a specified datetime range",
#                   "Apply a linear drift correction",
#                   "Apply a drift correction based on an equation",
#                   "Apply a linear offset correction",
#                   "Apply a percent scaling correction"),
#   priority = c(2, 1, 4, 5, 3, 6),
#   value1 = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
#   value1_description = c("Value below which to discard data", 
#                          NA, 
#                          "Drift value (depends on timestep_window)", 
#                          "Equation for drift (depends on timestep_window)", 
#                          "Shift for linear offset",
#                          "Scaling factor (%) to apply to data"),
#   value2 = c(NA, FALSE, FALSE, FALSE, FALSE, FALSE),
#   value2_description = c("Value above which to discard data", 
#                          NA, 
#                          NA, 
#                          NA, 
#                          NA, 
#                          NA),
#   timestep_window = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
#   equation = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
# )
# 
# DBI::dbExecute(con, "COMMENT ON TABLE correction_types IS 'Table to hold correction types for use in the corrections table'")
# 
# DBI::dbAppendTable(con, "correction_types", correction_types)
# 
# DBI::dbExecute(con, "CREATE TABLE corrections (
#   correction_id SERIAL PRIMARY KEY,
#   timeseries_id INTEGER NOT NULL REFERENCES timeseries (timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE,
#   start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
#   end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
#   correction_type INTEGER NOT NULL REFERENCES correction_types(correction_type_id) ON UPDATE CASCADE ON DELETE CASCADE,
#   value1 NUMERIC, -- only optional for 'delete' correction type
#   value2 NUMERIC,  -- Optional second value for two-point offset correction
#   timestep_window INTERVAL,  -- Applies to rolling average smoothing (window) as well as drift corrections (time step)
#   equation TEXT,  -- Equation for drift correction
#   created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
#   updated TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
#   created_by TEXT NOT NULL,
#   modified_by TEXT,
#   UNIQUE (timeseries_id, start_dt, end_dt, correction_type)
# )")
# 
# DBI::dbExecute(con, "COMMENT ON TABLE corrections IS 'Table to hold corrections for timeseries data.'")
# 
# 
# # Make function and trigger to ensure value1, value2, timestep_window are appropriately populated based on correction_type
# DBI::dbExecute(con, "
# CREATE OR REPLACE FUNCTION validate_corrections()
# RETURNS TRIGGER AS $$
# DECLARE
#   v1_required BOOLEAN;
#   v2_required BOOLEAN;
#   timestep_required BOOLEAN;
#   equation_required BOOLEAN;
# BEGIN
#   -- Fetch the correction type definition from correction_types
#   SELECT value1, value2, timestep_window, equation
#   INTO v1_required, v2_required, timestep_required, equation_required
#   FROM correction_types
#   WHERE correction_type_id = NEW.correction_type;
# 
#   -- Check value1
#   IF v1_required AND NEW.value1 IS NULL THEN
#     RAISE EXCEPTION 'value1 cannot be NULL for correction_type_id %', NEW.correction_type;
#   ELSIF NOT v1_required AND NEW.value1 IS NOT NULL THEN
#     RAISE EXCEPTION 'value1 must be NULL for correction_type_id %', NEW.correction_type;
#   END IF;
# 
#   -- Check value2
#   IF v2_required AND NEW.value2 IS NULL THEN
#     RAISE EXCEPTION 'value2 cannot be NULL for correction_type_id %', NEW.correction_type;
#   ELSIF NOT v2_required AND NEW.value2 IS NOT NULL THEN
#     RAISE EXCEPTION 'value2 must be NULL for correction_type_id %', NEW.correction_type;
#   END IF;
# 
#   -- Check timestep_window
#   IF timestep_required AND NEW.timestep_window IS NULL THEN
#     RAISE EXCEPTION 'timestep_window cannot be NULL for correction_type_id %', NEW.correction_type;
#   ELSIF NOT timestep_required AND NEW.timestep_window IS NOT NULL THEN
#     RAISE EXCEPTION 'timestep_window must be NULL for correction_type_id %', NEW.correction_type;
#   END IF;
#   
#   -- Check equation
#   IF equation_required AND NEW.equation IS NULL THEN
#     RAISE EXCEPTION 'equation cannot be NULL for correction_type_id %', NEW.correction_type;
#   ELSIF NOT equation_required AND NEW.equation IS NOT NULL THEN
#     RAISE EXCEPTION 'equation must be NULL for correction_type_id %', NEW.correction_type;
#   END IF;
# 
#   RETURN NEW;
# END;
# $$ LANGUAGE plpgsql;
# ")
# 
# 
# DBI::dbExecute(con, "CREATE TRIGGER validate_corrections_trigger
# BEFORE INSERT OR UPDATE ON corrections
# FOR EACH ROW
# EXECUTE FUNCTION validate_corrections();
# ")
# 
# 
# DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION apply_corrections(
#   p_timeseries_id INTEGER,
#   p_datetime TIMESTAMP WITH TIME ZONE,
#   p_value NUMERIC
# ) RETURNS NUMERIC AS $$
# DECLARE
#   corrected_value NUMERIC := p_value;
#   correction_row RECORD;
#   time_since_start NUMERIC;
#   time_window NUMERIC;
#   rate NUMERIC;
#   correction NUMERIC;
# BEGIN
#   IF p_value IS NULL THEN
#     RETURN NULL;
#     END IF;
#     
#   FOR correction_row IN
#     SELECT
#       c.value1 AS c_value1,
#       c.value2 AS c_value2,
#       c.timestep_window AS c_timestep_window,
#       c.equation AS c_equation,
#       c.start_dt,
#       c.end_dt,
#       ct.correction_type,
#       ct.priority
#     FROM corrections c
#     JOIN correction_types ct ON c.correction_type = ct.correction_type_id
#     WHERE c.timeseries_id = p_timeseries_id
#       AND c.start_dt <= p_datetime
#       AND c.end_dt >= p_datetime
#     ORDER BY ct.priority ASC
#   LOOP
#     -- Apply correction based on correction_row.correction_type
#     IF correction_row.correction_type = 'delete' THEN
#       -- Remove the data point
#       RETURN NULL;
#       
#     ELSIF correction_row.correction_type = 'trim' THEN
#       -- Remove data points outside of a specified value range
#       IF correction_row.c_value1 IS NOT NULL AND corrected_value < correction_row.c_value1 THEN
#         RETURN NULL;
#       ELSIF correction_row.c_value2 IS NOT NULL AND corrected_value > correction_row.c_value2 THEN
#         RETURN NULL;
#       END IF;
#       
#     ELSIF correction_row.correction_type = 'offset linear' THEN
#       -- Apply linear offset
#       corrected_value := corrected_value + correction_row.c_value1;
#       
#     ELSIF correction_row.correction_type = 'offset two-point' THEN
#       -- Apply two-point offset correction
#       RAISE EXCEPTION 'Offset two-point correction not implemented yet';
#       
#     ELSIF correction_row.correction_type = 'scale' THEN
#       -- Apply percent scaling
#       corrected_value := corrected_value * (correction_row.c_value1 / 100.0);
#       
#     ELSIF correction_row.correction_type = 'drift linear' THEN
#       -- Apply linear drift correction that continues until end_dt
#       IF p_datetime < correction_row.start_dt OR p_datetime > correction_row.end_dt THEN
#         -- No correction applied outside the correction period
#         correction := 0;
#       ELSE
#         -- During the correction period; apply proportional correction
#         time_since_start := EXTRACT(EPOCH FROM (p_datetime - correction_row.start_dt));
#         time_window := EXTRACT(EPOCH FROM correction_row.c_timestep_window);
#         rate := correction_row.c_value1 / time_window;  -- Correction per second
#         correction := rate * time_since_start;
#       END IF;
#       corrected_value := corrected_value + correction;
#       
#     ELSIF correction_row.correction_type = 'drift equation' THEN
#       -- Apply drift correction based on an equation
#       RAISE EXCEPTION 'Drift equation correction not implemented yet';
#     ELSE
#       RAISE NOTICE 'Correction type % not handled', correction_row.correction_type;
#     END IF;
#   END LOOP;
# 
#   RETURN corrected_value;
# END;
# $$ LANGUAGE plpgsql;
# ")
# 
# 
# DBI::dbExecute(con, "
# CREATE OR REPLACE VIEW corrected_measurements_continuous AS
# SELECT
# mc.timeseries_id,
# mc.datetime,
# mc.value AS value_raw,
# ac.value_corrected,
# mc.grade,
# mc.approval,
# mc.period,
# mc.imputed,
# mc.owner,
# mc.contributor
# FROM
# measurements_continuous mc
# CROSS JOIN LATERAL (
#   SELECT apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
# ) ac
# WHERE
# ac.value_corrected IS NOT NULL;
# ")
# 
# DBI::dbExecute(con, "
# CREATE OR REPLACE VIEW measurements_discrete_corrected AS
# SELECT
#   md.timeseries_id,
#   md.target_datetime,
#   md.datetime,
#   md.value AS value_raw,
#   ac.corrected_value AS value_corrected,
#   -- List other columns from measurements_discrete
#   md.result_value_type,
#   md.result_condition,
#   md.result_condition_value,
#   md.sample_type,
#   md.collection_method,
#   md.sample_fraction,
#   md.result_speciation,
#   md.lab,
#   md.protocol,
#   md.note,
#   md.owner,
#   md.contributor
# FROM
#   measurements_discrete md
# LEFT JOIN LATERAL (
#   SELECT apply_corrections(md.timeseries_id, md.datetime, md.value) AS corrected_value
# ) ac ON TRUE
# WHERE
#   (md.value IS NOT NULL AND ac.corrected_value IS NOT NULL)
#   OR md.value IS NULL;
# ")

 
# !!!! function calculate_stats takes values from measurements_continuous. Consider changing this to use the views table instead
