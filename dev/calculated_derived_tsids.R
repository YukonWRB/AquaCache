# Table to hold compound or calculated timeseries. These can then act as inputs to the corrected data views

# Add to table timeseries the following columns:
# timeseries_type CHECK IN (raw, calculated, compound), sensor_priority  CHECK IN (1, 2, 3), inputs (array), equation.
# IF timeseries_type = raw then input and equation should be NULL
# IF timeseries_type = calculated then input should contain at least one value and equation should be filled with something
# IF timeseries_type = compound then input should contain at least two values. 


DBI::dbExecute(con, "
               CREATE TABLE timeseries_derived (
               timeseries_derived_id SERIAL PRIMARY KEY,
               category TEXT NOT NULL CHECK(category IN ('discrete', 'continuous')),
               timeseries_type TEXT NOT NULL CHECK (timeseries_type IN ('calculated', 'compound')),
                 start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 end_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 notes TEXT,
                 share_with INTEGER[] NOT NULL DEFAULT '{1}', -- array of user_ids with whom this timeseries is shared
                 input_tsids INTEGER[] NOT NULL, -- for compound or calculated timeseries. A function and trigger will check that the inputs are existing timeseries_ids.
                 input_tsids_start TIMESTAMP WITH TIME ZONE[] NOT NULL, -- for compound or calculated timeseries. A function and trigger will check that each element of 'input_tsids' has a corresponding element in 'inputs_start'.
                 input_tsids_end TIMESTAMP WITH TIME ZONE[] NOT NULL, -- for compound or calculated timeseries. A function and trigger will check that each element of 'input_tsids' has a corresponding element in 'inputs_end'
                 equations TEXT[], -- for calculated timeseries
                 equations_start TIMESTAMP WITH TIME ZONE[], -- for calculated timeseries
                 equations_end TIMESTAMP WITH TIME ZONE[] -- for calculated timeseries
               );
               ")

DBI::dbExecute(con, "COMMENT ON TABLE timeseries_derived IS 'Table to hold compound or calculated timeseries metadata. Derived timeseries are calculated in view tables measurements_continuous_calculated and measurements_discrete_calculated on user request. These views in turn can feed into view tables measurements_continuous_corrected or measurements_discrete_corrected';")

DBI::dbExecute(con, "COMMENT ON COLUMN timeseries_derived.equations IS 'Equations should omit the output portion and be valid SQL expressions, and inputs refer to the values column of table measurements_continuous or measurements_discrete as per column category matched to the input_tsids and for the input_tsids_start and input_tsids_end datetimes specified. For example, if input_tsids = {1, 2} then the equation (input1 + input2) * 2 would mean (values of tsid 1 + values of tsid 2 (over the respective datetimes of application)) * 2. ;")


# Create trigger function to check that all inputs are existing timeseries_ids
DBI::dbExecute(con, "
               CREATE OR REPLACE FUNCTION check_timeseries_derived_inputs_exist()
RETURNS TRIGGER AS $$
DECLARE
    missing_input INTEGER;
BEGIN
    -- Only perform the check if inputs array is not NULL
    IF NEW.input_tsids IS NOT NULL THEN
        -- Unnest the input_tsids array and check for any values not present in timeseries.timeseries_id
        SELECT input_id INTO missing_input
        FROM unnest(NEW.input_tsids) AS input_id
        WHERE NOT EXISTS (
            SELECT 1 FROM timeseries WHERE timeseries_id = input_id
        )
        LIMIT 1;

        -- If a missing input is found, raise an exception
        IF missing_input IS NOT NULL THEN
            RAISE EXCEPTION 'Input timeseries_id % does not exist in timeseries table', missing_input;
        END IF;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
DBI::dbExecute(con, "
               CREATE TRIGGER check_timeseries_derived_inputs_exist_trigger
               BEFORE INSERT OR UPDATE ON timeseries_derived
               FOR EACH ROW
               EXECUTE FUNCTION check_timeseries_derived_inputs_exist();
               ")

# Create function and trigger to ensure that 'input_tsids', 'input_tsids_start', and 'input_tsids_end' are all the same length
DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION check_timeseries_derived_inputs_length()
RETURNS TRIGGER AS $$
BEGIN
    -- Ensure that all arrays are either NULL or NOT NULL together
    IF (NEW.input_tsids IS NULL AND (NEW.input_tsids_start IS NOT NULL OR NEW.input_tsids_end IS NOT NULL)) OR
       (NEW.input_tsids_start IS NULL AND (NEW.input_tsids IS NOT NULL OR NEW.input_tsids_end IS NOT NULL)) OR
       (NEW.input_tsids_end IS NULL AND (NEW.input_tsids IS NOT NULL OR NEW.input_tsids_start IS NOT NULL)) THEN
        RAISE EXCEPTION 'input_tsids, input_tsids_start, and input_tsids_end must all be NULL or all be NOT NULL';
    END IF;

    -- If arrays are not NULL, check that they are of the same length
    IF NEW.input_tsids IS NOT NULL THEN
        IF ARRAY_LENGTH(NEW.input_tsids, 1) != ARRAY_LENGTH(NEW.input_tsids_start, 1) THEN
            RAISE EXCEPTION 'input_tsids and input_tsids_start arrays must be the same length';
        END IF;
        IF ARRAY_LENGTH(NEW.input_tsids, 1) != ARRAY_LENGTH(NEW.input_tsids_end, 1) THEN
            RAISE EXCEPTION 'input_tsids and input_tsids_end arrays must be the same length';
        END IF;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
DBI::dbExecute(con, "
               CREATE TRIGGER check_timeseries_derived_inputs_length_trigger
               BEFORE INSERT OR UPDATE ON timeseries_derived
               FOR EACH ROW
               EXECUTE FUNCTION check_timeseries_derived_inputs_length();
               ")


DBI::dbExecute(con, "
              ALTER TABLE timeseries_derived
              ADD CONSTRAINT calculated_timeseries_requires_equations
              CHECK (timeseries_type != 'calculated' OR equations IS NOT NULL);
")

DBI::dbExecute(con, "
               ALTER TABLE timeseries_derived
ADD CONSTRAINT compound_timeseries_requires_no_equations
CHECK (timeseries_type != 'compound' OR equations IS NULL);
")



# Create function and trigger to ensure that 'equations', 'equations_start', and 'equations_end' are all the same length
DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION check_timeseries_derived_equations_length()
RETURNS TRIGGER AS $$
BEGIN
    -- Ensure that all arrays are either NULL or NOT NULL together
    IF (NEW.equations IS NULL AND (NEW.equations_start IS NOT NULL OR NEW.equations_end IS NOT NULL)) OR
       (NEW.equations_start IS NULL AND (NEW.equations IS NOT NULL OR NEW.equations_end IS NOT NULL)) OR
       (NEW.equations_end IS NULL AND (NEW.equations IS NOT NULL OR NEW.equations_start IS NOT NULL)) THEN
        RAISE EXCEPTION 'equations, equations_start, and equations_end must all be NULL or all be NOT NULL';
    END IF;

    -- If arrays are not NULL, check that they are of the same length
    IF NEW.equations IS NOT NULL THEN
        IF ARRAY_LENGTH(NEW.equations, 1) != ARRAY_LENGTH(NEW.equations_start, 1) THEN
            RAISE EXCEPTION 'equations and equations_start arrays must be the same length';
        END IF;
        IF ARRAY_LENGTH(NEW.equations, 1) != ARRAY_LENGTH(NEW.equations_end, 1) THEN
            RAISE EXCEPTION 'equations and equations_end arrays must be the same length';
        END IF;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

")
DBI::dbExecute(con, "
               CREATE TRIGGER check_timeseries_derived_equations_length_trigger
               BEFORE INSERT OR UPDATE ON timeseries_derived
               FOR EACH ROW
               EXECUTE FUNCTION check_timeseries_derived_equations_length();
               ")


# Create view tables for the derived/calculated timeseries

