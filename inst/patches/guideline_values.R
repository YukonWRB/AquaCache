# Create a table to hold guideline values
DBI::dbExecute(con, "CREATE TABLE discrete.guideline_values (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    name_fr TEXT NOT NULL,
    value NUMERIC,
    equation TEXT,
    start_datetime TIMESTAMP WITH TIME ZONE,
    end_datetime TIMESTAMP WITH TIME ZONE
  );")

# Enforce logic to ensure that the start_datetime is before the end_datetime
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION discrete.validate_guideline_start_end()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.start_datetime IS NOT NULL AND NEW.end_datetime IS NOT NULL THEN
        IF NEW.start_datetime >= NEW.end_datetime THEN
            RAISE EXCEPTION 'start_datetime must be before end_datetime';
        END IF;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
# trigger to enforce the logic
DBI::dbExecute(con, "CREATE TRIGGER validate_guideline_start_end_trg
BEFORE INSERT OR UPDATE ON discrete.guideline_values
FOR EACH ROW
EXECUTE PROCEDURE validate_guideline_start_end();
")

# Encore value and equation can't both be NULL
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION discrete.validate_guideline_values()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.value IS NULL AND NEW.equation IS NULL THEN
        RAISE EXCEPTION 'Either value or equation must be provided';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
# trigger to enforce the logic
DBI::dbExecute(con, "CREATE TRIGGER validate_guideline_values_trg
BEFORE INSERT OR UPDATE ON discrete.guideline_values
FOR EACH ROW
EXECUTE PROCEDURE validate_guideline_values();
")


