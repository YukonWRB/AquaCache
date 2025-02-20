# Patch 13
# Properly relocate postgis extension

# Initial checks #################
# Ensure the user is postgres OR admin as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user %in% c("postgres", "admin")) {
  stop("You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work.")
}

message("Working on Patch 13. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Add columns to locations table
  DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN anthropogenic_influence BOOLEAN DEFAULT FALSE;")
  # Comment on the column
  DBI::dbExecute(con, "COMMENT ON COLUMN locations.anthropogenic_influence IS 'Flag to indicate if the location has anthropogenic influence';")
  DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN sentinel_location BOOLEAN DEFAULT FALSE;")
  # Comment on the column
  DBI::dbExecute(con, "COMMENT ON COLUMN locations.sentinel_location IS 'Flag to indicate if the location is a sentinel location for climate change analyses and reports';")
  
  
  
  # Add column to timeseries table called 'compound' to indicate if the timeseries is a compound timeseries, and another column to indicate which timeseries are used to create the compound timeseries
  DBI::dbExecute(con, "ALTER TABLE timeseries ADD COLUMN derived BOOLEAN DEFAULT FALSE;")
  # Comment on the column
  DBI::dbExecute(con, "COMMENT ON COLUMN timeseries.derived IS 'Flag to indicate if the timeseries is a derived (calculated or compound) timeseries';")
  
  # Add a table to hold the child timeseries, the formulas to apply, and the time ranges to apply them in
  # The column 'child_timeseries' is a JSONB column that holds timeseries_id values in the form of a: timeseries_id, b: timeseries_id, etc.
  DBI::dbExecute(con, "CREATE TABLE derived_timeseries (
    derived_timeseries_id SERIAL PRIMARY KEY,
    timeseries_id INTEGER NOT NULL REFERENCES timeseries(timeseries_id) ON DELETE CASCADE ON UPDATE CASCADE,
    child_timeseries JSONB NOT NULL,
    formula TEXT NOT NULL,
    start_datetime TIMESTAMP WITH TIME ZONE, # Nullable to allow for calculations to apply to all past data
    end_datetime TIMESTAMP WITH TIME ZONE, # Nullable to allow for calculations to apply to all future data
  );")
  
  # Make a function to check the child_timeseries JSONB column
  DBI::dbExecute(con, "
                 CREATE OR REPLACE FUNCTION check_child_timeseries_json()
RETURNS TRIGGER AS $$
DECLARE
  key text;
  val text;
  child_id int;
BEGIN
  -- If child_timeseries is not null, iterate over each key/value
  IF NEW.child_timeseries IS NOT NULL THEN
    FOR key, val IN 
      SELECT * FROM jsonb_each_text(NEW.child_timeseries)
    LOOP
      -- Check that the key is just letters: a, b, c, ...
      IF key !~ '^[a-z]+$' THEN
        RAISE EXCEPTION 'Invalid key in child_timeseries: %, keys must be lowercase letters only', key;
      END IF;

      -- Convert value to int; throw an error if it can't be cast
      BEGIN
        child_id := val::int;
      EXCEPTION WHEN invalid_text_representation THEN
        RAISE EXCEPTION 'Value for child_timeseries key=% is not an integer: %', key, val;
      END;

      -- Check that timeseries_id actually exists
      IF NOT EXISTS (
        SELECT 1
        FROM timeseries
        WHERE timeseries_id = child_id
      ) THEN
        RAISE EXCEPTION 'child_timeseries references a non-existent timeseries_id: key=%, id=%', key, child_id;
      END IF;
    END LOOP;
  END IF;
  
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "
  CREATE TRIGGER check_child_timeseries_trg
BEFORE INSERT OR UPDATE ON derived_timeseries
FOR EACH ROW
EXECUTE PROCEDURE check_child_timeseries_json();
")
  
  
  
  # Validate the formula
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION validate_derived_timeseries()
RETURNS TRIGGER AS $$
DECLARE
    key   text;
    val   text;
    child_id int;
    formula_vars text[];
    json_keys   text[];
    var_name    text;
BEGIN
    /*
     * 1. Validate child_timeseries:
     *    - Ensure each key is alphabetical
     *    - Ensure each value is an integer referencing timeseries
     */
    IF NEW.child_timeseries IS NOT NULL THEN
        FOR key, val IN
            SELECT * FROM jsonb_each_text(NEW.child_timeseries)
        LOOP
            -- (a) Check that key is only letters
            IF key !~ '^[a-z]+$' THEN
                RAISE EXCEPTION 'Invalid key \"%\": only letters are allowed in child_timeseries JSON key', key;
            END IF;

            -- (b) Convert the value to int to ensure it is numeric
            BEGIN
                child_id := val::int;
            EXCEPTION WHEN invalid_text_representation THEN
                RAISE EXCEPTION 'Value for child_timeseries key=\"%\": \"%\" is not a valid integer', key, val;
            END;

            -- (c) Check the timeseries table for existence
            IF NOT EXISTS (
                SELECT 1 FROM timeseries
                 WHERE timeseries_id = child_id
            ) THEN
                RAISE EXCEPTION 'child_timeseries references non-existent timeseries_id % at key \"%\"', child_id, key;
            END IF;
        END LOOP;
    ELSE
        RAISE EXCEPTION 'child_timeseries JSON cannot be NULL.';
    END IF;

    /*
     * 2. Validate formula string basics:
     *    - Disallow suspicious characters (e.g. semicolons, quotes, or others).
     *    - Allow letters, digits, underscores, parentheses, whitespace, +, -, *, /, ^, etc.
     */
    IF NEW.formula ~ '[;\\'\"]' THEN
        RAISE EXCEPTION 'Formula contains disallowed characters (quotes or semicolons).'
            USING HINT = 'Allowed: letters, digits, operators (+-*/^), parentheses, underscores, spaces.';
    END IF;

    /*
     * 3. Extract variable references in the formula and ensure they match the JSON keys.
     *    We'll do a regex that picks up sequences of letters or underscores, ignoring digits for now.
*/
  -- Step (a): Gather array of JSON keys for quick membership checks
SELECT array_agg(k)
INTO json_keys
FROM (
  SELECT key
  FROM jsonb_each_text(NEW.child_timeseries)
) sub;

-- Step (b): parse formula to find variables
-- This regex grabs sequences of letters (and underscores). Tweak as needed.
SELECT regexp_split_to_array(
  -- We only want these variables, so let's carve them out
      (regexp_replace(NEW.formula, '[^a-zA-Z_]+', ' ', 'g')), 
      '\\s+'
    )
    INTO formula_vars;

    -- If the formula references no variables, formula_vars might be '{}'
    IF formula_vars IS NOT NULL THEN
      FOREACH var_name IN ARRAY formula_vars
      LOOP
        IF var_name = '' THEN
            CONTINUE; -- skip empty from splitting
        END IF;
        -- If it's not in the JSON keys, throw an error
  IF NOT (var_name = ANY (json_keys)) THEN
  RAISE EXCEPTION 'Formula references variable \"%\" that is not in child_timeseries JSON. Valid keys: %', var_name, json_keys;
  END IF;
  END LOOP;
  END IF;
  
  RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
                 ")
  
  DBI::dbExecute(con, "
  CREATE TRIGGER validate_derived_timeseries_trg
  BEFORE INSERT OR UPDATE ON derived_timeseries
  FOR EACH ROW
  EXECUTE PROCEDURE validate_derived_timeseries();
  ")
  
  
  # Create a function to evaluate a mathematical expression
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION eval_math_expr(
    formula     text,
    var_values  jsonb
)
RETURNS numeric
LANGUAGE plpgsql AS
$$
DECLARE
    safe_formula text := formula;
    result_val   numeric;
    kv record;         -- for looping over JSON key-value pairs
    var_name text;
    var_value numeric;
BEGIN
    ------------------------------------------------------------------------------
    -- 1. Quick sanity check on the formula string:
    --    - Disallow quotes, semicolons, or other suspicious characters
    --    - Allow letters, digits, parentheses, typical math operators, decimal points, underscores, and spaces
    ------------------------------------------------------------------------------
    IF safe_formula ~ '[;\'\"]' THEN
        RAISE EXCEPTION 'Formula contains disallowed characters (quotes or semicolons): %', safe_formula;
    END IF;
    IF safe_formula !~ '^[0-9a-zA-Z\\(\\)\\+\\-\\*/\\^_\\. ]+$' THEN
        RAISE EXCEPTION 'Formula contains unexpected characters: %', safe_formula;
    END IF;

    ------------------------------------------------------------------------------
    -- 2. Replace variables with their numeric values from var_values JSONB.
    --    For each \"key\":\"value\" pair, we do a global search/replace in safe_formula.
    --    Key must match word boundaries \b<var>\b so we don't replace partial matches.
------------------------------------------------------------------------------
  FOR kv IN
SELECT key, value
FROM jsonb_each(var_values)
LOOP
var_name  := kv.key;
var_value := kv.value::numeric;  -- cast the JSON value to numeric

-- Optionally, you could do a quick check on var_name: e.g. ensure it's [a-zA-Z]+
        IF var_name !~ '^[a-zA-Z]+$' THEN
            RAISE EXCEPTION 'Variable name % is invalid (only letters allowed).', var_name;
        END IF;

        -- Perform the replacement using a word-boundary regex to avoid partial replacements
        safe_formula := regexp_replace(
            safe_formula,
            '\m' || var_name || '\M',  -- \m and \M are PostgreSQL-specific \"start of word\"/\"end of word\"
            var_value::text,
            'g'
        );
    END LOOP;

    ------------------------------------------------------------------------------
    -- 3. Evaluate the resulting expression via dynamic SQL
    ------------------------------------------------------------------------------
    EXECUTE format('SELECT (%s)::numeric', safe_formula) INTO result_val;

    RETURN result_val;
END;
$$;
")
  

  
  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '13' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  message("Patch 13 applied successfully: columns were added to flag locations as having anthropogenic influence and to flag them as 'sentinel' locations for climate change analyses and reports.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 13 failed and the DB has been rolled back to its earlier state. ", e$message)
})
