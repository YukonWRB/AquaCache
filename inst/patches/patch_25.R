# Patch 25

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 25. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop("A transaction is already in progress. Please commit or rollback the current transaction before applying this patch.")
}
active <- dbTransBegin(con)

tryCatch({
  
  message("Creating infrastructure to handle water quality guideline values...")
  
  
  # Rename two columns in table 'discrete.results' for clarity. This requires modifying two trigger function as well
  DBI::dbExecute(con, "ALTER TABLE discrete.results RENAME COLUMN sample_fraction TO sample_fraction_id;")
  DBI::dbExecute(con, "ALTER TABLE discrete.results RENAME COLUMN result_speciation TO result_speciation_id;")
  
  # Edit two functions that reference these columns
  DBI::dbExecute(con, "
        CREATE OR REPLACE FUNCTION discrete.enforce_result_speciation()
       RETURNS trigger
       LANGUAGE plpgsql
      AS $function$
      BEGIN
          -- Check if the associated parameter_id requires result_speciation
          IF EXISTS (
              SELECT 1
              FROM parameters
              WHERE parameter_id = NEW.parameter_id
                AND result_speciation = TRUE
                AND NEW.result_speciation_id IS NULL
          ) THEN
              RAISE EXCEPTION 'result_speciation_id must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
          END IF;
          RETURN NEW;
      END;
      $function$
      ;"
                 )
  
  DBI::dbExecute(con, "
                CREATE OR REPLACE FUNCTION discrete.enforce_sample_fraction()
                 RETURNS trigger
                 LANGUAGE plpgsql
                AS $function$
                BEGIN
                    -- Check if the associated parameter_id requires a sample fraction
                    IF EXISTS (
                        SELECT 1
                        FROM parameters
                        WHERE parameter_id = NEW.parameter_id
                          AND sample_fraction = TRUE
                          AND NEW.sample_fraction_id IS NULL
                    ) THEN
                        RAISE EXCEPTION 'sample_fraction_id must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
                    END IF;
                    RETURN NEW;
                END;
                $function$
                ; 
                 ")
  
  
  # create a new table called discrete.guidelines
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS discrete.guidelines (
      guideline_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      publisher TEXT NOT NULL,
      guideline_name TEXT NOT NULL,
      reference TEXT,
      note TEXT,
      parameter_id INTEGER NOT NULL REFERENCES public.parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE,
      sample_fraction_id INTEGER REFERENCES discrete.sample_fractions(sample_fraction_id) ON UPDATE CASCADE ON DELETE SET NULL,
      result_speciation_id INTEGER REFERENCES discrete.result_speciations(result_speciation_id) ON UPDATE CASCADE ON DELETE SET NULL,
      guideline_sql TEXT NOT NULL,
      created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT DEFAULT CURRENT_USER NOT NULL,
      modified TIMESTAMP WITH TIME ZONE,
      modified_by TEXT,
      UNIQUE (guideline_name, publisher)
  );")
  
  # Triggers to track who created/modified records and when
  DBI::dbExecute(con, "create trigger trg_user_audit before update on discrete.guidelines for each row execute function public.user_modified()")
  DBI::dbExecute(con, "create trigger update_modify_time before update on discrete.guidelines for each row execute function public.update_modified()")
  
  # sample_fraction_id and result_speciation_id may need to be NOT NULL depending on what's entered in table 'parameters'. For any parameter, if result_speciation is TRUE, then result_speciation_id must be NOT NULL, and if sample_fraction is TRUE, then sample_fraction_id must be NOT NULL.
  # Use existing functions to enforce this; discrete.enforce_result_speciation and discrete.enforce_sample_fraction
  DBI::dbExecute(con, "DROP TRIGGER IF EXISTS trg_enforce_result_speciation ON discrete.guidelines;")
  DBI::dbExecute(con, "create trigger trg_enforce_result_speciation before insert or update on discrete.guidelines for each row execute function enforce_result_speciation();")
  DBI::dbExecute(con, "DROP TRIGGER IF EXISTS trg_enforce_sample_fraction ON discrete.guidelines;")
  DBI::dbExecute(con, "create trigger trg_enforce_sample_fraction before insert or update on discrete.guidelines for each row execute function enforce_sample_fraction();")
  
  
  # Safety functions to validate guideline SQL
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION discrete.guidelines_validate_trg()
RETURNS trigger
SET search_path = discrete, public
LANGUAGE plpgsql
AS $fn$
DECLARE
  expr            text := NEW.guideline_sql;
  scan            text;
  needs_sample    boolean;
  explain_json    jsonb;
  allowed_schemas text[] := ARRAY['discrete','public'];
  bad_schema      text;
  cmdtype         text;
BEGIN
  -- presence
  IF expr IS NULL OR btrim(expr) = '' THEN
    RAISE EXCEPTION 'Guideline SQL cannot be empty';
  END IF;

  -- quick lexical guards (blocks multiple statements / DDL)
  scan := expr;
  -- strip dollar-quoted strings
  scan := regexp_replace(scan, '(?s)\\$[^$]*\\$.*?\\$[^$]*\\$', '', 'g');
  -- strip single-quoted strings
  scan := regexp_replace(scan, '''([^''\\\\]|\\\\.)*''', '', 'g');
  -- strip comments
  scan := regexp_replace(scan, '--.*?(\\n|$)', '', 'g');
  scan := regexp_replace(scan, '/\\*.*?\\*/', '', 'gs');

  -- must not contain semicolons
  IF scan ~ ';' THEN
    RAISE EXCEPTION 'Guideline SQL must be a single statement (no semicolons)';
  END IF;

  -- must be a SELECT (allow WITH … SELECT)
  IF scan !~* '^[[:space:]]*\\(*[[:space:]]*(with[[:space:]]+.*select|select)([[:space:]]|\\()' THEN
    RAISE EXCEPTION 'Guideline SQL must begin with SELECT (optionally WITH … SELECT)';
  END IF;

  -- placeholder policy: only $1 allowed
  IF scan ~ '\\$[2-9]' THEN
    RAISE EXCEPTION 'Only $1 parameter placeholder is permitted';
  END IF;
  needs_sample := scan ~ '\\$1';

  -- Build a wrapper that enforces: single scalar numeric row
  -- If expr returns more than one row/col or non-numeric => error at EXPLAIN time (shape is checked)
  -- NOTE: we do EXPLAIN to *parse/plan only*, not execute.
  IF needs_sample THEN
    EXECUTE format($$
      EXPLAIN (VERBOSE, FORMAT JSON)
      WITH q AS (%s)
      SELECT (SELECT * FROM q)::numeric
    $$, expr)
    INTO explain_json
    USING NULL;  -- e.g., user must write $1::numeric in expr
  ELSE
    EXECUTE format($$
      EXPLAIN (VERBOSE, FORMAT JSON)
      WITH q AS (%s)
      SELECT (SELECT * FROM q)::numeric
    $$, expr)
    INTO explain_json;
  END IF;

  -- Schema whitelist using recursive walk of the plan tree
  WITH RECURSIVE plan_nodes AS (
    SELECT (explain_json->0->'Plan') AS n
    UNION ALL
    SELECT child
    FROM plan_nodes p
    CROSS JOIN LATERAL jsonb_array_elements(COALESCE(p.n->'Plans','[]')) AS children(child)
  ),
  schemas AS (
    SELECT DISTINCT lower(n->>'Schema') AS schem
    FROM plan_nodes
    WHERE n ? 'Schema'
  )
  SELECT schem
  INTO bad_schema
  FROM schemas
  WHERE schem IS NOT NULL
    AND schem <> ALL(allowed_schemas)
  LIMIT 1;

  IF bad_schema IS NOT NULL THEN
    RAISE EXCEPTION 'Guideline SQL references disallowed schema: %', bad_schema;
  END IF;

  -- If we got here, it parses, plans, is a SELECT, produces a single numeric scalar, and only touches allowed schemas.
  RETURN NEW;
END;
$fn$;
  ")
  
  suppressMessages(DBI::dbExecute(con, "DROP TRIGGER IF EXISTS trg_check_sql ON discrete.guidelines;"))
  
  DBI::dbExecute(con, "
    CREATE TRIGGER trg_check_sql
    BEFORE INSERT OR UPDATE OF guideline_sql ON discrete.guidelines
    FOR EACH ROW
    EXECUTE FUNCTION discrete.guidelines_validate_trg();
  ")
  
  
  # Create a function that will take values from guideline_sql column and execute the SQL to return the guideline value
  
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION discrete.get_guideline_value(
      in_guideline_id INTEGER,
      in_sample_id    INTEGER DEFAULT NULL
    )
    RETURNS NUMERIC
    SET search_path = discrete, public
    LANGUAGE plpgsql
    AS $fn$
    DECLARE
      expr          TEXT;
      result        NUMERIC;
      needs_sample  BOOLEAN;
    BEGIN
      SELECT g.guideline_sql INTO expr
      FROM discrete.guidelines g
      WHERE g.guideline_id = in_guideline_id;
    
      IF expr IS NULL THEN 
        RAISE EXCEPTION 'No guideline found with guideline_id %', in_guideline_id;
      END IF;
    
    IF expr ~ '\\$[2-9]' THEN
        RAISE EXCEPTION 'Guideline SQL may only use $1 parameter';
      END IF;

      -- Templates that need a sample must use $1
      needs_sample := position('$1' in expr) > 0;
    
      IF needs_sample AND in_sample_id IS NULL THEN
        RAISE EXCEPTION 'This guideline requires a sample_id but none was provided.';
      END IF;
    
      IF needs_sample THEN
        EXECUTE expr INTO STRICT result USING in_sample_id;
      ELSE
        EXECUTE expr INTO STRICT result;
      END IF;
    
      RETURN result;
    
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE EXCEPTION 'Guideline % SQL returned no value', in_guideline_id;
      WHEN TOO_MANY_ROWS THEN
        RAISE EXCEPTION 'Guideline % SQL returned more than one value', in_guideline_id;
    END;
    $fn$;
  ")
  
  DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION discrete.get_guideline_value(INTEGER, INTEGER) TO PUBLIC;")
  
  
  
  # Now migrate old SERIAL primary key columns to IDENTITY columns #################
  
  DBI::dbExecute(con, "
DO $$
DECLARE
  r record;
  nextval bigint;
  seq_schema text;
  seq_name   text;
  old_seq_tmp text;
BEGIN
  FOR r IN
    SELECT
      n.nspname  AS sch,
      c.relname  AS tbl,
      a.attname  AS col,
      pg_get_serial_sequence(format('%I.%I', n.nspname, c.relname), a.attname) AS seq
    FROM pg_attribute a
    JOIN pg_class     c ON c.oid = a.attrelid AND c.relkind = 'r'
    JOIN pg_namespace n ON n.oid = c.relnamespace
    JOIN pg_attrdef   d ON d.adrelid = a.attrelid AND d.adnum = a.attnum
    WHERE a.attnum > 0
      AND NOT a.attisdropped
      -- has nextval() default => SERIAL-like
      AND pg_get_expr(d.adbin, d.adrelid) LIKE 'nextval(%'
      -- (optional) limit to PK columns only
      AND EXISTS (
        SELECT 1 FROM pg_index i
        WHERE i.indrelid = c.oid AND i.indisprimary
          AND a.attnum = ANY(i.indkey)
      )
  LOOP
    -- Compute a safe next value before we drop/rename
    EXECUTE format(
      'SELECT GREATEST(
         (SELECT COALESCE(MAX(%1$I),0)+1 FROM %2$I.%3$I),
         (SELECT last_value + CASE WHEN is_called THEN 1 ELSE 0 END FROM %4$s)
       )',
      r.col, r.sch, r.tbl, r.seq
    ) INTO nextval;

    -- Parse schema and name of the existing serial sequence
    seq_schema := split_part(r.seq, '.', 1);
    seq_name   := split_part(r.seq, '.', 2);

    -- Rename the old serial sequence to free the original name
   old_seq_tmp := seq_name || '_old';
    EXECUTE format('ALTER SEQUENCE %s RENAME TO %I', r.seq, old_seq_tmp);

    -- Drop the DEFAULT and add IDENTITY with the original sequence name
    EXECUTE format('ALTER TABLE %I.%I ALTER COLUMN %I DROP DEFAULT', r.sch, r.tbl, r.col);
    EXECUTE format(
      'ALTER TABLE %I.%I ALTER COLUMN %I ADD GENERATED BY DEFAULT AS IDENTITY (SEQUENCE NAME %s)',
      r.sch, r.tbl, r.col, format('%I.%I', seq_schema, seq_name)
    );

    -- Set the identity start to the computed next value
    EXECUTE format('ALTER TABLE %I.%I ALTER COLUMN %I RESTART WITH %s', r.sch, r.tbl, r.col, nextval);

    -- Old serial sequence is no longer used; drop it
    EXECUTE format('DROP SEQUENCE %I.%I', seq_schema, old_seq_tmp);
  END LOOP;
END
$$ LANGUAGE plpgsql;
")
  
  
  
  # Make additions to water well tables so we can track installation purpose
  # Create a new table boreholes.borehole_well_purposes
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS boreholes.borehole_well_purposes (
      borehole_well_purpose_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      purpose_name TEXT NOT NULL UNIQUE,
      description TEXT NOT NULL,
      created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
      created_by TEXT DEFAULT CURRENT_USER NOT NULL,
      modified TIMESTAMP WITH TIME ZONE,
      modified_by TEXT
  );")
  
  # Triggers
  DBI::dbExecute(con, "create trigger trg_user_audit before update on boreholes.borehole_well_purposes for each row execute function user_modified()")
  DBI::dbExecute(con, "create trigger update_modify_time before update on boreholes.borehole_well_purposes for each row execute function update_modified()")
  
  # Populate with some common purposes
  df <- data.frame(purpose_name = c("monitoring", 
                                    "drinking water, residential", 
                                    "drinking water, municipal/commercial", 
                                    "irrigation", 
                                    "observation", 
                                    "dewatering", 
                                    "injection",
                                    "mineral exploration"),
                   description = c("Well installed for monitoring purposes, typically with a small diameter and screen.",
                                   "Well installed for private residence drinking water supply.",
                                   "Well installed for municipal or commercial drinking water supply.",
                                   "Well installed to provide water for agricultural irrigation.",
                                   "Well installed to observe groundwater levels or quality changes over time.",
                                   "Well installed to lower the groundwater table temporarily or permanently.",
                                   "Well installed to inject water or other fluids into the ground.",
                                   "Borehole installed as part of mineral exploration activities.")
  )
  
  DBI::dbAppendTable(con, "borehole_well_purposes", df)
  
  # Add a new column to boreholes and wells to reference the purpose
  DBI::dbExecute(con, "ALTER TABLE boreholes.boreholes ADD COLUMN borehole_well_purpose_id INTEGER REFERENCES boreholes.borehole_well_purposes(borehole_well_purpose_id) ON UPDATE CASCADE ON DELETE SET NULL;")
  DBI::dbExecute(con, "ALTER TABLE boreholes.wells ADD COLUMN borehole_well_purpose_id INTEGER REFERENCES boreholes.borehole_well_purposes(borehole_well_purpose_id) ON UPDATE CASCADE ON DELETE SET NULL;")
  
  # Also add columns inferred_purpose (boolean)
  DBI::dbExecute(con, "ALTER TABLE boreholes.boreholes ADD COLUMN inferred_purpose BOOLEAN DEFAULT TRUE;")
  DBI::dbExecute(con, "ALTER TABLE boreholes.wells ADD COLUMN inferred_purpose BOOLEAN DEFAULT TRUE;")
  
  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '25' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  
  message("Patch 25 applied successfully.")
  
}, error = function(e) {
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  stop("Patch 25 failed and the DB has been rolled back to its earlier state. ", e$message)
})
