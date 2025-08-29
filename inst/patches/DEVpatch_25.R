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
  
  # create a new table called discrete.guidelines
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS discrete.guidelines (
      guideline_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
      guideline_name TEXT NOT NULL,
      organization TEXT NOT NULL,
      description TEXT,
      parameter_id INTEGER NOT NULL REFERENCES public.parameters(parameter_id),
      sample_fraction_id INTEGER NOT NULL REFERENCES discrete.sample_fractions(sample_fraction_id),
      guideline_sql TEXT NOT NULL,
      created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
      created_by TEXT DEFAULT CURRENT_USER NOT NULL,
      modified TIMESTAMP WITH TIME ZONE,
      modified_by TEXT,
      UNIQUE (guideline_name, organization)
  );")
  
  # Triggers to track who created/modified records and when
  DBI::dbExecute(con, "create trigger trg_user_audit before update on discrete.guidelines for each row execute function public.user_modified()")
  DBI::dbExecute(con, "create trigger update_modify_time before update on discrete.guidelines for each row execute function public.update_modified()")
  
  # Safety functions to ensure that only SELECT statements are used in the guideline_sql column
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION discrete.guidelines_validate_trg()
    RETURNS trigger
    SET search_path = discrete, public
    LANGUAGE plpgsql
    AS $fn$
    DECLARE
      expr          text := NEW.guideline_sql;
      needs_sample  boolean;
      v             numeric;
      trimmed       text;
      scan          text;  -- expr with quoted strings removed (heuristic)
      k             text;
      forbidden     text[] := ARRAY[
        'insert','update','delete','merge','alter','drop','truncate','create',
        'grant','revoke','call','do','copy','vacuum','analyze','refresh','cluster',
        'reindex','security','execute','commit','rollback','savepoint','lock','unlock'
      ];
    BEGIN
      -- Basic presence
      IF expr IS NULL OR btrim(expr) = '' THEN
        RAISE EXCEPTION 'guideline_sql cannot be empty';
      END IF;
    
      -- One statement (no semicolons)
      IF position(';' in expr) > 0 THEN
        RAISE EXCEPTION 'guideline_sql must be a single statement (no semicolons)';
      END IF;
    
      -- Must be SELECT or WITH…SELECT
      trimmed := ltrim(expr);
      IF NOT (trimmed ~* '^\\s*select\\b' OR trimmed ~* '^\\s*with\\b.*\\bselect\\b') THEN
        RAISE EXCEPTION 'guideline_sql must be a SELECT (optionally WITH … SELECT)';
      END IF;
    
      -- Remove single-quoted and dollar-quoted strings (dot matches newlines)
      scan := regexp_replace(expr, $$'(?:''|[^'])*'$$, '', 'g');
      scan := regexp_replace(scan, '(?s)\\$[^$]*\\$.*?\\$[^$]*\\$', '', 'g');
    
      -- Block write/DDL/control keywords outside of quoted strings
      FOREACH k IN ARRAY forbidden LOOP
        IF scan ~* ('\\m' || k || '\\M') THEN
          RAISE EXCEPTION 'guideline_sql contains forbidden keyword: %', k;
        END IF;
      END LOOP;
    
      -- Must yield a single numeric scalar (≤1 row, 1 column)
      needs_sample := position('$1' in expr) > 0;
      IF needs_sample THEN
        EXECUTE format('WITH q AS (%s) SELECT (SELECT * FROM q)::numeric', expr)
          INTO v USING NULL::integer;  -- compile check for $1
      ELSE
        EXECUTE format('WITH q AS (%s) SELECT (SELECT * FROM q)::numeric', expr)
          INTO v;
      END IF;
    
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
