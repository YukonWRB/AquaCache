# Patch 55: constrain stored SQL executed by correction, compound, and
# guideline evaluation functions.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 55: constraining stored SQL expressions. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('continuous.corrections') IS NOT NULL AS has_corrections,
         to_regclass('continuous.timeseries_compounds') IS NOT NULL AS has_compounds,
         to_regclass('criteria.guideline_value_rules') IS NOT NULL AS has_guidelines,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info,
         to_regprocedure('criteria.validate_guideline_value_rule()') IS NOT NULL AS has_guideline_validator"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 55 requires correction, compound-timeseries, and guideline objects from earlier patches."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.numeric_sql_expression_is_safe(
         expression_sql TEXT,
         allowed_placeholders INTEGER[],
         allow_value_identifiers BOOLEAN
       )
       RETURNS BOOLEAN
       LANGUAGE plpgsql
       IMMUTABLE
       AS $function$
       DECLARE
         scan TEXT;
         token TEXT;
         approved_words CONSTANT TEXT[] := ARRAY[
           'abs', 'case', 'ceil', 'ceiling', 'coalesce', 'double', 'else',
           'end', 'exp', 'floor', 'greatest', 'least', 'ln', 'log', 'null',
           'nullif', 'numeric', 'power', 'precision', 'real', 'round', 'sign',
           'sqrt', 'then', 'when'
         ];
         forbidden_words CONSTANT TEXT[] := ARRAY[
           'alter', 'analyze', 'call', 'copy', 'create', 'delete', 'do',
           'drop', 'execute', 'grant', 'insert', 'merge', 'prepare', 'revoke',
           'select', 'set', 'truncate', 'update', 'vacuum', 'with'
         ];
       BEGIN
         IF expression_sql IS NULL OR btrim(expression_sql) = '' THEN
           RETURN TRUE;
         END IF;

         scan := expression_sql;
         IF scan ~ ';|--|/\\*|\\*/|''|\"' THEN
           RETURN FALSE;
         END IF;
         IF regexp_replace(
              scan,
              '(\\$[0-9]+|[A-Za-z_][A-Za-z0-9_]*|([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?|::|<=|>=|<>|!=|[-+*/%^(),<>=]|[[:space:]])',
              '',
              'g'
            ) <> '' THEN
           RETURN FALSE;
         END IF;

         FOR token IN
           SELECT match[1]
           FROM regexp_matches(scan, '\\$([0-9]+)', 'g') AS match
         LOOP
           IF token::INTEGER <> ALL(allowed_placeholders) THEN
             RETURN FALSE;
           END IF;
         END LOOP;

         FOR token IN
           SELECT lower(match[1])
           FROM regexp_matches(scan, '([A-Za-z_][A-Za-z0-9_]*)', 'g') AS match
         LOOP
           IF token = ANY(forbidden_words) THEN
             RETURN FALSE;
           END IF;
           IF NOT allow_value_identifiers AND token <> ALL(approved_words) THEN
             RETURN FALSE;
           END IF;
         END LOOP;

         FOR token IN
           SELECT lower(match[1])
           FROM regexp_matches(
             scan,
             '([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\(',
             'g'
           ) AS match
         LOOP
           IF token <> ALL(approved_words) THEN
             RETURN FALSE;
           END IF;
         END LOOP;

         RETURN TRUE;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.numeric_sql_expression_is_safe(TEXT, INTEGER[], BOOLEAN) OWNER TO admin"
    )

    unsafe_existing <- DBI::dbGetQuery(
      con,
      "SELECT 'correction' AS expression_type, correction_id AS record_id
       FROM continuous.corrections
       WHERE NOT continuous.numeric_sql_expression_is_safe(
         equation,
         ARRAY[1, 2]::INTEGER[],
         FALSE
       )
       UNION ALL
       SELECT 'compound' AS expression_type, timeseries_id AS record_id
       FROM continuous.timeseries_compounds
       WHERE NOT continuous.numeric_sql_expression_is_safe(
         expression_sql,
         ARRAY[]::INTEGER[],
         TRUE
       )"
    )
    if (nrow(unsafe_existing)) {
      stop(
        "Patch 55 found existing stored SQL that does not meet the new safety rules: ",
        paste(
          paste0(
            unsafe_existing$expression_type,
            " ",
            unsafe_existing$record_id
          ),
          collapse = ", "
        )
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.validate_correction_equation()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NOT continuous.numeric_sql_expression_is_safe(
           NEW.equation,
           ARRAY[1, 2]::INTEGER[],
           FALSE
         ) THEN
           RAISE EXCEPTION
             'equation must be a numeric expression using only $1, $2, and approved scalar functions.';
         END IF;
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.validate_correction_equation() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_correction_equation_trigger
       ON continuous.corrections"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_correction_equation_trigger
       BEFORE INSERT OR UPDATE OF equation ON continuous.corrections
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_correction_equation()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.validate_compound_expression()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NOT continuous.numeric_sql_expression_is_safe(
           NEW.expression_sql,
           ARRAY[]::INTEGER[],
           TRUE
         ) THEN
           RAISE EXCEPTION
             'expression_sql must be a numeric expression using member aliases and approved scalar functions.';
         END IF;
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.validate_compound_expression() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_compound_expression_trigger
       ON continuous.timeseries_compounds"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_compound_expression_trigger
       BEFORE INSERT OR UPDATE OF expression_sql
       ON continuous.timeseries_compounds
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_compound_expression()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.validate_guideline_value_rule()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         scan TEXT;
         explain_json JSONB;
         bad_schema TEXT;
       BEGIN
         IF NEW.algorithm_code = 'narrative' THEN
           IF NEW.bound_code IS NOT NULL THEN
             RAISE EXCEPTION
               'bound_code must be NULL when algorithm_code is narrative.';
           END IF;
         ELSIF NEW.bound_code IS NULL
            OR NEW.bound_code NOT IN ('lower', 'upper') THEN
           RAISE EXCEPTION
             'Numeric guideline rules must use bound_code lower or upper.';
         END IF;

         IF NEW.algorithm_code = 'constant' AND NEW.fixed_value IS NULL THEN
           RAISE EXCEPTION
             'fixed_value must be populated when algorithm_code is constant.';
         END IF;
         IF NEW.algorithm_code <> 'constant' AND NEW.fixed_value IS NOT NULL THEN
           RAISE EXCEPTION
             'fixed_value may only be populated when algorithm_code is constant.';
         END IF;

         IF NEW.algorithm_code = 'sql_scalar' THEN
           IF NEW.formula_sql IS NULL OR btrim(NEW.formula_sql) = '' THEN
             RAISE EXCEPTION
               'formula_sql must be populated when algorithm_code is sql_scalar.';
           END IF;

           scan := NEW.formula_sql;
           scan := regexp_replace(scan, '(?s)\\$[^$]*\\$.*?\\$[^$]*\\$', '', 'g');
           scan := regexp_replace(scan, '''([^''\\\\]|\\\\.)*''', '', 'g');
           scan := regexp_replace(scan, '--.*?(\\n|$)', '', 'g');
           scan := regexp_replace(scan, '/\\*.*?\\*/', '', 'gs');

           IF scan ~ ';' THEN
             RAISE EXCEPTION
               'formula_sql must be a single statement with no semicolons.';
           END IF;
           IF scan !~* '^[[:space:]]*\\(*[[:space:]]*(with[[:space:]]+.*select|select)([[:space:]]|\\()' THEN
             RAISE EXCEPTION
               'formula_sql must begin with SELECT or WITH ... SELECT.';
           END IF;
           IF scan ~ '\\$[2-9][0-9]*' THEN
             RAISE EXCEPTION
               'Only $1 may be used as a parameter placeholder in formula_sql.';
           END IF;
           IF scan ~* '\\m(alter|analyze|call|checkpoint|cluster|copy|create|deallocate|delete|discard|do|drop|execute|grant|insert|listen|lock|merge|notify|prepare|refresh|reindex|reset|revoke|set|truncate|unlisten|update|vacuum)\\M'
              OR scan ~* '\\m(dblink|lo_export|lo_import|pg_cancel_backend|pg_sleep|pg_terminate_backend)[[:space:]]*\\(' THEN
             RAISE EXCEPTION
               'formula_sql may not modify data, database objects, sessions, or server processes.';
           END IF;

           IF scan ~ '\\$1' THEN
             EXECUTE format(
               'EXPLAIN (VERBOSE, FORMAT JSON)
                WITH q AS (%s)
                SELECT (SELECT * FROM q)::numeric',
               NEW.formula_sql
             )
             INTO explain_json
             USING NULL::INTEGER;
           ELSE
             EXECUTE format(
               'EXPLAIN (VERBOSE, FORMAT JSON)
                WITH q AS (%s)
                SELECT (SELECT * FROM q)::numeric',
               NEW.formula_sql
             )
             INTO explain_json;
           END IF;

           IF explain_json::TEXT ~ '\"Node Type\": \"ModifyTable\"' THEN
             RAISE EXCEPTION
               'formula_sql may not contain a data-modifying plan.';
           END IF;

           WITH RECURSIVE plan_nodes AS (
             SELECT (explain_json->0->'Plan') AS n
             UNION ALL
             SELECT child
             FROM plan_nodes p
             CROSS JOIN LATERAL jsonb_array_elements(
               COALESCE(p.n->'Plans', '[]'::jsonb)
             ) AS children(child)
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
             AND schem <> ALL(ARRAY['criteria', 'discrete', 'public'])
           LIMIT 1;

           IF bad_schema IS NOT NULL THEN
             RAISE EXCEPTION
               'formula_sql references disallowed schema: %',
               bad_schema;
           END IF;
         ELSIF NEW.formula_sql IS NOT NULL THEN
           RAISE EXCEPTION
             'formula_sql may only be populated when algorithm_code is sql_scalar.';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION criteria.validate_guideline_value_rule() OWNER TO admin"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '55'
       WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(packageVersion("AquaCache")))
    )

    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message("Patch 55 applied successfully. Stored SQL expressions are constrained.")
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
