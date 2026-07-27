# Patch 55: constrain stored SQL executed by correction, compound, and
# guideline evaluation functions, enforce non-overlapping time and depth
# intervals without concurrency races, and standardize correction intervals as
# half-open ranges. Harden audit coverage, attribution, row-level access, and
# high-volume measurement writes. Begin point-in-time auditing of continuous QC
# and daily-calculation dependencies so historical plots can be reconstructed
# from source history rather than auditing the derived daily table.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 55: constraining stored SQL expressions and intervals, standardizing correction endpoints, and hardening historical audit reconstruction. Changes are being made within a transaction, so an error will roll back the database."
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
         to_regclass('continuous.approvals') IS NOT NULL AS has_approvals,
         to_regclass('continuous.grades') IS NOT NULL AS has_grades,
         to_regclass('continuous.owners') IS NOT NULL AS has_owners,
         to_regclass('continuous.contributors') IS NOT NULL AS has_contributors,
         to_regclass('continuous.qualifiers') IS NOT NULL AS has_qualifiers,
         to_regclass('continuous.forecasts') IS NOT NULL AS has_forecasts,
         to_regclass('boreholes.geology') IS NOT NULL AS has_geology,
         to_regclass('boreholes.permafrost') IS NOT NULL AS has_permafrost,
         to_regclass('criteria.guideline_value_rules') IS NOT NULL AS has_guidelines,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info,
         to_regprocedure('criteria.validate_guideline_value_rule()') IS NOT NULL AS has_guideline_validator,
         to_regprocedure('continuous.apply_corrections(integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections,
         to_regprocedure('continuous.apply_corrections_at(timestamp with time zone, integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections_at,
         EXISTS (
           SELECT 1
           FROM pg_extension
           WHERE extname = 'btree_gist'
         ) AS has_btree_gist"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 55 requires correction, compound-timeseries, continuous interval, forecast, borehole interval, guideline, historical audit, and btree_gist objects from earlier patches."
      )
    }

    duplicate_correction_priorities <- DBI::dbGetQuery(
      con,
      "SELECT
         priority,
         string_agg(correction_type, ', ' ORDER BY correction_type) AS correction_types
       FROM continuous.correction_types
       GROUP BY priority
       HAVING count(*) > 1
       ORDER BY priority"
    )
    if (nrow(duplicate_correction_priorities)) {
      duplicate_summary <- vapply(
        seq_len(nrow(duplicate_correction_priorities)),
        function(row_number) {
          paste0(
            duplicate_correction_priorities$priority[[row_number]],
            " (",
            duplicate_correction_priorities$correction_types[[row_number]],
            ")"
          )
        },
        character(1)
      )
      stop(
        "Patch 55 found duplicate correction type priorities. Assign unique priorities before applying this patch: ",
        paste(duplicate_summary, collapse = "; ")
      )
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.correction_types
       DROP CONSTRAINT IF EXISTS correction_types_priority_key"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.correction_types
       ADD CONSTRAINT correction_types_priority_key UNIQUE (priority)"
    )

    message("Replacing overlap triggers with exclusion constraints...")

    # Exclusion constraints use GiST predicate locking and remain correct when
    # concurrent transactions try to add conflicting intervals. The retired
    # trigger functions only compared against rows visible to their transaction.
    obsolete_overlap_triggers <- c(
      "DROP TRIGGER IF EXISTS check_approvals_overlap
       ON continuous.approvals",
      "DROP TRIGGER IF EXISTS check_grades_overlap
       ON continuous.grades",
      "DROP TRIGGER IF EXISTS check_owners_overlap
       ON continuous.owners",
      "DROP TRIGGER IF EXISTS check_contributors_overlap
       ON continuous.contributors",
      "DROP TRIGGER IF EXISTS check_qualifiers_overlap
       ON continuous.qualifiers",
      "DROP TRIGGER IF EXISTS trg_geology_no_overlap
       ON boreholes.geology",
      "DROP TRIGGER IF EXISTS trg_permafrost_no_overlap
       ON boreholes.permafrost"
    )
    for (statement in obsolete_overlap_triggers) {
      DBI::dbExecute(con, statement)
    }

    obsolete_overlap_functions <- c(
      "DROP FUNCTION IF EXISTS continuous.check_approvals_overlap()",
      "DROP FUNCTION IF EXISTS continuous.check_grades_overlap()",
      "DROP FUNCTION IF EXISTS continuous.check_owners_overlap()",
      "DROP FUNCTION IF EXISTS continuous.check_contributors_overlap()",
      "DROP FUNCTION IF EXISTS continuous.check_qualifiers_overlap()",
      "DROP FUNCTION IF EXISTS boreholes.prevent_geology_overlap()",
      "DROP FUNCTION IF EXISTS boreholes.prevent_permafrost_overlap()"
    )
    for (statement in obsolete_overlap_functions) {
      DBI::dbExecute(con, statement)
    }

    obsolete_location_trigger_functions <- c(
      "DROP FUNCTION IF EXISTS public.check_locations_metadata_acquisition_instruments()",
      "DROP FUNCTION IF EXISTS public.fill_locations_metadata_acquisition_missing()",
      "DROP FUNCTION IF EXISTS public.fill_locations_metadata_owners_operators_missing()",
      "DROP FUNCTION IF EXISTS public.fill_locations_metadata_transmission_missing()"
    )
    for (statement in obsolete_location_trigger_functions) {
      DBI::dbExecute(con, statement)
    }
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS criteria.get_guideline_value(
         INTEGER,
         INTEGER
       )"
    )

    # location_id is already the primary key. Keeping a second UNIQUE
    # constraint on the same column only duplicates index maintenance.
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations
       DROP CONSTRAINT IF EXISTS unique_location_id"
    )

    message("Repairing and activating forecast retention cleanup...")

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS delete_old_forecasts_trigger
       ON continuous.forecasts"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS continuous.delete_old_forecasts()"
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION continuous.delete_old_forecasts()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, continuous
       AS $function$
       BEGIN
         DELETE FROM continuous.forecasts
         WHERE datetime < CURRENT_TIMESTAMP - INTERVAL '2 weeks';
         RETURN NULL;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.delete_old_forecasts() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.delete_old_forecasts()
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS forecasts_datetime_idx
       ON continuous.forecasts (datetime)"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER delete_old_forecasts_trigger
       AFTER INSERT OR UPDATE ON continuous.forecasts
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.delete_old_forecasts()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.delete_old_forecasts() IS
       'Statement-level forecast retention trigger that removes rows whose valid datetime is more than two weeks old.'"
    )

    interval_constraints <- c(
      "ALTER TABLE continuous.approvals
       DROP CONSTRAINT IF EXISTS approvals_period_valid",
      "ALTER TABLE continuous.approvals
       ADD CONSTRAINT approvals_period_valid
       CHECK (start_dt <= end_dt)",
      "ALTER TABLE continuous.approvals
       DROP CONSTRAINT IF EXISTS approvals_no_overlap",
      "ALTER TABLE continuous.approvals
       ADD CONSTRAINT approvals_no_overlap
       EXCLUDE USING gist (
         timeseries_id WITH =,
         tstzrange(start_dt, end_dt, '[)') WITH &&
       )
       DEFERRABLE INITIALLY DEFERRED",
      "ALTER TABLE continuous.grades
       DROP CONSTRAINT IF EXISTS grades_period_valid",
      "ALTER TABLE continuous.grades
       ADD CONSTRAINT grades_period_valid
       CHECK (start_dt <= end_dt)",
      "ALTER TABLE continuous.grades
       DROP CONSTRAINT IF EXISTS grades_no_overlap",
      "ALTER TABLE continuous.grades
       ADD CONSTRAINT grades_no_overlap
       EXCLUDE USING gist (
         timeseries_id WITH =,
         tstzrange(start_dt, end_dt, '[)') WITH &&
       )
       DEFERRABLE INITIALLY DEFERRED",
      "ALTER TABLE continuous.owners
       DROP CONSTRAINT IF EXISTS owners_period_valid",
      "ALTER TABLE continuous.owners
       ADD CONSTRAINT owners_period_valid
       CHECK (start_dt <= end_dt)",
      "ALTER TABLE continuous.owners
       DROP CONSTRAINT IF EXISTS owners_no_overlap",
      "ALTER TABLE continuous.owners
       ADD CONSTRAINT owners_no_overlap
       EXCLUDE USING gist (
         timeseries_id WITH =,
         tstzrange(start_dt, end_dt, '[)') WITH &&
       )
       DEFERRABLE INITIALLY DEFERRED",
      "ALTER TABLE continuous.contributors
       DROP CONSTRAINT IF EXISTS contributors_period_valid",
      "ALTER TABLE continuous.contributors
       ADD CONSTRAINT contributors_period_valid
       CHECK (start_dt <= end_dt)",
      "ALTER TABLE continuous.contributors
       DROP CONSTRAINT IF EXISTS contributors_no_overlap",
      "ALTER TABLE continuous.contributors
       ADD CONSTRAINT contributors_no_overlap
       EXCLUDE USING gist (
         timeseries_id WITH =,
         tstzrange(start_dt, end_dt, '[)') WITH &&
       )
       DEFERRABLE INITIALLY DEFERRED",
      "ALTER TABLE continuous.qualifiers
       DROP CONSTRAINT IF EXISTS qualifiers_period_valid",
      "ALTER TABLE continuous.qualifiers
       ADD CONSTRAINT qualifiers_period_valid
       CHECK (start_dt <= end_dt)",
      "ALTER TABLE continuous.qualifiers
       DROP CONSTRAINT IF EXISTS qualifiers_no_overlap",
      "ALTER TABLE continuous.qualifiers
       ADD CONSTRAINT qualifiers_no_overlap
       EXCLUDE USING gist (
         timeseries_id WITH =,
         qualifier_type_id WITH =,
         tstzrange(start_dt, end_dt, '[)') WITH &&
       )
       DEFERRABLE INITIALLY DEFERRED",
      "ALTER TABLE boreholes.geology
       DROP CONSTRAINT IF EXISTS geology_depth_valid",
      "ALTER TABLE boreholes.geology
       ADD CONSTRAINT geology_depth_valid
       CHECK (
         depth_from_m >= 0
         AND depth_to_m >= 0
         AND depth_from_m < depth_to_m
       )",
      "ALTER TABLE boreholes.geology
       DROP CONSTRAINT IF EXISTS geology_no_overlap",
      "ALTER TABLE boreholes.geology
       ADD CONSTRAINT geology_no_overlap
       EXCLUDE USING gist (
         borehole_id WITH =,
         numrange(depth_from_m, depth_to_m, '[)') WITH &&
       )
       DEFERRABLE INITIALLY DEFERRED",
      "ALTER TABLE boreholes.permafrost
       DROP CONSTRAINT IF EXISTS permafrost_depth_valid",
      "ALTER TABLE boreholes.permafrost
       ADD CONSTRAINT permafrost_depth_valid
       CHECK (
         (
           depth_from_m IS NULL
           AND depth_to_m IS NULL
         )
         OR (
           depth_from_m IS NOT NULL
           AND depth_from_m >= 0
           AND (
             depth_to_m IS NULL
             OR (
               depth_to_m >= 0
               AND depth_from_m < depth_to_m
             )
           )
         )
       )",
      "ALTER TABLE boreholes.permafrost
       DROP CONSTRAINT IF EXISTS permafrost_no_overlap",
      "ALTER TABLE boreholes.permafrost
       ADD CONSTRAINT permafrost_no_overlap
       EXCLUDE USING gist (
         borehole_id WITH =,
         numrange(depth_from_m, depth_to_m, '[)') WITH &&
       )
       WHERE (depth_from_m IS NOT NULL)
       DEFERRABLE INITIALLY DEFERRED"
    )
    for (statement in interval_constraints) {
      DBI::dbExecute(con, statement)
    }

    message("Standardizing correction intervals as half-open ranges...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.apply_corrections(
         p_timeseries_id INTEGER,
         p_datetime TIMESTAMPTZ,
         p_value NUMERIC
       )
       RETURNS NUMERIC
       LANGUAGE plpgsql
       SET search_path = pg_catalog, continuous
       AS $function$
       DECLARE
         corrected_value NUMERIC := p_value;
         correction_row RECORD;
         time_since_start NUMERIC;
         time_window NUMERIC;
         rate NUMERIC;
         correction NUMERIC;
       BEGIN
         IF p_value IS NULL THEN
           RETURN NULL;
         END IF;

         FOR correction_row IN
           SELECT
             c.correction_id,
             c.value1 AS c_value1,
             c.value2 AS c_value2,
             c.timestep_window AS c_timestep_window,
             c.equation AS c_equation,
             c.start_dt,
             c.end_dt,
             ct.correction_type,
             ct.priority
           FROM continuous.corrections c
           JOIN continuous.correction_types ct
             ON c.correction_type = ct.correction_type_id
           WHERE c.timeseries_id = p_timeseries_id
             AND tstzrange(c.start_dt, c.end_dt, '[)') @> p_datetime
           ORDER BY ct.priority ASC, c.correction_id ASC
         LOOP
           IF correction_row.correction_type = 'delete' THEN
             RETURN NULL;

           ELSIF correction_row.correction_type = 'trim' THEN
             IF correction_row.c_value1 IS NOT NULL
               AND corrected_value < correction_row.c_value1 THEN
               RETURN NULL;
             ELSIF correction_row.c_value2 IS NOT NULL
               AND corrected_value > correction_row.c_value2 THEN
               RETURN NULL;
             END IF;

           ELSIF correction_row.correction_type = 'offset linear' THEN
             corrected_value := corrected_value + correction_row.c_value1;

           ELSIF correction_row.correction_type = 'offset two-point' THEN
             time_since_start := EXTRACT(
               EPOCH FROM (p_datetime - correction_row.start_dt)
             );
             time_window := EXTRACT(
               EPOCH FROM (
                 correction_row.end_dt - correction_row.start_dt
               )
             );
             IF time_window <= 0 THEN
               RAISE EXCEPTION
                 'Invalid time window for offset two-point correction';
             END IF;
             rate := (
               correction_row.c_value2 - correction_row.c_value1
             ) / time_window;
             correction := correction_row.c_value1 +
               rate * time_since_start;
             corrected_value := corrected_value + correction;

           ELSIF correction_row.correction_type = 'scale' THEN
             corrected_value := corrected_value *
               (correction_row.c_value1 / 100.0);

           ELSIF correction_row.correction_type = 'drift linear' THEN
             time_since_start := EXTRACT(
               EPOCH FROM (p_datetime - correction_row.start_dt)
             );
             time_window := EXTRACT(
               EPOCH FROM correction_row.c_timestep_window
             );
             IF time_window <= 0 THEN
               RAISE EXCEPTION
                 'Invalid timestep_window for drift linear correction';
             END IF;
             rate := correction_row.c_value1 / time_window;
             correction := rate * time_since_start;
             corrected_value := corrected_value + correction;

           ELSIF correction_row.correction_type = 'drift equation' THEN
             time_since_start := EXTRACT(
               EPOCH FROM (p_datetime - correction_row.start_dt)
             );
             EXECUTE format('SELECT %s', correction_row.c_equation)
               INTO correction
               USING corrected_value, time_since_start;
             corrected_value := correction;

           ELSE
             RAISE NOTICE 'Correction type % not handled',
               correction_row.correction_type;
           END IF;
         END LOOP;

         RETURN corrected_value;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.apply_corrections_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_datetime TIMESTAMPTZ,
         p_value NUMERIC
       )
       RETURNS NUMERIC
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, continuous, audit
       AS $function$
       DECLARE
         corrected_value NUMERIC := p_value;
         correction_row RECORD;
         time_since_start NUMERIC;
         time_window NUMERIC;
         rate NUMERIC;
         correction NUMERIC;
       BEGIN
         IF p_value IS NULL THEN
           RETURN NULL;
         END IF;

         FOR correction_row IN
           SELECT
             c.correction_id,
             c.value1 AS c_value1,
             c.value2 AS c_value2,
             c.timestep_window AS c_timestep_window,
             c.equation AS c_equation,
             c.start_dt,
             c.end_dt,
             ct.correction_type,
             ct.priority
           FROM audit.corrections_as_of(
             p_as_of,
             ARRAY[p_timeseries_id]
           ) c
           JOIN audit.correction_types_as_of(p_as_of) ct
             ON c.correction_type = ct.correction_type_id
           WHERE c.timeseries_id = p_timeseries_id
             AND tstzrange(c.start_dt, c.end_dt, '[)') @> p_datetime
           ORDER BY ct.priority ASC, c.correction_id ASC
         LOOP
           IF correction_row.correction_type = 'delete' THEN
             RETURN NULL;

           ELSIF correction_row.correction_type = 'trim' THEN
             IF correction_row.c_value1 IS NOT NULL
               AND corrected_value < correction_row.c_value1 THEN
               RETURN NULL;
             ELSIF correction_row.c_value2 IS NOT NULL
               AND corrected_value > correction_row.c_value2 THEN
               RETURN NULL;
             END IF;

           ELSIF correction_row.correction_type = 'offset linear' THEN
             corrected_value := corrected_value + correction_row.c_value1;

           ELSIF correction_row.correction_type = 'offset two-point' THEN
             time_since_start := EXTRACT(
               EPOCH FROM (p_datetime - correction_row.start_dt)
             );
             time_window := EXTRACT(
               EPOCH FROM (
                 COALESCE(correction_row.end_dt, p_datetime) -
                 correction_row.start_dt
               )
             );
             IF time_window <= 0 THEN
               RAISE EXCEPTION
                 'Invalid time window for offset two-point correction';
             END IF;
             rate := (
               correction_row.c_value2 - correction_row.c_value1
             ) / time_window;
             correction := correction_row.c_value1 +
               rate * time_since_start;
             corrected_value := corrected_value + correction;

           ELSIF correction_row.correction_type = 'scale' THEN
             corrected_value := corrected_value *
               (correction_row.c_value1 / 100.0);

           ELSIF correction_row.correction_type = 'drift linear' THEN
             time_since_start := EXTRACT(
               EPOCH FROM (p_datetime - correction_row.start_dt)
             );
             time_window := EXTRACT(
               EPOCH FROM correction_row.c_timestep_window
             );
             IF time_window <= 0 THEN
               RAISE EXCEPTION
                 'Invalid timestep_window for drift linear correction';
             END IF;
             rate := correction_row.c_value1 / time_window;
             correction := rate * time_since_start;
             corrected_value := corrected_value + correction;

           ELSIF correction_row.correction_type = 'drift equation' THEN
             time_since_start := EXTRACT(
               EPOCH FROM (p_datetime - correction_row.start_dt)
             );
             EXECUTE format('SELECT %s', correction_row.c_equation)
               INTO correction
               USING corrected_value, time_since_start;
             corrected_value := correction;

           ELSE
             RAISE NOTICE 'Correction type % not handled',
               correction_row.correction_type;
           END IF;
         END LOOP;

         RETURN corrected_value;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.numeric_sql_expression_is_safe(
         expression_sql TEXT,
         allowed_placeholders INTEGER[],
         allow_value_identifiers BOOLEAN
       )
       RETURNS BOOLEAN
       LANGUAGE plpgsql
       VOLATILE
       AS $function$
       DECLARE
         scan TEXT;
         word_scan TEXT;
         parse_scan TEXT;
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

         -- Reject every character and token shape outside the intentionally
         -- small numeric-expression grammar before asking PostgreSQL to parse
         -- the expression.
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

         -- Remove complete numeric literals before scanning words. Otherwise
         -- the exponent marker in a valid value such as 1e-3 is mistaken for
         -- an identifier.
         word_scan := regexp_replace(
           scan,
           '([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?',
           ' ',
           'g'
         );
         word_scan := regexp_replace(
           word_scan,
           '\\$[0-9]+',
           ' ',
           'g'
         );

         FOR token IN
           SELECT lower(match[1])
           FROM regexp_matches(
             word_scan,
             '([A-Za-z_][A-Za-z0-9_]*)',
             'g'
           ) AS match
         LOOP
           IF token = ANY(forbidden_words) THEN
             RETURN FALSE;
           END IF;
           IF NOT allow_value_identifiers AND token <> ALL(approved_words) THEN
             RETURN FALSE;
           END IF;
         END LOOP;

         -- Only built-in numeric casts are allowed. In compound expressions,
         -- other bare identifiers are member aliases, not arbitrary type
         -- names.
         FOR token IN
           SELECT lower(match[1])
           FROM regexp_matches(
             scan,
             '::[[:space:]]*([A-Za-z_][A-Za-z0-9_]*)',
             'g'
           ) AS match
         LOOP
           IF token <> ALL(ARRAY['double', 'numeric', 'real']) THEN
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

         -- Lexical safety alone accepts malformed expressions such as '$1 +'
         -- or 'CASE END'. Let PostgreSQL parse and type-check the complete
         -- expression. Placeholders are numeric by contract. Compound member
         -- aliases are not in scope here, so an undefined-column error is
         -- acceptable only for compound expressions; syntax and all other
         -- analysis errors remain failures.
         parse_scan := regexp_replace(
           scan,
           '\\$[0-9]+',
           'NULL::NUMERIC',
           'g'
         );
         BEGIN
           EXECUTE format(
             'EXPLAIN (FORMAT JSON) SELECT (%s)::NUMERIC',
             parse_scan
           );
         EXCEPTION
           WHEN undefined_column THEN
             RETURN allow_value_identifiers;
           WHEN OTHERS THEN
             RETURN FALSE;
         END;

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
      "REVOKE ALL ON FUNCTION continuous.validate_correction_equation()
       FROM PUBLIC"
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
      "REVOKE ALL ON FUNCTION continuous.validate_compound_expression()
       FROM PUBLIC"
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
         called_schema TEXT;
         called_function TEXT;
         volatile_function TEXT;
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
              OR scan ~* '\\m(dblink|lo_export|lo_import|pg_cancel_backend|pg_sleep|pg_terminate_backend|set_config)[[:space:]]*\\(' THEN
             RAISE EXCEPTION
               'formula_sql may not modify data, database objects, sessions, or server processes.';
           END IF;

           -- EXPLAIN does not execute target-list functions, so a lexical
           -- command blacklist alone cannot detect callable side effects such
           -- as set_config(), nextval(), advisory locks, or extension
           -- functions. Reject every resolved VOLATILE function. Existing
           -- governed guideline helpers are STABLE and numeric built-ins are
           -- IMMUTABLE.
           FOR called_schema, called_function IN
             SELECT
               lower(NULLIF(function_match[1], '')),
               lower(function_match[2])
             FROM regexp_matches(
               scan,
               '\\m(?:([A-Za-z_][A-Za-z0-9_]*)\\.)?([A-Za-z_][A-Za-z0-9_]*)[[:space:]]*\\(',
               'g'
             ) AS matches(function_match)
           LOOP
             -- These SQL grammar tokens may legitimately precede an opening
             -- parenthesis but are not function calls.
             IF called_schema IS NULL
                AND called_function = ANY(ARRAY[
                  'as',
                  'case',
                  'filter',
                  'in',
                  'over',
                  'select',
                  'values',
                  'when'
                ]) THEN
               CONTINUE;
             END IF;

             IF called_schema IS NOT NULL
                AND called_schema <> ALL(ARRAY[
                  'pg_catalog',
                  'criteria',
                  'discrete',
                  'public'
                ]) THEN
               RAISE EXCEPTION
                 'formula_sql references disallowed function schema: %',
                 called_schema;
             END IF;

             SELECT format('%I.%I', n.nspname, p.proname)
             INTO volatile_function
             FROM pg_proc p
             JOIN pg_namespace n ON n.oid = p.pronamespace
             WHERE lower(p.proname) = called_function
               AND p.provolatile = 'v'
               AND (
                 (
                   called_schema IS NOT NULL
                   AND lower(n.nspname) = called_schema
                 )
                 OR (
                   called_schema IS NULL
                   AND lower(n.nspname) = ANY(ARRAY[
                     'pg_catalog',
                     'criteria',
                     'discrete',
                     'public'
                   ])
                 )
               )
             ORDER BY n.nspname, p.oid
             LIMIT 1;

             IF volatile_function IS NOT NULL THEN
               RAISE EXCEPTION
                 'formula_sql may not call volatile function %.',
                 volatile_function;
             END IF;
           END LOOP;

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
      "REVOKE ALL ON FUNCTION criteria.validate_guideline_value_rule()
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_value_rule
       ON criteria.guideline_value_rules"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_value_rule
       BEFORE INSERT OR UPDATE
       ON criteria.guideline_value_rules
       FOR EACH ROW
       EXECUTE FUNCTION criteria.validate_guideline_value_rule()"
    )

    # Validate existing rules without updating their modified metadata. A
    # temporary four-column table lets PostgreSQL invoke the same row trigger
    # for every stored rule before the patch can commit.
    DBI::dbExecute(
      con,
      "DROP TABLE IF EXISTS pg_temp.patch55_guideline_rule_validation"
    )
    DBI::dbExecute(
      con,
      "CREATE TEMP TABLE patch55_guideline_rule_validation (
         algorithm_code TEXT,
         bound_code TEXT,
         fixed_value NUMERIC,
         formula_sql TEXT
       ) ON COMMIT DROP"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_value_rule
       BEFORE INSERT
       ON patch55_guideline_rule_validation
       FOR EACH ROW
       EXECUTE FUNCTION criteria.validate_guideline_value_rule()"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO patch55_guideline_rule_validation (
         algorithm_code,
         bound_code,
         fixed_value,
         formula_sql
       )
       SELECT
         algorithm_code,
         bound_code,
         fixed_value,
         formula_sql
       FROM criteria.guideline_value_rules"
    )
    DBI::dbExecute(
      con,
      "DROP TABLE patch55_guideline_rule_validation"
    )

    message(
      "Hardening audit coverage, attribution, access, and write volume..."
    )

    audit_required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('audit.general_log') IS NOT NULL AS has_general_log,
         to_regclass('audit.measurements_continuous_log') IS NOT NULL AS has_measurement_log,
         to_regprocedure('audit.jsonb_changed_fields(jsonb,jsonb)') IS NOT NULL AS has_json_diff"
    )
    if (!all(unlist(audit_required[1, ], use.names = FALSE))) {
      stop(
        "Audit hardening requires the patch 37 audit tables and ",
        "audit.jsonb_changed_fields()."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit.history_boundaries (
         history_name TEXT PRIMARY KEY,
         history_started_at TIMESTAMPTZ NOT NULL
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.history_boundaries OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE audit.history_boundaries FROM PUBLIC"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit.table_registry (
         schema_name TEXT NOT NULL,
         table_name TEXT NOT NULL,
         capture_mode TEXT NOT NULL CHECK (
           capture_mode IN (
             'generic_insert_update_delete',
             'generic_update_delete',
             'specialized_measurement',
             'specialized_qc',
             'excluded_reconstructible',
             'excluded_derived',
             'excluded_payload'
           )
         ),
         rationale TEXT NOT NULL,
         history_started_at TIMESTAMPTZ,
         updated_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
         PRIMARY KEY (schema_name, table_name)
       )"
    )
    DBI::dbExecute(con, "ALTER TABLE audit.table_registry OWNER TO admin")
    DBI::dbExecute(con, "REVOKE ALL ON TABLE audit.table_registry FROM PUBLIC")
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.table_registry
       DROP CONSTRAINT IF EXISTS table_registry_capture_mode_check"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.table_registry
       ADD CONSTRAINT table_registry_capture_mode_check CHECK (
         capture_mode IN (
           'generic_insert_update_delete',
           'generic_update_delete',
           'specialized_measurement',
           'specialized_qc',
           'excluded_reconstructible',
           'excluded_derived',
           'excluded_payload'
         )
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE audit.general_log
       DROP CONSTRAINT IF EXISTS general_log_action_check"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.general_log
       DROP CONSTRAINT IF EXISTS general_log_payload_consistency"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.general_log
       ADD CONSTRAINT general_log_action_check
       CHECK (action IN ('INSERT', 'UPDATE', 'DELETE'))"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.general_log
       ADD CONSTRAINT general_log_payload_consistency CHECK (
         (
           action = 'INSERT'
           AND original_data = '{}'::JSONB
           AND new_data IS NOT NULL
           AND changed_fields IS NOT NULL
         )
         OR (
           action = 'UPDATE'
           AND new_data IS NOT NULL
           AND changed_fields IS NOT NULL
         )
         OR (
           action = 'DELETE'
           AND new_data IS NULL
           AND changed_fields IS NULL
         )
       )"
    )

    # SECURITY DEFINER makes current_user the function owner. session_user is
    # the immutable database login and aquacache.audit_user is the optional
    # application actor asserted by that login.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.if_modified_func()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := session_user::TEXT;
         v_actor_user TEXT := NULLIF(
           current_setting('aquacache.audit_user', true),
           ''
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_skip_audit BOOLEAN := FALSE;
         v_row_created TIMESTAMPTZ;
         v_row_modified TIMESTAMPTZ;
         v_old_data JSONB;
         v_new_data JSONB;
         v_changed_fields JSONB;
         v_old_end_dt TIMESTAMPTZ;
         v_new_end_dt TIMESTAMPTZ;
         v_segment_start_dt TIMESTAMPTZ;
         v_timeseries_id INTEGER;
       BEGIN
         IF TG_OP = 'INSERT' THEN
           v_row_created := (to_jsonb(NEW) ->> 'created')::TIMESTAMPTZ;
           v_row_modified := (to_jsonb(NEW) ->> 'modified')::TIMESTAMPTZ;
           v_new_data := to_jsonb(NEW) - 'created' - 'modified';
           v_changed_fields := v_new_data;

           INSERT INTO audit.general_log (
             schema_name,
             table_name,
             user_name,
             actor_user,
             application_name,
             action,
             row_created,
             row_modified,
             original_data,
             new_data,
             changed_fields,
             action_timestamp,
             transaction_id
           ) VALUES (
             TG_TABLE_SCHEMA,
             TG_TABLE_NAME,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             v_row_created,
             v_row_modified,
             '{}'::JSONB,
             v_new_data,
             v_changed_fields,
             clock_timestamp(),
             txid_current()
           );
           RETURN NEW;
         END IF;

         IF TG_OP = 'DELETE' THEN
           v_row_created := (to_jsonb(OLD) ->> 'created')::TIMESTAMPTZ;
           v_row_modified := (to_jsonb(OLD) ->> 'modified')::TIMESTAMPTZ;
           -- Preserve row-level authorship on deletions. Timestamps remain in
           -- their typed columns to avoid duplicating them in the JSON payload.
           v_old_data := to_jsonb(OLD) - 'created' - 'modified';

           INSERT INTO audit.general_log (
             schema_name,
             table_name,
             user_name,
             actor_user,
             application_name,
             action,
             row_created,
             row_modified,
             original_data,
             new_data,
             changed_fields,
             action_timestamp,
             transaction_id
           ) VALUES (
             TG_TABLE_SCHEMA,
             TG_TABLE_NAME,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             v_row_created,
             v_row_modified,
             v_old_data,
             NULL,
             NULL,
             clock_timestamp(),
             txid_current()
           );
           RETURN OLD;
         END IF;

         v_row_created := COALESCE(
           (to_jsonb(NEW) ->> 'created')::TIMESTAMPTZ,
           (to_jsonb(OLD) ->> 'created')::TIMESTAMPTZ
         );
         v_row_modified := COALESCE(
           (to_jsonb(NEW) ->> 'modified')::TIMESTAMPTZ,
           (to_jsonb(OLD) ->> 'modified')::TIMESTAMPTZ
         );
         v_old_data := to_jsonb(OLD)
           - 'created' - 'modified' - 'created_by' - 'modified_by';
         v_new_data := to_jsonb(NEW)
           - 'created' - 'modified' - 'created_by' - 'modified_by';

         IF TG_TABLE_SCHEMA = 'continuous' AND TG_TABLE_NAME = 'timeseries' THEN
           v_old_data := v_old_data
             - 'last_new_data'
             - 'last_synchronize'
             - 'last_daily_calculation'
             - 'end_datetime'
             - 'start_datetime';
           v_new_data := v_new_data
             - 'last_new_data'
             - 'last_synchronize'
             - 'last_daily_calculation'
             - 'end_datetime'
             - 'start_datetime';
         END IF;

         v_changed_fields := audit.jsonb_changed_fields(
           v_old_data,
           v_new_data
         );

         IF
           TG_OP = 'UPDATE'
           AND TG_TABLE_SCHEMA = 'continuous'
           AND TG_TABLE_NAME = ANY(ARRAY[
             'approvals',
             'grades',
             'qualifiers',
             'owners',
             'contributors'
           ])
           AND v_changed_fields ? 'end_dt'
           AND (v_changed_fields - 'end_dt') = '{}'::JSONB
         THEN
           v_old_end_dt := (to_jsonb(OLD) ->> 'end_dt')::TIMESTAMPTZ;
           v_new_end_dt := (to_jsonb(NEW) ->> 'end_dt')::TIMESTAMPTZ;
           v_segment_start_dt := (
             to_jsonb(NEW) ->> 'start_dt'
           )::TIMESTAMPTZ;
           v_timeseries_id := (
             to_jsonb(NEW) ->> 'timeseries_id'
           )::INTEGER;

           IF v_new_end_dt > v_old_end_dt THEN
             EXECUTE format(
               'SELECT NOT EXISTS (
                  SELECT 1
                  FROM %I.%I t
                  WHERE t.timeseries_id = $1
                    AND t.start_dt > $2
                )',
               TG_TABLE_SCHEMA,
               TG_TABLE_NAME
             )
             INTO v_skip_audit
             USING v_timeseries_id, v_segment_start_dt;
           END IF;
         END IF;

         IF v_changed_fields = '{}'::JSONB OR v_skip_audit THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.general_log (
           schema_name,
           table_name,
           user_name,
           actor_user,
           application_name,
           action,
           row_created,
           row_modified,
           original_data,
           new_data,
           changed_fields,
           action_timestamp,
           transaction_id
         ) VALUES (
           TG_TABLE_SCHEMA,
           TG_TABLE_NAME,
           v_user_name,
           v_actor_user,
           v_application_name,
           TG_OP,
           v_row_created,
           v_row_modified,
           v_old_data,
           v_new_data,
           v_changed_fields,
           clock_timestamp(),
           txid_current()
         );
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.if_modified_func() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.if_modified_func() FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.if_modified_func() IS
       'Generic audit trigger for moderate-volume tables. Captures INSERT, meaningful UPDATE, and DELETE actions; user_name is the database login and actor_user is an optional application actor.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.log_measurements_continuous_change()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := session_user::TEXT;
         v_actor_user TEXT := NULLIF(
           current_setting('aquacache.audit_user', true),
           ''
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_old_data JSONB;
         v_new_data JSONB;
         v_business_old JSONB;
         v_business_new JSONB;
         v_changed_fields JSONB;
       BEGIN
         IF TG_OP = 'DELETE' THEN
           INSERT INTO audit.measurements_continuous_log (
             timeseries_id,
             measurement_datetime,
             user_name,
             actor_user,
             application_name,
             action,
             action_timestamp,
             original_data,
             new_data,
             changed_fields,
             transaction_id
           ) VALUES (
             OLD.timeseries_id,
             OLD.datetime,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             clock_timestamp(),
             to_jsonb(OLD),
             NULL,
             NULL,
             txid_current()
           );
           RETURN OLD;
         END IF;

         v_old_data := to_jsonb(OLD);
         v_new_data := to_jsonb(NEW);
         v_business_old := v_old_data
           - 'created' - 'modified' - 'created_by' - 'modified_by';
         v_business_new := v_new_data
           - 'created' - 'modified' - 'created_by' - 'modified_by';
         v_changed_fields := audit.jsonb_changed_fields(
           v_business_old,
           v_business_new
         );

         IF v_changed_fields = '{}'::JSONB THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.measurements_continuous_log (
           timeseries_id,
           measurement_datetime,
           user_name,
           actor_user,
           application_name,
           action,
           action_timestamp,
           original_data,
           new_data,
           changed_fields,
           transaction_id
         ) VALUES (
           OLD.timeseries_id,
           OLD.datetime,
           v_user_name,
           v_actor_user,
           v_application_name,
           TG_OP,
           clock_timestamp(),
           v_old_data,
           v_new_data,
           v_changed_fields,
           txid_current()
         );
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.log_measurements_continuous_change() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.log_measurements_continuous_change()
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.log_measurements_continuous_change() IS
       'Audits meaningful continuous measurement updates and deletions. Metadata-only timestamp/actor updates are suppressed; unchanged UPSERT conflicts generate no row update.'"
    )

    added_audit_targets <- data.frame(
      schema_name = c(
        rep("continuous", 7),
        rep("discrete", 8),
        rep("instruments", 7),
        rep("public", 3),
        "boreholes"
      ),
      table_name = c(
        "aggregation_types",
        "timeseries_types",
        "owners",
        "contributors",
        "rating_curves",
        "rating_curve_points",
        "rating_curve_shifts",
        "sample_documents",
        "cross_sections",
        "cross_section_verticals",
        "cross_section_points",
        "import_sources",
        "import_profiles",
        "import_parameter_mappings",
        "import_qualifier_mappings",
        "instrument_maintenance_due",
        "instrument_sensor_events",
        "instrument_sensor_event_slots",
        "sensor_makes",
        "sensor_models",
        "calibrate_depth",
        "calibrate_dissolved_oxygen",
        "locations_metadata_instrument_timeseries",
        "matrix_states",
        "organization_data_sharing_agreements",
        "drillers"
      ),
      stringsAsFactors = FALSE
    )

    for (i in seq_len(nrow(added_audit_targets))) {
      schema_name <- added_audit_targets$schema_name[[i]]
      table_name <- added_audit_targets$table_name[[i]]
      full_name <- paste0(schema_name, ".", table_name)
      exists <- DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1) IS NOT NULL AS exists",
        params = list(full_name)
      )$exists[[1]]
      if (!isTRUE(exists)) {
        stop("Required audit target does not exist: ", full_name)
      }

      trigger_name <- paste0("audit_", table_name, "_trigger")
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s ON %s.%s",
          trigger_name,
          schema_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          paste0(
            "CREATE TRIGGER %s ",
            "AFTER INSERT OR UPDATE OR DELETE ON %s.%s ",
            "FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
          ),
          trigger_name,
          schema_name,
          table_name
        )
      )
    }

    reconstruction_trigger_targets <- data.frame(
      schema_name = c(
        rep("continuous", 4),
        rep("public", 3)
      ),
      table_name = c(
        "corrections",
        "correction_types",
        "timeseries_compounds",
        "timeseries_compound_members",
        "grade_types",
        "approval_types",
        "qualifier_types"
      ),
      stringsAsFactors = FALSE
    )
    generic_trigger_targets <- DBI::dbGetQuery(
      con,
      "SELECT
         tn.nspname AS schema_name,
         tc.relname AS table_name,
         trg.tgname AS trigger_name
       FROM pg_trigger trg
       JOIN pg_class tc ON tc.oid = trg.tgrelid
       JOIN pg_namespace tn ON tn.oid = tc.relnamespace
       JOIN pg_proc p ON p.oid = trg.tgfoid
       JOIN pg_namespace pn ON pn.oid = p.pronamespace
       WHERE NOT trg.tgisinternal
         AND pn.nspname = 'audit'
         AND p.proname = 'if_modified_func'"
    )
    for (i in seq_len(nrow(generic_trigger_targets))) {
      schema_name <- generic_trigger_targets$schema_name[[i]]
      table_name <- generic_trigger_targets$table_name[[i]]
      trigger_name <- generic_trigger_targets$trigger_name[[i]]
      reconstruction_only <- any(
        reconstruction_trigger_targets$schema_name == schema_name &
          reconstruction_trigger_targets$table_name == table_name
      )
      trigger_events <- if (reconstruction_only) {
        "UPDATE OR DELETE"
      } else {
        "INSERT OR UPDATE OR DELETE"
      }

      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s ON %s.%s",
          trigger_name,
          schema_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          paste0(
            "CREATE TRIGGER %s AFTER %s ON %s.%s ",
            "FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
          ),
          trigger_name,
          trigger_events,
          schema_name,
          table_name
        )
      )
    }

    # Register every active audit trigger, including those installed by older
    # patches, so future schema reviews have a durable source of truth.
    DBI::dbExecute(
      con,
      "INSERT INTO audit.table_registry (
         schema_name,
         table_name,
         capture_mode,
         rationale,
         history_started_at,
         updated_at
       )
       SELECT
         tn.nspname,
         tc.relname,
         CASE
           WHEN tc.relname = 'measurements_continuous'
             THEN 'specialized_measurement'
           WHEN p.proname = 'log_continuous_qc_change'
             THEN 'specialized_qc'
           WHEN (tn.nspname, tc.relname) IN (
             ('continuous', 'corrections'),
             ('continuous', 'correction_types'),
             ('continuous', 'timeseries_compounds'),
             ('continuous', 'timeseries_compound_members'),
             ('public', 'grade_types'),
             ('public', 'approval_types'),
             ('public', 'qualifier_types')
           ) THEN 'generic_update_delete'
           ELSE 'generic_insert_update_delete'
         END,
         CASE
           WHEN tc.relname = 'measurements_continuous'
             THEN 'High-volume keyed audit log; INSERT is recoverable from row creation metadata.'
           WHEN p.proname = 'log_continuous_qc_change'
             THEN 'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.'
           WHEN (tn.nspname, tc.relname) IN (
             ('continuous', 'corrections'),
             ('continuous', 'correction_types'),
             ('continuous', 'timeseries_compounds'),
             ('continuous', 'timeseries_compound_members'),
             ('public', 'grade_types'),
             ('public', 'approval_types'),
             ('public', 'qualifier_types')
           ) THEN 'Creation is reconstructed from row metadata; UPDATE and DELETE retain compatibility with established as-of functions.'
           ELSE 'Material source or configuration table; INSERT, meaningful UPDATE, and DELETE are retained.'
         END,
         clock_timestamp(),
         clock_timestamp()
       FROM pg_trigger trg
       JOIN pg_class tc ON tc.oid = trg.tgrelid
       JOIN pg_namespace tn ON tn.oid = tc.relnamespace
       JOIN pg_proc p ON p.oid = trg.tgfoid
       JOIN pg_namespace pn ON pn.oid = p.pronamespace
       WHERE NOT trg.tgisinternal
         AND pn.nspname = 'audit'
       ON CONFLICT (schema_name, table_name) DO UPDATE
       SET capture_mode = EXCLUDED.capture_mode,
           rationale = EXCLUDED.rationale,
           history_started_at = CASE
             WHEN audit.table_registry.capture_mode IS DISTINCT FROM
               EXCLUDED.capture_mode
             THEN EXCLUDED.history_started_at
             ELSE audit.table_registry.history_started_at
           END,
           updated_at = EXCLUDED.updated_at"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit.table_registry (
         schema_name,
         table_name,
         capture_mode,
         rationale,
         history_started_at,
         updated_at
       ) VALUES
         (
           'continuous',
           'measurements_calculated_daily',
           'excluded_reconstructible',
           'Derived values are reconstructed from audited raw measurements and dependencies.',
           NULL,
           clock_timestamp()
         ),
         (
           'files',
           'documents',
           'excluded_payload',
           'Binary payload changes are intentionally excluded; document relationship metadata is audited.',
           NULL,
           clock_timestamp()
         )
       ON CONFLICT (schema_name, table_name) DO UPDATE
       SET capture_mode = EXCLUDED.capture_mode,
           rationale = EXCLUDED.rationale,
           history_started_at = EXCLUDED.history_started_at,
           updated_at = EXCLUDED.updated_at"
    )

    DBI::dbExecute(
      con,
      "DO $do$
       BEGIN
         IF NOT EXISTS (
           SELECT 1 FROM pg_roles WHERE rolname = 'audit_reviewer'
         ) THEN
           CREATE ROLE audit_reviewer NOLOGIN;
         END IF;
       END
       $do$"
    )
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA audit TO audit_reviewer")
    DBI::dbExecute(
      con,
      "GRANT SELECT ON audit.general_log,
                       audit.measurements_continuous_log,
                       audit.history_boundaries,
                       audit.table_registry
       TO audit_reviewer"
    )

    # Raw audit rows must not bypass source-table RLS. Ordinary readers retain
    # the SELECT grant required by invoker as-of functions, but policies expose
    # only visible continuous timeseries and globally readable reference data.
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.general_log ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS audit_general_log_select ON audit.general_log"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY audit_general_log_select
       ON audit.general_log
       FOR SELECT
       USING (
         pg_has_role(session_user, 'audit_reviewer', 'member')
         OR (
           schema_name = 'continuous'
           AND table_name IN (
             'correction_types',
             'aggregation_types',
             'timeseries_types'
           )
         )
         OR (
           schema_name = 'public'
           AND table_name IN (
             'grade_types',
             'approval_types',
             'qualifier_types'
           )
         )
         OR EXISTS (
           SELECT 1
           FROM continuous.timeseries visible
           WHERE visible.timeseries_id = CASE
             WHEN COALESCE(
               original_data ->> 'timeseries_id',
               new_data ->> 'timeseries_id'
             ) ~ '^[0-9]+$'
             THEN COALESCE(
               original_data ->> 'timeseries_id',
               new_data ->> 'timeseries_id'
             )::INTEGER
             ELSE NULL::INTEGER
           END
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.measurements_continuous_log
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS audit_measurements_continuous_log_select
       ON audit.measurements_continuous_log"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY audit_measurements_continuous_log_select
       ON audit.measurements_continuous_log
       FOR SELECT
       USING (
         pg_has_role(session_user, 'audit_reviewer', 'member')
         OR EXISTS (
           SELECT 1
           FROM continuous.timeseries visible
           WHERE visible.timeseries_id =
             measurements_continuous_log.timeseries_id
         )
       )"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.general_log IS
       'General-purpose audit log for moderate-volume source and configuration tables. Stores INSERT, meaningful UPDATE, and DELETE events; tables needed by established historical functions may retain UPDATE/DELETE capture with creation reconstructed from row metadata.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.user_name IS
       'Database login (session_user) that submitted the change; unlike current_user this is not replaced by a SECURITY DEFINER function owner.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.actor_user IS
       'Optional application-level actor supplied by the database login through aquacache.audit_user; NULL when no application actor was asserted.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.measurements_continuous_log IS
       'Audit log for meaningful updates and deletions of continuous measurements, keyed by timeseries and datetime. INSERT values remain in the source row until changed or deleted; metadata-only updates are suppressed.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.measurements_continuous_log.user_name IS
       'Database login (session_user) that submitted the measurement change.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.measurements_continuous_log.actor_user IS
       'Optional application-level actor supplied through aquacache.audit_user; NULL when none was asserted.'"
    )

    message("Adding point-in-time continuous QC reconstruction...")

    historical_qc_required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('audit.general_log') IS NOT NULL AS has_general_log,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_trigger,
         to_regprocedure('continuous.measurements_calculated_daily_at(timestamp with time zone, integer[], date, date)') IS NOT NULL AS has_daily_at,
         to_regclass('public.grade_types') IS NOT NULL AS has_grade_types,
         to_regclass('public.approval_types') IS NOT NULL AS has_approval_types,
         to_regclass('public.qualifier_types') IS NOT NULL AS has_qualifier_types"
    )
    if (!all(unlist(historical_qc_required[1, ], use.names = FALSE))) {
      stop(
        "Historical QC reconstruction requires the patch 37 audit objects, ",
        "continuous.measurements_calculated_daily_at(), and the continuous ",
        "QC interval/type tables."
      )
    }

    audit_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname
    historical_qc_roles <- intersect(
      c(
        "public_reader",
        "yg_editor_group",
        "yg_editor",
        "yg_reader_group",
        "yg_reader"
      ),
      audit_roles
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit.history_boundaries (
         history_name TEXT PRIMARY KEY,
         history_started_at TIMESTAMPTZ NOT NULL
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.history_boundaries OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE audit.history_boundaries FROM PUBLIC"
    )

    # The generic audit trigger intentionally suppresses routine end-date
    # extensions on temporal metadata. Historical QC reconstruction cannot
    # suppress those changes, because the interval end is part of the state
    # being reconstructed.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.log_continuous_qc_change()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := session_user::TEXT;
         v_actor_user TEXT := NULLIF(
           current_setting('aquacache.audit_user', true),
           ''
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_ignored_fields CONSTANT TEXT[] := ARRAY[
           'created',
           'modified',
           'created_by',
           'modified_by'
         ];
         v_row_created TIMESTAMPTZ;
         v_row_modified TIMESTAMPTZ;
         v_old_data JSONB;
         v_new_data JSONB;
         v_changed_fields JSONB;
       BEGIN
         v_row_created := (to_jsonb(OLD) ->> 'created')::TIMESTAMPTZ;
         v_row_modified := (to_jsonb(OLD) ->> 'modified')::TIMESTAMPTZ;
         v_old_data := to_jsonb(OLD) - v_ignored_fields;

         IF TG_OP = 'DELETE' THEN
           INSERT INTO audit.general_log (
             schema_name,
             table_name,
             user_name,
             actor_user,
             application_name,
             action,
             row_created,
             row_modified,
             original_data,
             new_data,
             changed_fields,
             action_timestamp,
             transaction_id
           ) VALUES (
             TG_TABLE_SCHEMA,
             TG_TABLE_NAME,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             v_row_created,
             v_row_modified,
             v_old_data,
             NULL,
             NULL,
             clock_timestamp(),
             txid_current()
           );
           RETURN OLD;
         END IF;

         v_row_modified := COALESCE(
           (to_jsonb(NEW) ->> 'modified')::TIMESTAMPTZ,
           v_row_modified
         );
         v_new_data := to_jsonb(NEW) - v_ignored_fields;
         v_changed_fields := audit.jsonb_changed_fields(
           v_old_data,
           v_new_data
         );
         IF v_changed_fields = '{}'::JSONB THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.general_log (
           schema_name,
           table_name,
           user_name,
           actor_user,
           application_name,
           action,
           row_created,
           row_modified,
           original_data,
           new_data,
           changed_fields,
           action_timestamp,
           transaction_id
         ) VALUES (
           TG_TABLE_SCHEMA,
           TG_TABLE_NAME,
           v_user_name,
           v_actor_user,
           v_application_name,
           TG_OP,
           v_row_created,
           v_row_modified,
           v_old_data,
           v_new_data,
           v_changed_fields,
           clock_timestamp(),
           txid_current()
         );
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.log_continuous_qc_change() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.log_continuous_qc_change() FROM PUBLIC"
    )

    qc_audit_targets <- data.frame(
      schema_name = c(
        rep("continuous", 3),
        rep("public", 3)
      ),
      table_name = c(
        "grades",
        "approvals",
        "qualifiers",
        "grade_types",
        "approval_types",
        "qualifier_types"
      ),
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(qc_audit_targets))) {
      schema_name <- qc_audit_targets$schema_name[[i]]
      table_name <- qc_audit_targets$table_name[[i]]
      trigger_name <- paste0("audit_", table_name, "_trigger")

      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s ON %s.%s",
          trigger_name,
          schema_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          paste0(
            "CREATE TRIGGER %s ",
            "AFTER UPDATE OR DELETE ON %s.%s ",
            "FOR EACH ROW EXECUTE FUNCTION audit.log_continuous_qc_change()"
          ),
          trigger_name,
          schema_name,
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "INSERT INTO audit.table_registry (
         schema_name,
         table_name,
         capture_mode,
         rationale,
         history_started_at,
         updated_at
       ) VALUES
         (
           'continuous',
           'grades',
           'specialized_qc',
           'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'approvals',
           'specialized_qc',
           'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'qualifiers',
           'specialized_qc',
           'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'public',
           'grade_types',
           'specialized_qc',
           'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'public',
           'approval_types',
           'specialized_qc',
           'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'public',
           'qualifier_types',
           'specialized_qc',
           'Point-in-time reconstruction requires every interval/type UPDATE and DELETE, including routine interval extensions.',
           clock_timestamp(),
           clock_timestamp()
         )
       ON CONFLICT (schema_name, table_name) DO UPDATE
       SET history_started_at = CASE
             WHEN audit.table_registry.capture_mode IS DISTINCT FROM
               EXCLUDED.capture_mode
             THEN EXCLUDED.history_started_at
             ELSE audit.table_registry.history_started_at
           END,
           capture_mode = EXCLUDED.capture_mode,
           rationale = EXCLUDED.rationale,
           updated_at = EXCLUDED.updated_at"
    )

    # Establish effective reliability boundaries only after all audit triggers
    # are attached. On a rerun, advance a stale boundary when a dependency's
    # capture mode began later; never move a reliable boundary backwards.
    DBI::dbExecute(
      con,
      "WITH boundary_values AS (
         SELECT
           'continuous_qc'::TEXT AS history_name,
           max(history_started_at) AS history_started_at
         FROM audit.table_registry
         WHERE (schema_name, table_name) IN (
           ('continuous', 'grades'),
           ('continuous', 'approvals'),
           ('continuous', 'qualifiers'),
           ('public', 'grade_types'),
           ('public', 'approval_types'),
           ('public', 'qualifier_types')
         )
         UNION ALL
         SELECT
           'continuous_daily_dependencies',
           max(history_started_at)
         FROM audit.table_registry
         WHERE (schema_name, table_name) IN (
           ('continuous', 'timeseries'),
           ('continuous', 'aggregation_types'),
           ('continuous', 'measurements_continuous'),
           ('continuous', 'corrections'),
           ('continuous', 'correction_types'),
           ('continuous', 'timeseries_compounds'),
           ('continuous', 'timeseries_compound_members'),
           ('continuous', 'grades'),
           ('public', 'grade_types')
         )
       )
       INSERT INTO audit.history_boundaries (
         history_name,
         history_started_at
       )
       SELECT history_name, history_started_at
       FROM boundary_values
       WHERE history_started_at IS NOT NULL
       ON CONFLICT (history_name) DO UPDATE
       SET history_started_at = GREATEST(
         audit.history_boundaries.history_started_at,
         EXCLUDED.history_started_at
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_continuous_qc_asof_idx
       ON audit.general_log (
         table_name,
         (COALESCE(
           original_data ->> 'timeseries_id',
           new_data ->> 'timeseries_id'
         )::INTEGER),
         action_timestamp,
         (COALESCE(
           original_data ->> 'grade_id',
           new_data ->> 'grade_id',
           original_data ->> 'approval_id',
           new_data ->> 'approval_id',
           original_data ->> 'qualifier_id',
           new_data ->> 'qualifier_id'
         )::INTEGER)
       )
       WHERE schema_name = 'continuous'
         AND table_name IN ('grades', 'approvals', 'qualifiers')"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_qc_types_asof_idx
       ON audit.general_log (
         table_name,
         (COALESCE(
           original_data ->> 'grade_type_id',
           new_data ->> 'grade_type_id',
           original_data ->> 'approval_type_id',
           new_data ->> 'approval_type_id',
           original_data ->> 'qualifier_type_id',
           new_data ->> 'qualifier_type_id'
         )::INTEGER),
         action_timestamp
       )
       WHERE schema_name = 'public'
         AND table_name IN (
           'grade_types',
           'approval_types',
           'qualifier_types'
         )"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.continuous_qc_rows_as_of(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL,
         p_start_datetime TIMESTAMPTZ DEFAULT NULL,
         p_end_datetime TIMESTAMPTZ DEFAULT NULL,
         p_qc_types TEXT[] DEFAULT NULL
       )
       RETURNS TABLE (
         qc_type TEXT,
         qc_row_id INTEGER,
         timeseries_id INTEGER,
         type_id INTEGER,
         start_dt TIMESTAMPTZ,
         end_dt TIMESTAMPTZ
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         requested_type TEXT;
         interval_table TEXT;
         row_id_column TEXT;
         type_id_column TEXT;
         history_started_at TIMESTAMPTZ;
       BEGIN
         SELECT max(boundary_value)
         INTO history_started_at
         FROM (
           SELECT hb.history_started_at AS boundary_value
           FROM audit.history_boundaries hb
           WHERE hb.history_name = 'continuous_qc'
           UNION ALL
           SELECT r.history_started_at
           FROM audit.table_registry r
           WHERE (r.schema_name, r.table_name) IN (
             ('continuous', 'grades'),
             ('continuous', 'approvals'),
             ('continuous', 'qualifiers'),
             ('public', 'grade_types'),
             ('public', 'approval_types'),
             ('public', 'qualifier_types')
           )
         ) boundaries;

         IF history_started_at IS NULL THEN
           RAISE EXCEPTION
             'Continuous QC history has not been initialized.';
         END IF;
         IF p_as_of < history_started_at THEN
           RAISE EXCEPTION
             'Continuous QC history is only reliable from % onward; requested as_of was %.',
             history_started_at,
             p_as_of;
         END IF;

         FOR requested_type IN
           SELECT DISTINCT requested.qc_type
           FROM unnest(
             COALESCE(
               p_qc_types,
               ARRAY['grade', 'approval', 'qualifier']::TEXT[]
             )
           ) AS requested(qc_type)
         LOOP
           CASE requested_type
             WHEN 'grade' THEN
               interval_table := 'grades';
               row_id_column := 'grade_id';
               type_id_column := 'grade_type_id';
             WHEN 'approval' THEN
               interval_table := 'approvals';
               row_id_column := 'approval_id';
               type_id_column := 'approval_type_id';
             WHEN 'qualifier' THEN
               interval_table := 'qualifiers';
               row_id_column := 'qualifier_id';
               type_id_column := 'qualifier_type_id';
             ELSE
               RAISE EXCEPTION
                 'Unsupported continuous QC type: %',
                 requested_type;
           END CASE;

           RETURN QUERY EXECUTE format(
             $sql$
               WITH current_rows AS (
                 SELECT
                   q.%1$I AS row_id,
                   q.timeseries_id,
                   q.%2$I AS type_id,
                   q.start_dt,
                   q.end_dt,
                   q.created AS row_created
                 FROM continuous.%3$I q
                 WHERE ($1 IS NULL OR q.timeseries_id = ANY($1))
               ),
               future_changes AS (
                 SELECT DISTINCT ON (
                   COALESCE(
                     g.original_data ->> %4$L,
                     g.new_data ->> %4$L
                   )
                 )
                   (g.original_data ->> %4$L)::INTEGER AS row_id,
                   (g.original_data ->> 'timeseries_id')::INTEGER AS timeseries_id,
                   (g.original_data ->> %5$L)::INTEGER AS type_id,
                   (g.original_data ->> 'start_dt')::TIMESTAMPTZ AS start_dt,
                   (g.original_data ->> 'end_dt')::TIMESTAMPTZ AS end_dt,
                   g.row_created
                 FROM audit.general_log g
                 WHERE g.schema_name = 'continuous'
                   AND g.table_name = %6$L
                   AND g.action_timestamp > $4
                   AND (
                     $1 IS NULL
                     OR COALESCE(
                       (g.original_data ->> 'timeseries_id')::INTEGER,
                       (g.new_data ->> 'timeseries_id')::INTEGER
                     ) = ANY($1)
                   )
                 ORDER BY
                   COALESCE(
                     g.original_data ->> %4$L,
                     g.new_data ->> %4$L
                   ),
                   g.action_timestamp,
                   g.log_id
               ),
               snapshot_rows AS (
                 SELECT
                   COALESCE(f.row_id, c.row_id) AS row_id,
                   CASE
                     WHEN f.row_id IS NOT NULL THEN f.timeseries_id
                     ELSE c.timeseries_id
                   END AS timeseries_id,
                   CASE
                     WHEN f.row_id IS NOT NULL THEN f.type_id
                     ELSE c.type_id
                   END AS type_id,
                   CASE
                     WHEN f.row_id IS NOT NULL THEN f.start_dt
                     ELSE c.start_dt
                   END AS start_dt,
                   CASE
                     WHEN f.row_id IS NOT NULL THEN f.end_dt
                     ELSE c.end_dt
                   END AS end_dt,
                   CASE
                     WHEN f.row_id IS NOT NULL THEN f.row_created
                     ELSE c.row_created
                   END AS row_created
                 FROM current_rows c
                 FULL OUTER JOIN future_changes f
                   ON f.row_id = c.row_id
               )
               SELECT
                 $5::TEXT AS qc_type,
                 s.row_id,
                 s.timeseries_id,
                 s.type_id,
                 s.start_dt,
                 s.end_dt
               FROM snapshot_rows s
               JOIN continuous.timeseries visible
                 ON visible.timeseries_id = s.timeseries_id
               WHERE s.row_created <= $4
                 AND ($1 IS NULL OR s.timeseries_id = ANY($1))
                 AND (
                   $2 IS NULL
                   OR s.end_dt > $2
                   OR (
                     s.start_dt = s.end_dt
                     AND s.start_dt >= $2
                   )
                 )
                 AND ($3 IS NULL OR s.start_dt < $3)
               ORDER BY s.timeseries_id, s.start_dt, s.end_dt, s.row_id
             $sql$,
             row_id_column,
             type_id_column,
             interval_table,
             row_id_column,
             type_id_column,
             interval_table
           )
           USING
             p_timeseries_ids,
             p_start_datetime,
             p_end_datetime,
             p_as_of,
             requested_type;
         END LOOP;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.continuous_qc_rows_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ,
         TEXT[]
       ) OWNER TO admin"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.continuous_qc_types_as_of(
         p_as_of TIMESTAMPTZ,
         p_qc_types TEXT[] DEFAULT NULL
       )
       RETURNS TABLE (
         qc_type TEXT,
         type_id INTEGER,
         type_code TEXT,
         type_description TEXT,
         type_description_fr TEXT,
         color_code VARCHAR
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         requested_type TEXT;
         type_table TEXT;
         type_id_column TEXT;
         type_code_column TEXT;
         type_description_column TEXT;
         type_description_fr_column TEXT;
         history_started_at TIMESTAMPTZ;
       BEGIN
         SELECT max(boundary_value)
         INTO history_started_at
         FROM (
           SELECT hb.history_started_at AS boundary_value
           FROM audit.history_boundaries hb
           WHERE hb.history_name = 'continuous_qc'
           UNION ALL
           SELECT r.history_started_at
           FROM audit.table_registry r
           WHERE (r.schema_name, r.table_name) IN (
             ('continuous', 'grades'),
             ('continuous', 'approvals'),
             ('continuous', 'qualifiers'),
             ('public', 'grade_types'),
             ('public', 'approval_types'),
             ('public', 'qualifier_types')
           )
         ) boundaries;

         IF history_started_at IS NULL THEN
           RAISE EXCEPTION
             'Continuous QC history has not been initialized.';
         END IF;
         IF p_as_of < history_started_at THEN
           RAISE EXCEPTION
             'Continuous QC history is only reliable from % onward; requested as_of was %.',
             history_started_at,
             p_as_of;
         END IF;

         FOR requested_type IN
           SELECT DISTINCT requested.qc_type
           FROM unnest(
             COALESCE(
               p_qc_types,
               ARRAY['grade', 'approval', 'qualifier']::TEXT[]
             )
           ) AS requested(qc_type)
         LOOP
           CASE requested_type
             WHEN 'grade' THEN
               type_table := 'grade_types';
               type_id_column := 'grade_type_id';
               type_code_column := 'grade_type_code';
               type_description_column := 'grade_type_description';
               type_description_fr_column := 'grade_type_description_fr';
             WHEN 'approval' THEN
               type_table := 'approval_types';
               type_id_column := 'approval_type_id';
               type_code_column := 'approval_type_code';
               type_description_column := 'approval_type_description';
               type_description_fr_column := 'approval_type_description_fr';
             WHEN 'qualifier' THEN
               type_table := 'qualifier_types';
               type_id_column := 'qualifier_type_id';
               type_code_column := 'qualifier_type_code';
               type_description_column := 'qualifier_type_description';
               type_description_fr_column := 'qualifier_type_description_fr';
             ELSE
               RAISE EXCEPTION
                 'Unsupported continuous QC type: %',
                 requested_type;
           END CASE;

           RETURN QUERY EXECUTE format(
             $sql$
               WITH current_rows AS (
                 SELECT
                   t.%1$I AS type_id,
                   t.%2$I AS type_code,
                   t.%3$I AS type_description,
                   t.%4$I AS type_description_fr,
                   t.color_code,
                   t.created AS row_created
                 FROM public.%5$I t
               ),
               future_changes AS (
                 SELECT DISTINCT ON (
                   COALESCE(
                     g.original_data ->> %6$L,
                     g.new_data ->> %6$L
                   )
                 )
                   (g.original_data ->> %6$L)::INTEGER AS type_id,
                   g.original_data ->> %7$L AS type_code,
                   g.original_data ->> %8$L AS type_description,
                   g.original_data ->> %9$L AS type_description_fr,
                   (g.original_data ->> 'color_code')::VARCHAR AS color_code,
                   g.row_created
                 FROM audit.general_log g
                 WHERE g.schema_name = 'public'
                   AND g.table_name = %10$L
                   AND g.action_timestamp > $1
                 ORDER BY
                   COALESCE(
                     g.original_data ->> %6$L,
                     g.new_data ->> %6$L
                   ),
                   g.action_timestamp,
                   g.log_id
               ),
               snapshot_rows AS (
                 SELECT
                   COALESCE(f.type_id, c.type_id) AS type_id,
                   CASE
                     WHEN f.type_id IS NOT NULL THEN f.type_code
                     ELSE c.type_code
                   END AS type_code,
                   CASE
                     WHEN f.type_id IS NOT NULL THEN f.type_description
                     ELSE c.type_description
                   END AS type_description,
                   CASE
                     WHEN f.type_id IS NOT NULL THEN f.type_description_fr
                     ELSE c.type_description_fr
                   END AS type_description_fr,
                   CASE
                     WHEN f.type_id IS NOT NULL THEN f.color_code
                     ELSE c.color_code
                   END AS color_code,
                   CASE
                     WHEN f.type_id IS NOT NULL THEN f.row_created
                     ELSE c.row_created
                   END AS row_created
                 FROM current_rows c
                 FULL OUTER JOIN future_changes f
                   ON f.type_id = c.type_id
               )
               SELECT
                 $2::TEXT AS qc_type,
                 s.type_id,
                 s.type_code,
                 s.type_description,
                 s.type_description_fr,
                 s.color_code
               FROM snapshot_rows s
               WHERE s.row_created <= $1
               ORDER BY s.type_id
             $sql$,
             type_id_column,
             type_code_column,
             type_description_column,
             type_description_fr_column,
             type_table,
             type_id_column,
             type_code_column,
             type_description_column,
             type_description_fr_column,
             type_table
           )
           USING p_as_of, requested_type;
         END LOOP;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.continuous_qc_types_as_of(
         TIMESTAMPTZ,
         TEXT[]
       ) OWNER TO admin"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.continuous_qc_intervals_as_of(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL,
         p_start_datetime TIMESTAMPTZ DEFAULT NULL,
         p_end_datetime TIMESTAMPTZ DEFAULT NULL,
         p_qc_types TEXT[] DEFAULT NULL
       )
       RETURNS TABLE (
         qc_type TEXT,
         timeseries_id INTEGER,
         start_dt TIMESTAMPTZ,
         end_dt TIMESTAMPTZ,
         type_id INTEGER,
         type_code TEXT,
         type_description TEXT,
         type_description_fr TEXT,
         color_code VARCHAR
       )
       LANGUAGE sql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
         WITH qc_rows AS MATERIALIZED (
           SELECT *
           FROM audit.continuous_qc_rows_as_of(
             p_as_of,
             p_timeseries_ids,
             p_start_datetime,
             p_end_datetime,
             p_qc_types
           )
         ),
         qc_types AS MATERIALIZED (
           SELECT *
           FROM audit.continuous_qc_types_as_of(
             p_as_of,
             p_qc_types
           )
         )
         SELECT
           q.qc_type,
           q.timeseries_id,
           q.start_dt,
           q.end_dt,
           q.type_id,
           t.type_code,
           t.type_description,
           t.type_description_fr,
           t.color_code
         FROM qc_rows q
         LEFT JOIN qc_types t
           ON t.qc_type = q.qc_type
          AND t.type_id = q.type_id
         ORDER BY
           q.timeseries_id,
           q.qc_type,
           q.start_dt,
           q.end_dt;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.continuous_qc_intervals_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ,
         TEXT[]
       ) OWNER TO admin"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.continuous_daily_settings_as_of(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         aggregation_type_id INTEGER,
         aggregation_type TEXT,
         timezone_daily_calc INTEGER,
         record_rate INTERVAL,
         timeseries_type TEXT
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         history_started_at TIMESTAMPTZ;
       BEGIN
         SELECT max(boundary_value)
         INTO history_started_at
         FROM (
           SELECT hb.history_started_at AS boundary_value
           FROM audit.history_boundaries hb
           WHERE hb.history_name = 'continuous_daily_dependencies'
           UNION ALL
           SELECT r.history_started_at
           FROM audit.table_registry r
           WHERE (r.schema_name, r.table_name) IN (
             ('continuous', 'timeseries'),
             ('continuous', 'aggregation_types'),
             ('continuous', 'measurements_continuous'),
             ('continuous', 'corrections'),
             ('continuous', 'correction_types'),
             ('continuous', 'timeseries_compounds'),
             ('continuous', 'timeseries_compound_members'),
             ('continuous', 'grades'),
             ('public', 'grade_types')
           )
         ) boundaries;

         IF history_started_at IS NULL THEN
           RAISE EXCEPTION
             'Continuous daily dependency history has not been initialized.';
         END IF;
         IF p_as_of < history_started_at THEN
           RAISE EXCEPTION
             'Continuous daily dependency history is only reliable from % onward; requested as_of was %.',
             history_started_at,
             p_as_of;
         END IF;

         RETURN QUERY
         WITH current_timeseries AS MATERIALIZED (
           SELECT
             t.timeseries_id,
             to_jsonb(t) AS row_state,
             t.created AS row_created
           FROM continuous.timeseries t
           WHERE p_timeseries_ids IS NULL
              OR t.timeseries_id = ANY(p_timeseries_ids)
         ),
         future_timeseries AS MATERIALIZED (
           SELECT DISTINCT ON (
             COALESCE(
               g.original_data ->> 'timeseries_id',
               g.new_data ->> 'timeseries_id'
             )
           )
             COALESCE(
               g.original_data ->> 'timeseries_id',
               g.new_data ->> 'timeseries_id'
             )::INTEGER AS timeseries_id,
             g.original_data AS row_state,
             g.row_created
           FROM audit.general_log g
           WHERE g.schema_name = 'continuous'
             AND g.table_name = 'timeseries'
             AND g.action_timestamp > p_as_of
             AND (
               p_timeseries_ids IS NULL
               OR COALESCE(
                 (g.original_data ->> 'timeseries_id')::INTEGER,
                 (g.new_data ->> 'timeseries_id')::INTEGER
               ) = ANY(p_timeseries_ids)
             )
           ORDER BY
             COALESCE(
               g.original_data ->> 'timeseries_id',
               g.new_data ->> 'timeseries_id'
             ),
             g.action_timestamp,
             g.log_id
         ),
         timeseries_snapshot AS MATERIALIZED (
           SELECT
             COALESCE(f.timeseries_id, c.timeseries_id) AS timeseries_id,
             CASE
               WHEN f.timeseries_id IS NOT NULL THEN f.row_state
               ELSE c.row_state
             END AS row_state,
             CASE
               WHEN f.timeseries_id IS NOT NULL THEN f.row_created
               ELSE c.row_created
             END AS row_created
           FROM current_timeseries c
           FULL OUTER JOIN future_timeseries f
             ON f.timeseries_id = c.timeseries_id
         ),
         current_aggregation_types AS MATERIALIZED (
           SELECT
             a.aggregation_type_id,
             to_jsonb(a) AS row_state,
             a.created AS row_created
           FROM continuous.aggregation_types a
         ),
         future_aggregation_types AS MATERIALIZED (
           SELECT DISTINCT ON (
             COALESCE(
               g.original_data ->> 'aggregation_type_id',
               g.new_data ->> 'aggregation_type_id'
             )
           )
             COALESCE(
               g.original_data ->> 'aggregation_type_id',
               g.new_data ->> 'aggregation_type_id'
             )::INTEGER AS aggregation_type_id,
             g.original_data AS row_state,
             g.row_created
           FROM audit.general_log g
           WHERE g.schema_name = 'continuous'
             AND g.table_name = 'aggregation_types'
             AND g.action_timestamp > p_as_of
           ORDER BY
             COALESCE(
               g.original_data ->> 'aggregation_type_id',
               g.new_data ->> 'aggregation_type_id'
             ),
             g.action_timestamp,
             g.log_id
         ),
         aggregation_snapshot AS MATERIALIZED (
           SELECT
             COALESCE(
               f.aggregation_type_id,
               c.aggregation_type_id
             ) AS aggregation_type_id,
             CASE
               WHEN f.aggregation_type_id IS NOT NULL THEN f.row_state
               ELSE c.row_state
             END AS row_state,
             CASE
               WHEN f.aggregation_type_id IS NOT NULL THEN f.row_created
               ELSE c.row_created
             END AS row_created
           FROM current_aggregation_types c
           FULL OUTER JOIN future_aggregation_types f
             ON f.aggregation_type_id = c.aggregation_type_id
         )
         SELECT
           ts.timeseries_id,
           (ts.row_state ->> 'aggregation_type_id')::INTEGER,
           agg.row_state ->> 'aggregation_type',
           COALESCE(
             (ts.row_state ->> 'timezone_daily_calc')::INTEGER,
             0
           ),
           (ts.row_state ->> 'record_rate')::INTERVAL,
           ts.row_state ->> 'timeseries_type'
         FROM timeseries_snapshot ts
         JOIN continuous.timeseries visible
           ON visible.timeseries_id = ts.timeseries_id
         JOIN aggregation_snapshot agg
           ON agg.aggregation_type_id =
             (ts.row_state ->> 'aggregation_type_id')::INTEGER
         WHERE ts.row_created <= p_as_of
           AND agg.row_created <= p_as_of
         ORDER BY ts.timeseries_id;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.continuous_daily_settings_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.continuous_daily_settings_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) FROM PUBLIC"
    )

    # Preserve the current calculated-daily implementation while replacing its
    # known current-grade and current-timeseries dependencies. This avoids
    # overwriting unrelated improvements made by later patches.
    daily_function_signature <- paste0(
      "continuous.measurements_calculated_daily_at(",
      "timestamp with time zone, integer[], date, date)"
    )
    daily_function_sql <- DBI::dbGetQuery(
      con,
      "SELECT pg_get_functiondef(to_regprocedure($1)) AS definition",
      params = list(daily_function_signature)
    )$definition[[1]]

    old_raw_cte_pattern <- paste0(
      "\\),\\s*",
      "raw_measurements AS MATERIALIZED \\("
    )
    old_grade_pattern <- paste0(
      "FROM continuous\\.grades g\\s+",
      "JOIN public\\.grade_types gt\\s+",
      "ON gt\\.grade_type_id = g\\.grade_type_id\\s+",
      "WHERE g\\.timeseries_id = st\\.timeseries_id\\s+",
      "AND gt\\.grade_type_code = 'N'\\s+",
      "AND g\\.start_dt <> g\\.end_dt\\s+",
      "AND m\\.datetime BETWEEN g\\.start_dt AND g\\.end_dt"
    )
    old_settings_pattern <- paste0(
      "FROM continuous\\.timeseries t\\s+",
      "JOIN continuous\\.aggregation_types at\\s+",
      "ON at\\.aggregation_type_id = t\\.aggregation_type_id\\s+",
      "WHERE \\(p_timeseries_ids IS NULL OR ",
      "t\\.timeseries_id = ANY\\(p_timeseries_ids\\)\\)"
    )
    historical_settings_sql <- paste0(
      "FROM audit.continuous_daily_settings_as_of(",
      "COALESCE(p_as_of, now()), p_timeseries_ids) t WHERE TRUE"
    )

    if (grepl(old_settings_pattern, daily_function_sql, perl = TRUE)) {
      daily_function_sql <- sub(
        old_settings_pattern,
        historical_settings_sql,
        daily_function_sql,
        perl = TRUE
      )
      daily_function_sql <- gsub(
        "at.aggregation_type",
        "t.aggregation_type",
        daily_function_sql,
        fixed = TRUE
      )
    } else if (
      !grepl(
        "audit.continuous_daily_settings_as_of",
        daily_function_sql,
        fixed = TRUE
      )
    ) {
      stop(
        "measurements_calculated_daily_at() no longer contains the expected ",
        "current timeseries/aggregation source."
      )
    }

    if (grepl(old_grade_pattern, daily_function_sql, perl = TRUE)) {
      raw_cte_matches <- gregexpr(
        old_raw_cte_pattern,
        daily_function_sql,
        perl = TRUE
      )[[1]]
      grade_matches <- gregexpr(
        old_grade_pattern,
        daily_function_sql,
        perl = TRUE
      )[[1]]
      if (
        raw_cte_matches[[1]] == -1L ||
          length(raw_cte_matches) != 1L ||
          grade_matches[[1]] == -1L ||
          length(grade_matches) != 1L
      ) {
        stop(
          "Could not identify exactly one calculated-daily raw CTE and ",
          "current-grade exclusion to replace."
        )
      }

      historical_grade_cte <- "),
         historical_unusable_grades AS MATERIALIZED (
           SELECT
             q.timeseries_id,
             q.start_dt,
             q.end_dt
           FROM audit.continuous_qc_intervals_as_of(
             COALESCE(p_as_of, now()),
             (
               SELECT array_agg(st.timeseries_id ORDER BY st.timeseries_id)
               FROM selected_timeseries st
             ),
             NULL::TIMESTAMPTZ,
             NULL::TIMESTAMPTZ,
             ARRAY['grade']::TEXT[]
           ) q
           WHERE q.type_code = 'N'
         ),
         raw_measurements AS MATERIALIZED ("
      daily_function_sql <- sub(
        old_raw_cte_pattern,
        historical_grade_cte,
        daily_function_sql,
        perl = TRUE
      )
      daily_function_sql <- sub(
        old_grade_pattern,
        paste0(
          "FROM historical_unusable_grades g ",
          "WHERE g.timeseries_id = st.timeseries_id ",
          "AND g.start_dt <> g.end_dt ",
          "AND m.datetime BETWEEN g.start_dt AND g.end_dt"
        ),
        daily_function_sql,
        perl = TRUE
      )
    } else if (
      !grepl(
        "historical_unusable_grades AS MATERIALIZED",
        daily_function_sql,
        fixed = TRUE
      )
    ) {
      stop(
        "measurements_calculated_daily_at() no longer contains the expected ",
        "current-grade exclusion. Review its implementation before applying ",
        "the historical QC patch."
      )
    }

    daily_function_sql <- gsub(
      "m.datetime BETWEEN g.start_dt AND g.end_dt",
      "m.datetime >= g.start_dt AND m.datetime < g.end_dt",
      daily_function_sql,
      fixed = TRUE
    )
    current_compound_members_pattern <- paste0(
      "FROM continuous\\.timeseries_compound_members m\\s+",
      "WHERE m\\.timeseries_id = t\\.timeseries_id"
    )
    historical_compound_members_sql <- paste0(
      "FROM audit.timeseries_compound_members_as_of(",
      "COALESCE(p_as_of, now()), ARRAY[t.timeseries_id]) m ",
      "WHERE m.timeseries_id = t.timeseries_id"
    )
    if (
      grepl(current_compound_members_pattern, daily_function_sql, perl = TRUE)
    ) {
      daily_function_sql <- sub(
        current_compound_members_pattern,
        historical_compound_members_sql,
        daily_function_sql,
        perl = TRUE
      )
    } else if (
      !grepl(
        "audit.timeseries_compound_members_as_of",
        daily_function_sql,
        fixed = TRUE
      )
    ) {
      stop(
        "measurements_calculated_daily_at() no longer contains the expected ",
        "compound-member availability check."
      )
    }
    corrected_daily_call <-
      "continuous.measurements_continuous_corrected_at"
    internal_daily_call <-
      "continuous.measurements_continuous_corrected_internal_at"
    if (grepl(corrected_daily_call, daily_function_sql, fixed = TRUE)) {
      daily_internal_tail <- "END,
             'actual'::text,
             NULL::integer
           ) m"
      if (!grepl(daily_internal_tail, daily_function_sql, fixed = TRUE)) {
        # PostgreSQL currently deparses this literal without the redundant
        # text cast, but retain support for either form.
        daily_internal_tail <- "END,
             'actual',
             NULL::integer
           ) m"
      }
      if (!grepl(daily_internal_tail, daily_function_sql, fixed = TRUE)) {
        stop(
          "measurements_calculated_daily_at() no longer contains the ",
          "expected corrected-measurement argument tail."
        )
      }
      daily_function_sql <- sub(
        corrected_daily_call,
        internal_daily_call,
        daily_function_sql,
        fixed = TRUE
      )
      daily_function_sql <- sub(
        daily_internal_tail,
        "END,
             ARRAY[]::integer[]
           ) m",
        daily_function_sql,
        fixed = TRUE
      )
    } else if (!grepl(internal_daily_call, daily_function_sql, fixed = TRUE)) {
      stop(
        "measurements_calculated_daily_at() no longer contains the ",
        "expected historical corrected-measurement source."
      )
    }
    DBI::dbExecute(con, daily_function_sql)

    corrected_function_signatures <- c(
      paste0(
        "continuous.measurements_continuous_corrected_at(",
        "timestamp with time zone, integer, timestamp with time zone, ",
        "timestamp with time zone, text, integer)"
      ),
      paste0(
        "continuous.measurements_continuous_corrected_internal_at(",
        "timestamp with time zone, integer, timestamp with time zone, ",
        "timestamp with time zone, integer[])"
      )
    )
    current_type_pattern <- paste0(
      "SELECT t\\.timeseries_type\\s+",
      "INTO v_type\\s+",
      "FROM continuous\\.timeseries t\\s+",
      "WHERE t\\.timeseries_id = p_timeseries_id;"
    )
    historical_type_sql <- "SELECT t.timeseries_type
           INTO v_type
           FROM audit.continuous_daily_settings_as_of(
             COALESCE(p_as_of, now()),
             ARRAY[p_timeseries_id]
           ) t
           WHERE t.timeseries_id = p_timeseries_id;"
    for (function_signature in corrected_function_signatures) {
      corrected_sql <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_functiondef(to_regprocedure($1)) AS definition",
        params = list(function_signature)
      )$definition[[1]]

      if (grepl(current_type_pattern, corrected_sql, perl = TRUE)) {
        corrected_sql <- sub(
          current_type_pattern,
          historical_type_sql,
          corrected_sql,
          perl = TRUE
        )
        DBI::dbExecute(con, corrected_sql)
      } else if (
        !grepl(
          "audit.continuous_daily_settings_as_of",
          corrected_sql,
          fixed = TRUE
        )
      ) {
        stop(
          function_signature,
          " no longer contains the expected current timeseries-type lookup."
        )
      }
    }

    # Apply ordinary historical corrections as a set. The earlier historical
    # implementation reconstructed the same correction definitions once per
    # measurement through apply_corrections_at().
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_basic_simple_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMPTZ,
         p_to TIMESTAMPTZ
       )
       RETURNS TABLE (
         datetime TIMESTAMPTZ,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE sql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
         WITH RECURSIVE correction_steps AS MATERIALIZED (
           SELECT
             row_number() OVER (
               ORDER BY ct.priority, c.correction_id
             )::INTEGER AS step,
             c.correction_id,
             c.value1,
             c.value2,
             c.timestep_window,
             c.start_dt,
             c.end_dt,
             ct.correction_type
           FROM audit.corrections_as_of(
             p_as_of,
             ARRAY[p_timeseries_id]
           ) c
           JOIN audit.correction_types_as_of(p_as_of) ct
             ON ct.correction_type_id = c.correction_type
           WHERE c.timeseries_id = p_timeseries_id
             AND (p_to IS NULL OR c.start_dt <= p_to)
             AND (p_from IS NULL OR c.end_dt > p_from)
         ),
         measurements AS MATERIALIZED (
           SELECT
             mc.datetime,
             mc.value AS value_corrected,
             mc.period,
             mc.imputed,
             mc.value IS NULL AS removed
           FROM audit.measurements_continuous_as_of(
             p_as_of,
             ARRAY[p_timeseries_id],
             p_from,
             p_to
           ) mc
         ),
         corrected AS (
           SELECT
             0::INTEGER AS step,
             m.datetime,
             m.value_corrected,
             m.period,
             m.imputed,
             m.removed
           FROM measurements m

           UNION ALL

           SELECT
             cs.step,
             c.datetime,
             CASE
               WHEN c.removed THEN NULL
               WHEN NOT (
                 cs.start_dt <= c.datetime
                 AND cs.end_dt > c.datetime
               ) THEN c.value_corrected
               WHEN cs.correction_type = 'delete' THEN NULL
               WHEN cs.correction_type = 'trim'
                 AND (
                   (
                     cs.value1 IS NOT NULL
                     AND c.value_corrected < cs.value1
                   )
                   OR (
                     cs.value2 IS NOT NULL
                     AND c.value_corrected > cs.value2
                   )
                 ) THEN NULL
               WHEN cs.correction_type = 'offset linear' THEN
                 c.value_corrected + cs.value1
               WHEN cs.correction_type = 'offset two-point' THEN
                 c.value_corrected + (
                   cs.value1
                   + (
                     (cs.value2 - cs.value1)
                     / extract(epoch FROM (cs.end_dt - cs.start_dt))
                   )
                   * extract(epoch FROM (c.datetime - cs.start_dt))
                 )
               WHEN cs.correction_type = 'scale' THEN
                 c.value_corrected * (cs.value1 / 100.0)
               WHEN cs.correction_type = 'drift linear' THEN
                 c.value_corrected + (
                   (
                     cs.value1
                     / extract(epoch FROM cs.timestep_window)
                   )
                   * extract(epoch FROM (c.datetime - cs.start_dt))
                 )
               ELSE c.value_corrected
             END AS value_corrected,
             c.period,
             c.imputed,
             c.removed
               OR (
                 cs.start_dt <= c.datetime
                 AND cs.end_dt > c.datetime
                 AND (
                   cs.correction_type = 'delete'
                   OR (
                     cs.correction_type = 'trim'
                     AND (
                       (
                         cs.value1 IS NOT NULL
                         AND c.value_corrected < cs.value1
                       )
                       OR (
                         cs.value2 IS NOT NULL
                         AND c.value_corrected > cs.value2
                       )
                     )
                   )
                 )
               ) AS removed
           FROM corrected c
           JOIN correction_steps cs
             ON cs.step = c.step + 1
         )
         SELECT
           c.datetime,
           c.value_corrected,
           c.period,
           c.imputed
         FROM corrected c
         WHERE c.step = (SELECT count(*) FROM correction_steps)
         ORDER BY c.datetime;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.measurements_continuous_corrected_basic_simple_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMPTZ,
         TIMESTAMPTZ
       ) OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.measurements_continuous_corrected_basic_simple_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMPTZ,
         TIMESTAMPTZ
       ) FROM PUBLIC"
    )

    # Avoid correction work entirely for the common no-correction case and use
    # the set-based helper for supported correction types.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_internal_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMPTZ,
         p_to TIMESTAMPTZ,
         p_path INTEGER[] DEFAULT ARRAY[]::INTEGER[]
       )
       RETURNS TABLE (
         datetime TIMESTAMPTZ,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         v_type TEXT;
         v_has_corrections BOOLEAN;
         v_simple_corrections BOOLEAN;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM audit.continuous_daily_settings_as_of(
           COALESCE(p_as_of, now()),
           ARRAY[p_timeseries_id]
         ) t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found at %',
             p_timeseries_id,
             p_as_of;
         END IF;

         SELECT
           count(*) > 0,
           count(*) <= 64
             AND count(*) FILTER (
               WHERE ct.correction_type NOT IN (
                 'delete',
                 'trim',
                 'offset linear',
                 'offset two-point',
                 'drift linear',
                 'scale'
               )
             ) = 0
         INTO v_has_corrections, v_simple_corrections
         FROM audit.corrections_as_of(
           p_as_of,
           ARRAY[p_timeseries_id]
         ) c
         JOIN audit.correction_types_as_of(p_as_of) ct
           ON ct.correction_type_id = c.correction_type
         WHERE c.timeseries_id = p_timeseries_id
           AND (p_to IS NULL OR c.start_dt <= p_to)
           AND (p_from IS NULL OR c.end_dt > p_from);

         IF v_type = 'basic' THEN
           IF NOT v_has_corrections THEN
             RETURN QUERY
             SELECT
               mc.datetime,
               mc.value AS value_corrected,
               mc.period,
               mc.imputed
             FROM audit.measurements_continuous_as_of(
               p_as_of,
               ARRAY[p_timeseries_id],
               p_from,
               p_to
             ) mc;

             RETURN;
           END IF;

           IF v_simple_corrections THEN
             RETURN QUERY
             SELECT
               simple.datetime,
               simple.value_corrected,
               simple.period,
               simple.imputed
             FROM continuous.measurements_continuous_corrected_basic_simple_at(
               p_as_of,
               p_timeseries_id,
               p_from,
               p_to
             ) simple;

             RETURN;
           END IF;

           RETURN QUERY
           SELECT
             mc.datetime,
             continuous.apply_corrections_at(
               p_as_of,
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed
           FROM audit.measurements_continuous_as_of(
             p_as_of,
             ARRAY[p_timeseries_id],
             p_from,
             p_to
           ) mc;

           RETURN;
         END IF;

         IF NOT v_has_corrections THEN
           RETURN QUERY
           SELECT
             src.datetime,
             src.value_raw AS value_corrected,
             src.period,
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window_at(
             p_as_of,
             p_timeseries_id,
             p_from,
             p_to,
             p_path
           ) src;

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           src.datetime,
           continuous.apply_corrections_at(
             p_as_of,
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed
         FROM continuous.resolve_compound_timeseries_raw_window_at(
           p_as_of,
           p_timeseries_id,
           p_from,
           p_to,
           p_path
         ) src;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.measurements_continuous_corrected_internal_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMPTZ,
         TIMESTAMPTZ,
         INTEGER[]
       ) OWNER TO admin"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) IS
       'Recomputes calculated daily rows for a requested as-of timestamp from audited continuous measurements, corrections, unusable-grade intervals, grade-type metadata, and historical aggregation settings. Requests before the effective source-history boundary are rejected because pre-boundary source changes cannot be reconstructed reliably.'"
    )

    # Trigger functions execute through their trigger and do not require the
    # invoking role to hold EXECUTE. Remove the default PUBLIC privilege from
    # all application-owned trigger functions, including older ones.
    public_trigger_functions <- DBI::dbGetQuery(
      con,
      "SELECT format(
         '%I.%I(%s)',
         n.nspname,
         p.proname,
         pg_get_function_identity_arguments(p.oid)
       ) AS function_signature
       FROM pg_proc p
       JOIN pg_namespace n ON n.oid = p.pronamespace
       WHERE p.prorettype = 'pg_catalog.trigger'::regtype
         AND n.nspname NOT IN (
           'pg_catalog',
           'information_schema'
         )
         AND n.nspname NOT LIKE 'pg_temp_%'
         AND has_function_privilege('public', p.oid, 'EXECUTE')
       ORDER BY n.nspname, p.proname"
    )$function_signature
    for (function_signature in public_trigger_functions) {
      DBI::dbExecute(
        con,
        paste0(
          "REVOKE ALL ON FUNCTION ",
          function_signature,
          " FROM PUBLIC"
        )
      )
    }

    historical_qc_functions <- c(
      "audit.continuous_qc_rows_as_of(TIMESTAMPTZ, INTEGER[], TIMESTAMPTZ, TIMESTAMPTZ, TEXT[])",
      "audit.continuous_qc_types_as_of(TIMESTAMPTZ, TEXT[])",
      "audit.continuous_qc_intervals_as_of(TIMESTAMPTZ, INTEGER[], TIMESTAMPTZ, TIMESTAMPTZ, TEXT[])",
      "audit.continuous_daily_settings_as_of(TIMESTAMPTZ, INTEGER[])"
    )
    historical_dependency_functions <- c(
      "audit.measurements_continuous_as_of(TIMESTAMPTZ, INTEGER[], TIMESTAMPTZ, TIMESTAMPTZ)",
      "audit.corrections_as_of(TIMESTAMPTZ, INTEGER[])",
      "audit.correction_types_as_of(TIMESTAMPTZ)",
      "audit.timeseries_compounds_as_of(TIMESTAMPTZ, INTEGER[])",
      "audit.timeseries_compound_members_as_of(TIMESTAMPTZ, INTEGER[])"
    )
    historical_corrected_functions <- c(
      "continuous.measurements_continuous_corrected_basic_simple_at(TIMESTAMPTZ, INTEGER, TIMESTAMPTZ, TIMESTAMPTZ)"
    )
    dependency_exists <- vapply(
      historical_dependency_functions,
      function(function_signature) {
        !is.na(
          DBI::dbGetQuery(
            con,
            "SELECT to_regprocedure($1)::TEXT AS function_signature",
            params = list(function_signature)
          )$function_signature[[1]]
        )
      },
      logical(1)
    )
    missing_historical_dependencies <-
      historical_dependency_functions[!dependency_exists]
    if (length(missing_historical_dependencies)) {
      stop(
        "Required historical dependency functions are missing: ",
        paste(missing_historical_dependencies, collapse = ", ")
      )
    }
    for (function_signature in historical_qc_functions) {
      DBI::dbExecute(
        con,
        paste0(
          "REVOKE ALL ON FUNCTION ",
          function_signature,
          " FROM PUBLIC"
        )
      )
    }
    for (role_name in historical_qc_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        paste0(
          "GRANT USAGE ON SCHEMA audit TO ",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "GRANT SELECT ON TABLE ",
          "audit.general_log, ",
          "audit.measurements_continuous_log, ",
          "audit.history_boundaries, ",
          "audit.table_registry TO ",
          quoted_role
        )
      )
      for (function_signature in c(
        historical_qc_functions,
        historical_dependency_functions,
        historical_corrected_functions
      )) {
        DBI::dbExecute(
          con,
          paste0(
            "GRANT EXECUTE ON FUNCTION ",
            function_signature,
            " TO ",
            quoted_role
          )
        )
      }
    }

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.continuous_qc_intervals_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ,
         TEXT[]
       ) IS
       'Reconstructs continuous grade, approval, and qualifier intervals and their type metadata at a requested timestamp. Requests before the continuous QC audit boundary are rejected because pre-boundary edits and deletions cannot be reconstructed reliably.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.continuous_daily_settings_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) IS
       'Reconstructs the timeseries and aggregation settings that affect corrected and calculated-daily values. Requests before the dependency-history boundary are rejected.'"
    )

    # Modify the cross-section points and verticals tables to constrain the reference bank to left/right OR null.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.cross_section_points DROP CONSTRAINT cross_section_points_rel_position_on_water_panel_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.cross_section_points ADD CONSTRAINT cross_section_points_rel_position_on_water_panel_check CHECK (((rel_position_on_water_panel >= (0)::numeric) AND (rel_position_on_water_panel <= (1)::numeric)));"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.cross_section_verticals DROP CONSTRAINT cross_section_verticals_reference_bank_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.cross_section_verticals ADD CONSTRAINT cross_section_verticals_reference_bank_check CHECK ((reference_bank = ANY (ARRAY['left'::text, 'right'::text])) OR reference_bank IS NULL);"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.cross_section_verticals ALTER COLUMN reference_bank SET DEFAULT NULL;"
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
    message(
      "Patch 55 applied successfully. Stored SQL and half-open intervals are constrained, audit access and coverage are hardened, and continuous source history supports QC and daily reconstruction."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
