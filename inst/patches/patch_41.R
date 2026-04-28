# Patch 41: function-first compound continuous timeseries support
#
# Design choices in this patch:
# - keep continuous.timeseries as the single catalog of continuous timeseries
# - distinguish raw/basic rows from derived/compound rows with one new column
# - define compounds with one header table plus one member table
# - leave continuous.measurements_continuous and
#   continuous.measurements_continuous_corrected unchanged for basic series
# - add a bounded function that returns corrected measurements for one
#   timeseries_id, basic or compound, over a requested datetime window
# - resolve compound values from current source data at query time, so new
#   data and source corrections flow through immediately
# - maintain start_datetime/end_datetime and realtime last_new_data metadata
#   in the database instead of in R data-ingest code
#
# Current limits of this version:
# - only one compound row can exist for a given natural timeseries key
# - formula compounds assume exact datetime alignment across members
# - callers should provide a timeseries_id and a bounded datetime range for
#   acceptable performance
# - compound members may be basic or compound timeseries; cycles are rejected
# - compound start_datetime/end_datetime metadata is derived from member
#   metadata extents and use_from/use_to windows rather than from a full
#   resolved-row scan, so internal gaps and phase offsets are not represented

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 41: adding compound continuous timeseries support. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

message("Starting transaction...")
active <- dbTransBegin(con)

tryCatch(
  {
    check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.measurements_continuous') IS NOT NULL AS has_measurements_continuous,
         to_regclass('continuous.measurements_continuous_corrected') IS NOT NULL AS has_measurements_continuous_corrected,
         to_regclass('continuous.corrections') IS NOT NULL AS has_corrections,
         to_regprocedure('continuous.apply_corrections(integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections,
         to_regprocedure('continuous.apply_corrections_at(timestamp with time zone, integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections_at,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_if_modified_func,
         to_regprocedure('audit.measurements_continuous_as_of(timestamp with time zone, integer[], timestamp with time zone, timestamp with time zone)') IS NOT NULL AS has_measurements_continuous_as_of"
    )

    if (
      !isTRUE(check$has_timeseries[[1]]) ||
        !isTRUE(check$has_measurements_continuous[[1]]) ||
        !isTRUE(check$has_measurements_continuous_corrected[[1]]) ||
        !isTRUE(check$has_corrections[[1]]) ||
        !isTRUE(check$has_apply_corrections[[1]]) ||
        !isTRUE(check$has_apply_corrections_at[[1]]) ||
        !isTRUE(check$has_audit_if_modified_func[[1]]) ||
        !isTRUE(check$has_measurements_continuous_as_of[[1]])
    ) {
      stop(
        "This patch requires continuous.timeseries, continuous.measurements_continuous, continuous.measurements_continuous_corrected, continuous.corrections, continuous.apply_corrections(), continuous.apply_corrections_at(), audit.if_modified_func(), and audit.measurements_continuous_as_of() to already exist."
      )
    }

    check_existing <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'timeseries'
             AND column_name = 'timeseries_type'
         ) AS has_timeseries_type,
         to_regclass('continuous.timeseries_types') IS NOT NULL AS has_timeseries_types,
         to_regclass('continuous.timeseries_compounds') IS NOT NULL AS has_timeseries_compounds,
         to_regclass('continuous.timeseries_compound_members') IS NOT NULL AS has_timeseries_compound_members"
    )

    if (
      isTRUE(check_existing$has_timeseries_type[[1]]) ||
        isTRUE(check_existing$has_timeseries_types[[1]]) ||
        isTRUE(check_existing$has_timeseries_compounds[[1]]) ||
        isTRUE(check_existing$has_timeseries_compound_members[[1]])
    ) {
      stop(
        "This patch appears to have already been applied, or a partial version of it is already present. Review the database state before running it again."
      )
    }

    existing_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname

    write_roles <- intersect(
      c("admin", "tkc_editor", "yg_editor_group", "cyfn_editor_group"),
      existing_roles
    )

    historical_function_roles <- intersect(
      c("yg_editor_group", "yg_editor", "yg_reader_group", "yg_reader"),
      existing_roles
    )

    q_ident <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    grant_table_privileges <- function(object_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON TABLE %s TO %s",
            privileges,
            object_name,
            q_ident(role_name)
          )
        )
      }
    }

    grant_function_privileges <- function(function_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON FUNCTION %s TO %s",
            privileges,
            function_name,
            q_ident(role_name)
          )
        )
      }
    }

    message(
      "Creating continuous.timeseries_types and adding timeseries_type to continuous.timeseries..."
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_types (
         timeseries_type TEXT PRIMARY KEY,
         timeseries_type_name TEXT NOT NULL UNIQUE,
         timeseries_type_name_fr TEXT NOT NULL UNIQUE,
         description TEXT,
         description_fr TEXT
       )"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_types IS
       'Lookup table for continuous.timeseries.timeseries_type so applications can translate and describe basic versus derived continuous timeseries consistently.'"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO continuous.timeseries_types (
         timeseries_type,
         timeseries_type_name,
         timeseries_type_name_fr,
         description,
         description_fr
       ) VALUES
         (
           'basic',
           'Basic',
           'De base',
           'Raw measurements stored directly in continuous.measurements_continuous.',
           'Mesures brutes stockées directement dans continuous.measurements_continuous.'
         ),
         (
           'compound',
           'Compound',
           'Composée',
           'Values resolved at query time from one or more other continuous timeseries.',
           'Valeurs résolues au moment de la requête à partir d''une ou plusieurs autres séries temporelles continues.'
         )"
    )

    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_types TO PUBLIC"
    )

    grant_table_privileges(
      "continuous.timeseries_types",
      "SELECT, INSERT, UPDATE, DELETE",
      write_roles
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ADD COLUMN timeseries_type TEXT NOT NULL DEFAULT 'basic'"
    )

    # Add a column to control public visibility, since some derived TS components are not meaningful to expose on their own or are just not publicly relevant, such as battery voltage or signal strength.
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries ADD COLUMN publicly_visible BOOLEAN NOT NULL DEFAULT TRUE"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ADD CONSTRAINT timeseries_timeseries_type_fkey
       FOREIGN KEY (timeseries_type)
       REFERENCES continuous.timeseries_types(timeseries_type)"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.timeseries_type IS
       'Foreign-keyed type code for the continuous timeseries. See continuous.timeseries_types for translated labels and descriptions.' "
    )

    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.publicly_visible IS
       'Controls whether this timeseries should be included in public API responses and user interfaces. Some derived timeseries components are not meaningful to expose on their own or are just not publicly relevant, such as battery voltage or signal strength.'"
    )

    message("Updating the continuous.timeseries uniqueness rule...")

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       DROP CONSTRAINT timeseries_unique"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ADD CONSTRAINT timeseries_unique
       UNIQUE NULLS NOT DISTINCT (
         location_id,
         parameter_id,
         aggregation_type_id,
         media_id,
         matrix_state_id,
         record_rate,
         z_id,
         sensor_priority,
         sub_location_id,
         timeseries_type
       )"
    )

    message("Creating compound definition tables...")

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_compounds (
         timeseries_id INTEGER PRIMARY KEY
           REFERENCES continuous.timeseries(timeseries_id)
           ON DELETE CASCADE,
         expression_sql TEXT,
         created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMP WITH TIME ZONE,
         modified_by TEXT
       )"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_compounds IS
       'One row per compound continuous timeseries. NULL expression_sql means resolve by taking the first available member in member_priority order, with optional use_from/use_to windows.'"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_compounds.expression_sql IS
       'Optional SQL expression evaluated after exact datetime alignment. The expression can reference member aliases directly, for example cond / (1 + 0.0191 * (temp - 25)).'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_compound_members (
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries_compounds(timeseries_id)
           ON DELETE CASCADE,
         member_alias TEXT NOT NULL,
         member_timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id),
         member_priority INTEGER NOT NULL DEFAULT 1,
         use_from TIMESTAMP WITH TIME ZONE,
         use_to TIMESTAMP WITH TIME ZONE,
         created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMP WITH TIME ZONE,
         modified_by TEXT,
         CONSTRAINT timeseries_compound_members_pkey
           PRIMARY KEY (timeseries_id, member_alias),
         CONSTRAINT timeseries_compound_member_alias_ck
           CHECK (member_alias ~ '^[A-Za-z][A-Za-z0-9_]*$'),
         CONSTRAINT timeseries_compound_member_priority_ck
           CHECK (member_priority > 0),
         CONSTRAINT timeseries_compound_member_range_ck
           CHECK (use_to IS NULL OR use_from IS NULL OR use_to > use_from)
       )"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_compound_members IS
       'Member series used to resolve a compound timeseries. use_from/use_to supports period stitching, and member_priority controls fallback order when expression_sql is NULL.'"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX timeseries_compound_members_member_idx
       ON continuous.timeseries_compound_members(member_timeseries_id)"
    )

    message("Applying access controls for the new compound tables...")

    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_compounds TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_compound_members TO PUBLIC"
    )

    grant_table_privileges(
      "continuous.timeseries_compounds",
      "SELECT, INSERT, UPDATE, DELETE",
      write_roles
    )
    grant_table_privileges(
      "continuous.timeseries_compound_members",
      "SELECT, INSERT, UPDATE, DELETE",
      write_roles
    )

    message("Creating validation functions and triggers...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.validate_compound_header()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = NEW.timeseries_id;

         IF v_type IS DISTINCT FROM 'compound' THEN
           RAISE EXCEPTION 'timeseries_id % must have timeseries_type = compound', NEW.timeseries_id;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_compound_header_tr
       BEFORE INSERT OR UPDATE ON continuous.timeseries_compounds
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_compound_header()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.validate_compound_member()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_type TEXT;
         v_has_cycle BOOLEAN;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = NEW.timeseries_id;

         IF v_type IS DISTINCT FROM 'compound' THEN
           RAISE EXCEPTION 'timeseries_id % must have timeseries_type = compound', NEW.timeseries_id;
         END IF;

         IF NEW.member_timeseries_id = NEW.timeseries_id THEN
           RAISE EXCEPTION 'compound timeseries % cannot depend on itself', NEW.timeseries_id;
         END IF;

         WITH RECURSIVE deps AS (
           SELECT NEW.member_timeseries_id AS timeseries_id
           UNION ALL
           SELECT m.member_timeseries_id
           FROM continuous.timeseries_compound_members m
           JOIN deps d
             ON m.timeseries_id = d.timeseries_id
         )
         SELECT EXISTS (
           SELECT 1
           FROM deps
           WHERE timeseries_id = NEW.timeseries_id
         )
         INTO v_has_cycle;

         IF v_has_cycle THEN
           RAISE EXCEPTION 'adding member % would create a compound timeseries cycle for %',
             NEW.member_timeseries_id,
             NEW.timeseries_id;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_compound_member_tr
       BEFORE INSERT OR UPDATE ON continuous.timeseries_compound_members
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_compound_member()"
    )

    message("Adding audit coverage for compound definition tables...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.prevent_compound_definition_key_change()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF
           TG_TABLE_NAME = 'timeseries_compounds' AND
           OLD.timeseries_id IS DISTINCT FROM NEW.timeseries_id
         THEN
           RAISE EXCEPTION 'timeseries_compounds.timeseries_id is immutable; delete and re-insert the definition instead';
         END IF;

         IF
           TG_TABLE_NAME = 'timeseries_compound_members' AND
           (
             OLD.timeseries_id IS DISTINCT FROM NEW.timeseries_id OR
             OLD.member_alias IS DISTINCT FROM NEW.member_alias
           )
         THEN
           RAISE EXCEPTION 'compound member parent and alias are immutable; delete and re-insert the member instead';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER prevent_compound_header_key_change_tr
       BEFORE UPDATE OF timeseries_id ON continuous.timeseries_compounds
       FOR EACH ROW
       EXECUTE FUNCTION continuous.prevent_compound_definition_key_change()"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER prevent_compound_member_key_change_tr
       BEFORE UPDATE OF timeseries_id, member_alias ON continuous.timeseries_compound_members
       FOR EACH ROW
       EXECUTE FUNCTION continuous.prevent_compound_definition_key_change()"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_timeseries_compounds_trigger
       AFTER UPDATE OR DELETE ON continuous.timeseries_compounds
       FOR EACH ROW
       EXECUTE FUNCTION audit.if_modified_func()"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_timeseries_compound_members_trigger
       AFTER UPDATE OR DELETE ON continuous.timeseries_compound_members
       FOR EACH ROW
       EXECUTE FUNCTION audit.if_modified_func()"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX general_log_timeseries_compounds_asof_idx
       ON audit.general_log (
         (
           COALESCE(
             original_data ->> 'timeseries_id',
             new_data ->> 'timeseries_id'
           )::integer
         ),
         action_timestamp,
         log_id
       )
       WHERE schema_name = 'continuous'
         AND table_name = 'timeseries_compounds'"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX general_log_timeseries_compound_members_asof_idx
       ON audit.general_log (
         (
           COALESCE(
             original_data ->> 'timeseries_id',
             new_data ->> 'timeseries_id'
           )::integer
         ),
         COALESCE(
           original_data ->> 'member_alias',
           new_data ->> 'member_alias'
         ),
         action_timestamp,
         log_id
       )
       WHERE schema_name = 'continuous'
         AND table_name = 'timeseries_compound_members'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.reject_compound_measurements()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           WHERE t.timeseries_id = NEW.timeseries_id
             AND t.timeseries_type = 'compound'
         ) THEN
           RAISE EXCEPTION 'raw measurements cannot be inserted for compound timeseries %', NEW.timeseries_id;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER reject_compound_measurements_tr
       BEFORE INSERT OR UPDATE ON continuous.measurements_continuous
       FOR EACH ROW
       EXECUTE FUNCTION continuous.reject_compound_measurements()"
    )

    message("Creating datetime metadata maintenance functions and triggers...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.initialize_compound_timeseries_bounds()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF
           NEW.timeseries_type = 'compound' AND
           (
             TG_OP = 'INSERT' OR
             OLD.timeseries_type IS DISTINCT FROM NEW.timeseries_type
           )
         THEN
           NEW.start_datetime := NULL;
           NEW.end_datetime := NULL;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER initialize_compound_timeseries_bounds_tr
       BEFORE INSERT OR UPDATE OF timeseries_type ON continuous.timeseries
       FOR EACH ROW
       EXECUTE FUNCTION continuous.initialize_compound_timeseries_bounds()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_timeseries_datetime_bounds(
         p_timeseries_ids INTEGER[]
       )
       RETURNS void
       LANGUAGE sql
       AS $function$
         WITH ids AS (
           SELECT DISTINCT x.timeseries_id
           FROM unnest(p_timeseries_ids) AS x(timeseries_id)
           WHERE x.timeseries_id IS NOT NULL
         ),
         bounds AS (
           SELECT
             ids.timeseries_id,
             (
               SELECT MIN(v.start_datetime)
               FROM (VALUES
                 (realtime_bounds.start_datetime),
                 (daily_bounds.start_datetime)
               ) AS v(start_datetime)
             ) AS start_datetime,
             (
               SELECT MAX(v.end_datetime)
               FROM (VALUES
                 (realtime_bounds.end_datetime),
                 (daily_bounds.end_datetime)
               ) AS v(end_datetime)
             ) AS end_datetime
           FROM ids
           LEFT JOIN LATERAL (
             SELECT
               MIN(mc.datetime) AS start_datetime,
               MAX(mc.datetime) AS end_datetime
             FROM continuous.measurements_continuous mc
             WHERE mc.timeseries_id = ids.timeseries_id
           ) realtime_bounds ON TRUE
           LEFT JOIN LATERAL (
             SELECT
               MIN(mcd.date::timestamp AT TIME ZONE 'UTC') AS start_datetime,
               MAX(mcd.date::timestamp AT TIME ZONE 'UTC') AS end_datetime
             FROM continuous.measurements_calculated_daily mcd
             WHERE mcd.timeseries_id = ids.timeseries_id
           ) daily_bounds ON TRUE
         )
         UPDATE continuous.timeseries t
         SET
           start_datetime = b.start_datetime,
           end_datetime = b.end_datetime
         FROM bounds b
         WHERE t.timeseries_id = b.timeseries_id
           AND t.timeseries_type = 'basic'
           AND (
             t.start_datetime IS DISTINCT FROM b.start_datetime OR
             t.end_datetime IS DISTINCT FROM b.end_datetime
           );
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_timeseries_datetime_bounds(INTEGER[]) IS
       'Recomputes start_datetime and end_datetime exactly for the supplied basic continuous timeseries_ids from measurements_continuous and measurements_calculated_daily.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_compound_timeseries_datetime_bounds(
         p_timeseries_id INTEGER
       )
       RETURNS void
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_expression TEXT;
         v_start TIMESTAMP WITH TIME ZONE;
         v_end TIMESTAMP WITH TIME ZONE;
       BEGIN
         IF p_timeseries_id IS NULL THEN
           RETURN;
         END IF;

         SELECT c.expression_sql
         INTO v_expression
         FROM continuous.timeseries t
         LEFT JOIN continuous.timeseries_compounds c
           ON c.timeseries_id = t.timeseries_id
         WHERE t.timeseries_id = p_timeseries_id
           AND t.timeseries_type = 'compound';

         IF NOT FOUND THEN
           RETURN;
         END IF;

         IF v_expression IS NULL THEN
           WITH member_bounds AS (
             SELECT
               CASE
                 WHEN member_ts.start_datetime IS NULL THEN NULL::timestamptz
                 WHEN m.use_from IS NULL THEN member_ts.start_datetime
                 ELSE GREATEST(member_ts.start_datetime, m.use_from)
               END AS effective_start,
               CASE
                 WHEN member_ts.end_datetime IS NULL THEN NULL::timestamptz
                 WHEN m.use_to IS NULL THEN member_ts.end_datetime
                 ELSE LEAST(member_ts.end_datetime, m.use_to)
               END AS effective_end
             FROM continuous.timeseries_compound_members m
             JOIN continuous.timeseries member_ts
               ON member_ts.timeseries_id = m.member_timeseries_id
             WHERE m.timeseries_id = p_timeseries_id
           )
           SELECT
             MIN(effective_start),
             MAX(effective_end)
           INTO v_start, v_end
           FROM member_bounds
           WHERE effective_start IS NOT NULL
             AND effective_end IS NOT NULL
             AND effective_end >= effective_start;
         ELSE
           WITH member_bounds AS (
             SELECT
               CASE
                 WHEN member_ts.start_datetime IS NULL THEN NULL::timestamptz
                 WHEN m.use_from IS NULL THEN member_ts.start_datetime
                 ELSE GREATEST(member_ts.start_datetime, m.use_from)
               END AS effective_start,
               CASE
                 WHEN member_ts.end_datetime IS NULL THEN NULL::timestamptz
                 WHEN m.use_to IS NULL THEN member_ts.end_datetime
                 ELSE LEAST(member_ts.end_datetime, m.use_to)
               END AS effective_end
             FROM continuous.timeseries_compound_members m
             JOIN continuous.timeseries member_ts
               ON member_ts.timeseries_id = m.member_timeseries_id
             WHERE m.timeseries_id = p_timeseries_id
           ),
           stats AS (
             SELECT
               COUNT(*) AS member_count,
               COUNT(*) FILTER (
                 WHERE effective_start IS NOT NULL
                   AND effective_end IS NOT NULL
                   AND effective_end >= effective_start
               ) AS valid_count,
               MAX(effective_start) FILTER (
                 WHERE effective_start IS NOT NULL
                   AND effective_end IS NOT NULL
                   AND effective_end >= effective_start
               ) AS intersect_start,
               MIN(effective_end) FILTER (
                 WHERE effective_start IS NOT NULL
                   AND effective_end IS NOT NULL
                   AND effective_end >= effective_start
               ) AS intersect_end
             FROM member_bounds
           )
           SELECT
             CASE
               WHEN
                 member_count > 0 AND
                 valid_count = member_count AND
                 intersect_start <= intersect_end
               THEN intersect_start
               ELSE NULL::timestamptz
             END,
             CASE
               WHEN
                 member_count > 0 AND
                 valid_count = member_count AND
                 intersect_start <= intersect_end
               THEN intersect_end
               ELSE NULL::timestamptz
             END
           INTO v_start, v_end
           FROM stats;
         END IF;

         UPDATE continuous.timeseries t
         SET
           start_datetime = v_start,
           end_datetime = v_end
         WHERE t.timeseries_id = p_timeseries_id
           AND (
             t.start_datetime IS DISTINCT FROM v_start OR
             t.end_datetime IS DISTINCT FROM v_end
           );
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_compound_timeseries_datetime_bounds(INTEGER) IS
       'Refreshes one compound timeseries start_datetime and end_datetime from current member timeseries metadata and compound member windows.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_direct_compound_timeseries_datetime_bounds(
         p_timeseries_ids INTEGER[]
       )
       RETURNS void
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_timeseries_id INTEGER;
       BEGIN
         IF array_length(p_timeseries_ids, 1) IS NULL THEN
           RETURN;
         END IF;

         FOR v_timeseries_id IN
           SELECT DISTINCT m.timeseries_id
           FROM unnest(p_timeseries_ids) AS src(timeseries_id)
           JOIN continuous.timeseries_compound_members m
             ON m.member_timeseries_id = src.timeseries_id
           WHERE src.timeseries_id IS NOT NULL
           ORDER BY m.timeseries_id
         LOOP
           PERFORM continuous.refresh_compound_timeseries_datetime_bounds(v_timeseries_id);
         END LOOP;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_compound_metadata_on_timeseries_change()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF
           NEW.start_datetime IS DISTINCT FROM OLD.start_datetime OR
           NEW.end_datetime IS DISTINCT FROM OLD.end_datetime OR
           NEW.timeseries_type IS DISTINCT FROM OLD.timeseries_type
         THEN
           PERFORM continuous.refresh_direct_compound_timeseries_datetime_bounds(
             ARRAY[NEW.timeseries_id]
           );
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_compound_metadata_on_timeseries_change_tr
       AFTER UPDATE OF start_datetime, end_datetime, timeseries_type
       ON continuous.timeseries
       FOR EACH ROW
       EXECUTE FUNCTION continuous.refresh_compound_metadata_on_timeseries_change()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_metadata_on_measurements_insert()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         WITH inserted_bounds AS (
           SELECT
             nr.timeseries_id,
             MIN(nr.datetime) AS start_datetime,
             MAX(nr.datetime) AS end_datetime
           FROM new_rows nr
           GROUP BY nr.timeseries_id
         )
         UPDATE continuous.timeseries t
         SET
           start_datetime = CASE
             WHEN t.start_datetime IS NULL THEN inserted_bounds.start_datetime
             ELSE LEAST(t.start_datetime, inserted_bounds.start_datetime)
           END,
           end_datetime = CASE
             WHEN t.end_datetime IS NULL THEN inserted_bounds.end_datetime
             ELSE GREATEST(t.end_datetime, inserted_bounds.end_datetime)
           END,
           last_new_data = CURRENT_TIMESTAMP
         FROM inserted_bounds
         WHERE t.timeseries_id = inserted_bounds.timeseries_id
           AND t.timeseries_type = 'basic'
           AND (
             t.start_datetime IS NULL OR
             inserted_bounds.start_datetime < t.start_datetime OR
             t.end_datetime IS NULL OR
             inserted_bounds.end_datetime > t.end_datetime OR
             t.last_new_data IS NULL OR
             t.last_new_data < CURRENT_TIMESTAMP
           );

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_basic_metadata_on_measurements_insert_tr
       AFTER INSERT ON continuous.measurements_continuous
       REFERENCING NEW TABLE AS new_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.refresh_basic_metadata_on_measurements_insert()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_metadata_on_measurements_delete()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_timeseries_ids INTEGER[];
       BEGIN
         SELECT ARRAY_AGG(DISTINCT orow.timeseries_id)
         INTO v_timeseries_ids
         FROM old_rows orow;

         IF array_length(v_timeseries_ids, 1) IS NOT NULL THEN
           PERFORM continuous.refresh_basic_timeseries_datetime_bounds(v_timeseries_ids);
         END IF;

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_basic_metadata_on_measurements_delete_tr
       AFTER DELETE ON continuous.measurements_continuous
       REFERENCING OLD TABLE AS old_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.refresh_basic_metadata_on_measurements_delete()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_metadata_on_measurements_update()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_timeseries_ids INTEGER[];
       BEGIN
         SELECT ARRAY_AGG(DISTINCT changed.timeseries_id)
         INTO v_timeseries_ids
         FROM (
           SELECT o.timeseries_id
           FROM old_rows o
           JOIN new_rows n
             ON n.measurement_row_id = o.measurement_row_id
           WHERE
             o.timeseries_id IS DISTINCT FROM n.timeseries_id OR
             o.datetime IS DISTINCT FROM n.datetime
           UNION
           SELECT n.timeseries_id
           FROM old_rows o
           JOIN new_rows n
             ON n.measurement_row_id = o.measurement_row_id
           WHERE
             o.timeseries_id IS DISTINCT FROM n.timeseries_id OR
             o.datetime IS DISTINCT FROM n.datetime
         ) changed;

         IF array_length(v_timeseries_ids, 1) IS NOT NULL THEN
           PERFORM continuous.refresh_basic_timeseries_datetime_bounds(v_timeseries_ids);
         END IF;

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_basic_metadata_on_measurements_update_tr
       AFTER UPDATE ON continuous.measurements_continuous
       REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.refresh_basic_metadata_on_measurements_update()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_insert()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         WITH inserted_bounds AS (
           SELECT
             nr.timeseries_id,
             MIN(nr.date::timestamp AT TIME ZONE 'UTC') AS start_datetime,
             MAX(nr.date::timestamp AT TIME ZONE 'UTC') AS end_datetime
           FROM new_rows nr
           GROUP BY nr.timeseries_id
         )
         UPDATE continuous.timeseries t
         SET
           start_datetime = CASE
             WHEN t.start_datetime IS NULL THEN inserted_bounds.start_datetime
             ELSE LEAST(t.start_datetime, inserted_bounds.start_datetime)
           END,
           end_datetime = CASE
             WHEN t.end_datetime IS NULL THEN inserted_bounds.end_datetime
             ELSE GREATEST(t.end_datetime, inserted_bounds.end_datetime)
           END
         FROM inserted_bounds
         WHERE t.timeseries_id = inserted_bounds.timeseries_id
           AND t.timeseries_type = 'basic'
           AND (
             t.start_datetime IS NULL OR
             inserted_bounds.start_datetime < t.start_datetime OR
             t.end_datetime IS NULL OR
             inserted_bounds.end_datetime > t.end_datetime
           );

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_basic_metadata_on_daily_measurements_insert_tr
       AFTER INSERT ON continuous.measurements_calculated_daily
       REFERENCING NEW TABLE AS new_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_insert()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_delete()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_timeseries_ids INTEGER[];
       BEGIN
         SELECT ARRAY_AGG(DISTINCT orow.timeseries_id)
         INTO v_timeseries_ids
         FROM old_rows orow;

         IF array_length(v_timeseries_ids, 1) IS NOT NULL THEN
           PERFORM continuous.refresh_basic_timeseries_datetime_bounds(v_timeseries_ids);
         END IF;

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_basic_metadata_on_daily_measurements_delete_tr
       AFTER DELETE ON continuous.measurements_calculated_daily
       REFERENCING OLD TABLE AS old_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_delete()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_update()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_timeseries_ids INTEGER[];
       BEGIN
         SELECT ARRAY_AGG(DISTINCT changed.timeseries_id)
         INTO v_timeseries_ids
         FROM (
           SELECT o.timeseries_id
           FROM old_rows o
           JOIN new_rows n
             ON n.measurement_row_id = o.measurement_row_id
           WHERE
             o.timeseries_id IS DISTINCT FROM n.timeseries_id OR
             o.date IS DISTINCT FROM n.date
           UNION
           SELECT n.timeseries_id
           FROM old_rows o
           JOIN new_rows n
             ON n.measurement_row_id = o.measurement_row_id
           WHERE
             o.timeseries_id IS DISTINCT FROM n.timeseries_id OR
             o.date IS DISTINCT FROM n.date
         ) changed;

         IF array_length(v_timeseries_ids, 1) IS NOT NULL THEN
           PERFORM continuous.refresh_basic_timeseries_datetime_bounds(v_timeseries_ids);
         END IF;

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_basic_metadata_on_daily_measurements_update_tr
       AFTER UPDATE ON continuous.measurements_calculated_daily
       REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_update()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_compound_metadata_on_header_change()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         PERFORM continuous.refresh_compound_timeseries_datetime_bounds(
           COALESCE(NEW.timeseries_id, OLD.timeseries_id)
         );

         RETURN COALESCE(NEW, OLD);
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_compound_metadata_on_header_change_tr
       AFTER INSERT OR UPDATE OR DELETE ON continuous.timeseries_compounds
       FOR EACH ROW
       EXECUTE FUNCTION continuous.refresh_compound_metadata_on_header_change()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_compound_metadata_on_member_change()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_timeseries_id INTEGER;
       BEGIN
         FOR v_timeseries_id IN
           SELECT DISTINCT ids.timeseries_id
           FROM (
             SELECT NEW.timeseries_id AS timeseries_id
             WHERE TG_OP <> 'DELETE'
             UNION
             SELECT OLD.timeseries_id AS timeseries_id
             WHERE TG_OP <> 'INSERT'
           ) ids
           WHERE ids.timeseries_id IS NOT NULL
         LOOP
           PERFORM continuous.refresh_compound_timeseries_datetime_bounds(v_timeseries_id);
         END LOOP;

         RETURN COALESCE(NEW, OLD);
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_compound_metadata_on_member_change_tr
       AFTER INSERT OR UPDATE OR DELETE ON continuous.timeseries_compound_members
       FOR EACH ROW
       EXECUTE FUNCTION continuous.refresh_compound_metadata_on_member_change()"
    )

    message("Creating historical compound definition helpers...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.timeseries_compounds_as_of(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         expression_sql TEXT,
         created TIMESTAMPTZ,
         modified TIMESTAMPTZ
       )
       LANGUAGE sql
       STABLE
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
         WITH current_rows AS (
           SELECT
             c.timeseries_id,
             to_jsonb(c) AS row_json,
             c.created AS row_created
           FROM continuous.timeseries_compounds c
           WHERE (
             p_timeseries_ids IS NULL OR
             c.timeseries_id = ANY(p_timeseries_ids)
           )
         ),
         future_changes AS (
           SELECT DISTINCT ON (x.timeseries_id)
             x.timeseries_id,
             x.row_json,
             x.row_created
           FROM (
             SELECT
               COALESCE(
                 (g.original_data ->> 'timeseries_id')::integer,
                 (g.new_data ->> 'timeseries_id')::integer
               ) AS timeseries_id,
               g.original_data ||
                 jsonb_strip_nulls(
                   jsonb_build_object(
                     'created', g.row_created,
                     'modified', g.row_modified
                   )
                 ) AS row_json,
               g.row_created,
               g.action_timestamp,
               g.log_id
             FROM audit.general_log g
             WHERE g.schema_name = 'continuous'
               AND g.table_name = 'timeseries_compounds'
               AND g.action_timestamp > p_as_of
               AND (
                 p_timeseries_ids IS NULL OR
                 COALESCE(
                   (g.original_data ->> 'timeseries_id')::integer,
                   (g.new_data ->> 'timeseries_id')::integer
                 ) = ANY(p_timeseries_ids)
               )
           ) x
           ORDER BY x.timeseries_id, x.action_timestamp ASC, x.log_id ASC
         ),
         snapshot_json AS (
           SELECT
             COALESCE(f.timeseries_id, c.timeseries_id) AS timeseries_id,
             COALESCE(f.row_json, c.row_json) AS row_json,
             COALESCE(f.row_created, c.row_created) AS row_created
           FROM current_rows c
           FULL OUTER JOIN future_changes f
             ON c.timeseries_id = f.timeseries_id
         )
         SELECT
           r.timeseries_id,
           r.expression_sql,
           r.created,
           r.modified
         FROM snapshot_json s
         CROSS JOIN LATERAL jsonb_to_record(s.row_json) AS r(
           timeseries_id INTEGER,
           expression_sql TEXT,
           created TIMESTAMPTZ,
           modified TIMESTAMPTZ
         )
         WHERE COALESCE(r.created, s.row_created) <= p_as_of
         ORDER BY r.timeseries_id;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.timeseries_compounds_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) IS
       'Reconstructs compound timeseries header definitions as they existed at a requested timestamp using continuous.timeseries_compounds plus audit.general_log.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.timeseries_compound_members_as_of(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         member_alias TEXT,
         member_timeseries_id INTEGER,
         member_priority INTEGER,
         use_from TIMESTAMPTZ,
         use_to TIMESTAMPTZ,
         created TIMESTAMPTZ,
         modified TIMESTAMPTZ
       )
       LANGUAGE sql
       STABLE
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
         WITH current_rows AS (
           SELECT
             m.timeseries_id,
             m.member_alias,
             to_jsonb(m) AS row_json,
             m.created AS row_created
           FROM continuous.timeseries_compound_members m
           WHERE (
             p_timeseries_ids IS NULL OR
             m.timeseries_id = ANY(p_timeseries_ids)
           )
         ),
         future_changes AS (
           SELECT DISTINCT ON (x.timeseries_id, x.member_alias)
             x.timeseries_id,
             x.member_alias,
             x.row_json,
             x.row_created
           FROM (
             SELECT
               COALESCE(
                 (g.original_data ->> 'timeseries_id')::integer,
                 (g.new_data ->> 'timeseries_id')::integer
               ) AS timeseries_id,
               COALESCE(
                 g.original_data ->> 'member_alias',
                 g.new_data ->> 'member_alias'
               ) AS member_alias,
               g.original_data ||
                 jsonb_strip_nulls(
                   jsonb_build_object(
                     'created', g.row_created,
                     'modified', g.row_modified
                   )
                 ) AS row_json,
               g.row_created,
               g.action_timestamp,
               g.log_id
             FROM audit.general_log g
             WHERE g.schema_name = 'continuous'
               AND g.table_name = 'timeseries_compound_members'
               AND g.action_timestamp > p_as_of
               AND (
                 p_timeseries_ids IS NULL OR
                 COALESCE(
                   (g.original_data ->> 'timeseries_id')::integer,
                   (g.new_data ->> 'timeseries_id')::integer
                 ) = ANY(p_timeseries_ids)
               )
           ) x
           ORDER BY
             x.timeseries_id,
             x.member_alias,
             x.action_timestamp ASC,
             x.log_id ASC
         ),
         snapshot_json AS (
           SELECT
             COALESCE(f.timeseries_id, c.timeseries_id) AS timeseries_id,
             COALESCE(f.member_alias, c.member_alias) AS member_alias,
             COALESCE(f.row_json, c.row_json) AS row_json,
             COALESCE(f.row_created, c.row_created) AS row_created
           FROM current_rows c
           FULL OUTER JOIN future_changes f
             ON c.timeseries_id = f.timeseries_id
            AND c.member_alias = f.member_alias
         )
         SELECT
           r.timeseries_id,
           r.member_alias,
           r.member_timeseries_id,
           r.member_priority,
           r.use_from,
           r.use_to,
           r.created,
           r.modified
         FROM snapshot_json s
         CROSS JOIN LATERAL jsonb_to_record(s.row_json) AS r(
           timeseries_id INTEGER,
           member_alias TEXT,
           member_timeseries_id INTEGER,
           member_priority INTEGER,
           use_from TIMESTAMPTZ,
           use_to TIMESTAMPTZ,
           created TIMESTAMPTZ,
           modified TIMESTAMPTZ
         )
         WHERE COALESCE(r.created, s.row_created) <= p_as_of
         ORDER BY r.timeseries_id, r.member_priority, r.member_alias;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.timeseries_compound_members_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) IS
       'Reconstructs compound timeseries member definitions as they existed at a requested timestamp using continuous.timeseries_compound_members plus audit.general_log.'"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.timeseries_compounds_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.timeseries_compound_members_as_of(
         TIMESTAMPTZ,
         INTEGER[]
       ) FROM PUBLIC"
    )

    grant_function_privileges(
      "audit.timeseries_compounds_as_of(TIMESTAMPTZ, INTEGER[])",
      "EXECUTE",
      historical_function_roles
    )
    grant_function_privileges(
      "audit.timeseries_compound_members_as_of(TIMESTAMPTZ, INTEGER[])",
      "EXECUTE",
      historical_function_roles
    )

    message("Creating compound resolver functions...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_internal(
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         p_path INTEGER[] DEFAULT ARRAY[]::INTEGER[]
       )
       RETURNS TABLE (
         datetime TIMESTAMP WITH TIME ZONE,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           SELECT
             mc.datetime,
             continuous.apply_corrections(
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed
           FROM continuous.measurements_continuous mc
           WHERE mc.timeseries_id = p_timeseries_id
             AND (p_from IS NULL OR mc.datetime >= p_from)
             AND (p_to IS NULL OR mc.datetime <= p_to);

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           src.datetime,
           continuous.apply_corrections(
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed
         FROM continuous.resolve_compound_timeseries_raw_window(
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
      "CREATE OR REPLACE FUNCTION continuous.resolve_compound_timeseries_raw_window(
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         p_path INTEGER[] DEFAULT ARRAY[]::INTEGER[]
       )
       RETURNS TABLE (
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_type TEXT;
         v_expression TEXT;
         v_first BOOLEAN := TRUE;
         v_inner_select TEXT := '';
         v_from_clause TEXT := '';
         v_period_expr TEXT := 'NULL::interval';
         v_imputed_expr TEXT := 'FALSE';
         v_sql TEXT;
         v_member RECORD;
       BEGIN
         IF p_timeseries_id = ANY (p_path) THEN
           RAISE EXCEPTION 'compound resolution cycle detected at timeseries %', p_timeseries_id;
         END IF;

         SELECT t.timeseries_type, c.expression_sql
         INTO v_type, v_expression
         FROM continuous.timeseries t
         LEFT JOIN continuous.timeseries_compounds c
           ON c.timeseries_id = t.timeseries_id
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type IS DISTINCT FROM 'compound' THEN
           RAISE EXCEPTION 'timeseries % must have timeseries_type = compound', p_timeseries_id;
         END IF;

         IF v_expression IS NULL THEN
           RETURN QUERY
           WITH member_data AS (
             SELECT
               m.member_alias,
               m.member_priority,
               src.datetime,
               src.value_corrected AS value_raw,
               src.period,
               src.imputed
             FROM continuous.timeseries_compound_members m
             JOIN LATERAL continuous.measurements_continuous_corrected_internal(
               m.member_timeseries_id,
               CASE
                 WHEN p_from IS NULL THEN m.use_from
                 WHEN m.use_from IS NULL THEN p_from
                 ELSE GREATEST(p_from, m.use_from)
               END,
               CASE
                 WHEN p_to IS NULL THEN m.use_to
                 WHEN m.use_to IS NULL THEN p_to
                 ELSE LEAST(p_to, m.use_to)
               END,
               p_path || p_timeseries_id
             ) src
               ON TRUE
             WHERE m.timeseries_id = p_timeseries_id
               AND (m.use_from IS NULL OR p_to IS NULL OR src.datetime >= m.use_from)
               AND (m.use_to IS NULL OR p_from IS NULL OR src.datetime < m.use_to)
           ),
           ranked AS (
             SELECT
               member_data.datetime,
               member_data.value_raw,
               member_data.period,
               member_data.imputed,
               ROW_NUMBER() OVER (
                 PARTITION BY member_data.datetime
                 ORDER BY member_data.member_priority, member_data.member_alias
               ) AS rn
             FROM member_data
             WHERE member_data.value_raw IS NOT NULL
           )
           SELECT
             ranked.datetime,
             ranked.value_raw,
             ranked.period,
             ranked.imputed
           FROM ranked
           WHERE rn = 1;

           RETURN;
         END IF;

         FOR v_member IN
           SELECT
             member_alias,
             member_timeseries_id,
             use_from,
             use_to
           FROM continuous.timeseries_compound_members
           WHERE timeseries_id = p_timeseries_id
           ORDER BY member_priority, member_alias
         LOOP
           IF v_first THEN
             v_inner_select := format(
               '%1$I.datetime, %1$I.value_corrected AS %1$I, %1$I.period AS %1$I__period, %1$I.imputed AS %1$I__imputed',
               v_member.member_alias
             );
             v_from_clause := format(
               '(SELECT *
                 FROM continuous.measurements_continuous_corrected_internal(
                   %1$s,
                   %2$L::timestamptz,
                   %3$L::timestamptz,
                   %4$L::integer[]
                 ) src
               ) %5$I',
               v_member.member_timeseries_id,
               CASE
                 WHEN p_from IS NULL THEN v_member.use_from
                 WHEN v_member.use_from IS NULL THEN p_from
                 ELSE GREATEST(p_from, v_member.use_from)
               END,
               CASE
                 WHEN p_to IS NULL THEN v_member.use_to
                 WHEN v_member.use_to IS NULL THEN p_to
                 ELSE LEAST(p_to, v_member.use_to)
               END,
               p_path || p_timeseries_id,
               v_member.member_alias
             );
             v_period_expr := format('aligned.%I__period', v_member.member_alias);
             v_imputed_expr := format('COALESCE(aligned.%I__imputed, FALSE)', v_member.member_alias);
             v_first := FALSE;
           ELSE
             v_inner_select := v_inner_select || format(
               ', %1$I.value_corrected AS %1$I, %1$I.period AS %1$I__period, %1$I.imputed AS %1$I__imputed',
               v_member.member_alias
             );
             v_from_clause := v_from_clause || format(
               ' JOIN (
                   SELECT *
                   FROM continuous.measurements_continuous_corrected_internal(
                     %1$s,
                     %2$L::timestamptz,
                     %3$L::timestamptz,
                     %4$L::integer[]
                   ) src
                 ) %5$I
                 USING (datetime)',
               v_member.member_timeseries_id,
               CASE
                 WHEN p_from IS NULL THEN v_member.use_from
                 WHEN v_member.use_from IS NULL THEN p_from
                 ELSE GREATEST(p_from, v_member.use_from)
               END,
               CASE
                 WHEN p_to IS NULL THEN v_member.use_to
                 WHEN v_member.use_to IS NULL THEN p_to
                 ELSE LEAST(p_to, v_member.use_to)
               END,
               p_path || p_timeseries_id,
               v_member.member_alias
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I__imputed, FALSE)',
               v_member.member_alias
             );
           END IF;
         END LOOP;

         IF v_first THEN
           RAISE EXCEPTION 'compound timeseries % has no members', p_timeseries_id;
         END IF;

         v_sql := format(
           'SELECT
              aligned.datetime,
              %1$s AS value_raw,
              %2$s AS period,
              %3$s AS imputed
            FROM (
              SELECT %4$s
              FROM %5$s
            ) aligned',
           v_expression,
           v_period_expr,
           v_imputed_expr,
           v_inner_select,
           v_from_clause
         );

         RETURN QUERY EXECUTE v_sql;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.resolve_compound_timeseries_raw_window(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Resolves one compound timeseries to raw derived values for a requested datetime window. Member values already include their own corrections.'"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_internal(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Internal helper used by the compound resolver so each recursive step only resolves one timeseries_id and one datetime window.'"
    )

    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.measurements_continuous_corrected CASCADE"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected(
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN,
         created TIMESTAMP WITH TIME ZONE,
         modified TIMESTAMP WITH TIME ZONE
       )
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           SELECT
             mc.timeseries_id,
             mc.datetime,
             mc.value AS value_raw,
             continuous.apply_corrections(
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed,
             mc.created,
             mc.modified
           FROM continuous.measurements_continuous mc
           WHERE mc.timeseries_id = p_timeseries_id
             AND (p_from IS NULL OR mc.datetime >= p_from)
             AND (p_to IS NULL OR mc.datetime <= p_to);

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           p_timeseries_id,
           src.datetime,
           src.value_raw,
           continuous.apply_corrections(
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed,
           NULL::timestamptz AS created,
           NULL::timestamptz AS modified
         FROM continuous.resolve_compound_timeseries_raw_window(
           p_timeseries_id,
           p_from,
           p_to,
           ARRAY[]::INTEGER[]
         ) src;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) IS
       'Returns corrected measurements for one basic or compound timeseries over a requested datetime window.'"
    )

    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.resolve_compound_timeseries_raw_window(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.measurements_continuous_corrected_internal(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.measurements_continuous_corrected(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) TO PUBLIC"
    )

    grant_function_privileges(
      "continuous.resolve_compound_timeseries_raw_window(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[])",
      "EXECUTE",
      write_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected_internal(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[])",
      "EXECUTE",
      write_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE)",
      "EXECUTE",
      write_roles
    )

    message("Creating point-in-time compound measurement functions...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.resolve_compound_timeseries_raw_window_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         p_path INTEGER[] DEFAULT ARRAY[]::INTEGER[]
       )
       RETURNS TABLE (
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
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
         v_expression TEXT;
         v_first BOOLEAN := TRUE;
         v_inner_select TEXT := '';
         v_from_clause TEXT := '';
         v_period_expr TEXT := 'NULL::interval';
         v_imputed_expr TEXT := 'FALSE';
         v_sql TEXT;
         v_member RECORD;
       BEGIN
         IF p_timeseries_id = ANY (p_path) THEN
           RAISE EXCEPTION 'compound resolution cycle detected at timeseries %', p_timeseries_id;
         END IF;

         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type IS DISTINCT FROM 'compound' THEN
           RAISE EXCEPTION 'timeseries % must have timeseries_type = compound', p_timeseries_id;
         END IF;

         SELECT c.expression_sql
         INTO v_expression
         FROM audit.timeseries_compounds_as_of(
           p_as_of,
           ARRAY[p_timeseries_id]
         ) c
         WHERE c.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RETURN;
         END IF;

         IF v_expression IS NULL THEN
           RETURN QUERY
           WITH member_data AS (
             SELECT
               m.member_alias,
               m.member_priority,
               src.datetime,
               src.value_corrected AS value_raw,
               src.period,
               src.imputed
             FROM audit.timeseries_compound_members_as_of(
               p_as_of,
               ARRAY[p_timeseries_id]
             ) m
             JOIN LATERAL continuous.measurements_continuous_corrected_internal_at(
               p_as_of,
               m.member_timeseries_id,
               CASE
                 WHEN p_from IS NULL THEN m.use_from
                 WHEN m.use_from IS NULL THEN p_from
                 ELSE GREATEST(p_from, m.use_from)
               END,
               CASE
                 WHEN p_to IS NULL THEN m.use_to
                 WHEN m.use_to IS NULL THEN p_to
                 ELSE LEAST(p_to, m.use_to)
               END,
               p_path || p_timeseries_id
             ) src
               ON TRUE
             WHERE m.timeseries_id = p_timeseries_id
               AND (m.use_from IS NULL OR p_to IS NULL OR src.datetime >= m.use_from)
               AND (m.use_to IS NULL OR p_from IS NULL OR src.datetime < m.use_to)
           ),
           ranked AS (
             SELECT
               member_data.datetime,
               member_data.value_raw,
               member_data.period,
               member_data.imputed,
               ROW_NUMBER() OVER (
                 PARTITION BY member_data.datetime
                 ORDER BY member_data.member_priority, member_data.member_alias
               ) AS rn
             FROM member_data
             WHERE member_data.value_raw IS NOT NULL
           )
           SELECT
             ranked.datetime,
             ranked.value_raw,
             ranked.period,
             ranked.imputed
           FROM ranked
           WHERE rn = 1;

           RETURN;
         END IF;

         FOR v_member IN
           SELECT
             member_alias,
             member_timeseries_id,
             use_from,
             use_to
           FROM audit.timeseries_compound_members_as_of(
             p_as_of,
             ARRAY[p_timeseries_id]
           )
           WHERE timeseries_id = p_timeseries_id
           ORDER BY member_priority, member_alias
         LOOP
           IF v_first THEN
             v_inner_select := format(
               '%1$I.datetime, %1$I.value_corrected AS %1$I, %1$I.period AS %1$I__period, %1$I.imputed AS %1$I__imputed',
               v_member.member_alias
             );
             v_from_clause := format(
               '(SELECT *
                 FROM continuous.measurements_continuous_corrected_internal_at(
                   %1$L::timestamptz,
                   %2$s,
                   %3$L::timestamptz,
                   %4$L::timestamptz,
                   %5$L::integer[]
                 ) src
               ) %6$I',
               p_as_of,
               v_member.member_timeseries_id,
               CASE
                 WHEN p_from IS NULL THEN v_member.use_from
                 WHEN v_member.use_from IS NULL THEN p_from
                 ELSE GREATEST(p_from, v_member.use_from)
               END,
               CASE
                 WHEN p_to IS NULL THEN v_member.use_to
                 WHEN v_member.use_to IS NULL THEN p_to
                 ELSE LEAST(p_to, v_member.use_to)
               END,
               p_path || p_timeseries_id,
               v_member.member_alias
             );
             v_period_expr := format('aligned.%I__period', v_member.member_alias);
             v_imputed_expr := format('COALESCE(aligned.%I__imputed, FALSE)', v_member.member_alias);
             v_first := FALSE;
           ELSE
             v_inner_select := v_inner_select || format(
               ', %1$I.value_corrected AS %1$I, %1$I.period AS %1$I__period, %1$I.imputed AS %1$I__imputed',
               v_member.member_alias
             );
             v_from_clause := v_from_clause || format(
               ' JOIN (
                   SELECT *
                   FROM continuous.measurements_continuous_corrected_internal_at(
                     %1$L::timestamptz,
                     %2$s,
                     %3$L::timestamptz,
                     %4$L::timestamptz,
                     %5$L::integer[]
                   ) src
                 ) %6$I
                 USING (datetime)',
               p_as_of,
               v_member.member_timeseries_id,
               CASE
                 WHEN p_from IS NULL THEN v_member.use_from
                 WHEN v_member.use_from IS NULL THEN p_from
                 ELSE GREATEST(p_from, v_member.use_from)
               END,
               CASE
                 WHEN p_to IS NULL THEN v_member.use_to
                 WHEN v_member.use_to IS NULL THEN p_to
                 ELSE LEAST(p_to, v_member.use_to)
               END,
               p_path || p_timeseries_id,
               v_member.member_alias
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I__imputed, FALSE)',
               v_member.member_alias
             );
           END IF;
         END LOOP;

         IF v_first THEN
           RAISE EXCEPTION 'compound timeseries % had no members at %',
             p_timeseries_id,
             p_as_of;
         END IF;

         v_sql := format(
           'SELECT
              aligned.datetime,
              %1$s AS value_raw,
              %2$s AS period,
              %3$s AS imputed
            FROM (
              SELECT %4$s
              FROM %5$s
            ) aligned',
           v_expression,
           v_period_expr,
           v_imputed_expr,
           v_inner_select,
           v_from_clause
         );

         RETURN QUERY EXECUTE v_sql;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.resolve_compound_timeseries_raw_window_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Resolves one compound timeseries to raw derived values for a requested datetime window using the compound definition and source measurements/corrections that existed at the requested as-of timestamp.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_internal_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         p_path INTEGER[] DEFAULT ARRAY[]::INTEGER[]
       )
       RETURNS TABLE (
         datetime TIMESTAMP WITH TIME ZONE,
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
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
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
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_internal_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Internal point-in-time helper used by the historical compound resolver so each recursive step only resolves one timeseries_id and one datetime window.'"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION continuous.measurements_continuous_corrected_at(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN,
         created TIMESTAMP WITH TIME ZONE,
         modified TIMESTAMP WITH TIME ZONE
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           SELECT
             mc.timeseries_id,
             mc.datetime,
             mc.value AS value_raw,
             continuous.apply_corrections_at(
               p_as_of,
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed,
             mc.created,
             mc.modified
           FROM audit.measurements_continuous_as_of(
             p_as_of,
             ARRAY[p_timeseries_id],
             p_from,
             p_to
           ) mc;

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           p_timeseries_id,
           src.datetime,
           src.value_raw,
           continuous.apply_corrections_at(
             p_as_of,
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed,
           NULL::timestamptz AS created,
           NULL::timestamptz AS modified
         FROM continuous.resolve_compound_timeseries_raw_window_at(
           p_as_of,
           p_timeseries_id,
           p_from,
           p_to,
           ARRAY[]::INTEGER[]
         ) src;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) IS
       'Returns corrected measurements for one basic or compound timeseries over a requested datetime window using measurements, corrections, and compound definitions as they existed at the requested as-of timestamp.'"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.resolve_compound_timeseries_raw_window_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.measurements_continuous_corrected_internal_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.measurements_continuous_corrected_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) FROM PUBLIC"
    )

    grant_function_privileges(
      "continuous.resolve_compound_timeseries_raw_window_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[])",
      "EXECUTE",
      historical_function_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected_internal_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[])",
      "EXECUTE",
      historical_function_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE)",
      "EXECUTE",
      historical_function_roles
    )
    message("Creating a dependency inspection view...")

    DBI::dbExecute(
      con,
      "CREATE VIEW continuous.timeseries_compound_dependencies AS
       SELECT
         parent.timeseries_id AS compound_timeseries_id,
         parent.timeseries_type AS compound_timeseries_type,
         m.member_alias,
         m.member_priority,
         m.use_from,
         m.use_to,
         member.timeseries_id AS member_timeseries_id,
         member.timeseries_type AS member_timeseries_type
       FROM continuous.timeseries_compound_members m
       JOIN continuous.timeseries parent
         ON parent.timeseries_id = m.timeseries_id
       JOIN continuous.timeseries member
         ON member.timeseries_id = m.member_timeseries_id"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON VIEW continuous.timeseries_compound_dependencies IS
       'Lists direct dependencies between compound timeseries and their member series.'"
    )

    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_compound_dependencies TO PUBLIC"
    )

    grant_table_privileges(
      "continuous.timeseries_compound_dependencies",
      "SELECT",
      write_roles
    )

    message(
      "Refreshing continuous.timeseries_metadata_en and continuous.timeseries_metadata_fr..."
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW continuous.timeseries_metadata_en AS
       SELECT
         ts.timeseries_id,
         loc.location_id,
         loc.name AS location_name,
         loc.alias AS alias_name,
         lz.z_meters AS depth_height_m,
         loc.latitude,
         loc.longitude,
         dc.conversion_m AS location_elevation,
         array_agg(DISTINCT proj.name) AS projects,
         array_agg(DISTINCT net.name) AS networks,
         mtypes.media_type,
         params.param_name AS parameter_name,
         get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id) AS units,
         at.aggregation_type,
         ts.record_rate AS recording_rate,
         ts.sensor_priority,
         ts.start_datetime,
         ts.end_datetime,
         ts.note,
         ts.timeseries_type AS timeseries_type_code,
         tt.timeseries_type_name AS timeseries_type,
         tt.description AS timeseries_type_description,
         ts.last_new_data
       FROM continuous.timeseries ts
       JOIN public.locations loc
         ON ts.location_id = loc.location_id
       LEFT JOIN public.parameters params
         ON ts.parameter_id = params.parameter_id
       LEFT JOIN public.media_types mtypes
         ON ts.media_id = mtypes.media_id
       LEFT JOIN continuous.aggregation_types at
         ON ts.aggregation_type_id = at.aggregation_type_id
       LEFT JOIN public.locations_z lz
         ON ts.z_id = lz.z_id
       LEFT JOIN public.locations_projects loc_proj
         ON loc.location_id = loc_proj.location_id
       LEFT JOIN public.projects proj
         ON loc_proj.project_id = proj.project_id
       LEFT JOIN public.locations_networks loc_net
         ON loc.location_id = loc_net.location_id
       LEFT JOIN public.networks net
         ON loc_net.network_id = net.network_id
       LEFT JOIN public.datum_conversions dc
         ON loc.location_id = dc.location_id
        AND dc.current = TRUE
       LEFT JOIN continuous.timeseries_types tt
         ON ts.timeseries_type = tt.timeseries_type
       GROUP BY
         ts.timeseries_id,
         loc.location_id,
         mtypes.media_type,
         params.param_name,
         get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id),
         at.aggregation_type,
         lz.z_meters,
         dc.conversion_m,
         tt.timeseries_type_name,
         tt.description"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr AS
       SELECT
         ts.timeseries_id,
         loc.location_id,
         loc.name_fr AS nom_endroit,
         loc.alias AS nom_alias,
         lz.z_meters AS profondeur_hauteur_m,
         loc.latitude,
         loc.longitude,
         dc.conversion_m AS \"élévation_endroit\",
         array_agg(DISTINCT proj.name_fr) AS projets,
         array_agg(DISTINCT net.name_fr) AS \"réseaux\",
         mtypes.media_type_fr AS \"type_de_média\",
         params.param_name_fr AS \"nom_paramètre\",
         get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id) AS \"unités\",
         ag.aggregation_type_fr AS \"type_agrégation\",
         ts.record_rate AS \"fréquence_enregistrement\",
         ts.sensor_priority AS \"priorité_capteur\",
         ts.start_datetime AS \"début\",
         ts.end_datetime AS fin,
         ts.note,
         ts.timeseries_type AS code_type_serie_temporelle,
         tt.timeseries_type_name_fr AS type_serie_temporelle,
         tt.description_fr AS description_type_serie_temporelle,
         ts.last_new_data AS dernier_nouvelles_donnees
       FROM continuous.timeseries ts
       JOIN public.locations loc
         ON ts.location_id = loc.location_id
       LEFT JOIN public.parameters params
         ON ts.parameter_id = params.parameter_id
       LEFT JOIN public.media_types mtypes
         ON ts.media_id = mtypes.media_id
       LEFT JOIN continuous.aggregation_types ag
         ON ts.aggregation_type_id = ag.aggregation_type_id
       LEFT JOIN public.locations_z lz
         ON ts.z_id = lz.z_id
       LEFT JOIN public.locations_projects loc_proj
         ON loc.location_id = loc_proj.location_id
       LEFT JOIN public.projects proj
         ON loc_proj.project_id = proj.project_id
       LEFT JOIN public.locations_networks loc_net
         ON loc.location_id = loc_net.location_id
       LEFT JOIN public.networks net
         ON loc_net.network_id = net.network_id
       LEFT JOIN public.datum_conversions dc
         ON loc.location_id = dc.location_id
        AND dc.current = TRUE
       LEFT JOIN continuous.timeseries_types tt
         ON ts.timeseries_type = tt.timeseries_type
       GROUP BY
         ts.timeseries_id,
         loc.location_id,
         mtypes.media_type_fr,
         params.param_name_fr,
         get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id),
         ag.aggregation_type_fr,
         lz.z_meters,
         dc.conversion_m,
         tt.timeseries_type_name_fr,
         tt.description_fr"
    )

    message(
      "Using function-based measurement access through continuous.measurements_continuous_corrected() and continuous.measurements_continuous_corrected_at()."
    )

    message("Creating database-side calculated daily refresh functions...")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.normalized_day_of_year(p_date date)
RETURNS integer
LANGUAGE sql
IMMUTABLE
AS $function$
  SELECT CASE
    WHEN EXTRACT(MONTH FROM p_date)::integer = 2
      AND EXTRACT(DAY FROM p_date)::integer = 29 THEN NULL::integer
    WHEN (
      (
        EXTRACT(YEAR FROM p_date)::integer % 400 = 0
        OR (
          EXTRACT(YEAR FROM p_date)::integer % 4 = 0
          AND EXTRACT(YEAR FROM p_date)::integer % 100 <> 0
        )
      )
      AND EXTRACT(DOY FROM p_date)::integer > 60
    ) THEN EXTRACT(DOY FROM p_date)::integer - 1
    ELSE EXTRACT(DOY FROM p_date)::integer
  END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.local_noon_to_utc(
  p_date date,
  p_timezone_offset_hours integer
)
RETURNS timestamp with time zone
LANGUAGE sql
IMMUTABLE
AS $function$
  SELECT (
    p_date::timestamp
    + interval '12 hours'
    - make_interval(hours => p_timezone_offset_hours)
  ) AT TIME ZONE 'UTC';
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.downstream_timeseries_ids(
  p_timeseries_ids integer[]
)
RETURNS TABLE(timeseries_id integer)
LANGUAGE sql
STABLE
AS $function$
  WITH RECURSIVE deps AS (
    SELECT DISTINCT x.timeseries_id
    FROM unnest(p_timeseries_ids) AS x(timeseries_id)
    WHERE x.timeseries_id IS NOT NULL

    UNION

    SELECT m.timeseries_id
    FROM continuous.timeseries_compound_members m
    JOIN deps d
      ON d.timeseries_id = m.member_timeseries_id
  )
  SELECT DISTINCT deps.timeseries_id
  FROM deps;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily(
  p_timeseries_id integer,
  p_changed_start date DEFAULT NULL,
  p_changed_end date DEFAULT NULL
)
RETURNS void
LANGUAGE plpgsql
AS $function$
DECLARE
  v_offset integer;
  v_aggregation_type text;
  v_record_rate interval;
  v_timeseries_type text;
  v_start_date date;
  v_end_date date;
  v_start_ts timestamp with time zone;
  v_end_ts timestamp with time zone;
  v_recalc_feb29 boolean;
BEGIN
  SELECT
    t.timezone_daily_calc,
    at.aggregation_type,
    t.record_rate,
    t.timeseries_type
  INTO
    v_offset,
    v_aggregation_type,
    v_record_rate,
    v_timeseries_type
  FROM continuous.timeseries t
  JOIN continuous.aggregation_types at
    ON at.aggregation_type_id = t.aggregation_type_id
  WHERE t.timeseries_id = p_timeseries_id;

  IF NOT FOUND THEN
    RETURN;
  END IF;

  IF v_record_rate IS NOT NULL AND v_record_rate > interval '1 day' THEN
    RETURN;
  END IF;

  IF v_timeseries_type = 'compound'
    AND NOT EXISTS (
      SELECT 1
      FROM continuous.timeseries_compound_members m
      WHERE m.timeseries_id = p_timeseries_id
    ) THEN
    RETURN;
  END IF;

  v_start_date := p_changed_start;
  v_end_date := p_changed_end;

  IF v_start_date IS NULL THEN
    WITH measurement_bounds AS (
      SELECT
        MIN((m.datetime + make_interval(hours => v_offset))::date) AS min_date,
        MAX((m.datetime + make_interval(hours => v_offset))::date) AS max_date
      FROM continuous.measurements_continuous_corrected(
        p_timeseries_id,
        NULL::timestamptz,
        NULL::timestamptz
      ) m
      WHERE m.period <= interval '1 day'
        AND NOT EXISTS (
          SELECT 1
          FROM continuous.grades g
          JOIN public.grade_types gt
            ON gt.grade_type_id = g.grade_type_id
          WHERE g.timeseries_id = p_timeseries_id
            AND gt.grade_type_code = 'N'
            AND g.start_dt <> g.end_dt
            AND m.datetime BETWEEN g.start_dt AND g.end_dt
        )
    ),
    daily_bounds AS (
      SELECT
        MIN(mcd.date) AS min_date,
        MAX(mcd.date) AS max_date
      FROM continuous.measurements_calculated_daily mcd
      WHERE mcd.timeseries_id = p_timeseries_id
    )
    SELECT
      LEAST(measurement_bounds.min_date, daily_bounds.min_date),
      GREATEST(measurement_bounds.max_date, daily_bounds.max_date)
    INTO v_start_date, v_end_date
    FROM measurement_bounds, daily_bounds;
  END IF;

  IF v_start_date IS NULL THEN
    RETURN;
  END IF;

  IF v_end_date IS NULL THEN
    v_end_date := v_start_date;
  END IF;

  IF v_end_date < v_start_date THEN
    v_start_date := p_changed_end;
    v_end_date := p_changed_start;
  END IF;

  v_start_ts := (
    v_start_date::timestamp - make_interval(hours => v_offset)
  ) AT TIME ZONE 'UTC';
  v_end_ts := (
    (v_end_date + 1)::timestamp - make_interval(hours => v_offset)
  ) AT TIME ZONE 'UTC' - interval '1 microsecond';

  WITH days AS (
    SELECT d::date AS date
    FROM generate_series(v_start_date, v_end_date, interval '1 day') AS d
  ),
  raw_measurements AS MATERIALIZED (
    SELECT
      (m.datetime + make_interval(hours => v_offset))::date AS date,
      m.value_corrected AS value,
      m.imputed
    FROM continuous.measurements_continuous_corrected(
      p_timeseries_id,
      v_start_ts,
      v_end_ts
    ) m
    WHERE m.period <= interval '1 day'
      AND NOT EXISTS (
        SELECT 1
        FROM continuous.grades g
        JOIN public.grade_types gt
          ON gt.grade_type_id = g.grade_type_id
        WHERE g.timeseries_id = p_timeseries_id
          AND gt.grade_type_code = 'N'
          AND g.start_dt <> g.end_dt
          AND m.datetime BETWEEN g.start_dt AND g.end_dt
      )
  ),
  existing_daily AS MATERIALIZED (
    SELECT mcd.date
    FROM continuous.measurements_calculated_daily mcd
    WHERE mcd.timeseries_id = p_timeseries_id
      AND mcd.date BETWEEN v_start_date AND v_end_date
  ),
  daily_values AS (
    SELECT
      days.date,
      CASE
        WHEN v_aggregation_type = 'sum' THEN SUM(raw_measurements.value)
        WHEN v_aggregation_type = 'median' THEN percentile_cont(0.5)
          WITHIN GROUP (ORDER BY raw_measurements.value)
          FILTER (WHERE raw_measurements.value IS NOT NULL)::numeric
        WHEN v_aggregation_type IN ('min', 'minimum') THEN
          MIN(raw_measurements.value)
        WHEN v_aggregation_type IN ('max', 'maximum') THEN
          MAX(raw_measurements.value)
        WHEN v_aggregation_type = '(min+max)/2' THEN
          (MIN(raw_measurements.value) + MAX(raw_measurements.value)) / 2
        ELSE AVG(raw_measurements.value)
      END AS value,
      COALESCE(BOOL_OR(raw_measurements.imputed), FALSE) AS imputed,
      COUNT(raw_measurements.date) AS measurement_count,
      existing_daily.date IS NOT NULL AS has_existing_daily
    FROM days
    LEFT JOIN raw_measurements
      ON raw_measurements.date = days.date
    LEFT JOIN existing_daily
      ON existing_daily.date = days.date
    GROUP BY days.date, existing_daily.date
  )
  INSERT INTO continuous.measurements_calculated_daily (
    timeseries_id,
    date,
    value,
    imputed
  )
  SELECT
    p_timeseries_id,
    daily_values.date,
    daily_values.value,
    daily_values.imputed
  FROM daily_values
  WHERE daily_values.measurement_count > 0
     OR daily_values.has_existing_daily
  ON CONFLICT (timeseries_id, date) DO UPDATE
  SET
    value = EXCLUDED.value,
    imputed = EXCLUDED.imputed,
    modified = CURRENT_TIMESTAMP,
    modified_by = CURRENT_USER
  WHERE measurements_calculated_daily.value IS DISTINCT FROM EXCLUDED.value
     OR measurements_calculated_daily.imputed IS DISTINCT FROM EXCLUDED.imputed;

  WITH affected_doys AS (
    SELECT DISTINCT continuous.normalized_day_of_year(d::date) AS doy
    FROM generate_series(v_start_date, v_end_date, interval '1 day') AS d
    WHERE continuous.normalized_day_of_year(d::date) IS NOT NULL
  ),
  base AS MATERIALIZED (
    SELECT
      mcd.timeseries_id,
      mcd.date,
      mcd.value,
      mcd.imputed,
      continuous.normalized_day_of_year(mcd.date) AS doy
    FROM continuous.measurements_calculated_daily mcd
    JOIN affected_doys ad
      ON ad.doy = continuous.normalized_day_of_year(mcd.date)
    WHERE mcd.timeseries_id = p_timeseries_id
      AND continuous.normalized_day_of_year(mcd.date) IS NOT NULL
  ),
  target AS (
    SELECT *
    FROM base
    WHERE date >= v_start_date
  ),
  hist AS (
    SELECT
      target.timeseries_id,
      target.date,
      target.value,
      target.imputed,
      COUNT(past.value) AS hist_count,
      MIN(past.value) AS hist_min,
      MAX(past.value) AS hist_max,
      AVG(past.value) AS hist_mean,
      percentile_cont(0.90) WITHIN GROUP (ORDER BY past.value)
        FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q90,
      percentile_cont(0.75) WITHIN GROUP (ORDER BY past.value)
        FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q75,
      percentile_cont(0.50) WITHIN GROUP (ORDER BY past.value)
        FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q50,
      percentile_cont(0.25) WITHIN GROUP (ORDER BY past.value)
        FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q25,
      percentile_cont(0.10) WITHIN GROUP (ORDER BY past.value)
        FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q10
    FROM target
    LEFT JOIN base past
      ON past.timeseries_id = target.timeseries_id
     AND past.doy = target.doy
     AND past.date < target.date
     AND past.value IS NOT NULL
    GROUP BY target.timeseries_id, target.date, target.value, target.imputed
  ),
  computed AS (
    SELECT
      hist.timeseries_id,
      hist.date,
      CASE
        WHEN hist.hist_count > 1 AND hist.value IS NOT NULL THEN
          ((hist.value - hist.hist_min) / NULLIF(hist.hist_max - hist.hist_min, 0)) * 100
        ELSE NULL::numeric
      END AS percent_historic_range,
      CASE
        WHEN hist.hist_count = 0 AND hist.value IS NOT NULL THEN hist.value
        WHEN hist.hist_count > 0 THEN hist.hist_max
        ELSE NULL::numeric
      END AS max,
      CASE
        WHEN hist.hist_count = 0 AND hist.value IS NOT NULL THEN hist.value
        WHEN hist.hist_count > 0 THEN hist.hist_min
        ELSE NULL::numeric
      END AS min,
      CASE WHEN hist.hist_count > 0 THEN hist.hist_q90 ELSE NULL::numeric END AS q90,
      CASE WHEN hist.hist_count > 0 THEN hist.hist_q75 ELSE NULL::numeric END AS q75,
      CASE WHEN hist.hist_count > 0 THEN hist.hist_q50 ELSE NULL::numeric END AS q50,
      CASE WHEN hist.hist_count > 0 THEN hist.hist_q25 ELSE NULL::numeric END AS q25,
      CASE WHEN hist.hist_count > 0 THEN hist.hist_q10 ELSE NULL::numeric END AS q10,
      CASE WHEN hist.hist_count > 0 THEN hist.hist_mean ELSE NULL::numeric END AS mean,
      CASE
        WHEN hist.hist_count = 0 AND hist.value IS NOT NULL THEN 1
        WHEN hist.hist_count > 0 THEN hist.hist_count + CASE WHEN hist.value IS NULL THEN 0 ELSE 1 END
        ELSE NULL::integer
      END AS doy_count
    FROM hist
  )
  UPDATE continuous.measurements_calculated_daily mcd
  SET
    percent_historic_range = computed.percent_historic_range,
    max = computed.max,
    min = computed.min,
    q90 = computed.q90,
    q75 = computed.q75,
    q50 = computed.q50,
    q25 = computed.q25,
    q10 = computed.q10,
    mean = computed.mean,
    doy_count = computed.doy_count,
    modified = CURRENT_TIMESTAMP,
    modified_by = CURRENT_USER
  FROM computed
  WHERE mcd.timeseries_id = computed.timeseries_id
    AND mcd.date = computed.date
    AND (
      mcd.percent_historic_range IS DISTINCT FROM computed.percent_historic_range OR
      mcd.max IS DISTINCT FROM computed.max OR
      mcd.min IS DISTINCT FROM computed.min OR
      mcd.q90 IS DISTINCT FROM computed.q90 OR
      mcd.q75 IS DISTINCT FROM computed.q75 OR
      mcd.q50 IS DISTINCT FROM computed.q50 OR
      mcd.q25 IS DISTINCT FROM computed.q25 OR
      mcd.q10 IS DISTINCT FROM computed.q10 OR
      mcd.mean IS DISTINCT FROM computed.mean OR
      mcd.doy_count IS DISTINCT FROM computed.doy_count
    );

  SELECT EXISTS (
    SELECT 1
    FROM generate_series(v_start_date, v_end_date, interval '1 day') AS d
    WHERE to_char(d::date, 'MM-DD') = '02-29'
       OR continuous.normalized_day_of_year(d::date) IN (59, 60)
  ) INTO v_recalc_feb29;

  IF v_recalc_feb29
    AND to_char((now() AT TIME ZONE 'UTC')::date, 'MM-DD') NOT IN ('02-29', '03-01', '03-02') THEN
    WITH feb AS (
      SELECT f.*
      FROM continuous.measurements_calculated_daily f
      WHERE f.timeseries_id = p_timeseries_id
        AND f.date >= v_start_date
        AND to_char(f.date, 'MM-DD') = '02-29'
    ),
    computed AS (
      SELECT
        feb.timeseries_id,
        feb.date,
        CASE WHEN b.percent_historic_range IS NOT NULL AND a.percent_historic_range IS NOT NULL THEN (b.percent_historic_range + a.percent_historic_range) / 2 ELSE NULL::numeric END AS percent_historic_range,
        CASE WHEN b.max IS NOT NULL AND a.max IS NOT NULL THEN (b.max + a.max) / 2 WHEN b.max IS NULL AND a.max IS NULL AND feb.value IS NOT NULL THEN feb.value ELSE NULL::numeric END AS max,
        CASE WHEN b.min IS NOT NULL AND a.min IS NOT NULL THEN (b.min + a.min) / 2 WHEN b.min IS NULL AND a.min IS NULL AND feb.value IS NOT NULL THEN feb.value ELSE NULL::numeric END AS min,
        CASE WHEN b.q90 IS NOT NULL AND a.q90 IS NOT NULL THEN (b.q90 + a.q90) / 2 ELSE NULL::numeric END AS q90,
        CASE WHEN b.q75 IS NOT NULL AND a.q75 IS NOT NULL THEN (b.q75 + a.q75) / 2 ELSE NULL::numeric END AS q75,
        CASE WHEN b.q50 IS NOT NULL AND a.q50 IS NOT NULL THEN (b.q50 + a.q50) / 2 ELSE NULL::numeric END AS q50,
        CASE WHEN b.q25 IS NOT NULL AND a.q25 IS NOT NULL THEN (b.q25 + a.q25) / 2 ELSE NULL::numeric END AS q25,
        CASE WHEN b.q10 IS NOT NULL AND a.q10 IS NOT NULL THEN (b.q10 + a.q10) / 2 ELSE NULL::numeric END AS q10,
        CASE WHEN b.mean IS NOT NULL AND a.mean IS NOT NULL THEN (b.mean + a.mean) / 2 ELSE NULL::numeric END AS mean,
        CASE WHEN b.doy_count IS NOT NULL AND a.doy_count IS NOT NULL THEN LEAST(b.doy_count, a.doy_count) WHEN b.doy_count IS NULL AND a.doy_count IS NULL AND feb.value IS NOT NULL THEN 1 ELSE NULL::integer END AS doy_count
      FROM feb
      LEFT JOIN continuous.measurements_calculated_daily b
        ON b.timeseries_id = feb.timeseries_id
       AND b.date = feb.date - 1
      LEFT JOIN continuous.measurements_calculated_daily a
        ON a.timeseries_id = feb.timeseries_id
       AND a.date = feb.date + 1
    )
    UPDATE continuous.measurements_calculated_daily mcd
    SET
      percent_historic_range = computed.percent_historic_range,
      max = computed.max,
      min = computed.min,
      q90 = computed.q90,
      q75 = computed.q75,
      q50 = computed.q50,
      q25 = computed.q25,
      q10 = computed.q10,
      mean = computed.mean,
      doy_count = computed.doy_count,
      modified = CURRENT_TIMESTAMP,
      modified_by = CURRENT_USER
    FROM computed
    WHERE mcd.timeseries_id = computed.timeseries_id
      AND mcd.date = computed.date
      AND (
        mcd.percent_historic_range IS DISTINCT FROM computed.percent_historic_range OR
        mcd.max IS DISTINCT FROM computed.max OR
        mcd.min IS DISTINCT FROM computed.min OR
        mcd.q90 IS DISTINCT FROM computed.q90 OR
        mcd.q75 IS DISTINCT FROM computed.q75 OR
        mcd.q50 IS DISTINCT FROM computed.q50 OR
        mcd.q25 IS DISTINCT FROM computed.q25 OR
        mcd.q10 IS DISTINCT FROM computed.q10 OR
        mcd.mean IS DISTINCT FROM computed.mean OR
        mcd.doy_count IS DISTINCT FROM computed.doy_count
      );
  END IF;

  UPDATE continuous.timeseries t
  SET last_daily_calculation = CURRENT_TIMESTAMP
  WHERE t.timeseries_id = p_timeseries_id;
END;
$function$;
  "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily(
  p_timeseries_ids integer[],
  p_changed_start date DEFAULT NULL,
  p_changed_end date DEFAULT NULL
)
RETURNS void
LANGUAGE plpgsql
AS $function$
DECLARE
  v_id integer;
BEGIN
  FOR v_id IN
    SELECT DISTINCT x.timeseries_id
    FROM unnest(p_timeseries_ids) AS x(timeseries_id)
    WHERE x.timeseries_id IS NOT NULL
  LOOP
    PERFORM continuous.refresh_calculated_daily(v_id, p_changed_start, p_changed_end);
  END LOOP;
END;
$function$;
  "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_from_ranges()
RETURNS void
LANGUAGE plpgsql
AS $function$
DECLARE
  v_request record;
BEGIN
  FOR v_request IN
    WITH RECURSIVE changed AS (
      SELECT *
      FROM pg_temp.calculated_daily_trigger_ranges
    ),
    impacted AS (
      SELECT timeseries_id, start_dt, end_dt
      FROM changed

      UNION ALL

      SELECT m.timeseries_id, impacted.start_dt, impacted.end_dt
      FROM impacted
      JOIN continuous.timeseries_compound_members m
        ON m.member_timeseries_id = impacted.timeseries_id
    )
    SELECT
      impacted.timeseries_id,
      MIN((impacted.start_dt + make_interval(hours => t.timezone_daily_calc))::date) AS start_date,
      MAX((impacted.end_dt + make_interval(hours => t.timezone_daily_calc))::date) AS end_date
    FROM impacted
    JOIN continuous.timeseries t
      ON t.timeseries_id = impacted.timeseries_id
    GROUP BY impacted.timeseries_id
  LOOP
    PERFORM continuous.refresh_calculated_daily(
      v_request.timeseries_id,
      v_request.start_date,
      v_request.end_date
    );
  END LOOP;

  RETURN;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_measurement_insert_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT timeseries_id, MIN(datetime), MAX(datetime)
  FROM new_rows
  GROUP BY timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_measurement_delete_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT timeseries_id, MIN(datetime), MAX(datetime)
  FROM old_rows
  GROUP BY timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_measurement_update_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT timeseries_id, MIN(datetime), MAX(datetime)
  FROM (
    SELECT o.timeseries_id, o.datetime
    FROM old_rows o
    JOIN new_rows n
      ON n.measurement_row_id = o.measurement_row_id
    WHERE o.timeseries_id IS DISTINCT FROM n.timeseries_id
       OR o.datetime IS DISTINCT FROM n.datetime
       OR o.value IS DISTINCT FROM n.value
       OR o.period IS DISTINCT FROM n.period
       OR o.imputed IS DISTINCT FROM n.imputed
    UNION ALL
    SELECT n.timeseries_id, n.datetime
    FROM old_rows o
    JOIN new_rows n
      ON n.measurement_row_id = o.measurement_row_id
    WHERE o.timeseries_id IS DISTINCT FROM n.timeseries_id
       OR o.datetime IS DISTINCT FROM n.datetime
       OR o.value IS DISTINCT FROM n.value
       OR o.period IS DISTINCT FROM n.period
       OR o.imputed IS DISTINCT FROM n.imputed
  ) x
  GROUP BY timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_correction_insert_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT timeseries_id, MIN(start_dt), MAX(end_dt)
  FROM new_rows
  GROUP BY timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_correction_delete_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT timeseries_id, MIN(start_dt), MAX(end_dt)
  FROM old_rows
  GROUP BY timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_correction_update_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT timeseries_id, MIN(start_dt), MAX(end_dt)
  FROM (
    SELECT o.timeseries_id, o.start_dt, o.end_dt
    FROM old_rows o
    JOIN new_rows n
      ON n.correction_id = o.correction_id
    WHERE o.timeseries_id IS DISTINCT FROM n.timeseries_id
       OR o.start_dt IS DISTINCT FROM n.start_dt
       OR o.end_dt IS DISTINCT FROM n.end_dt
       OR o.correction_type IS DISTINCT FROM n.correction_type
       OR o.value1 IS DISTINCT FROM n.value1
       OR o.value2 IS DISTINCT FROM n.value2
       OR o.timestep_window IS DISTINCT FROM n.timestep_window
       OR o.equation IS DISTINCT FROM n.equation
    UNION ALL
    SELECT n.timeseries_id, n.start_dt, n.end_dt
    FROM old_rows o
    JOIN new_rows n
      ON n.correction_id = o.correction_id
    WHERE o.timeseries_id IS DISTINCT FROM n.timeseries_id
       OR o.start_dt IS DISTINCT FROM n.start_dt
       OR o.end_dt IS DISTINCT FROM n.end_dt
       OR o.correction_type IS DISTINCT FROM n.correction_type
       OR o.value1 IS DISTINCT FROM n.value1
       OR o.value2 IS DISTINCT FROM n.value2
       OR o.timestep_window IS DISTINCT FROM n.timestep_window
       OR o.equation IS DISTINCT FROM n.equation
  ) x
  GROUP BY timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_grade_insert_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT nr.timeseries_id, MIN(nr.start_dt), MAX(nr.end_dt)
  FROM new_rows nr
  JOIN public.grade_types gt
    ON gt.grade_type_id = nr.grade_type_id
  WHERE gt.grade_type_code = 'N'
    AND nr.start_dt <> nr.end_dt
  GROUP BY nr.timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_grade_delete_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT orow.timeseries_id, MIN(orow.start_dt), MAX(orow.end_dt)
  FROM old_rows orow
  JOIN public.grade_types gt
    ON gt.grade_type_id = orow.grade_type_id
  WHERE gt.grade_type_code = 'N'
    AND orow.start_dt <> orow.end_dt
  GROUP BY orow.timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_grade_update_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF current_setting('continuous.skip_daily_refresh', true) = 'on' THEN
    RETURN NULL;
  END IF;

  CREATE TEMP TABLE calculated_daily_trigger_ranges (
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  ) ON COMMIT DROP;

  INSERT INTO calculated_daily_trigger_ranges
  SELECT x.timeseries_id, MIN(x.start_dt), MAX(x.end_dt)
  FROM (
    SELECT o.timeseries_id, o.start_dt, o.end_dt, o.grade_type_id
    FROM old_rows o
    JOIN new_rows n
      ON n.grade_id = o.grade_id
    WHERE o.timeseries_id IS DISTINCT FROM n.timeseries_id
       OR o.start_dt IS DISTINCT FROM n.start_dt
       OR o.end_dt IS DISTINCT FROM n.end_dt
       OR o.grade_type_id IS DISTINCT FROM n.grade_type_id
    UNION ALL
    SELECT n.timeseries_id, n.start_dt, n.end_dt, n.grade_type_id
    FROM old_rows o
    JOIN new_rows n
      ON n.grade_id = o.grade_id
    WHERE o.timeseries_id IS DISTINCT FROM n.timeseries_id
       OR o.start_dt IS DISTINCT FROM n.start_dt
       OR o.end_dt IS DISTINCT FROM n.end_dt
       OR o.grade_type_id IS DISTINCT FROM n.grade_type_id
  ) x
  JOIN public.grade_types gt
    ON gt.grade_type_id = x.grade_type_id
  WHERE gt.grade_type_code = 'N'
    AND x.start_dt <> x.end_dt
  GROUP BY x.timeseries_id;

  PERFORM continuous.refresh_calculated_daily_from_ranges();
  DROP TABLE calculated_daily_trigger_ranges;
  RETURN NULL;
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily_compound_definition_trg()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
DECLARE
  v_timeseries_ids integer[];
  v_id integer;
BEGIN
  IF TG_TABLE_NAME = 'timeseries_compounds' THEN
    SELECT ARRAY_AGG(DISTINCT x.timeseries_id)
    INTO v_timeseries_ids
    FROM continuous.downstream_timeseries_ids(
      ARRAY[COALESCE(NEW.timeseries_id, OLD.timeseries_id)]::integer[]
    ) x;
  ELSE
    SELECT ARRAY_AGG(DISTINCT x.timeseries_id)
    INTO v_timeseries_ids
    FROM continuous.downstream_timeseries_ids(
      ARRAY[
        COALESCE(NEW.timeseries_id, OLD.timeseries_id),
        COALESCE(NEW.member_timeseries_id, OLD.member_timeseries_id)
      ]::integer[]
    ) x;
  END IF;

  IF array_length(v_timeseries_ids, 1) IS NULL THEN
    RETURN COALESCE(NEW, OLD);
  END IF;

  FOREACH v_id IN ARRAY v_timeseries_ids LOOP
    IF EXISTS (
      SELECT 1
      FROM continuous.timeseries t
      WHERE t.timeseries_id = v_id
        AND t.timeseries_type = 'compound'
    ) THEN
      PERFORM continuous.refresh_calculated_daily(v_id, NULL::date, NULL::date);
    END IF;
  END LOOP;

  RETURN COALESCE(NEW, OLD);
END;
$function$;
    "
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_measurement_insert_tr ON continuous.measurements_continuous;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_measurement_insert_tr
AFTER INSERT ON continuous.measurements_continuous
REFERENCING NEW TABLE AS new_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_measurement_insert_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_measurement_delete_tr ON continuous.measurements_continuous;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_measurement_delete_tr
AFTER DELETE ON continuous.measurements_continuous
REFERENCING OLD TABLE AS old_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_measurement_delete_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_measurement_update_tr ON continuous.measurements_continuous;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_measurement_update_tr
AFTER UPDATE ON continuous.measurements_continuous
REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_measurement_update_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_correction_insert_tr ON continuous.corrections;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_correction_insert_tr
AFTER INSERT ON continuous.corrections
REFERENCING NEW TABLE AS new_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_correction_insert_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_correction_delete_tr ON continuous.corrections;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_correction_delete_tr
AFTER DELETE ON continuous.corrections
REFERENCING OLD TABLE AS old_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_correction_delete_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_correction_update_tr ON continuous.corrections;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_correction_update_tr
AFTER UPDATE ON continuous.corrections
REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_correction_update_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_grade_insert_tr ON continuous.grades;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_grade_insert_tr
AFTER INSERT ON continuous.grades
REFERENCING NEW TABLE AS new_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_grade_insert_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_grade_delete_tr ON continuous.grades;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_grade_delete_tr
AFTER DELETE ON continuous.grades
REFERENCING OLD TABLE AS old_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_grade_delete_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_grade_update_tr ON continuous.grades;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_grade_update_tr
AFTER UPDATE ON continuous.grades
REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
FOR EACH STATEMENT
EXECUTE FUNCTION continuous.refresh_calculated_daily_grade_update_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_compound_header_tr ON continuous.timeseries_compounds;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_compound_header_tr
AFTER INSERT OR UPDATE OR DELETE ON continuous.timeseries_compounds
FOR EACH ROW
EXECUTE FUNCTION continuous.refresh_calculated_daily_compound_definition_trg();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS refresh_calculated_daily_compound_member_tr ON continuous.timeseries_compound_members;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_calculated_daily_compound_member_tr
AFTER INSERT OR UPDATE OR DELETE ON continuous.timeseries_compound_members
FOR EACH ROW
EXECUTE FUNCTION continuous.refresh_calculated_daily_compound_definition_trg();"
    )

    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.normalized_day_of_year(date) TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.local_noon_to_utc(date, integer) TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.downstream_timeseries_ids(integer[]) TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.refresh_calculated_daily(integer, date, date) TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.refresh_calculated_daily(integer[], date, date) TO PUBLIC;"
    )

    message(
      "Updating daily-measurement audit behavior and database comments..."
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.log_measurements_calculated_daily_change()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := current_user::TEXT;
         v_actor_user TEXT := COALESCE(
           NULLIF(current_setting('aquacache.audit_user', true), ''),
           session_user::TEXT
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_measurement_row_id BIGINT;
         v_timeseries_id INTEGER;
         v_measurement_date DATE;
         v_old_data JSONB;
         v_new_data JSONB;
         v_changed_fields JSONB;
       BEGIN
         IF TG_OP = 'DELETE' THEN
           v_measurement_row_id := OLD.measurement_row_id;
           v_timeseries_id := OLD.timeseries_id;
           v_measurement_date := OLD.date;
           INSERT INTO audit.measurements_calculated_daily_log (
             measurement_row_id,
             timeseries_id,
             measurement_date,
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
             v_measurement_row_id,
             v_timeseries_id,
             v_measurement_date,
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
         v_measurement_row_id := OLD.measurement_row_id;
         v_timeseries_id := OLD.timeseries_id;
         v_measurement_date := OLD.date;
         v_changed_fields := audit.jsonb_changed_fields(v_old_data, v_new_data);

         IF v_changed_fields = '{}'::jsonb THEN
           RETURN NEW;
         END IF;

         IF
           NEW.created IS NOT NULL AND
           NEW.modified IS NOT NULL AND
           NEW.created = NEW.modified
         THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.measurements_calculated_daily_log (
           measurement_row_id,
           timeseries_id,
           measurement_date,
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
           v_measurement_row_id,
           v_timeseries_id,
           v_measurement_date,
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
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.log_measurements_calculated_daily_change() OWNER TO postgres;"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.measurements_continuous IS
       'Stores raw observations and imputed point values for basic continuous timeseries only. Compound timeseries do not store rows here. Prefer continuous.measurements_continuous_corrected() for current corrected values and continuous.measurements_continuous_corrected_at() for point-in-time corrected values when reading measurement data.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_continuous.value IS
       'Raw measured or imputed value before AquaCache corrections are applied. Use continuous.measurements_continuous_corrected() or continuous.measurements_continuous_corrected_at() to retrieve correction-adjusted values.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_continuous.period IS
       'Duration represented by the measurement. This can differ within a timeseries, for example where a sensor changes from hourly to daily values.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_continuous.imputed IS
       'TRUE when the stored raw value was imputed rather than directly measured. Imputed flags are carried through corrected and calculated-daily outputs.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.measurements_calculated_daily IS
       'Stores database-maintained daily aggregate rows for continuous timeseries. Values and historic statistics are calculated from corrected data returned by continuous.measurements_continuous_corrected(); source measurement, correction, grade, and compound-definition changes update this table through refresh_calculated_daily_measurement_insert_tr, refresh_calculated_daily_measurement_update_tr, refresh_calculated_daily_measurement_delete_tr, refresh_calculated_daily_correction_insert_tr, refresh_calculated_daily_correction_update_tr, refresh_calculated_daily_correction_delete_tr, refresh_calculated_daily_grade_insert_tr, refresh_calculated_daily_grade_update_tr, refresh_calculated_daily_grade_delete_tr, refresh_calculated_daily_compound_header_tr, and refresh_calculated_daily_compound_member_tr.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.value IS
       'Daily aggregate value calculated in the database from corrected continuous measurements for the timeseries aggregation type and timezone_daily_calc setting.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.imputed IS
       'TRUE when at least one corrected source measurement used for the daily aggregate was imputed, or when an imported daily value was marked imputed before backfill into continuous measurements.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.percent_historic_range IS
       'Percent of the historical same-day-of-year range for the daily value, calculated from earlier corrected daily values for the same timeseries. February 29 values are interpolated from February 28 and March 1 when enough surrounding data exist.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.max IS
       'Historical maximum corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.min IS
       'Historical minimum corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q90 IS
       'Historical 90th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q75 IS
       'Historical 75th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q50 IS
       'Historical median corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q25 IS
       'Historical 25th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q10 IS
       'Historical 10th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.mean IS
       'Historical mean corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.doy_count IS
       'Number of non-null corrected daily values available for this same day-of-year statistic after including the current date where applicable.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.no_update IS
       'Manual protection flag used by maintenance workflows to identify daily rows that should not be overwritten by external synchronization code.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.measurements_calculated_daily_log IS
       'Audit log for user-visible updates and deletions to continuous.measurements_calculated_daily. Updates made in the same transaction that created a row are skipped when created and modified timestamps are equal because no pre-update row version was externally visible.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.log_measurements_calculated_daily_change() IS
       'Trigger function that writes daily measurement updates and deletes into audit.measurements_calculated_daily_log, while suppressing same-transaction post-insert updates where NEW.created equals NEW.modified.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries IS
       'Single metadata catalog for continuous timeseries, including basic series stored in continuous.measurements_continuous and compound series resolved from other timeseries at query time. The timeseries_type column distinguishes basic and compound records.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.start_datetime IS
       'Earliest available timestamp for the timeseries. Database triggers maintain this from raw measurement bounds for basic series and member metadata bounds for compound series.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.end_datetime IS
       'Latest available timestamp for the timeseries. Database triggers maintain this from raw measurement bounds for basic series and member metadata bounds for compound series.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.last_daily_calculation IS
       'Timestamp of the most recent database-side refresh of continuous.measurements_calculated_daily for this timeseries.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE information.internal_status IS
       'Stores status timestamps and other small operational markers written by AquaCache database maintenance and import workflows.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE spatial.vectors IS
       'Stores point, line, and polygon geometries used by AquaCache spatial workflows. Insert vector geometries with AquaCache::insertACVector(); retrieve them through database queries or package helpers appropriate to the workflow.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) IS
       'Preferred current-data access function for continuous measurements. Returns raw and corrected values for one basic or compound timeseries over a bounded datetime window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) IS
       'Preferred point-in-time access function for continuous measurements. Returns raw and corrected values for one basic or compound timeseries over a bounded datetime window using measurements, corrections, and compound definitions as they existed at the requested as-of timestamp.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.measurements_calculated_daily_as_of(TIMESTAMPTZ, INTEGER[], DATE, DATE) IS
       'Reconstructs stored calculated daily rows as they existed at a requested timestamp using the current table plus audit.measurements_calculated_daily_log. Daily rows are stored outputs of database refresh triggers, not values recomputed by this function.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.normalized_day_of_year(DATE) IS
       'Returns a comparable day-of-year index for historical daily statistics, excluding February 29 and shifting later leap-year days so leap and non-leap years align.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.local_noon_to_utc(DATE, INTEGER) IS
       'Converts a local date and timezone offset to the UTC timestamp representing local noon for daily-value backfill and alignment.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.downstream_timeseries_ids(INTEGER[]) IS
       'Returns the supplied timeseries_ids plus compound timeseries that directly or indirectly depend on them.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily(INTEGER, DATE, DATE) IS
       'Recalculates stored daily aggregate values and historical daily statistics for one timeseries from corrected measurement data over the supplied date range.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily(INTEGER[], DATE, DATE) IS
       'Recalculates stored daily aggregate values and historical daily statistics for each supplied timeseries_id.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_from_ranges() IS
       'Statement-trigger helper that expands changed timestamp ranges through compound dependencies and refreshes affected calculated-daily date ranges.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_measurement_insert_trg() IS
       'After-insert statement trigger function on continuous.measurements_continuous that refreshes affected calculated-daily rows.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_measurement_update_trg() IS
       'After-update statement trigger function on continuous.measurements_continuous that refreshes calculated-daily rows when raw value, period, imputed flag, datetime, or timeseries_id changes.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_measurement_delete_trg() IS
       'After-delete statement trigger function on continuous.measurements_continuous that refreshes affected calculated-daily rows.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_correction_insert_trg() IS
       'After-insert statement trigger function on continuous.corrections that refreshes calculated-daily rows affected by new corrections.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_correction_update_trg() IS
       'After-update statement trigger function on continuous.corrections that refreshes calculated-daily rows affected by changed correction definitions or ranges.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_correction_delete_trg() IS
       'After-delete statement trigger function on continuous.corrections that refreshes calculated-daily rows affected by removed corrections.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_grade_insert_trg() IS
       'After-insert statement trigger function on continuous.grades that refreshes calculated-daily rows affected by new no-use grade intervals.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_grade_update_trg() IS
       'After-update statement trigger function on continuous.grades that refreshes calculated-daily rows affected by changed no-use grade intervals.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_grade_delete_trg() IS
       'After-delete statement trigger function on continuous.grades that refreshes calculated-daily rows affected by removed no-use grade intervals.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily_compound_definition_trg() IS
       'After-change row trigger function on compound definition tables that refreshes calculated-daily rows for affected compound timeseries.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.initialize_compound_timeseries_bounds() IS
       'Trigger function that initializes compound timeseries start_datetime and end_datetime metadata when a compound header is inserted.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_direct_compound_timeseries_datetime_bounds(INTEGER[]) IS
       'Refreshes start_datetime and end_datetime metadata for compound timeseries that directly depend on the supplied member timeseries_ids.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_metadata_on_measurements_insert() IS
       'After-insert statement trigger function that refreshes basic timeseries datetime bounds and last_new_data from inserted continuous measurements.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_metadata_on_measurements_update() IS
       'After-update statement trigger function that refreshes basic timeseries datetime bounds when measurement timestamps or timeseries_ids change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_metadata_on_measurements_delete() IS
       'After-delete statement trigger function that refreshes basic timeseries datetime bounds after continuous measurements are removed.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_insert() IS
       'After-insert statement trigger function that refreshes basic timeseries datetime bounds from inserted calculated-daily rows.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_update() IS
       'After-update statement trigger function that refreshes basic timeseries datetime bounds when calculated-daily dates or timeseries_ids change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_metadata_on_daily_measurements_delete() IS
       'After-delete statement trigger function that refreshes basic timeseries datetime bounds after calculated-daily rows are removed.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_compound_metadata_on_timeseries_change() IS
       'Trigger function that refreshes direct compound metadata when member timeseries bounds or type change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_compound_metadata_on_header_change() IS
       'Trigger function that refreshes compound timeseries bounds after a compound header is inserted, updated, or deleted.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_compound_metadata_on_member_change() IS
       'Trigger function that refreshes compound timeseries bounds after member rows or member windows change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.reject_compound_measurements() IS
       'Prevents rows from being stored in continuous.measurements_continuous for compound timeseries, whose values are resolved at query time.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.validate_compound_header() IS
       'Ensures each continuous.timeseries_compounds row references a timeseries catalog row with timeseries_type = compound.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.validate_compound_member() IS
       'Validates compound member rows, including member alias format, compound parent type, self-dependency, and recursive cycle checks.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.prevent_compound_definition_key_change() IS
       'Prevents updates to immutable compound-definition keys so audit history and dependency relationships remain stable.';"
    )

    message(
      "Backfilling WSC HYDAT daily means from measurements_calculated_daily into measurements_continuous..."
    )

    hydat_backfill_ids <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT mcd.timeseries_id
       FROM continuous.measurements_calculated_daily mcd
       JOIN continuous.timeseries t
         ON t.timeseries_id = mcd.timeseries_id
       WHERE t.source_fx = 'downloadWSC'
         AND mcd.value IS NOT NULL
       ORDER BY mcd.timeseries_id"
    )$timeseries_id

    if (length(hydat_backfill_ids) > 0) {
      message(
        "Found ",
        length(hydat_backfill_ids),
        " WSC HYDAT timeseries with daily values to backfill."
      )
      DBI::dbExecute(con, "SET LOCAL continuous.skip_daily_refresh = 'on'")

      hydat_backfill_rows <- 0L
      for (tsid in hydat_backfill_ids) {
        hydat_backfill_rows <- hydat_backfill_rows + DBI::dbExecute(
          con,
          "WITH daily AS MATERIALIZED (
             SELECT
               mcd.timeseries_id,
               mcd.date,
               continuous.local_noon_to_utc(
                 mcd.date,
                 t.timezone_daily_calc
               ) AS datetime,
               mcd.value,
               mcd.imputed,
               t.timezone_daily_calc
             FROM continuous.measurements_calculated_daily mcd
             JOIN continuous.timeseries t
               ON t.timeseries_id = mcd.timeseries_id
             WHERE mcd.timeseries_id = $1
               AND t.source_fx = 'downloadWSC'
               AND mcd.value IS NOT NULL
           ),
           bounds AS (
             SELECT MIN(datetime) AS start_datetime,
                    MAX(datetime) AS end_datetime
             FROM daily
           ),
           existing_days AS MATERIALIZED (
             SELECT
               (mc.datetime + make_interval(hours => t.timezone_daily_calc))::date AS date,
               bool_or(
                 mc.period IS DISTINCT FROM interval '1 day'
                 OR mc.datetime IS DISTINCT FROM continuous.local_noon_to_utc(
                   (mc.datetime + make_interval(hours => t.timezone_daily_calc))::date,
                   t.timezone_daily_calc
                 )
               ) AS has_higher_frequency
             FROM continuous.measurements_continuous mc
             JOIN continuous.timeseries t
               ON t.timeseries_id = mc.timeseries_id
             CROSS JOIN bounds b
             WHERE mc.timeseries_id = $1
               AND mc.datetime >= b.start_datetime - interval '1 day'
               AND mc.datetime <= b.end_datetime + interval '1 day'
             GROUP BY
               (mc.datetime + make_interval(hours => t.timezone_daily_calc))::date
           )
           INSERT INTO continuous.measurements_continuous (
             timeseries_id,
             datetime,
             value,
             period,
             imputed
           )
           SELECT
             daily.timeseries_id,
             daily.datetime,
             daily.value,
             interval '1 day',
             daily.imputed
           FROM daily
           LEFT JOIN existing_days e
             ON e.date = daily.date
           WHERE COALESCE(e.has_higher_frequency, FALSE) = FALSE
           ON CONFLICT (timeseries_id, datetime) DO UPDATE
           SET
             value = EXCLUDED.value,
             period = EXCLUDED.period,
             imputed = EXCLUDED.imputed,
             modified = CURRENT_TIMESTAMP,
             modified_by = CURRENT_USER
           WHERE continuous.measurements_continuous.period = interval '1 day'
             AND (
               continuous.measurements_continuous.value IS DISTINCT FROM EXCLUDED.value
               OR continuous.measurements_continuous.period IS DISTINCT FROM EXCLUDED.period
               OR continuous.measurements_continuous.imputed IS DISTINCT FROM EXCLUDED.imputed
             )",
          params = list(tsid)
        )
      }
      message(
        "Backfilled or updated ",
        hydat_backfill_rows,
        " WSC HYDAT daily rows in measurements_continuous."
      )
    }

    message(
      "Dropping public.locations.geom_id and removing obsolete location point vectors..."
    )

    # Remove the 'locations' table link to the 'vectors' table. Drop the
    # location-side FK before deleting vector rows; otherwise the ON DELETE
    # CASCADE relationship deletes locations and their dependent records.
    geoms <- DBI::dbGetQuery(
      con,
      "SELECT geom_id FROM public.locations WHERE geom_id IS NOT NULL"
    )$geom_id

    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations DROP COLUMN geom_id CASCADE;"
    )

    if (length(geoms) > 0) {
      DBI::dbExecute(
        con,
        paste0(
          "DELETE FROM spatial.vectors v
           WHERE v.geom_id IN (",
          paste(geoms, collapse = ","),
          ")
             AND NOT EXISTS (
               SELECT 1
               FROM files.documents_spatial ds
               WHERE ds.geom_id = v.geom_id
             )"
        )
      )
    }

    # Rename the 'measurements_calculated_daily_corrected_at' function
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS continuous.measurements_calculated_daily_corrected_at(TIMESTAMPTZ, INTEGER[], DATE, DATE);"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_calculated_daily_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL,
         p_start_date DATE DEFAULT NULL,
         p_end_date DATE DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         date DATE,
         value NUMERIC,
         imputed BOOLEAN,
         percent_historic_range NUMERIC,
         max NUMERIC,
         min NUMERIC,
         q90 NUMERIC,
         q75 NUMERIC,
         q50 NUMERIC,
         q25 NUMERIC,
         q10 NUMERIC,
         mean NUMERIC,
         doy_count INTEGER,
         created TIMESTAMPTZ,
         modified TIMESTAMPTZ
       )
       LANGUAGE sql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
         WITH visible_timeseries AS (
           SELECT ts.timeseries_id
           FROM continuous.timeseries ts
           WHERE (p_timeseries_ids IS NULL OR ts.timeseries_id = ANY(p_timeseries_ids))
         )
         SELECT
           mcd.timeseries_id,
           mcd.date,
           mcd.value,
           mcd.imputed,
           mcd.percent_historic_range,
           mcd.max,
           mcd.min,
           mcd.q90,
           mcd.q75,
           mcd.q50,
           mcd.q25,
           mcd.q10,
           mcd.mean,
           mcd.doy_count,
           mcd.created,
           mcd.modified
         FROM audit.measurements_calculated_daily_as_of(
           p_as_of,
           p_timeseries_ids,
           p_start_date,
           p_end_date
         ) mcd
         JOIN visible_timeseries vt
           ON mcd.timeseries_id = vt.timeseries_id
         ORDER BY mcd.timeseries_id, mcd.date;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) IS
       'Point-in-time access function for stored calculated daily rows. It reconstructs rows from audit history and does not recompute daily values. Visibility is filtered through the caller''s current access to continuous.timeseries.';"
    )

    # Drop the measurements_calculated_daily_corrected view if it exists, since it is no longer needed with the new measurements_calculated_daily_at function.
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.measurements_calculated_daily_corrected;"
    )

    # Wrap things up and commit
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '41'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    DBI::dbExecute(con, "COMMIT;")
    message(
      "Patch 41 applied successfully. Compound timeseries can now be defined in continuous.timeseries_compounds and continuous.timeseries_compound_members, continuous.measurements_continuous_corrected() can be used to query corrected measurements for one basic or compound timeseries over a requested datetime window, and start_datetime/end_datetime plus realtime last_new_data metadata is now maintained automatically for both basic and compound continuous timeseries."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    stop(e)
  }
)
