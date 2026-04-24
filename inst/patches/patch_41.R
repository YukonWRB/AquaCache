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
         to_regprocedure('continuous.apply_corrections(integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections"
    )

    if (
      !isTRUE(check$has_timeseries[[1]]) ||
        !isTRUE(check$has_measurements_continuous[[1]]) ||
        !isTRUE(check$has_measurements_continuous_corrected[[1]]) ||
        !isTRUE(check$has_corrections[[1]]) ||
        !isTRUE(check$has_apply_corrections[[1]])
    ) {
      stop(
        "This patch requires continuous.timeseries, continuous.measurements_continuous, continuous.measurements_continuous_corrected, continuous.corrections, and continuous.apply_corrections() to already exist."
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

    message("Creating compound resolver functions...")

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
             JOIN LATERAL continuous.measurements_continuous_corrected_ts_internal(
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
                 FROM continuous.measurements_continuous_corrected_ts_internal(
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
                   FROM continuous.measurements_continuous_corrected_ts_internal(
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
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_ts_internal(
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
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_ts_internal(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Internal helper used by the compound resolver so each recursive step only resolves one timeseries_id and one datetime window.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_ts(
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
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_ts(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) IS
       'Returns corrected measurements for one basic or compound timeseries over a requested datetime window.'"
    )

    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.resolve_compound_timeseries_raw_window(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.measurements_continuous_corrected_ts_internal(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.measurements_continuous_corrected_ts(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE) TO PUBLIC"
    )

    grant_function_privileges(
      "continuous.resolve_compound_timeseries_raw_window(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[])",
      "EXECUTE",
      write_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected_ts_internal(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[])",
      "EXECUTE",
      write_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected_ts(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE)",
      "EXECUTE",
      write_roles
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
      "Leaving view continuous.measurements_continuous_corrected unchanged for basic series for backward compatibility."
    )

    # Remove the 'locations' table link to the 'vectors' table.
    geoms <- DBI::dbGetQuery(
      con,
      "SELECT geom_id FROM public.locations WHERE geom_id IS NOT NULL"
    )$geom_id
    if (length(geoms) > 0) {
      DBI::dbExecute(
        con,
        paste0(
          "DELETE FROM spatial.vectors WHERE geom_id IN (",
          paste(geoms, collapse = ","),
          ")"
        )
      )
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations DROP COLUMN geom_id CASCADE;"
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
    dbTransCommit(con)
    message(
      "Patch 41 applied successfully. Compound timeseries can now be defined in continuous.timeseries_compounds and continuous.timeseries_compound_members, continuous.measurements_continuous_corrected_ts() can be used to query corrected measurements for one basic or compound timeseries over a requested datetime window, and start_datetime/end_datetime plus realtime last_new_data metadata is now maintained automatically for both basic and compound continuous timeseries."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(dbTransRollback(con), silent = TRUE)
    stop(e)
  }
)
