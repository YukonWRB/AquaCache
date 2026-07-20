# Patch 53: compound timeseries alignment tolerance
#
# Adds member-level alignment controls for compound continuous timeseries. Exact
# timestamp joins remain the default. When alignment_tolerance is set on a
# non-anchor expression member, compound resolution matches the nearest member
# point within that tolerance. reuse_member_values controls whether a single
# member point may be matched to more than one anchor timestamp.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 53: adding compound timeseries alignment tolerance. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

message("Starting transaction...")
active <- dbTransBegin(con)

patch_package_version <- tryCatch(
  as.character(packageVersion("AquaCache")),
  error = function(e) {
    if (file.exists("DESCRIPTION")) {
      desc <- read.dcf("DESCRIPTION")
      if ("Version" %in% colnames(desc)) {
        return(desc[1, "Version"])
      }
    }
    NA_character_
  }
)

tryCatch(
  {
    required_relations <- DBI::dbGetQuery(
      con,
      "SELECT missing_relation
       FROM (
         VALUES
           ('audit.general_log'),
           ('continuous.measurements_continuous'),
           ('continuous.timeseries'),
           ('continuous.timeseries_compounds'),
           ('continuous.timeseries_compound_members'),
           ('information.version_info')
       ) required(missing_relation)
       WHERE to_regclass(missing_relation) IS NULL"
    )

    if (nrow(required_relations) > 0) {
      stop(
        "This patch requires the following relations to already exist: ",
        paste(required_relations$missing_relation, collapse = ", ")
      )
    }

    required_functions <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure('continuous.measurements_continuous_corrected_internal(integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_current_internal,
         to_regprocedure('continuous.measurements_continuous_corrected_internal_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_at_internal,
         to_regprocedure('continuous.resolve_compound_timeseries_raw_window(integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_current_resolver,
         to_regprocedure('continuous.resolve_compound_timeseries_raw_window_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_at_resolver,
         to_regprocedure('continuous.apply_corrections(integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections,
         to_regprocedure('audit.timeseries_compound_members_as_of(timestamp with time zone, integer[])') IS NOT NULL AS has_members_as_of"
    )

    if (
      !isTRUE(required_functions$has_current_internal[[1]]) ||
        !isTRUE(required_functions$has_at_internal[[1]]) ||
        !isTRUE(required_functions$has_current_resolver[[1]]) ||
        !isTRUE(required_functions$has_at_resolver[[1]]) ||
        !isTRUE(required_functions$has_apply_corrections[[1]]) ||
        !isTRUE(required_functions$has_members_as_of[[1]])
    ) {
      stop(
        "This patch requires compound resolution and current correction functions from earlier patches to already exist."
      )
    }

    q_role <- function(role_name) {
      if (toupper(role_name) == "PUBLIC") {
        return("PUBLIC")
      }
      as.character(DBI::dbQuoteIdentifier(con, role_name))
    }

    existing_roles <- function() {
      DBI::dbGetQuery(con, "SELECT rolname FROM pg_catalog.pg_roles")$rolname
    }

    get_function_execute_grants <- function(function_signature) {
      DBI::dbGetQuery(
        con,
        "SELECT
           CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE grantee_role.rolname END AS grantee,
           acl.is_grantable
         FROM pg_catalog.pg_proc p
         CROSS JOIN LATERAL pg_catalog.aclexplode(
           COALESCE(p.proacl, pg_catalog.acldefault('f', p.proowner))
         ) acl
         LEFT JOIN pg_catalog.pg_roles grantee_role
           ON grantee_role.oid = acl.grantee
         WHERE p.oid = $1::regprocedure
           AND acl.privilege_type = 'EXECUTE'
         ORDER BY grantee",
        params = list(function_signature)
      )
    }

    apply_function_execute_grants <- function(function_signature, grants) {
      if (!nrow(grants)) {
        return(invisible(FALSE))
      }

      roles <- existing_roles()

      for (i in seq_len(nrow(grants))) {
        grantee <- grants$grantee[i]
        if (toupper(grantee) != "PUBLIC" && !grantee %in% roles) {
          next
        }

        DBI::dbExecute(
          con,
          sprintf(
            "GRANT EXECUTE ON FUNCTION %s TO %s%s",
            function_signature,
            q_role(grantee),
            if (isTRUE(grants$is_grantable[i])) " WITH GRANT OPTION" else ""
          )
        )
      }

      invisible(TRUE)
    }

    members_as_of_signature <- "audit.timeseries_compound_members_as_of(timestamp with time zone, integer[])"
    current_resolver_signature <- "continuous.resolve_compound_timeseries_raw_window(integer, timestamp with time zone, timestamp with time zone, integer[])"
    at_resolver_signature <- "continuous.resolve_compound_timeseries_raw_window_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, integer[])"

    members_as_of_grants <- get_function_execute_grants(members_as_of_signature)
    current_resolver_grants <- get_function_execute_grants(current_resolver_signature)
    at_resolver_grants <- get_function_execute_grants(at_resolver_signature)

    message("Adding compound member alignment columns...")
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_compound_members
       ADD COLUMN IF NOT EXISTS alignment_tolerance INTERVAL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_compound_members
       ADD COLUMN IF NOT EXISTS reuse_member_values BOOLEAN NOT NULL DEFAULT FALSE"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_compound_members
       ALTER COLUMN reuse_member_values SET DEFAULT FALSE"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM continuous.timeseries_compound_members
           WHERE reuse_member_values IS NULL
         ) THEN
           ALTER TABLE continuous.timeseries_compound_members DISABLE TRIGGER USER;

           UPDATE continuous.timeseries_compound_members
           SET reuse_member_values = FALSE
           WHERE reuse_member_values IS NULL;

           ALTER TABLE continuous.timeseries_compound_members ENABLE TRIGGER USER;
         END IF;
       END
       $$"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_compound_members
       ALTER COLUMN reuse_member_values SET NOT NULL"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM pg_catalog.pg_constraint
           WHERE conrelid = 'continuous.timeseries_compound_members'::regclass
             AND conname = 'timeseries_compound_member_alignment_tolerance_ck'
         ) THEN
           ALTER TABLE continuous.timeseries_compound_members
             ADD CONSTRAINT timeseries_compound_member_alignment_tolerance_ck
             CHECK (
               alignment_tolerance IS NULL OR
               alignment_tolerance >= interval '0 seconds'
             );
         END IF;
       END
       $$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_compound_members.alignment_tolerance IS
       'Optional maximum absolute time difference for matching this member to the already-aligned anchor timestamps in an expression-based compound timeseries. NULL requires exact timestamp alignment.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_compound_members.reuse_member_values IS
       'When alignment_tolerance is set, TRUE allows the same member measurement to align with multiple anchor timestamps. FALSE keeps only mutual nearest matches so a member point is used at most once per compound resolution.'"
    )

    message("Recreating historical compound member snapshot function...")
    DBI::dbExecute(con, paste0("DROP FUNCTION ", members_as_of_signature))
    DBI::dbExecute(
      con,
      "CREATE FUNCTION audit.timeseries_compound_members_as_of(
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
         alignment_tolerance INTERVAL,
         reuse_member_values BOOLEAN,
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
           r.alignment_tolerance,
           COALESCE(r.reuse_member_values, FALSE) AS reuse_member_values,
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
           alignment_tolerance INTERVAL,
           reuse_member_values BOOLEAN,
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
       'Reconstructs compound timeseries member definitions, including alignment controls, as they existed at a requested timestamp using continuous.timeseries_compound_members plus audit.general_log.'"
    )

    message("Recreating current compound resolver...")
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
         v_aligned_sql TEXT := '';
         v_output_columns TEXT := '';
         v_period_expr TEXT := 'NULL::interval';
         v_imputed_expr TEXT := 'FALSE';
         v_sql TEXT;
         v_member RECORD;
         v_window_from TIMESTAMPTZ;
         v_window_to TIMESTAMPTZ;
         v_query_from TIMESTAMPTZ;
         v_query_to TIMESTAMPTZ;
         v_source_sql TEXT;
         v_member_direct_basic BOOLEAN;
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

         v_window_from := p_from;
         v_window_to := p_to;

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
                 WHEN v_window_from IS NULL THEN m.use_from
                 WHEN m.use_from IS NULL THEN v_window_from
                 ELSE GREATEST(v_window_from, m.use_from)
               END,
               CASE
                 WHEN v_window_to IS NULL THEN m.use_to
                 WHEN m.use_to IS NULL THEN v_window_to
                 ELSE LEAST(v_window_to, m.use_to)
               END,
               p_path || p_timeseries_id
             ) src
               ON TRUE
             WHERE m.timeseries_id = p_timeseries_id
               AND (m.use_from IS NULL OR v_window_to IS NULL OR src.datetime >= m.use_from)
               AND (m.use_to IS NULL OR v_window_from IS NULL OR src.datetime < m.use_to)
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
             member_timeseries_type,
             use_from,
             use_to,
             alignment_tolerance,
             reuse_member_values
           FROM (
             SELECT
               m.member_alias,
               m.member_timeseries_id,
               mt.timeseries_type AS member_timeseries_type,
               m.use_from,
               m.use_to,
               m.alignment_tolerance,
               m.reuse_member_values,
               m.member_priority
             FROM continuous.timeseries_compound_members m
             JOIN continuous.timeseries mt
               ON mt.timeseries_id = m.member_timeseries_id
             WHERE m.timeseries_id = p_timeseries_id
           ) member_rows
           ORDER BY member_priority, member_alias
         LOOP
           IF v_first OR v_member.alignment_tolerance IS NULL THEN
             v_query_from := CASE
               WHEN v_window_from IS NULL THEN v_member.use_from
               WHEN v_member.use_from IS NULL THEN v_window_from
               ELSE GREATEST(v_window_from, v_member.use_from)
             END;
             v_query_to := CASE
               WHEN v_window_to IS NULL THEN v_member.use_to
               WHEN v_member.use_to IS NULL THEN v_window_to
               ELSE LEAST(v_window_to, v_member.use_to)
             END;
           ELSE
             v_query_from := CASE
               WHEN v_window_from IS NULL THEN v_member.use_from
               WHEN v_member.use_from IS NULL THEN v_window_from - v_member.alignment_tolerance
               ELSE GREATEST(
                 v_window_from - v_member.alignment_tolerance,
                 v_member.use_from
               )
             END;
             v_query_to := CASE
               WHEN v_window_to IS NULL THEN v_member.use_to
               WHEN v_member.use_to IS NULL THEN v_window_to + v_member.alignment_tolerance
               ELSE LEAST(
                 v_window_to + v_member.alignment_tolerance,
                 v_member.use_to
               )
             END;
           END IF;

           SELECT
             v_member.member_timeseries_type = 'basic' AND NOT EXISTS (
               SELECT 1
               FROM continuous.corrections c
               WHERE c.timeseries_id = v_member.member_timeseries_id
                 AND (v_query_to IS NULL OR c.start_dt <= v_query_to)
                 AND (v_query_from IS NULL OR c.end_dt >= v_query_from)
             )
           INTO v_member_direct_basic;

           IF v_member_direct_basic THEN
             v_source_sql := format(
               '(SELECT
                   mc.datetime,
                   mc.value AS value_corrected,
                   mc.period,
                   mc.imputed
                 FROM continuous.measurements_continuous mc
                 WHERE mc.timeseries_id = %1$s
                   AND (%2$L::timestamptz IS NULL OR mc.datetime >= %2$L::timestamptz)
                   AND (%3$L::timestamptz IS NULL OR mc.datetime <= %3$L::timestamptz)
               )',
               v_member.member_timeseries_id,
               v_query_from,
               v_query_to
             );
           ELSE
             v_source_sql := format(
               '(SELECT *
                 FROM continuous.measurements_continuous_corrected_internal(
                   %1$s,
                   %2$L::timestamptz,
                   %3$L::timestamptz,
                   %4$L::integer[]
                 ) src)',
               v_member.member_timeseries_id,
               v_query_from,
               v_query_to,
               p_path || p_timeseries_id
             );
           END IF;

           IF v_first THEN
             v_aligned_sql := format(
               'SELECT
                  %1$I.datetime,
                  %1$I.value_corrected AS %1$I,
                  %1$I.period AS %2$I,
                  %1$I.imputed AS %3$I
                FROM %4$s AS %1$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_source_sql
             );
             v_output_columns := format(
               '%1$I, %2$I, %3$I, %4$I',
               'datetime',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_period_expr := format(
               'aligned.%I',
               v_member.member_alias || '__period'
             );
             v_imputed_expr := format(
               'COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
             );
             v_first := FALSE;
           ELSIF v_member.alignment_tolerance IS NULL THEN
             v_aligned_sql := format(
               'SELECT
                  aligned.*,
                  %1$I.value_corrected AS %1$I,
                  %1$I.period AS %2$I,
                  %1$I.imputed AS %3$I
                FROM (%4$s) aligned
                JOIN %5$s AS %1$I
                  USING (datetime)',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_aligned_sql,
               v_source_sql
             );
             v_output_columns := v_output_columns || format(
               ', %1$I, %2$I, %3$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I, FALSE)',
             v_member.member_alias || '__imputed'
           );
          ELSIF v_member.reuse_member_values AND v_member_direct_basic THEN
            v_aligned_sql := format(
              'SELECT
                 aligned.*,
                 %1$I.value_corrected AS %1$I,
                 %1$I.period AS %2$I,
                 %1$I.imputed AS %3$I
               FROM (%4$s) aligned
               JOIN LATERAL (
                 SELECT
                   mc.datetime,
                   mc.value AS value_corrected,
                   mc.period,
                   mc.imputed
                 FROM continuous.measurements_continuous mc
                 WHERE mc.timeseries_id = %5$s
                   AND (%6$L::timestamptz IS NULL OR mc.datetime >= %6$L::timestamptz)
                   AND (%7$L::timestamptz IS NULL OR mc.datetime <= %7$L::timestamptz)
                   AND mc.datetime >= aligned.datetime - %8$L::interval
                   AND mc.datetime <= aligned.datetime + %8$L::interval
                 ORDER BY
                   abs(extract(epoch FROM (mc.datetime - aligned.datetime))),
                   mc.datetime
                 LIMIT 1
               ) AS %1$I
                 ON TRUE',
              v_member.member_alias,
              v_member.member_alias || '__period',
              v_member.member_alias || '__imputed',
              v_aligned_sql,
              v_member.member_timeseries_id,
              v_query_from,
              v_query_to,
              v_member.alignment_tolerance
            );
            v_output_columns := v_output_columns || format(
              ', %1$I, %2$I, %3$I',
              v_member.member_alias,
              v_member.member_alias || '__period',
              v_member.member_alias || '__imputed'
            );
            v_imputed_expr := v_imputed_expr || format(
              ' OR COALESCE(aligned.%I, FALSE)',
              v_member.member_alias || '__imputed'
            );
          ELSIF v_member.reuse_member_values THEN
             v_aligned_sql := format(
               'WITH aligned_prev AS (
                  %4$s
                ),
                member_source AS (
                  SELECT src.*
                  FROM %5$s AS src
                ),
                candidates AS (
                  SELECT
                    aligned_prev.*,
                    member_source.value_corrected AS %1$I,
                    member_source.period AS %2$I,
                    member_source.imputed AS %3$I,
                    member_source.datetime AS %7$I,
                    row_number() OVER (
                      PARTITION BY aligned_prev.datetime
                      ORDER BY
                        abs(extract(epoch FROM (member_source.datetime - aligned_prev.datetime))),
                        member_source.datetime
                    ) AS anchor_rank
                  FROM aligned_prev
                  JOIN member_source
                    ON member_source.datetime >= aligned_prev.datetime - %8$L::interval
                   AND member_source.datetime <= aligned_prev.datetime + %8$L::interval
                )
                SELECT %6$s
                FROM candidates
                WHERE anchor_rank = 1',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_aligned_sql,
               v_source_sql,
               v_output_columns || format(
                 ', %1$I, %2$I, %3$I',
                 v_member.member_alias,
                 v_member.member_alias || '__period',
                 v_member.member_alias || '__imputed'
               ),
               v_member.member_alias || '__matched_datetime',
               v_member.alignment_tolerance
             );
             v_output_columns := v_output_columns || format(
               ', %1$I, %2$I, %3$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
             );
           ELSE
             v_aligned_sql := format(
               'WITH aligned_prev AS (
                  %4$s
                ),
                member_source AS (
                  SELECT src.*
                  FROM %5$s AS src
                ),
                candidates AS (
                  SELECT
                    aligned_prev.*,
                    member_source.value_corrected AS %1$I,
                    member_source.period AS %2$I,
                    member_source.imputed AS %3$I,
                    row_number() OVER (
                      PARTITION BY aligned_prev.datetime
                      ORDER BY
                        abs(extract(epoch FROM (member_source.datetime - aligned_prev.datetime))),
                        member_source.datetime
                    ) AS anchor_rank,
                    row_number() OVER (
                      PARTITION BY member_source.datetime
                      ORDER BY
                        abs(extract(epoch FROM (member_source.datetime - aligned_prev.datetime))),
                        aligned_prev.datetime
                    ) AS member_rank
                  FROM aligned_prev
                  JOIN member_source
                    ON member_source.datetime >= aligned_prev.datetime - %7$L::interval
                   AND member_source.datetime <= aligned_prev.datetime + %7$L::interval
                )
                SELECT %6$s
                FROM candidates
                WHERE anchor_rank = 1
                  AND member_rank = 1',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_aligned_sql,
               v_source_sql,
               v_output_columns || format(
                 ', %1$I, %2$I, %3$I',
                 v_member.member_alias,
                 v_member.member_alias || '__period',
                 v_member.member_alias || '__imputed'
               ),
               v_member.alignment_tolerance
             );
             v_output_columns := v_output_columns || format(
               ', %1$I, %2$I, %3$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
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
            FROM (%4$s) aligned',
           v_expression,
           v_period_expr,
           v_imputed_expr,
           v_aligned_sql
         );

         RETURN QUERY EXECUTE v_sql;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.resolve_compound_timeseries_raw_window(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Resolves one compound timeseries to raw derived values for a requested datetime window. Expression members require exact timestamp alignment unless a non-anchor member defines alignment_tolerance, in which case the nearest point within tolerance is used according to reuse_member_values.'"
    )

    message("Recreating point-in-time compound resolver...")
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
         v_aligned_sql TEXT := '';
         v_output_columns TEXT := '';
         v_period_expr TEXT := 'NULL::interval';
         v_imputed_expr TEXT := 'FALSE';
         v_sql TEXT;
         v_member RECORD;
         v_window_from TIMESTAMPTZ;
         v_window_to TIMESTAMPTZ;
         v_query_from TIMESTAMPTZ;
         v_query_to TIMESTAMPTZ;
         v_source_sql TEXT;
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

         v_window_from := p_from;
         v_window_to := p_to;

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
                 WHEN v_window_from IS NULL THEN m.use_from
                 WHEN m.use_from IS NULL THEN v_window_from
                 ELSE GREATEST(v_window_from, m.use_from)
               END,
               CASE
                 WHEN v_window_to IS NULL THEN m.use_to
                 WHEN m.use_to IS NULL THEN v_window_to
                 ELSE LEAST(v_window_to, m.use_to)
               END,
               p_path || p_timeseries_id
             ) src
               ON TRUE
             WHERE m.timeseries_id = p_timeseries_id
               AND (m.use_from IS NULL OR v_window_to IS NULL OR src.datetime >= m.use_from)
               AND (m.use_to IS NULL OR v_window_from IS NULL OR src.datetime < m.use_to)
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
             use_to,
             alignment_tolerance,
             reuse_member_values
           FROM audit.timeseries_compound_members_as_of(
             p_as_of,
             ARRAY[p_timeseries_id]
           )
           WHERE timeseries_id = p_timeseries_id
           ORDER BY member_priority, member_alias
         LOOP
           IF v_first OR v_member.alignment_tolerance IS NULL THEN
             v_query_from := CASE
               WHEN v_window_from IS NULL THEN v_member.use_from
               WHEN v_member.use_from IS NULL THEN v_window_from
               ELSE GREATEST(v_window_from, v_member.use_from)
             END;
             v_query_to := CASE
               WHEN v_window_to IS NULL THEN v_member.use_to
               WHEN v_member.use_to IS NULL THEN v_window_to
               ELSE LEAST(v_window_to, v_member.use_to)
             END;
           ELSE
             v_query_from := CASE
               WHEN v_window_from IS NULL THEN v_member.use_from
               WHEN v_member.use_from IS NULL THEN v_window_from - v_member.alignment_tolerance
               ELSE GREATEST(
                 v_window_from - v_member.alignment_tolerance,
                 v_member.use_from
               )
             END;
             v_query_to := CASE
               WHEN v_window_to IS NULL THEN v_member.use_to
               WHEN v_member.use_to IS NULL THEN v_window_to + v_member.alignment_tolerance
               ELSE LEAST(
                 v_window_to + v_member.alignment_tolerance,
                 v_member.use_to
               )
             END;
           END IF;

           v_source_sql := format(
             '(SELECT *
               FROM continuous.measurements_continuous_corrected_internal_at(
                 %1$L::timestamptz,
                 %2$s,
                 %3$L::timestamptz,
                 %4$L::timestamptz,
                 %5$L::integer[]
               ) src)',
             p_as_of,
             v_member.member_timeseries_id,
             v_query_from,
             v_query_to,
             p_path || p_timeseries_id
           );

           IF v_first THEN
             v_aligned_sql := format(
               'SELECT
                  %1$I.datetime,
                  %1$I.value_corrected AS %1$I,
                  %1$I.period AS %2$I,
                  %1$I.imputed AS %3$I
                FROM %4$s AS %1$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_source_sql
             );
             v_output_columns := format(
               '%1$I, %2$I, %3$I, %4$I',
               'datetime',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_period_expr := format(
               'aligned.%I',
               v_member.member_alias || '__period'
             );
             v_imputed_expr := format(
               'COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
             );
             v_first := FALSE;
           ELSIF v_member.alignment_tolerance IS NULL THEN
             v_aligned_sql := format(
               'SELECT
                  aligned.*,
                  %1$I.value_corrected AS %1$I,
                  %1$I.period AS %2$I,
                  %1$I.imputed AS %3$I
                FROM (%4$s) aligned
                JOIN %5$s AS %1$I
                  USING (datetime)',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_aligned_sql,
               v_source_sql
             );
             v_output_columns := v_output_columns || format(
               ', %1$I, %2$I, %3$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
             );
           ELSIF v_member.reuse_member_values THEN
             v_aligned_sql := format(
               'WITH aligned_prev AS (
                  %4$s
                ),
                member_source AS (
                  SELECT src.*
                  FROM %5$s AS src
                ),
                candidates AS (
                  SELECT
                    aligned_prev.*,
                    member_source.value_corrected AS %1$I,
                    member_source.period AS %2$I,
                    member_source.imputed AS %3$I,
                    member_source.datetime AS %7$I,
                    row_number() OVER (
                      PARTITION BY aligned_prev.datetime
                      ORDER BY
                        abs(extract(epoch FROM (member_source.datetime - aligned_prev.datetime))),
                        member_source.datetime
                    ) AS anchor_rank
                  FROM aligned_prev
                  JOIN member_source
                    ON member_source.datetime >= aligned_prev.datetime - %8$L::interval
                   AND member_source.datetime <= aligned_prev.datetime + %8$L::interval
                )
                SELECT %6$s
                FROM candidates
                WHERE anchor_rank = 1',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_aligned_sql,
               v_source_sql,
               v_output_columns || format(
                 ', %1$I, %2$I, %3$I',
                 v_member.member_alias,
                 v_member.member_alias || '__period',
                 v_member.member_alias || '__imputed'
               ),
               v_member.member_alias || '__matched_datetime',
               v_member.alignment_tolerance
             );
             v_output_columns := v_output_columns || format(
               ', %1$I, %2$I, %3$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
             );
           ELSE
             v_aligned_sql := format(
               'WITH aligned_prev AS (
                  %4$s
                ),
                member_source AS (
                  SELECT src.*
                  FROM %5$s AS src
                ),
                candidates AS (
                  SELECT
                    aligned_prev.*,
                    member_source.value_corrected AS %1$I,
                    member_source.period AS %2$I,
                    member_source.imputed AS %3$I,
                    row_number() OVER (
                      PARTITION BY aligned_prev.datetime
                      ORDER BY
                        abs(extract(epoch FROM (member_source.datetime - aligned_prev.datetime))),
                        member_source.datetime
                    ) AS anchor_rank,
                    row_number() OVER (
                      PARTITION BY member_source.datetime
                      ORDER BY
                        abs(extract(epoch FROM (member_source.datetime - aligned_prev.datetime))),
                        aligned_prev.datetime
                    ) AS member_rank
                  FROM aligned_prev
                  JOIN member_source
                    ON member_source.datetime >= aligned_prev.datetime - %7$L::interval
                   AND member_source.datetime <= aligned_prev.datetime + %7$L::interval
                )
                SELECT %6$s
                FROM candidates
                WHERE anchor_rank = 1
                  AND member_rank = 1',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed',
               v_aligned_sql,
               v_source_sql,
               v_output_columns || format(
                 ', %1$I, %2$I, %3$I',
                 v_member.member_alias,
                 v_member.member_alias || '__period',
                 v_member.member_alias || '__imputed'
               ),
               v_member.alignment_tolerance
             );
             v_output_columns := v_output_columns || format(
               ', %1$I, %2$I, %3$I',
               v_member.member_alias,
               v_member.member_alias || '__period',
               v_member.member_alias || '__imputed'
             );
             v_imputed_expr := v_imputed_expr || format(
               ' OR COALESCE(aligned.%I, FALSE)',
               v_member.member_alias || '__imputed'
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
            FROM (%4$s) aligned',
           v_expression,
           v_period_expr,
           v_imputed_expr,
           v_aligned_sql
         );

         RETURN QUERY EXECUTE v_sql;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.resolve_compound_timeseries_raw_window_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE, INTEGER[]) IS
       'Resolves one compound timeseries to raw derived values for a requested datetime window using historical compound definitions and source measurements/corrections. Expression members require exact timestamp alignment unless a non-anchor member defines alignment_tolerance, in which case the nearest point within tolerance is used according to reuse_member_values.'"
    )

    message("Recreating public corrected-measurement wrappers with explicit compound window arguments...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_basic_simple(
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
         imputed BOOLEAN
       )
       LANGUAGE sql
       STABLE
       SECURITY INVOKER
       SET search_path = continuous, pg_catalog
       AS $function$
         WITH RECURSIVE correction_steps AS MATERIALIZED (
           SELECT
             row_number() OVER (
               ORDER BY ct.priority ASC, c.correction_id ASC
             )::INTEGER AS step,
             c.correction_id,
             c.value1,
             c.value2,
             c.timestep_window,
             c.equation,
             c.start_dt,
             c.end_dt,
             ct.correction_type,
             ct.priority
           FROM continuous.corrections c
           JOIN continuous.correction_types ct
             ON c.correction_type = ct.correction_type_id
           WHERE c.timeseries_id = p_timeseries_id
             AND (p_to IS NULL OR c.start_dt <= p_to)
             AND (p_from IS NULL OR c.end_dt >= p_from)
         ),
         measurements AS MATERIALIZED (
           SELECT
             mc.timeseries_id,
             mc.datetime,
             mc.value AS value_raw,
             mc.value AS value_corrected,
             mc.period,
             mc.imputed,
             (mc.value IS NULL) AS removed
           FROM continuous.measurements_continuous mc
           WHERE mc.timeseries_id = p_timeseries_id
             AND (p_from IS NULL OR mc.datetime >= p_from)
             AND (p_to IS NULL OR mc.datetime <= p_to)
         ),
         corrected AS (
           SELECT
             0::INTEGER AS step,
             m.timeseries_id,
             m.datetime,
             m.value_raw,
             m.value_corrected,
             m.period,
             m.imputed,
             m.removed
           FROM measurements m

           UNION ALL

           SELECT
             cs.step,
             c.timeseries_id,
             c.datetime,
             c.value_raw,
             CASE
               WHEN c.removed THEN NULL
               WHEN NOT (
                 cs.start_dt <= c.datetime AND
                 cs.end_dt >= c.datetime
               ) THEN c.value_corrected
               WHEN cs.correction_type = 'delete' THEN NULL
               WHEN cs.correction_type = 'trim' AND (
                 (cs.value1 IS NOT NULL AND c.value_corrected < cs.value1) OR
                 (cs.value2 IS NOT NULL AND c.value_corrected > cs.value2)
               ) THEN NULL
               WHEN cs.correction_type = 'offset linear' THEN
                 c.value_corrected + cs.value1
               WHEN cs.correction_type = 'offset two-point' THEN
                 c.value_corrected + (
                   cs.value1 + (
                     (cs.value2 - cs.value1) /
                     extract(epoch FROM (cs.end_dt - cs.start_dt))
                   ) * extract(epoch FROM (c.datetime - cs.start_dt))
                 )
               WHEN cs.correction_type = 'scale' THEN
                 c.value_corrected * (cs.value1 / 100.0)
               WHEN cs.correction_type = 'drift linear' THEN
                 c.value_corrected + (
                   (cs.value1 / extract(epoch FROM cs.timestep_window)) *
                   extract(epoch FROM (c.datetime - cs.start_dt))
                 )
               ELSE c.value_corrected
             END AS value_corrected,
             c.period,
             c.imputed,
             c.removed OR (
               cs.start_dt <= c.datetime AND
               cs.end_dt >= c.datetime AND
               (
                 cs.correction_type = 'delete' OR
                 (
                   cs.correction_type = 'trim' AND (
                     (cs.value1 IS NOT NULL AND c.value_corrected < cs.value1) OR
                     (cs.value2 IS NOT NULL AND c.value_corrected > cs.value2)
                   )
                 )
               )
             ) AS removed
           FROM corrected c
           JOIN correction_steps cs
             ON cs.step = c.step + 1
         )
         SELECT
           c.timeseries_id,
           c.datetime,
           c.value_raw,
           c.value_corrected,
           c.period,
           c.imputed
         FROM corrected c
         WHERE c.step = (SELECT count(*) FROM correction_steps)
       $function$"
    )

    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.measurements_continuous_corrected_basic_simple(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.measurements_continuous_corrected_basic_simple(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_basic_simple(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) IS
       'Applies ordinary current corrections to one basic timeseries over a requested datetime window as a set. Used by continuous.measurements_continuous_corrected() when overlapping corrections do not require dynamic drift-equation evaluation.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected(
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         statistic TEXT DEFAULT 'actual',
         resample_seconds INTEGER DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       SECURITY INVOKER
       AS $function$
       DECLARE
         v_type TEXT;
         v_statistic TEXT := lower(statistic);
         v_step INTERVAL;
         v_window_from TIMESTAMPTZ;
         v_window_to TIMESTAMPTZ;
         v_has_corrections BOOLEAN := TRUE;
         v_simple_corrections BOOLEAN := FALSE;
         v_correction_count INTEGER := 0;
       BEGIN
         v_window_from := p_from;
         v_window_to := p_to;

         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_statistic IS NULL THEN
           v_statistic := 'actual';
         END IF;

         IF v_statistic NOT IN ('actual', 'min', 'max', 'mean', 'median') THEN
           RAISE EXCEPTION 'statistic must be one of actual, min, max, mean, median';
         END IF;

         IF v_type = 'basic' THEN
           SELECT EXISTS (
             SELECT 1
             FROM continuous.corrections c
             WHERE c.timeseries_id = p_timeseries_id
               AND (v_window_to IS NULL OR c.start_dt <= v_window_to)
               AND (v_window_from IS NULL OR c.end_dt >= v_window_from)
           )
           INTO v_has_corrections;

           IF v_has_corrections THEN
             SELECT
               count(*)::INTEGER,
               count(*) <= 64 AND
                 count(*) FILTER (
                   WHERE ct.correction_type NOT IN (
                     'delete',
                     'trim',
                     'offset linear',
                     'offset two-point',
                     'drift linear',
                     'scale'
                   )
                 ) = 0
             FROM continuous.corrections c
             JOIN continuous.correction_types ct
               ON c.correction_type = ct.correction_type_id
             WHERE c.timeseries_id = p_timeseries_id
               AND (v_window_to IS NULL OR c.start_dt <= v_window_to)
               AND (v_window_from IS NULL OR c.end_dt >= v_window_from)
             INTO v_correction_count, v_simple_corrections;
           END IF;
         END IF;

         IF v_statistic = 'actual' THEN
           IF resample_seconds IS NOT NULL THEN
             RAISE EXCEPTION 'resample_seconds can only be used when statistic is min, max, mean, or median';
           END IF;

           IF v_type = 'basic' AND NOT v_has_corrections THEN
             RETURN QUERY
             SELECT
               mc.timeseries_id,
               mc.datetime,
               mc.value AS value_raw,
               mc.value AS value_corrected,
               mc.period,
               mc.imputed
             FROM continuous.measurements_continuous mc
             WHERE mc.timeseries_id = p_timeseries_id
               AND (v_window_from IS NULL OR mc.datetime >= v_window_from)
               AND (v_window_to IS NULL OR mc.datetime <= v_window_to);

             RETURN;
           END IF;

           IF v_type = 'basic' THEN
             IF v_simple_corrections THEN
               RETURN QUERY
               SELECT
                 simple.timeseries_id,
                 simple.datetime,
                 simple.value_raw,
                 simple.value_corrected,
                 simple.period,
                 simple.imputed
               FROM continuous.measurements_continuous_corrected_basic_simple(
                 p_timeseries_id,
                 v_window_from,
                 v_window_to
               ) simple;

               RETURN;
             END IF;

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
               mc.imputed
             FROM continuous.measurements_continuous mc
             WHERE mc.timeseries_id = p_timeseries_id
               AND (v_window_from IS NULL OR mc.datetime >= v_window_from)
               AND (v_window_to IS NULL OR mc.datetime <= v_window_to);

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
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window(
             p_timeseries_id,
             v_window_from,
             v_window_to,
             ARRAY[]::INTEGER[]
           ) src;

           RETURN;
         END IF;

         IF resample_seconds IS NULL OR resample_seconds <= 0 THEN
           RAISE EXCEPTION 'resample_seconds must be a positive integer when statistic is min, max, mean, or median';
         END IF;

         IF v_window_from IS NULL OR v_window_to IS NULL THEN
           RAISE EXCEPTION 'p_from and p_to are required when statistic is min, max, mean, or median';
         END IF;

         IF v_window_to < v_window_from THEN
           RAISE EXCEPTION 'p_to must be greater than or equal to p_from';
         END IF;

         v_step := make_interval(secs => resample_seconds);

         IF v_type = 'basic' AND NOT v_has_corrections THEN
           RETURN QUERY
           WITH binned_measurements AS (
             SELECT
               v_window_from + make_interval(
                 secs => (
                   floor(
                     extract(epoch FROM (src.datetime - v_window_from)) /
                       resample_seconds
                   ) * resample_seconds
                 )::double precision
               ) AS datetime,
               mc.value AS value_raw,
               mc.value AS value_corrected,
               mc.imputed
             FROM continuous.measurements_continuous mc
             WHERE mc.timeseries_id = p_timeseries_id
               AND mc.datetime >= v_window_from
               AND mc.datetime <= v_window_to
           ),
           grouped_measurements AS MATERIALIZED (
             SELECT
               binned_measurements.datetime,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_raw)
                 WHEN 'max' THEN max(binned_measurements.value_raw)
                 WHEN 'mean' THEN avg(binned_measurements.value_raw)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_raw::double precision
                   )
                 )::numeric
               END AS value_raw,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_corrected)
                 WHEN 'max' THEN max(binned_measurements.value_corrected)
                 WHEN 'mean' THEN avg(binned_measurements.value_corrected)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_corrected::double precision
                   )
                 )::numeric
               END AS value_corrected,
               bool_or(binned_measurements.imputed) AS imputed
             FROM binned_measurements
             GROUP BY binned_measurements.datetime
           ),
           bins AS (
             SELECT generate_series(v_window_from, v_window_to, v_step) AS datetime
           )
           SELECT
             p_timeseries_id AS timeseries_id,
             bins.datetime,
             grouped_measurements.value_raw,
             grouped_measurements.value_corrected,
             v_step AS period,
             grouped_measurements.imputed
           FROM bins
           LEFT JOIN grouped_measurements
             ON grouped_measurements.datetime = bins.datetime
           ORDER BY bins.datetime;

           RETURN;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           WITH binned_measurements AS (
             SELECT
               v_window_from + make_interval(
                 secs => (
                   floor(
                     extract(epoch FROM (src.datetime - v_window_from)) /
                       resample_seconds
                   ) * resample_seconds
                 )::double precision
               ) AS datetime,
               src.value_raw,
               src.value_corrected,
               src.imputed
             FROM continuous.measurements_continuous_corrected_basic_simple(
               p_timeseries_id,
               v_window_from,
               v_window_to
             ) src
             WHERE v_simple_corrections
             UNION ALL
             SELECT
               v_window_from + make_interval(
                 secs => (
                   floor(
                     extract(epoch FROM (mc.datetime - v_window_from)) /
                       resample_seconds
                   ) * resample_seconds
                 )::double precision
               ) AS datetime,
               mc.value AS value_raw,
               continuous.apply_corrections(
                 mc.timeseries_id,
                 mc.datetime,
                 mc.value
               ) AS value_corrected,
               mc.imputed
             FROM continuous.measurements_continuous mc
             WHERE NOT v_simple_corrections
               AND mc.timeseries_id = p_timeseries_id
               AND mc.datetime >= v_window_from
               AND mc.datetime <= v_window_to
           ),
           grouped_measurements AS MATERIALIZED (
             SELECT
               binned_measurements.datetime,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_raw)
                 WHEN 'max' THEN max(binned_measurements.value_raw)
                 WHEN 'mean' THEN avg(binned_measurements.value_raw)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_raw::double precision
                   )
                 )::numeric
               END AS value_raw,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_corrected)
                 WHEN 'max' THEN max(binned_measurements.value_corrected)
                 WHEN 'mean' THEN avg(binned_measurements.value_corrected)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_corrected::double precision
                   )
                 )::numeric
               END AS value_corrected,
               bool_or(binned_measurements.imputed) AS imputed
             FROM binned_measurements
             GROUP BY binned_measurements.datetime
           ),
           bins AS (
             SELECT generate_series(v_window_from, v_window_to, v_step) AS datetime
           )
           SELECT
             p_timeseries_id AS timeseries_id,
             bins.datetime,
             grouped_measurements.value_raw,
             grouped_measurements.value_corrected,
             v_step AS period,
             grouped_measurements.imputed
           FROM bins
           LEFT JOIN grouped_measurements
             ON grouped_measurements.datetime = bins.datetime
           ORDER BY bins.datetime;

           RETURN;
         END IF;

         RETURN QUERY
         WITH binned_measurements AS (
           SELECT
             v_window_from + make_interval(
               secs => (
                 floor(
                   extract(epoch FROM (src.datetime - v_window_from)) /
                     resample_seconds
                 ) * resample_seconds
               )::double precision
             ) AS datetime,
             src.value_raw,
             continuous.apply_corrections(
               p_timeseries_id,
               src.datetime,
               src.value_raw
             ) AS value_corrected,
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window(
             p_timeseries_id,
             v_window_from,
             v_window_to,
             ARRAY[]::INTEGER[]
           ) src
         ),
         grouped_measurements AS MATERIALIZED (
           SELECT
             binned_measurements.datetime,
             CASE v_statistic
               WHEN 'min' THEN min(binned_measurements.value_raw)
               WHEN 'max' THEN max(binned_measurements.value_raw)
               WHEN 'mean' THEN avg(binned_measurements.value_raw)
               WHEN 'median' THEN (
                 percentile_cont(0.5) WITHIN GROUP (
                   ORDER BY binned_measurements.value_raw::double precision
                 )
               )::numeric
             END AS value_raw,
             CASE v_statistic
               WHEN 'min' THEN min(binned_measurements.value_corrected)
               WHEN 'max' THEN max(binned_measurements.value_corrected)
               WHEN 'mean' THEN avg(binned_measurements.value_corrected)
               WHEN 'median' THEN (
                 percentile_cont(0.5) WITHIN GROUP (
                   ORDER BY binned_measurements.value_corrected::double precision
                 )
               )::numeric
             END AS value_corrected,
             bool_or(binned_measurements.imputed) AS imputed
           FROM binned_measurements
           GROUP BY binned_measurements.datetime
         ),
         bins AS (
           SELECT generate_series(v_window_from, v_window_to, v_step) AS datetime
         )
         SELECT
           p_timeseries_id AS timeseries_id,
           bins.datetime,
           grouped_measurements.value_raw,
           grouped_measurements.value_corrected,
           v_step AS period,
           grouped_measurements.imputed
         FROM bins
         LEFT JOIN grouped_measurements
           ON grouped_measurements.datetime = bins.datetime
         ORDER BY bins.datetime;
       END;
       $function$"
    )

    at_wrapper_def <- DBI::dbGetQuery(
      con,
      "SELECT pg_get_functiondef(
         'continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, text, integer)'::regprocedure
       ) AS def"
    )$def[[1]]
    if (!grepl("v_window_from TIMESTAMPTZ", at_wrapper_def, fixed = TRUE)) {
      at_wrapper_def <- sub(
        "v_step INTERVAL;",
        "v_step INTERVAL;
         v_window_from TIMESTAMPTZ;
         v_window_to TIMESTAMPTZ;",
        at_wrapper_def,
        fixed = TRUE
      )
      at_wrapper_def <- sub(
        "BEGIN
         SELECT t.timeseries_type",
        "BEGIN
         v_window_from := p_from;
         v_window_to := p_to;

         SELECT t.timeseries_type",
        at_wrapper_def,
        fixed = TRUE
      )
    }
    at_wrapper_def <- gsub(
      "p_timeseries_id,
             p_from,
             p_to,
             ARRAY[]::INTEGER[]",
      "p_timeseries_id,
             v_window_from,
             v_window_to,
             ARRAY[]::INTEGER[]",
      at_wrapper_def,
      fixed = TRUE
    )
    at_wrapper_def <- gsub(
      "p_timeseries_id,
             p_from,
             p_to,
             ARRAY[]::integer[]",
      "p_timeseries_id,
             v_window_from,
             v_window_to,
             ARRAY[]::integer[]",
      at_wrapper_def,
      fixed = TRUE
    )
    DBI::dbExecute(con, at_wrapper_def)

    DBI::dbExecute(con, paste0("REVOKE ALL ON FUNCTION ", members_as_of_signature, " FROM PUBLIC"))
    DBI::dbExecute(con, paste0("REVOKE ALL ON FUNCTION ", current_resolver_signature, " FROM PUBLIC"))
    DBI::dbExecute(con, paste0("REVOKE ALL ON FUNCTION ", at_resolver_signature, " FROM PUBLIC"))

    apply_function_execute_grants(members_as_of_signature, members_as_of_grants)
    apply_function_execute_grants(current_resolver_signature, current_resolver_grants)
    apply_function_execute_grants(at_resolver_signature, at_resolver_grants)

    message("Checking compound alignment patch objects...")
    result_check <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'timeseries_compound_members'
             AND column_name = 'alignment_tolerance'
         ) AS has_alignment_tolerance,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'timeseries_compound_members'
             AND column_name = 'reuse_member_values'
             AND is_nullable = 'NO'
         ) AS has_reuse_member_values,
         to_regprocedure('audit.timeseries_compound_members_as_of(timestamp with time zone, integer[])') IS NOT NULL AS has_members_as_of,
         to_regprocedure('continuous.resolve_compound_timeseries_raw_window(integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_current_resolver,
         to_regprocedure('continuous.resolve_compound_timeseries_raw_window_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_at_resolver,
         to_regprocedure('continuous.measurements_continuous_corrected_basic_simple(integer, timestamp with time zone, timestamp with time zone)') IS NOT NULL AS has_basic_simple"
    )

    if (
      !isTRUE(result_check$has_alignment_tolerance[[1]]) ||
        !isTRUE(result_check$has_reuse_member_values[[1]]) ||
        !isTRUE(result_check$has_members_as_of[[1]]) ||
        !isTRUE(result_check$has_current_resolver[[1]]) ||
        !isTRUE(result_check$has_at_resolver[[1]]) ||
        !isTRUE(result_check$has_basic_simple[[1]])
    ) {
      stop(
        "Patch 53 verification failed: compound alignment columns or resolver functions were not created as expected."
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '53'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        patch_package_version,
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    DBI::dbExecute(con, "COMMIT;")
    active <- FALSE

    message("Patch 53 applied successfully.")
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
