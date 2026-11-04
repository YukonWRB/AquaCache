# Patch 67: authorize reviewed parameter unit migrations and redesign extrema.
# Source with an existing postgres connection named `con`.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop("Patch 67 requires a postgres maintenance connection.")
}
if (getFromNamespace("dbTransCheck", "AquaCache")(con)) {
  stop("A transaction is already in progress. Commit or roll it back first.")
}

active <- getFromNamespace("dbTransBegin", "AquaCache")(con)
tryCatch(
  {
    DBI::dbExecute(con, "SET LOCAL lock_timeout = '30s'")
    DBI::dbExecute(con, "LOCK TABLE information.version_info IN EXCLUSIVE MODE")
    revision <- DBI::dbGetQuery(
      con,
      "SELECT version FROM information.version_info
       WHERE item = 'Last patch number'"
    )
    if (nrow(revision) != 1L || revision$version[[1]] != "66") {
      stop("Patch 67 requires exactly revision 66.")
    }

    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('public.parameters') IS NOT NULL AS parameters,
         to_regclass('continuous.extrema') IS NOT NULL AS extrema,
         EXISTS (SELECT 1 FROM pg_namespace WHERE nspname = 'audit') AS audit_schema,
         to_regprocedure('public.parameter_matrix_state_has_results(integer,text)')
           IS NOT NULL AS usage_function,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'extrema'
             AND column_name = 'uncertainty'
             AND udt_name = 'text'
         ) AS legacy_text_uncertainty,
         EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'admin') AS admin_role,
         COALESCE((
           SELECT has_schema_privilege(role.oid, 'continuous', 'CREATE')
           FROM pg_roles role WHERE role.rolname = 'admin'
         ), FALSE) AS admin_can_own_continuous_tables"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 67 requires parameters, continuous.extrema with its legacy text uncertainty column, the audit schema, the Patch 62 usage function, and an admin role that can own tables in the continuous schema."
      )
    }

    # Keep the empty-table check and replacement atomic with respect to writers.
    # Taking the lock only at DROP TABLE could discard a just-committed insert.
    DBI::dbExecute(con, "LOCK TABLE continuous.extrema IN ACCESS EXCLUSIVE MODE")
    extrema_rows <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::TEXT AS n FROM continuous.extrema"
    )
    if (as.numeric(extrema_rows$n[[1]]) > 0) {
      stop(paste(
        "Patch 67 recreates continuous.extrema and requires it to be empty.",
        "Migrate any extrema records separately before applying this patch."
      ))
    }

    DBI::dbExecute(con, "DROP TABLE continuous.extrema")
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.extrema_period_types (
         period_type_code TEXT PRIMARY KEY,
         period_type_name TEXT NOT NULL,
         period_type_name_fr TEXT NOT NULL,
         display_order SMALLINT NOT NULL UNIQUE,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         CONSTRAINT extrema_period_type_code_not_blank
           CHECK (btrim(period_type_code) <> ''),
         CONSTRAINT extrema_period_type_name_not_blank
           CHECK (btrim(period_type_name) <> '')
       )"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO continuous.extrema_period_types
         (period_type_code, period_type_name, period_type_name_fr, display_order)
       VALUES
         ('instantaneous', 'Instantaneous', 'Instantané', 1),
         ('1-day', '1 day', '1 jour', 2),
         ('2-day', '2 days', '2 jours', 3),
         ('3-day', '3 days', '3 jours', 4),
         ('4-day', '4 days', '4 jours', 5),
         ('5-day', '5 days', '5 jours', 6),
         ('6-day', '6 days', '6 jours', 7),
         ('7-day', '7 days', '7 jours', 8),
         ('monthly', 'Monthly', 'Mensuel', 9),
         ('yearly', 'yearly', 'annuel', 10)"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.extrema_conditions (
         condition_code TEXT PRIMARY KEY,
         condition_name TEXT NOT NULL,
         condition_name_fr TEXT NOT NULL,
         display_order SMALLINT NOT NULL UNIQUE,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         CONSTRAINT extrema_condition_code_not_blank
           CHECK (btrim(condition_code) <> ''),
         CONSTRAINT extrema_condition_name_not_blank
           CHECK (btrim(condition_name) <> '')
       )"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO continuous.extrema_conditions
         (condition_code, condition_name, condition_name_fr, display_order)
       VALUES
         ('open_water', 'Open water', 'Eau libre', 1),
         ('break_up', 'Break-up', 'Débâcle', 2),
         ('freeze_up', 'Freeze-up', 'Prise des glaces', 3),
         ('winter', 'Winter', 'Hiver', 4)"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.extrema_types (
         extremum_type_code TEXT PRIMARY KEY,
         extremum_type_name TEXT NOT NULL,
         extremum_type_name_fr TEXT NOT NULL,
         display_order SMALLINT NOT NULL UNIQUE,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         CONSTRAINT extrema_type_code_not_blank
           CHECK (btrim(extremum_type_code) <> ''),
         CONSTRAINT extrema_type_name_not_blank
           CHECK (btrim(extremum_type_name) <> '')
       )"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO continuous.extrema_types
         (extremum_type_code, extremum_type_name, extremum_type_name_fr, display_order)
       VALUES
         ('minimum', 'Minimum', 'Minimum', 1),
         ('maximum', 'Maximum', 'Maximum', 2)"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.extrema_record_sources (
         record_source_code TEXT PRIMARY KEY,
         record_source_name TEXT NOT NULL,
         record_source_name_fr TEXT NOT NULL,
         display_order SMALLINT NOT NULL UNIQUE,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         CONSTRAINT extrema_record_source_code_not_blank
           CHECK (btrim(record_source_code) <> ''),
         CONSTRAINT extrema_record_source_name_not_blank
           CHECK (btrim(record_source_name) <> '')
       )"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO continuous.extrema_record_sources
         (record_source_code, record_source_name, record_source_name_fr, display_order)
       VALUES
         ('calculated', 'Calculated from timeseries', 'Calculé à partir de la série chronologique', 1),
         ('curated', 'Curated', 'Validé manuellement', 2)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.extrema (
         extremum_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         agency_organization_id INTEGER NOT NULL
           REFERENCES public.organizations(organization_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         project_id INTEGER
           REFERENCES public.projects(project_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         extremum_year INTEGER NOT NULL,
         extremum_date DATE NOT NULL,
         timezone_daily_calc INTEGER NOT NULL,
         value NUMERIC NOT NULL,
         period_type_code TEXT NOT NULL
           REFERENCES continuous.extrema_period_types(period_type_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         condition_code TEXT NOT NULL
           REFERENCES continuous.extrema_conditions(condition_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         extremum_type_code TEXT NOT NULL
           REFERENCES continuous.extrema_types(extremum_type_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         record_source_code TEXT NOT NULL
           REFERENCES continuous.extrema_record_sources(record_source_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         uncertainty_lower NUMERIC,
         uncertainty_upper NUMERIC,
         source_reference TEXT,
         calculation_method TEXT,
         notes TEXT,
         deemed_primary BOOLEAN NOT NULL DEFAULT FALSE,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT extrema_uncertainty_lower_nonnegative CHECK (
           uncertainty_lower IS NULL
           OR (uncertainty_lower >= 0 AND uncertainty_lower <> 'NaN'::NUMERIC)
         ),
         CONSTRAINT extrema_uncertainty_upper_nonnegative CHECK (
           uncertainty_upper IS NULL
           OR (uncertainty_upper >= 0 AND uncertainty_upper <> 'NaN'::NUMERIC)
         ),
         CONSTRAINT extrema_timezone_daily_calc_valid CHECK (
           timezone_daily_calc >= -12 AND timezone_daily_calc <= 14
         ),
         CONSTRAINT extrema_curated_project_required CHECK (
           record_source_code <> 'curated' OR project_id IS NOT NULL
         ),
         CONSTRAINT extrema_calculation_method_required CHECK (
           record_source_code <> 'calculated'
           OR NULLIF(btrim(calculation_method), '') IS NOT NULL
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX extrema_timeseries_context_idx
       ON continuous.extrema (
         timeseries_id, extremum_year, period_type_code,
         condition_code, extremum_type_code
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX extrema_project_idx ON continuous.extrema (project_id)
       WHERE project_id IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX extrema_one_primary_per_context_idx
       ON continuous.extrema (
         timeseries_id, extremum_year, extremum_date, period_type_code,
         condition_code, extremum_type_code, COALESCE(project_id, 0)
       ) WHERE deemed_primary"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.extrema OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.extrema ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.extrema FORCE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY extrema_parent_timeseries_visibility
       ON continuous.extrema FOR ALL
       USING (
         EXISTS (
           SELECT 1 FROM continuous.timeseries timeseries
           WHERE timeseries.timeseries_id = extrema.timeseries_id
             AND (
               timeseries.share_with @> ARRAY['public_reader']::TEXT[]
               OR EXISTS (
                 SELECT 1 FROM unnest(timeseries.share_with) shared_role(role_name)
                 WHERE pg_has_role(CURRENT_USER, shared_role.role_name::NAME, 'member')
               )
             )
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1 FROM continuous.timeseries timeseries
           WHERE timeseries.timeseries_id = extrema.timeseries_id
             AND (
               timeseries.share_with @> ARRAY['public_reader']::TEXT[]
               OR EXISTS (
                 SELECT 1 FROM unnest(timeseries.share_with) shared_role(role_name)
                 WHERE pg_has_role(CURRENT_USER, shared_role.role_name::NAME, 'member')
               )
             )
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_extrema_trigger
       AFTER INSERT OR DELETE OR UPDATE ON continuous.extrema
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.extrema
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time BEFORE UPDATE ON continuous.extrema
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.extrema IS
       'Reported annual or seasonal extrema tied to a timeseries, an agency organization, a study, a controlled period/condition/type, and calculation or curation provenance. timezone_daily_calc snapshots the UTC offset in hours used to define the local extremum date. Multiple records may exist for each timeseries.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.extrema.timezone_daily_calc IS
       'UTC offset in hours, matching continuous.timeseries.timezone_daily_calc, that defines the local day for this extremum. Stored on the extremum so later timeseries timezone changes do not alter its interpretation.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.extrema.uncertainty_lower IS
       'Optional non-negative distance below value, in the same unit as value.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.extrema.uncertainty_upper IS
       'Optional non-negative distance above value, in the same unit as value.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.extrema.agency_organization_id IS
       'Organization responsible for the reported or curated extremum.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.extrema.project_id IS
       'Study or project context, required for curated records.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.extrema.record_source_code IS
       'Whether the value was calculated from timeseries data or curated by a reviewer.'"
    )

    extrema_reference_tables <- c(
      "continuous.extrema_period_types",
      "continuous.extrema_conditions",
      "continuous.extrema_types",
      "continuous.extrema_record_sources"
    )
    # Match the cluster-specific ACL on timeseries instead of assuming that
    # every AquaCache cluster has the same reader and editor roles.
    timeseries_acl <- DBI::dbGetQuery(
      con,
      "SELECT CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE role.rolname END AS grantee,
         acl.privilege_type, acl.is_grantable
       FROM pg_class table_info
       CROSS JOIN LATERAL aclexplode(
         COALESCE(table_info.relacl, acldefault('r', table_info.relowner))
       ) acl
       LEFT JOIN pg_roles role ON role.oid = acl.grantee
       WHERE table_info.oid = 'continuous.timeseries'::regclass
         AND acl.grantee <> table_info.relowner
         AND (acl.grantee = 0 OR role.rolname <> 'admin')
       ORDER BY grantee, privilege_type, is_grantable"
    )
    extrema_tables <- c("continuous.extrema", extrema_reference_tables)
    for (table in extrema_tables) {
      table_id <- DBI::Id(
        schema = "continuous",
        table = sub("^continuous\\.", "", table)
      )
      table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_id))
      DBI::dbExecute(con, paste("ALTER TABLE", table_sql, "OWNER TO admin"))
      existing_grantees <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT DISTINCT CASE WHEN acl.grantee = 0 THEN 'PUBLIC'",
          "ELSE role.rolname END AS grantee",
          "FROM pg_class table_info",
          "CROSS JOIN LATERAL aclexplode(",
          "COALESCE(table_info.relacl, acldefault('r', table_info.relowner))",
          ") acl LEFT JOIN pg_roles role ON role.oid = acl.grantee",
          "WHERE table_info.oid = to_regclass($1)",
          "AND acl.grantee <> table_info.relowner"
        ),
        params = list(table)
      )$grantee
      for (grantee in existing_grantees) {
        grantee_sql <- if (identical(grantee, "PUBLIC")) {
          "PUBLIC"
        } else {
          as.character(DBI::dbQuoteIdentifier(con, grantee))
        }
        DBI::dbExecute(
          con,
          paste(
            "REVOKE ALL PRIVILEGES ON TABLE", table_sql,
            "FROM", grantee_sql, "CASCADE"
          )
        )
      }
      for (acl_idx in seq_len(nrow(timeseries_acl))) {
        grantee <- timeseries_acl$grantee[[acl_idx]]
        grantee_sql <- if (identical(grantee, "PUBLIC")) {
          "PUBLIC"
        } else {
          as.character(DBI::dbQuoteIdentifier(con, grantee))
        }
        grant_option <- if (isTRUE(timeseries_acl$is_grantable[[acl_idx]])) {
          "WITH GRANT OPTION"
        } else {
          ""
        }
        DBI::dbExecute(
          con,
          paste(
            "GRANT", timeseries_acl$privilege_type[[acl_idx]],
            "ON TABLE", table_sql, "TO", grantee_sql, grant_option
          )
        )
      }
    }
    DBI::dbExecute(
      con,
      "ALTER SEQUENCE continuous.extrema_extremum_id_seq OWNER TO admin"
    )
    sequence_grantees <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE role.rolname END AS grantee
       FROM pg_class sequence_info
       CROSS JOIN LATERAL aclexplode(
         COALESCE(sequence_info.relacl, acldefault('S', sequence_info.relowner))
       ) acl
       LEFT JOIN pg_roles role ON role.oid = acl.grantee
       WHERE sequence_info.oid = 'continuous.extrema_extremum_id_seq'::regclass
         AND acl.grantee <> sequence_info.relowner"
    )$grantee
    for (grantee in sequence_grantees) {
      grantee_sql <- if (identical(grantee, "PUBLIC")) {
        "PUBLIC"
      } else {
        as.character(DBI::dbQuoteIdentifier(con, grantee))
      }
      DBI::dbExecute(
        con,
        paste(
          "REVOKE ALL PRIVILEGES ON SEQUENCE",
          "continuous.extrema_extremum_id_seq FROM", grantee_sql, "CASCADE"
        )
      )
    }
    for (grantee in unique(timeseries_acl$grantee[
      timeseries_acl$privilege_type == "INSERT"
    ])) {
      grantee_sql <- if (identical(grantee, "PUBLIC")) {
        "PUBLIC"
      } else {
        as.character(DBI::dbQuoteIdentifier(con, grantee))
      }
      DBI::dbExecute(
        con,
        paste(
          "GRANT USAGE, SELECT ON SEQUENCE continuous.extrema_extremum_id_seq TO",
          grantee_sql
        )
      )
    }

    for (table in extrema_tables) {
      actual_acl <- DBI::dbGetQuery(
        con,
        "SELECT CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE role.rolname END AS grantee,
           acl.privilege_type, acl.is_grantable
         FROM pg_class table_info
         CROSS JOIN LATERAL aclexplode(
           COALESCE(table_info.relacl, acldefault('r', table_info.relowner))
         ) acl
         LEFT JOIN pg_roles role ON role.oid = acl.grantee
         WHERE table_info.oid = to_regclass($1)
           AND acl.grantee <> table_info.relowner
           AND (acl.grantee = 0 OR role.rolname <> 'admin')
         ORDER BY grantee, privilege_type, is_grantable",
        params = list(table)
      )
      if (!isTRUE(all.equal(
        timeseries_acl,
        actual_acl,
        check.attributes = FALSE
      ))) {
        stop(paste("Privileges on", table, "do not match continuous.timeseries."))
      }
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE audit.parameter_unit_change_log (
         change_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
         parameter_id INTEGER NOT NULL,
         changed_by TEXT NOT NULL,
         changed_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
         transaction_id BIGINT NOT NULL DEFAULT txid_current(),
         old_units JSONB NOT NULL,
         new_units JSONB NOT NULL,
         change_plan JSONB NOT NULL,
         CONSTRAINT parameter_unit_change_log_objects CHECK (
           jsonb_typeof(old_units) = 'object'
           AND jsonb_typeof(new_units) = 'object'
           AND jsonb_typeof(change_plan) = 'object'
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.parameter_unit_change_log OWNER TO postgres"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON audit.parameter_unit_change_log FROM PUBLIC, admin"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON audit.parameter_unit_change_log TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.parameter_unit_change_log IS
       'Transactional audit record for reviewed AquaLink parameter-unit migrations. The record commits or rolls back with the associated data migration.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.begin_parameter_unit_change(
         in_parameter_id INTEGER,
         in_old_units JSONB,
         in_new_units JSONB,
         in_change_plan JSONB
       ) RETURNS BIGINT
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         current_units JSONB;
         change_id BIGINT;
         caller_is_admin BOOLEAN;
       BEGIN
         SELECT rolsuper OR pg_has_role(session_user, 'admin', 'member')
         INTO caller_is_admin
         FROM pg_roles
         WHERE rolname = session_user;
         IF NOT COALESCE(caller_is_admin, FALSE) THEN
           RAISE EXCEPTION 'Parameter unit migrations require the admin database role.'
             USING ERRCODE = '42501';
         END IF;
         IF in_parameter_id IS NULL
            OR jsonb_typeof(in_old_units) IS DISTINCT FROM 'object'
            OR jsonb_typeof(in_new_units) IS DISTINCT FROM 'object'
            OR jsonb_typeof(in_change_plan) IS DISTINCT FROM 'object' THEN
           RAISE EXCEPTION 'Parameter unit migration context is invalid.'
             USING ERRCODE = '22023';
         END IF;

         PERFORM pg_advisory_xact_lock(
           hashtextextended('parameter-unit-change:' || in_parameter_id::TEXT, 0)
         );
         SELECT jsonb_build_object(
           'units_liquid', parameter.units_liquid,
           'units_solid', parameter.units_solid,
           'units_gas', parameter.units_gas,
           'units_na', parameter.units_na
         )
         INTO current_units
         FROM public.parameters parameter
         WHERE parameter.parameter_id = in_parameter_id
         FOR UPDATE;
         IF NOT FOUND THEN
           RAISE EXCEPTION 'Parameter % no longer exists.', in_parameter_id
             USING ERRCODE = 'P0002';
         END IF;
         IF current_units IS DISTINCT FROM in_old_units THEN
           RAISE EXCEPTION 'The parameter units changed after the migration preview. Reload and review the change again.'
             USING ERRCODE = '40001';
         END IF;

         INSERT INTO audit.parameter_unit_change_log (
           parameter_id, changed_by, old_units, new_units, change_plan
         ) VALUES (
           in_parameter_id, session_user, in_old_units, in_new_units,
           in_change_plan
         ) RETURNING audit.parameter_unit_change_log.change_id INTO change_id;

         PERFORM set_config(
           'aqualink.parameter_unit_change_id', in_parameter_id::TEXT, TRUE
         );
         PERFORM set_config(
           'aqualink.parameter_unit_change_units', in_new_units::TEXT, TRUE
         );
         RETURN change_id;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.begin_parameter_unit_change(INTEGER, JSONB, JSONB, JSONB)
       OWNER TO postgres"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION public.begin_parameter_unit_change(INTEGER, JSONB, JSONB, JSONB)
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION public.begin_parameter_unit_change(INTEGER, JSONB, JSONB, JSONB)
       TO admin"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.parameter_matrix_state_has_results(
         p_parameter_id INTEGER,
         p_matrix_state_code TEXT
       ) RETURNS BOOLEAN
       LANGUAGE SQL
       STABLE
       SET search_path = pg_catalog, public, discrete, continuous, criteria, spatial
       AS $function$
         SELECT
           EXISTS (
             SELECT 1
             FROM discrete.results result
             JOIN public.matrix_states matrix_state USING (matrix_state_id)
             WHERE result.parameter_id = p_parameter_id
               AND matrix_state.matrix_state_code = p_matrix_state_code
           )
           OR EXISTS (
             SELECT 1 FROM continuous.timeseries timeseries
             JOIN public.matrix_states matrix_state USING (matrix_state_id)
             WHERE timeseries.parameter_id = p_parameter_id
               AND matrix_state.matrix_state_code = p_matrix_state_code
           )
           OR EXISTS (
             SELECT 1 FROM criteria.guidelines guideline
             JOIN public.matrix_states matrix_state USING (matrix_state_id)
             WHERE guideline.parameter_id = p_parameter_id
               AND matrix_state.matrix_state_code = p_matrix_state_code
           )
           OR EXISTS (
             SELECT 1 FROM criteria.guideline_rule_inputs input
             JOIN public.matrix_states matrix_state USING (matrix_state_id)
             WHERE input.parameter_id = p_parameter_id
               AND matrix_state.matrix_state_code = p_matrix_state_code
           )
           OR EXISTS (
             SELECT 1 FROM discrete.import_parameter_mappings mapping
             JOIN public.matrix_states matrix_state USING (matrix_state_id)
             JOIN discrete.import_mapping_sets mapping_set USING (import_mapping_set_id)
             WHERE mapping.parameter_id = p_parameter_id
               AND matrix_state.matrix_state_code = p_matrix_state_code
               AND mapping_set.status IN ('draft', 'published')
           )
           OR EXISTS (
             SELECT 1 FROM continuous.rating_curves curve
             WHERE curve.input_parameter_id = p_parameter_id
                OR curve.output_parameter_id = p_parameter_id
           )
           OR EXISTS (
             SELECT 1 FROM public.locations_metadata_instrument_connection_signals signal
             WHERE signal.parameter_id = p_parameter_id
           )
           OR EXISTS (
             SELECT 1 FROM spatial.raster_series_index raster_index
             WHERE raster_index.parameter_id = p_parameter_id
           )
           OR EXISTS (
             SELECT 1 FROM spatial.rasters_reference raster_reference
             WHERE raster_reference.parameter_id = p_parameter_id
           );
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.prevent_used_parameter_unit_update()
       RETURNS trigger
       LANGUAGE plpgsql
       SET search_path = pg_catalog, public
       AS $function$
       DECLARE
         unit_fields TEXT[] := ARRAY['liquid', 'solid', 'gas', 'not_applicable'];
         unit_columns TEXT[] := ARRAY[
           'units_liquid', 'units_solid', 'units_gas', 'units_na'
         ];
         i INTEGER;
         authorized_parameter_id INTEGER;
         authorized_units JSONB;
         current_units JSONB;
         caller_is_admin BOOLEAN;
       BEGIN
         SELECT rolsuper OR pg_has_role(session_user, 'admin', 'member')
         INTO caller_is_admin
         FROM pg_roles
         WHERE rolname = session_user;
         authorized_parameter_id := NULLIF(
           current_setting('aqualink.parameter_unit_change_id', TRUE), ''
         )::INTEGER;
         authorized_units := NULLIF(
           current_setting('aqualink.parameter_unit_change_units', TRUE), ''
         )::JSONB;
         current_units := jsonb_build_object(
           'units_liquid', NEW.units_liquid,
           'units_solid', NEW.units_solid,
           'units_gas', NEW.units_gas,
           'units_na', NEW.units_na
         );
         FOR i IN 1..array_length(unit_fields, 1) LOOP
           IF (to_jsonb(OLD) -> unit_columns[i]) IS DISTINCT FROM
                (to_jsonb(NEW) -> unit_columns[i])
              AND public.parameter_matrix_state_has_results(
                OLD.parameter_id, unit_fields[i]
              )
              AND NOT (
                COALESCE(caller_is_admin, FALSE)
                AND authorized_parameter_id = OLD.parameter_id
                AND authorized_units IS NOT DISTINCT FROM current_units
              ) THEN
             RAISE EXCEPTION
               'Cannot change % for parameter_id %. Existing records use this matrix state; use the reviewed AquaLink unit-change workflow.',
               unit_columns[i], OLD.parameter_id
               USING ERRCODE = 'check_violation';
           END IF;
         END LOOP;
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.prevent_used_parameter_unit_update() OWNER TO postgres"
    )

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('audit.parameter_unit_change_log') IS NOT NULL AS has_log,
         to_regclass('continuous.extrema_period_types') IS NOT NULL AS has_period_types,
         to_regclass('continuous.extrema_conditions') IS NOT NULL AS has_conditions,
         to_regclass('continuous.extrema_types') IS NOT NULL AS has_extrema_types,
         to_regclass('continuous.extrema_record_sources') IS NOT NULL AS has_record_sources,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'extrema'
             AND column_name = 'extremum_id'
             AND is_identity = 'YES'
         ) AS has_extremum_identity,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'extrema'
             AND column_name = 'timezone_daily_calc'
             AND data_type = 'integer'
             AND is_nullable = 'NO'
         ) AS has_extrema_timezone,
         EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conrelid = 'continuous.extrema'::regclass
             AND conname = 'extrema_timezone_daily_calc_valid'
         ) AS has_extrema_timezone_range,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'extrema'
             AND column_name = 'uncertainty_lower'
             AND data_type = 'numeric'
         ) AS has_numeric_uncertainty_lower,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'extrema'
             AND column_name = 'uncertainty_upper'
             AND data_type = 'numeric'
         ) AS has_numeric_uncertainty_upper,
         NOT EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'extrema'
             AND column_name = 'uncertainty'
         ) AS legacy_uncertainty_removed,
         (
            SELECT count(*) >= 7 FROM pg_constraint
           WHERE conrelid = 'continuous.extrema'::regclass
             AND contype = 'f'
         ) AS has_extrema_foreign_keys,
         EXISTS (
           SELECT 1 FROM pg_class
           WHERE oid = 'continuous.extrema'::regclass
             AND relrowsecurity AND relforcerowsecurity
         ) AS has_extrema_row_security,
         (
           SELECT role.rolname = 'admin'
           FROM pg_class table_info
           JOIN pg_roles role ON role.oid = table_info.relowner
           WHERE table_info.oid = 'continuous.extrema'::regclass
         ) AS extrema_owned_by_admin,
         (
           SELECT count(*) = 4
           FROM pg_class table_info
           JOIN pg_namespace ns ON ns.oid = table_info.relnamespace
           JOIN pg_roles role ON role.oid = table_info.relowner
           WHERE ns.nspname = 'continuous'
             AND table_info.relname IN (
               'extrema_period_types', 'extrema_conditions',
               'extrema_types', 'extrema_record_sources'
             )
             AND role.rolname = 'admin'
         ) AS extrema_lookups_owned_by_admin,
         (
           SELECT role.rolname = 'admin'
           FROM pg_class sequence_info
           JOIN pg_roles role ON role.oid = sequence_info.relowner
           WHERE sequence_info.oid = 'continuous.extrema_extremum_id_seq'::regclass
         ) AS extrema_sequence_owned_by_admin,
         to_regprocedure('public.begin_parameter_unit_change(integer,jsonb,jsonb,jsonb)')
           IS NOT NULL AS has_begin_function,
         EXISTS (
           SELECT 1 FROM pg_trigger
           WHERE tgrelid = 'public.parameters'::regclass
             AND tgname = 'prevent_used_parameter_unit_update'
             AND tgenabled <> 'D' AND NOT tgisinternal
         ) AS guard_enabled"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop("Patch 67 verification failed.")
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '67'
       WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(utils::packageVersion("AquaCache")))
    )
    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 67 applied successfully. Reviewed parameter unit migrations are logged and scoped to the admin role."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
