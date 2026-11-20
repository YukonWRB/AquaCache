# Patch 69 replaces mapping-set revisions with directly scoped current
# mappings. Existing audit events remain; revision tables and per-result run
# rows are removed. New runs snapshot configuration and link each result to its run.
# Whole-document sharing is resolved across all linked samples and locations.
patch_number <- 69L

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop("Patch 69 requires a postgres session.")
}
if (dbTransCheck(con)) {
  stop("A transaction is already in progress. Commit or roll it back first.")
}

last_patch <- DBI::dbGetQuery(
  con,
  "SELECT version FROM information.version_info
   WHERE item = 'Last patch number'"
)$version
if (length(last_patch) != 1L || last_patch != "68") {
  stop(
    "Patch 69 requires Patch 68; this database reports Patch ",
    paste(last_patch, collapse = ", "),
    "."
  )
}

required <- DBI::dbGetQuery(
  con,
  "SELECT
     to_regclass('discrete.import_sources') IS NOT NULL AS has_sources,
     to_regclass('discrete.import_profiles') IS NOT NULL AS has_profiles,
     to_regclass('discrete.import_mapping_sets') IS NOT NULL AS has_sets,
     to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS has_parameters,
     to_regclass('discrete.import_location_mappings') IS NOT NULL AS has_locations,
     to_regclass('discrete.import_result_flag_mappings') IS NOT NULL AS has_flags,
     to_regclass('discrete.import_runs') IS NOT NULL AS has_runs,
     to_regclass('discrete.import_run_rows') IS NOT NULL AS has_run_rows,
     to_regclass('discrete.results') IS NOT NULL AS has_results,
     to_regclass('discrete.samples') IS NOT NULL AS has_samples,
     to_regclass('discrete.sample_documents') IS NOT NULL AS has_sample_documents,
     to_regclass('public.locations') IS NOT NULL AS has_sample_locations,
     to_regclass('files.documents') IS NOT NULL AS has_documents,
     to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
     to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function,
     to_regprocedure('discrete.import_parameter_mapping_error(discrete.import_parameter_mappings)')
       IS NOT NULL AS has_mapping_validator"
)
if (!all(unlist(required[1, ], use.names = FALSE))) {
  stop(
    "Patch 69 requires the Patch 68 import, results, files, and audit schema."
  )
}

legacy_tables <- c(
  "import_mapping_sets_legacy",
  "import_parameter_mappings_legacy",
  "import_location_mappings_legacy",
  "import_result_flag_mappings_legacy"
)
already_migrated <- DBI::dbGetQuery(
  con,
  "SELECT to_regclass($1) IS NOT NULL AS exists",
  params = list(paste0("discrete.", legacy_tables[[1]]))
)$exists[[1]]
if (isTRUE(already_migrated)) {
  stop(
    "Patch 69 legacy mapping tables already exist; investigate partial migration."
  )
}

draft_count <- DBI::dbGetQuery(
  con,
  "SELECT count(*)::integer AS n
   FROM discrete.import_mapping_sets WHERE status = 'draft'"
)$n[[1]]
if (draft_count > 0L) {
  message(
    "Patch 69 will discard ",
    draft_count,
    " unpublished mapping scope(s) and retain the audit history. ",
    "Only the formerly published definitions become active."
  )
}

message(
  "Applying direct import-mapping schema to this database in one transaction."
)
DBI::dbExecute(con, "BEGIN")
active <- TRUE
tryCatch(
  {
    # Stage the old revision graph while direct rows and run snapshots are
    # copied. The temporary legacy tables are dropped before commit.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_mapping_sets
         RENAME TO import_mapping_sets_legacy"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
         RENAME TO import_parameter_mappings_legacy"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_location_mappings
         RENAME TO import_location_mappings_legacy"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
         RENAME TO import_result_flag_mappings_legacy"
    )
    for (table in c(
      "import_parameter_mappings",
      "import_location_mappings",
      "import_result_flag_mappings"
    )) {
      DBI::dbExecute(
        con,
        paste0(
          "ALTER TABLE discrete.",
          table,
          "_legacy RENAME CONSTRAINT ",
          table,
          "_unique TO ",
          table,
          "_legacy_unique"
        )
      )
    }

    # Remove revision-era row guards from the now-legacy tables. Their bodies
    # intentionally query the old relation names, which are reused below by
    # the directly scoped live tables.
    for (table in paste0(
      c(
        "import_mapping_sets",
        "import_parameter_mappings",
        "import_location_mappings",
        "import_result_flag_mappings"
      ),
      "_legacy"
    )) {
      old_guards <- DBI::dbGetQuery(
        con,
        "SELECT t.tgname
         FROM pg_trigger t
         JOIN pg_proc p ON p.oid = t.tgfoid
         WHERE t.tgrelid = to_regclass($1)
           AND NOT t.tgisinternal
           AND p.proname IN (
             'guard_import_mapping_row',
             'guard_import_mapping_set',
             'enforce_import_parameter_mapping_requirements'
           )",
        params = list(paste0("discrete.", table))
      )
      for (trigger_name in old_guards$tgname) {
        DBI::dbExecute(
          con,
          paste0(
            "DROP TRIGGER ",
            as.character(DBI::dbQuoteIdentifier(con, trigger_name)),
            " ON discrete.",
            table
          )
        )
      }
    }

    # Clone each old row shape, retaining typed target columns, checks,
    # generated keys, identity behavior, and secondary indexes. The set FK is
    # replaced by explicit source/profile scope columns below.
    mapping_specs <- list(
      list(table = "import_parameter_mappings", id = "import_mapping_id"),
      list(
        table = "import_location_mappings",
        id = "import_location_mapping_id"
      ),
      list(
        table = "import_result_flag_mappings",
        id = "import_result_flag_mapping_id"
      )
    )
    for (spec in mapping_specs) {
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TABLE discrete.",
          spec$table,
          " (LIKE discrete.",
          spec$table,
          "_legacy INCLUDING ALL)"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "ALTER TABLE discrete.",
          spec$table,
          " DROP COLUMN import_mapping_set_id CASCADE,
          ADD COLUMN import_source_id INTEGER NOT NULL,
          ADD COLUMN import_profile_id INTEGER"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "ALTER TABLE discrete.",
          spec$table,
          " ADD CONSTRAINT ",
          spec$table,
          "_source_fkey",
          " FOREIGN KEY (import_source_id)",
          " REFERENCES discrete.import_sources(import_source_id)",
          " ON UPDATE CASCADE ON DELETE RESTRICT,",
          " ADD CONSTRAINT ",
          spec$table,
          "_profile_source_fkey",
          " FOREIGN KEY (import_source_id, import_profile_id)",
          " REFERENCES discrete.import_profiles(import_source_id, import_profile_id)",
          " ON UPDATE CASCADE ON DELETE RESTRICT"
        )
      )
      DBI::dbExecute(
        con,
        paste0("ALTER TABLE discrete.", spec$table, " OWNER TO admin")
      )
    }

    # LIKE does not copy foreign keys. Recreate each target-catalogue FK from
    # the legacy table, excluding its former mapping-set parent FK.
    for (spec in mapping_specs) {
      legacy_name <- paste0("discrete.", spec$table, "_legacy")
      foreign_keys <- DBI::dbGetQuery(
        con,
        "SELECT conname, pg_get_constraintdef(oid) AS definition
         FROM pg_constraint
         WHERE conrelid = to_regclass($1)
           AND contype = 'f'
           AND confrelid <> to_regclass('discrete.import_mapping_sets_legacy')
         ORDER BY conname",
        params = list(legacy_name)
      )
      for (i in seq_len(nrow(foreign_keys))) {
        constraint_name <- as.character(DBI::dbQuoteIdentifier(
          con,
          foreign_keys$conname[[i]]
        ))
        DBI::dbExecute(
          con,
          paste0(
            "ALTER TABLE discrete.",
            spec$table,
            " ADD CONSTRAINT ",
            constraint_name,
            " ",
            foreign_keys$definition[[i]]
          )
        )
      }
    }

    # A NULL profile is the source-wide scope. NULLS NOT DISTINCT keeps one
    # unique rule per source/match key at both source and profile scope.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
       ADD CONSTRAINT import_parameter_mappings_unique
         UNIQUE NULLS NOT DISTINCT (
           import_source_id, import_profile_id, source_match
         )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_location_mappings
       ADD CONSTRAINT import_location_mappings_unique
         UNIQUE NULLS NOT DISTINCT (
           import_source_id, import_profile_id, source_location_key
         )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       ADD CONSTRAINT import_result_flag_mappings_unique
         UNIQUE NULLS NOT DISTINCT (
           import_source_id, import_profile_id,
           source_flag_column, source_flag_value
         )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX import_parameter_mappings_scope_idx
         ON discrete.import_parameter_mappings
           (import_source_id, import_profile_id, priority)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX import_location_mappings_scope_idx
         ON discrete.import_location_mappings
           (import_source_id, import_profile_id, priority)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX import_result_flag_mappings_scope_idx
         ON discrete.import_result_flag_mappings
           (import_source_id, import_profile_id, priority)"
    )

    # Copy each scope's formerly active definition. The original audit events
    # remain available after the revision tables are removed below.
    copy_published_mappings <- function(table, id_column) {
      legacy_table <- paste0(table, "_legacy")
      fields <- DBI::dbGetQuery(
        con,
        "SELECT column_name
         FROM information_schema.columns
         WHERE table_schema = 'discrete' AND table_name = $1
           AND is_generated = 'NEVER'
           AND column_name <> 'import_mapping_set_id'
         ORDER BY ordinal_position",
        params = list(legacy_table)
      )$column_name
      quote_fields <- vapply(
        fields,
        function(field) as.character(DBI::dbQuoteIdentifier(con, field)),
        character(1)
      )
      column_sql <- paste(quote_fields, collapse = ", ")
      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO discrete.",
          table,
          " (",
          column_sql,
          ", import_source_id, import_profile_id) ",
          "SELECT ",
          paste0("legacy.", quote_fields, collapse = ", "),
          ", scope.import_source_id, scope.import_profile_id ",
          "FROM discrete.",
          legacy_table,
          " legacy ",
          "JOIN discrete.import_mapping_sets_legacy scope ",
          "USING (import_mapping_set_id) ",
          "WHERE scope.status = 'published'"
        )
      )
      sequence <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_serial_sequence($1, $2) AS sequence_name",
        params = list(paste0("discrete.", table), id_column)
      )$sequence_name[[1]]
      maximum <- DBI::dbGetQuery(
        con,
        paste0("SELECT max(", id_column, ") AS max_id FROM discrete.", table)
      )$max_id[[1]]
      if (!is.na(sequence) && nzchar(sequence)) {
        if (is.na(maximum)) {
          DBI::dbGetQuery(
            con,
            "SELECT setval($1::regclass, 1, false)",
            params = list(sequence)
          )
        } else {
          DBI::dbGetQuery(
            con,
            "SELECT setval($1::regclass, $2, true)",
            params = list(sequence, as.numeric(maximum))
          )
        }
      }
    }
    for (spec in mapping_specs) {
      copy_published_mappings(spec$table, spec$id)
    }

    # Keep the audit events already recorded for the previous table names,
    # but remove their registry entries and avoid retaining revision tables.
    legacy_names <- c(
      "import_mapping_sets",
      "import_run_rows"
    )
    registry_params <- as.list(legacy_names)
    registry_placeholders <- paste0(
      "$",
      seq_along(registry_params),
      collapse = ","
    )
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM audit.table_registry
         WHERE schema_name = 'discrete' AND table_name IN (",
        registry_placeholders,
        ")"
      ),
      params = registry_params
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit.table_registry (
         schema_name, table_name, capture_mode, rationale,
         history_started_at, updated_at
       ) VALUES
         ('discrete', 'import_parameter_mappings',
          'generic_insert_update_delete',
          'Directly scoped live parameter mappings; run snapshots preserve the exact configuration used.',
          clock_timestamp(), clock_timestamp()),
         ('discrete', 'import_location_mappings',
          'generic_insert_update_delete',
          'Directly scoped live location mappings; run snapshots preserve the exact configuration used.',
          clock_timestamp(), clock_timestamp()),
         ('discrete', 'import_result_flag_mappings',
          'generic_insert_update_delete',
          'Directly scoped live result-symbol mappings; run snapshots preserve the exact configuration used.',
          clock_timestamp(), clock_timestamp())
       ON CONFLICT (schema_name, table_name) DO UPDATE
       SET capture_mode = EXCLUDED.capture_mode,
           rationale = EXCLUDED.rationale,
           updated_at = EXCLUDED.updated_at"
    )

    for (spec in mapping_specs) {
      table <- spec$table
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.",
          table,
          " FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER update_modify_time BEFORE UPDATE ON discrete.",
          table,
          " FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER audit_",
          table,
          "_trigger AFTER INSERT OR UPDATE OR DELETE ON discrete.",
          table,
          " FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "GRANT SELECT, INSERT, UPDATE, DELETE ON discrete.",
          table,
          " TO admin"
        )
      )
      DBI::dbExecute(
        con,
        paste0("GRANT SELECT ON discrete.", table, " TO public_reader")
      )
      sequence <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_serial_sequence($1, $2) AS sequence_name",
        params = list(paste0("discrete.", table), spec$id)
      )$sequence_name[[1]]
      if (!is.na(sequence) && nzchar(sequence)) {
        DBI::dbExecute(
          con,
          paste0("GRANT USAGE, SELECT ON SEQUENCE ", sequence, " TO admin")
        )
      }
      # LIKE does not retain ACLs. Preserve existing custom editor grants on
      # both the mapping relation and its replacement identity sequence.
      old_sequence <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_serial_sequence($1, $2) AS sequence_name",
        params = list(paste0("discrete.", table, "_legacy"), spec$id)
      )$sequence_name[[1]]
      objects <- list(list(
        old = paste0("discrete.", table, "_legacy"),
        new = paste0("discrete.", table),
        type = "TABLE"
      ))
      if (!is.na(sequence) && !is.na(old_sequence)) {
        objects <- c(
          objects,
          list(list(old = old_sequence, new = sequence, type = "SEQUENCE"))
        )
      }
      for (object in objects) {
        grants <- DBI::dbGetQuery(
          con,
          "SELECT CASE WHEN acl.grantee=0 THEN 'PUBLIC'
                       ELSE quote_ident(pg_get_userbyid(acl.grantee)) END AS grantee,
                  acl.privilege_type, acl.is_grantable
           FROM pg_class c, LATERAL aclexplode(c.relacl) acl
           WHERE c.oid = to_regclass($1)",
          params = list(object$old)
        )
        for (i in seq_len(nrow(grants))) {
          DBI::dbExecute(
            con,
            paste0(
              "GRANT ",
              grants$privilege_type[[i]],
              " ON ",
              object$type,
              " ",
              object$new,
              " TO ",
              grants$grantee[[i]],
              if (grants$is_grantable[[i]]) " WITH GRANT OPTION" else ""
            )
          )
        }
      }
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_runs
         DROP CONSTRAINT IF EXISTS import_runs_source_mapping_set_fkey,
         DROP CONSTRAINT IF EXISTS import_runs_profile_mapping_set_fkey,
         DROP COLUMN IF EXISTS source_mapping_set_id,
         DROP COLUMN IF EXISTS profile_mapping_set_id,
         DROP COLUMN IF EXISTS source_file_name,
         DROP COLUMN IF EXISTS source_file_hash,
         DROP COLUMN IF EXISTS source_file_size,
         ADD COLUMN configuration_snapshot JSONB NOT NULL DEFAULT '{}'::jsonb,
         ADD COLUMN configuration_snapshot_version SMALLINT NOT NULL DEFAULT 1,
         ADD COLUMN source_document_id INTEGER,
         ADD CONSTRAINT import_runs_configuration_snapshot_object
           CHECK (jsonb_typeof(configuration_snapshot) = 'object'),
         ADD CONSTRAINT import_runs_configuration_snapshot_version_positive
           CHECK (configuration_snapshot_version > 0),
         ADD CONSTRAINT import_runs_source_document_fkey
           FOREIGN KEY (source_document_id)
           REFERENCES files.documents(document_id)
           ON UPDATE CASCADE ON DELETE RESTRICT"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.import_runs.configuration_snapshot IS
       'Immutable effective parser, mapping, and per-upload option definition used by this run.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.import_runs.source_document_id IS
       'Exact uploaded source workbook document associated with this run.'"
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.guard_import_run_snapshot()
       RETURNS trigger LANGUAGE plpgsql AS $function$
       BEGIN
         IF NEW.configuration_snapshot IS DISTINCT FROM OLD.configuration_snapshot
            OR NEW.configuration_snapshot_version IS DISTINCT FROM OLD.configuration_snapshot_version
            OR NEW.source_document_id IS DISTINCT FROM OLD.source_document_id
            OR NEW.import_source_id IS DISTINCT FROM OLD.import_source_id
            OR NEW.import_profile_id IS DISTINCT FROM OLD.import_profile_id
            OR NEW.source_adapter_function IS DISTINCT FROM OLD.source_adapter_function
            OR NEW.adapter_version IS DISTINCT FROM OLD.adapter_version
            OR NEW.source_uri IS DISTINCT FROM OLD.source_uri
            OR NEW.created_by IS DISTINCT FROM OLD.created_by
            OR NEW.started_at IS DISTINCT FROM OLD.started_at
         THEN
           RAISE EXCEPTION 'Import-run configuration and source-file snapshots are immutable.';
         END IF;
         IF OLD.status IN ('committed', 'failed', 'cancelled') AND (
              NEW.status IS DISTINCT FROM OLD.status
              OR NEW.summary IS DISTINCT FROM OLD.summary
              OR NEW.validation_summary IS DISTINCT FROM OLD.validation_summary
              OR NEW.completed_at IS DISTINCT FROM OLD.completed_at
              OR NEW.committed_at IS DISTINCT FROM OLD.committed_at
         ) THEN
           RAISE EXCEPTION 'A completed import-run outcome is immutable; start a new run instead.';
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.guard_import_run_snapshot() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guard_import_run_snapshot
       BEFORE UPDATE ON discrete.import_runs
       FOR EACH ROW EXECUTE FUNCTION discrete.guard_import_run_snapshot()"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
         ADD COLUMN import_run_id INTEGER,
         ADD CONSTRAINT results_import_run_fkey
           FOREIGN KEY (import_run_id)
           REFERENCES discrete.import_runs(import_run_id)
           ON UPDATE CASCADE ON DELETE RESTRICT"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX results_import_run_idx
       ON discrete.results (import_run_id) WHERE import_run_id IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.import_run_id IS
       'Import run that first inserted this result; later changes remain in the result audit history.'"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.results result
       SET import_run_id = run_rows.import_run_id
       FROM (
         SELECT result_id, min(import_run_id) AS import_run_id
         FROM discrete.import_run_rows
         WHERE result_id IS NOT NULL
         GROUP BY result_id
         HAVING count(DISTINCT import_run_id) = 1
       ) run_rows
       WHERE result.result_id = run_rows.result_id"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.guard_result_import_run()
       RETURNS trigger LANGUAGE plpgsql AS $function$
       BEGIN
         IF NEW.import_run_id IS DISTINCT FROM OLD.import_run_id THEN
           RAISE EXCEPTION 'The import run that originally inserted a result is immutable.';
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.guard_result_import_run() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guard_result_import_run BEFORE UPDATE ON discrete.results
       FOR EACH ROW EXECUTE FUNCTION discrete.guard_result_import_run()"
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION files.guard_import_source_content()
       RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER
       SET search_path = pg_catalog, discrete AS $function$
       BEGIN
         IF (NEW.document IS DISTINCT FROM OLD.document
             OR NEW.format IS DISTINCT FROM OLD.format)
            AND EXISTS (SELECT 1 FROM discrete.import_runs
                        WHERE source_document_id = OLD.document_id)
         THEN
           RAISE EXCEPTION 'An archived import source file cannot be replaced. Upload a new document instead.';
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION files.guard_import_source_content() OWNER TO admin"
    )
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA discrete TO admin")
    DBI::dbExecute(con, "GRANT SELECT ON discrete.import_runs TO admin")
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guard_import_source_content
       BEFORE UPDATE OF document, format ON files.documents
       FOR EACH ROW EXECUTE FUNCTION files.guard_import_source_content()"
    )
    DBI::dbExecute(
      con,
      paste(
        readLines(
          system.file(
            "patches",
            "patch_69",
            "patch_69_import_run_visibility.sql",
            package = "AquaCache"
          ),
          warn = FALSE
        ),
        collapse = "\n"
      ),
      immediate = TRUE
    )

    DBI::dbExecute(
      con,
      paste(readLines(system.file(
        "patches", "patch_69", "patch_69_document_sharing.sql", package = "AquaCache"
      ), warn = FALSE), collapse = "\n"),
      immediate = TRUE
    )

    # Scope keys are the new concurrency boundary for direct edits and run
    # snapshots. Database-backed audit triggers retain each save's before/after.
    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.import_parameter_mapping_error(
         mapping_row discrete.import_parameter_mappings
       ) RETURNS TEXT
       LANGUAGE plpgsql STABLE SECURITY INVOKER
       SET search_path = pg_catalog, public, discrete
       AS $function$
       DECLARE
         requirement RECORD;
         missing_dimension TEXT;
         condition_key TEXT;
       BEGIN
         SELECT p.* INTO requirement
         FROM public.parameters p
         WHERE p.parameter_id = mapping_row.parameter_id;
         IF NOT FOUND THEN RETURN 'Parameter'; END IF;
         missing_dimension := CASE
           WHEN requirement.sample_fraction AND mapping_row.sample_fraction_id IS NULL
             THEN 'Sample fraction'
           WHEN requirement.reporting_basis AND mapping_row.reporting_basis_id IS NULL
             THEN 'Reporting basis'
           WHEN requirement.result_speciation AND mapping_row.result_speciation_id IS NULL
             THEN 'Chemical speciation'
           WHEN requirement.result_target AND mapping_row.result_target_id IS NULL
             THEN 'Analytical target'
           WHEN requirement.endpoint AND mapping_row.endpoint_mode IS NULL
             THEN 'Endpoint definition'
         END;
         IF mapping_row.matrix_state_id IS NULL
            AND public.get_unique_parameter_matrix_state_id(mapping_row.parameter_id) IS NULL
            AND requirement.analysis_conditions_required_keys_by_matrix <> '{}'::JSONB
         THEN
           missing_dimension := 'Matrix state';
         END IF;
         FOREACH condition_key IN ARRAY discrete.required_analysis_condition_keys(
           mapping_row.parameter_id, mapping_row.matrix_state_id
         ) LOOP
           IF NOT (mapping_row.analysis_conditions ? condition_key)
              OR mapping_row.analysis_conditions->condition_key = 'null'::JSONB
           THEN
             missing_dimension := 'Analysis condition ' || condition_key;
             EXIT;
           END IF;
         END LOOP;
         IF mapping_row.active AND (
           (mapping_row.reporting_basis_id IS NOT NULL AND NOT EXISTS (
             SELECT 1 FROM discrete.reporting_bases
             WHERE reporting_basis_id = mapping_row.reporting_basis_id AND active
           )) OR
           (mapping_row.result_speciation_id IS NOT NULL AND NOT EXISTS (
             SELECT 1 FROM discrete.result_speciations
             WHERE result_speciation_id = mapping_row.result_speciation_id AND active
           )) OR
           (mapping_row.result_target_id IS NOT NULL AND NOT EXISTS (
             SELECT 1 FROM discrete.result_targets
             WHERE result_target_id = mapping_row.result_target_id AND active
           ))
         ) THEN
           missing_dimension := COALESCE(
             missing_dimension,
             'Active chemistry catalogue selection'
           );
         END IF;
         RETURN missing_dimension;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.import_parameter_mapping_error(
         discrete.import_parameter_mappings
       ) OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION discrete.import_parameter_mapping_error(
         discrete.import_parameter_mappings
       ) TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.reject_incomplete_active_parameter_mapping()
       RETURNS trigger LANGUAGE plpgsql
       SET search_path = pg_catalog, discrete AS $function$
       DECLARE missing TEXT;
       BEGIN
         NEW.analysis_conditions := discrete.normalize_analysis_conditions(
           NEW.analysis_conditions
         );
         IF NEW.active THEN
           missing := discrete.import_parameter_mapping_error(NEW);
           IF missing IS NOT NULL THEN
             RAISE EXCEPTION USING ERRCODE = '23514',
               MESSAGE = 'Active import parameter mapping is incomplete: ' || missing;
           END IF;
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.reject_incomplete_active_parameter_mapping() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_active_parameter_mapping
       BEFORE INSERT OR UPDATE ON discrete.import_parameter_mappings
       FOR EACH ROW EXECUTE FUNCTION discrete.reject_incomplete_active_parameter_mapping()"
    )

    # New uploads no longer need revision IDs. Retain their old audit events,
    # but remove the historical tables after active definitions have moved.
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.import_parameter_mapping_error(
         discrete.import_parameter_mappings_legacy
       )"
    )
    DBI::dbExecute(
      con,
      "DROP TABLE discrete.import_parameter_mappings_legacy,
                  discrete.import_location_mappings_legacy,
                  discrete.import_result_flag_mappings_legacy,
                  discrete.import_mapping_sets_legacy"
    )
    DBI::dbExecute(con, "DROP TABLE discrete.import_run_rows")
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.guard_import_mapping_row()"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.guard_import_mapping_set()"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.enforce_import_parameter_mapping_requirements()"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'Last patch number'",
      params = list(as.character(patch_number))
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
      "Patch 69 applied: live mappings now have direct source/profile scope; ",
      "mapping-set tables and per-result run rows are removed; new runs snapshot ",
      "configuration and results link directly to their run."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
