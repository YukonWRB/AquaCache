# Patch 59 creates provider-neutral raw transmission storage, integrates
# adapter-driven operational history, and moves raster-series datetime
# metadata maintenance into database triggers. downloadNESDIS is the first
# adapter to archive and replay payloads through the shared storage and history
# contracts.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 59: creating provider-neutral durable transmission payload storage, enabling NESDIS replay, documenting adapter-driven import history, and installing raster-series metadata triggers. Changes are being made within a transaction, so an error will roll back the database."
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
         to_regclass(
           'public.locations_metadata_transmission_setups'
         ) IS NOT NULL AS has_transmission_setups,
         to_regclass(
           'continuous.transmission_import_runs'
         ) IS NOT NULL AS has_import_runs,
         to_regclass(
           'public.source_adapter_capabilities'
         ) IS NOT NULL AS has_adapter_capabilities,
         to_regclass(
           'discrete.sample_group_types'
         ) IS NOT NULL AS has_sample_group_types,
         to_regclass(
           'audit.table_registry'
         ) IS NOT NULL AS has_audit_registry,
         to_regprocedure(
           'audit.if_modified_func()'
         ) IS NOT NULL AS has_audit_function,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 59 requires the transmission, source-adapter, sample-group, and audit schema created by earlier patches."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "58") {
      stop("Patch 59 must be applied to a database at Patch 58.")
    }

    target_exists <- DBI::dbGetQuery(
      con,
      "SELECT to_regclass(
         'continuous.transmission_payloads'
       ) IS NOT NULL AS has_transmission_payloads"
    )$has_transmission_payloads
    if (isTRUE(target_exists)) {
      stop(
        "Patch 59 target table already exists. Investigate the partial migration before applying this patch."
      )
    }

    adapter_schema_json <- DBI::dbGetQuery(
      con,
      "SELECT argument_schema::text AS argument_schema
       FROM public.source_adapter_capabilities
       WHERE source_fx = 'downloadNESDIS'
         AND data_domain = 'continuous'"
    )$argument_schema
    if (length(adapter_schema_json) != 1L) {
      stop(
        "Patch 59 requires exactly one continuous downloadNESDIS capability."
      )
    }
    adapter_schema <- jsonlite::fromJSON(
      adapter_schema_json,
      simplifyVector = FALSE
    )
    argument_names <- vapply(
      adapter_schema$arguments,
      `[[`,
      character(1),
      "name"
    )
    if ("from_storage" %in% argument_names) {
      stop(
        "Patch 59 found from_storage in the downloadNESDIS argument schema before the payload table existed. Investigate the partial migration."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.transmission_payloads (
         transmission_payload_id BIGINT PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY,
         transmission_setup_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_transmission_setups(
             transmission_setup_id
           )
           ON DELETE RESTRICT ON UPDATE CASCADE,
         transmission_datetime TIMESTAMPTZ NOT NULL,
         payload_text TEXT NOT NULL,
         payload_md5 TEXT GENERATED ALWAYS AS (md5(payload_text)) STORED,
         payload_bytes BIGINT GENERATED ALWAYS AS (
           octet_length(payload_text)::bigint
         ) STORED,
         source_server TEXT,
         source_metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
         retrieved TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         CONSTRAINT transmission_payloads_identity_key UNIQUE (
           transmission_setup_id,
           transmission_datetime,
           payload_md5
         ),
         CONSTRAINT transmission_payloads_payload_not_empty CHECK (
           octet_length(payload_text) > 0
         ),
         CONSTRAINT transmission_payloads_metadata_object CHECK (
           jsonb_typeof(source_metadata) = 'object'
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.transmission_payloads OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.transmission_payloads IS
       'Provider-neutral, durable, indefinite archive of individual raw text transmissions captured before parsing. Overlapping retrievals are deduplicated by setup, transmission time, and payload hash. Adapters that implement the stored-replay contract can reparse these payloads without contacting their provider.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_payloads.payload_text IS
       'Original text of one transmission as received by AquaCache, including any provider header needed for replay. PostgreSQL may compress this value transparently using TOAST.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_payloads.transmission_datetime IS
       'UTC transmission time supplied by the source adapter. Replay windows select archived messages using this timestamp.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_payloads.source_server IS
       'Optional provider endpoint, server, gateway, or other source identifier reported by the adapter.'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX transmission_payloads_setup_datetime_idx
       ON continuous.transmission_payloads (
         transmission_setup_id,
         transmission_datetime,
         transmission_payload_id
       )"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.transmission_import_runs IS
       'Provider-neutral operational history for transmission-adapter invocations and direct route imports. Each row records the query window, retrieval mode, parser outcome, counts, and errors for one route. Successful and no-data live query windows provide the incremental retrieval cursor only after any delegated measurement write completes; stored and supplied replay runs remain audit history but do not advance that cursor.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_import_runs.measurements_parsed IS
       'Measurements mapped for this invocation. In timeseries-adapter mode this is the number returned for the selected timeseries.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_import_runs.measurements_inserted IS
       'Measurements inserted or upserted by the direct importer or delegated AquaCache workflow. A legitimate zero includes no-data, unchanged, or filtered results; source_metadata identifies delegated completion.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_import_runs.source_metadata IS
       'Provider and execution metadata. Standard keys include retrieval_mode (live, storage, or supplied), adapter_mode, adapter_timeseries_id, measurement_write_delegated, measurement_workflow, measurement_write_completed, and measurement_write_failed.'"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE continuous.transmission_payloads FROM PUBLIC"
    )
    database_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname
    reader_roles <- intersect(
      c("yg_reader_group", "yg_reader"),
      database_roles
    )
    editor_roles <- intersect(
      c("yg_editor_group", "yg_editor", "continuous_editor"),
      database_roles
    )
    for (role_name in reader_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON TABLE continuous.transmission_payloads TO %s",
          quoted_role
        )
      )
    }
    for (role_name in editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT, INSERT ON TABLE continuous.transmission_payloads TO %s",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT USAGE, SELECT ON SEQUENCE
             continuous.transmission_payloads_transmission_payload_id_seq
           TO %s",
          quoted_role
        )
      )
    }

    adapter_schema$arguments[[length(adapter_schema$arguments) + 1L]] <-
      sourceAdapterArgument(
        name = "from_storage",
        source = "runtime",
        help = paste0(
          "synchronize_continuous can set this to TRUE for an explicit ",
          "replay from continuous.transmission_payloads. Normal scheduled ",
          "imports leave it FALSE and archive each live LRGS transmission ",
          "before parsing."
        )
      )
    registerSourceAdapterArguments(
      con = con,
      source_fx = "downloadNESDIS",
      data_domain = "continuous",
      arguments = adapter_schema$arguments,
      schema_version = adapter_schema$schema_version
    )

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass(
           'continuous.transmission_payloads'
         ) IS NOT NULL AS has_payload_table,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid =
             'continuous.transmission_payloads'::regclass
             AND conname = 'transmission_payloads_identity_key'
         ) AS has_identity_constraint,
         EXISTS (
           SELECT 1
           FROM public.source_adapter_capabilities sac,
             jsonb_array_elements(
               sac.argument_schema -> 'arguments'
             ) argument
           WHERE sac.source_fx = 'downloadNESDIS'
             AND sac.data_domain = 'continuous'
             AND argument ->> 'name' = 'from_storage'
             AND argument ->> 'source' = 'runtime'
         ) AS has_replay_argument"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop("Patch 59 verification failed.")
    }

    # Patch 58 introduced this governed catalogue without an audit trigger.
    # Capture all future changes, including inserts because the table has no
    # row-level creation metadata from which they could be reconstructed.
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_sample_group_types_trigger
       AFTER INSERT OR UPDATE OR DELETE ON discrete.sample_group_types
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )

    # Register audit triggers introduced after Patch 55, plus the new
    # sample-group catalogue and the intentional raw-payload exclusion. The
    # Patch 56 triggers capture UPDATE and DELETE; row creation is available
    # from their source-table metadata.
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
           'public',
           'source_adapter_capabilities',
           'generic_update_delete',
           'Source-adapter capabilities define how import functions are configured and invoked; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'transmission_timeseries_mappings',
           'generic_update_delete',
           'Transmission mappings determine how provider payload fields populate continuous timeseries; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'timeseries_source_adapters',
           'generic_update_delete',
           'Source-adapter assignments control continuous fetch and synchronization behavior; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'sample_series_source_adapters',
           'generic_update_delete',
           'Source-adapter assignments control discrete sample-series import behavior; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'files',
           'image_series_source_adapters',
           'generic_update_delete',
           'Source-adapter assignments control image-series import behavior; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'spatial',
           'raster_series_source_adapters',
           'generic_update_delete',
           'Source-adapter assignments control raster-series import behavior; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'boreholes',
           'drill_methods',
           'generic_update_delete',
           'Drilling-method definitions affect the interpretation of borehole construction metadata; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'boreholes',
           'seal_materials',
           'generic_update_delete',
           'Seal-material definitions affect the interpretation of well construction metadata; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'boreholes',
           'screen_materials',
           'generic_update_delete',
           'Screen-material definitions affect the interpretation of well construction metadata; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'boreholes',
           'screen_types',
           'generic_update_delete',
           'Screen-type definitions affect the interpretation of well construction metadata; creation is reconstructed from row metadata.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'sample_group_types',
           'generic_insert_update_delete',
           'Sample group types affect the interpretation of blanks, controls, and associated environmental samples.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'transmission_payloads',
           'excluded_payload',
           'Transmission payloads are not expected to be altered in this database, and deletions are likely to be intentional once enough time has elapsed from original transmission ingestion to know that no historic reconstruction is necessary.',
           NULL,
           clock_timestamp()
         )
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

    audit_verification <- DBI::dbGetQuery(
      con,
      "WITH expected_registry (
         schema_name,
         table_name,
         capture_mode
       ) AS (
         VALUES
           ('public', 'source_adapter_capabilities', 'generic_update_delete'),
           ('continuous', 'transmission_timeseries_mappings', 'generic_update_delete'),
           ('continuous', 'timeseries_source_adapters', 'generic_update_delete'),
           ('discrete', 'sample_series_source_adapters', 'generic_update_delete'),
           ('files', 'image_series_source_adapters', 'generic_update_delete'),
           ('spatial', 'raster_series_source_adapters', 'generic_update_delete'),
           ('boreholes', 'drill_methods', 'generic_update_delete'),
           ('boreholes', 'seal_materials', 'generic_update_delete'),
           ('boreholes', 'screen_materials', 'generic_update_delete'),
           ('boreholes', 'screen_types', 'generic_update_delete'),
           ('discrete', 'sample_group_types', 'generic_insert_update_delete'),
           ('continuous', 'transmission_payloads', 'excluded_payload')
       ), audit_triggers AS (
         SELECT DISTINCT
           table_namespace.nspname AS schema_name,
           table_class.relname AS table_name
         FROM pg_trigger trg
         JOIN pg_class table_class
           ON table_class.oid = trg.tgrelid
         JOIN pg_namespace table_namespace
           ON table_namespace.oid = table_class.relnamespace
         JOIN pg_proc trigger_function
           ON trigger_function.oid = trg.tgfoid
         JOIN pg_namespace function_namespace
           ON function_namespace.oid = trigger_function.pronamespace
         WHERE NOT trg.tgisinternal
           AND function_namespace.nspname = 'audit'
       )
       SELECT
         (
           SELECT count(*) = 12
           FROM expected_registry expected
           JOIN audit.table_registry registry
             USING (schema_name, table_name, capture_mode)
         ) AS has_expected_registry,
         EXISTS (
           SELECT 1
           FROM pg_trigger trg
           JOIN pg_class table_class
             ON table_class.oid = trg.tgrelid
           JOIN pg_namespace table_namespace
             ON table_namespace.oid = table_class.relnamespace
           JOIN pg_proc trigger_function
             ON trigger_function.oid = trg.tgfoid
           JOIN pg_namespace function_namespace
             ON function_namespace.oid = trigger_function.pronamespace
           WHERE NOT trg.tgisinternal
             AND trg.tgname = 'audit_sample_group_types_trigger'
             AND table_namespace.nspname = 'discrete'
             AND table_class.relname = 'sample_group_types'
             AND function_namespace.nspname = 'audit'
             AND trigger_function.proname = 'if_modified_func'
             AND (trg.tgtype & 1) = 1
             AND (trg.tgtype & 2) = 0
             AND (trg.tgtype & 4) = 4
             AND (trg.tgtype & 8) = 8
             AND (trg.tgtype & 16) = 16
             AND (trg.tgtype & 64) = 0
         ) AS has_sample_group_types_audit,
         -- Scoped to the tables THIS patch registers. The unscoped form
         -- asserted that every table carrying an audit trigger has a row in
         -- audit.table_registry; that invariant has never held. On the branch
         -- test fixture at patch 58 there are 103 pre-existing audited tables
         -- with no registry row - public.locations, discrete.samples,
         -- continuous.timeseries among them - so the check failed on any
         -- database. audit.table_registry is not created by any patch in the
         -- 53-59 range, and patches 55, 57 and 59 each register only the tables
         -- they themselves add.
         NOT EXISTS (
           SELECT 1
           FROM audit_triggers trg
           JOIN expected_registry expected
             USING (schema_name, table_name)
           LEFT JOIN audit.table_registry registry
             USING (schema_name, table_name)
           WHERE registry.schema_name IS NULL
         ) AS all_audit_triggers_registered,
         NOT EXISTS (
           SELECT 1
           FROM expected_registry expected
           JOIN audit.table_registry registry
             USING (schema_name, table_name)
           LEFT JOIN audit_triggers trg
             USING (schema_name, table_name)
           WHERE registry.capture_mode NOT LIKE 'excluded_%'
             AND trg.schema_name IS NULL
         ) AS all_registered_audits_have_triggers"
    )
    if (!all(unlist(audit_verification[1, ], use.names = FALSE))) {
      failed_audit_checks <- names(audit_verification)[
        !vapply(audit_verification[1, ], isTRUE, logical(1))
      ]
      stop(
        "Patch 59 audit registry verification failed: ",
        paste(failed_audit_checks, collapse = ", "),
        "."
      )
    }

    # raster_series_index datetime metadata is derived from
    # rasters_reference. Statement-level triggers keep it correct for bulk
    # appends while exact refreshes cover updates, deletes, and this backfill.
    raster_metadata_patch <- system.file(
      "patches",
      "patch_59_raster_series_metadata.R",
      package = "AquaCache"
    )
    if (!nzchar(raster_metadata_patch)) {
      stop("Patch 59 raster-series metadata helper was not installed.")
    }
    sys.source(raster_metadata_patch, envir = environment())
    install_raster_series_metadata_triggers(con)

    # Update the language around approval_type_code 'A' to be less likely to be confused with grades
    DBI::dbExecute(
      con,
      "UPDATE public.approval_types SET approval_type_description = 'Reviewed, final', approval_type_description_fr = 'Examiné, finales' WHERE approval_type_code = 'A'"
    )

    # Add qualifier for instrument error
    DBI::dbExecute(
      con,
      "INSERT INTO public.qualifier_types (qualifier_type_code, qualifier_type_description, qualifier_type_description_fr, color_code) VALUES ('ERROR', 'Instrument error', 'Erreur d''instrument', '#fe3200') ON CONFLICT (qualifier_type_code) DO UPDATE SET qualifier_type_code = 'ERROR', qualifier_type_description = 'Instrument error', qualifier_type_description_fr = 'Erreur d''instrument', color_code = '#fe3200'"
    )

    # Drop unique key on locations.name_fr, so that users who don't speak French can input nothing and 'Traduction requise!' gets input instead.
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations DROP CONSTRAINT IF EXISTS locations_name_fr_key"
    )
    # Set a default of 'Traduction requise!'
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations ALTER COLUMN name_fr SET DEFAULT 'Traduction requise!'"
    )

    # Add name constraints to sub_locations
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations DROP CONSTRAINT IF EXISTS sub_locations_name_key"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations ADD CONSTRAINT sub_locations_name_key UNIQUE (sub_location_name)"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations ALTER COLUMN sub_location_name_fr SET DEFAULT 'Traduction requise!'"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '59'
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
      "Patch 59 applied successfully. Provider-neutral payload storage and adapter import history are ready, live NESDIS transmissions are archived for durable replay, and raster-series datetime metadata is maintained by database triggers."
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
