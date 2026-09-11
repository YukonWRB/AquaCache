# Patch 60 adds generic component-based discrete results, supports multiple
# qualifiers and observers for one sample, hardens observer identity, adds
# time-ranged continuous notes, and standardizes source-update protection
# across continuous and discrete data.
# discrete.results remains the canonical reportable result; result components
# and their versioned calculation configuration are normalized in child
# tables below it.
#
# Implementation goals:
# 1. Support component-built results for any parameter and aggregation type,
#    initially mean, median, min, max, sum, and weighted mean.
# 2. Keep one canonical discrete.results row per reportable parameter and have
#    PostgreSQL maintain it (not R!) from included result components. A canonical
#    result may be NULL only while an aggregation-aware transaction assembles it;
#    commit-time validation requires a calculable, non-NULL value.
# 3. Normalize sample qualifiers and observers as many-to-many associations;
#    migrate and then remove discrete.samples.sample_qualifier.
# 4. Make calculation choices explicit and reproducible with a calculation
#    version and validated JSON arguments for missing values, non-detects,
#    multipliers, and final rounding.
# 5. Preserve AquaCache audit, RLS, metadata-view, and privilege conventions,
#    and expose the new contract through discrete ingestion functions. Add new
#    tables to audit tracking.
# 6. Rename source-update protection consistently, remove the obsolete derived
#    daily flag, and add independent protection to continuous QC intervals.
# 7. Add auditable, visibility-aware notes that can overlap and apply to
#    explicit temporal ranges on a continuous timeseries.
# 8. Preserve result-specific laboratory identifiers and normalized grade and
#    approval metadata in the canonical result and broad metadata views.
#
# -------------------------------------------------------------------------------
# Outstanding release work:
# 1. Rename this file to 'patch_60.R' once finalized so it gets read by AquaConnect()!
# 2. Regenerate the checked-in AquaCache test database from create_test_DB()
#    after this patch is finalized, then replace the YGwater test database copy.
#
# Longer-term work:
# 1. Snow survey workbook creation and ingestion functions (in this package and
#    YGwater) currently work with the 'snow' database. These functions will need
#    to work on 'aquacache' to fully close out 'snowdb'. This will also allow NWT
#    to use the same snow survey forms if they choose to do so.
# 2. Point-in-time reconstruction using audit tables is currently implemented in
#    continuous plots (YGwater package). Let's implement that for discrete plots as
#    well when we're fairly certain that the schema won't change further.
#
# -------------------------------------------------------------------------------

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 60: adding generic result aggregations and components, multi-valued sample qualifiers, sample observers, time-ranged continuous notes, and database-maintained canonical results. Changes are being made within a transaction, so an error will roll back the database."
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
         to_regclass('discrete.samples') IS NOT NULL AS has_samples,
         to_regclass('discrete.results') IS NOT NULL AS has_results,
         to_regclass('discrete.import_profiles') IS NOT NULL AS has_import_profiles,
         to_regclass('public.qualifier_types') IS NOT NULL AS has_qualifier_types,
         to_regclass('public.grade_types') IS NOT NULL AS has_grade_types,
         to_regclass('public.approval_types') IS NOT NULL AS has_approval_types,
         to_regclass('instruments.observers') IS NOT NULL AS has_observers,
         to_regclass('discrete.samples_metadata_en') IS NOT NULL AS has_samples_metadata_en,
         to_regclass('discrete.samples_metadata_fr') IS NOT NULL AS has_samples_metadata_fr,
         to_regclass('discrete.results_metadata_en') IS NOT NULL AS has_results_metadata_en,
         to_regclass('discrete.results_metadata_fr') IS NOT NULL AS has_results_metadata_fr,
         to_regclass('continuous.measurements_continuous') IS NOT NULL AS has_measurements_continuous,
         to_regclass('continuous.measurements_calculated_daily') IS NOT NULL AS has_measurements_calculated_daily,
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.grades') IS NOT NULL AS has_grades,
         to_regclass('continuous.approvals') IS NOT NULL AS has_approvals,
         to_regclass('continuous.qualifiers') IS NOT NULL AS has_continuous_qualifiers,
         to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function,
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_modified_function,
         to_regprocedure('public.user_modified()') IS NOT NULL AS has_user_function,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 60 requires the continuous measurement and quality-control schema, discrete sample/result schema, qualifier and observer catalogues, metadata views, audit framework, timestamp/user triggers, and version table created by earlier patches."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "59") {
      stop("Patch 60 must be applied to a database at Patch 59.")
    }

    sample_qualifier_column <- DBI::dbGetQuery(
      con,
      "SELECT count(*) = 1 AS available
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'samples'
         AND column_name = 'sample_qualifier'"
    )$available[[1]]
    if (!isTRUE(sample_qualifier_column)) {
      stop(
        "Patch 60 requires discrete.samples.sample_qualifier so existing values can be migrated before the column is removed."
      )
    }

    legacy_no_update_columns <- DBI::dbGetQuery(
      con,
      "SELECT count(*) = 4 AS available
       FROM information_schema.columns
       WHERE (table_schema, table_name, column_name) IN (
         ('continuous', 'measurements_continuous', 'no_update'),
         ('continuous', 'measurements_calculated_daily', 'no_update'),
         ('discrete', 'samples', 'no_update'),
         ('discrete', 'results', 'no_update')
       )"
    )$available[[1]]
    if (!isTRUE(legacy_no_update_columns)) {
      stop(
        "Patch 60 requires the four legacy no_update columns so three can be renamed to no_source_update and the obsolete calculated-daily column can be removed."
      )
    }

    invalid_result_conditions <- DBI::dbGetQuery(
      con,
      "SELECT
         count(*) FILTER (
           WHERE result IS NULL AND result_condition IS NULL
         )::integer AS unexplained_null_results,
         count(*) FILTER (
           WHERE result_condition IN (1, 2)
             AND result_condition_value IS NULL
         )::integer AS missing_condition_values,
         count(*) FILTER (
           WHERE (result_condition IS NULL OR result_condition NOT IN (1, 2))
             AND result_condition_value IS NOT NULL
         )::integer AS unexpected_condition_values
       FROM discrete.results"
    )
    if (any(unlist(invalid_result_conditions[1, ], use.names = FALSE) > 0L)) {
      invalid_counts <- paste(
        paste0(
          names(invalid_result_conditions),
          "=",
          invalid_result_conditions[1, ]
        ),
        collapse = ", "
      )
      stop(
        "Patch 60 cannot enforce the canonical result invariant until existing results are repaired: ",
        invalid_counts,
        "."
      )
    }

    target_state <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.sample_qualifiers') IS NOT NULL AS has_sample_qualifiers,
         to_regclass('discrete.sample_observers') IS NOT NULL AS has_sample_observers,
         to_regclass('discrete.result_aggregation_types') IS NOT NULL AS has_result_aggregation_types,
         to_regclass('discrete.result_aggregations') IS NOT NULL AS has_result_aggregations,
         to_regclass('discrete.result_components') IS NOT NULL AS has_result_components,
         to_regclass('continuous.notes') IS NOT NULL AS has_continuous_notes,
         to_regclass('discrete.result_aggregation_summary') IS NOT NULL AS has_result_aggregation_summary,
         to_regclass('discrete.stale_result_aggregations') IS NOT NULL AS has_stale_result_aggregations,
         to_regprocedure('discrete.calculate_result_aggregation(integer)') IS NOT NULL AS has_calculation_function,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE column_name = 'no_source_update'
             AND (
               (table_schema = 'continuous' AND table_name IN (
                 'measurements_continuous',
                 'measurements_calculated_daily',
                 'grades',
                 'approvals',
                 'qualifiers'
               ))
               OR (table_schema = 'discrete' AND table_name IN (
                 'samples',
                 'results'
               ))
             )
         ) AS has_source_update_columns"
    )
    if (any(unlist(target_state[1, ], use.names = FALSE))) {
      stop(
        "Patch 60 found one or more target tables or columns already present. Investigate the partial migration before applying this patch."
      )
    }

    existing_result_metadata_columns <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'results'
         AND column_name IN (
           'lab_report_no',
           'lab_sample_no',
           'grade_type_id',
           'approval_type_id'
         )"
    )$column_name
    if (length(existing_result_metadata_columns)) {
      stop(
        "Patch 60 found result metadata columns already present: ",
        paste(existing_result_metadata_columns, collapse = ", "),
        ". Investigate the partial migration before applying this patch."
      )
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
         ADD COLUMN lab_report_no TEXT,
         ADD COLUMN lab_sample_no TEXT,
         ADD COLUMN grade_type_id INTEGER
           REFERENCES public.grade_types(grade_type_id)
           ON DELETE SET NULL ON UPDATE CASCADE,
         ADD COLUMN approval_type_id INTEGER
           REFERENCES public.approval_types(approval_type_id)
           ON DELETE SET NULL ON UPDATE CASCADE"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.lab_report_no IS
       'Laboratory report number associated with this result, if available.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.lab_sample_no IS
       'Laboratory sample number associated with this result, if available.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.grade_type_id IS
       'Optional result-level data grade from public.grade_types.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.approval_type_id IS
       'Optional result-level approval status from public.approval_types.'"
    )

    # The new relations and functions are owned by admin. Their foreign-key and
    # calculation paths reference these schemas while running with owner rights.
    DBI::dbExecute(
      con,
      "GRANT USAGE ON SCHEMA discrete, instruments, public TO admin"
    )

    observer_duplicate <- DBI::dbGetQuery(
      con,
      "SELECT
         observer_first,
         observer_last,
         organization,
         array_agg(observer_id ORDER BY observer_id) AS observer_ids
       FROM instruments.observers
       GROUP BY observer_first, observer_last, organization
       HAVING count(*) > 1
       LIMIT 1"
    )
    if (nrow(observer_duplicate)) {
      stop(
        "Patch 60 found duplicate observer identities. Resolve observer IDs ",
        paste(observer_duplicate$observer_ids[[1]], collapse = ", "),
        " before applying this patch."
      )
    }

    metadata_view_names <- c(
      "samples_metadata_en",
      "samples_metadata_fr",
      "results_metadata_en",
      "results_metadata_fr"
    )
    metadata_view_definitions <- stats::setNames(
      vapply(
        metadata_view_names,
        function(view_name) {
          DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT pg_get_viewdef('discrete.%s'::regclass, true) AS definition",
              view_name
            )
          )$definition[[1]]
        },
        character(1)
      ),
      metadata_view_names
    )
    metadata_view_definitions <- gsub(
      "no_update",
      "no_source_update",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_privileges <- DBI::dbGetQuery(
      con,
      "SELECT table_name, grantee, privilege_type
       FROM information_schema.role_table_grants
       WHERE table_schema = 'discrete'
         AND table_name IN (
           'samples_metadata_en',
           'samples_metadata_fr',
           'results_metadata_en',
           'results_metadata_fr'
         )
       ORDER BY table_name, grantee, privilege_type"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.observers
       DROP CONSTRAINT IF EXISTS observers_unique"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.observers
       ADD CONSTRAINT observers_unique
       UNIQUE NULLS NOT DISTINCT (
         observer_first,
         observer_last,
         organization
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
         DROP CONSTRAINT chk_result_condition_value,
         ADD CONSTRAINT chk_result_condition_value CHECK (
           (
             result_condition IN (1, 2)
             AND result_condition_value IS NOT NULL
           ) OR (
             (result_condition IS NULL OR result_condition NOT IN (1, 2))
             AND result_condition_value IS NULL
           )
         )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON CONSTRAINT observers_unique
       ON instruments.observers IS
       'Prevents duplicate people even if an observer identity field becomes nullable in a future schema version.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.sample_qualifiers (
         sample_id INTEGER NOT NULL
           REFERENCES discrete.samples(sample_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         qualifier_type_id INTEGER NOT NULL
           REFERENCES public.qualifier_types(qualifier_type_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         note TEXT,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         PRIMARY KEY (sample_id, qualifier_type_id)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_qualifiers OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX sample_qualifiers_type_idx
       ON discrete.sample_qualifiers (qualifier_type_id, sample_id)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_qualifiers IS
       'Many-to-many association between discrete samples and all applicable data qualifiers.'"
    )
    legacy_sample_qualifier_count <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::integer AS n
       FROM discrete.samples
       WHERE sample_qualifier IS NOT NULL"
    )$n[[1]]
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.sample_qualifiers (
         sample_id,
         qualifier_type_id
       )
       SELECT sample_id, sample_qualifier
       FROM discrete.samples
       WHERE sample_qualifier IS NOT NULL"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.sample_observers (
         sample_id INTEGER NOT NULL
           REFERENCES discrete.samples(sample_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         observer_id INTEGER NOT NULL
           REFERENCES instruments.observers(observer_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         observer_role TEXT NOT NULL DEFAULT 'sampler',
         note TEXT,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         PRIMARY KEY (sample_id, observer_id, observer_role),
         CONSTRAINT sample_observers_role_not_blank CHECK (
           btrim(observer_role) <> ''
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_observers OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX sample_observers_observer_idx
       ON discrete.sample_observers (observer_id, sample_id)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_observers IS
       'Associates people from instruments.observers with a discrete sample and records their role in collecting or documenting it. The observer catalogue remains shared by sampling, calibration, and maintenance workflows.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.notes (
         note_id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         note TEXT NOT NULL,
         start_dt TIMESTAMPTZ NOT NULL,
         end_dt TIMESTAMPTZ NOT NULL,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         modified TIMESTAMPTZ,
         no_source_update BOOLEAN NOT NULL DEFAULT FALSE,
         CONSTRAINT notes_text_not_blank CHECK (btrim(note) <> ''),
         CONSTRAINT notes_period_valid CHECK (start_dt <= end_dt)
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX notes_timeseries_period_idx
       ON continuous.notes (timeseries_id, start_dt, end_dt)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.notes IS
       'Free-text annotations that apply to explicit time ranges on a continuous timeseries. Different notes may overlap because independent observations can apply to the same data.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.notes.no_source_update IS
       'TRUE prevents source-adapter and source-synchronization workflows from modifying or deleting this note; direct user edits remain allowed.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.result_aggregation_types (
         result_aggregation_type_id INTEGER
           GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
         aggregation_type TEXT NOT NULL UNIQUE,
         description TEXT NOT NULL,
         requires_weight BOOLEAN NOT NULL DEFAULT FALSE,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT result_aggregation_types_code_not_blank CHECK (
           btrim(aggregation_type) <> ''
         ),
         CONSTRAINT result_aggregation_type_lower CHECK (
            aggregation_type = lower(btrim(aggregation_type))
            AND aggregation_type ~ '^[a-z][a-z0-9_]*$'
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_aggregation_types OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.result_aggregation_types (
         aggregation_type,
         description,
         requires_weight
       ) VALUES
         ('mean', 'Arithmetic mean of contributing component values.', FALSE),
         ('median', 'Median of contributing component values.', FALSE),
         ('min', 'Minimum contributing component value.', FALSE),
         ('max', 'Maximum contributing component value.', FALSE),
         ('sum', 'Sum of contributing component values.', FALSE),
         ('weighted_mean', 'Weighted mean of contributing component values.', TRUE)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.result_aggregation_types IS
       'Controlled aggregation algorithms supported for component-built discrete results. Codes are stable API values; inactive types remain interpretable historically.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.result_aggregations (
         result_id INTEGER PRIMARY KEY
           REFERENCES discrete.results(result_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         result_aggregation_type_id INTEGER NOT NULL
           REFERENCES discrete.result_aggregation_types(
             result_aggregation_type_id
           ) ON DELETE RESTRICT ON UPDATE CASCADE,
          calculation_version INTEGER NOT NULL DEFAULT 1,
          calculation_arguments JSONB NOT NULL DEFAULT '{}'::jsonb,
          expected_count INTEGER,
          note TEXT,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT result_aggregations_version_positive CHECK (
           calculation_version > 0
         ),
          CONSTRAINT result_aggregations_arguments_object CHECK (
            jsonb_typeof(calculation_arguments) = 'object'
          ),
          CONSTRAINT result_aggregations_expected_count_positive CHECK (
            expected_count > 0
          )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_aggregations OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX result_aggregations_type_idx
       ON discrete.result_aggregations (
         result_aggregation_type_id,
         result_id
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.result_aggregations IS
       'One row marks a canonical discrete result as component-built and records its aggregation algorithm, implementation version, validated calculation arguments, and optional expected observation count. Direct writers must insert the parent result, aggregation configuration, and components in one transaction after explicitly deferring the three validate_result_aggregation constraint triggers, refresh the canonical value, and set those constraints to IMMEDIATE before commit.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.result_aggregations.calculation_arguments IS
       'Version 1 accepts missing_values (ignore, propagate, error), non_detects (exclude, zero, condition_value, half_condition_value, error), multiplier (number), and rounding_digits (integer, applied last). Unknown keys are rejected.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.result_aggregations.expected_count IS
       'Optional number of component observations expected by the result protocol. NULL means the protocol has no fixed expected count. This is independent of whether recorded components are included in the aggregate.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.result_components (
         result_component_id INTEGER
           GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
         result_id INTEGER NOT NULL
           REFERENCES discrete.result_aggregations(result_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         observation_number INTEGER NOT NULL,
         observation_datetime TIMESTAMPTZ,
         result NUMERIC,
         result_condition INTEGER
           REFERENCES discrete.result_conditions(result_condition_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         result_condition_value NUMERIC,
         included_in_aggregate BOOLEAN NOT NULL DEFAULT TRUE,
         weight NUMERIC,
         note TEXT,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT result_components_number_positive CHECK (
           observation_number > 0
         ),
         CONSTRAINT result_components_result_condition CHECK (
           result_condition IS NULL OR result IS NULL
         ),
          CONSTRAINT result_components_condition_value CHECK (
            (
              result_condition IN (1, 2)
              AND result_condition_value IS NOT NULL
            ) OR (
              (result_condition IS NULL OR result_condition NOT IN (1, 2))
              AND result_condition_value IS NULL
            )
          ),
         CONSTRAINT result_components_weight_positive CHECK (
           weight IS NULL OR weight > 0
         ),
         CONSTRAINT result_components_exclusion_reason CHECK (
           included_in_aggregate OR NULLIF(btrim(note), '') IS NOT NULL
         ),
         CONSTRAINT result_components_observation_key UNIQUE (
           result_id,
           observation_number
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_components OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX result_components_result_idx
       ON discrete.result_components (result_id, result_component_id)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.result_components IS
       'Individual observations used to calculate one canonical discrete result. Parameter, matrix state, fraction, speciation, and other analytical identity are inherited through result_id.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.validate_result_aggregation_arguments()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         unknown_arguments TEXT[];
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM discrete.result_aggregation_types
           WHERE result_aggregation_type_id =
             NEW.result_aggregation_type_id
             AND active
         ) THEN
           RAISE EXCEPTION
             'New result aggregations require an active aggregation type.';
         END IF;
         IF NEW.calculation_version <> 1 THEN
           RAISE EXCEPTION
             'Unsupported result aggregation calculation_version: %.',
             NEW.calculation_version;
         END IF;

         SELECT array_agg(key ORDER BY key)
         INTO unknown_arguments
         FROM jsonb_object_keys(NEW.calculation_arguments) AS key
         WHERE key NOT IN (
           'missing_values',
           'non_detects',
           'multiplier',
           'rounding_digits'
         );
         IF unknown_arguments IS NOT NULL THEN
           RAISE EXCEPTION
             'Unknown result aggregation calculation argument(s): %.',
             array_to_string(unknown_arguments, ', ');
         END IF;

         IF NEW.calculation_arguments ? 'missing_values' AND (
           jsonb_typeof(NEW.calculation_arguments -> 'missing_values') <>
             'string'
           OR NEW.calculation_arguments ->> 'missing_values' NOT IN (
             'ignore', 'propagate', 'error'
           )
         ) THEN
           RAISE EXCEPTION
             'calculation_arguments.missing_values must be ignore, propagate, or error.';
         END IF;
         IF NEW.calculation_arguments ? 'non_detects' AND (
           jsonb_typeof(NEW.calculation_arguments -> 'non_detects') <>
             'string'
           OR NEW.calculation_arguments ->> 'non_detects' NOT IN (
             'exclude',
             'zero',
             'condition_value',
             'half_condition_value',
             'error'
           )
         ) THEN
           RAISE EXCEPTION
             'calculation_arguments.non_detects has an unsupported value.';
         END IF;
         IF NEW.calculation_arguments ? 'multiplier' AND
            jsonb_typeof(NEW.calculation_arguments -> 'multiplier') <>
              'number' THEN
           RAISE EXCEPTION
             'calculation_arguments.multiplier must be a number.';
         END IF;
         IF NEW.calculation_arguments ? 'rounding_digits' AND (
           jsonb_typeof(NEW.calculation_arguments -> 'rounding_digits') <>
             'number'
           OR (NEW.calculation_arguments ->> 'rounding_digits')::numeric <>
             trunc((NEW.calculation_arguments ->> 'rounding_digits')::numeric)
           OR abs((NEW.calculation_arguments ->> 'rounding_digits')::numeric) >
             1000
         ) THEN
           RAISE EXCEPTION
             'calculation_arguments.rounding_digits must be an integer between -1000 and 1000.';
         END IF;
         RETURN NEW;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_result_aggregation_arguments_trigger
       BEFORE INSERT OR UPDATE
       ON discrete.result_aggregations
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation_arguments()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.result_component_numeric_value(
         component_result NUMERIC,
         component_condition INTEGER,
         component_condition_value NUMERIC,
         calculation_arguments JSONB
       )
       RETURNS NUMERIC
       LANGUAGE plpgsql
       IMMUTABLE
       AS $$
       DECLARE
         non_detects TEXT := COALESCE(
           calculation_arguments ->> 'non_detects',
           'exclude'
         );
       BEGIN
         IF component_result IS NOT NULL THEN
           RETURN component_result;
         END IF;
         IF component_condition NOT IN (1, 4) OR component_condition IS NULL THEN
           RETURN NULL;
         END IF;
         IF non_detects = 'exclude' THEN
           RETURN NULL;
         ELSIF non_detects = 'zero' THEN
           RETURN 0;
         ELSIF non_detects = 'error' THEN
           RAISE EXCEPTION
             'An included non-detect requires an explicit calculation policy.';
         ELSIF component_condition_value IS NULL THEN
           RAISE EXCEPTION
             'The selected non-detect policy requires result_condition_value.';
         ELSIF non_detects = 'condition_value' THEN
           RETURN component_condition_value;
         ELSIF non_detects = 'half_condition_value' THEN
           RETURN component_condition_value / 2;
         END IF;
         RAISE EXCEPTION 'Unsupported non_detects calculation argument: %.', non_detects;
       END;
       $$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.calculate_result_aggregation(
         target_result_id INTEGER
       )
       RETURNS NUMERIC
       LANGUAGE plpgsql
       STABLE
       AS $$
       DECLARE
         target_type TEXT;
         target_requires_weight BOOLEAN;
         arguments JSONB;
         missing_values TEXT;
         component_count INTEGER;
         missing_count INTEGER;
         missing_weight_count INTEGER;
         calculated_value NUMERIC;
       BEGIN
         SELECT
           rat.aggregation_type,
           rat.requires_weight,
           ra.calculation_arguments
         INTO target_type, target_requires_weight, arguments
         FROM discrete.result_aggregations ra
         JOIN discrete.result_aggregation_types rat
           USING (result_aggregation_type_id)
         WHERE ra.result_id = target_result_id;
         IF NOT FOUND THEN
           RETURN NULL;
         END IF;

         missing_values := COALESCE(
           arguments ->> 'missing_values',
           'ignore'
         );
         WITH component_values AS (
           SELECT
             discrete.result_component_numeric_value(
               rc.result,
               rc.result_condition,
               rc.result_condition_value,
               arguments
             ) AS numeric_value,
             rc.weight,
             COALESCE(
               rc.result IS NULL
                 AND rc.result_condition IN (1, 4)
                 AND COALESCE(arguments ->> 'non_detects', 'exclude') =
                   'exclude',
               FALSE
             ) AS ignored_non_detect
           FROM discrete.result_components rc
           WHERE rc.result_id = target_result_id
             AND rc.included_in_aggregate
         )
         SELECT
           count(*)::integer,
           count(*) FILTER (
             WHERE numeric_value IS NULL AND NOT ignored_non_detect
           )::integer,
           count(*) FILTER (
             WHERE numeric_value IS NOT NULL AND weight IS NULL
           )::integer
         INTO component_count, missing_count, missing_weight_count
         FROM component_values;

         IF component_count = 0 THEN
           RETURN NULL;
         ELSIF missing_values = 'error' AND missing_count > 0 THEN
           RAISE EXCEPTION
             'Result % has included components without calculable numeric values.',
             target_result_id;
         ELSIF missing_values = 'propagate' AND missing_count > 0 THEN
           RETURN NULL;
         ELSIF target_requires_weight AND missing_weight_count > 0 THEN
           RAISE EXCEPTION
             'Weighted aggregation for result % requires a positive weight for every contributing component.',
             target_result_id;
         END IF;

         WITH component_values AS (
           SELECT
             discrete.result_component_numeric_value(
               rc.result,
               rc.result_condition,
               rc.result_condition_value,
               arguments
             ) AS numeric_value,
             rc.weight
           FROM discrete.result_components rc
           WHERE rc.result_id = target_result_id
             AND rc.included_in_aggregate
         )
         SELECT CASE target_type
           WHEN 'mean' THEN avg(numeric_value)
           WHEN 'median' THEN percentile_cont(0.5)
             WITHIN GROUP (ORDER BY numeric_value)
           WHEN 'min' THEN min(numeric_value)
           WHEN 'max' THEN max(numeric_value)
           WHEN 'sum' THEN sum(numeric_value)
           WHEN 'weighted_mean' THEN
             sum(numeric_value * weight) / NULLIF(sum(weight), 0)
           ELSE NULL
         END
         INTO calculated_value
         FROM component_values
         WHERE numeric_value IS NOT NULL;

         IF target_type NOT IN (
           'mean', 'median', 'min', 'max', 'sum', 'weighted_mean'
         ) THEN
           RAISE EXCEPTION 'Unsupported result aggregation type: %.', target_type;
         END IF;
         IF calculated_value IS NOT NULL THEN
           calculated_value := calculated_value * COALESCE(
             (arguments ->> 'multiplier')::numeric,
             1
           );
           IF arguments ? 'rounding_digits' THEN
             calculated_value := round(
               calculated_value,
               (arguments ->> 'rounding_digits')::integer
             );
           END IF;
         END IF;
         RETURN calculated_value;
       END;
       $$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.refresh_result_aggregation(
         target_result_id INTEGER
       )
       RETURNS VOID
       LANGUAGE plpgsql
       AS $$
       DECLARE
         calculated_value NUMERIC;
       BEGIN
         PERFORM 1
         FROM discrete.results
         WHERE result_id = target_result_id
         FOR UPDATE;
         IF NOT FOUND OR NOT EXISTS (
           SELECT 1
           FROM discrete.result_aggregations
           WHERE result_id = target_result_id
         ) THEN
           RETURN;
         END IF;

          calculated_value := discrete.calculate_result_aggregation(
            target_result_id
          );
          UPDATE discrete.results
          SET result = calculated_value,
              result_condition = NULL,
              result_condition_value = NULL
          WHERE result_id = target_result_id
            AND (
              result IS DISTINCT FROM calculated_value
              OR result_condition IS NOT NULL
              OR result_condition_value IS NOT NULL
            );
        END;
        $$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.refresh_result_aggregations(
         target_result_ids INTEGER[] DEFAULT NULL
       )
       RETURNS INTEGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         updated_count INTEGER;
       BEGIN
         PERFORM 1
         FROM discrete.results r
         JOIN discrete.result_aggregations ra USING (result_id)
         WHERE target_result_ids IS NULL
            OR r.result_id = ANY(target_result_ids)
         ORDER BY r.result_id
         FOR UPDATE OF r;

         WITH calculated AS (
           SELECT
             ra.result_id,
             discrete.calculate_result_aggregation(ra.result_id)
               AS calculated_value
           FROM discrete.result_aggregations ra
           WHERE target_result_ids IS NULL
              OR ra.result_id = ANY(target_result_ids)
         ), updated AS (
           UPDATE discrete.results r
           SET result = calculated.calculated_value,
               result_condition = NULL,
               result_condition_value = NULL
           FROM calculated
           WHERE r.result_id = calculated.result_id
             AND (
               r.result IS DISTINCT FROM calculated.calculated_value
               OR r.result_condition IS NOT NULL
               OR r.result_condition_value IS NOT NULL
             )
           RETURNING r.result_id
         )
         SELECT count(*)::integer
         INTO updated_count
         FROM updated;
         RETURN updated_count;
       END;
       $$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.refresh_result_aggregation_trigger()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF COALESCE(
           NULLIF(
             current_setting(
               'aquacache.defer_result_aggregation_refresh',
               TRUE
             ),
             ''
           ),
           'off'
         ) = 'on' THEN
           RETURN NULL;
         END IF;
         IF TG_OP = 'DELETE' THEN
           IF TG_TABLE_NAME = 'result_components' THEN
             PERFORM discrete.refresh_result_aggregation(OLD.result_id);
           END IF;
         ELSIF TG_OP = 'INSERT' THEN
           PERFORM discrete.refresh_result_aggregation(NEW.result_id);
         ELSE
           IF NEW.result_id IS DISTINCT FROM OLD.result_id THEN
             PERFORM discrete.refresh_result_aggregation(OLD.result_id);
           END IF;
           PERFORM discrete.refresh_result_aggregation(NEW.result_id);
         END IF;
         RETURN NULL;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION discrete.refresh_result_aggregations(integer[]) IS
       'Locks the requested parent results in result_id order, recalculates them in one set-based operation, updates only changed canonical values, and returns the number updated. A NULL array refreshes all configured results.'"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_result_aggregation_config_insert_trigger
       AFTER INSERT
       ON discrete.result_aggregations
       FOR EACH ROW
       EXECUTE FUNCTION discrete.refresh_result_aggregation_trigger()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_result_aggregation_config_update_trigger
       AFTER UPDATE OF
         result_id,
         result_aggregation_type_id,
         calculation_version,
         calculation_arguments
       ON discrete.result_aggregations
       FOR EACH ROW
       WHEN (
         OLD.result_id IS DISTINCT FROM NEW.result_id
         OR OLD.result_aggregation_type_id IS DISTINCT FROM
           NEW.result_aggregation_type_id
         OR OLD.calculation_version IS DISTINCT FROM NEW.calculation_version
         OR OLD.calculation_arguments IS DISTINCT FROM NEW.calculation_arguments
       )
       EXECUTE FUNCTION discrete.refresh_result_aggregation_trigger()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_result_aggregation_components_change_trigger
       AFTER INSERT OR DELETE
       ON discrete.result_components
       FOR EACH ROW
       EXECUTE FUNCTION discrete.refresh_result_aggregation_trigger()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_result_aggregation_components_update_trigger
       AFTER UPDATE OF
         result_id,
         result,
         result_condition,
         result_condition_value,
         included_in_aggregate,
         weight
       ON discrete.result_components
       FOR EACH ROW
       WHEN (
         OLD.result_id IS DISTINCT FROM NEW.result_id
         OR OLD.result IS DISTINCT FROM NEW.result
         OR OLD.result_condition IS DISTINCT FROM NEW.result_condition
         OR OLD.result_condition_value IS DISTINCT FROM
           NEW.result_condition_value
         OR OLD.included_in_aggregate IS DISTINCT FROM
           NEW.included_in_aggregate
         OR OLD.weight IS DISTINCT FROM NEW.weight
       )
       EXECUTE FUNCTION discrete.refresh_result_aggregation_trigger()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.validate_result_aggregation()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         target_result_id INTEGER;
         stored_result NUMERIC;
         stored_condition INTEGER;
          stored_condition_value NUMERIC;
          component_count INTEGER;
          expected_result NUMERIC;
          is_aggregated BOOLEAN;
        BEGIN
         target_result_id := CASE
           WHEN TG_OP = 'DELETE' THEN OLD.result_id
           ELSE NEW.result_id
         END;
         SELECT EXISTS (
           SELECT 1
           FROM discrete.result_aggregations
           WHERE result_id = target_result_id
         ) INTO is_aggregated;

         SELECT result, result_condition, result_condition_value
         INTO stored_result, stored_condition, stored_condition_value
         FROM discrete.results
         WHERE result_id = target_result_id;
         IF NOT FOUND THEN
           RETURN NULL;
         END IF;
         IF NOT is_aggregated THEN
           IF (stored_result IS NULL) = (stored_condition IS NULL) THEN
             RAISE EXCEPTION
               'Direct result % must have exactly one of result or result_condition.',
               target_result_id;
           END IF;
           RETURN NULL;
         END IF;

         SELECT count(*)::integer
         INTO component_count
         FROM discrete.result_components
         WHERE result_id = target_result_id;
         IF component_count = 0 THEN
           RAISE EXCEPTION
             'Aggregated result % must have at least one result component.',
             target_result_id;
         END IF;
         IF stored_condition IS NOT NULL OR stored_condition_value IS NOT NULL THEN
           RAISE EXCEPTION
             'Aggregated result % cannot also carry a result condition.',
             target_result_id;
         END IF;

         expected_result := discrete.calculate_result_aggregation(
           target_result_id
         );
         IF expected_result IS NULL THEN
           RAISE EXCEPTION
             'Aggregated result % must calculate to a non-NULL value.',
             target_result_id;
         END IF;
         IF stored_result IS DISTINCT FROM expected_result THEN
           RAISE EXCEPTION
             'Aggregated result % is %, but its components calculate to %.',
             target_result_id,
             stored_result,
             expected_result;
         END IF;
         RETURN NULL;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_result_aggregation_result_trigger
       AFTER INSERT OR UPDATE
       ON discrete.results
       DEFERRABLE INITIALLY IMMEDIATE
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_result_aggregation_config_trigger
       AFTER INSERT OR UPDATE OR DELETE
       ON discrete.result_aggregations
       DEFERRABLE INITIALLY IMMEDIATE
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_result_aggregation_components_trigger
       AFTER INSERT OR UPDATE OR DELETE
       ON discrete.result_components
       DEFERRABLE INITIALLY IMMEDIATE
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation()"
    )

    # ---------------------------------------------------------------------
    # The aggregate-to-direct conversion path, and the delete guard that it
    # makes safe.
    #
    # Deleting a result_aggregations row cascades to its components but leaves
    # discrete.results untouched, so it is possible to strip a calculated
    # result of its inputs while keeping the derived value - a result that
    # presents as measured when it was calculated from evidence that no longer
    # exists. Guarding the delete on its own is not enough, because
    # synchronize_discrete_sample_detail() legitimately has to remove an
    # aggregation: when incoming detail no longer aggregates a result, that
    # result must become a direct one.
    #
    # So the conversion is made explicit first, and only then is the raw delete
    # closed off. Sanctioned deletes announce themselves through a
    # transaction-local guard variable - the same idiom already used above by
    # aquacache.defer_result_aggregation_refresh, and by aquacache.audit_user
    # since patch 37.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.forbid_result_aggregation_delete()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         -- An administrative conversion is in progress. Ordinary roles cannot
         -- authorize themselves merely by setting the custom GUC.
         IF current_user IN ('admin', 'postgres')
            AND COALESCE(
              NULLIF(
                current_setting(
                  'aquacache.allow_result_aggregation_delete',
                  TRUE
                ),
                ''
              ),
              'off'
            ) = 'on' THEN
           RETURN OLD;
         END IF;
         -- A foreign-key cascade from discrete.results reaches this trigger
         -- from inside PostgreSQL's referential-action trigger. Detecting that
         -- nested execution avoids querying the FORCE-RLS parent while the
         -- cascade is running under the child table owner.
         IF pg_trigger_depth() > 1 THEN
           RETURN OLD;
         END IF;
         RAISE EXCEPTION
           'Cannot delete the result_aggregations row for result % directly, because that would leave a calculated result with no components. Delete the result itself, exclude components with included_in_aggregate = FALSE, or convert the result with discrete.convert_result_aggregation_to_direct().',
           OLD.result_id;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER forbid_result_aggregation_delete_trigger
       BEFORE DELETE
       ON discrete.result_aggregations
       FOR EACH ROW
       EXECUTE FUNCTION discrete.forbid_result_aggregation_delete()"
    )

    # The explicit administrative conversion. SECURITY INVOKER deliberately:
    # the admin maintenance session remains subject to the same FORCE RLS
    # policies as its surrounding synchronization transaction.
    #
    # The reason is preserved in the audit trail without touching the shared
    # audit function: it is written onto the aggregation row immediately before
    # the delete, so audit_result_aggregations_trigger captures it in new_data
    # of the UPDATE and again in original_data of the DELETE.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.convert_result_aggregation_to_direct(
         result_id INTEGER,
         conversion_mode TEXT,
         result NUMERIC DEFAULT NULL,
         result_condition INTEGER DEFAULT NULL,
         result_condition_value NUMERIC DEFAULT NULL,
         reason TEXT DEFAULT NULL
       )
       RETURNS VOID
       LANGUAGE plpgsql
       SECURITY INVOKER
       AS $$
       DECLARE
         v_result_id INTEGER :=
           convert_result_aggregation_to_direct.result_id;
         v_mode TEXT := lower(btrim(coalesce(
           convert_result_aggregation_to_direct.conversion_mode, '')));
         v_reason TEXT := btrim(coalesce(
           convert_result_aggregation_to_direct.reason, ''));
         v_result NUMERIC := convert_result_aggregation_to_direct.result;
         v_condition INTEGER :=
           convert_result_aggregation_to_direct.result_condition;
         v_condition_value NUMERIC :=
           convert_result_aggregation_to_direct.result_condition_value;
         v_calculated NUMERIC;
         v_calculated_condition INTEGER;
         v_calculated_condition_value NUMERIC;
         v_expected NUMERIC;
         v_caller_deferred BOOLEAN;
       BEGIN
         -- Conversion is an administrative maintenance operation. Keeping the
         -- function SECURITY INVOKER preserves the same RLS context used by
         -- the surrounding synchronization transaction.
         IF current_user NOT IN ('admin', 'postgres') THEN
           RAISE EXCEPTION
             'Role % may not convert result aggregations; use an admin connection.',
             current_user;
         END IF;

         -- 7. A nonblank reason is mandatory.
         IF v_reason = '' THEN
           RAISE EXCEPTION
             'A nonblank reason is required to convert result % from an aggregate to a direct result.',
             v_result_id;
         END IF;

         IF v_mode NOT IN ('preserve_calculated', 'replace') THEN
           RAISE EXCEPTION
             'conversion_mode must be preserve_calculated or replace, not %.',
             coalesce(convert_result_aggregation_to_direct.conversion_mode,
                      '<NULL>');
         END IF;

         -- 1. Lock the aggregation, result, and current components in that
         -- order. Component writers acquire a foreign-key lock on the
         -- aggregation before their refresh trigger locks the result, so using
         -- the same order here avoids a conversion/component-insert deadlock.
         PERFORM 1
           FROM discrete.result_aggregations ra
          WHERE ra.result_id = v_result_id
            FOR UPDATE;
         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Result % has no aggregation to convert; it is already a direct result.',
             v_result_id;
         END IF;

         SELECT r.result, r.result_condition, r.result_condition_value
           INTO v_calculated, v_calculated_condition,
                v_calculated_condition_value
           FROM discrete.results r
          WHERE r.result_id = v_result_id
            FOR UPDATE;
         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Result % does not exist, or is not visible to role %.',
             v_result_id, current_user;
         END IF;

         PERFORM 1
           FROM discrete.result_components rc
          WHERE rc.result_id = v_result_id
          ORDER BY rc.result_component_id
          FOR UPDATE;

         -- 4. Validate and assign the new direct-result state.
         IF v_mode = 'preserve_calculated' THEN
           IF convert_result_aggregation_to_direct.result IS NOT NULL
              OR convert_result_aggregation_to_direct.result_condition
                   IS NOT NULL
              OR convert_result_aggregation_to_direct.result_condition_value
                   IS NOT NULL THEN
             RAISE EXCEPTION
               'conversion_mode preserve_calculated keeps the calculated value and does not accept a supplied result for result %. Use replace instead.',
               v_result_id;
           END IF;
           v_expected := discrete.calculate_result_aggregation(v_result_id);
           IF v_expected IS NULL THEN
             RAISE EXCEPTION
               'Result % has no calculable aggregate value to preserve. Repair its components, or use conversion_mode replace with an explicit value.',
               v_result_id;
           END IF;
           IF v_calculated IS DISTINCT FROM v_expected
              OR v_calculated_condition IS NOT NULL
              OR v_calculated_condition_value IS NOT NULL THEN
             RAISE EXCEPTION
               'Result % is stale or has an invalid aggregate condition. Refresh it before preserving the calculated value.',
               v_result_id;
           END IF;
           v_result := v_calculated;
           v_condition := NULL;
           v_condition_value := NULL;
         ELSE
           IF v_result IS NULL AND v_condition IS NULL THEN
             RAISE EXCEPTION
               'conversion_mode replace requires an explicit result or result_condition for result %.',
               v_result_id;
           END IF;
         END IF;

         -- 7 (continued). Record the reason on the aggregation row before it is
         -- deleted, so both audit rows carry it.
         UPDATE discrete.result_aggregations ra
            SET note = concat_ws(
                  chr(10),
                  NULLIF(btrim(ra.note), ''),
                  format(
                    'Converted to a direct result (%s) by %s: %s',
                    v_mode, current_user, v_reason
                  )
                )
          WHERE ra.result_id = v_result_id;

         -- 3. Defer the aggregation constraints: between the assignment and the
         -- delete the row is briefly both aggregate and direct.
         --
         -- A caller that has already deferred them owns the constraint mode for
         -- the whole transaction and may have other rows mid-flight - a newly
         -- inserted aggregate parent whose components are not written yet, for
         -- instance - that would fail if checked now. Setting IMMEDIATE fires
         -- every outstanding check, not only this function's, so when a caller
         -- has deferred, leave the mode alone and let it restore IMMEDIATE when
         -- its own work is complete. set_result_aggregation_constraints()
         -- records the mode it set.
         v_caller_deferred := COALESCE(
           NULLIF(
             current_setting(
               'aquacache.result_aggregation_constraints',
               TRUE
             ),
             ''
           ),
           'immediate'
         ) = 'deferred';

         IF NOT v_caller_deferred THEN
           SET CONSTRAINTS
             discrete.validate_result_aggregation_result_trigger,
             discrete.validate_result_aggregation_config_trigger,
             discrete.validate_result_aggregation_components_trigger
           DEFERRED;
         END IF;

         UPDATE discrete.results r
            SET result = v_result,
                result_condition = v_condition,
                result_condition_value = v_condition_value
          WHERE r.result_id = v_result_id;

         -- 5. Delete the aggregation, cascading to its components.
         PERFORM set_config(
           'aquacache.allow_result_aggregation_delete', 'on', TRUE);
         DELETE FROM discrete.result_aggregations ra
          WHERE ra.result_id = v_result_id;
         PERFORM set_config(
           'aquacache.allow_result_aggregation_delete', 'off', TRUE);

         -- 6. Restore immediate constraints, which also validates the result
         -- now rather than deferring the failure to COMMIT. Only when this
         -- function deferred them itself - see the note at step 3. A caller
         -- that deferred them gets its conversion validated at its own
         -- restore, or failing that at COMMIT.
         IF NOT v_caller_deferred THEN
           SET CONSTRAINTS
             discrete.validate_result_aggregation_result_trigger,
             discrete.validate_result_aggregation_config_trigger,
             discrete.validate_result_aggregation_components_trigger
           IMMEDIATE;
         END IF;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION discrete.convert_result_aggregation_to_direct(
         INTEGER, TEXT, NUMERIC, INTEGER, NUMERIC, TEXT
       ) IS
       'Administrative, RLS-respecting conversion of a component-built result into a direct result, deleting its aggregation and components. preserve_calculated first verifies that the stored value matches the locked components; replace requires an explicitly supplied result or result condition. A nonblank reason is required and is preserved in the audit trail on discrete.result_aggregations. This is the only sanctioned way to remove an aggregation while keeping its result; a direct DELETE is refused by forbid_result_aggregation_delete_trigger.'"
    )

    component_function_signatures <- c(
      "validate_result_aggregation_arguments()",
      "result_component_numeric_value(numeric,integer,numeric,jsonb)",
      "calculate_result_aggregation(integer)",
      "refresh_result_aggregation(integer)",
      "refresh_result_aggregations(integer[])",
      "refresh_result_aggregation_trigger()",
      "validate_result_aggregation()",
      "forbid_result_aggregation_delete()",
      "convert_result_aggregation_to_direct(integer,text,numeric,integer,numeric,text)"
    )
    for (function_signature in component_function_signatures) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER FUNCTION discrete.%s OWNER TO admin",
          function_signature
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "REVOKE ALL ON FUNCTION discrete.%s FROM PUBLIC",
          function_signature
        )
      )
    }

    for (table_name in c(
      "sample_qualifiers",
      "sample_observers",
      "result_aggregation_types",
      "result_aggregations",
      "result_components"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER update_%s_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER update_%s_modified_by
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER audit_%s_trigger
           AFTER INSERT OR UPDATE OR DELETE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()",
          table_name,
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_notes_modified
       BEFORE UPDATE ON continuous.notes
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_notes_modified_by
       BEFORE UPDATE ON continuous.notes
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_notes_trigger
       AFTER INSERT OR UPDATE OR DELETE ON continuous.notes
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
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
           'discrete',
           'sample_qualifiers',
           'generic_insert_update_delete',
           'Sample qualifier membership affects quality interpretation and downstream filtering.',
           clock_timestamp(),
           clock_timestamp()
         ),
          (
            'discrete',
            'sample_observers',
           'generic_insert_update_delete',
           'Sample-observer attribution is provenance for field collection and documentation.',
           clock_timestamp(),
           clock_timestamp()
         ),
          (
            'discrete',
            'result_aggregation_types',
            'generic_insert_update_delete',
            'Aggregation type definitions control how component-built results are calculated and interpreted.',
            clock_timestamp(),
            clock_timestamp()
          ),
          (
            'discrete',
            'result_aggregations',
            'generic_insert_update_delete',
            'Aggregation configuration determines the canonical reportable result and must remain reproducible.',
            clock_timestamp(),
            clock_timestamp()
          ),
          (
            'discrete',
            'result_components',
            'generic_insert_update_delete',
            'Component values and inclusion decisions determine the canonical reportable aggregate result.',
            clock_timestamp(),
            clock_timestamp()
          ),
          (
            'continuous',
            'notes',
            'generic_insert_update_delete',
            'Time-ranged notes provide user-authored context for interpreting continuous observations.',
            clock_timestamp(),
            clock_timestamp()
          )"
    )

    for (table_name in c(
      "sample_qualifiers",
      "sample_observers"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE discrete.%s ENABLE ROW LEVEL SECURITY",
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY %s_parent_sample_access
           ON discrete.%s
           FOR ALL
           USING (
             EXISTS (
               SELECT 1
               FROM discrete.samples s
               WHERE s.sample_id = %s.sample_id
             )
           )
           WITH CHECK (
             EXISTS (
               SELECT 1
               FROM discrete.samples s
               WHERE s.sample_id = %s.sample_id
             )
           )",
          table_name,
          table_name,
          table_name,
          table_name
        )
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_aggregations ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_components ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY result_aggregations_parent_access
       ON discrete.result_aggregations
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM discrete.results r
           WHERE r.result_id = result_aggregations.result_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM discrete.results r
           WHERE r.result_id = result_aggregations.result_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY result_components_parent_access
       ON discrete.result_components
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM discrete.results r
           WHERE r.result_id = result_components.result_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM discrete.results r
           WHERE r.result_id = result_components.result_id
         )
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.notes ENABLE ROW LEVEL SECURITY"
    )
    note_visibility_sql <- "EXISTS (
      SELECT 1
      FROM continuous.timeseries ts
      WHERE ts.timeseries_id = notes.timeseries_id
    )"
    note_policy_statements <- c(
      sprintf(
        "CREATE POLICY parent_visibility_restrict
         ON continuous.notes AS RESTRICTIVE FOR SELECT USING (%s)",
        note_visibility_sql
      ),
      sprintf(
        "CREATE POLICY parent_visibility_restrict_insert
         ON continuous.notes AS RESTRICTIVE FOR INSERT WITH CHECK (%s)",
        note_visibility_sql
      ),
      sprintf(
        "CREATE POLICY parent_visibility_restrict_update
         ON continuous.notes AS RESTRICTIVE FOR UPDATE
         USING (%s) WITH CHECK (%s)",
        note_visibility_sql,
        note_visibility_sql
      ),
      sprintf(
        "CREATE POLICY parent_visibility_restrict_delete
         ON continuous.notes AS RESTRICTIVE FOR DELETE USING (%s)",
        note_visibility_sql
      ),
      "CREATE POLICY parent_visibility_allow_select
       ON continuous.notes FOR SELECT USING (true)",
      "CREATE POLICY parent_visibility_allow_insert
       ON continuous.notes FOR INSERT WITH CHECK (true)",
      "CREATE POLICY parent_visibility_allow_update
       ON continuous.notes FOR UPDATE USING (true) WITH CHECK (true)",
      "CREATE POLICY parent_visibility_allow_delete
       ON continuous.notes FOR DELETE USING (true)"
    )
    for (statement in note_policy_statements) {
      DBI::dbExecute(con, statement)
    }

    # Apply the same owner-enforced visibility chain used by locations and
    # continuous timeseries throughout the application-facing discrete model.
    # A hidden location therefore hides its samples; a hidden sample hides its
    # results and normalized associations; and a hidden result hides its
    # aggregation configuration and components. FORCE also prevents an ordinary
    # non-bypass table owner from skipping these policies. Roles explicitly
    # granted PostgreSQL's BYPASSRLS attribute remain administrative exceptions.
    for (table_name in c(
      "samples",
      "results",
      "sample_documents",
      "sample_groups",
      "sample_group_members",
      "sample_qualifiers",
      "sample_observers",
      "result_aggregations",
      "result_components"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE discrete.%s FORCE ROW LEVEL SECURITY",
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE
         discrete.sample_qualifiers,
         discrete.sample_observers,
         discrete.result_aggregation_types,
         discrete.result_aggregations,
         discrete.result_components
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON SEQUENCE
         discrete.result_aggregation_types_result_aggregation_type_id_seq,
         discrete.result_components_result_component_id_seq
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE continuous.notes FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON SEQUENCE continuous.notes_note_id_seq FROM PUBLIC"
    )

    # Quote a role for generated GRANT statements while preserving PostgreSQL's
    # special unquoted PUBLIC grantee.
    quote_grantee <- function(role_name) {
      if (identical(role_name, "PUBLIC")) {
        return("PUBLIC")
      }
      as.character(DBI::dbQuoteIdentifier(con, role_name))
    }

    note_default_grantees <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT grants.grantee
       FROM information_schema.role_table_grants grants
       JOIN pg_tables tables
         ON tables.schemaname = grants.table_schema
        AND tables.tablename = grants.table_name
       WHERE grants.table_schema = 'continuous'
         AND grants.table_name = 'notes'
         AND grants.grantee <> tables.tableowner"
    )$grantee
    for (role_name in note_default_grantees) {
      DBI::dbExecute(
        con,
        sprintf(
          "REVOKE ALL ON TABLE continuous.notes FROM %s",
          quote_grantee(role_name)
        )
      )
    }

    continuous_note_privileges <- DBI::dbGetQuery(
      con,
      "SELECT grantee,
              string_agg(
                privilege_type,
                ', ' ORDER BY privilege_type
              ) AS privileges
       FROM information_schema.role_table_grants
       WHERE table_schema = 'continuous'
         AND table_name = 'grades'
         AND grantee <> (
           SELECT tableowner
           FROM pg_tables
           WHERE schemaname = 'continuous' AND tablename = 'grades'
         )
       GROUP BY grantee
       ORDER BY grantee"
    )
    for (i in seq_len(nrow(continuous_note_privileges))) {
      role_name <- continuous_note_privileges$grantee[[i]]
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT %s ON TABLE continuous.notes TO %s",
          continuous_note_privileges$privileges[[i]],
          quote_grantee(role_name)
        )
      )
      if (
        grepl(
          "(^|, )INSERT($|, )",
          continuous_note_privileges$privileges[[i]]
        )
      ) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
               continuous.notes_note_id_seq TO %s",
            quote_grantee(role_name)
          )
        )
      }
    }

    # CREATE TABLE can apply cluster-specific default privileges. Clear those
    # from the new child tables before copying the explicit grants from their
    # parents; otherwise a local default grant can broaden access and make the
    # exact-inheritance verification below fail.
    new_child_tables <- c(
      "sample_qualifiers",
      "sample_observers",
      "result_aggregations",
      "result_components"
    )
    inherited_default_grantees <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT grants.grantee
       FROM information_schema.role_table_grants grants
       JOIN pg_tables tables
         ON tables.schemaname = grants.table_schema
        AND tables.tablename = grants.table_name
       WHERE grants.table_schema = 'discrete'
         AND grants.table_name = ANY($1::text[])
         AND grants.grantee <> tables.tableowner",
      params = list(paste0("{", paste(new_child_tables, collapse = ","), "}"))
    )$grantee
    for (role_name in inherited_default_grantees) {
      DBI::dbExecute(
        con,
        sprintf(
          "REVOKE ALL ON TABLE %s FROM %s",
          paste0(
            "discrete.",
            new_child_tables,
            collapse = ", "
          ),
          quote_grantee(role_name)
        )
      )
    }

    # Copy the discrete source table's explicit DML grants to each new related
    # table. Returning the grant inventory also lets the sequence-grant logic
    # below derive which roles can insert rows.
    inherit_table_privileges <- function(source_table, target_tables) {
      role_privileges <- DBI::dbGetQuery(
        con,
        "SELECT
           grantee,
           string_agg(
             privilege_type,
             ', ' ORDER BY privilege_type
           ) AS privileges
         FROM information_schema.role_table_grants
         WHERE table_schema = 'discrete'
           AND table_name = $1
           AND privilege_type IN ('SELECT', 'INSERT', 'UPDATE', 'DELETE')
           AND grantee <> (
             SELECT tableowner
             FROM pg_tables
             WHERE schemaname = 'discrete' AND tablename = $1
           )
         GROUP BY grantee
         ORDER BY grantee",
        params = list(source_table)
      )
      for (i in seq_len(nrow(role_privileges))) {
        quoted_role <- quote_grantee(role_privileges$grantee[[i]])
        for (target_table in target_tables) {
          DBI::dbExecute(
            con,
            sprintf(
              "GRANT %s ON TABLE discrete.%s TO %s",
              role_privileges$privileges[[i]],
              target_table,
              quoted_role
            )
          )
        }
      }
      role_privileges
    }

    sample_privileges <- inherit_table_privileges(
      "samples",
      c("sample_qualifiers", "sample_observers")
    )
    result_privileges <- inherit_table_privileges(
      "results",
      c("result_aggregations", "result_components")
    )
    # An aggregation definition may be removed only by the administrative
    # aggregate-to-direct conversion operation (or by the cascade from its
    # parent result). Components retain the parent result's DELETE grants so
    # editors can replace observation detail inside a validated transaction.
    result_delete_roles <- unique(result_privileges$grantee[
      grepl("(^|, )DELETE($|, )", result_privileges$privileges)
    ])
    for (role_name in result_delete_roles) {
      DBI::dbExecute(
        con,
        sprintf(
          "REVOKE DELETE ON TABLE discrete.result_aggregations FROM %s",
          quote_grantee(role_name)
        )
      )
    }
    result_select_roles <- result_privileges$grantee[
      grepl("(^|, )SELECT($|, )", result_privileges$privileges)
    ]
    for (role_name in unique(result_select_roles)) {
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON TABLE discrete.result_aggregation_types TO %s",
          quote_grantee(role_name)
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          paste0(
            "GRANT EXECUTE ON FUNCTION ",
            "discrete.result_component_numeric_value(numeric,integer,numeric,jsonb), ",
            "discrete.calculate_result_aggregation(integer) TO %s"
          ),
          quote_grantee(role_name)
        )
      )
    }
    component_mutation_roles <- unique(
      result_privileges$grantee[
        grepl(
          "(^|, )(INSERT|UPDATE|DELETE)($|, )",
          result_privileges$privileges
        )
      ]
    )
    for (role_name in component_mutation_roles) {
      DBI::dbExecute(
        con,
        sprintf(
          paste0(
            "GRANT EXECUTE ON FUNCTION ",
            "discrete.result_component_numeric_value(numeric,integer,numeric,jsonb), ",
            "discrete.calculate_result_aggregation(integer), ",
            "discrete.refresh_result_aggregation(integer), ",
            "discrete.refresh_result_aggregations(integer[]) TO %s"
          ),
          quote_grantee(role_name)
        )
      )
    }
    insert_roles <- unique(c(
      result_privileges$grantee[
        grepl("(^|, )INSERT($|, )", result_privileges$privileges)
      ]
    ))
    for (role_name in insert_roles) {
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
             discrete.result_components_result_component_id_seq
           TO %s",
          quote_grantee(role_name)
        )
      )
    }

    # Verify effective privileges for ordinary roles, including inherited role
    # membership. Owners, members of either owner role, and superusers are
    # administrative exceptions with implicit rights that are not expected to
    # match between parent and child tables. PUBLIC ACLs are added separately
    # because PUBLIC is not a row in pg_roles.
    table_privilege_differences <- function(
      source_table,
      target_table,
      privilege_types = c("SELECT", "INSERT", "UPDATE", "DELETE")
    ) {
      DBI::dbGetQuery(
        con,
        "WITH requested_privileges AS (
           SELECT unnest($3::text[]) AS privilege_type
         ), relations AS (
           SELECT relation.oid, relation.relname, relation.relowner,
                  relation.relacl
           FROM pg_class relation
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'discrete'
             AND relation.relname IN ($1, $2)
         ), administrative_roles AS (
           SELECT roles.oid
           FROM pg_roles roles
           WHERE roles.rolsuper
              OR EXISTS (
                SELECT 1
                FROM relations relation
                WHERE pg_has_role(
                  roles.oid, relation.relowner, 'member'
                )
              )
         ), source_privileges AS (
           SELECT roles.rolname::text AS grantee,
                  requested.privilege_type
           FROM pg_roles roles
           CROSS JOIN requested_privileges requested
           WHERE roles.oid NOT IN (SELECT oid FROM administrative_roles)
             AND has_table_privilege(
             roles.oid,
             format('discrete.%I', $1),
             requested.privilege_type
           )
           UNION ALL
           SELECT 'PUBLIC', acl.privilege_type
           FROM relations relation
           CROSS JOIN LATERAL aclexplode(COALESCE(
             relation.relacl,
             acldefault('r', relation.relowner)
           )) acl
           JOIN requested_privileges requested
             ON requested.privilege_type = acl.privilege_type
           WHERE relation.relname = $1 AND acl.grantee = 0
         ), target_privileges AS (
           SELECT roles.rolname::text AS grantee,
                  requested.privilege_type
           FROM pg_roles roles
           CROSS JOIN requested_privileges requested
           WHERE roles.oid NOT IN (SELECT oid FROM administrative_roles)
             AND has_table_privilege(
             roles.oid,
             format('discrete.%I', $2),
             requested.privilege_type
           )
           UNION ALL
           SELECT 'PUBLIC', acl.privilege_type
           FROM relations relation
           CROSS JOIN LATERAL aclexplode(COALESCE(
             relation.relacl,
             acldefault('r', relation.relowner)
           )) acl
           JOIN requested_privileges requested
             ON requested.privilege_type = acl.privilege_type
           WHERE relation.relname = $2 AND acl.grantee = 0
         )
         SELECT 'missing' AS difference, missing.*
         FROM (
           SELECT * FROM source_privileges
           EXCEPT
           SELECT * FROM target_privileges
         ) missing
         UNION ALL
         SELECT 'unexpected' AS difference, unexpected.*
         FROM (
           SELECT * FROM target_privileges
           EXCEPT
           SELECT * FROM source_privileges
         ) unexpected
         ORDER BY difference, grantee, privilege_type",
        params = list(
          source_table,
          target_table,
          paste0("{", paste(privilege_types, collapse = ","), "}")
        )
      )
    }
    restricted_delete_differences <- function(target_table) {
      DBI::dbGetQuery(
        con,
        # Check explicit ACLs here, not effective rights. PostgreSQL's built-in
        # pg_write_all_data role has unavoidable effective DELETE rights on
        # every table; the trigger still rejects its direct deletion because
        # that role is neither admin nor postgres.
        "WITH target AS (
           SELECT relation.oid, relation.relowner, relation.relacl
           FROM pg_class relation
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'discrete'
             AND relation.relname = $1
         )
         SELECT 'unexpected' AS difference,
                CASE
                  WHEN acl.grantee = 0 THEN 'PUBLIC'
                  ELSE roles.rolname::text
                END AS grantee,
                'DELETE'::text AS privilege_type
         FROM target
         CROSS JOIN LATERAL aclexplode(target.relacl) acl
         LEFT JOIN pg_roles roles ON roles.oid = acl.grantee
         WHERE acl.privilege_type = 'DELETE'
           AND (
             acl.grantee = 0
             OR (
               NOT roles.rolsuper
               AND NOT pg_has_role(
                 roles.oid, target.relowner, 'member'
               )
             )
           )
         ORDER BY grantee",
        params = list(target_table)
      )
    }
    privilege_differences <- list(
      sample_qualifiers = table_privilege_differences(
        "samples",
        "sample_qualifiers"
      ),
      sample_observers = table_privilege_differences(
        "samples",
        "sample_observers"
      ),
      result_aggregations = rbind(
        table_privilege_differences(
          "results",
          "result_aggregations",
          c("SELECT", "INSERT", "UPDATE")
        ),
        restricted_delete_differences("result_aggregations")
      ),
      result_components = table_privilege_differences(
        "results",
        "result_components"
      )
    )
    privilege_checks <- !vapply(
      privilege_differences,
      nrow,
      integer(1)
    )
    if (!all(privilege_checks)) {
      failed_privileges <- do.call(
        rbind,
        lapply(
          names(privilege_differences)[!privilege_checks],
          function(table_name) {
            cbind(
              table_name = table_name,
              privilege_differences[[table_name]]
            )
          }
        )
      )
      stop(
        "Patch 60 table privilege verification failed:\n",
        paste(
          capture.output(print(failed_privileges, row.names = FALSE)),
          collapse = "\n"
        ),
        "."
      )
    }
    roles_have_function <- function(role_names, function_signatures) {
      all(vapply(
        role_names,
        function(role_name) {
          all(vapply(
            function_signatures,
            function(function_signature) {
              if (identical(role_name, "PUBLIC")) {
                return(isTRUE(DBI::dbGetQuery(
                  con,
                  "SELECT EXISTS (
                     SELECT 1
                     FROM pg_proc function_definition
                     CROSS JOIN LATERAL aclexplode(COALESCE(
                       function_definition.proacl,
                       acldefault('f', function_definition.proowner)
                     )) acl
                     WHERE function_definition.oid = to_regprocedure($1)
                       AND acl.grantee = 0
                       AND acl.privilege_type = 'EXECUTE'
                   ) AS allowed",
                  params = list(function_signature)
                )$allowed[[1]]))
              }
              isTRUE(DBI::dbGetQuery(
                con,
                "SELECT has_function_privilege($1, $2, 'EXECUTE') AS allowed",
                params = list(role_name, function_signature)
              )$allowed[[1]])
            },
            logical(1)
          ))
        },
        logical(1)
      ))
    }
    result_reader_functions <- c(
      "discrete.result_component_numeric_value(numeric,integer,numeric,jsonb)",
      "discrete.calculate_result_aggregation(integer)"
    )
    result_mutation_functions <- c(
      result_reader_functions,
      "discrete.refresh_result_aggregation(integer)",
      "discrete.refresh_result_aggregations(integer[])"
    )
    function_privilege_checks <- c(
      result_readers = roles_have_function(
        unique(result_select_roles),
        result_reader_functions
      ),
      result_mutators = roles_have_function(
        unique(component_mutation_roles),
        result_mutation_functions
      ),
      aggregation_conversion_admin = roles_have_function(
        "admin",
        paste0(
          "discrete.convert_result_aggregation_to_direct",
          "(integer,text,numeric,integer,numeric,text)"
        )
      ),
      component_sequence = all(vapply(
        insert_roles,
        function(role_name) {
          if (identical(role_name, "PUBLIC")) {
            return(isTRUE(DBI::dbGetQuery(
              con,
              "SELECT count(DISTINCT acl.privilege_type) = 3 AS allowed
               FROM pg_class sequence_definition
               CROSS JOIN LATERAL aclexplode(COALESCE(
                 sequence_definition.relacl,
                 acldefault('S', sequence_definition.relowner)
               )) acl
               WHERE sequence_definition.oid =
                 'discrete.result_components_result_component_id_seq'::regclass
                 AND acl.grantee = 0
                 AND acl.privilege_type IN ('USAGE', 'SELECT', 'UPDATE')"
            )$allowed[[1]]))
          }
          isTRUE(DBI::dbGetQuery(
            con,
            "SELECT
               has_sequence_privilege($1, $2, 'USAGE')
               AND has_sequence_privilege($1, $2, 'SELECT')
               AND has_sequence_privilege($1, $2, 'UPDATE') AS allowed",
            params = list(
              role_name,
              "discrete.result_components_result_component_id_seq"
            )
          )$allowed[[1]])
        },
        logical(1)
      ))
    )
    if (!all(function_privilege_checks)) {
      stop(
        "Patch 60 function or sequence privilege verification failed: ",
        paste(
          names(function_privilege_checks)[!function_privilege_checks],
          collapse = ", "
        ),
        "."
      )
    }

    # Rewrite a pg_get_viewdef() sample-metadata definition: remove the former
    # scalar qualifier join, add qualifier arrays, and preserve the
    # language-specific qualifier description. Post-transform checks below
    # reject unexpected source-view shapes before any view is replaced.
    transform_sample_metadata_view <- function(view_definition, french) {
      qualifier_pattern <- paste0(
        "(?s)s\\.sample_qualifier AS sample_qualifier_id,.*?",
        "s\\.owner AS owner_id,"
      )
      qualifier_columns <- if (french) {
        "qualifier_metadata.sample_qualifier_ids,
    qualifier_metadata.sample_qualifier_codes,
    qualifier_metadata.sample_qualifier_descriptions_fr,
    qualifier_metadata.sample_qualifier_notes,
    s.owner AS owner_id,"
      } else {
        "qualifier_metadata.sample_qualifier_ids,
    qualifier_metadata.sample_qualifier_codes,
    qualifier_metadata.sample_qualifier_descriptions,
    qualifier_metadata.sample_qualifier_notes,
    s.owner AS owner_id,"
      }
      updated_definition <- sub(
        qualifier_pattern,
        qualifier_columns,
        view_definition,
        perl = TRUE
      )
      updated_definition <- gsub(
        paste0(
          "[[:space:]]+LEFT JOIN (public\\.)?qualifier_types qt",
          " ON s\\.sample_qualifier = qt\\.qualifier_type_id"
        ),
        "",
        updated_definition,
        perl = TRUE
      )
      updated_definition <- sub(
        ";[[:space:]]*$",
        "",
        updated_definition
      )
      qualifier_description <- if (french) {
        "COALESCE(
               qt2.qualifier_type_description_fr,
               qt2.qualifier_type_description
             )"
      } else {
        "qt2.qualifier_type_description"
      }
      paste0(
        updated_definition,
        "
       LEFT JOIN LATERAL (
         SELECT
           array_agg(
             sq.qualifier_type_id ORDER BY sq.qualifier_type_id
           ) AS sample_qualifier_ids,
           array_agg(
             qt2.qualifier_type_code ORDER BY sq.qualifier_type_id
           ) AS sample_qualifier_codes,
           array_agg(
             ",
        qualifier_description,
        " ORDER BY sq.qualifier_type_id
           ) AS ",
        if (french) {
          "sample_qualifier_descriptions_fr"
        } else {
          "sample_qualifier_descriptions"
        },
        ",
           array_agg(
             sq.note ORDER BY sq.qualifier_type_id
           ) AS sample_qualifier_notes",
        "
          FROM discrete.sample_qualifiers sq
          JOIN public.qualifier_types qt2
            ON qt2.qualifier_type_id = sq.qualifier_type_id
          WHERE sq.sample_id = s.sample_id
       ) qualifier_metadata ON TRUE"
      )
    }

    # Extend a pg_get_viewdef() result-metadata definition with the normalized
    # sample associations and result-aggregation contract. This deliberately
    # builds on the transformed sample view so both metadata layers expose the
    # same qualifier representation without exposing observer identities.
    transform_result_metadata_view <- function(view_definition, french) {
      qualifier_pattern <- paste0(
        "(?s)sm\\.sample_qualifier_id,.*?",
        "sm\\.owner_id AS sample_owner_id,"
      )
      qualifier_columns <- if (french) {
        "sm.sample_qualifier_ids,
    sm.sample_qualifier_codes,
    sm.sample_qualifier_descriptions_fr,
    sm.sample_qualifier_notes,
    sm.owner_id AS sample_owner_id,"
      } else {
        "sm.sample_qualifier_ids,
    sm.sample_qualifier_codes,
    sm.sample_qualifier_descriptions,
    sm.sample_qualifier_notes,
    sm.owner_id AS sample_owner_id,"
      }
      updated_definition <- sub(
        qualifier_pattern,
        qualifier_columns,
        view_definition,
        perl = TRUE
      )
      updated_definition <- sub(
        "(r\\.result(?: AS [^,]+)?),",
        paste0(
          "\\1,\n",
          "    r.lab_report_no,\n",
          "    r.lab_sample_no,\n",
          "    r.grade_type_id AS result_grade_id,\n",
          "    result_grade.grade_type_code AS result_grade_code,\n",
          if (french) {
            paste0(
              "    COALESCE(result_grade.grade_type_description_fr, ",
              "result_grade.grade_type_description) AS ",
              "result_grade_description_fr,\n"
            )
          } else {
            "    result_grade.grade_type_description AS result_grade_description,\n"
          },
          "    r.approval_type_id AS result_approval_id,\n",
          "    result_approval.approval_type_code AS result_approval_code,\n",
          if (french) {
            paste0(
              "    COALESCE(result_approval.approval_type_description_fr, ",
              "result_approval.approval_type_description) AS ",
              "result_approval_description_fr,\n"
            )
          } else {
            paste0(
              "    result_approval.approval_type_description AS ",
              "result_approval_description,\n"
            )
          },
          "    aggregation_metadata.result_aggregation_type_id,\n",
          "    aggregation_metadata.aggregation_type,\n",
          "    aggregation_metadata.calculation_version,\n",
          "    aggregation_metadata.calculation_arguments,\n",
          "    aggregation_metadata.expected_count,"
        ),
        updated_definition,
        perl = TRUE
      )
      updated_definition <- sub(
        ";[[:space:]]*$",
        "",
        updated_definition
      )
      paste0(
        updated_definition,
        "
       LEFT JOIN public.grade_types result_grade
         ON result_grade.grade_type_id = r.grade_type_id
       LEFT JOIN public.approval_types result_approval
         ON result_approval.approval_type_id = r.approval_type_id
       LEFT JOIN LATERAL (
         SELECT
            ra.result_aggregation_type_id,
            rat.aggregation_type,
            ra.calculation_version,
            ra.calculation_arguments,
            ra.expected_count
         FROM discrete.result_aggregations ra
         JOIN discrete.result_aggregation_types rat
           USING (result_aggregation_type_id)
         WHERE ra.result_id = r.result_id
       ) aggregation_metadata ON TRUE"
      )
    }

    metadata_view_definitions[["samples_metadata_en"]] <-
      transform_sample_metadata_view(
        metadata_view_definitions[["samples_metadata_en"]],
        FALSE
      )
    metadata_view_definitions[["samples_metadata_fr"]] <-
      transform_sample_metadata_view(
        metadata_view_definitions[["samples_metadata_fr"]],
        TRUE
      )
    metadata_view_definitions[["results_metadata_en"]] <-
      transform_result_metadata_view(
        metadata_view_definitions[["results_metadata_en"]],
        FALSE
      )
    metadata_view_definitions[["results_metadata_fr"]] <-
      transform_result_metadata_view(
        metadata_view_definitions[["results_metadata_fr"]],
        TRUE
      )

    transformed_views_valid <- vapply(
      names(metadata_view_definitions),
      function(view_name) {
        view_definition <- metadata_view_definitions[[view_name]]
        !grepl("\\bsample_qualifier_id\\b", view_definition, perl = TRUE) &&
          !grepl("no_update", view_definition, fixed = TRUE) &&
          grepl("no_source_update", view_definition, fixed = TRUE) &&
          grepl("sample_qualifier_ids", view_definition, fixed = TRUE) &&
          !grepl("observer_", view_definition, fixed = TRUE) &&
          (!startsWith(view_name, "results_") ||
            (grepl("lab_report_no", view_definition, fixed = TRUE) &&
              grepl("lab_sample_no", view_definition, fixed = TRUE) &&
              grepl("result_grade_id", view_definition, fixed = TRUE) &&
              grepl("result_approval_id", view_definition, fixed = TRUE) &&
              grepl("aggregation_type", view_definition, fixed = TRUE) &&
              grepl(
                "calculation_arguments",
                view_definition,
                fixed = TRUE
              ) &&
              grepl("expected_count", view_definition, fixed = TRUE)))
      },
      logical(1)
    )
    if (!all(transformed_views_valid)) {
      stop(
        "Patch 60 could not safely transform metadata views: ",
        paste(
          names(transformed_views_valid)[!transformed_views_valid],
          collapse = ", "
        )
      )
    }

    DBI::dbExecute(
      con,
      "DROP VIEW discrete.results_metadata_en,
         discrete.results_metadata_fr,
         discrete.samples_metadata_en,
         discrete.samples_metadata_fr"
    )
    source_protection_schema <- c(
      "ALTER TABLE continuous.measurements_continuous
         RENAME COLUMN no_update TO no_source_update",
      "ALTER TABLE continuous.measurements_calculated_daily
         DROP COLUMN no_update",
      "ALTER TABLE discrete.samples
         RENAME COLUMN no_update TO no_source_update",
      "ALTER TABLE discrete.results
         RENAME COLUMN no_update TO no_source_update",
      "UPDATE continuous.measurements_continuous
       SET no_source_update = FALSE
       WHERE no_source_update IS NULL",
      "UPDATE discrete.samples
       SET no_source_update = FALSE
       WHERE no_source_update IS NULL",
      "UPDATE discrete.results
       SET no_source_update = FALSE
       WHERE no_source_update IS NULL",
      "ALTER TABLE continuous.measurements_continuous
         ALTER COLUMN no_source_update SET DEFAULT FALSE,
         ALTER COLUMN no_source_update SET NOT NULL",
      "ALTER TABLE discrete.samples
         ALTER COLUMN no_source_update SET DEFAULT FALSE,
         ALTER COLUMN no_source_update SET NOT NULL",
      "ALTER TABLE discrete.results
         ALTER COLUMN no_source_update SET DEFAULT FALSE,
         ALTER COLUMN no_source_update SET NOT NULL",
      "ALTER TABLE continuous.grades
         ADD COLUMN no_source_update BOOLEAN NOT NULL DEFAULT FALSE",
      "ALTER TABLE continuous.approvals
         ADD COLUMN no_source_update BOOLEAN NOT NULL DEFAULT FALSE",
      "ALTER TABLE continuous.qualifiers
         ADD COLUMN no_source_update BOOLEAN NOT NULL DEFAULT FALSE",
      "UPDATE discrete.import_profiles
       SET column_map = jsonb_set(
         column_map - 'no_update',
         '{no_source_update}',
         COALESCE(column_map -> 'no_source_update', column_map -> 'no_update'),
         TRUE
       )
       WHERE column_map ? 'no_update'",
      "UPDATE discrete.import_profiles
       SET defaults = jsonb_set(
         defaults - 'no_update',
         '{no_source_update}',
         COALESCE(defaults -> 'no_source_update', defaults -> 'no_update'),
         TRUE
       )
       WHERE defaults ? 'no_update'"
    )
    for (statement in source_protection_schema) {
      DBI::dbExecute(con, statement)
    }

    source_protection_comments <- c(
      "continuous.measurements_continuous" = "TRUE prevents source-adapter and source-synchronization workflows from replacing this measurement; direct user edits remain allowed.",
      "continuous.grades" = "TRUE prevents source-adapter and source-synchronization workflows from modifying or deleting this grade interval; direct user edits remain allowed.",
      "continuous.approvals" = "TRUE prevents source-adapter and source-synchronization workflows from modifying or deleting this approval interval; direct user edits remain allowed.",
      "continuous.qualifiers" = "TRUE prevents source-adapter and source-synchronization workflows from modifying or deleting this qualifier interval; direct user edits remain allowed.",
      "discrete.samples" = "TRUE prevents source-adapter and source-synchronization workflows from replacing this sample; direct user edits remain allowed.",
      "discrete.results" = "TRUE prevents source-adapter and source-synchronization workflows from replacing this result; direct user edits remain allowed."
    )
    for (table_name in names(source_protection_comments)) {
      DBI::dbExecute(
        con,
        sprintf(
          "COMMENT ON COLUMN %s.no_source_update IS %s",
          table_name,
          DBI::dbQuoteString(con, source_protection_comments[[table_name]])
        )
      )
    }
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.import_profiles.defaults IS
       'JSON object of profile-level defaults such as media_id, collection_method, sample_type, owner, contributor, laboratory, result_type, matrix_state_id, and no_source_update.'"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples DROP COLUMN sample_qualifier"
    )
    for (view_name in c(
      "samples_metadata_en",
      "samples_metadata_fr",
      "results_metadata_en",
      "results_metadata_fr"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE VIEW discrete.%s
           WITH (security_invoker = true, security_barrier = true)
           AS
           %s",
          view_name,
          metadata_view_definitions[[view_name]]
        )
      )
      DBI::dbExecute(
        con,
        sprintf("ALTER VIEW discrete.%s OWNER TO admin", view_name)
      )
    }
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_en IS
       'English-language view that flattens key discrete sample metadata, including all qualifier associations. Observer identities are deliberately excluded.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_fr IS
       'French-language view that flattens key discrete sample metadata, including all qualifier associations. Observer identities are deliberately excluded.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_metadata_en IS
       'English-language view that joins each discrete result to flattened sample metadata, result-level laboratory and quality metadata, and optional component-aggregation configuration.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_metadata_fr IS
       'French-language view that joins each discrete result to flattened sample metadata, result-level laboratory and quality metadata, and optional component-aggregation configuration.'"
    )
    for (i in seq_len(nrow(metadata_view_privileges))) {
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT %s ON discrete.%s TO %s",
          metadata_view_privileges$privilege_type[[i]],
          metadata_view_privileges$table_name[[i]],
          quote_grantee(metadata_view_privileges$grantee[[i]])
        )
      )
    }

    DBI::dbExecute(
      con,
      "CREATE VIEW discrete.result_aggregation_summary
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT
         r.result_id,
         r.sample_id,
         r.parameter_id,
         r.matrix_state_id,
         r.sample_fraction_id,
         r.result_speciation_id,
         ra.result_aggregation_type_id,
         rat.aggregation_type,
          ra.calculation_version,
          ra.calculation_arguments,
          ra.expected_count,
          r.result AS stored_result,
          calculation.calculated_result,
          calculation.calculated_result IS NOT NULL
            AND r.result IS NOT DISTINCT FROM calculation.calculated_result
            AS result_is_current,
          count(rc.result_component_id)::integer AS component_count,
          CASE
            WHEN ra.expected_count IS NULL THEN NULL
            ELSE GREATEST(
              ra.expected_count - count(rc.result_component_id)::integer,
              0
            )
          END AS missing_component_count,
          CASE
            WHEN ra.expected_count IS NULL THEN NULL
            ELSE count(rc.result_component_id) < ra.expected_count
          END AS has_component_shortfall,
         count(rc.result_component_id) FILTER (
           WHERE rc.included_in_aggregate
         )::integer AS included_component_count,
         count(rc.result_component_id) FILTER (
           WHERE NOT rc.included_in_aggregate
         )::integer AS excluded_component_count,
         count(rc.result_component_id) FILTER (
           WHERE rc.result IS NULL
         )::integer AS missing_or_conditioned_component_count,
         count(rc.result_component_id) FILTER (
           WHERE rc.result_condition IN (1, 4)
         )::integer AS non_detect_component_count,
         sum(rc.weight) FILTER (
           WHERE rc.included_in_aggregate
             AND component_value.numeric_value IS NOT NULL
         ) AS included_weight_sum,
         avg(rc.result) FILTER (
           WHERE rc.included_in_aggregate
         ) AS included_raw_mean,
         percentile_cont(0.5) WITHIN GROUP (
           ORDER BY rc.result
         ) FILTER (
           WHERE rc.included_in_aggregate
         ) AS included_raw_median,
         min(rc.result) FILTER (
           WHERE rc.included_in_aggregate
         ) AS included_raw_minimum,
         max(rc.result) FILTER (
           WHERE rc.included_in_aggregate
         ) AS included_raw_maximum,
         stddev_samp(rc.result) FILTER (
           WHERE rc.included_in_aggregate
         ) AS included_raw_standard_deviation,
         array_agg(rc.observation_number ORDER BY rc.observation_number)
           FILTER (WHERE NOT rc.included_in_aggregate)
           AS excluded_observation_numbers,
         count(rc.result_component_id) FILTER (
           WHERE rc.included_in_aggregate
             AND component_value.numeric_value IS NOT NULL
         )::integer AS contributing_component_count
       FROM discrete.results r
       JOIN discrete.result_aggregations ra
         ON ra.result_id = r.result_id
       JOIN discrete.result_aggregation_types rat
         USING (result_aggregation_type_id)
       LEFT JOIN discrete.result_components rc
         ON rc.result_id = r.result_id
       LEFT JOIN LATERAL (
         SELECT discrete.result_component_numeric_value(
           rc.result,
           rc.result_condition,
           rc.result_condition_value,
           ra.calculation_arguments
         ) AS numeric_value
       ) component_value ON TRUE
       LEFT JOIN LATERAL (
         SELECT discrete.calculate_result_aggregation(r.result_id)
           AS calculated_result
       ) calculation ON TRUE
       GROUP BY
         r.result_id,
         r.sample_id,
         r.parameter_id,
         r.matrix_state_id,
         r.sample_fraction_id,
         r.result_speciation_id,
         ra.result_aggregation_type_id,
         rat.aggregation_type,
          ra.calculation_version,
          ra.calculation_arguments,
          ra.expected_count,
          calculation.calculated_result"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW discrete.result_aggregation_summary OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.result_aggregation_summary IS
       'One row per component-built result with its calculation contract, expected and observed component counts, collection shortfall, stored/calculated agreement, inclusion and contribution counts, weights, and raw descriptive statistics.'"
    )
    DBI::dbExecute(
      con,
      "CREATE VIEW discrete.stale_result_aggregations
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT *
       FROM discrete.result_aggregation_summary
       WHERE NOT result_is_current"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW discrete.stale_result_aggregations OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.stale_result_aggregations IS
       'Component-built results whose stored canonical value is NULL, uncalculable, or different from the current calculation. Committed rows should never appear here.'"
    )
    select_roles <- unique(c(
      sample_privileges$grantee[
        grepl("(^|, )SELECT($|, )", sample_privileges$privileges)
      ],
      result_privileges$grantee[
        grepl("(^|, )SELECT($|, )", result_privileges$privileges)
      ]
    ))
    for (role_name in select_roles) {
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON discrete.result_aggregation_summary TO %s",
          quote_grantee(role_name)
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON discrete.stale_result_aggregations TO %s",
          quote_grantee(role_name)
        )
      )
    }

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.sample_qualifiers') IS NOT NULL
           AS has_sample_qualifiers,
         to_regclass('continuous.notes') IS NOT NULL
           AS has_continuous_notes,
         to_regclass('discrete.sample_observers') IS NOT NULL
           AS has_sample_observers,
         to_regclass('discrete.result_aggregation_types') IS NOT NULL
           AS has_result_aggregation_types,
         to_regclass('discrete.result_aggregations') IS NOT NULL
           AS has_result_aggregations,
         to_regclass('discrete.result_components') IS NOT NULL
           AS has_result_components,
         to_regclass('discrete.result_aggregation_summary') IS NOT NULL
           AS has_result_aggregation_summary,
         to_regclass('discrete.stale_result_aggregations') IS NOT NULL
           AS has_stale_result_aggregations,
         to_regprocedure('discrete.refresh_result_aggregations(integer[])')
           IS NOT NULL AS has_batch_refresh_function,
         to_regprocedure(
           'discrete.convert_result_aggregation_to_direct(integer,text,numeric,integer,numeric,text)'
         ) IS NOT NULL AS has_aggregation_conversion_function,
         EXISTS (
           SELECT 1
           FROM pg_trigger trigger_definition
           JOIN pg_class relation
             ON relation.oid = trigger_definition.tgrelid
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'discrete'
             AND relation.relname = 'result_aggregations'
             AND trigger_definition.tgname =
               'forbid_result_aggregation_delete_trigger'
             AND NOT trigger_definition.tgisinternal
         ) AS has_aggregation_delete_guard,
         has_schema_privilege('admin', 'discrete', 'USAGE')
           AND has_schema_privilege('admin', 'instruments', 'USAGE')
           AND has_schema_privilege('admin', 'public', 'USAGE')
           AS admin_has_required_schema_usage,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'samples'
             AND column_name = 'sample_qualifier'
         ) AS removed_scalar_sample_qualifier,
         (
           SELECT count(*)
           FROM information_schema.columns
           WHERE column_name = 'no_source_update'
             AND is_nullable = 'NO'
             AND (
               (table_schema = 'continuous' AND table_name IN (
                 'measurements_continuous',
                 'grades',
                 'approvals',
                 'qualifiers',
                 'notes'
               ))
               OR (table_schema = 'discrete' AND table_name IN (
                 'samples',
                 'results'
               ))
             )
         ) = 7 AS all_source_update_columns_available,
         (
           SELECT count(*)
           FROM information_schema.columns
           WHERE table_schema = 'continuous'
             AND table_name = 'notes'
             AND column_name IN (
               'note_id',
               'timeseries_id',
               'note',
               'start_dt',
               'end_dt',
               'created',
               'created_by',
               'modified_by',
               'modified',
               'no_source_update'
             )
         ) = 10 AS continuous_notes_has_expected_columns,
         NOT EXISTS (
           SELECT 1
           FROM (
             SELECT conname
             FROM pg_constraint
             WHERE conrelid = 'continuous.notes'::regclass
               AND conname IN (
                 'notes_pkey',
                 'notes_timeseries_id_fkey',
                 'notes_text_not_blank',
                 'notes_period_valid'
               )
           ) present
           RIGHT JOIN (
             VALUES
               ('notes_pkey'),
               ('notes_timeseries_id_fkey'),
               ('notes_text_not_blank'),
               ('notes_period_valid')
           ) expected(conname) USING (conname)
           WHERE present.conname IS NULL
         ) AS continuous_notes_has_expected_constraints,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE column_name = 'no_update'
             AND (
               (table_schema = 'continuous' AND table_name IN (
                 'measurements_continuous',
                 'measurements_calculated_daily',
                 'grades',
                 'approvals',
                 'qualifiers'
               ))
               OR (table_schema = 'discrete' AND table_name IN (
                 'samples',
                 'results'
               ))
             )
         ) AS removed_legacy_no_update_columns,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name IN (
               'samples_metadata_en',
               'samples_metadata_fr',
               'results_metadata_en',
               'results_metadata_fr'
             )
             AND column_name LIKE '%no_update%'
         ) AS metadata_views_removed_legacy_no_update_names,
         (
           SELECT count(*)
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name IN (
               'samples_metadata_en',
               'samples_metadata_fr',
               'results_metadata_en',
               'results_metadata_fr'
             )
             AND column_name LIKE '%no_source_update%'
         ) = 6 AS metadata_views_have_source_update_names,
         NOT EXISTS (
           SELECT 1
           FROM discrete.import_profiles
           WHERE column_map ? 'no_update'
              OR defaults ? 'no_update'
         ) AS import_profiles_removed_legacy_no_update_keys,
         (
           SELECT count(*)
           FROM discrete.result_aggregation_types
           WHERE aggregation_type IN (
             'mean', 'median', 'min', 'max', 'sum', 'weighted_mean'
           )
         ) = 6 AS all_initial_aggregation_types_available,
         NOT EXISTS (
           SELECT 1
           FROM (
             VALUES
               ('discrete', 'sample_qualifiers', 'audit_sample_qualifiers_trigger'),
                ('discrete', 'sample_observers', 'audit_sample_observers_trigger'),
               ('discrete', 'result_aggregation_types', 'audit_result_aggregation_types_trigger'),
               ('discrete', 'result_aggregations', 'audit_result_aggregations_trigger'),
               ('discrete', 'result_components', 'audit_result_components_trigger'),
               ('continuous', 'notes', 'audit_notes_trigger')
           ) expected(schema_name, table_name, trigger_name)
           LEFT JOIN audit.table_registry registry
             ON registry.schema_name = expected.schema_name
            AND registry.table_name = expected.table_name
            AND registry.capture_mode = 'generic_insert_update_delete'
           LEFT JOIN pg_namespace namespace
             ON namespace.nspname = expected.schema_name
           LEFT JOIN pg_class relation
             ON relation.relnamespace = namespace.oid
            AND relation.relname = expected.table_name
           LEFT JOIN pg_trigger trigger_definition
             ON trigger_definition.tgrelid = relation.oid
            AND trigger_definition.tgname = expected.trigger_name
            AND NOT trigger_definition.tgisinternal
           WHERE registry.table_name IS NULL
              OR trigger_definition.oid IS NULL
         ) AS all_audit_configuration_available,
         (
           SELECT count(*)
           FROM pg_trigger trigger_definition
           JOIN pg_class relation
             ON relation.oid = trigger_definition.tgrelid
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'continuous'
             AND relation.relname = 'notes'
             AND trigger_definition.tgname IN (
               'update_notes_modified',
               'update_notes_modified_by',
               'audit_notes_trigger'
             )
             AND NOT trigger_definition.tgisinternal
         ) = 3 AS continuous_notes_has_change_triggers,
         EXISTS (
           SELECT 1
           FROM pg_class relation
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'continuous'
             AND relation.relname = 'notes'
             AND relation.relrowsecurity
         ) AS continuous_notes_uses_rls,
         (
           SELECT count(*)
           FROM pg_policies
           WHERE schemaname = 'continuous'
             AND tablename = 'notes'
             AND policyname IN (
               'parent_visibility_restrict',
               'parent_visibility_restrict_insert',
               'parent_visibility_restrict_update',
               'parent_visibility_restrict_delete',
               'parent_visibility_allow_select',
               'parent_visibility_allow_insert',
               'parent_visibility_allow_update',
               'parent_visibility_allow_delete'
             )
         ) = 8 AS continuous_notes_has_visibility_policies,
         NOT EXISTS (
           (
             SELECT grantee, privilege_type
             FROM information_schema.role_table_grants
             WHERE table_schema = 'continuous'
               AND table_name = 'grades'
             EXCEPT
             SELECT grantee, privilege_type
             FROM information_schema.role_table_grants
             WHERE table_schema = 'continuous'
               AND table_name = 'notes'
           )
           UNION ALL
           (
             SELECT grantee, privilege_type
             FROM information_schema.role_table_grants
             WHERE table_schema = 'continuous'
               AND table_name = 'notes'
             EXCEPT
             SELECT grantee, privilege_type
             FROM information_schema.role_table_grants
             WHERE table_schema = 'continuous'
               AND table_name = 'grades'
           )
         ) AS continuous_note_privileges_match_grades,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.role_table_grants grants
           WHERE grants.table_schema = 'continuous'
             AND grants.table_name = 'notes'
             AND grants.privilege_type = 'INSERT'
             AND NOT CASE
               WHEN grants.grantee = 'PUBLIC' THEN EXISTS (
                 SELECT 1
                 FROM pg_class sequence_definition
                 CROSS JOIN LATERAL aclexplode(COALESCE(
                   sequence_definition.relacl,
                   acldefault('S', sequence_definition.relowner)
                 )) acl
                 WHERE sequence_definition.oid =
                   'continuous.notes_note_id_seq'::regclass
                   AND acl.grantee = 0
                   AND acl.privilege_type = 'USAGE'
               )
               ELSE has_sequence_privilege(
                 grants.grantee,
                 'continuous.notes_note_id_seq',
                 'USAGE'
               )
             END
         ) AS continuous_note_insert_roles_have_sequence_usage,
         (
           SELECT count(*)
           FROM pg_class relation
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'discrete'
             AND relation.relname IN (
               'sample_qualifiers',
                'sample_observers',
                'result_aggregations',
               'result_components'
             )
             AND relation.relrowsecurity
         ) = 4 AS all_parent_scoped_tables_use_rls,
         (
           SELECT count(*)
           FROM pg_class relation
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'discrete'
             AND relation.relname IN (
               'samples',
               'results',
               'sample_documents',
               'sample_groups',
               'sample_group_members',
               'sample_qualifiers',
                'sample_observers',
                'result_aggregations',
               'result_components'
             )
             AND relation.relrowsecurity
             AND relation.relforcerowsecurity
         ) = 9 AS discrete_visibility_hierarchy_forces_rls,
         (
           SELECT count(*)
           FROM pg_policies
           WHERE schemaname = 'discrete'
             AND policyname IN (
               'sample_qualifiers_parent_sample_access',
                'sample_observers_parent_sample_access',
                'result_aggregations_parent_access',
               'result_components_parent_access'
             )
             AND cmd = 'ALL'
             AND qual IS NOT NULL
             AND with_check IS NOT NULL
         ) = 4 AS all_parent_access_policies_available,
         (
           SELECT count(*)
           FROM pg_trigger trigger_definition
           JOIN pg_class relation
             ON relation.oid = trigger_definition.tgrelid
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'discrete'
             AND trigger_definition.tgname IN (
               'validate_result_aggregation_result_trigger',
               'validate_result_aggregation_config_trigger',
               'validate_result_aggregation_components_trigger'
             )
             AND trigger_definition.tgdeferrable
             AND NOT trigger_definition.tginitdeferred
         ) = 3 AS aggregation_constraints_initially_immediate,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'result_aggregations'
             AND column_name = 'expected_count'
             AND is_nullable = 'YES'
         ) AS result_aggregations_has_expected_count,
         (
           SELECT count(*)
           FROM information_schema.views v
           JOIN information_schema.columns c
             USING (table_schema, table_name)
           WHERE v.table_schema = 'discrete'
             AND v.table_name IN (
               'samples_metadata_en',
               'samples_metadata_fr',
               'results_metadata_en',
               'results_metadata_fr'
             )
             AND c.column_name = 'sample_qualifier_ids'
         ) = 4 AS all_metadata_views_have_qualifier_arrays,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.views v
           JOIN information_schema.columns c
             USING (table_schema, table_name)
           WHERE v.table_schema = 'discrete'
             AND v.table_name IN (
               'samples_metadata_en',
               'samples_metadata_fr',
               'results_metadata_en',
               'results_metadata_fr'
             )
             AND (
               c.column_name LIKE 'observer_%'
               OR c.column_name LIKE 'sample_observer_%'
             )
         ) AS metadata_views_exclude_observers,
         (
           SELECT count(*)
           FROM information_schema.views v
           JOIN information_schema.columns c
             USING (table_schema, table_name)
           WHERE v.table_schema = 'discrete'
             AND v.table_name IN (
               'results_metadata_en',
               'results_metadata_fr'
             )
              AND c.column_name IN (
               'aggregation_type',
                 'calculation_arguments',
                 'expected_count'
               )
         ) = 6 AS result_metadata_views_have_aggregation_contract,
         (
           SELECT count(*)
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'result_aggregation_summary'
             AND column_name IN (
               'expected_count',
               'missing_component_count',
               'has_component_shortfall'
             )
         ) = 3 AS aggregation_summary_has_expected_count_status,
         NOT EXISTS (
           SELECT 1 FROM discrete.stale_result_aggregations
         ) AS no_stale_result_aggregations"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      failed_verification <- names(verification)[
        !vapply(verification[1, ], isTRUE, logical(1))
      ]
      stop(
        "Patch 60 schema verification failed: ",
        paste(failed_verification, collapse = ", "),
        "."
      )
    }

    migrated_sample_qualifier_count <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::integer AS n
       FROM discrete.sample_qualifiers"
    )$n[[1]]
    if (migrated_sample_qualifier_count != legacy_sample_qualifier_count) {
      stop(
        "Patch 60 sample qualifier migration count does not match the legacy column."
      )
    }

    # Rename the function 'downloadSnowCourse' in the database's source adapter registry to remove ambiguity with NWT's snow course download/fetch function
    DBI::dbExecute(
      con,
      "INSERT INTO public.source_adapter_capabilities (
         source_fx,
         data_domain,
         adapter_kind,
         requires_transmission_mapping,
         inject_timeseries_id,
         parallel_group_strategy,
         parallel_group_args,
         allow_empty_initial_fetch,
         transmission_method_codes,
         argument_schema,
         ui_config,
         enabled,
         note,
         created_by,
         modified_by,
         created,
         modified
       )
       SELECT
         'downloadSnowCourseYG',
         data_domain,
         adapter_kind,
         requires_transmission_mapping,
         inject_timeseries_id,
         parallel_group_strategy,
         parallel_group_args,
         allow_empty_initial_fetch,
         transmission_method_codes,
         argument_schema,
         ui_config,
         enabled,
         'Retrieves snow-course observations from the Yukon Government''s PostgreSQL snow database.',
         created_by,
         modified_by,
         created,
         modified
       FROM public.source_adapter_capabilities
       WHERE source_fx = 'downloadSnowCourse'
         AND data_domain = 'discrete'"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.sample_series_source_adapters SET 
      source_fx = 'downloadSnowCourseYG' WHERE source_fx = 'downloadSnowCourse'"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.samples SET import_source = 'downloadSnowCourseYG' WHERE import_source = 'downloadSnowCourse'"
    )
    DBI::dbExecute(
      con,
      "DELETE FROM public.source_adapter_capabilities
       WHERE source_fx = 'downloadSnowCourse'
         AND data_domain = 'discrete'"
    )

    # Fix the overly restrictive sub_location unique constraint. This was also done in patch_59, but it was missed in the original patch_59 implementation so this ensures even application across systems.
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations DROP CONSTRAINT IF EXISTS sub_locations_name_key"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.sub_locations ADD CONSTRAINT sub_locations_name_key UNIQUE (location_id, sub_location_name)"
    )

    # Record the patch version before the final verification so that the
    # verification is the last database operation before COMMIT and can still
    # roll the entire schema change back if any final-state invariant fails.
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '60'
       WHERE item = 'Last patch number'"
    )
    patch_package_version <- as.character(packageVersion("AquaCache"))
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(patch_package_version)
    )

    final_verification <- DBI::dbGetQuery(
      con,
      "SELECT
         (
           SELECT count(*)
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'results'
             AND (column_name, data_type, is_nullable) IN (
               ('lab_report_no', 'text', 'YES'),
               ('lab_sample_no', 'text', 'YES'),
               ('grade_type_id', 'integer', 'YES'),
               ('approval_type_id', 'integer', 'YES')
             )
         ) = 4 AS result_metadata_columns_are_available,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'results'
             AND column_name IN ('grade_id', 'qualifier_id', 'approval_id')
         ) AS ambiguous_result_metadata_columns_are_absent,
         to_regclass('discrete.result_qualifiers') IS NULL
           AS result_qualifier_table_is_absent,
         position(
           'pg_trigger_depth() > 1' IN pg_get_functiondef(
             'discrete.forbid_result_aggregation_delete()'::regprocedure
           )
         ) > 0 AS aggregation_delete_guard_allows_parent_cascade,
         (
           SELECT count(*)
           FROM pg_constraint constraint_definition
           WHERE constraint_definition.conrelid = 'discrete.results'::regclass
             AND constraint_definition.contype = 'f'
             AND constraint_definition.confupdtype = 'c'
             AND constraint_definition.confdeltype = 'n'
             AND constraint_definition.confrelid IN (
               'public.grade_types'::regclass,
               'public.approval_types'::regclass
             )
         ) = 2 AS result_quality_foreign_keys_are_safe,
         NOT EXISTS (
           SELECT 1
           FROM (
             VALUES
               ('results_metadata_en', 'lab_report_no'),
               ('results_metadata_en', 'lab_sample_no'),
               ('results_metadata_en', 'result_grade_id'),
               ('results_metadata_en', 'result_grade_code'),
               ('results_metadata_en', 'result_grade_description'),
               ('results_metadata_en', 'result_approval_id'),
               ('results_metadata_en', 'result_approval_code'),
               ('results_metadata_en', 'result_approval_description'),
               ('results_metadata_fr', 'lab_report_no'),
               ('results_metadata_fr', 'lab_sample_no'),
               ('results_metadata_fr', 'result_grade_id'),
               ('results_metadata_fr', 'result_grade_code'),
               ('results_metadata_fr', 'result_grade_description_fr'),
               ('results_metadata_fr', 'result_approval_id'),
               ('results_metadata_fr', 'result_approval_code'),
               ('results_metadata_fr', 'result_approval_description_fr')
           ) expected(table_name, column_name)
           LEFT JOIN information_schema.columns actual
             ON actual.table_schema = 'discrete'
            AND actual.table_name = expected.table_name
            AND actual.column_name = expected.column_name
           WHERE actual.column_name IS NULL
         ) AS result_metadata_views_are_complete,
         EXISTS (
           SELECT 1
           FROM public.source_adapter_capabilities
           WHERE source_fx = 'downloadSnowCourseYG'
             AND data_domain = 'discrete'
         )
         AND NOT EXISTS (
           SELECT 1
           FROM public.source_adapter_capabilities
           WHERE source_fx = 'downloadSnowCourse'
             AND data_domain = 'discrete'
         )
         AND NOT EXISTS (
           SELECT 1
           FROM discrete.sample_series_source_adapters
           WHERE source_fx = 'downloadSnowCourse'
         )
         AND NOT EXISTS (
           SELECT 1
           FROM discrete.samples
           WHERE import_source = 'downloadSnowCourse'
         ) AS snow_course_adapter_rename_is_complete,
         EXISTS (
           SELECT 1
           FROM pg_constraint constraint_definition
           WHERE constraint_definition.conrelid = 'public.sub_locations'::regclass
             AND constraint_definition.conname = 'sub_locations_name_key'
             AND constraint_definition.contype = 'u'
             AND pg_get_constraintdef(constraint_definition.oid) =
               'UNIQUE (location_id, sub_location_name)'
         ) AS sub_location_uniqueness_is_scoped,
         (
           SELECT version = '60'
           FROM information.version_info
           WHERE item = 'Last patch number'
         ) AS patch_number_is_current,
         (
           SELECT version = $1
           FROM information.version_info
           WHERE item = 'AquaCache R package used for last patch'
         ) AS patch_package_version_is_current",
      params = list(patch_package_version)
    )
    if (!all(unlist(final_verification[1, ], use.names = FALSE))) {
      failed_final_verification <- names(final_verification)[
        !vapply(final_verification[1, ], isTRUE, logical(1))
      ]
      stop(
        "Patch 60 final verification failed: ",
        paste(failed_final_verification, collapse = ", "),
        "."
      )
    }

    # Commit the fully verified schema before attempting the optional data
    # migration from the Yukon SnowDB.
    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 60 applied successfully. Generic result aggregations, result components, multi-valued sample qualifiers, sample observers, time-ranged continuous notes, and source-update protection are ready."
    )

    # SnowDB is a Yukon source database. Probe for it on the same server as
    # this AquaCache connection and skip quietly for partner installations.
    # Any migration error is reported after commit and cannot roll back or make
    # the Patch 60 schema upgrade appear to have failed.
    message(
      "Checking if the Yukon SnowDB is available for optional snow-course component migration..."
    )
    tryCatch(
      local({
        server_address <- DBI::dbGetQuery(
          con,
          "SELECT host(inet_server_addr()) AS server_address"
        )$server_address[[1L]]
        snow_settings <- c(
          port = Sys.getenv("snowPort"),
          user = Sys.getenv("snowUser", Sys.getenv("snowAdminUser")),
          password = Sys.getenv("snowPass", Sys.getenv("snowAdminPass"))
        )
        if (
          is.na(server_address) ||
            !nzchar(server_address) ||
            any(!nzchar(snow_settings))
        ) {
          message(
            "Snow-course component migration skipped: the AquaCache server ",
            "address or SnowDB connection settings are unavailable."
          )
        } else {
          snow_con <- tryCatch(
            {
              DBI::dbConnect(
                RPostgres::Postgres(),
                dbname = Sys.getenv("snowName", "snow"),
                host = server_address,
                port = snow_settings[["port"]],
                user = snow_settings[["user"]],
                password = snow_settings[["password"]],
                connect_timeout = 5L
              )
              message("SnowDB connection established.")
            },
            error = function(e) NULL
          )
          if (is.null(snow_con)) {
            message(
              "Snow-course component migration skipped: SnowDB is not ",
              "reachable at AquaCache server ",
              server_address,
              "."
            )
          } else {
            DBI::dbExecute(snow_con, "SET timezone = 'UTC'")
            on.exit(
              {
                if (DBI::dbIsValid(snow_con)) DBI::dbDisconnect(snow_con)
              },
              add = TRUE
            )
            migration_candidates <- c(
              system.file(
                "patches",
                "migrate_snow_course_components.R",
                package = "AquaCache"
              ),
              file.path(
                "inst",
                "patches",
                "migrate_snow_course_components.R"
              )
            )
            migration_script <- migration_candidates[
              nzchar(migration_candidates) & file.exists(migration_candidates)
            ][1L]
            if (is.na(migration_script)) {
              stop(
                "Could not find patches/migrate_snow_course_components.R."
              )
            }
            old_options <- options(
              AquaCache.snow_component_migration.source_only = TRUE
            )
            on.exit(options(old_options), add = TRUE)
            migration_environment <- new.env(parent = globalenv())
            sys.source(migration_script, envir = migration_environment)
            migration_environment$run_snow_component_migration(
              apply_changes = TRUE,
              aquacache = con,
              snow = snow_con,
              confirm_production = TRUE
            )
            message("Snow-course component migration completed successfully.")
          }
        }
      }),
      error = function(e) {
        message(
          "WARNING: Patch 60 is committed, but the optional snow-course component ",
          "migration failed: ",
          conditionMessage(e),
          " Re-run inst/patches/migrate_snow_course_components.R after ",
          "correcting the problem."
        )
      }
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
