# Patch 60 adds generic component-based discrete results, supports multiple
# qualifiers and observers for one sample, and hardens observer identity.
# discrete.results remains the canonical reportable result; result components
# and their versioned calculation configuration are normalized in child
# tables below it.
#
# Implementation goals:
# 1. Support component-built results for any parameter and aggregation type,
#    initially mean, median, min, max, sum, and weighted mean.
# 2. Keep one canonical discrete.results row per reportable parameter and have
#    PostgreSQL maintain it (not R!) from included result components. A canonical result
#    may be NULL while it is assembled and when calculation yields no value.
# 3. Normalize sample qualifiers and observers as many-to-many associations;
#    migrate and then remove discrete.samples.sample_qualifier.
# 4. Make calculation choices explicit and reproducible with a calculation
#    version and validated JSON arguments for missing values, non-detects,
#    multipliers, and final rounding.
# 5. Preserve AquaCache audit, RLS, metadata-view, and privilege conventions,
#    and expose the new contract through discrete ingestion functions. Add new
#    tables to audit tracking
#
# -------------------------------------------------------------------------------
# Still missing or to do:
# 0. Rename this file to 'patch_60.R' once finalized so it gets read by AquaConnect()!
# 1. Check/ensure that result calculation/recalculation from components isn't too
#    costly, as I think it will run on every insert/update of new components.
#    Every recalculation will also trigger a new entry to the 'audit' table as it's
#    an UPDATE. That will quickly baloon the audit table, so has to be addressed!
#    Since composite results are derived from a table that's also audited, perhaps
#    composite results simply get excluded from auditing? That would require computation,
#    and adds complications to point in time reconstruction, however.
# 2. result updates/recalculations should probably be conditional: right now, even
#    a change to a component note, modified, or modified_by would trigger a recalculation.
# 3. Calculation versions can be kept in this construct - is that necessary, given
#    the audit tables? Also right now the trigger rejects any version other than 1 I think?
# 4. Update YGwater application pieces (when this is totally finalized). Consumption-only
#    modules/functions require a change to 'plotDiscrete.R' and to Shiny app module
#    'discreteData.R', while the 'admin' side of the application (add/edit samples/results)
#    will require updates to at least the editSamples.R module.
# 5. Update 'downloadSnowCourseYG' so it can be used to fetch composite results from
#    the YG snow survey database. This will be the first step to deprecating that database.
# 6. Re-create the 'testdb' fixture when this patch is finally applied; also update
#    the test fixture in the 'YGwater' package.
# 7. This file currently adds observers to the view tables. Observers should not be in
#    public-facing views to preserve privacy and need to be removed.
# 8. A final check should be made to ensure that cluster roles get the right permissions
#    on the new tables. Permissions should match those of table 'discrete.results'.
#    The discovery and application of permissions should work regardless of the cluster
#    on which this patch is being run on, i.e. no hard coding of roles.
# 9. A final check is necessary to ensure that RLS policies are properly applied. The
#   same policies as on discrete.results should be used, which should be restricting
#   visibility based on visibility of samples (themselves restricted based on location
#   visibility AND the share_with column of discrete.samples table)
# 10. Should 'result_aggregations' contain a column 'expected_count'? Take the example
#    of a 10 point survey where only 9 results are used. Having that column would make it
#    explicit that 1 result was either not collected or was excluded. If that's useful
#    let's add the column and add something to the views
# 11. Deleting a result_aggregations row cascades to result_components, but does nothing
#    to results on deletion to result_aggregations. It's therefore possible to delete
#    result components completely and remove a result aggregation entry while retaining a
#    result. Since almost evrything else in the DB is coded to ON DELETE CASCADE, this is
#    risky. We either document it properly (e.g. deleting a result_aggregation row will
#    essentially convert an aggregate result to a simple result) or revoke direct deleve
#    on result_aggregations, forcing deletes of an aggregation to happen via deletion of
#    the result in discrete.results.

# Later stuff:
# 1. snow survey workbook creation and ingestion functions (in this package and
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
  "Working on patch 60: adding generic result aggregations and components, multi-valued sample qualifiers, sample observers, and database-maintained canonical results. Changes are being made within a transaction, so an error will roll back the database."
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
         to_regclass('public.qualifier_types') IS NOT NULL AS has_qualifier_types,
         to_regclass('instruments.observers') IS NOT NULL AS has_observers,
         to_regclass('discrete.samples_metadata_en') IS NOT NULL AS has_samples_metadata_en,
         to_regclass('discrete.samples_metadata_fr') IS NOT NULL AS has_samples_metadata_fr,
         to_regclass('discrete.results_metadata_en') IS NOT NULL AS has_results_metadata_en,
         to_regclass('discrete.results_metadata_fr') IS NOT NULL AS has_results_metadata_fr,
         to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function,
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_modified_function,
         to_regprocedure('public.user_modified()') IS NOT NULL AS has_user_function,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 60 requires the discrete sample/result schema, qualifier and observer catalogues, metadata views, audit framework, timestamp/user triggers, and version table created by earlier patches."
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

    target_state <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.sample_qualifiers') IS NOT NULL AS has_sample_qualifiers,
         to_regclass('discrete.sample_observers') IS NOT NULL AS has_sample_observers,
         to_regclass('discrete.result_aggregation_types') IS NOT NULL AS has_result_aggregation_types,
         to_regclass('discrete.result_aggregations') IS NOT NULL AS has_result_aggregations,
         to_regclass('discrete.result_components') IS NOT NULL AS has_result_components,
         to_regclass('discrete.result_aggregation_summary') IS NOT NULL AS has_result_aggregation_summary,
         to_regprocedure('discrete.calculate_result_aggregation(integer)') IS NOT NULL AS has_calculation_function"
    )
    if (any(unlist(target_state[1, ], use.names = FALSE))) {
      stop(
        "Patch 60 found one or more target tables or columns already present. Investigate the partial migration before applying this patch."
      )
    }

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
       'One row marks a canonical discrete result as component-built and records its aggregation algorithm, implementation version, and validated calculation arguments.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.result_aggregations.calculation_arguments IS
       'Version 1 accepts missing_values (ignore, propagate, error), non_detects (exclude, zero, condition_value, half_condition_value, error), multiplier (number), and rounding_digits (integer, applied last). Unknown keys are rejected.'"
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
           result_condition_value IS NULL
           OR result_condition IN (1, 2)
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
         WHERE result_id = target_result_id;
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
      "CREATE TRIGGER refresh_result_aggregation_config_trigger
       AFTER INSERT OR UPDATE OR DELETE
       ON discrete.result_aggregations
       FOR EACH ROW
       EXECUTE FUNCTION discrete.refresh_result_aggregation_trigger()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER refresh_result_aggregation_components_trigger
       AFTER INSERT OR UPDATE OR DELETE
       ON discrete.result_components
       FOR EACH ROW
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
       BEGIN
         target_result_id := CASE
           WHEN TG_OP = 'DELETE' THEN OLD.result_id
           ELSE NEW.result_id
         END;
         IF NOT EXISTS (
           SELECT 1
           FROM discrete.result_aggregations
           WHERE result_id = target_result_id
         ) THEN
           RETURN NULL;
         END IF;

         SELECT result, result_condition, result_condition_value
         INTO stored_result, stored_condition, stored_condition_value
         FROM discrete.results
         WHERE result_id = target_result_id;
         IF NOT FOUND THEN
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
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_result_aggregation_config_trigger
       AFTER INSERT OR UPDATE OR DELETE
       ON discrete.result_aggregations
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_result_aggregation_components_trigger
       AFTER INSERT OR UPDATE OR DELETE
       ON discrete.result_components
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_result_aggregation()"
    )

    component_function_signatures <- c(
      "validate_result_aggregation_arguments()",
      "result_component_numeric_value(numeric,integer,numeric,jsonb)",
      "calculate_result_aggregation(integer)",
      "refresh_result_aggregation(integer)",
      "refresh_result_aggregation_trigger()",
      "validate_result_aggregation()"
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

    # Quote a role for generated GRANT statements while preserving PostgreSQL's
    # special unquoted PUBLIC grantee.
    quote_grantee <- function(role_name) {
      if (identical(role_name, "PUBLIC")) {
        return("PUBLIC")
      }
      as.character(DBI::dbQuoteIdentifier(con, role_name))
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
            "discrete.refresh_result_aggregation(integer) TO %s"
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

    # Rewrite a pg_get_viewdef() sample-metadata definition: remove the former
    # scalar qualifier join, add qualifier/observer arrays, and preserve the
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
    observer_metadata.observer_ids,
    observer_metadata.observer_names,
    observer_metadata.observer_organizations,
    observer_metadata.observer_roles,
    observer_metadata.sample_observer_notes,
    s.owner AS owner_id,"
      } else {
        "qualifier_metadata.sample_qualifier_ids,
    qualifier_metadata.sample_qualifier_codes,
    qualifier_metadata.sample_qualifier_descriptions,
    qualifier_metadata.sample_qualifier_notes,
    observer_metadata.observer_ids,
    observer_metadata.observer_names,
    observer_metadata.observer_organizations,
    observer_metadata.observer_roles,
    observer_metadata.sample_observer_notes,
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
       ) qualifier_metadata ON TRUE
       LEFT JOIN LATERAL (
         SELECT
           array_agg(
             so.observer_id
             ORDER BY so.observer_role, so.observer_id
           ) AS observer_ids,
           array_agg(
             concat_ws(' ', obs.observer_first, obs.observer_last)
             ORDER BY so.observer_role, so.observer_id
           ) AS observer_names,
           array_agg(
             obs.organization
             ORDER BY so.observer_role, so.observer_id
           ) AS observer_organizations,
           array_agg(
             so.observer_role
             ORDER BY so.observer_role, so.observer_id
           ) AS observer_roles,
           array_agg(
             so.note
             ORDER BY so.observer_role, so.observer_id
           ) AS sample_observer_notes
         FROM discrete.sample_observers so
         JOIN instruments.observers obs
           ON obs.observer_id = so.observer_id
         WHERE so.sample_id = s.sample_id
       ) observer_metadata ON TRUE"
      )
    }

    # Extend a pg_get_viewdef() result-metadata definition with the normalized
    # sample associations and result-aggregation contract. This deliberately
    # builds on the transformed sample view so both metadata layers expose the
    # same qualifier and observer representation.
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
    sm.observer_ids,
    sm.observer_names,
    sm.observer_organizations,
    sm.observer_roles,
    sm.sample_observer_notes,
    sm.owner_id AS sample_owner_id,"
      } else {
        "sm.sample_qualifier_ids,
    sm.sample_qualifier_codes,
    sm.sample_qualifier_descriptions,
    sm.sample_qualifier_notes,
    sm.observer_ids,
    sm.observer_names,
    sm.observer_organizations,
    sm.observer_roles,
    sm.sample_observer_notes,
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
          "    aggregation_metadata.result_aggregation_type_id,\n",
          "    aggregation_metadata.aggregation_type,\n",
          "    aggregation_metadata.calculation_version,\n",
          "    aggregation_metadata.calculation_arguments,"
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
       LEFT JOIN LATERAL (
         SELECT
           ra.result_aggregation_type_id,
           rat.aggregation_type,
           ra.calculation_version,
           ra.calculation_arguments
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
          grepl("sample_qualifier_ids", view_definition, fixed = TRUE) &&
          grepl("observer_ids", view_definition, fixed = TRUE) &&
          (!startsWith(view_name, "results_") ||
            (grepl("aggregation_type", view_definition, fixed = TRUE) &&
              grepl(
                "calculation_arguments",
                view_definition,
                fixed = TRUE
              )))
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
       'English-language view that flattens key discrete sample metadata, including all qualifier and observer associations.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_fr IS
       'French-language view that flattens key discrete sample metadata, including all qualifier and observer associations.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_metadata_en IS
       'English-language view that joins each discrete result to flattened sample metadata and optional component-aggregation configuration.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_metadata_fr IS
       'French-language view that joins each discrete result to flattened sample metadata and optional component-aggregation configuration.'"
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
         r.result AS stored_result,
         calculation.calculated_result,
         r.result IS NOT DISTINCT FROM
           calculation.calculated_result
           AS result_is_current,
         count(rc.result_component_id)::integer AS component_count,
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
         calculation.calculated_result"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW discrete.result_aggregation_summary OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.result_aggregation_summary IS
       'One row per component-built result with its calculation contract, stored/calculated agreement, inclusion and contribution counts, weights, and raw descriptive statistics.'"
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
    }

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.sample_qualifiers') IS NOT NULL
           AS has_sample_qualifiers,
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
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'samples'
             AND column_name = 'sample_qualifier'
         ) AS removed_scalar_sample_qualifier,
         (
           SELECT count(*)
           FROM discrete.result_aggregation_types
           WHERE aggregation_type IN (
             'mean', 'median', 'min', 'max', 'sum', 'weighted_mean'
           )
         ) = 6 AS all_initial_aggregation_types_available,
         (
           SELECT count(*)
           FROM audit.table_registry
           WHERE schema_name = 'discrete'
             AND table_name IN (
                'sample_qualifiers',
                'sample_observers',
                'result_aggregation_types',
                'result_aggregations',
                'result_components'
              )
         ) = 5 AS all_tables_registered_for_audit,
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
             AND c.column_name = 'observer_ids'
         ) = 4 AS all_metadata_views_have_observer_arrays,
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
                'calculation_arguments'
              )
         ) = 4 AS result_metadata_views_have_aggregation_contract"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop("Patch 60 schema verification failed.")
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
      "UPDATE public.source_adapter_capabilities SET
         source_fx ='downloadSnowCourseYG',
         note = 'Retrieves snow-course observations from the Yukon Government''s Postgresql snow database.'
       WHERE source_fx = 'downloadSnowCourse'
       AND data_domain = 'discrete'
      "
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

    # Set the patch version in the database and commit.
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '60'
       WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(packageVersion("AquaCache")))
    )

    # Commit and be done!
    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 60 applied successfully. Generic result aggregations, result components, multi-valued qualifiers, and sample observers are ready."
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
