# Patch 47: database-driven water-quality guideline engine
#
# Adds structured guideline applicability, declarative value derivation rules,
# sample-specific input resolution, and result-level guideline evaluation
# functions. Existing guideline rows are discarded because the previous
# guideline_sql design had not become a production dependency.

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 47: adding database-driven guideline derivation and application. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
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
    # Basic sanity checks ####################################################
    check <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "to_regclass('discrete.guidelines') IS NOT NULL AS has_guidelines,",
        "to_regclass('discrete.guidelines_media_types') IS NOT NULL AS has_guideline_media,",
        "to_regclass('discrete.guidelines_fractions') IS NOT NULL AS has_guideline_fractions,",
        "to_regclass('discrete.results') IS NOT NULL AS has_results,",
        "to_regclass('discrete.samples') IS NOT NULL AS has_samples,",
        "to_regclass('public.parameters') IS NOT NULL AS has_parameters,",
        "to_regclass('public.matrix_states') IS NOT NULL AS has_matrix_states,",
        "to_regclass('public.units') IS NOT NULL AS has_units,",
        "to_regclass('public.media_types') IS NOT NULL AS has_media_types"
      )
    )

    if (
      !isTRUE(check$has_guidelines[[1]]) ||
        !isTRUE(check$has_guideline_media[[1]]) ||
        !isTRUE(check$has_guideline_fractions[[1]]) ||
        !isTRUE(check$has_results[[1]]) ||
        !isTRUE(check$has_samples[[1]]) ||
        !isTRUE(check$has_parameters[[1]]) ||
        !isTRUE(check$has_matrix_states[[1]]) ||
        !isTRUE(check$has_units[[1]]) ||
        !isTRUE(check$has_media_types[[1]])
    ) {
      stop(
        paste(
          "Patch 47 requires discrete.guidelines,",
          "discrete.guidelines_media_types,",
          "discrete.guidelines_fractions, discrete.results,",
          "discrete.samples, public.parameters, public.matrix_states,",
          "public.units, and public.media_types to already exist."
        )
      )
    }

    # Existing guideline rows have never been used by application code. Start
    # the guideline catalogue cleanly so the new model can enforce stronger
    # identifiers and versioning rules.
    DBI::dbExecute(
      con,
      "TRUNCATE TABLE
         discrete.guidelines_fractions,
         discrete.guidelines_media_types,
         discrete.guidelines,
         discrete.guideline_series,
         discrete.guideline_publishers
       RESTART IDENTITY CASCADE;"
    )

    # Existing column names predate the versioned guideline model.
    col_check <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'guidelines'
         AND column_name IN ('publisher', 'series');"
    )$column_name
    if ("publisher" %in% col_check && !"publisher_id" %in% col_check) {
      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.guidelines
         RENAME COLUMN publisher TO publisher_id;"
      )
    }
    if ("series" %in% col_check && !"series_id" %in% col_check) {
      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.guidelines
         RENAME COLUMN series TO series_id;"
      )
    }

    # Controlled vocabulary #################################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_comparison_operators (
         operator_code TEXT PRIMARY KEY,
         operator_symbol TEXT NOT NULL,
         operator_name TEXT NOT NULL UNIQUE,
         operator_name_fr TEXT,
         requires_lower_bound BOOLEAN NOT NULL DEFAULT FALSE,
         requires_upper_bound BOOLEAN NOT NULL DEFAULT TRUE,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_comparison_operators OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_comparison_operators IS
       'Controlled vocabulary for how measured values are compared with guideline values.';"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_comparison_operators (
         operator_code, operator_symbol, operator_name, operator_name_fr,
         requires_lower_bound, requires_upper_bound, description
       )
       VALUES
         ('lte', '<=', 'less than or equal to', 'inferieur ou egal a',
          FALSE, TRUE, 'Measured value should be less than or equal to the guideline value.'),
         ('gte', '>=', 'greater than or equal to', 'superieur ou egal a',
          TRUE, FALSE, 'Measured value should be greater than or equal to the guideline value.'),
         ('range', 'between', 'within range', 'dans la plage',
          TRUE, TRUE, 'Measured value should fall between lower and upper guideline values.'),
         ('eq', '=', 'equal to', 'egal a',
          TRUE, TRUE, 'Measured value should equal the guideline value.'),
         ('narrative', 'narrative', 'narrative guideline', 'ligne directrice narrative',
          FALSE, FALSE, 'Guideline is narrative or site-specific and may not produce a numeric comparison.')
       ON CONFLICT (operator_code) DO UPDATE
       SET operator_symbol = EXCLUDED.operator_symbol,
           operator_name = EXCLUDED.operator_name,
           operator_name_fr = EXCLUDED.operator_name_fr,
           requires_lower_bound = EXCLUDED.requires_lower_bound,
           requires_upper_bound = EXCLUDED.requires_upper_bound,
           description = EXCLUDED.description;"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_value_algorithms (
         algorithm_code TEXT PRIMARY KEY,
         algorithm_name TEXT NOT NULL UNIQUE,
         algorithm_name_fr TEXT,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_value_algorithms OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_value_algorithms IS
       'Controlled vocabulary for database-side algorithms used to derive guideline values.';"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_value_algorithms (
         algorithm_code, algorithm_name, algorithm_name_fr, description
       )
       VALUES
         ('constant', 'Constant value', 'Valeur constante',
          'Returns a fixed numeric guideline value.'),
         ('lookup_range', 'Lookup by input range', 'Recherche par plage',
          'Uses a sample-specific input value to select an output value from non-overlapping numeric ranges.'),
         ('lookup_grid', 'Lookup by multiple input ranges', 'Recherche par grille',
          'Uses two or more sample-specific input values to select an output value from a multidimensional range table.'),
         ('linear', 'Linear formula', 'Formule lineaire',
          'Computes intercept + slope * input_value using stored coefficients.'),
         ('log_linear', 'Log-linear formula', 'Formule log-lineaire',
          'Computes exp(intercept + slope * ln(input_value)) using stored coefficients.'),
         ('power', 'Power formula', 'Formule de puissance',
         'Computes factor * input_value ^ exponent using stored coefficients.'),
         ('db_function', 'Registered database function', 'Fonction de base de donnees enregistree',
         'Collects declared sample-specific inputs and passes them as JSONB to a registered database function that returns a numeric guideline value.'),
         ('model_result_cache', 'Stored model result', 'Resultat de modele stocke',
          'Collects declared sample-specific inputs and retrieves a matching previously calculated model result from the database.'),
         ('narrative', 'Narrative or non-numeric guideline', 'Ligne directrice narrative ou non numerique',
          'Returns a non-numeric guideline status for narrative, operational, or otherwise non-scalar guidelines.'),
         ('sql_scalar', 'Governed SQL scalar', 'Scalaire SQL gouverne',
          'Executes a stored single-scalar SQL expression as an escape hatch for formulae not yet represented declaratively.')
       ON CONFLICT (algorithm_code) DO UPDATE
       SET algorithm_name = EXCLUDED.algorithm_name,
           algorithm_name_fr = EXCLUDED.algorithm_name_fr,
           description = EXCLUDED.description;"
    )

    # Applicability reference tables #######################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_jurisdictions (
         jurisdiction_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         jurisdiction_code TEXT NOT NULL UNIQUE,
         jurisdiction_name TEXT NOT NULL UNIQUE,
         jurisdiction_name_fr TEXT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_jurisdiction_levels (
         jurisdiction_level_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         jurisdiction_level_code TEXT NOT NULL UNIQUE,
         jurisdiction_level_name TEXT NOT NULL UNIQUE,
         jurisdiction_level_name_fr TEXT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_protection_goals (
         protection_goal_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         protection_goal_code TEXT NOT NULL UNIQUE,
         protection_goal_name TEXT NOT NULL UNIQUE,
         protection_goal_name_fr TEXT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_exposure_durations (
         exposure_duration_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         exposure_duration_code TEXT NOT NULL UNIQUE,
         exposure_duration_name TEXT NOT NULL UNIQUE,
         exposure_duration_name_fr TEXT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_averaging_periods (
         averaging_period_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         averaging_period_code TEXT NOT NULL UNIQUE,
         averaging_period_name TEXT NOT NULL UNIQUE,
         averaging_period_name_fr TEXT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_jurisdictions OWNER TO admin;
       ALTER TABLE discrete.guideline_jurisdiction_levels OWNER TO admin;
       ALTER TABLE discrete.guideline_protection_goals OWNER TO admin;
       ALTER TABLE discrete.guideline_exposure_durations OWNER TO admin;
       ALTER TABLE discrete.guideline_averaging_periods OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_jurisdictions (
         jurisdiction_code, jurisdiction_name, sort_order
       )
       VALUES
         ('BC', 'British Columbia', 10),
         ('YT', 'Yukon', 20),
         ('CA', 'Canada', 30),
         ('US', 'United States', 40),
         ('EU', 'European Union', 50),
         ('SITE_SPECIFIC', 'Site-specific', 900),
         ('TEST_FIXTURE', 'Test fixture', 990)
       ON CONFLICT (jurisdiction_code) DO UPDATE
       SET jurisdiction_name = EXCLUDED.jurisdiction_name,
           sort_order = EXCLUDED.sort_order;"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_jurisdiction_levels (
         jurisdiction_level_code, jurisdiction_level_name, sort_order
       )
       VALUES
         ('FEDERAL', 'Federal', 10),
         ('PROVINCIAL', 'Provincial', 20),
         ('TERRITORIAL', 'Territorial', 30),
         ('PROV_TERR', 'Provincial/territorial', 40),
         ('STATE', 'State', 50),
         ('INTERNATIONAL', 'International', 60),
         ('SITE_SPECIFIC', 'Site-specific', 900),
         ('TEST_FIXTURE', 'Test fixture', 990)
       ON CONFLICT (jurisdiction_level_code) DO UPDATE
       SET jurisdiction_level_name = EXCLUDED.jurisdiction_level_name,
           sort_order = EXCLUDED.sort_order;"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_protection_goals (
         protection_goal_code, protection_goal_name, sort_order
       )
       VALUES
         ('FRESHWATER_AQUATIC_LIFE', 'Freshwater aquatic life', 10),
         ('MARINE_AQUATIC_LIFE', 'Marine aquatic life', 20),
         ('DRINKING_WATER', 'Drinking water', 30),
         ('RECREATION', 'Recreation', 40),
         ('AGRICULTURAL_WATER_USE', 'Agricultural water use', 50),
         ('IRRIGATION', 'Irrigation', 60),
         ('LIVESTOCK', 'Livestock', 70),
         ('HUMAN_HEALTH', 'Human health', 80),
         ('SITE_SPECIFIC', 'Site-specific', 900)
       ON CONFLICT (protection_goal_code) DO UPDATE
       SET protection_goal_name = EXCLUDED.protection_goal_name,
           sort_order = EXCLUDED.sort_order;"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_exposure_durations (
         exposure_duration_code, exposure_duration_name, sort_order
       )
       VALUES
         ('SHORT_TERM_ACUTE', 'Short-term acute', 10),
         ('LONG_TERM_CHRONIC', 'Long-term chronic', 20),
         ('ACUTE', 'Acute', 30),
         ('CHRONIC', 'Chronic', 40),
         ('INSTANTANEOUS_MAXIMUM', 'Instantaneous maximum', 50),
         ('INSTANTANEOUS_MINIMUM', 'Instantaneous minimum', 60),
         ('SAMPLE_SPECIFIC', 'Sample-specific', 70),
         ('NARRATIVE', 'Narrative', 900)
       ON CONFLICT (exposure_duration_code) DO UPDATE
       SET exposure_duration_name = EXCLUDED.exposure_duration_name,
           sort_order = EXCLUDED.sort_order;"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_averaging_periods (
         averaging_period_code, averaging_period_name, sort_order
       )
       VALUES
         ('SINGLE_SAMPLE', 'Single sample', 10),
         ('INSTANTANEOUS', 'Instantaneous', 20),
         ('1_HOUR_AVERAGE', '1-hour average', 30),
         ('4_DAY_AVERAGE', '4-day average', 40),
         ('24_HOUR_AVERAGE', '24-hour average', 50),
         ('30_DAY_AVERAGE', '30-day average', 60),
         ('ANNUAL_AVERAGE', 'Annual average', 70),
         ('5_SAMPLES_30_DAYS', '5 samples in 30 days', 80),
         ('SAMPLE_SPECIFIC', 'Sample-specific', 90),
         ('SOURCE_DOCUMENT', 'See source document', 900)
       ON CONFLICT (averaging_period_code) DO UPDATE
       SET averaging_period_name = EXCLUDED.averaging_period_name,
           sort_order = EXCLUDED.sort_order;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_jurisdictions IS
       'Controlled jurisdictions used by water-quality guidelines.';
       COMMENT ON TABLE discrete.guideline_jurisdiction_levels IS
       'Controlled jurisdiction levels used by water-quality guidelines.';
       COMMENT ON TABLE discrete.guideline_protection_goals IS
       'Controlled protection goals used by water-quality guidelines.';
       COMMENT ON TABLE discrete.guideline_exposure_durations IS
       'Controlled exposure durations used by water-quality guidelines and model outputs.';
       COMMENT ON TABLE discrete.guideline_averaging_periods IS
       'Controlled averaging periods used by water-quality guidelines and model outputs.';"
    )

    # Publisher and source-series metadata ##################################
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_publishers
       ADD COLUMN IF NOT EXISTS publisher_code TEXT,
       ADD COLUMN IF NOT EXISTS publisher_url TEXT,
       ADD COLUMN IF NOT EXISTS note TEXT,
       ADD COLUMN IF NOT EXISTS created_by TEXT DEFAULT CURRENT_USER NOT NULL,
       ADD COLUMN IF NOT EXISTS modified_by TEXT,
       ADD COLUMN IF NOT EXISTS created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
       ADD COLUMN IF NOT EXISTS modified TIMESTAMPTZ;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_publishers
       ALTER COLUMN publisher_name SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_publishers_code_key
       ON discrete.guideline_publishers (publisher_code)
       WHERE publisher_code IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_publishers IS
       'Organizations that publish or maintain environmental quality guidelines.';"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_series
       ADD COLUMN IF NOT EXISTS series_code TEXT,
       ADD COLUMN IF NOT EXISTS series_url TEXT,
       ADD COLUMN IF NOT EXISTS citation TEXT,
       ADD COLUMN IF NOT EXISTS note TEXT,
       ADD COLUMN IF NOT EXISTS created_by TEXT DEFAULT CURRENT_USER NOT NULL,
       ADD COLUMN IF NOT EXISTS modified_by TEXT,
       ADD COLUMN IF NOT EXISTS created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
       ADD COLUMN IF NOT EXISTS modified TIMESTAMPTZ;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_series_code_key
       ON discrete.guideline_series (series_code)
       WHERE series_code IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_series IS
       'Publication series, editions, or guideline collections that contain individual guideline values.';"
    )

    # External and database model metadata ##################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_models (
         model_code TEXT PRIMARY KEY,
         model_name TEXT NOT NULL,
         publisher_id INTEGER
           REFERENCES discrete.guideline_publishers(publisher_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         model_version TEXT,
         model_type TEXT NOT NULL,
         source_document_title TEXT,
         source_url TEXT,
         executable_url TEXT,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT guideline_models_type_check
           CHECK (
             model_type IN (
               'external_software',
               'database_function',
               'lookup_workbook',
               'published_table'
             )
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_models OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_models IS
       'Guideline calculation models such as BLM software, registered database calculators, lookup workbooks, or published tables.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_model_inputs (
         model_code TEXT NOT NULL
           REFERENCES discrete.guideline_models(model_code)
           ON UPDATE CASCADE ON DELETE CASCADE,
         input_code TEXT NOT NULL,
         input_name TEXT NOT NULL,
         parameter_id INTEGER
           REFERENCES public.parameters(parameter_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         matrix_state_id INTEGER
           REFERENCES public.matrix_states(matrix_state_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         sample_fraction_id INTEGER
           REFERENCES discrete.sample_fractions(sample_fraction_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         result_speciation_id INTEGER
           REFERENCES discrete.result_speciations(result_speciation_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         input_units INTEGER
           REFERENCES public.units(unit_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         lower_calibrated_bound NUMERIC,
         upper_calibrated_bound NUMERIC,
         bounds_action TEXT NOT NULL DEFAULT 'flag',
         required BOOLEAN NOT NULL DEFAULT TRUE,
         importance TEXT,
         default_value NUMERIC,
         default_method TEXT,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         PRIMARY KEY (model_code, input_code),
         CONSTRAINT guideline_model_inputs_bounds_check
           CHECK (
             lower_calibrated_bound IS NULL
             OR upper_calibrated_bound IS NULL
             OR lower_calibrated_bound <= upper_calibrated_bound
           ),
         CONSTRAINT guideline_model_inputs_bounds_action_check
           CHECK (bounds_action IN ('flag', 'clamp', 'reject')),
         CONSTRAINT guideline_model_inputs_importance_check
           CHECK (importance IS NULL OR importance IN ('low', 'medium', 'high'))
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_model_inputs OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_model_inputs_parameter_idx
       ON discrete.guideline_model_inputs (
         parameter_id, matrix_state_id, sample_fraction_id,
         result_speciation_id
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_model_inputs IS
       'Declared input chemistry required by guideline models, including BLM input bounds and default behaviour.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_model_outputs (
         model_code TEXT NOT NULL
           REFERENCES discrete.guideline_models(model_code)
           ON UPDATE CASCADE ON DELETE CASCADE,
         output_code TEXT NOT NULL,
         output_name TEXT NOT NULL,
         comparison_operator_code TEXT NOT NULL
           REFERENCES discrete.guideline_comparison_operators(operator_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         output_units INTEGER
           REFERENCES public.units(unit_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         exposure_duration_id INTEGER
           REFERENCES discrete.guideline_exposure_durations(exposure_duration_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         averaging_period_id INTEGER
           REFERENCES discrete.guideline_averaging_periods(averaging_period_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         PRIMARY KEY (model_code, output_code)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_model_outputs OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_model_outputs
       ADD COLUMN IF NOT EXISTS exposure_duration_id INTEGER
         REFERENCES discrete.guideline_exposure_durations(exposure_duration_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS averaging_period_id INTEGER
         REFERENCES discrete.guideline_averaging_periods(averaging_period_id)
         ON UPDATE CASCADE ON DELETE RESTRICT;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guideline_model_outputs'
             AND column_name = 'exposure_duration'
         ) THEN
           INSERT INTO discrete.guideline_exposure_durations (
             exposure_duration_code, exposure_duration_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(exposure_duration), 10)),
             exposure_duration,
             800
           FROM discrete.guideline_model_outputs
           WHERE exposure_duration IS NOT NULL
             AND btrim(exposure_duration) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_exposure_durations existing
               WHERE lower(existing.exposure_duration_name) =
                 lower(discrete.guideline_model_outputs.exposure_duration)
             )
           ON CONFLICT (exposure_duration_name) DO NOTHING;

           UPDATE discrete.guideline_model_outputs gmo
           SET exposure_duration_id = ged.exposure_duration_id
           FROM discrete.guideline_exposure_durations ged
           WHERE gmo.exposure_duration_id IS NULL
             AND lower(gmo.exposure_duration) = lower(ged.exposure_duration_name);

           ALTER TABLE discrete.guideline_model_outputs
             DROP COLUMN exposure_duration;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guideline_model_outputs'
             AND column_name = 'averaging_period'
         ) THEN
           INSERT INTO discrete.guideline_averaging_periods (
             averaging_period_code, averaging_period_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(averaging_period), 10)),
             averaging_period,
             800
           FROM discrete.guideline_model_outputs
           WHERE averaging_period IS NOT NULL
             AND btrim(averaging_period) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_averaging_periods existing
               WHERE lower(existing.averaging_period_name) =
                 lower(discrete.guideline_model_outputs.averaging_period)
             )
           ON CONFLICT (averaging_period_name) DO NOTHING;

           UPDATE discrete.guideline_model_outputs gmo
           SET averaging_period_id = gap.averaging_period_id
           FROM discrete.guideline_averaging_periods gap
           WHERE gmo.averaging_period_id IS NULL
             AND lower(gmo.averaging_period) = lower(gap.averaging_period_name);

           ALTER TABLE discrete.guideline_model_outputs
             DROP COLUMN averaging_period;
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_model_outputs IS
       'Named outputs produced by a guideline model, such as acute or chronic BLM copper guideline values.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_model_results (
         model_result_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         model_code TEXT NOT NULL,
         model_output_code TEXT NOT NULL,
         sample_id INTEGER NOT NULL
           REFERENCES discrete.samples(sample_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         guideline_value NUMERIC,
         output_status TEXT NOT NULL DEFAULT 'value',
         input_payload JSONB NOT NULL,
         input_hash TEXT NOT NULL,
         model_version TEXT,
         model_run_datetime TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         generated_by TEXT DEFAULT CURRENT_USER NOT NULL,
         source_artifact TEXT,
         message TEXT,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT guideline_model_results_output_fkey
           FOREIGN KEY (model_code, model_output_code)
           REFERENCES discrete.guideline_model_outputs(model_code, output_code)
           ON UPDATE CASCADE ON DELETE CASCADE,
         CONSTRAINT guideline_model_results_status_check
           CHECK (
             output_status IN (
               'value',
               'missing_input',
               'outside_calibrated_range',
               'model_error',
               'no_recommended_guideline',
               'site_specific'
             )
           ),
         CONSTRAINT guideline_model_results_value_required
           CHECK (
             (output_status = 'value' AND guideline_value IS NOT NULL)
             OR (output_status <> 'value')
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_model_results OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS discrete.guideline_model_results_signature_key;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX guideline_model_results_signature_key
       ON discrete.guideline_model_results (
         model_code,
         model_output_code,
         sample_id,
         input_hash,
         COALESCE(model_version, '')
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_model_results IS
       'Database cache of externally or database-calculated guideline model outputs keyed to the exact input signature used.';"
    )

    # Extend the existing guideline table ###################################
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_check_sql ON discrete.guidelines;"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.guidelines_validate_trg() CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       DROP CONSTRAINT IF EXISTS guidelines_guideline_name_publisher_key;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       DROP COLUMN IF EXISTS guideline_sql;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       ADD COLUMN IF NOT EXISTS guideline_code TEXT,
       ADD COLUMN IF NOT EXISTS parent_guideline_code TEXT,
       ADD COLUMN IF NOT EXISTS version_label TEXT,
       ADD COLUMN IF NOT EXISTS jurisdiction_id INTEGER
         REFERENCES discrete.guideline_jurisdictions(jurisdiction_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS jurisdiction_level_id INTEGER
         REFERENCES discrete.guideline_jurisdiction_levels(jurisdiction_level_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS protection_goal_id INTEGER
         REFERENCES discrete.guideline_protection_goals(protection_goal_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS exposure_duration_id INTEGER
         REFERENCES discrete.guideline_exposure_durations(exposure_duration_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS averaging_period_id INTEGER
         REFERENCES discrete.guideline_averaging_periods(averaging_period_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS comparison_operator_code TEXT
         REFERENCES discrete.guideline_comparison_operators(operator_code)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS source_document_title TEXT,
       ADD COLUMN IF NOT EXISTS source_url TEXT,
       ADD COLUMN IF NOT EXISTS source_page TEXT,
       ADD COLUMN IF NOT EXISTS source_table TEXT,
       ADD COLUMN IF NOT EXISTS source_section TEXT,
       ADD COLUMN IF NOT EXISTS source_effective_date DATE,
       ADD COLUMN IF NOT EXISTS source_retrieved_date DATE,
       ADD COLUMN IF NOT EXISTS valid_from DATE,
       ADD COLUMN IF NOT EXISTS valid_to DATE,
       ADD COLUMN IF NOT EXISTS source_revision TEXT,
       ADD COLUMN IF NOT EXISTS supersedes_guideline_id INTEGER
         REFERENCES discrete.guidelines(guideline_id)
         ON UPDATE CASCADE ON DELETE SET NULL,
       ADD COLUMN IF NOT EXISTS review_status TEXT NOT NULL DEFAULT 'draft',
       ADD COLUMN IF NOT EXISTS approved_by TEXT,
       ADD COLUMN IF NOT EXISTS approved_at TIMESTAMPTZ;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guidelines'
             AND column_name = 'jurisdiction'
         ) THEN
           INSERT INTO discrete.guideline_jurisdictions (
             jurisdiction_code, jurisdiction_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(jurisdiction), 10)),
             jurisdiction,
             800
           FROM discrete.guidelines
           WHERE jurisdiction IS NOT NULL
             AND btrim(jurisdiction) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_jurisdictions existing
               WHERE lower(existing.jurisdiction_name) =
                 lower(discrete.guidelines.jurisdiction)
             )
           ON CONFLICT (jurisdiction_name) DO NOTHING;

           UPDATE discrete.guidelines g
           SET jurisdiction_id = gj.jurisdiction_id
           FROM discrete.guideline_jurisdictions gj
           WHERE g.jurisdiction_id IS NULL
             AND lower(g.jurisdiction) = lower(gj.jurisdiction_name);

           ALTER TABLE discrete.guidelines DROP COLUMN jurisdiction;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guidelines'
             AND column_name = 'jurisdiction_level'
         ) THEN
           INSERT INTO discrete.guideline_jurisdiction_levels (
             jurisdiction_level_code, jurisdiction_level_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(jurisdiction_level), 10)),
             jurisdiction_level,
             800
           FROM discrete.guidelines
           WHERE jurisdiction_level IS NOT NULL
             AND btrim(jurisdiction_level) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_jurisdiction_levels existing
               WHERE lower(existing.jurisdiction_level_name) =
                 lower(discrete.guidelines.jurisdiction_level)
             )
           ON CONFLICT (jurisdiction_level_name) DO NOTHING;

           UPDATE discrete.guidelines g
           SET jurisdiction_level_id = gjl.jurisdiction_level_id
           FROM discrete.guideline_jurisdiction_levels gjl
           WHERE g.jurisdiction_level_id IS NULL
             AND lower(g.jurisdiction_level) = lower(gjl.jurisdiction_level_name);

           ALTER TABLE discrete.guidelines DROP COLUMN jurisdiction_level;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guidelines'
             AND column_name = 'protection_goal'
         ) THEN
           INSERT INTO discrete.guideline_protection_goals (
             protection_goal_code, protection_goal_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(protection_goal), 10)),
             protection_goal,
             800
           FROM discrete.guidelines
           WHERE protection_goal IS NOT NULL
             AND btrim(protection_goal) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_protection_goals existing
               WHERE lower(existing.protection_goal_name) =
                 lower(discrete.guidelines.protection_goal)
             )
           ON CONFLICT (protection_goal_name) DO NOTHING;

           UPDATE discrete.guidelines g
           SET protection_goal_id = gpg.protection_goal_id
           FROM discrete.guideline_protection_goals gpg
           WHERE g.protection_goal_id IS NULL
             AND lower(g.protection_goal) = lower(gpg.protection_goal_name);

           ALTER TABLE discrete.guidelines DROP COLUMN protection_goal;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guidelines'
             AND column_name = 'exposure_duration'
         ) THEN
           INSERT INTO discrete.guideline_exposure_durations (
             exposure_duration_code, exposure_duration_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(exposure_duration), 10)),
             exposure_duration,
             800
           FROM discrete.guidelines
           WHERE exposure_duration IS NOT NULL
             AND btrim(exposure_duration) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_exposure_durations existing
               WHERE lower(existing.exposure_duration_name) =
                 lower(discrete.guidelines.exposure_duration)
             )
           ON CONFLICT (exposure_duration_name) DO NOTHING;

           UPDATE discrete.guidelines g
           SET exposure_duration_id = ged.exposure_duration_id
           FROM discrete.guideline_exposure_durations ged
           WHERE g.exposure_duration_id IS NULL
             AND lower(g.exposure_duration) = lower(ged.exposure_duration_name);

           ALTER TABLE discrete.guidelines DROP COLUMN exposure_duration;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'guidelines'
             AND column_name = 'averaging_period'
         ) THEN
           INSERT INTO discrete.guideline_averaging_periods (
             averaging_period_code, averaging_period_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(averaging_period), 10)),
             averaging_period,
             800
           FROM discrete.guidelines
           WHERE averaging_period IS NOT NULL
             AND btrim(averaging_period) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.guideline_averaging_periods existing
               WHERE lower(existing.averaging_period_name) =
                 lower(discrete.guidelines.averaging_period)
             )
           ON CONFLICT (averaging_period_name) DO NOTHING;

           UPDATE discrete.guidelines g
           SET averaging_period_id = gap.averaging_period_id
           FROM discrete.guideline_averaging_periods gap
           WHERE g.averaging_period_id IS NULL
             AND lower(g.averaging_period) = lower(gap.averaging_period_name);

           ALTER TABLE discrete.guidelines DROP COLUMN averaging_period;
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "WITH ranked AS (
         SELECT
           jurisdiction_id,
           first_value(jurisdiction_id) OVER (
             PARTITION BY lower(btrim(jurisdiction_name))
             ORDER BY sort_order, jurisdiction_id
           ) AS keep_id
         FROM discrete.guideline_jurisdictions
       )
       UPDATE discrete.guidelines g
       SET jurisdiction_id = ranked.keep_id
       FROM ranked
       WHERE g.jurisdiction_id = ranked.jurisdiction_id
         AND ranked.jurisdiction_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           jurisdiction_id,
           first_value(jurisdiction_id) OVER (
             PARTITION BY lower(btrim(jurisdiction_name))
             ORDER BY sort_order, jurisdiction_id
           ) AS keep_id
         FROM discrete.guideline_jurisdictions
       )
       DELETE FROM discrete.guideline_jurisdictions gj
       USING ranked
       WHERE gj.jurisdiction_id = ranked.jurisdiction_id
         AND ranked.jurisdiction_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           jurisdiction_level_id,
           first_value(jurisdiction_level_id) OVER (
             PARTITION BY lower(btrim(jurisdiction_level_name))
             ORDER BY sort_order, jurisdiction_level_id
           ) AS keep_id
         FROM discrete.guideline_jurisdiction_levels
       )
       UPDATE discrete.guidelines g
       SET jurisdiction_level_id = ranked.keep_id
       FROM ranked
       WHERE g.jurisdiction_level_id = ranked.jurisdiction_level_id
         AND ranked.jurisdiction_level_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           jurisdiction_level_id,
           first_value(jurisdiction_level_id) OVER (
             PARTITION BY lower(btrim(jurisdiction_level_name))
             ORDER BY sort_order, jurisdiction_level_id
           ) AS keep_id
         FROM discrete.guideline_jurisdiction_levels
       )
       DELETE FROM discrete.guideline_jurisdiction_levels gjl
       USING ranked
       WHERE gjl.jurisdiction_level_id = ranked.jurisdiction_level_id
         AND ranked.jurisdiction_level_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           protection_goal_id,
           first_value(protection_goal_id) OVER (
             PARTITION BY lower(btrim(protection_goal_name))
             ORDER BY sort_order, protection_goal_id
           ) AS keep_id
         FROM discrete.guideline_protection_goals
       )
       UPDATE discrete.guidelines g
       SET protection_goal_id = ranked.keep_id
       FROM ranked
       WHERE g.protection_goal_id = ranked.protection_goal_id
         AND ranked.protection_goal_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           protection_goal_id,
           first_value(protection_goal_id) OVER (
             PARTITION BY lower(btrim(protection_goal_name))
             ORDER BY sort_order, protection_goal_id
           ) AS keep_id
         FROM discrete.guideline_protection_goals
       )
       DELETE FROM discrete.guideline_protection_goals gpg
       USING ranked
       WHERE gpg.protection_goal_id = ranked.protection_goal_id
         AND ranked.protection_goal_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           exposure_duration_id,
           first_value(exposure_duration_id) OVER (
             PARTITION BY lower(btrim(exposure_duration_name))
             ORDER BY sort_order, exposure_duration_id
           ) AS keep_id
         FROM discrete.guideline_exposure_durations
       )
       UPDATE discrete.guidelines g
       SET exposure_duration_id = ranked.keep_id
       FROM ranked
       WHERE g.exposure_duration_id = ranked.exposure_duration_id
         AND ranked.exposure_duration_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           exposure_duration_id,
           first_value(exposure_duration_id) OVER (
             PARTITION BY lower(btrim(exposure_duration_name))
             ORDER BY sort_order, exposure_duration_id
           ) AS keep_id
         FROM discrete.guideline_exposure_durations
       )
       UPDATE discrete.guideline_model_outputs gmo
       SET exposure_duration_id = ranked.keep_id
       FROM ranked
       WHERE gmo.exposure_duration_id = ranked.exposure_duration_id
         AND ranked.exposure_duration_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           exposure_duration_id,
           first_value(exposure_duration_id) OVER (
             PARTITION BY lower(btrim(exposure_duration_name))
             ORDER BY sort_order, exposure_duration_id
           ) AS keep_id
         FROM discrete.guideline_exposure_durations
       )
       DELETE FROM discrete.guideline_exposure_durations ged
       USING ranked
       WHERE ged.exposure_duration_id = ranked.exposure_duration_id
         AND ranked.exposure_duration_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           averaging_period_id,
           first_value(averaging_period_id) OVER (
             PARTITION BY lower(btrim(averaging_period_name))
             ORDER BY sort_order, averaging_period_id
           ) AS keep_id
         FROM discrete.guideline_averaging_periods
       )
       UPDATE discrete.guidelines g
       SET averaging_period_id = ranked.keep_id
       FROM ranked
       WHERE g.averaging_period_id = ranked.averaging_period_id
         AND ranked.averaging_period_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           averaging_period_id,
           first_value(averaging_period_id) OVER (
             PARTITION BY lower(btrim(averaging_period_name))
             ORDER BY sort_order, averaging_period_id
           ) AS keep_id
         FROM discrete.guideline_averaging_periods
       )
       UPDATE discrete.guideline_model_outputs gmo
       SET averaging_period_id = ranked.keep_id
       FROM ranked
       WHERE gmo.averaging_period_id = ranked.averaging_period_id
         AND ranked.averaging_period_id <> ranked.keep_id;

       WITH ranked AS (
         SELECT
           averaging_period_id,
           first_value(averaging_period_id) OVER (
             PARTITION BY lower(btrim(averaging_period_name))
             ORDER BY sort_order, averaging_period_id
           ) AS keep_id
         FROM discrete.guideline_averaging_periods
       )
       DELETE FROM discrete.guideline_averaging_periods gap
       USING ranked
       WHERE gap.averaging_period_id = ranked.averaging_period_id
         AND ranked.averaging_period_id <> ranked.keep_id;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_jurisdictions_name_lwr_key
         ON discrete.guideline_jurisdictions (lower(btrim(jurisdiction_name)));
       CREATE UNIQUE INDEX IF NOT EXISTS guideline_jurisdiction_levels_name_lwr_key
         ON discrete.guideline_jurisdiction_levels (lower(btrim(jurisdiction_level_name)));
       CREATE UNIQUE INDEX IF NOT EXISTS guideline_protection_goals_name_lwr_key
         ON discrete.guideline_protection_goals (lower(btrim(protection_goal_name)));
       CREATE UNIQUE INDEX IF NOT EXISTS guideline_exposure_durations_name_lwr_key
         ON discrete.guideline_exposure_durations (lower(btrim(exposure_duration_name)));
       CREATE UNIQUE INDEX IF NOT EXISTS guideline_averaging_periods_name_lwr_key
         ON discrete.guideline_averaging_periods (lower(btrim(averaging_period_name)));"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.guidelines
       SET comparison_operator_code = COALESCE(comparison_operator_code, 'lte'),
           valid_from = COALESCE(valid_from, DATE '1900-01-01'),
           guideline_code = COALESCE(
             guideline_code,
             'legacy-' || guideline_id::TEXT
           )
       WHERE comparison_operator_code IS NULL
          OR valid_from IS NULL
          OR guideline_code IS NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       ALTER COLUMN guideline_code SET NOT NULL,
       ALTER COLUMN comparison_operator_code SET DEFAULT 'lte',
       ALTER COLUMN comparison_operator_code SET NOT NULL,
       ALTER COLUMN valid_from SET DEFAULT DATE '1900-01-01',
       ALTER COLUMN valid_from SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF NOT EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conname = 'guidelines_valid_date_range'
             AND conrelid = 'discrete.guidelines'::regclass
         ) THEN
           ALTER TABLE discrete.guidelines
             ADD CONSTRAINT guidelines_valid_date_range
             CHECK (valid_to IS NULL OR valid_to >= valid_from);
         END IF;

         IF NOT EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conname = 'guidelines_review_status_check'
             AND conrelid = 'discrete.guidelines'::regclass
         ) THEN
           ALTER TABLE discrete.guidelines
             ADD CONSTRAINT guidelines_review_status_check
             CHECK (
               review_status IN (
                 'draft', 'reviewed', 'approved', 'retired', 'superseded'
               )
             );
         END IF;

         IF NOT EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conname = 'guidelines_code_key'
             AND conrelid = 'discrete.guidelines'::regclass
         ) THEN
           ALTER TABLE discrete.guidelines
             ADD CONSTRAINT guidelines_code_key UNIQUE (guideline_code);
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_parameter_state_speciation_idx
       ON discrete.guidelines (
         parameter_id, matrix_state_id, result_speciation_id
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_validity_idx
       ON discrete.guidelines (valid_from, valid_to);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_comparison_operator_idx
       ON discrete.guidelines (comparison_operator_code);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_applicability_idx
       ON discrete.guidelines (
         jurisdiction_id, jurisdiction_level_id, protection_goal_id,
         exposure_duration_id, averaging_period_id
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guidelines IS
       'One versioned published guideline or criterion that can be matched to analytical results and evaluated entirely in the database.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.guidelines.guideline_code IS
       'Stable unique code for this specific guideline version, exposure duration, and protection goal.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.guidelines.comparison_operator_code IS
       'How measured values are compared with the derived guideline value or values.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.guidelines.valid_from IS
       'First date on which this guideline version is applicable for interpretation.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.guidelines.valid_to IS
       'Last date on which this guideline version is applicable. NULL means still current.';"
    )

    # Declarative value rules ###############################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_value_rules (
         rule_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         guideline_id INTEGER NOT NULL
           REFERENCES discrete.guidelines(guideline_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         model_code TEXT,
         model_output_code TEXT,
         function_schema TEXT,
         function_name TEXT,
         bound_code TEXT DEFAULT 'upper',
         algorithm_code TEXT NOT NULL
           REFERENCES discrete.guideline_value_algorithms(algorithm_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         fixed_value NUMERIC,
         formula_sql TEXT,
         min_output_value NUMERIC,
         max_output_value NUMERIC,
         rounding_digits INTEGER,
         rounding_method TEXT NOT NULL DEFAULT 'none',
         missing_input_policy TEXT NOT NULL DEFAULT 'no_value',
         rule_priority INTEGER NOT NULL DEFAULT 100,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         precision_note TEXT,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT guideline_value_rules_bound_code_check
           CHECK (
             (algorithm_code = 'narrative' AND bound_code IS NULL)
             OR (
               algorithm_code <> 'narrative'
               AND bound_code IN ('lower', 'upper')
             )
           ),
         CONSTRAINT guideline_value_rules_rounding_method_check
           CHECK (rounding_method IN ('none', 'round', 'floor', 'ceiling')),
         CONSTRAINT guideline_value_rules_missing_input_policy_check
           CHECK (missing_input_policy IN ('error', 'no_value')),
         CONSTRAINT guideline_value_rules_output_bounds_check
           CHECK (
             min_output_value IS NULL OR max_output_value IS NULL
             OR min_output_value <= max_output_value
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_value_rules OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_value_rules
       ADD COLUMN IF NOT EXISTS model_code TEXT,
       ADD COLUMN IF NOT EXISTS model_output_code TEXT,
       ADD COLUMN IF NOT EXISTS function_schema TEXT,
       ADD COLUMN IF NOT EXISTS function_name TEXT;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF to_regclass('discrete.guideline_value_rules') IS NULL THEN
           RETURN;
         END IF;

         IF EXISTS (
           SELECT 1
           FROM discrete.guideline_value_rules
           WHERE bound_code = 'single'
             AND algorithm_code <> 'narrative'
         ) THEN
           RAISE EXCEPTION
             'bound_code = single is no longer supported for numeric guideline rules; migrate those rules to lower or upper before applying patch 47.';
         END IF;

         ALTER TABLE discrete.guideline_value_rules
           DROP CONSTRAINT IF EXISTS guideline_value_rules_bound_code_check;

         ALTER TABLE discrete.guideline_value_rules
           ALTER COLUMN bound_code DROP NOT NULL,
           ALTER COLUMN bound_code SET DEFAULT 'upper';

         UPDATE discrete.guideline_value_rules
         SET bound_code = NULL
         WHERE algorithm_code = 'narrative'
           AND bound_code = 'single';

         ALTER TABLE discrete.guideline_value_rules
           ADD CONSTRAINT guideline_value_rules_bound_code_check
           CHECK (
             (algorithm_code = 'narrative' AND bound_code IS NULL)
             OR (
               algorithm_code <> 'narrative'
               AND bound_code IN ('lower', 'upper')
             )
           );
       END $$;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF NOT EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conname = 'guideline_value_rules_model_output_fkey'
             AND conrelid = 'discrete.guideline_value_rules'::regclass
         ) THEN
           ALTER TABLE discrete.guideline_value_rules
             ADD CONSTRAINT guideline_value_rules_model_output_fkey
             FOREIGN KEY (model_code, model_output_code)
             REFERENCES discrete.guideline_model_outputs(model_code, output_code)
             ON UPDATE CASCADE
             ON DELETE RESTRICT;
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_value_rules_guideline_idx
       ON discrete.guideline_value_rules (
         guideline_id, active, rule_priority, bound_code
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_value_rules_model_idx
       ON discrete.guideline_value_rules (
         model_code, model_output_code
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_value_rules IS
       'Declarative rules that derive lower and/or upper numeric guideline bounds for a guideline. Narrative rules have no bound code.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.guideline_value_rules.formula_sql IS
       'Governed SQL scalar expression used only when a guideline cannot yet be represented by a declarative algorithm.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_rule_inputs (
         input_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         rule_id INTEGER NOT NULL
           REFERENCES discrete.guideline_value_rules(rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         input_code TEXT NOT NULL,
         input_name TEXT,
         parameter_id INTEGER NOT NULL
           REFERENCES public.parameters(parameter_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         matrix_state_id INTEGER NOT NULL
           REFERENCES public.matrix_states(matrix_state_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         sample_fraction_id INTEGER
           REFERENCES discrete.sample_fractions(sample_fraction_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         result_speciation_id INTEGER
           REFERENCES discrete.result_speciations(result_speciation_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         result_type INTEGER
           REFERENCES discrete.result_types(result_type_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         search_scope TEXT NOT NULL DEFAULT 'same_sample',
         aggregate_method TEXT NOT NULL DEFAULT 'single',
         allow_condition_value BOOLEAN NOT NULL DEFAULT FALSE,
         required BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT guideline_rule_inputs_code_key
           UNIQUE (rule_id, input_code),
         CONSTRAINT guideline_rule_inputs_scope_check
           CHECK (search_scope IN ('same_sample')),
         CONSTRAINT guideline_rule_inputs_aggregate_check
           CHECK (aggregate_method IN ('single', 'avg', 'min', 'max'))
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_rule_inputs OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_rule_inputs_rule_idx
       ON discrete.guideline_rule_inputs (rule_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_rule_inputs_parameter_idx
       ON discrete.guideline_rule_inputs (
         parameter_id, matrix_state_id, sample_fraction_id,
         result_speciation_id, result_type
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_rule_inputs IS
       'Sample-specific analytical results required to derive a guideline value, such as hardness, pH, or temperature.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_rule_coefficients (
         rule_id INTEGER NOT NULL
           REFERENCES discrete.guideline_value_rules(rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         coefficient_name TEXT NOT NULL,
         coefficient_value NUMERIC NOT NULL,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         PRIMARY KEY (rule_id, coefficient_name)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_rule_coefficients OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_rule_coefficients IS
       'Named numeric coefficients used by declarative guideline formula algorithms.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_lookup_values (
         lookup_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         rule_id INTEGER NOT NULL
           REFERENCES discrete.guideline_value_rules(rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         input_code TEXT NOT NULL,
         lower_bound NUMERIC,
         upper_bound NUMERIC,
         lower_inclusive BOOLEAN NOT NULL DEFAULT TRUE,
         upper_inclusive BOOLEAN NOT NULL DEFAULT TRUE,
         output_value NUMERIC,
         output_status TEXT NOT NULL DEFAULT 'value',
         output_label TEXT,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT guideline_lookup_values_status_check
           CHECK (
             output_status IN (
               'value', 'no_recommended_guideline', 'site_specific'
             )
           ),
         CONSTRAINT guideline_lookup_values_value_required
           CHECK (
             (output_status = 'value' AND output_value IS NOT NULL)
             OR (output_status <> 'value' AND output_value IS NULL)
           ),
         CONSTRAINT guideline_lookup_values_range_order
           CHECK (
             lower_bound IS NULL OR upper_bound IS NULL
             OR lower_bound < upper_bound
             OR (
               lower_bound = upper_bound
               AND lower_inclusive
               AND upper_inclusive
             )
           ),
         CONSTRAINT guideline_lookup_values_input_fkey
           FOREIGN KEY (rule_id, input_code)
           REFERENCES discrete.guideline_rule_inputs(rule_id, input_code)
           ON UPDATE CASCADE ON DELETE CASCADE
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_lookup_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_values_rule_input_idx
       ON discrete.guideline_lookup_values (rule_id, input_code, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_lookup_values IS
       'Non-overlapping numeric input ranges and corresponding output values for lookup-based guideline derivation.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_lookup_tables (
         lookup_table_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         rule_id INTEGER NOT NULL UNIQUE
           REFERENCES discrete.guideline_value_rules(rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         table_code TEXT NOT NULL,
         table_name TEXT,
         interpolation_method TEXT NOT NULL DEFAULT 'none',
         no_match_status TEXT NOT NULL DEFAULT 'no_matching_cell',
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         UNIQUE (lookup_table_id, rule_id),
         CONSTRAINT guideline_lookup_tables_interpolation_check
           CHECK (interpolation_method IN ('none')),
         CONSTRAINT guideline_lookup_tables_no_match_status_check
           CHECK (
             no_match_status IN (
               'no_matching_cell', 'no_recommended_guideline', 'site_specific'
             )
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_lookup_tables OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_lookup_tables_rule_code_key
       ON discrete.guideline_lookup_tables (rule_id, table_code);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_lookup_tables IS
       'Header rows for multidimensional lookup tables used by guideline lookup_grid rules.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_lookup_dimensions (
         dimension_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         lookup_table_id INTEGER NOT NULL,
         rule_id INTEGER NOT NULL,
         input_code TEXT NOT NULL,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         UNIQUE (lookup_table_id, input_code),
         UNIQUE (dimension_id, lookup_table_id),
         CONSTRAINT guideline_lookup_dimensions_table_fkey
           FOREIGN KEY (lookup_table_id, rule_id)
           REFERENCES discrete.guideline_lookup_tables(lookup_table_id, rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         CONSTRAINT guideline_lookup_dimensions_input_fkey
           FOREIGN KEY (rule_id, input_code)
           REFERENCES discrete.guideline_rule_inputs(rule_id, input_code)
           ON UPDATE CASCADE ON DELETE CASCADE
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_lookup_dimensions OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_dimensions_table_idx
       ON discrete.guideline_lookup_dimensions (lookup_table_id, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_lookup_dimensions IS
       'Input dimensions for multidimensional guideline lookup tables.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_lookup_cells (
         cell_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         lookup_table_id INTEGER NOT NULL
           REFERENCES discrete.guideline_lookup_tables(lookup_table_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         output_value NUMERIC,
         output_status TEXT NOT NULL DEFAULT 'value',
         output_label TEXT,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         UNIQUE (cell_id, lookup_table_id),
         CONSTRAINT guideline_lookup_cells_status_check
           CHECK (
             output_status IN (
               'value', 'no_recommended_guideline', 'site_specific'
             )
           ),
         CONSTRAINT guideline_lookup_cells_value_required
           CHECK (
             (output_status = 'value' AND output_value IS NOT NULL)
             OR (output_status <> 'value' AND output_value IS NULL)
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_lookup_cells OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_cells_table_idx
       ON discrete.guideline_lookup_cells (lookup_table_id, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_lookup_cells IS
       'Output cells for multidimensional guideline lookup tables.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.guideline_lookup_cell_ranges (
         cell_range_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         cell_id INTEGER NOT NULL,
         lookup_table_id INTEGER NOT NULL,
         dimension_id INTEGER NOT NULL,
         lower_bound NUMERIC,
         upper_bound NUMERIC,
         lower_inclusive BOOLEAN NOT NULL DEFAULT TRUE,
         upper_inclusive BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         UNIQUE (cell_id, dimension_id),
         CONSTRAINT guideline_lookup_cell_ranges_cell_fkey
           FOREIGN KEY (cell_id, lookup_table_id)
           REFERENCES discrete.guideline_lookup_cells(cell_id, lookup_table_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         CONSTRAINT guideline_lookup_cell_ranges_dimension_fkey
           FOREIGN KEY (dimension_id, lookup_table_id)
           REFERENCES discrete.guideline_lookup_dimensions(dimension_id, lookup_table_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         CONSTRAINT guideline_lookup_cell_ranges_order
           CHECK (
             lower_bound IS NULL OR upper_bound IS NULL
             OR lower_bound < upper_bound
             OR (
               lower_bound = upper_bound
               AND lower_inclusive
               AND upper_inclusive
             )
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guideline_lookup_cell_ranges OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_cell_ranges_table_idx
       ON discrete.guideline_lookup_cell_ranges (lookup_table_id, dimension_id);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.guideline_lookup_cell_ranges IS
       'Per-dimension numeric ranges that define each multidimensional lookup output cell.';"
    )

    # Validation triggers ###################################################
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.validate_guideline_value_rule()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         scan TEXT;
         explain_json JSONB;
         bad_schema TEXT;
         function_signature REGPROCEDURE;
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

           IF scan ~ '\\$[2-9]' THEN
             RAISE EXCEPTION
               'Only $1 may be used as a parameter placeholder in formula_sql.';
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
             AND schem <> ALL(ARRAY['discrete', 'public'])
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

         IF NEW.algorithm_code IN ('db_function', 'model_result_cache') THEN
           IF NEW.model_code IS NULL OR NEW.model_output_code IS NULL THEN
             RAISE EXCEPTION
               'model_code and model_output_code are required for % rules.',
               NEW.algorithm_code;
           END IF;
         ELSIF NEW.model_code IS NOT NULL OR NEW.model_output_code IS NOT NULL THEN
           RAISE EXCEPTION
             'model_code and model_output_code may only be populated for model-backed rules.';
         END IF;

         IF NEW.algorithm_code = 'db_function' THEN
           IF NEW.function_schema IS NULL OR NEW.function_name IS NULL THEN
             RAISE EXCEPTION
               'function_schema and function_name are required for db_function rules.';
           END IF;

           function_signature := to_regprocedure(
             format('%I.%I(jsonb)', NEW.function_schema, NEW.function_name)
           );

           IF function_signature IS NULL THEN
             RAISE EXCEPTION
               'Registered guideline function %.%(jsonb) does not exist.',
               NEW.function_schema,
               NEW.function_name;
           END IF;
         ELSIF NEW.function_schema IS NOT NULL OR NEW.function_name IS NOT NULL THEN
           RAISE EXCEPTION
             'function_schema and function_name may only be populated for db_function rules.';
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.validate_guideline_value_rule() OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_value_rule
       ON discrete.guideline_value_rules;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_value_rule
       BEFORE INSERT OR UPDATE
       ON discrete.guideline_value_rules
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_guideline_value_rule();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.validate_guideline_lookup_value()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         new_bounds TEXT;
         existing_record RECORD;
       BEGIN
         new_bounds := concat(
           CASE WHEN NEW.lower_inclusive THEN '[' ELSE '(' END,
           CASE WHEN NEW.upper_inclusive THEN ']' ELSE ')' END
         );

         IF isempty(numrange(NEW.lower_bound, NEW.upper_bound, new_bounds)) THEN
           RAISE EXCEPTION
             'Lookup range for rule_id %, input_code % is empty.',
             NEW.rule_id,
             NEW.input_code;
         END IF;

         SELECT glv.lookup_id
         INTO existing_record
         FROM discrete.guideline_lookup_values glv
         WHERE glv.rule_id = NEW.rule_id
           AND glv.input_code = NEW.input_code
           AND glv.lookup_id IS DISTINCT FROM NEW.lookup_id
           AND numrange(
             glv.lower_bound,
             glv.upper_bound,
             concat(
               CASE WHEN glv.lower_inclusive THEN '[' ELSE '(' END,
               CASE WHEN glv.upper_inclusive THEN ']' ELSE ')' END
             )
           ) && numrange(NEW.lower_bound, NEW.upper_bound, new_bounds)
         LIMIT 1;

         IF FOUND THEN
           RAISE EXCEPTION
             'Lookup range overlaps existing lookup_id % for rule_id %, input_code %.',
             existing_record.lookup_id,
             NEW.rule_id,
             NEW.input_code;
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.validate_guideline_lookup_value()
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_lookup_value
       ON discrete.guideline_lookup_values;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_lookup_value
       BEFORE INSERT OR UPDATE
       ON discrete.guideline_lookup_values
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_guideline_lookup_value();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.validate_guideline_lookup_cell_range()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         new_bounds TEXT;
         overlap_cell_id INTEGER;
         dimension_count INTEGER;
         current_cell_complete BOOLEAN;
       BEGIN
         new_bounds := concat(
           CASE WHEN NEW.lower_inclusive THEN '[' ELSE '(' END,
           CASE WHEN NEW.upper_inclusive THEN ']' ELSE ')' END
         );

         IF isempty(numrange(NEW.lower_bound, NEW.upper_bound, new_bounds)) THEN
           RAISE EXCEPTION
             'Lookup cell range for cell_id %, dimension_id % is empty.',
             NEW.cell_id,
             NEW.dimension_id;
         END IF;

         SELECT count(*)::INTEGER
         INTO dimension_count
         FROM discrete.guideline_lookup_dimensions gld
         WHERE gld.lookup_table_id = NEW.lookup_table_id;

         SELECT count(*) = dimension_count
         INTO current_cell_complete
         FROM discrete.guideline_lookup_cell_ranges glcr
         WHERE glcr.cell_id = NEW.cell_id;

         IF NOT current_cell_complete THEN
           RETURN NEW;
         END IF;

         SELECT other_cell.cell_id
         INTO overlap_cell_id
         FROM discrete.guideline_lookup_cells other_cell
         WHERE other_cell.lookup_table_id = NEW.lookup_table_id
           AND other_cell.cell_id <> NEW.cell_id
           AND (
             SELECT count(*) = dimension_count
             FROM discrete.guideline_lookup_cell_ranges other_range
             WHERE other_range.cell_id = other_cell.cell_id
           )
           AND NOT EXISTS (
             SELECT 1
             FROM discrete.guideline_lookup_dimensions dim
             JOIN discrete.guideline_lookup_cell_ranges this_range
               ON this_range.cell_id = NEW.cell_id
              AND this_range.dimension_id = dim.dimension_id
             JOIN discrete.guideline_lookup_cell_ranges other_range
               ON other_range.cell_id = other_cell.cell_id
              AND other_range.dimension_id = dim.dimension_id
             WHERE dim.lookup_table_id = NEW.lookup_table_id
               AND NOT (
                 numrange(
                   this_range.lower_bound,
                   this_range.upper_bound,
                   concat(
                     CASE WHEN this_range.lower_inclusive THEN '[' ELSE '(' END,
                     CASE WHEN this_range.upper_inclusive THEN ']' ELSE ')' END
                   )
                 ) && numrange(
                   other_range.lower_bound,
                   other_range.upper_bound,
                   concat(
                     CASE WHEN other_range.lower_inclusive THEN '[' ELSE '(' END,
                     CASE WHEN other_range.upper_inclusive THEN ']' ELSE ')' END
                   )
                 )
               )
           )
         LIMIT 1;

         IF overlap_cell_id IS NOT NULL THEN
           RAISE EXCEPTION
             'Lookup cell % overlaps existing cell % in lookup_table_id %.',
             NEW.cell_id,
             overlap_cell_id,
             NEW.lookup_table_id;
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.validate_guideline_lookup_cell_range()
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_lookup_cell_range
       ON discrete.guideline_lookup_cell_ranges;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_lookup_cell_range
       AFTER INSERT OR UPDATE
       ON discrete.guideline_lookup_cell_ranges
       FOR EACH ROW
       EXECUTE FUNCTION discrete.validate_guideline_lookup_cell_range();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guideline_rule_input_units
       ON discrete.guideline_rule_inputs;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_guideline_rule_input_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id
       ON discrete.guideline_rule_inputs
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guideline_model_input_units
       ON discrete.guideline_model_inputs;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_guideline_model_input_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id
       ON discrete.guideline_model_inputs
       FOR EACH ROW EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )

    # Derivation functions ##################################################
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.guideline_apply_rounding(
         in_value NUMERIC,
         in_digits INTEGER,
         in_method TEXT
       )
       RETURNS NUMERIC
       LANGUAGE plpgsql
       IMMUTABLE
       AS $function$
       DECLARE
         scale_factor NUMERIC;
       BEGIN
         IF in_value IS NULL OR in_method IS NULL OR in_method = 'none' THEN
           RETURN in_value;
         END IF;

         IF in_digits IS NULL THEN
           in_digits := 0;
         END IF;

         IF in_method = 'round' THEN
           RETURN round(in_value, in_digits);
         END IF;

         scale_factor := power(10::NUMERIC, in_digits);

         IF in_method = 'floor' THEN
           RETURN floor(in_value * scale_factor) / scale_factor;
         ELSIF in_method = 'ceiling' THEN
           RETURN ceiling(in_value * scale_factor) / scale_factor;
         END IF;

         RAISE EXCEPTION 'Unsupported rounding method: %', in_method;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.guideline_apply_rounding(NUMERIC, INTEGER, TEXT)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.guideline_get_input_value(
         in_sample_id INTEGER,
         in_input_id INTEGER
       )
       RETURNS TABLE(
         input_id INTEGER,
         input_code TEXT,
         input_value NUMERIC,
         source_result_id INTEGER,
         status TEXT,
         message TEXT
       )
       LANGUAGE plpgsql
       STABLE
       AS $function$
       DECLARE
         input_row RECORD;
         matching_count INTEGER;
         usable_count INTEGER;
         selected_result_id INTEGER;
         selected_value NUMERIC;
       BEGIN
         SELECT gri.*
         INTO input_row
         FROM discrete.guideline_rule_inputs gri
         WHERE gri.input_id = in_input_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'No guideline input found with input_id %.',
             in_input_id;
         END IF;

         IF in_sample_id IS NULL THEN
           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                  NULL::INTEGER, 'missing_sample_id'::TEXT,
                  'A sample_id is required to derive this input.'::TEXT;
           RETURN;
         END IF;

         IF input_row.search_scope <> 'same_sample' THEN
           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                  NULL::INTEGER, 'unsupported_search_scope'::TEXT,
                  format(
                    'Unsupported input search_scope: %s.',
                    input_row.search_scope
                  )::TEXT;
           RETURN;
         END IF;

         WITH candidates AS (
           SELECT
             r.result_id,
             CASE
               WHEN r.result IS NOT NULL THEN r.result
               WHEN input_row.allow_condition_value THEN r.result_condition_value
               ELSE NULL
             END AS value_for_use
           FROM discrete.results r
           WHERE r.sample_id = in_sample_id
             AND r.parameter_id = input_row.parameter_id
             AND (
               input_row.matrix_state_id IS NULL
               OR r.matrix_state_id = input_row.matrix_state_id
             )
             AND (
               input_row.sample_fraction_id IS NULL
               OR r.sample_fraction_id IS NOT DISTINCT FROM
                 input_row.sample_fraction_id
             )
             AND (
               input_row.result_speciation_id IS NULL
               OR r.result_speciation_id IS NOT DISTINCT FROM
                 input_row.result_speciation_id
             )
             AND (
               input_row.result_type IS NULL
               OR r.result_type IS NOT DISTINCT FROM input_row.result_type
             )
         )
         SELECT count(*)::INTEGER,
                count(value_for_use)::INTEGER
         INTO matching_count, usable_count
         FROM candidates;

         IF matching_count = 0 THEN
           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                  NULL::INTEGER, 'missing_input'::TEXT,
                  format(
                    'No matching result found for input %s on sample_id %s.',
                    input_row.input_code,
                    in_sample_id
                  )::TEXT;
           RETURN;
         END IF;

         IF usable_count = 0 THEN
           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                  NULL::INTEGER, 'missing_input_value'::TEXT,
                  format(
                    'Matching result for input %s has no usable numeric value.',
                    input_row.input_code
                  )::TEXT;
           RETURN;
         END IF;

         IF usable_count > 1 AND input_row.aggregate_method = 'single' THEN
           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                  NULL::INTEGER, 'ambiguous_input'::TEXT,
                  format(
                    'Input %s matched %s usable results; specify an aggregate method or narrower input qualifiers.',
                    input_row.input_code,
                    usable_count
                  )::TEXT;
           RETURN;
         END IF;

         WITH candidates AS (
           SELECT
             r.result_id,
             CASE
               WHEN r.result IS NOT NULL THEN r.result
               WHEN input_row.allow_condition_value THEN r.result_condition_value
               ELSE NULL
             END AS value_for_use
           FROM discrete.results r
           WHERE r.sample_id = in_sample_id
             AND r.parameter_id = input_row.parameter_id
             AND (
               input_row.matrix_state_id IS NULL
               OR r.matrix_state_id = input_row.matrix_state_id
             )
             AND (
               input_row.sample_fraction_id IS NULL
               OR r.sample_fraction_id IS NOT DISTINCT FROM
                 input_row.sample_fraction_id
             )
             AND (
               input_row.result_speciation_id IS NULL
               OR r.result_speciation_id IS NOT DISTINCT FROM
                 input_row.result_speciation_id
             )
             AND (
               input_row.result_type IS NULL
               OR r.result_type IS NOT DISTINCT FROM input_row.result_type
             )
         )
         SELECT
           CASE input_row.aggregate_method
             WHEN 'avg' THEN avg(value_for_use)
             WHEN 'min' THEN min(value_for_use)
             WHEN 'max' THEN max(value_for_use)
             ELSE min(value_for_use)
           END,
           CASE
             WHEN input_row.aggregate_method = 'single'
               THEN min(result_id)
             ELSE NULL::INTEGER
           END
         INTO selected_value, selected_result_id
         FROM candidates
         WHERE value_for_use IS NOT NULL;

         RETURN QUERY
         SELECT input_row.input_id, input_row.input_code, selected_value,
                selected_result_id, 'value'::TEXT, NULL::TEXT;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.guideline_get_input_value(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.guideline_collect_rule_inputs(
         in_rule_id INTEGER,
         in_sample_id INTEGER DEFAULT NULL
       )
       RETURNS TABLE(
         rule_id INTEGER,
         derivation_inputs JSONB,
         input_hash TEXT,
         output_status TEXT,
         message TEXT
       )
       LANGUAGE sql
       STABLE
       AS $function$
         WITH collected AS (
           SELECT
             gri.input_id,
             gri.input_code,
             gri.input_name,
             gri.required,
             resolved.input_value AS raw_input_value,
             CASE
               WHEN resolved.status = 'value'
                    AND gmi.bounds_action = 'clamp'
                    AND gmi.lower_calibrated_bound IS NOT NULL
                    AND resolved.input_value < gmi.lower_calibrated_bound
                 THEN gmi.lower_calibrated_bound
               WHEN resolved.status = 'value'
                    AND gmi.bounds_action = 'clamp'
                    AND gmi.upper_calibrated_bound IS NOT NULL
                    AND resolved.input_value > gmi.upper_calibrated_bound
                 THEN gmi.upper_calibrated_bound
               ELSE resolved.input_value
             END AS input_value,
             resolved.source_result_id,
             CASE
               WHEN resolved.status = 'value'
                    AND gmi.bounds_action = 'reject'
                    AND (
                      (
                        gmi.lower_calibrated_bound IS NOT NULL
                        AND resolved.input_value < gmi.lower_calibrated_bound
                      )
                      OR (
                        gmi.upper_calibrated_bound IS NOT NULL
                        AND resolved.input_value > gmi.upper_calibrated_bound
                      )
                    )
                 THEN 'outside_calibrated_range'
               ELSE resolved.status
             END AS status,
             NULLIF(
               concat_ws(
                 ' ',
                 resolved.message,
                 CASE
                   WHEN resolved.status = 'value'
                        AND (
                          (
                            gmi.lower_calibrated_bound IS NOT NULL
                            AND resolved.input_value < gmi.lower_calibrated_bound
                          )
                          OR (
                            gmi.upper_calibrated_bound IS NOT NULL
                            AND resolved.input_value > gmi.upper_calibrated_bound
                          )
                        )
                     THEN format(
                       'Input %s value %s is outside the calibrated range%s%s; bounds action is %s.',
                       gri.input_code,
                       resolved.input_value,
                       CASE
                         WHEN gmi.lower_calibrated_bound IS NULL THEN ''
                         ELSE format(' lower %s', gmi.lower_calibrated_bound)
                       END,
                       CASE
                         WHEN gmi.upper_calibrated_bound IS NULL THEN ''
                         ELSE format(' upper %s', gmi.upper_calibrated_bound)
                       END,
                       gmi.bounds_action
                     )
                   ELSE NULL
                 END
               ),
               ''
             ) AS message,
             gmi.lower_calibrated_bound,
             gmi.upper_calibrated_bound,
             gmi.bounds_action
           FROM discrete.guideline_rule_inputs gri
           JOIN discrete.guideline_value_rules gr
             ON gr.rule_id = gri.rule_id
           LEFT JOIN discrete.guideline_model_inputs gmi
             ON gmi.model_code = gr.model_code
            AND gmi.input_code = gri.input_code
           CROSS JOIN LATERAL discrete.guideline_get_input_value(
             in_sample_id,
             gri.input_id
           ) resolved
           WHERE gri.rule_id = in_rule_id
         ),
         payload AS (
           SELECT
             COALESCE(
               jsonb_agg(
                 jsonb_build_object(
                   'input_id', input_id,
                   'input_code', input_code,
                   'input_name', input_name,
                   'value', input_value,
                   'raw_value', raw_input_value,
                   'source_result_id', source_result_id,
                   'status', status,
                   'required', required,
                   'message', message,
                   'lower_calibrated_bound', lower_calibrated_bound,
                   'upper_calibrated_bound', upper_calibrated_bound,
                   'bounds_action', bounds_action
                 )
                 ORDER BY input_code
               ),
               '[]'::JSONB
             ) AS derivation_inputs,
             COALESCE(
               bool_or(required AND status <> 'value'),
               FALSE
             ) AS has_required_failure,
             string_agg(DISTINCT status, ', ' ORDER BY status)
               FILTER (WHERE required AND status <> 'value') AS failure_status,
             string_agg(message, ' ' ORDER BY input_code)
               FILTER (WHERE message IS NOT NULL) AS failure_message
           FROM collected
         )
         SELECT
           in_rule_id,
           derivation_inputs,
           md5(derivation_inputs::TEXT),
           CASE
             WHEN has_required_failure THEN failure_status
             ELSE 'value'
           END AS output_status,
           failure_message AS message
         FROM payload;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.guideline_collect_rule_inputs(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.evaluate_guideline_rule(
         in_rule_id INTEGER,
         in_sample_id INTEGER DEFAULT NULL
       )
       RETURNS TABLE(
         rule_id INTEGER,
         guideline_id INTEGER,
         bound_code TEXT,
         guideline_value NUMERIC,
         output_status TEXT,
         derivation_inputs JSONB,
         message TEXT
       )
       LANGUAGE plpgsql
       STABLE
       AS $function$
       DECLARE
         rule_row RECORD;
         input_row RECORD;
         input_result RECORD;
         lookup_row RECORD;
         input_count INTEGER;
         coefficient_intercept NUMERIC;
         coefficient_slope NUMERIC;
         coefficient_factor NUMERIC;
         coefficient_exponent NUMERIC;
         computed_value NUMERIC;
         computed_status TEXT := 'value';
         computed_message TEXT;
         input_payload JSONB := '[]'::JSONB;
         collected_inputs RECORD;
         model_result_row RECORD;
         lookup_table_row RECORD;
         lookup_cell_row RECORD;
       BEGIN
         SELECT gr.*
         INTO rule_row
         FROM discrete.guideline_value_rules gr
         WHERE gr.rule_id = in_rule_id
           AND gr.active;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'No active guideline value rule found with rule_id %.',
             in_rule_id;
         END IF;

         IF rule_row.algorithm_code = 'constant' THEN
           computed_value := rule_row.fixed_value;

         ELSIF rule_row.algorithm_code = 'narrative' THEN
           computed_status := 'narrative';
           computed_message := COALESCE(
             rule_row.note,
             'This guideline is narrative or non-numeric.'
           );

         ELSIF rule_row.algorithm_code IN (
           'lookup_range', 'linear', 'log_linear', 'power'
         ) THEN
           SELECT count(*)::INTEGER
           INTO input_count
           FROM discrete.guideline_rule_inputs gri
           WHERE gri.rule_id = rule_row.rule_id;

           IF input_count <> 1 THEN
             RAISE EXCEPTION
               'Algorithm % requires exactly one input for rule_id %, found %.',
               rule_row.algorithm_code,
               rule_row.rule_id,
               input_count;
           END IF;

           SELECT gri.*
           INTO input_row
           FROM discrete.guideline_rule_inputs gri
           WHERE gri.rule_id = rule_row.rule_id
           ORDER BY gri.input_code
           LIMIT 1;

           SELECT *
           INTO input_result
           FROM discrete.guideline_get_input_value(
             in_sample_id,
             input_row.input_id
           );

           input_payload := jsonb_build_array(
             jsonb_build_object(
               'input_id', input_result.input_id,
               'input_code', input_result.input_code,
               'value', input_result.input_value,
               'source_result_id', input_result.source_result_id,
               'status', input_result.status,
               'message', input_result.message
             )
           );

           IF input_result.status <> 'value' THEN
             IF rule_row.missing_input_policy = 'error' THEN
               RAISE EXCEPTION
                 'Could not derive guideline rule %. Input status: %. %',
                 rule_row.rule_id,
                 input_result.status,
                 COALESCE(input_result.message, '');
             END IF;

             RETURN QUERY
             SELECT rule_row.rule_id, rule_row.guideline_id,
                    rule_row.bound_code, NULL::NUMERIC,
                    input_result.status,
                    input_payload,
                    input_result.message;
             RETURN;
           END IF;

           IF rule_row.algorithm_code = 'lookup_range' THEN
             SELECT glv.*
             INTO lookup_row
             FROM discrete.guideline_lookup_values glv
             WHERE glv.rule_id = rule_row.rule_id
               AND glv.input_code = input_row.input_code
               AND numrange(
                 glv.lower_bound,
                 glv.upper_bound,
                 concat(
                   CASE WHEN glv.lower_inclusive THEN '[' ELSE '(' END,
                   CASE WHEN glv.upper_inclusive THEN ']' ELSE ')' END
                 )
               ) @> input_result.input_value
             ORDER BY glv.sort_order, glv.lookup_id
             LIMIT 1;

             IF NOT FOUND THEN
               computed_status := 'no_matching_range';
               computed_message := format(
                 'No lookup range matched input %s value %s.',
                 input_row.input_code,
                 input_result.input_value
               );
             ELSIF lookup_row.output_status <> 'value' THEN
               computed_status := lookup_row.output_status;
               computed_message := lookup_row.note;
             ELSE
               computed_value := lookup_row.output_value;
               input_payload := jsonb_set(
                 input_payload,
                 '{0,lookup_id}',
                 to_jsonb(lookup_row.lookup_id),
                 TRUE
               );
             END IF;

           ELSIF rule_row.algorithm_code = 'linear' THEN
             SELECT
               max(coefficient_value) FILTER (
                 WHERE coefficient_name = 'intercept'
               ),
               max(coefficient_value) FILTER (
                 WHERE coefficient_name = 'slope'
               )
             INTO coefficient_intercept, coefficient_slope
             FROM discrete.guideline_rule_coefficients
             WHERE rule_id = rule_row.rule_id;

             IF coefficient_intercept IS NULL OR coefficient_slope IS NULL THEN
               RAISE EXCEPTION
                 'Linear rule_id % requires intercept and slope coefficients.',
                 rule_row.rule_id;
             END IF;

             computed_value :=
               coefficient_intercept + coefficient_slope * input_result.input_value;

           ELSIF rule_row.algorithm_code = 'log_linear' THEN
             SELECT
               max(coefficient_value) FILTER (
                 WHERE coefficient_name = 'intercept'
               ),
               max(coefficient_value) FILTER (
                 WHERE coefficient_name = 'slope'
               )
             INTO coefficient_intercept, coefficient_slope
             FROM discrete.guideline_rule_coefficients
             WHERE rule_id = rule_row.rule_id;

             IF coefficient_intercept IS NULL OR coefficient_slope IS NULL THEN
               RAISE EXCEPTION
                 'Log-linear rule_id % requires intercept and slope coefficients.',
                 rule_row.rule_id;
             END IF;

             IF input_result.input_value <= 0 THEN
               computed_status := 'invalid_input_value';
               computed_message := format(
                 'Input %s must be greater than zero for log-linear calculation.',
                 input_row.input_code
               );
             ELSE
               computed_value := exp(
                 coefficient_intercept +
                   coefficient_slope * ln(input_result.input_value)
               );
             END IF;

           ELSIF rule_row.algorithm_code = 'power' THEN
             SELECT
               max(coefficient_value) FILTER (
                 WHERE coefficient_name = 'factor'
               ),
               max(coefficient_value) FILTER (
                 WHERE coefficient_name = 'exponent'
               )
             INTO coefficient_factor, coefficient_exponent
             FROM discrete.guideline_rule_coefficients
             WHERE rule_id = rule_row.rule_id;

             IF coefficient_factor IS NULL OR coefficient_exponent IS NULL THEN
               RAISE EXCEPTION
                 'Power rule_id % requires factor and exponent coefficients.',
                 rule_row.rule_id;
             END IF;

             IF input_result.input_value <= 0 THEN
               computed_status := 'invalid_input_value';
               computed_message := format(
                 'Input %s must be greater than zero for power calculation.',
                 input_row.input_code
               );
             ELSE
               computed_value := coefficient_factor * exp(
                 coefficient_exponent * ln(input_result.input_value)
               );
             END IF;
           END IF;

         ELSIF rule_row.algorithm_code = 'lookup_grid' THEN
           SELECT *
           INTO collected_inputs
           FROM discrete.guideline_collect_rule_inputs(
             rule_row.rule_id,
             in_sample_id
           );

           input_payload := jsonb_build_object(
             'inputs', collected_inputs.derivation_inputs
           );

           IF collected_inputs.output_status <> 'value' THEN
             IF rule_row.missing_input_policy = 'error' THEN
               RAISE EXCEPTION
                 'Could not derive guideline rule %. Input status: %. %',
                 rule_row.rule_id,
                 collected_inputs.output_status,
                 COALESCE(collected_inputs.message, '');
             END IF;

             RETURN QUERY
             SELECT rule_row.rule_id, rule_row.guideline_id,
                    rule_row.bound_code, NULL::NUMERIC,
                    collected_inputs.output_status,
                    input_payload,
                    collected_inputs.message;
             RETURN;
           END IF;

           SELECT glt.*
           INTO lookup_table_row
           FROM discrete.guideline_lookup_tables glt
           WHERE glt.rule_id = rule_row.rule_id;

           IF NOT FOUND THEN
             computed_status := 'configuration_error';
             computed_message := format(
               'No multidimensional lookup table is registered for rule_id %.',
               rule_row.rule_id
             );
           ELSE
             SELECT glc.*
             INTO lookup_cell_row
             FROM discrete.guideline_lookup_cells glc
             WHERE glc.lookup_table_id = lookup_table_row.lookup_table_id
               AND NOT EXISTS (
                 SELECT 1
                 FROM discrete.guideline_lookup_dimensions gld
                 LEFT JOIN discrete.guideline_lookup_cell_ranges glcr
                   ON glcr.cell_id = glc.cell_id
                  AND glcr.dimension_id = gld.dimension_id
                 LEFT JOIN LATERAL (
                   SELECT
                     (input_item->>'value')::NUMERIC AS input_value,
                     input_item->>'status' AS input_status
                   FROM jsonb_array_elements(
                     collected_inputs.derivation_inputs
                   ) input_item
                   WHERE input_item->>'input_code' = gld.input_code
                   LIMIT 1
                 ) resolved_input ON TRUE
                 WHERE gld.lookup_table_id = lookup_table_row.lookup_table_id
                   AND (
                     resolved_input.input_status IS DISTINCT FROM 'value'
                     OR glcr.cell_range_id IS NULL
                     OR NOT (
                       numrange(
                         glcr.lower_bound,
                         glcr.upper_bound,
                         concat(
                           CASE WHEN glcr.lower_inclusive THEN '[' ELSE '(' END,
                           CASE WHEN glcr.upper_inclusive THEN ']' ELSE ')' END
                         )
                       ) @> resolved_input.input_value
                     )
                   )
               )
             ORDER BY glc.sort_order, glc.cell_id
             LIMIT 1;

             IF NOT FOUND THEN
               computed_status := lookup_table_row.no_match_status;
               computed_message := format(
                 'No lookup grid cell matched rule_id %.',
                 rule_row.rule_id
               );
               input_payload := input_payload || jsonb_build_object(
                 'lookup_table_id',
                 lookup_table_row.lookup_table_id
               );
             ELSIF lookup_cell_row.output_status <> 'value' THEN
               computed_status := lookup_cell_row.output_status;
               computed_message := lookup_cell_row.note;
               input_payload := input_payload || jsonb_build_object(
                 'lookup_table_id',
                 lookup_table_row.lookup_table_id,
                 'lookup_cell_id',
                 lookup_cell_row.cell_id
               );
             ELSE
               computed_value := lookup_cell_row.output_value;
               input_payload := input_payload || jsonb_build_object(
                 'lookup_table_id',
                 lookup_table_row.lookup_table_id,
                 'lookup_cell_id',
                 lookup_cell_row.cell_id,
                 'lookup_label',
                 lookup_cell_row.output_label
               );
             END IF;
           END IF;

         ELSIF rule_row.algorithm_code = 'sql_scalar' THEN
           IF in_sample_id IS NULL AND rule_row.formula_sql ~ '\\$1' THEN
             IF rule_row.missing_input_policy = 'error' THEN
               RAISE EXCEPTION
                 'rule_id % requires a sample_id for sql_scalar formula.',
                 rule_row.rule_id;
             END IF;
             computed_status := 'missing_sample_id';
             computed_message := 'A sample_id is required to derive this guideline value.';
           ELSIF rule_row.formula_sql ~ '\\$1' THEN
             EXECUTE rule_row.formula_sql
             INTO STRICT computed_value
             USING in_sample_id;
           ELSE
             EXECUTE rule_row.formula_sql
             INTO STRICT computed_value;
           END IF;

         ELSIF rule_row.algorithm_code IN ('db_function', 'model_result_cache') THEN
           SELECT *
           INTO collected_inputs
           FROM discrete.guideline_collect_rule_inputs(
             rule_row.rule_id,
             in_sample_id
           );

           input_payload := jsonb_build_object(
             'inputs', collected_inputs.derivation_inputs,
             'input_hash', collected_inputs.input_hash,
             'model_code', rule_row.model_code,
             'model_output_code', rule_row.model_output_code
           );

           IF collected_inputs.output_status <> 'value' THEN
             IF rule_row.missing_input_policy = 'error' THEN
               RAISE EXCEPTION
                 'Could not derive guideline rule %. Input status: %. %',
                 rule_row.rule_id,
                 collected_inputs.output_status,
                 COALESCE(collected_inputs.message, '');
             END IF;

             RETURN QUERY
             SELECT rule_row.rule_id, rule_row.guideline_id,
                    rule_row.bound_code, NULL::NUMERIC,
                    collected_inputs.output_status,
                    input_payload,
                    collected_inputs.message;
             RETURN;
           END IF;

           IF rule_row.algorithm_code = 'db_function' THEN
             computed_message := collected_inputs.message;
             input_payload := input_payload || jsonb_build_object(
               'function_schema', rule_row.function_schema,
               'function_name', rule_row.function_name
             );

             EXECUTE format(
               'SELECT %I.%I($1::jsonb)::numeric',
               rule_row.function_schema,
               rule_row.function_name
             )
             INTO computed_value
             USING collected_inputs.derivation_inputs;

             IF computed_value IS NULL THEN
               computed_status := 'no_value';
               computed_message := format(
                 'Guideline function %.% returned NULL.',
                 rule_row.function_schema,
                 rule_row.function_name
               );
             END IF;
           ELSE
             SELECT gmr.*
             INTO model_result_row
             FROM discrete.guideline_model_results gmr
             WHERE gmr.model_code = rule_row.model_code
               AND gmr.model_output_code = rule_row.model_output_code
               AND gmr.sample_id = in_sample_id
               AND gmr.input_hash = collected_inputs.input_hash
             ORDER BY gmr.model_run_datetime DESC, gmr.model_result_id DESC
             LIMIT 1;

             IF NOT FOUND THEN
               computed_status := 'missing_model_result';
               computed_message := format(
                 'No stored model result found for model %s output %s, sample_id %s, input hash %s.',
                 rule_row.model_code,
                 rule_row.model_output_code,
                 in_sample_id,
                 collected_inputs.input_hash
               );
             ELSE
               computed_value := model_result_row.guideline_value;
               computed_status := model_result_row.output_status;
               computed_message := concat_ws(
                 ' ',
                 collected_inputs.message,
                 model_result_row.message
               );
               input_payload := input_payload || jsonb_build_object(
                 'model_result_id', model_result_row.model_result_id,
                 'model_version', model_result_row.model_version,
                 'model_run_datetime', model_result_row.model_run_datetime,
                 'source_artifact', model_result_row.source_artifact
               );
             END IF;
           END IF;

         ELSE
           RAISE EXCEPTION
             'Unsupported guideline algorithm: %.',
             rule_row.algorithm_code;
         END IF;

         IF computed_value IS NOT NULL THEN
           IF rule_row.min_output_value IS NOT NULL
              AND computed_value < rule_row.min_output_value THEN
             computed_value := rule_row.min_output_value;
             computed_message := concat_ws(
               ' ',
               computed_message,
               'Minimum output cap applied.'
             );
           END IF;

           IF rule_row.max_output_value IS NOT NULL
              AND computed_value > rule_row.max_output_value THEN
             computed_value := rule_row.max_output_value;
             computed_message := concat_ws(
               ' ',
               computed_message,
               'Maximum output cap applied.'
             );
           END IF;

           computed_value := discrete.guideline_apply_rounding(
             computed_value,
             rule_row.rounding_digits,
             rule_row.rounding_method
           );
         END IF;

         RETURN QUERY
         SELECT rule_row.rule_id, rule_row.guideline_id,
                rule_row.bound_code, computed_value,
                computed_status, input_payload, computed_message;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.evaluate_guideline_rule(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.evaluate_guideline(
         in_guideline_id INTEGER,
         in_sample_id INTEGER DEFAULT NULL
       )
       RETURNS TABLE(
         rule_id INTEGER,
         guideline_id INTEGER,
         bound_code TEXT,
         guideline_value NUMERIC,
         output_status TEXT,
         derivation_inputs JSONB,
         message TEXT
       )
       LANGUAGE sql
       STABLE
       AS $function$
         SELECT evaluated.*
         FROM discrete.guideline_value_rules gr
         CROSS JOIN LATERAL discrete.evaluate_guideline_rule(
           gr.rule_id,
           in_sample_id
         ) evaluated
         WHERE gr.guideline_id = in_guideline_id
           AND gr.active
         ORDER BY gr.rule_priority, gr.rule_id;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.evaluate_guideline(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.get_guideline_value(
         in_guideline_id INTEGER,
         in_sample_id INTEGER DEFAULT NULL
       )
       RETURNS NUMERIC
       SET search_path = discrete, public
       LANGUAGE plpgsql
       STABLE
       AS $fn$
       DECLARE
         result NUMERIC;
       BEGIN
         SELECT eg.guideline_value
         INTO result
         FROM discrete.evaluate_guideline(
           in_guideline_id,
           in_sample_id
         ) eg
         WHERE eg.output_status = 'value'
         ORDER BY
           CASE eg.bound_code
             WHEN 'upper' THEN 1
             WHEN 'lower' THEN 2
             ELSE 4
           END,
           eg.rule_id
         LIMIT 1;

         IF result IS NULL THEN
           RAISE EXCEPTION
             'Guideline % did not produce a scalar numeric value.',
             in_guideline_id;
         END IF;

         RETURN result;
       END;
       $fn$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.get_guideline_value(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.applicable_guideline_rules_for_result(
         in_result_id INTEGER,
         in_as_of_date DATE DEFAULT CURRENT_DATE,
         in_include_unresolved BOOLEAN DEFAULT TRUE
       )
       RETURNS TABLE(
         result_id INTEGER,
         sample_id INTEGER,
         guideline_id INTEGER,
         rule_id INTEGER,
         guideline_code TEXT,
         guideline_name TEXT,
         publisher_name TEXT,
         series_name TEXT,
         jurisdiction TEXT,
         protection_goal TEXT,
         exposure_duration TEXT,
         averaging_period TEXT,
         parameter_id INTEGER,
         parameter_name TEXT,
         matrix_state_id INTEGER,
         matrix_state_code TEXT,
         units TEXT,
         bound_code TEXT,
         comparison_operator_code TEXT,
         comparison_symbol TEXT,
         result_value NUMERIC,
         guideline_value NUMERIC,
         output_status TEXT,
         comparison_status TEXT,
         derivation_inputs JSONB,
         message TEXT,
         source_document_title TEXT,
         source_url TEXT,
         source_page TEXT,
         source_table TEXT,
         source_section TEXT
       )
       LANGUAGE sql
       STABLE
       AS $function$
         WITH result_row AS (
           SELECT
             r.result_id,
             r.sample_id,
             s.media_id,
             s.datetime::DATE AS sample_date,
             r.parameter_id,
             p.param_name AS parameter_name,
             r.matrix_state_id,
             ms.matrix_state_code,
             public.get_parameter_unit_name(
               r.parameter_id,
               r.matrix_state_id
             ) AS units,
             r.sample_fraction_id,
             r.result_speciation_id,
             r.result AS result_value
           FROM discrete.results r
           JOIN discrete.samples s
             ON s.sample_id = r.sample_id
           LEFT JOIN public.parameters p
             ON p.parameter_id = r.parameter_id
           LEFT JOIN public.matrix_states ms
             ON ms.matrix_state_id = r.matrix_state_id
           WHERE r.result_id = in_result_id
         )
         SELECT
           rr.result_id,
           rr.sample_id,
           g.guideline_id,
           eg.rule_id,
           g.guideline_code,
           g.guideline_name,
           gp.publisher_name,
           gs.series_name,
           gj.jurisdiction_name AS jurisdiction,
           gpg.protection_goal_name AS protection_goal,
           ged.exposure_duration_name AS exposure_duration,
           gap.averaging_period_name AS averaging_period,
           rr.parameter_id,
           rr.parameter_name,
           rr.matrix_state_id,
           rr.matrix_state_code,
           rr.units,
           eg.bound_code,
           g.comparison_operator_code,
           op.operator_symbol AS comparison_symbol,
           rr.result_value,
           eg.guideline_value,
           eg.output_status,
           CASE
             WHEN eg.output_status <> 'value' THEN eg.output_status
             WHEN rr.result_value IS NULL THEN 'no_numeric_result'
             WHEN eg.bound_code = 'upper'
                  AND rr.result_value <= eg.guideline_value THEN 'meets'
             WHEN eg.bound_code = 'upper' THEN 'exceeds'
             WHEN eg.bound_code = 'lower'
                  AND rr.result_value >= eg.guideline_value THEN 'meets'
             WHEN eg.bound_code = 'lower' THEN 'below'
             ELSE 'not_evaluated'
           END AS comparison_status,
           eg.derivation_inputs,
           eg.message,
           g.source_document_title,
           g.source_url,
           g.source_page,
           g.source_table,
           g.source_section
         FROM result_row rr
         JOIN discrete.guidelines g
           ON g.parameter_id = rr.parameter_id
          AND g.matrix_state_id = rr.matrix_state_id
         JOIN discrete.guideline_comparison_operators op
           ON op.operator_code = g.comparison_operator_code
         LEFT JOIN discrete.guideline_publishers gp
           ON gp.publisher_id = g.publisher_id
         LEFT JOIN discrete.guideline_series gs
           ON gs.series_id = g.series_id
         LEFT JOIN discrete.guideline_jurisdictions gj
           ON gj.jurisdiction_id = g.jurisdiction_id
         LEFT JOIN discrete.guideline_protection_goals gpg
           ON gpg.protection_goal_id = g.protection_goal_id
         LEFT JOIN discrete.guideline_exposure_durations ged
           ON ged.exposure_duration_id = g.exposure_duration_id
         LEFT JOIN discrete.guideline_averaging_periods gap
           ON gap.averaging_period_id = g.averaging_period_id
         CROSS JOIN LATERAL discrete.evaluate_guideline(
           g.guideline_id,
           rr.sample_id
         ) eg
         WHERE in_as_of_date >= g.valid_from
           AND (g.valid_to IS NULL OR in_as_of_date <= g.valid_to)
           AND (
             g.result_speciation_id IS NULL
             OR g.result_speciation_id IS NOT DISTINCT FROM
               rr.result_speciation_id
           )
           AND (
             NOT EXISTS (
               SELECT 1
               FROM discrete.guidelines_media_types gm_any
               WHERE gm_any.guideline_id = g.guideline_id
             )
             OR EXISTS (
               SELECT 1
               FROM discrete.guidelines_media_types gm
               WHERE gm.guideline_id = g.guideline_id
                 AND gm.media_id = rr.media_id
             )
           )
           AND (
             NOT EXISTS (
               SELECT 1
               FROM discrete.guidelines_fractions gf_any
               WHERE gf_any.guideline_id = g.guideline_id
             )
             OR EXISTS (
               SELECT 1
               FROM discrete.guidelines_fractions gf
               WHERE gf.guideline_id = g.guideline_id
                 AND gf.fraction_id IS NOT DISTINCT FROM
                   rr.sample_fraction_id
             )
           )
           AND (
             in_include_unresolved
             OR eg.output_status = 'value'
           )
         ORDER BY g.guideline_id, eg.rule_id;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.applicable_guideline_rules_for_result(
         INTEGER, DATE, BOOLEAN
       ) OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS discrete.results_guideline_values;"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.applicable_guidelines_for_result(
         INTEGER, DATE, BOOLEAN
       );"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.applicable_guidelines_for_result(
         in_result_id INTEGER,
         in_as_of_date DATE DEFAULT CURRENT_DATE,
         in_include_unresolved BOOLEAN DEFAULT TRUE
       )
       RETURNS TABLE(
         result_id INTEGER,
         sample_id INTEGER,
         guideline_id INTEGER,
         guideline_code TEXT,
         guideline_name TEXT,
         publisher_name TEXT,
         series_name TEXT,
         jurisdiction TEXT,
         protection_goal TEXT,
         exposure_duration TEXT,
         averaging_period TEXT,
         parameter_id INTEGER,
         parameter_name TEXT,
         matrix_state_id INTEGER,
         matrix_state_code TEXT,
         units TEXT,
         comparison_operator_code TEXT,
         comparison_symbol TEXT,
         result_value NUMERIC,
         lower_guideline_value NUMERIC,
         upper_guideline_value NUMERIC,
         output_status TEXT,
         comparison_status TEXT,
         derivation_inputs JSONB,
         message TEXT,
         source_document_title TEXT,
         source_url TEXT,
         source_page TEXT,
         source_table TEXT,
         source_section TEXT
       )
       LANGUAGE sql
       STABLE
       AS $function$
         WITH rule_rows AS (
           SELECT *
           FROM discrete.applicable_guideline_rules_for_result(
             in_result_id,
             in_as_of_date,
             in_include_unresolved
           )
         ),
         grouped AS (
           SELECT
             result_id,
             sample_id,
             guideline_id,
             guideline_code,
             guideline_name,
             publisher_name,
             series_name,
             jurisdiction,
             protection_goal,
             exposure_duration,
             averaging_period,
             parameter_id,
             parameter_name,
             matrix_state_id,
             matrix_state_code,
             units,
             comparison_operator_code,
             comparison_symbol,
             result_value,
             max(guideline_value) FILTER (
             WHERE bound_code = 'lower' AND output_status = 'value'
           ) AS lower_guideline_value,
           max(guideline_value) FILTER (
              WHERE bound_code = 'upper'
                AND output_status = 'value'
            ) AS upper_guideline_value,
             bool_and(output_status = 'value') AS all_values_resolved,
             string_agg(DISTINCT output_status, ', ' ORDER BY output_status)
               FILTER (WHERE output_status <> 'value') AS unresolved_statuses,
             jsonb_agg(
               jsonb_build_object(
                 'rule_id', rule_id,
                 'bound_code', bound_code,
                 'inputs', derivation_inputs
               )
               ORDER BY rule_id
             ) AS derivation_inputs,
             string_agg(message, ' ' ORDER BY rule_id)
               FILTER (WHERE message IS NOT NULL) AS message,
             source_document_title,
             source_url,
             source_page,
             source_table,
             source_section
           FROM rule_rows
           GROUP BY
             result_id, sample_id, guideline_id, guideline_code,
             guideline_name, publisher_name, series_name, jurisdiction,
             protection_goal, exposure_duration, averaging_period,
             parameter_id, parameter_name, matrix_state_id,
             matrix_state_code, units, comparison_operator_code,
             comparison_symbol, result_value, source_document_title,
             source_url, source_page, source_table, source_section
         )
         SELECT
           result_id,
           sample_id,
           guideline_id,
           guideline_code,
           guideline_name,
           publisher_name,
           series_name,
           jurisdiction,
           protection_goal,
           exposure_duration,
           averaging_period,
           parameter_id,
           parameter_name,
           matrix_state_id,
           matrix_state_code,
           units,
           comparison_operator_code,
           comparison_symbol,
           result_value,
           lower_guideline_value,
           upper_guideline_value,
           CASE
             WHEN all_values_resolved THEN 'value'
             ELSE unresolved_statuses
           END AS output_status,
           CASE
             WHEN NOT all_values_resolved THEN unresolved_statuses
             WHEN result_value IS NULL THEN 'no_numeric_result'
             WHEN comparison_operator_code = 'lte'
                  AND result_value <= upper_guideline_value THEN 'meets'
             WHEN comparison_operator_code = 'lte' THEN 'exceeds'
             WHEN comparison_operator_code = 'gte'
                  AND result_value >= lower_guideline_value THEN 'meets'
             WHEN comparison_operator_code = 'gte' THEN 'below'
             WHEN comparison_operator_code = 'range'
                  AND result_value >= lower_guideline_value
                  AND result_value <= upper_guideline_value THEN 'meets'
             WHEN comparison_operator_code = 'range'
                  AND result_value < lower_guideline_value THEN 'below'
             WHEN comparison_operator_code = 'range'
                  AND result_value > upper_guideline_value THEN 'exceeds'
             WHEN comparison_operator_code = 'eq'
                  AND (
                    lower_guideline_value IS NULL
                    OR upper_guideline_value IS NULL
                    OR lower_guideline_value <> upper_guideline_value
                  ) THEN 'invalid_guideline_bounds'
             WHEN comparison_operator_code = 'eq'
                  AND result_value = upper_guideline_value THEN 'meets'
             WHEN comparison_operator_code = 'eq' THEN 'does_not_equal'
             ELSE 'not_evaluated'
           END AS comparison_status,
           derivation_inputs,
           message,
           source_document_title,
           source_url,
           source_page,
           source_table,
           source_section
         FROM grouped
         WHERE in_include_unresolved
            OR all_values_resolved;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.applicable_guidelines_for_result(
         INTEGER, DATE, BOOLEAN
       ) OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.results_guideline_rule_values
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT ag.*
       FROM discrete.results r
       CROSS JOIN LATERAL discrete.applicable_guideline_rules_for_result(
         r.result_id,
         CURRENT_DATE,
         TRUE
       ) ag;"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW discrete.results_guideline_rule_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_guideline_rule_values IS
       'Detailed database-driven view with one row per matched guideline rule or bound.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.results_guideline_values
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT ag.*
       FROM discrete.results r
       CROSS JOIN LATERAL discrete.applicable_guidelines_for_result(
         r.result_id,
         CURRENT_DATE,
         TRUE
       ) ag;"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW discrete.results_guideline_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_guideline_values IS
       'Database-driven one-row-per-guideline view that reports derived lower_guideline_value and upper_guideline_value bounds for each result. Per-rule scalar guideline_value rows are exposed by discrete.results_guideline_rule_values.';"
    )

    # Audit, modification metadata, and grants ##############################
    for (table_name in c(
      "guideline_comparison_operators",
      "guideline_value_algorithms",
      "guideline_models",
      "guideline_model_inputs",
      "guideline_model_outputs",
      "guideline_model_results",
      "guideline_value_rules",
      "guideline_rule_inputs",
      "guideline_rule_coefficients",
      "guideline_lookup_values",
      "guideline_lookup_tables",
      "guideline_lookup_dimensions",
      "guideline_lookup_cells",
      "guideline_lookup_cell_ranges"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_user_modified
           ON discrete.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified();",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_update_modified
           ON discrete.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified();",
          table_name,
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "DO $$
       DECLARE
         table_name TEXT;
       BEGIN
         IF to_regprocedure('audit.if_modified_func()') IS NULL THEN
           RETURN;
         END IF;

         FOREACH table_name IN ARRAY ARRAY[
           'guideline_comparison_operators',
           'guideline_value_algorithms',
           'guideline_jurisdictions',
           'guideline_jurisdiction_levels',
           'guideline_protection_goals',
           'guideline_exposure_durations',
           'guideline_averaging_periods',
           'guideline_models',
           'guideline_model_inputs',
           'guideline_model_outputs',
           'guideline_model_results',
           'guideline_value_rules',
           'guideline_rule_inputs',
           'guideline_rule_coefficients',
           'guideline_lookup_values',
           'guideline_lookup_tables',
           'guideline_lookup_dimensions',
           'guideline_lookup_cells',
           'guideline_lookup_cell_ranges'
         ]
         LOOP
           EXECUTE format(
             'DROP TRIGGER IF EXISTS audit_%I_trigger ON discrete.%I;',
             table_name,
             table_name
           );
           EXECUTE format(
             'CREATE TRIGGER audit_%I_trigger
              AFTER UPDATE OR DELETE ON discrete.%I
              FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func();',
             table_name,
             table_name
           );
         END LOOP;
       END $$;"
    )

    DBI::dbExecute(
      con,
      "DO $$
       DECLARE
         role_name TEXT;
       BEGIN
         FOREACH role_name IN ARRAY ARRAY['postgres', 'admin']
         LOOP
           IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = role_name) THEN
             EXECUTE format('GRANT USAGE ON SCHEMA discrete TO %I;', role_name);
             EXECUTE format(
               'GRANT ALL PRIVILEGES ON TABLE
                  discrete.guideline_comparison_operators,
                  discrete.guideline_value_algorithms,
                  discrete.guideline_jurisdictions,
                  discrete.guideline_jurisdiction_levels,
                  discrete.guideline_protection_goals,
                  discrete.guideline_exposure_durations,
                  discrete.guideline_averaging_periods,
                  discrete.guideline_publishers,
                  discrete.guideline_series,
                  discrete.guidelines,
                  discrete.guidelines_media_types,
                  discrete.guidelines_fractions,
                  discrete.guideline_models,
                  discrete.guideline_model_inputs,
                  discrete.guideline_model_outputs,
                  discrete.guideline_model_results,
                  discrete.guideline_value_rules,
                  discrete.guideline_rule_inputs,
                  discrete.guideline_rule_coefficients,
                  discrete.guideline_lookup_values,
                  discrete.guideline_lookup_tables,
                  discrete.guideline_lookup_dimensions,
                  discrete.guideline_lookup_cells,
                  discrete.guideline_lookup_cell_ranges,
                  discrete.results_guideline_rule_values,
                  discrete.results_guideline_values
                TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA discrete TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT EXECUTE ON FUNCTION
                  discrete.guideline_apply_rounding(NUMERIC, INTEGER, TEXT),
                  discrete.guideline_get_input_value(INTEGER, INTEGER),
                  discrete.guideline_collect_rule_inputs(INTEGER, INTEGER),
                  discrete.evaluate_guideline_rule(INTEGER, INTEGER),
                  discrete.evaluate_guideline(INTEGER, INTEGER),
                  discrete.get_guideline_value(INTEGER, INTEGER),
                  discrete.applicable_guideline_rules_for_result(INTEGER, DATE, BOOLEAN),
                  discrete.applicable_guidelines_for_result(INTEGER, DATE, BOOLEAN)
                TO %I;',
               role_name
             );
           END IF;
         END LOOP;

         FOREACH role_name IN ARRAY ARRAY[
           'public_reader',
           'yg_reader_group',
           'yg_reader',
           'discrete_editor'
         ]
         LOOP
           IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = role_name) THEN
             EXECUTE format('GRANT USAGE ON SCHEMA discrete TO %I;', role_name);
             EXECUTE format(
               'GRANT SELECT ON TABLE
                  discrete.guideline_comparison_operators,
                  discrete.guideline_value_algorithms,
                  discrete.guideline_jurisdictions,
                  discrete.guideline_jurisdiction_levels,
                  discrete.guideline_protection_goals,
                  discrete.guideline_exposure_durations,
                  discrete.guideline_averaging_periods,
                  discrete.guideline_publishers,
                  discrete.guideline_series,
                  discrete.guidelines,
                  discrete.guidelines_media_types,
                  discrete.guidelines_fractions,
                  discrete.guideline_models,
                  discrete.guideline_model_inputs,
                  discrete.guideline_model_outputs,
                  discrete.guideline_model_results,
                  discrete.guideline_value_rules,
                  discrete.guideline_rule_inputs,
                  discrete.guideline_rule_coefficients,
                  discrete.guideline_lookup_values,
                  discrete.guideline_lookup_tables,
                  discrete.guideline_lookup_dimensions,
                  discrete.guideline_lookup_cells,
                  discrete.guideline_lookup_cell_ranges,
                  discrete.results_guideline_rule_values,
                  discrete.results_guideline_values
                TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT EXECUTE ON FUNCTION
                  discrete.guideline_apply_rounding(NUMERIC, INTEGER, TEXT),
                  discrete.guideline_get_input_value(INTEGER, INTEGER),
                  discrete.guideline_collect_rule_inputs(INTEGER, INTEGER),
                  discrete.evaluate_guideline_rule(INTEGER, INTEGER),
                  discrete.evaluate_guideline(INTEGER, INTEGER),
                  discrete.get_guideline_value(INTEGER, INTEGER),
                  discrete.applicable_guideline_rules_for_result(INTEGER, DATE, BOOLEAN),
                  discrete.applicable_guidelines_for_result(INTEGER, DATE, BOOLEAN)
                TO %I;',
               role_name
             );
           END IF;
         END LOOP;
       END $$;"
    )

    # If we got here, everything worked and we can commit the transaction.
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '47'
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
    message(
      "Patch 47 applied successfully. Database-driven guideline derivation is installed."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    stop(
      "Patch 47 failed and the database has been rolled back to its previous state. ",
      e$message
    )
  }
)
