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
        "to_regclass('discrete.results') IS NOT NULL AS has_results,",
        "to_regclass('discrete.samples') IS NOT NULL AS has_samples,",
        "to_regclass('discrete.sample_fractions') IS NOT NULL AS has_sample_fractions,",
        "to_regclass('discrete.result_speciations') IS NOT NULL AS has_result_speciations,",
        "to_regclass('discrete.result_types') IS NOT NULL AS has_result_types,",
        "to_regclass('public.parameters') IS NOT NULL AS has_parameters,",
        "to_regclass('public.matrix_states') IS NOT NULL AS has_matrix_states,",
        "to_regclass('public.locations') IS NOT NULL AS has_locations,",
        "to_regclass('public.units') IS NOT NULL AS has_units,",
        "to_regclass('public.media_types') IS NOT NULL AS has_media_types"
      )
    )

    if (
      !isTRUE(check$has_results[[1]]) ||
        !isTRUE(check$has_samples[[1]]) ||
        !isTRUE(check$has_sample_fractions[[1]]) ||
        !isTRUE(check$has_result_speciations[[1]]) ||
        !isTRUE(check$has_result_types[[1]]) ||
        !isTRUE(check$has_parameters[[1]]) ||
        !isTRUE(check$has_matrix_states[[1]]) ||
        !isTRUE(check$has_locations[[1]]) ||
        !isTRUE(check$has_units[[1]]) ||
        !isTRUE(check$has_media_types[[1]])
    ) {
      stop(
        paste(
          "Patch 47 requires discrete.results, discrete.samples,",
          "discrete.sample_fractions, discrete.result_speciations,",
          "discrete.result_types, public.parameters, public.matrix_states,",
          "public.locations, public.units, and public.media_types to already exist."
        )
      )
    }

    # Create a new schema called 'criteria' to hold the new guideline model tables. This keeps them separate from the existing tables and makes it easier to also manage guidelines/rules for continuous data.
    DBI::dbExecute(
      con,
      "CREATE SCHEMA IF NOT EXISTS criteria;"
    )
    DBI::dbExecute(
      con,
      "ALTER SCHEMA criteria OWNER TO admin;"
    )
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA criteria TO public;")

    for (sql in c(
      "DROP VIEW IF EXISTS discrete.results_guideline_values CASCADE;",
      "DROP VIEW IF EXISTS discrete.results_guideline_rule_values CASCADE;",
      "DROP VIEW IF EXISTS criteria.results_guideline_values CASCADE;",
      "DROP VIEW IF EXISTS criteria.results_guideline_rule_values CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.applicable_guidelines_for_result(INTEGER, DATE, BOOLEAN) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.applicable_guideline_rules_for_result(INTEGER, DATE, BOOLEAN) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.get_guideline_value(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.evaluate_guideline(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.evaluate_guideline_rule(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.guideline_collect_rule_inputs(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.guideline_get_input_value(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.guideline_apply_rounding(NUMERIC, INTEGER, TEXT) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.get_sample_hardness(INTEGER, INTEGER[]) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.get_sample_hardness(INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.validate_guideline_lookup_cell_range() CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.validate_guideline_lookup_value() CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.validate_guideline_value_rule() CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.enforce_guideline_matrix_state() CASCADE;",
      "DROP FUNCTION IF EXISTS discrete.guidelines_validate_trg() CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.applicable_guidelines_for_result(INTEGER, DATE, BOOLEAN) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.applicable_guideline_rules_for_result(INTEGER, DATE, BOOLEAN) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.get_guideline_value(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.evaluate_guideline(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.evaluate_guideline_rule(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.guideline_collect_rule_inputs(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.guideline_get_input_value(INTEGER, INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.guideline_apply_rounding(NUMERIC, INTEGER, TEXT) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.get_sample_hardness(INTEGER, INTEGER[]) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.get_sample_hardness(INTEGER) CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.validate_guideline_lookup_cell_range() CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.validate_guideline_lookup_value() CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.validate_guideline_value_rule() CASCADE;",
      "DROP FUNCTION IF EXISTS criteria.guidelines_validate_trg() CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_locations CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_narrative_values CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_lookup_cell_ranges CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_lookup_cells CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_lookup_dimensions CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_lookup_tables CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_lookup_values CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_rule_coefficients CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_rule_inputs CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_value_rules CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_model_results CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_model_outputs CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_model_inputs CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_models CASCADE;",
      "DROP TABLE IF EXISTS criteria.guidelines_fractions CASCADE;",
      "DROP TABLE IF EXISTS criteria.guidelines_media_types CASCADE;",
      "DROP TABLE IF EXISTS criteria.guidelines CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_series CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_publishers CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_averaging_periods CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_exposure_durations CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_protection_goals CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_jurisdiction_levels CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_jurisdictions CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_value_algorithms CASCADE;",
      "DROP TABLE IF EXISTS criteria.guideline_comparison_operators CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_locations CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_narrative_values CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_lookup_cell_ranges CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_lookup_cells CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_lookup_dimensions CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_lookup_tables CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_lookup_values CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_rule_coefficients CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_rule_inputs CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_value_rules CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_model_results CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_model_outputs CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_model_inputs CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_models CASCADE;",
      "DROP TABLE IF EXISTS discrete.guidelines_fractions CASCADE;",
      "DROP TABLE IF EXISTS discrete.guidelines_media_types CASCADE;",
      "DROP TABLE IF EXISTS discrete.guidelines CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_series CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_publishers CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_averaging_periods CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_exposure_durations CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_protection_goals CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_jurisdiction_levels CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_jurisdictions CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_value_algorithms CASCADE;",
      "DROP TABLE IF EXISTS discrete.guideline_comparison_operators CASCADE;"
    )) {
      DBI::dbExecute(con, sql)
    }

    # Bootstrap the core catalogue tables. Later sections add controlled
    # vocabulary columns, constraints, comments, and derivation rules.
    DBI::dbExecute(
      con,
      "CREATE TABLE criteria.guideline_publishers (
         publisher_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         publisher_name TEXT NOT NULL
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE criteria.guideline_series (
         series_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         series_name TEXT NOT NULL,
         publisher_id INTEGER
           REFERENCES criteria.guideline_publishers(publisher_id)
           ON UPDATE CASCADE ON DELETE SET NULL
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE criteria.guidelines (
         guideline_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         guideline_name TEXT NOT NULL,
         publisher_id INTEGER
           REFERENCES criteria.guideline_publishers(publisher_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         series_id INTEGER
           REFERENCES criteria.guideline_series(series_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         reference TEXT,
         general_notes TEXT,
         applicability_notes TEXT,
         parameter_id INTEGER NOT NULL
           REFERENCES public.parameters(parameter_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         matrix_state_id INTEGER NOT NULL
           REFERENCES public.matrix_states(matrix_state_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         result_speciation_id INTEGER
           REFERENCES discrete.result_speciations(result_speciation_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE criteria.guidelines_media_types (
         guideline_id INTEGER NOT NULL
           REFERENCES criteria.guidelines(guideline_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         media_id INTEGER NOT NULL
           REFERENCES public.media_types(media_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         PRIMARY KEY (guideline_id, media_id)
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE criteria.guidelines_fractions (
         guideline_id INTEGER NOT NULL
           REFERENCES criteria.guidelines(guideline_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         fraction_id INTEGER NOT NULL
           REFERENCES discrete.sample_fractions(sample_fraction_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         PRIMARY KEY (guideline_id, fraction_id)
       );"
    )
    for (sql in c(
      "ALTER TABLE criteria.guideline_publishers OWNER TO admin;",
      "ALTER TABLE criteria.guideline_series OWNER TO admin;",
      "ALTER TABLE criteria.guidelines OWNER TO admin;",
      "ALTER TABLE criteria.guidelines_media_types OWNER TO admin;",
      "ALTER TABLE criteria.guidelines_fractions OWNER TO admin;"
    )) {
      DBI::dbExecute(con, sql)
    }

    # Controlled vocabulary #################################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_comparison_operators (
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
      "ALTER TABLE criteria.guideline_comparison_operators OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_comparison_operators IS
       'Controlled vocabulary for how measured values are compared with guideline values.';"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_comparison_operators (
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
      "CREATE TABLE IF NOT EXISTS criteria.guideline_value_algorithms (
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
      "ALTER TABLE criteria.guideline_value_algorithms OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_value_algorithms IS
       'Controlled vocabulary for database-side algorithms used to derive guideline values.';"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_value_algorithms (
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
    DBI::dbExecute(
      con,
      "DELETE FROM criteria.guideline_value_algorithms
       WHERE algorithm_code = 'db_function';"
    )

    # Applicability reference tables #######################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_jurisdictions (
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
      "CREATE TABLE IF NOT EXISTS criteria.guideline_jurisdiction_levels (
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
      "CREATE TABLE IF NOT EXISTS criteria.guideline_protection_goals (
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
      "CREATE TABLE IF NOT EXISTS criteria.guideline_exposure_durations (
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
      "CREATE TABLE IF NOT EXISTS criteria.guideline_averaging_periods (
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
    for (sql in c(
      "ALTER TABLE criteria.guideline_jurisdictions OWNER TO admin;",
      "ALTER TABLE criteria.guideline_jurisdiction_levels OWNER TO admin;",
      "ALTER TABLE criteria.guideline_protection_goals OWNER TO admin;",
      "ALTER TABLE criteria.guideline_exposure_durations OWNER TO admin;",
      "ALTER TABLE criteria.guideline_averaging_periods OWNER TO admin;"
    )) {
      DBI::dbExecute(con, sql)
    }
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_jurisdictions (
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
      "INSERT INTO criteria.guideline_jurisdiction_levels (
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
      "INSERT INTO criteria.guideline_protection_goals (
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
      "INSERT INTO criteria.guideline_exposure_durations (
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
      "INSERT INTO criteria.guideline_averaging_periods (
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
    for (sql in c(
      "COMMENT ON TABLE criteria.guideline_jurisdictions IS
       'Controlled jurisdictions used by water-quality guidelines.';",
      "COMMENT ON TABLE criteria.guideline_jurisdiction_levels IS
       'Controlled jurisdiction levels used by water-quality guidelines.';",
      "COMMENT ON TABLE criteria.guideline_protection_goals IS
       'Controlled protection goals used by water-quality guidelines.';",
      "COMMENT ON TABLE criteria.guideline_exposure_durations IS
       'Controlled exposure durations used by water-quality guidelines and model outputs.';",
      "COMMENT ON TABLE criteria.guideline_averaging_periods IS
       'Controlled averaging periods used by water-quality guidelines and model outputs.';"
    )) {
      DBI::dbExecute(con, sql)
    }

    # Publisher and source-series metadata ##################################
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_publishers
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
      "ALTER TABLE criteria.guideline_publishers
       ALTER COLUMN publisher_name SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_publishers_code_key
       ON criteria.guideline_publishers (publisher_code)
       WHERE publisher_code IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_publishers IS
       'Organizations that publish or maintain environmental quality guidelines.';"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_series
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
       ON criteria.guideline_series (series_code)
       WHERE series_code IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_series IS
       'Publication series, editions, or guideline collections that contain individual guideline values.';"
    )

    # External model metadata ###############################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_models (
         model_code TEXT PRIMARY KEY,
         model_name TEXT NOT NULL,
         publisher_id INTEGER
           REFERENCES criteria.guideline_publishers(publisher_id)
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
               'lookup_workbook',
               'published_table'
             )
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_models OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DELETE FROM criteria.guideline_models
       WHERE model_type = 'database_function';"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_models
       DROP CONSTRAINT IF EXISTS guideline_models_type_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_models
       ADD CONSTRAINT guideline_models_type_check
       CHECK (
         model_type IN (
           'external_software',
           'lookup_workbook',
           'published_table'
         )
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_models IS
       'External guideline calculation models such as BLM software, lookup workbooks, or published tables.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_model_inputs (
         model_code TEXT NOT NULL
           REFERENCES criteria.guideline_models(model_code)
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
      "ALTER TABLE criteria.guideline_model_inputs OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_model_inputs_parameter_idx
       ON criteria.guideline_model_inputs (
         parameter_id, matrix_state_id, sample_fraction_id,
         result_speciation_id
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_model_inputs IS
       'Declared input chemistry required by guideline models, including BLM input bounds and default behaviour.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_model_outputs (
         model_code TEXT NOT NULL
           REFERENCES criteria.guideline_models(model_code)
           ON UPDATE CASCADE ON DELETE CASCADE,
         output_code TEXT NOT NULL,
         output_name TEXT NOT NULL,
         comparison_operator_code TEXT NOT NULL
           REFERENCES criteria.guideline_comparison_operators(operator_code)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         output_units INTEGER
           REFERENCES public.units(unit_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         exposure_duration_id INTEGER
           REFERENCES criteria.guideline_exposure_durations(exposure_duration_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         averaging_period_id INTEGER
           REFERENCES criteria.guideline_averaging_periods(averaging_period_id)
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
      "ALTER TABLE criteria.guideline_model_outputs OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_model_outputs
       ADD COLUMN IF NOT EXISTS exposure_duration_id INTEGER
         REFERENCES criteria.guideline_exposure_durations(exposure_duration_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS averaging_period_id INTEGER
         REFERENCES criteria.guideline_averaging_periods(averaging_period_id)
         ON UPDATE CASCADE ON DELETE RESTRICT;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'criteria'
             AND table_name = 'guideline_model_outputs'
             AND column_name = 'exposure_duration'
         ) THEN
           INSERT INTO criteria.guideline_exposure_durations (
             exposure_duration_code, exposure_duration_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(exposure_duration), 10)),
             exposure_duration,
             800
           FROM criteria.guideline_model_outputs
           WHERE exposure_duration IS NOT NULL
             AND btrim(exposure_duration) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_exposure_durations existing
               WHERE lower(existing.exposure_duration_name) =
                 lower(criteria.guideline_model_outputs.exposure_duration)
             )
           ON CONFLICT (exposure_duration_name) DO NOTHING;

           UPDATE criteria.guideline_model_outputs gmo
           SET exposure_duration_id = ged.exposure_duration_id
           FROM criteria.guideline_exposure_durations ged
           WHERE gmo.exposure_duration_id IS NULL
             AND lower(gmo.exposure_duration) = lower(ged.exposure_duration_name);

           ALTER TABLE criteria.guideline_model_outputs
             DROP COLUMN exposure_duration;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'criteria'
             AND table_name = 'guideline_model_outputs'
             AND column_name = 'averaging_period'
         ) THEN
           INSERT INTO criteria.guideline_averaging_periods (
             averaging_period_code, averaging_period_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(averaging_period), 10)),
             averaging_period,
             800
           FROM criteria.guideline_model_outputs
           WHERE averaging_period IS NOT NULL
             AND btrim(averaging_period) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_averaging_periods existing
               WHERE lower(existing.averaging_period_name) =
                 lower(criteria.guideline_model_outputs.averaging_period)
             )
           ON CONFLICT (averaging_period_name) DO NOTHING;

           UPDATE criteria.guideline_model_outputs gmo
           SET averaging_period_id = gap.averaging_period_id
           FROM criteria.guideline_averaging_periods gap
           WHERE gmo.averaging_period_id IS NULL
             AND lower(gmo.averaging_period) = lower(gap.averaging_period_name);

           ALTER TABLE criteria.guideline_model_outputs
             DROP COLUMN averaging_period;
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_model_outputs IS
       'Named outputs produced by a guideline model, such as acute or chronic BLM copper guideline values.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_model_results (
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
           REFERENCES criteria.guideline_model_outputs(model_code, output_code)
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
      "ALTER TABLE criteria.guideline_model_results OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS criteria.guideline_model_results_signature_key;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX guideline_model_results_signature_key
       ON criteria.guideline_model_results (
         model_code,
         model_output_code,
         sample_id,
         input_hash,
         COALESCE(model_version, '')
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_model_results IS
       'Database cache of externally or database-calculated guideline model outputs keyed to the exact input signature used.';"
    )

    # Extend the existing guideline table ###################################
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_check_sql ON criteria.guidelines;"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS criteria.guidelines_validate_trg() CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guidelines
       DROP CONSTRAINT IF EXISTS guidelines_guideline_name_publisher_key;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guidelines
       DROP COLUMN IF EXISTS guideline_sql;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guidelines
       ADD COLUMN IF NOT EXISTS guideline_code TEXT,
       ADD COLUMN IF NOT EXISTS parent_guideline_code TEXT,
       ADD COLUMN IF NOT EXISTS version_label TEXT,
       ADD COLUMN IF NOT EXISTS jurisdiction_id INTEGER
         REFERENCES criteria.guideline_jurisdictions(jurisdiction_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS jurisdiction_level_id INTEGER
         REFERENCES criteria.guideline_jurisdiction_levels(jurisdiction_level_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS protection_goal_id INTEGER
         REFERENCES criteria.guideline_protection_goals(protection_goal_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS exposure_duration_id INTEGER
         REFERENCES criteria.guideline_exposure_durations(exposure_duration_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS averaging_period_id INTEGER
         REFERENCES criteria.guideline_averaging_periods(averaging_period_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN IF NOT EXISTS comparison_operator_code TEXT
         REFERENCES criteria.guideline_comparison_operators(operator_code)
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
         REFERENCES criteria.guidelines(guideline_id)
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
           WHERE table_schema = 'criteria'
             AND table_name = 'guidelines'
             AND column_name = 'jurisdiction'
         ) THEN
           INSERT INTO criteria.guideline_jurisdictions (
             jurisdiction_code, jurisdiction_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(jurisdiction), 10)),
             jurisdiction,
             800
           FROM criteria.guidelines
           WHERE jurisdiction IS NOT NULL
             AND btrim(jurisdiction) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_jurisdictions existing
               WHERE lower(existing.jurisdiction_name) =
                 lower(criteria.guidelines.jurisdiction)
             )
           ON CONFLICT (jurisdiction_name) DO NOTHING;

           UPDATE criteria.guidelines g
           SET jurisdiction_id = gj.jurisdiction_id
           FROM criteria.guideline_jurisdictions gj
           WHERE g.jurisdiction_id IS NULL
             AND lower(g.jurisdiction) = lower(gj.jurisdiction_name);

           ALTER TABLE criteria.guidelines DROP COLUMN jurisdiction;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'criteria'
             AND table_name = 'guidelines'
             AND column_name = 'jurisdiction_level'
         ) THEN
           INSERT INTO criteria.guideline_jurisdiction_levels (
             jurisdiction_level_code, jurisdiction_level_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(jurisdiction_level), 10)),
             jurisdiction_level,
             800
           FROM criteria.guidelines
           WHERE jurisdiction_level IS NOT NULL
             AND btrim(jurisdiction_level) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_jurisdiction_levels existing
               WHERE lower(existing.jurisdiction_level_name) =
                 lower(criteria.guidelines.jurisdiction_level)
             )
           ON CONFLICT (jurisdiction_level_name) DO NOTHING;

           UPDATE criteria.guidelines g
           SET jurisdiction_level_id = gjl.jurisdiction_level_id
           FROM criteria.guideline_jurisdiction_levels gjl
           WHERE g.jurisdiction_level_id IS NULL
             AND lower(g.jurisdiction_level) = lower(gjl.jurisdiction_level_name);

           ALTER TABLE criteria.guidelines DROP COLUMN jurisdiction_level;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'criteria'
             AND table_name = 'guidelines'
             AND column_name = 'protection_goal'
         ) THEN
           INSERT INTO criteria.guideline_protection_goals (
             protection_goal_code, protection_goal_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(protection_goal), 10)),
             protection_goal,
             800
           FROM criteria.guidelines
           WHERE protection_goal IS NOT NULL
             AND btrim(protection_goal) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_protection_goals existing
               WHERE lower(existing.protection_goal_name) =
                 lower(criteria.guidelines.protection_goal)
             )
           ON CONFLICT (protection_goal_name) DO NOTHING;

           UPDATE criteria.guidelines g
           SET protection_goal_id = gpg.protection_goal_id
           FROM criteria.guideline_protection_goals gpg
           WHERE g.protection_goal_id IS NULL
             AND lower(g.protection_goal) = lower(gpg.protection_goal_name);

           ALTER TABLE criteria.guidelines DROP COLUMN protection_goal;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'criteria'
             AND table_name = 'guidelines'
             AND column_name = 'exposure_duration'
         ) THEN
           INSERT INTO criteria.guideline_exposure_durations (
             exposure_duration_code, exposure_duration_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(exposure_duration), 10)),
             exposure_duration,
             800
           FROM criteria.guidelines
           WHERE exposure_duration IS NOT NULL
             AND btrim(exposure_duration) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_exposure_durations existing
               WHERE lower(existing.exposure_duration_name) =
                 lower(criteria.guidelines.exposure_duration)
             )
           ON CONFLICT (exposure_duration_name) DO NOTHING;

           UPDATE criteria.guidelines g
           SET exposure_duration_id = ged.exposure_duration_id
           FROM criteria.guideline_exposure_durations ged
           WHERE g.exposure_duration_id IS NULL
             AND lower(g.exposure_duration) = lower(ged.exposure_duration_name);

           ALTER TABLE criteria.guidelines DROP COLUMN exposure_duration;
         END IF;

         IF EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'criteria'
             AND table_name = 'guidelines'
             AND column_name = 'averaging_period'
         ) THEN
           INSERT INTO criteria.guideline_averaging_periods (
             averaging_period_code, averaging_period_name, sort_order
           )
           SELECT DISTINCT
             'CUSTOM_' || upper(left(md5(averaging_period), 10)),
             averaging_period,
             800
           FROM criteria.guidelines
           WHERE averaging_period IS NOT NULL
             AND btrim(averaging_period) <> ''
             AND NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_averaging_periods existing
               WHERE lower(existing.averaging_period_name) =
                 lower(criteria.guidelines.averaging_period)
             )
           ON CONFLICT (averaging_period_name) DO NOTHING;

           UPDATE criteria.guidelines g
           SET averaging_period_id = gap.averaging_period_id
           FROM criteria.guideline_averaging_periods gap
           WHERE g.averaging_period_id IS NULL
             AND lower(g.averaging_period) = lower(gap.averaging_period_name);

           ALTER TABLE criteria.guidelines DROP COLUMN averaging_period;
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "DO $do$
       BEGIN
       WITH ranked AS (
         SELECT
           jurisdiction_id,
           first_value(jurisdiction_id) OVER (
             PARTITION BY lower(btrim(jurisdiction_name))
             ORDER BY sort_order, jurisdiction_id
           ) AS keep_id
         FROM criteria.guideline_jurisdictions
       )
       UPDATE criteria.guidelines g
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
         FROM criteria.guideline_jurisdictions
       )
       DELETE FROM criteria.guideline_jurisdictions gj
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
         FROM criteria.guideline_jurisdiction_levels
       )
       UPDATE criteria.guidelines g
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
         FROM criteria.guideline_jurisdiction_levels
       )
       DELETE FROM criteria.guideline_jurisdiction_levels gjl
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
         FROM criteria.guideline_protection_goals
       )
       UPDATE criteria.guidelines g
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
         FROM criteria.guideline_protection_goals
       )
       DELETE FROM criteria.guideline_protection_goals gpg
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
         FROM criteria.guideline_exposure_durations
       )
       UPDATE criteria.guidelines g
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
         FROM criteria.guideline_exposure_durations
       )
       UPDATE criteria.guideline_model_outputs gmo
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
         FROM criteria.guideline_exposure_durations
       )
       DELETE FROM criteria.guideline_exposure_durations ged
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
         FROM criteria.guideline_averaging_periods
       )
       UPDATE criteria.guidelines g
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
         FROM criteria.guideline_averaging_periods
       )
       UPDATE criteria.guideline_model_outputs gmo
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
         FROM criteria.guideline_averaging_periods
       )
       DELETE FROM criteria.guideline_averaging_periods gap
       USING ranked
       WHERE gap.averaging_period_id = ranked.averaging_period_id
         AND ranked.averaging_period_id <> ranked.keep_id;
       END
       $do$;"
    )
    for (sql in c(
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_jurisdictions_name_lwr_key
       ON criteria.guideline_jurisdictions (lower(btrim(jurisdiction_name)));",
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_jurisdiction_levels_name_lwr_key
       ON criteria.guideline_jurisdiction_levels (lower(btrim(jurisdiction_level_name)));",
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_protection_goals_name_lwr_key
       ON criteria.guideline_protection_goals (lower(btrim(protection_goal_name)));",
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_exposure_durations_name_lwr_key
       ON criteria.guideline_exposure_durations (lower(btrim(exposure_duration_name)));",
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_averaging_periods_name_lwr_key
       ON criteria.guideline_averaging_periods (lower(btrim(averaging_period_name)));"
    )) {
      DBI::dbExecute(con, sql)
    }
    DBI::dbExecute(
      con,
      "UPDATE criteria.guidelines
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
      "ALTER TABLE criteria.guidelines
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
             AND conrelid = 'criteria.guidelines'::regclass
         ) THEN
           ALTER TABLE criteria.guidelines
             ADD CONSTRAINT guidelines_valid_date_range
             CHECK (valid_to IS NULL OR valid_to >= valid_from);
         END IF;

         IF NOT EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conname = 'guidelines_review_status_check'
             AND conrelid = 'criteria.guidelines'::regclass
         ) THEN
           ALTER TABLE criteria.guidelines
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
             AND conrelid = 'criteria.guidelines'::regclass
         ) THEN
           ALTER TABLE criteria.guidelines
             ADD CONSTRAINT guidelines_code_key UNIQUE (guideline_code);
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_parameter_state_speciation_idx
       ON criteria.guidelines (
         parameter_id, matrix_state_id, result_speciation_id
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_validity_idx
       ON criteria.guidelines (valid_from, valid_to);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_comparison_operator_idx
       ON criteria.guidelines (comparison_operator_code);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_applicability_idx
       ON criteria.guidelines (
         jurisdiction_id, jurisdiction_level_id, protection_goal_id,
         exposure_duration_id, averaging_period_id
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guidelines IS
       'One versioned published guideline or criterion that can be matched to analytical results and evaluated entirely in the database.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guidelines.guideline_code IS
       'Stable unique code for this specific guideline version, exposure duration, and protection goal.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guidelines.comparison_operator_code IS
       'How measured values are compared with the derived guideline value or values.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guidelines.valid_from IS
       'First date on which this guideline version is applicable for interpretation.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guidelines.valid_to IS
       'Last date on which this guideline version is applicable. NULL means still current.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_locations (
         guideline_location_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY,
         guideline_id INTEGER NOT NULL
           REFERENCES criteria.guidelines(guideline_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         location_id INTEGER NOT NULL
           REFERENCES public.locations(location_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         UNIQUE (guideline_id, location_id)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_locations OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_locations_location_idx
       ON criteria.guideline_locations (location_id, guideline_id)
       WHERE active;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_locations IS
       'Optional many-to-many location applicability for criteria/guidelines. No active rows for a guideline means the guideline is not location-limited; one or more active rows means it applies only at those locations.';"
    )

    # Declarative value rules ###############################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_value_rules (
         rule_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         guideline_id INTEGER NOT NULL
           REFERENCES criteria.guidelines(guideline_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         model_code TEXT,
         model_output_code TEXT,
         bound_code TEXT DEFAULT 'upper',
         algorithm_code TEXT NOT NULL
           REFERENCES criteria.guideline_value_algorithms(algorithm_code)
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
      "ALTER TABLE criteria.guideline_value_rules OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_value_rules
       ADD COLUMN IF NOT EXISTS model_code TEXT,
       ADD COLUMN IF NOT EXISTS model_output_code TEXT,
       DROP COLUMN IF EXISTS function_schema,
       DROP COLUMN IF EXISTS function_name;"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF to_regclass('criteria.guideline_value_rules') IS NULL THEN
           RETURN;
         END IF;

         IF EXISTS (
           SELECT 1
           FROM criteria.guideline_value_rules
           WHERE bound_code = 'single'
             AND algorithm_code <> 'narrative'
         ) THEN
           RAISE EXCEPTION
             'bound_code = single is no longer supported for numeric guideline rules; migrate those rules to lower or upper before applying patch 47.';
         END IF;

         ALTER TABLE criteria.guideline_value_rules
           DROP CONSTRAINT IF EXISTS guideline_value_rules_bound_code_check;

         ALTER TABLE criteria.guideline_value_rules
           ALTER COLUMN bound_code DROP NOT NULL,
           ALTER COLUMN bound_code SET DEFAULT 'upper';

         UPDATE criteria.guideline_value_rules
         SET bound_code = NULL
         WHERE algorithm_code = 'narrative'
           AND bound_code = 'single';

         ALTER TABLE criteria.guideline_value_rules
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
             AND conrelid = 'criteria.guideline_value_rules'::regclass
         ) THEN
           ALTER TABLE criteria.guideline_value_rules
             ADD CONSTRAINT guideline_value_rules_model_output_fkey
             FOREIGN KEY (model_code, model_output_code)
             REFERENCES criteria.guideline_model_outputs(model_code, output_code)
             ON UPDATE CASCADE
             ON DELETE RESTRICT;
         END IF;
       END $$;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_value_rules_guideline_idx
       ON criteria.guideline_value_rules (
         guideline_id, active, rule_priority, bound_code
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_value_rules_model_idx
       ON criteria.guideline_value_rules (
         model_code, model_output_code
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_value_rules IS
       'Declarative rules that derive lower and/or upper numeric guideline bounds for a guideline. Narrative rules have no bound code.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guideline_value_rules.formula_sql IS
       'Governed SQL scalar expression used only when a guideline cannot yet be represented by a declarative algorithm.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_rule_inputs (
         input_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         rule_id INTEGER NOT NULL
           REFERENCES criteria.guideline_value_rules(rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         input_code TEXT NOT NULL,
         input_name TEXT,
         input_source TEXT NOT NULL DEFAULT 'sample_result',
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
         result_type_preference INTEGER[],
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
         CONSTRAINT guideline_rule_inputs_source_check
           CHECK (input_source IN ('sample_result', 'hardness_helper')),
         CONSTRAINT guideline_rule_inputs_scope_check
           CHECK (search_scope IN ('same_sample')),
         CONSTRAINT guideline_rule_inputs_aggregate_check
           CHECK (aggregate_method IN ('single', 'avg', 'min', 'max'))
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_rule_inputs
       ADD COLUMN IF NOT EXISTS input_source TEXT NOT NULL DEFAULT 'sample_result',
       ADD COLUMN IF NOT EXISTS result_type_preference INTEGER[];"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_rule_inputs
       DROP CONSTRAINT IF EXISTS guideline_rule_inputs_source_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_rule_inputs
       ADD CONSTRAINT guideline_rule_inputs_source_check
       CHECK (input_source IN ('sample_result', 'hardness_helper'));"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_rule_inputs OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_rule_inputs_rule_idx
       ON criteria.guideline_rule_inputs (rule_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_rule_inputs_parameter_idx
       ON criteria.guideline_rule_inputs (
         parameter_id, matrix_state_id, sample_fraction_id,
         result_speciation_id, result_type
       );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_rule_inputs IS
       'Sample-specific analytical results required to derive a guideline value, such as hardness, pH, or temperature.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guideline_rule_inputs.input_source IS
       'How the input value is resolved. sample_result reads matching sample result rows; hardness_helper uses criteria.get_sample_hardness(sample_id).';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN criteria.guideline_rule_inputs.result_type_preference IS
       'Optional ordered list of result_type_id values. The first result type with usable results is preferred before aggregate_method is applied.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_rule_coefficients (
         rule_id INTEGER NOT NULL
           REFERENCES criteria.guideline_value_rules(rule_id)
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
      "ALTER TABLE criteria.guideline_rule_coefficients OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_rule_coefficients IS
       'Named numeric coefficients used by declarative guideline formula algorithms.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_lookup_values (
         lookup_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         rule_id INTEGER NOT NULL
           REFERENCES criteria.guideline_value_rules(rule_id)
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
           REFERENCES criteria.guideline_rule_inputs(rule_id, input_code)
           ON UPDATE CASCADE ON DELETE CASCADE
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_lookup_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_values_rule_input_idx
       ON criteria.guideline_lookup_values (rule_id, input_code, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_lookup_values IS
       'Non-overlapping numeric input ranges and corresponding output values for lookup-based guideline derivation.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_lookup_tables (
         lookup_table_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         rule_id INTEGER NOT NULL UNIQUE
           REFERENCES criteria.guideline_value_rules(rule_id)
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
      "ALTER TABLE criteria.guideline_lookup_tables OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_lookup_tables_rule_code_key
       ON criteria.guideline_lookup_tables (rule_id, table_code);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_lookup_tables IS
       'Header rows for multidimensional lookup tables used by guideline lookup_grid rules.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_lookup_dimensions (
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
           REFERENCES criteria.guideline_lookup_tables(lookup_table_id, rule_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         CONSTRAINT guideline_lookup_dimensions_input_fkey
           FOREIGN KEY (rule_id, input_code)
           REFERENCES criteria.guideline_rule_inputs(rule_id, input_code)
           ON UPDATE CASCADE ON DELETE CASCADE
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_lookup_dimensions OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_dimensions_table_idx
       ON criteria.guideline_lookup_dimensions (lookup_table_id, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_lookup_dimensions IS
       'Input dimensions for multidimensional guideline lookup tables.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_lookup_cells (
         cell_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         lookup_table_id INTEGER NOT NULL
           REFERENCES criteria.guideline_lookup_tables(lookup_table_id)
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
      "ALTER TABLE criteria.guideline_lookup_cells OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_cells_table_idx
       ON criteria.guideline_lookup_cells (lookup_table_id, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_lookup_cells IS
       'Output cells for multidimensional guideline lookup tables.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_lookup_cell_ranges (
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
           REFERENCES criteria.guideline_lookup_cells(cell_id, lookup_table_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         CONSTRAINT guideline_lookup_cell_ranges_dimension_fkey
           FOREIGN KEY (dimension_id, lookup_table_id)
           REFERENCES criteria.guideline_lookup_dimensions(dimension_id, lookup_table_id)
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
      "ALTER TABLE criteria.guideline_lookup_cell_ranges OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_lookup_cell_ranges_table_idx
       ON criteria.guideline_lookup_cell_ranges (lookup_table_id, dimension_id);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_lookup_cell_ranges IS
       'Per-dimension numeric ranges that define each multidimensional lookup output cell.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS criteria.guideline_narrative_values (
         narrative_value_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         guideline_id INTEGER NOT NULL
           REFERENCES criteria.guidelines(guideline_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         value_code TEXT NOT NULL,
         condition_label TEXT NOT NULL,
         max_change_value NUMERIC,
         max_change_percent NUMERIC,
         change_unit TEXT,
         background_lower_bound NUMERIC,
         background_upper_bound NUMERIC,
         background_unit TEXT,
         duration_label TEXT,
         flow_condition TEXT,
         sort_order INTEGER NOT NULL DEFAULT 100,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         UNIQUE (guideline_id, value_code),
         CONSTRAINT guideline_narrative_values_change_required
           CHECK (
             max_change_value IS NOT NULL
             OR max_change_percent IS NOT NULL
             OR note IS NOT NULL
           ),
         CONSTRAINT guideline_narrative_values_background_order
           CHECK (
             background_lower_bound IS NULL
             OR background_upper_bound IS NULL
             OR background_lower_bound <= background_upper_bound
           )
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.guideline_narrative_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guideline_narrative_values_guideline_idx
       ON criteria.guideline_narrative_values (guideline_id, sort_order);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE criteria.guideline_narrative_values IS
       'Structured non-scalar guideline values, such as allowable increases from background under named conditions.';"
    )

    # Validation triggers ###################################################
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

         IF NEW.algorithm_code = 'model_result_cache' THEN
           IF NEW.model_code IS NULL OR NEW.model_output_code IS NULL THEN
             RAISE EXCEPTION
               'model_code and model_output_code are required for model_result_cache rules.';
           END IF;
         ELSIF NEW.model_code IS NOT NULL OR NEW.model_output_code IS NOT NULL THEN
           RAISE EXCEPTION
             'model_code and model_output_code may only be populated for model-backed rules.';
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION criteria.validate_guideline_value_rule() OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_value_rule
       ON criteria.guideline_value_rules;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_value_rule
       BEFORE INSERT OR UPDATE
       ON criteria.guideline_value_rules
       FOR EACH ROW
       EXECUTE FUNCTION criteria.validate_guideline_value_rule();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.validate_guideline_lookup_value()
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
         FROM criteria.guideline_lookup_values glv
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
      "ALTER FUNCTION criteria.validate_guideline_lookup_value()
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_lookup_value
       ON criteria.guideline_lookup_values;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_lookup_value
       BEFORE INSERT OR UPDATE
       ON criteria.guideline_lookup_values
       FOR EACH ROW
       EXECUTE FUNCTION criteria.validate_guideline_lookup_value();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.validate_guideline_lookup_cell_range()
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
         FROM criteria.guideline_lookup_dimensions gld
         WHERE gld.lookup_table_id = NEW.lookup_table_id;

         SELECT count(*) = dimension_count
         INTO current_cell_complete
         FROM criteria.guideline_lookup_cell_ranges glcr
         WHERE glcr.cell_id = NEW.cell_id;

         IF NOT current_cell_complete THEN
           RETURN NEW;
         END IF;

         SELECT other_cell.cell_id
         INTO overlap_cell_id
         FROM criteria.guideline_lookup_cells other_cell
         WHERE other_cell.lookup_table_id = NEW.lookup_table_id
           AND other_cell.cell_id <> NEW.cell_id
           AND (
             SELECT count(*) = dimension_count
             FROM criteria.guideline_lookup_cell_ranges other_range
             WHERE other_range.cell_id = other_cell.cell_id
           )
           AND NOT EXISTS (
             SELECT 1
             FROM criteria.guideline_lookup_dimensions dim
             JOIN criteria.guideline_lookup_cell_ranges this_range
               ON this_range.cell_id = NEW.cell_id
              AND this_range.dimension_id = dim.dimension_id
             JOIN criteria.guideline_lookup_cell_ranges other_range
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
      "ALTER FUNCTION criteria.validate_guideline_lookup_cell_range()
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS validate_guideline_lookup_cell_range
       ON criteria.guideline_lookup_cell_ranges;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_guideline_lookup_cell_range
       AFTER INSERT OR UPDATE
       ON criteria.guideline_lookup_cell_ranges
       FOR EACH ROW
       EXECUTE FUNCTION criteria.validate_guideline_lookup_cell_range();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guideline_rule_input_units
       ON criteria.guideline_rule_inputs;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_guideline_rule_input_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id
       ON criteria.guideline_rule_inputs
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guideline_model_input_units
       ON criteria.guideline_model_inputs;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_guideline_model_input_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id
       ON criteria.guideline_model_inputs
       FOR EACH ROW EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )

    # Derivation functions ##################################################
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.get_sample_hardness(
         in_sample_id INTEGER
       )
       RETURNS NUMERIC
       LANGUAGE sql
       STABLE
       SET search_path = discrete, public
       AS $function$
       WITH ids AS (
         SELECT
           (
             SELECT parameter_id
             FROM public.parameters
             WHERE lower(param_name) = 'hardness'
             ORDER BY parameter_id
             LIMIT 1
           ) AS hardness_parameter_id,
           (
             SELECT parameter_id
             FROM public.parameters
             WHERE lower(param_name) = 'calcium'
             ORDER BY parameter_id
             LIMIT 1
           ) AS calcium_parameter_id,
           (
             SELECT parameter_id
             FROM public.parameters
             WHERE lower(param_name) = 'magnesium'
             ORDER BY parameter_id
             LIMIT 1
           ) AS magnesium_parameter_id,
           (
             SELECT sample_fraction_id
             FROM discrete.sample_fractions
             WHERE lower(sample_fraction) = 'dissolved'
             ORDER BY sample_fraction_id
             LIMIT 1
           ) AS dissolved_fraction_id,
           (
             SELECT sample_fraction_id
             FROM discrete.sample_fractions
             WHERE lower(sample_fraction) = 'total'
             ORDER BY sample_fraction_id
             LIMIT 1
           ) AS total_fraction_id,
           (
             SELECT result_speciation_id
             FROM discrete.result_speciations
             WHERE lower(result_speciation) = 'as caco3'
             ORDER BY result_speciation_id
             LIMIT 1
           ) AS caco3_speciation_id
       ),
       vals AS (
         SELECT
           max(r.result) FILTER (
             WHERE ids.calcium_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND r.parameter_id = ids.calcium_parameter_id
               AND r.sample_fraction_id = ids.dissolved_fraction_id
           ) AS calcium_dissolved,
           max(r.result) FILTER (
             WHERE ids.magnesium_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND r.parameter_id = ids.magnesium_parameter_id
               AND r.sample_fraction_id = ids.dissolved_fraction_id
           ) AS magnesium_dissolved,
           max(r.result) FILTER (
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND ids.caco3_speciation_id IS NOT NULL
               AND r.parameter_id = ids.hardness_parameter_id
               AND r.sample_fraction_id = ids.dissolved_fraction_id
               AND r.result_speciation_id = ids.caco3_speciation_id
           ) AS hardness_dissolved_caco3,
           max(r.result) FILTER (
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND r.parameter_id = ids.hardness_parameter_id
               AND r.sample_fraction_id = ids.dissolved_fraction_id
           ) AS hardness_dissolved_any_speciation,
           max(r.result) FILTER (
             WHERE ids.calcium_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND r.parameter_id = ids.calcium_parameter_id
               AND r.sample_fraction_id = ids.total_fraction_id
           ) AS calcium_total,
           max(r.result) FILTER (
             WHERE ids.magnesium_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND r.parameter_id = ids.magnesium_parameter_id
               AND r.sample_fraction_id = ids.total_fraction_id
           ) AS magnesium_total,
           max(r.result) FILTER (
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND ids.caco3_speciation_id IS NOT NULL
               AND r.parameter_id = ids.hardness_parameter_id
               AND r.sample_fraction_id = ids.total_fraction_id
               AND r.result_speciation_id = ids.caco3_speciation_id
           ) AS hardness_total_caco3,
           max(r.result) FILTER (
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND r.parameter_id = ids.hardness_parameter_id
               AND r.sample_fraction_id = ids.total_fraction_id
           ) AS hardness_total_any_speciation
         FROM ids
         LEFT JOIN discrete.results r
           ON r.sample_id = in_sample_id
       )
       SELECT CASE
         WHEN calcium_dissolved > 0 AND magnesium_dissolved > 0
           THEN 2.497 * calcium_dissolved + 4.118 * magnesium_dissolved
         WHEN hardness_dissolved_caco3 > 0
           THEN hardness_dissolved_caco3
         WHEN hardness_dissolved_any_speciation > 0
           THEN hardness_dissolved_any_speciation
         WHEN calcium_total > 0 AND magnesium_total > 0
           THEN 2.497 * calcium_total + 4.118 * magnesium_total
         WHEN hardness_total_caco3 > 0
           THEN hardness_total_caco3
         WHEN hardness_total_any_speciation > 0
           THEN hardness_total_any_speciation
         ELSE NULL::NUMERIC
       END
       FROM vals;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION criteria.get_sample_hardness(INTEGER)
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION criteria.get_sample_hardness(INTEGER) IS
       'Returns sample hardness as CaCO3 from same-sample results, preferring dissolved calcium/magnesium, then dissolved hardness as CaCO3, then total calcium/magnesium, then total hardness as CaCO3. Parameter, fraction, and speciation identifiers are resolved from reference tables rather than hard-coded IDs.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.get_sample_hardness(
         in_sample_id INTEGER,
         in_result_type_preference INTEGER[]
       )
       RETURNS NUMERIC
       LANGUAGE sql
       STABLE
       SET search_path = discrete, public
       AS $function$
       WITH ids AS (
         SELECT
           (
             SELECT parameter_id
             FROM public.parameters
             WHERE lower(param_name) = 'hardness'
             ORDER BY parameter_id
             LIMIT 1
           ) AS hardness_parameter_id,
           (
             SELECT parameter_id
             FROM public.parameters
             WHERE lower(param_name) = 'calcium'
             ORDER BY parameter_id
             LIMIT 1
           ) AS calcium_parameter_id,
           (
             SELECT parameter_id
             FROM public.parameters
             WHERE lower(param_name) = 'magnesium'
             ORDER BY parameter_id
             LIMIT 1
           ) AS magnesium_parameter_id,
           (
             SELECT sample_fraction_id
             FROM discrete.sample_fractions
             WHERE lower(sample_fraction) = 'dissolved'
             ORDER BY sample_fraction_id
             LIMIT 1
           ) AS dissolved_fraction_id,
           (
             SELECT sample_fraction_id
             FROM discrete.sample_fractions
             WHERE lower(sample_fraction) = 'total'
             ORDER BY sample_fraction_id
             LIMIT 1
           ) AS total_fraction_id,
           (
             SELECT result_speciation_id
             FROM discrete.result_speciations
             WHERE lower(result_speciation) = 'as caco3'
             ORDER BY result_speciation_id
             LIMIT 1
           ) AS caco3_speciation_id
       ),
       candidates AS (
         SELECT
           r.*,
           CASE
             WHEN in_result_type_preference IS NULL
                  OR array_length(in_result_type_preference, 1) IS NULL
               THEN 1
             ELSE array_position(in_result_type_preference, r.result_type)
           END AS preference_rank
         FROM discrete.results r
         WHERE r.sample_id = in_sample_id
           AND r.result IS NOT NULL
           AND (
             in_result_type_preference IS NULL
             OR array_length(in_result_type_preference, 1) IS NULL
             OR r.result_type = ANY(in_result_type_preference)
           )
       ),
       vals AS (
         SELECT
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.calcium_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND c.parameter_id = ids.calcium_parameter_id
               AND c.sample_fraction_id = ids.dissolved_fraction_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS calcium_dissolved,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.magnesium_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND c.parameter_id = ids.magnesium_parameter_id
               AND c.sample_fraction_id = ids.dissolved_fraction_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS magnesium_dissolved,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND ids.caco3_speciation_id IS NOT NULL
               AND c.parameter_id = ids.hardness_parameter_id
               AND c.sample_fraction_id = ids.dissolved_fraction_id
               AND c.result_speciation_id = ids.caco3_speciation_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS hardness_dissolved_caco3,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.dissolved_fraction_id IS NOT NULL
               AND c.parameter_id = ids.hardness_parameter_id
               AND c.sample_fraction_id = ids.dissolved_fraction_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS hardness_dissolved_any_speciation,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.calcium_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND c.parameter_id = ids.calcium_parameter_id
               AND c.sample_fraction_id = ids.total_fraction_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS calcium_total,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.magnesium_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND c.parameter_id = ids.magnesium_parameter_id
               AND c.sample_fraction_id = ids.total_fraction_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS magnesium_total,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND ids.caco3_speciation_id IS NOT NULL
               AND c.parameter_id = ids.hardness_parameter_id
               AND c.sample_fraction_id = ids.total_fraction_id
               AND c.result_speciation_id = ids.caco3_speciation_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS hardness_total_caco3,
           (
             SELECT c.result
             FROM candidates c, ids
             WHERE ids.hardness_parameter_id IS NOT NULL
               AND ids.total_fraction_id IS NOT NULL
               AND c.parameter_id = ids.hardness_parameter_id
               AND c.sample_fraction_id = ids.total_fraction_id
             ORDER BY c.preference_rank, c.result_id
             LIMIT 1
           ) AS hardness_total_any_speciation
       )
       SELECT CASE
         WHEN calcium_dissolved > 0 AND magnesium_dissolved > 0
           THEN 2.497 * calcium_dissolved + 4.118 * magnesium_dissolved
         WHEN hardness_dissolved_caco3 > 0
           THEN hardness_dissolved_caco3
         WHEN hardness_dissolved_any_speciation > 0
           THEN hardness_dissolved_any_speciation
         WHEN calcium_total > 0 AND magnesium_total > 0
           THEN 2.497 * calcium_total + 4.118 * magnesium_total
         WHEN hardness_total_caco3 > 0
           THEN hardness_total_caco3
         WHEN hardness_total_any_speciation > 0
           THEN hardness_total_any_speciation
         ELSE NULL::NUMERIC
       END
       FROM vals;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION criteria.get_sample_hardness(INTEGER, INTEGER[])
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION criteria.get_sample_hardness(INTEGER, INTEGER[]) IS
       'Returns sample hardness as CaCO3 using the same metadata-driven fallback as criteria.get_sample_hardness(integer), while respecting an optional ordered result_type_id preference array.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.guideline_apply_rounding(
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
      "ALTER FUNCTION criteria.guideline_apply_rounding(NUMERIC, INTEGER, TEXT)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.guideline_get_input_value(
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
         selected_rank INTEGER;
         hardness_value NUMERIC;
       BEGIN
         SELECT gri.*
         INTO input_row
         FROM criteria.guideline_rule_inputs gri
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

         IF input_row.input_source = 'hardness_helper' THEN
           hardness_value := criteria.get_sample_hardness(
             in_sample_id,
             input_row.result_type_preference
           );

           IF hardness_value IS NULL THEN
             RETURN QUERY
             SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                    NULL::INTEGER, 'missing_input'::TEXT,
                    format(
                      'No same-sample hardness value could be derived for sample_id %s.',
                      in_sample_id
                    )::TEXT;
             RETURN;
           END IF;

           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, hardness_value,
                  NULL::INTEGER, 'value'::TEXT,
                  'Hardness resolved by criteria.get_sample_hardness(sample_id).'::TEXT;
           RETURN;
         END IF;

         WITH candidates AS (
           SELECT
             r.result_id,
             r.result_type,
             CASE
               WHEN input_row.result_type_preference IS NULL
                    OR array_length(input_row.result_type_preference, 1) IS NULL
                 THEN 1
               ELSE array_position(input_row.result_type_preference, r.result_type)
             END AS preference_rank,
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
             AND (
               input_row.result_type_preference IS NULL
               OR array_length(input_row.result_type_preference, 1) IS NULL
               OR r.result_type = ANY(input_row.result_type_preference)
             )
         )
         SELECT count(*)::INTEGER,
                count(value_for_use)::INTEGER,
                min(preference_rank) FILTER (WHERE value_for_use IS NOT NULL)
         INTO matching_count, usable_count, selected_rank
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

         WITH candidates AS (
           SELECT
             r.result_id,
             CASE
               WHEN input_row.result_type_preference IS NULL
                    OR array_length(input_row.result_type_preference, 1) IS NULL
                 THEN 1
               ELSE array_position(input_row.result_type_preference, r.result_type)
             END AS preference_rank,
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
             AND (
               input_row.result_type_preference IS NULL
               OR array_length(input_row.result_type_preference, 1) IS NULL
               OR r.result_type = ANY(input_row.result_type_preference)
             )
         ),
         preferred_candidates AS (
           SELECT *
           FROM candidates
           WHERE value_for_use IS NOT NULL
             AND preference_rank IS NOT DISTINCT FROM selected_rank
         )
         SELECT count(*)::INTEGER
         INTO usable_count
         FROM preferred_candidates;

         IF usable_count > 1 AND input_row.aggregate_method = 'single' THEN
           RETURN QUERY
           SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC,
                  NULL::INTEGER, 'ambiguous_input'::TEXT,
                  format(
                    'Input %s matched %s usable results at the selected result-type preference rank; specify an aggregate method or narrower input qualifiers.',
                    input_row.input_code,
                    usable_count
                  )::TEXT;
           RETURN;
         END IF;

         WITH candidates AS (
           SELECT
             r.result_id,
             CASE
               WHEN input_row.result_type_preference IS NULL
                    OR array_length(input_row.result_type_preference, 1) IS NULL
                 THEN 1
               ELSE array_position(input_row.result_type_preference, r.result_type)
             END AS preference_rank,
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
             AND (
               input_row.result_type_preference IS NULL
               OR array_length(input_row.result_type_preference, 1) IS NULL
               OR r.result_type = ANY(input_row.result_type_preference)
             )
         ),
         preferred_candidates AS (
           SELECT *
           FROM candidates
           WHERE value_for_use IS NOT NULL
             AND preference_rank IS NOT DISTINCT FROM selected_rank
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
         FROM preferred_candidates;

         RETURN QUERY
         SELECT input_row.input_id, input_row.input_code, selected_value,
                selected_result_id, 'value'::TEXT, NULL::TEXT;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION criteria.guideline_get_input_value(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.guideline_collect_rule_inputs(
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
             gri.input_source,
             gri.result_type_preference,
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
           FROM criteria.guideline_rule_inputs gri
           JOIN criteria.guideline_value_rules gr
             ON gr.rule_id = gri.rule_id
           LEFT JOIN criteria.guideline_model_inputs gmi
             ON gmi.model_code = gr.model_code
            AND gmi.input_code = gri.input_code
           CROSS JOIN LATERAL criteria.guideline_get_input_value(
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
                   'input_source', input_source,
                   'result_type_preference', result_type_preference,
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
      "ALTER FUNCTION criteria.guideline_collect_rule_inputs(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.evaluate_guideline_rule(
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
         FROM criteria.guideline_value_rules gr
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
           FROM criteria.guideline_rule_inputs gri
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
           FROM criteria.guideline_rule_inputs gri
           WHERE gri.rule_id = rule_row.rule_id
           ORDER BY gri.input_code
           LIMIT 1;

           SELECT *
           INTO input_result
           FROM criteria.guideline_get_input_value(
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
             FROM criteria.guideline_lookup_values glv
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
             FROM criteria.guideline_rule_coefficients
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
             FROM criteria.guideline_rule_coefficients
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
             FROM criteria.guideline_rule_coefficients
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
           FROM criteria.guideline_collect_rule_inputs(
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
           FROM criteria.guideline_lookup_tables glt
           WHERE glt.rule_id = rule_row.rule_id;

           IF NOT FOUND THEN
             computed_status := 'configuration_error';
             computed_message := format(
               'No multidimensional lookup table is registered for rule_id %s.',
               rule_row.rule_id
             );
           ELSE
             SELECT glc.*
             INTO lookup_cell_row
             FROM criteria.guideline_lookup_cells glc
             WHERE glc.lookup_table_id = lookup_table_row.lookup_table_id
               AND NOT EXISTS (
                 SELECT 1
                 FROM criteria.guideline_lookup_dimensions gld
                 LEFT JOIN criteria.guideline_lookup_cell_ranges glcr
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
                 'No lookup grid cell matched rule_id %s.',
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

         ELSIF rule_row.algorithm_code = 'model_result_cache' THEN
           SELECT *
           INTO collected_inputs
           FROM criteria.guideline_collect_rule_inputs(
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

           SELECT gmr.*
           INTO model_result_row
           FROM criteria.guideline_model_results gmr
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

           computed_value := criteria.guideline_apply_rounding(
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
      "ALTER FUNCTION criteria.evaluate_guideline_rule(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.evaluate_guideline(
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
         FROM criteria.guideline_value_rules gr
         CROSS JOIN LATERAL criteria.evaluate_guideline_rule(
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
      "ALTER FUNCTION criteria.evaluate_guideline(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.get_guideline_value(
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
         FROM criteria.evaluate_guideline(
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
      "ALTER FUNCTION criteria.get_guideline_value(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.applicable_guideline_rules_for_result(
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
             s.location_id,
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
         JOIN criteria.guidelines g
           ON g.parameter_id = rr.parameter_id
          AND g.matrix_state_id = rr.matrix_state_id
         JOIN criteria.guideline_comparison_operators op
           ON op.operator_code = g.comparison_operator_code
         LEFT JOIN criteria.guideline_publishers gp
           ON gp.publisher_id = g.publisher_id
         LEFT JOIN criteria.guideline_series gs
           ON gs.series_id = g.series_id
         LEFT JOIN criteria.guideline_jurisdictions gj
           ON gj.jurisdiction_id = g.jurisdiction_id
         LEFT JOIN criteria.guideline_protection_goals gpg
           ON gpg.protection_goal_id = g.protection_goal_id
         LEFT JOIN criteria.guideline_exposure_durations ged
           ON ged.exposure_duration_id = g.exposure_duration_id
         LEFT JOIN criteria.guideline_averaging_periods gap
           ON gap.averaging_period_id = g.averaging_period_id
         CROSS JOIN LATERAL criteria.evaluate_guideline(
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
               FROM criteria.guidelines_media_types gm_any
               WHERE gm_any.guideline_id = g.guideline_id
             )
             OR EXISTS (
               SELECT 1
               FROM criteria.guidelines_media_types gm
               WHERE gm.guideline_id = g.guideline_id
                 AND gm.media_id = rr.media_id
             )
           )
           AND (
             NOT EXISTS (
               SELECT 1
               FROM criteria.guidelines_fractions gf_any
               WHERE gf_any.guideline_id = g.guideline_id
             )
             OR EXISTS (
               SELECT 1
               FROM criteria.guidelines_fractions gf
               WHERE gf.guideline_id = g.guideline_id
                 AND gf.fraction_id IS NOT DISTINCT FROM
                   rr.sample_fraction_id
             )
           )
           AND (
             NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_locations gl_any
               WHERE gl_any.guideline_id = g.guideline_id
                 AND gl_any.active
             )
             OR EXISTS (
               SELECT 1
               FROM criteria.guideline_locations gl
               WHERE gl.guideline_id = g.guideline_id
                 AND gl.active
                 AND gl.location_id = rr.location_id
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
      "ALTER FUNCTION criteria.applicable_guideline_rules_for_result(
         INTEGER, DATE, BOOLEAN
       ) OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS criteria.results_guideline_values;"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS criteria.applicable_guidelines_for_result(
         INTEGER, DATE, BOOLEAN
       );"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION criteria.applicable_guidelines_for_result(
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
           FROM criteria.applicable_guideline_rules_for_result(
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
      "ALTER FUNCTION criteria.applicable_guidelines_for_result(
         INTEGER, DATE, BOOLEAN
       ) OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW criteria.results_guideline_rule_values
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT ag.*
       FROM discrete.results r
       CROSS JOIN LATERAL criteria.applicable_guideline_rules_for_result(
         r.result_id,
         CURRENT_DATE,
         TRUE
       ) ag;"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW criteria.results_guideline_rule_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW criteria.results_guideline_rule_values IS
       'Detailed database-driven view with one row per matched guideline rule or bound.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW criteria.results_guideline_values
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT ag.*
       FROM discrete.results r
       CROSS JOIN LATERAL criteria.applicable_guidelines_for_result(
         r.result_id,
         CURRENT_DATE,
         TRUE
       ) ag;"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW criteria.results_guideline_values OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW criteria.results_guideline_values IS
       'Database-driven one-row-per-guideline view that reports derived lower_guideline_value and upper_guideline_value bounds for each result. Per-rule scalar guideline_value rows are exposed by criteria.results_guideline_rule_values.';"
    )

    # Audit, modification metadata, and grants ##############################
    for (table_name in c(
      "guideline_comparison_operators",
      "guideline_value_algorithms",
      "guideline_jurisdictions",
      "guideline_jurisdiction_levels",
      "guideline_protection_goals",
      "guideline_exposure_durations",
      "guideline_averaging_periods",
      "guideline_publishers",
      "guideline_series",
      "guidelines",
      "guidelines_media_types",
      "guidelines_fractions",
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
      "guideline_lookup_cell_ranges",
      "guideline_narrative_values"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_user_modified
           ON criteria.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON criteria.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified();",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_update_modified
           ON criteria.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON criteria.%s
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
           'guideline_publishers',
           'guideline_series',
           'guidelines',
           'guidelines_media_types',
           'guidelines_fractions',
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
            'guideline_lookup_cell_ranges',
            'guideline_narrative_values'
         ]
         LOOP
           EXECUTE format(
             'DROP TRIGGER IF EXISTS audit_%I_trigger ON criteria.%I;',
             table_name,
             table_name
           );
           EXECUTE format(
             'CREATE TRIGGER audit_%I_trigger
              AFTER UPDATE OR DELETE ON criteria.%I
              FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func();',
             table_name,
             table_name
           );
         END LOOP;
       END $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS guideline_locations_user_modified
       ON criteria.guideline_locations;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guideline_locations_user_modified
       BEFORE UPDATE ON criteria.guideline_locations
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS guideline_locations_update_modified
       ON criteria.guideline_locations;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guideline_locations_update_modified
       BEFORE UPDATE ON criteria.guideline_locations
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF to_regprocedure('audit.if_modified_func()') IS NOT NULL THEN
           DROP TRIGGER IF EXISTS audit_guideline_locations_trigger
             ON criteria.guideline_locations;
           CREATE TRIGGER audit_guideline_locations_trigger
             AFTER UPDATE OR DELETE ON criteria.guideline_locations
             FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func();
         END IF;
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
             EXECUTE format('GRANT USAGE ON SCHEMA criteria TO %I;', role_name);
             EXECUTE format(
               'GRANT ALL PRIVILEGES ON TABLE
                  criteria.guideline_comparison_operators,
                  criteria.guideline_value_algorithms,
                  criteria.guideline_jurisdictions,
                  criteria.guideline_jurisdiction_levels,
                  criteria.guideline_protection_goals,
                  criteria.guideline_exposure_durations,
                  criteria.guideline_averaging_periods,
                  criteria.guideline_publishers,
                  criteria.guideline_series,
                  criteria.guidelines,
                  criteria.guidelines_media_types,
                  criteria.guidelines_fractions,
                  criteria.guideline_models,
                  criteria.guideline_model_inputs,
                  criteria.guideline_model_outputs,
                  criteria.guideline_model_results,
                  criteria.guideline_value_rules,
                  criteria.guideline_rule_inputs,
                  criteria.guideline_rule_coefficients,
                  criteria.guideline_lookup_values,
                   criteria.guideline_lookup_tables,
                   criteria.guideline_lookup_dimensions,
                   criteria.guideline_lookup_cells,
                   criteria.guideline_lookup_cell_ranges,
                   criteria.guideline_narrative_values,
                   criteria.results_guideline_rule_values,
                  criteria.results_guideline_values
                TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT ALL PRIVILEGES ON TABLE
                  criteria.guideline_locations
                TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA discrete TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA criteria TO %I;',
               role_name
             );
             EXECUTE format(
                'GRANT EXECUTE ON FUNCTION
                   criteria.get_sample_hardness(INTEGER),
                   criteria.get_sample_hardness(INTEGER, INTEGER[]),
                   criteria.guideline_apply_rounding(NUMERIC, INTEGER, TEXT),
                  criteria.guideline_get_input_value(INTEGER, INTEGER),
                  criteria.guideline_collect_rule_inputs(INTEGER, INTEGER),
                  criteria.evaluate_guideline_rule(INTEGER, INTEGER),
                  criteria.evaluate_guideline(INTEGER, INTEGER),
                  criteria.get_guideline_value(INTEGER, INTEGER),
                  criteria.applicable_guideline_rules_for_result(INTEGER, DATE, BOOLEAN),
                  criteria.applicable_guidelines_for_result(INTEGER, DATE, BOOLEAN)
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
             EXECUTE format('GRANT USAGE ON SCHEMA criteria TO %I;', role_name);
             EXECUTE format(
               'GRANT SELECT ON TABLE
                  criteria.guideline_comparison_operators,
                  criteria.guideline_value_algorithms,
                  criteria.guideline_jurisdictions,
                  criteria.guideline_jurisdiction_levels,
                  criteria.guideline_protection_goals,
                  criteria.guideline_exposure_durations,
                  criteria.guideline_averaging_periods,
                  criteria.guideline_publishers,
                  criteria.guideline_series,
                  criteria.guidelines,
                  criteria.guidelines_media_types,
                  criteria.guidelines_fractions,
                  criteria.guideline_models,
                  criteria.guideline_model_inputs,
                  criteria.guideline_model_outputs,
                  criteria.guideline_model_results,
                  criteria.guideline_value_rules,
                  criteria.guideline_rule_inputs,
                  criteria.guideline_rule_coefficients,
                  criteria.guideline_lookup_values,
                   criteria.guideline_lookup_tables,
                   criteria.guideline_lookup_dimensions,
                   criteria.guideline_lookup_cells,
                   criteria.guideline_lookup_cell_ranges,
                   criteria.guideline_narrative_values,
                   criteria.results_guideline_rule_values,
                  criteria.results_guideline_values
                TO %I;',
               role_name
             );
             EXECUTE format(
               'GRANT SELECT ON TABLE
                  criteria.guideline_locations
                TO %I;',
               role_name
             );
             EXECUTE format(
                'GRANT EXECUTE ON FUNCTION
                   criteria.get_sample_hardness(INTEGER),
                   criteria.get_sample_hardness(INTEGER, INTEGER[]),
                   criteria.guideline_apply_rounding(NUMERIC, INTEGER, TEXT),
                  criteria.guideline_get_input_value(INTEGER, INTEGER),
                  criteria.guideline_collect_rule_inputs(INTEGER, INTEGER),
                  criteria.evaluate_guideline_rule(INTEGER, INTEGER),
                  criteria.evaluate_guideline(INTEGER, INTEGER),
                  criteria.get_guideline_value(INTEGER, INTEGER),
                  criteria.applicable_guideline_rules_for_result(INTEGER, DATE, BOOLEAN),
                  criteria.applicable_guidelines_for_result(INTEGER, DATE, BOOLEAN)
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
