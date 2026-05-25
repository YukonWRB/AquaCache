# Deep test seed for patch 47 guideline behaviours.
#
# This script is for development/test databases. It creates persistent samples,
# guidelines, and smoke checks for constants, ranges, lower-bound criteria,
# lookup-derived values, site-specific/no-value outcomes, narrative guidelines,
# and multi-input database functions.

created_connection <- FALSE

if (!exists("con", inherits = TRUE)) {
  suppressPackageStartupMessages({
    library(DBI)
    library(RPostgres)
  })

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("aquacacheName"),
    host = Sys.getenv("aquacacheHost"),
    port = Sys.getenv("aquacachePort"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass")
  )
  created_connection <- TRUE
}

one_value <- function(sql, params = list(), label = "query") {
  if (length(params) == 0) {
    out <- DBI::dbGetQuery(con, sql)
  } else {
    out <- DBI::dbGetQuery(con, sql, params = params)
  }
  if (nrow(out) == 0 || is.na(out[[1]][[1]])) {
    stop("Could not resolve ", label, call. = FALSE)
  }
  out[[1]][[1]]
}

parameter_id <- function(pattern, label, preferred = character()) {
  params <- list(pattern)
  if (length(preferred) > 0) {
    preferred_sql <- paste(
      "WHEN lower(param_name) = lower($",
      seq_along(preferred) + 1L,
      ") THEN ",
      seq_along(preferred) - 1L,
      sep = "",
      collapse = " "
    )
    params <- c(params, as.list(preferred))
    order_sql <- paste0("CASE ", preferred_sql, " ELSE 100 END, parameter_id")
  } else {
    order_sql <- "parameter_id"
  }

  one_value(
    paste0(
      "SELECT parameter_id
       FROM public.parameters
       WHERE lower(param_name) ~ lower($1)
       ORDER BY ",
      order_sql,
      "
       LIMIT 1"
    ),
    params = params,
    label = label
  )
}

upsert_publisher <- function(code, name, url, note = NA_character_) {
  DBI::dbExecute(
    con,
    "UPDATE discrete.guideline_publishers
     SET publisher_name = $2,
         publisher_url = $3,
         note = $4
     WHERE publisher_code = $1",
    params = list(code, name, url, note)
  )

  existing <- DBI::dbGetQuery(
    con,
    "SELECT publisher_id
     FROM discrete.guideline_publishers
     WHERE publisher_code = $1",
    params = list(code)
  )
  if (nrow(existing) > 0) {
    return(existing$publisher_id[[1]])
  }

  one_value(
    "INSERT INTO discrete.guideline_publishers (
       publisher_code, publisher_name, publisher_url, note
     )
     VALUES ($1, $2, $3, $4)
     RETURNING publisher_id",
    params = list(code, name, url, note),
    label = paste("publisher", code)
  )
}

upsert_series <- function(code, name, publisher_id, url, citation,
                          note = NA_character_) {
  DBI::dbExecute(
    con,
    "UPDATE discrete.guideline_series
     SET series_name = $2,
         publisher_id = $3,
         series_url = $4,
         citation = $5,
         note = $6
     WHERE series_code = $1",
    params = list(code, name, publisher_id, url, citation, note)
  )

  existing <- DBI::dbGetQuery(
    con,
    "SELECT series_id
     FROM discrete.guideline_series
     WHERE series_code = $1",
    params = list(code)
  )
  if (nrow(existing) > 0) {
    return(existing$series_id[[1]])
  }

  one_value(
    "INSERT INTO discrete.guideline_series (
       series_code, series_name, publisher_id, series_url, citation, note
     )
     VALUES ($1, $2, $3, $4, $5, $6)
     RETURNING series_id",
    params = list(code, name, publisher_id, url, citation, note),
    label = paste("series", code)
  )
}

upsert_guideline_ref <- function(table, id_col, code_col, name_col, prefix,
                                 name) {
  if (is.na(name) || !nzchar(trimws(name))) return(NA_integer_)
  existing <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s
       FROM %s
       WHERE lower(btrim(%s)) = lower(btrim($1))
       LIMIT 1",
      id_col, table, name_col
    ),
    params = list(name)
  )
  if (nrow(existing)) return(existing[[id_col]][[1]])
  inserted <- DBI::dbGetQuery(
    con,
    sprintf(
      "WITH existing AS (
         SELECT %1$s
         FROM %2$s
         WHERE lower(btrim(%3$s)) = lower(btrim($1))
         LIMIT 1
       ),
       inserted AS (
         INSERT INTO %2$s (%4$s, %3$s, sort_order)
         SELECT $2 || '_' || upper(left(md5($1), 10)), btrim($1), 800
         WHERE NOT EXISTS (SELECT 1 FROM existing)
         ON CONFLICT DO NOTHING
         RETURNING %1$s
       )
       SELECT %1$s FROM inserted
       UNION ALL
       SELECT %1$s FROM existing
       LIMIT 1",
      id_col, table, name_col, code_col
    ),
    params = list(name, prefix)
  )
  if (nrow(inserted)) return(inserted[[id_col]][[1]])
  existing <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s
       FROM %s
       WHERE lower(btrim(%s)) = lower(btrim($1))
       LIMIT 1",
      id_col, table, name_col
    ),
    params = list(name)
  )
  if (nrow(existing)) return(existing[[id_col]][[1]])
  stop("Could not resolve guideline reference value: ", name, call. = FALSE)
}

jurisdiction_id <- function(name) upsert_guideline_ref(
  "discrete.guideline_jurisdictions", "jurisdiction_id",
  "jurisdiction_code", "jurisdiction_name", "JUR", name
)
jurisdiction_level_id <- function(name) upsert_guideline_ref(
  "discrete.guideline_jurisdiction_levels", "jurisdiction_level_id",
  "jurisdiction_level_code", "jurisdiction_level_name", "LEVEL", name
)
protection_goal_id <- function(name) upsert_guideline_ref(
  "discrete.guideline_protection_goals", "protection_goal_id",
  "protection_goal_code", "protection_goal_name", "GOAL", name
)
exposure_duration_id <- function(name) upsert_guideline_ref(
  "discrete.guideline_exposure_durations", "exposure_duration_id",
  "exposure_duration_code", "exposure_duration_name", "EXPOSURE", name
)
averaging_period_id <- function(name) upsert_guideline_ref(
  "discrete.guideline_averaging_periods", "averaging_period_id",
  "averaging_period_code", "averaging_period_name", "AVG", name
)

insert_result <- function(sample_id, result_type, parameter_id,
                          sample_fraction_id, result, result_value_type,
                          result_speciation_id = NA_integer_,
                          matrix_state_id, analysis_datetime) {
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.results (
       sample_id, result_type, parameter_id, sample_fraction_id,
       result, result_condition, result_condition_value,
       result_value_type, result_speciation_id, analysis_datetime,
       share_with, no_update, matrix_state_id
     )
     VALUES (
       $1, $2, $3, $4, $5, NULL, NULL, $6, $7, $8,
       ARRAY['public_reader'], false, $9
     )",
    params = list(
      sample_id, result_type, parameter_id, sample_fraction_id,
      result, result_value_type, result_speciation_id, analysis_datetime,
      matrix_state_id
    )
  )
}

insert_guideline <- function(code, name, publisher_id, series_id,
                             parameter_id, matrix_state_id,
                             comparison_operator_code, source_url,
                             source_document_title, exposure_duration,
                             averaging_period, protection_goal,
                             jurisdiction = "test fixture",
                             source_table = NA_character_,
                             source_section = NA_character_,
                             fraction_id = NA_integer_,
                             media_id = NA_integer_) {
  jurisdiction_id_value <- jurisdiction_id(jurisdiction)
  jurisdiction_level_id_value <- jurisdiction_level_id("test fixture")
  protection_goal_id_value <- protection_goal_id(protection_goal)
  exposure_duration_id_value <- exposure_duration_id(exposure_duration)
  averaging_period_id_value <- averaging_period_id(averaging_period)
  guideline_id <- one_value(
    "INSERT INTO discrete.guidelines (
       guideline_code, guideline_name, publisher_id, series_id,
       parameter_id, matrix_state_id, comparison_operator_code,
       jurisdiction_id, jurisdiction_level_id, protection_goal_id,
       exposure_duration_id, averaging_period_id, source_document_title,
       source_url, source_table, source_section, source_retrieved_date,
       valid_from, review_status
     )
     VALUES (
       $1, $2, $3, $4, $5, $6, $7,
       $8, $9, $10, $11, $12, $13, $14, $15, $16,
       CURRENT_DATE, DATE '1900-01-01', 'draft'
     )
     RETURNING guideline_id",
    params = list(
      code, name, publisher_id, series_id, parameter_id, matrix_state_id,
      comparison_operator_code, jurisdiction_id_value,
      jurisdiction_level_id_value, protection_goal_id_value,
      exposure_duration_id_value, averaging_period_id_value,
      source_document_title, source_url, source_table, source_section
    ),
    label = code
  )

  if (!is.na(media_id)) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guidelines_media_types (guideline_id, media_id)
       VALUES ($1, $2)",
      params = list(guideline_id, media_id)
    )
  }

  if (!is.na(fraction_id)) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guidelines_fractions (guideline_id, fraction_id)
       VALUES ($1, $2)",
      params = list(guideline_id, fraction_id)
    )
  }

  guideline_id
}

constant_rule <- function(guideline_id, bound_code, value, note = NA_character_,
                          priority = 100L) {
  one_value(
    "INSERT INTO discrete.guideline_value_rules (
       guideline_id, bound_code, algorithm_code, fixed_value,
       rule_priority, note
     )
     VALUES ($1, $2, 'constant', $3, $4, $5)
     RETURNING rule_id",
    params = list(guideline_id, bound_code, value, priority, note),
    label = paste("rule", guideline_id)
  )
}

message("Seeding broad patch 47 guideline tests...")

invisible(tryCatch(
  DBI::dbWithTransaction(con, {
  required <- DBI::dbGetQuery(
    con,
    "SELECT
       to_regclass('discrete.guideline_value_rules') IS NOT NULL AS has_rules,
       to_regprocedure(
         'discrete.guideline_collect_rule_inputs(integer, integer)'
       ) IS NOT NULL AS has_collector"
  )
  if (!isTRUE(required$has_rules[[1]]) ||
      !isTRUE(required$has_collector[[1]])) {
    stop("Apply patch_47.R before running this seed script.")
  }

  matrix_liquid <- one_value(
    "SELECT matrix_state_id
     FROM public.matrix_states
     WHERE matrix_state_code = 'liquid'
     LIMIT 1",
    label = "liquid matrix"
  )
  media_surface <- one_value(
    "SELECT media_id
     FROM public.media_types
     WHERE lower(media_type) = 'surface water'
     LIMIT 1",
    label = "surface water"
  )
  result_type_lab <- one_value(
    "SELECT result_type_id
     FROM discrete.result_types
     WHERE lower(result_type) = 'lab'
     LIMIT 1",
    label = "lab result type"
  )
  result_type_field <- one_value(
    "SELECT result_type_id
     FROM discrete.result_types
     WHERE lower(result_type) = 'field'
     LIMIT 1",
    label = "field result type"
  )
  result_value_actual <- one_value(
    "SELECT result_value_type_id
     FROM discrete.result_value_types
     WHERE lower(result_value_type) = 'actual'
     LIMIT 1",
    label = "actual result value type"
  )
  fraction_total <- one_value(
    "SELECT sample_fraction_id
     FROM discrete.sample_fractions
     WHERE lower(sample_fraction) = 'total'
     LIMIT 1",
    label = "total fraction"
  )
  fraction_dissolved <- one_value(
    "SELECT sample_fraction_id
     FROM discrete.sample_fractions
     WHERE lower(sample_fraction) = 'dissolved'
     LIMIT 1",
    label = "dissolved fraction"
  )
  spec_caco3 <- one_value(
    "SELECT result_speciation_id
     FROM discrete.result_speciations
     WHERE lower(result_speciation) = 'as caco3'
     LIMIT 1",
    label = "as CaCO3 speciation"
  )

  chloride_param <- parameter_id("^chloride$", "chloride", "chloride")
  sulfate_param <- parameter_id("^sulfate$|^sulphate$", "sulfate", "sulfate")
  hardness_param <- parameter_id("^hardness$", "hardness", "hardness")
  ph_param <- parameter_id("^ph$", "pH", "pH")
  do_param <- parameter_id("^oxygen, dissolved$", "dissolved oxygen", "oxygen, dissolved")
  turbidity_param <- parameter_id("^turbidity$", "turbidity", "turbidity")
  zinc_param <- parameter_id("^zinc$", "zinc", "zinc")
  doc_param <- parameter_id(
    "^organic carbon$|^dissolved organic carbon$|^doc$",
    "dissolved organic carbon",
    c("dissolved organic carbon", "organic carbon")
  )

  sample_ids <- DBI::dbGetQuery(
    con,
    "SELECT sample_id
     FROM discrete.samples
     WHERE import_source = 'patch_47_deep_guideline_fixture'"
  )
  for (sample_id in sample_ids$sample_id) {
    DBI::dbExecute(
      con,
      "DELETE FROM discrete.results WHERE sample_id = $1",
      params = list(sample_id)
    )
    DBI::dbExecute(
      con,
      "DELETE FROM discrete.samples WHERE sample_id = $1",
      params = list(sample_id)
    )
  }

  fixture_guideline_codes <- c(
    "CCME-CL-FW-LT-2011",
    "CCME-CL-FW-ST-2011",
    "EPA-CL-FW-CCC-1988",
    "EPA-CL-FW-CMC-1988",
    "BC-SO4-FW-LT-HARDNESS-2013",
    "EPA-PH-FW-RANGE-1986",
    "BC-DO-FW-MIN-1997",
    "CCME-ZN-FW-ST-DOC-HARDNESS",
    "CCME-ZN-FW-ST-DOC-HARDNESS-SQLSCALAR",
    "BC-TURBIDITY-FW-BACKGROUND-NARRATIVE"
  )
  for (code in fixture_guideline_codes) {
    DBI::dbExecute(
      con,
      "DELETE FROM discrete.guidelines
       WHERE guideline_code = $1",
      params = list(code)
    )
  }
  DBI::dbExecute(
    con,
    "DELETE FROM discrete.guideline_models
     WHERE model_code = 'CCME_ZN_SHORT_TERM'"
  )

  ccme_id <- upsert_publisher(
    "CCME",
    "Canadian Council of Ministers of the Environment",
    "https://ccme.ca",
    "Seeded for patch 47 deep testing."
  )
  epa_id <- upsert_publisher(
    "US_EPA",
    "United States Environmental Protection Agency",
    "https://www.epa.gov/wqc",
    "Seeded for patch 47 deep testing."
  )
  bc_env_id <- upsert_publisher(
    "BC_ENV",
    "British Columbia Ministry of Environment and Climate Change Strategy",
    "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines",
    "Seeded for patch 47 deep testing."
  )

  ccme_series <- upsert_series(
    "CCME_CWQG_AQ_LIFE",
    "CCME Canadian water quality guidelines for aquatic life",
    ccme_id,
    "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
    "Canadian environmental quality guidelines."
  )
  epa_series <- upsert_series(
    "EPA_NRWQC_AQ_LIFE",
    "EPA national recommended aquatic life criteria",
    epa_id,
    "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table",
    "National Recommended Water Quality Criteria - Aquatic Life Criteria Table."
  )
  bc_series <- upsert_series(
    "BC_AQ_LIFE",
    "B.C. approved aquatic life water quality guidelines",
    bc_env_id,
    "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines/approved-water-quality-guidelines",
    "B.C. approved aquatic life water quality guidelines."
  )

  chloride_source <- "https://ccme.ca/en/chemical/28"
  epa_source <- "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table"
  sulfate_source <- "https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/sulphate/bc_moe_wqg_sulphate.pdf"
  do_source <- "https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/oxygen-or.pdf"
  turbidity_source <- "https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/wqg_summary_aquaticlife_wildlife_agri.pdf"
  zinc_source <- "https://ccme.ca/en/chemical/229"

  message("  adding chloride constants")
  guideline_id <- insert_guideline(
    "CCME-CL-FW-LT-2011",
    "CCME chloride freshwater long-term aquatic life guideline",
    ccme_id, ccme_series, chloride_param, matrix_liquid, "lte",
    chloride_source, "CCME chloride guideline page",
    "long-term", "indefinite exposure", "Freshwater aquatic life",
    "Canada", fraction_id = fraction_total, media_id = media_surface
  )
  constant_rule(guideline_id, "upper", 120, "120 mg/L chloride ion.")

  guideline_id <- insert_guideline(
    "CCME-CL-FW-ST-2011",
    "CCME chloride freshwater short-term aquatic life benchmark",
    ccme_id, ccme_series, chloride_param, matrix_liquid, "lte",
    chloride_source, "CCME chloride guideline page",
    "short-term", "transient exposure", "Freshwater aquatic life",
    "Canada", fraction_id = fraction_total, media_id = media_surface
  )
  constant_rule(guideline_id, "upper", 640, "640 mg/L chloride ion.")

  guideline_id <- insert_guideline(
    "EPA-CL-FW-CCC-1988",
    "EPA chloride freshwater chronic criterion",
    epa_id, epa_series, chloride_param, matrix_liquid, "lte",
    epa_source, "EPA national recommended aquatic life criteria table",
    "chronic", "continuous concentration", "Freshwater aquatic life",
    "United States", fraction_id = fraction_total, media_id = media_surface
  )
  constant_rule(guideline_id, "upper", 230, "230 mg/L chloride ion.")

  guideline_id <- insert_guideline(
    "EPA-CL-FW-CMC-1988",
    "EPA chloride freshwater acute criterion",
    epa_id, epa_series, chloride_param, matrix_liquid, "lte",
    epa_source, "EPA national recommended aquatic life criteria table",
    "acute", "maximum concentration", "Freshwater aquatic life",
    "United States", fraction_id = fraction_total, media_id = media_surface
  )
  constant_rule(guideline_id, "upper", 860, "860 mg/L chloride ion.")

  message("  adding sulphate lookup")
  sulfate_guideline <- insert_guideline(
    "BC-SO4-FW-LT-HARDNESS-2013",
    "B.C. sulphate freshwater aquatic life guideline by hardness",
    bc_env_id, bc_series, sulfate_param, matrix_liquid, "lte",
    sulfate_source, "Ambient Water Quality Guidelines for Sulphate",
    "long-term chronic", "30-day average", "Freshwater aquatic life",
    "British Columbia", source_table = "Sulphate guideline by water hardness",
    fraction_id = fraction_total, media_id = media_surface
  )
  sulfate_rule <- one_value(
    "INSERT INTO discrete.guideline_value_rules (
       guideline_id, bound_code, algorithm_code, rule_priority, note
     )
     VALUES ($1, 'upper', 'lookup_range', 10, 'B.C. sulphate lookup by hardness as CaCO3.')
     RETURNING rule_id",
    params = list(sulfate_guideline),
    label = "sulfate lookup rule"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_rule_inputs (
       rule_id, input_code, input_name, parameter_id, matrix_state_id,
       sample_fraction_id, result_speciation_id, result_type,
       aggregate_method, required
     )
     VALUES (
       $1, 'hardness_mg_l_caco3', 'Hardness as CaCO3', $2, $3,
       $4, $5, $6, 'single', true
     )",
    params = list(
      sulfate_rule, hardness_param, matrix_liquid, fraction_total,
      spec_caco3, result_type_lab
    )
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_lookup_values (
       rule_id, input_code, lower_bound, upper_bound,
       lower_inclusive, upper_inclusive, output_value,
       output_status, output_label, sort_order
     )
     VALUES
       ($1, 'hardness_mg_l_caco3', 0, 30, true, true, 128, 'value', '0-30 mg/L hardness', 10),
       ($1, 'hardness_mg_l_caco3', 30, 75, false, true, 218, 'value', '31-75 mg/L hardness', 20),
       ($1, 'hardness_mg_l_caco3', 75, 180, false, true, 309, 'value', '76-180 mg/L hardness', 30),
       ($1, 'hardness_mg_l_caco3', 180, 250, false, true, 429, 'value', '181-250 mg/L hardness', 40),
       ($1, 'hardness_mg_l_caco3', 250, NULL, false, false, NULL, 'site_specific', '>250 mg/L hardness requires site-specific assessment', 50)",
    params = list(sulfate_rule)
  )

  message("  adding pH range and dissolved oxygen minimum")
  ph_guideline <- insert_guideline(
    "EPA-PH-FW-RANGE-1986",
    "EPA freshwater aquatic life pH range",
    epa_id, epa_series, ph_param, matrix_liquid, "range",
    epa_source, "EPA national recommended aquatic life criteria table",
    "chronic", "ambient", "Freshwater aquatic life",
    "United States", source_section = "pH 6.5-9.0", media_id = media_surface
  )
  constant_rule(ph_guideline, "lower", 6.5, "Freshwater pH lower bound.", 10)
  constant_rule(ph_guideline, "upper", 9.0, "Freshwater pH upper bound.", 20)

  do_guideline <- insert_guideline(
    "BC-DO-FW-MIN-1997",
    "B.C. dissolved oxygen freshwater instantaneous minimum fixture",
    bc_env_id, bc_series, do_param, matrix_liquid, "gte",
    do_source, "B.C. dissolved oxygen overview report",
    "instantaneous minimum", "single result", "Freshwater aquatic life",
    "British Columbia", source_section = "minimum dissolved oxygen fixture",
    media_id = media_surface
  )
  constant_rule(do_guideline, "lower", 5, "Minimum dissolved oxygen fixture value in mg/L.", 10)

  message("  adding zinc database function")
  DBI::dbExecute(
    con,
    "CREATE OR REPLACE FUNCTION discrete.test_ccme_zinc_short_term(
       inputs JSONB
     )
     RETURNS NUMERIC
     LANGUAGE sql
     IMMUTABLE
     AS $function$
       WITH values AS (
         SELECT
           max((entry->>'value')::NUMERIC)
             FILTER (WHERE entry->>'input_code' = 'hardness_mg_l_caco3') AS hardness,
           max((entry->>'value')::NUMERIC)
             FILTER (WHERE entry->>'input_code' = 'doc_mg_l') AS doc
         FROM jsonb_array_elements(inputs) AS items(entry)
       )
       SELECT CASE
         WHEN hardness IS NULL OR doc IS NULL OR hardness <= 0 OR doc <= 0
           THEN NULL::NUMERIC
         ELSE exp(0.833 * ln(hardness) + 0.240 * ln(doc) + 0.526) / 1000
       END
       FROM values;
     $function$;"
  )
  DBI::dbExecute(
    con,
    "ALTER FUNCTION discrete.test_ccme_zinc_short_term(JSONB)
     OWNER TO admin;"
  )

  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_models (
       model_code, model_name, publisher_id, model_version, model_type,
       source_document_title, source_url, description
     )
     VALUES (
       'CCME_ZN_SHORT_TERM',
       'CCME dissolved zinc short-term benchmark equation',
       $1,
       'current CCME web value',
       'database_function',
       'CCME zinc water quality guideline page',
       $2,
       'Multi-input dissolved zinc benchmark equation using hardness and DOC.'
     )",
    params = list(ccme_id, zinc_source)
  )
  zinc_unit <- one_value(
    "SELECT public.get_parameter_unit_id($1, $2)",
    params = list(zinc_param, matrix_liquid),
    label = "zinc unit"
  )
  zinc_exposure_duration <- exposure_duration_id("short-term")
  zinc_averaging_period <- averaging_period_id("sample-specific")
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_model_outputs (
       model_code, output_code, output_name, comparison_operator_code,
       output_units, exposure_duration_id, averaging_period_id, note
     )
     VALUES (
       'CCME_ZN_SHORT_TERM', 'short_term',
       'Short-term dissolved zinc benchmark', 'lte', $1,
       $2, $3,
       'Function returns database zinc units; source equation is published in ug/L.'
     )",
    params = list(
      zinc_unit,
      zinc_exposure_duration,
      zinc_averaging_period
    )
  )
  add_zinc_model_input <- function(input_code, input_name, parameter_id,
                                   fraction_id, speciation_id,
                                   lower_bound, upper_bound,
                                   sort_order) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_model_inputs (
         model_code, input_code, input_name, parameter_id, matrix_state_id,
         sample_fraction_id, result_speciation_id, input_units,
         lower_calibrated_bound, upper_calibrated_bound,
         bounds_action, required, sort_order
       )
       VALUES (
         'CCME_ZN_SHORT_TERM', $1, $2, $3, $4, $5, $6,
         public.get_parameter_unit_id($3, $4), $7, $8,
         'reject', true, $9
       )",
      params = list(
        input_code, input_name, parameter_id, matrix_liquid, fraction_id,
        speciation_id, lower_bound, upper_bound, sort_order
      )
    )
  }
  add_zinc_model_input(
    "hardness_mg_l_caco3", "Hardness as CaCO3", hardness_param,
    fraction_total, spec_caco3, 13.8, 250.5, 10
  )
  add_zinc_model_input(
    "doc_mg_l", "Dissolved organic carbon", doc_param,
    fraction_dissolved, NA_integer_, 0.3, 17.3, 20
  )

  zinc_guideline <- insert_guideline(
    "CCME-ZN-FW-ST-DOC-HARDNESS",
    "CCME dissolved zinc short-term benchmark by hardness and DOC",
    ccme_id, ccme_series, zinc_param, matrix_liquid, "lte",
    zinc_source, "CCME zinc guideline page",
    "short-term", "sample-specific", "Freshwater aquatic life",
    "Canada", source_section = "short-term benchmark equation",
    fraction_id = fraction_dissolved, media_id = media_surface
  )
  zinc_rule <- one_value(
    "INSERT INTO discrete.guideline_value_rules (
       guideline_id, model_code, model_output_code, function_schema,
       function_name, bound_code, algorithm_code, missing_input_policy,
       rule_priority, note
     )
     VALUES (
       $1, 'CCME_ZN_SHORT_TERM', 'short_term', 'discrete',
       'test_ccme_zinc_short_term', 'upper', 'db_function',
       'no_value', 10,
       'CCME equation: exp(0.833 ln(hardness) + 0.240 ln(DOC) + 0.526), converted from ug/L to mg/L.'
     )
     RETURNING rule_id",
    params = list(zinc_guideline),
    label = "zinc rule"
  )
  for (input in list(
    list("hardness_mg_l_caco3", "Hardness as CaCO3", hardness_param, fraction_total, spec_caco3),
    list("doc_mg_l", "Dissolved organic carbon", doc_param, fraction_dissolved, NA_integer_)
  )) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_rule_inputs (
         rule_id, input_code, input_name, parameter_id, matrix_state_id,
         sample_fraction_id, result_speciation_id, result_type,
         aggregate_method, required
       )
       VALUES (
         $1, $2, $3, $4, $5, $6, $7, $8, 'single', true
       )",
      params = list(
        zinc_rule, input[[1]], input[[2]], input[[3]], matrix_liquid,
        input[[4]], input[[5]], result_type_lab
      )
    )
  }

  zinc_grid_guideline <- insert_guideline(
    "TEST-ZN-FW-ST-HARDNESS-DOC-GRID",
    "Test fixture dissolved zinc benchmark by hardness and DOC lookup grid",
    ccme_id, ccme_series, zinc_param, matrix_liquid, "lte",
    zinc_source, "Multidimensional lookup-grid fixture",
    "short-term", "sample-specific", "Freshwater aquatic life",
    "test fixture", source_section = "two-input lookup grid fixture",
    fraction_id = fraction_dissolved, media_id = media_surface
  )
  zinc_grid_rule <- one_value(
    "INSERT INTO discrete.guideline_value_rules (
       guideline_id, bound_code, algorithm_code, missing_input_policy,
       rule_priority, note
     )
     VALUES (
       $1, 'upper', 'lookup_grid', 'no_value', 10,
       'Two-dimensional lookup-grid fixture using hardness and DOC inputs.'
     )
     RETURNING rule_id",
    params = list(zinc_grid_guideline),
    label = "zinc lookup grid rule"
  )
  for (input in list(
    list("hardness_mg_l_caco3", "Hardness as CaCO3", hardness_param, fraction_total, spec_caco3),
    list("doc_mg_l", "Dissolved organic carbon", doc_param, fraction_dissolved, NA_integer_)
  )) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_rule_inputs (
         rule_id, input_code, input_name, parameter_id, matrix_state_id,
         sample_fraction_id, result_speciation_id, result_type,
         aggregate_method, required
       )
       VALUES (
         $1, $2, $3, $4, $5, $6, $7, $8, 'single', true
       )",
      params = list(
        zinc_grid_rule, input[[1]], input[[2]], input[[3]], matrix_liquid,
        input[[4]], input[[5]], result_type_lab
      )
    )
  }
  zinc_grid_table <- one_value(
    "INSERT INTO discrete.guideline_lookup_tables (
       rule_id, table_code, table_name, no_match_status, note
     )
     VALUES (
       $1, 'TEST_ZN_HARDNESS_DOC_GRID',
       'Test fixture hardness-DOC lookup grid',
       'no_matching_cell',
       'Synthetic two-dimensional grid used to exercise database-native multidimensional lookup evaluation.'
     )
     RETURNING lookup_table_id",
    params = list(zinc_grid_rule),
    label = "zinc lookup grid table"
  )
  zinc_grid_dimensions <- DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.guideline_lookup_dimensions (
       lookup_table_id, rule_id, input_code, sort_order
     )
     VALUES
       ($1, $2, 'hardness_mg_l_caco3', 10),
       ($1, $2, 'doc_mg_l', 20)
     RETURNING dimension_id, input_code",
    params = list(zinc_grid_table, zinc_grid_rule)
  )
  zinc_grid_dimension_id <- function(input_code) {
    zinc_grid_dimensions$dimension_id[
      match(input_code, zinc_grid_dimensions$input_code)
    ]
  }
  add_zinc_grid_cell <- function(label, h_low, h_high, doc_low, doc_high,
                                 output, sort_order) {
    cell_id <- one_value(
      "INSERT INTO discrete.guideline_lookup_cells (
         lookup_table_id, output_value, output_status, output_label, sort_order
       )
       VALUES ($1, $2, 'value', $3, $4)
       RETURNING cell_id",
      params = list(zinc_grid_table, output, label, sort_order),
      label = paste("zinc lookup grid cell", label)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_lookup_cell_ranges (
         cell_id, lookup_table_id, dimension_id, lower_bound, upper_bound,
         lower_inclusive, upper_inclusive
       )
       VALUES
         ($1, $2, $3, $4, $5, true, false),
         ($1, $2, $6, $7, $8, true, false)",
      params = list(
        cell_id, zinc_grid_table,
        zinc_grid_dimension_id("hardness_mg_l_caco3"),
        h_low, h_high,
        zinc_grid_dimension_id("doc_mg_l"),
        doc_low, doc_high
      )
    )
  }
  add_zinc_grid_cell("low hardness, low DOC", 0, 150, 0, 5, 0.10, 10)
  add_zinc_grid_cell("low hardness, high DOC", 0, 150, 5, 20, 0.14, 20)
  add_zinc_grid_cell("high hardness, low DOC", 150, 250, 0, 5, 0.08, 30)
  add_zinc_grid_cell("high hardness, high DOC", 150, 250, 5, 20, 0.12, 40)

  zinc_sql_guideline <- insert_guideline(
    "CCME-ZN-FW-ST-DOC-HARDNESS-SQLSCALAR",
    "CCME dissolved zinc short-term benchmark by SQL scalar fixture",
    ccme_id, ccme_series, zinc_param, matrix_liquid, "lte",
    zinc_source, "CCME zinc guideline page",
    "short-term", "sample-specific", "Freshwater aquatic life",
    "Canada", source_section = "short-term benchmark equation; SQL scalar fixture",
    fraction_id = fraction_dissolved, media_id = media_surface
  )
  zinc_formula_sql <- sprintf(
    "WITH vals AS (
       SELECT
         max(r.result) FILTER (
           WHERE r.parameter_id = %d
             AND r.sample_fraction_id = %d
             AND r.result_speciation_id = %d
         ) AS hardness,
         max(r.result) FILTER (
           WHERE r.parameter_id = %d
             AND r.sample_fraction_id = %d
         ) AS doc
       FROM discrete.results r
       WHERE r.sample_id = $1
     )
     SELECT CASE
       WHEN hardness IS NULL OR doc IS NULL OR hardness <= 0 OR doc <= 0
         THEN NULL::NUMERIC
       ELSE exp(0.833 * ln(hardness) + 0.240 * ln(doc) + 0.526) / 1000
     END
     FROM vals",
    hardness_param,
    fraction_total,
    spec_caco3,
    doc_param,
    fraction_dissolved
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_value_rules (
       guideline_id, bound_code, algorithm_code, formula_sql,
       missing_input_policy, rule_priority, note
     )
     VALUES (
       $1, 'upper', 'sql_scalar', $2, 'no_value', 20,
       'Legacy-style multi-input SQL scalar fixture for comparison with the governed db_function rule.'
     )",
    params = list(zinc_sql_guideline, zinc_formula_sql)
  )

  message("  adding turbidity narrative")
  turbidity_guideline <- insert_guideline(
    "BC-TURBIDITY-FW-BACKGROUND-NARRATIVE",
    "B.C. turbidity change-from-background guideline",
    bc_env_id, bc_series, turbidity_param, matrix_liquid, "narrative",
    turbidity_source, "Summary of Water Quality Guidelines: Aquatic Life, Wildlife and Agriculture",
    "varies", "duration and background dependent", "Freshwater aquatic life",
    "British Columbia", source_section = "turbidity change from background",
    media_id = media_surface
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_value_rules (
       guideline_id, bound_code, algorithm_code, rule_priority, note
     )
     VALUES (
       $1, NULL, 'narrative', 10,
       'Guideline is expressed as an allowed change from background that depends on flow/background condition and duration.'
     )",
    params = list(turbidity_guideline)
  )

  template <- DBI::dbGetQuery(
    con,
    "SELECT location_id, sub_location_id, collection_method, sample_type,
            sample_grade, sample_approval, owner, contributor, sampling_org
     FROM discrete.samples
     WHERE media_id = $1
     ORDER BY sample_id
     LIMIT 1",
    params = list(media_surface)
  )
  if (nrow(template) == 0) {
    stop("Need at least one existing surface-water sample for fixture metadata.")
  }

  message("  adding fixture samples and results")
  make_sample <- function(source_id, note, offset_hours) {
    one_value(
      "INSERT INTO discrete.samples (
         location_id, sub_location_id, media_id, z, datetime,
         target_datetime, collection_method, sample_type,
         sample_volume_ml, sample_grade, sample_approval,
         owner, contributor, sampling_org, share_with, import_source,
         no_update, note, import_source_id
       )
       VALUES (
         $1, $2, $3, 0,
         TIMESTAMPTZ '2024-08-01 12:00+00' + ($4 * INTERVAL '1 hour'),
         TIMESTAMPTZ '2024-08-01 12:00+00' + ($4 * INTERVAL '1 hour'),
         $5, $6, 1000, $7, $8, $9, $10, $11,
         ARRAY['public_reader'], 'patch_47_deep_guideline_fixture',
         false, $12, $13
       )
       RETURNING sample_id",
      params = list(
        template$location_id[[1]], template$sub_location_id[[1]],
        media_surface, offset_hours, template$collection_method[[1]],
        template$sample_type[[1]], template$sample_grade[[1]],
        template$sample_approval[[1]], template$owner[[1]],
        template$contributor[[1]], template$sampling_org[[1]],
        note, source_id
      ),
      label = source_id
    )
  }

  samples <- list(
    PASS = make_sample("WQG-DEEP-PASS", "Fixture sample expected to meet most scalar guidelines.", 1),
    FAIL = make_sample("WQG-DEEP-FAIL", "Fixture sample expected to fail several scalar guidelines.", 2),
    SITE = make_sample("WQG-DEEP-SITE", "Fixture sample with hardness above B.C. sulphate lookup range.", 3),
    MISS = make_sample("WQG-DEEP-MISS", "Fixture sample missing hardness for sulphate lookup.", 4),
    ZN_OK = make_sample("WQG-DEEP-ZN-OK", "Fixture sample meeting CCME zinc formula benchmark.", 5),
    ZN_BAD = make_sample("WQG-DEEP-ZN-BAD", "Fixture sample exceeding CCME zinc formula benchmark.", 6),
    ZN_OUT = make_sample("WQG-DEEP-ZN-OUT", "Fixture sample outside CCME zinc calibrated input domain.", 7)
  )

  base_time <- as.POSIXct("2024-08-02 12:00:00", tz = "UTC")

  add_common <- function(sample_id, chloride, sulfate, hardness, ph, do, turbidity, hour) {
    ts <- base_time + hour * 3600
    insert_result(sample_id, result_type_lab, chloride_param, fraction_total, chloride, result_value_actual, NA_integer_, matrix_liquid, ts)
    insert_result(sample_id, result_type_lab, sulfate_param, fraction_total, sulfate, result_value_actual, NA_integer_, matrix_liquid, ts)
    if (!is.na(hardness)) {
      insert_result(sample_id, result_type_lab, hardness_param, fraction_total, hardness, result_value_actual, spec_caco3, matrix_liquid, ts)
    }
    insert_result(sample_id, result_type_field, ph_param, NA_integer_, ph, result_value_actual, NA_integer_, matrix_liquid, ts)
    insert_result(sample_id, result_type_field, do_param, NA_integer_, do, result_value_actual, NA_integer_, matrix_liquid, ts)
    insert_result(sample_id, result_type_field, turbidity_param, NA_integer_, turbidity, result_value_actual, NA_integer_, matrix_liquid, ts)
  }

  add_common(samples$PASS, 100, 200, 74, 7.2, 8.0, 3, 1)
  add_common(samples$FAIL, 700, 450, 200, 9.3, 4.5, 15, 2)
  add_common(samples$SITE, 50, 300, 260, 7.0, 7.0, 4, 3)
  add_common(samples$MISS, 50, 50, NA_real_, 7.0, 7.0, 4, 4)

  add_zinc_sample <- function(sample_id, zinc, hardness, doc, hour) {
    ts <- base_time + hour * 3600
    insert_result(sample_id, result_type_lab, zinc_param, fraction_dissolved, zinc, result_value_actual, NA_integer_, matrix_liquid, ts)
    insert_result(sample_id, result_type_lab, hardness_param, fraction_total, hardness, result_value_actual, spec_caco3, matrix_liquid, ts)
    insert_result(sample_id, result_type_lab, doc_param, fraction_dissolved, doc, result_value_actual, NA_integer_, matrix_liquid, ts)
  }
  add_zinc_sample(samples$ZN_OK, 0.08, 100, 2, 5)
  add_zinc_sample(samples$ZN_BAD, 0.12, 100, 2, 6)
  add_zinc_sample(samples$ZN_OUT, 0.05, 260, 20, 7)

  message("  running smoke checks")
  result_for <- function(source_id, parameter_id) {
    one_value(
      "SELECT r.result_id
       FROM discrete.samples s
       JOIN discrete.results r
         ON r.sample_id = s.sample_id
       WHERE s.import_source_id = $1
         AND r.parameter_id = $2
       ORDER BY r.result_id
       LIMIT 1",
      params = list(source_id, parameter_id),
      label = paste(source_id, parameter_id)
    )
  }

  expect_status <- function(source_id, parameter_id, guideline_code,
                            expected_output, expected_comparison) {
    result_id <- as.integer(result_for(source_id, parameter_id))
    guideline_literal <- DBI::dbQuoteLiteral(con, guideline_code)
    row <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT lower_guideline_value, upper_guideline_value,
          output_status, comparison_status
       FROM discrete.applicable_guidelines_for_result(",
        result_id,
        ", CURRENT_DATE, TRUE)
       WHERE guideline_code = ",
        guideline_literal
      )
    )
    if (nrow(row) != 1) {
      stop("Expected one guideline row for ", source_id, " / ", guideline_code)
    }
    if (!identical(row$output_status[[1]], expected_output) ||
        !identical(row$comparison_status[[1]], expected_comparison)) {
      stop(
        "Unexpected status for ", source_id, " / ", guideline_code,
        ": output=", row$output_status[[1]],
        ", comparison=", row$comparison_status[[1]]
      )
    }
    invisible(row)
  }

  expect_status("WQG-DEEP-PASS", chloride_param, "CCME-CL-FW-LT-2011", "value", "meets")
  expect_status("WQG-DEEP-FAIL", chloride_param, "CCME-CL-FW-LT-2011", "value", "exceeds")
  expect_status("WQG-DEEP-PASS", sulfate_param, "BC-SO4-FW-LT-HARDNESS-2013", "value", "meets")
  expect_status("WQG-DEEP-FAIL", sulfate_param, "BC-SO4-FW-LT-HARDNESS-2013", "value", "exceeds")
  expect_status("WQG-DEEP-SITE", sulfate_param, "BC-SO4-FW-LT-HARDNESS-2013", "site_specific", "site_specific")
  expect_status("WQG-DEEP-MISS", sulfate_param, "BC-SO4-FW-LT-HARDNESS-2013", "missing_input", "missing_input")
  expect_status("WQG-DEEP-PASS", ph_param, "EPA-PH-FW-RANGE-1986", "value", "meets")
  expect_status("WQG-DEEP-FAIL", ph_param, "EPA-PH-FW-RANGE-1986", "value", "exceeds")
  expect_status("WQG-DEEP-PASS", do_param, "BC-DO-FW-MIN-1997", "value", "meets")
  expect_status("WQG-DEEP-FAIL", do_param, "BC-DO-FW-MIN-1997", "value", "below")
  expect_status("WQG-DEEP-PASS", turbidity_param, "BC-TURBIDITY-FW-BACKGROUND-NARRATIVE", "narrative", "narrative")
  expect_status("WQG-DEEP-ZN-OK", zinc_param, "CCME-ZN-FW-ST-DOC-HARDNESS", "value", "meets")
  expect_status("WQG-DEEP-ZN-BAD", zinc_param, "CCME-ZN-FW-ST-DOC-HARDNESS", "value", "exceeds")
  expect_status("WQG-DEEP-ZN-OUT", zinc_param, "CCME-ZN-FW-ST-DOC-HARDNESS", "outside_calibrated_range", "outside_calibrated_range")
  expect_status("WQG-DEEP-ZN-OK", zinc_param, "TEST-ZN-FW-ST-HARDNESS-DOC-GRID", "value", "meets")
  expect_status("WQG-DEEP-ZN-BAD", zinc_param, "TEST-ZN-FW-ST-HARDNESS-DOC-GRID", "value", "exceeds")
  expect_status("WQG-DEEP-ZN-OUT", zinc_param, "TEST-ZN-FW-ST-HARDNESS-DOC-GRID", "no_matching_cell", "no_matching_cell")
  expect_status("WQG-DEEP-ZN-OK", zinc_param, "CCME-ZN-FW-ST-DOC-HARDNESS-SQLSCALAR", "value", "meets")
  expect_status("WQG-DEEP-ZN-BAD", zinc_param, "CCME-ZN-FW-ST-DOC-HARDNESS-SQLSCALAR", "value", "exceeds")
  invisible(NULL)
  }),
  finally = {
    if (isTRUE(created_connection) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
))

message("Deep patch 47 guideline fixtures and smoke checks completed.")
