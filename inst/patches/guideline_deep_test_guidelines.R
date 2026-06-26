# Deep test seed for patch 50 guideline behaviours.
#
# This script is for development/test databases. It first runs the lightweight
# patch 50 smoke seed, then adds lower-bound, SQL-scalar, missing-input, and
# narrative examples against the simplified guideline model.

deep_created_connection <- FALSE

if (!exists("con", inherits = TRUE)) {
  suppressPackageStartupMessages({
    library(DBI)
    library(RPostgres)
  })

  readRenviron(file.path("C:", "Users", "gtdelapl", "Documents", ".Renviron"))
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("aquacacheTestName", Sys.getenv("aquacacheName")),
    host = Sys.getenv("aquacacheTestHost", Sys.getenv("aquacacheHost")),
    port = Sys.getenv("aquacacheTestPort", Sys.getenv("aquacachePort")),
    user = Sys.getenv("aquacacheTestUser", Sys.getenv("aquacacheAdminUser")),
    password = Sys.getenv("aquacacheTestPass", Sys.getenv("aquacacheAdminPass"))
  )
  deep_created_connection <- TRUE
}

this_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(this_file)) {
  this_file <- file.path("inst", "patches", "guideline_deep_test_guidelines.R")
}
seed_dir <- dirname(normalizePath(this_file, mustWork = FALSE))
source(file.path(seed_dir, "guideline_test_seed_guidelines.R"), local = FALSE)

message("Seeding deep patch 50 guideline examples...")

invisible(tryCatch(
  DBI::dbWithTransaction(con, {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
       to_regclass('criteria.guideline_value_rules') IS NOT NULL AS has_rules,
       to_regprocedure('criteria.evaluate_guideline_rule(integer, integer)')
         IS NOT NULL AS has_evaluator"
    )
    if (!isTRUE(required$has_rules[[1]]) || !isTRUE(required$has_evaluator[[1]])) {
      stop("Apply inst/patches/guideline_patch.R before running this seed script.")
    }

    sample_ids <- DBI::dbGetQuery(
      con,
      "SELECT sample_id
       FROM discrete.samples
       WHERE import_source = 'patch_50_deep_guideline_fixture'"
    )
    for (sample_id in sample_ids$sample_id) {
      DBI::dbExecute(con, "DELETE FROM discrete.results WHERE sample_id = $1", params = list(sample_id))
      DBI::dbExecute(con, "DELETE FROM discrete.samples WHERE sample_id = $1", params = list(sample_id))
    }
    DBI::dbExecute(
      con,
      "DELETE FROM criteria.guidelines
       WHERE guideline_code IN (
         'PATCH50-DO-MIN',
         'PATCH50-ZN-DOC-HARDNESS-SQL',
         'PATCH50-TURBIDITY-NARRATIVE'
       )"
    )

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
       WHERE lower(media_type) IN ('surface water', 'freshwater (surface)')
       LIMIT 1",
      label = "surface water media"
    )
    result_type_lab <- one_value(
      "SELECT result_type_id FROM discrete.result_types WHERE lower(result_type) = 'lab' LIMIT 1",
      label = "lab result type"
    )
    result_type_field <- one_value(
      "SELECT result_type_id FROM discrete.result_types WHERE lower(result_type) = 'field' LIMIT 1",
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

    do_param <- parameter_id("^oxygen, dissolved$", "dissolved oxygen", "oxygen, dissolved")
    turbidity_param <- parameter_id("^turbidity$", "turbidity", "turbidity")
    zinc_param <- parameter_id("^zinc$", "zinc", "zinc")
    hardness_param <- parameter_id("^hardness$", "hardness", "hardness")
    doc_param <- parameter_id(
      "^organic carbon$|^dissolved organic carbon$|^doc$",
      "dissolved organic carbon",
      c("dissolved organic carbon", "organic carbon")
    )

    ccme_id <- upsert_publisher(
      "CCME",
      "Canadian Council of Ministers of the Environment",
      "https://ccme.ca",
      "Seeded for patch 50 deep testing."
    )
    bc_id <- upsert_publisher(
      "BC_ENV",
      "British Columbia Ministry of Environment and Climate Change Strategy",
      "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines",
      "Seeded for patch 50 deep testing."
    )
    ccme_series <- upsert_series(
      "PATCH50_DEEP_CCME",
      "Patch 50 CCME deep fixtures",
      ccme_id,
      "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
      "Synthetic CCME-shaped fixtures for patch 50."
    )
    bc_series <- upsert_series(
      "PATCH50_DEEP_BC",
      "Patch 50 B.C. deep fixtures",
      bc_id,
      "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines/approved-water-quality-guidelines",
      "Synthetic B.C.-shaped fixtures for patch 50."
    )

    do_guideline <- insert_guideline(
      "PATCH50-DO-MIN",
      "Patch 50 dissolved oxygen lower-bound guideline",
      bc_id,
      bc_series,
      do_param,
      matrix_liquid,
      "gte",
      "British Columbia",
      "Freshwater aquatic life",
      "Short-term acute",
      "Single sample",
      "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines",
      "Patch 50 dissolved oxygen lower-bound fixture",
      NA_integer_,
      media_surface
    )
    constant_rule(do_guideline, "lower", 5)

    zinc_sql_guideline <- insert_guideline(
      "PATCH50-ZN-DOC-HARDNESS-SQL",
      "Patch 50 dissolved zinc SQL-scalar guideline",
      ccme_id,
      ccme_series,
      zinc_param,
      matrix_liquid,
      "lte",
      "Canada",
      "Freshwater aquatic life",
      "Short-term acute",
      "Single sample",
      "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
      "Patch 50 zinc SQL-scalar fixture",
      fraction_dissolved,
      media_surface
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
    zinc_sql_rule <- one_value(
      "INSERT INTO criteria.guideline_value_rules (
         guideline_id, bound_code, algorithm_code, formula_sql,
         missing_input_policy, rule_priority, note
       )
       VALUES (
         $1, 'upper', 'sql_scalar', $2, 'no_value', 10,
         'Synthetic multi-input SQL scalar fixture.'
       )
       RETURNING rule_id",
      params = list(zinc_sql_guideline, zinc_formula_sql),
      label = "zinc SQL scalar rule"
    )
    for (input in list(
      list("hardness_mg_l_caco3", "Hardness as CaCO3", hardness_param, fraction_total, spec_caco3),
      list("doc_mg_l", "Dissolved organic carbon", doc_param, fraction_dissolved, NA_integer_)
    )) {
      DBI::dbExecute(
        con,
        "INSERT INTO criteria.guideline_rule_inputs (
           rule_id, input_code, input_name, input_source, parameter_id,
           matrix_state_id, sample_fraction_id, result_speciation_id,
           result_type, aggregate_method, required, note
         )
         VALUES (
           $1, $2, $3, 'sample_result', $4, $5, $6, $7, $8,
           'single', true,
           'Dependency metadata for the SQL scalar fixture.'
         )",
        params = list(
          zinc_sql_rule,
          input[[1]],
          input[[2]],
          input[[3]],
          matrix_liquid,
          input[[4]],
          input[[5]],
          result_type_lab
        )
      )
    }

    turbidity_guideline <- insert_guideline(
      "PATCH50-TURBIDITY-NARRATIVE",
      "Patch 50 turbidity narrative guideline",
      bc_id,
      bc_series,
      turbidity_param,
      matrix_liquid,
      "narrative",
      "British Columbia",
      "Freshwater aquatic life",
      "Narrative",
      "See source document",
      "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines",
      "Patch 50 turbidity narrative fixture",
      NA_integer_,
      media_surface
    )
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_value_rules (
         guideline_id, bound_code, algorithm_code, rule_priority, note
       )
       VALUES (
         $1, NULL, 'narrative', 10,
         'Narrative fixture for allowed change from background.'
       )",
      params = list(turbidity_guideline)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_narrative_values (
         guideline_id, value_code, condition_label, max_change_value,
         max_change_percent, change_unit, background_lower_bound,
         background_upper_bound, background_unit, duration_label,
         flow_condition, sort_order, note
       )
       VALUES
         ($1, 'CLEAR_24H_8_NTU', 'Clear flows, 24 h maximum',
          8, NULL, 'NTU', NULL, NULL, 'NTU', '24 h', 'clear flows', 10,
          'Synthetic narrative fixture.'),
         ($1, 'TURBID_GT_50_10_PERCENT', 'Turbid flows, background >50 NTU',
          NULL, 10, NULL, 50, NULL, 'NTU', 'any time', 'turbid flows', 20,
          'Synthetic narrative fixture.')",
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
    if (!nrow(template)) {
      stop("Need at least one existing surface-water sample for fixture metadata.")
    }

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
           TIMESTAMPTZ '2024-10-01 12:00+00' + ($4 * INTERVAL '1 hour'),
           TIMESTAMPTZ '2024-10-01 12:00+00' + ($4 * INTERVAL '1 hour'),
           $5, $6, 1000, $7, $8, $9, $10, $11,
           ARRAY['public_reader'], 'patch_50_deep_guideline_fixture',
           false, $12, $13
         )
         RETURNING sample_id",
        params = list(
          template$location_id[[1]],
          template$sub_location_id[[1]],
          media_surface,
          offset_hours,
          template$collection_method[[1]],
          template$sample_type[[1]],
          template$sample_grade[[1]],
          template$sample_approval[[1]],
          template$owner[[1]],
          template$contributor[[1]],
          template$sampling_org[[1]],
          note,
          source_id
        ),
        label = source_id
      )
    }

    samples <- list(
      OK = make_sample("PATCH50-DEEP-OK", "Meets deep scalar guidelines.", 1),
      BAD = make_sample("PATCH50-DEEP-BAD", "Fails deep scalar guidelines.", 2),
      MISS = make_sample("PATCH50-DEEP-MISS", "Missing DOC for SQL scalar.", 3)
    )
    base_time <- as.POSIXct("2024-10-02 12:00:00", tz = "UTC")

    add_deep_results <- function(sample_id, do_value, zinc, hardness, doc, turbidity, hour) {
      ts <- base_time + hour * 3600
      insert_result(sample_id, result_type_field, do_param, NA_integer_, do_value, result_value_actual, NA_integer_, matrix_liquid, ts)
      insert_result(sample_id, result_type_lab, zinc_param, fraction_dissolved, zinc, result_value_actual, NA_integer_, matrix_liquid, ts)
      insert_result(sample_id, result_type_lab, hardness_param, fraction_total, hardness, result_value_actual, spec_caco3, matrix_liquid, ts)
      if (!is.na(doc)) {
        insert_result(sample_id, result_type_lab, doc_param, fraction_dissolved, doc, result_value_actual, NA_integer_, matrix_liquid, ts)
      }
      insert_result(sample_id, result_type_field, turbidity_param, NA_integer_, turbidity, result_value_actual, NA_integer_, matrix_liquid, ts)
    }

    add_deep_results(samples$OK, 8.0, 0.08, 100, 2, 3, 1)
    add_deep_results(samples$BAD, 4.2, 0.12, 100, 2, 15, 2)
    add_deep_results(samples$MISS, 8.0, 0.05, 100, NA_real_, 4, 3)

    expect_status <- function(source_id, parameter_id, guideline_code, output, comparison) {
      row <- DBI::dbGetQuery(
        con,
        "SELECT ag.output_status, ag.comparison_status
         FROM discrete.samples s
         JOIN discrete.results r
           ON r.sample_id = s.sample_id
         JOIN criteria.applicable_guidelines_for_result(
           r.result_id, CURRENT_DATE, TRUE
         ) ag
           ON true
         WHERE s.import_source_id = $1
           AND r.parameter_id = $2
           AND ag.guideline_code = $3",
        params = list(source_id, parameter_id, guideline_code)
      )
      if (
        nrow(row) != 1L ||
          !identical(row$output_status[[1]], output) ||
          !identical(row$comparison_status[[1]], comparison)
      ) {
        stop(
          "Unexpected status for ",
          source_id,
          " / ",
          guideline_code,
          call. = FALSE
        )
      }
    }

    expect_status("PATCH50-DEEP-OK", do_param, "PATCH50-DO-MIN", "value", "meets")
    expect_status("PATCH50-DEEP-BAD", do_param, "PATCH50-DO-MIN", "value", "below")
    expect_status("PATCH50-DEEP-OK", zinc_param, "PATCH50-ZN-DOC-HARDNESS-SQL", "value", "meets")
    expect_status("PATCH50-DEEP-BAD", zinc_param, "PATCH50-ZN-DOC-HARDNESS-SQL", "value", "exceeds")
    expect_status("PATCH50-DEEP-MISS", zinc_param, "PATCH50-ZN-DOC-HARDNESS-SQL", "no_value", "no_value")
    expect_status("PATCH50-DEEP-OK", turbidity_param, "PATCH50-TURBIDITY-NARRATIVE", "narrative", "narrative")
  }),
  finally = {
    if (isTRUE(deep_created_connection) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
))

message("Deep patch 50 guideline fixtures and smoke checks completed.")
