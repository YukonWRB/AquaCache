# Test seed for patch 50 guideline examples.
#
# This script is for development and validation databases, not production
# migration. It expects inst/patches/guideline_patch.R to have already been
# applied and uses the aquacacheTest* connection variables when run standalone.

created_connection <- FALSE

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
  created_connection <- TRUE
}

one_value <- function(sql, params = list(), label = "query") {
  value <- if (length(params)) {
    DBI::dbGetQuery(con, sql, params = params)
  } else {
    DBI::dbGetQuery(con, sql)
  }
  if (!nrow(value) || is.na(value[[1]][[1]])) {
    stop("Could not resolve ", label, call. = FALSE)
  }
  value[[1]][[1]]
}

parameter_id <- function(pattern, label, preferred = character()) {
  params <- list(pattern)
  if (length(preferred)) {
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
    "UPDATE criteria.guideline_publishers
     SET publisher_name = $2,
         publisher_url = $3,
         note = $4
     WHERE publisher_code = $1",
    params = list(code, name, url, note)
  )
  existing <- DBI::dbGetQuery(
    con,
    "SELECT publisher_id
     FROM criteria.guideline_publishers
     WHERE publisher_code = $1",
    params = list(code)
  )
  if (nrow(existing)) {
    return(existing$publisher_id[[1]])
  }
  one_value(
    "INSERT INTO criteria.guideline_publishers (
       publisher_code, publisher_name, publisher_url, note
     )
     VALUES ($1, $2, $3, $4)
     RETURNING publisher_id",
    params = list(code, name, url, note),
    label = paste("publisher", code)
  )
}

upsert_series <- function(code, name, publisher_id, url, citation) {
  DBI::dbExecute(
    con,
    "UPDATE criteria.guideline_series
     SET series_name = $2,
         publisher_id = $3,
         series_url = $4,
         citation = $5
     WHERE series_code = $1",
    params = list(code, name, publisher_id, url, citation)
  )
  existing <- DBI::dbGetQuery(
    con,
    "SELECT series_id
     FROM criteria.guideline_series
     WHERE series_code = $1",
    params = list(code)
  )
  if (nrow(existing)) {
    return(existing$series_id[[1]])
  }
  one_value(
    "INSERT INTO criteria.guideline_series (
       series_code, series_name, publisher_id, series_url, citation
     )
     VALUES ($1, $2, $3, $4, $5)
     RETURNING series_id",
    params = list(code, name, publisher_id, url, citation),
    label = paste("series", code)
  )
}

ref_id <- function(table, id_col, code_col, name_col, prefix, name) {
  existing <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s
       FROM %s
       WHERE lower(btrim(%s)) = lower(btrim($1))
       LIMIT 1",
      id_col,
      table,
      name_col
    ),
    params = list(name)
  )
  if (nrow(existing)) {
    return(existing[[id_col]][[1]])
  }
  one_value(
    sprintf(
      "INSERT INTO %s (%s, %s, sort_order)
       VALUES ($1, $2, 800)
       ON CONFLICT (%s) DO UPDATE
       SET %s = EXCLUDED.%s
       RETURNING %s",
      table,
      code_col,
      name_col,
      code_col,
      name_col,
      name_col,
      id_col
    ),
    params = list(
      paste0(
        prefix,
        "_",
        substr(gsub("[^A-Z0-9]+", "_", toupper(name)), 1, 40)
      ),
      name
    ),
    label = name
  )
}

jurisdiction_id <- function(name) {
  ref_id(
    "criteria.guideline_jurisdictions",
    "jurisdiction_id",
    "jurisdiction_code",
    "jurisdiction_name",
    "JUR",
    name
  )
}
protection_goal_id <- function(name) {
  ref_id(
    "criteria.guideline_protection_goals",
    "protection_goal_id",
    "protection_goal_code",
    "protection_goal_name",
    "GOAL",
    name
  )
}
exposure_duration_id <- function(name) {
  ref_id(
    "criteria.guideline_exposure_durations",
    "exposure_duration_id",
    "exposure_duration_code",
    "exposure_duration_name",
    "EXPOSURE",
    name
  )
}
averaging_period_id <- function(name) {
  ref_id(
    "criteria.guideline_averaging_periods",
    "averaging_period_id",
    "averaging_period_code",
    "averaging_period_name",
    "AVG",
    name
  )
}

insert_guideline <- function(
  code,
  name,
  publisher_id,
  series_id,
  parameter_id,
  matrix_state_id,
  comparison_operator_code,
  jurisdiction,
  protection_goal,
  exposure_duration,
  averaging_period,
  source_url,
  source_document_title,
  fraction_id = NA_integer_,
  media_id = NA_integer_
) {
  guideline_id <- one_value(
    "INSERT INTO criteria.guidelines (
       guideline_code, guideline_name, publisher_id, series_id,
       parameter_id, matrix_state_id, comparison_operator_code,
       jurisdiction_id, protection_goal_id, exposure_duration_id,
       averaging_period_id, source_document_title, source_url,
       source_retrieved_date, valid_from, review_status
     )
     VALUES (
       $1, $2, $3, $4, $5, $6, $7,
       $8, $9, $10, $11, $12, $13, CURRENT_DATE,
       DATE '1900-01-01', 'approved'
     )
     RETURNING guideline_id",
    params = list(
      code,
      name,
      publisher_id,
      series_id,
      parameter_id,
      matrix_state_id,
      comparison_operator_code,
      jurisdiction_id(jurisdiction),
      protection_goal_id(protection_goal),
      exposure_duration_id(exposure_duration),
      averaging_period_id(averaging_period),
      source_document_title,
      source_url
    ),
    label = code
  )

  if (!is.na(media_id)) {
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guidelines_media_types (guideline_id, media_id)
       VALUES ($1, $2)",
      params = list(guideline_id, media_id)
    )
  }
  if (!is.na(fraction_id)) {
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guidelines_fractions (guideline_id, fraction_id)
       VALUES ($1, $2)",
      params = list(guideline_id, fraction_id)
    )
  }
  guideline_id
}

constant_rule <- function(guideline_id, bound_code, value, priority = 100L) {
  DBI::dbExecute(
    con,
    "INSERT INTO criteria.guideline_value_rules (
       guideline_id, bound_code, algorithm_code, fixed_value, rule_priority
     )
     VALUES ($1, $2, 'constant', $3, $4)",
    params = list(guideline_id, bound_code, value, priority)
  )
}

insert_result <- function(
  sample_id,
  result_type,
  parameter_id,
  sample_fraction_id,
  result,
  result_value_type,
  result_speciation_id,
  matrix_state_id,
  analysis_datetime
) {
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.results (
       sample_id, result_type, parameter_id, sample_fraction_id,
       result, result_condition, result_condition_value, result_value_type,
       result_speciation_id, analysis_datetime, share_with, no_update,
       matrix_state_id
     )
     VALUES (
       $1, $2, $3, $4, $5, NULL, NULL, $6, $7, $8,
       ARRAY['public_reader'], false, $9
     )",
    params = list(
      sample_id,
      result_type,
      parameter_id,
      sample_fraction_id,
      result,
      result_value_type,
      result_speciation_id,
      analysis_datetime,
      matrix_state_id
    )
  )
}

message("Seeding patch 50 guideline smoke examples...")

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
       WHERE import_source = 'patch_50_guideline_smoke_fixture'"
    )
    for (sample_id in sample_ids$sample_id) {
      DBI::dbExecute(con, "DELETE FROM discrete.results WHERE sample_id = $1", params = list(sample_id))
      DBI::dbExecute(con, "DELETE FROM discrete.samples WHERE sample_id = $1", params = list(sample_id))
    }
    DBI::dbExecute(
      con,
      "DELETE FROM criteria.guidelines
       WHERE guideline_code IN (
         'PATCH50-CL-LT',
         'PATCH50-PH-RANGE',
         'PATCH50-ZN-HARDNESS-POWER',
         'PATCH50-ZN-HARDNESS-LOGLINEAR'
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

    chloride_param <- parameter_id("^chloride$", "chloride", "chloride")
    ph_param <- parameter_id("^ph$", "pH", "pH")
    zinc_param <- parameter_id("^zinc$", "zinc", "zinc")
    hardness_param <- parameter_id("^hardness$", "hardness", "hardness")

    ccme_id <- upsert_publisher(
      "CCME",
      "Canadian Council of Ministers of the Environment",
      "https://ccme.ca",
      "Seeded for patch 50 smoke testing."
    )
    test_series <- upsert_series(
      "PATCH50_SMOKE",
      "Patch 50 smoke guideline fixtures",
      ccme_id,
      "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
      "Synthetic fixtures for the simplified patch 50 guideline engine."
    )

    chloride_guideline <- insert_guideline(
      "PATCH50-CL-LT",
      "Patch 50 chloride fixed upper guideline",
      ccme_id,
      test_series,
      chloride_param,
      matrix_liquid,
      "lte",
      "Canada",
      "Freshwater aquatic life",
      "Long-term chronic",
      "Single sample",
      "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
      "Patch 50 smoke fixed value",
      fraction_total,
      media_surface
    )
    constant_rule(chloride_guideline, "upper", 120)

    ph_guideline <- insert_guideline(
      "PATCH50-PH-RANGE",
      "Patch 50 pH range guideline",
      ccme_id,
      test_series,
      ph_param,
      matrix_liquid,
      "range",
      "Canada",
      "Freshwater aquatic life",
      "Long-term chronic",
      "Single sample",
      "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
      "Patch 50 smoke range value",
      NA_integer_,
      media_surface
    )
    constant_rule(ph_guideline, "lower", 6.5, 10L)
    constant_rule(ph_guideline, "upper", 9.0, 20L)

    zinc_guideline <- insert_guideline(
      "PATCH50-ZN-HARDNESS-LOGLINEAR",
      "Patch 50 zinc hardness log-linear guideline",
      ccme_id,
      test_series,
      zinc_param,
      matrix_liquid,
      "lte",
      "Canada",
      "Freshwater aquatic life",
      "Short-term acute",
      "Single sample",
      "https://ccme.ca/en/current-activities/canadian-environmental-quality-guidelines",
      "Patch 50 smoke hardness equation",
      fraction_dissolved,
      media_surface
    )
    zinc_rule <- one_value(
      "INSERT INTO criteria.guideline_value_rules (
         guideline_id, bound_code, algorithm_code, rule_priority, note
       )
       VALUES ($1, 'upper', 'log_linear', 10, 'Synthetic hardness-based log-linear formula: exp(intercept + slope * ln(hardness)).')
       RETURNING rule_id",
      params = list(zinc_guideline),
      label = "zinc log-linear rule"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_rule_inputs (
         rule_id, input_code, input_name, input_source, parameter_id,
         matrix_state_id, sample_fraction_id, result_speciation_id,
         result_type, aggregate_method, required
       )
       VALUES (
         $1, 'hardness_mg_l_caco3', 'Hardness as CaCO3',
         'sample_result', $2, $3, $4, $5, $6, 'single', true
       )",
      params = list(
        zinc_rule,
        hardness_param,
        matrix_liquid,
        fraction_total,
        spec_caco3,
        result_type_lab
      )
    )
    DBI::dbExecute(
      con,
      "INSERT INTO criteria.guideline_rule_coefficients (
         rule_id, coefficient_name, coefficient_value
       )
       VALUES
         ($1, 'intercept', -6.907755278982137),
         ($1, 'slope', 0.5)",
      params = list(zinc_rule)
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
           TIMESTAMPTZ '2024-09-01 12:00+00' + ($4 * INTERVAL '1 hour'),
           TIMESTAMPTZ '2024-09-01 12:00+00' + ($4 * INTERVAL '1 hour'),
           $5, $6, 1000, $7, $8, $9, $10, $11,
           ARRAY['public_reader'], 'patch_50_guideline_smoke_fixture',
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

    sample_ok <- make_sample("PATCH50-SMOKE-OK", "Meets smoke guidelines.", 1)
    sample_bad <- make_sample("PATCH50-SMOKE-BAD", "Exceeds smoke guidelines.", 2)
    base_time <- as.POSIXct("2024-09-02 12:00:00", tz = "UTC")

    add_smoke_results <- function(sample_id, chloride, ph, zinc, hardness, hour) {
      ts <- base_time + hour * 3600
      insert_result(sample_id, result_type_lab, chloride_param, fraction_total, chloride, result_value_actual, NA_integer_, matrix_liquid, ts)
      insert_result(sample_id, result_type_field, ph_param, NA_integer_, ph, result_value_actual, NA_integer_, matrix_liquid, ts)
      insert_result(sample_id, result_type_lab, zinc_param, fraction_dissolved, zinc, result_value_actual, NA_integer_, matrix_liquid, ts)
      insert_result(sample_id, result_type_lab, hardness_param, fraction_total, hardness, result_value_actual, spec_caco3, matrix_liquid, ts)
    }

    add_smoke_results(sample_ok, 80, 7.2, 0.008, 100, 1)
    add_smoke_results(sample_bad, 200, 9.4, 0.02, 100, 2)

    expect_status <- function(source_id, parameter_id, guideline_code, expected) {
      row <- DBI::dbGetQuery(
        con,
        "SELECT ag.comparison_status
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
      if (nrow(row) != 1L || !identical(row$comparison_status[[1]], expected)) {
        stop(
          "Unexpected status for ",
          source_id,
          " / ",
          guideline_code,
          call. = FALSE
        )
      }
    }

    expect_status("PATCH50-SMOKE-OK", chloride_param, "PATCH50-CL-LT", "meets")
    expect_status("PATCH50-SMOKE-BAD", chloride_param, "PATCH50-CL-LT", "exceeds")
    expect_status("PATCH50-SMOKE-OK", ph_param, "PATCH50-PH-RANGE", "meets")
    expect_status("PATCH50-SMOKE-BAD", ph_param, "PATCH50-PH-RANGE", "exceeds")
    expect_status("PATCH50-SMOKE-OK", zinc_param, "PATCH50-ZN-HARDNESS-LOGLINEAR", "meets")
    expect_status("PATCH50-SMOKE-BAD", zinc_param, "PATCH50-ZN-HARDNESS-LOGLINEAR", "exceeds")
  }),
  finally = {
    if (isTRUE(created_connection) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
))

message("Patch 50 guideline smoke examples seeded and checked.")
