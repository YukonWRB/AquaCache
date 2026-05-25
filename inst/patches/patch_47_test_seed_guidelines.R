# Test seed for patch 47: real-world guideline examples and BC copper BLM
# comparison samples.
#
# This is intentionally separate from patch_47.R. It is for development and
# validation databases, not a production migration. It expects patch_47.R to
# have already been applied.

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
    value <- DBI::dbGetQuery(con, sql)
  } else {
    value <- DBI::dbGetQuery(con, sql, params = params)
  }
  if (nrow(value) == 0 || is.na(value[[1]][[1]])) {
    stop("Could not resolve ", label, call. = FALSE)
  }
  value[[1]][[1]]
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

  sql <- paste0(
    "SELECT parameter_id
     FROM public.parameters
     WHERE lower(param_name) ~ lower($1)
     ORDER BY ",
    order_sql,
    "
     LIMIT 1"
  )

  one_value(sql, params = params, label = label)
}

upsert_publisher <- function(code, name, url, note = NULL) {
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

upsert_series <- function(
  code,
  name,
  url,
  citation,
  note = NULL,
  publisher_id
) {
  DBI::dbExecute(
    con,
    "UPDATE discrete.guideline_series
     SET series_name = $1,
         series_url = $2,
         citation = $3,
         note = $4
     WHERE series_code = $5",
    params = list(name, url, citation, note, code)
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
       series_code, series_name, series_url, citation, note, publisher_id
     )
     VALUES ($1, $2, $3, $4, $5 , $6)
     RETURNING series_id",
    params = list(code, name, url, citation, note, publisher_id),
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

insert_result <- function(
  sample_id,
  result_type,
  parameter_id,
  sample_fraction_id,
  result_value,
  result_value_type,
  result_speciation_id,
  matrix_state_id,
  analysis_datetime
) {
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
      sample_id,
      result_type,
      parameter_id,
      sample_fraction_id,
      result_value,
      result_value_type,
      result_speciation_id,
      analysis_datetime,
      matrix_state_id
    )
  )
}

message("Seeding patch 47 guideline examples and BC copper BLM samples...")

invisible(tryCatch(
  DBI::dbWithTransaction(con, {
  required_tables <- DBI::dbGetQuery(
    con,
    "SELECT
       to_regclass('discrete.guideline_models') IS NOT NULL AS has_models,
       to_regprocedure(
         'discrete.guideline_collect_rule_inputs(integer, integer)'
       ) IS NOT NULL AS has_input_collector"
  )
  if (
    !isTRUE(required_tables$has_models[[1]]) ||
      !isTRUE(required_tables$has_input_collector[[1]])
  ) {
    stop("Apply inst/patches/patch_47.R before running this seed script.")
  }

  bc_env_id <- upsert_publisher(
    "BC_ENV",
    "British Columbia Ministry of Environment and Climate Change Strategy",
    "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines",
    "Seeded for patch 47 development testing."
  )
  bc_series_id <- upsert_series(
    "BC_AQ_LIFE",
    "B.C. approved aquatic life water quality guidelines",
    "https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-quality/water-quality-guidelines/approved-water-quality-guidelines",
    "British Columbia approved water quality guidelines for aquatic life.",
    "Seeded for patch 47 development testing.",
    publisher_id = bc_env_id
  )

  matrix_liquid <- one_value(
    sql = "SELECT matrix_state_id
     FROM public.matrix_states
     WHERE matrix_state_code = 'liquid'
     LIMIT 1",
    label = "liquid matrix state"
  )
  media_surface <- one_value(
    "SELECT media_id
     FROM public.media_types
     WHERE lower(media_type) = 'surface water'
     LIMIT 1",
    label = "surface water media"
  )
  result_type_field <- one_value(
    "SELECT result_type_id
     FROM discrete.result_types
     WHERE lower(result_type) = 'field'
     LIMIT 1",
    label = "field result type"
  )
  result_type_lab <- one_value(
    "SELECT result_type_id
     FROM discrete.result_types
     WHERE lower(result_type) = 'lab'
     LIMIT 1",
    label = "lab result type"
  )
  result_value_actual <- one_value(
    "SELECT result_value_type_id
     FROM discrete.result_value_types
     WHERE lower(result_value_type) = 'actual'
     LIMIT 1",
    label = "actual result value type"
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

  copper_param <- parameter_id(
    "(^|, )copper($|,)",
    "copper parameter",
    c("copper")
  )
  temp_param <- parameter_id(
    "^temperature, water$|water temperature",
    "water temperature parameter",
    c("temperature, water")
  )
  hardness_param <- parameter_id(
    "^hardness$",
    "hardness parameter",
    c("hardness")
  )
  doc_param <- parameter_id(
    "dissolved organic carbon|organic carbon|^doc$",
    "dissolved organic carbon parameter",
    c("dissolved organic carbon", "organic carbon")
  )
  ph_param <- parameter_id("^ph$", "pH parameter", c("pH"))

  optional_full_blm_inputs <- list(
    calcium = "calcium",
    magnesium = "magnesium",
    sodium = "sodium",
    potassium = "potassium",
    sulfate = "sulfate|sulphate",
    chloride = "chloride",
    alkalinity = "alkalinity"
  )
  optional_full_blm_parameters <- lapply(
    names(optional_full_blm_inputs),
    function(name) {
      tryCatch(
        parameter_id(
          optional_full_blm_inputs[[name]],
          paste(name, "parameter")
        ),
        error = function(e) NA_integer_
      )
    }
  )
  names(optional_full_blm_parameters) <- names(optional_full_blm_inputs)

  copper_unit <- one_value(
    "SELECT public.get_parameter_unit_id($1, $2)",
    params = list(copper_param, matrix_liquid),
    label = "copper unit"
  )
  copper_unit_name <- one_value(
    "SELECT public.get_parameter_unit_name($1, $2)",
    params = list(copper_param, matrix_liquid),
    label = "copper unit name"
  )
  guideline_scale_from_ug_l <- if (
    grepl("mg/l", tolower(copper_unit_name), fixed = TRUE)
  ) {
    0.001
  } else {
    1
  }

  fixture_sample_ids <- DBI::dbGetQuery(
    con,
    "SELECT sample_id
     FROM discrete.samples
     WHERE import_source = 'patch_47_bc_blm_fixture'"
  )
  if (nrow(fixture_sample_ids) > 0) {
    for (sample_id in fixture_sample_ids$sample_id) {
      DBI::dbExecute(
        con,
        "DELETE FROM discrete.results
         WHERE sample_id = $1",
        params = list(sample_id)
      )
      DBI::dbExecute(
        con,
        "DELETE FROM discrete.samples
         WHERE sample_id = $1",
        params = list(sample_id)
      )
    }
  }

  DBI::dbExecute(
    con,
    "DELETE FROM discrete.guideline_model_results
     WHERE model_code IN ('BC_BLM_CU_SIMPLIFIED', 'BC_BLM_CU_FULL')"
  )
  DBI::dbExecute(
    con,
    "DELETE FROM discrete.guidelines
     WHERE guideline_code IN (
       'BC-CU-BLM-CHRONIC-2019',
       'BC-CU-BLM-ACUTE-2019'
     )"
  )
  DBI::dbExecute(
    con,
    "DELETE FROM discrete.guideline_models
     WHERE model_code IN ('BC_BLM_CU_SIMPLIFIED', 'BC_BLM_CU_FULL')"
  )

  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_models (
       model_code, model_name, publisher_id, model_version, model_type,
       source_document_title, source_url, executable_url, description
     )
     VALUES
       (
         'BC_BLM_CU_SIMPLIFIED',
         'B.C. simplified copper biotic ligand model',
         $1,
         '2019',
         'external_software',
         'Copper Water Quality Guideline for the Protection of Aquatic Life: User''s Guide',
         'https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/copper/bc_copper_wqg_aquatic_life_users_guide.pdf',
         'https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/copper/bc_blm_setup.exe',
         'Simplified BC BLM path using temperature, hardness, DOC, and pH. The remaining major ions are estimated by the official software.'
       ),
       (
         'BC_BLM_CU_FULL',
         'B.C. full copper biotic ligand model',
         $1,
         '2019',
         'external_software',
         'BC BLM User''s Manual',
         'https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/copper/bc_blm_users_manual.pdf',
         'https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/copper/bc_blm_setup.exe',
         'Full BC BLM path for copper with full water chemistry.'
       )",
    params = list(bc_env_id)
  )

  add_model_input <- function(
    model_code,
    input_code,
    input_name,
    parameter_id,
    speciation_id = NA_integer_,
    sort_order = 100L,
    note = NA
  ) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_model_inputs (
         model_code, input_code, input_name, parameter_id, matrix_state_id,
         sample_fraction_id, result_speciation_id, input_units,
         required, sort_order, note
       )
       VALUES (
         $1, $2, $3, $4, $5, NULL, $6,
         public.get_parameter_unit_id($4, $5),
         true, $7, $8
       )",
      params = list(
        model_code,
        input_code,
        input_name,
        parameter_id,
        matrix_liquid,
        ifelse(is.na(speciation_id), NA_integer_, speciation_id),
        sort_order,
        note
      )
    )
  }

  simplified_inputs <- list(
    list(
      "temperature_c",
      "Water temperature",
      temp_param,
      NA_integer_,
      10L,
      result_type_field
    ),
    list(
      "hardness_mg_l_caco3",
      "Hardness as CaCO3",
      hardness_param,
      spec_caco3,
      20L,
      result_type_lab
    ),
    list(
      "doc_mg_l",
      "Dissolved organic carbon",
      doc_param,
      NA_integer_,
      30L,
      result_type_lab
    ),
    list("ph", "pH", ph_param, NA_integer_, 40L, result_type_field)
  )
  for (input in simplified_inputs) {
    add_model_input(
      "BC_BLM_CU_SIMPLIFIED",
      input[[1]],
      input[[2]],
      input[[3]],
      input[[4]],
      input[[5]]
    )
    add_model_input(
      "BC_BLM_CU_FULL",
      input[[1]],
      input[[2]],
      input[[3]],
      input[[4]],
      input[[5]],
      "Required by both the simplified and full BC BLM paths."
    )
  }

  full_order <- 50L
  for (name in names(optional_full_blm_parameters)) {
    parameter <- optional_full_blm_parameters[[name]]
    if (!is.na(parameter)) {
      add_model_input(
        "BC_BLM_CU_FULL",
        paste0(name, "_mg_l"),
        name,
        parameter,
        NA_integer_,
        full_order,
        "Additional full BC BLM input."
      )
      full_order <- full_order + 10L
    }
  }

  DBI::dbExecute(
    con,
    "INSERT INTO discrete.guideline_model_outputs (
       model_code, output_code, output_name, comparison_operator_code,
       output_units, exposure_duration_id, averaging_period_id, note
     )
     VALUES
       ('BC_BLM_CU_SIMPLIFIED', 'chronic', 'Long-term chronic dissolved copper WQG', 'lte', $1, $2, $3, 'Output value stored in the database copper parameter units.'),
       ('BC_BLM_CU_SIMPLIFIED', 'acute', 'Short-term acute dissolved copper WQG', 'lte', $1, $4, $5, 'Output value stored in the database copper parameter units.'),
       ('BC_BLM_CU_FULL', 'chronic', 'Long-term chronic dissolved copper WQG', 'lte', $1, $2, $3, 'Output value stored in the database copper parameter units.'),
       ('BC_BLM_CU_FULL', 'acute', 'Short-term acute dissolved copper WQG', 'lte', $1, $4, $5, 'Output value stored in the database copper parameter units.')",
    params = list(
      copper_unit,
      exposure_duration_id("long-term chronic"),
      averaging_period_id("sample-specific; see source document"),
      exposure_duration_id("short-term acute"),
      averaging_period_id("sample-specific")
    )
  )

  insert_guideline <- function(code, name, exposure, averaging, output_code) {
    jurisdiction_id_value <- jurisdiction_id("British Columbia")
    jurisdiction_level_id_value <- jurisdiction_level_id("provincial")
    protection_goal_id_value <- protection_goal_id("Freshwater aquatic life")
    exposure_duration_id_value <- exposure_duration_id(exposure)
    averaging_period_id_value <- averaging_period_id(averaging)
    guideline_id <- one_value(
      "INSERT INTO discrete.guidelines (
         guideline_code, 
         guideline_name, 
         publisher_id, 
         series_id,
         parameter_id, 
         matrix_state_id, 
         comparison_operator_code,
         jurisdiction_id, 
         jurisdiction_level_id, 
         protection_goal_id,
         exposure_duration_id, 
         averaging_period_id, 
         source_document_title,
         source_url, 
         source_table, 
         source_effective_date,
         source_retrieved_date, 
         valid_from, 
         review_status
       )
       VALUES (
         $1, $2, $3, $4, $5, $6, 'lte',
         $7, $8, $9, $10, $11,
         'Copper Water Quality Guideline for the Protection of Aquatic Life: User''s Guide',
         'https://www2.gov.bc.ca/assets/gov/environment/air-land-water/water/waterquality/water-quality-guidelines/approved-wqgs/copper/bc_copper_wqg_aquatic_life_users_guide.pdf',
         'Table 1', DATE '2019-03-01', CURRENT_DATE,
         DATE '2019-03-01', 'draft'
       )
       RETURNING guideline_id",
      params = list(
        code,
        name,
        bc_env_id,
        bc_series_id,
        copper_param,
        matrix_liquid,
        jurisdiction_id_value,
        jurisdiction_level_id_value,
        protection_goal_id_value,
        exposure_duration_id_value,
        averaging_period_id_value
      ),
      label = code
    )

    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guidelines_media_types (guideline_id, media_id)
       VALUES ($1, $2)",
      params = list(guideline_id, media_surface)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guidelines_fractions (guideline_id, fraction_id)
       VALUES ($1, $2)",
      params = list(guideline_id, fraction_dissolved)
    )

    rule_id <- one_value(
      "INSERT INTO discrete.guideline_value_rules (
         guideline_id, model_code, model_output_code, bound_code,
         algorithm_code, missing_input_policy, rule_priority,
         precision_note, note
       )
       VALUES (
         $1, 'BC_BLM_CU_SIMPLIFIED', $2, 'upper',
         'model_result_cache', 'no_value', 10,
         'Published comparison values are reported to one decimal place in ug/L.',
         'Uses stored official BC BLM comparison outputs for fixture samples; production use should store software outputs per sample or call a registered database model function.'
       )
       RETURNING rule_id",
      params = list(guideline_id, output_code),
      label = paste(code, "rule")
    )

    for (input in simplified_inputs) {
      DBI::dbExecute(
        con,
        "INSERT INTO discrete.guideline_rule_inputs (
           rule_id, input_code, input_name, parameter_id, matrix_state_id,
           sample_fraction_id, result_speciation_id, result_type,
           aggregate_method, allow_condition_value, required, note
         )
         VALUES (
           $1, $2, $3, $4, $5, NULL, $6, $7,
           'single', false, true,
           'BC BLM simplified water-chemistry input.'
         )",
        params = list(
          rule_id,
          input[[1]],
          input[[2]],
          input[[3]],
          matrix_liquid,
          ifelse(is.na(input[[4]]), NA_integer_, input[[4]]),
          input[[6]]
        )
      )
    }

    rule_id
  }

  chronic_rule <- insert_guideline(
    "BC-CU-BLM-CHRONIC-2019",
    "B.C. dissolved copper aquatic life BLM guideline - chronic",
    "long-term chronic",
    "5 samples in 30 days; see B.C. user guide",
    "chronic"
  )
  acute_rule <- insert_guideline(
    "BC-CU-BLM-ACUTE-2019",
    "B.C. dissolved copper aquatic life BLM guideline - acute",
    "short-term acute",
    "sample-specific",
    "acute"
  )

  # Published B.C. copper user-guide Table 1 examples. Source values are in
  # ug/L; values inserted into guideline_model_results are scaled to the
  # database copper parameter units when needed.
  scenarios <- data.frame(
    scenario = paste0("Scenario ", 1:8),
    temperature_c = rep(15, 8),
    hardness_mg_l_caco3 = c(30, 30, 30, 30, 150, 150, 150, 150),
    doc_mg_l = c(3, 3, 12.5, 12.5, 3, 3, 12.5, 12.5),
    ph = c(6.5, 8, 6.5, 8, 6.5, 8, 6.5, 8),
    chronic_ug_l = c(0.2, 1.2, 0.6, 5.1, 0.2, 2.0, 1.0, 8.1),
    acute_ug_l = c(0.9, 7.3, 3.8, 30.2, 1.6, 11.4, 6.8, 46.9),
    stringsAsFactors = FALSE
  )

  sample_template <- DBI::dbGetQuery(
    con,
    "SELECT location_id, sub_location_id, collection_method, sample_type,
            sample_grade, sample_approval, owner, contributor, sampling_org
     FROM discrete.samples
     WHERE media_id = $1
     ORDER BY sample_id
     LIMIT 1",
    params = list(media_surface)
  )
  if (nrow(sample_template) == 0) {
    stop(
      "The seed script needs at least one existing surface-water sample to copy reference metadata from."
    )
  }

  sample_ids <- integer(nrow(scenarios))

  for (i in seq_len(nrow(scenarios))) {
    scenario <- scenarios[i, ]
    sample_ids[[i]] <- one_value(
      "INSERT INTO discrete.samples (
         location_id, sub_location_id, media_id, z, datetime,
         target_datetime, collection_method, sample_type,
         sample_volume_ml, sample_grade, sample_approval,
         owner, contributor, sampling_org, share_with, import_source,
         no_update, note, import_source_id
       )
       VALUES (
         $1, $2, $3, 0,
         TIMESTAMPTZ '2024-07-01 12:00+00' + (($4 - 1) * INTERVAL '1 hour'),
         TIMESTAMPTZ '2024-07-01 12:00+00' + (($4 - 1) * INTERVAL '1 hour'),
         $5, $6, 1000, $7, $8,
         $9, $10, $11, ARRAY['public_reader'],
         'patch_47_bc_blm_fixture', false, $12, $13
       )
       RETURNING sample_id",
      params = list(
        sample_template$location_id[[1]],
        sample_template$sub_location_id[[1]],
        media_surface,
        i,
        sample_template$collection_method[[1]],
        sample_template$sample_type[[1]],
        sample_template$sample_grade[[1]],
        sample_template$sample_approval[[1]],
        sample_template$owner[[1]],
        sample_template$contributor[[1]],
        sample_template$sampling_org[[1]],
        paste0(
          "Patch 47 BC copper BLM comparison fixture: ",
          scenario$scenario
        ),
        sprintf("BC-BLM-CU-S%02d", i)
      ),
      label = paste("BC BLM sample", i)
    )

    analysis_datetime <- as.POSIXct(
      "2024-07-02 12:00:00",
      tz = "UTC"
    ) +
      (i - 1) * 3600

    insert_result(
      sample_ids[[i]],
      result_type_field,
      temp_param,
      fraction_dissolved, # Sample fraction _id
      scenario$temperature_c,
      result_value_actual,
      NA_integer_,
      matrix_liquid,
      analysis_datetime
    )
    insert_result(
      sample_ids[[i]],
      result_type_lab,
      hardness_param,
      fraction_dissolved,
      scenario$hardness_mg_l_caco3,
      result_value_actual,
      spec_caco3,
      matrix_liquid,
      analysis_datetime
    )
    insert_result(
      sample_ids[[i]],
      result_type_lab,
      doc_param,
      fraction_dissolved,
      scenario$doc_mg_l,
      result_value_actual,
      NA_integer_,
      matrix_liquid,
      analysis_datetime
    )
    insert_result(
      sample_ids[[i]],
      result_type_field,
      ph_param,
      NA_integer_,
      scenario$ph,
      result_value_actual,
      NA_integer_,
      matrix_liquid,
      analysis_datetime
    )
    insert_result(
      sample_ids[[i]],
      result_type_lab,
      copper_param,
      fraction_dissolved,
      scenario$chronic_ug_l * guideline_scale_from_ug_l * 0.8,
      result_value_actual,
      NA_integer_,
      matrix_liquid,
      analysis_datetime
    )

    chronic_inputs <- DBI::dbGetQuery(
      con,
      "SELECT *
       FROM discrete.guideline_collect_rule_inputs($1, $2)",
      params = list(chronic_rule, sample_ids[[i]])
    )
    acute_inputs <- DBI::dbGetQuery(
      con,
      "SELECT *
       FROM discrete.guideline_collect_rule_inputs($1, $2)",
      params = list(acute_rule, sample_ids[[i]])
    )

    if (
      chronic_inputs$output_status[[1]] != "value" ||
        acute_inputs$output_status[[1]] != "value"
    ) {
      stop("Could not resolve BC BLM fixture inputs for ", scenario$scenario)
    }

    DBI::dbExecute(
      con,
      "INSERT INTO discrete.guideline_model_results (
         model_code, model_output_code, sample_id, guideline_value,
         output_status, input_payload, input_hash, model_version,
         source_artifact, message, note
       )
       VALUES
         (
           'BC_BLM_CU_SIMPLIFIED', 'chronic', $1, $2, 'value',
           $3::jsonb, $4, '2019', 'B.C. copper user guide Table 1',
           NULL, $5
         ),
         (
           'BC_BLM_CU_SIMPLIFIED', 'acute', $1, $6, 'value',
           $7::jsonb, $8, '2019', 'B.C. copper user guide Table 1',
           NULL, $9
         )",
      params = list(
        sample_ids[[i]],
        scenario$chronic_ug_l * guideline_scale_from_ug_l,
        as.character(chronic_inputs$derivation_inputs[[1]]),
        chronic_inputs$input_hash[[1]],
        paste0("Source value ", scenario$chronic_ug_l, " ug/L."),
        scenario$acute_ug_l * guideline_scale_from_ug_l,
        as.character(acute_inputs$derivation_inputs[[1]]),
        acute_inputs$input_hash[[1]],
        paste0("Source value ", scenario$acute_ug_l, " ug/L.")
      )
    )
  }

  smoke <- DBI::dbGetQuery(
    con,
     "SELECT
       s.import_source_id,
       g.guideline_code,
       ag.upper_guideline_value AS guideline_value,
       ag.output_status,
       ag.comparison_status
     FROM discrete.samples s
     JOIN discrete.results r
       ON r.sample_id = s.sample_id
      AND r.parameter_id = $1
     JOIN discrete.applicable_guidelines_for_result(
       r.result_id, CURRENT_DATE, TRUE
     ) ag
       ON true
     JOIN discrete.guidelines g
       ON g.guideline_id = ag.guideline_id
     WHERE s.import_source = 'patch_47_bc_blm_fixture'
       AND g.guideline_code LIKE 'BC-CU-BLM-%'
     ORDER BY s.import_source_id, g.guideline_code",
    params = list(copper_param)
  )

  if (
    nrow(smoke) != nrow(scenarios) * 2L ||
      any(smoke$output_status != "value")
  ) {
    stop(
      "BC copper BLM fixture smoke test did not resolve all guideline values."
    )
  }
  }),
  finally = {
    if (isTRUE(created_connection) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
))

message(
  "Seeded BC copper BLM comparison samples. Guideline values are stored in ",
  copper_unit_name,
  "; multiply by ",
  1 / guideline_scale_from_ug_l,
  " to compare with published ug/L values when needed."
)
