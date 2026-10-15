# Runs the actual migration and source workflows only on the explicitly opted-in
# dev Patch 64 database. All sample, mapping and catalogue changes roll back;
# identity sequences can advance. Production is never a target of this test.
test_that("Patch 65 preserves valid imports, refines source results, and gates publication", {
  skip_if(
    Sys.getenv("AQUACACHE_PATCH65_DATABASE") != "aquacache",
    "dev integration opt-in required"
  )
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "aquacache",
    host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
    connect_timeout = 5,
    options = "-c statement_timeout=180000 -c lock_timeout=5000"
  )
  on.exit(DBI::dbDisconnect(con))
  on.exit(
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE),
    add = TRUE,
    after = FALSE
  )
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS db,inet_server_addr()::TEXT AS host,
    SESSION_USER AS user,(SELECT version FROM information.version_info WHERE item='Last patch number') AS patch"
  )
  stopifnot(
    identity$db == "aquacache",
    identity$host == "10.250.12.154/32",
    identity$user == "postgres",
    identity$patch == "64"
  )
  before <- DBI::dbGetQuery(
    con,
    "SELECT count(*)::INTEGER AS n,sum(result_id::NUMERIC) AS ids,sum(result) AS values FROM discrete.results"
  )
  legacy <- DBI::dbGetQuery(
    con,
    "SELECT r.result_id,r.sample_id FROM discrete.results r
    JOIN discrete.samples s USING(sample_id) JOIN public.parameters p USING(parameter_id)
    WHERE s.location_id=24 AND s.source_adapter_function='downloadECCCwq' AND NOT s.no_source_update
      AND NOT r.no_source_update AND lower(p.param_name)='arsenic' AND r.result_speciation_id IS NULL
      AND r.sample_fraction_id IS NOT NULL ORDER BY s.datetime DESC LIMIT 1"
  )
  stopifnot(nrow(legacy) == 1L)
  lines <- readLines(
    system.file("patches", "patch_65.R", package = "AquaCache"),
    warn = FALSE
  )
  commit <- grep('DBI::dbExecute(con, "COMMIT")', lines, fixed = TRUE)
  begin <- grep('active <- dbTransBegin(con)', lines, fixed = TRUE)
  stopifnot(length(commit) == 1L, length(begin) == 1L)
  lines[commit] <- 'DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")'
  # Acquire the busy continuous parent before parameter DDL, avoiding the
  # observed dev ingestion lock-order inversion. This is test orchestration.
  lines <- append(
    lines,
    c(
      'DBI::dbExecute(con, "SET LOCAL lock_timeout=\'30s\'")',
      'DBI::dbExecute(con, "LOCK TABLE continuous.timeseries IN ACCESS EXCLUSIVE MODE")',
      'DBI::dbExecute(con, "SET LOCAL lock_timeout=\'5s\'")'
    ),
    after = begin
  )
  patch <- new.env(parent = asNamespace("AquaCache"))
  patch$con <- con
  eval(parse(text = lines), patch)

  timezone_step <- DBI::dbGetQuery(
    con,
    "SELECT step_id FROM continuous.timeseries_aggregation_method_steps s
    WHERE evaluation_timezone='UTC' AND (SELECT count(*) FROM continuous.timeseries_aggregation_method_steps other
      WHERE other.aggregation_method_id=s.aggregation_method_id)=1 ORDER BY step_id LIMIT 1"
  )$step_id
  DBI::dbExecute(con, "SAVEPOINT timezone_checks")
  DBI::dbExecute(
    con,
    "UPDATE continuous.timeseries_aggregation_method_steps SET evaluation_timezone='America/Whitehorse' WHERE step_id=$1",
    params = list(timezone_step)
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT evaluation_timezone FROM continuous.timeseries_aggregation_method_steps WHERE step_id=$1",
      params = list(timezone_step)
    )$evaluation_timezone,
    "America/Whitehorse"
  )
  expect_error(
    DBI::dbExecute(
      con,
      sprintf(
        "UPDATE continuous.timeseries_aggregation_method_steps SET evaluation_timezone='Invalid/Timezone' WHERE step_id=%s",
        as.character(timezone_step)
      ),
      immediate = TRUE
    ),
    "recognized PostgreSQL/IANA"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT timezone_checks",
    immediate = TRUE
  ))
  DBI::dbExecute(con, "RELEASE SAVEPOINT timezone_checks", immediate = TRUE)

  sample <- DBI::dbGetQuery(
    con,
    "SELECT location_id,sub_location_id,media_id,collection_method,sample_type,owner,contributor
    FROM discrete.samples WHERE location_id=24 AND source_adapter_function='downloadECCCwq' ORDER BY datetime DESC LIMIT 1"
  )
  sample$datetime <- as.POSIXct(
    format(Sys.time() - 120, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    tz = "UTC"
  )
  sample$source_adapter_function <- "downloadECCCwq"
  sample$external_sample_id <- paste0(
    "patch65_workflow_",
    as.integer(Sys.time())
  )
  parameters <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id,lower(param_name) AS name FROM public.parameters
    WHERE lower(param_name) IN ('calcium','magnesium','arsenic','alkalinity','hardness')"
  )
  id <- stats::setNames(parameters$parameter_id, parameters$name)
  fractions <- DBI::dbGetQuery(
    con,
    "SELECT sample_fraction_id,sample_fraction FROM discrete.sample_fractions"
  )
  dissolved <- fractions$sample_fraction_id[
    fractions$sample_fraction == "dissolved"
  ]
  total <- fractions$sample_fraction_id[fractions$sample_fraction == "total"]
  liquid <- DBI::dbGetQuery(
    con,
    "SELECT matrix_state_id FROM public.matrix_states WHERE matrix_state_code='liquid'"
  )$matrix_state_id
  bases <- DBI::dbGetQuery(
    con,
    "SELECT reporting_basis_code,reporting_basis_id FROM discrete.reporting_bases"
  )
  as_as <- bases$reporting_basis_id[bases$reporting_basis_code == "as_as"]
  as_caco3 <- bases$reporting_basis_id[bases$reporting_basis_code == "as_caco3"]
  all_arsenic <- DBI::dbGetQuery(
    con,
    "SELECT result_speciation_id FROM discrete.result_speciations WHERE result_speciation_code='arsenic_all_species'"
  )$result_speciation_id
  # A silent adapter fixture uses the actual registered signature, then runs the
  # real synchronization code and database guards. No remote provider is called.
  source_payload <- NULL
  adapter <- function(...) source_payload
  formals(adapter) <- formals(downloadECCCwq)
  synchronize <- function(payload, delete = FALSE) {
    source_payload <<- payload
    testthat::with_mocked_bindings(
      synchronize_discrete(
        con,
        sample_series_id = 89,
        start_datetime = payload[[1]]$sample$datetime,
        delete = delete
      ),
      downloadECCCwq = adapter,
      .package = "AquaCache"
    )
  }
  DBI::dbExecute(
    con,
    "UPDATE discrete.sample_series SET synch_from=NULL,synch_to=NULL,active=TRUE,sync_remote=TRUE WHERE sample_series_id=89"
  )

  # Real ECCC CSV parsing plus getNewDiscrete: a bad alkalinity mapping must not
  # discard calcium, and another entirely invalid source sample is not inserted.
  keys <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT ON(p.param_name) lower(p.param_name) AS name,
    m.source_match->>'input_param' AS variable,m.source_match->>'input_unit' AS unit
    FROM discrete.import_parameter_mappings m JOIN discrete.import_mapping_sets sets USING(import_mapping_set_id)
    JOIN discrete.import_sources s USING(import_source_id) JOIN public.parameters p USING(parameter_id)
    WHERE s.source_code='downloadECCCeq1' AND sets.status='published' AND m.active
      AND lower(p.param_name) IN ('alkalinity','calcium') ORDER BY p.param_name,m.priority,m.import_mapping_id"
  )
  stopifnot(nrow(keys) == 2L)
  csv <- data.table::data.table(
    SITE_NO = "YT08AA0010",
    DATE_TIME_HEURE = format(sample$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    FLAG_MARQUEUR = "<",
    VALUE_VALEUR = c(100, 20),
    VARIABLE = keys$variable,
    SAMPLE_ID = sample$external_sample_id,
    UNIT = keys$unit
  )
  only_bad <- csv[keys$name == "alkalinity"]
  only_bad[, `:=`(
    DATE_TIME_HEURE = format(
      sample$datetime + 1,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    SAMPLE_ID = paste0(sample$external_sample_id, "_invalid")
  )]
  csv <- rbind(csv, only_bad)
  data.table::setnames(
    csv,
    c("SAMPLE_ID", "UNIT"),
    c("SAMPLE_ID_\u00c9CHANTILLON", "UNIT_UNIT\u00c9")
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  data.table::fwrite(csv, path, quote = TRUE)
  DBI::dbExecute(
    con,
    "UPDATE discrete.sample_series SET synch_from=$1 WHERE sample_series_id=89",
    params = list(sample$datetime - 1)
  )
  DBI::dbExecute(
    con,
    "UPDATE discrete.sample_series_source_adapters SET source_fx_args=source_fx_args ||
    jsonb_build_object('file',$1::TEXT,'tz','UTC') WHERE sample_series_id=89 AND source_fx='downloadECCCwq' AND active",
    params = list(path)
  )
  expect_warning(
    imported <- getNewDiscrete(con, sample_series_id = 89),
    "skipped result row"
  )
  imported_sample <- DBI::dbGetQuery(
    con,
    "SELECT sample_id FROM discrete.samples WHERE external_sample_id=$1",
    params = list(sample$external_sample_id)
  )$sample_id
  expect_length(imported_sample, 1L)
  expect_equal(nrow(imported), 1L)
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT parameter_id FROM discrete.results WHERE sample_id=$1",
      params = list(imported_sample)
    )$parameter_id,
    unname(id[["calcium"]])
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n FROM discrete.samples WHERE external_sample_id=$1",
      params = list(only_bad$SAMPLE_ID)
    )$n,
    0L
  )

  # Existing unknown chemistry is enriched atomically in place, with its result
  # ID and measurement retained. Repeating synchronization is idempotent.
  DBI::dbExecute(
    con,
    "UPDATE discrete.sample_series SET synch_from=NULL WHERE sample_series_id=89"
  )
  old <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.results WHERE result_id=$1",
    params = list(legacy$result_id)
  )
  old_sample <- DBI::dbGetQuery(
    con,
    "SELECT location_id,sub_location_id,z,media_id,datetime,collection_method,sample_type,owner,contributor,
    external_sample_id,source_adapter_function FROM discrete.samples WHERE sample_id=$1",
    params = list(old$sample_id)
  )
  remote <- old[,
    setdiff(
      names(old),
      c(
        "result_id",
        "sample_id",
        "created",
        "created_by",
        "modified",
        "modified_by",
        "analysis_conditions_key"
      )
    ),
    drop = FALSE
  ]
  remote$reporting_basis_id <- as_as
  remote$result_speciation_id <- all_arsenic
  count_before <- DBI::dbGetQuery(
    con,
    "SELECT count(*)::INTEGER AS n FROM discrete.results WHERE sample_id=$1",
    params = list(old$sample_id)
  )$n
  synchronize(list(list(sample = old_sample, results = remote)))
  enriched <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.results WHERE result_id=$1",
    params = list(old$result_id)
  )
  expect_identical(enriched$result_id, old$result_id)
  expect_equal(enriched$result, old$result)
  expect_identical(enriched$reporting_basis_id, as_as)
  expect_identical(enriched$result_speciation_id, all_arsenic)
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n FROM discrete.results WHERE sample_id=$1",
      params = list(old$sample_id)
    )$n,
    count_before
  )
  synchronize(list(list(sample = old_sample, results = remote)))
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT * FROM discrete.results WHERE result_id=$1",
      params = list(old$result_id)
    ),
    enriched
  )

  # Named-list conditions work on both new and already stored source results.
  condition_sample <- sample
  condition_sample$datetime <- sample$datetime + 2
  condition_sample$external_sample_id <- paste0(
    sample$external_sample_id,
    "_conditions"
  )
  condition_result <- data.frame(
    parameter_id = id[["arsenic"]],
    result_type = 2L,
    matrix_state_id = liquid,
    sample_fraction_id = total,
    reporting_basis_id = as_as,
    result_speciation_id = all_arsenic,
    result = 1.25
  )
  condition_result$analysis_conditions <- I(list(list(
    reference_temperature_kelvin = 298.15
  )))
  synchronize(list(list(sample = condition_sample, results = condition_result)))
  saved <- DBI::dbGetQuery(
    con,
    "SELECT r.* FROM discrete.results r JOIN discrete.samples s USING(sample_id) WHERE s.external_sample_id=$1",
    params = list(condition_sample$external_sample_id)
  )
  expect_equal(nrow(saved), 1L)
  expect_identical(
    jsonlite::fromJSON(saved$analysis_conditions)$reference_temperature_kelvin,
    298.15
  )
  condition_result$note <- "updated through named-list conditions"
  synchronize(list(list(sample = condition_sample, results = condition_result)))
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT result_id,note FROM discrete.results WHERE result_id=$1",
      params = list(saved$result_id)
    )$note,
    condition_result$note
  )

  # Invalid remote measurements must not delete stored counterparts, even when
  # deletion is explicitly enabled. Valid measurements in the same sample update.
  partial <- rbind(
    condition_result,
    transform(
      condition_result,
      parameter_id = id[["alkalinity"]],
      result_speciation_id = NA_integer_,
      reporting_basis_id = as_caco3
    )
  )
  partial$result[[1]] <- 1.5
  stored_alkalinity <- data.frame(
    sample_id = saved$sample_id,
    parameter_id = id[["alkalinity"]],
    result_type = 2L,
    matrix_state_id = liquid,
    sample_fraction_id = total,
    reporting_basis_id = as_caco3,
    endpoint_mode = "gran",
    result = 13
  )
  dbAppendTableRLS(con, "discrete.results", stored_alkalinity)
  stored_alkalinity_id <- DBI::dbGetQuery(
    con,
    "SELECT result_id FROM discrete.results WHERE sample_id=$1 AND parameter_id=$2",
    params = list(saved$sample_id, id[["alkalinity"]])
  )$result_id
  expect_warning(
    synchronize(
      list(list(sample = condition_sample, results = partial)),
      delete = TRUE
    ),
    "skipped result row"
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id=$1",
      params = list(saved$result_id)
    )$result,
    1.5
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id=$1",
      params = list(stored_alkalinity_id)
    )$result,
    13
  )

  protected <- condition_result
  protected$result <- 9
  DBI::dbExecute(
    con,
    "UPDATE discrete.results SET no_source_update=TRUE WHERE result_id=$1",
    params = list(saved$result_id)
  )
  synchronize(list(list(sample = condition_sample, results = protected)))
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id=$1",
      params = list(saved$result_id)
    )$result,
    1.5
  )

  # Fraction constraints apply to observed hardness and Ca/Mg-derived hardness.
  hardness_sample <- sample
  hardness_sample$datetime <- sample$datetime + 3
  hardness_sample$external_sample_id <- paste0(
    sample$external_sample_id,
    "_hardness"
  )
  hardness_results <- data.frame(
    parameter_id = id[["hardness"]],
    result_type = 2L,
    matrix_state_id = liquid,
    sample_fraction_id = c(dissolved, total),
    reporting_basis_id = as_caco3,
    result = c(100, 500)
  )
  hardness_sample_id <- addNewDiscrete(con, hardness_sample, hardness_results)
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT criteria.get_sample_hardness($1,NULL::INTEGER[],$2) AS value",
      params = list(hardness_sample_id, total)
    )$value,
    500
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT criteria.get_sample_hardness($1,NULL::INTEGER[],$2) AS value",
      params = list(hardness_sample_id, dissolved)
    )$value,
    100
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT criteria.get_sample_hardness($1) AS value",
      params = list(hardness_sample_id)
    )$value,
    100
  )
  input <- DBI::dbGetQuery(
    con,
    "SELECT input_id FROM criteria.guideline_rule_inputs WHERE input_source='hardness_helper' ORDER BY input_id LIMIT 1"
  )$input_id
  DBI::dbExecute(
    con,
    "UPDATE criteria.guideline_rule_inputs SET sample_fraction_id=$1,lower_calibrated_bound=NULL,upper_calibrated_bound=NULL WHERE input_id=$2",
    params = list(total, input)
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT input_value FROM criteria.guideline_get_input_value($1,$2)",
      params = list(hardness_sample_id, input)
    )$input_value,
    500
  )
  unsupported <- fractions$sample_fraction_id[
    fractions$sample_fraction == "acid soluble"
  ]
  DBI::dbExecute(
    con,
    "UPDATE criteria.guideline_rule_inputs SET sample_fraction_id=$1 WHERE input_id=$2",
    params = list(unsupported, input)
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT status FROM criteria.guideline_get_input_value($1,$2)",
      params = list(hardness_sample_id, input)
    )$status,
    "incompatible_hardness_expression"
  )
  DBI::dbExecute(
    con,
    "DELETE FROM discrete.results WHERE sample_id=$1 AND sample_fraction_id=$2",
    params = list(hardness_sample_id, total)
  )
  expect_true(is.na(
    DBI::dbGetQuery(
      con,
      "SELECT criteria.get_sample_hardness($1,NULL::INTEGER[],$2) AS value",
      params = list(hardness_sample_id, total)
    )$value
  ))
  calcium_basis <- bases$reporting_basis_id[
    bases$reporting_basis_code == "as_ca"
  ]
  magnesium_basis <- bases$reporting_basis_id[
    bases$reporting_basis_code == "as_mg"
  ]
  components <- data.frame(
    sample_id = hardness_sample_id,
    parameter_id = rep(c(id[["calcium"]], id[["magnesium"]]), 2L),
    result_type = 2L,
    matrix_state_id = liquid,
    sample_fraction_id = rep(c(dissolved, total), each = 2L),
    reporting_basis_id = rep(c(calcium_basis, magnesium_basis), 2L),
    result = c(10, 1, 20, 2)
  )
  dbAppendTableRLS(con, "discrete.results", components)
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT criteria.get_sample_hardness($1,NULL::INTEGER[],$2) AS value",
      params = list(hardness_sample_id, total)
    )$value,
    2.497 * 20 + 4.118 * 2
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT criteria.get_sample_hardness($1,NULL::INTEGER[],$2) AS value",
      params = list(hardness_sample_id, dissolved)
    )$value,
    2.497 * 10 + 4.118 * 1
  )

  # Cloned incomplete rows are editable evidence in a draft. Neither the R
  # publication helper nor a direct SQL status update may publish that draft.
  scope <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT s.import_source_id,s.import_profile_id,s.import_mapping_set_id
    FROM discrete.import_mapping_sets s JOIN discrete.import_parameter_mappings m USING(import_mapping_set_id)
    WHERE s.status='published' AND m.active AND discrete.import_parameter_mapping_error(m) IS NOT NULL
    ORDER BY s.import_mapping_set_id LIMIT 1"
  )
  draft <- import_mapping_prepare_revision(
    con,
    scope$import_source_id,
    scope$import_profile_id
  )
  expect_error(
    import_mapping_publish_revision(con, draft),
    "Cannot publish incomplete"
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT status FROM discrete.import_mapping_sets WHERE import_mapping_set_id=$1",
      params = list(scope$import_mapping_set_id)
    )$status,
    "published"
  )
  DBI::dbExecute(con, "SAVEPOINT publication_rejection")
  expect_error(
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_mapping_sets SET status='published' WHERE import_mapping_set_id=$1",
      params = list(draft)
    ),
    "Cannot publish incomplete"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT publication_rejection",
    immediate = TRUE
  ))
  DBI::dbExecute(
    con,
    "RELEASE SAVEPOINT publication_rejection",
    immediate = TRUE
  )
  DBI::dbExecute(
    con,
    "UPDATE discrete.import_parameter_mappings m SET active=FALSE WHERE m.import_mapping_set_id=$1
    AND m.active AND discrete.import_parameter_mapping_error(m) IS NOT NULL",
    params = list(draft)
  )
  import_mapping_publish_revision(con, draft)
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT status FROM discrete.import_mapping_sets WHERE import_mapping_set_id=$1",
      params = list(draft)
    )$status,
    "published"
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT status FROM discrete.import_mapping_sets WHERE import_mapping_set_id=$1",
      params = list(scope$import_mapping_set_id)
    )$status,
    "retired"
  )

  unassigned <- import_mapping_prepare_revision(
    con,
    scope$import_source_id,
    scope$import_profile_id
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.import_parameter_mappings(import_mapping_set_id,source_match,parameter_id,result_type,active)
    VALUES($1,'{\"input_param\":\"unassigned-test\"}',NULL,2,TRUE)",
    params = list(unassigned)
  )
  expect_error(import_mapping_publish_revision(con, unassigned), "Parameter")
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT status FROM discrete.import_mapping_sets WHERE import_mapping_set_id=$1",
      params = list(draft)
    )$status,
    "published"
  )
  DBI::dbExecute(
    con,
    "UPDATE discrete.import_parameter_mappings SET active=FALSE WHERE import_mapping_set_id=$1 AND parameter_id IS NULL",
    params = list(unassigned)
  )
  import_mapping_publish_revision(con, unassigned)
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT status FROM discrete.import_mapping_sets WHERE import_mapping_set_id=$1",
      params = list(unassigned)
    )$status,
    "published"
  )

  DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
  DBI::dbExecute(con, "ROLLBACK")
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n,sum(result_id::NUMERIC) AS ids,sum(result) AS values FROM discrete.results"
    ),
    before
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT version FROM information.version_info WHERE item='Last patch number'"
    )$version,
    "64"
  )
})
