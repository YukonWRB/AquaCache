# Explicit opt-in for the reset development database. This executes the real
# patch, replaces only its final COMMIT with immediate constraint validation,
# exercises the resulting schema, and rolls everything back. Identity sequence
# increments may survive rollback. Never enable this against production.
test_that("Patch 65 preserves chemistry policy and enforces calculation graphs", {
  skip_if(
    Sys.getenv("AQUACACHE_PATCH65_DATABASE") != "aquacache",
    "reset dev Patch 64 integration opt-in required"
  )
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "aquacache",
    host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
    options = "-c statement_timeout=180000 -c lock_timeout=5000",
    connect_timeout = 5
  )
  on.exit(DBI::dbDisconnect(con))
  on.exit(
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE),
    add = TRUE,
    after = FALSE
  )
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS db, inet_server_addr()::TEXT AS host,
       (SELECT version FROM information.version_info WHERE item='Last patch number') AS patch"
  )
  stopifnot(
    identity$db == "aquacache",
    identity$host == "10.250.12.154/32",
    identity$patch == "64"
  )
  fractions_before <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.sample_fractions ORDER BY sample_fraction_id"
  )
  policy_before <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id, sample_fraction FROM public.parameters ORDER BY parameter_id"
  )
  result_before <- DBI::dbGetQuery(
    con,
    "SELECT count(*)::INTEGER AS n, sum(result_id::NUMERIC) AS ids, sum(result) AS values FROM discrete.results"
  )
  unfiltered <- DBI::dbGetQuery(
    con,
    "SELECT m.import_mapping_id, m.result_speciation_id FROM discrete.import_parameter_mappings m
     JOIN discrete.sample_fractions f USING (sample_fraction_id) WHERE lower(f.sample_fraction)='unfiltered'"
  )
  # Exercise legacy profile defaults even if this cluster does not currently
  # use those defaults. Fixture changes are inside the patch's transaction.
  profile_ids <- DBI::dbGetQuery(
    con,
    "SELECT import_profile_id FROM discrete.import_profiles ORDER BY import_profile_id LIMIT 2"
  )$import_profile_id
  lines <- readLines(
    system.file("patches", "patch_65.R", package = "AquaCache"),
    warn = FALSE
  )
  commit <- grep('DBI::dbExecute(con, "COMMIT")', lines, fixed = TRUE)
  stopifnot(length(commit) == 1L)
  lines[commit] <- 'DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")'
  if (length(profile_ids) == 2L) {
    unfiltered_id <- fractions_before$sample_fraction_id[
      fractions_before$sample_fraction == "unfiltered"
    ]
    free_id <- fractions_before$sample_fraction_id[
      fractions_before$sample_fraction == "free"
    ]
    stopifnot(length(unfiltered_id) == 1L, length(free_id) == 1L)
    begin <- grep("active <- dbTransBegin(con)", lines, fixed = TRUE)
    fixture <- sprintf(
      "DBI::dbExecute(con, \"UPDATE discrete.import_profiles SET defaults =
      (defaults - ARRAY['result_speciation_id','result_speciation','result_descriptor_id','result_descriptor']) ||
      jsonb_build_object('sample_fraction_id', CASE import_profile_id WHEN %s THEN %s ELSE %s END)
      WHERE import_profile_id IN (%s,%s)\")",
      profile_ids[[1]],
      unfiltered_id,
      free_id,
      profile_ids[[1]],
      profile_ids[[2]]
    )
    lines <- append(lines, fixture, after = begin)
  }
  begin <- grep("active <- dbTransBegin(con)", lines, fixed = TRUE)
  lines <- append(
    lines,
    c(
      'DBI::dbExecute(con, "SET LOCAL lock_timeout=\'30s\'")',
      'DBI::dbExecute(con, "LOCK TABLE continuous.timeseries IN ACCESS EXCLUSIVE MODE")',
      'DBI::dbExecute(con, "SET LOCAL lock_timeout=\'5s\'")'
    ),
    after = begin
  )
  patch_environment <- new.env(parent = asNamespace("AquaCache"))
  patch_environment$con <- con
  eval(parse(text = lines), envir = patch_environment)
  review_path <- Sys.getenv("AQUACACHE_PATCH65_REVIEW_PATH")
  if (nzchar(review_path)) {
    jsonlite::write_json(
      patch_environment$mapping_requirements_to_review,
      review_path,
      pretty = TRUE,
      auto_unbox = TRUE
    )
  }
  expect_true(all(
    DBI::dbGetQuery(
      con,
      "SELECT window_end_offset-window_start_offset = INTERVAL '24 hours' AS valid
     FROM continuous.timeseries_aggregation_method_steps step
     JOIN continuous.timeseries_aggregation_methods method USING(aggregation_method_id)
     WHERE method.source_name='Environment and Climate Change Canada'
       AND step.operation_type='window_aggregate' AND step.evaluation_interval=INTERVAL '1 day'"
    )$valid
  ))
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n, sum(result_id::NUMERIC) AS ids, sum(result) AS values FROM discrete.results"
    ),
    result_before
  )
  policy_after <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id, sample_fraction FROM public.parameters ORDER BY parameter_id"
  )
  expect_identical(
    policy_after$sample_fraction,
    policy_before$sample_fraction[match(
      policy_after$parameter_id,
      policy_before$parameter_id
    )]
  )
  expect_false(any(
    DBI::dbGetQuery(
      con,
      "SELECT sample_fraction FROM discrete.sample_fractions"
    )$sample_fraction %in%
      c(
        "unfiltered",
        "free",
        "weak acid dissociable",
        "strong acid dissociable",
        "inorganic"
      )
  ))
  expect_true(all(
    c("acid soluble", "extractable", "total recoverable") %in%
      DBI::dbGetQuery(
        con,
        "SELECT sample_fraction FROM discrete.sample_fractions"
      )$sample_fraction
  ))
  expect_false(any(
    c("acid soluble", "extractable", "total recoverable", "total") %in%
      DBI::dbGetQuery(
        con,
        "SELECT result_target FROM discrete.result_targets"
      )$result_target
  ))
  history <- DBI::dbGetQuery(
    con,
    "SELECT legacy_id, legacy_label FROM information.patch65_result_descriptor_crosswalk
     WHERE legacy_catalogue='sample_fractions' ORDER BY legacy_id"
  )
  expect_identical(history$legacy_id, fractions_before$sample_fraction_id)
  expect_identical(history$legacy_label, fractions_before$sample_fraction)
  expect_true(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n FROM information.patch65_result_descriptor_crosswalk
     WHERE legacy_catalogue='sample_fractions' AND legacy_label='unfiltered'
       AND sample_fraction_id IS NOT NULL AND result_target_id IS NULL"
    )$n ==
      1L
  )
  if (length(profile_ids) == 2L) {
    migrated_defaults <- DBI::dbGetQuery(
      con,
      "SELECT p.import_profile_id, f.sample_fraction, t.result_target
       FROM discrete.import_profiles p
       LEFT JOIN discrete.sample_fractions f ON f.sample_fraction_id=(p.defaults->>'sample_fraction_id')::INTEGER
       LEFT JOIN discrete.result_targets t ON t.result_target_id=(p.defaults->>'result_target_id')::INTEGER
       WHERE p.import_profile_id IN ($1,$2) ORDER BY p.import_profile_id",
      params = as.list(profile_ids)
    )
    expect_identical(
      migrated_defaults$sample_fraction,
      c("total", NA_character_)
    )
    expect_identical(migrated_defaults$result_target, c(NA_character_, "free"))
  }
  if (nrow(unfiltered)) {
    migrated <- DBI::dbGetQuery(
      con,
      "SELECT m.import_mapping_id,f.sample_fraction,m.result_target_id FROM discrete.import_parameter_mappings m
       JOIN discrete.sample_fractions f USING(sample_fraction_id)
       WHERE m.import_mapping_id IN (SELECT jsonb_array_elements_text($1::JSONB)::INTEGER) ORDER BY 1",
      params = list(jsonlite::toJSON(
        unfiltered$import_mapping_id,
        auto_unbox = FALSE
      ))
    )
    expect_identical(migrated$sample_fraction, rep("total", nrow(unfiltered)))
    # An unfiltered fraction alone supplies no analytical-target evidence.
    if (all(is.na(unfiltered$result_speciation_id))) {
      expect_true(all(is.na(migrated$result_target_id)))
    }
  }
  sql <- function(statement) DBI::dbExecute(con, statement, immediate = TRUE)
  rejected <- function(statement, message) {
    sql("SAVEPOINT schema_case")
    expect_error(
      {
        sql(statement)
        sql("SET CONSTRAINTS ALL IMMEDIATE")
      },
      message
    )
    sql("ROLLBACK TO SAVEPOINT schema_case")
  }
  rejected(
    "UPDATE information.patch65_result_descriptor_crosswalk SET legacy_label='changed'",
    "immutable"
  )
  rejected(
    "DELETE FROM information.patch65_result_descriptor_crosswalk",
    "immutable"
  )
  rejected(
    "TRUNCATE information.patch65_result_descriptor_crosswalk",
    "immutable"
  )

  # Independent requirements must hold without any advisory associations.
  parameter <- DBI::dbGetQuery(
    con,
    "INSERT INTO public.parameters (param_name,sample_fraction,reporting_basis,result_speciation,result_target,
       units_liquid,plot_default_y_orientation)
     SELECT 'Patch 65 integration chemistry',FALSE,TRUE,TRUE,TRUE,min(unit_id),'normal'
     FROM public.units WHERE lower(unit_name)='mg/l' RETURNING parameter_id"
  )$parameter_id
  ids <- DBI::dbGetQuery(
    con,
    "SELECT (SELECT min(sample_id) FROM discrete.samples) AS sample,
       (SELECT matrix_state_id FROM public.matrix_states WHERE matrix_state_code='liquid') AS matrix,
       (SELECT min(reporting_basis_id) FROM discrete.reporting_bases) AS basis,
       (SELECT min(result_speciation_id) FROM discrete.result_speciations) AS form,
       (SELECT min(result_target_id) FROM discrete.result_targets) AS target"
  )
  insert_result <- sprintf(
    "INSERT INTO discrete.results (sample_id,parameter_id,matrix_state_id,result_type,result,reporting_basis_id,result_speciation_id,result_target_id)
    VALUES (%s,%s,%s,2,17,%s,%s,%s)",
    ids$sample,
    parameter,
    ids$matrix,
    "%s",
    "%s",
    "%s"
  )
  rejected(
    sprintf(insert_result, "NULL", ids$form, ids$target),
    "Reporting basis is required"
  )
  rejected(
    sprintf(insert_result, ids$basis, "NULL", ids$target),
    "Chemical speciation is required"
  )
  rejected(
    sprintf(insert_result, ids$basis, ids$form, "NULL"),
    "Analytical target is required"
  )
  sql(sprintf(insert_result, ids$basis, ids$form, ids$target))
  sql(sprintf(
    "INSERT INTO public.parameter_reporting_basis_suggestions VALUES (%s,%s,1)",
    parameter,
    ids$basis
  ))
  sql(sprintf(
    "DELETE FROM public.parameter_reporting_basis_suggestions WHERE parameter_id=%s",
    parameter
  ))
  expect_identical(
    unname(as.list(DBI::dbGetQuery(
      con,
      "SELECT reporting_basis,result_speciation,result_target FROM public.parameters WHERE parameter_id=$1",
      params = list(parameter)
    ))),
    list(TRUE, TRUE, TRUE)
  )

  # Use a seeded mean series, but remove its recipes inside a savepoint so the
  # invalid graph tests cannot conflict with seeded applicability periods.
  sql("SAVEPOINT graph_fixtures")
  sql("SET CONSTRAINTS ALL DEFERRED")
  type <- DBI::dbGetQuery(
    con,
    "SELECT aggregation_type,aggregation_type_id FROM continuous.aggregation_types"
  )
  mean_id <- type$aggregation_type_id[match("mean", type$aggregation_type)]
  min_id <- type$aggregation_type_id[match("minimum", type$aggregation_type)]
  max_id <- type$aggregation_type_id[match("maximum", type$aggregation_type)]
  instant_id <- type$aggregation_type_id[match(
    "instantaneous",
    type$aggregation_type
  )]
  extrema_id <- type$aggregation_type_id[match(
    "(min+max)/2",
    type$aggregation_type
  )]
  series <- DBI::dbGetQuery(
    con,
    "SELECT min(timeseries_id) AS id FROM continuous.timeseries WHERE aggregation_type_id=$1",
    params = list(mean_id)
  )$id
  stopifnot(!is.na(series))
  sql(sprintf(
    "DELETE FROM continuous.timeseries_aggregation_methods WHERE timeseries_id=%s",
    series
  ))
  graph <- sprintf(
    "INSERT INTO continuous.timeseries_aggregation_methods (aggregation_method_id,timeseries_id) VALUES (910003,%s);
    INSERT INTO continuous.timeseries_aggregation_method_steps (step_id,aggregation_method_id,stage_number,operation_type,aggregation_type_id,window_start_offset,window_end_offset)
    VALUES (920005,910003,1,'window_aggregate',%s,'-1 hour','0 seconds'),(920006,910003,1,'window_aggregate',%s,'-1 hour','0 seconds'),
      (920007,910003,2,'arithmetic_mean',%s,NULL,NULL);
    INSERT INTO continuous.timeseries_aggregation_method_step_inputs (aggregation_method_id,step_id,input_number,input_step_id,component_weight)
    VALUES (910003,920007,1,920005,%s),(910003,920007,2,920006,1);",
    series,
    min_id,
    max_id,
    "%s",
    "%s"
  )
  rejected(sprintf(graph, instant_id, "1"), "aggregation type must describe")
  rejected(sprintf(graph, mean_id, "'NaN'"), "weight_positive")
  sql(sprintf(
    "UPDATE continuous.timeseries SET aggregation_type_id=%s WHERE timeseries_id=%s",
    extrema_id,
    series
  ))
  rejected(sprintf(graph, extrema_id, "2"), "equal weights")
  sql(sprintf(
    "UPDATE continuous.timeseries SET aggregation_type_id=%s WHERE timeseries_id=%s",
    mean_id,
    series
  ))
  sql(sprintf(graph, mean_id, "1"))
  sql("SET CONSTRAINTS ALL IMMEDIATE; SET CONSTRAINTS ALL DEFERRED")
  sql(
    "DELETE FROM continuous.timeseries_aggregation_methods WHERE aggregation_method_id=910003"
  )
  sql(sprintf(
    "INSERT INTO continuous.timeseries_aggregation_methods (aggregation_method_id,timeseries_id,applicable_from,applicable_until)
    VALUES (910004,%s,'-infinity','2000-01-01'),(910005,%s,'2000-01-01','infinity');
    INSERT INTO continuous.timeseries_aggregation_method_steps (step_id,aggregation_method_id,stage_number,aggregation_type_id,window_start_offset,window_end_offset)
    VALUES (920008,910004,1,%s,'-1 hour','0 seconds'),(920009,910005,1,%s,'-1 hour','0 seconds');
    SET CONSTRAINTS ALL IMMEDIATE; SET CONSTRAINTS ALL DEFERRED;",
    series,
    series,
    mean_id,
    mean_id
  ))
  rejected(
    "UPDATE continuous.timeseries_aggregation_method_steps SET aggregation_method_id=910005 WHERE step_id=920008;
    DELETE FROM continuous.timeseries_aggregation_method_steps WHERE step_id=920009",
    "at least one calculation step"
  )
  sql(
    "DELETE FROM continuous.timeseries_aggregation_methods WHERE aggregation_method_id=910004; SET CONSTRAINTS ALL IMMEDIATE"
  )
  sql("ROLLBACK TO SAVEPOINT graph_fixtures")
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n FROM continuous.timeseries_aggregation_method_steps
     WHERE operation_type='window_aggregate' AND window_start_offset >= window_end_offset"
    )$n,
    0L
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n FROM continuous.timeseries_aggregation_method_steps step
     WHERE stage_number>1 AND NOT EXISTS(SELECT 1 FROM continuous.timeseries_aggregation_method_step_inputs input
       WHERE input.aggregation_method_id=step.aggregation_method_id AND input.step_id=step.step_id)"
    )$n,
    0L
  )
  sql("ROLLBACK")
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT version FROM information.version_info WHERE item='Last patch number'"
    )$version,
    "64"
  )
  expect_false(
    DBI::dbGetQuery(
      con,
      "SELECT to_regclass('continuous.timeseries_aggregation_methods') IS NOT NULL AS exists"
    )$exists
  )
})
