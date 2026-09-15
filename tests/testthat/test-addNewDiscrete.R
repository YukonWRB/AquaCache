test_that("addNewDiscrete inserts a new sample and results", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  sample_template <- DBI::dbGetQuery(
    con,
    "SELECT s.*
     FROM discrete.samples s
     JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type
     WHERE st.requires_location
       AND NOT st.requires_sample_group
     LIMIT 1"
  )
  if (nrow(sample_template) == 0) {
    testthat::skip("No sample data available for addNewDiscrete test.")
  }
  results_template <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.results LIMIT 1"
  )
  if (nrow(results_template) == 0) {
    testthat::skip("No results data available for addNewDiscrete test.")
  }

  sample <- sample_template[1, , drop = FALSE]
  max_dt <- DBI::dbGetQuery(con, "SELECT MAX(datetime) FROM discrete.samples")[[
    1
  ]]
  if (is.na(max_dt)) {
    max_dt <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  }
  sample$datetime <- max_dt + lubridate::dhours(1)
  sample <- sample[, setdiff(names(sample), "sample_id"), drop = FALSE]
  sample$import_source <- NA_character_
  sample$import_source_id <- NA_character_

  results <- results_template[1, , drop = FALSE]
  results <- results[,
    setdiff(names(results), c("result_id", "sample_id")),
    drop = FALSE
  ]

  invalid_source <- sample
  invalid_source$import_source <- "testthat"
  expect_error(
    addNewDiscrete(con, invalid_source, results),
    "must either both be supplied or both be absent"
  )

  sample_id <- addNewDiscrete(con, sample, results)

  inserted_sample <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM discrete.samples WHERE sample_id = $1",
    params = list(sample_id)
  )[[1]]
  inserted_results <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM discrete.results WHERE sample_id = $1",
    params = list(sample_id)
  )[[1]]

  expect_equal(inserted_sample, 1)
  expect_equal(inserted_results, nrow(results))
})


test_that("addNewDiscrete maintains a canonical result aggregation", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK"), add = TRUE, after = FALSE)

  sample <- DBI::dbGetQuery(
    con,
    "SELECT s.*
     FROM discrete.samples s
     JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type
     WHERE st.requires_location
       AND NOT st.requires_sample_group
     LIMIT 1"
  )
  results <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.results WHERE result IS NOT NULL LIMIT 1"
  )
  qualifier <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id FROM public.qualifier_types LIMIT 1"
  )
  observer <- DBI::dbGetQuery(
    con,
    "SELECT observer_id FROM instruments.observers LIMIT 1"
  )
  if (!nrow(sample) || !nrow(results) || !nrow(qualifier)) {
    testthat::skip("Composite sample reference data are unavailable.")
  }

  sample$sample_id <- NULL
  sample$datetime <- as.POSIXct(Sys.time(), tz = "UTC") + 172800
  sample$import_source <- NA_character_
  sample$import_source_id <- NA_character_
  results$result_id <- NULL
  results$sample_id <- NULL
  grade_type_id <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM public.grade_types ORDER BY grade_type_id LIMIT 1"
  )$grade_type_id[[1L]]
  approval_type_id <- DBI::dbGetQuery(
    con,
    "SELECT approval_type_id
     FROM public.approval_types
     ORDER BY approval_type_id
     LIMIT 1"
  )$approval_type_id[[1L]]
  results$lab_report_no <- "TEST-COMPOSITE-REPORT"
  results$lab_sample_no <- "TEST-COMPOSITE-SAMPLE"
  results$grade_type_id <- grade_type_id
  results$approval_type_id <- approval_type_id
  component_values <- data.frame(
    result_row = 1L,
    observation_number = 1:3,
    result = c(8, 10, 100),
    included_in_aggregate = c(TRUE, TRUE, FALSE),
    note = c(NA, NA, "Poor quality core")
  )
  sample_id <- addNewDiscrete(
    con = con,
    sample = sample,
    results = results,
    sample_qualifiers = qualifier$qualifier_type_id,
    sample_observers = if (nrow(observer)) observer$observer_id else NULL,
    result_aggregations = data.frame(
      result_row = 1L,
      aggregation_type = "mean",
      expected_count = 3L
    ),
    result_components = component_values
  )
  DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")

  canonical <- DBI::dbGetQuery(
    con,
    "SELECT r.result_id, r.result, r.lab_report_no, r.lab_sample_no,
            r.grade_type_id, r.approval_type_id,
            rat.aggregation_type, ra.expected_count
     FROM discrete.results r
     JOIN discrete.result_aggregations ra USING (result_id)
     JOIN discrete.result_aggregation_types rat
       USING (result_aggregation_type_id)
     WHERE sample_id = $1",
    params = list(sample_id)
  )
  expect_equal(canonical$result[[1]], 9)
  expect_identical(canonical$lab_report_no[[1]], "TEST-COMPOSITE-REPORT")
  expect_identical(canonical$lab_sample_no[[1]], "TEST-COMPOSITE-SAMPLE")
  expect_equal(canonical$grade_type_id[[1]], grade_type_id)
  expect_equal(canonical$approval_type_id[[1]], approval_type_id)
  expect_identical(canonical$aggregation_type[[1]], "mean")
  expect_equal(canonical$expected_count[[1]], 3L)
  view_metadata <- DBI::dbGetQuery(
    con,
    "SELECT lab_report_no, lab_sample_no, result_grade_id,
            result_approval_id
     FROM discrete.results_metadata_en
     WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )
  expect_identical(view_metadata$lab_report_no[[1]], "TEST-COMPOSITE-REPORT")
  expect_identical(view_metadata$lab_sample_no[[1]], "TEST-COMPOSITE-SAMPLE")
  expect_equal(view_metadata$result_grade_id[[1]], grade_type_id)
  expect_equal(view_metadata$result_approval_id[[1]], approval_type_id)
  canonical_update_count <- DBI::dbGetQuery(
    con,
    "SELECT count(*)::integer AS n
     FROM audit.general_log
     WHERE schema_name = 'discrete'
       AND table_name = 'results'
       AND action = 'UPDATE'
       AND (new_data ->> 'result_id')::integer = $1",
    params = list(canonical$result_id[[1]])
  )$n[[1]]
  expect_equal(canonical_update_count, 1L)
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*) FROM discrete.result_components
     WHERE result_id = $1",
      params = list(canonical$result_id[[1]])
    )[[1]],
    3
  )
  DBI::dbExecute(
    con,
    "UPDATE discrete.result_aggregations
     SET expected_count = 4
     WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )
  shortfall <- DBI::dbGetQuery(
    con,
    "SELECT expected_count, component_count, missing_component_count,
            has_component_shortfall
     FROM discrete.result_aggregation_summary
     WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )
  expect_equal(shortfall$expected_count[[1]], 4L)
  expect_equal(shortfall$missing_component_count[[1]], 1L)
  expect_true(shortfall$has_component_shortfall[[1]])

  expected_by_type <- c(
    mean = 9,
    median = 9,
    min = 8,
    max = 10,
    sum = 18
  )
  for (aggregation_type in names(expected_by_type)) {
    DBI::dbExecute(
      con,
      "UPDATE discrete.result_aggregations ra
       SET result_aggregation_type_id = rat.result_aggregation_type_id
       FROM discrete.result_aggregation_types rat
       WHERE ra.result_id = $1
         AND rat.aggregation_type = $2",
      params = list(canonical$result_id[[1]], aggregation_type)
    )
    expect_equal(
      DBI::dbGetQuery(
        con,
        "SELECT result FROM discrete.results WHERE result_id = $1",
        params = list(canonical$result_id[[1]])
      )$result[[1]],
      expected_by_type[[aggregation_type]],
      info = aggregation_type
    )
  }

  DBI::dbExecute(con, "SAVEPOINT reject_uncalculable_aggregate")
  expect_error(
    DBI::dbExecute(
      con,
      "UPDATE discrete.result_components
       SET included_in_aggregate = FALSE,
           note = COALESCE(note, 'Excluded for test')
       WHERE result_id = $1",
      params = list(canonical$result_id[[1]])
    ),
    "must calculate to a non-NULL value"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT reject_uncalculable_aggregate"
  ))
  DBI::dbExecute(con, "RELEASE SAVEPOINT reject_uncalculable_aggregate")
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id = $1",
      params = list(canonical$result_id[[1]])
    )$result[[1]],
    18
  )

  parent_modified <- DBI::dbGetQuery(
    con,
    "SELECT modified FROM discrete.results WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )$modified[[1]]
  DBI::dbExecute(
    con,
    "UPDATE discrete.result_components
     SET note = COALESCE(note, 'Component note') || ' revised'
     WHERE result_id = $1 AND observation_number = 1",
    params = list(canonical$result_id[[1]])
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT modified FROM discrete.results WHERE result_id = $1",
      params = list(canonical$result_id[[1]])
    )$modified[[1]],
    parent_modified
  )

  DBI::dbExecute(
    con,
    "UPDATE discrete.result_components
     SET included_in_aggregate = TRUE,
         weight = CASE observation_number
           WHEN 1 THEN 1
           WHEN 2 THEN 3
           ELSE 1
         END
     WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )
  DBI::dbExecute(
    con,
    "UPDATE discrete.result_aggregations ra
     SET result_aggregation_type_id = rat.result_aggregation_type_id,
         calculation_arguments =
           '{\"multiplier\": 10, \"rounding_digits\": 0}'::jsonb
     FROM discrete.result_aggregation_types rat
     WHERE ra.result_id = $1
       AND rat.aggregation_type = 'weighted_mean'",
    params = list(canonical$result_id[[1]])
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id = $1",
      params = list(canonical$result_id[[1]])
    )$result[[1]],
    276
  )
})


test_that("result aggregation inputs normalize calculation arguments", {
  normalized <- normalize_discrete_result_aggregations(
    results = data.frame(
      parameter_id = 1L,
      result = NA_real_,
      result_type = 1L
    ),
    result_aggregations = data.frame(
      result_row = 1L,
      aggregation_type = "mean",
      expected_count = 2L,
      calculation_arguments = I(list(list(
        missing_values = "ignore",
        rounding_digits = 1L
      )))
    ),
    result_components = data.frame(
      result_row = 1L,
      observation_number = 1:2,
      result = c(8, 10)
    )
  )

  expect_identical(
    normalized$result_aggregations$calculation_arguments[[1]],
    "{\"missing_values\":\"ignore\",\"rounding_digits\":1}"
  )
  expect_true(all(is.na(normalized$results$result)))
  expect_equal(normalized$result_aggregations$expected_count, 2L)
  expect_error(
    normalize_discrete_result_aggregations(
      results = normalized$results,
      result_aggregations = data.frame(
        result_row = 1L,
        aggregation_type = "mean",
        expected_count = 0L
      ),
      result_components = data.frame(
        result_row = 1L,
        observation_number = 1L,
        result = 8
      )
    ),
    "expected_count must contain positive integers"
  )
  expect_error(
    normalize_discrete_result_aggregations(
      results = normalized$results,
      result_aggregations = data.frame(
        result_row = 1L,
        aggregation_type = "mean"
      ),
      result_components = data.frame(
        result_row = 1L,
        observation_number = 1L,
        result = NA_real_,
        result_condition = 1L
      )
    ),
    "Conditions 1 and 2 require result_condition_value"
  )
  expect_error(
    normalize_discrete_result_aggregations(
      results = normalized$results,
      result_aggregations = data.frame(
        result_row = 1L,
        aggregation_type = "mean"
      ),
      result_components = data.frame(
        result_row = 1L,
        observation_number = 1L,
        result = 8,
        included_in_aggregate = FALSE
      )
    ),
    "needs a nonblank note"
  )
})


test_that("result aggregation inputs accept data.table objects", {
  normalized <- normalize_discrete_result_aggregations(
    results = data.table::data.table(
      parameter_id = 1L,
      result = 9,
      result_type = 1L
    ),
    result_aggregations = data.table::data.table(
      result_row = 1L,
      aggregation_type = "mean"
    ),
    result_components = data.table::data.table(
      result_row = 1L,
      observation_number = 1:2,
      result = c(8, 10)
    )
  )

  expect_s3_class(normalized$result_aggregations, "data.frame")
  expect_s3_class(normalized$result_components, "data.frame")
  expect_equal(normalized$result_aggregations$result_row, 1L)
  expect_equal(normalized$result_components$result, c(8, 10))
  expect_true(is.na(normalized$results$result[[1]]))
})


test_that("result aggregation constraints fail at the responsible statement", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK"), add = TRUE, after = FALSE)
  direct_result <- DBI::dbGetQuery(
    con,
    "SELECT r.result_id
     FROM discrete.results r
     LEFT JOIN discrete.result_aggregations ra USING (result_id)
     WHERE r.result IS NOT NULL AND ra.result_id IS NULL
     LIMIT 1"
  )$result_id[[1]]
  mean_type <- DBI::dbGetQuery(
    con,
    "SELECT result_aggregation_type_id
     FROM discrete.result_aggregation_types
     WHERE aggregation_type = 'mean'"
  )$result_aggregation_type_id[[1]]

  trigger_timing <- DBI::dbGetQuery(
    con,
    "SELECT tgdeferrable, tginitdeferred
     FROM pg_trigger
     WHERE tgname IN (
       'validate_result_aggregation_result_trigger',
       'validate_result_aggregation_config_trigger',
       'validate_result_aggregation_components_trigger'
     )"
  )
  expect_equal(nrow(trigger_timing), 3L)
  expect_true(all(trigger_timing$tgdeferrable))
  expect_false(any(trigger_timing$tginitdeferred))

  DBI::dbExecute(con, "SAVEPOINT reject_empty_aggregation")
  expect_error(
    suppressWarnings(DBI::dbExecute(
      con,
      "INSERT INTO discrete.result_aggregations (
         result_id, result_aggregation_type_id
       ) VALUES ($1, $2)",
      params = list(direct_result, mean_type)
    )),
    "must have at least one result component"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT reject_empty_aggregation"
  ))
  DBI::dbExecute(con, "RELEASE SAVEPOINT reject_empty_aggregation")

  DBI::dbExecute(con, "SAVEPOINT reject_unexplained_null")
  expect_error(
    suppressWarnings(DBI::dbExecute(
      con,
      "UPDATE discrete.results
       SET result = NULL, result_condition = NULL
       WHERE result_id = $1",
      params = list(direct_result)
    )),
    "must have exactly one of result or result_condition"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT reject_unexplained_null"
  ))
  DBI::dbExecute(con, "RELEASE SAVEPOINT reject_unexplained_null")
})


test_that("discrete visibility inherits from location through composite results", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  forced_tables <- DBI::dbGetQuery(
    con,
    "SELECT relation.relname
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
       AND relation.relforcerowsecurity"
  )$relname
  expect_setequal(
    forced_tables,
    c(
      "samples",
      "results",
      "sample_documents",
      "sample_groups",
      "sample_group_members",
      "sample_qualifiers",
      "sample_observers",
      "result_aggregations",
      "result_components"
    )
  )

  required_rls_tables <- c(
    "public.locations",
    "discrete.samples",
    "discrete.results",
    "discrete.result_aggregations",
    "discrete.result_components"
  )
  role_can_select <- function(role_name) {
    vapply(
      required_rls_tables,
      function(table_name) {
        DBI::dbGetQuery(
          con,
          "SELECT has_table_privilege($1, $2, 'SELECT') AS allowed",
          params = list(role_name, table_name)
        )$allowed[[1]]
      },
      logical(1)
    )
  }
  if (!all(role_can_select("public_reader"))) {
    testthat::skip("The RLS test role lacks required SELECT grants.")
  }

  target <- DBI::dbGetQuery(
    con,
    "SELECT
       result.result_id,
       result.result,
       result.sample_id,
       sample.location_id
     FROM discrete.results result
     JOIN discrete.samples sample USING (sample_id)
     LEFT JOIN discrete.result_aggregations aggregation USING (result_id)
     WHERE result.result IS NOT NULL
       AND sample.location_id IS NOT NULL
       AND aggregation.result_id IS NULL
     LIMIT 1"
  )
  if (!nrow(target)) {
    testthat::skip("No located direct result is available for the RLS test.")
  }

  dbTransBegin(con)
  on.exit(
    {
      suppressWarnings(try(DBI::dbExecute(con, "RESET ROLE"), silent = TRUE))
      suppressWarnings(try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE))
    },
    add = TRUE,
    after = FALSE
  )

  visibility_role <- paste0(
    "patch60_rls_test_",
    DBI::dbGetQuery(con, "SELECT pg_backend_pid()")[[1]]
  )
  quoted_visibility_role <- DBI::dbQuoteIdentifier(con, visibility_role)
  DBI::dbExecute(
    con,
    sprintf("CREATE ROLE %s NOLOGIN", quoted_visibility_role)
  )
  if (
    isTRUE(DBI::dbGetQuery(
      con,
      "SELECT pg_has_role('public_reader', $1, 'member') AS is_member",
      params = list(visibility_role)
    )$is_member[[1]])
  ) {
    testthat::skip("public_reader unexpectedly inherits the test sharing role.")
  }
  visibility_array <- paste0("{", visibility_role, "}")

  mean_type <- DBI::dbGetQuery(
    con,
    "SELECT result_aggregation_type_id
     FROM discrete.result_aggregation_types
     WHERE aggregation_type = 'mean'"
  )$result_aggregation_type_id[[1]]
  set_result_aggregation_constraints(con, "deferred")
  DBI::dbExecute(
    con,
    "UPDATE discrete.results
     SET result = NULL,
         result_condition = NULL,
         result_condition_value = NULL,
         share_with = $2::text[]
     WHERE result_id = $1",
    params = list(target$result_id[[1]], visibility_array)
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.result_aggregations (
       result_id, result_aggregation_type_id
     ) VALUES ($1, $2)",
    params = list(target$result_id[[1]], mean_type)
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.result_components (
       result_id, observation_number, result
     ) VALUES ($1, 1, $2)",
    params = list(target$result_id[[1]], target$result[[1]])
  )
  set_result_aggregation_constraints(con, "immediate")
  DBI::dbExecute(
    con,
    "UPDATE discrete.samples
     SET share_with = $2::text[]
     WHERE sample_id = $1",
    params = list(target$sample_id[[1]], visibility_array)
  )
  DBI::dbExecute(
    con,
    "UPDATE public.locations
     SET share_with = $2::text[]
     WHERE location_id = $1",
    params = list(target$location_id[[1]], visibility_array)
  )

  row_counts <- function() {
    c(
      location = DBI::dbGetQuery(
        con,
        "SELECT count(*) FROM public.locations WHERE location_id = $1",
        params = list(target$location_id[[1]])
      )[[1]],
      sample = DBI::dbGetQuery(
        con,
        "SELECT count(*) FROM discrete.samples WHERE sample_id = $1",
        params = list(target$sample_id[[1]])
      )[[1]],
      result = DBI::dbGetQuery(
        con,
        "SELECT count(*) FROM discrete.results WHERE result_id = $1",
        params = list(target$result_id[[1]])
      )[[1]],
      aggregation = DBI::dbGetQuery(
        con,
        "SELECT count(*) FROM discrete.result_aggregations WHERE result_id = $1",
        params = list(target$result_id[[1]])
      )[[1]],
      component = DBI::dbGetQuery(
        con,
        "SELECT count(*) FROM discrete.result_components WHERE result_id = $1",
        params = list(target$result_id[[1]])
      )[[1]]
    )
  }

  DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
  expect_equal(unname(row_counts()), rep(0, 5))
  DBI::dbExecute(con, "RESET ROLE")
})


test_that("synchronization replaces and removes result aggregation detail", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)
  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK"), add = TRUE, after = FALSE)

  sample <- DBI::dbGetQuery(
    con,
    "SELECT s.*
     FROM discrete.samples s
     JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type
     WHERE st.requires_location
       AND NOT st.requires_sample_group
     LIMIT 1"
  )
  results <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.results WHERE result IS NOT NULL LIMIT 1"
  )
  if (!nrow(sample) || !nrow(results)) {
    testthat::skip("Discrete sample reference data are unavailable.")
  }
  sample$sample_id <- NULL
  sample$datetime <- as.POSIXct(Sys.time(), tz = "UTC") + 259200
  sample$import_source <- NA_character_
  sample$import_source_id <- NA_character_
  results$result_id <- NULL
  results$sample_id <- NULL
  results$result <- 12
  sample_id <- addNewDiscrete(con, sample, results)
  result_id <- DBI::dbGetQuery(
    con,
    "SELECT result_id FROM discrete.results WHERE sample_id = $1",
    params = list(sample_id)
  )$result_id[[1]]

  normalized <- normalize_discrete_result_aggregations(
    results = results,
    result_aggregations = data.frame(
      result_row = 1L,
      aggregation_type = "mean"
    ),
    result_components = data.frame(
      result_row = 1L,
      observation_number = 1:2,
      result = c(8, 10)
    )
  )
  synchronize_discrete_sample_detail(
    con = con,
    sample_id = sample_id,
    remote_results = normalized$results,
    result_ids = result_id,
    pending_results = list(),
    sample_qualifiers = NULL,
    sample_observers = NULL,
    result_aggregations = normalized$result_aggregations,
    result_components = normalized$result_components
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id = $1",
      params = list(result_id)
    )$result[[1]],
    9
  )

  DBI::dbExecute(con, "SAVEPOINT reject_direct_aggregation_delete")
  expect_error(
    DBI::dbExecute(
      con,
      "DELETE FROM discrete.result_aggregations WHERE result_id = $1",
      params = list(result_id)
    ),
    "Cannot delete.*directly"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT reject_direct_aggregation_delete"
  ))
  DBI::dbExecute(con, "RELEASE SAVEPOINT reject_direct_aggregation_delete")

  DBI::dbExecute(con, "SAVEPOINT allow_parent_result_delete")
  expect_equal(
    DBI::dbExecute(
      con,
      "DELETE FROM discrete.results WHERE result_id = $1",
      params = list(result_id)
    ),
    1L
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)
     FROM discrete.result_aggregations
     WHERE result_id = $1",
      params = list(result_id)
    )[[1]],
    0
  )
  DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT allow_parent_result_delete")
  DBI::dbExecute(con, "RELEASE SAVEPOINT allow_parent_result_delete")

  current_role <- DBI::dbGetQuery(
    con,
    "SELECT roles.rolsuper
     FROM pg_roles roles
     WHERE roles.rolname = current_user"
  )
  if (isTRUE(current_role$rolsuper[[1]])) {
    guard_role <- paste0(
      "patch60_guard_test_",
      DBI::dbGetQuery(con, "SELECT pg_backend_pid()")[[1]]
    )
    quoted_guard_role <- DBI::dbQuoteIdentifier(con, guard_role)
    DBI::dbExecute(
      con,
      sprintf("CREATE ROLE %s NOLOGIN BYPASSRLS", quoted_guard_role)
    )
    DBI::dbExecute(
      con,
      sprintf("GRANT USAGE ON SCHEMA discrete TO %s", quoted_guard_role)
    )
    DBI::dbExecute(
      con,
      sprintf(
        paste0(
          "GRANT SELECT ON discrete.results, ",
          "discrete.result_aggregations TO %s"
        ),
        quoted_guard_role
      )
    )
    DBI::dbExecute(
      con,
      sprintf(
        "GRANT DELETE ON discrete.result_aggregations TO %s",
        quoted_guard_role
      )
    )
    DBI::dbExecute(
      con,
      sprintf(
        paste0(
          "GRANT EXECUTE ON FUNCTION ",
          "discrete.convert_result_aggregation_to_direct(",
          "integer,text,numeric,integer,numeric,text) TO %s"
        ),
        quoted_guard_role
      )
    )
    DBI::dbExecute(con, sprintf("SET LOCAL ROLE %s", quoted_guard_role))
    DBI::dbExecute(con, "SAVEPOINT reject_guard_guc_bypass")
    DBI::dbExecute(
      con,
      "SELECT set_config(
         'aquacache.allow_result_aggregation_delete', 'on', TRUE
       )"
    )
    expect_error(
      DBI::dbExecute(
        con,
        "DELETE FROM discrete.result_aggregations WHERE result_id = $1",
        params = list(result_id)
      ),
      "Cannot delete.*directly"
    )
    suppressWarnings(DBI::dbExecute(
      con,
      "ROLLBACK TO SAVEPOINT reject_guard_guc_bypass"
    ))
    DBI::dbExecute(con, "RELEASE SAVEPOINT reject_guard_guc_bypass")

    DBI::dbExecute(con, "SAVEPOINT reject_non_admin_conversion")
    expect_error(
      DBI::dbExecute(
        con,
        "SELECT discrete.convert_result_aggregation_to_direct(
           $1, 'preserve_calculated', NULL, NULL, NULL, $2
         )",
        params = list(result_id, "Non-admin conversion test")
      ),
      "may not convert result aggregations"
    )
    suppressWarnings(DBI::dbExecute(
      con,
      "ROLLBACK TO SAVEPOINT reject_non_admin_conversion"
    ))
    DBI::dbExecute(con, "RELEASE SAVEPOINT reject_non_admin_conversion")
    DBI::dbExecute(con, "RESET ROLE")
  }

  normalized_replacement <- normalize_discrete_result_aggregations(
    results = results,
    result_aggregations = data.frame(
      result_row = 1L,
      aggregation_type = "sum",
      expected_count = 2L,
      note = "Replacement aggregation"
    ),
    result_components = data.frame(
      result_row = 1L,
      observation_number = 1:2,
      result = c(7, 13)
    )
  )
  synchronize_discrete_sample_detail(
    con = con,
    sample_id = sample_id,
    remote_results = normalized_replacement$results,
    result_ids = result_id,
    pending_results = list(),
    sample_qualifiers = NULL,
    sample_observers = NULL,
    result_aggregations = normalized_replacement$result_aggregations,
    result_components = normalized_replacement$result_components
  )
  rebuilt <- DBI::dbGetQuery(
    con,
    "SELECT r.result, rat.aggregation_type, ra.expected_count, ra.note,
            count(rc.result_component_id)::integer AS component_count
     FROM discrete.results r
     JOIN discrete.result_aggregations ra USING (result_id)
     JOIN discrete.result_aggregation_types rat
       USING (result_aggregation_type_id)
     JOIN discrete.result_components rc USING (result_id)
     WHERE r.result_id = $1
     GROUP BY r.result, rat.aggregation_type, ra.expected_count, ra.note",
    params = list(result_id)
  )
  expect_equal(rebuilt$result[[1]], 20)
  expect_identical(rebuilt$aggregation_type[[1]], "sum")
  expect_equal(rebuilt$expected_count[[1]], 2L)
  expect_identical(rebuilt$note[[1]], "Replacement aggregation")
  expect_equal(rebuilt$component_count[[1]], 2L)
  aggregation_audit <- DBI::dbGetQuery(
    con,
    "SELECT action, count(*)::integer AS n
     FROM audit.general_log
     WHERE transaction_id = txid_current()
       AND schema_name = 'discrete'
       AND table_name = 'result_aggregations'
       AND COALESCE(
         (new_data ->> 'result_id')::integer,
         (original_data ->> 'result_id')::integer
       ) = $1
     GROUP BY action",
    params = list(result_id)
  )
  expect_equal(
    aggregation_audit$n[aggregation_audit$action == "UPDATE"],
    1L
  )
  expect_length(
    aggregation_audit$n[aggregation_audit$action == "DELETE"],
    0L
  )

  set_result_aggregation_constraints(con, "deferred")
  DBI::dbExecute(
    con,
    "SET LOCAL aquacache.defer_result_aggregation_refresh = 'on'"
  )
  DBI::dbExecute(con, "SAVEPOINT reject_stale_preservation")
  DBI::dbExecute(
    con,
    "UPDATE discrete.result_components
     SET result = result + 1
     WHERE result_id = $1 AND observation_number = 1",
    params = list(result_id)
  )
  expect_error(
    DBI::dbExecute(
      con,
      "SELECT discrete.convert_result_aggregation_to_direct(
         $1, 'preserve_calculated', NULL, NULL, NULL, $2
       )",
      params = list(result_id, "Stale-value test")
    ),
    "stale or has an invalid aggregate condition"
  )
  suppressWarnings(DBI::dbExecute(
    con,
    "ROLLBACK TO SAVEPOINT reject_stale_preservation"
  ))
  DBI::dbExecute(con, "RELEASE SAVEPOINT reject_stale_preservation")
  DBI::dbExecute(
    con,
    "SET LOCAL aquacache.defer_result_aggregation_refresh = 'off'"
  )
  set_result_aggregation_constraints(con, "immediate")

  # Check if current user is one of 'admin' or 'postgres', otherwise the function won't run
  curr_user <- DBI::dbGetQuery(con, "SELECT current_user")[[1]]
  if (curr_user %in% c("admin", "postgres")) {
    DBI::dbExecute(
      con,
      "SELECT discrete.convert_result_aggregation_to_direct(
         $1, 'preserve_calculated', NULL, NULL, NULL, $2
       )",
      params = list(result_id, "Preserve the verified aggregate for test")
    )
    expect_equal(
      DBI::dbGetQuery(
        con,
        "SELECT result FROM discrete.results WHERE result_id = $1",
        params = list(result_id)
      )$result[[1]],
      20
    )
    expect_equal(
      DBI::dbGetQuery(
        con,
        "SELECT count(*) FROM discrete.result_aggregations WHERE result_id = $1",
        params = list(result_id)
      )[[1]],
      0
    )
  }

  synchronize_discrete_sample_detail(
    con = con,
    sample_id = sample_id,
    remote_results = normalized_replacement$results,
    result_ids = result_id,
    pending_results = list(),
    sample_qualifiers = NULL,
    sample_observers = NULL,
    result_aggregations = normalized_replacement$result_aggregations,
    result_components = normalized_replacement$result_components
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id = $1",
      params = list(result_id)
    )$result[[1]],
    20
  )

  synchronize_discrete_sample_detail(
    con = con,
    sample_id = sample_id,
    remote_results = results,
    result_ids = result_id,
    pending_results = list(),
    sample_qualifiers = NULL,
    sample_observers = NULL,
    result_aggregations = data.frame(),
    result_components = data.frame()
  )
  DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT result FROM discrete.results WHERE result_id = $1",
      params = list(result_id)
    )$result[[1]],
    12
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*) FROM discrete.result_aggregations WHERE result_id = $1",
      params = list(result_id)
    )[[1]],
    0
  )
  conversion_audit <- DBI::dbGetQuery(
    con,
    "SELECT original_data ->> 'note' AS note
     FROM audit.general_log
     WHERE transaction_id = txid_current()
       AND schema_name = 'discrete'
       AND table_name = 'result_aggregations'
       AND action = 'DELETE'
       AND (original_data ->> 'result_id')::integer = $1
       AND original_data ->> 'note' LIKE
         '%Incoming sample detail no longer aggregates this result%'",
    params = list(result_id)
  )
  expect_equal(nrow(conversion_audit), 1L)
  expect_match(
    conversion_audit$note[[1]],
    "Incoming sample detail no longer aggregates this result"
  )
})


test_that("sample group helpers create and assign idempotently", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  sample <- DBI::dbGetQuery(
    con,
    "SELECT sample_id, owner
     FROM discrete.samples
     WHERE owner IS NOT NULL
     LIMIT 1"
  )
  if (nrow(sample) == 0L) {
    testthat::skip("No owned sample is available for the sample-group test.")
  }
  code <- paste0("test-group-", format(Sys.time(), "%Y%m%d%H%M%OS6"))

  group_types <- getSampleGroupTypes(con)
  expect_equal(nrow(group_types), 7L)
  expect_true(all(group_types$active))
  expect_true(all(nzchar(group_types$group_type_name_fr)))
  expect_identical(group_types$sort_order, sort(group_types$sort_order))

  group_id <- createSampleGroup(
    con = con,
    group_type = "qc_set",
    owner = sample$owner[[1]],
    group_code = code,
    group_name = "Test QC set"
  )
  same_group_id <- createSampleGroup(
    con = con,
    group_type = "qc_set",
    owner = sample$owner[[1]],
    group_code = code,
    group_name = "Ignored duplicate label"
  )
  expect_equal(same_group_id, group_id)

  assignSamplesToGroup(
    con = con,
    sample_id = sample$sample_id[[1]],
    sample_group_id = group_id
  )
  assignSamplesToGroup(
    con = con,
    sample_id = sample$sample_id[[1]],
    sample_group_id = group_id
  )
  membership_count <- DBI::dbGetQuery(
    con,
    "SELECT count(*)
     FROM discrete.sample_group_members
     WHERE sample_group_id = $1 AND sample_id = $2",
    params = list(group_id, sample$sample_id[[1]])
  )[[1]]
  expect_equal(membership_count, 1)
})


test_that("discrete import records retain sample IDs and source input", {
  sample <- data.frame(location_id = NA_integer_, sample_type = 4L)
  results <- data.frame(parameter_id = 1L, result = 0)
  groups <- data.frame(group_type = "trip", group_code = "trip-1")
  qualifiers <- data.frame(qualifier_type_id = 2L)
  observers <- data.frame(observer_id = 3L, observer_role = "sampler")
  aggregations <- data.frame(
    result_row = 1L,
    aggregation_type = "mean"
  )
  components <- data.frame(
    result_row = 1L,
    observation_number = 1:2,
    result = c(9, 11)
  )

  record <- new_discrete_import_record(
    sample_series_id = 2L,
    sample_id = 17L,
    action = "inserted",
    sample = sample,
    results = results,
    sample_groups = groups,
    sample_qualifiers = qualifiers,
    sample_observers = observers,
    result_aggregations = aggregations,
    result_components = components
  )

  expect_named(
    record,
    c(
      "sample_series_id",
      "sample_id",
      "action",
      "sample",
      "results",
      "sample_groups",
      "sample_qualifiers",
      "sample_observers",
      "result_aggregations",
      "result_components"
    )
  )
  expect_identical(record$sample_id, 17L)
  expect_identical(record$sample[[1]], sample)
  expect_identical(record$results[[1]], results)
  expect_identical(record$sample_groups[[1]], groups)
  expect_identical(record$sample_qualifiers[[1]], qualifiers)
  expect_identical(record$sample_observers[[1]], observers)
  expect_identical(record$result_aggregations[[1]], aggregations)
  expect_identical(record$result_components[[1]], components)

  empty <- bind_discrete_import_records(list())
  expect_named(empty, names(record))
  expect_equal(nrow(empty), 0L)
})


test_that("addNewDiscrete commits a locationless blank with its group", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  sample <- DBI::dbGetQuery(
    con,
    "SELECT s.*
     FROM discrete.samples s
     WHERE s.owner IS NOT NULL
     ORDER BY s.sample_id
     LIMIT 1"
  )
  results <- DBI::dbGetQuery(
    con,
    "SELECT * FROM discrete.results ORDER BY result_id LIMIT 1"
  )
  blank_type <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE NOT requires_location AND requires_sample_group
     ORDER BY sample_type_id
     LIMIT 1"
  )

  sample$sample_id <- NULL
  sample$location_id <- NA_integer_
  sample$sub_location_id <- NA_integer_
  sample$sample_type <- blank_type$sample_type_id[[1]]
  sample$datetime <- as.POSIXct(Sys.time(), tz = "UTC") + 86400
  sample$import_source <- "testthat"
  sample$import_source_id <- paste0(
    "locationless-blank-",
    format(Sys.time(), "%Y%m%d%H%M%OS6")
  )

  results$result_id <- NULL
  results$sample_id <- NULL
  groups <- data.frame(
    group_type = "trip",
    group_code = paste0("test-trip-", format(Sys.time(), "%Y%m%d%H%M%OS6")),
    group_name = "Test trip",
    sequence_in_group = 1L,
    member_note = "Locationless blank",
    stringsAsFactors = FALSE
  )

  expect_error(
    addNewDiscrete(con, sample, results),
    "requires at least one sample group"
  )
  sample_id <- addNewDiscrete(
    con = con,
    sample = sample,
    results = results,
    sample_groups = groups
  )
  DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")

  inserted <- DBI::dbGetQuery(
    con,
    "SELECT s.location_id, count(sgm.sample_group_member_id) AS groups
     FROM discrete.samples s
     LEFT JOIN discrete.sample_group_members sgm ON sgm.sample_id = s.sample_id
     WHERE s.sample_id = $1
     GROUP BY s.sample_id, s.location_id",
    params = list(sample_id)
  )
  expect_true(is.na(inserted$location_id[[1]]))
  expect_equal(inserted$groups[[1]], 1)
})
