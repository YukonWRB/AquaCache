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
  results_template <- DBI::dbGetQuery(con, "SELECT * FROM discrete.results LIMIT 1")
  if (nrow(results_template) == 0) {
    testthat::skip("No results data available for addNewDiscrete test.")
  }

  sample <- sample_template[1, , drop = FALSE]
  max_dt <- DBI::dbGetQuery(con, "SELECT MAX(datetime) FROM discrete.samples")[[1]]
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
  if (!DBI::dbExistsTable(
    con,
    DBI::Id(schema = "discrete", table = "result_components")
  )) {
    testthat::skip("The test database has not applied patch 60.")
  }

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
  if (!nrow(sample) || !nrow(results) || !nrow(qualifier) || !nrow(observer)) {
    testthat::skip("Composite sample reference data are unavailable.")
  }

  sample$sample_id <- NULL
  sample$datetime <- as.POSIXct(Sys.time(), tz = "UTC") + 172800
  sample$import_source <- NA_character_
  sample$import_source_id <- NA_character_
  results$result_id <- NULL
  results$sample_id <- NULL
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
    sample_observers = observer$observer_id,
    result_aggregations = data.frame(
      result_row = 1L,
      aggregation_type = "mean"
    ),
    result_components = component_values
  )
  DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")

  canonical <- DBI::dbGetQuery(
    con,
    "SELECT r.result_id, r.result, rat.aggregation_type
     FROM discrete.results r
     JOIN discrete.result_aggregations ra USING (result_id)
     JOIN discrete.result_aggregation_types rat
       USING (result_aggregation_type_id)
     WHERE sample_id = $1",
    params = list(sample_id)
  )
  expect_equal(canonical$result[[1]], 9)
  expect_identical(canonical$aggregation_type[[1]], "mean")
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) FROM discrete.result_components
     WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )[[1]], 3)

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

  DBI::dbExecute(
    con,
    "UPDATE discrete.result_components
     SET included_in_aggregate = FALSE,
         note = COALESCE(note, 'Excluded for test')
     WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )
  expect_true(is.na(DBI::dbGetQuery(
    con,
    "SELECT result FROM discrete.results WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )$result[[1]]))

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
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT result FROM discrete.results WHERE result_id = $1",
    params = list(canonical$result_id[[1]])
  )$result[[1]], 276)
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


test_that("synchronization replaces and removes result aggregation detail", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)
  if (!DBI::dbExistsTable(
    con,
    DBI::Id(schema = "discrete", table = "result_aggregations")
  )) {
    testthat::skip("The test database has not applied patch 60.")
  }
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
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT result FROM discrete.results WHERE result_id = $1",
    params = list(result_id)
  )$result[[1]], 9)

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
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT result FROM discrete.results WHERE result_id = $1",
    params = list(result_id)
  )$result[[1]], 12)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) FROM discrete.result_aggregations WHERE result_id = $1",
    params = list(result_id)
  )[[1]], 0)
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
