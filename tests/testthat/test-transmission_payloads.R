test_that("generic transmission storage accepts provider-neutral payloads", {
  statements <- character()
  parameters <- list()
  local_mocked_bindings(
    dbGetQuery = function(con, statement, params = NULL, ...) {
      statements <<- c(statements, statement)
      parameters[[length(parameters) + 1L]] <<- params
      data.frame(transmission_payload_id = 91L)
    },
    .package = "DBI"
  )

  result <- AquaCache:::transmission_store_payloads(
    con = structure(list(), class = "mock_con"),
    transmission_setup_ids = 27L,
    payloads = data.frame(
      transmission_datetime = as.POSIXct(
        "2026-08-20 12:00:00",
        tz = "UTC"
      ),
      payload_text = "cellular logger payload",
      stringsAsFactors = FALSE
    ),
    source_server = "cellular-gateway",
    source_metadata = list(transport = "cellular")
  )

  expect_equal(result$transmissions_archived, 1L)
  expect_equal(result$transmissions_inserted, 1L)
  expect_match(statements, "continuous.transmission_payloads", fixed = TRUE)
  expect_equal(parameters[[1L]][[1L]], 27L)
  expect_equal(parameters[[1L]][[2L]], "cellular-gateway")
  expect_match(parameters[[1L]][[3L]], '"transport":"cellular"')
})

test_that("runtime replay support is declared by adapter capabilities", {
  capability <- data.frame(source_fx = "downloadCellular")
  capability$argument_schema <- list(list(
    schema_version = 1L,
    arguments = list(list(
      name = "from_storage",
      source = "runtime",
      help = "Replay archived cellular transmissions."
    ))
  ))

  expect_true(AquaCache:::source_adapter_supports_runtime_argument(
    capability,
    "from_storage"
  ))
  expect_false(AquaCache:::source_adapter_supports_runtime_argument(
    capability,
    "unknown_argument"
  ))
})

test_that("generic transmission run history records terminal outcomes", {
  statement <- NULL
  parameters <- NULL
  local_mocked_bindings(
    dbGetQuery = function(con, sql, params = NULL, ...) {
      statement <<- sql
      parameters <<- params
      data.frame(transmission_import_run_id = 801)
    },
    .package = "DBI"
  )

  run_id <- AquaCache:::transmission_record_import_run(
    con = structure(list(), class = "mock_con"),
    transmission_route_id = 22L,
    query_since = as.POSIXct("2026-08-20 00:00:00", tz = "UTC"),
    query_until = as.POSIXct("2026-08-21 00:00:00", tz = "UTC"),
    importer = "downloadCellular",
    source_server = "cellular-gateway",
    status = "success",
    payload_bytes = 120,
    transmissions_received = 2L,
    measurements_parsed = 8L,
    source_metadata = list(retrieval_mode = "live")
  )

  expect_equal(run_id, 801)
  expect_match(statement, "RETURNING transmission_import_run_id", fixed = TRUE)
  expect_match(statement, "clock_timestamp()", fixed = TRUE)
  expect_equal(parameters[[1L]], 22L)
  expect_equal(parameters[[4L]], "downloadCellular")
  expect_equal(parameters[[6L]], "success")
  expect_match(parameters[[13L]], '"retrieval_mode":"live"')
})

test_that("delegated transmission runs receive workflow write counts", {
  statement <- NULL
  parameters <- NULL
  local_mocked_bindings(
    dbExecute = function(con, sql, params = NULL, ...) {
      statement <<- sql
      parameters <<- params
      2L
    },
    .package = "DBI"
  )

  updated <- AquaCache:::transmission_finalize_import_runs(
    con = structure(list(), class = "mock_con"),
    transmission_import_run_ids = c(801, 802),
    measurements_inserted = 4L,
    workflow = "synchronize_continuous"
  )

  expect_equal(updated, 2L)
  expect_match(statement, "measurements_inserted = $1", fixed = TRUE)
  expect_match(statement, "IN (801, 802)", fixed = TRUE)
  expect_equal(parameters, list(4L, "synchronize_continuous"))
})

test_that("incomplete delegated transmission writes are marked failed", {
  statement <- NULL
  parameters <- NULL
  local_mocked_bindings(
    dbExecute = function(con, sql, params = NULL, ...) {
      statement <<- sql
      parameters <<- params
      1L
    },
    .package = "DBI"
  )

  updated <- AquaCache:::transmission_fail_import_runs(
    con = structure(list(), class = "mock_con"),
    transmission_import_run_ids = 803,
    workflow = "getNewContinuous",
    error_message = "measurement write failed"
  )

  expect_equal(updated, 1L)
  expect_match(statement, "SET status = 'failed'", fixed = TRUE)
  expect_match(statement, "'measurement_write_failed', TRUE", fixed = TRUE)
  expect_equal(
    parameters,
    list("measurement write failed", "getNewContinuous")
  )
})
