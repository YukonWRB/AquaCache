test_that("getNewRasters handles end_datetime without start_datetime", {
  prelim <- as.POSIXct("2026-08-10 06:00:00", tz = "UTC")
  captured_args <- NULL

  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM spatial.raster_series_index", statement, fixed = TRUE)) {
        return(data.frame(
          raster_series_id = 1L,
          end_datetime = as.POSIXct("2026-08-11", tz = "UTC"),
          last_issue = as.POSIXct(NA, tz = "UTC"),
          type = "reanalysis",
          source_fx = "downloadHRDPA",
          source_fx_args = NA_character_,
          fetch_priority = 1L,
          parameter_name = "precipitation",
          active = TRUE
        ))
      }
      if (grepl("flag = 'PRELIMINARY'", statement, fixed = TRUE)) {
        return(data.frame(min = prelim))
      }
      stop("Unexpected query in getNewRasters test: ", statement)
    },
    dbExecute = function(...) 1L,
    .package = "DBI"
  )
  local_mocked_bindings(
    getSourceAdapterCapabilities = function(...) {
      data.frame(source_fx = "downloadHRDPA")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    downloadHRDPA = function(...) {
      captured_args <<- list(...)
      NULL
    },
    .package = "AquaCache"
  )

  result <- suppressMessages(getNewRasters(
    raster_series_ids = "all",
    con = structure(list(), class = "mock_con"),
    start_datetime = NULL,
    end_datetime = as.POSIXct("2026-08-12", tz = "UTC")
  ))

  expect_identical(result, character())
  expect_equal(captured_args$start_datetime, prelim - 1)
  expect_equal(
    captured_args$end_datetime,
    as.POSIXct("2026-08-12", tz = "UTC")
  )
})

test_that("getNewRasters uses an explicit start for an empty forecast series", {
  requested_start <- as.POSIXct("2026-08-20 00:00:00", tz = "UTC")
  captured_args <- NULL

  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM spatial.raster_series_index", statement, fixed = TRUE)) {
        return(data.frame(
          raster_series_id = 10L,
          end_datetime = as.POSIXct(NA, tz = "UTC"),
          last_issue = as.POSIXct(NA, tz = "UTC"),
          type = "forecast",
          source_fx = "downloadHRDPS",
          source_fx_args = NA_character_,
          fetch_priority = 1L,
          parameter_name = "precipitation",
          active = TRUE
        ))
      }
      stop("Unexpected query in empty forecast test: ", statement)
    },
    dbExecute = function(...) 1L,
    .package = "DBI"
  )
  local_mocked_bindings(
    getSourceAdapterCapabilities = function(...) {
      data.frame(source_fx = "downloadHRDPS")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    downloadHRDPS = function(...) {
      captured_args <<- list(...)
      NULL
    },
    .package = "AquaCache"
  )

  result <- suppressMessages(getNewRasters(
    raster_series_ids = 10L,
    con = structure(list(), class = "mock_con"),
    start_datetime = requested_start
  ))

  expect_identical(result, character())
  expect_equal(captured_args$start_datetime, requested_start)
  expect_null(captured_args$end_datetime)
})

test_that("getNewRasters surfaces append failures without changing forecast cleanup", {
  issue <- as.POSIXct("2026-08-27 00:00:00", tz = "UTC")
  successful_valid_from <- issue + 3600
  failed_valid_from <- issue + 7200
  executed_sql <- character()

  forecast <- list(
    list(
      rast = structure(list(), class = "mock_raster"),
      valid_from = successful_valid_from,
      valid_to = successful_valid_from + 3600,
      issued = issue,
      source = "https://example.test/PT001H.grib2",
      flag = NA_character_,
      units = "mm",
      model = "HRDPS"
    ),
    list(
      rast = structure(list(), class = "mock_raster"),
      valid_from = failed_valid_from,
      valid_to = failed_valid_from + 3600,
      issued = issue,
      source = "https://example.test/PT002H.grib2",
      flag = NA_character_,
      units = "mm",
      model = "HRDPS"
    ),
    forecast = TRUE,
    issued = issue
  )

  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM spatial.raster_series_index", statement, fixed = TRUE)) {
        return(data.frame(
          raster_series_id = 10L,
          end_datetime = issue,
          last_issue = issue - 6 * 3600,
          type = "forecast",
          source_fx = "downloadHRDPS",
          source_fx_args = NA_character_,
          fetch_priority = 1L,
          parameter_name = "precipitation",
          active = TRUE
        ))
      }
      if (grepl("SELECT reference_id", statement, fixed = TRUE)) {
        return(data.frame(reference_id = NA_integer_))
      }
      stop("Unexpected query in getNewRasters append-failure test: ", statement)
    },
    dbExecute = function(con, statement, ...) {
      executed_sql <<- c(executed_sql, statement)
      1L
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    getSourceAdapterCapabilities = function(...) {
      data.frame(source_fx = "downloadHRDPS")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(...) TRUE,
    downloadHRDPS = function(...) forecast,
    insertACModelRaster = function(valid_from, ...) {
      if (identical(as.numeric(valid_from), as.numeric(failed_valid_from))) {
        stop("simulated raster2pgsql failure")
      }
      1001L
    },
    .package = "AquaCache"
  )

  expect_warning(
    result <- suppressMessages(getNewRasters(
      raster_series_ids = 10L,
      con = structure(list(), class = "mock_con")
    )),
    paste0(
      "raster 2 of 2, valid_from 2026-08-27 02:00:00 UTC.*",
      "simulated raster2pgsql failure"
    )
  )

  append_errors <- attr(result, "append_errors")
  expect_identical(as.vector(result), "10")
  expect_s3_class(append_errors, "data.table")
  expect_equal(nrow(append_errors), 1L)
  expect_equal(append_errors$raster_index, 2L)
  expect_equal(append_errors$valid_from, failed_valid_from)
  expect_match(append_errors$error, "simulated raster2pgsql failure")

  cleanup_sql <- executed_sql[
    grepl(
      "DELETE FROM spatial.rasters_reference WHERE raster_series_id",
      executed_sql,
      fixed = TRUE
    )
  ]
  expect_length(cleanup_sql, 1L)
  expect_match(cleanup_sql, "2026-08-27 01:00:00", fixed = TRUE)
  expect_match(cleanup_sql, "2026-08-27 02:00:00", fixed = TRUE)
  expect_false(any(grepl(
    "UPDATE spatial.raster_series_index",
    executed_sql,
    fixed = TRUE
  )))
})
