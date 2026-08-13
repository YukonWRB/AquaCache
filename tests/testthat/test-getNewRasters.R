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
