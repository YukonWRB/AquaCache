test_that("addNewContinuous inserts a new measurement", {
  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT t.timeseries_id
     FROM timeseries t
     JOIN measurements_continuous mc
       ON mc.timeseries_id = t.timeseries_id
     WHERE t.timeseries_type = 'basic'
     GROUP BY t.timeseries_id
     LIMIT 1"
  )[[1]]
  if (is.na(tsid)) {
    testthat::skip("No timeseries data available for addNewContinuous test.")
  }

  last_row <- DBI::dbGetQuery(
    con,
    "SELECT datetime, value, period FROM measurements_continuous WHERE timeseries_id = $1 ORDER BY datetime DESC LIMIT 1",
    params = list(tsid)
  )
  if (nrow(last_row) == 0) {
    testthat::skip(
      "No continuous measurements available for the test timeseries."
    )
  }

  new_datetime <- last_row$datetime + lubridate::dhours(1)
  df <- data.frame(
    datetime = new_datetime,
    value = last_row$value + 0.123
  )
  if (!is.na(last_row$period)) {
    df$period <- last_row$period
  }

  active <- dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  addNewContinuous(tsid, df, con = con, target = "realtime", overwrite = "no")

  inserted <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM measurements_continuous WHERE timeseries_id = $1 AND datetime = $2",
    params = list(tsid, new_datetime)
  )[[1]]
  expect_equal(inserted, 1)
})

test_that("addNewContinuous errors when datetime is missing", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id
     FROM timeseries
     WHERE timeseries_type = 'basic'
     LIMIT 1"
  )[[1]]
  if (is.na(tsid)) {
    testthat::skip("No timeseries data available for addNewContinuous test.")
  }
  expect_error(
    addNewContinuous(
      tsid,
      data.frame(value = 1),
      con = con,
      target = "realtime"
    ),
    "must contain a column named 'datetime'"
  )
})

test_that("addNewContinuous rejects compound timeseries", {
  local_mocked_bindings(
    dbGetQuery = function(con, statement, params = NULL, ...) {
      data.frame(
        timeseries_id = params[[1]],
        timeseries_type = "compound"
      )
    },
    .package = "DBI"
  )

  expect_error(
    addNewContinuous(
      9001L,
      data.frame(
        datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        value = 1
      ),
      con = structure(list(), class = "mock_con")
    ),
    "only be added to basic timeseries"
  )
})
