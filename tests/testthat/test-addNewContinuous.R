test_that("addNewContinuous inserts a new measurement", {
  skip_if_no_postgres()

  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries LIMIT 1"
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
  skip_if_no_postgres()

  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries LIMIT 1"
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
