skip_on_cran()

test_that("period is calculated", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  data <- readRDS(test_path("data", "periodicity_data.rds"))
  res <- calculate_period(
    data,
    timeseries_id = 541,
    con = con
  )
  expect_snapshot(res)

  # Same thing with data.table
  data <- data.table::as.data.table(data)
  resdt <- calculate_period(
    data,
    timeseries_id = 541,
    con = con
  )
  expect_snapshot(resdt)
})

test_that("error thrown when too few measurements and no timeseries id", {
  df <- data.frame(
    datetime = as.POSIXct(c("2023-01-01", "2023-01-02"), tz = "UTC")
  )
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  expect_error(
    calculate_period(df, timeseries_id = NA, con = con),
    "There were too few measurements to calculate a period and no timeseries_id was provided to fetch additional data."
  )

  # Works with data.table as well
  dt <- data.table::data.table(
    datetime = as.POSIXct(c("2023-01-01", "2023-01-02"), tz = "UTC")
  )
  expect_error(
    calculate_period(dt, timeseries_id = NA, con = con),
    "There were too few measurements to calculate a period and no timeseries_id was provided to fetch additional data."
  )
})
