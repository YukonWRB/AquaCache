test_that("period is calculated", {
  skip_on_cran()
  data <- readRDS(test_path("data", "periodicity_data.rds"))
  res <- calculate_period(
    data,
    timeseries_id = 541,
    con = AquaConnect(silent = TRUE)
  )
  expect_snapshot(res)
})

test_that("error thrown when too few measurements and no timeseries id", {
  df <- data.frame(
    datetime = as.POSIXct(c("2023-01-01", "2023-01-02"), tz = "UTC")
  )
  expect_error(
    calculate_period(df, timeseries_id = NA),
    "There were too few measurements to calculate a period and no timeseries_id was provided to fetch additional data."
  )
})
