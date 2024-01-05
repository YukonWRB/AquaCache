test_that("period is calculated", {
  data <- readRDS(test_path("data", "periodicity_data.rds"))
  res <- calculate_period(data, timeseries_id = 541, con = hydrometConnect(silent=TRUE))
  expect_snapshot(res)
})
