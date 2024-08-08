test_that("period is calculated", {
  # Requires a connection to the Aqua database so can't run on CI
  skip_on_cran()
  skip_on_ci()
  data <- readRDS(test_path("data", "periodicity_data.rds"))
  res <- calculate_period(data, timeseries_id = 541, con = AquaConnect(silent = TRUE))
  expect_snapshot(res)
})
