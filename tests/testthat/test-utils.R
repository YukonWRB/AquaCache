test_that("fmt formats datetimes as UTC strings", {
  time <- as.POSIXct("2024-01-01 00:00:00", tz = "Etc/GMT+2")
  expect_equal(fmt(time), "2024-01-01 02:00:00")

  utc_times <- as.POSIXct(c("2024-06-01 12:34:56", "2024-06-02 01:02:03"), tz = "UTC")
  expect_equal(fmt(utc_times), c("2024-06-01 12:34:56", "2024-06-02 01:02:03"))
})
