skip_if_offline()

test_that("downloadECCCwx fetches hourly data", {
  con <- create_test_con()
  start_dt <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2024-01-02 00:01:00", tz = "UTC")
  location <- 27950
  interval <- "hour"
  res <- downloadECCCwx(location, "temp", start_dt, end_dt, interval, con)
  
  expect_equal(nrow(res), 25)
  expect_s3_class(res$datetime, "POSIXct")
  expect_named(res, c("datetime", "value", "grade", "approval", "qualifier", "owner", "contributor"))
  DBI::dbDisconnect(con)
})

test_that("downloadECCCwx handles daily data", {
  con <- create_test_con()
  start_dt <- as.Date("2024-01-01")
  end_dt <- as.Date("2024-01-05")
  location <- 27950
  interval <- "day"
  res <- downloadECCCwx(location, "mean_temp", start_dt, end_dt, interval, con)
  
  expect_equal(nrow(res), 5)
  expect_s3_class(res$datetime, "POSIXct")
  expect_named(res, c("datetime", "value", "grade", "approval", "qualifier", "owner", "contributor"))
  
  DBI::dbDisconnect(con)
})
