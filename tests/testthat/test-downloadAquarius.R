skip_if_offline()

test_that("downloadAquarius fetches data", {
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  start_dt <- as.POSIXct("2022-01-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-01-02 00:01:00", tz = "UTC")
  res <- downloadAquarius(
    location = "TEST",
    parameter = "Air Temp.Temperature",
    start_datetime = start_dt,
    end_datetime = end_dt,
    login = c("readonly", "WaterIsLife"),
    server = "https://yukon.aquaticinformatics.net/AQUARIUS",
    con = con
  )

  expect_equal(nrow(res), 25)
  # res$datetime is POSIXct
  expect_s3_class(res$datetime, "POSIXct")
  # res$datetime is in UTC timezone
  expect_equal(attr(res$datetime, "tzone"), "UTC")
  expect_named(res, c("datetime", "value", "approval", "grade", "qualifier"))
})
