skip_if_offline()

test_that("downloadNWIS fetches data", {
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- downloadNWIS(
    location = "15041200",
    parameter = "00060",
    start_datetime = "2025-09-09 00:00:00",
    end_datetime = "2025-09-09 23:59:59",
    con = con
  )

  expect_equal(nrow(res), 96)
  # res$datetime is POSIXct
  expect_s3_class(res$datetime, "POSIXct")
  # res$datetime is in UTC timezone
  expect_equal(attr(res$datetime, "tzone"), "UTC")
  expect_named(
    res,
    c(
      "datetime",
      "value",
      "approval",
      "qualifier"
    )
  )
})
