test_that("downloadECCCwxMinute queries filtered API rows and deduplicates rows", {
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- downloadECCCwxMinute(
    location = "CVXY",
    parameter = "air_temp",
    start_datetime = Sys.time() - 60 * 60,
    end_datetime = Sys.time(),
    con = con
  )

  expect_gt(nrow(res), 0)
  expect_true(inherits(res, "data.table"))
  expect_named(
    res,
    c(
      "datetime",
      "value",
      "grade",
      "approval",
      "qualifier",
      "owner",
      "contributor"
    )
  )
  expect_true(inherits(res$datetime, "POSIXct"))
})
