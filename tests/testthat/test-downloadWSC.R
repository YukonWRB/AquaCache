skip_if_offline()

test_that("downloadWSC fetches data", {
  con <- create_test_con()
  start_dt <- Sys.time() - 60 * 60 * 24 * 7 # 7 days ago
  end_dt <- Sys.time() # now
  res <- downloadWSC(location = "09EA004", parameter = 47, start_datetime = start_dt, end_datetime = end_dt, con)
  
  expect_gt(nrow(res), 0)
  expect_s3_class(res$datetime, "POSIXct")
  expect_named(res, c("datetime", "value", "approval", "grade", "qualifier"))
  DBI::dbDisconnect(con)
})
