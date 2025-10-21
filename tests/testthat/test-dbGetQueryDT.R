test_that("dbGetQueryDT converts SQLite date columns", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(
    con,
    "CREATE TABLE test (id INTEGER, date TEXT, datetime TEXT, start_dt TEXT)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO test VALUES (1, '2024-01-01', '2024-01-01 12:00:00', '2024-01-01 13:00:00')"
  )
  res <- dbGetQueryDT(con, "SELECT * FROM test")
  expect_true(data.table::is.data.table(res))
  expect_s3_class(res$date, "Date")
  expect_s3_class(res$datetime, "POSIXct")
  expect_s3_class(res$start_dt, "POSIXct")
  DBI::dbDisconnect(con)
})
