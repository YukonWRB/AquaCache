skip_if_offline()

create_test_con <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE organizations (organization_id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "INSERT INTO organizations VALUES (1, 'Environment and Climate Change Canada')")
  DBI::dbExecute(con, "CREATE TABLE grade_types (grade_type_id INTEGER, grade_type_code TEXT)")
  DBI::dbExecute(con, "INSERT INTO grade_types VALUES (1, 'UNS')")
  DBI::dbExecute(con, "CREATE TABLE approval_types (approval_type_id INTEGER, approval_type_code TEXT)")
  DBI::dbExecute(con, "INSERT INTO approval_types VALUES (1, 'UNS')")
  DBI::dbExecute(con, "CREATE TABLE qualifier_types (qualifier_type_id INTEGER, qualifier_type_code TEXT)")
  DBI::dbExecute(con, "INSERT INTO qualifier_types VALUES (1, 'UNS')")
  con
}

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
