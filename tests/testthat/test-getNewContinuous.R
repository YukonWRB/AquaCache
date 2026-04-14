test_that("getNewContinuous does not delete history when period calculation needs context", {
  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  candidate <- DBI::dbGetQuery(
    con,
    "SELECT t.timeseries_id
     FROM timeseries t
     JOIN aggregation_types at ON at.aggregation_type_id = t.aggregation_type_id
     JOIN measurements_continuous mc ON mc.timeseries_id = t.timeseries_id
     WHERE at.aggregation_type <> 'instantaneous'
     GROUP BY t.timeseries_id
     HAVING COUNT(mc.datetime) > 20
     ORDER BY COUNT(mc.datetime) DESC
     LIMIT 1"
  )
  if (nrow(candidate) == 0) {
    testthat::skip("No non-instantaneous timeseries with enough history found.")
  }

  tsid <- candidate$timeseries_id[[1]]
  history <- DBI::dbGetQuery(
    con,
    "SELECT datetime, value
     FROM measurements_continuous
     WHERE timeseries_id = $1
     ORDER BY datetime DESC
     LIMIT 5",
    params = list(tsid)
  )
  if (nrow(history) < 2) {
    testthat::skip("Not enough history to build a regression test for getNewContinuous.")
  }

  step_seconds <- median(as.numeric(diff(sort(history$datetime)), units = "secs"))
  if (is.na(step_seconds) || step_seconds <= 0) {
    step_seconds <- 3600
  }

  last_datetime <- max(history$datetime)
  new_rows <- data.frame(
    datetime = last_datetime + step_seconds * seq_len(3),
    value = as.numeric(history$value[[1]]) + c(0.1, 0.2, 0.3)
  )

  AquaCache:::dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  DBI::dbExecute(
    con,
    "CREATE TEMP TABLE ac_delete_log (n integer NOT NULL DEFAULT 0)"
  )
  DBI::dbExecute(con, "INSERT INTO ac_delete_log DEFAULT VALUES")
  DBI::dbExecute(
    con,
    "CREATE OR REPLACE FUNCTION pg_temp.ac_log_measurements_delete()
     RETURNS trigger
     LANGUAGE plpgsql
     AS $$
     BEGIN
       UPDATE ac_delete_log SET n = n + 1;
       RETURN OLD;
     END;
     $$;"
  )
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TRIGGER ac_log_measurements_delete
       BEFORE DELETE ON measurements_continuous
       FOR EACH ROW
       WHEN (OLD.timeseries_id = ",
      tsid,
      ")
       EXECUTE FUNCTION pg_temp.ac_log_measurements_delete();"
    )
  )

  DBI::dbExecute(
    con,
    "UPDATE timeseries
     SET source_fx = $1,
         source_fx_args = $2
     WHERE timeseries_id = $3",
    params = list(
      "downloadRWIS",
      '{"location":"TEST","parameter":"TEST"}',
      tsid
    )
  )

  testthat::local_mocked_bindings(
    downloadRWIS = function(start_datetime, con, ...) {
      new_rows
    },
    .package = "AquaCache"
  )

  result <- AquaCache::getNewContinuous(
    con = con,
    timeseries_id = tsid,
    active = "all"
  )

  delete_count <- DBI::dbGetQuery(con, "SELECT n FROM ac_delete_log")[[1]]
  inserted_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*)
     FROM measurements_continuous
     WHERE timeseries_id = $1
       AND datetime >= $2",
    params = list(tsid, min(new_rows$datetime))
  )[[1]]

  expect_equal(delete_count, 0)
  expect_equal(inserted_count, nrow(new_rows))
  expect_true(tsid %in% result$timeseries_id)
})

test_that("getNewContinuous passes source_fx_args to downloadECCCwxMinute", {
  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  candidate <- DBI::dbGetQuery(
    con,
    "SELECT t.timeseries_id, MAX(mc.datetime) AS last_datetime
     FROM timeseries t
     JOIN aggregation_types at ON at.aggregation_type_id = t.aggregation_type_id
     JOIN measurements_continuous mc ON mc.timeseries_id = t.timeseries_id
     WHERE at.aggregation_type = 'instantaneous'
     GROUP BY t.timeseries_id
     ORDER BY MAX(mc.datetime) DESC
     LIMIT 1"
  )
  if (nrow(candidate) == 0) {
    testthat::skip("No instantaneous timeseries with history found.")
  }

  tsid <- candidate$timeseries_id[[1]]
  last_datetime <- candidate$last_datetime[[1]]
  new_rows <- data.frame(
    datetime = last_datetime + c(60, 120),
    value = c(1.1, 1.2)
  )
  captured <- new.env(parent = emptyenv())

  AquaCache:::dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  DBI::dbExecute(
    con,
    "UPDATE timeseries
     SET source_fx = $1,
         source_fx_args = $2
     WHERE timeseries_id = $3",
    params = list(
      "downloadECCCwxMinute",
      '{"location":"CVXY","parameter":"temp","station_type":"AUTO"}',
      tsid
    )
  )

  testthat::local_mocked_bindings(
    downloadECCCwxMinute = function(
      start_datetime,
      con,
      location,
      parameter,
      station_type = "AUTO",
      ...
    ) {
      captured$start_datetime <- start_datetime
      captured$location <- location
      captured$parameter <- parameter
      captured$station_type <- station_type
      new_rows
    },
    .package = "AquaCache"
  )

  result <- AquaCache::getNewContinuous(
    con = con,
    timeseries_id = tsid,
    active = "all"
  )

  inserted_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*)
     FROM measurements_continuous
     WHERE timeseries_id = $1
       AND datetime >= $2",
    params = list(tsid, min(new_rows$datetime))
  )[[1]]

  expect_true(inherits(captured$start_datetime, "POSIXct"))
  expect_equal(captured$start_datetime, last_datetime + 1)
  expect_equal(captured$location, "CVXY")
  expect_equal(captured$parameter, "temp")
  expect_equal(captured$station_type, "AUTO")
  expect_equal(inserted_count, nrow(new_rows))
  expect_true(tsid %in% result$timeseries_id)
})
