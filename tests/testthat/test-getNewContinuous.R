mock_getnew_timeseries_table <- function(
  timeseries_ids,
  source_fx_name,
  source_fx_args = rep(NA_character_, length(timeseries_ids)),
  last_data_point = rep(as.POSIXct(NA, tz = "UTC"), length(timeseries_ids)),
  aggregation_type = rep("instantaneous", length(timeseries_ids))
) {
  source_fx <- if (length(source_fx_name) == 1L) {
    rep(source_fx_name, length(timeseries_ids))
  } else {
    source_fx_name
  }

  data.frame(
    parameter_id = rep(1L, length(timeseries_ids)),
    timeseries_id = timeseries_ids,
    source_fx = source_fx,
    source_fx_args = source_fx_args,
    aggregation_type = aggregation_type,
    default_owner = rep(NA_integer_, length(timeseries_ids)),
    default_data_sharing_agreement_id = rep(NA_integer_, length(timeseries_ids)),
    active = rep(TRUE, length(timeseries_ids)),
    last_data_point = last_data_point,
    stringsAsFactors = FALSE
  )
}

mock_getnew_db_get_query <- function(
  timeseries_ids,
  source_fx_name,
  source_fx_args = rep(NA_character_, length(timeseries_ids)),
  last_data_point = rep(as.POSIXct(NA, tz = "UTC"), length(timeseries_ids))
) {
  function(con, statement, params = NULL, ...) {
    if (grepl("FROM timeseries t", statement, fixed = TRUE)) {
      return(mock_getnew_timeseries_table(
        timeseries_ids = timeseries_ids,
        source_fx_name = source_fx_name,
        source_fx_args = source_fx_args,
        last_data_point = last_data_point
      ))
    }
    if (grepl("FROM grade_types", statement, fixed = TRUE)) {
      return(data.frame(grade_type_id = 1L))
    }
    if (grepl("FROM approval_types", statement, fixed = TRUE)) {
      return(data.frame(approval_type_id = 1L))
    }
    if (grepl("FROM qualifier_types", statement, fixed = TRUE)) {
      return(data.frame(qualifier_type_id = 1L))
    }
    if (
      grepl(
        "SELECT MAX(datetime) FROM measurements_continuous WHERE timeseries_id = $1",
        statement,
        fixed = TRUE
      )
    ) {
      tsid <- params[[1]]
      idx <- match(tsid, timeseries_ids)
      if (is.na(idx)) {
        stop(sprintf("Unexpected timeseries_id in worker query: %s", tsid))
      }
      return(data.frame(last_data_point = last_data_point[[idx]]))
    }

    stop(sprintf("Unexpected dbGetQuery statement in test: %s", statement))
  }
}

test_that("getNewContinuous groups cache-sharing ECCC tasks in parallel", {
  skip_if_not_installed("foreach")
  skip_if_not_installed("doSNOW")

  connect_calls <- 0L
  captured <- new.env(parent = emptyenv())
  captured$parameters <- character()
  captured$connect_args <- list()

  timeseries_ids <- c(1323L, 1322L, 2000L)
  last_data_point <- as.POSIXct(
    c("2026-01-02 00:00:00", "2026-01-01 00:00:00", "2026-01-03 00:00:00"),
    tz = "UTC"
  )
  source_fx_args <- c(
    '{"location":"27950","parameter":"temp","interval":"hour"}',
    '{"location":"27950","parameter":"wind_spd","interval":"hour"}',
    '{"location":"88888","parameter":"temp","interval":"hour"}'
  )

  mock_dopar <- function(obj, expr) {
    expr_sub <- substitute(expr)
    parent_env <- parent.frame()
    rows <- lapply(
      obj$iter,
      function(i) {
        eval(expr_sub, envir = list2env(list(i = i), parent = parent_env))
      }
    )
    do.call(obj$combine, rows)
  }

  local_mocked_bindings(
    AquaConnect = function(
      name = Sys.getenv("aquacacheName"),
      host = Sys.getenv("aquacacheHost"),
      port = Sys.getenv("aquacachePort"),
      username = Sys.getenv("aquacacheAdminUser"),
      password = Sys.getenv("aquacacheAdminPass"),
      silent = FALSE
    ) {
      connect_calls <<- connect_calls + 1L
      captured$connect_args[[length(captured$connect_args) + 1L]] <- list(
        name = name,
        host = host,
        port = port,
        username = username,
        password = password,
        silent = silent
      )
      structure(list(), class = "mock_con")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(con, silent = TRUE) TRUE,
    adjust_grade = function(...) invisible(TRUE),
    adjust_approval = function(...) invisible(TRUE),
    adjust_qualifier = function(...) invisible(TRUE),
    adjust_owner = function(...) invisible(TRUE),
    adjust_contributor = function(...) invisible(TRUE),
    adjust_data_sharing_agreement = function(...) invisible(TRUE),
    dbAppendTableRLS = function(con, table, value) invisible(TRUE),
    calculate_stats = function(...) invisible(TRUE),
    downloadECCCwx = function(start_datetime, con, location, parameter, interval, ...) {
      captured$parameters <- c(captured$parameters, parameter)
      data.frame(
        datetime = start_datetime,
        value = 1
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_getnew_db_get_query(
      timeseries_ids = timeseries_ids,
      source_fx_name = "downloadECCCwx",
      source_fx_args = source_fx_args,
      last_data_point = last_data_point
    ),
    dbExecute = function(con, statement, ...) 1L,
    dbDisconnect = function(con, ...) invisible(TRUE),
    .package = "DBI"
  )
  local_mocked_bindings(
    detectCores = function(...) 4L,
    makeCluster = function(...) structure(list(), class = "mock_cluster"),
    stopCluster = function(cl) invisible(TRUE),
    clusterExport = function(cl, varlist, envir) invisible(TRUE),
    .package = "parallel"
  )
  local_mocked_bindings(
    registerDoSNOW = function(cl) invisible(TRUE),
    .package = "doSNOW"
  )
  local_mocked_bindings(
    foreach = function(i, .combine = rbind, ...) {
      list(iter = i, combine = .combine)
    },
    `%dopar%` = mock_dopar,
    .package = "foreach"
  )

  expect_message(
    res <- getNewContinuous(
      con = NULL,
      timeseries_id = timeseries_ids,
      active = "all",
      dbName = "mock_db",
      dbHost = "mock_host",
      dbPort = "5432",
      dbUser = "mock_user",
      dbPass = "mock_pass"
    ),
    regexp = "Parallel plan: 3 timeseries across 2 task groups"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$timeseries_id, timeseries_ids)
  expect_equal(connect_calls, 3L)
  expect_equal(captured$parameters, c("wind_spd", "temp", "temp"))
  expect_true(all(vapply(
    captured$connect_args,
    function(args) identical(args, list(
      name = "mock_db",
      host = "mock_host",
      port = "5432",
      username = "mock_user",
      password = "mock_pass",
      silent = TRUE
    )),
    logical(1)
  )))
})

test_that("getNewContinuous groups cache-sharing ECCC minute tasks in parallel", {
  skip_if_not_installed("foreach")
  skip_if_not_installed("doSNOW")

  connect_calls <- 0L
  captured <- new.env(parent = emptyenv())
  captured$parameters <- character()
  captured$connect_args <- list()

  timeseries_ids <- c(1323L, 1322L, 2000L)
  last_data_point <- as.POSIXct(
    c("2026-01-02 00:00:00", "2026-01-01 00:00:00", "2026-01-03 00:00:00"),
    tz = "UTC"
  )
  source_fx_args <- c(
    '{"location":"CVXY","parameter":"temp","station_type":"AUTO"}',
    '{"location":"CVXY","parameter":"wind_spd","station_type":"AUTO"}',
    '{"location":"CYXY","parameter":"temp","station_type":"AUTO"}'
  )

  mock_dopar <- function(obj, expr) {
    expr_sub <- substitute(expr)
    parent_env <- parent.frame()
    rows <- lapply(
      obj$iter,
      function(i) {
        eval(expr_sub, envir = list2env(list(i = i), parent = parent_env))
      }
    )
    do.call(obj$combine, rows)
  }

  local_mocked_bindings(
    AquaConnect = function(
      name = Sys.getenv("aquacacheName"),
      host = Sys.getenv("aquacacheHost"),
      port = Sys.getenv("aquacachePort"),
      username = Sys.getenv("aquacacheAdminUser"),
      password = Sys.getenv("aquacacheAdminPass"),
      silent = FALSE
    ) {
      connect_calls <<- connect_calls + 1L
      captured$connect_args[[length(captured$connect_args) + 1L]] <- list(
        name = name,
        host = host,
        port = port,
        username = username,
        password = password,
        silent = silent
      )
      structure(list(), class = "mock_con")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(con, silent = TRUE) TRUE,
    adjust_grade = function(...) invisible(TRUE),
    adjust_approval = function(...) invisible(TRUE),
    adjust_qualifier = function(...) invisible(TRUE),
    adjust_owner = function(...) invisible(TRUE),
    adjust_contributor = function(...) invisible(TRUE),
    adjust_data_sharing_agreement = function(...) invisible(TRUE),
    dbAppendTableRLS = function(con, table, value) invisible(TRUE),
    calculate_stats = function(...) invisible(TRUE),
    downloadECCCwxMinute = function(
      start_datetime,
      con,
      location,
      parameter,
      station_type = "AUTO",
      ...
    ) {
      captured$parameters <- c(captured$parameters, parameter)
      data.frame(
        datetime = start_datetime,
        value = 1
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_getnew_db_get_query(
      timeseries_ids = timeseries_ids,
      source_fx_name = "downloadECCCwxMinute",
      source_fx_args = source_fx_args,
      last_data_point = last_data_point
    ),
    dbExecute = function(con, statement, ...) 1L,
    dbDisconnect = function(con, ...) invisible(TRUE),
    .package = "DBI"
  )
  local_mocked_bindings(
    detectCores = function(...) 4L,
    makeCluster = function(...) structure(list(), class = "mock_cluster"),
    stopCluster = function(cl) invisible(TRUE),
    clusterExport = function(cl, varlist, envir) invisible(TRUE),
    .package = "parallel"
  )
  local_mocked_bindings(
    registerDoSNOW = function(cl) invisible(TRUE),
    .package = "doSNOW"
  )
  local_mocked_bindings(
    foreach = function(i, .combine = rbind, ...) {
      list(iter = i, combine = .combine)
    },
    `%dopar%` = mock_dopar,
    .package = "foreach"
  )

  expect_message(
    res <- getNewContinuous(
      con = NULL,
      timeseries_id = timeseries_ids,
      active = "all",
      dbName = "mock_db",
      dbHost = "mock_host",
      dbPort = "5432",
      dbUser = "mock_user",
      dbPass = "mock_pass"
    ),
    regexp = "Cache-sharing groups cover 3 timeseries across 2 groups; largest group size = 2\\."
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$timeseries_id, timeseries_ids)
  expect_equal(connect_calls, 3L)
  expect_equal(captured$parameters, c("wind_spd", "temp", "temp"))
  expect_true(all(vapply(
    captured$connect_args,
    function(args) identical(args, list(
      name = "mock_db",
      host = "mock_host",
      port = "5432",
      username = "mock_user",
      password = "mock_pass",
      silent = TRUE
    )),
    logical(1)
  )))
})

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

test_that("getNewContinuous recalculates stats from the earliest imported datetime", {
  captured <- new.env(parent = emptyenv())
  tsid <- 1323L
  new_rows <- data.frame(
    datetime = as.POSIXct(
      c(
        "2026-01-03 12:00:00",
        "2026-01-04 12:00:00",
        "2026-01-05 12:00:00"
      ),
      tz = "UTC"
    ),
    value = c(1, 2, 3)
  )

  local_mocked_bindings(
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(con, silent = TRUE) TRUE,
    adjust_grade = function(...) invisible(TRUE),
    adjust_approval = function(...) invisible(TRUE),
    adjust_qualifier = function(...) invisible(TRUE),
    adjust_owner = function(...) invisible(TRUE),
    adjust_contributor = function(...) invisible(TRUE),
    adjust_data_sharing_agreement = function(...) invisible(TRUE),
    dbAppendTableRLS = function(con, table, value) invisible(TRUE),
    calculate_stats = function(con, timeseries_id, start_recalc = NULL) {
      captured$timeseries_id <- timeseries_id
      captured$start_recalc <- start_recalc
      invisible(TRUE)
    },
    downloadRWIS = function(start_datetime, con, ...) {
      new_rows
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_getnew_db_get_query(
      timeseries_ids = tsid,
      source_fx_name = "downloadRWIS",
      source_fx_args = '{"location":"TEST","parameter":"TEST"}',
      last_data_point = as.POSIXct("2026-01-02 00:00:00", tz = "UTC")
    ),
    dbExecute = function(con, statement, ...) 1L,
    .package = "DBI"
  )

  result <- getNewContinuous(
    con = structure(list(), class = "mock_con"),
    timeseries_id = tsid,
    active = "all",
    stats = TRUE
  )

  expect_equal(captured$timeseries_id, tsid)
  expect_equal(captured$start_recalc, min(new_rows$datetime))
  expect_true(tsid %in% result$timeseries_id)
})
