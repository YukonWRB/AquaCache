mock_sync_timeseries_table <- function(
  timeseries_ids,
  source_fx_name,
  source_fx_args = rep(NA_character_, length(timeseries_ids))
) {
  source_fx <- if (length(source_fx_name) == 1L) {
    rep(source_fx_name, length(timeseries_ids))
  } else {
    source_fx_name
  }

  data.frame(
    parameter_id = rep(1L, length(timeseries_ids)),
    timeseries_id = timeseries_ids,
    timeseries_source_adapter_id = seq_along(timeseries_ids),
    source_fx = source_fx,
    source_fx_args = source_fx_args,
    synchronize_priority = rep(1L, length(timeseries_ids)),
    last_daily_calculation = rep(
      as.POSIXct(NA, tz = "UTC"),
      length(timeseries_ids)
    ),
    aggregation_type = rep("instantaneous", length(timeseries_ids)),
    default_owner = rep(NA_integer_, length(timeseries_ids)),
    active = rep(TRUE, length(timeseries_ids)),
    sync_remote = rep(TRUE, length(timeseries_ids)),
    transmission_platform_identifier = rep(
      NA_character_,
      length(timeseries_ids)
    ),
    stringsAsFactors = FALSE
  )
}

mock_sync_db_get_query <- function(
  timeseries_ids,
  source_fx_name,
  source_fx_args = rep(NA_character_, length(timeseries_ids))
) {
  function(con, statement, ...) {
    if (
      grepl("FROM public.source_adapter_capabilities", statement, fixed = TRUE) &&
        !grepl("FROM continuous.timeseries t", statement, fixed = TRUE)
    ) {
      return(data.frame(
        source_fx = c(
          "downloadECCCwx",
          "downloadECCCwxMinute",
          "downloadNESDIS",
          "downloadRWIS"
        ),
        data_domain = rep("continuous", 4L),
        adapter_kind = c("standard", "standard", "transmission", "standard"),
        requires_transmission_mapping = c(FALSE, FALSE, TRUE, FALSE),
        inject_timeseries_id = c(FALSE, FALSE, TRUE, FALSE),
        parallel_group_strategy = c(
          "source_args",
          "source_args",
          "transmission_platform",
          "timeseries"
        ),
        parallel_group_args_json = c(
          '["location","interval"]',
          '["location"]',
          "[]",
          "[]"
        ),
        allow_empty_initial_fetch = c(FALSE, FALSE, TRUE, FALSE),
        transmission_method_codes_json = c("[]", "[]", '["GOES_DCS"]', "[]"),
        argument_schema_json = c(
          '{"schema_version":1,"arguments":[]}',
          '{"schema_version":1,"arguments":[]}',
          paste0(
            '{"schema_version":1,"arguments":[',
            '{"name":"from_storage","source":"runtime",',
            '"help":"Replay archived transmissions."},',
            '{"name":"end_datetime","source":"runtime",',
            '"help":"End of the replay window."}]}'
          ),
          '{"schema_version":1,"arguments":[]}'
        ),
        ui_config_json = rep("{}", 4L),
        enabled = TRUE,
        note = NA_character_
      ))
    }
    if (
      grepl("FROM continuous.timeseries t", statement, fixed = TRUE)
    ) {
      return(mock_sync_timeseries_table(
        timeseries_ids = timeseries_ids,
        source_fx_name = source_fx_name,
        source_fx_args = source_fx_args
      ))
    }
    if (grepl("FROM public.grade_types", statement, fixed = TRUE)) {
      return(data.frame(grade_type_id = 1L))
    }
    if (grepl("FROM public.approval_types", statement, fixed = TRUE)) {
      return(data.frame(approval_type_id = 1L))
    }
    if (grepl("FROM public.qualifier_types", statement, fixed = TRUE)) {
      return(data.frame(qualifier_type_id = 1L))
    }

    stop(sprintf("Unexpected dbGetQuery statement in test: %s", statement))
  }
}

test_that("synchronize_continuous records sequential failures instead of leaving NAs", {
  local_mocked_bindings(
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    downloadRWIS = function(start_datetime, con, ...) {
      warning("simulated synchronize warning")
      data.frame(
        datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        value = 1
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(
      c(1323L, 1322L),
      "downloadRWIS"
    ),
    dbExecute = function(con, statement, ...) 1L,
    .package = "DBI"
  )

  res <- synchronize_continuous(
    con = structure(list(), class = "mock_con"),
    timeseries_id = c(1323L, 1322L),
    start_datetime = "2026-01-01 00:00"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$timeseries_id, c(1323L, 1322L))
  expect_equal(res$success, c(FALSE, FALSE))
  expect_false(any(is.na(res$success)))
  expect_match(
    res$message,
    "^Warning: simulated synchronize warning$",
    all = FALSE
  )
})

test_that("synchronize_continuous handles a single parallel result without simplifying dimensions", {
  skip_if_not_installed("foreach")
  skip_if_not_installed("doSNOW")

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
    AquaConnect = function(...) structure(list(), class = "mock_con"),
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    downloadRWIS = function(start_datetime, con, ...) {
      warning("simulated synchronize warning")
      data.frame(
        datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        value = 1
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(1323L, "downloadRWIS"),
    dbExecute = function(con, statement, ...) 1L,
    dbDisconnect = function(con, ...) invisible(TRUE),
    .package = "DBI"
  )
  local_mocked_bindings(
    check_installed = function(...) invisible(NULL),
    .package = "rlang"
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

  res <- synchronize_continuous(
    con = NULL,
    timeseries_id = 1323L,
    start_datetime = "2026-01-01 00:00",
    dbName = "mock_db",
    dbHost = "mock_host",
    dbPort = "5432",
    dbUser = "mock_user",
    dbPass = "mock_pass"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$timeseries_id, 1323L)
  expect_false(res$success)
  expect_match(res$message, "^Warning: simulated synchronize warning$")
})

test_that("synchronize_continuous groups cache-sharing ECCC tasks in parallel", {
  skip_if_not_installed("foreach")
  skip_if_not_installed("doSNOW")

  connect_calls <- 0L
  captured <- new.env(parent = emptyenv())
  captured$parameters <- character()
  captured$cluster_exports <- character()

  adapter_capabilities <- data.frame(
    source_fx = c("downloadRWIS", "downloadECCCwx"),
    inject_timeseries_id = c(FALSE, FALSE),
    parallel_group_strategy = c("timeseries", "source_args"),
    stringsAsFactors = FALSE
  )
  adapter_capabilities$parallel_group_args <- I(list(
    character(),
    c("location", "interval")
  ))

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
    AquaConnect = function(...) {
      connect_calls <<- connect_calls + 1L
      structure(list(), class = "mock_con")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(con, silent = TRUE) TRUE,
    getSourceAdapterCapabilities = function(...) adapter_capabilities,
    downloadECCCwx = function(start_datetime, con, location, parameter, interval, ...) {
      captured$parameters <- c(captured$parameters, parameter)
      data.frame(
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric()
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(
      timeseries_ids = c(1323L, 1322L, 2000L),
      source_fx_name = "downloadECCCwx",
      source_fx_args = source_fx_args
    ),
    dbExecute = function(con, statement, ...) 1L,
    dbDisconnect = function(con, ...) invisible(TRUE),
    .package = "DBI"
  )
  local_mocked_bindings(
    check_installed = function(...) invisible(NULL),
    .package = "rlang"
  )
  local_mocked_bindings(
    detectCores = function(...) 4L,
    makeCluster = function(...) structure(list(), class = "mock_cluster"),
    stopCluster = function(cl) invisible(TRUE),
    clusterExport = function(cl, varlist, envir) {
      captured$cluster_exports <- varlist
      invisible(TRUE)
    },
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

  res <- synchronize_continuous(
    con = NULL,
    timeseries_id = c(1323L, 1322L, 2000L),
    start_datetime = c(
      "2026-01-02 00:00",
      "2026-01-01 00:00",
      "2026-01-01 00:00"
    ),
    dbName = "mock_db",
    dbHost = "mock_host",
    dbPort = "5432",
    dbUser = "mock_user",
    dbPass = "mock_pass"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$timeseries_id, c(1323L, 1322L, 2000L))
  expect_true(all(res$success))
  expect_equal(connect_calls, 3L)
  expect_equal(captured$parameters, c("wind_spd", "temp", "temp"))
  expect_true("source_adapter_args_decode" %in% captured$cluster_exports)
})

test_that("synchronize_continuous groups cache-sharing ECCC minute tasks in parallel", {
  skip_if_not_installed("foreach")
  skip_if_not_installed("doSNOW")

  connect_calls <- 0L
  captured <- new.env(parent = emptyenv())
  captured$parameters <- character()

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
    AquaConnect = function(...) {
      connect_calls <<- connect_calls + 1L
      structure(list(), class = "mock_con")
    },
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(con, silent = TRUE) TRUE,
    downloadECCCwxMinute = function(start_datetime, con, location, parameter, station_type = "AUTO", ...) {
      captured$parameters <- c(captured$parameters, parameter)
      data.frame(
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric()
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(
      timeseries_ids = c(1323L, 1322L, 2000L),
      source_fx_name = "downloadECCCwxMinute",
      source_fx_args = source_fx_args
    ),
    dbExecute = function(con, statement, ...) 1L,
    dbDisconnect = function(con, ...) invisible(TRUE),
    .package = "DBI"
  )
  local_mocked_bindings(
    check_installed = function(...) invisible(NULL),
    .package = "rlang"
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

  res <- synchronize_continuous(
    con = NULL,
    timeseries_id = c(1323L, 1322L, 2000L),
    start_datetime = c(
      "2026-01-02 00:00",
      "2026-01-01 00:00",
      "2026-01-01 00:00"
    ),
    dbName = "mock_db",
    dbHost = "mock_host",
    dbPort = "5432",
    dbUser = "mock_user",
    dbPass = "mock_pass"
  )

  expect_s3_class(res, "data.frame")
  expect_equal(res$timeseries_id, c(1323L, 1322L, 2000L))
  expect_true(all(res$success))
  expect_equal(connect_calls, 3L)
  expect_equal(captured$parameters, c("wind_spd", "temp", "temp"))
})

test_that("synchronize_continuous injects a registered stored replay window", {
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(...) TRUE,
    downloadNESDIS = function(
      start_datetime,
      end_datetime,
      con,
      timeseries_id,
      from_storage,
      ...
    ) {
      captured$start_datetime <- start_datetime
      captured$end_datetime <- end_datetime
      captured$timeseries_id <- timeseries_id
      captured$from_storage <- from_storage
      data.frame(
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric()
      )
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(1261L, "downloadNESDIS"),
    dbExecute = function(con, statement, ...) 1L,
    .package = "DBI"
  )

  result <- synchronize_continuous(
    con = structure(list(), class = "mock_con"),
    timeseries_id = 1261L,
    start_datetime = "2026-07-01 00:00:00",
    from_storage = TRUE,
    end_datetime = "2026-07-15 00:00:00"
  )

  expect_true(result$success)
  expect_true(captured$from_storage)
  expect_equal(captured$timeseries_id, 1261L)
  expect_equal(
    captured$start_datetime,
    as.POSIXct("2026-07-01 00:00:00", tz = "UTC")
  )
  expect_equal(
    captured$end_datetime,
    as.POSIXct("2026-07-15 00:00:00", tz = "UTC")
  )
})

test_that("stored replay does not compare beyond its last parsed observation", {
  measurement_query <- NULL
  finalized <- NULL
  base_query <- mock_sync_db_get_query(1261L, "downloadNESDIS")
  observation_time <- as.POSIXct("2026-07-10 12:00:00", tz = "UTC")
  local_mocked_bindings(
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    dbTransBegin = function(...) TRUE,
    adjust_grade = function(...) invisible(TRUE),
    adjust_approval = function(...) invisible(TRUE),
    adjust_qualifier = function(...) invisible(TRUE),
    adjust_owner = function(...) invisible(TRUE),
    adjust_contributor = function(...) invisible(TRUE),
    downloadNESDIS = function(from_storage, end_datetime, ...) {
      result <- data.frame(datetime = observation_time, value = 1)
      attr(result, "transmission_import_run_ids") <- 6101
      result
    },
    transmission_finalize_import_runs = function(...) {
      finalized <<- list(...)
      invisible(1L)
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      if (
        grepl(
          "FROM continuous.measurements_continuous",
          statement,
          fixed = TRUE
        )
      ) {
        measurement_query <<- statement
        return(data.frame(
          no_update = FALSE,
          datetime = observation_time,
          value = 1,
          period = "00:00:00",
          imputed = FALSE
        ))
      }
      base_query(con, statement, ...)
    },
    dbExecute = function(con, statement, ...) 1L,
    .package = "DBI"
  )

  result <- synchronize_continuous(
    con = structure(list(), class = "mock_con"),
    timeseries_id = 1261L,
    start_datetime = "2026-07-01 00:00:00",
    from_storage = TRUE,
    end_datetime = "2026-07-15 00:00:00"
  )

  expect_true(result$success)
  expect_match(
    measurement_query,
    "datetime <= '2026-07-10 12:00:00'",
    fixed = TRUE
  )
  expect_false(grepl("2026-07-15", measurement_query, fixed = TRUE))
  expect_equal(finalized$transmission_import_run_ids, 6101)
  expect_equal(finalized$measurements_inserted, 0L)
  expect_equal(finalized$workflow, "synchronize_continuous")
})

test_that("stored replay rejects adapters that have not opted in", {
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(1323L, "downloadRWIS"),
    dbExecute = function(con, statement, ...) 1L,
    .package = "DBI"
  )

  expect_error(
    synchronize_continuous(
      con = structure(list(), class = "mock_con"),
      timeseries_id = 1323L,
      start_datetime = "2026-07-01 00:00:00",
      from_storage = TRUE,
      end_datetime = "2026-07-15 00:00:00"
    ),
    "not supported by the selected source adapter.*downloadRWIS"
  )
})
