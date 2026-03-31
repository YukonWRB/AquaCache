mock_sync_timeseries_table <- function(timeseries_ids, source_fx_name) {
  data.frame(
    parameter_id = rep(1L, length(timeseries_ids)),
    timeseries_id = timeseries_ids,
    source_fx = rep(source_fx_name, length(timeseries_ids)),
    source_fx_args = rep(NA_character_, length(timeseries_ids)),
    last_daily_calculation = rep(
      as.POSIXct(NA, tz = "UTC"),
      length(timeseries_ids)
    ),
    aggregation_type = rep("instantaneous", length(timeseries_ids)),
    default_owner = rep(NA_integer_, length(timeseries_ids)),
    active = rep(TRUE, length(timeseries_ids)),
    sync_remote = rep(TRUE, length(timeseries_ids)),
    stringsAsFactors = FALSE
  )
}

mock_sync_db_get_query <- function(timeseries_ids, source_fx_name) {
  function(con, statement, ...) {
    if (
      grepl("FROM timeseries t JOIN aggregation_types", statement, fixed = TRUE)
    ) {
      return(mock_sync_timeseries_table(timeseries_ids, source_fx_name))
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

    stop(sprintf("Unexpected dbGetQuery statement in test: %s", statement))
  }
}

test_sync_source_warning <- function(start_datetime, con) {
  warning("simulated synchronize warning")
  data.frame(
    datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    value = 1
  )
}

test_that("synchronize_continuous records sequential failures instead of leaving NAs", {
  had_existing_source_fx <- exists(
    "test_sync_source_warning",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (had_existing_source_fx) {
    existing_source_fx <- get(
      "test_sync_source_warning",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }
  assign(
    "test_sync_source_warning",
    test_sync_source_warning,
    envir = .GlobalEnv
  )
  on.exit(
    {
      if (had_existing_source_fx) {
        assign(
          "test_sync_source_warning",
          existing_source_fx,
          envir = .GlobalEnv
        )
      } else {
        rm(list = "test_sync_source_warning", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  local_mocked_bindings(
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(
      c(1323L, 1322L),
      "test_sync_source_warning"
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

  had_existing_source_fx <- exists(
    "test_sync_source_warning",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (had_existing_source_fx) {
    existing_source_fx <- get(
      "test_sync_source_warning",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }
  assign(
    "test_sync_source_warning",
    test_sync_source_warning,
    envir = .GlobalEnv
  )
  on.exit(
    {
      if (had_existing_source_fx) {
        assign(
          "test_sync_source_warning",
          existing_source_fx,
          envir = .GlobalEnv
        )
      } else {
        rm(list = "test_sync_source_warning", envir = .GlobalEnv)
      }
    },
    add = TRUE
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
    AquaConnect = function(...) structure(list(), class = "mock_con"),
    advisory_lock_acquire = function(...) TRUE,
    advisory_lock_release = function(...) TRUE,
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = mock_sync_db_get_query(1323L, "test_sync_source_warning"),
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

test_that("select_changed_daily_stats treats missing existing rows as changed", {
  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      empty_daily_stats()
    },
    .package = "DBI"
  )

  rows <- data.frame(
    date = as.Date("2026-01-02"),
    value = 1.23,
    imputed = FALSE
  )

  res <- select_changed_daily_stats(
    structure(list(), class = "mock_con"),
    1323L,
    rows
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_equal(res$date, as.Date("2026-01-02"))
  expect_equal(res$value, 1.23)
  expect_false(res$imputed)
})
