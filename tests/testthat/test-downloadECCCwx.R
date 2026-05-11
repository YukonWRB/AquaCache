skip_if_offline()

test_that("downloadECCCwx fetches hourly data", {
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  start_dt <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2024-01-02 00:01:00", tz = "UTC")
  location <- 27950
  interval <- "hour"
  res <- downloadECCCwx(location, "temp", start_dt, end_dt, interval, con)

  expect_equal(nrow(res), 25)
  # res$datetime is POSIXct
  expect_s3_class(res$datetime, "POSIXct")
  # res$datetime is in UTC timezone
  expect_equal(attr(res$datetime, "tzone"), "UTC")
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
})

test_that("downloadECCCwx reuses a cached superset date range", {
  skip_if_not_installed("weathercan")

  cache_dir <- file.path(
    tempdir(),
    paste0("downloadECCCwx-test-", as.integer(stats::runif(1, 1, 1e6)))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  dl <- data.frame(
    time = as.POSIXct(
      c(
        "2024-01-02 00:00:00",
        "2024-01-02 01:00:00",
        "2024-01-02 02:00:00"
      ),
      tz = "UTC"
    ),
    temp = c(1.1, 1.2, 1.3)
  )
  save(
    dl,
    file = file.path(cache_dir, "27950_hour_2024-01-01_2024-01-05.rdata")
  )

  testthat::local_mocked_bindings(
    dlECCCwx_cache_dir = function() {
      cache_dir
    },
    .env = asNamespace("AquaCache")
  )
  testthat::local_mocked_bindings(
    weather_dl = function(...) {
      stop("downloadECCCwx should have used the cached superset file.")
    },
    .package = "weathercan"
  )
  testthat::local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM organizations", statement, fixed = TRUE)) {
        return(data.frame(organization_id = 1L))
      }
      if (grepl("FROM grade_types", statement, fixed = TRUE)) {
        return(data.frame(grade_type_id = 2L))
      }
      if (grepl("FROM approval_types", statement, fixed = TRUE)) {
        return(data.frame(approval_type_id = 3L))
      }
      if (grepl("FROM qualifier_types", statement, fixed = TRUE)) {
        return(data.frame(qualifier_type_id = 4L))
      }

      stop(sprintf("Unexpected dbGetQuery statement in test: %s", statement))
    },
    .package = "DBI"
  )

  res <- downloadECCCwx(
    location = 27950,
    parameter = "temp",
    start_datetime = as.POSIXct("2024-01-02 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2024-01-02 02:00:00", tz = "UTC"),
    interval = "hour",
    con = structure(list(), class = "mock_con")
  )

  expect_equal(nrow(res), 3L)
  expect_equal(res$value, c(1.1, 1.2, 1.3))
  expect_equal(res$owner, rep(1L, 3L))
  expect_equal(res$grade, rep(2L, 3L))
  expect_equal(res$approval, rep(3L, 3L))
  expect_equal(res$qualifier, rep(4L, 3L))
})

test_that("downloadECCCwx handles daily data", {
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  start_dt <- as.Date("2024-01-01")
  end_dt <- as.Date("2024-01-05")
  location <- 27950
  interval <- "day"
  res <- downloadECCCwx(location, "mean_temp", start_dt, end_dt, interval, con)

  expect_equal(nrow(res), 5)
  expect_s3_class(res$datetime, "POSIXct")
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
})
