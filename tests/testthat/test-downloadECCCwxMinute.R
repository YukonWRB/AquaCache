test_that("downloadECCCwxMinute queries filtered API rows and deduplicates rows", {
  con <- structure(list(), class = "mock_con")

  fetch_calls <- character()

  testthat::local_mocked_bindings(
    dlECCCwxMinute_fetch_csv = function(url) {
      fetch_calls <<- c(fetch_calls, url)
      data.table::data.table(
        `date_tm-value` = c(
          "2026-04-16T12:01:00Z",
          "2026-04-16T12:00:00Z",
          "2026-04-16T12:00:00Z"
        ),
        air_temp = c(1.4, 1.2, 1.2),
        `air_temp-uom` = c("C", "C", "C")
      )
    },
    dlECCCwxMinute_db_defaults = function(con) {
      list(
        organization = 99L,
        grade = 1L,
        approval = 2L,
        qualifier = 3L
      )
    },
    .env = asNamespace("AquaCache")
  )

  res <- AquaCache::downloadECCCwxMinute(
    location = "CVXY",
    parameter = "temp",
    start_datetime = as.POSIXct("2026-04-16 12:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-04-16 12:01:59", tz = "UTC"),
    con = con
  )

  expect_equal(nrow(res), 2L)
  expect_equal(
    res$datetime,
    as.POSIXct(
      c("2026-04-16 12:00:00", "2026-04-16 12:01:00"),
      tz = "UTC"
    )
  )
  expect_equal(res$value, c(1.2, 1.4))
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
  expect_length(fetch_calls, 1L)
  expect_true(any(grepl("url=CVXY", fetch_calls, fixed = TRUE)))
  expect_true(any(grepl("properties=date_tm-value%2Cair_temp%2Cair_temp-uom", fetch_calls, fixed = TRUE)))
  expect_true(any(grepl("limit=2000", fetch_calls, fixed = TRUE)))
})

test_that("downloadECCCwxMinute converts wind speed from km/h to m/s", {
  con <- structure(list(), class = "mock_con")

  testthat::local_mocked_bindings(
    dlECCCwxMinute_fetch_csv = function(url) {
      data.table::data.table(
        `date_tm-value` = c(
          "2026-04-16T12:01:00Z",
          "2026-04-16T12:00:00Z"
        ),
        avg_wnd_spd_10m_pst1mt = c(18, 36),
        `avg_wnd_spd_10m_pst1mt-uom` = c("km/h", "km/h")
      )
    },
    dlECCCwxMinute_db_defaults = function(con) {
      list(
        organization = 99L,
        grade = 1L,
        approval = 2L,
        qualifier = 3L
      )
    },
    .env = asNamespace("AquaCache")
  )

  res <- AquaCache::downloadECCCwxMinute(
    location = "CVXY",
    parameter = "wind_spd",
    start_datetime = as.POSIXct("2026-04-16 12:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-04-16 12:01:59", tz = "UTC"),
    con = con
  )

  expect_equal(res$value, c(10, 5))
})
