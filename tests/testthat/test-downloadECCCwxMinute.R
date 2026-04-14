test_that("downloadECCCwxMinute caches parsed minute files by station and day", {
  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cache_dir <- file.path(
    tempdir(),
    paste0("downloadECCCwxMinute-test-", as.integer(stats::runif(1, 1, 1e6)))
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  listing <- data.table::data.table(
    filename = c(
      "2026-03-15-0000-CVXY-AUTO-minute-swob.xml",
      "2026-03-15-0001-CVXY-AUTO-minute-swob.xml"
    ),
    datetime = as.POSIXct(
      c("2026-03-15 00:00:00", "2026-03-15 00:01:00"),
      tz = "UTC"
    )
  )
  xml_0000 <- paste(
    readLines(testthat::test_path("fixtures", "swob-minute-0000.xml"), warn = FALSE),
    collapse = "\n"
  )
  xml_0001 <- paste(
    readLines(testthat::test_path("fixtures", "swob-minute-0001.xml"), warn = FALSE),
    collapse = "\n"
  )
  fetch_calls <- character()
  list_calls <- 0L

  testthat::local_mocked_bindings(
    downloadECCCwxMinute_cache_dir = function() {
      cache_dir
    },
    downloadECCCwxMinute_list_day_files = function(location, station_type, day) {
      list_calls <<- list_calls + 1L
      listing
    },
    downloadECCCwxMinute_fetch_xml = function(url) {
      fetch_calls <<- c(fetch_calls, basename(url))
      if (grepl("0000", url, fixed = TRUE)) {
        return(xml_0000)
      }

      xml_0001
    },
    .env = asNamespace("AquaCache")
  )

  res_temp <- AquaCache::downloadECCCwxMinute(
    location = "CVXY",
    parameter = "temp",
    start_datetime = as.POSIXct("2026-03-15 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-03-15 00:01:59", tz = "UTC"),
    con = con
  )
  expect_equal(nrow(res_temp), 2)
  expect_equal(res_temp$value, c(1.2, 1.4))
  expect_named(
    res_temp,
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
  expect_equal(list_calls, 1L)
  expect_equal(length(fetch_calls), 2L)

  res_hum <- AquaCache::downloadECCCwxMinute(
    location = "CVXY",
    parameter = "rel_hum",
    start_datetime = as.POSIXct("2026-03-15 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-03-15 00:01:59", tz = "UTC"),
    con = con
  )
  expect_equal(res_hum$value, c(65, 66))
  expect_equal(list_calls, 1L)
  expect_equal(length(fetch_calls), 2L)

  res_wind <- AquaCache::downloadECCCwxMinute(
    location = "CVXY",
    parameter = "wind_spd",
    start_datetime = as.POSIXct("2026-03-15 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-03-15 00:01:59", tz = "UTC"),
    con = con
  )
  expect_equal(res_wind$value, c(10, 5))
  expect_equal(list_calls, 1L)
  expect_equal(length(fetch_calls), 2L)

  expect_true(file.exists(file.path(cache_dir, "CVXY_AUTO_2026-03-15.rds")))
})
