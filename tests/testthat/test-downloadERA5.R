test_that("downloadERA5 bypasses keyring and builds current CDS requests", {
  withr::local_envvar(c(ecmwfr_PAT = "previous-token"))
  observed <- new.env(parent = emptyenv())

  local_mocked_bindings(
    vect = function(...) data.frame(PREABBR = "YT"),
    project = function(x, ...) x,
    ext = function(...) list(ymax = 70, xmin = -142, ymin = 59, xmax = -123),
    .package = "terra"
  )
  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "snow_depth",
        shortName = "sd",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    wf_set_key = function(...) stop("wf_set_key should not be called"),
    wf_request_batch = function(request_list, workers, retry, ...) {
      observed$token <- Sys.getenv("ecmwfr_PAT")
      observed$requests <- request_list
      observed$workers <- workers
      observed$retry <- retry
      stop("download test sentinel")
    },
    .package = "ecmwfr"
  )

  expect_error(
    downloadERA5(
      start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      clip = "YT",
      param = "snow_depth",
      user = "legacy-user",
      key = "request-token"
    ),
    "download test sentinel"
  )

  expect_identical(observed$token, "request-token")
  expect_identical(Sys.getenv("ecmwfr_PAT"), "previous-token")
  expect_identical(observed$workers, 1L)
  expect_identical(observed$retry, 5)
  expect_length(observed$requests, 1)
  expect_identical(observed$requests[[1]]$data_format, "netcdf")
  expect_identical(observed$requests[[1]]$download_format, "unarchived")
  expect_false("format" %in% names(observed$requests[[1]]))
  expect_identical(observed$requests[[1]]$target, "ERA5_sd_2026010100.nc")
  expect_equal(observed$requests[[1]]$area, c(70, -142, 59, -123))
})

test_that("downloadERA5 converts forecast accumulations to hourly rasters", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  observed <- new.env(parent = emptyenv())
  accumulated_values <- c(
    ERA5_tp_2025123123.nc = 10,
    ERA5_tp_2026010100.nc = 13,
    ERA5_tp_2026010101.nc = 4,
    ERA5_tp_2026010102.nc = 9
  )
  raster_templates <- lapply(accumulated_values, function(value) {
    terra::rast(
      nrows = 1,
      ncols = 1,
      xmin = -142,
      xmax = -141,
      ymin = 59,
      ymax = 60,
      vals = value
    )
  })

  local_mocked_bindings(
    vect = function(...) data.frame(PREABBR = "YT"),
    project = function(x, ...) x,
    ext = function(...) list(ymax = 70, xmin = -142, ymin = 59, xmax = -123),
    rast = function(filename) raster_templates[[basename(filename)]],
    .package = "terra"
  )
  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "total_precipitation",
        shortName = "tp",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    wf_request_batch = function(request_list, path, ...) {
      observed$requests <- request_list
      file.path(path, vapply(request_list, `[[`, character(1), "target"))
    },
    .package = "ecmwfr"
  )

  result <- downloadERA5(
    start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-01-01 02:00:00", tz = "UTC"),
    clip = "YT",
    param = "total_precipitation",
    hrs = 0:2
  )

  expect_length(result, 4)
  expect_identical(
    vapply(observed$requests, `[[`, character(1), "target"),
    names(accumulated_values)
  )
  expect_equal(
    unname(vapply(
      result[1:3],
      function(x) terra::values(x$rast)[1],
      numeric(1)
    )),
    c(3, 4, 5)
  )
  expect_equal(
    unname(vapply(
      result[1:3],
      function(x) as.numeric(x$valid_to),
      numeric(1)
    )),
    as.numeric(as.POSIXct("2026-01-01 00:00:00", tz = "UTC")) + 0:2 * 3600
  )
  expect_identical(result$forecast, FALSE)
})

test_that("downloadERA5 groups accumulated predecessor hours efficiently", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  observed <- new.env(parent = emptyenv())

  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "total_precipitation",
        shortName = "tp",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    wf_request_batch = function(request_list, ...) {
      observed$requests <- request_list
      stop("download test sentinel")
    },
    .package = "ecmwfr"
  )

  expect_error(
    downloadERA5(
      start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      end_datetime = as.POSIXct("2026-01-31 00:00:00", tz = "UTC"),
      clip = NULL,
      param = "total_precipitation",
      hrs = 0
    ),
    "download test sentinel"
  )

  expect_identical(
    vapply(observed$requests, `[[`, character(1), "target"),
    c(
      "ERA5_tp_2025123123.nc",
      "ERA5_tp_2026010100_to_2026013100.nc",
      "ERA5_tp_2026010123_to_2026013023.nc"
    )
  )
})

test_that("downloadERA5 rejects a missing ECMWF token before downloading", {
  expect_error(
    downloadERA5(
      start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      clip = NULL,
      param = "total_precipitation",
      key = ""
    ),
    "No ECMWF API token was supplied"
  )
})
