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
      stop("<html><title>502 Bad Gateway</title><body>nginx</body></html>")
    },
    wf_request = function(...) stop("502 Bad Gateway"),
    .package = "ecmwfr"
  )

  expect_message(
    expect_error(
      downloadERA5(
        start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        clip = "YT",
        param = "snow_depth",
        user = "legacy-user",
        key = "request-token",
        max_attempts = 1L,
        retry_delay = 0
      ),
      "No data was downloaded"
    ),
    "ERA5 batch request ended early: HTTP 502 Bad Gateway"
  )

  expect_identical(observed$token, "request-token")
  expect_identical(Sys.getenv("ecmwfr_PAT"), "previous-token")
  expect_identical(observed$workers, 1L)
  expect_identical(observed$retry, 5)
  expect_length(observed$requests, 1)
  expect_identical(observed$requests[[1]]$data_format, "netcdf")
  expect_identical(observed$requests[[1]]$download_format, "unarchived")
  expect_false("format" %in% names(observed$requests[[1]]))
  expect_false("date" %in% names(observed$requests[[1]]))
  expect_identical(observed$requests[[1]]$year, "2026")
  expect_identical(observed$requests[[1]]$month, "01")
  expect_identical(observed$requests[[1]]$day, "01")
  expect_identical(observed$requests[[1]]$target, "ERA5_sd_2026010100.nc")
  expect_equal(observed$requests[[1]]$area, c(70, -142, 59, -123))
  expect_null(names(observed$requests[[1]]$area))
})

test_that("downloadERA5 accepts numeric and SpatExtent bounding boxes", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  observed_areas <- list()
  extent <- terra::ext(c(-142, -123, 59, 70))

  local_mocked_bindings(
    vect = function(...) stop("Province geometry should not be loaded"),
    .package = "terra"
  )
  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    wf_request_batch = function(request_list, ...) {
      observed_areas[[length(observed_areas) + 1L]] <<-
        request_list[[1L]]$area
      stop("download test sentinel")
    },
    wf_request = function(...) stop("download test sentinel"),
    .package = "ecmwfr"
  )

  bounding_boxes <- list(
    c(70, -142, 59, -123),
    c(east = -123, south = 59, north = 70, west = -142),
    c(xmax = -123, ymin = 59, xmin = -142, ymax = 70),
    extent
  )
  for (bounding_box in bounding_boxes) {
    expect_error(
      downloadERA5(
        start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        clip = bounding_box,
        param = "2m_temperature",
        max_attempts = 1L,
        retry_delay = 0
      ),
      "No data was downloaded"
    )
  }

  expect_length(observed_areas, 4L)
  for (area in observed_areas) {
    expect_identical(area, c(70, -142, 59, -123))
    expect_null(names(area))
  }
})

test_that("downloadERA5 validates bounding boxes before requesting data", {
  start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")

  expect_error(
    downloadERA5(
      start_datetime = start,
      end_datetime = start,
      clip = c(59, -142, 70, -123),
      param = "2m_temperature",
      key = "test-token"
    ),
    "north must be greater than south"
  )
  expect_error(
    downloadERA5(
      start_datetime = start,
      end_datetime = start,
      clip = c(left = -142, right = -123, bottom = 59, top = 70),
      param = "2m_temperature",
      key = "test-token"
    ),
    "must use north, west, south, east or xmin, xmax, ymin, ymax"
  )
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
    wf_request = function(...) stop("download test sentinel"),
    .package = "ecmwfr"
  )

  expect_error(
    downloadERA5(
      start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      end_datetime = as.POSIXct("2026-01-31 00:00:00", tz = "UTC"),
      clip = NULL,
      param = "total_precipitation",
      hrs = 0,
      max_attempts = 1L,
      retry_delay = 0
    ),
    "No data was downloaded"
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

test_that("downloadERA5 combines instantaneous hours into monthly requests", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
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
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
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
    wf_request = function(...) stop("download test sentinel"),
    .package = "ecmwfr"
  )

  expect_error(
    downloadERA5(
      start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
      end_datetime = as.POSIXct("2026-01-31 23:00:00", tz = "UTC"),
      clip = "YT",
      param = "2m_temperature",
      hrs = "c(0:23)",
      batch = "TRUE",
      max_attempts = 1L,
      retry_delay = 0
    ),
    "No data was downloaded"
  )

  expect_length(observed$requests, 1L)
  request <- observed$requests[[1L]]
  expect_identical(request$time, sprintf("%02d:00", 0:23))
  expect_identical(
    request$target,
    "ERA5_2t_2026010100_to_2026013123.nc"
  )
  expect_identical(request$year, "2026")
  expect_identical(request$month, "01")
  expect_identical(request$day, sprintf("%02d", 1:31))
})

test_that("downloadERA5 retries a transient sequential submission failure", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  attempts <- 0L
  raster_template <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = -142,
    xmax = -141,
    ymin = 59,
    ymax = 60,
    vals = 1
  )

  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    rast = function(...) raster_template,
    .package = "terra"
  )
  local_mocked_bindings(
    wf_request = function(request, path, ...) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        stop("502 Bad Gateway")
      }
      file <- file.path(path, request$target)
      job <- new.env(parent = emptyenv())
      job$download <- function(...) invisible(job)
      job$is_success <- function() TRUE
      job$get_file <- function() file
      job$get_status <- function() "successful"
      job$delete <- function() invisible(job)
      job
    },
    .package = "ecmwfr"
  )

  result <- downloadERA5(
    start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    clip = NULL,
    param = "2m_temperature",
    batch = FALSE,
    max_attempts = 2L,
    retry_delay = 0,
    request_timeout = 1
  )

  expect_identical(attempts, 2L)
  expect_length(result, 2L)
  expect_equal(
    result[[1L]]$valid_to,
    as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  expect_identical(result$forecast, FALSE)
})

test_that("downloadERA5 retries download from the same completed CDS job", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  submissions <- 0L
  transfers <- 0L
  raster_template <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = -142,
    xmax = -141,
    ymin = 59,
    ymax = 60,
    vals = 1
  )

  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    rast = function(...) raster_template,
    .package = "terra"
  )
  local_mocked_bindings(
    wf_request = function(request, path, ...) {
      submissions <<- submissions + 1L
      file <- file.path(path, request$target)
      job <- new.env(parent = emptyenv())
      job$download <- function(...) {
        transfers <<- transfers + 1L
        if (transfers == 1L) stop("502 Bad Gateway")
        invisible(job)
      }
      job$is_success <- function() transfers > 1L
      job$get_file <- function() file
      job$get_status <- function() "successful"
      job$delete <- function() invisible(job)
      job
    },
    .package = "ecmwfr"
  )

  result <- downloadERA5(
    start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    clip = NULL,
    param = "2m_temperature",
    batch = FALSE,
    max_attempts = 2L,
    retry_delay = 0,
    request_timeout = 1
  )

  expect_identical(submissions, 1L)
  expect_identical(transfers, 2L)
  expect_length(result, 2L)
})

test_that("downloadERA5 polls pending jobs and applies one total timeout", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  polls <- 0L

  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    wf_request = function(...) {
      job <- new.env(parent = emptyenv())
      job$download <- function(...) {
        polls <<- polls + 1L
        invisible(job)
      }
      job$is_success <- function() FALSE
      job$get_status <- function() "queued"
      job$delete <- function() invisible(job)
      job
    },
    .package = "ecmwfr"
  )

  expect_message(
    expect_error(
      downloadERA5(
        start_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        end_datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
        clip = NULL,
        param = "2m_temperature",
        batch = FALSE,
        request_timeout = 0.05,
        poll_interval = 0.01
      ),
      "No data was downloaded"
    ),
    "is in CDS status 'queued'"
  )
  expect_gte(polls, 1L)
  expect_lt(polls, 20L)
})

test_that("downloadERA5 returns only a complete prefix after partial batch failure", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  observed <- new.env(parent = emptyenv())
  raster_template <- terra::rast(
    nrows = 1,
    ncols = 1,
    nlyrs = 2,
    xmin = -142,
    xmax = -141,
    ymin = 59,
    ymax = 60
  )
  terra::values(raster_template) <- matrix(c(1, 2), nrow = 1)

  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    vect = function(...) data.frame(PREABBR = "YT"),
    project = function(x, ...) x,
    ext = function(...) list(ymax = 70, xmin = -142, ymin = 59, xmax = -123),
    rast = function(...) raster_template,
    .package = "terra"
  )
  local_mocked_bindings(
    wf_request_batch = function(request_list, path, ...) {
      observed$requests <- request_list
      file.create(file.path(path, request_list[[1L]]$target))
      file.create(file.path(path, request_list[[3L]]$target))
      stop("502 Bad Gateway")
    },
    wf_request = function(...) stop("502 Bad Gateway"),
    .package = "ecmwfr"
  )

  result <- suppressMessages(downloadERA5(
    start_datetime = as.POSIXct("2026-01-31 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-03-01 01:00:00", tz = "UTC"),
    clip = "YT",
    param = "2m_temperature",
    hrs = 0:1,
    batch = TRUE,
    max_attempts = 1L,
    retry_delay = 0
  ))

  expect_length(observed$requests, 3L)
  expect_length(result, 3L)
  expect_equal(
    unname(vapply(
      result[1:2],
      function(x) as.numeric(x$valid_to),
      numeric(1)
    )),
    as.numeric(as.POSIXct("2026-01-31 00:00:00", tz = "UTC")) + 0:1 * 3600
  )
  expect_identical(result$forecast, FALSE)
})

test_that("downloadERA5 uses NetCDF times to stop before an omitted layer", {
  withr::local_envvar(c(ecmwfr_PAT = "test-token"))
  start <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  raster_template <- terra::rast(
    nrows = 1,
    ncols = 1,
    nlyrs = 2,
    xmin = -142,
    xmax = -141,
    ymin = 59,
    ymax = 60
  )
  terra::values(raster_template) <- matrix(c(1, 3), nrow = 1)
  terra::time(raster_template) <- start + c(0, 2) * 3600

  local_mocked_bindings(
    read_html = function(...) structure(list(), class = "era5_test_page"),
    html_table = function(...) {
      list(data.frame(
        `Variable name in CDS` = "2m_temperature",
        shortName = "2t",
        check.names = FALSE
      ))
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    vect = function(...) data.frame(PREABBR = "YT"),
    project = function(x, ...) x,
    ext = function(...) list(ymax = 70, xmin = -142, ymin = 59, xmax = -123),
    rast = function(...) raster_template,
    .package = "terra"
  )
  local_mocked_bindings(
    wf_request_batch = function(request_list, path, ...) {
      file.path(path, request_list[[1L]]$target)
    },
    .package = "ecmwfr"
  )

  result <- suppressMessages(downloadERA5(
    start_datetime = start,
    end_datetime = start + 2 * 3600,
    clip = "YT",
    param = "2m_temperature",
    hrs = 0:2,
    batch = TRUE
  ))

  expect_length(result, 2L)
  expect_equal(result[[1L]]$valid_to, start)
  expect_equal(terra::values(result[[1L]]$rast)[1], 1)
  expect_identical(result$forecast, FALSE)
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
