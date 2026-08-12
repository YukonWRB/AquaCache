test_that("downloadHRDPA supplies fallback units and missing issued metadata", {
  cache_dir <- file.path(tempdir(), "downloadHRDPA")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  now <- as.POSIXct("2026-08-12 12:00:00", tz = "UTC")
  available <- data.frame(
    file = "20260812T1159_test.grib2",
    datetime = now - 60,
    prelim = FALSE,
    path = "https://example.test/hrdpa.grib2"
  )
  saveRDS(
    available,
    file.path(cache_dir, format(Sys.time(), "%Y%m%d%H%M.rds"))
  )

  local_mocked_bindings(
    rast = function(...) list("raw-raster"),
    units = function(...) NULL,
    project = function(...) "projected-raster",
    .package = "terra"
  )

  result <- suppressMessages(downloadHRDPA(
    parameter = "APCP-Accum6h_Sfc",
    start_datetime = now - 3600
  ))

  expect_equal(result[[1]]$units, "kg/(m^2)")
  expect_true(is.na(result[[1]]$issued))
  expect_equal(result[[1]]$rast, "projected-raster")
  expect_false(result$forecast)
})
