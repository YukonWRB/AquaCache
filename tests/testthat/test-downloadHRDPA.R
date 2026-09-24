test_that("downloadHRDPA supplies fallback units and missing issued metadata", {
  cache_dir <- file.path(tempdir(), "downloadHRDPA")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  now <- Sys.time()
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

  observed <- new.env(parent = emptyenv())
  observed$crop_count <- 0L

  local_mocked_bindings(
    rast = function(...) list("raw-raster"),
    units = function(...) NULL,
    project = function(...) "projected-raster",
    ext = function(x) {
      observed$extent <- x
      "bbox-extent"
    },
    crop = function(x, y) {
      observed$crop_count <- observed$crop_count + 1L
      observed$crop_extent <- y
      "cropped-raster"
    },
    .package = "terra"
  )

  result <- suppressMessages(downloadHRDPA(
    parameter = "APCP-Accum6h_Sfc",
    start_datetime = now - 3600,
    clip = c(70, -142, 59, -123)
  ))

  expect_equal(result[[1]]$units, "kg/(m^2)")
  expect_equal(result[[1]]$rast, "cropped-raster")
  expect_equal(observed$extent, c(-142, -123, 59, 70))
  expect_identical(observed$crop_extent, "bbox-extent")
  expect_identical(observed$crop_count, 1L)
  expect_false(result$forecast)
})
