test_that("downloadCaLDAS crops to a JSON-compatible bounding box", {
  cache_dir <- file.path(tempdir(), "downloadCaLDAS")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  now <- Sys.time()
  available <- data.frame(
    file = "20260911T1200_test.nc",
    datetime = now - 60,
    prelim = FALSE,
    path = "https://example.test/caldas.nc"
  )
  saveRDS(
    available,
    file.path(cache_dir, format(Sys.time(), "%Y%m%d%H%M.rds"))
  )

  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    curl_download = function(...) invisible(NULL),
    .package = "curl"
  )
  local_mocked_bindings(
    rast = function(...) list("raw-raster"),
    units = function(...) "K",
    project = function(...) "projected-raster",
    ext = function(x) {
      observed$extent <- x
      "bbox-extent"
    },
    crop = function(x, y) {
      observed$crop_extent <- y
      "cropped-raster"
    },
    .package = "terra"
  )

  result <- suppressMessages(downloadCaLDAS(
    parameter = "AirTemp_AGL-1.5m",
    start_datetime = now - 3600,
    clip = c(70, -142, 59, -123)
  ))

  expect_equal(result[[1]]$units, "K")
  expect_equal(result[[1]]$rast, "cropped-raster")
  expect_equal(observed$extent, c(-142, -123, 59, 70))
  expect_identical(observed$crop_extent, "bbox-extent")
  expect_false(result$forecast)
})
