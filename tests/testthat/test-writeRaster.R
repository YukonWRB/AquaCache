test_that("automatic raster tiling limits tile dimensions", {
  small <- terra::rast(ncols = 304, nrows = 151)
  current_hrdps <- terra::rast(ncols = 897, nrows = 386)
  large <- terra::rast(ncols = 1025, nrows = 1025)

  small_layout <- AquaCache:::raster_tile_layout(small)
  hrdps_layout <- AquaCache:::raster_tile_layout(current_hrdps)
  large_layout <- AquaCache:::raster_tile_layout(large)

  expect_equal(unname(small_layout$blocks), c(1L, 1L))
  expect_equal(unname(small_layout$tile_dimensions), c(304, 151))
  expect_equal(small_layout$tile_count, 1)
  expect_null(small_layout$tile_option)

  expect_equal(unname(hrdps_layout$blocks), c(2L, 1L))
  expect_equal(unname(hrdps_layout$tile_dimensions), c(449, 386))
  expect_equal(hrdps_layout$tile_count, 2)
  expect_equal(hrdps_layout$tile_option, "449x386")
  expect_equal(hrdps_layout$tile_ranges$columns$row, c(1L, 450L))
  expect_equal(hrdps_layout$tile_ranges$columns$nrows, c(449L, 448L))
  expect_equal(hrdps_layout$tile_ranges$rows$row, 1L)
  expect_equal(hrdps_layout$tile_ranges$rows$nrows, 386L)

  expect_equal(unname(large_layout$blocks), c(3L, 3L))
  expect_equal(unname(large_layout$tile_dimensions), c(342, 342))
  expect_equal(large_layout$tile_count, 9)
  expect_true(all(large_layout$tile_dimensions <= 512L))
  expect_equal(sum(large_layout$tile_ranges$columns$nrows), 1025L)
  expect_equal(sum(large_layout$tile_ranges$rows$nrows), 1025L)
})

test_that("raster tile layout validates its internal size limit", {
  raster <- terra::rast(ncols = 1025, nrows = 513)

  expect_error(
    AquaCache:::raster_tile_layout(raster, max_tile_size = 0),
    "positive integer"
  )
  expect_error(
    AquaCache:::raster_tile_layout(raster, max_tile_size = 1.5),
    "positive integer"
  )
  expect_error(
    AquaCache:::raster_tile_layout(raster, max_tile_size = NA),
    "positive integer"
  )

  custom <- AquaCache:::raster_tile_layout(raster, max_tile_size = 256)
  expect_equal(unname(custom$blocks), c(5L, 3L))
  expect_equal(unname(custom$tile_dimensions), c(205, 171))
  expect_equal(custom$tile_count, 15)
  expect_equal(custom$tile_option, "205x171")
})

test_that("raster writers no longer expose manual block controls", {
  raster_functions <- list(
    AquaCache::writeRaster,
    AquaCache::writeRaster_old,
    AquaCache::insertACModelRaster,
    AquaCache::insertACRaster
  )

  expect_true(all(vapply(
    raster_functions,
    function(fun) !"blocks" %in% names(formals(fun)),
    logical(1)
  )))
})
