test_that("downloadHRDPS compacts unavailable forecast-hour files", {
  hour_links <- sprintf("%03d/", 0:48)

  local_mocked_bindings(
    session = function(url) url,
    html_elements = function(x, ...) x,
    html_attr = function(x, ...) {
      if (grepl("/[0-9]{2}/$", x)) hour_links else "00/"
    },
    .package = "rvest"
  )
  local_mocked_bindings(
    HEAD = function(url, ...) url,
    http_error = function(response) grepl("PT000H", response, fixed = TRUE),
    .package = "httr"
  )
  local_mocked_bindings(
    rast = function(x) list(structure(x, class = "mock_raster")),
    units = function(x) "mm",
    project = function(x, ...) x,
    .package = "terra"
  )

  expect_message(
    result <- downloadHRDPS(
      parameter = "APCP-Accum1h_Sfc",
      start_datetime = as.POSIXct("2000-01-01", tz = "UTC")
    ),
    "finished downloading 48 new rasters; 1 of 49 candidate files was unavailable"
  )

  issue <- result$issued
  result$forecast <- NULL
  result$issued <- NULL

  expect_length(result, 48L)
  expect_match(result[[1]]$source, "PT001H.grib2", fixed = TRUE)
  expect_match(result[[48]]$source, "PT048H.grib2", fixed = TRUE)
  expect_equal(result[[1]]$valid_from, issue + 3600)
  expect_equal(result[[48]]$valid_to, issue + 49 * 3600)
  expect_false(any(vapply(result, is.null, logical(1))))
})
