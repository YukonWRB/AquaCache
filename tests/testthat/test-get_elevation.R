test_that("get_elevation validates coordinates", {
  expect_error(get_elevation(-100, 100), "'lat' must be between -90 and 90.")
  expect_error(get_elevation(50, 300), "'lon' must be between -180 and 180.")
  expect_no_error(get_elevation(50, -120))
})

test_that("get_elevation validates sources", {
  expect_error(
    get_elevation(50, -120, source = "not_a_source"),
    "Unknown elevation source(s)*"
  )
})

test_that("get_elevation returns the expected structure", {
  res <- get_elevation(65, -135)

  expect_identical(class(res$elevation), "numeric")
  expect_identical(class(res$elevation_units), "character")
  expect_identical(class(res$source), "character")
  expect_identical(class(res$elevation), "numeric")
  expect_identical(class(res$resolution_x), "numeric")
  expect_identical(class(res$resolution_y), "numeric")
  expect_identical(class(res$vertical_datum), "character")

  expect_named(
    res,
    c(
      "elevation",
      "elevation_units",
      "source",
      "resolution_x",
      "resolution_y",
      "resolution_units",
      "vertical_datum"
    )
  )
})

test_that("ArcticDEM retries low-level request failures", {
  request <- AquaCache:::.elevation_arcticdem_request(
    lat = 64,
    lon = -135
  )

  expect_equal(
    request$policies$retry_max_tries,
    3
  )
  expect_true(
    request$policies$retry_on_failure
  )
  expect_identical(class(request)[[1]], "httr2_request")
})

test_that("get_elevation returns the correct ArcticDEM result", {
  res <- expect_no_error(get_elevation(
    65.12345,
    -130.12345,
    source = "arcticdem"
  ))
  expect_identical(res$elevation, 968.828)
  res_direct <- expect_no_error(.elevation_arcticdem(65.12345, -130.12345))
  expect_identical(res$elevation, res_direct$elevation)
})

test_that("get_elevation returns the correct usgs result", {
  # USGS seems to presistently reject automated calls
  skip_on_ci()
  skip_on_cran()
  res <- expect_no_error(get_elevation(35, -100, source = "usgs"))
  expect_identical(round(res$elevation, 2), 560.97)
  res_direct <- expect_no_error(.elevation_usgs(35, -100))
  expect_identical(res$elevation, res_direct$elevation)
})

test_that("get_elevation returns the correct cdem result", {
  res <- expect_no_error(get_elevation(55.1234, -120.1234, source = "cdem"))
  expect_identical(res$elevation, 994)
  res_direct <- expect_no_error(.elevation_geogratis(
    55.1234,
    -120.1234,
    source = "cdem"
  ))
  expect_identical(res$elevation, res_direct$elevation)
})

test_that("get_elevation returns the correct cdsm result", {
  res <- expect_no_error(get_elevation(55.1234, -120.1234, source = "cdsm"))
  expect_identical(res$elevation, 995.284)
  res_direct <- expect_no_error(.elevation_geogratis(
    55.1234,
    -120.1234,
    source = "cdsm"
  ))
  expect_identical(res$elevation, res_direct$elevation)
})
