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

test_that("get_elevation returns an elevation", {
  res <- get_elevation(65, -135)

  expect_identical(class(res$elevation), "numeric")
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
