test_that("generateACLocationCode returns NHN-based codes", {
  skip_on_ci()
  skip_on_cran()

  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  nhn_count <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS count
    FROM spatial.vectors
    WHERE layer_name = 'National Hydro Network - Basins';
    "
  )[1, 1]
  if (nhn_count < 1338) {
    skip("NHN basins are not available in the test database.")
  }

  sample_poly <- DBI::dbGetQuery(
    con,
    "
    SELECT
      feature_name,
      ST_Y(ST_Centroid(geom)) AS latitude,
      ST_X(ST_Centroid(geom)) AS longitude
    FROM spatial.vectors
    WHERE layer_name = 'National Hydro Network - Basins'
    LIMIT 1;
    "
  )

  prefix <- sub("^([0-9]{2})([A-Za-z]{2,3}).*$", "\\1\\2", sample_poly$feature_name)
  if (identical(prefix, sample_poly$feature_name)) {
    skip("NHN basin name does not match expected prefix pattern.")
  }

  location_type <- DBI::dbGetQuery(
    con,
    "SELECT type_id, type_suffix FROM location_types WHERE type_suffix IS NOT NULL LIMIT 1;"
  )
  if (nrow(location_type) == 0) {
    skip("No location type suffix available in the test database.")
  }

  codes <- generateACLocationCode(
    latitude = sample_poly$latitude,
    longitude = sample_poly$longitude,
    location_type = location_type$type_id,
    con = con,
    ask = FALSE
  )

  expect_type(codes, "character")
  expect_length(codes, 1)
  expected_prefix <- paste0(prefix, "-", location_type$type_suffix)
  expect_match(codes, paste0("^", expected_prefix, "-\\d{5}$"))
})
