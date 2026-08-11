empty_source_adapter_capabilities <- function(...) {
  data.table::data.table(source_fx = character())
}

test_that("getNewImages rejects unregistered image adapters", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = empty_source_adapter_capabilities,
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbExecute = function(...) invisible(0L),
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM files.image_series", statement, fixed = TRUE)) {
        return(data.frame(
          img_series_id = 1L,
          last_img = as.POSIXct("2026-01-01", tz = "UTC"),
          source_fx = "downloadWSCImages",
          source_fx_args = NA_character_,
          active = TRUE,
          location_id = 1L
        ))
      }
      stop("Unexpected database query in image-domain enforcement test.")
    },
    .package = "DBI"
  )

  expect_error(
    getNewImages(
      image_series_ids = "all",
      con = structure(list(), class = "mock_con")
    ),
    "image domain"
  )
})

test_that("getNewRasters rejects unregistered raster adapters", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = empty_source_adapter_capabilities,
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbExecute = function(...) invisible(0L),
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM spatial.raster_series_index", statement, fixed = TRUE)) {
        return(data.frame(
          raster_series_id = 1L,
          end_datetime = as.POSIXct("2026-01-01", tz = "UTC"),
          last_issue = as.POSIXct(NA, tz = "UTC"),
          type = "reanalysis",
          source_fx = "downloadCaLDAS",
          source_fx_args = NA_character_,
          parameter_name = "snow water equivalent",
          active = TRUE
        ))
      }
      stop("Unexpected database query in raster-domain enforcement test.")
    },
    .package = "DBI"
  )

  expect_error(
    getNewRasters(
      raster_series_ids = "all",
      con = structure(list(), class = "mock_con")
    ),
    "raster domain"
  )
})

test_that("getNewDiscrete rejects unregistered discrete adapters", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = empty_source_adapter_capabilities,
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbExecute = function(...) invisible(0L),
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM discrete.sample_series", statement, fixed = TRUE)) {
        return(data.frame(
          sample_series_id = 1L,
          source_fx = "downloadECCCwq",
          active = TRUE,
          stringsAsFactors = FALSE
        ))
      }
      stop("Unexpected database query in discrete-domain enforcement test.")
    },
    .package = "DBI"
  )

  expect_error(
    getNewDiscrete(
      sample_series_id = 1L,
      con = structure(list(), class = "mock_con")
    ),
    "discrete domain"
  )
})
