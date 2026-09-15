test_that("raster clip bounding boxes survive source-adapter JSON", {
  source_fx_args <- jsonlite::toJSON(
    list(clip = c(70, -142, 59, -123)),
    auto_unbox = TRUE
  )
  decoded <- source_adapter_args_decode(source_fx_args)

  expect_identical(decoded$clip, c(70L, -142L, 59L, -123L))
  expect_identical(
    raster_clip_normalize(decoded$clip),
    list(type = "bbox", value = c(70, -142, 59, -123))
  )

  named_json <- paste0(
    '{"clip":{"north":70,"west":-142,',
    '"south":59,"east":-123}}'
  )
  expect_identical(
    raster_clip_normalize(source_adapter_args_decode(named_json)$clip),
    list(type = "bbox", value = c(70, -142, 59, -123))
  )
})

test_that("raster source assignments serialize bounding boxes as JSON arrays", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = function(...) {
      data.frame(source_fx = "downloadERA5")
    },
    .package = "AquaCache"
  )
  assignments <- data.frame(
    source_fx = "downloadERA5",
    source_fx_args = I(list(list(
      param = "2m_temperature",
      clip = c(70, -142, 59, -123)
    ))),
    fetch_priority = 1L
  )

  normalized <- source_adapter_assignments_normalize(
    assignments,
    con = structure(list(), class = "mock_con"),
    data_domain = "raster"
  )
  stored <- jsonlite::fromJSON(normalized$source_fx_args[[1L]])

  expect_identical(stored$clip, c(70L, -142L, 59L, -123L))
})

test_that("raster clip validation is shared across downloaders", {
  expect_error(
    raster_clip_normalize(c(59, -142, 70, -123)),
    "north must be greater than south"
  )
  expect_error(
    raster_clip_normalize(c(70, -190, 59, -123)),
    "longitudes"
  )
  expect_error(
    raster_clip_normalize(c(north = 70, left = -142, south = 59, east = -123)),
    "must use north, west, south, east or xmin, xmax, ymin, ymax"
  )
})
