mock_assignment_capabilities <- function(...) {
  data.table::data.table(
    source_fx = c("downloadNESDIS", "downloadRWIS")
  )
}

test_that("source assignments retain independent fetch and synchronize priorities", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = mock_assignment_capabilities,
    .package = "AquaCache"
  )

  assignments <- data.frame(
    source_fx = c("downloadNESDIS", "downloadRWIS"),
    fetch_priority = c(1L, NA_integer_),
    synchronize_priority = c(NA_integer_, 1L),
    active = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  assignments$source_fx_args <- list(
    list(cache = TRUE),
    list(location = "TEST", parameter = "TA")
  )

  normalized <- AquaCache:::source_adapter_assignments_normalize(
    assignments,
    con = structure(list(), class = "mock_con"),
    data_domain = "continuous"
  )

  expect_equal(normalized$fetch_priority, c(1L, NA_integer_))
  expect_equal(normalized$synchronize_priority, c(NA_integer_, 1L))
  expect_equal(
    jsonlite::fromJSON(normalized$source_fx_args[[2L]])$location,
    "TEST"
  )
})

test_that("inactive assignments may retain priorities used by active rows", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = mock_assignment_capabilities,
    .package = "AquaCache"
  )

  assignments <- data.frame(
    source_fx = c("downloadNESDIS", "downloadRWIS"),
    fetch_priority = c(1L, 1L),
    synchronize_priority = c(1L, 1L),
    active = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  expect_no_error(AquaCache:::source_adapter_assignments_normalize(
    assignments,
    con = structure(list(), class = "mock_con"),
    data_domain = "continuous"
  ))

  assignments$active <- TRUE
  expect_error(
    AquaCache:::source_adapter_assignments_normalize(
      assignments,
      con = structure(list(), class = "mock_con"),
      data_domain = "continuous"
    ),
    "cannot repeat fetch_priority"
  )
})

test_that("every assignment has at least one operation priority", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = mock_assignment_capabilities,
    .package = "AquaCache"
  )

  expect_error(
    AquaCache:::source_adapter_assignments_normalize(
      data.frame(
        source_fx = "downloadRWIS",
        fetch_priority = NA_integer_,
        synchronize_priority = NA_integer_,
        active = FALSE
      ),
      con = structure(list(), class = "mock_con"),
      data_domain = "continuous"
    ),
    "needs a fetch_priority"
  )
})

test_that("image and raster assignments require fetch priorities only", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = mock_assignment_capabilities,
    .package = "AquaCache"
  )

  image_assignment <- data.frame(
    source_fx = "downloadRWIS",
    fetch_priority = 1L,
    active = TRUE
  )
  normalized <- AquaCache:::source_adapter_assignments_normalize(
    image_assignment,
    con = structure(list(), class = "mock_con"),
    data_domain = "image"
  )
  expect_equal(normalized$fetch_priority, 1L)
  expect_true(is.na(normalized$synchronize_priority))

  image_assignment$synchronize_priority <- 1L
  expect_error(
    AquaCache:::source_adapter_assignments_normalize(
      image_assignment,
      con = structure(list(), class = "mock_con"),
      data_domain = "raster"
    ),
    "do not support synchronization priorities"
  )
})

test_that("image and raster assignment inserts use their child tables", {
  local_mocked_bindings(
    getSourceAdapterCapabilities = mock_assignment_capabilities,
    .package = "AquaCache"
  )
  statements <- character()
  local_mocked_bindings(
    dbGetQuery = function(con, statement, params, ...) {
      statements <<- c(statements, statement)
      data.frame(id = length(statements))
    },
    .package = "DBI"
  )
  assignment <- data.frame(
    source_fx = "downloadRWIS",
    fetch_priority = 1L,
    active = TRUE
  )

  AquaCache:::source_adapter_assignments_insert(
    con = structure(list(), class = "mock_con"),
    data_domain = "image",
    series_id = 10L,
    assignments = assignment
  )
  AquaCache:::source_adapter_assignments_insert(
    con = structure(list(), class = "mock_con"),
    data_domain = "raster",
    series_id = 20L,
    assignments = assignment
  )

  expect_match(statements[[1L]], "files.image_series_source_adapters", fixed = TRUE)
  expect_match(statements[[2L]], "spatial.raster_series_source_adapters", fixed = TRUE)
  expect_false(any(grepl("synchronize_priority", statements, fixed = TRUE)))
})
