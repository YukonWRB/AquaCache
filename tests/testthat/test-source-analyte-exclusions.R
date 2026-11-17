test_that("profiles expose source exclusions as lists or JSON text", {
  queried <- NULL
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      queried <<- statement
      data.frame(
        excluded_analytes = '["Mercury"]',
        column_map = '{}',
        wide_config = '{}',
        defaults = '{}',
        sample_identity = '[]',
        result_identity = '[]',
        validation_rules = '{}'
      )
    },
    .package = "DBI"
  )
  parsed <- getImportProfiles(NULL)
  expect_identical(parsed$excluded_analytes[[1]], "Mercury")
  expect_match(queried, "array_to_json(s.excluded_analytes)", fixed = TRUE)
  expect_identical(
    getImportProfiles(NULL, parse_json = FALSE)$excluded_analytes,
    '["Mercury"]'
  )
})

test_that("source exclusion edits change the immutable configuration snapshot", {
  exclusions <- '["Mercury"]'
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      expect_match(statement, "array_to_json(excluded_analytes)", fixed = TRUE)
      data.frame(
        import_source_id = 1L,
        source_code = "LAB",
        source_name = "Lab",
        source_description = NA_character_,
        active = TRUE,
        excluded_analytes = exclusions
      )
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    getImportProfiles = function(...) data.frame(profile_code = "workbook"),
    getImportParameterMappings = function(...) data.frame(),
    getImportLocationMappings = function(...) data.frame(),
    getImportResultFlagMappings = function(...) data.frame(),
    .package = "AquaCache"
  )
  before <- getImportConfigurationSnapshot(NULL, "LAB", "workbook")
  expect_identical(before$source$excluded_analytes, "Mercury")
  exclusions <- '[]'
  after <- getImportConfigurationSnapshot(NULL, "LAB", "workbook")
  expect_identical(after$source$excluded_analytes, list())
  expect_false(identical(before, after))
})
