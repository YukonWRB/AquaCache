test_that("historical chemistry enrichment preserves an unambiguous result identity", {
  stored <- data.frame(result_id = c(101L, 102L), parameter_id = 4L,
    result_type = 2L, sample_fraction_id = c(5L, 19L), result = c(0.2, 0.3),
    reporting_basis_id = NA_integer_, result_speciation_id = NA_integer_)
  remote <- stored[1, setdiff(names(stored), "result_id")]
  remote$reporting_basis_id <- 34L
  remote$result_speciation_id <- 4L
  match <- match_discrete_import_results(stored, remote)
  expect_identical(match$result_ids, 101L)
  expect_false(match$ambiguous)
  remote$result <- 0.8
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
  stored$reporting_basis_id[[1]] <- 2L
  remote$result <- 0.2
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
})

test_that("batch matching never lets source order resolve ambiguous unknowns", {
  stored <- data.frame(result_id = 101L, parameter_id = 4L,
    result_type = 2L, sample_fraction_id = NA_integer_, result = 0.2)
  remote <- data.frame(parameter_id = 4L, result_type = 2L,
    sample_fraction_id = c(5L, 19L), result = 0.2)
  match <- match_discrete_import_results(stored, remote)
  expect_true(all(match$ambiguous))
  expect_true(all(is.na(match$result_ids)))
  expect_identical(match$retained_ids, 101L)
  expect_identical(match_discrete_import_results(stored, remote[2:1, ])$ambiguous, c(TRUE, TRUE))
  # An already exact counterpart takes priority over a broader unknown row.
  stored <- rbind(stored, transform(stored, result_id = 102L, sample_fraction_id = 5L))
  expect_identical(match_discrete_import_results(stored, remote[1, ])$result_ids, 102L)
})

test_that("condition refinement preserves known values and array interpretation", {
  stored <- data.frame(result_id = 101L, parameter_id = 4L, result_type = 2L, result = 1,
    analysis_conditions = '{"mass_basis":"dry","reference":{"temperature":298.15}}')
  remote <- stored[, setdiff(names(stored), "result_id")]
  remote$analysis_conditions <- I(list(list(reference = list(pressure = 101.325, temperature = 298.15), mass_basis = "dry")))
  expect_identical(match_discrete_import_results(stored, remote)$result_ids, 101L)
  remote$analysis_conditions[[1]]$mass_basis <- "wet"
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
  stored$analysis_conditions <- '{"custom":[1,2]}'
  remote$analysis_conditions <- '{"custom":[2,1]}'
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
  stored$analysis_conditions <- '{"custom":[]}'
  remote$analysis_conditions <- '{"custom":[1]}'
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
  remote$analysis_conditions <- '{"custom":{}}'
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
  stored$analysis_conditions <- '{"custom":{}}'
  remote$analysis_conditions <- '{"custom":{"reference":1}}'
  expect_identical(match_discrete_import_results(stored, remote)$result_ids, 101L)
})

test_that("derived parents can gain metadata without comparing a recomputed value to NULL", {
  stored <- data.frame(result_id = 101L, parameter_id = 4L, result_type = 2L,
    result = 10, is_aggregated = TRUE, reporting_basis_id = NA_integer_)
  remote <- data.frame(parameter_id = 4L, result_type = 2L, result = NA_real_, reporting_basis_id = 3L)
  expect_identical(match_discrete_import_results(stored, remote, 1L)$result_ids, 101L)
  expect_true(is.na(match_discrete_import_results(stored, remote)$result_ids))
  empty <- stored[FALSE, ]
  expect_identical(match_discrete_import_results(empty, remote)$result_ids, NA_integer_)
})

test_that("partial filtering remaps aggregation parents and their components", {
  local_mocked_bindings(
    dbTransCheck = function(...) FALSE,
    normalize_discrete_result_matrix_states = function(con, sample_media_id, results) {
      results$matrix_state_id <- 1L
      results
    },
    validateResultChemistry = function(con, results, ...) ifelse(results$parameter_id == 2L, "Endpoint missing", ""),
    .package = "AquaCache"
  )
  local_mocked_bindings(dbGetQuery = function(con, statement, ...) {
    if (grepl("AS field", statement, fixed = TRUE)) return(data.frame(
      field = c("result_type", "matrix_state_id"), id = c(2L, 1L)))
    if (grepl("get_parameter_unit_id", statement, fixed = TRUE)) return(data.frame(row_index = c(1L, 3L), valid = TRUE))
    data.frame(result_type_id = 1L)
  }, .package = "DBI")
  results <- data.frame(parameter_id = 1:3, result_type = 2L, result = c(10, 20, NA_real_))
  aggregations <- data.frame(result_row = c(2L, 3L), aggregation_type = "arithmetic_mean")
  components <- data.frame(result_row = c(2L, 3L), observation_number = 1L, result = c(20, 30))
  prepared <- prepare_discrete_import_results(NULL, 1L, results, aggregations, components)
  expect_identical(prepared$results$parameter_id, c(1L, 3L))
  expect_identical(prepared$result_aggregations$result_row, 2L)
  expect_identical(prepared$result_components$result_row, 2L)
  expect_identical(prepared$result_components$result, 30)
  expect_identical(prepared$rejected_results$parameter_id, 2L)
  expect_identical(results$parameter_id, 1:3)
})

test_that("a malformed reserved condition does not discard an independent valid result", {
  real_validate <- validateResultChemistry
  local_mocked_bindings(dbTransCheck = function(...) FALSE,
    normalize_discrete_result_matrix_states = function(con, sample_media_id, results) {
      results$matrix_state_id <- 1L
      results
    },
    validateResultChemistry = function(con, results, ...) real_validate(NULL, results,
      parameters = data.frame(parameter_id = 1L), error = FALSE), .package = "AquaCache")
  local_mocked_bindings(dbGetQuery = function(con, statement, ...) {
    if (grepl("AS field", statement, fixed = TRUE)) return(data.frame(
      field = c("result_type", "matrix_state_id"), id = c(2L, 1L)))
    if (grepl("get_parameter_unit_id", statement, fixed = TRUE)) return(data.frame(row_index = 2L, valid = TRUE))
    data.frame(result_type_id = 1L)
  }, .package = "DBI")
  results <- data.frame(parameter_id = 1L, result_type = 2L, result = c(10,20))
  results$analysis_conditions <- I(list(list(mass_basis = NULL), list(mass_basis = "dry")))
  prepared <- prepare_discrete_import_results(NULL, 1L, results)
  expect_match(prepared$errors[[1]], "mass_basis")
  expect_identical(prepared$errors[[2]], "")
  expect_identical(prepared$results$result, 20)
  expect_identical(prepared$results$analysis_conditions, '{"mass_basis":"dry"}')
})
