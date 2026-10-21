test_that("independent analytical requirements cannot substitute for one another", {
  parameters <- data.frame(parameter_id = 1L, sample_fraction = FALSE,
    reporting_basis = TRUE, result_speciation = TRUE, result_target = TRUE, endpoint = FALSE)
  result <- data.frame(parameter_id = 1L, reporting_basis_id = 2L)
  messages <- validateResultChemistry(NULL, result, parameters, error = FALSE)
  expect_match(messages, "Chemical speciation is required")
  expect_match(messages, "Analytical target is required")
  expect_false(grepl("Reporting basis is required", messages))
  result$result_speciation_id <- 3L
  result$result_target_id <- 4L
  expect_identical(validateResultChemistry(NULL, result, parameters), "")
})

test_that("matrix-specific condition requirements do not burden other states", {
  parameters <- data.frame(parameter_id = 1L,
    analysis_conditions_required_keys_by_matrix = '{"solid":["mass_basis"],"gas":["gas_basis"]}')
  states <- data.frame(matrix_state_id = 1:3, matrix_state_code = c("liquid", "solid", "gas"))
  rows <- data.frame(parameter_id = 1L, matrix_state_id = 1:3, analysis_conditions = "{}")
  messages <- validateResultChemistry(NULL, rows, parameters, error = FALSE, matrix_states = states)
  expect_identical(messages[[1]], "")
  expect_match(messages[[2]], "mass_basis")
  expect_match(messages[[3]], "gas_basis")
  rows$analysis_conditions <- c("{}", '{"mass_basis":"dry"}', '{"gas_basis":"wet"}')
  expect_identical(validateResultChemistry(NULL, rows, parameters, matrix_states = states), rep("", 3))
})

test_that("analysis conditions retain numeric meaning and reject malformed context", {
  expect_identical(normalizeAnalysisConditions('{"mass_basis":"dry"}'), '{"mass_basis":"dry"}')
  expect_identical(normalizeAnalysisConditions(list(b = 2, a = 1)), '{"a":1,"b":2}')
  expect_error(normalizeAnalysisConditions(list(label = "dry", value = Inf)), "finite")
  expect_error(normalizeAnalysisConditions(list(mass_basis = c("dry", "wet"))), "mass_basis")
  expect_error(normalizeAnalysisConditions(list(reference_pressure_kpa = 0)), "positive")
  expect_error(normalizeAnalysisConditions(list(mass_basis = NULL)), "mass_basis")
  expect_error(normalizeAnalysisConditions('{"gas_basis":null}'), "gas_basis")
  expect_error(normalizeAnalysisConditions(list(reference_pressure_kpa = NULL)), "positive")
  expect_error(normalizeAnalysisConditions('[1,2]'), "JSON object")
  parameters <- data.frame(parameter_id = 1L)
  parameters$analysis_conditions_required_keys <- list(c("mass_basis", "reference_temperature_kelvin"))
  expect_error(validateResultChemistry(NULL, data.frame(parameter_id = 1L,
    analysis_conditions = '{"mass_basis":"dry"}'), parameters), "reference_temperature_kelvin")
  expect_identical(validateResultChemistry(NULL, data.frame(parameter_id = 1L,
    analysis_conditions = '{"mass_basis":"dry","reference_temperature_kelvin":293.15}'), parameters), "")
})

test_that("endpoint validation distinguishes unknown from measured endpoints", {
  parameters <- data.frame(parameter_id = 1L, endpoint = TRUE)
  results <- data.frame(parameter_id = rep(1L, 8L),
    endpoint_mode = c("fixed", "fixed", "gran", "fixed", NA, "gran", "unknown", "unknown"),
    endpoint_ph = c(-1, 16, NA, NA, 4.5, NaN, NA, 5))
  messages <- validateResultChemistry(NULL, results, parameters, error = FALSE)
  expect_identical(messages[c(1:3, 7)], rep("", 4))
  expect_true(all(nzchar(messages[c(4:6, 8)])))
  expect_error(validateResultChemistry(NULL, results, parameters), "valid endpoint")
})

test_that("optional IDs remain typed and validation does not alter caller data", {
  parameters <- data.frame(parameter_id = 1L)
  results <- data.table::data.table(parameter_id = c(1L, 1L, 1L),
    reporting_basis_id = c(NA, 1.5, NaN))
  before <- data.table::copy(results)
  messages <- validateResultChemistry(NULL, results, parameters, error = FALSE)
  expect_identical(messages[[1]], "")
  expect_true(all(nzchar(messages[2:3])))
  expect_identical(results, before)
  expect_error(validateResultChemistry(NULL, data.frame(parameter_id = 99L), parameters), "valid parameter")
})
