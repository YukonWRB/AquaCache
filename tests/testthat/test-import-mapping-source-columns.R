test_that("source columns can have the same names as destination metadata", {
  criteria <- list(source.param_name = "Nitrogen", source.unit = "mg/L",
    source.result_speciation = "nitrate", source.sample_fraction = "dissolved", source.result_type = "lab")
  mappings <- data.table::data.table(import_mapping_id = 1L, parameter_id = 42L,
    priority = 50L, profile_specific = FALSE, source_match_size = length(criteria),
    source_match_values = list(criteria))
  source <- list(param_name = "Nitrogen", unit = "mg/L", result_speciation = "nitrate",
    sample_fraction = "dissolved", result_type = "lab")
  expect_equal(AquaCache:::import_mapping_resolve_match(mappings, source)$parameter_id, 42L)
  qualified <- stats::setNames(source, paste0("source.", names(source)))
  expect_equal(AquaCache:::import_mapping_resolve_match(mappings, qualified)$parameter_id, 42L)
  for (field in c("result_speciation", "sample_fraction", "result_type")) {
    changed <- source
    changed[[field]] <- "different"
    expect_null(AquaCache:::import_mapping_resolve_match(mappings, changed))
  }
  expect_null(AquaCache:::import_mapping_resolve_match(mappings, source[c("param_name", "unit")]))
})

test_that("missing-mapping reports keep distinct source fractions separate", {
  criteria <- list(ParamCode = "Al", input_unit = "mg/L", source.sample_fraction = "dissolved")
  mapping <- data.table::data.table(import_mapping_id = 1L, parameter_id = 42L,
    priority = 50L, profile_specific = FALSE, source_match_size = 3L, source_match_values = list(criteria))
  samples <- data.table::data.table(SampleId = 1L, CollectDateTime = as.POSIXct("2026-01-01", tz = "UTC"))
  results <- data.table::data.table(SampleId = 1L, ParamCode = "Al", Units = "mg/L", ParamDesc = "Aluminum",
    Result = c("0.1", "0.2"), sample_fraction = c("dissolved", "total"))
  report <- downloadEQWin_missing_mappings(samples, results, mapping)
  expect_equal(nrow(report), 1L)
  expect_equal(report$n_results, 1L)
  expect_identical(jsonlite::fromJSON(report$source_match)$source.sample_fraction, "total")

  mapping$source_match_values <- list(list(input_param = "Al", input_unit = "mg/L", source.sample_fraction = "dissolved"))
  source <- data.table::data.table(SITE_NO = "TEST", DATE_TIME_HEURE = as.POSIXct("2026-01-01", tz = "UTC"),
    FLAG_MARQUEUR = "", VALUE_VALEUR = c("0.1", "0.2"), VARIABLE = "Al", SAMPLE_ID_TEST = "s1",
    UNIT_UNIT_TEST = "mg/L", sample_fraction = c("dissolved", "total"))
  report <- downloadECCCwq_missing_mappings(source, mapping)
  expect_equal(nrow(report), 1L)
  expect_equal(report$n_results, 1L)
  expect_identical(jsonlite::fromJSON(report$source_match)$source.sample_fraction, "total")
})
