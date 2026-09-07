test_that("ECCC missing mapping report identifies unmapped and incomplete rows", {
  mapping <- data.table::data.table(
    import_mapping_id = c(1L, 2L),
    source_match_values = list(
      list(input_param = "KNOWN", input_unit = "mg/L"),
      list(input_param = "INCOMPLETE", input_unit = "mg/L")
    ),
    source_match_size = 2L,
    priority = 100L,
    parameter_id = c(1L, NA_integer_)
  )
  source <- data.table::data.table(
    SITE_NO = "TEST",
    DATE_TIME_HEURE = as.POSIXct("2026-01-01 12:00:00", tz = "UTC"),
    FLAG_MARQUEUR = "",
    VALUE_VALEUR = c("1", "2", "3"),
    VARIABLE = c("KNOWN", "INCOMPLETE", "NEW"),
    SAMPLE_ID_TEST = c("sample-1", "sample-2", "sample-3"),
    UNIT_UNIT_TEST = "mg/L"
  )

  out <- downloadECCCwq_missing_mappings(source, mapping)

  expect_equal(out$input_param, c("INCOMPLETE", "NEW"))
  expect_equal(out$missing_reason, c("missing_parameter_id", "no_mapping"))
  expect_equal(out$import_mapping_id, c(2L, NA_integer_))
  expect_equal(out$n_results, c(1L, 1L))
})

test_that("EQWin missing mapping report identifies unmapped and incomplete rows", {
  mapping <- data.table::data.table(
    import_mapping_id = c(1L, 2L),
    source_match_values = list(
      list(ParamCode = "KNOWN", input_unit = "mg/L"),
      list(ParamCode = "INCOMPLETE", input_unit = "mg/L")
    ),
    source_match_size = 2L,
    priority = 100L,
    parameter_id = c(1L, NA_integer_)
  )
  samples <- data.table::data.table(
    SampleId = c(1L, 2L, 3L),
    CollectDateTime = as.POSIXct(
      c("2026-01-01 12:00:00", "2026-01-02 12:00:00", "2026-01-03 12:00:00"),
      tz = "UTC"
    )
  )
  results <- data.table::data.table(
    SampleId = c(1L, 2L, 3L),
    ParamCode = c("KNOWN", "INCOMPLETE", "NEW"),
    Units = "mg/L",
    ParamDesc = c("Known", "Incomplete", "New"),
    Result = c("1", "2", "3")
  )

  out <- downloadEQWin_missing_mappings(samples, results, mapping)

  expect_equal(out$ParamCode, c("INCOMPLETE", "NEW"))
  expect_equal(out$missing_reason, c("missing_parameter_id", "no_mapping"))
  expect_equal(out$import_mapping_id, c(2L, NA_integer_))
  expect_equal(out$n_results, c(1L, 1L))
})
