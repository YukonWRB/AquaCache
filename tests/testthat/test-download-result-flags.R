test_that("source-wide flags affect both download adapters", {
  key <- data.frame(
    import_source_id = 1L,
    parameter_id = 42L,
    result_type_id = 2L,
    protocol_method_id = NA_integer_,
    sample_fraction_id = NA_integer_,
    result_value_type_id = 1L,
    reporting_basis_id = NA_integer_,
    result_speciation_id = NA_integer_,
    result_target_id = NA_integer_,
    endpoint_mode = NA_character_,
    endpoint_ph = NA_real_,
    analysis_conditions = "{}",
    matrix_state_id = 1L,
    conversion = 2,
    result_offset = 1
  )
  rules <- data.frame(
    import_result_flag_mapping_id = 1:5,
    source_flag_column = c(
      NA,
      "FLAG_MARQUEUR",
      "ResultCode",
      "ResultQuality",
      "ResultCode"
    ),
    source_flag_value = c("omit", "<", "ND", "omit", "reject"),
    profile_specific = FALSE,
    priority = c(1L, 50L, 50L, 50L, 50L),
    result_action = c(
      "reject_row",
      "note_only",
      "set_result_null",
      "skip_result",
      "reject_row"
    ),
    result_condition_id = c(
      NA_integer_,
      NA_integer_,
      1L,
      NA_integer_,
      NA_integer_
    ),
    result_condition_value_source = c(
      "none",
      "none",
      "method_detection_limit",
      "none",
      "none"
    ),
    result_condition_value_literal = NA_real_,
    note_template = c(NA, "Documented flag", "Laboratory MDL", NA, NA)
  )
  # ECCC's omission rule is column-specific, like the EQWin rule.
  rules <- rbind(
    rules,
    transform(
      rules[4L, ],
      import_result_flag_mapping_id = 6L,
      source_flag_column = "FLAG_MARQUEUR"
    )
  )
  eq_results <- data.frame(
    SampleId = c(1L, 2L),
    Result = "10",
    ResultCode = c("ND", ""),
    ResultQuality = c("", "omit"),
    DetailComment = "Source note",
    ParamCode = "A",
    Units = "mg/L",
    LMDL = 0.5
  )
  local_mocked_bindings(
    import_mapping_load_db = function(...) key,
    import_mapping_resolve_match = function(...) key,
    getImportResultFlagMappings = function(
      con,
      source_code,
      profile_code = NULL
    ) {
      expect_null(profile_code)
      rules
    },
    validateResultChemistry = function(con, results, ...) rep("", nrow(results))
  )
  local_mocked_bindings(
    dbExecute = function(...) 0L,
    dbGetQuery = function(con, statement, ...) {
      if (grepl("FROM eqstns", statement, fixed = TRUE)) {
        return(data.frame(StnId = 1L, StnType = "SW"))
      }
      if (grepl("FROM eqsampls", statement, fixed = TRUE)) {
        return(data.frame(
          SampleId = 1:2,
          SampleNo = c("S1", "S2"),
          SampleClass = "M",
          SampleComments = "",
          CollectDateTime = as.POSIXct(
            c("2026-01-01 10:00:00", "2026-01-02 10:00:00"),
            tz = "UTC"
          )
        ))
      }
      if (grepl("FROM eqdetail", statement, fixed = TRUE)) {
        return(eq_results)
      }
      if (grepl("discrete.result_conditions", statement, fixed = TRUE)) {
        return(data.frame(
          result_condition_id = 1:2,
          result_condition = c("Below Detection Limit", "Above Detection Limit")
        ))
      }
      if (grepl("discrete.result_types", statement, fixed = TRUE)) {
        return(data.frame(result_type_id = 2L, result_type_name = "lab"))
      }
      id <- sub("^[[:space:]]*SELECT ([a-z_]+).*", "\\1", statement)
      stats::setNames(data.frame(1L), id)
    },
    .package = "DBI"
  )

  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "SITE_NO,DATE_TIME_HEURE,FLAG_MARQUEUR,VALUE_VALEUR,VARIABLE,UNIT_UNIT\u00C9,SAMPLE_ID_\u00C9CHANTILLON",
      "TEST,2026-01-01 10:00:00,<,10,A,mg/L,S1",
      "TEST,2026-01-01 10:00:00,omit,10,A,mg/L,S1"
    ),
    path,
    useBytes = TRUE
  )
  eccc <- downloadECCCwq(
    "TEST",
    path,
    tz = "UTC",
    start_datetime = "2026-01-01",
    con = "db"
  )
  expect_equal(nrow(eccc[[1]]$results), 1L)
  expect_equal(eccc[[1]]$results$result_condition_value, 21)
  expect_equal(eccc[[1]]$results$result_condition_id, 1L)
  expect_identical(eccc[[1]]$results$note, "Documented flag")
  expect_identical(eccc[[1]]$skipped_parameter_ids, 42L)

  eqwin <- downloadEQWin("TEST", "2026-01-01", con = "db", EQCon = "access")
  expect_true(is.na(eqwin[[1]]$results$result))
  expect_equal(eqwin[[1]]$results$result_condition_value, 2)
  expect_equal(eqwin[[1]]$results$result_condition_id, 1L)
  expect_match(
    eqwin[[1]]$results$note,
    "Source note; Laboratory MDL",
    fixed = TRUE
  )
  expect_equal(nrow(eqwin[[2]]$results), 0L)
  expect_identical(eqwin[[2]]$skipped_parameter_ids, 42L)

  eq_results$ResultCode[[1L]] <- "reject"
  expect_error(
    downloadEQWin("TEST", "2026-01-01", con = "db", EQCon = "access"),
    "result flag rejects SampleId 1"
  )
})
