test_that("profile parameter mappings override source-wide mappings", {
  mappings <- data.table::data.table(
    import_mapping_id = c(1L, 2L),
    profile_specific = c(FALSE, TRUE),
    source_match_values = list(
      list(parameter_code = "AL", unit = "ug/L"),
      list(parameter_code = "AL", unit = "ug/L")
    ),
    source_match_size = c(2L, 2L),
    priority = c(10L, 100L),
    parameter_id = c(11L, 22L)
  )

  result <- AquaCache:::import_mapping_resolve_match(
    mappings,
    list(parameter_code = "AL", unit = "ug/L")
  )

  expect_identical(result$parameter_id[[1]], 22L)
  expect_true(result$profile_specific[[1]])
})

test_that("source-wide mapping behaviour remains the default", {
  mappings <- data.table::data.table(
    import_mapping_id = c(1L, 2L),
    source_match_values = list(
      list(parameter_code = "AL"),
      list(parameter_code = "AL", unit = "ug/L")
    ),
    source_match_size = c(1L, 2L),
    priority = c(100L, 100L),
    parameter_id = c(11L, 22L)
  )

  result <- AquaCache:::import_mapping_resolve_match(
    mappings,
    list(parameter_code = "AL", unit = "ug/L")
  )

  expect_identical(result$parameter_id[[1]], 22L)
})

test_that("Patch 61 creates versioned scoped mappings and portable grants", {
  patch <- paste(
    readLines(
      system.file("patches", "patch_61.R", package = "AquaCache"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    patch,
    "CREATE TABLE discrete.import_mapping_sets",
    fixed = TRUE
  )
  expect_false(grepl("ADD COLUMN match_schema", patch, fixed = TRUE))
  expect_match(
    patch,
    "import_parameter_mappings_set_fkey",
    fixed = TRUE
  )
  expect_match(
    patch,
    "CREATE TABLE discrete.import_location_mappings",
    fixed = TRUE
  )
  expect_match(
    patch,
    "RENAME TO import_result_flag_mappings",
    fixed = TRUE
  )
  expect_match(patch, "source_flag_column", fixed = TRUE)
  expect_match(patch, "source_flag_value", fixed = TRUE)
  expect_match(patch, "result_condition_id", fixed = TRUE)
  expect_match(
    patch,
    "column_map - 'result_qualifier'",
    fixed = TRUE
  )
  expect_match(
    patch,
    "'result_flag',\n             column_map -> 'result_qualifier'",
    fixed = TRUE
  )
  expect_match(
    patch,
    "This import syntax is distinct from normalized AquaCache qualifiers",
    fixed = TRUE
  )
  expect_match(
    patch,
    "to_regclass('discrete.import_qualifier_mappings') IS NULL",
    fixed = TRUE
  )
  expect_match(patch, "source_adapter_function", fixed = TRUE)
  expect_match(patch, "external_sample_id", fixed = TRUE)
  expect_match(patch, "'replicate_set'", fixed = TRUE)
  expect_match(
    patch,
    "ALTER TABLE discrete.samples DROP COLUMN linked_with",
    fixed = TRUE
  )
  expect_match(patch, "patch61_linked_sample_components", fixed = TRUE)
  expect_match(patch, "metadata_view_legacy_links_removed", fixed = TRUE)
  expect_match(patch, "aclexplode", fixed = TRUE)
  expect_match(patch, "samples_sample_id_seq", fixed = TRUE)
  expect_match(
    patch,
    "Import runs retain the exact source and profile mapping-set IDs",
    fixed = TRUE
  )
  expect_match(patch, "guard_import_mapping_row", fixed = TRUE)
  expect_match(patch, "import_columns_documented", fixed = TRUE)
  expect_match(patch, "patch61_commit", fixed = TRUE)
  expect_match(patch, "guard_sample_external_identity", fixed = TRUE)
  expect_match(
    patch,
    "to_regclass('discrete.import_profile_defaults') IS NULL",
    fixed = TRUE
  )
  expect_false(grepl(
    "CREATE TABLE discrete.import_profile_defaults",
    patch,
    fixed = TRUE
  ))
  expect_false(grepl("yg_editor_group", patch, fixed = TRUE))
})

test_that("result-flag mapping APIs use unambiguous terminology", {
  exports <- getNamespaceExports("AquaCache")

  expect_true(all(c(
    "upsertImportResultFlagMappings",
    "getImportResultFlagMappings",
    "resolveImportResultFlagMapping"
  ) %in% exports))
  expect_false(any(c(
    "upsertImportQualifierMappings",
    "getImportQualifierMappings",
    "resolveImportQualifierMapping"
  ) %in% exports))

  expect_true(all(c(
    "source_flag_value",
    "source_flag_column"
  ) %in% names(formals(AquaCache::resolveImportResultFlagMapping))))
})

