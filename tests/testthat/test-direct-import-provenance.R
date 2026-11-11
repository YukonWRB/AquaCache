test_that("direct mappings and immutable upload provenance work in testdb", {
  skip_if(Sys.getenv("AQUACACHE_TEST_PATCH69") != "true", "Patch 69 testdb opt-in required")
  stopifnot(Sys.getenv("aquacacheName") == "testdb", Sys.getenv("aquacacheHost") != "199.247.132.26")
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))
  identity <- DBI::dbGetQuery(con,
    "SELECT current_database() AS db, (SELECT version FROM information.version_info
     WHERE item='Last patch number') AS patch")
  stopifnot(identity$db == "testdb", identity$patch == "69")
  DBI::dbBegin(con)
  code <- paste0("patch69_review_", Sys.getpid())
  parameter <- DBI::dbGetQuery(con,
    "SELECT parameter_id FROM public.parameters WHERE NOT sample_fraction
     AND NOT reporting_basis AND NOT result_speciation AND NOT result_target AND NOT endpoint
     AND cardinality(analysis_conditions_required_keys)=0
     AND analysis_conditions_required_keys_by_matrix='{}'::jsonb
     AND units_liquid IS NOT NULL ORDER BY parameter_id LIMIT 1")$parameter_id
  mapping <- data.frame(code = "KNOWN", parameter_id = parameter,
    result_type = 2L, matrix_state_id = 1L, result_value_type = 1L)
  upsertImportParameterMappings(con, code, mapping, "code")
  profile_id <- upsertImportProfile(con, code, "workbook", "Review workbook",
    datetime_origin = "text", column_map = list(parameter_code = "Analyte", result = "Value"))
  upsertImportParameterMappings(con, code, transform(mapping, conversion = 2),
    "code", profile_code = "workbook")
  rows <- getImportParameterMappings(con, code, "workbook")
  expect_equal(nrow(rows), 2L)
  expect_equal(rows$conversion, c(2, 1))
  review <- getImportMappingReview(con, code)
  expect_equal(nrow(review), 2L)
  expect_false(any(c("status", "version", "import_mapping_set_id") %in% names(review)))
  snapshot <- getImportConfigurationSnapshot(con, code, "workbook", list(negative_values_are_mdl = TRUE))
  expect_identical(snapshot$upload_options$negative_values_are_mdl, TRUE)
  expect_equal(length(snapshot$mappings$parameters), 2L)
  upsertImportParameterMappings(con, code, transform(mapping, conversion = 3),
    "code", profile_code = "workbook")
  expect_equal(nrow(getImportParameterMappings(con, code, "workbook")), 2L)
  expect_false(identical(snapshot, getImportConfigurationSnapshot(con, code, "workbook")))
  expect_equal(snapshot$mappings$parameters[[1]]$conversion, 2)
  DBI::dbExecute(con, "UPDATE discrete.import_sources SET source_name='Review laboratory',active=FALSE WHERE source_code=$1",
    params = list(code))
  upsertImportParameterMappings(con, code, transform(mapping, conversion = 3), "code", profile_code = "workbook")
  source <- DBI::dbGetQuery(con, "SELECT source_name,active FROM discrete.import_sources WHERE source_code=$1",
    params = list(code))
  expect_identical(source$source_name, "Review laboratory")
  expect_false(source$active)
  DBI::dbExecute(con, "UPDATE discrete.import_sources SET active=TRUE WHERE source_code=$1", params = list(code))

  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  writeBin(charToRaw("Patch 69 archived source workbook test"), file)
  type <- DBI::dbGetQuery(con, "SELECT document_type_en FROM files.document_types LIMIT 1")$document_type_en
  document_id <- insertACDocument(file, paste0(code, ".xlsx"), type,
    "Synthetic source for provenance verification.", con = con)$new_document_id
  source_id <- rows$import_source_id[[1]]
  run1 <- createImportRun(con, source_id, profile_id,
    source_document_id = document_id, configuration_snapshot = snapshot)
  run2 <- createImportRun(con, source_id, profile_id,
    source_document_id = document_id, configuration_snapshot = snapshot)
  completeImportRun(con, run1)
  completeImportRun(con, run2)
  stored <- DBI::dbGetQuery(con,
    "SELECT source_document_id, configuration_snapshot::text AS snapshot
     FROM discrete.import_runs WHERE import_run_id=$1", params = list(run1))
  expect_equal(stored$source_document_id, document_id)
  expect_identical(jsonlite::fromJSON(stored$snapshot, simplifyVector = FALSE)$upload_options$negative_values_are_mdl, TRUE)
  fields <- DBI::dbListFields(con, DBI::Id(schema = "discrete", table = "import_runs"))
  expect_false(any(c("source_file_name", "source_file_size", "source_file_hash", "source_file_sha256") %in% fields))

  result_sql <- "INSERT INTO discrete.results (
    sample_id,result_type,parameter_id,result,result_value_type,matrix_state_id,analysis_conditions,import_run_id,analysis_datetime
  ) SELECT sample_id,2,$2,42,1,1,'{}'::jsonb,$1::integer,'2026-11-09'::timestamptz+$1::integer*interval '1 second'
    FROM discrete.samples ORDER BY sample_id LIMIT 1 RETURNING result_id,sample_id"
  first <- DBI::dbGetQuery(con, result_sql, params = list(run1, parameter))
  second <- DBI::dbGetQuery(con, result_sql, params = list(run2, parameter))
  expect_equal(first$sample_id, second$sample_id)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT count(DISTINCT import_run_id) AS n FROM discrete.results WHERE result_id IN ($1,$2)",
    params = list(first$result_id, second$result_id))$n, 2)

  expect_database_error <- function(sql, params, pattern) {
    DBI::dbExecute(con, "SAVEPOINT expected_failure")
    statement <- DBI::dbSendStatement(con, sql)
    expect_error(tryCatch(DBI::dbBind(statement, params),
      finally = DBI::dbClearResult(statement)), pattern)
    DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT expected_failure")
  }
  expect_database_error("UPDATE discrete.import_runs SET configuration_snapshot='{}' WHERE import_run_id=$1",
    list(run1), "immutable")
  expect_database_error("UPDATE discrete.import_runs SET source_adapter_function='changed' WHERE import_run_id=$1",
    list(run1), "immutable")
  expect_database_error("UPDATE discrete.import_runs SET summary='{}',status='failed' WHERE import_run_id=$1",
    list(run1), "outcome is immutable")
  expect_database_error("UPDATE discrete.import_runs SET created_by='tester' WHERE import_run_id=$1",
    list(run1), "immutable")
  expect_database_error("UPDATE discrete.results SET import_run_id=$1 WHERE result_id=$2",
    list(run2, first$result_id), "immutable")
  expect_database_error("UPDATE files.documents SET document=decode('1234','hex') WHERE document_id=$1",
    list(document_id), "cannot be replaced")
  expect_equal(DBI::dbExecute(con, "UPDATE files.documents SET share_with=ARRAY['public_reader'] WHERE document_id=$1",
    params = list(document_id)), 1L)
  audit <- DBI::dbGetQuery(con,
    "SELECT count(*) AS n FROM audit.general_log WHERE schema_name='discrete'
     AND table_name='import_parameter_mappings' AND new_data->>'import_source_id'=$1",
    params = list(as.character(source_id)))
  expect_gt(as.numeric(audit$n), 0)

  DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
  expect_equal(nrow(DBI::dbGetQuery(con,
    "SELECT configuration_snapshot FROM discrete.import_runs WHERE import_run_id=$1", params = list(run1))), 1L)
  DBI::dbExecute(con, "RESET ROLE")
  group <- DBI::dbGetQuery(con,
    "SELECT rolname FROM pg_roles WHERE NOT rolcanlogin AND rolname !~ '^pg_'
     AND NOT pg_has_role('public_reader',oid,'member') ORDER BY rolname LIMIT 1")$rolname
  DBI::dbExecute(con, "UPDATE files.documents SET share_with=ARRAY[$1] WHERE document_id=$2",
    params = list(group, document_id))
  DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
  expect_equal(nrow(DBI::dbGetQuery(con,
    "SELECT configuration_snapshot FROM discrete.import_runs WHERE import_run_id=$1", params = list(run1))), 0L)
  DBI::dbExecute(con, "RESET ROLE")

  # A normal creator can finalize their own documentless run, but cannot
  # pretend to be another uploader to gain access to that person's metadata.
  DBI::dbExecute(con, "GRANT USAGE ON SCHEMA discrete,files TO tester")
  DBI::dbExecute(con, "GRANT SELECT,INSERT,UPDATE ON discrete.import_runs TO tester")
  DBI::dbExecute(con, "GRANT SELECT ON files.documents TO tester")
  sequence <- DBI::dbGetQuery(con,
    "SELECT pg_get_serial_sequence('discrete.import_runs','import_run_id') AS name")$name
  DBI::dbExecute(con, paste0("GRANT USAGE ON SEQUENCE ", sequence, " TO tester"))
  DBI::dbExecute(con, "SET LOCAL ROLE tester")
  own <- DBI::dbGetQuery(con,
    "INSERT INTO discrete.import_runs(import_source_id,configuration_snapshot)
     VALUES ($1,'{}') RETURNING import_run_id", params = list(source_id))$import_run_id
  expect_silent(completeImportRun(con, own))
  expect_equal(nrow(DBI::dbGetQuery(con,
    "SELECT import_run_id FROM discrete.import_runs WHERE import_run_id=$1", params = list(own))), 1L)
  expect_database_error(
    "INSERT INTO discrete.import_runs(import_source_id,created_by) VALUES ($1,'postgres')",
    list(source_id), "row-level security")
  DBI::dbExecute(con, "RESET ROLE")
})
