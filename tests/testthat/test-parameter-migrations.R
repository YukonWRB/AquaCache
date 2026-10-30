# Explicit opt-in: this test inserts fixtures only in the named testdb and rolls
# back every change. It never applies a schema patch or commits a parameter merge.
test_that("parameter merges preserve analytical identity and roll back failures", {
  skip_if(Sys.getenv("AQUACACHE_INTEGRATION_DATABASE") != "testdb", "testdb integration opt-in required")
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "testdb", host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"), user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"))
  on.exit(DBI::dbDisconnect(con))
  identity <- DBI::dbGetQuery(con,
    "SELECT current_database() AS db, inet_server_addr()::TEXT AS host,
      (SELECT version FROM information.version_info WHERE item = 'Last patch number') AS patch")
  expect_identical(identity$db, "testdb")
  expect_identical(identity$host, "10.250.12.154/32")
  skip_if(identity$patch != "65", "revised Patch 65 required")
  DBI::dbExecute(con, "BEGIN")
  on.exit(DBI::dbExecute(con, "ROLLBACK"), add = TRUE, after = FALSE)
  unit <- DBI::dbGetQuery(con, "SELECT min(unit_id) AS id FROM public.units WHERE lower(unit_name) = 'mg/l'")$id
  sample <- DBI::dbGetQuery(con, "SELECT min(sample_id) AS id FROM discrete.samples")$id
  matrix <- DBI::dbGetQuery(con, "SELECT matrix_state_id AS id FROM public.matrix_states WHERE matrix_state_code = 'liquid'")$id
  basis <- DBI::dbGetQuery(con, "SELECT reporting_basis_id AS id FROM discrete.reporting_bases WHERE reporting_basis_code = 'as_caco3'")$id
  form <- DBI::dbGetQuery(con, "SELECT min(result_speciation_id) AS id FROM discrete.result_speciations WHERE active")$id
  target <- DBI::dbGetQuery(con, "SELECT min(result_target_id) AS id FROM discrete.result_targets WHERE active")$id
  prefix <- paste0("Parameter merge integration ", Sys.getpid(), " ")
  parameter <- function(label, unit_id = unit) {
    force(label)
    force(unit_id)
    DBI::dbGetQuery(con,
    "INSERT INTO public.parameters (param_name, sample_fraction, plot_default_y_orientation, units_liquid)
      VALUES ($1,FALSE,'normal',$2) RETURNING parameter_id", params = list(paste0(prefix, label), unit_id))$parameter_id
  }
  result <- function(id, ph = 4.5, value = 21.5, basis_id = basis, form_id = form, target_id = target) DBI::dbGetQuery(con,
    "INSERT INTO discrete.results (sample_id, parameter_id, matrix_state_id, result_type, result,
      reporting_basis_id, result_speciation_id, result_target_id, endpoint_mode, endpoint_ph, analysis_conditions)
      VALUES ($1,$2,$3,2,$4,$5,$6,$7,'fixed',$8,'{\"reference_temperature_kelvin\":293.15}') RETURNING result_id",
    params = list(sample, id, matrix, value, basis_id, form_id, target_id, ph))$result_id
  source_id <- parameter("source")
  target_id <- parameter("target")
  row_id <- result(source_id)
  before <- DBI::dbGetQuery(con, "SELECT * FROM discrete.results WHERE result_id = $1", params = list(row_id))
  group <- DBI::dbGetQuery(con, "SELECT min(group_id) AS id FROM public.parameter_groups")$id
  subgroups <- DBI::dbGetQuery(con, "SELECT sub_group_id FROM public.parameter_sub_groups ORDER BY sub_group_id LIMIT 2")$sub_group_id
  for (subgroup in subgroups) DBI::dbExecute(con,
    "INSERT INTO public.parameter_relationships (parameter_id,group_id,sub_group_id) VALUES ($1,$2,$3)",
    params = list(source_id, group, subgroup))
  DBI::dbExecute(con,
    "INSERT INTO public.parameter_relationships (parameter_id,group_id,sub_group_id) VALUES ($1,$2,$3)",
    params = list(target_id, group, subgroups[[1]]))
  for (table in c("parameter_reporting_basis_suggestions", "parameter_speciation_suggestions", "parameter_target_suggestions")) {
    field <- switch(table, parameter_reporting_basis_suggestions = "reporting_basis_id",
      parameter_speciation_suggestions = "result_speciation_id", parameter_target_suggestions = "result_target_id")
    id <- switch(field, reporting_basis_id = basis, result_speciation_id = form, result_target_id = target)
    DBI::dbExecute(con, sprintf("INSERT INTO public.%s (parameter_id,%s,display_order) VALUES ($1,$3,10),($2,$3,20)", table, field),
      params = list(source_id, target_id, id))
  }
  expect_true(migrate_parameter_references(con, source_id, target_id, delete_source = TRUE))
  after <- DBI::dbGetQuery(con, "SELECT * FROM discrete.results WHERE result_id = $1", params = list(row_id))
  expect_identical(after$parameter_id, target_id)
  retained <- c("result", "sample_id", "matrix_state_id", "reporting_basis_id", "result_speciation_id",
    "result_target_id", "endpoint_mode", "endpoint_ph", "analysis_conditions", "analysis_conditions_key")
  expect_identical(after[retained], before[retained])
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM public.parameters WHERE parameter_id = $1", params = list(source_id))$n, 0)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM public.parameter_relationships WHERE parameter_id = $1", params = list(target_id))$n, 2)
  for (table in c("parameter_reporting_basis_suggestions", "parameter_speciation_suggestions", "parameter_target_suggestions")) {
    rows <- DBI::dbGetQuery(con, sprintf("SELECT display_order FROM public.%s WHERE parameter_id = $1", table), params = list(target_id))
    expect_identical(rows$display_order, 10L)
  }
  duplicate_id <- parameter("conflict")
  conflict_row <- result(duplicate_id, value = 99)
  DBI::dbExecute(con, "INSERT INTO public.parameter_reporting_basis_suggestions (parameter_id,reporting_basis_id,display_order)
    VALUES ($1,$2,5)", params = list(duplicate_id, basis))
  expect_error(migrate_parameter_references(con, duplicate_id, target_id), "duplicate key")
  expect_identical(DBI::dbGetQuery(con, "SELECT parameter_id FROM discrete.results WHERE result_id = $1", params = list(conflict_row))$parameter_id, duplicate_id)
  expect_true(dbTransCheck(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT display_order FROM public.parameter_reporting_basis_suggestions
    WHERE parameter_id = $1", params = list(target_id))$display_order, 10L)
  expect_identical(DBI::dbGetQuery(con, "SELECT display_order FROM public.parameter_reporting_basis_suggestions
    WHERE parameter_id = $1", params = list(duplicate_id))$display_order, 5L)
  failure_id <- parameter("metadata failure")
  failure_row <- result(failure_id, ph = 8.3)
  expect_error(revise_parameter_catalogue_entry(con, paste0(prefix, "metadata failure"), paste0(prefix, "target"),
    plot_default_y_orientation = "invalid", delete_source = TRUE))
  expect_identical(DBI::dbGetQuery(con, "SELECT parameter_id FROM discrete.results WHERE result_id = $1", params = list(failure_row))$parameter_id, failure_id)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM public.parameters WHERE parameter_id = $1", params = list(failure_id))$n, 1)
  incompatible_id <- parameter("different units", DBI::dbGetQuery(con, "SELECT min(unit_id) AS id FROM public.units WHERE unit_id <> $1", params = list(unit))$id)
  expect_error(migrate_parameter_references(con, incompatible_id, target_id), "unit definitions differ")
  fill_source <- parameter("fill source")
  fill_target <- parameter("fill target")
  fill_row <- result(fill_source, form_id = NA_integer_, target_id = NA_integer_)
  expect_identical(revise_parameter_catalogue_entry(con, paste0(prefix, "fill source"), paste0(prefix, "fill target"),
    mapping_reporting_basis = "as_n", mapping_result_speciation = form, mapping_result_target = target,
    reporting_basis = TRUE, result_speciation = TRUE, result_target = TRUE, endpoint = TRUE), fill_target)
  filled <- DBI::dbGetQuery(con, "SELECT reporting_basis_id, result_speciation_id, result_target_id FROM discrete.results WHERE result_id = $1", params = list(fill_row))
  expect_identical(unname(as.list(filled)), list(basis, form, target))
  nitrogen_basis <- DBI::dbGetQuery(con, "SELECT reporting_basis_id AS id FROM discrete.reporting_bases WHERE reporting_basis_code = 'as_n'")$id
  migrate_parameter_references(con, fill_target, fill_target, reporting_basis_id = nitrogen_basis, preserve_existing_chemistry = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT reporting_basis_id FROM discrete.results WHERE result_id = $1", params = list(fill_row))$reporting_basis_id, nitrogen_basis)
  expect_true(dbTransCheck(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT tgenabled::TEXT AS state FROM pg_trigger WHERE tgname = 'guard_import_mapping_row' AND tgrelid = 'discrete.import_parameter_mappings'::regclass")$state, "O")
})
