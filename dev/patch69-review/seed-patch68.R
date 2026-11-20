readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
pkgload::load_all(".", quiet = TRUE)
con <- AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
source("tests/testthat/setup.R")
sync_test_sequences(con)
legacy <- new.env(parent = asNamespace("AquaCache"))
api <- tempfile(fileext = ".R")
stopifnot(system2("git", c("show", "2c8bb297cb896ea44a71695a9291d92c7689bfab:R/import_mappings.R"), stdout = api) == 0L)
sys.source(api, legacy)
unlink(api)
code <- "patch69_migration_fixture"
parameter <- DBI::dbGetQuery(con,
  "SELECT parameter_id FROM public.parameters WHERE NOT sample_fraction AND NOT reporting_basis
   AND NOT result_speciation AND NOT result_target AND NOT endpoint
   AND cardinality(analysis_conditions_required_keys)=0
   AND analysis_conditions_required_keys_by_matrix='{}'::jsonb AND units_liquid IS NOT NULL
   ORDER BY parameter_id LIMIT 1")$parameter_id
mapping <- data.frame(code = "KNOWN", parameter_id = parameter, result_type = 2L,
  matrix_state_id = 1L, result_value_type = 1L, conversion = 1)
legacy$upsertImportParameterMappings(con, code, mapping, "code")
profile_id <- legacy$upsertImportProfile(con, code, "workbook", "Migration fixture",
  datetime_origin = "text", column_map = list(parameter_code = "Analyte", result = "Value"))
legacy$upsertImportParameterMappings(con, code, transform(mapping, conversion = 2), "code", profile_code = "workbook")
location <- DBI::dbGetQuery(con, "SELECT location_id FROM public.locations ORDER BY location_id LIMIT 1")$location_id
legacy$upsertImportLocationMappings(con, code, data.frame(source_location_code = "SITE", location_id = location))
legacy$upsertImportResultFlagMappings(con, code, data.frame(source_flag_value = "SKIP", result_action = "skip_result"))
source_id <- DBI::dbGetQuery(con, "SELECT import_source_id FROM discrete.import_sources WHERE source_code=$1", params = list(code))$import_source_id
run <- legacy$createImportRun(con, source_id, profile_id, source_file_name = "historical.xlsx")
result <- DBI::dbGetQuery(con, "SELECT result_id,sample_id FROM discrete.results ORDER BY result_id LIMIT 1")
legacy$appendImportRunRows(con, run, data.frame(source_row_number = 1L,
  sample_id = result$sample_id, result_id = result$result_id, validation_status = "committed"))
legacy$completeImportRun(con, run)
legacy$upsertImportParameterMappings(con, code, transform(mapping, conversion = 9), "code", publish = FALSE)
DBI::dbExecute(con, "GRANT SELECT,INSERT,UPDATE ON discrete.import_parameter_mappings TO tester")
seq <- DBI::dbGetQuery(con, "SELECT pg_get_serial_sequence('discrete.import_parameter_mappings','import_mapping_id') AS name")$name
DBI::dbExecute(con, paste0("GRANT USAGE,SELECT ON SEQUENCE ", seq, " TO tester"))
DBI::dbDisconnect(con)
cat("Seeded published source/profile overrides, an unpublished draft, other mappings, result provenance and custom grants.\n")
