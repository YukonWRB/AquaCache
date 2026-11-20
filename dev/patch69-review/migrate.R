readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
pkgload::load_all(".", quiet = TRUE)
con <- AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
before <- DBI::dbGetQuery(con,
  "SELECT 'parameter' AS kind, count(*) AS n FROM discrete.import_parameter_mappings m JOIN discrete.import_mapping_sets s USING (import_mapping_set_id) WHERE s.status='published'
   UNION ALL SELECT 'location', count(*) FROM discrete.import_location_mappings m JOIN discrete.import_mapping_sets s USING (import_mapping_set_id) WHERE s.status='published'
   UNION ALL SELECT 'result_flag', count(*) FROM discrete.import_result_flag_mappings m JOIN discrete.import_mapping_sets s USING (import_mapping_set_id) WHERE s.status='published'")
print(before)
audit_start <- DBI::dbGetQuery(con,
  "SELECT table_name,history_started_at FROM audit.table_registry
   WHERE schema_name='discrete' AND table_name IN
   ('import_parameter_mappings','import_location_mappings','import_result_flag_mappings') ORDER BY table_name")
source("inst/patches/patch_69.R", local = TRUE)
after <- DBI::dbGetQuery(con,
  "SELECT 'parameter' AS kind, count(*) AS n FROM discrete.import_parameter_mappings
   UNION ALL SELECT 'location', count(*) FROM discrete.import_location_mappings
   UNION ALL SELECT 'result_flag', count(*) FROM discrete.import_result_flag_mappings")
stopifnot(identical(before, after))
stopifnot(is.na(DBI::dbGetQuery(con, "SELECT to_regclass('discrete.import_mapping_sets')::text AS table_name")$table_name))
stopifnot(is.na(DBI::dbGetQuery(con, "SELECT to_regclass('discrete.import_run_rows')::text AS table_name")$table_name))
print(after)
stopifnot(identical(audit_start, DBI::dbGetQuery(con,
  "SELECT table_name,history_started_at FROM audit.table_registry
   WHERE schema_name='discrete' AND table_name IN
   ('import_parameter_mappings','import_location_mappings','import_result_flag_mappings') ORDER BY table_name")))
rows <- getImportParameterMappings(con, "patch69_migration_fixture", "workbook")
stopifnot(identical(rows$conversion, c(2, 1)))
stopifnot(DBI::dbGetQuery(con,
  "SELECT has_table_privilege('tester','discrete.import_parameter_mappings','UPDATE') AS allowed")$allowed)
stopifnot(DBI::dbGetQuery(con,
  "SELECT has_sequence_privilege('tester',pg_get_serial_sequence('discrete.import_parameter_mappings','import_mapping_id'),'USAGE') AS allowed")$allowed)
stopifnot(DBI::dbGetQuery(con,
  "SELECT count(*) AS n FROM discrete.results WHERE import_run_id IS NOT NULL")$n == 1)
stopifnot(DBI::dbGetQuery(con,
  "SELECT count(*) AS n FROM audit.table_registry WHERE schema_name='discrete'
   AND table_name IN ('import_mapping_sets','import_run_rows')")$n == 0)
cat("Verified scope overrides, draft discard, migrated result provenance, removed registry entries and retained editor grants.\n")
DBI::dbDisconnect(con)
