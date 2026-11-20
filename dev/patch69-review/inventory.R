readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
pkgload::load_all(".", quiet = TRUE)
con <- AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
stopifnot(DBI::dbGetQuery(con, "SELECT current_database() AS db")$db == "testdb")
stopifnot(DBI::dbGetQuery(con,
  "SELECT version FROM information.version_info WHERE item='Last patch number'")$version == "69")
stopifnot(DBI::dbGetQuery(con,
  "SELECT count(*) AS n FROM information_schema.tables WHERE table_schema='discrete'
   AND (table_name IN ('import_mapping_sets','import_run_rows') OR table_name LIKE 'import%_legacy')")$n == 0)
fields <- DBI::dbListFields(con, DBI::Id(schema = "discrete", table = "import_runs"))
stopifnot(all(c("source_document_id","configuration_snapshot") %in% fields),
  !any(grepl("^source_file_", fields)))
stopifnot(DBI::dbGetQuery(con,
  "SELECT count(*) AS n FROM discrete.results r JOIN discrete.import_runs ir USING(import_run_id)
   JOIN discrete.import_sources src ON src.import_source_id=ir.import_source_id
   WHERE src.source_code IN ('patch69_worker_review','patch69_browser_review')
   AND (ir.source_document_id IS NULL OR ir.configuration_snapshot='{}'::jsonb OR NOT EXISTS (
     SELECT 1 FROM discrete.sample_documents sd
     WHERE sd.sample_id=r.sample_id AND sd.document_id=ir.source_document_id))")$n == 0)
browser <- DBI::dbGetQuery(con,
  "SELECT r.result_id,r.import_run_id,ir.source_document_id FROM discrete.results r
   JOIN discrete.import_runs ir USING(import_run_id)
   JOIN discrete.import_sources src ON src.import_source_id=ir.import_source_id
   WHERE src.source_code='patch69_browser_review' AND r.lab_sample_no='BROWSER-B'")
stopifnot(nrow(browser) == 1L)
DBI::dbWithTransaction(con, {
  DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
  stopifnot(nrow(DBI::dbGetQuery(con, "SELECT result_id FROM discrete.results WHERE result_id=$1",
    params = list(browser$result_id))) == 1L)
  stopifnot(nrow(DBI::dbGetQuery(con, "SELECT import_run_id FROM discrete.import_runs WHERE import_run_id=$1",
    params = list(browser$import_run_id))) == 0L)
  stopifnot(nrow(DBI::dbGetQuery(con, "SELECT document_id FROM files.documents WHERE document_id=$1",
    params = list(browser$source_document_id))) == 0L)
})
print(DBI::dbGetQuery(con, "SELECT current_database(), session_user, version FROM information.version_info WHERE item = 'Last patch number'"))
print(DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'discrete' AND table_name LIKE 'import%' ORDER BY table_name"))
print(DBI::dbGetQuery(con, "SELECT column_name, data_type, is_nullable FROM information_schema.columns WHERE table_schema='discrete' AND table_name='import_runs' ORDER BY ordinal_position"))
print(DBI::dbGetQuery(con, "SELECT count(*) AS samples FROM discrete.samples"))
print(DBI::dbGetQuery(con, "SELECT count(*) AS documents FROM files.documents"))
DBI::dbDisconnect(con)
cat("Final audit passed: Patch 69, no legacy import tables or duplicated source-file fields, complete new-upload links and protected workbook provenance.\n")
