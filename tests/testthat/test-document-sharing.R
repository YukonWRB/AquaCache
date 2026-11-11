test_that("whole-document sharing includes hidden samples and location restrictions", {
  skip_if(Sys.getenv("AQUACACHE_TEST_PATCH69") != "true", "Patch 69 testdb opt-in required")
  con <- AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
  on.exit(DBI::dbDisconnect(con))
  stopifnot(DBI::dbGetQuery(con, "SELECT current_database() AS db")$db == "testdb")
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  prefix <- paste0("sharing69_", Sys.getpid())
  groups <- paste0(prefix, c("_a", "_b"))
  editor <- paste0(prefix, "_editor")
  reader <- paste0(prefix, "_reader")
  for (role in c(groups, editor, reader)) {
    DBI::dbExecute(con, paste("CREATE ROLE", DBI::dbQuoteIdentifier(con, role)))
    DBI::dbExecute(con, paste("GRANT USAGE ON SCHEMA public,discrete,files TO", DBI::dbQuoteIdentifier(con, role)))
    DBI::dbExecute(con, paste("GRANT SELECT ON discrete.samples,discrete.sample_documents,public.locations,files.documents TO",
      DBI::dbQuoteIdentifier(con, role)))
  }
  DBI::dbExecute(con, paste("GRANT", DBI::dbQuoteIdentifier(con, groups[[1]]), "TO", DBI::dbQuoteIdentifier(con, editor)))
  DBI::dbExecute(con, paste("GRANT UPDATE ON files.documents TO", DBI::dbQuoteIdentifier(con, editor)))
  for (role in c(editor, reader)) DBI::dbExecute(con, paste(
    "GRANT EXECUTE ON FUNCTION discrete.get_document_sharing_groups(integer[]) TO", DBI::dbQuoteIdentifier(con, role)))

  location <- DBI::dbGetQuery(con,
    "INSERT INTO public.locations(location_code,name,latitude,longitude)
     VALUES ($1,'Document sharing regression',60,-135) RETURNING location_id", params = list(prefix))$location_id
  samples <- integer()
  for (i in 1:2) samples <- c(samples, DBI::dbGetQuery(con,
    "INSERT INTO discrete.samples(location_id,media_id,datetime,collection_method,sample_type,owner)
     SELECT $1,media_id,'2026-01-01'::timestamptz+$2::integer*interval '1 hour',collection_method,sample_type,owner
     FROM discrete.samples ORDER BY sample_id LIMIT 1 RETURNING sample_id", params = list(location, i))$sample_id)
  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  writeBin(charToRaw(prefix), file)
  type <- DBI::dbGetQuery(con, "SELECT document_type_en FROM files.document_types ORDER BY document_type_id LIMIT 1")$document_type_en
  document <- insertACDocument(file, prefix, type, "Document sharing regression", con = con)$new_document_id
  DBI::dbExecute(con, "INSERT INTO discrete.sample_documents(sample_id,document_id) VALUES ($1,$3),($2,$3)",
    params = list(samples[[1]], samples[[2]], document))

  sharing <- function() jsonlite::fromJSON(DBI::dbGetQuery(con,
    "SELECT array_to_json(allowed_groups)::text AS groups FROM discrete.get_document_sharing_groups(ARRAY[$1::integer])",
    params = list(document))$groups[[1]])
  database_error <- function(sql, params = NULL, pattern) {
    DBI::dbExecute(con, "SAVEPOINT sharing_failure")
    if (is.null(params)) {
      sql <- paste(sql, "WHERE $1::boolean")
      params <- list(TRUE)
    }
    statement <- DBI::dbSendQuery(con, sql)
    expect_error(tryCatch({
      DBI::dbBind(statement, params)
      DBI::dbFetch(statement)
    }, finally = DBI::dbClearResult(statement)), pattern)
    DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT sharing_failure")
  }
  expect_identical(sharing(), "public_reader")
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1,$2] WHERE sample_id=$3",
    params = list(groups[[1]], groups[[2]], samples[[1]]))
  expect_identical(sharing(), groups)
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1] WHERE sample_id=$2",
    params = list(groups[[1]], samples[[2]]))
  expect_identical(sharing(), groups[[1]])

  # The editor can modify this document, but the second linked sample is hidden.
  DBI::dbExecute(con, "UPDATE files.documents SET share_with=ARRAY[$1] WHERE document_id=$2", params = list(groups[[1]], document))
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1] WHERE sample_id=$2", params = list(groups[[2]], samples[[2]]))
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY['public_reader'] WHERE sample_id=$1", params = list(samples[[1]]))
  DBI::dbExecute(con, paste("SET LOCAL ROLE", DBI::dbQuoteIdentifier(con, editor)))
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT sample_id FROM discrete.samples WHERE sample_id=ANY(ARRAY[$1::integer,$2::integer])",
    params = as.list(samples))), 1L)
  expect_identical(sharing(), groups[[2]])
  # The old visible-only calculation returned public_reader here. The complete
  # result is private B, and ordinary caller RLS rejects applying that group.
  database_error("UPDATE files.documents SET share_with=ARRAY[$1] WHERE document_id=$2 RETURNING document_id",
    list(groups[[2]], document), "row-level security")
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[2147483647])", pattern = "unavailable or cannot be modified")
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[$1::integer,2147483647])",
    list(document), "unavailable or cannot be modified")
  DBI::dbExecute(con, "RESET ROLE")
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1] WHERE sample_id=$2", params = list(groups[[1]], samples[[1]]))
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[$1::integer])", list(document), "no access group shared")

  # Public sample ACLs cannot override a private parent location.
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY['public_reader'] WHERE location_id=$1", params = list(location))
  DBI::dbExecute(con, "UPDATE public.locations SET share_with=ARRAY[$1] WHERE location_id=$2", params = list(groups[[2]], location))
  DBI::dbExecute(con, paste("SET LOCAL ROLE", DBI::dbQuoteIdentifier(con, editor)))
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT sample_id FROM discrete.samples WHERE location_id=$1", params = list(location))), 0L)
  expect_identical(sharing(), groups[[2]])
  DBI::dbExecute(con, "RESET ROLE")
  DBI::dbExecute(con, "UPDATE files.documents SET share_with=ARRAY[$1] WHERE document_id=$2", params = list(groups[[2]], document))
  DBI::dbExecute(con, paste("SET LOCAL ROLE", DBI::dbQuoteIdentifier(con, editor)))
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[$1::integer])", list(document), "unavailable or cannot be modified")
  DBI::dbExecute(con, "RESET ROLE")
  DBI::dbExecute(con, paste("SET LOCAL ROLE", DBI::dbQuoteIdentifier(con, reader)))
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[$1::integer])", list(document), "unavailable or cannot be modified")
  DBI::dbExecute(con, "RESET ROLE")
  expect_false(DBI::dbGetQuery(con,
    "SELECT has_function_privilege('public_reader','discrete.get_document_sharing_groups(integer[])','EXECUTE') AS allowed")$allowed)
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[NULL::integer])", pattern = "positive integers")
  # A locationless QC sample still contributes its own sharing restriction.
  qc_type <- DBI::dbGetQuery(con,
    "SELECT sample_type_id FROM discrete.sample_types WHERE NOT requires_location ORDER BY sample_type_id LIMIT 1")$sample_type_id
  stopifnot(length(qc_type) == 1L)
  DBI::dbExecute(con, "UPDATE discrete.samples SET location_id=NULL,sample_type=$1,share_with=ARRAY[$2] WHERE sample_id=$3",
    params = list(qc_type, groups[[1]], samples[[1]]))
  DBI::dbExecute(con, "DELETE FROM discrete.sample_documents WHERE sample_id=$1 AND document_id=$2", params = list(samples[[2]], document))
  expect_identical(sharing(), groups[[1]])
  DBI::dbExecute(con, "DELETE FROM discrete.sample_documents WHERE document_id=$1", params = list(document))
  database_error("SELECT * FROM discrete.get_document_sharing_groups(ARRAY[$1::integer])", list(document), "no access group shared")
})
