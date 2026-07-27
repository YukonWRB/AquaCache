test_that("insertACDocument binds quote-containing metadata", {
  skip_if(tolower(Sys.getenv("aquacacheName")) != "testdb")
  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)
  DBI::dbBegin(con)

  document_type <- DBI::dbGetQuery(
    con,
    "SELECT document_type_en FROM files.document_types ORDER BY document_type_id LIMIT 1"
  )$document_type_en[[1]]
  path <- tempfile(fileext = ".txt")
  writeBin(charToRaw(paste0("security-document-", Sys.time())), path)
  on.exit(unlink(path), add = TRUE)

  name <- paste0("Security document ' ", format(Sys.time(), "%OS6"))
  result <- insertACDocument(
    path = path,
    name = name,
    type = document_type,
    description = "A description with apostrophe's and braces {safe}",
    tags = c("tag's", "tag,comma"),
    authors = c("O'Brien", "Example, Author"),
    con = con
  )
  saved <- DBI::dbGetQuery(
    con,
    "SELECT d.name, d.description, tag, author
     FROM files.documents d
     CROSS JOIN LATERAL unnest(d.tags) AS tags(tag)
     CROSS JOIN LATERAL unnest(d.authors) AS authors(author)
     WHERE d.document_id = $1",
    params = list(result$new_document_id)
  )

  expect_identical(saved$name[[1]], name)
  expect_identical(
    saved$description[[1]],
    "A description with apostrophe's and braces {safe}"
  )
  expect_setequal(unique(saved$tag), c("tag's", "tag,comma"))
  expect_setequal(unique(saved$author), c("O'Brien", "Example, Author"))
})
