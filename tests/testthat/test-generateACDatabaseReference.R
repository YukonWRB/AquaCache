test_that("generateACDatabaseReference creates filtered HTML output", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)

  out_file <- tempfile(fileext = ".html")

  out <- generateACDatabaseReference(
    con = con,
    output_file = out_file,
    schemas = c("continuous", "public"),
    check_visibility = FALSE,
    open = FALSE
  )

  expect_true(file.exists(out))
  expect_equal(normalizePath(out, winslash = "/", mustWork = TRUE), normalizePath(out_file, winslash = "/", mustWork = TRUE))

  html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

  expect_match(html, "<!DOCTYPE html>")
  expect_match(html, "Schema TOC")
  expect_match(html, "id=\"schema-continuous\"")
  expect_match(html, "id=\"schema-public\"")
  expect_no_match(html, "id=\"schema-audit\"")
  expect_match(html, "id=\"search-status\"")
  expect_match(html, "id=\"expand-all\"")
  expect_no_match(html, "<details class=\"schema-group schema-group-tables\" open>")
  expect_no_match(html, "<details class=\"schema-group schema-group-views\" open>")
})

test_that("generateACDatabaseReference can emit an embeddable HTML fragment", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)

  out_file <- tempfile(fileext = ".html")

  out <- generateACDatabaseReference(
    con = con,
    output_file = out_file,
    standalone = FALSE,
    schemas = "public",
    check_visibility = FALSE,
    open = FALSE
  )

  expect_true(file.exists(out))

  html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

  expect_no_match(html, "<!DOCTYPE html>")
  expect_no_match(html, "<html lang=\"en\">")
  expect_no_match(html, "<body>")
  expect_match(html, "<div id=\"acdb-reference-root\" class=\"acdb-reference acdb-reference-embedded\">")
  expect_match(html, "<style>")
  expect_match(html, "id=\"search-status\"")
  expect_match(html, "\\.acdb-reference \\.page\\{")
  expect_match(html, "\\.acdb-reference-embedded\\{width:100vw;max-width:none;")
  expect_no_match(html, "body\\{margin:0;background:")
})

test_that("generateACDatabaseReference discovers visible schemas when schemas is NULL", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)

  out_file <- tempfile(fileext = ".html")

  out <- generateACDatabaseReference(
    con = con,
    output_file = out_file,
    schemas = NULL,
    check_visibility = FALSE,
    open = FALSE
  )

  expect_true(file.exists(out))

  html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")

  expect_match(html, "id=\"schema-public\"")
  expect_no_match(html, "id=\"schema-information-schema\"")
  expect_no_match(html, "id=\"schema-pg-")
})
