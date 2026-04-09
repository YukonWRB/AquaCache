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

  expect_match(html, "Schema TOC")
  expect_match(html, "id=\"schema-continuous\"")
  expect_match(html, "id=\"schema-public\"")
  expect_no_match(html, "id=\"schema-audit\"")
  expect_match(html, "id=\"search-status\"")
  expect_match(html, "id=\"expand-all\"")
})
