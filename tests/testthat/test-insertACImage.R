test_that("insertACImage binds quote-containing descriptions and tags", {
  skip_if(tolower(Sys.getenv("aquacacheName")) != "testdb")
  con <- connect_test()
  on.exit(cleanup_postgres_session(con), add = TRUE)
  DBI::dbBegin(con)

  image_type <- DBI::dbGetQuery(
    con,
    "SELECT image_type_id FROM files.image_types ORDER BY image_type_id LIMIT 1"
  )$image_type_id[[1]]
  path <- tempfile(fileext = ".png")
  writeBin(
    c(as.raw(c(137, 80, 78, 71)), charToRaw(as.character(Sys.time()))),
    path
  )
  on.exit(unlink(path), add = TRUE)

  expect_true(insertACImage(
    object = path,
    datetime = as.POSIXct(Sys.time(), tz = "UTC"),
    image_type = image_type,
    description = "Image description with apostrophe's text",
    tags = c("tag's", "tag,comma"),
    latitude = 60,
    longitude = -135,
    con = con
  ))
  saved <- DBI::dbGetQuery(
    con,
    "SELECT i.description, tag
     FROM files.images i
     CROSS JOIN LATERAL unnest(i.tags) AS tags(tag)
     WHERE i.file_hash = md5($1)",
    params = list(paste0(
      readBin(path, "raw", n = file.info(path)$size),
      collapse = ""
    ))
  )

  expect_identical(
    saved$description[[1]],
    "Image description with apostrophe's text"
  )
  expect_setequal(saved$tag, c("tag's", "tag,comma"))
})
