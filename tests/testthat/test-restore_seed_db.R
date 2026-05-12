test_that("restore target validation rejects system databases", {
  expect_error(
    aquacache_validate_restore_target_name("postgres"),
    "system PostgreSQL database"
  )
  expect_error(
    aquacache_validate_restore_target_name("template0"),
    "system PostgreSQL database"
  )
  expect_error(
    aquacache_validate_restore_target_name("template1"),
    "system PostgreSQL database"
  )
  expect_equal(
    aquacache_validate_restore_target_name(" aquacache_seed "),
    "aquacache_seed"
  )
  expect_error(
    aquacache_validate_restore_target_name(paste(rep("a", 64), collapse = "")),
    "63 bytes"
  )
})


test_that("restore_seed_db refuses postgres before external commands", {
  sql_file <- tempfile(fileext = ".sql")
  writeLines("SELECT 1;", sql_file)

  expect_error(
    restore_seed_db(file = sql_file, name = "postgres"),
    "system PostgreSQL database"
  )
})


test_that("restore validation keeps maintenance database separate", {
  expect_error(
    aquacache_validate_restore_maintenance_db("admin_db", "admin_db"),
    "must be different"
  )
  expect_equal(
    aquacache_validate_restore_maintenance_db("postgres", "aquacache_seed"),
    "postgres"
  )
})


test_that("restore connection validation accepts numeric ports", {
  expect_true(aquacache_validate_restore_connection_arg(5432, "port"))
  expect_true(aquacache_validate_restore_connection_arg("5432", "port"))
  expect_error(
    aquacache_validate_restore_connection_arg("", "port"),
    "non-empty"
  )
})


test_that("seed dump metadata is parsed from sql and gz files", {
  lines <- c(
    "-- AquaCache seed database dump",
    "-- AquaCache package version: 2.7.7",
    "-- AquaCache patch number: 45",
    "-- AquaCache source database: aquacache",
    "CREATE SCHEMA information;"
  )

  sql_file <- tempfile(fileext = ".sql")
  writeLines(lines, sql_file)

  metadata <- aquacache_read_seed_dump_metadata(sql_file)
  expect_equal(metadata$package_version, "2.7.7")
  expect_equal(metadata$patch_number, "45")
  expect_equal(aquacache_seed_dump_metadata_patch(metadata), 45L)

  gz_file <- tempfile(fileext = ".sql.gz")
  con <- gzfile(gz_file, open = "wt")
  writeLines(lines, con)
  close(con)

  metadata_gz <- aquacache_read_seed_dump_metadata(gz_file)
  expect_equal(metadata_gz$source_database, "aquacache")
  expect_equal(aquacache_seed_dump_metadata_patch(metadata_gz), 45L)
})


test_that("public_reader role bootstrap creates or updates the login", {
  text <- aquacache_public_reader_role_sql()

  expect_match(
    text,
    "CREATE ROLE public_reader LOGIN PASSWORD ''aquacache''",
    fixed = TRUE
  )
  expect_match(
    text,
    "ALTER ROLE public_reader LOGIN PASSWORD ''aquacache''",
    fixed = TRUE
  )
  expect_match(text, "pg_catalog.pg_roles", fixed = TRUE)
})


test_that("restore role helper always runs create-or-alter block", {
  sql_seen <- NULL
  con <- structure(list(), class = "dummy_connection")

  local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      sql_seen <<- statement
      1L
    },
    .package = "DBI"
  )

  expect_true(aquacache_ensure_restore_roles(con))
  expect_match(
    sql_seen,
    "ALTER ROLE public_reader LOGIN PASSWORD ''aquacache''",
    fixed = TRUE
  )
})


test_that("gzipped sql dumps are materialized without changing contents", {
  sql <- "SELECT 'AquaCache restore test';\n"
  gz_file <- tempfile(fileext = ".sql.gz")
  con <- gzfile(gz_file, open = "wt")
  writeLines(sql, con, sep = "")
  close(con)

  materialized <- aquacache_materialize_sql_dump(gz_file)
  on.exit(materialized$cleanup(), add = TRUE)

  expect_true(file.exists(materialized$path))
  expect_equal(readLines(materialized$path, warn = FALSE), trimws(sql))
})
