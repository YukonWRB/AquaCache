skip_if_no_postgres <- function() {
  testthat::skip_if_not_installed("RPostgres")
  required <- c(
    "aquacacheName",
    "aquacacheHost",
    "aquacachePort",
    "aquacacheAdminUser",
    "aquacacheAdminPass"
  )
  missing <- required[Sys.getenv(required, unset = "") == ""]
  if (length(missing) > 0) {
    testthat::skip(
      paste(
        "Postgres test database credentials not available:",
        paste(missing, collapse = ", ")
      )
    )
  }
}

connect_postgres_test_db <- function() {
  skip_if_no_postgres()
  tryCatch(
    {
      con <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("aquacacheName"),
        host = Sys.getenv("aquacacheHost"),
        port = Sys.getenv("aquacachePort"),
        user = Sys.getenv("aquacacheAdminUser"),
        password = Sys.getenv("aquacacheAdminPass")
      )
      DBI::dbExecute(con, "SET timezone = 'UTC'")
      con
    },
    error = function(err) {
      testthat::skip(paste(
        "Unable to connect to Postgres test database:",
        err$message
      ))
    }
  )
}

cleanup_postgres_session <- function(con) {
  try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
  DBI::dbDisconnect(con)
}

test_that("dbTransBegin starts a new transaction when none is active", {
  con <- connect_postgres_test_db()
  on.exit(cleanup_postgres_session(con))

  expect_true(dbTransBegin(con))
  expect_true(dbTransCheck(con))
})

test_that("dbTransBegin returns FALSE when transaction already active", {
  con <- connect_postgres_test_db()
  on.exit(cleanup_postgres_session(con))

  expect_true(dbTransBegin(con))
  res <- expect_message(
    dbTransBegin(con, silent = FALSE),
    "Transaction already active"
  )
  expect_false(suppressMessages(dbTransBegin(con, silent = FALSE)))
})

test_that("dbTransCheck activates a transaction started with BEGIN", {
  con <- connect_postgres_test_db()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")
  expect_true(dbTransCheck(con))
})

test_that("dbTransCheck returns FALSE when no transaction is active", {
  con <- connect_postgres_test_db()
  on.exit(cleanup_postgres_session(con))

  expect_false(dbTransCheck(con))
})
