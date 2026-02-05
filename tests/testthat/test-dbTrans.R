test_that("dbTransBegin starts a new transaction when none is active", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  expect_true(dbTransBegin(con))
  expect_true(dbTransCheck(con))
})

test_that("dbTransBegin returns FALSE when transaction already active", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  expect_true(dbTransBegin(con))
  res <- expect_message(
    dbTransBegin(con, silent = FALSE),
    "Transaction already active"
  )
  expect_false(suppressMessages(dbTransBegin(con, silent = FALSE)))
})

test_that("dbTransCheck activates a transaction started with BEGIN", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  DBI::dbExecute(con, "BEGIN;")
  expect_true(dbTransCheck(con))
})

test_that("dbTransCheck returns FALSE when no transaction is active", {
  con <- connect_test()
  on.exit(cleanup_postgres_session(con))

  expect_false(dbTransCheck(con))
})
