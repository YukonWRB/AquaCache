# Tests for transaction helper utilities in utils.R

# Provide a dummy connection object for clarity
fake_con <- structure(list(description = "fake connection"), class = "DBIConnection")

# Helper query used by both helpers
transaction_query <- "SELECT pg_current_xact_id_if_assigned() IS NOT NULL AS is_transaction;"


test_that("dbTransCheck triggers activity when initial check is inactive", {
  queries <- character()
  executes <- character()
  response_index <- 0
  responses <- list(
    data.frame(is_transaction = FALSE),
    data.frame(is_transaction = TRUE)
  )

  testthat::with_mocked_bindings(
    {
      result <- dbTransCheck(fake_con)
      expect_true(result)
      expect_equal(queries, rep(transaction_query, 2))
      expect_equal(executes, "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;")
    },
    `DBI::dbGetQuery` = function(con, statement) {
      queries <<- c(queries, statement)
      response_index <<- response_index + 1
      responses[[response_index]]
    },
    `DBI::dbExecute` = function(con, statement) {
      executes <<- c(executes, statement)
      invisible(0L)
    }
  )
})


test_that("dbTransBegin returns FALSE when transaction already active", {
  executions <- 0L

  testthat::with_mocked_bindings(
    {
      result <- dbTransBegin(fake_con)
      expect_false(result)
      expect_equal(executions, 0L)
    },
    dbTransCheck = function(con) TRUE,
    `DBI::dbExecute` = function(...) {
      executions <<- executions + 1L
      invisible(0L)
    }
  )
})


test_that("dbTransBegin starts a new transaction when inactive", {
  queries <- character()
  executes <- character()

  testthat::with_mocked_bindings(
    {
      result <- dbTransBegin(fake_con)
      expect_true(result)
      expect_equal(executes, c("BEGIN;", "CREATE TEMPORARY TABLE a (b int) ON COMMIT DROP;"))
      expect_equal(queries, transaction_query)
    },
    dbTransCheck = function(con) FALSE,
    `DBI::dbExecute` = function(con, statement) {
      executes <<- c(executes, statement)
      invisible(0L)
    },
    `DBI::dbGetQuery` = function(con, statement) {
      queries <<- c(queries, statement)
      data.frame(is_transaction = TRUE)
    }
  )
})
