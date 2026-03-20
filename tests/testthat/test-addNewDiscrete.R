test_that("addNewDiscrete inserts a new sample and results", {
  testthat::skip_on_cran()

  con <- connect_test()
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  dbTransBegin(con)
  on.exit(DBI::dbExecute(con, "ROLLBACK;"), add = TRUE, after = FALSE)

  sample_template <- DBI::dbGetQuery(con, "SELECT * FROM samples LIMIT 1")
  if (nrow(sample_template) == 0) {
    testthat::skip("No sample data available for addNewDiscrete test.")
  }
  results_template <- DBI::dbGetQuery(con, "SELECT * FROM results LIMIT 1")
  if (nrow(results_template) == 0) {
    testthat::skip("No results data available for addNewDiscrete test.")
  }

  sample <- sample_template[1, , drop = FALSE]
  max_dt <- DBI::dbGetQuery(con, "SELECT MAX(datetime) FROM samples")[[1]]
  if (is.na(max_dt)) {
    max_dt <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  }
  sample$datetime <- max_dt + lubridate::dhours(1)
  sample <- sample[, setdiff(names(sample), "sample_id"), drop = FALSE]

  results <- results_template[1, , drop = FALSE]
  results <- results[,
    setdiff(names(results), c("result_id", "sample_id")),
    drop = FALSE
  ]

  sample_id <- addNewDiscrete(con, sample, results)

  inserted_sample <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM samples WHERE sample_id = $1",
    params = list(sample_id)
  )[[1]]
  inserted_results <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM results WHERE sample_id = $1",
    params = list(sample_id)
  )[[1]]

  expect_equal(inserted_sample, 1)
  expect_equal(inserted_results, nrow(results))
})
