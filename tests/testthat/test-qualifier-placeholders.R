test_that("separate qualifier runs do not bridge a protected placeholder", {
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")
  current <- data.frame(
    qualifier_id = 1L, timeseries_id = 1L, qualifier_type_id = 1L,
    start_dt = t0 + 4 * 3600, end_dt = t0 + 5 * 3600,
    no_source_update = TRUE
  )
  incoming <- data.frame(
    datetime = t0 + c(0, 1, 2, 3, 8, 9) * 3600,
    qualifier = c(3L, 3L, 2L, 2L, 3L, 3L),
    rank = 1L, no_source_update = FALSE
  )
  local_mocked_bindings(dbGetQuery = function(...) current, .package = "DBI")
  proposed <- NULL
  local_mocked_bindings(
    reconcile_segment_changes = function(..., proposed_state) {
      proposed <<- proposed_state
    },
    .package = "AquaCache"
  )

  result <- reconcile_qualifier_placeholders(NULL, 1L, incoming, c(1L, 2L), TRUE)

  expect_identical(result, incoming)
  expect_equal(proposed, current)
  # Without the intervening different code, this really is one continuous
  # incoming interval, and must still be blocked by the protected placeholder.
  expect_warning(
    blocked <- reconcile_qualifier_placeholders(
      NULL, 1L, incoming[incoming$qualifier == 3L, ], c(1L, 2L), TRUE
    ),
    "interval overlaps"
  )
  expect_equal(nrow(blocked), 0L)
})

test_that("placeholder trimming preserves gaps between distinct incoming runs", {
  t0 <- as.POSIXct("2026-01-01", tz = "UTC")
  current <- data.frame(
    qualifier_id = 1L, timeseries_id = 1L, qualifier_type_id = 2L,
    start_dt = t0, end_dt = t0 + 10 * 3600,
    no_source_update = FALSE
  )
  incoming <- data.frame(
    datetime = t0 + c(0, 1, 3, 4, 8, 9) * 3600,
    qualifier = c(3L, 3L, 2L, 2L, 3L, 3L),
    rank = 1L, no_source_update = FALSE
  )
  local_mocked_bindings(dbGetQuery = function(...) current, .package = "DBI")
  proposed <- NULL
  local_mocked_bindings(
    reconcile_segment_changes = function(..., proposed_state) {
      proposed <<- proposed_state
    },
    .package = "AquaCache"
  )

  result <- reconcile_qualifier_placeholders(NULL, 1L, incoming, c(1L, 2L), TRUE)

  expect_identical(result, incoming)
  expect_equal(proposed$start_dt, t0 + c(1, 9) * 3600)
  expect_equal(proposed$end_dt, t0 + c(8, 10) * 3600)
  expect_equal(proposed$qualifier_id, c(1L, NA_integer_))
})
