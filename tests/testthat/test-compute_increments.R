make_ts <- function(vals, start = "2025-06-01 00:00:00", by = "hour") {
  data.frame(
    datetime = as.POSIXct(
      seq(
        from = as.POSIXct(start, tz = "UTC"),
        by = by,
        length.out = length(vals)
      ),
      tz = "UTC"
    ),
    value = vals
  )
}
test_that("basic increasing series yields correct increments", {
  ts <- make_ts(c(0, 1, 3, 6))
  out <- compute_increments(ts, reset_drop = 20, min_pos = 0)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("datetime", "value"))
  expect_equal(nrow(out), nrow(ts))

  # First is NA; then 1, 2, 3
  expect_true(is.na(out$value[1]))
  expect_equal(out$value[-1], c(1, 2, 3))
  # no negatives
  expect_true(all(out$value[!is.na(out$value)] >= 0))
})

test_that("small negative wiggles are ignored (baseline holds)", {
  # drop by 1 (not a reset), then rebound; only counts rise above baseline
  ts <- make_ts(c(10, 9, 10.5, 11))
  out <- compute_increments(ts, reset_drop = 20, min_pos = 0)

  # First NA, then 0 (ignored wiggle), then 0.5, then 0.5
  expect_true(is.na(out$value[1]))
  expect_equal(out$value[2:4], c(0, 0.5, 0.5))
})

test_that("large drop triggers reset and increment at reset step is 0", {
  ts <- make_ts(c(50, 10, 12)) # drop of 40 -> reset
  out <- compute_increments(ts, reset_drop = 20, min_pos = 0)

  # At reset step: 0; then increments from new baseline
  expect_true(is.na(out$value[1]))
  expect_equal(out$value[2:3], c(0, 2))
})

test_that("reset occurs when drop equals the threshold (<= -reset_drop)", {
  ts <- make_ts(c(30, 10, 11)) # drop of 20 exactly
  out <- compute_increments(ts, reset_drop = 20, min_pos = 0)

  expect_true(is.na(out$value[1]))
  expect_equal(out$value[2:3], c(0, 1))
})

test_that("min_pos filters tiny positives; strict greater-than behavior", {
  ts <- make_ts(c(0, 0.01, 0.02, 0.05))
  out <- compute_increments(ts, reset_drop = 20, min_pos = 0.02)

  # add == min_pos is not counted (strict >)
  # 0.01 -> ignored, 0.02 -> ignored, 0.03 -> counted
  expect_true(is.na(out$value[1]))
  expect_equal(out$value[2:4], c(0, 0, 0.03))
})

test_that("NAs in value propagate to increments", {
  ts <- make_ts(c(0, 1, NA, 3, 4))
  out <- compute_increments(ts)

  # d at i=3 is NA, so inc[3] stays NA; i=4 uses NA prev -> NA; i=5 uses prev=3 -> valid
  expect_true(is.na(out$value[1]))
  expect_true(is.na(out$value[3]))
  expect_true(is.na(out$value[4]))
  expect_equal(out$value[5], 1)
})

test_that("unsorted input is sorted by datetime before computation", {
  # Reverse the order intentionally
  ts <- make_ts(c(0, 1, 3, 6))
  ts <- ts[4:1, ] # unsorted
  out <- compute_increments(ts, reset_drop = 20, min_pos = 0)

  # Output should be sorted and equivalent to basic case
  expect_true(is.unsorted(ts$datetime)) # input unsorted
  expect_false(is.unsorted(out$datetime)) # output sorted
  expect_true(is.na(out$value[1]))
  expect_equal(out$value[-1], c(1, 2, 3))
})

test_that("output always has non-negative increments (excluding NA)", {
  ts <- make_ts(c(10, 9, 10.5, 10, 12, 11.5, 30)) # wiggles and rebounds
  out <- compute_increments(ts)
  expect_true(all(out$value[!is.na(out$value)] >= 0))
})

test_that("error when less than two points are provided", {
  ts1 <- make_ts(5)[1, , drop = FALSE]
  expect_error(compute_increments(ts1), "at least two points")
})
