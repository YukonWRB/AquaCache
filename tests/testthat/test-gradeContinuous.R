# Pure-function tests for gradeContinuous() helpers and rules. They need no
# database; the database path was exercised against a full copy of NWT's
# database inside rolled-back transactions.

hourly <- function(n, start = "2026-01-01 00:00:00") {
  as.POSIXct(start, tz = "UTC") + (seq_len(n) - 1) * 3600
}
ctx_hourly <- list(utc_offset = -7, rate_s = 3600)

test_that("rolling helpers match brute force and zoo's alignment", {
  x <- c(1, 4, 2, 8, 5, 7, NA, 3, 6, 9, 2, 4)
  k <- 5
  centred <- vapply(seq_along(x), function(i) {
    if (i - 2 < 1 || i + 2 > length(x)) return(NA_real_)
    mean(x[(i - 2):(i + 2)])
  }, numeric(1))
  trailing <- vapply(seq_along(x), function(i) {
    if (i < k) return(NA_real_)
    mean(x[(i - k + 1):i])
  }, numeric(1))
  expect_equal(qc_roll_mean(x, k), centred)
  expect_equal(qc_roll_mean(x, k, align = "right"), trailing)

  # Even width: zoo's centre is offsets -(k/2 - 1) .. k/2.
  k <- 4
  even <- vapply(seq_along(x), function(i) {
    if (i - 1 < 1 || i + 2 > length(x)) return(NA_real_)
    mean(x[(i - 1):(i + 2)])
  }, numeric(1))
  expect_equal(qc_roll_mean(x, k), even)

  sd_bf <- vapply(seq_along(x), function(i) {
    if (i - 2 < 1 || i + 2 > length(x)) return(NA_real_)
    stats::sd(x[(i - 2):(i + 2)])
  }, numeric(1))
  expect_equal(qc_roll_sd(x, 5), sd_bf)
})

test_that("rolling windows give no result across a gap in the record", {
  dt <- as.POSIXct("2026-01-01", tz = "UTC") + c(0:9, 15:24) * 3600
  expect_equal(
    qc_window_span_ok(dt, 3, "right", 3600),
    c(FALSE, FALSE, rep(TRUE, 8), FALSE, FALSE, rep(TRUE, 8))
  )
  expect_equal(
    qc_window_span_ok(dt, 5, "center", 3600),
    c(FALSE, FALSE, rep(TRUE, 6), rep(FALSE, 4), rep(TRUE, 6), FALSE, FALSE)
  )
})

test_that("one code per reading by severity", {
  a <- c(NA, "CT", "CT", NA, "SUS")
  b <- c("PSM", "PSM", NA, NA, "ERROR")
  expect_equal(qc_combine_codes(list(a, b)), c("PSM", "PSM", "CT", NA, "ERROR"))
})

test_that("runs become ranges, single readings included", {
  dt <- hourly(6)
  r <- qc_runs_to_ranges(dt, c("UNS", "UNS", "CT", "UNS", "PSM", "PSM"))
  expect_equal(r$code, c("UNS", "CT", "UNS", "PSM"))
  expect_equal(r$start_dt, dt[c(1, 3, 4, 5)])
  expect_equal(r$end_dt, dt[c(2, 3, 4, 6)])
  expect_equal(r$n, c(2L, 1L, 1L, 2L))
  expect_equal(nrow(qc_runs_to_ranges(dt[0], character(0))), 0L)
})

test_that("rain while cold is CT", {
  x <- data.frame(
    datetime = hourly(4),
    value = c(0.2, 0, 0.5, 0.3),
    air_temp = c(-1, -5, 2, NA)
  )
  a <- list(max_air_temp = -0.1, qualifier = "CT")
  expect_equal(qc_rain_cold(x, a, ctx_hourly), c("CT", NA, NA, NA))
})

test_that("stuck sensor flags the middle of a constant run, splitting zeros", {
  a <- list(width = 5, min_sd = 0.001, qualifier = "ERROR", zero_split = TRUE,
            zero_max_air_temp = 0, zero_qualifier = "SUS")
  x <- data.frame(
    datetime = hourly(16),
    value = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 5, 0, 0, 0, 0, 0, 0),
    air_temp = c(rep(5, 10), -2, -2, -2, 3, 3, 3)
  )
  out <- qc_stuck_sensor(x, a, ctx_hourly)
  # Six 3s -> the middle two are stuck (N - 4).
  expect_equal(out[1:10], c(NA, NA, NA, NA, "ERROR", "ERROR", NA, NA, NA, NA))
  # Zeros: stuck where the full window fits; SUS when cold, nothing when warm.
  expect_equal(out[13:14], c("SUS", NA))
})

test_that("direction while calm is ERROR", {
  x <- data.frame(datetime = hourly(3), value = c(90, 180, 270), wind_speed = c(0, 1.5, NA))
  a <- list(max_wind_speed = 0, qualifier = "ERROR")
  expect_equal(qc_dir_calm(x, a, ctx_hourly), c("ERROR", NA, NA))
})

test_that("snowmelt: trailing mean flags warm rain after a cold spell", {
  n <- 60
  x <- data.frame(
    datetime = hourly(n, "2026-03-01 07:00:00"),
    value = 0,
    air_temp = c(rep(-10, 50), rep(3, 10))
  )
  x$value[55] <- 1.2
  a <- list(first_warm_days = 0, cold_window = 24, cold_align = "trailing", qualifier = "PSM")
  out <- qc_snowmelt_tips(x, a, ctx_hourly)
  expect_equal(which(!is.na(out)), 55L)
  # A gap inside the window means no mean, so no flag.
  x_gap <- x[-(40:45), ]
  out_gap <- qc_snowmelt_tips(x_gap, a, ctx_hourly)
  expect_true(all(is.na(out_gap)))
})

test_that("rule arguments merge catalogue defaults with assignment overrides", {
  skip_if_not_installed("jsonlite")
  schema <- '{"schema_version":1,"arguments":[{"name":"width","type":"integer","default":5},{"name":"min_sd","type":"number","default":0.001}]}'
  expect_equal(qc_merge_rule_args(schema, NA), list(width = 5L, min_sd = 0.001))
  expect_equal(qc_merge_rule_args(schema, '{"min_sd":0.1}'), list(width = 5L, min_sd = 0.1))
})
