test_that("best-effort qualifier range subtraction keeps unmatched intervals", {
  start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  end <- start + 100
  cuts <- data.frame(
    start_dt = start + c(20, 25, 70),
    end_dt = start + c(30, 40, 80)
  )

  remaining <- qc_subtract_qualifier_ranges(start, end, cuts)

  expect_equal(nrow(remaining), 3L)
  expect_equal(
    as.numeric(remaining$start_dt),
    as.numeric(start) + c(0, 40.000001, 80.000001),
    tolerance = 1e-7
  )
  expect_equal(
    as.numeric(remaining$end_dt),
    as.numeric(start) + c(19.999999, 69.999999, 100),
    tolerance = 1e-7
  )
})

test_that("best-effort qualifier range subtraction handles boundaries", {
  start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  end <- start + 10

  whole <- qc_subtract_qualifier_ranges(
    start,
    end,
    data.frame(start_dt = start, end_dt = end)
  )
  before <- qc_subtract_qualifier_ranges(
    start,
    end,
    data.frame(start_dt = start - 10, end_dt = start - 1)
  )
  after <- qc_subtract_qualifier_ranges(
    start,
    end,
    data.frame(start_dt = end + 1, end_dt = end + 10)
  )
  one_edge <- qc_subtract_qualifier_ranges(
    start,
    end,
    data.frame(start_dt = start, end_dt = start)
  )

  expect_equal(nrow(whole), 0L)
  expect_equal(as.numeric(before$start_dt), as.numeric(start))
  expect_equal(as.numeric(before$end_dt), as.numeric(end))
  expect_equal(as.numeric(after$start_dt), as.numeric(start))
  expect_equal(as.numeric(after$end_dt), as.numeric(end))
  expect_equal(nrow(one_edge), 1L)
  expect_gt(as.numeric(one_edge$start_dt[[1L]]), as.numeric(start))
})

test_that("best effort keeps output still produced by another rule", {
  start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  datetime <- start + 0:4
  preview_with <- list(
    readings = data.frame(
      datetime = datetime,
      code = c("PSM", "PSM", "CT", "CT", "UNS")
    ),
    ranges = data.frame(
      code = c("PSM", "CT", "UNS"),
      start_dt = datetime[c(1L, 3L, 5L)],
      end_dt = datetime[c(2L, 4L, 5L)],
      n = c(2L, 2L, 1L)
    )
  )
  preview_without <- list(
    readings = data.frame(
      datetime = datetime,
      code = c("PSM", "UNS", "UNS", "CT", "UNS")
    )
  )

  candidates <- qc_unique_rule_output_ranges(
    preview_with,
    preview_without,
    emitted_codes = c("PSM", "CT")
  )

  expect_equal(candidates$code, c("PSM", "CT"))
  expect_equal(as.numeric(candidates$start_dt), as.numeric(datetime[2:3]))
  expect_equal(as.numeric(candidates$end_dt), as.numeric(datetime[2:3]))
})

test_that("best effort skips protected readings that the grader would not write", {
  start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  datetime <- start + 0:4
  preview_with <- list(
    readings = data.frame(
      datetime = datetime,
      code = c("PSM", "PSM", "CT", "CT", "UNS")
    ),
    ranges = data.frame(
      code = c("PSM", "CT", "UNS"),
      start_dt = datetime[c(1L, 4L, 5L)],
      end_dt = datetime[c(2L, 4L, 5L)],
      n = c(2L, 1L, 1L)
    )
  )
  preview_without <- list(
    readings = data.frame(
      datetime = datetime,
      code = c("PSM", "UNS", "UNS", "CT", "UNS")
    )
  )

  candidates <- qc_unique_rule_output_ranges(
    preview_with,
    preview_without,
    emitted_codes = c("PSM", "CT")
  )

  expect_equal(candidates$code, "PSM")
  expect_equal(as.numeric(candidates$start_dt), as.numeric(datetime[[2L]]))
  expect_equal(as.numeric(candidates$end_dt), as.numeric(datetime[[2L]]))
})

test_that("best effort removes unique output when the remaining code is missing", {
  datetime <- as.POSIXct("2026-01-01", tz = "UTC") + 0:3
  with_rule <- list(
    readings = data.frame(datetime = datetime, code = c("CT", "CT", "CT", NA)),
    ranges = data.frame(code = "CT", start_dt = datetime[1], end_dt = datetime[3], n = 3L)
  )
  without_rule <- list(readings = data.frame(
    datetime = datetime, code = c(NA, "CT", "UNS", NA)
  ))

  candidates <- qc_unique_rule_output_ranges(with_rule, without_rule, "CT")

  expect_equal(candidates$code, rep("CT", 2))
  expect_equal(candidates$start_dt, datetime[c(1, 3)])
  expect_equal(candidates$end_dt, datetime[c(1, 3)])
  expect_equal(candidates$n, c(1L, 1L))
})
