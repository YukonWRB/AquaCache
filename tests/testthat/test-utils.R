# Tests for utility functions in utils.R

# Tests on fmt function ##############
test_that("fmt formats datetimes as UTC strings", {
  time <- as.POSIXct("2024-01-01 00:00:00", tz = "Etc/GMT+2")
  expect_equal(fmt(time), "2024-01-01 02:00:00")

  utc_times <- as.POSIXct(
    c("2024-06-01 12:34:56", "2024-06-02 01:02:03"),
    tz = "UTC"
  )
  expect_equal(fmt(utc_times), c("2024-06-01 12:34:56", "2024-06-02 01:02:03"))
})


# Tests on inf_to_na function ##############
test_that("inf_to_na returns NAs on data.frames", {
  test <- data.frame(
    a = c(1, 2, Inf, Inf, 5),
    b = c(-Inf, 3, 4, -Inf, 6),
    c = c(NaN, 2, 3, 4, NaN)
  )
  result <- inf_to_na(test)
  # Ensure that no Inf or -Inf values remain
  expect_false(any(is.infinite(result$a)))
  expect_false(any(is.infinite(result$b)))
  expect_false(any(is.infinite(result$c)))
})

test_that("inf_to_na returns NAs on data.tables", {
  test <- data.table::data.table(
    a = c(1, 2, Inf, Inf, 5),
    b = c(-Inf, 3, 4, -Inf, 6),
    c = c(NaN, 2, 3, 4, NaN)
  )
  result <- inf_to_na(test)
  # Ensure that no Inf or -Inf values remain
  expect_false(any(is.infinite(result$a)))
  expect_false(any(is.infinite(result$b)))
  expect_false(any(is.infinite(result$c)))
})

test_that("inf_to_na returns NAs on vectors", {
  # positive inf check
  test <- c(1, 2, Inf, Inf, 5)
  result <- inf_to_na(test)
  expect_false(any(is.infinite(result)))

  test <- c(1, 2, -Inf, -Inf, 5)
  result <- inf_to_na(test)
  expect_false(any(is.infinite(result)))

  test <- c(1, 2, NaN, NaN)
  result <- inf_to_na(test)
  expect_false(any(is.infinite(result)))
})


test_that("PostgreSQL utility checks reject wrappers without a real client", {
  ext <- if (.Platform$OS.type == "windows") ".bat" else ".sh"
  broken <- tempfile("psql-broken-", fileext = ext)

  if (.Platform$OS.type == "windows") {
    writeLines(
      c(
        "@echo off",
        "echo Error: You must install at least one postgresql-client-<version> package 1>&2",
        "exit /B 1"
      ),
      broken
    )
  } else {
    writeLines(
      c(
        "#!/bin/sh",
        "echo 'Error: You must install at least one postgresql-client-<version> package' >&2",
        "exit 1"
      ),
      broken
    )
    Sys.chmod(broken, mode = "0755")
  }

  info <- aquacache_postgres_utility_info(broken)
  expect_false(info$available)
  expect_true(is.na(info$major))
})


test_that("PostgreSQL utility checks keep working utilities with unparsed versions", {
  ext <- if (.Platform$OS.type == "windows") ".bat" else ".sh"
  utility <- tempfile("psql-custom-", fileext = ext)

  if (.Platform$OS.type == "windows") {
    writeLines(
      c(
        "@echo off",
        "echo custom PostgreSQL utility"
      ),
      utility
    )
  } else {
    writeLines(
      c(
        "#!/bin/sh",
        "echo 'custom PostgreSQL utility'"
      ),
      utility
    )
    Sys.chmod(utility, mode = "0755")
  }

  info <- aquacache_postgres_utility_info(utility)
  expect_true(info$available)
  expect_true(is.na(info$major))
})
