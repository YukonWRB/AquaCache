test_that("inf_to_na returns NAs on data.frames", {
  test <- data.frame(a = c(1, 2, Inf, Inf, 5), b = c(-Inf, 3, 4, -Inf, 6), c = c(NaN, 2, 3, 4, NaN))
  result <- inf_to_na(test)
  # Ensure that no Inf or -Inf values remain
  expect_false(any(is.infinite(result$a)))
  expect_false(any(is.infinite(result$b)))
  expect_false(any(is.infinite(result$c)))
})

test_that("inf_to_na returns NAs on data.tables", {
  test <- data.table::data.table(a = c(1, 2, Inf, Inf, 5), b = c(-Inf, 3, 4, -Inf, 6), c = c(NaN, 2, 3, 4, NaN))
  result <- inf_to_na(test)
  # Ensure that no Inf or -Inf values remain
  expect_false(any(is.infinite(result$a)))
  expect_false(any(is.infinite(result$b)))
  expect_false(any(is.infinite(result$c)))
})

test_that("inf_to_na returns NAs on vectors", {
  # positive inf check
  test <- c(1,2, Inf, Inf, 5)
  result <- inf_to_na(test)
  expect_false(any(is.infinite(result)))
  
  test <- c(1,2, -Inf, -Inf, 5)
  result <- inf_to_na(test)
  expect_false(any(is.infinite(result)))
  
  test <- c(1, 2, NaN, NaN)
  result <- inf_to_na(test)
  expect_false(any(is.infinite(result)))
})


