test_that("mapping review rejects invalid filters before querying", {
  expect_error(getImportMappingReview(NULL, source_code = " "), "source_code")
  expect_error(getImportMappingReview(NULL, source_code = c("EQWin", "ALS")), "source_code")
})
