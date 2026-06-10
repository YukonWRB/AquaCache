test_that("addACLocation supports patch 48 public location fields", {
  path <- system.file("R", "addACLocation.R", package = "AquaCache")
  if (path == "") {
    path <- testthat::test_path("..", "..", "R", "addACLocation.R")
  }

  expect_true(file.exists(path))
  expect_silent(parse(path))

  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expected_terms <- c(
    "exact_share_with = NA",
    "public_geom_type = \"exact_point\"",
    "role_array_literal",
    "has_exact_share_with",
    "public.location_public_geometries",
    "public.location_masked_point",
    "INSERT INTO public.location_public_geometries",
    "ST_SetSRID",
    "ST_MakePoint",
    "stable_toroid"
  )

  for (term in expected_terms) {
    expect_true(
      grepl(term, txt, fixed = TRUE),
      info = paste("Missing addACLocation public geometry term:", term)
    )
  }
})
