test_that("patch 48 defines stable public-safe location representations", {
  patch_path <- system.file("patches", "patch_48.R", package = "AquaCache")
  if (patch_path == "") {
    patch_path <- testthat::test_path("..", "..", "inst", "patches", "patch_48.R")
  }

  expect_true(file.exists(patch_path))
  expect_silent(parse(patch_path))

  patch_text <- paste(readLines(patch_path, warn = FALSE), collapse = "\n")
  expected_terms <- c(
    "public.locations\n       ADD COLUMN IF NOT EXISTS exact_share_with text[]",
    "CREATE OR REPLACE FUNCTION public.location_exact_visible",
    "CREATE OR REPLACE FUNCTION public.location_masked_point",
    "CREATE TABLE IF NOT EXISTS public.location_public_geometries",
    "CREATE TABLE IF NOT EXISTS public.location_reporting_areas",
    "CREATE TABLE IF NOT EXISTS public.location_reporting_area_members",
    "ALTER TABLE public.location_reporting_area_members ENABLE ROW LEVEL SECURITY",
    "CREATE OR REPLACE VIEW public.locations_public",
    "CREATE OR REPLACE VIEW discrete.samples_public",
    "CREATE OR REPLACE VIEW discrete.results_public",
    "public_location_id",
    "reporting_area_id",
    "WITH (security_barrier = true)",
    "coordinate_restricted_roles",
    "GRANT SELECT (%s) ON TABLE public.locations",
    "public_reader can still query public.locations.latitude directly",
    "public_geom_type IN ('exact_point', 'masked_point', 'reporting_polygon')",
    "patch_48_dry_run",
    "ROLLBACK;"
  )

  for (term in expected_terms) {
    expect_true(
      grepl(term, patch_text, fixed = TRUE),
      info = paste("Missing patch term:", term)
    )
  }

  expect_false(
    grepl("\\brandom\\s*\\(", patch_text),
    info = "Public location masking must be stored/stable, not per-query random."
  )
})

test_that("public location validation SQL documents privacy checks and grant audit", {
  validation_path <- system.file(
    "validation",
    "public_location_representations.sql",
    package = "AquaCache"
  )
  if (validation_path == "") {
    validation_path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "validation",
      "public_location_representations.sql"
    )
  }

  expect_true(file.exists(validation_path))

  validation_text <- paste(readLines(validation_path, warn = FALSE), collapse = "\n")
  expected_terms <- c(
    "locations without active public geometry",
    "masked points outside configured offset range",
    "public-safe locations exposing exact geometry when exact is hidden",
    "public-safe locations without public_location_id",
    "public-safe samples without public_location_id",
    "public_reader can select raw location coordinates",
    "public-safe samples without public-safe location row",
    "public-safe results without public-safe sample row",
    "Informational grant audit",
    "raw_locations_select",
    "raw_location_latitude_select",
    "raw_location_longitude_select",
    "reporting_area_members_select",
    "samples_metadata_select",
    "results_metadata_select"
  )

  for (term in expected_terms) {
    expect_true(
      grepl(term, validation_text, fixed = TRUE),
      info = paste("Missing validation term:", term)
    )
  }
})

test_that("public location representation deployment note documents rollout boundary", {
  doc_path <- system.file(
    "validation",
    "public_location_representations.md",
    package = "AquaCache"
  )
  if (doc_path == "") {
    doc_path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "validation",
      "public_location_representations.md"
    )
  }

  expect_true(file.exists(doc_path))

  doc_text <- paste(readLines(doc_path, warn = FALSE), collapse = "\n")
  expected_terms <- c(
    "patch_48_dry_run <- TRUE",
    "exact_share_with",
    "masked_point",
    "reporting_polygon",
    "public_location_id",
    "public.locations_public",
    "discrete.samples_public",
    "discrete.results_public",
    "column privileges",
    "raw-table grants"
  )

  for (term in expected_terms) {
    expect_true(
      grepl(term, doc_text, fixed = TRUE),
      info = paste("Missing documentation term:", term)
    )
  }
})
