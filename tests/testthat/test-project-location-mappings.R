test_that("project resolution rejects conflicts and accepts agreeing targets", {
  mappings <- data.frame(
    source_location_code = rep(" SW01 ", 4),
    project_id = c(NA, 1L, 2L, 1L),
    location_id = c(10L, 20L, 30L, 21L),
    sub_location_id = NA_integer_,
    profile_specific = c(TRUE, FALSE, TRUE, TRUE)
  )
  resolve <- function(project = NULL, rows = mappings) {
    resolveImportLocationMapping(
      NULL,
      "ALS",
      "sw01",
      project_id = project,
      mappings = rows
    )
  }
  expect_identical(resolve(1L)$location_id, 21L)
  expect_identical(resolve(2L)$location_id, 30L)
  expect_identical(resolve(3L)$location_id, 10L)
  ambiguous <- resolve()
  expect_equal(nrow(ambiguous), 0L)
  expect_setequal(attr(ambiguous, "candidates")$location_id, c(10L, 21L, 30L))
  agreeing <- mappings
  agreeing$location_id <- 10L
  expect_identical(resolve(rows = agreeing)$location_id, 10L)
  agreeing$sub_location_id[[3]] <- 2L
  expect_equal(nrow(resolve(rows = agreeing)), 0L)
  expect_equal(nrow(resolve(3L, mappings[!is.na(mappings$project_id), ])), 0L)
})

test_that("project mappings round-trip against an opted-in dev database", {
  skip_if(
    Sys.getenv("AQUACACHE_TEST_PATCH72") != "true",
    "Patch 72 database opt-in required"
  )
  database <- Sys.getenv("AQUACACHE_PROJECT_MAPPING_TEST_DB", "testdb")
  stopifnot(database %in% c("aquacache", "testdb"))
  con <- AquaConnect(
    name = database,
    host = "10.250.12.154",
    check = FALSE,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con))
  projects <- DBI::dbGetQuery(
    con,
    "SELECT project_id FROM public.projects ORDER BY project_id LIMIT 2"
  )$project_id
  locations <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM public.locations ORDER BY location_id LIMIT 2"
  )$location_id
  skip_if(
    length(projects) != 2L || length(locations) != 2L,
    "Two projects and locations required"
  )
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  code <- paste0("CODEX_PROJECT_TEST_", Sys.getpid())
  rows <- data.frame(
    source_location_code = "SW01",
    project_id = c(NA_integer_, projects),
    location_id = c(locations[[1]], locations)
  )
  upsertImportLocationMappings(con, code, rows)
  expect_equal(nrow(getImportLocationMappings(con, code)), 3L)
  expect_equal(
    nrow(getImportLocationMappings(con, code, project_id = projects[[1]])),
    2L
  )
  expect_equal(nrow(resolveImportLocationMapping(con, code, "sw01")), 0L)
  expect_identical(
    resolveImportLocationMapping(
      con,
      code,
      "sw01",
      project_id = projects[[2]]
    )$location_id,
    locations[[2]]
  )
  rows$location_id <- locations[[1]]
  upsertImportLocationMappings(con, code, rows)
  expect_equal(nrow(getImportLocationMappings(con, code)), 3L)
  expect_identical(
    resolveImportLocationMapping(con, code, "sw01")$location_id,
    locations[[1]]
  )
  snapshot <- getImportConfigurationSnapshot(
    con,
    code,
    options = list(location_project_id = projects[[1]])
  )
  expect_length(snapshot$mappings$locations, 3L)
  expect_identical(snapshot$upload_options$location_project_id, projects[[1]])
  review <- getImportMappingReview(con, code)
  expect_equal(nrow(review), 3L)
  expect_true(all(grepl("project_id", review$source_match, fixed = TRUE)))
})
