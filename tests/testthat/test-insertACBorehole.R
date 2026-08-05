test_that("insertACBorehole resolves drill-method names to catalogue IDs", {
  lookup_params <- NULL
  insert_statement <- NULL
  insert_params <- NULL

  local_mocked_bindings(
    dbExecute = function(...) 0L,
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("FROM boreholes.drill_methods", statement, fixed = TRUE)) {
        lookup_params <<- params
        return(data.frame(drill_method_id = 6L))
      }
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        insert_statement <<- statement
        insert_params <<- params
        return(data.frame(borehole_id = 101L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    well_name = "Test borehole",
    latitude = 60,
    longitude = -135,
    drill_method = "  Rotary - mud  "
  )

  expect_equal(result, 101L)
  expect_equal(lookup_params, list("Rotary - mud"))
  expect_match(insert_statement, "$12", fixed = TRUE)
  expect_equal(insert_params[[12]], 6L)
})

test_that("insertACBorehole validates drill-method integer IDs", {
  lookup_params <- NULL

  local_mocked_bindings(
    dbExecute = function(...) 0L,
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("FROM boreholes.drill_methods", statement, fixed = TRUE)) {
        lookup_params <<- params
        return(data.frame(drill_method_id = 4L))
      }
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        return(data.frame(borehole_id = 102L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    well_name = "Test borehole",
    latitude = 60,
    longitude = -135,
    drill_method = 4L
  )

  expect_equal(result, 102L)
  expect_equal(lookup_params, list(4L))
})

test_that("insertACBorehole binds text containing apostrophes", {
  borehole_statement <- NULL
  borehole_params <- NULL
  well_statement <- NULL
  well_params <- NULL

  local_mocked_bindings(
    dbExecute = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.wells", statement, fixed = TRUE)) {
        well_statement <<- statement
        well_params <<- params
      }
      0L
    },
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        borehole_statement <<- statement
        borehole_params <<- params
        return(data.frame(borehole_id = 103L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    well_name = "Contractor's borehole",
    latitude = 60,
    longitude = -135,
    location_source = "Owner's GPS",
    is_well = TRUE,
    notes_borehole = "10in seal from 0 to 15'",
    notes_well = "Well's notes"
  )

  expect_equal(result, 103L)
  expect_false(grepl("Contractor's", borehole_statement, fixed = TRUE))
  expect_false(grepl("15'", borehole_statement, fixed = TRUE))
  expect_equal(borehole_params[[5]], "Contractor's borehole")
  expect_equal(borehole_params[[6]], "Owner's GPS")
  expect_equal(borehole_params[[14]], "10in seal from 0 to 15'")
  expect_true(all(lengths(borehole_params) == 1L))
  expect_false(grepl("Well's notes", well_statement, fixed = TRUE))
  expect_equal(well_params[[10]], "Well's notes")
  expect_true(all(lengths(well_params) == 1L))
})

test_that("insertACBorehole rejects unknown drill methods", {
  local_mocked_bindings(
    dbExecute = function(...) 0L,
    dbGetQuery = function(...) data.frame(drill_method_id = integer()),
    .package = "DBI"
  )

  expect_error(
    insertACBorehole(
      con = structure(list(), class = "mock_con"),
      well_name = "Test borehole",
      latitude = 60,
      longitude = -135,
      drill_method = "Not a method"
    ),
    "does not match exactly one entry"
  )
})

test_that("insertACBorehole resolves and binds seal and screen details", {
  well_statement <- NULL
  well_params <- NULL

  local_mocked_bindings(
    dbExecute = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.wells", statement, fixed = TRUE)) {
        well_statement <<- statement
        well_params <<- params
      }
      0L
    },
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("FROM boreholes.seal_materials", statement, fixed = TRUE)) {
        return(data.frame(seal_material_id = 2L))
      }
      if (grepl("FROM boreholes.screen_materials", statement, fixed = TRUE)) {
        return(data.frame(screen_material_id = 3L))
      }
      if (grepl("FROM boreholes.screen_types", statement, fixed = TRUE)) {
        return(data.frame(screen_type_id = 4L))
      }
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        return(data.frame(borehole_id = 104L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    well_name = "Construction details",
    latitude = 60,
    longitude = -135,
    is_well = TRUE,
    seal_material = "Bentonite chips or pellets",
    seal_diameter_mm = 203.2,
    seal_depth_from = 0,
    seal_depth_to = 6.1,
    screen_material = "Stainless steel",
    screen_type = "Continuous wire-wrap"
  )

  expect_equal(result, 104L)
  expect_match(well_statement, "seal_material_id", fixed = TRUE)
  expect_match(well_statement, "screen_type_id", fixed = TRUE)
  expect_equal(well_params[12:17], list(2L, 203.2, 0, 6.1, 3L, 4L))
})

test_that("insertACBorehole rejects reversed construction intervals", {
  local_mocked_bindings(
    dbExecute = function(...) 0L,
    .package = "DBI"
  )

  expect_error(
    insertACBorehole(
      con = structure(list(), class = "mock_con"),
      well_name = "Reversed screen",
      latitude = 60,
      longitude = -135,
      top_of_screen = 20,
      bottom_of_screen = 10
    ),
    "bottom_of_screen.*greater than or equal"
  )
  expect_error(
    insertACBorehole(
      con = structure(list(), class = "mock_con"),
      well_name = "Reversed seal",
      latitude = 60,
      longitude = -135,
      seal_depth_from = 10,
      seal_depth_to = 5
    ),
    "seal_depth_to.*greater than or equal"
  )
})

test_that("Patch 56 migrates borehole drill methods as governed reference data", {
  patch <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "patches", "patch_56.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    patch,
    "CREATE TABLE boreholes.drill_methods",
    fixed = TRUE
  )
  expect_match(patch, "ON CONFLICT (method_name) DO UPDATE", fixed = TRUE)
  expect_match(
    patch,
    "SET drill_method = dm.drill_method_id::text",
    fixed = TRUE
  )
  expect_match(patch, "boreholes_drill_method_fkey", fixed = TRUE)
  expect_match(patch, "ON DELETE SET NULL", fixed = TRUE)
  expect_match(patch, "boreholes_drill_method_idx", fixed = TRUE)
  expect_match(patch, "audit_drill_methods_trigger", fixed = TRUE)
  expect_match(
    patch,
    "GRANT SELECT ON TABLE boreholes.drill_methods TO PUBLIC",
    fixed = TRUE
  )
  expect_match(
    patch,
    "ON SEQUENCE boreholes.drill_methods_drill_method_id_seq",
    fixed = TRUE
  )
  expect_false(
    grepl(
      "SELECT drill_method, drill_method_id",
      patch,
      fixed = TRUE
    )
  )
})

test_that("Patch 56 adds governed well seal and screen construction data", {
  patch <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "patches", "patch_56.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  for (table_name in c(
    "boreholes.seal_materials",
    "boreholes.screen_materials",
    "boreholes.screen_types"
  )) {
    expect_match(patch, paste("CREATE TABLE", table_name), fixed = TRUE)
  }
  for (column_name in c(
    "seal_material_id",
    "seal_diameter_mm",
    "seal_depth_from_m",
    "seal_depth_to_m",
    "screen_material_id",
    "screen_type_id"
  )) {
    expect_match(patch, paste("ADD COLUMN", column_name), fixed = TRUE)
  }
  expect_match(patch, "wells_seal_interval_valid", fixed = TRUE)
  expect_match(
    patch,
    "SET screen_top_depth_m = screen_bottom_depth_m",
    fixed = TRUE
  )
  expect_match(patch, "wells_screen_interval_valid", fixed = TRUE)
  expect_match(patch, "Continuous wire-wrap", fixed = TRUE)
  expect_match(patch, "Open hole", fixed = TRUE)
  expect_match(patch, "Drill cuttings", fixed = TRUE)
  expect_match(patch, "CREATE TRIGGER audit_%s_trigger", fixed = TRUE)
  expect_match(
    patch,
    "boreholes.screen_types_screen_type_id_seq",
    fixed = TRUE
  )
})
