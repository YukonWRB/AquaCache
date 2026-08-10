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
    borehole_name = "Contractor's borehole",
    well_name = "Contractor's well 1",
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
  expect_equal(well_params[[2]], "Contractor's well 1")
  expect_equal(well_params[[11]], "Well's notes")
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
  expect_equal(well_params[13:18], list(2L, 203.2, 0, 6.1, 3L, 4L))
})

test_that("insertACBorehole inserts any number of wells for one borehole", {
  well_params <- list()

  local_mocked_bindings(
    dbExecute = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.wells", statement, fixed = TRUE)) {
        well_params[[length(well_params) + 1L]] <<- params
      }
      0L
    },
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        return(data.frame(borehole_id = 105L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    borehole_name = "Westbay BH-01",
    well_name = c("Port 1", "Port 2", "Port 3"),
    latitude = 60,
    longitude = -135,
    is_well = TRUE,
    casing_od = 50.8,
    top_of_screen = c(10, 20, 30),
    bottom_of_screen = c(12, 22, 32),
    notes_well = c("Shallow", "Middle", "Deep"),
    purpose_well_inferred = c(FALSE, TRUE, FALSE),
    share_with_well = list(
      "public_reader",
      c("public_reader", "internal_reader"),
      "internal_reader"
    )
  )

  expect_equal(result, 105L)
  expect_length(well_params, 3L)
  expect_equal(vapply(well_params, `[[`, character(1), 2L), c("Port 1", "Port 2", "Port 3"))
  expect_equal(vapply(well_params, `[[`, numeric(1), 3L), rep(50.8, 3L))
  expect_equal(vapply(well_params, `[[`, numeric(1), 4L), c(10, 20, 30))
  expect_equal(vapply(well_params, `[[`, character(1), 11L), c("Shallow", "Middle", "Deep"))
  expect_equal(well_params[[2]][[12]], "{public_reader,internal_reader}")
})

test_that("insertACBorehole rejects mismatched well-vector lengths", {
  local_mocked_bindings(
    dbExecute = function(...) 0L,
    .package = "DBI"
  )

  expect_error(
    insertACBorehole(
      con = structure(list(), class = "mock_con"),
      borehole_name = "Two wells",
      well_name = c("Well 1", "Well 2"),
      latitude = 60,
      longitude = -135,
      is_well = TRUE,
      casing_od = c(50, 60, 70)
    ),
    "length one or the number of wells"
  )
})

test_that("insertACBorehole accepts Unknown depth to bedrock", {
  borehole_params <- NULL

  local_mocked_bindings(
    dbExecute = function(...) 0L,
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        borehole_params <<- params
        return(data.frame(borehole_id = 106L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    well_name = "Unknown bedrock depth",
    latitude = 60,
    longitude = -135,
    bedrock_reached = NULL,
    depth_to_bedrock = "Unknown"
  )

  expect_equal(result, 106L)
  expect_true(is.na(borehole_params[[9]]))
  expect_true(is.na(borehole_params[[10]]))
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
      is_well = TRUE,
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
      is_well = TRUE,
      seal_depth_from = 10,
      seal_depth_to = 5
    ),
    "seal_depth_to.*greater than or equal"
  )
})

test_that("insertACBorehole passes a custom document name to insertACDocument", {
  document_args <- NULL
  document_link <- NULL

  local_mocked_bindings(
    insertACDocument = function(...) {
      document_args <<- list(...)
      list(new_document_id = 501L)
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbExecute = function(con, statement, params = NULL, ...) {
      if (grepl("boreholes_documents", statement, fixed = TRUE)) {
        document_link <<- params
      }
      0L
    },
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        return(data.frame(borehole_id = 107L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  result <- insertACBorehole(
    con = structure(list(), class = "mock_con"),
    path = "test-log.pdf",
    document_name = "Split log - borehole 1",
    borehole_name = "Borehole 1",
    latitude = 60,
    longitude = -135
  )

  expect_equal(result, 107L)
  expect_equal(document_args$name, "Split log - borehole 1")
  expect_equal(document_link, list(107L, 501L))
})

test_that("insertACBorehole keeps the historical default document name", {
  document_name <- NULL

  local_mocked_bindings(
    insertACDocument = function(...) {
      document_name <<- list(...)$name
      list(new_document_id = 502L)
    },
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbExecute = function(...) 0L,
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO boreholes.boreholes", statement, fixed = TRUE)) {
        return(data.frame(borehole_id = 108L))
      }
      stop("Unexpected query in insertACBorehole test: ", statement)
    },
    .package = "DBI"
  )

  insertACBorehole(
    con = structure(list(), class = "mock_con"),
    path = "test-log.pdf",
    borehole_name = "Borehole 2",
    latitude = 60,
    longitude = -135
  )

  expect_equal(document_name, "Document for borehole/well Borehole 2")
})
