test_that("addACLocation keeps per-location sharing groups separate", {
  inserted_share_with <- character()
  next_location_id <- 100L

  local_mocked_bindings(
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO public.locations", statement, fixed = TRUE)) {
        inserted_share_with <<- c(inserted_share_with, params[[7]])
        next_location_id <<- next_location_id + 1L
        return(data.frame(location_id = next_location_id))
      }
      if (grepl("public.datum_list", statement, fixed = TRUE)) {
        return(data.frame(datum_id = 10L))
      }
      if (grepl("public.location_types", statement, fixed = TRUE)) {
        return(data.frame(type_id = 1L))
      }
      if (grepl("SELECT location_id FROM public.locations", statement, fixed = TRUE)) {
        return(data.frame(location_id = NA_integer_))
      }
      stop("Unexpected query in addACLocation test: ", statement)
    },
    dbExecute = function(...) 1L,
    .package = "DBI"
  )
  local_mocked_bindings(
    dbTransBegin = function(...) TRUE,
    .package = "AquaCache"
  )

  suppressMessages(addACLocation(
    name = c("Location A", "Location B"),
    name_fr = c("Endroit A", "Endroit B"),
    alias = c("A", "B"),
    location_code = c("LOC-A", "LOC-B"),
    latitude = c(60, 61),
    longitude = c(-135, -136),
    share_with = c("group_a", "group_b"),
    location_type = c(1L, 1L),
    note = c(NA_character_, NA_character_),
    contact = c(NA_character_, NA_character_),
    datum_id_from = c(10L, 10L),
    datum_id_to = c(10L, 10L),
    conversion_m = c(0, 0),
    current = c(TRUE, TRUE),
    network = c(NA_integer_, NA_integer_),
    project = c(NA_integer_, NA_integer_),
    con = structure(list(), class = "mock_con")
  ))

  expect_equal(inserted_share_with, c("{group_a}", "{group_b}"))
})

test_that("addACLocation estimates omitted elevations and resolves their datum", {
  inserted_conversion <- NULL

  local_mocked_bindings(
    get_elevation = function(lat, lon, details = TRUE, ...) {
      expect_equal(lat, 60)
      expect_equal(lon, -135)
      list(
        elevation = 721.4,
        source = "CDEM",
        vertical_datum = "CGVD28"
      )
    },
    dbTransBegin = function(...) TRUE,
    .package = "AquaCache"
  )
  local_mocked_bindings(
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO public.locations", statement, fixed = TRUE)) {
        return(data.frame(location_id = 101L))
      }
      if (grepl("SELECT datum_id, datum_name_en", statement, fixed = TRUE)) {
        return(data.frame(
          datum_id = c(10L, 35L, 110L),
          datum_name_en = c(
            "ASSUMED DATUM",
            "CGVD28 (assumed)",
            "CGVD28 (approximate)"
          )
        ))
      }
      if (grepl("public.location_types", statement, fixed = TRUE)) {
        return(data.frame(type_id = 1L))
      }
      if (grepl("SELECT location_id FROM public.locations", statement, fixed = TRUE)) {
        return(data.frame(location_id = NA_integer_))
      }
      stop("Unexpected query in automatic elevation test: ", statement)
    },
    dbExecute = function(con, statement, params = NULL, ...) {
      if (grepl("INSERT INTO public.datum_conversions", statement, fixed = TRUE)) {
        inserted_conversion <<- params
      }
      1L
    },
    .package = "DBI"
  )

  result <- suppressMessages(addACLocation(
    name = "Estimated location",
    alias = NA_character_,
    location_code = "EST-001",
    latitude = 60,
    longitude = -135,
    share_with = "public_reader",
    location_type = 1L,
    note = NA_character_,
    contact = NA_character_,
    network = NA_integer_,
    project = NA_integer_,
    con = structure(list(), class = "mock_con")
  ))

  expect_equal(inserted_conversion, list(101L, 10L, 110L, 721.4, TRUE))
  expect_equal(result$elevation_m, 721.4)
  expect_identical(result$datum_id_from, 10L)
  expect_identical(result$datum_id_to, 110L)
  expect_identical(result$vertical_datum, "CGVD28")
  expect_true(result$elevation_estimated)
})

test_that("unknown elevation datums are created safely", {
  select_count <- 0L
  locked <- FALSE

  local_mocked_bindings(
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("SELECT datum_id, datum_name_en", statement, fixed = TRUE)) {
        select_count <<- select_count + 1L
        return(data.frame(
          datum_id = c(10L, 1000L),
          datum_name_en = c("ASSUMED DATUM", "NAVD88")
        ))
      }
      if (grepl("INSERT INTO public.datum_list", statement, fixed = TRUE)) {
        expect_identical(params, list("EGM2008"))
        return(data.frame(datum_id = 1001L))
      }
      stop("Unexpected query in datum creation test: ", statement)
    },
    dbExecute = function(con, statement, ...) {
      if (grepl("LOCK TABLE public.datum_list", statement, fixed = TRUE)) {
        locked <<- TRUE
      }
      1L
    },
    .package = "DBI"
  )

  datum_id <- AquaCache:::.match_or_create_location_datum(
    con = structure(list(), class = "mock_con"),
    datum_name = "EGM2008"
  )

  expect_identical(datum_id, 1001L)
  expect_true(locked)
  expect_identical(select_count, 2L)
})
