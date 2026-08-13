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
