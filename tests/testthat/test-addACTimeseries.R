test_that("addACTimeseries accepts mixed missing and valid sub-locations", {
  next_timeseries_id <- 200L

  local_mocked_bindings(
    dbGetQuery = function(con, statement, params = NULL, ...) {
      if (grepl("SELECT location_id FROM public.locations", statement, fixed = TRUE)) {
        return(data.frame(location_id = 1L))
      }
      if (grepl("SELECT location_code, name FROM public.locations", statement, fixed = TRUE)) {
        return(data.frame(location_code = "LOC-1", name = "Location 1"))
      }
      if (grepl("public.sub_locations", statement, fixed = TRUE)) {
        return(data.frame(sub_location_id = 7L))
      }
      if (grepl("public.parameters", statement, fixed = TRUE)) {
        return(data.frame(parameter_id = 1L))
      }
      if (grepl("public.media_types", statement, fixed = TRUE)) {
        return(data.frame(media_id = 1L))
      }
      if (grepl("public.organizations", statement, fixed = TRUE)) {
        return(data.frame(organization_id = 1L))
      }
      if (grepl("continuous.aggregation_types", statement, fixed = TRUE)) {
        return(data.frame(aggregation_type_id = 1L))
      }
      if (grepl("INSERT INTO continuous.timeseries", statement, fixed = TRUE)) {
        next_timeseries_id <<- next_timeseries_id + 1L
        return(data.frame(timeseries_id = next_timeseries_id))
      }
      stop("Unexpected query in addACTimeseries test: ", statement)
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    resolve_parameter_matrix_state = function(...) 1L,
    source_adapter_assignments_normalize = function(...) {
      data.frame(
        active = logical(),
        fetch_priority = integer(),
        source_fx = character()
      )
    },
    .package = "AquaCache"
  )

  expect_no_warning(suppressMessages(addACTimeseries(
    start_datetime = rep(as.POSIXct("2026-01-01", tz = "UTC"), 2),
    location = c(1, 1),
    sub_location = c(NA_integer_, 7L),
    z = c(NA_real_, NA_real_),
    parameter = c(1, 1),
    media = c(1, 1),
    matrix_state_id = c(1L, 1L),
    sensor_priority = c(1, 1),
    aggregation_type = c("instantaneous", "instantaneous"),
    record_rate = c("1 hour", "1 hour"),
    share_with = c("public_reader", "public_reader"),
    owner = c(1L, 1L),
    note = c(NA_character_, NA_character_),
    con = structure(list(), class = "mock_con")
  )))
})
