test_that("source-managed attribute segments are clipped around protection", {
  boundaries <- as.POSIXct(
    c("2020-01-01", "2020-04-01", "2020-07-01", "2021-01-01"),
    tz = "UTC"
  )
  existing <- data.frame(
    grade_id = 1:3,
    timeseries_id = 29L,
    grade_type_id = c(1L, 9L, 1L),
    start_dt = boundaries[c(1, 2, 3)],
    end_dt = boundaries[c(2, 3, 4)],
    no_source_update = c(FALSE, TRUE, FALSE)
  )
  imported <- data.frame(
    datetime = boundaries[c(1, 4)],
    grade = c(2L, 2L),
    no_source_update = FALSE
  )

  proposed <- build_attribute_segments(
    imported,
    value_col = "grade",
    id_col = "grade_id",
    timeseries_id = 29L
  )
  names(proposed)[names(proposed) == "grade"] <- "grade_type_id"
  proposed <- clip_segments_around_protected(
    proposed,
    existing[existing$no_source_update, , drop = FALSE]
  )
  result <- collapse_segments_with_split(
    existing,
    proposed,
    value_col = "grade_type_id",
    id_col = "grade_id",
    timeseries_id = 29L,
    protection_col = "no_source_update"
  )

  expect_equal(result$grade_type_id, c(2L, 9L, 2L))
  expect_equal(result$no_source_update, c(FALSE, TRUE, FALSE))
  expect_equal(result$grade_id, 1:3)
  expect_equal(result$start_dt, boundaries[c(1, 2, 3)])
  expect_equal(result$end_dt, boundaries[c(2, 3, 4)])
})

test_that("qualifier merging retains source-protection boundaries", {
  segments <- data.frame(
    qualifier_id = c(10L, 11L),
    timeseries_id = 29L,
    qualifier_type_id = 18L,
    start_dt = as.POSIXct(c("2024-01-01", "2024-02-01"), tz = "UTC"),
    end_dt = as.POSIXct(c("2024-02-01", "2024-03-01"), tz = "UTC"),
    no_source_update = c(TRUE, FALSE)
  )

  result <- merge_overlapping_same_value_segments(
    segments,
    value_col = "qualifier_type_id",
    id_col = "qualifier_id",
    protection_col = "no_source_update"
  )

  expect_equal(nrow(result$segments), 2)
  expect_equal(result$segments$no_source_update, c(FALSE, TRUE))
  expect_length(result$delete_ids, 0)
})

test_that("source grade and approval updates never modify protected rows", {
  boundaries <- as.POSIXct(
    c("2020-01-01", "2020-04-01", "2020-07-01", "2021-01-01"),
    tz = "UTC"
  )

  for (attribute in c("grade", "approval")) {
    id_col <- paste0(attribute, "_id")
    type_col <- paste0(attribute, "_type_id")
    code_col <- paste0(attribute, "_type_code")
    table_name <- paste0("continuous.", attribute, "s")
    executed <- list()

    existing <- data.frame(
      id = 1:3,
      timeseries_id = 29L,
      type = c(1L, 9L, 1L),
      start_dt = boundaries[c(1, 2, 3)],
      end_dt = boundaries[c(2, 3, 4)],
      no_source_update = c(FALSE, TRUE, FALSE)
    )
    names(existing)[names(existing) == "id"] <- id_col
    names(existing)[names(existing) == "type"] <- type_col

    local_mocked_bindings(
      dbTransBegin = function(...) FALSE,
      .package = "AquaCache"
    )
    local_mocked_bindings(
      dbGetQuery = function(con, statement, ...) {
        if (grepl(paste0("public.", attribute, "_types"), statement)) {
          catalogue <- data.frame(id = c(1L, 9L, 2L), code = c("UNS", "UNK", "SRC"))
          names(catalogue) <- c(type_col, code_col)
          return(catalogue)
        }
        if (grepl(table_name, statement, fixed = TRUE)) {
          return(existing)
        }
        stop("Unexpected query: ", statement)
      },
      dbExecute = function(con, statement, params = NULL, ...) {
        executed[[length(executed) + 1L]] <<- list(
          statement = statement,
          params = params
        )
        1L
      },
      .package = "DBI"
    )

    incoming <- data.frame(
      datetime = boundaries[c(1, 4)],
      value = c(2L, 2L)
    )
    names(incoming)[2] <- attribute
    adjust_function <- get(paste0("adjust_", attribute), mode = "function")
    expect_warning(
      adjust_function(
        structure(list(), class = "mock_con"),
        29L,
        incoming,
        source_update = TRUE
      ),
      NA
    )

    updated_ids <- vapply(
      executed,
      function(call) {
        if (!grepl("^UPDATE", call$statement)) {
          return(NA_integer_)
        }
        as.integer(utils::tail(call$params, 1))
      },
      integer(1)
    )
    expect_setequal(updated_ids[!is.na(updated_ids)], c(1L, 3L))
    expect_false(2L %in% updated_ids)
  }
})
