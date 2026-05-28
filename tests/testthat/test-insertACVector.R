test_that("insertACVector_attribute_json matches row-wise jsonlite output", {
  tbl <- data.frame(
    feature = c("one", "two"),
    description_text = c("first", "second"),
    character_value = c("a,b", "quote\"slash\\newline\n"),
    numeric_value = c(1.23456789012345, Inf),
    integer_value = c(1L, NA_integer_),
    logical_value = c(TRUE, NA),
    date_value = as.Date(c("2024-01-01", NA)),
    datetime_value = as.POSIXct(
      c("2024-01-01 12:00:00", NA),
      tz = "America/Whitehorse"
    ),
    factor_value = factor(c("alpha", NA)),
    layer_name = c("drop", "drop"),
    stringsAsFactors = FALSE
  )

  expected <- vapply(
    seq_len(nrow(tbl)),
    function(i) {
      attribute_data <- tbl[
        i,
        setdiff(
          names(tbl),
          c(
            "layer_name",
            "feature_name",
            "description",
            "feature",
            "description_text"
          )
        ),
        drop = FALSE
      ]
      attribute_list <- as.list(attribute_data[1, , drop = FALSE])
      attribute_list <- lapply(attribute_list, function(value) {
        val <- value[[1]]
        if (inherits(val, "POSIXct")) {
          val <- format(val, tz = "UTC", usetz = TRUE)
        } else if (inherits(val, "Date")) {
          val <- format(val, "%Y-%m-%d")
        } else if (is.factor(val)) {
          val <- as.character(val)
        }
        val
      })
      as.character(jsonlite::toJSON(
        attribute_list,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        POSIXt = "ISO8601",
        digits = NA
      ))
    },
    character(1)
  )

  result <- AquaCache:::insertACVector_attribute_json(
    tbl,
    feature_name_col = "feature",
    description_col = "description_text"
  )

  expect_equal(result, expected)
})

test_that("insertACVector_attribute_json returns NA when there are no attributes", {
  tbl <- data.frame(
    feature_name = c("one", "two"),
    description = c("first", "second")
  )

  result <- AquaCache:::insertACVector_attribute_json(tbl)

  expect_equal(result, c(NA_character_, NA_character_))
})

test_that("insertACVector_resolve_actions handles existing features without row scans", {
  existing <- data.frame(
    feature_name = c("two", "four"),
    geom_id = c(22L, 44L),
    geom_type = c("POINT", "POINT")
  )
  feature_names <- c("one", "two", "three", "four")

  skipped <- AquaCache:::insertACVector_resolve_actions(
    existing = existing,
    feature_names = feature_names,
    layer_name = "test layer",
    overwrite = FALSE,
    ask = FALSE
  )
  expect_equal(skipped$actions, c("insert", "skip", "insert", "skip"))
  expect_equal(skipped$update_geom_ids, rep(NA_integer_, 4))

  updated <- suppressMessages(AquaCache:::insertACVector_resolve_actions(
    existing = existing,
    feature_names = feature_names,
    layer_name = "test layer",
    overwrite = TRUE,
    ask = FALSE
  ))
  expect_equal(updated$actions, c("insert", "update", "insert", "update"))
  expect_equal(updated$update_geom_ids, c(NA_integer_, 22L, NA_integer_, 44L))
})

test_that("insertACVector_resolve_actions refuses ambiguous updates", {
  existing <- data.frame(
    feature_name = c("two", "two"),
    geom_id = c(22L, 23L),
    geom_type = c("POINT", "POINT")
  )

  expect_error(
    AquaCache:::insertACVector_resolve_actions(
      existing = existing,
      feature_names = c("one", "two"),
      layer_name = "test layer",
      overwrite = TRUE,
      ask = FALSE
    ),
    "Multiple existing database entries"
  )
})
