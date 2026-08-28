#' Validate and normalize component-built discrete results
#'
#' Validates the result aggregation data supplied to discrete-data ingestion
#' functions and converts it to the database-ready representation used by
#' AquaCache. The `result_row` columns provide a temporary link to rows in
#' `results`; they are replaced with database `result_id` values after the
#' parent results have been inserted.
#'
#' Aggregations and components must be supplied together. Each aggregation
#' must uniquely reference one row in `results` and identify its calculation
#' using exactly one of `aggregation_type` or
#' `result_aggregation_type_id`. Each component must reference an aggregated
#' result row and have a positive observation number that is unique within
#' that result. Excluded observations require a nonblank note explaining the
#' exclusion.
#'
#' Canonical values for aggregated rows are deliberately set to `NA` in
#' `results`. This allows the parent result to be inserted first; database
#' triggers calculate and maintain its value after the aggregation metadata
#' and components are inserted.
#'
#' @param results A data frame of rows destined for `discrete.results`.
#'   Its row positions are referenced by `result_row` in the other inputs.
#' @param result_aggregations An optional data frame with one row per
#'   component-built result. It must contain `result_row` and exactly one of
#'   `aggregation_type` or `result_aggregation_type_id`. Optional columns are
#'   `calculation_version`, `calculation_arguments`, and `note`.
#'   `calculation_arguments` may contain JSON object strings or named lists.
#' @param result_components An optional data frame of component observations.
#'   Required columns are `result_row`, `observation_number`, and `result`.
#'   Optional columns are `observation_datetime`, `result_condition`,
#'   `result_condition_value`, `included_in_aggregate`, `weight`, and `note`.
#'
#' @return A list containing normalized `results`, `result_aggregations`, and
#'   `result_components` data frames. When no aggregation inputs are supplied,
#'   the latter two elements are `NULL`.
#'
#' @keywords internal
#' @noRd
normalize_discrete_result_aggregations <- function(
  results,
  result_aggregations = NULL,
  result_components = NULL
) {
  has_aggregations <- !is.null(result_aggregations)
  has_components <- !is.null(result_components)
  if (xor(has_aggregations, has_components)) {
    stop(
      "result_aggregations and result_components must be supplied together."
    )
  }
  if (!has_aggregations) {
    return(list(
      results = results,
      result_aggregations = NULL,
      result_components = NULL
    ))
  }
  if (
    !inherits(result_aggregations, "data.frame") ||
      !inherits(result_components, "data.frame")
  ) {
    stop("Result aggregation inputs must be data frames.")
  }
  if (!nrow(result_aggregations) && !nrow(result_components)) {
    return(list(
      results = results,
      result_aggregations = result_aggregations,
      result_components = result_components
    ))
  }
  if (!nrow(result_aggregations) || !nrow(result_components)) {
    stop(
      "result_aggregations and result_components must both contain rows, or ",
      "both be empty."
    )
  }

  if (!"result_row" %in% names(result_aggregations)) {
    stop("result_aggregations must contain result_row.")
  }
  type_columns <- intersect(
    c("result_aggregation_type_id", "aggregation_type"),
    names(result_aggregations)
  )
  if (length(type_columns) != 1L) {
    stop(
      "result_aggregations must contain exactly one of ",
      "result_aggregation_type_id or aggregation_type."
    )
  }
  aggregation_columns <- c(
    "result_row",
    type_columns,
    "calculation_version",
    "calculation_arguments",
    "note"
  )
  result_aggregations <- result_aggregations[,
    intersect(aggregation_columns, names(result_aggregations)),
    drop = FALSE
  ]
  result_aggregations$result_row <- as.integer(
    result_aggregations$result_row
  )
  if (
    any(is.na(result_aggregations$result_row)) ||
      any(result_aggregations$result_row < 1L) ||
      any(result_aggregations$result_row > nrow(results)) ||
      anyDuplicated(result_aggregations$result_row)
  ) {
    stop(
      "result_aggregations$result_row must uniquely reference rows in results."
    )
  }
  if (identical(type_columns, "result_aggregation_type_id")) {
    result_aggregations$result_aggregation_type_id <- as.integer(
      result_aggregations$result_aggregation_type_id
    )
    if (any(is.na(result_aggregations$result_aggregation_type_id))) {
      stop("result_aggregation_type_id cannot contain missing values.")
    }
  } else {
    result_aggregations$aggregation_type <- trimws(
      as.character(result_aggregations$aggregation_type)
    )
    if (
      any(is.na(result_aggregations$aggregation_type)) ||
        any(!nzchar(result_aggregations$aggregation_type))
    ) {
      stop("aggregation_type cannot contain missing or blank values.")
    }
  }
  if (!"calculation_version" %in% names(result_aggregations)) {
    result_aggregations$calculation_version <- 1L
  }
  result_aggregations$calculation_version <- as.integer(
    result_aggregations$calculation_version
  )
  if (
    any(is.na(result_aggregations$calculation_version)) ||
      any(result_aggregations$calculation_version < 1L)
  ) {
    stop("calculation_version must contain positive integers.")
  }
  if (!"calculation_arguments" %in% names(result_aggregations)) {
    result_aggregations$calculation_arguments <- rep(
      "{}",
      nrow(result_aggregations)
    )
  } else {
    arguments <- result_aggregations$calculation_arguments
    if (is.list(arguments)) {
      arguments <- vapply(
        arguments,
        function(x) {
          if (is.null(x) || !length(x)) {
            return("{}")
          }
          as.character(jsonlite::toJSON(
            x,
            auto_unbox = TRUE,
            null = "null",
            na = "null"
          ))
        },
        character(1)
      )
    } else {
      arguments <- as.character(arguments)
      arguments[is.na(arguments) | !nzchar(trimws(arguments))] <- "{}"
    }
    valid_arguments <- vapply(
      arguments,
      function(x) {
        trimmed <- trimws(x)
        jsonlite::validate(trimmed) &&
          startsWith(trimmed, "{") &&
          endsWith(trimmed, "}")
      },
      logical(1)
    )
    if (!all(valid_arguments)) {
      stop("calculation_arguments must contain JSON objects or named lists.")
    }
    result_aggregations$calculation_arguments <- arguments
  }

  required_component_columns <- c("result_row", "observation_number", "result")
  missing_component_columns <- setdiff(
    required_component_columns,
    names(result_components)
  )
  if (length(missing_component_columns)) {
    stop(
      "result_components is missing required columns: ",
      paste(missing_component_columns, collapse = ", "),
      "."
    )
  }
  component_columns <- c(
    required_component_columns,
    "observation_datetime",
    "result_condition",
    "result_condition_value",
    "included_in_aggregate",
    "weight",
    "note"
  )
  result_components <- result_components[,
    intersect(component_columns, names(result_components)),
    drop = FALSE
  ]
  result_components$result_row <- as.integer(result_components$result_row)
  result_components$observation_number <- as.integer(
    result_components$observation_number
  )
  result_components$result <- as.numeric(result_components$result)
  if (
    !nrow(result_components) ||
      any(is.na(result_components$result_row)) ||
      any(!result_components$result_row %in% result_aggregations$result_row)
  ) {
    stop(
      "Every result component must reference a configured result_aggregations$result_row."
    )
  }
  if (
    any(is.na(result_components$observation_number)) ||
      any(result_components$observation_number < 1L) ||
      anyDuplicated(result_components[c("result_row", "observation_number")])
  ) {
    stop(
      "observation_number must be positive and unique within each result row."
    )
  }
  if (
    any(
      !is.na(result_components$result) &
        !is.finite(result_components$result)
    )
  ) {
    stop("result_components$result must contain finite values or NA.")
  }
  if (!"result_condition" %in% names(result_components)) {
    result_components$result_condition <- NA_integer_
  }
  result_components$result_condition <- as.integer(
    result_components$result_condition
  )
  if (!"result_condition_value" %in% names(result_components)) {
    result_components$result_condition_value <- NA_real_
  }
  result_components$result_condition_value <- as.numeric(
    result_components$result_condition_value
  )
  if (
    any(
      !is.na(result_components$result) &
        !is.na(result_components$result_condition)
    )
  ) {
    stop("A result component cannot have both result and result_condition.")
  }
  invalid_condition_value <- !is.na(
    result_components$result_condition_value
  ) &
    !result_components$result_condition %in% c(1L, 2L)
  invalid_condition_value[is.na(invalid_condition_value)] <- TRUE
  if (any(invalid_condition_value)) {
    stop("result_condition_value is only valid for conditions 1 and 2.")
  }
  if (!"included_in_aggregate" %in% names(result_components)) {
    result_components$included_in_aggregate <- TRUE
  }
  result_components$included_in_aggregate <- as.logical(
    result_components$included_in_aggregate
  )
  if (any(is.na(result_components$included_in_aggregate))) {
    stop("included_in_aggregate cannot contain missing values.")
  }
  if (!"weight" %in% names(result_components)) {
    result_components$weight <- NA_real_
  }
  result_components$weight <- as.numeric(result_components$weight)
  if (
    any(
      !is.na(result_components$weight) &
        (!is.finite(result_components$weight) | result_components$weight <= 0)
    )
  ) {
    stop("result component weights must be finite and greater than zero.")
  }
  if (!"note" %in% names(result_components)) {
    result_components$note <- NA_character_
  }
  exclusion_has_reason <- !is.na(result_components$note) &
    nzchar(trimws(as.character(result_components$note)))
  if (any(!result_components$included_in_aggregate & !exclusion_has_reason)) {
    stop("Every excluded result component needs a nonblank note.")
  }
  missing_component_rows <- setdiff(
    result_aggregations$result_row,
    unique(result_components$result_row)
  )
  if (length(missing_component_rows)) {
    stop(
      "Every result aggregation needs at least one component; missing result_row: ",
      paste(missing_component_rows, collapse = ", "),
      "."
    )
  }

  if (!"result_condition" %in% names(results)) {
    results$result_condition <- NA_integer_
  }
  if (!"result_condition_value" %in% names(results)) {
    results$result_condition_value <- NA_real_
  }
  aggregate_rows <- result_aggregations$result_row
  results$result[aggregate_rows] <- NA_real_
  results$result_condition[aggregate_rows] <- NA_integer_
  results$result_condition_value[aggregate_rows] <- NA_real_

  list(
    results = results,
    result_aggregations = result_aggregations,
    result_components = result_components
  )
}


#' Insert aggregation metadata and component observations
#'
#' Converts temporary `result_row` references to the database-generated
#' `result_id` values for their parent results, resolves textual aggregation
#' types against active rows in `discrete.result_aggregation_types`, and
#' appends the aggregation and component rows to their discrete-schema tables.
#'
#' The parent rows in `discrete.results` must already exist. This helper does
#' not open a transaction; callers are responsible for inserting parent
#' results, aggregation metadata, and components in one transaction so that a
#' partially constructed composite result cannot be committed.
#'
#' @param con An open DBI connection to an AquaCache database.
#' @param result_ids An integer vector of database result IDs in the same order
#'   as the `results` rows passed to
#'   `normalize_discrete_result_aggregations()` (internal function).
#' @param result_aggregations A normalized aggregation data frame returned by
#'   `normalize_discrete_result_aggregations()` (internal function).
#' @param result_components A normalized component data frame returned by
#'  `normalize_discrete_result_aggregations()` (internal function).
#'
#' @return `NULL`, invisibly. The function is called for its database effects.
#'
#' @keywords internal
#' @noRd
append_discrete_result_aggregations <- function(
  con,
  result_ids,
  result_aggregations,
  result_components
) {
  if (is.null(result_aggregations) || !nrow(result_aggregations)) {
    return(invisible(NULL))
  }
  aggregations <- result_aggregations
  if ("aggregation_type" %in% names(aggregations)) {
    type_lookup <- DBI::dbGetQuery(
      con,
      "SELECT result_aggregation_type_id, aggregation_type
       FROM discrete.result_aggregation_types
       WHERE active"
    )
    aggregations$result_aggregation_type_id <-
      type_lookup$result_aggregation_type_id[
        match(aggregations$aggregation_type, type_lookup$aggregation_type)
      ]
    if (any(is.na(aggregations$result_aggregation_type_id))) {
      stop(
        "Unknown or inactive aggregation_type: ",
        paste(
          unique(aggregations$aggregation_type[
            is.na(aggregations$result_aggregation_type_id)
          ]),
          collapse = ", "
        ),
        "."
      )
    }
    aggregations$aggregation_type <- NULL
  }
  aggregations$result_id <- result_ids[aggregations$result_row]
  aggregations$result_row <- NULL
  dbAppendTableRLS(con, "discrete.result_aggregations", aggregations)

  components <- result_components
  components$result_id <- result_ids[components$result_row]
  components$result_row <- NULL
  dbAppendTableRLS(con, "discrete.result_components", components)
  invisible(NULL)
}
