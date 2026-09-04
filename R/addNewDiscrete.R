#' Add new discrete sample data to the database
#'
#' @description
#' Appends a discrete sample, its associations, canonical results, and optional
#' component measurements to AquaCache inside one transaction, returning the
#' created `sample_id`. The formats are defined in the `details` section.
#'
#' @details
#' The 'sample' data.frame must contain the following columns:
#' - 'location_id': a numeric location ID, or `NA` only when the selected sample
#'   type does not require a location. Every locationless sample must have at
#'   least one `sample_groups` entry.
#' - 'media_id': a numeric specifying the media_id of the data point from table 'medias'.
#' - 'datetime': a POSIXct datetime object in UTC 0 time zone, specifying the datetime of the data point.
#' - 'collection_method': a numeric specifying the collection_method_id of the data point from table 'collection_methods', such as 1 (observation), 27 (water bottle), or 14 (pump).
#' - 'sample_type': a numeric specifying the sample_type_id of the data point from table 'sample_types', such as 1 (grab), 2 (composite), or 3 (integrated).
#' - 'owner': the numeric organization ID that owns the sample.
#' Optional columns are:
#' - 'import_source_id': a source-specific identifier used to match automated
#'   imports across runs. It must be supplied with `import_source`; manual
#'   samples may omit both fields.
#' - 'target_datetime': a POSIXct datetime object in UTC 0 time zone, specifying an artificial datetime for the data point which can be used for data analysis or plotting purposes.
#' - 'note': a character string with a note about the data point(s).
#' - 'no_source_update': logical; `TRUE` preserves the sample from later
#'   source-adapter synchronization while still allowing direct user edits.
#' - 'contributor': the numeric organization ID that contributed the sample.
#' - 'approval': the approval status of the data, as a character string. This should match entries in the 'approvals' table and an error will be thrown if it does not.
#' - 'grade': the grade of the data, as a character string. This should match entries in the 'grades' table and an error will be thrown if it does not.
#'
#'
#' The 'results' data.frame should contain one row per result and must contain the following columns:
#' - 'parameter_id': a numeric specifying the parameter_id of the data point from table 'parameters'.
#' - 'result': a numeric specifying the sample's results, matched to the parameters
#' - 'result_type': a numeric specifying the result_type_id of the data point from table 'result_types', such as 1 (concentration), 2 (load), or 3 (other).
#' Additionally, the following columns may need to be included:
#' - 'result_condition': a numeric specifying the result condition of the data point from table 'result_conditions', such as "< DL" or "> DL". Only necessary if there are NA values in the 'result' column that should be interpreted as a specific condition. If not provided, rows with NA values will be dropped.
#' - 'result_condition_value': a numeric specifying the value of the result condition, such as 0.1 for "< DL 0.1". Necessary if column 'result_condition' is provided AND contains values of 1 or 2, i.e. 'Below Detection/Quantification Limit' or 'Above Detection/Quantification Limit'.
#' - 'matrix_state_id' or 'matrix_state': an optional numeric id or text code/name specifying the physical matrix state of the analyzed result from table 'matrix_states'. If omitted, the database defaults it from the parent sample media.
#' - 'sample_fraction_id': a numeric specifying the sample_fraction_id of the data point from table 'sample_fractions', such as 19 ('total'), 5 ('dissolved'), or 18 ('suspended'). Required if the column 'sample_fraction' in table 'parameters' is TRUE for the parameter in question.
#' - 'result_speciation_id': a numeric specifying the result_speciation_id of the data point from table 'result_speciations', such as 3 (as CaCO3), 5 (as CN), or 44 (of S). Required if the column 'result_speciation' in table 'parameters' is TRUE for the parameter in question.
#' - 'no_source_update': logical; `TRUE` preserves that result from later
#'   source-adapter synchronization while still allowing direct user edits.
#'
#' `sample_qualifiers` may be a vector of `qualifier_type_id` values or a data
#' frame containing `qualifier_type_id` and optional `note`. `sample_observers`
#' may be a vector of `observer_id` values or a data frame containing
#' `observer_id` and optional `observer_role` and `note`.
#'
#' Component-built results use two data frames. `result_aggregations` has one
#' row per aggregated result, with `result_row` (the one-based row in `results`)
#' and exactly one of `aggregation_type` or `result_aggregation_type_id`.
#' Supported type codes are `mean`, `median`, `min`, `max`, `sum`, and
#' `weighted_mean`. Optional columns are `calculation_version` (default 1),
#' `calculation_arguments` (a JSON object or named-list column), positive integer
#' `expected_count` (or `NA` when the protocol has no fixed count), and `note`.
#'
#' Version 1 calculation arguments support `missing_values` (`ignore`,
#' `propagate`, or `error`), `non_detects` (`exclude`, `zero`,
#' `condition_value`, `half_condition_value`, or `error`), numeric `multiplier`,
#' and integer `rounding_digits`. Multiplication and rounding happen after the
#' aggregation, with rounding last.
#'
#' `result_components` has required `result_row`, `observation_number`, and
#' `result` columns; `result` may be `NA` when a condition or missing observation
#' is stored. Optional columns are `observation_datetime`, `result_condition`,
#' `result_condition_value`, `included_in_aggregate` (default `TRUE`), `weight`,
#' and `note`. Excluded observations require a note. Each configured aggregation
#' requires at least one component. The database calculates the canonical
#' `results.result`; a temporary `NULL` used during assembly cannot commit, and
#' an aggregation with no calculable included value is rejected.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param sample A data.frame containing the sample metadata for a single discrete sample. Should contain a single row for a single sample.
#' @param results A data.frame containing the results corresponding to the sample. Should contain one row per result.
#' @param sample_groups Optional sample-group specifications. Supply a numeric
#'   vector of existing `sample_group_id` values, or a data frame with one row
#'   per group. A data-frame row can contain `sample_group_id`, or the arguments
#'   used by [createSampleGroup()]. Optional membership columns are
#'   `sequence_in_group` and `member_note`. Groups with the same owner, type,
#'   and code are reused.
#' @param sample_qualifiers Optional sample-level qualifier associations.
#' @param sample_observers Optional sample-level observer associations.
#' @param result_aggregations Optional aggregation configurations linked to rows
#'   in `results`.
#' @param result_components Optional observations used by the configured result
#'   aggregations.
#'
#' @return The database sample_id for the inserted sample.
#' @export

addNewDiscrete <- function(
  con,
  sample,
  results,
  sample_groups = NULL,
  sample_qualifiers = NULL,
  sample_observers = NULL,
  result_aggregations = NULL,
  result_components = NULL
) {
  # Ensure the sample df has only one row
  if (nrow(sample) != 1) {
    stop("The 'sample' data.frame must have exactly one row.")
  }

  if (nrow(results) < 1) {
    stop("The 'results' data.frame must have at least one row.")
  }
  if (!("no_source_update" %in% names(sample))) {
    sample$no_source_update <- FALSE
  }
  sample$no_source_update[is.na(sample$no_source_update)] <- FALSE
  if (!("no_source_update" %in% names(results))) {
    results$no_source_update <- FALSE
  }
  results$no_source_update[is.na(results$no_source_update)] <- FALSE

  missing_result_columns <- setdiff(
    c("parameter_id", "result", "result_type"),
    names(results)
  )
  if (length(missing_result_columns)) {
    stop(
      "The 'results' data.frame is missing required columns: ",
      paste(missing_result_columns, collapse = ", "),
      "."
    )
  }

  if ("sample_qualifier" %in% names(sample)) {
    stop(
      "sample must not contain sample_qualifier. Supply all sample qualifiers ",
      "through sample_qualifiers."
    )
  }

  required_sample_columns <- c(
    "location_id",
    "media_id",
    "datetime",
    "collection_method",
    "sample_type",
    "owner"
  )
  missing_sample_columns <- setdiff(required_sample_columns, names(sample))
  if (length(missing_sample_columns) > 0L) {
    stop(
      "The 'sample' data.frame is missing required columns: ",
      paste(missing_sample_columns, collapse = ", "),
      "."
    )
  }

  source_value <- if ("import_source" %in% names(sample)) {
    sample$import_source[[1]]
  } else {
    NA_character_
  }
  source_id_value <- if ("import_source_id" %in% names(sample)) {
    sample$import_source_id[[1]]
  } else {
    NA_character_
  }
  source_missing <- length(source_value) == 0L || is.na(source_value)
  source_id_missing <- length(source_id_value) == 0L || is.na(source_id_value)
  if (!source_missing && !nzchar(trimws(as.character(source_value)))) {
    stop("import_source must be nonblank when supplied.")
  }
  if (!source_id_missing && !nzchar(trimws(as.character(source_id_value)))) {
    stop("import_source_id must be nonblank when supplied.")
  }
  if (xor(source_missing, source_id_missing)) {
    stop(
      "import_source and import_source_id must either both be supplied or ",
      "both be absent."
    )
  }

  sample_type <- suppressWarnings(as.integer(sample$sample_type[[1]]))
  type_requirements <- DBI::dbGetQuery(
    con,
    "SELECT requires_location, requires_sample_group
     FROM discrete.sample_types
     WHERE sample_type_id = $1;",
    params = list(sample_type)
  )
  if (nrow(type_requirements) != 1L) {
    stop("sample_type does not identify one discrete.sample_types row.")
  }

  location_id <- suppressWarnings(as.integer(sample$location_id[[1]]))
  sub_location_id <- if ("sub_location_id" %in% names(sample)) {
    suppressWarnings(as.integer(sample$sub_location_id[[1]]))
  } else {
    NA_integer_
  }
  has_groups <- if (inherits(sample_groups, "data.frame")) {
    nrow(sample_groups) > 0L
  } else {
    length(sample_groups) > 0L
  }
  if (isTRUE(type_requirements$requires_location[[1]]) && is.na(location_id)) {
    stop("The selected sample_type requires a location_id.")
  }
  if (is.na(location_id) && !is.na(sub_location_id)) {
    stop("A sub_location_id cannot be supplied without a location_id.")
  }
  if (
    (is.na(location_id) ||
      isTRUE(type_requirements$requires_sample_group[[1]])) &&
      !has_groups
  ) {
    stop(
      "This sample requires at least one sample group. Supply sample_groups ",
      "so the sample and membership can be committed together."
    )
  }

  # Normalize a compact ID vector or association data frame. This local helper
  # is intentionally nested because it is specific to the two association
  # arguments accepted here. It retains only the ID and allowed optional
  # columns, coerces IDs to integer, and rejects missing or duplicate links.
  normalize_association <- function(x, id_column, optional_columns) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!inherits(x, "data.frame")) {
      x <- data.frame(value = x)
      names(x) <- id_column
    }
    if (!id_column %in% names(x)) {
      stop(id_column, " is required in its association input.")
    }
    x <- x[, intersect(c(id_column, optional_columns), names(x)), drop = FALSE]
    if (nrow(x) && any(is.na(x[[id_column]]))) {
      stop(id_column, " cannot contain missing values.")
    }
    x[[id_column]] <- as.integer(x[[id_column]])
    if (anyDuplicated(x[, setdiff(names(x), "note"), drop = FALSE])) {
      stop("Duplicate ", id_column, " association rows are not allowed.")
    }
    x
  }

  sample_qualifiers <- normalize_association(
    sample_qualifiers,
    "qualifier_type_id",
    "note"
  )
  sample_observers <- normalize_association(
    sample_observers,
    "observer_id",
    c("observer_role", "note")
  )
  if (!is.null(sample_observers)) {
    if (!"observer_role" %in% names(sample_observers)) {
      sample_observers$observer_role <- "sampler"
    }
    sample_observers$observer_role <- trimws(
      as.character(sample_observers$observer_role)
    )
    if (any(is.na(sample_observers$observer_role)) ||
        any(!nzchar(sample_observers$observer_role))) {
      stop("sample_observers$observer_role must be non-missing and nonblank.")
    }
    if (anyDuplicated(sample_observers[c("observer_id", "observer_role")])) {
      stop("Duplicate observer_id and observer_role associations are not allowed.")
    }
  }

  normalized_aggregations <- normalize_discrete_result_aggregations(
    results = results,
    result_aggregations = result_aggregations,
    result_components = result_components
  )
  results <- normalized_aggregations$results
  result_aggregations <- normalized_aggregations$result_aggregations
  result_components <- normalized_aggregations$result_components

  # Define a commit function that will be run within a transaction
  commit_fx <- function(
    con,
    sample,
    results,
    sample_groups,
    sample_qualifiers,
    sample_observers,
    result_aggregations,
    result_components
  ) {
    # Insert the sample data
    if ("sample_id" %in% names(sample)) {
      stop("sample must not supply sample_id; it is generated by the database.")
    }
    sample_id <- DBI::dbGetQuery(
      con,
      "SELECT nextval(
         pg_get_serial_sequence('discrete.samples', 'sample_id')
       )::integer AS sample_id;"
    )$sample_id[[1]]
    sample$sample_id <- sample_id
    dbAppendTableRLS(con, "discrete.samples", sample)

    link_discrete_sample_groups(
      con = con,
      sample_id = sample_id,
      sample_groups = sample_groups,
      default_owner = sample$owner[[1]],
      default_contributor = if ("contributor" %in% names(sample)) {
        sample$contributor[[1]]
      } else {
        NA_integer_
      }
    )

    if (!is.null(sample_qualifiers) && nrow(sample_qualifiers)) {
      sample_qualifiers$sample_id <- sample_id
      dbAppendTableRLS(con, "discrete.sample_qualifiers", sample_qualifiers)
    }
    if (!is.null(sample_observers) && nrow(sample_observers)) {
      sample_observers$sample_id <- sample_id
      dbAppendTableRLS(con, "discrete.sample_observers", sample_observers)
    }

    # Insert the results data
    results$sample_id <- sample_id
    results <- normalize_discrete_result_matrix_states(
      con = con,
      sample_media_id = sample$media_id[1],
      results = results
    )
    result_ids <- DBI::dbGetQuery(
      con,
      "SELECT nextval(
         pg_get_serial_sequence('discrete.results', 'result_id')
       )::integer AS result_id
       FROM generate_series(1, $1)",
      params = list(nrow(results))
    )$result_id
    results$result_id <- result_ids
    dbAppendTableRLS(con, "discrete.results", results)

    append_discrete_result_aggregations(
      con = con,
      result_ids = result_ids,
      result_aggregations = result_aggregations,
      result_components = result_components
    )

    return(sample_id)
  }

  # Append values in a transaction block ##########
  activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
  has_result_aggregations <- !is.null(result_aggregations) &&
    nrow(result_aggregations) > 0L
  if (activeTrans) {
    sample_id <- tryCatch(
      {
        if (has_result_aggregations) {
          set_result_aggregation_constraints(con, "deferred")
        }
        committed_sample_id <- commit_fx(
          con,
          sample,
          results,
          sample_groups,
          sample_qualifiers,
          sample_observers,
          result_aggregations,
          result_components
        )
        if (has_result_aggregations) {
          set_result_aggregation_constraints(con, "immediate")
        }
        DBI::dbExecute(con, "COMMIT;")
        committed_sample_id
      },
      error = function(e) {
        DBI::dbExecute(con, "ROLLBACK;")
        stop(e)
      }
    )
  } else {
    # we're already in a transaction
    if (has_result_aggregations) {
      set_result_aggregation_constraints(con, "deferred")
    }
    sample_id <- commit_fx(
      con,
      sample,
      results,
      sample_groups,
      sample_qualifiers,
      sample_observers,
      result_aggregations,
      result_components
    )
    if (has_result_aggregations) {
      set_result_aggregation_constraints(con, "immediate")
    }
  }

  return(sample_id)
}
