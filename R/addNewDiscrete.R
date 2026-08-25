#' Add new discrete sample data to the database
#'
#' @description
#' Appends a discrete sample, its sample-group memberships, and its results to
#' AquaCache inside one transaction, returning the created `sample_id`. The
#' formats of the data frames are defined in the `details` section.
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
#' - 'contributor': the numeric organization ID that contributed the sample.
#' - 'approval': the approval status of the data, as a character string. This should match entries in the 'approvals' table and an error will be thrown if it does not.
#' - 'grade': the grade of the data, as a character string. This should match entries in the 'grades' table and an error will be thrown if it does not.
#' - 'qualifier': the qualifier of the data, as a character string. This should match entries in the 'qualifiers' table and an error will be thrown if it does not.
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
#'
#' @return The database sample_id for the inserted sample.
#' @export

addNewDiscrete <- function(con, sample, results, sample_groups = NULL) {
  # Ensure the sample df has only one row
  if (nrow(sample) != 1) {
    stop("The 'sample' data.frame must have exactly one row.")
  }

  if (nrow(results) < 1) {
    stop("The 'results' data.frame must have at least one row.")
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

  # Define a commit function that will be run within a transaction
  commit_fx <- function(con, sample, results, sample_groups) {
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

    # Insert the results data
    results$sample_id <- sample_id
    results <- normalize_discrete_result_matrix_states(
      con = con,
      sample_media_id = sample$media_id[1],
      results = results
    )
    dbAppendTableRLS(con, "discrete.results", results)

    return(sample_id)
  }

  # Append values in a transaction block ##########
  activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
  if (activeTrans) {
    sample_id <- tryCatch(
      {
        committed_sample_id <- commit_fx(
          con,
          sample,
          results,
          sample_groups
        )
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
    sample_id <- commit_fx(con, sample, results, sample_groups)
  }

  return(sample_id)
}
