#' Add new discrete sample data to the database
#'
#' @description
#' Appends a discrete sample and its results to the AquaCache database inside a transaction, returning the created sample_id. The format of the data.frames passed as arguments are defined in the `details` section.
#'
#' @details
#' The 'sample' data.frame must contain the following columns:
#' - 'location_id': a numeric specifying the location_id of the data point from table 'locations'.
#' - 'media_id': a numeric specifying the media_id of the data point from table 'medias'.
#' - 'datetime': a POSIXct datetime object in UTC 0 time zone, specifying the datetime of the data point.
#' - 'collection_method': a numeric specifying the collection_method_id of the data point from table 'collection_methods', such as 1 (observation), 27 (water bottle), or 14 (pump).
#' - 'sample_type': a numeric specifying the sample_type_id of the data point from table 'sample_types', such as 1 (grab), 2 (composite), or 3 (integrated).
#' - 'owner': the owner of the data, as a character string. This should match entries in the 'organizations' table and an error will be thrown if it does not.
#' - 'import_source_id': a numeric specifying the import_source_id of the data point from table 'import_sources' (use for tracking purposes).
#' Optional columns are:
#' - 'target_datetime': a POSIXct datetime object in UTC 0 time zone, specifying an artificial datetime for the data point which can be used for data analysis or plotting purposes.
#' - 'note': a character string with a note about the data point(s).
#' - 'contributor' the name of the person or organization that contributed the data, as a character string. This should match entries in the 'organizations' table and an error will be thrown if it does not.
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
#' - 'sample_fraction_id': a numeric specifying the sample_fraction_id of the data point from table 'sample_fractions', such as 19 ('total'), 5 ('dissolved'), or 18 ('suspended'). Required if the column 'sample_fraction' in table 'parameters' is TRUE for the parameter in question.
#' - 'result_speciation_id': a numeric specifying the result_speciation_id of the data point from table 'result_speciations', such as 3 (as CaCO3), 5 (as CN), or 44 (of S). Required if the column 'result_speciation' in table 'parameters' is TRUE for the parameter in question.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param sample A data.frame containing the sample metadata for a single discrete sample. Should contain a single row for a single sample.
#' @param results A data.frame containing the results corresponding to the sample. Should contain one row per result.
#'
#' @return The database sample_id for the inserted sample.
#' @export

addNewDiscrete <- function(con, sample, results) {
  # Ensure the sample df has only one row
  if (nrow(sample) != 1) {
    stop("The 'sample' data.frame must have exactly one row.")
  }

  if (nrow(results) < 1) {
    stop("The 'results' data.frame must have at least one row.")
  }

  # Define a commit function that will be run within a transaction
  commit_fx <- function(con, sample, results) {
    # Insert the sample data
    DBI::dbAppendTable(con, "samples", sample)

    # Get the sample_id using all fields that define a unique sample
    sample_id <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT sample_id FROM samples WHERE location_id = ",
        sample$location_id,
        " AND datetime = '",
        sample$datetime,
        " UTC'",
        " AND media_id = ",
        sample$media_id,
        " AND sample_type = ",
        sample$sample_type,
        " AND collection_method = ",
        sample$collection_method,
        ifelse(
          is.null(sample$sub_location_id) || is.na(sample$sub_location_id),
          " AND sub_location_id IS NULL",
          paste0(" AND sub_location_id = ", sample$sub_location_id)
        ),
        ifelse(
          is.null(sample$z) || is.na(sample$z),
          " AND z IS NULL",
          paste0(" AND z = ", sample$z)
        ),
        " AND import_source = '",
        sample$import_source,
        "';"
      )
    )[1, 1]

    # Insert the results data
    results$sample_id <- sample_id
    DBI::dbAppendTable(con, "results", results)

    return(sample_id)
  }

  # Append values in a transaction block ##########
  activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
  if (activeTrans) {
    sample_id <- tryCatch(
      {
        committed_sample_id <- commit_fx(con, sample, results)
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
    sample_id <- commit_fx(con, sample, results)
  }

  return(sample_id)
}
