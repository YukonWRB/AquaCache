#' Prepare independent result rows from one source sample
#'
#' Invalid measurements are rejected individually. Result-row references in
#' aggregation configuration and components are remapped after filtering so a
#' rejected parent cannot shift its children onto another measurement. Structural
#' errors in the aggregation payload still stop the record instead of guessing
#' which relationships the source intended. Manual addNewDiscrete() stays strict.
#' @param con AquaCache connection.
#' @param sample_media_id Parent sample media identifier.
#' @param results Source result rows.
#' @param result_aggregations,result_components Optional source aggregation data.
#' @return Normalized results and children, rejected original rows, and one
#'   diagnostic per original result. Missing analytical metadata is not inferred.
#' @noRd
prepare_discrete_import_results <- function(
  con,
  sample_media_id,
  results,
  result_aggregations = NULL,
  result_components = NULL
) {
  results <- as.data.frame(data.table::copy(results))
  required <- c("parameter_id", "result_type", "result")
  if (!all(required %in% names(results))) {
    stop("Source results require: ", paste(required, collapse = ", "), ".")
  }
  normalized <- normalize_discrete_result_aggregations(
    results,
    result_aggregations,
    result_components
  )
  results <- normalized$results
  errors <- rep("", nrow(results))
  parameter_id <- suppressWarnings(as.numeric(results$parameter_id))
  errors[
    is.na(parameter_id) |
      !is.finite(parameter_id) |
      parameter_id < 1 |
      parameter_id != trunc(parameter_id)
  ] <- "Choose a valid parameter."
  if (!"matrix_state_id" %in% names(results)) {
    results$matrix_state_id <- NA_integer_
  }
  # Resolve matrix state per row; one unknown source label must not discard the
  # other measurements. The resolver performs the existing unit/media checks.
  for (i in which(!nzchar(errors))) {
    resolved <- tryCatch(
      normalize_discrete_result_matrix_states(
        con,
        sample_media_id,
        results[i, , drop = FALSE]
      ),
      error = function(e) e
    )
    if (inherits(resolved, "error")) {
      errors[[i]] <- conditionMessage(resolved)
    } else {
      results$matrix_state_id[[i]] <- resolved$matrix_state_id[[1]]
    }
  }
  results$matrix_state <- NULL
  chemistry <- validateResultChemistry(con, results, error = FALSE)
  errors <- trimws(paste(errors, chemistry))
  aggregated <- seq_len(nrow(results)) %in%
    normalized$result_aggregations$result_row
  condition <- if ("result_condition" %in% names(results)) {
    results$result_condition
  } else {
    rep(NA_integer_, nrow(results))
  }
  limit <- if ("result_condition_value" %in% names(results)) {
    results$result_condition_value
  } else {
    rep(NA_real_, nrow(results))
  }
  value <- suppressWarnings(as.numeric(results$result))
  invalid_value <- !aggregated &
    ((!is.na(results$result) & is.na(value)) |
      (!is.na(value) & (!is.finite(value) | !is.na(condition))) |
      (is.na(value) & is.na(condition)) |
      (condition %in% c(1L, 2L) & (is.na(limit) | !is.finite(limit))) |
      (!is.na(limit) & (!condition %in% c(1L, 2L) | !is.finite(limit))))
  errors[invalid_value] <- trimws(paste(
    errors[invalid_value],
    "Record a finite value or a valid result condition and detection limit."
  ))
  results$result <- value
  # Both import and synchronization write scalar JSON strings, including when
  # an adapter supplied a named-list column. PostgreSQL generates the digest.
  raw_conditions <- if ("analysis_conditions" %in% names(results)) {
    results$analysis_conditions
  } else {
    rep(list(NULL), nrow(results))
  }
  conditions <- rep("{}", nrow(results))
  for (i in which(!nzchar(errors))) {
    conditions[[i]] <- normalizeAnalysisConditions(raw_conditions[[i]])
  }
  results$analysis_conditions <- conditions
  results$analysis_conditions_key <- NULL
  keep <- which(!nzchar(errors))
  row_map <- match(seq_len(nrow(results)), keep)
  for (child in c("result_aggregations", "result_components")) {
    rows <- normalized[[child]]
    if (!is.null(rows)) {
      rows <- rows[rows$result_row %in% keep, , drop = FALSE]
      rows$result_row <- row_map[rows$result_row]
      normalized[[child]] <- rows
    }
  }
  normalized$rejected_results <- results[nzchar(errors), , drop = FALSE]
  normalized$errors <- errors
  normalized$results <- results[keep, , drop = FALSE]
  normalized
}

#' Match source measurements to stored analytical identities
#'
#' Exact matches may receive source corrections. A historical identity can be
#' enriched in place only when all recorded metadata agrees, measurement values
#' are unchanged, and the pairing is unique across the entire source batch. This
#' prevents source row order from assigning one unknown result to two distinct
#' qualified measurements. Arrays in analysis conditions must remain identical;
#' named objects can gain previously missing members. Values for remote rows
#' that are derived aggregation parents are compared through their components,
#' not against a stored recomputed value.
#' @param database_results Stored result rows for the source-owned sample.
#' @param remote_results Prepared result rows from that same source sample.
#' @param aggregated_result_rows Row positions in `remote_results` that are
#'   derived aggregation parents.
#' @return Matched result IDs, an ambiguity flag per remote row, and stored IDs
#'   to retain when ambiguous source rows cannot safely be reconciled.
#' @noRd
match_discrete_import_results <- function(
  database_results,
  remote_results,
  aggregated_result_rows = integer()
) {
  database_results <- as.data.frame(database_results)
  remote_results <- as.data.frame(remote_results)
  keys <- c(
    "parameter_id",
    "result_type",
    "matrix_state_id",
    "sample_fraction_id",
    "result_value_type",
    "reporting_basis_id",
    "result_speciation_id",
    "result_target_id",
    "endpoint_mode",
    "endpoint_ph",
    "protocol_method",
    "laboratory",
    "analysis_datetime"
  )
  for (field in keys) {
    if (!field %in% names(database_results)) {
      database_results[[field]] <- rep(NA, nrow(database_results))
    }
    if (!field %in% names(remote_results)) {
      remote_results[[field]] <- rep(NA, nrow(remote_results))
    }
  }
  conditions <- function(rows) {
    if (!"analysis_conditions" %in% names(rows)) {
      return(rep("{}", nrow(rows)))
    }
    vapply(rows$analysis_conditions, normalizeAnalysisConditions, character(1))
  }
  old_conditions <- conditions(database_results)
  new_conditions <- conditions(remote_results)
  refines_conditions <- function(old, new) {
    if (is.null(old)) {
      return(TRUE)
    }
    if (is.list(old) && length(old) && !is.null(names(old))) {
      return(
        is.list(new) &&
          !is.null(names(new)) &&
          all(names(old) %in% names(new)) &&
          all(vapply(
            names(old),
            function(key) refines_conditions(old[[key]], new[[key]]),
            logical(1)
          ))
      )
    }
    identical(old, new)
  }
  exact <- refinement <- matrix(
    FALSE,
    nrow(database_results),
    nrow(remote_results)
  )
  for (i in seq_len(nrow(remote_results))) {
    same <- compatible <- rep(TRUE, nrow(database_results))
    for (field in keys) {
      old <- database_results[[field]]
      new <- remote_results[[field]][[i]]
      equal <- if (is.na(new)) is.na(old) else !is.na(old) & old == new
      same <- same & equal
      compatible <- compatible &
        if (field %in% c("parameter_id", "result_type")) {
          equal
        } else {
          is.na(old) | equal
        }
    }
    exact[, i] <- same & old_conditions == new_conditions[[i]]
    # For direct results, metadata enrichment is safe only when the original
    # value/censoring and recorded laboratory identifiers agree. Aggregate
    # parent values and censoring metadata are recomputed from components, so
    # they cannot identify the stored parent during matching.
    measurement_fields <- c("lab_sample_no", "lab_report_no")
    if (!(i %in% aggregated_result_rows)) {
      measurement_fields <- c(
        "result",
        "result_condition",
        "result_condition_value",
        measurement_fields
      )
    }
    for (field in measurement_fields) {
      old <- if (field %in% names(database_results)) {
        database_results[[field]]
      } else {
        rep(NA, nrow(database_results))
      }
      new <- if (field %in% names(remote_results)) {
        remote_results[[field]][[i]]
      } else {
        NA
      }
      compatible <- compatible &
        if (is.na(new)) {
          is.na(old)
        } else {
          vapply(
            old,
            function(value) {
              !is.na(value) &&
                isTRUE(all.equal(value, new, check.attributes = FALSE))
            },
            logical(1)
          )
        }
    }
    for (j in which(compatible)) {
      compatible[[j]] <- old_conditions[[j]] == "{}" ||
        refines_conditions(
          jsonlite::fromJSON(old_conditions[[j]], simplifyVector = FALSE),
          jsonlite::fromJSON(new_conditions[[i]], simplifyVector = FALSE)
        )
    }
    refinement[, i] <- compatible
  }
  # Exact identities claim their rows first. All other enrichment proposals are
  # considered together, rather than letting the first remote row win.
  candidates <- exact
  unclaimed <- rowSums(exact) == 0L
  for (i in which(colSums(exact) == 0L)) {
    candidates[, i] <- refinement[, i] & unclaimed
  }
  ambiguous <- colSums(candidates) > 1L |
    colSums(candidates & (rowSums(candidates) > 1L)) > 0L
  ids <- rep(NA_integer_, nrow(remote_results))
  for (i in which(!ambiguous & colSums(candidates) == 1L)) {
    ids[[i]] <- database_results$result_id[which(candidates[, i])]
  }
  list(
    result_ids = ids,
    ambiguous = ambiguous,
    retained_ids = database_results$result_id[
      rowSums(candidates[, ambiguous, drop = FALSE]) > 0L
    ]
  )
}
