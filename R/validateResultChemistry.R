#' Validate analytical metadata for discrete results
#'
#' @description
#' Check independently required reporting bases, chemical speciations, analytical
#' targets, and endpoint definitions before importing or entering results. The
#' database performs authoritative checks again when results are written.
#' @param con AquaCache connection. May be `NULL` when `parameters` is supplied.
#' @param results A data.frame or data.table containing `parameter_id` and any
#'   applicable `sample_fraction_id`, `reporting_basis_id`,
#'   `result_speciation_id`, `result_target_id`, `endpoint_mode`, and `endpoint_ph`.
#' @param parameters Optional parameter metadata with `parameter_id` and the
#'   `sample_fraction`, `reporting_basis`, `result_speciation`, `result_target`,
#'   and `endpoint` requirement columns. Supplying cached metadata avoids a query.
#' @param error Whether to stop when errors occur. With `FALSE`, return one
#'   validation message per result row; an empty string means the row passed.
#' @param matrix_states Optional cached matrix-state catalogue. Needed with
#'   cached parameters when condition requirements differ by matrix state.
#' @return Invisibly, a character vector aligned with result rows.
#' @details
#' Advisory parameter suggestions do not restrict valid selections. Missing
#' analytical metadata never implies an all-species scope or a fixed endpoint.
#' The explicit endpoint mode `unknown` records an unreported endpoint and
#' requires missing pH.
#' pH values may lie outside 0--14, but must be finite. Physical-unit and
#' reporting-basis conversions require an explicitly defined analytical target.
#' @export
validateResultChemistry <- function(con, results, parameters = NULL, error = TRUE, matrix_states = NULL) {
  rows <- data.table::as.data.table(results)
  if (!"parameter_id" %in% names(rows)) stop("Results require parameter_id.", call. = FALSE)
  if (!nrow(rows)) return(invisible(character()))
  ids <- suppressWarnings(as.numeric(rows$parameter_id))
  messages <- rep("", nrow(rows))
  invalid_id <- is.na(ids) | !is.finite(ids) | ids < 1 | ids != trunc(ids)
  messages[invalid_id] <- "Choose a valid parameter."
  if (is.null(parameters)) {
    if (is.null(con)) stop("Supply con or parameter metadata.", call. = FALSE)
    wanted <- unique(ids[!invalid_id])
    parameters <- if (length(wanted)) DBI::dbGetQuery(con,
      "SELECT parameter_id, sample_fraction, reporting_basis, result_speciation,
              result_target, endpoint, to_jsonb(analysis_conditions_required_keys) AS analysis_conditions_required_keys,
              analysis_conditions_required_keys_by_matrix FROM public.parameters
       WHERE parameter_id IN (SELECT jsonb_array_elements_text($1::jsonb)::INTEGER)",
      params = list(jsonlite::toJSON(wanted, auto_unbox = FALSE))) else data.frame(parameter_id = integer())
  }
  parameters <- data.table::as.data.table(parameters)
  match_id <- match(ids, parameters$parameter_id)
  messages[is.na(match_id)] <- "Choose a valid parameter."
  requirements <- c("sample_fraction", "reporting_basis", "result_speciation", "result_target", "endpoint")
  columns <- c("sample_fraction_id", "reporting_basis_id", "result_speciation_id", "result_target_id", "endpoint_mode")
  labels <- c("Sample fraction", "Reporting basis", "Chemical speciation", "Analytical target", "Endpoint definition")
  for (i in seq_along(requirements)) {
    requirement <- requirements[[i]]
    column <- columns[[i]]
    values <- if (column %in% names(rows)) rows[[column]] else rep(NA, nrow(rows))
    missing <- is.na(values) | trimws(as.character(values)) == ""
    required <- if (requirement %in% names(parameters))
      parameters[[requirement]][match_id] %in% TRUE else rep(FALSE, nrow(rows))
    failed <- required & missing
    message <- paste0(labels[[i]], " is required for the selected parameter.")
    messages[failed] <- ifelse(messages[failed] == "", message, paste(messages[failed], message))
    if (column != "endpoint_mode") {
      numbers <- suppressWarnings(as.numeric(values))
      failed <- is.nan(numbers) | (!missing & (is.na(numbers) | !is.finite(numbers) | numbers < 1 | numbers != trunc(numbers)))
      message <- paste0(labels[[i]], " must be a positive integer ID.")
      messages[failed] <- ifelse(messages[failed] == "", message, paste(messages[failed], message))
    }
  }
  mode <- if ("endpoint_mode" %in% names(rows)) trimws(as.character(rows$endpoint_mode)) else rep(NA_character_, nrow(rows))
  mode[!is.na(mode) & mode == ""] <- NA_character_
  raw_ph <- if ("endpoint_ph" %in% names(rows)) rows$endpoint_ph else rep(NA_real_, nrow(rows))
  ph <- suppressWarnings(as.numeric(raw_ph))
  ph_present <- is.nan(ph) | (!is.na(raw_ph) & trimws(as.character(raw_ph)) != "")
  failed <- (!is.na(mode) & !mode %in% c("fixed", "inflection", "gran", "other", "unknown")) |
    (mode %in% "fixed" & !ph_present) | (mode %in% "unknown" & ph_present) |
    (is.na(mode) & ph_present) |
    (ph_present & (is.na(ph) | !is.finite(ph)))
  message <- paste(
    "Record a valid endpoint mode; fixed endpoints require finite pH,",
    "and unknown endpoints must not include pH."
  )
  messages[failed] <- ifelse(messages[failed] == "", message, paste(messages[failed], message))
  policies <- if ("analysis_conditions_required_keys_by_matrix" %in% names(parameters))
    lapply(parameters$analysis_conditions_required_keys_by_matrix, function(value) {
      if (is.character(value)) jsonlite::fromJSON(value, simplifyVector = FALSE) else value
    }) else rep(list(list()), nrow(parameters))
  global_keys <- if ("analysis_conditions_required_keys" %in% names(parameters))
    lapply(parameters$analysis_conditions_required_keys, function(value) {
      if (is.character(value) && length(value) == 1L) {
        # RPostgres exposes TEXT[] as pq__text strings; cached queries use JSON
        # arrays. Accept both encodings, as well as ordinary R list columns.
        if (value %in% c("{}", "[]")) return(character())
        if (startsWith(value, "[")) return(as.character(jsonlite::fromJSON(value)))
        if (startsWith(value, "{") && endsWith(value, "}")) {
          fields <- utils::read.csv(text = substr(value, 2L, nchar(value) - 1L), header = FALSE,
            colClasses = "character", na.strings = character(), allowEscapes = TRUE, check.names = FALSE)
          return(unlist(fields, use.names = FALSE))
        }
      }
      as.character(unlist(value, use.names = FALSE))
    }) else rep(list(character()), nrow(parameters))
  has_policies <- any(lengths(policies) > 0L)
  codes <- if ("matrix_state_code" %in% names(rows)) as.character(rows$matrix_state_code) else rep(NA_character_, nrow(rows))
  if (has_policies && anyNA(codes)) {
    if (is.null(matrix_states) && !is.null(con)) matrix_states <- DBI::dbGetQuery(con,
      "SELECT matrix_state_id, matrix_state_code FROM public.matrix_states")
    if (!is.null(matrix_states) && "matrix_state_id" %in% names(rows)) {
      codes <- matrix_states$matrix_state_code[match(rows$matrix_state_id, matrix_states$matrix_state_id)]
    }
  }
  for (i in seq_len(nrow(rows))) {
    raw_conditions <- if ("analysis_conditions" %in% names(rows)) rows$analysis_conditions[[i]] else NULL
    normalized <- tryCatch(normalizeAnalysisConditions(raw_conditions), error = function(e) e)
    if (inherits(normalized, "error")) {
      messages[[i]] <- trimws(paste(messages[[i]], conditionMessage(normalized)))
      next
    }
    if (!is.na(match_id[[i]])) {
      parameter <- match_id[[i]]
      keys <- global_keys[[parameter]]
      policy <- policies[[parameter]]
      if (length(policy)) {
        if (is.na(codes[[i]])) {
          messages[[i]] <- trimws(paste(messages[[i]], "Choose a valid matrix state for its analysis conditions."))
        } else keys <- unique(c(keys, unlist(policy[[codes[[i]]]], use.names = FALSE)))
      }
      conditions <- jsonlite::fromJSON(normalized, simplifyVector = FALSE)
      missing <- keys[vapply(keys, function(key) is.null(conditions[[key]]), logical(1))]
      if (length(missing)) messages[[i]] <- trimws(paste(messages[[i]],
        "Required analysis conditions:", paste(missing, collapse = ", ")))
    }
  }
  if (isTRUE(error) && any(nzchar(messages))) {
    stop(paste(unique(messages[nzchar(messages)]), collapse = "\n"), call. = FALSE)
  }
  invisible(messages)
}
