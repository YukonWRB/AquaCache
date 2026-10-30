#' Validate and serialize conditions that define a chemical measurement
#'
#' @param conditions A JSON object string, named list, or missing value.
#' @return A JSON object string suitable for an `analysis_conditions` column.
#' @details
#' Conditions describe numeric interpretation, such as dry versus wet mass or
#' gas reference conditions. Laboratory, import, and calculation provenance use
#' their existing fields. Additional named conditions are supported without a
#' database migration. Common mass/gas and reference-condition fields are checked.
#' Omit unknown optional fields; when supplied, a common field must contain its
#' documented value rather than JSON null, matching the database checks.
#' @export
normalizeAnalysisConditions <- function(conditions = NULL) {
  if (is.null(conditions) || !length(conditions) ||
      (is.character(conditions) && length(conditions) == 1L &&
       (is.na(conditions) || !nzchar(trimws(conditions))))) return("{}")
  if (is.character(conditions) && length(conditions) == 1L) {
    if (!grepl("^[[:space:]]*\\{", conditions) || !jsonlite::validate(conditions)) {
      stop("Analysis conditions must be a JSON object.", call. = FALSE)
    }
    conditions <- jsonlite::fromJSON(conditions, simplifyVector = FALSE)
  }
  if (!is.list(conditions) || (length(conditions) &&
      (is.null(names(conditions)) || anyNA(names(conditions)) ||
       any(!nzchar(trimws(names(conditions)))) || anyDuplicated(names(conditions))))) {
    stop("Analysis conditions require unique, nonempty names.", call. = FALSE)
  }
  # Validate before JSON serialization: mixed types must not conceal Inf/NaN
  # through character coercion, and named objects have a stable key order.
  normalize_value <- function(value) {
    if (is.numeric(value) && any(!is.finite(value))) {
      stop("Numeric analysis conditions must be finite.", call. = FALSE)
    }
    if (is.list(value)) {
      if (!is.null(names(value))) {
        if (anyNA(names(value)) || any(!nzchar(trimws(names(value)))) || anyDuplicated(names(value))) {
          stop("Analysis conditions require unique, nonempty names.", call. = FALSE)
        }
        value <- value[order(names(value))]
      }
      value <- lapply(value, normalize_value)
    }
    value
  }
  conditions <- normalize_value(conditions)
  for (field in c("mass_basis", "gas_basis")) {
    value <- conditions[[field]]
    choices <- if (field == "mass_basis") c("dry", "wet", "ash", "other") else c("dry", "wet", "other")
    if (field %in% names(conditions) && (is.null(value) || !is.character(value) ||
        length(value) != 1L || is.na(value) || !value %in% choices)) {
      stop(field, " must be one of: ", paste(choices, collapse = ", "), ".", call. = FALSE)
    }
  }
  for (field in c("reference_temperature_kelvin", "reference_pressure_kpa")) {
    value <- conditions[[field]]
    if (field %in% names(conditions) && (is.null(value) || !is.numeric(value) ||
        length(value) != 1L || !is.finite(value) || value <= 0)) {
      stop(field, " must be a positive finite number.", call. = FALSE)
    }
  }
  if (!length(conditions)) return("{}")
  as.character(jsonlite::toJSON(conditions, auto_unbox = TRUE, null = "null", digits = NA))
}
