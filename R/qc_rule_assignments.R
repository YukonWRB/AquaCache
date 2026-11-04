# Schema-driven configuration for per-timeseries automatic QC rules.
#
# Rule schemas use the same field descriptors as source-adapter forms. QC
# assignments store effective argument values and explicit companion choices;
# absent companion overrides use the declared same-location lookup policy.

qc_rule_capability_key <- function(rule_fx, rule_version) {
  paste0(rule_fx, "@", as.integer(rule_version))
}

qc_rule_integer_id <- function(value, label, nullable = FALSE) {
  if (
    nullable &&
      (is.null(value) || !length(value) ||
        (length(value) == 1L && is.na(value)))
  ) {
    return(NA_integer_)
  }
  numeric_value <- suppressWarnings(as.numeric(as.character(value)))
  if (
    length(numeric_value) != 1L ||
      is.na(numeric_value) ||
      !is.finite(numeric_value) ||
      numeric_value < 1 ||
      numeric_value > .Machine$integer.max ||
      numeric_value != floor(numeric_value)
  ) {
    stop(label, " must be one positive integer.")
  }
  as.integer(numeric_value)
}

qc_rule_schema_decode <- function(value) {
  if (is.list(value)) {
    schema <- value
  } else if (
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  ) {
    schema <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  } else {
    stop("QC argument_schema must be a decoded list or one JSON object.")
  }
  if (!is.list(schema) || is.null(names(schema))) {
    stop("QC argument_schema must be a named JSON object.")
  }
  schema
}

validateQCRuleArgumentSchema <- function(argument_schema) {
  schema <- qc_rule_schema_decode(argument_schema)
  if (
    length(schema$schema_version) != 1L ||
      is.na(schema$schema_version) ||
      as.integer(schema$schema_version) != 2L
  ) {
    stop("QC argument_schema.schema_version must be 2.")
  }
  if (!is.list(schema$arguments)) {
    stop("QC argument_schema.arguments must be an array.")
  }
  if (!is.list(schema$companions)) {
    stop("QC argument_schema.companions must be an array.")
  }
  if (
    (length(schema$arguments) && !is.null(names(schema$arguments))) ||
      (length(schema$companions) && !is.null(names(schema$companions)))
  ) {
    stop("QC argument_schema arguments and companions must be arrays, not objects.")
  }

  # Reuse the source-adapter descriptor validator for labels, controls, types,
  # choices, and numeric limits. Rule arguments are data, not R formals, so
  # the adapter-specific function-signature check is intentionally disabled.
  validateSourceAdapterArgumentSchema(
    list(schema_version = 1L, arguments = schema$arguments),
    check_function = FALSE
  )

  argument_names <- vapply(
    schema$arguments,
    function(argument) argument$name,
    character(1)
  )
  if (anyDuplicated(argument_names)) {
    stop("QC argument_schema contains duplicate argument names.")
  }
  for (argument in schema$arguments) {
    if (!argument$source %in% c("user", "internal")) {
      stop(
        "QC argument '", argument$name,
        "' must use source 'user' or 'internal'."
      )
    }
    if (is.null(argument$default)) {
      stop("QC argument '", argument$name, "' requires a default.")
    }
    if (!is.null(argument$units) &&
        (!is.character(argument$units) ||
         length(argument$units) != 1L ||
         is.na(argument$units))) {
      stop("QC argument '", argument$name, "' has invalid units metadata.")
    }
    if (identical(argument$source, "user")) {
      for (numeric_field in c("minimum", "maximum", "step")) {
        value <- argument[[numeric_field]]
        if (
          !is.null(value) &&
            (!is.numeric(value) || length(value) != 1L ||
              !is.finite(value))
        ) {
          stop(
            "QC argument '", argument$name, "' has an invalid ",
            numeric_field, "."
          )
        }
      }
      if (
        !is.null(argument$minimum) && !is.null(argument$maximum) &&
          argument$minimum > argument$maximum
      ) {
        stop("QC argument '", argument$name, "' has minimum above maximum.")
      }
      if (!is.null(argument$step) && argument$step <= 0) {
        stop("QC argument '", argument$name, "' has a non-positive step.")
      }
      qc_rule_validate_value(argument$default, argument)
    }
    if (!is.null(argument$visible_when)) {
      condition <- argument$visible_when
      if (
        !is.list(condition) ||
          !isTRUE(condition$argument %in% argument_names) ||
          !is.logical(condition$equals) ||
          length(condition$equals) != 1L ||
          is.na(condition$equals)
      ) {
        stop(
          "QC argument '", argument$name,
          "' has an invalid visible_when condition."
        )
      }
      condition_argument <- schema$arguments[[
        match(condition$argument, argument_names)
      ]]
      if (
        condition_argument$source != "user" ||
          condition_argument$value_type != "logical"
      ) {
        stop(
          "QC argument '", argument$name,
          "' visibility may depend only on a logical user argument."
        )
      }
    }
  }

  companion_roles <- character(length(schema$companions))
  for (i in seq_along(schema$companions)) {
    companion <- schema$companions[[i]]
    required <- c(
      "role", "label", "parameter_name", "selection_strategy",
      "allow_explicit_override", "missing_behavior", "help"
    )
    if (!is.list(companion) || length(setdiff(required, names(companion)))) {
      stop(
        "Every QC companion descriptor requires: ",
        paste(required, collapse = ", "),
        "."
      )
    }
    if (
      !is.character(companion$role) ||
        length(companion$role) != 1L ||
        is.na(companion$role) ||
        !nzchar(trimws(companion$role)) ||
        !is.character(companion$label) ||
        length(companion$label) != 1L ||
        is.na(companion$label) ||
        !nzchar(trimws(companion$label)) ||
        !is.character(companion$parameter_name) ||
        length(companion$parameter_name) != 1L ||
        is.na(companion$parameter_name) ||
        !nzchar(trimws(companion$parameter_name)) ||
        !identical(
          companion$selection_strategy,
          "same_location_lowest_sensor_priority"
        ) ||
        !is.logical(companion$allow_explicit_override) ||
        length(companion$allow_explicit_override) != 1L ||
        is.na(companion$allow_explicit_override) ||
        !identical(companion$missing_behavior, "warn_and_use_na")
    ) {
      stop("QC companion descriptor ", i, " has invalid metadata.")
    }
    if (!is.null(companion$when)) {
      condition <- companion$when
      if (
        !is.list(condition) ||
          !isTRUE(condition$argument %in% argument_names) ||
          !is.logical(condition$equals) ||
          length(condition$equals) != 1L ||
          is.na(condition$equals)
      ) {
        stop(
          "QC companion role '", companion$role,
          "' has an invalid when condition."
        )
      }
      condition_argument <- schema$arguments[[
        match(condition$argument, argument_names)
      ]]
      if (
        condition_argument$value_type != "logical" ||
          !isTRUE(condition_argument$source == "user")
      ) {
        stop(
          "QC companion role '", companion$role,
          "' may only depend on a logical user argument."
        )
      }
    }
    companion_roles[[i]] <- companion$role
  }
  if (anyDuplicated(companion_roles)) {
    stop("QC companion roles must be unique within a rule schema.")
  }

  invisible(schema)
}

qc_rule_schema_defaults <- function(schema) {
  stats::setNames(
    lapply(schema$arguments, function(argument) argument$default),
    vapply(
      schema$arguments,
      function(argument) argument$name,
      character(1)
    )
  )
}

qc_rule_decode_object <- function(value, label) {
  if (
    is.null(value) ||
      !length(value) ||
      (is.atomic(value) && all(is.na(value)))
  ) {
    return(list())
  }
  if (is.character(value) && length(value) == 1L) {
    if (is.na(value) || !nzchar(trimws(value))) return(list())
    value <- tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(e) {
        stop(label, " must be valid JSON: ", conditionMessage(e))
      }
    )
  }
  if (is.atomic(value) && length(value) && !is.null(names(value))) {
    value <- as.list(value)
  }
  if (!is.list(value) || (length(value) && is.null(names(value)))) {
    stop(label, " must be a named object.")
  }
  if (length(value) && any(!nzchar(names(value)))) {
    stop(label, " must use non-blank object keys.")
  }
  value
}

qc_rule_json_object <- function(value) {
  if (!length(value)) return("{}")
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
}

qc_rule_validate_value <- function(value, argument) {
  label <- paste0("QC argument '", argument$label, "'")
  type <- argument$value_type
  if (is.list(value) && type %in% c("character_vector", "numeric_vector")) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
    if (type == "character_vector") value <- as.character(value)
    if (type == "numeric_vector") {
      value <- suppressWarnings(as.numeric(value))
    }
  }
  if (is.null(value) || !length(value) || all(is.na(value))) {
    if (isTRUE(argument$required)) {
      stop(label, " is required.")
    }
    return(argument$default)
  }

  valid <- switch(
    type,
    character = is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(trimws(value)),
    numeric = is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value),
    integer = is.numeric(value) && length(value) == 1L &&
      !is.na(value) && is.finite(value) && value == floor(value) &&
      abs(value) <= .Machine$integer.max,
    logical = is.logical(value) && length(value) == 1L && !is.na(value),
    character_vector = is.character(value) && length(value) > 0L &&
      !anyNA(value),
    numeric_vector = is.numeric(value) && length(value) > 0L &&
      all(is.finite(value)),
    FALSE
  )
  if (!valid) stop(label, " has an invalid value for type ", type, ".")

  choices <- if (is.null(argument$choices)) NULL else {
    unlist(argument$choices, recursive = TRUE, use.names = FALSE)
  }
  if (!is.null(choices) &&
      any(!as.character(value) %in% as.character(choices))) {
    stop(label, " is outside its allowed choices.")
  }
  if (!is.null(argument$minimum) &&
      any(value < argument$minimum)) {
    stop(label, " is below its minimum.")
  }
  if (!is.null(argument$maximum) &&
      any(value > argument$maximum)) {
    stop(label, " is above its maximum.")
  }
  value
}

qc_rule_active_companions <- function(schema, args) {
  Filter(
    function(companion) {
      when <- companion$when
      is.null(when) ||
        identical(args[[when$argument]], when$equals)
    },
    schema$companions
  )
}

qc_rule_effective_args <- function(schema, value) {
  given <- qc_rule_decode_object(value, "rule_args")
  names_allowed <- vapply(
    schema$arguments,
    function(argument) argument$name,
    character(1)
  )
  unknown <- setdiff(names(given), names_allowed)
  if (length(unknown)) {
    stop(
      "Unknown QC argument(s): ",
      paste(unknown, collapse = ", "),
      "."
    )
  }

  effective <- utils::modifyList(qc_rule_schema_defaults(schema), given)
  for (argument in schema$arguments) {
    name <- argument$name
    if (identical(argument$source, "internal")) {
      if (
        name %in% names(given) &&
          !identical(given[[name]], argument$default)
      ) {
        stop("QC argument '", name, "' is managed by the rule.")
      }
      effective[[name]] <- argument$default
    } else {
      effective[[name]] <- qc_rule_validate_value(
        effective[[name]],
        argument
      )
    }
  }
  effective
}

qc_rule_companion_overrides <- function(value, schema, args) {
  overrides <- qc_rule_decode_object(value, "companion_timeseries")
  active <- qc_rule_active_companions(schema, args)
  roles <- vapply(active, function(companion) companion$role, character(1))
  unknown <- setdiff(names(overrides), roles)
  if (length(unknown)) {
    stop(
      "Unknown or inactive QC companion role(s): ",
      paste(unknown, collapse = ", "),
      "."
    )
  }
  for (role in names(overrides)) {
    descriptor <- active[[match(
      role,
      vapply(active, function(companion) companion$role, character(1))
    )]]
    if (!isTRUE(descriptor$allow_explicit_override)) {
      stop("QC companion '", role, "' does not allow an explicit override.")
    }
    id <- overrides[[role]]
    if (
      !is.numeric(id) ||
        length(id) != 1L ||
        is.na(id) ||
        !is.finite(id) ||
        id < 1 ||
        id > .Machine$integer.max ||
        id != floor(id)
    ) {
      stop(
        "QC companion '", role,
        "' must be one positive integer timeseries_id."
      )
    }
    overrides[[role]] <- as.integer(id)
  }
  overrides
}

#' Read the available automatic QC rule definitions
#'
#' Returns enabled, versioned rule definitions and the schema AquaLink uses to
#' build argument and companion-series controls.
#'
#' @param con An open AquaCache database connection.
#' @param enabled_only Whether to omit disabled definitions.
#' @return A data.frame with one row per rule version and a parsed
#'   \code{argument_schema} and \code{primary_parameters} list-columns.
#'   Empty primary-parameter values indicate rules that accept any parameter.
#' @export
getQCRuleCapabilities <- function(con, enabled_only = TRUE) {
  if (
    !is.logical(enabled_only) ||
      length(enabled_only) != 1L ||
      is.na(enabled_only)
  ) {
    stop("getQCRuleCapabilities: enabled_only must be TRUE or FALSE.")
  }
  capabilities <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT rule_fx, rule_version, description, emits,",
      "argument_schema::text AS argument_schema_json, enabled, note",
      "FROM public.qc_rule_capabilities",
      if (enabled_only) "WHERE enabled" else "",
      "ORDER BY rule_fx, rule_version"
    )
  )
  capabilities$argument_schema <- lapply(
    capabilities$argument_schema_json,
    function(value) {
      schema <- qc_rule_schema_decode(value)
      validateQCRuleArgumentSchema(schema)
      schema
    }
  )
  capabilities$primary_parameters <- lapply(
    seq_len(nrow(capabilities)),
    function(i) qc_rule_primary_parameters(
      capabilities$rule_fx[[i]],
      capabilities$rule_version[[i]]
    )
  )
  capabilities$argument_schema_json <- NULL

  rule_keys <- qc_rule_capability_key(
    capabilities$rule_fx,
    capabilities$rule_version
  )
  unknown <- setdiff(rule_keys, names(qc_rule_registry))
  if (length(unknown)) {
    stop(
      "QC rule definition(s) have no R implementation: ",
      paste(unknown, collapse = ", "),
      "."
    )
  }
  capabilities[]
}

#' List QC rule assignments for a continuous timeseries
#'
#' @param con An open AquaCache database connection.
#' @param timeseries_id One continuous timeseries_id.
#' @return A data.frame with parsed argument schema and effective assignment
#'   values in list-columns.
#' @export
getTimeseriesQCRules <- function(con, timeseries_id) {
  tsid <- qc_rule_integer_id(timeseries_id, "timeseries_id")
  assignments <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT r.timeseries_qc_rule_id, r.timeseries_id,",
      "r.rule_fx, r.rule_version, r.rule_args::text AS rule_args_json,",
      "r.companion_timeseries::text AS companion_timeseries_json,",
      "r.run_order, r.active, r.note, c.description, c.emits,",
      "c.argument_schema::text AS argument_schema_json, c.enabled",
      "FROM continuous.timeseries_qc_rules r",
      "JOIN public.qc_rule_capabilities c",
      "ON c.rule_fx = r.rule_fx AND c.rule_version = r.rule_version",
      "WHERE r.timeseries_id = $1",
      "ORDER BY r.run_order, r.timeseries_qc_rule_id"
    ),
    params = list(tsid)
  )
  assignments$argument_schema <- lapply(
    assignments$argument_schema_json,
    function(value) {
      schema <- qc_rule_schema_decode(value)
      validateQCRuleArgumentSchema(schema)
      schema
    }
  )
  assignments$rule_args <- lapply(
    assignments$rule_args_json,
    qc_rule_decode_object,
    label = "rule_args"
  )
  assignments$companion_timeseries <- lapply(
    assignments$companion_timeseries_json,
    qc_rule_decode_object,
    label = "companion_timeseries"
  )
  assignments$argument_schema_json <- NULL
  assignments$rule_args_json <- NULL
  assignments$companion_timeseries_json <- NULL
  assignments
}

#' List compatible companion timeseries for an automatic QC rule
#'
#' @param con An open AquaCache database connection.
#' @param location_id Location whose series can be companions.
#' @param parameter_name Exact parameter name from the rule schema.
#' @param exclude_timeseries_id Optional target timeseries to exclude.
#' @return Compatible active timeseries ordered by sensor priority.
#' @export
getQCRuleCompanionCandidates <- function(
  con,
  location_id,
  parameter_name,
  exclude_timeseries_id = NULL
) {
  location_id <- qc_rule_integer_id(location_id, "location_id")
  if (
    !is.character(parameter_name) ||
      length(parameter_name) != 1L ||
      is.na(parameter_name) ||
      !nzchar(trimws(parameter_name))
  ) {
    stop("parameter_name must be one non-blank parameter name.")
  }
  exclude_timeseries_id <- qc_rule_integer_id(
    exclude_timeseries_id,
    "exclude_timeseries_id",
    nullable = TRUE
  )
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT t.timeseries_id, p.param_name, t.sensor_priority,",
      "t.record_rate, t.timeseries_type",
      "FROM continuous.timeseries t",
      "JOIN public.parameters p ON p.parameter_id = t.parameter_id",
      "WHERE t.location_id = $1 AND p.param_name = $2",
      "AND t.active AND ($3::integer IS NULL OR t.timeseries_id <> $3)",
      "ORDER BY t.sensor_priority NULLS LAST, t.timeseries_id"
    ),
    params = list(location_id, parameter_name, exclude_timeseries_id)
  )
}

qc_rule_assignments_normalize <- function(con, timeseries_id, assignments) {
  tsid <- qc_rule_integer_id(timeseries_id, "timeseries_id")
  series <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT t.location_id, p.param_name",
      "FROM continuous.timeseries t",
      "JOIN public.parameters p USING (parameter_id)",
      "WHERE t.timeseries_id = $1"
    ),
    params = list(tsid)
  )
  if (nrow(series) != 1L) stop("No visible continuous timeseries ", tsid, ".")

  if (is.null(assignments)) assignments <- data.frame()
  if (!is.data.frame(assignments)) {
    stop("assignments must be a data.frame.")
  }
  if (nrow(assignments) == 0L) {
    return(data.frame(
      timeseries_qc_rule_id = integer(),
      rule_fx = character(),
      rule_version = integer(),
      rule_args = character(),
      companion_timeseries = character(),
      run_order = integer(),
      active = logical(),
      note = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (!"rule_fx" %in% names(assignments)) {
    stop("Every QC assignment requires rule_fx.")
  }

  capabilities <- as.data.frame(
    getQCRuleCapabilities(con, enabled_only = FALSE),
    stringsAsFactors = FALSE
  )
  assignments <- as.data.frame(assignments, stringsAsFactors = FALSE)
  n <- nrow(assignments)
  if (!"timeseries_qc_rule_id" %in% names(assignments)) {
    assignments$timeseries_qc_rule_id <- rep(NA_integer_, n)
  }
  if (!"rule_version" %in% names(assignments)) {
    assignments$rule_version <- rep(NA_integer_, n)
  }
  if (!"rule_args" %in% names(assignments)) {
    assignments$rule_args <- rep(list(NULL), n)
  }
  if (!"companion_timeseries" %in% names(assignments)) {
    assignments$companion_timeseries <- rep(list(NULL), n)
  }
  if (!"run_order" %in% names(assignments)) {
    assignments$run_order <- seq_len(n)
  }
  if (!"active" %in% names(assignments)) {
    assignments$active <- rep(TRUE, n)
  }
  if (!"note" %in% names(assignments)) {
    assignments$note <- rep(NA_character_, n)
  }

  existing_assignments <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_qc_rule_id, rule_fx, rule_version
     FROM continuous.timeseries_qc_rules
     WHERE timeseries_id = $1",
    params = list(tsid)
  )

  row_value <- function(column, i) {
    value <- assignments[[column]]
    value[[i]]
  }
  ids_numeric <- suppressWarnings(as.numeric(as.character(
    assignments$timeseries_qc_rule_id
  )))
  if (
    length(ids_numeric) != n ||
      any(!is.na(ids_numeric) &
        (!is.finite(ids_numeric) | ids_numeric < 1 |
          ids_numeric > .Machine$integer.max |
          ids_numeric != floor(ids_numeric)))
  ) {
    stop("Invalid timeseries_qc_rule_id values.")
  }
  ids <- as.integer(ids_numeric)
  if (any(ids[!is.na(ids)] < 1L) || anyDuplicated(ids[!is.na(ids)])) {
    stop("QC assignment ids must be positive and unique.")
  }

  fx <- trimws(as.character(assignments$rule_fx))
  if (anyNA(fx) || any(!nzchar(fx))) stop("Every QC assignment needs rule_fx.")
  version_numeric <- suppressWarnings(as.numeric(as.character(
    assignments$rule_version
  )))
  if (
    length(version_numeric) != n ||
      any(!is.na(version_numeric) &
        (!is.finite(version_numeric) | version_numeric < 1 |
          version_numeric > .Machine$integer.max |
          version_numeric != floor(version_numeric)))
  ) {
    stop("Invalid rule_version values.")
  }
  version <- as.integer(version_numeric)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    capability_rows <- capabilities[capabilities$rule_fx == fx[[i]], , drop = FALSE]
    if (is.na(version[[i]])) {
      existing_row <- match(
        ids[[i]],
        existing_assignments$timeseries_qc_rule_id
      )
      retain_existing_version <- !is.na(ids[[i]]) &&
        !is.na(existing_row) &&
        identical(existing_assignments$rule_fx[[existing_row]], fx[[i]])
      if (retain_existing_version) {
        version[[i]] <- existing_assignments$rule_version[[existing_row]]
      } else {
        enabled_rows <- capability_rows[capability_rows$enabled, , drop = FALSE]
        if (nrow(enabled_rows) == 0L) {
          stop("No enabled QC rule definition is available for ", fx[[i]], ".")
        }
        version[[i]] <- max(enabled_rows$rule_version)
      }
    }
    capability <- capability_rows[capability_rows$rule_version == version[[i]], , drop = FALSE]
    if (nrow(capability) != 1L) {
      stop("No QC rule definition ", fx[[i]], "@", version[[i]], ".")
    }
    schema <- capability$argument_schema[[1L]]
    primary_parameters <- capability$primary_parameters[[1L]]
    if (
      length(primary_parameters) &&
        !series$param_name[[1L]] %in% primary_parameters
    ) {
      stop(
        "QC rule '", fx[[i]], "@", version[[i]],
        "' is not compatible with primary parameter '",
        series$param_name[[1L]], "'."
      )
    }
    validateQCRuleArgumentSchema(schema)
    args <- qc_rule_effective_args(schema, row_value("rule_args", i))
    companion_overrides <- qc_rule_companion_overrides(
      row_value("companion_timeseries", i),
      schema,
      args
    )
    active_companions <- qc_rule_active_companions(schema, args)

    for (role in names(companion_overrides)) {
      role_index <- match(
        role,
        vapply(active_companions, function(companion) companion$role, character(1))
      )
      descriptor <- active_companions[[role_index]]
      candidate <- getQCRuleCompanionCandidates(
        con,
        series$location_id[[1L]],
        descriptor$parameter_name,
        exclude_timeseries_id = tsid
      )
      if (!companion_overrides[[role]] %in% candidate$timeseries_id) {
        stop(
          "QC companion '", role,
          "' must be an active timeseries at the same location with parameter '",
          descriptor$parameter_name, "'."
        )
      }
    }

    order_value <- suppressWarnings(as.numeric(as.character(
      assignments$run_order[[i]]
    )))
    if (
      length(order_value) != 1L ||
        is.na(order_value) ||
        !is.finite(order_value) ||
        order_value < 1 ||
        order_value > 32767 ||
        order_value != floor(order_value)
    ) {
      stop("run_order must be a positive integer no greater than 32767.")
    }
    is_active <- assignments$active[[i]]
    if (!is.logical(is_active) || length(is_active) != 1L || is.na(is_active)) {
      stop("active must be TRUE or FALSE for every QC assignment.")
    }
    if (!isTRUE(capability$enabled[[1L]]) && is_active) {
      retained_disabled_version <- !is.na(ids[[i]]) && any(
        existing_assignments$timeseries_qc_rule_id == ids[[i]] &
          existing_assignments$rule_fx == fx[[i]] &
          existing_assignments$rule_version == version[[i]]
      )
      if (!retained_disabled_version) {
        stop(
          "Disabled QC rule versions cannot be added or reactivated; an",
          " existing pinned assignment can remain active."
        )
      }
    }
    note <- assignments$note[[i]]
    if (length(note) != 1L) stop("note must be one character value or NA.")
    if (!is.na(note) && !is.character(note)) stop("note must be character or NA.")

    out[[i]] <- data.frame(
      timeseries_qc_rule_id = ids[[i]],
      rule_fx = fx[[i]],
      rule_version = version[[i]],
      rule_args = as.character(jsonlite::toJSON(
        args, auto_unbox = TRUE, null = "null", digits = NA
      )),
      companion_timeseries = qc_rule_json_object(companion_overrides),
      run_order = as.integer(order_value),
      active = is_active,
      note = as.character(note),
      stringsAsFactors = FALSE
    )
  }
  normalized <- do.call(rbind, out)
  active_rows <- normalized[normalized$active, , drop = FALSE]
  if (anyDuplicated(active_rows$run_order)) {
    stop("Active QC assignments cannot repeat run_order.")
  }

  existing_ids <- existing_assignments$timeseries_qc_rule_id
  if (any(!ids[!is.na(ids)] %in% existing_ids)) {
    stop("A QC assignment id does not belong to timeseries ", tsid, ".")
  }
  normalized$timeseries_id <- tsid
  normalized
}

#' Save the complete QC assignment set for one continuous timeseries
#'
#' The supplied data.frame is the desired assignment set. Omitted existing
#' rows are deactivated, preserving assignment identity and its audit trail.
#' Effective argument values are materialized from schema defaults before
#' saving.
#'
#' @param con An open AquaCache database connection.
#' @param timeseries_id One continuous timeseries_id.
#' @param assignments Desired assignments with rule_fx, rule_args,
#'   companion_timeseries, run_order, active, and note columns. The optional
#'   rule_version is retained for an existing assignment when omitted; for a
#'   new assignment, the highest enabled version is selected. The optional
#'   \code{timeseries_qc_rule_id} identifies rows to edit; omit it for new rows.
#' @return Assignment ids invisibly.
#' @export
setTimeseriesQCRules <- function(con, timeseries_id, assignments) {
  normalized <- qc_rule_assignments_normalize(
    con,
    timeseries_id,
    assignments
  )
  tsid <- as.integer(timeseries_id)
  active_transaction <- dbTransBegin(con)
  if (!active_transaction) {
    DBI::dbExecute(con, "SAVEPOINT set_timeseries_qc_rules")
  }

  saved_ids <- tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE continuous.timeseries_qc_rules
       SET active = FALSE
       WHERE timeseries_id = $1",
      params = list(tsid)
    )
    ids <- integer(nrow(normalized))
    for (i in seq_len(nrow(normalized))) {
      row <- normalized[i, ]
      id <- row$timeseries_qc_rule_id[[1L]]
      if (is.na(id)) {
        ids[[i]] <- DBI::dbGetQuery(
          con,
          "INSERT INTO continuous.timeseries_qc_rules
             (timeseries_id, rule_fx, rule_version, rule_args,
              companion_timeseries, run_order, active, note)
           VALUES ($1, $2, $3, $4::jsonb, $5::jsonb, $6, FALSE, $7)
           RETURNING timeseries_qc_rule_id",
          params = list(
            tsid,
            row$rule_fx[[1L]],
            row$rule_version[[1L]],
            row$rule_args[[1L]],
            row$companion_timeseries[[1L]],
            row$run_order[[1L]],
            row$note[[1L]]
          )
        )$timeseries_qc_rule_id[[1L]]
      } else {
        updated <- DBI::dbExecute(
          con,
          "UPDATE continuous.timeseries_qc_rules
           SET rule_fx = $2, rule_version = $3, rule_args = $4::jsonb,
               companion_timeseries = $5::jsonb, run_order = $6, note = $7
           WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $8",
          params = list(
            tsid,
            row$rule_fx[[1L]],
            row$rule_version[[1L]],
            row$rule_args[[1L]],
            row$companion_timeseries[[1L]],
            row$run_order[[1L]],
            row$note[[1L]],
            id
          )
        )
        if (updated != 1L) stop("QC assignment update did not affect one row.")
        ids[[i]] <- id
      }
    }
    for (i in seq_len(nrow(normalized))) {
      if (isTRUE(normalized$active[[i]])) {
        DBI::dbExecute(
          con,
          "UPDATE continuous.timeseries_qc_rules
           SET active = TRUE
           WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $2",
          params = list(tsid, ids[[i]])
        )
      }
    }
    ids
  }, error = function(e) {
    if (active_transaction) {
      DBI::dbExecute(con, "ROLLBACK")
    } else {
      DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT set_timeseries_qc_rules")
      DBI::dbExecute(con, "RELEASE SAVEPOINT set_timeseries_qc_rules")
    }
    stop(e)
  })

  if (active_transaction) {
    DBI::dbExecute(con, "COMMIT")
  } else {
    DBI::dbExecute(con, "RELEASE SAVEPOINT set_timeseries_qc_rules")
  }
  invisible(saved_ids)
}
