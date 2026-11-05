#' Describe one QC rule argument
#'
#' Creates a JSON-compatible descriptor for a rule argument. User arguments
#' include the label and control metadata shown by clients such as AquaLink;
#' internal arguments provide implementation values such as an output
#' qualifier. The complete schema, including cross-argument conditions, is
#' validated when the capability is registered.
#'
#' @param name Name used by the QC implementation in its `a` argument list.
#' @param source Whether the argument is editable by a user or managed by the
#'   rule implementation.
#' @param help Non-blank explanation of the argument.
#' @param default Required value used when a new assignment is created.
#' @param label User-facing label. Required when `source = "user"`.
#' @param value_type Storage type for a user argument: `character`, `numeric`,
#'   `integer`, or `logical` (vector types are also supported by the schema).
#' @param control Client control for a user argument, such as `numeric`,
#'   `checkbox`, `select`, or `text`.
#' @param required Whether a user must provide the value.
#' @param choices Optional allowed values for a select control.
#' @param minimum,maximum,step Optional numeric constraints.
#' @param advanced Whether clients should place the control in an advanced
#'   section.
#' @param units Optional unit label for the argument.
#' @param visible_when Optional condition of the form
#'   `list(argument = "other_argument", equals = TRUE)`.
#'
#' @return A validated QC argument descriptor.
#' @seealso [registerQCRuleCapability()]
#' @export
qcRuleArgument <- function(
  name,
  source = c("user", "internal"),
  help,
  default,
  label = NULL,
  value_type = NULL,
  control = NULL,
  required = FALSE,
  choices = NULL,
  minimum = NULL,
  maximum = NULL,
  step = NULL,
  advanced = FALSE,
  units = NULL,
  visible_when = NULL
) {
  source <- match.arg(source)
  argument <- list(
    name = name,
    source = source,
    help = help,
    default = default,
    label = label,
    value_type = value_type,
    control = control,
    required = required,
    choices = choices,
    minimum = minimum,
    maximum = maximum,
    step = step,
    advanced = advanced,
    units = units,
    visible_when = visible_when
  )
  argument <- argument[!vapply(argument, is.null, logical(1))]
  validateSourceAdapterArgumentSchema(
    list(schema_version = 1L, arguments = list(argument)),
    check_function = FALSE
  )
  if (is.null(argument$default)) {
    stop("QC argument '", name, "' requires a non-NULL default.")
  }
  argument
}

#' Describe one QC companion role
#'
#' Creates a descriptor for a companion timeseries used by a QC rule. By
#' default, grading selects the active same-location series with the lowest
#' sensor-priority value. An explicit series can be saved on an assignment;
#' when none is available, the grading function warns and receives `NA` for
#' this role.
#'
#' @param role Name of the companion column supplied to the QC implementation.
#' @param label User-facing label for the companion selector.
#' @param parameter_name Exact parameter name required for the companion.
#' @param help User-facing explanation of how the rule uses the companion.
#' @param when Optional condition, such as
#'   `list(argument = "use_companion", equals = TRUE)`, that activates this
#'   role only for a particular argument value.
#' @param allow_explicit_override Whether an assignment may select a specific
#'   companion timeseries instead of using automatic same-location selection.
#'
#' @return A QC companion descriptor. Conditions are validated against the
#'   complete argument schema when the capability is registered.
#' @seealso [registerQCRuleCapability()]
#' @export
qcRuleCompanion <- function(
  role,
  label,
  parameter_name,
  help,
  when = NULL,
  allow_explicit_override = TRUE
) {
  required_text <- list(
    role = role,
    label = label,
    parameter_name = parameter_name,
    help = help
  )
  invalid_text <- names(required_text)[!vapply(
    required_text,
    function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    },
    logical(1)
  )]
  if (length(invalid_text)) {
    stop(
      "QC companion fields must be non-blank character values: ",
      paste(invalid_text, collapse = ", "),
      "."
    )
  }
  if (
    !is.logical(allow_explicit_override) ||
      length(allow_explicit_override) != 1L ||
      is.na(allow_explicit_override)
  ) {
    stop("allow_explicit_override must be TRUE or FALSE.")
  }

  companion <- list(
    role = role,
    label = label,
    parameter_name = parameter_name,
    selection_strategy = "same_location_lowest_sensor_priority",
    allow_explicit_override = allow_explicit_override,
    missing_behavior = "warn_and_use_na",
    help = help
  )
  if (!is.null(when)) companion$when <- when
  companion
}

#' Register a versioned QC rule capability
#'
#' Inserts one complete rule definition into
#' `public.qc_rule_capabilities`. The matching R implementation must already
#' be present in `qc_rule_registry`. This helper is intended for database
#' patches: it validates the implementation, argument schema, and emitted
#' qualifier codes, performs one parameterized `INSERT`, and does not begin or
#' commit a transaction. The calling patch owns transaction control.
#'
#' A rule version is immutable once inserted. To change the implementation,
#' emitted codes, or argument defaults/schema, add another `rule_version`.
#' Assigning the registered rule to individual timeseries is a separate step.
#' Primary-parameter eligibility remains in the matching R registry entry's
#' `primary_parameters` field; it is not written to the database by this
#' helper.
#'
#' @param con An open AquaCache database connection.
#' @param rule_fx Name used for the rule in the capability table.
#' @param rule_version Positive integer matching the implementation key
#'   `rule_fx@rule_version` in `qc_rule_registry`.
#' @param description Short explanation of the grading rule.
#' @param emits Non-empty character vector of qualifier codes the rule may
#'   return. Every code must already exist in `public.qualifier_types`.
#' @param arguments Unnamed list of descriptors created by
#'   [qcRuleArgument()].
#' @param companions Unnamed list of descriptors created by
#'   [qcRuleCompanion()].
#' @param enabled Whether the new capability is available for assignment.
#' @param note Optional administrative note.
#'
#' @return Invisibly returns the validated capability metadata and schema.
#' @seealso [getQCRuleCapabilities()], [setTimeseriesQCRules()]
#' @export
registerQCRuleCapability <- function(
  con,
  rule_fx,
  rule_version = 1L,
  description,
  emits,
  arguments = list(),
  companions = list(),
  enabled = TRUE,
  note = NULL
) {
  if (
    !is.character(rule_fx) ||
      length(rule_fx) != 1L ||
      is.na(rule_fx) ||
      !nzchar(trimws(rule_fx))
  ) {
    stop("rule_fx must be one non-blank rule name.")
  }
  rule_version <- qc_rule_integer_id(rule_version, "rule_version")
  if (
    !is.character(description) ||
      length(description) != 1L ||
      is.na(description) ||
      !nzchar(trimws(description))
  ) {
    stop("description must be one non-blank character value.")
  }
  if (
    !is.character(emits) ||
      !length(emits) ||
      anyNA(emits) ||
      any(!nzchar(trimws(emits))) ||
      anyDuplicated(emits)
  ) {
    stop("emits must contain unique, non-blank qualifier codes.")
  }
  if (!is.list(arguments) || !is.list(companions)) {
    stop("arguments and companions must be lists of QC descriptors.")
  }
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    stop("enabled must be TRUE or FALSE.")
  }
  if (
    !is.null(note) &&
      (!is.character(note) || length(note) != 1L)
  ) {
    stop("note must be NULL or one character value.")
  }
  if (is.null(note)) note <- NA_character_

  key <- qc_rule_capability_key(rule_fx, rule_version)
  implementation <- qc_rule_registry[[key]]
  if (
    is.null(implementation) ||
      !is.function(implementation$fx) ||
      !is.function(implementation$window)
  ) {
    stop(
      "No complete R implementation is registered for QC rule ",
      key,
      "."
    )
  }

  schema <- list(
    schema_version = 2L,
    arguments = arguments,
    companions = companions
  )
  schema <- validateQCRuleArgumentSchema(schema)
  missing_codes <- setdiff(
    emits,
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT qualifier_type_code",
        "FROM public.qualifier_types",
        "WHERE qualifier_type_code = ANY($1::text[])"
      ),
      params = list(qc_pg_text_array(emits))
    )$qualifier_type_code
  )
  if (length(missing_codes)) {
    stop(
      "Qualifier code(s) must exist before registering this rule: ",
      paste(missing_codes, collapse = ", "),
      "."
    )
  }

  inserted <- DBI::dbExecute(
    con,
    "INSERT INTO public.qc_rule_capabilities
       (rule_fx, rule_version, description, emits, argument_schema,
        enabled, note)
     VALUES ($1, $2, $3, $4::text[], $5::jsonb, $6, $7)",
    params = list(
      rule_fx,
      rule_version,
      description,
      qc_pg_text_array(emits),
      as.character(jsonlite::toJSON(
        schema,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      )),
      enabled,
      note
    )
  )
  if (!identical(inserted, 1L)) {
    stop("Expected to insert one QC rule capability row; inserted ", inserted, ".")
  }

  invisible(list(
    rule_fx = rule_fx,
    rule_version = rule_version,
    description = description,
    emits = emits,
    argument_schema = schema,
    enabled = enabled,
    note = note
  ))
}
