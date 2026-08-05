#' Get source-adapter capabilities
#'
#' Reads the provider-neutral adapter registry created by database Patch 56.
#' The registry is shared by AquaCache import workflows and clients such as
#' YGwater, so applicable download functions and their behavior are not inferred
#' from function names. Enabled rows are the authoritative list of source
#' functions that may be assigned within each data domain.
#'
#' @param con An open AquaCache database connection. When `NULL`, a connection
#'   is opened with [AquaConnect()] and closed before returning.
#' @param source_fx Optional character vector of source-function names to
#'   return.
#' @param data_domain Optional character vector containing one or more of
#'   `"continuous"`, `"discrete"`, `"image"`, or `"raster"`.
#' @param enabled_only If `TRUE`, return only enabled registry entries.
#'
#' @return A `data.table` with one row per registered adapter and data domain.
#'   `parallel_group_args`, `transmission_method_codes`, `argument_schema`, and
#'   `ui_config` are list columns.
#' @export
getSourceAdapterCapabilities <- function(
  con = NULL,
  source_fx = NULL,
  data_domain = NULL,
  enabled_only = TRUE
) {
  if (
    !is.logical(enabled_only) ||
      length(enabled_only) != 1L ||
      is.na(enabled_only)
  ) {
    stop("getSourceAdapterCapabilities: enabled_only must be TRUE or FALSE.")
  }

  if (!is.null(source_fx)) {
    source_fx <- unique(trimws(as.character(source_fx)))
    if (anyNA(source_fx) || any(!nzchar(source_fx))) {
      stop(
        "getSourceAdapterCapabilities: source_fx must contain non-blank names."
      )
    }
  }

  valid_domains <- c("continuous", "discrete", "image", "raster")
  if (!is.null(data_domain)) {
    data_domain <- unique(trimws(as.character(data_domain)))
    if (
      anyNA(data_domain) ||
        any(!nzchar(data_domain)) ||
        any(!data_domain %in% valid_domains)
    ) {
      stop(
        "getSourceAdapterCapabilities: data_domain must contain only ",
        paste(valid_domains, collapse = ", "),
        "."
      )
    }
  }

  disconnect <- FALSE
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    disconnect <- TRUE
  }
  if (disconnect) {
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  capabilities <- tryCatch(
    data.table::as.data.table(DBI::dbGetQuery(
      con,
      "SELECT
         source_fx,
         data_domain,
         adapter_kind,
         requires_transmission_mapping,
         inject_timeseries_id,
         parallel_group_strategy,
         array_to_json(parallel_group_args)::text
           AS parallel_group_args_json,
         allow_empty_initial_fetch,
         array_to_json(transmission_method_codes)::text
           AS transmission_method_codes_json,
         argument_schema::text AS argument_schema_json,
         ui_config::text AS ui_config_json,
         enabled,
         note
       FROM public.source_adapter_capabilities
       ORDER BY data_domain, source_fx"
    )),
    error = function(e) {
      stop(
        "getSourceAdapterCapabilities requires database Patch 56: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  capabilities[["parallel_group_args"]] <- lapply(
    capabilities[["parallel_group_args_json"]],
    jsonlite::fromJSON
  )
  capabilities[["parallel_group_args_json"]] <- NULL
  capabilities[["transmission_method_codes"]] <- lapply(
    capabilities[["transmission_method_codes_json"]],
    jsonlite::fromJSON
  )
  capabilities[["transmission_method_codes_json"]] <- NULL
  capabilities[["argument_schema"]] <- Map(
    function(schema_json, adapter_name) {
      schema <- jsonlite::fromJSON(schema_json, simplifyVector = FALSE)
      validateSourceAdapterArgumentSchema(
        schema,
        source_fx = adapter_name,
        check_function = TRUE
      )
      schema
    },
    capabilities[["argument_schema_json"]],
    capabilities[["source_fx"]]
  )
  capabilities[["argument_schema_json"]] <- NULL
  capabilities[["ui_config"]] <- lapply(
    capabilities[["ui_config_json"]],
    function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)
  )
  capabilities[["ui_config_json"]] <- NULL

  if (enabled_only) {
    capabilities <- capabilities[which(capabilities$enabled)]
  }
  if (!is.null(source_fx)) {
    keep <- capabilities[["source_fx"]] %in% source_fx
    capabilities <- capabilities[which(keep)]
  }
  if (!is.null(data_domain)) {
    keep <- capabilities[["data_domain"]] %in% data_domain
    capabilities <- capabilities[which(keep)]
  }

  capabilities[]
}

#' Describe one source-adapter argument
#'
#' Creates and validates one argument descriptor for the versioned schema stored
#' in `public.source_adapter_capabilities.argument_schema`. Package developers
#' can use these descriptors in database patches when registering a new source
#' adapter or changing the documented arguments of an existing adapter.
#'
#' Every argument is classified by who supplies it. `"user"` arguments are
#' exposed as controls in clients such as YGwater. `"runtime"`,
#' `"environment"`, and `"internal"` arguments are displayed as read-only
#' documentation explaining how the adapter receives them.
#'
#' @param name Name of an argument in the registered R function signature.
#' @param source One of `"user"`, `"runtime"`, `"environment"`, or
#'   `"internal"`.
#' @param help Non-blank user-facing explanation of the argument and, for
#'   managed arguments, how it is supplied.
#' @param label User-facing control label. Required when `source = "user"`.
#' @param value_type Storage type for a user argument: `"character"`,
#'   `"numeric"`, `"integer"`, `"logical"`, `"character_vector"`, or
#'   `"numeric_vector"`.
#' @param control Client control for a user argument: `"text"`, `"password"`,
#'   `"numeric"`, `"checkbox"`, `"select"`, or `"multiselect"`.
#' @param required Whether a user must supply the argument.
#' @param default Optional default value shown by clients.
#' @param choices Optional allowed values, required for select controls.
#' @param minimum,maximum,step Optional numeric control constraints.
#' @param advanced Whether clients should place the control in an advanced
#'   section.
#'
#' @return A validated JSON-compatible argument descriptor.
sourceAdapterArgument <- function(
  name,
  source = c("user", "runtime", "environment", "internal"),
  help,
  label = NULL,
  value_type = NULL,
  control = NULL,
  required = FALSE,
  default = NULL,
  choices = NULL,
  minimum = NULL,
  maximum = NULL,
  step = NULL,
  advanced = FALSE
) {
  source <- match.arg(source)
  argument <- list(
    name = name,
    source = source,
    help = help,
    label = label,
    value_type = value_type,
    control = control,
    required = required,
    default = default,
    choices = choices,
    minimum = minimum,
    maximum = maximum,
    step = step,
    advanced = advanced
  )
  argument <- argument[!vapply(argument, is.null, logical(1))]
  validateSourceAdapterArgumentSchema(
    list(schema_version = 1L, arguments = list(argument)),
    check_function = FALSE
  )
  argument
}

#' Register source-adapter argument documentation
#'
#' Validates and writes the argument catalogue for one existing row in
#' `public.source_adapter_capabilities`. This is primarily an authoring helper
#' for AquaCache database patches. It performs one parameterized `UPDATE` and
#' deliberately does not start or commit a transaction, allowing the calling
#' patch to control the complete migration transaction.
#'
#' @param con An open AquaCache database connection.
#' @param source_fx Registered source-function name.
#' @param data_domain One of `"continuous"`, `"discrete"`, `"image"`, or
#'   `"raster"`.
#' @param arguments A list of descriptors created with
#'   [sourceAdapterArgument()].
#' @param schema_version Argument-schema version. Only version 1 is currently
#'   supported.
#' @param check_function Whether to verify that every documented argument is a
#'   formal argument of `source_fx` in AquaCache.
#'
#' @return The validated schema invisibly.
#' @export
registerSourceAdapterArguments <- function(
  con,
  source_fx,
  data_domain,
  arguments,
  schema_version = 1L,
  check_function = TRUE
) {
  if (
    !is.character(source_fx) ||
      length(source_fx) != 1L ||
      is.na(source_fx) ||
      !nzchar(trimws(source_fx))
  ) {
    stop("source_fx must be one non-blank function name.")
  }
  data_domain <- match.arg(
    data_domain,
    c("continuous", "discrete", "image", "raster")
  )
  if (!is.list(arguments)) {
    stop("arguments must be a list of source-adapter argument descriptors.")
  }

  schema <- list(
    schema_version = as.integer(schema_version),
    arguments = arguments
  )
  validateSourceAdapterArgumentSchema(
    schema,
    source_fx = source_fx,
    check_function = check_function
  )

  updated <- DBI::dbExecute(
    con,
    "UPDATE public.source_adapter_capabilities
     SET argument_schema = $1::jsonb
     WHERE source_fx = $2
       AND data_domain = $3",
    params = list(
      as.character(jsonlite::toJSON(
        schema,
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      )),
      source_fx,
      data_domain
    )
  )
  if (!identical(updated, 1L)) {
    stop(
      "Expected one source-adapter registry row for ",
      source_fx,
      " in domain ",
      data_domain,
      "; updated ",
      updated,
      "."
    )
  }

  invisible(schema)
}

#' Validate a source-adapter argument schema
#'
#' Validates the versioned JSON-compatible argument catalogue stored in
#' `public.source_adapter_capabilities.argument_schema`. The schema classifies
#' adapter arguments as user-configurable, runtime-supplied,
#' environment-supplied, or internal. User arguments additionally describe the
#' value type and Shiny control used by clients such as YGwater.
#'
#' @param argument_schema A decoded schema list or a JSON string.
#' @param source_fx Optional registered AquaCache source-function name. When
#'   supplied and `check_function` is `TRUE`, catalogued argument names are
#'   checked against the function formals.
#' @param check_function Whether to check argument names against the AquaCache
#'   function signature when `source_fx` is supplied.
#'
#' @return The validated decoded schema, invisibly.
#' @noRd
#' @keywords internal
validateSourceAdapterArgumentSchema <- function(
  argument_schema,
  source_fx = NULL,
  check_function = !is.null(source_fx)
) {
  if (inherits(argument_schema, "character")) {
    if (length(argument_schema) != 1L || is.na(argument_schema)) {
      stop("argument_schema must be one JSON string or a decoded list.")
    }
    argument_schema <- tryCatch(
      jsonlite::fromJSON(argument_schema, simplifyVector = FALSE),
      error = function(e) {
        stop("Invalid argument_schema JSON: ", conditionMessage(e))
      }
    )
  }
  if (!is.list(argument_schema) || is.null(names(argument_schema))) {
    stop("argument_schema must be a named JSON object.")
  }
  if (
    is.null(argument_schema$schema_version) ||
      length(argument_schema$schema_version) != 1L ||
      !identical(as.integer(argument_schema$schema_version), 1L)
  ) {
    stop("argument_schema.schema_version must be 1.")
  }

  arguments <- argument_schema$arguments
  if (is.null(arguments)) {
    arguments <- list()
  }
  if (!is.list(arguments)) {
    stop("argument_schema.arguments must be a JSON array.")
  }

  valid_sources <- c("user", "runtime", "environment", "internal")
  valid_types <- c(
    "character",
    "numeric",
    "integer",
    "logical",
    "character_vector",
    "numeric_vector"
  )
  valid_controls <- c(
    "text",
    "password",
    "numeric",
    "checkbox",
    "select",
    "multiselect"
  )
  type_controls <- list(
    character = c("text", "password", "select"),
    numeric = c("numeric", "select"),
    integer = c("numeric", "select"),
    logical = "checkbox",
    character_vector = c("text", "multiselect"),
    numeric_vector = c("text", "multiselect")
  )

  argument_names <- character(length(arguments))
  for (i in seq_along(arguments)) {
    argument <- arguments[[i]]
    if (!is.list(argument) || is.null(names(argument))) {
      stop("Every argument_schema.arguments entry must be an object.")
    }
    name <- argument$name
    source <- argument$source
    if (
      is.null(name) ||
        length(name) != 1L ||
        is.na(name) ||
        !grepl("^[A-Za-z][A-Za-z0-9._]*$", name)
    ) {
      stop("Every catalogued argument requires a valid R argument name.")
    }
    if (is.null(source) || length(source) != 1L || !source %in% valid_sources) {
      stop("Argument '", name, "' has an invalid source classification.")
    }
    argument_names[[i]] <- name

    if (
      is.null(argument$help) ||
        length(argument$help) != 1L ||
        is.na(argument$help) ||
        !nzchar(trimws(argument$help))
    ) {
      stop("Argument '", name, "' requires non-blank help text.")
    }

    if (identical(source, "user")) {
      value_type <- argument$value_type
      control <- argument$control
      required <- argument$required
      if (
        is.null(argument$label) ||
          length(argument$label) != 1L ||
          is.na(argument$label) ||
          !nzchar(trimws(argument$label))
      ) {
        stop("User argument '", name, "' requires a non-blank label.")
      }
      if (
        is.null(value_type) ||
          length(value_type) != 1L ||
          !value_type %in% valid_types
      ) {
        stop("User argument '", name, "' has an invalid value_type.")
      }
      if (
        is.null(control) ||
          length(control) != 1L ||
          !control %in% valid_controls ||
          !control %in% type_controls[[value_type]]
      ) {
        stop(
          "User argument '",
          name,
          "' has a control incompatible with its value_type."
        )
      }
      if (
        is.null(required) ||
          !is.logical(required) ||
          length(required) != 1L ||
          is.na(required)
      ) {
        stop("User argument '", name, "' requires a logical required flag.")
      }
      if (
        control %in%
          c("select", "multiselect") &&
          (is.null(argument$choices) || !length(argument$choices))
      ) {
        stop("Select argument '", name, "' requires choices.")
      }
      for (numeric_field in c("minimum", "maximum", "step")) {
        value <- argument[[numeric_field]]
        if (!is.null(value) && (!is.numeric(value) || length(value) != 1L)) {
          stop(
            "Argument '",
            name,
            "' field '",
            numeric_field,
            "' must be one number."
          )
        }
      }
    }
  }

  if (anyDuplicated(argument_names)) {
    stop(
      "argument_schema contains duplicate argument names: ",
      paste(
        unique(argument_names[duplicated(argument_names)]),
        collapse = ", "
      ),
      "."
    )
  }

  if (isTRUE(check_function)) {
    if (
      !inherits(source_fx, "character") ||
        length(source_fx) != 1L ||
        is.na(source_fx) ||
        !nzchar(source_fx)
    ) {
      stop("source_fx must be one non-blank function name.")
    }
    if (!source_fx %in% ls(getNamespace("AquaCache"))) {
      stop("Catalogued source function '", source_fx, "' does not exist.")
    }
    formal_names <- names(formals(get(source_fx, asNamespace("AquaCache"))))
    unknown_names <- setdiff(argument_names, formal_names)
    if (length(unknown_names)) {
      stop(
        "argument_schema for ",
        source_fx,
        " contains names absent from the function signature: ",
        paste(unknown_names, collapse = ", "),
        "."
      )
    }
  }

  invisible(argument_schema)
}

#' Decode stored source-adapter arguments
#'
#' Converts the JSON object stored in a source-adapter assignment's
#' `source_fx_args` column to the named list passed to its registered function.
#'
#' @param source_fx_args A JSON object, or a missing/empty value.
#'
#' @return A named list. Missing and empty values return an empty list.
#' @noRd
#' @keywords internal
source_adapter_args_decode <- function(source_fx_args) {
  if (
    is.null(source_fx_args) ||
      length(source_fx_args) == 0L ||
      is.na(source_fx_args) ||
      !nzchar(source_fx_args)
  ) {
    return(list())
  }
  args <- jsonlite::fromJSON(source_fx_args, simplifyVector = TRUE)
  if (!is.list(args) || is.null(names(args))) {
    stop("source_fx_args must be a JSON object with named arguments.")
  }
  args
}

source_adapter_assignments_normalize <- function(
  assignments,
  con,
  data_domain
) {
  valid_domains <- c("continuous", "discrete", "image", "raster")
  if (!data_domain %in% valid_domains) {
    stop("Unsupported source-adapter data domain: ", data_domain, ".")
  }
  supports_synchronize <- data_domain %in% c("continuous", "discrete")
  if (is.null(assignments) || length(assignments) == 0L) {
    return(data.frame(
      source_fx = character(),
      source_fx_args = character(),
      fetch_priority = integer(),
      synchronize_priority = integer(),
      active = logical(),
      note = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (!inherits(assignments, "data.frame")) {
    stop("Source-adapter assignments must be supplied as a data.frame.")
  }
  if (!"source_fx" %in% names(assignments)) {
    stop("Source-adapter assignments require a source_fx column.")
  }

  assignments <- as.data.frame(assignments, stringsAsFactors = FALSE)
  defaults <- list(
    source_fx_args = rep(NA_character_, nrow(assignments)),
    fetch_priority = rep(NA_integer_, nrow(assignments)),
    synchronize_priority = rep(NA_integer_, nrow(assignments)),
    active = rep(TRUE, nrow(assignments)),
    note = rep(NA_character_, nrow(assignments))
  )
  for (name in names(defaults)) {
    if (!name %in% names(assignments)) {
      assignments[[name]] <- defaults[[name]]
    }
  }

  assignments$source_fx <- trimws(as.character(assignments$source_fx))
  if (anyNA(assignments$source_fx) || any(!nzchar(assignments$source_fx))) {
    stop("Every source-adapter assignment requires a non-blank source_fx.")
  }

  encode_args <- function(value) {
    if (is.null(value) || length(value) == 0L || all(is.na(value))) {
      return(NA_character_)
    }
    if (is.list(value) && !is.data.frame(value)) {
      if (is.null(names(value)) || any(!nzchar(names(value)))) {
        stop("List-valued source_fx_args must be a named list.")
      }
      return(as.character(jsonlite::toJSON(value, auto_unbox = TRUE)))
    }
    value <- as.character(value[[1L]])
    if (is.na(value) || !nzchar(trimws(value))) {
      return(NA_character_)
    }
    decoded <- jsonlite::fromJSON(value, simplifyVector = FALSE)
    if (!is.list(decoded) || is.null(names(decoded))) {
      stop("source_fx_args must be a JSON object or a named list.")
    }
    value
  }
  raw_args <- assignments$source_fx_args
  assignments$source_fx_args <- vapply(
    seq_len(nrow(assignments)),
    function(i) {
      value <- if (is.list(raw_args)) raw_args[[i]] else raw_args[i]
      encode_args(value)
    },
    character(1)
  )

  normalize_priority <- function(value, name) {
    numeric_value <- suppressWarnings(as.numeric(value))
    bad <- !is.na(numeric_value) &
      (numeric_value <= 0 | numeric_value != floor(numeric_value))
    if (any(bad)) {
      stop(name, " must contain positive whole numbers or NA.")
    }
    as.integer(numeric_value)
  }
  assignments$fetch_priority <- normalize_priority(
    assignments$fetch_priority,
    "fetch_priority"
  )
  assignments$synchronize_priority <- normalize_priority(
    assignments$synchronize_priority,
    "synchronize_priority"
  )
  if (!supports_synchronize && any(!is.na(assignments$synchronize_priority))) {
    stop(
      data_domain,
      " source assignments do not support synchronization priorities."
    )
  }
  if (!supports_synchronize && any(is.na(assignments$fetch_priority))) {
    stop("Every ", data_domain, " source assignment needs a fetch_priority.")
  }
  if (
    supports_synchronize &&
      any(
        is.na(assignments$fetch_priority) &
          is.na(assignments$synchronize_priority)
      )
  ) {
    stop(
      "Every source-adapter assignment needs a fetch_priority, a ",
      "synchronize_priority, or both."
    )
  }
  if (
    !is.logical(assignments$active) ||
      anyNA(assignments$active)
  ) {
    stop("active must contain TRUE or FALSE for every source assignment.")
  }

  active_rows <- assignments[assignments$active, , drop = FALSE]
  priority_names <- if (supports_synchronize) {
    c("fetch_priority", "synchronize_priority")
  } else {
    "fetch_priority"
  }
  for (priority_name in priority_names) {
    priority <- active_rows[[priority_name]]
    priority <- priority[!is.na(priority)]
    if (anyDuplicated(priority)) {
      stop(
        "Active source-adapter assignments cannot repeat ",
        priority_name,
        "."
      )
    }
  }

  registered <- getSourceAdapterCapabilities(
    con = con,
    data_domain = data_domain
  )$source_fx
  missing <- setdiff(unique(assignments$source_fx), registered)
  if (length(missing) > 0L) {
    stop(
      "Missing or disabled ",
      data_domain,
      " source-adapter capabilities: ",
      paste(missing, collapse = ", "),
      "."
    )
  }

  assignments[,
    c(
      "source_fx",
      "source_fx_args",
      "fetch_priority",
      "synchronize_priority",
      "active",
      "note"
    ),
    drop = FALSE
  ]
}

source_adapter_assignments_insert <- function(
  con,
  data_domain,
  series_id,
  assignments
) {
  assignments <- source_adapter_assignments_normalize(
    assignments = assignments,
    con = con,
    data_domain = data_domain
  )
  if (nrow(assignments) == 0L) {
    return(invisible(integer()))
  }

  table_details <- switch(
    data_domain,
    continuous = list(
      table = "continuous.timeseries_source_adapters",
      id_column = "timeseries_id",
      return_column = "timeseries_source_adapter_id",
      supports_synchronize = TRUE
    ),
    discrete = list(
      table = "discrete.sample_series_source_adapters",
      id_column = "sample_series_id",
      return_column = "sample_series_source_adapter_id",
      supports_synchronize = TRUE
    ),
    image = list(
      table = "files.image_series_source_adapters",
      id_column = "img_series_id",
      return_column = "image_series_source_adapter_id",
      supports_synchronize = FALSE
    ),
    raster = list(
      table = "spatial.raster_series_source_adapters",
      id_column = "raster_series_id",
      return_column = "raster_series_source_adapter_id",
      supports_synchronize = FALSE
    ),
    stop("Source assignments are not implemented for domain: ", data_domain)
  )

  inserted <- integer(nrow(assignments))
  for (i in seq_len(nrow(assignments))) {
    sql <- if (isTRUE(table_details$supports_synchronize)) {
      sprintf(
        "INSERT INTO %s (
           %s, source_fx, source_fx_args, fetch_priority,
           synchronize_priority, active, note
         ) VALUES ($1, $2, $3::jsonb, $4, $5, $6, $7)
         RETURNING %s",
        table_details$table,
        table_details$id_column,
        table_details$return_column
      )
    } else {
      sprintf(
        "INSERT INTO %s (
           %s, source_fx, source_fx_args, fetch_priority, active, note
         ) VALUES ($1, $2, $3::jsonb, $4, $5, $6)
         RETURNING %s",
        table_details$table,
        table_details$id_column,
        table_details$return_column
      )
    }
    params <- list(
      as.integer(series_id),
      assignments$source_fx[[i]],
      assignments$source_fx_args[[i]],
      assignments$fetch_priority[[i]]
    )
    if (isTRUE(table_details$supports_synchronize)) {
      params <- c(params, list(assignments$synchronize_priority[[i]]))
    }
    params <- c(
      params,
      list(
        assignments$active[[i]],
        assignments$note[[i]]
      )
    )
    inserted[[i]] <- DBI::dbGetQuery(
      con,
      sql,
      params = params
    )[[1L, 1L]]
  }
  invisible(inserted)
}
