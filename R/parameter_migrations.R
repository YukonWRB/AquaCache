#' @title Revise a parameter catalogue entry
#' @description
#' Rename a parameter, merge it into another catalogue entry, update selected
#' metadata, and carry parameter-level references across the database. Analytical
#' metadata is preserved unless explicit replacements are supplied.
#'
#' @param con An active AquaCache database connection and transaction.
#' @param source_name Current parameter name to revise.
#' @param target_name Canonical name after revision. Defaults to `source_name`.
#' @param source_description Optional expected description used to ensure the
#'   intended source row was selected.
#' @param param_name_fr French parameter name to set, if supplied.
#' @param description English parameter description to set, if supplied.
#' @param description_fr French parameter description to set, if supplied.
#' @param plot_default_y_orientation Plot orientation to set, if supplied.
#' @param sample_fraction Whether results require a sample fraction value.
#'   `NULL` preserves the existing flag, `TRUE` requires a value, and `FALSE`
#'   makes it optional. Refers to public.parameters.sample_fraction.
#' @param mapping_fraction Optional label in `discrete.sample_fractions` to
#'   apply to result rows and import mappings moved from the source.
#' @param result_descriptor Legacy staging requirement flag. `NULL` preserves
#'   it, `TRUE` requires a value, and `FALSE` makes it optional.
#' @param mapping_descriptor Optional label in `discrete.result_descriptors` to
#'   apply to result rows and import mappings moved from the source.
#' @param mapping_descriptor_by Optional named character vector of labels to
#'   select for mappings by a JSON key in `source_match` (for example, named by
#'   input unit). This overrides the scalar label for import mappings.
#' @param mapping_descriptor_field JSON key in `source_match` used with
#'   `mapping_descriptor_by`. Defaults to `"input_unit"`.
#' @param preserve_existing_descriptor If `TRUE`, retain populated descriptor
#'   values and fill only missing values with `mapping_descriptor`.
#' @param reporting_basis,result_speciation,result_target,endpoint Optional logical
#'   requirement flags for the revised analytical schema. NULL preserves a flag.
#' @param mapping_reporting_basis,mapping_result_speciation,mapping_result_target
#'   Optional existing catalogue ID, stable code, or label to apply to moved
#'   references. Catalogue entries are resolved, never created implicitly.
#' @param preserve_existing_chemistry Fill only missing analytical IDs when TRUE.
#'   Defaults to TRUE; use FALSE for an explicit correction of populated IDs.
#' @param delete_source Whether to delete a distinct source parameter after its
#'   references have moved. Defaults to `FALSE`.
#' @return The target `parameter_id`, invisibly.
#' @details
#' Descriptor arguments belong to Patch 65's temporary legacy staging schema,
#' before the reporting-basis/speciation/target migration. For current databases,
#' use the independent analytical arguments or omit them to preserve all existing
#' metadata. Conflicting result identities abort the operation; no result is
#' discarded. Shared advisory suggestions retain the lowest display order.
#' Merges require matching physical-unit definitions; no numeric conversion is
#' performed. The entire revision is protected by a savepoint, and the caller
#' decides whether to commit or roll back its enclosing transaction. References
#' embedded in SQL formulae or JSON payloads are not foreign keys and must be
#' reviewed separately before merging parameters.
#' This is a reviewed catalogue-migration tool, not the normal import-mapping
#' editing workflow. It intentionally rewrites references in published and
#' retired mapping snapshots when the source parameter identity was incorrect.
#' Such history rewrites should be explicit in a database patch and run inside
#' its transaction. Ordinary import mapping changes should use draft revisions.
#' @keywords internal
revise_parameter_catalogue_entry <- function(
  con,
  source_name,
  target_name = source_name,
  source_description = NULL,
  param_name_fr = NULL,
  description = NULL,
  description_fr = NULL,
  plot_default_y_orientation = NULL,
  sample_fraction = NULL,
  mapping_fraction = NULL,
  result_descriptor = NULL,
  mapping_descriptor = NULL,
  mapping_descriptor_by = NULL,
  mapping_descriptor_field = "input_unit",
  preserve_existing_descriptor = FALSE,
  delete_source = FALSE,
  reporting_basis = NULL,
  result_speciation = NULL,
  result_target = NULL,
  endpoint = NULL,
  mapping_reporting_basis = NULL,
  mapping_result_speciation = NULL,
  mapping_result_target = NULL,
  preserve_existing_chemistry = TRUE
) {
  if (!isTRUE(dbTransCheck(con))) {
    stop("Parameter catalogue revisions must run inside an active transaction.")
  }
  if (
    !is.character(source_name) ||
      length(source_name) != 1L ||
      is.na(source_name) ||
      !is.character(target_name) ||
      length(target_name) != 1L ||
      is.na(target_name)
  ) {
    stop(
      "Source and target parameter names must each be one non-missing value."
    )
  }
  if (
    !is.logical(delete_source) ||
      length(delete_source) != 1L ||
      is.na(delete_source)
  ) {
    stop("delete_source must be TRUE or FALSE.")
  }
  if (
    !is.logical(preserve_existing_descriptor) ||
      length(preserve_existing_descriptor) != 1L ||
      is.na(preserve_existing_descriptor)
  ) {
    stop("preserve_existing_descriptor must be TRUE or FALSE.")
  }

  if (
    !is.null(sample_fraction) &&
      (!is.logical(sample_fraction) ||
        length(sample_fraction) != 1L ||
        is.na(sample_fraction))
  ) {
    stop("sample_fraction must be TRUE, FALSE, or NULL.")
  }

  if (
    !is.null(result_descriptor) &&
      (!is.logical(result_descriptor) ||
        length(result_descriptor) != 1L ||
        is.na(result_descriptor))
  ) {
    stop("result_descriptor must be TRUE, FALSE, or NULL.")
  }
  if (!is.null(mapping_descriptor_by)) {
    if (
      !is.character(mapping_descriptor_by) ||
        length(mapping_descriptor_by) == 0L ||
        anyNA(mapping_descriptor_by) ||
        is.null(names(mapping_descriptor_by)) ||
        any(!nzchar(mapping_descriptor_by)) ||
        anyNA(names(mapping_descriptor_by)) ||
        any(!nzchar(names(mapping_descriptor_by))) ||
        anyDuplicated(names(mapping_descriptor_by))
    ) {
      stop(
        "mapping_descriptor_by must be a named character vector with unique, non-empty keys."
      )
    }
    if (
      !is.character(mapping_descriptor_field) ||
        length(mapping_descriptor_field) != 1L ||
        is.na(mapping_descriptor_field) ||
        !nzchar(mapping_descriptor_field)
    ) {
      stop("mapping_descriptor_field must be one non-empty JSON key.")
    }
  }
  for (flag in c("reporting_basis", "result_speciation", "result_target", "endpoint")) {
    value <- get(flag)
    if (!is.null(value) && (!is.logical(value) || length(value) != 1L || is.na(value))) {
      stop(flag, " must be TRUE, FALSE, or NULL.")
    }
  }
  if (!is.logical(preserve_existing_chemistry) || length(preserve_existing_chemistry) != 1L ||
      is.na(preserve_existing_chemistry)) stop("preserve_existing_chemistry must be TRUE or FALSE.")
  scalar_metadata <- list(
    source_description = source_description,
    param_name_fr = param_name_fr,
    description = description,
    description_fr = description_fr,
    plot_default_y_orientation = plot_default_y_orientation
  )
  invalid_metadata <- names(scalar_metadata)[vapply(
    scalar_metadata,
    function(value) !is.null(value) && length(value) != 1L,
    logical(1)
  )]
  if (length(invalid_metadata) > 0L) {
    stop(
      "Each supplied metadata value must be scalar: ",
      paste(invalid_metadata, collapse = ", "),
      "."
    )
  }

  find_parameter <- function(name) {
    DBI::dbGetQuery(
      con,
      "SELECT parameter_id, description
       FROM public.parameters
       WHERE lower(btrim(param_name)) = lower(btrim($1))",
      params = list(name)
    )
  }

  DBI::dbExecute(con, "SAVEPOINT parameter_catalogue_revision")
  tryCatch({
    source <- find_parameter(source_name)
    target <- find_parameter(target_name)
    if (nrow(source) > 1L || nrow(target) > 1L) {
      stop("A parameter name resolved to more than one parameter row.")
    }
    if (nrow(source) == 0L && nrow(target) == 0L) {
      stop("Could not find source or target parameter: ", source_name)
    }
    if (nrow(source) == 1L && !is.null(source_description)) {
      actual_description <- source$description[[1]]
      if (
        is.na(actual_description) ||
          tolower(trimws(actual_description)) !=
            tolower(trimws(source_description))
      ) {
        stop(
          "The source parameter description did not match the expected value for '",
          source_name,
          "'."
        )
      }
    }

    source_id <- if (nrow(source) == 1L) {
      as.integer(source$parameter_id[[1]])
    } else {
      NA_integer_
    }
    target_id <- if (nrow(target) == 1L) {
      as.integer(target$parameter_id[[1]])
    } else {
      source_id
    }

    ensure_fraction_id <- function(name) {
      res <- DBI::dbGetQuery(
        con,
        "SELECT * FROM discrete.sample_fractions WHERE sample_fraction = $1",
        params = list(name)
      )

      if (nrow(res) == 0) {
        stop("The sample_fraction you specified does not exist")
      }
      if (nrow(res) > 1) {
        stop(
          "The sample_fraction you specified matches to more than one database entries."
        )
      }
      return(res$sample_fraction_id)
    }
    fraction_id <- NA_integer_
    if (!is.null(mapping_fraction)) {
      if (
        !is.character(mapping_fraction) ||
          length(mapping_fraction) != 1L ||
          is.na(mapping_fraction)
      ) {
        stop("mapping_fraction must be one non-missing label or NULL.")
      }
      fraction_id <- ensure_fraction_id(mapping_fraction)
    }

    ensure_descriptor_id <- function(name) {
      DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.result_descriptors (result_descriptor)
         VALUES ($1)
         ON CONFLICT (result_descriptor) DO UPDATE
           SET result_descriptor = EXCLUDED.result_descriptor
         RETURNING result_descriptor_id",
        params = list(name)
      )$result_descriptor_id[[1]]
    }
    descriptor_id <- NA_integer_
    if (!is.null(mapping_descriptor)) {
      if (
        !is.character(mapping_descriptor) ||
          length(mapping_descriptor) != 1L ||
          is.na(mapping_descriptor)
      ) {
        stop("mapping_descriptor must be one non-missing label or NULL.")
      }
      descriptor_id <- ensure_descriptor_id(mapping_descriptor)
    }

    mapping_descriptor_ids <- NULL
    if (!is.null(mapping_descriptor_by)) {
      mapping_descriptor_ids <- vapply(
        mapping_descriptor_by,
        ensure_descriptor_id,
        integer(1)
      )
    }

    dimensions <- list(reporting_basis = mapping_reporting_basis,
      result_speciation = mapping_result_speciation, result_target = mapping_result_target)
    dimension_ids <- stats::setNames(rep(NA_integer_, length(dimensions)), names(dimensions))
    for (dimension in names(dimensions)) {
      if (is.null(dimensions[[dimension]])) next
      table <- switch(dimension, reporting_basis = "discrete.reporting_bases",
        result_speciation = "discrete.result_speciations", result_target = "discrete.result_targets")
      value <- dimensions[[dimension]]
      id_column <- paste0(dimension, "_id")
      if (is.numeric(value) || grepl("^[0-9]+$", as.character(value))) {
        value <- suppressWarnings(as.numeric(value))
        if (!is.finite(value) || value < 1 || value != trunc(value) || value > .Machine$integer.max) {
          stop("Supply a valid catalogue ID for ", dimension, ".")
        }
        predicate <- paste0(id_column, " = $1")
      } else {
        predicate <- paste0("lower(", dimension, ") = lower($1) OR lower(", dimension, "_code) = lower($1)")
        value <- trimws(as.character(value))
      }
      found <- DBI::dbGetQuery(con, paste0("SELECT ", id_column, " FROM ", table, " WHERE ", predicate), params = list(value))
      if (nrow(found) != 1L) stop("Catalogue value did not resolve uniquely for ", dimension, ".")
      dimension_ids[[dimension]] <- found[[id_column]][[1]]
    }
    if (!is.na(source_id)) {
      migrate_parameter_references(
        con,
        source_parameter_id = source_id,
        target_parameter_id = target_id,
        result_descriptor_id = descriptor_id,
        sample_fraction_id = fraction_id,
        mapping_descriptor_ids = mapping_descriptor_ids,
        mapping_descriptor_field = mapping_descriptor_field,
        preserve_existing_descriptor = preserve_existing_descriptor,
        delete_source = delete_source && source_id != target_id,
        reporting_basis_id = dimension_ids[["reporting_basis"]],
        result_speciation_id = dimension_ids[["result_speciation"]],
        result_target_id = dimension_ids[["result_target"]],
        preserve_existing_chemistry = preserve_existing_chemistry
      )
    }

    updates <- list(
      param_name = target_name,
      param_name_fr = param_name_fr,
      description = description,
      description_fr = description_fr,
      plot_default_y_orientation = plot_default_y_orientation,
      sample_fraction = sample_fraction,
      result_descriptor = result_descriptor,
      reporting_basis = reporting_basis, result_speciation = result_speciation,
      result_target = result_target, endpoint = endpoint
    )
    updates <- updates[!vapply(updates, is.null, logical(1))]
    if (length(updates) > 0L) {
      columns <- vapply(
        names(updates),
        function(name) as.character(DBI::dbQuoteIdentifier(con, name)),
        character(1)
      )
      assignments <- paste0(columns, " = $", seq_along(updates))
      sql <- paste0(
        "UPDATE public.parameters SET ",
        paste(assignments, collapse = ", "),
        " WHERE parameter_id = $",
        length(updates) + 1L
      )
      DBI::dbExecute(
        con,
        sql,
        params = c(unname(updates), list(target_id))
      )
    }

    DBI::dbExecute(con, "RELEASE SAVEPOINT parameter_catalogue_revision")
    invisible(target_id)
  }, error = function(e) {
    try(DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT parameter_catalogue_revision"), silent = TRUE)
    try(DBI::dbExecute(con, "RELEASE SAVEPOINT parameter_catalogue_revision"), silent = TRUE)
    stop(e)
  })
}

#' @title Move parameter references
#' @description Rewrite every discovered foreign-key reference to a parameter.
#' @param con Active database connection and transaction.
#' @param source_parameter_id Parameter ID whose references will move.
#' @param target_parameter_id Existing parameter ID receiving those references.
#' @param result_descriptor_id Optional descriptor ID to set on referencing rows
#'   that carry a `result_descriptor_id` column. Defaults to `NA` (preserve it).
#' @param sample_fraction_id Optional sample fraction ID to set on referencing rows that carry a `sample_fraction_id` column. Defaults to `NA` (preserve it).
#' @param mapping_descriptor_ids Optional named integer vector of per-mapping
#'   descriptor IDs, selected by a JSON key in `source_match`.
#' @param mapping_descriptor_field JSON key used with
#'   `mapping_descriptor_ids`. Defaults to `"input_unit"`.
#' @param preserve_existing_descriptor If `TRUE`, only fill missing descriptor
#'   values; otherwise replace them with the supplied descriptor ID(s).
#' @param reporting_basis_id,result_speciation_id,result_target_id Optional
#'   analytical catalogue IDs for referencing records that carry those columns.
#'   NA preserves the column. Legacy descriptors and current analytical IDs
#'   cannot be supplied together.
#' @param preserve_existing_chemistry Fill only missing analytical IDs when TRUE.
#' @param delete_source Whether to delete the source parameter after its
#'   references move. Defaults to `FALSE`.
#' @return `TRUE`, invisibly.
#' @details The import mapping row guard is temporarily disabled for this
#'   operation because this helper is reserved for reviewed catalogue
#'   corrections that intentionally rewrite published and retired snapshots.
#'   Ordinary import mapping changes must use draft revisions.
#' @keywords internal
migrate_parameter_references <- function(
  con,
  source_parameter_id,
  target_parameter_id,
  result_descriptor_id = NA_integer_,
  sample_fraction_id = NA_integer_,
  mapping_descriptor_ids = NULL,
  mapping_descriptor_field = "input_unit",
  preserve_existing_descriptor = FALSE,
  delete_source = FALSE,
  reporting_basis_id = NA_integer_,
  result_speciation_id = NA_integer_,
  result_target_id = NA_integer_,
  preserve_existing_chemistry = TRUE
) {
  coerce_id <- function(value, name, allow_na = FALSE) {
    if (length(value) != 1L || is.na(value)) {
      if (allow_na && length(value) == 1L && is.na(value)) {
        return(NA_integer_)
      }
      stop(name, " must be one valid integer.")
    }
    integer_value <- suppressWarnings(as.integer(value))
    numeric_value <- suppressWarnings(as.numeric(value))
    if (
      is.na(integer_value) ||
        is.na(numeric_value) ||
        !is.finite(numeric_value) || integer_value < 1L || integer_value != numeric_value
    ) {
      stop(name, " must be one valid integer.")
    }
    integer_value
  }

  source_parameter_id <- coerce_id(source_parameter_id, "source_parameter_id")
  target_parameter_id <- coerce_id(target_parameter_id, "target_parameter_id")
  result_descriptor_id <- coerce_id(
    result_descriptor_id,
    "result_descriptor_id",
    allow_na = TRUE
  )

  sample_fraction_id <- coerce_id(
    sample_fraction_id,
    "sample_fraction_id",
    allow_na = TRUE
  )

  chemistry_ids <- list(reporting_basis_id = reporting_basis_id,
    result_speciation_id = result_speciation_id, result_target_id = result_target_id)
  chemistry_ids <- lapply(names(chemistry_ids), function(field) coerce_id(chemistry_ids[[field]], field, allow_na = TRUE)) |>
    stats::setNames(c("reporting_basis_id", "result_speciation_id", "result_target_id"))
  chemistry_ids <- chemistry_ids[!vapply(chemistry_ids, is.na, logical(1))]
  if (length(chemistry_ids) && (!is.na(result_descriptor_id) || !is.null(mapping_descriptor_ids))) {
    stop("Use either legacy descriptors or current analytical IDs, not both.")
  }
  if (!is.logical(preserve_existing_chemistry) || length(preserve_existing_chemistry) != 1L ||
      is.na(preserve_existing_chemistry)) stop("preserve_existing_chemistry must be TRUE or FALSE.")
  if (!is.null(mapping_descriptor_ids)) {
    if (
      !is.numeric(mapping_descriptor_ids) ||
        length(mapping_descriptor_ids) == 0L ||
        anyNA(mapping_descriptor_ids) ||
        is.null(names(mapping_descriptor_ids)) ||
        anyNA(names(mapping_descriptor_ids)) ||
        any(!nzchar(names(mapping_descriptor_ids))) ||
        anyDuplicated(names(mapping_descriptor_ids)) ||
        any(!is.finite(mapping_descriptor_ids)) ||
        any(mapping_descriptor_ids != floor(mapping_descriptor_ids))
    ) {
      stop(
        "mapping_descriptor_ids must be a named integer vector with unique keys."
      )
    }
    mapping_descriptor_keys <- names(mapping_descriptor_ids)
    mapping_descriptor_ids <- as.integer(mapping_descriptor_ids)
    names(mapping_descriptor_ids) <- mapping_descriptor_keys
  }
  if (
    !is.character(mapping_descriptor_field) ||
      length(mapping_descriptor_field) != 1L ||
      is.na(mapping_descriptor_field) ||
      !nzchar(mapping_descriptor_field)
  ) {
    stop("mapping_descriptor_field must be one non-empty JSON key.")
  }

  if (
    length(source_parameter_id) != 1L ||
      is.na(source_parameter_id) ||
      length(target_parameter_id) != 1L ||
      is.na(target_parameter_id)
  ) {
    stop("Source and target parameter IDs must each be one valid integer.")
  }
  if (
    !is.logical(delete_source) ||
      length(delete_source) != 1L ||
      is.na(delete_source)
  ) {
    stop("delete_source must be TRUE or FALSE.")
  }
  if (
    !is.logical(preserve_existing_descriptor) ||
      length(preserve_existing_descriptor) != 1L ||
      is.na(preserve_existing_descriptor)
  ) {
    stop("preserve_existing_descriptor must be TRUE or FALSE.")
  }
  if (delete_source && source_parameter_id == target_parameter_id) {
    stop("A parameter cannot be deleted after migrating it to itself.")
  }
  if (!isTRUE(dbTransCheck(con))) {
    stop("Parameter migrations must run inside an active database transaction.")
  }

  source_exists <- DBI::dbGetQuery(
    con,
    "SELECT EXISTS (
       SELECT 1 FROM public.parameters WHERE parameter_id = $1
     ) AS found",
    params = list(source_parameter_id)
  )$found[[1]]

  target_exists <- DBI::dbGetQuery(
    con,
    "SELECT EXISTS (
       SELECT 1 FROM public.parameters WHERE parameter_id = $1
     ) AS found",
    params = list(target_parameter_id)
  )$found[[1]]
  if (!isTRUE(source_exists) || !isTRUE(target_exists)) {
    stop("Both source and target parameter IDs must exist before migration.")
  }

  references <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT
       key_column.table_schema,
       key_column.table_name,
       key_column.column_name,
       EXISTS (
         SELECT 1
         FROM information_schema.columns descriptor_column
         WHERE descriptor_column.table_schema = key_column.table_schema
           AND descriptor_column.table_name = key_column.table_name
           AND descriptor_column.column_name = 'result_descriptor_id'
       ) AS has_result_descriptor,
       EXISTS (
         SELECT 1
         FROM information_schema.columns fraction_column
         WHERE fraction_column.table_schema = key_column.table_schema
           AND fraction_column.table_name = key_column.table_name
           AND fraction_column.column_name = 'sample_fraction_id'
       ) AS has_sample_fraction
     FROM information_schema.key_column_usage key_column
     JOIN information_schema.table_constraints table_constraint
       ON table_constraint.constraint_catalog = key_column.constraint_catalog
      AND table_constraint.constraint_schema = key_column.constraint_schema
      AND table_constraint.constraint_name = key_column.constraint_name
      AND table_constraint.table_schema = key_column.table_schema
      AND table_constraint.table_name = key_column.table_name
     JOIN information_schema.constraint_column_usage referenced_column
       ON referenced_column.constraint_catalog = key_column.constraint_catalog
      AND referenced_column.constraint_schema = key_column.constraint_schema
      AND referenced_column.constraint_name = key_column.constraint_name
     WHERE table_constraint.constraint_type = 'FOREIGN KEY'
       AND referenced_column.table_schema = 'public'
       AND referenced_column.table_name = 'parameters'
       AND referenced_column.column_name = 'parameter_id'
     UNION
     SELECT
       parameter_column.table_schema,
       parameter_column.table_name,
       parameter_column.column_name,
       EXISTS (
         SELECT 1
         FROM information_schema.columns descriptor_column
         WHERE descriptor_column.table_schema = parameter_column.table_schema
           AND descriptor_column.table_name = parameter_column.table_name
           AND descriptor_column.column_name = 'result_descriptor_id'
           ) AS has_result_descriptor,
           EXISTS (
             SELECT 1
             FROM information_schema.columns fraction_column
             WHERE fraction_column.table_schema = parameter_column.table_schema
               AND fraction_column.table_name = parameter_column.table_name
               AND fraction_column.column_name = 'sample_fraction_id'
           ) AS has_sample_fraction
     FROM information_schema.columns parameter_column
     WHERE parameter_column.table_schema = 'discrete'
       AND parameter_column.table_name IN (
         'results', 'import_parameter_mappings'
       )
       AND parameter_column.column_name = 'parameter_id'
     ORDER BY table_schema, table_name, column_name"
  )

  if (nrow(references) == 0L) {
    stop("No database references to public.parameters.parameter_id were found.")
  }

  # Matching unit IDs are essential: relabelling a parameter never converts data.
  units <- DBI::dbGetQuery(con, "SELECT parameter_id, units_liquid, units_solid, units_gas, units_na,
      to_regclass('discrete.reporting_bases') IS NOT NULL AS chemistry_schema
    FROM public.parameters WHERE parameter_id IN ($1,$2) ORDER BY parameter_id FOR UPDATE",
    params = list(source_parameter_id, target_parameter_id))
  if (source_parameter_id != target_parameter_id && any(units$chemistry_schema) && !identical(
      unname(as.list(units[units$parameter_id == source_parameter_id, c("units_liquid", "units_solid", "units_gas", "units_na"), drop = FALSE])),
      unname(as.list(units[units$parameter_id == target_parameter_id, c("units_liquid", "units_solid", "units_gas", "units_na"), drop = FALSE])))) {
    stop("Source and target physical-unit definitions differ. Resolve unit conversions before merging.")
  }
  chemistry_columns <- DBI::dbGetQuery(con, "SELECT table_schema, table_name, column_name
    FROM information_schema.columns WHERE column_name IN
      ('reporting_basis_id','result_speciation_id','result_target_id')")
  if (length(chemistry_ids) && any(!names(chemistry_ids) %in% chemistry_columns$column_name)) {
    stop("The database does not have the requested analytical dimensions.")
  }
  has_mapping_rows <- any(
    references$table_schema == "discrete" &
      references$table_name == "import_parameter_mappings"
  )
  if (has_mapping_rows) {
    guard <- DBI::dbGetQuery(
      con,
      "SELECT tgenabled
       FROM pg_trigger
       WHERE tgrelid = 'discrete.import_parameter_mappings'::regclass
         AND tgname = 'guard_import_mapping_row'
         AND NOT tgisinternal"
    )
    if (nrow(guard) != 1L || guard$tgenabled[[1]] != "O") {
      stop("The import mapping immutability guard is missing or not enabled.")
    }
  }

  DBI::dbExecute(con, "SAVEPOINT parameter_reference_migration")
  tryCatch(
    {
      if (
        source_parameter_id != target_parameter_id &&
          any(
            references$table_schema == "public" &
              references$table_name == "parameter_relationships" &
              references$column_name == "parameter_id"
          )
      ) {
        # Keep a target's existing group membership and drop duplicate source
        # memberships before changing the remaining foreign keys.
        DBI::dbExecute(
          con,
          "DELETE FROM public.parameter_relationships source
           USING public.parameter_relationships target
           WHERE source.parameter_id = $1
             AND target.parameter_id = $2
             AND target.group_id = source.group_id
             AND target.sub_group_id IS NOT DISTINCT FROM source.sub_group_id",
          params = list(source_parameter_id, target_parameter_id)
        )
      }

      # Suggestions are advisory associations, so duplicate links may safely
      # coalesce; result rows and other constrained references never do.
      suggestion_tables <- intersect(references$table_name[references$table_schema == "public"],
        c("parameter_reporting_basis_suggestions", "parameter_speciation_suggestions",
          "parameter_target_suggestions", "parameter_descriptor_suggestions"))
      if (source_parameter_id != target_parameter_id) for (table in suggestion_tables) {
        id <- DBI::dbGetQuery(con, "SELECT column_name FROM information_schema.columns
          WHERE table_schema = 'public' AND table_name = $1
            AND right(column_name, 3) = '_id' AND column_name <> 'parameter_id'",
          params = list(table))$column_name
        if (length(id) != 1L) stop("Review suggestion identity before merging: ", table)
        table_sql <- as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = "public", table = table)))
        id_sql <- as.character(DBI::dbQuoteIdentifier(con, id))
        DBI::dbExecute(con, sprintf("UPDATE %s target SET display_order = LEAST(target.display_order, source.display_order)
          FROM %s source WHERE source.parameter_id = $1 AND target.parameter_id = $2
          AND target.%s = source.%s", table_sql, table_sql, id_sql, id_sql),
          params = list(source_parameter_id, target_parameter_id))
        DBI::dbExecute(con, sprintf("DELETE FROM %s source USING %s target
          WHERE source.parameter_id = $1 AND target.parameter_id = $2 AND target.%s = source.%s",
          table_sql, table_sql, id_sql, id_sql), params = list(source_parameter_id, target_parameter_id))
      }

      if (has_mapping_rows) {
        DBI::dbExecute(
          con,
          "ALTER TABLE discrete.import_parameter_mappings
           DISABLE TRIGGER guard_import_mapping_row"
        )
      }

      for (i in seq_len(nrow(references))) {
        table_schema <- references$table_schema[[i]]
        table_name <- references$table_name[[i]]
        column_name <- references$column_name[[i]]
        is_mapping_table <- table_schema == "discrete" &&
          table_name == "import_parameter_mappings"
        has_descriptor_update <-
          !is.na(result_descriptor_id) &&
          isTRUE(references$has_result_descriptor[[i]]) ||
          (is_mapping_table && !is.null(mapping_descriptor_ids))
        fields <- chemistry_columns$column_name[chemistry_columns$table_schema == table_schema &
          chemistry_columns$table_name == table_name]
        row_chemistry <- chemistry_ids[intersect(names(chemistry_ids), fields)]
        has_fraction_update <-
          !is.na(sample_fraction_id) &&
          isTRUE(references$has_sample_fraction[[i]])
        if (
          source_parameter_id == target_parameter_id &&
            !has_descriptor_update &&
            !has_fraction_update && !length(row_chemistry)
        ) {
          next
        }

        table_sql <- as.character(
          DBI::dbQuoteIdentifier(
            con,
            DBI::Id(schema = table_schema, table = table_name)
          )
        )
        column_sql <- as.character(
          DBI::dbQuoteIdentifier(con, column_name)
        )

        if (length(row_chemistry)) {
          # Assign all supplied dimensions and the target parameter together,
          # allowing the database guard to validate the complete final identity.
          values <- c(list(target_parameter_id), unname(row_chemistry))
          assignments <- c(paste0(column_sql, " = $1"), vapply(seq_along(row_chemistry), function(j) {
            field_sql <- as.character(DBI::dbQuoteIdentifier(con, names(row_chemistry)[[j]]))
            value_sql <- paste0("$", j + 1L)
            if (preserve_existing_chemistry) value_sql <- paste0("COALESCE(", field_sql, ", ", value_sql, ")")
            paste0(field_sql, " = ", value_sql)
          }, character(1)))
          if (has_fraction_update) {
            values <- c(values, list(sample_fraction_id))
            assignments <- c(assignments, paste0("sample_fraction_id = $", length(values)))
          }
          values <- c(values, list(source_parameter_id))
          DBI::dbExecute(con, paste0("UPDATE ", table_sql, " SET ", paste(assignments, collapse = ", "),
            " WHERE ", column_sql, " = $", length(values)), params = values)
          next
        }

        # Assign an existing sample fraction to referencing records.
        # This runs before changing the parameter ID.
        if (has_fraction_update) {
          fraction_sql <- as.character(
            DBI::dbQuoteIdentifier(con, "sample_fraction_id")
          )

          sql <- paste0(
            "UPDATE ",
            table_sql,
            " SET ",
            fraction_sql,
            " = $1",
            " WHERE ",
            column_sql,
            " = $2"
          )

          DBI::dbExecute(
            con,
            sql,
            params = list(
              sample_fraction_id,
              source_parameter_id
            )
          )
        }

        if (
          source_parameter_id == target_parameter_id &&
            !has_descriptor_update
        ) {
          next
        }

        descriptor_sql <- if (isTRUE(references$has_result_descriptor[[i]])) {
          as.character(DBI::dbQuoteIdentifier(con, "result_descriptor_id"))
        } else {
          NULL
        }

        if (is_mapping_table && !is.null(mapping_descriptor_ids)) {
          if (is.null(descriptor_sql)) {
            stop(
              "Import parameter mappings do not have a result_descriptor_id column."
            )
          }
          source_matches <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT DISTINCT source_match ->> $2 AS match_value",
              " FROM ",
              table_sql,
              " WHERE ",
              column_sql,
              " = $1",
              if (preserve_existing_descriptor) {
                " AND result_descriptor_id IS NULL"
              } else {
                ""
              }
            ),
            params = list(source_parameter_id, mapping_descriptor_field)
          )$match_value
          unmatched <- setdiff(source_matches, names(mapping_descriptor_ids))
          if (anyNA(source_matches) || length(unmatched) > 0L) {
            stop(
              "No mapping descriptor was supplied for source_match value(s): ",
              paste(
                c(if (anyNA(source_matches)) "<NULL>" else NULL, unmatched),
                collapse = ", "
              )
            )
          }

          match_cases <- paste0(
            " WHEN $",
            4L + 2L * (seq_along(mapping_descriptor_ids) - 1L),
            " THEN $",
            5L + 2L * (seq_along(mapping_descriptor_ids) - 1L),
            "::integer"
          )
          descriptor_case <- paste0(
            "CASE source_match ->> $3",
            paste(match_cases, collapse = ""),
            " ELSE NULL END"
          )
          descriptor_value_sql <- if (preserve_existing_descriptor) {
            paste0("COALESCE(", descriptor_sql, ", ", descriptor_case, ")")
          } else {
            descriptor_case
          }
          missing_descriptor_filter <- if (
            preserve_existing_descriptor &&
              source_parameter_id == target_parameter_id
          ) {
            paste0(" AND ", descriptor_sql, " IS NULL")
          } else {
            ""
          }
          descriptor_params <- vector(
            "list",
            2L * length(mapping_descriptor_ids)
          )
          descriptor_params[seq.int(1L, length(descriptor_params), by = 2L)] <-
            as.list(names(mapping_descriptor_ids))
          descriptor_params[seq.int(2L, length(descriptor_params), by = 2L)] <-
            as.list(unname(mapping_descriptor_ids))
          sql <- paste0(
            "UPDATE ",
            table_sql,
            " SET ",
            column_sql,
            " = $1, ",
            descriptor_sql,
            " = ",
            descriptor_value_sql,
            " WHERE ",
            column_sql,
            " = $2",
            missing_descriptor_filter
          )
          DBI::dbExecute(
            con,
            sql,
            params = c(
              list(
                target_parameter_id,
                source_parameter_id,
                mapping_descriptor_field
              ),
              descriptor_params
            )
          )
        } else if (
          !is.na(result_descriptor_id) &&
            isTRUE(references$has_result_descriptor[[i]])
        ) {
          descriptor_value_sql <- if (preserve_existing_descriptor) {
            paste0("COALESCE(", descriptor_sql, ", $2)")
          } else {
            "$2"
          }
          missing_descriptor_filter <- if (
            preserve_existing_descriptor &&
              source_parameter_id == target_parameter_id
          ) {
            paste0(" AND ", descriptor_sql, " IS NULL")
          } else {
            ""
          }
          sql <- paste0(
            "UPDATE ",
            table_sql,
            " SET ",
            column_sql,
            " = $1, ",
            descriptor_sql,
            " = ",
            descriptor_value_sql,
            " WHERE ",
            column_sql,
            " = $3",
            missing_descriptor_filter
          )
          DBI::dbExecute(
            con,
            sql,
            params = list(
              target_parameter_id,
              result_descriptor_id,
              source_parameter_id
            )
          )
        } else {
          sql <- paste0(
            "UPDATE ",
            table_sql,
            " SET ",
            column_sql,
            " = $1",
            " WHERE ",
            column_sql,
            " = $2"
          )
          DBI::dbExecute(
            con,
            sql,
            params = list(target_parameter_id, source_parameter_id)
          )
        }
      }

      if (has_mapping_rows) {
        DBI::dbExecute(
          con,
          "ALTER TABLE discrete.import_parameter_mappings
           ENABLE TRIGGER guard_import_mapping_row"
        )
      }

      if (delete_source) {
        deleted <- DBI::dbGetQuery(
          con,
          "DELETE FROM public.parameters
           WHERE parameter_id = $1
           RETURNING parameter_id",
          params = list(source_parameter_id)
        )
        if (nrow(deleted) != 1L) {
          stop(
            "The source parameter was not deleted after its references moved."
          )
        }
      }

      DBI::dbExecute(con, "RELEASE SAVEPOINT parameter_reference_migration")
    },
    error = function(e) {
      try(
        DBI::dbExecute(
          con,
          "ROLLBACK TO SAVEPOINT parameter_reference_migration"
        ),
        silent = TRUE
      )
      try(
        DBI::dbExecute(con, "RELEASE SAVEPOINT parameter_reference_migration"),
        silent = TRUE
      )
      stop(e)
    }
  )

  invisible(TRUE)
}
