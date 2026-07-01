#' Upload source import parameter mappings
#'
#' @description
#' Adds or updates parameter mappings for an external source in
#' `discrete.import_sources` and `discrete.import_parameter_mappings`. The
#' source-side matching fields are chosen by `match_columns`, so simple sources
#' can match on a variable/unit pair while richer source databases can match on
#' any combination of source columns.
#'
#' @param con A connection to the AquaCache database.
#' @param source_code Stable code for the external source or key set.
#' @param mappings A data.frame/data.table of mappings, or a path to a CSV or
#'   Excel workbook file.
#' @param match_columns Character vector of column names from `mappings` that
#'   identify the source-side result.
#' @param source_name Human-readable source name. Defaults to `source_code`.
#' @param source_description Optional source description.
#' @param target_columns Optional named list overriding which columns contain
#'   AquaCache target values. Names can include `parameter`, `result_type`,
#'   `sample_fraction`, `result_value_type`, `result_speciation`, and
#'   `matrix_state`.
#'
#' @return Invisibly returns the resolved mapping table that was uploaded.
#' @export
upsertImportParameterMappings <- function(
  con,
  source_code,
  mappings,
  match_columns,
  source_name = source_code,
  source_description = NULL,
  target_columns = import_mapping_default_target_columns()
) {
  import_mapping_source_match_json <- function(row, match_columns) {
    match_values <- as.list(row[, match_columns, with = FALSE])
    for (nm in names(match_values)) {
      value <- match_values[[nm]][[1]]
      if (length(value) == 0 || is.null(value) || is.na(value)) {
        value <- ""
      }
      match_values[[nm]] <- as.character(value)
    }
    jsonlite::toJSON(match_values, auto_unbox = TRUE, null = "null")
  }

  import_mapping_blank_to_na <- function(x, exclude = character()) {
    char_cols <- setdiff(
      names(x)[vapply(x, is.character, logical(1))],
      exclude
    )
    for (col in char_cols) {
      data.table::set(
        x,
        i = which(
          !nzchar(trimws(x[[col]])) | toupper(trimws(x[[col]])) == "NA"
        ),
        j = col,
        value = NA_character_
      )
    }
    invisible(x)
  }

  if (!import_mapping_tables_available(con)) {
    stop(
      "The database does not have import mapping tables. Apply patch 49 first."
    )
  }
  if (
    !is.character(source_code) ||
      length(source_code) != 1 ||
      !nzchar(source_code)
  ) {
    stop("'source_code' must be a single non-empty character value.")
  }
  if (!is.character(match_columns) || length(match_columns) < 1) {
    stop("'match_columns' must name at least one source-side matching column.")
  }
  if (is.null(source_description)) {
    source_description <- NA_character_
  }

  mappings <- import_mapping_read_input(mappings)
  missing_match_columns <- setdiff(match_columns, names(mappings))
  if (length(missing_match_columns) > 0) {
    stop(
      "The mapping input is missing match_columns: ",
      paste(missing_match_columns, collapse = ", ")
    )
  }

  mappings <- data.table::as.data.table(mappings)
  import_mapping_blank_to_na(mappings, exclude = match_columns)
  if ("ignore" %in% names(mappings)) {
    ignore <- import_mapping_as_logical(mappings$ignore)
    keep <- is.na(ignore) | !ignore
    mappings <- mappings[keep]
  }
  resolved <- import_mapping_resolve_targets(
    con,
    mappings,
    target_columns = target_columns
  )

  active_trans <- dbTransBegin(con)
  tryCatch(
    {
      source_id <- DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.import_sources
           (source_code, source_name, source_description, active)
         VALUES ($1, $2, $3, TRUE)
         ON CONFLICT (source_code) DO UPDATE
         SET source_name = EXCLUDED.source_name,
             source_description = EXCLUDED.source_description,
             active = TRUE
         RETURNING import_source_id;",
        params = list(source_code, source_name, source_description)
      )$import_source_id[[1]]

      for (i in seq_len(nrow(resolved))) {
        source_match <- import_mapping_source_match_json(
          resolved[i],
          match_columns
        )
        DBI::dbExecute(
          con,
          "INSERT INTO discrete.import_parameter_mappings (
             import_source_id,
             source_match,
             parameter_id,
             result_type,
             sample_fraction_id,
             result_value_type,
             result_speciation_id,
             matrix_state_id,
             conversion,
             result_offset,
             priority,
             active,
             note
           ) VALUES (
             $1, $2::jsonb, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13
           )
           ON CONFLICT (import_source_id, source_match) DO UPDATE
           SET parameter_id = EXCLUDED.parameter_id,
               result_type = EXCLUDED.result_type,
               sample_fraction_id = EXCLUDED.sample_fraction_id,
               result_value_type = EXCLUDED.result_value_type,
               result_speciation_id = EXCLUDED.result_speciation_id,
               matrix_state_id = EXCLUDED.matrix_state_id,
               conversion = EXCLUDED.conversion,
               result_offset = EXCLUDED.result_offset,
               priority = EXCLUDED.priority,
               active = EXCLUDED.active,
               note = EXCLUDED.note;",
          params = list(
            source_id,
            source_match,
            resolved$parameter_id[[i]],
            resolved$result_type[[i]],
            resolved$sample_fraction_id[[i]],
            resolved$result_value_type[[i]],
            resolved$result_speciation_id[[i]],
            resolved$matrix_state_id[[i]],
            resolved$conversion[[i]],
            resolved$result_offset[[i]],
            resolved$priority[[i]],
            resolved$active[[i]],
            resolved$note[[i]]
          )
        )
      }

      if (active_trans) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active_trans) {
        try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
      }
      stop(e)
    }
  )

  invisible(resolved)
}

#' Add or update a reusable file import profile
#'
#' @description
#' Adds or updates a profile that describes how an uploaded spreadsheet or
#' delimited file should be read and normalized before parameter mappings are
#' applied. This is intended for Shiny upload workflows where users select or
#' create a key/profile before previewing lab or field data.
#'
#' @param con A connection to the AquaCache database.
#' @param source_code Stable source/key code in `discrete.import_sources`.
#' @param profile_code Stable profile code within the source.
#' @param profile_name Human-readable profile name.
#' @param source_name Human-readable source name, used if the source row must
#'   be created.
#' @param source_description Optional source description, used if the source row
#'   must be created or updated.
#' @param profile_description Optional profile description.
#' @param file_type File type: `csv`, `tsv`, `txt`, `xlsx`, `xlsm`, or `xls`.
#' @param parser_type Source data shape: `long`, `wide`, or `mixed`.
#' @param sheet_strategy Sheet selection strategy: `name`, `index`, `first`,
#'   `name_or_first`, or `all`.
#' @param sheet_name Optional sheet name.
#' @param sheet_index Optional 1-based sheet index.
#' @param header_row 1-based row containing column names.
#' @param units_row Optional 1-based row containing units for wide files.
#' @param parameter_row Optional 1-based row containing parameter labels/codes
#'   for wide files.
#' @param data_start_row 1-based first data row.
#' @param datetime_origin How datetimes are represented: `excel_1900`,
#'   `excel_1904`, `text`, `posix`, or `date_time_columns`.
#' @param timezone Time zone used to interpret source datetimes.
#' @param column_map Named list mapping AquaCache staging fields to source
#'   columns.
#' @param wide_config Named list describing wide-format result columns.
#' @param defaults Named list of profile-level defaults.
#' @param sample_identity Character vector naming fields that identify a sample.
#' @param result_identity Character vector naming fields that identify a result.
#' @param validation_rules Named list of extra validation rules for the UI or
#'   parser.
#' @param active Whether this profile can be selected for new imports.
#' @param note Optional note.
#'
#' @return The `import_profile_id`.
#' @export
upsertImportProfile <- function(
  con,
  source_code,
  profile_code,
  profile_name,
  source_name = source_code,
  source_description = NULL,
  profile_description = NULL,
  file_type = "xlsx",
  parser_type = "long",
  sheet_strategy = "name_or_first",
  sheet_name = NULL,
  sheet_index = NULL,
  header_row = 1L,
  units_row = NULL,
  parameter_row = NULL,
  data_start_row = 2L,
  datetime_origin = "excel_1900",
  timezone = "UTC",
  column_map = list(),
  wide_config = list(),
  defaults = list(),
  sample_identity = c(
    "location_id",
    "sub_location_id",
    "media_id",
    "z",
    "datetime",
    "sample_type",
    "collection_method"
  ),
  result_identity = c(
    "result_type",
    "parameter_id",
    "matrix_state_id",
    "sample_fraction_id",
    "result_value_type",
    "result_speciation_id",
    "protocol_method",
    "laboratory",
    "analysis_datetime"
  ),
  validation_rules = list(),
  active = TRUE,
  note = NULL
) {
  if (!import_mapping_tables_available(con)) {
    stop(
      "The database does not have import mapping tables. Apply patch 49 first."
    )
  }
  if (is.null(source_description)) {
    source_description <- NA_character_
  }
  if (is.null(profile_description)) {
    profile_description <- NA_character_
  }
  if (is.null(sheet_name)) {
    sheet_name <- NA_character_
  }
  if (is.null(sheet_index)) {
    sheet_index <- NA_integer_
  }
  if (is.null(units_row)) {
    units_row <- NA_integer_
  }
  if (is.null(parameter_row)) {
    parameter_row <- NA_integer_
  }
  if (is.null(note)) {
    note <- NA_character_
  }

  source_id <- import_mapping_upsert_source(
    con = con,
    source_code = source_code,
    source_name = source_name,
    source_description = source_description
  )

  out <- DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_profiles (
       import_source_id,
       profile_code,
       profile_name,
       profile_description,
       file_type,
       parser_type,
       sheet_strategy,
       sheet_name,
       sheet_index,
       header_row,
       units_row,
       parameter_row,
       data_start_row,
       datetime_origin,
       timezone,
       column_map,
       wide_config,
       defaults,
       sample_identity,
       result_identity,
       validation_rules,
       active,
       note
     ) VALUES (
       $1, $2, $3, $4, $5, $6, $7, $8, $9, $10,
       $11, $12, $13, $14, $15, $16::jsonb, $17::jsonb,
       $18::jsonb, $19::jsonb, $20::jsonb, $21::jsonb, $22, $23
     )
     ON CONFLICT (import_source_id, profile_code) DO UPDATE
     SET profile_name = EXCLUDED.profile_name,
         profile_description = EXCLUDED.profile_description,
         file_type = EXCLUDED.file_type,
         parser_type = EXCLUDED.parser_type,
         sheet_strategy = EXCLUDED.sheet_strategy,
         sheet_name = EXCLUDED.sheet_name,
         sheet_index = EXCLUDED.sheet_index,
         header_row = EXCLUDED.header_row,
         units_row = EXCLUDED.units_row,
         parameter_row = EXCLUDED.parameter_row,
         data_start_row = EXCLUDED.data_start_row,
         datetime_origin = EXCLUDED.datetime_origin,
         timezone = EXCLUDED.timezone,
         column_map = EXCLUDED.column_map,
         wide_config = EXCLUDED.wide_config,
         defaults = EXCLUDED.defaults,
         sample_identity = EXCLUDED.sample_identity,
         result_identity = EXCLUDED.result_identity,
         validation_rules = EXCLUDED.validation_rules,
         active = EXCLUDED.active,
         note = EXCLUDED.note
     RETURNING import_profile_id;",
    params = list(
      source_id,
      profile_code,
      profile_name,
      profile_description,
      file_type,
      parser_type,
      sheet_strategy,
      sheet_name,
      sheet_index,
      as.integer(header_row),
      units_row,
      parameter_row,
      as.integer(data_start_row),
      datetime_origin,
      timezone,
      import_mapping_json(column_map, object = TRUE),
      import_mapping_json(wide_config, object = TRUE),
      import_mapping_json(defaults, object = TRUE),
      import_mapping_json(sample_identity, object = FALSE),
      import_mapping_json(result_identity, object = FALSE),
      import_mapping_json(validation_rules, object = TRUE),
      isTRUE(active),
      note
    )
  )

  out$import_profile_id[[1]]
}

#' List file import profiles
#'
#' @description
#' List file import profiles from the AquaCache database. This can be filtered by source code and/or active status.
#' @param con A connection to the AquaCache database.
#' @param source_code Optional source code filter.
#' @param active Optional active filter. Use `NULL` to return both active and
#'   inactive profiles.
#' @param parse_json If `TRUE`, JSON columns are returned as list columns.
#'
#' @return A data.frame of import profiles.
#' @export
getImportProfiles <- function(
  con,
  source_code = NULL,
  active = TRUE,
  parse_json = TRUE
) {
  where <- character()
  params <- list()
  if (!is.null(source_code)) {
    params <- c(params, list(source_code))
    where <- c(where, paste0("s.source_code = $", length(params)))
  }
  if (!is.null(active)) {
    params <- c(params, list(isTRUE(active)))
    where <- c(where, paste0("p.active = $", length(params)))
  }
  where <- if (length(where)) {
    paste("WHERE", paste(where, collapse = " AND "))
  } else {
    ""
  }

  sql <- paste0(
    "SELECT
       p.import_profile_id,
       s.import_source_id,
       s.source_code,
       s.source_name,
       p.profile_code,
       p.profile_name,
       p.profile_description,
       p.file_type,
       p.parser_type,
       p.sheet_strategy,
       p.sheet_name,
       p.sheet_index,
       p.header_row,
       p.units_row,
       p.parameter_row,
       p.data_start_row,
       p.datetime_origin,
       p.timezone,
       p.column_map::text AS column_map,
       p.wide_config::text AS wide_config,
       p.defaults::text AS defaults,
       p.sample_identity::text AS sample_identity,
       p.result_identity::text AS result_identity,
       p.validation_rules::text AS validation_rules,
       p.active,
       p.note
     FROM discrete.import_profiles p
     JOIN discrete.import_sources s
       ON s.import_source_id = p.import_source_id
     ",
    where,
    "
     ORDER BY s.source_code, p.profile_name, p.profile_code;"
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  if (parse_json && nrow(out) > 0) {
    json_cols <- c(
      "column_map",
      "wide_config",
      "defaults",
      "sample_identity",
      "result_identity",
      "validation_rules"
    )
    for (col in json_cols) {
      out[[col]] <- lapply(out[[col]], jsonlite::fromJSON)
    }
  }
  out
}

#' Add or update import qualifier mappings
#'
#' @description
#' Add or update keys for use in importing foreign water quality data to AquaCache. Used notably by [downloadECCCwq()] and [downloadEQWin()].
#'
#' @param con A connection to the AquaCache database.
#' @param source_code Stable source/key code in `discrete.import_sources`.
#' @param mappings A data.frame/data.table of qualifier mappings.
#' @param profile_code Optional profile code. When supplied, mappings are scoped
#'   to that profile; otherwise they apply to the source.
#' @param source_name Human-readable source name, used if the source row must
#'   be created.
#'
#' @return Invisibly returns the uploaded mappings.
#' @export
upsertImportQualifierMappings <- function(
  con,
  source_code,
  mappings,
  profile_code = NULL,
  source_name = source_code
) {
  required <- c("qualifier_value")
  missing <- setdiff(required, names(mappings))
  if (length(missing) > 0) {
    stop(
      "Missing qualifier mapping column(s): ",
      paste(missing, collapse = ", ")
    )
  }

  source_id <- import_mapping_upsert_source(
    con = con,
    source_code = source_code,
    source_name = source_name
  )
  profile_id <- NULL
  if (!is.null(profile_code)) {
    profile_id <- DBI::dbGetQuery(
      con,
      "SELECT import_profile_id
       FROM discrete.import_profiles
       WHERE import_source_id = $1
         AND profile_code = $2;",
      params = list(source_id, profile_code)
    )
    if (nrow(profile_id) != 1) {
      stop(
        "Could not find exactly one import profile for source_code/profile_code."
      )
    }
    profile_id <- profile_id$import_profile_id[[1]]
  } else {
    profile_id <- NA_integer_
  }

  mappings <- data.table::as.data.table(mappings)
  optional_defaults <- list(
    qualifier_column = NA_character_,
    result_condition = NA,
    result_condition_value_source = "none",
    result_condition_value_literal = NA_real_,
    result_action = "keep_result",
    note_template = NA_character_,
    priority = 100L,
    active = TRUE,
    note = NA_character_
  )
  for (nm in names(optional_defaults)) {
    if (!(nm %in% names(mappings))) {
      mappings[[nm]] <- optional_defaults[[nm]]
    }
  }

  condition_lookup <- DBI::dbGetQuery(
    con,
    "SELECT result_condition_id, result_condition
     FROM discrete.result_conditions;"
  )

  for (i in seq_len(nrow(mappings))) {
    condition <- mappings$result_condition[[i]]
    if (import_mapping_is_missing(condition)) {
      condition <- NA_integer_
    } else if (
      is.numeric(condition) || grepl("^[0-9]+$", as.character(condition))
    ) {
      condition <- as.integer(condition)
    } else {
      hit <- condition_lookup[
        tolower(condition_lookup$result_condition) ==
          tolower(trimws(as.character(condition))),
      ]
      if (nrow(hit) != 1) {
        stop("Unknown result_condition qualifier mapping value: ", condition)
      }
      condition <- hit$result_condition_id[[1]]
    }

    DBI::dbExecute(
      con,
      "INSERT INTO discrete.import_qualifier_mappings (
         import_source_id,
         import_profile_id,
         qualifier_column,
         qualifier_value,
         result_condition,
         result_condition_value_source,
         result_condition_value_literal,
         result_action,
         note_template,
         priority,
         active,
         note
       ) VALUES (
         $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12
       )
       ON CONFLICT (
         import_source_id,
         import_profile_id,
         qualifier_column,
         qualifier_value
       ) DO UPDATE
       SET result_condition = EXCLUDED.result_condition,
           result_condition_value_source = EXCLUDED.result_condition_value_source,
           result_condition_value_literal = EXCLUDED.result_condition_value_literal,
           result_action = EXCLUDED.result_action,
           note_template = EXCLUDED.note_template,
           priority = EXCLUDED.priority,
           active = EXCLUDED.active,
           note = EXCLUDED.note;",
      params = list(
        source_id,
        profile_id,
        mappings$qualifier_column[[i]],
        mappings$qualifier_value[[i]],
        condition,
        mappings$result_condition_value_source[[i]],
        mappings$result_condition_value_literal[[i]],
        mappings$result_action[[i]],
        mappings$note_template[[i]],
        as.integer(mappings$priority[[i]]),
        isTRUE(import_mapping_as_logical(mappings$active[[i]])),
        mappings$note[[i]]
      )
    )
  }

  invisible(mappings)
}

#' Create an import upload session
#'
#' @param con A connection to the AquaCache database.
#' @param import_profile_id Import profile ID.
#' @param source_file_name Optional uploaded source file name.
#' @param source_file_hash Optional source file hash.
#' @param source_file_size Optional source file size in bytes.
#' @param summary Optional named list of session summary values.
#' @param validation_summary Optional named list of validation summary values.
#' @param note Optional note.
#'
#' @return The `import_upload_session_id`.
#' @export
createImportUploadSession <- function(
  con,
  import_profile_id,
  source_file_name = NULL,
  source_file_hash = NULL,
  source_file_size = NULL,
  summary = list(),
  validation_summary = list(),
  note = NULL
) {
  if (is.null(source_file_name)) {
    source_file_name <- NA_character_
  }
  if (is.null(source_file_hash)) {
    source_file_hash <- NA_character_
  }
  if (is.null(source_file_size)) {
    source_file_size <- NA_integer_
  }
  if (is.null(note)) {
    note <- NA_character_
  }

  DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_upload_sessions (
       import_profile_id,
       source_file_name,
       source_file_hash,
       source_file_size,
       summary,
       validation_summary,
       note
     ) VALUES (
       $1, $2, $3, $4, $5::jsonb, $6::jsonb, $7
     )
     RETURNING import_upload_session_id;",
    params = list(
      as.integer(import_profile_id),
      source_file_name,
      source_file_hash,
      source_file_size,
      import_mapping_json(summary, object = TRUE),
      import_mapping_json(validation_summary, object = TRUE),
      note
    )
  )$import_upload_session_id[[1]]
}

#' Append or update staged import rows
#'
#' @param con A connection to the AquaCache database.
#' @param import_upload_session_id Import upload session ID.
#' @param rows A data.frame/data.table with optional columns matching
#'   `discrete.import_upload_rows`.
#'
#' @return Invisibly returns `rows`.
#' @export
appendImportUploadRows <- function(
  con,
  import_upload_session_id,
  rows
) {
  rows <- data.table::as.data.table(rows)
  defaults <- list(
    sheet_name = NA_character_,
    source_row_number = NA_integer_,
    result_index = 1L,
    source_record = NULL,
    normalized_sample = NULL,
    normalized_result = NULL,
    validation_status = "pending",
    validation_messages = NULL,
    sample_id = NA_integer_,
    result_id = NA_integer_
  )
  for (nm in names(defaults)) {
    if (!(nm %in% names(rows))) {
      if (
        nm %in%
          c(
            "source_record",
            "normalized_sample",
            "normalized_result",
            "validation_messages"
          )
      ) {
        rows[[nm]] <- replicate(nrow(rows), list(), simplify = FALSE)
      } else {
        rows[[nm]] <- defaults[[nm]]
      }
    }
  }

  for (i in seq_len(nrow(rows))) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.import_upload_rows (
         import_upload_session_id,
         sheet_name,
         source_row_number,
         result_index,
         source_record,
         normalized_sample,
         normalized_result,
         validation_status,
         validation_messages,
         sample_id,
         result_id
       ) VALUES (
         $1, $2, $3, $4, $5::jsonb, $6::jsonb, $7::jsonb,
         $8, $9::jsonb, $10, $11
       )
       ON CONFLICT (
         import_upload_session_id,
         sheet_name,
         source_row_number,
         result_index
       ) DO UPDATE
       SET source_record = EXCLUDED.source_record,
           normalized_sample = EXCLUDED.normalized_sample,
           normalized_result = EXCLUDED.normalized_result,
           validation_status = EXCLUDED.validation_status,
           validation_messages = EXCLUDED.validation_messages,
           sample_id = EXCLUDED.sample_id,
           result_id = EXCLUDED.result_id;",
      params = list(
        as.integer(import_upload_session_id),
        rows$sheet_name[[i]],
        rows$source_row_number[[i]],
        rows$result_index[[i]],
        import_mapping_json(rows$source_record[[i]], object = TRUE),
        import_mapping_json(rows$normalized_sample[[i]], object = TRUE),
        import_mapping_json(rows$normalized_result[[i]], object = TRUE),
        rows$validation_status[[i]],
        import_mapping_json(rows$validation_messages[[i]], object = FALSE),
        rows$sample_id[[i]],
        rows$result_id[[i]]
      )
    )
  }

  invisible(rows)
}

#' Add or update an import mapping source
#'
#' @description
#' Adds a new import mapping source or updates an existing one in the AquaCache database.
#' @param con A connection to the AquaCache database.
#' @param source_code The code for the import source.
#' @param source_name The name of the import source. Defaults to `source_code`.
#' @param source_description A description of the import source. Defaults to `NULL`.
#' @return The ID of the import source.
#' @noRd
#' @keywords internal
import_mapping_upsert_source <- function(
  con,
  source_code,
  source_name = source_code,
  source_description = NULL
) {
  if (is.null(source_description)) {
    source_description <- NA_character_
  }
  DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_sources
       (source_code, source_name, source_description, active)
     VALUES ($1, $2, $3, TRUE)
     ON CONFLICT (source_code) DO UPDATE
     SET source_name = EXCLUDED.source_name,
         source_description = EXCLUDED.source_description,
         active = TRUE
     RETURNING import_source_id;",
    params = list(source_code, source_name, source_description)
  )$import_source_id[[1]]
}

import_mapping_json <- function(x, object = TRUE) {
  if (is.null(x)) {
    x <- if (object) list() else character()
  }
  if (object && length(x) == 0L) {
    return("{}")
  }
  if (is.character(x) && length(x) == 1L) {
    trimmed <- trimws(x)
    if (grepl("^\\{.*\\}$|^\\[.*\\]$", trimmed)) {
      return(trimmed)
    }
  }
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
}

#' Check if import mapping tables are available
#'
#' @description Checks if the necessary tables for import mappings (`discrete.import_sources` and `discrete.import_parameter_mappings`) exist in the database. This is used to determine if import mapping functionality can be used, and to provide a clear error message if the tables are missing (e.g., if patch 49 has not been applied).
#' @param con A connection to the AquaCache database.
#' @return TRUE if both `discrete.import_sources` and `discrete.import_parameter_mappings` tables exist, FALSE otherwise.
#' @keywords internal
#' @noRd
import_mapping_tables_available <- function(con) {
  isTRUE(DBI::dbGetQuery(
    con,
    "SELECT to_regclass('discrete.import_sources') IS NOT NULL
       AND to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS available;"
  )$available[[1]])
}

#' Read import mapping input
#'
#' @description
#' Reads the import mapping input, which can be either a data.frame/data.table or a path to a CSV or Excel file. If a file path is provided, the function reads the file and returns it as a data.table. If a data.frame/data.table is provided, it is returned as a data.table. The function also checks that the file exists if a path is provided, and that the input is of the correct type.
#' @param mappings A data.frame/data.table of mappings, or a path to a CSV or Excel workbook file.
#' @return A data.table containing the import mappings.
#' @keywords internal
#' @noRd
import_mapping_read_input <- function(mappings) {
  if (is.character(mappings) && length(mappings) == 1) {
    if (!file.exists(mappings)) {
      stop("The mapping file does not exist: ", mappings)
    }
    ext <- tolower(tools::file_ext(mappings))
    if (ext %in% c("xlsx", "xlsm", "xls")) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop(
          "Reading Excel import keys requires the optional 'openxlsx' package."
        )
      }
      return(openxlsx::read.xlsx(mappings))
    }
    return(data.table::fread(
      mappings,
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    ))
  }
  if (!inherits(mappings, "data.frame")) {
    stop("'mappings' must be a data.frame, data.table, or CSV path.")
  }
  mappings
}

#' Default target columns for import mappings
#'
#' @description
#' Provides the default candidate column names for each AquaCache target field in import mappings. This function returns a named list where each name corresponds to an AquaCache target field (e.g., `parameter`, `result_type`, `sample_fraction`, `result_value_type`, `result_speciation`, `matrix_state`) and each value is a character vector of candidate column names that the `import_mapping_resolve_targets()` function will look for in the input mapping table to identify the source of the target values. Users can override these defaults by providing a custom `target_columns` argument to the `upsertImportParameterMappings()` function.
#' @return A named list of default target column candidates for import mappings.
#' @keywords internal
#' @noRd
import_mapping_default_target_columns <- function() {
  list(
    parameter = c("parameter_id", "parameter", "param_name"),
    result_type = c("result_type", "result_type_id"),
    sample_fraction = c("sample_fraction_id", "sample_fraction"),
    result_value_type = c("result_value_type", "result_value_type_id"),
    result_speciation = c("result_speciation_id", "result_speciation"),
    matrix_state = c("matrix_state_id", "matrix_state")
  )
}

#' Resolve import mapping target IDs
#'
#' @description
#' Resolves the target IDs for an import mapping table by looking up the corresponding values in the database. For example, it resolves `parameter_id` by looking up the `parameter` name in the `public.parameters` table, resolves `result_type` by looking up the `result_type` name in the `discrete.result_types` table, and so on. The function also handles various input formats for the target values, such as allowing either IDs or names, and converting logical values to IDs when appropriate. The resolved target IDs are added as new columns to the mapping table, and the function returns the updated table.
#' @param con A connection to the AquaCache database.
#' @param mappings A data.table of import mappings for a given source, with an additional list column `source_match_values` containing the parsed JSON values of the `source_match` column.
#' @param target_columns A named list overriding which columns contain AquaCache target values. Names can include `parameter`, `result_type`, `sample_fraction`, `result_value_type`, `result_speciation`, and `matrix_state`. Each element should be a character vector of candidate column names to look for in the `mappings` table for that target.
#' @return The input `mappings` data.table with additional columns for the resolved target IDs, such as `parameter_id`, `result_type`, `sample_fraction_id`, `result_value_type_id`, `result_speciation_id`, and `matrix_state_id`.
#' @keywords internal
#' @noRd
import_mapping_resolve_targets <- function(
  con,
  mappings,
  target_columns = import_mapping_default_target_columns()
) {
  import_mapping_resolve_lookup_vector <- function(
    con,
    values,
    id_column,
    table,
    label_columns,
    value_name,
    allow_na
  ) {
    import_mapping_resolve_lookup_id <- function(
      value,
      con,
      id_column,
      table,
      label_columns,
      value_name,
      allow_na
    ) {
      if (import_mapping_is_missing(value)) {
        if (allow_na) {
          return(NA_integer_)
        }
        stop("Missing required import mapping value for ", value_name, ".")
      }
      if (is.logical(value)) {
        value <- as.integer(value)
        if (allow_na && identical(value, 0L)) {
          return(NA_integer_)
        }
        return(value)
      }
      if (is.numeric(value) || grepl("^[0-9]+$", trimws(as.character(value)))) {
        value <- as.integer(value)
        if (allow_na && identical(value, 0L)) {
          return(NA_integer_)
        }
        return(value)
      }

      predicates <- paste(
        sprintf("LOWER(%s) = LOWER($1)", label_columns),
        collapse = " OR "
      )
      sql <- sprintf(
        "SELECT %s FROM %s WHERE %s ORDER BY %s;",
        id_column,
        table,
        predicates,
        id_column
      )
      hits <- DBI::dbGetQuery(
        con,
        sql,
        params = list(trimws(as.character(value)))
      )

      if (nrow(hits) == 0) {
        stop("Unknown ", value_name, " import mapping value: ", value)
      }
      if (nrow(hits) > 1) {
        stop("Ambiguous ", value_name, " import mapping value: ", value)
      }

      as.integer(hits[[id_column]][[1]])
    }

    vapply(
      values,
      import_mapping_resolve_lookup_id,
      integer(1),
      con = con,
      id_column = id_column,
      table = table,
      label_columns = label_columns,
      value_name = value_name,
      allow_na = allow_na
    )
  }

  target_columns <- utils::modifyList(
    import_mapping_default_target_columns(),
    target_columns
  )

  result <- data.table::copy(mappings)
  result[,
    parameter_id := import_mapping_resolve_lookup_vector(
      con,
      import_mapping_column(result, target_columns$parameter),
      "parameter_id",
      "public.parameters",
      c("param_name"),
      "parameter",
      allow_na = TRUE
    )
  ]
  result[,
    result_type := import_mapping_resolve_lookup_vector(
      con,
      import_mapping_column(result, target_columns$result_type),
      "result_type_id",
      "discrete.result_types",
      c("result_type"),
      "result_type",
      allow_na = FALSE
    )
  ]
  result[,
    sample_fraction_id := import_mapping_resolve_lookup_vector(
      con,
      import_mapping_column(result, target_columns$sample_fraction),
      "sample_fraction_id",
      "discrete.sample_fractions",
      c("sample_fraction"),
      "sample_fraction",
      allow_na = TRUE
    )
  ]
  result[,
    result_value_type := import_mapping_resolve_lookup_vector(
      con,
      import_mapping_column(result, target_columns$result_value_type),
      "result_value_type_id",
      "discrete.result_value_types",
      c("result_value_type"),
      "result_value_type",
      allow_na = TRUE
    )
  ]
  result[,
    result_speciation_id := import_mapping_resolve_lookup_vector(
      con,
      import_mapping_column(result, target_columns$result_speciation),
      "result_speciation_id",
      "discrete.result_speciations",
      c("result_speciation"),
      "result_speciation",
      allow_na = TRUE
    )
  ]

  matrix_state_input <- import_mapping_column(
    result,
    target_columns$matrix_state
  )
  result[,
    matrix_state_id := vapply(
      matrix_state_input,
      function(x) {
        if (import_mapping_is_missing(x)) {
          return(NA_integer_)
        }
        resolve_matrix_state_identifier(con, x)
      },
      integer(1)
    )
  ]

  if (!("conversion" %in% names(result))) {
    result[, conversion := 1]
  }
  result[, conversion := as.numeric(conversion)]
  result[is.na(conversion), conversion := 1]

  if (!("result_offset" %in% names(result))) {
    result[, result_offset := 0]
  }
  result[, result_offset := as.numeric(result_offset)]
  result[is.na(result_offset), result_offset := 0]

  if (!("priority" %in% names(result))) {
    result[, priority := 100L]
  }
  result[, priority := as.integer(priority)]
  result[is.na(priority), priority := 100L]

  if (!("active" %in% names(result))) {
    result[, active := TRUE]
  }
  result[, active := import_mapping_as_logical(active)]
  result[is.na(active), active := TRUE]

  if (!("note" %in% names(result))) {
    result[, note := NA_character_]
  }

  return(result)
}

#' Extract a column from the import mapping table based on candidate column names
#'
#' @description
#' Extracts a column from the import mapping table by checking for the presence of candidate column names. The function takes a data.table and a character vector of candidate column names, and returns the  first column that matches one of the candidate names. If none of the candidate columns are found in the data.table, it returns a character vector of NA values with the same length as the number of rows in the data.table. This is used to flexibly identify which column in the input mapping table corresponds to a given target field (e.g., parameter, result_type) based on common naming conventions.
#' @param x A data.table containing the import mappings.
#' @param candidates A character vector of candidate column names to look for in the data.table.
#' @return A character vector containing the values from the first matching column, or NA if no matching column is found.
#' @keywords internal
#' @noRd
import_mapping_column <- function(x, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(x)) {
      return(x[[candidate]])
    }
  }
  rep(NA_character_, nrow(x))
}

#' Check if an import mapping value is missing
#'
#' @description
#' Checks if a given value from an import mapping is considered missing. This function treats values as missing if they are NULL, NA, empty strings, or character strings that are "NA" or "NULL" (case-insensitive). This is used to determine if a required value is missing from the import mapping, and to handle optional values appropriately.
#' @param x A value to check for missingness.
#' @return TRUE if the value is considered missing, FALSE otherwise.
#' @keywords internal
#' @noRd
import_mapping_is_missing <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(TRUE)
  }
  if (is.na(x)) {
    return(TRUE)
  }
  if (is.character(x)) {
    x <- trimws(x)
    return(!nzchar(x) || toupper(x) %in% c("NA", "NULL"))
  }
  FALSE
}


#' Convert various representations of logical values to R logicals
#' @description
#' Converts various representations of logical values (e.g., "true", "false", "1", "0", "yes", "no") into R logicals (`TRUE` or `FALSE`).
#' @param x A value or vector of values to be converted to logical.
#' @return A logical vector corresponding to the input values.
#' @keywords internal
#' @noRd
import_mapping_as_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  x <- trimws(tolower(as.character(x)))
  x[x %in% c("true", "t", "1", "yes", "y")] <- "TRUE"
  x[x %in% c("false", "f", "0", "no", "n")] <- "FALSE"
  as.logical(x)
}

#' Load import mappings for a given source key
#'
#' @description Attempts to load active import parameter mappings for a given source key by matching against the `source_code` in `discrete.import_sources`. The function tries various transformations of the key (e.g., stripping directory and extension) to find a match. If a match is found, the corresponding active mappings from `discrete.import_parameter_mappings` are returned as a data.table with an additional list column `source_match_values` containing the parsed JSON values of the `source_match` column. If no match is found, NULL is returned.
#' @param con A connection to the AquaCache database.
#' @param key A source key to identify the import source, such as a source code or filename. The function will attempt to find an active import source with a matching `source_code`, trying various transformations of the key (e.g., stripping directory and extension) to find a match
#' @keywords internal
#' @noRd
import_mapping_load_db <- function(con, key) {
  if (!import_mapping_tables_available(con)) {
    return(NULL)
  }

  import_mapping_source_candidates <- function(key) {
    key <- as.character(key)[1]
    unique(c(
      key,
      tools::file_path_sans_ext(key),
      basename(key),
      tools::file_path_sans_ext(basename(key))
    ))
  }

  for (source_code in import_mapping_source_candidates(key)) {
    source <- DBI::dbGetQuery(
      con,
      "SELECT import_source_id
       FROM discrete.import_sources
       WHERE source_code = $1
         AND active
       LIMIT 1;",
      params = list(source_code)
    )
    if (nrow(source) == 0) {
      next
    }

    mappings <- DBI::dbGetQuery(
      con,
      "SELECT
         import_mapping_id,
         source_match::text AS source_match,
         parameter_id,
         result_type,
         sample_fraction_id,
         result_value_type,
         result_speciation_id,
         matrix_state_id,
         conversion,
         result_offset,
         priority
       FROM discrete.import_parameter_mappings
       WHERE import_source_id = $1
         AND active
       ORDER BY priority, import_mapping_id;",
      params = list(source$import_source_id[[1]])
    )

    if (nrow(mappings) == 0) {
      next
    }

    mappings <- data.table::as.data.table(mappings)
    mappings[,
      source_match_values := lapply(
        source_match,
        jsonlite::fromJSON,
        simplifyVector = TRUE
      )
    ]
    mappings[,
      source_match_size := vapply(
        source_match_values,
        length,
        integer(1)
      )
    ]
    attr(mappings, "source_code") <- source_code
    return(mappings)
  }

  NULL
}

#' Resolve an import mapping for a given source match
#'
#' @param mappings A data.table of import mappings for a given source, with an additional list column `source_match_values` containing the parsed JSON values of the `source_match` column.
#' @param source_match A named list of source-side values to match against the `source_match_values` in the mappings. Names should correspond to the keys used in the `source_match` JSON, which in turn should correspond to the `match_columns` used when uploading the mappings.
#' @return A single row of the `mappings` data.table that best matches the `source_match`, or NULL if no mappings match.
#' @details
#' The function identifies which mappings match the provided `source_match` values, then selects the best match based on the highest priority and most specific match (i.e., largest number of non-empty matching criteria). If there are multiple tied best matches, an error is raised.
#' @keywords internal
#' @noRd
import_mapping_resolve_match <- function(mappings, source_match) {
  source_match <- lapply(source_match, function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
      return("")
    }
    as.character(x[[1]])
  })

  hits <- vapply(
    mappings$source_match_values,
    function(criteria) {
      criteria_names <- names(criteria)
      if (!all(criteria_names %in% names(source_match))) {
        return(FALSE)
      }
      all(vapply(
        criteria_names,
        function(nm) {
          !is.na(source_match[[nm]]) &&
            identical(as.character(criteria[[nm]]), source_match[[nm]])
        },
        logical(1)
      ))
    },
    logical(1)
  )

  if (!any(hits)) {
    return(NULL)
  }

  matched <- mappings[hits]
  data.table::setorder(
    matched,
    priority,
    -source_match_size,
    import_mapping_id
  )
  tied <- matched[
    priority == matched$priority[[1]] &
      source_match_size == matched$source_match_size[[1]]
  ]
  if (nrow(tied) > 1) {
    stop(
      "Multiple import mappings matched the same source record with equal priority and specificity."
    )
  }

  matched[1]
}
