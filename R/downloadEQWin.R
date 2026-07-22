#' Bring EQWin water quality data into AquaCache
#'
#' @description
#' Brings water quality data from an EQWin Access database into the list format
#' expected by [getNewDiscrete()] and [synchronize_discrete()]. Parameter
#' mapping is read from `discrete.import_parameter_mappings` when the requested
#' key/source code has been loaded with [upsertImportParameterMappings()]. EQWin
#' `udf_*` fields and `eqsampls.LastModified` are intentionally not used.
#' EQWin `SampleClass` values are mapped to AquaCache sample types where
#' possible: regular monitoring (`M`) to routine samples, QA/QC duplicate or
#' split samples (`D`) to generic QC samples, incident samples (`I`) to
#' `sample-other`, and unset/default samples (`XX`) to `unknown`. Blank sample
#' wording in the sample number or comments is mapped to field, trip, or lab
#' blank sample types when present.
#'
#' @param location EQWin station code from `eqstns.StnCode`.
#' @param start_datetime Start datetime for `eqsampls.CollectDateTime`.
#' @param end_datetime End datetime for `eqsampls.CollectDateTime`.
#' @param EQpath File path to the target EQWin database. Used to open an
#'   Access connection when `EQCon` is `NULL` and, unless `EQsource_id` is
#'   provided, to identify the source database in sample import IDs.
#' @param key Import mapping source code or key path. Defaults to the stable
#'   EQWin source code used by [upsertImportParameterMappings()].
#' @param con AquaCache connection.
#' @param EQCon Optional existing Access database connection, primarily used by
#'   [getNewDiscrete()] and [synchronize_discrete()] to reuse connections for
#'   sample series that share the same `EQpath`.
#' @param EQsource_id Stable identifier for the EQWin database being imported.
#'   This value is combined with EQWin `SampleId` in
#'   `discrete.samples.import_source_id` so samples from different EQWin
#'   databases do not collide. Defaults to the normalized `EQpath` when an
#'   `EQpath` is supplied, otherwise `"EQWin"` for caller-managed connections.
#' @param tz Time zone used to interpret EQWin collection datetimes.
#' @param unknown_time_local Local-time value assigned to collection datetimes
#'   stored by EQWin as midnight, which commonly indicates an unknown time.
#' @param media_id Optional AquaCache media ID override.
#' @param collection_method Optional AquaCache collection method ID override.
#' @param sample_type Optional AquaCache sample type ID override.
#'
#' @return A list of samples and associated results. Returns an empty list when
#'   no mapped data are found.
#' @export
downloadEQWin <- function(
  location,
  start_datetime,
  end_datetime = Sys.time(),
  EQpath = NULL,
  key = "EQWin",
  con = NULL,
  EQCon = NULL,
  EQsource_id = NULL,
  tz = "MST",
  unknown_time_local = "12:00:00",
  media_id = NULL,
  collection_method = NULL,
  sample_type = NULL
) {
  start_datetime <- eqwin_as_utc(start_datetime, date_end = FALSE)
  end_datetime <- eqwin_as_utc(end_datetime, date_end = TRUE)

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  DBI::dbExecute(con, "SET timezone = 'UTC'")

  if (is.null(EQCon)) {
    if (is.null(EQpath) || is.na(EQpath) || !nzchar(EQpath)) {
      stop("Provide either 'EQCon' or 'EQpath'.")
    }
    EQCon <- AccessConnect(EQpath, silent = TRUE)
    on.exit(DBI::dbDisconnect(EQCon), add = TRUE)
  }
  eqwin_import_source <- eqwin_source_identifier(EQpath, EQsource_id)

  mapping <- import_mapping_load_db(con, key)
  if (is.null(mapping)) {
    mapping <- eqwin_load_key_from_file(con, key)
  }

  station <- DBI::dbGetQuery(
    EQCon,
    paste0(
      "SELECT StnId, StnCode, StnDesc, StnType
       FROM eqstns
       WHERE StnCode = '",
      eqwin_access_escape(location),
      "'"
    )
  )
  if (nrow(station) == 0) {
    stop("No EQWin station found for location '", location, "'.")
  }
  if (nrow(station) > 1) {
    stop("More than one EQWin station found for location '", location, "'.")
  }

  defaults <- eqwin_discrete_defaults(
    con = con,
    stn_type = station$StnType[[1]],
    media_id = media_id,
    collection_method = collection_method,
    sample_type = sample_type
  )

  start_local <- eqwin_utc_to_local_access(start_datetime, tz)
  end_local <- eqwin_utc_to_local_access(end_datetime, tz)
  samples <- DBI::dbGetQuery(
    EQCon,
    paste0(
      "SELECT SampleId, SampleNo, StnId, CollectDateTime, SampleClass,
              SampleMatrixCode, SampleComments
       FROM eqsampls
       WHERE StnId = ",
      station$StnId[[1]],
      " AND CollectDateTime >= #",
      start_local,
      "# AND CollectDateTime <= #",
      end_local,
      "#"
    )
  )
  if (nrow(samples) == 0) {
    return(list())
  }

  sample_ids <- paste(samples$SampleId, collapse = ",")
  results <- DBI::dbGetQuery(
    EQCon,
    paste0(
      "SELECT
          d.SampleId,
          d.ParamId,
          d.Result,
          d.ResultCode,
          d.ProtocolId,
          d.LabId,
          d.LabSampleNo,
          d.LMDL,
          d.ConfLimit,
          d.ResultQuality,
          d.DetailComment,
          p.ParamCode,
          p.ParamDesc,
          p.Units
       FROM eqdetail AS d
       INNER JOIN eqparams AS p ON d.ParamId = p.ParamId
       WHERE d.SampleId IN (",
      sample_ids,
      ")"
    )
  )
  if (nrow(results) == 0) {
    return(list())
  }

  samples <- data.table::as.data.table(samples)
  results <- data.table::as.data.table(results)
  result_conditions <- DBI::dbGetQuery(
    con,
    "SELECT result_condition_id, result_condition FROM discrete.result_conditions;"
  )
  below_detection <- result_conditions[
    grep(
      "below detection",
      result_conditions$result_condition,
      ignore.case = TRUE
    ),
    "result_condition_id"
  ][[1]]
  above_detection <- result_conditions[
    grep(
      "above detection",
      result_conditions$result_condition,
      ignore.case = TRUE
    ),
    "result_condition_id"
  ][[1]]

  out <- vector("list", nrow(samples))
  out_i <- 0L
  for (i in seq_len(nrow(samples))) {
    sample_row <- samples[i]
    sample_results <- results[SampleId == sample_row$SampleId[[1]]]
    if (nrow(sample_results) == 0) {
      next
    }

    result_rows <- vector("list", nrow(sample_results))
    result_i <- 0L
    for (j in seq_len(nrow(sample_results))) {
      source_row <- sample_results[j]
      param_row <- import_mapping_resolve_match(
        mapping,
        list(
          input_param = source_row$ParamCode[[1]],
          ParamDesc = source_row$ParamDesc[[1]],
          input_unit = source_row$Units[[1]]
        )
      )
      if (is.null(param_row) || is.na(param_row$parameter_id[[1]])) {
        next
      }

      parsed <- eqwin_parse_result(
        source_row$Result[[1]],
        conversion = param_row$conversion[[1]],
        result_offset = param_row$result_offset[[1]],
        below_detection = below_detection,
        above_detection = above_detection
      )
      if (is.null(parsed)) {
        warning(
          "Skipping non-numeric EQWin result for SampleId ",
          source_row$SampleId[[1]],
          ", ParamCode ",
          source_row$ParamCode[[1]],
          "."
        )
        next
      }

      result_i <- result_i + 1L
      result_rows[[result_i]] <- data.frame(
        result_type = param_row$result_type[[1]],
        parameter_id = param_row$parameter_id[[1]],
        sample_fraction_id = param_row$sample_fraction_id[[1]],
        result = parsed$result,
        result_condition = parsed$result_condition,
        result_condition_value = parsed$result_condition_value,
        result_value_type = param_row$result_value_type[[1]],
        result_speciation_id = param_row$result_speciation_id[[1]],
        matrix_state_id = param_row$matrix_state_id[[1]],
        note = eqwin_collapse_note(c(
          source_row$ResultCode[[1]],
          source_row$ResultQuality[[1]],
          source_row$DetailComment[[1]]
        )),
        stringsAsFactors = FALSE
      )
    }

    if (result_i == 0L) {
      next
    }
    result_rows <- data.table::rbindlist(
      result_rows[seq_len(result_i)],
      fill = TRUE
    )
    result_rows <- eqwin_drop_incomplete_required_targets(con, result_rows)
    if (nrow(result_rows) == 0) {
      next
    }
    sample_type_i <- eqwin_sample_type(
      sample_class = sample_row$SampleClass[[1]],
      sample_no = sample_row$SampleNo[[1]],
      sample_comments = sample_row$SampleComments[[1]],
      defaults = defaults
    )

    out_i <- out_i + 1L
    out[[out_i]] <- list(
      sample = data.frame(
        media_id = defaults$media_id,
        datetime = eqwin_collect_datetime_to_utc(
          sample_row$CollectDateTime[[1]],
          tz = tz,
          unknown_time_local = unknown_time_local
        ),
        collection_method = defaults$collection_method,
        sample_type = sample_type_i,
        import_source_id = paste0(
          eqwin_import_source,
          "-",
          as.character(sample_row$SampleId[[1]])
        ),
        note = eqwin_collapse_note(c(
          sample_row$SampleNo[[1]],
          paste0("EQWin SampleClass: ", sample_row$SampleClass[[1]]),
          sample_row$SampleComments[[1]]
        )),
        stringsAsFactors = FALSE
      ),
      results = as.data.frame(result_rows)
    )
  }

  if (out_i == 0L) {
    return(list())
  }
  out[seq_len(out_i)]
}

eqwin_source_identifier <- function(EQpath, EQsource_id) {
  if (!is.null(EQsource_id) && !is.na(EQsource_id) && nzchar(EQsource_id)) {
    return(as.character(EQsource_id))
  }
  if (!is.null(EQpath) && !is.na(EQpath) && nzchar(EQpath)) {
    path <- tryCatch(
      normalizePath(EQpath, winslash = "/", mustWork = FALSE),
      error = function(e) as.character(EQpath)
    )
    return(path)
  }
  "EQWin"
}

eqwin_connection_cache_new <- function() {
  cache <- new.env(parent = emptyenv())
  cache$connections <- list()
  cache
}

eqwin_connection_cache_get <- function(cache, EQpath) {
  if (is.null(EQpath) || length(EQpath) == 0 || is.na(EQpath[[1]]) ||
      !nzchar(EQpath[[1]])) {
    return(NULL)
  }

  path <- tryCatch(
    normalizePath(EQpath[[1]], winslash = "/", mustWork = FALSE),
    error = function(e) EQpath[[1]]
  )
  con <- cache$connections[[path]]
  if (!is.null(con)) {
    if (isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
      return(con)
    }
    cache$connections[[path]] <- NULL
  }

  con <- AccessConnect(path, silent = TRUE)
  if (is.null(con) ||
      !isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
    stop("Could not connect to EQWin database at '", path, "'.")
  }
  cache$connections[[path]] <- con
  con
}

eqwin_connection_cache_disconnect <- function(cache) {
  if (is.null(cache) || is.null(cache$connections)) {
    return(invisible(NULL))
  }

  for (con in cache$connections) {
    if (isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  }
  cache$connections <- list()
  invisible(NULL)
}

eqwin_load_key_from_file <- function(con, key) {
  if (!file.exists(key)) {
    keypath <- system.file("import_keys", key, package = "AquaCache")
    if (keypath == "") {
      keypath <- system.file(
        "import_keys",
        paste0(key, ".xlsx"),
        package = "AquaCache"
      )
    }
    if (keypath == "") {
      keypath <- system.file(
        "import_keys",
        paste0(key, ".csv"),
        package = "AquaCache"
      )
    }
    if (keypath == "") {
      stop(
        "No import mapping rows found for EQWin key/source code '",
        key,
        "', and no matching key file was found."
      )
    }
    key <- keypath
  }

  target_columns <- import_mapping_default_target_columns()
  target_columns$sample_fraction <- c(
    "sample_fraction_id",
    "sample_fraction_AC"
  )
  target_columns$result_speciation <- c(
    "result_speciation_id",
    "result_speciation_AC",
    "result_speciation"
  )
  key_data <- import_mapping_read_input(key)
  if ("ignore" %in% names(key_data)) {
    ignore <- import_mapping_as_logical(key_data$ignore)
    key_data <- key_data[is.na(ignore) | !ignore, ]
  }
  key_value_missing <- function(x) {
    if (is.null(x)) {
      return(TRUE)
    }
    out <- is.na(x)
    if (is.character(x)) {
      trimmed <- trimws(x)
      out <- out | !nzchar(trimmed) | toupper(trimmed) %in% c("NA", "NULL")
    }
    out
  }
  if ("sample_fraction_AC" %in% names(key_data)) {
    if (!("sample_fraction_id" %in% names(key_data))) {
      key_data$sample_fraction_id <- NA
    }
    use_ac_fraction <- key_value_missing(key_data$sample_fraction_id) &
      !key_value_missing(key_data$sample_fraction_AC)
    key_data$sample_fraction_id[use_ac_fraction] <- key_data$sample_fraction_AC[use_ac_fraction]
  }
  if ("result_speciation_AC" %in% names(key_data)) {
    if (!("result_speciation_id" %in% names(key_data))) {
      key_data$result_speciation_id <- NA
    }
    use_ac_speciation <- key_value_missing(key_data$result_speciation_id) &
      !key_value_missing(key_data$result_speciation_AC)
    key_data$result_speciation_id[use_ac_speciation] <- key_data$result_speciation_AC[use_ac_speciation]
  }
  mapping <- import_mapping_resolve_targets(
    con,
    data.table::as.data.table(key_data),
    target_columns = target_columns
  )
  mapping[,
    source_match_values := lapply(
      seq_len(.N),
      function(i) {
        list(
          input_param = as.character(input_param[[i]]),
          ParamDesc = as.character(ParamDesc[[i]]),
          input_unit = as.character(input_unit[[i]])
        )
      }
    )
  ]
  mapping[, source_match_size := 3L]
  mapping[, import_mapping_id := seq_len(.N)]
  if (!("priority" %in% names(mapping))) {
    mapping[, priority := 100L]
  }
  mapping
}

eqwin_as_utc <- function(x, date_end) {
  if (inherits(x, "Date")) {
    x <- as.POSIXct(x, tz = "UTC")
    if (date_end) {
      x <- x + 60 * 60 * 23 + 60 * 59 + 59
    }
    return(x)
  }
  if (inherits(x, "character")) {
    if (nchar(x) == 10) {
      x <- as.POSIXct(x, tz = "UTC")
      if (date_end) {
        x <- x + 60 * 60 * 23 + 60 * 59 + 59
      }
      return(x)
    }
    return(as.POSIXct(x, tz = "UTC"))
  }
  if (inherits(x, "POSIXct")) {
    attr(x, "tzone") <- "UTC"
    return(x)
  }
  stop("Datetime values must be Date, character, or POSIXct.")
}

eqwin_utc_to_local_access <- function(x, tz) {
  format(as.POSIXct(x, tz = "UTC"), tz = tz, format = "%Y-%m-%d %H:%M:%S")
}

eqwin_collect_datetime_to_utc <- function(x, tz, unknown_time_local) {
  x_chr <- as.character(x)
  if (nchar(x_chr) == 10) {
    x_chr <- paste(x_chr, "00:00:00")
  }
  if (grepl(" 00:00:00$", x_chr)) {
    x_chr <- sub(" 00:00:00$", paste0(" ", unknown_time_local), x_chr)
  }
  out <- as.POSIXct(x_chr, tz = tz)
  attr(out, "tzone") <- "UTC"
  out
}

eqwin_access_escape <- function(x) {
  gsub("'", "''", as.character(x), fixed = TRUE)
}

eqwin_discrete_defaults <- function(
  con,
  stn_type,
  media_id,
  collection_method,
  sample_type
) {
  stn_type <- toupper(trimws(as.character(stn_type)))
  media_label <- if (stn_type %in% c("GW", "GROUNDWATER")) {
    "groundwater"
  } else {
    "surface water"
  }

  if (is.null(media_id)) {
    media_id <- DBI::dbGetQuery(
      con,
      "SELECT media_id FROM public.media_types WHERE media_type = $1;",
      params = list(media_label)
    )$media_id[[1]]
  }
  if (is.null(collection_method)) {
    method_label <- if (identical(media_label, "groundwater")) {
      "Pump"
    } else {
      "Water Bottle (direct fill)"
    }
    collection_method <- DBI::dbGetQuery(
      con,
      "SELECT collection_method_id
       FROM discrete.collection_methods
       WHERE collection_method = $1;",
      params = list(method_label)
    )$collection_method_id[[1]]
  }
  if (is.null(sample_type)) {
    sample_type <- DBI::dbGetQuery(
      con,
      "SELECT sample_type_id
       FROM discrete.sample_types
       WHERE sample_type = 'sample-routine';"
    )$sample_type_id[[1]]
  }
  sample_type_replicate <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE sample_type = 'QC-sample-other';"
  )
  if (nrow(sample_type_replicate) == 0) {
    sample_type_replicate <- DBI::dbGetQuery(
      con,
      "SELECT sample_type_id
       FROM discrete.sample_types
       WHERE sample_type = 'QC-sample-field replicate';"
    )
  }
  sample_type_field_blank <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE sample_type = 'QC-sample-field blank';"
  )
  sample_type_trip_blank <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE sample_type = 'QC-sample-trip blank';"
  )
  sample_type_lab_blank <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE sample_type = 'QC-sample-lab blank';"
  )
  sample_type_other <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE sample_type = 'sample-other';"
  )
  sample_type_unknown <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM discrete.sample_types
     WHERE sample_type = 'unknown';"
  )
  if (nrow(sample_type_replicate) == 0) {
    sample_type_replicate <- NA_integer_
  } else {
    sample_type_replicate <- sample_type_replicate$sample_type_id[[1]]
  }
  if (nrow(sample_type_field_blank) == 0) {
    sample_type_field_blank <- NA_integer_
  } else {
    sample_type_field_blank <- sample_type_field_blank$sample_type_id[[1]]
  }
  if (nrow(sample_type_trip_blank) == 0) {
    sample_type_trip_blank <- NA_integer_
  } else {
    sample_type_trip_blank <- sample_type_trip_blank$sample_type_id[[1]]
  }
  if (nrow(sample_type_lab_blank) == 0) {
    sample_type_lab_blank <- NA_integer_
  } else {
    sample_type_lab_blank <- sample_type_lab_blank$sample_type_id[[1]]
  }
  if (nrow(sample_type_other) == 0) {
    sample_type_other <- NA_integer_
  } else {
    sample_type_other <- sample_type_other$sample_type_id[[1]]
  }
  if (nrow(sample_type_unknown) == 0) {
    sample_type_unknown <- NA_integer_
  } else {
    sample_type_unknown <- sample_type_unknown$sample_type_id[[1]]
  }

  list(
    media_id = as.integer(media_id),
    collection_method = as.integer(collection_method),
    sample_type = as.integer(sample_type),
    sample_type_replicate = as.integer(sample_type_replicate),
    sample_type_field_blank = as.integer(sample_type_field_blank),
    sample_type_trip_blank = as.integer(sample_type_trip_blank),
    sample_type_lab_blank = as.integer(sample_type_lab_blank),
    sample_type_other = as.integer(sample_type_other),
    sample_type_unknown = as.integer(sample_type_unknown)
  )
}

eqwin_sample_type <- function(sample_class, sample_no, sample_comments, defaults) {
  sample_text <- paste(
    tolower(trimws(as.character(sample_no))),
    tolower(trimws(as.character(sample_comments)))
  )
  sample_class <- toupper(trimws(as.character(sample_class)))

  if (grepl("\\btrip\\s+blank\\b", sample_text)) {
    return(eqwin_default_sample_type(defaults$sample_type_trip_blank, defaults))
  }
  if (grepl("\\blab(oratory)?\\s+blank\\b|\\blabblank\\b", sample_text)) {
    return(eqwin_default_sample_type(defaults$sample_type_lab_blank, defaults))
  }
  if (grepl("\\bfield\\s+blank\\b|\\bblank\\b", sample_text)) {
    return(eqwin_default_sample_type(defaults$sample_type_field_blank, defaults))
  }
  if (identical(sample_class, "D")) {
    return(eqwin_default_sample_type(defaults$sample_type_replicate, defaults))
  }
  if (identical(sample_class, "I")) {
    return(eqwin_default_sample_type(defaults$sample_type_other, defaults))
  }
  if (identical(sample_class, "XX")) {
    return(eqwin_default_sample_type(defaults$sample_type_unknown, defaults))
  }
  defaults$sample_type
}

eqwin_default_sample_type <- function(value, defaults) {
  if (!is.na(value)) {
    return(as.integer(value))
  }
  defaults$sample_type
}

eqwin_parse_result <- function(
  x,
  conversion,
  result_offset,
  below_detection,
  above_detection
) {
  x <- trimws(as.character(x))
  if (!nzchar(x) || is.na(x)) {
    return(NULL)
  }

  condition <- NA_integer_
  if (startsWith(x, "<")) {
    condition <- below_detection
    x <- sub("^<\\s*", "", x)
  } else if (startsWith(x, ">")) {
    condition <- above_detection
    x <- sub("^>\\s*", "", x)
  }

  value <- suppressWarnings(as.numeric(x))
  if (is.na(value)) {
    return(NULL)
  }
  conversion <- as.numeric(conversion)
  result_offset <- as.numeric(result_offset)
  if (is.na(conversion)) {
    conversion <- 1
  }
  if (is.na(result_offset)) {
    result_offset <- 0
  }
  value <- value * conversion + result_offset

  if (is.na(condition)) {
    return(list(
      result = value,
      result_condition = NA_integer_,
      result_condition_value = NA_real_
    ))
  }

  list(
    result = NA_real_,
    result_condition = condition,
    result_condition_value = value
  )
}

eqwin_drop_incomplete_required_targets <- function(con, results) {
  params <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT parameter_id, sample_fraction, result_speciation
       FROM public.parameters
       WHERE parameter_id IN (",
      paste(unique(results$parameter_id), collapse = ","),
      ");"
    )
  )
  results <- merge(results, params, by = "parameter_id", all.x = TRUE)
  needs_fraction <- !is.na(results$sample_fraction) &
    as.logical(results$sample_fraction)
  needs_speciation <- !is.na(results$result_speciation) &
    as.logical(results$result_speciation)
  keep <- !(needs_fraction & is.na(results$sample_fraction_id))
  keep <- keep & !(needs_speciation & is.na(results$result_speciation_id))
  results <- results[keep, ]
  results$sample_fraction <- NULL
  results$result_speciation <- NULL
  results
}

eqwin_collapse_note <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) {
    return(NA_character_)
  }
  paste(unique(x), collapse = "; ")
}
