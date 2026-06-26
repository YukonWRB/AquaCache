#' Bring EQWin water quality data into AquaCache
#'
#' @description
#' Brings water quality data from an EQWin Access database into the list format
#' expected by [getNewDiscrete()] and [synchronize_discrete()]. Parameter
#' mapping is read from `discrete.import_parameter_mappings` when the requested
#' key/source code has been loaded with [upsertImportParameterMappings()]. EQWin
#' `udf_*` fields and `eqsampls.LastModified` are intentionally not used.
#'
#' @param location EQWin station code from `eqstns.StnCode`.
#' @param start_datetime Start datetime for `eqsampls.CollectDateTime`.
#' @param end_datetime End datetime for `eqsampls.CollectDateTime`.
#' @param EQpath File path to the target EQWin database. Used only when `EQCon`
#'   is `NULL`.
#' @param key Import mapping source code or key path. Defaults to the working
#'   EQWin source code used by [upsertImportParameterMappings()].
#' @param con AquaCache connection.
#' @param EQCon Optional existing Access database connection.
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
  key = "EQWin_key_working_copy",
  con = NULL,
  EQCon = NULL,
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
    "SELECT result_condition_id, result_condition FROM result_conditions;"
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
    sample_class <- as.character(sample_row$SampleClass[[1]])
    sample_type_i <- if (identical(sample_class, "D")) {
      defaults$sample_type_replicate
    } else {
      defaults$sample_type
    }
    if (is.na(sample_type_i)) {
      sample_type_i <- defaults$sample_type
    }

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
          as.character(sample_row$SampleId[[1]]),
          "-",
          EQpath
        ),
        note = eqwin_collapse_note(c(
          sample_row$SampleNo[[1]],
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
  key_data <- import_mapping_read_input(key)
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
      "SELECT media_id FROM media_types WHERE media_type = $1;",
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
       FROM collection_methods
       WHERE collection_method = $1;",
      params = list(method_label)
    )$collection_method_id[[1]]
  }
  if (is.null(sample_type)) {
    sample_type <- DBI::dbGetQuery(
      con,
      "SELECT sample_type_id
       FROM sample_types
       WHERE sample_type = 'sample-routine';"
    )$sample_type_id[[1]]
  }
  sample_type_replicate <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id
     FROM sample_types
     WHERE sample_type = 'QC-sample-field replicate';"
  )
  if (nrow(sample_type_replicate) == 0) {
    sample_type_replicate <- NA_integer_
  } else {
    sample_type_replicate <- sample_type_replicate$sample_type_id[[1]]
  }

  list(
    media_id = as.integer(media_id),
    collection_method = as.integer(collection_method),
    sample_type = as.integer(sample_type),
    sample_type_replicate = as.integer(sample_type_replicate)
  )
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
       FROM parameters
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
