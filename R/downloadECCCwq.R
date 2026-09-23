#' Bring ECCC water quality data to AquaCache
#'
#' @description
#'
#' Brings in water quality data from ECCC long-term monitoring sites and transforms them into the aquacache database format. The data is read from an ECCC water quality .csv file available from their open data portal. The function filters the data for the specified location and datetime range, applies parameter mappings loaded in `discrete.import_parameter_mappings`, and prepares the data for insertion into the aquacache database. The function returns a list of samples and their associated results ready for import.
#'
#' Note that ECCC's results are converted to the AquaCache database parameters and units using the requested database import key. Users should ensure that the import key has been loaded before importing ECCC data. In addition, users should verify that the datetime values in the ECCC data file are correctly interpreted, as they may be provided in a local time zone and need to be converted to UTC for proper storage in AquaCache.
#'
#' @param location The location code associated with the ECCC monitoring site. Must be a valid location code in the `SITE_NO` field of the ECCC water quality .csv files.
#' @param file Path (URL) to the ECCC water quality .csv file containing the data to be imported for the specified location.
#' @param key Import mapping source code in `discrete.import_sources`.
#' @param tz Time zone of the input data (does NOT apply to `start_datetime` or `end_datetime` parameters). This is used to correctly interpret the datetime values in the ECCC data file. Common time zones include "UTC", "MST", "PST", etc.
#' @param start_datetime Start datetime (inclusive) from which to fetch measurements. Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime End datetime (inclusive) to which to fetch measurements. Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param con A connection to the aquacache database, only used if an offset is calculated for an old_loc. If not provided, a connection will be attempted using AquaConnect().
#' @param warn_unmapped If `TRUE`, warn when an ECCC variable/unit does not
#'   have an import mapping. Set to `FALSE` for automated synchronization
#'   workflows where unmapped source variables are expected.
#' @param mode `"data"` returns samples/results for import. `"missing_mappings"`
#'   returns source variable/unit combinations in the requested data that do not
#'   have a complete database import mapping.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export

downloadECCCwq <- function(
  location,
  file,
  key = "downloadECCCeq1.csv",
  tz,
  start_datetime,
  end_datetime = Sys.time(),
  con = NULL,
  warn_unmapped = interactive(),
  mode = c("data", "missing_mappings")
) {
  warn_unmapped <- isTRUE(as.logical(warn_unmapped))
  mode <- match.arg(mode)

  # ---- Cached download for `file` (URL or local path) ----
  is_url <- grepl("^(https?|ftp)://", file, ignore.case = TRUE)

  if (is_url) {
    cache_dir <- file.path(tempdir(), "AquaCache_ECCC_cache")
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    # Fallback: sanitize the URL into a filename (trim to avoid path length issues)
    safe <- gsub("[^A-Za-z0-9]+", "_", utils::URLdecode(file))
    # Remove the string "long_term_water_quality_monitoring_data" to shorten filename
    safe <- gsub("long_term_water_quality_monitoring_data_", "", safe)
    safe <- gsub(
      "https_data_donnees_az_ec_gc_ca_api_file_path_substances_monitor_",
      "",
      safe
    )
    safe <- gsub("_csv$", "", safe)
    fname <- paste0(safe, ".csv")

    local_path <- file.path(cache_dir, fname)

    # Download if missing or empty (treat empty as partial/failed prior download)
    if (!file.exists(local_path) || isTRUE(file.info(local_path)$size == 0)) {
      part_path <- paste0(local_path, ".part")
      if (file.exists(part_path)) {
        unlink(part_path)
      }

      tryCatch(
        {
          curl::curl_download(
            url = file,
            destfile = part_path,
            mode = "wb",
            quiet = TRUE
          )

          # Basic guard: ensure we didn’t get an empty file
          if (!file.exists(part_path) || file.info(part_path)$size == 0) {
            stop("Downloaded file is empty or missing.")
          }

          # Finalize
          if (file.exists(local_path)) {
            unlink(local_path)
          }
          file.rename(part_path, local_path)
        },
        error = function(e) {
          if (file.exists(part_path)) {
            unlink(part_path)
          }
          stop(
            "Failed to download ECCC water quality file: ",
            conditionMessage(e)
          )
        }
      )
    }

    file_path_to_read <- local_path
  } else {
    # It's a local path already
    file_path_to_read <- file
  }

  # Read the cached/local file
  # encoding should be UTF-8
  file <- data.table::fread(file_path_to_read, encoding = "UTF-8")
  # ---- end cached download block ----

  # Ensure that 'file' and 'key' have the necessary columns
  required_columns_file <- c(
    "SITE_NO",
    "DATE_TIME_HEURE",
    "FLAG_MARQUEUR",
    "VALUE_VALEUR",
    "UNIT_UNIT\u00C9",
    "VARIABLE",
    "SAMPLE_ID_\u00C9CHANTILLON"
  )

  missing_columns_file <- setdiff(required_columns_file, colnames(file))
  if (length(missing_columns_file) > 0) {
    stop(
      paste0(
        "The following required columns are missing from the data file: ",
        paste(missing_columns_file, collapse = ", ")
      )
    )
  }

  # Check parameters and set defaults ########################################
  # Checking start_datetime parameter
  tryCatch(
    {
      if (inherits(start_datetime, "character") & nchar(start_datetime) > 10) {
        #Does not necessarily default to 0 hour.
        start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
      } else if (inherits(start_datetime, "POSIXct")) {
        attr(start_datetime, "tzone") <- "UTC"
      } else if (
        inherits(start_datetime, "Date") |
          (inherits(start_datetime, "character") &
            nchar(start_datetime) == 10)
      ) {
        #defaults to 0 hour
        start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
      } else {
        stop("Parameter start_datetime could not be coerced to POSIXct.")
      }
    },
    error = function(e) {
      stop("Failed to convert parameter start_datetime to POSIXct.")
    }
  )

  # Checking end_datetime parameter
  tryCatch(
    {
      if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) {
        #Does not necessarily default to 0 hour.
        end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      } else if (inherits(end_datetime, "POSIXct")) {
        attr(end_datetime, "tzone") <- "UTC"
      } else if (
        inherits(end_datetime, "Date") |
          (inherits(end_datetime, "character") & nchar(end_datetime) == 10)
      ) {
        #defaults to very end of day
        end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
        end_datetime <- end_datetime + 60 * 60 * 23.9999
      } else {
        stop("Parameter end_datetime could not be coerced to POSIXct.")
      }
    },
    error = function(e) {
      stop("Failed to convert parameter end_datetime to POSIXct.")
    }
  )

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  DBI::dbExecute(con, "SET timezone = 'UTC'")

  key_source <- key
  key <- import_mapping_load_db(con, key_source)
  if (is.null(key)) {
    stop(
      "No database import mapping rows found for ECCC key/source code '",
      key_source,
      "'. Load the key into discrete.import_parameter_mappings before calling downloadECCCwq()."
    )
  }

  # pre-processing
  file$DATE_TIME_HEURE <- as.POSIXct(file$DATE_TIME_HEURE, tz = tz)
  # Convert to UTC for storage in AquaCache
  attr(file$DATE_TIME_HEURE, "tzone") <- "UTC"

  # Now get the new data and return it ############################################
  all_results <- file[
    file$SITE_NO == location &
      file$DATE_TIME_HEURE >= start_datetime &
      file$DATE_TIME_HEURE <= end_datetime,
  ]

  if (identical(mode, "missing_mappings")) {
    return(downloadECCCwq_missing_mappings(all_results, key))
  }
  all_samples <- unique(all_results$DATE_TIME_HEURE)

  media_id <- DBI::dbGetQuery(
    con,
    "SELECT media_id FROM public.media_types WHERE media_type = 'surface water';"
  )[1, 1]
  owner_contributor <- DBI::dbGetQuery(
    con,
    "SELECT organization_id FROM public.organizations WHERE name = 'Environment and Climate Change Canada';"
  )[1, 1]
  if (is.na(owner_contributor)) {
    DBI::dbExecute(
      con,
      "INSERT INTO public.organizations (name) VALUES ('Environment and Climate Change Canada');"
    )
  }
  sample_type <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id FROM discrete.sample_types WHERE sample_type = 'sample-routine';"
  )[1, 1]
  collect_method <- DBI::dbGetQuery(
    con,
    "SELECT collection_method_id FROM discrete.collection_methods WHERE collection_method LIKE ('Water Bottle%');"
  )[1, 1]
  result_conditions <- DBI::dbGetQuery(
    con,
    "SELECT result_condition_id, result_condition FROM discrete.result_conditions;"
  )
  eccc_lab <- DBI::dbGetQuery(
    con,
    "SELECT lab_id FROM discrete.laboratories WHERE lab_name = 'Environment and Climate Change Canada';"
  )[1, 1]
  if (is.na(eccc_lab)) {
    eccc_lab <- DBI::dbGetQuery(
      con,
      "INSERT INTO discrete.laboratories (lab_name) VALUES ('Environment and Climate Change Canada') RETURNING lab_id;"
    )[1, 1]
  }

  # Build the list required by getNewDiscrete
  samples <- list()
  for (i in seq_along(all_samples)) {
    subset <- all_results[all_results$DATE_TIME_HEURE == all_samples[i], ]
    sample <- data.frame(
      media_id = media_id,
      datetime = all_samples[i],
      collection_method = collect_method,
      sample_type = sample_type,
      owner = owner_contributor,
      contributor = owner_contributor,
      external_sample_id = paste(
        unique(subset[["SAMPLE_ID_\u00C9CHANTILLON"]]),
        collapse = ","
      ),
      import_source_id = unique(key$import_source_id)[[1]]
      # source_adapter_function is added by getNewDiscrete
    )
    results <- data.frame()
    for (j in seq_len(nrow(subset))) {
      var <- subset$VARIABLE[j]
      if (nchar(var) == 0) {
        next
      }
      input_unit <- subset[["UNIT_UNIT\u00C9"]][j]
      param_row <- import_mapping_resolve_match(
        key,
        list(
          input_param = var,
          input_unit = input_unit
        )
      )
      if (is.null(param_row) || nrow(param_row) == 0) {
        if (warn_unmapped) {
          warning(paste0(
            "No parameter mapping found for variable '",
            var,
            "' with unit '",
            input_unit,
            "'. Skipping this result."
          ))
        }
        next
      }
      if (is.na(param_row$parameter_id[[1]])) {
        if (var != "RESIDUE NONFILTERABLE") {
          if (warn_unmapped) {
            warning(paste0(
              "Parameter mapping for variable '",
              var,
              "' with unit '",
              input_unit,
              "' has no parameter_id assigned. Skipping this result."
            ))
          }
        }
        next
      }

      # Isolate the value and apply conversion
      result_value <- as.numeric(subset$VALUE_VALEUR[j]) *
        as.numeric(param_row$conversion[1]) +
        as.numeric(param_row$result_offset[1])

      # Look for '<' or '>' in the FLAG_MARQUEUR to set result_condition and result_condition_value
      flag <- subset$FLAG_MARQUEUR[j]
      result_condition_value <- NA
      result_condition <- NA
      if (!is.na(flag) && flag == "<") {
        result_condition <- result_conditions[
          grep(
            "below detection",
            result_conditions$result_condition,
            ignore.case = TRUE
          ),
          "result_condition_id"
        ]
        result_condition_value <- result_value
        result_value <- NA
      } else if (!is.na(flag) && flag == ">") {
        result_condition <- result_conditions[
          grep(
            "above detection",
            result_conditions$result_condition,
            ignore.case = TRUE
          ),
          "result_condition_id"
        ]
        result_condition_value <- result_value
        result_value <- NA
      } else if (flag != "") {
        warning(paste0(
          "Unrecognized value for FLAG_MARQUEUR '",
          flag,
          "' for variable '",
          var,
          "' of sample dated on '",
          as.character(all_samples[i]),
          " . Skipping this result."
        ))
        next
      }

      matrix_state_id <- param_row$matrix_state_id[1]
      matrix_state <- if ("matrix_state" %in% names(param_row)) {
        param_row$matrix_state[1]
      } else {
        NA_character_
      }

      # Build the result row
      result <- data.frame(
        result_type = param_row$result_type[1],
        parameter_id = param_row$parameter_id[1],
        sample_fraction_id = param_row$sample_fraction_id[1],
        result = result_value,
        result_condition = result_condition,
        result_condition_value = result_condition_value,
        result_value_type = param_row$result_value_type[1],
        result_speciation_id = param_row$result_speciation_id[1],
        laboratory = eccc_lab,
        matrix_state_id = matrix_state_id,
        matrix_state = matrix_state,
        stringsAsFactors = FALSE
      )
      results <- rbind(results, result)
    }

    samples[[i]] <- list(sample = sample, results = results)
  }

  return(samples)
}

downloadECCCwq_missing_mappings <- function(all_results, mapping) {
  output_cols <- c(
    "input_param",
    "input_unit",
    "n_results",
    "n_samples",
    "first_datetime",
    "last_datetime",
    "example_sample_id",
    "import_mapping_id",
    "missing_reason",
    "mapping_error",
    "source_match"
  )
  empty_report <- data.table::data.table(
    input_param = character(),
    input_unit = character(),
    n_results = integer(),
    n_samples = integer(),
    first_datetime = as.POSIXct(character(), tz = "UTC"),
    last_datetime = as.POSIXct(character(), tz = "UTC"),
    example_sample_id = character(),
    import_mapping_id = integer(),
    missing_reason = character(),
    mapping_error = character(),
    source_match = character()
  )

  if (nrow(all_results) == 0L) {
    return(empty_report)
  }

  first_non_missing <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0L) {
      return(NA_character_)
    }
    x[[1]]
  }
  min_datetime <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      return(as.POSIXct(NA, tz = "UTC"))
    }
    min(x)
  }
  max_datetime <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      return(as.POSIXct(NA, tz = "UTC"))
    }
    max(x)
  }
  source_column <- function(x, expected, prefix) {
    cols <- names(x)
    exact <- match(expected, cols)
    if (!is.na(exact)) {
      return(exact)
    }
    matches <- grep(paste0("^", prefix), cols, value = TRUE)
    if (length(matches) == 1L) {
      return(match(matches[[1]], cols))
    }
    stop("Could not resolve expected ECCC source column '", expected, "'.")
  }

  source <- data.table::copy(data.table::as.data.table(all_results))
  sample_col <- source_column(source, "SAMPLE_ID_\u00C9CHANTILLON", "SAMPLE_ID_")
  unit_col <- source_column(source, "UNIT_UNIT\u00C9", "UNIT_UNIT")
  source[, eccc_sample_id := as.character(source[[sample_col]])]
  source[, input_param := data.table::fifelse(is.na(VARIABLE), "", as.character(VARIABLE))]
  source[, input_unit := data.table::fifelse(
    is.na(source[[unit_col]]),
    "",
    as.character(source[[unit_col]])
  )]
  source <- source[nzchar(input_param)]
  if (nrow(source) == 0L) {
    return(empty_report)
  }

  report <- source[, .(
    n_results = .N,
    n_samples = data.table::uniqueN(eccc_sample_id),
    first_datetime = min_datetime(DATE_TIME_HEURE),
    last_datetime = max_datetime(DATE_TIME_HEURE),
    example_sample_id = first_non_missing(eccc_sample_id)
  ), by = .(input_param, input_unit)]

  statuses <- lapply(seq_len(nrow(report)), function(i) {
    import_mapping_match_status(
      mapping,
      list(
        input_param = report$input_param[[i]],
        input_unit = report$input_unit[[i]]
      )
    )
  })
  report[, missing_reason := vapply(statuses, `[[`, character(1), "status")]
  report[, mapping_error := vapply(statuses, function(x) x$message, character(1))]
  report[, import_mapping_id := vapply(
    statuses,
    function(x) {
      if (is.null(x$mapping)) {
        return(NA_integer_)
      }
      as.integer(x$mapping$import_mapping_id[[1]])
    },
    integer(1)
  )]
  report <- report[missing_reason != "mapped"]
  if (nrow(report) == 0L) {
    return(empty_report)
  }

  report[, source_match := vapply(
    seq_len(.N),
    function(i) {
      jsonlite::toJSON(
        list(
          input_param = report$input_param[[i]],
          input_unit = report$input_unit[[i]]
        ),
        auto_unbox = TRUE,
        null = "null"
      )
    },
    character(1)
  )]
  data.table::setorderv(report, c("missing_reason", "input_param", "input_unit"))
  report[, ..output_cols]
}
