#' Bring ECCC water quality data to AquaCache
#'
#' @description
#'
#' Brings in water quality data from ECCC long-term monitoring sites and transforms them into the aquacache database format. The data is read from an ECCC water quality .csv file available from their open data portal. The function filters the data for the specified location and datetime range, applies parameter mappings defined in an import key .csv file, and prepares the data for insertion into the aquacache database. The function returns a list of samples and their associated results ready for import.
#'
#' Note that ECCC's results are converted to the AquaCache database parameters and units using the provided key file. Users should ensure that the key file contains accurate mappings for the parameters of interest. In addition, users should verify that the datetime values in the ECCC data file are correctly interpreted, as they may be provided in a local time zone and need to be converted to UTC for proper storage in AquaCache.
#'
#' @param location The location code associated with the ECCC monitoring site. Must be a valid location code in the `SITE_NO` field of the ECCC water quality .csv files.
#' @param file Path (URL) to the ECCC water quality .csv file containing the data to be imported for the specified location.
#' @param key Path to the import key .csv file defining the parameter mappings for ECCC water quality data.
#' @param tz Time zone of the input data (does NOT apply to `start_datetime` or `end_datetime` parameters). This is used to correctly interpret the datetime values in the ECCC data file. Common time zones include "UTC", "MST", "PST", etc.
#' @param start_datetime Start datetime (inclusive) from which to fetch measurements. Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime End datetime (inclusive) to which to fetch measurements. Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param con A connection to the aquacache database, only used if an offset is calculated for an old_loc. If not provided, a connection will be attempted using AquaConnect().
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
  con = NULL
) {
  # location <- "YT09AB0006"
  # file <- "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv"
  # key <- "downloadECCCeq1.csv"
  # start_datetime <- "2020-01-01"
  # end_datetime <- Sys.time()
  # tz = "MST"

  # Load the file and key
  keypath <- system.file(
    "import_keys",
    key,
    package = "AquaCache"
  )
  if (keypath == "") {
    stop(
      "The key you specified cannot be found in this package's inst/import_keys folder. "
    )
  }
  key <- data.table::fread(keypath, stringsAsFactors = FALSE)

  required_columns_key <- c(
    "input_param",
    "input_unit",
    "parameter_id",
    "conversion",
    "result_type",
    "sample_fraction",
    "result_value_type",
    "result_speciation_id"
  )

  missing_columns_key <- setdiff(required_columns_key, colnames(key))

  if (length(missing_columns_key) > 0) {
    stop(
      paste0(
        "The following required columns are missing from the key file: ",
        paste(missing_columns_key, collapse = ", ")
      )
    )
  }

  # ---- Cached download for `file` (URL or local path) ----
  is_url <- grepl("^(https?|ftp)://", file, ignore.case = TRUE)

  if (is_url) {
    cache_dir <- file.path(tempdir(), "AquaCache_ECCC_cache")
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

    # Construct a stable, safe filename. Prefer a short hash if {digest} is available.
    if (requireNamespace("digest", quietly = TRUE)) {
      key_hash <- substr(digest::digest(file, algo = "xxhash64"), 1, 16)
      fname <- paste0("eccc_wq_", key_hash, ".csv")
    } else {
      # Fallback: sanitize the URL into a filename (trim to avoid path length issues)
      safe <- gsub("[^A-Za-z0-9]+", "_", utils::URLdecode(file))
      fname <- paste0(substr(safe, 1, 120), ".csv")
    }

    local_path <- file.path(cache_dir, fname)

    # Download if missing or empty (treat empty as partial/failed prior download)
    if (!file.exists(local_path) || isTRUE(file.info(local_path)$size == 0)) {
      part_path <- paste0(local_path, ".part")
      if (file.exists(part_path)) {
        unlink(part_path)
      }

      # Prefer curl::curl_download for robustness; fall back to utils if curl not available
      tryCatch(
        {
          if (requireNamespace("curl", quietly = TRUE)) {
            curl::curl_download(
              file,
              destfile = part_path,
              mode = "wb",
              quiet = TRUE
            )
          } else {
            utils::download.file(
              file,
              destfile = part_path,
              mode = "wb",
              quiet = TRUE
            )
          }

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
  file <- data.table::fread(file_path_to_read)
  # ---- end cached download block ----

  # file <- data.table::fread(file)

  # Ensure that 'file' and 'key' have the necessary columns
  required_columns_file <- c(
    "SITE_NO",
    "DATE_TIME_HEURE",
    "FLAG_MARQUEUR",
    "VALUE_VALEUR",
    "UNIT_UNITÉ",
    "VARIABLE",
    "SAMPLE_ID_ÉCHANTILLON"
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

  media_id <- DBI::dbGetQuery(
    con,
    "SELECT media_id FROM media_types WHERE media_type = 'surface water';"
  )[1, 1]
  owner_contributor <- DBI::dbGetQuery(
    con,
    "SELECT organization_id FROM organizations WHERE name = 'Environment and Climate Change Canada';"
  )[1, 1]
  if (is.na(owner_contributor)) {
    DBI::dbExecute(
      con,
      "INSERT INTO organizations (name) VALUES ('Environment and Climate Change Canada');"
    )
  }
  sample_type <- DBI::dbGetQuery(
    con,
    "SELECT sample_type_id FROM sample_types WHERE sample_type = 'sample-routine';"
  )[1, 1]
  collect_method <- DBI::dbGetQuery(
    con,
    "SELECT collection_method_id FROM collection_methods WHERE collection_method = 'Water Bottle';"
  )[1, 1]
  result_conditions <- DBI::dbGetQuery(
    con,
    "SELECT result_condition_id, result_condition FROM result_conditions;"
  )
  eccc_lab <- DBI::dbGetQuery(
    con,
    "SELECT lab_id FROM laboratories WHERE lab_name = 'Environment and Climate Change Canada';"
  )[1, 1]
  if (is.na(eccc_lab)) {
    eccc_lab <- DBI::dbGetQuery(
      con,
      "INSERT INTO laboratories (lab_name) VALUES ('Environment and Climate Change Canada') RETURNING lab_id;"
    )[1, 1]
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

  all_samples <- unique(all_results$DATE_TIME_HEURE)

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
      import_source_id = paste(
        unique(subset$SAMPLE_ID_ÉCHANTILLON),
        collapse = ","
      )
      # import_source is added in by getNewDiscrete
    )
    results <- data.frame()
    for (j in seq_len(nrow(subset))) {
      var <- subset$VARIABLE[j]
      if (nchar(var) == 0) {
        next
      }
      param_row <- key[
        key$input_param == var &
          key$input_unit == subset$UNIT_UNITÉ[j],
      ]
      if (nrow(param_row) == 0) {
        warning(paste0(
          "No parameter mapping found for variable '",
          var,
          "' with unit '",
          subset$UNIT_UNITÉ[j],
          "'. Skipping this result."
        ))
        next
      }
      if (is.na(param_row$parameter_id)) {
        if (var != "RESIDUE NONFILTERABLE") {
          warning(paste0(
            "Parameter mapping for variable '",
            var,
            "' with unit '",
            subset$UNIT_UNITÉ[j],
            "' has no parameter_id assigned. Skipping this result."
          ))
        }
        next
      }

      # Isolate the value and apply conversion
      result_value <- as.numeric(subset$VALUE_VALEUR[j]) *
        as.numeric(param_row$conversion[1])

      # Look for '<' or '>' in the FLAG_MARQUEUR to set result_condition and result_condition_value
      flag <- subset$FLAG_MARQUEUR[j]
      result_condition_value <- NA
      result_condition <- NA
      if (flag == "<") {
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
      } else if (flag == ">") {
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

      # Build the result row
      result <- data.frame(
        result_type = param_row$result_type[1],
        parameter_id = param_row$parameter_id[1],
        sample_fraction_id = param_row$sample_fraction[1],
        result = result_value,
        result_condition = result_condition,
        result_condition_value = result_condition_value,
        result_value_type = param_row$result_value_type[1],
        result_speciation_id = param_row$result_speciation_id[1],
        laboratory = eccc_lab
      )
      results <- rbind(results, result)
    }

    samples[[i]] <- list(sample = sample, results = results)
  }

  return(samples)
}
