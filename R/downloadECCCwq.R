#' Bring ECCC water quality data to AquaCache
#'
#' @description
#'
#' Brings in water quality data from ECCC long-term monitoring sites and transforms them into the aquacache database format.
#'
#' @param location The location code associated with the ECCC monitoring site. Must be a valid location code in the `SITE_NO` field of the ECCC water quality .csv files.
#' @param file Path (URL) to the ECCC water quality .csv file containing the data to be imported for the specified location.
#' @param key Path to the import key .csv file defining the parameter mappings for ECCC water quality data.
#' @param start_datetime Start datetime (inclusive) from which to fetch measurements. Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime End datetime (inclusive) to which to fetch measurements. Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param con A connection to the aquacache database, only used if an offset is calculated for an old_loc. If not provided, a connection will be attempted using AquaConnect().
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export

downloadECCCwq <- function(
  location,
  file,
  key = system.file("import_keys", "ECCCwq.csv", package = "AquaCache"),
  start_datetime,
  end_datetime = Sys.time(),
  con = NULL
) {
  # location <- "YT09AB0006"
  # file <- "https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv"
  # key <- system.file("import_keys", "downloadECCCeq1.csv", package = "AquaCache")
  # start_datetime <- "2020-01-01"
  # end_datetime <- Sys.time()

  # Load the file and key
  file <- data.table::fread(file)
  key <- data.table::fread(key, stringsAsFactors = FALSE)

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
          (inherits(start_datetime, "character") & nchar(start_datetime) == 10)
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

  location_id <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT location_id FROM locations WHERE location = '",
      location,
      "';"
    )
  )[1, 1]
  if (is.na(location_id)) {
    stop(
      paste0(
        "Location '",
        location,
        "' not found in aquacache database. Please create the location before importing data."
      )
    )
  }

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

  # pre-processing
  # Presumably comes from ECCC in local time zone MST (UTC-7)
  file$DATE_TIME_HEURE <- as.POSIXct(file$DATE_TIME_HEURE, tz = "MST")
  # Convert to UTC for storage in AquaCache
  attr(file$DATE_TIME_HEURE, "tzone") <- "UTC"

  # Now get the new data and return it ############################################
  all_results <- file[
    file$SITE_NO == location &
      as.POSIXct(file$DATE_TIME, tz = "UTC") >= start_datetime &
      as.POSIXct(file$DATE_TIME, tz = "UTC") <= end_datetime,
  ]

  all_samples <- unique(all_results$DATE_TIME_HEURE)

  # Build the list required by getNewDiscrete
  samples <- list()
  for (i in seq_along(all_samples)) {
    subset <- all_results[all_results$DATE_TIME_HEURE == all_samples[i], ]
    sample <- data.frame(
      location_id = location_id,
      media_id = media_id,
      datetime = all_samples[i],
      collection_method = collect_method,
      sample_type = sample_type,
      owner = owner_contributor,
      contributor = owner_contributor,
      import_source_id = unique(subset$SAMPLE_ID_ÉCHANTILLON)
    )
    results <- data.frame()

    samples[[i]] <- list(sample = sample, results = results)
  }

  return(samples)
}
