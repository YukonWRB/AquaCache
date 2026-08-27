#' Store raw transmission payloads
#'
#' Provider-neutral storage used by transmission source adapters. Adapters are
#' responsible for separating a provider response into individual payloads and
#' supplying each payload's transmission time before calling this function.
#'
#' @keywords internal
#' @noRd
transmission_store_payloads <- function(
  con,
  transmission_setup_ids,
  payloads,
  source_server = NULL,
  source_metadata = list()
) {
  setup_ids <- unique(as.integer(transmission_setup_ids))
  if (length(setup_ids) == 0L || anyNA(setup_ids) || any(setup_ids <= 0L)) {
    stop("No valid transmission setup was available for payload storage.")
  }

  payloads <- data.table::as.data.table(payloads)
  required_columns <- c("transmission_datetime", "payload_text")
  if (!all(required_columns %in% names(payloads))) {
    stop(
      "Stored payloads require transmission_datetime and payload_text columns."
    )
  }
  if (nrow(payloads) == 0L) {
    return(list(
      transmissions_archived = 0L,
      transmissions_inserted = 0L
    ))
  }

  if (
    !inherits(payloads$transmission_datetime, c("POSIXct", "Date")) &&
      !is.character(payloads$transmission_datetime)
  ) {
    stop(
      "transmission_datetime must contain POSIXct, Date, or character values."
    )
  }
  transmission_datetime <- as.POSIXct(
    payloads$transmission_datetime,
    tz = "UTC"
  )
  payload_text <- as.character(payloads$payload_text)
  if (anyNA(transmission_datetime)) {
    stop("Every stored payload requires a valid transmission_datetime.")
  }
  if (anyNA(payload_text) || any(!nzchar(payload_text))) {
    stop("Every stored payload requires non-empty payload_text.")
  }
  if (is.null(source_server)) {
    source_server <- NA_character_
  } else {
    source_server <- as.character(source_server)
    if (length(source_server) != 1L || is.na(source_server)) {
      stop("source_server must be NULL or one non-missing character value.")
    }
  }
  if (!is.list(source_metadata)) {
    stop("source_metadata must be a list.")
  }
  if (
    length(source_metadata) > 0L &&
      (is.null(names(source_metadata)) || any(!nzchar(names(source_metadata))))
  ) {
    stop("source_metadata must be an empty or named list.")
  }

  payload_rows <- data.table::data.table(
    transmission_datetime = format(
      transmission_datetime,
      "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC"
    ),
    payload_text = payload_text
  )
  payload_json <- as.character(jsonlite::toJSON(
    payload_rows,
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null"
  ))
  metadata_json <- if (length(source_metadata) == 0L) {
    "{}"
  } else {
    as.character(jsonlite::toJSON(
      source_metadata,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ))
  }
  inserted <- 0L

  for (setup_id in setup_ids) {
    stored <- DBI::dbGetQuery(
      con,
      "INSERT INTO continuous.transmission_payloads (
         transmission_setup_id,
         transmission_datetime,
         payload_text,
         source_server,
         source_metadata
       )
       SELECT
         $1,
         payload.transmission_datetime,
         payload.payload_text,
         $2,
         $3::jsonb
       FROM jsonb_to_recordset($4::jsonb) AS payload(
         transmission_datetime timestamptz,
         payload_text text
       )
       ON CONFLICT ON CONSTRAINT transmission_payloads_identity_key
       DO NOTHING
       RETURNING transmission_payload_id",
      params = list(
        setup_id,
        source_server,
        metadata_json,
        payload_json
      )
    )
    inserted <- inserted + nrow(stored)
  }

  list(
    transmissions_archived = nrow(payloads) * length(setup_ids),
    transmissions_inserted = inserted
  )
}

#' Retrieve raw transmission payloads
#'
#' Provider-neutral retrieval used by source adapters that implement stored
#' replay. Parsing and duplicate-observation resolution remain the adapter's
#' responsibility.
#'
#' @keywords internal
#' @noRd
transmission_fetch_payloads <- function(
  con,
  transmission_setup_ids,
  since,
  until
) {
  setup_ids <- unique(as.integer(transmission_setup_ids))
  if (length(setup_ids) == 0L || anyNA(setup_ids) || any(setup_ids <= 0L)) {
    stop("No valid transmission setup was available for payload replay.")
  }

  valid_time_type <- function(value) {
    inherits(value, c("POSIXct", "Date")) || is.character(value)
  }
  if (!valid_time_type(since) || !valid_time_type(until)) {
    stop("since and until must be POSIXct, Date, or character values.")
  }
  since <- as.POSIXct(since, tz = "UTC")
  until <- as.POSIXct(until, tz = "UTC")
  if (
    length(since) != 1L ||
      length(until) != 1L ||
      is.na(since) ||
      is.na(until) ||
      since >= until
  ) {
    stop("Stored payload replay requires a valid since/until time window.")
  }

  dbGetQueryDT(
    con,
    paste0(
      "SELECT
         transmission_payload_id,
         transmission_datetime,
         payload_text
       FROM continuous.transmission_payloads
       WHERE transmission_setup_id IN (",
      paste(setup_ids, collapse = ", "),
      ")
         AND transmission_datetime >= $1
         AND transmission_datetime <= $2
       ORDER BY transmission_datetime, transmission_payload_id"
    ),
    params = list(since, until)
  )
}

#' Record one transmission-adapter invocation
#'
#' @keywords internal
#' @noRd
transmission_record_import_run <- function(
  con,
  transmission_route_id,
  query_since,
  query_until,
  importer,
  source_server,
  status,
  payload_bytes = 0,
  transmissions_received = 0L,
  measurements_parsed = 0L,
  measurements_inserted = 0L,
  last_message_datetime = as.POSIXct(NA, tz = "UTC"),
  payload_reference = NULL,
  source_metadata = list(),
  error_message = NA_character_
) {
  route_id <- suppressWarnings(as.integer(transmission_route_id))
  if (length(route_id) != 1L || is.na(route_id) || route_id <= 0L) {
    stop("A valid transmission_route_id is required for import history.")
  }
  if (
    !is.character(importer) ||
      length(importer) != 1L ||
      is.na(importer) ||
      !nzchar(trimws(importer))
  ) {
    stop("A non-blank importer is required for import history.")
  }
  if (!status %in% c("running", "success", "no_data", "failed")) {
    stop("Invalid transmission import status: ", status, ".")
  }
  if (!is.list(source_metadata)) {
    stop("source_metadata must be a list.")
  }
  if (
    length(source_metadata) > 0L &&
      (is.null(names(source_metadata)) || any(!nzchar(names(source_metadata))))
  ) {
    stop("source_metadata must be an empty or named list.")
  }

  source_server <- if (is.null(source_server)) {
    NA_character_
  } else {
    as.character(source_server)
  }
  payload_reference <- if (is.null(payload_reference)) {
    NA_character_
  } else {
    as.character(payload_reference)
  }
  error_message <- if (is.null(error_message)) {
    NA_character_
  } else {
    as.character(error_message)
  }
  completed_sql <- if (status == "running") "NULL" else "clock_timestamp()"
  metadata_json <- as.character(jsonlite::toJSON(
    source_metadata,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))

  inserted <- DBI::dbGetQuery(
    con,
    paste0(
      "INSERT INTO continuous.transmission_import_runs (
         transmission_route_id,
         query_since,
         query_until,
         importer,
         source_server,
         status,
         payload_bytes,
         transmissions_received,
         measurements_parsed,
         measurements_inserted,
         last_message_datetime,
         payload_reference,
         source_metadata,
         error_message,
         completed
       ) VALUES (
         $1, $2, $3, $4, $5, $6, $7, $8, $9, $10,
         $11, $12, $13::jsonb, $14, ",
      completed_sql,
      "
       )
       RETURNING transmission_import_run_id"
    ),
    params = list(
      route_id,
      query_since,
      query_until,
      importer,
      source_server,
      status,
      as.numeric(payload_bytes),
      as.integer(transmissions_received),
      as.integer(measurements_parsed),
      as.integer(measurements_inserted),
      last_message_datetime,
      payload_reference,
      metadata_json,
      error_message
    )
  )
  as.numeric(inserted$transmission_import_run_id)
}

#' Validate transmission import-run identifiers
#'
#' @keywords internal
#' @noRd
transmission_validate_import_run_ids <- function(
  transmission_import_run_ids
) {
  run_ids <- unique(suppressWarnings(as.numeric(transmission_import_run_ids)))
  if (length(run_ids) == 0L) {
    return(numeric())
  }
  if (anyNA(run_ids) || any(run_ids <= 0) || any(run_ids %% 1 != 0)) {
    stop("Invalid transmission_import_run_ids supplied for finalization.")
  }
  run_ids
}

#' @keywords internal
#' @noRd
transmission_finalize_import_runs <- function(
  con,
  transmission_import_run_ids,
  measurements_inserted,
  workflow
) {
  run_ids <- transmission_validate_import_run_ids(
    transmission_import_run_ids
  )
  if (length(run_ids) == 0L) {
    return(invisible(0L))
  }
  measurements_inserted <- suppressWarnings(as.integer(measurements_inserted))
  if (
    length(measurements_inserted) != 1L ||
      is.na(measurements_inserted) ||
      measurements_inserted < 0L
  ) {
    stop("measurements_inserted must be one non-negative integer.")
  }
  if (
    !is.character(workflow) ||
      length(workflow) != 1L ||
      is.na(workflow) ||
      !nzchar(trimws(workflow))
  ) {
    stop("workflow must be one non-blank character value.")
  }

  updated <- DBI::dbExecute(
    con,
    paste0(
      "UPDATE continuous.transmission_import_runs
       SET measurements_inserted = $1,
           source_metadata = source_metadata || jsonb_build_object(
             'measurement_workflow', $2::text,
             'measurement_write_completed', TRUE
           )
       WHERE transmission_import_run_id IN (",
      paste(format(run_ids, scientific = FALSE, trim = TRUE), collapse = ", "),
      ")"
    ),
    params = list(measurements_inserted, workflow)
  )
  if (updated != length(run_ids)) {
    stop(
      "Could not finalize every delegated transmission import run: expected ",
      length(run_ids),
      ", updated ",
      updated,
      "."
    )
  }
  invisible(updated)
}

#' Mark a delegated transmission measurement write as failed
#'
#' @keywords internal
#' @noRd
transmission_fail_import_runs <- function(
  con,
  transmission_import_run_ids,
  workflow,
  error_message = "Delegated measurement workflow did not complete."
) {
  run_ids <- transmission_validate_import_run_ids(
    transmission_import_run_ids
  )
  if (length(run_ids) == 0L) {
    return(invisible(0L))
  }
  if (
    !is.character(workflow) ||
      length(workflow) != 1L ||
      is.na(workflow) ||
      !nzchar(trimws(workflow))
  ) {
    stop("workflow must be one non-blank character value.")
  }
  if (
    !is.character(error_message) ||
      length(error_message) != 1L ||
      is.na(error_message) ||
      !nzchar(trimws(error_message))
  ) {
    stop("error_message must be one non-blank character value.")
  }

  updated <- DBI::dbExecute(
    con,
    paste0(
      "UPDATE continuous.transmission_import_runs
       SET status = 'failed',
           measurements_inserted = 0,
           error_message = $1,
           source_metadata = source_metadata || jsonb_build_object(
             'measurement_workflow', $2::text,
             'measurement_write_completed', FALSE,
             'measurement_write_failed', TRUE
           )
       WHERE transmission_import_run_id IN (",
      paste(format(run_ids, scientific = FALSE, trim = TRUE), collapse = ", "),
      ")"
    ),
    params = list(error_message, workflow)
  )
  if (updated != length(run_ids)) {
    stop(
      "Could not fail every delegated transmission import run: expected ",
      length(run_ids),
      ", updated ",
      updated,
      "."
    )
  }
  invisible(updated)
}
