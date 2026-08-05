#' Download GOES transmissions from NESDIS directly into AquaCache
#'
#' Retrieves GOES Data Collection System messages through an installed OpenDCS
#' LRGS client, parses SHEF, comma-delimited, or BLM payloads, applies
#' database-defined field mappings, and appends observations to basic AquaCache
#' continuous timeseries. Routes sharing one DCP address are downloaded once
#' and then parsed and mapped independently, allowing one transmission to feed
#' multiple locations or message layouts.
#'
#' Transmission setup and route metadata are stored in
#' `public.locations_metadata_transmission_*`. Field mappings and durable import
#' history are stored in the provider-neutral tables added by database patch 56.
#' LRGS credentials are read from environment variables by default and are
#' never stored in the database.
#'
#' ## Route-configured payload formats
#'
#' SHEF routes need no parser settings. BLM and delimited routes store their
#' layout in `route_config.parser_config`, keeping format differences with the
#' route rather than in function arguments. BLM configuration requires
#' `fields`, ordered one per payload row, and normally supplies
#' `sample_interval_seconds`, `sample_offset_seconds`, and `values_order`.
#' Delimited configuration uses `has_header`; headerless messages also require
#' `fields`. Datetimes can come from `datetime_field`, with optional
#' `datetime_format` and `datetime_timezone`, or can be reconstructed using
#' `record_interval_seconds`, `record_offset_seconds`, and `records_order`.
#' Both parsers include the LRGS header fields by default; set
#' `include_lrgs_header_fields` to `FALSE` to omit them. Parsed text fields are
#' retained in `raw_value` and receive a missing numeric `value`; AquaCache does
#' not apply rwdm-style range clamping.
#'
#' @param transmission_route_id Optional integer vector of GOES transmission
#'   route IDs. When `NULL`, all currently effective GOES DCS routes having
#'   enabled field mappings are imported.
#' @param timeseries_id Optional basic timeseries ID. When supplied,
#'   `downloadNESDIS()` acts as a `source_fx` adapter and returns only the
#'   `datetime` and `value` columns for that timeseries. Measurements and
#'   import-run history are then left for [getNewContinuous()] to manage.
#' @param start_datetime Optional start of the LRGS query window. When omitted,
#'   the latest successful route query is used with the configured overlap.
#' @param end_datetime End of the query window. Defaults to the current time.
#' @param con AquaCache database connection. When `NULL`, [AquaConnect()] is
#'   used and the connection is closed on exit.
#' @param client_path Path to OpenDCS `getDcpMessages.bat`. The default is read
#'   from `NESDIS_LRGS_CLIENT`, falling back to the standard local install path.
#' @param username LRGS username. Defaults to `NESDIS_LRGS_USER`.
#' @param password LRGS password. Defaults to `NESDIS_LRGS_PASSWORD`.
#' @param servers Optional character vector of LRGS servers. Route-level
#'   `route_config.lrgs_servers` takes precedence when present.
#' @param port Optional LRGS port. Route-level `route_config.lrgs_port` takes
#'   precedence. The default is 16003.
#' @param overwrite Passed to [addNewContinuous()]. `"no"` is recommended for
#'   scheduled imports; `"conflict"` can be used for a deliberate replay.
#' @param write If `FALSE`, return normalized mapped observations without
#'   changing measurements or import history.
#' @param raw_messages Optional named character vector or list keyed by DCP
#'   address. This bypasses the LRGS client for replay and testing.
#' @param payload_reference Optional external file or object-store reference
#'   recorded with each import run when `raw_messages` came from durable
#'   storage.
#' @param parser Optional custom parser function for a future message format.
#'   It must accept `message`, `dcp_address`, and `message_format`; parsers may
#'   also accept `route_config` or `...`. It must return a data.frame with
#'   `source_field`, `datetime`, `raw_value`, and `value`. Non-numeric payload
#'   values remain available in `raw_value` while `value` is `NA_real_`.
#' @param cache If `TRUE`, reuse an LRGS payload already downloaded during the
#'   current R session when it covers the requested DCP and time window. The
#'   cache stores raw transmissions so each route can still use its own parser
#'   and mappings.
#'
#' @return With `timeseries_id = NULL`, a list containing `summary`, with one
#'   row per route, and `data`, the normalized mapped observations considered
#'   during the run. With `timeseries_id` supplied, a data table containing
#'   `datetime` and `value`, suitable for use as a `source_fx`.
#' @export
downloadNESDIS <- function(
  transmission_route_id = NULL,
  timeseries_id = NULL,
  start_datetime = NULL,
  end_datetime = Sys.time(),
  con = NULL,
  client_path = Sys.getenv(
    "NESDIS_LRGS_CLIENT",
    "C:/opendcs-7.0.16-RC06/bin/getDcpMessages.bat"
  ),
  username = Sys.getenv("NESDIS_LRGS_USER"),
  password = Sys.getenv("NESDIS_LRGS_PASSWORD"),
  servers = NULL,
  port = NULL,
  overwrite = "no",
  write = TRUE,
  raw_messages = NULL,
  payload_reference = NULL,
  parser = NULL,
  cache = TRUE
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  if (!overwrite %in% c("no", "conflict")) {
    stop(
      "downloadNESDIS: 'overwrite' must be either 'no' or 'conflict'."
    )
  }
  if (!is.logical(write) || length(write) != 1L || is.na(write)) {
    stop("downloadNESDIS: 'write' must be TRUE or FALSE.")
  }
  if (!is.logical(cache) || length(cache) != 1L || is.na(cache)) {
    stop("downloadNESDIS: 'cache' must be TRUE or FALSE.")
  }

  adapter_timeseries_id <- nesdis_validate_timeseries_id(timeseries_id)
  adapter_mode <- !is.null(adapter_timeseries_id)
  if (adapter_mode) {
    write <- FALSE
  }

  end_datetime <- nesdis_as_utc(end_datetime, "end_datetime")
  if (!is.null(start_datetime)) {
    start_datetime <- nesdis_as_utc(start_datetime, "start_datetime")
    if (start_datetime >= end_datetime) {
      stop("downloadNESDIS: start_datetime must precede end_datetime.")
    }
  }

  route_ids <- nesdis_validate_route_ids(transmission_route_id)
  routes <- nesdis_get_routes(con, route_ids, end_datetime)
  if (nrow(routes) == 0L) {
    stop("downloadNESDIS: No configured GOES DCS routes were found.")
  }
  routes[, platform_identifier := toupper(trimws(platform_identifier))]

  mappings <- nesdis_get_mappings(con, routes$transmission_route_id)
  if (adapter_mode) {
    mappings <- mappings[timeseries_id == adapter_timeseries_id]
    if (nrow(mappings) == 0L) {
      stop(
        "downloadNESDIS: No enabled, currently effective GOES DCS mapping ",
        "was found for timeseries_id ",
        adapter_timeseries_id,
        "."
      )
    }
    mapped_route_ids <- unique(mappings$transmission_route_id)
    if (length(mapped_route_ids) != 1L) {
      stop(
        "downloadNESDIS: timeseries_id ",
        adapter_timeseries_id,
        " has mappings to more than one currently effective route. Pass a ",
        "single transmission_route_id or correct the overlapping mappings."
      )
    }
    routes <- routes[
      transmission_route_id == mapped_route_ids[[1L]]
    ]
  }
  routes <- routes[
    routes$transmission_route_id %in% mappings$transmission_route_id,
  ]
  if (nrow(routes) == 0L) {
    stop(
      "downloadNESDIS: The selected routes have no enabled transmission ",
      "timeseries mappings."
    )
  }

  route_configs <- lapply(routes$route_config, nesdis_parse_json_object)
  routes[,
    max_days := vapply(
      route_configs,
      nesdis_config_number,
      numeric(1),
      key = "max_days",
      default = 14
    )
  ]
  routes[,
    overlap_minutes := vapply(
      route_configs,
      nesdis_config_number,
      numeric(1),
      key = "overlap_minutes",
      default = 5
    )
  ]

  cursor <- nesdis_get_cursors(con, routes$transmission_route_id)
  routes <- merge(
    routes,
    cursor,
    by = "transmission_route_id",
    all.x = TRUE,
    sort = FALSE
  )
  routes[, query_until := end_datetime]
  if (is.null(start_datetime)) {
    routes[,
      query_since := as.POSIXct(
        ifelse(
          is.na(last_query_until),
          as.numeric(query_until - max_days * 86400),
          as.numeric(last_query_until - overlap_minutes * 60)
        ),
        origin = "1970-01-01",
        tz = "UTC"
      )
    ]
  } else {
    routes[, query_since := start_datetime]
  }
  if (adapter_mode) {
    routes[,
      query_since := as.POSIXct(
        pmax(
          as.numeric(query_since),
          as.numeric(query_until - max_days * 86400)
        ),
        origin = "1970-01-01",
        tz = "UTC"
      )
    ]
  }
  routes[
    query_since < start_datetime_setup,
    query_since := start_datetime_setup
  ]
  if (any(routes$query_since >= routes$query_until)) {
    bad <- routes$query_since >= routes$query_until
    stop(
      "downloadNESDIS: The effective setup period leaves no query window for ",
      "route(s) ",
      paste(routes$transmission_route_id[bad], collapse = ", "),
      "."
    )
  }

  dcp_addresses <- unique(routes$platform_identifier)
  invalid_dcp <- is.na(dcp_addresses) |
    !grepl("^[[:xdigit:]]{8}$", dcp_addresses)
  if (any(invalid_dcp)) {
    stop(
      "downloadNESDIS: Every selected setup requires an eight-character ",
      "hexadecimal platform_identifier (DCP address)."
    )
  }

  supplied_messages <- nesdis_normalize_raw_messages(
    raw_messages,
    dcp_addresses
  )
  summaries <- list()
  mapped_results <- list()
  summary_index <- 0L
  data_index <- 0L

  for (dcp_address in dcp_addresses) {
    group_routes <- routes[platform_identifier == dcp_address]
    group_since <- min(group_routes$query_since)
    group_until <- max(group_routes$query_until)

    fetch_result <- if (!is.null(supplied_messages)) {
      list(
        message = supplied_messages[[dcp_address]],
        server = "supplied",
        source_metadata = list(retrieval = "raw_messages")
      )
    } else {
      route_config <- nesdis_parse_json_object(group_routes$route_config[[1L]])
      fetch_servers <- route_config$lrgs_servers %||%
        servers %||%
        nesdis_default_servers()
      fetch_port <- route_config$lrgs_port %||% port %||% 16003
      timeout_seconds <- route_config$timeout_seconds %||% 600
      timezone_offset <- route_config$lrgs_timezone_offset_hours %||% -8

      tryCatch(
        nesdis_fetch_cached(
          dcp_address = dcp_address,
          since = group_since,
          until = group_until,
          client_path = client_path,
          username = username,
          password = password,
          servers = fetch_servers,
          port = fetch_port,
          timezone_offset = timezone_offset,
          timeout_seconds = timeout_seconds,
          cache = cache
        ),
        error = identity
      )
    }

    if (inherits(fetch_result, "error")) {
      for (route_row in seq_len(nrow(group_routes))) {
        summary_index <- summary_index + 1L
        route <- group_routes[route_row]
        if (write) {
          nesdis_record_import_run(
            con = con,
            route = route,
            status = "failed",
            source_server = NA_character_,
            payload_bytes = 0,
            transmissions_received = 0,
            measurements_parsed = 0,
            measurements_inserted = 0,
            last_message_datetime = as.POSIXct(NA, tz = "UTC"),
            error_message = conditionMessage(fetch_result),
            payload_reference = payload_reference,
            source_metadata = list()
          )
        }
        summaries[[summary_index]] <- nesdis_summary_row(
          route,
          "failed",
          NA_character_,
          0,
          0,
          0,
          0,
          conditionMessage(fetch_result)
        )
      }
      next
    }

    raw_message <- fetch_result$message %||% ""
    payload_bytes <- length(charToRaw(enc2utf8(raw_message)))

    for (route_row in seq_len(nrow(group_routes))) {
      route <- group_routes[route_row]
      route_mappings <- mappings[
        transmission_route_id == route$transmission_route_id
      ]

      parsed <- tryCatch(
        nesdis_parse_dispatch(
          message = raw_message,
          dcp_address = dcp_address,
          message_format = route$message_format,
          route_config = route$route_config[[1L]],
          parser = parser
        ),
        error = identity
      )

      if (inherits(parsed, "error")) {
        summary_index <- summary_index + 1L
        if (write) {
          nesdis_record_import_run(
            con = con,
            route = route,
            status = "failed",
            source_server = fetch_result$server,
            payload_bytes = payload_bytes,
            transmissions_received = 0,
            measurements_parsed = 0,
            measurements_inserted = 0,
            last_message_datetime = as.POSIXct(NA, tz = "UTC"),
            error_message = conditionMessage(parsed),
            payload_reference = payload_reference,
            source_metadata = fetch_result$source_metadata %||% list()
          )
        }
        summaries[[summary_index]] <- nesdis_summary_row(
          route,
          "failed",
          fetch_result$server,
          payload_bytes,
          0,
          0,
          0,
          conditionMessage(parsed)
        )
        next
      }

      transmissions_received <- attr(parsed, "transmissions_received") %||% 0L
      route_data <- nesdis_apply_mappings(parsed, route_mappings)
      route_data <- route_data[
        datetime >= route$query_since & datetime <= route$query_until
      ]
      if (nrow(route_data) > 0L) {
        data_index <- data_index + 1L
        mapped_results[[data_index]] <- route_data
      }

      result <- nesdis_write_route(
        con = con,
        route = route,
        data = route_data,
        write = write,
        overwrite = overwrite,
        source_server = fetch_result$server,
        payload_bytes = payload_bytes,
        transmissions_received = transmissions_received,
        payload_reference = payload_reference,
        source_metadata = fetch_result$source_metadata %||% list()
      )
      summary_index <- summary_index + 1L
      summaries[[summary_index]] <- result$summary
    }
  }

  summary <- data.table::rbindlist(summaries, fill = TRUE)
  data <- if (length(mapped_results)) {
    data.table::rbindlist(mapped_results, fill = TRUE)
  } else {
    nesdis_empty_mapped_data()
  }

  if (adapter_mode) {
    failed <- summary$status == "failed"
    if (any(failed)) {
      stop(
        "downloadNESDIS: Could not fetch timeseries_id ",
        adapter_timeseries_id,
        ": ",
        paste(unique(summary$error_message[failed]), collapse = "; ")
      )
    }
    if (nrow(data) == 0L) {
      return(data.table::data.table(
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric()
      ))
    }
    adapter_data <- data[
      timeseries_id == adapter_timeseries_id,
      .(datetime, value)
    ]
    data.table::setorder(adapter_data, datetime)
    duplicate_datetimes <- duplicated(adapter_data$datetime)
    if (any(duplicate_datetimes)) {
      stop(
        "downloadNESDIS: The configured mapping produced duplicate datetimes ",
        "for timeseries_id ",
        adapter_timeseries_id,
        "."
      )
    }
    return(adapter_data)
  }

  list(summary = summary, data = data)
}

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

#' @keywords internal
#' @noRd
nesdis_as_utc <- function(x, argument) {
  if (length(x) != 1L) {
    stop("downloadNESDIS: ", argument, " must contain exactly one value.")
  }
  if (inherits(x, "Date")) {
    x <- as.POSIXct(x, tz = "UTC")
  } else if (!inherits(x, "POSIXct")) {
    x <- as.POSIXct(x, tz = "UTC")
  }
  if (is.na(x)) {
    stop("downloadNESDIS: Could not interpret ", argument, " as a datetime.")
  }
  as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
}

#' @keywords internal
#' @noRd
nesdis_validate_route_ids <- function(route_ids) {
  if (is.null(route_ids)) {
    return(NULL)
  }
  route_ids <- suppressWarnings(as.integer(route_ids))
  if (
    anyNA(route_ids) ||
      any(route_ids <= 0L) ||
      anyDuplicated(route_ids)
  ) {
    stop(
      "downloadNESDIS: transmission_route_id must contain unique positive integers."
    )
  }
  route_ids
}

#' @keywords internal
#' @noRd
nesdis_validate_timeseries_id <- function(timeseries_id) {
  if (is.null(timeseries_id)) {
    return(NULL)
  }
  if (length(timeseries_id) != 1L) {
    stop("downloadNESDIS: timeseries_id must contain exactly one value.")
  }
  timeseries_id <- suppressWarnings(as.integer(timeseries_id))
  if (is.na(timeseries_id) || timeseries_id <= 0L) {
    stop("downloadNESDIS: timeseries_id must be a positive integer.")
  }
  timeseries_id
}

#' @keywords internal
#' @noRd
nesdis_get_routes <- function(con, route_ids, effective_at) {
  route_filter <- if (is.null(route_ids)) {
    ""
  } else {
    paste0(
      " AND r.transmission_route_id IN (",
      paste(route_ids, collapse = ", "),
      ")"
    )
  }
  sql <- paste0(
    "SELECT
       r.transmission_route_id,
       r.transmission_setup_id,
       r.message_format,
       r.route_config::text AS route_config,
       r.route_name,
       s.platform_identifier,
       s.start_datetime AS start_datetime_setup,
       s.end_datetime AS end_datetime_setup,
       lmi.location_id
     FROM public.locations_metadata_transmission_routes r
     JOIN public.locations_metadata_transmission_setups s
       ON s.transmission_setup_id = r.transmission_setup_id
     JOIN instruments.transmission_methods tm
       ON tm.transmission_method_id = s.transmission_method_id
     JOIN public.locations_metadata_instruments lmi
       ON lmi.metadata_id = s.logger_metadata_id
     WHERE tm.method_code = 'GOES_DCS'
       AND s.start_datetime <= $1
       AND (s.end_datetime IS NULL OR s.end_datetime > $1)",
    route_filter,
    " ORDER BY r.transmission_route_id"
  )
  data.table::as.data.table(DBI::dbGetQuery(
    con,
    sql,
    params = list(effective_at)
  ))
}

#' @keywords internal
#' @noRd
nesdis_get_mappings <- function(con, route_ids) {
  if (length(route_ids) == 0L) {
    return(data.table::data.table())
  }
  sql <- paste0(
    "SELECT
       transmission_mapping_id,
       transmission_route_id,
       source_field,
       timeseries_id,
       value_multiplier,
       value_offset,
       missing_values::text AS missing_values,
       mapping_config::text AS mapping_config
     FROM continuous.transmission_timeseries_mappings
     WHERE enabled
       AND transmission_route_id IN (",
    paste(as.integer(route_ids), collapse = ", "),
    ")
     ORDER BY transmission_route_id, transmission_mapping_id"
  )
  data.table::as.data.table(DBI::dbGetQuery(con, sql))
}

#' @keywords internal
#' @noRd
nesdis_get_cursors <- function(con, route_ids) {
  sql <- paste0(
    "SELECT transmission_route_id, max(query_until) AS last_query_until
     FROM continuous.transmission_import_runs
     WHERE importer = 'downloadNESDIS'
       AND status IN ('success', 'no_data')
       AND transmission_route_id IN (",
    paste(as.integer(route_ids), collapse = ", "),
    ")
     GROUP BY transmission_route_id"
  )
  data.table::as.data.table(DBI::dbGetQuery(con, sql))
}

#' @keywords internal
#' @noRd
nesdis_parse_json_object <- function(x) {
  if (is.list(x) && !inherits(x, "pq_json")) {
    return(x)
  }
  if (length(x) == 0L || is.na(x) || !nzchar(x)) {
    return(list())
  }
  parsed <- jsonlite::fromJSON(as.character(x), simplifyVector = TRUE)
  if (!is.list(parsed)) {
    stop("downloadNESDIS: route_config must be a JSON object.")
  }
  parsed
}

#' @keywords internal
#' @noRd
nesdis_config_number <- function(config, key, default) {
  value <- if (
    is.null(names(config)) ||
      !key %in% names(config) ||
      is.null(config[[key]])
  ) {
    default
  } else {
    config[[key]]
  }
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || value < 0) {
    stop(
      "downloadNESDIS: route_config.",
      key,
      " must be one non-negative number."
    )
  }
  value
}

#' @keywords internal
#' @noRd
nesdis_default_servers <- function() {
  c(
    "205.156.2.189",
    "205.156.2.186",
    "205.156.2.174",
    "152.61.129.81",
    "152.61.129.82",
    "152.61.129.93"
  )
}

#' @keywords internal
#' @noRd
nesdis_normalize_raw_messages <- function(raw_messages, dcp_addresses) {
  if (is.null(raw_messages)) {
    return(NULL)
  }
  if (is.character(raw_messages) && length(raw_messages) == 1L) {
    if (length(dcp_addresses) != 1L) {
      stop(
        "downloadNESDIS: A single unnamed raw message can only be used with ",
        "one DCP address."
      )
    }
    raw_messages <- stats::setNames(list(raw_messages), dcp_addresses)
  } else {
    raw_messages <- as.list(raw_messages)
  }
  if (!is.null(names(raw_messages))) {
    names(raw_messages) <- toupper(trimws(names(raw_messages)))
  }
  if (is.null(names(raw_messages)) || any(!nzchar(names(raw_messages)))) {
    stop("downloadNESDIS: raw_messages must be named by DCP address.")
  }
  if (anyDuplicated(names(raw_messages))) {
    stop("downloadNESDIS: raw_messages contains duplicate DCP addresses.")
  }
  missing <- setdiff(dcp_addresses, names(raw_messages))
  if (length(missing)) {
    stop(
      "downloadNESDIS: raw_messages is missing DCP address(es): ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  raw_messages
}

#' @keywords internal
#' @noRd
nesdis_cache_dir <- function() {
  file.path(tempdir(), "downloadNESDIS")
}

#' @keywords internal
#' @noRd
nesdis_find_cached_payload <- function(dcp_address, since, until) {
  cache_dir <- nesdis_cache_dir()
  if (!dir.exists(cache_dir)) {
    return(NULL)
  }

  cache_files <- list.files(
    cache_dir,
    pattern = paste0("^", dcp_address, "_.*\\.rds$"),
    full.names = TRUE
  )
  if (length(cache_files) == 0L) {
    return(NULL)
  }

  requested_since <- as.numeric(since)
  requested_until <- as.numeric(until)
  candidates <- lapply(cache_files, function(cache_file) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (
      !is.list(cached) ||
        !identical(cached$dcp_address, dcp_address) ||
        is.null(cached$since) ||
        is.null(cached$until) ||
        is.null(cached$result)
    ) {
      return(NULL)
    }
    cached_since <- as.numeric(cached$since)
    cached_until <- as.numeric(cached$until)
    if (
      is.na(cached_since) ||
        is.na(cached_until) ||
        cached_since > requested_since ||
        cached_until < requested_until
    ) {
      return(NULL)
    }
    list(
      payload = cached,
      width = cached_until - cached_since
    )
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0L) {
    return(NULL)
  }
  candidates[[which.min(vapply(
    candidates,
    `[[`,
    numeric(1),
    "width"
  ))]]$payload
}

#' @keywords internal
#' @noRd
nesdis_save_cached_payload <- function(dcp_address, since, until, result) {
  cache_dir <- nesdis_cache_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(
    cache_dir,
    paste0(
      dcp_address,
      "_",
      format(since, "%Y%m%dT%H%M%S", tz = "UTC"),
      "_",
      format(until, "%Y%m%dT%H%M%S", tz = "UTC"),
      "_",
      Sys.getpid(),
      ".rds"
    )
  )
  temporary_file <- tempfile(
    pattern = paste0(dcp_address, "_"),
    tmpdir = cache_dir,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary_file, force = TRUE), add = TRUE)
  saveRDS(
    list(
      dcp_address = dcp_address,
      since = since,
      until = until,
      cached_at = Sys.time(),
      result = result
    ),
    temporary_file
  )
  if (!file.rename(temporary_file, cache_file)) {
    stop("downloadNESDIS: Could not finalize the LRGS payload cache file.")
  }
  invisible(cache_file)
}

#' @keywords internal
#' @noRd
nesdis_fetch_cached <- function(
  dcp_address,
  since,
  until,
  client_path,
  username,
  password,
  servers,
  port,
  timezone_offset,
  timeout_seconds,
  cache = TRUE
) {
  # OpenDCS criteria have minute precision. Expanding to full minute bounds
  # also lets sequential source_fx calls share a payload despite slightly
  # different Sys.time() values.
  fetch_since <- as.POSIXct(
    floor(as.numeric(since) / 60) * 60,
    origin = "1970-01-01",
    tz = "UTC"
  )
  fetch_until <- as.POSIXct(
    ceiling(as.numeric(until) / 60) * 60,
    origin = "1970-01-01",
    tz = "UTC"
  )

  if (cache) {
    cached <- nesdis_find_cached_payload(
      dcp_address,
      fetch_since,
      fetch_until
    )
    if (!is.null(cached)) {
      result <- cached$result
      result$source_metadata <- c(
        result$source_metadata %||% list(),
        list(
          cache_hit = TRUE,
          cached_at = format(
            cached$cached_at,
            "%Y-%m-%dT%H:%M:%SZ",
            tz = "UTC"
          )
        )
      )
      return(result)
    }
  }

  result <- nesdis_fetch_lrgs(
    dcp_address = dcp_address,
    since = fetch_since,
    until = fetch_until,
    client_path = client_path,
    username = username,
    password = password,
    servers = servers,
    port = port,
    timezone_offset = timezone_offset,
    timeout_seconds = timeout_seconds
  )
  if (cache) {
    nesdis_save_cached_payload(
      dcp_address,
      fetch_since,
      fetch_until,
      result
    )
  }
  result
}

#' @keywords internal
#' @noRd
nesdis_fetch_lrgs <- function(
  dcp_address,
  since,
  until,
  client_path,
  username,
  password,
  servers,
  port,
  timezone_offset,
  timeout_seconds
) {
  if (!nzchar(client_path) || !file.exists(client_path)) {
    stop(
      "downloadNESDIS: OpenDCS LRGS client was not found at '",
      client_path,
      "'. Set NESDIS_LRGS_CLIENT environment variables or pass client_path."
    )
  }
  if (!nzchar(username) || !nzchar(password)) {
    stop(
      "downloadNESDIS: Set NESDIS_LRGS_USER and NESDIS_LRGS_PASSWORD environment variables or pass ",
      "username and password explicitly."
    )
  }
  servers <- as.character(unlist(servers, use.names = FALSE))
  servers <- servers[nzchar(servers)]
  if (length(servers) == 0L) {
    stop("downloadNESDIS: At least one LRGS server is required.")
  }

  criteria_file <- tempfile(
    pattern = paste0("nesdis_", dcp_address, "_"),
    fileext = ".sc"
  )
  raw_log <- tempfile(
    pattern = paste0("nesdis_", dcp_address, "_"),
    fileext = ".log"
  )
  stderr_file <- tempfile(
    pattern = paste0("nesdis_", dcp_address, "_stderr_"),
    fileext = ".log"
  )
  on.exit(
    unlink(c(criteria_file, raw_log, stderr_file), force = TRUE),
    add = TRUE
  )

  writeLines(
    c(
      "#",
      "# LRGS Search Criteria",
      "#",
      paste0("DRS_SINCE: ", format(since, "%Y/%j %H:%M", tz = "UTC")),
      paste0("DRS_UNTIL: ", format(until, "%Y/%j %H:%M", tz = "UTC")),
      paste0("DCP_ADDRESS: ", dcp_address)
    ),
    criteria_file,
    useBytes = TRUE
  )

  attempts <- list()
  for (server in servers) {
    arguments <- c(
      "-Y",
      as.character(timezone_offset),
      "-h",
      server,
      "-p",
      as.character(as.integer(port)),
      "-u",
      username,
      "-P",
      password,
      "-f",
      criteria_file,
      "-v",
      "-n",
      "-l",
      raw_log
    )
    normalized_client <- normalizePath(
      client_path,
      winslash = "\\",
      mustWork = TRUE
    )
    output <- tryCatch(
      system2(
        normalized_client,
        args = vapply(arguments, shQuote, character(1), type = "cmd"),
        stdout = TRUE,
        stderr = stderr_file,
        timeout = as.integer(timeout_seconds)
      ),
      error = identity
    )

    if (inherits(output, "error")) {
      attempts[[server]] <- conditionMessage(output)
      next
    }
    status <- attr(output, "status") %||% 0L
    message_text <- paste(output, collapse = "\n")
    attempts[[server]] <- list(
      status = status,
      payload_bytes = length(charToRaw(enc2utf8(message_text)))
    )
    if (identical(status, 0L) && nchar(message_text, type = "bytes") > 38L) {
      return(list(
        message = message_text,
        server = server,
        source_metadata = list(
          retrieval = "OpenDCS getDcpMessages",
          attempted_servers = names(attempts),
          lrgs_port = as.integer(port)
        )
      ))
    }
  }

  # A successful empty response is a valid no-data result. If every call
  # failed, retain the attempt summary in the error without exposing secrets.
  successful <- vapply(
    attempts,
    function(x) is.list(x) && identical(x$status, 0L),
    logical(1)
  )
  if (any(successful)) {
    server <- names(attempts)[which(successful)[1L]]
    return(list(
      message = "",
      server = server,
      source_metadata = list(
        retrieval = "OpenDCS getDcpMessages",
        attempted_servers = names(attempts),
        lrgs_port = as.integer(port)
      )
    ))
  }

  stop(
    "downloadNESDIS: All LRGS servers failed for DCP ",
    dcp_address,
    ". Attempted: ",
    paste(names(attempts), collapse = ", "),
    "."
  )
}

#' @keywords internal
#' @noRd
nesdis_parse_dispatch <- function(
  message,
  dcp_address,
  message_format,
  route_config = list(),
  parser = NULL
) {
  if (!is.list(route_config) || inherits(route_config, "pq_json")) {
    route_config <- nesdis_parse_json_object(route_config)
  }
  parser_config <- route_config$parser_config %||% list()
  if (!is.list(parser_config) || inherits(parser_config, "pq_json")) {
    stop("downloadNESDIS: route_config.parser_config must be a JSON object.")
  }

  if (!is.null(parser)) {
    if (!is.function(parser)) {
      stop("downloadNESDIS: parser must be a function.")
    }
    parser_args <- list(
      message = message,
      dcp_address = dcp_address,
      message_format = message_format
    )
    parser_formals <- names(formals(parser))
    if ("route_config" %in% parser_formals || "..." %in% parser_formals) {
      parser_args$route_config <- route_config
    }
    parsed <- do.call(parser, parser_args)
    return(nesdis_validate_parser_output(parsed))
  }

  normalized_format <- gsub("[^A-Z0-9]", "", toupper(message_format %||% ""))
  if (normalized_format %in% c("SHEF", "SHEFMCMASTER")) {
    return(nesdis_parse_shef(message, dcp_address))
  }
  if (normalized_format %in% c("CSV", "COMMADELIMITED", "COMMASEPARATED")) {
    return(nesdis_parse_delimited(message, dcp_address, parser_config))
  }
  if (normalized_format %in% c("BLM", "BUREAUOFLANDMANAGEMENT")) {
    return(nesdis_parse_blm(message, dcp_address, parser_config))
  }
  stop(
    "downloadNESDIS: Unsupported message_format '",
    message_format,
    "'. Supported built-in formats are SHEF, SHEF_McMaster, CSV, ",
    "comma-delimited, and BLM; pass a custom parser function for another ",
    "format."
  )
}

#' @keywords internal
#' @noRd
nesdis_extract_lrgs_transmissions <- function(message, dcp_address) {
  if (length(message) == 0L || is.na(message) || !nzchar(message)) {
    return(list())
  }

  dcp_address <- toupper(trimws(dcp_address))
  lines <- strsplit(message, "\r\n|\n|\r", perl = TRUE)[[1L]]
  lines <- trimws(lines)
  possible_header <- startsWith(toupper(lines), dcp_address) &
    nchar(lines, type = "chars") >= 38L &
    grepl("^[[:digit:]]{11}$", substr(lines, 9L, 19L))
  starts <- which(possible_header)
  if (!length(starts)) {
    return(list())
  }

  transmissions <- vector("list", length(starts))
  valid_index <- 0L
  for (i in seq_along(starts)) {
    first <- starts[[i]]
    last <- if (i < length(starts)) starts[[i + 1L]] - 1L else length(lines)
    header_line <- lines[[first]]
    if (nchar(header_line, type = "chars") < 38L) {
      next
    }

    transmission_time <- as.POSIXct(
      strptime(substr(header_line, 9L, 19L), "%y%j%H%M%S", tz = "UTC")
    )
    if (is.na(transmission_time)) {
      next
    }

    inline_payload <- if (nchar(header_line, type = "chars") > 38L) {
      trimws(substr(header_line, 39L, nchar(header_line, type = "chars")))
    } else {
      ""
    }
    following_lines <- if (last > first) {
      lines[(first + 1L):last]
    } else {
      character()
    }
    body_lines <- c(inline_payload, following_lines)
    body_lines <- body_lines[nzchar(body_lines)]

    valid_index <- valid_index + 1L
    transmissions[[valid_index]] <- list(
      header = substr(header_line, 1L, 38L),
      transmission_time = transmission_time,
      body_lines = body_lines,
      transmission_sequence = i
    )
  }
  transmissions[seq_len(valid_index)]
}

#' @keywords internal
#' @noRd
nesdis_lrgs_header_rows <- function(
  header,
  transmission_time,
  order_offset = 0L
) {
  header_values <- list(
    YSS = trimws(substr(header, 21L, 22L)),
    DCP_Freq_Drft = trimws(substr(header, 23L, 24L)),
    `Message Size` = trimws(substr(header, 33L, 37L)),
    `GPS Synch` = substr(header, 38L, 38L)
  )
  for (field in names(header_values)) {
    header_values[[field]] <- nesdis_signal_value(header_values[[field]], field)
  }

  raw_values <- unlist(header_values, use.names = FALSE)
  data.table::data.table(
    source_field = names(header_values),
    datetime = transmission_time,
    raw_value = as.character(raw_values),
    value = suppressWarnings(as.numeric(raw_values)),
    transmission_order = order_offset + seq_along(header_values)
  )
}

#' @keywords internal
#' @noRd
nesdis_empty_parsed_data <- function(transmissions_received = 0L) {
  result <- data.table::data.table(
    source_field = character(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    raw_value = character(),
    value = numeric()
  )
  attr(result, "transmissions_received") <- as.integer(transmissions_received)
  result
}

#' @keywords internal
#' @noRd
nesdis_finalize_parsed_data <- function(rows, transmissions_received) {
  if (!length(rows)) {
    return(nesdis_empty_parsed_data(transmissions_received))
  }
  result <- data.table::rbindlist(rows, fill = TRUE)
  if (!nrow(result)) {
    return(nesdis_empty_parsed_data(transmissions_received))
  }
  data.table::setorder(result, source_field, datetime, transmission_order)
  result <- result[, .SD[.N], by = .(source_field, datetime)]
  result[, transmission_order := NULL]
  data.table::setorder(result, datetime, source_field)
  attr(result, "transmissions_received") <- as.integer(transmissions_received)
  result
}

#' @keywords internal
#' @noRd
nesdis_parser_fields <- function(config, format_name) {
  fields <- config$fields %||% character()
  fields <- as.character(unlist(fields, use.names = FALSE))
  fields <- trimws(fields)
  if (!length(fields) || any(!nzchar(fields)) || anyDuplicated(fields)) {
    stop(
      "downloadNESDIS: route_config.parser_config.fields must contain ",
      "unique, non-empty field names for ",
      format_name,
      "."
    )
  }
  fields
}

#' @keywords internal
#' @noRd
nesdis_parser_number <- function(config, key, default = 0, minimum = 0) {
  value <- config[[key]] %||% default
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || value < minimum) {
    stop(
      "downloadNESDIS: route_config.parser_config.",
      key,
      " must be one number greater than or equal to ",
      minimum,
      "."
    )
  }
  value
}

#' @keywords internal
#' @noRd
nesdis_parser_choice <- function(config, key, default, choices) {
  value <- tolower(as.character(config[[key]] %||% default))
  if (length(value) != 1L || is.na(value) || !value %in% choices) {
    stop(
      "downloadNESDIS: route_config.parser_config.",
      key,
      " must be one of: ",
      paste(choices, collapse = ", "),
      "."
    )
  }
  value
}

#' @keywords internal
#' @noRd
nesdis_parse_blm <- function(message, dcp_address, config) {
  fields <- nesdis_parser_fields(config, "BLM")
  interval_seconds <- nesdis_parser_number(
    config,
    "sample_interval_seconds",
    default = 0
  )
  offset_seconds <- nesdis_parser_number(
    config,
    "sample_offset_seconds",
    default = 0
  )
  values_order <- nesdis_parser_choice(
    config,
    "values_order",
    default = "oldest_first",
    choices = c("newest_first", "oldest_first")
  )
  strict_field_count <- config$strict_field_count %||% TRUE
  if (
    !is.logical(strict_field_count) ||
      length(strict_field_count) != 1L ||
      is.na(strict_field_count)
  ) {
    stop(
      "downloadNESDIS: route_config.parser_config.strict_field_count must ",
      "be TRUE or FALSE."
    )
  }
  include_header <- config$include_lrgs_header_fields %||% TRUE
  if (
    !is.logical(include_header) ||
      length(include_header) != 1L ||
      is.na(include_header)
  ) {
    stop(
      "downloadNESDIS: route_config.parser_config.include_lrgs_header_fields ",
      "must be TRUE or FALSE."
    )
  }
  delimiter_pattern <- as.character(
    config$delimiter_pattern %||% "[[:space:]]+"
  )
  if (length(delimiter_pattern) != 1L || !nzchar(delimiter_pattern)) {
    stop(
      "downloadNESDIS: route_config.parser_config.delimiter_pattern must be ",
      "one non-empty regular expression."
    )
  }

  transmissions <- nesdis_extract_lrgs_transmissions(message, dcp_address)
  if (!length(transmissions)) {
    return(nesdis_empty_parsed_data())
  }

  rows <- list()
  row_index <- 0L
  for (transmission in transmissions) {
    body_lines <- transmission$body_lines
    no_data <- grepl(
      "^NO[[:space:]]+DATA([[:space:]]+AVAILABLE.*)?$",
      body_lines,
      ignore.case = TRUE
    )
    body_lines <- body_lines[!no_data]
    if (!length(body_lines)) {
      next
    }
    if (strict_field_count && length(body_lines) != length(fields)) {
      stop(
        "downloadNESDIS: BLM payload for DCP ",
        dcp_address,
        " at ",
        format(transmission$transmission_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        " UTC has ",
        length(body_lines),
        " field rows; parser_config.fields defines ",
        length(fields),
        "."
      )
    }

    order_base <- transmission$transmission_sequence * 1000000L
    if (include_header) {
      row_index <- row_index + 1L
      rows[[row_index]] <- nesdis_lrgs_header_rows(
        transmission$header,
        transmission$transmission_time,
        order_base
      )
    }

    field_count <- min(length(body_lines), length(fields))
    for (field_index in seq_len(field_count)) {
      raw_values <- strsplit(
        trimws(body_lines[[field_index]]),
        delimiter_pattern,
        perl = TRUE
      )[[1L]]
      raw_values <- raw_values[nzchar(raw_values)]
      if (!length(raw_values)) {
        next
      }
      if (length(raw_values) > 1L && interval_seconds <= 0) {
        stop(
          "downloadNESDIS: BLM field '",
          fields[[field_index]],
          "' contains multiple samples but parser_config.sample_interval_seconds ",
          "is not positive."
        )
      }

      sample_age <- seq.int(0L, length(raw_values) - 1L)
      if (values_order == "oldest_first") {
        sample_age <- rev(sample_age)
      }
      datetimes <- transmission$transmission_time -
        offset_seconds -
        sample_age * interval_seconds
      row_index <- row_index + 1L
      rows[[row_index]] <- data.table::data.table(
        source_field = fields[[field_index]],
        datetime = as.POSIXct(
          as.numeric(datetimes),
          origin = "1970-01-01",
          tz = "UTC"
        ),
        raw_value = raw_values,
        value = vapply(raw_values, nesdis_numeric_value, numeric(1)),
        transmission_order = order_base +
          100L +
          field_index * 1000L +
          seq_along(raw_values)
      )
    }
  }
  nesdis_finalize_parsed_data(rows, length(transmissions))
}

#' @keywords internal
#' @noRd
nesdis_parse_delimited_datetime <- function(values, format_string, timezone) {
  values <- as.character(values)
  if (
    !is.null(format_string) && length(format_string) && nzchar(format_string)
  ) {
    parsed <- as.POSIXct(strptime(values, format_string, tz = timezone))
  } else {
    candidates <- c(
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%d %H:%M:%S",
      "%Y/%m/%d %H:%M:%S"
    )
    parsed <- as.POSIXct(
      rep(NA_real_, length(values)),
      origin = "1970-01-01",
      tz = timezone
    )
    for (candidate in candidates) {
      missing <- is.na(parsed)
      if (!any(missing)) {
        break
      }
      parsed[missing] <- as.POSIXct(
        strptime(values[missing], candidate, tz = timezone)
      )
    }
  }
  as.POSIXct(as.numeric(parsed), origin = "1970-01-01", tz = "UTC")
}

#' @keywords internal
#' @noRd
nesdis_parse_delimited <- function(message, dcp_address, config) {
  has_header <- config$has_header %||% FALSE
  if (
    !is.logical(has_header) || length(has_header) != 1L || is.na(has_header)
  ) {
    stop(
      "downloadNESDIS: route_config.parser_config.has_header must be TRUE or FALSE."
    )
  }
  fields <- if (has_header) character() else nesdis_parser_fields(config, "CSV")
  delimiter <- as.character(config$delimiter %||% ",")
  if (length(delimiter) != 1L || nchar(delimiter, type = "chars") != 1L) {
    stop(
      "downloadNESDIS: route_config.parser_config.delimiter must be one character."
    )
  }
  quote_character <- as.character(config$quote %||% "\"")
  if (length(quote_character) != 1L) {
    stop("downloadNESDIS: route_config.parser_config.quote must be one string.")
  }
  datetime_field <- as.character(config$datetime_field %||% "")
  if (length(datetime_field) != 1L) {
    stop(
      "downloadNESDIS: route_config.parser_config.datetime_field must be one field name."
    )
  }
  datetime_format <- config$datetime_format %||% NULL
  if (!is.null(datetime_format)) {
    datetime_format <- as.character(datetime_format)
    if (length(datetime_format) != 1L) {
      stop(
        "downloadNESDIS: route_config.parser_config.datetime_format must be ",
        "one format string."
      )
    }
  }
  datetime_timezone <- as.character(config$datetime_timezone %||% "UTC")
  if (length(datetime_timezone) != 1L || !nzchar(datetime_timezone)) {
    stop(
      "downloadNESDIS: route_config.parser_config.datetime_timezone must be ",
      "one non-empty timezone."
    )
  }
  record_interval <- nesdis_parser_number(
    config,
    "record_interval_seconds",
    default = 0
  )
  record_offset <- nesdis_parser_number(
    config,
    "record_offset_seconds",
    default = 0
  )
  records_order <- nesdis_parser_choice(
    config,
    "records_order",
    default = "oldest_first",
    choices = c("newest_first", "oldest_first")
  )
  skip_rows <- nesdis_parser_number(config, "skip_rows", default = 0)
  if (skip_rows != as.integer(skip_rows)) {
    stop(
      "downloadNESDIS: route_config.parser_config.skip_rows must be a whole number."
    )
  }
  include_header <- config$include_lrgs_header_fields %||% TRUE
  if (
    !is.logical(include_header) ||
      length(include_header) != 1L ||
      is.na(include_header)
  ) {
    stop(
      "downloadNESDIS: route_config.parser_config.include_lrgs_header_fields ",
      "must be TRUE or FALSE."
    )
  }

  transmissions <- nesdis_extract_lrgs_transmissions(message, dcp_address)
  if (!length(transmissions)) {
    return(nesdis_empty_parsed_data())
  }

  rows <- list()
  row_index <- 0L
  for (transmission in transmissions) {
    body_lines <- transmission$body_lines
    body_lines <- body_lines[
      !grepl(
        "^NO[[:space:]]+DATA([[:space:]]+AVAILABLE.*)?$",
        body_lines,
        ignore.case = TRUE
      )
    ]
    if (skip_rows > 0L && length(body_lines)) {
      body_lines <- body_lines[-seq_len(min(skip_rows, length(body_lines)))]
    }
    if (!length(body_lines)) {
      next
    }

    text_connection <- textConnection(paste(body_lines, collapse = "\n"))
    parsed_table <- tryCatch(
      utils::read.table(
        text_connection,
        header = has_header,
        sep = delimiter,
        quote = quote_character,
        comment.char = "",
        fill = TRUE,
        blank.lines.skip = TRUE,
        colClasses = "character",
        na.strings = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE,
        strip.white = TRUE
      ),
      error = identity
    )
    close(text_connection)
    if (inherits(parsed_table, "error")) {
      stop(
        "downloadNESDIS: Could not parse delimited payload for DCP ",
        dcp_address,
        ": ",
        conditionMessage(parsed_table)
      )
    }
    if (!nrow(parsed_table)) {
      next
    }
    names(parsed_table) <- trimws(names(parsed_table))
    if (!has_header) {
      if (ncol(parsed_table) != length(fields)) {
        stop(
          "downloadNESDIS: Delimited payload for DCP ",
          dcp_address,
          " has ",
          ncol(parsed_table),
          " columns; parser_config.fields defines ",
          length(fields),
          "."
        )
      }
      names(parsed_table) <- fields
    }
    if (
      any(!nzchar(names(parsed_table))) || anyDuplicated(names(parsed_table))
    ) {
      stop(
        "downloadNESDIS: Delimited payload field names must be unique and non-empty."
      )
    }

    if (nzchar(datetime_field)) {
      if (!datetime_field %in% names(parsed_table)) {
        stop(
          "downloadNESDIS: parser_config.datetime_field '",
          datetime_field,
          "' is not present in the delimited payload."
        )
      }
      datetimes <- nesdis_parse_delimited_datetime(
        parsed_table[[datetime_field]],
        datetime_format,
        datetime_timezone
      )
      if (anyNA(datetimes)) {
        stop(
          "downloadNESDIS: Could not parse one or more values in delimited ",
          "datetime field '",
          datetime_field,
          "'."
        )
      }
    } else {
      if (nrow(parsed_table) > 1L && record_interval <= 0) {
        stop(
          "downloadNESDIS: A delimited payload with multiple records requires ",
          "parser_config.datetime_field or a positive ",
          "parser_config.record_interval_seconds."
        )
      }
      record_age <- seq.int(0L, nrow(parsed_table) - 1L)
      if (records_order == "oldest_first") {
        record_age <- rev(record_age)
      }
      datetimes <- transmission$transmission_time -
        record_offset -
        record_age * record_interval
    }

    order_base <- transmission$transmission_sequence * 1000000L
    if (include_header) {
      row_index <- row_index + 1L
      rows[[row_index]] <- nesdis_lrgs_header_rows(
        transmission$header,
        transmission$transmission_time,
        order_base
      )
    }
    value_fields <- setdiff(names(parsed_table), datetime_field)
    for (field_index in seq_along(value_fields)) {
      field <- value_fields[[field_index]]
      raw_values <- as.character(parsed_table[[field]])
      row_index <- row_index + 1L
      rows[[row_index]] <- data.table::data.table(
        source_field = field,
        datetime = as.POSIXct(
          as.numeric(datetimes),
          origin = "1970-01-01",
          tz = "UTC"
        ),
        raw_value = raw_values,
        value = vapply(raw_values, nesdis_numeric_value, numeric(1)),
        transmission_order = order_base +
          100L +
          field_index * 1000L +
          seq_along(raw_values)
      )
    }
  }
  nesdis_finalize_parsed_data(rows, length(transmissions))
}

#' @keywords internal
#' @noRd
nesdis_validate_parser_output <- function(parsed) {
  transmissions_received <- attr(parsed, "transmissions_received") %||% 0L
  parsed <- data.table::as.data.table(parsed)
  required <- c("source_field", "datetime", "raw_value", "value")
  missing <- setdiff(required, names(parsed))
  if (length(missing)) {
    stop(
      "downloadNESDIS: Custom parser output is missing column(s): ",
      paste(missing, collapse = ", "),
      "."
    )
  }
  if (!inherits(parsed$datetime, "POSIXct")) {
    parsed[, datetime := as.POSIXct(datetime, tz = "UTC")]
  }
  parsed[,
    datetime := as.POSIXct(
      as.numeric(datetime),
      origin = "1970-01-01",
      tz = "UTC"
    )
  ]
  parsed[, source_field := as.character(source_field)]
  parsed[, raw_value := as.character(raw_value)]
  parsed[, value := suppressWarnings(as.numeric(value))]
  parsed <- parsed[
    !is.na(datetime) & nzchar(source_field)
  ]
  attr(parsed, "transmissions_received") <- as.integer(transmissions_received)
  parsed
}

#' @keywords internal
#' @noRd
nesdis_parse_shef <- function(message, dcp_address) {
  if (length(message) == 0L || is.na(message) || !nzchar(message)) {
    empty <- data.table::data.table(
      source_field = character(),
      datetime = as.POSIXct(character(), tz = "UTC"),
      raw_value = character(),
      value = numeric()
    )
    attr(empty, "transmissions_received") <- 0L
    return(empty)
  }

  lines <- strsplit(message, "\r\n|\n|\r", perl = TRUE)[[1L]]
  lines <- trimws(lines)
  lines <- lines[startsWith(lines, dcp_address)]
  if (length(lines) == 0L) {
    empty <- data.table::data.table(
      source_field = character(),
      datetime = as.POSIXct(character(), tz = "UTC"),
      raw_value = character(),
      value = numeric()
    )
    attr(empty, "transmissions_received") <- 0L
    return(empty)
  }

  parsed_lines <- lapply(
    lines,
    nesdis_parse_shef_line,
    dcp_address = dcp_address
  )
  valid <- !vapply(parsed_lines, is.null, logical(1))
  parsed_lines <- parsed_lines[valid]
  if (length(parsed_lines) == 0L) {
    stop(
      "downloadNESDIS: Payload for DCP ",
      dcp_address,
      " contained no valid LRGS/SHEF transmissions."
    )
  }

  result <- data.table::rbindlist(parsed_lines, fill = TRUE)
  data.table::setorder(result, source_field, datetime, transmission_order)
  result <- result[, .SD[.N], by = .(source_field, datetime)]
  result[, transmission_order := NULL]
  data.table::setorder(result, datetime, source_field)
  attr(result, "transmissions_received") <- length(parsed_lines)
  result
}

#' @keywords internal
#' @noRd
nesdis_parse_shef_line <- function(line, dcp_address) {
  parts <- strsplit(line, ":", fixed = TRUE)[[1L]]
  header <- parts[[1L]]
  if (
    nchar(header, type = "chars") < 38L ||
      !startsWith(header, dcp_address)
  ) {
    return(NULL)
  }

  timestamp_text <- substr(header, 9L, 19L)
  transmission_time <- as.POSIXct(
    strptime(timestamp_text, format = "%y%j%H%M%S", tz = "UTC")
  )
  if (is.na(transmission_time)) {
    return(NULL)
  }

  header_values <- list(
    YSS = trimws(substr(header, 21L, 22L)),
    DCP_Freq_Drft = trimws(substr(header, 23L, 24L)),
    `Message Size` = trimws(substr(header, 33L, 37L)),
    `GPS Synch` = substr(header, 38L, 38L)
  )
  header_values$YSS <- nesdis_signal_value(header_values$YSS, "YSS")
  header_values$DCP_Freq_Drft <- nesdis_signal_value(
    header_values$DCP_Freq_Drft,
    "DCP_Freq_Drft"
  )
  header_values$`Message Size` <- nesdis_signal_value(
    header_values$`Message Size`,
    "Message Size"
  )
  header_values$`GPS Synch` <- nesdis_signal_value(
    header_values$`GPS Synch`,
    "GPS Synch"
  )

  rows <- list()
  index <- 0L
  for (field in names(header_values)) {
    index <- index + 1L
    raw_value <- as.character(header_values[[field]])
    rows[[index]] <- data.table::data.table(
      source_field = field,
      datetime = transmission_time,
      raw_value = raw_value,
      value = suppressWarnings(as.numeric(raw_value)),
      transmission_order = index
    )
  }

  if (length(parts) > 1L) {
    for (segment in parts[-1L]) {
      segment_rows <- nesdis_parse_shef_segment(
        segment,
        transmission_time,
        index
      )
      if (nrow(segment_rows)) {
        index <- max(segment_rows$transmission_order)
        rows[[length(rows) + 1L]] <- segment_rows
      }
    }
  }
  data.table::rbindlist(rows, fill = TRUE)
}

#' @keywords internal
#' @noRd
nesdis_signal_value <- function(value, field) {
  if (field == "GPS Synch") {
    return(if (identical(value, "\"")) "1" else value)
  }
  value
}

#' @keywords internal
#' @noRd
nesdis_parse_shef_segment <- function(
  segment,
  transmission_time,
  order_offset = 0L
) {
  segment <- trimws(segment)
  if (!nzchar(segment)) {
    return(data.table::data.table())
  }
  tokens <- strsplit(segment, "[[:space:]]+", perl = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) < 2L) {
    return(data.table::data.table())
  }

  source_field <- tokens[[1L]]
  if (!grepl("^[[:alnum:]_.-]+$", source_field)) {
    return(data.table::data.table())
  }

  if (length(tokens) >= 4L) {
    offset_minutes <- suppressWarnings(as.numeric(tokens[[2L]]))
    interval_minutes <- suppressWarnings(as.numeric(
      gsub("[^0-9+.-]", "", tokens[[3L]])
    ))
    if (is.na(offset_minutes) || is.na(interval_minutes)) {
      return(data.table::data.table())
    }
    raw_values <- tokens[4L:length(tokens)]
    datetimes <- transmission_time -
      offset_minutes * 60 -
      seq.int(0, length(raw_values) - 1L) * interval_minutes * 60
  } else {
    raw_values <- tokens[[2L]]
    datetimes <- transmission_time
  }

  values <- vapply(raw_values, nesdis_numeric_value, numeric(1))
  data.table::data.table(
    source_field = source_field,
    datetime = as.POSIXct(
      as.numeric(datetimes),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    raw_value = as.character(raw_values),
    value = values,
    transmission_order = order_offset + seq_along(raw_values)
  )
}

#' @keywords internal
#' @noRd
nesdis_numeric_value <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.na(numeric_value)) {
    return(numeric_value)
  }
  suppressWarnings(as.numeric(gsub("[[:alpha:]]+$", "", value)))
}

#' @keywords internal
#' @noRd
nesdis_apply_mappings <- function(parsed, mappings) {
  if (nrow(parsed) == 0L || nrow(mappings) == 0L) {
    return(nesdis_empty_mapped_data())
  }
  parsed <- data.table::copy(data.table::as.data.table(parsed))
  mappings <- data.table::copy(data.table::as.data.table(mappings))
  mapped <- merge(
    parsed,
    mappings,
    by = "source_field",
    allow.cartesian = TRUE,
    sort = FALSE
  )
  if (nrow(mapped) == 0L) {
    return(nesdis_empty_mapped_data())
  }

  mapped[,
    is_missing_source := vapply(
      seq_len(.N),
      function(i) {
        missing_values <- nesdis_parse_json_array(mapped$missing_values[[i]])
        if (length(missing_values) == 0L) {
          return(FALSE)
        }
        raw <- mapped$raw_value[[i]]
        numeric_raw <- suppressWarnings(as.numeric(raw))
        raw %in%
          as.character(missing_values) ||
          (!is.na(numeric_raw) &&
            numeric_raw %in% suppressWarnings(as.numeric(missing_values)))
      },
      logical(1)
    )
  ]
  mapped[is_missing_source == TRUE, value := NA_real_]
  mapped[,
    value := value * as.numeric(value_multiplier) + as.numeric(value_offset)
  ]
  mapped <- mapped[!is.na(datetime) & !is.na(value)]
  if (nrow(mapped) == 0L) {
    return(nesdis_empty_mapped_data())
  }

  mapped[, transmission_route_id := as.integer(transmission_route_id)]
  mapped[, timeseries_id := as.integer(timeseries_id)]
  data.table::setorder(mapped, timeseries_id, datetime)
  mapped <- mapped[, .SD[.N], by = .(timeseries_id, datetime)]
  mapped[, .(
    transmission_route_id,
    transmission_mapping_id,
    source_field,
    timeseries_id,
    datetime,
    raw_value,
    value
  )]
}

#' @keywords internal
#' @noRd
nesdis_parse_json_array <- function(x) {
  if (is.list(x) && !inherits(x, "pq_json")) {
    return(unlist(x, use.names = FALSE))
  }
  if (length(x) == 0L || is.na(x) || !nzchar(x)) {
    return(character())
  }
  parsed <- jsonlite::fromJSON(as.character(x), simplifyVector = TRUE)
  if (is.null(parsed)) {
    return(character())
  }
  unlist(parsed, use.names = FALSE)
}

#' @keywords internal
#' @noRd
nesdis_empty_mapped_data <- function() {
  data.table::data.table(
    transmission_route_id = integer(),
    transmission_mapping_id = integer(),
    source_field = character(),
    timeseries_id = integer(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    raw_value = character(),
    value = numeric()
  )
}

#' @keywords internal
#' @noRd
nesdis_write_route <- function(
  con,
  route,
  data,
  write,
  overwrite,
  source_server,
  payload_bytes,
  transmissions_received,
  payload_reference,
  source_metadata
) {
  measurements_parsed <- nrow(data)
  last_message_datetime <- if (measurements_parsed) {
    max(data$datetime)
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  status <- if (measurements_parsed) "success" else "no_data"

  if (!write) {
    return(list(
      summary = nesdis_summary_row(
        route,
        status,
        source_server,
        payload_bytes,
        transmissions_received,
        measurements_parsed,
        0L,
        NA_character_
      )
    ))
  }

  started_transaction <- dbTransBegin(con)
  savepoint_name <- "download_nesdis_route"
  if (!started_transaction) {
    DBI::dbExecute(con, paste("SAVEPOINT", savepoint_name))
  }

  tryCatch(
    {
      measurements_inserted <- 0L
      if (measurements_parsed) {
        for (target_timeseries_id in unique(data$timeseries_id)) {
          incoming <- data[timeseries_id == target_timeseries_id]
          existing <- if (overwrite == "no") {
            DBI::dbGetQuery(
              con,
              "SELECT datetime
               FROM continuous.measurements_continuous
               WHERE timeseries_id = $1
                 AND datetime BETWEEN $2 AND $3",
              params = list(
                target_timeseries_id,
                min(incoming$datetime),
                max(incoming$datetime)
              )
            )$datetime
          } else {
            as.POSIXct(character(), tz = "UTC")
          }
          if (length(existing)) {
            incoming <- incoming[!datetime %in% existing]
          }
          if (nrow(incoming) == 0L) {
            next
          }

          addNewContinuous(
            tsid = target_timeseries_id,
            df = incoming[, .(datetime, value)],
            con = con,
            overwrite = overwrite
          )
          measurements_inserted <- measurements_inserted + nrow(incoming)
        }
      }

      nesdis_record_import_run(
        con = con,
        route = route,
        status = status,
        source_server = source_server,
        payload_bytes = payload_bytes,
        transmissions_received = transmissions_received,
        measurements_parsed = measurements_parsed,
        measurements_inserted = measurements_inserted,
        last_message_datetime = last_message_datetime,
        error_message = NA_character_,
        payload_reference = payload_reference,
        source_metadata = source_metadata
      )

      if (started_transaction) {
        DBI::dbExecute(con, "COMMIT")
      } else {
        DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint_name))
      }

      list(
        summary = nesdis_summary_row(
          route,
          status,
          source_server,
          payload_bytes,
          transmissions_received,
          measurements_parsed,
          measurements_inserted,
          NA_character_
        )
      )
    },
    error = function(e) {
      if (started_transaction) {
        try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
      } else {
        try(
          DBI::dbExecute(
            con,
            paste("ROLLBACK TO SAVEPOINT", savepoint_name)
          ),
          silent = TRUE
        )
        try(
          DBI::dbExecute(
            con,
            paste("RELEASE SAVEPOINT", savepoint_name)
          ),
          silent = TRUE
        )
      }

      try(
        nesdis_record_import_run(
          con = con,
          route = route,
          status = "failed",
          source_server = source_server,
          payload_bytes = payload_bytes,
          transmissions_received = transmissions_received,
          measurements_parsed = measurements_parsed,
          measurements_inserted = 0,
          last_message_datetime = last_message_datetime,
          error_message = conditionMessage(e),
          payload_reference = payload_reference,
          source_metadata = source_metadata
        ),
        silent = TRUE
      )

      list(
        summary = nesdis_summary_row(
          route,
          "failed",
          source_server,
          payload_bytes,
          transmissions_received,
          measurements_parsed,
          0L,
          conditionMessage(e)
        )
      )
    }
  )
}

#' @keywords internal
#' @noRd
nesdis_record_import_run <- function(
  con,
  route,
  status,
  source_server,
  payload_bytes,
  transmissions_received,
  measurements_parsed,
  measurements_inserted,
  last_message_datetime,
  error_message,
  payload_reference,
  source_metadata
) {
  DBI::dbExecute(
    con,
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
       $1, $2, $3, 'downloadNESDIS', $4, $5, $6, $7, $8, $9,
       $10, $11, $12::jsonb, $13, clock_timestamp()
     )",
    params = list(
      route$transmission_route_id,
      route$query_since,
      route$query_until,
      source_server,
      status,
      as.numeric(payload_bytes),
      as.integer(transmissions_received),
      as.integer(measurements_parsed),
      as.integer(measurements_inserted),
      last_message_datetime,
      payload_reference,
      jsonlite::toJSON(
        source_metadata,
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      ),
      error_message
    )
  )
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
nesdis_summary_row <- function(
  route,
  status,
  source_server,
  payload_bytes,
  transmissions_received,
  measurements_parsed,
  measurements_inserted,
  error_message
) {
  data.table::data.table(
    transmission_route_id = route$transmission_route_id,
    route_name = route$route_name,
    dcp_address = route$platform_identifier,
    message_format = route$message_format,
    query_since = route$query_since,
    query_until = route$query_until,
    status = status,
    source_server = source_server,
    payload_bytes = as.numeric(payload_bytes),
    transmissions_received = as.integer(transmissions_received),
    measurements_parsed = as.integer(measurements_parsed),
    measurements_inserted = as.integer(measurements_inserted),
    error_message = error_message
  )
}
