# Configure direct NESDIS imports for AquaCache timeseries currently sourced
# from RWDM.
#
# The script is deliberately dry-run by default. It reads station telemetry
# metadata from RWDM, matches it to AquaCache downloadRWIS adapter arguments,
# and creates the provider-neutral Patch 56 setup, route, field mappings, and
# adapter assignments in one AquaCache transaction.
#
# Run from the AquaCache repository root. Connection values are read from
# ../.Renviron unless they are already present in the process environment.
#
# Preview Antimony Creek on dev:
#   $env:RWIS_NESDIS_AQUACACHE_HOST = "10.250.12.154"
#   $env:RWIS_NESDIS_STATIONS = "ANT"
#   & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' `
#     inst/scripts/configure_RWIS_NESDIS_migration.R
#
# Apply Antimony Creek on dev:
#   $env:RWIS_NESDIS_APPLY = "YES"
#   & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' `
#     inst/scripts/configure_RWIS_NESDIS_migration.R
#
# Preview all active matched RWDM stations:
#   $env:RWIS_NESDIS_STATIONS = "ALL"
#   $env:RWIS_NESDIS_APPLY = "NO"
#   & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' `
#     inst/scripts/configure_RWIS_NESDIS_migration.R
#
# Applying ALL additionally requires RWIS_NESDIS_CONFIRM_BULK=YES. The apply
# is refused while any source field is unresolved. SHEF field codes and BLM
# precipitation products that are derived rather than direct payload fields
# must be entered in mapping_overrides below after checking a raw message.

suppressPackageStartupMessages({
  library(data.table)
  library(DBI)
})

renviron_path <- file.path("..", ".Renviron")
if (file.exists(renviron_path)) {
  readRenviron(renviron_path)
}

apply_changes <- identical(
  toupper(trimws(Sys.getenv("RWIS_NESDIS_APPLY", "NO"))),
  "YES"
)
include_inactive_stations <- identical(
  toupper(trimws(Sys.getenv("RWIS_NESDIS_INCLUDE_INACTIVE", "NO"))),
  "YES"
)
station_selection <- trimws(strsplit(
  Sys.getenv("RWIS_NESDIS_STATIONS", "ANT"),
  ",",
  fixed = TRUE
)[[1L]])
station_selection <- toupper(station_selection[nzchar(station_selection)])
if (!length(station_selection)) {
  stop("RWIS_NESDIS_STATIONS must name at least one station or ALL.")
}
bulk_mode <- identical(station_selection, "ALL")
if ("ALL" %in% station_selection && !bulk_mode) {
  stop("Use ALL by itself in RWIS_NESDIS_STATIONS.")
}
if (
  apply_changes &&
    bulk_mode &&
    !identical(
      toupper(trimws(Sys.getenv("RWIS_NESDIS_CONFIRM_BULK", "NO"))),
      "YES"
    )
) {
  stop("Applying ALL requires RWIS_NESDIS_CONFIRM_BULK=YES.")
}

# Add only mappings confirmed from raw payloads. Parameter is the value in the
# existing downloadRWIS source_fx_args. One row overrides one station/series.
# value_multiplier and value_offset are applied after missing-value handling.
mapping_overrides <- data.table(
  station = character(),
  parameter = character(),
  source_field = character(),
  value_multiplier = numeric(),
  value_offset = numeric()
)

# Provider/station payloads can override the defaults below. Delimited layouts
# cannot be inferred from the RWDM transmission_format label and therefore
# always need an override. Examples:
# route_parser_overrides <- list(
#   JPK = list(
#     has_header = TRUE,
#     delimiter = ",",
#     datetime_field = "datetime_utc",
#     datetime_format = "%Y/%m/%d %H:%M:%S",
#     datetime_timezone = "UTC"
#   ),
#   POOL = list(
#     has_header = FALSE,
#     delimiter = ",",
#     fields = c("STN", "ta", "rh"),
#     record_interval_seconds = 3600,
#     record_offset_seconds = 1800,
#     records_order = "oldest_first"
#   )
# )
route_parser_overrides <- list()

required_environment <- c(
  "aquacacheName",
  "aquacachePort",
  "aquacacheAdminUser",
  "aquacacheAdminPass",
  "rwisName",
  "rwisHost",
  "rwisPort",
  "rwisUser",
  "rwisPass"
)
missing_environment <- required_environment[!nzchar(Sys.getenv(
  required_environment
))]
if (length(missing_environment)) {
  stop(
    "Missing database environment variable(s): ",
    paste(missing_environment, collapse = ", "),
    "."
  )
}

aquacache_host <- Sys.getenv(
  "RWIS_NESDIS_AQUACACHE_HOST",
  Sys.getenv("aquacacheHost")
)
aquacache_name <- Sys.getenv(
  "RWIS_NESDIS_AQUACACHE_NAME",
  Sys.getenv("aquacacheName")
)
if (!nzchar(aquacache_host) || !nzchar(aquacache_name)) {
  stop(
    "Set RWIS_NESDIS_AQUACACHE_HOST and, if needed, ",
    "RWIS_NESDIS_AQUACACHE_NAME."
  )
}

aquacache <- dbConnect(
  RPostgres::Postgres(),
  dbname = aquacache_name,
  host = aquacache_host,
  port = Sys.getenv("aquacachePort"),
  user = Sys.getenv("aquacacheAdminUser"),
  password = Sys.getenv("aquacacheAdminPass")
)
rwdm <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("rwisName"),
  host = Sys.getenv("rwisHost"),
  port = Sys.getenv("rwisPort"),
  user = Sys.getenv("rwisUser"),
  password = Sys.getenv("rwisPass")
)
on.exit({
  if (dbIsValid(rwdm)) {
    dbDisconnect(rwdm)
  }
  if (dbIsValid(aquacache)) {
    dbDisconnect(aquacache)
  }
}, add = TRUE)
invisible(dbExecute(aquacache, "SET timezone = 'UTC'"))

target <- dbGetQuery(
  aquacache,
  "SELECT current_database() AS database_name,
          inet_server_addr()::text AS server_address,
          current_user AS database_user"
)
message(
  if (apply_changes) "APPLY target: " else "DRY-RUN target: ",
  target$database_name[[1L]],
  " on ",
  target$server_address[[1L]],
  " as ",
  target$database_user[[1L]],
  "."
)

required_schema <- dbGetQuery(
  aquacache,
  "SELECT
     to_regclass('public.source_adapter_capabilities') IS NOT NULL AS registry,
     to_regclass('continuous.timeseries_source_adapters') IS NOT NULL AS adapters,
     to_regclass('continuous.transmission_timeseries_mappings') IS NOT NULL AS mappings,
     to_regclass('public.locations_metadata_transmission_setups') IS NOT NULL AS setups,
     to_regclass('public.locations_metadata_transmission_routes') IS NOT NULL AS routes"
)
if (!all(unlist(required_schema[1L, ], use.names = FALSE))) {
  stop("The target AquaCache database does not have the complete Patch 56 schema.")
}
capabilities <- dbGetQuery(
  aquacache,
  "SELECT source_fx
   FROM public.source_adapter_capabilities
   WHERE data_domain = 'continuous'
     AND source_fx IN ('downloadNESDIS', 'downloadRWIS')"
)$source_fx
if (!setequal(capabilities, c("downloadNESDIS", "downloadRWIS"))) {
  stop("The target adapter registry must contain downloadNESDIS and downloadRWIS.")
}

rwdm_stations <- as.data.table(dbGetQuery(
  rwdm,
  "SELECT
     abbreviation AS station,
     name AS station_name,
     is_active AS station_active,
     monitoring_start_date,
     monitoring_end_date,
     data_timezone,
     datalogger_model,
     transmission_format,
     transmission_frequency,
     transmission_time,
     upper(btrim(goes_address)) AS dcp_address
   FROM stations_station
   WHERE goes_address IS NOT NULL
     AND btrim(goes_address) <> ''"
))
rwdm_stations[, station := toupper(trimws(station))]
rwdm_stations[, transmission_format := trimws(transmission_format)]

rwis_adapters <- as.data.table(dbGetQuery(
  aquacache,
  "SELECT
     tsa.timeseries_source_adapter_id AS rwis_adapter_id,
     tsa.timeseries_id,
     tsa.source_fx_args::text AS rwis_source_fx_args,
     tsa.fetch_priority AS rwis_fetch_priority,
     tsa.synchronize_priority AS rwis_synchronize_priority,
     tsa.active AS rwis_adapter_active,
     t.location_id,
     l.location_code,
     l.name AS location_name,
     p.param_name,
     jsonb_extract_path_text(tsa.source_fx_args, 'location') AS station,
     jsonb_extract_path_text(tsa.source_fx_args, 'parameter') AS parameter
   FROM continuous.timeseries_source_adapters tsa
   JOIN continuous.timeseries t USING (timeseries_id)
   JOIN public.locations l USING (location_id)
   JOIN public.parameters p USING (parameter_id)
   WHERE tsa.source_fx = 'downloadRWIS'
   ORDER BY tsa.timeseries_id"
))
rwis_adapters[, station := toupper(trimws(station))]
rwis_adapters[, parameter := trimws(parameter)]

duplicate_rwis <- rwis_adapters[, .N, by = timeseries_id][N > 1L]
if (nrow(duplicate_rwis)) {
  stop(
    "More than one downloadRWIS adapter exists for timeseries_id(s): ",
    paste(duplicate_rwis$timeseries_id, collapse = ", "),
    "."
  )
}

plan <- merge(
  rwis_adapters,
  rwdm_stations,
  by = "station",
  all = FALSE,
  sort = FALSE
)
if (bulk_mode) {
  if (!include_inactive_stations) {
    plan <- plan[station_active == TRUE]
  }
} else {
  missing_stations <- setdiff(station_selection, plan$station)
  if (length(missing_stations)) {
    stop(
      "No matched RWDM GOES/downloadRWIS series were found for station(s): ",
      paste(missing_stations, collapse = ", "),
      "."
    )
  }
  plan <- plan[station %in% station_selection]
}
if (!nrow(plan)) {
  stop("No matched timeseries remain after applying the station filters.")
}

plan[, format_key := tolower(gsub("[^[:alnum:]]", "", transmission_format))]
plan[
  format_key == "csv",
  transmission_format := "comma-delimited"
]
plan[
  format_key == "commadelimited",
  transmission_format := "comma-delimited"
]
plan[format_key == "blm", transmission_format := "BLM"]
plan[format_key == "shef", transmission_format := "SHEF"]
plan[, format_key := tolower(gsub("[^[:alnum:]]", "", transmission_format))]

plan[, source_field := fifelse(
  format_key == "blm" & parameter %in% c("rn1", "ws", "wd", "ta", "rh", "vb"),
  parameter,
  fifelse(
    format_key == "commadelimited" & station %in% names(route_parser_overrides),
    parameter,
    NA_character_
  )
)]
plan[, `:=`(value_multiplier = 1, value_offset = 0)]

if (nrow(mapping_overrides)) {
  mapping_overrides[, station := toupper(trimws(station))]
  mapping_overrides[, parameter := trimws(parameter)]
  if (anyDuplicated(mapping_overrides, by = c("station", "parameter"))) {
    stop("mapping_overrides contains duplicate station/parameter rows.")
  }
  plan[mapping_overrides, on = .(station, parameter), `:=`(
    source_field = i.source_field,
    value_multiplier = i.value_multiplier,
    value_offset = i.value_offset
  )]
}

station_issues <- unique(plan[
  is.na(dcp_address) |
    !grepl("^[[:xdigit:]]{8}$", dcp_address) |
    !format_key %in% c("blm", "shef", "commadelimited"),
  .(station, dcp_address, transmission_format)
])
unresolved <- plan[
  is.na(source_field) | !nzchar(trimws(source_field)),
  .(
    station,
    station_name,
    transmission_format,
    timeseries_id,
    parameter,
    param_name
  )
]
duplicate_fields <- plan[
  !is.na(source_field),
  .N,
  by = .(station, source_field)
][N > 1L]
invalid_transforms <- plan[
  is.na(value_multiplier) |
    !is.finite(value_multiplier) |
    value_multiplier == 0 |
    is.na(value_offset) |
    !is.finite(value_offset)
]

routes <- unique(plan[, .(
  station,
  station_name,
  station_active,
  location_id,
  location_code,
  dcp_address,
  transmission_format,
  format_key,
  monitoring_start_date,
  monitoring_end_date,
  data_timezone,
  datalogger_model,
  transmission_frequency,
  transmission_time
)])
ambiguous_locations <- routes[, .N, by = station][N > 1L]
if (nrow(ambiguous_locations)) {
  stop(
    "A station matched more than one AquaCache location: ",
    paste(ambiguous_locations$station, collapse = ", "),
    "."
  )
}

interval_seconds <- function(value) {
  value <- tolower(trimws(value))
  fifelse(
    value %in% c("hourly", "1 hour", "1-hourly"),
    3600L,
    fifelse(value %in% c("3 hours", "3-hourly", "three-hourly"), 10800L, NA_integer_)
  )
}
routes[, transmit_interval_seconds := interval_seconds(
  transmission_frequency
)]
routes[, schedule_reference_time_utc := fifelse(
  grepl("^[0-2][0-9]:[0-5][0-9](:[0-5][0-9])?$", transmission_time),
  transmission_time,
  NA_character_
)]
routes[, route_name := paste(station, "GOES", transmission_format)]

blm_fields <- c(
  "blm_row_01",
  "blm_row_02",
  "ws",
  "wd",
  "rn1",
  "wg",
  "tmax1",
  "tmin1",
  "rhmax1",
  "rhmin1",
  "vb",
  "blm_row_12",
  "ta",
  "rh"
)
routes[, route_config := mapply(
  function(format, station, transmit_interval_seconds) {
    parser_config <- route_parser_overrides[[station]]
    if (is.null(parser_config)) {
      parser_config <- switch(
        format,
        blm = list(
          fields = blm_fields,
          sample_interval_seconds = 900,
          sample_offset_seconds = 0,
          timestamp_floor_seconds = transmit_interval_seconds,
          values_order = "oldest_first",
          strict_field_count = TRUE,
          include_lrgs_header_fields = TRUE
        ),
        shef = list(),
        list()
      )
    }
    if (is.null(parser_config)) {
      parser_config <- list()
    }
    if (
      format == "commadelimited" &&
        is.null(parser_config$include_lrgs_header_fields)
    ) {
      parser_config$include_lrgs_header_fields <- TRUE
    }
    jsonlite::toJSON(
      list(
        max_days = 14,
        overlap_minutes = 5,
        parser_config = parser_config
      ),
      auto_unbox = TRUE,
      null = "null"
    )
  },
  format_key,
  station,
  transmit_interval_seconds,
  USE.NAMES = FALSE
)]

missing_route_config <- routes[
  format_key == "commadelimited" &
    !station %in% names(route_parser_overrides)
]

cat("\nTransmission routes:\n")
print(routes[, .(
  station,
  station_name,
  location_id,
  dcp_address,
  transmission_format,
  schedule_reference_time_utc,
  transmit_interval_seconds
)])
cat("\nTimeseries mappings and adapter changes:\n")
print(plan[, .(
  station,
  timeseries_id,
  parameter,
  param_name,
  source_field,
  rwis_adapter_id,
  rwis_adapter_active
)][order(station, timeseries_id)])

if (nrow(station_issues)) {
  cat("\nUnsupported or incomplete station metadata:\n")
  print(station_issues)
}
if (nrow(unresolved)) {
  cat("\nUnresolved source fields (add confirmed mapping_overrides):\n")
  print(unresolved)
}
if (nrow(duplicate_fields)) {
  cat("\nDuplicate source fields within a route:\n")
  print(duplicate_fields)
}
if (nrow(invalid_transforms)) {
  cat("\nInvalid mapping transforms:\n")
  print(invalid_transforms[, .(
    station,
    timeseries_id,
    parameter,
    value_multiplier,
    value_offset
  )])
}
if (nrow(missing_route_config)) {
  cat("\nDelimited routes needing route_parser_overrides:\n")
  print(missing_route_config[, .(
    station,
    station_name,
    dcp_address,
    transmission_format
  )])
}

if (!apply_changes) {
  message("Dry run complete; no AquaCache rows were changed.")
  quit(save = "no", status = 0L)
}
if (
  nrow(station_issues) ||
    nrow(unresolved) ||
    nrow(duplicate_fields) ||
    nrow(invalid_transforms) ||
    nrow(missing_route_config)
) {
  stop("Apply refused until every reported route and source-field issue is resolved.")
}
if (anyNA(routes$monitoring_start_date)) {
  stop(
    "Apply refused because monitoring_start_date is missing for station(s): ",
    paste(routes[is.na(monitoring_start_date)]$station, collapse = ", "),
    "."
  )
}
if (any(is.na(routes$transmit_interval_seconds))) {
  stop(
    "Apply refused because RWDM transmission_frequency could not be converted ",
    "for station(s): ",
    paste(routes[is.na(transmit_interval_seconds)]$station, collapse = ", "),
    "."
  )
}

other_active <- as.data.table(dbGetQuery(
  aquacache,
  paste0(
    "SELECT timeseries_id, source_fx, fetch_priority, synchronize_priority\n",
    "FROM continuous.timeseries_source_adapters\n",
    "WHERE active\n",
    "  AND source_fx NOT IN ('downloadRWIS', 'downloadNESDIS')\n",
    "  AND timeseries_id IN (",
    paste(plan$timeseries_id, collapse = ", "),
    ")"
  )
))
if (nrow(other_active)) {
  print(other_active)
  stop("Apply refused because selected timeseries have another active adapter.")
}

transaction_started <- dbBegin(aquacache)
committed <- FALSE
on.exit({
  if (transaction_started && !committed && dbIsValid(aquacache)) {
    dbRollback(aquacache)
  }
}, add = TRUE)

tryCatch(
  {
    dbExecute(
      aquacache,
      "SELECT pg_advisory_xact_lock(hashtext('configure_RWIS_NESDIS_migration'))"
    )
    transmission_method <- dbGetQuery(
      aquacache,
      "SELECT transmission_method_id
       FROM instruments.transmission_methods
       WHERE method_code = 'GOES_DCS'"
    )
    if (nrow(transmission_method) != 1L) {
      stop("Exactly one GOES_DCS transmission method is required.")
    }
    transmission_method_id <- transmission_method$transmission_method_id[[1L]]

    for (route_index in seq_len(nrow(routes))) {
      route <- routes[route_index]
      route_plan <- plan[station == route$station]

      setup <- dbGetQuery(
        aquacache,
        "SELECT transmission_setup_id
         FROM public.locations_metadata_transmission_setups
         WHERE location_id = $1
           AND transmission_method_id = $2
           AND upper(coalesce(provider_name, '')) = 'NESDIS'
           AND upper(coalesce(platform_identifier, '')) = $3
           AND end_datetime IS NULL
         FOR UPDATE",
        params = list(
          route$location_id,
          transmission_method_id,
          route$dcp_address
        )
      )
      if (nrow(setup) > 1L) {
        stop("Multiple current NESDIS setups exist for station ", route$station, ".")
      }
      if (!nrow(setup)) {
        setup <- dbGetQuery(
          aquacache,
          "INSERT INTO public.locations_metadata_transmission_setups (
             location_id,
             logger_metadata_id,
             transmission_method_id,
             provider_name,
             platform_identifier,
             transmission_config,
             note,
             start_datetime,
             end_datetime
           ) VALUES ($1, NULL, $2, 'NESDIS', $3, '{}'::jsonb, $4, $5, NULL)
           RETURNING transmission_setup_id",
          params = list(
            route$location_id,
            transmission_method_id,
            route$dcp_address,
            paste0("Migrated from RWDM station ", route$station, "."),
            as.POSIXct(route$monitoring_start_date, tz = "UTC")
          )
        )
      }
      setup_id <- setup$transmission_setup_id[[1L]]

      existing_route <- dbGetQuery(
        aquacache,
        "SELECT transmission_route_id
         FROM public.locations_metadata_transmission_routes
         WHERE transmission_setup_id = $1
           AND route_name = $2
         FOR UPDATE",
        params = list(setup_id, route$route_name)
      )
      if (nrow(existing_route) > 1L) {
        stop("Duplicate named routes exist for station ", route$station, ".")
      }
      if (!nrow(existing_route)) {
        existing_route <- dbGetQuery(
          aquacache,
          "INSERT INTO public.locations_metadata_transmission_routes (
             transmission_setup_id,
             route_name,
             endpoint_identifier,
             message_format,
             schedule_reference_time_utc,
             transmit_interval_seconds,
             route_config,
             note
           ) VALUES ($1, $2, $3, $4, $5::time, $6, $7::jsonb, $8)
           RETURNING transmission_route_id",
          params = list(
            setup_id,
            route$route_name,
            route$dcp_address,
            route$transmission_format,
            route$schedule_reference_time_utc,
            route$transmit_interval_seconds,
            route$route_config,
            paste0("Configured from RWDM station ", route$station, " metadata.")
          )
        )
      } else {
        dbExecute(
          aquacache,
          "UPDATE public.locations_metadata_transmission_routes
           SET endpoint_identifier = $2,
               message_format = $3,
               schedule_reference_time_utc = $4::time,
               transmit_interval_seconds = $5,
               route_config = $6::jsonb
           WHERE transmission_route_id = $1",
          params = list(
            existing_route$transmission_route_id[[1L]],
            route$dcp_address,
            route$transmission_format,
            route$schedule_reference_time_utc,
            route$transmit_interval_seconds,
            route$route_config
          )
        )
      }
      route_id <- existing_route$transmission_route_id[[1L]]

      for (mapping_index in seq_len(nrow(route_plan))) {
        mapping <- route_plan[mapping_index]
        dbExecute(
          aquacache,
          "INSERT INTO continuous.transmission_timeseries_mappings (
             transmission_route_id,
             source_field,
             timeseries_id,
             value_multiplier,
             value_offset,
             missing_values,
             mapping_config,
             enabled,
             note
           ) VALUES ($1, $2, $3, $4, $5, '[\"-9999\"]'::jsonb,
                     '{}'::jsonb, TRUE, $6)
           ON CONFLICT (transmission_route_id, timeseries_id) DO UPDATE
           SET source_field = EXCLUDED.source_field,
               value_multiplier = EXCLUDED.value_multiplier,
               value_offset = EXCLUDED.value_offset,
               missing_values = EXCLUDED.missing_values,
               mapping_config = EXCLUDED.mapping_config,
               enabled = TRUE",
          params = list(
            route_id,
            mapping$source_field,
            mapping$timeseries_id,
            mapping$value_multiplier,
            mapping$value_offset,
            paste0("Direct mapping from RWDM station ", route$station, ".")
          )
        )

        dbExecute(
          aquacache,
          "UPDATE continuous.timeseries_source_adapters
           SET active = FALSE,
               fetch_priority = 2,
               synchronize_priority = 2
           WHERE timeseries_source_adapter_id = $1",
          params = list(mapping$rwis_adapter_id)
        )
        nesdis_adapter <- dbGetQuery(
          aquacache,
          "SELECT timeseries_source_adapter_id
           FROM continuous.timeseries_source_adapters
           WHERE timeseries_id = $1
             AND source_fx = 'downloadNESDIS'
           FOR UPDATE",
          params = list(mapping$timeseries_id)
        )
        if (nrow(nesdis_adapter) > 1L) {
          stop(
            "Multiple downloadNESDIS adapters exist for timeseries_id ",
            mapping$timeseries_id,
            "."
          )
        }
        if (!nrow(nesdis_adapter)) {
          dbExecute(
            aquacache,
            "INSERT INTO continuous.timeseries_source_adapters (
               timeseries_id,
               source_fx,
               source_fx_args,
               fetch_priority,
               synchronize_priority,
               active,
               note
             ) VALUES ($1, 'downloadNESDIS', '{}'::jsonb, 1, 1, TRUE, $2)",
            params = list(
              mapping$timeseries_id,
              paste0("Primary direct GOES import for RWDM station ", route$station, ".")
            )
          )
        } else {
          dbExecute(
            aquacache,
            "UPDATE continuous.timeseries_source_adapters
             SET source_fx_args = '{}'::jsonb,
                 fetch_priority = 1,
                 synchronize_priority = 1,
                 active = TRUE
             WHERE timeseries_source_adapter_id = $1",
            params = list(nesdis_adapter$timeseries_source_adapter_id[[1L]])
          )
        }
      }
    }

    dbCommit(aquacache)
    committed <- TRUE
  },
  error = function(error) {
    if (transaction_started && !committed && dbIsValid(aquacache)) {
      dbRollback(aquacache)
      transaction_started <<- FALSE
    }
    stop(conditionMessage(error), call. = FALSE)
  }
)

final_state <- as.data.table(dbGetQuery(
  aquacache,
  paste0(
    "SELECT\n",
    "  t.timeseries_id,\n",
    "  a.source_fx,\n",
    "  a.fetch_priority,\n",
    "  a.synchronize_priority,\n",
    "  a.active\n",
    "FROM continuous.timeseries t\n",
    "JOIN continuous.timeseries_source_adapters a USING (timeseries_id)\n",
    "WHERE t.timeseries_id IN (",
    paste(plan$timeseries_id, collapse = ", "),
    ")\n",
    "ORDER BY t.timeseries_id, a.active DESC, a.fetch_priority, a.source_fx"
  )
))
cat("\nCommitted adapter state:\n")
print(final_state)
message(
  "Configured ",
  nrow(routes),
  " transmission route(s) and ",
  nrow(plan),
  " timeseries mapping(s) on ",
  target$database_name[[1L]],
  "."
)
