# Migrate AquaCache snow-course results to component-backed aggregations.
#
# This script is rerunnable and deliberately dry-run by default. It corrects
# legacy snow results from liquid to solid while preserving their result_id,
# removes only unprotected duplicate solid rows that have no dependent import
# or aggregation records, and then uses synchronize_discrete() to replace each
# unprotected source result with its SnowDB observations and calculated mean.
# Each sample series is committed independently; a warning or error rolls back
# that series. Rows protected by samples.no_source_update or
# results.no_source_update are never changed.
#
# Install the AquaCache package containing the component-capable
# downloadSnowCourseYG() before applying this script.
# Take and verify a database backup before an apply run, especially on
# production. Run the dry-run against the same target immediately beforehand.
#
# Run from the AquaCache repository root. Connection values are read from
# ../.Renviron unless already present in the process environment.
#
# Preview dev:
#   $env:SNOW_COMPONENTS_AQUACACHE_HOST = "10.250.12.154"
#   & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' `
#     inst/patches/migrate_snow_course_components.R
#
# Apply dev:
#   $env:SNOW_COMPONENTS_APPLY = "YES"
#   & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' `
#     inst/patches/migrate_snow_course_components.R
#
# Retry selected series only:
#   $env:SNOW_COMPONENTS_SERIES = "12,34"
# If SnowDB is not on the AquaCache host:
#   $env:SNOW_COMPONENTS_SNOW_HOST = "10.250.12.154"
#
# Applying to the production host has a second explicit guard:
#   $env:SNOW_COMPONENTS_CONFIRM_PRODUCTION = "YES"

suppressPackageStartupMessages({
  library(AquaCache)
  library(data.table)
  library(DBI)
})

run_snow_component_migration <- function(
  apply_changes = NULL,
  aquacache = NULL,
  snow = NULL,
  target_host = NULL,
  target_name = NULL,
  target_port = NULL,
  snow_host = NULL,
  snow_name = NULL,
  snow_port = NULL,
  confirm_production = NULL
) {
  renviron_path <- file.path("..", ".Renviron")
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }

  is_yes <- function(name, default = "NO") {
    identical(toupper(trimws(Sys.getenv(name, default))), "YES")
  }

  if (is.null(apply_changes)) {
    apply_changes <- is_yes("SNOW_COMPONENTS_APPLY")
  }
  if (length(apply_changes) != 1L || is.na(apply_changes)) {
    stop("apply_changes must be one non-missing logical value.")
  }
  apply_changes <- isTRUE(apply_changes)
  if (is.null(confirm_production)) {
    confirm_production <- is_yes("SNOW_COMPONENTS_CONFIRM_PRODUCTION")
  }
  if (length(confirm_production) != 1L || is.na(confirm_production)) {
    stop("confirm_production must be one non-missing logical value.")
  }
  confirm_production <- isTRUE(confirm_production)

  supplied_aquacache <- !is.null(aquacache)
  supplied_snow <- !is.null(snow)
  if (is.null(target_host)) {
    target_host <- trimws(Sys.getenv("SNOW_COMPONENTS_AQUACACHE_HOST"))
  }
  if (is.null(target_name)) {
    target_name <- Sys.getenv(
      "SNOW_COMPONENTS_AQUACACHE_NAME",
      Sys.getenv("aquacacheName")
    )
  }
  if (is.null(target_port)) {
    target_port <- Sys.getenv(
      "SNOW_COMPONENTS_AQUACACHE_PORT",
      Sys.getenv("aquacachePort")
    )
  }
  if (is.null(snow_host)) {
    snow_host <- Sys.getenv("SNOW_COMPONENTS_SNOW_HOST", target_host)
  }
  if (is.null(snow_name)) {
    snow_name <- Sys.getenv("SNOW_COMPONENTS_SNOW_NAME", "snow")
  }
  if (is.null(snow_port)) {
    snow_port <- Sys.getenv(
      "SNOW_COMPONENTS_SNOW_PORT",
      Sys.getenv("snowPort")
    )
  }

  required_environment <- c(
    aquacacheAdminUser = Sys.getenv("aquacacheAdminUser"),
    aquacacheAdminPass = Sys.getenv("aquacacheAdminPass"),
    snow_user = Sys.getenv("snowUser", Sys.getenv("snowAdminUser")),
    snow_password = Sys.getenv("snowPass", Sys.getenv("snowAdminPass"))
  )
  missing_environment <- names(required_environment)[
    !nzchar(required_environment)
  ]
  missing_connection_settings <- c(
    if (!supplied_aquacache && !nzchar(target_host)) "AquaCache host",
    if (!supplied_aquacache && !nzchar(target_name)) "AquaCache database name",
    if (!supplied_aquacache && !nzchar(target_port)) "AquaCache port",
    if (!supplied_snow && !nzchar(snow_host)) "SnowDB host",
    if (!supplied_snow && !nzchar(snow_port)) "SnowDB port",
    if (!supplied_aquacache) {
      missing_environment[missing_environment %in% c(
        "aquacacheAdminUser",
        "aquacacheAdminPass"
      )]
    },
    if (!supplied_snow) {
      missing_environment[missing_environment %in% c(
        "snow_user",
        "snow_password"
      )]
    }
  )
  if (length(missing_connection_settings)) {
    stop(
      "Missing database setting(s): ",
      paste(missing_connection_settings, collapse = ", "),
      "."
    )
  }

  production_host <- "199.247.132.26"
  if (
    apply_changes &&
      identical(target_host, production_host) &&
      !confirm_production
  ) {
    stop(
      "Applying to production requires ",
      "SNOW_COMPONENTS_CONFIRM_PRODUCTION=YES."
    )
  }

  adapter_body <- paste(deparse(body(downloadSnowCourseYG)), collapse = "\n")
  if (!grepl("result_components", adapter_body, fixed = TRUE)) {
    stop(
      "The loaded AquaCache package does not contain the component-capable ",
      "downloadSnowCourseYG(). Install the updated package first."
    )
  }

  postgres_driver <- RPostgres::Postgres()
  if (!supplied_aquacache) {
    message("Connecting to AquaCache target...")
    aquacache <- dbConnect(
      postgres_driver,
      dbname = target_name,
      host = target_host,
      port = target_port,
      user = required_environment[["aquacacheAdminUser"]],
      password = required_environment[["aquacacheAdminPass"]]
    )
    on.exit(
      {
        if (dbIsValid(aquacache)) dbDisconnect(aquacache)
      },
      add = TRUE
    )
  }
  if (!DBI::dbIsValid(aquacache)) {
    stop("The supplied AquaCache connection is not valid.")
  }
  if (!supplied_snow) {
    message(
      "Connecting to SnowDB ",
      snow_name,
      " on ",
      snow_host,
      ":",
      snow_port,
      " as ",
      required_environment[["snow_user"]],
      "..."
    )
    snow <- AquaCache::snowConnect(
      name = snow_name,
      host = snow_host,
      port = snow_port,
      username = required_environment[["snow_user"]],
      password = required_environment[["snow_password"]],
      silent = TRUE
    )
    on.exit(
      {
        if (dbIsValid(snow)) dbDisconnect(snow)
      },
      add = TRUE
    )
  }
  if (!DBI::dbIsValid(snow)) {
    stop("The supplied SnowDB connection is not valid.")
  }
  invisible(dbExecute(aquacache, "SET timezone = 'UTC'"))
  invisible(dbExecute(snow, "SET timezone = 'UTC'"))

  target <- dbGetQuery(
    aquacache,
    "SELECT current_database() AS database_name,
          host(inet_server_addr()) AS server_address,
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
  if (
    apply_changes &&
      identical(target$server_address[[1L]], production_host) &&
      !confirm_production
  ) {
    stop(
      "The connected server is production. Applying requires ",
      "SNOW_COMPONENTS_CONFIRM_PRODUCTION=YES."
    )
  }

  required_schema <- dbGetQuery(
    aquacache,
    "SELECT
     to_regclass('discrete.result_aggregation_types') IS NOT NULL AS types,
     to_regclass('discrete.result_aggregations') IS NOT NULL AS aggregations,
     to_regclass('discrete.result_components') IS NOT NULL AS components,
     to_regclass('discrete.result_aggregation_summary') IS NOT NULL AS summary"
  )
  if (!all(unlist(required_schema[1L, ], use.names = FALSE))) {
    stop(
      "The target AquaCache database does not have the complete Patch 60 schema."
    )
  }

  states <- dbGetQuery(
    aquacache,
    "SELECT matrix_state_id, matrix_state_code
   FROM public.matrix_states
   WHERE matrix_state_code IN ('liquid', 'solid')"
  )
  if (!setequal(states$matrix_state_code, c("liquid", "solid"))) {
    stop("Could not resolve both liquid and solid matrix states.")
  }
  liquid_id <- states$matrix_state_id[states$matrix_state_code == "liquid"]
  solid_id <- states$matrix_state_id[states$matrix_state_code == "solid"]

  series <- as.data.table(dbGetQuery(
    aquacache,
    "SELECT ss.sample_series_id, ss.location_id, l.location_code,
          jsonb_extract_path_text(ssa.source_fx_args, 'location')
            AS source_location,
          jsonb_extract_path_text(ssa.source_fx_args, 'old_loc')
            AS old_source_location,
          ssa.source_fx_args::text AS source_fx_args
   FROM discrete.sample_series ss
   JOIN public.locations l USING (location_id)
   JOIN discrete.sample_series_source_adapters ssa USING (sample_series_id)
   WHERE ssa.source_fx = 'downloadSnowCourseYG'
     AND ssa.active
     AND ssa.synchronize_priority IS NOT NULL
   ORDER BY ss.sample_series_id"
  ))
  if (!nrow(series)) {
    stop("No active snow-course synchronization adapters were found.")
  }
  if (any(is.na(series$source_location) | !nzchar(series$source_location))) {
    stop(
      "Every selected snow-course adapter must specify ",
      "source_fx_args.location."
    )
  }

  series_selection <- trimws(Sys.getenv("SNOW_COMPONENTS_SERIES", "ALL"))
  if (!identical(toupper(series_selection), "ALL")) {
    selected_ids <- suppressWarnings(as.integer(trimws(strsplit(
      series_selection,
      ",",
      fixed = TRUE
    )[[1L]])))
    if (!length(selected_ids) || any(is.na(selected_ids))) {
      stop("SNOW_COMPONENTS_SERIES must be ALL or comma-separated integer IDs.")
    }
    missing_ids <- setdiff(selected_ids, series$sample_series_id)
    if (length(missing_ids)) {
      stop(
        "Selected snow sample series do not exist or are not active adapters: ",
        paste(missing_ids, collapse = ", "),
        "."
      )
    }
    series <- series[sample_series_id %in% selected_ids]
  }

  location_ids <- paste(unique(series$location_id), collapse = ",")
  plan <- dbGetQuery(
    aquacache,
    paste0(
      "SELECT
       count(DISTINCT s.sample_id)::integer AS existing_samples,
       count(DISTINCT s.sample_id) FILTER (
         WHERE s.no_source_update
       )::integer AS protected_samples,
       count(DISTINCT r.result_id) FILTER (
         WHERE r.no_source_update
       )::integer AS protected_results,
       count(DISTINCT r.result_id) FILTER (
         WHERE r.matrix_state_id = ",
      liquid_id,
      "
           AND NOT s.no_source_update AND NOT r.no_source_update
       )::integer AS liquid_results_to_correct
     FROM discrete.samples s
     LEFT JOIN discrete.results r USING (sample_id)
     WHERE s.source_adapter_function = 'downloadSnowCourseYG'
       AND s.location_id IN (",
      location_ids,
      ")"
    )
  )
  plan$duplicate_solid_results_to_remove <- dbGetQuery(
    aquacache,
    paste0(
      "SELECT count(*)::integer AS result_count
     FROM discrete.results solid
     JOIN discrete.samples s ON s.sample_id = solid.sample_id
     JOIN discrete.results liquid
       ON liquid.sample_id = solid.sample_id
      AND liquid.parameter_id = solid.parameter_id
      AND liquid.result_type = solid.result_type
      AND liquid.sample_fraction_id IS NOT DISTINCT FROM
        solid.sample_fraction_id
      AND liquid.result_value_type IS NOT DISTINCT FROM
        solid.result_value_type
      AND liquid.result_speciation_id IS NOT DISTINCT FROM
        solid.result_speciation_id
      AND liquid.protocol_method IS NOT DISTINCT FROM solid.protocol_method
      AND liquid.laboratory IS NOT DISTINCT FROM solid.laboratory
      AND liquid.analysis_datetime IS NOT DISTINCT FROM
        solid.analysis_datetime
     WHERE solid.matrix_state_id = ",
      solid_id,
      "
       AND liquid.matrix_state_id = ",
      liquid_id,
      "
       AND NOT solid.no_source_update
       AND NOT liquid.no_source_update
       AND NOT s.no_source_update
       AND s.source_adapter_function = 'downloadSnowCourseYG'
       AND s.location_id IN (",
      location_ids,
      ")
       AND NOT EXISTS (
         SELECT 1 FROM discrete.result_aggregations ra
         WHERE ra.result_id = solid.result_id
       )
       AND NOT EXISTS (
         SELECT 1 FROM discrete.import_run_rows iur
         WHERE iur.result_id = solid.result_id
       )"
    )
  )$result_count[[1L]]
  primary_source_locations <- unique(series$source_location)
  source_locations <- unique(c(
    primary_source_locations,
    series$old_source_location[
      !is.na(series$old_source_location) & nzchar(series$old_source_location)
    ]
  ))
  source_location_sql <- paste(
    dbQuoteString(snow, source_locations),
    collapse = ","
  )
  primary_source_location_sql <- paste(
    dbQuoteString(snow, primary_source_locations),
    collapse = ","
  )
  plan$source_surveys <- dbGetQuery(
    snow,
    paste0(
      "SELECT count(*)::integer AS survey_count
     FROM public.surveys
     WHERE location IN (",
      source_location_sql,
      ")"
    )
  )$survey_count[[1L]]
  latest_surveys <- as.data.table(dbGetQuery(
    snow,
    paste0(
      "SELECT DISTINCT ON (s.location)
            s.location, s.survey_id, s.survey_date
     FROM public.surveys s
     JOIN public.measurements m USING (survey_id)
     WHERE s.location IN (",
      primary_source_location_sql,
      ")
       AND (m.swe IS NOT NULL OR m.depth IS NOT NULL)
       AND (m.exclude_flag IS NULL OR NOT m.exclude_flag)
     ORDER BY s.location, s.survey_date DESC, s.survey_id DESC"
    )
  ))
  missing_source_locations <- setdiff(
    primary_source_locations,
    latest_surveys$location
  )
  if (length(missing_source_locations)) {
    stop(
      "No calculable SnowDB survey was found for configured location(s): ",
      paste(missing_source_locations, collapse = ", "),
      "."
    )
  }
  for (i in seq_len(nrow(latest_surveys))) {
    latest <- latest_surveys[i]
    adapter_records <- AquaCache::downloadSnowCourseYG(
      location = latest$location,
      start_datetime = latest$survey_date - 1,
      end_datetime = latest$survey_date + 1,
      con = aquacache,
      snowCon = snow
    )
    record_index <- which(vapply(
      adapter_records,
      function(record) {
        identical(
          as.character(record$sample$external_sample_id[[1L]]),
          as.character(latest$survey_id)
        )
      },
      logical(1)
    ))
    if (length(record_index) != 1L) {
      stop(
        "The snow adapter did not return survey ",
        latest$survey_id,
        " for location ",
        latest$location,
        "."
      )
    }
    record <- adapter_records[[record_index]]
    if (
      !all(
        c("sample", "results", "result_aggregations", "result_components") %in%
          names(record)
      ) ||
        !nrow(record$results) ||
        nrow(record$result_aggregations) != nrow(record$results) ||
        !nrow(record$result_components)
    ) {
      stop(
        "The snow adapter returned an incomplete component contract for survey ",
        latest$survey_id,
        "."
      )
    }
    excluded_notes <- record$result_components$note[
      !record$result_components$included_in_aggregate
    ]
    if (
      length(excluded_notes) &&
        any(is.na(excluded_notes) | !nzchar(trimws(excluded_notes)))
    ) {
      stop(
        "The snow adapter returned an unexplained excluded component for survey ",
        latest$survey_id,
        "."
      )
    }
  }
  message(
    "Validated the component contract for ",
    nrow(latest_surveys),
    " configured SnowDB locations."
  )
  message("Selected ", nrow(series), " sample series.")
  print(plan, row.names = FALSE)
  if (!apply_changes) {
    message(
      "Dry run complete. Set SNOW_COMPONENTS_APPLY=YES to apply this plan."
    )
    return(invisible(plan))
  }

  migration_results <- vector("list", nrow(series))
  for (i in seq_len(nrow(series))) {
    current <- series[i]
    message(
      "[",
      i,
      "/",
      nrow(series),
      "] series ",
      current$sample_series_id,
      ", Aqua ",
      current$location_code,
      ", SnowDB ",
      current$source_location
    )
    warnings <- character()
    failure <- NULL
    deleted_duplicates <- 0L
    corrected_states <- 0L
    synchronized <- NULL
    dbBegin(aquacache)
    tryCatch(
      {
        deleted_duplicates <- dbExecute(
          aquacache,
          "DELETE FROM discrete.results solid
         USING discrete.results liquid, discrete.samples s
         WHERE solid.sample_id = s.sample_id
           AND liquid.sample_id = solid.sample_id
           AND solid.matrix_state_id = $1
           AND liquid.matrix_state_id = $2
           AND NOT solid.no_source_update
           AND NOT liquid.no_source_update
           AND NOT s.no_source_update
           AND s.source_adapter_function = 'downloadSnowCourseYG'
           AND s.location_id = $3
           AND liquid.parameter_id = solid.parameter_id
           AND liquid.result_type = solid.result_type
           AND liquid.sample_fraction_id IS NOT DISTINCT FROM
             solid.sample_fraction_id
           AND liquid.result_value_type IS NOT DISTINCT FROM
             solid.result_value_type
           AND liquid.result_speciation_id IS NOT DISTINCT FROM
             solid.result_speciation_id
           AND liquid.protocol_method IS NOT DISTINCT FROM
             solid.protocol_method
           AND liquid.laboratory IS NOT DISTINCT FROM solid.laboratory
           AND liquid.analysis_datetime IS NOT DISTINCT FROM
             solid.analysis_datetime
           AND NOT EXISTS (
             SELECT 1 FROM discrete.result_aggregations ra
             WHERE ra.result_id = solid.result_id
           )
           AND NOT EXISTS (
             SELECT 1 FROM discrete.import_run_rows iur
             WHERE iur.result_id = solid.result_id
           )",
          params = list(solid_id, liquid_id, current$location_id)
        )
        corrected_states <- dbExecute(
          aquacache,
          "UPDATE discrete.results r
         SET matrix_state_id = $1
         FROM discrete.samples s
         WHERE r.sample_id = s.sample_id
           AND r.matrix_state_id = $2
           AND NOT r.no_source_update
           AND NOT s.no_source_update
           AND s.source_adapter_function = 'downloadSnowCourseYG'
           AND s.location_id = $3",
          params = list(solid_id, liquid_id, current$location_id)
        )
        synchronized <- withCallingHandlers(
          AquaCache::synchronize_discrete(
            con = aquacache,
            sample_series_id = current$sample_series_id,
            start_datetime = as.POSIXct("1900-01-01", tz = "UTC"),
            active = "all",
            sync_remote_false = TRUE,
            delete = FALSE,
            snowCon = snow
          ),
          warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        if (length(warnings)) {
          stop(paste(warnings, collapse = " | "))
        }
        dbCommit(aquacache)
      },
      error = function(e) {
        failure <<- conditionMessage(e)
        try(dbRollback(aquacache), silent = TRUE)
      }
    )

    migration_results[[i]] <- data.table(
      sample_series_id = current$sample_series_id,
      location_code = current$location_code,
      source_location = current$source_location,
      deleted_duplicates = deleted_duplicates,
      corrected_states = corrected_states,
      synchronized_records = if (is.null(synchronized)) {
        0L
      } else {
        nrow(synchronized)
      },
      committed = is.null(failure),
      failure = if (is.null(failure)) NA_character_ else failure
    )
    message(
      if (is.null(failure)) "Committed." else paste0("Rolled back: ", failure)
    )
  }

  migration_results <- rbindlist(migration_results)
  print(migration_results, row.names = FALSE)
  print(
    migration_results[, .(
      series = .N,
      committed = sum(committed),
      failed = sum(!committed),
      deleted_duplicates = sum(deleted_duplicates[committed]),
      corrected_states = sum(corrected_states[committed]),
      synchronized_records = sum(synchronized_records[committed])
    )],
    row.names = FALSE
  )
  if (any(!migration_results$committed)) {
    stop(
      "At least one sample series rolled back. Retry the failed series before ",
      "running finalization."
    )
  }

  # A source edit can change a result identity field such as result_value_type.
  # Synchronization then creates the new canonical result and, with delete=FALSE,
  # deliberately leaves the old direct row in place. Remove such a superseded
  # source row only when an aggregation for the same sample and parameter now
  # exists and the direct row is unprotected and unreferenced.
  dbBegin(aquacache)
  superseded_results <- tryCatch(
    {
      deleted <- dbExecute(
        aquacache,
        paste0(
          "DELETE FROM discrete.results direct
         USING discrete.samples s
         WHERE direct.sample_id = s.sample_id
           AND s.source_adapter_function = 'downloadSnowCourseYG'
           AND s.location_id IN (",
          location_ids,
          ")
           AND NOT s.no_source_update
           AND NOT direct.no_source_update
           AND NOT EXISTS (
             SELECT 1 FROM discrete.result_aggregations own_aggregation
             WHERE own_aggregation.result_id = direct.result_id
           )
           AND EXISTS (
             SELECT 1
             FROM discrete.results replacement
             JOIN discrete.result_aggregations replacement_aggregation
               ON replacement_aggregation.result_id = replacement.result_id
             WHERE replacement.sample_id = direct.sample_id
               AND replacement.parameter_id = direct.parameter_id
               AND replacement.result_type = direct.result_type
               AND replacement.protocol_method IS NOT DISTINCT FROM
                 direct.protocol_method
               AND replacement.matrix_state_id = direct.matrix_state_id
           )
           AND NOT EXISTS (
             SELECT 1 FROM discrete.import_run_rows iur
             WHERE iur.result_id = direct.result_id
           )"
        )
      )
      dbCommit(aquacache)
      deleted
    },
    error = function(e) {
      try(dbRollback(aquacache), silent = TRUE)
      stop("Failed to remove superseded direct snow results: ", e$message)
    }
  )
  message("Removed ", superseded_results, " superseded direct snow result(s).")

  # Some pre-existing mapped samples can sit just before or exactly on a
  # sample-series synchronization boundary. Convert those exact existing source
  # IDs without relaxing the boundary and importing additional historical rows.
  remaining_samples <- as.data.table(dbGetQuery(
    aquacache,
    paste0(
      "SELECT DISTINCT s.sample_id, s.external_sample_id, s.datetime,
            s.location_id, s.media_id
     FROM discrete.samples s
     JOIN discrete.results r USING (sample_id)
     LEFT JOIN discrete.result_aggregations ra USING (result_id)
     WHERE s.source_adapter_function = 'downloadSnowCourseYG'
       AND s.location_id IN (",
      location_ids,
      ")
       AND NOT s.no_source_update
       AND NOT r.no_source_update
       AND ra.result_id IS NULL
     ORDER BY s.sample_id"
    )
  ))
  finalized_samples <- 0L
  if (nrow(remaining_samples)) {
    result_identity_sql <-
      "SELECT result_id, no_source_update
     FROM discrete.results
     WHERE sample_id = $1
       AND result_type = $2
       AND parameter_id = $3
       AND matrix_state_id IS NOT DISTINCT FROM $4
       AND sample_fraction_id IS NOT DISTINCT FROM $5
       AND result_value_type IS NOT DISTINCT FROM $6
       AND result_speciation_id IS NOT DISTINCT FROM $7
       AND protocol_method IS NOT DISTINCT FROM $8
       AND laboratory IS NOT DISTINCT FROM $9
       AND analysis_datetime IS NOT DISTINCT FROM $10"

    for (sample_index in seq_len(nrow(remaining_samples))) {
      existing_sample <- remaining_samples[sample_index]
      candidates <- series[location_id == existing_sample$location_id]
      matched_record <- NULL
      for (candidate_index in seq_len(nrow(candidates))) {
        candidate <- candidates[candidate_index]
        adapter_arguments <- AquaCache:::source_adapter_args_decode(
          candidate$source_fx_args
        )
        adapter_arguments <- c(
          list(
            start_datetime = existing_sample$datetime - 24 * 60 * 60,
            end_datetime = existing_sample$datetime + 24 * 60 * 60,
            con = aquacache,
            snowCon = snow
          ),
          adapter_arguments
        )
        records <- do.call(AquaCache::downloadSnowCourseYG, adapter_arguments)
        record_index <- which(vapply(
          records,
          function(record) {
            identical(
              as.character(record$sample$external_sample_id[[1L]]),
              as.character(existing_sample$external_sample_id)
            )
          },
          logical(1)
        ))
        if (length(record_index) == 1L) {
          if (!is.null(matched_record)) {
            stop(
              "More than one snow adapter returned external_sample_id ",
              existing_sample$external_sample_id,
              " for sample_id ",
              existing_sample$sample_id,
              "."
            )
          }
          matched_record <- records[[record_index]]
        }
      }
      if (is.null(matched_record)) {
        stop(
          "No configured snow adapter returned external_sample_id ",
          existing_sample$external_sample_id,
          " for sample_id ",
          existing_sample$sample_id,
          "."
        )
      }

      remote_results <- AquaCache:::normalize_discrete_result_matrix_states(
        con = aquacache,
        sample_media_id = existing_sample$media_id,
        results = matched_record$results
      )
      normalized <- AquaCache:::normalize_discrete_result_aggregations(
        results = remote_results,
        result_aggregations = matched_record$result_aggregations,
        result_components = matched_record$result_components
      )
      remote_results <- normalized$results
      result_ids <- integer(nrow(remote_results))
      for (result_index in seq_len(nrow(remote_results))) {
        remote_result <- remote_results[result_index, ]
        identity_value <- function(column, missing_value = NA_integer_) {
          if (column %in% names(remote_result)) {
            remote_result[[column]][[1L]]
          } else {
            missing_value
          }
        }
        match <- dbGetQuery(
          aquacache,
          result_identity_sql,
          params = list(
            existing_sample$sample_id,
            as.integer(remote_result$result_type[[1L]]),
            as.integer(remote_result$parameter_id[[1L]]),
            as.integer(identity_value("matrix_state_id")),
            as.integer(identity_value("sample_fraction_id")),
            as.integer(identity_value("result_value_type")),
            as.integer(identity_value("result_speciation_id")),
            as.integer(identity_value("protocol_method")),
            as.integer(identity_value("laboratory")),
            as.POSIXct(
              identity_value(
                "analysis_datetime",
                as.POSIXct(NA, tz = "UTC")
              ),
              tz = "UTC"
            )
          )
        )
        if (nrow(match) != 1L || match$no_source_update[[1L]]) {
          stop(
            "Could not uniquely match an unprotected canonical result for ",
            "sample_id ",
            existing_sample$sample_id,
            ", remote result row ",
            result_index,
            "."
          )
        }
        result_ids[[result_index]] <- match$result_id[[1L]]
      }

      dbBegin(aquacache)
      tryCatch(
        {
          AquaCache:::synchronize_discrete_sample_detail(
            con = aquacache,
            sample_id = existing_sample$sample_id,
            remote_results = remote_results,
            result_ids = result_ids,
            pending_results = list(),
            sample_qualifiers = NULL,
            sample_observers = NULL,
            result_aggregations = normalized$result_aggregations,
            result_components = normalized$result_components
          )
          dbCommit(aquacache)
        },
        error = function(e) {
          try(dbRollback(aquacache), silent = TRUE)
          stop(
            "Failed to finalize snow sample_id ",
            existing_sample$sample_id,
            ": ",
            e$message
          )
        }
      )
      finalized_samples <- finalized_samples + 1L
    }
  }
  message("Finalized ", finalized_samples, " pre-boundary snow sample(s).")

  verification <- dbGetQuery(
    aquacache,
    paste0(
      "SELECT
       count(DISTINCT s.sample_id)::integer AS snow_samples,
       count(DISTINCT ra.result_id)::integer AS aggregations,
       count(rc.result_component_id)::integer AS components,
       count(DISTINCT r.result_id) FILTER (
         WHERE ra.result_id IS NULL AND NOT s.no_source_update
           AND NOT r.no_source_update
       )::integer AS unprotected_direct_results,
       count(DISTINCT r.result_id) FILTER (
         WHERE r.matrix_state_id <> ",
      solid_id,
      "
           AND NOT s.no_source_update AND NOT r.no_source_update
       )::integer AS unprotected_non_solid_results,
       count(DISTINCT ra.result_id) FILTER (
         WHERE NOT summary.result_is_current
       )::integer AS stale_aggregations
     FROM discrete.samples s
     LEFT JOIN discrete.results r USING (sample_id)
     LEFT JOIN discrete.result_aggregations ra USING (result_id)
     LEFT JOIN discrete.result_components rc USING (result_id)
     LEFT JOIN discrete.result_aggregation_summary summary USING (result_id)
     WHERE s.source_adapter_function = 'downloadSnowCourseYG'
       AND s.location_id IN (",
      location_ids,
      ")"
    )
  )
  print(verification, row.names = FALSE)

  if (
    any(!migration_results$committed) ||
      verification$unprotected_direct_results[[1L]] > 0L ||
      verification$unprotected_non_solid_results[[1L]] > 0L ||
      verification$stale_aggregations[[1L]] > 0L
  ) {
    stop(
      "Migration verification found a failed series, an unprotected legacy ",
      "result, or a stale snow aggregation."
    )
  }

  invisible(migration_results)
}

if (!isTRUE(getOption("AquaCache.snow_component_migration.source_only"))) {
  run_snow_component_migration()
}
