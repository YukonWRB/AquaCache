#' Get new continuous-category data
#'
#' @description
#'
#' Retrieves new real-time data starting from the last local data point. The
#' active assignment with the lowest fetch priority in
#' `continuous.timeseries_source_adapters` supplies each timeseries' source
#' function and arguments. Timeseries without an eligible assignment are
#' ignored. See [addACTimeseries()] for creating assignments.
#'
#' If you are a developer, note that download or source functions MUST be registered in AquaCache using function [registerSourceAdapterArguments()], and that this operation would normally be completed using the 'patch' system. See patch_56.R for examples.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes `start_datetime`, defaulting to the instant after the
#' last local point. Other fetch parameters come from the selected assignment's
#' `source_fx_args` JSON object.
#'
#' ## Multi-timeseries transmission sources:
#' Source-adapter capabilities registered by database Patch 56 control
#' automatic argument injection and cache-sharing groups. For example,
#' `downloadNESDIS` receives the current `timeseries_id` automatically and
#' timeseries sharing a DCP address reuse one raw transmission payload.
#' Platform identifiers and field mappings remain database metadata; they do
#' not need to be duplicated in each timeseries assignment's arguments.
#' Transmission adapters can also attach operational import-run IDs to their
#' result. After successfully appending measurements, `getNewContinuous()`
#' finalizes those rows in `continuous.transmission_import_runs` with the actual
#' number inserted by this workflow.
#'
#' Every assigned `source_fx` must have an enabled entry in
#' `public.source_adapter_capabilities` for the continuous domain. This keeps
#' scheduled imports and administrative clients aligned on the supported
#' continuous-data adapters.
#'
#' ## Assigning measurement periods:
#' With the exception of "instantaneous" timeseries which automatically receive a period of "00:00:00" (0 time), the period associated with measurements (ex: 1 hour precipitation sum) is derived from the interval between measurements UNLESS a period column is provided by the source function (column source_fx, may also depend on source_fx_args). This function typically fetches only a few hours of measurements at a time, so if the interval cannot be conclusively determined from the new data (i.e. hourly measurements over four hours with two measurements missed) then additional data points will be pulled from the database.
#'
#' If a period supplied by any data fetch function cannot be coerced to an period object acceptable to "duration" data type, NULL values will be entered to differentiate from instantaneous periods of "00:00:00".
#'
#' ## Sharing privileges and ownership
#' This is dictated by the timeseries table, and checked prior to passing data through view tables to public users.
#'
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after. If you wish to run this function in parallel you MUST leave this argument NULL. If you also specify connection parameters in later arguments they will be used, otherwise the function will use the AquaConnect defaults.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'continuous'.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#' @param dbName The name of the database to connect to. If left NULL, the function will use the default database name from the .Renviron file as per [AquaConnect()].
#' @param dbHost The host address of the database. If left NULL, the function will use the default host address from the .Renviron file as per [AquaConnect()].
#' @param dbPort The port of the database. If left NULL, the function will use the default port from the .Renviron file as per [AquaConnect()].
#' @param dbUser The username for the database. If left NULL, the function will use the default username from the .Renviron file as per [AquaConnect()].
#' @param dbPass The password for the database. If left NULL, the function will use the default password from the .Renviron file as per [AquaConnect()].
#' @param verbose If TRUE, will print the timeseries_id of each iteration as it is processed. Default is FALSE.
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export

getNewContinuous <- function(
  con = NULL,
  timeseries_id = "all",
  active = 'default',
  dbName = NULL,
  dbHost = NULL,
  dbPort = NULL,
  dbUser = NULL,
  dbPass = NULL,
  verbose = FALSE
) {
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }

  parallel <- FALSE
  parallel_disabled_reason <- NULL
  if (is.null(con)) {
    if (is.null(dbName)) {
      dbName <- Sys.getenv("aquacacheName")
      if (!nzchar(dbName)) {
        dbName <- "aquacache"
      }
    }
    if (is.null(dbHost)) {
      dbHost <- Sys.getenv("aquacacheHost")
      if (!nzchar(dbHost)) {
        dbHost <- NULL
      }
    }
    if (is.null(dbPort)) {
      dbPort <- Sys.getenv("aquacachePort")
      if (!nzchar(dbPort)) {
        dbPort <- NULL
      }
    }
    if (is.null(dbUser)) {
      dbUser <- Sys.getenv("aquacacheAdminUser")
      if (!nzchar(dbUser)) {
        dbUser <- NULL
      }
    }
    if (is.null(dbPass)) {
      dbPass <- Sys.getenv("aquacacheAdminPass")
      if (!nzchar(dbPass)) {
        dbPass <- NULL
      }
    }

    if (
      any(vapply(
        list(dbName, dbHost, dbPort, dbUser, dbPass),
        is.null,
        logical(1)
      ))
    ) {
      stop(
        "Unable to establish a connection. Please provide a connection, all connection parameters, or set them in the .Renviron file."
      )
    }

    con <- AquaConnect(
      name = dbName,
      host = dbHost,
      port = dbPort,
      username = dbUser,
      password = dbPass,
      silent = TRUE
    )
    on.exit(DBI::dbDisconnect(con))
    parallel <- requireNamespace("foreach", quietly = TRUE) &&
      requireNamespace("doSNOW", quietly = TRUE)
    if (parallel) {
      `%dopar%` <- foreach::`%dopar%`
    } else {
      parallel_disabled_reason <- "missing_packages"
    }
  } else {
    parallel_disabled_reason <- "connection_provided"
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")
  adapter_capabilities <- getSourceAdapterCapabilities(
    con = con,
    data_domain = "continuous"
  )

  # Create table of basic timeseries that can fetch material data.
  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(
      con,
      "SELECT
        t.parameter_id,
        t.timeseries_id,
        t.timeseries_type,
        source.timeseries_source_adapter_id,
        source.source_fx,
        source.source_fx_args,
        source.fetch_priority,
        at.aggregation_type,
        t.default_owner,
        t.default_data_sharing_agreement_id,
        t.active,
        mc.last_data_point,
        transmission_route.platform_identifier
          AS transmission_platform_identifier
      FROM continuous.timeseries t
      JOIN continuous.aggregation_types at ON t.aggregation_type_id = at.aggregation_type_id
      LEFT JOIN LATERAL (
        SELECT
          tsa.timeseries_source_adapter_id,
          tsa.source_fx,
          tsa.source_fx_args,
          tsa.fetch_priority
        FROM continuous.timeseries_source_adapters tsa
        WHERE tsa.timeseries_id = t.timeseries_id
          AND tsa.active
          AND tsa.fetch_priority IS NOT NULL
        ORDER BY tsa.fetch_priority, tsa.timeseries_source_adapter_id
        LIMIT 1
      ) source ON TRUE
      LEFT JOIN (
        SELECT timeseries_id, MAX(datetime) AS last_data_point
        FROM continuous.measurements_continuous
        GROUP BY timeseries_id
      ) mc ON mc.timeseries_id = t.timeseries_id
      LEFT JOIN LATERAL (
        SELECT UPPER(TRIM(s.platform_identifier)) AS platform_identifier
        FROM public.source_adapter_capabilities sac
        JOIN continuous.transmission_timeseries_mappings m
          ON m.timeseries_id = t.timeseries_id
         AND m.enabled
        JOIN public.locations_metadata_transmission_routes r
          ON r.transmission_route_id = m.transmission_route_id
        JOIN public.locations_metadata_transmission_setups s
          ON s.transmission_setup_id = r.transmission_setup_id
        JOIN instruments.transmission_methods tm
          ON tm.transmission_method_id = s.transmission_method_id
        WHERE sac.source_fx = source.source_fx
          AND sac.data_domain = 'continuous'
          AND sac.enabled
          AND sac.requires_transmission_mapping
          AND (
            cardinality(sac.transmission_method_codes) = 0
            OR tm.method_code = ANY(sac.transmission_method_codes)
          )
          AND s.start_datetime <= CURRENT_TIMESTAMP
          AND (s.end_datetime IS NULL OR s.end_datetime > CURRENT_TIMESTAMP)
        ORDER BY s.start_datetime DESC, r.transmission_route_id
        LIMIT 1
      ) transmission_route ON TRUE
      WHERE t.timeseries_type = 'basic'
        AND source.source_fx IS NOT NULL;"
    )
  } else {
    requested_ids <- suppressWarnings(as.integer(timeseries_id))
    if (any(is.na(requested_ids))) {
      stop(
        "getNewContinuous: Parameter 'timeseries_id' must be 'all' or a vector of integer timeseries IDs."
      )
    }
    requested_ids <- unique(requested_ids)
    requested_sql <- paste(requested_ids, collapse = ", ")

    requested_timeseries <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT
          t.parameter_id,
          t.timeseries_id,
          t.timeseries_type,
          source.timeseries_source_adapter_id,
          source.source_fx,
          source.source_fx_args,
          source.fetch_priority,
          at.aggregation_type,
          t.default_owner,
          t.default_data_sharing_agreement_id,
          t.active,
          mc.last_data_point,
          transmission_route.platform_identifier
            AS transmission_platform_identifier
        FROM continuous.timeseries t
        JOIN continuous.aggregation_types at ON t.aggregation_type_id = at.aggregation_type_id
        LEFT JOIN LATERAL (
          SELECT
            tsa.timeseries_source_adapter_id,
            tsa.source_fx,
            tsa.source_fx_args,
            tsa.fetch_priority
          FROM continuous.timeseries_source_adapters tsa
          WHERE tsa.timeseries_id = t.timeseries_id
            AND tsa.active
            AND tsa.fetch_priority IS NOT NULL
          ORDER BY tsa.fetch_priority, tsa.timeseries_source_adapter_id
          LIMIT 1
        ) source ON TRUE
        LEFT JOIN (
          SELECT timeseries_id, MAX(datetime) AS last_data_point
          FROM continuous.measurements_continuous
          GROUP BY timeseries_id
        ) mc ON mc.timeseries_id = t.timeseries_id
        LEFT JOIN LATERAL (
          SELECT UPPER(TRIM(s.platform_identifier)) AS platform_identifier
          FROM public.source_adapter_capabilities sac
          JOIN continuous.transmission_timeseries_mappings m
            ON m.timeseries_id = t.timeseries_id
           AND m.enabled
          JOIN public.locations_metadata_transmission_routes r
            ON r.transmission_route_id = m.transmission_route_id
          JOIN public.locations_metadata_transmission_setups s
            ON s.transmission_setup_id = r.transmission_setup_id
          JOIN instruments.transmission_methods tm
            ON tm.transmission_method_id = s.transmission_method_id
          WHERE sac.source_fx = source.source_fx
            AND sac.data_domain = 'continuous'
            AND sac.enabled
            AND sac.requires_transmission_mapping
            AND (
              cardinality(sac.transmission_method_codes) = 0
              OR tm.method_code = ANY(sac.transmission_method_codes)
            )
            AND s.start_datetime <= CURRENT_TIMESTAMP
            AND (s.end_datetime IS NULL OR s.end_datetime > CURRENT_TIMESTAMP)
          ORDER BY s.start_datetime DESC, r.transmission_route_id
          LIMIT 1
        ) transmission_route ON TRUE
        WHERE t.timeseries_id IN (",
        requested_sql,
        ");"
      )
    )
    if (nrow(requested_timeseries) == 0) {
      stop("getNewContinuous: None of the requested timeseries IDs were found.")
    }

    non_basic <- requested_timeseries$timeseries_id[
      requested_timeseries$timeseries_type != "basic"
    ]
    if (length(non_basic) > 0) {
      non_basic_message <- paste0(
        "getNewContinuous: New data can only be fetched for basic timeseries. ",
        "The following timeseries_id values are not basic and will be ignored: ",
        paste(non_basic, collapse = ", "),
        "."
      )
      if (length(non_basic) == nrow(requested_timeseries)) {
        stop(non_basic_message)
      }
      warning(non_basic_message)
      requested_timeseries <- requested_timeseries[
        requested_timeseries$timeseries_type == "basic",
        ,
        drop = FALSE
      ]
    }

    missing_ids <- setdiff(requested_ids, requested_timeseries$timeseries_id)
    all_timeseries <- requested_timeseries[
      !is.na(requested_timeseries$source_fx),
      ,
      drop = FALSE
    ]
    if (
      length(missing_ids) > 0 ||
        nrow(all_timeseries) != nrow(requested_timeseries)
    ) {
      warning(
        "At least one requested timeseries ID was not found or has no active source-adapter assignment with a fetch priority."
      )
    }
  }

  if (active == 'default') {
    all_timeseries <- all_timeseries[all_timeseries$active, ]
  }

  if (nrow(all_timeseries) == 0) {
    stop("Could not find any timeseries matching your input parameters.")
  }

  unregistered_sources <- sort(unique(
    all_timeseries$source_fx[
      !all_timeseries$source_fx %in% adapter_capabilities$source_fx
    ]
  ))
  if (length(unregistered_sources) > 0L) {
    affected <- vapply(
      unregistered_sources,
      function(source_fx) {
        ids <- all_timeseries$timeseries_id[
          all_timeseries$source_fx == source_fx
        ]
        paste0(source_fx, " (timeseries_id: ", paste(ids, collapse = ", "), ")")
      },
      character(1)
    )
    stop(
      "getNewContinuous: Every source_fx must have an enabled entry in ",
      "public.source_adapter_capabilities for the continuous domain. ",
      "Missing or disabled: ",
      paste(affected, collapse = "; "),
      "."
    )
  }

  origin_datetime <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")

  build_status_row <- function(
    row_index,
    timeseries_id,
    state,
    updated = FALSE,
    rows_added = 0L,
    message = NA_character_
  ) {
    data.frame(
      row_index = row_index,
      timeseries_id = timeseries_id,
      state = state,
      updated = updated,
      rows_added = as.integer(rows_added),
      message = message,
      stringsAsFactors = FALSE
    )
  }

  parse_source_fx_args_safe <- function(source_fx_args) {
    if (length(source_fx_args) == 0 || is.na(source_fx_args)) {
      return(NULL)
    }

    tryCatch(
      {
        args <- source_adapter_args_decode(source_fx_args)
        if (!length(args)) {
          return(NULL)
        }
        args
      },
      error = function(e) NULL
    )
  }

  get_source_fx_arg <- function(args, name, default = NULL) {
    if (is.null(args) || is.null(args[[name]]) || length(args[[name]]) == 0) {
      return(default)
    }

    value <- as.character(args[[name]][[1]])
    if (is.na(value) || !nzchar(value)) {
      return(default)
    }

    value
  }

  get_adapter_capability <- function(source_fx) {
    matches <- adapter_capabilities$source_fx == source_fx
    rows <- adapter_capabilities[which(matches), , drop = FALSE]
    if (nrow(rows) != 1L) {
      stop(
        "getNewContinuous: Source-adapter registry lookup failed for ",
        source_fx,
        "."
      )
    }

    rows
  }

  get_row_last_data_point <- function(i) {
    last_point <- all_timeseries$last_data_point[[i]]
    if (is.null(last_point) || is.na(last_point)) {
      return(origin_datetime)
    }

    as.POSIXct(last_point, tz = "UTC")
  }

  get_parallel_group_key <- function(i) {
    source_fx <- all_timeseries$source_fx[[i]]
    args <- parse_source_fx_args_safe(all_timeseries$source_fx_args[[i]])
    capability <- get_adapter_capability(source_fx)

    strategy <- capability$parallel_group_strategy[[1]]
    if (identical(strategy, "source_args")) {
      group_args <- capability$parallel_group_args[[1]]
      group_values <- vapply(
        group_args,
        function(name) get_source_fx_arg(args, name, default = NA_character_),
        character(1)
      )
      if (length(group_values) > 0L && !anyNA(group_values)) {
        return(paste(c(source_fx, group_values), collapse = "|"))
      }
    }
    if (
      identical(strategy, "transmission_platform") &&
        "transmission_platform_identifier" %in% names(all_timeseries)
    ) {
      dcp_address <- all_timeseries$transmission_platform_identifier[[i]]
      if (
        length(dcp_address) == 1L &&
          !is.na(dcp_address) &&
          nzchar(dcp_address)
      ) {
        return(paste(source_fx, toupper(trimws(dcp_address)), sep = "|"))
      }
    }

    paste0("timeseries|", all_timeseries$timeseries_id[[i]])
  }

  order_parallel_group_members <- function(indices) {
    if (length(indices) <= 1) {
      return(indices)
    }

    last_seconds <- vapply(
      indices,
      function(idx) as.numeric(get_row_last_data_point(idx)),
      numeric(1)
    )
    indices[order(
      last_seconds,
      all_timeseries$timeseries_id[indices],
      na.last = TRUE
    )]
  }

  build_parallel_groups <- function() {
    if (nrow(all_timeseries) == 0) {
      return(list())
    }

    group_keys <- vapply(
      seq_len(nrow(all_timeseries)),
      get_parallel_group_key,
      character(1)
    )
    split_indices <- split(
      seq_len(nrow(all_timeseries)),
      factor(group_keys, levels = unique(group_keys))
    )
    lapply(split_indices, order_parallel_group_members)
  }

  summarize_task_groups <- function(task_groups) {
    if (length(task_groups) == 0) {
      return(list(
        task_group_count = 0L,
        task_group_sizes = integer(),
        cache_group_count = 0L,
        cache_timeseries_count = 0L,
        largest_group_size = 0L,
        cache_source_message = NULL
      ))
    }

    task_group_sizes <- lengths(task_groups)
    task_group_names <- names(task_groups)
    cache_group_flags <- !startsWith(task_group_names, "timeseries|")
    cache_group_sizes <- task_group_sizes[cache_group_flags]

    cache_source_message <- NULL
    if (any(cache_group_flags)) {
      cache_sources <- sub("\\|.*$", "", task_group_names[cache_group_flags])
      cache_group_counts <- tapply(
        rep.int(1L, length(cache_sources)),
        cache_sources,
        sum
      )
      cache_timeseries_counts <- tapply(
        cache_group_sizes,
        cache_sources,
        sum
      )
      cache_source_message <- paste(
        paste0(
          names(cache_timeseries_counts),
          "=",
          as.integer(cache_timeseries_counts),
          " timeseries in ",
          as.integer(cache_group_counts),
          " groups"
        ),
        collapse = "; "
      )
    }

    list(
      task_group_count = length(task_groups),
      task_group_sizes = task_group_sizes,
      cache_group_count = sum(cache_group_flags),
      cache_timeseries_count = sum(cache_group_sizes),
      largest_group_size = max(task_group_sizes),
      cache_source_message = cache_source_message
    )
  }

  task_groups <- build_parallel_groups()
  task_summary <- summarize_task_groups(task_groups)

  grade_unknown <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM public.grade_types WHERE grade_type_code = 'UNK';"
  )[1, 1]
  if (is.na(grade_unknown)) {
    stop(
      "getNewContinuous: Could not find grade type 'Unknown' in the database."
    )
  }
  approval_unknown <- DBI::dbGetQuery(
    con,
    "SELECT approval_type_id FROM public.approval_types WHERE approval_type_code = 'UNK';"
  )[1, 1]
  if (is.na(approval_unknown)) {
    stop(
      "getNewContinuous: Could not find approval type 'Unknown' in the database."
    )
  }
  qualifier_unknown <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id FROM public.qualifier_types WHERE qualifier_type_code = 'UNK';"
  )[1, 1]
  if (is.na(qualifier_unknown)) {
    stop(
      "getNewContinuous: Could not find qualifier type 'Unknown' in the database."
    )
  }

  message("Fetching new continuous data with getNewContinuous...")
  if (verbose && parallel) {
    message("Verbose per-timeseries messages are disabled in parallel mode.")
  }
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_timeseries), style = 3)
  }

  # These internal functions are copied into the getNewContinuous execution
  # environment and exported explicitly below. Installed package namespaces do
  # not attach internal functions to PSOCK workers merely because AquaCache is
  # listed in foreach's .packages argument. Resolve them from the namespace so
  # this also works when getNewContinuous.R is sourced by a deployment script.
  transmission_finalize_import_runs_worker <-
    getFromNamespace("transmission_finalize_import_runs", "AquaCache")
  transmission_fail_import_runs_worker <- getFromNamespace(
    "transmission_fail_import_runs",
    "AquaCache"
  )

  worker <- function(i, con, parallel) {
    tsid <- all_timeseries$timeseries_id[i]
    if (verbose && !parallel) {
      message(
        "Processing iteration ",
        i,
        " of ",
        nrow(all_timeseries),
        ", timeseries_id: ",
        tsid
      )
    }

    # Acquire a lock for this timeseries to prevent concurrent updates, notably by synchronize_continuous
    # IMPORTANT: this lock does not wait if another process has it, it just skips to the next timeseries. Synchronize_continuous **will** wait for the lock to be released, on the other hand.
    lock_namespace <- "aquacache_timeseries"
    lock_acquired <- advisory_lock_acquire(
      con = con,
      namespace = lock_namespace,
      key = tsid,
      wait = FALSE
    )
    if (!isTRUE(lock_acquired)) {
      return(build_status_row(
        i,
        tsid,
        state = "locked",
        message = "Locked by another process."
      ))
    }
    on.exit(advisory_lock_release(con, lock_namespace, tsid), add = TRUE)

    aggregation_type <- all_timeseries$aggregation_type[i]
    source_fx <- all_timeseries$source_fx[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    owner <- all_timeseries$default_owner[i]
    data_sharing_agreement_id <-
      all_timeseries$default_data_sharing_agreement_id[i]

    # Find the last data point in measurements_continuous
    # Not using the value preloaded in all_timeseries in case it changed before
    # this worker acquired the lock.
    last_data_point <- DBI::dbGetQuery(
      con,
      "SELECT MAX(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1",
      params = list(tsid)
    )[1, 1] +
      1

    if (is.na(last_data_point)) {
      last_data_point <- origin_datetime
    }

    args_list <- list(start_datetime = last_data_point, con = con)
    if (!is.na(source_fx_args)) {
      args <- source_adapter_args_decode(source_fx_args)
      args_list <- c(args_list, args)
    }
    capability <- get_adapter_capability(source_fx)
    if (isTRUE(capability$inject_timeseries_id[[1]])) {
      # A shared source can feed many timeseries. Keep source_fx_args focused
      # on adapter options and inject the current timeseries automatically.
      args_list <- args_list[names(args_list) != "timeseries_id"]
      args_list$timeseries_id <- tsid
    }

    ts <- do.call(source_fx, args_list)
    transmission_import_run_ids <- attr(
      ts,
      "transmission_import_run_ids",
      exact = TRUE
    )
    transmission_runs_finalized <- length(transmission_import_run_ids) == 0L
    on.exit(
      {
        if (!transmission_runs_finalized) {
          try(
            transmission_fail_import_runs_worker(
              con = con,
              transmission_import_run_ids = transmission_import_run_ids,
              workflow = "getNewContinuous"
            ),
            silent = TRUE
          )
        }
      },
      add = TRUE
    )
    finalize_transmission_runs <- function(measurements_inserted) {
      if (length(transmission_import_run_ids) == 0L) {
        transmission_runs_finalized <<- TRUE
        return(invisible(0L))
      }
      transmission_finalize_import_runs_worker(
        con = con,
        transmission_import_run_ids = transmission_import_run_ids,
        measurements_inserted = measurements_inserted,
        workflow = "getNewContinuous"
      )
      transmission_runs_finalized <<- TRUE
    }

    # Make sure we've got at a data.frame or data.table
    if (
      !inherits(ts, "data.frame") &&
        !data.table::is.data.table(ts)
    ) {
      stop(
        "getNewContinuous: The data returned by source_fx ",
        source_fx,
        " is not tabular."
      )
    }
    # Make that there are at least some rows (otherwise we might fail when checking for column names)
    if (nrow(ts) == 0) {
      finalize_transmission_runs(0L)
      return(build_status_row(i, tsid, state = "no_new_data"))
    }

    # Make sure we have the required columns
    if (!all(c("value", "datetime") %in% names(ts))) {
      stop(
        "getNewContinuous: The data returned by source_fx ",
        source_fx,
        " does not have columns named 'value' and 'datetime'."
      )
    }

    # Remove rows with NA values in the value column, and then filter to only new data points after the last data point in the database
    ts <- ts[!is.na(ts$value), ]
    if (nrow(ts) > 0) {
      ts <- ts[ts$datetime >= last_data_point, ]
    }

    # If there are no new data points after that, skip to the next timeseries.
    if (nrow(ts) == 0) {
      finalize_transmission_runs(0L)
      return(build_status_row(i, tsid, state = "no_new_data"))
    }

    # Add required columns with default values if they don't exist
    ts$timeseries_id <- tsid
    ts$imputed <- FALSE

    if ("owner" %in% names(ts)) {
      if (!is.null(owner)) {
        ts$owner[is.na(ts$owner)] <- owner
      }
    } else if (!is.null(owner)) {
      ts$owner <- owner
    }

    if (!("approval" %in% names(ts))) {
      ts$approval <- approval_unknown
    }

    if (!("grade" %in% names(ts))) {
      ts$grade <- grade_unknown
    }

    if (!("qualifier" %in% names(ts))) {
      ts$qualifier <- qualifier_unknown
    }

    if ("data_sharing_agreement_id" %in% names(ts)) {
      if (!is.na(data_sharing_agreement_id)) {
        ts$data_sharing_agreement_id[
          is.na(ts$data_sharing_agreement_id)
        ] <- data_sharing_agreement_id
      }
    } else if (!is.na(data_sharing_agreement_id)) {
      ts$data_sharing_agreement_id <- data_sharing_agreement_id
    }

    run_db_updates <- function(write_fx) {
      active_trans <- dbTransBegin(con)
      if (active_trans) {
        tryCatch(
          {
            write_fx()
            DBI::dbExecute(con, "COMMIT;")
          },
          error = function(e) {
            DBI::dbExecute(con, "ROLLBACK;")
            stop(e)
          }
        )
      } else {
        write_fx()
      }
    }

    commit_fx <- function(con, ts, tsid) {
      adjust_grade(con, tsid, ts[, c("datetime", "grade")])
      adjust_approval(con, tsid, ts[, c("datetime", "approval")])
      adjust_qualifier(con, tsid, ts[, c("datetime", "qualifier")])
      if ("owner" %in% names(ts)) {
        adjust_owner(con, tsid, ts[, c("datetime", "owner")])
      }
      if ("contributor" %in% names(ts)) {
        adjust_contributor(con, tsid, ts[, c("datetime", "contributor")])
      }

      if ("data_sharing_agreement_id" %in% names(ts)) {
        adjust_data_sharing_agreement(
          con,
          tsid,
          ts[, c("datetime", "data_sharing_agreement_id")]
        )
      }

      ts <- ts[, c("datetime", "value", "timeseries_id", "imputed")]

      if (aggregation_type == "instantaneous") {
        ts$period <- "00:00:00"
      } else if (
        (aggregation_type != "instantaneous") && !("period" %in% names(ts))
      ) {
        requested_datetimes <- ts$datetime
        ts_period <- calculate_period(
          data = ts,
          timeseries_id = tsid,
          con = con
        )
        no_period <- dbGetQueryDT(
          con,
          paste0(
            "SELECT datetime FROM continuous.measurements_continuous WHERE timeseries_id = ",
            tsid,
            " AND datetime >= (SELECT MIN(datetime) FROM continuous.measurements_continuous WHERE period IS NULL AND timeseries_id = ",
            tsid,
            ") AND datetime NOT IN ('",
            paste(requested_datetimes, collapse = "', '"),
            "');"
          )
        )
        if (nrow(no_period) > 0 && "period" %in% names(ts_period)) {
          no_period$period <- ts_period$period[match(
            no_period$datetime,
            ts_period$datetime
          )]
          no_period <- no_period[!is.na(no_period$period), , drop = FALSE]
          if (nrow(no_period) > 0) {
            for (j in seq_len(nrow(no_period))) {
              DBI::dbExecute(
                con,
                "UPDATE continuous.measurements_continuous SET period = $1 WHERE datetime = $2 AND timeseries_id = $3",
                params = list(
                  no_period$period[j],
                  no_period$datetime[j],
                  tsid
                )
              )
            }
          }
        }
        ts <- ts_period[
          ts_period$datetime %in% requested_datetimes,
          ,
          drop = FALSE
        ]
      } else {
        check <- lubridate::period(unique(ts$period))
        if (NA %in% check) {
          ts$period <- NA
        }
      }

      dbAppendTableRLS(con, "continuous.measurements_continuous", ts)
    }

    rows_added <- nrow(ts)
    run_db_updates(function() {
      commit_fx(con, ts, tsid)
      finalize_transmission_runs(rows_added)
    })

    build_status_row(
      i,
      tsid,
      state = "updated",
      updated = TRUE,
      rows_added = rows_added
    )
  }

  run_worker_iteration <- function(i, con, parallel) {
    tryCatch(
      worker(i, con = con, parallel = parallel),
      error = function(e) {
        build_status_row(
          i,
          all_timeseries$timeseries_id[i],
          state = "error",
          message = paste0("Error: ", conditionMessage(e))
        )
      }
    )
  }

  run_worker_group <- function(indices, con, parallel) {
    status_group <- vector("list", length(indices))

    for (j in seq_along(indices)) {
      status_group[[j]] <- run_worker_iteration(
        indices[[j]],
        con = con,
        parallel = parallel
      )
    }

    do.call(rbind, status_group)
  }

  if (parallel) {
    n.cores <- parallel::detectCores() - 2
    if (n.cores > length(task_groups)) {
      n.cores <- length(task_groups)
    }
    if (n.cores < 1) {
      n.cores <- 1
      warning(
        "getNewContinuous: Parallel mode requested but only 2 or fewer CPU cores were detected. Running on a single core."
      )
    }

    message(
      "\nParallel plan: ",
      nrow(all_timeseries),
      " timeseries across ",
      task_summary$task_group_count,
      " task groups"
    )
    if (task_summary$cache_timeseries_count > 0) {
      message(
        "Cache-sharing groups cover ",
        task_summary$cache_timeseries_count,
        " timeseries across ",
        task_summary$cache_group_count,
        " groups; largest group size = ",
        task_summary$largest_group_size,
        ". ",
        task_summary$cache_source_message
      )
    }
    message("Iterating in parallel with ", n.cores, " CPU cores")

    cl <- parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      c(
        "all_timeseries",
        "adapter_capabilities",
        "grade_unknown",
        "approval_unknown",
        "qualifier_unknown",
        "origin_datetime",
        "task_groups",
        "dbName",
        "dbHost",
        "dbPort",
        "dbUser",
        "dbPass",
        "source_adapter_args_decode",
        "transmission_finalize_import_runs_worker",
        "transmission_fail_import_runs_worker"
      ),
      envir = environment()
    )

    doSNOW::registerDoSNOW(cl)
    if (interactive()) {
      progress <- function(n) {
        completed_groups <- min(as.integer(n), length(task_groups))
        if (is.na(completed_groups) || completed_groups < 1) {
          utils::setTxtProgressBar(pb, 0)
        } else {
          utils::setTxtProgressBar(
            pb,
            sum(task_summary$task_group_sizes[seq_len(completed_groups)])
          )
        }
      }
      opts <- list(progress = progress)

      status_rows <- foreach::foreach(
        i = seq_along(task_groups),
        .packages = c(
          "DBI",
          "jsonlite",
          "lubridate",
          "AquaCache",
          "data.table",
          "utils"
        ),
        .options.snow = opts,
        .combine = rbind,
        .errorhandling = "pass"
      ) %dopar%
        {
          parcon <- NULL
          tryCatch(
            {
              parcon <- AquaConnect(
                name = dbName,
                host = dbHost,
                port = dbPort,
                username = dbUser,
                password = dbPass,
                silent = TRUE
              )
              DBI::dbExecute(parcon, "SET timezone = 'UTC'")
              run_worker_group(
                task_groups[[i]],
                con = parcon,
                parallel = TRUE
              )
            },
            finally = {
              if (!is.null(parcon)) {
                DBI::dbDisconnect(parcon)
              }
            }
          )
        }
    } else {
      status_rows <- foreach::foreach(
        i = seq_along(task_groups),
        .packages = c(
          "DBI",
          "jsonlite",
          "lubridate",
          "AquaCache",
          "data.table",
          "utils"
        ),
        .combine = rbind,
        .errorhandling = "pass"
      ) %dopar%
        {
          parcon <- NULL
          tryCatch(
            {
              parcon <- AquaConnect(
                name = dbName,
                host = dbHost,
                port = dbPort,
                username = dbUser,
                password = dbPass,
                silent = TRUE
              )
              DBI::dbExecute(parcon, "SET timezone = 'UTC'")
              run_worker_group(
                task_groups[[i]],
                con = parcon,
                parallel = TRUE
              )
            },
            finally = {
              if (!is.null(parcon)) {
                DBI::dbDisconnect(parcon)
              }
            }
          )
        }
    }
  } else {
    if (identical(parallel_disabled_reason, "missing_packages")) {
      message(
        "Parallel packages 'foreach' and 'doSNOW' are not available; running in sequence."
      )
    }
    message(
      "\nIterating through ",
      nrow(all_timeseries),
      " timeseries in sequence. This may take a while, please be patient."
    )
    if (task_summary$cache_timeseries_count > 0) {
      message(
        "Detected ",
        task_summary$cache_timeseries_count,
        " cache-sharing timeseries across ",
        task_summary$cache_group_count,
        " groups; sequential mode can reuse the session cache directly."
      )
    }

    status_rows <- vector("list", nrow(all_timeseries))
    sequential_indices <- unlist(task_groups, use.names = FALSE)
    for (position in seq_along(sequential_indices)) {
      j <- sequential_indices[[position]]
      status_rows[[j]] <- run_worker_iteration(
        j,
        con = con,
        parallel = FALSE
      )
      if (interactive()) {
        utils::setTxtProgressBar(pb, position)
      }
    }
    status_rows <- do.call(rbind, status_rows)
  }

  if (interactive()) {
    close(pb)
  }

  if (nrow(status_rows) > 0) {
    status_rows <- status_rows[order(status_rows$row_index), , drop = FALSE]
  }
  status_rows$row_index <- NULL

  updated_count <- sum(status_rows$updated)
  rows_added_total <- sum(status_rows$rows_added, na.rm = TRUE)
  no_new_count <- sum(status_rows$state == "no_new_data")
  locked_count <- sum(status_rows$state == "locked")
  failed_count <- sum(status_rows$state == "error")

  message(
    updated_count,
    " of ",
    nrow(all_timeseries),
    " timeseries were updated; added ",
    rows_added_total,
    " rows in total. ",
    no_new_count,
    " had no new rows, ",
    locked_count,
    " were skipped because locked, and ",
    failed_count,
    " failed."
  )
  if (failed_count > 0) {
    failed_status <- utils::head(
      status_rows[status_rows$state == "error", , drop = FALSE],
      5
    )
    message(
      "Failures (first ",
      nrow(failed_status),
      "): ",
      paste(
        paste0(
          failed_status$timeseries_id,
          " (",
          failed_status$message,
          ")"
        ),
        collapse = "; "
      )
    )
  }
  if (locked_count > 0) {
    locked_ids <- utils::head(
      status_rows$timeseries_id[status_rows$state == "locked"],
      5
    )
    message(
      "Locked/skipped timeseries_id (first ",
      length(locked_ids),
      "): ",
      paste(locked_ids, collapse = ", ")
    )
  }

  try(
    {
      DBI::dbExecute(
        con,
        "UPDATE information.internal_status SET value = NOW() WHERE event = 'last_new_continuous'"
      )
    },
    silent = TRUE
  )

  success <- status_rows[status_rows$updated, "timeseries_id", drop = FALSE]
  if (nrow(success) > 0) {
    return(success)
  }
} # End of function
