#' Synchronize hydro DB with remote sources
#'
#' @description
#'
#' This synchronize function pulls and replaces data referenced in table 'timeseries' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence. New data is also brought in, if any exists on the remote. Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables (Water Survey of Canada).
#'
#' If you leave the 'con' object as NULL the function will try to run in parallel, which dramatically speeds things up. You can pass connection parameters to the function if you want to run it in parallel, but you must leave 'con' as NULL. If you don't want to run in parallel, you can pass a connection object to the function and it will run in sequence.
#'
#' In addition, grades, qualifiers, and approvals are always updated as it's computationally cheaper to do so than to check if they need updating.
#'
#' NOTE that any data point labelled as imputed = TRUE is only replaced if a value is found in the remote exactly matching the datetime of the imputed entry, and any data point labelled as no_source_update = TRUE is not replaced by the remote data (imputed or not). Grade, approval, and qualifier intervals carrying the same flag are also preserved.
#'
#' A timeseries whose selected synchronization assignment uses
#' `downloadAquarius` needs credentials in `.Renviron` or in that assignment's
#' arguments; see [downloadAquarius()].
#'
#' Transmission adapters can attach operational import-run IDs to their result.
#' After comparison and database writing, `synchronize_continuous()` finalizes
#' those rows in `continuous.transmission_import_runs` with the number of
#' measurements inserted or upserted. Stored-replay runs are retained for audit
#' but do not advance a live retrieval cursor.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after. If you wish to run this function in parallel you MUST leave this argument NULL. If you also specify connection parameters in later arguments they will be used, otherwise the function will use the AquaConnect defaults.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `timeseries_id`, or one per element of `timeseries_id.`
#' @param active Sets behavior for checking timeseries or not. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all timeseries
#' @param sync_remote_false Controls whether to synchronize timeseries that have the `sync_remote` column set to FALSE in the `timeseries` table. Use with extreme caution as setting to TRUE will override the timeseries' intended behavior and overwrite local data with remote data.
#' @param dbName The name of the database to connect to. If left NULL, the function will use the default database name from the .Renviron file as per [AquaConnect()].
#' @param dbHost The host address of the database. If left NULL, the function will use the default host address from the .Renviron file as per [AquaConnect()].
#' @param dbPort The port of the database. If left NULL, the function will use the default port from the .Renviron file as per [AquaConnect()].
#' @param dbUser The username for the database. If left NULL, the function will use the default username from the .Renviron file as per [AquaConnect()].
#' @param dbPass The password for the database. If left NULL, the function will use the default password from the .Renviron file as per [AquaConnect()].
#' @param from_storage If `TRUE`, ask each selected source adapter to replay raw
#'   transmissions from `continuous.transmission_payloads` instead of contacting
#'   its provider. Every selected adapter must declare `from_storage` as a
#'   managed runtime argument in `public.source_adapter_capabilities`, accept
#'   the managed `end_datetime` argument, and implement archive retrieval and
#'   parsing. `downloadNESDIS` currently supports this contract; future cellular
#'   or other transmission adapters can use the same archive and opt in through
#'   the registry. Replay uses each adapter's current parser and field mappings.
#' @param end_datetime End of the archived-transmission replay window. Used only
#'   when `from_storage = TRUE` and defaults to the current time. It is supplied
#'   to every replay-capable adapter. Measurements after the last observation
#'   parsed from the selected archive are not compared or removed.
#'
#' @return A data.frame showing the status of synchronization for each timeseries as well as the error or warning message, if any, plus updated entries in the hydro database.
#' @export
#'
#'
#TODO: incorporate a way to use the parameter "modifiedSince" for data from NWIS, and look into if this is possible for Aquarius and WSC (don't think so, but hey)

synchronize_continuous <- function(
  con = NULL,
  timeseries_id = "all",
  start_datetime,
  active = 'default',
  sync_remote_false = FALSE,
  dbName = NULL,
  dbHost = NULL,
  dbPort = NULL,
  dbUser = NULL,
  dbPass = NULL,
  from_storage = FALSE,
  end_datetime = Sys.time()
) {
  i <- NULL # to avoid R CMD check warnings

  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  if (
    !is.logical(from_storage) ||
      length(from_storage) != 1L ||
      is.na(from_storage)
  ) {
    stop("from_storage must be TRUE or FALSE.")
  }

  if (inherits(start_datetime, "Date")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else if (inherits(start_datetime, "character")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else if (!inherits(start_datetime, "POSIXct")) {
    stop("start_datetime must be a Date, character, or POSIXct object.")
  }
  start_datetime <- as.POSIXct(
    as.numeric(start_datetime),
    origin = "1970-01-01",
    tz = "UTC"
  )
  if (from_storage) {
    if (inherits(end_datetime, "Date")) {
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "character")) {
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (!inherits(end_datetime, "POSIXct")) {
      stop("end_datetime must be a Date, character, or POSIXct object.")
    }
    end_datetime <- as.POSIXct(
      as.numeric(end_datetime),
      origin = "1970-01-01",
      tz = "UTC"
    )
    if (is.na(end_datetime) || anyNA(start_datetime)) {
      stop("Could not interpret the stored-transmission replay window.")
    }
    if (any(start_datetime >= end_datetime)) {
      stop("Every start_datetime must precede end_datetime during replay.")
    }
  }

  if (is.null(con)) {
    # Try to set parameters from the .Renviron file where missing
    if (is.null(dbName)) {
      dbName <- "aquacache"
    }
    if (is.null(dbHost)) {
      dbHost <- Sys.getenv("aquacacheHost")
    }
    if (is.null(dbPort)) {
      dbPort <- Sys.getenv("aquacachePort")
    }
    if (is.null(dbUser)) {
      dbUser <- Sys.getenv("aquacacheAdminUser")
    }
    if (is.null(dbPass)) {
      dbPass <- Sys.getenv("aquacacheAdminPass")
    }

    if (any(is.null(c(dbName, dbHost, dbPort, dbUser, dbPass)))) {
      stop(
        "Unable to establish a connection. Please provide a connection, all connection parameters, or set them in the .Renviron file."
      )
    } else {
      con <- AquaConnect(
        name = dbName,
        host = dbHost,
        port = dbPort,
        username = dbUser,
        password = dbPass,
        silent = TRUE
      )
      parallel <- TRUE # TRUE because the connection parameters can be passed on to parallel instances
    }
    on.exit(DBI::dbDisconnect(con))
    rlang::check_installed(
      "foreach",
      reason = "to run this function in parallel"
    )
    rlang::check_installed(
      "doSNOW",
      reason = "to run this function in parallel"
    )

    # Set the dopar variable to avoid issue calling the variable in the worker function
    `%dopar%` <- foreach::`%dopar%`
  } else {
    parallel <- FALSE # FALSE because the connection parameters are already set and can't be passed on to parallel instances
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")
  adapter_capabilities <- getSourceAdapterCapabilities(
    con = con,
    data_domain = "continuous"
  )

  # Check length of start_datetime is either 1 of same as timeseries_id
  if (length(start_datetime) != 1) {
    if (length(start_datetime) != length(timeseries_id)) {
      stop(
        "There is not exactly one element to start_datetime per valid timeseries_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for timeseries_id that doesn't exist."
      )
    }
  } else {
    timeseries_id <- unique(timeseries_id)
  }

  requested_filter <- if (timeseries_id[1] == "all") {
    ""
  } else {
    paste0(
      "WHERE t.timeseries_id IN (",
      paste(timeseries_id, collapse = ", "),
      ")"
    )
  }
  all_timeseries <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         t.parameter_id,
         t.timeseries_id,
         source.timeseries_source_adapter_id,
         source.source_fx,
         source.source_fx_args,
         source.synchronize_priority,
         t.last_daily_calculation,
         at.aggregation_type,
         t.default_owner,
         t.active,
         t.sync_remote,
         transmission_route.platform_identifier
           AS transmission_platform_identifier
       FROM continuous.timeseries t
       JOIN continuous.aggregation_types at
         ON t.aggregation_type_id = at.aggregation_type_id
       LEFT JOIN LATERAL (
         SELECT
           tsa.timeseries_source_adapter_id,
           tsa.source_fx,
           tsa.source_fx_args,
           tsa.synchronize_priority
         FROM continuous.timeseries_source_adapters tsa
         WHERE tsa.timeseries_id = t.timeseries_id
           AND tsa.active
           AND tsa.synchronize_priority IS NOT NULL
         ORDER BY
           tsa.synchronize_priority,
           tsa.timeseries_source_adapter_id
         LIMIT 1
       ) source ON TRUE
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
       ",
      requested_filter
    )
  )
  if (timeseries_id[1] != "all") {
    if (length(unique(timeseries_id)) != nrow(all_timeseries)) {
      fail <- timeseries_id[!timeseries_id %in% all_timeseries$timeseries_id]
      ifelse(
        (length(fail) == 1),
        warning(
          "Could not find one of the timeseries_ids that you specified: ID ",
          fail,
          " is missing from the database."
        ),
        warning(
          "Could not find some of the timeseries_ids that you specified: IDs ",
          paste(fail, collapse = ", "),
          " are missing from the database."
        )
      )
    }
  }

  if (active == 'default') {
    all_timeseries <- all_timeseries[all_timeseries$active, ]
  }

  if (!sync_remote_false) {
    all_timeseries <- all_timeseries[
      all_timeseries$sync_remote,
    ]
  }

  if (nrow(all_timeseries) == 0L) {
    stop("Could not find any timeseries matching your input parameters.")
  }
  missing_source <- is.na(all_timeseries$source_fx)
  if (any(missing_source)) {
    warning(
      "The following timeseries have no active source-adapter assignment ",
      "with a synchronize priority and will be ignored: ",
      paste(all_timeseries$timeseries_id[missing_source], collapse = ", "),
      "."
    )
    all_timeseries <- all_timeseries[!missing_source, , drop = FALSE]
  }
  if (nrow(all_timeseries) == 0L) {
    stop(
      "Could not find any timeseries with an active source-adapter ",
      "assignment for synchronization."
    )
  }

  unregistered_sources <- setdiff(
    unique(all_timeseries$source_fx),
    adapter_capabilities$source_fx
  )
  if (length(unregistered_sources) > 0L) {
    stop(
      "synchronize_continuous: Missing or disabled continuous source ",
      "adapter capabilities: ",
      paste(unregistered_sources, collapse = ", "),
      "."
    )
  }
  if (from_storage) {
    selected_sources <- unique(all_timeseries$source_fx)
    replay_support <- vapply(
      selected_sources,
      function(source_fx) {
        matches <- adapter_capabilities[["source_fx"]] == source_fx
        capability <- adapter_capabilities[
          which(matches),
          ,
          drop = FALSE
        ]
        all(vapply(
          c("from_storage", "end_datetime"),
          function(argument_name) {
            source_adapter_supports_runtime_argument(
              capability,
              argument_name
            )
          },
          logical(1)
        ))
      },
      logical(1)
    )
    if (any(!replay_support)) {
      stop(
        "synchronize_continuous: from_storage = TRUE is not supported by ",
        "the selected source adapter(s): ",
        paste(selected_sources[!replay_support], collapse = ", "),
        ". Replay-capable adapters must register from_storage and ",
        "end_datetime as runtime arguments."
      )
    }
  }

  grade_unknown <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM public.grade_types WHERE grade_type_code = 'UNK';"
  )[1, 1]
  if (is.na(grade_unknown)) {
    stop("synchronize: Could not find grade type 'Unknown' in the database.")
  }
  approval_unknown <- DBI::dbGetQuery(
    con,
    "SELECT approval_type_id FROM public.approval_types WHERE approval_type_code = 'UNK';"
  )[1, 1]
  if (is.na(approval_unknown)) {
    stop("synchronize: Could not find approval type 'Unknown' in the database.")
  }
  qualifier_unknown <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id FROM public.qualifier_types WHERE qualifier_type_code = 'UNK';"
  )[1, 1]
  if (is.na(qualifier_unknown)) {
    stop(
      "synchronize: Could not find qualifier type 'Unknown' in the database."
    )
  }

  build_status_row <- function(
    row_index,
    timeseries_id,
    success,
    message = NA_character_
  ) {
    data.frame(
      row_index = row_index,
      timeseries_id = timeseries_id,
      success = success,
      message = if (isTRUE(success)) NA_character_ else as.character(message),
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
        "synchronize_continuous: Source-adapter registry lookup failed for ",
        source_fx,
        ". Available continuous adapters: ",
        paste(adapter_capabilities$source_fx, collapse = ", "),
        "."
      )
    }
    rows
  }

  get_row_start_datetime <- function(i) {
    if (length(start_datetime) > 1) {
      start_datetime[[i]]
    } else {
      start_datetime
    }
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

    start_seconds <- vapply(
      indices,
      function(idx) as.numeric(get_row_start_datetime(idx)),
      numeric(1)
    )
    indices[order(
      start_seconds,
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

  # Define a worker function that either gets passed to parallel or sequential for loops over rows in all_timeseries
  worker <- function(
    i,
    all_timeseries,
    approval_unknown,
    grade_unknown,
    qualifier_unknown,
    start_datetime,
    from_storage,
    end_datetime,
    parallel,
    con
  ) {
    parameter <- all_timeseries$parameter_id[i]
    aggregation_type <- all_timeseries$aggregation_type[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    owner <- all_timeseries$default_owner[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    start_dt <- if (length(start_datetime) > 1) {
      start_datetime[i]
    } else {
      start_datetime
    }

    lock_namespace <- "aquacache_timeseries"
    lock_acquired <- FALSE
    acquire_timeseries_lock <- function() {
      if (!lock_acquired) {
        # This lock waits for other processes to release the lock, but only
        # after the remote fetch has completed so cache-sharing work is not
        # serialized behind DB writes.
        advisory_lock_acquire(
          con = con,
          namespace = lock_namespace,
          key = tsid,
          wait = TRUE
        )
        lock_acquired <<- TRUE
      }
    }
    on.exit(
      {
        if (lock_acquired) {
          advisory_lock_release(con, lock_namespace, tsid)
        }
      },
      add = TRUE
    )

    run_db_updates <- function(write_fx) {
      active_trans <- dbTransBegin(con)
      execute_write_fx <- function() {
        withCallingHandlers(
          write_fx(),
          warning = function(w) {
            stop(structure(
              list(message = conditionMessage(w), call = NULL),
              class = c("synchronize_worker_warning", "error", "condition")
            ))
          }
        )
      }

      if (active_trans) {
        tryCatch(
          {
            execute_write_fx()
            DBI::dbExecute(con, "COMMIT;")
          },
          error = function(e) {
            DBI::dbExecute(con, "ROLLBACK;")
            stop(e)
          }
        )
      } else {
        execute_write_fx()
      }
    }

    apply_remote_attributes <- function(write_remote) {
      if ("owner" %in% names(write_remote)) {
        adjust_owner(
          con,
          tsid,
          write_remote[, c("datetime", "owner")],
          delete = TRUE
        )
        write_remote$owner <- NULL
      }
      if ("contributor" %in% names(write_remote)) {
        adjust_contributor(
          con,
          tsid,
          write_remote[, c("datetime", "contributor")],
          delete = TRUE
        )
        write_remote$contributor <- NULL
      }
      if ("grade" %in% names(write_remote)) {
        adjust_grade(
          con,
          tsid,
          write_remote[, c("datetime", "grade")],
          delete = TRUE,
          source_update = TRUE
        )
        write_remote$grade <- NULL
      }
      if ("approval" %in% names(write_remote)) {
        adjust_approval(
          con,
          tsid,
          write_remote[, c("datetime", "approval")],
          delete = TRUE,
          source_update = TRUE
        )
        write_remote$approval <- NULL
      }
      if ("qualifier" %in% names(write_remote)) {
        adjust_qualifier(
          con,
          tsid,
          write_remote[, c("datetime", "qualifier")],
          delete = TRUE,
          source_update = TRUE
        )
        write_remote$qualifier <- NULL
      }

      write_remote
    }

    args_list <- list(start_datetime = start_dt, con = con)
    if (!is.na(source_fx_args)) {
      # add some arguments if they are specified
      args <- source_adapter_args_decode(source_fx_args)
      args_list <- c(args_list, args)
    }
    capability <- get_adapter_capability(source_fx)
    if (isTRUE(capability$inject_timeseries_id[[1]])) {
      args_list <- args_list[names(args_list) != "timeseries_id"]
      args_list$timeseries_id <- tsid
    }
    if (from_storage) {
      args_list <- args_list[
        !names(args_list) %in% c("from_storage", "end_datetime")
      ]
      args_list$from_storage <- TRUE
      args_list$end_datetime <- end_datetime
    }

    inRemote <- do.call(source_fx, args_list) # Get the data using the args_list
    transmission_import_run_ids <- attr(
      inRemote,
      "transmission_import_run_ids",
      exact = TRUE
    )
    transmission_runs_finalized <- length(transmission_import_run_ids) == 0L
    on.exit(
      {
        if (!transmission_runs_finalized) {
          try(
            getFromNamespace(
              "transmission_fail_import_runs",
              "AquaCache"
            )(
              con = con,
              transmission_import_run_ids = transmission_import_run_ids,
              workflow = "synchronize_continuous"
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
      getFromNamespace(
        "transmission_finalize_import_runs",
        "AquaCache"
      )(
        con = con,
        transmission_import_run_ids = transmission_import_run_ids,
        measurements_inserted = measurements_inserted,
        workflow = "synchronize_continuous"
      )
      transmission_runs_finalized <<- TRUE
    }
    inRemote <- inRemote[!is.na(inRemote$value), ]

    if (nrow(inRemote) == 0) {
      # There was no data in remote for the date range specified
      acquire_timeseries_lock()
      run_db_updates(function() {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE continuous.timeseries SET last_synchronize = '",
            .POSIXct(Sys.time(), "UTC"),
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
        finalize_transmission_runs(0L)
      })
      return()
    } else if (!all(c("value", "datetime") %in% names(inRemote))) {
      stop(
        "The function specified in source_fx must return a data.frame or data.table with columns named 'value' and 'datetime', at minimum."
      )
    }

    acquire_timeseries_lock()
    replay_upper_bound <- if (from_storage) {
      paste0(" AND datetime <= '", fmt(max(inRemote$datetime)), "'")
    } else {
      ""
    }
    inDB <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT no_source_update, datetime, value, period, imputed FROM continuous.measurements_continuous WHERE timeseries_id = ",
        tsid,
        " AND datetime >= '",
        min(inRemote$datetime),
        "'",
        replay_upper_bound,
        ";"
      )
    )
    # Set aside rows protected from source updates.
    source_protected <- inDB[inDB$no_source_update, ]
    inDB <- inDB[!inDB$no_source_update, ]
    inDB$no_source_update <- NULL
    source_protected$no_source_update <- NULL
    # Check if any imputed data points are present in the new data; replace the imputed value if TRUE and a non-imputed value now exists
    imputed <- inDB[inDB$imputed, ]
    imputed.remains <- data.frame()
    if (nrow(imputed) > 0) {
      imputed_remains <- imputed[
        !(imputed$datetime %in% inRemote$datetime),
        ,
        drop = FALSE
      ]
      source_protected <- rbind(source_protected, imputed_remains)
    }

    # Adjust parameters
    if (!("approval" %in% names(inRemote))) {
      inRemote$approval <- approval_unknown
    }
    if (!("grade" %in% names(inRemote))) {
      inRemote$grade <- grade_unknown
    }
    if (!("qualifier" %in% names(inRemote))) {
      inRemote$qualifier <- qualifier_unknown
    }
    if (!is.null(owner)) {
      # There may not be an owner assigned in table timeseries
      if (!("owner" %in% names(inRemote))) {
        inRemote$owner <- owner
      }
    }

    attribute_remote <- inRemote
    # Measurement protection is independent from grade, approval, and
    # qualifier protection, so retain the full remote frame for interval
    # adjustment while removing protected timestamps from value comparison.
    if (nrow(source_protected) > 0) {
      inRemote <- inRemote[
        !(inRemote$datetime %in% source_protected$datetime),
        ,
        drop = FALSE
      ]
    }
    if (nrow(inRemote) == 0) {
      run_db_updates(function() {
        apply_remote_attributes(attribute_remote)
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE continuous.timeseries SET last_synchronize = '",
            .POSIXct(Sys.time(), "UTC"),
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
        finalize_transmission_runs(0L)
      })
      return()
    }

    if (nrow(inDB) > 0) {
      # If nothing inDB it's an automatic mismatch so this is skipped
      min_inRemote <- min(inRemote$datetime)
      min_inDB <- min(inDB$datetime)
      if (min_inRemote > min_inDB) {
        # if TRUE means that the DB has older data than the remote, which happens notably for the WSC. This older data can't be compared and is thus discarded.
        inDB <- inDB[inDB$datetime >= min_inRemote, ]
      }

      if (min_inRemote < min_inDB) {
        # if TRUE means that the remote has older data than the DB, so immediately declare mismatch = TRUE.
        mismatch <- TRUE
        cutoff <- min_inRemote
      } else {
        # order both timeseries to compare them
        inDB <- inDB[order(inDB$datetime), ]
        inRemote <- inRemote[order(inRemote$datetime), ]

        # Create a unique datetime key for both data frames
        # Make keys
        inRemote$key <- paste(
          substr(as.character(inRemote$datetime), 1, 22),
          inRemote$value,
          sep = "|"
        )
        inDB$key <- paste(
          substr(as.character(inDB$datetime), 1, 22),
          inDB$value,
          sep = "|"
        )

        # Check for mismatches using set operations.
        mismatch_keys_remote <- inRemote$key[!(inRemote$key %in% inDB$key)]
        # Check the inverse as well, in case there are points in the DB that are not in the remote
        mismatch_keys_db <- inDB$key[!(inDB$key %in% inRemote$key)]

        # assume inRemote$key and inDB$key already exist
        remote_dates <- inRemote$datetime[
          inRemote$key %in% mismatch_keys_remote
        ]
        db_dates <- inDB$datetime[inDB$key %in% mismatch_keys_db]

        if (length(remote_dates) > 0 && length(db_dates) > 0) {
          mismatch <- TRUE
          cutoff <- min(min(remote_dates), min(db_dates))
        } else if (length(remote_dates) == 0 && length(db_dates) > 0) {
          mismatch <- TRUE
          cutoff <- min(db_dates)
        } else if (length(remote_dates) > 0 && length(db_dates) == 0) {
          mismatch <- TRUE
          cutoff <- min(remote_dates)
        } else {
          mismatch <- FALSE
        }

        inRemote$key <- NULL
      }
    } else {
      # There's no data in the DB but there is some in the remote. Automatic mismatch.
      mismatch <- TRUE
      cutoff <- min(inRemote$datetime)
    }

    if (mismatch) {
      # mismatch is TRUE: there was a mismatch between the remote and the local data
      inRemote <- inRemote[inRemote$datetime >= cutoff, ]
      if (nrow(inRemote) > 0) {
        # assign a period to the data
        if (aggregation_type == "instantaneous") {
          # Period is always 0 for instantaneous data
          inRemote$period <- "00:00:00"
        } else if (
          (aggregation_type != "instantaneous") &
            !("period" %in% names(inRemote))
        ) {
          # aggregation_types of mean, median, min, max should all have a period
          period <- calculate_period(
            data = inRemote[, "datetime"],
            timeseries_id = tsid,
            con = con
          )
          inRemote <- merge(inRemote, period, by = "datetime", all.x = TRUE)
        } else {
          # Check to make sure that the supplied period can actually be coerced to a period
          check <- lubridate::period(unique(inRemote$period))
          if (NA %in% check) {
            inRemote$period <- NA
          }
        }
        inRemote$imputed <- FALSE
        inRemote$timeseries_id <- tsid
      }

      # Now commit the changes to the database
      commit_fx <- function(con, tsid, inRemote, cutoff, inDB) {
        same_or_na <- function(x, y) {
          both_na <- is.na(x) & is.na(y)
          same <- rep(FALSE, length(x))
          same[both_na] <- TRUE

          both_present <- !is.na(x) & !is.na(y)
          same[both_present] <- x[both_present] == y[both_present]
          same
        }

        existing_measurements <- inDB[
          inDB$datetime >= cutoff,
          c("datetime", "value", "period", "imputed"),
          drop = FALSE
        ]
        remote_measurements <- if (nrow(inRemote) > 0) {
          inRemote[, c("datetime", "value", "period", "imputed"), drop = FALSE]
        } else {
          data.frame(
            datetime = as.POSIXct(character(), tz = "UTC"),
            value = numeric(),
            period = character(),
            imputed = logical()
          )
        }

        comparison <- merge(
          existing_measurements,
          remote_measurements,
          by = "datetime",
          all = TRUE,
          suffixes = c("_db", "_remote")
        )

        same_value <- same_or_na(comparison$value_db, comparison$value_remote)
        same_period <- same_or_na(
          as.character(comparison$period_db),
          as.character(comparison$period_remote)
        )
        same_imputed <- same_or_na(
          comparison$imputed_db,
          comparison$imputed_remote
        )
        row_changed <- !(same_value & same_period & same_imputed)

        delete_datetimes <- sort(unique(comparison$datetime[
          !is.na(comparison$value_db) &
            is.na(comparison$value_remote)
        ]))
        append_datetimes <- sort(unique(comparison$datetime[
          !is.na(comparison$value_remote) &
            (is.na(comparison$value_db) | row_changed)
        ]))
        append_rows <- remote_measurements[
          remote_measurements$datetime %in% append_datetimes,
          c("datetime", "value", "period", "imputed"),
          drop = FALSE
        ]
        if (length(delete_datetimes) > 0) {
          DBI::dbExecute(
            con,
            paste0(
              "DELETE FROM continuous.measurements_continuous WHERE timeseries_id = ",
              tsid,
              " AND datetime IN ('",
              paste(fmt(delete_datetimes), collapse = "', '"),
              "');"
            )
          )
        }

        if (nrow(append_rows) > 0) {
          append_rows$timeseries_id <- tsid
          dbAppendTableRLS(
            con,
            "continuous.measurements_continuous",
            append_rows[, c(
              "datetime",
              "value",
              "period",
              "timeseries_id",
              "imputed"
            )],
            on_conflict = "update",
            conflict_cols = c("timeseries_id", "datetime"),
            update_cols = c("value", "period", "imputed")
          )
        }
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE continuous.timeseries SET last_synchronize = '",
            .POSIXct(Sys.time(), "UTC"),
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
        nrow(append_rows)
      }
      run_db_updates(function() {
        apply_remote_attributes(
          attribute_remote[attribute_remote$datetime >= cutoff, , drop = FALSE]
        )
        write_remote <- inRemote
        write_remote <- write_remote[write_remote$datetime >= cutoff, ]
        if (nrow(write_remote) > 0) {
          #assign a period to the data
          if (aggregation_type == "instantaneous") {
            #Period is always 0 for instantaneous data
            write_remote$period <- "00:00:00"
          } else if (
            (aggregation_type != "instantaneous") &
              !("period" %in% names(write_remote))
          ) {
            #aggregation_types of mean, median, min, max should all have a period
            period <- calculate_period(
              data = write_remote[, "datetime"],
              timeseries_id = tsid,
              con = con
            )
            write_remote <- merge(
              write_remote,
              period,
              by = "datetime",
              all.x = TRUE
            )
          } else {
            #Check to make sure that the supplied period can actually be coerced to a period
            check <- lubridate::period(unique(write_remote$period))
            if (NA %in% check) {
              write_remote$period <- NA
            }
          }
          write_remote$imputed <- FALSE
          write_remote$timeseries_id <- tsid
        }

        measurements_inserted <- commit_fx(
          con,
          tsid,
          write_remote,
          cutoff,
          inDB
        )
        finalize_transmission_runs(measurements_inserted)
      })
    } else {
      # mismatch is FALSE: there was data in the remote but no mismatch. Do basic checks and update the last_synchronize date.
      run_db_updates(function() {
        apply_remote_attributes(attribute_remote)

        DBI::dbExecute(
          con,
          paste0(
            "UPDATE continuous.timeseries SET last_synchronize = '",
            .POSIXct(Sys.time(), "UTC"),
            "' WHERE timeseries_id = ",
            tsid,
            ";"
          )
        )
        finalize_transmission_runs(0L)

        # Bounds are maintained by database triggers.
      })
    }
  } # End of worker function

  run_worker_iteration <- function(i, con, parallel) {
    tryCatch(
      {
        worker(
          i,
          all_timeseries,
          approval_unknown,
          grade_unknown,
          qualifier_unknown,
          start_datetime,
          from_storage,
          end_datetime,
          parallel = parallel,
          con = con
        )
        build_status_row(i, all_timeseries$timeseries_id[i], TRUE)
      },
      warning = function(w) {
        build_status_row(
          i,
          all_timeseries$timeseries_id[i],
          FALSE,
          paste0("Warning: ", conditionMessage(w))
        )
      },
      error = function(e) {
        prefix <- if (inherits(e, "synchronize_worker_warning")) {
          "Warning: "
        } else {
          "Error: "
        }
        build_status_row(
          i,
          all_timeseries$timeseries_id[i],
          FALSE,
          paste0(prefix, conditionMessage(e))
        )
      }
    )
  }

  run_worker_group <- function(indices, con, parallel) {
    updated_group <- vector("list", length(indices))

    for (j in seq_along(indices)) {
      updated_group[[j]] <- run_worker_iteration(
        indices[[j]],
        con = con,
        parallel = parallel
      )
    }

    do.call(rbind, updated_group)
  }

  task_groups <- build_parallel_groups()
  task_summary <- summarize_task_groups(task_groups)

  start <- Sys.time()

  message("Synchronizing timeseries with synchronize_continuous...")

  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_timeseries), style = 3)
  }

  if (nrow(all_timeseries) == 0) {
    updated <- build_status_row(
      integer(),
      numeric(),
      logical(),
      character()
    )
  } else if (parallel) {
    # !Important note when troubleshooting parallel stuff: load_all() doesn't work, as the .packages argument of foreach::foreach attaches the installed version of AquaCache.
    n.cores <- parallel::detectCores() - 2
    # Limit the number of cores to the number of independent cache-sharing task
    # groups so as to free up resources.
    if (n.cores > length(task_groups)) {
      n.cores <- length(task_groups)
    }
    if (n.cores < 1) {
      n.cores <- 1
      warning(
        "You're trying to run in parallel but I could only detect 2 or fewer CPU cores. Running on a single core."
      )
    }

    cl <- parallel::makeCluster(n.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      c(
        "all_timeseries",
        "adapter_capabilities",
        "approval_unknown",
        "grade_unknown",
        "qualifier_unknown",
        "start_datetime",
        "dbName",
        "dbHost",
        "dbPort",
        "dbUser",
        "dbPass",
        "task_groups",
        "source_adapter_args_decode"
      ),
      envir = environment()
    )

    doSNOW::registerDoSNOW(cl)
    if (interactive()) {
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
      message(
        "Iterating in parallel with ",
        n.cores,
        " CPU cores"
      )

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

      updated <- foreach::foreach(
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
              parcon <- AquaCache::AquaConnect(
                name = dbName,
                host = dbHost,
                port = dbPort,
                username = dbUser,
                password = dbPass,
                silent = TRUE
              )
              run_worker_group(task_groups[[i]], con = parcon, parallel = TRUE)
            },
            finally = {
              # Close the connection if it was opened in this iteration
              if (!is.null(parcon)) {
                DBI::dbDisconnect(parcon)
              }
            }
          )
        }
    } else {
      updated <- foreach::foreach(
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
              parcon <- AquaCache::AquaConnect(
                name = dbName,
                host = dbHost,
                port = dbPort,
                username = dbUser,
                password = dbPass,
                silent = TRUE
              )
              run_worker_group(task_groups[[i]], con = parcon, parallel = TRUE)
            },
            finally = {
              # Close the connection if it was opened in this iteration
              if (!is.null(parcon)) {
                DBI::dbDisconnect(parcon)
              }
            }
          )
        }
    }
  } else {
    # Not parallel
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
    updated <- vector("list", nrow(all_timeseries))

    for (j in seq_len(nrow(all_timeseries))) {
      updated[[j]] <- run_worker_iteration(j, con = con, parallel = FALSE)
      if (interactive()) {
        utils::setTxtProgressBar(pb, j)
      }
    } # End of for loop

    updated <- do.call(rbind, updated)
  } # End of not parallel block

  if (interactive()) {
    close(pb)
  }

  if (nrow(updated) > 0) {
    updated <- updated[order(updated$row_index), , drop = FALSE]
  }
  updated$row_index <- NULL

  DBI::dbExecute(
    con,
    paste0(
      "UPDATE information.internal_status SET value = '",
      .POSIXct(Sys.time(), "UTC"),
      "' WHERE event = 'last_sync_continuous';"
    )
  )
  message(
    "Successfully checked ",
    sum(updated$success, na.rm = TRUE),
    " timeseries of ",
    nrow(all_timeseries),
    " timeseries; failed on ",
    sum(!updated$success, na.rm = TRUE),
    " timeseries. See the returned data.frame for more information."
  )
  diff <- Sys.time() - start
  message(
    "Total elapsed time for synchronize continuous: ",
    round(diff[[1]], 2),
    " ",
    units(diff),
    ". End of function."
  )
  return(updated)
} #End of function
