#' Synchronize mutable metadata for one discrete sample
#'
#' Adds any source-provided sample-group memberships and updates mutable sample
#' columns whose remote values differ from the database. Local visibility and
#' source ownership are protected: `share_with` and `source_adapter_function` are never
#' changed by this helper.
#'
#' The operation participates in an existing transaction or opens and manages
#' one when called outside a transaction. Group membership and sample metadata
#' therefore succeed or fail together.
#'
#' @param con An open DBI connection to an AquaCache database.
#' @param database_sample A one-row data frame containing the current
#'   `discrete.samples` row.
#' @param remote_sample A one-row data frame containing the normalized remote
#'   sample values.
#' @param valid_sample_names Character vector of sample columns eligible for
#'   synchronization.
#' @param sample_groups Optional source-provided sample-group specifications.
#' @param default_owner Organization ID used for groups that omit `owner`.
#' @param default_contributor Organization ID used for groups that omit
#'   `contributor`.
#'
#' @return `TRUE` when at least one sample column was updated, otherwise
#'   `FALSE`. Group-membership changes do not affect the returned value.
#'
#' @keywords internal
#' @noRd
synchronize_discrete_sample_metadata <- function(
  con,
  database_sample,
  remote_sample,
  valid_sample_names,
  sample_groups,
  default_owner,
  default_contributor
) {
  active_trans <- dbTransBegin(con)
  transaction_finished <- FALSE
  on.exit(
    if (active_trans && !transaction_finished) {
      try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    },
    add = TRUE
  )
  tryCatch(
    {
      link_discrete_sample_groups(
        con = con,
        sample_id = database_sample$sample_id[[1]],
        sample_groups = sample_groups,
        default_owner = if (!is.na(default_owner)) {
          default_owner
        } else {
          database_sample$owner[[1]]
        },
        default_contributor = if (!is.na(default_contributor)) {
          default_contributor
        } else {
          database_sample$contributor[[1]]
        }
      )

      changed_columns <- character()
      changed_values <- list()
      for (column in intersect(names(remote_sample), valid_sample_names)) {
        # Synchronization must not change local visibility or reassign a
        # sample to a different source adapter.
        if (column %in% c("share_with", "source_adapter_function")) {
          next
        }

        database_value <- database_sample[[column]]
        remote_value <- remote_sample[[column]]
        if (!is.numeric(database_value)) {
          converted <- suppressWarnings(as.numeric(database_value))
          if (length(converted) == 1L && !is.na(converted)) {
            database_value <- converted
          }
        }
        if (!is.numeric(remote_value)) {
          converted <- suppressWarnings(as.numeric(remote_value))
          if (length(converted) == 1L && !is.na(converted)) {
            remote_value <- converted
          }
        }
        if (!isTRUE(all.equal(database_value, remote_value))) {
          changed_columns <- c(changed_columns, column)
          changed_values[[length(changed_values) + 1L]] <-
            if (length(remote_value) == 1L && is.na(remote_value)) {
              NA
            } else {
              remote_value
            }
        }
      }

      if (length(changed_columns) > 0L) {
        set_sql <- paste0(
          as.character(DBI::dbQuoteIdentifier(con, changed_columns)),
          " = $",
          seq_along(changed_columns),
          collapse = ", "
        )
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE discrete.samples SET ",
            set_sql,
            " WHERE sample_id = $",
            length(changed_values) + 1L,
            ";"
          ),
          params = c(changed_values, list(database_sample$sample_id[[1]]))
        )
      }
      if (active_trans) {
        DBI::dbExecute(con, "COMMIT;")
      }
      transaction_finished <- TRUE
      length(changed_columns) > 0L
    },
    error = function(e) {
      if (active_trans) {
        DBI::dbExecute(con, "ROLLBACK;")
        transaction_finished <<- TRUE
      }
      stop(e)
    }
  )
}


#' Synchronize associations and aggregation detail for one sample
#'
#' Applies source-provided sample qualifiers, observers, result aggregations,
#' and component observations after canonical result rows have been matched.
#' Omitted collections are preserved. Supplied qualifier or observer
#' collections replace their corresponding sample associations. Supplied
#' aggregation data replaces aggregation detail for the affected canonical
#' results and restores ordinary result values when a previously aggregated
#' result becomes direct.
#'
#' Parent result rows required by a new aggregation are inserted before its
#' aggregation metadata and components. The helper participates in the caller's
#' transaction or creates one so replacement cannot be partially committed.
#' Converting a formerly aggregated result to a direct result is an
#' administrative operation and therefore requires an `admin` or `postgres`
#' connection.
#'
#' @param con An open DBI connection to an AquaCache database.
#' @param sample_id Integer ID of the sample being synchronized.
#' @param remote_results Normalized remote result rows. Row positions correspond
#'   to `result_ids` and to `result_row` in aggregation inputs.
#' @param result_ids Integer vector mapping each remote result row to its
#'   canonical database `result_id`.
#' @param pending_results List of new parent result rows that must be inserted
#'   before aggregation detail.
#' @param sample_qualifiers Optional replacement qualifier associations. `NULL`
#'   preserves existing rows; a supplied empty data frame removes them.
#' @param sample_observers Optional replacement observer associations. `NULL`
#'   preserves existing rows; a supplied empty data frame removes them.
#' @param result_aggregations Optional normalized aggregation configurations.
#'   `NULL` preserves existing aggregation detail.
#' @param result_components Optional normalized component observations paired
#'   with `result_aggregations`.
#'
#' @return `NULL`, invisibly. The function is called for its database effects.
#'
#' @keywords internal
#' @noRd
synchronize_discrete_sample_detail <- function(
  con,
  sample_id,
  remote_results,
  result_ids,
  pending_results,
  sample_qualifiers,
  sample_observers,
  result_aggregations,
  result_components
) {
  if (
    all(vapply(
      list(
        sample_qualifiers,
        sample_observers,
        result_aggregations,
        result_components
      ),
      is.null,
      logical(1)
    ))
  ) {
    return(invisible(NULL))
  }

  active_trans <- dbTransBegin(con)
  transaction_finished <- FALSE
  on.exit(
    if (active_trans && !transaction_finished) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    },
    add = TRUE
  )
  tryCatch(
    {
      if (!is.null(result_aggregations)) {
        set_result_aggregation_constraints(con, "deferred")
      }
      if (length(pending_results)) {
        dbAppendTableRLS(
          con,
          "discrete.results",
          data.table::rbindlist(
            pending_results,
            use.names = TRUE,
            fill = TRUE
          )
        )
      }

      if (!is.null(sample_qualifiers)) {
        DBI::dbExecute(
          con,
          "DELETE FROM discrete.sample_qualifiers WHERE sample_id = $1",
          params = list(sample_id)
        )
        if (!inherits(sample_qualifiers, "data.frame")) {
          sample_qualifiers <- data.frame(
            qualifier_type_id = as.integer(sample_qualifiers)
          )
        }
        sample_qualifiers <- sample_qualifiers[,
          intersect(c("qualifier_type_id", "note"), names(sample_qualifiers)),
          drop = FALSE
        ]
        if (!"qualifier_type_id" %in% names(sample_qualifiers)) {
          stop("sample_qualifiers must contain qualifier_type_id.")
        }
        if (nrow(sample_qualifiers)) {
          sample_qualifiers$sample_id <- sample_id
          dbAppendTableRLS(
            con,
            "discrete.sample_qualifiers",
            sample_qualifiers
          )
        }
      }

      if (!is.null(sample_observers)) {
        DBI::dbExecute(
          con,
          "DELETE FROM discrete.sample_observers WHERE sample_id = $1",
          params = list(sample_id)
        )
        if (!inherits(sample_observers, "data.frame")) {
          sample_observers <- data.frame(
            observer_id = as.integer(sample_observers)
          )
        }
        sample_observers <- sample_observers[,
          intersect(
            c("observer_id", "observer_role", "note"),
            names(sample_observers)
          ),
          drop = FALSE
        ]
        if (!"observer_id" %in% names(sample_observers)) {
          stop("sample_observers must contain observer_id.")
        }
        if (!"observer_role" %in% names(sample_observers)) {
          sample_observers$observer_role <- "sampler"
        }
        if (nrow(sample_observers)) {
          sample_observers$sample_id <- sample_id
          dbAppendTableRLS(
            con,
            "discrete.sample_observers",
            sample_observers
          )
        }
      }

      if (!is.null(result_aggregations)) {
        target_rows <- sort(unique(result_aggregations$result_row))
        target_result_ids <- result_ids[target_rows]
        if (any(is.na(target_result_ids))) {
          stop(
            "A result aggregation could not be matched to a canonical result."
          )
        }

        database_aggregations <- DBI::dbGetQuery(
          con,
          "SELECT ra.result_id
           FROM discrete.result_aggregations ra
           JOIN discrete.results r USING (result_id)
           WHERE r.sample_id = $1",
          params = list(sample_id)
        )
        previously_aggregated <- intersect(
          result_ids[!is.na(result_ids)],
          database_aggregations$result_id
        )
        affected_result_ids <- unique(c(
          target_result_ids,
          previously_aggregated
        ))
        rebuilding_ids <- integer()
        if (length(affected_result_ids)) {
          placeholders <- paste0("$", seq_along(affected_result_ids))
          protected_results <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT result_id FROM discrete.results WHERE no_source_update AND result_id IN (",
              paste(placeholders, collapse = ", "),
              ")"
            ),
            params = as.list(as.integer(affected_result_ids))
          )$result_id
          if (length(protected_results)) {
            stop(
              "Cannot replace aggregation detail for no_source_update result_id(s): ",
              paste(protected_results, collapse = ", "),
              "."
            )
          }
          # Results that are no longer aggregated become direct results through
          # the explicit conversion operation, which deletes the aggregation,
          # assigns the new direct state, and preserves a reason in the audit
          # trail. A raw DELETE here is refused by
          # forbid_result_aggregation_delete_trigger.
          converting_ids <- setdiff(previously_aggregated, target_result_ids)
          for (converting_id in converting_ids) {
            result_row <- which(result_ids == converting_id)[[1]]
            result_condition <- if (
              "result_condition" %in% names(remote_results)
            ) {
              remote_results$result_condition[[result_row]]
            } else {
              NA_integer_
            }
            result_condition_value <- if (
              "result_condition_value" %in% names(remote_results)
            ) {
              remote_results$result_condition_value[[result_row]]
            } else {
              NA_real_
            }
            incoming_result <- remote_results$result[[result_row]]
            # With nothing to assign there is no replacement value, so the
            # calculated value stands as the new direct one.
            conversion_mode <- if (
              is.na(incoming_result) && is.na(result_condition)
            ) {
              "preserve_calculated"
            } else {
              "replace"
            }
            DBI::dbExecute(
              con,
              "SELECT discrete.convert_result_aggregation_to_direct(
                 $1, $2, $3, $4, $5, $6
               )",
              params = list(
                as.integer(converting_id),
                conversion_mode,
                if (identical(conversion_mode, "preserve_calculated")) {
                  NA_real_
                } else {
                  incoming_result
                },
                if (identical(conversion_mode, "preserve_calculated")) {
                  NA_integer_
                } else {
                  result_condition
                },
                if (identical(conversion_mode, "preserve_calculated")) {
                  NA_real_
                } else {
                  result_condition_value
                },
                paste0(
                  "Incoming sample detail no longer aggregates this result ",
                  "(synchronize_discrete_sample_detail, source_adapter_function ",
                  "sample_id ",
                  sample_id,
                  ")."
                )
              )
            )
          }
          # Targets keep their aggregation row and audit identity. The shared
          # writer updates their definitions and replaces their components
          # while the constraints are deferred.
          rebuilding_ids <- intersect(
            target_result_ids,
            database_aggregations$result_id
          )
        }

        for (result_row in target_rows) {
          DBI::dbExecute(
            con,
            "UPDATE discrete.results
             SET result = NULL,
                 result_condition = NULL,
                 result_condition_value = NULL
             WHERE result_id = $1",
            params = list(result_ids[[result_row]])
          )
        }
        append_discrete_result_aggregations(
          con = con,
          result_ids = result_ids,
          result_aggregations = result_aggregations,
          result_components = result_components,
          replace_existing_result_ids = rebuilding_ids
        )
      }

      if (!is.null(result_aggregations)) {
        set_result_aggregation_constraints(con, "immediate")
      }

      if (active_trans) {
        DBI::dbExecute(con, "COMMIT")
      }
      transaction_finished <- TRUE
      invisible(NULL)
    },
    error = function(e) {
      if (active_trans) {
        try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
        transaction_finished <<- TRUE
      }
      stop(e)
    }
  )
}


#' Synchronize hydro DB with remote sources
#'
#' @description
#'
#' This synchronize function pulls and replaces data referenced in table 'sample_series' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence.
#'
#' @details
#' Each sample series uses the active source-adapter assignment with the lowest
#' synchronization priority. Samples missing remotely are deleted only when
#' their existing `source_adapter_function` matches the selected source function,
#' protecting records imported through another route.
#'
#' Every source function must have an enabled discrete-domain entry in
#' `public.source_adapter_capabilities`.
#'
#' Source functions use the same `sample`, `results`, and optional association
#' and result-aggregation return contract documented by [getNewDiscrete()]. Existing
#' group memberships are not removed when `sample_groups` is omitted, and
#' source-provided groups are added without replacing unrelated memberships.
#' For a matched sample, supplied qualifier and observer collections replace
#' their respective associations; omission preserves the database values.
#' Supplied aggregation configurations and components replace that detail for
#' matched canonical results; two supplied empty data frames explicitly remove
#' existing aggregation detail through the administrative aggregate-to-direct
#' conversion, while omission preserves it.
#' Locationless remote samples are matched by their database-enforced
#' `source_adapter_function` and `external_sample_id` identity. Located samples retain
#' location and collection-context matching because source IDs may legitimately
#' recur at different locations.
#' Invalid source result rows are skipped without deleting their stored
#' counterparts, including with `delete = TRUE`. Compatible enrichment of
#' historical unknown metadata preserves `result_id` when the pairing is unique
#' across the source batch and identifies the same measurement. Recorded,
#' conflicting chemistry is not treated as enrichment. Ambiguous pairings are
#' retained for review, and `no_source_update` is respected.
#' Named-list analysis conditions are normalized to JSON before matching and
#' writing. Result reconciliation and child updates use one transaction.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param sample_series_id The sample_series_id you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `sample_series_id`, or one per element of `sample_series_id`
#' @param active Sets behavior for checking sample_series_ids or not. If set to 'default', the function will look to the column 'active' in the 'sample_series_id' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all sample_series_id
#' @param sync_remote_false Controls whether to synchronize sample_series that have the `sync_remote` column set to FALSE in the `sample_series` table. Usually if this column is set to FALSE it means that the series should not be synchronized, so use with caution!
#' @param delete If TRUE, the function will delete located samples and/or
#'   results that are not found remotely when their `source_adapter_function` matches the
#'   selected source function. Locationless samples are not deleted
#'   automatically because they are not owned by one location-based sample
#'   series. If FALSE, no data are deleted.
#' @param snowCon A connection to the snow course database, created with [snowConnect()]. NULL will create a connection using the same connection host and port as the 'con' connection object and close it afterwards. Not used if no data is pulled from the snow database.
#'
#' @return A data.table with one row per inserted or matched remote sample and
#'   columns `sample_series_id`, `sample_id`, `action`, and list-columns
#'   containing the normalized source inputs.
#' @export
#'

synchronize_discrete <- function(
  con = NULL,
  sample_series_id = "all",
  start_datetime,
  active = 'default',
  sync_remote_false = FALSE,
  delete = FALSE,
  snowCon = NULL
) {
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }

  if (inherits(start_datetime, "Date")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else if (inherits(start_datetime, "character")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else if (!inherits(start_datetime, "POSIXct")) {
    stop("start_datetime must be a Date, character, or POSIXct object.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")
  EQWinConCache <- eqwin_connection_cache_new()
  on.exit(eqwin_connection_cache_disconnect(EQWinConCache), add = TRUE)

  series_select_sql <-
    "SELECT
       ss.*,
       source.sample_series_source_adapter_id,
       source.source_fx,
       source.source_fx_args,
       source.synchronize_priority
     FROM discrete.sample_series ss
     LEFT JOIN LATERAL (
       SELECT
         ssa.sample_series_source_adapter_id,
         ssa.source_fx,
         ssa.source_fx_args,
         ssa.synchronize_priority
       FROM discrete.sample_series_source_adapters ssa
       WHERE ssa.sample_series_id = ss.sample_series_id
         AND ssa.active
         AND ssa.synchronize_priority IS NOT NULL
       ORDER BY
         ssa.synchronize_priority,
         ssa.sample_series_source_adapter_id
       LIMIT 1
     ) source ON TRUE"

  start <- Sys.time()

  message("Synchronizing sample series with synchronize_discrete...")

  # Check length of start_datetime is either 1 of same as sample_series_id
  if (length(start_datetime) != 1) {
    if (length(start_datetime) != length(sample_series_id)) {
      stop(
        "There is not exactly one element to start_datetime per valid sample_series_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for sample_series_id that doesn't exist."
      )
    }
    start_datetime_by_series <- stats::setNames(
      as.list(start_datetime),
      as.character(sample_series_id)
    )
  } else {
    sample_series_id <- unique(sample_series_id)
    start_datetime_by_series <- NULL
  }

  if (sample_series_id[1] == "all") {
    all_series <- DBI::dbGetQuery(con, series_select_sql)
  } else {
    all_series <- DBI::dbGetQuery(
      con,
      paste0(
        series_select_sql,
        " WHERE ss.sample_series_id IN (",
        paste(sample_series_id, collapse = ", "),
        ");"
      )
    )
    if (length(unique(sample_series_id)) != nrow(all_series)) {
      fail <- sample_series_id[
        !sample_series_id %in% all_series$sample_series_id
      ]
      ifelse(
        (length(fail) == 1),
        warning(
          "Could not find one of the sample_series_ids that you specified: ID ",
          fail,
          " is missing from the database."
        ),
        warning(
          "Could not find some of the sample_series_ids that you specified: IDs ",
          paste(fail, collapse = ", "),
          " are missing from the database."
        )
      )
    }
  }

  if (active == 'default') {
    all_series <- all_series[all_series$active, ]
  }
  if (!sync_remote_false) {
    all_series <- all_series[all_series$sync_remote, ]
  }
  if (nrow(all_series) == 0) {
    stop("Could not find any sample series matching your input parameters.")
  }

  missing_source <- is.na(all_series$source_fx)
  if (any(missing_source)) {
    warning(
      "The following sample series have no active source-adapter assignment ",
      "with a synchronize priority and will be ignored: ",
      paste(all_series$sample_series_id[missing_source], collapse = ", "),
      "."
    )
    all_series <- all_series[!missing_source, , drop = FALSE]
  }
  if (nrow(all_series) == 0L) {
    stop(
      "Could not find any sample series with an active source-adapter ",
      "assignment for synchronization."
    )
  }

  registered_source_fx <- getSourceAdapterCapabilities(
    con = con,
    data_domain = "discrete"
  )$source_fx
  unregistered_source_fx <- setdiff(
    unique(all_series$source_fx),
    registered_source_fx
  )
  if (length(unregistered_source_fx) > 0L) {
    stop(
      "synchronize_discrete: Every source_fx must have an enabled entry in ",
      "public.source_adapter_capabilities for the discrete domain. ",
      "Missing or disabled: ",
      paste(unregistered_source_fx, collapse = ", "),
      "."
    )
  }

  valid_sample_names <- DBI::dbListFields(con, DBI::Id(schema = "discrete", table = "samples"))
  valid_result_names <- DBI::dbListFields(con, DBI::Id(schema = "discrete", table = "results"))

  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_series), style = 3)
  }

  new_samples <- 0 # Counter for number of newly created sample series
  updated_samples <- 0 # Counter for number of updated sample series
  updated_results <- 0 # Counter for number of updated results
  new_results <- 0 # Counter for number of new results
  sync_records <- list()

  # Start of for loop ########################################################
  for (i in seq_len(nrow(all_series))) {
    sid <- all_series$sample_series_id[i]

    # Acquire a lock for this timeseries to prevent concurrent updates, notably by getNewDiscrete
    # IMPORTANT: this lock will wait for other processes to release the lock, so if another process is stuck, this will be stuck too.
    lock_namespace <- "aquacache_sample_series"
    advisory_lock_acquire(
      con = con,
      namespace = lock_namespace,
      key = sid,
      wait = TRUE
    )

    tryCatch(
      {
        loc_id <- all_series$location_id[i]
        sub_loc_id <- all_series$sub_location_id[i]
        synch_from <- all_series$synch_from[i]
        synch_to <- all_series$synch_to[i]
        source_fx <- all_series$source_fx[i]
        source_fx_args <- all_series$source_fx_args[i]
        default_owner <- all_series$default_owner[i]
        default_contributor <- all_series$default_contributor[i]

        # Start from the caller's requested datetime, bounded by the sample
        # series' configured synchronization window.
        start_i <- if (is.null(start_datetime_by_series)) {
          start_datetime
        } else {
          start_datetime_by_series[[as.character(sid)]]
        }
        if (is.null(start_i)) {
          stop("No start_datetime was mapped for sample_series_id ", sid, ".")
        }
        if (!is.na(synch_from)) {
          start_i <- max(start_i, synch_from)
        }
        end_i <- if (!is.na(synch_to)) synch_to else Sys.time()

        if (source_fx == "downloadSnowCourseYG" & is.null(snowCon)) {
          # Try with the same host and port as the AquaCache connection
          dets <- DBI::dbGetQuery(
            con,
            "SELECT inet_server_addr() AS ip, inet_server_port() AS port"
          )
          snowCon <- snowConnect(
            host = dets$ip,
            port = dets$port,
            silent = TRUE
          )
          on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
        }

        args_list <- list(
          start_datetime = start_i,
          end_datetime = end_i,
          con = con
        )
        if (!is.na(source_fx_args)) {
          # add some arguments if they are specified
          args <- source_adapter_args_decode(source_fx_args)
          args_list <- c(args_list, args)
        }

        if (source_fx == "downloadEQWin") {
          args_list[["EQCon"]] <- eqwin_connection_cache_get(
            EQWinConCache,
            args_list[["EQpath"]]
          )
        }
        if (source_fx == "downloadSnowCourseYG") {
          args_list[["snowCon"]] <- snowCon
        }

        inRemote <- do.call(source_fx, args_list) # Get the data using the args_list

        if (length(inRemote) == 0) {
          # There was no data in remote for the date range specified
          DBI::dbExecute(
            con,
            "UPDATE discrete.sample_series SET last_synchronize = NOW() WHERE sample_series_id = $1",
            params = list(sid)
          )

          next
        } else {
          if (!inherits(inRemote, "list")) {
            stop(
              "For sample_series_id ",
              sid,
              " the source function did not return a list."
            )
          } else if (!inherits(inRemote[[1]], "list")) {
            stop(
              "For sample_series_id ",
              sid,
              " the source function did not return a list of lists (one element per sample, with two data.frames: one for sample metadata, the other for associated results)."
            )
          }

          if (delete) {
            # Extract and order the 'datetime' of each sample in the list
            extract_datetime <- function(x) {
              if (!("sample" %in% names(x))) {
                return(as.POSIXct(NA, tz = "UTC"))
              }
              if (is.null(x$sample$datetime) || is.na(x$sample$datetime)) {
                return(as.POSIXct(NA, tz = "UTC"))
              }
              as.POSIXct(x$sample$datetime, tz = "UTC")
            }
            inRemote_datetimes <- vapply(
              inRemote,
              extract_datetime,
              FUN.VALUE = as.POSIXct(NA, tz = "UTC")
            )
            inRemote_datetimes <- as.POSIXct(inRemote_datetimes, tz = "UTC")
            if (any(is.na(inRemote_datetimes))) {
              warning(
                "For sample_series_id ",
                sid,
                " the source function returned one or more samples with missing datetimes. Delete logic will skip those samples."
              )
            }
            order_idx <- order(inRemote_datetimes, na.last = TRUE)
            inRemote <- inRemote[order_idx]
            inRemote_datetimes <- inRemote_datetimes[order_idx]
          }

          for (j in seq_along(inRemote)) {
            if (
              !("sample" %in% names(inRemote[[j]])) |
                !("results" %in% names(inRemote[[j]]))
            ) {
              warning(
                "For sample_series_id ",
                sid,
                " the source function did not return a list with elements named 'sample' and 'results'. Failed on list element ",
                j,
                ", moving on to next element."
              )
              next
            }

            inRemote_sample <- inRemote[[j]][["sample"]]
            inRemote_results <- inRemote[[j]][["results"]]
            if ("sample_qualifier" %in% names(inRemote_sample)) {
              warning(
                "For sample_series_id ",
                sid,
                " element ",
                j,
                " returned sample_qualifier. Return sample_qualifiers as a ",
                "separate element instead. Skipping this source record."
              )
              next
            }
            sample_groups <- if ("sample_groups" %in% names(inRemote[[j]])) {
              inRemote[[j]][["sample_groups"]]
            } else {
              NULL
            }
            sample_qualifiers <- inRemote[[j]][["sample_qualifiers"]]
            sample_observers <- inRemote[[j]][["sample_observers"]]
            result_aggregations <- inRemote[[j]][["result_aggregations"]]
            result_components <- inRemote[[j]][["result_components"]]
            names_inRemote_samp <- names(inRemote_sample)

            # Normalize adapter location aliases before matching. An explicit
            # NA location_id is retained for Patch 57 locationless samples.
            if ("location" %in% names_inRemote_samp) {
              inRemote_sample$location_id <- loc_id
              inRemote_sample$location <- NULL
              names_inRemote_samp <- names(inRemote_sample)
            } else if (!("location_id" %in% names_inRemote_samp)) {
              inRemote_sample$location_id <- loc_id
              names_inRemote_samp <- names(inRemote_sample)
            }
            if ("sub_location" %in% names_inRemote_samp) {
              inRemote_sample$sub_location_id <- sub_loc_id
              inRemote_sample$sub_location <- NULL
              names_inRemote_samp <- names(inRemote_sample)
            } else if (!("sub_location_id" %in% names_inRemote_samp)) {
              inRemote_sample$sub_location_id <- if (
                is.na(inRemote_sample$location_id[[1]])
              ) {
                NA_integer_
              } else {
                sub_loc_id
              }
              names_inRemote_samp <- names(inRemote_sample)
            }
            inRemote_sample$source_adapter_function <- source_fx
            names_inRemote_samp <- names(inRemote_sample)
            if (
              !("external_sample_id" %in% names_inRemote_samp) ||
                is.na(inRemote_sample$external_sample_id[[1]]) ||
                !nzchar(trimws(as.character(
                  inRemote_sample$external_sample_id[[1]]
                )))
            ) {
              warning(
                "Every source sample must have a non-missing, nonblank ",
                "external_sample_id."
              )
              next
            }

            if (delete && !is.na(inRemote_sample$location_id[[1]])) {
              delete_has_prev <- j > 1 && !is.na(inRemote_datetimes[j - 1])
              delete_has_curr <- !is.na(inRemote_datetimes[j])
              if (delete_has_curr) {
                if (j == 1) {
                  # Delete any samples between the start of the series and the first sample in the remote data, if any. Cascades to results.
                  DBI::dbExecute(
                    con,
                    paste0(
                      "DELETE FROM discrete.samples WHERE datetime > '",
                      start_i,
                      "' AND datetime < '",
                      inRemote_datetimes[j],
                      "' AND location_id = ",
                      loc_id,
                      " AND sub_location_id ",
                      if (!is.na(sub_loc_id)) {
                        paste0("= ", sub_loc_id)
                      } else {
                        "IS NULL"
                      },
                      " AND z ",
                      if (!is.null(inRemote_sample$z)) {
                        paste0("= ", inRemote_sample$z)
                      } else {
                        "IS NULL"
                      },
                      " AND media_id = ",
                      inRemote_sample$media_id,
                      " AND sample_type = ",
                      inRemote_sample$sample_type,
                      " AND collection_method = ",
                      inRemote_sample$collection_method,
                      " AND source_adapter_function = '",
                      source_fx,
                      "' AND no_source_update IS FALSE;"
                    )
                  )
                } else if (j == length(inRemote) && delete_has_prev) {
                  DBI::dbExecute(
                    con,
                    paste0(
                      "DELETE FROM discrete.samples WHERE datetime < '",
                      end_i,
                      "' AND datetime > '",
                      inRemote_datetimes[j],
                      "' AND location_id = ",
                      loc_id,
                      " AND sub_location_id ",
                      if (!is.na(sub_loc_id)) {
                        paste0("= ", sub_loc_id)
                      } else {
                        "IS NULL"
                      },
                      " AND z ",
                      if (!is.null(inRemote_sample$z)) {
                        paste0("= ", inRemote_sample$z)
                      } else {
                        "IS NULL"
                      },
                      " AND media_id = ",
                      inRemote_sample$media_id,
                      " AND sample_type = ",
                      inRemote_sample$sample_type,
                      " AND collection_method = ",
                      inRemote_sample$collection_method,
                      " AND source_adapter_function = '",
                      source_fx,
                      "' AND no_source_update IS FALSE;"
                    )
                  )
                } else if (delete_has_prev) {
                  DBI::dbExecute(
                    con,
                    paste0(
                      "DELETE FROM discrete.samples WHERE datetime BETWEEN '",
                      inRemote_datetimes[j - 1] + 1,
                      "' AND '",
                      inRemote_datetimes[j] - 1,
                      "' AND location_id = ",
                      loc_id,
                      " AND sub_location_id ",
                      if (!is.na(sub_loc_id)) {
                        paste0("= ", sub_loc_id)
                      } else {
                        "IS NULL"
                      },
                      " AND z ",
                      if (!is.null(inRemote_sample$z)) {
                        paste0("= ", inRemote_sample$z)
                      } else {
                        "IS NULL"
                      },
                      " AND media_id = ",
                      inRemote_sample$media_id,
                      " AND sample_type = ",
                      inRemote_sample$sample_type,
                      " AND collection_method = ",
                      inRemote_sample$collection_method,
                      " AND source_adapter_function = '",
                      source_fx,
                      "' AND no_source_update IS FALSE;"
                    )
                  )
                }
              }
            }

            if (nrow(inRemote_results) == 0) {
              next
            }

            prepared <- prepare_discrete_import_results(con, inRemote_sample$media_id[[1]],
              inRemote_results, result_aggregations, result_components)
            if (any(nzchar(prepared$errors))) {
              rejected <- which(nzchar(prepared$errors))
              warning("For sample_series_id ", sid, ", sample ", j,
                ", skipped result row(s): ", paste(sprintf("%s (%s)", rejected,
                  prepared$errors[rejected]), collapse = "; "), call. = FALSE)
            }
            inRemote_results <- prepared$results
            result_aggregations <- prepared$result_aggregations
            result_components <- prepared$result_components
            if (!nrow(inRemote_results)) next
            aggregated_result_rows <- result_aggregations$result_row
            names_inRemote_res <- names(inRemote_results)

            remote_location_id <- suppressWarnings(as.integer(
              inRemote_sample$location_id[[1]]
            ))
            remote_sub_location_id <- suppressWarnings(as.integer(
              inRemote_sample$sub_location_id[[1]]
            ))
            remote_z <- if (
              "z" %in% names_inRemote_samp && length(inRemote_sample$z) > 0L
            ) {
              suppressWarnings(as.numeric(inRemote_sample$z[[1]]))
            } else {
              NA_real_
            }
            if (is.na(remote_location_id)) {
              inDB_sample <- find_locationless_import_sample(
                con = con,
                source_adapter_function = source_fx,
                external_sample_id = inRemote_sample$external_sample_id
              )
            } else {
              inDB_sample <- DBI::dbGetQuery(
                con,
                "SELECT *
                 FROM discrete.samples
                 WHERE datetime = $1
                   AND location_id = $2
                   AND sub_location_id IS NOT DISTINCT FROM $3
                   AND z IS NOT DISTINCT FROM $4
                   AND media_id = $5
                   AND sample_type = $6
                   AND collection_method = $7;",
                params = list(
                  as.POSIXct(inRemote_sample$datetime[[1]], tz = "UTC"),
                  remote_location_id,
                  remote_sub_location_id,
                  remote_z,
                  as.integer(inRemote_sample$media_id[[1]]),
                  as.integer(inRemote_sample$sample_type[[1]]),
                  as.integer(inRemote_sample$collection_method[[1]])
                )
              )
            }
            if (nrow(inDB_sample) > 1L) {
              warning(
                "For sample_series_id ",
                sid,
                " the remote sample matched more than one database sample."
              )
              next
            }
            if (
              nrow(inDB_sample) == 1L &&
                !is.na(remote_location_id) &&
                !isTRUE(
                  as.character(inDB_sample$source_adapter_function[[1]]) ==
                    as.character(source_fx)
                )
            ) {
              stop(
                "For sample_series_id ",
                sid,
                " the remote sample matches the unique location context of ",
                "sample_id ",
                inDB_sample$sample_id[[1]],
                ", but that sample belongs to source_adapter_function ",
                if (is.na(inDB_sample$source_adapter_function[[1]])) {
                  "NULL"
                } else {
                  shQuote(as.character(inDB_sample$source_adapter_function[[1]]))
                },
                " rather than ",
                shQuote(as.character(source_fx)),
                ". Refusing to update or insert against a sample owned by ",
                "another source."
              )
            }

            # Check for any changes/additions/subtractions to the sample metadata
            # If changes are detected, update the sample metadata
            if (nrow(inDB_sample) > 0) {
              # Check existing DB sample and results. If no sample is found, add the sample and corresponding results in else section
              if (inDB_sample$no_source_update) {
                # If no_source_update is TRUE, skip to the next sample
                next
              }
              # Check existing DB sample and results ##################
              ## Check sample metadata ##############
              sample_updated <- synchronize_discrete_sample_metadata(
                con = con,
                database_sample = inDB_sample,
                remote_sample = inRemote_sample,
                valid_sample_names = valid_sample_names,
                sample_groups = sample_groups,
                default_owner = default_owner,
                default_contributor = default_contributor
              )
              if (sample_updated) {
                updated_samples <- updated_samples + 1
              }

              # Get the results for the sample
              active_result_trans <- dbTransBegin(con)
              DBI::dbExecute(con, "SAVEPOINT synchronize_discrete_results")
              sample_new_results <- sample_updated_results <- 0L
              tryCatch({
                inDB_results <- DBI::dbGetQuery(
                  con,
                  paste0(
                    "SELECT r.*, (ra.result_id IS NOT NULL) AS is_aggregated
                     FROM discrete.results r
                     LEFT JOIN discrete.result_aggregations ra USING (result_id)
                     WHERE r.sample_id = ",
                    inDB_sample$sample_id,
                    " FOR UPDATE OF r;"
                  )
                )

                inDB_results$analysis_conditions <- vapply(inDB_results$analysis_conditions,
                  normalizeAnalysisConditions, character(1))
                # Rejected source rows are not evidence that stored measurements
                # disappeared. Keep their parameter variants even with delete=TRUE.
                rejected_ids <- prepared$rejected_results$parameter_id
                inDB_results$checked <- inDB_results$parameter_id %in% rejected_ids |
                  anyNA(rejected_ids)
                matches <- match_discrete_import_results(inDB_results, inRemote_results, aggregated_result_rows)
                inDB_results$checked[inDB_results$result_id %in% matches$retained_ids] <- TRUE
                remote_result_ids <- rep(NA_integer_, nrow(inRemote_results))
                pending_aggregated_results <- list()

                for (k in seq_len(nrow(inRemote_results))) {
                  sub <- inRemote_results[k, ]
                  names_inRemote_sub <- names(sub)
                  is_remote_aggregated <- k %in% aggregated_result_rows
                  resolved_sub_matrix_state_id <- sub$matrix_state_id
                  # The complete source batch has already resolved exact matches
                  # and unambiguous enrichments of historical unknown metadata.

                  if (matches$ambiguous[[k]]) {
                    warning("Ambiguous historical result for sample_series_id ", sid,
                      ", sample ", j, ", result row ", k, "; retained for review.", call. = FALSE)
                    next
                  }
                  matched_id <- matches$result_ids[[k]]
                  inDB_sub <- inDB_results[inDB_results$result_id %in% matched_id, , drop = FALSE]
                  if (nrow(inDB_sub) == 0) {
                    # Append new values
                    sub$result_id <- DBI::dbGetQuery(
                      con,
                      "SELECT nextval(
                         pg_get_serial_sequence(
                           'discrete.results',
                           'result_id'
                         )
                       )::integer AS result_id"
                    )$result_id[[1]]
                    remote_result_ids[[k]] <- sub$result_id
                    sub$sample_id <- inDB_sample$sample_id
                    sub$matrix_state_id <- resolved_sub_matrix_state_id
                    if (is_remote_aggregated) {
                      pending_aggregated_results[[
                        length(
                          pending_aggregated_results
                        ) +
                          1L
                      ]] <- sub
                    } else {
                      dbAppendTableRLS(con, "discrete.results", sub)
                    }

                    sample_new_results <- sample_new_results + 1L
                  } else if (nrow(inDB_sub) == 1) {
                    remote_result_ids[[k]] <- inDB_sub$result_id[[1]]
                    if (isTRUE(inDB_sub$no_source_update[[1]])) {
                      if (is_remote_aggregated) {
                        stop(
                          "Cannot synchronize component values for no_source_update ",
                          "result_id ",
                          inDB_sub$result_id[[1]],
                          "."
                        )
                      }
                      inDB_results[
                        inDB_results$result_id == inDB_sub$result_id,
                        "checked"
                      ] <- TRUE
                      next
                    }
                    # matching result found, check and adjust if necessary
                    # Check for differences in the results
                    columns <- setdiff(intersect(names_inRemote_sub, valid_result_names),
                      c("result_id", "sample_id", "analysis_conditions_key", "created",
                        "created_by", "modified", "modified_by"))
                    if (is_remote_aggregated || isTRUE(inDB_sub$is_aggregated[[1]])) {
                      columns <- setdiff(columns, c("result", "result_condition", "result_condition_value"))
                    }
                    changed <- columns[!vapply(columns, function(field)
                      isTRUE(all.equal(inDB_sub[[field]], sub[[field]], check.attributes = FALSE)), logical(1))]
                    updated_results_flag <- length(changed) > 0L
                    if (updated_results_flag) {
                      # Enrich all identity fields in one statement. The chemistry
                      # trigger must see the complete new representation, rather
                      # than an intermediate basis/form/endpoint combination.
                      assignments <- sprintf("%s = $%s",
                        as.character(DBI::dbQuoteIdentifier(con, changed)), seq_along(changed))
                      DBI::dbExecute(con, paste0("UPDATE discrete.results SET ",
                        paste(assignments, collapse = ", "), " WHERE result_id = $", length(changed) + 1L),
                        params = c(unname(lapply(changed, function(field) sub[[field]][[1]])),
                          list(inDB_sub$result_id[[1]])))
                    }
                    if (updated_results_flag) {
                      sample_updated_results <- sample_updated_results + 1L
                    }
                    inDB_results[
                      inDB_results$result_id == inDB_sub$result_id,
                      "checked"
                    ] <- TRUE # result entry will not be deleted from the database
                  } else {
                    warning(
                      "For sample_series_id ",
                      sid,
                      ", returned sample ",
                      j,
                      " (sample_datetime ",
                      inRemote_sample$datetime,
                      ") the source function returned a result that matched more than one result in the database. This should not happen. Skipping this result."
                    )
                    inDB_results[
                      inDB_results$result_id == inDB_sub$result_id,
                      "checked"
                    ] <- TRUE # result entry will not be deleted
                  }
                }
                synchronize_discrete_sample_detail(
                  con = con,
                  sample_id = inDB_sample$sample_id[[1]],
                  remote_results = inRemote_results,
                  result_ids = remote_result_ids,
                  pending_results = pending_aggregated_results,
                  sample_qualifiers = sample_qualifiers,
                  sample_observers = sample_observers,
                  result_aggregations = result_aggregations,
                  result_components = result_components
                )
                # Remove from the database any results that were not checked if delete is TRUE
                if (delete) {
                  to_delete <- inDB_results[
                    !inDB_results$checked & !inDB_results$no_source_update,
                    "result_id"
                  ]
                  if (length(to_delete) > 0) {
                    DBI::dbExecute(
                      con,
                      paste0(
                        "DELETE FROM discrete.results WHERE result_id IN (",
                        paste(to_delete, collapse = ", "),
                        ") AND no_source_update IS FALSE;"
                      )
                    )
                  }
                }
                DBI::dbExecute(con, "RELEASE SAVEPOINT synchronize_discrete_results")
                if (active_result_trans) DBI::dbExecute(con, "COMMIT")
              }, error = function(e) {
                DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT synchronize_discrete_results")
                DBI::dbExecute(con, "RELEASE SAVEPOINT synchronize_discrete_results")
                if (active_result_trans) DBI::dbExecute(con, "ROLLBACK")
                stop(e)
              })
              new_results <- new_results + sample_new_results
              updated_results <- updated_results + sample_updated_results
              sync_records[[length(sync_records) + 1L]] <-
                new_discrete_import_record(
                  sample_series_id = sid,
                  sample_id = inDB_sample$sample_id[[1]],
                  action = "synchronized",
                  sample = inRemote_sample,
                  results = inRemote_results,
                  sample_groups = sample_groups,
                  sample_qualifiers = sample_qualifiers,
                  sample_observers = sample_observers,
                  result_aggregations = result_aggregations,
                  result_components = result_components
                )

              # Inserting a new sample #########
            } else {
              # No database sample was found, add the sample and corresponding results (follow same process as getNewDiscrete)
              ## Checks on sample metadata ###########
              # Functions may pass the location code instead of location_id, change it
              # Also possible that the function did not pass 'location_id' at all, if so fill it in using 'loc_id'
              if ("location" %in% names_inRemote_samp) {
                inRemote_sample$location_id <- loc_id
                inRemote_sample$location <- NULL
                names_inRemote_samp <- names(inRemote_sample)
              } else if (!("location_id" %in% names_inRemote_samp)) {
                inRemote_sample$location_id <- loc_id
                names_inRemote_samp <- names(inRemote_sample)
              }
              if ("sub_location" %in% names_inRemote_samp) {
                inRemote_sample$sub_location_id <- sub_loc_id
                inRemote_sample$sub_location <- NULL
                names_inRemote_samp <- names(inRemote_sample)
              }

              # Source adapters must return a stable external sample identifier.
              mandatory_samp <- c(
                "location_id",
                "media_id",
                "datetime",
                "collection_method",
                "sample_type",
                "external_sample_id"
              )
              if (!all(c(mandatory_samp) %in% names_inRemote_samp)) {
                # Make an error message stating which column is missing
                missing <- c(mandatory_samp)[
                  !c(mandatory_samp) %in% names_inRemote_samp
                ]
                warning(
                  "For sample_series_id ",
                  sid,
                  ", returned sample ",
                  j,
                  " (sample_datetime ",
                  inRemote_sample$datetime,
                  ") the source function did not return one or more mandatory column(s) for the sample metadata to enable the addition of new samples found in the remote: '",
                  paste(missing, collapse = "', '"),
                  "'. Skipping to next sample."
                )
                next
              }

              # Apply default owner/contributor if not provided
              if (
                !("owner" %in% names_inRemote_samp) ||
                  is.na(inRemote_sample$owner)
              ) {
                inRemote_sample$owner <- default_owner
                names_inRemote_samp <- names(inRemote_sample)
              }
              if (
                is.null(inRemote_sample$owner) || is.na(inRemote_sample$owner)
              ) {
                warning(
                  "For sample_series_id ",
                  sid,
                  ", returned sample ",
                  j,
                  " (sample_datetime ",
                  inRemote_sample$datetime,
                  ") the source function did not provide an owner and there is no default owner for the sample series. Skipping to next sample."
                )
                next
              }
              if (
                !("contributor" %in% names_inRemote_samp) ||
                  is.na(inRemote_sample$contributor)
              ) {
                if (!is.na(default_contributor)) {
                  inRemote_sample$contributor <- default_contributor
                }
              }

              # Use share_with from the source function when supplied, otherwise fall back to datatabase default
              if ("share_with" %in% names_inRemote_samp) {
                if (!is.list(inRemote_sample$share_with)) {
                  inRemote_sample$share_with <- paste0(
                    "{",
                    paste(inRemote_sample$share_with, collapse = ", "),
                    "}"
                  )
                }
              }

              ## Checks on results ###########
              # Check that the results have the mandatory columns
              mandatory_res <- c("result", "result_type", "parameter_id")
              if (!all(c(mandatory_res) %in% names_inRemote_res)) {
                # Make an error message stating which column is missing
                missing <- c(mandatory_res)[
                  !c(mandatory_res) %in% names_inRemote_res
                ]
                warning(
                  "For sample_series_id ",
                  sid,
                  ", returned sample ",
                  j,
                  " (sample_datetime ",
                  inRemote_sample$datetime,
                  ") the source function did not return one or more mandatory column(s) for the results: '",
                  paste(missing, collapse = "', '"),
                  "'. Skipping to the next sample."
                )
                next
              }

              # Append values
              sample_action <- "inserted"
              sample_id <- tryCatch(
                {
                  addNewDiscrete(
                    con = con,
                    sample = inRemote_sample,
                    results = inRemote_results,
                    sample_groups = sample_groups,
                    sample_qualifiers = sample_qualifiers,
                    sample_observers = sample_observers,
                    result_aggregations = result_aggregations,
                    result_components = result_components
                  )
                },
                error = function(e) {
                  if (is.na(inRemote_sample$location_id[[1]])) {
                    existing_sample <- find_locationless_import_sample(
                      con = con,
                      source_adapter_function = source_fx,
                      external_sample_id = inRemote_sample$external_sample_id
                    )
                    if (nrow(existing_sample) == 1L) {
                      link_discrete_sample_groups(
                        con = con,
                        sample_id = existing_sample$sample_id[[1]],
                        sample_groups = sample_groups,
                        default_owner = inRemote_sample$owner[[1]],
                        default_contributor = if (
                          "contributor" %in% names(inRemote_sample)
                        ) {
                          inRemote_sample$contributor[[1]]
                        } else {
                          NA_integer_
                        }
                      )
                      sample_action <<- "existing"
                      return(existing_sample$sample_id[[1]])
                    }
                  }
                  warning(
                    "synchronize_discrete: Failed to commit new data for sample_series_id, ",
                    sid,
                    ". Failed on fetched sample number ",
                    j,
                    " with error message: ",
                    e$message
                  )
                  NA
                }
              )
              if (!is.na(sample_id)) {
                if (sample_action == "inserted") {
                  new_samples <- new_samples + 1
                }
                sync_records[[length(sync_records) + 1L]] <-
                  new_discrete_import_record(
                    sample_series_id = sid,
                    sample_id = sample_id,
                    action = sample_action,
                    sample = inRemote_sample,
                    results = inRemote_results,
                    sample_groups = sample_groups,
                    sample_qualifiers = sample_qualifiers,
                    sample_observers = sample_observers,
                    result_aggregations = result_aggregations,
                    result_components = result_components
                  )
              }
            } # End of if no sample is found (making a new one)
          } # End of loop over inRemote
        }
      },
      error = function(e) {
        warning(
          "synchronize discrete failed on sample_series_id ",
          sid,
          " with error: ",
          e$message
        )
      },
      finally = {
        # Release the lock
        advisory_lock_release(con, lock_namespace, sid)
      }
    ) # End of tryCatch

    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
  } # End of for loop

  if (interactive()) {
    close(pb)
  }

  try(
    # In a try in case the user doesn't have update permissions on internal_status
    {
      DBI::dbExecute(
        con,
        "UPDATE information.internal_status SET value = NOW() WHERE event = 'last_synchronize_discrete';"
      )
    },
    silent = TRUE
  )

  message(
    "Found ",
    new_samples,
    " new samples to add to the ",
    nrow(all_series),
    " sample_series provided."
  )
  message(
    "Found ",
    updated_samples,
    " samples to update for the ",
    nrow(all_series),
    " sample_series provided."
  )
  message(
    "Found ",
    new_results,
    " new results to add to the ",
    nrow(all_series),
    " sample_series provided."
  )
  message(
    "Found ",
    updated_results,
    " results to update for the ",
    nrow(all_series),
    " sample_series provided."
  )
  diff <- Sys.time() - start
  message(
    "Total elapsed time for synchronize discrete: ",
    round(diff[[1]], 2),
    " ",
    units(diff),
    ". End of function."
  )
  bind_discrete_import_records(sync_records)
} #End of function
