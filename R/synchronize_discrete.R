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
        if (column %in% c("share_with", "import_source")) {
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


#' Synchronize hydro DB with remote sources
#'
#' @description
#'
#' This synchronize function pulls and replaces data referenced in table 'sample_series' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence.
#'
#' @details
#' Each sample series uses the active source-adapter assignment with the lowest
#' synchronization priority. Samples missing remotely are deleted only when
#' their existing `import_source` matches the selected source function,
#' protecting records imported through another route.
#'
#' Every source function must have an enabled discrete-domain entry in
#' `public.source_adapter_capabilities`.
#'
#' Source functions use the same `sample`, `results`, and optional
#' `sample_groups` return contract documented by [getNewDiscrete()]. Existing
#' group memberships are not removed when `sample_groups` is omitted, and
#' source-provided groups are added without replacing unrelated memberships.
#' Locationless remote samples are matched by their database-enforced
#' `import_source` and `import_source_id` identity. Located samples retain
#' location and collection-context matching because source IDs may legitimately
#' recur at different locations.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param sample_series_id The sample_series_id you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `sample_series_id`, or one per element of `sample_series_id`
#' @param active Sets behavior for checking sample_series_ids or not. If set to 'default', the function will look to the column 'active' in the 'sample_series_id' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all sample_series_id
#' @param sync_remote_false Controls whether to synchronize sample_series that have the `sync_remote` column set to FALSE in the `sample_series` table. Usually if this column is set to FALSE it means that the series should not be synchronized, so use with caution!
#' @param delete If TRUE, the function will delete located samples and/or
#'   results that are not found remotely when their `import_source` matches the
#'   selected source function. Locationless samples are not deleted
#'   automatically because they are not owned by one location-based sample
#'   series. If FALSE, no data are deleted.
#' @param snowCon A connection to the snow course database, created with [snowConnect()]. NULL will create a connection using the same connection host and port as the 'con' connection object and close it afterwards. Not used if no data is pulled from the snow database.
#'
#' @return A data.table with one row per inserted or matched remote sample and
#'   columns `sample_series_id`, `sample_id`, `action`, and list-columns
#'   containing the normalized `sample`, `results`, and `sample_groups` input.
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

  valid_sample_names <- DBI::dbGetQuery(
    con,
    "SELECT column_name FROM information_schema.columns WHERE table_schema = 'discrete' AND table_name = 'samples';"
  )[, 1]
  valid_result_names <- DBI::dbGetQuery(
    con,
    "SELECT column_name FROM information_schema.columns WHERE table_schema = 'discrete' AND table_name = 'results';"
  )[, 1]

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

        if (source_fx == "downloadSnowCourse" & is.null(snowCon)) {
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
        if (source_fx == "downloadSnowCourse") {
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
            sample_groups <- if ("sample_groups" %in% names(inRemote[[j]])) {
              inRemote[[j]][["sample_groups"]]
            } else {
              NULL
            }
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
            inRemote_sample$import_source <- source_fx
            names_inRemote_samp <- names(inRemote_sample)
            if (
              !("import_source_id" %in% names_inRemote_samp) ||
                is.na(inRemote_sample$import_source_id[[1]]) ||
                !nzchar(trimws(as.character(
                  inRemote_sample$import_source_id[[1]]
                )))
            ) {
              warning(
                "Every source sample must have a non-missing, nonblank ",
                "import_source_id."
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
                      " AND import_source = '",
                      source_fx,
                      "' AND no_update IS FALSE;"
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
                      " AND import_source = '",
                      source_fx,
                      "' AND no_update IS FALSE;"
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
                      " AND import_source = '",
                      source_fx,
                      "' AND no_update IS FALSE;"
                    )
                  )
                }
              }
            }

            if (nrow(inRemote_results) == 0) {
              next
            }

            inRemote_results <- tryCatch(
              {
                normalize_discrete_result_matrix_states(
                  con = con,
                  sample_media_id = inRemote_sample$media_id[1],
                  results = inRemote_results
                )
              },
              error = function(e) {
                warning(
                  "For sample_series_id ",
                  sid,
                  ", returned sample ",
                  j,
                  " (sample_datetime ",
                  inRemote_sample$datetime,
                  ") the source function returned an invalid matrix_state value: ",
                  e$message,
                  " Skipping to next sample."
                )
                NULL
              }
            )
            if (is.null(inRemote_results)) {
              next
            }
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
                import_source = source_fx,
                import_source_id = inRemote_sample$import_source_id
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
                  as.character(inDB_sample$import_source[[1]]) ==
                    as.character(source_fx)
                )
            ) {
              stop(
                "For sample_series_id ",
                sid,
                " the remote sample matches the unique location context of ",
                "sample_id ",
                inDB_sample$sample_id[[1]],
                ", but that sample belongs to import_source ",
                if (is.na(inDB_sample$import_source[[1]])) {
                  "NULL"
                } else {
                  shQuote(as.character(inDB_sample$import_source[[1]]))
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
              if (inDB_sample$no_update) {
                # If no_update is TRUE, skip to the next sample
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
              inDB_results <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT * FROM discrete.results WHERE sample_id = ",
                  inDB_sample$sample_id,
                  ";"
                )
              )

              inDB_results$checked <- FALSE # This will be used to track which rows have been checked

              for (k in seq_len(nrow(inRemote_results))) {
                sub <- inRemote_results[k, ]
                names_inRemote_sub <- names(sub)
                resolved_sub_matrix_state_id <- sub$matrix_state_id
                # Sort out if there's an equivalent row in inDB_result. There could be new results! Results are unique on result_type, parameter_id, matrix_state_id, sample_fraction_id, result_value_type, result_speciation_id, protocol_method, laboratory, analysis_datetime, but not all columns might be populated in 'sub'

                idx <- inDB_results$result_type == sub$result_type &
                  inDB_results$parameter_id == sub$parameter_id
                idx <- idx &
                  if (!is.na(resolved_sub_matrix_state_id)) {
                    inDB_results$matrix_state_id == resolved_sub_matrix_state_id
                  } else {
                    is.na(inDB_results$matrix_state_id)
                  }
                idx <- idx &
                  (if (
                    "result_value_type" %in%
                      names_inRemote_sub &&
                      !is.na(sub$result_value_type)
                  ) {
                    inDB_results$result_value_type == sub$result_value_type
                  } else {
                    is.na(inDB_results$result_value_type)
                  })
                idx <- idx &
                  (if (
                    "result_speciation_id" %in%
                      names_inRemote_sub &&
                      !is.na(sub$result_speciation_id)
                  ) {
                    inDB_results$result_speciation_id ==
                      sub$result_speciation_id
                  } else {
                    is.na(inDB_results$result_speciation_id)
                  })
                idx <- idx &
                  (if (
                    "protocol_method" %in%
                      names_inRemote_sub &&
                      !is.na(sub$protocol_method)
                  ) {
                    inDB_results$protocol_method == sub$protocol_method
                  } else {
                    is.na(inDB_results$protocol_method)
                  })
                idx <- idx &
                  (if (
                    "laboratory" %in%
                      names_inRemote_sub &&
                      !is.na(sub$laboratory)
                  ) {
                    inDB_results$laboratory == sub$laboratory
                  } else {
                    is.na(inDB_results$laboratory)
                  })
                idx <- idx &
                  (if (
                    "analysis_datetime" %in%
                      names_inRemote_sub &&
                      !is.na(sub$analysis_datetime)
                  ) {
                    inDB_results$analysis_datetime == sub$analysis_datetime
                  } else {
                    is.na(inDB_results$analysis_datetime)
                  })
                idx <- idx &
                  (if (
                    "sample_fraction_id" %in%
                      names_inRemote_sub &&
                      !is.na(sub$sample_fraction_id)
                  ) {
                    inDB_results$sample_fraction_id == sub$sample_fraction_id
                  } else {
                    is.na(inDB_results$sample_fraction_id)
                  })
                inDB_sub <- inDB_results[idx, ]

                if (nrow(inDB_sub) == 0) {
                  # looks like a new result, add it (actually it might match an existing one but there's no way to know because some of the unique key columns were changed. If that's the case the 'old' one will be removed later)
                  ## Checks on results ###########
                  # Check that the results have the mandatory columns
                  mandatory_res <- c("result", "result_type", "parameter_id")
                  if (!all(c(mandatory_res) %in% names_inRemote_sub)) {
                    # Make an error message stating which column is missing
                    missing <- c(mandatory_res)[
                      !c(mandatory_res) %in% names_inRemote_sub
                    ]
                    stop(
                      "For sample_series_id ",
                      sid,
                      "  returned sample ",
                      j,
                      "(sample_datetime ",
                      inRemote_sample$datetime,
                      ") the source function did not return one or more mandatory column(s) for the result: '",
                      paste(missing, collapse = "', '"),
                      "'."
                    )
                  }

                  # More complex checks if 'result' is NA
                  # if there are NAs in the 'result' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
                  if (is.na(sub$result)) {
                    if (!("result_condition" %in% names_inRemote_sub)) {
                      warning(
                        "On sample_series_id ",
                        sid,
                        ", returned sample ",
                        j,
                        "(sample_datetime ",
                        inRemote_sample$datetime,
                        "), a value of NA is in column 'result' but there is no provided column 'result_condition'. Skipping this result."
                      )
                      next
                    } else {
                      # check that 'result_condition' is not NA.
                      if (is.na(sub$result_condition)) {
                        warning(
                          "On sample_series_id ",
                          sid,
                          ", returned sample ",
                          j,
                          "(sample_datetime ",
                          inRemote_sample$datetime,
                          "), a value of NA is in column 'result' but there is no corresponding value in column 'result_condition'. Skipping this result."
                        )
                        next
                      } else {
                        # check that 'result_condition' is not NA.
                        if (sub$result_condition %in% c(1, 2)) {
                          if (
                            !("result_condition_value" %in% names_inRemote_sub)
                          ) {
                            warning(
                              "For sample_series_id ",
                              sid,
                              " the source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value."
                            )
                            next
                          } else {
                            if (is.na(sub$result_condition_value)) {
                              warning(
                                "On sample_series_id ",
                                sid,
                                ", returned sample ",
                                j,
                                "(sample_datetime ",
                                inRemote_sample$datetime,
                                "), a value of 1 or 2 is in column 'result_condition' but there is no corresponding value in column 'result_condition_value. Skipping this result."
                              )
                              next
                            }
                          }
                        }
                      }
                    }
                  } # End of additional checks if any NA values in 'result' column are returned

                  # Get the result_speciation and sample_fraction boolean values for the parameters. If at least one TRUE then data must contain columns result_speciation_id and sample_fraction_id.
                  result_speciation <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT parameter_id, result_speciation AS result_speciation_bool FROM public.parameters WHERE parameter_id = ",
                      sub$parameter_id,
                      ";"
                    )
                  )
                  sample_fraction <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM public.parameters WHERE parameter_id = ",
                      sub$parameter_id,
                      ";"
                    )
                  )
                  if (result_speciation$result_speciation_bool) {
                    if (!("result_speciation_id" %in% names_inRemote_sub)) {
                      warning(
                        "The source function did not return a column 'result_speciation_id' but the database mandates this for parameter ",
                        sub$parameter_id,
                        ". Error occured on sample_series_id ",
                        sid,
                        ", returned sample ",
                        j,
                        "(sample_datetime ",
                        inRemote_sample$datetime,
                        "). Skipping this result."
                      )
                      next
                    } else {
                      # Check that value in result_speciation_id column of sub are not NA where necessary
                      if (is.na(sub$result_speciation_id)) {
                        warning(
                          "For sample_series_id ",
                          sid,
                          " the source function returned NA for 'result_speciation_id' for parameter ",
                          sub$parameter_id,
                          " where the database mandates this value. Error occured on sample_series_id ",
                          sid,
                          ", returned sample ",
                          j,
                          " (sample_datetime ",
                          inRemote_sample$datetime,
                          "). Skipping this result."
                        )
                        next
                      }
                    }
                  }
                  if (sample_fraction$sample_fraction_bool) {
                    if (!("sample_fraction_id" %in% names_inRemote_sub)) {
                      warning(
                        "The source function did not return a column 'sample_fraction_id' but the database mandates this for parameter ",
                        sub$parameter_id,
                        ". Error occured on sample_series_id ",
                        sid,
                        ", returned sample ",
                        j,
                        "(sample_datetime ",
                        inRemote_sample$datetime,
                        "). Skipping this result."
                      )
                      next
                    } else {
                      # Check that value in sample_fraction_id column of sub are not NA where necessary
                      if (is.na(sub$sample_fraction_id)) {
                        warning(
                          "For sample_series_id ",
                          sid,
                          " the source function returned NA for 'sample_fraction_id' for parameter ",
                          sub$parameter_id,
                          " where the database mandates this value. Error occured on sample_series_id ",
                          sid,
                          ", returned sample ",
                          j,
                          " (sample_datetime ",
                          inRemote_sample$datetime,
                          "). Skipping this result."
                        )
                        next
                      }
                    }
                  }

                  # Append new values
                  sub$sample_id <- inDB_sample$sample_id
                  sub$matrix_state_id <- resolved_sub_matrix_state_id
                  dbAppendTableRLS(con, "discrete.results", sub)

                  new_results <- new_results + 1
                } else if (nrow(inDB_sub) == 1) {
                  if (isTRUE(inDB_sub$no_update[[1]])) {
                    inDB_results[
                      inDB_results$result_id == inDB_sub$result_id,
                      "checked"
                    ] <- TRUE
                    next
                  }
                  # matching result found, check and adjust if necessary
                  # Check for differences in the results
                  updated_results_flag <- FALSE
                  for (l in names_inRemote_sub) {
                    if (l %in% valid_result_names) {
                      inDB_l <- inDB_sub[[l]]
                      sub_l <- sub[[l]]
                      # If the relevant columns in the two data.frames are all numbers, convert to numeric
                      if (!inherits(inDB_l, "numeric")) {
                        if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", inDB_l)) {
                          inDB_l <- as.numeric(inDB_l)
                        }
                      }
                      if (!inherits(sub_l, "numeric")) {
                        if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", sub_l)) {
                          sub_l <- as.numeric(sub_l)
                        }
                      }
                      if (!isTRUE(all.equal(inDB_l, sub_l))) {
                        result_col <- as.character(
                          DBI::dbQuoteIdentifier(con, l)
                        )
                        DBI::dbExecute(
                          con,
                          paste0(
                            "UPDATE discrete.results SET ",
                            result_col,
                            " = $1 WHERE result_id = $2;"
                          ),
                          params = list(
                            if (!is.na(sub_l)) sub_l else NA,
                            inDB_sub$result_id[[1]]
                          )
                        )

                        updated_results_flag <- TRUE
                      }
                    }
                  }
                  if (updated_results_flag) {
                    updated_results <- updated_results + 1
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
              # Remove from the database any results that were not checked if delete is TRUE
              if (delete) {
                to_delete <- inDB_results[
                  !inDB_results$checked & !inDB_results$no_update,
                  "result_id"
                ]
                if (length(to_delete) > 0) {
                  DBI::dbExecute(
                    con,
                    paste0(
                      "DELETE FROM discrete.results WHERE result_id IN (",
                      paste(to_delete, collapse = ", "),
                      ") AND no_update IS FALSE;"
                    )
                  )
                }
              }
              sync_records[[length(sync_records) + 1L]] <-
                new_discrete_import_record(
                  sample_series_id = sid,
                  sample_id = inDB_sample$sample_id[[1]],
                  action = "synchronized",
                  sample = inRemote_sample,
                  results = inRemote_results,
                  sample_groups = sample_groups
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

              # Check that the sample data has the required columns at minimum: c("location_id", "media_id", "datetime", "collection_method", "sample_type", "import_source_id"). Note that import_source_id is only mandatory because this function pulls data in from a remote source
              mandatory_samp <- c(
                "location_id",
                "media_id",
                "datetime",
                "collection_method",
                "sample_type",
                "import_source_id"
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

              # More complex checks if 'result' is NA
              # if there are NAs in the 'result' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
              if (any(is.na(inRemote_results$result))) {
                if (!("result_condition" %in% names_inRemote_res)) {
                  warning(
                    "For sample_series_id ",
                    sid,
                    ", returned sample ",
                    j,
                    " (sample_datetime ",
                    inRemote_sample$datetime,
                    ") the source function returned NA values in the column 'result' but did not return a column called 'result_condition'. Skipping to next sample."
                  )
                  next
                } else {
                  # Check that each NA in 'result' has a corresponding entry in 'result_condition'
                  sub.results <- inRemote_results[
                    is.na(inRemote_results$result),
                  ]
                  check_result_condition <- FALSE # prevents repeatedly checking for the same thing

                  next_flag <- FALSE
                  for (l in seq_len(nrow(sub.results))) {
                    if (
                      is.na(sub.results$result[l]) &
                        is.na(sub.results$result_condition[l])
                    ) {
                      warning(
                        "For sample_series_id ",
                        sid,
                        ", returned sample ",
                        j,
                        " (sample_datetime ",
                        inRemote_sample$datetime,
                        ") the source function returned at least one NA result in the column 'result' but did not return a corresponding entry in the column 'result_condition'. Skipping to the next sample."
                      )
                      next_flag <- TRUE
                    } else {
                      if (!check_result_condition) {
                        if (any(sub.results$result_condition %in% c(1, 2))) {
                          if (
                            !("result_condition_value" %in%
                              names(inRemote_results))
                          ) {
                            warning(
                              "For sample_series_id ",
                              sid,
                              ", returned sample ",
                              j,
                              " (sample_datetime ",
                              inRemote_sample$datetime,
                              ") the source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value. Skipping to the next sample."
                            )
                            next_flag <- TRUE
                          }
                        }
                        check_result_condition <- TRUE
                      }

                      if (sub.results$result_condition[l] %in% c(1, 2)) {
                        if (is.na(sub.results$result_condition_value[l])) {
                          warning(
                            "For sample_series_id ",
                            sid,
                            ", returned sample ",
                            j,
                            " (sample_datetime ",
                            inRemote_sample$datetime,
                            ") the source function returned a value of 1 or 2 in the column 'result_condition' (indicating above or below detection limit) but did not return a corresponding entry in the column 'result_condition_value'. Skipping to the next sample"
                          )
                          next_flag <- TRUE
                        }
                      }
                    }
                  } # End of looping over each row with NA in result column
                  if (next_flag) {
                    next
                  }
                }
              } # End of additional checks if any NA values in 'result' column are returned

              # Get the result_speciation and sample_fraction boolean values for the parameters. If at least one TRUE then data must contain columns result_speciation and sample_fraction_id.
              result_speciation <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT parameter_id, result_speciation AS result_speciation_bool FROM public.parameters WHERE parameter_id IN (",
                  paste(unique(inRemote_results$parameter_id), collapse = ", "),
                  ");"
                )
              )
              sample_fraction <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM public.parameters WHERE parameter_id IN (",
                  paste(unique(inRemote_results$parameter_id), collapse = ", "),
                  ");"
                )
              )
              if (any(result_speciation$result_speciation_bool)) {
                if (!("result_speciation_id" %in% names(inRemote_results))) {
                  warning(
                    "For sample_series_id ",
                    sid,
                    ", returned sample ",
                    j,
                    " (sample_datetime ",
                    inRemote_sample$datetime,
                    ") the source function did not return a column 'result_speciation_id' but the database mandates this for at least one of the parameters. Skipping to next sample."
                  )
                  next
                } else {
                  # Check that values in the result_speciation_id column are not NA where necessary
                  merge <- merge(
                    inRemote_results,
                    result_speciation,
                    by = "parameter_id"
                  )
                  # For rows where result_speciation_bool is TRUE, check that the corresponding result_speciation_id column is not NA
                  chk <- with(
                    merge,
                    result_speciation_bool & is.na(result_speciation_id)
                  )
                  if (any(chk)) {
                    warning(
                      "For sample_series_id ",
                      sid,
                      ", returned sample ",
                      j,
                      " (sample_datetime ",
                      inRemote_sample$datetime,
                      ") the source function returned NA values in the column 'result_speciation_id' for at least one parameter where the database mandates this value. Skipping to next sample."
                    )
                    next
                  }
                }
              }
              if (any(sample_fraction$sample_fraction_bool)) {
                if (!("sample_fraction_id" %in% names(inRemote_results))) {
                  warning(
                    "For sample_series_id ",
                    sid,
                    ", returned sample ",
                    j,
                    " (sample_datetime ",
                    inRemote_sample$datetime,
                    ") the source function did not return a column 'sample_fraction_id' but the database mandates this for at least one of the parameters. Skipping to next sample."
                  )
                  next
                } else {
                  # Check that all values in the sample_fraction_id column are not NA where necessary
                  merge <- merge(
                    inRemote_results,
                    sample_fraction,
                    by = "parameter_id"
                  )
                  # For rows where sample_fraction_bool is TRUE, check that the corresponding sample_fraction_id column is not NA
                  chk <- with(
                    merge,
                    sample_fraction_bool & is.na(sample_fraction_id)
                  )
                  if (any(chk)) {
                    warning(
                      "For sample_series_id ",
                      sid,
                      ", returned sample ",
                      j,
                      " (sample_datetime ",
                      inRemote_sample$datetime,
                      "), the source function returned NA values in the column 'sample_fraction_id' for at least one parameter where the database mandates this value. Skipping to next sample."
                    )
                    next
                  }
                }
              }

              # Append values
              sample_action <- "inserted"
              sample_id <- tryCatch(
                {
                  addNewDiscrete(
                    con = con,
                    sample = inRemote_sample,
                    results = inRemote_results,
                    sample_groups = sample_groups
                  )
                },
                error = function(e) {
                  if (is.na(inRemote_sample$location_id[[1]])) {
                    existing_sample <- find_locationless_import_sample(
                      con = con,
                      import_source = source_fx,
                      import_source_id = inRemote_sample$import_source_id
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
                    sample_groups = sample_groups
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
      warning = function(w) {
        warning(
          "synchronize discrete had a warning on sample_series_id ",
          sid,
          " with warning: ",
          w$message
        )
      },
      message = function(m) {
        message(
          "synchronize discrete had a message on sample_series_id ",
          sid,
          " with message: ",
          m$message
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
