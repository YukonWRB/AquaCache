#' Get new rasters
#'
#' @description
#' Retrieves rasters using the active assignment with the lowest fetch priority
#' in `spatial.raster_series_source_adapters` for each raster series. See
#' [addACRasterSeries()] for creating a series and its assignments.
#' Every source function must have an enabled raster-domain entry in
#' `public.source_adapter_capabilities`.
#' When `con = NULL`, independent raster series are processed in parallel when
#' the parallel backend is available. Each worker opens a connection on its
#' first assigned series and reuses it until the worker exits. Supplying `con`
#' keeps processing sequential.
#'
#' If you are a developer, note that download or source functions MUST be registered in AquaCache using function [registerSourceAdapterArguments()], and that this operation would normally be completed using the 'patch' system. See patch_56.R for examples.
#'
#' @param raster_series_ids A vector of raster_series_id's. Default 'all' fetches all ids in the raster_series_index table.
#' @param con A connection to the database. Default is NULL, which uses the package default connection settings and closes the connection afterwards. Parallel processing requires `con = NULL`; a supplied connection is used sequentially.
#' @param keep_forecasts Should forecasts be kept or replaced? Default is 'selective', which keeps only rasters for which there is no new forecast. 'all' keeps all forecasts, and 'none' replaces all forecasts. This does not apply to raster series labelled as 'reanalysis'
#' @param active Sets behavior for import of new rasters for raster series. If set to 'default', the column 'active' in the raster_series_index table will determine whether to get new raster or not. If set to 'all', all raster series will be fetched regardless of the 'active' column.
#' @param start_datetime A start datetime to fetch rasters from. By default, fetches from the last raster end_datetime + 1 second, however this parameter is provided for flexibility. If combined with `replace = TRUE`, could be used to replace rasters from a specific datetime to `end_datetime`. Specify as POSIXct or something coercible to POSIXct; coercion will be done with to UTC time zone. Existing forecast series continue from `last_issue`; this value is used only to initialize a forecast series that has no issue metadata yet.
#' @param end_datetime An end datetime to fetch rasters to. By default, fetches to the current time, however this parameter is provided for flexibility. If combined with `replace = TRUE` (Warning! parameter not implemented yet), could be used to replace rasters from `start_datetime` to a specific datetime. Specify as POSIXct or something coercible to POSIXct; coercion will be done with to UTC time zone. Only used for reanalysis rasters!
#' @return A character vector of raster series IDs that appended at least one
#'   raster. If individual rasters fail, processing continues, a detailed
#'   warning is emitted after all series have been processed, and the returned
#'   vector has an `append_errors` attribute containing the failure details.
#'   For reanalysis series, later rasters in the same series are not attempted
#'   after an append failure, preventing the series endpoint from advancing
#'   past missing data.
#' @export

getNewRasters <- function(
  raster_series_ids = "all",
  con = NULL,
  keep_forecasts = 'selective',
  active = 'default',
  start_datetime = NULL,
  end_datetime = NULL
) {
  restore_spatial_env <- unset_postgres_spatial_env()
  if (is.function(restore_spatial_env)) {
    on.exit(restore_spatial_env(), add = TRUE)
  }

  if (!keep_forecasts %in% c('selective', 'all', 'none')) {
    stop(
      "The 'keep_forecasts' parameter must be either 'selective', 'all', or 'none'."
    )
  }

  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }

  # Checks and conversions for datetimes
  if (!is.null(start_datetime)) {
    if (!inherits(start_datetime, "POSIXct")) {
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else {
      attr(start_datetime, "tzone") <- "UTC"
    }
  }

  if (!is.null(end_datetime)) {
    if (!inherits(end_datetime, "POSIXct")) {
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else {
      attr(end_datetime, "tzone") <- "UTC"
    }
  }

  parallel <- FALSE
  parallel_disabled_reason <- NULL
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
    parallel <- requireNamespace("foreach", quietly = TRUE) &&
      requireNamespace("doSNOW", quietly = TRUE)
    if (!parallel) {
      parallel_disabled_reason <- "missing_packages"
    }
  } else {
    parallel_disabled_reason <- "connection_provided"
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  raster_series_select_sql <-
    "SELECT
       rs.raster_series_id,
       rs.end_datetime,
       rs.last_issue,
       rt.raster_type_name AS type,
       source.source_fx,
       source.source_fx_args,
       source.fetch_priority,
       COALESCE(
         p.param_name,
         rs.parameter,
         CAST(rs.parameter_id AS TEXT)
       ) AS parameter_name,
       rs.active
     FROM spatial.raster_series_index rs
     JOIN spatial.raster_types rt ON rt.raster_type_id = rs.raster_type_id
     LEFT JOIN public.parameters p ON p.parameter_id = rs.parameter_id
     LEFT JOIN LATERAL (
       SELECT rsa.source_fx, rsa.source_fx_args, rsa.fetch_priority
       FROM spatial.raster_series_source_adapters rsa
       WHERE rsa.raster_series_id = rs.raster_series_id
         AND rsa.active
       ORDER BY rsa.fetch_priority, rsa.raster_series_source_adapter_id
       LIMIT 1
     ) source ON TRUE"

  # Create table of meta_ids
  if (raster_series_ids[1] == "all") {
    meta_ids <- DBI::dbGetQuery(
      con,
      raster_series_select_sql
    )
  } else {
    meta_ids <- DBI::dbGetQuery(
      con,
      paste0(
        raster_series_select_sql,
        " WHERE rs.raster_series_id IN ('",
        paste(raster_series_ids, collapse = "', '"),
        "');"
      )
    )
    if (length(raster_series_ids) != nrow(meta_ids)) {
      warning(
        "At least one requested raster_series_id could not be found."
      )
    }
  }

  if (active == 'default') {
    meta_ids <- meta_ids[meta_ids$active, ]
  }

  if (nrow(meta_ids) == 0) {
    message("No raster_series_id's found to update based on input parameters.")
    return(NULL)
  }

  missing_source <- is.na(meta_ids$source_fx)
  if (any(missing_source)) {
    warning(
      "The following raster series have no active source-adapter assignment ",
      "and will be ignored: ",
      paste(meta_ids$raster_series_id[missing_source], collapse = ", "),
      "."
    )
    meta_ids <- meta_ids[!missing_source, , drop = FALSE]
  }
  if (nrow(meta_ids) == 0L) {
    message("No raster series has an active source-adapter assignment.")
    return(NULL)
  }

  registered_source_fx <- getSourceAdapterCapabilities(
    con = con,
    data_domain = "raster"
  )$source_fx
  unregistered_source_fx <- setdiff(
    unique(meta_ids$source_fx),
    registered_source_fx
  )
  if (length(unregistered_source_fx) > 0L) {
    stop(
      "getNewRasters: Every source_fx must have an enabled entry in ",
      "public.source_adapter_capabilities for the raster domain. ",
      "Missing or disabled: ",
      paste(unregistered_source_fx, collapse = ", "),
      "."
    )
  }

  message("Fetching new rasters with getNewRasters")

  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(meta_ids), style = 3)
  }

  process_raster_series <- function(i, con) {
    count <- 0L
    raster_count <- 0L
    success <- character(0)
    append_errors <- list()
    series_error <- NULL
    id <- meta_ids[i, "raster_series_id"]
    make_series_result <- function(locked = FALSE) {
      list(
        raster_series_id = as.character(id),
        count = as.integer(count),
        success = success,
        raster_count = as.integer(raster_count),
        append_errors = append_errors,
        locked = locked,
        error = series_error
      )
    }
    message(
      "Working on raster_series_id ",
      id,
      " for parameter '",
      meta_ids[i, "parameter_name"],
      "' and source function '",
      meta_ids[i, "source_fx"],
      "'"
    )
    lock_namespace <- "aquacache_raster_series"
    lock_acquired <- advisory_lock_acquire(
      con = con,
      namespace = lock_namespace,
      key = id,
      wait = FALSE
    )
    if (!isTRUE(lock_acquired)) {
      return(make_series_result(locked = TRUE))
    }

    source_fx <- meta_ids[i, "source_fx"]
    source_fx_args <- meta_ids[i, "source_fx_args"]
    type <- meta_ids[i, "type"]
    end_datetime_i <- end_datetime
    start_datetime_i <- start_datetime
    if (type == "reanalysis") {
      # Reanalysis data may have preliminary rasters that should be replaced when final versions are produced.
      if (is.na(meta_ids[i, "end_datetime"])) {
        prelim <- NA
      } else {
        prelim <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT min(valid_to) FROM spatial.rasters_reference WHERE flag = 'PRELIMINARY' AND valid_from > '",
            meta_ids[i, "end_datetime"] - 60 * 60 * 24 * 30,
            "' AND raster_series_id = ",
            id,
            ";"
          )
        )[1, 1] # searches for rasters labelled 'prelim' within the last 30 days. HRDPA filters on product end times, so resume immediately before valid_to.
      }
      if (!is.na(prelim)) {
        if (!is.null(end_datetime)) {
          if (
            !is.null(start_datetime_i) &&
              start_datetime_i < prelim
          ) {
            next_instant <- start_datetime_i
          } else {
            next_instant <- prelim - 1 # one second before the last raster end_datetime so that the last earliest prelim raster is replaced.
          }
        } else {
          next_instant <- prelim - 1 # one second before the last raster end_datetime so that the last earliest prelim raster is replaced.
        }
      } else {
        # prelim is NA, so there are no preliminary rasters to replace. Fetch from the last raster end_datetime + 1 second, or from start_datetime if it is specified.
        if (!is.null(start_datetime_i)) {
          next_instant <- start_datetime_i
        } else {
          # start_datetime was null, find the last raster in table rasters_reference
          next_instant <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT MAX(valid_to) FROM spatial.rasters_reference WHERE raster_series_id = ",
              id
            )
          )[1, 1] +
            1 # one second after the last raster end_datetime
          if (is.na(next_instant)) {
            next_instant <- meta_ids[i, "end_datetime"]
          }
        }
      }
    } else if (type == "forecast") {
      # Existing forecast series continue from last_issue. An explicit start is
      # used only to initialize a new, empty series.
      if (!is.null(end_datetime_i)) {
        end_datetime_i <- NULL
      }
      next_instant <- meta_ids[i, "last_issue"]
      if (is.na(next_instant)) {
        if (!is.null(start_datetime_i)) {
          next_instant <- start_datetime_i
        } else {
          # A legacy empty series may have an end_datetime but no last_issue.
          next_instant <- meta_ids[i, "end_datetime"] + 1
        }
      }
      start_datetime_i <- NULL
    } else {
      # For other types of rasters, we fetch from the last raster end_datetime + 1 second, or from start_datetime if it is specified.
      if (!is.null(start_datetime_i)) {
        next_instant <- start_datetime_i
      } else {
        next_instant <- meta_ids[i, "end_datetime"] + 1 # one second after the last raster end_datetime
        if (is.na(next_instant)) {
          next_instant <- meta_ids[i, "end_datetime"]
        }
      }
    }

    tryCatch(
      withCallingHandlers(
        {
          args_list <- list(start_datetime = next_instant)
          if (!is.null(end_datetime_i)) {
            args_list$end_datetime <- end_datetime_i # If end_datetime_i is specified, add it to the args_list
          }
          if (!is.na(source_fx_args)) {
            # add some arguments if they are specified
            args <- source_adapter_args_decode(source_fx_args)
            args_list <- c(args_list, args)
          }

          rasters <- suppressWarnings(do.call(source_fx, args_list)) # Get the data using the args_list

          if (length(rasters) == 0) {
            # No new rasters found
            return(make_series_result())
          }
          if (!inherits(rasters, "list")) {
            warning(
              "The function specified in source_fx for raser_series_id ",
              id,
              " did not return a list. See documentation for details."
            )
            return(make_series_result())
          }

          # Extract forecast and issued_datetime from the list
          forecast <- rasters[["forecast"]]
          if (is.null(forecast)) {
            forecast <- FALSE
          }
          # The list of rasters 'rasters' only has an 'issued' element if the rasters are from a forecast. If not, it is NULL.
          issued_datetime <- rasters[["issued"]]
          if (is.null(issued_datetime)) {
            issued_datetime <- NA
          }
          rasters[["forecast"]] <- NULL # Remove the list element to simplify code below
          rasters[["issued"]] <- NULL

          null_rasters <- vapply(rasters, is.null, logical(1))
          if (any(null_rasters)) {
            message(
              "getNewRasters: Ignoring ",
              sum(null_rasters),
              " unavailable raster candidate",
              if (sum(null_rasters) == 1L) "." else "s."
            )
            rasters <- rasters[!null_rasters]
          }

          if (length(rasters) > 0) {
            series_raster_count <- 0L
            message(
              "Appending rasters for raster_series_id ",
              id,
              " with source function '",
              source_fx,
              "'"
            )
            for (j in seq_along(rasters)) {
              message(
                "Appending raster ",
                j,
                " of ",
                length(rasters),
                " for raster_series_id ",
                id
              )
              rast <- rasters[[j]]
              if (is.null(rast)) {
                next
              }
              append_failed <- FALSE

              valid_from <- rast[["valid_from"]]
              valid_to <- rast[["valid_to"]]
              issued <- rast[["issued"]]
              source <- rast[["source"]]
              flag <- rast[["flag"]]
              if (is.null(flag)) {
                flag <- NA
              }
              units <- rast[["units"]]
              model <- rast[["model"]]
              rast <- rast[["rast"]]

              # Append rasters one by one in transactions
              tryCatch(
                {
                  dbTransBegin(con)
                  DBI::dbExecute(con, "SET LOCAL lock_timeout = '30s';")
                  # Check if the raster already exists. If it does but flag is PRELIMINARY AND the new one is not, delete the prelim one and replace.
                  exists <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT reference_id FROM spatial.rasters_reference WHERE valid_from = '",
                      valid_from,
                      "' AND raster_series_id = ",
                      id,
                      " AND flag = 'PRELIMINARY';"
                    )
                  )[1, 1]
                  if (!is.na(exists) & is.na(flag)) {
                    # If the raster already exists and the new one is not a prelim, delete the prelim one and replace.
                    DBI::dbExecute(
                      con,
                      paste0(
                        "DELETE FROM spatial.rasters_reference WHERE reference_id = ",
                        exists,
                        ";"
                      )
                    ) # This should cascade to the rasters table
                  } else if (!is.na(exists) & !is.na(flag)) {
                    # The preliminary raster is unchanged. Close the transaction
                    # before skipping so the caller's connection is not left in
                    # an active transaction.
                    DBI::dbExecute(con, "ROLLBACK")
                    message("Already present; skipped")
                    next
                  } else if (is.na(exists)) {
                    # Check if the raster already exists in non-preliminary format
                    existing_final <- DBI::dbGetQuery(
                      con,
                      "SELECT
                         reference_id,
                         valid_to IS NOT DISTINCT FROM $3::timestamptz AND
                         issued IS NOT DISTINCT FROM $4::timestamptz AND
                         source IS NOT DISTINCT FROM $5::text AS is_identical
                       FROM spatial.rasters_reference
                       WHERE valid_from = $1
                         AND raster_series_id = $2
                         AND flag IS NULL
                       ORDER BY reference_id
                       LIMIT 1",
                      params = list(
                        valid_from,
                        id,
                        valid_to,
                        if (is.null(issued)) {
                          as.POSIXct(NA, tz = "UTC")
                        } else {
                          issued
                        },
                        if (is.null(source)) NA_character_ else source
                      )
                    )
                    if (nrow(existing_final) > 0L) {
                      # Never replace a final raster with a preliminary one.
                      # Identical final rasters are also idempotent no-ops.
                      if (
                        !is.na(flag) || isTRUE(existing_final$is_identical[1])
                      ) {
                        DBI::dbExecute(con, "ROLLBACK")
                        message("Already present; skipped")
                        next
                      }

                      DBI::dbExecute(
                        con,
                        "DELETE FROM spatial.rasters_reference
                         WHERE reference_id = $1",
                        params = list(existing_final$reference_id[1])
                      ) # This will cascade to the rasters table
                    }
                  } # else continue along and insert the new raster
                  suppressMessages(insertACModelRaster(
                    raster = rast,
                    raster_series_id = id,
                    valid_from = valid_from,
                    valid_to = valid_to,
                    issued = issued,
                    flag = flag,
                    source = source,
                    units = units,
                    model = model,
                    con = con
                  ))
                  raster_count <- raster_count + 1
                  series_raster_count <- series_raster_count + 1L

                  # On success, commit the transaction
                  DBI::dbExecute(con, "COMMIT")
                  message("Success")
                },
                error = function(e) {
                  append_error <- data.table::data.table(
                    raster_series_id = as.integer(id),
                    raster_index = as.integer(j),
                    rasters_returned = as.integer(length(rasters)),
                    valid_from = as.POSIXct(
                      valid_from,
                      origin = "1970-01-01",
                      tz = "UTC"
                    ),
                    valid_to = as.POSIXct(
                      valid_to,
                      origin = "1970-01-01",
                      tz = "UTC"
                    ),
                    source = if (is.null(source) || length(source) == 0L) {
                      NA_character_
                    } else {
                      as.character(source[[1L]])
                    },
                    error = conditionMessage(e)
                  )
                  append_errors[[length(append_errors) + 1L]] <<- append_error
                  append_failed <<- TRUE

                  message(
                    "getNewRasters: Failed to append raster ",
                    j,
                    " of ",
                    length(rasters),
                    " for raster_series_id ",
                    id,
                    " (valid_from ",
                    format(valid_from, tz = "UTC", usetz = TRUE),
                    ", valid_to ",
                    format(valid_to, tz = "UTC", usetz = TRUE),
                    ", source ",
                    append_error$source,
                    "): ",
                    append_error$error
                  )
                  # On error, rollback the transaction
                  tryCatch(
                    DBI::dbExecute(con, "ROLLBACK"),
                    error = function(rollback_error) {
                      message(
                        "getNewRasters: Rollback after the failed raster append ",
                        "also failed: ",
                        conditionMessage(rollback_error)
                      )
                    }
                  )
                }
              )
              if (append_failed && type == "reanalysis") {
                message(
                  "getNewRasters: Stopping raster_series_id ",
                  id,
                  " after an append failure so its reanalysis timeline ",
                  "cannot advance past missing data."
                )
                break
              }
            }

            if (forecast && series_raster_count > 0) {
              # Delete old forecast rasters as requested. Database triggers
              # recompute the series bounds and last_issue after each delete.
              valid_from <- as.POSIXct(
                sapply(rasters, function(x) {
                  if (is.null(x$valid_from)) NA else x$valid_from
                }),
                tz = "UTC"
              )
              valid_from <- valid_from[!is.na(valid_from)]
              if (keep_forecasts == 'selective') {
                message(
                  "Deleting old forecasts for raster_series_id ",
                  id,
                  " for rasters with valid_from in '",
                  paste(valid_from, collapse = "', '"),
                  "' and issued within 5 seconds of '",
                  issued_datetime,
                  "'"
                )
                # Find the reference_id in table rasters_reference corresponding to valid_from times in the newly appended rasters and delete the old ones
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM spatial.rasters_reference WHERE raster_series_id = ",
                    id,
                    " AND valid_from IN ('",
                    paste(valid_from, collapse = "', '"),
                    "') AND issued NOT BETWEEN '",
                    issued - 5,
                    "' AND '",
                    issued + 5,
                    "';"
                  )
                )
              } else if (keep_forecasts == 'none') {
                message(
                  "Deleting all old forecasts for raster_series_id ",
                  id,
                  " with issued within 5 seconds of '",
                  issued_datetime,
                  "'"
                )
                # Delete all rasters from this series
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM spatial.rasters_reference WHERE raster_series_id = ",
                    id,
                    " AND issued NOT BETWEEN '",
                    issued - 5,
                    "' AND '",
                    issued + 5,
                    "';"
                  )
                )
              } # else keep_forecasts == 'all', so delete nothing
            }

            if (series_raster_count > 0) {
              count <- count + 1
              success <- c(success, id)
            } else {
              message(
                "getNewRasters: No rasters were appended for raster_series_id ",
                id,
                "."
              )
            }
          } else {
            message(
              "getNewRasters: No new rasters found for raster_series_id ",
              id,
              "."
            )
          }
        },
        warning = function(w) {
          message(
            "getNewRasters: Warning while processing raster_series_id ",
            id,
            ": ",
            conditionMessage(w)
          )
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        series_error <<- conditionMessage(e)
      },
      finally = {
        advisory_lock_release(con, lock_namespace, id)
      }
    )

    make_series_result()
  }

  # Keep worker functions and their data separate from the preflight connection.
  worker_env <- new.env(parent = asNamespace("AquaCache"))
  worker_env$meta_ids <- meta_ids
  worker_env$keep_forecasts <- keep_forecasts
  worker_env$start_datetime <- start_datetime
  worker_env$end_datetime <- end_datetime
  environment(process_raster_series) <- worker_env
  run_env <- new.env(parent = worker_env)
  run_env$process_raster_series <- process_raster_series

  run_raster_series <- function(i, con) {
    tryCatch(
      process_raster_series(i, con),
      error = function(e) {
        list(
          raster_series_id = as.character(meta_ids$raster_series_id[[i]]),
          count = 0L,
          success = character(0),
          raster_count = 0L,
          append_errors = list(),
          locked = FALSE,
          error = conditionMessage(e)
        )
      }
    )
  }
  environment(run_raster_series) <- run_env

  if (parallel && nrow(meta_ids) > 1L) {
    n_cores <- parallel::detectCores() - 2L
    if (is.na(n_cores) || n_cores < 1L) {
      n_cores <- 1L
      warning(
        "getNewRasters: Parallel mode requested but only 2 or fewer CPU cores were detected. Running with one worker."
      )
    }
    n_cores <- min(n_cores, nrow(meta_ids))
    message(
      "Iterating through ",
      nrow(meta_ids),
      " raster series in parallel with ",
      n_cores,
      " workers."
    )

    cl <- parallel::makeCluster(n_cores)
    on.exit(
      {
        try(
          parallel::clusterEvalQ(cl, {
            worker_connection_name <- ".aquacache_getNewRasters_con"
            if (exists(
              worker_connection_name,
              envir = .GlobalEnv,
              inherits = FALSE
            )) {
              worker_con <- get(
                worker_connection_name,
                envir = .GlobalEnv,
                inherits = FALSE
              )
              if (DBI::dbIsValid(worker_con)) {
                DBI::dbDisconnect(worker_con)
              }
              rm(list = worker_connection_name, envir = .GlobalEnv)
            }
          }),
          silent = TRUE
        )
        parallel::stopCluster(cl)
      },
      add = TRUE
    )
    parallel::clusterExport(
      cl,
      "run_raster_series",
      envir = environment()
    )
    doSNOW::registerDoSNOW(cl)
    if (interactive()) {
      progress <- function(n) {
        utils::setTxtProgressBar(
          pb,
          min(as.integer(n), nrow(meta_ids))
        )
      }
    }
    opts <- if (interactive()) list(progress = progress) else list()
    `%dopar%` <- foreach::`%dopar%`
    worker_results <- foreach::foreach(
      i = seq_len(nrow(meta_ids)),
      .packages = c("DBI", "AquaCache", "data.table", "jsonlite"),
      .options.snow = opts,
      .combine = "c",
      .multicombine = TRUE,
      .errorhandling = "pass"
    ) %dopar% {
      task_result <- tryCatch(
        {
          worker_connection_name <- ".aquacache_getNewRasters_con"
          if (exists(
            worker_connection_name,
            envir = .GlobalEnv,
            inherits = FALSE
          )) {
            worker_con <- get(
              worker_connection_name,
              envir = .GlobalEnv,
              inherits = FALSE
            )
          } else {
            worker_con <- AquaConnect(silent = TRUE, check = FALSE)
            assign(worker_connection_name, worker_con, envir = .GlobalEnv)
            DBI::dbExecute(worker_con, "SET timezone = 'UTC'")
          }
          run_raster_series(i, con = worker_con)
        },
        error = function(e) {
          list(
            raster_series_id = as.character(meta_ids$raster_series_id[[i]]),
            count = 0L,
            success = character(0),
            raster_count = 0L,
            append_errors = list(),
            locked = FALSE,
            error = conditionMessage(e)
          )
        }
      )
      list(task_result)
    }
  } else {
    if (identical(parallel_disabled_reason, "missing_packages")) {
      message(
        "Parallel packages 'foreach' and 'doSNOW' are not available; running in sequence."
      )
    } else if (identical(parallel_disabled_reason, "connection_provided")) {
      message("A connection was supplied; raster series will be processed in sequence.")
    }
    worker_results <- vector("list", nrow(meta_ids))
    for (i in seq_len(nrow(meta_ids))) {
      worker_results[[i]] <- run_raster_series(i, con = con)
      if (interactive()) {
        utils::setTxtProgressBar(pb, i)
      }
    }
  }

  if (interactive()) {
    close(pb)
  }

  locked <- vapply(worker_results, function(result) result$locked, logical(1))
  if (any(locked)) {
    for (raster_series_id in vapply(
      worker_results[locked],
      function(result) result$raster_series_id,
      character(1)
    )) {
      warning(
        "getNewRasters: Skipping raster_series_id ",
        raster_series_id,
        " because it is locked by another process."
      )
    }
  }

  failed <- vapply(
    worker_results,
    function(result) !is.null(result$error),
    logical(1)
  )
  if (any(failed)) {
    for (index in which(failed)) {
      warning(
        "getNewRasters: Failed to get new rasters or to append new rasters for raster_series_id ",
        worker_results[[index]]$raster_series_id,
        " with error message: ",
        worker_results[[index]]$error
      )
    }
  }

  count <- sum(vapply(worker_results, function(result) result$count, integer(1)))
  raster_count <- sum(vapply(
    worker_results,
    function(result) result$raster_count,
    integer(1)
  ))
  success <- unlist(lapply(worker_results, function(result) result$success), use.names = FALSE)
  append_errors <- unlist(
    lapply(worker_results, function(result) result$append_errors),
    recursive = FALSE
  )

  message(
    count,
    " of ",
    nrow(meta_ids),
    " raster_series_id's were updated."
  )
  message(raster_count, " rasters were added in total.")

  try(
    # In a try in case the user doesn't have update permissions on internal_status
    {
      DBI::dbExecute(
        con,
        "UPDATE information.internal_status SET value = NOW() WHERE event = 'last_new_rasters';"
      )
    },
    silent = TRUE
  )

  if (length(append_errors) > 0L) {
    append_errors <- data.table::rbindlist(
      append_errors,
      use.names = TRUE
    )
    attr(success, "append_errors") <- append_errors

    error_details <- paste0(
      "  - raster_series_id ",
      append_errors$raster_series_id,
      ", raster ",
      append_errors$raster_index,
      " of ",
      append_errors$rasters_returned,
      ", valid_from ",
      format(append_errors$valid_from, tz = "UTC", usetz = TRUE),
      ", valid_to ",
      format(append_errors$valid_to, tz = "UTC", usetz = TRUE),
      ", source ",
      append_errors$source,
      ": ",
      append_errors$error
    )
    warning(
      "getNewRasters: ",
      nrow(append_errors),
      " raster append",
      if (nrow(append_errors) == 1L) "" else "s",
      " failed. Forecast retention/deletion followed the requested ",
      "keep_forecasts policy. Failure details:\n",
      paste(error_details, collapse = "\n"),
      call. = FALSE
    )
  }

  return(success)
}
