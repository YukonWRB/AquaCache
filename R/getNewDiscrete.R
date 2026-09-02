#' Get new discrete-category data
#'
#' @description
#'
#' Retrieves new discrete data starting from the last local sample. The active
#' assignment with the lowest fetch priority in
#' `discrete.sample_series_source_adapters` supplies each series' source
#' function and arguments. Each series may also have a configured time range.
#' Every source function must have an enabled discrete-domain entry in
#' `public.source_adapter_capabilities`.
#'
#' ## Making functions called by getNewDiscrete:
#' An eligible source-adapter assignment specifies the function called to get
#' new data and, optionally, its arguments. Source functions must return
#' a list of lists, each containing data frames named `sample` and `results`.
#' The `sample` data frame must contain the following columns:
#' - 'location_id': a numeric location ID. It may be `NA` only for a sample
#'   type whose `requires_location` flag is false. If the column is omitted,
#'   the sample-series location is used.
#' - 'media_id': a numeric specifying the media_id of the data point from table 'medias'.
#' - 'datetime': a POSIXct datetime object in UTC 0 time zone, specifying the datetime of the data point.
#' - 'collection_method': a numeric specifying the collection_method_id of the data point from table 'collection_methods', such as 1 (observation), 27 (water bottle), or 14 (pump).
#' - 'sample_type': a numeric specifying the sample_type_id of the data point from table 'sample_types', such as 1 (grab), 2 (composite), or 3 (integrated).
#' - 'owner': the numeric organization ID that owns the sample. If omitted, the
#'   sample-series default owner is used.
#' - 'import_source_id': a non-missing source-specific identifier used to
#'   match the sample across runs. Together with the registered source
#'   function, it is the database-enforced identity for a locationless sample.
#' Optional columns are:
#' - 'target_datetime': a POSIXct datetime object in UTC 0 time zone, specifying an artificial datetime for the data point which can be used for data analysis or plotting purposes.
#' - 'note': a character string with a note about the data point(s).
#' - 'contributor': the numeric contributing organization ID. If omitted, the
#'   sample-series default contributor is used.
#' - 'approval': the approval status of the data, as a character string. This should match entries in the 'approvals' table and an error will be thrown if it does not.
#' - 'grade': the grade of the data, as a character string. This should match entries in the 'grades' table and an error will be thrown if it does not.
#' - 'qualifier': the qualifier of the data, as a character string. This should match entries in the 'qualifiers' table and an error will be thrown if it does not.
#'
#' The 'results' data.frame should contain one row per result and must contain the following columns:
#' - 'parameter_id': a numeric specifying the parameter_id of the data point from table 'parameters'.
#' - 'result': a numeric specifying the sample's results, matched to the parameters
#' - 'result_type': a numeric specifying the result_type_id of the data point from table 'result_types', such as 1 (concentration), 2 (load), or 3 (other).
#' Additionally, the following columns may need to be included:
#' - 'result_condition': a numeric specifying the result condition of the data point from table 'result_conditions', such as "< DL" or "> DL". Only necessary if there are NA values in the 'result' column that should be interpreted as a specific condition. If not provided, rows with NA values will be dropped.
#' - 'result_condition_value': a numeric specifying the value of the result condition, such as 0.1 for "< DL 0.1". Necessary if column 'result_condition' is provided AND contains values of 1 or 2, i.e. 'Below Detection/Quantification Limit' or 'Above Detection/Quantification Limit'.
#' - 'matrix_state_id' or 'matrix_state': an optional numeric id or text code/name specifying the physical matrix state of the analyzed result from table 'matrix_states'. If omitted, the database defaults it from the parent sample media.
#' - 'sample_fraction_id': a numeric specifying the sample_fraction_id of the data point from table 'sample_fractions', such as 19 ('total'), 5 ('dissolved'), or 18 ('suspended'). Required if the column 'sample_fraction' in table 'parameters' is TRUE for the parameter in question.
#' - 'result_speciation_id': a numeric specifying the result_speciation_id of the data point from table 'result_speciations', such as 3 (as CaCO3), 5 (as CN), or 44 (of S). Required if the column 'result_speciation' in table 'parameters' is TRUE for the parameter in question.
#'
#' Each returned sample list may also contain `sample_groups`. It can be a
#' vector of existing `sample_group_id` values or a data frame in the format
#' accepted by [addNewDiscrete()]. Source adapters should return the same
#' `group_type`, `owner`, and `group_code` for related samples; the group will
#' be created once and reused. A locationless sample, or a sample type whose
#' `requires_sample_group` flag is true, must provide at least one group.
#' The optional `sample_qualifiers`, `sample_observers`, `result_aggregations`,
#' and `result_components` elements use the formats documented by
#' [addNewDiscrete()].
#'
#' `getNewDiscrete()` is insertion-only. If a source record has already been
#' imported, the function reports that sample with `action = "existing"` but
#' does not update its sample row, group memberships, qualifiers, observers,
#' results, aggregation configuration, or components. Use
#' [synchronize_discrete()] when existing source records must be reconciled.
#'
#' Additionally, functions must be able to handle the case where no new data is available and return an empty list.
#' If you are a developer, note that download or source functions MUST be registered in AquaCache using function [registerSourceAdapterArguments()], and that this operation would normally be completed using the 'patch' system. See patch_56.R for examples.
#'
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param location_id The location_ids you wish to have updated, as character or numeric vector. Defaults to NULL which will fetch data from all location_ids in the 'sample_series' table for all corresponding time ranges using the associated source functions (if more than one per location).
#' @param sub_location_id The sub_location_ids you wish to have updated, as character or numeric vector. Defaults to NULL which will fetch data from all sub_location_ids in the 'sample_series' table for all corresponding time ranges using the associated source functions (if more than one per location).
#' @param sample_series_id The sample_series_ids you wish to have updated, as character or numeric vector. Defaults to NULL, giving precedence to 'location_id'. This can be useful when wanting to synch all time ranges for a location that may have different sample_series_ids.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'sample_series' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#' @param snowCon A connection to the snow course database, created with [snowConnect()]. NULL will create a connection using the same connection host and port as the 'con' connection object and close it afterwards. Not used if no data is pulled from the snow database.
#'
#' @return A data.table with one row per inserted or previously imported sample
#'   and columns
#'   `sample_series_id`, `sample_id`, `action`, and list-columns containing the
#'   normalized source inputs. An empty result has the same columns.
#' @export

getNewDiscrete <- function(
  con = NULL,
  location_id = NULL,
  sub_location_id = NULL,
  sample_series_id = NULL,
  active = 'default',
  snowCon = NULL
) {
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }

  # Make sure that location_id and sample_series_id are not both specified
  if (!is.null(location_id) & !is.null(sample_series_id)) {
    stop("location_id and sample_series_id cannot both be specified (not NULL)")
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
       source.fetch_priority
     FROM discrete.sample_series ss
     LEFT JOIN LATERAL (
       SELECT
         ssa.sample_series_source_adapter_id,
         ssa.source_fx,
         ssa.source_fx_args,
         ssa.fetch_priority
       FROM discrete.sample_series_source_adapters ssa
       WHERE ssa.sample_series_id = ss.sample_series_id
         AND ssa.active
         AND ssa.fetch_priority IS NOT NULL
       ORDER BY ssa.fetch_priority, ssa.sample_series_source_adapter_id
       LIMIT 1
     ) source ON TRUE"

  if (is.null(location_id)) {
    if (is.null(sample_series_id)) {
      all_series <- DBI::dbGetQuery(
        con,
        paste0(
          series_select_sql,
          " WHERE (ss.synch_to IS NULL OR ss.synch_to >= now())"
        )
      )
    } else {
      all_series <- DBI::dbGetQuery(
        con,
        paste0(
          series_select_sql,
          " WHERE ss.sample_series_id IN (",
          paste(sample_series_id, collapse = ", "),
          ") AND (ss.synch_to IS NULL OR ss.synch_to >= now())"
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
  } else {
    if (is.null(sub_location_id)) {
      all_series <- DBI::dbGetQuery(
        con,
        paste0(
          series_select_sql,
          " WHERE ss.location_id IN (",
          paste(location_id, collapse = ", "),
          ") AND (ss.synch_to IS NULL OR ss.synch_to >= now())"
        )
      )
    } else {
      all_series <- DBI::dbGetQuery(
        con,
        paste0(
          series_select_sql,
          " WHERE ss.location_id IN (",
          paste(location_id, collapse = ", "),
          ") AND ss.sub_location_id IN (",
          paste(sub_location_id, collapse = ", "),
          ") AND (ss.synch_to IS NULL OR ss.synch_to >= now())"
        )
      )
    }
    if (length(unique(location_id)) != nrow(all_series)) {
      fail <- location_id[!location_id %in% all_series$location_id]
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
  if (nrow(all_series) == 0) {
    stop(
      "Could not find any active sample series matching your input parameters."
    )
  }

  missing_source <- is.na(all_series$source_fx)
  if (any(missing_source)) {
    warning(
      "The following sample series have no active source-adapter assignment ",
      "with a fetch priority and will be ignored: ",
      paste(all_series$sample_series_id[missing_source], collapse = ", "),
      "."
    )
    all_series <- all_series[!missing_source, , drop = FALSE]
  }
  if (nrow(all_series) == 0L) {
    stop(
      "Could not find any sample series with an active source-adapter ",
      "assignment for fetching."
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
      "getNewDiscrete: Every source_fx must have an enabled entry in ",
      "public.source_adapter_capabilities for the discrete domain. ",
      "Missing or disabled: ",
      paste(unregistered_source_fx, collapse = ", "),
      "."
    )
  }

  count <- 0 #counter for number of successful new pulls (samples - not individual results)
  import_records <- list()

  # Run for loop over timeseries rows
  message("Fetching new discrete data with getNewDiscrete...")

  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_series), style = 3)
  }
  for (i in seq_len(nrow(all_series))) {
    sid <- all_series$sample_series_id[i]

    # Acquire a lock for this sample series to prevent concurrent updates, notably by synchronize_discrete
    # IMPORTANT: this lock does not wait if another process has it, it just skips to the next sample series synchronize_discrete **will** wait for the lock to be released, on the other hand.
    lock_namespace <- "aquacache_sample_series"
    lock_acquired <- advisory_lock_acquire(
      con = con,
      namespace = lock_namespace,
      key = sid,
      wait = FALSE
    )
    if (!isTRUE(lock_acquired)) {
      warning(
        "getNewDiscrete: Skipping sample_series_id ",
        sid,
        " because it is locked by another process."
      )
      next
    }

    tryCatch(
      {
        loc_id <- all_series$location_id[i]
        sub_loc_id <- all_series$sub_location_id[i]
        source_fx <- all_series$source_fx[i]
        source_fx_args <- all_series$source_fx_args[i]
        owner <- all_series$default_owner[i]
        contributor <- all_series$default_contributor[i]
        range_start <- all_series$synch_from[i]
        range_end <- all_series$synch_to[i]

        # Find the last data point for this series
        query <- paste0(
          "SELECT MAX(datetime) FROM discrete.samples WHERE location_id = ",
          loc_id,
          " AND import_source = '",
          source_fx,
          "'"
        )
        if (!is.na(sub_loc_id)) {
          query <- paste0(query, " AND sub_location_id = ", sub_loc_id)
        } else {
          query <- paste0(query, " AND sub_location_id IS NULL")
        }
        if (!is.na(range_start)) {
          query <- paste0(
            query,
            " AND datetime >= '",
            as.character(range_start),
            " UTC'"
          )
        }
        if (!is.na(range_end)) {
          query <- paste0(
            query,
            " AND datetime <= '",
            as.character(range_end),
            " UTC'"
          )
        }
        last_data_point <- DBI::dbGetQuery(con, query)[1, 1]
        if (is.na(last_data_point)) {
          # If the series has no existing samples in its configured window,
          # start at synch_from rather than importing older source history.
          last_data_point <- if (!is.na(range_start)) {
            range_start
          } else {
            as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
          }
        } else {
          last_data_point <- last_data_point + 1
        }

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
          con = con,
          start_datetime = last_data_point,
          end_datetime = if (is.na(range_end)) Sys.time() else range_end
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

        ## Get the data ##############
        data <- do.call(source_fx, args_list) # Get the data using the args_list

        if (length(data) == 0) {
          next
        }

        if (!inherits(data, "list")) {
          stop(
            "For sample_series_id ",
            sid,
            " the source function did not return a list."
          )
        } else if (!inherits(data[[1]], "list")) {
          stop(
            "For sample_series_id ",
            sid,
            " the source function did not return a list of lists (one element per sample, with two data.frames: one for sample metadata, the other for associated results)."
          )
        }

        # Work on each list element to populate the 'samples' and 'results' tables
        for (j in seq_along(data)) {
          if (
            !("sample" %in% names(data[[j]])) |
              !("results" %in% names(data[[j]]))
          ) {
            warning(
              "For sample_series_id ",
              sid,
              " the source function did not return a list with elements named 'sample' and 'results'. Failed on list element ",
              j,
              ". Skipping to next element."
            )
            next
          }

          # Make sure that results element has at least one row
          if (nrow(data[[j]][["results"]]) == 0) {
            next
          }

          ## Checks on sample metadata ###########
          # Ensure the sample data has required minimum columns
          sample <- data[[j]][["sample"]]
          if ("sample_qualifier" %in% names(sample)) {
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
          sample_groups <- if ("sample_groups" %in% names(data[[j]])) {
            data[[j]][["sample_groups"]]
          } else {
            NULL
          }
          sample_qualifiers <- data[[j]][["sample_qualifiers"]]
          sample_observers <- data[[j]][["sample_observers"]]
          result_aggregations <- data[[j]][["result_aggregations"]]
          result_components <- data[[j]][["result_components"]]

          # Functions may pass the location code instead of location_id, change it
          # Also possible that the function did not pass 'location_id' at all, if so fill it in using 'loc_id'
          names_samp <- names(sample)
          if ("location" %in% names_samp) {
            sample$location_id <- loc_id
            sample$location <- NULL
            names_samp <- names(sample)
          } else if (!("location_id" %in% names_samp)) {
            sample$location_id <- loc_id
            names_samp <- names(sample)
          }
          if ("sub_location" %in% names_samp) {
            sample$sub_location_id <- sub_loc_id
            sample$sub_location <- NULL
            names_samp <- names(sample)
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
          if (!all(c(mandatory_samp) %in% names_samp)) {
            # Make an error message stating which column is missing
            missing <- c(mandatory_samp)[!c(mandatory_samp) %in% names_samp]
            warning(
              "For sample_series_id ",
              sid,
              " element ",
              j,
              " (sample_datetime ",
              sample$datetime,
              ") the source function did not return one or more mandatory column(s) for the sample metadata: '",
              paste(missing, collapse = "', '"),
              "' Skipping to next sample."
            )
            next
          }

          sample$import_source <- source_fx

          # Apply default owner/contributor if not provided
          if (!("owner" %in% names_samp) || is.na(sample$owner)) {
            sample$owner <- owner
            names_samp <- names(sample)
          }
          if (is.null(sample$owner) || is.na(sample$owner)) {
            warning(
              "For sample_series_id ",
              sid,
              " element ",
              j,
              " (sample_datetime ",
              sample$datetime,
              ") the source function did not provide an owner and there is no default owner for the sample series. Skipping to next sample."
            )
            next
          }
          if (!("contributor" %in% names_samp) || is.na(sample$contributor)) {
            if (!is.na(contributor)) sample$contributor <- contributor
          }

          # Use share_with from the source function when supplied, otherwise it'll fall back to the database default
          if ("share_with" %in% names_samp) {
            if (!is.list(sample$share_with)) {
              sample$share_with <- paste0(
                "{",
                paste(sample$share_with, collapse = ", "),
                "}"
              )
            }
          }

          # Checks on sample results ############
          # Ensure the results have required minimum columns
          results <- data[[j]][["results"]]
          names_res <- names(results)
          # Check that the results have the mandatory columns
          mandatory_res <- c("result", "result_type", "parameter_id")
          if (!all(c(mandatory_res) %in% names_res)) {
            # Make an error message stating which column is missing
            missing <- c(mandatory_res)[!c(mandatory_res) %in% names_res]
            warning(
              "For sample_series_id ",
              sid,
              " element ",
              j,
              " (sample_datetime ",
              sample$datetime,
              ") the source function did not return one or more mandatory column(s) for the results: '",
              paste(missing, collapse = "', '"),
              "'. Skipping to next sample."
            )
            next
          }

          results <- tryCatch(
            {
              normalize_discrete_result_matrix_states(
                con = con,
                sample_media_id = sample$media_id[1],
                results = results
              )
            },
            error = function(e) {
              warning(
                "For sample_series_id ",
                sid,
                " element ",
                j,
                " (sample_datetime ",
                sample$datetime,
                ") the source function returned an invalid matrix_state value: ",
                e$message,
                " Skipping to next sample."
              )
              NULL
            }
          )
          if (is.null(results)) {
            next
          }
          names_res <- names(results)

          # More complex checks if 'result' is NA
          # if there are NAs in the 'result' column, those rows with NAs should have a corresponding entry in the 'result_condition' column.
          aggregated_result_rows <- if (
            !is.null(result_aggregations) &&
              "result_row" %in% names(result_aggregations)
          ) {
            unique(as.integer(result_aggregations$result_row))
          } else {
            integer()
          }
          direct_missing_result <- is.na(results$result) &
            !seq_len(nrow(results)) %in% aggregated_result_rows
          if (any(direct_missing_result)) {
            if (!("result_condition" %in% names_res)) {
              warning(
                "For sample_series_id ",
                sid,
                ", sample ",
                j,
                " (sample_datetime ",
                sample$datetime,
                ") the source function returned NA values in the column 'result' but did not return a column called 'result_condition'. Skipping to next sample."
              )
              next
            } else {
              # Check that each NA in 'result' has a corresponding entry in 'result_condition'
              sub.results <- results[direct_missing_result, ]
              check_result_condition <- FALSE # prevents repeatedly checking for the same thing

              next_flag <- FALSE
              for (k in seq_len(nrow(sub.results))) {
                if (
                  is.na(sub.results$result[k]) &
                    is.na(sub.results$result_condition[k])
                ) {
                  warning(
                    "For sample_series_id ",
                    sid,
                    " element ",
                    j,
                    " (sample_datetime ",
                    sample$datetime,
                    ") the source function returned at least one NA result in the column 'result' but did not return a corresponding entry in the column 'result_condition'. Skipping to next sample."
                  )
                  next_flag <- TRUE
                } else {
                  if (!check_result_condition) {
                    if (any(sub.results$result_condition %in% c(1, 2))) {
                      if (!("result_condition_value" %in% names(results))) {
                        warning(
                          "For sample_series_id ",
                          sid,
                          " element ",
                          j,
                          " (sample_datetime ",
                          sample$datetime,
                          ") the source function returned at least one row where 'result_condition' is 1 or 2 (above/below detetion limit) but there is no column for the necessary result_condition_value. Skipping to next sample."
                        )
                        next_flag <- TRUE
                      }
                    }
                    check_result_condition <- TRUE
                  }

                  if (sub.results$result_condition[k] %in% c(1, 2)) {
                    if (is.na(sub.results$result_condition_value[k])) {
                      warning(
                        "For sample_series_id ",
                        sid,
                        " element ",
                        j,
                        " (sample_datetime ",
                        sample$datetime,
                        ") the source function returned a value of 1 or 2 in the column 'result_condition' (indicating above or below detection limit) but did not return a corresponding entry in the column 'result_condition_value'. Skipping to the next sample."
                      )
                      next_flag <- TRUE
                    }
                  }
                }
              } # End of looping over each row with NA in result column
            }
            if (next_flag) {
              next
            }
          } # End of additional checks fs any NA values in 'result' column are returned

          # Get the result_speciation and sample_fraction boolean values for the parameters. If at least one TRUE then data must contain columns result_speciation_id and sample_fraction_id.
          result_speciation <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT parameter_id, result_speciation AS result_speciation_bool FROM public.parameters WHERE parameter_id IN (",
              paste(unique(results$parameter_id), collapse = ", "),
              ");"
            )
          )
          sample_fraction <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM public.parameters WHERE parameter_id IN (",
              paste(unique(results$parameter_id), collapse = ", "),
              ");"
            )
          )
          if (any(result_speciation$result_speciation_bool)) {
            if (!("result_speciation_id" %in% names_res)) {
              warning(
                "For sample_series_id ",
                sid,
                " element ",
                j,
                " (sample_datetime ",
                sample$datetime,
                ") the source function did not return a column 'result_speciation_id' but the database mandates this for at least one of the parameters. Skipping to next sample."
              )
              next
            } else {
              # Check that values in the result_speciation_id column are not NA where necessary
              merge <- merge(results, result_speciation, by = "parameter_id")
              # For rows where result_speciation_bool is TRUE, check that the corresponding result_speciation_id column is not NA
              chk <- with(
                merge,
                result_speciation_bool & is.na(result_speciation_id)
              )
              if (any(chk)) {
                params <- merge$parameter_id[chk]
                warning(
                  "For sample_series_id ",
                  sid,
                  " element ",
                  j,
                  " (sample_datetime ",
                  sample$datetime,
                  ") the source function returned NA values in the column 'result_speciation_id' for parameter ",
                  paste(params, collapse = ", "),
                  " where the database mandates this value. Skipping to next sample."
                )
                next
              }
            }
          }
          if (any(sample_fraction$sample_fraction_bool)) {
            if (!("sample_fraction_id" %in% names_res)) {
              warning(
                "For sample_series_id ",
                sid,
                " element ",
                j,
                " (sample_datetime ",
                sample$datetime,
                ") the source function did not return a column 'sample_fraction_id' but the database mandates this for at least one of the parameters. Skipping to next sample."
              )
              next
            } else {
              # Check that all values in the sample_fraction_id column are not NA where necessary
              merge <- merge(results, sample_fraction, by = "parameter_id")
              # For rows where sample_fraction_bool is TRUE, check that the corresponding sample_fraction column is not NA
              chk <- with(
                merge,
                sample_fraction_bool & is.na(sample_fraction_id)
              )
              if (any(chk)) {
                warning(
                  "For sample_series_id ",
                  sid,
                  " element ",
                  j,
                  " (sample_datetime ",
                  sample$datetime,
                  ") the source function returned NA values in the column 'sample_fraction_id' for at least one parameter where the database mandates this value. Skipping to next sample."
                )
                next
              }
            }
          }

          # Every adapter record carries a source ID. Patch 58 makes the source
          # pair unique for locationless samples, where location metadata
          # cannot provide a retry key.
          if (
            is.na(sample$import_source_id[[1]]) ||
              !nzchar(trimws(as.character(sample$import_source_id[[1]])))
          ) {
            warning(
              "For sample_series_id ",
              sid,
              " element ",
              j,
              " import_source_id must be non-missing and nonblank. ",
              "Skipping this source record."
            )
            next
          }
          if (is.na(suppressWarnings(as.integer(sample$location_id[[1]])))) {
            existing_sample <- find_locationless_import_sample(
              con = con,
              import_source = source_fx,
              import_source_id = sample$import_source_id
            )
            if (nrow(existing_sample) == 1L) {
              # This importer is deliberately insertion-only. Return the
              # existing identity without reconciling any parent or child data.
              import_records[[length(import_records) + 1L]] <-
                new_discrete_import_record(
                  sample_series_id = sid,
                  sample_id = existing_sample$sample_id[[1]],
                  action = "existing",
                  sample = sample,
                  results = results,
                  sample_groups = sample_groups,
                  sample_qualifiers = sample_qualifiers,
                  sample_observers = sample_observers,
                  result_aggregations = result_aggregations,
                  result_components = result_components
                )
              next
            }
          }

          # Append values
          # Transaction is started each time inside addNewDiscrete
          sample_action <- "inserted"
          sample_id <- tryCatch(
            {
              addNewDiscrete(
                con = con,
                sample = sample,
                results = results,
                sample_groups = sample_groups,
                sample_qualifiers = sample_qualifiers,
                sample_observers = sample_observers,
                result_aggregations = result_aggregations,
                result_components = result_components
              )
            },
            error = function(e) {
              if (
                is.na(suppressWarnings(as.integer(sample$location_id[[1]])))
              ) {
                existing_sample <- find_locationless_import_sample(
                  con = con,
                  import_source = source_fx,
                  import_source_id = sample$import_source_id
                )
                if (nrow(existing_sample) == 1L) {
                  # A concurrent importer won the insert race. Preserve its
                  # stored detail; synchronization owns all update semantics.
                  sample_action <<- "existing"
                  return(existing_sample$sample_id[[1]])
                }
              }
              warning(
                "getNewDiscrete: Failed to commit new data for sample_series_id, ",
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
              count <- count + 1
            }
            import_records[[length(import_records) + 1L]] <-
              new_discrete_import_record(
                sample_series_id = sid,
                sample_id = sample_id,
                action = sample_action,
                sample = sample,
                results = results,
                sample_groups = sample_groups,
                sample_qualifiers = sample_qualifiers,
                sample_observers = sample_observers,
                result_aggregations = result_aggregations,
                result_components = result_components
              )
          }
        } # End of looping over each list element (sample) for a sample_series_id
        DBI::dbExecute(
          con,
          "UPDATE discrete.sample_series SET last_new_data = now() WHERE sample_series_id = $1",
          params = list(sid)
        ) # Update the last new data column
      },
      error = function(e) {
        warning(
          "getNewDiscrete: Failed to get new data or to append new data for sample_series_id ",
          sid,
          ". Error message: ",
          e$message
        )
      },
      finally = {
        # Release the lock
        advisory_lock_release(con, lock_namespace, sid)
      }
    ) #End of tryCatch

    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
  } # End of for loop

  if (interactive()) {
    close(pb)
  }

  message(
    count,
    " samples were found for the ",
    nrow(all_series),
    " sample_series specified."
  )
  try(
    # In a try in case the user doesn't have update permissions on internal_status
    {
      DBI::dbExecute(
        con,
        "UPDATE information.internal_status SET value = NOW() WHERE event = 'last_new_discrete'"
      )
    },
    silent = TRUE
  )
  bind_discrete_import_records(import_records)
}
