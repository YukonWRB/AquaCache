#' Synchronize hydro DB with remote sources
#'
#' @description
#'
#' This synchronize function pulls and replaces data referenced in table 'sample_series' if and when a discrepancy is observed between the remote repository and the local data store, with the remote taking precedence.
#'
#' @details
#' Deleting sample data found in AquaCache but not in the remote sources is done with the following logic: each sample_series_id is checked for any data found on the remote using the source_fx and the synch_from and synch_to datetimes assigned in table 'sample_series'. Any samples not found in the remote are deleted from the local database if the 'import_source' of the new and existing samples match. If the 'import_source' of the new and existing samples do not match, the sample is not deleted.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param sample_series_id The sample_series_id you wish to have updated, as character or numeric vector. Defaults to "all".
#' @param start_datetime The datetime (as a POSIXct, Date, or character) from which to look for possible new data. You can specify a single start_datetime to apply to all `sample_series_id`, or one per element of `sample_series_id`
#' @param active Sets behavior for checking sample_series_ids or not. If set to 'default', the function will look to the column 'active' in the 'sample_series_id' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and check all sample_series_id
#' @param delete If TRUE, the function will delete any samples and/or results that are not found in the remote source IF these samples are labelled in column 'import_source' as having the same import source. If FALSE, the function will not delete any data. See details for more info.
#' @param snowCon A connection to the snow course database, created with [snowConnect()]. NULL will create a connection using the same connection host and port as the 'con' connection object and close it afterwards. Not used if no data is pulled from the snow database.
#' @param EQCon A connection to the EQWin database, created with [EQConnect()]. NULL will create a connection and close it afterwards. Not used if no data is pulled from the EQWin database.
#'
#' @return Updated entries in the hydro database.
#' @export
#'

synchronize_discrete <- function(
  con = NULL,
  sample_series_id = "all",
  start_datetime,
  active = 'default',
  delete = FALSE,
  snowCon = NULL,
  EQCon = NULL
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

  start <- Sys.time()

  message("Synchronizing sample series with synchronize_discrete...")

  #Check length of start_datetime is either 1 of same as sample_series_id
  if (length(start_datetime) != 1) {
    if (length(start_datetime) != length(sample_series_id)) {
      stop(
        "There is not exactly one element to start_datetime per valid sample_series_id specified by you in the database. Either you're missing elements to start_datetime or you are looking for sample_series_id that doesn't exist."
      )
    }
  } else {
    sample_series_id <- unique(sample_series_id)
  }

  if (sample_series_id[1] == "all") {
    all_series <- DBI::dbGetQuery(con, "SELECT * FROM sample_series;")
  } else {
    all_series <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM sample_series WHERE sample_series_id IN (",
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

  valid_sample_names <- DBI::dbGetQuery(
    con,
    "SELECT column_name FROM information_schema.columns WHERE table_schema = 'discrete' AND table_name = 'samples';"
  )[, 1]
  valid_result_names <- DBI::dbGetQuery(
    con,
    "SELECT column_name FROM information_schema.columns WHERE table_schema = 'discrete' AND table_name = 'results';"
  )[, 1]

  # Define a function to commit the data to the database, used later for each sample
  commit_fx <- function(con, sample, results) {
    # Insert the sample data
    DBI::dbAppendTable(con, "samples", sample)

    # Get the sample_id using all fields that define a unique sample
    sample_id <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT sample_id FROM samples WHERE location_id = ",
        sample$location_id,
        " AND datetime = '",
        sample$datetime,
        " UTC'",
        " AND media_id = ",
        sample$media_id,
        " AND sample_type = ",
        sample$sample_type,
        " AND collection_method = ",
        sample$collection_method,
        ifelse(
          is.null(sample$sub_location_id) || is.na(sample$sub_location_id),
          " AND sub_location_id IS NULL",
          paste0(" AND sub_location_id = ", sample$sub_location_id)
        ),
        ifelse(
          is.null(sample$z) || is.na(sample$z),
          " AND z IS NULL",
          paste0(" AND z = ", sample$z)
        ),
        " AND import_source = '",
        sample$import_source,
        "';"
      )
    )[1, 1]

    # Insert the results data
    results$sample_id <- sample_id
    DBI::dbAppendTable(con, "results", results)

    return(sample_id)
  }

  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_series), style = 3)
  }

  new_samples <- 0 # Counter for number of newly created sample series
  updated_samples <- 0 # Counter for number of updated sample series
  updated_results <- 0 # Counter for number of updated results
  new_results <- 0 # Counter for number of new results

  # Start of for loop ########################################################
  for (i in 1:nrow(all_series)) {
    sid <- all_series$sample_series_id[i]
    loc_id <- all_series$location_id[i]
    sub_loc_id <- all_series$sub_location_id[i]
    synch_from <- all_series$synch_from[i]
    synch_to <- all_series$synch_to[i]
    source_fx <- all_series$source_fx[i]
    source_fx_args <- all_series$source_fx_args[i]
    default_owner <- all_series$default_owner[i]
    default_contributor <- all_series$default_contributor[i]

    # start/end datetime for the sample series
    start_i <- if (!is.na(synch_from)) {
      min(start_datetime, synch_from)
    } else {
      start_datetime
    }
    end_i <- if (!is.na(synch_to)) synch_to else Sys.time()

    # both functions downloadEQWin and downloadSnowCourse can establish their own connections, but this is repetitive and inefficient. Instead, we make the connection once and pass the connection to the function.
    if (source_fx == "downloadEQWin" & is.null(EQCon)) {
      EQCon <- EQConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(EQCon), add = TRUE)
    }

    if (source_fx == "downloadSnowCourse" & is.null(snowCon)) {
      # Try with the same host and port as the AquaCache connection
      dets <- DBI::dbGetQuery(
        con,
        "SELECT inet_server_addr() AS ip, inet_server_port() AS port"
      )
      snowCon <- snowConnect(host = dets$ip, port = dets$port, silent = TRUE)
      on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
    }

    tryCatch(
      {
        args_list <- list(
          start_datetime = start_i,
          end_datetime = end_i,
          con = con
        )
        if (!is.na(source_fx_args)) {
          #add some arguments if they are specified
          args <- jsonlite::fromJSON(source_fx_args)
          args_list <- c(args_list, lapply(args, as.character))
        }

        if (source_fx == "downloadEQWin") {
          args_list[["EQCon"]] <- EQCon
        }
        if (source_fx == "downloadSnowCourse") {
          args_list[["snowCon"]] <- snowCon
        }

        inRemote <- do.call(source_fx, args_list) #Get the data using the args_list

        if (length(inRemote) == 0) {
          # There was no data in remote for the date range specified
          DBI::dbExecute(
            con,
            paste0(
              "UPDATE sample_series SET last_synchronize = '",
              .POSIXct(Sys.time(), "UTC"),
              "' WHERE sample_series_id = ",
              sid,
              ";"
            )
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
            # Extract the 'datetime' of each sample in the list (make a blank element if it's not found)
            inRemote_datetimes <- lapply(inRemote, function(x) {
              if ("sample" %in% names(x)) x$sample$datetime else NA
            })
          }

          for (j in 1:length(inRemote)) {
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
            names_inRemote_samp <- names(inRemote_sample)
            names_inRemote_res <- names(inRemote_results)

            if (delete) {
              if (j == 1) {
                # Delete any samples between the start of the series and the first sample in the remote data, if any. Cascades to results.
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM samples WHERE datetime > '",
                    start_i,
                    "' AND datetime < '",
                    inRemote_datetimes[[j]],
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
              } else if (j == length(inRemote)) {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM samples WHERE datetime < '",
                    end_i,
                    "' AND datetime > '",
                    inRemote_datetimes[[j]],
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
              } else {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM samples WHERE datetime BETWEEN '",
                    inRemote_datetimes[[j - 1]] + 1,
                    "' AND '",
                    inRemote_datetimes[[j]] - 1,
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

            if (nrow(inRemote_results) == 0) {
              next
            }

            inDB_sample <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT * FROM samples WHERE datetime = '",
                inRemote_sample$datetime,
                "' AND location_id = ",
                loc_id,
                " AND sub_location_id ",
                if (!is.na(sub_loc_id)) paste0("= ", sub_loc_id) else "IS NULL",
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
                ";"
              )
            )

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
              updated_samples_flag <- FALSE
              for (k in intersect(names_inRemote_samp, valid_sample_names)) {
                inDB_k <- inDB_sample[[k]]
                inRemote_k <- inRemote_sample[[k]]
                # Attempt numeric conversion where possible
                if (!is.numeric(inDB_k)) {
                  conv <- suppressWarnings(as.numeric(inDB_k))
                  if (!is.na(conv)) inDB_k <- conv
                }
                if (!is.numeric(inRemote_k)) {
                  conv <- suppressWarnings(as.numeric(inRemote_k))
                  if (!is.na(conv)) inRemote_k <- conv
                }
                if (isFALSE(all.equal(inDB_k, inRemote_k))) {
                  # If TRUE, update the DB
                  to_insert <- if (!is.na(inRemote_k)) inRemote_k else NULL
                  if (is.null(to_insert)) {
                    DBI::dbExecute(
                      con,
                      paste0(
                        "UPDATE samples SET ",
                        k,
                        " = NULL WHERE sample_id = ",
                        inDB_sample$sample_id,
                        ";"
                      )
                    )
                  } else {
                    DBI::dbExecute(
                      con,
                      paste0(
                        "UPDATE samples SET ",
                        k,
                        " = '",
                        to_insert,
                        "' WHERE sample_id = ",
                        inDB_sample$sample_id,
                        ";"
                      )
                    )
                  }
                  updated_samples_flag <- TRUE
                }
              }
              if (updated_samples_flag) {
                updated_samples <- updated_samples + 1
              }

              # Get the results for the sample
              inDB_results <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT * FROM results WHERE sample_id = ",
                  inDB_sample$sample_id,
                  ";"
                )
              )

              inDB_results$checked <- FALSE # This will be used to track which rows have been checked

              for (k in 1:nrow(inRemote_results)) {
                sub <- inRemote_results[k, ]
                names_inRemote_sub <- names(sub)
                # Sort out if there's an equivalent row in inDB_result. There could be new results! Results are unique on result_type, parameter_id, sample_fraction_id, result_value_type, result_speciation_id, protocol_method, laboratory, analysis_datetime, but not all columns might be populated in 'sub'

                idx <- inDB_results$result_type == sub$result_type &
                  inDB_results$parameter_id == sub$parameter_id
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
                      "SELECT parameter_id, result_speciation AS result_speciation_bool FROM parameters WHERE parameter_id = ",
                      sub$parameter_id,
                      ");"
                    )
                  )
                  sample_fraction <- DBI::dbGetQuery(
                    con,
                    paste0(
                      "SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM parameters WHERE parameter_id = ",
                      sub$parameter_id,
                      ");"
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
                  DBI::dbAppendTable(con, "results", sub)

                  new_results <- new_results + 1
                } else if (nrow(inDB_sub) == 1) {
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
                      if (isFALSE(all.equal(inDB_l, sub_l))) {
                        to_insert <- if (!is.na(sub_l)) sub_l else "NULL"
                        message(
                          "Found discrepancy in ",
                          l,
                          " for sample_series_id ",
                          sid,
                          " and sample ",
                          j,
                          " and sample row ",
                          k,
                          ". Updating the database."
                        )
                        DBI::dbExecute(
                          con,
                          paste0(
                            "UPDATE results SET ",
                            l,
                            " = '",
                            to_insert,
                            "' WHERE result_id = ",
                            inDB_sub$result_id,
                            ";"
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
                      "DELETE FROM results WHERE result_id IN (",
                      paste(to_delete, collapse = ", "),
                      ");"
                    )
                  )
                }
              }

              # Inserting a new sample #########
            } else {
              # No database sample was found, add the sample and corresponding results (follow same process as getNewDiscrete)
              ## Checks on sample metadata ###########
              # Functions may pass the location code instead of location_id, change it
              if ("location" %in% names_inRemote_samp) {
                inRemote_sample$location_id <- loc_id
                inRemote_sample$location <- NULL
                names_inRemote_samp <- names(inRemote_sample)
              }
              if ("sub_location" %in% names_inRemote_samp) {
                inRemote_sample$sub_location_id <- sub_loc_id
                inRemote_sample$sub_location <- NULL
                names_inRemote_samp <- names(inRemote_sample)
              }

              # Check that the sample data has the required columns at minimum: c("location_id", "media_id", "datetime", "collection_method", "sample_type", "owner", "import_source_id"). Note that import_source_id is only mandatory because this function pulls data in from a remote source
              mandatory_samp <- c(
                "location_id",
                "media_id",
                "datetime",
                "collection_method",
                "sample_type",
                "owner",
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

              inRemote_sample$import_source <- source_fx

              # Apply default owner/contributor if not provided
              if (
                !("owner" %in% names_inRemote_samp) ||
                  is.na(inRemote_sample$owner)
              ) {
                inRemote_sample$owner <- default_owner
              }
              if (
                !("contributor" %in% names_inRemote_samp) ||
                  is.na(inRemote_sample$contributor)
              ) {
                if (!is.na(default_contributor)) {
                  inRemote_sample$contributor <- default_contributor
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
                  for (l in 1:nrow(sub.results)) {
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
                  "SELECT parameter_id, result_speciation AS result_speciation_bool FROM parameters WHERE parameter_id IN (",
                  paste(unique(inRemote_results$parameter_id), collapse = ", "),
                  ");"
                )
              )
              sample_fraction <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT parameter_id, sample_fraction AS sample_fraction_bool FROM parameters WHERE parameter_id IN (",
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

              # Append values in a transaction block ##########
              activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
              if (activeTrans) {
                tryCatch(
                  {
                    commit_fx(con, inRemote_sample, inRemote_results)
                    DBI::dbExecute(con, "COMMIT;")
                  },
                  error = function(e) {
                    DBI::dbExecute(con, "ROLLBACK;")
                    warning(
                      "synchronize_discrete: Failed to commit new data for sample_series_id ",
                      sid,
                      " and list element ",
                      j,
                      " . Error message: ",
                      e$message
                    )
                  }
                )
              } else {
                # we're already in a transaction
                commit_fx(con, inRemote_sample, inRemote_results)
              }
              new_samples <- new_samples + 1
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
      }
    ) # End of tryCatch

    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
  } # End of for loop

  if (interactive()) {
    close(pb)
  }

  DBI::dbExecute(
    con,
    paste0(
      "UPDATE internal_status SET value = '",
      .POSIXct(Sys.time(), "UTC"),
      "' WHERE event = 'last_synchronize_discrete';"
    )
  )
  message(
    "Found ",
    new_samples,
    " new samples to add at the ",
    nrow(all_series),
    " sample_series provided."
  )
  message(
    "Found ",
    updated_samples,
    " samples to update at the ",
    nrow(all_series),
    " sample_series provided."
  )
  message(
    "Found ",
    new_results,
    " new results to add at the ",
    nrow(all_series),
    " sample_series provided."
  )
  message(
    "Found ",
    updated_results,
    " results to update at the ",
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
} #End of function
