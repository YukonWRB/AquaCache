#' Update HYDAT-related timeseries
#'
#' @description
#'
#' First checks the local version of HYDAT using function [hydat_check()] and updates if needed, then checks the local copy against the one last used by the database. If needed or if force_update == TRUE, proceeds to checking each location specified for new data and replaces old data wherever a discrepancy is noted. If all WSC timeseries in the WRB hydro database are in parameter timeseries_id, will also update the internal_status table with the HYDAT version used for the update.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param timeseries_id Character vector of timeseries_ids for which to look for updates. "all" will attempt to update all timeseries where the import function is downloadWSC.
#' @param force_update Set TRUE if you want to force a check of each location against the local copy of HYDAT.
#'
#' @return Updated daily means where HYDAT values were replaced by updated values, and TRUE if a new version of hydat was found.
#' @export
#'

update_hydat <- function(
  con = AquaConnect(silent = TRUE),
  timeseries_id = "all",
  force_update = FALSE
) {
  #Check if the local copy of HYDAT needs an update
  hydat_check(silent = FALSE)
  hydat_path <- tidyhydat::hy_downloaded_db()

  #Check now if the DB should be updated
  local_hydat <- tidyhydat::hy_version(hydat_path)$Date
  DB_hydat <- DBI::dbGetQuery(
    con,
    "SELECT value FROM internal_status WHERE event = 'HYDAT_version'"
  )[1, 1]
  if (!is.na(DB_hydat)) {
    if (DB_hydat != local_hydat) {
      new_hydat <- TRUE
    } else {
      new_hydat <- FALSE
    }
  } else {
    new_hydat <- TRUE
  }
  if (!new_hydat & force_update) {
    message(
      "Local version of HYDAT is current, updating related timeseries because force_update is set to TRUE."
    )
  }

  if (new_hydat | force_update) {
    #Get the required timeseries_ids
    if (timeseries_id[1] == "all") {
      all_timeseries <- DBI::dbGetQuery(
        con,
        "SELECT t.parameter_id, t.timeseries_id, at.aggregation_type FROM timeseries t JOIN aggregation_types AS at ON t.aggregation_type_id = at.aggregation_type_id WHERE source_fx = 'downloadWSC';"
      )
    } else {
      all_timeseries <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT t.parameter_id, t.timeseries_id, at.aggregation_type FROM timeseries t JOIN aggregation_types AS at ON t.aggregation_type_id = at.aggregation_type_id WHERE timeseries_id IN ('",
          paste(timeseries_id, collapse = "', '"),
          "') AND source_fx = 'downloadWSC';"
        )
      )
      if (length(timeseries_id) != nrow(all_timeseries)) {
        fail <- timeseries_id[
          !(timeseries_id %in% all_timeseries$timeseries_id)
        ]
        if ((length(fail) == 1)) {
          warning(
            "update_hydat: Could not find one of the timeseries_ids that you specified: ID ",
            fail,
            " is missing from the database."
          )
        } else {
          warning(
            "update_hydat: Could not find some of the timeseries_ids that you specified: IDs ",
            paste(fail, collapse = ", "),
            " are missing from the database."
          )
        }
      }
    }

    # Now find the value for 'location' in the JSONB timeseries.source_fx_args column for each timeseries_id
    locations <- c()
    for (i in 1:nrow(all_timeseries)) {
      loc <- DBI::dbGetQuery(
        con,
        "SELECT source_fx_args FROM timeseries WHERE timeseries_id = $1;",
        params = list(all_timeseries$timeseries_id[i])
      )[1, 1]
      loc <- jsonlite::fromJSON(loc)$location
      locations <- c(locations, loc)
    }
    all_timeseries$location <- locations

    # Get organization_id for 'Water Survey of Canada'
    organization_id <- DBI::dbGetQuery(
      con,
      "SELECT organization_id FROM organizations WHERE name = 'Water Survey of Canada'"
    )[1, 1]
    if (is.na(organization_id)) {
      df <- data.frame(name = 'Water Survey of Canada')
      DBI::dbAppendTable(con, "organizations", df)
    }

    grade_unspecified <- DBI::dbGetQuery(
      con,
      "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNS'"
    )[1, 1]
    if (is.na(grade_unspecified)) {
      stop(
        "Grade type 'Unspecified' (column grade_type_description) not found in the database. Please add it before proceeding."
      )
    }
    approval_approved <- DBI::dbGetQuery(
      con,
      "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'A'"
    )[1, 1]
    if (is.na(approval_approved)) {
      stop(
        "Approval type 'Approved' (column approval_type_description) not found in the database. Please add it before proceeding."
      )
    }

    qualifiers <- DBI::dbGetQuery(con, "SELECT * FROM qualifier_types")

    #Now update historical HYDAT timeseries.

    qualifier_mapping <- c(
      "B" = qualifiers[
        qualifiers$qualifier_type_code == "ICE",
        "qualifier_type_id"
      ],
      "E" = qualifiers[
        qualifiers$qualifier_type_code == "EST",
        "qualifier_type_id"
      ],
      "D" = qualifiers[
        qualifiers$qualifier_type_code == "DRY",
        "qualifier_type_id"
      ],
      "A" = qualifiers[
        qualifiers$qualifier_type_code == "UNK",
        "qualifier_type_id"
      ]
    )

    message(
      "Updating database with information in HYDAT due to new HYDAT version or request to force update..."
    )
    for (i in unique(all_timeseries$location)) {
      tryCatch(
        {
          new_flow <- as.data.frame(tidyhydat::hy_daily_flows(i))
          new_flow <- new_flow[, c("Date", "Value", "Symbol")]
          names(new_flow) <- c("date", "value", "qualifier")
          new_flow <- new_flow[!is.na(new_flow$value), ]

          new_flow$qualifier <- ifelse(
            new_flow$qualifier %in% names(qualifier_mapping),
            qualifier_mapping[new_flow$qualifier],
            qualifiers[
              qualifiers$qualifier_type_code == "UNK",
              "qualifier_type_id"
            ]
          )
          new_flow$qualifier <- as.integer(new_flow$qualifier)

          new_flow$owner <- organization_id
          new_flow$contributor <- organization_id
          new_flow$approval <- approval_approved
          new_flow$grade <- grade_unspecified
          new_flow$imputed <- FALSE
        },
        error = function(e) {
          new_flow <<- data.frame()
        }
      )
      tryCatch(
        {
          new_level <- as.data.frame(tidyhydat::hy_daily_levels(i))
          new_level <- new_level[, c("Date", "Value", "Symbol")]
          names(new_level) <- c("date", "value", "qualifier")
          new_level <- new_level[!is.na(new_level$value), ]

          new_level$qualifier <- ifelse(
            new_level$qualifier %in% names(qualifier_mapping),
            qualifier_mapping[new_level$qualifier],
            qualifiers[
              qualifiers$qualifier_type_code == "UNK",
              "qualifier_type_id"
            ]
          )
          new_level$qualifier <- as.integer(new_level$qualifier)

          new_level$owner <- organization_id
          new_level$contributor <- organization_id
          new_level$approval <- approval_approved
          new_level$grade <- grade_unspecified
          new_level$imputed <- FALSE
        },
        error = function(e) {
          new_level <<- data.frame()
        }
      )

      if (nrow(new_flow) > 0) {
        tryCatch(
          {
            parameter_id <- DBI::dbGetQuery(
              con,
              "SELECT parameter_id FROM parameters WHERE param_name = 'water flow'"
            )[1, 1]
            media_id <- DBI::dbGetQuery(
              con,
              "SELECT media_id FROM media_types WHERE media_type = 'surface water'"
            )[1, 1]
            location_id <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT location_id FROM locations WHERE location = '",
                i,
                "';"
              )
            )[1, 1]
            tsid_flow <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT timeseries_id FROM timeseries WHERE parameter_id = ",
                parameter_id,
                " AND location = '",
                i,
                "' AND source_fx = 'downloadWSC'"
              )
            )[1, 1]
            if (length(tsid_flow) == 0 | is.na(tsid_flow)) {
              #There is no realtime or daily data yet, and no corresponding tsid.
              new_entry <- data.frame(
                "location" = i,
                "location_id" = location_id,
                "parameter_id" = parameter_id,
                "category" = "continuous",
                "aggregation_type" = "instantaneous",
                "record_rate" = "5 minutes", # HYDAT is daily, but it should always correspond with a timeseries that has realtime data even it it's not in the database. This will ensure that the data starts coming in to complement the data being added to the 'measurements_calculated_daily' table here.
                "media_id" = media_id,
                "start_datetime" = min(new_flow$date),
                "end_datetime" = max(new_flow$date),
                "last_new_data" = .POSIXct(Sys.time(), tz = "UTC"),
                "share_with" = "{pulic_reader}",
                "owner" = 1,
                "source_fx" = "downloadWSC"
              )
              DBI::dbAppendTable(con, "timeseries", new_entry)
              tsid_flow <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT timeseries_id FROM timeseries WHERE location = '",
                  i,
                  "' AND parameter_id = ",
                  parameter_id,
                  " AND source_fx = 'downloadWSC';"
                )
              )[1, 1]
              new_flow$timeseries_id <- tsid_flow

              DBI::dbAppendTable(
                con,
                "measurements_calculated_daily",
                new_flow[, c("date", "value", "imputed", "timeseries_id")]
              )
              calculate_stats(
                timeseries_id = tsid_flow,
                con = con,
                start_recalc = min(new_flow$date)
              )

              # Now adjust the other tables
              names(new_flow[names(new_flow) == "date"]) <- "datetime"
              new_flow$datetime <- as.POSIXct(new_flow$datetime, tz = "UTC")

              adjust_approval(
                con,
                tsid_flow,
                new_flow[!is.na(new_flow$value), c("datetime", "approval")]
              )
              adjust_qualifier(
                con,
                tsid_flow,
                new_flow[!is.na(new_flow$value), c("datetime", "qualifier")]
              )
              adjust_owner(
                con,
                tsid_flow,
                new_flow[!is.na(new_flow$value), c("datetime", "owner")]
              )
              adjust_grade(
                con,
                tsid_flow,
                new_flow[!is.na(new_flow$value), c("datetime", "grade")]
              )
              adjust_contributor(
                con,
                tsid_flow,
                new_flow[!is.na(new_flow$value), c("datetime", "contributor")]
              )

              message(
                "Found historical flow daily means for a location that didn't yet exist in the local database. Added an entry to table 'timeseries' and calculated new daily stats."
              )
            } else {
              #There is a corresponding tsid in the database
              existing <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT date, value, imputed FROM measurements_calculated_daily WHERE timeseries_id = ",
                  tsid_flow
                )
              )
              if (nrow(existing) > 0) {
                #There is an entry in timeseries table AND existing data in measurements_calculated_daily
                #Find out if any imputed data should be left alone
                imputed <- existing[existing$imputed, ]
                imputed.remains <- data.frame()
                if (nrow(imputed) > 0) {
                  for (j in 1:nrow(imputed)) {
                    if (!(imputed[j, "date"] %in% new_flow$date)) {
                      imputed.remains <- rbind(imputed.remains, imputed[j, ])
                    }
                  }
                }

                # Adjust the ancillary tables
                adjust_flow <- new_flow
                names(adjust_flow)[names(adjust_flow) == "date"] <- "datetime"
                adjust_flow$datetime <- as.POSIXct(
                  adjust_flow$datetime,
                  tz = "UTC"
                )

                adjust_approval(
                  con,
                  tsid_flow,
                  adjust_flow[
                    !is.na(adjust_flow$value),
                    c("datetime", "approval")
                  ]
                )
                adjust_qualifier(
                  con,
                  tsid_flow,
                  adjust_flow[
                    !is.na(adjust_flow$value),
                    c("datetime", "qualifier")
                  ]
                )
                adjust_owner(
                  con,
                  tsid_flow,
                  adjust_flow[!is.na(adjust_flow$value), c("datetime", "owner")]
                )
                adjust_grade(
                  con,
                  tsid_flow,
                  adjust_flow[!is.na(adjust_flow$value), c("datetime", "grade")]
                )
                adjust_contributor(
                  con,
                  tsid_flow,
                  adjust_flow[
                    !is.na(adjust_flow$value),
                    c("datetime", "contributor")
                  ]
                )

                # Create a unique key for both data frames
                # order both data.frames by date to compare them
                new_flow <- new_flow[order(new_flow$date), ]
                existing <- existing[order(existing$date), ]
                new_flow$key <- paste(new_flow$date, new_flow$value, sep = "|")
                existing$key <- paste(existing$date, existing$value, sep = "|")

                # Check for mismatches using set operations
                mismatch_keys <- setdiff(new_flow$key, existing$key)

                # Check if there are any discrepancies
                if (length(mismatch_keys) > 0) {
                  mismatch <- TRUE
                  date <- new_flow[new_flow$key %in% mismatch_keys, "date"]
                  date <- min(date)
                } else {
                  mismatch <- FALSE
                }

                start <- min(existing$date, new_flow$date)

                if (mismatch) {
                  new_flow$key <- NULL
                  new_flow <- new_flow[new_flow$date >= date, ]
                  new_flow$timeseries_id <- tsid_flow

                  commit_fx1 <- function(
                    con,
                    imputed.remains,
                    tsid_flow,
                    new_flow,
                    existing,
                    start
                  ) {
                    if (nrow(imputed.remains) > 0) {
                      DBI::dbExecute(
                        con,
                        paste0(
                          "DELETE FROM measurements_calculated_daily WHERE timeseries_id = ",
                          tsid_flow,
                          " AND date BETWEEN '",
                          min(new_flow$date),
                          "' AND '",
                          max(new_flow$date),
                          "' AND date NOT IN ('",
                          paste(imputed.remains$date, collapse = "', '"),
                          "');"
                        )
                      )
                      DBI::dbAppendTable(
                        con,
                        "measurements_calculated_daily",
                        new_flow[, c(
                          "date",
                          "value",
                          "timeseries_id",
                          "imputed"
                        )]
                      )
                    } else {
                      DBI::dbExecute(
                        con,
                        paste0(
                          "DELETE FROM measurements_calculated_daily WHERE timeseries_id = ",
                          tsid_flow,
                          " AND date BETWEEN '",
                          min(new_flow$date),
                          "' AND '",
                          max(new_flow$date),
                          "';"
                        )
                      )
                      DBI::dbAppendTable(
                        con,
                        "measurements_calculated_daily",
                        new_flow[, c(
                          "date",
                          "value",
                          "timeseries_id",
                          "imputed"
                        )]
                      )
                    }

                    DBI::dbExecute(
                      con,
                      paste0(
                        "UPDATE timeseries SET start_datetime = '",
                        start,
                        "'WHERE timeseries_id = ",
                        tsid_flow,
                        ";"
                      )
                    )
                  }

                  activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
                  if (activeTrans) {
                    tryCatch(
                      {
                        commit_fx1(
                          con,
                          imputed.remains,
                          tsid_flow,
                          new_flow,
                          existing,
                          start
                        )
                        DBI::dbExecute(con, "COMMIT;")
                      },
                      error = function(e) {
                        DBI::dbExecute(con, "ROLLBACK;")
                        warning(
                          "update_hydat: Failed to add new flow data for location ",
                          i
                        )
                      }
                    )
                  } else {
                    # we're already in a transaction
                    commit_fx1(
                      con,
                      imputed.remains,
                      tsid_flow,
                      new_flow,
                      existing,
                      start
                    )
                  }

                  calculate_stats(
                    timeseries_id = tsid_flow,
                    con = con,
                    start_recalc = start
                  )
                } else {
                  # Check that start_datetime is correct in timeseries table
                  DBI::dbExecute(
                    con,
                    paste0(
                      "UPDATE timeseries SET start_datetime = '",
                      start,
                      "'WHERE timeseries_id = ",
                      tsid_flow,
                      ";"
                    )
                  )
                }
              } else {
                #There is an entry in timeseries table, but no daily data
                new_flow$timeseries_id <- tsid_flow

                commit_fx2 <- function(con, tsid_flow, new_flow) {
                  DBI::dbAppendTable(
                    con,
                    "measurements_calculated_daily",
                    new_flow[, c("date", "value", "timeseries_id", "imputed")]
                  )
                  DBI::dbExecute(
                    con,
                    paste0(
                      "UPDATE timeseries SET start_datetime = '",
                      min(new_flow$date),
                      "'WHERE timeseries_id = ",
                      tsid_flow,
                      ";"
                    )
                  )
                }

                activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
                if (activeTrans) {
                  tryCatch(
                    {
                      commit_fx2(con, tsid_flow, new_flow)
                      DBI::dbExecute(con, "COMMIT;")
                    },
                    error = function(e) {
                      DBI::dbExecute(con, "ROLLBACK;")
                      warning(
                        "update_hydat: Failed to add new flow data for location ",
                        i
                      )
                    }
                  )
                } else {
                  # we're already in a transaction
                  commit_fx2(con, tsid_flow, new_flow)
                }

                calculate_stats(
                  timeseries_id = tsid_flow,
                  con = con,
                  start_recalc = min(new_flow$date)
                )

                # Now adjust the other tables
                names(new_flow)[names(new_flow) == "date"] <- "datetime"
                new_flow$datetime <- as.POSIXct(new_flow$datetime, tz = "UTC")

                adjust_approval(
                  con,
                  tsid_flow,
                  new_flow[!is.na(new_flow$value), c("datetime", "approval")]
                )
                adjust_qualifier(
                  con,
                  tsid_flow,
                  new_flow[!is.na(new_flow$value), c("datetime", "qualifier")]
                )
                adjust_owner(
                  con,
                  tsid_flow,
                  new_flow[!is.na(new_flow$value), c("datetime", "owner")]
                )
                adjust_grade(
                  con,
                  tsid_flow,
                  new_flow[!is.na(new_flow$value), c("datetime", "grade")]
                )
                adjust_contributor(
                  con,
                  tsid_flow,
                  new_flow[!is.na(new_flow$value), c("datetime", "contributor")]
                )

                message(
                  "Found historical flow daily means for a location that had no data or only had realtime data. Added new entries to measurements_calculated_daily and calculated daily stats."
                )
              }
            }
            # Now find the earliest/last datetime between measurements_calculated_daily and measurements_realtime and update start_datetime in timeseries table if needed
            cd <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT MIN(date) AS start_datetime, MAX(date) AS end_datetime FROM measurements_calculated_daily WHERE timeseries_id = ",
                tsid_flow
              )
            )
            rt <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT MIN(datetime) AS start_datetime, MAX(datetime) AS end_datetime FROM measurements_continuous WHERE timeseries_id = ",
                tsid_flow
              )
            )
            if (is.na(cd$start_datetime) && !is.na(rt$start_datetime)) {
              sdt <- rt$start_datetime
            }
            if (!is.na(cd$start_datetime) && is.na(rt$start_datetime)) {
              sdt <- cd$start_datetime
            }
            if (!is.na(cd$start_datetime) && !is.na(rt$start_datetime)) {
              sdt <- min(cd$start_datetime, rt$start_datetime)
            }

            if (is.na(cd$end_datetime) && !is.na(rt$end_datetime)) {
              edt <- rt$end_datetime
            }
            if (!is.na(cd$end_datetime) && is.na(rt$end_datetime)) {
              edt <- cd$end_datetime
            }
            if (!is.na(cd$end_datetime) && !is.na(rt$end_datetime)) {
              edt <- min(cd$end_datetime, rt$end_datetime)
            }

            DBI::dbExecute(
              con,
              paste0(
                "UPDATE timeseries SET start_datetime = '",
                sdt,
                "' WHERE timeseries_id = ",
                tsid_flow
              )
            )
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE timeseries SET end_datetime = '",
                edt,
                "' WHERE timeseries_id = ",
                tsid_flow
              )
            )
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE timeseries SET last_new_data = NOW() WHERE timeseries_id = ",
                tsid_flow
              )
            )
          },
          error = function(e) {
            warning(
              "Something went wrong when trying to add new flow data for location ",
              i
            )
          }
        )
      } #End of for flow loop

      if (nrow(new_level) > 0) {
        tryCatch(
          {
            parameter_id <- DBI::dbGetQuery(
              con,
              "SELECT parameter_id FROM parameters WHERE param_name = 'water level'"
            )[1, 1]
            media_id <- DBI::dbGetQuery(
              con,
              "SELECT media_id FROM media_types WHERE media_type = 'surface water'"
            )[1, 1]
            location_id <- DBI::dbGetQuery(
              con,
              "SELECT location_id FROM locations WHERE location = $1",
              params = list(i)
            )[1, 1]
            tsid_level <- DBI::dbGetQuery(
              con,
              "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location = $2 AND source_fx = 'downloadWSC'",
              params = list(parameter_id, i)
            )[1, 1]
            if (length(tsid_level) == 0 | is.na(tsid_level)) {
              # There is no realtime or daily data yet, and no corresponding tsid.
              new_entry <- data.frame(
                "location" = i,
                "location_id" = location_id,
                "parameter_id" = parameter_id,
                "category" = "continuous",
                "aggregation_type" = "instantaneous",
                "record_rate" = "5 minutes", # HYDAT is daily, but it should always correspond with a timeseries that has realtime data even it it's not in the database. This will ensure that the data starts coming in to complement the data being added to the 'measurements_calculated_daily' table here.
                "media_id" = media_id,
                "start_datetime" = min(new_level$date),
                "end_datetime" = max(new_level$date),
                "last_new_data" = .POSIXct(Sys.time(), tz = "UTC"),
                "share_with" = "{public_reader}",
                "owner" = 1,
                "source_fx" = "downloadWSC"
              )
              DBI::dbAppendTable(con, "timeseries", new_entry)
              tsid_level <- DBI::dbGetQuery(
                con,
                "SELECT timeseries_id FROM timeseries WHERE location = $1 AND parameter_id = $2 AND source_fx = 'downloadWSC';",
                params = list(i, parameter_id)
              )[1, 1]
              new_level$timeseries_id <- tsid_level

              DBI::dbAppendTable(
                con,
                "measurements_calculated_daily",
                new_level[, c("date", "value", "imputed", "timeseries_id")]
              )
              calculate_stats(
                timeseries_id = tsid_level,
                con = con,
                start_recalc = min(new_level$date)
              )

              # Now adjust the other tables
              names(new_level)[names(new_level) == "date"] <- "datetime"
              new_level$datetime <- as.POSIXct(new_level$datetime, tz = "UTC")

              adjust_approval(
                con,
                tsid_level,
                new_level[!is.na(new_level$value), c("datetime", "approval")]
              )
              adjust_qualifier(
                con,
                tsid_level,
                new_level[!is.na(new_level$value), c("datetime", "qualifier")]
              )
              adjust_owner(
                con,
                tsid_level,
                new_level[!is.na(new_level$value), c("datetime", "owner")]
              )
              adjust_grade(
                con,
                tsid_level,
                new_level[!is.na(new_level$value), c("datetime", "grade")]
              )
              adjust_contributor(
                con,
                tsid_level,
                new_level[!is.na(new_level$value), c("datetime", "contributor")]
              )

              message(
                "Found historical level daily means for a location that didn't yet exist in the local database. Added an entry to table 'timeseries' and calculated new daily stats."
              )
            } else {
              # There is a corresponding tsid in the database
              existing <- DBI::dbGetQuery(
                con,
                "SELECT date, value, imputed FROM measurements_calculated_daily WHERE timeseries_id = $1",
                params = list(tsid_level)
              )
              if (nrow(existing) > 0) {
                #There is an entry in timeseries table AND existing data
                #Find out if any imputed data should be left alone
                imputed <- existing[existing$imputed, ]
                imputed.remains <- data.frame()
                if (nrow(imputed) > 0) {
                  for (j in 1:nrow(imputed)) {
                    if (!(imputed[j, "date"] %in% new_flow$date)) {
                      imputed.remains <- rbind(imputed.remains, imputed[j, ])
                    }
                  }
                }

                # Adjust the ancillary tables
                adjust_level <- new_level
                names(adjust_level)[names(adjust_level) == "date"] <- "datetime"
                adjust_level$datetime <- as.POSIXct(
                  adjust_level$datetime,
                  tz = "UTC"
                )

                adjust_approval(
                  con,
                  tsid_level,
                  adjust_level[
                    !is.na(adjust_level$value),
                    c("datetime", "approval")
                  ]
                )
                adjust_qualifier(
                  con,
                  tsid_level,
                  adjust_level[
                    !is.na(adjust_level$value),
                    c("datetime", "qualifier")
                  ]
                )
                adjust_owner(
                  con,
                  tsid_level,
                  adjust_level[
                    !is.na(adjust_level$value),
                    c("datetime", "owner")
                  ]
                )
                adjust_grade(
                  con,
                  tsid_level,
                  adjust_level[
                    !is.na(adjust_level$value),
                    c("datetime", "grade")
                  ]
                )
                adjust_contributor(
                  con,
                  tsid_level,
                  adjust_level[
                    !is.na(adjust_level$value),
                    c("datetime", "contributor")
                  ]
                )

                # Create a unique key for both data frames
                # Order both data.frames by date to compare them
                new_level <- new_level[order(new_level$date), ]
                existing <- existing[order(existing$date), ]
                new_level$key <- paste(
                  new_level$date,
                  new_level$value,
                  sep = "|"
                )
                existing$key <- paste(existing$date, existing$value, sep = "|")

                # Check for mismatches using set operations
                mismatch_keys <- setdiff(new_level$key, existing$key)

                # Check if there are any discrepancies
                if (length(mismatch_keys) > 0) {
                  mismatch <- TRUE
                  date <- new_level[new_level$key %in% mismatch_keys, "date"]
                  date <- min(date)
                } else {
                  mismatch <- FALSE
                }

                start <- min(min(existing$date), new_level$date)

                if (mismatch) {
                  new_level$key <- NULL
                  new_level <- new_level[new_level$date >= date, ]
                  new_level$timeseries_id <- tsid_level

                  commit_fx3 <- function(
                    con,
                    imputed.remains,
                    tsid_level,
                    new_level,
                    existing,
                    start
                  ) {
                    if (nrow(imputed.remains) > 0) {
                      DBI::dbExecute(
                        con,
                        paste0(
                          "DELETE FROM measurements_calculated_daily WHERE timeseries_id = ",
                          tsid_level,
                          " AND date BETWEEN '",
                          min(new_level$date),
                          "' AND '",
                          max(new_level$date),
                          "' AND date NOT IN ('",
                          paste(imputed.remains$date, collapse = "', '"),
                          "');"
                        )
                      )
                      DBI::dbAppendTable(
                        con,
                        "measurements_calculated_daily",
                        new_level[, c(
                          "date",
                          "value",
                          "timeseries_id",
                          "imputed"
                        )]
                      )
                    } else {
                      DBI::dbExecute(
                        con,
                        "DELETE FROM measurements_calculated_daily WHERE timeseries_id = $1 AND date BETWEEN $2 AND $3;",
                        params = list(
                          tsid_level,
                          min(new_level$date),
                          max(new_level$date)
                        )
                      )
                      DBI::dbAppendTable(
                        con,
                        "measurements_calculated_daily",
                        new_level[, c(
                          "date",
                          "value",
                          "timeseries_id",
                          "imputed"
                        )]
                      )
                    }

                    DBI::dbExecute(
                      con,
                      "UPDATE timeseries SET start_datetime = $1 WHERE timeseries_id = $2",
                      params = list(start, tsid_level)
                    )
                  }

                  activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
                  if (activeTrans) {
                    tryCatch(
                      {
                        commit_fx3(
                          con,
                          imputed.remains,
                          tsid_level,
                          new_level,
                          existing,
                          start
                        )
                        DBI::dbExecute(con, "COMMIT;")
                      },
                      error = function(e) {
                        DBI::dbExecute(con, "ROLLBACK;")
                        warning(
                          "update_hydat: Failed to add new level data for location ",
                          i
                        )
                      }
                    )
                  } else {
                    # we're already in a transaction
                    commit_fx3(
                      con,
                      imputed.remains,
                      tsid_level,
                      new_level,
                      existing,
                      start
                    )
                  }

                  calculate_stats(
                    timeseries_id = tsid_level,
                    con = con,
                    start_recalc = start
                  )
                } else {
                  # Check that star_datetime is correct in timeseries table
                  DBI::dbExecute(
                    con,
                    "UPDATE timeseries SET start_datetime = $1 WHERE timeseries_id = $2",
                    params = list(start, tsid_level)
                  )
                }
              } else {
                #There is an entry in timeseries table, but no daily data
                new_level$timeseries_id <- tsid_level

                commit_fx4 <- function(con, tsid_level, new_level) {
                  DBI::dbAppendTable(
                    con,
                    "measurements_calculated_daily",
                    new_level[, c("date", "value", "timeseries_id", "imputed")]
                  )
                  DBI::dbExecute(
                    con,
                    "UPDATE timeseries SET start_datetime = $1 WHERE timeseries_id = $2",
                    params = list(min(new_level$date), tsid_level)
                  )
                }

                activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
                if (activeTrans) {
                  tryCatch(
                    {
                      commit_fx4(con, tsid_level, new_level)
                      DBI::dbExecute(con, "COMMIT;")
                    },
                    error = function(e) {
                      DBI::dbExecute(con, "ROLLBACK;")
                      warning(
                        "update_hydat: Failed to add new level data for location ",
                        i
                      )
                    }
                  )
                } else {
                  # we're already in a transaction
                  commit_fx4(con, tsid_level, new_level)
                }

                calculate_stats(
                  timeseries_id = tsid_level,
                  con = con,
                  start_recalc = min(new_level$date)
                )

                # Now adjust the other tables
                names(new_level)[names(new_level) == "date"] <- "datetime"
                new_level$datetime <- as.POSIXct(new_level$datetime, tz = "UTC")

                adjust_approval(
                  con,
                  tsid_level,
                  new_level[!is.na(new_level$value), c("datetime", "approval")]
                )
                adjust_qualifier(
                  con,
                  tsid_level,
                  new_level[!is.na(new_level$value), c("datetime", "qualifier")]
                )
                adjust_owner(
                  con,
                  tsid_level,
                  new_level[!is.na(new_level$value), c("datetime", "owner")]
                )
                adjust_grade(
                  con,
                  tsid_level,
                  new_level[!is.na(new_level$value), c("datetime", "grade")]
                )
                adjust_contributor(
                  con,
                  tsid_level,
                  new_level[
                    !is.na(new_level$value),
                    c("datetime", "contributor")
                  ]
                )

                message(
                  "Found historical level daily means for a location that had no data or only had realtime data. Added new entries to measurements_calculated_daily and calculated daily stats."
                )
              }
            }

            # Now find the earliest/last datetime between measurements_calculated_daily and measurements_realtime and update start_datetime in timeseries table if needed
            cd <- DBI::dbGetQuery(
              con,
              "SELECT MIN(date) AS start_datetime, MAX(date) AS end_datetime FROM measurements_calculated_daily WHERE timeseries_id = $1",
              params = list(tsid_level)
            )
            rt <- DBI::dbGetQuery(
              con,
              "SELECT MIN(datetime) AS start_datetime, MAX(datetime) AS end_datetime FROM measurements_continuous WHERE timeseries_id = $1",
              params = list(tsid_level)
            )
            if (is.na(cd$start_datetime) && !is.na(rt$start_datetime)) {
              sdt <- rt$start_datetime
            }
            if (!is.na(cd$start_datetime) && is.na(rt$start_datetime)) {
              sdt <- cd$start_datetime
            }
            if (!is.na(cd$start_datetime) && !is.na(rt$start_datetime)) {
              sdt <- min(cd$start_datetime, rt$start_datetime)
            }

            if (is.na(cd$end_datetime) && !is.na(rt$end_datetime)) {
              edt <- rt$end_datetime
            }
            if (!is.na(cd$end_datetime) && is.na(rt$end_datetime)) {
              edt <- cd$end_datetime
            }
            if (!is.na(cd$end_datetime) && !is.na(rt$end_datetime)) {
              edt <- min(cd$end_datetime, rt$end_datetime)
            }

            DBI::dbExecute(
              con,
              "UPDATE timeseries SET start_datetime = $1 WHERE timeseries_id = $2",
              params = list(sdt, tsid_level)
            )
            DBI::dbExecute(
              con,
              "UPDATE timeseries SET end_datetime = $1 WHERE timeseries_id = $2",
              params = list(edt, tsid_level)
            )
            DBI::dbExecute(
              con,
              "UPDATE timeseries SET last_new_data = NOW() WHERE timeseries_id = $1",
              params = list(tsid_level)
            )
          },
          error = function(e) {
            warning(
              "Something went wrong when trying to add new level data for location ",
              i
            )
          }
        )
      } #End of for level loop
    } #End of for loop iterating over locations

    try(
      # In a try in case the user doesn't have update permissions on internal_status
      {
        DBI::dbExecute(
          con,
          "UPDATE internal_status SET value = NOW() WHERE event = 'HYDAT_version'"
        )
      },
      silent = TRUE
    )

    message("Completed update of HYDAT related data.")
  } else {
    message(
      "No updates were made because the last HYDAT version referenced in the database is the same as the current HYDAT, and you didn't specify force_update = TRUE."
    )
  } #End of function portion that seeks to update HYDAT related data
  return(new_hydat)
} #End of function
