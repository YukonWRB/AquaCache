#' Get new continuous-category data
#'
#' @description
#'
#' Retrieves new real-time data starting from the last data point in the local database, using the function specified in the timeseries table column "source_fx". Only works on stations that are ALREADY in the measurements_continuous table and that have a proper entry in the timeseries table; refer to [addACTimeseries()] for how to add new stations. Does not work on any timeseries of category "discrete": for that, use [getNewDiscrete()]. Timeseries with no specified souce_fx will be ignored.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function for start_datetime, defaults to the instant after the last point already existing in the DB. The rest of the fetch parameters are set using the "source_fx_args" column in the "timeseries" table; refer to [addACTimeseries()] for a description of how to formulate these arguments.
#'
#' ## Assigning measurement periods:
#' With the exception of "instantaneous" timeseries which automatically receive a period of "00:00:00" (0 time), the period associated with measurements (ex: 1 hour precipitation sum) is derived from the interval between measurements UNLESS a period column is provided by the source function (column source_fx, may also depend on source_fx_args). This function typically fetches only a few hours of measurements at a time, so if the interval cannot be conclusively determined from the new data (i.e. hourly measurements over four hours with two measurements missed) then additional data points will be pulled from the database.
#'
#' If a period supplied by any data fetch function cannot be coerced to an period object acceptable to "duration" data type, NULL values will be entered to differentiate from instantaneous periods of "00:00:00".
#'
#' ## Sharing privileges and ownership
#' This is dictated by the timeseries table, and checked prior to passing data through view tables to public users.
#'
#' @param con  A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#' @param timeseries_id The timeseries_ids you wish to have updated, as character or numeric vector. Defaults to "all", which means all timeseries of category 'continuous'.
#' @param active Sets behavior for import of new data. If set to 'default', the function will look to the column 'active' in the 'timeseries' table to determine if new data should be fetched. If set to 'all', the function will ignore the 'active' column and import all data.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export

getNewContinuous <- function(
  con = NULL,
  timeseries_id = "all",
  active = 'default'
) {
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  # Create table of timeseries
  if (timeseries_id[1] == "all") {
    all_timeseries <- DBI::dbGetQuery(
      con,
      "SELECT t.location, t.parameter_id, t.timeseries_id, t.source_fx, t.source_fx_args, t.end_datetime, at.aggregation_type, t.default_owner, t.active FROM timeseries t JOIN aggregation_types at ON t.aggregation_type_id = at.aggregation_type_id WHERE source_fx IS NOT NULL;"
    )
  } else {
    all_timeseries <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT t.location, t.parameter_id, t.timeseries_id, t.source_fx, t.source_fx_args, t.end_datetime, at.aggregation_type, t.default_owner, t.active FROM timeseries t JOIN aggregation_types at ON t.aggregation_type_id = at.aggregation_type_id WHERE timeseries_id IN ('",
        paste(timeseries_id, collapse = "', '"),
        "') AND source_fx IS NOT NULL;"
      )
    )
    if (length(timeseries_id) != nrow(all_timeseries)) {
      warning(
        "At least one of the timeseries IDs you called for cannot be found in the database or has no function specified in column source_fx."
      )
    }
  }

  if (active == 'default') {
    all_timeseries <- all_timeseries[all_timeseries$active, ]
  }

  if (nrow(all_timeseries) == 0) {
    stop("Could not find any timeseries matching your input parameters.")
  }

  count <- 0 #counter for number of successful new pulls
  success <- data.frame(
    "location" = NULL,
    "parameter_id" = NULL,
    "timeseries" = NULL
  )

  grade_unknown <- DBI::dbGetQuery(
    con,
    "SELECT grade_type_id FROM grade_types WHERE grade_type_code = 'UNK';"
  )[1, 1]
  if (is.na(grade_unknown)) {
    stop(
      "getNewContinuous: Could not find grade type 'Unknown' in the database."
    )
  }
  approval_unknown <- DBI::dbGetQuery(
    con,
    "SELECT approval_type_id FROM approval_types WHERE approval_type_code = 'UNK';"
  )[1, 1]
  if (is.na(approval_unknown)) {
    stop(
      "getNewContinuous: Could not find approval type 'Unknown' in the database."
    )
  }
  qualifier_unknown <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id FROM qualifier_types WHERE qualifier_type_code = 'UNK';"
  )[1, 1]
  if (is.na(qualifier_unknown)) {
    stop(
      "getNewContinuous: Could not find qualifier type 'Unknown' in the database."
    )
  }

  message("Fetching new continuous data with getNewContinuous...")
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(all_timeseries), style = 3)
  }
  # Run for loop over timeseries rows
  for (i in 1:nrow(all_timeseries)) {
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter_id[i]
    aggregation_type <- all_timeseries$aggregation_type[i]
    tsid <- all_timeseries$timeseries_id[i]
    source_fx <- all_timeseries$source_fx[i]
    source_fx_args <- all_timeseries$source_fx_args[i]
    owner <- all_timeseries$default_owner[i]

    last_data_point <- all_timeseries$end_datetime[i] + 1 #one second after the last data point

    tryCatch(
      {
        args_list <- list(start_datetime = last_data_point, con = con)
        if (!is.na(source_fx_args)) {
          #add some arguments if they are specified
          args <- jsonlite::fromJSON(source_fx_args)
          args_list <- c(args_list, lapply(args, as.character))
        }

        ts <- do.call(source_fx, args_list) #Get the data using the args_list
        ts <- ts[!is.na(ts$value), ]

        if (nrow(ts) > 0) {
          # Check that ts has columns named 'value' and 'datetime' at minimum
          if (!("value" %in% names(ts)) | !("datetime" %in% names(ts))) {
            stop(
              "getNewContinuous: The data returned by source_fx does not have columns named 'value' and 'datetime'."
            )
          }

          ts$timeseries_id <- tsid
          ts$imputed <- FALSE

          if ("owner" %in% names(ts)) {
            if (!is.null(owner)) {
              ts$owner[is.na(ts$owner)] <- owner
            }
          } else {
            if (!is.null(owner)) {
              ts$owner <- owner
            }
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

          commit_fx <- function(con, ts, last_data_point, tsid) {
            adjust_grade(con, tsid, ts[, c("datetime", "grade")])
            adjust_approval(con, tsid, ts[, c("datetime", "approval")])
            adjust_qualifier(con, tsid, ts[, c("datetime", "qualifier")])
            if ("owner" %in% names(ts)) {
              adjust_owner(con, tsid, ts[, c("datetime", "owner")])
            }
            if ("contributor" %in% names(ts)) {
              adjust_contributor(con, tsid, ts[, c("datetime", "contributor")])
            }

            # Drop columns no longer necessary
            ts <- ts[, c("datetime", "value", "timeseries_id", "imputed")]

            #assign a period to the data
            if (aggregation_type == "instantaneous") {
              #Period is always 0 for instantaneous data
              ts$period <- "00:00:00"
            } else if (
              (aggregation_type != "instantaneous") & !("period" %in% names(ts))
            ) {
              #aggregation_types of mean, median, min, max should all have a period
              ts <- calculate_period(data = ts, timeseries_id = tsid, con = con)
            } else {
              #Check to make sure that the supplied period can actually be coerced to a period
              check <- lubridate::period(unique(ts$period))
              if (NA %in% check) {
                ts$period <- NA
              }
            }

            if (min(ts$datetime) < last_data_point - 1) {
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM measurements_continuous WHERE datetime >= '",
                  min(ts$datetime),
                  "' AND timeseries_id = ",
                  tsid,
                  ";"
                )
              )
            }
            DBI::dbAppendTable(con, "measurements_continuous", ts)
            #make the new entry into table timeseries
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE timeseries SET end_datetime = '",
                max(ts$datetime),
                "', last_new_data = '",
                .POSIXct(Sys.time(), "UTC"),
                "' WHERE timeseries_id = ",
                tsid,
                ";"
              )
            )
          } # End of commit_fx function

          activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress or was set up, otherwise commit will happen in the original calling function.
          if (activeTrans) {
            tryCatch(
              {
                commit_fx(con, ts, last_data_point, tsid)
                DBI::dbExecute(con, "COMMIT;")
                count <- count + 1
                success <- rbind(
                  success,
                  data.frame(
                    "location" = loc,
                    "parameter_id" = parameter,
                    "timeseries_id" = tsid
                  )
                )
              },
              error = function(e) {
                DBI::dbExecute(con, "ROLLBACK;")
                warning(
                  "getNewContinuous: Failed to append new data at location ",
                  loc,
                  " and parameter ",
                  parameter,
                  " (timeseries_id ",
                  all_timeseries$timeseries_id[i],
                  "). Returned error '",
                  e$message,
                  "'."
                )
              }
            )
          } else {
            commit_fx(con, ts, last_data_point, tsid)
            count <- count + 1
            success <- rbind(
              success,
              data.frame(
                "location" = loc,
                "parameter_id" = parameter,
                "timeseries_id" = tsid
              )
            )
          }
        }
      },
      error = function(e) {
        warning(
          "getNewContinuous: Failed to get new data or to append new data at location ",
          loc,
          " and parameter ",
          parameter,
          " (timeseries_id ",
          all_timeseries$timeseries_id[i],
          "). Returned error '",
          e$message,
          "'."
        )
      }
    ) #End of tryCatch

    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
  } # End of for loop

  if (interactive()) {
    close(pb)
  }

  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE internal_status SET value = '",
      .POSIXct(Sys.time(), "UTC"),
      "' WHERE event = 'last_new_continuous'"
    )
  )

  if (nrow(success) > 0) {
    return(success)
  }
} #End of function
