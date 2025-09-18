#' Get new rasters
#'
#' @description
#'
#' Retrieves new data corresponding to entries in the table "raster_series_index" for which the column 'public' is TRUE. You can add a new raster series with [addACRasterSeries()]. As with the timeseries and images table, fetching new data depends on the function listed in the source_fx column of the relevant table and optionally on parameters in column source_fx_args. Refer to [addACTimeseries()] for a description of how to formulate these arguments.
#'
#' @param raster_series_ids A vector of raster_series_id's. Default 'all' fetches all ids in the raster_series_index table.
#' @param con A connection to the database. Default is NULL, which will use the package default connection settings and close the connection afterwards.
#' @param keep_forecasts Should forecasts be kept or replaced? Default is 'selective', which keeps only rasters for which there is no new forecast. 'all' keeps all forecasts, and 'none' replaces all forecasts. This does not apply to raster series labelled as 'reanalysis'
#' @param active Sets behavior for import of new rasters for raster series. If set to 'default', the column 'active' in the raster_series_index table will determine whether to get new raster or not. If set to 'all', all image series will be fetched regardless of the 'active' column.
#' @param start_datetime A start datetime to fetch rasters from. By default, fetches from the last raster end_datetime + 1 second, however this parameter is provided for flexibility. If combined with `replace = TRUE`, could be used to replace rasters from a specific datetime to `end_datetime`. Specify as POSIXct or something coercible to POSIXct; coercion will be done with to UTC time zone. Only used for reanalysis rasters!
#' @param end_datetime An end datetime to fetch rasters to. By default, fetches to the current time, however this parameter is provided for flexibility. If combined with `replace = TRUE` (Warning! parameter not implemented yet), could be used to replace rasters from `start_datetime` to a specific datetime. Specify as POSIXct or something coercible to POSIXct; coercion will be done with to UTC time zone. Only used for reanalysis rasters!
#' @export

getNewRasters <- function(
  raster_series_ids = "all",
  con = NULL,
  keep_forecasts = 'selective',
  active = 'default',
  start_datetime = NULL,
  end_datetime = NULL
) {
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

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  # Create table of meta_ids
  if (raster_series_ids[1] == "all") {
    meta_ids <- DBI::dbGetQuery(
      con,
      "SELECT raster_series_id, end_datetime, last_issue, type, source_fx, source_fx_args, parameter, active FROM raster_series_index WHERE source_fx IS NOT NULL;"
    )
  } else {
    meta_ids <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT raster_series_id, end_datetime, last_issue, type, source_fx, source_fx_args, parameter, active FROM raster_series_index WHERE raster_series_id IN ('",
        paste(raster_series_ids, collapse = "', '"),
        "') AND source_fx IS NOT NULL;"
      )
    )
    if (length(raster_series_ids) != nrow(meta_ids)) {
      warning(
        "At least one of the raster_series_ids you called for cannot be found in the database or has no function specified in column source_fx of table raster_series_index"
      )
    }
  }

  if (active == 'default') {
    meta_ids <- meta_ids[meta_ids$active, ]
  }

  if (nrow(meta_ids) == 0) {
    message("No raster_series_id's found to update base on input parameters.")
    return(NULL)
  }

  message("Fetching new rasters with getNewRasters")

  count <- 0 #counter for number of successful new pulls
  raster_count <- 0
  success <- character(0)
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(meta_ids), style = 3)
  }
  for (i in 1:nrow(meta_ids)) {
    id <- meta_ids[i, "raster_series_id"]
    message(
      "Working on raster_series_id ",
      id,
      " for parameter '",
      meta_ids[i, "parameter"],
      "' and source function '",
      meta_ids[i, "source_fx"],
      "'"
    )
    source_fx <- meta_ids[i, "source_fx"]
    source_fx_args <- meta_ids[i, "source_fx_args"]
    type <- meta_ids[i, "type"]
    end_datetime_i <- end_datetime
    start_datetime_i <- start_datetime
    if (type == "reanalysis") {
      # Reanalysis data may have preliminary rasters that should be replaced when final versions are produced.
      prelim <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT min(valid_from) FROM rasters_reference WHERE flag = 'PRELIMINARY' AND valid_from > '",
          meta_ids[i, "end_datetime"] - 60 * 60 * 24 * 30,
          "' AND raster_series_id = ",
          id,
          ";"
        )
      )[1, 1] #searches for rasters labelled 'prelim' within the last 30 days. If exists, try to replace it and later rasters
      if (!is.na(prelim)) {
        if (!is.null(end_datetime)) {
          if (start_datetime_i < prelim) {
            next_instant <- start_datetime_i
          } else {
            next_instant <- prelim - 1 # one second before the last raster end_datetime so that the last earliest prelim raster is replaced.
          }
        } else {
          next_instant <- prelim - 1 # one second before the last raster end_datetime so that the last earliest prelim raster is replaced.
        }
      } else {
        if (!is.null(start_datetime_i)) {
          next_instant <- start_datetime_i
        } else {
          next_instant <- meta_ids[i, "end_datetime"] + 1 # one second after the last raster end_datetime
        }
      }
    } else if (type == "forecast") {
      # Forecast rasters should be replaced with the next forecast, so we fetch the last_issue
      if (!is.null(end_datetime_i)) {
        end_datetime_i <- NULL
      }
      if (!is.null(start_datetime_i)) {
        start_datetime_i <- NULL
      }
      next_instant <- meta_ids[i, "last_issue"]
      if (is.na(next_instant)) {
        #If there is no last_issue, we fetch from the last raster end_datetime. This could happen when creating a new series.
        next_instant <- meta_ids[i, "end_datetime"] + 1 #one second after the last raster end_datetime
      }
    }

    tryCatch(
      {
        args_list <- list(start_datetime = next_instant)
        if (!is.null(end_datetime_i)) {
          args_list$end_datetime <- end_datetime_i # If end_datetime_i is specified, add it to the args_list
        }
        if (!is.na(source_fx_args)) {
          #add some arguments if they are specified
          args <- jsonlite::fromJSON(source_fx_args)
          args_list <- c(args_list, lapply(args, as.character))
        }

        rasters <- do.call(source_fx, args_list) #Get the data using the args_list

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

        if (!is.null(rasters)) {
          for (j in 1:length(rasters)) {
            rast <- rasters[[j]]
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
            #Check if the raster already exists. If it does but flag is PRELIMINARY AND the new one is not, delete the prelim one and replace.
            exists <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT reference_id FROM rasters_reference WHERE valid_from = '",
                valid_from,
                "' AND raster_series_id = ",
                id,
                " AND flag = 'PRELIMINARY';"
              )
            )[1, 1]
            if (!is.na(exists) & is.na(flag)) {
              #If the raster already exists and the new one is not a prelim, delete the prelim one and replace.
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM rasters_reference WHERE reference_id = ",
                  exists,
                  ";"
                )
              ) #This should cascade to the rasters table
            } else if (!is.na(exists) & !is.na(flag)) {
              #If the raster already exists and the new one is a prelim, skip to to the next one
              next
            } else if (is.na(exists)) {
              # Check if the raster already exists in non-preliminary format
              exists <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT reference_id FROM rasters_reference WHERE valid_from = '",
                  valid_from,
                  "' AND raster_series_id = ",
                  id,
                  " AND flag IS NULL;"
                )
              )[1, 1]
              # Delete the old raster if it exists
              if (!is.na(exists)) {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM rasters_reference WHERE reference_id = ",
                    exists,
                    ";"
                  )
                ) #This should cascade to the rasters table
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
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE raster_series_index SET last_new_raster = '",
                .POSIXct(Sys.time(), tz = "UTC"),
                "' WHERE raster_series_id = ",
                id,
                ";"
              )
            )
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE raster_series_index SET end_datetime = '",
                valid_to,
                "' WHERE raster_series_id = ",
                id,
                ";"
              )
            )
            raster_count <- raster_count + 1
          }

          if (forecast) {
            # Delete the old forecast rasters as per input parameters, adjust raster_series_index.last_issue
            valid_from <- as.POSIXct(
              sapply(rasters, function(x) x$valid_from),
              tz = "UTC"
            )
            if (keep_forecasts == 'selective') {
              # Find the reference_id in table rasters_reference corresponding to valid_from times in the newly appended rasters and delete the old ones
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM rasters_reference WHERE raster_series_id = ",
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
              # Delete all rasters from this series
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM rasters_reference WHERE raster_series_id = ",
                  id,
                  " AND issued NOT BETWEEN '",
                  issued - 5,
                  "' AND '",
                  issued + 5,
                  "';"
                )
              )
              # Update start_datetime in the relevant table
              DBI::dbExecute(
                con,
                paste0(
                  "UPDATE raster_series_index SET last_issue = '",
                  issued_datetime,
                  "', start_datetime = '",
                  min(valid_from),
                  "' WHERE raster_series_id = ",
                  id,
                  ";"
                )
              )
            } # else keep_forecasts == 'all', so delete nothing
          }

          count <- count + 1
          success <- c(success, id)
        } else {
          message(
            "getNewRasters: No new rasters found for raster_series_id ",
            id,
            "."
          )
        }
      },
      error = function(e) {
        warning(
          "getNewRasters: Failed to get new rasters or to append new rasters for raster_series_id ",
          id,
          " with error message: ",
          e$message
        )
      },
      warning = function(w) {
        warning(
          "getNewRasters: Warning while processing raster_series_id ",
          id,
          ": ",
          w$message
        )
      }
    )

    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
  } # End of for loop

  if (interactive()) {
    close(pb)
  }

  message(
    count,
    " out of ",
    nrow(meta_ids),
    " raster_series_id's were updated."
  )
  message(raster_count, " rasters were added in total.")
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE internal_status SET value = '",
      .POSIXct(Sys.time(), "UTC"),
      "' WHERE event = 'last_new_rasters'"
    )
  )
  return(success)
}
