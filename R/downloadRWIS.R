#' Get data from the RWIS database
#'
#'@description
#' A function used to fetch data from the 'RWDM' database, normally only accessible from within the Yukon Government network.
#'
#' @param location Location abbreviation, corresponding to the `abbreviation` field of RWDM table 'stations_station'.
#' @param parameter One of the rwdm parameter codes, corresponding to a column name in the 'measurements_measurement' table.
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param con A connection to the aquacache database, necessary to allow for the mapping of NWIS approvals, grades, and qualifiers to the database. If left NULL connection will be made and closed automatically using the function [AquaConnect()] and its default parameters.
#' @param rwis A connection to the RWIS database. If left NULL a connection will be made and closed automatically using the function [RWISConnect()] and its default parameters.
#'
#' @return A data.frame object of hydrometric data, with datetimes in UTC-0.
#' @export

downloadRWIS <- function(
  location,
  parameter,
  start_datetime,
  end_datetime = Sys.time(),
  con = NULL,
  rwis = NULL
) {
  if (is.null(rwis)) {
    rwis <- RWISConnect()
    on.exit(DBI::dbDisconnect(rwis), add = TRUE)
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  tryCatch(
    {
      if (inherits(start_datetime, c("character", "Date"))) {
        # Either way defaults to 0 hour
        start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
      } else if (inherits(start_datetime, "POSIXct")) {
        attr(start_datetime, "tzone") <- "UTC"
      } else {
        stop("Parameter start_datetime could not be coerced to POSIXct.")
      }
      if (nchar(start_datetime) == 10) {
        start_datetime <- paste0(start_datetime, " 00:00:00")
      }
    },
    error = function(e) {
      stop("Failed to convert parameter start_datetime to POSIXct.")
    }
  )
  tryCatch(
    {
      if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) {
        # Does not necessarily default to 0 hour.
        end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      } else if (inherits(end_datetime, "POSIXct")) {
        attr(end_datetime, "tzone") <- "UTC"
      } else if (
        inherits(end_datetime, "Date") |
          (inherits(end_datetime, "character") & nchar(end_datetime) == 10)
      ) {
        end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
        end_datetime <- end_datetime + 60 * 60 * 23.9999
      } else {
        stop("Parameter end_datetime could not be coerced to POSIXct.")
      }
      if (nchar(end_datetime) == 10) {
        end_datetime <- paste0(end_datetime, " 00:00:00")
      }
    },
    error = function(e) {
      stop("Failed to convert parameter end_datetime to POSIXct.")
    }
  )

  tryCatch(
    {
      # Get the 'id' of the location from the abbreviation
      # abbreviation is unique in stations_station table
      stn_id <- DBI::dbGetQuery(
        rwis,
        "SELECT id FROM stations_station WHERE abbreviation = $1",
        params = list(location)
      )[1, 1]

      # Get the data from measurements_measurement table for the specified location id, parameter, and datetime range
      # USE UNNEST to extract the array column corresponding to the parameter
      # Filter out NULLs and -9999 (missing data code)
      data <- DBI::dbGetQuery(
        rwis,
        paste0(
          "SELECT measurement_time AS datetime, value ",
          "FROM measurements_measurement, ",
          "UNNEST(",
          parameter,
          ") AS value ",
          "WHERE station_id = ",
          stn_id,
          " AND measurement_time BETWEEN '",
          start_datetime,
          "' AND '",
          end_datetime,
          "' AND value IS NOT NULL AND value != -9999 ",
          "ORDER BY measurement_time ASC"
        )
      )

      # Check for duplicate datetimes and trow a stop if found
      if (any(duplicated(data$datetime))) {
        stop(
          "Duplicate datetimes found in RWIS data for location ",
          location,
          " and parameter ",
          parameter,
          " after removing NULL and -9999 array values.."
        )
      }

      return(data)
    },
    error = function(e) {
      data <- data.frame()
      return(data)
    }
  )
}
