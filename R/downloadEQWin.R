#' Bring EQWin water quality data into the aquacache database
#'
#' @description
#'
#' Brings in water quality data from EQWin databases.
#'
#' @param location The location code associated with the EQWin station. Must match an entry in the eqstns table, column StnCode.
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, but if POSIXct the object's tzone attribute will be preserved. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Same as start_datetime but not quite!
#' @param EQpath The file path to the target EQWin database.
#' @param key A path to the .csv file mapping EQWin parameter codes to aquacache parameter codes.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export

downloadEQWin <- function(
  location,
  sub_location,
  start_datetime,
  end_datetime = Sys.time(),
  EQpath = NULL,
  key = NULL
) {
  # Testing parameters
  EQpath <- "G:/water/Data/Databases_Virtual_Machines/databases/EQWinDB/WaterResourcesEG.mdb"

  stop(
    "This function has not yet been updated to work with the new (as of early August 2024) measurements_discrete table schema."
  )

  # Checking start_datetime parameter
  tryCatch(
    {
      if (inherits(start_datetime, "character") & nchar(start_datetime) > 10) {
        #Does not necessarily default to 0 hour.
        start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
      } else if (
        inherits(start_datetime, "Date") |
          (inherits(start_datetime, "character") & nchar(start_datetime) == 10)
      ) {
        #defaults to 0 hour
        start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
      } else {
        stop("Parameter start_datetime could not be coerced to POSIXct.")
      }
    },
    error = function(e) {
      stop("Failed to convert parameter start_datetime to POSIXct.")
    }
  )

  # Checking end_datetime parameter
  tryCatch(
    {
      if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) {
        #Does not necessarily default to 0 hour.
        end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      } else if (
        inherits(end_datetime, "Date") |
          (inherits(end_datetime, "character") & nchar(end_datetime) == 10)
      ) {
        #defaults to very end of day
        end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
        end_datetime <- end_datetime + 60 * 60 * 23.9999
      } else {
        stop("Parameter end_datetime could not be coerced to POSIXct.")
      }
    },
    error = function(e) {
      stop("Failed to convert parameter end_datetime to POSIXct.")
    }
  )

  # Connect
  if (!file.exists(EQpath)) {
    stop("The specified EQWin database path does not exist.")
  } else {
    # Ensure it's a .mdb file
    if (tools::file_ext(EQpath) != "mdb") {
      stop(
        "The specified EQWin database path does not appear to be a .mdb file."
      )
    }
  }
  EQcon <- EQConnect(path = EQpath, silent = TRUE)
  on.exit(DBI::dbDisconnect(EQcon), add = TRUE)

  # Load key
  if (!file.exists(key)) {
    stop("The specified key file path does not exist.")
  } else {
    # Ensure it's a .csv file
    if (tools::file_ext(key) != "csv") {
      stop("The specified key file path does not appear to be a .csv file.")
    }
  }
  key <- read.csv(key, stringsAsFactors = FALSE)

  # Get the data ##############
  # Find the StnId
  stn <- DBI::dbGetQuery(
    EQcon,
    paste0(
      "SELECT StnId FROM eqstns WHERE StnCode = '",
      location,
      "';"
    )
  )
  # Fetch all samples between start and end datetimes and stick these into a list as data.frames. Each sample is in a list element, and each list element will also have a data.frame of results associated with that sample.
  samps <- DBI::dbGetQuery(
    EQcon,
    paste0(
      "SELECT SampleId, CollectDateTime FROM eqsampls WHERE StnId = ",
      stn,
      " AND CollectDateTime >= #",
      as.character(start_datetime),
      "# AND CollectDateTime <= #",
      as.character(end_datetime),
      "#;"
    )
  )
  if (nrow(samps) == 0) {
    message("No new data found.")
    return(data.frame())
  }

  # Many CollectDateTime entries have time 00:00:00 which is unlikely to be correct. We will assume that if the time is 00:00:00 then the actual time is unknown and we will set it to 19:00 UTC on that date (noon local Yukon time). If the time is something other than 00:00:00 we will assume it is correct and convert to UTC.
  if (
    length(samps$CollectDateTime[
      format(samps$CollectDateTime, "%H:%M:%S") == "00:00:00"
    ]) >
      0
  ) {
    samps$CollectDateTime[
      format(samps$CollectDateTime, "%H:%M:%S") == "00:00:00"
    ] <-
      paste0(
        format(
          as.Date(samps$CollectDateTime[
            format(samps$CollectDateTime, "%H:%M:%S") == "00:00:00"
          ]),
          "%Y-%m-%d"
        ),
        " 5:00:00"
      )
  }

  # Convert time zone to MST (it comes out of the database as UTC but is actually MST) and then set tzone attribute to UTC so that when printed it shows the correct time in UTC.
  lubridate::tz(samps$CollectDateTime) <- "MST"
  attr(samps$CollectDateTime, "tzone") <- "UTC"

  samps_list <- split(samps, seq(nrow(samps)))

  # Fetch results for all samples in one query to more efficiently match parameters to aquacache codes
  results <- DBI::dbGetQuery(
    EQcon,
    paste0(
      "SELECT * FROM eqdetail WHERE SampleId IN (",
      paste0(samps$SampleId, collapse = ","),
      ");"
    )
  )

  if (nrow(results) == 0) {
    message("No new data found.")
    return(data.frame())
  }

  results_list <- split(results, results$SampleID)
  # Add results to samps_list
  samps_list <- lapply(
    samps_list,
    function(x) {
      x$results <- results_list[[as.character(x$SampleID)]]
      return(x)
    }
  )
}
