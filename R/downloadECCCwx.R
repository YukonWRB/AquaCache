#' Get realtime data from ECCC met locations (observations)
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' A function used to fetch weather data from ECCC, using the weathercan package for speed and simplicity. Since ECCC weather data comes in as a tibble with ~36 rows (all parameters)and there is no way to tailor the request to a single parameter, this function will save the output of the first download as an .rdata file to the session temporary folder. Subsequent runs of the function will search the temporary folder for a suitable file and attempt to use it, downloading again only if no suitable file is found. Temporary folder contents are deleted when the R session is closed.
#'
#' @param location An ECCC Station ID (not to be mistaken for other IDs such as the Nav Canada ID, the WMO ID, or the Climate ID). See [weathercan::stations()] for help finding the right ID.
#' @param parameter_id The name of the column containing the desired data, as output in the data.frame given by [weathercan::weather_dl()]. Taken from the parameter_id column of the 'settings' table. Note that this column name varies depending on the interval specified (hour, day, month).
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param interval The interval to pass to [weathercan::weather_dl()], one of "hour", "day", "month".
#'
#' @return A data.frame of hydrometric data, with datetimes in UTC-0.
#' @export


downloadECCCwx <- function(location, parameter_id, start_datetime, end_datetime = Sys.time(), interval)
{

  # Checking start_datetime parameter
  tryCatch({
    if (inherits(start_datetime, "character") & nchar(start_datetime) > 10) { #Does not necessarily default to 0 hour.
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else if (inherits(start_datetime, "POSIXct")) {
      attr(start_datetime, "tzone") <- "UTC"
    } else if (inherits(start_datetime, "Date") | (inherits(start_datetime, "character") & nchar(start_datetime) == 10)) { #defaults to 0 hour
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else {
      stop("Parameter start_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter start_datetime to POSIXct.")
  })

  # Checking end_datetime parameter
  tryCatch({
    if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) { #Does not necessarily default to 0 hour.
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "POSIXct")) {
      attr(end_datetime, "tzone") <- "UTC"
    } else if (inherits(end_datetime, "Date") | (inherits(end_datetime, "character") & nchar(end_datetime) == 10)) { #defaults to very end of day
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      end_datetime <- end_datetime + 60*60*23.9999
    } else {
      stop("Parameter end_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter end_datetime to POSIXct.")
  })

  # Check if there already exists a temporary file with the required interval, location, start_datetime, and end_datetime.
  files <- list.files(paste0(tempdir(), "/downloadECCCwx"))

  if (length(files) == 0) {
    file_exists <- FALSE
  } else {
    name <- paste0(location, "_", interval, "_", substr(start_datetime, 1, 10), "_", substr(end_datetime, 1, 10), ".rdata")
    if (name %in% files) {
     load(paste0(tempdir(), "/downloadECCCwx/", location, "_", interval, "_", substr(start_datetime, 1, 10), "_", substr(end_datetime, 1, 10), ".rdata"))
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
  }

  # If there is no file that matches necessary use, download
  if (!file_exists) {
    dl <- suppressMessages(weathercan::weather_dl(location, start = start_datetime, interval = interval, time_disp = "UTC", quiet = TRUE))
    #Save the file to the tempdir, from which it will be deleted once the R session ends
    dir.create(paste0(tempdir(), "/downloadECCCwx"), showWarnings = FALSE)
    save(dl, file = paste0(tempdir(), "/downloadECCCwx/", location, "_", interval, "_", substr(start_datetime, 1, 10), "_", substr(end_datetime, 1, 10), ".rdata"))
  }

  #Extract the necessary information according to the parameter_id
  if (nrow(dl) > 0) {
    if ("time" %in% names(dl)) { # then it must be hourly
      data <- data.frame(datetime = dl$time,
                         value = dl[[parameter_id]]) #Note the different subsetting because dl is a tibble.
      data <- data[data$datetime > start_datetime & data$datetime < end_datetime & !is.na(data$value) , ]
      if (nrow(data) > 0) {
        data$grade <- 9 #Undefined
        data$approval <- 5 #Undefined
      }
    } else if (("date" %in% names(dl)) & !("time" %in% names(dl))) { #Must be daily or more
      data <- data.frame(datetime = as.POSIXct(dl$date, tz = "UTC") + 30*60*60,  #Observations building daily values end at 6 UTC on following day (so values reported on the 24th include hours 07 to 23 on the 24th plus 00 to 06 on the 25th)
                         value = dl[[parameter_id]]) #Note the different subsetting because dl is a tibble.
      data <- data[data$datetime > start_datetime & data$datetime < end_datetime & !is.na(data$value) , ]
      if (nrow(data) > 0) {
        data$grade <- 9 #Undefined
        data$approval <- 5 #Undefined
      }
    } else {
      stop("downloadECCCwx: Column named 'time' or 'date' has not been found.")
    }
  } else {
    data <- data.frame()
  }

  return(data)
}

