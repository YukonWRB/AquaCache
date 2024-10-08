#' Get realtime data from NWIS locations
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' A function used to fetch USGS data from their REST API. See details [here](https://waterservices.usgs.gov/rest/IV-Service.html). Unit conversions are performed to metric for common parameters like level, flow, temperature.
#'
#' @param location One ore more USGS station codes.
#' @param parameter_id One or more USGS parameter codes. 65 for instantaneous gauge level, 60 for mean daily flow, 61 for instantaneous flow (though beware, these two might be flipped), 10 for water temperature, for example; see more [here](https://help.waterdata.usgs.gov/codes-and-parameters/parameters).
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param modifiedSince Optional. A number of hours to narrow the request down to only data points modified within the last x hours. Default NULL fetches all data with the `start_datetime` and `end_datetime` range.
#'
#' @return A data.frame object of hydrometric data, with datetimes in UTC-0.
#' @export

downloadNWIS <- function(location, parameter_id, start_datetime, end_datetime = Sys.time(), modifiedSince = NULL)
{

  if (!inherits(parameter_id, "numeric")) {
    as.numeric(parameter_id)
  } else {
    parameter_id <- sprintf("%05d", parameter_id) #param codes always have 5 digits, but padded with leading zeros
  }
  if (inherits(location, "numeric")) { #location codes are always at least 8 digits, but if entering a numeric starting with 0s (some codes contain letters too) then these are not retained.
    location <- sprintf("%08d", location)
  }
  tryCatch({
    if (inherits(start_datetime, c("character", "Date"))) { #Either way defaults to 0 hour
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else if (inherits(start_datetime, "POSIXct")) {
      attr(start_datetime, "tzone") <- "UTC"
    } else {
      stop("Parameter start_datetime could not be coerced to POSIXct.")
    }
    if (nchar(start_datetime) == 10) {
      start_datetime <- paste0(start_datetime, " 00:00:00")
    }
  }, error = function(e) {
    stop("Failed to convert parameter start_datetime to POSIXct.")
  })
  tryCatch({
    if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) { #Does not necessarily default to 0 hour.
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "POSIXct")) {
      attr(end_datetime, "tzone") <- "UTC"
    } else if (inherits(end_datetime, "Date") | (inherits(end_datetime, "character") & nchar(end_datetime) == 10)) {
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      end_datetime <- end_datetime + 60*60*23.9999
    } else {
      stop("Parameter end_datetime could not be coerced to POSIXct.")
    }
    if (nchar(end_datetime) == 10) {
      end_datetime <- paste0(end_datetime, " 00:00:00")
    }
  }, error = function(e) {
    stop("Failed to convert parameter end_datetime to POSIXct.")
  })

  tryCatch({ # readNWISdata returns throws an error if there are no rows returned (if run in absence of new data).
    if (!is.null(modifiedSince)) {
      data <- dataRetrieval::readNWISdata(sites = location,
                                          service = "iv",
                                          parameterCd = parameter_id,
                                          startDate =  paste0(substr(start_datetime, 1, 10), "T", substr(start_datetime, 12,16), "z"),
                                          endDate = paste0(substr(end_datetime, 1, 10), "T", substr(end_datetime, 12,16), "z"),
                                          modifiedSince = modifiedSince,
                                          asDateTime = TRUE,
                                          tz = "UTC",
                                          convertType = TRUE)[, c(3:5)]
    } else {
      data <-  dataRetrieval::readNWISdata(sites = location,
                                           service = "iv",
                                           parameterCd = parameter_id,
                                           startDate = paste0(substr(start_datetime, 1, 10), "T", substr(start_datetime, 12,16), "z"),
                                           endDate = paste0(substr(end_datetime, 1, 10), "T", substr(end_datetime, 12,16), "z"),
                                           asDateTime = TRUE,
                                           tz = "UTC",
                                           convertType = TRUE)[, c(3:5)]
    }
    if (nrow(data) > 0) {
      colnames(data) <- c("datetime", "value", "combined")
      data <- data[!is.na(data$value) , ]
      if (parameter_id == "00011") { #temp in F into C
        data$value <- (data$value - 32) / 1.8
      } else if (parameter_id %in% c("00060", "00061")) { #flow in ft3/s into m3/s
        data$value <- data$value * 0.028316832
      } else if (parameter_id %in% c("00065", "62610", "62611", "72150")) { #levels in ft into meters
        data$value <- data$value * 0.3048
      }

      #Extract first capital letter, which is the approval
      data$approval <- gsub("^([APR]).*", "\\1", data$combined)
      approval_mapping <- c("A" = "A",
                            "P" = "N",
                            "R" = "A")
      data$approval <- ifelse(data$approval %in% names(approval_mapping),
                              approval_mapping[data$approval],
                              "Z")
      
      #After that it's the grade, maybe.Anything that's not clearly a grade gets Z, unknown
      data$grade <- trimws(gsub("^[APR](.*)", "\\1", data$combined))
      data$grade[data$grade == ""] <- "U"
      grade_mapping <- c("e" = "E",
                         "ice" = "I",
                         "Ice i" = "I",
                         "<" = "E",
                         ">" = "E")
      data$grade <- ifelse(data$grade %in% names(grade_mapping),
                           grade_mapping[data$grade],
                           "Z")

      data <- data[, -which(names(data) == "combined")]
      return(data)
    } else {
      data <- data.frame()
      return(data)
    }
  }, error = function(e) {
    data <- data.frame()
    return(data)
  })

}
