#' Bring EQWin water quality data into the aquacache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Brings in water quality data from the EQWin database. <DL values are transformed to the negative of the detection limit, > instrument range is left as the value. A note is added in either case to the 'note' column.
#'
#' @param location The location code (project code, i.e. the portion in parentheses) associated with the EQWin station.
#' @param sub_location The sub-location code (station code, i.e. the portion after the parentheses) associated with the EQWin station.
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param EQcon connection to the EQWin database. See EQConnect for details.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export

downloadEQWin <- function(location, sub_location, start_datetime, end_datetime = Sys.time(), EQcon = EQConnect(silent = TRUE)) {

  stop("This function has not yet been updated to work with the new (as of early August 2024) measurements_discrete table schema.")
  
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
  }, error = function(e){
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

  # Get the data
  StnId <- DBI::dbGetQuery(EQcon, paste0("SELECT StnId FROM eqstns WHERE StnCode = '", location, "';"))[1,1]
  # find the samples taken within the datetime range (there won't necessarily be reported values each time for the parameter in question)
  SampleIds <- DBI::dbGetQuery(EQcon, paste0("SELECT SampleId, CollectDateTime, SampleClass FROM eqsampls WHERE StnId = ", StnId, " AND CollectDateTime >= #",  substr(as.character(start_datetime), 1,19), "# AND CollectDateTime <= #", substr(as.character(end_datetime), 1,19), "#;"))
  if (nrow(SampleIds) > 0) {
    SampleIds$CollectDateTime <- lubridate::force_tz(SampleIds$CollectDateTime, "MST")
    ParamId <- DBI::dbGetQuery(EQcon, paste0("SELECT ParamId FROM eqparams WHERE ParamCode = '", parameter_id, "'"))[1,1]
    # Find the measurements for the parameter of interest. There won't necessarily be a measurement for each sample (nrow(meas) may be < nrow(sampleIds))
    meas <- DBI::dbGetQuery(EQcon, paste0("SELECT SampleId, Result FROM eqdetail WHERE SampleId IN (", paste(SampleIds$SampleId, collapse = ", "), ") AND ParamId = ", ParamId, ";"))
    if (nrow(meas) > 0) {
      result <- merge(meas, SampleIds)
      result <- result[ -which(names(result) == "SampleId")]
      names(result) <- c("value", "datetime", "sample_class")
      result$note <- NA
      result[grep("<", result$value), "note"] <- "Value below detection limit"
      result[grep(">", result$value), "note"] <- "Value above instrument range"
      #make <DL values the negative of the DL, remove > and leave the rest
      result$value <- as.numeric(gsub("<", "-", result$value))
      result$value <- as.numeric(gsub(">", "", result$value))
      result$sample_class[result$sample_class == "XX"] <- "U"
    } else {
      result <- data.frame()
    }
  }  else {
    result <- data.frame()
  }
  return(result)
}
