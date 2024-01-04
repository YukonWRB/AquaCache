#' Bring EQWin water quality data into the hydromet database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Brings in water quality data from the EQWin database.
#'
#' @param location The location code associated with the snow course.
#' @param param_code The parameter code as specified in the EQWin table eqparams.
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export


# location <- "YOWN-0101"
# param_code <- "U-D"
# start_datetime <- Sys.time()-60*60*24*20000
# end_datetime <- Sys.time()

getEQWin <- function(location, param_code, start_datetime, end_datetime = Sys.time()) {

  # Checking start_datetime parameter
  tryCatch({
    if (inherits(start_datetime, "character") & nchar(start_datetime) > 10){ #Does not necessarily default to 0 hour.
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else if (inherits(start_datetime, "POSIXct")){
      attr(start_datetime, "tzone") <- "UTC"
    } else if (inherits(start_datetime, "Date") | (inherits(start_datetime, "character") & nchar(start_datetime) == 10)){ #defaults to 0 hour
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else {
      stop("Parameter start_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e){
    stop("Failed to convert parameter start_datetime to POSIXct.")
  })

  # Checking end_datetime parameter
  tryCatch({
    if (inherits(end_datetime, "character") & nchar(end_datetime) > 10){ #Does not necessarily default to 0 hour.
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "POSIXct")){
      attr(end_datetime, "tzone") <- "UTC"
    } else if (inherits(end_datetime, "Date") | (inherits(end_datetime, "character") & nchar(end_datetime) == 10)){ #defaults to very end of day
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      end_datetime <- end_datetime + 60*60*23.9999
    } else {
      stop("Parameter end_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter end_datetime to POSIXct.")
  })

  # Set EQWin database connection
  EQcon <- EQConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(EQcon))

  # Get the data

  StnId <- DBI::dbGetQuery(EQcon, paste0("SELECT StnId FROM eqstns WHERE StnCode = '", location, "';"))[1,1]
  SampleIds <- DBI::dbGetQuery(EQcon, paste0("SELECT SampleId, CollectDateTime, SampleClass FROM eqsampls WHERE StnId = ", StnId, " AND CollectDateTime >= #",  substr(as.character(start_datetime), 1,19), "# AND CollectDateTime <= #", substr(as.character(end_datetime), 1,19), "#;"))
  SampleIds$CollectDateTime <- lubridate::force_tz(SampleIds$CollectDateTime, "MST")
  ParamId <- DBI::dbGetQuery(EQcon, paste0("SELECT ParamId FROM eqparams WHERE ParamCode = '", param_code, "'"))[1,1]
  samps <- DBI::dbGetQuery(EQcon, paste0("SELECT SampleId, Result FROM eqdetail WHERE SampleId IN (", paste(SampleIds$SampleId, collapse = ", "), ") AND ParamId = ", ParamId, ";"))

  if (nrow(samps) > 0){
    result <- merge(samps, SampleIds)
    result <- result[ -1]
    names(result) <- c("value", "datetime", "sample_class")
    #make <DL values the negative of the DL
    result$value <- as.numeric(gsub("<", "-", result$value))
  } else {
    result <- data.frame()
  }
  return(result)
}
