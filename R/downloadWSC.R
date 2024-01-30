#' Get realtime data from the WSC
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' A fast, pared down method of fetching WSC realtime data (at least compared to tidyhydat and tidyhydat.ws options). Dispenses with extra columns that those packages include and uses data.table::fread to speed up parsing.
#'
#' @param location A WSC station number.
#' @param param_code A WSC parameter code. 47 for discharge primary (sensor derived), 8 for discharge (sensor measured), 46 for level, 5 for water temperature, 4 for air temperature. See the full list using [tidyhydat::param_id].
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#'
#' @return A data.table object of hydrometric data, with datetimes in UTC-0.
#' @export

downloadWSC <- function (location, param_code, start_datetime, end_datetime = Sys.time())
{
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

  if (nchar(as.character(start_datetime)) == 10){
    start_datetime <- paste0(start_datetime, " 00:00:00")
  }
  if (nchar(as.character(end_datetime)) == 10) {
    end_datetime <- paste0(end_datetime, " 00:00:00")
  }

  # Pull data from WSC
  baseurl <- "https://wateroffice.ec.gc.ca/services/real_time_data/csv/inline?"
  location_string <- paste0("stations[]=", location)
  parameters_string <- paste0("parameters[]=", param_code)
  datetime_string <- paste0("start_date=", substr(start_datetime, 1, 10), "%20", substr(start_datetime, 12, 19), "&end_date=", substr(end_datetime, 1, 10), "%20", substr(end_datetime, 12, 19))
  url <- paste0(baseurl, location_string, "&", parameters_string, "&", datetime_string)

  data <- data.table::fread(url, showProgress = FALSE, data.table = FALSE, select = c("Date", "Value/Valeur", "Symbol/Symbole", "Approval/Approbation"), col.names = c("datetime", "value", "grade", "approval"))

} #End of function
