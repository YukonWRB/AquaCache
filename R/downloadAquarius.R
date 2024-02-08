#' Get formatted timeseries data from Aquarius.
#'
#'@description
#' `r lifecycle::badge("stable")`
#'
#' Pared-down and modified version of YGWater::aq_download.
#'
#' ##Passwords:
#' To store login credentials in your .renviron file call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS="your password". The server should be entered at server="your_server_url". You can also store credentials in the timeseries table in the column source_fx_args, but beware that these credentials are then sitting in the database un-encrypted.

#'
#' @param location The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`.
#' @param param_code The timeseries name, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `Wlevel_bgs.Calculated`.
#' @param start_datetime The first day or instant for which you want information. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying a POSIXct the UTC offset associated with the time will be used, otherwise UTC 0 will be assumed. If only a date is specified it will be assigned the first moment of the day. Times requested prior to the actual timeseries start will be adjusted to match available data.
#' @param end_datetime The last day or instant for which you want information. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying a POSIXct the UTC offset associated with the time will be used, otherwise UTC 0 will be assumed. If only a date is specified it will be assigned the last moment of the day. Times requested prior to the actual timeseries end will be adjusted to match available data.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron file; see details.
#' @param server The URL for your organization's Aquarius web server. Default pulls from your .renviron file; see details.
#'
#' @return A data.frame of timeseries data with columns for datetime, value, grade, approval.
#'
#' @export

downloadAquarius <- function(location,
                             param_code,
                             start_datetime,
                             end_datetime = Sys.Date(),
                             login = Sys.getenv(c("AQUSER", "AQPASS")),
                             server = Sys.getenv("AQSERVER")
)
{
  #Check that login and server credentials exist
  if (nchar(server) == 0 | is.null(server)) {
    stop("downloadAquarius: It looks like you haven't provided a server, or that it can't be found in your .Renviron file if you left the function defaults.")
  }
  if (nchar(login[1]) == 0 | is.null(login[1])) {
    stop("downloadAquarius: It looks like you haven't provided a username, or that it can't be found in your .Renviron file if you left the function defaults.")
  }
  if (nchar(login[2]) == 0 | is.null(login[2])) {
    stop("downloadAquarius: It looks like you haven't provided a password, or that it can't be found in your .Renviron file if you left the function defaults.")
  }

  source(system.file("scripts",  "timeseries_client.R", package = "HydroMetDB")) #This loads the code dependencies

  #Make the Aquarius configuration
  config = list(
    server = server,
    username = login[1],
    password = login[2],
    timeSeriesName = paste0(param_code, "@", location)
  )

  # Connect to Aquarius server
  timeseries$connect(config$server,
                     config$username,
                     config$password)
  on.exit(timeseries$disconnect())

  if (inherits(start_datetime, "POSIXct")) {
    attr(start_datetime, "tzone") <- "UTC"
  }
  start <- as.character(start_datetime)
  if (nchar(start) == 10) {
    start <- paste0(start, " 00:00:00")
  }
  start <- gsub(" ", "T", start)
  start <- paste0(start, "-00:00")

  if (inherits(end_datetime, "POSIXct")) {
    attr(end_datetime, "tzone") <- "UTC"
  }
  end <- as.character(end_datetime)
  if (nchar(end) == 10) {
    end <- paste0(end, " 23:59:59.9999999")
  }
  end <- gsub(" ", "T", end)
  end <- paste0(end, "-00:00")

  # Read corrected time-series data from Aquarius, format time series to POSIXct
  RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName), queryFrom = start, queryTo = end)

  #Make the basic timeseries
  ts <- data.frame(datetime = RawDL$Points$Timestamp,
                   value = RawDL$Points$Value$Numeric)
  ts <- ts[!is.na(ts$value) , ]

  if (nrow(ts) > 0) {
    # format times to POSIXct, fix offset
    offset <- substr(ts$datetime[1], nchar(ts$datetime[1]) - 5, nchar(ts$datetime[1]))
    offset <- gsub(":", "", offset)
    ts$datetime <- paste0(substr(ts$datetime, 1, nchar(ts$datetime) - 6), offset)
    ts$datetime <- as.POSIXct(ts$datetime, format = "%Y-%m-%dT%H:%M:%OS%z")

    #format approvals, grade times
    approvals <- RawDL$Approvals[, -which(names(RawDL$Approvals) %in% c("DateAppliedUtc", "User", "Comment", "LevelDescription"))]
    stoffset <- substr(approvals$StartTime[1], nchar(approvals$StartTime[1]) - 5, nchar(approvals$StartTime[1]))
    stoffset <- gsub(":", "", stoffset)
    approvals$StartTime <- paste0(substr(approvals$StartTime, 1, nchar(approvals$StartTime) - 6), stoffset)
    approvals$StartTime <- as.POSIXct(approvals$StartTime, format = "%Y-%m-%dT%H:%M:%OS%z")
    endoffset <- substr(approvals$EndTime[1], nchar(approvals$EndTime[1]) - 5, nchar(approvals$EndTime[1]))
    endoffset <- gsub(":", "", endoffset)
    approvals$EndTime <- paste0(substr(approvals$EndTime, 1, nchar(approvals$EndTime) - 6), endoffset)
    approvals$EndTime <- as.POSIXct(approvals$EndTime, format = "%Y-%m-%dT%H:%M:%OS%z")

    colnames(approvals) <- c("level", "start_time", "end_time")

    approval_mapping <- c("800" = "N",
                         "900" = "C",
                         "950" = "C",
                         "975" = "R",
                         "1200" = "A",
                         "1300" = "A")
    approvals$level <- ifelse(as.character(approvals$level) %in% names(approval_mapping),
                                    approval_mapping[as.character(approvals$level)],
                                    "Z")

    grades <- RawDL$Grades
    stoffset <- substr(grades$StartTime[1], nchar(grades$StartTime[1]) - 5, nchar(grades$StartTime[1]))
    stoffset <- gsub(":", "", stoffset)
    grades$StartTime <- paste0(substr(grades$StartTime, 1, nchar(grades$StartTime) - 6), stoffset)
    grades$StartTime <- as.POSIXct(grades$StartTime, format = "%Y-%m-%dT%H:%M:%OS%z")
    endoffset <- substr(grades$EndTime[1], nchar(grades$EndTime[1]) - 5, nchar(grades$EndTime[1]))
    endoffset <- gsub(":", "", endoffset)
    grades$EndTime <- paste0(substr(grades$EndTime, 1, nchar(grades$EndTime) - 6), endoffset)
    grades$EndTime <- as.POSIXct(grades$EndTime, format = "%Y-%m-%dT%H:%M:%OS%z")

    colnames(grades) <- c("level", "start_time", "end_time")

    grade_mapping <- c("-55" = "R",
                       "-50" = "U",
                       "-y" = "D",
                       "-6" = "E",
                       "-5" = "N",
                       "-4" = "C",
                       "-3" = "E",
                       "-2" = "N",
                       "-1" = "U",
                       "0" = "U",
                       "1" = "I",
                       "2" = "E",
                       "3" = "C",
                       "4" = "B",
                       "5" = "A",
                       "10" = "U",
                       "11" = "U",
                       "12" = "D",
                       "14" = "C",
                       "15" = "B",
                       "21" = "N",
                       "30" = "N",
                       "31" = "N",
                       "99" = "N",
                       "100" = "N",
                       "101" = "U",
                       "103" = "U",
                       "105" = "D",
                       "110" = "E",
                       "115" = "E",
                       "120" = "S",
                       "124" = "U",
                       "125" = "B",
                       "130" = "A")
    grades$level <- ifelse(as.character(grades$level) %in% names(grade_mapping),
                              grade_mapping[as.character(grades$level)],
                              "Z")

    #Add in grades and approval columns
    ts <- ts[!duplicated(ts) , ] #In unknown circumstances, Aquarius spits out duplicate points.
    ts$grade <- NA
    for (i in 1:nrow(grades)) {
      if (min(ts$datetime) > grades$start_time[i]) { #if the grade is prior to the first ts point
        ts[ts$datetime == min(ts$datetime), "grade"] <- grades$level[i]
      } else if (nrow(ts[ts$datetime == grades$start_time[i],]) != 0) { #if the times line up properly (are snapped to a point)
        ts[ts$datetime == grades$start_time[i], "grade"] <- grades$level[i]
      } else if (which.min(abs(ts$datetime - grades$start_time[i])) != nrow(ts)) { #if the times do not line up with anything in ts (not snapped), but not after the ts end
        index <- which.min(abs(ts$datetime - grades$start_time[i])) + 1
        ts[index, "grade"] <- grades$level[i]
      } # and if the last grade start is after then end of the ts, do nothing with it!
    }

    ts$approval <- NA
    for (i in 1:nrow(approvals)) {
      if (min(ts$datetime) > approvals$start_time[i]) { #if the approval is prior to the first ts point
        ts[ts$datetime == min(ts$datetime),]$approval <- approvals$level[i]
      } else if (nrow(ts[ts$datetime == approvals$start_time[i],]) != 0) { #if the times line up properly (are snapped to a point)
        ts[ts$datetime == approvals$start_time[i],]$approval <- approvals$level[i]
      } else if (which.min(abs(ts$datetime - approvals$start_time[i])) != nrow(ts)) { #if the times do not line up with anything in ts (not snapped), but not after the ts end
        index <- which.min(abs(ts$datetime - approvals$start_time[i])) + 1
        ts[index,]$approval <- approvals$level[i]
      } # and if the last approval start is after then end of the ts, do nothing with it!
    }
    ts <- tidyr::fill(ts, c("grade", "approval"), .direction = "down")
    attr(ts$datetime, "tzone") <- "UTC"
    return(ts)
  } else {
    ts <- data.frame()
    return(ts)
  }

}
