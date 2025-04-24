#' Get formatted timeseries data from Aquarius.
#'
#'@description
#' `r lifecycle::badge("stable")`
#'
#' Pared-down and modified version of YGWater::aq_download.
#' ## Passwords and server credentials:
#' To store login credentials in your .renviron file call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS="your password". The server should be entered at server="your_server_url". You can also store credentials in the timeseries table in the column source_fx_args, but beware that these credentials are then sitting in the database un-encrypted.
#' ## Grades, approvals, qualifiers:
#' This function will attempt to map Aquarius grades, approvals, and qualifiers to the database defaults. The function was designed around the Yukon Water Resources Branch Aquarius schemes, but 'forks' can be implemented for each based on the server URL. If you need to implement a fork, see the commented-out example in the function at line 116.
#'
#' @param location The location ID, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `29EA001` or `YOWN-0804`.
#' @param parameter The timeseries name, exactly as visible in Aquarius web portal, as a character vector of length 1. Typically of form `Wlevel_bgs.Calculated`.
#' @param start_datetime The first day or instant for which you want information. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying a POSIXct the UTC offset associated with the time will be used, otherwise UTC 0 will be assumed. If only a date is specified it will be assigned the first moment of the day. Times requested prior to the actual timeseries start will be adjusted to match available data.
#' @param end_datetime The last day or instant for which you want information. You can specify a Date object, POSIXct object, or character vector of form yyyy-mm-dd or yyyy-mm-dd HH:mm:ss. If specifying a POSIXct the UTC offset associated with the time will be used, otherwise UTC 0 will be assumed. If only a date is specified it will be assigned the last moment of the day. Times requested prior to the actual timeseries end will be adjusted to match available data.
#' @param login Your Aquarius login credentials as a character vector of two. Default pulls information from your .renviron file; see details.
#' @param server The URL for your organization's Aquarius web server. Default pulls from your .renviron file; see details.
#' @param con A connection to the aquacache database, necessary to allow for the mapping of Aquarius approvals, grades, and qualifiers to the database. If left NULL connection will be made and closed automatically.
#'
#' @return A data.frame of timeseries data with columns for datetime, value, grade, approval.
#'
#' @export

downloadAquarius <- function(location,
                             parameter,
                             start_datetime,
                             end_datetime = Sys.Date(),
                             login = Sys.getenv(c("AQUSER", "AQPASS")),
                             server = Sys.getenv("AQSERVER"),
                             con = NULL
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

  source(system.file("scripts",  "timeseries_client.R", package = "AquaCache")) #This loads the code dependencies

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  #Make the Aquarius configuration
  config = list(
    server = server,
    username = login[1],
    password = login[2],
    timeSeriesName = paste0(parameter, "@", location)
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
    ts$datetime <- as.POSIXct(ts$datetime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")

    #format approvals, grade, qualifiers times
    approvals_DB <- DBI::dbGetQuery(con, "SELECT * FROM approval_types")
    if (is.null(nrow(RawDL$Approvals)) || nrow(RawDL$Approvals) == 0) {  # Then it's probably an empty list or data.frame because there are no approvals
      approvals <- data.frame(level = approvals_DB[approvals_DB$approval_type_code == "UNS", "approval_type_id"], start_time = min(ts$datetime), end_time = max(ts$datetime))
    } else {
      approvals <- RawDL$Approvals[, c("ApprovalLevel", "StartTime", "EndTime")]
      stoffset <- substr(approvals$StartTime[1], nchar(approvals$StartTime[1]) - 5, nchar(approvals$StartTime[1]))
      stoffset <- gsub(":", "", stoffset)
      approvals$StartTime <- paste0(substr(approvals$StartTime, 1, nchar(approvals$StartTime) - 6), stoffset)
      approvals$StartTime <- as.POSIXct(approvals$StartTime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      endoffset <- substr(approvals$EndTime[1], nchar(approvals$EndTime[1]) - 5, nchar(approvals$EndTime[1]))
      endoffset <- gsub(":", "", endoffset)
      approvals$EndTime <- paste0(substr(approvals$EndTime, 1, nchar(approvals$EndTime) - 6), endoffset)
      approvals$EndTime <- as.POSIXct(approvals$EndTime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      
      colnames(approvals) <- c("level", "start_time", "end_time")
      approval_mapping <- c("800" = approvals_DB[approvals_DB$approval_type_code == "N", "approval_type_id"],
                            "900" = approvals_DB[approvals_DB$approval_type_code == "C", "approval_type_id"],
                            "950" = approvals_DB[approvals_DB$approval_type_code == "C", "approval_type_id"],
                            "975" = approvals_DB[approvals_DB$approval_type_code == "A", "approval_type_id"],
                            "1200" = approvals_DB[approvals_DB$approval_type_code == "A", "approval_type_id"],
                            "1300" = approvals_DB[approvals_DB$approval_type_code == "A", "approval_type_id"])
      
      approvals$level <- ifelse(as.character(approvals$level) %in% names(approval_mapping),
                                approval_mapping[as.character(approvals$level)],
                                approvals_DB[approvals_DB$approval_type_code == "UNK", "approval_type_id"])
    }
    
    grades_DB <- DBI::dbGetQuery(con, "SELECT * FROM grade_types")
    if (is.null(nrow(RawDL$Grades)) || nrow(RawDL$Grades) == 0) {  # Then it's probably an empty list or data.frame because there are no grades
      grades <- data.frame(level = grades_DB[grades_DB$grade_type_code == "UNS", "grade_type_id"], start_time = min(ts$datetime), end_time = max(ts$datetime))
    } else {
      grades <- RawDL$Grades[, c("GradeCode", "StartTime", "EndTime")]
      stoffset <- substr(grades$StartTime[1], nchar(grades$StartTime[1]) - 5, nchar(grades$StartTime[1]))
      stoffset <- gsub(":", "", stoffset)
      grades$StartTime <- paste0(substr(grades$StartTime, 1, nchar(grades$StartTime) - 6), stoffset)
      grades$StartTime <- as.POSIXct(grades$StartTime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      endoffset <- substr(grades$EndTime[1], nchar(grades$EndTime[1]) - 5, nchar(grades$EndTime[1]))
      endoffset <- gsub(":", "", endoffset)
      grades$EndTime <- paste0(substr(grades$EndTime, 1, nchar(grades$EndTime) - 6), endoffset)
      grades$EndTime <- as.POSIXct(grades$EndTime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      
      colnames(grades) <- c("level", "start_time", "end_time")
      grade_mapping <- c("0" = grades_DB[grades_DB$grade_type_code == "UNS", "grade_type_id"],
                         "-5" = grades_DB[grades_DB$grade_type_code == "MISS", "grade_type_id"],
                         "-3" = grades_DB[grades_DB$grade_type_code == "E", "grade_type_id"],
                         "-2" = grades_DB[grades_DB$grade_type_code == "N", "grade_type_id"],
                         "-1" = grades_DB[grades_DB$grade_type_code == "UNS", "grade_type_id"],
                         "5" = grades_DB[grades_DB$grade_type_code == "A", "grade_type_id"],
                         "4" = grades_DB[grades_DB$grade_type_code == "B", "grade_type_id"],
                         "3" = grades_DB[grades_DB$grade_type_code == "C", "grade_type_id"],
                         "2" = grades_DB[grades_DB$grade_type_code == "D", "grade_type_id"],
                         "12" = grades_DB[grades_DB$grade_type_code == "D", "grade_type_id"],
                         "14" = grades_DB[grades_DB$grade_type_code == "B", "grade_type_id"],
                         "15" = grades_DB[grades_DB$grade_type_code == "A", "grade_type_id"],
                         "21" = grades_DB[grades_DB$grade_type_code == "C", "grade_type_id"],
                         "30" = grades_DB[grades_DB$grade_type_code == "B", "grade_type_id"],
                         "31" = grades_DB[grades_DB$grade_type_code == "B", "grade_type_id"])
      grades$level <- ifelse(as.character(grades$level) %in% names(grade_mapping),
                             grade_mapping[as.character(grades$level)],
                             grades_DB[grades_DB$grade_type_code == "UNK", "grade_type_id"])
    }
    
    qualifiers_DB <- DBI::dbGetQuery(con, "SELECT * FROM qualifier_types")
    if (is.null(nrow(RawDL$Qualifiers)) || nrow(RawDL$Qualifiers) == 0) {  # Then it's probably an empty list or data.frame because there are no qualifiers
      qualifiers <- data.frame(level = qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNS", "qualifier_type_id"], start_time = min(ts$datetime), end_time = max(ts$datetime))
    } else {
      qualifiers <- RawDL$Qualifiers[, c("Identifier", "StartTime", "EndTime")]
      stoffset <- substr(qualifiers$StartTime[1], nchar(qualifiers$StartTime[1]) - 5, nchar(qualifiers$StartTime[1]))
      stoffset <- gsub(":", "", stoffset)
      qualifiers$StartTime <- paste0(substr(qualifiers$StartTime, 1, nchar(qualifiers$StartTime) - 6), stoffset)
      qualifiers$StartTime <- as.POSIXct(qualifiers$StartTime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      endoffset <- substr(qualifiers$EndTime[1], nchar(qualifiers$EndTime[1]) - 5, nchar(qualifiers$EndTime[1]))
      endoffset <- gsub(":", "", endoffset)
      qualifiers$EndTime <- paste0(substr(qualifiers$EndTime, 1, nchar(qualifiers$EndTime) - 6), endoffset)
      qualifiers$EndTime <- as.POSIXct(qualifiers$EndTime, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")
      
      colnames(qualifiers) <- c("level", "start_time", "end_time")
      qualifier_mapping <- c("BKW" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "BW", "qualifier_type_id"],
                             "DD" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "DD", "qualifier_type_id"],
                             "DRY" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "DRY", "qualifier_type_id"],
                             "E" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "EST", "qualifier_type_id"],
                             "ES" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "SUS", "qualifier_type_id"],
                             "FI" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "INT", "qualifier_type_id"],
                             "HW-MISS" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "HW-MISS", "qualifier_type_id"],
                             "ICE" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "ICE", "qualifier_type_id"],
                             "ICE-EST" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "ICE-EST", "qualifier_type_id"],
                             "LW-MISS" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "LW-MISS", "qualifier_type_id"],
                             "OOW" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "OOW", "qualifier_type_id"],
                             "PMMAX" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "PMMAX", "qualifier_type_id"],
                             "PMMIN" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "PMMIN", "qualifier_type_id"],
                             "PYMAX" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "PYMAX", "qualifier_type_id"],
                             "PYMIN" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "PYMIN", "qualifier_type_id"],
                             "REL" = qualifiers_DB[qualifiers_DB$qualifier_type_code == "REL", "qualifier_type_id"])
      qualifiers$level <- ifelse(as.character(qualifiers$level) %in% names(qualifier_mapping),
                                 qualifier_mapping[as.character(qualifiers$level)],
                                 qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNK", "qualifier_type_id"])
    }
    
    #Add in grades, approval, and qualifier columns
    ts <- ts[!duplicated(ts) , ] #In unknown circumstances, Aquarius spits out duplicate points.
    
    ts <- ts[order(ts$datetime), ]
    
    # Get the row indices in ts corresponding to qualifier/approval/grade start and end times.
    # For each row, if the time is before ts_min, use the first index.
    # If it exactly matches a ts time, use that index (found via match).
    # Otherwise, use findInterval (and ensure the index isn’t beyond ts).
    apply_intervals <- function(df, 
                                intervals,
                                new_col,
                                time_col = "datetime",
                                start_col = "start_time",
                                end_col   = "end_time") {

      ts_min <- min(df[[time_col]])
      n_ts   <- nrow(df)
      
      # Calculate start indices
      start_idx <- ifelse(
        intervals[[start_col]] < ts_min,
        1,
        ifelse(
          intervals[[start_col]] %in% df[[time_col]],
          match(intervals[[start_col]], df[[time_col]]),
          pmin(findInterval(intervals[[start_col]], df[[time_col]]), n_ts)
        )
      )
      
      # Calculate end indices
      end_idx <- ifelse(
        intervals[[end_col]] < ts_min,
        1,
        ifelse(
          intervals[[end_col]] %in% df[[time_col]],
          match(intervals[[end_col]], df[[time_col]]),
          pmin(findInterval(intervals[[end_col]], df[[time_col]]), n_ts)
        )
      )
      
      # Initialize (or reset) the qualifier/approval/grade column
      df[[new_col]] <- NA
      
      # Loop through each interval and assign the qualifier
      for (i in seq_along(start_idx)) {
        st <- df[[time_col]][start_idx[i]]
        ed <- df[[time_col]][end_idx[i]]
        sel <- df[[time_col]] >= st & df[[time_col]] <= ed
        
        # append if already present, otherwise assign
        df[[new_col]][sel] <- ifelse(
          is.na(df[[new_col]][sel]),
          intervals$level[i],
          paste(df[[new_col]][sel], intervals$level[i], sep = ",")
        )
      }
      return(df)
    }
    
    ts <- apply_intervals(ts, approvals, "approval")
    ts <- apply_intervals(ts, grades, "grade")
    ts <- apply_intervals(ts, qualifiers, "qualifier")
    
    # Replace NA values in grade, approval, and qualifier with the unspecified values
    ts$grade[is.na(ts$grade)] <- grades_DB[grades_DB$grade_type_code == "UNS", "grade_type_id"]
    ts$approval[is.na(ts$approval)] <- approvals_DB[approvals_DB$approval_type_code == "UNS", "approval_type_id"]
    ts$qualifier[is.na(ts$qualifier)] <- qualifiers_DB[qualifiers_DB$qualifier_type_code == "UNS", "qualifier_type_id"]
    
    return(ts)
  } else {
    ts <- data.frame()
    return(ts)
  }

}
