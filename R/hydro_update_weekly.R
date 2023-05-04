#' Weekly update of hydro database
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved". Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param locations The locations you wish to have updated as a character vector. Defaults to "all", though the meaning of all is dependent on the parameter 'aquarius'. Will search for all timeseries with the specified location codes, so level + flow, snow depth + SWE, etc.
#' @param WSC_range The starting date from which to pull real-time WSC data from the web and replace in the local database. Default is max possible days.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next two parameters. FALSE will only replace WSC data.
#' @param aquarius_range Should only unapproved (unlocked) data be replaced, or all available data? Select from "all" or "unapproved". Default is "unapproved".
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return Updated entries in the hydro database.
#' @import tidyhydat.ws
#' @export
#'


hydro_update_weekly <- function(path, WSC_range = Sys.Date()-577, aquarius = TRUE, aquarius_range = "unapproved", server = "https://yukon.aquaticinformatics.net/AQUARIUS")
{

  if (!(aquarius_range %in% c("all", "unapproved"))){
    stop("The parameter aquarius_range must be either 'all' or 'unapproved'")
  }

  if (aquarius){
    if (is.null(Sys.getenv("AQPASS"))){
      stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
    }
    if (is.null(Sys.getenv("AQUSER"))){
      stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
    }
  }
  if (is.null(Sys.getenv("WS_USRNM"))){
    stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
  }
  if (is.null(Sys.getenv("WS_PWD"))){
    stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
  }

  hydro <- WRBtools::hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(hydro))

  if (aquarius){
    aq_names <- DBI::dbGetQuery(hydro, "SELECT parameter, value FROM settings WHERE application  = 'aquarius'")
  }

  if (locations == "all"){
    all_timeseries <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM timeseries WHERE type = 'continuous' AND location IN ('", paste(locations, collapse = "', '"), "')"))
  }

  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    operator <- all_timeseries$operator[i]

    tryCatch({
      ts <- data.frame() #Make empty df here so that if (nrow(ts) > 0) works as intended later.
      if (operator == "WRB" & aquarius){
        ts_name <- aq_names[aq_names$parameter == parameter, 2]
        if (aquarius_range == "unapproved"){
          realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM realtime WHERE parameter = '", parameter, "' AND location = '", all_timeseries$location[i], "' AND NOT approval = 'approved'"))
          first_unapproved <- min(realtime$datetime_UTC)
          data <- WRBtools::aq_download(loc_id = all_timeseries$location[i], ts_name = ts_name, start = first_unapproved, server = server)
        } else if (aquarius_range == "all"){
          realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM realtime WHERE parameter = '", parameter, "' AND location = '", all_timeseries$location[i], "' AND datetime_UTC >= '", first_unapproved, "'"))
          data <- WRBtools::aq_download(loc_id = all_timeseries$location[i], ts_name = ts_name, server = server)
        }
        ts <- data.frame("location" = all_timeseries$location[i], "parameter" = parameter, "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)

      } else if (operator == "WSC"){
        realtime <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM realtime WHERE parameter = '", parameter, "' AND location = '", all_timeseries$location[i], "' AND datetime_UTC >= '", WSC_range, "'"))
        token <- suppressMessages(tidyhydat.ws::token_ws())
        data <- suppressMessages(tidyhydat.ws::realtime_ws(all_timeseries$location[i], if (parameter == "flow") 47 else if (parameter == "level") 46, start_date = as.POSIXct(WSC_range), end_date = .POSIXct(Sys.time(), "UTC"), token = token))
        data <- data[,c(2,4,1)]
        names(data) <- c("datetime_UTC", "value", "location")
        data$parameter <- parameter
        data$datetime_UTC <- as.character(data$datetime_UTC)
        data$approval <- "preliminary"
        ts <- data
      }

      if (nrow(ts) > 0){
        mismatch <- FALSE
        done <- FALSE
        row <- 1
        while (!mismatch & !done){
          datetime <- ts$datetime_UTC[row]
          if (datetime %in% realtime$datetime_UTC){ # check that the corresponding time exists in realtime. If not, mismatch is TRUE
            if (!(ts[ts$datetime_UTC == datetime, "value"] == realtime[realtime$datetime_UTC == datetime, "value"])){ #check that values are the same
              mismatch <- TRUE
              if (row > 1) { #Go back to the last known good point if not the first row
                datetime <- ts$datetime_UTC[row-100]
              }
            } else {
              row <- row + 100 #Go up by increments of 100 for the sake of speed
            }
          } else {
            mismatch <- TRUE
            if (row > 1) { #Go back to the last known good point if not the first row
              datetime <- ts$datetime_UTC[row-100]
            }
          }
          if (row == nrow(ts)){
            done <- TRUE
          }
        }
        if (mismatch){
          ts <- ts[ts$datetime_UTC >= datetime ,]
          DBI::dbExecute(hydro, paste0("DELETE FROM realtime WHERE parameter = '", parameter, "' AND location = '", loc, "' AND datetime_UTC BETWEEN '", min(ts$datetime_UTC), "' AND '", max(ts$datetime_UTC), "'"))
          DBI::dbAppendTable(hydro, "realtime", ts)
          DBI::dbExecute(hydro, paste0("DELETE FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "' AND date >= '", substr(min(ts$datetime_UTC), 1, 10), "'"))
          #make the new entry into table timeseries
          end <- max(max(realtime$datetime_UTC), ts$datetime_UTC)
          DBI::dbExecute(hydro, paste0("UPDATE timeseries SET end_datetime_UTC = '", end, "' WHERE location = '", all_timeseries$location[i], "' AND parameter = '", parameter, "' AND type = 'continuous'"))

          #Recalculate daily means and statistics
          calculate_stats(timeseries = data.frame("location" = loc,
                                                  "parameter" = parameter),
                          path = path,
                          start_recalc = substr(datetime, 1, 10))
        }
      }
    }, error = function(e) {
      print(paste0("Failed on location ", all_timeseries$location[i], " and parameter ", all_timeseries$parameter[i]))
    }
    )
  }

  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_weekly'"))

} #End of function
