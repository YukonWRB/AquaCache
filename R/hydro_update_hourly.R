#' Hourly update of real-time data
#'
#' Retrieves new real-time data from WSC and Aquarius, starting from the last data point in the local database. Also updates the location_data_range table. Only works on stations that are ALREADY in the realtime tables; refer to function hydro_update_daily for how to add new stations. Timeseries that have an identical location name in WSC real-time data and Aquarius will only pull from WSC information.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next five parameters. FALSE will only populate with WSC data.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @import tidyhydat.ws
#' @export

hydro_update_hourly <- function(path, aquarius = TRUE, server = "https://yukon.aquaticinformatics.net/AQUARIUS")

{

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

  aq_names <- DBI::dbGetQuery(hydro, "SELECT parameter, value FROM settings WHERE application  = 'aquarius'")

  count <- 0 #counter for number of successful stations
  all_timeseries <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'")
  success <- data.frame()
  token_time <- Sys.time()-1
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    operator <- all_timeseries$operator[i]

    tryCatch({
      if (operator == "WRB" & aquarius){
        ts_name <- aq_names[aq_names$parameter == parameter , 2]
        data <- WRBtools::aq_download(loc_id = all_timeseries$location[i], ts_name = ts_name, start = as.POSIXct(all_timeseries$end_datetime[i], tz= "UTC") + 1, server = server)
        ts <- data.frame("location" = all_timeseries$location[i], "parameter" = parameter, "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)

      } else if (operator == "WSC"){
        if (token_time < Sys.time()){
          token <- suppressMessages(tidyhydat.ws::token_ws())
          token_time <- Sys.time() + 60*9 #make valid for 9 minutes (max is 10 minutes)
        }
        token <- suppressMessages(tidyhydat.ws::token_ws())
        data <- suppressMessages(tidyhydat.ws::realtime_ws(all_timeseries$location[i], if (parameter == "flow") 47 else if (parameter == "level") 46, start_date = as.POSIXct(all_timeseries$end_datetime[i], tz="UTC") + 1, end_date = .POSIXct(Sys.time(), "UTC"),  token = token))
        data <- data[,c(2,4,1)]
        names(data) <- c("datetime_UTC", "value", "location")
        data$datetime_UTC <- as.character(data$datetime_UTC)
        data$approval <- "preliminary"
        data$parameter <- parameter
        ts <- data
      }

      if (nrow(ts) > 0){
        DBI::dbExecute(hydro, paste0("DELETE FROM realtime WHERE location = '", loc, "' AND parameter = '", parameter, "' AND datetime_UTC BETWEEN '", min(ts$datetime_UTC), "' AND '", max(ts$datetime_UTC), "'"))
        DBI::dbAppendTable(hydro, "realtime", ts)
        #make the new entry into table timeseries
        DBI::dbExecute(hydro, paste0("UPDATE timeseries SET end_datetime_UTC = '", as.character(max(ts$datetime_UTC)),"' WHERE location = '", all_timeseries$location[i], "' AND parameter = '", parameter, "' AND type = 'continuous'"))
        count <- count + 1
        success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "operator" = operator))
        DBI::dbExecute(hydro, paste0("UPDATE timeseries SET last_new_data_UTC = '", .POSIXct(Sys.time(), "UTC"), "' WHERE location= '", loc, "' AND parameter = '", parameter, "' AND operator = '", operator, "' AND type = 'continuous'"))
      }
    }, error = function(e) {}
    ) #End of tryCatch
  } #End of iteration over each location + param
  print(paste0(count, " out of ", nrow(all_timeseries), " timeseries were updated."))
  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_realtime'"))
  return(success)
} #End of function

