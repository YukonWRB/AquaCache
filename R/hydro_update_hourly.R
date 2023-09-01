#' Hourly update of real-time data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new real-time data from WSC and Aquarius, starting from the last data point in the local database. Also updates the location_data_range table. Only works on stations that are ALREADY in the realtime tables; refer to function hydro_update_daily for how to add new stations.
#'
#' Any timeseries labelled as 'getRealtimeAQ' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [WRBtools::aq_download()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [WRBtools::hydroConnect()].
#' @param tsid Specific timeseries to update. Default "all" will try to update all timeseries in the database.
#' @return The database is updated in-place, and a data.frame is generated with one row per updated location.
#' @export

hydro_update_hourly <- function(con, tsid="all")

{

  #TODO: Instead of selecting aquarius = TRUE, now the default is TRUE and tsid is optional. THIS IS NOT IMPLEMENTED YET
  on.exit(DBI::dbDisconnect(con))
  all_timeseries <- DBI::dbGetQuery(con, "SELECT * FROM timeseries WHERE category = 'continuous';")
  ts_names <- DBI::dbGetQuery(con,  "SELECT * FROM settings;")

  count <- 0 #counter for number of
  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    operator <- all_timeseries$operator[i]

    tryCatch({
      if (operator == "WRB" & aquarius){
        ts_name <- ts_names[ts_names$parameter == parameter & ts_names$application == "aquarius", "remote_param_name"]
        data <- WRBtools::aq_download(loc_id = loc, ts_name = ts_name, start = all_timeseries$end_datetime[i] + 1)
        ts <- data.frame("datetime" = data$timeseries$timestamp_UTC, "value" = data$timeseries$value, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)
      } else if (operator == "WSC"){
        ts_name <- ts_names[ts_names$parameter == parameter & ts_names$application == "WSC", "remote_param_name"]
        data <- getRealtimeWSC(loc, ts_name, start_datetime = all_timeseries$end_datetime[i] + 1)
        ts <- data[,c(2,4)]
        names(ts) <- c("datetime", "value")
        ts$approval <- "preliminary"
      }

      if (nrow(ts) > 0){
        ts$timeseries_id <- all_timeseries$timeseries_id[i]
        DBI::dbAppendTable(con, "realtime", ts)
        #make the new entry into table timeseries
        DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE location = '", loc, "' AND parameter = '", parameter, "' AND category = 'continuous'"))
        count <- count + 1
        success <- rbind(success, data.frame("location" = loc, "parameter" = parameter, "operator" = operator))
      }
    }, error = function(e) {
      warning("Failed on location ", loc, " and parameter ", parameter, " (timeseries_id ", all_timeseries$timeseries_id[i], ").")
    }
    ) #End of tryCatch
  } #End of iteration over each location + param
  message(count, " out of ", nrow(all_timeseries), " timeseries were updated.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_realtime'"))
  return(success)
} #End of function

