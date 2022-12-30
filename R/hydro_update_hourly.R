#This will just pull data from WSC tidyhydat and from Aquarius starting from the last data point in the local DB. location_data_range table will also be updated.

#' Hourly update of real-time data
#'
#' Retrieves new real-time data from WSC and Aquarius, starting from the last data point in the local database. Also updates the location_data_range table. Only works on stations that are ALREADY in the realtime tables; refer to function hydro_update_daily for how to add new stations.
#'
#' Timeseries that have an identical location name in WSC real-time data and Aquarius will only pull from WSC information.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next five parameters. FALSE will only populate with WSC data.
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return The database is updated in-place.
#' @import tidyhydat.ws
#' @export

hydro_update_hourly <- function(path, aquarius = TRUE, stage = "Stage.Publish", discharge = "Discharge.Publish", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS")

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

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations")
  for (i in 1:nrow(locations)){
    loc <- locations$location[i]
    type <- locations$data_type[i]
    table_name <- if (type == "SWE") "snow_pillow_SWE" else if (type == "depth") "snow_pillow_depth" else type
    operator <- locations$operator[i]
    units <- if (type == "SWE") "mm SWE" else if (type == "depth") "cm" else if (type == "level") "m" else if (type == "flow") "m3/s"
    tryCatch({
      if (operator == "WRB" & aquarius){
        data <- WRBtools::aq_download(loc_id = locations$location[i], ts_name = SWE, start = locations$end_datetime[i], server = server)
        ts <- data.frame("location" = locations$location[i], "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = units, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)

      } else if (operator == "WSC"){
        token <- suppressMessages(tidyhydat.ws::token_ws())
        data <- suppressMessages(tidyhydat.ws::realtime_ws(locations$location[i], if (type == "flow") 47 else if (type == "level") 46, start_date = locations$end_datetime[i], end_date = Sys.time(),  token = token))
        data <- data[,c(2,4,1)]
        names(data) <- c("datetime_UTC", "value", "location")
        data$datetime_UTC <- as.character(data$datetime_UTC)
        data$approval <- "preliminary"
        data$units <- units
        ts <- data
      }

      if (nrow(ts) > 0){
        DBI::dbExecute(hydro, paste0("DELETE FROM ", table_name, "_realtime WHERE datetime_UTC BETWEEN '", min(ts$datetime_UTC), "' AND '", max(ts$datetime_UTC), "'"))
        DBI::dbAppendTable(hydro, paste0(table_name, "_realtime"), ts)
        #make the new entry into table locations
        DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime = '", as.character(max(ts$datetime_UTC)),"' WHERE location = '", locations$location[i], "' AND data_type = '", type, "'"))
      }
    }, error = function(e) {
      print(paste0("Hydro_update_hourly failed on location ", locations$location[i], " and data type ", locations$data_type[i], ". The station may not currently be reporting."))
    }
    )
  }

} #End of function

