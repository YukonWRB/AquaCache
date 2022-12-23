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

  #update from Aquarius
  if (aquarius){
    tryCatch({

      aq_flow <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_flow_realtime WHERE MAX(datetime_UTC) GROUP BY location")
      if (nrow(aq_flow) > 0){
        aq_flow_data <- data.frame()
        for (i in 1:nrow(aq_flow)){
          last_time <- aq_flow$datetime_UTC[i]
          data <- WRBtools::aq_download(aq_flow$location[i], aq_flow$datetime_UTC[i])$timeseries[c(1,2)]
          data$location <- aq_flow$location[i]
          names(data) <- c("datetime_UTC", "flow", "location")
          data()$approval <- "preliminary"
          data$datetime_UTC <- as.character(data$datetime_UTC)


          #remove last_time if present from the downloaded data
          aq_flow_data <- rbind(aq_flow_data, data)
          DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime = '", max(data$datetime_UTC), "' WHERE location = '", aq_flow$location[i], "' AND data_type = 'flow'"))
        }
      }

      aq_level <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_level_realtime")
      if (nrow(aq_level) > 0){

      }

      aq_depth <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_snow_pillow_depth_realtime")
      for (i in aq_depth){

      }
      aq_SWE <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_snow_pillow_SWE_realtime")
      for (i in aq_SWE){

      }
    }, error = function(e) {
      warning("New information from Aquarius may not have been properly downloaded. Try again later.")
    }
    )
  }


  #update from WSC
  library(tidyhydat.ws)
  on.exit(detach("package:tidyhydat.ws", unload = TRUE))

  tryCatch({
    WSC_flow <- DBI::dbGetQuery(hydro, "SELECT * FROM WSC_flow_realtime WHERE MAX(datetime_UTC) GROUP BY location")
    for (i in WSC_flow){
      last_time <- WSC_flow$datetime_UTC
      tidyhydat.ws::realtime_ws(i, 47, start_date = last_time, end_date = .POSIXct(Sys.time(), "UTC"))
    }
    WSC_level <- DBI::dbGetQuery(hydro, "SELECT * FROM WSC_level_realtime")
    for (i in WSC_level){

    }

  }, error = function(e) {
    warning("New information from the WSC may not have been properly downloaded. Try again later.")
  }
  )


} #End of function

