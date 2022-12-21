#This will just pull data from WSC tidyhydat and from Aquarius starting from the last data point in the local DB. location_data_range table will also be updated.

#' Hourly update of real-time data
#'
#' Retrieves new real-time data from WSC and Aquarius, starting from the last data point in the local database. Also updates the location_data_range table. Only works on stations that are ALREADY in the realtime tables; refer to function hydro_update_daily for how to add new stations.
#'
#' Timeseries that have an identical location name in WSC real-time data and Aquarius will only pull from WSC information.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return The database is updated in-place.
#' @export

hydro_update_hourly <- function(path, stage = "Stage.Publish", discharge = "Discharge.Publish", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS")

{

  if (is.null(Sys.getenv("AQPASS"))){
    stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
  }
  if (is.null(Sys.getenv("AQUSER"))){
    stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
  }
  if (is.null(Sys.getenv("WS_USRNM"))){
    stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
  }
  if (is.null(Sys.getenv("WS_PWD"))){
    stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
  }

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  token <- NA #This is for the tidyhydat.ws functions, prevents having to re-issue a token more often than necessary

  WSC_flow_stns <- DBI::dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WSC_flow_realtime GROUP BY location")

  ##### hydro_update_daily needs to be run before line below
  WSC_daily_flows_extrema <- DBI::dbGetQuery(hydro, "SELECT MAX(flow), MIN(flow) FROM WSC_flow_daily GROUP BY date")

  for (i in WSC_flow_stns$location){
    if (is.na(token)){ #getting the token slows things down a touch, this way the token is only fetched if needed.
      token <- tidyhydat.ws::token_ws()
    } else if (attr(token, "time") < (Sys.time()-9*60)) { #tokens are valid for 10 minutes max.
      token <- tidyhydat.ws::token_ws()
    }
    last_datetime <- WSC_flow_stns[WSC_flow_stns$location==i,]$`MAX(datetime_UTC)`
    data <- tidyhydat.ws::realtime_ws(i, 47, start_date = last_datetime, end = .POSIXct(Sys.time(), "UTC"), token = token) #this download includes the last time point in the database, which needs to be removed later. Unfortunately not all data points are 5 minutes apart so last_datetime can't just be advanced 5 minutes.
    data <- data[-(data$Date == last_datetime),]
    data <- data[,c(1,2,4)]
    names(data) <- c("location", "datetime_UTC", "flow")
    data$datetime_UTC <- as.character(data$datetime_UTC)
    data$approval <- "preliminary"


    ####hydro_update_daily needs to be run at least once before the chunk below
    #((measurement - historic min for the day) / (historic max for the day - historic min for the day)) * 100 BUT not including current year's readings
    data$percent_historic_range <- NA
    for (j in 1:nrow(data)){
      day <- substr(data$datetime_UTC[j], 1, 10)
      data$percent_historic_range[j] <- ((data$flow[j] - historic min) / (historic max - historic min)) * 100
    }


    DBI::dbAppendTable(hydro, "WSC_flow_realtime", data)
    #update the locations table for this station
    DBI::dbExecute(hydro, paste0("UPDATE locations SET end_date = '", max(data$datetime_UTC), "' WHERE location = '", i, "' AND data_type = 'flow'"))
  }


  WSC_level_stns <- DBI::dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WSC_level_realtime GROUP BY location")
  for (i in WSC_level_stns$location){
    if (is.na(token)){ #getting the token slows things down a touch, this way the token is only fetched if needed.
      token <- tidyhydat.ws::token_ws()
    } else if (attr(token, "time") < (Sys.time()-9*60)) { #tokens are valid for 10 minutes max.
      token <- tidyhydat.ws::token_ws()
    }
    last_datetime <- WSC_level_stns[WSC_level_stns$location==i,]$`MAX(datetime_UTC)`
    data <- tidyhydat.ws::realtime_ws(i, 47, start_date = last_datetime, end = .POSIXct(Sys.time(), "UTC"), token = token) #this download includes the last time point in the database, which needs to be removed later. Unfortunately not all data points are 5 minutes apart so last_datetime can't just be advanced 5 minutes.
    data <- data[-(data$Date == last_datetime),]
    data <- data[,c(1,2,4)]
    names(data) <- c("location", "datetime_UTC", "level")
    data$datetime_UTC <- as.character(data$datetime_UTC)
    data$approval <- "preliminary"
    data$percent_historic_range <- NA
    #calculation of percent_historic_range should be performed here!
    DBI::dbAppendTable(hydro, "WSC_level_realtime", data)
  }

  WRB_flow_stns <- DBI::dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_flow_realtime GROUP BY location")
  for (i in WRB_flow_stns$location){
    last_datetime <- WRB_flow_stns[WRB_flow_stns$location==i,]$`MAX(datetime_UTC)`
    data <- WRBtools::aq_download(i, discharge, start = last_datetime)
    data <-
  }

  WRB_level_stns <- DBI:dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_level_realtime GROUP BY location")
  for (i in WRB_level_stns$location){
    last_datetime <- WRB_level_stns[WRB_level_stns$location==i,]$`MAX(datetime_UTC)`
    data <- WRBtools::aq_download(i, stage, start = last_datetime)
  }

  WRB_snow_depth_stns <- DBI:dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_snow_pillow_depth_realtime GROUP BY location")

  WRB_snow_SWE_stns <- DBI:dbGetQuerry(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_snow_pillow_SWE_realtime GROUP BY location")


} #End of function

