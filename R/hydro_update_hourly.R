#This will just pull data from WSC tidyhydat and from Aquarius starting from the last data point in the local DB. location_data_range table will also be updated.

#' Hourly update of real-time data
#'
#' Pulls new real-time from WSC and Aquarius, starting from the last data point in the local database. Also updates the location_data_range table. Only works on stations that are ALREADY in the realtime tables.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#'
#' @return Updated WSC_xx_realtime and WRB_xx_realtime tables, and updated location_data_range table.
#'
#' @export
#'

hydro_update_hourly <- function(path){

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  token <- NA #This is for the tidyhydat.ws functions, prevents having to re-issue a token more often than necessary

  WSC_flow_stns <- DBI::dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WSC_flow_realtime GROUP BY location")
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


    #((measurement - historic min) / (historic max - historic min)) * 100
    data$percent_max_historic <- NA
    for (j in 1:nrow(data)){
      day <- substr(data$datetime_UTC[j], 1, 10)
      data$percent_max_historic[j] <- ((data$flow[j] - historic min) / (historic max - historic min)) * 100
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
    data$percent_max_historic <- NA
    #calculation of percent_max_historic should be performed here!
    DBI::dbAppendTable(hydro, "WSC_level_realtime", data)
  }

  WRB_flow_stns <- DBI::dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_flow_realtime GROUP BY location")
  for (i in WRB_flow_stns$location){
    last_datetime <- WRB_flow_stns[WRB_flow_stns$location==i,]$`MAX(datetime_UTC)`
    data <- WRBtools::aq_download(i, "Discharge.Publish", start = last_datetime)
    data <-
  }

  WRB_level_stns <- DBI:dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_level_realtime GROUP BY location")
  for (i in WRB_level_stns$location){
    last_datetime <- WRB_level_stns[WRB_level_stns$location==i,]$`MAX(datetime_UTC)`
    data <- WRBtools::aq_download(i, "Stage.Publish", start = last_datetime)
  }

  WRB_snow_depth_stns <- DBI:dbGetQuery(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_snow_pillow_depth_realtime GROUP BY location")

  WRB_snow_SWE_stns <- DBI:dbGetQuerry(hydro, "SELECT location, MAX(datetime_UTC) FROM WRB_snow_pillow_SWE_realtime GROUP BY location")


  for (i in WSC_flow_stns){

  }

  for (i in WSC_level_stns){

  }

  for (i in WRB_flow_stns){

  }

  for (i in WRB_level_stns){

  }

  for (i in WRB_snow_depth_stns){

  }

  for (i in WRB_snow_SWE_stns){

  }

} #End of function

