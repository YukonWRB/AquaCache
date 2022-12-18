#' Initial creation of tables containing WSC data
#'
#' Pulls data from Aquarius as well as from WSC to perform the initial creation of WSC-related tables. The incorporation of Aquarius data is done to fill the gap between the 18 months of real-time data and the published historical data (daily means), as well as to provide finer resolution data.
#'
#' Uses two functions under the hood that require some set-up of the .Renviron file: tidyhydat.ws::realtime_ws and WRBtools::aq_download. See respective help files for setup information.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#'
#' @return Updated tables in the database.
#' @export
#'

initial_WSC <- function(path) {

  #Initial checks
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

  #WSC stations will be used to download from Aquarius, populate both datum tables, and pull realtime data from WSC to update the last 18 months of Aquarius data
  #Yukon and Liard WSC locations
  WSCstns <- c("08AA003","08AA005","08AA007","08AA008","08AA009","08AA010","08AA011","08AA012","08AB001","08AC001","08AC002","09AA001","09AA004","09AA012","09AA013","09AA017","09AB001","09AB004","09AB010","09AC001","09AC007","09AD002","09AE002","09AE003","09AE006","09AG001","09AG002","09AH001","09AH003","09AH004","09AH005","09BA001","09BB001","09BC001","09BC002","09BC004","09CA001","09CA002",'09CA004',"09CA006","09CB001","09CD001","09DA001","09DB001","09DC005","09DC006","09DD003","09DD004","09EA003","09EA004","09EA005","09EA006","09EB001","09EB003","09EB004","09FA001","09FB002","09FB003","09FC001","09FD002","09FD003","10AA001","10AA004","10AA005","10AA006","10AB001","10AC005","10AD002","10BD001","10DB001","10MA001","10MA002","10MA003","10MB003","10MB004","10MD001","10MD002")


  #Add the realtime data to the database
  # Download data from AQ
  aqFlow <- list()
  aqLevel <- list()
  for (i in WSCstns){
    try(aqLevel[[i]] <- WRBtools::aq_download(i, "Stage.Preliminary")$timeseries[,c(1,2)])
  }
  for (i in WSCstns){
    try(aqFlow[[i]] <- WRBtools::aq_download(i, "Discharge.Preliminary")$timeseries[,c(1,2)])
  }

  for (i in names(aqLevel)){
    aqLevel[[i]]$location <- i
    names(aqLevel[[i]]) <- c("datetime_UTC", "level", "location")
    aqLevel[[i]]$datetime_UTC <- as.character(aqLevel[[i]]$datetime_UTC)
  }
  for (i in names(aqFlow)){
    aqFlow[[i]]$location <- i
    names(aqFlow[[i]]) <- c("datetime_UTC", "flow", "location")
    aqFlow[[i]]$datetime_UTC <- as.character(aqFlow[[i]]$datetime_UTC)
  }

  flow_rt <- do.call("rbind", aqFlow)
  rownames(flow_rt) <- NULL
  level_rt <- do.call("rbind", aqLevel)
  rownames(level_rt) <- NULL

  DBI::dbAppendTable(hydro, "WSC_level_realtime", level_rt)
  DBI::dbAppendTable(hydro, "WSC_flow_realtime", flow_rt)

  #Refresh the last 18 months with realtime data in case there were changes
  new_realtime <- list(flow = list(), level = list())
  library(tidyhydat.ws) #necessary because internal data is not properly specified
  for (i in WSCstns){
    token <- tidyhydat.ws::token_ws(username = Sys.getenv("WS_USRNM"), password = Sys.getenv("WS_PWD"))
    try(new_realtime$flow[[i]] <- tidyhydat.ws::realtime_ws(i, 47, start_date = Sys.Date()-577, end_date = Sys.Date(), token = token))
    try(new_realtime$level[[i]] <- tidyhydat.ws::realtime_ws(i, 46, start_date = Sys.Date()-577, end_date = Sys.Date(), token = token))
  }

  #keep only what's necessary from the raw download (drop columns) and format columns; at the same time, make entries in the locations table
  for (i in names(new_realtime$flow)){
    new_realtime$flow[[i]] <- new_realtime$flow[[i]][,c(2,4,1)]
    names(new_realtime$flow[[i]]) <- c("datetime_UTC", "flow", "location")
    new_realtime$flow[[i]]$datetime_UTC <- as.character(new_realtime$flow[[i]]$datetime_UTC)
    location_info <- data.frame("location" = i, "data_type" = "flow", "start_datetime" = min(new_realtime$flow[[i]]$datetime_UTC), "end_datetime" = max(new_realtime$flow[[i]]$datetime_UTC), "latitude" = tidyhydat::hy_stations(i)$LATITUDE, "longitude" = tidyhydat::hy_stations(i)$LONGITUDE, operator = "WSC", network = "Canada Yukon Hydrometric Network")
    DBI::dbAppendTable(hydro, "locations", location_info)
  }

  for (i in names(new_realtime$level)){
    new_realtime$level[[i]] <- new_realtime$level[[i]][,c(2,4,1)]
    names(new_realtime$level[[i]]) <- c("datetime_UTC", "level", "location")
    new_realtime$level[[i]]$datetime_UTC <- as.character(new_realtime$level[[i]]$datetime_UTC)
    location_info <- data.frame("location" = i, "data_type" = "level", "start_datetime" = min(new_realtime$level[[i]]$datetime_UTC), "end_datetime" = max(new_realtime$level[[i]]$datetime_UTC), "latitude" = tidyhydat::hy_stations(i)$LATITUDE, "longitude" = tidyhydat::hy_stations(i)$LONGITUDE, operator = "WSC", network = "Canada Yukon Hydrometric Network")
    DBI::dbAppendTable(hydro, "locations", location_info)
  }

  new_flow_rt <- do.call("rbind", new_realtime$flow)
  rownames(new_flow_rt) <- NULL
  new_level_rt <- do.call("rbind", new_realtime$level)
  rownames(new_level_rt) <- NULL

  #Delete the entries and then append the new data into the database. This has the added bonus of adding new rows as well as replacing existing rows
  delete_bracket <- c(min(new_flow_rt$datetime_UTC), max(new_flow_rt$datetime_UTC))
  DBI::dbExecute(hydro, paste0("DELETE FROM WSC_flow_realtime WHERE datetime_UTC BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "'")) #NOTE: SQL BETWEEN is inclusive
  DBI::dbAppendTable(hydro, "WSC_flow_realtime", new_flow_rt)

  delete_bracket <- c(min(new_level_rt$datetime_UTC), max(new_level_rt$datetime_UTC))
  DBI::dbExecute(hydro, paste0("DELETE FROM WSC_level_realtime WHERE datetime_UTC BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "'")) #NOTE: SQL BETWEEN is inclusive
  DBI::dbAppendTable(hydro, "WSC_level_realtime", new_level_rt)


} #End of function
