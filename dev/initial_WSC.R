#' Initial creation of tables containing WSC data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#'
#' Pulls data from Aquarius as well as from WSC to perform the initial creation of WSC-related tables. The incorporation of Aquarius data is done to fill the gap between the 18 months of real-time data and the published historical data (daily means), as well as to provide finer resolution data beyond the last 18 months. A local copy of the HYDAT database will be created or updated, if needed.
#'
#' WSC stations should be specified in a character vector of arbitrary length. The naming of WSC stations should follow the WSC convention, e.g. "09AB001". To successfully download from Aquarius these timeseries must be named IDENTICALLY in Aquarius. You must also specify the standard naming scheme for your stage (level) and discharge(flow) timeseries, in the form Parameter.Label.
#'
#' Uses two functions under the hood that require some set-up of the .Renviron file: [tidyhydat.ws::realtime_ws()] and [YGWater::aq_download()]. See respective help files for setup information.
#'
#Any timeseries labelled as 'getRealtimeAQ' in the source_fx column in the timeseries table will need your Aquarius username, password, and server address present in your .Renviron profile: see [YGWater::aq_download()] for more information.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param WSC_stns The WSC stations you wish to pull information for. In the event that these stations are mirrored in your Aquarius database the function will attempt to fetch that information. Otherwise, only information from the HYDAT database and from real-time WSC data will be incorporated. The default, "yukon" is a preset list of 77 stations in or relevant to Yukon.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next three parameters. FALSE will only populate with WSC data.
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All WSC_stns must have the same names. !This ONLY applies to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All WSC_stns must have the same names. !This ONLY applies to WSC stations mirrored in Aquarius.
#'
#' @return Updated tables in the database.
#' @export
#'

initial_WSC <- function(con = hydrometConnect(), WSC_stns = "yukon", aquarius = TRUE, stage = "Stage.Preliminary", discharge = "Discharge.Preliminary")

  {

  if (tolower(WSC_stns)[1] == "yukon"){
    #Yukon and Liard WSC timeseries
    WSC_stns <- c("08AA003","08AA005","08AA007","08AA008","08AA009","08AA010","08AA011","08AA012","08AB001","08AC001","08AC002","09AA001","09AA004","09AA012","09AA013","09AA017","09AB001","09AB004","09AB010","09AC001","09AC007","09AD002","09AE002","09AE003","09AE006","09AG001","09AG002","09AH001","09AH003","09AH004","09AH005","09BA001","09BB001","09BC001","09BC002","09BC004","09CA001","09CA002",'09CA004',"09CA006","09CB001","09CD001","09DA001","09DB001","09DC005","09DC006","09DD003","09DD004","09EA003","09EA004","09EA005","09EA006","09EB001","09EB003","09EB004","09FA001","09FB002","09FB003","09FC001","09FD002","09FD003","10AA001","10AA004","10AA005","10AA006","10AB001","10AC005","10AD002","10BD001","10DB001","10MA001","10MA002","10MA003","10MB003","10MB004","10MD001","10MD002", "09AB008")
  }

  #Check hydat version, update if needed.
  hydat_check()

  if (aquarius){
      #Add the realtime data held in Aquarius to the database
      # Download data from AQ
      aqFlow <- list()
      aqLevel <- list()
      for (i in WSC_stns){
        try(aqLevel[[i]] <- YGWater::aq_download(loc_id = i, ts_name = stage))
      }
      for (i in WSC_stns){
        try(aqFlow[[i]] <- YGWater::aq_download(i, discharge))
      }

      level_rt <- data.frame()
      for (i in names(aqLevel)){
        if (nrow(aqLevel[[i]]$timeseries) > 0){
          timeseries <- aqLevel[[i]]$timeseries[c(1,2)]
          ts <- data.frame("location" = i,
                           "parameter" = "level",
                           "unit" = "m",
                           "category" = "continuous",
                           "period_type" = "instantaneous",
                           "param_type" = "hydrometric",
                           "start_datetime" = min(timeseries$datetime),
                           "end_datetime" = max(timeseries$datetime),
                           "last_new_data" = .POSIXct(Sys.time(), "UTC"),
                           "operator" = "WSC",
                           "network" = "Canada Yukon Hydrometric Network",
                           "public" = TRUE,
                           "source_fx" = "downloadWSC")
          try(DBI::dbAppendTable(con, "timeseries", ts))
          tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND category = 'continuous'"))[1,1]

          timeseries$timeseries_id <- tsid
          level_rt <- rbind(level_rt, timeseries)
        }
      }
      level_rt$approval <- "Provisional/Provisoire"
      level_rt$grade <- "-1"
      level_rt$period <- "00:00:00"

      flow_rt <- data.frame()
      for (i in names(aqFlow)){
        if (nrow(aqFlow[[i]]$timeseries) > 0){
          timeseries <- aqFlow[[i]]$timeseries[c(1,2)]
          ts <- data.frame("location" = i,
                           "parameter" = "flow",
                           "unit" = "m3/s",
                           "category" = "continuous",
                           "period_type" = "instantaneous",
                           "param_type" = "hydrometric",
                           "start_datetime" = min(timeseries$datetime),
                           "end_datetime" = max(timeseries$datetime),
                           "last_new_data" = .POSIXct(Sys.time(), "UTC"),
                           "operator" = "WSC",
                           "network" = "Canada Yukon Hydrometric Network",
                           "public" = TRUE,
                           "source_fx" = "downloadWSC")
          try(DBI::dbAppendTable(con, "timeseries", ts))
          tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND category = 'continuous'"))[1,1]
          timeseries$timeseries_id <- tsid
          flow_rt <- rbind(flow_rt, timeseries)
        }
      }
      flow_rt$approval <- "Provisional/Provisoire"
      flow_rt$grade <- "-1"
      flow_rt$period <- "00:00:00"

      DBI::dbAppendTable(con, "measurements_continuous", level_rt)
      DBI::dbAppendTable(con, "measurements_continuous", flow_rt)
      print("Timeseries existing in Aquarius have been downloaded and appended to the local database.")
  }

  #Refresh the last 18 months with realtime data in case there were changes, or to incorporate new stations. At the same time get the station name from tidyhydat.
  new_realtime <- list(flow = list(), level = list())
  for (i in WSC_stns){
    try({
      new_realtime$flow[[i]] <- downloadWSC(i, 47, start_datetime = Sys.Date()-577)
      if (nrow(new_realtime$flow[[i]]) > 0) new_realtime$flow[[i]]$period <- "00:00:00"})
    try({
      new_realtime$level[[i]] <- downloadWSC(i, 46, start_datetime = Sys.Date()-577)
      if (nrow(new_realtime$level[[i]]) >0) new_realtime$level[[i]]$period <- "00:00:00"
      })
  }

  #keep only what's necessary from the raw download (drop columns) and format columns; at the same time, make entries in the locations and timeseries table
  for (i in names(new_realtime$flow)){
    if (nrow(new_realtime$flow[[i]]) > 0){
      start_AQ <- min(aqFlow[[i]]$timeseries$datetime)
      start_ws <- min(new_realtime$flow[[i]]$datetime)
      start_datetime <- hablar::rationalize(min(c(start_AQ, start_ws)))
      name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
      location_info <- data.frame("location" = i,
                                  "name" = name,
                                  "latitude" = tidyhydat::hy_stations(i)$LATITUDE,
                                  "longitude" = tidyhydat::hy_stations(i)$LONGITUDE)
      try(DBI::dbAppendTable(con, "locations", location_info), silent = TRUE)

      existing <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND category = 'continuous'"))
      if (nrow(existing) == 0){ #Not there yet
        timeseries_info <- data.frame("location" = i,
                                      "parameter" = "flow",
                                      "unit" = "m3/s",
                                      "category" = "continuous",
                                      "period_type" = "instantaneous",
                                      "param_type" = "hydrometric",
                                      "start_datetime" = start_datetime,
                                      "end_datetime" = max(new_realtime$flow[[i]]$datetime),
                                      "last_new_data" = .POSIXct(Sys.time(), "UTC"),
                                      "operator" = "WSC",
                                      "network" = "Canada Yukon Hydrometric Network",
                                      "public" = TRUE,
                                      "source_fx" = "downloadWSC")
        DBI::dbAppendTable(con, "timeseries", timeseries_info)
        tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND category = 'continuous'"))[1,1]
      } else {
        tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND category = 'continuous'"))[1,1]
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "', end_datetime = '", max(new_realtime$flow[[i]]$datetime), "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid))
      }
      delete_bracket <- c(min(new_realtime$flow[[i]]$datetime), max(new_realtime$flow[[i]]$datetime))
      DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE datetime BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "' AND timeseries_id = ", tsid))
      new_realtime$flow[[i]]$timeseries_id <- tsid
      DBI::dbAppendTable(con, "measurements_continuous", new_realtime$flow[[i]])
    }
  }

  for (i in names(new_realtime$level)){
    if (nrow(new_realtime$level[[i]]) > 0){
      start_AQ <- min(aqLevel[[i]]$timeseries$datetime)
      start_ws <- min(new_realtime$level[[i]]$datetime)
      start_datetime <- hablar::rationalize(min(c(start_AQ, start_ws)))
      name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
      location_info <- data.frame("location" = i,
                                  "name" = name,
                                  "latitude" = tidyhydat::hy_stations(i)$LATITUDE,
                                  "longitude" = tidyhydat::hy_stations(i)$LONGITUDE)
      try(DBI::dbAppendTable(con, "locations", location_info), silent = TRUE)

      existing <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND category = 'continuous'"))
      if (nrow(existing) == 0){ #Not there yet
        timeseries_info <- data.frame("location" = i,
                                      "parameter" = "level",
                                      "unit" = "m3/s",
                                      "category" = "continuous",
                                      "period_type" = "instantaneous",
                                      "param_type" = "hydrometric",
                                      "start_datetime" = start_datetime,
                                      "end_datetime" = max(new_realtime$level[[i]]$datetime),
                                      "last_new_data" = .POSIXct(Sys.time(), "UTC"),
                                      "operator" = "WSC",
                                      "network" = "Canada Yukon Hydrometric Network",
                                      "public" = TRUE,
                                      "source_fx" = "downloadWSC")
        DBI::dbAppendTable(con, "timeseries", timeseries_info)
        tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND category = 'continuous'"))[1,1]
      } else {
        tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND category = 'continuous'"))[1,1]
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "', end_datetime = '", max(new_realtime$level[[i]]$datetime), "', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", tsid))
      }
      delete_bracket <- c(min(new_realtime$level[[i]]$datetime), max(new_realtime$level[[i]]$datetime))
      DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE datetime BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "' AND timeseries_id = ", tsid))
      new_realtime$level[[i]]$timeseries_id <- tsid
      DBI::dbAppendTable(con, "measurements_continuous", new_realtime$level[[i]])
    }
  }

  #Now deal with historical "HYDAT" information, inserting into measurement tables and adding entry to timeseries table. These daily means take precedence over those later calculated by
  for (i in WSC_stns){
    tryCatch({
      level <- tidyhydat::hy_daily_levels(i)[,c("Date", "Value")]
      colnames(level) <- c("date", "value")
      level$approval <- "approved"
      level$grade <- "-1"
      level$period_type <- "mean"
      start_datetime <- as.POSIXct(paste0(min(level$date), " 00:00:00"), tz = "UTC")
      tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND category = 'continuous'"))
      if (nrow(tsid) > 0) {
        tsid <- tsid[1,1]
        level$timeseries_id <- tsid
        DBI::dbAppendTable(con, "calculated_daily", level)
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "' WHERE timeseries_id = ", tsid))
      } else { #no corresponding realtime station yet
        end_datetime <- as.POSIXct(paste0(max(level$date), " 00:00:00"), tz = "UTC")
        latitude <- tidyhydat::hy_stations(i)$LATITUDE
        longitude <- tidyhydat::hy_stations(i)$LONGITUDE
        name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
        DBI::dbExecute(con, paste0("INSERT INTO timeseries (location, parameter, unit, category, period_type, param_type, start_datetime, end_datetime, last_new_data, operator, network, public, source_fx) VALUES ('", i, "', 'level', 'm', 'continuous', 'instantaneous', 'hydrometric', '", start_datetime, "', '", end_datetime, "', '", .POSIXct(Sys.time(), "UTC"), " 'WSC', 'Canada Yukon Hydrometric Network', TRUE, 'downloadWSC')"))
        DBI::dbExecute(con, paste0("INSERT OR IGNORE INTO locations (location, name, latitude, longitude) VALUES ('", i, "', '", name, "', '", latitude, "', '", longitude, "'"))
        tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'level' AND category = 'continuous'"))[1,1]
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "' WHERE timeseries_id = ", tsid))
        level$timeseries_id <- tsid
        DBI::dbAppendTable(con, "calculated_daily", level)
      }
    }, error = function(e){
      message("No level for station ", i)
      }
    )
    tryCatch({
      flow <- tidyhydat::hy_daily_flows(i)[,c("Date", "Value")]
      colnames(flow) <- c("date", "value")
      flow$approval <- "approved"
      flow$grade <- "-1"
      flow$period_type <- "mean"
      start_datetime <- as.POSIXct(paste0(min(flow$date), " 00:00:00"), tz = "UTC")
      tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND category = 'continuous'"))
      if (nrow(tsid) > 0){
        tsid <- tsid[1,1]
        flow$timeseries_id <- tsid
        DBI::dbAppendTable(con, "calculated_daily", flow)
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "' WHERE timeseries_id = ", tsid))
      } else { #no corresponding realtime station yet
        end_datetime <- as.POSIXct(paste0(max(flow$date), " 00:00:00"), tz = "UTC")
        latitude <- tidyhydat::hy_stations(i)$LATITUDE
        longitude <- tidyhydat::hy_stations(i)$LONGITUDE
        name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
        DBI::dbExecute(con, paste0("INSERT INTO timeseries (location, parameter, unit, category, period_type, param_type, start_datetime, end_datetime, last_new_data, operator, network, public, source_fx) VALUES ('", i, "', 'flow', 'm', 'continuous', 'instantaneous', 'hydrometric', '", start_datetime, "', '", end_datetime, "', '", .POSIXct(Sys.time(), "UTC"), " 'WSC', 'Canada Yukon Hydrometric Network', TRUE, 'downloadWSC')"))
        DBI::dbExecute(con, paste0("INSERT OR IGNORE INTO locations (location, name, latitude, longitude) VALUES ('", i, "', '", name, "', '", latitude, "', '", longitude, "'"))
        tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", i, "' AND parameter = 'flow' AND category = 'continuous'"))[1,1]
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "' WHERE timeseries_id = ", tsid))
        flow$timeseries_id <- tsid
        DBI::dbAppendTable(con, "calculated_daily", flow)
      }
    }, error = function(e){
     message("No flow for station ", i)
    }
    )
  } #End of for loop adding historical WSC data
  message("Timeseries existing in the WSC historical database (HYDAT) have been appended to the local database.")

} #End of function
