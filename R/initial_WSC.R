#' Initial creation of tables containing WSC data
#'
#' Pulls data from Aquarius as well as from WSC to perform the initial creation of WSC-related tables. The incorporation of Aquarius data is done to fill the gap between the 18 months of real-time data and the published historical data (daily means), as well as to provide finer resolution data beyond the last 18 months. A local copy of the HYDAT database will be created or updated, if needed.
#'
#' WSC stations should be specified in a character vector of arbitrary length. The naming of WSC stations should follow the WSC convention, e.g. "09AB001". To successfully download from Aquarius these timeseries must be named IDENTICALLY in Aquarius. You must also specify the standard naming scheme for your stage (level) and discharge(flow) timeseries, in the form Parameter.Label.
#'
#' Uses two functions under the hood that require some set-up of the .Renviron file: tidyhydat.ws::realtime_ws and WRBtools::aq_download. See respective help files for setup information.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param WSC_stns The WSC stations you wish to pull information for. In the event that these stations are mirrored in your Aquarius database the function will attempt to fetch that information. Otherwise, only information from the HYDAT database and from real-time WSC data will be incorporated. The default, "yukon" is a preset list of 77 stations in or relevant to Yukon.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next three parameters. FALSE will only populate with WSC data.
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All WSC_stns must have the same names. !This ONLY applies to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All WSC_stns must have the same names. !This ONLY applies to WSC stations mirrored in Aquarius.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return Updated tables in the database, including WSC real-time and daily tables and locations table
#' @import tidyhydat.ws
#' @export
#'

initial_WSC <- function(path, WSC_stns = "yukon", aquarius = TRUE, stage = "Stage.Preliminary", discharge = "Discharge.Preliminary", server = "https://yukon.aquaticinformatics.net/AQUARIUS")

  {
  #NOTE: be careful with SQL statements and additions/updates to rows. Values that are in 'single quotes' are entered as character (text) values, even if the value is only numbers. To have numbers as numeric values, leave the single quote out. This is done automatically when appending a data.frame based on the column type, so long as the column type is what you need!

  if (tolower(WSC_stns) == "yukon"){
    #Yukon and Liard WSC locations
    WSC_stns <- c("08AA003","08AA005","08AA007","08AA008","08AA009","08AA010","08AA011","08AA012","08AB001","08AC001","08AC002","09AA001","09AA004","09AA012","09AA013","09AA017","09AB001","09AB004","09AB010","09AC001","09AC007","09AD002","09AE002","09AE003","09AE006","09AG001","09AG002","09AH001","09AH003","09AH004","09AH005","09BA001","09BB001","09BC001","09BC002","09BC004","09CA001","09CA002",'09CA004',"09CA006","09CB001","09CD001","09DA001","09DB001","09DC005","09DC006","09DD003","09DD004","09EA003","09EA004","09EA005","09EA006","09EB001","09EB003","09EB004","09FA001","09FB002","09FB003","09FC001","09FD002","09FD003","10AA001","10AA004","10AA005","10AA006","10AB001","10AC005","10AD002","10BD001","10DB001","10MA001","10MA002","10MA003","10MB003","10MB004","10MD001","10MD002", "09AB008")
  }

  #Initial checks
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

  #Check hydat version, update if needed.
  tryCatch({hydat_path <- tidyhydat::hy_downloaded_db() #Attempts to get the hydat path, in case it's downloaded already.
  local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
  }, error = function(e) {hydat_path <- NULL})

  new_hydat <- FALSE
  if (!is.null(hydat_path) & exists("local_hydat")){ #If hydat already exists, compare version numbers
    local_hydat <- gsub("-", "", as.character(local_hydat))
    remote_hydat <- tidyhydat::hy_remote()
    if (local_hydat != remote_hydat){ #if remote version is more recent, download new version
      tidyhydat::download_hydat(ask=FALSE)
      hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
      new_hydat <- TRUE
      print("The local WSC HYDAT database was updated.")
    } else {
      print("The local copy of the WSC HYDAT database is up to date.")
    }
  } else if (is.null(hydat_path) | !exists("local_hydat")) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask=FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
    new_hydat <- TRUE
    print("A local copy of the WSC HYDAT database was installed.")
  }


  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))
  DBI::dbExecute(hydro, "PRAGMA busy_timeout=100000")

  if (aquarius){
      #Add the realtime data held in Aquarius to the database
      # Download data from AQ
      aqFlow <- list()
      aqLevel <- list()
      for (i in WSC_stns){
        try(aqLevel[[i]] <- WRBtools::aq_download(loc_id = i, ts_name ="Stage.Preliminary", server = server))
      }
      for (i in WSC_stns){
        try(aqFlow[[i]] <- WRBtools::aq_download(i, "Discharge.Preliminary", server = server))
      }

      level_rt <- data.frame()
      for (i in names(aqLevel)){
        if (nrow(aqLevel[[i]]$timeseries) > 0){
          timeseries <- aqLevel[[i]]$timeseries[c(1,2)]
          timeseries$location <- i
          level_rt <- rbind(level_rt, timeseries)
        }
      }
      names(level_rt) <- c("datetime_UTC", "value", "location")
      level_rt$approval <- "preliminary"
      level_rt$units <- "m"
      level_rt$datetime_UTC <- as.character(level_rt$datetime_UTC)

      flow_rt <- data.frame()
      for (i in names(aqFlow)){
        if (nrow(aqFlow[[i]]$timeseries) > 0){
          timeseries <- aqFlow[[i]]$timeseries[c(1,2)]
          timeseries$location <- i
          flow_rt <- rbind(flow_rt, timeseries)
        }
      }
      names(flow_rt) <- c("datetime_UTC", "value", "location")
      flow_rt$approval <- "preliminary"
      flow_rt$units <- "m3/s"
      flow_rt$datetime_UTC <- as.character(flow_rt$datetime_UTC)

      DBI::dbAppendTable(hydro, "level_realtime", level_rt)
      DBI::dbAppendTable(hydro, "flow_realtime", flow_rt)
      print("Timeseries existing in Aquarius have been downloaded and appended to the local database.")
  }

  #Refresh the last 18 months with realtime data in case there were changes, or to incorporate new stations. At the same time get the station name from tidyhydat.
  new_realtime <- list(flow = list(), level = list())
  for (i in WSC_stns){
    token <- tidyhydat.ws::token_ws(username = Sys.getenv("WS_USRNM"), password = Sys.getenv("WS_PWD"))
    try(new_realtime$flow[[i]] <- tidyhydat.ws::realtime_ws(i, 47, start_date = Sys.Date()-577, end_date = Sys.Date(), token = token))
    try(new_realtime$level[[i]] <- tidyhydat.ws::realtime_ws(i, 46, start_date = Sys.Date()-577, end_date = Sys.Date(), token = token))
  }

  #keep only what's necessary from the raw download (drop columns) and format columns; at the same time, make entries in the locations table
  for (i in names(new_realtime$flow)){
    new_realtime$flow[[i]] <- new_realtime$flow[[i]][,c(2,4,1)]
    names(new_realtime$flow[[i]]) <- c("datetime_UTC", "value", "location")
    new_realtime$flow[[i]]$approval <- "preliminary"
    new_realtime$flow[[i]]$units <- "m3/s"
    new_realtime$flow[[i]]$datetime_UTC <- as.character(new_realtime$flow[[i]]$datetime_UTC)
    start_AQ <- as.character(min(aqFlow[[i]]$timeseries$timestamp_UTC))
    start_ws <- min(new_realtime$flow[[i]]$datetime_UTC)
    start_datetime <- hablar::rationalize(min(c(start_AQ, start_ws)))
    existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", i, "' AND data_type = 'flow'"))
    name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
    if (nrow(existing) == 0){
      location_info <- data.frame("location" = i, "name" = name, "data_type" = "flow", "start_datetime" = start_datetime, "end_datetime" = max(new_realtime$flow[[i]]$datetime_UTC), "latitude" = tidyhydat::hy_stations(i)$LATITUDE, "longitude" = tidyhydat::hy_stations(i)$LONGITUDE, operator = "WSC", network = "Canada Yukon Hydrometric Network")
      DBI::dbAppendTable(hydro, "locations", location_info)
    } else {
      DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", max(new_realtime$flow[[i]]$datetime_UTC), "' WHERE location = '", i, "' AND data_type = 'flow'"))
    }
  }

  for (i in names(new_realtime$level)){
    new_realtime$level[[i]] <- new_realtime$level[[i]][,c(2,4,1)]
    names(new_realtime$level[[i]]) <- c("datetime_UTC", "value", "location")
    new_realtime$level[[i]]$approval <- "preliminary"
    new_realtime$level[[i]]$units <- "m"
    new_realtime$level[[i]]$datetime_UTC <- as.character(new_realtime$level[[i]]$datetime_UTC)
    start_AQ <- as.character(min(aqLevel[[i]]$timeseries$timestamp_UTC))
    start_ws <- min(new_realtime$level[[i]]$datetime_UTC)
    start_datetime <- hablar::rationalize(min(c(start_AQ, start_ws)))
    existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", i, "' AND data_type = 'level'"))
    name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
    if (nrow(existing) == 0){
      location_info <- data.frame("location" = i, "name" = name, "data_type" = "level", "start_datetime" = start_datetime, "end_datetime" = max(new_realtime$level[[i]]$datetime_UTC), "latitude" = tidyhydat::hy_stations(i)$LATITUDE, "longitude" = tidyhydat::hy_stations(i)$LONGITUDE, operator = "WSC", network = "Canada Yukon Hydrometric Network")
      DBI::dbAppendTable(hydro, "locations", location_info)
    } else {
      DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", max(new_realtime$level[[i]]$datetime_UTC), "' WHERE location = '", i, "' AND data_type = 'level'"))
    }
  }

  new_flow_rt <- do.call("rbind", new_realtime$flow)
  rownames(new_flow_rt) <- NULL
  new_level_rt <- do.call("rbind", new_realtime$level)
  rownames(new_level_rt) <- NULL

  #Delete the entries and then append the new data into the database. This has the added bonus of adding new rows as well as replacing existing rows
  delete_bracket <- c(min(new_flow_rt$datetime_UTC), max(new_flow_rt$datetime_UTC))
  DBI::dbExecute(hydro, paste0("DELETE FROM flow_realtime WHERE datetime_UTC BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "'")) #NOTE: SQL BETWEEN is inclusive
  DBI::dbAppendTable(hydro, "flow_realtime", new_flow_rt)

  delete_bracket <- c(min(new_level_rt$datetime_UTC), max(new_level_rt$datetime_UTC))
  DBI::dbExecute(hydro, paste0("DELETE FROM level_realtime WHERE datetime_UTC BETWEEN '", delete_bracket[1], "' AND '", delete_bracket[2], "'")) #NOTE: SQL BETWEEN is inclusive
  DBI::dbAppendTable(hydro, "level_realtime", new_level_rt)
  print("Timeseries existing in the WSC real-time database have been downloaded and appended to the local database.")


  #Now deal with historical "HYDAT" information, inserting into measurement tables and adding entry to locations table
  for (i in WSC_stns){
    tryCatch({
      level <- tidyhydat::hy_daily_levels(i)[,-c(3,5)]
      colnames(level) <- c("location", "date", "value")
      level$approval <- "approved"
      level$units <- "m"
      level$date <- as.character(level$date)
      DBI::dbAppendTable(hydro, "level_daily", level)

      start_datetime <- paste0(min(level$date), " 00:00:00")
      existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", i, "' AND data_type = 'level'"))
      if (nrow(existing) == 0){ #no corresponding realtime station yet
        end_datetime <- paste0(max(level$date), " 00:00:00")
        latitude <- tidyhydat::hy_stations(i)$LATITUDE
        longitude <- tidyhydat::hy_stations(i)$LONGITUDE
        name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
        DBI::dbExecute(hydro, paste0("INSERT INTO locations (location, name, data_type, start_datetime, end_datetime, latitude, longitude, operator, network) VALUES ('", i, "', '", name, "', 'level', '", start_datetime, "', '", end_datetime, "', ", latitude, ", ", longitude, ", 'WSC', 'Canada Yukon Hydrometric Network')"))
      } else if (is.null(existing$end_datetime)) {
        end_datetime <- paste0(max(level$date), " 00:00:00")
        DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE location = '", i, "' AND data_type = 'level'"))
      } else {
        DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "' WHERE location = '", i, "' AND data_type = 'level'"))
      }
    }, error = function(e){
      print(paste0("No level for station ", i))
      }
    )
    tryCatch ({
      flow <- tidyhydat::hy_daily_flows(i)[,-c(3,5)]
      colnames(flow) <- c("location", "date", "value")
      flow$approval <- "approved"
      flow$units <- "m3/s"
      flow$date <- as.character(flow$date)
      DBI::dbAppendTable(hydro, "flow_daily", flow)

      start_datetime <- paste0(min(flow$date), " 00:00:00")
      existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM locations WHERE location = '", i, "' AND data_type = 'flow'"))
      if (nrow(existing) == 0){ #no corresponding realtime station yet
        end_datetime <- paste0(max(flow$date), " 00:00:00")
        latitude <- tidyhydat::hy_stations(i)$LATITUDE
        longitude <- tidyhydat::hy_stations(i)$LONGITUDE
        name <- stringr::str_to_title(tidyhydat::hy_stations(i)$STATION_NAME)
        DBI::dbExecute(hydro, paste0("INSERT INTO locations (location, name, data_type, start_datetime, end_datetime, latitude, longitude, operator, network) VALUES ('", i, "', '", name, "', 'flow', '", start_datetime, "', '", end_datetime, "', '", latitude, "', '", longitude, "', 'WSC', 'Canada Yukon Hydrometric Network')"))
      } else if (is.null(existing$end_datetime)) {
        end_datetime <- paste0(max(flow$date), " 00:00:00")
        DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE location = '", i, "' AND data_type = 'flow'"))
      } else {
        DBI::dbExecute(hydro, paste0("UPDATE locations SET start_datetime = '", start_datetime, "' WHERE location = '", i, "' AND data_type = 'flow'"))
      }
    }, error = function(e) {
      print(paste0("No flows for station ", i))
    }
    )
  } #End of for loop adding historical WSC data
  print("Timeseries existing in the WSC historical database (HYDAT) have been appended to the local database.")

} #End of function
