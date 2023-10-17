# Data migration from old SQLite to new postgres DB

load_all() #to attache this package
# 1. deal with the timeseries, locations, and datum tables.
# get each distinct timeseries from the timeseries DB, in list elements.
oldcon <- WRBtools::hydroConnect()
newcon <- hydrometConnect()
ts_con <- DBI::dbGetQuery(oldcon, "SELECT location, parameter, units, operator, network FROM timeseries WHERE type = 'continuous'")
ts_con$param_type <- "hydrometric"
ts_con[ts_con$parameter %in% c("snow depth", "SWE"), "param_type"] <- "meteorological"
ts_con$source_fx <- "getRealtimeWSC"
ts_con[ts_con$operator == "WRB" , "source_fx"] <- "getRealtimeAquarius"
ts_con$source_fx_args <- NA
ts_con$public <- TRUE
ts_con$category <- "continuous"
ts_con$period_type <- "instantaneous"
names(ts_con)[names(ts_con) == "units"] <- "unit"


ts_disc <- DBI::dbGetQuery(oldcon, "SELECT location, parameter, units, operator, network FROM timeseries WHERE type = 'discrete'")
ts_disc$param_type <- "meteorological"
ts_disc$source_fx <- "getSnowCourse"
ts_disc$source_fx_args <- NA
ts_disc$public <- TRUE
ts_disc$category <- "discrete"
ts_disc$period_type <- "instantaneous"
names(ts_disc)[names(ts_disc) == "units"] <- "unit"

# Transfer over the locations table and add the new point column
locs <- DBI::dbGetQuery(oldcon, paste0("SELECT * from locations;"))
DBI::dbAppendTable(newcon, "locations", locs)
DBI::dbExecute(newcon, "UPDATE locations SET point = ST_SetSRID(ST_MakePoint(longitude, latitude), 4269) WHERE point IS NULL;")
# Transfer over the datums
datums <- DBI::dbGetQuery(oldcon, "SELECT * FROM datum_conversions")
datums <- datums[datums$location != "09AB008", ] # for some reason it's no present in the locations table, will have to be added in later
DBI::dbAppendTable(newcon, "datum_conversions", datums)

# Transfer over the newly created timeseries


for (i in 1:nrow(ts_con)){
  ts <- ts_con[i, ]
  data <- DBI::dbGetQuery(oldcon, paste0("SELECT datetime_UTC, value, grade, approval FROM realtime WHERE location = '", ts$location, "' AND parameter = '", ts$parameter, "';"))
  if (nrow(data) > 0){
    DBI::dbAppendTable(newcon, "timeseries", ts)
    tsid <- DBI::dbGetQuery(newcon, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", ts$location, "' AND parameter = '", ts$parameter, "';"))[1,1]
    names(data)[names(data) == "datetime_UTC"] <- "datetime"
    data$datetime <- as.POSIXct(data$datetime, tz = "UTC")
    data$timeseries_id <- tsid
    data$period <- "00:00:00"
    data$imputed <- FALSE
    DBI::dbAppendTable(newcon, "measurements_continuous", data)
    start_datetime <- min(data$datetime)
    end_datetime <- max(data$datetime)
    DBI::dbExecute(newcon, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE timeseries_id = ", tsid, ";"))
  } else {
    warning("No data found for location ", ts_con[i, "location"], ", parateter ", ts_con[i, "parameter"])
  }
}

for (i in 1:nrow(ts_disc)){
  ts <- ts_disc[i, ]
  if (ts$parameter == "SWE"){
    SWE_data <- DBI::dbGetQuery(oldcon, paste0("SELECT target_date, sample_date, value FROM discrete WHERE parameter = 'SWE' AND location = '", ts$location, "';"))
    depth_data <- data.frame()
  } else {
    depth_data <-DBI::dbGetQuery(oldcon, paste0("SELECT target_date, sample_date, value FROM discrete WHERE parameter = 'snow depth' AND location = '", ts$location, "';"))
    SWE_data <- data.frame()
  }

  if (ts$parameter == "SWE"){
    if (nrow(SWE_data) > 0){
      DBI::dbAppendTable(newcon, "timeseries", ts)
      tsid <- DBI::dbGetQuery(newcon, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", ts$location, "' AND parameter = '", ts$parameter, "';"))[1,1]
      names(SWE_data)[names(SWE_data) == "target_date"] <- "target_datetime"
      SWE_data$target_datetime <- as.POSIXct(SWE_data$target_datetime, tz = "UTC")
      names(SWE_data)[names(SWE_data) == "sample_date"] <- "datetime"
      SWE_data$datetime <- as.POSIXct(SWE_data$datetime, tz = "UTC")
      SWE_data$timeseries_id <- tsid
      DBI::dbAppendTable(newcon, "measurements_discrete", SWE_data)
      start_datetime <- min(SWE_data$datetime)
      end_datetime <- max(SWE_data$datetime)
      DBI::dbExecute(newcon, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE timeseries_id = ", tsid, ";"))
    } else {
      warning("No SWE data found for location ", ts_con[i, "location"], ", parameter ", ts_con[i, "parameter"])
    }
  } else {
    if (nrow(depth_data) > 0){
      DBI::dbAppendTable(newcon, "timeseries", ts)
      tsid <- DBI::dbGetQuery(newcon, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", ts$location, "' AND parameter = '", ts$parameter, "';"))[1,1]
      names(depth_data)[names(depth_data) == "target_date"] <- "target_datetime"
      depth_data$target_datetime <- as.POSIXct(depth_data$target_datetime, tz = "UTC")
      names(depth_data)[names(depth_data) == "sample_date"] <- "datetime"
      depth_data$datetime <- as.POSIXct(depth_data$datetime, tz = "UTC")
      depth_data$timeseries_id <- tsid
      DBI::dbAppendTable(newcon, "measurements_discrete", depth_data)
      start_datetime <- min(depth_data$datetime)
      end_datetime <- max(depth_data$datetime)
      DBI::dbExecute(newcon, paste0("UPDATE timeseries SET start_datetime = '", start_datetime, "', end_datetime = '", end_datetime, "' WHERE timeseries_id = ", tsid, ";"))
    } else {
      warning("No SWE data found for location ", ts_con[i, "location"], ", parameter ", ts_con[i, "parameter"])
    }
  }
}

#Now bring in the hydat data
update_hydat(newcon, force_update = TRUE)
#And synchronize the realtime timeseries
synchronizeContinuous(newcon, start_datetime = Sys.time()-1000*24*60*60)
#NOTE both functions above call calculate_stats where needed.

# 4. Add watershed polygons to the DB
basins <- terra::vect("G:/water/Common_GW_SW/Data/basins/drainage_polygons.shp")
basins <- basins[, c(1,2)]
names(basins) <- c("name", "description")
basins$polygon_type <- "drainage_basin"

for (i in 1:nrow(basins)){
  addVector(newcon, basins[i, ], "polygons")
}
