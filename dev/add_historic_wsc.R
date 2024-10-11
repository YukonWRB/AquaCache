# Getting additional water level, flow, and temperature data from WSC to AquaCache

con <- AquaConnect()

# retrieve the owner ID form the owners_contributors table
owner_id <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada'")[1,1]
if (is.na(owner_id)) {
  DBI::dbExecute(con, "INSERT INTO owners_contributors (name) VALUES ('Water Survey of Canada')")
  owner_id <- DBI::dbGetQuery(con, "SELECT owner_contributor_id FROM owners_contributors WHERE name = 'Water Survey of Canada'")[1,1]
}

# 1. Temperature ####

# NOTE!!! This portion does not create new locations in the database. See line 90 for a suggestion on how to do this.

# Set the path to the folder containing all temp data sheets
path <- "C:/Users/gtdelapl/OneDrive - Government of Yukon/Desktop/Water temp"

files <- list.files(path)
files <- data.frame(path = paste0(path, files), 
                    loc = substr(files, 1, 7))
exist_locs <- DBI::dbGetQuery(con, "SELECT DISTINCT location, location_id FROM locations")

for (i in 1:nrow(files)) {
  if (files$loc[i] %in% exist_locs$location) {
    print(paste0(files$loc[i], " already exists, proceeding"))
    exist_ts <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", files$loc[i], "' AND parameter_id = 1251"))
    
    data <- read.csv(files$path[i], header = TRUE, sep = ",", skip = 11)
    if (nrow(data) == 0) {
      print(paste0("No data found for ", files$loc[i], ", skipping"))
      next
    }
    data$TimeStamp <- as.POSIXct(data$TimeStamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    data$TimeStamp <- data$TimeStamp + 8*3600
    names(data) <- c("datetime", "value")
    if (nrow(exist_ts) == 1) {
      # Add a bunch of cols to the df for compatibility with the database
      data$timeseries_id <- exist_ts$timeseries_id
      data$owner <- owner_id
      data$contributor <- owner_id
      data$period <- 0
      data$approval <- 5
      data$qualifier <- 8
      
      # Delete existing data and replace with new data
      DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", exist_ts$timeseries_id, " AND datetime >= '", min(data$datetime), "' AND datetime <= '", max(data$datetime), "'"))
      DBI::dbWriteTable(con, "measurements_continuous", data, append = TRUE)
      
      # adjust start and end dates
      exist_end_dt <- DBI::dbGetQuery(con, paste0("SELECT end_datetime FROM timeseries WHERE timeseries_id = ", exist_ts$timeseries_id))
      exist_start_dt <- DBI::dbGetQuery(con, paste0("SELECT start_datetime FROM timeseries WHERE timeseries_id = ", exist_ts$timeseries_id))
      if (exist_end_dt$end_datetime < max(data$datetime)) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", max(data$datetime), "' WHERE timeseries_id = ", exist_ts$timeseries_id))
      }
      if (exist_start_dt$start_datetime > min(data$datetime)) {
        DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(data$datetime), "' WHERE timeseries_id = ", exist_ts$timeseries_id))
      }
      
      # Recalculate stats
      calculate_stats(con, exist_ts$timeseries_id, min(data$datetime))
      
    } else if (nrow(exist_ts) == 0) {
      print(paste0("No time series found for ", files$loc[i], " but location exists, creating new time series"))
      add_ts <- data.frame(location = files$loc[i],
                           parameter_id = 1251,
                           media_id = 1,
                           category = "continuous",
                           period_type = "instantaneous",
                           start_datetime = min(data$datetime),
                           end_datetime = max(data$datetime),
                           source_fx = "downloadWSC",
                           record_rate = "< 1 day",
                           location_id = exist_locs$location_id[exist_locs$location == files$loc[i]],
                           owner = 1)
      DBI::dbWriteTable(con, "timeseries", add_ts, append = TRUE)
      add_tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", files$loc[i], "' AND parameter_id = 1251"))
      
      data$timeseries_id <- add_tsid$timeseries_id
      data$owner <- owner_id
      data$contributor <- owner_id
      data$period <- 0
      data$approval <- 5
      data$qualifier <- 8
      DBI::dbWriteTable(con, "measurements_continuous", data, append = TRUE)
      
      # Get new data if exists
      getNewContinuous(con, add_tsid$timeseries_id)
      
      # Recalculate stats
      calculate_stats(con, add_tsid$timeseries_id, min(data$datetime))
      

    } else {
      print(paste0("Multiple time series found for ", files$loc[i], ", skipping"))
    }
    
  } else {
    # If desired, you could add a loop to add the location to the database here. I suggest using AquaCache::addACLocation(), populating the parameters with info taken directly from HYDAT.
    print(paste0("Location ", files$loc[i], " does not exist, skipping"))
  }
}


# 2. Water level ####
locs_level <- DBI::dbGetQuery(con, "SELECT DISTINCT location, timeseries_id FROM timeseries WHERE parameter_id = 1165 AND source_fx = 'downloadWSC'")

for (i in 1:nrow(locs_level)) {
  
  message(i, " of ", nrow(locs_level))
  
  code <- substr(locs_level$location[i], 1, 2)
  loc <- substr(locs_level$location[i], 1,7)
  
  url <- paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/UnitValueData/Stage/corrected/", code, "/Stage.Working@", loc, ".20110101_corrected.csv.xz")
  
  success <- TRUE
  tryCatch({
    download.file(url, destfile = paste0(tempdir(), "/level_", loc, ".csv.xz"))
  }, error = function(e) {
    print(paste0("No level data for location ", loc, ", skipping"))
    success <<- FALSE
  })
  if (!success) {
    next
  }
  data <- read.csv(paste0(tempdir(), "/level_", loc, ".csv.xz"), header = TRUE, sep = ",", skip = 14)
  
  # drop UTC-8 column and rename cols
  data <- data[, -c(2,6)]
  names(data) <- c("datetime", "value", "approval", "qualifier")
  data <- data[!is.na(data$value), ]
  data$datetime <- lubridate::parse_date_time(data$datetime, orders = c("ymd_HMS", "ymd_HMSz", "ymd_HMS3z"), tz = "UTC")
  data <- data[!is.na(data$datetime), ]
  
  if (nrow(data) == 0) {
    print(paste0("No level data for location ", loc, ", skipping"))
    next
  }
  
  data$qualifier <- as.character(data$qualifier)
  # Map qualifiers to AquaCache codes
  data$qualifier[data$qualifier == "-1"] <- "7"
  data$qualifier[data$qualifier == "10"] <- "1"
  data$qualifier[data$qualifier == "20"] <- "4"
  data$qualifier[data$qualifier == "30"] <- "7"
  data$qualifier[data$qualifier == "40"] <- "2"
  data$qualifier[data$qualifier == "50"] <- "8"
  data$qualifier[data$qualifier == ""] <- "8"
  data$qualifier[data$qualifier == "-2"] <- "8"
  data$qualifier[data$qualifier == "0"] <- "8"
  
  # Same for approvals
  data$approval[data$approval == "Approved"] <- "1"
  data$approval[data$approval == "Reviewed"] <- "3"
  data$approval[data$approval == "Preliminary"] <- "4"
  data$approval[data$approval == ""] <- "6"
  data$approval[data$approval == "Checked"] <- "3"
  data$approval[data$approval == "Unspecified"] <- "5"
  data$approval[data$approval == "Undefined"] <- "5"
  
  data$period <- 0
  data$owner <- owner_id
  data$contributor <- owner_id
  data$timeseries_id <- locs_level$timeseries_id[i]
  
  # Replace existing data
  DBI::dbWithTransaction(
    con, 
    {
      DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", locs_level$timeseries_id[i], " AND datetime >= '", min(data$datetime), "' AND datetime <= '", max(data$datetime), "';"))
      DBI::dbWriteTable(con, "measurements_continuous", data, append = TRUE)
    }
  )
  print(paste0("Level data for ", loc, " added"))
}

# 3. Flow ####

locs_flow <- DBI::dbGetQuery(con, "SELECT DISTINCT location, timeseries_id FROM timeseries WHERE parameter_id = 1150 AND source_fx = 'downloadWSC'")

for (i in 1:nrow(locs_flow)) {
  
  message(i, " of ", nrow(locs_flow))
  
  code <- substr(locs_flow$location[i], 1, 2)
  loc <- substr(locs_flow$location[i], 1,7)
  
  url <- paste0("https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/UnitValueData/Discharge/corrected/", code, "/Discharge.Working@", loc, ".20110101_corrected.csv.xz")
  
  success <- TRUE
  tryCatch({
    download.file(url, destfile = paste0(tempdir(), "/flow_", loc, ".csv.xz"))
  }, error = function(e) {
    print(paste0("No flow data for location ", loc, ", skipping"))
    success <<- FALSE
  })
  if (!success) {
    next
  }
  
  data <- read.csv(paste0(tempdir(), "/flow_", loc, ".csv.xz"), header = TRUE, sep = ",", skip = 14)
  file.remove(paste0(tempdir(), "/flow_", loc, ".csv.xz"))
  if (nrow(data) == 0) {
    print(paste0("No Flow data for location ", loc, ", skipping"))
    next
  }
  
  # drop UTC-8 column and rename cols
  data <- data[, -c(2,6)]
  names(data) <- c("datetime", "value", "approval", "qualifier")
  data <- data[!is.na(data$value), ]
  data$datetime <- lubridate::parse_date_time(data$datetime, orders = c("ymd_HMS", "ymd_HMSz", "ymd_HMS3z"), tz = "UTC")
  data <- data[!is.na(data$datetime), ]
  
  if (nrow(data) == 0) {
    print(paste0("No level data for location ", loc, ", skipping"))
    next
  }
  
  data$qualifier <- as.character(data$qualifier)
  # Map qualifiers to AquaCache codes
  data$qualifier[data$qualifier == "-1"] <- "7"
  data$qualifier[data$qualifier == "10"] <- "1"
  data$qualifier[data$qualifier == "20"] <- "4"
  data$qualifier[data$qualifier == "30"] <- "7"
  data$qualifier[data$qualifier == "40"] <- "2"
  data$qualifier[data$qualifier == "50"] <- "8"
  data$qualifier[data$qualifier == ""] <- "8"
  data$qualifier[data$qualifier == "-2"] <- "8"
  data$qualifier[data$qualifier == "0"] <- "8"
  
  # Same for approvals
  data$approval[data$approval == "Approved"] <- "1"
  data$approval[data$approval == "Reviewed"] <- "3"
  data$approval[data$approval == "Preliminary"] <- "4"
  data$approval[data$approval == ""] <- "6"
  data$approval[data$approval == "Checked"] <- "3"
  data$approval[data$approval == "Unspecified"] <- "5"
  data$approval[data$approval == "Undefined"] <- "5"
  
  
  data$period <- 0
  data$owner <- owner_id
  data$contributor <- owner_id
  data$timeseries_id <- locs_flow$timeseries_id[i]
  
  # Replace existing data
  DBI::dbWithTransaction(
    con, 
    {
      DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", locs_flow$timeseries_id[i], " AND datetime >= '", min(data$datetime), "' AND datetime <= '", max(data$datetime), "';"))
      
      DBI::dbWriteTable(con, "measurements_continuous", data, append = TRUE)
    }
  )
  AquaCache::synchronize(con = con, timeseries_id = locs_flow$timeseries_id[i], start_datetime = "2010-12-31")
  print(paste0("Flow data for ", loc, " added"))
}
