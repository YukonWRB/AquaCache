#' Snow course data to database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' **This fuction works with the Microsoft Access snow survey database, circa pre spring 2023**. This function must be refreshed once this database changes.
#'
#' Pulls and processes snow course data before updating entries into the local hydrometric database.
#'
#' @param hydro_db_path Full path to the hydrology database
#' @param snow_db_path Full path to the snow survey database, passed to [YGWater::snowConnect()].
#' @param inactive Should inactive stations be included?
#' @param overwrite Should all snow course information be overwritten? Only do this if there's been a change in the snow_survey database and you deem this to be the fastest way to reflect that change in the hydro database.
#'
#' @return The hydro database is updated in place.
#' @export
#'

getSnowCourse <- function(hydro_db_path, snow_db_path = "//carver/infosys/Snow/DB/SnowDB.mdb", inactive = FALSE, overwrite = FALSE){


  hydro <- YGWater::hydroConnect(path = hydro_db_path, silent = TRUE)
  on.exit(DBI::dbDisconnect(hydro), add=TRUE)

  tables <- DBI::dbListTables(hydro)
  DBI::dbDisconnect(hydro)
  if (!("discrete" %in% tables)){
    print("The table 'discrete' is being created in the database using function initial_create.")
    initial_create(path = hydro_db_path, extras = "snow courses", overwrite = FALSE)
  }

  snowCon <- YGWater::snowConnect_access(path = snow_db_path, silent = TRUE)
  on.exit(DBI::dbDisconnect(snowCon), add=TRUE)

  #Get locations and measurements
  locations <- DBI::dbReadTable(snowCon, "SNOW_COURSE")
  meas <- DBI::dbGetQuery(snowCon, paste0("SELECT * FROM SNOW_SAMPLE WHERE SNOW_COURSE_ID IN ('", paste(locations$SNOW_COURSE_ID, collapse = "', '"), "')"))
  DBI::dbDisconnect(snowCon)

  #Manipulate/preprocess things a bit
  meas <- meas[which(meas$EXCLUDE_FLG==0),] # OMIT VALUES OF EXCLUDEFLG=1, aka TRUE
  meas$SAMPLE_DATE <- as.Date(meas$SAMPLE_DATE)
  meas$year <- lubridate::year(meas$SAMPLE_DATE)
  meas$month <- lubridate::month(meas$SAMPLE_DATE)
  #Fix up the location metadata table
  locations$LATITUDE_SEC[is.na(locations$LATITUDE_SEC)] <- as.numeric(0)
  latitude <- locations$LATITUDE_DEG + locations$LATITUDE_MIN/60 + locations$LATITUDE_SEC/3600
  locations$LONGITUDE_SEC[is.na(locations$LONGITUDE_SEC)] <- as.numeric(0)
  longitude <- -1 * abs(locations$LONGITUDE_DEG + locations$LONGITUDE_MIN/60 + locations$LONGITUDE_SEC/3600) #currently all longitudes are not negative, but if it changes then -1*abs will ensure all are rendered negative regardless.
  locations <- locations[ , c("SNOW_COURSE_ID", "SNOW_COURSE_NAME", "ACTIVE_FLG", "ELEVATION")]
  locations <- cbind(locations, latitude, longitude)

  #Deal with special cases - correction factors
  # Special case (i) Twin Creeks: 09BA-SC02B will take precedence over A from 2016 onwards.
  subset <- meas[meas$SNOW_COURSE_ID %in% c("09BA-SC02A", "09BA-SC02B"),]
  duplicated <- data.frame(table(subset$SAMPLE_DATE))
  duplicated <- duplicated[duplicated$Freq > 1 , ]
  duplicated <- as.Date(as.vector(duplicated$Var1))
  swe_factor <- NULL
  for (i in 1:length(duplicated)){ # Calculate correction factors:
    a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
    b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
    swe_factor[i] <- 1 + (b-a)/a
  }
  depth_factor <- NULL
  for (i in 1:length(duplicated)){
    a <- subset[subset$SNOW_COURSE_ID == "09BA-SC02A" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
    b <- subset[subset$SNOW_COURSE_ID == "09BA-SC02B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
    depth_factor[i] <- 1 + (b-a)/a
  }
  swe_correction <- mean(swe_factor)
  depth_correction <- mean(depth_factor)
  meas <- meas[!(meas$SNOW_COURSE_ID=="09BA-SC02A" & meas$year >= 2016),] # Remove 09BA-SC02A values in 2016 and later.
  meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- swe_correction*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="09BA-SC02A"]) # Multiply all 09BA-SC02A values by correction factors
  meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- depth_correction*(meas$DEPTH[meas$SNOW_COURSE_ID=="09BA-SC02A"])
  meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="09BA-SC02A"] <- "09BA-SC02B" #Rename as 09BA-SC02B (A will no longer exist here)
  locations <- locations[!(locations$SNOW_COURSE_ID == "09BA-SC02A") , ]

  # Special case (ii) Hyland 10AD-SC01 and 10AD-SC01B. B will take precedence over (no letter) from 2018 onwards.
  subset <- meas[meas$SNOW_COURSE_ID %in% c("10AD-SC01", "10AD-SC01B"),]
  duplicated <- data.frame(table(subset$SAMPLE_DATE))
  duplicated <- duplicated[duplicated$Freq > 1 , ]
  duplicated <- as.Date(as.vector(duplicated$Var1))
  swe_factor <- NULL
  for (i in 1:length(duplicated)){ # Calculate correction factors
    a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
    b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "SNOW_WATER_EQUIV"]
    swe_factor[i] <- 1 + (b-a)/a
  }
  depth_factor <- NULL
  for (i in 1:length(duplicated)){
    a <- subset[subset$SNOW_COURSE_ID == "10AD-SC01" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
    b <- subset[subset$SNOW_COURSE_ID == "10AD-SC01B" & subset$SAMPLE_DATE == duplicated[i], "DEPTH"]
    depth_factor[i] <- 1 + (b-a)/a
  }
  swe_correction <- mean(swe_factor)
  depth_correction <- mean(depth_factor)
  meas <- meas[!(meas$SNOW_COURSE_ID=="10AD-SC01" & meas$year>=2018),] #Remove SC01 blank values in 2018 and later.
  meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"] <- swe_correction*(meas$SNOW_WATER_EQUIV[meas$SNOW_COURSE_ID=="10AD-SC01"]) #Multiply all remaining SC01 values by correction factors
  meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"] <- depth_correction*(meas$DEPTH[meas$SNOW_COURSE_ID=="10AD-SC01"])
  meas$SNOW_COURSE_ID[meas$SNOW_COURSE_ID=="10AD-SC01"] <- "10AD-SC01B" #Step 3: Rename as 010AD-SC01B (blank will no longer exist)
  locations <- locations[!(locations$SNOW_COURSE_ID == "10AD-SC01") , ]

  if (!inactive){ #Filter out the inactive stations if inactive is FALSE
    remove <- locations[locations$ACTIVE_FLG==TRUE,]$SNOW_COURSE_ID
    meas <- meas[meas$SNOW_COURSE_ID %in% remove , ]
    locations <- locations[locations$ACTIVE_FLG == TRUE ,]
  }

  hydro <- YGWater::hydroConnect(path = hydro_db_path, silent = TRUE)
  for (i in 1:nrow(locations)){
    #get new measurements
    if (overwrite){
      DBI::dbExecute(hydro, paste0("DELETE FROM discrete WHERE parameter IN ('snow depth', 'SWE') AND location = '", locations$SNOW_COURSE_ID[i], "'"))
      DBI::dbExecute(hydro, paste0("DELETE FROM timeseries WHERE parameter IN ('snow depth', 'SWE') AND category = 'discrete' AND location = '", locations$SNOW_COURSE_ID[i], "'"))
      DBI::dbExecute(hydro,  paste0("DELETE FROM locations WHERE location = '", locations$SNOW_COURSE_ID[i], "'"))
      DBI::dbExecute(hydro, paste0("DELETE FROM datum_conversions WHERE location = '", locations$SNOW_COURSE_ID[i], "'"))
    }
    df <- meas[meas$SNOW_COURSE_ID == locations$SNOW_COURSE_ID[i] , ]
    existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM discrete WHERE location = '", locations$SNOW_COURSE_ID[i], "'"))
    df <- df[!(as.character(df$SAMPLE_DATE) %in% existing$target_date) ,] #Retain only new survey dates
    if (nrow(df) > 0){
      SWE <- data.frame("location" = df$SNOW_COURSE_ID, "target_date" = df$SAMPLE_DATE, "datetime" = df$SURVEY_DATE, "value" = df$SNOW_WATER_EQUIV, "parameter" = "SWE")
      depth <- data.frame("location" = df$SNOW_COURSE_ID, "target_date" = df$SAMPLE_DATE, "datetime" = df$SURVEY_DATE, "value" = df$DEPTH, "parameter" = "snow depth")
      df <- rbind(SWE, depth) #bring SWE and DEPTH back together in a 'long' format
      df[is.na(df$datetime) , ]$datetime <- df[is.na(df$datetime), ]$target_date #sometimes no survey data is given, but there is a target date. Presumably no field visit was conducted because there was 100% chance of no snow.
      df <- df[!is.na(df$value), ] #remove rows where there is no measurement
      if (nrow(df) > 0){ #condition again, in case na rows were removed
        df$target_date <- as.character(df$target_date)
        df$datetime <- as.character(df$datetime)
        # insert new measurements to the DB
        DBI::dbAppendTable(hydro, "discrete", df)
        # update timeseries table
        start <- as.character(min(df$target_date))
        end <- as.character(max(df$target_date))
        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO timeseries (location, parameter, unit, category, start_datetime_UTC, end_datetime_UTC, last_new_data_UTC, operator, network) VALUES ('", locations$SNOW_COURSE_ID[i], "', 'SWE', 'mm', 'discrete', '", start, "', '", end, "', '", as.character(.POSIXct(Sys.time(), "UTC")), "', 'WRB', 'Snow Survey Network')"))
        DBI::dbExecute(hydro, paste0("UPDATE timeseries SET end_datetime_UTC = '", end, "', last_new_data_UTC = '", as.character(.POSIXct(Sys.time(), "UTC")), "' WHERE location = '", locations$SNOW_COURSE_ID[i], "' AND parameter = 'SWE' AND category = 'discrete'"))

        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO timeseries (location, parameter, unit, category, start_datetime_UTC, end_datetime_UTC, last_new_data_UTC, operator, network) VALUES ('", locations$SNOW_COURSE_ID[i], "', 'snow depth', 'cm', 'discrete', '", start, "', '", end, "', '", as.character(.POSIXct(Sys.time(), "UTC")), "', 'WRB', 'Snow Survey Network')"))
        DBI::dbExecute(hydro, paste0("UPDATE timeseries SET end_datetime_UTC = '", end, "', last_new_data_UTC = '", as.character(.POSIXct(Sys.time(), "UTC")), "' WHERE location = '", locations$SNOW_COURSE_ID[i], "' AND parameter = 'snow depth' AND category = 'discrete'"))

        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO locations (location, name, latitude, longitude) VALUES ('", locations$SNOW_COURSE_ID[i], "', '", gsub("'", "", locations$SNOW_COURSE_NAME[i]), "', '", locations$latitude[i], "', '", locations$longitude[i], "')"))

        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO datum_conversions (location, datum_id_from, datum_id_to, conversion_m, current) VALUES ('", locations$SNOW_COURSE_ID[i], "', ' 10', '110', '", locations$ELEVATION[i], "', 'TRUE')"))
      }
    }
  } #End of for loop iterating over locations
  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_snow_courses'"))
  DBI::dbDisconnect(hydro)
  print("Snow course survey data is updated in the database.")
}
