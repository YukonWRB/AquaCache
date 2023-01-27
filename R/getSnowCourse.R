
#' Snow course data to database
#'
#' Pulls and processes snow course data before updating entries into the local hydrometric database. Calls function WRBtools::snowInfo for the bulk of the work.
#'
#' @param hydro_db_path Full path to the hydrology database
#' @param snow_db_path Full path to the snow survey database
#' @param inactive Should inactive stations be included?
#' @param overwrite Should all snow course information be overwritten? Only do this if there's been a change in the snow_survey database and you deem this to be the fastest way to reflect that change in the hydro database.
#'
#' @return The hydro database is updated in place.
#' @export
#'

getSnowCourse <- function(hydro_db_path, snow_db_path = "X:/Snow/DB/SnowDB.mdb", inactive = FALSE, overwrite = FALSE){


  hydro <- DBI::dbConnect(RSQLite::SQLite(), hydro_db_path)
  on.exit(DBI::dbDisconnect(hydro), add=TRUE)
  DBI::dbExecute(hydro, "PRAGMA busy_timeout=100000")

  tables <- DBI::dbListTables(hydro)
  if (!("snow_courses" %in% tables)){
    print("The table 'snow_courses' is being created in the database using function initial_create.")
    initial_create(path = hydro_db_path, extras = "snow courses", overwrite = FALSE)
  }

  data <- WRBtools::snowInfo(db_path = snow_db_path, locations = "all", inactive = inactive, save_path = NULL, stats = FALSE, plots = FALSE, quiet = TRUE)

  for (i in 1:nrow(data$locations)){
    #get new measurements
    if (overwrite){
      DBI::dbExecute(hydro, paste0("DELETE FROM snow_courses WHERE location = '", data$locations$location_ID[i], "'"))
      DBI::dbExecute(hydro, paste0("DELETE FROM locations WHERE location = '", data$locations$location_ID[i], "'"))
    }
    df <- data$measurements[data$measurements$SNOW_COURSE_ID == data$locations$location_ID[i] , ]
    existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM snow_courses WHERE location = '", data$locations$location_ID[i], "'"))
    df <-  df[!(as.character(df$SAMPLE_DATE) %in% existing$target_date) ,] #Retain only new survey dates
    if (nrow(df) > 0){
      SWE <- data.frame("location" = df$SNOW_COURSE_ID, "target_date" = df$SAMPLE_DATE, "survey_date" = df$SURVEY_DATE, "type" = "SWE", "value" = df$SNOW_WATER_EQUIV, "units" = "mm")
      depth <- data.frame("location" = df$SNOW_COURSE_ID, "target_date" = df$SAMPLE_DATE, "survey_date" = df$SURVEY_DATE, "type" = "snow depth", "value" = df$DEPTH, "units" = "cm")
      df <- rbind(SWE, depth) #bring SWE and DEPTH back together in a 'long' format
      df[is.na(df$survey_date) , ]$survey_date <- df[is.na(df$survey_date), ]$target_date #sometimes no survey data is given, but there is a target date. Presumably no field visit was conducted because there was 100% change of no snow.
      df <- df[!is.na(df$value), ] #remove rows where there is no measurement
      if (nrow(df) > 0){ #condition again, in case na rows were removed
        df$target_date <- as.character(df$target_date)
        df$survey_date <- as.character(df$survey_date)
        # insert new measurements to the DB
        DBI::dbAppendTable(hydro, "snow_courses", df)
        # update locations table
        start <- as.character(min(df$target_date))
        end <- as.character(max(df$target_date))
        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO locations (location, name, data_type, start_datetime, end_datetime, latitude, longitude, operator, network) VALUES ('", data$locations$location_ID[i], "', '", gsub("'", "", data$locations$location_name[i]), "', 'manual snow survey', '", start, "', '", end, "', ", data$locations$latitude[i], ", ", data$locations$longitude[i], ", 'WRB', 'Snow Survey Network')"))
        DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime = '", end, "' WHERE location = '", data$locations$location_ID[i], "'"))
      }
    }
  }
  print("Snow course survey data is updated in the database.")
}
