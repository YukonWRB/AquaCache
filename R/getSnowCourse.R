
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


  hydro <- WRBtools::hydroConnect(path = hydro_db_path)
  on.exit(DBI::dbDisconnect(hydro), add=TRUE)

  tables <- DBI::dbListTables(hydro)
  if (!("discrete" %in% tables)){
    print("The table 'discrete' is being created in the database using function initial_create.")
    initial_create(path = hydro_db_path, extras = "snow courses", overwrite = FALSE)
  }

  data <- WRBtools::snowInfo(db_path = snow_db_path, locations = "all", inactive = inactive, save_path = NULL, stats = FALSE, plots = FALSE, quiet = TRUE)

  for (i in 1:nrow(data$locations)){
    #get new measurements
    if (overwrite){
      DBI::dbExecute(hydro, paste0("DELETE FROM discrete WHERE parameter IN ('snow depth', 'SWE') AND location = '", data$locations$location_ID[i], "'"))
      DBI::dbExecute(hydro, paste0("DELETE FROM locations WHERE parameter IN ('snow depth', 'SWE') AND type = 'discrete' AND location = '", data$locations$location_ID[i], "'"))
    }
    df <- data$measurements[data$measurements$SNOW_COURSE_ID == data$locations$location_ID[i] , ]
    existing <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM discrete WHERE location = '", data$locations$location_ID[i], "'"))
    df <-  df[!(as.character(df$SAMPLE_DATE) %in% existing$target_date) ,] #Retain only new survey dates
    if (nrow(df) > 0){
      SWE <- data.frame("location" = df$SNOW_COURSE_ID, "target_date" = df$SAMPLE_DATE, "sample_date" = df$SURVEY_DATE, "value" = df$SNOW_WATER_EQUIV, "units" = "mm", "parameter" = "SWE")
      depth <- data.frame("location" = df$SNOW_COURSE_ID, "target_date" = df$SAMPLE_DATE, "sample_date" = df$SURVEY_DATE, "value" = df$DEPTH, "units" = "cm", "parameter" = "snow depth")
      df <- rbind(SWE, depth) #bring SWE and DEPTH back together in a 'long' format
      df[is.na(df$sample_date) , ]$sample_date <- df[is.na(df$sample_date), ]$target_date #sometimes no survey data is given, but there is a target date. Presumably no field visit was conducted because there was 100% chance of no snow.
      df <- df[!is.na(df$value), ] #remove rows where there is no measurement
      if (nrow(df) > 0){ #condition again, in case na rows were removed
        df$target_date <- as.character(df$target_date)
        df$sample_date <- as.character(df$sample_date)
        # insert new measurements to the DB
        DBI::dbAppendTable(hydro, "discrete", df)
        # update locations table
        start <- as.character(min(df$target_date))
        end <- as.character(max(df$target_date))
        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO locations (location, name, parameter, type, start_datetime_UTC, end_datetime_UTC, latitude, longitude, operator, network) VALUES ('", data$locations$location_ID[i], "', '", gsub("'", "", data$locations$location_name[i]), "', 'SWE', 'discrete', '", start, "', '", end, "', ", data$locations$latitude[i], ", ", data$locations$longitude[i], ", 'WRB', 'Snow Survey Network')"))
        DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime_UTC = '", end, "' WHERE location = '", data$locations$location_ID[i], "' AND parameter = 'SWE' AND type = 'discrete'"))
        DBI::dbExecute(hydro, paste0("INSERT OR IGNORE INTO locations (location, name, parameter, type, start_datetime_UTC, end_datetime_UTC, latitude, longitude, operator, network) VALUES ('", data$locations$location_ID[i], "', '", gsub("'", "", data$locations$location_name[i]), "', 'snow depth', 'discrete', '", start, "', '", end, "', ", data$locations$latitude[i], ", ", data$locations$longitude[i], ", 'WRB', 'Snow Survey Network')"))
        DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime_UTC = '", end, "' WHERE location = '", data$locations$location_ID[i], "' AND parameter = 'snow depth' AND type = 'discrete'"))
      }
    }
  }
  DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_snow_courses'"))
  print("Snow course survey data is updated in the database.")
}
