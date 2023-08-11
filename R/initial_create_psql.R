#' Initial hydro database creation.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Creates an SQLite database or replaces an existing database. Establishsd pre-set table structure and populates defaults in the "settings" and "datum_list" tables. Primary keys for most tables are on the location and parameter, location, parameter, and datetime, or location, parameter, and date columns. No indices are specified as the primary key fulfills this task already.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [WRBtools::hydroConnect()].
#' @param extras The basic database consists of tables for water level and flow, plus metadata tables. Extra tables for distance measurements (e.g. bridge radar distance), snow pillows, snow course or other discrete measurements, precipitation rasters (forecast and reanalysis products), automatic still images at monitoring locations, forecast values (level and flow), and watershed polygons can be created. Select "all" or specify a vector containing anyt of "distance", "snow pillows", "rasters", "atuo_images", "forecasts", "discrete", "watersheds", or leave "none" for nothing.
#' @param overwrite TRUE overwrites the database, if one exists. Nothing will be kept. FALSE will create tables only where they are missing.
#'
#' @return An SQLite database in the folder location specified by 'path'.
#' @export
#'


#NOTE: postgreSQL uses the 'text' data type, but Microsoft SQL server equivalent is varchar(max). Replace all can be used to adapt this script.
#NOTE: For datetimes to work with both postgres and SQL server, ISO8601 should be used: "2022-01-01T00:00:00-07:00" for MST. This is the new ISO standard anyways.

initial_create_psql <- function(con, extras = "none", overwrite = FALSE) {

  on.exit(DBI::dbDisconnect(con))

  if (overwrite){
    for (i in DBI::dbListTables(con)){
      DBI::dbExecute(con, paste0("DROP TABLE ", i))
    }
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(con, "VACUUM")
  }

  # Create the tables for WSC data first
  # realtime table

  DBI::dbExecute(con, "CREATE TABLE if not exists realtime (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 value NUMERIC,
                 grade TEXT,
                 approval TEXT,
                 PRIMARY KEY (location, parameter, datetime))
                 ")

  # daily table
  DBI::dbExecute(con, "CREATE TABLE if not exists daily (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC,
                 grade TEXT,
                 approval TEXT,
                 percent_historic_range NUMERIC,
                 max NUMERIC,
                 min NUMERIC,
                 QP90 NUMERIC,
                 QP75 NUMERIC,
                 QP50 NUMERIC,
                 QP25 NUMERIC,
                 QP10 NUMERIC,
                 PRIMARY KEY (location, parameter, date))")



  if (extras %in% c("all", "rasters")){
    DBI::dbExecute(con, "CREATE TABLE if not exists rasters (
                   description TEXT NOT NULL,
                   parameter TEXT,
                   units TEXT,
                   valid_from TIMESTAMP WITH TIME ZONE,
                   valid_to TIMESTAMP WITH TIME ZONE,
                   file_path TEXT NOT NULL UNIQUE,
                   PRIMARY KEY (description, parameter, file_path))")
  }

  if (extras %in% c("all", "auto_images", "auto images")){
    DBI::dbExecute(con, "CREATE TABLE if not exists auto_images (
                   location TEXT NOT NULL,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   file TEXT NOT NULL,
                   PRIMARY KEY (location, datetime))")
  }

  if (extras %in% c("all", "forecasts", "forecast")){
    DBI::dbExecute(con, "CREATE TABLE if not exists forecasts (
                   location TEXT NOT NULL,
                   parameter TEXT NOT NULL,
                   issue_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   value NUMERIC,
                   units TEXT,
                   PRIMARY KEY (location, parameter, datetime))")
  }

  if (extras %in% c("all", "snow_courses", "snow courses", "discrete")){
    DBI::dbExecute(con, "CREATE TABLE if not exists discrete (
                   location TEXT NOT NULL,
                   parameter TEXT NOT NULL,
                   target_date DATE,
                   sample_date DATE NOT NULL,
                   value NUMERIC NOT NULL,
                   PRIMARY KEY (location, parameter, sample_date))")
  }

  if (extras %in% c("all", "watersheds", "polygons")){
    DBI::dbExecute(con, "CREATE TABLE if not exists polygons (
                   description TEXT NOT NULL,
                   parameter TEXT,
                   file_path TEXT NOT NULL UNIQUE,
                   location TEXT,
                   PRIMARY KEY (description, file_path))")
  }

  # And tables that hold metadata for all locations
  DBI::dbExecute(con, "CREATE TABLE if not exists datum_conversions (
                 location TEXT NOT NULL,
                 datum_id_from INTEGER NOT NULL,
                 datum_id_to INTEGER NOT NULL,
                 conversion_m NUMERIC NOT NULL,
                 current TEXT NOT NULL,
                 PRIMARY KEY (location, datum_id_to, current))")

  DBI::dbExecute(con, "CREATE TABLE if not exists datum_list (
                 datum_id INTEGER NOT NULL,
                 datum_name_en TEXT NOT NULL,
                 datum_name_fr TEXT NOT NULL,
                 PRIMARY KEY (datum_id))")

  DBI::dbExecute(con, "CREATE TABLE if not exists locations (
                 location TEXT UNIQUE NOT NULL,
                 name TEXT,
                 latitude NUMERIC,
                 longitude NUMERIC,
                 PRIMARY KEY (location))")

  #The column timeseries_id is auto created for each new entry
  DBI::dbExecute(con, "CREATE TABLE if not exists timeseries (
                 timeseries_id TEXT NOT NULL,
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 units TEXT NOT NULL,
                 type TEXT NOT NULL,
                 start_datetime TIMESTAMP WITH TIME ZONE,
                 end_datetime TIMESTAMP WITH TIME ZONE,
                 last_new_data TIMESTAMP WITH TIME ZONE,
                 last_daily_calculation TIMESTAMP WITH TIME ZONE,
                 operator TEXT,
                 network TEXT,
                 PRIMARY KEY (location, parameter, type))")

  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_unique_location_parameter
ON timeseries (location, parameter);
")

  #Note for locations table: many columns are not NOT NULL because they have to accept null values for initial creation. This is not an oversight.

  DBI::dbExecute(con, "CREATE TABLE if not exists internal_status (
                 event TEXT NOT NULL,
                 value TIMESTAMP WITH TIME ZONE,
                 PRIMARY KEY (event))")

  internal_status <- data.frame("event" = c("HYDAT_version", "last_update_realtime", "last_update_daily", "last_update_weekly", "last_update_snow_courses", "last_update_watersheds", "last_update_rasters"),
                                "value" = NA)
  DBI::dbAppendTable(con, "internal_status", internal_status)

  # And a table to hold value pairs to control timeseries visibility and Aquarius TS names
  DBI::dbExecute(con, "CREATE TABLE if not exists settings (
                 application TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 value TEXT NOT NULL,
                 PRIMARY KEY (parameter))")

  params <- data.frame("application" = "aquarius",
                       "parameter" = c("level", "flow", "SWE", "snow depth", "distance", "water temperature", "air temperature"),
                       "value" = c("Stage.Corrected", "Discharge.Corrected", "SWE.Corrected", "Snow Depth.Corrected", "Distance.Corrected", "Water Temp.Corrected", "Air Temp.Corrected"))
  try(DBI::dbAppendTable(con, "settings", params))

  #Populate datum_list table
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
    }
  } else if (is.null(hydat_path) | !exists("local_hydat")) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask=FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
  }

  hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
  on.exit(DBI::dbDisconnect(hydat))

  datums_existing <- DBI::dbGetQuery(con, "SELECT datum_id FROM datum_list")
  if (nrow(datums_existing) == 0){
    datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
    names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
    DBI::dbAppendTable(con, "datum_list", datum_list)
  }


  #Add in foreign keys
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION calculate_timeseries_id()
  RETURNS TRIGGER AS $$
    BEGIN
  NEW.timeseries_id := (
    SELECT timeseries_id
    FROM timeseries
    WHERE location = NEW.location
    AND parameter = NEW.parameter
    AND type = NEW.type
  );
  RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;")

  DBI::dbExecute(con, "CREATE TRIGGER trigger_calculate_timeseries_id
                 AFTER INSERT ON timeseries
                 FOR EACH ROW
                 EXECUTE FUNCTION calculate_timeseries_id();
  ")

  DBI::dbExecute(con, "ALTER TABLE realtime ADD CONSTRAINT fk_realtime_location_parameter FOREIGN KEY (location, parameter) REFERENCES timeseries(location, parameter) ON DELETE CASCADE;")


  print(paste0("The database was successfully created at ", path, "."))
}
