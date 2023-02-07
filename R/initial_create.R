#' Initial hydro database creation.
#'
#' Creates an SQLite database or replaces an existing database. Established pre-set table structure and populates defaults in the "settings" and "datum_list" tables. All tables are created as WITHOUT ROWID tables, with primary keys for most tables on the location and parameter, location and datetime_UTC, or location and date columns.
#'
#' @param path The path to the local hydro SQLite database or the location where it should be created, with extension.
#' @param extras The basic database consists of tables for water level and flow, plus metadata tables. Extra tables for distance measurements (e.g. bridge radar distance), snow pillows, snow course or other discrete measurements, precipitation rasters (forecast and reanalysis products), automatic still images at monitoring locations, forecast values (level and flow), and watershed polygons can be created. Select "all" or specify a vector containing anyt of "distance", "snow pillows", "rasters", "atuo_images", "forecasts", "discrete", "watersheds", or leave "none" for nothing.
#' @param overwrite TRUE overwrites the database, if one exists in the same path. Nothing will be kept. FALSE will create tables only where they are missing.
#' @param new Allows you to create a new SQLite database. By default, the connection to the database path checks for an existing database. You can create a new DB at 'path' if 'path' does not point to an existing database. Creates an SQLite database.
#'
#' @return An SQLite database in the folder location specified by 'path'.
#' @export
#'

initial_create <- function(path, extras = "none", overwrite = FALSE, new = FALSE) {

  if (!new){
    hydro <- WRBtools::hydroConnect(path = path)
  } else {
    hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
    overwrite <- FALSE
  }
  on.exit(DBI::dbDisconnect(hydro))


  if (overwrite){
    for (i in DBI::dbListTables(hydro)){
      DBI::dbExecute(hydro, paste0("DROP TABLE ", i))
    }
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(hydro, "VACUUM")
  }

  # Create the tables for WSC data first
  # realtime table

  DBI::dbExecute(hydro, "CREATE TABLE if not exists realtime (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 grade TEXT,
                 approval TEXT,
                 PRIMARY KEY (location, parameter, datetime_UTC)
                 FOREIGN KEY (location) REFERENCES locations(location)
                 FOREIGN KEY (parameter) REFERENCES locations(parameter))
                 WITHOUT ROWID")

  # daily table
  DBI::dbExecute(hydro, "CREATE TABLE if not exists daily (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 date TEXT NOT NULL,
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
                 PRIMARY KEY (location, parameter, date)
                 FOREIGN KEY (location) REFERENCES locations(location)
                 FOREIGN KEY (parameter) REFERENCES locations(parameter))
                 WITHOUT ROWID")


  if (extras %in% c("all", "rasters")){
    DBI::dbExecute(hydro, "CREATE TABLE if not exists rasters (
                 description TEXT NOT NULL,
                 parameter TEXT,
                 units TEXT,
                 valid_from,
                 valid_to,
                 file_path TEXT NOT NULL UNIQUE,
                 PRIMARY KEY (description, parameter, file_path))
                 WITHOUT ROWID")
    #NOTE: the files are not stored in the DB, only the file path. The script will enter the file path in the DB after putting the file in a folder, located in the same directory as the database.
  }

  if (extras %in% c("all", "auto_images", "auto images")){
    DBI::dbExecute(hydro, "CREATE TABLE if not exists auto_images (
                 location TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 file TEXT NOT NULL,
                 PRIMARY KEY (location, datetime_UTC)
                 FOREIGN KEY (location) REFERENCES locations(location))
                 WITHOUT ROWID")
  }

  if (extras %in% c("all", "forecasts", "forecast")){
    DBI::dbExecute(hydro, "CREATE TABLE if not exists forecasts (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 issue_time_UTC TEXT,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 PRIMARY KEY (location, parameter, datetime_UTC)
                 FOREIGN KEY (location) REFERENCES locations(location))
                 WITHOUT ROWID")
  }

  if (extras %in% c("all", "snow_courses", "snow courses", "discrete")){
    DBI::dbExecute(hydro, "CREATE TABLE if not exists discrete (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 target_date TEXT,
                 sample_date TEXT NOT NULL,
                 value NUMERIC NOT NULL,
                 PRIMARY KEY (location, parameter, sample_date)
                 FOREIGN KEY (location) REFERENCES locations(location)
                 FOREIGN KEY (parameter) REFERENCES locations(parameter))
                 WITHOUT ROWID")
  }

  if (extras %in% c("all", "watersheds", "polygons")){
    DBI::dbExecute(hydro, "CREATE TABLE if not exists polygons (
                   description TEXT NOT NULL,
                   parameter TEXT,
                   file_path TEXT NOT NULL UNIQUE,
                   location TEXT,
                   PRIMARY KEY (description, file_path),
                   FOREIGN KEY (location) REFERENCES locations(location))
                   WITHOUT ROWID")
  }


  # And tables that hold metadata for all locations
  DBI::dbExecute(hydro, "CREATE TABLE if not exists datum_conversions (
                 location TEXT NOT NULL,
                 datum_id_from INTEGER NOT NULL,
                 datum_id_to INTEGER NOT NULL,
                 conversion_m NUMERIC NOT NULL,
                 current BOOLEAN NOT NULL,
                 PRIMARY KEY (location, datum_id_to)
                 FOREIGN KEY (location) REFERENCES locations(location)
                 FOREIGN KEY (datum_id_from) REFERENCES datum_list(datum_id)
                 FOREIGN KEY (datum_id_to) REFERENCES datum_list(datum_id))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE if not exists datum_list (
                 datum_id INTEGER NOT NULL,
                 datum_name_en TEXT NOT NULL,
                 datum_name_fr TEXT NOT NULL,
                 PRIMARY KEY (datum_id))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE if not exists locations (
                 location TEXT NOT NULL,
                 name TEXT,
                 latitude NUMERIC,
                 longitude NUMERIC,
                 PRIMARY KEY (location))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE if not exists timeseries (
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 units TEXT NOT NULL,
                 type TEXT NOT NULL,
                 start_datetime_UTC TEXT,
                 end_datetime_UTC TEXT,
                 last_new_data_UTC TEXT,
                 last_daily_calculation_UTC TEXT,
                 operator TEXT,
                 network TEXT,
                 PRIMARY KEY (location, parameter, type))
                 WITHOUT ROWID")

  #Note for locations table: many columns are not NOT NULL because they have to accept null values for initial creation. This is not an oversight.

  DBI::dbExecute(hydro, "CREATE TABLE if not exists internal_status (
                 event TEXT NOT NULL,
                 value,
                 PRIMARY KEY (event))
                 WITHOUT ROWID")

  internal_status <- data.frame("event" = c("HYDAT_version", "last_update_realtime", "last_update_daily", "last_update_weekly", "last_update_snow_courses", "last_update_watersheds", "last_update_rasters"),
                                "value" = NA)
  DBI::dbAppendTable(hydro, "internal_status", internal_status)

  # And a table to hold value pairs to control timeseries visibility and Aquarius TS names
  DBI::dbExecute(hydro, "CREATE TABLE if not exists settings (
  application TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 value TEXT,
                 PRIMARY KEY (parameter))
                 WITHOUT ROWID")

  params <- data.frame("application" = "aquarius",
                       "parameter" = c("level", "flow", "SWE", "snow depth", "distance", "water temperature", "air temperature"),
                       "value" = c("Stage.Corrected", "Discharge.Corrected", "SWE.Corrected", "Snow Depth.Corrected", "Distance.Corrected", "Water Temp.Corrected", "Air Temp.Corrected"))
  try(DBI::dbAppendTable(hydro, "settings", params))

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

  datums_existing <- DBI::dbGetQuery(hydro, "SELECT datum_id FROM datum_list")
  if (nrow(datums_existing) == 0){
    datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
    names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
    DBI::dbAppendTable(hydro, "datum_list", datum_list)
  }
  print(paste0("The database was successfully created at ", path, "."))
}
