#' Initial hydro database creation.
#'
#' Creates an SQLite database or replaces an existing database. Established pre-set table structure and populates defaults in the "settings" and "datum_list" tables. All tables are created as WITHOUT ROWID tables, with primary keys for most tables on the location and data_type, location and datetime_UTC, or location and date columns.
#'
#' @param path The path to the local hydro SQLite database or the location where it should be created, with extension.
#'
#' @return An SQLite database in the folder location specified by 'path'.
#' @export
#'

initial_create <- function(path) {

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  for (i in DBI::dbListTables(hydro)){
    DBI::dbExecute(hydro, paste0("DROP TABLE ", i))
  }
  # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
  DBI::dbExecute(hydro, "VACUUM")

  # Create the tables for WSC data first
  # level realtime table

  DBI::dbExecute(hydro, "CREATE TABLE level_realtime (
                 location TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 grade TEXT,
                 approval TEXT,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  # flow realtime table
  DBI::dbExecute(hydro, "CREATE TABLE flow_realtime (
                 location TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 grade TEXT,
                 approval TEXT,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  # level daily table
  DBI::dbExecute(hydro, "CREATE TABLE level_daily (
                 location TEXT NOT NULL,
                 date TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
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
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # flow daily table
  DBI::dbExecute(hydro, "CREATE TABLE flow_daily (
                 location TEXT NOT NULL,
                 date TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
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
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # snow pillow data
  DBI::dbExecute(hydro, "CREATE TABLE snow_SWE_realtime (
                 location TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 grade NUMERIC,
                 approval NUMERIC,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE snow_SWE_daily (
                 location TEXT NOT NULL,
                 date TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
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
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  #snow depth data
  DBI::dbExecute(hydro, "CREATE TABLE snow_depth_realtime (
                 location TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 grade TEXT,
                 approval TEXT,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE snow_depth_daily (
                 location TEXT NOT NULL,
                 date TEXT NOT NULL,
                 value, NUMERIC
                 units TEXT,
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
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # Distance data
  DBI::dbExecute(hydro, "CREATE TABLE distance_realtime (
                 location TEXT NOT NULL,
                 datetime_UTC TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
                 grade TEXT,
                 approval TEXT,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE distance_daily (
                 location TEXT NOT NULL,
                 date TEXT NOT NULL,
                 value NUMERIC,
                 units TEXT,
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
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # And tables that hold metadata for all locations
  DBI::dbExecute(hydro, "CREATE TABLE datum_conversions (
                 location TEXT NOT NULL,
                 datum_id_from INTEGER NOT NULL,
                 datum_id_to INTEGER NOT NULL,
                 conversion_m NUMERIC NOT NULL,
                 current BOOLEAN NOT NULL,
                 PRIMARY KEY (location, datum_id_to))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE datum_list (
                 datum_id INTEGER NOT NULL,
                 datum_name_en TEXT NOT NULL,
                 datum_name_fr TEXT NOT NULL,
                 PRIMARY KEY (datum_id))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE locations (
                 location TEXT NOT NULL,
                 name TEXT,
                 data_type TEXT NOT NULL,
                 start_datetime TEXT,
                 end_datetime TEXT,
                 latitude NUMERIC,
                 longitude NUMERIC,
                 operator TEXT,
                 network TEXT,
                 PRIMARY KEY (location, data_type))
                 WITHOUT ROWID")
  #Note for locations table: many columns are not NOT NULL because they have to accept null values for intial creation. This is not an oversight.

  # And a table to hold value pairs to control timeseries visibility
  DBI::dbExecute(hydro, "CREATE TABLE settings (
                 parameter TEXT NOT NULL,
                 value TEXT,
                 PRIMARY KEY (parameter))
                 WITHOUT ROWID")

  # And check your tables to make sure everything is good
  DBI::dbListTables(hydro)

  #Populate the 'settings' table with defaults
  settings <- data.frame(parameter = c("level unapproved visible", "flow unapproved visible", "snow unapproved visible", "bridge distance unapproved visible", "level min grade", "flow min grade", "snow min grade", "bridge distance min grade"), value = c(TRUE, TRUE, TRUE, TRUE, "C", "C", "C", "C"))
  DBI::dbAppendTable(hydro, "settings", settings)


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

  datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
  names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
  DBI::dbAppendTable(hydro, "datum_list", datum_list)
  print(paste0("The database was successfully created at ", path, "."))
}
