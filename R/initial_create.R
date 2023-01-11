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
                 location,
                 datetime_UTC,
                 value,
                 units,
                 grade,
                 approval,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  # flow realtime table
  DBI::dbExecute(hydro, "CREATE TABLE flow_realtime (
                 location,
                 datetime_UTC,
                 value,
                 units,
                 grade,
                 approval,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  # level daily table
  DBI::dbExecute(hydro, "CREATE TABLE level_daily (
                 location,
                 date,
                 value,
                 units,
                 grade,
                 approval,
                 percent_historic_range,
                 max,
                 min,
                 QP90,
                 QP75,
                 QP50,
                 QP25,
                 QP10,
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # flow daily table
  DBI::dbExecute(hydro, "CREATE TABLE flow_daily (
                 location,
                 date,
                 value,
                 units,
                 grade,
                 approval,
                 percent_historic_range,
                 max,
                 min,
                 QP90,
                 QP75,
                 QP50,
                 QP25,
                 QP10,
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # snow pillow data
  DBI::dbExecute(hydro, "CREATE TABLE snow_SWE_realtime (
                 location,
                 datetime_UTC,
                 value,
                 units,
                 grade,
                 approval,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE snow_SWE_daily (
                 location,
                 date,
                 value,
                 units,
                 grade,
                 approval,
                 percent_historic_range,
                 max,
                 min,
                 QP90,
                 QP75,
                 QP50,
                 QP25,
                 QP10,
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  #snow depth data
  DBI::dbExecute(hydro, "CREATE TABLE snow_depth_realtime (
                 location,
                 datetime_UTC,
                 value,
                 units,
                 grade,
                 approval,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE snow_depth_daily (
                 location,
                 date,
                 value,
                 units,
                 grade,
                 approval,
                 percent_historic_range,
                 max,
                 min,
                 QP90,
                 QP75,
                 QP50,
                 QP25,
                 QP10,
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # Distance data
  DBI::dbExecute(hydro, "CREATE TABLE distance_realtime (
                 location,
                 datetime_UTC,
                 value,
                 units,
                 grade,
                 approval,
                 PRIMARY KEY (location, datetime_UTC))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE distance_daily (
                 location,
                 date,
                 value,
                 units,
                 grade,
                 approval,
                 percent_historic_range,
                 max,
                 min,
                 QP90,
                 QP75,
                 QP50,
                 QP25,
                 QP10,
                 PRIMARY KEY (location, date))
                 WITHOUT ROWID")

  # And tables that hold metadata for all locations
  DBI::dbExecute(hydro, "CREATE TABLE datum_conversions (
                 location,
                 datum_id_from,
                 datum_id_to,
                 current,
                 PRIMARY KEY (location, datum_id_to))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE datum_list (
                 datum_id,
                 datum_name_en,
                 datum_name_fr,
                 PRIMARY KEY (datum_id))
                 WITHOUT ROWID")

  DBI::dbExecute(hydro, "CREATE TABLE locations (
                 location,
                 name,
                 data_type,
                 start_datetime,
                 end_datetime,
                 latitude,
                 longitude,
                 operator,
                 network,
                 PRIMARY KEY (location, data_type))
                 WITHOUT ROWID")

  # And a table to hold value pairs to control timeseries visibility
  DBI::dbExecute(hydro, "CREATE TABLE settings (
                 parameter,
                 value,
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
