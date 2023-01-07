#' Initial hydro database creation.
#'
#' Creates an SQLite database or replaces an existing database. Established pre-set table structure and populates defaults in the "settings" table.
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
  DBI::dbCreateTable(hydro, "level_realtime", fields = c(location = NA, datetime_UTC = NA, value = NA, units = NA, grade = NA, approval = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX level_minute_index ON level_realtime (datetime_UTC, location);")
  # flow realtime table
  DBI::dbCreateTable(hydro, "flow_realtime", fields = c(location = NA, datetime_UTC = NA, value = NA, units = NA, grade = NA, approval = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX flow_minute_index ON flow_realtime (datetime_UTC, location);")
  # level historic table
  DBI::dbCreateTable(hydro, "level_daily", fields = c(location = NA, date = NA, value = NA, units = NA, grade = NA, approval = NA,percent_historic_range = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX level_daily_index ON level_daily (date, location);")
  # flow historic table
  DBI::dbCreateTable(hydro, "flow_daily", fields = c(location = NA, date = NA, value = NA, units = NA, grade = NA, approval =NA, percent_historic_range = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX flow_daily_index ON flow_daily (date, location);")

  # snow pillow data
  DBI::dbCreateTable(hydro, "snow_pillow_SWE_realtime", fields = c(location = NA, datetime_UTC = NA, value = NA, units = NA, grade = NA, approval = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX snow_pillow_SWE_minute_index ON snow_pillow_SWE_realtime (datetime_UTC, location);")

  DBI::dbCreateTable(hydro, "snow_pillow_SWE_daily", fields = c(location= NA, date = NA, value = NA, units = NA, grade = NA, approval = NA, percent_historic_range = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX snow_pillow_SWE_daily_index ON snow_pillow_SWE_daily (date, location);")

  DBI::dbCreateTable(hydro, "snow_pillow_depth_realtime", field = c(location = NA, datetime_UTC = NA, value = NA, units = NA, grade = NA, approval = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX snow_pillow_depth_minute_index ON snow_pillow_depth_realtime (datetime_UTC, location);")

  DBI::dbCreateTable(hydro, "snow_pillow_depth_daily",  fields = c(location= NA, date = NA, value = NA, units = NA, grade = NA, approval = NA, percent_historic_range = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX snow_pillow_depth_daily_index ON snow_pillow_depth_daily (date, location);")

  # Bridge radar data
  DBI::dbCreateTable(hydro, "bridge_distance_realtime", fields = c(location = NA, datetime_UTC = NA, value = NA, units = NA, grade = NA, approval = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX bridge_distance_minute_index ON bridge_distance_realtime (datetime_UTC, location);")

  DBI::dbCreateTable(hydro, "bridge_distance_daily", fields = c(location= NA, date = NA, value = NA, units = NA, grade = NA, approval = NA, percent_historic_range = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX bridge_distance_daily_index ON bridge_distance_daily (date, location);")


  # And lastly a table that holds metadata for all locations
  DBI::dbCreateTable(hydro, "datum_conversions", fields = c(location = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX datum_conversions_index ON datum_conversions (location, datum_id_from, datum_id_to, current);")

  DBI::dbCreateTable(hydro, "datum_list", fields = c(datum_id = NA, datum_name_en = NA, datum_name_fr = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX datum_list_index ON datum_list (datum_id);")

  DBI::dbCreateTable(hydro, "locations", fields = c(location = NA, name = NA, data_type = NA, start_datetime = NA, end_datetime = NA, latitude = NA, longitude = NA, operator = NA, network = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX locations_index ON locations (location, data_type);")

  # And a table to hold value pairs to control timeseries visibility
  DBI::dbCreateTable(hydro, "settings", fields = c(parameter = NA, value = NA))
  DBI::dbExecute(hydro, "CREATE UNIQUE INDEX settings_index ON settings (parameter);")

  # And check your tables to make sure everything is good
  DBI::dbListTables(hydro)

  #Populate the 'settings' table with defaults
  settings <- data.frame(parameter = c("level unapproved visible", "flow unapproved visible", "snow unapproved visible", "bridge distance unapproved visible", "level min grade", "flow min grade", "snow min grade", "bridge distance min grade"), value = c(TRUE, TRUE, TRUE, TRUE, "C", "C", "C", "C"))
  RSQLite::dbWriteTable(hydro, "settings", settings, overwrite = TRUE)


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
  RSQLite::dbWriteTable(hydro, "datum_list", datum_list, overwrite = TRUE)
  print(paste0("The database was successfully created at ", path, "."))
}
