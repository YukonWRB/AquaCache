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
  DBI::dbCreateTable(hydro, "WSC_level_realtime", fields = c(location = NA, datetime_UTC = NA, level = NA, approval = NA, percent_max_historic = NA))
  # flow realtime table
  DBI::dbCreateTable(hydro, "WSC_flow_realtime", fields = c(location = NA, datetime_UTC = NA, flow = NA, approval = NA, percent_max_historic = NA))
  # level historic table
  DBI::dbCreateTable(hydro, "WSC_level_daily", fields = c(location = NA, date = NA, level = NA, approval = NA,percent_max_historic = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  # flow historic table
  DBI::dbCreateTable(hydro, "WSC_flow_daily", fields = c(location = NA, date = NA, flow = NA, approval =NA, percent_max_historic = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))

  # Now add tables for data held solely by the WRB
  # snow pillow data
  DBI::dbCreateTable(hydro, "WRB_snow_pillow_SWE_realtime", fields = c(location = NA, datetime_UTC = NA, SWE = NA, grade = NA, approval = NA, percent_max_historic = NA))
  DBI::dbCreateTable(hydro, "WRB_snow_pillow_SWE_daily", fields = c(location= NA, date = NA, SWE = NA,  grade = NA, approval = NA, percent_max_historic = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbCreateTable(hydro, "WRB_snow_pillow_depth_realtime", field = c(location = NA, datetime_UTC = NA, depth = NA, grade = NA, approval = NA, percent_max_historic = NA))
  DBI::dbCreateTable(hydro, "WRB_snow_pillow_depth_daily", fields = c(location= NA, date = NA, depth = NA, percent_max_historic = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  # small stream network data
  DBI::dbCreateTable(hydro, "WRB_flow_realtime", fields = c(location = NA, datetime_UTC = NA, flow = NA, grade = NA, approval = NA, percent_max_historic = NA))
  DBI::dbCreateTable(hydro, "WRB_level_realtime", fields = c(location = NA, datetime_UTC = NA, level = NA, percent_max_historic = NA))
  DBI::dbCreateTable(hydro, "WRB_flow_daily", fields = c(location = NA, date = NA, year = NA, dayofyear = NA, flow = NA, grade = NA, approval = NA, percent_max_historic = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))
  DBI::dbCreateTable(hydro, "WRB_level_daily", fields = c(location = NA, date = NA, year = NA, dayofyear = NA, level = NA, grade = NA, approval = NA, percent_max_historic = NA, max = NA, min = NA, QP90 = NA, QP75 = NA, QP50 = NA, QP25 = NA, QP10 = NA))

  # And lastly a table that holds metadata for all locations
  DBI::dbCreateTable(hydro, "datum_conversions", fields = c(location = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA))
  DBI::dbCreateTable(hydro, "datum_list", fields = c(datum_id = NA, datum_name_en = NA, datum_name_fr = NA))
  DBI::dbCreateTable(hydro, "locations", fields = c(location = NA, data_type = NA, start_datetime = NA, end_datetime = NA, latitude = NA, longitude = NA, operator = NA, network = NA))

  # And a table to hold value pairs to control timeseries visibility
  DBI::dbCreateTable(hydro, "settings", fields = c(parameter = NA, value = NA))

  # And check your tables to make sure everything is good
  DBI::dbListTables(hydro)

  #Populate the 'settings' table with defaults
  settings <- data.frame(parameter = c("WSC level preliminary visible", "WSC flow preliminary visible", "WRB level preliminary visible", "WRB flow prelininary visible", "WRB level min grade", "WRB flow min grade", "WRB snow pillow preliminary visible", "WRB snow pillow min grade"), value = c(TRUE, TRUE, TRUE, TRUE, "C", "C", TRUE, "C"))
  RSQLite::dbWriteTable(hydro, "settings", settings, overwrite = TRUE)

}
