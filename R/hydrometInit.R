#' Initial hydrometric/meteorological database creation.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Creates a postgreSQL database or replaces an existing database. Establishes pre-set schemas and populates initial rows in the "settings" and "datum_list" tables. No indices are specified as the primary key fulfills this task already.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param overwrite TRUE overwrites the database, if one exists. Nothing will be kept. FALSE will create tables only where they are missing.
#'
#' @return An SQLite database in the folder location specified by 'path'.
#' @export
#'


#NOTE: postgreSQL uses the 'text' data type, but Microsoft SQL server equivalent is varchar(max). Replace all can be used to adapt this script.
#NOTE: For datetimes to work with both postgres and SQL server, ISO8601 should be used: "2022-01-01T00:00:00-07:00" for MST. This is the new ISO standard anyways.

hydrometInit <- function(con = hydrometConnect(), overwrite = FALSE) {

  if (overwrite){
    DBI::dbExecute(con, "DROP EXTENSION postgis CASCADE")
    for (i in DBI::dbListTables(con)){
      tryCatch({
        DBI::dbExecute(con, paste0("DROP TABLE ", i, " CASCADE"))
      }, error = function(e) {
        DBI::dbExecute(con, paste0("DROP VIEW ", i))
      })
    }
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(con, "VACUUM")
  }

  #Create the postgis extension, which adds a few necessary tables. Then, create a new schema and set the schema of these tables to decrease clutter in the DB.
  rpostgis::pgPostGIS(con, raster = TRUE)

  # measurements_continuous table
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_continuous (
                 timeseries_id INTEGER NOT NULL,
                 datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 value NUMERIC NOT NULL,
                 grade TEXT,
                 approval TEXT,
                 period INTERVAL,
                 imputed BOOLEAN NOT NULL,
                 PRIMARY KEY (timeseries_id, datetime))
                 ")

  # calculated_daily table
  DBI::dbExecute(con, "CREATE TABLE if not exists calculated_daily (
                 timeseries_id INTEGER NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC,
                 grade TEXT,
                 approval TEXT,
                 imputed BOOLEAN,
                 percent_historic_range NUMERIC,
                 max NUMERIC,
                 min NUMERIC,
                 q90 NUMERIC,
                 q75 NUMERIC,
                 q50 NUMERIC,
                 q25 NUMERIC,
                 q10 NUMERIC,
                 PRIMARY KEY (timeseries_id, date))")

  DBI::dbExecute(con, "CREATE TABLE if not exists images (
                   location TEXT NOT NULL,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   file BYTEA NOT NULL,
                   image_type TEXT NOT NULL CHECK(image_type IN ('auto', 'manual')),
                   PRIMARY KEY (location, datetime, image_type))")

  DBI::dbExecute(con, "CREATE TABLE if not exists forecasts (
                   timeseries_id INTEGER,
                   issue_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   value NUMERIC,
                   PRIMARY KEY (timeseries_id, datetime))")

  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_discrete (
                   timeseries_id INTEGER,
                   target_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE,
                   value NUMERIC NOT NULL,
                   sample_class TEXT,
                   note TEXT,
                   PRIMARY KEY (timeseries_id, datetime))")

  # Create spatial-primary tables ########
  DBI::dbExecute(con, "CREATE TABLE if not exists polygons (
                 polygon_id SERIAL PRIMARY KEY,
                 polygon_type TEXT NOT NULL CHECK(polygon_type IN ('drainage_basin', 'buffer', 'waterbody', 'prov_terr', 'country', 'other')),
                 name TEXT NOT NULL,
                 description TEXT NOT NULL,
                 geom geometry(Polygon, 4269) NOT NULL,
                 CONSTRAINT enforce_dims_geom CHECK (st_ndims(geom) = 2),
                 CONSTRAINT enforce_geotype_geom CHECK (geometrytype(geom) = 'POLYGON'::text),
                 CONSTRAINT enforce_srid_geom CHECK (st_srid(geom) = 4269),
                 CONSTRAINT enforce_valid_geom CHECK (st_isvalid(geom)),
                 UNIQUE (name, polygon_type));")
  DBI::dbExecute(con, "CREATE INDEX polygons_idx ON polygons USING GIST (geom);") #Forces use of GIST indexing which is necessary for large polygons

  # In theory the function rpostgis::pgWriteRast should be able to insert the raster, and then
  # DBI::dbExecute(con, "CREATE TABLE if not exists rasters_index (
  #                  rid INTEGER PRIMARY KEY,
  #                  parameter TEXT NOT NULL,
  #                  description TEXT,
  #                  units TEXT NOT NULL,
  #                  valid_from TIMESTAMP WITH TIME ZONE,
  #                  valid_to TIMESTAMP WITH TIME ZONE,
  #                  issued TIMESTAMP WITH TIME ZONE,
  #                  source TEXT,
  #                  bands TEXT);")
  #
  # DBI::dbExecute(con, "CREATE TABLE if not exists rasters (
  #                  rid INTEGER);")
  # DBI::dbExecute(con, "CREATE INDEX rasters_rast_st_conhull_idx ON rasters USING gist( ST_ConvexHull(raster) );")

  # And tables that hold metadata for all locations
  DBI::dbExecute(con, "CREATE TABLE if not exists datum_list (
    datum_id INTEGER PRIMARY KEY,
    datum_name_en TEXT NOT NULL,
    datum_name_fr TEXT NOT NULL);")

  DBI::dbExecute(con, "CREATE TABLE if not exists datum_conversions (
                 conversion_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 datum_id_from INTEGER NOT NULL REFERENCES datum_list (datum_id),
                 datum_id_to INTEGER NOT NULL REFERENCES datum_list (datum_id),
                 conversion_m NUMERIC NOT NULL,
                 current BOOLEAN NOT NULL,
                 UNIQUE (location, datum_id_to, current));")

  DBI::dbExecute(con, "CREATE TABLE if not exists locations (
                 location TEXT PRIMARY KEY,
                 name TEXT NOT NULL,
                 latitude NUMERIC NOT NULL,
                 longitude NUMERIC NOT NULL,
                 point geometry(POINT, 4269))")

  #The column timeseries_id is auto created for each new entry
  DBI::dbExecute(con, "CREATE TABLE if not exists timeseries (
                 timeseries_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 param_type TEXT NOT NULL, CHECK(param_type IN ('meteorological', 'hydrometric', 'water chemistry', 'geochemistry', 'atmospheric chemistry')),
                 unit TEXT NOT NULL,
                 category TEXT NOT NULL CHECK(category IN ('discrete', 'continuous')),
                 period_type TEXT NOT NULL CHECK(period_type IN ('instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2')),
                 start_datetime TIMESTAMP WITH TIME ZONE,
                 end_datetime TIMESTAMP WITH TIME ZONE,
                 last_new_data TIMESTAMP WITH TIME ZONE,
                 last_daily_calculation TIMESTAMP WITH TIME ZONE,
                 last_synchronize TIMESTAMP WITH TIME ZONE,
                 operator TEXT,
                 network TEXT,
                 public BOOLEAN NOT NULL,
                 source_fx TEXT NOT NULL,
                 source_fx_args TEXT,
                 UNIQUE (location, parameter, category, period_type));")

  DBI::dbExecute(con, "CREATE TABLE if not exists peaks (
                 timeseries_id INTEGER PRIMARY KEY,
                 agency TEXT NOT NULL,
                 year NUMERIC NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC NOT NULL,
                 period_type TEXT NOT NULL CHECK(period_type IN ('instantaneous', '1-day', '2-day', '3-day', '4-day', '5-day', '6-day', '7-day', 'monthly')),
                 condition TEXT NOT NULL CHECK(condition IN ('open water', 'break-up', 'freeze-up', 'winter')),
                 extrema TEXT NOT NULL CHECK(extrema IN ('minimum', 'maximum')),
                 uncertainty TEXT,
                 notes TEXT,
                 deemed_primary BOOLEAN NOT NULL,
                 UNIQUE (timeseries_id, agency, year, period_type, condition, extrema));")

  DBI::dbExecute(con, "CREATE TABLE if not exists thresholds (
                 timeseries_id INTEGER PRIMARY KEY,
                 high_advisory NUMERIC,
                 high_watch NUMERIC,
                 high_warning NUMERIC,
                 flood_minor NUMERIC,
                 flood_major NUMERIC,
                 high_firs_human_impacts NUMERIC,
                 low_advisory NUMERIC,
                 low_watch NUMERIC,
                 low_warning NUMERIC,
                 low_first_human_impacts NUMERIC,
                 low_aquatic_life_impacts_minor NUMERIC,
                 low_aquatic_life_impacts_major NUMERIC,
                 high_aquatic_life_impacts_minor NUMERIC,
                 high_aquatic_life_impacts_major NUMERIC,
                 FSL NUMERIC,
                 LSL NUMERIC);")


  DBI::dbExecute(con, "CREATE TABLE if not exists internal_status (
                 event TEXT NOT NULL,
                 value TIMESTAMP WITH TIME ZONE,
                 PRIMARY KEY (event))")

  internal_status <- data.frame("event" = c("HYDAT_version", "last_new_continuous",  "last_new_discrete", "last_update_daily", "last_sync_continuous", "last_sync_discrete", "last_update_watersheds", "last_update_rasters", "last_update_polygons", "last_vacuum"),
                                "value" = NA)
  DBI::dbAppendTable(con, "internal_status", internal_status)

  # And a table to hold value pairs to control parameter names
  DBI::dbExecute(con, "CREATE TABLE if not exists settings (
                 source_fx TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 remote_param_name TEXT NOT NULL,
                 PRIMARY KEY (source_fx, parameter))")

  params_AQ <- data.frame("source_fx" = "getRealtimeAquarius",
                          "parameter" = c("level", "flow", "SWE", "snow depth", "distance", "water temperature", "air temperature"),
                          "remote_param_name" = c("Stage.Corrected", "Discharge.Corrected", "SWE.Corrected", "Snow Depth.Corrected", "Distance.Corrected", "Water Temp.Corrected", "Air Temp.Corrected"))
  params_WSC <- data.frame("source_fx" = "getRealtimeWSC",
                           "parameter" = c("level", "flow", "water temperature"),
                           "remote_param_name" = c("46", "47", "5"))
  params_USGS <- data.frame("source_fx" = "getRealtimeNWIS",
                            "parameter" = c("level", "flow", "water temperature"),
                            "remote_param_name" = c("00065", "00060", "00010"))
  params <- rbind(params_AQ, params_WSC, params_USGS)
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
  DBI::dbExecute(con,
                 "ALTER TABLE timeseries
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location);")

  DBI::dbExecute(con,
                 "ALTER TABLE calculated_daily
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id);")

  DBI::dbExecute(con,
                 "ALTER TABLE measurements_continuous
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id);")

  DBI::dbExecute(con,
                 "ALTER TABLE measurements_discrete
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id);")

  DBI::dbExecute(con,
                 "ALTER TABLE peaks
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id);")

  DBI::dbExecute(con,
                 "ALTER TABLE forecasts
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id);")

  DBI::dbExecute(con,
                 "ALTER TABLE datum_conversions
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location);")

  DBI::dbExecute(con,
                 "ALTER TABLE images
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location);")

  DBI::dbExecute(con,
                 "ALTER TABLE thresholds
                 ADD CONSTRAINT fk_timeseries_id
                 FOREIGN KEY (timeseries_id)
                 REFERENCES timeseries(timeseries_id);")

  # Create a read-only account
  tryCatch({
    DBI::dbExecute(con, "CREATE ROLE hydromet_read WITH LOGIN PASSWORD 'hydromet';")
    DBI::dbExecute(con, "GRANT CONNECT ON DATABASE hydromet TO hydromet_read;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public TO hydromet_read;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO hydromet_read;")
  }, error = function(e) {
    warning("May have failed to create new read only account with name hydromet_read. Ignore this message if it already exists (this function would not have erased the old account), otherwise check if it exists before worrying.")
  })

  message("The database was successfully created. If a read-only account was created it has username hydromet_read and password hydromet")
}
