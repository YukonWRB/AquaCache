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

hydrometInit <- function(con = hydrometConnect(), overwrite = FALSE) {

  # Overwrite and vacuum if requested ###########
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

  # Add tables with constraints ###############
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
                   image_id SERIAL PRIMARY KEY,
                   img_meta_id INTEGER NOT NULL,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   fetch_datetime TIMESTAMP WITH TIME ZONE,
                   format TEXT NOT NULL,
                   file BYTEA NOT NULL)")

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS images_index (
                 img_meta_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 img_type TEXT CHECK(img_type IN ('auto', 'manual')),
                 first_img TIMESTAMP WITH TIME ZONE,
                 last_img TIMESTAMP WITH TIME ZONE,
                 last_new_img TIMESTAMP WITH TIME ZONE,
                 public BOOLEAN NOT NULL,
                 public_delay INTERVAL,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 UNIQUE (location, img_type));")

  DBI::dbExecute(con, "CREATE TABLE if not exists forecasts (
                   timeseries_id INTEGER,
                   issue_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   value NUMERIC,
                   min NUMERIC,
                   q10 NUMERIC,
                   q25 NUMERIC,
                   q50 NUMERIC,
                   q75 NUMERIC,
                   q90 NUMERIC,
                   max NUMERIC,
                   PRIMARY KEY (timeseries_id, datetime))")

  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_discrete (
                   timeseries_id INTEGER,
                   target_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE,
                   value NUMERIC NOT NULL,
                   sample_class TEXT,
                   note TEXT,
                   PRIMARY KEY (timeseries_id, datetime))")

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
                 point geometry(POINT, 4269),
                 note TEXT)")

  DBI::dbExecute(con, "CREATE TABLE if not exists documents (
                 document_id SERIAL PRIMARY KEY,
                 location TEXT NOT NUKK,
                 description TEXT NOT NULL,
                 format TEXT NOT NULL,
                 document BYTEA NOT NULL);")

  #The column timeseries_id is auto created for each new entry
  DBI::dbExecute(con, "CREATE TABLE if not exists timeseries (
                 timeseries_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 param_type TEXT NOT NULL, CHECK(param_type IN ('meteorological', 'surface water physical', 'surface water chemistry', 'ground water chemistry', 'ground water physical', 'geochemistry', 'atmospheric chemistry')),
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
                 public_delay INTERVAL,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 note TEXT,
                 UNIQUE (location, parameter, category, period_type, param_type));")

  DBI::dbExecute(con, "CREATE TABLE if not exists extrema (
                 timeseries_id INTEGER PRIMARY KEY,
                 agency TEXT NOT NULL,
                 year NUMERIC NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC NOT NULL,
                 period_type TEXT NOT NULL CHECK(period_type IN ('instantaneous', '1-day', '2-day', '3-day', '4-day', '5-day', '6-day', '7-day', 'monthly', 'yearly')),
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
                 high_first_human_impacts NUMERIC,
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

  internal_status <- data.frame("event" = c("HYDAT_version", "last_new_continuous",  "last_new_discrete", "last_update_daily", "last_sync", "last_sync_discrete", "last_update_watersheds", "last_new_rasters", "last_new_polygons", "last_vacuum", "last_new_images"),
                                "value" = NA)
  DBI::dbAppendTable(con, "internal_status", internal_status)

  # And a table to hold value pairs to control parameter names
  DBI::dbExecute(con, "CREATE TABLE if not exists settings (
                 source_fx TEXT NOT NULL,
                 parameter TEXT NOT NULL,
                 remote_param_name TEXT NOT NULL,
                 PRIMARY KEY (source_fx, parameter))")

  params_AQ <- data.frame("source_fx" = "downloadAquarius",
                          "parameter" = c("level", "flow", "SWE", "snow depth", "distance", "water temperature", "air temperature"),
                          "remote_param_name" = c("Stage.Corrected", "Discharge.Corrected", "SWE.Corrected", "Snow Depth.Corrected", "Distance.Corrected", "Water Temp.Corrected", "Air Temp.Corrected"))
  params_WSC <- data.frame("source_fx" = "downloadWSC",
                           "parameter" = c("level", "flow", "water temperature"),
                           "remote_param_name" = c("46", "47", "5"))
  params_USGS <- data.frame("source_fx" = "downloadNWIS",
                            "parameter" = c("level", "flow", "water temperature"),
                            "remote_param_name" = c("00065", "00060", "00010"))
  params <- rbind(params_AQ, params_WSC, params_USGS)
  try(DBI::dbAppendTable(con, "settings", params))


  #Populate datum_list table from hydat database #############
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


  # Ceate postgis extension and related tables ###############
  #Create the postgis extension, which adds a few necessary tables. Then, create a new schema and set the schema of these tables to decrease clutter in the DB.
  rpostgis::pgPostGIS(con, raster = TRUE)

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

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS raster_series_index (
                 raster_series_id SERIAL PRIMARY KEY,
                 model TEXT UNIQUE NOT NULL,
                 start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 end_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 last_new_raster TIMESTAMP WITH TIME ZONE NOT NULL,
                 public BOOLEAN NOT NULL,
                 public_delay INTERVAL,
                 source_fx TEXT,
                 source_fx_args TEXT);")

  #NOTE: Additional raster tables are created upon first use of the raster addition function.


  #Add in foreign keys ###########
  DBI::dbExecute(con,
                 "ALTER TABLE timeseries
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location)
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE calculated_daily
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id)
  ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE measurements_continuous
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE measurements_discrete
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE extrema
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE forecasts
  ADD CONSTRAINT fk_timeseries_id
  FOREIGN KEY (timeseries_id)
  REFERENCES timeseries(timeseries_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")


  DBI::dbExecute(con,
                 "ALTER TABLE datum_conversions
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE images_index
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  DBI::dbExecute(con,
                 "ALTER TABLE images
  ADD CONSTRAINT fk_img_meta_id
  FOREIGN KEY (img_meta_id)
  REFERENCES images_index(img_meta_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con, "ALTER TABLE images_index FOREIGN KEY images_index.location REFERENCES locations.location")
  DBI::dbExecute(con, "ALTER TABLE images FOREIGN KEY images.img_meta_id REFERENCES images_index.img_meta_id")

  DBI::dbExecute(con,
                 "ALTER TABLE documents
                 ADD CONSTRAINT fk_location
                 FOREIGN KEY (location)
                 REFERENCES locations(location)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE thresholds
                 ADD CONSTRAINT fk_timeseries_id
                 FOREIGN KEY (timeseries_id)
                 REFERENCES timeseries(timeseries_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")


  # Add table comments #############
  # timeseries
  DBI::dbExecute(con, "
  COMMENT ON TABLE timeseries IS 'Provides a record of every timeseries in the database. Each timeseries is unique by its combination of location, parameter, category (continuous or discrete), and period_type. For this purpose, continuous data is data gathered at regular and frequent intervals (usually max 1 day), while discrete data includes infrequent, often manual measurements of values such as snow depth or dissolved element parametes.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN timeseries.timeseries_id IS 'Autoincrements each time a timeseries is added. NOTE that timeseries should only be added using the R function addHydrometTimeseries.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.location IS 'Matches to the locations table.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.category IS 'Discrete or continuous. Continuous data is data gathered at regular and frequent intervals (usually max 1 day), while discrete data includes infrequent, often manual measurements of values such as snow depth or dissolved element parametes.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.period_type IS 'One of instantaneous, sum, mean, median, min, max, or (min+max)/2. This last value is used for the ''daily mean'' temperatures at met stations which are in fact not true mean temps.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.start_datetime IS 'First data point for the timeseries.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.end_datetime IS 'Last data point for the timeseries.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.last_new_data IS 'Time at which data was last appended to the timeseries';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.last_daily_calculation IS 'Time at which daily means were calculated using function calculate_stats. Not used for discrete timeseries.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.last_synchronize IS 'Time at which the timeseries was cross-checked against values held by the remote or partner database; the local store should have been updated to reflect the remote.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.operator IS 'Name of the agency or person responsible for the data represented in this timeseries.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.public_delay IS 'For public = TRUE stations, an option delay with which to serve the data to the public.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.source_fx IS 'Function (from the R package HydroMetDB) to use for incorporation of new data.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.timeseries.source_fx_args IS 'Optional arguments to pass to the source function. See notes in function addHydrometTimeseries for usage.';
  ")

  #measurements_continuous
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements_continuous IS 'Stores observations and imputed values for continuous timeseries.'
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.measurements_continuous.source_fx_args IS 'The periodicity of data can change within a timeseries, for example if recording rates go from every 6 hours to hourly.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.measurements_continuous.source_fx_args IS 'Imputed values may be user-entered.';
  ")

  # settings
  DBI::dbExecute(con, "COMMENT ON TABLE public.settings IS 'This table stores the name of functions used to pull in new data, the parameter with which they are associated, and the remote parameter name to pass to the source function for each function and database parameter name.'
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.source_fx IS 'The R function (from the HydroMetDB package) to use for fetching data.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.source_fx IS 'Parameter names used in the timeseries table.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.source_fx IS 'The parameter name or code to pass to the parameter param_code of the R function specified in source_fx.';
  ")

  # calculated_daily
  DBI::dbExecute(con, "COMMENT ON TABLE public.calculated_daily IS 'Stores calculated daily mean values for timeseries present in table measurements_continuous. Values should not be entered or modified manually but instead are calculated by the HydroMetDB package function calculate_stats.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.imputed IS 'TRUE in this column means that at least one of the measurements used for the daily mean calculation was imputed.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.percent_historic_range IS 'The percent of historical range for that measurement compared to all previous records for the same day of year (not including the current measurement). Only populated once a minimum of three values exist for the current day of year (including the current value). February 29 values are the mean of February 28 and March 1.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.max IS 'Historical max for the day of year, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.min IS 'Historical min for the day of year, excluding current measurement.'
  ")

  # forecasts
  DBI::dbExecute(con, "COMMENT ON TABLE public.forecasts IS 'Holds forecast timeseries information. Each timeseries must match up with a timeseries_id from the timeseries table. Quantiles are optional. Data should be deleted after a certain time interval to prevent unecessarily burdening the database.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.forecasts.issue_datetime IS 'The datetime at which the forecast data point (row) was issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.forecasts.issue_datetime IS 'The datetime for which the forecast data point (row) is valid.'
  ")

  # datum_list
  DBI::dbExecute(con, "COMMENT ON TABLE public.datum_list IS 'Holds datum ids (referenced in the table datum_conversions) and their corresponding names in french and english. Taken directly from the datum list provided by HYDAT. Non-hydat datums can be added with datum_id beginning at 1000.'
  ")

  # datum_conversions
  DBI::dbExecute(con, "COMMENT ON TABLE public.datum_conversions IS 'Holds vertical datum conversions in meters, as well as identifying the most recent conversion for the timeseries.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.datum_conversions.conversion_id IS 'Integer autoincrement column uniquely identifying the conversion.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.datum_conversions.datum_id_from IS 'The datum_id (matching an entry in table datum_list) from which the conversion value is from. Datum_id 10 is equal to the station assumed datum (station 0 relative to some arbitrary local benchmark).'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.datum_conversions.datum_id_to IS 'The datum_id (matching an entry in table datum_list) from which the conversion value is to.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.datum_conversions.conversion_m IS 'The elevation offset in meters to apply if transforming elevation values from the datum_id_from to the datum_id_to.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.datum_conversions.current IS 'TRUE means that the conversion is the most up-to-date in the database. Only one conversion_id can be current for each location.'
  ")

  # images
  DBI::dbExecute(con, "COMMENT ON TABLE public.images IS 'Holds images or local conditions specific to each location. Originally designed to hold auto-captured images at WSC locations, but could be used for other location images. NOT intended to capture what the instrumentation looks like, only what the conditions at the location are.'
  ")

  # internal_status
  DBI::dbExecute(con, "COMMENT ON TABLE public.internal_status IS 'Holds information about when a certain operation took place on the database using the R functions in the HydroMetDB package.'
  ")

  # extrema
  DBI::dbExecute(con, "COMMENT ON TABLE public.extrema IS 'Holds vetted information about extrema specific to each time-series. Can be used for calculating return periods. Entries unique on timeseries_id, agency, year, period_type, condition, extrema, which allows for multiple different types of extrema from different authorities (agencies) for each timeseries.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.agency IS 'The agency (authority) which calculated the extreme value. Ex: Water Resources Branch, Water Survey of Canada, Tetra Tech, etc.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.year IS 'The year for which each value is valid.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.date IS 'The exact date on which the extreme value occured.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.period_type IS 'One of instantaneous, 1-day, 2-day, 3-day, 4-day, 5-day, 6-day, 7-day, monthly, yearly. For example, a 1-day max flow is the maximum 1-day averaged flow for the year; instantaneous max flow is the greatest value single data point recorded for the year.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.condition IS 'One of open water, break-up, freeze-up, winter. Any given timeseries can have one value of each for each year.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.extrema IS 'One of minimum or maximum. Necessary along with other descriptive columns to fully describe what each value represents.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.uncertainty IS ''
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.extrema.deemed_primary IS 'If TRUE then the extrema value is the best (most reliable) value and should be used for most calculations and representations.'
  ")

  # Thresholds
  DBI::dbExecute(con, "COMMENT ON TABLE public.thresholds IS 'Holds threshold values for a variety of things like streamflows, water levels, flood levels, aquatic life inpacts.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.high_advisory IS 'Value at which a high water advisory is to be issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.high_watch IS 'Value at which a high water watch is to be issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.high_warning IS 'Value at which a high water warning is to be issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.flood_minor IS 'Value at which a minor flood is to be declared.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.flood_major IS 'Value at which a major flood is to be declared.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.high_first_human_impacts IS 'High-side value at which first human impacts are known, such as impacts to navigation.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.low_first_human_impacts IS 'Low-side value at which first human impacts are known, such as impacts to navigation.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.low_advisory IS 'Value at which a low water advisory is to be issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.low_watch IS 'Value at which a low water watch is to be issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.low_warning IS 'Value at which a low water warning is to be issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.low_aquatic_life_impacts_minor IS 'Low-side (water temp, level, flow) minor impact threshold to aquatic life.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.low_aquatic_life_impacts_major IS 'Low-side (water temp, level, flow) major impact threshold to aquatic life.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.high_aquatic_life_impacts_minor IS 'High-side (water temp, level, flow) minor impact threshold to aquatic life.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.high_aquatic_life_impacts_major IS 'High-side (water temp, level, flow) major impact threshold to aquatic life.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.fsl IS 'Full supply level (as per water use licence for control structure operations)'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.thresholds.lsl IS 'Low supply level (as per water use licence for control structure operations)'
  ")

  # polygons
  DBI::dbExecute(con, "COMMENT ON TABLE public.polygons IS 'Holds polygons necessary for representing drainage basins, watersheds, etc.'
  ")

  # measurements_discrete
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements_discrete IS 'Holds discrete observations, such as snow survey results, laboratory analyses, etc.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.target_datetime IS 'Optional column to be used for things like snow surveys where the measurements are around a certain target date and need to be plotted with the target date rather than the actual sample date.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.datetime IS 'The datetime on which the measurement was taken, or on which the sample was acquired in the field.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.sample_class IS 'Mostly for aqueous chem samples, to identify the sample as regular, field duplicate, lab duplicate, etc.'
  ")

  # raster_series_index
  DBI::dbExecute(con, "COMMENT ON TABLE public.raster_series_index IS 'Holds metadata about raster series, such as reanalysis or forecast rasters. '
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.raster_series_index.end_datetime IS 'For rasters that have a valid_from and valid_to time, this is the valid_from of the latest raster in the database..'
  ")



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
