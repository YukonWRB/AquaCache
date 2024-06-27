#' Initial hydrometric/meteorological database creation.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Creates a postgreSQL database or replaces an existing database. Establishes pre-set schemas and populates initial rows in the "settings" and "datum_list" tables. No indices are specified as the primary key fulfills this task already.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaCacheCon()].
#' @param overwrite TRUE overwrites the database, if one exists. Nothing will be kept. FALSE will create tables only where they are missing.
#'
#' @return New tables in the target postgres database. 
#' @export
#'

AquaCacheInit <- function(con = AquaCacheCon(), overwrite = FALSE) {

  # Initial setup ############################
  # Overwrite and vacuum if requested
  if (overwrite) {
    DBI::dbExecute(con, "DROP EXTENSION postgis CASCADE")
    for (i in DBI::dbListTables(con)) {
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
  # measurements_continuous table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_continuous (
                 timeseries_id INTEGER NOT NULL,
                 datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 value NUMERIC NOT NULL,
                 grade TEXT,
                 approval TEXT,
                 period INTERVAL,
                 imputed BOOLEAN NOT NULL DEFAULT FALSE,
                 PRIMARY KEY (timeseries_id, datetime))
                 ")
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements_continuous IS 'Stores observations and imputed values for continuous timeseries.'
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.measurements_continuous.period IS 'Greater than 0 for min, max, sum, mean types of measurements. The periodicity of data can change within a timeseries, for example if recording rates go from every 6 hours to hourly.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.measurements_continuous.imputed IS 'Imputed values may be user-entered. Imputed values are automatically replaced if/when a value becomes available on the remote data store.';
  ")

  # calculated_daily table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists calculated_daily (
                 timeseries_id INTEGER NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC,
                 grade TEXT,
                 approval TEXT,
                 imputed BOOLEAN NOT NULL DEFAULT FALSE,
                 percent_historic_range NUMERIC,
                 max NUMERIC,
                 min NUMERIC,
                 q90 NUMERIC,
                 q75 NUMERIC,
                 q50 NUMERIC,
                 q25 NUMERIC,
                 q10 NUMERIC,
                 mean NUMERIC,
                 doy_count INTEGER,
                 PRIMARY KEY (timeseries_id, date));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.calculated_daily IS 'Stores calculated daily mean values for timeseries present in table measurements_continuous. Values should not be entered or modified manually but instead are calculated by the AquaCache package function calculate_stats.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.imputed IS 'TRUE in this column means that at least one of the measurements used for the daily mean calculation was imputed, or, for daily means provided solely in the HYDAT database, that a value was imputed directly to this table.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.percent_historic_range IS 'The percent of historical range for that measurement compared to all previous records for the same day of year (not including the current measurement). Only populated once a minimum of three values exist for the current day of year (including the current value). February 29 values are the mean of February 28 and March 1. 

For example, a value equal to the maximum historic value is equal to 100% of historical range, while one at the miniumu value is 0%. Values above or below the historical range can have values of less than 0 or greater than 100.

The formula used for the calculation is ((current - min) / (max - min)) * 100'
  ")
  
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.max IS 'Historical max for the day of year, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.min IS 'Historical min for the day of year, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.q50 IS 'Historical 50th quantile or median, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.calculated_daily.q25 IS 'Number of measurements existing in the calculated_daily table for each day including historic and current measurement.'")

  # images table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists images (
                   image_id SERIAL PRIMARY KEY,
                   img_meta_id INTEGER NOT NULL,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   fetch_datetime TIMESTAMP WITH TIME ZONE,
                   format TEXT NOT NULL,
                   file BYTEA NOT NULL,
                   description TEXT,
                   UNIQUE (img_meta_id, datetime));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.images IS 'Holds images of local conditions specific to each location. Originally designed to hold auto-captured images at WSC locations, but could be used for other location images. NOT intended to capture what the instrumentation looks like, only what the conditions at the location are.'")

  # images_index table #################
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS images_index (
                 img_meta_id SERIAL PRIMARY KEY,
                 img_type TEXT NOT NULL CHECK(img_type IN ('auto', 'manual')),
                 first_img TIMESTAMP WITH TIME ZONE,
                 last_img TIMESTAMP WITH TIME ZONE,
                 last_new_img TIMESTAMP WITH TIME ZONE,
                 public BOOLEAN NOT NULL,
                 public_delay INTERVAL,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 description TEXT,
                 location_id INTEGER NOT NULL,
                 active BOOLEAN,
                 UNIQUE (location_id, img_type));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.images_index IS 'Index for images table. Each location at which there is one or more image gets an entry here; images in table images are linked to this table using the img_meta_id.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.images_index.active IS 'Defines if the image series should or should not be imported.'")

  # forecasts table #################
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
  DBI::dbExecute(con, "COMMENT ON TABLE public.forecasts IS 'Holds forecast timeseries information. Each timeseries must match up with a timeseries_id from the timeseries table. Quantiles are optional. Data should be deleted after a certain time interval to prevent unecessarily burdening the database.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.forecasts.issue_datetime IS 'The datetime at which the forecast data point (row) was issued.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.forecasts.issue_datetime IS 'The datetime for which the forecast data point (row) is valid.'
  ")

  # measurements_discrete table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_discrete (
                   timeseries_id INTEGER,
                   target_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE,
                   value NUMERIC NOT NULL,
                   sample_class TEXT,
                   note TEXT,
                   PRIMARY KEY (timeseries_id, datetime, sample_class);")
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements_discrete IS 'Holds discrete observations, such as snow survey results, laboratory analyses, etc.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.target_datetime IS 'Optional column to be used for things like snow surveys where the measurements are around a certain target date and need to be plotted with the target date rather than the actual sample date.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.datetime IS 'The datetime on which the measurement was taken, or on which the sample was acquired in the field.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.sample_class IS 'Mostly for aqueous chem samples, to identify the sample as regular, field duplicate, lab duplicate, etc.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.value IS 'Values below the detection limit are listed as the negative of the detection limit to keep everything numeric.'
  ")

  # sample_class table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists sample_class (
                 code TEXT PRIMARY KEY,
                 description TEXT NOT NULL,
                 description_fr TEXT);")
  class <- data.frame("code" = c("M", "D", "I", "U"),
                      "description" = c("Monitoring (routine)", "Duplicate/Replicate or split sample", "Incident response", "Undefined"))
  DBI::dbAppendTable(con, "sample_class", class)
  DBI::dbExecute(con,
                 "ALTER TABLE measurements_discrete
                 ADD CONSTRAINT fk_class
                 FOREIGN KEY (sample_class)
                 REFERENCES sample_class(code)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION check_sample_class_exists()
RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM sample_class WHERE code = NEW.sample_class) THEN
        RAISE EXCEPTION 'Invalid sample class code: %', NEW.sample_class;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "CREATE TRIGGER before_insert_or_update_sample_class
BEFORE INSERT OR UPDATE OF sample_class ON measurements_discrete
FOR EACH ROW
EXECUTE FUNCTION check_sample_class_exists();
")

  # grades table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists grades (
                 code TEXT PRIMARY KEY,
                 description TEXT NOT NULL,
                 description_fr TEXT);")
  grades <- data.frame("code" = c("A", "B", "C", "D", "E", "N", "R", "I", "U", "S", "Z"),
                       "description" = c("Excellent", "Good", "Fair", "Poor", "Estimated", "Unusable", "Draw down recovery", "Ice", "Undefined", "Sensor issues", "Unknown"))
  DBI::dbAppendTable(con, "grades", grades)
  DBI::dbExecute(con,
                 "ALTER TABLE measurements_continuous
                 ADD CONSTRAINT fk_grade
                 FOREIGN KEY (grade)
                 REFERENCES grades(code)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  DBI::dbExecute(con,
                 "ALTER TABLE calculated_daily
                 ADD CONSTRAINT fk_grade
                 FOREIGN KEY (grade)
                 REFERENCES grades(code)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION check_grade_exists_continuous()
RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM grades WHERE code = NEW.grade) THEN
        RAISE EXCEPTION 'Invalid grade code: %', NEW.grade;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "CREATE TRIGGER before_insert_or_update_grade_continuous
BEFORE INSERT OR UPDATE OF grade ON measurements_continuous
FOR EACH ROW
EXECUTE FUNCTION check_grade_exists_continuous();
")

  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION check_grade_exists_daily()
RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM grades WHERE code = NEW.grade) THEN
        RAISE EXCEPTION 'Invalid grade code: %', NEW.grade;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "CREATE TRIGGER before_insert_or_update_grade_daily
BEFORE INSERT OR UPDATE OF grade ON calculated_daily
FOR EACH ROW
EXECUTE FUNCTION check_grade_exists_daily();
")


  # approvals table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists approvals (
                 code TEXT PRIMARY KEY,
                 description TEXT NOT NULL,
                 description_fr TEXT);")
  approvals <- data.frame("code" = c("A", "C", "R", "N", "U", "Z"),
                          "description" = c("Approved", "Work up complete: ready for review", "Reviewed, pending approval", "Not reviewed", "Undefined", "Unknown"))
  DBI::dbAppendTable(con, "approvals", approvals)
  DBI::dbExecute(con,
                 "ALTER TABLE measurements_continuous
                 ADD CONSTRAINT fk_approval
                 FOREIGN KEY (approval)
                 REFERENCES approvals(code)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  DBI::dbExecute(con,
                 "ALTER TABLE calculated_daily
                 ADD CONSTRAINT fk_approval
                 FOREIGN KEY (approval)
                 REFERENCES approvals(code)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  #Function and trigger for checking measurements_continuous
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION check_approval_exists_continuous()
RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM approvals WHERE code = NEW.approval) THEN
        RAISE EXCEPTION 'Invalid approval code: %', NEW.approval;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "CREATE TRIGGER before_insert_or_update_approval_on_continuous
BEFORE INSERT OR UPDATE OF approval ON measurements_continuous
FOR EACH ROW
EXECUTE FUNCTION check_approval_exists_continuous();
")
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION check_approval_exists_daily()
RETURNS TRIGGER AS $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM approvals WHERE code = NEW.approval) THEN
        RAISE EXCEPTION 'Invalid approval code: %', NEW.approval;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "CREATE TRIGGER before_insert_or_update_approval_on_daily
BEFORE INSERT OR UPDATE OF approval ON calculated_daily
FOR EACH ROW
EXECUTE FUNCTION check_approval_exists_daily();
")


  # datum_list table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists datum_list (
    datum_id INTEGER PRIMARY KEY,
    datum_name_en TEXT NOT NULL,
    datum_name_fr TEXT NOT NULL);")
  DBI::dbExecute(con, "COMMENT ON TABLE public.datum_list IS 'Holds datum ids (referenced in the table datum_conversions) and their corresponding names in french and english. Taken directly from the datum list provided by HYDAT. Non-hydat datums can be added with datum_id beginning at 1000.'
  ")
  
  # datum_conversions table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists datum_conversions (
                 conversion_id SERIAL PRIMARY KEY,
                 location_id INTEGER NOT NULL,
                 datum_id_from INTEGER NOT NULL REFERENCES datum_list (datum_id),
                 datum_id_to INTEGER NOT NULL REFERENCES datum_list (datum_id),
                 conversion_m NUMERIC NOT NULL,
                 current BOOLEAN NOT NULL,
                 UNIQUE (location_id, datum_id_to, current));")
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

  # locations table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists locations (
                 location_id SERIAL PRIMARY KEY,
                 location TEXT UNIQUE NOT NULL,
                 name TEXT UNIQUE NOT NULL,
                 name_fr UNIQUE TEXT,
                 latitude NUMERIC NOT NULL,
                 longitude NUMERIC NOT NULL
                 contact TEXT,
                 geom_id INTEGER NOT NULL,
                 note TEXT
                 );")
  
  
  # networks tables ##################################################
  DBI::dbExecute(con, "CREATE TABLE if not exists networks (
                 network_id SERIAL PRIMARY KEY,
                 name TEXT UNIQUE NOT NULL,
                 name_fr TEXT TUNIQUE,
                 description TEXT NOT NULL,
                 description_fr TEXT,
                 type TEXT NOT NULL CHECK(type IN ('research', 'monitoring', 'other'))
                 );")
  tbl <- data.frame(name = c("Canada Yukon Hydrometric Network", "ECCC Meteorology Network", "Small Stream Network", "Snow Survey Network", "National Streamflow Network (US)", "Yukon Observational Well Network", "Highway Observation Network", "Yukon Water Quality Network - Water Licence Data", "Yukon Water Quality Network - Groundwater Monitoring", "Yukon Water Quality Network - Surface Water Monitoring"),
                    description = c("Monitoring of large water bodies and rivers in Yukon and surrounding jurisdictions.", "Monitoring of meteorological conditions, largely at airports.", "Monitoring of smaller waterbodies and rivers/streams in Yukon.", "Monitoring of snow pack conditions in Yukon and surrounding jursidictions.", "Monitoring of large stream conditions at sites within Alaska.", "Monitoring of groundwater conditions at dedicated and adopted wells in Yukon.", "Monitoring of water quality (chemical) parameters at background and disturbance sites, often linked to water use licences.", "Monitoring of water quality parameters at sites subject to a water use licence.", "Monitoring of background ground water quality parameters.", "Monitoring of background surface water quality parameters."),
                    type = "monitoring")
  DBI::dbAppendTable(con, "networks", tbl)
  
  # make intermediary table for locations_networks
  DBI::dbExecute(con, "CREATE TABLE if not exists locations_networks (
                 network_id INTEGER NOT NULL REFERENCES networks (network_id),
                 location_id INTEGER NOT NULL REFERENCES locations (location_id),
                 UNIQUE (network_id, location_id));")
  
  
  # projects table #################################################
  DBI::dbExecute(con, "CREATE TABLE if not exists projects (
                 project_id SERIAL PRIMARY KEY,
                 name TEXT UNIQUE NOT NULL,
                 name_fr TEXT UNIQUE,
                 description TEXT NOT NULL,
                 description_fr TEXT,
                 type TEXT NOT NULL CHECK(type IN ('research', 'monitoring', 'other', 'incident response'))
                 );")
  
  # Since one location can have multiple projects, and one project can have multiple locations, we need a many-to-many relationship.
  # make intermediary table for locations_projects
  DBI::dbExecute(con, "CREATE TABLE if not exists locations_projects (
                 project_id INTEGER NOT NULL REFERENCES projects (project_id),
                 location_id INTEGER NOT NULL REFERENCES locations (location_id),
                 UNIQUE (project_id, location_id));")
  

  # documents tables #################
  DBI::dbExecute(con, "CREATE TABLE if not exists document_types (
                 document_type_id SERIAL PRIMARY KEY,
                 document_type_en TEXT NOT NULL UNIQUE,
                 document_type_fr TEXT NOT NULL UNIQUE,
                 description_en TEXT,
                 description_fr TEXT);")
  
  DBI::dbExecute(con, "CREATE TABLE if not exists documents (
                 document_id SERIAL PRIMARY KEY,
                 name TEXT UNIQUE NOT NULL,
                 type INTEGER NOT NULL,
                 has_points BOOLEAN NOT NULL DEFAULT FALSE,
                 has_lines BOOLEAN NOT NULL DEFAULT FALSE,
                 has_polygons BOOLEAN NOT NULL DEFAULT FALSE,
                 authors TEXT[],
                 url TEXT,
                 publish_date DATE,
                 description TEXT NOT NULL,
                 format TEXT NOT NULL,
                 document BYTEA NOT NULL,
                 public BOOLEAN NOT NULL DEFAULT FALSE,
                 FOREIGN KEY (type) REFERENCES document_types(document_type_id) ON UPDATE CASCADE ON DELETE CASCADE);")
  DBI::dbExecute(con, "COMMENT ON TABLE public.documents IS 'Holds documents and metadata associated with each document. Each document can be associated with one or more location, line, or polygon, or all three.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.documents.document_type IS 'One of thesis, report, well log, conference paper, poster, journal article, map, graph, protocol, grading scheme, metadata, other'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.documents.has_points IS 'Flag to indicate that the document_spatial has a point entry for this document.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.documents.authors IS 'An *array* of one or more authors.'")


  # timeseries table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists timeseries (
                 timeseries_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 parameter INTEGER NOT NULL,
                 param_type INTEGER NOT NULL,
                 category TEXT NOT NULL CHECK(category IN ('discrete', 'continuous')),
                 period_type TEXT NOT NULL CHECK(period_type IN ('instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2')),
                 record_rate TEXT NOT NULL,
                 z NUMERIC,
                 start_datetime TIMESTAMP WITH TIME ZONE,
                 end_datetime TIMESTAMP WITH TIME ZONE,
                 last_new_data TIMESTAMP WITH TIME ZONE,
                 last_daily_calculation TIMESTAMP WITH TIME ZONE,
                 last_synchronize TIMESTAMP WITH TIME ZONE,
                 network TEXT,
                 public BOOLEAN NOT NULL,
                 public_delay INTERVAL,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 active BOOLEAN NOT NULL DEFAULT TRUE,
                 note TEXT,
                 UNIQUE (location, parameter, category, period_type, param_type, record_rate, z),
                 CONSTRAINT check_record_rate_constraints
                     CHECK (
                     (category = 'discrete' AND record_rate IS NULL) OR
                     (category = 'continuous' AND record_rate IN ('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))
                     )
                 );")
  DBI::dbExecute(con, "
  COMMENT ON TABLE public.timeseries IS 'Provides a record of every timeseries in the database. Each timeseries is unique by its combination of location, parameter, param_type, category (continuous or discrete), period_type, record_rate, and z (elevation).Continuous data is data gathered at regular and usually frequent intervals, while discrete data includes infrequent, often manual measurements of values such as snow depth or dissolved element parameters.';
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.active IS 'Defines if the timeseries should or should not be added to or back-corrected by various AquaCache package functions.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.z IS 'Elevation of the measurement station, in meters. Used for things like thermistor strings, wind towers, or forecast climate parameters at different heights. Z elevations should be taken in the context of the location's assigned elevation and datum.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.timeseries_id IS 'Autoincrements each time a timeseries is added. NOTE that timeseries should only be added using the R function addACTimeseries.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.location_id IS 'Matches to the locations table.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.category IS 'Discrete or continuous. Continuous data is data gathered at regular and frequent intervals (usually max 1 day), while discrete data includes infrequent, often manual measurements of values such as snow depth or dissolved element parametes.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.period_type IS 'One of instantaneous, sum, mean, median, min, max, or (min+max)/2. This last value is used for the ''daily mean'' temperatures at met stations which are in fact not true mean temps.'; ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.record_rate IS 'For continuous timeseries, one of < 1 day, 1 day, 1 week, 4 weeks, 1 month, year. For discrete timeseries, NULL.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.start_datetime IS 'First data point for the timeseries.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.end_datetime IS 'Last data point for the timeseries.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.last_new_data IS 'Time at which data was last appended to the timeseries';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.last_daily_calculation IS 'Time at which daily means were calculated using function calculate_stats. Not used for discrete timeseries.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.last_synchronize IS 'Time at which the timeseries was cross-checked against values held by the remote or partner database; the local store should have been updated to reflect the remote.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.public_delay IS 'For public = TRUE stations, an option delay with which to serve the data to the public.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.source_fx IS 'Function (from the R package AquaCache) to use for incorporation of new data.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.source_fx_args IS 'Optional arguments to pass to the source function. See notes in function addACTimeseries for usage.';")
  
  # parameters table #################
  DBI::dbExecute(con, "CREATE TABLE parameters (
               param_code SERIAL PRIMARY KEY,
               param_name TEXT UNIQUE NOT NULL,
               unit TEXT UNIQUE NOT NULL,
               param_name_fr TEXT UNIQUE NOT NULL,
               group TEXT NOT NULL,
               group_fr TEXT NOT NULL,
               sub_group TEXT,
               sub_group_fr TEXT,
               description TEXT
               description_fr TEXT,
               plot_default_y_orientation TEXT NOT NULL CHECK(plot_default_y_orientation IN ('normal', 'inverted')),
                 plot_default_floor NUMERIC,
                 plot_default_ceiling NUMERIC);")
  
  # param_types table #################
  DBI::dbExecute(con, "CREATE TABLE param_types (
               param_type_code SERIAL PRIMARY KEY,
               param_type TEXT UNIQUE NOT NULL,
               param_type_fr TEXT UNIQUE NOT NULL,
               description TEXT,
               description_fr TEXT);")

  # extrema table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists extrema (
                 timeseries_id INTEGER PRIMARY KEY,
                 agency TEXT NOT NULL,
                 year NUMERIC NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC NOT NULL,
                 period_type TEXT NOT NULL CHECK(period_type IN ('instantaneous', '1-day', '2-day', '3-day', '4-day', '5-day', '6-day', '7-day', 'monthly', 'yearly')),
                 condition TEXT NOT NULL CHECK(condition IN ('open water', 'break-up', 'freeze-up', 'winter')),
                 extrema TEXT NOT NULL CHECK(extrema IN ('minimum', 'maximum')),
                 notes TEXT,
                 deemed_primary BOOLEAN NOT NULL,
                 UNIQUE (timeseries_id, agency, year, period_type, condition, extrema));")
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

  # thresholds table #################
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

  # internal_status table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists internal_status (
                 event TEXT NOT NULL,
                 value TIMESTAMP WITH TIME ZONE,
                 PRIMARY KEY (event))")
  DBI::dbExecute(con, "COMMENT ON TABLE public.internal_status IS 'Holds information about when a certain operation took place on the database using the R functions in the AquaCache package.'
  ")
  internal_status <- data.frame("event" = c("HYDAT_version", "last_new_continuous",  "last_new_discrete", "last_update_daily", "last_sync", "last_sync_discrete", "last_new_rasters", "last_new_vectors", "last_vacuum", "last_new_images"),
                                "value" = NA)
  DBI::dbAppendTable(con, "internal_status", internal_status)

  # settings table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists settings (
                 source_fx TEXT NOT NULL,
                 parameter INTEGER NOT NULL,
                 period_type TEXT NOT NULL,
                 record_rate TEXT,
                 remote_param_name TEXT NOT NULL,
                 UNIQUE (source_fx, parameter, period_type, record_rate))")
  DBI::dbExecute(con, "COMMENT ON TABLE public.settings IS 'This table stores the name of functions used to pull in new data, the parameter with which they are associated, and the remote parameter name to pass to the source function for each function and database parameter name.'
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.source_fx IS 'The R function (from the AquaCache package) to use for fetching data.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.parameter IS 'Parameter integer codes used in the timeseries table.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.remote_param_name IS 'The parameter name or code to pass to the parameter param_code of the R function specified in source_fx.';
  ")

  #Populate datum_list table from hydat database #############
  #Check hydat version, update if needed.
  tryCatch({hydat_path <- tidyhydat::hy_downloaded_db() #Attempts to get the hydat path, in case it's downloaded already.
  local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
  }, error = function(e) {hydat_path <- NULL})

  if (!is.null(hydat_path) & exists("local_hydat")) { #If hydat already exists, compare version numbers
    local_hydat <- gsub("-", "", as.character(local_hydat))
    remote_hydat <- tidyhydat::hy_remote()
    if (local_hydat != remote_hydat) { #if remote version is more recent, download new version
      tidyhydat::download_hydat(ask = FALSE)
      hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
    }
  } else if (is.null(hydat_path) | !exists("local_hydat")) {# if hydat does not already exist, download fresh to the default location
    tidyhydat::download_hydat(ask = FALSE)
    hydat_path <- tidyhydat::hy_downloaded_db()
  }

  hydat <- DBI::dbConnect(RSQLite::SQLite(), hydat_path)
  on.exit(DBI::dbDisconnect(hydat))

  datums_existing <- DBI::dbGetQuery(con, "SELECT datum_id FROM datum_list")
  if (nrow(datums_existing) == 0) {
    datum_list <- DBI::dbReadTable(hydat, "DATUM_LIST")
    names(datum_list) <- c("datum_id", "datum_name_en", "datum_name_fr")
    DBI::dbAppendTable(con, "datum_list", datum_list)
  }


  # Ceate postgis extension and related tables ###############
  #Create the postgis extension, which adds a few necessary tables. Then, create a new schema and set the schema of these tables to decrease clutter in the DB.
  rpostgis::pgPostGIS(con, raster = TRUE)

  # vectors table ################################################
  DBI::dbExecute(con, "CREATE TABLE if not exists vectors (
               geom_id SERIAL PRIMARY KEY,
               geom_type TEXT NOT NULL CHECK(geom_type IN ('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon')),
               layer_name TEXT NOT NULL,
               feature_name TEXT NOT NULL,
               description TEXT,
               geom GEOMETRY(Geometry, 4269) NOT NULL,
               CONSTRAINT enforce_dims_geom CHECK (st_ndims(geom) = 2),
               CONSTRAINT enforce_srid_geom CHECK (st_srid(geom) = 4269),
               CONSTRAINT enforce_valid_geom CHECK (st_isvalid(geom)),
               UNIQUE (layer_name, feature_name, geom_type)
               );")
  DBI::dbExecute(con, "COMMENT ON TABLE public.vectors IS 'Holds points, lines, or polygons as geometry objects that can be references by other tables. For example, the locations table references a geom_id for each location. Retrieve objects from this table using function AquaCache::fetchVector, insert them using AquaCache::insertACVector.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.vectors.geom IS 'Enforces epsg:4269 (NAD83).';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.vectors.layer_name IS 'Non-optional descriptive name for the layer.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.vectors.feature_name IS 'Non-optional descriptive name for the feature.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.vectors.description IS 'Optional but highly recommended long-form description of the geometry object.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.vectors.geom_type IS '*DO NOT TOUCH* Auto-populated by trigger based on the geometry type for each entry.';")

  # -- Create an UPDATE trigger to populate geom_type_auto
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION update_geom_type()
RETURNS TRIGGER AS $$
  BEGIN
NEW.geom_type := ST_GeometryType(NEW.geom);
RETURN NEW;
END;
$$ LANGUAGE plpgsql;")
  DBI::dbExecute(con, "CREATE TRIGGER update_geom_type_trigger
BEFORE INSERT OR UPDATE ON vectors
FOR EACH ROW
EXECUTE FUNCTION update_geom_type();
")
  DBI::dbExecute(con, "CREATE INDEX geometry_idx ON vectors USING GIST (geom);") #Forces use of GIST indexing which is necessary for large polygons

  # documents_spatial table ################################################
  # Create table to link the documents to geoms (since documents can be associated with n number of geoms)
  DBI::dbExecute(con, "CREATE TABLE documents_spatial (
  document_id INT REFERENCES documents(document_id),
  geom_id INT REFERENCES vectors(geom_id),
  PRIMARY KEY (document_id, geom_id)
);")

  # raster tables ################################################
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS raster_series_index (
                 raster_series_id SERIAL PRIMARY KEY,
                 model TEXT NOT NULL,
                 type TEXT NOT NULL CHECK(type IN ('reanalysis', 'forecast')),
                 parameter TEXT NOT NULL,
                 param_description TEXT,
                 start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 end_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 last_new_raster TIMESTAMP WITH TIME ZONE NOT NULL,
                 last_issue TIMESTAMP WITH TIME ZONE,
                 public BOOLEAN NOT NULL,
                 public_delay INTERVAL,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 active BOOLEAN NOT NULL DEFAULT TRUE,
                 UNIQUE (model, parameter));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.raster_series_index IS 'Holds metadata about raster series, such as reanalysis or forecast rasters. '
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.raster_series_index.end_datetime IS 'For rasters that have a valid_from and valid_to time, this is the valid_from of the latest raster in the database.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.raster_series_index.active IS 'Defines if the raster series should or should not be imported.'")

  #NOTE: Additional raster tables are created upon first use of the raster addition function.


  #Add in foreign keys ###########
  DBI::dbExecute(con, "ALTER TABLE settings 
                 ADD CONSTRAINT fk_parameter
                 FOREIGN KEY (parameter)
                 REFERENCES parameters(param_code)
                 ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con,
                 "ALTER TABLE locations
  ADD CONSTRAINT fk_geom_id
  FOREIGN KEY (geom_id)
  REFERENCES vectors(geom_id)
                 ON UPDATE CASCADE ON DELETE CASCADE;")
  DBI::dbExecute(con,
                 "ALTER TABLE timeseries
  ADD CONSTRAINT fk_location_id
  FOREIGN KEY (location_id)
  REFERENCES locations(location_id)
                 ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con, "ALTER TABLE timeseries ADD CONSTRAINT fk_parameter FOREIGN KEY (parameter) REFERENCES parameters(param_code) ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con, "ALTER TABLE timeseries ADD CONSTRAINT fk_param_type FOREIGN KEY (param_type) REFERENCES param_types(param_type_code) ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con,
                 "ALTER TABLE timeseries
  ADD CONSTRAINT fk_location_id
  FOREIGN KEY (location_id)
  REFERENCES locations(location_id)
                 ON UPDATE CASCADE ON DELETE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE timeseries
  ADD CONSTRAINT fk_location
  FOREIGN KEY (location)
  REFERENCES locations(location)
                 ON UPDATE CASCADE ON DELETE CASCADE;")

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
  ADD CONSTRAINT fk_location_id
  FOREIGN KEY (location_id)
  REFERENCES locations(location_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE images_index
  ADD CONSTRAINT fk_location_id
  FOREIGN KEY (location_id)
  REFERENCES locations(location_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  
  DBI::dbExecute(con,
                 "ALTER TABLE images
  ADD CONSTRAINT fk_img_meta_id
  FOREIGN KEY (img_meta_id)
  REFERENCES images_index(img_meta_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")

  DBI::dbExecute(con,
                 "ALTER TABLE thresholds
                 ADD CONSTRAINT fk_timeseries_id
                 FOREIGN KEY (timeseries_id)
                 REFERENCES timeseries(timeseries_id)
                 ON DELETE CASCADE
                 ON UPDATE CASCADE;")
  

  #Create triggers and functions ########################################
  # Create functions and triggers to update the boolean flags in the documents table
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION update_document_flags_after_insert()
RETURNS TRIGGER AS $$
  BEGIN
-- Check the geom_type for the inserted record
IF (SELECT geom_type FROM vectors WHERE geom_id = NEW.geom_id) IN ('ST_Point', 'ST_MultiPoint') THEN
UPDATE documents
SET has_points = TRUE
WHERE document_id = NEW.document_id;
END IF;
IF (SELECT geom_type FROM vectors WHERE geom_id = NEW.geom_id) IN ('ST_LineString', 'ST_MultiLineString') THEN
UPDATE documents
SET has_lines = TRUE
WHERE document_id = NEW.document_id;
END IF;
IF (SELECT geom_type FROM vectors WHERE geom_id = NEW.geom_id) IN ('ST_Polygon', 'ST_MultiPolygon') THEN
UPDATE documents
SET has_polygons = TRUE
WHERE document_id = NEW.document_id;
END IF;

RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")

  DBI::dbExecute(con, "CREATE TRIGGER documents_spatial_after_insert
AFTER INSERT ON documents_spatial
FOR EACH ROW
EXECUTE FUNCTION update_document_flags_after_insert();
               ")



  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION update_document_flags_after_delete()
RETURNS TRIGGER AS $$
  BEGIN
-- Check if there are still entries for the document_id with geom_type 'POINT'
IF NOT EXISTS (
  SELECT 1
  FROM documents_spatial ds
  JOIN vectors g ON ds.geom_id = g.geom_id
  WHERE ds.document_id = OLD.document_id AND g.geom_type IN ('ST_Point', 'ST_MultiPoint')
) THEN
UPDATE documents
SET has_points = FALSE
WHERE document_id = OLD.document_id;
END IF;

IF NOT EXISTS (
  SELECT 1
  FROM documents_spatial ds
  JOIN vectors g ON ds.geom_id = g.geom_id
  WHERE ds.document_id = OLD.document_id AND g.geom_type IN ('ST_LineString', 'ST_MultiLineString')
  ) THEN
UPDATE documents
SET has_points = FALSE
WHERE document_id = OLD.document_id;
END IF;

IF NOT EXISTS (
  SELECT 1
  FROM documents_spatial ds
  JOIN vectors g ON ds.geom_id = g.geom_id
  WHERE ds.document_id = OLD.document_id AND g.geom_type IN ('ST_Polygon', 'ST_MultiPolygon')
  ) THEN
UPDATE documents
SET has_polygons = FALSE
WHERE document_id = OLD.document_id;
END IF;


RETURN OLD;
END;
$$ LANGUAGE plpgsql;
")

  DBI::dbExecute(con, "CREATE TRIGGER documents_spatial_after_delete
AFTER DELETE ON documents_spatial
FOR EACH ROW
EXECUTE FUNCTION update_document_flags_after_delete();
               ")

  # Create view table(s) ########################################
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW measurements_hourly AS
SELECT
    timeseries_id,
    date_trunc('hour', datetime) AS datetime,
    AVG(value) AS value,
    (
        SELECT grade 
        FROM unnest(array_agg(grade)) AS unq(grade)
        GROUP BY grade
        ORDER BY COUNT(*) DESC
        LIMIT 1
    ) AS grade,
    (
        SELECT approval 
        FROM unnest(array_agg(approval)) AS unq(approval)
        GROUP BY approval
        ORDER BY COUNT(*) DESC
        LIMIT 1
    ) AS approval,
    BOOL_OR(imputed) AS imputed
FROM
    measurements_continuous
GROUP BY
    timeseries_id,
    date_trunc('hour', datetime)
ORDER BY
    datetime;
")
  
  # Views for timeseries metadata
  DBI::dbExecute(con, paste("CREATE OR REPLACE VIEW public.timeseries_metadata_en AS",
                            "SELECT ts.timeseries_id, ptypes.param_type AS parameter_type, params.group AS parameter_group, ts.category, params.param_name AS parameter_name, params.unit, ts.period_type, ts.record_rate AS recording_rate, ts.start_datetime, ts.end_datetime, ts.note, loc.location_id, loc.location AS location_code, loc.name AS location_name, loc.latitude, loc.longitude, ts.public",
                            "FROM timeseries AS ts ",
                            "JOIN locations AS loc ON ts.location_id = loc.location_id ",
                            "LEFT JOIN parameters AS params ON ts.parameter = params.param_code",
                            "LEFT JOIN param_types AS ptypes ON ts.param_type = ptypes.param_type_code",
                            "ORDER BY ts.timeseries_id;"))
  
  DBI::dbExecute(con, paste("CREATE OR REPLACE VIEW public.timeseries_metadata_fr AS",
                            "SELECT ts.timeseries_id, ptypes.param_type_fr AS type_de_paramètre, params.group_fr AS groupe_de_paramètres, ts.category, params.param_name_fr AS nom_paramètre, params.unit AS unités, ts.period_type, ts.record_rate AS recording_rate, ts.start_datetime AS début, ts.end_datetime AS fin, ts.note, loc.location AS location_code, loc.name_fr AS nom_endroit, loc.latitude, loc.longitude, ts.public",
                            "FROM timeseries AS ts ",
                            "JOIN locations AS loc ON ts.location_id = loc.location_id ",
                            "LEFT JOIN parameters AS params ON ts.parameter = params.param_code",
                            "LEFT JOIN param_types AS ptypes ON ts.param_type = ptypes.param_type_code",
                            "ORDER BY ts.timeseries_id;"))
  
  # View for location metadata
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW location_metadata_en AS
SELECT
    loc.location_id,
    loc.location AS location_code,
    loc.name AS name,
    loc.latitude,
    loc.longitude,
    dc.conversion_m AS elevation,
    dl.datum_name_en AS datum,
    loc.note,
    array_agg(DISTINCT proj.name) AS projects,
    array_agg(DISTINCT net.name) AS networks
FROM locations AS loc
LEFT JOIN locations_projects AS loc_proj ON loc.location_id = loc_proj.location_id
LEFT JOIN projects AS proj ON loc_proj.project_id = proj.project_id
LEFT JOIN locations_networks AS loc_net ON loc.location_id = loc_net.location_id
LEFT JOIN networks AS net ON loc_net.network_id = net.network_id
LEFT JOIN datum_conversions AS dc ON loc.location_id = dc.location_id AND dc.current = TRUE
LEFT JOIN datum_list AS dl ON dc.datum_id_to = dl.datum_id
GROUP BY loc.location_id, loc.location, loc.name, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_en;
"
  )
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW location_metadata_fr AS
SELECT
    loc.location_id,
    loc.location AS code_de_site,
    loc.name_fr AS nom,
    loc.latitude,
    loc.longitude,
    dc.conversion_m AS altitude,
    dl.datum_name_fr AS datum,
    loc.note,
    array_agg(DISTINCT proj.name_fr) AS projets,
    array_agg(DISTINCT net.name_fr) AS réseaux
FROM locations AS loc
LEFT JOIN locations_projects AS loc_proj ON loc.location_id = loc_proj.location_id
LEFT JOIN projects AS proj ON loc_proj.project_id = proj.project_id
LEFT JOIN locations_networks AS loc_net ON loc.location_id = loc_net.location_id
LEFT JOIN networks AS net ON loc_net.network_id = net.network_id
LEFT JOIN datum_conversions AS dc ON loc.location_id = dc.location_id AND dc.current = TRUE
LEFT JOIN datum_list AS dl ON dc.datum_id_to = dl.datum_id
GROUP BY loc.location_id, loc.location, loc.name_fr, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_fr;
"
  )

  # Create a read-only account ########################################
  tryCatch({
    DBI::dbExecute(con, "CREATE ROLE AquaCache_read WITH LOGIN PASSWORD 'AquaCache';")
    DBI::dbExecute(con, "GRANT CONNECT ON DATABASE AquaCache TO AquaCache_read;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public TO AquaCache_read;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO AquaCache_read;")
  }, error = function(e) {
    warning("May have failed to create new read only account with name AquaCache_read. Ignore this message if it already exists (this function would not have erased the old account).")
  })

  message("The database was successfully created. If a read-only account was created it has username AquaCache_read and password AquaCache")
}
