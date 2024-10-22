#' Initial hydrometric/meteorological database creation.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' NOTE: there is no guarantee that the code in this function will work as expected. It is intended to be a starting point for creating a database schema and populating it with initial values. The user should review the code and make any necessary changes to suit their needs. It may be necessary to run the code line by line rather than as a function if failure occurs, as it is possible that SQL errors exist in this script.
#' 
#' Populates a postgreSQL database with tables, pre-set information, relationships, checks, triggers, etc. designed to hold timeseries and discrete data, documents, images, rasters, vectors, and other data types relevant to hydrology, meteorology, water quality, geochemistry, and other physical sciences.. Establishes pre-set schemas and populates some tables with initial values. Note that no indices are specified as the primary keys fulfill this task already. 
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param overwrite TRUE overwrites the database, if one exists. Nothing will be kept. FALSE will create tables only where they are missing.
#'
#' @return New tables in the target postgres database. 
#' @export
#'

AquaCacheInit <- function(con = AquaConnect(), overwrite = FALSE) {

  stop("This function is no longer being co-edited alon with database changes. We recommend contacting the package maintainer for a .SQL file that will enable you to be up to date with the latest database changes.")
  
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
  
  # user and user group tables #################
  DBI::dbExecute(con, "CREATE TABLE if not exists user_groups (
                 group_id SERIAL PRIMARY KEY,
                 group_name TEXT UNIQUE NOT NULL,
                 group_description TEXT NOT NULL);")
  
  groups <- data.frame(group_name = c("public"),
                       group_description = c("Default group for all public users."))
  
  DBI::dbAppendTable(con, "user_groups", groups)
  
  # Create function and trigger to make sure that the public group cannot be deleted
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION prevent_delete_public_group()
RETURNS TRIGGER AS $$
BEGIN
    IF OLD.group_id = 1 THEN
        RAISE EXCEPTION 'Cannot delete the public group (group_id 1)';
    END IF;
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "CREATE TRIGGER prevent_delete_public_group_trigger
BEFORE DELETE ON user_groups
FOR EACH ROW
EXECUTE FUNCTION prevent_delete_public_group();
")
  
  # Create users table
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS users (
                 user_id SERIAL PRIMARY KEY,
                 username TEXT UNIQUE NOT NULL,
                 email TEXT UNIQUE NOT NULL,
                 user_groups INTEGER[] NOT NULL DEFAULT '{1}',
                 password_hash TEXT NOT NULL,
                 password_salt TEXT NOT NULL,
                 algorithm TEXT NOT NULL DEFAULT 'sha256',
                 FOREIGN KEY (group_id) REFERENCES user_groups(group_id)
);")
  
  # owners_contributors table #################
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS owners_contributors (
                 owner_contributor_id SERIAL PRIMARY KEY,
                 name TEXT UNIQUE NOT NULL,
                 contact_name TEXT,
                 phone TEXT,
                 email TEXT,
                 note TEXT
               );")
  
  
  # measurements_continuous table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_continuous (
                 timeseries_id INTEGER NOT NULL,
                 datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                 value NUMERIC NOT NULL,
                 grade TEXT,
                 approval TEXT,
                 period INTERVAL,
                 imputed BOOLEAN NOT NULL DEFAULT FALSE,
                 no_update BOOLEAN NOT NULL DEFAULT FALSE,
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
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

  # measurements_calculated_daily table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_calculated_daily (
                 timeseries_id INTEGER NOT NULL,
                 date DATE NOT NULL,
                 value NUMERIC,
                 grade TEXT,
                 approval TEXT,
                 imputed BOOLEAN NOT NULL DEFAULT FALSE,
                 no_update BOOLEAN NOT NULL DEFAULT FALSE,
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
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 PRIMARY KEY (timeseries_id, date));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements_calculated_daily IS 'Stores calculated daily mean values for timeseries present in table measurements_continuous. Values should not be entered or modified manually but instead are calculated by the AquaCache package function calculate_stats.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_calculated_daily.imputed IS 'TRUE in this column means that at least one of the measurements used for the daily mean calculation was imputed, or, for daily means provided solely in the HYDAT database, that a value was imputed directly to this table.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_calculated_daily.percent_historic_range IS 'The percent of historical range for that measurement compared to all previous records for the same day of year (not including the current measurement). Only populated once a minimum of three values exist for the current day of year (including the current value). February 29 values are the mean of February 28 and March 1. 

For example, a value equal to the maximum historic value is equal to 100% of historical range, while one at the miniumu value is 0%. Values above or below the historical range can have values of less than 0 or greater than 100.

The formula used for the calculation is ((current - min) / (max - min)) * 100'
  ")
  
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_calculated_daily.max IS 'Historical max for the day of year, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_calculated_daily.min IS 'Historical min for the day of year, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_calculated_daily.q50 IS 'Historical 50th quantile or median, excluding current measurement.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_calculated_daily.q25 IS 'Number of measurements existing in the measurements_calculated_daily table for each day including historic and current measurement.'")

  # images table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists images (
                   image_id SERIAL PRIMARY KEY,
                   img_meta_id INTEGER,
                   datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                   latitude NUMERIC,
                   longitude NUMERIC,
                   azimuth_true NUMERIC,
                   altitude_m_agl NUMERIC,
                   altitude_m_asl NUMERIC,
                   fetch_datetime TIMESTAMP WITH TIME ZONE,
                   format TEXT NOT NULL,
                   file BYTEA NOT NULL,
                   description TEXT,
                   share_with INTEGER[] NOT NULL DEFAULT '{1}',
                   owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   UNIQUE (img_meta_id, latitude, longitude, datetime));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.images IS 'Holds images of local conditions specific to each location. Originally designed to hold auto-captured images at WSC locations, but could be used for other location images such as setup documentation. For image series or images necessarily associated with a location, images are linked to the images_index table using the img_meta_id.'")

  # images_index table #################
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS images_index (
                 img_meta_id SERIAL PRIMARY KEY,
                 img_type TEXT NOT NULL CHECK(img_type IN ('auto', 'manual')),
                 first_img TIMESTAMP WITH TIME ZONE,
                 last_img TIMESTAMP WITH TIME ZONE,
                 last_new_img TIMESTAMP WITH TIME ZONE,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 description TEXT,
                 location_id INTEGER NOT NULL REFERENCES locations (location_id) ON DELETE CASCADE ON UPDATE CASCADE,
                 visibility_public TEXT NOT NULL CHECK(visibility_public IN ('exact', 'region', 'jitter')) DEFAULT 'exact',
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 active BOOLEAN,
                 UNIQUE (location_id, img_type));")
  DBI::dbExecute(con, "COMMENT ON TABLE public.images_index IS 'Index for image series in images table. Each location at which there is an image series gets an entry here; images in table images are linked to this table using the img_meta_id.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.images_index.active IS 'Defines if the image series should or should not be imported.'")
  
  
  ## Constraint functions/triggers on image tables ################
  # Ensure that lat/long is populated if img_meta_id is NULL, and that lat/long is NULL if img_meta_id is NOT NULL
  DBI::dbExecute(con, "
               CREATE OR REPLACE FUNCTION enforce_lat_lon_constraints()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- If img_meta_id is NOT NULL, ensure latitude and longitude are NULL
                  IF NEW.img_meta_id IS NOT NULL THEN
                      IF NEW.latitude IS NOT NULL OR NEW.longitude IS NOT NULL THEN
                          RAISE EXCEPTION 'When img_meta_id is NOT NULL, latitude and longitude must be NULL.';
                      END IF;
                  -- If img_meta_id is NULL, ensure latitude and longitude are NOT NULL
                  ELSE
                      IF NEW.latitude IS NULL OR NEW.longitude IS NULL THEN
                          RAISE EXCEPTION 'When img_meta_id is NULL, latitude and longitude must be NOT NULL.';
                      END IF;
                  END IF;
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
               CREATE TRIGGER trigger_enforce_lat_lon_constraints
                BEFORE INSERT OR UPDATE ON images
                FOR EACH ROW
                EXECUTE FUNCTION enforce_lat_lon_constraints();
")
  
  # Make function/trigger on table images that enforces most restrictive share_with for tables images and images_index table. In other words, if images_index is NOT {1}, then images should not be {1}. However, if images_index is {1}, then images can be {2,3,4} or {1}
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION enforce_share_with_restriction()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- Skip check if img_meta_id is NULL
                  IF NEW.img_meta_id IS NULL THEN
                      RETURN NEW;
                  END IF;
                  
                  -- Check if images_index.share_with is NOT {1}
                  IF NOT (1 = ANY(NEW.share_with)) THEN
                      -- Retrieve the corresponding share_with from images_index
                      PERFORM 1
                      FROM images_index
                      WHERE img_meta_id = NEW.img_meta_id
                        AND NOT (1 = ANY(share_with));
                        
                      -- If images_index.share_with is NOT {1}, raise an exception
                      IF FOUND THEN
                          RAISE EXCEPTION 'images_index entry for img_meta_id % has a restrictive share_with, images.share_with cannot be {1}', NEW.img_meta_id;
                      END IF;
                  END IF;
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
               CREATE TRIGGER trigger_enforce_share_with_restriction
              BEFORE INSERT OR UPDATE ON images
              FOR EACH ROW
              EXECUTE FUNCTION enforce_share_with_restriction();
               ")
  

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
  
  
  # Tables to describe discrete samples #################
  # sample_types
  DBI::dbExecute(con, "CREATE TABLE sample_types (
  sample_type_id SERIAL PRIMARY KEY,
  sample_type TEXT UNIQUE NOT NULL,
  sample_type_fr TEXT UNIQUE
);")
  sample_types <- data.frame(sample_type = c("field msr/obs", "QC-field replicate msr/obs", "QC-sample-blind duplicate", "QC-sample-field blank", "QC-sample-field replicate", "QC-sample-field spike", "QC-sample-inter-lab split", "QC-sample-lab blank", "QC-sample-lab duplicate", "QC-sample-lab matrix spike", "QC-sample-lab re-analysis", "QC-sample-lab spike", "QC-sample-lab split", "QC-sample-measurement precision sample", "QC-sample-other", "QC-sample-post-preservative blank", "QC-sample-pre-preservative blank", "QC-sample-reference sample", "QC-sample-trip blank", "QC-negative control", "sample-composite with parents", "sample-composite without parents", "sample-field split", "sample-field subsample", "sample-integrated cross-sectional profile", "sample-integrated flow proportioned", "sample-integrated horizontal profile", "sample-integrated time series", "sample-integrated horizontal and vertical composite profile", "sample-integrated vertical profile", "sample-negative control", "sample-other", "sample-positive control", "sample-routine")
  )
  DBI::dbAppendTable(con, "sample_types", sample_types)
  
  # collection_methods
  DBI::dbExecute(con, "CREATE TABLE collection_methods (
  collection_method_id SERIAL PRIMARY KEY,
  collection_method TEXT UNIQUE NOT NULL
);")
  col_methods <- data.frame(collection_method = c("Observation", "Bucket", "DH-81", "DH-95", "Diffusion Gradient in Thin Film", "Gravity Corer (Generic)", "Kemmerer Bottle", "Nansen Bottle", "Niskin Bottle", "Osterberg Piston Sampler", "pH Paper", "Polar Orga. Chem. Integrative Sampler", "Probe/Sensor", "Pump", "Secchi Disk", "Sediment peeper", "Sediment sub-core", "Sediment Trap", "Semipermeable Membrane Device", "Syringe", "Test strip", "Turbidimeter", "Turbidity Tube", "Van Dorn Bottle", "Variable voltage pulsator unit", "Vinyl Tube", "Water Bottle", "Water Sampler (Other)", "WBH-96", "Whirl-pak bag", "Auger", "Trenching", "Diamond core drill", "RC drill", "Sonic drill")
  )
  DBI::dbAppendTable(con, "collection_methods", col_methods)
  
  # sample_fractions
  DBI::dbExecute(con, "CREATE TABLE sample_fractions (
  sample_fraction_id SERIAL PRIMARY KEY,
  sample_fraction TEXT UNIQUE NOT NULL
);")
  fractions <- data.frame(sample_fraction = c("weak acid dissociable", "strong acid dissociable", "acid Soluble", "bioavailable", "dissolved", "extractable", "filterable", "filtered", "filtered, field", "filtered, lab", "free", "inorganic", "non-settleable", "non-volatile", "organic", "settleable", "supernate", "suspended", "total", "total recoverable", "total Residual", "unfiltered", "vapor", "volatile")
  )
  DBI::dbAppendTable(con, "sample_fractions", fractions)
  
  # result_speciations
  DBI::dbExecute(con, "CREATE TABLE result_speciations (
  result_speciation_id SERIAL PRIMARY KEY,
  result_speciation TEXT UNIQUE NOT NULL
);")
  speciations <- data.frame(result_speciation = c("as B", "as Ca", "as CaCO3", "as Cl", "as CN", "as CO3", "as DPA", "as F", "as H", "as H2S", "as HCO3", "as LAS", "as Mg", "as N", "as Na", "as NaCL", "as NH3", "as NH4", "as NO2", "as NO3", "as O2", "as OH", "as P", "as PO4", "as Ra", "as Ra226", "as S", "as Se", "as Si", "as SiO2", "as SO3", "as SO4", "as U3O8", "of CH4", "of CO2", "of H2O", "of Inorganic C", "of NH4", "of NO3", "of Organic C", "of Organic N", "of PO4", "of SO4", "of S")
  )
  DBI::dbAppendTable(con, "result_speciations", speciations)
  
  # result_value_types
  DBI::dbExecute(con, "CREATE TABLE result_value_types (
  result_value_type_id SERIAL PRIMARY KEY,
  result_value_type TEXT UNIQUE NOT NULL
);")
  value_types <- data.frame(result_value_type = c("Actual", "Blank corrected", "Calculated", "Control adjusted", "Estimated")
  )
  DBI::dbAppendTable(con, "result_value_types", value_types)
  
  # laboratories
  DBI::dbExecute(con, "CREATE TABLE laboratories (
  lab_id SERIAL PRIMARY KEY,
  lab_name TEXT UNIQUE NOT NULL
);")
  labs <- data.frame(lab_name = c("Saskatchewan Research Council", "ALS Environmental", "Bodycote Testing Group", "Chemex Lab", "Cantest", "CARO Analytical Services", "Envirotest", "Exova", "Maxxam", "Norwest Labs", "Pacific Environmental Science Centre", "United Keno Mine Lab", "Axys Analytical Services", "AGAT Laboratories", "Water Resources Lab", "Nautilus Environmental", "University of Calgary", "Environment Canada", "Element", "Bureau Veritas Labs (formally Maxxam)")
  )
  DBI::dbAppendTable(con, "laboratories", labs)
  
  # protocols
  DBI::dbExecute(con, "CREATE TABLE analysis_protocols (
  protocol_id SERIAL PRIMARY KEY,
  protocol_name TEXT UNIQUE NOT NULL,
  protocol_description TEXT,
  url TEXT
);")
  ss_protocol <- data.frame(protocol_name = "BC Snow Survey Sampling Guide", url = "https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww2.gov.bc.ca%2Fassets%2Fgov%2Fenvironment%2Fair-land-water%2Fwater%2Fsnow%2Fsnow_survey_sampling_guide.pdf&psig=AOvVaw3HMaMRejyngEj0syvm3l88&ust=1723232510800000&source=images&cd=vfe&opi=89978449&ved=0CAQQn5wMahcKEwjwkOe-k-aHAxUAAAAAHQAAAAAQBA")
  DBI::dbAppendTable(con, "analysis_protocols", ss_protocol)
  
  # result_conditions
  DBI::dbExecute(con, "CREATE TABLE result_conditions (
  result_condition_id SERIAL PRIMARY KEY,
  result_condition TEXT UNIQUE NOT NULL
);")
  conditions <- data.frame(result_condition = c("Below Detection/Quantification Limit", "Above Detection/Quantification Limit", "Detected Not Quantified", "Not Detected", "Not Reported"))
  DBI::dbAppendTable(con, "result_conditions", conditions)
  

  # measurements_discrete table #################
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements_discrete (
                   timeseries_id INTEGER,
                   target_datetime TIMESTAMP WITH TIME ZONE,
                   datetime TIMESTAMP WITH TIME ZONE,
                   value NUMERIC,
                   result_value_type INTEGER NOT NULL REFERENCES result_value_types(result_value_type_id) ON DELETE CASCADE ON UPDATE CASCADE,
                   result_condition INTEGER REFERENCES result_conditions(result_condition_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   result_condition_value NUMERIC,
                   sample_type INTEGER NOT NULL REFERENCES sample_types(sample_type_id) ON DELETE CASCADE ON UPDATE CASCADE,
                   collect_method INTEGER REFERENCES collection_methods(collection_method_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   sample_fraction INTEGER REFERENCES sample_fractions(sample_fraction_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   result_speciation INTEGER REFERENCES result_speciations(result_speciation_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   lab INTEGER REFERENCES laboratories(lab_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   protocol INTEGER REFERENCES analysis_protocols(protocol_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   note TEXT,
                   no_update BOOLEAN NOT NULL DEFAULT FALSE,
                   share_with INTEGER[] NOT NULL DEFAULT '{1}',
                   owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                   contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE
                   UNIQUE NULLS NOT DISTINCT(timeseries_id, datetime, sample_type, collection_method, sample_fraction, result_speciation, result_value_type)
                 );")
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements_discrete IS 'Holds discrete observations, such as snow survey results, laboratory analyses, etc.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.target_datetime IS 'Optional column to be used for things like snow surveys where the measurements are around a certain target date and need to be plotted with the target date rather than the actual sample date.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements_discrete.datetime IS 'The datetime on which the measurement was taken, or on which the sample was acquired in the field.'
  ")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.value IS 'The value of the measurement if it is a number. For cases of < DL, > DL, etc. refer to column result_condition.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.result_value_type IS 'The type of result, such as actual, blank corrected, etc.';")
  # If result_condition is not null, value must be null
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD CONSTRAINT chk_result_condition CHECK (result_condition IS NULL OR value IS NULL);")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.result_condition IS 'The condition of the result, such as below detection limit, above detection limit, etc. If this column is populated then column value must be NULL';")
  # Add constraint that result_condition_value must be a number if result_condition is < DL or > DL
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD CONSTRAINT chk_result_condition_value CHECK (result_condition_value IS NULL OR result_condition IN (1, 2));")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.result_condition_value IS 'The value of the result condition, such as the detection limit, if the result condition is < DL or > DL.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.sample_type IS 'The type of sample taken, such as field measurement/observation, quality control, etc.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.collection_method IS 'The method used to collect the sample, such as bucket, pump, etc. Can be NULL.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.result_speciation IS 'The speciation of the result, such as as CaCO3, as P, as N, etc.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.sample_fraction IS 'The fraction of the sample, such as filtered, total, etc.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.lab IS 'The laboratory that performed the analysis. Can be NULL for field measurements or if the lab is not known.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN measurements_discrete.protocol IS 'The protocol used to analyze the sample. Can be NULL.';")

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
                 "ALTER TABLE measurements_calculated_daily
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
BEFORE INSERT OR UPDATE OF grade ON measurements_calculated_daily
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
                 "ALTER TABLE measurements_calculated_daily
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
BEFORE INSERT OR UPDATE OF approval ON measurements_calculated_daily
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

  # locations tables #################
  DBI::dbExecute(con, "CREATE TABLE location_types (
                 type_id SERIAL PRIMARY KEY,
                 type TEXT UNIQUE NOT NULL,
                 type_fr TEXT);")
  
  # Initial location types table
  loc_types <- data.frame(type = c("river/stream", "lake/pond", "estuary", "wetland", "ocean", "stormwater storage", "sewer sanitary", "sewer storm", "reservoir", "canal/ditch drainage", "canal/ditch irrigation", "well", "spring/seep", "atmosphere", "meteorological station", "snowpack"),
                          type_fr = c("rivi\u00E8re/cours d'eau", "lac/\u00E9tang", "estuaire", "zone humide", "oc\u00E9an", "r\u00E9servoir d'eau pluviale", "\u00E9gout sanitaire", "\u00E9gout pluvial", "r\u00E9servoir", "canal/drainage", "canal/irrigation", "puit", "source/ruissellement", "atmosph\u00E8re", "station m\u00E9t\u00E9orologique", "accumulation de neige"))
  DBI::dbAppendTable(con, "location_types", loc_types)
  
  DBI::dbExecute(con, "CREATE TABLE if not exists locations (
                 location_id SERIAL PRIMARY KEY,
                 location TEXT UNIQUE NOT NULL,
                 location_type INTEGER NOT NULL REFERENCES location_types (type_id),
                 name TEXT UNIQUE NOT NULL,
                 name_fr UNIQUE TEXT,
                 latitude NUMERIC NOT NULL,
                 longitude NUMERIC NOT NULL
                 contact TEXT,
                 install_purpose TEXT,
                 current_purpose TEXT,
                 geom_id INTEGER NOT NULL,
                 location_images INTEGER[],
                 note TEXT,
                 visibility_public TEXT NOT NULL CHECK(visibility_public IN ('exact', 'region', 'jitter')) DEFAULT 'exact',
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 );")
  # function/trigger to make sure that array elements of location_images are all valid image_ids in table 'images'
  DBI::dbExecute(con, "
                      CREATE OR REPLACE FUNCTION check_location_images()
                      RETURNS TRIGGER AS $$
                      BEGIN
                          IF NEW.location_images IS NOT NULL THEN
                              -- Loop through each element in the location_images array
                              FOR i IN 1..array_length(NEW.location_images, 1) LOOP
                                  -- Check if the image_id exists in the images table
                                  PERFORM 1
                                  FROM images
                                  WHERE image_id = NEW.location_images[i];
                                  
                                  -- If image_id is not found, raise an exception
                                  IF NOT FOUND THEN
                                      RAISE EXCEPTION 'Invalid image_id: %, for location_id: %', NEW.location_images[i], NEW.location_id;
                                  END IF;
                              END LOOP;
                          END IF;
                          RETURN NEW;
                      END;
                      $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
                 CREATE TRIGGER trigger_check_location_images
                BEFORE INSERT OR UPDATE ON locations
                FOR EACH ROW
                EXECUTE FUNCTION check_location_images();
")
  
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.visibility IS 'Visibility of the location on the map. Exact means the location is to be shown exactly where it is. Region means the location is to be shown generally with a region. Jitter means the location is shown at a random location within a toroid of inner/outer radius 2, 5 km around the exact point location. This column does not apply if column public is FALSE.'")
  
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.share_with IS 'The user group which is allowed to see this location and any data linked to it. NULL means the location is public.'")
  
  DBI::dbExecute(con, "COMMENT ON TABLE public.locations IS 'Holds location information, including the location name, latitude, longitude, and contact information. The geom_id is a reference to the vectors table, which holds the geometry for the location. Visibility is a flag to indicate how the location should be displayed on the map, if it is shared. Share_with is an array of user groups that are allowed to see the location and any data linked to it.'")
  
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
                 description_fr TEXT,
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE
                 );")
  
  doc_types <- data.frame(document_type_en = c("thesis", "data sharing agreement", "report", "well log", "conference paper","poster",
                          "journal article", "map", "graph", "protocol", "metadata", "audit"),
                          document_type_fr = c("th\u00E8se", "accord de partage de donn\u00E9es", "rapport", "journal de puits", "article de conf\u00E9rence", "affiche", "article de journal", "carte", "graphique", "protocole", "m\u00E9tadonn\u00E9es", "audit"))
  DBI::dbAppendTable(con, "document_types", doc_types)
  
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
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 FOREIGN KEY (type) REFERENCES document_types(document_type_id) ON UPDATE CASCADE ON DELETE CASCADE);")
  DBI::dbExecute(con, "COMMENT ON TABLE public.documents IS 'Holds documents and metadata associated with each document. Each document can be associated with one or more location, line, or polygon, or all three.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.documents.has_points IS 'Flag to indicate that the document_spatial has a point entry for this document.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.documents.authors IS 'An *array* of one or more authors.'")
  
  
  # Add data_sharing_agreement_id to locations and enforce checks
  DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN data_sharing_agreement_id INTEGER REFERENCES documents(document_id)")
  
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.data_sharing_agreement_id IS 'An optional link to a data sharing agreement from the documents table. A check is enforced to ensure that this column only references documents of type ''data sharing agreement''.'")
  
  # Add constraint to documents.data_sharing_agreement_id
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION check_data_sharing_agreement() 
RETURNS TRIGGER AS $$
  BEGIN
IF NEW.data_sharing_agreement_id IS NOT NULL THEN
-- Check if the referenced document is of the correct type
IF NOT EXISTS (
  SELECT 1
  FROM documents d
  JOIN document_types dt ON d.type = dt.document_type_id
  WHERE d.document_id = NEW.data_sharing_agreement_id
  AND dt.document_type_en = 'data sharing agreement'
) THEN
RAISE EXCEPTION 'Invalid document type: data_sharing_agreement must reference a document of type ''data sharing agreement''';
END IF;
END IF;
RETURN NEW;
END;
$$ LANGUAGE plpgsql;")
  
  DBI::dbExecute(con, "CREATE TRIGGER trg_check_data_sharing_agreement
BEFORE INSERT OR UPDATE ON locations
FOR EACH ROW
EXECUTE FUNCTION check_data_sharing_agreement();")


  # timeseries table #################
  DBI::dbExecute(con, "
  CREATE TABLE if not exists timeseries (
                 timeseries_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 parameter_id INTEGER NOT NULL,
                 media_id INTEGER NOT NULL,
                 category TEXT NOT NULL CHECK(category IN ('discrete', 'continuous')),
                 period_type TEXT NOT NULL CHECK(period_type IN ('instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2')),
                 record_rate TEXT NOT NULL,
                 z NUMERIC,
                 sensor_priority INTEGER,  -- this permits primary, secondary, tertiary, etc. sensors to be defined for a location/parameter/media combination
                 start_datetime TIMESTAMP WITH TIME ZONE,
                 end_datetime TIMESTAMP WITH TIME ZONE,
                 last_new_data TIMESTAMP WITH TIME ZONE,
                 last_daily_calculation TIMESTAMP WITH TIME ZONE,
                 last_synchronize TIMESTAMP WITH TIME ZONE,
                 network TEXT,
                 source_fx TEXT,
                 source_fx_args TEXT,
                 active BOOLEAN NOT NULL DEFAULT TRUE,
                 note TEXT,
                 share_with INTEGER[] NOT NULL DEFAULT '{1}',
                 owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
                 UNIQUE NULLS NOT DISTINCT (location, parameter_id, category, period_type, media_id, record_rate, z, sensor_priority),
                 CONSTRAINT check_record_rate_constraints
                     CHECK (
                     (category = 'discrete' AND record_rate IS NULL) OR
                     (category = 'continuous' AND record_rate IN ('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))
                     )
                 );
                 ")
  
  ## Add table and column comments #################
  DBI::dbExecute(con, "
  COMMENT ON TABLE public.timeseries IS 'Provides a record of every timeseries in the database. Each timeseries is unique by its combination of location, parameter, media_id, category (continuous or discrete), period_type, record_rate, and z (elevation).Continuous data is data gathered at regular and usually frequent intervals, while discrete data includes infrequent, often manual measurements of values such as snow depth or dissolved element parameters.';
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
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.source_fx IS 'Function (from the R package AquaCache) to use for incorporation of new data.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.timeseries.source_fx_args IS 'Optional arguments to pass to the source function. See notes in function addACTimeseries for usage.';")
  
  
  # parameter related tables #################
  DBI::dbExecute(con, "
CREATE TABLE parameter_groups (
    group_id SERIAL PRIMARY KEY,
    group_name TEXT NOT NULL UNIQUE,
    group_name_fr TEXT,
    description TEXT,
    description_fr TEXT
);")
  
  # populate the parameter_groups table
  grps <- data.frame(group_name = c("organics", "inorganics", "nutrients", "physical", "biological", "stable isotopes", "radiochemical"),
                     group_name_fr = c("organiques", "inorganiques", "nutriments", "physiques", "biologiques", "isotopes stables", "radiochimique"),
                     description = c("Organic (carbon-containing) parameters such as pesticides", "Inorganic parameters", "Nutrient parameters", "Physical parameters such as water level, temperature", "Biological parameters such as algae, bacteria", "Isotopes which do not undergoe radioactive decay", "Isotopes which undergo radioactive decay, radiation parameters"),
                     description_fr = c("Param\u00E8tres organiques (contenant du carbone) tels que les pesticides", "Param\u00E8tres inorganiques", "Param\u00E8tres nutritifs", "Param\u00E8tres physiques tels que le niveau d'eau, la temp\u00E9rature", "Param\u00E8tres biologiques tels que les algues, les bact\u00E9ries", "Isotopes qui ne subissent pas de d\u00E9sint\u00E8gration radioactive", "Isotopes qui subissent une d\u00E9sint\u00E8gration radioactive, param\u00E8tres de radiation"))
  
  DBI::dbAppendTable(con, "parameter_groups", grps)
  
  
  # Create parameter_sub_groups table
  DBI::dbExecute(con, "
CREATE TABLE parameter_sub_groups (
    sub_group_id SERIAL PRIMARY KEY,
    sub_group_name TEXT NOT NULL,
    sub_group_name_fr TEXT,
    description TEXT,
    description_fr TEXT,
    UNIQUE(sub_group_name),
    unique(sub_group_name_fr)
);")
  
  sub_grps <- data.frame( sub_group_name = c("pesticides/insecticides/fungicides", "pharmaceuticals", "cyano/phytotoxins", "PFAS", "PCBs", "PAHs", "BDEs", "hydrocarbons - others", "metals", "non-metals", "anions", "cations", "nitrogen", "phosphorus", "toxicity", "microbiological", "photosynthetic pigments", "other"),
                          sub_group_name_fr = c("pesticides/insecticides/fongicides", "m\u00E9dicaments", "cyano/phytotoxines", "PFAS", "PCB", "HPA", NA, "hydrocarbures - autres", "m\u00E9taux", "non-m\u00E9taliques", "anions", "cations", "azote", "phosphore", "toxicit\u00E9", "microbiologique", "pigments photosynth\u00E9tiques", "autre"),
                          description = c("Pesticides, insecticides, and fungicides", "Pharmaceutical compounds", "Cyanotoxins and phytotoxins", "Per- and polyfluoroalkyl substances", "Brominated diphenyl ethers", "Polychlorinated biphenyls", "Polycyclic aromatic hydrocarbons", "hydrocarbons, others", "Metallic elements", "Non-metallic elements", "Anions", "Cations", "Nitrogen compounds", "Phosphorus compounds", "Toxicity parameters such as LC50", "Microbiological parameters such as bacterial counts, algae concentration", NA, NA),
                          description_fr = c("Pesticides, insecticides et fongicides", "Compos\u00E9s pharmaceutiques", "Cyano-toxines et phytotoxines", "Substances per- et polyfluoroalkyles", NA, "Biph\u00E9nyles polychlor\u00E9s", "Hydrocarbures aromatiques polycycliques", "hydrocarbures, autres", "El\u00E9ments m\u00E9talliques", "El\u00E9ments non-m\u00E9taliques", "Anions", "Cations","Compos\u00E9s azot\u00E9s", "Compos\u00E9s phosphor\u00E9s", "Param\u00E8tres de toxicit\u00E9 tels que LC50", "Param\u00E8tres microbiologiques tels que les comptes bact\u00E9riens, la concentration en algues", NA, NA))
  
  DBI::dbAppendTable(con, "parameter_sub_groups", sub_grps)

  # Create parameters and parameter_relationships tables
  DBI::dbExecute(con, "CREATE TABLE parameters (
               parameter_id SERIAL PRIMARY KEY,
               param_name TEXT UNIQUE NOT NULL,
               param_name_fr TEXT UNIQUE,
               description TEXT,
               description_fr TEXT,
               unit_default TEXT NOT NULL,
               unit_solid TEXT,
               cas_number TEXT,
               result_speciation BOOLEAN NOT NULL,
               sample_fraction BOOLEAN NOT NULL,
               plot_default_y_orientation TEXT NOT NULL CHECK(plot_default_y_orientation IN ('normal', 'inverted')),
               plot_default_floor NUMERIC,
               plot_default_ceiling NUMERIC);")
  DBI::dbExecute(con, "COMMENT ON TABLE public.parameters IS 'Holds information about each parameter, including the parameter name, description, unit, and default plotting orientation, ceiling, and floor to facilitate plotting. Parameters are associated with groups and sub-groups via the table parameter_relationships.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN parameters.unit_default IS 'SI units only. For example, m3/s, mg/L, etc. Import functions should convert to SI units listed here before appending to the DB.';")
  DBI::dbExecute(con, "COMMENT ON COLUMN parameters.result_speciation IS 'Controls if results using this parameter require an entry to column result_speciation of table measurements_discrete';")
  DBI::dbExecute(con, "COMMENT ON COLUMN parameters.sample_fraction IS 'Controls if results using this parameter require an entry to column sample_fraction of table measurements_discrete';")
  
  # Create parameter_relationships table with unique constraint on the combination of parameter_id, group_id, and sub_group_id
  # The unique key ensures that a parameter can be in more than one group and sub-group. For example, the sub-group 'pesticides' can be in the group 'organic' and 'inorganic'.
  DBI::dbExecute(con, "
CREATE TABLE parameter_relationships (
    relationship_id SERIAL PRIMARY KEY,
    parameter_id INTEGER NOT NULL,
    group_id INTEGER NOT NULL,
    sub_group_id INTEGER,
    FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id),
    FOREIGN KEY (group_id) REFERENCES parameter_groups(group_id),
    FOREIGN KEY (sub_group_id) REFERENCES parameter_sub_groups(sub_group_id),
    UNIQUE NULLS NOT DISTINCT(parameter_id, group_id, sub_group_id)
);")
  

  # Load the parameters, groups, sub-groups table from the inst folder or the package root if installed
  params <- utils::read.csv(system.file("extdata/parameters.csv", package = "AquaCache"))
  params$param_name <- tolower(params$param_name)  # Make sure all parameter names are lowercase, YGwater package has function to ensure proper capitalization based on selected language
  params <- params[, -which(names(params) == "DataStream_group")]
  names(params) <- c("param_name", "result_speciation", "sample_fraction", "cas_number", "group", "subgroup", "unit_default", "unit_solid")
  params$plot_default_y_orientation <- "normal"
  # Change Yes/No values to TRUE/FALSE
  params[params$result_speciation == "Yes", "result_speciation"] <- TRUE
  params[params$result_speciation == "No", "result_speciation"] <- FALSE
  params[params$sample_fraction == "Yes", "sample_fraction"] <- TRUE
  params[params$sample_fraction == "No", "sample_fraction"] <- FALSE
  params$sample_fraction <- as.logical(params$sample_fraction)
  params$result_speciation <- as.logical(params$result_speciation)
  
  DBI::dbWriteTable(con, "parameters", params[, c("param_name", "result_speciation", "sample_fraction", "cas_number", "unit_default", "plot_default_y_orientation")], append = TRUE)
  
  # Establish relationships between parameters and groups/sub-groups using columns group and subgroup of params, put these into the parameter_relationships table
  # First get the parameter, group and sub-group ids
  subgroups <- DBI::dbGetQuery(con, "SELECT sub_group_id, sub_group_name FROM parameter_sub_groups")
  groups <- DBI::dbGetQuery(con, "SELECT group_id, group_name FROM parameter_groups")
  parameters <- DBI::dbGetQuery(con, "SELECT parameter_id, param_name FROM parameters")
  parameters <- merge(params[, c("param_name", "group", "subgroup")], parameters, by = "param_name", all.x = TRUE)
  parameters <- merge(parameters, groups, by.x = "group", by.y = "group_name", all.x = TRUE)
  parameters <- merge(parameters, subgroups[, c("sub_group_id", "sub_group_name")], by.x = "subgroup", by.y = "sub_group_name", all.x = TRUE)
  
  DBI::dbAppendTable(con, "parameter_relationships", parameters[, c("parameter_id", "group_id", "sub_group_id")], append = TRUE)
  
  
  # media_types table #################
  DBI::dbExecute(con, "CREATE TABLE media_types (
               media_id SERIAL PRIMARY KEY,
               media_type TEXT UNIQUE NOT NULL,
               media_type_fr TEXT UNIQUE NOT NULL,
               description TEXT,
               description_fr TEXT);")
  
  medias <- data.frame(media_type = c("surface water", "ground water", "waste water", "waste water effluent", "seep", "drinking water", "sediment, sub-surface", "sediment, surface water", "rain water", "ocean water", "atmospheric"),
                       media_type_fr = c("eau de surface", "eau souterraine", "eau us\u00E9e", "effluent d'eau us\u00E9e", "exsurgence", "eau potable", "s\u00E9diment, sol", "s\u00E9diment, eau de surface", "eau de pluie", "eau de mer", "atmosph\u00E9rique"),
                       description = c("Surface water", "Water extracted from the ground.", "Water samples prior to or during waste water treatment.", "Water downstream of waste water treatment facilities", "Water that comes out of the ground by without pumping.", "Water used for drinking purposes, pre or post treatment.", "Sediment from a soil sample outside of an aquatic environment.", "Sediment from a surface water environment", NA, NA, "Temperature, humidity, wind speed, precipitation rate and accumulation, etc."),
                       description_fr = c("Eau de surface", "Eau extraite du sol.", "\u00C9chantillons d'eau avant ou pendant le traitement des eaux us\u00E9es.", "Eau en aval des installations de traitement des eaux us\u00E9es", "Eau qui sort du sol sans pompage.", "Eau utilis\u00E9e \u00E0 des fins de consommation, avant ou apr\u00E8s traitement.", "S\u00E9diment provenant d'un \u00E9chantillon de sol hors d'un environment aquatique.", "S\u00E9diment provenant d'un environment aquatique en surface.", NA, NA, "Temp\u00E9rature, humidit\u00E9, vitesse du vent, taux et accumulation de pr\u00E9cipitations, etc.")
  )
  DBI::dbAppendTable(con, "media_types", medias)


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
                 parameter_id INTEGER NOT NULL,
                 period_type TEXT NOT NULL,
                 record_rate TEXT,
                 remote_param_name TEXT NOT NULL,
                 UNIQUE (source_fx, parameter_id, period_type, record_rate))")
  DBI::dbExecute(con, "COMMENT ON TABLE public.settings IS 'This table stores the name of functions used to pull in new data, the parameter with which they are associated, and the remote parameter name to pass to the source function for each function and database parameter name.'
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.source_fx IS 'The R function (from the AquaCache package) to use for fetching data.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.parameter_id IS 'Parameter integer codes used in the timeseries table.';
  ")
  DBI::dbExecute(con, "
  COMMENT ON COLUMN public.settings.remote_param_name IS 'The parameter name or code to pass to the parameter parameter_id of the R function specified in source_fx.';
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
                 ADD CONSTRAINT fk_parameter_id
                 FOREIGN KEY (parameter_id)
                 REFERENCES parameters(parameter_id)
                 ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con,
                 "ALTER TABLE locations
  ADD CONSTRAINT fk_geom_id
  FOREIGN KEY (geom_id)
  REFERENCES vectors(geom_id)
                 ON UPDATE CASCADE ON DELETE CASCADE;")
  DBI::dbExecute(con,
                 "ALTER TABLE locations
  ADD CONSTRAINT fk_location_type
  FOREIGN KEY (location_type)
  REFERENCES location_types(type_id)
                 ON UPDATE CASCADE;")
  
  DBI::dbExecute(con,
                 "ALTER TABLE timeseries
  ADD CONSTRAINT fk_location_id
  FOREIGN KEY (location_id)
  REFERENCES locations(location_id)
                 ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con, "ALTER TABLE timeseries ADD CONSTRAINT fk_parameter_id FOREIGN KEY (parameter_id) REFERENCES parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE;")
  
  DBI::dbExecute(con, "ALTER TABLE timeseries ADD CONSTRAINT fk_media_type FOREIGN KEY (media_id) REFERENCES media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE;")
  
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
                 "ALTER TABLE measurements_calculated_daily
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
   
  ## Views for timeseries metadata ########################################
  DBI::dbExecute(con, paste("CREATE OR REPLACE VIEW public.timeseries_metadata_en WITH (security_invoker = TRUE) AS",
                            "SELECT ts.timeseries_id, mtypes.media_type AS media_type, ts.category, params.param_name AS parameter_name, params.unit_default AS default_units, params.unit_solid AS units_solid_medium, ts.period_type, ts.record_rate AS recording_rate, ts.start_datetime, ts.end_datetime, ts.note, loc.location_id, loc.location AS location_code, loc.name AS location_name, loc.latitude, loc.longitude",
                            "FROM timeseries AS ts ",
                            "JOIN locations AS loc ON ts.location_id = loc.location_id ",
                            "LEFT JOIN parameters AS params ON ts.parameter_id = params.parameter_id",
                            "LEFT JOIN media_types AS mtypes ON ts.media_id = mtypes.media_id",
                            "ORDER BY ts.timeseries_id;"))

  DBI::dbExecute(con, paste("CREATE OR REPLACE VIEW public.timeseries_metadata_fr WITH (security_invoker = TRUE) AS",
                            "SELECT ts.timeseries_id, mtypes.media_type_fr AS type_de_m\u00E9dia, ts.category AS cat\u00E9gorie, params.param_name_fr AS nom_param\u00E8tre, params.unit_default AS unit\u00E9s_par_d\u00E9faut, params.unit_solid AS unit\u00E9s_media_solide, ts.period_type, ts.record_rate AS recording_rate, ts.start_datetime AS d\u00E9but, ts.end_datetime AS fin, ts.note, loc.location_id, loc.location AS location_code, loc.name_fr AS nom_endroit, loc.latitude, loc.longitude",
                            "FROM timeseries AS ts ",
                            "JOIN locations AS loc ON ts.location_id = loc.location_id ",
                            "LEFT JOIN parameters AS params ON ts.parameter_id = params.parameter_id",
                            "LEFT JOIN media_types AS mtypes ON ts.media_id = mtypes.media_id",
                            "ORDER BY ts.timeseries_id;"))
  
  ## View for location metadata ########################################
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW location_metadata_en WITH (security_invoker = TRUE) AS
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
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW location_metadata_fr WITH (security_invoker = TRUE) AS
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
    array_agg(DISTINCT net.name_fr) AS r\u00E9seaux
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

  
  # Add tables, functions, and view tables to apply corrections. ################
  
  DBI::dbExecute(con, "CREATE TABLE correction_types (
  correction_type_id SERIAL PRIMARY KEY,
  correction_type TEXT NOT NULL,
  description TEXT NOT NULL,
  priority INTEGER NOT NULL,
  value1 BOOLEAN DEFAULT FALSE,
  value1_description TEXT,
  value2 BOOLEAN DEFAULT FALSE,
  value2_description TEXT,
  timestep_window BOOLEAN DEFAULT FALSE,
  equation BOOLEAN NOT NULL DEFAULT FALSE,
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (priority)
)")
  
  correction_types <- data.frame(
    correction_type = c("trim",
                        "delete",
                        "drift linear",
                        "drift equation",
                        "offset linear",
                        "scale"),
    description = c("Remove data points outside of a specified value range",
                    "Remove data points within a specified datetime range",
                    "Apply a linear drift correction",
                    "Apply a drift correction based on an equation",
                    "Apply a linear offset correction",
                    "Apply a percent scaling correction"),
    priority = c(2, 1, 4, 5, 3, 6),
    value1 = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    value1_description = c("Value below which to discard data", 
                           NA, 
                           "Drift value (depends on timestep_window)", 
                           "Equation for drift (depends on timestep_window)", 
                           "Shift for linear offset",
                           "Scaling factor (%) to apply to data"),
    value2 = c(NA, FALSE, FALSE, FALSE, FALSE, FALSE),
    value2_description = c("Value above which to discard data", 
                           NA, 
                           NA, 
                           NA, 
                           NA, 
                           NA),
    timestep_window = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    equation = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  DBI::dbAppendTable(con, "correction_types", correction_types)
  DBI::dbExecute(con, "COMMENT ON TABLE correction_types IS 'Table to hold correction types and information for their use in the corrections table'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.correction_types.priority IS 'Defines the order in which corrections are applied. Lower numbers are applied first.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.correction_types.value1 IS 'Whether or not value1 is required for this correction type. NULL if optional'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.correction_types.value2 IS 'Whether or not value2 is required for this correction type. NULL if optional'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.correction_types.timestep_window IS 'Whether or not timestep_window is required for this correction type. NULL if optional'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.correction_types.equation IS 'Whether or not equation is required for this correction type. NULL if optional'")

  DBI::dbExecute(con, "CREATE TABLE corrections (
  correction_id SERIAL PRIMARY KEY,
  timeseries_id INTEGER NOT NULL REFERENCES timeseries (timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE,
  start_dt TIMESTAMP WITH TIME ZONE NOT NULL,
  end_dt TIMESTAMP WITH TIME ZONE NOT NULL,
  correction_type INTEGER NOT NULL REFERENCES correction_types(correction_type_id) ON UPDATE CASCADE ON DELETE CASCADE,
  value1 NUMERIC, -- only optional for 'delete' correction type
  value2 NUMERIC,  -- Optional second value for two-point offset correction
  timestep_window INTERVAL,  -- Applies to rolling average smoothing (window) as well as drift corrections (time step)
  equation TEXT,  -- Equation for drift correction
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  created_by TEXT NOT NULL,
  modified_by TEXT,
  UNIQUE (timeseries_id, start_dt, end_dt, correction_type)
)")
  
  DBI::dbExecute(con, "COMMENT ON TABLE corrections IS 'Table to hold corrections for timeseries data. Each correction applies to a timeseries for a specified datetime range; overlapping corrections are applied in order of priority as defined in table correction_types.'")
  
  
  # Make function and trigger to ensure value1, value2, timestep_window are appropriately populated based on correction_type
  DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION validate_corrections()
RETURNS TRIGGER AS $$
DECLARE
  v1_required BOOLEAN;
  v2_required BOOLEAN;
  timestep_required BOOLEAN;
  equation_required BOOLEAN;
BEGIN
  -- Fetch the correction type definition from correction_types
  SELECT value1, value2, timestep_window, equation
  INTO v1_required, v2_required, timestep_required, equation_required
  FROM correction_types
  WHERE correction_type_id = NEW.correction_type;

  -- Check value1
  IF v1_required AND NEW.value1 IS NULL THEN
    RAISE EXCEPTION 'value1 cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT v1_required AND NEW.value1 IS NOT NULL THEN
    RAISE EXCEPTION 'value1 must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  -- Check value2
  IF v2_required AND NEW.value2 IS NULL THEN
    RAISE EXCEPTION 'value2 cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT v2_required AND NEW.value2 IS NOT NULL THEN
    RAISE EXCEPTION 'value2 must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  -- Check timestep_window
  IF timestep_required AND NEW.timestep_window IS NULL THEN
    RAISE EXCEPTION 'timestep_window cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT timestep_required AND NEW.timestep_window IS NOT NULL THEN
    RAISE EXCEPTION 'timestep_window must be NULL for correction_type_id %', NEW.correction_type;
  END IF;
  
  -- Check equation
  IF equation_required AND NEW.equation IS NULL THEN
    RAISE EXCEPTION 'equation cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT equation_required AND NEW.equation IS NOT NULL THEN
    RAISE EXCEPTION 'equation must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_corrections_trigger
BEFORE INSERT OR UPDATE ON corrections
FOR EACH ROW
EXECUTE FUNCTION validate_corrections();
")
  
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION apply_corrections(
  p_timeseries_id INTEGER,
  p_datetime TIMESTAMP WITH TIME ZONE,
  p_value NUMERIC
) RETURNS NUMERIC AS $$
DECLARE
  corrected_value NUMERIC := p_value;
  correction_row RECORD;
  time_since_start NUMERIC;
  time_window NUMERIC;
  rate NUMERIC;
  correction NUMERIC;
BEGIN
  IF p_value IS NULL THEN
    RETURN NULL;
    END IF;
    
  FOR correction_row IN
    SELECT
      c.value1 AS c_value1,
      c.value2 AS c_value2,
      c.timestep_window AS c_timestep_window,
      c.equation AS c_equation,
      c.start_dt,
      c.end_dt,
      ct.correction_type,
      ct.priority
    FROM corrections c
    JOIN correction_types ct ON c.correction_type = ct.correction_type_id
    WHERE c.timeseries_id = p_timeseries_id
      AND c.start_dt <= p_datetime
      AND c.end_dt >= p_datetime
    ORDER BY ct.priority ASC
  LOOP
    -- Apply correction based on correction_row.correction_type
    IF correction_row.correction_type = 'delete' THEN
      -- Remove the data point
      RETURN NULL;
      
    ELSIF correction_row.correction_type = 'trim' THEN
      -- Remove data points outside of a specified value range
      IF correction_row.c_value1 IS NOT NULL AND corrected_value < correction_row.c_value1 THEN
        RETURN NULL;
      ELSIF correction_row.c_value2 IS NOT NULL AND corrected_value > correction_row.c_value2 THEN
        RETURN NULL;
      END IF;
      
    ELSIF correction_row.correction_type = 'offset linear' THEN
      -- Apply linear offset
      corrected_value := corrected_value + correction_row.c_value1;
      
    ELSIF correction_row.correction_type = 'offset two-point' THEN
      -- Apply two-point offset correction
      RAISE EXCEPTION 'Offset two-point correction not implemented yet';
      
    ELSIF correction_row.correction_type = 'scale' THEN
      -- Apply percent scaling
      corrected_value := corrected_value * (correction_row.c_value1 / 100.0);
      
    ELSIF correction_row.correction_type = 'drift linear' THEN
      -- Apply linear drift correction that continues until end_dt
      IF p_datetime < correction_row.start_dt OR p_datetime > correction_row.end_dt THEN
        -- No correction applied outside the correction period
        correction := 0;
      ELSE
        -- During the correction period; apply proportional correction
        time_since_start := EXTRACT(EPOCH FROM (p_datetime - correction_row.start_dt));
        time_window := EXTRACT(EPOCH FROM correction_row.c_timestep_window);
        rate := correction_row.c_value1 / time_window;  -- Correction per second
        correction := rate * time_since_start;
      END IF;
      corrected_value := corrected_value + correction;
      
    ELSIF correction_row.correction_type = 'drift equation' THEN
      -- Apply drift correction based on an equation
      RAISE EXCEPTION 'Drift equation correction not implemented yet';
    ELSE
      RAISE NOTICE 'Correction type % not handled', correction_row.correction_type;
    END IF;
  END LOOP;

  RETURN corrected_value;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "
CREATE OR REPLACE VIEW measurements_continuous_corrected AS
SELECT
mc.timeseries_id,
mc.datetime,
mc.value AS value_raw,
ac.value_corrected,
mc.grade,
mc.approval,
mc.period,
mc.imputed,
mc.owner,
mc.contributor
FROM
measurements_continuous mc
CROSS JOIN LATERAL (
  SELECT apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
) ac
WHERE
ac.value_corrected IS NOT NULL;
")
  
  DBI::dbExecute(con, "
CREATE OR REPLACE VIEW measurements_discrete_corrected AS
SELECT
  md.timeseries_id,
  md.target_datetime,
  md.datetime,
  md.value AS value_raw,
  ac.corrected_value AS value_corrected,
  -- List other columns from measurements_discrete
  md.result_value_type,
  md.result_condition,
  md.result_condition_value,
  md.sample_type,
  md.collection_method,
  md.sample_fraction,
  md.result_speciation,
  md.lab,
  md.protocol,
  md.note,
  md.owner,
  md.contributor
FROM
  measurements_discrete md
LEFT JOIN LATERAL (
  SELECT apply_corrections(md.timeseries_id, md.datetime, md.value) AS corrected_value
) ac ON TRUE
WHERE
  (md.value IS NOT NULL AND ac.corrected_value IS NOT NULL)
  OR md.value IS NULL;
")
  
  # End of view table creation
  
  
  # Enforce referential integrity for share_with columns linked to the user_groups table ########################################
  # Can't be done using FK as share_with is an array but user_groups column is not
  # Create function to remove a deleted group_id from all protected tables
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION remove_group_id_from_share_with()
RETURNS TRIGGER AS $$
BEGIN
    -- Update the share_with array to remove the deleted group_id for each table
    EXECUTE 'UPDATE locations SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE timeseries SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_continuous SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_discrete SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_calculated_daily SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE images SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE images_index SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE documents SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;
")
  # Create trigger to run function above when a group_id is deleted
  DBI::dbExecute(con, "CREATE TRIGGER remove_group_id_trigger
BEFORE DELETE ON user_groups
FOR EACH ROW
EXECUTE FUNCTION remove_group_id_from_share_with();
")
  
  # Create functions to validate and update users.user_groups
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION validate_user_groups()
RETURNS TRIGGER AS $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM unnest(NEW.user_groups) AS group_id
        WHERE group_id NOT IN (SELECT group_id FROM user_groups)
    ) THEN
        RAISE EXCEPTION 'Invalid group_id in user_groups array';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_user_groups_trigger
BEFORE INSERT OR UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION validate_user_groups();
")
  
  
  # Enforce referential integrity for user_groups array column
  # Create function to remove a deleted group_id from users.user_groups
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION remove_group_id_from_users()
RETURNS TRIGGER AS $$
BEGIN
    -- Update the share_with array to remove the deleted group_id for each table
    EXECUTE 'UPDATE users SET user_groups = array_remove(user_groups, $1) WHERE $1 = ANY(user_groups)' USING OLD.group_id;
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;
")
  # Create trigger to run function above when a group_id is deleted
  DBI::dbExecute(con, "CREATE TRIGGER remove_group_id_trigger2
BEFORE DELETE ON user_groups
FOR EACH ROW
EXECUTE FUNCTION remove_group_id_from_users();")
  
  
  # Create function to validate share_with array
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION validate_share_with()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if any group_id in the NEW.share_with array is not present in user_groups table
    IF EXISTS (
        SELECT 1
        FROM unnest(NEW.share_with) AS group_id
        WHERE group_id NOT IN (SELECT group_id FROM user_groups)
    ) THEN
        RAISE EXCEPTION 'Invalid group_id in share_with array';
    END IF;
    
    -- Check if 1 (public group) is in the share_with array
    IF 1 = ANY(NEW.share_with) AND array_length(NEW.share_with, 1) > 1 THEN
        RAISE EXCEPTION 'If group_id 1 (public) is present in the share_with array, it must be the only value';
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

")
  
  # Create triggers to run function above when share_with array is updated on each table
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_locations
BEFORE INSERT OR UPDATE ON locations
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_timeseries
BEFORE INSERT OR UPDATE ON timeseries
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_measurements_continuous
BEFORE INSERT OR UPDATE ON measurements_continuous
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_measurements_discrete
BEFORE INSERT OR UPDATE ON measurements_discrete
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_measurements_calculated_daily
BEFORE INSERT OR UPDATE ON measurements_calculated_daily
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_images
BEFORE INSERT OR UPDATE ON images
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_images_index
BEFORE INSERT OR UPDATE ON images_index
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  
  DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_documents
BEFORE INSERT OR UPDATE ON documents
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")
  

  
  # Enable row level security on tables locations, timeseries, measurements_continuous, measurements_discrete, measurements_calculated_daily, images, images_index, and documents ########################################
  DBI::dbExecute(con, "ALTER TABLE locations ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE timeseries ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE images ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE images_index ENABLE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE documents ENABLE ROW LEVEL SECURITY;")
  
  # Create policy for each table
  DBI::dbExecute(con, "CREATE POLICY location_policy ON locations
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY timeseries_policy ON timeseries
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY measurements_continuous_policy ON measurements_continuous
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY measurements_discrete_policy ON measurements_discrete
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY measurements_calculated_daily_policy ON measurements_calculated_daily
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY images_policy ON images
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY images_index_policy ON images_index
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  DBI::dbExecute(con, "CREATE POLICY documents_policy ON documents
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = ANY(u.user_groups)
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
  
  
  # -- Force RLS to be applied
  DBI::dbExecute(con, "ALTER TABLE locations FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE timeseries FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE measurements_continuous FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE measurements_discrete FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE images FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE images_index FORCE ROW LEVEL SECURITY;")
  DBI::dbExecute(con, "ALTER TABLE documents FORCE ROW LEVEL SECURITY;")
  
  
  
  
  
  # Create calibration and instrument maintenance tables and relationships in a new schema ########################################
  
  DBI::dbExecute(con, "CREATE SCHEMA instruments;")
  
  # Set search_path for the current session (it's set later on for all future sessions)
  DBI::dbExecute(con, 'SET search_path TO public, instruments')
  
  ## Create "instruments" and related tables ########################################
  
  DBI::dbExecute(con, "CREATE TABLE instruments.instrument_type (
                 type_id SERIAL PRIMARY KEY,
                 type TEXT NOT NULL,
                 description TEXT NOT NULL,
                 create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                 modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
                 )
                 ;")
  
  DBI::dbExecute(con, "CREATE TABLE instruments.instrument_make (
                 make_id SERIAL PRIMARY KEY,
                 make TEXT NOT NULL,
                 description TEXT,
                 create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                 modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
                 )
                 ;")
  
  DBI::dbExecute(con, "CREATE TABLE instruments.instrument_model (
                 model_id SERIAL PRIMARY KEY,
                 model TEXT NOT NULL,
                 description TEXT
                 create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                 modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
                 )
                 ;")
  
  DBI::dbExecute(con, "CREATE TABLE instruments.observers (
                 observer_id SERIAL PRIMARY KEY,
                 observer_first TEXT NOT NULL,
                 observer_last TEXT NOT NULL
                 organization TEXT NOT NULL,
                 create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                 modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                 UNIQUE (observer_first, observer_last, organization)
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.instruments (
                  instrument_id SERIAL PRIMARY KEY,
                  obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                  observer NUMERIC NOT NULL,
                  make TEXT NOT NULL,
                  model INTEGER NOT NULL,
                  type TEXT NOT NULL,
                  holds_replaceable_sensors BOOLEAN NOT NULL DEFAULT FALSE,
                  serial_no TEXT NOT NULL,
                  asset_tag TEXT,
                  date_in_service DATE,
                  date_purchased DATE,
                  date_end_of_life DATE,
                  retired_by TEXT,
                  date_retired DATE,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  UNIQUE (serial_no),
                  FOREIGN KEY (make) REFERENCES instruments.instrument_make(make_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (model) REFERENCES instruments.instrument_model(model_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (type) REFERENCES instruments.instrument_type(type_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.instrument_deployment (
                  deployment_id SERIAL PRIMARY KEY,
                  instrument_id INTEGER NOT NULL,
                  deployment_start_date DATE NOT NULL,
                  deployment_end_date DATE,
                  deployment_location TEXT,
                  deployment_purpose TEXT,
                  deployment_notes TEXT,
                  location_id INTEGER NOT NULL REFERENCES locations(location_id) ON UPDATE CASCADE ON DELETE SET NULL,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE
                  )
                 ;")
  
  ## Create "sensors" tables ########################################
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.sensor_types (
                  sensor_type_id SERIAL PRIMARY KEY,
                  sensor_type TEXT NOT NULL,
                  sensor_type_description TEXT,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
                 )")
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.sensors (
                  sensor_id SERIAL PRIMARY KEY,
                  sensor_type TEXT NOT NULL,
                  sensor_serial TEXT NOT NULL,
                  sensor_make TEXT NOT NULL,
                  sensor_model TEXT NOT NULL,
                  date_in_service DATE,
                  date_purchased DATE,
                  retired_by TEXT,
                  date_retired DATE,
                  sensor_asset_tag TEXT,
                  sensor_notes TEXT,
                  UNIQUE (sensor_serial),
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  FOREIGN KEY (sensor_type) REFERENCES instruments.sensor_types(sensor_type_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  
  ## Create instrument and sensor "maintenance" tables ########################################
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.instrument_maintenance (
                  event_id SERIAL PRIMARY KEY,
                  instrument_id INTEGER NOT NULL,
                  observer INTEGER NOT NULL,
                  obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                  note TEXT NOT NULL,
                  date_maintenance_due DATE,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.array_maintenance_changes (
                event_id SERIAL PRIMARY KEY,
                instrument_id INTEGER NOT NULL,
                observer INTEGER NOT NULL,
                obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                sensor1_id INTEGER NOT NULL,
                sensor1_notes TEXT,
                sensor2_id INTEGER,
                sensor2_notes TEXT,
                sensor3_id INTEGER,
                sensor3_notes TEXT,
                sensor4_id INTEGER,
                sensor4_notes TEXT,
                sensor5_id INTEGER,
                sensor5_notes TEXT,
                sensor6_id INTEGER,
                sensor6_notes TEXT,
                sensor7_id INTEGER,
                sensor7_notes TEXT,
                sensor8_id INTEGER,
                sensor8_notes TEXT,
                create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                FOREIGN KEY (instrument_id) REFERENCES instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor1_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor2_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor3_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor4_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor5_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor6_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor7_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor8_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE
                )
               ;")
  DBI::dbExecute(con, "COMMENT ON TABLE instruments.array_maintenance_changes IS 'This table is used to record changes to the sensors in an instrument array. Each row represents a single maintenance event, and the notes field should contain a description of the changes or maintenance made to each sensor. A simple maintenance event with no change of sensor should have the same sensor_id as the previous entry for that instrument and sensorX_id, while sensor changes must be recorded with a new sensor_id.'")
  
  
  
  ## Create "calibrations" table and associated tables #########################################
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrations (
                  calibration_id SERIAL PRIMARY KEY,
                  observer INTEGER NOT NULL,
                  obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                  id_sensor_holder INTEGER NOT NULL,
                  id_handheld_meter INTEGER,
                  purpose TEXT,
                  complete BOOLEAN NOT NULL DEFAULT FALSE,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  FOREIGN KEY (id_sensor_holder) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (id_handheld_meter) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_temperature (
                  calibration_id INTEGER NOT NULL,
                  temp_reference_desc TEXT NOT NULL,
                  temp_reference NUMERIC NOT NULL,
                  temp_observed NUMERIC NOT NULL,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_specific_conductance (
                  calibration_id INTEGER NOT NULL,
                  SpC1_std NUMERIC NOT NULL,
                  SpC2_std NUMERIC NOT NULL,
                  SpC1_pre NUMERIC NOT NULL,
                  SpC2_pre NUMERIC NOT NULL,
                  SpC1_post NUMERIC NOT NULL,
                  SpC2_post NUMERIC NOT NULL,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_pH (
                  calibration_id INTEGER NOT NULL,
                  pH1_std NUMERIC NOT NULL,
                  pH2_std NUMERIC NOT NULL,
                  pH3_std NUMERIC,
                  pH1_pre_val NUMERIC NOT NULL,
                  pH2_pre_val NUMERIC NOT NULL,
                  pH3_pre_val NUMERIC,
                  pH1_mV NUMERIC NOT NULL,
                  pH2_mV NUMERIC NOT NULL,
                  pH3_mV NUMERIC,
                  pH1_post_val NUMERIC NOT NULL,
                  pH2_post_val NUMERIC NOT NULL,
                  pH3_post_val NUMERIC,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                )
                ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_ORP (
                  calibration_id INTEGER NOT NULL,
                  orp_std NUMERIC NOT NULL,
                  orp_pre_mV NUMERIC NOT NULL,
                  orp_post_mV NUMERIC NOT NULL,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_turbidity (
                  calibration_id INTEGER NOT NULL,
                  turb1_std NUMERIC NOT NULL,
                  turb2_std NUMERIC NOT NULL,
                  turb1_pre NUMERIC NOT NULL,
                  turb2_pre NUMERIC NOT NULL,
                  turb1_post NUMERIC NOT NULL,
                  turb2_post NUMERIC NOT NULL,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_dissolved_oxygen (
                  calibration_id INTEGER NOT NULL,
                  baro_press_pre NUMERIC NOT NULL,
                  baro_press_post NUMERIC NOT NULL,
                  DO_pre_mgl NUMERIC NOT NULL,
                  DO_post_mgl NUMERIC NOT NULL,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  DBI::dbExecute(con,
                 "CREATE TABLE instruments.calibrate_depth (
                  calibration_id INTEGER NOT NULL,
                  depth_check_ok BOOLEAN NOT NULL,
                  depth_changes_ok BOOLEAN,
                  create_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  modify_datetime TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")
  
  
  ## Function and Trigger to update the modify_datetime column on update ########################################
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION instruments.update_modify_datetime()
    RETURNS TRIGGER AS $$
    BEGIN
      NEW.modify_datetime = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
  ")
  
  # Creating triggers for tables
  tables_to_update <- c('instrument_type', 'instrument_make', 'instrument_model', 'observers', 'instruments', 'instrument_deployment', 'sensor_types', 'sensors', 'instrument_maintenance', 'array_maintenance_changes', 'calibrations', 'calibrate_temperature', 'calibrate_specific_conductance', 'calibrate_pH', 'calibrate_ORP', 'calibrate_turbidity', 'calibrate_dissolved_oxygen', 'calibrate_depth')
  
  for (table in tables_to_update) {
    DBI::dbExecute(con, paste0("
      CREATE TRIGGER update_", table, "_modify_datetime
      BEFORE UPDATE ON instruments.", table, "
      FOR EACH ROW
      EXECUTE FUNCTION instruments.update_modify_datetime();
    "))
  }
  
  
  # Create metadata tables for the main database that link to the 'instruments' schema as well ########################################
  
  # Locations metadata that can change over time and for which records should be kept is part of table 'location_metadata_xxx'
  ## Location ownership metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_owners_operators (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               owner INTEGER NOT NULL REFERENCES owners_contributors(owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
               operator INTEGER NOT NULL REFERENCES owners_contributors(owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE,
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_owners_operators IS 'Ownership and operator metadata for locations.'")
  # populate missing entries with the most recent entry and ensure end_datetime gets filled in when new data is added
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_owners_operators_missing()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- Check that both owner and operator are not NULL
                  IF NEW.owner IS NULL AND NEW.operator IS NULL THEN
                      RAISE EXCEPTION 'Both owner and operator cannot be NULL';
                  END IF;
                  
                  -- Step 1: If owner is NULL, fill it with the previous value
                  IF NEW.owner IS NULL THEN
                      SELECT owner
                      INTO NEW.owner
                      FROM locations_metadata_owners_operators
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 2: If operator is NULL, fill it with the previous value
                  IF NEW.operator IS NULL THEN
                      SELECT operator
                      INTO NEW.operator
                      FROM locations_metadata_owners_operators
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 3: If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_owners_operators
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_owners_operators_missing_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_owners_operators
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_owners_operators_missing();
")
  
  ## Site access metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_access (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               method TEXT, -- helicopter, boat, foot, etc.
               health_safety TEXT,
               notes TEXT,
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_access IS 'Site access metadata for locations.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_access.method IS 'Method of access, such as helicopter, boat, foot, etc.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_access.health_safety IS 'Health and safety considerations for accessing the site.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_access.notes IS 'Additional notes about site access.'")
  # populate missing entries with the most recent entry and ensure end_datetime gets filled in when new data is added
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_access_missing()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- Check that method, and notes, and health_safety are not NULL
                  IF NEW.method IS NULL AND NEW.notes IS NULL AND NEW.health_safety IS NULL THEN
                      RAISE EXCEPTION 'Method, notes, and health_safety cannot be NULL';
                  END IF;
                  
                  -- Step 1: If method is NULL, fill it with the previous value
                  IF NEW.method IS NULL THEN
                      SELECT method
                      INTO NEW.method
                      FROM locations_metadata_access
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 2: If notes is NULL, fill it with the previous value
                  IF NEW.notes IS NULL THEN
                      SELECT notes
                      INTO NEW.notes
                      FROM locations_metadata_access
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 3: If health_safety is NULL, fill it with the previous value
                  IF NEW.health_safety IS NULL THEN
                      SELECT health_safety
                      INTO NEW.health_safety
                      FROM locations_metadata_access
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 4: If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_access
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_access_missing_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_access
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_access_missing();
")
  
  ## Table for location maintenance such as brushing, cleaning, etc. ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_maintenance (
               location_maintenance_id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id),
               maintenance_performed TEXT,
               date_performed DATE
               maintenance_due TEXT,
               maintenance_flag BOOLEAN
               ")
  
  DBI::dbExecute(con, "
               CREATE OR REPLACE FUNCTION enforce_maintenance_constraints()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if at least one of maintenance_performed or maintenance_due is not NULL
    IF NEW.maintenance_performed IS NULL AND NEW.maintenance_due IS NULL THEN
        RAISE EXCEPTION 'Either maintenance_performed or maintenance_due must be provided.';
    END IF;

    -- If maintenance_due is not NULL, set maintenance_flag to TRUE
    IF NEW.maintenance_due IS NOT NULL THEN
        NEW.maintenance_flag := TRUE;
    END IF;

    -- If maintenance_performed is not NULL, ensure date_performed is also provided
    IF NEW.maintenance_performed IS NOT NULL AND NEW.date_performed IS NULL THEN
        RAISE EXCEPTION 'date_performed cannot be NULL when maintenance_performed is provided.';
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")
  
  DBI::dbExecute(con, "
               CREATE TRIGGER trigger_enforce_maintenance_constraints
BEFORE INSERT OR UPDATE ON locations_metadata_maintenance
FOR EACH ROW
EXECUTE FUNCTION enforce_maintenance_constraints();
")
  
  ## Data transmission metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_transmission (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               method TEXT NOT NULL, -- GOES, Iridium, etc.
               transmitter INTEGER REFERENCES instruments.instruments(instrument_id) ON DELETE SET NULL ON UPDATE CASCADE,
               transmission_code TEXT,
               battery INTEGER REFERENCES instruments.instruments(instrument_id) ON DELETE SET NULL ON UPDATE CASCADE,
               solar_panel INTEGER REFERENCES instruments.instruments(instrument_id) ON DELETE SET NULL ON UPDATE CASCADE,
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_transmission IS 'Data transmission metadata for locations.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_transmission.method IS 'Method of data transmission, such as GOES, cellular, Iridium, etc.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_transmission.transmitter IS 'Transmitter instrument ID.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_transmission.battery IS 'Battery instrument ID.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_transmission.solar_panel IS 'Solar panel instrument ID.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_transmission.transmission_code IS 'Transmission code, if applicable.'")
  
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_transmission_missing()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_transmission
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_transmission_missing_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_transmission
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_transmission_missing();
")
  
  
  ## Data acquisition metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_acquisition (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               instruments INTEGER[] NOT NULL,
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_acquisition IS 'Data acquisition metadata for locations, such as sondes, instrument arrays, etc. These may depend on instruments in table locations_metadata_transmission for data transmission.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_acquisition.instruments IS 'Array of instrument IDs used for data acquisition, such as sonde IDs, loggers, etc. Each array element must exist in table instruments.instruments, enforced with a trigger and function.'")
  
  
  # Add check that all elements of 'instruments' exist in table 'instruments.instruments' as this can't be done with a foreign key on the array
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION check_locations_metadata_acquisition_instruments()
              RETURNS TRIGGER AS $$
              BEGIN
                  IF NEW.instruments IS NOT NULL THEN
                      FOR i IN 1..array_length(NEW.instruments, 1) LOOP
                          IF NOT EXISTS (SELECT 1 FROM instruments.instruments WHERE instrument_id = NEW.instruments[i]) THEN
                              RAISE EXCEPTION 'Instrument ID % does not exist in table instruments.instruments.', NEW.instruments[i];
                          END IF;
                      END LOOP;
                  END IF;
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER check_locations_metadata_acquisition_instruments_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_acquisition
              FOR EACH ROW
              EXECUTE FUNCTION check_locations_metadata_acquisition_instruments();
")
  
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_acquisition_missing()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_acquisition
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_acquisition_missing_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_acquisition
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_acquisition_missing();
")
  
  
  
  ## General infrastructure metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_infrastructure (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               site_description TEXT, -- i.e. 'open field', 'forest', 'lake', etc.
               infrastructure_description TEXT, -- i.e. 'tower', 'shelter', 'cabin', 'well', etc.
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_infrastructure IS 'General infrastructure metadata for locations as freehand text.'")
  # populate missing entries with the most recent entry and ensure end_datetime gets filled in when new data is added
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_infrastructure_missing()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- Check that both site_description and infrastructure_description are not NULL
                  IF NEW.site_description IS NULL AND NEW.infrastructure_description IS NULL THEN
                      RAISE EXCEPTION 'Both site_description and infrastructure_description cannot be NULL';
                  END IF;
                  
                  -- Step 1: If site_description is NULL, fill it with the previous value
                  IF NEW.site_description IS NULL THEN
                      SELECT site_description 
                      INTO NEW.site_description
                      FROM locations_metadata_infrastructure
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 2: If infrastructure_description is NULL, fill it with the previous value
                  IF NEW.infrastructure_description IS NULL THEN
                      SELECT infrastructure_description 
                      INTO NEW.infrastructure_description
                      FROM locations_metadata_infrastructure
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 3: If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_infrastructure
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_infrastructure_missing_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_infrastructure
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_infrastructure_missing();
")
  
  ## Hydrometric-specific infrastructure metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_infrastructure_hydromet (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               stilling_well TEXT,
               staff_gauge TEXT,
               tower TEXT,
               shelter TEXT,
               cableway TEXT,
               reach_substrate TEXT,
               reach_grade NUMERIC,
               reach_note TEXT,
               health_safety TEXT,
               local_g NUMERIC,
               magnetic_declination NUMERIC,
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_infrastructure_hydromet IS 'Hydrometric-specific infrastructure metadata.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.stilling_well IS 'Type of stilling well and details.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.staff_gauge IS 'Type of staff gauge and details.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.tower IS 'Type of tower and details.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.shelter IS 'Type of shelter and details.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.reach_substrate IS 'Substrate type at reach.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.reach_grade IS 'Grade of reach (approximate, degrees).'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.reach_note IS 'Additional notes about reach.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_hydromet.local_g IS 'Location-specific gravitational constant'")
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_infrastructure_hydromet()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- Check that end_datetime gets populated
                  UPDATE locations_metadata_infrastructure_hydromet
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_infrastructure_hydromet_trigger
              BEFORE INSERT ON locations_metadata_infrastructure_hydromet
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_infrastructure_hydromet();
")
  
  ## GW-specific infrastructure metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_infrastructure_groundwater (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               well_diameter_cm NUMERIC,
               stick_up_m NUMERIC,
               start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
               end_datetime TIMESTAMP WITH TIME ZONE,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_infrastructure_groundwater IS 'Groundwater-specific infrastructure metadata.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_groundwater.well_depth_btoc_m IS 'Depth of well in meters below top of casing.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_groundwater.well_diameter_cm IS 'Diameter of well in centimeters.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_infrastructure_groundwater.stick_up_m IS 'Distance from top of casing to ground surface in meters.'")
  # populate missing entries with the most recent entry and ensure end_datetime gets filled in when new data is added
  DBI::dbExecute(con, "
              CREATE OR REPLACE FUNCTION fill_locations_metadata_infrastructure_groundwater()
              RETURNS TRIGGER AS $$
              BEGIN
                  -- Check that end_datetime gets populated
                  UPDATE locations_metadata_infrastructure_groundwater
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$ LANGUAGE plpgsql;
")
  DBI::dbExecute(con, "
              CREATE TRIGGER fill_locations_metadata_infrastructure_groundwater_trigger
              BEFORE INSERT OR UPDATE ON locations_metadata_infrastructure_groundwater
              FOR EACH ROW
              EXECUTE FUNCTION fill_locations_metadata_infrastructure_groundwater();
")
  
  ## Stream cross-section metadata ########################################
  DBI::dbExecute(con, "
               CREATE TABLE locations_metadata_xsections (
               id SERIAL PRIMARY KEY,
               location_id INTEGER NOT NULL REFERENCES locations(location_id) ON DELETE CASCADE,
               x_m NUMERIC[],
               y_top_m NUMERIC[] DEFAULT '{0}',
               y_bot_m NUMERIC[],
               mean_velocity_m_s NUMERIC[],
               instrument INTEGER REFERENCES instruments.instruments(instrument_id) ON DELETE SET NULL ON UPDATE CASCADE,
               angle_deg NUMERIC DEFAULT 90,
               water_level_m NUMERIC,
               xsection_substrate TEXT,
               xsection_note TEXT,
               measurement_grade TEXT REFERENCES grades(code) ON DELETE SET NULL ON UPDATE CASCADE,
               valid TIMESTAMP WITH TIME ZONE NOT NULL,
               created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
               modified TIMESTAMP WITH TIME ZONE);
               ")
  DBI::dbExecute(con, "COMMENT ON TABLE locations_metadata_xsections IS 'Cross-section and reach metadata for hydrometric sites.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.x_m IS 'Distance along cross-section from left bank in meters.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.y_top_m IS 'Top elevation of cross-section in meters (default 0).'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.y_bot_m IS 'Bottom elevation of cross-section in meters.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.mean_velocity_m_s IS 'Mean velocity of water at cross-section x-value in cubic meters per second.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.instrument IS 'Instrument used to measure velocity, referencing instruments.instruments table.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.angle_deg IS 'Angle of cross-section relative to perpendicular to channel orientation. 90 (default) is perpendicular.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.water_level_m IS 'Water level at time of cross-section measurements in meters.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.xsection_substrate IS 'Substrate type at cross-section (description).'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.xSection_note IS 'Additional notes about cross-section.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN locations_metadata_xsections.valid IS 'Timestamp of validity of data.'")
  
  
  ## Tables to hold rating curves and relevant data ########################################
  DBI::dbExecute(con, "CREATE TABLE rating_curves (
  curve_id SERIAL PRIMARY KEY,
  location_id INTEGER NOT NULL REFERENCES locations(location_id),
  input_parameter_id INTEGER NOT NULL REFERENCES parameters(parameter_id),
  output_parameter_id INTEGER NOT NULL REFERENCES parameters(parameter_id),
  valid_from TIMESTAMP WITH TIME ZONE,
  valid_to TIMESTAMP WITH TIME ZONE,
  offset_value NUMERIC NOT NULL DEFAULT 0,
  approval TEXT REFERENCES approvals(code),
  created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  modified TIMESTAMP WITH TIME ZONE,
  description TEXT,
  notes TEXT,
  UNIQUE(location_id, input_parameter_id, output_parameter_id, valid_from)
)")
  
  DBI::dbExecute(con, "CREATE TABLE rating_curve_points (
  curve_point_id SERIAL PRIMARY KEY,
  curve_id INTEGER NOT NULL REFERENCES rating_curves(curve_id),
  input_value numeric NOT NULL,
  output_value numeric NOT NULL,
  UNIQUE(curve_id, input_value)
  )
")
  
  DBI::dbExecute(con, "CREATE TABLE rating_curve_shifts (
  curve_shift_id SERIAL PRIMARY KEY,
  curve_id INTEGER NOT NULL REFERENCES rating_curves(curve_id),
  shift_start TIMESTAMP WITH TIME ZONE NOT NULL,
  shift_end TIMESTAMP WITH TIME ZONE NOT NULL,
  shift_value numeric NOT NULL,
  created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  modified TIMESTAMP WITH TIME ZONE,
  UNIQUE(curve_id, shift_start, shift_end)
)")
  
  # Enable btree_gist extension, which is required for the exclusion constraints
  DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS btree_gist;
")
  
  DBI::dbExecute(con, "ALTER TABLE rating_curves
ADD CONSTRAINT rating_curves_no_overlap
EXCLUDE USING gist (
  location_id WITH =,
  input_parameter_id WITH =,
  output_parameter_id WITH =,
  tstzrange(valid_from, valid_to, '[]') WITH &&
);
")
  
  DBI::dbExecute(con, "ALTER TABLE rating_curve_shifts
ADD CONSTRAINT rating_curve_shifts_no_overlap
EXCLUDE USING gist (
  curve_id WITH =,
  tstzrange(shift_start, shift_end, '[]') WITH &&
);
")
  
  ## Function and Trigger to update the `modified` column on update for multiple tables ########################################
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION update_modified()
    RETURNS TRIGGER AS $$
    BEGIN
      NEW.modified = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
  ")
  
  # Creating triggers for tables
  tables_to_update <- c('locations_metadata_owners_operators', 'locations_metadata_access', 'locations_metadata_transmission', 'locations_metadata_acquisition', 'locations_metadata_infrastructure', 'locations_metadata_infrastructure_hydromet', 'locations_metadata_infrastructure_groundwater', 'locations_metadata_xsections', 'rating_curves', 'rating_curve_shifts')
  
  for (table in tables_to_update) {
    DBI::dbExecute(con, paste0("
      CREATE TRIGGER update_", table, "_modified
      BEFORE UPDATE ON ", table, "
      FOR EACH ROW
      EXECUTE FUNCTION update_modified();
    "))
  }
  
  tables_to_update2 <- c("measurements_continuous", "measurements_discrete", "measurements_calculated_daily")
  
  # Create the trigger function
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION update_created_modified()
  RETURNS TRIGGER AS $$
  BEGIN
    NEW.created_modified = CURRENT_TIMESTAMP;
    RETURN NEW;
  END;
  $$ LANGUAGE plpgsql;
")
  
  # Create triggers for each table
  for (table in tables_to_update2) {
    DBI::dbExecute(con, paste0("
    CREATE TRIGGER update_", table, "_created_modified
    BEFORE INSERT OR UPDATE ON ", table, "
    FOR EACH ROW
    EXECUTE FUNCTION update_created_modified();
  "))
  }
  
  # Create metadata view tables for internal purposes 3
#   DBI::dbExecute(con, "
#   CREATE OR REPLACE VIEW location_core_metadata AS
#   SELECT
#   l.location_id,
#   l.location,
#   l.name,
#   l.latitude,
#   l.longitude,
#   l.contact,
#   l.install_purpose,
#   l.current_purpose,
#   l.note,
#   -- Owners and Operators
#   oo.owner,
#   oo.operator,
#   -- Access Metadata
#   a.method AS access_method,
#   a.health_safety AS access_health_safety,
#   a.notes AS access_notes,
#   -- Maintenance Metadata
#   m.maintenance_performed AS last_maintenance_performed,
#   m.date_performed AS last_maintenance_performed_date,
#   m.maintenance_due,
#   m.maintenance_flag,
#   -- Transmission Metadata
#   t.method AS transmission_method,
#   t.transmitter,
#   t.transmission_code,
#   t.battery,
#   t.solar_panel,
#   t.instruments AS transmission_instruments,
#   -- General Infrastructure Metadata
#   i.site_description,
#   i.infrastructure_description
#   FROM
#   locations l
#   -- Join with Owners and Operators metadata (latest entry)
#   LEFT JOIN LATERAL (
#     SELECT *
#       FROM locations_metadata_owners_operators oo
#     WHERE oo.location_id = l.location_id
#     AND oo.start_datetime <= NOW()
#     AND (oo.end_datetime IS NULL OR oo.end_datetime > NOW())
#     ORDER BY oo.start_datetime DESC
#     LIMIT 1
#   ) oo ON TRUE
#   -- Join with Access metadata (latest entry)
#   LEFT JOIN LATERAL (
#     SELECT *
#       FROM locations_metadata_access a
#     WHERE a.location_id = l.location_id
#     AND a.start_datetime <= NOW()
#     AND (a.end_datetime IS NULL OR a.end_datetime > NOW())
#     ORDER BY a.start_datetime DESC
#     LIMIT 1
#   ) a ON TRUE
#   -- Join with Maintenance metadata (latest entry)
#   LEFT JOIN LATERAL (
#     SELECT *
#       FROM locations_metadata_maintenance m
#     WHERE m.location_id = l.location_id
#     ORDER BY m.date_performed DESC
#     LIMIT 1
#   ) m ON TRUE
#   -- Join with Transmission metadata (latest entry)
#   LEFT JOIN LATERAL (
#     SELECT *
#       FROM locations_metadata_transmission t
#     WHERE t.location_id = l.location_id
#     AND t.start_datetime <= NOW()
#     AND (t.end_datetime IS NULL OR t.end_datetime > NOW())
#     ORDER BY t.start_datetime DESC
#     LIMIT 1
#   ) t ON TRUE
#   -- Join with General Infrastructure metadata (latest entry)
#   LEFT JOIN LATERAL (
#     SELECT *
#       FROM locations_metadata_infrastructure i
#     WHERE i.location_id = l.location_id
#     AND i.start_datetime <= NOW()
#     AND (i.end_datetime IS NULL OR i.end_datetime > NOW())
#     ORDER BY i.start_datetime DESC
#     LIMIT 1
#   ) i ON TRUE
#   ORDER BY
#   l.location_id;
#   ")
#   
#   DBI::dbExecute(con, "CREATE OR REPLACE VIEW location_hydrometric_metadata AS
# SELECT
#     l.location_id,
#     l.location,
#     l.name,
#     l.latitude,
#     l.longitude,
#     -- Hydrometric Infrastructure Metadata
#     ih.stilling_well,
#     ih.staff_gauge,
#     ih.tower,
#     ih.shelter,
#     ih.cableway,
#     ih.reach_substrate,
#     ih.reach_grade,
#     ih.reach_note,
#     ih.health_safety AS hydromet_health_safety,
#     ih.local_g,
#     ih.magnetic_declination,
#     ih.start_datetime AS hydromet_start,
#     ih.end_datetime AS hydromet_end
# FROM
#     locations l
#     -- Join with Hydrometric Infrastructure metadata (latest entry)
#     INNER JOIN LATERAL (
#         SELECT *
#         FROM locations_metadata_infrastructure_hydromet ih
#         WHERE ih.location_id = l.location_id
#           AND ih.start_datetime <= NOW()
#           AND (ih.end_datetime IS NULL OR ih.end_datetime > NOW())
#         ORDER BY ih.start_datetime DESC
#         LIMIT 1
#     ) ih ON TRUE
# ORDER BY
#     l.location_id;
# ")
#   
#   DBI::dbExecutE(con, "CREATE OR REPLACE VIEW location_groundwater_metadata AS
# SELECT
#     l.location_id,
#     l.location,
#     l.name,
#     l.latitude,
#     l.longitude,
#     -- Groundwater Infrastructure Metadata
#     ig.well_depth_btoc_m,
#     ig.well_diameter_cm,
#     ig.stick_up_m,
#     ig.start_datetime AS groundwater_start,
#     ig.end_datetime AS groundwater_end
# FROM
#     locations l
#     -- Join with Groundwater Infrastructure metadata (latest entry)
#     INNER JOIN LATERAL (
#         SELECT *
#         FROM locations_metadata_infrastructure_groundwater ig
#         WHERE ig.location_id = l.location_id
#           AND ig.start_datetime <= NOW()
#           AND (ig.end_datetime IS NULL OR ig.end_datetime > NOW())
#         ORDER BY ig.start_datetime DESC
#         LIMIT 1
#     ) ig ON TRUE
# ORDER BY
#     l.location_id;
# ")
  
  
  # Modify the search path to include all schemas ########################################
  DBI::dbExecute(con, 'ALTER DATABASE "AquaCache" SET search_path TO public, instruments, information;')
  # Set search_path for the current session in case the user doesn't disconnect right away
  DBI::dbExecute(con, 'SET search_path TO public, instruments, information')
  
  
  # Create accounts ########################################
  tryCatch({
    DBI::dbExecute(con, "CREATE ROLE AquaCache_read WITH LOGIN PASSWORD 'AquaCache';")
    DBI::dbExecute(con, 'GRANT CONNECT ON DATABASE "AquaCache" TO AquaCache_read;')
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public TO AquaCache_read;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA instruments TO AquaCache_read;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO AquaCache_read;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA instruments TO AquaCache_read;")
  }, error = function(e) {
    warning("May have failed to create new read only account with name AquaCache_read. Ignore this message if it already exists (this function would not have erased the old account).")
  })
  # Account able to add/edit/delete data and metadata
  tryCatch({
    DBI::dbExecute(con, "CREATE ROLE AC_editor WITH LOGIN PASSWORD '!Water2024';")
    DBI::dbExecute(con, 'GRANT CONNECT ON DATABASE "AquaCache" TO AC_editor;')
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public TO AC_editor;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA instruments TO AC_editor;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO AC_editor;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA instruments TO AC_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO AC_editor;")
    DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA instruments GRANT INSERT, UPDATE, DELETE ON TABLES TO AC_editor;")
    
    DBI::dbExecute(con, "GRANT INSERT, UPDATE, DELETE ON TABLE public.corrections, public.datum_conversions, public.documents, public.documents_spatial, public.extrema, public.forecasts, public.images, public.images_index, public.laboratories, public.locations, public.locations_metadata_access, public.locations_metadata_infrastructure, public.locations_metadata_infrastructure_groundwater, public.locations_metadata_infrastructure_hydromet, public.locations_metadata_maintenance, public.locations_metadata_owners_operators, public.locations_metadata_transmission, public.locations_metadata_xsections, public.locations_networks, public.locations_projects, public.measurements_continuous, public.measurements_discrete, public.networks, public.owners_contributors, public.projects, public.rasters, public.rasters_reference, public.thresholds, public.timeseries, public.vectors TO AC_editor;")
    DBI::dbExecute(con, "GRANT INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA instruments TO AC_editor;")
    
  }, error = function(e) {
    warning("May have failed to create new read only account with name AquaCache_read. Ignore this message if it already exists (this function would not have erased the old account).")
  })
  

  message("The database was successfully created. If a read-only account was created it has username AquaCache_read and password AquaCache")
}
