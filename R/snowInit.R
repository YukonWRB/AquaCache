#' Initial PostgreSQL snow database creation.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Creates a PostgreSQL database or replaces an existing database. Established pre-set table structure. All tables are created and with primary keys.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [snowConnect()].
#' @param overwrite TRUE overwrites the database, if one exists in the same path. Nothing will be kept. FALSE will create tables only where they are missing.
#'
#' @return A PostgreSQL database in ....
#' @export
#'
#'

#TODO: deal with geometry of polygon fields of basin and sub-basin

snowInit <- function(con = snowConnect(), overwrite = FALSE) {

  if (overwrite){
    DBI::dbExecute(con, "DROP EXTENSION postgis CASCADE")
    for (i in DBI::dbListTables(con)){
      tryCatch({
        DBI::dbExecute(con, paste0("DROP TABLE ", i, " CASCADE"))
      }, error = function(e) {
        DBI::dbExecute(con, paste0("DROP VIEW ", i))
      })
    }

    DBI::dbExecute(con, "VACUUM")
  }

  rpostgis::pgPostGIS(con, raster = TRUE)

  #### basins
  DBI::dbExecute(con, "CREATE TABLE if not exists basins (
                 basin TEXT PRIMARY KEY,
                 polygon POLYGON)"
  )

  #### sub-basins
  DBI::dbExecute(con, "CREATE TABLE if not exists sub_basins (
                 sub_basin TEXT PRIMARY KEY,
                 polygon geometry(Polygon, 4269) NOT NULL,
                 CONSTRAINT enforce_dims_geom2 CHECK (st_ndims(polygon) = 2),
                 CONSTRAINT enforce_geotype_geom2 CHECK (geometrytype(polygon) = 'POLYGON'::text),
                 CONSTRAINT enforce_srid_geom2 CHECK (st_srid(polygon) = 4269),
                 CONSTRAINT enforce_valid_geom2 CHECK (st_isvalid(polygon)))")#,

  #### locations
  DBI::dbExecute(con, "CREATE TABLE if not exists locations (
                 location TEXT PRIMARY KEY,
                 name TEXT NOT NULL UNIQUE,
                 agency TEXT,
                 basin TEXT,
                 sub_basin TEXT,
                 active BOOLEAN,
                 elevation NUMERIC,
                 latitude NUMERIC,
                 longitude NUMERIC,
                 notes TEXT,

                 FOREIGN KEY (basin) REFERENCES basins(basin),
                 FOREIGN KEY (sub_basin) REFERENCES sub_basins(sub_basin))"
  )

  #### maintenance
  DBI::dbExecute(con, "CREATE TABLE if not exists maintenance (
                 maintenance_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 date DATE NOT NULL,
                 maintenance TEXT NOT NULL,
                 completed BOOLEAN NOT NULL,

                 FOREIGN KEY (location) REFERENCES locations(location))"
  )

  #### surveys
  DBI::dbExecute(con, "CREATE TABLE if not exists surveys (
                 survey_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 target_date DATE NOT NULL,
                 survey_date DATE NOT NULL,
                 notes TEXT,
                 sampler_name TEXT,
                 CONSTRAINT survey_loc UNIQUE (survey_date, location),

                 FOREIGN KEY (location) REFERENCES locations(location))"
  )

  #### measurements
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements (
                 measurement_id SERIAL PRIMARY KEY,
                 survey_id INTEGER NOT NULL,
                 sample_datetime TIMESTAMP NOT NULL,
                 estimate_flag BOOLEAN NOT NULL,
                 exclude_flag BOOLEAN NOT NULL,
                 SWE NUMERIC,
                 depth NUMERIC,
                 average BOOLEAN,
                 notes TEXT,

                 FOREIGN KEY (survey_id) REFERENCES surveys(survey_id))"
  )

  #### means (view)
  DBI::dbExecute(con, paste0("CREATE VIEW means AS ",
                             "WITH measurement_counts AS (",
                             "  SELECT survey_id, COUNT(*) AS total_count ",
                             "  FROM measurements ",
                             "  GROUP BY survey_id",
                             ")",
                             "SELECT surveys.location, measurements.survey_id, surveys.target_date, ",
                             "AVG(swe) AS swe, AVG(depth) AS depth, MIN(sample_datetime) AS sample_datetime, ",
                             "STDDEV(swe) AS swe_sd, STDDEV(depth) AS depth_sd, ",
                             "COUNT(*) AS sample_count_used, ",
                             "total_count - COUNT(*) AS sample_count_ex, ",
                             "BOOL_OR(measurements.estimate_flag) AS estimate_flag ",
                             "FROM measurements ",
                             "INNER JOIN surveys ON measurements.survey_id = surveys.survey_id ",
                             "LEFT JOIN measurement_counts ON measurements.survey_id = measurement_counts.survey_id ",
                             "WHERE exclude_flag = FALSE ",
                             "GROUP BY measurements.survey_id, surveys.location, surveys.target_date, total_count"
  ))

  #### Add Comments to table and columns
  ## basins
  DBI::dbExecute(con, "COMMENT ON TABLE public.basins IS 'Stores the basins used to categorize the snow courses. Contains the name of the basin and the polygon that represents it. Currently does not contain the polygons.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.basins.basin IS 'The unique name for the basin.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.basins.polygon IS 'Polygon that represents the spatial extent of the basin. Currently is NULL for all basins. Will be updated to type GEOMETRY(POLYGON, 4269) when polygons are added.'")

  ## sub_basins
  DBI::dbExecute(con, "COMMENT ON TABLE public.sub_basins IS 'Stores the sub-basins used to categorize the snow courses in the snow bulletin. Contains the name of the basin and the polygon that represents it.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.sub_basins.sub_basin IS 'The unique name for the sub_basin.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.sub_basins.polygon IS 'Polygon that represents the spatial extent of the sub_basin.'")

  ## locations
  DBI::dbExecute(con, "COMMENT ON TABLE public.locations IS 'Stores information relevant to a snow course location. A snow course is as a specific area where snow surveys are routinely conducted.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.location IS 'The unique identifier of the snow course location. The id was created based on the Water Survey of Canada id convention, with -SC added for snow course, and a number, starting at 01.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.name IS 'The commonly used name of the snow course.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.agency IS 'The agency that conducts the snow survey at this snow course. Options are: 1. Parcs Canada, 2. Private Contract, 3. Vuntut Gwitchin First Nation, 4. Yukon Energy Corporation, 5. Yukon Energy Mines and Resources, Compliance Monitoring and Inspections Branch, and 6. Yukon Environment, Water Resources Branch.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.basin IS 'The basin is which the snow course is contained. A foreign key reffering to basin from basins table.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.sub_basin IS 'The sub-basin is which the snow course is contained. Refers to the basin names used in the snow bulletin. A foreign key reffering to sub_basin from sub_basins table.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.active IS 'TRUE if the snow course is still active, FALSE if snow data was collected in the past, but is no longer being collected.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.elevation IS 'Elevation in metres of the snow course. Is not a highly accurate value. Collected from a GPS or using Google Earth.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.latitude IS 'Latitude of the snow course in decimal degrees. Does not need to be extremely precise as the samples are taken over an area.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.longitude IS 'Longitude of the snow course in decimal degrees. Does not need to be extremely precise as the samples are taken over an area.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.locations.notes IS 'Notes specific to a snow course location. Could be general location description. Notes specific to a survey should go in the surveys table.'")

  ## maintenance
  DBI::dbExecute(con, "COMMENT ON TABLE public.maintenance IS 'Keeps a log of snow course maintenance, including what needs to be completed and what has already been done. Populated through auto-increment.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.maintenance_id IS 'The unique identifier of the maintenance entry.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.location IS 'The snow course for which this maintenance is linked to. A foreign key reffering to location from locations table.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.date IS 'The date that the maintenance requirement was noted.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.maintenance IS 'The maintenance to be completed. Ex: sign 4 is missing and needs replacing'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.completed IS 'TRUE if the maintenance has been completed. FALSE if the maintenance has yet to be completed.'")

  ## surveys
  DBI::dbExecute(con, "COMMENT ON TABLE public.surveys IS 'Stores the details of a single snow survey. A snow survey is the collection of multiple samples at a single snow course during a single visit. Does not contain the samples themselves. The table is the connection between the locations and the measurements.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.survey_id IS 'The unique identifier of the snow survey. Populated through auto-increment.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.location IS 'The location (snow course) where the snow survey was conducted. A foreign key referring to location of the locations table. The location-survey_date combination must be unique.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.target_date IS 'The targetted date of the snow survey. Usually the first of the month.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.survey_date IS 'The date on which the snow survey was completed. Usually within a couple of days of the target date. The location-survey_date combination must be unique.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.notes IS 'General notes on the snow survey. Concatenation of all condition notes from snow survey template (Weather at time of sampling, Sampling conditions, Remarks.)'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.sampler_name IS 'The names of the people who completed the sample. This was not collected prior to the 2024 snow season.'")

  ## measurements
  DBI::dbExecute(con, "COMMENT ON TABLE public.measurements IS 'Stores the details of a single snow sample. A single snow survey will contain multiple samples, tipically 10. However, preceding 2024, only the average of samples was noted in the database, and as such only a single swe and depth measurement are given per snow survey.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.measurement_id IS 'The unique identifier of the sample. Populated through auto-increment.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.survey_id IS 'The survey to which the measurement is linked. A foreign key referring to survey_id of the surveys table.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.sample_datetime IS 'The date and time on which the sample was collected. This can be the same time for all samples if a time was not given. If a start and end time are given, times will be set to equal increments between start and end time of survey. Survey_id and sample_datetime do not need to be a unique combination because measurement_id will be unique.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.estimate_flag IS 'Completed during QAQC. Is only used when average = TRUE. Indicates that the average measurement was estimated.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.exclude_flag IS 'Completed during QAQC. Instead of removing the sample, it is kept, but with this flag.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.swe IS 'Measured SWE for a single sample or the average of multiple samples if average = TRUE.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.depth IS 'Measured depth for a single sample or the average of multiple samples if average = TRUE.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.average IS 'TRUE if depth and SWE values represent an average of multiple samples. FALSE if SWE and depth values are for a single sample. All entries before the 2024 snow season are calculated averages.'")
  DBI::dbExecute(con, "COMMENT ON COLUMN public.measurements.notes IS 'Notes specific to a sample. Ex: ground ice layer thickness, number of attempts, etc.'")

  "Before the institution of this postgreSQL database, only the average of the samples were recorded in the database. As such, for the years 2023 and before, only depth and swe are averages, "
  ## means


  #### Create a read-only account
  tryCatch({
    DBI::dbExecute(con, "CREATE ROLE snow_read WITH LOGIN PASSWORD 'snow';")
    DBI::dbExecute(con, "GRANT CONNECT ON DATABASE snowDB TO snow_read;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public TO snow_read;")
    DBI::dbExecute(con, "GRANT SELECT ON means to snow_read;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO snow_read;")
  }, error = function(e) {
    warning("Not able to create a new read only account with name snow_read. Ignore this message if it already exists (this function would not have erased the old account)")
  })

}

