#' Initial PostgreSQL snow database creation.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Creates a PostgreSQL database or replaces an existing database. Established pre-set table structure. All tables are created and with primary keys.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [snowConnect_pg()].
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

  # if (overwrite){
  #   for (i in DBI::dbListTables(con)){
  #     DBI::dbExecute(con, paste0("DROP TABLE ", i, " CASCADE"))
  #   }
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(con, "VACUUM")
  }

  rpostgis::pgPostGIS(con, raster = TRUE)

  # Create tables
  ### Basin  with geom test
  # Create spatial-primary tables ########
  # DBI::dbExecute(con, "CREATE TABLE if not exists basins (
  #                basin TEXT PRIMARY KEY,
  #                polygon geometry(Polygon, 4269) NOT NULL,
  #                CONSTRAINT enforce_dims_geom CHECK (st_ndims(polygon) = 2),
  #                CONSTRAINT enforce_geotype_geom CHECK (geometrytype(polygon) = 'POLYGON'::text),
  #                CONSTRAINT enforce_srid_geom CHECK (st_srid(polygon) = 4269),
  #                CONSTRAINT enforce_valid_geom CHECK (st_isvalid(polygon)),
  #                UNIQUE (basin, polygon));")
  # DBI::dbExecute(con, "CREATE INDEX polygons_idx ON basins USING GIST (polygon);") #Forces use of GIST indexing which is necessary for large polygons
  # ###

  # basins
  DBI::dbExecute(con, "CREATE TABLE if not exists basins (
                 basin TEXT PRIMARY KEY,
                 polygon POLYGON)"
  )

  DBI::dbExecute(con, "CREATE TABLE if not exists sub_basins (
                 sub_basin TEXT PRIMARY KEY,
                 polygon geometry(Polygon, 4269) NOT NULL,
                 CONSTRAINT enforce_dims_geom2 CHECK (st_ndims(polygon) = 2),
                 CONSTRAINT enforce_geotype_geom2 CHECK (geometrytype(polygon) = 'POLYGON'::text),
                 CONSTRAINT enforce_srid_geom2 CHECK (st_srid(polygon) = 4269),
                 CONSTRAINT enforce_valid_geom2 CHECK (st_isvalid(polygon)))")#,
                 #UNIQUE (sub_basin, polygon));")
  #DBI::dbExecute(con, "CREATE INDEX polygons_idx2 ON sub_basins USING GIST (polygon);") #Forces use of GIST indexing which is necessary for large polygons

  # # sub_basins
  # DBI::dbExecute(con, "CREATE TABLE if not exists sub_basins (
  #                sub_basin TEXT PRIMARY KEY,
  #                polygon POLYGON)"
  # )

  # locations
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

  # maintenance
  DBI::dbExecute(con, "CREATE TABLE if not exists maintenance (
                 maintenance_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 date DATE NOT NULL,
                 maintenance TEXT NOT NULL,
                 completed BOOLEAN NOT NULL,

                 FOREIGN KEY (location) REFERENCES locations(location))"
  )

  # survey
  DBI::dbExecute(con, "CREATE TABLE if not exists surveys (
                 survey_id SERIAL PRIMARY KEY,
                 location TEXT NOT NULL,
                 target_date DATE NOT NULL,
                 survey_date DATE NOT NULL,
                 notes TEXT,
                 CONSTRAINT survey_loc UNIQUE (survey_date, location),

                 FOREIGN KEY (location) REFERENCES locations(location))"
  )

  # measurements
  DBI::dbExecute(con, "CREATE TABLE if not exists measurements (
                 measurement_id SERIAL PRIMARY KEY,
                 survey_id INTEGER NOT NULL,
                 sample_datetime TIMESTAMP NOT NULL,
                 sampler_name TEXT,
                 estimate_flag BOOLEAN NOT NULL,
                 exclude_flag BOOLEAN NOT NULL,
                 SWE NUMERIC,
                 depth NUMERIC,
                 average BOOLEAN,
                 notes TEXT,

                 FOREIGN KEY (survey_id) REFERENCES surveys(survey_id))"
  )
#                 CONSTRAINT survey_sample_time UNIQUE (survey_id, sample_datetime),

  DBI::dbExecute(con, paste0("CREATE VIEW means AS ",
                             "WITH measurement_counts AS (",
                             "  SELECT survey_id, COUNT(*) AS total_count ",
                             "  FROM measurements ",
                             "  GROUP BY survey_id",
                             ")",
                             "SELECT surveys.location, measurements.survey_id, ",
                             "AVG(swe) AS swe, AVG(depth) AS depth, MIN(sample_datetime) AS sample_datetime, ",
                             "STDDEV(swe) AS swe_sd, STDDEV(depth) AS depth_sd, ",
                             "COUNT(*) AS sample_count_used, ",
                             "total_count - COUNT(*) AS sample_count_ex, ",
                             "BOOL_OR(measurements.estimate_flag) AS estimate_flag ",
                             "FROM measurements ",
                             "INNER JOIN surveys ON measurements.survey_id = surveys.survey_id ",
                             "LEFT JOIN measurement_counts ON measurements.survey_id = measurement_counts.survey_id ",
                             "WHERE exclude_flag = FALSE ",
                             "GROUP BY measurements.survey_id, surveys.location, total_count"
  ))

  # Create a read-only account
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

