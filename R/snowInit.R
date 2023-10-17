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
#'

#TODO: deal with geometry of polygon fields of basin and sub-basin

# snowInit(snowConnect_pg(name = "snowDB", host = "localHost", port = "5432", username = "postgres", password = ?, silent = FALSE), overwrite = TRUE)

snowInit <- function(con = snowConnect_pg(), overwrite = FALSE) {


  if (overwrite){
    for (i in DBI::dbListTables(con)){
      DBI::dbExecute(con, paste0("DROP TABLE ", i, " CASCADE"))
    }
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(con, "VACUUM")
  }

  # Create tables
  # basins
  DBI::dbExecute(con, "CREATE TABLE if not exists basins (
                 basin TEXT PRIMARY KEY,
                 polygon POLYGON)"
  )

  # sub_basins
  DBI::dbExecute(con, "CREATE TABLE if not exists sub_basins (
                 sub_basin TEXT PRIMARY KEY,
                 polygon POLYGON)"
  )

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
  DBI::dbExecute(con, "CREATE TABLE if not exists survey (
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
                 notes TEXT,
                 CONSTRAINT survey_sample_time UNIQUE (survey_id, sample_datetime),

                 FOREIGN KEY (survey_id) REFERENCES survey(survey_id))"
  )

  # Create a read-only account
  tryCatch({
    DBI::dbExecute(con, "CREATE ROLE snow_read WITH LOGIN PASSWORD 'snow';")
    DBI::dbExecute(con, "GRANT CONNECT ON DATABASE snowDB TO snow_read;")
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA public TO snow_read;")
    DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA public TO snow_read;")
  }, error = function(e) {
    warning("Not able to create a new read only account with name snow_read. Ignore this message if it already exists (this function would not have erased the old account)")
  })

}

