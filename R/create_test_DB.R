#' Create a small test database
#' 
#' This function uses the `pg_dump` utility to create a dump of the schema and a subset of the data in an aquacache PostgreSQL database. The schema dump is saved to an SQL file in the specified output path. The resulting SQL includes a command to set the database `search_path` on restore so that queries without schema qualifiers work. You must call this function from a machine with the PostgreSQL pg_dump utility installed.
#'
#' @param name Target database name (i.e. the one to be dumped). By default, it is set to "aquacache". If you want to dump a different database, specify its name here.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form aquacachePort="1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminUser="username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminPass="password".
#' @param outpath The path to the folder/location where the .sql dump will be saved. If "choose", the user will be prompted to select a folder. Default is this package's tests/testthat/fixtures folder.
#' @param replace If TRUE and a file of name testdb.sql.gz exists in `outpath` will attempt to replaced the file.
#' @param pg_dump The path to the pg_dump utility. By default (NULL), the function searches the PATH for the utility, but this might not always work.
#' @param psql The path to the psql utility. By default (NULL), the function searches the PATH for the utility, but this might not always work.
#' @param continuous_locations Character vector of location codes to retain in continuous tables. Defaults to the first location found in the database when `NULL`.
#' @param discrete_locations Character vector of location codes to retain in discrete tables.  Defaults to `continuous_locations` when `NULL`.
#' @param start_datetime The start datetime for the data to be copied.  If `NULL`, data is copied from the beginning of records to `end_datetime` for the affected locations. This does not apply to the 'measurements_calculated_daily' table, which is always copied from the beginning of records.
#' @param end_datetime The end datetime for the data to be copied.  If `NULL`, all data is copied from `start_datetime` to the end of records for the affected locations. This **does** apply to the 'measurements_calculated_daily' table.
#' @param delete If TRUE, will delete the test_temp database after the function is done. If FALSE, the database will remain for further testing.
#'
#' @returns An SQL file containing the schema definition saved to the 'outfile' path.
#' @export
#'

create_test_db <- function(name = "aquacache", 
                           host = Sys.getenv("aquacacheHost"), 
                           port = Sys.getenv("aquacachePort"), 
                           username = Sys.getenv("aquacacheAdminUser"), 
                           password = Sys.getenv("aquacacheAdminPass"), 
                           outpath = testthat::test_path("fixtures"),
                           replace = TRUE,
                           pg_dump = NULL, 
                           psql = NULL, 
                           continuous_locations = NULL, 
                           discrete_locations = NULL, 
                           start_datetime = "2015-12-31 00:00",
                           end_datetime = "2023-01-02 00:00",
                           delete = TRUE) {
  
  # Quick parameter setting for testing
  # name <- "aquacache"
  # host <- Sys.getenv("aquacacheHost")
  # port <- Sys.getenv("aquacachePort")
  # username <- "postgres"
  # password <- Sys.getenv("aquacacheAdminPass")
  # outpath <- "choose"
  # pg_dump <- "C:/Program Files\\PostgreSQL\\17\\bin\\pg_dump.exe"
  # psql <- "C:\\Program Files\\PostgreSQL\\17\\bin\\psql.exe"
  # continuous_locations <- NULL
  # discrete_locations <- NULL
  # start_datetime <- "2020-01-01 00:00"
  # end_datetime <- "2024-01-01 00:00"
  
  
  rlang::check_installed("R.utils", reason = "to gzip the output file")
  
  # Check or prompt for output path
  if (outpath == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want the zipped SQL file saved.")
    outpath <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"), "Desktop"))
  }
  
  # Validate the `pg_dump` utility
  if (is.null(pg_dump)) {
    pg_dump <- Sys.which("pg_dump")
  }
  if (!file.exists(pg_dump)) {
    stop("The specified pg_dump utility does not exist or could not be found.")
  }
  
  # Validate the `psql` utility
  if (is.null(psql)) {
    psql <- Sys.which("psql")
  }
  if (!file.exists(psql)) {
    stop("The specified psql utility does not exist or could not be found.")
  }
  
  # Connect to the PostgreSQL database using AquaConnect
  con <- AquaConnect(name = name, host = host, port = port, username = username, password = password, silent = TRUE)
  
  # Create an empty database on the connected PostgreSQL server
  tryCatch({
    # Check if the test_temp database exists already
    existing_dbs <- DBI::dbGetQuery(con, "SELECT datname FROM pg_database;")
    if ("test_temp" %in% existing_dbs$datname) {
      # Ask the user if they want to delete and replace the existing test_temp database
      message("The test_temp database already exists. Do you want to delete it and create a new one? (yes/no)")
      user_input <- readline(prompt = "Type 'yes' to delete the existing database: ")
      if (tolower(user_input) != "yes") {
        stop("Exiting without creating a new test_temp database. Please delete the existing database manually if you want to proceed.")
      } else {
        # Delete the existing test_temp database
        DBI::dbExecute(con, "DROP DATABASE IF EXISTS test_temp;")
      }
    }
    DBI::dbExecute(con, "CREATE DATABASE test_temp;")
  }, error = function(e) {
    stop("Failed to create test_temp database. Ensure you have the necessary permissions to create a new database on the server (likely logged in as postgres).")
  })
  
  # Connect to the newly created test database
  test_con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "test_temp", host = host, port = port, user = username, password = password)
  
  # delete the test_temp database after the function is done
  on.exit({
    DBI::dbDisconnect(test_con)
    if (delete) {DBI::dbExecute(con, "DROP DATABASE IF EXISTS test_temp;")} else NULL
    DBI::dbDisconnect(con)
  }, add = TRUE)
  
  # pg_dump from the original database to the test database without data
  Sys.setenv(PGPASSWORD = password) # Pass the password securely
  
  # Create the pg_dump command for schema/data
  schema_outfile <- file.path(tempdir(), paste0(name, "_schema_dump.sql"))
  schema_outfile <- normalizePath(schema_outfile, mustWork = FALSE)
  dump_args <- c(
    "-s",                 # schema only
    "-U", username,
    "-h", host,
    "-p", port,
    "-f", shQuote(schema_outfile),
    name                  # the database to dump
  )
  
  # Run pg_dump to create the schema dump
  dump_out <- system2(
    command = pg_dump,
    args    = dump_args,
    stdout  = TRUE,
    stderr  = TRUE
  )
  
  # Check if pg_dump was successful
  exit_status <- attr(dump_out, "status")
  if (is.null(exit_status)) exit_status <- 0
  if (exit_status != 0) {
    stop(
      "pg_dump failed (exit ", exit_status, ")\n",
      paste(dump_out, collapse = "\n")
    )
  }
  
  # sanity‑check that the file exists & is non‑empty
  if (!file.exists(schema_outfile) || file.info(schema_outfile)$size == 0) {
    stop("schema dump did not produce a valid file at ", schema_outfile)
  }
  
  # Load the schema dump into the test database using psql
  load_command <- sprintf(
    '"%s" -U %s -h %s -p %s -d test_temp -f "%s"',
    psql,
    username,
    host,
    port,
    schema_outfile
  )
  
  res <- system(load_command, intern = TRUE, ignore.stderr = FALSE)
  
  # Check if the schema was loaded successfully
  if (DBI::dbGetQuery(test_con, "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public';")[1,1] == 0) {
    stop("Failed to load schema into the test database. Please check your database connection and credentials.")
  }
  
  message("Schema loaded into test database successfully.")
  
  # Set the timezone to UTC for test_con (AquaConnect already sets it for the main connection)
  DBI::dbExecute(test_con, "SET timezone = 'UTC'")
  
  # Set the search path (takes effect at next connection)
  DBI::dbExecute(test_con, "ALTER DATABASE test_temp SET search_path TO public, continuous, discrete, spatial, files, instruments, information;")
  # Update the search path for this session
  DBI::dbExecute(test_con, "SET search_path TO public, continuous, discrete, spatial, files, instruments, information;")
  
  message("Loading ancillary tables...")
  full_tbls <- c("continuous.aggregation_types",
                 "discrete.collection_methods",
                 "discrete.protocols_methods",
                 "discrete.result_conditions",
                 "discrete.result_speciations",
                 "discrete.result_types",
                 "discrete.result_value_types",
                 "discrete.sample_fractions",
                 "discrete.sample_types",
                 "files.document_types",
                 "information.internal_status",
                 "information.version_info",
                 "public.approval_types",
                 "public.correction_types",
                 "public.datum_list",
                 "public.grade_types",
                 "public.location_types",
                 "public.media_types",
                 "public.network_project_types",
                 "public.parameters",
                 "public.parameter_groups",
                 "public.parameter_sub_groups",
                 "public.qualifier_types",
                 "public.organizations"
  )
  
  # Load the ancillary tables into the test database using DBI
  for (tbl in full_tbls) {
    message("Loading table ", tbl, " into the test database...")
    
    # Read the table from the original database
    query <- paste0("SELECT * FROM ", tbl, ";")
    data <- DBI::dbGetQuery(con, query)
    
    # Write the table to the test database
    DBI::dbAppendTable(test_con, DBI::SQL(tbl), data)
  }
  
  
  message("\nLoading subsets of data into the test database...")
  if (is.null(continuous_locations)) {
    # Find the locations for the Liard River at upper crossing and Marsh Lake, plus Tagish meteorological
    continuous_locations <- DBI::dbGetQuery(con, "SELECT DISTINCT(location_id) FROM timeseries WHERE location IN ('09EA004', '09AB004', '09AA-M1', '48168')")$location_id
  } 
  if (is.null(discrete_locations)) {
    discrete_locations <- c(46, 44)  # Tagish and Log cabin snow courses
  }
  
  locations <- unique(c(continuous_locations, discrete_locations))
  
  if (is.null(start_datetime)) {
    start_datetime <- "1800-01-01 00:00"
  }
  
  if (is.null(end_datetime)) {
    end_datetime <- Sys.time()
  }
  
  # locations table
  locs <- DBI::dbGetQuery(con, sprintf("SELECT * FROM locations WHERE location_id IN (%s)", paste(locations, collapse = ", ")))
  # Get the 'vectors' table entries referenced by column geom_id
  geom_ids <- unique(locs$geom_id)
  vect <- DBI::dbGetQuery(con, sprintf("SELECT * FROM vectors WHERE geom_id IN (%s)", paste(geom_ids, collapse = ", ")))
  # Append the vectors entries to the test database
  message("Loading table spatial.vectors into the test database...")
  DBI::dbAppendTable(test_con, DBI::SQL("spatial.vectors"), vect)
  message("Loading table public.locations into the test database...")
  DBI::dbAppendTable(test_con, "locations", locs)
  
  # Select entries in locations_networks and locations_projects that match the selected locations
  loc_networks <- DBI::dbGetQuery(con, sprintf("SELECT * FROM locations_networks WHERE location_id IN (%s)", paste(locations, collapse = ", ")))
  loc_projects <- DBI::dbGetQuery(con, sprintf("SELECT * FROM locations_projects WHERE location_id IN (%s)", paste(locations, collapse = ", ")))
  # Select and append the relevant networks and projects
  if (nrow(loc_networks) > 0) {
    networks <- DBI::dbGetQuery(con, sprintf("SELECT * FROM networks WHERE network_id IN (%s)", paste(unique(loc_networks$network_id), collapse = ", ")))
    message("Loading table public.networks into the test database...")
    DBI::dbAppendTable(test_con, DBI::SQL("public.networks"), networks)
    message("Loading table public.locations_networks into the test database...")
    DBI::dbAppendTable(test_con, DBI::SQL("public.locations_networks"), loc_networks)
  }
  if (nrow(loc_projects) > 0) {
    projects <- DBI::dbGetQuery(con, sprintf("SELECT * FROM projects WHERE project_id IN (%s)", paste(unique(loc_projects$project_id), collapse = ", ")))
    message("Loading table public.projects into the test database...")
    DBI::dbAppendTable(test_con, DBI::SQL("public.projects"), projects)
    message("Loading table public.locations_projects into the test database...")
    DBI::dbAppendTable(test_con, DBI::SQL("public.locations_projects"), loc_projects)
  }
  
  # datum conversions (applies to discrete or continuous)
  dc <- DBI::dbGetQuery(con, sprintf("SELECT * FROM datum_conversions WHERE location_id IN (%s)",
                                     paste(locations, collapse = ",")))
  message("Loading table public.datum_conversions into the test database...")
  DBI::dbAppendTable(test_con, "datum_conversions", dc)
  
  
  # Continuous data --------------------------------------------------------
  if (length(continuous_locations) > 0) {
    ts <- DBI::dbGetQuery(con, sprintf("SELECT * FROM timeseries WHERE location_id IN (%s)",
                                       paste(continuous_locations, collapse = ",")))
    if (nrow(ts) > 0) {
      message("Loading table continuous.timeseries into the test database")
      DBI::dbAppendTable(test_con, "timeseries", ts)
      
      ts_ids <- paste(ts$timeseries_id, collapse = ",")
      
      g <- DBI::dbGetQuery(con, paste0("SELECT * FROM grades WHERE timeseries_id IN (", ts_ids, ")"))
      message("Loading table continuous.grades into the test database")
      DBI::dbAppendTable(test_con, "grades", g)
      
      a <- DBI::dbGetQuery(con, paste0("SELECT * FROM approvals WHERE timeseries_id IN (", ts_ids, ")"))
      message("Loading table continuous.approvals into the test database")
      DBI::dbAppendTable(test_con, "approvals", a)
      
      q <- DBI::dbGetQuery(con, paste0("SELECT * FROM qualifiers WHERE timeseries_id IN (", ts_ids, ")"))
      message("Loading table continuous.qualifiers into the test database")
      DBI::dbAppendTable(test_con, "qualifiers", q)
      
      mc <- DBI::dbGetQuery(con,
                            sprintf("SELECT * FROM measurements_continuous WHERE timeseries_id IN (%s) AND datetime >= '%s' AND datetime <= '%s'",
                                    ts_ids, start_datetime, end_datetime))
      message("Loading table continuous.measurements_continuous into the test database")
      DBI::dbAppendTable(test_con, "measurements_continuous", mc)
      
      md <- DBI::dbGetQuery(con,
                            sprintf("SELECT * FROM measurements_calculated_daily WHERE timeseries_id IN (%s) AND date <= '%s'",
                                    ts_ids, end_datetime))
      message("Loading table continuous.measurements_calculated_daily into the test database")
      DBI::dbAppendTable(test_con, "measurements_calculated_daily", md)
      
      corr <- DBI::dbGetQuery(con,
                              sprintf("SELECT * FROM corrections WHERE timeseries_id IN (%s)",
                                      ts_ids))
      message("Loading table continuous.corrections into the test database")
      DBI::dbAppendTable(test_con, "corrections", corr)
      
    } else {
      warning("No continuous timeseries found for the specified locations.")
    }
  }
  
  # Discrete data ---------------------------------------------------------
  if (length(discrete_locations) > 0) {
    
    ss <- DBI::dbGetQuery(con, sprintf("SELECT * FROM sample_series WHERE location_id IN (%s)",
                                       paste(discrete_locations, collapse = ",")))
    message("Loading table discrete.sample_series into the test database")
    DBI::dbAppendTable(test_con, "sample_series", ss)
    
    samples <- DBI::dbGetQuery(con,
                               sprintf("SELECT * FROM samples WHERE location_id IN (%s) AND datetime >= '%s' AND datetime <= '%s'",
                                       paste(discrete_locations, collapse = ","), start_datetime, end_datetime))
    
    if (nrow(samples) > 0) {
      message("Loading table discrete.samples into the test database")
      DBI::dbAppendTable(test_con, "samples", samples)
      sample_ids <- paste(samples$sample_id, collapse = ",")
      results <- DBI::dbGetQuery(con,
                                 sprintf("SELECT * FROM results WHERE sample_id IN (%s)",
                                         sample_ids))
      message("Loading table discrete.results into the test database")
      DBI::dbAppendTable(test_con, "results", results)
    } else {
      warning("No discrete samples found for the specified locations.")
    }
  }
  
  
  
  # Do a final dump of the test database schema and data
  # Create the pg_dump command for schema/data
  outpath <- file.path(paste0(outpath, "/test_db.sql"))
  outpath <- normalizePath(outpath, mustWork = FALSE)
  
  if (replace && file.exists(outpath)) {
    file.remove(outpath)
  }
  
  dump_args <- c(
    "-U", username,
    "-h", host,
    "-p", port,
    "-f", shQuote(outpath),
    "-d test_temp"
  )
  
  # Run pg_dump to create the schema dump
  dump_out <- system2(
    command = pg_dump,
    args    = dump_args,
    stdout  = TRUE,
    stderr  = TRUE
  )
  
  # Check if pg_dump was successful
  exit_status <- attr(dump_out, "status")
  if (is.null(exit_status)) exit_status <- 0
  if (exit_status != 0) {
    stop(
      "pg_dump failed (exit ", exit_status, ")\n",
      paste(dump_out, collapse = "\n")
    )
  }
  
  # sanity‑check that the file exists & is non‑empty
  if (!file.exists(outpath) || file.info(outpath)$size == 0) {
    stop("schema dump did not produce a valid file at ", outpath)
  }
  
  # append statement to set search_path on restore
  alter_stmt <- paste(
    "-- ensure search_path is set when restoring the database",
    "DO $$",
    "BEGIN",
    "  EXECUTE format('ALTER DATABASE %I SET search_path TO public, continuous, discrete, spatial, files, instruments, information;', current_database());",
    "END$$;",
    sep = "\n"
  )
  cat(alter_stmt, file = outpath, append = TRUE)
  
  # gz the output file
  schema_outfile <- paste0(outpath, ".gz")
  if (replace && file.exists(schema_outfile)) {
    warning("The gzipped output file already exists at ", schema_outfile, ". It will be overwritten.")
    file.remove(schema_outfile)  # Remove the .gz file if it exists and replace is TRUE
  }
  
  R.utils::gzip(outpath, destname = schema_outfile, remove = TRUE)
  
  message("Test database .sql dump created successfully at ", schema_outfile)
  
  # Clear the password environment variable
  Sys.unsetenv("PGPASSWORD")
  
  # Return paths of the output file
  return(schema_outfile)
}
