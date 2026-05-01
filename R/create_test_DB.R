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
#' @param continuous_locations Vector of location IDs, location codes, or aliases to retain in continuous tables. Defaults to selected long-lived hydrometric and meteorological locations when `NULL`.
#' @param discrete_locations Vector of location IDs, location codes, or aliases to retain in discrete tables. Defaults to selected snow-course locations when `NULL`.
#' @param start_datetime The start datetime for the data to be copied.  If `NULL`, data is copied from the beginning of records to `end_datetime` for the affected locations. This does not apply to the 'measurements_calculated_daily' table, which is always copied from the beginning of records - this is to enable historic range and return period calculations to be reproduced.
#' @param end_datetime The end datetime for the data to be copied.  If `NULL`, all data is copied from `start_datetime` to the end of records for the affected locations. This **does** apply to the 'measurements_calculated_daily' table.
#' @param delete If TRUE, will delete the test_temp database after the function is done. If FALSE, the database will remain for further testing.
#'
#' @seealso [db_dump()]
#'
#' @returns An SQL file containing the schema definition saved to the 'outfile' path.
#' @export
#'

create_test_db <- function(
  name = "aquacache",
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
  start_datetime = "2020-01-01 00:00",
  end_datetime = "2024-01-01 00:00",
  delete = TRUE
) {
  # Quick parameter setting for testing
  # name <- "aquacache"
  # host <- Sys.getenv("aquacacheHost")
  # port <- Sys.getenv("aquacachePort")
  # username <- "postgres"
  # replace <- TRUE
  # password <- Sys.getenv("aquacacheAdminPass")
  # outpath <- testthat::test_path("fixtures")
  # pg_dump <- "C:/Program Files\\PostgreSQL\\18\\bin\\pg_dump.exe"
  # psql <- "C:\\Program Files\\PostgreSQL\\18\\bin\\psql.exe"
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
    message(
      "Select the path to the folder where you want the zipped SQL file saved."
    )
    outpath <- rstudioapi::selectDirectory(
      caption = "Select Save Folder",
      path = file.path(Sys.getenv("USERPROFILE"), "Desktop")
    )
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
  con <- AquaConnect(
    name = name,
    host = host,
    port = port,
    username = username,
    password = password,
    silent = TRUE
  )

  # Create an empty database on the connected PostgreSQL server
  tryCatch(
    {
      # Check if the test_temp database exists already
      existing_dbs <- DBI::dbGetQuery(con, "SELECT datname FROM pg_database;")
      if ("test_temp" %in% existing_dbs$datname) {
        # Ask the user if they want to delete and replace the existing test_temp database
        message(
          "The test_temp database already exists. Do you want to delete it and create a new one? (yes/no)"
        )
        user_input <- readline(
          prompt = "Type 'yes' to delete the existing database: "
        )
        if (tolower(user_input) != "yes") {
          stop(
            "Exiting without creating a new test_temp database. Please delete the existing database manually if you want to proceed."
          )
        } else {
          # Delete the existing test_temp database
          DBI::dbExecute(con, "DROP DATABASE IF EXISTS test_temp;")
        }
      }
      DBI::dbExecute(con, "CREATE DATABASE test_temp;")
    },
    error = function(e) {
      stop(
        "Failed to create test_temp database. Ensure you have the necessary permissions to create a new database on the server (likely logged in as postgres)."
      )
    }
  )

  # Connect to the newly created test database
  test_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "test_temp",
    host = host,
    port = port,
    user = username,
    password = password
  )

  # delete the test_temp database after the function is done
  on.exit(
    {
      DBI::dbDisconnect(test_con)
      if (delete) {
        DBI::dbExecute(con, "DROP DATABASE IF EXISTS test_temp;")
      } else {
        NULL
      }
    },
    add = TRUE
  )

  # pg_dump from the original database to the test database without data
  Sys.setenv(PGPASSWORD = password) # Pass the password securely

  # Create the pg_dump command for schema/data
  schema_outfile <- file.path(tempdir(), paste0(name, "_schema_dump.sql"))
  schema_outfile <- normalizePath(schema_outfile, mustWork = FALSE)
  dump_args <- c(
    "-s", # schema only
    "--no-owner", # do not include commands to set ownership of objects to the original owner
    "--no-acl", # do not include commands to set access privileges (grant/revoke) of objects
    "-U",
    username,
    "-h",
    host,
    "-p",
    port,
    "-f",
    shQuote(schema_outfile),
    name # the database to dump
  )

  # Run pg_dump to create the schema dump
  dump_out <- system2(
    command = pg_dump,
    args = dump_args,
    stdout = TRUE,
    stderr = TRUE
  )

  # Check if pg_dump was successful
  exit_status <- attr(dump_out, "status")
  if (is.null(exit_status)) {
    exit_status <- 0
  }
  if (exit_status != 0) {
    stop(
      "pg_dump failed (exit ",
      exit_status,
      ")\n",
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
  if (
    DBI::dbGetQuery(
      test_con,
      "SELECT COUNT(*) FROM information_schema.tables WHERE table_schema = 'public';"
    )[1, 1] ==
      0
  ) {
    stop(
      "Failed to load schema into the test database. Please check your database connection and credentials."
    )
  }

  message("Schema loaded into test database successfully.")

  # Set the timezone to UTC for test_con (AquaConnect already sets it for the main connection)
  DBI::dbExecute(test_con, "SET timezone = 'UTC'")

  db_search_path <- paste(
    c(
      "public",
      "continuous",
      "discrete",
      "spatial",
      "files",
      "instruments",
      "boreholes",
      "information",
      "application",
      "audit",
      "field"
    ),
    collapse = ", "
  )

  # Set the search path (takes effect at next connection)
  DBI::dbExecute(
    test_con,
    sprintf("ALTER DATABASE test_temp SET search_path TO %s;", db_search_path)
  )
  # Update the search path for this session
  DBI::dbExecute(
    test_con,
    sprintf("SET search_path TO %s;", db_search_path)
  )
  DBI::dbExecute(test_con, "SET continuous.skip_daily_refresh = 'on';")

  quote_sql <- function(x) {
    as.character(DBI::dbQuoteString(con, as.character(x)))
  }

  id_csv <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0) {
      return(NULL)
    }
    paste(as.integer(x), collapse = ",")
  }

  value_csv <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 0) {
      return(NULL)
    }
    paste(quote_sql(x), collapse = ",")
  }

  empty_table <- function(tbl) {
    DBI::dbGetQuery(con, paste0("SELECT * FROM ", tbl, " WHERE FALSE"))
  }

  append_if_rows <- function(tbl, data, label = tbl) {
    if (nrow(data) == 0) {
      return(invisible(FALSE))
    }
    message("Loading table ", label, " into the test database...")
    DBI::dbAppendTable(test_con, DBI::SQL(tbl), data)
    invisible(TRUE)
  }

  table_exists <- function(schema, table) {
    DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table))
  }

  resolve_location_ids <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(integer())
    }
    x <- unique(x[!is.na(x)])
    numeric_ids <- suppressWarnings(as.integer(x))
    numeric_ids <- unique(numeric_ids[!is.na(numeric_ids)])

    ids <- integer()
    if (length(numeric_ids) > 0) {
      ids <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT location_id
           FROM public.locations
           WHERE location_id IN (%s)",
          id_csv(numeric_ids)
        )
      )$location_id
    }

    text_values <- as.character(x)
    text_values <- text_values[!text_values %in% as.character(numeric_ids)]
    if (length(text_values) > 0) {
      text_ids <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT location_id
           FROM public.locations
           WHERE location_code IN (%s)
              OR alias IN (%s)",
          value_csv(text_values),
          value_csv(text_values)
        )
      )$location_id
      ids <- c(ids, text_ids)
    }

    unique(as.integer(ids))
  }

  message("Loading ancillary tables...")
  full_tbls <- c(
    "continuous.aggregation_types",
    "discrete.collection_methods",
    "discrete.protocols_methods",
    "discrete.result_conditions",
    "discrete.result_speciations",
    "discrete.result_types",
    "discrete.result_value_types",
    "discrete.sample_fractions",
    "discrete.sample_types",
    "discrete.laboratories",
    "files.document_types",
    "information.internal_status",
    "information.version_info",
    "instruments.communication_protocol_families",
    "instruments.communication_protocols",
    "instruments.instrument_type",
    "instruments.instrument_make",
    "instruments.instrument_model",
    "instruments.observers",
    "instruments.sensor_types",
    "instruments.suppliers",
    "instruments.transmission_component_roles",
    "instruments.transmission_method_families",
    "instruments.transmission_methods",
    "public.approval_types",
    "continuous.correction_types",
    "continuous.timeseries_types",
    "public.datum_list",
    "public.grade_types",
    "public.location_types",
    "public.matrix_states",
    "public.media_types",
    "public.network_project_types",
    "public.units",
    "public.unit_conversions",
    "public.parameter_groups",
    "public.parameter_sub_groups",
    "public.parameters",
    "public.parameter_relationships",
    "public.qualifier_types",
    "public.organizations",
    "public.languages",
    "spatial.raster_types"
  )

  # Load the ancillary tables into the test database using DBI
  for (tbl in full_tbls) {
    # Read the table from the original database
    data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", tbl))

    # Write the table to the test database
    append_if_rows(tbl, data)
  }

  message("\nLoading subsets of data into the test database...")
  if (is.null(continuous_locations)) {
    # Find the locations for the Liard River at upper crossing and Marsh Lake, plus Tagish meteorological
    continuous_location_ids <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT t.location_id
       FROM continuous.timeseries t
       JOIN public.locations l
         ON t.location_id = l.location_id
       WHERE l.location_code IN ('09EA004', '09AB004', '09AA-M1', '48168')
          OR l.alias IN ('09EA004', '09AB004', '09AA-M1', '48168')"
    )$location_id
  } else {
    continuous_location_ids <- resolve_location_ids(continuous_locations)
  }

  if (is.null(discrete_locations)) {
    discrete_location_ids <- c(46L, 44L) # Tagish and Log Cabin snow courses
  } else {
    discrete_location_ids <- resolve_location_ids(discrete_locations)
  }

  if (is.null(start_datetime)) {
    start_datetime <- "1800-01-01 00:00"
  }

  if (is.null(end_datetime)) {
    end_datetime <- Sys.time()
  }

  if (length(continuous_location_ids) > 0) {
    ts <- DBI::dbGetQuery(
      con,
      sprintf(
        "WITH RECURSIVE deps(timeseries_id) AS (
           SELECT t.timeseries_id
           FROM continuous.timeseries t
           WHERE t.location_id IN (%s)

           UNION

           SELECT m.member_timeseries_id
           FROM deps d
           JOIN continuous.timeseries_compound_members m
             ON m.timeseries_id = d.timeseries_id
         )
         SELECT DISTINCT t.*
         FROM continuous.timeseries t
         JOIN deps d
           ON d.timeseries_id = t.timeseries_id
         ORDER BY t.timeseries_id",
        id_csv(continuous_location_ids)
      )
    )
  } else {
    ts <- empty_table("continuous.timeseries")
  }

  if (length(discrete_location_ids) > 0) {
    samples <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT *
         FROM discrete.samples
         WHERE location_id IN (%s)
           AND datetime >= %s
           AND datetime <= %s",
        id_csv(discrete_location_ids),
        quote_sql(start_datetime),
        quote_sql(end_datetime)
      )
    )
  } else {
    samples <- empty_table("discrete.samples")
  }

  locations <- unique(c(
    continuous_location_ids,
    discrete_location_ids,
    if (nrow(ts) > 0) ts$location_id else integer()
  ))

  if (length(locations) == 0) {
    stop("No locations were resolved for the requested test database subset.")
  }

  # locations table
  locs <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM public.locations WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )

  # Drainage basins are no longer referenced directly from locations.
  vect <- if (nrow(locs) > 0) {
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT *
         FROM spatial.vectors
         WHERE feature_name IN (%s)
           AND layer_name = 'Drainage_basins'",
        value_csv(locs$location_code)
      )
    )
  } else {
    empty_table("spatial.vectors")
  }
  append_if_rows("spatial.vectors", vect)
  append_if_rows("public.locations", locs)

  sub_locations <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM public.sub_locations WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )
  append_if_rows("public.sub_locations", sub_locations)

  loc_z <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM public.locations_z WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )
  append_if_rows("public.locations_z", loc_z)

  # Select entries in locations_networks and locations_projects that match the selected locations
  loc_networks <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM public.locations_networks WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )
  loc_projects <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM public.locations_projects WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )
  # Select and append the relevant networks and projects
  if (nrow(loc_networks) > 0) {
    networks <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT * FROM public.networks WHERE network_id IN (%s)",
        id_csv(loc_networks$network_id)
      )
    )
    append_if_rows("public.networks", networks)
    append_if_rows("public.locations_networks", loc_networks)
  }
  if (nrow(loc_projects) > 0) {
    projects <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT * FROM public.projects WHERE project_id IN (%s)",
        id_csv(loc_projects$project_id)
      )
    )
    append_if_rows("public.projects", projects)
    append_if_rows("public.locations_projects", loc_projects)
  }

  # datum conversions (applies to discrete or continuous)
  dc <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM public.datum_conversions WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )
  append_if_rows("public.datum_conversions", dc)

  ts_ids <- if (nrow(ts) > 0) {
    id_csv(ts$timeseries_id)
  } else {
    NULL
  }

  agreements <- if (!is.null(ts_ids)) {
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT *
         FROM continuous.timeseries_data_sharing_agreements
         WHERE timeseries_id IN (%s)",
        ts_ids
      )
    )
  } else {
    empty_table("continuous.timeseries_data_sharing_agreements")
  }

  document_ids <- integer()
  if (
    nrow(ts) > 0 &&
      "default_data_sharing_agreement_id" %in% names(ts)
  ) {
    document_ids <- c(document_ids, ts$default_data_sharing_agreement_id)
  }
  if (nrow(agreements) > 0) {
    document_ids <- c(document_ids, agreements$data_sharing_agreement_id)
  }
  if (
    nrow(samples) > 0 &&
      "data_sharing_agreement_id" %in% names(samples)
  ) {
    document_ids <- c(document_ids, samples$data_sharing_agreement_id)
  }
  document_ids <- unique(document_ids[!is.na(document_ids)])

  if (length(document_ids) > 0) {
    documents <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT *
         FROM files.documents
         WHERE document_id IN (%s)",
        id_csv(document_ids)
      )
    )
    append_if_rows("files.documents", documents)
  }

  location_names <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT *
       FROM public.location_names
       WHERE location_id IN (%s)",
      id_csv(locations)
    )
  )
  append_if_rows("public.location_names", location_names)

  # Continuous data --------------------------------------------------------
  if (length(continuous_location_ids) > 0) {
    if (nrow(ts) > 0) {
      append_if_rows("continuous.timeseries", ts)

      compounds <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT *
           FROM continuous.timeseries_compounds
           WHERE timeseries_id IN (%s)",
          ts_ids
        )
      )
      append_if_rows("continuous.timeseries_compounds", compounds)

      compound_members <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT *
           FROM continuous.timeseries_compound_members
           WHERE timeseries_id IN (%s)",
          ts_ids
        )
      )
      append_if_rows(
        "continuous.timeseries_compound_members",
        compound_members
      )

      g <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT * FROM continuous.grades WHERE timeseries_id IN (",
          ts_ids,
          ")"
        )
      )
      append_if_rows("continuous.grades", g)

      a <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT * FROM continuous.approvals WHERE timeseries_id IN (",
          ts_ids,
          ")"
        )
      )
      append_if_rows("continuous.approvals", a)

      q <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT * FROM continuous.qualifiers WHERE timeseries_id IN (",
          ts_ids,
          ")"
        )
      )
      append_if_rows("continuous.qualifiers", q)

      owners <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT * FROM continuous.owners WHERE timeseries_id IN (",
          ts_ids,
          ")"
        )
      )
      append_if_rows("continuous.owners", owners)

      contributors <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT * FROM continuous.contributors WHERE timeseries_id IN (",
          ts_ids,
          ")"
        )
      )
      append_if_rows("continuous.contributors", contributors)

      mc <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT *
           FROM continuous.measurements_continuous
           WHERE timeseries_id IN (%s)
             AND datetime >= %s
             AND datetime <= %s",
          ts_ids,
          quote_sql(start_datetime),
          quote_sql(end_datetime)
        )
      )
      append_if_rows("continuous.measurements_continuous", mc)

      md <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT *
           FROM continuous.measurements_calculated_daily
           WHERE timeseries_id IN (%s)
             AND date <= %s",
          ts_ids,
          quote_sql(as.Date(end_datetime))
        )
      )
      append_if_rows("continuous.measurements_calculated_daily", md)

      corr <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT * FROM continuous.corrections WHERE timeseries_id IN (%s)",
          ts_ids
        )
      )
      append_if_rows("continuous.corrections", corr)

      append_if_rows(
        "continuous.timeseries_data_sharing_agreements",
        agreements
      )

      if (
        DBI::dbExistsTable(
          con,
          DBI::Id(schema = "public", table = "locations_metadata_instruments")
        )
      ) {
        lmi <- DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT *
             FROM public.locations_metadata_instruments
             WHERE location_id IN (%s)
               AND (
                 timeseries_id IS NULL OR
                 timeseries_id IN (%s)
               )",
            id_csv(locations),
            ts_ids
          )
        )
        if (nrow(lmi) > 0) {
          metadata_ids <- id_csv(lmi$metadata_id)

          if (any(!is.na(lmi$instrument_id))) {
            lmi_instruments <- DBI::dbGetQuery(
              con,
              sprintf(
                "SELECT *
                 FROM instruments.instruments
                 WHERE instrument_id IN (%s)",
                id_csv(lmi$instrument_id)
              )
            )
            append_if_rows("instruments.instruments", lmi_instruments)
          }

          append_if_rows("public.locations_metadata_instruments", lmi)

          if (
            DBI::dbExistsTable(
              con,
              DBI::Id(
                schema = "public",
                table = "locations_metadata_instrument_connections"
              )
            )
          ) {
            connections <- DBI::dbGetQuery(
              con,
              sprintf(
                "SELECT *
                 FROM public.locations_metadata_instrument_connections
                 WHERE instrument_metadata_id IN (%s)
                    OR logger_metadata_id IN (%s)",
                metadata_ids,
                metadata_ids
              )
            )
            if (nrow(connections) > 0) {
              append_if_rows(
                "public.locations_metadata_instrument_connections",
                connections
              )

              connection_ids <- id_csv(connections$connection_id)
              if (
                DBI::dbExistsTable(
                  con,
                  DBI::Id(
                    schema = "public",
                    table = "locations_metadata_instrument_connection_signals"
                  )
                )
              ) {
                connection_signals <- DBI::dbGetQuery(
                  con,
                  sprintf(
                    "SELECT *
                     FROM public.locations_metadata_instrument_connection_signals
                     WHERE connection_id IN (%s)
                       AND (
                         timeseries_id IS NULL OR
                         timeseries_id IN (%s)
                       )",
                    connection_ids,
                    ts_ids
                  )
                )
                append_if_rows(
                  "public.locations_metadata_instrument_connection_signals",
                  connection_signals
                )
              }
            }
          }

          if (
            DBI::dbExistsTable(
              con,
              DBI::Id(
                schema = "public",
                table = "locations_metadata_transmission_setups"
              )
            )
          ) {
            transmission_setups <- DBI::dbGetQuery(
              con,
              sprintf(
                "SELECT *
                 FROM public.locations_metadata_transmission_setups
                 WHERE logger_metadata_id IN (%s)",
                metadata_ids
              )
            )
            if (nrow(transmission_setups) > 0) {
              append_if_rows(
                "public.locations_metadata_transmission_setups",
                transmission_setups
              )

              setup_ids <- id_csv(transmission_setups$transmission_setup_id)
              if (
                DBI::dbExistsTable(
                  con,
                  DBI::Id(
                    schema = "public",
                    table = "locations_metadata_transmission_routes"
                  )
                )
              ) {
                transmission_routes <- DBI::dbGetQuery(
                  con,
                  sprintf(
                    "SELECT *
                     FROM public.locations_metadata_transmission_routes
                     WHERE transmission_setup_id IN (%s)",
                    setup_ids
                  )
                )
                append_if_rows(
                  "public.locations_metadata_transmission_routes",
                  transmission_routes
                )
              }

              if (
                DBI::dbExistsTable(
                  con,
                  DBI::Id(
                    schema = "public",
                    table = "locations_metadata_transmission_components"
                  )
                )
              ) {
                transmission_components <- DBI::dbGetQuery(
                  con,
                  sprintf(
                    "SELECT *
                     FROM public.locations_metadata_transmission_components
                     WHERE transmission_setup_id IN (%s)",
                    setup_ids
                  )
                )
                append_if_rows(
                  "public.locations_metadata_transmission_components",
                  transmission_components
                )
              }
            }
          }
        }
      }
    } else {
      warning("No continuous timeseries found for the specified locations.")
    }
  }

  # Discrete data ---------------------------------------------------------
  if (length(discrete_location_ids) > 0) {
    ss <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT *
         FROM discrete.sample_series
         WHERE location_id IN (%s)",
        id_csv(discrete_location_ids)
      )
    )
    append_if_rows("discrete.sample_series", ss)

    if (nrow(samples) > 0) {
      if (
        "field_visit_id" %in%
          names(samples) &&
          any(!is.na(samples$field_visit_id)) &&
          table_exists("field", "field_visits")
      ) {
        field_visits <- DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT *
             FROM field.field_visits
             WHERE field_visit_id IN (%s)",
            id_csv(samples$field_visit_id)
          )
        )
        append_if_rows("field.field_visits", field_visits)
      }

      append_if_rows("discrete.samples", samples)
      sample_ids <- id_csv(samples$sample_id)
      results <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT *
           FROM discrete.results
           WHERE sample_id IN (%s)",
          sample_ids
        )
      )
      append_if_rows("discrete.results", results)
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
    "--no-owner", # do not include commands to set ownership of objects to the original owner
    "--no-acl", # do not include commands to set access privileges (grant/revoke
    "-U",
    username,
    "-h",
    host,
    "-p",
    port,
    "-f",
    shQuote(outpath),
    "-d test_temp"
  )

  message("\nCreating final schema and data dump...")
  # Run pg_dump to create the schema dump
  dump_out <- system2(
    command = pg_dump,
    args = dump_args,
    stdout = TRUE,
    stderr = TRUE
  )

  # Check if pg_dump was successful
  exit_status <- attr(dump_out, "status")
  if (is.null(exit_status)) {
    exit_status <- 0
  }
  if (exit_status != 0) {
    stop(
      "pg_dump failed (exit ",
      exit_status,
      ")\n",
      paste(dump_out, collapse = "\n")
    )
  }

  # sanity‑check that the file exists & is non‑empty
  if (!file.exists(outpath) || file.info(outpath)$size == 0) {
    stop("schema dump did not produce a valid file at ", outpath)
  }

  # append statement to set search_path on restore
  alter_stmt <- sprintf(
    "-- ensure search_path is set when restoring the database
DO $$
BEGIN
  EXECUTE format('ALTER DATABASE %%I SET search_path TO %s;', current_database());
END$$;",
    db_search_path
  )
  cat(alter_stmt, file = outpath, append = TRUE)

  # gz the output file
  schema_outfile <- paste0(outpath, ".gz")
  if (replace && file.exists(schema_outfile)) {
    warning(
      "The gzipped output file already exists at ",
      schema_outfile,
      ". It will be overwritten."
    )
    file.remove(schema_outfile) # Remove the .gz file if it exists and replace is TRUE
  }

  R.utils::gzip(outpath, destname = schema_outfile, remove = TRUE)

  message("Test database .sql dump created successfully at ", schema_outfile)

  # Clear the password environment variable
  Sys.unsetenv("PGPASSWORD")

  # Return paths of the output file
  return(schema_outfile)
}
