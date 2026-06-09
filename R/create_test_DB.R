#' Create a small test database
#'
#' This function uses the `pg_dump` utility to create a dump of the schema and deterministic synthetic fixture data in an aquacache PostgreSQL database. The schema dump is saved to an SQL file in the specified output path. The resulting SQL includes AquaCache package/patch metadata, a bootstrap block for the `public_reader` login role using password `"aquacache"`, and a command to set the database `search_path` on restore so that queries without schema qualifiers work. You must call this function from a machine with the PostgreSQL pg_dump utility installed.
#'
#' @param name Target database name (i.e. the one to be dumped). By default, it is set to "aquacache". If you want to dump a different database, specify its name here.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form aquacachePort="1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminUser="username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminPass="password".
#' @param outpath The path to the folder/location where the .sql dump will be saved. If "choose", the user will be prompted to select a folder. Default is this package's tests/testthat/fixtures folder.
#' @param replace If TRUE and a file of name testdb.sql.gz exists in `outpath` will attempt to replaced the file.
#' @param pg_dump The path to the pg_dump utility. By default (NULL), the function searches the PATH for the utility and if that fails will look in normal default locations, but this might not always work.
#' @param psql The path to the psql utility. By default (NULL), the function searches the PATH for the utility and if that fails will look in normal default locations, but this might not always work.
#' @param delete If TRUE, will delete the testdb database after the function is done. If FALSE, the database will remain for further testing.
#'
#' @seealso [db_dump()]
#'
#' @returns Path to a gzipped SQL dump containing schema and deterministic fixture data.
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
  # delete = TRUE
  rlang::check_installed("R.utils", reason = "to gzip the output file")

  # Ensure that 'outpath' exists and is a directory
  if (!dir.exists(outpath)) {
    stop("The specified outpath does not exist or is not a directory.")
  }

  # Validate the `pg_dump` utility
  if (is.null(pg_dump)) {
    pg_dump <- find_postgres_utility("pg_dump")
  }
  if (!file.exists(pg_dump)) {
    stop("The specified pg_dump utility does not exist or could not be found.")
  }

  # Validate the `psql` utility
  if (is.null(psql)) {
    psql <- find_postgres_utility("psql")
  }
  if (!file.exists(psql)) {
    stop("The specified psql utility does not exist or could not be found.")
  }

  # Function to check for spatial extensions
  check_required_extensions <- function(
    con,
    required = c("postgis", "postgis_raster")
  ) {
    installed <- DBI::dbGetQuery(
      con,
      "SELECT extname FROM pg_extension"
    )$extname

    missing <- setdiff(required, installed)

    if (length(missing) > 0) {
      stop(
        "Database is missing required PostgreSQL extension(s): ",
        paste(missing, collapse = ", "),
        ". Install them before creating the test database."
      )
    }

    invisible(TRUE)
  }

  # Connect to the target PostgreSQL database using AquaConnect
  con <- AquaConnect(
    name = name,
    host = host,
    port = port,
    username = username,
    password = password,
    silent = TRUE,
    check = FALSE
  )

  # Create an empty database on the connected PostgreSQL server
  tryCatch(
    {
      # Check if the testdb database exists already
      existing_dbs <- DBI::dbGetQuery(con, "SELECT datname FROM pg_database;")
      if ("testdb" %in% existing_dbs$datname) {
        # Ask the user if they want to delete and replace the existing testdb database
        message(
          "The testdb database already exists. Do you want to delete it and create a new one? (yes/no)"
        )
        user_input <- readline(
          prompt = "Type 'yes' to delete the existing database: "
        )
        if (tolower(user_input) != "yes") {
          stop(
            "Exiting without creating a new testdb database. Please delete the existing database manually if you want to proceed."
          )
        } else {
          # Delete the existing testdb database
          DBI::dbExecute(con, "DROP DATABASE IF EXISTS testdb WITH (FORCE);")
        }
      }
      DBI::dbExecute(con, "CREATE DATABASE testdb;")
    },
    error = function(e) {
      stop(
        "Failed to create testdb database. Ensure you have the necessary permissions to create a new database on the server (likely logged in as postgres)."
      )
    }
  )

  # Connect to the newly created test database
  test_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "testdb",
    host = host,
    port = port,
    user = username,
    password = password
  )

  # delete the testdb database after the function is done and disconnect from both databases
  on.exit(
    {
      if (exists("test_con") && DBI::dbIsValid(test_con)) {
        try(DBI::dbDisconnect(test_con), silent = TRUE)
      }

      if (exists("con") && DBI::dbIsValid(con)) {
        if (delete) {
          try(
            DBI::dbExecute(con, "DROP DATABASE IF EXISTS testdb WITH (FORCE);"),
            silent = TRUE
          )
        }
        try(DBI::dbDisconnect(con), silent = TRUE)
      }
    },
    add = TRUE
  )

  # pg_dump from the original database to the test database without data
  Sys.setenv(PGPASSWORD = password) # Pass the password securely
  on.exit(
    # Clear the password environment variable
    Sys.unsetenv("PGPASSWORD"),
    add = TRUE
  )

  # Create the pg_dump command for schema/data
  schema_outfile <- file.path(tempdir(), paste0(name, "_schema_dump.sql"))
  schema_outfile <- normalizePath(schema_outfile, mustWork = FALSE)
  dump_args <- c(
    "-s", # schema only
    "--no-owner", # do not include commands to set ownership of objects to the original owner
    "--no-acl", # do not include commands to set access privileges (grant/revoke) of objects
    "--exclude-extension=pg_stat_statements",
    "-U",
    username,
    "-h",
    host,
    "-p",
    port,
    "-f",
    schema_outfile,
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

  # sanity check that the file exists & is non empty
  if (!file.exists(schema_outfile) || file.info(schema_outfile)$size == 0) {
    stop("schema dump did not produce a valid file at ", schema_outfile)
  }

  # Load the schema dump into the test database using psql
  load_out <- system2(
    command = psql,
    args = c(
      "-U",
      username,
      "-h",
      host,
      "-p",
      port,
      "-d",
      "testdb",
      "-f",
      schema_outfile
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  load_status <- attr(load_out, "status")
  if (is.null(load_status)) {
    load_status <- 0
  }

  if (load_status != 0) {
    stop(
      "psql failed loading schema (exit ",
      load_status,
      ")\n",
      paste(load_out, collapse = "\n")
    )
  }

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
      "field",
      "boreholes",
      "information",
      "application",
      "audit"
    ),
    collapse = ", "
  )

  # Set the search path (takes effect at next connection)
  DBI::dbExecute(
    test_con,
    sprintf("ALTER DATABASE testdb SET search_path TO %s;", db_search_path)
  )
  # Update the search path for this session
  DBI::dbExecute(
    test_con,
    sprintf("SET search_path TO %s;", db_search_path)
  )

  append_if_rows <- function(tbl, data, label = tbl) {
    if (nrow(data) == 0) {
      return(invisible(FALSE))
    }
    message("Loading table ", label, " into the test database...")
    DBI::dbAppendTable(test_con, DBI::SQL(tbl), data)
    invisible(TRUE)
  }

  message("Loading ancillary tables...")
  full_tbls <- c(
    "continuous.aggregation_types",
    "continuous.correction_types",
    "continuous.timeseries_types",
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
    "files.image_types",
    "information.internal_status",
    "information.version_info",
    "instruments.communication_protocol_families",
    "instruments.communication_protocols",
    "instruments.instrument_types",
    "instruments.instrument_makes",
    "instruments.instrument_models",
    "instruments.sensor_types",
    "instruments.suppliers",
    "instruments.transmission_component_roles",
    "instruments.transmission_method_families",
    "instruments.transmission_methods",
    "public.approval_types",
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
    "spatial.raster_types",
    "boreholes.borehole_well_purposes",
    "boreholes.casing_materials",
    "boreholes.drillers",
    "discrete.guideline_publishers",
    "discrete.guideline_series",
    "discrete.guidelines",
    "discrete.guidelines_fractions",
    "discrete.guidelines_media_types"
  )

  # Load the ancillary tables into the test database using DBI
  for (tbl in full_tbls) {
    # Read the table from the original database
    data <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", tbl))

    # Write the table to the test database
    append_if_rows(tbl, data)
  }

  message("\nLoading deterministic synthetic test data...")

  first_id <- function(sql, label) {
    value <- DBI::dbGetQuery(test_con, sql)[[1]]
    if (length(value) == 0 || is.na(value[[1]])) {
      stop("No lookup row available for ", label, ".")
    }
    as.integer(value[[1]])
  }

  parameter_id <- function(name, fallback_sql) {
    value <- DBI::dbGetQuery(
      test_con,
      "SELECT parameter_id
       FROM public.parameters
       WHERE param_name = $1
       LIMIT 1",
      params = list(name)
    )$parameter_id
    if (length(value) > 0) {
      return(as.integer(value[[1]]))
    }
    first_id(fallback_sql, paste0("parameter ", name))
  }

  fake_location_id <- 1L
  fake_sub_location_id <- 1L
  fake_z_surface <- 1L
  fake_z_air <- 2L
  fake_z_snow <- 3L
  fake_network_id <- 1L
  fake_project_id <- 1L
  fake_organization_id <- 1L

  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.organizations (
         organization_id, name, name_fr, note, created_by, created
       ) VALUES (
         %d, 'Placeholder organization',
         'Organisation generique',
         'Generic placeholder organization for portable AquaCache seed databases.',
         'AquaCache seed', '2020-01-01 00:00+00'
       )",
      fake_organization_id
    )
  )
  owner_org <- fake_organization_id
  contributor_org <- fake_organization_id

  location_type_id <- first_id(
    "SELECT type_id FROM public.location_types ORDER BY type_id LIMIT 1",
    "location type"
  )
  network_project_type_id <- first_id(
    "SELECT id FROM public.network_project_types ORDER BY id LIMIT 1",
    "network/project type"
  )
  media_surface <- first_id(
    "SELECT media_id FROM public.media_types WHERE media_type = 'surface water' LIMIT 1",
    "surface water media"
  )
  media_atmospheric <- first_id(
    "SELECT media_id FROM public.media_types WHERE media_type = 'atmospheric' LIMIT 1",
    "atmospheric media"
  )
  media_snow <- first_id(
    "SELECT media_id FROM public.media_types WHERE media_type = 'snow' LIMIT 1",
    "snow media"
  )
  media_groundwater <- first_id(
    "SELECT media_id FROM public.media_types WHERE media_type = 'groundwater' LIMIT 1",
    "groundwater media"
  )
  media_rain <- first_id(
    "SELECT media_id FROM public.media_types WHERE media_type = 'rain water' LIMIT 1",
    "rain water media"
  )
  matrix_liquid <- first_id(
    "SELECT matrix_state_id FROM public.matrix_states WHERE matrix_state_code = 'liquid' LIMIT 1",
    "liquid matrix"
  )
  matrix_gas <- first_id(
    "SELECT matrix_state_id FROM public.matrix_states WHERE matrix_state_code = 'gas' LIMIT 1",
    "gas matrix"
  )
  agg_instant <- first_id(
    "SELECT aggregation_type_id FROM continuous.aggregation_types WHERE aggregation_type = 'instantaneous' LIMIT 1",
    "instantaneous aggregation"
  )
  agg_mean <- first_id(
    "SELECT aggregation_type_id FROM continuous.aggregation_types WHERE aggregation_type = 'mean' LIMIT 1",
    "mean aggregation"
  )
  agg_sum <- first_id(
    "SELECT aggregation_type_id FROM continuous.aggregation_types WHERE aggregation_type = 'sum' LIMIT 1",
    "sum aggregation"
  )
  water_level_param <- parameter_id(
    "water level",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  water_flow_param <- parameter_id(
    "water flow",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  precip_param <- parameter_id(
    "precipitation, total",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  air_temp_param <- parameter_id(
    "temperature, air",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  water_temp_param <- parameter_id(
    "temperature, water",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  swe_param <- parameter_id(
    "snow water equivalent",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  conductance_param <- parameter_id(
    "specific conductance",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  conductivity_param <- parameter_id(
    "conductivity",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  ph_param <- parameter_id(
    "pH",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  turbidity_param <- parameter_id(
    "turbidity",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  alkalinity_param <- parameter_id(
    "alkalinity",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  hardness_param <- parameter_id(
    "hardness",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  chloride_param <- parameter_id(
    "chloride",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  sulfate_param <- parameter_id(
    "sulfate",
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )
  grade_a <- first_id(
    "SELECT grade_type_id FROM public.grade_types WHERE grade_type_code = 'A' LIMIT 1",
    "grade A"
  )
  grade_b <- first_id(
    "SELECT grade_type_id FROM public.grade_types WHERE grade_type_code = 'B' LIMIT 1",
    "grade B"
  )
  approval_a <- first_id(
    "SELECT approval_type_id FROM public.approval_types WHERE approval_type_code = 'A' LIMIT 1",
    "approval A"
  )
  approval_n <- first_id(
    "SELECT approval_type_id FROM public.approval_types WHERE approval_type_code = 'N' LIMIT 1",
    "approval N"
  )
  qualifier_ice <- first_id(
    "SELECT qualifier_type_id FROM public.qualifier_types ORDER BY qualifier_type_id LIMIT 1",
    "qualifier"
  )
  correction_offset <- first_id(
    "SELECT correction_type_id FROM continuous.correction_types WHERE correction_type = 'offset linear' LIMIT 1",
    "offset correction"
  )
  collection_method_id <- first_id(
    "SELECT collection_method_id FROM discrete.collection_methods ORDER BY collection_method_id LIMIT 1",
    "collection method"
  )
  collection_method_pump <- first_id(
    "SELECT collection_method_id FROM discrete.collection_methods WHERE collection_method = 'Pump' LIMIT 1",
    "pump collection method"
  )
  collection_method_bottle <- first_id(
    "SELECT collection_method_id FROM discrete.collection_methods WHERE collection_method = 'Water Bottle (direct fill)' LIMIT 1",
    "water bottle collection method"
  )
  sample_type_id <- first_id(
    "SELECT sample_type_id FROM discrete.sample_types ORDER BY sample_type_id LIMIT 1",
    "sample type"
  )
  sample_type_routine <- first_id(
    "SELECT sample_type_id FROM discrete.sample_types WHERE sample_type = 'sample-routine' LIMIT 1",
    "routine sample type"
  )
  sample_type_field_blank <- first_id(
    "SELECT sample_type_id FROM discrete.sample_types WHERE sample_type = 'QC-sample-field blank' LIMIT 1",
    "field blank sample type"
  )
  sample_type_field_replicate <- first_id(
    "SELECT sample_type_id FROM discrete.sample_types WHERE sample_type = 'QC-sample-field replicate' LIMIT 1",
    "field replicate sample type"
  )
  result_type_field <- first_id(
    "SELECT result_type_id FROM discrete.result_types WHERE result_type = 'field' LIMIT 1",
    "field result type"
  )
  result_type_lab <- first_id(
    "SELECT result_type_id FROM discrete.result_types WHERE result_type = 'lab' LIMIT 1",
    "lab result type"
  )
  sample_fraction_total <- first_id(
    "SELECT sample_fraction_id FROM discrete.sample_fractions WHERE sample_fraction = 'total' LIMIT 1",
    "total sample fraction"
  )
  sample_fraction_dissolved <- first_id(
    "SELECT sample_fraction_id FROM discrete.sample_fractions WHERE sample_fraction = 'dissolved' LIMIT 1",
    "dissolved sample fraction"
  )
  result_value_actual <- first_id(
    "SELECT result_value_type_id FROM discrete.result_value_types WHERE result_value_type = 'Actual' LIMIT 1",
    "actual result value type"
  )
  result_value_calculated <- first_id(
    "SELECT result_value_type_id FROM discrete.result_value_types WHERE result_value_type = 'Calculated' LIMIT 1",
    "calculated result value type"
  )
  result_value_estimated <- first_id(
    "SELECT result_value_type_id FROM discrete.result_value_types WHERE result_value_type = 'Estimated' LIMIT 1",
    "estimated result value type"
  )
  result_condition_id <- first_id(
    "SELECT result_condition_id FROM discrete.result_conditions ORDER BY result_condition_id LIMIT 1",
    "result condition"
  )
  result_speciation_caco3 <- first_id(
    "SELECT result_speciation_id FROM discrete.result_speciations WHERE result_speciation = 'as CaCO3' LIMIT 1",
    "CaCO3 result speciation"
  )
  protocol_method_id <- first_id(
    "SELECT protocol_id FROM discrete.protocols_methods ORDER BY protocol_id LIMIT 1",
    "protocol/method"
  )
  laboratory_id <- first_id(
    "SELECT lab_id FROM discrete.laboratories ORDER BY lab_id LIMIT 1",
    "laboratory"
  )

  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.networks (
         network_id, name, description, type
       ) VALUES (
         %d, 'Synthetic monitoring network',
         'Network used only by AquaCache automated tests.', %d
       )",
      fake_network_id,
      network_project_type_id
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.projects (
         project_id, name, description, type
       ) VALUES (
         %d, 'Synthetic monitoring project',
         'Project used only by AquaCache automated tests.', %d
       )",
      fake_project_id,
      network_project_type_id
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.locations (
         location_code, name, latitude, longitude, note, location_id,
         name_fr, share_with, location_type, install_purpose,
         current_purpose, jurisdictional_relevance,
         anthropogenic_influence, sentinel_location, alias
       ) VALUES (
         'TEST_LOC_001', 'Synthetic Test Location',
         60.0000, -135.0000,
         'Deterministic fake location for automated tests.', %d,
         'Endroit de test synthetique', ARRAY['public_reader'], %d,
         'automated tests', 'automated tests',
         false, false, true, 'SYNTHETIC_TEST_LOCATION'
       )",
      fake_location_id,
      location_type_id
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.locations_networks (network_id, location_id)
       VALUES (%d, %d)",
      fake_network_id,
      fake_location_id
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.locations_projects (project_id, location_id)
       VALUES (%d, %d)",
      fake_project_id,
      fake_location_id
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.sub_locations (
         sub_location_id, location_id, sub_location_name,
         sub_location_name_fr, latitude, longitude, note, share_with
       ) VALUES (
         %d, %d, 'Synthetic left bank',
         'Rive gauche synthetique', 60.0001, -135.0001,
         'Deterministic fake sub-location.', ARRAY['public_reader']
       )",
      fake_sub_location_id,
      fake_location_id
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO public.locations_z (
         z_id, location_id, sub_location_id, z_meters, note
       ) OVERRIDING SYSTEM VALUE VALUES
         (%d, %d, NULL, 700.00, 'Synthetic water surface datum'),
         (%d, %d, NULL, 702.50, 'Synthetic air sensor height'),
         (%d, %d, %d, 699.50, 'Synthetic snow course elevation')",
      fake_z_surface,
      fake_location_id,
      fake_z_air,
      fake_location_id,
      fake_z_snow,
      fake_location_id,
      fake_sub_location_id
    )
  )

  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.timeseries (
         timeseries_id, parameter_id, media_id, aggregation_type_id,
         start_datetime, end_datetime, last_new_data, source_fx,
         source_fx_args, note, record_rate, location_id, z_id, active,
         share_with, default_owner, sensor_priority, sub_location_id,
         timezone_daily_calc, sync_remote, matrix_state_id,
         timeseries_type, publicly_visible
       ) VALUES
         (1, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"water_level\"}'::jsonb,
          'Synthetic 15-minute water level.', interval '15 minutes',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (2, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"water_temperature\"}'::jsonb,
          'Synthetic 15-minute water temperature.', interval '15 minutes',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (3, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"air_temperature\"}'::jsonb,
          'Synthetic hourly air temperature.', interval '1 hour',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (4, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"precipitation\"}'::jsonb,
          'Synthetic hourly precipitation totals.', interval '1 hour',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (5, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"swe\"}'::jsonb,
          'Synthetic daily snow water equivalent.', interval '1 day',
          %d, %d, true, ARRAY['public_reader'], %d, 1, %d, 0, true,
          %d, 'basic', true),
         (6, %d, %d, %d, NULL, NULL, NULL,
          NULL, NULL, 'Synthetic compound fallback water level.', interval '15 minutes',
          %d, %d, true, ARRAY['public_reader'], %d, 2, NULL, 0, true,
          %d, 'compound', true),
         (7, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"conductance\"}'::jsonb,
         'Synthetic hourly specific conductance.', interval '1 hour',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (8, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"water_flow\"}'::jsonb,
          'Synthetic hourly water flow.', interval '1 hour',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (9, %d, %d, %d, NULL, NULL, NULL,
          'downloadSynthetic', '{\"series\":\"conductivity\"}'::jsonb,
          'Synthetic hourly conductivity used as a compound member.', interval '1 hour',
          %d, %d, true, ARRAY['public_reader'], %d, 1, NULL, 0, true,
          %d, 'basic', true),
         (10, %d, %d, %d, NULL, NULL, NULL,
          NULL, NULL,
          'Synthetic temperature-corrected specific conductance derived from conductivity and water temperature.',
          interval '1 hour',
          %d, %d, true, ARRAY['public_reader'], %d, 2, NULL, 0, true,
          %d, 'compound', true)",
      water_level_param,
      media_surface,
      agg_instant,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid,
      water_temp_param,
      media_surface,
      agg_instant,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid,
      air_temp_param,
      media_atmospheric,
      agg_mean,
      fake_location_id,
      fake_z_air,
      owner_org,
      matrix_gas,
      precip_param,
      media_atmospheric,
      agg_sum,
      fake_location_id,
      fake_z_air,
      owner_org,
      matrix_liquid,
      swe_param,
      media_snow,
      agg_mean,
      fake_location_id,
      fake_z_snow,
      owner_org,
      fake_sub_location_id,
      matrix_liquid,
      water_level_param,
      media_surface,
      agg_instant,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid,
      conductance_param,
      media_surface,
      agg_mean,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid,
      water_flow_param,
      media_surface,
      agg_instant,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid,
      conductivity_param,
      media_surface,
      agg_mean,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid,
      conductance_param,
      media_surface,
      agg_mean,
      fake_location_id,
      fake_z_surface,
      owner_org,
      matrix_liquid
    )
  )

  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.timeseries_compounds (
       timeseries_id, expression_sql
     ) VALUES
       (6, NULL),
       (10, 'cond / (1 + 0.0191 * (temp - 25))')"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.timeseries_compound_members (
       timeseries_id, member_alias, member_timeseries_id,
       member_priority, use_from, use_to
     ) VALUES
       (6, 'primary_level', 1, 1, NULL, NULL),
       (6, 'backup_level', 2, 2, '2023-01-05 00:00+00', NULL),
       (10, 'temp', 2, 1, NULL, NULL),
       (10, 'cond', 9, 1, NULL, NULL)"
  )

  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       1,
       gs.datetime,
       round(
         (10 + sin(extract(epoch FROM gs.datetime) / 86400.0) * 0.25)::numeric,
         3
       ),
       interval '15 minutes',
       false,
       false,
       1001000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '15 minutes'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       2,
       gs.datetime,
       round(
         (4 + cos(extract(epoch FROM gs.datetime) / 43200.0) * 1.5)::numeric,
         3
       ),
       interval '15 minutes',
       mod((row_number() OVER (ORDER BY gs.datetime))::integer, 37) = 0,
       false,
       1002000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '15 minutes'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       3,
       gs.datetime,
       round(
         (-12 + sin(extract(epoch FROM gs.datetime) / 172800.0) * 8)::numeric,
         2
       ),
       interval '1 hour',
       false,
       false,
       1003000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '1 hour'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       4,
       gs.datetime,
       CASE
         WHEN mod((row_number() OVER (ORDER BY gs.datetime))::integer, 8) = 0 THEN 0.6
         WHEN mod((row_number() OVER (ORDER BY gs.datetime))::integer, 13) = 0 THEN 1.2
         ELSE 0
       END,
       interval '1 hour',
       false,
       false,
       1004000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '1 hour'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       5,
       gs.datetime,
       100 + row_number() OVER (ORDER BY gs.datetime) * 3,
       interval '1 day',
       false,
       false,
       1005000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 12:00+00'::timestamptz,
       '2024-01-01 12:00+00'::timestamptz,
       interval '1 day'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       7,
       gs.datetime,
       150 + row_number() OVER (ORDER BY gs.datetime) * 0.5,
       interval '1 hour',
       false,
       false,
       1007000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '1 hour'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       8,
       gs.datetime,
       round(
         (25 + sin(extract(epoch FROM gs.datetime) / 129600.0) * 4)::numeric,
         3
       ),
       interval '1 hour',
       false,
       false,
       1008000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '1 hour'
     ) AS gs(datetime)"
  )
  DBI::dbExecute(
    test_con,
    "INSERT INTO continuous.measurements_continuous (
       timeseries_id, datetime, value, period, imputed, no_update,
       measurement_row_id
     )
     SELECT
       9,
       gs.datetime,
       round(
         (138 + sin(extract(epoch FROM gs.datetime) / 216000.0) * 18 +
          cos(extract(epoch FROM gs.datetime) / 604800.0) * 7)::numeric,
         3
       ),
       interval '1 hour',
       false,
       false,
       1009000000 + row_number() OVER (ORDER BY gs.datetime)
     FROM generate_series(
       '2020-01-01 00:00+00'::timestamptz,
       '2024-01-01 00:00+00'::timestamptz,
       interval '1 hour'
     ) AS gs(datetime)"
  )

  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.grades (
         grade_id, timeseries_id, grade_type_id, start_dt, end_dt
       ) VALUES
         (1, 1, %d, '2020-01-01 00:00+00', '2022-01-01 00:00+00'),
         (2, 1, %d, '2022-01-01 00:00+00', '2024-01-02 00:00+00'),
         (3, 2, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (4, 9, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (5, 10, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00')",
      grade_a,
      grade_b,
      grade_a,
      grade_b,
      grade_a
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.approvals (
         approval_id, timeseries_id, approval_type_id, start_dt, end_dt
       ) VALUES
         (1, 1, %d, '2020-01-01 00:00+00', '2022-07-01 00:00+00'),
         (2, 1, %d, '2022-07-01 00:00+00', '2024-01-02 00:00+00'),
         (3, 3, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (4, 9, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (5, 10, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00')",
      approval_a,
      approval_n,
      approval_a,
      approval_a,
      approval_a
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.qualifiers (
         qualifier_id, timeseries_id, qualifier_type_id, start_dt, end_dt
       ) VALUES
         (1, 1, %d, '2023-01-03 00:00+00', '2023-01-04 00:00+00'),
         (2, 2, %d, '2023-01-02 00:00+00', '2023-01-02 12:00+00')",
      qualifier_ice,
      qualifier_ice
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.owners (
         owner_id, timeseries_id, organization_id, start_dt, end_dt
       ) VALUES
         (1, 1, %d, '2020-01-01 00:00+00', '2022-07-01 00:00+00'),
         (2, 1, %d, '2022-07-01 00:00+00', '2024-01-02 00:00+00'),
         (3, 2, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (4, 9, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (5, 10, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00')",
      owner_org,
      contributor_org,
      owner_org,
      owner_org,
      owner_org
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.contributors (
         contributor_id, timeseries_id, organization_id, start_dt, end_dt
       ) VALUES
         (1, 1, %d, '2020-01-01 00:00+00', '2022-07-01 00:00+00'),
         (2, 1, %d, '2022-07-01 00:00+00', '2024-01-02 00:00+00'),
         (3, 3, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (4, 9, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00'),
         (5, 10, %d, '2020-01-01 00:00+00', '2024-01-02 00:00+00')",
      contributor_org,
      owner_org,
      contributor_org,
      contributor_org,
      contributor_org
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO continuous.corrections (
         correction_id, timeseries_id, start_dt, end_dt,
         correction_type, value1, value2, timestep_window, equation
       ) VALUES
         (1, 1, '2023-01-03 00:00+00', '2023-01-04 00:00+00',
          %d, 0.25, NULL, NULL, NULL)",
      correction_offset
    )
  )

  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO discrete.sample_series (
         sample_series_id, location_id, sub_location_id, synch_from,
         synch_to, default_owner, default_contributor, last_new_data,
         active, source_fx, source_fx_args, note, sync_remote
       ) VALUES (
         1, %d, %d, '2023-01-01 00:00+00',
         '2023-01-10 00:00+00', %d, %d, '2023-01-10 00:00+00',
         true, 'downloadSyntheticDiscrete',
         '{\"series\":\"synthetic_samples\"}'::jsonb,
         'Synthetic sample series for automated tests.', true
       )",
      fake_location_id,
      fake_sub_location_id,
      owner_org,
      contributor_org
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO discrete.samples (
         sample_id, location_id, sub_location_id, media_id, z, datetime,
         target_datetime, collection_method, sample_type, sample_volume_ml,
         sample_grade, sample_approval, sample_qualifier, owner,
         contributor, sampling_org, share_with, import_source,
         no_update, note, import_source_id
       ) VALUES
         (1, %d, %d, %d, 0.5, '2023-01-01 12:00+00',
          '2023-01-01 12:00+00', %d, %d, 250, %d, %d, %d,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Synthetic water quality sample 1.', 'SYN-S1'),
         (2, %d, %d, %d, 0.5, '2023-02-01 12:00+00',
          '2023-02-01 12:00+00', %d, %d, 250, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Synthetic water quality sample 2.', 'SYN-S2'),
         (3, %d, %d, %d, 0.5, '2023-03-01 12:00+00',
          '2023-03-01 12:00+00', %d, %d, 500, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Synthetic water quality sample 3.', 'SYN-S3')",
      fake_location_id,
      fake_sub_location_id,
      media_surface,
      collection_method_id,
      sample_type_id,
      grade_a,
      approval_a,
      qualifier_ice,
      owner_org,
      contributor_org,
      owner_org,
      fake_location_id,
      fake_sub_location_id,
      media_surface,
      collection_method_id,
      sample_type_id,
      grade_b,
      approval_n,
      owner_org,
      contributor_org,
      owner_org,
      fake_location_id,
      fake_sub_location_id,
      media_surface,
      collection_method_id,
      sample_type_id,
      grade_a,
      approval_a,
      owner_org,
      contributor_org,
      owner_org
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO discrete.samples (
         sample_id, location_id, sub_location_id, media_id, z, datetime,
         target_datetime, collection_method, sample_type, linked_with,
         sample_volume_ml, purge_volume_l, purge_time_min, flow_rate_l_min,
         sample_grade, sample_approval, sample_qualifier, owner,
         contributor, sampling_org, share_with, import_source,
         no_update, note, import_source_id
       ) VALUES
         (4, %d, %d, %d, 0.5, '2023-04-01 12:00+00',
          '2023-04-01 12:00+00', %d, %d, NULL,
          1000, NULL, NULL, NULL, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Routine spring freshet surface-water chemistry sample.', 'SYN-S4'),
         (5, %d, %d, %d, 0.5, '2023-04-01 12:05+00',
          '2023-04-01 12:00+00', %d, %d, 4,
          1000, NULL, NULL, NULL, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Field replicate paired with SYN-S4.', 'SYN-S5'),
         (6, %d, %d, %d, 0.5, '2023-04-01 12:10+00',
          '2023-04-01 12:00+00', %d, %d, 4,
          500, NULL, NULL, NULL, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          true, 'Field blank paired with the April surface-water sample.', 'SYN-S6'),
         (7, %d, %d, %d, -4.2, '2023-05-15 18:00+00',
          '2023-05-15 18:00+00', %d, %d, NULL,
          1000, 18.5, 21, 0.9, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Pumped groundwater chemistry sample with purge metadata.', 'SYN-S7'),
         (8, %d, %d, %d, NULL, '2023-06-01 09:00+00',
          '2023-06-01 09:00+00', %d, %d, NULL,
          750, NULL, NULL, NULL, %d, %d, NULL,
          %d, %d, %d, ARRAY['public_reader'], 'synthetic_fixture',
          false, 'Rain-water grab sample after a synthetic storm event.', 'SYN-S8')",
      fake_location_id,
      fake_sub_location_id,
      media_surface,
      collection_method_bottle,
      sample_type_routine,
      grade_a,
      approval_a,
      owner_org,
      contributor_org,
      owner_org,
      fake_location_id,
      fake_sub_location_id,
      media_surface,
      collection_method_bottle,
      sample_type_field_replicate,
      grade_a,
      approval_a,
      owner_org,
      contributor_org,
      owner_org,
      fake_location_id,
      fake_sub_location_id,
      media_surface,
      collection_method_bottle,
      sample_type_field_blank,
      grade_a,
      approval_a,
      owner_org,
      contributor_org,
      owner_org,
      fake_location_id,
      fake_sub_location_id,
      media_groundwater,
      collection_method_pump,
      sample_type_routine,
      grade_b,
      approval_n,
      owner_org,
      contributor_org,
      owner_org,
      fake_location_id,
      fake_sub_location_id,
      media_rain,
      collection_method_id,
      sample_type_routine,
      grade_a,
      approval_a,
      owner_org,
      contributor_org,
      owner_org
    )
  )
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO discrete.results (
         result_id, sample_id, result_type, parameter_id, sample_fraction_id,
         result, result_condition, result_condition_value,
         result_value_type, analysis_datetime, share_with, no_update,
         matrix_state_id
       ) VALUES
         (1, 1, %d, %d, %d, 6.7, NULL, NULL, %d,
          '2023-01-02 12:00+00', ARRAY['public_reader'], false, %d),
         (2, 1, %d, %d, %d, 118, NULL, NULL, %d,
          '2023-01-02 12:00+00', ARRAY['public_reader'], false, %d),
         (3, 2, %d, %d, %d, 6.9, NULL, NULL, %d,
          '2023-02-02 12:00+00', ARRAY['public_reader'], false, %d),
         (4, 2, %d, %d, %d, 124, NULL, NULL, %d,
          '2023-02-02 12:00+00', ARRAY['public_reader'], false, %d),
         (5, 3, %d, %d, %d, NULL, %d, 0.01, %d,
          '2023-03-02 12:00+00', ARRAY['public_reader'], false, %d)",
      result_type_field,
      ph_param,
      sample_fraction_total,
      result_value_actual,
      matrix_liquid,
      result_type_lab,
      conductance_param,
      sample_fraction_total,
      result_value_actual,
      matrix_liquid,
      result_type_field,
      water_temp_param,
      sample_fraction_total,
      result_value_actual,
      matrix_liquid,
      result_type_lab,
      conductance_param,
      sample_fraction_total,
      result_value_actual,
      matrix_liquid,
      result_type_lab,
      conductance_param,
      sample_fraction_total,
      result_condition_id,
      result_value_actual,
      matrix_liquid
    )
  )

  # Results 6-15
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO discrete.results (
       result_id, sample_id, result_type, parameter_id, sample_fraction_id,
       result, result_condition, result_condition_value,
       result_value_type, result_speciation_id, protocol_method,
       laboratory, analysis_datetime, share_with, no_update,
       matrix_state_id
     ) VALUES
       (6, 4, %d, %d, NULL, 7.1, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-04-01 12:05+00', ARRAY['public_reader'], false, %d),
       (7, 4, %d, %d, NULL, 5.8, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-04-01 12:05+00', ARRAY['public_reader'], false, %d),
       (8, 4, %d, %d, NULL, 3.4, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-04-01 12:05+00', ARRAY['public_reader'], false, %d),
       (9, 4, %d, %d, NULL, 129.6, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-04-01 12:05+00', ARRAY['public_reader'], false, %d),
       (10, 4, %d, %d, %d, 68, NULL, NULL, %d, %d, %d, %d,
        '2023-04-02 09:00+00', ARRAY['public_reader'], false, %d),
       (11, 4, %d, %d, %d, 74, NULL, NULL, %d, %d, %d, %d,
        '2023-04-02 09:00+00', ARRAY['public_reader'], false, %d),
       (12, 4, %d, %d, %d, 1.7, NULL, NULL, %d, NULL, %d, %d,
        '2023-04-02 09:00+00', ARRAY['public_reader'], false, %d),
       (13, 4, %d, %d, %d, 3.6, NULL, NULL, %d, NULL, %d, %d,
        '2023-04-02 09:00+00', ARRAY['public_reader'], false, %d),
       (14, 5, %d, %d, NULL, 7.0, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-04-01 12:10+00', ARRAY['public_reader'], false, %d),
       (15, 5, %d, %d, NULL, 127.9, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-04-01 12:10+00', ARRAY['public_reader'], false, %d)",
      result_type_field,
      ph_param,
      result_value_actual,
      matrix_liquid,
      result_type_field,
      water_temp_param,
      result_value_actual,
      matrix_liquid,
      result_type_field,
      turbidity_param,
      result_value_estimated,
      matrix_liquid,
      result_type_field,
      conductance_param,
      result_value_calculated,
      matrix_liquid,
      result_type_lab,
      alkalinity_param,
      sample_fraction_total,
      result_value_actual,
      result_speciation_caco3,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      hardness_param,
      sample_fraction_total,
      result_value_actual,
      result_speciation_caco3,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      chloride_param,
      sample_fraction_dissolved,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      sulfate_param,
      sample_fraction_dissolved,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_field,
      ph_param,
      result_value_actual,
      matrix_liquid,
      result_type_field,
      conductance_param,
      result_value_actual,
      matrix_liquid
    )
  )

  # Results 16-26
  DBI::dbExecute(
    test_con,
    sprintf(
      "INSERT INTO discrete.results (
       result_id, sample_id, result_type, parameter_id, sample_fraction_id,
       result, result_condition, result_condition_value,
       result_value_type, result_speciation_id, protocol_method,
       laboratory, analysis_datetime, share_with, no_update,
       matrix_state_id
     ) VALUES
       (16, 6, %d, %d, %d, NULL, %d, 0.02, %d, NULL, %d, %d,
        '2023-04-02 09:30+00', ARRAY['public_reader'], true, %d),
       (17, 6, %d, %d, %d, NULL, %d, 0.05, %d, NULL, %d, %d,
        '2023-04-02 09:30+00', ARRAY['public_reader'], true, %d),
       (18, 7, %d, %d, NULL, 6.8, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-05-15 18:15+00', ARRAY['public_reader'], false, %d),
       (19, 7, %d, %d, NULL, 412, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-05-15 18:15+00', ARRAY['public_reader'], false, %d),
       (20, 7, %d, %d, %d, 182, NULL, NULL, %d, %d, %d, %d,
        '2023-05-16 10:00+00', ARRAY['public_reader'], false, %d),
       (21, 7, %d, %d, %d, 96, NULL, NULL, %d, %d, %d, %d,
        '2023-05-16 10:00+00', ARRAY['public_reader'], false, %d),
       (22, 7, %d, %d, %d, 12.4, NULL, NULL, %d, NULL, %d, %d,
        '2023-05-16 10:00+00', ARRAY['public_reader'], false, %d),
       (23, 7, %d, %d, %d, 18.1, NULL, NULL, %d, NULL, %d, %d,
        '2023-05-16 10:00+00', ARRAY['public_reader'], false, %d),
       (24, 8, %d, %d, NULL, 5.6, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-06-01 09:15+00', ARRAY['public_reader'], false, %d),
       (25, 8, %d, %d, NULL, 22.5, NULL, NULL, %d, NULL, NULL, NULL,
        '2023-06-01 09:15+00', ARRAY['public_reader'], false, %d),
       (26, 8, %d, %d, %d, NULL, %d, 0.02, %d, NULL, %d, %d,
        '2023-06-02 08:00+00', ARRAY['public_reader'], false, %d)",
      result_type_lab,
      chloride_param,
      sample_fraction_dissolved,
      result_condition_id,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      sulfate_param,
      sample_fraction_dissolved,
      result_condition_id,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_field,
      ph_param,
      result_value_actual,
      matrix_liquid,
      result_type_field,
      conductivity_param,
      result_value_actual,
      matrix_liquid,
      result_type_lab,
      alkalinity_param,
      sample_fraction_total,
      result_value_actual,
      result_speciation_caco3,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      hardness_param,
      sample_fraction_total,
      result_value_actual,
      result_speciation_caco3,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      chloride_param,
      sample_fraction_dissolved,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_lab,
      sulfate_param,
      sample_fraction_dissolved,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid,
      result_type_field,
      ph_param,
      result_value_actual,
      matrix_liquid,
      result_type_field,
      conductivity_param,
      result_value_estimated,
      matrix_liquid,
      result_type_lab,
      chloride_param,
      sample_fraction_dissolved,
      result_condition_id,
      result_value_actual,
      protocol_method_id,
      laboratory_id,
      matrix_liquid
    )
  )

  # Insert a sample row into spatial.vectors
  df <- data.frame(
    feature_name = "sample_feature",
    description = "This is a sample vector layer.",
    x = 0,
    y = 0,
    stringsAsFactors = FALSE
  )
  test_vect <- terra::vect(
    x = df,
    geom = c("x", "y"),
    crs = "EPSG:4326"
  )
  insertACVector(
    con = target_con,
    geom = test_vect,
    layer_name = "sample_layer",
    feature_name_col = "feature_name",
    description_col = "description",
    ask = FALSE
  )

  # Insert basic application schema data

  text <- data.frame(
    id = c("news_head", "news_body", "hr"),
    text_en = c(
      "<h3>Updates and changes</h3><br>",
      "This page can be used to post news and updates about this application or the underlying data. It is currently just a placeholder to demonstrate how to include static text content in the database and display it on the frontend.",
      "<hr>"
    ),
    text_fr = c(
      "<h3>Mises \u00e0 jour et changements</h3><br>",
      "Cette page peut \u00eatre utilis\u00e9e pour publier des nouvelles et des mises \u00e0 jour sur cette application ou les donn\u00e9es sous-jacentes. Il s'agit actuellement d'un simple espace r\u00e9serv\u00e9 pour d\u00e9montrer comment inclure du contenu texte statique dans la base de donn\u00e9es et l'afficher sur cette application.",
      "<hr>"
    )
  )
  for (i in seq_len(nrow(text))) {
    DBI::dbExecute(
      test_con,
      sprintf(
        "INSERT INTO application.text (id, text_en, text_fr)
         VALUES (%s, %s, %s)",
        DBI::dbQuoteString(test_con, text$id[i]),
        DBI::dbQuoteString(test_con, text$text_en[i]),
        DBI::dbQuoteString(test_con, text$text_fr[i])
      )
    )
  }

  content <- DBI::dbGetQuery(
    con,
    "SELECT page, position, content_type, content_id FROM page_content WHERE content_id IN ('news_head', 'news_body', 'hr')"
  )
  for (i in seq_len(nrow(content))) {
    DBI::dbExecute(
      test_con,
      sprintf(
        "INSERT INTO page_content (page, position, content_type, content_id)
         VALUES (%s, %d, %s, %s)",
        DBI::dbQuoteString(test_con, content$page[i]),
        content$position[i],
        DBI::dbQuoteString(test_con, content$content_type[i]),
        DBI::dbQuoteString(test_con, content$content_id[i])
      )
    )
  }

  # Reset all sequences in the test database to avoid conflicts with future inserts
  reset_identity_sequences <- function(con) {
    sql <- "
    SELECT
      seq_ns.nspname AS sequence_schema,
      seq.relname AS sequence_name,
      tbl_ns.nspname AS table_schema,
      tbl.relname AS table_name,
      col.attname AS column_name
    FROM pg_class seq
    JOIN pg_namespace seq_ns
      ON seq_ns.oid = seq.relnamespace
    JOIN pg_depend dep
      ON dep.objid = seq.oid
    JOIN pg_class tbl
      ON tbl.oid = dep.refobjid
    JOIN pg_namespace tbl_ns
      ON tbl_ns.oid = tbl.relnamespace
    JOIN pg_attribute col
      ON col.attrelid = tbl.oid
     AND col.attnum = dep.refobjsubid
    WHERE seq.relkind = 'S'
      AND dep.deptype IN ('a', 'i')
      AND seq_ns.nspname NOT IN ('pg_catalog', 'information_schema');
    "

    seqs <- DBI::dbGetQuery(con, sql)

    for (i in seq_len(nrow(seqs))) {
      seq_name <- DBI::dbQuoteIdentifier(
        con,
        DBI::Id(
          schema = seqs$sequence_schema[i],
          table = seqs$sequence_name[i]
        )
      )

      tbl_name <- DBI::dbQuoteIdentifier(
        con,
        DBI::Id(
          schema = seqs$table_schema[i],
          table = seqs$table_name[i]
        )
      )

      col_name <- DBI::dbQuoteIdentifier(con, seqs$column_name[i])

      reset_sql <- sprintf(
        "SELECT setval(
           %s::regclass,
           CASE
             WHEN (SELECT MAX(%s) FROM %s) IS NULL THEN 1
             ELSE (SELECT MAX(%s) FROM %s)
           END,
           (SELECT MAX(%s) FROM %s) IS NOT NULL
         );",
        DBI::dbQuoteString(con, as.character(seq_name)),
        col_name,
        tbl_name,
        col_name,
        tbl_name,
        col_name,
        tbl_name
      )

      DBI::dbExecute(con, reset_sql)
    }

    invisible(seqs)
  }
  message("Resetting identity sequences in the test database...")
  reset_identity_sequences(test_con)

  # Do a final dump of the test database schema and data
  # Create the pg_dump command for schema/data
  outpath <- file.path(paste0(outpath, "/test_db.sql"))
  outpath <- normalizePath(outpath, mustWork = FALSE)

  if (replace && file.exists(outpath)) {
    file.remove(outpath)
  }

  dump_args <- c(
    "--no-owner", # do not include commands to set ownership of objects to the original owner
    "--no-acl", # do not include commands to set access privileges (grant/revoke)
    "--exclude-extension=pg_stat_statements", # Leaves postgis extensions in
    "-U",
    username,
    "-h",
    host,
    "-p",
    port,
    "-f",
    outpath,
    "-d",
    "testdb"
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

  # sanity check that the file exists & is non empty
  if (!file.exists(outpath) || file.info(outpath)$size == 0) {
    stop("schema dump did not produce a valid file at ", outpath)
  }

  dump_patch_number <- aquacache_db_patch_number(test_con)
  aquacache_prepend_lines_to_file(
    outpath,
    c(
      aquacache_seed_dump_metadata_lines(
        source_database = name,
        patch_number = dump_patch_number
      ),
      "-- ensure public_reader exists for restores that do not use restore_seed_db()",
      aquacache_public_reader_role_sql()
    )
  )

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

  # Return paths of the output file
  return(schema_outfile)
}
