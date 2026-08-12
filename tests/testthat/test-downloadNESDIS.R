make_lrgs_shef_line <- function(
  dcp_address,
  timestamp = "26190120000",
  body = ""
) {
  header <- paste0(
    dcp_address,
    timestamp,
    "G",
    "42",
    "01",
    "N",
    "N",
    "123",
    "E",
    "AB",
    "00042",
    "\""
  )
  stopifnot(nchar(header) == 38L)
  paste0(header, body)
}

read_nesdis_fixture <- function(name) {
  paste(
    readLines(
      testthat::test_path("fixtures", name),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

make_test_lrgs_client <- function() {
  client_dir <- tempfile("opendcs-client-")
  dir.create(client_dir)
  launcher <- if (.Platform$OS.type == "windows") {
    "getDcpMessages.bat"
  } else {
    "getDcpMessages"
  }
  client <- file.path(client_dir, launcher)
  writeLines(
    if (.Platform$OS.type == "windows") "@echo off" else "#!/bin/sh",
    client,
    useBytes = TRUE
  )
  if (.Platform$OS.type != "windows") {
    Sys.chmod(client, mode = "0755")
  }
  list(directory = client_dir, client = client)
}

test_that("NESDIS route lookup uses the setup location without a logger join", {
  statement <- NULL
  parameters <- NULL
  local_mocked_bindings(
    dbGetQuery = function(con, sql, params = NULL, ...) {
      statement <<- sql
      parameters <<- params
      data.frame()
    },
    .package = "DBI"
  )

  result <- AquaCache:::nesdis_get_routes(
    con = structure(list(), class = "mock_con"),
    route_ids = 17L,
    effective_at = as.POSIXct("2026-08-05 12:00:00", tz = "UTC")
  )

  expect_s3_class(result, "data.table")
  expect_match(statement, "s.location_id", fixed = TRUE)
  expect_false(grepl("locations_metadata_instruments", statement, fixed = TRUE))
  expect_equal(length(parameters), 1L)
})

test_that("standard SHEF transmissions are parsed to normalized long data", {
  line <- make_lrgs_shef_line(
    "47002136",
    body = ":VB 0 I15 12.5 12.4:TA 0 I15 1.2A 1.1"
  )

  parsed <- AquaCache:::nesdis_parse_shef(line, "47002136")

  expect_equal(attr(parsed, "transmissions_received"), 1L)
  expect_true(all(
    c(
      "YSS",
      "DCP_Freq_Drft",
      "Message Size",
      "GPS Synch",
      "VB",
      "TA"
    ) %in% parsed$source_field
  ))
  expect_equal(parsed[source_field == "VB"]$value, c(12.4, 12.5))
  expect_equal(
    diff(as.numeric(parsed[source_field == "VB"]$datetime)),
    15 * 60
  )
  expect_equal(parsed[source_field == "TA"]$value, c(1.1, 1.2))
  expect_equal(parsed[source_field == "GPS Synch"]$value, 1)
})

test_that("McMaster underscore fields are preserved and independently mapped", {
  line <- make_lrgs_shef_line(
    "47011656",
    body = paste0(
      ":VB 0 I15 12.1 12.0",
      ":AT 0 I15 -5.1 -5.2",
      ":BBS_VB 0 I15 11.9 11.8",
      ":BBS_AT 0 I15 -6.1 -6.2"
    )
  )
  parsed <- AquaCache:::nesdis_parse_dispatch(
    line,
    "47011656",
    "SHEF_McMaster"
  )

  alpine_mappings <- data.table::data.table(
    transmission_mapping_id = c(1L, 2L),
    transmission_route_id = 101L,
    source_field = c("VB", "AT"),
    timeseries_id = c(1001L, 1002L),
    value_multiplier = c(1, 1),
    value_offset = c(0, 0),
    missing_values = c("[]", "[]"),
    mapping_config = c("{}", "{}")
  )
  buckbrush_mappings <- data.table::data.table(
    transmission_mapping_id = c(3L, 4L),
    transmission_route_id = 102L,
    source_field = c("BBS_VB", "BBS_AT"),
    timeseries_id = c(2001L, 2002L),
    value_multiplier = c(1, 1),
    value_offset = c(0, 0),
    missing_values = c("[]", "[]"),
    mapping_config = c("{}", "{}")
  )

  alpine <- AquaCache:::nesdis_apply_mappings(parsed, alpine_mappings)
  buckbrush <- AquaCache:::nesdis_apply_mappings(
    parsed,
    buckbrush_mappings
  )

  expect_setequal(alpine$source_field, c("VB", "AT"))
  expect_setequal(buckbrush$source_field, c("BBS_VB", "BBS_AT"))
  expect_true(all(alpine$transmission_route_id == 101L))
  expect_true(all(buckbrush$transmission_route_id == 102L))
  expect_false(any(grepl("^BBS_", alpine$source_field)))
})

test_that("BLM transmissions use route-configured rows and sample timing", {
  parsed <- AquaCache:::nesdis_parse_dispatch(
    read_nesdis_fixture("nesdis_blm.txt"),
    "2C63C4B2",
    "BLM",
    route_config = list(
      parser_config = list(
        fields = c("rn1", "ws", "wd", "ta", "rh", "vb", "SDQ"),
        sample_interval_seconds = 15 * 60,
        sample_offset_seconds = 2 * 60,
        values_order = "oldest_first"
      )
    )
  )

  expect_equal(attr(parsed, "transmissions_received"), 1L)
  expect_equal(parsed[source_field == "ta"]$value, c(10, 11, 12, 13))
  expect_equal(parsed[source_field == "rh"]$value, c(80, 101, 104, -2))
  expect_equal(
    parsed[source_field == "ta"]$datetime,
    as.POSIXct(
      c(
        "2026-08-04 11:13:00",
        "2026-08-04 11:28:00",
        "2026-08-04 11:43:00",
        "2026-08-04 11:58:00"
      ),
      tz = "UTC"
    )
  )
  expect_equal(
    parsed[source_field == "SDQ"]$raw_value,
    c("GOOD", "GOOD", "SUSPECT", "GOOD")
  )
  expect_true(all(is.na(parsed[source_field == "SDQ"]$value)))
})

test_that("comma-delimited transmissions preserve numeric and text fields", {
  parsed <- AquaCache:::nesdis_parse_dispatch(
    read_nesdis_fixture("nesdis_comma_delimited.txt"),
    "2C63D7C4",
    "comma-delimited",
    route_config = list(
      parser_config = list(
        has_header = TRUE,
        delimiter = ",",
        datetime_field = "datetime_utc",
        datetime_format = "%Y/%m/%d %H:%M:%S",
        datetime_timezone = "UTC"
      )
    )
  )

  expect_equal(attr(parsed, "transmissions_received"), 1L)
  expect_equal(parsed[source_field == "ta"]$value, c(7.2, 7.8))
  expect_equal(parsed[source_field == "STN"]$raw_value, c("JPK", "JPK"))
  expect_true(all(is.na(parsed[source_field == "STN"]$value)))
  expect_equal(
    parsed[source_field == "SDQ"]$raw_value,
    c("GOOD", "SUSPECT")
  )
  expect_true(all(is.na(parsed[source_field == "SDQ"]$value)))
})

test_that("headerless delimited transmissions use configured record timing", {
  line <- make_lrgs_shef_line(
    "2C64667E",
    timestamp = "26216123000",
    body = "POOL,4.1,82\nPOOL,4.3,81"
  )
  parsed <- AquaCache:::nesdis_parse_dispatch(
    line,
    "2C64667E",
    "CSV",
    route_config = list(
      parser_config = list(
        fields = c("STN", "ta", "rh"),
        record_interval_seconds = 60 * 60,
        record_offset_seconds = 30 * 60,
        records_order = "oldest_first"
      )
    )
  )

  expect_equal(parsed[source_field == "ta"]$value, c(4.1, 4.3))
  expect_equal(
    parsed[source_field == "ta"]$datetime,
    as.POSIXct(
      c("2026-08-04 11:00:00", "2026-08-04 12:00:00"),
      tz = "UTC"
    )
  )
})

test_that("mapping transformations and missing-value rules are applied", {
  parsed <- data.table::data.table(
    source_field = rep("SD", 3),
    datetime = as.POSIXct(
      c(
        "2026-07-01 00:00:00",
        "2026-07-01 00:15:00",
        "2026-07-01 00:30:00"
      ),
      tz = "UTC"
    ),
    raw_value = c("1.5", "99999", "2"),
    value = c(1.5, 99999, 2)
  )
  mappings <- data.table::data.table(
    transmission_mapping_id = 1L,
    transmission_route_id = 1L,
    source_field = "SD",
    timeseries_id = 10L,
    value_multiplier = 100,
    value_offset = 1,
    missing_values = '["99999"]',
    mapping_config = "{}"
  )

  mapped <- AquaCache:::nesdis_apply_mappings(parsed, mappings)

  expect_equal(mapped$value, c(151, 201))
  expect_equal(nrow(mapped), 2L)
})

test_that("unsupported formats fail with an extension instruction", {
  expect_error(
    AquaCache:::nesdis_parse_dispatch(
      "",
      "47002136",
      "FUTURE_BINARY_FORMAT"
    ),
    "pass a custom parser"
  )
})

test_that("custom parser output is normalized and validated", {
  custom <- function(message, dcp_address, message_format) {
    data.frame(
      source_field = "field.path",
      datetime = "2026-07-01 00:00:00",
      raw_value = "42",
      value = "42"
    )
  }

  parsed <- AquaCache:::nesdis_parse_dispatch(
    "ignored",
    "47002136",
    "JSON_v2",
    parser = custom
  )

  expect_s3_class(parsed$datetime, "POSIXct")
  expect_type(parsed$value, "double")
  expect_equal(parsed$value, 42)
})

test_that("custom parsers can receive route configuration", {
  custom <- function(message, dcp_address, message_format, route_config) {
    data.frame(
      source_field = route_config$parser_config$field,
      datetime = "2026-08-04 12:00:00",
      raw_value = "42",
      value = 42
    )
  }

  parsed <- AquaCache:::nesdis_parse_dispatch(
    "ignored",
    "47002136",
    "FUTURE_FORMAT",
    route_config = list(parser_config = list(field = "configured.field")),
    parser = custom
  )

  expect_equal(parsed$source_field, "configured.field")
})

test_that("downloadNESDIS passes route parser configuration to CSV", {
  route <- data.table::data.table(
    transmission_route_id = 102L,
    transmission_setup_id = 12L,
    message_format = "comma-delimited",
    route_config = jsonlite::toJSON(
      list(
        max_days = 2,
        parser_config = list(
          has_header = TRUE,
          datetime_field = "datetime_utc",
          datetime_format = "%Y/%m/%d %H:%M:%S",
          datetime_timezone = "UTC"
        )
      ),
      auto_unbox = TRUE
    ),
    route_name = "CSV route",
    platform_identifier = "2C63D7C4",
    start_datetime_setup = as.POSIXct("2020-01-01", tz = "UTC"),
    end_datetime_setup = as.POSIXct(NA, tz = "UTC"),
    location_id = 1L
  )
  mapping <- data.table::data.table(
    transmission_mapping_id = 2L,
    transmission_route_id = 102L,
    source_field = "ta",
    timeseries_id = 9002L,
    value_multiplier = 1,
    value_offset = 0,
    missing_values = "[]",
    mapping_config = "{}"
  )

  local_mocked_bindings(
    nesdis_get_routes = function(...) route,
    nesdis_get_mappings = function(...) mapping,
    nesdis_get_cursors = function(...) {
      data.table::data.table(
        transmission_route_id = integer(),
        last_query_until = as.POSIXct(character(), tz = "UTC")
      )
    },
    .package = "AquaCache"
  )

  result <- downloadNESDIS(
    timeseries_id = 9002L,
    start_datetime = as.POSIXct("2026-08-04 10:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-08-04 13:00:00", tz = "UTC"),
    con = structure(list(), class = "mock_con"),
    raw_messages = read_nesdis_fixture("nesdis_comma_delimited.txt")
  )

  expect_equal(result$value, c(7.2, 7.8))
  expect_equal(
    result$datetime,
    as.POSIXct(
      c("2026-08-04 11:00:00", "2026-08-04 12:00:00"),
      tz = "UTC"
    )
  )
})

test_that("timeseries adapter returns the standard source_fx contract", {
  route <- data.table::data.table(
    transmission_route_id = 101L,
    transmission_setup_id = 11L,
    message_format = "FUTURE_JSON",
    route_config = '{"max_days":2}',
    route_name = "test route",
    platform_identifier = "47002136",
    start_datetime_setup = as.POSIXct(
      "2020-01-01 00:00:00",
      tz = "UTC"
    ),
    end_datetime_setup = as.POSIXct(NA, tz = "UTC"),
    location_id = 1L
  )
  mapping <- data.table::data.table(
    transmission_mapping_id = 1L,
    transmission_route_id = 101L,
    source_field = "water.level",
    timeseries_id = 9001L,
    value_multiplier = 2,
    value_offset = 1,
    missing_values = "[]",
    mapping_config = "{}"
  )
  custom <- function(message, dcp_address, message_format) {
    data.frame(
      source_field = rep("water.level", 2),
      datetime = c(
        "2020-01-01 00:00:00",
        "2026-07-29 12:00:00"
      ),
      raw_value = c("4", "5"),
      value = c(4, 5)
    )
  }

  local_mocked_bindings(
    nesdis_get_routes = function(...) route,
    nesdis_get_mappings = function(...) mapping,
    nesdis_get_cursors = function(...) {
      data.table::data.table(
        transmission_route_id = integer(),
        last_query_until = as.POSIXct(character(), tz = "UTC")
      )
    },
    nesdis_record_import_run = function(...) {
      stop("adapter mode must not write import history")
    },
    .package = "AquaCache"
  )

  result <- downloadNESDIS(
    timeseries_id = 9001L,
    start_datetime = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
    end_datetime = as.POSIXct("2026-07-30 00:00:00", tz = "UTC"),
    con = structure(list(), class = "mock_con"),
    raw_messages = "ignored",
    parser = custom
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("datetime", "value"))
  expect_equal(result$value, 11)
  expect_equal(
    result$datetime,
    as.POSIXct("2026-07-29 12:00:00", tz = "UTC")
  )
})

test_that("raw DCP cache reuses a covering payload", {
  cache_dir <- tempfile("downloadNESDIS-cache-")
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)
  fetch_count <- 0L

  local_mocked_bindings(
    nesdis_cache_dir = function() cache_dir,
    nesdis_fetch_lrgs = function(...) {
      fetch_count <<- fetch_count + 1L
      list(
        message = "payload",
        server = "test-server",
        source_metadata = list(retrieval = "test")
      )
    },
    .package = "AquaCache"
  )

  first <- AquaCache:::nesdis_fetch_cached(
    dcp_address = "47002136",
    since = as.POSIXct("2026-07-29 00:00:20", tz = "UTC"),
    until = as.POSIXct("2026-07-29 02:00:15", tz = "UTC"),
    client_path = "unused",
    username = "unused",
    password = "unused",
    servers = "unused",
    port = 16003,
    timezone_offset = -8,
    timeout_seconds = 30,
    cache = TRUE
  )
  second <- AquaCache:::nesdis_fetch_cached(
    dcp_address = "47002136",
    since = as.POSIXct("2026-07-29 00:30:00", tz = "UTC"),
    until = as.POSIXct("2026-07-29 01:30:00", tz = "UTC"),
    client_path = "unused",
    username = "unused",
    password = "unused",
    servers = "unused",
    port = 16003,
    timezone_offset = -8,
    timeout_seconds = 30,
    cache = TRUE
  )

  expect_equal(first$message, "payload")
  expect_equal(second$message, "payload")
  expect_equal(fetch_count, 1L)
  expect_true(second$source_metadata$cache_hit)
})

test_that("OpenDCS client discovery honors an explicit path", {
  test_client <- make_test_lrgs_client()
  on.exit(unlink(test_client$directory, recursive = TRUE, force = TRUE), add = TRUE)

  expect_equal(
    AquaCache:::nesdis_resolve_client_path(test_client$client),
    normalizePath(test_client$client, mustWork = TRUE)
  )
})

test_that("default LRGS servers use NOAA's documented hostnames", {
  expect_equal(
    AquaCache:::nesdis_default_servers(),
    c(
      "cdadata.wcda.noaa.gov",
      "cdabackup.wcda.noaa.gov",
      "lrgseddn1.cr.usgs.gov",
      "lrgseddn2.cr.usgs.gov"
    )
  )
})

test_that("OpenDCS client discovery honors NESDIS_LRGS_CLIENT", {
  test_client <- make_test_lrgs_client()
  on.exit(unlink(test_client$directory, recursive = TRUE, force = TRUE), add = TRUE)
  withr::local_envvar(c(NESDIS_LRGS_CLIENT = test_client$client))

  expect_equal(
    AquaCache:::nesdis_resolve_client_path(),
    normalizePath(test_client$client, mustWork = TRUE)
  )
})

test_that("OpenDCS client discovery searches PATH", {
  test_client <- make_test_lrgs_client()
  on.exit(unlink(test_client$directory, recursive = TRUE, force = TRUE), add = TRUE)
  withr::local_envvar(c(
    NESDIS_LRGS_CLIENT = NA,
    DCSTOOL_HOME = NA,
    PATH = test_client$directory
  ))

  expect_equal(
    AquaCache:::nesdis_resolve_client_path(),
    normalizePath(test_client$client, mustWork = TRUE)
  )
})

test_that("OpenDCS client discovery searches DCSTOOL_HOME", {
  test_client <- make_test_lrgs_client()
  dcstool_home <- tempfile("DCSTOOL_HOME-")
  dir.create(file.path(dcstool_home, "bin"), recursive = TRUE)
  dcstool_client <- file.path(
    dcstool_home,
    "bin",
    basename(test_client$client)
  )
  file.copy(test_client$client, dcstool_client)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(dcstool_client, mode = "0755")
  }
  on.exit(
    unlink(
      c(test_client$directory, dcstool_home),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  withr::local_envvar(c(
    NESDIS_LRGS_CLIENT = NA,
    DCSTOOL_HOME = dcstool_home,
    PATH = ""
  ))

  expect_equal(
    AquaCache:::nesdis_resolve_client_path(),
    normalizePath(dcstool_client, mustWork = TRUE)
  )
})

test_that("OpenDCS client discovery searches versioned standard installs", {
  test_client <- make_test_lrgs_client()
  install_root <- tempfile("opendcs-standard-")
  old_bin <- file.path(install_root, "7.0.16", "opendcs-7.0.16", "bin")
  new_bin <- file.path(install_root, "7.0.17", "opendcs-7.0.17", "bin")
  dir.create(old_bin, recursive = TRUE)
  dir.create(new_bin, recursive = TRUE)
  old_client <- file.path(old_bin, basename(test_client$client))
  new_client <- file.path(new_bin, basename(test_client$client))
  file.copy(test_client$client, old_client)
  file.copy(test_client$client, new_client)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(c(old_client, new_client), mode = "0755")
  }
  Sys.setFileTime(old_client, as.POSIXct("2026-01-01", tz = "UTC"))
  Sys.setFileTime(new_client, as.POSIXct("2026-02-01", tz = "UTC"))
  on.exit(
    unlink(
      c(test_client$directory, install_root),
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  withr::local_envvar(c(
    NESDIS_LRGS_CLIENT = NA,
    DCSTOOL_HOME = NA,
    PATH = ""
  ))

  expect_equal(
    AquaCache:::nesdis_resolve_client_path(
      standard_install_roots = install_root
    ),
    normalizePath(new_client, mustWork = TRUE)
  )
})

test_that("OpenDCS batch clients are invoked directly on Windows", {
  skip_if(.Platform$OS.type != "windows")

  client <- tempfile(fileext = ".bat")
  on.exit(unlink(client, force = TRUE), add = TRUE)
  writeLines(
    c(
      "@echo off",
      ":check_args",
      "if \"%~1\"==\"\" goto payload",
      "if /I \"%~1\"==\"-v\" exit /b 31",
      "if /I \"%~1\"==\"-l\" exit /b 32",
      "shift",
      "goto check_args",
      ":payload",
      "echo cli: password=test-password",
      'echo 4700000126209131337G4201NN123EAB00042":NESDIS_TEST 0 I15 42'
    ),
    client,
    useBytes = TRUE
  )

  fetched <- AquaCache:::nesdis_fetch_lrgs(
    dcp_address = "47000001",
    since = as.POSIXct("2026-07-28 00:00:00", tz = "UTC"),
    until = as.POSIXct("2026-07-28 01:00:00", tz = "UTC"),
    client_path = client,
    username = "test-user",
    password = "test-password",
    servers = "127.0.0.1",
    port = 16003,
    timezone_offset = -8,
    timeout_seconds = 30
  )

  expect_equal(fetched$server, "127.0.0.1")
  expect_match(fetched$message, "NESDIS_TEST", fixed = TRUE)
  expect_false(grepl("test-password", fetched$message, fixed = TRUE))
  expect_match(fetched$message, "<redacted>", fixed = TRUE)
})

test_that("OpenDCS exception output is reported as a retrieval failure", {
  skip_if(.Platform$OS.type != "windows")

  client <- tempfile(fileext = ".bat")
  on.exit(unlink(client, force = TRUE), add = TRUE)
  writeLines(
    c(
      "@echo off",
      "echo Exception while attempting to start gdm: java.lang.RuntimeException: Unable to open log file."
    ),
    client,
    useBytes = TRUE
  )

  expect_error(
    AquaCache:::nesdis_fetch_lrgs(
      dcp_address = "47000001",
      since = as.POSIXct("2026-07-28 00:00:00", tz = "UTC"),
      until = as.POSIXct("2026-07-28 01:00:00", tz = "UTC"),
      client_path = client,
      username = "test-user",
      password = "test-password",
      servers = "127.0.0.1",
      port = 16003,
      timezone_offset = -8,
      timeout_seconds = 30
    ),
    "Unable to open log file",
    fixed = TRUE
  )
})

test_that("OpenDCS can use unauthenticated DDS access", {
  skip_if(.Platform$OS.type != "windows")

  client <- tempfile(fileext = ".bat")
  on.exit(unlink(client, force = TRUE), add = TRUE)
  writeLines(
    c(
      "@echo off",
      ":check_args",
      "if \"%~1\"==\"\" goto payload",
      "if \"%~1\"==\"-P\" exit /b 33",
      "shift",
      "goto check_args",
      ":payload",
      'echo 4700000126209131337G4201NN123EAB00042":NESDIS_TEST 0 I15 42'
    ),
    client,
    useBytes = TRUE
  )

  fetched <- AquaCache:::nesdis_fetch_lrgs(
    dcp_address = "47000001",
    since = as.POSIXct("2026-07-28 00:00:00", tz = "UTC"),
    until = as.POSIXct("2026-07-28 01:00:00", tz = "UTC"),
    client_path = client,
    username = "test-user",
    password = "",
    servers = "127.0.0.1",
    port = 16003,
    timezone_offset = -8,
    timeout_seconds = 30
  )

  expect_match(fetched$message, "NESDIS_TEST", fixed = TRUE)
})
