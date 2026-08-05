test_that("adapter capabilities require database Patch 56", {
  local_mocked_bindings(
    dbGetQuery = function(...) stop("relation does not exist"),
    .package = "DBI"
  )

  expect_error(
    getSourceAdapterCapabilities(
      con = structure(list(), class = "mock_con")
    ),
    "requires database Patch 56"
  )
})

test_that("only the runtime registry reader is public", {
  exports <- getNamespaceExports("AquaCache")

  expect_true("getSourceAdapterCapabilities" %in% exports)
  expect_false(any(c(
    "sourceAdapterArgument",
    "registerSourceAdapterArguments",
    "validateSourceAdapterArgumentSchema"
  ) %in% exports))
})

test_that("adapter capabilities are parsed from the Patch 56 registry", {
  query_count <- 0L
  local_mocked_bindings(
    dbGetQuery = function(con, statement, ...) {
      query_count <<- query_count + 1L
      data.frame(
        source_fx = "downloadNESDIS",
        data_domain = "continuous",
        adapter_kind = "transmission",
        requires_transmission_mapping = TRUE,
        inject_timeseries_id = TRUE,
        parallel_group_strategy = "source_args",
        parallel_group_args_json = '["station","interval"]',
        allow_empty_initial_fetch = TRUE,
        transmission_method_codes_json = '["IRIDIUM_SBD"]',
        argument_schema_json = paste0(
          '{"schema_version":1,"arguments":[',
          '{"name":"timeseries_id","source":"runtime",',
          '"help":"Injected from the import queue."}]}'
        ),
        ui_config_json = '{"provider_name":"Example"}',
        enabled = TRUE,
        note = NA_character_
      )
    },
    .package = "DBI"
  )

  capabilities <- getSourceAdapterCapabilities(
    con = structure(list(), class = "mock_con"),
    source_fx = "downloadNESDIS",
    data_domain = "continuous"
  )

  expect_equal(query_count, 1L)
  expect_equal(capabilities$parallel_group_args[[1]], c("station", "interval"))
  expect_equal(
    capabilities$transmission_method_codes[[1]],
    "IRIDIUM_SBD"
  )
  expect_equal(capabilities$ui_config[[1]]$provider_name, "Example")
  expect_equal(
    capabilities$argument_schema[[1]]$arguments[[1]]$name,
    "timeseries_id"
  )
  expect_equal(capabilities$data_domain, "continuous")
})

test_that("adapter capability domains are validated", {
  expect_error(
    getSourceAdapterCapabilities(
      con = structure(list(), class = "mock_con"),
      data_domain = "document"
    ),
    "data_domain must contain only"
  )
})

test_that("Patch 56 creates and seeds the source adapter registry", {
  patch <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "patches", "patch_56.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    patch,
    "CREATE TABLE public.source_adapter_capabilities",
    fixed = TRUE
  )
  expect_match(patch, "PRIMARY KEY (source_fx, data_domain)", fixed = TRUE)
  expect_match(
    patch,
    "data_domain IN ('continuous', 'discrete', 'image', 'raster')",
    fixed = TRUE
  )
  expect_match(patch, "argument_schema JSONB", fixed = TRUE)
  expect_match(
    patch,
    "registerSourceAdapterArguments(",
    fixed = TRUE
  )
  expect_match(patch, "Aquarius location identifier", fixed = TRUE)
  expect_match(patch, "AQSERVER from the R environment", fixed = TRUE)
  expected_adapters <- c(
    "downloadAquarius",
    "downloadECCCwx",
    "downloadECCCwxMinute",
    "downloadNESDIS",
    "downloadNWIS",
    "downloadRWIS",
    "downloadWSC",
    "downloadECCCwq",
    "downloadEQWin",
    "downloadSnowCourse",
    "downloadNupointImages",
    "downloadWSCImages",
    "downloadCaLDAS",
    "downloadERA5",
    "downloadHRDPA",
    "downloadHRDPS"
  )
  for (source_fx in expected_adapters) {
    expect_match(patch, paste0("'", source_fx, "'"), fixed = TRUE)
  }
  expect_match(patch, "ARRAY['GOES_DCS']", fixed = TRUE)
  expect_match(
    patch,
    "CREATE TABLE continuous.timeseries_source_adapters",
    fixed = TRUE
  )
  expect_match(
    patch,
    "CREATE TABLE discrete.sample_series_source_adapters",
    fixed = TRUE
  )
  expect_match(
    patch,
    "CREATE TABLE files.image_series_source_adapters",
    fixed = TRUE
  )
  expect_match(
    patch,
    "CREATE TABLE spatial.raster_series_source_adapters",
    fixed = TRUE
  )
  expect_match(
    patch,
    "ALTER TABLE files.image_series\n       DROP COLUMN source_fx",
    fixed = TRUE
  )
  expect_match(
    patch,
    "audit_source_adapter_capabilities_trigger",
    fixed = TRUE
  )
})

test_that("sourceAdapterArgument constructs validated descriptors", {
  argument <- sourceAdapterArgument(
    name = "location",
    source = "user",
    help = "Provider station identifier.",
    label = "Station ID",
    value_type = "character",
    control = "text",
    required = TRUE
  )

  expect_identical(argument$name, "location")
  expect_identical(argument$source, "user")
  expect_true(argument$required)
  expect_error(
    sourceAdapterArgument(
      name = "location",
      source = "user",
      help = "",
      label = "Station ID",
      value_type = "character",
      control = "text"
    ),
    "requires non-blank help text"
  )
})

test_that("registerSourceAdapterArguments validates and updates one row", {
  executed <- NULL
  local_mocked_bindings(
    dbExecute = function(con, statement, params, ...) {
      executed <<- list(statement = statement, params = params)
      1L
    },
    .package = "DBI"
  )
  argument <- sourceAdapterArgument(
    name = "location",
    source = "user",
    help = "Aquarius location identifier.",
    label = "Location",
    value_type = "character",
    control = "text",
    required = TRUE
  )

  schema <- registerSourceAdapterArguments(
    con = structure(list(), class = "mock_con"),
    source_fx = "downloadAquarius",
    data_domain = "continuous",
    arguments = list(argument)
  )

  expect_identical(schema$schema_version, 1L)
  expect_identical(schema$arguments[[1]], argument)
  expect_match(executed$statement, "SET argument_schema = $1::jsonb", fixed = TRUE)
  expect_identical(executed$params[[2]], "downloadAquarius")
  expect_identical(executed$params[[3]], "continuous")
  expect_equal(
    jsonlite::fromJSON(executed$params[[1]])$arguments$name,
    "location"
  )
})

test_that("registerSourceAdapterArguments requires exactly one registry row", {
  local_mocked_bindings(
    dbExecute = function(...) 0L,
    .package = "DBI"
  )
  argument <- sourceAdapterArgument(
    name = "start_datetime",
    source = "runtime",
    help = "AquaCache supplies the synchronization start."
  )

  expect_error(
    registerSourceAdapterArguments(
      con = structure(list(), class = "mock_con"),
      source_fx = "downloadAquarius",
      data_domain = "continuous",
      arguments = list(argument)
    ),
    "updated 0"
  )
})

test_that("typed source arguments remain typed when decoded", {
  args <- source_adapter_args_decode(
    '{"station":"2101300","difference":true,"reset_drop":20,"hrs":[0,6]}'
  )

  expect_identical(args$station, "2101300")
  expect_true(args$difference)
  expect_identical(args$reset_drop, 20L)
  expect_identical(args$hrs, c(0L, 6L))
})
