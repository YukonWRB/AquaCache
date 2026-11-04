test_that("QC assignment APIs validate and persist versioned settings transactionally", {
  skip_if(Sys.getenv("AQUACACHE_INTEGRATION_DATABASE") != "testdb",
          "testdb integration opt-in required")
  skip_if_not_installed("RPostgres")
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "testdb",
    host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS db, inet_server_addr()::text AS host"
  )
  expect_identical(identity$db, "testdb")
  expect_identical(identity$host, "10.250.12.154/32")
  if (DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.qc_rule_capabilities') IS NOT NULL AS exists"
  )$exists[[1L]]) {
    skip("QC schema already exists; use the migrated-schema integration test")
  }

  series <- DBI::dbGetQuery(
    con,
    "SELECT target.timeseries_id, target.location_id, air.timeseries_id AS air_temp_id
     FROM continuous.timeseries target
     JOIN public.parameters target_parameter
       ON target_parameter.parameter_id = target.parameter_id
     JOIN continuous.timeseries air
       ON air.location_id = target.location_id
     JOIN public.parameters air_parameter
       ON air_parameter.parameter_id = air.parameter_id
     WHERE target.active AND air.active
       AND target.timeseries_id <> air.timeseries_id
       AND air_parameter.param_name = 'temperature, air'
     ORDER BY target.timeseries_id, air.sensor_priority NULLS LAST,
       air.timeseries_id
     LIMIT 1"
  )
  skip_if(nrow(series) != 1L, "testdb lacks same-location air-temperature fixtures")

  schema <- list(
    schema_version = 2L,
    arguments = list(
      list(
        name = "width", source = "user", label = "Window width",
        help = "Number of readings in the rolling window.",
        value_type = "integer", control = "numeric", required = TRUE,
        default = 5L, minimum = 2, step = 1
      ),
      list(
        name = "min_sd", source = "user", label = "Maximum standard deviation",
        help = "A rolling standard deviation below this value is treated as a stuck sensor.",
        value_type = "numeric", control = "numeric", required = TRUE,
        default = 0.001, minimum = 0, step = 0.001
      ),
      list(
        name = "zero_split", source = "user", label = "Split zero readings",
        help = "Use air temperature to classify stuck zero readings.",
        value_type = "logical", control = "checkbox", required = TRUE,
        default = FALSE
      ),
      list(
        name = "zero_max_air_temp", source = "user", label = "Cold threshold",
        help = "Below this value, stuck zeros are classified as frozen.",
        value_type = "numeric", control = "numeric", required = TRUE,
        default = 0, step = 0.1,
        visible_when = list(argument = "zero_split", equals = TRUE)
      ),
      list(
        name = "qualifier", source = "internal",
        help = "Qualifier written for a stuck sensor.", default = "ERROR"
      ),
      list(
        name = "zero_qualifier", source = "internal",
        help = "Qualifier written for a cold stuck zero.", default = "SUS"
      )
    ),
    companions = list(list(
      role = "air_temp", label = "Air temperature",
      parameter_name = "temperature, air",
      selection_strategy = "same_location_lowest_sensor_priority",
      allow_explicit_override = TRUE,
      missing_behavior = "warn_and_use_na",
      help = "Used only when zero-splitting is enabled.",
      when = list(argument = "zero_split", equals = TRUE)
    ))
  )
  schema_json <- as.character(jsonlite::toJSON(
    schema, auto_unbox = TRUE, null = "null", digits = NA
  ))

  DBI::dbBegin(con)
  on.exit(try(DBI::dbRollback(con), silent = TRUE), add = TRUE, after = FALSE)
  DBI::dbGetQuery(
    con,
    "SELECT pg_advisory_xact_lock(hashtext('qc-rule-assignment-integration'))"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE public.qc_rule_capabilities (
       rule_fx TEXT NOT NULL,
       rule_version INTEGER NOT NULL,
       description TEXT NOT NULL,
       emits TEXT[] NOT NULL,
       argument_schema JSONB NOT NULL,
       enabled BOOLEAN NOT NULL DEFAULT TRUE,
       note TEXT,
       PRIMARY KEY (rule_fx, rule_version)
     )"
  )
  DBI::dbExecute(
    con,
    "CREATE FUNCTION public.qc_rule_version_immutable()
     RETURNS trigger
     LANGUAGE plpgsql
     AS $qc_rule_version_immutable$
     BEGIN
       IF NEW.rule_fx IS DISTINCT FROM OLD.rule_fx
         OR NEW.rule_version IS DISTINCT FROM OLD.rule_version
         OR NEW.emits IS DISTINCT FROM OLD.emits
         OR NEW.argument_schema IS DISTINCT FROM OLD.argument_schema
       THEN
         RAISE EXCEPTION
           'QC rule semantics are versioned; create a new rule_version instead.'
           USING ERRCODE = 'check_violation';
       END IF;
       RETURN NEW;
     END;
     $qc_rule_version_immutable$"
  )
  DBI::dbExecute(
    con,
    "CREATE TRIGGER qc_rule_capabilities_version_immutable
     BEFORE UPDATE ON public.qc_rule_capabilities
     FOR EACH ROW EXECUTE FUNCTION public.qc_rule_version_immutable()"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO public.qc_rule_capabilities
       (rule_fx, rule_version, description, emits, argument_schema)
     VALUES ('qc_stuck_sensor', 1, 'Stuck sensor', '{ERROR,SUS}', $1::jsonb)",
    params = list(schema_json)
  )
  unrestricted_schema_json <- as.character(jsonlite::toJSON(
    list(schema_version = 2L, arguments = list(), companions = list()),
    auto_unbox = TRUE, null = "null", digits = NA
  ))
  DBI::dbExecute(
    con,
    "INSERT INTO public.qc_rule_capabilities
       (rule_fx, rule_version, description, emits, argument_schema)
     VALUES ('qc_dir_calm', 1, 'Calm wind direction', '{ERROR}', $1::jsonb)",
    params = list(unrestricted_schema_json)
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE continuous.timeseries_qc_rules (
       timeseries_qc_rule_id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
       timeseries_id INTEGER NOT NULL
         REFERENCES continuous.timeseries(timeseries_id) ON DELETE CASCADE,
       rule_fx TEXT NOT NULL,
       rule_version INTEGER NOT NULL,
       rule_args JSONB NOT NULL CHECK (jsonb_typeof(rule_args) = 'object'),
       companion_timeseries JSONB NOT NULL DEFAULT '{}'::jsonb
         CHECK (jsonb_typeof(companion_timeseries) = 'object'),
       run_order SMALLINT NOT NULL CHECK (run_order > 0),
       active BOOLEAN NOT NULL DEFAULT TRUE,
       note TEXT,
       FOREIGN KEY (rule_fx, rule_version)
         REFERENCES public.qc_rule_capabilities(rule_fx, rule_version)
     )"
  )
  DBI::dbExecute(
    con,
    "CREATE UNIQUE INDEX timeseries_qc_rules_active_order_key
     ON continuous.timeseries_qc_rules (timeseries_id, run_order) WHERE active"
  )

  candidates <- getQCRuleCompanionCandidates(
    con,
    series$location_id[[1L]],
    "temperature, air",
    exclude_timeseries_id = series$timeseries_id[[1L]]
  )
  expect_true(series$air_temp_id[[1L]] %in% candidates$timeseries_id)

  assignment <- data.frame(
    rule_fx = "qc_stuck_sensor",
    rule_version = 1L,
    run_order = 1L,
    active = TRUE,
    note = NA_character_,
    stringsAsFactors = FALSE
  )
  assignment$rule_args <- I(list(list(
    zero_split = TRUE,
    zero_max_air_temp = -1
  )))
  assignment$companion_timeseries <- I(list(list(
    air_temp = series$air_temp_id[[1L]]
  )))
  created_id <- setTimeseriesQCRules(
    con,
    series$timeseries_id[[1L]],
    assignment
  )
  stored <- getTimeseriesQCRules(con, series$timeseries_id[[1L]])
  expect_equal(nrow(stored), 1L)
  expect_identical(stored$timeseries_qc_rule_id[[1L]], created_id[[1L]])
  expect_equal(stored$rule_args[[1L]]$width, 5)
  expect_equal(stored$rule_args[[1L]]$zero_max_air_temp, -1)
  expect_identical(
    as.integer(stored$companion_timeseries[[1L]][["air_temp"]]),
    as.integer(series$air_temp_id[[1L]])
  )

  edited <- stored[, c(
    "timeseries_qc_rule_id", "rule_fx", "rule_version", "rule_args",
    "companion_timeseries", "run_order", "active", "note"
  )]
  edited$rule_args[[1L]] <- list(zero_split = FALSE)
  edited$companion_timeseries[[1L]] <- list()
  edited$run_order[[1L]] <- 2L
  setTimeseriesQCRules(con, series$timeseries_id[[1L]], edited)
  stored_after_edit <- getTimeseriesQCRules(con, series$timeseries_id[[1L]])
  expect_identical(stored_after_edit$timeseries_qc_rule_id[[1L]], created_id[[1L]])
  expect_false(stored_after_edit$rule_args[[1L]]$zero_split)
  expect_length(stored_after_edit$companion_timeseries[[1L]], 0L)
  expect_identical(stored_after_edit$run_order[[1L]], 2L)

  assignment_count <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.timeseries_qc_rules
     WHERE timeseries_id = $1",
    params = list(series$timeseries_id[[1L]])
  )$n[[1L]]
  qualifier_count <- DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE timeseries_id = $1",
    params = list(series$timeseries_id[[1L]])
  )$n[[1L]]
  measurement_bounds <- DBI::dbGetQuery(
    con,
    "SELECT max(datetime) AS latest
     FROM continuous.measurements_continuous
     WHERE timeseries_id = $1",
    params = list(series$timeseries_id[[1L]])
  )
  if (is.na(measurement_bounds$latest[[1L]])) {
    preview <- gradeContinuous(
      con,
      series$timeseries_id[[1L]],
      write = FALSE,
      assignments = edited
    )
  } else {
    latest <- as.POSIXct(measurement_bounds$latest[[1L]], tz = "UTC")
    preview <- gradeContinuous(
      con,
      series$timeseries_id[[1L]],
      from = latest - 7 * 24 * 3600,
      to = latest,
      write = FALSE,
      assignments = edited
    )
  }
  expect_true(is.null(preview) || is.list(preview))
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT count(*) AS n FROM continuous.timeseries_qc_rules
       WHERE timeseries_id = $1",
      params = list(series$timeseries_id[[1L]])
    )$n[[1L]],
    assignment_count
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT count(*) AS n FROM continuous.qualifiers
       WHERE timeseries_id = $1",
      params = list(series$timeseries_id[[1L]])
    )$n[[1L]],
    qualifier_count
  )
  expect_error(
    gradeContinuous(
      con,
      series$timeseries_id[[1L]],
      write = TRUE,
      assignments = edited
    ),
    "gradeContinuous"
  )

  # Adding a newer catalogue version with a changed default must not repin
  # an existing assignment when its rule_version is omitted from an edit.
  test_version_pinning <- function() {
    registry_env <- environment(getQCRuleCapabilities)
    original_registry <- get("qc_rule_registry", envir = registry_env)
    registry_was_locked <- bindingIsLocked("qc_rule_registry", registry_env)
    if (registry_was_locked) {
      unlockBinding("qc_rule_registry", registry_env)
    }
    on.exit(
      {
        if (bindingIsLocked("qc_rule_registry", registry_env)) {
          unlockBinding("qc_rule_registry", registry_env)
        }
        assign("qc_rule_registry", original_registry, envir = registry_env)
        if (registry_was_locked) {
          lockBinding("qc_rule_registry", registry_env)
        }
      },
      add = TRUE
    )
    versioned_registry <- original_registry
    versioned_registry[["qc_stuck_sensor@2"]] <-
      original_registry[["qc_stuck_sensor@1"]]
    assign("qc_rule_registry", versioned_registry, envir = registry_env)
    schema_v2 <- schema
    schema_v2$arguments[[1L]]$default <- 7L
    schema_v2_json <- as.character(jsonlite::toJSON(
      schema_v2, auto_unbox = TRUE, null = "null", digits = NA
    ))
    DBI::dbExecute(
      con,
      "INSERT INTO public.qc_rule_capabilities
         (rule_fx, rule_version, description, emits, argument_schema)
       VALUES ('qc_stuck_sensor', 2, 'Stuck sensor v2', '{ERROR,SUS}', $1::jsonb)",
      params = list(schema_v2_json)
    )
    assignments_without_versions <- data.frame(
      timeseries_qc_rule_id = c(
        stored_after_edit$timeseries_qc_rule_id[[1L]], NA_integer_
      ),
      rule_fx = c("qc_stuck_sensor", "qc_stuck_sensor"),
      run_order = c(2L, 3L),
      active = c(TRUE, TRUE),
      note = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )
    assignments_without_versions$rule_args <- I(list(
      stored_after_edit$rule_args[[1L]],
      list(zero_split = FALSE)
    ))
    assignments_without_versions$companion_timeseries <- I(list(
      list(),
      list()
    ))
    setTimeseriesQCRules(
      con,
      series$timeseries_id[[1L]],
      assignments_without_versions
    )
    pinned <- getTimeseriesQCRules(con, series$timeseries_id[[1L]])
    expect_identical(as.integer(pinned$rule_version), c(1L, 2L))
    expect_equal(pinned$rule_args[[1L]]$width, 5)
    expect_equal(pinned$rule_args[[2L]]$width, 7)
    DBI::dbExecute(
      con,
      "DELETE FROM continuous.timeseries_qc_rules
       WHERE timeseries_qc_rule_id = $1",
      params = list(pinned$timeseries_qc_rule_id[[2L]])
    )
    DBI::dbExecute(
      con,
      "DELETE FROM public.qc_rule_capabilities
       WHERE rule_fx = 'qc_stuck_sensor' AND rule_version = 2"
    )
  }
  test_version_pinning()

  DBI::dbExecute(con, "SAVEPOINT qc_rule_semantics_test")
  expect_error(
    DBI::dbExecute(
      con,
      "UPDATE public.qc_rule_capabilities
       SET argument_schema = jsonb_set(argument_schema, '{arguments,0,default}', '9')
       WHERE rule_fx = 'qc_stuck_sensor' AND rule_version = 1"
    ),
    "QC rule semantics are versioned"
  )
  DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT qc_rule_semantics_test")

  invalid <- edited
  invalid$rule_args[[1L]] <- list(unknown_argument = 1)
  expect_error(
    setTimeseriesQCRules(con, series$timeseries_id[[1L]], invalid),
    "Unknown QC argument"
  )
  invalid <- edited
  invalid$rule_args[[1L]] <- list(width = 1L)
  expect_error(
    setTimeseriesQCRules(con, series$timeseries_id[[1L]], invalid),
    "below its minimum"
  )
  invalid <- edited
  invalid$rule_args[[1L]] <- list(zero_split = TRUE)
  invalid$companion_timeseries[[1L]] <- list(air_temp = series$timeseries_id[[1L]])
  expect_error(
    setTimeseriesQCRules(con, series$timeseries_id[[1L]], invalid),
    "active timeseries at the same location"
  )
  incompatible_target <- data.frame(
    rule_fx = "qc_dir_calm",
    rule_version = 1L,
    run_order = 1L,
    active = TRUE,
    note = NA_character_,
    stringsAsFactors = FALSE
  )
  incompatible_target$rule_args <- I(list(list()))
  incompatible_target$companion_timeseries <- I(list(list()))
  expect_error(
    setTimeseriesQCRules(
      con,
      series$air_temp_id[[1L]],
      incompatible_target
    ),
    "not compatible with primary parameter"
  )

  DBI::dbExecute(
    con,
    "UPDATE public.qc_rule_capabilities
     SET enabled = FALSE WHERE rule_fx = 'qc_stuck_sensor' AND rule_version = 1"
  )
  setTimeseriesQCRules(con, series$timeseries_id[[1L]], edited)
  retained <- getTimeseriesQCRules(con, series$timeseries_id[[1L]])
  expect_true(retained$active[[1L]])
  expect_false(retained$enabled[[1L]])

  setTimeseriesQCRules(
    con,
    series$timeseries_id[[1L]],
    data.frame()
  )
  inactive <- getTimeseriesQCRules(con, series$timeseries_id[[1L]])
  expect_false(any(inactive$active))
  DBI::dbRollback(con)
  expect_false(DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.qc_rule_capabilities') IS NOT NULL AS exists"
  )$exists[[1L]])
})
