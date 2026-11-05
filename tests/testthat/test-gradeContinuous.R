# Pure-function tests for gradeContinuous() helpers and rules. They need no
# database; the database path was exercised against a full copy of NWT's
# database inside rolled-back transactions.

hourly <- function(n, start = "2026-01-01 00:00:00") {
  as.POSIXct(start, tz = "UTC") + (seq_len(n) - 1) * 3600
}
ctx_hourly <- list(utc_offset = -7, rate_s = 3600)

test_that("rolling helpers match brute force and zoo's alignment", {
  x <- c(1, 4, 2, 8, 5, 7, NA, 3, 6, 9, 2, 4)
  k <- 5
  centred <- vapply(
    seq_along(x),
    function(i) {
      if (i - 2 < 1 || i + 2 > length(x)) {
        return(NA_real_)
      }
      mean(x[(i - 2):(i + 2)])
    },
    numeric(1)
  )
  trailing <- vapply(
    seq_along(x),
    function(i) {
      if (i < k) {
        return(NA_real_)
      }
      mean(x[(i - k + 1):i])
    },
    numeric(1)
  )
  expect_equal(qc_roll_mean(x, k), centred)
  expect_equal(qc_roll_mean(x, k, align = "right"), trailing)

  # Even width: zoo's centre is offsets -(k/2 - 1) .. k/2.
  k <- 4
  even <- vapply(
    seq_along(x),
    function(i) {
      if (i - 1 < 1 || i + 2 > length(x)) {
        return(NA_real_)
      }
      mean(x[(i - 1):(i + 2)])
    },
    numeric(1)
  )
  expect_equal(qc_roll_mean(x, k), even)

  sd_bf <- vapply(
    seq_along(x),
    function(i) {
      if (i - 2 < 1 || i + 2 > length(x)) {
        return(NA_real_)
      }
      stats::sd(x[(i - 2):(i + 2)])
    },
    numeric(1)
  )
  expect_equal(qc_roll_sd(x, 5), sd_bf)
})

test_that("rolling windows give no result across a gap in the record", {
  dt <- as.POSIXct("2026-01-01", tz = "UTC") + c(0:9, 15:24) * 3600
  expect_equal(
    qc_window_span_ok(dt, 3, "right", 3600),
    c(FALSE, FALSE, rep(TRUE, 8), FALSE, FALSE, rep(TRUE, 8))
  )
  expect_equal(
    qc_window_span_ok(dt, 5, "center", 3600),
    c(FALSE, FALSE, rep(TRUE, 6), rep(FALSE, 4), rep(TRUE, 6), FALSE, FALSE)
  )
})

test_that("one code per reading by severity", {
  a <- c(NA, "CT", "CT", NA, "SUS")
  b <- c("PSM", "PSM", NA, NA, "ERROR")
  expect_equal(qc_combine_codes(list(a, b)), c("PSM", "PSM", "CT", NA, "ERROR"))
})

test_that("runs become ranges, single readings included", {
  dt <- hourly(6)
  r <- qc_runs_to_ranges(dt, c("UNS", "UNS", "CT", "UNS", "PSM", "PSM"))
  expect_equal(r$code, c("UNS", "CT", "UNS", "PSM"))
  expect_equal(r$start_dt, dt[c(1, 3, 4, 5)])
  expect_equal(r$end_dt, dt[c(2, 3, 4, 6)])
  expect_equal(r$n, c(2L, 1L, 1L, 2L))
  expect_equal(nrow(qc_runs_to_ranges(dt[0], character(0))), 0L)
})

test_that("protected readings get no new range, and break runs", {
  dt <- hourly(6)
  prot <- data.frame(start_dt = dt[3], end_dt = dt[4])
  held <- qc_covered(dt, prot)
  expect_equal(held, c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(qc_covered(dt, prot[0, ]), rep(FALSE, 6))
  # All UNS: without the mask this would be one range over the protected pair.
  r <- qc_runs_to_ranges(dt, ifelse(held, NA_character_, "UNS"))
  r <- r[!is.na(r$code), ]
  expect_equal(r$start_dt, dt[c(1, 5)])
  expect_equal(r$end_dt, dt[c(2, 6)])
  expect_equal(qc_pg_text_array(c("UNS", 'a"b')), '{"UNS","a\\"b"}')
})

test_that("rain while cold is CT", {
  x <- data.frame(
    datetime = hourly(4),
    value = c(0.2, 0, 0.5, 0.3),
    air_temp = c(-1, -5, 2, NA)
  )
  a <- list(max_air_temp = -0.1, qualifier = "CT")
  expect_equal(qc_rain_cold(x, a, ctx_hourly), c("CT", NA, NA, NA))
})

test_that("stuck sensor flags the middle of a constant run, splitting zeros", {
  a <- list(
    width = 5,
    min_sd = 0.001,
    qualifier = "ERROR",
    zero_split = TRUE,
    zero_max_air_temp = 0,
    zero_qualifier = "SUS"
  )
  x <- data.frame(
    datetime = hourly(16),
    value = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 5, 0, 0, 0, 0, 0, 0),
    air_temp = c(rep(5, 10), -2, -2, -2, 3, 3, 3)
  )
  out <- qc_stuck_sensor(x, a, ctx_hourly)
  # Six 3s -> the middle two are stuck (N - 4).
  expect_equal(out[1:10], c(NA, NA, NA, NA, "ERROR", "ERROR", NA, NA, NA, NA))
  # Zeros: stuck where the full window fits; SUS when cold, nothing when warm.
  expect_equal(out[13:14], c("SUS", NA))
})

test_that("direction while calm is ERROR", {
  x <- data.frame(
    datetime = hourly(3),
    value = c(90, 180, 270),
    wind_speed = c(0, 1.5, NA)
  )
  a <- list(max_wind_speed = 0, qualifier = "ERROR")
  expect_equal(qc_dir_calm(x, a, ctx_hourly), c("ERROR", NA, NA))
})

test_that("snowmelt: trailing mean flags warm rain after a cold spell", {
  n <- 60
  x <- data.frame(
    datetime = hourly(n, "2026-03-01 07:00:00"),
    value = 0,
    air_temp = c(rep(-10, 50), rep(3, 10))
  )
  x$value[55] <- 1.2
  a <- list(
    first_warm_days = 0,
    cold_window = 24,
    cold_align = "trailing",
    qualifier = "PSM"
  )
  out <- qc_snowmelt_tips(x, a, ctx_hourly)
  expect_equal(which(!is.na(out)), 55L)
  # A gap inside the window means no mean, so no flag.
  x_gap <- x[-(40:45), ]
  out_gap <- qc_snowmelt_tips(x_gap, a, ctx_hourly)
  expect_true(all(is.na(out_gap)))
})

test_that("automatic QC policies use conservative, rate-aware settings", {
  policy <- qc_auto_rule_policy()
  rules <- stats::setNames(
    policy,
    vapply(policy, `[[`, character(1), "rule_fx")
  )

  expect_equal(
    rules$qc_rain_cold$arguments(data.frame(rate_s = 3600))$max_air_temp,
    -0.1
  )
  expect_equal(
    rules$qc_dir_calm$arguments(data.frame(rate_s = 3600))$max_wind_speed,
    0
  )
  expect_equal(
    rules$qc_snowmelt_tips$arguments(data.frame(rate_s = 3600))$cold_window,
    720L
  )
  expect_equal(
    rules$qc_snowmelt_tips$arguments(data.frame(rate_s = 86400))$cold_window,
    30L
  )
  expect_null(rules$qc_snowmelt_tips$arguments(data.frame(rate_s = NA_real_)))
  expect_false("qc_stuck_sensor" %in% names(rules))
})

test_that("QC rule versions declare compatible primary parameters", {
  expect_identical(
    qc_rule_primary_parameters("qc_rain_cold", 1L),
    "precipitation, rain"
  )
  expect_identical(
    qc_rule_primary_parameters("qc_dir_calm", 1L),
    "wind direction (direction from, expressed 0-360 deg)"
  )
  expect_setequal(
    qc_rule_primary_parameters("qc_snowmelt_tips", 1L),
    c("precipitation, rain", "precipitation, total")
  )
  expect_length(qc_rule_primary_parameters("qc_stuck_sensor", 1L), 0L)
})

test_that("rule arguments merge catalogue defaults with assignment overrides", {
  skip_if_not_installed("jsonlite")
  schema <- jsonlite::toJSON(
    list(
      schema_version = 2L,
      arguments = list(
        list(
          name = "width",
          source = "user",
          label = "Window width",
          help = "Number of readings in the rolling window.",
          value_type = "integer",
          control = "numeric",
          required = TRUE,
          default = 5L,
          minimum = 2,
          step = 1
        ),
        list(
          name = "min_sd",
          source = "user",
          label = "Minimum SD",
          help = "Standard deviation threshold.",
          value_type = "numeric",
          control = "numeric",
          required = TRUE,
          default = 0.001,
          minimum = 0,
          step = 0.001
        )
      ),
      companions = list()
    ),
    auto_unbox = TRUE,
    null = "null"
  )
  expect_equal(qc_merge_rule_args(schema, NA), list(width = 5L, min_sd = 0.001))
  expect_equal(
    qc_merge_rule_args(schema, '{"min_sd":0.1}'),
    list(width = 5L, min_sd = 0.1)
  )
  expect_error(
    qc_merge_rule_args(schema, '{"unknown":1}'),
    "Unknown QC argument"
  )
  expect_error(
    qc_merge_rule_args(schema, '{"width":1.5}'),
    "invalid value"
  )
})


test_that("companion roles are schema-declared and conditional", {
  schema <- list(
    schema_version = 2L,
    arguments = list(list(
      name = "zero_split",
      source = "user",
      label = "Split zero readings",
      help = "Use air temperature for stuck zero readings.",
      value_type = "logical",
      control = "checkbox",
      required = TRUE,
      default = FALSE
    )),
    companions = list(list(
      role = "air_temp",
      label = "Air temperature",
      parameter_name = "temperature, air",
      selection_strategy = "same_location_lowest_sensor_priority",
      allow_explicit_override = TRUE,
      missing_behavior = "warn_and_use_na",
      help = "Used when zero-splitting is enabled.",
      when = list(argument = "zero_split", equals = TRUE)
    ))
  )
  validateQCRuleArgumentSchema(schema)
  expect_length(qc_rule_active_companions(schema, list(zero_split = FALSE)), 0L)
  expect_identical(
    vapply(
      qc_rule_active_companions(schema, list(zero_split = TRUE)),
      `[[`,
      character(1),
      "role"
    ),
    "air_temp"
  )
  expect_equal(
    qc_rule_companion_overrides(
      '{"air_temp":101}',
      schema,
      list(zero_split = TRUE)
    ),
    list(air_temp = 101L)
  )
  expect_error(
    qc_rule_companion_overrides(
      '{"air_temp":101}',
      schema,
      list(zero_split = FALSE)
    ),
    "Unknown or inactive"
  )
  expect_error(
    qc_rule_companion_overrides(
      '{"air_temp":1.5}',
      schema,
      list(zero_split = TRUE)
    ),
    "one positive integer"
  )
})

test_that("rule dependencies and rolling-window arguments are validated", {
  stuck <- qc_rule_registry[["qc_stuck_sensor@1"]]
  expect_error(
    stuck$window(list(width = 1)),
    "integer greater than or equal to 2"
  )
  expect_error(
    qc_validate_integer_arg(list(width = 2147483648), "width", 2L),
    "integer greater than or equal to 2"
  )

  snowmelt <- qc_rule_registry[["qc_snowmelt_tips@1"]]
  expect_error(
    snowmelt$window(list(
      cold_window = 0,
      first_warm_days = 0,
      cold_align = "trailing"
    )),
    "cold_window"
  )
  expect_error(
    snowmelt$window(list(
      cold_window = 1,
      first_warm_days = -1,
      cold_align = "trailing"
    )),
    "first_warm_days"
  )
  expect_error(
    snowmelt$window(list(
      cold_window = 1,
      first_warm_days = 0,
      cold_align = "centered"
    )),
    "cold_align"
  )
})

test_that("empty import datetime vectors skip the grading hook", {
  expect_identical(
    grade_after_import_if_data(NULL, 1L, as.POSIXct(character(), tz = "UTC")),
    FALSE
  )
  expect_identical(grade_after_import_if_data(NULL, 1L, NULL), FALSE)
  expect_identical(
    grade_after_import_if_data(
      NULL,
      1L,
      as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
    ),
    FALSE
  )
})

test_that("qualifier exclusion validation is scoped to the QC constraint", {
  skip_if(
    Sys.getenv("AQUACACHE_INTEGRATION_DATABASE") != "testdb",
    "testdb integration opt-in required"
  )
  skip_if_not_installed("RPostgres")
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "testdb",
    host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass")
  )
  on.exit(DBI::dbDisconnect(con))
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS db,
            inet_server_addr()::text AS host"
  )
  expect_identical(identity$db, "testdb")
  expect_identical(identity$host, "10.250.12.154/32")
  constraint <- DBI::dbGetQuery(
    con,
    "SELECT condeferrable, condeferred
     FROM pg_constraint
     WHERE conrelid = 'continuous.qualifiers'::regclass
       AND conname = 'qualifiers_no_overlap'"
  )
  skip_if(
    nrow(constraint) != 1L,
    "qualifier exclusion constraint is unavailable"
  )
  expect_true(constraint$condeferrable)
  expect_true(constraint$condeferred)

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id
     FROM continuous.timeseries
     ORDER BY timeseries_id
     LIMIT 1"
  )$timeseries_id
  qualifier_type_id <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id
     FROM public.qualifier_types
     WHERE qualifier_type_code = 'UNS'"
  )$qualifier_type_id
  skip_if(
    !length(tsid) || !length(qualifier_type_id),
    "testdb lacks qualifier fixtures"
  )

  DBI::dbExecute(con, "BEGIN")
  on.exit(DBI::dbExecute(con, "ROLLBACK"), add = TRUE, after = FALSE)
  DBI::dbExecute(
    con,
    "INSERT INTO continuous.qualifiers
       (timeseries_id, qualifier_type_id, start_dt, end_dt, no_source_update)
     VALUES ($1, $2, '2999-01-01 00:00:00+00', '2999-01-01 01:00:00+00', FALSE)",
    params = list(tsid, qualifier_type_id)
  )
  DBI::dbExecute(con, "SAVEPOINT qc_overlap_probe")
  DBI::dbExecute(
    con,
    "INSERT INTO continuous.qualifiers
       (timeseries_id, qualifier_type_id, start_dt, end_dt, no_source_update)
     VALUES ($1, $2, '2999-01-01 00:30:00+00', '2999-01-01 01:30:00+00', FALSE)",
    params = list(tsid, qualifier_type_id)
  )
  expect_error(qc_validate_qualifier_overlaps(con), "qualifiers_no_overlap")
  DBI::dbExecute(con, "ROLLBACK TO SAVEPOINT qc_overlap_probe")
  DBI::dbExecute(con, "RELEASE SAVEPOINT qc_overlap_probe")
  expect_true(qc_validate_qualifier_overlaps(con))
})
