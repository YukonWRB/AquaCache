test_that("saved QC rule removal applies its selected qualifier cleanup", {
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
  transaction_open <- FALSE
  on.exit({
    if (DBI::dbIsValid(con)) {
      if (transaction_open) {
        try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS db, inet_server_addr()::text AS host"
  )
  expect_identical(identity$db, "testdb")
  expect_identical(identity$host, "10.250.12.154/32")
  has_qc_schema <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.qc_rule_capabilities') IS NOT NULL
       AND to_regclass('continuous.timeseries_qc_rules') IS NOT NULL AS available"
  )$available[[1L]]
  skip_if(!has_qc_schema, "testdb does not have the QC schema")

  timeseries <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id
     FROM continuous.measurements_continuous
     GROUP BY timeseries_id
     HAVING count(*) >= 2
     ORDER BY count(*) DESC, timeseries_id
     LIMIT 1"
  )
  skip_if(nrow(timeseries) != 1L, "testdb has no timeseries with readings")
  timeseries_id <- timeseries$timeseries_id[[1L]]
  capability <- DBI::dbGetQuery(
    con,
    "SELECT rule_fx, rule_version
     FROM public.qc_rule_capabilities
     ORDER BY rule_fx, rule_version
     LIMIT 1"
  )
  best_effort_capability <- DBI::dbGetQuery(
    con,
    "SELECT rule_fx, rule_version
     FROM public.qc_rule_capabilities
     WHERE rule_fx = 'qc_stuck_sensor' AND rule_version = 1"
  )
  qualifier_types <- DBI::dbGetQuery(
    con,
    "SELECT qualifier_type_id, qualifier_type_code
     FROM public.qualifier_types
     WHERE qualifier_type_code IN ('ERROR', 'PSM')"
  )
  skip_if(
    nrow(capability) != 1L || nrow(best_effort_capability) != 1L ||
      !all(c("ERROR", "PSM") %in% qualifier_types$qualifier_type_code),
    "testdb lacks QC rule or qualifier fixtures"
  )

  transaction_open <- dbTransBegin(con)
  if (!transaction_open) {
    stop("Could not start the rollback-only test transaction.")
  }
  DBI::dbExecute(
    con,
    "DELETE FROM continuous.timeseries_qc_rules WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )
  next_order <- 100L
  next_time <- as.POSIXct(Sys.time(), tz = "UTC") + 20 * 365 * 86400
  insert_assignment <- function(
    rule_fx = capability$rule_fx[[1L]],
    rule_version = capability$rule_version[[1L]],
    rule_args = "{}"
  ) {
    next_order <<- next_order + 1L
    DBI::dbGetQuery(
      con,
      "INSERT INTO continuous.timeseries_qc_rules
         (timeseries_id, rule_fx, rule_version, rule_args,
          companion_timeseries, run_order, active, note)
       VALUES ($1, $2, $3, $4::jsonb, '{}'::jsonb, $5, FALSE,
          'transactional QC removal test')
       RETURNING timeseries_qc_rule_id",
      params = list(
        timeseries_id,
        rule_fx,
        rule_version,
        rule_args,
        next_order
      )
    )$timeseries_qc_rule_id[[1L]]
  }
  insert_qualifier <- function(
    code,
    protected = FALSE,
    start_dt = NULL,
    end_dt = NULL
  ) {
    type_id <- qualifier_types$qualifier_type_id[
      match(code, qualifier_types$qualifier_type_code)
    ]
    if (is.null(start_dt) || is.null(end_dt)) {
      next_time <<- next_time + 3600
      start_dt <- next_time
      end_dt <- next_time + 60
    }
    DBI::dbGetQuery(
      con,
      "INSERT INTO continuous.qualifiers
         (timeseries_id, qualifier_type_id, start_dt, end_dt,
          no_source_update)
       VALUES ($1, $2, $3, $4, $5)
       RETURNING qualifier_id",
      params = list(
        timeseries_id,
        type_id,
        start_dt,
        end_dt,
        protected
      )
    )$qualifier_id[[1L]]
  }

  # "leave" removes only the assignment.
  leave_rule <- insert_assignment()
  leave_qualifiers <- c(
    insert_qualifier("ERROR"),
    insert_qualifier("PSM", protected = TRUE)
  )
  removeTimeseriesQCRule(
    con,
    timeseries_id,
    leave_rule,
    action = "leave"
  )
  expect_true(dbTransCheck(con))
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.timeseries_qc_rules
     WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $2",
    params = list(timeseries_id, leave_rule)
  )$n[[1L]], 0)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE qualifier_id = ANY($1::integer[])",
    params = list(qc_pg_int_array(leave_qualifiers))
  )$n[[1L]], 2)

  # "best_effort" removes predicted ERROR output for the target rule only.
  best_effort_rule <- insert_assignment(
    rule_fx = best_effort_capability$rule_fx[[1L]],
    rule_version = best_effort_capability$rule_version[[1L]],
    rule_args = '{"width":2,"min_sd":1000000000000,"qualifier":"ERROR","zero_split":false}'
  )
  assignments <- getTimeseriesQCRules(con, timeseries_id)
  best_effort_index <- match(
    best_effort_rule,
    assignments$timeseries_qc_rule_id
  )
  best_effort_assignment <- assignments[
    best_effort_index,
    ,
    drop = FALSE
  ]
  best_effort_assignment$active <- TRUE
  preview <- gradeContinuous(
    con,
    timeseries_id,
    write = FALSE,
    assignments = best_effort_assignment
  )
  if (
    is.null(preview) || is.null(preview$ranges) ||
      !any(preview$ranges$code == "ERROR")
  ) {
    stop("testdb readings did not produce a predicted stuck-sensor range.")
  }
  existing_error_ranges <- DBI::dbGetQuery(
    con,
    "SELECT q.start_dt, q.end_dt
     FROM continuous.qualifiers q
     JOIN public.qualifier_types t USING (qualifier_type_id)
     WHERE q.timeseries_id = $1 AND t.qualifier_type_code = 'ERROR'",
    params = list(timeseries_id)
  )
  eligible_times <- preview$readings$datetime[
    preview$readings$code == "ERROR" &
      qc_covered(preview$readings$datetime, preview$ranges)
  ]
  eligible_times <- eligible_times[!qc_covered(
    eligible_times,
    existing_error_ranges
  )]
  expect_gt(length(eligible_times), 0L)
  best_effort_time <- eligible_times[[1L]]
  best_effort_qualifier <- insert_qualifier(
    "ERROR",
    start_dt = best_effort_time,
    end_dt = best_effort_time + 0.5
  )
  overlapping_protected <- insert_qualifier(
    "PSM",
    protected = TRUE,
    start_dt = best_effort_time,
    end_dt = best_effort_time + 0.5
  )
  removed <- removeTimeseriesQCRule(
    con,
    timeseries_id,
    best_effort_rule,
    action = "best_effort"
  )
  expect_true(dbTransCheck(con))
  expect_equal(removed$qualifier_rows_deleted, 1L)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.timeseries_qc_rules
     WHERE timeseries_qc_rule_id = $1",
    params = list(best_effort_rule)
  )$n[[1L]], 0)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE qualifier_id = $1",
    params = list(best_effort_qualifier)
  )$n[[1L]], 0)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE qualifier_id = $1 AND no_source_update IS TRUE",
    params = list(overlapping_protected)
  )$n[[1L]], 1)

  # "regrade_remaining" clears unprotected qualifiers and retains the other rule.
  regrade_rule <- insert_assignment()
  remaining_rule <- insert_assignment()
  regrade_qualifiers <- c(
    insert_qualifier("ERROR"),
    insert_qualifier("PSM", protected = TRUE)
  )
  removeTimeseriesQCRule(
    con,
    timeseries_id,
    regrade_rule,
    action = "regrade_remaining"
  )
  expect_true(dbTransCheck(con))
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.timeseries_qc_rules
     WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $2",
    params = list(timeseries_id, regrade_rule)
  )$n[[1L]], 0)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.timeseries_qc_rules
     WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $2",
    params = list(timeseries_id, remaining_rule)
  )$n[[1L]], 1)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE qualifier_id = $1",
    params = list(regrade_qualifiers[[2L]])
  )$n[[1L]], 1)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE qualifier_id = $1",
    params = list(regrade_qualifiers[[1L]])
  )$n[[1L]], 0)

  # "clear_unprotected" is valid only for the last saved assignment.
  DBI::dbExecute(
    con,
    "DELETE FROM continuous.timeseries_qc_rules
     WHERE timeseries_id = $1 AND timeseries_qc_rule_id = $2",
    params = list(timeseries_id, remaining_rule)
  )
  final_rule <- insert_assignment()
  final_qualifiers <- c(
    insert_qualifier("ERROR"),
    insert_qualifier("PSM", protected = TRUE)
  )
  removeTimeseriesQCRule(
    con,
    timeseries_id,
    final_rule,
    action = "clear_unprotected"
  )
  expect_true(dbTransCheck(con))
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE timeseries_id = $1
       AND no_source_update IS DISTINCT FROM TRUE",
    params = list(timeseries_id)
  )$n[[1L]], 0)
  expect_equal(DBI::dbGetQuery(
    con,
    "SELECT count(*) AS n FROM continuous.qualifiers
     WHERE qualifier_id = ANY($1::integer[])
       AND no_source_update IS TRUE",
    params = list(qc_pg_int_array(c(
      leave_qualifiers[[2L]],
      regrade_qualifiers[[2L]],
      final_qualifiers[[2L]],
      overlapping_protected
    )))
  )$n[[1L]], 4)
})
