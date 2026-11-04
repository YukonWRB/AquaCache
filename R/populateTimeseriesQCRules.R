# Automatic, conservative population of common cross-timeseries QC rules.
#
# This policy deliberately excludes qc_stuck_sensor: useful thresholds depend
# on parameter units, sensor resolution, and sampling interval, and a flatline
# can be a real environmental condition. The other rules have direct
# same-location checks with fixed, conservative defaults.

qc_auto_rule_policy <- function() {
  list(
    list(
      rule_fx = "qc_rain_cold",
      rule_version = 1L,
      primary_parameter_names = "precipitation, rain",
      arguments = function(series) list(max_air_temp = -0.3)
    ),
    list(
      rule_fx = "qc_snowmelt_tips",
      rule_version = 1L,
      primary_parameter_names = c(
        "precipitation, rain",
        "precipitation, total"
      ),
      arguments = function(series) {
        rate_s <- series$rate_s[[1L]]
        if (!is.finite(rate_s) || rate_s <= 0) return(NULL)
        # Use a 30-day cold spell at the series' configured recording rate.
        cold_window <- ceiling(30 * 24 * 60 * 60 / rate_s)
        if (!is.finite(cold_window) || cold_window > .Machine$integer.max) {
          return(NULL)
        }
        list(
          first_warm_days = 2L,
          cold_window = as.integer(max(1, cold_window)),
          cold_align = "trailing"
        )
      }
    ),
    list(
      rule_fx = "qc_dir_calm",
      rule_version = 1L,
      primary_parameter_names =
        "wind direction (direction from, expressed 0-360 deg)",
      arguments = function(series) list(max_wind_speed = 0)
    )
  )
}

qc_auto_empty_result <- function() {
  data.frame(
    timeseries_id = integer(),
    location_id = integer(),
    parameter_name = character(),
    rule_fx = character(),
    rule_version = integer(),
    action = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

qc_auto_assignment_result <- function(
  series,
  policy,
  action,
  reason
) {
  data.frame(
    timeseries_id = as.integer(series$timeseries_id[[1L]]),
    location_id = as.integer(series$location_id[[1L]]),
    parameter_name = as.character(series$param_name[[1L]]),
    rule_fx = policy$rule_fx,
    rule_version = as.integer(policy$rule_version),
    action = action,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

#' Automatically assign safe, same-location QC rules
#'
#' Idempotently adds configured rules when an eligible primary series and its
#' required active companion series exist at the same location. By default,
#' this returns a dry-run plan. Automatic assignments are added after the
#' catalogue defaults are materialized and are pinned to their rule version;
#' existing assignments, including inactive ones, are never changed.
#'
#' Current policy:
#' describe{
#'   \item{Rain below cold threshold}{Assigns \code{qc_rain_cold@1} to
#'     \code{precipitation, rain} series with same-location air temperature,
#'     using -0.3 degrees C.}
#'   \item{Possible snowmelt tips}{Assigns \code{qc_snowmelt_tips@1} to rain
#'     and total precipitation series with same-location air temperature,
#'     using a trailing 30-day cold window derived from the recording rate.}
#'   \item{Wind direction in calm conditions}{Assigns \code{qc_dir_calm@1}
#'     to wind-direction series with same-location wind speed, flagging speed
#'     at or below 0 m/s.}
#' }
#'
#' Stuck-sensor rules remain manual because their thresholds depend on units,
#' sensor resolution, and site conditions.
#'
#' @param con An open AquaCache connection.
#' @param location_ids Optional location ids to scan. NULL scans all locations
#'   with an eligible active primary series.
#' @param dry_run If TRUE, return proposed actions without writing. Set FALSE
#'   to create assignments.
#' @return A data.frame with one row per eligible primary rule. The `action`
#'   column can be `would_create`, `created`, `already_configured`,
#'   `missing_companion`, `not_eligible`, or `order_limit`.
#' @export
populateTimeseriesQCRules <- function(
  con,
  location_ids = NULL,
  dry_run = TRUE
) {
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("populateTimeseriesQCRules: dry_run must be TRUE or FALSE.")
  }
  if (!is.null(location_ids)) {
    if (!length(location_ids)) return(qc_auto_empty_result())
    location_ids <- vapply(
      location_ids,
      qc_rule_integer_id,
      integer(1),
      label = "location_id"
    )
    location_ids <- unique(location_ids)
  }

  schema_exists <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.qc_rule_capabilities') IS NOT NULL
              AND to_regclass('continuous.timeseries_qc_rules') IS NOT NULL
       AS available"
  )$available[[1L]]
  if (!isTRUE(schema_exists)) return(qc_auto_empty_result())

  policies <- qc_auto_rule_policy()
  parameter_names <- unique(unlist(lapply(
    policies,
    `[[`,
    "primary_parameter_names"
  ), use.names = FALSE))
  series_sql <- paste(
    "SELECT t.timeseries_id, t.location_id, p.param_name,",
    "EXTRACT(EPOCH FROM t.record_rate::interval)::double precision AS rate_s",
    "FROM continuous.timeseries t",
    "JOIN public.parameters p ON p.parameter_id = t.parameter_id",
    "WHERE t.active AND p.param_name = ANY($1::text[])"
  )
  series_params <- list(qc_pg_text_array(parameter_names))
  if (!is.null(location_ids)) {
    series_sql <- paste(
      series_sql,
      "AND t.location_id = ANY($2::integer[])"
    )
    series_params[[2L]] <- qc_pg_int_array(location_ids)
  }
  series <- DBI::dbGetQuery(con, series_sql, params = series_params)
  if (!nrow(series)) return(qc_auto_empty_result())

  capabilities <- getQCRuleCapabilities(con, enabled_only = TRUE)
  capability_key <- qc_rule_capability_key(
    capabilities$rule_fx,
    capabilities$rule_version
  )
  companion_parameters <- unique(unlist(lapply(
    capabilities$argument_schema,
    function(schema) vapply(
      schema$companions,
      function(companion) companion$parameter_name,
      character(1)
    )
  ), use.names = FALSE))
  companions <- if (length(companion_parameters)) {
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT t.location_id, t.timeseries_id, p.param_name,",
        "t.sensor_priority",
        "FROM continuous.timeseries t",
        "JOIN public.parameters p ON p.parameter_id = t.parameter_id",
        "WHERE t.active AND p.param_name = ANY($1::text[])",
        "AND t.location_id = ANY($2::integer[])",
        "ORDER BY t.sensor_priority NULLS LAST, t.timeseries_id"
      ),
      params = list(
        qc_pg_text_array(companion_parameters),
        qc_pg_int_array(unique(series$location_id))
      )
    )
  } else {
    data.frame(
      location_id = integer(),
      timeseries_id = integer(),
      param_name = character(),
      sensor_priority = integer()
    )
  }

  proposals <- list()
  for (policy in policies) {
    key <- qc_rule_capability_key(policy$rule_fx, policy$rule_version)
    capability_index <- match(key, capability_key)
    if (is.na(capability_index)) next
    schema <- capabilities$argument_schema[[capability_index]]
    target_rows <- which(series$param_name %in% policy$primary_parameter_names)
    for (i in target_rows) {
      target <- series[i, , drop = FALSE]
      arguments <- policy$arguments(target)
      if (is.null(arguments)) {
        proposals[[length(proposals) + 1L]] <- list(
          result = qc_auto_assignment_result(
            target,
            policy,
            "not_eligible",
            "The recording rate cannot define a safe automatic window."
          ),
          arguments = NULL
        )
        next
      }
      effective <- qc_rule_effective_args(schema, arguments)
      required_companions <- qc_rule_active_companions(schema, effective)
      missing <- vapply(required_companions, function(descriptor) {
        !any(
          companions$location_id == target$location_id[[1L]] &
            companions$param_name == descriptor$parameter_name
        )
      }, logical(1))
      if (any(missing)) {
        labels <- vapply(
          required_companions[missing],
          `[[`,
          character(1),
          "label"
        )
        proposals[[length(proposals) + 1L]] <- list(
          result = qc_auto_assignment_result(
            target,
            policy,
            "missing_companion",
            paste0("No active same-location companion: ",
                   paste(labels, collapse = ", "), ".")
          ),
          arguments = NULL
        )
        next
      }
      proposals[[length(proposals) + 1L]] <- list(
        result = qc_auto_assignment_result(
          target,
          policy,
          "would_create",
          "Eligible primary and companion series are active at this location."
        ),
        arguments = arguments
      )
    }
  }
  if (!length(proposals)) return(qc_auto_empty_result())

  results <- do.call(rbind, lapply(proposals, `[[`, "result"))
  proposal_args <- lapply(proposals, `[[`, "arguments")
  candidate_rows <- which(results$action == "would_create")
  if (!length(candidate_rows)) return(results)

  existing_rule_pairs <- function(rows) {
    if (!length(rows)) {
      return(data.frame(
        timeseries_id = integer(),
        rule_fx = character(),
        stringsAsFactors = FALSE
      ))
    }
    DBI::dbGetQuery(
      con,
      paste(
        "SELECT DISTINCT timeseries_id, rule_fx",
        "FROM continuous.timeseries_qc_rules",
        "WHERE timeseries_id = ANY($1::integer[])",
        "AND rule_fx = ANY($2::text[])"
      ),
      params = list(
        qc_pg_int_array(unique(results$timeseries_id[rows])),
        qc_pg_text_array(unique(results$rule_fx[rows]))
      )
    )
  }

  existing <- existing_rule_pairs(candidate_rows)
  already <- candidate_rows[vapply(candidate_rows, function(i) {
    any(
      existing$timeseries_id == results$timeseries_id[[i]] &
        existing$rule_fx == results$rule_fx[[i]]
    )
  }, logical(1))]
  results$action[already] <- "already_configured"
  results$reason[already] <-
    "An assignment already exists; automatic setup leaves it unchanged."
  create_rows <- setdiff(candidate_rows, already)
  if (dry_run || !length(create_rows)) return(results)

  owns_transaction <- dbTransBegin(con)
  savepoint <- "populate_timeseries_qc_rules"
  if (!owns_transaction) DBI::dbExecute(con, paste("SAVEPOINT", savepoint))
  tryCatch({
    target_ids <- unique(results$timeseries_id[create_rows])
    locked <- DBI::dbGetQuery(
      con,
      "SELECT timeseries_id FROM continuous.timeseries
       WHERE timeseries_id = ANY($1::integer[]) AND active FOR UPDATE",
      params = list(qc_pg_int_array(target_ids))
    )$timeseries_id
    inactive <- create_rows[!results$timeseries_id[create_rows] %in% locked]
    if (length(inactive)) {
      results$action[inactive] <- "not_eligible"
      results$reason[inactive] <-
        "The primary series became inactive before the assignment was saved."
      create_rows <- setdiff(create_rows, inactive)
    }
    if (!length(create_rows)) {
      if (owns_transaction) {
        DBI::dbExecute(con, "COMMIT")
      } else {
        DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint))
      }
      return(results)
    }
    target_ids <- unique(results$timeseries_id[create_rows])

    # Recheck after locking the target series so concurrent import hooks do
    # not create duplicate assignments for the same rule.
    existing <- existing_rule_pairs(create_rows)
    for (id in target_ids) {
      rows <- create_rows[results$timeseries_id[create_rows] == id]
      already_locked <- rows[vapply(rows, function(i) {
        any(
          existing$timeseries_id == results$timeseries_id[[i]] &
            existing$rule_fx == results$rule_fx[[i]]
        )
      }, logical(1))]
      if (length(already_locked)) {
        results$action[already_locked] <- "already_configured"
        results$reason[already_locked] <-
          "An assignment already exists; automatic setup leaves it unchanged."
      }
      rows <- setdiff(rows, already_locked)
      if (!length(rows)) next

      stored <- getTimeseriesQCRules(con, id)
      assignments <- if (nrow(stored)) {
        stored[, c(
          "timeseries_qc_rule_id", "rule_fx", "rule_version", "run_order",
          "active", "note", "rule_args", "companion_timeseries"
        )]
      } else {
        data.frame(
          timeseries_qc_rule_id = integer(),
          rule_fx = character(),
          rule_version = integer(),
          run_order = integer(),
          active = logical(),
          note = character(),
          stringsAsFactors = FALSE
        ) |>
          transform(rule_args = I(list()), companion_timeseries = I(list()))
      }
      next_order <- if (nrow(assignments) && any(assignments$active)) {
        max(assignments$run_order[assignments$active]) + 1L
      } else {
        1L
      }
      if (next_order + length(rows) - 1L > 32767L) {
        results$action[rows] <- "order_limit"
        results$reason[rows] <-
          "No active run_order is available for another QC assignment."
        next
      }
      for (i in rows) {
        new_assignment <- data.frame(
          timeseries_qc_rule_id = NA_integer_,
          rule_fx = results$rule_fx[[i]],
          rule_version = results$rule_version[[i]],
          run_order = next_order,
          active = TRUE,
          note = paste(
            "Automatically configured by qc_auto_policy_v1.",
            "Edit or deactivate in AquaLink to tune this rule."
          ),
          stringsAsFactors = FALSE
        )
        new_assignment$rule_args <- I(list(proposal_args[[i]]))
        new_assignment$companion_timeseries <- I(list(list()))
        assignments <- rbind(assignments, new_assignment)
        next_order <- next_order + 1L
      }
      setTimeseriesQCRules(con, id, assignments)
      results$action[rows] <- "created"
      results$reason[rows] <-
        "Added by qc_auto_policy_v1; existing assignments were preserved."
    }

    if (owns_transaction) {
      DBI::dbExecute(con, "COMMIT")
    } else {
      DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint))
    }
  }, error = function(e) {
    if (owns_transaction) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    } else {
      try(DBI::dbExecute(con, paste("ROLLBACK TO SAVEPOINT", savepoint)), silent = TRUE)
      try(DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint)), silent = TRUE)
    }
    stop(e)
  })
  results
}
