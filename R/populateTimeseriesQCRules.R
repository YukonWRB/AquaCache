# Automatic, conservative population of common cross-timeseries QC rules.
#
# The stuck-sensor rule is available for explicit selection, but is excluded
# from the default policy because its threshold depends on parameter units,
# sensor resolution, and sampling interval, and a flatline can be a real
# environmental condition. The other rules have direct same-location checks
# with conservative defaults.

qc_auto_rule_policy <- function(include_manual = FALSE) {
  policies <- list(
    list(
      rule_fx = "qc_rain_cold",
      rule_version = 1L,
      primary_parameter_names = "precipitation, rain",
      arguments = function(series) list(max_air_temp = -0.1)
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
        if (!is.finite(rate_s) || rate_s <= 0) {
          return(NULL)
        }
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
      primary_parameter_names = "wind direction (direction from, expressed 0-360 deg)",
      arguments = function(series) list(max_wind_speed = 0)
    )
  )
  if (isTRUE(include_manual)) {
    policies <- c(policies, list(list(
      rule_fx = "qc_stuck_sensor",
      rule_version = 1L,
      all_active_parameters = TRUE,
      arguments = function(series) list()
    )))
  }
  policies
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
#' When explicitly called, idempotently adds configured rules when an eligible
#' primary series and its required active companion series exist at the same
#' location. Imports do not call this function or create assignments. By
#' default, this returns a dry-run plan. Automatic assignments are added after the
#' catalogue defaults are materialized and are pinned to their rule version;
#' existing assignments, including inactive ones, are never changed. Select
#' one or more `rule_fx` values to populate only those policies. The default
#' policy omits the broadly applicable stuck-sensor rule; select
#' `qc_stuck_sensor` explicitly to assign it to all active continuous series
#' using its registered argument defaults. When committing a filtered batch,
#' the function backfills grading by default; this can be disabled explicitly.
#'
#' @details The current policy assigns:
#' \describe{
#'   \item{Cold rain}{`qc_rain_cold@1` to rain precipitation with same-location
#'     air temperature, using a -0.1 degrees C threshold.}
#'   \item{Possible snowmelt tips}{`qc_snowmelt_tips@1` to rain and total
#'     precipitation with same-location air temperature, using a trailing
#'     30-day cold window derived from the recording rate.}
#'   \item{Wind direction during calm conditions}{`qc_dir_calm@1` to
#'     wind-direction series with same-location wind speed at or below 0 m/s.}
#' }
#' `qc_stuck_sensor` is available by explicit selection and applies to all
#' active continuous parameters. Its defaults may be unsuitable for some
#' units, sensors, recording rates, or site conditions, so it is omitted when
#' `rule_fx` is NULL.
#'
#' When backfill is enabled, all active QC rules on each affected timeseries
#' are reevaluated so the final qualifier reflects their combined results.
#' Protected qualifier ranges remain unchanged. If `backfill_from` is NULL,
#' the entire record is regraded; see [gradeContinuous()] for how unprotected
#' managed qualifiers that no rule reproduces are handled.
#'
#' @param con An open AquaCache connection.
#' @param location_ids Optional location ids to scan. NULL scans all locations
#'   with an eligible active primary series. Combine with `timeseries_ids`
#'   and/or `parameter_ids` to narrow the target series; supplied filters are
#'   combined with AND.
#' @param dry_run If TRUE, return proposed actions without writing. Set FALSE
#'   to create assignments.
#' @param rule_fx Optional character vector of automatic-policy rule names.
#'   NULL applies default policies (excluding `qc_stuck_sensor`); a supplied
#'   vector applies only those policies and may explicitly include
#'   `qc_stuck_sensor`. A rule must be defined by `qc_auto_rule_policy()`.
#' @param backfill Whether to grade existing data after the assignment step.
#'   By default this is TRUE when committing a `rule_fx`-filtered batch and
#'   FALSE for an unfiltered call. Imports do not call this function.
#'   Backfill is only available with `dry_run = FALSE`.
#' @param backfill_from,backfill_to Optional UTC datetime bounds for backfill.
#'   NULL `backfill_from` means the start of each timeseries record; NULL
#'   `backfill_to` means its latest reading. Bounds are inclusive.
#' @param timeseries_ids Optional timeseries ids to scan. NULL considers all
#'   otherwise eligible series. Combine with `location_ids` and/or
#'   `parameter_ids` to narrow the targets; supplied filters are combined with
#'   AND.
#' @param parameter_ids Optional parameter ids to scan. NULL considers all
#'   otherwise eligible parameters. Combine with `location_ids` and/or
#'   `timeseries_ids` to narrow the targets; supplied filters are combined
#'   with AND.
#' @return A data frame with one row per matching primary-series/rule policy
#'   considered. It has `timeseries_id`, `location_id`, `parameter_name`,
#'   `rule_fx`, `rule_version`, `action`, and `reason` columns. `action` is one of
#'   `would_create`, `created`, `already_configured`, `missing_companion`,
#'   `not_eligible`, or `order_limit`. No eligible candidates produces a
#'   zero-row data frame with the same columns. When backfill is enabled,
#'   `created` and active `already_configured` rows are graded through the
#'   requested window before the function returns. A grading error rolls back
#'   both the batch's new assignments and its qualifier changes.
#' @seealso [getTimeseriesQCRules()], [setTimeseriesQCRules()],
#'   [gradeContinuous()]
#' @export
populateTimeseriesQCRules <- function(
  con,
  location_ids = NULL,
  dry_run = TRUE,
  rule_fx = NULL,
  backfill = !dry_run && !is.null(rule_fx),
  backfill_from = NULL,
  backfill_to = NULL,
  timeseries_ids = NULL,
  parameter_ids = NULL
) {
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("populateTimeseriesQCRules: dry_run must be TRUE or FALSE.")
  }
  if (!is.logical(backfill) || length(backfill) != 1L || is.na(backfill)) {
    stop("populateTimeseriesQCRules: backfill must be TRUE or FALSE.")
  }
  if (backfill && dry_run) {
    stop("populateTimeseriesQCRules: backfill requires dry_run = FALSE.")
  }
  if (!backfill && (!is.null(backfill_from) || !is.null(backfill_to))) {
    stop(
      "populateTimeseriesQCRules: backfill_from and backfill_to require backfill = TRUE."
    )
  }
  normalize_bound <- function(value, label) {
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) != 1L || anyNA(value)) {
      stop("populateTimeseriesQCRules: ", label, " must be one datetime or NULL.")
    }
    value <- tryCatch(
      as.POSIXct(value, tz = "UTC"),
      error = function(e) {
        stop(
          "populateTimeseriesQCRules: invalid ",
          label,
          ": ",
          conditionMessage(e)
        )
      }
    )
    if (length(value) != 1L || is.na(value)) {
      stop("populateTimeseriesQCRules: ", label, " must be one valid datetime.")
    }
    value
  }
  backfill_from <- normalize_bound(backfill_from, "backfill_from")
  backfill_to <- normalize_bound(backfill_to, "backfill_to")
  if (
    !is.null(backfill_from) &&
      !is.null(backfill_to) &&
      backfill_from > backfill_to
  ) {
    stop("populateTimeseriesQCRules: backfill_from must not follow backfill_to.")
  }
  policies <- qc_auto_rule_policy(include_manual = !is.null(rule_fx))
  all_policy_fx <- vapply(policies, `[[`, character(1), "rule_fx")
  if (!is.null(rule_fx)) {
    if (
      !is.character(rule_fx) ||
        anyNA(rule_fx) ||
        any(!nzchar(trimws(rule_fx)))
    ) {
      stop("populateTimeseriesQCRules: rule_fx must contain non-blank names.")
    }
    rule_fx <- unique(trimws(rule_fx))
    unknown_policy_fx <- setdiff(rule_fx, all_policy_fx)
    if (length(unknown_policy_fx)) {
      stop(
        "No automatic QC population policy is defined for: ",
        paste(unknown_policy_fx, collapse = ", "),
        ". Add the rule to qc_auto_rule_policy() before selecting it."
      )
    }
    policies <- Filter(function(policy) policy$rule_fx %in% rule_fx, policies)
  }
  if (!length(policies)) {
    return(qc_auto_empty_result())
  }
  selected_rule_fx <- vapply(policies, `[[`, character(1), "rule_fx")
  normalize_ids <- function(ids, label) {
    if (is.null(ids) || !length(ids)) {
      return(ids)
    }
    unique(vapply(
      ids,
      qc_rule_integer_id,
      integer(1),
      label = label
    ))
  }
  location_ids <- normalize_ids(location_ids, "location_id")
  timeseries_ids <- normalize_ids(timeseries_ids, "timeseries_id")
  parameter_ids <- normalize_ids(parameter_ids, "parameter_id")
  if (any(vapply(
    list(location_ids, timeseries_ids, parameter_ids),
    function(ids) !is.null(ids) && !length(ids),
    logical(1)
  ))) {
    return(qc_auto_empty_result())
  }

  schema_exists <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('public.qc_rule_capabilities') IS NOT NULL
              AND to_regclass('continuous.timeseries_qc_rules') IS NOT NULL
       AS available"
  )$available[[1L]]
  if (!isTRUE(schema_exists)) {
    return(qc_auto_empty_result())
  }

  all_active_parameters <- any(vapply(
    policies,
    function(policy) isTRUE(policy$all_active_parameters),
    logical(1)
  ))
  parameter_names <- unique(unlist(
    lapply(
      policies,
      `[[`,
      "primary_parameter_names"
    ),
    use.names = FALSE
  ))
  series_sql <- paste(
    "SELECT t.timeseries_id, t.location_id, p.param_name,",
    "EXTRACT(EPOCH FROM t.record_rate::interval)::double precision AS rate_s",
    "FROM continuous.timeseries t",
    "JOIN public.parameters p ON p.parameter_id = t.parameter_id",
    "WHERE t.active AND ($1::boolean OR p.param_name = ANY($2::text[]))"
  )
  series_params <- list(
    all_active_parameters,
    qc_pg_text_array(parameter_names)
  )
  if (!is.null(location_ids)) {
    series_params[[length(series_params) + 1L]] <- qc_pg_int_array(location_ids)
    series_sql <- paste(
      series_sql,
      sprintf("AND t.location_id = ANY($%d::integer[])", length(series_params))
    )
  }
  if (!is.null(timeseries_ids)) {
    series_params[[length(series_params) + 1L]] <- qc_pg_int_array(timeseries_ids)
    series_sql <- paste(
      series_sql,
      sprintf("AND t.timeseries_id = ANY($%d::integer[])", length(series_params))
    )
  }
  if (!is.null(parameter_ids)) {
    series_params[[length(series_params) + 1L]] <- qc_pg_int_array(parameter_ids)
    series_sql <- paste(
      series_sql,
      sprintf("AND t.parameter_id = ANY($%d::integer[])", length(series_params))
    )
  }
  series <- DBI::dbGetQuery(con, series_sql, params = series_params)
  if (!nrow(series)) {
    return(qc_auto_empty_result())
  }

  capabilities <- getQCRuleCapabilities(con, enabled_only = TRUE)
  capability_key <- qc_rule_capability_key(
    capabilities$rule_fx,
    capabilities$rule_version
  )
  policy_key <- vapply(
    policies,
    function(policy) {
      qc_rule_capability_key(policy$rule_fx, policy$rule_version)
    },
    character(1)
  )
  policy_capability_index <- match(policy_key, capability_key)
  if (!is.null(rule_fx) && anyNA(policy_capability_index)) {
    missing_rules <- vapply(
      policies[is.na(policy_capability_index)],
      `[[`,
      character(1),
      "rule_fx"
    )
    stop(
      "No enabled QC capability is registered for selected rule(s): ",
      paste(missing_rules, collapse = ", "),
      "."
    )
  }
  capability_indices <- policy_capability_index[!is.na(policy_capability_index)]
  companion_parameters <- unique(unlist(
    lapply(
      capabilities$argument_schema[capability_indices],
      function(schema) {
        vapply(
          schema$companions,
          function(companion) companion$parameter_name,
          character(1)
        )
      }
    ),
    use.names = FALSE
  ))
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
    if (is.na(capability_index)) {
      next
    }
    schema <- capabilities$argument_schema[[capability_index]]
    target_rows <- if (isTRUE(policy$all_active_parameters)) {
      seq_len(nrow(series))
    } else {
      which(series$param_name %in% policy$primary_parameter_names)
    }
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
      missing <- vapply(
        required_companions,
        function(descriptor) {
          !any(
            companions$location_id == target$location_id[[1L]] &
              companions$param_name == descriptor$parameter_name
          )
        },
        logical(1)
      )
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
            paste0(
              "No active same-location companion: ",
              paste(labels, collapse = ", "),
              "."
            )
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
  if (!length(proposals)) {
    return(qc_auto_empty_result())
  }

  results <- do.call(rbind, lapply(proposals, `[[`, "result"))
  proposal_args <- lapply(proposals, `[[`, "arguments")
  candidate_rows <- which(results$action == "would_create")
  if (!length(candidate_rows) && !backfill) {
    return(results)
  }

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
  already <- candidate_rows[vapply(
    candidate_rows,
    function(i) {
      any(
        existing$timeseries_id == results$timeseries_id[[i]] &
          existing$rule_fx == results$rule_fx[[i]]
      )
    },
    logical(1)
  )]
  results$action[already] <- "already_configured"
  results$reason[already] <-
    "An assignment already exists; automatic setup leaves it unchanged."
  create_rows <- setdiff(candidate_rows, already)
  backfill_rows <- if (backfill) {
    which(results$action == "already_configured")
  } else {
    integer()
  }
  if (dry_run || (!length(create_rows) && !length(backfill_rows))) {
    return(results)
  }

  owns_transaction <- dbTransBegin(con)
  savepoint <- "populate_timeseries_qc_rules"
  if (!owns_transaction) {
    DBI::dbExecute(con, paste("SAVEPOINT", savepoint))
  }
  tryCatch(
    {
      lock_rows <- unique(c(create_rows, backfill_rows))
      target_ids <- sort(unique(results$timeseries_id[lock_rows]))
      locked <- if (length(target_ids)) {
        DBI::dbGetQuery(
          con,
          "SELECT timeseries_id FROM continuous.timeseries
       WHERE timeseries_id = ANY($1::integer[]) AND active FOR UPDATE",
          params = list(qc_pg_int_array(target_ids))
        )$timeseries_id
      } else {
        integer()
      }
      inactive <- create_rows[!results$timeseries_id[create_rows] %in% locked]
      if (length(inactive)) {
        results$action[inactive] <- "not_eligible"
        results$reason[inactive] <-
          "The primary series became inactive before the assignment was saved."
        create_rows <- setdiff(create_rows, inactive)
      }
      if (length(create_rows)) {
        target_ids <- unique(results$timeseries_id[create_rows])

        # Recheck after locking the target series so concurrent import hooks do
        # not create duplicate assignments for the same rule.
        existing <- existing_rule_pairs(create_rows)
        for (id in target_ids) {
          rows <- create_rows[results$timeseries_id[create_rows] == id]
          already_locked <- rows[vapply(
            rows,
            function(i) {
              any(
                existing$timeseries_id == results$timeseries_id[[i]] &
                  existing$rule_fx == results$rule_fx[[i]]
              )
            },
            logical(1)
          )]
          if (length(already_locked)) {
            results$action[already_locked] <- "already_configured"
            results$reason[already_locked] <-
              "An assignment already exists; automatic setup leaves it unchanged."
          }
          rows <- setdiff(rows, already_locked)
          if (!length(rows)) {
            next
          }

          stored <- getTimeseriesQCRules(con, id)
          assignments <- if (nrow(stored)) {
            stored[, c(
              "timeseries_qc_rule_id",
              "rule_fx",
              "rule_version",
              "run_order",
              "active",
              "note",
              "rule_args",
              "companion_timeseries"
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
      }

      if (backfill) {
        regrade_ids <- unique(results$timeseries_id[
          results$action %in% c("created", "already_configured")
        ])
        regrade_ids <- intersect(regrade_ids, locked)
        for (id in regrade_ids) {
          stored <- getTimeseriesQCRules(con, id)
          active_selected <- stored$active &
            stored$rule_fx %in% selected_rule_fx
          if (any(active_selected)) {
            gradeContinuous(
              con,
              timeseries_id = id,
              from = backfill_from,
              to = backfill_to
            )
          }
        }
      }

      if (owns_transaction) {
        DBI::dbExecute(con, "COMMIT")
      } else {
        DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint))
      }
    },
    error = function(e) {
      if (owns_transaction) {
        try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
      } else {
        try(
          DBI::dbExecute(con, paste("ROLLBACK TO SAVEPOINT", savepoint)),
          silent = TRUE
        )
        try(
          DBI::dbExecute(con, paste("RELEASE SAVEPOINT", savepoint)),
          silent = TRUE
        )
      }
      stop(e)
    }
  )
  results
}
