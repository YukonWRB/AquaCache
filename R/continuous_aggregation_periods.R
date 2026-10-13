# Fill missing per-measurement periods when the recipe records a support window.
# Evaluation intervals describe output cadence, not measurement duration, so
# they are deliberately not used as a period. Callers calculate any remaining
# periods with the existing timestamp-based fallback.
apply_timeseries_aggregation_periods <- function(data, timeseries_id, con) {
  if (!("period" %in% names(data))) {
    data$period <- NA_character_
  } else {
    data$period <- as.character(data$period)
  }

  missing_period <- is.na(data$period) | !nzchar(trimws(data$period))
  if (!any(missing_period)) {
    return(data)
  }

  methods_available <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass(
       'continuous.timeseries_aggregation_methods'
     ) IS NOT NULL AS available"
  )$available[[1]]
  if (!isTRUE(methods_available)) {
    return(data)
  }

  method_periods <- DBI::dbGetQuery(
    con,
    "WITH terminal_steps AS (
       SELECT
         step.aggregation_method_id,
         step.operation_type,
         step.window_start_offset,
         step.window_end_offset
       FROM continuous.timeseries_aggregation_method_steps step
       WHERE NOT EXISTS (
         SELECT 1
         FROM continuous.timeseries_aggregation_method_step_inputs input
         WHERE input.aggregation_method_id = step.aggregation_method_id
           AND input.input_step_id = step.step_id
       )
     ),
     leaf_periods AS (
       SELECT
         step.aggregation_method_id,
         count(*) FILTER (WHERE leaf_period.period IS NULL)
           AS unresolved_period_count,
         count(DISTINCT leaf_period.period) AS period_count,
         min(leaf_period.period) AS period
       FROM continuous.timeseries_aggregation_method_steps step
       CROSS JOIN LATERAL (
         SELECT CASE
           WHEN step.window_start_offset IS NOT NULL
            AND step.window_end_offset IS NOT NULL
           THEN step.window_end_offset - step.window_start_offset
         END AS period
       ) leaf_period
       WHERE NOT EXISTS (
         SELECT 1
         FROM continuous.timeseries_aggregation_method_step_inputs input
         WHERE input.aggregation_method_id = step.aggregation_method_id
           AND input.step_id = step.step_id
       )
       GROUP BY step.aggregation_method_id
     ),
     resolved_periods AS (
       SELECT
         method.applicable_from,
         method.applicable_until,
         CASE
           WHEN terminal.operation_type = 'window_aggregate'
            AND terminal.window_start_offset IS NOT NULL
            AND terminal.window_end_offset IS NOT NULL
           THEN terminal.window_end_offset - terminal.window_start_offset
           WHEN terminal.operation_type = 'arithmetic_mean'
            AND leaves.unresolved_period_count = 0
            AND leaves.period_count = 1
           THEN leaves.period
         END AS period
       FROM continuous.timeseries_aggregation_methods method
       LEFT JOIN terminal_steps terminal
         ON terminal.aggregation_method_id = method.aggregation_method_id
       LEFT JOIN leaf_periods leaves
         ON leaves.aggregation_method_id = method.aggregation_method_id
       WHERE method.timeseries_id = $1
     )
     SELECT
       CASE WHEN isfinite(applicable_from)
         THEN extract(epoch FROM applicable_from)::double precision
       END AS applicable_from_epoch,
       CASE WHEN isfinite(applicable_until)
         THEN extract(epoch FROM applicable_until)::double precision
       END AS applicable_until_epoch,
       period::text AS period
     FROM resolved_periods
     WHERE period > INTERVAL '0'
     ORDER BY applicable_from",
    params = list(as.integer(timeseries_id))
  )

  if (nrow(method_periods) == 0L) {
    return(data)
  }

  from_epoch <- method_periods$applicable_from_epoch
  from_epoch[is.na(from_epoch)] <- -Inf
  until_epoch <- method_periods$applicable_until_epoch
  until_epoch[is.na(until_epoch)] <- Inf
  datetime_epoch <- as.numeric(data$datetime)

  for (method_index in seq_len(nrow(method_periods))) {
    row_indices <- which(
      missing_period &
        datetime_epoch >= from_epoch[[method_index]] &
        datetime_epoch < until_epoch[[method_index]]
    )
    if (length(row_indices) > 0L) {
      data$period[row_indices] <- method_periods$period[[method_index]]
      missing_period[row_indices] <- FALSE
    }
  }

  data
}
