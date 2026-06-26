# Patch 50: borehole bedrock flag and calculated-daily audit volume controls

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 50: adding a borehole bedrock flag and reducing calculated-daily audit volume."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

active <- FALSE
active <- dbTransBegin(con)

tryCatch(
  {
    check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('boreholes.boreholes') IS NOT NULL AS has_boreholes,
         to_regclass('continuous.measurements_calculated_daily') IS NOT NULL AS has_daily,
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.aggregation_types') IS NOT NULL AS has_aggregation_types,
         to_regprocedure('continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, text, integer)') IS NOT NULL AS has_continuous_at,
         to_regprocedure('continuous.normalized_day_of_year(date)') IS NOT NULL AS has_normalized_doy,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )

    if (
      !isTRUE(check$has_boreholes[[1]]) ||
        !isTRUE(check$has_daily[[1]]) ||
        !isTRUE(check$has_timeseries[[1]]) ||
        !isTRUE(check$has_aggregation_types[[1]]) ||
        !isTRUE(check$has_continuous_at[[1]]) ||
        !isTRUE(check$has_normalized_doy[[1]]) ||
        !isTRUE(check$has_version_info[[1]])
    ) {
      stop(
        "Patch 50 requires boreholes.boreholes, continuous.measurements_calculated_daily, continuous.timeseries, continuous.aggregation_types, continuous.measurements_continuous_corrected_at(), continuous.normalized_day_of_year(), and information.version_info to already exist."
      )
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes
       ADD COLUMN IF NOT EXISTS bedrock_reached BOOLEAN DEFAULT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.boreholes.bedrock_reached IS
       'TRUE if bedrock was reached, FALSE if not, NULL if unknown.';"
    )

    # update so that bedrock_reached is NULL where depth_to_bedrock_m is NULL
    DBI::dbExecute(
      con,
      "UPDATE boreholes.boreholes
       SET bedrock_reached = NULL
       WHERE depth_to_bedrock_m IS NULL;"
    )

    bad <- DBI::dbGetQuery(
      con,
      "SELECT borehole_id, bedrock_reached, depth_to_bedrock_m
       FROM boreholes.boreholes
       WHERE (bedrock_reached IS TRUE AND depth_to_bedrock_m IS NULL)
          OR (bedrock_reached IS DISTINCT FROM TRUE AND depth_to_bedrock_m IS NOT NULL)
       LIMIT 20;"
    )
    if (nrow(bad) > 0) {
      stop(
        "Patch 50 cannot add the constraint bedrock_depth_check because at least one borehole has inconsistent bedrock_reached and depth_to_bedrock_m values. Ensure that no boreholes have bedrock_reached = TRUE with depth_to_bedrock_m = NULL, and no boreholes have bedrock_reached != TRUE with depth_to_bedrock_m != NULL. The first 20 inconsistent boreholes are:\n",
        paste(capture.output(print(bad)), collapse = "\n")
      )
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes
       DROP CONSTRAINT IF EXISTS bedrock_depth_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes
       ADD CONSTRAINT bedrock_depth_check
       CHECK (
         (bedrock_reached IS TRUE AND depth_to_bedrock_m IS NOT NULL) OR
         (bedrock_reached IS DISTINCT FROM TRUE AND depth_to_bedrock_m IS NULL)
       );"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS audit_measurements_calculated_daily_trigger
       ON continuous.measurements_calculated_daily;"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS audit.measurements_calculated_daily_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       );"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS audit.log_measurements_calculated_daily_change();"
    )
    DBI::dbExecute(
      con,
      "DROP TABLE IF EXISTS audit.measurements_calculated_daily_log;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_calculated_daily_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL,
         p_start_date DATE DEFAULT NULL,
         p_end_date DATE DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         date DATE,
         value NUMERIC,
         imputed BOOLEAN,
         percent_historic_range NUMERIC,
         max NUMERIC,
         min NUMERIC,
         q90 NUMERIC,
         q75 NUMERIC,
         q50 NUMERIC,
         q25 NUMERIC,
         q10 NUMERIC,
         mean NUMERIC,
         doy_count INTEGER,
         percent_historic_range_30yr NUMERIC,
         max_30yr NUMERIC,
         min_30yr NUMERIC,
         q90_30yr NUMERIC,
         q75_30yr NUMERIC,
         q50_30yr NUMERIC,
         q25_30yr NUMERIC,
         q10_30yr NUMERIC,
         mean_30yr NUMERIC,
         doy_count_30yr INTEGER,
         created TIMESTAMPTZ,
         modified TIMESTAMPTZ
       )
       LANGUAGE sql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
         WITH selected_timeseries AS MATERIALIZED (
           SELECT
             t.timeseries_id,
             COALESCE(t.timezone_daily_calc, 0) AS timezone_daily_calc,
             at.aggregation_type,
             t.record_rate,
             t.timeseries_type
           FROM continuous.timeseries t
           JOIN continuous.aggregation_types at
             ON at.aggregation_type_id = t.aggregation_type_id
           WHERE (p_timeseries_ids IS NULL OR t.timeseries_id = ANY(p_timeseries_ids))
             AND (t.record_rate IS NULL OR t.record_rate <= interval '1 day')
             AND (
               t.timeseries_type <> 'compound'
               OR EXISTS (
                 SELECT 1
                 FROM continuous.timeseries_compound_members m
                 WHERE m.timeseries_id = t.timeseries_id
               )
             )
         ),
         raw_measurements AS MATERIALIZED (
           SELECT
             st.timeseries_id,
             st.aggregation_type,
             (m.datetime + make_interval(hours => st.timezone_daily_calc))::date AS date,
             m.value_corrected AS value,
             m.imputed
           FROM selected_timeseries st
           CROSS JOIN LATERAL continuous.measurements_continuous_corrected_at(
             COALESCE(p_as_of, now()),
             st.timeseries_id,
             NULL::timestamptz,
             CASE
               WHEN p_end_date IS NULL THEN NULL::timestamptz
               ELSE (
                 (p_end_date + 1)::timestamp -
                   make_interval(hours => st.timezone_daily_calc)
               ) AT TIME ZONE 'UTC' - interval '1 microsecond'
             END,
             'actual',
             NULL::integer
           ) m
           WHERE m.period <= interval '1 day'
             AND (
               p_end_date IS NULL OR
                 (m.datetime + make_interval(hours => st.timezone_daily_calc))::date <= p_end_date
             )
             AND NOT EXISTS (
               SELECT 1
               FROM continuous.grades g
               JOIN public.grade_types gt
                 ON gt.grade_type_id = g.grade_type_id
               WHERE g.timeseries_id = st.timeseries_id
                 AND gt.grade_type_code = 'N'
                 AND g.start_dt <> g.end_dt
                 AND m.datetime BETWEEN g.start_dt AND g.end_dt
             )
         ),
         daily_values AS MATERIALIZED (
           SELECT
             raw_measurements.timeseries_id,
             raw_measurements.date,
             CASE
               WHEN raw_measurements.aggregation_type = 'sum' THEN
                 SUM(raw_measurements.value)
               WHEN raw_measurements.aggregation_type = 'median' THEN
                 (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY raw_measurements.value
                   ) FILTER (WHERE raw_measurements.value IS NOT NULL)
                 )::numeric
               WHEN raw_measurements.aggregation_type IN ('min', 'minimum') THEN
                 MIN(raw_measurements.value)
               WHEN raw_measurements.aggregation_type IN ('max', 'maximum') THEN
                 MAX(raw_measurements.value)
               WHEN raw_measurements.aggregation_type = '(min+max)/2' THEN
                 (MIN(raw_measurements.value) + MAX(raw_measurements.value)) / 2
               ELSE AVG(raw_measurements.value)
             END AS value,
             COALESCE(BOOL_OR(raw_measurements.imputed), FALSE) AS imputed
           FROM raw_measurements
           GROUP BY
             raw_measurements.timeseries_id,
             raw_measurements.date,
             raw_measurements.aggregation_type
         ),
         base AS MATERIALIZED (
           SELECT
             daily_values.timeseries_id,
             daily_values.date,
             daily_values.value,
             daily_values.imputed,
             continuous.normalized_day_of_year(daily_values.date) AS doy
           FROM daily_values
           WHERE continuous.normalized_day_of_year(daily_values.date) IS NOT NULL
         ),
         target AS MATERIALIZED (
           SELECT *
           FROM base
           WHERE (p_start_date IS NULL OR base.date >= p_start_date)
             AND (p_end_date IS NULL OR base.date <= p_end_date)
         ),
         hist AS (
           SELECT
             target.timeseries_id,
             target.date,
             target.value,
             target.imputed,
             COUNT(past.value) AS hist_count,
             MIN(past.value) AS hist_min,
             MAX(past.value) AS hist_max,
             AVG(past.value) AS hist_mean,
             (
               percentile_cont(0.90) WITHIN GROUP (ORDER BY past.value)
                 FILTER (WHERE past.value IS NOT NULL)
             )::numeric AS hist_q90,
             (
               percentile_cont(0.75) WITHIN GROUP (ORDER BY past.value)
                 FILTER (WHERE past.value IS NOT NULL)
             )::numeric AS hist_q75,
             (
               percentile_cont(0.50) WITHIN GROUP (ORDER BY past.value)
                 FILTER (WHERE past.value IS NOT NULL)
             )::numeric AS hist_q50,
             (
               percentile_cont(0.25) WITHIN GROUP (ORDER BY past.value)
                 FILTER (WHERE past.value IS NOT NULL)
             )::numeric AS hist_q25,
             (
               percentile_cont(0.10) WITHIN GROUP (ORDER BY past.value)
                 FILTER (WHERE past.value IS NOT NULL)
             )::numeric AS hist_q10,
             COUNT(past.value) FILTER (
               WHERE past.date >= target.date - interval '30 years'
             ) AS hist_count_30yr,
             MIN(past.value) FILTER (
               WHERE past.date >= target.date - interval '30 years'
             ) AS hist_min_30yr,
             MAX(past.value) FILTER (
               WHERE past.date >= target.date - interval '30 years'
             ) AS hist_max_30yr,
             AVG(past.value) FILTER (
               WHERE past.date >= target.date - interval '30 years'
             ) AS hist_mean_30yr,
             (
               percentile_cont(0.90) WITHIN GROUP (ORDER BY past.value)
                 FILTER (
                   WHERE past.value IS NOT NULL
                     AND past.date >= target.date - interval '30 years'
                 )
             )::numeric AS hist_q90_30yr,
             (
               percentile_cont(0.75) WITHIN GROUP (ORDER BY past.value)
                 FILTER (
                   WHERE past.value IS NOT NULL
                     AND past.date >= target.date - interval '30 years'
                 )
             )::numeric AS hist_q75_30yr,
             (
               percentile_cont(0.50) WITHIN GROUP (ORDER BY past.value)
                 FILTER (
                   WHERE past.value IS NOT NULL
                     AND past.date >= target.date - interval '30 years'
                 )
             )::numeric AS hist_q50_30yr,
             (
               percentile_cont(0.25) WITHIN GROUP (ORDER BY past.value)
                 FILTER (
                   WHERE past.value IS NOT NULL
                     AND past.date >= target.date - interval '30 years'
                 )
             )::numeric AS hist_q25_30yr,
             (
               percentile_cont(0.10) WITHIN GROUP (ORDER BY past.value)
                 FILTER (
                   WHERE past.value IS NOT NULL
                     AND past.date >= target.date - interval '30 years'
                 )
             )::numeric AS hist_q10_30yr
           FROM target
           LEFT JOIN base past
             ON past.timeseries_id = target.timeseries_id
            AND past.doy = target.doy
            AND past.date < target.date
            AND past.value IS NOT NULL
           GROUP BY target.timeseries_id, target.date, target.value, target.imputed
         )
         SELECT
           hist.timeseries_id,
           hist.date,
           hist.value,
           hist.imputed,
           CASE
             WHEN hist.hist_count > 1 AND hist.value IS NOT NULL THEN
               ((hist.value - hist.hist_min) / NULLIF(hist.hist_max - hist.hist_min, 0)) * 100
             ELSE NULL::numeric
           END AS percent_historic_range,
           CASE
             WHEN hist.hist_count = 0 AND hist.value IS NOT NULL THEN hist.value
             WHEN hist.hist_count > 0 THEN hist.hist_max
             ELSE NULL::numeric
           END AS max,
           CASE
             WHEN hist.hist_count = 0 AND hist.value IS NOT NULL THEN hist.value
             WHEN hist.hist_count > 0 THEN hist.hist_min
             ELSE NULL::numeric
           END AS min,
           CASE WHEN hist.hist_count > 0 THEN hist.hist_q90 ELSE NULL::numeric END AS q90,
           CASE WHEN hist.hist_count > 0 THEN hist.hist_q75 ELSE NULL::numeric END AS q75,
           CASE WHEN hist.hist_count > 0 THEN hist.hist_q50 ELSE NULL::numeric END AS q50,
           CASE WHEN hist.hist_count > 0 THEN hist.hist_q25 ELSE NULL::numeric END AS q25,
           CASE WHEN hist.hist_count > 0 THEN hist.hist_q10 ELSE NULL::numeric END AS q10,
           CASE WHEN hist.hist_count > 0 THEN hist.hist_mean ELSE NULL::numeric END AS mean,
           CASE
             WHEN hist.hist_count = 0 AND hist.value IS NOT NULL THEN 1
             WHEN hist.hist_count > 0 THEN hist.hist_count + CASE WHEN hist.value IS NULL THEN 0 ELSE 1 END
             ELSE NULL::integer
           END AS doy_count,
           CASE
             WHEN hist.hist_count_30yr > 1 AND hist.value IS NOT NULL THEN
               ((hist.value - hist.hist_min_30yr) / NULLIF(hist.hist_max_30yr - hist.hist_min_30yr, 0)) * 100
             ELSE NULL::numeric
           END AS percent_historic_range_30yr,
           CASE
             WHEN hist.hist_count_30yr = 0 AND hist.value IS NOT NULL THEN hist.value
             WHEN hist.hist_count_30yr > 0 THEN hist.hist_max_30yr
             ELSE NULL::numeric
           END AS max_30yr,
           CASE
             WHEN hist.hist_count_30yr = 0 AND hist.value IS NOT NULL THEN hist.value
             WHEN hist.hist_count_30yr > 0 THEN hist.hist_min_30yr
             ELSE NULL::numeric
           END AS min_30yr,
           CASE WHEN hist.hist_count_30yr > 0 THEN hist.hist_q90_30yr ELSE NULL::numeric END AS q90_30yr,
           CASE WHEN hist.hist_count_30yr > 0 THEN hist.hist_q75_30yr ELSE NULL::numeric END AS q75_30yr,
           CASE WHEN hist.hist_count_30yr > 0 THEN hist.hist_q50_30yr ELSE NULL::numeric END AS q50_30yr,
           CASE WHEN hist.hist_count_30yr > 0 THEN hist.hist_q25_30yr ELSE NULL::numeric END AS q25_30yr,
           CASE WHEN hist.hist_count_30yr > 0 THEN hist.hist_q10_30yr ELSE NULL::numeric END AS q10_30yr,
           CASE WHEN hist.hist_count_30yr > 0 THEN hist.hist_mean_30yr ELSE NULL::numeric END AS mean_30yr,
           CASE
             WHEN hist.hist_count_30yr = 0 AND hist.value IS NOT NULL THEN 1
             WHEN hist.hist_count_30yr > 0 THEN hist.hist_count_30yr + CASE WHEN hist.value IS NULL THEN 0 ELSE 1 END
             ELSE NULL::integer
           END AS doy_count_30yr,
           NULL::timestamptz AS created,
           NULL::timestamptz AS modified
         FROM hist
         ORDER BY hist.timeseries_id, hist.date;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) IS
       'Recomputes calculated daily rows for a requested as-of timestamp from audited continuous measurements and corrections, with current unusable-grade exclusions. The current calculated-daily table is a derived cache and is no longer row-audited; this function does not reconstruct historical stored cache rows byte-for-byte.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.measurements_calculated_daily IS
       'Database-maintained daily aggregate and historical-statistics cache for continuous timeseries. Rows are derived from continuous measurements, corrections, grades, and compound definitions and are not row-audited.';"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '50'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    DBI::dbExecute(con, "COMMIT;")
    active <- FALSE

    message(
      "Patch 50 applied successfully."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
