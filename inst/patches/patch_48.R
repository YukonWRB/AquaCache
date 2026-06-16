# Patch 48: calculated-daily tail trimming and rolling 30-year stats

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 48: fixing calculated-daily tail cleanup and adding rolling 30-year daily statistics. Schema and function changes are made in a transaction; backfill runs afterward per timeseries_id under advisory locks."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

active <- FALSE
message("Starting schema/function transaction...")
active <- dbTransBegin(con)

tryCatch(
  {
    check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('continuous.measurements_continuous') IS NOT NULL AS has_measurements_continuous,
         to_regclass('continuous.measurements_calculated_daily') IS NOT NULL AS has_measurements_calculated_daily,
         to_regclass('audit.measurements_calculated_daily_log') IS NOT NULL AS has_measurements_calculated_daily_log,
         to_regprocedure('continuous.refresh_calculated_daily(integer, date, date)') IS NOT NULL AS has_refresh_calculated_daily,
         to_regprocedure('audit.measurements_calculated_daily_as_of(timestamp with time zone, integer[], date, date)') IS NOT NULL AS has_daily_as_of,
         to_regprocedure('continuous.measurements_calculated_daily_at(timestamp with time zone, integer[], date, date)') IS NOT NULL AS has_daily_at"
    )

    if (
      !isTRUE(check$has_measurements_continuous[[1]]) ||
        !isTRUE(check$has_measurements_calculated_daily[[1]]) ||
        !isTRUE(check$has_measurements_calculated_daily_log[[1]]) ||
        !isTRUE(check$has_refresh_calculated_daily[[1]]) ||
        !isTRUE(check$has_daily_as_of[[1]]) ||
        !isTRUE(check$has_daily_at[[1]])
    ) {
      stop(
        "This patch requires continuous.measurements_continuous, continuous.measurements_calculated_daily, audit.measurements_calculated_daily_log, continuous.refresh_calculated_daily(), audit.measurements_calculated_daily_as_of(), and continuous.measurements_calculated_daily_at() to already exist."
      )
    }

    q_ident <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    function_execute_grants <- DBI::dbGetQuery(
      con,
      "WITH target_functions AS (
         SELECT
           'audit.measurements_calculated_daily_as_of(timestamp with time zone, integer[], date, date)'::regprocedure AS function_oid,
           'audit.measurements_calculated_daily_as_of(TIMESTAMPTZ, INTEGER[], DATE, DATE)'::text AS function_signature
         UNION ALL
         SELECT
           'continuous.measurements_calculated_daily_at(timestamp with time zone, integer[], date, date)'::regprocedure AS function_oid,
           'continuous.measurements_calculated_daily_at(TIMESTAMPTZ, INTEGER[], DATE, DATE)'::text AS function_signature
       ),
       function_acls AS (
         SELECT
           target_functions.function_signature,
           COALESCE(pg_proc.proacl, acldefault('f', pg_proc.proowner)) AS acl
         FROM target_functions
         JOIN pg_proc
           ON pg_proc.oid = target_functions.function_oid
       )
       SELECT DISTINCT
         function_acls.function_signature,
         CASE
           WHEN acl.grantee = 0 THEN 'PUBLIC'
           ELSE pg_roles.rolname
         END AS role_name
       FROM function_acls
       CROSS JOIN LATERAL aclexplode(function_acls.acl) acl
       LEFT JOIN pg_roles
         ON pg_roles.oid = acl.grantee
       WHERE acl.privilege_type = 'EXECUTE'
       ORDER BY function_signature, role_name"
    )

    message(
      "Adding rolling 30-year columns to continuous.measurements_calculated_daily..."
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily
       ADD COLUMN IF NOT EXISTS percent_historic_range_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS max_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS min_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS q90_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS q75_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS q50_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS q25_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS q10_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS mean_30yr NUMERIC,
       ADD COLUMN IF NOT EXISTS doy_count_30yr INTEGER;"
    )

    message(
      "Creating tail-trim helper for calculated daily rows and accessory intervals..."
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.trim_continuous_timeseries_tail(
         p_timeseries_id integer
       )
       RETURNS void
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_offset integer;
         v_timeseries_type text;
         v_last_measurement_datetime timestamp with time zone;
         v_last_measurement_date date;
         v_previous_skip text;
         v_table text;
       BEGIN
         SELECT t.timezone_daily_calc, t.timeseries_type
         INTO v_offset, v_timeseries_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND OR v_timeseries_type <> 'basic' THEN
           RETURN;
         END IF;

         SELECT
           MAX(mc.datetime),
           MAX((mc.datetime + make_interval(hours => v_offset))::date)
         INTO v_last_measurement_datetime, v_last_measurement_date
         FROM continuous.measurements_continuous mc
         WHERE mc.timeseries_id = p_timeseries_id;

         IF v_last_measurement_datetime IS NULL THEN
           DELETE FROM continuous.measurements_calculated_daily mcd
           WHERE mcd.timeseries_id = p_timeseries_id;
         ELSE
           DELETE FROM continuous.measurements_calculated_daily mcd
           WHERE mcd.timeseries_id = p_timeseries_id
             AND mcd.date > v_last_measurement_date
             AND NOT EXISTS (
               SELECT 1
               FROM continuous.measurements_continuous mc
               WHERE mc.timeseries_id = p_timeseries_id
                 AND (mc.datetime + make_interval(hours => v_offset))::date = mcd.date
             );
         END IF;

         v_previous_skip := current_setting('continuous.skip_daily_refresh', true);
         PERFORM set_config('continuous.skip_daily_refresh', 'on', true);

         FOREACH v_table IN ARRAY ARRAY[
           'approvals',
           'grades',
           'qualifiers',
           'owners',
           'contributors'
         ]
         LOOP
           IF v_last_measurement_datetime IS NULL THEN
             EXECUTE format(
               'DELETE FROM continuous.%I WHERE timeseries_id = $1',
               v_table
             )
             USING p_timeseries_id;
           ELSE
             EXECUTE format(
               'DELETE FROM continuous.%I WHERE timeseries_id = $1 AND start_dt > $2',
               v_table
             )
             USING p_timeseries_id, v_last_measurement_datetime;

             EXECUTE format(
               'UPDATE continuous.%I SET end_dt = $2 WHERE timeseries_id = $1 AND end_dt > $2 AND start_dt <= $2',
               v_table
             )
             USING p_timeseries_id, v_last_measurement_datetime;
           END IF;
         END LOOP;

         IF v_previous_skip IS NULL THEN
           PERFORM set_config('continuous.skip_daily_refresh', '', true);
         ELSE
           PERFORM set_config('continuous.skip_daily_refresh', v_previous_skip, true);
         END IF;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.trim_continuous_timeseries_tail(integer)
       OWNER TO postgres;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.trim_continuous_timeseries_tail(integer)
       TO PUBLIC;"
    )

    message(
      "Replacing continuous.refresh_calculated_daily() with rolling 30-year stats and tail cleanup..."
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_calculated_daily(
         p_timeseries_id integer,
         p_changed_start date DEFAULT NULL,
         p_changed_end date DEFAULT NULL
       )
       RETURNS void
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         v_offset integer;
         v_aggregation_type text;
         v_record_rate interval;
         v_timeseries_type text;
         v_start_date date;
         v_end_date date;
         v_start_ts timestamp with time zone;
         v_end_ts timestamp with time zone;
         v_recalc_feb29 boolean;
       BEGIN
         SELECT
           t.timezone_daily_calc,
           at.aggregation_type,
           t.record_rate,
           t.timeseries_type
         INTO
           v_offset,
           v_aggregation_type,
           v_record_rate,
           v_timeseries_type
         FROM continuous.timeseries t
         JOIN continuous.aggregation_types at
           ON at.aggregation_type_id = t.aggregation_type_id
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RETURN;
         END IF;

         IF v_record_rate IS NOT NULL AND v_record_rate > interval '1 day' THEN
           RETURN;
         END IF;

         IF v_timeseries_type = 'compound'
           AND NOT EXISTS (
             SELECT 1
             FROM continuous.timeseries_compound_members m
             WHERE m.timeseries_id = p_timeseries_id
           ) THEN
           RETURN;
         END IF;

         v_start_date := p_changed_start;
         v_end_date := p_changed_end;

         IF v_start_date IS NULL THEN
           WITH measurement_bounds AS (
             SELECT
               MIN((m.datetime + make_interval(hours => v_offset))::date) AS min_date,
               MAX((m.datetime + make_interval(hours => v_offset))::date) AS max_date
             FROM continuous.measurements_continuous_corrected(
               p_timeseries_id,
               NULL::timestamptz,
               NULL::timestamptz
             ) m
             WHERE m.period <= interval '1 day'
               AND NOT EXISTS (
                 SELECT 1
                 FROM continuous.grades g
                 JOIN public.grade_types gt
                   ON gt.grade_type_id = g.grade_type_id
                 WHERE g.timeseries_id = p_timeseries_id
                   AND gt.grade_type_code = 'N'
                   AND g.start_dt <> g.end_dt
                   AND m.datetime BETWEEN g.start_dt AND g.end_dt
               )
           ),
           daily_bounds AS (
             SELECT
               MIN(mcd.date) AS min_date,
               MAX(mcd.date) AS max_date
             FROM continuous.measurements_calculated_daily mcd
             WHERE mcd.timeseries_id = p_timeseries_id
           )
           SELECT
             LEAST(measurement_bounds.min_date, daily_bounds.min_date),
             GREATEST(measurement_bounds.max_date, daily_bounds.max_date)
           INTO v_start_date, v_end_date
           FROM measurement_bounds, daily_bounds;
         END IF;

         IF v_start_date IS NULL THEN
           PERFORM continuous.trim_continuous_timeseries_tail(p_timeseries_id);
           RETURN;
         END IF;

         IF v_end_date IS NULL THEN
           v_end_date := v_start_date;
         END IF;

         IF v_end_date < v_start_date THEN
           v_start_date := p_changed_end;
           v_end_date := p_changed_start;
         END IF;

         v_start_ts := (
           v_start_date::timestamp - make_interval(hours => v_offset)
         ) AT TIME ZONE 'UTC';
         v_end_ts := (
           (v_end_date + 1)::timestamp - make_interval(hours => v_offset)
         ) AT TIME ZONE 'UTC' - interval '1 microsecond';

         WITH days AS (
           SELECT d::date AS date
           FROM generate_series(v_start_date, v_end_date, interval '1 day') AS d
         ),
         raw_measurements AS MATERIALIZED (
           SELECT
             (m.datetime + make_interval(hours => v_offset))::date AS date,
             m.value_corrected AS value,
             m.imputed
           FROM continuous.measurements_continuous_corrected(
             p_timeseries_id,
             v_start_ts,
             v_end_ts
           ) m
           WHERE m.period <= interval '1 day'
             AND NOT EXISTS (
               SELECT 1
               FROM continuous.grades g
               JOIN public.grade_types gt
                 ON gt.grade_type_id = g.grade_type_id
               WHERE g.timeseries_id = p_timeseries_id
                 AND gt.grade_type_code = 'N'
                 AND g.start_dt <> g.end_dt
                 AND m.datetime BETWEEN g.start_dt AND g.end_dt
             )
         ),
         existing_daily AS MATERIALIZED (
           SELECT mcd.date
           FROM continuous.measurements_calculated_daily mcd
           WHERE mcd.timeseries_id = p_timeseries_id
             AND mcd.date BETWEEN v_start_date AND v_end_date
         ),
         daily_values AS (
           SELECT
             days.date,
             CASE
               WHEN v_aggregation_type = 'sum' THEN SUM(raw_measurements.value)
               WHEN v_aggregation_type = 'median' THEN percentile_cont(0.5)
                 WITHIN GROUP (ORDER BY raw_measurements.value)
                 FILTER (WHERE raw_measurements.value IS NOT NULL)::numeric
               WHEN v_aggregation_type IN ('min', 'minimum') THEN
                 MIN(raw_measurements.value)
               WHEN v_aggregation_type IN ('max', 'maximum') THEN
                 MAX(raw_measurements.value)
               WHEN v_aggregation_type = '(min+max)/2' THEN
                 (MIN(raw_measurements.value) + MAX(raw_measurements.value)) / 2
               ELSE AVG(raw_measurements.value)
             END AS value,
             COALESCE(BOOL_OR(raw_measurements.imputed), FALSE) AS imputed,
             COUNT(raw_measurements.date) AS measurement_count,
             existing_daily.date IS NOT NULL AS has_existing_daily
           FROM days
           LEFT JOIN raw_measurements
             ON raw_measurements.date = days.date
           LEFT JOIN existing_daily
             ON existing_daily.date = days.date
           GROUP BY days.date, existing_daily.date
         )
         INSERT INTO continuous.measurements_calculated_daily (
           timeseries_id,
           date,
           value,
           imputed
         )
         SELECT
           p_timeseries_id,
           daily_values.date,
           daily_values.value,
           daily_values.imputed
         FROM daily_values
         WHERE daily_values.measurement_count > 0
            OR daily_values.has_existing_daily
         ON CONFLICT (timeseries_id, date) DO UPDATE
         SET
           value = EXCLUDED.value,
           imputed = EXCLUDED.imputed,
           modified = CURRENT_TIMESTAMP,
           modified_by = CURRENT_USER
         WHERE measurements_calculated_daily.value IS DISTINCT FROM EXCLUDED.value
            OR measurements_calculated_daily.imputed IS DISTINCT FROM EXCLUDED.imputed;

         WITH affected_doys AS (
           SELECT DISTINCT continuous.normalized_day_of_year(d::date) AS doy
           FROM generate_series(v_start_date, v_end_date, interval '1 day') AS d
           WHERE continuous.normalized_day_of_year(d::date) IS NOT NULL
         ),
         base AS MATERIALIZED (
           SELECT
             mcd.timeseries_id,
             mcd.date,
             mcd.value,
             mcd.imputed,
             continuous.normalized_day_of_year(mcd.date) AS doy
           FROM continuous.measurements_calculated_daily mcd
           JOIN affected_doys ad
             ON ad.doy = continuous.normalized_day_of_year(mcd.date)
           WHERE mcd.timeseries_id = p_timeseries_id
             AND continuous.normalized_day_of_year(mcd.date) IS NOT NULL
         ),
         target AS MATERIALIZED (
           SELECT *
           FROM base
           WHERE date >= v_start_date
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
             percentile_cont(0.90) WITHIN GROUP (ORDER BY past.value)
               FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q90,
             percentile_cont(0.75) WITHIN GROUP (ORDER BY past.value)
               FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q75,
             percentile_cont(0.50) WITHIN GROUP (ORDER BY past.value)
               FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q50,
             percentile_cont(0.25) WITHIN GROUP (ORDER BY past.value)
               FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q25,
             percentile_cont(0.10) WITHIN GROUP (ORDER BY past.value)
               FILTER (WHERE past.value IS NOT NULL)::numeric AS hist_q10,
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
             percentile_cont(0.90) WITHIN GROUP (ORDER BY past.value)
               FILTER (
                 WHERE past.value IS NOT NULL
                   AND past.date >= target.date - interval '30 years'
               )::numeric AS hist_q90_30yr,
             percentile_cont(0.75) WITHIN GROUP (ORDER BY past.value)
               FILTER (
                 WHERE past.value IS NOT NULL
                   AND past.date >= target.date - interval '30 years'
               )::numeric AS hist_q75_30yr,
             percentile_cont(0.50) WITHIN GROUP (ORDER BY past.value)
               FILTER (
                 WHERE past.value IS NOT NULL
                   AND past.date >= target.date - interval '30 years'
               )::numeric AS hist_q50_30yr,
             percentile_cont(0.25) WITHIN GROUP (ORDER BY past.value)
               FILTER (
                 WHERE past.value IS NOT NULL
                   AND past.date >= target.date - interval '30 years'
               )::numeric AS hist_q25_30yr,
             percentile_cont(0.10) WITHIN GROUP (ORDER BY past.value)
               FILTER (
                 WHERE past.value IS NOT NULL
                   AND past.date >= target.date - interval '30 years'
               )::numeric AS hist_q10_30yr
           FROM target
           LEFT JOIN base past
             ON past.timeseries_id = target.timeseries_id
            AND past.doy = target.doy
            AND past.date < target.date
            AND past.value IS NOT NULL
           GROUP BY target.timeseries_id, target.date, target.value, target.imputed
         ),
         computed AS (
           SELECT
             hist.timeseries_id,
             hist.date,
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
             END AS doy_count_30yr
           FROM hist
         )
         UPDATE continuous.measurements_calculated_daily mcd
         SET
           percent_historic_range = computed.percent_historic_range,
           max = computed.max,
           min = computed.min,
           q90 = computed.q90,
           q75 = computed.q75,
           q50 = computed.q50,
           q25 = computed.q25,
           q10 = computed.q10,
           mean = computed.mean,
           doy_count = computed.doy_count,
           percent_historic_range_30yr = computed.percent_historic_range_30yr,
           max_30yr = computed.max_30yr,
           min_30yr = computed.min_30yr,
           q90_30yr = computed.q90_30yr,
           q75_30yr = computed.q75_30yr,
           q50_30yr = computed.q50_30yr,
           q25_30yr = computed.q25_30yr,
           q10_30yr = computed.q10_30yr,
           mean_30yr = computed.mean_30yr,
           doy_count_30yr = computed.doy_count_30yr,
           modified = CURRENT_TIMESTAMP,
           modified_by = CURRENT_USER
         FROM computed
         WHERE mcd.timeseries_id = computed.timeseries_id
           AND mcd.date = computed.date
           AND (
             mcd.percent_historic_range IS DISTINCT FROM computed.percent_historic_range OR
             mcd.max IS DISTINCT FROM computed.max OR
             mcd.min IS DISTINCT FROM computed.min OR
             mcd.q90 IS DISTINCT FROM computed.q90 OR
             mcd.q75 IS DISTINCT FROM computed.q75 OR
             mcd.q50 IS DISTINCT FROM computed.q50 OR
             mcd.q25 IS DISTINCT FROM computed.q25 OR
             mcd.q10 IS DISTINCT FROM computed.q10 OR
             mcd.mean IS DISTINCT FROM computed.mean OR
             mcd.doy_count IS DISTINCT FROM computed.doy_count OR
             mcd.percent_historic_range_30yr IS DISTINCT FROM computed.percent_historic_range_30yr OR
             mcd.max_30yr IS DISTINCT FROM computed.max_30yr OR
             mcd.min_30yr IS DISTINCT FROM computed.min_30yr OR
             mcd.q90_30yr IS DISTINCT FROM computed.q90_30yr OR
             mcd.q75_30yr IS DISTINCT FROM computed.q75_30yr OR
             mcd.q50_30yr IS DISTINCT FROM computed.q50_30yr OR
             mcd.q25_30yr IS DISTINCT FROM computed.q25_30yr OR
             mcd.q10_30yr IS DISTINCT FROM computed.q10_30yr OR
             mcd.mean_30yr IS DISTINCT FROM computed.mean_30yr OR
             mcd.doy_count_30yr IS DISTINCT FROM computed.doy_count_30yr
           );

         SELECT EXISTS (
           SELECT 1
           FROM generate_series(v_start_date, v_end_date, interval '1 day') AS d
           WHERE to_char(d::date, 'MM-DD') = '02-29'
              OR continuous.normalized_day_of_year(d::date) IN (59, 60)
         ) INTO v_recalc_feb29;

         IF v_recalc_feb29
           AND to_char((now() AT TIME ZONE 'UTC')::date, 'MM-DD') NOT IN ('02-29', '03-01', '03-02') THEN
           WITH feb AS (
             SELECT f.*
             FROM continuous.measurements_calculated_daily f
             WHERE f.timeseries_id = p_timeseries_id
               AND f.date >= v_start_date
               AND to_char(f.date, 'MM-DD') = '02-29'
           ),
           computed AS (
             SELECT
               feb.timeseries_id,
               feb.date,
               CASE WHEN b.percent_historic_range IS NOT NULL AND a.percent_historic_range IS NOT NULL THEN (b.percent_historic_range + a.percent_historic_range) / 2 ELSE NULL::numeric END AS percent_historic_range,
               CASE WHEN b.max IS NOT NULL AND a.max IS NOT NULL THEN (b.max + a.max) / 2 WHEN b.max IS NULL AND a.max IS NULL AND feb.value IS NOT NULL THEN feb.value ELSE NULL::numeric END AS max,
               CASE WHEN b.min IS NOT NULL AND a.min IS NOT NULL THEN (b.min + a.min) / 2 WHEN b.min IS NULL AND a.min IS NULL AND feb.value IS NOT NULL THEN feb.value ELSE NULL::numeric END AS min,
               CASE WHEN b.q90 IS NOT NULL AND a.q90 IS NOT NULL THEN (b.q90 + a.q90) / 2 ELSE NULL::numeric END AS q90,
               CASE WHEN b.q75 IS NOT NULL AND a.q75 IS NOT NULL THEN (b.q75 + a.q75) / 2 ELSE NULL::numeric END AS q75,
               CASE WHEN b.q50 IS NOT NULL AND a.q50 IS NOT NULL THEN (b.q50 + a.q50) / 2 ELSE NULL::numeric END AS q50,
               CASE WHEN b.q25 IS NOT NULL AND a.q25 IS NOT NULL THEN (b.q25 + a.q25) / 2 ELSE NULL::numeric END AS q25,
               CASE WHEN b.q10 IS NOT NULL AND a.q10 IS NOT NULL THEN (b.q10 + a.q10) / 2 ELSE NULL::numeric END AS q10,
               CASE WHEN b.mean IS NOT NULL AND a.mean IS NOT NULL THEN (b.mean + a.mean) / 2 ELSE NULL::numeric END AS mean,
               CASE WHEN b.doy_count IS NOT NULL AND a.doy_count IS NOT NULL THEN LEAST(b.doy_count, a.doy_count) WHEN b.doy_count IS NULL AND a.doy_count IS NULL AND feb.value IS NOT NULL THEN 1 ELSE NULL::integer END AS doy_count,
               CASE WHEN b.percent_historic_range_30yr IS NOT NULL AND a.percent_historic_range_30yr IS NOT NULL THEN (b.percent_historic_range_30yr + a.percent_historic_range_30yr) / 2 ELSE NULL::numeric END AS percent_historic_range_30yr,
               CASE WHEN b.max_30yr IS NOT NULL AND a.max_30yr IS NOT NULL THEN (b.max_30yr + a.max_30yr) / 2 WHEN b.max_30yr IS NULL AND a.max_30yr IS NULL AND feb.value IS NOT NULL THEN feb.value ELSE NULL::numeric END AS max_30yr,
               CASE WHEN b.min_30yr IS NOT NULL AND a.min_30yr IS NOT NULL THEN (b.min_30yr + a.min_30yr) / 2 WHEN b.min_30yr IS NULL AND a.min_30yr IS NULL AND feb.value IS NOT NULL THEN feb.value ELSE NULL::numeric END AS min_30yr,
               CASE WHEN b.q90_30yr IS NOT NULL AND a.q90_30yr IS NOT NULL THEN (b.q90_30yr + a.q90_30yr) / 2 ELSE NULL::numeric END AS q90_30yr,
               CASE WHEN b.q75_30yr IS NOT NULL AND a.q75_30yr IS NOT NULL THEN (b.q75_30yr + a.q75_30yr) / 2 ELSE NULL::numeric END AS q75_30yr,
               CASE WHEN b.q50_30yr IS NOT NULL AND a.q50_30yr IS NOT NULL THEN (b.q50_30yr + a.q50_30yr) / 2 ELSE NULL::numeric END AS q50_30yr,
               CASE WHEN b.q25_30yr IS NOT NULL AND a.q25_30yr IS NOT NULL THEN (b.q25_30yr + a.q25_30yr) / 2 ELSE NULL::numeric END AS q25_30yr,
               CASE WHEN b.q10_30yr IS NOT NULL AND a.q10_30yr IS NOT NULL THEN (b.q10_30yr + a.q10_30yr) / 2 ELSE NULL::numeric END AS q10_30yr,
               CASE WHEN b.mean_30yr IS NOT NULL AND a.mean_30yr IS NOT NULL THEN (b.mean_30yr + a.mean_30yr) / 2 ELSE NULL::numeric END AS mean_30yr,
               CASE WHEN b.doy_count_30yr IS NOT NULL AND a.doy_count_30yr IS NOT NULL THEN LEAST(b.doy_count_30yr, a.doy_count_30yr) WHEN b.doy_count_30yr IS NULL AND a.doy_count_30yr IS NULL AND feb.value IS NOT NULL THEN 1 ELSE NULL::integer END AS doy_count_30yr
             FROM feb
             LEFT JOIN continuous.measurements_calculated_daily b
               ON b.timeseries_id = feb.timeseries_id
              AND b.date = feb.date - 1
             LEFT JOIN continuous.measurements_calculated_daily a
               ON a.timeseries_id = feb.timeseries_id
              AND a.date = feb.date + 1
           )
           UPDATE continuous.measurements_calculated_daily mcd
           SET
             percent_historic_range = computed.percent_historic_range,
             max = computed.max,
             min = computed.min,
             q90 = computed.q90,
             q75 = computed.q75,
             q50 = computed.q50,
             q25 = computed.q25,
             q10 = computed.q10,
             mean = computed.mean,
             doy_count = computed.doy_count,
             percent_historic_range_30yr = computed.percent_historic_range_30yr,
             max_30yr = computed.max_30yr,
             min_30yr = computed.min_30yr,
             q90_30yr = computed.q90_30yr,
             q75_30yr = computed.q75_30yr,
             q50_30yr = computed.q50_30yr,
             q25_30yr = computed.q25_30yr,
             q10_30yr = computed.q10_30yr,
             mean_30yr = computed.mean_30yr,
             doy_count_30yr = computed.doy_count_30yr,
             modified = CURRENT_TIMESTAMP,
             modified_by = CURRENT_USER
           FROM computed
           WHERE mcd.timeseries_id = computed.timeseries_id
             AND mcd.date = computed.date
             AND (
               mcd.percent_historic_range IS DISTINCT FROM computed.percent_historic_range OR
               mcd.max IS DISTINCT FROM computed.max OR
               mcd.min IS DISTINCT FROM computed.min OR
               mcd.q90 IS DISTINCT FROM computed.q90 OR
               mcd.q75 IS DISTINCT FROM computed.q75 OR
               mcd.q50 IS DISTINCT FROM computed.q50 OR
               mcd.q25 IS DISTINCT FROM computed.q25 OR
               mcd.q10 IS DISTINCT FROM computed.q10 OR
               mcd.mean IS DISTINCT FROM computed.mean OR
               mcd.doy_count IS DISTINCT FROM computed.doy_count OR
               mcd.percent_historic_range_30yr IS DISTINCT FROM computed.percent_historic_range_30yr OR
               mcd.max_30yr IS DISTINCT FROM computed.max_30yr OR
               mcd.min_30yr IS DISTINCT FROM computed.min_30yr OR
               mcd.q90_30yr IS DISTINCT FROM computed.q90_30yr OR
               mcd.q75_30yr IS DISTINCT FROM computed.q75_30yr OR
               mcd.q50_30yr IS DISTINCT FROM computed.q50_30yr OR
               mcd.q25_30yr IS DISTINCT FROM computed.q25_30yr OR
               mcd.q10_30yr IS DISTINCT FROM computed.q10_30yr OR
               mcd.mean_30yr IS DISTINCT FROM computed.mean_30yr OR
               mcd.doy_count_30yr IS DISTINCT FROM computed.doy_count_30yr
             );
         END IF;

         PERFORM continuous.trim_continuous_timeseries_tail(p_timeseries_id);

         UPDATE continuous.timeseries t
         SET last_daily_calculation = CURRENT_TIMESTAMP
         WHERE t.timeseries_id = p_timeseries_id;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.refresh_calculated_daily(integer, date, date)
       OWNER TO postgres;"
    )

    message(
      "Recreating calculated-daily as-of functions with rolling 30-year columns..."
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       );"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION audit.measurements_calculated_daily_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       );"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.measurements_calculated_daily_as_of(
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
         no_update BOOLEAN,
         created_by TEXT,
         modified_by TEXT,
         created TIMESTAMPTZ,
         modified TIMESTAMPTZ
       )
       LANGUAGE plpgsql
       STABLE
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       BEGIN
         RETURN QUERY EXECUTE
         $sql$
           WITH candidate_row_ids AS (
             SELECT mcd.measurement_row_id
             FROM continuous.measurements_calculated_daily mcd
             WHERE ($1 IS NULL OR mcd.timeseries_id = ANY($1))
               AND ($2 IS NULL OR mcd.date >= $2)
               AND ($3 IS NULL OR mcd.date <= $3)
             UNION
             SELECT log.measurement_row_id
             FROM audit.measurements_calculated_daily_log log
             WHERE log.measurement_row_id IS NOT NULL
               AND log.action_timestamp > $4
               AND (
                 $1 IS NULL OR
                   (log.original_data ->> 'timeseries_id')::integer = ANY($1)
               )
               AND (
                 $2 IS NULL OR
                   (log.original_data ->> 'date')::date >= $2
               )
               AND (
                 $3 IS NULL OR
                   (log.original_data ->> 'date')::date <= $3
               )
           ),
           current_rows AS (
             SELECT
               mcd.measurement_row_id,
               to_jsonb(mcd) AS row_json,
               mcd.created AS row_created
             FROM continuous.measurements_calculated_daily mcd
             JOIN candidate_row_ids ids
               ON ids.measurement_row_id = mcd.measurement_row_id
           ),
           future_changes AS (
             SELECT DISTINCT ON (log.measurement_row_id)
               log.measurement_row_id,
               log.original_data AS row_json,
               NULLIF(log.original_data ->> 'created', '')::timestamptz AS row_created
             FROM audit.measurements_calculated_daily_log log
             JOIN candidate_row_ids ids
               ON ids.measurement_row_id = log.measurement_row_id
             WHERE log.measurement_row_id IS NOT NULL
               AND log.action_timestamp > $4
             ORDER BY
               log.measurement_row_id,
               log.action_timestamp ASC,
               log.log_id ASC
           ),
           snapshot_json AS (
             SELECT
               COALESCE(f.measurement_row_id, c.measurement_row_id) AS measurement_row_id,
               COALESCE(f.row_json, c.row_json) AS row_json,
               COALESCE(f.row_created, c.row_created) AS row_created
             FROM current_rows c
             FULL OUTER JOIN future_changes f
               ON c.measurement_row_id = f.measurement_row_id
           )
           SELECT
             r.timeseries_id,
             r.date,
             r.value,
             r.imputed,
             r.percent_historic_range,
             r.max,
             r.min,
             r.q90,
             r.q75,
             r.q50,
             r.q25,
             r.q10,
             r.mean,
             r.doy_count,
             r.percent_historic_range_30yr,
             r.max_30yr,
             r.min_30yr,
             r.q90_30yr,
             r.q75_30yr,
             r.q50_30yr,
             r.q25_30yr,
             r.q10_30yr,
             r.mean_30yr,
             r.doy_count_30yr,
             r.no_update,
             r.created_by,
             r.modified_by,
             r.created,
             r.modified
           FROM snapshot_json s
           CROSS JOIN LATERAL jsonb_populate_record(
             NULL::continuous.measurements_calculated_daily,
             s.row_json
           ) AS r
           WHERE COALESCE(r.created, s.row_created) <= $4
             AND ($1 IS NULL OR r.timeseries_id = ANY($1))
             AND ($2 IS NULL OR r.date >= $2)
             AND ($3 IS NULL OR r.date <= $3)
           ORDER BY r.timeseries_id, r.date
         $sql$
         USING p_timeseries_ids, p_start_date, p_end_date, p_as_of;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.measurements_calculated_daily_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) OWNER TO postgres;"
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
         WITH visible_timeseries AS (
           SELECT ts.timeseries_id
           FROM continuous.timeseries ts
           WHERE (p_timeseries_ids IS NULL OR ts.timeseries_id = ANY(p_timeseries_ids))
         )
         SELECT
           mcd.timeseries_id,
           mcd.date,
           mcd.value,
           mcd.imputed,
           mcd.percent_historic_range,
           mcd.max,
           mcd.min,
           mcd.q90,
           mcd.q75,
           mcd.q50,
           mcd.q25,
           mcd.q10,
           mcd.mean,
           mcd.doy_count,
           mcd.percent_historic_range_30yr,
           mcd.max_30yr,
           mcd.min_30yr,
           mcd.q90_30yr,
           mcd.q75_30yr,
           mcd.q50_30yr,
           mcd.q25_30yr,
           mcd.q10_30yr,
           mcd.mean_30yr,
           mcd.doy_count_30yr,
           mcd.created,
           mcd.modified
         FROM audit.measurements_calculated_daily_as_of(
           p_as_of,
           p_timeseries_ids,
           p_start_date,
           p_end_date
         ) mcd
         JOIN visible_timeseries vt
           ON mcd.timeseries_id = vt.timeseries_id
         ORDER BY mcd.timeseries_id, mcd.date;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) OWNER TO postgres;"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.measurements_calculated_daily_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) FROM PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.measurements_calculated_daily_at(
         TIMESTAMPTZ,
         INTEGER[],
         DATE,
         DATE
       ) FROM PUBLIC;"
    )

    for (i in seq_len(nrow(function_execute_grants))) {
      role_name <- function_execute_grants$role_name[[i]]
      quoted_role <- if (identical(role_name, "PUBLIC")) {
        "PUBLIC"
      } else {
        q_ident(role_name)
      }
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT EXECUTE ON FUNCTION %s TO %s;",
          function_execute_grants$function_signature[[i]],
          quoted_role
        )
      )
    }

    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.refresh_calculated_daily(integer, date, date)
       TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.refresh_calculated_daily(integer[], date, date)
       TO PUBLIC;"
    )

    message(
      "Updating comments for calculated-daily rolling 30-year statistics..."
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.trim_continuous_timeseries_tail(integer) IS
       'Deletes trailing calculated-daily rows for basic timeseries after the last raw continuous measurement and trims approval, grade, qualifier, owner, and contributor intervals so they do not extend beyond existing raw data.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.percent_historic_range_30yr IS
       'Rolling 30-year percent of historical range for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.max_30yr IS
       'Rolling 30-year maximum corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.min_30yr IS
       'Rolling 30-year minimum corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q90_30yr IS
       'Rolling 30-year 90th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q75_30yr IS
       'Rolling 30-year 75th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q50_30yr IS
       'Rolling 30-year median corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q25_30yr IS
       'Rolling 30-year 25th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.q10_30yr IS
       'Rolling 30-year 10th percentile corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.mean_30yr IS
       'Rolling 30-year mean corrected daily value for the same day of year before this date; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_calculated_daily.doy_count_30yr IS
       'Count of non-null same-day-of-year values in the rolling 30-year historical window plus the current row when non-null; maintained by continuous.refresh_calculated_daily().';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.measurements_calculated_daily_as_of(TIMESTAMPTZ, INTEGER[], DATE, DATE) IS
       'Reconstructs stored calculated daily rows as they existed at a requested timestamp using the current table plus audit.measurements_calculated_daily_log, including rolling 30-year historical statistic columns. Daily rows are stored outputs of database refresh triggers, not values recomputed by this function.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_calculated_daily_at(TIMESTAMPTZ, INTEGER[], DATE, DATE) IS
       'Point-in-time access function for stored calculated daily rows, including rolling 30-year historical statistic columns. It reconstructs rows from audit history and does not recompute daily values. Visibility is filtered through the caller''s current access to continuous.timeseries.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_calculated_daily(INTEGER, DATE, DATE) IS
       'Refreshes stored daily aggregate rows, full-history statistics, rolling 30-year statistics, and trailing-row cleanup for one continuous timeseries over the affected date range.';"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS continuous.refresh_calculated_daily_30yr_statistics(integer, date, date);"
    )

    DBI::dbExecute(con, "COMMIT;")
    active <- FALSE

    message(
      "Creating day-of-year calculated-daily lookup index concurrently..."
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX CONCURRENTLY IF NOT EXISTS measurements_calculated_daily_tsid_doy_date_idx
       ON continuous.measurements_calculated_daily (
         timeseries_id,
         continuous.normalized_day_of_year(date),
         date
       )
       INCLUDE (value, imputed)
       WHERE continuous.normalized_day_of_year(date) IS NOT NULL;"
    )

    message(
      "Schema and function changes committed. Starting rolling 30-year backfill outside the patch transaction."
    )

    message(
      "Backfilling rolling 30-year statistics for existing calculated-daily rows. Each timeseries_id is protected by the aquacache_timeseries advisory lock used by getNewContinuous() and synchronize_continuous() to prevent these functions from conflicting with the backfill."
    )
    backfill_ids <- DBI::dbGetQuery(
      con,
      "SELECT t.timeseries_id
       FROM continuous.timeseries t
       WHERE t.record_rate IS NULL OR t.record_rate <= interval '1 day'
       ORDER BY t.timeseries_id"
    )$timeseries_id

    lock_namespace <- "aquacache_timeseries"

    if (length(backfill_ids) > 0) {
      pb <- utils::txtProgressBar(
        min = 0,
        max = length(backfill_ids),
        style = 3
      )
      on.exit(close(pb), add = TRUE)
      for (i in seq_along(backfill_ids)) {
        tsid <- backfill_ids[[i]]
        utils::setTxtProgressBar(pb, i)
        lock_acquired <- FALSE
        tryCatch(
          {
            DBI::dbExecute(
              con,
              "SELECT pg_advisory_lock(hashtext($1), $2);",
              params = list(lock_namespace, tsid)
            )
            lock_acquired <- TRUE

            DBI::dbExecute(
              con,
              "SELECT continuous.refresh_calculated_daily(
                 $1::integer,
                 NULL::date,
                 NULL::date
               )",
              params = list(tsid)
            )
          },
          finally = {
            if (lock_acquired) {
              DBI::dbGetQuery(
                con,
                "SELECT pg_advisory_unlock(hashtext($1), $2);",
                params = list(lock_namespace, tsid)
              )
            }
          }
        )
      }
    }
    message("Backfill complete.")

    active <- dbTransBegin(con)
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '48'
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
      "Patch 48 applied successfully. Calculated daily rows now trim trailing no-data dates, accessory intervals are bounded by existing raw data, and rolling 30-year daily statistics are maintained and exposed through as-of functions."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    } else {
      message(
        "Error detected outside an active patch transaction. Schema/function changes may already be committed; rerun the patch to resume the backfill after addressing the error."
      )
    }
    stop(e)
  }
)
