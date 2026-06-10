message("Starting patch 49: update public.get_csw_layer() for public-safe location geometry.")

dry_run <- exists("patch_49_dry_run", inherits = TRUE) &&
  isTRUE(get("patch_49_dry_run", inherits = TRUE))

if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- AquaConnect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
}

DBI::dbExecute(con, "BEGIN;")

tryCatch(
  {
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.get_csw_layer()
       RETURNS TABLE(
         location text,
         station_name text,
         station_name_fr text,
         latitude numeric,
         longitude numeric,
         type text,
         owner_name text,
         owner_name_fr text,
         timeseries_id integer,
         parameter_id integer,
         param_name text,
         param_name_fr text,
         param_units text,
         date date,
         value numeric,
         percent_historic_range numeric,
         mean numeric,
         min numeric,
         max numeric,
         doy_count integer,
         drainage_area_km2 numeric,
         datum_name_en text,
         datum_name_fr text
       )
       LANGUAGE sql
       AS $function$
       WITH core AS (
         SELECT
           l.location_id,
           l.public_location_id,
           l.location_code,
           l.name,
           l.name_fr,
           COALESCE(
             l.latitude,
             ST_Y(ST_PointOnSurface(l.public_geom))::numeric
           ) AS latitude,
           COALESCE(
             l.longitude,
             ST_X(ST_PointOnSurface(l.public_geom))::numeric
           ) AS longitude,
           l.location_type_name AS type,
           t.timeseries_id,
           t.parameter_id,
           t.sub_location_id,
           p.param_name,
           p.param_name_fr,
           public.get_parameter_unit_name(
             t.parameter_id,
             t.matrix_state_id
           ) AS param_units,
           mcd.date,
           mcd.value,
           mcd.percent_historic_range,
           mcd.mean,
           mcd.min,
           mcd.max,
           mcd.doy_count
         FROM public.locations_public l
         JOIN continuous.timeseries t
           ON t.location_id = l.location_id
         JOIN public.parameters p
           ON p.parameter_id = t.parameter_id
         JOIN continuous.measurements_calculated_daily mcd
           ON mcd.timeseries_id = t.timeseries_id
         WHERE
           l.location_type IN (1, 2, 16)
           AND l.jurisdictional_relevance IS TRUE
           AND p.parameter_id IN (1150, 1165, 21, 1220)
           AND mcd.date >= NOW() - INTERVAL '30 days'
           AND COALESCE(
             l.latitude,
             ST_Y(ST_PointOnSurface(l.public_geom))::numeric
           ) IS NOT NULL
           AND COALESCE(
             l.longitude,
             ST_X(ST_PointOnSurface(l.public_geom))::numeric
           ) IS NOT NULL
       ),
       core_locs AS (
         SELECT DISTINCT
           c.location_id,
           c.location_code
         FROM core c
       ),
       loc_owner AS (
         SELECT DISTINCT ON (t2.location_id)
           t2.location_id,
           org.name AS owner_name,
           org.name_fr AS owner_name_fr
         FROM continuous.timeseries t2
         JOIN core_locs cl
           ON cl.location_id = t2.location_id
         JOIN continuous.owners o
           ON o.timeseries_id = t2.timeseries_id
         JOIN public.organizations org
           ON org.organization_id = o.organization_id
         ORDER BY
           t2.location_id,
           o.start_dt DESC,
           o.end_dt DESC,
           o.owner_id DESC
       ),
       datum_current AS (
         SELECT DISTINCT ON (dc.location_id)
           dc.location_id,
           dc.conversion_m,
           dl.datum_name_en,
           dl.datum_name_fr
         FROM public.datum_conversions dc
         JOIN core_locs cl
           ON cl.location_id = dc.location_id
         LEFT JOIN public.datum_list dl
           ON dl.datum_id = dc.datum_id_to
         WHERE dc.current IS TRUE
         ORDER BY
           dc.location_id,
           dc.modified DESC NULLS LAST,
           dc.conversion_id DESC
       ),
       drainage AS (
         SELECT
           v.feature_name,
           (ST_Area(v.geom::geography) / 1000000)::numeric AS drainage_area_km2
         FROM spatial.vectors v
         JOIN core_locs cl
           ON cl.location_code = v.feature_name
         WHERE v.layer_name = 'Drainage basins'
       )
       SELECT
         c.location_code AS location,
         CASE
           WHEN sl.sub_location_name IS NOT NULL
             THEN CONCAT(c.name, ' - ', sl.sub_location_name)
           ELSE c.name
         END AS station_name,
         CASE
           WHEN sl.sub_location_name_fr IS NOT NULL
             THEN CONCAT(c.name_fr, ' - ', sl.sub_location_name_fr)
           ELSE c.name_fr
         END AS station_name_fr,
         c.latitude,
         c.longitude,
         c.type,
         lo.owner_name,
         lo.owner_name_fr,
         c.timeseries_id,
         c.parameter_id,
         c.param_name,
         c.param_name_fr,
         c.param_units,
         c.date,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.value + COALESCE(dc.conversion_m, 0)
           ELSE c.value
         END AS value,
         c.percent_historic_range,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.mean + COALESCE(dc.conversion_m, 0)
           ELSE c.mean
         END AS mean,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.min + COALESCE(dc.conversion_m, 0)
           ELSE c.min
         END AS min,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.max + COALESCE(dc.conversion_m, 0)
           ELSE c.max
         END AS max,
         c.doy_count,
         dr.drainage_area_km2,
         CASE
           WHEN c.param_name = 'water level'
             THEN dc.datum_name_en
           ELSE NULL
         END AS datum_name_en,
         CASE
           WHEN c.param_name = 'water level'
             THEN dc.datum_name_fr
           ELSE NULL
         END AS datum_name_fr
       FROM core c
       LEFT JOIN public.sub_locations sl
         ON sl.sub_location_id = c.sub_location_id
       LEFT JOIN loc_owner lo
         ON lo.location_id = c.location_id
       LEFT JOIN datum_current dc
         ON dc.location_id = c.location_id
       LEFT JOIN drainage dr
         ON dr.feature_name = c.location_code
       ORDER BY c.location_code, c.param_name, c.date;
       $function$"
    )

    check <- DBI::dbGetQuery(
      con,
      "SELECT to_regprocedure('public.get_csw_layer()') IS NOT NULL AS ok"
    )
    if (!isTRUE(check$ok[[1]])) {
      stop("public.get_csw_layer() was not created.")
    }

    if (dry_run) {
      DBI::dbExecute(con, "ROLLBACK;")
      message("Patch 49 dry run completed successfully and was rolled back.")
    } else {
      DBI::dbExecute(
        con,
        "UPDATE information.version_info SET version = '49'
         WHERE item = 'Last patch number';"
      )
      DBI::dbExecute(con, "COMMIT;")
      message("Patch 49 applied successfully.")
    }
  },
  error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    stop("Patch 49 failed: ", e$message)
  }
)
