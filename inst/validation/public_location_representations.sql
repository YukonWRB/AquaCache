-- Public location representation validation.
--
-- Run these checks after patch_48 has been applied. The first result set should
-- return only false-free object checks; the bad_count checks should return zero
-- rows. The final grant-audit query is intentionally informational: it shows
-- roles that can still query raw exact-coordinate sources directly and should
-- be reviewed before treating a public application role as view-only.

SET search_path = public, discrete, continuous, spatial, pg_catalog;

-- Required objects.
SELECT
  to_regclass('public.location_public_geometries') IS NOT NULL AS has_public_geometries,
  to_regclass('public.location_reporting_areas') IS NOT NULL AS has_reporting_areas,
  to_regclass('public.location_reporting_area_members') IS NOT NULL AS has_reporting_members,
  to_regclass('public.locations_public') IS NOT NULL AS has_locations_public,
  to_regclass('discrete.samples_public') IS NOT NULL AS has_samples_public,
  to_regclass('discrete.results_public') IS NOT NULL AS has_results_public,
  to_regprocedure('public.location_exact_visible(text[])') IS NOT NULL AS has_exact_visible,
  to_regprocedure(
    'public.location_masked_point(numeric,numeric,numeric,numeric,text)'
  ) IS NOT NULL AS has_masked_point;

-- Expected: zero rows.
SELECT check_name, bad_count
FROM (
  SELECT
    'locations without active public geometry' AS check_name,
    count(*)::integer AS bad_count
  FROM public.locations loc
  WHERE NOT EXISTS (
    SELECT 1
    FROM public.location_public_geometries lpg
    WHERE lpg.location_id = loc.location_id
      AND lpg.active
  )

  UNION ALL

  SELECT
    'locations with more than one active public geometry' AS check_name,
    count(*)::integer AS bad_count
  FROM (
    SELECT location_id
    FROM public.location_public_geometries
    WHERE active
    GROUP BY location_id
    HAVING count(*) > 1
  ) duplicates

  UNION ALL

  SELECT
    'masked points outside configured offset range' AS check_name,
    count(*)::integer AS bad_count
  FROM public.location_public_geometries lpg
  JOIN public.locations loc
    ON loc.location_id = lpg.location_id
  WHERE lpg.active
    AND lpg.public_geom_type = 'masked_point'
    AND (
      (
        lpg.min_offset_m IS NOT NULL
        AND ST_Distance(
          lpg.public_geom::geography,
          ST_SetSRID(
            ST_MakePoint(
              loc.longitude::double precision,
              loc.latitude::double precision
            ),
            4326
          )::geography
        ) < lpg.min_offset_m
      )
      OR (
        lpg.max_offset_m IS NOT NULL
        AND ST_Distance(
          lpg.public_geom::geography,
          ST_SetSRID(
            ST_MakePoint(
              loc.longitude::double precision,
              loc.latitude::double precision
            ),
            4326
          )::geography
        ) > lpg.max_offset_m
      )
    )

  UNION ALL

  SELECT
    'public-safe locations exposing exact geometry when exact is hidden' AS check_name,
    count(*)::integer AS bad_count
  FROM public.locations_public lp
  WHERE NOT lp.exact_location_visible
    AND lp.public_geom_type = 'exact_point'

  UNION ALL

  SELECT
    'public-safe locations without public_location_id' AS check_name,
    count(*)::integer AS bad_count
  FROM public.locations_public lp
  WHERE lp.public_location_id IS NULL

  UNION ALL

  SELECT
    'public-safe samples without public_location_id' AS check_name,
    count(*)::integer AS bad_count
  FROM discrete.samples_public sp
  WHERE sp.public_location_id IS NULL

  UNION ALL

  SELECT
    'public_reader can select raw location coordinates' AS check_name,
    count(*)::integer AS bad_count
  FROM (
    VALUES
      ('latitude'),
      ('longitude')
  ) AS coordinate_columns(column_name)
  WHERE EXISTS (
      SELECT 1
      FROM pg_roles r
      WHERE r.rolname = 'public_reader'
    )
    AND has_column_privilege(
      'public_reader',
      'public.locations',
      coordinate_columns.column_name,
      'SELECT'
    )

  UNION ALL

  SELECT
    'public-safe samples without public-safe location row' AS check_name,
    count(*)::integer AS bad_count
  FROM discrete.samples_public sp
  LEFT JOIN public.locations_public lp
    ON lp.location_id = sp.location_id
  WHERE lp.location_id IS NULL

  UNION ALL

  SELECT
    'public-safe results without public-safe sample row' AS check_name,
    count(*)::integer AS bad_count
  FROM discrete.results_public rp
  LEFT JOIN discrete.samples_public sp
    ON sp.sample_id = rp.sample_id
  WHERE sp.sample_id IS NULL
) checks
WHERE bad_count > 0;

-- Optional role check. Run inside a transaction as an admin:
--   BEGIN;
--   SET LOCAL ROLE public_reader;
--   <run the zero-row bad_count check above>
--   ROLLBACK;

-- Informational grant audit. These rows are not necessarily patch failures, but
-- any public application role listed with raw table or exact metadata access can
-- still bypass the public-safe views if it is allowed to run arbitrary SQL.
SELECT
  role_name,
  has_table_privilege(role_name, 'public.locations', 'SELECT') AS raw_locations_select,
  has_column_privilege(
    role_name,
    'public.locations',
    'latitude',
    'SELECT'
  ) AS raw_location_latitude_select,
  has_column_privilege(
    role_name,
    'public.locations',
    'longitude',
    'SELECT'
  ) AS raw_location_longitude_select,
  has_table_privilege(role_name, 'discrete.samples', 'SELECT') AS raw_samples_select,
  has_table_privilege(role_name, 'discrete.results', 'SELECT') AS raw_results_select,
  has_table_privilege(
    role_name,
    'public.location_metadata_en',
    'SELECT'
  ) AS location_metadata_select,
  has_table_privilege(
    role_name,
    'discrete.samples_metadata_en',
    'SELECT'
  ) AS samples_metadata_select,
  has_table_privilege(
    role_name,
    'discrete.results_metadata_en',
    'SELECT'
  ) AS results_metadata_select,
  has_table_privilege(
    role_name,
    'public.location_reporting_area_members',
    'SELECT'
  ) AS reporting_area_members_select
FROM (
  VALUES
    ('public_reader'),
    ('yg_reader_group'),
    ('yg_editor_group'),
    ('cyfn_editor_group'),
    ('tkc_group'),
    ('admin')
) AS roles(role_name)
WHERE EXISTS (
  SELECT 1
  FROM pg_roles r
  WHERE r.rolname = roles.role_name
)
ORDER BY role_name;
