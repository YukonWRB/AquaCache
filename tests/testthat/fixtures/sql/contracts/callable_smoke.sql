-- @statement
SET LOCAL search_path = pg_temp, continuous, discrete, files, public, instruments, application, spatial, pg_catalog;

-- @statement
SELECT plan(10);

-- @covers continuous.apply_corrections(integer, timestamp with time zone, numeric)
-- @statement
SELECT lives_ok(
  'SELECT continuous.apply_corrections(1, now(), 12.34::numeric);',
  'continuous.apply_corrections(integer, timestamptz, numeric) smoke'
);

-- @covers continuous.delete_old_forecasts()
-- @statement
SELECT lives_ok(
  'SELECT continuous.delete_old_forecasts();',
  'continuous.delete_old_forecasts() smoke'
);

-- @covers continuous.trunc_hour_utc(timestamp with time zone)
-- @statement
SELECT lives_ok(
  $$SELECT continuous.trunc_hour_utc('2024-01-01 12:34:56+00'::timestamptz);$$,
  'continuous.trunc_hour_utc(timestamptz) smoke'
);

-- @covers discrete.get_guideline_value(integer, integer)
-- @statement
SELECT lives_ok(
$test$
WITH picked_parameter AS (
  SELECT
    p.parameter_id,
    COALESCE(p.result_speciation, false) AS needs_speciation
  FROM public.parameters p
  ORDER BY COALESCE(p.result_speciation, false), p.parameter_id
  LIMIT 1
),
existing_spec AS (
  SELECT rs.result_speciation_id
  FROM discrete.result_speciations rs
  ORDER BY rs.result_speciation_id
  LIMIT 1
),
inserted_spec AS (
  INSERT INTO discrete.result_speciations (result_speciation)
  SELECT format('__ac_contract_speciation_%s', txid_current())
  WHERE NOT EXISTS (SELECT 1 FROM existing_spec)
  RETURNING result_speciation_id
),
spec_pick AS (
  SELECT result_speciation_id FROM existing_spec
  UNION ALL
  SELECT result_speciation_id FROM inserted_spec
  LIMIT 1
),
inserted_publisher AS (
  INSERT INTO discrete.guideline_publishers (
    publisher_name,
    publisher_name_fr,
    country,
    prov_terr_state
  )
  VALUES (
    format('__ac_contract_pub_%s', txid_current()),
    format('__ac_contract_pub_fr_%s', txid_current()),
    'CA',
    'YT'
  )
  RETURNING publisher_id
),
inserted_guideline AS (
  INSERT INTO discrete.guidelines (
    publisher,
    guideline_name,
    parameter_id,
    result_speciation_id,
    guideline_sql
  )
  SELECT
    (SELECT publisher_id FROM inserted_publisher),
    format('__ac_contract_guideline_%s', txid_current()),
    pp.parameter_id,
    CASE
      WHEN pp.needs_speciation THEN (SELECT result_speciation_id FROM spec_pick)
      ELSE NULL
    END,
    'SELECT 7::numeric'
  FROM picked_parameter pp
  RETURNING guideline_id
)
SELECT discrete.get_guideline_value(
  (SELECT guideline_id FROM inserted_guideline),
  NULL
);
$test$,
  'discrete.get_guideline_value(integer, integer) smoke'
);

-- @covers discrete.get_sample_hardness(integer)
-- @statement
SELECT lives_ok(
  'SELECT discrete.get_sample_hardness(0);',
  'discrete.get_sample_hardness(integer) smoke'
);

-- @covers discrete.get_sample_val(integer, integer, integer, integer)
-- @statement
SELECT lives_ok(
  'SELECT discrete.get_sample_val(0, 0, NULL, NULL);',
  'discrete.get_sample_val(integer, integer, integer, integer) smoke'
);

-- @covers public.cleanup_share_with_role(text, text, boolean)
-- @statement
SELECT lives_ok(
  $$SELECT count(*) FROM public.cleanup_share_with_role('__ac_missing_role__', NULL, true);$$,
  'public.cleanup_share_with_role(text, text, boolean) smoke'
);

-- @covers public.drop_role_if_unused(text)
-- @statement
SELECT lives_ok(
  $$SELECT * FROM public.drop_role_if_unused('__ac_missing_role__');$$,
  'public.drop_role_if_unused(text) smoke'
);

-- @covers public.get_csw_layer()
-- @statement
SELECT lives_ok(
  'SELECT count(*) FROM public.get_csw_layer();',
  'public.get_csw_layer() smoke'
);

-- @covers public.get_shareable_principals_for(regclass, text[], text[])
-- @statement
SELECT lives_ok(
  $$SELECT *
    FROM public.get_shareable_principals_for(
      'public.locations'::regclass,
      ARRAY['SELECT'::text],
      ARRAY['public_reader'::text, 'admin'::text]
    )
    LIMIT 10;$$,
  'public.get_shareable_principals_for(regclass, text[], text[]) smoke'
);

-- @statement
SELECT * FROM finish();
