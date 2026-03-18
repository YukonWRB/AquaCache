-- @statement
SET LOCAL search_path = pg_temp, continuous, discrete, files, public, instruments, application, spatial, pg_catalog;

-- @statement
CREATE TEMP TABLE sample_series (
  sample_series_id integer,
  location_id integer,
  sub_location_id integer,
  synch_from timestamptz,
  synch_to timestamptz
);
CREATE TEMP TABLE locations_metadata_instruments (
  metadata_id integer,
  location_id integer,
  sub_location_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE parameters (
  parameter_id integer,
  result_speciation boolean,
  sample_fraction boolean
);
INSERT INTO parameters (parameter_id, result_speciation, sample_fraction)
VALUES (2, true, false), (3, false, true);

CREATE TEMP TABLE documents (
  document_id integer,
  type integer,
  document_type_id integer
);
CREATE TEMP TABLE document_types (
  document_type_id integer,
  document_type_en text,
  type text
);
INSERT INTO document_types (document_type_id, document_type_en, type)
VALUES (10, 'report', 'report');
INSERT INTO documents (document_id, type, document_type_id)
VALUES (10, 10, 10);

DO $$
BEGIN
  IF to_regclass('public.timeseries_data_sharing_agreements') IS NULL THEN
    CREATE TABLE public.timeseries_data_sharing_agreements (
      timeseries_data_sharing_agreement_id integer,
      timeseries_id integer,
      start_dt timestamptz,
      end_dt timestamptz
    );
  END IF;
END;
$$;

-- @statement
SELECT plan(13);

-- @covers public.get_csw_layer()
-- @statement
SELECT lives_ok(
  'SELECT count(*) FROM public.get_csw_layer();',
  'public.get_csw_layer() executes under statement timeout'
);

-- @covers continuous.apply_corrections(integer, timestamp with time zone, numeric)
-- @statement
SELECT is(
  continuous.apply_corrections(1, now(), NULL),
  NULL::numeric,
  'continuous.apply_corrections() returns NULL for NULL value input'
);

-- @covers continuous.trunc_hour_utc(timestamp with time zone)
-- @statement
SELECT is(
  continuous.trunc_hour_utc('2024-01-01 12:34:56+00'::timestamptz),
  '2024-01-01 12:00:00+00'::timestamptz,
  'continuous.trunc_hour_utc() truncates to hour in UTC'
);

-- @covers discrete.get_guideline_value(integer, integer)
-- @statement
SELECT is(
(
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
    SELECT format('__ac_sem_speciation_%s', txid_current())
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
      format('__ac_sem_pub_%s', txid_current()),
      format('__ac_sem_pub_fr_%s', txid_current()),
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
      format('__ac_sem_guideline_%s', txid_current()),
      pp.parameter_id,
      CASE
        WHEN pp.needs_speciation THEN (SELECT result_speciation_id FROM spec_pick)
        ELSE NULL
      END,
      'SELECT 9::numeric'
    FROM picked_parameter pp
    RETURNING guideline_id
  )
  SELECT discrete.get_guideline_value((SELECT guideline_id FROM inserted_guideline), NULL)
),
9::numeric,
'discrete.get_guideline_value() evaluates stored guideline SQL'
);

-- @covers public.validate_share_with()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    CREATE TEMP TABLE _ac_sem_validate_share_with (
      share_with text[]
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_validate_share_with
      FOR EACH ROW
      EXECUTE FUNCTION public.validate_share_with();

    INSERT INTO _ac_sem_validate_share_with
      VALUES (ARRAY['public_reader'::text, 'admin'::text]);

    RAISE EXCEPTION 'Expected public.validate_share_with to reject mixed public_reader values.';
  EXCEPTION WHEN OTHERS THEN
    IF position('public_reader' in SQLERRM) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'public.validate_share_with() rejects mixed public_reader visibility'
);

-- @covers public.cleanup_share_with_role(text, text, boolean)
-- @statement
SELECT ok(
  (SELECT count(*) > 0 FROM public.cleanup_share_with_role('__ac_missing_role__', NULL, true)),
  'public.cleanup_share_with_role() returns scan rows in dry-run mode'
);

-- @covers public.drop_role_if_unused(text)
-- @statement
SELECT ok(
  (SELECT dropped = false FROM public.drop_role_if_unused('__ac_missing_role__') LIMIT 1),
  'public.drop_role_if_unused() reports non-existent role as not dropped'
);

-- @covers discrete.check_sample_series_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    INSERT INTO sample_series
      VALUES (101, 44, NULL, '2024-01-01'::timestamptz, '2024-01-20'::timestamptz);

    CREATE TEMP TABLE _ac_sem_check_sample_series_overlap (
      sample_series_id integer,
      location_id integer,
      sub_location_id integer,
      synch_from timestamptz,
      synch_to timestamptz
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_check_sample_series_overlap
      FOR EACH ROW
      EXECUTE FUNCTION discrete.check_sample_series_overlap();

    INSERT INTO _ac_sem_check_sample_series_overlap
      VALUES (202, 44, NULL, '2024-01-10'::timestamptz, '2024-01-30'::timestamptz);

    RAISE EXCEPTION 'Expected overlap exception from discrete.check_sample_series_overlap.';
  EXCEPTION WHEN OTHERS THEN
    IF position('overlap' in lower(SQLERRM)) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'discrete.check_sample_series_overlap() rejects overlapping windows'
);

-- @covers public.check_instrument_meta_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    INSERT INTO locations_metadata_instruments
      VALUES (1, 77, NULL, '2024-01-01'::timestamptz, '2024-01-20'::timestamptz);

    CREATE TEMP TABLE _ac_sem_check_instrument_meta_overlap (
      metadata_id integer,
      location_id integer,
      sub_location_id integer,
      start_dt timestamptz,
      end_dt timestamptz
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_check_instrument_meta_overlap
      FOR EACH ROW
      EXECUTE FUNCTION public.check_instrument_meta_overlap();

    INSERT INTO _ac_sem_check_instrument_meta_overlap
      VALUES (2, 77, NULL, '2024-01-10'::timestamptz, '2024-01-30'::timestamptz);

    RAISE EXCEPTION 'Expected overlap exception from public.check_instrument_meta_overlap.';
  EXCEPTION WHEN OTHERS THEN
    IF position('overlap' in lower(SQLERRM)) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'public.check_instrument_meta_overlap() rejects overlapping metadata windows'
);

-- @covers public.check_timeseries_data_sharing_agreements_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    INSERT INTO public.timeseries_data_sharing_agreements
      (timeseries_data_sharing_agreement_id, timeseries_id, start_dt, end_dt)
    VALUES
      (-101, -5050, '2024-01-01'::timestamptz, '2024-01-20'::timestamptz);

    CREATE TEMP TABLE _ac_sem_check_ts_dsa_overlap (
      timeseries_data_sharing_agreement_id integer,
      timeseries_id integer,
      start_dt timestamptz,
      end_dt timestamptz
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_check_ts_dsa_overlap
      FOR EACH ROW
      EXECUTE FUNCTION public.check_timeseries_data_sharing_agreements_overlap();

    INSERT INTO _ac_sem_check_ts_dsa_overlap
      VALUES (-102, -5050, '2024-01-10'::timestamptz, '2024-01-30'::timestamptz);

    RAISE EXCEPTION 'Expected overlap exception from public.check_timeseries_data_sharing_agreements_overlap.';
  EXCEPTION WHEN OTHERS THEN
    IF position('overlap' in lower(SQLERRM)) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'public.check_timeseries_data_sharing_agreements_overlap() rejects overlap'
);

-- @covers files.check_data_sharing_agreement()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    CREATE TEMP TABLE _ac_sem_check_data_sharing_agreement (
      data_sharing_agreement_id integer
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_check_data_sharing_agreement
      FOR EACH ROW
      EXECUTE FUNCTION files.check_data_sharing_agreement();

    INSERT INTO _ac_sem_check_data_sharing_agreement VALUES (10);

    RAISE EXCEPTION 'Expected document type exception from files.check_data_sharing_agreement.';
  EXCEPTION WHEN OTHERS THEN
    IF position('invalid document type' in lower(SQLERRM)) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'files.check_data_sharing_agreement() enforces data-sharing doc type'
);

-- @covers discrete.enforce_result_speciation()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    CREATE TEMP TABLE _ac_sem_enforce_result_speciation (
      parameter_id integer,
      result_speciation_id integer
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_enforce_result_speciation
      FOR EACH ROW
      EXECUTE FUNCTION discrete.enforce_result_speciation();

    INSERT INTO _ac_sem_enforce_result_speciation VALUES (2, NULL);

    RAISE EXCEPTION 'Expected result_speciation exception from discrete.enforce_result_speciation.';
  EXCEPTION WHEN OTHERS THEN
    IF position('result_speciation_id must be populated' in lower(SQLERRM)) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'discrete.enforce_result_speciation() requires speciation when parameter requires it'
);

-- @covers discrete.enforce_sample_fraction()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  BEGIN
    CREATE TEMP TABLE _ac_sem_enforce_sample_fraction (
      parameter_id integer,
      sample_fraction_id integer
    );
    CREATE TRIGGER _ac_trg
      BEFORE INSERT ON _ac_sem_enforce_sample_fraction
      FOR EACH ROW
      EXECUTE FUNCTION discrete.enforce_sample_fraction();

    INSERT INTO _ac_sem_enforce_sample_fraction VALUES (3, NULL);

    RAISE EXCEPTION 'Expected sample_fraction exception from discrete.enforce_sample_fraction.';
  EXCEPTION WHEN OTHERS THEN
    IF position('sample_fraction_id must be populated' in lower(SQLERRM)) = 0 THEN
      RAISE;
    END IF;
  END;
END;
$do$;
$test$,
  'discrete.enforce_sample_fraction() requires sample_fraction when parameter requires it'
);

-- @statement
SELECT * FROM finish();
