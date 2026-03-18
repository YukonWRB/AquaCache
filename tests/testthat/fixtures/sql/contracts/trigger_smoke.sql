-- @statement
SET LOCAL search_path = pg_temp, continuous, discrete, files, public, instruments, application, spatial, pg_catalog;

-- @statement
CREATE TEMP TABLE approvals (
  approval_id integer,
  timeseries_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE contributors (
  organization_id integer,
  timeseries_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE grades (
  grade_id integer,
  timeseries_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE owners (
  organization_id integer,
  timeseries_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE qualifiers (
  qualifier_id integer,
  qualifier_type_id integer,
  timeseries_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE correction_types (
  correction_type_id integer,
  value1 boolean,
  value2 boolean,
  timestep_window boolean,
  equation boolean
);
INSERT INTO correction_types (correction_type_id, value1, value2, timestep_window, equation)
VALUES (1, false, false, false, false);

CREATE TEMP TABLE sample_series (
  sample_series_id integer,
  location_id integer,
  sub_location_id integer,
  synch_from timestamptz,
  synch_to timestamptz
);
CREATE TEMP TABLE parameters (
  parameter_id integer,
  result_speciation boolean,
  sample_fraction boolean
);
INSERT INTO parameters (parameter_id, result_speciation, sample_fraction)
VALUES (1, false, false), (2, true, false), (3, false, true);

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
VALUES (1, 'data sharing agreement', 'data sharing agreement');
INSERT INTO documents (document_id, type, document_type_id)
VALUES (1, 1, 1);

CREATE TEMP TABLE images (
  image_id integer
);
CREATE TEMP TABLE images_index (
  img_series_id integer,
  share_with text[]
);

CREATE TEMP TABLE locations_metadata_instruments (
  metadata_id integer,
  location_id integer,
  sub_location_id integer,
  start_dt timestamptz,
  end_dt timestamptz
);
CREATE TEMP TABLE locations_metadata_access (
  location_id integer,
  method text,
  notes text,
  health_safety text,
  start_datetime timestamptz,
  end_datetime timestamptz
);
CREATE TEMP TABLE locations_metadata_acquisition (
  location_id integer,
  start_datetime timestamptz,
  end_datetime timestamptz
);
CREATE TEMP TABLE locations_metadata_infrastructure (
  location_id integer,
  site_description text,
  infrastructure_description text,
  start_datetime timestamptz,
  end_datetime timestamptz
);
CREATE TEMP TABLE locations_metadata_infrastructure_groundwater (
  location_id integer,
  start_datetime timestamptz,
  end_datetime timestamptz
);
CREATE TEMP TABLE locations_metadata_infrastructure_hydromet (
  location_id integer,
  start_datetime timestamptz,
  end_datetime timestamptz
);
CREATE TEMP TABLE locations_metadata_owners_operators (
  location_id integer,
  owner integer,
  operator integer,
  start_datetime timestamptz,
  end_datetime timestamptz
);
CREATE TEMP TABLE locations_metadata_transmission (
  location_id integer,
  start_datetime timestamptz,
  end_datetime timestamptz
);

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
SELECT plan(38);

-- @covers boreholes.prevent_geology_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_prevent_geology_overlap (
    geo_record_id integer,
    borehole_id integer,
    depth_from_m numeric,
    depth_to_m numeric
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT OR UPDATE ON _ac_tg_prevent_geology_overlap
    FOR EACH ROW
    EXECUTE FUNCTION boreholes.prevent_geology_overlap();

  INSERT INTO _ac_tg_prevent_geology_overlap
    (geo_record_id, borehole_id, depth_from_m, depth_to_m)
  VALUES
    (1, 1, 0.0, 1.0);
END;
$do$;
$test$,
  'boreholes.prevent_geology_overlap() smoke'
);

-- @covers boreholes.prevent_permafrost_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_prevent_permafrost_overlap (
    permafrost_record_id integer,
    borehole_id integer,
    depth_from_m numeric,
    depth_to_m numeric
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT OR UPDATE ON _ac_tg_prevent_permafrost_overlap
    FOR EACH ROW
    EXECUTE FUNCTION boreholes.prevent_permafrost_overlap();

  INSERT INTO _ac_tg_prevent_permafrost_overlap
    (permafrost_record_id, borehole_id, depth_from_m, depth_to_m)
  VALUES
    (1, 1, 0.0, 1.0);
END;
$do$;
$test$,
  'boreholes.prevent_permafrost_overlap() smoke'
);

-- @covers continuous.check_approvals_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_approvals_overlap (
    approval_id integer,
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_approvals_overlap
    FOR EACH ROW
    EXECUTE FUNCTION continuous.check_approvals_overlap();

  INSERT INTO _ac_tg_check_approvals_overlap
    VALUES (1, 1, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'continuous.check_approvals_overlap() smoke'
);

-- @covers continuous.check_contributors_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_contributors_overlap (
    organization_id integer,
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_contributors_overlap
    FOR EACH ROW
    EXECUTE FUNCTION continuous.check_contributors_overlap();

  INSERT INTO _ac_tg_check_contributors_overlap
    VALUES (1, 1, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'continuous.check_contributors_overlap() smoke'
);

-- @covers continuous.check_grades_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_grades_overlap (
    grade_id integer,
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_grades_overlap
    FOR EACH ROW
    EXECUTE FUNCTION continuous.check_grades_overlap();

  INSERT INTO _ac_tg_check_grades_overlap
    VALUES (1, 1, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'continuous.check_grades_overlap() smoke'
);

-- @covers continuous.check_owners_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_owners_overlap (
    organization_id integer,
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_owners_overlap
    FOR EACH ROW
    EXECUTE FUNCTION continuous.check_owners_overlap();

  INSERT INTO _ac_tg_check_owners_overlap
    VALUES (1, 1, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'continuous.check_owners_overlap() smoke'
);

-- @covers continuous.check_qualifiers_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_qualifiers_overlap (
    qualifier_id integer,
    qualifier_type_id integer,
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_qualifiers_overlap
    FOR EACH ROW
    EXECUTE FUNCTION continuous.check_qualifiers_overlap();

  INSERT INTO _ac_tg_check_qualifiers_overlap
    VALUES (1, 1, 1, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'continuous.check_qualifiers_overlap() smoke'
);

-- @covers continuous.validate_corrections()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_validate_corrections (
    correction_type integer,
    value1 numeric,
    value2 numeric,
    timestep_window interval,
    equation text
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_validate_corrections
    FOR EACH ROW
    EXECUTE FUNCTION continuous.validate_corrections();

  INSERT INTO _ac_tg_validate_corrections
    VALUES (1, NULL, NULL, NULL, NULL);
END;
$do$;
$test$,
  'continuous.validate_corrections() smoke'
);

-- @covers discrete.check_sample_series_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_sample_series_overlap (
    sample_series_id integer,
    location_id integer,
    sub_location_id integer,
    synch_from timestamptz,
    synch_to timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_sample_series_overlap
    FOR EACH ROW
    EXECUTE FUNCTION discrete.check_sample_series_overlap();

  INSERT INTO _ac_tg_check_sample_series_overlap
    VALUES (1, 1, NULL, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'discrete.check_sample_series_overlap() smoke'
);

-- @covers discrete.enforce_result_speciation()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_enforce_result_speciation (
    parameter_id integer,
    result_speciation_id integer
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_enforce_result_speciation
    FOR EACH ROW
    EXECUTE FUNCTION discrete.enforce_result_speciation();

  INSERT INTO _ac_tg_enforce_result_speciation VALUES (1, NULL);
END;
$do$;
$test$,
  'discrete.enforce_result_speciation() smoke'
);

-- @covers discrete.enforce_sample_fraction()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_enforce_sample_fraction (
    parameter_id integer,
    sample_fraction_id integer
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_enforce_sample_fraction
    FOR EACH ROW
    EXECUTE FUNCTION discrete.enforce_sample_fraction();

  INSERT INTO _ac_tg_enforce_sample_fraction VALUES (1, NULL);
END;
$do$;
$test$,
  'discrete.enforce_sample_fraction() smoke'
);

-- @covers discrete.guidelines_validate_trg()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_guidelines_validate_trg (
    guideline_sql text
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_guidelines_validate_trg
    FOR EACH ROW
    EXECUTE FUNCTION discrete.guidelines_validate_trg();

  INSERT INTO _ac_tg_guidelines_validate_trg VALUES ('SELECT 1::numeric');
END;
$do$;
$test$,
  'discrete.guidelines_validate_trg() smoke'
);

-- @covers files.check_data_sharing_agreement()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_data_sharing_agreement (
    data_sharing_agreement_id integer
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_data_sharing_agreement
    FOR EACH ROW
    EXECUTE FUNCTION files.check_data_sharing_agreement();

  INSERT INTO _ac_tg_check_data_sharing_agreement VALUES (NULL);
END;
$do$;
$test$,
  'files.check_data_sharing_agreement() smoke'
);

-- @covers files.check_default_data_sharing_agreement()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_default_dsa (
    default_data_sharing_agreement_id integer
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_default_dsa
    FOR EACH ROW
    EXECUTE FUNCTION files.check_default_data_sharing_agreement();

  INSERT INTO _ac_tg_check_default_dsa VALUES (NULL);
END;
$do$;
$test$,
  'files.check_default_data_sharing_agreement() smoke'
);

-- @covers files.check_location_images()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_location_images (
    location_id integer,
    location_images integer[]
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_location_images
    FOR EACH ROW
    EXECUTE FUNCTION files.check_location_images();

  INSERT INTO _ac_tg_check_location_images VALUES (1, NULL);
END;
$do$;
$test$,
  'files.check_location_images() smoke'
);

-- @covers files.enforce_share_with_restriction()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_enforce_share_with_restriction (
    img_series_id integer,
    share_with text[]
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_enforce_share_with_restriction
    FOR EACH ROW
    EXECUTE FUNCTION files.enforce_share_with_restriction();

  INSERT INTO _ac_tg_enforce_share_with_restriction VALUES (NULL, ARRAY['public_reader'::text]);
END;
$do$;
$test$,
  'files.enforce_share_with_restriction() smoke'
);

-- @covers public.check_instrument_meta_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_instrument_meta_overlap (
    metadata_id integer,
    location_id integer,
    sub_location_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_instrument_meta_overlap
    FOR EACH ROW
    EXECUTE FUNCTION public.check_instrument_meta_overlap();

  INSERT INTO _ac_tg_check_instrument_meta_overlap
    VALUES (1, 1, NULL, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'public.check_instrument_meta_overlap() smoke'
);

-- @covers public.check_locations_metadata_acquisition_instruments()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_loc_meta_acq_instr (
    instruments integer[]
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_loc_meta_acq_instr
    FOR EACH ROW
    EXECUTE FUNCTION public.check_locations_metadata_acquisition_instruments();

  INSERT INTO _ac_tg_check_loc_meta_acq_instr VALUES (NULL);
END;
$do$;
$test$,
  'public.check_locations_metadata_acquisition_instruments() smoke'
);

-- @covers public.check_page_content_integrity()
-- @statement
SELECT lives_ok(
$test$
DO $do$
DECLARE
  v_content_id text := format('__ac_contract_text_%s', txid_current());
BEGIN
  INSERT INTO application.text (id, text_en)
  VALUES (v_content_id, 'contract smoke text');

  CREATE TEMP TABLE _ac_tg_check_page_content_integrity (
    content_type text,
    content_id text
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_page_content_integrity
    FOR EACH ROW
    EXECUTE FUNCTION public.check_page_content_integrity();

  INSERT INTO _ac_tg_check_page_content_integrity VALUES ('text', v_content_id);
END;
$do$;
$test$,
  'public.check_page_content_integrity() smoke'
);

-- @covers public.check_timeseries_data_sharing_agreements_overlap()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_check_ts_dsa_overlap (
    timeseries_data_sharing_agreement_id integer,
    timeseries_id integer,
    start_dt timestamptz,
    end_dt timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_check_ts_dsa_overlap
    FOR EACH ROW
    EXECUTE FUNCTION public.check_timeseries_data_sharing_agreements_overlap();

  INSERT INTO _ac_tg_check_ts_dsa_overlap
    VALUES (-1, -999999, '2024-01-01'::timestamptz, '2024-01-02'::timestamptz);
END;
$do$;
$test$,
  'public.check_timeseries_data_sharing_agreements_overlap() smoke'
);

-- @covers public.enforce_maintenance_constraints()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_enforce_maintenance_constraints (
    maintenance_performed text,
    date_performed date,
    maintenance_due text,
    maintenance_flag boolean
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_enforce_maintenance_constraints
    FOR EACH ROW
    EXECUTE FUNCTION public.enforce_maintenance_constraints();

  INSERT INTO _ac_tg_enforce_maintenance_constraints
    VALUES (NULL, NULL, 'inspect staff gauge', NULL);
END;
$do$;
$test$,
  'public.enforce_maintenance_constraints() smoke'
);

-- @covers public.fill_locations_metadata_access_missing()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_access_missing (
    location_id integer,
    method text,
    notes text,
    health_safety text,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_access_missing
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_access_missing();

  INSERT INTO _ac_tg_fill_loc_meta_access_missing
    VALUES (1, 'boat', NULL, NULL, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_access_missing() smoke'
);

-- @covers public.fill_locations_metadata_acquisition_missing()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_acq_missing (
    location_id integer,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_acq_missing
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_acquisition_missing();

  INSERT INTO _ac_tg_fill_loc_meta_acq_missing
    VALUES (1, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_acquisition_missing() smoke'
);

-- @covers public.fill_locations_metadata_infrastructure_groundwater()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_infra_gw (
    location_id integer,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_infra_gw
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_groundwater();

  INSERT INTO _ac_tg_fill_loc_meta_infra_gw
    VALUES (1, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_infrastructure_groundwater() smoke'
);

-- @covers public.fill_locations_metadata_infrastructure_hydromet()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_infra_hm (
    location_id integer,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_infra_hm
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_hydromet();

  INSERT INTO _ac_tg_fill_loc_meta_infra_hm
    VALUES (1, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_infrastructure_hydromet() smoke'
);

-- @covers public.fill_locations_metadata_infrastructure_missing()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_infra_missing (
    location_id integer,
    site_description text,
    infrastructure_description text,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_infra_missing
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_missing();

  INSERT INTO _ac_tg_fill_loc_meta_infra_missing
    VALUES (1, 'site desc', NULL, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_infrastructure_missing() smoke'
);

-- @covers public.fill_locations_metadata_owners_operators_missing()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_owner_missing (
    location_id integer,
    owner integer,
    operator integer,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_owner_missing
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_owners_operators_missing();

  INSERT INTO _ac_tg_fill_loc_meta_owner_missing
    VALUES (1, 1, NULL, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_owners_operators_missing() smoke'
);

-- @covers public.fill_locations_metadata_transmission_missing()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_fill_loc_meta_trans_missing (
    location_id integer,
    start_datetime timestamptz,
    end_datetime timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_fill_loc_meta_trans_missing
    FOR EACH ROW
    EXECUTE FUNCTION public.fill_locations_metadata_transmission_missing();

  INSERT INTO _ac_tg_fill_loc_meta_trans_missing
    VALUES (1, now(), NULL);
END;
$do$;
$test$,
  'public.fill_locations_metadata_transmission_missing() smoke'
);

-- @covers public.update_modified()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_update_modified (
    id integer,
    modified timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE UPDATE ON _ac_tg_update_modified
    FOR EACH ROW
    EXECUTE FUNCTION public.update_modified();

  INSERT INTO _ac_tg_update_modified VALUES (1, NULL);
  UPDATE _ac_tg_update_modified SET id = 2 WHERE id = 1;
END;
$do$;
$test$,
  'public.update_modified() smoke'
);

-- @covers public.update_updated()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_update_updated (
    id integer,
    updated timestamptz
  );
  CREATE TRIGGER _ac_trg
    BEFORE UPDATE ON _ac_tg_update_updated
    FOR EACH ROW
    EXECUTE FUNCTION public.update_updated();

  INSERT INTO _ac_tg_update_updated VALUES (1, NULL);
  UPDATE _ac_tg_update_updated SET id = 2 WHERE id = 1;
END;
$do$;
$test$,
  'public.update_updated() smoke'
);

-- @covers public.user_modified()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_user_modified (
    id integer,
    modified_by text
  );
  CREATE TRIGGER _ac_trg
    BEFORE UPDATE ON _ac_tg_user_modified
    FOR EACH ROW
    EXECUTE FUNCTION public.user_modified();

  INSERT INTO _ac_tg_user_modified VALUES (1, NULL);
  UPDATE _ac_tg_user_modified SET id = 2 WHERE id = 1;
END;
$do$;
$test$,
  'public.user_modified() smoke'
);

-- @covers public.validate_documents_array()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_validate_documents_array (
    documents integer[]
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_validate_documents_array
    FOR EACH ROW
    EXECUTE FUNCTION public.validate_documents_array();

  INSERT INTO _ac_tg_validate_documents_array VALUES (ARRAY[1]);
END;
$do$;
$test$,
  'public.validate_documents_array() smoke'
);

-- @covers public.validate_share_with()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_validate_share_with (
    share_with text[]
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT ON _ac_tg_validate_share_with
    FOR EACH ROW
    EXECUTE FUNCTION public.validate_share_with();

  INSERT INTO _ac_tg_validate_share_with VALUES (ARRAY['public_reader'::text]);
END;
$do$;
$test$,
  'public.validate_share_with() smoke'
);

-- @covers spatial.sync_raster_reference_cell_size()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_sync_rr_cell_size (
    reference_id integer,
    rast spatial.raster
  );
  CREATE TRIGGER _ac_trg
    AFTER INSERT ON _ac_tg_sync_rr_cell_size
    REFERENCING NEW TABLE AS new_rows
    FOR EACH STATEMENT
    EXECUTE FUNCTION spatial.sync_raster_reference_cell_size();

  INSERT INTO _ac_tg_sync_rr_cell_size (reference_id) VALUES (NULL);
END;
$do$;
$test$,
  'spatial.sync_raster_reference_cell_size() smoke'
);

-- @covers spatial.sync_raster_reference_cell_size_deg()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_sync_rr_cell_size_deg (
    reference_id integer,
    rast spatial.raster
  );
  CREATE TRIGGER _ac_trg
    AFTER INSERT ON _ac_tg_sync_rr_cell_size_deg
    REFERENCING NEW TABLE AS new_rows
    FOR EACH STATEMENT
    EXECUTE FUNCTION spatial.sync_raster_reference_cell_size_deg();

  INSERT INTO _ac_tg_sync_rr_cell_size_deg (reference_id) VALUES (NULL);
END;
$do$;
$test$,
  'spatial.sync_raster_reference_cell_size_deg() smoke'
);

-- @covers spatial.sync_rr_cell_size_deg_ins()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_sync_rr_ins (
    reference_id integer,
    rast spatial.raster
  );
  CREATE TRIGGER _ac_trg
    AFTER INSERT ON _ac_tg_sync_rr_ins
    REFERENCING NEW TABLE AS new_rows
    FOR EACH STATEMENT
    EXECUTE FUNCTION spatial.sync_rr_cell_size_deg_ins();

  INSERT INTO _ac_tg_sync_rr_ins (reference_id) VALUES (NULL);
END;
$do$;
$test$,
  'spatial.sync_rr_cell_size_deg_ins() smoke'
);

-- @covers spatial.sync_rr_cell_size_deg_upd()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_sync_rr_upd (
    reference_id integer,
    rast spatial.raster
  );
  CREATE TRIGGER _ac_trg
    AFTER UPDATE ON _ac_tg_sync_rr_upd
    REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
    FOR EACH STATEMENT
    EXECUTE FUNCTION spatial.sync_rr_cell_size_deg_upd();

  INSERT INTO _ac_tg_sync_rr_upd (reference_id) VALUES (NULL);
  UPDATE _ac_tg_sync_rr_upd SET reference_id = NULL;
END;
$do$;
$test$,
  'spatial.sync_rr_cell_size_deg_upd() smoke'
);

-- @covers spatial.update_geom_type()
-- @statement
SELECT lives_ok(
$test$
DO $do$
BEGIN
  CREATE TEMP TABLE _ac_tg_update_geom_type (
    geom geometry,
    geom_type text
  );
  CREATE TRIGGER _ac_trg
    BEFORE INSERT OR UPDATE ON _ac_tg_update_geom_type
    FOR EACH ROW
    EXECUTE FUNCTION spatial.update_geom_type();

  INSERT INTO _ac_tg_update_geom_type (geom, geom_type)
  VALUES (ST_GeomFromText('POINT(0 0)', 4326), NULL);
END;
$do$;
$test$,
  'spatial.update_geom_type() smoke'
);

-- @statement
SELECT * FROM finish();
