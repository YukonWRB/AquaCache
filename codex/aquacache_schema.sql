--
-- PostgreSQL database dump
--

-- Dumped from database version 17.2
-- Dumped by pg_dump version 17.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: application; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA application;


--
-- Name: SCHEMA application; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA application IS 'Schema to hold application-related data, such as text and images which need frequent updates, metrics such as number of viewers, plots generated, etc.';


--
-- Name: continuous; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA continuous;


--
-- Name: SCHEMA continuous; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA continuous IS 'Schema to hold continuous data and associated metadata.';


--
-- Name: discrete; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA discrete;


--
-- Name: SCHEMA discrete; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA discrete IS 'Schema to hold discrete data and associated metadata.';


--
-- Name: files; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA files;


--
-- Name: SCHEMA files; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA files IS 'Schema to hold files and associated metadata.';


--
-- Name: information; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA information;


--
-- Name: instruments; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA instruments;


--
-- Name: spatial; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA spatial;


--
-- Name: btree_gist; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS btree_gist WITH SCHEMA public;


--
-- Name: EXTENSION btree_gist; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION btree_gist IS 'support for indexing common datatypes in GiST';


--
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA spatial;


--
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry and geography spatial types and functions';


--
-- Name: postgis_raster; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis_raster WITH SCHEMA spatial;


--
-- Name: EXTENSION postgis_raster; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION postgis_raster IS 'PostGIS raster types and functions';


--
-- Name: apply_corrections(integer, timestamp with time zone, numeric); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.apply_corrections(p_timeseries_id integer, p_datetime timestamp with time zone, p_value numeric) RETURNS numeric
    LANGUAGE plpgsql
    AS $$
DECLARE
  corrected_value NUMERIC := p_value;
  correction_row RECORD;
  time_since_start NUMERIC;
  time_window NUMERIC;
  rate NUMERIC;
  correction NUMERIC;
BEGIN
  IF p_value IS NULL THEN
    RETURN NULL;
    END IF;
    
  FOR correction_row IN
    SELECT
      c.value1 AS c_value1,
      c.value2 AS c_value2,
      c.timestep_window AS c_timestep_window,
      c.equation AS c_equation,
      c.start_dt,
      c.end_dt,
      ct.correction_type,
      ct.priority
    FROM corrections c
    JOIN correction_types ct ON c.correction_type = ct.correction_type_id
    WHERE c.timeseries_id = p_timeseries_id
      AND c.start_dt <= p_datetime
      AND c.end_dt >= p_datetime
    ORDER BY ct.priority ASC
  LOOP
    -- Apply correction based on correction_row.correction_type
    IF correction_row.correction_type = 'delete' THEN
      -- Remove the data point
      RETURN NULL;
      
    ELSIF correction_row.correction_type = 'trim' THEN
      -- Remove data points outside of a specified value range
      IF correction_row.c_value1 IS NOT NULL AND corrected_value < correction_row.c_value1 THEN
        RETURN NULL;
      ELSIF correction_row.c_value2 IS NOT NULL AND corrected_value > correction_row.c_value2 THEN
        RETURN NULL;
      END IF;
      
    ELSIF correction_row.correction_type = 'offset linear' THEN
      -- Apply linear offset
      corrected_value := corrected_value + correction_row.c_value1;
      
    ELSIF correction_row.correction_type = 'offset two-point' THEN
      -- Apply two-point offset correction
      RAISE EXCEPTION 'Offset two-point correction not implemented yet';
      
    ELSIF correction_row.correction_type = 'scale' THEN
      -- Apply percent scaling
      corrected_value := corrected_value * (correction_row.c_value1 / 100.0);
      
    ELSIF correction_row.correction_type = 'drift linear' THEN
      -- Apply linear drift correction that continues until end_dt
      IF p_datetime < correction_row.start_dt OR p_datetime > correction_row.end_dt THEN
        -- No correction applied outside the correction period
        correction := 0;
      ELSE
        -- During the correction period; apply proportional correction
        time_since_start := EXTRACT(EPOCH FROM (p_datetime - correction_row.start_dt));
        time_window := EXTRACT(EPOCH FROM correction_row.c_timestep_window);
        rate := correction_row.c_value1 / time_window;  -- Correction per second
        correction := rate * time_since_start;
      END IF;
      corrected_value := corrected_value + correction;
      
    ELSIF correction_row.correction_type = 'drift equation' THEN
      -- Apply drift correction based on an equation
      RAISE EXCEPTION 'Drift equation correction not implemented yet';
    ELSE
      RAISE NOTICE 'Correction type % not handled', correction_row.correction_type;
    END IF;
  END LOOP;

  RETURN corrected_value;
END;
$$;


--
-- Name: check_approvals_overlap(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.check_approvals_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
                BEGIN
                    IF EXISTS (
                        SELECT 1
                        FROM approvals
                        WHERE timeseries_id = NEW.timeseries_id
                        AND approval_id != NEW.approval_id  -- Exclude the current row in an UPDATE
                        AND (
                            (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                        )
                    ) THEN
                        RAISE EXCEPTION 'Approvals cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                    END IF;
                    RETURN NEW;
                END;
                $$;


--
-- Name: check_contributors_overlap(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.check_contributors_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
  IF EXISTS (
    SELECT 1
    FROM contributors
    WHERE timeseries_id = NEW.timeseries_id
    AND organization_id != NEW.organization_id  -- Exclude the current row in an UPDATE
    AND (
      (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
    )
  ) THEN
  RAISE EXCEPTION 'Contributors cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
  END IF;
  RETURN NEW;
  END;
  $$;


--
-- Name: check_grades_overlap(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.check_grades_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  IF EXISTS (
                      SELECT 1
                      FROM grades
                      WHERE timeseries_id = NEW.timeseries_id
                      AND grade_id != NEW.grade_id  -- Exclude the current row in an UPDATE
                      AND (
                          (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                      )
                  ) THEN
                      RAISE EXCEPTION 'Grades cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
                  END IF;
                  RETURN NEW;
              END;
              $$;


--
-- Name: check_owners_overlap(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.check_owners_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
  IF EXISTS (
    SELECT 1
    FROM owners
    WHERE timeseries_id = NEW.timeseries_id
    AND organization_id != NEW.organization_id  -- Exclude the current row in an UPDATE
    AND (
      (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
    )
  ) THEN
  RAISE EXCEPTION 'Owners cannot overlap in time for the same timeseries_id. Failed on: %', NEW.timeseries_id;
  END IF;
  RETURN NEW;
  END;
  $$;


--
-- Name: check_qualifiers_overlap(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.check_qualifiers_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
                                        BEGIN
                                            IF EXISTS (
                                                SELECT 1
                                                FROM qualifiers
                                                WHERE timeseries_id = NEW.timeseries_id
                                                AND qualifier_type_id = NEW.qualifier_type_id -- exclude qualifiers of different types
                                                AND qualifier_id IS DISTINCT FROM NEW.qualifier_id  -- Exclude the current row in an UPDATE
                                                AND (
                                                    (NEW.start_dt < end_dt AND NEW.end_dt > start_dt)
                                                )
                                            ) THEN
                                                RAISE EXCEPTION 'Qualifiers of type % cannot overlap in time for the same timeseries_id. Failed on: %', NEW.qualifier_type_id, NEW.timeseries_id;
                                            END IF;
                                            RETURN NEW;
                                        END;
                                        $$;


--
-- Name: delete_old_forecasts(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.delete_old_forecasts() RETURNS void
    LANGUAGE plpgsql
    AS $$ BEGIN
    DELETE FROM forecasts
    WHERE valid_datetime < NOW() - INTERVAL '2 weeks';
END;
$$;


--
-- Name: validate_corrections(); Type: FUNCTION; Schema: continuous; Owner: -
--

CREATE FUNCTION continuous.validate_corrections() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  v1_required BOOLEAN;
  v2_required BOOLEAN;
  timestep_required BOOLEAN;
  equation_required BOOLEAN;
BEGIN
  -- Fetch the correction type definition from correction_types
  SELECT value1, value2, timestep_window, equation
  INTO v1_required, v2_required, timestep_required, equation_required
  FROM correction_types
  WHERE correction_type_id = NEW.correction_type;

  -- Check value1
  IF v1_required AND NEW.value1 IS NULL THEN
    RAISE EXCEPTION 'value1 cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT v1_required AND NEW.value1 IS NOT NULL THEN
    RAISE EXCEPTION 'value1 must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  -- Check value2
  IF v2_required AND NEW.value2 IS NULL THEN
    RAISE EXCEPTION 'value2 cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT v2_required AND NEW.value2 IS NOT NULL THEN
    RAISE EXCEPTION 'value2 must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  -- Check timestep_window
  IF timestep_required AND NEW.timestep_window IS NULL THEN
    RAISE EXCEPTION 'timestep_window cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT timestep_required AND NEW.timestep_window IS NOT NULL THEN
    RAISE EXCEPTION 'timestep_window must be NULL for correction_type_id %', NEW.correction_type;
  END IF;
  
  -- Check equation
  IF equation_required AND NEW.equation IS NULL THEN
    RAISE EXCEPTION 'equation cannot be NULL for correction_type_id %', NEW.correction_type;
  ELSIF NOT equation_required AND NEW.equation IS NOT NULL THEN
    RAISE EXCEPTION 'equation must be NULL for correction_type_id %', NEW.correction_type;
  END IF;

  RETURN NEW;
END;
$$;


--
-- Name: check_sample_series_overlap(); Type: FUNCTION; Schema: discrete; Owner: -
--

CREATE FUNCTION discrete.check_sample_series_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Ensure synch_from is before synch_to if both are NOT NULL
    IF NEW.synch_from IS NOT NULL AND NEW.synch_to IS NOT NULL AND NEW.synch_from >= NEW.synch_to THEN
        RAISE EXCEPTION 'The start of the new synchronization period must be before the end of the new synchronization period.';
    END IF;

    -- Check for overlaps with existing records
    IF EXISTS (
        SELECT 1
        FROM discrete.sample_series
        WHERE location_id = NEW.location_id
          AND sub_location_id = NEW.sub_location_id
          AND sample_series_id != NEW.sample_series_id -- Exclude the current row in an UPDATE
          AND (
              -- Case 1: Both synch_from and synch_to are defined
              (NEW.synch_from IS NOT NULL AND NEW.synch_to IS NOT NULL
               AND (synch_from, synch_to) OVERLAPS (NEW.synch_from, NEW.synch_to))
              
              -- Case 2: Existing synch_from IS NULL, synch_to IS NOT NULL
              OR (synch_from IS NULL AND synch_to IS NOT NULL
                  AND NEW.synch_from IS NOT NULL AND NEW.synch_from < synch_to)
              
              -- Case 3: Existing synch_to IS NULL, synch_from IS NOT NULL
              OR (synch_to IS NULL AND synch_from IS NOT NULL
                  AND NEW.synch_to IS NOT NULL AND NEW.synch_to > synch_from)
              
              -- Case 4: Both synch_from and synch_to are NULL (unbounded time range)
              OR (NEW.synch_from IS NULL AND NEW.synch_to IS NULL
                  AND synch_from IS NULL AND synch_to IS NULL)
          )
    ) THEN
        RAISE EXCEPTION 'Time range overlap detected for location_id: %, sub_location_id: %', NEW.location_id, NEW.sub_location_id;
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: enforce_result_speciation(); Type: FUNCTION; Schema: discrete; Owner: -
--

CREATE FUNCTION discrete.enforce_result_speciation() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Check if the associated parameter_id requires result_speciation
    IF EXISTS (
        SELECT 1
        FROM parameters
        WHERE parameter_id = NEW.parameter_id
          AND result_speciation = TRUE
          AND NEW.result_speciation IS NULL
    ) THEN
        RAISE EXCEPTION 'Result speciation must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: enforce_sample_fraction(); Type: FUNCTION; Schema: discrete; Owner: -
--

CREATE FUNCTION discrete.enforce_sample_fraction() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Check if the associated parameter_id requires a sample fraction
    IF EXISTS (
        SELECT 1
        FROM parameters
        WHERE parameter_id = NEW.parameter_id
          AND sample_fraction = TRUE
          AND NEW.sample_fraction IS NULL
    ) THEN
        RAISE EXCEPTION 'Sample fraction must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: guideline_pb(integer); Type: FUNCTION; Schema: discrete; Owner: -
--

CREATE FUNCTION discrete.guideline_pb(x_id integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $$
    DECLARE
        Hard_T NUMERIC;
        Hard_D NUMERIC;
        Ca_T NUMERIC;
        Ca_D NUMERIC;
        Mg_T NUMERIC;
        Mg_D NUMERIC;
        H NUMERIC; -- hardness calculated w.r.t. specific guidleine

    BEGIN
        SELECT results.result INTO Hard_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 100 AND results.sample_fraction = 5;
        SELECT results.result INTO Hard_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 100 AND results.sample_fraction = 19;
        SELECT results.result INTO Ca_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1061 AND results.sample_fraction = 5;
        SELECT results.result INTO Ca_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1061 AND results.sample_fraction = 19;
        SELECT results.result INTO Mg_T FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1103 AND results.sample_fraction = 5;
        SELECT results.result INTO Mg_D FROM results WHERE results.sample_id = x_id AND results.parameter_id = 1103 AND results.sample_fraction = 19;

        IF (Ca_D IS NOT NULL AND Mg_D IS NOT NULL) AND (Ca_D * Mg_D > 0) THEN
            H := (2.497*Ca_D+4.118 *Mg_D);

        ELSIF (Hard_D IS NOT NULL) AND (Hard_D > 0) THEN
            H := Hard_D;
        ELSEIF (Ca_T IS NOT NULL AND Mg_T IS NOT NULL) AND (Ca_T * Mg_T > 0) THEN
            H := (2.497*Ca_T+4.118 *Mg_T);
        ELSEIF (Hard_T IS NOT NULL) AND (Hard_T > 0) THEN
            H := Hard_T;
        ELSE
            H := NULL;
        END IF;

        RETURN H;
    END;
    $$;


--
-- Name: validate_guideline_start_end(); Type: FUNCTION; Schema: discrete; Owner: -
--

CREATE FUNCTION discrete.validate_guideline_start_end() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.start_datetime IS NOT NULL AND NEW.end_datetime IS NOT NULL THEN
        IF NEW.start_datetime >= NEW.end_datetime THEN
            RAISE EXCEPTION 'start_datetime must be before end_datetime';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: validate_guideline_values(); Type: FUNCTION; Schema: discrete; Owner: -
--

CREATE FUNCTION discrete.validate_guideline_values() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.value IS NULL AND NEW.equation IS NULL THEN
        RAISE EXCEPTION 'Either value or equation must be provided';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: check_data_sharing_agreement(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.check_data_sharing_agreement() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
IF NEW.data_sharing_agreement_id IS NOT NULL THEN
-- Check if the referenced document is of the correct type
IF NOT EXISTS (
  SELECT 1
  FROM documents d
  JOIN document_types dt ON d.type = dt.document_type_id
  WHERE d.document_id = NEW.data_sharing_agreement_id
  AND dt.document_type_en = 'data sharing agreement'
) THEN
RAISE EXCEPTION 'Invalid document type: data_sharing_agreement must reference a document of type ''data sharing agreement''';
END IF;
END IF;
RETURN NEW;
END;
$$;


--
-- Name: check_location_images(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.check_location_images() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
                      BEGIN
                          IF NEW.location_images IS NOT NULL THEN
                              -- Loop through each element in the location_images array
                              FOR i IN 1..array_length(NEW.location_images, 1) LOOP
                                  -- Check if the image_id exists in the images table
                                  PERFORM 1
                                  FROM images
                                  WHERE image_id = NEW.location_images[i];
                                  
                                  -- If image_id is not found, raise an exception
                                  IF NOT FOUND THEN
                                      RAISE EXCEPTION 'Invalid image_id: %, for location_id: %', NEW.location_images[i], NEW.location_id;
                                  END IF;
                              END LOOP;
                          END IF;
                          RETURN NEW;
                      END;
                      $$;


--
-- Name: enforce_share_with_restriction(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.enforce_share_with_restriction() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- Skip check if img_meta_id is NULL
                  IF NEW.img_meta_id IS NULL THEN
                      RETURN NEW;
                  END IF;
                  
                  -- Check if images_index.share_with is NOT '{public_reader}'
                  IF NOT ('public_reader' = ANY(NEW.share_with)) THEN
                      -- Retrieve the corresponding share_with from images_index
                      PERFORM 1
                      FROM images_index
                      WHERE img_meta_id = NEW.img_meta_id
                        AND NOT ('public_reader' = ANY(share_with));
                        
                      -- If images_index.share_with is NOT {'public_reader'}, raise an exception
                      IF FOUND THEN
                          RAISE EXCEPTION 'images_index entry for img_meta_id % has a restrictive share_with, images.share_with cannot be {public_reader}', NEW.img_meta_id;
                      END IF;
                  END IF;
                  RETURN NEW;
              END;
              $$;


--
-- Name: update_document_flags_after_delete(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.update_document_flags_after_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
-- Check if there are still entries for the document_id with geom_type 'POINT'
IF NOT EXISTS (
  SELECT 1
  FROM documents_spatial ds
  JOIN vectors g ON ds.geom_id = g.geom_id
  WHERE ds.document_id = OLD.document_id AND g.geom_type IN ('ST_Point', 'ST_MultiPoint')
) THEN
UPDATE documents
SET has_points = FALSE
WHERE document_id = OLD.document_id;
END IF;

IF NOT EXISTS (
  SELECT 1
  FROM documents_spatial ds
  JOIN vectors g ON ds.geom_id = g.geom_id
  WHERE ds.document_id = OLD.document_id AND g.geom_type IN ('ST_LineString', 'ST_MultiLineString')
  ) THEN
UPDATE documents
SET has_points = FALSE
WHERE document_id = OLD.document_id;
END IF;

IF NOT EXISTS (
  SELECT 1
  FROM documents_spatial ds
  JOIN vectors g ON ds.geom_id = g.geom_id
  WHERE ds.document_id = OLD.document_id AND g.geom_type IN ('ST_Polygon', 'ST_MultiPolygon')
  ) THEN
UPDATE documents
SET has_polygons = FALSE
WHERE document_id = OLD.document_id;
END IF;


RETURN OLD;
END;
$$;


--
-- Name: update_document_flags_after_insert(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.update_document_flags_after_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
-- Check the geom_type for the inserted record
IF (SELECT geom_type FROM vectors WHERE geom_id = NEW.geom_id) IN ('ST_Point', 'ST_MultiPoint') THEN
UPDATE documents
SET has_points = TRUE
WHERE document_id = NEW.document_id;
END IF;
IF (SELECT geom_type FROM vectors WHERE geom_id = NEW.geom_id) IN ('ST_LineString', 'ST_MultiLineString') THEN
UPDATE documents
SET has_lines = TRUE
WHERE document_id = NEW.document_id;
END IF;
IF (SELECT geom_type FROM vectors WHERE geom_id = NEW.geom_id) IN ('ST_Polygon', 'ST_MultiPolygon') THEN
UPDATE documents
SET has_polygons = TRUE
WHERE document_id = NEW.document_id;
END IF;

RETURN NEW;
END;
$$;


--
-- Name: update_line_flag(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.update_line_flag() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
UPDATE documents
SET has_lines = TRUE
WHERE document_id = NEW.document_id;
RETURN NEW;
END;
$$;


--
-- Name: update_location_flag(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.update_location_flag() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
UPDATE documents
SET has_locations = TRUE
WHERE document_id = NEW.document_id;
RETURN NEW;
END;
$$;


--
-- Name: update_polygon_flag(); Type: FUNCTION; Schema: files; Owner: -
--

CREATE FUNCTION files.update_polygon_flag() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
UPDATE documents
SET has_polygons = TRUE
WHERE document_id = NEW.document_id;
RETURN NEW;
END;
$$;


--
-- Name: update_modify_datetime(); Type: FUNCTION; Schema: instruments; Owner: -
--

CREATE FUNCTION instruments.update_modify_datetime() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
      NEW.modify_datetime = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$;


--
-- Name: check_instrument_meta_overlap(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_instrument_meta_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  IF EXISTS (
                      SELECT 1
                      FROM locations_metadata_instruments
                      WHERE location_id = NEW.location_id
                      AND sub_location_id = NEW.sub_location_id
                      AND metadata_id != NEW.metadata_id  -- Exclude the current row in an UPDATE
                      AND (
                          NEW.start_dt < end_dt AND (NEW.end_dt IS NULL OR NEW.end_dt > start_dt)
                      )
                  ) THEN
                      RAISE EXCEPTION 'Time range overlap detected for location_id: %, sub_location_id: %', NEW.location_id, NEW.sub_location_id;
                  END IF;
                  RETURN NEW;
              END;
              $$;


--
-- Name: check_instruments_reference(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_instruments_reference() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Check if all instrument IDs in the instruments array exist in the instruments table
    IF EXISTS (
        SELECT 1
        FROM UNNEST(NEW.instruments) AS instrument_id
        WHERE NOT EXISTS (
            SELECT 1 FROM instruments WHERE instrument_id = instrument_id
        )
    ) THEN
        RAISE EXCEPTION 'Invalid Instrument ID: %', instrument_id;
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: check_location_exists(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_location_exists() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
      IF NOT EXISTS (SELECT 1 FROM locations WHERE location_id = NEW.location_id) THEN
        RAISE EXCEPTION 'location_id % does not exist in locations table', NEW.location_id;
      END IF;
      RETURN NEW;
    END;
    $$;


--
-- Name: check_locations_metadata_acquisition_instruments(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_locations_metadata_acquisition_instruments() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  IF NEW.instruments IS NOT NULL THEN
                      FOR i IN 1..array_length(NEW.instruments, 1) LOOP
                          IF NOT EXISTS (SELECT 1 FROM instruments.instruments WHERE instrument_id = NEW.instruments[i]) THEN
                              RAISE EXCEPTION 'Instrument ID % does not exist in table instruments.instruments.', NEW.instruments[i];
                          END IF;
                      END LOOP;
                  END IF;
                  RETURN NEW;
              END;
              $$;


--
-- Name: check_page_content_integrity(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.check_page_content_integrity() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NEW.content_type = 'text' THEN
    IF NOT EXISTS (
      SELECT 1 FROM application.text WHERE id = NEW.content_id
    ) THEN
      RAISE EXCEPTION 'Text id % does not exist', NEW.content_id;
    END IF;
  ELSIF NEW.content_type = 'image' THEN
    IF NOT EXISTS (
      SELECT 1 FROM application.images WHERE id = NEW.content_id
    ) THEN
      RAISE EXCEPTION 'Image id % does not exist', NEW.content_id;
    END IF;
  ELSE
    RAISE EXCEPTION 'Unknown content type: %', NEW.content_type;
  END IF;
  RETURN NEW;
END;
$$;


--
-- Name: enforce_maintenance_constraints(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.enforce_maintenance_constraints() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Check if at least one of maintenance_performed or maintenance_due is not NULL
    IF NEW.maintenance_performed IS NULL AND NEW.maintenance_due IS NULL THEN
        RAISE EXCEPTION 'Either maintenance_performed or maintenance_due must be provided.';
    END IF;

    -- If maintenance_due is not NULL, set maintenance_flag to TRUE
    IF NEW.maintenance_due IS NOT NULL THEN
        NEW.maintenance_flag := TRUE;
    END IF;

    -- If maintenance_performed is not NULL, ensure date_performed is also provided
    IF NEW.maintenance_performed IS NOT NULL AND NEW.date_performed IS NULL THEN
        RAISE EXCEPTION 'date_performed cannot be NULL when maintenance_performed is provided.';
    END IF;

    RETURN NEW;
END;
$$;


--
-- Name: fill_locations_metadata_access_missing(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_access_missing() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- Check that method, and notes, and health_safety are not NULL
                  IF NEW.method IS NULL AND NEW.notes IS NULL AND NEW.health_safety IS NULL THEN
                      RAISE EXCEPTION 'Method, notes, and health_safety cannot be NULL';
                  END IF;
                  
                  -- Step 1: If method is NULL, fill it with the previous value
                  IF NEW.method IS NULL THEN
                      SELECT method
                      INTO NEW.method
                      FROM locations_metadata_access
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 2: If notes is NULL, fill it with the previous value
                  IF NEW.notes IS NULL THEN
                      SELECT notes
                      INTO NEW.notes
                      FROM locations_metadata_access
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 3: If health_safety is NULL, fill it with the previous value
                  IF NEW.health_safety IS NULL THEN
                      SELECT health_safety
                      INTO NEW.health_safety
                      FROM locations_metadata_access
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 4: If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_access
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: fill_locations_metadata_acquisition_missing(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_acquisition_missing() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_acquisition
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: fill_locations_metadata_infrastructure_groundwater(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_infrastructure_groundwater() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- Check that end_datetime gets populated
                  UPDATE locations_metadata_infrastructure_groundwater
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: fill_locations_metadata_infrastructure_hydromet(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_infrastructure_hydromet() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- Check that end_datetime gets populated
                  UPDATE locations_metadata_infrastructure_hydromet
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: fill_locations_metadata_infrastructure_missing(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_infrastructure_missing() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- Check that both site_description and infrastructure_description are not NULL
                  IF NEW.site_description IS NULL AND NEW.infrastructure_description IS NULL THEN
                      RAISE EXCEPTION 'Both site_description and infrastructure_description cannot be NULL';
                  END IF;
                  
                  -- Step 1: If site_description is NULL, fill it with the previous value
                  IF NEW.site_description IS NULL THEN
                      SELECT site_description 
                      INTO NEW.site_description
                      FROM locations_metadata_infrastructure
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 2: If infrastructure_description is NULL, fill it with the previous value
                  IF NEW.infrastructure_description IS NULL THEN
                      SELECT infrastructure_description 
                      INTO NEW.infrastructure_description
                      FROM locations_metadata_infrastructure
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 3: If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_infrastructure
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: fill_locations_metadata_owners_operators_missing(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_owners_operators_missing() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- Check that both owner and operator are not NULL
                  IF NEW.owner IS NULL AND NEW.operator IS NULL THEN
                      RAISE EXCEPTION 'Both owner and operator cannot be NULL';
                  END IF;
                  
                  -- Step 1: If owner is NULL, fill it with the previous value
                  IF NEW.owner IS NULL THEN
                      SELECT owner
                      INTO NEW.owner
                      FROM locations_metadata_owners_operators
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 2: If operator is NULL, fill it with the previous value
                  IF NEW.operator IS NULL THEN
                      SELECT operator
                      INTO NEW.operator
                      FROM locations_metadata_owners_operators
                      WHERE location_id = NEW.location_id
                      ORDER BY start_datetime DESC
                      LIMIT 1;
                  END IF;
                  
                  -- Step 3: If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_owners_operators
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: fill_locations_metadata_transmission_missing(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.fill_locations_metadata_transmission_missing() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
              BEGIN
                  -- If a previous entry exists without an end_datetime, update its end_datetime
                  UPDATE locations_metadata_transmission
                  SET end_datetime = NEW.start_datetime
                  WHERE location_id = NEW.location_id
                    AND end_datetime IS NULL;
                    
                  RETURN NEW;
              END;
              $$;


--
-- Name: get_csw_layer(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_csw_layer() RETURNS TABLE(location text, station_name text, latitude numeric, longitude numeric, type text, owner_name text, timeseries_id integer, parameter_id integer, param_name text, date date, value numeric, percent_historic_range numeric, mean numeric, min numeric, max numeric, drainage_area_km2 numeric)
    LANGUAGE plpgsql
    AS $$
  BEGIN
RETURN QUERY
SELECT 
l.location, 
CASE 
WHEN sl.sub_location_name IS NOT NULL 
THEN CONCAT(l.name, ' - ', sl.sub_location_name) 
ELSE l.name 
END AS station_name, 
l.latitude, 
l.longitude, 
lt.type, 
o.name AS owner_name,
t.timeseries_id, 
t.parameter_id, 
p.param_name, 
mcdc.date, 
mcdc.value, 
mcdc.percent_historic_range, 
mcdc.mean, 
mcdc.min, 
mcdc.max,
d.drainage_area_km2
FROM 
public.locations l
INNER JOIN 
public.location_types lt ON l.location_type = lt.type_id
INNER JOIN 
continuous.timeseries t ON t.location = l.location
FULL JOIN 
public.sub_locations sl ON sl.sub_location_id = t.sub_location_id 
INNER JOIN 
public.parameters p ON p.parameter_id = t.parameter_id 
INNER JOIN 
continuous.measurements_calculated_daily_corrected mcdc ON mcdc.timeseries_id = t.timeseries_id
FULL JOIN 
public.locations_metadata_owners_operators lmoo ON lmoo.location_id = l.location_id 
FULL JOIN 
public.organizations o ON o.organization_id = lmoo.owner
LEFT JOIN (
  SELECT DISTINCT feature_name, (ST_Area(geom::geography)/1000000)::numeric AS drainage_area_km2
  FROM vectors 
  WHERE layer_name = 'Drainage basins'
) d ON d.feature_name = l.location
WHERE 
(lt.type_id = 1 OR lt.type_id = 2)
AND l.jurisdictional_relevance IS TRUE
AND (p.parameter_id = 1150 OR p.parameter_id = 1165)
AND mcdc.date >= NOW() - INTERVAL '30 days';
END;
$$;


--
-- Name: get_roles_with_select_on_locations(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.get_roles_with_select_on_locations() RETURNS TABLE(role_name text)
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
BEGIN
  RETURN QUERY
  SELECT grantee::text
  FROM information_schema.role_table_grants
  WHERE table_name = 'locations'
    AND privilege_type = 'SELECT'
    AND grantee::text NOT IN (
      SELECT rolname
      FROM pg_roles
      WHERE rolbypassrls = TRUE
    );
END;
$$;


--
-- Name: update_created_modified(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_created_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    NEW.created_modified = CURRENT_TIMESTAMP;
    RETURN NEW;
  END;
  $$;


--
-- Name: update_modified(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
      NEW.modified = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$;


--
-- Name: update_updated(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.update_updated() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
               BEGIN
               NEW.updated = NOW();
               RETURN NEW;
               END;
               $$;


--
-- Name: user_in_group(text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.user_in_group(group_name text) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
                      SELECT pg_has_role(current_user, group_name, 'MEMBER');
                    $$;


--
-- Name: user_modified(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.user_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
      IF TG_OP = 'UPDATE' THEN
        NEW.modified_by := current_user;
      END IF;
      RETURN NEW;
    END;
    $$;


--
-- Name: validate_documents_array(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.validate_documents_array() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
        BEGIN
        -- skip validation if the documents column is NULL
        IF NEW.documents IS NOT NULL THEN
            -- Check if every document ID in the array exists in the documents table
            IF EXISTS (
                SELECT 1
                FROM unnest(NEW.documents) AS doc_id
                LEFT JOIN documents ON documents.document_id = doc_id
                WHERE documents.document_id IS NULL
            ) THEN
                RAISE EXCEPTION 'One or more document IDs in the documents array do not exist in the documents table.';
            END IF;
        END IF;
        
            RETURN NEW;
        END;
        $$;


--
-- Name: validate_guideline_start_end(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.validate_guideline_start_end() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.start_datetime IS NOT NULL AND NEW.end_datetime IS NOT NULL THEN
        IF NEW.start_datetime >= NEW.end_datetime THEN
            RAISE EXCEPTION 'start_datetime must be before end_datetime';
        END IF;
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: validate_guideline_values(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.validate_guideline_values() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.value IS NULL AND NEW.equation IS NULL THEN
        RAISE EXCEPTION 'Either value or equation must be provided';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: validate_share_with(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.validate_share_with() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM unnest(NEW.share_with) AS group_name
        WHERE NOT EXISTS (
            SELECT 1
            FROM pg_roles
            WHERE rolname = group_name
        )
    ) THEN
        RAISE EXCEPTION 'Invalid group_name in share_with array';
    END IF;
    IF 'public_reader' = ANY(NEW.share_with) AND array_length(NEW.share_with, 1) > 1 THEN
        RAISE EXCEPTION 'If public_reader is present in the share_with array, it must be the only value';
    END IF;
    RETURN NEW;
END;
$$;


--
-- Name: update_geom_type(); Type: FUNCTION; Schema: spatial; Owner: -
--

CREATE FUNCTION spatial.update_geom_type() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
NEW.geom_type := ST_GeometryType(NEW.geom);
RETURN NEW;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: images; Type: TABLE; Schema: application; Owner: -
--

CREATE TABLE application.images (
    id text NOT NULL,
    image bytea NOT NULL,
    format text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: page_content; Type: TABLE; Schema: application; Owner: -
--

CREATE TABLE application.page_content (
    page text NOT NULL,
    "position" integer NOT NULL,
    content_type text NOT NULL,
    content_id text NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    CONSTRAINT page_content_content_type_check CHECK ((content_type = ANY (ARRAY['text'::text, 'image'::text])))
);


--
-- Name: TABLE page_content; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON TABLE application.page_content IS 'Table to hold the content for each page, with the position of each element. This is used to order the text and images on each page.';


--
-- Name: COLUMN page_content.page; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON COLUMN application.page_content.page IS 'The page to which the content belongs.';


--
-- Name: COLUMN page_content."position"; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON COLUMN application.page_content."position" IS 'The position of the content on the page.';


--
-- Name: COLUMN page_content.content_type; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON COLUMN application.page_content.content_type IS 'The type of content, either text or image.';


--
-- Name: COLUMN page_content.content_id; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON COLUMN application.page_content.content_id IS 'The id of the content, either the text id or the image id.';


--
-- Name: text; Type: TABLE; Schema: application; Owner: -
--

CREATE TABLE application.text (
    id text NOT NULL,
    text_en text NOT NULL,
    text_fr text,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE text; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON TABLE application.text IS 'Table to hold frequently changed text for the application, such as news, descriptions, etc. Text which is more static is instead stored in the R package files.';


--
-- Name: COLUMN text.id; Type: COMMENT; Schema: application; Owner: -
--

COMMENT ON COLUMN application.text.id IS 'Unique identifier for the text; this is referenced in the application to select the correct entry.';


--
-- Name: aggregation_types; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.aggregation_types (
    aggregation_type_id integer NOT NULL,
    aggregation_type text NOT NULL,
    aggregation_type_fr text NOT NULL,
    description text,
    description_fr text
);


--
-- Name: aggregation_types_aggregation_type_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.aggregation_types_aggregation_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: aggregation_types_aggregation_type_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.aggregation_types_aggregation_type_id_seq OWNED BY continuous.aggregation_types.aggregation_type_id;


--
-- Name: approvals; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.approvals (
    approval_id integer NOT NULL,
    timeseries_id integer NOT NULL,
    approval_type_id integer NOT NULL,
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: approvals_approval_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.approvals_approval_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: approvals_approval_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.approvals_approval_id_seq OWNED BY continuous.approvals.approval_id;


--
-- Name: contributors; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.contributors (
    contributor_id integer NOT NULL,
    timeseries_id integer NOT NULL,
    organization_id integer NOT NULL,
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: contributors_contributor_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.contributors_contributor_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: contributors_contributor_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.contributors_contributor_id_seq OWNED BY continuous.contributors.contributor_id;


--
-- Name: corrections; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.corrections (
    correction_id integer NOT NULL,
    timeseries_id integer NOT NULL,
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone NOT NULL,
    correction_type integer NOT NULL,
    value1 numeric,
    value2 numeric,
    timestep_window interval,
    equation text,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE corrections; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.corrections IS 'Table to hold corrections for timeseries data. Each correction applies to a timeseries for a specified datetime range; overlapping corrections are applied in order of priority as defined in table correction_types.';


--
-- Name: corrections_correction_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.corrections_correction_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: corrections_correction_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.corrections_correction_id_seq OWNED BY continuous.corrections.correction_id;


--
-- Name: extrema; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.extrema (
    timeseries_id integer NOT NULL,
    agency text NOT NULL,
    year numeric NOT NULL,
    date date NOT NULL,
    value numeric NOT NULL,
    period_type text NOT NULL,
    condition text NOT NULL,
    extrema text NOT NULL,
    uncertainty text,
    notes text,
    deemed_primary boolean NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    CONSTRAINT peaks_condition_check CHECK ((condition = ANY (ARRAY['open water'::text, 'break-up'::text, 'freeze-up'::text, 'winter'::text]))),
    CONSTRAINT peaks_extrema_check CHECK ((extrema = ANY (ARRAY['minimum'::text, 'maximum'::text]))),
    CONSTRAINT peaks_period_type_check CHECK ((period_type = ANY (ARRAY['instantaneous'::text, '1-day'::text, '2-day'::text, '3-day'::text, '4-day'::text, '5-day'::text, '6-day'::text, '7-day'::text, 'monthly'::text])))
);


--
-- Name: TABLE extrema; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.extrema IS 'Holds vetted information about extrema specific to each time-series. Can be used for calculating return periods. Entries unique on timeseries_id, agency, year, period_type, condition, extrema, which allows for multiple different types of extrema from different authorities (agencies) for each timeseries.';


--
-- Name: COLUMN extrema.agency; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.agency IS 'The agency (authority) which calculated the extreme value. Ex: Water Resources Branch, Water Survey of Canada, Tetra Tech, etc.';


--
-- Name: COLUMN extrema.year; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.year IS 'The year for which each value is valid.';


--
-- Name: COLUMN extrema.date; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.date IS 'The exact date on which the extreme value occured.';


--
-- Name: COLUMN extrema.period_type; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.period_type IS 'One of instantaneous, 1-day, 2-day, 3-day, 4-day, 5-day, 6-day, 7-day, monthly, yearly. For example, a 1-day max flow is the maximum 1-day averaged flow for the year; instantaneous max flow is the greatest value single data point recorded for the year.';


--
-- Name: COLUMN extrema.condition; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.condition IS 'One of open water, break-up, freeze-up, winter. Any given timeseries can have one value of each for each year.';


--
-- Name: COLUMN extrema.extrema; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.extrema IS 'One of minimum or maximum. Necessary along with other descriptive columns to fully describe what each value represents.';


--
-- Name: COLUMN extrema.deemed_primary; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.extrema.deemed_primary IS 'If TRUE then the extrema value is the best (most reliable) value and should be used for most calculations and representations.';


--
-- Name: forecasts; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.forecasts (
    timeseries_id integer NOT NULL,
    issue_datetime timestamp with time zone,
    datetime timestamp with time zone NOT NULL,
    value numeric,
    min numeric,
    q10 numeric,
    q25 numeric,
    q50 numeric,
    q75 numeric,
    q90 numeric,
    max numeric
);


--
-- Name: TABLE forecasts; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.forecasts IS 'Holds forecast timeseries information. Each timeseries must match up with a timeseries_id from the timeseries table. Quantiles are optional. Data should be deleted after a certain time interval to prevent unecessarily burdening the database.';


--
-- Name: COLUMN forecasts.issue_datetime; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.forecasts.issue_datetime IS 'The datetime for which the forecast data point (row) is valid.';


--
-- Name: grades; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.grades (
    grade_id integer NOT NULL,
    timeseries_id integer NOT NULL,
    grade_type_id integer NOT NULL,
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: grades_grade_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.grades_grade_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: grades_grade_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.grades_grade_id_seq OWNED BY continuous.grades.grade_id;


--
-- Name: measurements_calculated_daily; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.measurements_calculated_daily (
    timeseries_id integer NOT NULL,
    date date NOT NULL,
    value numeric,
    imputed boolean DEFAULT false NOT NULL,
    percent_historic_range numeric,
    max numeric,
    min numeric,
    q90 numeric,
    q75 numeric,
    q50 numeric,
    q25 numeric,
    q10 numeric,
    mean numeric,
    doy_count integer,
    no_update boolean DEFAULT false NOT NULL,
    created_modified timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE measurements_calculated_daily; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.measurements_calculated_daily IS 'Stores calculated daily mean values for timeseries present in table measurements_continuous. Values should not be entered or modified manually but instead are calculated by the HydroMetDB package function calculate_stats.';


--
-- Name: COLUMN measurements_calculated_daily.imputed; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.imputed IS 'TRUE in this column means that at least one of the measurements used for the daily mean calculation was imputed, or, for daily means provided solely in the HYDAT database, that a value was imputed directly to this table.';


--
-- Name: COLUMN measurements_calculated_daily.percent_historic_range; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.percent_historic_range IS 'The percent of historical range for that measurement compared to all previous records for the same day of year (not including the current measurement). Only populated once a minimum of three values exist for the current day of year (including the current value). February 29 values are the mean of February 28 and March 1.

For example, a value equal to the maximum historic value is equal to 100% of historical range, while one at the miniumu value is 0%. Values above or below the historical range can have values of less than 0 or greater than 100.

The formula used for the calculation is ((current - min) / (max - min)) * 100';


--
-- Name: COLUMN measurements_calculated_daily.max; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.max IS 'Historical max for the day of year, excluding current measurement.';


--
-- Name: COLUMN measurements_calculated_daily.min; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.min IS 'Historical min for the day of year, excluding current measurement.';


--
-- Name: COLUMN measurements_calculated_daily.q50; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.q50 IS 'Historical 50th quantile or median, excluding current measurement.';


--
-- Name: COLUMN measurements_calculated_daily.doy_count; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.doy_count IS 'Number of measurements existing in the calculated_daily table for each day including historic and current measurement.';


--
-- Name: timeseries; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.timeseries (
    timeseries_id integer NOT NULL,
    location text NOT NULL,
    parameter_id integer NOT NULL,
    media_id integer NOT NULL,
    aggregation_type_id integer NOT NULL,
    start_datetime timestamp with time zone,
    end_datetime timestamp with time zone,
    last_new_data timestamp with time zone,
    last_daily_calculation timestamp with time zone,
    last_synchronize timestamp with time zone,
    source_fx text,
    source_fx_args jsonb,
    note text,
    record_rate interval,
    location_id integer NOT NULL,
    z numeric,
    active boolean DEFAULT true NOT NULL,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    sensor_priority integer DEFAULT 1,
    sub_location_id integer,
    default_owner integer
);

ALTER TABLE ONLY continuous.timeseries FORCE ROW LEVEL SECURITY;


--
-- Name: TABLE timeseries; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.timeseries IS 'Provides a record of every continuous-type timeseries in the database. Each timeseries is unique by its combination of location, sub_location, parameter, media, period_type, record_rate, sensor priority, and z (elevation or depth). Continuous data is data gathered at regular and usually frequent intervals by automatic means. Refer to table sample_series for lab and manual measurement.';


--
-- Name: COLUMN timeseries.timeseries_id; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.timeseries_id IS 'Autoincrements each time a timeseries is added. NOTE that timeseries should only be added using the R function addACTimeseries from the package AquaCache to ensure correctness of the data.';


--
-- Name: COLUMN timeseries.location; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.location IS 'Matches to the locations table.';


--
-- Name: COLUMN timeseries.aggregation_type_id; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.aggregation_type_id IS 'Descriptor for the type of aggregation applicable to this timeseries. Must match an entry to table aggregation_types, for example one of instantaneous, sum, mean, median, min, max, or (min+max)/2.';


--
-- Name: COLUMN timeseries.start_datetime; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.start_datetime IS 'First data point for the timeseries.';


--
-- Name: COLUMN timeseries.end_datetime; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.end_datetime IS 'Last data point for the timeseries.';


--
-- Name: COLUMN timeseries.last_new_data; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.last_new_data IS 'Time at which data was last appended to the timeseries';


--
-- Name: COLUMN timeseries.last_daily_calculation; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.last_daily_calculation IS 'Time at which daily means were calculated using function calculate_stats. Not used for discrete timeseries.';


--
-- Name: COLUMN timeseries.last_synchronize; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.last_synchronize IS 'Time at which the timeseries was cross-checked against values held by the remote or partner database; the local store should have been updated to reflect the remote.';


--
-- Name: COLUMN timeseries.source_fx; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.source_fx IS 'The function used to download and pre-process data for addition to a timeseries. Must be a function present in the R package AquaCache.';


--
-- Name: COLUMN timeseries.source_fx_args; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.source_fx_args IS 'Arguments to pass to the source_fx in a JSON format. Used for example to specify the url from which to fetch data or the location and parameter.';


--
-- Name: COLUMN timeseries.record_rate; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.record_rate IS 'The general recording interval for the timeseries, only used to differentiate between sub-daily, daily, weekly, or monthly recording. A more refined measure of the periodicity of the data is recorded in the measurements_continuous table.';


--
-- Name: COLUMN timeseries.location_id; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.location_id IS 'Matches to the locations table.';


--
-- Name: COLUMN timeseries.z; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.z IS 'Elevation of the measurement station, in meters. Used for things like thermistor strings, wind towers, or forecast climate parameters at different heights. Z elevations should be taken in the context of the location''s assigned elevation and datum.';


--
-- Name: COLUMN timeseries.active; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.timeseries.active IS 'Defines if the timeseries should or should not be added to or back-corrected by various AquaCache package functions.';


--
-- Name: measurements_calculated_daily_corrected; Type: VIEW; Schema: continuous; Owner: -
--

CREATE VIEW continuous.measurements_calculated_daily_corrected AS
 SELECT mcd.timeseries_id,
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
    mcd.doy_count
   FROM (continuous.measurements_calculated_daily mcd
     JOIN continuous.timeseries ts ON ((mcd.timeseries_id = ts.timeseries_id)))
  WHERE (('public_reader'::text = ANY (ts.share_with)) OR (EXISTS ( SELECT 1
           FROM unnest(ts.share_with) role(role)
          WHERE (pg_has_role(CURRENT_USER, (role.role)::name, 'MEMBER'::text) AND ((role.role)::name IN ( SELECT pg_roles.rolname
                   FROM pg_roles))))));


--
-- Name: measurements_continuous; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.measurements_continuous (
    timeseries_id integer NOT NULL,
    datetime timestamp with time zone NOT NULL,
    value numeric NOT NULL,
    period interval,
    imputed boolean DEFAULT false NOT NULL,
    no_update boolean DEFAULT false NOT NULL,
    created_modified timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: TABLE measurements_continuous; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.measurements_continuous IS 'Stores observations and imputed values for continuous timeseries.';


--
-- Name: COLUMN measurements_continuous.period; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_continuous.period IS 'Greater than 0 for min, max, sum, mean types of measurements. The periodicity of data can change within a timeseries, for example if recording rates go from every 6 hours to hourly.';


--
-- Name: COLUMN measurements_continuous.imputed; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.measurements_continuous.imputed IS 'Imputed values may be user-entered. Imputed values are automatically replaced if/when a value becomes available on the remote data store';


--
-- Name: measurements_continuous_corrected; Type: VIEW; Schema: continuous; Owner: -
--

CREATE VIEW continuous.measurements_continuous_corrected AS
 SELECT mc.timeseries_id,
    mc.datetime,
    mc.value AS value_raw,
        CASE
            WHEN (EXISTS ( SELECT 1
               FROM continuous.corrections c
              WHERE ((c.timeseries_id = mc.timeseries_id) AND (c.start_dt <= mc.datetime) AND (c.end_dt >= mc.datetime)))) THEN continuous.apply_corrections(mc.timeseries_id, mc.datetime, mc.value)
            ELSE mc.value
        END AS value_corrected,
    mc.period,
    mc.imputed
   FROM (continuous.measurements_continuous mc
     JOIN continuous.timeseries ts ON ((mc.timeseries_id = ts.timeseries_id)))
  WHERE (('public_reader'::text = ANY (ts.share_with)) OR (EXISTS ( SELECT 1
           FROM unnest(ts.share_with) role(role)
          WHERE (pg_has_role(CURRENT_USER, (role.role)::name, 'MEMBER'::text) AND ((role.role)::name IN ( SELECT pg_roles.rolname
                   FROM pg_roles))))));


--
-- Name: measurements_hourly_corrected; Type: VIEW; Schema: continuous; Owner: -
--

CREATE VIEW continuous.measurements_hourly_corrected AS
 SELECT timeseries_id,
    date_trunc('hour'::text, datetime) AS datetime,
    avg(value_raw) AS value_raw,
    avg(value_corrected) AS value_corrected,
    bool_or(imputed) AS imputed
   FROM continuous.measurements_continuous_corrected mcc
  GROUP BY timeseries_id, (date_trunc('hour'::text, datetime))
  ORDER BY (date_trunc('hour'::text, datetime));


--
-- Name: owners; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.owners (
    owner_id integer NOT NULL,
    timeseries_id integer NOT NULL,
    organization_id integer NOT NULL,
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: owners_owner_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.owners_owner_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: owners_owner_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.owners_owner_id_seq OWNED BY continuous.owners.owner_id;


--
-- Name: qualifiers; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.qualifiers (
    qualifier_id integer NOT NULL,
    timeseries_id integer NOT NULL,
    qualifier_type_id integer NOT NULL,
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL,
    updated timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: qualifiers_qualifier_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.qualifiers_qualifier_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: qualifiers_qualifier_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.qualifiers_qualifier_id_seq OWNED BY continuous.qualifiers.qualifier_id;


--
-- Name: rating_curve_points; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.rating_curve_points (
    curve_point_id integer NOT NULL,
    curve_id integer NOT NULL,
    input_value numeric NOT NULL,
    output_value numeric NOT NULL
);


--
-- Name: rating_curve_points_curve_point_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.rating_curve_points_curve_point_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rating_curve_points_curve_point_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.rating_curve_points_curve_point_id_seq OWNED BY continuous.rating_curve_points.curve_point_id;


--
-- Name: rating_curve_shifts; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.rating_curve_shifts (
    curve_shift_id integer NOT NULL,
    curve_id integer NOT NULL,
    shift_start timestamp with time zone NOT NULL,
    shift_end timestamp with time zone NOT NULL,
    shift_value numeric NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modified timestamp with time zone
);


--
-- Name: rating_curve_shifts_curve_shift_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.rating_curve_shifts_curve_shift_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rating_curve_shifts_curve_shift_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.rating_curve_shifts_curve_shift_id_seq OWNED BY continuous.rating_curve_shifts.curve_shift_id;


--
-- Name: rating_curves; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.rating_curves (
    curve_id integer NOT NULL,
    location_id integer NOT NULL,
    input_parameter_id integer NOT NULL,
    output_parameter_id integer NOT NULL,
    valid_from timestamp with time zone,
    valid_to timestamp with time zone,
    offset_value numeric DEFAULT 0 NOT NULL,
    approval integer,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modified timestamp with time zone,
    description text,
    notes text
);


--
-- Name: rating_curves_curve_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.rating_curves_curve_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rating_curves_curve_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.rating_curves_curve_id_seq OWNED BY continuous.rating_curves.curve_id;


--
-- Name: thresholds; Type: TABLE; Schema: continuous; Owner: -
--

CREATE TABLE continuous.thresholds (
    timeseries_id integer NOT NULL,
    high_advisory numeric,
    high_watch numeric,
    high_warning numeric,
    flood_minor numeric,
    flood_major numeric,
    high_first_human_impacts numeric,
    low_advisory numeric,
    low_watch numeric,
    low_warning numeric,
    low_first_human_impacts numeric,
    low_aquatic_life_impacts_minor numeric,
    low_aquatic_life_impacts_major numeric,
    high_aquatic_life_impacts_minor numeric,
    high_aquatic_life_impacts_major numeric,
    fsl numeric,
    lsl numeric,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE thresholds; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON TABLE continuous.thresholds IS 'Holds threshold values for a variety of things like streamflows, water levels, flood levels, aquatic life inpacts.';


--
-- Name: COLUMN thresholds.high_advisory; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.high_advisory IS 'Value at which a high water advisory is to be issued.';


--
-- Name: COLUMN thresholds.high_watch; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.high_watch IS 'Value at which a high water watch is to be issued.';


--
-- Name: COLUMN thresholds.high_warning; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.high_warning IS 'Value at which a high water warning is to be issued.';


--
-- Name: COLUMN thresholds.flood_minor; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.flood_minor IS 'Value at which a minor flood is to be declared.';


--
-- Name: COLUMN thresholds.flood_major; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.flood_major IS 'Value at which a major flood is to be declared.';


--
-- Name: COLUMN thresholds.high_first_human_impacts; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.high_first_human_impacts IS 'High-side value at which first human impacts are known, such as impacts to navigation.';


--
-- Name: COLUMN thresholds.low_advisory; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.low_advisory IS 'Value at which a low water advisory is to be issued.';


--
-- Name: COLUMN thresholds.low_watch; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.low_watch IS 'Value at which a low water watch is to be issued.';


--
-- Name: COLUMN thresholds.low_warning; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.low_warning IS 'Value at which a low water warning is to be issued.';


--
-- Name: COLUMN thresholds.low_first_human_impacts; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.low_first_human_impacts IS 'Low-side value at which first human impacts are known, such as impacts to navigation.';


--
-- Name: COLUMN thresholds.low_aquatic_life_impacts_minor; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.low_aquatic_life_impacts_minor IS 'Low-side (water temp, level, flow) minor impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.low_aquatic_life_impacts_major; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.low_aquatic_life_impacts_major IS 'Low-side (water temp, level, flow) major impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.high_aquatic_life_impacts_minor; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.high_aquatic_life_impacts_minor IS 'High-side (water temp, level, flow) minor impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.high_aquatic_life_impacts_major; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.high_aquatic_life_impacts_major IS 'High-side (water temp, level, flow) major impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.fsl; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.fsl IS 'Full supply level (as per water use licence for control structure operations)';


--
-- Name: COLUMN thresholds.lsl; Type: COMMENT; Schema: continuous; Owner: -
--

COMMENT ON COLUMN continuous.thresholds.lsl IS 'Low supply level (as per water use licence for control structure operations)';


--
-- Name: locations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations (
    location text NOT NULL,
    name text NOT NULL,
    latitude numeric NOT NULL,
    longitude numeric NOT NULL,
    note text,
    contact text,
    location_id integer NOT NULL,
    geom_id integer NOT NULL,
    name_fr text,
    visibility_public text DEFAULT 'exact'::text NOT NULL,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    location_type integer,
    data_sharing_agreement_id integer,
    install_purpose text,
    current_purpose text,
    location_images integer[],
    jurisdictional_relevance boolean DEFAULT true,
    anthropogenic_influence boolean DEFAULT false,
    sentinel_location boolean DEFAULT false,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    CONSTRAINT locations_visibility_public_check CHECK ((visibility_public = ANY (ARRAY['exact'::text, 'region'::text, 'jitter'::text])))
);

ALTER TABLE ONLY public.locations FORCE ROW LEVEL SECURITY;


--
-- Name: COLUMN locations.data_sharing_agreement_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations.data_sharing_agreement_id IS 'An optional link to a data sharing agreement from the documents table. A check is enforced to ensure that this column only references documents of type ''data sharing agreement''.';


--
-- Name: COLUMN locations.install_purpose; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations.install_purpose IS 'Purpose of original installation.';


--
-- Name: COLUMN locations.current_purpose; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations.current_purpose IS 'Current purpose for location/station operation.';


--
-- Name: COLUMN locations.jurisdictional_relevance; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations.jurisdictional_relevance IS 'Whether the location is publicly relevant to the jursdiction operating this database. Can be used for filtering results from public-facing applications if desired without labelling the location or associated timeseries as non-public.';


--
-- Name: COLUMN locations.anthropogenic_influence; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations.anthropogenic_influence IS 'Flag to indicate if the location has anthropogenic influence';


--
-- Name: COLUMN locations.sentinel_location; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations.sentinel_location IS 'Flag to indicate if the location is a sentinel location for climate change analyses and reports';


--
-- Name: media_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.media_types (
    media_id integer NOT NULL,
    media_type text NOT NULL,
    media_type_fr text NOT NULL,
    description text,
    description_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: parameters; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.parameters (
    parameter_id integer NOT NULL,
    param_name text NOT NULL,
    param_name_fr text,
    description text,
    description_fr text,
    unit_default text NOT NULL,
    unit_solid text,
    cas_number text,
    result_speciation boolean NOT NULL,
    sample_fraction boolean NOT NULL,
    plot_default_y_orientation text NOT NULL,
    plot_default_floor numeric,
    plot_default_ceiling numeric,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    CONSTRAINT parameters_plot_default_y_orientation_check CHECK ((plot_default_y_orientation = ANY (ARRAY['normal'::text, 'inverted'::text])))
);


--
-- Name: TABLE parameters; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.parameters IS 'Holds information about each parameter, including the parameter name, description, unit, and default plotting orientation, ceiling, and floor to facilitate plotting. Parameters are associated with groups and sub-groups via the table parameter_relationships.';


--
-- Name: COLUMN parameters.unit_default; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.parameters.unit_default IS 'SI units only. For example, m3/s, mg/L, etc. Import functions should convert to SI units listed here before appending to the DB.';


--
-- Name: COLUMN parameters.result_speciation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.parameters.result_speciation IS 'Controls if results using this parameter require an entry to column result_speciation of table measurements_discrete';


--
-- Name: COLUMN parameters.sample_fraction; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.parameters.sample_fraction IS 'Controls if results using this parameter require an entry to column sample_fraction of table measurements_discrete';


--
-- Name: timeseries_metadata_en; Type: VIEW; Schema: continuous; Owner: -
--

CREATE VIEW continuous.timeseries_metadata_en WITH (security_invoker='true') AS
 SELECT ts.timeseries_id,
    mtypes.media_type,
    params.param_name AS parameter_name,
    params.unit_default AS default_units,
    params.unit_solid AS units_solid_medium,
    at.aggregation_type,
    ts.record_rate AS recording_rate,
    ts.start_datetime,
    ts.end_datetime,
    ts.note,
    loc.location_id,
    loc.location AS location_code,
    loc.name AS location_name,
    loc.latitude,
    loc.longitude
   FROM ((((continuous.timeseries ts
     JOIN public.locations loc ON ((ts.location_id = loc.location_id)))
     LEFT JOIN public.parameters params ON ((ts.parameter_id = params.parameter_id)))
     LEFT JOIN public.media_types mtypes ON ((ts.media_id = mtypes.media_id)))
     LEFT JOIN continuous.aggregation_types at ON ((ts.aggregation_type_id = at.aggregation_type_id)))
  ORDER BY ts.timeseries_id;


--
-- Name: timeseries_metadata_fr; Type: VIEW; Schema: continuous; Owner: -
--

CREATE VIEW continuous.timeseries_metadata_fr WITH (security_invoker='true') AS
 SELECT ts.timeseries_id,
    mtypes.media_type_fr AS "type_de_média",
    params.param_name_fr AS "nom_paramètre",
    params.unit_default AS "unités_par_défaut",
    params.unit_solid AS "unités_media_solide",
    ag.aggregation_type_fr AS "type_agrégation",
    ts.record_rate AS "fréquence_enregistrement",
    ts.start_datetime AS "début",
    ts.end_datetime AS fin,
    ts.note,
    loc.location_id,
    loc.location AS location_code,
    loc.name_fr AS nom_endroit,
    loc.latitude,
    loc.longitude
   FROM ((((continuous.timeseries ts
     JOIN public.locations loc ON ((ts.location_id = loc.location_id)))
     LEFT JOIN public.parameters params ON ((ts.parameter_id = params.parameter_id)))
     LEFT JOIN public.media_types mtypes ON ((ts.media_id = mtypes.media_id)))
     LEFT JOIN continuous.aggregation_types ag ON ((ts.aggregation_type_id = ag.aggregation_type_id)))
  ORDER BY ts.timeseries_id;


--
-- Name: timeseries_timeseries_id_seq; Type: SEQUENCE; Schema: continuous; Owner: -
--

CREATE SEQUENCE continuous.timeseries_timeseries_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: timeseries_timeseries_id_seq; Type: SEQUENCE OWNED BY; Schema: continuous; Owner: -
--

ALTER SEQUENCE continuous.timeseries_timeseries_id_seq OWNED BY continuous.timeseries.timeseries_id;


--
-- Name: protocols_methods; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.protocols_methods (
    protocol_id integer NOT NULL,
    protocol_name text NOT NULL,
    protocol_description text,
    url text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: analysis_protocols_protocol_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.analysis_protocols_protocol_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: analysis_protocols_protocol_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.analysis_protocols_protocol_id_seq OWNED BY discrete.protocols_methods.protocol_id;


--
-- Name: collection_methods; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.collection_methods (
    collection_method_id integer NOT NULL,
    collection_method text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: collection_methods_collection_method_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.collection_methods_collection_method_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: collection_methods_collection_method_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.collection_methods_collection_method_id_seq OWNED BY discrete.collection_methods.collection_method_id;


--
-- Name: guideline_values; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.guideline_values (
    id integer NOT NULL,
    name text NOT NULL,
    name_fr text NOT NULL,
    value numeric,
    equation text,
    start_datetime timestamp with time zone,
    end_datetime timestamp with time zone
);


--
-- Name: guideline_values_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.guideline_values_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: guideline_values_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.guideline_values_id_seq OWNED BY discrete.guideline_values.id;


--
-- Name: laboratories; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.laboratories (
    lab_id integer NOT NULL,
    lab_name text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: laboratories_lab_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.laboratories_lab_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: laboratories_lab_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.laboratories_lab_id_seq OWNED BY discrete.laboratories.lab_id;


--
-- Name: result_conditions; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.result_conditions (
    result_condition_id integer NOT NULL,
    result_condition text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: result_conditions_result_condition_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.result_conditions_result_condition_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: result_conditions_result_condition_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.result_conditions_result_condition_id_seq OWNED BY discrete.result_conditions.result_condition_id;


--
-- Name: result_speciations; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.result_speciations (
    result_speciation_id integer NOT NULL,
    result_speciation text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: result_speciations_result_speciation_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.result_speciations_result_speciation_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: result_speciations_result_speciation_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.result_speciations_result_speciation_id_seq OWNED BY discrete.result_speciations.result_speciation_id;


--
-- Name: result_types; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.result_types (
    result_type_id integer NOT NULL,
    result_type text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: result_types_result_type_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.result_types_result_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: result_types_result_type_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.result_types_result_type_id_seq OWNED BY discrete.result_types.result_type_id;


--
-- Name: result_value_types; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.result_value_types (
    result_value_type_id integer NOT NULL,
    result_value_type text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: result_value_types_result_value_type_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.result_value_types_result_value_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: result_value_types_result_value_type_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.result_value_types_result_value_type_id_seq OWNED BY discrete.result_value_types.result_value_type_id;


--
-- Name: results; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.results (
    result_id integer NOT NULL,
    sample_id integer NOT NULL,
    result_type integer NOT NULL,
    parameter_id integer NOT NULL,
    sample_fraction integer,
    result numeric,
    result_condition integer,
    result_condition_value numeric,
    result_value_type integer,
    result_speciation integer,
    protocol_method integer,
    laboratory integer,
    analysis_datetime timestamp with time zone,
    share_with text[] DEFAULT '{public_reader}'::text[],
    no_update boolean DEFAULT false,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    CONSTRAINT chk_result_condition CHECK (((result_condition IS NULL) OR (result IS NULL))),
    CONSTRAINT chk_result_condition_value CHECK (((result_condition_value IS NULL) OR (result_condition = ANY (ARRAY[1, 2]))))
);


--
-- Name: results_result_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.results_result_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: results_result_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.results_result_id_seq OWNED BY discrete.results.result_id;


--
-- Name: sample_fractions; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.sample_fractions (
    sample_fraction_id integer NOT NULL,
    sample_fraction text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: sample_fractions_sample_fraction_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.sample_fractions_sample_fraction_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sample_fractions_sample_fraction_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.sample_fractions_sample_fraction_id_seq OWNED BY discrete.sample_fractions.sample_fraction_id;


--
-- Name: sample_series; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.sample_series (
    sample_series_id integer NOT NULL,
    location_id integer NOT NULL,
    sub_location_id integer,
    synch_from timestamp with time zone,
    synch_to timestamp with time zone,
    default_owner integer NOT NULL,
    default_contributor integer,
    last_new_data timestamp with time zone,
    last_synchronize timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    source_fx text NOT NULL,
    source_fx_args jsonb,
    note text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE sample_series; Type: COMMENT; Schema: discrete; Owner: -
--

COMMENT ON TABLE discrete.sample_series IS 'Provides a means of automating the import of discrete data (lab or field measurements) via the source_fx and optional arguments, designed to work with the AquaCache R package. Actual sample metadata is stored in the samples table, measurements in the results table.';


--
-- Name: sample_series_sample_series_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.sample_series_sample_series_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sample_series_sample_series_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.sample_series_sample_series_id_seq OWNED BY discrete.sample_series.sample_series_id;


--
-- Name: sample_types; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.sample_types (
    sample_type_id integer NOT NULL,
    sample_type text NOT NULL,
    sample_type_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: sample_types_sample_type_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.sample_types_sample_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sample_types_sample_type_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.sample_types_sample_type_id_seq OWNED BY discrete.sample_types.sample_type_id;


--
-- Name: samples; Type: TABLE; Schema: discrete; Owner: -
--

CREATE TABLE discrete.samples (
    sample_id integer NOT NULL,
    location_id integer NOT NULL,
    sub_location_id integer,
    media_id integer NOT NULL,
    z numeric,
    datetime timestamp with time zone NOT NULL,
    target_datetime timestamp with time zone,
    collection_method integer NOT NULL,
    sample_type integer NOT NULL,
    linked_with integer,
    sample_volume_ml numeric,
    purge_volume_l numeric,
    purge_time_min numeric,
    flow_rate_l_min numeric,
    wave_hgt_m numeric,
    sample_grade integer,
    sample_approval integer,
    sample_qualifier integer,
    owner integer NOT NULL,
    contributor integer,
    comissioning_org integer,
    sampling_org integer,
    documents integer[],
    share_with text[] DEFAULT '{public_reader}'::text[],
    import_source text,
    no_update boolean DEFAULT false,
    note text,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    import_source_id text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: samples_sample_id_seq; Type: SEQUENCE; Schema: discrete; Owner: -
--

CREATE SEQUENCE discrete.samples_sample_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: samples_sample_id_seq; Type: SEQUENCE OWNED BY; Schema: discrete; Owner: -
--

ALTER SEQUENCE discrete.samples_sample_id_seq OWNED BY discrete.samples.sample_id;


--
-- Name: document_types; Type: TABLE; Schema: files; Owner: -
--

CREATE TABLE files.document_types (
    document_type_id integer NOT NULL,
    document_type_en text NOT NULL,
    document_type_fr text NOT NULL,
    description_en text,
    description_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: document_types_document_type_id_seq; Type: SEQUENCE; Schema: files; Owner: -
--

CREATE SEQUENCE files.document_types_document_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: document_types_document_type_id_seq; Type: SEQUENCE OWNED BY; Schema: files; Owner: -
--

ALTER SEQUENCE files.document_types_document_type_id_seq OWNED BY files.document_types.document_type_id;


--
-- Name: documents; Type: TABLE; Schema: files; Owner: -
--

CREATE TABLE files.documents (
    document_id integer NOT NULL,
    name text NOT NULL,
    type integer NOT NULL,
    has_points boolean DEFAULT false NOT NULL,
    has_lines boolean DEFAULT false NOT NULL,
    has_polygons boolean DEFAULT false NOT NULL,
    authors text[],
    url text,
    publish_date date,
    description text NOT NULL,
    format text NOT NULL,
    document bytea NOT NULL,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    owner integer,
    contributor integer,
    tags text[],
    file_hash text GENERATED ALWAYS AS (md5(encode(document, 'hex'::text))) STORED,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);

ALTER TABLE ONLY files.documents FORCE ROW LEVEL SECURITY;


--
-- Name: TABLE documents; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON TABLE files.documents IS 'Holds documents and metadata associated with each document. Each document can be associated with one or more location, line, or polygon, or all three.';


--
-- Name: COLUMN documents.type; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.documents.type IS 'One of thesis, report, well log, conference paper, poster, journal article, map, graph, protocol, grading scheme, metadata, other';


--
-- Name: COLUMN documents.has_points; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.documents.has_points IS 'Flag to indicate that the document_spatial has a point entry for this document.';


--
-- Name: COLUMN documents.authors; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.documents.authors IS 'An *array* of one or more authors.';


--
-- Name: COLUMN documents.file_hash; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.documents.file_hash IS 'MD5 hash of the document file, used for ensuring that no two documents are identical.';


--
-- Name: documents_document_id_seq; Type: SEQUENCE; Schema: files; Owner: -
--

CREATE SEQUENCE files.documents_document_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: documents_document_id_seq; Type: SEQUENCE OWNED BY; Schema: files; Owner: -
--

ALTER SEQUENCE files.documents_document_id_seq OWNED BY files.documents.document_id;


--
-- Name: documents_spatial; Type: TABLE; Schema: files; Owner: -
--

CREATE TABLE files.documents_spatial (
    document_id integer NOT NULL,
    geom_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: image_series; Type: TABLE; Schema: files; Owner: -
--

CREATE TABLE files.image_series (
    img_meta_id integer NOT NULL,
    first_img timestamp with time zone,
    last_img timestamp with time zone,
    last_new_img timestamp with time zone,
    source_fx text,
    source_fx_args jsonb,
    description text,
    location_id integer NOT NULL,
    active boolean,
    visibility_public text DEFAULT 'exact'::text NOT NULL,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    owner integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    CONSTRAINT images_index_visibility_public_check CHECK ((visibility_public = ANY (ARRAY['exact'::text, 'region'::text, 'jitter'::text])))
);

ALTER TABLE ONLY files.image_series FORCE ROW LEVEL SECURITY;


--
-- Name: TABLE image_series; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON TABLE files.image_series IS 'Index for image series in images table. Each location at which there is an image series gets an entry here; images in table images are linked to this table using the img_meta_id.';


--
-- Name: COLUMN image_series.active; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.image_series.active IS 'Defines if the image series should or should not be imported.';


--
-- Name: image_types; Type: TABLE; Schema: files; Owner: -
--

CREATE TABLE files.image_types (
    image_type_id integer NOT NULL,
    image_type text NOT NULL,
    description text,
    default_tag_options text[],
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: image_types_image_type_id_seq; Type: SEQUENCE; Schema: files; Owner: -
--

CREATE SEQUENCE files.image_types_image_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: image_types_image_type_id_seq; Type: SEQUENCE OWNED BY; Schema: files; Owner: -
--

ALTER SEQUENCE files.image_types_image_type_id_seq OWNED BY files.image_types.image_type_id;


--
-- Name: images; Type: TABLE; Schema: files; Owner: -
--

CREATE TABLE files.images (
    image_id integer NOT NULL,
    img_meta_id integer,
    datetime timestamp with time zone NOT NULL,
    fetch_datetime timestamp with time zone,
    format text NOT NULL,
    file bytea NOT NULL,
    description text,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    owner integer,
    contributor integer,
    latitude numeric NOT NULL,
    longitude numeric NOT NULL,
    azimuth_true numeric,
    elevation_agl_m numeric,
    elevation_msl_m numeric,
    tags text[],
    location_id integer,
    file_hash text GENERATED ALWAYS AS (md5(encode(file, 'hex'::text))) STORED,
    image_type integer NOT NULL,
    create_datetime timestamp without time zone DEFAULT now(),
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);

ALTER TABLE ONLY files.images FORCE ROW LEVEL SECURITY;


--
-- Name: TABLE images; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON TABLE files.images IS 'Holds images of local conditions specific to each location. Originally designed to hold auto-captured images at WSC locations, but could be used for other location images such as setup documentation. For image series or images necessarily associated with a location, images are linked to the images_index table using the img_meta_id.';


--
-- Name: COLUMN images.img_meta_id; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.img_meta_id IS 'Keyed to table images_index to associate an image with other images taken at this location. May be NULL for images not part of a series.';


--
-- Name: COLUMN images.latitude; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.latitude IS 'If the image is not part of an image series (and doesnt have an entry in column img_meta_id) this column must be populated. Enforced via trigger+function.';


--
-- Name: COLUMN images.longitude; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.longitude IS 'If the image is not part of an image series (and doesnt have an entry in column img_meta_id) this column must be populated. Enforced via trigger+function.';


--
-- Name: COLUMN images.azimuth_true; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.azimuth_true IS 'Direction of the camera in degrees from true north.';


--
-- Name: COLUMN images.elevation_agl_m; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.elevation_agl_m IS 'Elevation in meters above ground level';


--
-- Name: COLUMN images.elevation_msl_m; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.elevation_msl_m IS 'Elevation in meters above mean sea level';


--
-- Name: COLUMN images.file_hash; Type: COMMENT; Schema: files; Owner: -
--

COMMENT ON COLUMN files.images.file_hash IS 'MD5 hash of the image file, used to ensure that no two images are identical';


--
-- Name: images_image_id_seq; Type: SEQUENCE; Schema: files; Owner: -
--

CREATE SEQUENCE files.images_image_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: images_image_id_seq; Type: SEQUENCE OWNED BY; Schema: files; Owner: -
--

ALTER SEQUENCE files.images_image_id_seq OWNED BY files.images.image_id;


--
-- Name: images_index_img_meta_id_seq; Type: SEQUENCE; Schema: files; Owner: -
--

CREATE SEQUENCE files.images_index_img_meta_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: images_index_img_meta_id_seq; Type: SEQUENCE OWNED BY; Schema: files; Owner: -
--

ALTER SEQUENCE files.images_index_img_meta_id_seq OWNED BY files.image_series.img_meta_id;


--
-- Name: internal_status; Type: TABLE; Schema: information; Owner: -
--

CREATE TABLE information.internal_status (
    event text NOT NULL,
    value timestamp with time zone
);


--
-- Name: TABLE internal_status; Type: COMMENT; Schema: information; Owner: -
--

COMMENT ON TABLE information.internal_status IS 'Holds information about when a certain operation took place on the database using the R functions in the HydroMetDB package.';


--
-- Name: version_info; Type: TABLE; Schema: information; Owner: -
--

CREATE TABLE information.version_info (
    id integer NOT NULL,
    item text,
    version text,
    created_modified timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: version_info_id_seq; Type: SEQUENCE; Schema: information; Owner: -
--

CREATE SEQUENCE information.version_info_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: version_info_id_seq; Type: SEQUENCE OWNED BY; Schema: information; Owner: -
--

ALTER SEQUENCE information.version_info_id_seq OWNED BY information.version_info.id;


--
-- Name: array_maintenance_changes; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.array_maintenance_changes (
    instrument_id integer NOT NULL,
    observer integer NOT NULL,
    obs_datetime timestamp with time zone NOT NULL,
    sensor1_id integer,
    sensor1_notes text,
    sensor2_id integer,
    sensor2_notes text,
    sensor3_id integer,
    sensor3_notes text,
    sensor4_id integer,
    sensor4_notes text,
    sensor5_id integer,
    sensor5_notes text,
    sensor6_id integer,
    sensor6_notes text,
    sensor7_id integer,
    sensor7_notes text,
    sensor8_id integer,
    sensor8_notes text,
    event_id integer NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE array_maintenance_changes; Type: COMMENT; Schema: instruments; Owner: -
--

COMMENT ON TABLE instruments.array_maintenance_changes IS 'This table is used to record changes to the sensors in an instrument array. Each row represents a single maintenance event, and the notes field should contain a description of the changes or maintenance made to each sensor. A simple maintenance event with no change of sensor should have the same sensor_id as the previous entry for that instrument and sensorX_id, while sensor changes must be recorded with a new sensor_id.';


--
-- Name: array_maintenance_changes_event_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.array_maintenance_changes_event_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: array_maintenance_changes_event_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.array_maintenance_changes_event_id_seq OWNED BY instruments.array_maintenance_changes.event_id;


--
-- Name: calibrate_depth; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_depth (
    calibration_id integer NOT NULL,
    depth_check_ok boolean NOT NULL,
    depth_changes_ok boolean,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrate_dissolved_oxygen; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_dissolved_oxygen (
    calibration_id integer NOT NULL,
    baro_press_pre numeric NOT NULL,
    baro_press_post numeric NOT NULL,
    do_pre_mgl numeric NOT NULL,
    do_post_mgl numeric NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrate_orp; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_orp (
    calibration_id integer NOT NULL,
    orp_std numeric NOT NULL,
    orp_pre_mv numeric NOT NULL,
    orp_post_mv numeric NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrate_ph; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_ph (
    calibration_id integer NOT NULL,
    ph1_std numeric NOT NULL,
    ph2_std numeric NOT NULL,
    ph3_std numeric,
    ph1_pre_val numeric NOT NULL,
    ph2_pre_val numeric NOT NULL,
    ph3_pre_val numeric,
    ph1_mv numeric NOT NULL,
    ph2_mv numeric NOT NULL,
    ph3_mv numeric,
    ph1_post_val numeric NOT NULL,
    ph2_post_val numeric NOT NULL,
    ph3_post_val numeric,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrate_specific_conductance; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_specific_conductance (
    calibration_id integer NOT NULL,
    spc1_std numeric NOT NULL,
    spc2_std numeric NOT NULL,
    spc1_pre numeric NOT NULL,
    spc2_pre numeric NOT NULL,
    spc1_post numeric NOT NULL,
    spc2_post numeric NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrate_temperature; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_temperature (
    calibration_id integer NOT NULL,
    temp_reference_desc text NOT NULL,
    temp_reference numeric NOT NULL,
    temp_observed numeric NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrate_turbidity; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrate_turbidity (
    calibration_id integer NOT NULL,
    turb1_std numeric NOT NULL,
    turb2_std numeric NOT NULL,
    turb1_pre numeric NOT NULL,
    turb2_pre numeric NOT NULL,
    turb1_post numeric NOT NULL,
    turb2_post numeric NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrations; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.calibrations (
    calibration_id integer NOT NULL,
    observer integer NOT NULL,
    obs_datetime timestamp with time zone NOT NULL,
    id_sensor_holder integer NOT NULL,
    id_handheld_meter integer,
    complete boolean DEFAULT false NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    purpose text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: calibrations_calibration_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.calibrations_calibration_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: calibrations_calibration_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.calibrations_calibration_id_seq OWNED BY instruments.calibrations.calibration_id;


--
-- Name: instrument_maintenance; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.instrument_maintenance (
    event_id integer NOT NULL,
    instrument_id integer NOT NULL,
    observer integer NOT NULL,
    obs_datetime timestamp with time zone NOT NULL,
    note text NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    date_maintenance_due date,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: instrument_maintenance_event_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.instrument_maintenance_event_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: instrument_maintenance_event_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.instrument_maintenance_event_id_seq OWNED BY instruments.instrument_maintenance.event_id;


--
-- Name: instrument_make; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.instrument_make (
    make_id integer NOT NULL,
    make text NOT NULL,
    description text,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: instrument_make_make_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.instrument_make_make_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: instrument_make_make_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.instrument_make_make_id_seq OWNED BY instruments.instrument_make.make_id;


--
-- Name: instrument_model; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.instrument_model (
    model_id integer NOT NULL,
    model text NOT NULL,
    description text,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: instrument_model_model_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.instrument_model_model_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: instrument_model_model_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.instrument_model_model_id_seq OWNED BY instruments.instrument_model.model_id;


--
-- Name: instrument_type; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.instrument_type (
    type_id integer NOT NULL,
    type text NOT NULL,
    description text NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: instrument_type_type_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.instrument_type_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: instrument_type_type_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.instrument_type_type_id_seq OWNED BY instruments.instrument_type.type_id;


--
-- Name: instruments; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.instruments (
    instrument_id integer NOT NULL,
    obs_datetime timestamp with time zone NOT NULL,
    observer integer NOT NULL,
    make integer NOT NULL,
    model integer NOT NULL,
    type integer NOT NULL,
    holds_replaceable_sensors boolean DEFAULT false NOT NULL,
    serial_no text NOT NULL,
    asset_tag text,
    date_in_service date,
    date_purchased date,
    retired_by text,
    date_retired date,
    owner character varying,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    date_end_of_life date,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: instruments_instrument_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.instruments_instrument_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: instruments_instrument_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.instruments_instrument_id_seq OWNED BY instruments.instruments.instrument_id;


--
-- Name: observers; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.observers (
    observer_id integer NOT NULL,
    observer_first text NOT NULL,
    observer_last text NOT NULL,
    organization text NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: observers_observer_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.observers_observer_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: observers_observer_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.observers_observer_id_seq OWNED BY instruments.observers.observer_id;


--
-- Name: sensor_types; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.sensor_types (
    sensor_type_id integer NOT NULL,
    sensor_type text NOT NULL,
    sensor_type_description text,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: sensor_types_sensor_type_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.sensor_types_sensor_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sensor_types_sensor_type_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.sensor_types_sensor_type_id_seq OWNED BY instruments.sensor_types.sensor_type_id;


--
-- Name: sensors; Type: TABLE; Schema: instruments; Owner: -
--

CREATE TABLE instruments.sensors (
    sensor_id integer NOT NULL,
    sensor_type integer NOT NULL,
    sensor_serial text NOT NULL,
    sensor_make text NOT NULL,
    sensor_model text NOT NULL,
    date_in_service date,
    date_purchased date,
    retired_by text,
    date_retired date,
    sensor_asset_tag text,
    sensor_notes text,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    date_maintenance_due date,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: sensors_sensor_id_seq; Type: SEQUENCE; Schema: instruments; Owner: -
--

CREATE SEQUENCE instruments.sensors_sensor_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sensors_sensor_id_seq; Type: SEQUENCE OWNED BY; Schema: instruments; Owner: -
--

ALTER SEQUENCE instruments.sensors_sensor_id_seq OWNED BY instruments.sensors.sensor_id;


--
-- Name: approval_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.approval_types (
    approval_type_id integer NOT NULL,
    approval_type_code text NOT NULL,
    approval_type_description text NOT NULL,
    approval_type_description_fr text,
    color_code character varying(9),
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: approval_types_approval_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.approval_types_approval_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: approval_types_approval_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.approval_types_approval_type_id_seq OWNED BY public.approval_types.approval_type_id;


--
-- Name: correction_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.correction_types (
    correction_type_id integer NOT NULL,
    correction_type text NOT NULL,
    description text NOT NULL,
    priority integer NOT NULL,
    value1 boolean DEFAULT false,
    value1_description text,
    value2 boolean DEFAULT false,
    value2_description text,
    timestep_window boolean DEFAULT false,
    equation boolean DEFAULT false NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE correction_types; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.correction_types IS 'Table to hold correction types and information for their use in the corrections table';


--
-- Name: COLUMN correction_types.priority; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.correction_types.priority IS 'Defines the order in which corrections are applied. Lower numbers are applied first.';


--
-- Name: COLUMN correction_types.value1; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.correction_types.value1 IS 'Whether or not value1 is required for this correction type. NULL if optional';


--
-- Name: COLUMN correction_types.value2; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.correction_types.value2 IS 'Whether or not value2 is required for this correction type. NULL if optional';


--
-- Name: COLUMN correction_types.timestep_window; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.correction_types.timestep_window IS 'Whether or not timestep_window is required for this correction type. NULL if optional';


--
-- Name: COLUMN correction_types.equation; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.correction_types.equation IS 'Whether or not equation is required for this correction type. NULL if optional';


--
-- Name: correction_types_correction_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.correction_types_correction_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: correction_types_correction_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.correction_types_correction_type_id_seq OWNED BY public.correction_types.correction_type_id;


--
-- Name: datum_conversions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.datum_conversions (
    conversion_id integer NOT NULL,
    datum_id_from integer NOT NULL,
    datum_id_to integer NOT NULL,
    conversion_m numeric NOT NULL,
    current boolean NOT NULL,
    location_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE datum_conversions; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.datum_conversions IS 'Holds vertical datum conversions in meters, as well as identifying the most recent conversion for the timeseries.';


--
-- Name: COLUMN datum_conversions.conversion_id; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.datum_conversions.conversion_id IS 'Integer autoincrement column uniquely identifying the conversion.';


--
-- Name: COLUMN datum_conversions.datum_id_from; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.datum_conversions.datum_id_from IS 'The datum_id (matching an entry in table datum_list) from which the conversion value is from. Datum_id 10 is equal to the station assumed datum (station 0 relative to some arbitrary local benchmark).';


--
-- Name: COLUMN datum_conversions.datum_id_to; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.datum_conversions.datum_id_to IS 'The datum_id (matching an entry in table datum_list) from which the conversion value is to.';


--
-- Name: COLUMN datum_conversions.conversion_m; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.datum_conversions.conversion_m IS 'The elevation offset in meters to apply if transforming elevation values from the datum_id_from to the datum_id_to.';


--
-- Name: COLUMN datum_conversions.current; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.datum_conversions.current IS 'TRUE means that the conversion is the most up-to-date in the database. Only one conversion_id can be current for each location.';


--
-- Name: datum_conversions_conversion_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.datum_conversions_conversion_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: datum_conversions_conversion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.datum_conversions_conversion_id_seq OWNED BY public.datum_conversions.conversion_id;


--
-- Name: datum_list; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.datum_list (
    datum_id integer NOT NULL,
    datum_name_en text NOT NULL,
    datum_name_fr text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE datum_list; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.datum_list IS 'Holds datum ids (referenced in the table datum_conversions) and their corresponding names in french and english. Taken directly from the datum list provided by HYDAT. Non-hydat datums can be added with datum_id beginning at 1000.';


--
-- Name: grade_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.grade_types (
    grade_type_id integer NOT NULL,
    grade_type_code text NOT NULL,
    grade_type_description text NOT NULL,
    grade_type_description_fr text,
    color_code character varying(9),
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: grade_types_grade_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.grade_types_grade_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: grade_types_grade_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.grade_types_grade_type_id_seq OWNED BY public.grade_types.grade_type_id;


--
-- Name: locations_networks; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_networks (
    network_id integer NOT NULL,
    location_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: locations_projects; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_projects (
    project_id integer NOT NULL,
    location_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: networks; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.networks (
    network_id integer NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    name_fr text,
    description_fr text,
    type integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: projects; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.projects (
    project_id integer NOT NULL,
    name text NOT NULL,
    description text NOT NULL,
    name_fr text,
    description_fr text,
    type integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: location_metadata_en; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.location_metadata_en WITH (security_invoker='true') AS
 SELECT loc.location_id,
    loc.location AS location_code,
    loc.name,
    loc.latitude,
    loc.longitude,
    dc.conversion_m AS elevation,
    dl.datum_name_en AS datum,
    loc.note,
    array_agg(DISTINCT proj.name) AS projects,
    array_agg(DISTINCT net.name) AS networks
   FROM ((((((public.locations loc
     LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
     LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
     LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
     LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
     LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
     LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
  GROUP BY loc.location_id, loc.location, loc.name, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_en;


--
-- Name: location_metadata_fr; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.location_metadata_fr WITH (security_invoker='true') AS
 SELECT loc.location_id,
    loc.location AS code_de_site,
    loc.name_fr AS nom,
    loc.latitude,
    loc.longitude,
    dc.conversion_m AS altitude,
    dl.datum_name_fr AS datum,
    loc.note,
    array_agg(DISTINCT proj.name_fr) AS projets,
    array_agg(DISTINCT net.name_fr) AS "réseaux"
   FROM ((((((public.locations loc
     LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
     LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
     LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
     LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
     LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
     LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
  GROUP BY loc.location_id, loc.location, loc.name_fr, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_fr;


--
-- Name: location_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.location_types (
    type_id integer NOT NULL,
    type text NOT NULL,
    type_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: location_types_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.location_types_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: location_types_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.location_types_type_id_seq OWNED BY public.location_types.type_id;


--
-- Name: locations_location_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_location_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_location_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_location_id_seq OWNED BY public.locations.location_id;


--
-- Name: locations_metadata_access; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_access (
    id integer NOT NULL,
    location_id integer NOT NULL,
    method text,
    health_safety text,
    notes text,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified timestamp with time zone,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE locations_metadata_access; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.locations_metadata_access IS 'Site access metadata for locations.';


--
-- Name: COLUMN locations_metadata_access.method; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_access.method IS 'Method of access, such as helicopter, boat, foot, etc.';


--
-- Name: COLUMN locations_metadata_access.health_safety; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_access.health_safety IS 'Health and safety considerations for accessing the site.';


--
-- Name: COLUMN locations_metadata_access.notes; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_access.notes IS 'Additional notes about site access.';


--
-- Name: locations_metadata_access_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_access_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_access_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_access_id_seq OWNED BY public.locations_metadata_access.id;


--
-- Name: locations_metadata_infrastructure; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_infrastructure (
    id integer NOT NULL,
    location_id integer NOT NULL,
    site_description text,
    infrastructure_description text,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified timestamp with time zone,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE locations_metadata_infrastructure; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.locations_metadata_infrastructure IS 'General infrastructure metadata for locations as freehand text.';


--
-- Name: locations_metadata_infrastructure_groundwater; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_infrastructure_groundwater (
    id integer NOT NULL,
    location_id integer NOT NULL,
    well_diameter_cm numeric,
    stick_up_m numeric,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified timestamp with time zone,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE locations_metadata_infrastructure_groundwater; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.locations_metadata_infrastructure_groundwater IS 'Groundwater-specific infrastructure metadata.';


--
-- Name: COLUMN locations_metadata_infrastructure_groundwater.well_diameter_cm; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_groundwater.well_diameter_cm IS 'Diameter of well in centimeters.';


--
-- Name: COLUMN locations_metadata_infrastructure_groundwater.stick_up_m; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_groundwater.stick_up_m IS 'Distance from top of casing to ground surface in meters.';


--
-- Name: locations_metadata_infrastructure_groundwater_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_infrastructure_groundwater_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_infrastructure_groundwater_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_infrastructure_groundwater_id_seq OWNED BY public.locations_metadata_infrastructure_groundwater.id;


--
-- Name: locations_metadata_infrastructure_hydromet; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_infrastructure_hydromet (
    id integer NOT NULL,
    location_id integer NOT NULL,
    stilling_well text,
    staff_gauge text,
    tower text,
    shelter text,
    cableway text,
    reach_substrate text,
    reach_grade numeric,
    reach_note text,
    health_safety text,
    local_g numeric,
    magnetic_declination numeric,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified timestamp with time zone,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE locations_metadata_infrastructure_hydromet; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.locations_metadata_infrastructure_hydromet IS 'Hydrometric-specific infrastructure metadata.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.stilling_well; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.stilling_well IS 'Type of stilling well and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.staff_gauge; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.staff_gauge IS 'Type of staff gauge and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.tower; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.tower IS 'Type of tower and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.shelter; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.shelter IS 'Type of shelter and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.reach_substrate; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.reach_substrate IS 'Substrate type at reach.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.reach_grade; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.reach_grade IS 'Grade of reach (approximate, degrees).';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.reach_note; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.reach_note IS 'Additional notes about reach.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.local_g; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.local_g IS 'Location-specific gravitational constant';


--
-- Name: locations_metadata_infrastructure_hydromet_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_infrastructure_hydromet_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_infrastructure_hydromet_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_infrastructure_hydromet_id_seq OWNED BY public.locations_metadata_infrastructure_hydromet.id;


--
-- Name: locations_metadata_infrastructure_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_infrastructure_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_infrastructure_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_infrastructure_id_seq OWNED BY public.locations_metadata_infrastructure.id;


--
-- Name: locations_metadata_instruments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_instruments (
    metadata_id integer NOT NULL,
    location_id integer NOT NULL,
    sub_location_id integer,
    instruments integer[],
    start_dt timestamp with time zone NOT NULL,
    end_dt timestamp with time zone,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: locations_metadata_instruments_metadata_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_instruments_metadata_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_instruments_metadata_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_instruments_metadata_id_seq OWNED BY public.locations_metadata_instruments.metadata_id;


--
-- Name: locations_metadata_maintenance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_maintenance (
    location_maintenance_id integer NOT NULL,
    location_id integer NOT NULL,
    maintenance_performed text,
    date_performed date,
    maintenance_due text,
    maintenance_flag boolean,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: locations_metadata_maintenance_location_maintenance_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_maintenance_location_maintenance_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_maintenance_location_maintenance_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_maintenance_location_maintenance_id_seq OWNED BY public.locations_metadata_maintenance.location_maintenance_id;


--
-- Name: locations_metadata_owners_operators; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_owners_operators (
    id integer NOT NULL,
    location_id integer NOT NULL,
    owner integer NOT NULL,
    operator integer NOT NULL,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified timestamp with time zone,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE locations_metadata_owners_operators; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.locations_metadata_owners_operators IS 'Ownership and operator metadata for locations.';


--
-- Name: locations_metadata_owners_operators_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_owners_operators_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_owners_operators_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_owners_operators_id_seq OWNED BY public.locations_metadata_owners_operators.id;


--
-- Name: locations_metadata_xsections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.locations_metadata_xsections (
    id integer NOT NULL,
    location_id integer NOT NULL,
    x_m numeric[],
    y_top_m numeric[] DEFAULT '{0}'::numeric[],
    y_bot_m numeric[],
    mean_velocity_m_s numeric[],
    instrument integer,
    angle_deg numeric DEFAULT 90,
    water_level_m numeric,
    xsection_substrate text,
    xsection_note text,
    measurement_grade integer,
    valid timestamp with time zone NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified timestamp with time zone,
    sub_location_id integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: TABLE locations_metadata_xsections; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.locations_metadata_xsections IS 'Cross-section and reach metadata for hydrometric sites.';


--
-- Name: COLUMN locations_metadata_xsections.x_m; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.x_m IS 'Distance along cross-section from left bank in meters.';


--
-- Name: COLUMN locations_metadata_xsections.y_top_m; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.y_top_m IS 'Top elevation of cross-section in meters (default 0).';


--
-- Name: COLUMN locations_metadata_xsections.y_bot_m; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.y_bot_m IS 'Bottom elevation of cross-section in meters.';


--
-- Name: COLUMN locations_metadata_xsections.mean_velocity_m_s; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.mean_velocity_m_s IS 'Mean velocity of water at cross-section x-value in cubic meters per second.';


--
-- Name: COLUMN locations_metadata_xsections.instrument; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.instrument IS 'Instrument used to measure velocity, referencing instruments.instruments table.';


--
-- Name: COLUMN locations_metadata_xsections.angle_deg; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.angle_deg IS 'Angle of cross-section relative to perpendicular to channel orientation. 90 (default) is perpendicular.';


--
-- Name: COLUMN locations_metadata_xsections.water_level_m; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.water_level_m IS 'Water level at time of cross-section measurements in meters.';


--
-- Name: COLUMN locations_metadata_xsections.xsection_substrate; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.xsection_substrate IS 'Substrate type at cross-section (description).';


--
-- Name: COLUMN locations_metadata_xsections.xsection_note; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.xsection_note IS 'Additional notes about cross-section.';


--
-- Name: COLUMN locations_metadata_xsections.valid; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON COLUMN public.locations_metadata_xsections.valid IS 'Timestamp of validity of data.';


--
-- Name: locations_metadata_xsections_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.locations_metadata_xsections_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: locations_metadata_xsections_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.locations_metadata_xsections_id_seq OWNED BY public.locations_metadata_xsections.id;


--
-- Name: network_project_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.network_project_types (
    id integer NOT NULL,
    name text NOT NULL,
    name_fr text,
    description text,
    description_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: network_project_types_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.network_project_types_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: network_project_types_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.network_project_types_id_seq OWNED BY public.network_project_types.id;


--
-- Name: networks_network_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.networks_network_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: networks_network_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.networks_network_id_seq OWNED BY public.networks.network_id;


--
-- Name: organizations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.organizations (
    organization_id integer NOT NULL,
    name text NOT NULL,
    contact_name text,
    phone text,
    email text,
    note text,
    name_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: owners_contributors_owner_contributor_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.owners_contributors_owner_contributor_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: owners_contributors_owner_contributor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.owners_contributors_owner_contributor_id_seq OWNED BY public.organizations.organization_id;


--
-- Name: param_type_param_type_code_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.param_type_param_type_code_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: param_type_param_type_code_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.param_type_param_type_code_seq OWNED BY public.media_types.media_id;


--
-- Name: parameter_groups; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.parameter_groups (
    group_id integer NOT NULL,
    group_name text NOT NULL,
    group_name_fr text,
    description text,
    description_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: parameter_groups_group_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.parameter_groups_group_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: parameter_groups_group_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.parameter_groups_group_id_seq OWNED BY public.parameter_groups.group_id;


--
-- Name: parameter_relationships; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.parameter_relationships (
    relationship_id integer NOT NULL,
    parameter_id integer NOT NULL,
    group_id integer NOT NULL,
    sub_group_id integer
);


--
-- Name: parameter_relationships_relationship_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.parameter_relationships_relationship_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: parameter_relationships_relationship_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.parameter_relationships_relationship_id_seq OWNED BY public.parameter_relationships.relationship_id;


--
-- Name: parameter_sub_groups; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.parameter_sub_groups (
    sub_group_id integer NOT NULL,
    sub_group_name text NOT NULL,
    sub_group_name_fr text,
    description text,
    description_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: parameter_sub_groups_sub_group_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.parameter_sub_groups_sub_group_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: parameter_sub_groups_sub_group_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.parameter_sub_groups_sub_group_id_seq OWNED BY public.parameter_sub_groups.sub_group_id;


--
-- Name: parameters_param_code_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.parameters_param_code_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: parameters_param_code_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.parameters_param_code_seq OWNED BY public.parameters.parameter_id;


--
-- Name: projects_project_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.projects_project_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: projects_project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.projects_project_id_seq OWNED BY public.projects.project_id;


--
-- Name: qualifier_types; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.qualifier_types (
    qualifier_type_id integer NOT NULL,
    qualifier_type_code text NOT NULL,
    qualifier_type_description text,
    qualifier_type_description_fr text,
    color_code character varying(9),
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: qualifier_types_qualifier_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.qualifier_types_qualifier_type_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: qualifier_types_qualifier_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.qualifier_types_qualifier_type_id_seq OWNED BY public.qualifier_types.qualifier_type_id;


--
-- Name: sub_locations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sub_locations (
    sub_location_id integer NOT NULL,
    location_id integer NOT NULL,
    sub_location_name text NOT NULL,
    sub_location_name_fr text,
    latitude numeric,
    longitude numeric,
    note text,
    share_with text[],
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


--
-- Name: sub_locations_sub_location_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.sub_locations_sub_location_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sub_locations_sub_location_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.sub_locations_sub_location_id_seq OWNED BY public.sub_locations.sub_location_id;


--
-- Name: raster_series_index; Type: TABLE; Schema: spatial; Owner: -
--

CREATE TABLE spatial.raster_series_index (
    raster_series_id integer NOT NULL,
    model text NOT NULL,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone NOT NULL,
    last_new_raster timestamp with time zone NOT NULL,
    source_fx text,
    source_fx_args jsonb,
    parameter text NOT NULL,
    last_issue timestamp with time zone,
    type text NOT NULL,
    active boolean DEFAULT true NOT NULL,
    param_description text,
    CONSTRAINT raster_series_index_type_check CHECK ((type = ANY (ARRAY['reanalysis'::text, 'forecast'::text])))
);


--
-- Name: TABLE raster_series_index; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON TABLE spatial.raster_series_index IS 'Holds metadata about raster series, such as reanalysis or forecast rasters. ';


--
-- Name: COLUMN raster_series_index.end_datetime; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.raster_series_index.end_datetime IS 'For rasters that have a valid_from and valid_to time, this is the valid_from of the latest raster in the database.';


--
-- Name: COLUMN raster_series_index.active; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.raster_series_index.active IS 'Defines if the raster series should or should not be imported.';


--
-- Name: raster_series_index_raster_series_id_seq; Type: SEQUENCE; Schema: spatial; Owner: -
--

CREATE SEQUENCE spatial.raster_series_index_raster_series_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: raster_series_index_raster_series_id_seq; Type: SEQUENCE OWNED BY; Schema: spatial; Owner: -
--

ALTER SEQUENCE spatial.raster_series_index_raster_series_id_seq OWNED BY spatial.raster_series_index.raster_series_id;


--
-- Name: rasters; Type: TABLE; Schema: spatial; Owner: -
--

CREATE TABLE spatial.rasters (
    rid integer NOT NULL,
    reference_id integer,
    r_class text,
    r_proj4 text,
    rast spatial.raster NOT NULL,
    CONSTRAINT enforce_height_rast CHECK ((spatial.st_height(rast) = ANY (ARRAY[578, 580]))),
    CONSTRAINT enforce_nodata_values_rast CHECK ((spatial._raster_constraint_nodata_values(rast) = '{-99999.0000000000}'::numeric[])),
    CONSTRAINT enforce_num_bands_rast CHECK ((spatial.st_numbands(rast) = 1)),
    CONSTRAINT enforce_out_db_rast CHECK ((spatial._raster_constraint_out_db(rast) = '{f}'::boolean[])),
    CONSTRAINT enforce_pixel_types_rast CHECK ((spatial._raster_constraint_pixel_types(rast) = '{32BF}'::text[])),
    CONSTRAINT enforce_width_rast CHECK ((spatial.st_width(rast) = ANY (ARRAY[576, 578])))
);


--
-- Name: TABLE rasters; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON TABLE spatial.rasters IS 'Holds raster tiles. Rasters may be broken up in multiple tiles, so refer to table rasters_reference to find the reference_ID for each raster. Otherwise this table is designed for extracting rasters using R, hence the r_class and r_proj4 columns.';


--
-- Name: COLUMN rasters.reference_id; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.rasters.reference_id IS 'Matches a unique entry in table rasters_reference. If a raster is broken up into tiles, each tile will have the same reference_id; this number is what identifies them as being tiles of the same raster.';


--
-- Name: rasters_reference; Type: TABLE; Schema: spatial; Owner: -
--

CREATE TABLE spatial.rasters_reference (
    reference_id integer NOT NULL,
    raster_series_id integer,
    type text,
    model text,
    description text,
    band_names text NOT NULL,
    units text,
    valid_from timestamp with time zone,
    valid_to timestamp with time zone,
    issued timestamp with time zone,
    source text,
    flag text,
    CONSTRAINT check_model_constraints CHECK ((((type = 'model'::text) AND (valid_from IS NOT NULL) AND (valid_to IS NOT NULL)) OR ((type = 'other'::text) AND (description IS NOT NULL)))),
    CONSTRAINT rasters_reference_type_check CHECK ((type = ANY (ARRAY['model'::text, 'other'::text])))
);


--
-- Name: TABLE rasters_reference; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON TABLE spatial.rasters_reference IS 'References rasters in the rasters table, since the later might have rasters broken up in multiple tiles. This table has one reference_id per raster, which may be linked to multiple entries in table rasters.';


--
-- Name: COLUMN rasters_reference.reference_id; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.rasters_reference.reference_id IS 'Used to identify one or more raster tiles (large rasters may be broken up in tiles for performance) in the table rasters.';


--
-- Name: COLUMN rasters_reference.raster_series_id; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.rasters_reference.raster_series_id IS 'Identifies a time-series of rasters, the details of which are stored in table raster_series_index.';


--
-- Name: COLUMN rasters_reference.model; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.rasters_reference.model IS 'If the raster is generated from a model such as a climate model enter the name here. This is more useful for one-off rasters, as model timeseries will also list the model in table raster_series_index.';


--
-- Name: COLUMN rasters_reference.flag; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.rasters_reference.flag IS 'Used to flag rasters that require further review or that need to be deleted after a certain period. Reanalysis products in particular can have preliminary issues, in which case PRELIMINARY would be entered here.';


--
-- Name: rasters_reference_reference_id_seq; Type: SEQUENCE; Schema: spatial; Owner: -
--

CREATE SEQUENCE spatial.rasters_reference_reference_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rasters_reference_reference_id_seq; Type: SEQUENCE OWNED BY; Schema: spatial; Owner: -
--

ALTER SEQUENCE spatial.rasters_reference_reference_id_seq OWNED BY spatial.rasters_reference.reference_id;


--
-- Name: rasters_rid_seq; Type: SEQUENCE; Schema: spatial; Owner: -
--

CREATE SEQUENCE spatial.rasters_rid_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rasters_rid_seq; Type: SEQUENCE OWNED BY; Schema: spatial; Owner: -
--

ALTER SEQUENCE spatial.rasters_rid_seq OWNED BY spatial.rasters.rid;


--
-- Name: vectors; Type: TABLE; Schema: spatial; Owner: -
--

CREATE TABLE spatial.vectors (
    geom_id integer NOT NULL,
    geom_type text NOT NULL,
    layer_name text NOT NULL,
    feature_name text NOT NULL,
    description text,
    geom spatial.geometry(Geometry,4269) NOT NULL,
    CONSTRAINT enforce_dims_geom CHECK ((spatial.st_ndims(geom) = 2)),
    CONSTRAINT enforce_srid_geom CHECK ((spatial.st_srid(geom) = 4269)),
    CONSTRAINT enforce_valid_geom CHECK (spatial.st_isvalid(geom)),
    CONSTRAINT vectors_geom_type_check CHECK ((geom_type = ANY (ARRAY['ST_Point'::text, 'ST_MultiPoint'::text, 'ST_LineString'::text, 'ST_MultiLineString'::text, 'ST_Polygon'::text, 'ST_MultiPolygon'::text])))
);


--
-- Name: TABLE vectors; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON TABLE spatial.vectors IS 'Holds points, lines, or polygons as geometry objects that can be references by other tables. For example, the locations table references a geom_id for each location. Retrieve objects from this table using function HydroMetDB::fetchVector, insert them using HydroMetDB::insertHydrometVector.';


--
-- Name: COLUMN vectors.geom_type; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.vectors.geom_type IS '*DO NOT TOUCH* Auto-populated by trigger based on the geometry type for each entry.';


--
-- Name: COLUMN vectors.layer_name; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.vectors.layer_name IS 'Non-optional descriptive name for the layer.';


--
-- Name: COLUMN vectors.feature_name; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.vectors.feature_name IS 'Non-optional descriptive name for the feature.';


--
-- Name: COLUMN vectors.description; Type: COMMENT; Schema: spatial; Owner: -
--

COMMENT ON COLUMN spatial.vectors.description IS 'Optional but highly recommended long-form description of the geometry object.';


--
-- Name: vectors_geom_id_seq; Type: SEQUENCE; Schema: spatial; Owner: -
--

CREATE SEQUENCE spatial.vectors_geom_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: vectors_geom_id_seq; Type: SEQUENCE OWNED BY; Schema: spatial; Owner: -
--

ALTER SEQUENCE spatial.vectors_geom_id_seq OWNED BY spatial.vectors.geom_id;


--
-- Name: aggregation_types aggregation_type_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.aggregation_types ALTER COLUMN aggregation_type_id SET DEFAULT nextval('continuous.aggregation_types_aggregation_type_id_seq'::regclass);


--
-- Name: approvals approval_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.approvals ALTER COLUMN approval_id SET DEFAULT nextval('continuous.approvals_approval_id_seq'::regclass);


--
-- Name: contributors contributor_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.contributors ALTER COLUMN contributor_id SET DEFAULT nextval('continuous.contributors_contributor_id_seq'::regclass);


--
-- Name: corrections correction_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.corrections ALTER COLUMN correction_id SET DEFAULT nextval('continuous.corrections_correction_id_seq'::regclass);


--
-- Name: grades grade_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.grades ALTER COLUMN grade_id SET DEFAULT nextval('continuous.grades_grade_id_seq'::regclass);


--
-- Name: owners owner_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.owners ALTER COLUMN owner_id SET DEFAULT nextval('continuous.owners_owner_id_seq'::regclass);


--
-- Name: qualifiers qualifier_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.qualifiers ALTER COLUMN qualifier_id SET DEFAULT nextval('continuous.qualifiers_qualifier_id_seq'::regclass);


--
-- Name: rating_curve_points curve_point_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_points ALTER COLUMN curve_point_id SET DEFAULT nextval('continuous.rating_curve_points_curve_point_id_seq'::regclass);


--
-- Name: rating_curve_shifts curve_shift_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_shifts ALTER COLUMN curve_shift_id SET DEFAULT nextval('continuous.rating_curve_shifts_curve_shift_id_seq'::regclass);


--
-- Name: rating_curves curve_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves ALTER COLUMN curve_id SET DEFAULT nextval('continuous.rating_curves_curve_id_seq'::regclass);


--
-- Name: timeseries timeseries_id; Type: DEFAULT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries ALTER COLUMN timeseries_id SET DEFAULT nextval('continuous.timeseries_timeseries_id_seq'::regclass);


--
-- Name: collection_methods collection_method_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.collection_methods ALTER COLUMN collection_method_id SET DEFAULT nextval('discrete.collection_methods_collection_method_id_seq'::regclass);


--
-- Name: guideline_values id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.guideline_values ALTER COLUMN id SET DEFAULT nextval('discrete.guideline_values_id_seq'::regclass);


--
-- Name: laboratories lab_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.laboratories ALTER COLUMN lab_id SET DEFAULT nextval('discrete.laboratories_lab_id_seq'::regclass);


--
-- Name: protocols_methods protocol_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.protocols_methods ALTER COLUMN protocol_id SET DEFAULT nextval('discrete.analysis_protocols_protocol_id_seq'::regclass);


--
-- Name: result_conditions result_condition_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_conditions ALTER COLUMN result_condition_id SET DEFAULT nextval('discrete.result_conditions_result_condition_id_seq'::regclass);


--
-- Name: result_speciations result_speciation_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_speciations ALTER COLUMN result_speciation_id SET DEFAULT nextval('discrete.result_speciations_result_speciation_id_seq'::regclass);


--
-- Name: result_types result_type_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_types ALTER COLUMN result_type_id SET DEFAULT nextval('discrete.result_types_result_type_id_seq'::regclass);


--
-- Name: result_value_types result_value_type_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_value_types ALTER COLUMN result_value_type_id SET DEFAULT nextval('discrete.result_value_types_result_value_type_id_seq'::regclass);


--
-- Name: results result_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results ALTER COLUMN result_id SET DEFAULT nextval('discrete.results_result_id_seq'::regclass);


--
-- Name: sample_fractions sample_fraction_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_fractions ALTER COLUMN sample_fraction_id SET DEFAULT nextval('discrete.sample_fractions_sample_fraction_id_seq'::regclass);


--
-- Name: sample_series sample_series_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series ALTER COLUMN sample_series_id SET DEFAULT nextval('discrete.sample_series_sample_series_id_seq'::regclass);


--
-- Name: sample_types sample_type_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_types ALTER COLUMN sample_type_id SET DEFAULT nextval('discrete.sample_types_sample_type_id_seq'::regclass);


--
-- Name: samples sample_id; Type: DEFAULT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples ALTER COLUMN sample_id SET DEFAULT nextval('discrete.samples_sample_id_seq'::regclass);


--
-- Name: document_types document_type_id; Type: DEFAULT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.document_types ALTER COLUMN document_type_id SET DEFAULT nextval('files.document_types_document_type_id_seq'::regclass);


--
-- Name: documents document_id; Type: DEFAULT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents ALTER COLUMN document_id SET DEFAULT nextval('files.documents_document_id_seq'::regclass);


--
-- Name: image_series img_meta_id; Type: DEFAULT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_series ALTER COLUMN img_meta_id SET DEFAULT nextval('files.images_index_img_meta_id_seq'::regclass);


--
-- Name: image_types image_type_id; Type: DEFAULT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_types ALTER COLUMN image_type_id SET DEFAULT nextval('files.image_types_image_type_id_seq'::regclass);


--
-- Name: images image_id; Type: DEFAULT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images ALTER COLUMN image_id SET DEFAULT nextval('files.images_image_id_seq'::regclass);


--
-- Name: version_info id; Type: DEFAULT; Schema: information; Owner: -
--

ALTER TABLE ONLY information.version_info ALTER COLUMN id SET DEFAULT nextval('information.version_info_id_seq'::regclass);


--
-- Name: array_maintenance_changes event_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes ALTER COLUMN event_id SET DEFAULT nextval('instruments.array_maintenance_changes_event_id_seq'::regclass);


--
-- Name: calibrations calibration_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrations ALTER COLUMN calibration_id SET DEFAULT nextval('instruments.calibrations_calibration_id_seq'::regclass);


--
-- Name: instrument_maintenance event_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_maintenance ALTER COLUMN event_id SET DEFAULT nextval('instruments.instrument_maintenance_event_id_seq'::regclass);


--
-- Name: instrument_make make_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_make ALTER COLUMN make_id SET DEFAULT nextval('instruments.instrument_make_make_id_seq'::regclass);


--
-- Name: instrument_model model_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_model ALTER COLUMN model_id SET DEFAULT nextval('instruments.instrument_model_model_id_seq'::regclass);


--
-- Name: instrument_type type_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_type ALTER COLUMN type_id SET DEFAULT nextval('instruments.instrument_type_type_id_seq'::regclass);


--
-- Name: instruments instrument_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments ALTER COLUMN instrument_id SET DEFAULT nextval('instruments.instruments_instrument_id_seq'::regclass);


--
-- Name: observers observer_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.observers ALTER COLUMN observer_id SET DEFAULT nextval('instruments.observers_observer_id_seq'::regclass);


--
-- Name: sensor_types sensor_type_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.sensor_types ALTER COLUMN sensor_type_id SET DEFAULT nextval('instruments.sensor_types_sensor_type_id_seq'::regclass);


--
-- Name: sensors sensor_id; Type: DEFAULT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.sensors ALTER COLUMN sensor_id SET DEFAULT nextval('instruments.sensors_sensor_id_seq'::regclass);


--
-- Name: approval_types approval_type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.approval_types ALTER COLUMN approval_type_id SET DEFAULT nextval('public.approval_types_approval_type_id_seq'::regclass);


--
-- Name: correction_types correction_type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.correction_types ALTER COLUMN correction_type_id SET DEFAULT nextval('public.correction_types_correction_type_id_seq'::regclass);


--
-- Name: datum_conversions conversion_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.datum_conversions ALTER COLUMN conversion_id SET DEFAULT nextval('public.datum_conversions_conversion_id_seq'::regclass);


--
-- Name: grade_types grade_type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.grade_types ALTER COLUMN grade_type_id SET DEFAULT nextval('public.grade_types_grade_type_id_seq'::regclass);


--
-- Name: location_types type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_types ALTER COLUMN type_id SET DEFAULT nextval('public.location_types_type_id_seq'::regclass);


--
-- Name: locations location_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations ALTER COLUMN location_id SET DEFAULT nextval('public.locations_location_id_seq'::regclass);


--
-- Name: locations_metadata_access id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_access ALTER COLUMN id SET DEFAULT nextval('public.locations_metadata_access_id_seq'::regclass);


--
-- Name: locations_metadata_infrastructure id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure ALTER COLUMN id SET DEFAULT nextval('public.locations_metadata_infrastructure_id_seq'::regclass);


--
-- Name: locations_metadata_infrastructure_groundwater id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater ALTER COLUMN id SET DEFAULT nextval('public.locations_metadata_infrastructure_groundwater_id_seq'::regclass);


--
-- Name: locations_metadata_infrastructure_hydromet id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet ALTER COLUMN id SET DEFAULT nextval('public.locations_metadata_infrastructure_hydromet_id_seq'::regclass);


--
-- Name: locations_metadata_instruments metadata_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_instruments ALTER COLUMN metadata_id SET DEFAULT nextval('public.locations_metadata_instruments_metadata_id_seq'::regclass);


--
-- Name: locations_metadata_maintenance location_maintenance_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_maintenance ALTER COLUMN location_maintenance_id SET DEFAULT nextval('public.locations_metadata_maintenance_location_maintenance_id_seq'::regclass);


--
-- Name: locations_metadata_owners_operators id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators ALTER COLUMN id SET DEFAULT nextval('public.locations_metadata_owners_operators_id_seq'::regclass);


--
-- Name: locations_metadata_xsections id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections ALTER COLUMN id SET DEFAULT nextval('public.locations_metadata_xsections_id_seq'::regclass);


--
-- Name: media_types media_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.media_types ALTER COLUMN media_id SET DEFAULT nextval('public.param_type_param_type_code_seq'::regclass);


--
-- Name: network_project_types id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.network_project_types ALTER COLUMN id SET DEFAULT nextval('public.network_project_types_id_seq'::regclass);


--
-- Name: networks network_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.networks ALTER COLUMN network_id SET DEFAULT nextval('public.networks_network_id_seq'::regclass);


--
-- Name: organizations organization_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organizations ALTER COLUMN organization_id SET DEFAULT nextval('public.owners_contributors_owner_contributor_id_seq'::regclass);


--
-- Name: parameter_groups group_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_groups ALTER COLUMN group_id SET DEFAULT nextval('public.parameter_groups_group_id_seq'::regclass);


--
-- Name: parameter_relationships relationship_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_relationships ALTER COLUMN relationship_id SET DEFAULT nextval('public.parameter_relationships_relationship_id_seq'::regclass);


--
-- Name: parameter_sub_groups sub_group_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_sub_groups ALTER COLUMN sub_group_id SET DEFAULT nextval('public.parameter_sub_groups_sub_group_id_seq'::regclass);


--
-- Name: parameters parameter_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameters ALTER COLUMN parameter_id SET DEFAULT nextval('public.parameters_param_code_seq'::regclass);


--
-- Name: projects project_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects ALTER COLUMN project_id SET DEFAULT nextval('public.projects_project_id_seq'::regclass);


--
-- Name: qualifier_types qualifier_type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.qualifier_types ALTER COLUMN qualifier_type_id SET DEFAULT nextval('public.qualifier_types_qualifier_type_id_seq'::regclass);


--
-- Name: sub_locations sub_location_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sub_locations ALTER COLUMN sub_location_id SET DEFAULT nextval('public.sub_locations_sub_location_id_seq'::regclass);


--
-- Name: raster_series_index raster_series_id; Type: DEFAULT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.raster_series_index ALTER COLUMN raster_series_id SET DEFAULT nextval('spatial.raster_series_index_raster_series_id_seq'::regclass);


--
-- Name: rasters rid; Type: DEFAULT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters ALTER COLUMN rid SET DEFAULT nextval('spatial.rasters_rid_seq'::regclass);


--
-- Name: rasters_reference reference_id; Type: DEFAULT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters_reference ALTER COLUMN reference_id SET DEFAULT nextval('spatial.rasters_reference_reference_id_seq'::regclass);


--
-- Name: vectors geom_id; Type: DEFAULT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.vectors ALTER COLUMN geom_id SET DEFAULT nextval('spatial.vectors_geom_id_seq'::regclass);


--
-- Name: images images_pkey; Type: CONSTRAINT; Schema: application; Owner: -
--

ALTER TABLE ONLY application.images
    ADD CONSTRAINT images_pkey PRIMARY KEY (id);


--
-- Name: text text_pkey; Type: CONSTRAINT; Schema: application; Owner: -
--

ALTER TABLE ONLY application.text
    ADD CONSTRAINT text_pkey PRIMARY KEY (id);


--
-- Name: aggregation_types aggregation_types_aggregation_type_fr_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.aggregation_types
    ADD CONSTRAINT aggregation_types_aggregation_type_fr_key UNIQUE (aggregation_type_fr);


--
-- Name: aggregation_types aggregation_types_aggregation_type_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.aggregation_types
    ADD CONSTRAINT aggregation_types_aggregation_type_key UNIQUE (aggregation_type);


--
-- Name: aggregation_types aggregation_types_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.aggregation_types
    ADD CONSTRAINT aggregation_types_pkey PRIMARY KEY (aggregation_type_id);


--
-- Name: approvals approvals_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.approvals
    ADD CONSTRAINT approvals_pkey PRIMARY KEY (approval_id);


--
-- Name: measurements_calculated_daily calculated_daily_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.measurements_calculated_daily
    ADD CONSTRAINT calculated_daily_pkey PRIMARY KEY (timeseries_id, date);


--
-- Name: contributors contributors_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.contributors
    ADD CONSTRAINT contributors_pkey PRIMARY KEY (contributor_id);


--
-- Name: corrections corrections_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_pkey PRIMARY KEY (correction_id);


--
-- Name: corrections corrections_timeseries_id_start_dt_end_dt_correction_type_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_timeseries_id_start_dt_end_dt_correction_type_key UNIQUE (timeseries_id, start_dt, end_dt, correction_type);


--
-- Name: forecasts forecasts_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.forecasts
    ADD CONSTRAINT forecasts_pkey PRIMARY KEY (timeseries_id, datetime);


--
-- Name: grades grades_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.grades
    ADD CONSTRAINT grades_pkey PRIMARY KEY (grade_id);


--
-- Name: measurements_continuous measurements_continuous_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.measurements_continuous
    ADD CONSTRAINT measurements_continuous_pkey PRIMARY KEY (timeseries_id, datetime);


--
-- Name: owners owners_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.owners
    ADD CONSTRAINT owners_pkey PRIMARY KEY (owner_id);


--
-- Name: extrema peaks_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.extrema
    ADD CONSTRAINT peaks_pkey PRIMARY KEY (timeseries_id);


--
-- Name: extrema peaks_timeseries_id_agency_year_period_type_condition_extre_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.extrema
    ADD CONSTRAINT peaks_timeseries_id_agency_year_period_type_condition_extre_key UNIQUE (timeseries_id, agency, year, period_type, condition, extrema);


--
-- Name: qualifiers qualifiers_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.qualifiers
    ADD CONSTRAINT qualifiers_pkey PRIMARY KEY (qualifier_id);


--
-- Name: rating_curve_points rating_curve_points_curve_id_input_value_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_points
    ADD CONSTRAINT rating_curve_points_curve_id_input_value_key UNIQUE (curve_id, input_value);


--
-- Name: rating_curve_points rating_curve_points_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_points
    ADD CONSTRAINT rating_curve_points_pkey PRIMARY KEY (curve_point_id);


--
-- Name: rating_curve_shifts rating_curve_shifts_curve_id_shift_start_shift_end_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_curve_id_shift_start_shift_end_key UNIQUE (curve_id, shift_start, shift_end);


--
-- Name: rating_curve_shifts rating_curve_shifts_no_overlap; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_no_overlap EXCLUDE USING gist (curve_id WITH =, tstzrange(shift_start, shift_end, '[]'::text) WITH &&);


--
-- Name: rating_curve_shifts rating_curve_shifts_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_pkey PRIMARY KEY (curve_shift_id);


--
-- Name: rating_curves rating_curves_location_id_input_parameter_id_output_paramet_key; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_location_id_input_parameter_id_output_paramet_key UNIQUE (location_id, input_parameter_id, output_parameter_id, valid_from);


--
-- Name: rating_curves rating_curves_no_overlap; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_no_overlap EXCLUDE USING gist (location_id WITH =, input_parameter_id WITH =, output_parameter_id WITH =, tstzrange(valid_from, valid_to, '[]'::text) WITH &&);


--
-- Name: rating_curves rating_curves_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_pkey PRIMARY KEY (curve_id);


--
-- Name: thresholds thresholds_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.thresholds
    ADD CONSTRAINT thresholds_pkey PRIMARY KEY (timeseries_id);


--
-- Name: timeseries timeseries_pkey; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_pkey PRIMARY KEY (timeseries_id);


--
-- Name: timeseries timeseries_unique; Type: CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_unique UNIQUE NULLS NOT DISTINCT (location_id, parameter_id, aggregation_type_id, media_id, record_rate, z, sensor_priority, sub_location_id);


--
-- Name: protocols_methods analysis_protocols_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.protocols_methods
    ADD CONSTRAINT analysis_protocols_pkey PRIMARY KEY (protocol_id);


--
-- Name: protocols_methods analysis_protocols_protocol_name_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.protocols_methods
    ADD CONSTRAINT analysis_protocols_protocol_name_key UNIQUE (protocol_name);


--
-- Name: collection_methods collection_methods_collection_method_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.collection_methods
    ADD CONSTRAINT collection_methods_collection_method_key UNIQUE (collection_method);


--
-- Name: collection_methods collection_methods_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.collection_methods
    ADD CONSTRAINT collection_methods_pkey PRIMARY KEY (collection_method_id);


--
-- Name: guideline_values guideline_values_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.guideline_values
    ADD CONSTRAINT guideline_values_pkey PRIMARY KEY (id);


--
-- Name: laboratories laboratories_lab_name_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.laboratories
    ADD CONSTRAINT laboratories_lab_name_key UNIQUE (lab_name);


--
-- Name: laboratories laboratories_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.laboratories
    ADD CONSTRAINT laboratories_pkey PRIMARY KEY (lab_id);


--
-- Name: result_conditions result_conditions_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_conditions
    ADD CONSTRAINT result_conditions_pkey PRIMARY KEY (result_condition_id);


--
-- Name: result_conditions result_conditions_result_condition_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_conditions
    ADD CONSTRAINT result_conditions_result_condition_key UNIQUE (result_condition);


--
-- Name: result_speciations result_speciations_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_speciations
    ADD CONSTRAINT result_speciations_pkey PRIMARY KEY (result_speciation_id);


--
-- Name: result_speciations result_speciations_result_speciation_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_speciations
    ADD CONSTRAINT result_speciations_result_speciation_key UNIQUE (result_speciation);


--
-- Name: result_types result_types_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_types
    ADD CONSTRAINT result_types_pkey PRIMARY KEY (result_type_id);


--
-- Name: result_value_types result_value_types_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_value_types
    ADD CONSTRAINT result_value_types_pkey PRIMARY KEY (result_value_type_id);


--
-- Name: result_value_types result_value_types_result_value_type_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.result_value_types
    ADD CONSTRAINT result_value_types_result_value_type_key UNIQUE (result_value_type);


--
-- Name: results results_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_pkey PRIMARY KEY (result_id);


--
-- Name: results results_sample_id_parameter_id_sample_fraction_result_value_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_sample_id_parameter_id_sample_fraction_result_value_key UNIQUE NULLS NOT DISTINCT (sample_id, parameter_id, sample_fraction, result_value_type, result_speciation, protocol_method, laboratory, analysis_datetime);


--
-- Name: sample_fractions sample_fractions_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_fractions
    ADD CONSTRAINT sample_fractions_pkey PRIMARY KEY (sample_fraction_id);


--
-- Name: sample_fractions sample_fractions_sample_fraction_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_fractions
    ADD CONSTRAINT sample_fractions_sample_fraction_key UNIQUE (sample_fraction);


--
-- Name: sample_series sample_series_location_id_sub_location_id_synch_from_synch__key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_location_id_sub_location_id_synch_from_synch__key UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, synch_from, synch_to);


--
-- Name: sample_series sample_series_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_pkey PRIMARY KEY (sample_series_id);


--
-- Name: sample_types sample_types_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_types
    ADD CONSTRAINT sample_types_pkey PRIMARY KEY (sample_type_id);


--
-- Name: sample_types sample_types_sample_type_fr_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_types
    ADD CONSTRAINT sample_types_sample_type_fr_key UNIQUE (sample_type_fr);


--
-- Name: sample_types sample_types_sample_type_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_types
    ADD CONSTRAINT sample_types_sample_type_key UNIQUE (sample_type);


--
-- Name: samples samples_location_id_sub_location_id_media_id_z_datetime_sam_key; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_location_id_sub_location_id_media_id_z_datetime_sam_key UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, media_id, z, datetime, sample_type, collection_method);


--
-- Name: samples samples_pkey; Type: CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_pkey PRIMARY KEY (sample_id);


--
-- Name: document_types document_type_en_unique; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.document_types
    ADD CONSTRAINT document_type_en_unique UNIQUE (document_type_en);


--
-- Name: document_types document_type_fr_unique; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.document_types
    ADD CONSTRAINT document_type_fr_unique UNIQUE (document_type_fr);


--
-- Name: document_types document_types_pkey; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.document_types
    ADD CONSTRAINT document_types_pkey PRIMARY KEY (document_type_id);


--
-- Name: documents documents_name_key; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_name_key UNIQUE (name);


--
-- Name: documents documents_pkey; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_pkey PRIMARY KEY (document_id);


--
-- Name: documents_spatial documents_spatial_pkey; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents_spatial
    ADD CONSTRAINT documents_spatial_pkey PRIMARY KEY (document_id, geom_id);


--
-- Name: image_types image_types_pkey; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_types
    ADD CONSTRAINT image_types_pkey PRIMARY KEY (image_type_id);


--
-- Name: image_series images_index_pkey; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT images_index_pkey PRIMARY KEY (img_meta_id);


--
-- Name: images images_pkey; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_pkey PRIMARY KEY (image_id);


--
-- Name: documents unique_document_hash; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT unique_document_hash UNIQUE (file_hash);


--
-- Name: images unique_image_hash; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT unique_image_hash UNIQUE (file_hash);


--
-- Name: image_series unique_location; Type: CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT unique_location UNIQUE (location_id);


--
-- Name: internal_status internal_status_pkey; Type: CONSTRAINT; Schema: information; Owner: -
--

ALTER TABLE ONLY information.internal_status
    ADD CONSTRAINT internal_status_pkey PRIMARY KEY (event);


--
-- Name: version_info version_info_pkey; Type: CONSTRAINT; Schema: information; Owner: -
--

ALTER TABLE ONLY information.version_info
    ADD CONSTRAINT version_info_pkey PRIMARY KEY (id);


--
-- Name: array_maintenance_changes array_maintenance_changes_pk; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_pk PRIMARY KEY (event_id);


--
-- Name: calibrate_depth calibrate_depth_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_depth
    ADD CONSTRAINT calibrate_depth_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_dissolved_oxygen calibrate_dissolved_oxygen_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_dissolved_oxygen
    ADD CONSTRAINT calibrate_dissolved_oxygen_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_orp calibrate_orp_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_orp
    ADD CONSTRAINT calibrate_orp_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_ph calibrate_ph_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_ph
    ADD CONSTRAINT calibrate_ph_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_specific_conductance calibrate_specific_conductance_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_specific_conductance
    ADD CONSTRAINT calibrate_specific_conductance_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_temperature calibrate_temperature_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_temperature
    ADD CONSTRAINT calibrate_temperature_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_turbidity calibrate_turbidity_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_turbidity
    ADD CONSTRAINT calibrate_turbidity_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrations calibrations_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_pkey PRIMARY KEY (calibration_id);


--
-- Name: instrument_maintenance instrument_maintenance_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_maintenance
    ADD CONSTRAINT instrument_maintenance_pkey PRIMARY KEY (event_id);


--
-- Name: instrument_make instrument_make_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_make
    ADD CONSTRAINT instrument_make_pkey PRIMARY KEY (make_id);


--
-- Name: instrument_model instrument_model_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_model
    ADD CONSTRAINT instrument_model_pkey PRIMARY KEY (model_id);


--
-- Name: instrument_type instrument_type_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_type
    ADD CONSTRAINT instrument_type_pkey PRIMARY KEY (type_id);


--
-- Name: instruments instruments_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_pkey PRIMARY KEY (instrument_id);


--
-- Name: instruments instruments_serial_no_key; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_serial_no_key UNIQUE (serial_no);


--
-- Name: observers observers_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.observers
    ADD CONSTRAINT observers_pkey PRIMARY KEY (observer_id);


--
-- Name: observers observers_unique; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.observers
    ADD CONSTRAINT observers_unique UNIQUE (observer_first, observer_last, organization);


--
-- Name: sensor_types sensor_types_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.sensor_types
    ADD CONSTRAINT sensor_types_pkey PRIMARY KEY (sensor_type_id);


--
-- Name: sensors sensors_pkey; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.sensors
    ADD CONSTRAINT sensors_pkey PRIMARY KEY (sensor_id);


--
-- Name: sensors sensors_sensor_serial_key; Type: CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.sensors
    ADD CONSTRAINT sensors_sensor_serial_key UNIQUE (sensor_serial);


--
-- Name: approval_types approval_types_approval_type_code_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.approval_types
    ADD CONSTRAINT approval_types_approval_type_code_key UNIQUE (approval_type_code);


--
-- Name: approval_types approval_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.approval_types
    ADD CONSTRAINT approval_types_pkey PRIMARY KEY (approval_type_id);


--
-- Name: correction_types correction_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.correction_types
    ADD CONSTRAINT correction_types_pkey PRIMARY KEY (correction_type_id);


--
-- Name: correction_types correction_types_priority_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.correction_types
    ADD CONSTRAINT correction_types_priority_key UNIQUE (priority);


--
-- Name: datum_conversions datum_conversions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT datum_conversions_pkey PRIMARY KEY (conversion_id);


--
-- Name: datum_list datum_list_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.datum_list
    ADD CONSTRAINT datum_list_pkey PRIMARY KEY (datum_id);


--
-- Name: grade_types grade_types_grade_type_code_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.grade_types
    ADD CONSTRAINT grade_types_grade_type_code_key UNIQUE (grade_type_code);


--
-- Name: grade_types grade_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.grade_types
    ADD CONSTRAINT grade_types_pkey PRIMARY KEY (grade_type_id);


--
-- Name: location_types location_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_types
    ADD CONSTRAINT location_types_pkey PRIMARY KEY (type_id);


--
-- Name: location_types location_types_type_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.location_types
    ADD CONSTRAINT location_types_type_key UNIQUE (type);


--
-- Name: locations locations_id_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_id_pkey PRIMARY KEY (location_id);


--
-- Name: locations locations_location_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_location_key UNIQUE (location);


--
-- Name: locations_metadata_access locations_metadata_access_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_access locations_metadata_access_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwater_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwater_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwater_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwater_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_instruments locations_metadata_instruments_location_id_sub_location_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_location_id_sub_location_id_key UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_instruments locations_metadata_instruments_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_pkey PRIMARY KEY (metadata_id);


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_pkey PRIMARY KEY (location_maintenance_id);


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_xsections locations_metadata_xsections_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_xsections locations_metadata_xsections_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations locations_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_name_fr_key UNIQUE (name_fr);


--
-- Name: locations locations_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_name_key UNIQUE (name);


--
-- Name: network_project_types network_project_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.network_project_types
    ADD CONSTRAINT network_project_types_pkey PRIMARY KEY (id);


--
-- Name: locations_networks networks_locations_network_id_location_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_networks
    ADD CONSTRAINT networks_locations_network_id_location_id_key UNIQUE (network_id, location_id);


--
-- Name: networks networks_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_name_fr_key UNIQUE (name_fr);


--
-- Name: networks networks_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_name_key UNIQUE (name);


--
-- Name: networks networks_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_pkey PRIMARY KEY (network_id);


--
-- Name: organizations owners_contributors_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organizations
    ADD CONSTRAINT owners_contributors_name_key UNIQUE (name);


--
-- Name: organizations owners_contributors_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.organizations
    ADD CONSTRAINT owners_contributors_pkey PRIMARY KEY (organization_id);


--
-- Name: media_types param_type_param_type_fr_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.media_types
    ADD CONSTRAINT param_type_param_type_fr_key UNIQUE (media_type_fr);


--
-- Name: media_types param_type_param_type_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.media_types
    ADD CONSTRAINT param_type_param_type_key UNIQUE (media_type);


--
-- Name: media_types param_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.media_types
    ADD CONSTRAINT param_type_pkey PRIMARY KEY (media_id);


--
-- Name: parameter_groups parameter_groups_group_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_groups
    ADD CONSTRAINT parameter_groups_group_name_key UNIQUE (group_name);


--
-- Name: parameter_groups parameter_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_groups
    ADD CONSTRAINT parameter_groups_pkey PRIMARY KEY (group_id);


--
-- Name: parameter_relationships parameter_relationships_param_code_group_id_sub_group_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_param_code_group_id_sub_group_id_key UNIQUE NULLS NOT DISTINCT (parameter_id, group_id, sub_group_id);


--
-- Name: parameter_relationships parameter_relationships_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_pkey PRIMARY KEY (relationship_id);


--
-- Name: parameter_sub_groups parameter_sub_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_sub_groups
    ADD CONSTRAINT parameter_sub_groups_pkey PRIMARY KEY (sub_group_id);


--
-- Name: parameter_sub_groups parameter_sub_groups_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_sub_groups
    ADD CONSTRAINT parameter_sub_groups_unique UNIQUE (sub_group_name);


--
-- Name: parameter_sub_groups parameter_sub_groups_unique_1; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_sub_groups
    ADD CONSTRAINT parameter_sub_groups_unique_1 UNIQUE (sub_group_name_fr);


--
-- Name: parameters parameters_param_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT parameters_param_name_fr_key UNIQUE (param_name_fr);


--
-- Name: parameters parameters_param_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT parameters_param_name_key UNIQUE (param_name);


--
-- Name: parameters parameters_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT parameters_pkey PRIMARY KEY (parameter_id);


--
-- Name: locations_projects projects_locations_project_id_location_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_projects
    ADD CONSTRAINT projects_locations_project_id_location_id_key UNIQUE (project_id, location_id);


--
-- Name: projects projects_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_name_fr_key UNIQUE (name_fr);


--
-- Name: projects projects_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_name_key UNIQUE (name);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (project_id);


--
-- Name: qualifier_types qualifier_types_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.qualifier_types
    ADD CONSTRAINT qualifier_types_pkey PRIMARY KEY (qualifier_type_id);


--
-- Name: qualifier_types qualifier_types_qualifier_type_code_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.qualifier_types
    ADD CONSTRAINT qualifier_types_qualifier_type_code_key UNIQUE (qualifier_type_code);


--
-- Name: sub_locations sub_locations_location_id_sub_location_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sub_locations
    ADD CONSTRAINT sub_locations_location_id_sub_location_name_key UNIQUE (location_id, sub_location_name);


--
-- Name: sub_locations sub_locations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sub_locations
    ADD CONSTRAINT sub_locations_pkey PRIMARY KEY (sub_location_id);


--
-- Name: locations unique_location_id; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT unique_location_id UNIQUE (location_id);


--
-- Name: rasters enforce_max_extent_rast; Type: CHECK CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE spatial.rasters
    ADD CONSTRAINT enforce_max_extent_rast CHECK ((spatial.st_envelope(rast) OPERATOR(spatial.@) '0103000020826D0D000100000005000000E692DAB5BDB52DC02895726F51780D40E692DAB5BDB52DC03CDA4C0AD7BB3040D8145D137299FDBF3CDA4C0AD7BB3040D8145D137299FDBF2895726F51780D40E692DAB5BDB52DC02895726F51780D40'::spatial.geometry)) NOT VALID;


--
-- Name: raster_series_index raster_series_index_model_parameter_key; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.raster_series_index
    ADD CONSTRAINT raster_series_index_model_parameter_key UNIQUE (model, parameter);


--
-- Name: raster_series_index raster_series_index_pkey; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.raster_series_index
    ADD CONSTRAINT raster_series_index_pkey PRIMARY KEY (raster_series_id);


--
-- Name: rasters rasters_pkey; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters
    ADD CONSTRAINT rasters_pkey PRIMARY KEY (rid);


--
-- Name: rasters_reference rasters_reference_pkey; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters_reference
    ADD CONSTRAINT rasters_reference_pkey PRIMARY KEY (reference_id);


--
-- Name: rasters_reference rasters_reference_raster_series_id_flag_valid_from_valid_to_key; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters_reference
    ADD CONSTRAINT rasters_reference_raster_series_id_flag_valid_from_valid_to_key UNIQUE NULLS NOT DISTINCT (raster_series_id, flag, valid_from, valid_to, issued);


--
-- Name: vectors vectors_layer_name_feature_name_geom_type_key; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.vectors
    ADD CONSTRAINT vectors_layer_name_feature_name_geom_type_key UNIQUE (layer_name, feature_name, geom_type);


--
-- Name: vectors vectors_pkey; Type: CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.vectors
    ADD CONSTRAINT vectors_pkey PRIMARY KEY (geom_id);


--
-- Name: idx_approvals_timeseries_time; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX idx_approvals_timeseries_time ON continuous.approvals USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_contributors_timeseries_time; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX idx_contributors_timeseries_time ON continuous.contributors USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_corrections_timeseries_date_range; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX idx_corrections_timeseries_date_range ON continuous.corrections USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_grades_timeseries_time; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX idx_grades_timeseries_time ON continuous.grades USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_owners_timeseries_time; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX idx_owners_timeseries_time ON continuous.owners USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_qualifiers_timeseries_time; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX idx_qualifiers_timeseries_time ON continuous.qualifiers USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: measurements_calculated_daily_date_idx; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX measurements_calculated_daily_date_idx ON continuous.measurements_calculated_daily USING btree (date);


--
-- Name: measurements_continuous_datetime_idx; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX measurements_continuous_datetime_idx ON continuous.measurements_continuous USING btree (datetime);


--
-- Name: timeseries_share_with_gin_idx; Type: INDEX; Schema: continuous; Owner: -
--

CREATE INDEX timeseries_share_with_gin_idx ON continuous.timeseries USING gin (share_with);


--
-- Name: idx_results_parameter_id; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_results_parameter_id ON discrete.results USING btree (parameter_id);


--
-- Name: idx_results_result_condition; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_results_result_condition ON discrete.results USING btree (result_condition);


--
-- Name: idx_results_sample_id; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_results_sample_id ON discrete.results USING btree (sample_id);


--
-- Name: idx_results_sample_parameter; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_results_sample_parameter ON discrete.results USING btree (sample_id, parameter_id);


--
-- Name: idx_samples_datetime; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_samples_datetime ON discrete.samples USING btree (datetime);


--
-- Name: idx_samples_location_id; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_samples_location_id ON discrete.samples USING btree (location_id);


--
-- Name: idx_samples_media_id; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX idx_samples_media_id ON discrete.samples USING btree (media_id);


--
-- Name: results_share_with_gin_idx; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX results_share_with_gin_idx ON discrete.results USING gin (share_with);


--
-- Name: samples_share_with_gin_idx; Type: INDEX; Schema: discrete; Owner: -
--

CREATE INDEX samples_share_with_gin_idx ON discrete.samples USING gin (share_with);


--
-- Name: documents_share_with_gin_idx; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX documents_share_with_gin_idx ON files.documents USING gin (share_with);


--
-- Name: documents_type_idx; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX documents_type_idx ON files.documents USING btree (type);


--
-- Name: idx_documents_tags; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX idx_documents_tags ON files.documents USING gin (tags);


--
-- Name: idx_images_tags; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX idx_images_tags ON files.images USING gin (tags);


--
-- Name: images_datetime_idx; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX images_datetime_idx ON files.images USING btree (datetime);


--
-- Name: images_index_share_with_gin_idx; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX images_index_share_with_gin_idx ON files.image_series USING gin (share_with);


--
-- Name: images_location_id_idx; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX images_location_id_idx ON files.images USING btree (location_id);


--
-- Name: images_share_with_gin_idx; Type: INDEX; Schema: files; Owner: -
--

CREATE INDEX images_share_with_gin_idx ON files.images USING gin (share_with);


--
-- Name: locations_share_with_gin_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX locations_share_with_gin_idx ON public.locations USING gin (share_with);


--
-- Name: sub_locations_share_with_gin_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX sub_locations_share_with_gin_idx ON public.sub_locations USING gin (share_with);


--
-- Name: geometry_idx; Type: INDEX; Schema: spatial; Owner: -
--

CREATE INDEX geometry_idx ON spatial.vectors USING gist (geom);


--
-- Name: rasters_rast_st_conhull_idx; Type: INDEX; Schema: spatial; Owner: -
--

CREATE INDEX rasters_rast_st_conhull_idx ON spatial.rasters USING gist (spatial.st_convexhull(rast));


--
-- Name: rasters_reference_raster_series_id_idx; Type: INDEX; Schema: spatial; Owner: -
--

CREATE INDEX rasters_reference_raster_series_id_idx ON spatial.rasters_reference USING btree (raster_series_id);


--
-- Name: rasters_reference_valid_from_idx; Type: INDEX; Schema: spatial; Owner: -
--

CREATE INDEX rasters_reference_valid_from_idx ON spatial.rasters_reference USING btree (valid_from);


--
-- Name: rasters_reference_valid_to_idx; Type: INDEX; Schema: spatial; Owner: -
--

CREATE INDEX rasters_reference_valid_to_idx ON spatial.rasters_reference USING btree (valid_to);


--
-- Name: page_content trg_page_content_integrity; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER trg_page_content_integrity BEFORE INSERT OR UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.check_page_content_integrity();


--
-- Name: images trg_user_audit; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON application.images FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: page_content trg_user_audit; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: text trg_user_audit; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON application.text FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: images update_image_modified; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER update_image_modified BEFORE UPDATE ON application.images FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: page_content update_page_content_modified; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER update_page_content_modified BEFORE UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: text update_text_modified; Type: TRIGGER; Schema: application; Owner: -
--

CREATE TRIGGER update_text_modified BEFORE UPDATE ON application.text FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: approvals check_approvals_overlap; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE CONSTRAINT TRIGGER check_approvals_overlap AFTER INSERT OR UPDATE ON continuous.approvals DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_approvals_overlap();


--
-- Name: contributors check_contributors_overlap; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE CONSTRAINT TRIGGER check_contributors_overlap AFTER INSERT OR UPDATE ON continuous.contributors DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_contributors_overlap();


--
-- Name: grades check_grades_overlap; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE CONSTRAINT TRIGGER check_grades_overlap AFTER INSERT OR UPDATE ON continuous.grades DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_grades_overlap();


--
-- Name: owners check_owners_overlap; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE CONSTRAINT TRIGGER check_owners_overlap AFTER INSERT OR UPDATE ON continuous.owners DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_owners_overlap();


--
-- Name: qualifiers check_qualifiers_overlap; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE CONSTRAINT TRIGGER check_qualifiers_overlap AFTER INSERT OR UPDATE ON continuous.qualifiers DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_qualifiers_overlap();


--
-- Name: corrections trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.corrections FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: extrema trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.extrema FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: thresholds trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.thresholds FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: approvals update_approvals_updated; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_approvals_updated BEFORE UPDATE ON continuous.approvals FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: contributors update_contributors_updated; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_contributors_updated BEFORE UPDATE ON continuous.contributors FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: corrections update_corrections_updated; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_corrections_updated BEFORE UPDATE ON continuous.corrections FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: grades update_grades_updated; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_grades_updated BEFORE UPDATE ON continuous.grades FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: measurements_calculated_daily update_measurements_calculated_daily_created_modified; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_measurements_calculated_daily_created_modified BEFORE INSERT OR UPDATE ON continuous.measurements_calculated_daily FOR EACH ROW EXECUTE FUNCTION public.update_created_modified();


--
-- Name: measurements_continuous update_measurements_continuous_created_modified; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_measurements_continuous_created_modified BEFORE INSERT OR UPDATE ON continuous.measurements_continuous FOR EACH ROW EXECUTE FUNCTION public.update_created_modified();


--
-- Name: owners update_owners_updated; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_owners_updated BEFORE UPDATE ON continuous.owners FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: qualifiers update_qualifiers_updated; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_qualifiers_updated BEFORE UPDATE ON continuous.qualifiers FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: rating_curve_shifts update_rating_curve_shifts_modified; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_rating_curve_shifts_modified BEFORE UPDATE ON continuous.rating_curve_shifts FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: rating_curves update_rating_curves_modified; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER update_rating_curves_modified BEFORE UPDATE ON continuous.rating_curves FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: corrections validate_corrections_trigger; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER validate_corrections_trigger BEFORE INSERT OR UPDATE ON continuous.corrections FOR EACH ROW EXECUTE FUNCTION continuous.validate_corrections();


--
-- Name: timeseries validate_share_with_trigger; Type: TRIGGER; Schema: continuous; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON continuous.timeseries FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: results trg_enforce_result_speciation; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_enforce_result_speciation BEFORE INSERT OR UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION discrete.enforce_result_speciation();


--
-- Name: results trg_enforce_sample_fraction; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_enforce_sample_fraction BEFORE INSERT OR UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION discrete.enforce_sample_fraction();


--
-- Name: collection_methods trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.collection_methods FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: laboratories trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.laboratories FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: protocols_methods trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.protocols_methods FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_conditions trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_conditions FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_speciations trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_speciations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_types trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_value_types trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_value_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: results trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_fractions trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.sample_fractions FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_series trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.sample_series FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_types trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.sample_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: samples trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_series trigger_check_sample_series_overlap; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER trigger_check_sample_series_overlap BEFORE INSERT OR UPDATE ON discrete.sample_series FOR EACH ROW EXECUTE FUNCTION discrete.check_sample_series_overlap();


--
-- Name: results update_results_modified; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER update_results_modified BEFORE UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: samples update_samples_modified; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER update_samples_modified BEFORE UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: samples validate_documents_trigger; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER validate_documents_trigger BEFORE INSERT OR UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.validate_documents_array();


--
-- Name: guideline_values validate_guideline_start_end_trg; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER validate_guideline_start_end_trg BEFORE INSERT OR UPDATE ON discrete.guideline_values FOR EACH ROW EXECUTE FUNCTION public.validate_guideline_start_end();


--
-- Name: guideline_values validate_guideline_values_trg; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER validate_guideline_values_trg BEFORE INSERT OR UPDATE ON discrete.guideline_values FOR EACH ROW EXECUTE FUNCTION public.validate_guideline_values();


--
-- Name: results validate_share_with_trigger; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: samples validate_share_with_trigger; Type: TRIGGER; Schema: discrete; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: documents_spatial documents_spatial_after_delete; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER documents_spatial_after_delete AFTER DELETE ON files.documents_spatial FOR EACH ROW EXECUTE FUNCTION files.update_document_flags_after_delete();


--
-- Name: documents_spatial documents_spatial_after_insert; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER documents_spatial_after_insert AFTER INSERT ON files.documents_spatial FOR EACH ROW EXECUTE FUNCTION files.update_document_flags_after_insert();


--
-- Name: document_types trg_user_audit; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.document_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: documents trg_user_audit; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.documents FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: documents_spatial trg_user_audit; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.documents_spatial FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: image_series trg_user_audit; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.image_series FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: image_types trg_user_audit; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.image_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: images trg_user_audit; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.images FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: images trigger_enforce_share_with_restriction; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER trigger_enforce_share_with_restriction BEFORE INSERT OR UPDATE ON files.images FOR EACH ROW EXECUTE FUNCTION files.enforce_share_with_restriction();


--
-- Name: documents validate_share_with_trigger; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON files.documents FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: image_series validate_share_with_trigger; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON files.image_series FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: images validate_share_with_trigger; Type: TRIGGER; Schema: files; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON files.images FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: version_info update_version_info_created_modified; Type: TRIGGER; Schema: information; Owner: -
--

CREATE TRIGGER update_version_info_created_modified BEFORE INSERT OR UPDATE ON information.version_info FOR EACH ROW EXECUTE FUNCTION public.update_created_modified();


--
-- Name: array_maintenance_changes trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.array_maintenance_changes FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_depth trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_depth FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_dissolved_oxygen trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_dissolved_oxygen FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_orp trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_orp FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_ph trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_ph FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_specific_conductance trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_specific_conductance FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_temperature trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_temperature FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_turbidity trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_turbidity FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrations trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_maintenance trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_maintenance FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_make trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_make FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_model trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_model FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_type trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_type FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instruments trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instruments FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: observers trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.observers FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sensor_types trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.sensor_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sensors trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.sensors FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: array_maintenance_changes update_array_maintenance_changes_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_array_maintenance_changes_modify_datetime BEFORE UPDATE ON instruments.array_maintenance_changes FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_depth update_calibrate_depth_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_depth_modify_datetime BEFORE UPDATE ON instruments.calibrate_depth FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_dissolved_oxygen update_calibrate_dissolved_oxygen_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_dissolved_oxygen_modify_datetime BEFORE UPDATE ON instruments.calibrate_dissolved_oxygen FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_orp update_calibrate_orp_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_orp_modify_datetime BEFORE UPDATE ON instruments.calibrate_orp FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_ph update_calibrate_ph_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_ph_modify_datetime BEFORE UPDATE ON instruments.calibrate_ph FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_specific_conductance update_calibrate_specific_conductance_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_specific_conductance_modify_datetime BEFORE UPDATE ON instruments.calibrate_specific_conductance FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_temperature update_calibrate_temperature_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_temperature_modify_datetime BEFORE UPDATE ON instruments.calibrate_temperature FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_turbidity update_calibrate_turbidity_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrate_turbidity_modify_datetime BEFORE UPDATE ON instruments.calibrate_turbidity FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrations update_calibrations_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_calibrations_modify_datetime BEFORE UPDATE ON instruments.calibrations FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_maintenance update_instrument_maintenance_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_instrument_maintenance_modify_datetime BEFORE UPDATE ON instruments.instrument_maintenance FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_make update_instrument_make_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_instrument_make_modify_datetime BEFORE UPDATE ON instruments.instrument_make FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_model update_instrument_model_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_instrument_model_modify_datetime BEFORE UPDATE ON instruments.instrument_model FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_type update_instrument_type_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_instrument_type_modify_datetime BEFORE UPDATE ON instruments.instrument_type FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instruments update_instruments_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_instruments_modify_datetime BEFORE UPDATE ON instruments.instruments FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: observers update_observers_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_observers_modify_datetime BEFORE UPDATE ON instruments.observers FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: sensor_types update_sensor_types_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_sensor_types_modify_datetime BEFORE UPDATE ON instruments.sensor_types FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: sensors update_sensors_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: -
--

CREATE TRIGGER update_sensors_modify_datetime BEFORE UPDATE ON instruments.sensors FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: locations_metadata_instruments check_instrument_meta_overlap; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER check_instrument_meta_overlap AFTER INSERT OR UPDATE ON public.locations_metadata_instruments DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.check_instrument_meta_overlap();


--
-- Name: locations_metadata_instruments check_instruments_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER check_instruments_trigger BEFORE INSERT OR UPDATE ON public.locations_metadata_instruments FOR EACH ROW EXECUTE FUNCTION public.check_instruments_reference();


--
-- Name: locations_metadata_access fill_locations_metadata_access_missing_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER fill_locations_metadata_access_missing_trigger BEFORE INSERT ON public.locations_metadata_access FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_access_missing();


--
-- Name: locations_metadata_infrastructure_groundwater fill_locations_metadata_infrastructure_groundwater_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER fill_locations_metadata_infrastructure_groundwater_trigger BEFORE INSERT ON public.locations_metadata_infrastructure_groundwater FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_groundwater();


--
-- Name: locations_metadata_infrastructure_hydromet fill_locations_metadata_infrastructure_hydromet_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER fill_locations_metadata_infrastructure_hydromet_trigger BEFORE INSERT ON public.locations_metadata_infrastructure_hydromet FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_hydromet();


--
-- Name: locations_metadata_infrastructure fill_locations_metadata_infrastructure_missing_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER fill_locations_metadata_infrastructure_missing_trigger BEFORE INSERT ON public.locations_metadata_infrastructure FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_missing();


--
-- Name: locations_metadata_owners_operators fill_locations_metadata_owners_operators_missing_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER fill_locations_metadata_owners_operators_missing_trigger BEFORE INSERT ON public.locations_metadata_owners_operators FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_owners_operators_missing();


--
-- Name: locations trg_check_data_sharing_agreement; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_check_data_sharing_agreement BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION files.check_data_sharing_agreement();


--
-- Name: approval_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.approval_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: correction_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.correction_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: datum_conversions trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.datum_conversions FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: datum_list trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.datum_list FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: grade_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.grade_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: location_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.location_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_access trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_access FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_infrastructure trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_infrastructure FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_infrastructure_groundwater trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_infrastructure_groundwater FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_infrastructure_hydromet trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_infrastructure_hydromet FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_instruments trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_instruments FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_maintenance trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_maintenance FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_owners_operators trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_owners_operators FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_xsections trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_xsections FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_networks trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_networks FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_projects trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_projects FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: media_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.media_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: network_project_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.network_project_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: networks trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.networks FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: organizations trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.organizations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: parameter_groups trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.parameter_groups FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: parameter_sub_groups trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.parameter_sub_groups FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: parameters trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.parameters FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: projects trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.projects FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: qualifier_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.qualifier_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sub_locations trg_user_audit; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.sub_locations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations trigger_check_location_images; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trigger_check_location_images BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION files.check_location_images();


--
-- Name: locations_metadata_maintenance trigger_enforce_maintenance_constraints; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trigger_enforce_maintenance_constraints BEFORE INSERT OR UPDATE ON public.locations_metadata_maintenance FOR EACH ROW EXECUTE FUNCTION public.enforce_maintenance_constraints();


--
-- Name: locations_metadata_access update_locations_metadata_access_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_access_modified BEFORE UPDATE ON public.locations_metadata_access FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_infrastructure_groundwater update_locations_metadata_infrastructure_groundwater_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_infrastructure_groundwater_modified BEFORE UPDATE ON public.locations_metadata_infrastructure_groundwater FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_infrastructure_hydromet update_locations_metadata_infrastructure_hydromet_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_infrastructure_hydromet_modified BEFORE UPDATE ON public.locations_metadata_infrastructure_hydromet FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_infrastructure update_locations_metadata_infrastructure_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_infrastructure_modified BEFORE UPDATE ON public.locations_metadata_infrastructure FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_instruments update_locations_metadata_instruments_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_instruments_modified BEFORE UPDATE ON public.locations_metadata_instruments FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_owners_operators update_locations_metadata_owners_operators_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_owners_operators_modified BEFORE UPDATE ON public.locations_metadata_owners_operators FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_xsections update_locations_metadata_xsections_modified; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER update_locations_metadata_xsections_modified BEFORE UPDATE ON public.locations_metadata_xsections FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations validate_share_with_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: sub_locations validate_share_with_trigger; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON public.sub_locations FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: vectors update_geom_type_trigger; Type: TRIGGER; Schema: spatial; Owner: -
--

CREATE TRIGGER update_geom_type_trigger BEFORE INSERT OR UPDATE ON spatial.vectors FOR EACH ROW EXECUTE FUNCTION spatial.update_geom_type();


--
-- Name: approvals approvals_approval_type_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.approvals
    ADD CONSTRAINT approvals_approval_type_id_fkey FOREIGN KEY (approval_type_id) REFERENCES public.approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: approvals approvals_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.approvals
    ADD CONSTRAINT approvals_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contributors contributors_owner_contributor_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.contributors
    ADD CONSTRAINT contributors_owner_contributor_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contributors contributors_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.contributors
    ADD CONSTRAINT contributors_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: corrections corrections_correction_type_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_correction_type_fkey FOREIGN KEY (correction_type) REFERENCES public.correction_types(correction_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: corrections corrections_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_aggregation_type; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_aggregation_type FOREIGN KEY (aggregation_type_id) REFERENCES continuous.aggregation_types(aggregation_type_id);


--
-- Name: timeseries fk_location; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_location FOREIGN KEY (location) REFERENCES public.locations(location) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_location_id; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_location_id FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_media_type; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_media_type FOREIGN KEY (media_id) REFERENCES public.media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_parameter; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_parameter FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: measurements_continuous fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.measurements_continuous
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: measurements_calculated_daily fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.measurements_calculated_daily
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: extrema fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.extrema
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: forecasts fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.forecasts
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: thresholds fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.thresholds
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: grades grades_grade_type_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.grades
    ADD CONSTRAINT grades_grade_type_id_fkey FOREIGN KEY (grade_type_id) REFERENCES public.grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: grades grades_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.grades
    ADD CONSTRAINT grades_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: owners owners_owner_contributor_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.owners
    ADD CONSTRAINT owners_owner_contributor_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: owners owners_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.owners
    ADD CONSTRAINT owners_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: qualifiers qualifiers_qualifier_type_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.qualifiers
    ADD CONSTRAINT qualifiers_qualifier_type_id_fkey FOREIGN KEY (qualifier_type_id) REFERENCES public.qualifier_types(qualifier_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: qualifiers qualifiers_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.qualifiers
    ADD CONSTRAINT qualifiers_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rating_curve_points rating_curve_points_curve_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_points
    ADD CONSTRAINT rating_curve_points_curve_id_fkey FOREIGN KEY (curve_id) REFERENCES continuous.rating_curves(curve_id);


--
-- Name: rating_curve_shifts rating_curve_shifts_curve_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_curve_id_fkey FOREIGN KEY (curve_id) REFERENCES continuous.rating_curves(curve_id);


--
-- Name: rating_curves rating_curves_approval_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_approval_fkey FOREIGN KEY (approval) REFERENCES public.approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rating_curves rating_curves_input_parameter_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_input_parameter_id_fkey FOREIGN KEY (input_parameter_id) REFERENCES public.parameters(parameter_id);


--
-- Name: rating_curves rating_curves_location_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: rating_curves rating_curves_output_parameter_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_output_parameter_id_fkey FOREIGN KEY (output_parameter_id) REFERENCES public.parameters(parameter_id);


--
-- Name: timeseries timeseries_default_owner_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_default_owner_fkey FOREIGN KEY (default_owner) REFERENCES public.organizations(organization_id);


--
-- Name: timeseries timeseries_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: -
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: results results_laboratory_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_laboratory_fkey FOREIGN KEY (laboratory) REFERENCES discrete.laboratories(lab_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_parameter_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_parameter_id_fkey FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: results results_protocol_method_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_protocol_method_fkey FOREIGN KEY (protocol_method) REFERENCES discrete.protocols_methods(protocol_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_result_condition_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_condition_fkey FOREIGN KEY (result_condition) REFERENCES discrete.result_conditions(result_condition_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: results results_result_speciation_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_speciation_fkey FOREIGN KEY (result_speciation) REFERENCES discrete.result_speciations(result_speciation_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_result_type_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_type_fkey FOREIGN KEY (result_type) REFERENCES discrete.result_types(result_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: results results_result_value_type_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_value_type_fkey FOREIGN KEY (result_value_type) REFERENCES discrete.result_value_types(result_value_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_sample_fraction_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_sample_fraction_fkey FOREIGN KEY (sample_fraction) REFERENCES discrete.sample_fractions(sample_fraction_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_sample_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_sample_id_fkey FOREIGN KEY (sample_id) REFERENCES discrete.samples(sample_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: sample_series sample_series_default_contributor_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_default_contributor_fkey FOREIGN KEY (default_contributor) REFERENCES public.organizations(organization_id);


--
-- Name: sample_series sample_series_default_owner_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_default_owner_fkey FOREIGN KEY (default_owner) REFERENCES public.organizations(organization_id);


--
-- Name: sample_series sample_series_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: sample_series sample_series_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: samples samples_collection_method_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_collection_method_fkey FOREIGN KEY (collection_method) REFERENCES discrete.collection_methods(collection_method_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_comissioning_org_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_comissioning_org_fkey FOREIGN KEY (comissioning_org) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_contributor_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_contributor_fkey FOREIGN KEY (contributor) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_linked_with_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_linked_with_fkey FOREIGN KEY (linked_with) REFERENCES discrete.samples(sample_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_media_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_media_id_fkey FOREIGN KEY (media_id) REFERENCES public.media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_owner_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_sample_approval_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_approval_fkey FOREIGN KEY (sample_approval) REFERENCES public.approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sample_grade_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_grade_fkey FOREIGN KEY (sample_grade) REFERENCES public.grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sample_qualifier_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_qualifier_fkey FOREIGN KEY (sample_qualifier) REFERENCES public.qualifier_types(qualifier_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sample_type_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_type_fkey FOREIGN KEY (sample_type) REFERENCES discrete.sample_types(sample_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_sampling_org_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sampling_org_fkey FOREIGN KEY (sampling_org) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: -
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: documents documents_contributor_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_contributor_fkey FOREIGN KEY (contributor) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: documents documents_owner_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: documents_spatial documents_spatial_document_id_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents_spatial
    ADD CONSTRAINT documents_spatial_document_id_fkey FOREIGN KEY (document_id) REFERENCES files.documents(document_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: documents_spatial documents_spatial_geom_id_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents_spatial
    ADD CONSTRAINT documents_spatial_geom_id_fkey FOREIGN KEY (geom_id) REFERENCES spatial.vectors(geom_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: documents documents_type_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_type_fkey FOREIGN KEY (type) REFERENCES files.document_types(document_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: images fk_img_meta_id; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT fk_img_meta_id FOREIGN KEY (img_meta_id) REFERENCES files.image_series(img_meta_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: image_series fk_location_id; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT fk_location_id FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: images images_contributor_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_contributor_fkey FOREIGN KEY (contributor) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: images images_image_type_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_image_type_fkey FOREIGN KEY (image_type) REFERENCES files.image_types(image_type_id);


--
-- Name: image_series images_index_owner_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT images_index_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: images images_location_id_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: images images_owner_fkey; Type: FK CONSTRAINT; Schema: files; Owner: -
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: array_maintenance_changes array_maintenance_changes_instrument_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_instrument_id_fkey FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor1_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor1_id_fkey FOREIGN KEY (sensor1_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor2_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor2_id_fkey FOREIGN KEY (sensor2_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor3_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor3_id_fkey FOREIGN KEY (sensor3_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor4_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor4_id_fkey FOREIGN KEY (sensor4_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor5_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor5_id_fkey FOREIGN KEY (sensor5_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor6_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor6_id_fkey FOREIGN KEY (sensor6_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor7_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor7_id_fkey FOREIGN KEY (sensor7_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor8_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor8_id_fkey FOREIGN KEY (sensor8_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_depth calibrate_depth_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_depth
    ADD CONSTRAINT calibrate_depth_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_dissolved_oxygen calibrate_dissolved_oxygen_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_dissolved_oxygen
    ADD CONSTRAINT calibrate_dissolved_oxygen_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_orp calibrate_orp_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_orp
    ADD CONSTRAINT calibrate_orp_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_ph calibrate_ph_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_ph
    ADD CONSTRAINT calibrate_ph_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_specific_conductance calibrate_specific_conductance_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_specific_conductance
    ADD CONSTRAINT calibrate_specific_conductance_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_temperature calibrate_temperature_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_temperature
    ADD CONSTRAINT calibrate_temperature_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_turbidity calibrate_turbidity_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrate_turbidity
    ADD CONSTRAINT calibrate_turbidity_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrations calibrations_id_handheld_meter_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_id_handheld_meter_fkey FOREIGN KEY (id_handheld_meter) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrations calibrations_id_sensor_holder_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_id_sensor_holder_fkey FOREIGN KEY (id_sensor_holder) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrations calibrations_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instrument_maintenance instrument_maintenance_instrument_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_maintenance
    ADD CONSTRAINT instrument_maintenance_instrument_id_fkey FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instrument_maintenance instrument_maintenance_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instrument_maintenance
    ADD CONSTRAINT instrument_maintenance_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_make_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_make_fkey FOREIGN KEY (make) REFERENCES instruments.instrument_make(make_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_model_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_model_fkey FOREIGN KEY (model) REFERENCES instruments.instrument_model(model_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_type_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_type_fkey FOREIGN KEY (type) REFERENCES instruments.instrument_type(type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: sensors sensors_sensor_type_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: -
--

ALTER TABLE ONLY instruments.sensors
    ADD CONSTRAINT sensors_sensor_type_fkey FOREIGN KEY (sensor_type) REFERENCES instruments.sensor_types(sensor_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: datum_conversions datum_conversions_datum_id_from_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT datum_conversions_datum_id_from_fkey FOREIGN KEY (datum_id_from) REFERENCES public.datum_list(datum_id);


--
-- Name: datum_conversions datum_conversions_datum_id_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT datum_conversions_datum_id_to_fkey FOREIGN KEY (datum_id_to) REFERENCES public.datum_list(datum_id);


--
-- Name: locations fk_geom_id; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT fk_geom_id FOREIGN KEY (geom_id) REFERENCES spatial.vectors(geom_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: datum_conversions fk_location_id; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT fk_location_id FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations fk_location_type; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT fk_location_type FOREIGN KEY (location_type) REFERENCES public.location_types(type_id) ON UPDATE CASCADE;


--
-- Name: locations locations_data_sharing_agreement_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_data_sharing_agreement_id_fkey FOREIGN KEY (data_sharing_agreement_id) REFERENCES files.documents(document_id);


--
-- Name: locations_metadata_access locations_metadata_access_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_access locations_metadata_access_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwa_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwa_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwater_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwater_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_instruments locations_metadata_instruments_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_instruments locations_metadata_instruments_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_operator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_operator_fkey FOREIGN KEY (operator) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_owner_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_xsections locations_metadata_xsections_grade_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_grade_fkey FOREIGN KEY (measurement_grade) REFERENCES public.grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_instrument_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_instrument_fkey FOREIGN KEY (instrument) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_networks networks_locations_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_networks
    ADD CONSTRAINT networks_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: locations_networks networks_locations_network_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_networks
    ADD CONSTRAINT networks_locations_network_id_fkey FOREIGN KEY (network_id) REFERENCES public.networks(network_id);


--
-- Name: networks networks_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_type_fkey FOREIGN KEY (type) REFERENCES public.network_project_types(id);


--
-- Name: parameter_relationships parameter_relationships_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_group_id_fkey FOREIGN KEY (group_id) REFERENCES public.parameter_groups(group_id);


--
-- Name: parameter_relationships parameter_relationships_param_code_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_param_code_fkey FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id);


--
-- Name: parameter_relationships parameter_relationships_sub_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_sub_group_id_fkey FOREIGN KEY (sub_group_id) REFERENCES public.parameter_sub_groups(sub_group_id);


--
-- Name: locations_projects projects_locations_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_projects
    ADD CONSTRAINT projects_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: locations_projects projects_locations_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.locations_projects
    ADD CONSTRAINT projects_locations_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.projects(project_id);


--
-- Name: projects projects_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_type_fkey FOREIGN KEY (type) REFERENCES public.network_project_types(id);


--
-- Name: sub_locations sub_locations_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sub_locations
    ADD CONSTRAINT sub_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rasters_reference fk_raster_series_id; Type: FK CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters_reference
    ADD CONSTRAINT fk_raster_series_id FOREIGN KEY (raster_series_id) REFERENCES spatial.raster_series_index(raster_series_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rasters fk_reference_id; Type: FK CONSTRAINT; Schema: spatial; Owner: -
--

ALTER TABLE ONLY spatial.rasters
    ADD CONSTRAINT fk_reference_id FOREIGN KEY (reference_id) REFERENCES spatial.rasters_reference(reference_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries rls; Type: POLICY; Schema: continuous; Owner: -
--

CREATE POLICY rls ON continuous.timeseries USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(timeseries.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(timeseries.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: timeseries; Type: ROW SECURITY; Schema: continuous; Owner: -
--

ALTER TABLE continuous.timeseries ENABLE ROW LEVEL SECURITY;

--
-- Name: results; Type: ROW SECURITY; Schema: discrete; Owner: -
--

ALTER TABLE discrete.results ENABLE ROW LEVEL SECURITY;

--
-- Name: results rls; Type: POLICY; Schema: discrete; Owner: -
--

CREATE POLICY rls ON discrete.results USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(results.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(results.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: samples rls; Type: POLICY; Schema: discrete; Owner: -
--

CREATE POLICY rls ON discrete.samples USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(samples.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(samples.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: samples; Type: ROW SECURITY; Schema: discrete; Owner: -
--

ALTER TABLE discrete.samples ENABLE ROW LEVEL SECURITY;

--
-- Name: documents; Type: ROW SECURITY; Schema: files; Owner: -
--

ALTER TABLE files.documents ENABLE ROW LEVEL SECURITY;

--
-- Name: image_series; Type: ROW SECURITY; Schema: files; Owner: -
--

ALTER TABLE files.image_series ENABLE ROW LEVEL SECURITY;

--
-- Name: images; Type: ROW SECURITY; Schema: files; Owner: -
--

ALTER TABLE files.images ENABLE ROW LEVEL SECURITY;

--
-- Name: documents rls; Type: POLICY; Schema: files; Owner: -
--

CREATE POLICY rls ON files.documents USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(documents.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(documents.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: image_series rls; Type: POLICY; Schema: files; Owner: -
--

CREATE POLICY rls ON files.image_series USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(image_series.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(image_series.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: images rls; Type: POLICY; Schema: files; Owner: -
--

CREATE POLICY rls ON files.images USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(images.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(images.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: locations; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.locations ENABLE ROW LEVEL SECURITY;

--
-- Name: locations rls; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY rls ON public.locations USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(locations.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(locations.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: sub_locations rls; Type: POLICY; Schema: public; Owner: -
--

CREATE POLICY rls ON public.sub_locations USING ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(sub_locations.share_with) g(g)
  WHERE public.user_in_group(g.g))))) WITH CHECK ((('public_reader'::text = ANY (share_with)) OR (EXISTS ( SELECT 1
   FROM unnest(sub_locations.share_with) g(g)
  WHERE public.user_in_group(g.g)))));


--
-- Name: sub_locations; Type: ROW SECURITY; Schema: public; Owner: -
--

ALTER TABLE public.sub_locations ENABLE ROW LEVEL SECURITY;

--
-- PostgreSQL database dump complete
--

