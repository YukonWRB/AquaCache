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
-- Name: application; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA application;


ALTER SCHEMA application OWNER TO postgres;

--
-- Name: SCHEMA application; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA application IS 'Schema to hold application-related data, such as text and images which need frequent updates, metrics such as number of viewers, plots generated, etc.';


--
-- Name: boreholes; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA boreholes;


ALTER SCHEMA boreholes OWNER TO postgres;

--
-- Name: continuous; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA continuous;


ALTER SCHEMA continuous OWNER TO postgres;

--
-- Name: SCHEMA continuous; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA continuous IS 'Schema to hold continuous data and associated metadata.';


--
-- Name: discrete; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA discrete;


ALTER SCHEMA discrete OWNER TO postgres;

--
-- Name: SCHEMA discrete; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA discrete IS 'Schema to hold discrete data and associated metadata.';


--
-- Name: field; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA field;


ALTER SCHEMA field OWNER TO postgres;

--
-- Name: files; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA files;


ALTER SCHEMA files OWNER TO postgres;

--
-- Name: SCHEMA files; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA files IS 'Schema to hold files and associated metadata.';


--
-- Name: information; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA information;


ALTER SCHEMA information OWNER TO postgres;

--
-- Name: instruments; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA instruments;


ALTER SCHEMA instruments OWNER TO postgres;

--
-- Name: spatial; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA spatial;


ALTER SCHEMA spatial OWNER TO postgres;

--
-- Name: btree_gist; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS btree_gist WITH SCHEMA public;


--
-- Name: EXTENSION btree_gist; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION btree_gist IS 'support for indexing common datatypes in GiST';


--
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA spatial;


--
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry and geography spatial types and functions';


--
-- Name: postgis_raster; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis_raster WITH SCHEMA spatial;


--
-- Name: EXTENSION postgis_raster; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis_raster IS 'PostGIS raster types and functions';


--
-- Name: prevent_geology_overlap(); Type: FUNCTION; Schema: boreholes; Owner: postgres
--

CREATE FUNCTION boreholes.prevent_geology_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
      DECLARE
    conflict_count INTEGER;
    BEGIN
    SELECT 1
    INTO conflict_count
    FROM boreholes.geology g
    WHERE g.borehole_id    = NEW.borehole_id
    AND NEW.depth_from_m < g.depth_to_m
    AND NEW.depth_to_m   > g.depth_from_m
    -- on UPDATE skip comparing against itself
    AND (TG_OP = 'INSERT' OR g.geo_record_id <> NEW.geo_record_id)
    LIMIT 1;
    
    IF FOUND THEN
    RAISE EXCEPTION
    'Cannot add geology [% – %] for borehole %: overlaps existing interval',
    NEW.depth_from_m, NEW.depth_to_m, NEW.borehole_id;
    END IF;
    
    RETURN NEW;
    END;
    $$;


ALTER FUNCTION boreholes.prevent_geology_overlap() OWNER TO postgres;

--
-- Name: prevent_permafrost_overlap(); Type: FUNCTION; Schema: boreholes; Owner: postgres
--

CREATE FUNCTION boreholes.prevent_permafrost_overlap() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
      DECLARE
    conflict_count INTEGER;
    BEGIN
    SELECT 1
    INTO conflict_count
    FROM boreholes.permafrost p
    WHERE p.borehole_id    = NEW.borehole_id
    AND NEW.depth_from_m < p.depth_to_m
    AND NEW.depth_to_m   > p.depth_from_m
    -- on UPDATE skip comparing against itself
    AND (TG_OP = 'INSERT' OR p.permafrost_record_id <> NEW.permafrost_record_id)
    LIMIT 1;
    
    IF FOUND THEN
    RAISE EXCEPTION
    'Cannot add permafrost [% – %] for borehole %: overlaps existing interval',
    NEW.depth_from_m, NEW.depth_to_m, NEW.borehole_id;
    END IF;
    
    RETURN NEW;
    END;
    $$;


ALTER FUNCTION boreholes.prevent_permafrost_overlap() OWNER TO postgres;

--
-- Name: apply_corrections(integer, timestamp with time zone, numeric); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.apply_corrections(p_timeseries_id integer, p_datetime timestamp with time zone, p_value numeric) OWNER TO postgres;

--
-- Name: check_approvals_overlap(); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.check_approvals_overlap() OWNER TO postgres;

--
-- Name: check_contributors_overlap(); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.check_contributors_overlap() OWNER TO postgres;

--
-- Name: check_grades_overlap(); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.check_grades_overlap() OWNER TO postgres;

--
-- Name: check_owners_overlap(); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.check_owners_overlap() OWNER TO postgres;

--
-- Name: check_qualifiers_overlap(); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.check_qualifiers_overlap() OWNER TO postgres;

--
-- Name: delete_old_forecasts(); Type: FUNCTION; Schema: continuous; Owner: postgres
--

CREATE FUNCTION continuous.delete_old_forecasts() RETURNS void
    LANGUAGE plpgsql
    AS $$ BEGIN
    DELETE FROM forecasts
    WHERE valid_datetime < NOW() - INTERVAL '2 weeks';
END;
$$;


ALTER FUNCTION continuous.delete_old_forecasts() OWNER TO postgres;

--
-- Name: trunc_hour_utc(timestamp with time zone); Type: FUNCTION; Schema: continuous; Owner: postgres
--

CREATE FUNCTION continuous.trunc_hour_utc(ts timestamp with time zone) RETURNS timestamp with time zone
    LANGUAGE sql IMMUTABLE
    AS $$ SELECT date_trunc('hour', ts AT TIME ZONE 'UTC') AT TIME ZONE 'UTC'; $$;


ALTER FUNCTION continuous.trunc_hour_utc(ts timestamp with time zone) OWNER TO postgres;

--
-- Name: validate_corrections(); Type: FUNCTION; Schema: continuous; Owner: postgres
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


ALTER FUNCTION continuous.validate_corrections() OWNER TO postgres;

--
-- Name: check_sample_series_overlap(); Type: FUNCTION; Schema: discrete; Owner: postgres
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


ALTER FUNCTION discrete.check_sample_series_overlap() OWNER TO postgres;

--
-- Name: enforce_result_speciation(); Type: FUNCTION; Schema: discrete; Owner: postgres
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
                AND NEW.result_speciation_id IS NULL
          ) THEN
              RAISE EXCEPTION 'result_speciation_id must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
          END IF;
          RETURN NEW;
      END;
      $$;


ALTER FUNCTION discrete.enforce_result_speciation() OWNER TO postgres;

--
-- Name: enforce_sample_fraction(); Type: FUNCTION; Schema: discrete; Owner: postgres
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
                          AND NEW.sample_fraction_id IS NULL
                    ) THEN
                        RAISE EXCEPTION 'sample_fraction_id must be populated for parameter_id % as defined in the parameters table.', NEW.parameter_id;
                    END IF;
                    RETURN NEW;
                END;
                $$;


ALTER FUNCTION discrete.enforce_sample_fraction() OWNER TO postgres;

--
-- Name: get_guideline_value(integer, integer); Type: FUNCTION; Schema: discrete; Owner: admin
--

CREATE FUNCTION discrete.get_guideline_value(in_guideline_id integer, in_sample_id integer DEFAULT NULL::integer) RETURNS numeric
    LANGUAGE plpgsql
    SET search_path TO 'discrete', 'public'
    AS $_$
    DECLARE
      expr          TEXT;
      result        NUMERIC;
      needs_sample  BOOLEAN;
    BEGIN
      SELECT g.guideline_sql INTO expr
      FROM discrete.guidelines g
      WHERE g.guideline_id = in_guideline_id;
    
      IF expr IS NULL THEN 
        RAISE EXCEPTION 'No guideline found with guideline_id %', in_guideline_id;
      END IF;
    
    IF expr ~ '\$[2-9]' THEN
        RAISE EXCEPTION 'Guideline SQL may only use $1 parameter';
      END IF;

      -- Templates that need a sample must use $1
      needs_sample := position('$1' in expr) > 0;
    
      IF needs_sample AND in_sample_id IS NULL THEN
        RAISE EXCEPTION 'This guideline requires a sample_id but none was provided.';
      END IF;
    
      IF needs_sample THEN
        EXECUTE expr INTO STRICT result USING in_sample_id;
      ELSE
        EXECUTE expr INTO STRICT result;
      END IF;
    
      RETURN result;
    
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE EXCEPTION 'Guideline % SQL returned no value', in_guideline_id;
      WHEN TOO_MANY_ROWS THEN
        RAISE EXCEPTION 'Guideline % SQL returned more than one value', in_guideline_id;
    END;
    $_$;


ALTER FUNCTION discrete.get_guideline_value(in_guideline_id integer, in_sample_id integer) OWNER TO admin;

--
-- Name: get_sample_hardness(integer); Type: FUNCTION; Schema: discrete; Owner: postgres
--

CREATE FUNCTION discrete.get_sample_hardness(in_sample_id integer) RETURNS numeric
    LANGUAGE sql STABLE
    SET search_path TO 'discrete', 'public'
    AS $_$
                WITH vals AS (
                  SELECT
                    MAX(result) FILTER (WHERE parameter_id = 1061 AND sample_fraction_id = 5)  AS ca_d,
                    MAX(result) FILTER (WHERE parameter_id = 1103 AND sample_fraction_id = 5)  AS mg_d,
                    MAX(result) FILTER (WHERE parameter_id = 100  AND sample_fraction_id = 5  AND result_speciation_id = 3) AS hard_d,
                    MAX(result) FILTER (WHERE parameter_id = 1061 AND sample_fraction_id = 19) AS ca_t,
                    MAX(result) FILTER (WHERE parameter_id = 1103 AND sample_fraction_id = 19) AS mg_t,
                    MAX(result) FILTER (WHERE parameter_id = 100  AND sample_fraction_id = 19 AND result_speciation_id = 3) AS hard_t
                  FROM discrete.results
                  WHERE sample_id = $1   -- use $1 placeholder, not in_sample_id
                )
                SELECT CASE
                  WHEN ca_d IS NOT NULL AND mg_d IS NOT NULL AND ca_d > 0 AND mg_d > 0 
                    THEN 2.497*ca_d + 4.118*mg_d
                  WHEN hard_d IS NOT NULL AND hard_d > 0 
                    THEN hard_d
                  WHEN ca_t IS NOT NULL AND mg_t IS NOT NULL AND ca_t > 0 AND mg_t > 0 
                    THEN 2.497*ca_t + 4.118*mg_t
                  WHEN hard_t IS NOT NULL AND hard_t > 0 
                    THEN hard_t
                END
                FROM vals;
                $_$;


ALTER FUNCTION discrete.get_sample_hardness(in_sample_id integer) OWNER TO postgres;

--
-- Name: get_sample_val(integer, integer, integer, integer); Type: FUNCTION; Schema: discrete; Owner: postgres
--

CREATE FUNCTION discrete.get_sample_val(sample_id integer, parameter_id integer, sample_fraction_id integer DEFAULT NULL::integer, result_speciation_id integer DEFAULT NULL::integer) RETURNS numeric
    LANGUAGE sql STABLE
    SET search_path TO 'discrete', 'public'
    AS $_$
                SELECT MAX(result)::numeric
                FROM discrete.results
                WHERE sample_id = $1
                  AND parameter_id = $2
                  AND sample_fraction_id IS NOT DISTINCT FROM $3 -- allows match on NULL
                  AND result_speciation_id IS NOT DISTINCT FROM $4; -- allows match on NULL
                $_$;


ALTER FUNCTION discrete.get_sample_val(sample_id integer, parameter_id integer, sample_fraction_id integer, result_speciation_id integer) OWNER TO postgres;

--
-- Name: guideline_pb(integer); Type: FUNCTION; Schema: discrete; Owner: admin
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


ALTER FUNCTION discrete.guideline_pb(x_id integer) OWNER TO admin;

--
-- Name: guidelines_validate_trg(); Type: FUNCTION; Schema: discrete; Owner: postgres
--

CREATE FUNCTION discrete.guidelines_validate_trg() RETURNS trigger
    LANGUAGE plpgsql
    SET search_path TO 'discrete', 'public'
    AS $_$
DECLARE
  expr            text := NEW.guideline_sql;
  scan            text;
  needs_sample    boolean;
  explain_json    jsonb;
  allowed_schemas text[] := ARRAY['discrete','public'];
  bad_schema      text;
  cmdtype         text;
BEGIN
  -- presence
  IF expr IS NULL OR btrim(expr) = '' THEN
    RAISE EXCEPTION 'Guideline SQL cannot be empty';
  END IF;

  -- quick lexical guards (blocks multiple statements / DDL)
  scan := expr;
  -- strip dollar-quoted strings
  scan := regexp_replace(scan, '(?s)\$[^$]*\$.*?\$[^$]*\$', '', 'g');
  -- strip single-quoted strings
  scan := regexp_replace(scan, '''([^''\\]|\\.)*''', '', 'g');
  -- strip comments
  scan := regexp_replace(scan, '--.*?(\n|$)', '', 'g');
  scan := regexp_replace(scan, '/\*.*?\*/', '', 'gs');

  -- must not contain semicolons
  IF scan ~ ';' THEN
    RAISE EXCEPTION 'Guideline SQL must be a single statement (no semicolons)';
  END IF;

  -- must be a SELECT (allow WITH … SELECT)
  IF scan !~* '^[[:space:]]*\(*[[:space:]]*(with[[:space:]]+.*select|select)([[:space:]]|\()' THEN
    RAISE EXCEPTION 'Guideline SQL must begin with SELECT (optionally WITH … SELECT)';
  END IF;

  -- placeholder policy: only $1 allowed
  IF scan ~ '\$[2-9]' THEN
    RAISE EXCEPTION 'Only $1 parameter placeholder is permitted';
  END IF;
  needs_sample := scan ~ '\$1';

  -- Build a wrapper that enforces: single scalar numeric row
  -- If expr returns more than one row/col or non-numeric => error at EXPLAIN time (shape is checked)
  -- NOTE: we do EXPLAIN to *parse/plan only*, not execute.
  IF needs_sample THEN
    EXECUTE format($$
      EXPLAIN (VERBOSE, FORMAT JSON)
      WITH q AS (%s)
      SELECT (SELECT * FROM q)::numeric
    $$, expr)
    INTO explain_json
    USING NULL;  -- e.g., user must write $1::numeric in expr
  ELSE
    EXECUTE format($$
      EXPLAIN (VERBOSE, FORMAT JSON)
      WITH q AS (%s)
      SELECT (SELECT * FROM q)::numeric
    $$, expr)
    INTO explain_json;
  END IF;

  -- Schema whitelist using recursive walk of the plan tree
  WITH RECURSIVE plan_nodes AS (
    SELECT (explain_json->0->'Plan') AS n
    UNION ALL
    SELECT child
    FROM plan_nodes p
    CROSS JOIN LATERAL jsonb_array_elements(COALESCE(p.n->'Plans','[]')) AS children(child)
  ),
  schemas AS (
    SELECT DISTINCT lower(n->>'Schema') AS schem
    FROM plan_nodes
    WHERE n ? 'Schema'
  )
  SELECT schem
  INTO bad_schema
  FROM schemas
  WHERE schem IS NOT NULL
    AND schem <> ALL(allowed_schemas)
  LIMIT 1;

  IF bad_schema IS NOT NULL THEN
    RAISE EXCEPTION 'Guideline SQL references disallowed schema: %', bad_schema;
  END IF;

  -- If we got here, it parses, plans, is a SELECT, produces a single numeric scalar, and only touches allowed schemas.
  RETURN NEW;
END;
$_$;


ALTER FUNCTION discrete.guidelines_validate_trg() OWNER TO postgres;

--
-- Name: validate_guideline_start_end(); Type: FUNCTION; Schema: discrete; Owner: admin
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


ALTER FUNCTION discrete.validate_guideline_start_end() OWNER TO admin;

--
-- Name: check_data_sharing_agreement(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.check_data_sharing_agreement() OWNER TO postgres;

--
-- Name: check_location_images(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.check_location_images() OWNER TO postgres;

--
-- Name: enforce_share_with_restriction(); Type: FUNCTION; Schema: files; Owner: postgres
--

CREATE FUNCTION files.enforce_share_with_restriction() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
  -- Skip check if img_series_id is NULL
  IF NEW.img_series_id IS NULL THEN
  RETURN NEW;
  END IF;
  
  -- Check if images_index.share_with is NOT '{public_reader}'
  IF NOT ('public_reader' = ANY(NEW.share_with)) THEN
  -- Retrieve the corresponding share_with from images_index
  PERFORM 1
  FROM images_index
  WHERE img_series_id = NEW.img_series_id
  AND NOT ('public_reader' = ANY(share_with));
  
  -- If images_index.share_with is NOT {'public_reader'}, raise an exception
  IF FOUND THEN
  RAISE EXCEPTION 'images_index entry for img_series_id % has a restrictive share_with, images.share_with cannot be {public_reader}', NEW.img_series_id;
  END IF;
  END IF;
  RETURN NEW;
  END;
  $$;


ALTER FUNCTION files.enforce_share_with_restriction() OWNER TO postgres;

--
-- Name: update_document_flags_after_delete(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.update_document_flags_after_delete() OWNER TO postgres;

--
-- Name: update_document_flags_after_insert(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.update_document_flags_after_insert() OWNER TO postgres;

--
-- Name: update_line_flag(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.update_line_flag() OWNER TO postgres;

--
-- Name: update_location_flag(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.update_location_flag() OWNER TO postgres;

--
-- Name: update_polygon_flag(); Type: FUNCTION; Schema: files; Owner: postgres
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


ALTER FUNCTION files.update_polygon_flag() OWNER TO postgres;

--
-- Name: update_modify_datetime(); Type: FUNCTION; Schema: instruments; Owner: postgres
--

CREATE FUNCTION instruments.update_modify_datetime() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
      NEW.modify_datetime = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$;


ALTER FUNCTION instruments.update_modify_datetime() OWNER TO postgres;

--
-- Name: check_instrument_meta_overlap(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.check_instrument_meta_overlap() OWNER TO postgres;

--
-- Name: check_location_exists(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.check_location_exists() OWNER TO postgres;

--
-- Name: check_locations_metadata_acquisition_instruments(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.check_locations_metadata_acquisition_instruments() OWNER TO postgres;

--
-- Name: check_page_content_integrity(); Type: FUNCTION; Schema: public; Owner: admin
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


ALTER FUNCTION public.check_page_content_integrity() OWNER TO admin;

--
-- Name: count_estimate(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.count_estimate(query text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
    DECLARE
  plan jsonb;
  BEGIN
  EXECUTE FORMAT('EXPLAIN (FORMAT JSON) %s', query) INTO plan;
  RETURN plan->0->'Plan'->'Plan Rows';
  END;
  $$;


ALTER FUNCTION public.count_estimate(query text) OWNER TO postgres;

--
-- Name: current_user_roles(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.current_user_roles() RETURNS text[]
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_temp', 'pg_catalog'
    AS $$
        SELECT coalesce(array_agg(r.rolname), ARRAY[]::text[])
      FROM pg_roles r
      WHERE pg_has_role(current_user, r.oid, 'member');
      $$;


ALTER FUNCTION public.current_user_roles() OWNER TO postgres;

--
-- Name: enforce_maintenance_constraints(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.enforce_maintenance_constraints() OWNER TO postgres;

--
-- Name: fill_locations_metadata_access_missing(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_access_missing() OWNER TO postgres;

--
-- Name: fill_locations_metadata_acquisition_missing(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_acquisition_missing() OWNER TO postgres;

--
-- Name: fill_locations_metadata_infrastructure_groundwater(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_infrastructure_groundwater() OWNER TO postgres;

--
-- Name: fill_locations_metadata_infrastructure_hydromet(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_infrastructure_hydromet() OWNER TO postgres;

--
-- Name: fill_locations_metadata_infrastructure_missing(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_infrastructure_missing() OWNER TO postgres;

--
-- Name: fill_locations_metadata_owners_operators_missing(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_owners_operators_missing() OWNER TO postgres;

--
-- Name: fill_locations_metadata_transmission_missing(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.fill_locations_metadata_transmission_missing() OWNER TO postgres;

--
-- Name: get_csw_layer(); Type: FUNCTION; Schema: public; Owner: admin
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


ALTER FUNCTION public.get_csw_layer() OWNER TO admin;

--
-- Name: get_shareable_groups_for(regclass, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_shareable_groups_for(_rel regclass, _privs text[] DEFAULT ARRAY['SELECT'::text]) RETURNS TABLE(role_name text)
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_temp', 'pg_catalog'
    AS $$
  SELECT r.rolname
  FROM pg_roles r
  WHERE r.rolcanlogin = false             -- groups only
    AND r.rolname <> 'public'             -- exclude PUBLIC
    AND r.rolname <> 'pg_read_all_data'  -- exclude built-in roles
    AND EXISTS (                           -- has ANY requested privilege
      SELECT 1
      FROM unnest(_privs) p
      WHERE has_table_privilege(r.oid, _rel, p)
    )
  ORDER BY r.rolname;
$$;


ALTER FUNCTION public.get_shareable_groups_for(_rel regclass, _privs text[]) OWNER TO postgres;

--
-- Name: get_shareable_principals_for(regclass, text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.get_shareable_principals_for(_rel regclass, _privs text[] DEFAULT ARRAY['SELECT'::text], _always_include text[] DEFAULT ARRAY['public_reader'::text, 'admin'::text]) RETURNS TABLE(role_name text)
    LANGUAGE sql STABLE SECURITY DEFINER
    SET search_path TO 'pg_temp', 'pg_catalog'
    AS $$
  SELECT x.role_name
  FROM (
    SELECT r.rolname AS role_name
    FROM pg_roles r
    WHERE r.rolcanlogin = false
      AND r.rolname <> 'public'
      AND r.rolname <> 'pg_read_all_data'
      AND r.rolname NOT LIKE 'pg\_%'      -- hide system roles
      AND r.rolname NOT LIKE 'rds\_%'     -- (optional) hide AWS RDS roles
      AND EXISTS (
        SELECT 1 FROM unnest(_privs) p
        WHERE has_table_privilege(r.oid, _rel, p)
      )

    UNION

    SELECT r.rolname
    FROM pg_roles r
    WHERE r.rolname = ANY(_always_include)
  ) AS x
  ORDER BY
    CASE WHEN x.role_name = ANY(_always_include) THEN 0 ELSE 1 END,
    x.role_name;
$$;


ALTER FUNCTION public.get_shareable_principals_for(_rel regclass, _privs text[], _always_include text[]) OWNER TO postgres;

--
-- Name: update_created_modified(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.update_created_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
    NEW.created_modified = CURRENT_TIMESTAMP;
    RETURN NEW;
  END;
  $$;


ALTER FUNCTION public.update_created_modified() OWNER TO postgres;

--
-- Name: update_modified(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.update_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
      NEW.modified = CURRENT_TIMESTAMP;
      RETURN NEW;
    END;
    $$;


ALTER FUNCTION public.update_modified() OWNER TO postgres;

--
-- Name: update_updated(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.update_updated() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
               BEGIN
               NEW.updated = NOW();
               RETURN NEW;
               END;
               $$;


ALTER FUNCTION public.update_updated() OWNER TO postgres;

--
-- Name: user_in_group(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.user_in_group(group_name text) RETURNS boolean
    LANGUAGE sql STABLE
    AS $$
                      SELECT pg_has_role(current_user, group_name, 'MEMBER');
                    $$;


ALTER FUNCTION public.user_in_group(group_name text) OWNER TO postgres;

--
-- Name: user_modified(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.user_modified() OWNER TO postgres;

--
-- Name: validate_documents_array(); Type: FUNCTION; Schema: public; Owner: postgres
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


ALTER FUNCTION public.validate_documents_array() OWNER TO postgres;

--
-- Name: validate_guideline_start_end(); Type: FUNCTION; Schema: public; Owner: admin
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


ALTER FUNCTION public.validate_guideline_start_end() OWNER TO admin;

--
-- Name: validate_guideline_values(); Type: FUNCTION; Schema: public; Owner: admin
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


ALTER FUNCTION public.validate_guideline_values() OWNER TO admin;

--
-- Name: validate_share_with(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.validate_share_with() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  DECLARE
bad text;
BEGIN
-- Canonicalize: de-duplicate & sort (optional but handy)
NEW.share_with :=
  (SELECT array_agg(DISTINCT g ORDER BY g)
   FROM unnest(NEW.share_with) AS u(g));

-- 'public_reader' must be the only value if present
IF 'public_reader' = ANY(NEW.share_with)
AND array_length(NEW.share_with, 1) > 1
THEN
RAISE EXCEPTION 'If public_reader is present, it must be the only value';
END IF;

-- Find any entry that is NOT an allowed group or 'public_reader'
SELECT g
INTO bad
FROM unnest(NEW.share_with) AS u(g)
WHERE NOT EXISTS (
  SELECT 1
  FROM pg_roles r
  WHERE r.rolname = g
  AND (
    r.rolname = 'public_reader'           -- sentinel
    OR (r.rolcanlogin = false             -- groups only
        AND r.rolname <> 'public'         -- exclude pseudo-role
        AND r.rolname !~ '^pg_')   -- exclude built-ins
  )
)
LIMIT 1;

IF FOUND THEN
RAISE EXCEPTION 'Invalid value in share_with: % (allowed: group roles or public_reader)', bad;
END IF;

RETURN NEW;
END;
$$;


ALTER FUNCTION public.validate_share_with() OWNER TO postgres;

--
-- Name: update_geom_type(); Type: FUNCTION; Schema: spatial; Owner: postgres
--

CREATE FUNCTION spatial.update_geom_type() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  BEGIN
NEW.geom_type := ST_GeometryType(NEW.geom);
RETURN NEW;
END;
$$;


ALTER FUNCTION spatial.update_geom_type() OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: feedback; Type: TABLE; Schema: application; Owner: admin
--

CREATE TABLE application.feedback (
    "timestamp" timestamp with time zone DEFAULT now(),
    sentiment boolean,
    comment text,
    page text,
    app_state jsonb
);


ALTER TABLE application.feedback OWNER TO admin;

--
-- Name: images; Type: TABLE; Schema: application; Owner: admin
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


ALTER TABLE application.images OWNER TO admin;

--
-- Name: page_content; Type: TABLE; Schema: application; Owner: admin
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


ALTER TABLE application.page_content OWNER TO admin;

--
-- Name: TABLE page_content; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON TABLE application.page_content IS 'Table to hold the content for each page, with the position of each element. This is used to order the text and images on each page.';


--
-- Name: COLUMN page_content.page; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON COLUMN application.page_content.page IS 'The page to which the content belongs.';


--
-- Name: COLUMN page_content."position"; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON COLUMN application.page_content."position" IS 'The position of the content on the page.';


--
-- Name: COLUMN page_content.content_type; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON COLUMN application.page_content.content_type IS 'The type of content, either text or image.';


--
-- Name: COLUMN page_content.content_id; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON COLUMN application.page_content.content_id IS 'The id of the content, either the text id or the image id.';


--
-- Name: text; Type: TABLE; Schema: application; Owner: admin
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


ALTER TABLE application.text OWNER TO admin;

--
-- Name: TABLE text; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON TABLE application.text IS 'Table to hold frequently changed text for the application, such as news, descriptions, etc. Text which is more static is instead stored in the R package files.';


--
-- Name: COLUMN text.id; Type: COMMENT; Schema: application; Owner: admin
--

COMMENT ON COLUMN application.text.id IS 'Unique identifier for the text; this is referenced in the application to select the correct entry.';


--
-- Name: borehole_well_purposes; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.borehole_well_purposes (
    borehole_well_purpose_id integer NOT NULL,
    purpose_name text NOT NULL,
    description text NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified timestamp with time zone,
    modified_by text
);


ALTER TABLE boreholes.borehole_well_purposes OWNER TO postgres;

--
-- Name: borehole_well_purposes_borehole_well_purpose_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.borehole_well_purposes ALTER COLUMN borehole_well_purpose_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME boreholes.borehole_well_purposes_borehole_well_purpose_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: boreholes; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.boreholes (
    borehole_id bigint NOT NULL,
    location_id integer,
    borehole_name text,
    completion_date date,
    drilled_by integer,
    drill_method text,
    comissioned_by text,
    latitude numeric NOT NULL,
    longitude numeric NOT NULL,
    location_source text,
    ground_elevation_m numeric,
    elevation_source text,
    depth_m numeric,
    import_borehole_id text,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modified timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    notes text,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    private_expiry date,
    borehole_well_purpose_id integer,
    inferred_purpose boolean DEFAULT true,
    depth_to_bedrock_m numeric
);


ALTER TABLE boreholes.boreholes OWNER TO postgres;

--
-- Name: boreholes_borehole_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.boreholes ALTER COLUMN borehole_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.boreholes_borehole_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: boreholes_documents; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.boreholes_documents (
    borehole_id integer NOT NULL,
    document_id integer NOT NULL
);


ALTER TABLE boreholes.boreholes_documents OWNER TO postgres;

--
-- Name: boreholes_no_coords; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.boreholes_no_coords (
    borehole_id integer NOT NULL,
    borehole_name text,
    completion_date date,
    drilled_by integer,
    drill_method text,
    comissioned_by text,
    latitude numeric,
    longitude numeric,
    location_source text,
    ground_elevation_m numeric,
    elevation_source text,
    depth_m numeric,
    import_borehole_id text,
    created timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    updated timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE boreholes.boreholes_no_coords OWNER TO postgres;

--
-- Name: boreholes_no_coords_borehole_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.boreholes_no_coords ALTER COLUMN borehole_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.boreholes_no_coords_borehole_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: boreholes_no_coords_documents; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.boreholes_no_coords_documents (
    borehole_id integer NOT NULL,
    document_id integer NOT NULL
);


ALTER TABLE boreholes.boreholes_no_coords_documents OWNER TO postgres;

--
-- Name: casing_materials; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.casing_materials (
    casing_material_id integer NOT NULL,
    material_name text NOT NULL,
    material_name_fr text,
    created timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    updated timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE boreholes.casing_materials OWNER TO postgres;

--
-- Name: casing_materials_casing_material_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.casing_materials ALTER COLUMN casing_material_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.casing_materials_casing_material_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: drillers; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.drillers (
    driller_id integer NOT NULL,
    name text NOT NULL,
    address text,
    phone text,
    email text,
    created timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    updated timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE boreholes.drillers OWNER TO postgres;

--
-- Name: drillers_driller_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.drillers ALTER COLUMN driller_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.drillers_driller_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: geology; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.geology (
    geo_record_id integer NOT NULL,
    borehole_id integer NOT NULL,
    depth_from_m numeric NOT NULL,
    depth_to_m numeric NOT NULL,
    color text,
    consolidated boolean NOT NULL,
    primary_material text,
    secondary_material text,
    texture text,
    description text,
    created timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    updated timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE boreholes.geology OWNER TO postgres;

--
-- Name: geology_geo_record_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.geology ALTER COLUMN geo_record_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.geology_geo_record_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: permafrost; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.permafrost (
    permafrost_record_id integer NOT NULL,
    borehole_id integer NOT NULL,
    depth_from_m numeric,
    depth_to_m numeric,
    ice_description text,
    created timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    updated timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE boreholes.permafrost OWNER TO postgres;

--
-- Name: permafrost_permafrost_record_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.permafrost ALTER COLUMN permafrost_record_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.permafrost_permafrost_record_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: wells; Type: TABLE; Schema: boreholes; Owner: postgres
--

CREATE TABLE boreholes.wells (
    well_id integer NOT NULL,
    borehole_id integer NOT NULL,
    casing_material integer,
    casing_diameter_mm numeric,
    casing_depth_to_m numeric,
    stick_up_height_m numeric,
    screen_top_depth_m numeric,
    screen_bottom_depth_m numeric,
    screen_comment text,
    static_water_level_m numeric,
    estimated_yield_lps numeric,
    created timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    updated timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    notes text,
    borehole_well_purpose_id integer,
    inferred_purpose boolean DEFAULT true
);


ALTER TABLE boreholes.wells OWNER TO postgres;

--
-- Name: wells_well_id_seq; Type: SEQUENCE; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.wells ALTER COLUMN well_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME boreholes.wells_well_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: aggregation_types; Type: TABLE; Schema: continuous; Owner: postgres
--

CREATE TABLE continuous.aggregation_types (
    aggregation_type_id integer NOT NULL,
    aggregation_type text NOT NULL,
    aggregation_type_fr text NOT NULL,
    description text,
    description_fr text
);


ALTER TABLE continuous.aggregation_types OWNER TO postgres;

--
-- Name: aggregation_types_aggregation_type_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.aggregation_types ALTER COLUMN aggregation_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.aggregation_types_aggregation_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: approvals; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.approvals OWNER TO postgres;

--
-- Name: approvals_approval_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.approvals ALTER COLUMN approval_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.approvals_approval_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: contributors; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.contributors OWNER TO postgres;

--
-- Name: contributors_contributor_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.contributors ALTER COLUMN contributor_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.contributors_contributor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: correction_types; Type: TABLE; Schema: continuous; Owner: admin
--

CREATE TABLE continuous.correction_types (
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


ALTER TABLE continuous.correction_types OWNER TO admin;

--
-- Name: TABLE correction_types; Type: COMMENT; Schema: continuous; Owner: admin
--

COMMENT ON TABLE continuous.correction_types IS 'Table to hold correction types and information for their use in the corrections table';


--
-- Name: COLUMN correction_types.priority; Type: COMMENT; Schema: continuous; Owner: admin
--

COMMENT ON COLUMN continuous.correction_types.priority IS 'Defines the order in which corrections are applied. Lower numbers are applied first.';


--
-- Name: COLUMN correction_types.value1; Type: COMMENT; Schema: continuous; Owner: admin
--

COMMENT ON COLUMN continuous.correction_types.value1 IS 'Whether or not value1 is required for this correction type. NULL if optional';


--
-- Name: COLUMN correction_types.value2; Type: COMMENT; Schema: continuous; Owner: admin
--

COMMENT ON COLUMN continuous.correction_types.value2 IS 'Whether or not value2 is required for this correction type. NULL if optional';


--
-- Name: COLUMN correction_types.timestep_window; Type: COMMENT; Schema: continuous; Owner: admin
--

COMMENT ON COLUMN continuous.correction_types.timestep_window IS 'Whether or not timestep_window is required for this correction type. NULL if optional';


--
-- Name: COLUMN correction_types.equation; Type: COMMENT; Schema: continuous; Owner: admin
--

COMMENT ON COLUMN continuous.correction_types.equation IS 'Whether or not equation is required for this correction type. NULL if optional';


--
-- Name: correction_types_correction_type_id_seq; Type: SEQUENCE; Schema: continuous; Owner: admin
--

ALTER TABLE continuous.correction_types ALTER COLUMN correction_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.correction_types_correction_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: corrections; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.corrections OWNER TO postgres;

--
-- Name: TABLE corrections; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.corrections IS 'Table to hold corrections for timeseries data. Each correction applies to a timeseries for a specified datetime range; overlapping corrections are applied in order of priority as defined in table correction_types.';


--
-- Name: corrections_correction_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.corrections ALTER COLUMN correction_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.corrections_correction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: extrema; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.extrema OWNER TO postgres;

--
-- Name: TABLE extrema; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.extrema IS 'Holds vetted information about extrema specific to each time-series. Can be used for calculating return periods. Entries unique on timeseries_id, agency, year, period_type, condition, extrema, which allows for multiple different types of extrema from different authorities (agencies) for each timeseries.';


--
-- Name: COLUMN extrema.agency; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.agency IS 'The agency (authority) which calculated the extreme value. Ex: Water Resources Branch, Water Survey of Canada, Tetra Tech, etc.';


--
-- Name: COLUMN extrema.year; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.year IS 'The year for which each value is valid.';


--
-- Name: COLUMN extrema.date; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.date IS 'The exact date on which the extreme value occured.';


--
-- Name: COLUMN extrema.period_type; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.period_type IS 'One of instantaneous, 1-day, 2-day, 3-day, 4-day, 5-day, 6-day, 7-day, monthly, yearly. For example, a 1-day max flow is the maximum 1-day averaged flow for the year; instantaneous max flow is the greatest value single data point recorded for the year.';


--
-- Name: COLUMN extrema.condition; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.condition IS 'One of open water, break-up, freeze-up, winter. Any given timeseries can have one value of each for each year.';


--
-- Name: COLUMN extrema.extrema; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.extrema IS 'One of minimum or maximum. Necessary along with other descriptive columns to fully describe what each value represents.';


--
-- Name: COLUMN extrema.deemed_primary; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.extrema.deemed_primary IS 'If TRUE then the extrema value is the best (most reliable) value and should be used for most calculations and representations.';


--
-- Name: forecasts; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.forecasts OWNER TO postgres;

--
-- Name: TABLE forecasts; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.forecasts IS 'Holds forecast timeseries information. Each timeseries must match up with a timeseries_id from the timeseries table. Quantiles are optional. Data should be deleted after a certain time interval to prevent unecessarily burdening the database.';


--
-- Name: COLUMN forecasts.issue_datetime; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.forecasts.issue_datetime IS 'The datetime for which the forecast data point (row) is valid.';


--
-- Name: grades; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.grades OWNER TO postgres;

--
-- Name: grades_grade_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.grades ALTER COLUMN grade_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.grades_grade_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: measurements_calculated_daily; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.measurements_calculated_daily OWNER TO postgres;

--
-- Name: TABLE measurements_calculated_daily; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.measurements_calculated_daily IS 'Stores calculated daily mean values for timeseries present in table measurements_continuous. Values should not be entered or modified manually but instead are calculated by the HydroMetDB package function calculate_stats.';


--
-- Name: COLUMN measurements_calculated_daily.imputed; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.imputed IS 'TRUE in this column means that at least one of the measurements used for the daily mean calculation was imputed, or, for daily means provided solely in the HYDAT database, that a value was imputed directly to this table.';


--
-- Name: COLUMN measurements_calculated_daily.percent_historic_range; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.percent_historic_range IS 'The percent of historical range for that measurement compared to all previous records for the same day of year (not including the current measurement). Only populated once a minimum of three values exist for the current day of year (including the current value). February 29 values are the mean of February 28 and March 1.

For example, a value equal to the maximum historic value is equal to 100% of historical range, while one at the miniumu value is 0%. Values above or below the historical range can have values of less than 0 or greater than 100.

The formula used for the calculation is ((current - min) / (max - min)) * 100';


--
-- Name: COLUMN measurements_calculated_daily.max; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.max IS 'Historical max for the day of year, excluding current measurement.';


--
-- Name: COLUMN measurements_calculated_daily.min; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.min IS 'Historical min for the day of year, excluding current measurement.';


--
-- Name: COLUMN measurements_calculated_daily.q50; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.q50 IS 'Historical 50th quantile or median, excluding current measurement.';


--
-- Name: COLUMN measurements_calculated_daily.doy_count; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_calculated_daily.doy_count IS 'Number of measurements existing in the calculated_daily table for each day including historic and current measurement.';


--
-- Name: timeseries; Type: TABLE; Schema: continuous; Owner: postgres
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
    z_id integer,
    active boolean DEFAULT true NOT NULL,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    sensor_priority integer DEFAULT 1,
    sub_location_id integer,
    default_owner integer,
    private_expiry date,
    timezone_daily_calc integer DEFAULT 0 NOT NULL,
    sync_remote boolean DEFAULT true NOT NULL,
    CONSTRAINT timezone_daily_calc_valid CHECK (((timezone_daily_calc >= '-12'::integer) AND (timezone_daily_calc <= 14)))
);

ALTER TABLE ONLY continuous.timeseries FORCE ROW LEVEL SECURITY;


ALTER TABLE continuous.timeseries OWNER TO postgres;

--
-- Name: TABLE timeseries; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.timeseries IS 'Provides a record of every continuous-type timeseries in the database. Each timeseries is unique by its combination of location, sub_location, parameter, media, period_type, record_rate, sensor priority, and z (elevation or depth). Continuous data is data gathered at regular and usually frequent intervals by automatic means. Refer to table sample_series for lab and manual measurement.';


--
-- Name: COLUMN timeseries.timeseries_id; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.timeseries_id IS 'Autoincrements each time a timeseries is added. NOTE that timeseries should only be added using the R function addACTimeseries from the package AquaCache to ensure correctness of the data.';


--
-- Name: COLUMN timeseries.location; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.location IS 'Matches to the locations table.';


--
-- Name: COLUMN timeseries.aggregation_type_id; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.aggregation_type_id IS 'Descriptor for the type of aggregation applicable to this timeseries. Must match an entry to table aggregation_types, for example one of instantaneous, sum, mean, median, min, max, or (min+max)/2.';


--
-- Name: COLUMN timeseries.start_datetime; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.start_datetime IS 'First data point for the timeseries.';


--
-- Name: COLUMN timeseries.end_datetime; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.end_datetime IS 'Last data point for the timeseries.';


--
-- Name: COLUMN timeseries.last_new_data; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.last_new_data IS 'Time at which data was last appended to the timeseries';


--
-- Name: COLUMN timeseries.last_daily_calculation; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.last_daily_calculation IS 'Time at which daily means were calculated using function calculate_stats. Not used for discrete timeseries.';


--
-- Name: COLUMN timeseries.last_synchronize; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.last_synchronize IS 'Time at which the timeseries was cross-checked against values held by the remote or partner database; the local store should have been updated to reflect the remote.';


--
-- Name: COLUMN timeseries.source_fx; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.source_fx IS 'The function used to download and pre-process data for addition to a timeseries. Must be a function present in the R package AquaCache.';


--
-- Name: COLUMN timeseries.source_fx_args; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.source_fx_args IS 'Arguments to pass to the source_fx in a JSON format. Used for example to specify the url from which to fetch data or the location and parameter.';


--
-- Name: COLUMN timeseries.record_rate; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.record_rate IS 'The general recording interval for the timeseries, only used to differentiate between sub-daily, daily, weekly, or monthly recording. A more refined measure of the periodicity of the data is recorded in the measurements_continuous table.';


--
-- Name: COLUMN timeseries.location_id; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.location_id IS 'Matches to the locations table.';


--
-- Name: COLUMN timeseries.z_id; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.z_id IS 'Elevation of the measurement station, in meters. Used for things like thermistor strings, wind towers, or forecast climate parameters at different heights. Z elevations should be taken in the context of the location''s assigned elevation and datum.';


--
-- Name: COLUMN timeseries.active; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.active IS 'Defines if the timeseries should or should not be added to or back-corrected by various AquaCache package functions.';


--
-- Name: COLUMN timeseries.timezone_daily_calc; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.timeseries.timezone_daily_calc IS 'Timezone offset in hours from UTC used for daily mean and statistics calculations. Range is -12 to +14. Default is 0 (UTC).';


--
-- Name: measurements_calculated_daily_corrected; Type: VIEW; Schema: continuous; Owner: postgres
--

CREATE VIEW continuous.measurements_calculated_daily_corrected WITH (security_barrier='true') AS
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
     JOIN continuous.timeseries ts USING (timeseries_id));


ALTER VIEW continuous.measurements_calculated_daily_corrected OWNER TO postgres;

--
-- Name: measurements_continuous; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.measurements_continuous OWNER TO postgres;

--
-- Name: TABLE measurements_continuous; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.measurements_continuous IS 'Stores observations and imputed values for continuous timeseries.';


--
-- Name: COLUMN measurements_continuous.period; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_continuous.period IS 'Greater than 0 for min, max, sum, mean types of measurements. The periodicity of data can change within a timeseries, for example if recording rates go from every 6 hours to hourly.';


--
-- Name: COLUMN measurements_continuous.imputed; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.measurements_continuous.imputed IS 'Imputed values may be user-entered. Imputed values are automatically replaced if/when a value becomes available on the remote data store';


--
-- Name: measurements_continuous_corrected; Type: VIEW; Schema: continuous; Owner: postgres
--

CREATE VIEW continuous.measurements_continuous_corrected WITH (security_barrier='true') AS
 SELECT mc.timeseries_id,
    mc.datetime,
    mc.value AS value_raw,
    ac.value_corrected,
    mc.period,
    mc.imputed
   FROM (continuous.measurements_continuous mc
     CROSS JOIN LATERAL ( SELECT continuous.apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected) ac);


ALTER VIEW continuous.measurements_continuous_corrected OWNER TO postgres;

--
-- Name: measurements_hourly_corrected; Type: VIEW; Schema: continuous; Owner: postgres
--

CREATE VIEW continuous.measurements_hourly_corrected AS
 SELECT timeseries_id,
    continuous.trunc_hour_utc(datetime) AS datetime,
    avg(value_raw) AS value_raw,
    avg(value_corrected) AS value_corrected,
    bool_or(imputed) AS imputed
   FROM continuous.measurements_continuous_corrected mcc
  GROUP BY timeseries_id, (continuous.trunc_hour_utc(datetime));


ALTER VIEW continuous.measurements_hourly_corrected OWNER TO postgres;

--
-- Name: owners; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.owners OWNER TO postgres;

--
-- Name: owners_owner_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.owners ALTER COLUMN owner_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.owners_owner_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: qualifiers; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.qualifiers OWNER TO postgres;

--
-- Name: qualifiers_qualifier_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.qualifiers ALTER COLUMN qualifier_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.qualifiers_qualifier_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: rating_curve_points; Type: TABLE; Schema: continuous; Owner: postgres
--

CREATE TABLE continuous.rating_curve_points (
    curve_point_id integer NOT NULL,
    curve_id integer NOT NULL,
    input_value numeric NOT NULL,
    output_value numeric NOT NULL
);


ALTER TABLE continuous.rating_curve_points OWNER TO postgres;

--
-- Name: rating_curve_points_curve_point_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.rating_curve_points ALTER COLUMN curve_point_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.rating_curve_points_curve_point_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: rating_curve_shifts; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.rating_curve_shifts OWNER TO postgres;

--
-- Name: rating_curve_shifts_curve_shift_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.rating_curve_shifts ALTER COLUMN curve_shift_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.rating_curve_shifts_curve_shift_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: rating_curves; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.rating_curves OWNER TO postgres;

--
-- Name: rating_curves_curve_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.rating_curves ALTER COLUMN curve_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.rating_curves_curve_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: thresholds; Type: TABLE; Schema: continuous; Owner: postgres
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


ALTER TABLE continuous.thresholds OWNER TO postgres;

--
-- Name: TABLE thresholds; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON TABLE continuous.thresholds IS 'Holds threshold values for a variety of things like streamflows, water levels, flood levels, aquatic life inpacts.';


--
-- Name: COLUMN thresholds.high_advisory; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.high_advisory IS 'Value at which a high water advisory is to be issued.';


--
-- Name: COLUMN thresholds.high_watch; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.high_watch IS 'Value at which a high water watch is to be issued.';


--
-- Name: COLUMN thresholds.high_warning; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.high_warning IS 'Value at which a high water warning is to be issued.';


--
-- Name: COLUMN thresholds.flood_minor; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.flood_minor IS 'Value at which a minor flood is to be declared.';


--
-- Name: COLUMN thresholds.flood_major; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.flood_major IS 'Value at which a major flood is to be declared.';


--
-- Name: COLUMN thresholds.high_first_human_impacts; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.high_first_human_impacts IS 'High-side value at which first human impacts are known, such as impacts to navigation.';


--
-- Name: COLUMN thresholds.low_advisory; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.low_advisory IS 'Value at which a low water advisory is to be issued.';


--
-- Name: COLUMN thresholds.low_watch; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.low_watch IS 'Value at which a low water watch is to be issued.';


--
-- Name: COLUMN thresholds.low_warning; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.low_warning IS 'Value at which a low water warning is to be issued.';


--
-- Name: COLUMN thresholds.low_first_human_impacts; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.low_first_human_impacts IS 'Low-side value at which first human impacts are known, such as impacts to navigation.';


--
-- Name: COLUMN thresholds.low_aquatic_life_impacts_minor; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.low_aquatic_life_impacts_minor IS 'Low-side (water temp, level, flow) minor impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.low_aquatic_life_impacts_major; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.low_aquatic_life_impacts_major IS 'Low-side (water temp, level, flow) major impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.high_aquatic_life_impacts_minor; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.high_aquatic_life_impacts_minor IS 'High-side (water temp, level, flow) minor impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.high_aquatic_life_impacts_major; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.high_aquatic_life_impacts_major IS 'High-side (water temp, level, flow) major impact threshold to aquatic life.';


--
-- Name: COLUMN thresholds.fsl; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.fsl IS 'Full supply level (as per water use licence for control structure operations)';


--
-- Name: COLUMN thresholds.lsl; Type: COMMENT; Schema: continuous; Owner: postgres
--

COMMENT ON COLUMN continuous.thresholds.lsl IS 'Low supply level (as per water use licence for control structure operations)';


--
-- Name: locations; Type: TABLE; Schema: public; Owner: postgres
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
    private_expiry date,
    created timestamp with time zone DEFAULT now(),
    modified timestamp with time zone
);

ALTER TABLE ONLY public.locations FORCE ROW LEVEL SECURITY;


ALTER TABLE public.locations OWNER TO postgres;

--
-- Name: COLUMN locations.data_sharing_agreement_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations.data_sharing_agreement_id IS 'An optional link to a data sharing agreement from the documents table. A check is enforced to ensure that this column only references documents of type ''data sharing agreement''.';


--
-- Name: COLUMN locations.install_purpose; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations.install_purpose IS 'Purpose of original installation.';


--
-- Name: COLUMN locations.current_purpose; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations.current_purpose IS 'Current purpose for location/station operation.';


--
-- Name: COLUMN locations.jurisdictional_relevance; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations.jurisdictional_relevance IS 'Whether the location is publicly relevant to the jursdiction operating this database. Can be used for filtering results from public-facing applications if desired without labelling the location or associated timeseries as non-public.';


--
-- Name: COLUMN locations.anthropogenic_influence; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations.anthropogenic_influence IS 'Flag to indicate if the location has anthropogenic influence';


--
-- Name: COLUMN locations.sentinel_location; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations.sentinel_location IS 'Flag to indicate if the location is a sentinel location for climate change analyses and reports';


--
-- Name: locations_z; Type: TABLE; Schema: public; Owner: admin
--

CREATE TABLE public.locations_z (
    z_id integer NOT NULL,
    location_id integer NOT NULL,
    sub_location_id integer,
    z_meters numeric NOT NULL,
    note text,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified timestamp with time zone,
    modified_by text
);


ALTER TABLE public.locations_z OWNER TO admin;

--
-- Name: TABLE locations_z; Type: COMMENT; Schema: public; Owner: admin
--

COMMENT ON TABLE public.locations_z IS 'Table to hold depth/height (z) information for monitoring locations. This is referenced for timeseries as well as instrument deployment purposes, but note that sample data elevations/depths are not linked to this table.';


--
-- Name: media_types; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.media_types OWNER TO postgres;

--
-- Name: parameters; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.parameters OWNER TO postgres;

--
-- Name: TABLE parameters; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.parameters IS 'Holds information about each parameter, including the parameter name, description, unit, and default plotting orientation, ceiling, and floor to facilitate plotting. Parameters are associated with groups and sub-groups via the table parameter_relationships.';


--
-- Name: COLUMN parameters.unit_default; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.parameters.unit_default IS 'SI units only. For example, m3/s, mg/L, etc. Import functions should convert to SI units listed here before appending to the DB.';


--
-- Name: COLUMN parameters.result_speciation; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.parameters.result_speciation IS 'Controls if results using this parameter require an entry to column result_speciation of table measurements_discrete';


--
-- Name: COLUMN parameters.sample_fraction; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.parameters.sample_fraction IS 'Controls if results using this parameter require an entry to column sample_fraction of table measurements_discrete';


--
-- Name: timeseries_metadata_en; Type: VIEW; Schema: continuous; Owner: admin
--

CREATE VIEW continuous.timeseries_metadata_en WITH (security_invoker='true') AS
 SELECT ts.timeseries_id,
    loc.location_id,
    loc.name AS location_name,
    lz.z_meters AS depth_height_m,
    mtypes.media_type,
    params.param_name AS parameter_name,
    params.unit_default AS units,
    at.aggregation_type,
    ts.record_rate AS recording_rate,
    ts.start_datetime,
    ts.end_datetime,
    ts.note
   FROM (((((continuous.timeseries ts
     JOIN public.locations loc ON ((ts.location_id = loc.location_id)))
     LEFT JOIN public.parameters params ON ((ts.parameter_id = params.parameter_id)))
     LEFT JOIN public.media_types mtypes ON ((ts.media_id = mtypes.media_id)))
     LEFT JOIN continuous.aggregation_types at ON ((ts.aggregation_type_id = at.aggregation_type_id)))
     LEFT JOIN public.locations_z lz ON ((ts.z_id = lz.z_id)))
  ORDER BY ts.timeseries_id;


ALTER VIEW continuous.timeseries_metadata_en OWNER TO admin;

--
-- Name: timeseries_metadata_fr; Type: VIEW; Schema: continuous; Owner: admin
--

CREATE VIEW continuous.timeseries_metadata_fr WITH (security_invoker='true') AS
 SELECT ts.timeseries_id,
    loc.location_id,
    loc.name_fr AS nom_endroit,
    lz.z_meters AS profondeur_hauteur_m,
    mtypes.media_type_fr AS "type_de_média",
    params.param_name_fr AS "nom_paramètre",
    params.unit_default AS "unités",
    ag.aggregation_type_fr AS "type_agrégation",
    ts.record_rate AS "fréquence_enregistrement",
    ts.start_datetime AS "début",
    ts.end_datetime AS fin,
    ts.note
   FROM (((((continuous.timeseries ts
     JOIN public.locations loc ON ((ts.location_id = loc.location_id)))
     LEFT JOIN public.parameters params ON ((ts.parameter_id = params.parameter_id)))
     LEFT JOIN public.media_types mtypes ON ((ts.media_id = mtypes.media_id)))
     LEFT JOIN continuous.aggregation_types ag ON ((ts.aggregation_type_id = ag.aggregation_type_id)))
     LEFT JOIN public.locations_z lz ON ((ts.z_id = lz.z_id)))
  ORDER BY ts.timeseries_id;


ALTER VIEW continuous.timeseries_metadata_fr OWNER TO admin;

--
-- Name: timeseries_timeseries_id_seq; Type: SEQUENCE; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.timeseries ALTER COLUMN timeseries_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME continuous.timeseries_timeseries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: protocols_methods; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.protocols_methods (
    protocol_id integer NOT NULL,
    protocol_name text NOT NULL,
    protocol_description text,
    url text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.protocols_methods OWNER TO postgres;

--
-- Name: analysis_protocols_protocol_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.protocols_methods ALTER COLUMN protocol_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.analysis_protocols_protocol_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: collection_methods; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.collection_methods (
    collection_method_id integer NOT NULL,
    collection_method text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.collection_methods OWNER TO postgres;

--
-- Name: collection_methods_collection_method_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.collection_methods ALTER COLUMN collection_method_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.collection_methods_collection_method_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: guideline_values; Type: TABLE; Schema: discrete; Owner: admin
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


ALTER TABLE discrete.guideline_values OWNER TO admin;

--
-- Name: guideline_values_id_seq; Type: SEQUENCE; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.guideline_values ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.guideline_values_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: guidelines; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.guidelines (
    guideline_id integer NOT NULL,
    publisher text NOT NULL,
    guideline_name text NOT NULL,
    reference text,
    note text,
    parameter_id integer NOT NULL,
    sample_fraction_id integer,
    result_speciation_id integer,
    guideline_sql text NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified timestamp with time zone,
    modified_by text
);


ALTER TABLE discrete.guidelines OWNER TO postgres;

--
-- Name: guidelines_guideline_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.guidelines ALTER COLUMN guideline_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME discrete.guidelines_guideline_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: laboratories; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.laboratories (
    lab_id integer NOT NULL,
    lab_name text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.laboratories OWNER TO postgres;

--
-- Name: laboratories_lab_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.laboratories ALTER COLUMN lab_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.laboratories_lab_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: result_conditions; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.result_conditions (
    result_condition_id integer NOT NULL,
    result_condition text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.result_conditions OWNER TO postgres;

--
-- Name: result_conditions_result_condition_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.result_conditions ALTER COLUMN result_condition_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.result_conditions_result_condition_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: result_speciations; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.result_speciations (
    result_speciation_id integer NOT NULL,
    result_speciation text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.result_speciations OWNER TO postgres;

--
-- Name: result_speciations_result_speciation_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.result_speciations ALTER COLUMN result_speciation_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.result_speciations_result_speciation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: result_types; Type: TABLE; Schema: discrete; Owner: admin
--

CREATE TABLE discrete.result_types (
    result_type_id integer NOT NULL,
    result_type text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.result_types OWNER TO admin;

--
-- Name: result_types_result_type_id_seq; Type: SEQUENCE; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.result_types ALTER COLUMN result_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.result_types_result_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: result_value_types; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.result_value_types (
    result_value_type_id integer NOT NULL,
    result_value_type text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.result_value_types OWNER TO postgres;

--
-- Name: result_value_types_result_value_type_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.result_value_types ALTER COLUMN result_value_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.result_value_types_result_value_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: results; Type: TABLE; Schema: discrete; Owner: admin
--

CREATE TABLE discrete.results (
    result_id integer NOT NULL,
    sample_id integer NOT NULL,
    result_type integer NOT NULL,
    parameter_id integer NOT NULL,
    sample_fraction_id integer,
    result numeric,
    result_condition integer,
    result_condition_value numeric,
    result_value_type integer,
    result_speciation_id integer,
    protocol_method integer,
    laboratory integer,
    analysis_datetime timestamp with time zone,
    share_with text[] DEFAULT '{public_reader}'::text[],
    no_update boolean DEFAULT false,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    private_expiry date,
    CONSTRAINT chk_result_condition CHECK (((result_condition IS NULL) OR (result IS NULL))),
    CONSTRAINT chk_result_condition_value CHECK (((result_condition_value IS NULL) OR (result_condition = ANY (ARRAY[1, 2]))))
);


ALTER TABLE discrete.results OWNER TO admin;

--
-- Name: results_result_id_seq; Type: SEQUENCE; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.results ALTER COLUMN result_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.results_result_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: sample_fractions; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.sample_fractions (
    sample_fraction_id integer NOT NULL,
    sample_fraction text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.sample_fractions OWNER TO postgres;

--
-- Name: sample_fractions_sample_fraction_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.sample_fractions ALTER COLUMN sample_fraction_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.sample_fractions_sample_fraction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: sample_series; Type: TABLE; Schema: discrete; Owner: admin
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
    modified_by text,
    sync_remote boolean DEFAULT true NOT NULL
);


ALTER TABLE discrete.sample_series OWNER TO admin;

--
-- Name: TABLE sample_series; Type: COMMENT; Schema: discrete; Owner: admin
--

COMMENT ON TABLE discrete.sample_series IS 'Provides a means of automating the import of discrete data (lab or field measurements) via the source_fx and optional arguments, designed to work with the AquaCache R package. Actual sample metadata is stored in the samples table, measurements in the results table.';


--
-- Name: sample_series_sample_series_id_seq; Type: SEQUENCE; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.sample_series ALTER COLUMN sample_series_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.sample_series_sample_series_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: sample_types; Type: TABLE; Schema: discrete; Owner: postgres
--

CREATE TABLE discrete.sample_types (
    sample_type_id integer NOT NULL,
    sample_type text NOT NULL,
    sample_type_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE discrete.sample_types OWNER TO postgres;

--
-- Name: sample_types_sample_type_id_seq; Type: SEQUENCE; Schema: discrete; Owner: postgres
--

ALTER TABLE discrete.sample_types ALTER COLUMN sample_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.sample_types_sample_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: samples; Type: TABLE; Schema: discrete; Owner: admin
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
    modified_by text,
    private_expiry date,
    field_visit_id integer
);


ALTER TABLE discrete.samples OWNER TO admin;

--
-- Name: samples_sample_id_seq; Type: SEQUENCE; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.samples ALTER COLUMN sample_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME discrete.samples_sample_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: field_visit_images; Type: TABLE; Schema: field; Owner: postgres
--

CREATE TABLE field.field_visit_images (
    field_visit_id integer NOT NULL,
    image_id integer NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified timestamp with time zone,
    modified_by text
);


ALTER TABLE field.field_visit_images OWNER TO postgres;

--
-- Name: TABLE field_visit_images; Type: COMMENT; Schema: field; Owner: postgres
--

COMMENT ON TABLE field.field_visit_images IS 'Junction table to allow many-to-many relationship between field visits and images taken during the visit.';


--
-- Name: field_visit_instruments; Type: TABLE; Schema: field; Owner: postgres
--

CREATE TABLE field.field_visit_instruments (
    field_visit_id integer NOT NULL,
    instrument_id integer NOT NULL,
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified timestamp with time zone,
    modified_by text
);


ALTER TABLE field.field_visit_instruments OWNER TO postgres;

--
-- Name: TABLE field_visit_instruments; Type: COMMENT; Schema: field; Owner: postgres
--

COMMENT ON TABLE field.field_visit_instruments IS 'Junction table to allow many-to-many relationship between field visits and instruments used during the visit.';


--
-- Name: field_visits; Type: TABLE; Schema: field; Owner: postgres
--

CREATE TABLE field.field_visits (
    field_visit_id integer NOT NULL,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    location_id integer NOT NULL,
    sub_location_id integer,
    purpose text,
    cloud_cover_percent numeric,
    precip_current_rate text,
    precip_24h_mm numeric,
    precip_48h_mm numeric,
    air_temp_c numeric,
    wind text,
    note text,
    share_with text[] DEFAULT ARRAY['public_reader'::text],
    created timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified timestamp with time zone,
    modified_by text,
    precip_current_type text,
    CONSTRAINT field_visit_period_valid CHECK (((end_datetime IS NULL) OR (end_datetime > start_datetime))),
    CONSTRAINT field_visits_air_temp_c_check CHECK (((air_temp_c >= ('-40'::integer)::numeric) AND (air_temp_c <= (40)::numeric))),
    CONSTRAINT field_visits_cloud_cover_percent_check CHECK (((cloud_cover_percent >= (0)::numeric) AND (cloud_cover_percent <= (100)::numeric))),
    CONSTRAINT field_visits_precip_24h_mm_check CHECK ((precip_24h_mm >= (0)::numeric)),
    CONSTRAINT field_visits_precip_48h_mm_check CHECK ((precip_48h_mm >= (0)::numeric)),
    CONSTRAINT field_visits_precip_current_rate_check CHECK ((precip_current_rate = ANY (ARRAY['None'::text, 'Light'::text, 'Moderate'::text, 'Heavy'::text]))),
    CONSTRAINT field_visits_precip_current_type_check CHECK ((precip_current_type = ANY (ARRAY['None'::text, 'Rain'::text, 'Snow'::text, 'Mixed'::text, 'Hail'::text, 'Freezing rain'::text])))
);


ALTER TABLE field.field_visits OWNER TO postgres;

--
-- Name: TABLE field_visits; Type: COMMENT; Schema: field; Owner: postgres
--

COMMENT ON TABLE field.field_visits IS 'Table to hold metadata about field visits to monitoring locations. Samples can be linked to field visits and field visits to instruments used during the visit.';


--
-- Name: field_visits_field_visit_id_seq; Type: SEQUENCE; Schema: field; Owner: postgres
--

ALTER TABLE field.field_visits ALTER COLUMN field_visit_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME field.field_visits_field_visit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: document_types; Type: TABLE; Schema: files; Owner: postgres
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


ALTER TABLE files.document_types OWNER TO postgres;

--
-- Name: document_types_document_type_id_seq; Type: SEQUENCE; Schema: files; Owner: postgres
--

ALTER TABLE files.document_types ALTER COLUMN document_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME files.document_types_document_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: documents; Type: TABLE; Schema: files; Owner: postgres
--

CREATE TABLE files.documents (
    document_id integer NOT NULL,
    name text NOT NULL,
    type integer NOT NULL,
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
    modified_by text,
    private_expiry date
);

ALTER TABLE ONLY files.documents FORCE ROW LEVEL SECURITY;


ALTER TABLE files.documents OWNER TO postgres;

--
-- Name: TABLE documents; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON TABLE files.documents IS 'Holds documents and metadata associated with each document. Each document can be associated with one or more location, line, or polygon, or all three.';


--
-- Name: COLUMN documents.type; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.documents.type IS 'One of thesis, report, well log, conference paper, poster, journal article, map, graph, protocol, grading scheme, metadata, other';


--
-- Name: COLUMN documents.authors; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.documents.authors IS 'An *array* of one or more authors.';


--
-- Name: COLUMN documents.file_hash; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.documents.file_hash IS 'MD5 hash of the document file, used for ensuring that no two documents are identical.';


--
-- Name: documents_document_id_seq; Type: SEQUENCE; Schema: files; Owner: postgres
--

ALTER TABLE files.documents ALTER COLUMN document_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME files.documents_document_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: documents_spatial; Type: TABLE; Schema: files; Owner: postgres
--

CREATE TABLE files.documents_spatial (
    document_id integer NOT NULL,
    geom_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE files.documents_spatial OWNER TO postgres;

--
-- Name: image_series; Type: TABLE; Schema: files; Owner: postgres
--

CREATE TABLE files.image_series (
    img_series_id integer NOT NULL,
    first_img timestamp with time zone,
    last_img timestamp with time zone,
    last_new_img timestamp with time zone,
    source_fx text,
    source_fx_args jsonb,
    description text,
    location_id integer NOT NULL,
    active boolean,
    share_with text[] DEFAULT '{public_reader}'::text[] NOT NULL,
    owner integer,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    private_expiry date
);

ALTER TABLE ONLY files.image_series FORCE ROW LEVEL SECURITY;


ALTER TABLE files.image_series OWNER TO postgres;

--
-- Name: TABLE image_series; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON TABLE files.image_series IS 'Index for image series in images table. Each location at which there is an image series gets an entry here; images in table images are linked to this table using the img_meta_id.';


--
-- Name: COLUMN image_series.active; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.image_series.active IS 'Defines if the image series should or should not be imported.';


--
-- Name: image_types; Type: TABLE; Schema: files; Owner: postgres
--

CREATE TABLE files.image_types (
    image_type_id integer NOT NULL,
    image_type text NOT NULL,
    description text,
    default_tag_options text[],
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE files.image_types OWNER TO postgres;

--
-- Name: image_types_image_type_id_seq; Type: SEQUENCE; Schema: files; Owner: postgres
--

ALTER TABLE files.image_types ALTER COLUMN image_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME files.image_types_image_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: images; Type: TABLE; Schema: files; Owner: postgres
--

CREATE TABLE files.images (
    image_id integer NOT NULL,
    img_series_id integer,
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
    file_hash text GENERATED ALWAYS AS (md5(encode(file, 'hex'::text))) STORED NOT NULL,
    image_type integer NOT NULL,
    create_datetime timestamp without time zone DEFAULT now(),
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    private_expiry date
);

ALTER TABLE ONLY files.images FORCE ROW LEVEL SECURITY;


ALTER TABLE files.images OWNER TO postgres;

--
-- Name: TABLE images; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON TABLE files.images IS 'Holds images of local conditions specific to each location. Originally designed to hold auto-captured images at WSC locations, but could be used for other location images such as setup documentation. For image series or images necessarily associated with a location, images are linked to the images_index table using the img_meta_id.';


--
-- Name: COLUMN images.img_series_id; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.img_series_id IS 'Keyed to table images_index to associate an image with other images taken at this location. May be NULL for images not part of a series.';


--
-- Name: COLUMN images.latitude; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.latitude IS 'If the image is not part of an image series (and doesnt have an entry in column img_meta_id) this column must be populated. Enforced via trigger+function.';


--
-- Name: COLUMN images.longitude; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.longitude IS 'If the image is not part of an image series (and doesnt have an entry in column img_meta_id) this column must be populated. Enforced via trigger+function.';


--
-- Name: COLUMN images.azimuth_true; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.azimuth_true IS 'Direction of the camera in degrees from true north.';


--
-- Name: COLUMN images.elevation_agl_m; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.elevation_agl_m IS 'Elevation in meters above ground level';


--
-- Name: COLUMN images.elevation_msl_m; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.elevation_msl_m IS 'Elevation in meters above mean sea level';


--
-- Name: COLUMN images.file_hash; Type: COMMENT; Schema: files; Owner: postgres
--

COMMENT ON COLUMN files.images.file_hash IS 'MD5 hash of the image file, used to ensure that no two images are identical';


--
-- Name: images_image_id_seq; Type: SEQUENCE; Schema: files; Owner: postgres
--

ALTER TABLE files.images ALTER COLUMN image_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME files.images_image_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: images_index_img_meta_id_seq; Type: SEQUENCE; Schema: files; Owner: postgres
--

ALTER TABLE files.image_series ALTER COLUMN img_series_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME files.images_index_img_meta_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: internal_status; Type: TABLE; Schema: information; Owner: postgres
--

CREATE TABLE information.internal_status (
    event text NOT NULL,
    value timestamp with time zone
);


ALTER TABLE information.internal_status OWNER TO postgres;

--
-- Name: TABLE internal_status; Type: COMMENT; Schema: information; Owner: postgres
--

COMMENT ON TABLE information.internal_status IS 'Holds information about when a certain operation took place on the database using the R functions in the HydroMetDB package.';


--
-- Name: version_info; Type: TABLE; Schema: information; Owner: postgres
--

CREATE TABLE information.version_info (
    id integer NOT NULL,
    item text,
    version text,
    created_modified timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);


ALTER TABLE information.version_info OWNER TO postgres;

--
-- Name: version_info_id_seq; Type: SEQUENCE; Schema: information; Owner: postgres
--

ALTER TABLE information.version_info ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME information.version_info_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: array_maintenance_changes; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.array_maintenance_changes OWNER TO postgres;

--
-- Name: TABLE array_maintenance_changes; Type: COMMENT; Schema: instruments; Owner: postgres
--

COMMENT ON TABLE instruments.array_maintenance_changes IS 'This table is used to record changes to the sensors in an instrument array. Each row represents a single maintenance event, and the notes field should contain a description of the changes or maintenance made to each sensor. A simple maintenance event with no change of sensor should have the same sensor_id as the previous entry for that instrument and sensorX_id, while sensor changes must be recorded with a new sensor_id.';


--
-- Name: array_maintenance_changes_event_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.array_maintenance_changes ALTER COLUMN event_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.array_maintenance_changes_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: calibrate_depth; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_depth OWNER TO postgres;

--
-- Name: calibrate_dissolved_oxygen; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_dissolved_oxygen OWNER TO postgres;

--
-- Name: calibrate_orp; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_orp OWNER TO postgres;

--
-- Name: calibrate_ph; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_ph OWNER TO postgres;

--
-- Name: calibrate_specific_conductance; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_specific_conductance OWNER TO postgres;

--
-- Name: calibrate_temperature; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_temperature OWNER TO postgres;

--
-- Name: calibrate_turbidity; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrate_turbidity OWNER TO postgres;

--
-- Name: calibrations; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.calibrations OWNER TO postgres;

--
-- Name: calibrations_calibration_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.calibrations ALTER COLUMN calibration_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.calibrations_calibration_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: instrument_maintenance; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.instrument_maintenance OWNER TO postgres;

--
-- Name: instrument_maintenance_event_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.instrument_maintenance ALTER COLUMN event_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.instrument_maintenance_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: instrument_make; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.instrument_make OWNER TO postgres;

--
-- Name: instrument_make_make_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.instrument_make ALTER COLUMN make_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.instrument_make_make_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: instrument_model; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.instrument_model OWNER TO postgres;

--
-- Name: instrument_model_model_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.instrument_model ALTER COLUMN model_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.instrument_model_model_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: instrument_type; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.instrument_type OWNER TO postgres;

--
-- Name: instrument_type_type_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.instrument_type ALTER COLUMN type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.instrument_type_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: instruments; Type: TABLE; Schema: instruments; Owner: postgres
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
    owner integer NOT NULL,
    create_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    modify_datetime timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    date_end_of_life date,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE instruments.instruments OWNER TO postgres;

--
-- Name: instruments_instrument_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.instruments ALTER COLUMN instrument_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.instruments_instrument_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: observers; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.observers OWNER TO postgres;

--
-- Name: observers_observer_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.observers ALTER COLUMN observer_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.observers_observer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: sensor_types; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.sensor_types OWNER TO postgres;

--
-- Name: sensor_types_sensor_type_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.sensor_types ALTER COLUMN sensor_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.sensor_types_sensor_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: sensors; Type: TABLE; Schema: instruments; Owner: postgres
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


ALTER TABLE instruments.sensors OWNER TO postgres;

--
-- Name: sensors_sensor_id_seq; Type: SEQUENCE; Schema: instruments; Owner: postgres
--

ALTER TABLE instruments.sensors ALTER COLUMN sensor_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME instruments.sensors_sensor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: approval_types; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.approval_types OWNER TO postgres;

--
-- Name: approval_types_approval_type_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.approval_types ALTER COLUMN approval_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.approval_types_approval_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: datum_conversions; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.datum_conversions OWNER TO postgres;

--
-- Name: TABLE datum_conversions; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.datum_conversions IS 'Holds vertical datum conversions in meters, as well as identifying the most recent conversion for the timeseries.';


--
-- Name: COLUMN datum_conversions.conversion_id; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.datum_conversions.conversion_id IS 'Integer autoincrement column uniquely identifying the conversion.';


--
-- Name: COLUMN datum_conversions.datum_id_from; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.datum_conversions.datum_id_from IS 'The datum_id (matching an entry in table datum_list) from which the conversion value is from. Datum_id 10 is equal to the station assumed datum (station 0 relative to some arbitrary local benchmark).';


--
-- Name: COLUMN datum_conversions.datum_id_to; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.datum_conversions.datum_id_to IS 'The datum_id (matching an entry in table datum_list) from which the conversion value is to.';


--
-- Name: COLUMN datum_conversions.conversion_m; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.datum_conversions.conversion_m IS 'The elevation offset in meters to apply if transforming elevation values from the datum_id_from to the datum_id_to.';


--
-- Name: COLUMN datum_conversions.current; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.datum_conversions.current IS 'TRUE means that the conversion is the most up-to-date in the database. Only one conversion_id can be current for each location.';


--
-- Name: datum_conversions_conversion_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.datum_conversions ALTER COLUMN conversion_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.datum_conversions_conversion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: datum_list; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.datum_list (
    datum_id integer NOT NULL,
    datum_name_en text NOT NULL,
    datum_name_fr text NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE public.datum_list OWNER TO postgres;

--
-- Name: TABLE datum_list; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.datum_list IS 'Holds datum ids (referenced in the table datum_conversions) and their corresponding names in french and english. Taken directly from the datum list provided by HYDAT. Non-hydat datums can be added with datum_id beginning at 1000.';


--
-- Name: grade_types; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.grade_types OWNER TO postgres;

--
-- Name: grade_types_grade_type_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.grade_types ALTER COLUMN grade_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.grade_types_grade_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_networks; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.locations_networks (
    network_id integer NOT NULL,
    location_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE public.locations_networks OWNER TO postgres;

--
-- Name: locations_projects; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.locations_projects (
    project_id integer NOT NULL,
    location_id integer NOT NULL,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE public.locations_projects OWNER TO postgres;

--
-- Name: networks; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.networks OWNER TO postgres;

--
-- Name: projects; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.projects OWNER TO postgres;

--
-- Name: location_metadata_en; Type: VIEW; Schema: public; Owner: postgres
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


ALTER VIEW public.location_metadata_en OWNER TO postgres;

--
-- Name: location_metadata_fr; Type: VIEW; Schema: public; Owner: postgres
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


ALTER VIEW public.location_metadata_fr OWNER TO postgres;

--
-- Name: location_types; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.location_types (
    type_id integer NOT NULL,
    type text NOT NULL,
    type_fr text,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text
);


ALTER TABLE public.location_types OWNER TO postgres;

--
-- Name: location_types_type_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.location_types ALTER COLUMN type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.location_types_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_location_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations ALTER COLUMN location_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_location_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_access; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.locations_metadata_access OWNER TO postgres;

--
-- Name: TABLE locations_metadata_access; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.locations_metadata_access IS 'Site access metadata for locations.';


--
-- Name: COLUMN locations_metadata_access.method; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_access.method IS 'Method of access, such as helicopter, boat, foot, etc.';


--
-- Name: COLUMN locations_metadata_access.health_safety; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_access.health_safety IS 'Health and safety considerations for accessing the site.';


--
-- Name: COLUMN locations_metadata_access.notes; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_access.notes IS 'Additional notes about site access.';


--
-- Name: locations_metadata_access_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_access ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_access_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_infrastructure; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.locations_metadata_infrastructure OWNER TO postgres;

--
-- Name: TABLE locations_metadata_infrastructure; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.locations_metadata_infrastructure IS 'General infrastructure metadata for locations as freehand text.';


--
-- Name: locations_metadata_infrastructure_groundwater; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.locations_metadata_infrastructure_groundwater OWNER TO postgres;

--
-- Name: TABLE locations_metadata_infrastructure_groundwater; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.locations_metadata_infrastructure_groundwater IS 'Groundwater-specific infrastructure metadata.';


--
-- Name: COLUMN locations_metadata_infrastructure_groundwater.well_diameter_cm; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_groundwater.well_diameter_cm IS 'Diameter of well in centimeters.';


--
-- Name: COLUMN locations_metadata_infrastructure_groundwater.stick_up_m; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_groundwater.stick_up_m IS 'Distance from top of casing to ground surface in meters.';


--
-- Name: locations_metadata_infrastructure_groundwater_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_infrastructure_groundwater ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_infrastructure_groundwater_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_infrastructure_hydromet; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.locations_metadata_infrastructure_hydromet OWNER TO postgres;

--
-- Name: TABLE locations_metadata_infrastructure_hydromet; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.locations_metadata_infrastructure_hydromet IS 'Hydrometric-specific infrastructure metadata.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.stilling_well; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.stilling_well IS 'Type of stilling well and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.staff_gauge; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.staff_gauge IS 'Type of staff gauge and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.tower; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.tower IS 'Type of tower and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.shelter; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.shelter IS 'Type of shelter and details.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.reach_substrate; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.reach_substrate IS 'Substrate type at reach.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.reach_grade; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.reach_grade IS 'Grade of reach (approximate, degrees).';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.reach_note; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.reach_note IS 'Additional notes about reach.';


--
-- Name: COLUMN locations_metadata_infrastructure_hydromet.local_g; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_infrastructure_hydromet.local_g IS 'Location-specific gravitational constant';


--
-- Name: locations_metadata_infrastructure_hydromet_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_infrastructure_hydromet ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_infrastructure_hydromet_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_infrastructure_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_infrastructure ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_infrastructure_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_instruments; Type: TABLE; Schema: public; Owner: admin
--

CREATE TABLE public.locations_metadata_instruments (
    metadata_id integer NOT NULL,
    location_id integer NOT NULL,
    sub_location_id integer,
    instrument_id integer,
    start_datetime timestamp with time zone NOT NULL,
    end_datetime timestamp with time zone,
    created timestamp with time zone DEFAULT now() NOT NULL,
    modified timestamp with time zone,
    created_by text DEFAULT CURRENT_USER NOT NULL,
    modified_by text,
    note text,
    z_id integer,
    CONSTRAINT instrument_period_valid CHECK (((end_datetime IS NULL) OR (start_datetime < end_datetime)))
);


ALTER TABLE public.locations_metadata_instruments OWNER TO admin;

--
-- Name: locations_metadata_instruments_metadata_id_seq; Type: SEQUENCE; Schema: public; Owner: admin
--

ALTER TABLE public.locations_metadata_instruments ALTER COLUMN metadata_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_instruments_metadata_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_maintenance; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.locations_metadata_maintenance OWNER TO postgres;

--
-- Name: locations_metadata_maintenance_location_maintenance_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_maintenance ALTER COLUMN location_maintenance_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_maintenance_location_maintenance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_owners_operators; Type: TABLE; Schema: public; Owner: postgres
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
    modified_by text,
    note text,
    CONSTRAINT ownership_period_valid CHECK (((end_datetime IS NULL) OR (start_datetime < end_datetime)))
);


ALTER TABLE public.locations_metadata_owners_operators OWNER TO postgres;

--
-- Name: TABLE locations_metadata_owners_operators; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.locations_metadata_owners_operators IS 'Ownership and operator metadata for locations.';


--
-- Name: locations_metadata_owners_operators_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_owners_operators ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_owners_operators_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_metadata_xsections; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.locations_metadata_xsections OWNER TO postgres;

--
-- Name: TABLE locations_metadata_xsections; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON TABLE public.locations_metadata_xsections IS 'Cross-section and reach metadata for hydrometric sites.';


--
-- Name: COLUMN locations_metadata_xsections.x_m; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.x_m IS 'Distance along cross-section from left bank in meters.';


--
-- Name: COLUMN locations_metadata_xsections.y_top_m; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.y_top_m IS 'Top elevation of cross-section in meters (default 0).';


--
-- Name: COLUMN locations_metadata_xsections.y_bot_m; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.y_bot_m IS 'Bottom elevation of cross-section in meters.';


--
-- Name: COLUMN locations_metadata_xsections.mean_velocity_m_s; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.mean_velocity_m_s IS 'Mean velocity of water at cross-section x-value in cubic meters per second.';


--
-- Name: COLUMN locations_metadata_xsections.instrument; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.instrument IS 'Instrument used to measure velocity, referencing instruments.instruments table.';


--
-- Name: COLUMN locations_metadata_xsections.angle_deg; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.angle_deg IS 'Angle of cross-section relative to perpendicular to channel orientation. 90 (default) is perpendicular.';


--
-- Name: COLUMN locations_metadata_xsections.water_level_m; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.water_level_m IS 'Water level at time of cross-section measurements in meters.';


--
-- Name: COLUMN locations_metadata_xsections.xsection_substrate; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.xsection_substrate IS 'Substrate type at cross-section (description).';


--
-- Name: COLUMN locations_metadata_xsections.xsection_note; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.xsection_note IS 'Additional notes about cross-section.';


--
-- Name: COLUMN locations_metadata_xsections.valid; Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON COLUMN public.locations_metadata_xsections.valid IS 'Timestamp of validity of data.';


--
-- Name: locations_metadata_xsections_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.locations_metadata_xsections ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.locations_metadata_xsections_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: locations_z_z_id_seq; Type: SEQUENCE; Schema: public; Owner: admin
--

ALTER TABLE public.locations_z ALTER COLUMN z_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.locations_z_z_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: network_project_types; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.network_project_types OWNER TO postgres;

--
-- Name: network_project_types_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.network_project_types ALTER COLUMN id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.network_project_types_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: networks_network_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.networks ALTER COLUMN network_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.networks_network_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: organizations; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.organizations OWNER TO postgres;

--
-- Name: owners_contributors_owner_contributor_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.organizations ALTER COLUMN organization_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.owners_contributors_owner_contributor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: param_type_param_type_code_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.media_types ALTER COLUMN media_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.param_type_param_type_code_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: parameter_groups; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.parameter_groups OWNER TO postgres;

--
-- Name: parameter_groups_group_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.parameter_groups ALTER COLUMN group_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.parameter_groups_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: parameter_relationships; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.parameter_relationships (
    relationship_id integer NOT NULL,
    parameter_id integer NOT NULL,
    group_id integer NOT NULL,
    sub_group_id integer
);


ALTER TABLE public.parameter_relationships OWNER TO postgres;

--
-- Name: parameter_relationships_relationship_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.parameter_relationships ALTER COLUMN relationship_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.parameter_relationships_relationship_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: parameter_sub_groups; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.parameter_sub_groups OWNER TO postgres;

--
-- Name: parameter_sub_groups_sub_group_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.parameter_sub_groups ALTER COLUMN sub_group_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.parameter_sub_groups_sub_group_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: parameters_param_code_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.parameters ALTER COLUMN parameter_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.parameters_param_code_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: projects_project_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.projects ALTER COLUMN project_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.projects_project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: qualifier_types; Type: TABLE; Schema: public; Owner: postgres
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


ALTER TABLE public.qualifier_types OWNER TO postgres;

--
-- Name: qualifier_types_qualifier_type_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

ALTER TABLE public.qualifier_types ALTER COLUMN qualifier_type_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.qualifier_types_qualifier_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: sub_locations; Type: TABLE; Schema: public; Owner: admin
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
    modified_by text,
    private_expiry date
);


ALTER TABLE public.sub_locations OWNER TO admin;

--
-- Name: sub_locations_sub_location_id_seq; Type: SEQUENCE; Schema: public; Owner: admin
--

ALTER TABLE public.sub_locations ALTER COLUMN sub_location_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME public.sub_locations_sub_location_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: raster_series_index; Type: TABLE; Schema: spatial; Owner: postgres
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


ALTER TABLE spatial.raster_series_index OWNER TO postgres;

--
-- Name: TABLE raster_series_index; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON TABLE spatial.raster_series_index IS 'Holds metadata about raster series, such as reanalysis or forecast rasters. ';


--
-- Name: COLUMN raster_series_index.end_datetime; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.raster_series_index.end_datetime IS 'For rasters that have a valid_from and valid_to time, this is the valid_from of the latest raster in the database.';


--
-- Name: COLUMN raster_series_index.active; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.raster_series_index.active IS 'Defines if the raster series should or should not be imported.';


--
-- Name: raster_series_index_raster_series_id_seq; Type: SEQUENCE; Schema: spatial; Owner: postgres
--

ALTER TABLE spatial.raster_series_index ALTER COLUMN raster_series_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME spatial.raster_series_index_raster_series_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: rasters; Type: TABLE; Schema: spatial; Owner: admin
--

CREATE TABLE spatial.rasters (
    rid integer NOT NULL,
    reference_id integer,
    r_class text,
    r_proj4 text,
    rast spatial.raster NOT NULL
);


ALTER TABLE spatial.rasters OWNER TO admin;

--
-- Name: TABLE rasters; Type: COMMENT; Schema: spatial; Owner: admin
--

COMMENT ON TABLE spatial.rasters IS 'Holds raster tiles. Rasters may be broken up in multiple tiles, so refer to table rasters_reference to find the reference_ID for each raster. Otherwise this table is designed for extracting rasters using R, hence the r_class and r_proj4 columns.';


--
-- Name: COLUMN rasters.reference_id; Type: COMMENT; Schema: spatial; Owner: admin
--

COMMENT ON COLUMN spatial.rasters.reference_id IS 'Matches a unique entry in table rasters_reference. If a raster is broken up into tiles, each tile will have the same reference_id; this number is what identifies them as being tiles of the same raster.';


--
-- Name: rasters_reference; Type: TABLE; Schema: spatial; Owner: postgres
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


ALTER TABLE spatial.rasters_reference OWNER TO postgres;

--
-- Name: TABLE rasters_reference; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON TABLE spatial.rasters_reference IS 'References rasters in the rasters table, since the later might have rasters broken up in multiple tiles. This table has one reference_id per raster, which may be linked to multiple entries in table rasters.';


--
-- Name: COLUMN rasters_reference.reference_id; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.rasters_reference.reference_id IS 'Used to identify one or more raster tiles (large rasters may be broken up in tiles for performance) in the table rasters.';


--
-- Name: COLUMN rasters_reference.raster_series_id; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.rasters_reference.raster_series_id IS 'Identifies a time-series of rasters, the details of which are stored in table raster_series_index.';


--
-- Name: COLUMN rasters_reference.model; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.rasters_reference.model IS 'If the raster is generated from a model such as a climate model enter the name here. This is more useful for one-off rasters, as model timeseries will also list the model in table raster_series_index.';


--
-- Name: COLUMN rasters_reference.flag; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.rasters_reference.flag IS 'Used to flag rasters that require further review or that need to be deleted after a certain period. Reanalysis products in particular can have preliminary issues, in which case PRELIMINARY would be entered here.';


--
-- Name: rasters_reference_reference_id_seq; Type: SEQUENCE; Schema: spatial; Owner: postgres
--

ALTER TABLE spatial.rasters_reference ALTER COLUMN reference_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME spatial.rasters_reference_reference_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: rasters_rid_seq; Type: SEQUENCE; Schema: spatial; Owner: admin
--

ALTER TABLE spatial.rasters ALTER COLUMN rid ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME spatial.rasters_rid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: vectors; Type: TABLE; Schema: spatial; Owner: postgres
--

CREATE TABLE spatial.vectors (
    geom_id integer NOT NULL,
    geom_type text NOT NULL,
    layer_name text NOT NULL,
    feature_name text NOT NULL,
    description text,
    geom spatial.geometry(Geometry,4269) NOT NULL,
    attributes jsonb,
    CONSTRAINT enforce_dims_geom CHECK ((spatial.st_ndims(geom) = 2)),
    CONSTRAINT enforce_srid_geom CHECK ((spatial.st_srid(geom) = 4269)),
    CONSTRAINT enforce_valid_geom CHECK (spatial.st_isvalid(geom)),
    CONSTRAINT vectors_geom_type_check CHECK ((geom_type = ANY (ARRAY['ST_Point'::text, 'ST_MultiPoint'::text, 'ST_LineString'::text, 'ST_MultiLineString'::text, 'ST_Polygon'::text, 'ST_MultiPolygon'::text])))
);


ALTER TABLE spatial.vectors OWNER TO postgres;

--
-- Name: TABLE vectors; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON TABLE spatial.vectors IS 'Holds points, lines, or polygons as geometry objects that can be references by other tables. For example, the locations table references a geom_id for each location. Retrieve objects from this table using function HydroMetDB::fetchVector, insert them using HydroMetDB::insertHydrometVector.';


--
-- Name: COLUMN vectors.geom_type; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.vectors.geom_type IS '*DO NOT TOUCH* Auto-populated by trigger based on the geometry type for each entry.';


--
-- Name: COLUMN vectors.layer_name; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.vectors.layer_name IS 'Non-optional descriptive name for the layer.';


--
-- Name: COLUMN vectors.feature_name; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.vectors.feature_name IS 'Non-optional descriptive name for the feature.';


--
-- Name: COLUMN vectors.description; Type: COMMENT; Schema: spatial; Owner: postgres
--

COMMENT ON COLUMN spatial.vectors.description IS 'Optional but highly recommended long-form description of the geometry object.';


--
-- Name: vectors_geom_id_seq; Type: SEQUENCE; Schema: spatial; Owner: postgres
--

ALTER TABLE spatial.vectors ALTER COLUMN geom_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME spatial.vectors_geom_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: images images_pkey; Type: CONSTRAINT; Schema: application; Owner: admin
--

ALTER TABLE ONLY application.images
    ADD CONSTRAINT images_pkey PRIMARY KEY (id);


--
-- Name: text text_pkey; Type: CONSTRAINT; Schema: application; Owner: admin
--

ALTER TABLE ONLY application.text
    ADD CONSTRAINT text_pkey PRIMARY KEY (id);


--
-- Name: borehole_well_purposes borehole_well_purposes_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.borehole_well_purposes
    ADD CONSTRAINT borehole_well_purposes_pkey PRIMARY KEY (borehole_well_purpose_id);


--
-- Name: borehole_well_purposes borehole_well_purposes_purpose_name_key; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.borehole_well_purposes
    ADD CONSTRAINT borehole_well_purposes_purpose_name_key UNIQUE (purpose_name);


--
-- Name: boreholes_documents boreholes_documents_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_documents
    ADD CONSTRAINT boreholes_documents_pkey PRIMARY KEY (borehole_id, document_id);


--
-- Name: boreholes_no_coords_documents boreholes_no_coords_documents_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_no_coords_documents
    ADD CONSTRAINT boreholes_no_coords_documents_pkey PRIMARY KEY (borehole_id, document_id);


--
-- Name: boreholes_no_coords boreholes_no_coords_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_no_coords
    ADD CONSTRAINT boreholes_no_coords_pkey PRIMARY KEY (borehole_id);


--
-- Name: boreholes boreholes_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes
    ADD CONSTRAINT boreholes_pkey PRIMARY KEY (borehole_id);


--
-- Name: casing_materials casing_materials_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.casing_materials
    ADD CONSTRAINT casing_materials_pkey PRIMARY KEY (casing_material_id);


--
-- Name: drillers drillers_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.drillers
    ADD CONSTRAINT drillers_pkey PRIMARY KEY (driller_id);


--
-- Name: geology geology_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.geology
    ADD CONSTRAINT geology_pkey PRIMARY KEY (geo_record_id);


--
-- Name: permafrost permafrost_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.permafrost
    ADD CONSTRAINT permafrost_pkey PRIMARY KEY (permafrost_record_id);


--
-- Name: wells wells_pkey; Type: CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.wells
    ADD CONSTRAINT wells_pkey PRIMARY KEY (well_id);


--
-- Name: aggregation_types aggregation_types_aggregation_type_fr_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.aggregation_types
    ADD CONSTRAINT aggregation_types_aggregation_type_fr_key UNIQUE (aggregation_type_fr);


--
-- Name: aggregation_types aggregation_types_aggregation_type_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.aggregation_types
    ADD CONSTRAINT aggregation_types_aggregation_type_key UNIQUE (aggregation_type);


--
-- Name: aggregation_types aggregation_types_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.aggregation_types
    ADD CONSTRAINT aggregation_types_pkey PRIMARY KEY (aggregation_type_id);


--
-- Name: approvals approvals_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.approvals
    ADD CONSTRAINT approvals_pkey PRIMARY KEY (approval_id);


--
-- Name: measurements_calculated_daily calculated_daily_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.measurements_calculated_daily
    ADD CONSTRAINT calculated_daily_pkey PRIMARY KEY (timeseries_id, date);


--
-- Name: contributors contributors_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.contributors
    ADD CONSTRAINT contributors_pkey PRIMARY KEY (contributor_id);


--
-- Name: correction_types correction_types_pkey; Type: CONSTRAINT; Schema: continuous; Owner: admin
--

ALTER TABLE ONLY continuous.correction_types
    ADD CONSTRAINT correction_types_pkey PRIMARY KEY (correction_type_id);


--
-- Name: correction_types correction_types_priority_key; Type: CONSTRAINT; Schema: continuous; Owner: admin
--

ALTER TABLE ONLY continuous.correction_types
    ADD CONSTRAINT correction_types_priority_key UNIQUE (priority);


--
-- Name: corrections corrections_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_pkey PRIMARY KEY (correction_id);


--
-- Name: corrections corrections_timeseries_id_start_dt_end_dt_correction_type_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_timeseries_id_start_dt_end_dt_correction_type_key UNIQUE (timeseries_id, start_dt, end_dt, correction_type);


--
-- Name: forecasts forecasts_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.forecasts
    ADD CONSTRAINT forecasts_pkey PRIMARY KEY (timeseries_id, datetime);


--
-- Name: grades grades_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.grades
    ADD CONSTRAINT grades_pkey PRIMARY KEY (grade_id);


--
-- Name: measurements_continuous measurements_continuous_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.measurements_continuous
    ADD CONSTRAINT measurements_continuous_pkey PRIMARY KEY (timeseries_id, datetime);


--
-- Name: owners owners_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.owners
    ADD CONSTRAINT owners_pkey PRIMARY KEY (owner_id);


--
-- Name: extrema peaks_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.extrema
    ADD CONSTRAINT peaks_pkey PRIMARY KEY (timeseries_id);


--
-- Name: extrema peaks_timeseries_id_agency_year_period_type_condition_extre_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.extrema
    ADD CONSTRAINT peaks_timeseries_id_agency_year_period_type_condition_extre_key UNIQUE (timeseries_id, agency, year, period_type, condition, extrema);


--
-- Name: qualifiers qualifiers_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.qualifiers
    ADD CONSTRAINT qualifiers_pkey PRIMARY KEY (qualifier_id);


--
-- Name: rating_curve_points rating_curve_points_curve_id_input_value_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_points
    ADD CONSTRAINT rating_curve_points_curve_id_input_value_key UNIQUE (curve_id, input_value);


--
-- Name: rating_curve_points rating_curve_points_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_points
    ADD CONSTRAINT rating_curve_points_pkey PRIMARY KEY (curve_point_id);


--
-- Name: rating_curve_shifts rating_curve_shifts_curve_id_shift_start_shift_end_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_curve_id_shift_start_shift_end_key UNIQUE (curve_id, shift_start, shift_end);


--
-- Name: rating_curve_shifts rating_curve_shifts_no_overlap; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_no_overlap EXCLUDE USING gist (curve_id WITH =, tstzrange(shift_start, shift_end, '[]'::text) WITH &&);


--
-- Name: rating_curve_shifts rating_curve_shifts_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_pkey PRIMARY KEY (curve_shift_id);


--
-- Name: rating_curves rating_curves_location_id_input_parameter_id_output_paramet_key; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_location_id_input_parameter_id_output_paramet_key UNIQUE (location_id, input_parameter_id, output_parameter_id, valid_from);


--
-- Name: rating_curves rating_curves_no_overlap; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_no_overlap EXCLUDE USING gist (location_id WITH =, input_parameter_id WITH =, output_parameter_id WITH =, tstzrange(valid_from, valid_to, '[]'::text) WITH &&);


--
-- Name: rating_curves rating_curves_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_pkey PRIMARY KEY (curve_id);


--
-- Name: thresholds thresholds_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.thresholds
    ADD CONSTRAINT thresholds_pkey PRIMARY KEY (timeseries_id);


--
-- Name: timeseries timeseries_pkey; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_pkey PRIMARY KEY (timeseries_id);


--
-- Name: timeseries timeseries_unique; Type: CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_unique UNIQUE NULLS NOT DISTINCT (location_id, parameter_id, aggregation_type_id, media_id, record_rate, z_id, sensor_priority, sub_location_id);


--
-- Name: protocols_methods analysis_protocols_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.protocols_methods
    ADD CONSTRAINT analysis_protocols_pkey PRIMARY KEY (protocol_id);


--
-- Name: protocols_methods analysis_protocols_protocol_name_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.protocols_methods
    ADD CONSTRAINT analysis_protocols_protocol_name_key UNIQUE (protocol_name);


--
-- Name: collection_methods collection_methods_collection_method_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.collection_methods
    ADD CONSTRAINT collection_methods_collection_method_key UNIQUE (collection_method);


--
-- Name: collection_methods collection_methods_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.collection_methods
    ADD CONSTRAINT collection_methods_pkey PRIMARY KEY (collection_method_id);


--
-- Name: guideline_values guideline_values_pkey; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.guideline_values
    ADD CONSTRAINT guideline_values_pkey PRIMARY KEY (id);


--
-- Name: guidelines guidelines_guideline_name_publisher_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.guidelines
    ADD CONSTRAINT guidelines_guideline_name_publisher_key UNIQUE (guideline_name, publisher);


--
-- Name: guidelines guidelines_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.guidelines
    ADD CONSTRAINT guidelines_pkey PRIMARY KEY (guideline_id);


--
-- Name: laboratories laboratories_lab_name_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.laboratories
    ADD CONSTRAINT laboratories_lab_name_key UNIQUE (lab_name);


--
-- Name: laboratories laboratories_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.laboratories
    ADD CONSTRAINT laboratories_pkey PRIMARY KEY (lab_id);


--
-- Name: result_conditions result_conditions_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.result_conditions
    ADD CONSTRAINT result_conditions_pkey PRIMARY KEY (result_condition_id);


--
-- Name: result_conditions result_conditions_result_condition_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.result_conditions
    ADD CONSTRAINT result_conditions_result_condition_key UNIQUE (result_condition);


--
-- Name: result_speciations result_speciations_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.result_speciations
    ADD CONSTRAINT result_speciations_pkey PRIMARY KEY (result_speciation_id);


--
-- Name: result_speciations result_speciations_result_speciation_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.result_speciations
    ADD CONSTRAINT result_speciations_result_speciation_key UNIQUE (result_speciation);


--
-- Name: result_types result_types_pkey; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.result_types
    ADD CONSTRAINT result_types_pkey PRIMARY KEY (result_type_id);


--
-- Name: result_value_types result_value_types_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.result_value_types
    ADD CONSTRAINT result_value_types_pkey PRIMARY KEY (result_value_type_id);


--
-- Name: result_value_types result_value_types_result_value_type_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.result_value_types
    ADD CONSTRAINT result_value_types_result_value_type_key UNIQUE (result_value_type);


--
-- Name: results results_pkey; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_pkey PRIMARY KEY (result_id);


--
-- Name: sample_fractions sample_fractions_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.sample_fractions
    ADD CONSTRAINT sample_fractions_pkey PRIMARY KEY (sample_fraction_id);


--
-- Name: sample_fractions sample_fractions_sample_fraction_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.sample_fractions
    ADD CONSTRAINT sample_fractions_sample_fraction_key UNIQUE (sample_fraction);


--
-- Name: sample_series sample_series_location_id_sub_location_id_synch_from_synch__key; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_location_id_sub_location_id_synch_from_synch__key UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, synch_from, synch_to);


--
-- Name: sample_series sample_series_pkey; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_pkey PRIMARY KEY (sample_series_id);


--
-- Name: results sample_type_parameter_fraction_result_value_key; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT sample_type_parameter_fraction_result_value_key UNIQUE NULLS NOT DISTINCT (sample_id, result_type, parameter_id, sample_fraction_id, result_value_type, result_speciation_id, protocol_method, laboratory, analysis_datetime);


--
-- Name: sample_types sample_types_pkey; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.sample_types
    ADD CONSTRAINT sample_types_pkey PRIMARY KEY (sample_type_id);


--
-- Name: sample_types sample_types_sample_type_fr_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.sample_types
    ADD CONSTRAINT sample_types_sample_type_fr_key UNIQUE (sample_type_fr);


--
-- Name: sample_types sample_types_sample_type_key; Type: CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.sample_types
    ADD CONSTRAINT sample_types_sample_type_key UNIQUE (sample_type);


--
-- Name: samples samples_location_id_sub_location_id_media_id_z_datetime_sam_key; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_location_id_sub_location_id_media_id_z_datetime_sam_key UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, media_id, z, datetime, sample_type, collection_method);


--
-- Name: samples samples_pkey; Type: CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_pkey PRIMARY KEY (sample_id);


--
-- Name: field_visit_images field_visit_images_pkey; Type: CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visit_images
    ADD CONSTRAINT field_visit_images_pkey PRIMARY KEY (field_visit_id, image_id);


--
-- Name: field_visit_instruments field_visit_instruments_pkey; Type: CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visit_instruments
    ADD CONSTRAINT field_visit_instruments_pkey PRIMARY KEY (field_visit_id, instrument_id);


--
-- Name: field_visits field_visits_pkey; Type: CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visits
    ADD CONSTRAINT field_visits_pkey PRIMARY KEY (field_visit_id);


--
-- Name: field_visits unique_visit; Type: CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visits
    ADD CONSTRAINT unique_visit UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, start_datetime);


--
-- Name: document_types document_type_en_unique; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.document_types
    ADD CONSTRAINT document_type_en_unique UNIQUE (document_type_en);


--
-- Name: document_types document_type_fr_unique; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.document_types
    ADD CONSTRAINT document_type_fr_unique UNIQUE (document_type_fr);


--
-- Name: document_types document_types_pkey; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.document_types
    ADD CONSTRAINT document_types_pkey PRIMARY KEY (document_type_id);


--
-- Name: documents documents_name_key; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_name_key UNIQUE (name);


--
-- Name: documents documents_pkey; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_pkey PRIMARY KEY (document_id);


--
-- Name: documents_spatial documents_spatial_pkey; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents_spatial
    ADD CONSTRAINT documents_spatial_pkey PRIMARY KEY (document_id, geom_id);


--
-- Name: image_types image_types_pkey; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.image_types
    ADD CONSTRAINT image_types_pkey PRIMARY KEY (image_type_id);


--
-- Name: image_series images_index_pkey; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT images_index_pkey PRIMARY KEY (img_series_id);


--
-- Name: images images_pkey; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_pkey PRIMARY KEY (image_id);


--
-- Name: documents unique_document_hash; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT unique_document_hash UNIQUE (file_hash);


--
-- Name: images unique_image_hash; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT unique_image_hash UNIQUE (file_hash);


--
-- Name: image_series unique_location; Type: CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT unique_location UNIQUE (location_id);


--
-- Name: internal_status internal_status_pkey; Type: CONSTRAINT; Schema: information; Owner: postgres
--

ALTER TABLE ONLY information.internal_status
    ADD CONSTRAINT internal_status_pkey PRIMARY KEY (event);


--
-- Name: version_info version_info_pkey; Type: CONSTRAINT; Schema: information; Owner: postgres
--

ALTER TABLE ONLY information.version_info
    ADD CONSTRAINT version_info_pkey PRIMARY KEY (id);


--
-- Name: array_maintenance_changes array_maintenance_changes_pk; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_pk PRIMARY KEY (event_id);


--
-- Name: calibrate_depth calibrate_depth_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_depth
    ADD CONSTRAINT calibrate_depth_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_dissolved_oxygen calibrate_dissolved_oxygen_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_dissolved_oxygen
    ADD CONSTRAINT calibrate_dissolved_oxygen_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_orp calibrate_orp_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_orp
    ADD CONSTRAINT calibrate_orp_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_ph calibrate_ph_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_ph
    ADD CONSTRAINT calibrate_ph_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_specific_conductance calibrate_specific_conductance_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_specific_conductance
    ADD CONSTRAINT calibrate_specific_conductance_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_temperature calibrate_temperature_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_temperature
    ADD CONSTRAINT calibrate_temperature_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrate_turbidity calibrate_turbidity_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_turbidity
    ADD CONSTRAINT calibrate_turbidity_pkey PRIMARY KEY (calibration_id);


--
-- Name: calibrations calibrations_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_pkey PRIMARY KEY (calibration_id);


--
-- Name: instrument_maintenance instrument_maintenance_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instrument_maintenance
    ADD CONSTRAINT instrument_maintenance_pkey PRIMARY KEY (event_id);


--
-- Name: instrument_make instrument_make_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instrument_make
    ADD CONSTRAINT instrument_make_pkey PRIMARY KEY (make_id);


--
-- Name: instrument_model instrument_model_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instrument_model
    ADD CONSTRAINT instrument_model_pkey PRIMARY KEY (model_id);


--
-- Name: instrument_type instrument_type_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instrument_type
    ADD CONSTRAINT instrument_type_pkey PRIMARY KEY (type_id);


--
-- Name: instruments instruments_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_pkey PRIMARY KEY (instrument_id);


--
-- Name: instruments instruments_serial_no_key; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_serial_no_key UNIQUE (serial_no);


--
-- Name: observers observers_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.observers
    ADD CONSTRAINT observers_pkey PRIMARY KEY (observer_id);


--
-- Name: observers observers_unique; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.observers
    ADD CONSTRAINT observers_unique UNIQUE (observer_first, observer_last, organization);


--
-- Name: sensor_types sensor_types_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.sensor_types
    ADD CONSTRAINT sensor_types_pkey PRIMARY KEY (sensor_type_id);


--
-- Name: sensors sensors_pkey; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.sensors
    ADD CONSTRAINT sensors_pkey PRIMARY KEY (sensor_id);


--
-- Name: sensors sensors_sensor_serial_key; Type: CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.sensors
    ADD CONSTRAINT sensors_sensor_serial_key UNIQUE (sensor_serial);


--
-- Name: approval_types approval_types_approval_type_code_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.approval_types
    ADD CONSTRAINT approval_types_approval_type_code_key UNIQUE (approval_type_code);


--
-- Name: approval_types approval_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.approval_types
    ADD CONSTRAINT approval_types_pkey PRIMARY KEY (approval_type_id);


--
-- Name: datum_conversions datum_conversions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT datum_conversions_pkey PRIMARY KEY (conversion_id);


--
-- Name: datum_list datum_list_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.datum_list
    ADD CONSTRAINT datum_list_pkey PRIMARY KEY (datum_id);


--
-- Name: grade_types grade_types_grade_type_code_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.grade_types
    ADD CONSTRAINT grade_types_grade_type_code_key UNIQUE (grade_type_code);


--
-- Name: grade_types grade_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.grade_types
    ADD CONSTRAINT grade_types_pkey PRIMARY KEY (grade_type_id);


--
-- Name: location_types location_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.location_types
    ADD CONSTRAINT location_types_pkey PRIMARY KEY (type_id);


--
-- Name: location_types location_types_type_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.location_types
    ADD CONSTRAINT location_types_type_key UNIQUE (type);


--
-- Name: locations locations_id_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_id_pkey PRIMARY KEY (location_id);


--
-- Name: locations locations_location_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_location_key UNIQUE (location);


--
-- Name: locations_metadata_access locations_metadata_access_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_access locations_metadata_access_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwater_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwater_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwater_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwater_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_instruments locations_metadata_instruments_pkey; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_pkey PRIMARY KEY (metadata_id);


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_pkey PRIMARY KEY (location_maintenance_id);


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_xsections locations_metadata_xsections_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_pkey PRIMARY KEY (id);


--
-- Name: locations_metadata_xsections locations_metadata_xsections_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_unique UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id);


--
-- Name: locations locations_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_name_fr_key UNIQUE (name_fr);


--
-- Name: locations locations_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_name_key UNIQUE (name);


--
-- Name: locations_z locations_z_location_id_sub_location_id_z_meters_key; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_z
    ADD CONSTRAINT locations_z_location_id_sub_location_id_z_meters_key UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, z_meters);


--
-- Name: locations_z locations_z_pkey; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_z
    ADD CONSTRAINT locations_z_pkey PRIMARY KEY (z_id);


--
-- Name: network_project_types network_project_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.network_project_types
    ADD CONSTRAINT network_project_types_pkey PRIMARY KEY (id);


--
-- Name: locations_networks networks_locations_network_id_location_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_networks
    ADD CONSTRAINT networks_locations_network_id_location_id_key UNIQUE (network_id, location_id);


--
-- Name: networks networks_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_name_fr_key UNIQUE (name_fr);


--
-- Name: networks networks_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_name_key UNIQUE (name);


--
-- Name: networks networks_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_pkey PRIMARY KEY (network_id);


--
-- Name: locations_metadata_instruments no_overlap_location_instrument; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT no_overlap_location_instrument EXCLUDE USING gist (location_id WITH =, instrument_id WITH =, tstzrange(start_datetime, COALESCE(end_datetime, 'infinity'::timestamp with time zone), '[)'::text) WITH &&) WHERE ((sub_location_id IS NULL)) DEFERRABLE;


--
-- Name: locations_metadata_owners_operators no_overlap_location_only; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT no_overlap_location_only EXCLUDE USING gist (location_id WITH =, tstzrange(start_datetime, COALESCE(end_datetime, 'infinity'::timestamp with time zone), '[)'::text) WITH &&) WHERE ((sub_location_id IS NULL)) DEFERRABLE;


--
-- Name: locations_metadata_owners_operators no_overlap_sub_location; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT no_overlap_sub_location EXCLUDE USING gist (sub_location_id WITH =, tstzrange(start_datetime, COALESCE(end_datetime, 'infinity'::timestamp with time zone), '[)'::text) WITH &&) WHERE ((sub_location_id IS NOT NULL)) DEFERRABLE;


--
-- Name: locations_metadata_instruments no_overlap_sub_location_instrument; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT no_overlap_sub_location_instrument EXCLUDE USING gist (sub_location_id WITH =, instrument_id WITH =, tstzrange(start_datetime, COALESCE(end_datetime, 'infinity'::timestamp with time zone), '[)'::text) WITH &&) WHERE ((sub_location_id IS NOT NULL)) DEFERRABLE;


--
-- Name: organizations owners_contributors_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.organizations
    ADD CONSTRAINT owners_contributors_name_key UNIQUE (name);


--
-- Name: organizations owners_contributors_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.organizations
    ADD CONSTRAINT owners_contributors_pkey PRIMARY KEY (organization_id);


--
-- Name: media_types param_type_param_type_fr_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.media_types
    ADD CONSTRAINT param_type_param_type_fr_key UNIQUE (media_type_fr);


--
-- Name: media_types param_type_param_type_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.media_types
    ADD CONSTRAINT param_type_param_type_key UNIQUE (media_type);


--
-- Name: media_types param_type_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.media_types
    ADD CONSTRAINT param_type_pkey PRIMARY KEY (media_id);


--
-- Name: parameter_groups parameter_groups_group_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_groups
    ADD CONSTRAINT parameter_groups_group_name_key UNIQUE (group_name);


--
-- Name: parameter_groups parameter_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_groups
    ADD CONSTRAINT parameter_groups_pkey PRIMARY KEY (group_id);


--
-- Name: parameter_relationships parameter_relationships_param_code_group_id_sub_group_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_param_code_group_id_sub_group_id_key UNIQUE NULLS NOT DISTINCT (parameter_id, group_id, sub_group_id);


--
-- Name: parameter_relationships parameter_relationships_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_pkey PRIMARY KEY (relationship_id);


--
-- Name: parameter_sub_groups parameter_sub_groups_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_sub_groups
    ADD CONSTRAINT parameter_sub_groups_pkey PRIMARY KEY (sub_group_id);


--
-- Name: parameter_sub_groups parameter_sub_groups_unique; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_sub_groups
    ADD CONSTRAINT parameter_sub_groups_unique UNIQUE (sub_group_name);


--
-- Name: parameter_sub_groups parameter_sub_groups_unique_1; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_sub_groups
    ADD CONSTRAINT parameter_sub_groups_unique_1 UNIQUE (sub_group_name_fr);


--
-- Name: parameters parameters_param_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT parameters_param_name_fr_key UNIQUE (param_name_fr);


--
-- Name: parameters parameters_param_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT parameters_param_name_key UNIQUE (param_name);


--
-- Name: parameters parameters_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT parameters_pkey PRIMARY KEY (parameter_id);


--
-- Name: locations_projects projects_locations_project_id_location_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_projects
    ADD CONSTRAINT projects_locations_project_id_location_id_key UNIQUE (project_id, location_id);


--
-- Name: projects projects_name_fr_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_name_fr_key UNIQUE (name_fr);


--
-- Name: projects projects_name_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_name_key UNIQUE (name);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (project_id);


--
-- Name: qualifier_types qualifier_types_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qualifier_types
    ADD CONSTRAINT qualifier_types_pkey PRIMARY KEY (qualifier_type_id);


--
-- Name: qualifier_types qualifier_types_qualifier_type_code_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.qualifier_types
    ADD CONSTRAINT qualifier_types_qualifier_type_code_key UNIQUE (qualifier_type_code);


--
-- Name: sub_locations sub_locations_pkey; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.sub_locations
    ADD CONSTRAINT sub_locations_pkey PRIMARY KEY (sub_location_id);


--
-- Name: locations unique_location_id; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT unique_location_id UNIQUE (location_id);


--
-- Name: locations_metadata_instruments unique_location_instrument_period; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT unique_location_instrument_period UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, instrument_id, start_datetime, end_datetime);


--
-- Name: locations_metadata_owners_operators unique_location_ownership_period; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT unique_location_ownership_period UNIQUE NULLS NOT DISTINCT (location_id, sub_location_id, start_datetime, end_datetime);


--
-- Name: sub_locations unique_subloc_per_location; Type: CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.sub_locations
    ADD CONSTRAINT unique_subloc_per_location UNIQUE (location_id, sub_location_id);


--
-- Name: raster_series_index raster_series_index_model_parameter_key; Type: CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.raster_series_index
    ADD CONSTRAINT raster_series_index_model_parameter_key UNIQUE (model, parameter);


--
-- Name: raster_series_index raster_series_index_pkey; Type: CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.raster_series_index
    ADD CONSTRAINT raster_series_index_pkey PRIMARY KEY (raster_series_id);


--
-- Name: rasters rasters_pkey; Type: CONSTRAINT; Schema: spatial; Owner: admin
--

ALTER TABLE ONLY spatial.rasters
    ADD CONSTRAINT rasters_pkey PRIMARY KEY (rid);


--
-- Name: rasters_reference rasters_reference_pkey; Type: CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.rasters_reference
    ADD CONSTRAINT rasters_reference_pkey PRIMARY KEY (reference_id);


--
-- Name: rasters_reference rasters_reference_raster_series_id_flag_valid_from_valid_to_key; Type: CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.rasters_reference
    ADD CONSTRAINT rasters_reference_raster_series_id_flag_valid_from_valid_to_key UNIQUE NULLS NOT DISTINCT (raster_series_id, flag, valid_from, valid_to, issued);


--
-- Name: vectors vectors_layer_name_feature_name_geom_type_key; Type: CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.vectors
    ADD CONSTRAINT vectors_layer_name_feature_name_geom_type_key UNIQUE (layer_name, feature_name, geom_type);


--
-- Name: vectors vectors_pkey; Type: CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.vectors
    ADD CONSTRAINT vectors_pkey PRIMARY KEY (geom_id);


--
-- Name: idx_borehole_lat_lon; Type: INDEX; Schema: boreholes; Owner: postgres
--

CREATE INDEX idx_borehole_lat_lon ON boreholes.boreholes USING btree (latitude, longitude);


--
-- Name: idx_borehole_name; Type: INDEX; Schema: boreholes; Owner: postgres
--

CREATE INDEX idx_borehole_name ON boreholes.boreholes USING btree (borehole_name);


--
-- Name: idx_borehole_no_coords_lat_lon; Type: INDEX; Schema: boreholes; Owner: postgres
--

CREATE INDEX idx_borehole_no_coords_lat_lon ON boreholes.boreholes_no_coords USING btree (latitude, longitude);


--
-- Name: idx_borehole_no_coords_name; Type: INDEX; Schema: boreholes; Owner: postgres
--

CREATE INDEX idx_borehole_no_coords_name ON boreholes.boreholes_no_coords USING btree (borehole_name);


--
-- Name: idx_approvals_timeseries_time; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_approvals_timeseries_time ON continuous.approvals USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_contributors_timeseries_time; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_contributors_timeseries_time ON continuous.contributors USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_corrections_timeseries_date_range; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_corrections_timeseries_date_range ON continuous.corrections USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_corrections_timeseries_range_gist; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_corrections_timeseries_range_gist ON continuous.corrections USING gist (timeseries_id, tstzrange(start_dt, end_dt));


--
-- Name: idx_grades_timeseries_time; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_grades_timeseries_time ON continuous.grades USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_owners_timeseries_time; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_owners_timeseries_time ON continuous.owners USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: idx_qualifiers_timeseries_time; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX idx_qualifiers_timeseries_time ON continuous.qualifiers USING btree (timeseries_id, start_dt, end_dt);


--
-- Name: measurements_calculated_daily_date_idx; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX measurements_calculated_daily_date_idx ON continuous.measurements_calculated_daily USING btree (date);


--
-- Name: measurements_continuous_datetime_idx; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX measurements_continuous_datetime_idx ON continuous.measurements_continuous USING btree (datetime);


--
-- Name: measurements_continuous_hour_idx; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX measurements_continuous_hour_idx ON continuous.measurements_continuous USING btree (timeseries_id, continuous.trunc_hour_utc(datetime));


--
-- Name: timeseries_share_with_gin_idx; Type: INDEX; Schema: continuous; Owner: postgres
--

CREATE INDEX timeseries_share_with_gin_idx ON continuous.timeseries USING gin (share_with);


--
-- Name: idx_results_parameter_id; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_results_parameter_id ON discrete.results USING btree (parameter_id);


--
-- Name: idx_results_result_condition; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_results_result_condition ON discrete.results USING btree (result_condition);


--
-- Name: idx_results_sample_id; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_results_sample_id ON discrete.results USING btree (sample_id);


--
-- Name: idx_results_sample_parameter; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_results_sample_parameter ON discrete.results USING btree (sample_id, parameter_id);


--
-- Name: idx_samples_datetime; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_samples_datetime ON discrete.samples USING btree (datetime);


--
-- Name: idx_samples_location_id; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_samples_location_id ON discrete.samples USING btree (location_id);


--
-- Name: idx_samples_media_id; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX idx_samples_media_id ON discrete.samples USING btree (media_id);


--
-- Name: results_share_with_gin_idx; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX results_share_with_gin_idx ON discrete.results USING gin (share_with);


--
-- Name: samples_share_with_gin_idx; Type: INDEX; Schema: discrete; Owner: admin
--

CREATE INDEX samples_share_with_gin_idx ON discrete.samples USING gin (share_with);


--
-- Name: field_visits_location_id_idx; Type: INDEX; Schema: field; Owner: postgres
--

CREATE INDEX field_visits_location_id_idx ON field.field_visits USING btree (location_id);


--
-- Name: field_visits_start_datetime_idx; Type: INDEX; Schema: field; Owner: postgres
--

CREATE INDEX field_visits_start_datetime_idx ON field.field_visits USING btree (start_datetime);


--
-- Name: timeseries_share_with_gin_idx; Type: INDEX; Schema: field; Owner: postgres
--

CREATE INDEX timeseries_share_with_gin_idx ON field.field_visits USING gin (share_with);


--
-- Name: documents_share_with_gin_idx; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX documents_share_with_gin_idx ON files.documents USING gin (share_with);


--
-- Name: documents_type_idx; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX documents_type_idx ON files.documents USING btree (type);


--
-- Name: idx_documents_tags; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX idx_documents_tags ON files.documents USING gin (tags);


--
-- Name: idx_images_tags; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX idx_images_tags ON files.images USING gin (tags);


--
-- Name: images_datetime_idx; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX images_datetime_idx ON files.images USING btree (datetime);


--
-- Name: images_index_share_with_gin_idx; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX images_index_share_with_gin_idx ON files.image_series USING gin (share_with);


--
-- Name: images_location_id_idx; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX images_location_id_idx ON files.images USING btree (location_id);


--
-- Name: images_share_with_gin_idx; Type: INDEX; Schema: files; Owner: postgres
--

CREATE INDEX images_share_with_gin_idx ON files.images USING gin (share_with);


--
-- Name: locations_share_with_gin_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX locations_share_with_gin_idx ON public.locations USING gin (share_with);


--
-- Name: locations_z_location_id_idx; Type: INDEX; Schema: public; Owner: admin
--

CREATE INDEX locations_z_location_id_idx ON public.locations_z USING btree (location_id);


--
-- Name: sub_locations_share_with_gin_idx; Type: INDEX; Schema: public; Owner: admin
--

CREATE INDEX sub_locations_share_with_gin_idx ON public.sub_locations USING gin (share_with);


--
-- Name: geometry_idx; Type: INDEX; Schema: spatial; Owner: postgres
--

CREATE INDEX geometry_idx ON spatial.vectors USING gist (geom);


--
-- Name: rasters_rast_st_conhull_idx; Type: INDEX; Schema: spatial; Owner: admin
--

CREATE INDEX rasters_rast_st_conhull_idx ON spatial.rasters USING gist (spatial.st_convexhull(rast));


--
-- Name: rasters_reference_raster_series_id_idx; Type: INDEX; Schema: spatial; Owner: postgres
--

CREATE INDEX rasters_reference_raster_series_id_idx ON spatial.rasters_reference USING btree (raster_series_id);


--
-- Name: rasters_reference_valid_from_idx; Type: INDEX; Schema: spatial; Owner: postgres
--

CREATE INDEX rasters_reference_valid_from_idx ON spatial.rasters_reference USING btree (valid_from);


--
-- Name: rasters_reference_valid_to_idx; Type: INDEX; Schema: spatial; Owner: postgres
--

CREATE INDEX rasters_reference_valid_to_idx ON spatial.rasters_reference USING btree (valid_to);


--
-- Name: page_content trg_page_content_integrity; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER trg_page_content_integrity BEFORE INSERT OR UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.check_page_content_integrity();


--
-- Name: images trg_user_audit; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON application.images FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: page_content trg_user_audit; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: text trg_user_audit; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON application.text FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: images update_image_modified; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER update_image_modified BEFORE UPDATE ON application.images FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: page_content update_page_content_modified; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER update_page_content_modified BEFORE UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: text update_text_modified; Type: TRIGGER; Schema: application; Owner: admin
--

CREATE TRIGGER update_text_modified BEFORE UPDATE ON application.text FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: geology trg_geology_no_overlap; Type: TRIGGER; Schema: boreholes; Owner: postgres
--

CREATE TRIGGER trg_geology_no_overlap BEFORE INSERT OR UPDATE ON boreholes.geology FOR EACH ROW EXECUTE FUNCTION boreholes.prevent_geology_overlap();


--
-- Name: permafrost trg_permafrost_no_overlap; Type: TRIGGER; Schema: boreholes; Owner: postgres
--

CREATE TRIGGER trg_permafrost_no_overlap BEFORE INSERT OR UPDATE ON boreholes.permafrost FOR EACH ROW EXECUTE FUNCTION boreholes.prevent_permafrost_overlap();


--
-- Name: borehole_well_purposes trg_user_audit; Type: TRIGGER; Schema: boreholes; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON boreholes.borehole_well_purposes FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: borehole_well_purposes update_modify_time; Type: TRIGGER; Schema: boreholes; Owner: postgres
--

CREATE TRIGGER update_modify_time BEFORE UPDATE ON boreholes.borehole_well_purposes FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: boreholes validate_share_with_trigger; Type: TRIGGER; Schema: boreholes; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON boreholes.boreholes FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: wells validate_share_with_trigger; Type: TRIGGER; Schema: boreholes; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON boreholes.wells FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: approvals check_approvals_overlap; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE CONSTRAINT TRIGGER check_approvals_overlap AFTER INSERT OR UPDATE ON continuous.approvals DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_approvals_overlap();


--
-- Name: contributors check_contributors_overlap; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE CONSTRAINT TRIGGER check_contributors_overlap AFTER INSERT OR UPDATE ON continuous.contributors DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_contributors_overlap();


--
-- Name: grades check_grades_overlap; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE CONSTRAINT TRIGGER check_grades_overlap AFTER INSERT OR UPDATE ON continuous.grades DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_grades_overlap();


--
-- Name: owners check_owners_overlap; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE CONSTRAINT TRIGGER check_owners_overlap AFTER INSERT OR UPDATE ON continuous.owners DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_owners_overlap();


--
-- Name: qualifiers check_qualifiers_overlap; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE CONSTRAINT TRIGGER check_qualifiers_overlap AFTER INSERT OR UPDATE ON continuous.qualifiers DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION continuous.check_qualifiers_overlap();


--
-- Name: correction_types trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.correction_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: corrections trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.corrections FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: extrema trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.extrema FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: thresholds trg_user_audit; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON continuous.thresholds FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: approvals update_approvals_updated; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_approvals_updated BEFORE UPDATE ON continuous.approvals FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: contributors update_contributors_updated; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_contributors_updated BEFORE UPDATE ON continuous.contributors FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: corrections update_corrections_updated; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_corrections_updated BEFORE UPDATE ON continuous.corrections FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: grades update_grades_updated; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_grades_updated BEFORE UPDATE ON continuous.grades FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: measurements_calculated_daily update_measurements_calculated_daily_created_modified; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_measurements_calculated_daily_created_modified BEFORE INSERT OR UPDATE ON continuous.measurements_calculated_daily FOR EACH ROW EXECUTE FUNCTION public.update_created_modified();


--
-- Name: measurements_continuous update_measurements_continuous_created_modified; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_measurements_continuous_created_modified BEFORE INSERT OR UPDATE ON continuous.measurements_continuous FOR EACH ROW EXECUTE FUNCTION public.update_created_modified();


--
-- Name: owners update_owners_updated; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_owners_updated BEFORE UPDATE ON continuous.owners FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: qualifiers update_qualifiers_updated; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_qualifiers_updated BEFORE UPDATE ON continuous.qualifiers FOR EACH ROW EXECUTE FUNCTION public.update_updated();


--
-- Name: rating_curve_shifts update_rating_curve_shifts_modified; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_rating_curve_shifts_modified BEFORE UPDATE ON continuous.rating_curve_shifts FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: rating_curves update_rating_curves_modified; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER update_rating_curves_modified BEFORE UPDATE ON continuous.rating_curves FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: corrections validate_corrections_trigger; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER validate_corrections_trigger BEFORE INSERT OR UPDATE ON continuous.corrections FOR EACH ROW EXECUTE FUNCTION continuous.validate_corrections();


--
-- Name: timeseries validate_share_with_trigger; Type: TRIGGER; Schema: continuous; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON continuous.timeseries FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: guidelines trg_check_sql; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_check_sql BEFORE INSERT OR UPDATE OF guideline_sql ON discrete.guidelines FOR EACH ROW EXECUTE FUNCTION discrete.guidelines_validate_trg();


--
-- Name: guidelines trg_enforce_result_speciation; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_enforce_result_speciation BEFORE INSERT OR UPDATE ON discrete.guidelines FOR EACH ROW EXECUTE FUNCTION discrete.enforce_result_speciation();


--
-- Name: results trg_enforce_result_speciation; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trg_enforce_result_speciation BEFORE INSERT OR UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION discrete.enforce_result_speciation();


--
-- Name: guidelines trg_enforce_sample_fraction; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_enforce_sample_fraction BEFORE INSERT OR UPDATE ON discrete.guidelines FOR EACH ROW EXECUTE FUNCTION discrete.enforce_sample_fraction();


--
-- Name: results trg_enforce_sample_fraction; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trg_enforce_sample_fraction BEFORE INSERT OR UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION discrete.enforce_sample_fraction();


--
-- Name: collection_methods trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.collection_methods FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: guidelines trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.guidelines FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: laboratories trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.laboratories FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: protocols_methods trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.protocols_methods FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_conditions trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_conditions FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_speciations trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_speciations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_types trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: result_value_types trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.result_value_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: results trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_fractions trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.sample_fractions FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_series trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.sample_series FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_types trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.sample_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: samples trg_user_audit; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sample_series trigger_check_sample_series_overlap; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER trigger_check_sample_series_overlap BEFORE INSERT OR UPDATE ON discrete.sample_series FOR EACH ROW EXECUTE FUNCTION discrete.check_sample_series_overlap();


--
-- Name: guidelines update_modify_time; Type: TRIGGER; Schema: discrete; Owner: postgres
--

CREATE TRIGGER update_modify_time BEFORE UPDATE ON discrete.guidelines FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: results update_results_modified; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER update_results_modified BEFORE UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: samples update_samples_modified; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER update_samples_modified BEFORE UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: samples validate_documents_trigger; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER validate_documents_trigger BEFORE INSERT OR UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.validate_documents_array();


--
-- Name: guideline_values validate_guideline_start_end_trg; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER validate_guideline_start_end_trg BEFORE INSERT OR UPDATE ON discrete.guideline_values FOR EACH ROW EXECUTE FUNCTION public.validate_guideline_start_end();


--
-- Name: guideline_values validate_guideline_values_trg; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER validate_guideline_values_trg BEFORE INSERT OR UPDATE ON discrete.guideline_values FOR EACH ROW EXECUTE FUNCTION public.validate_guideline_values();


--
-- Name: results validate_share_with_trigger; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON discrete.results FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: samples validate_share_with_trigger; Type: TRIGGER; Schema: discrete; Owner: admin
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON discrete.samples FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: field_visit_images trg_user_audit; Type: TRIGGER; Schema: field; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON field.field_visit_images FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: field_visit_instruments trg_user_audit; Type: TRIGGER; Schema: field; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON field.field_visit_instruments FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: field_visits trg_user_audit; Type: TRIGGER; Schema: field; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON field.field_visits FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: field_visit_images update_modify_time; Type: TRIGGER; Schema: field; Owner: postgres
--

CREATE TRIGGER update_modify_time BEFORE UPDATE ON field.field_visit_images FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: field_visit_instruments update_modify_time; Type: TRIGGER; Schema: field; Owner: postgres
--

CREATE TRIGGER update_modify_time BEFORE UPDATE ON field.field_visit_instruments FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: field_visits update_modify_time; Type: TRIGGER; Schema: field; Owner: postgres
--

CREATE TRIGGER update_modify_time BEFORE UPDATE ON field.field_visits FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: documents_spatial documents_spatial_after_delete; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER documents_spatial_after_delete AFTER DELETE ON files.documents_spatial FOR EACH ROW EXECUTE FUNCTION files.update_document_flags_after_delete();


--
-- Name: documents_spatial documents_spatial_after_insert; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER documents_spatial_after_insert AFTER INSERT ON files.documents_spatial FOR EACH ROW EXECUTE FUNCTION files.update_document_flags_after_insert();


--
-- Name: document_types trg_user_audit; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.document_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: documents trg_user_audit; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.documents FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: documents_spatial trg_user_audit; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.documents_spatial FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: image_series trg_user_audit; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.image_series FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: image_types trg_user_audit; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.image_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: images trg_user_audit; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON files.images FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: images trigger_enforce_share_with_restriction; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER trigger_enforce_share_with_restriction BEFORE INSERT OR UPDATE ON files.images FOR EACH ROW EXECUTE FUNCTION files.enforce_share_with_restriction();


--
-- Name: documents validate_share_with_trigger; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON files.documents FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: image_series validate_share_with_trigger; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON files.image_series FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: images validate_share_with_trigger; Type: TRIGGER; Schema: files; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON files.images FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: version_info update_version_info_created_modified; Type: TRIGGER; Schema: information; Owner: postgres
--

CREATE TRIGGER update_version_info_created_modified BEFORE INSERT OR UPDATE ON information.version_info FOR EACH ROW EXECUTE FUNCTION public.update_created_modified();


--
-- Name: array_maintenance_changes trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.array_maintenance_changes FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_depth trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_depth FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_dissolved_oxygen trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_dissolved_oxygen FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_orp trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_orp FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_ph trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_ph FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_specific_conductance trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_specific_conductance FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_temperature trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_temperature FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrate_turbidity trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrate_turbidity FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: calibrations trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.calibrations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_maintenance trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_maintenance FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_make trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_make FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_model trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_model FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instrument_type trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instrument_type FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: instruments trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.instruments FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: observers trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.observers FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sensor_types trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.sensor_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sensors trg_user_audit; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON instruments.sensors FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: array_maintenance_changes update_array_maintenance_changes_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_array_maintenance_changes_modify_datetime BEFORE UPDATE ON instruments.array_maintenance_changes FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_depth update_calibrate_depth_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_depth_modify_datetime BEFORE UPDATE ON instruments.calibrate_depth FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_dissolved_oxygen update_calibrate_dissolved_oxygen_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_dissolved_oxygen_modify_datetime BEFORE UPDATE ON instruments.calibrate_dissolved_oxygen FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_orp update_calibrate_orp_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_orp_modify_datetime BEFORE UPDATE ON instruments.calibrate_orp FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_ph update_calibrate_ph_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_ph_modify_datetime BEFORE UPDATE ON instruments.calibrate_ph FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_specific_conductance update_calibrate_specific_conductance_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_specific_conductance_modify_datetime BEFORE UPDATE ON instruments.calibrate_specific_conductance FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_temperature update_calibrate_temperature_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_temperature_modify_datetime BEFORE UPDATE ON instruments.calibrate_temperature FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrate_turbidity update_calibrate_turbidity_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrate_turbidity_modify_datetime BEFORE UPDATE ON instruments.calibrate_turbidity FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: calibrations update_calibrations_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_calibrations_modify_datetime BEFORE UPDATE ON instruments.calibrations FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_maintenance update_instrument_maintenance_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_instrument_maintenance_modify_datetime BEFORE UPDATE ON instruments.instrument_maintenance FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_make update_instrument_make_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_instrument_make_modify_datetime BEFORE UPDATE ON instruments.instrument_make FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_model update_instrument_model_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_instrument_model_modify_datetime BEFORE UPDATE ON instruments.instrument_model FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instrument_type update_instrument_type_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_instrument_type_modify_datetime BEFORE UPDATE ON instruments.instrument_type FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: instruments update_instruments_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_instruments_modify_datetime BEFORE UPDATE ON instruments.instruments FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: observers update_observers_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_observers_modify_datetime BEFORE UPDATE ON instruments.observers FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: sensor_types update_sensor_types_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_sensor_types_modify_datetime BEFORE UPDATE ON instruments.sensor_types FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: sensors update_sensors_modify_datetime; Type: TRIGGER; Schema: instruments; Owner: postgres
--

CREATE TRIGGER update_sensors_modify_datetime BEFORE UPDATE ON instruments.sensors FOR EACH ROW EXECUTE FUNCTION instruments.update_modify_datetime();


--
-- Name: locations_metadata_instruments check_instrument_meta_overlap; Type: TRIGGER; Schema: public; Owner: admin
--

CREATE CONSTRAINT TRIGGER check_instrument_meta_overlap AFTER INSERT OR UPDATE ON public.locations_metadata_instruments DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.check_instrument_meta_overlap();


--
-- Name: locations_metadata_access fill_locations_metadata_access_missing_trigger; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER fill_locations_metadata_access_missing_trigger BEFORE INSERT ON public.locations_metadata_access FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_access_missing();


--
-- Name: locations_metadata_infrastructure_groundwater fill_locations_metadata_infrastructure_groundwater_trigger; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER fill_locations_metadata_infrastructure_groundwater_trigger BEFORE INSERT ON public.locations_metadata_infrastructure_groundwater FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_groundwater();


--
-- Name: locations_metadata_infrastructure_hydromet fill_locations_metadata_infrastructure_hydromet_trigger; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER fill_locations_metadata_infrastructure_hydromet_trigger BEFORE INSERT ON public.locations_metadata_infrastructure_hydromet FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_hydromet();


--
-- Name: locations_metadata_infrastructure fill_locations_metadata_infrastructure_missing_trigger; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER fill_locations_metadata_infrastructure_missing_trigger BEFORE INSERT ON public.locations_metadata_infrastructure FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_infrastructure_missing();


--
-- Name: locations_metadata_owners_operators fill_locations_metadata_owners_operators_missing_trigger; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER fill_locations_metadata_owners_operators_missing_trigger BEFORE INSERT ON public.locations_metadata_owners_operators FOR EACH ROW EXECUTE FUNCTION public.fill_locations_metadata_owners_operators_missing();


--
-- Name: locations trg_check_data_sharing_agreement; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_check_data_sharing_agreement BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION files.check_data_sharing_agreement();


--
-- Name: approval_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.approval_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: datum_conversions trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.datum_conversions FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: datum_list trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.datum_list FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: grade_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.grade_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: location_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.location_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_access trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_access FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_infrastructure trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_infrastructure FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_infrastructure_groundwater trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_infrastructure_groundwater FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_infrastructure_hydromet trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_infrastructure_hydromet FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_instruments trg_user_audit; Type: TRIGGER; Schema: public; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_instruments FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_maintenance trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_maintenance FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_owners_operators trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_owners_operators FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_metadata_xsections trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_metadata_xsections FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_networks trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_networks FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations_projects trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.locations_projects FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: media_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.media_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: network_project_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.network_project_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: networks trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.networks FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: organizations trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.organizations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: parameter_groups trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.parameter_groups FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: parameter_sub_groups trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.parameter_sub_groups FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: parameters trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.parameters FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: projects trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.projects FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: qualifier_types trg_user_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.qualifier_types FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: sub_locations trg_user_audit; Type: TRIGGER; Schema: public; Owner: admin
--

CREATE TRIGGER trg_user_audit BEFORE UPDATE ON public.sub_locations FOR EACH ROW EXECUTE FUNCTION public.user_modified();


--
-- Name: locations trigger_check_location_images; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trigger_check_location_images BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION files.check_location_images();


--
-- Name: locations_metadata_maintenance trigger_enforce_maintenance_constraints; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER trigger_enforce_maintenance_constraints BEFORE INSERT OR UPDATE ON public.locations_metadata_maintenance FOR EACH ROW EXECUTE FUNCTION public.enforce_maintenance_constraints();


--
-- Name: locations_metadata_access update_locations_metadata_access_modified; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_locations_metadata_access_modified BEFORE UPDATE ON public.locations_metadata_access FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_infrastructure_groundwater update_locations_metadata_infrastructure_groundwater_modified; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_locations_metadata_infrastructure_groundwater_modified BEFORE UPDATE ON public.locations_metadata_infrastructure_groundwater FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_infrastructure_hydromet update_locations_metadata_infrastructure_hydromet_modified; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_locations_metadata_infrastructure_hydromet_modified BEFORE UPDATE ON public.locations_metadata_infrastructure_hydromet FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_infrastructure update_locations_metadata_infrastructure_modified; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_locations_metadata_infrastructure_modified BEFORE UPDATE ON public.locations_metadata_infrastructure FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_instruments update_locations_metadata_instruments_modified; Type: TRIGGER; Schema: public; Owner: admin
--

CREATE TRIGGER update_locations_metadata_instruments_modified BEFORE UPDATE ON public.locations_metadata_instruments FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_owners_operators update_locations_metadata_owners_operators_modified; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_locations_metadata_owners_operators_modified BEFORE UPDATE ON public.locations_metadata_owners_operators FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations_metadata_xsections update_locations_metadata_xsections_modified; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER update_locations_metadata_xsections_modified BEFORE UPDATE ON public.locations_metadata_xsections FOR EACH ROW EXECUTE FUNCTION public.update_modified();


--
-- Name: locations validate_share_with_trigger; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: sub_locations validate_share_with_trigger; Type: TRIGGER; Schema: public; Owner: admin
--

CREATE TRIGGER validate_share_with_trigger BEFORE INSERT OR UPDATE ON public.sub_locations FOR EACH ROW EXECUTE FUNCTION public.validate_share_with();


--
-- Name: vectors update_geom_type_trigger; Type: TRIGGER; Schema: spatial; Owner: postgres
--

CREATE TRIGGER update_geom_type_trigger BEFORE INSERT OR UPDATE ON spatial.vectors FOR EACH ROW EXECUTE FUNCTION spatial.update_geom_type();


--
-- Name: boreholes boreholes_borehole_well_purpose_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes
    ADD CONSTRAINT boreholes_borehole_well_purpose_id_fkey FOREIGN KEY (borehole_well_purpose_id) REFERENCES boreholes.borehole_well_purposes(borehole_well_purpose_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: boreholes_documents boreholes_documents_borehole_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_documents
    ADD CONSTRAINT boreholes_documents_borehole_id_fkey FOREIGN KEY (borehole_id) REFERENCES boreholes.boreholes(borehole_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: boreholes_documents boreholes_documents_document_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_documents
    ADD CONSTRAINT boreholes_documents_document_id_fkey FOREIGN KEY (document_id) REFERENCES files.documents(document_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: boreholes boreholes_drilled_by_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes
    ADD CONSTRAINT boreholes_drilled_by_fkey FOREIGN KEY (drilled_by) REFERENCES boreholes.drillers(driller_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: boreholes boreholes_location_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes
    ADD CONSTRAINT boreholes_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: boreholes_no_coords_documents boreholes_no_coords_documents_borehole_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_no_coords_documents
    ADD CONSTRAINT boreholes_no_coords_documents_borehole_id_fkey FOREIGN KEY (borehole_id) REFERENCES boreholes.boreholes_no_coords(borehole_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: boreholes_no_coords_documents boreholes_no_coords_documents_document_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_no_coords_documents
    ADD CONSTRAINT boreholes_no_coords_documents_document_id_fkey FOREIGN KEY (document_id) REFERENCES files.documents(document_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: boreholes_no_coords boreholes_no_coords_drilled_by_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.boreholes_no_coords
    ADD CONSTRAINT boreholes_no_coords_drilled_by_fkey FOREIGN KEY (drilled_by) REFERENCES boreholes.drillers(driller_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: geology geology_borehole_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.geology
    ADD CONSTRAINT geology_borehole_id_fkey FOREIGN KEY (borehole_id) REFERENCES boreholes.boreholes(borehole_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: permafrost permafrost_borehole_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.permafrost
    ADD CONSTRAINT permafrost_borehole_id_fkey FOREIGN KEY (borehole_id) REFERENCES boreholes.boreholes(borehole_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wells wells_borehole_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.wells
    ADD CONSTRAINT wells_borehole_id_fkey FOREIGN KEY (borehole_id) REFERENCES boreholes.boreholes(borehole_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: wells wells_borehole_well_purpose_id_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.wells
    ADD CONSTRAINT wells_borehole_well_purpose_id_fkey FOREIGN KEY (borehole_well_purpose_id) REFERENCES boreholes.borehole_well_purposes(borehole_well_purpose_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: wells wells_casing_material_fkey; Type: FK CONSTRAINT; Schema: boreholes; Owner: postgres
--

ALTER TABLE ONLY boreholes.wells
    ADD CONSTRAINT wells_casing_material_fkey FOREIGN KEY (casing_material) REFERENCES boreholes.casing_materials(casing_material_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: approvals approvals_approval_type_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.approvals
    ADD CONSTRAINT approvals_approval_type_id_fkey FOREIGN KEY (approval_type_id) REFERENCES public.approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: approvals approvals_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.approvals
    ADD CONSTRAINT approvals_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contributors contributors_owner_contributor_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.contributors
    ADD CONSTRAINT contributors_owner_contributor_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contributors contributors_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.contributors
    ADD CONSTRAINT contributors_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: corrections corrections_correction_type_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_correction_type_fkey FOREIGN KEY (correction_type) REFERENCES continuous.correction_types(correction_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: corrections corrections_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.corrections
    ADD CONSTRAINT corrections_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_aggregation_type; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_aggregation_type FOREIGN KEY (aggregation_type_id) REFERENCES continuous.aggregation_types(aggregation_type_id);


--
-- Name: timeseries fk_location; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_location FOREIGN KEY (location) REFERENCES public.locations(location) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_location_id; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_location_id FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_media_type; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_media_type FOREIGN KEY (media_id) REFERENCES public.media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_parameter; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_parameter FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: measurements_continuous fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.measurements_continuous
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: measurements_calculated_daily fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.measurements_calculated_daily
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: extrema fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.extrema
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: forecasts fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.forecasts
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: thresholds fk_timeseries_id; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.thresholds
    ADD CONSTRAINT fk_timeseries_id FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: timeseries fk_timeseries_z; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT fk_timeseries_z FOREIGN KEY (z_id) REFERENCES public.locations_z(z_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: grades grades_grade_type_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.grades
    ADD CONSTRAINT grades_grade_type_id_fkey FOREIGN KEY (grade_type_id) REFERENCES public.grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: grades grades_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.grades
    ADD CONSTRAINT grades_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: owners owners_owner_contributor_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.owners
    ADD CONSTRAINT owners_owner_contributor_id_fkey FOREIGN KEY (organization_id) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: owners owners_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.owners
    ADD CONSTRAINT owners_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: qualifiers qualifiers_qualifier_type_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.qualifiers
    ADD CONSTRAINT qualifiers_qualifier_type_id_fkey FOREIGN KEY (qualifier_type_id) REFERENCES public.qualifier_types(qualifier_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: qualifiers qualifiers_timeseries_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.qualifiers
    ADD CONSTRAINT qualifiers_timeseries_id_fkey FOREIGN KEY (timeseries_id) REFERENCES continuous.timeseries(timeseries_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rating_curve_points rating_curve_points_curve_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_points
    ADD CONSTRAINT rating_curve_points_curve_id_fkey FOREIGN KEY (curve_id) REFERENCES continuous.rating_curves(curve_id);


--
-- Name: rating_curve_shifts rating_curve_shifts_curve_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curve_shifts
    ADD CONSTRAINT rating_curve_shifts_curve_id_fkey FOREIGN KEY (curve_id) REFERENCES continuous.rating_curves(curve_id);


--
-- Name: rating_curves rating_curves_approval_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_approval_fkey FOREIGN KEY (approval) REFERENCES public.approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rating_curves rating_curves_input_parameter_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_input_parameter_id_fkey FOREIGN KEY (input_parameter_id) REFERENCES public.parameters(parameter_id);


--
-- Name: rating_curves rating_curves_location_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: rating_curves rating_curves_output_parameter_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.rating_curves
    ADD CONSTRAINT rating_curves_output_parameter_id_fkey FOREIGN KEY (output_parameter_id) REFERENCES public.parameters(parameter_id);


--
-- Name: timeseries timeseries_default_owner_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_default_owner_fkey FOREIGN KEY (default_owner) REFERENCES public.organizations(organization_id);


--
-- Name: timeseries timeseries_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: continuous; Owner: postgres
--

ALTER TABLE ONLY continuous.timeseries
    ADD CONSTRAINT timeseries_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: guidelines guidelines_parameter_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.guidelines
    ADD CONSTRAINT guidelines_parameter_id_fkey FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: guidelines guidelines_result_speciation_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.guidelines
    ADD CONSTRAINT guidelines_result_speciation_id_fkey FOREIGN KEY (result_speciation_id) REFERENCES discrete.result_speciations(result_speciation_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: guidelines guidelines_sample_fraction_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: postgres
--

ALTER TABLE ONLY discrete.guidelines
    ADD CONSTRAINT guidelines_sample_fraction_id_fkey FOREIGN KEY (sample_fraction_id) REFERENCES discrete.sample_fractions(sample_fraction_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_laboratory_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_laboratory_fkey FOREIGN KEY (laboratory) REFERENCES discrete.laboratories(lab_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_parameter_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_parameter_id_fkey FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: results results_protocol_method_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_protocol_method_fkey FOREIGN KEY (protocol_method) REFERENCES discrete.protocols_methods(protocol_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_result_condition_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_condition_fkey FOREIGN KEY (result_condition) REFERENCES discrete.result_conditions(result_condition_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: results results_result_speciation_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_speciation_fkey FOREIGN KEY (result_speciation_id) REFERENCES discrete.result_speciations(result_speciation_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_result_type_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_type_fkey FOREIGN KEY (result_type) REFERENCES discrete.result_types(result_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: results results_result_value_type_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_result_value_type_fkey FOREIGN KEY (result_value_type) REFERENCES discrete.result_value_types(result_value_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_sample_fraction_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_sample_fraction_fkey FOREIGN KEY (sample_fraction_id) REFERENCES discrete.sample_fractions(sample_fraction_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: results results_sample_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.results
    ADD CONSTRAINT results_sample_id_fkey FOREIGN KEY (sample_id) REFERENCES discrete.samples(sample_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: sample_series sample_series_default_contributor_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_default_contributor_fkey FOREIGN KEY (default_contributor) REFERENCES public.organizations(organization_id);


--
-- Name: sample_series sample_series_default_owner_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_default_owner_fkey FOREIGN KEY (default_owner) REFERENCES public.organizations(organization_id);


--
-- Name: sample_series sample_series_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: sample_series sample_series_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.sample_series
    ADD CONSTRAINT sample_series_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: samples samples_collection_method_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_collection_method_fkey FOREIGN KEY (collection_method) REFERENCES discrete.collection_methods(collection_method_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_comissioning_org_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_comissioning_org_fkey FOREIGN KEY (comissioning_org) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_contributor_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_contributor_fkey FOREIGN KEY (contributor) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_field_visit_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_field_visit_id_fkey FOREIGN KEY (field_visit_id) REFERENCES field.field_visits(field_visit_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_linked_with_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_linked_with_fkey FOREIGN KEY (linked_with) REFERENCES discrete.samples(sample_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_media_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_media_id_fkey FOREIGN KEY (media_id) REFERENCES public.media_types(media_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_owner_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_sample_approval_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_approval_fkey FOREIGN KEY (sample_approval) REFERENCES public.approval_types(approval_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sample_grade_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_grade_fkey FOREIGN KEY (sample_grade) REFERENCES public.grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sample_qualifier_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_qualifier_fkey FOREIGN KEY (sample_qualifier) REFERENCES public.qualifier_types(qualifier_type_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sample_type_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sample_type_fkey FOREIGN KEY (sample_type) REFERENCES discrete.sample_types(sample_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: samples samples_sampling_org_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sampling_org_fkey FOREIGN KEY (sampling_org) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: samples samples_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: discrete; Owner: admin
--

ALTER TABLE ONLY discrete.samples
    ADD CONSTRAINT samples_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: field_visit_images field_visit_images_field_visit_id_fkey; Type: FK CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visit_images
    ADD CONSTRAINT field_visit_images_field_visit_id_fkey FOREIGN KEY (field_visit_id) REFERENCES field.field_visits(field_visit_id) ON DELETE CASCADE;


--
-- Name: field_visit_images field_visit_images_image_id_fkey; Type: FK CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visit_images
    ADD CONSTRAINT field_visit_images_image_id_fkey FOREIGN KEY (image_id) REFERENCES files.images(image_id) ON DELETE CASCADE;


--
-- Name: field_visit_instruments field_visit_instruments_field_visit_id_fkey; Type: FK CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visit_instruments
    ADD CONSTRAINT field_visit_instruments_field_visit_id_fkey FOREIGN KEY (field_visit_id) REFERENCES field.field_visits(field_visit_id) ON DELETE CASCADE;


--
-- Name: field_visit_instruments field_visit_instruments_instrument_id_fkey; Type: FK CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visit_instruments
    ADD CONSTRAINT field_visit_instruments_instrument_id_fkey FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: field_visits field_visits_location_id_fkey; Type: FK CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visits
    ADD CONSTRAINT field_visits_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: field_visits fk_field_visit_subloc; Type: FK CONSTRAINT; Schema: field; Owner: postgres
--

ALTER TABLE ONLY field.field_visits
    ADD CONSTRAINT fk_field_visit_subloc FOREIGN KEY (location_id, sub_location_id) REFERENCES public.sub_locations(location_id, sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: documents documents_contributor_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_contributor_fkey FOREIGN KEY (contributor) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: documents documents_owner_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: documents_spatial documents_spatial_document_id_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents_spatial
    ADD CONSTRAINT documents_spatial_document_id_fkey FOREIGN KEY (document_id) REFERENCES files.documents(document_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: documents_spatial documents_spatial_geom_id_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents_spatial
    ADD CONSTRAINT documents_spatial_geom_id_fkey FOREIGN KEY (geom_id) REFERENCES spatial.vectors(geom_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: documents documents_type_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.documents
    ADD CONSTRAINT documents_type_fkey FOREIGN KEY (type) REFERENCES files.document_types(document_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: images fk_img_meta_id; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT fk_img_meta_id FOREIGN KEY (img_series_id) REFERENCES files.image_series(img_series_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: image_series fk_location_id; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT fk_location_id FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: images images_contributor_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_contributor_fkey FOREIGN KEY (contributor) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: images images_image_type_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_image_type_fkey FOREIGN KEY (image_type) REFERENCES files.image_types(image_type_id);


--
-- Name: image_series images_index_owner_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.image_series
    ADD CONSTRAINT images_index_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: images images_location_id_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: images images_owner_fkey; Type: FK CONSTRAINT; Schema: files; Owner: postgres
--

ALTER TABLE ONLY files.images
    ADD CONSTRAINT images_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: array_maintenance_changes array_maintenance_changes_instrument_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_instrument_id_fkey FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor1_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor1_id_fkey FOREIGN KEY (sensor1_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor2_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor2_id_fkey FOREIGN KEY (sensor2_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor3_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor3_id_fkey FOREIGN KEY (sensor3_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor4_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor4_id_fkey FOREIGN KEY (sensor4_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor5_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor5_id_fkey FOREIGN KEY (sensor5_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor6_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor6_id_fkey FOREIGN KEY (sensor6_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor7_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor7_id_fkey FOREIGN KEY (sensor7_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: array_maintenance_changes array_maintenance_changes_sensor8_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.array_maintenance_changes
    ADD CONSTRAINT array_maintenance_changes_sensor8_id_fkey FOREIGN KEY (sensor8_id) REFERENCES instruments.sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_depth calibrate_depth_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_depth
    ADD CONSTRAINT calibrate_depth_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_dissolved_oxygen calibrate_dissolved_oxygen_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_dissolved_oxygen
    ADD CONSTRAINT calibrate_dissolved_oxygen_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_orp calibrate_orp_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_orp
    ADD CONSTRAINT calibrate_orp_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_ph calibrate_ph_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_ph
    ADD CONSTRAINT calibrate_ph_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_specific_conductance calibrate_specific_conductance_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_specific_conductance
    ADD CONSTRAINT calibrate_specific_conductance_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_temperature calibrate_temperature_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_temperature
    ADD CONSTRAINT calibrate_temperature_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrate_turbidity calibrate_turbidity_calibration_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrate_turbidity
    ADD CONSTRAINT calibrate_turbidity_calibration_id_fkey FOREIGN KEY (calibration_id) REFERENCES instruments.calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrations calibrations_id_handheld_meter_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_id_handheld_meter_fkey FOREIGN KEY (id_handheld_meter) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrations calibrations_id_sensor_holder_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_id_sensor_holder_fkey FOREIGN KEY (id_sensor_holder) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calibrations calibrations_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.calibrations
    ADD CONSTRAINT calibrations_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments fk_instrument_owner; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT fk_instrument_owner FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instrument_maintenance instrument_maintenance_instrument_id_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instrument_maintenance
    ADD CONSTRAINT instrument_maintenance_instrument_id_fkey FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instrument_maintenance instrument_maintenance_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instrument_maintenance
    ADD CONSTRAINT instrument_maintenance_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_make_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_make_fkey FOREIGN KEY (make) REFERENCES instruments.instrument_make(make_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_model_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_model_fkey FOREIGN KEY (model) REFERENCES instruments.instrument_model(model_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_observer_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_observer_fkey FOREIGN KEY (observer) REFERENCES instruments.observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: instruments instruments_type_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.instruments
    ADD CONSTRAINT instruments_type_fkey FOREIGN KEY (type) REFERENCES instruments.instrument_type(type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: sensors sensors_sensor_type_fkey; Type: FK CONSTRAINT; Schema: instruments; Owner: postgres
--

ALTER TABLE ONLY instruments.sensors
    ADD CONSTRAINT sensors_sensor_type_fkey FOREIGN KEY (sensor_type) REFERENCES instruments.sensor_types(sensor_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: datum_conversions datum_conversions_datum_id_from_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT datum_conversions_datum_id_from_fkey FOREIGN KEY (datum_id_from) REFERENCES public.datum_list(datum_id);


--
-- Name: datum_conversions datum_conversions_datum_id_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT datum_conversions_datum_id_to_fkey FOREIGN KEY (datum_id_to) REFERENCES public.datum_list(datum_id);


--
-- Name: locations fk_geom_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT fk_geom_id FOREIGN KEY (geom_id) REFERENCES spatial.vectors(geom_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_instruments fk_loc_meta_instrument; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT fk_loc_meta_instrument FOREIGN KEY (instrument_id) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_z fk_loc_z_loc; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_z
    ADD CONSTRAINT fk_loc_z_loc FOREIGN KEY (location_id, sub_location_id) REFERENCES public.sub_locations(location_id, sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: datum_conversions fk_location_id; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.datum_conversions
    ADD CONSTRAINT fk_location_id FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations fk_location_type; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT fk_location_type FOREIGN KEY (location_type) REFERENCES public.location_types(type_id) ON UPDATE CASCADE;


--
-- Name: locations_metadata_owners_operators fk_subloc_composite; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT fk_subloc_composite FOREIGN KEY (location_id, sub_location_id) REFERENCES public.sub_locations(location_id, sub_location_id);


--
-- Name: locations locations_data_sharing_agreement_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations
    ADD CONSTRAINT locations_data_sharing_agreement_id_fkey FOREIGN KEY (data_sharing_agreement_id) REFERENCES files.documents(document_id);


--
-- Name: locations_metadata_access locations_metadata_access_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_access locations_metadata_access_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_access
    ADD CONSTRAINT locations_metadata_access_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwa_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwa_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_infrastructure_groundwater locations_metadata_infrastructure_groundwater_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_groundwater
    ADD CONSTRAINT locations_metadata_infrastructure_groundwater_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_infrastructure_hydromet locations_metadata_infrastructure_hydromet_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure_hydromet
    ADD CONSTRAINT locations_metadata_infrastructure_hydromet_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_infrastructure locations_metadata_infrastructure_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_infrastructure
    ADD CONSTRAINT locations_metadata_infrastructure_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_instruments locations_metadata_instruments_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_instruments locations_metadata_instruments_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_instruments locations_metadata_instruments_z_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_metadata_instruments
    ADD CONSTRAINT locations_metadata_instruments_z_id_fkey FOREIGN KEY (z_id) REFERENCES public.locations_z(z_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id);


--
-- Name: locations_metadata_maintenance locations_metadata_maintenance_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_maintenance
    ADD CONSTRAINT locations_metadata_maintenance_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_operator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_operator_fkey FOREIGN KEY (operator) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_owners_operators locations_metadata_owners_operators_owner_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_owners_operators
    ADD CONSTRAINT locations_metadata_owners_operators_owner_fkey FOREIGN KEY (owner) REFERENCES public.organizations(organization_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_grade_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_grade_fkey FOREIGN KEY (measurement_grade) REFERENCES public.grade_types(grade_type_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_instrument_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_instrument_fkey FOREIGN KEY (instrument) REFERENCES instruments.instruments(instrument_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE;


--
-- Name: locations_metadata_xsections locations_metadata_xsections_sub_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_metadata_xsections
    ADD CONSTRAINT locations_metadata_xsections_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id);


--
-- Name: locations_z locations_z_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.locations_z
    ADD CONSTRAINT locations_z_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_networks networks_locations_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_networks
    ADD CONSTRAINT networks_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_networks networks_locations_network_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_networks
    ADD CONSTRAINT networks_locations_network_id_fkey FOREIGN KEY (network_id) REFERENCES public.networks(network_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: networks networks_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.networks
    ADD CONSTRAINT networks_type_fkey FOREIGN KEY (type) REFERENCES public.network_project_types(id);


--
-- Name: parameter_relationships parameter_relationships_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_group_id_fkey FOREIGN KEY (group_id) REFERENCES public.parameter_groups(group_id);


--
-- Name: parameter_relationships parameter_relationships_param_code_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_param_code_fkey FOREIGN KEY (parameter_id) REFERENCES public.parameters(parameter_id);


--
-- Name: parameter_relationships parameter_relationships_sub_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.parameter_relationships
    ADD CONSTRAINT parameter_relationships_sub_group_id_fkey FOREIGN KEY (sub_group_id) REFERENCES public.parameter_sub_groups(sub_group_id);


--
-- Name: locations_projects projects_locations_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_projects
    ADD CONSTRAINT projects_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: locations_projects projects_locations_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.locations_projects
    ADD CONSTRAINT projects_locations_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.projects(project_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: projects projects_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_type_fkey FOREIGN KEY (type) REFERENCES public.network_project_types(id);


--
-- Name: sub_locations sub_locations_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: admin
--

ALTER TABLE ONLY public.sub_locations
    ADD CONSTRAINT sub_locations_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rasters_reference fk_raster_series_id; Type: FK CONSTRAINT; Schema: spatial; Owner: postgres
--

ALTER TABLE ONLY spatial.rasters_reference
    ADD CONSTRAINT fk_raster_series_id FOREIGN KEY (raster_series_id) REFERENCES spatial.raster_series_index(raster_series_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: rasters fk_reference_id; Type: FK CONSTRAINT; Schema: spatial; Owner: admin
--

ALTER TABLE ONLY spatial.rasters
    ADD CONSTRAINT fk_reference_id FOREIGN KEY (reference_id) REFERENCES spatial.rasters_reference(reference_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: boreholes; Type: ROW SECURITY; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.boreholes ENABLE ROW LEVEL SECURITY;

--
-- Name: boreholes rls; Type: POLICY; Schema: boreholes; Owner: postgres
--

CREATE POLICY rls ON boreholes.boreholes USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: wells rls; Type: POLICY; Schema: boreholes; Owner: postgres
--

CREATE POLICY rls ON boreholes.wells USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: wells; Type: ROW SECURITY; Schema: boreholes; Owner: postgres
--

ALTER TABLE boreholes.wells ENABLE ROW LEVEL SECURITY;

--
-- Name: timeseries rls; Type: POLICY; Schema: continuous; Owner: postgres
--

CREATE POLICY rls ON continuous.timeseries USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: timeseries; Type: ROW SECURITY; Schema: continuous; Owner: postgres
--

ALTER TABLE continuous.timeseries ENABLE ROW LEVEL SECURITY;

--
-- Name: results; Type: ROW SECURITY; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.results ENABLE ROW LEVEL SECURITY;

--
-- Name: results rls; Type: POLICY; Schema: discrete; Owner: admin
--

CREATE POLICY rls ON discrete.results USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: samples rls; Type: POLICY; Schema: discrete; Owner: admin
--

CREATE POLICY rls ON discrete.samples USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: samples; Type: ROW SECURITY; Schema: discrete; Owner: admin
--

ALTER TABLE discrete.samples ENABLE ROW LEVEL SECURITY;

--
-- Name: field_visits; Type: ROW SECURITY; Schema: field; Owner: postgres
--

ALTER TABLE field.field_visits ENABLE ROW LEVEL SECURITY;

--
-- Name: field_visits rls; Type: POLICY; Schema: field; Owner: postgres
--

CREATE POLICY rls ON field.field_visits FOR SELECT USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: documents; Type: ROW SECURITY; Schema: files; Owner: postgres
--

ALTER TABLE files.documents ENABLE ROW LEVEL SECURITY;

--
-- Name: image_series; Type: ROW SECURITY; Schema: files; Owner: postgres
--

ALTER TABLE files.image_series ENABLE ROW LEVEL SECURITY;

--
-- Name: images; Type: ROW SECURITY; Schema: files; Owner: postgres
--

ALTER TABLE files.images ENABLE ROW LEVEL SECURITY;

--
-- Name: documents rls; Type: POLICY; Schema: files; Owner: postgres
--

CREATE POLICY rls ON files.documents USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: image_series rls; Type: POLICY; Schema: files; Owner: postgres
--

CREATE POLICY rls ON files.image_series USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: images rls; Type: POLICY; Schema: files; Owner: postgres
--

CREATE POLICY rls ON files.images USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: locations; Type: ROW SECURITY; Schema: public; Owner: postgres
--

ALTER TABLE public.locations ENABLE ROW LEVEL SECURITY;

--
-- Name: locations rls; Type: POLICY; Schema: public; Owner: postgres
--

CREATE POLICY rls ON public.locations USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: sub_locations rls; Type: POLICY; Schema: public; Owner: admin
--

CREATE POLICY rls ON public.sub_locations USING (((share_with @> ARRAY['public_reader'::text]) OR (share_with && public.current_user_roles())));


--
-- Name: sub_locations; Type: ROW SECURITY; Schema: public; Owner: admin
--

ALTER TABLE public.sub_locations ENABLE ROW LEVEL SECURITY;

--
-- Name: SCHEMA application; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA application TO admin;
GRANT USAGE ON SCHEMA application TO PUBLIC;


--
-- Name: SCHEMA boreholes; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA boreholes TO admin;
GRANT USAGE ON SCHEMA boreholes TO PUBLIC;
GRANT USAGE ON SCHEMA boreholes TO yg_reader_group;
GRANT USAGE ON SCHEMA boreholes TO yg_editor_group;
GRANT USAGE ON SCHEMA boreholes TO tkc_group;


--
-- Name: SCHEMA continuous; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA continuous TO admin;
GRANT USAGE ON SCHEMA continuous TO public_reader;
GRANT USAGE ON SCHEMA continuous TO yg_reader_group;
GRANT USAGE ON SCHEMA continuous TO yg_editor_group;
GRANT USAGE ON SCHEMA continuous TO tkc_group;


--
-- Name: SCHEMA discrete; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA discrete TO public_reader;
GRANT ALL ON SCHEMA discrete TO admin;
GRANT USAGE ON SCHEMA discrete TO yg_reader_group;
GRANT USAGE ON SCHEMA discrete TO yg_editor_group;
GRANT USAGE ON SCHEMA discrete TO tkc_group;


--
-- Name: SCHEMA field; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA field TO admin;
GRANT USAGE ON SCHEMA field TO PUBLIC;


--
-- Name: SCHEMA files; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA files TO admin;
GRANT USAGE ON SCHEMA files TO public_reader;
GRANT USAGE ON SCHEMA files TO yg_reader_group;
GRANT USAGE ON SCHEMA files TO yg_editor_group;
GRANT USAGE ON SCHEMA files TO tkc_group;


--
-- Name: SCHEMA information; Type: ACL; Schema: -; Owner: postgres
--

GRANT ALL ON SCHEMA information TO admin;
GRANT USAGE ON SCHEMA information TO yg_reader_group;
GRANT USAGE ON SCHEMA information TO public_reader;


--
-- Name: SCHEMA instruments; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA instruments TO public_reader;
GRANT ALL ON SCHEMA instruments TO admin;
GRANT USAGE ON SCHEMA instruments TO yg_reader_group;
GRANT USAGE ON SCHEMA instruments TO yg_editor_group;
GRANT USAGE ON SCHEMA instruments TO tkc_group;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: pg_database_owner
--

GRANT USAGE ON SCHEMA public TO public_reader;
GRANT ALL ON SCHEMA public TO admin;
GRANT USAGE ON SCHEMA public TO yg_reader_group;
GRANT USAGE ON SCHEMA public TO yg_editor_group;
GRANT USAGE ON SCHEMA public TO tkc_group;


--
-- Name: SCHEMA spatial; Type: ACL; Schema: -; Owner: postgres
--

GRANT USAGE ON SCHEMA spatial TO public_reader;
GRANT ALL ON SCHEMA spatial TO admin;
GRANT USAGE ON SCHEMA spatial TO yg_reader_group;
GRANT USAGE ON SCHEMA spatial TO yg_editor_group;
GRANT USAGE ON SCHEMA spatial TO tkc_group;


--
-- Name: FUNCTION gbtreekey16_in(cstring); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey16_in(cstring) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey16_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey16_out(public.gbtreekey16); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey16_out(public.gbtreekey16) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey16_out(public.gbtreekey16) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey2_in(cstring); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey2_in(cstring) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey2_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey2_out(public.gbtreekey2); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey2_out(public.gbtreekey2) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey2_out(public.gbtreekey2) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey32_in(cstring); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey32_in(cstring) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey32_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey32_out(public.gbtreekey32); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey32_out(public.gbtreekey32) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey32_out(public.gbtreekey32) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey4_in(cstring); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey4_in(cstring) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey4_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey4_out(public.gbtreekey4); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey4_out(public.gbtreekey4) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey4_out(public.gbtreekey4) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey8_in(cstring); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey8_in(cstring) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey8_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey8_out(public.gbtreekey8); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey8_out(public.gbtreekey8) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey8_out(public.gbtreekey8) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey_var_in(cstring); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey_var_in(cstring) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey_var_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gbtreekey_var_out(public.gbtreekey_var); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbtreekey_var_out(public.gbtreekey_var) TO admin;
GRANT ALL ON FUNCTION public.gbtreekey_var_out(public.gbtreekey_var) TO yg_reader_group;


--
-- Name: FUNCTION raster_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.raster_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION raster_out(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_out(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_out(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION box2d_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box2d_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.box2d_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION box2d_out(spatial.box2d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box2d_out(spatial.box2d) TO admin;
GRANT ALL ON FUNCTION spatial.box2d_out(spatial.box2d) TO yg_reader_group;


--
-- Name: FUNCTION box2df_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box2df_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.box2df_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION box2df_out(spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box2df_out(spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.box2df_out(spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION box3d_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box3d_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.box3d_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION box3d_out(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box3d_out(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.box3d_out(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION geography_analyze(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_analyze(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_analyze(internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_in(cstring, oid, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_in(cstring, oid, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geography_in(cstring, oid, integer) TO yg_reader_group;


--
-- Name: FUNCTION geography_out(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_out(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_out(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_recv(internal, oid, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_recv(internal, oid, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geography_recv(internal, oid, integer) TO yg_reader_group;


--
-- Name: FUNCTION geography_send(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_send(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_send(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_typmod_in(cstring[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_typmod_in(cstring[]) TO admin;
GRANT ALL ON FUNCTION spatial.geography_typmod_in(cstring[]) TO yg_reader_group;


--
-- Name: FUNCTION geography_typmod_out(integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_typmod_out(integer) TO admin;
GRANT ALL ON FUNCTION spatial.geography_typmod_out(integer) TO yg_reader_group;


--
-- Name: FUNCTION geometry_analyze(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_analyze(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_analyze(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION geometry_out(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_out(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_out(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_recv(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_recv(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_recv(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_send(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_send(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_send(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_typmod_in(cstring[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_typmod_in(cstring[]) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_typmod_in(cstring[]) TO yg_reader_group;


--
-- Name: FUNCTION geometry_typmod_out(integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_typmod_out(integer) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_typmod_out(integer) TO yg_reader_group;


--
-- Name: FUNCTION gidx_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.gidx_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.gidx_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION gidx_out(spatial.gidx); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.gidx_out(spatial.gidx) TO admin;
GRANT ALL ON FUNCTION spatial.gidx_out(spatial.gidx) TO yg_reader_group;


--
-- Name: FUNCTION spheroid_in(cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.spheroid_in(cstring) TO admin;
GRANT ALL ON FUNCTION spatial.spheroid_in(cstring) TO yg_reader_group;


--
-- Name: FUNCTION spheroid_out(spatial.spheroid); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.spheroid_out(spatial.spheroid) TO admin;
GRANT ALL ON FUNCTION spatial.spheroid_out(spatial.spheroid) TO yg_reader_group;


--
-- Name: FUNCTION box3d(spatial.box2d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box3d(spatial.box2d) TO admin;
GRANT ALL ON FUNCTION spatial.box3d(spatial.box2d) TO yg_reader_group;


--
-- Name: FUNCTION geometry(spatial.box2d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(spatial.box2d) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(spatial.box2d) TO yg_reader_group;


--
-- Name: FUNCTION box(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.box(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION box2d(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box2d(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.box2d(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION geometry(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION geography(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.geography(bytea) TO yg_reader_group;


--
-- Name: FUNCTION geometry(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(bytea) TO yg_reader_group;


--
-- Name: FUNCTION bytea(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.bytea(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.bytea(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography(spatial.geography, integer, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography(spatial.geography, integer, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.geography(spatial.geography, integer, boolean) TO yg_reader_group;


--
-- Name: FUNCTION geometry(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION box(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.box(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION box2d(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box2d(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.box2d(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION box3d(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box3d(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.box3d(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION bytea(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.bytea(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.bytea(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geography(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geography(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry(spatial.geometry, integer, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(spatial.geometry, integer, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(spatial.geometry, integer, boolean) TO yg_reader_group;


--
-- Name: FUNCTION "json"(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial."json"(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial."json"(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION jsonb(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.jsonb(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.jsonb(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION path(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.path(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.path(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION point(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.point(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.point(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION polygon(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.polygon(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.polygon(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION text(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.text(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.text(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry(path); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(path) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(path) TO yg_reader_group;


--
-- Name: FUNCTION geometry(point); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(point) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(point) TO yg_reader_group;


--
-- Name: FUNCTION geometry(polygon); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(polygon) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(polygon) TO yg_reader_group;


--
-- Name: FUNCTION box3d(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box3d(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.box3d(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION bytea(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.bytea(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.bytea(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_convexhull(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_convexhull(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_convexhull(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION geometry(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry(text) TO admin;
GRANT ALL ON FUNCTION spatial.geometry(text) TO yg_reader_group;


--
-- Name: FUNCTION prevent_geology_overlap(); Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON FUNCTION boreholes.prevent_geology_overlap() TO admin;


--
-- Name: FUNCTION prevent_permafrost_overlap(); Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON FUNCTION boreholes.prevent_permafrost_overlap() TO admin;


--
-- Name: FUNCTION apply_corrections(p_timeseries_id integer, p_datetime timestamp with time zone, p_value numeric); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.apply_corrections(p_timeseries_id integer, p_datetime timestamp with time zone, p_value numeric) TO admin;
GRANT ALL ON FUNCTION continuous.apply_corrections(p_timeseries_id integer, p_datetime timestamp with time zone, p_value numeric) TO yg_reader_group;


--
-- Name: FUNCTION check_approvals_overlap(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.check_approvals_overlap() TO admin;
GRANT ALL ON FUNCTION continuous.check_approvals_overlap() TO yg_reader_group;


--
-- Name: FUNCTION check_contributors_overlap(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.check_contributors_overlap() TO admin;
GRANT ALL ON FUNCTION continuous.check_contributors_overlap() TO yg_reader_group;


--
-- Name: FUNCTION check_grades_overlap(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.check_grades_overlap() TO admin;
GRANT ALL ON FUNCTION continuous.check_grades_overlap() TO yg_reader_group;


--
-- Name: FUNCTION check_owners_overlap(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.check_owners_overlap() TO admin;
GRANT ALL ON FUNCTION continuous.check_owners_overlap() TO yg_reader_group;


--
-- Name: FUNCTION check_qualifiers_overlap(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.check_qualifiers_overlap() TO admin;
GRANT ALL ON FUNCTION continuous.check_qualifiers_overlap() TO yg_reader_group;


--
-- Name: FUNCTION delete_old_forecasts(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.delete_old_forecasts() TO admin;
GRANT ALL ON FUNCTION continuous.delete_old_forecasts() TO yg_reader_group;


--
-- Name: FUNCTION trunc_hour_utc(ts timestamp with time zone); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.trunc_hour_utc(ts timestamp with time zone) TO public_reader;
GRANT ALL ON FUNCTION continuous.trunc_hour_utc(ts timestamp with time zone) TO admin;


--
-- Name: FUNCTION validate_corrections(); Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON FUNCTION continuous.validate_corrections() TO admin;
GRANT ALL ON FUNCTION continuous.validate_corrections() TO yg_reader_group;


--
-- Name: FUNCTION check_sample_series_overlap(); Type: ACL; Schema: discrete; Owner: postgres
--

GRANT ALL ON FUNCTION discrete.check_sample_series_overlap() TO public_reader;
GRANT ALL ON FUNCTION discrete.check_sample_series_overlap() TO admin;
GRANT ALL ON FUNCTION discrete.check_sample_series_overlap() TO yg_reader_group;


--
-- Name: FUNCTION enforce_result_speciation(); Type: ACL; Schema: discrete; Owner: postgres
--

GRANT ALL ON FUNCTION discrete.enforce_result_speciation() TO public_reader;
GRANT ALL ON FUNCTION discrete.enforce_result_speciation() TO admin;
GRANT ALL ON FUNCTION discrete.enforce_result_speciation() TO yg_reader_group;


--
-- Name: FUNCTION enforce_sample_fraction(); Type: ACL; Schema: discrete; Owner: postgres
--

GRANT ALL ON FUNCTION discrete.enforce_sample_fraction() TO public_reader;
GRANT ALL ON FUNCTION discrete.enforce_sample_fraction() TO admin;
GRANT ALL ON FUNCTION discrete.enforce_sample_fraction() TO yg_reader_group;


--
-- Name: FUNCTION get_sample_hardness(in_sample_id integer); Type: ACL; Schema: discrete; Owner: postgres
--

GRANT ALL ON FUNCTION discrete.get_sample_hardness(in_sample_id integer) TO public_reader;
GRANT ALL ON FUNCTION discrete.get_sample_hardness(in_sample_id integer) TO admin;
GRANT ALL ON FUNCTION discrete.get_sample_hardness(in_sample_id integer) TO yg_reader_group;


--
-- Name: FUNCTION get_sample_val(sample_id integer, parameter_id integer, sample_fraction_id integer, result_speciation_id integer); Type: ACL; Schema: discrete; Owner: postgres
--

GRANT ALL ON FUNCTION discrete.get_sample_val(sample_id integer, parameter_id integer, sample_fraction_id integer, result_speciation_id integer) TO public_reader;
GRANT ALL ON FUNCTION discrete.get_sample_val(sample_id integer, parameter_id integer, sample_fraction_id integer, result_speciation_id integer) TO admin;
GRANT ALL ON FUNCTION discrete.get_sample_val(sample_id integer, parameter_id integer, sample_fraction_id integer, result_speciation_id integer) TO yg_reader_group;


--
-- Name: FUNCTION guidelines_validate_trg(); Type: ACL; Schema: discrete; Owner: postgres
--

GRANT ALL ON FUNCTION discrete.guidelines_validate_trg() TO public_reader;
GRANT ALL ON FUNCTION discrete.guidelines_validate_trg() TO admin;
GRANT ALL ON FUNCTION discrete.guidelines_validate_trg() TO yg_reader_group;


--
-- Name: FUNCTION check_data_sharing_agreement(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.check_data_sharing_agreement() TO admin;
GRANT ALL ON FUNCTION files.check_data_sharing_agreement() TO yg_reader_group;


--
-- Name: FUNCTION check_location_images(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.check_location_images() TO admin;
GRANT ALL ON FUNCTION files.check_location_images() TO yg_reader_group;


--
-- Name: FUNCTION enforce_share_with_restriction(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.enforce_share_with_restriction() TO admin;
GRANT ALL ON FUNCTION files.enforce_share_with_restriction() TO yg_reader_group;


--
-- Name: FUNCTION update_document_flags_after_delete(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.update_document_flags_after_delete() TO admin;
GRANT ALL ON FUNCTION files.update_document_flags_after_delete() TO yg_reader_group;


--
-- Name: FUNCTION update_document_flags_after_insert(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.update_document_flags_after_insert() TO admin;
GRANT ALL ON FUNCTION files.update_document_flags_after_insert() TO yg_reader_group;


--
-- Name: FUNCTION update_line_flag(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.update_line_flag() TO admin;
GRANT ALL ON FUNCTION files.update_line_flag() TO yg_reader_group;


--
-- Name: FUNCTION update_location_flag(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.update_location_flag() TO admin;
GRANT ALL ON FUNCTION files.update_location_flag() TO yg_reader_group;


--
-- Name: FUNCTION update_polygon_flag(); Type: ACL; Schema: files; Owner: postgres
--

GRANT ALL ON FUNCTION files.update_polygon_flag() TO admin;
GRANT ALL ON FUNCTION files.update_polygon_flag() TO yg_reader_group;


--
-- Name: FUNCTION update_modify_datetime(); Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON FUNCTION instruments.update_modify_datetime() TO admin;
GRANT ALL ON FUNCTION instruments.update_modify_datetime() TO yg_reader_group;


--
-- Name: FUNCTION cash_dist(money, money); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.cash_dist(money, money) TO admin;
GRANT ALL ON FUNCTION public.cash_dist(money, money) TO yg_reader_group;


--
-- Name: FUNCTION check_instrument_meta_overlap(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.check_instrument_meta_overlap() TO admin;
GRANT ALL ON FUNCTION public.check_instrument_meta_overlap() TO yg_reader_group;


--
-- Name: FUNCTION check_location_exists(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.check_location_exists() TO admin;
GRANT ALL ON FUNCTION public.check_location_exists() TO yg_reader_group;


--
-- Name: FUNCTION check_locations_metadata_acquisition_instruments(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.check_locations_metadata_acquisition_instruments() TO admin;
GRANT ALL ON FUNCTION public.check_locations_metadata_acquisition_instruments() TO yg_reader_group;


--
-- Name: FUNCTION count_estimate(query text); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.count_estimate(query text) TO admin;
GRANT ALL ON FUNCTION public.count_estimate(query text) TO yg_reader_group;


--
-- Name: FUNCTION current_user_roles(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.current_user_roles() TO admin;
GRANT ALL ON FUNCTION public.current_user_roles() TO yg_reader_group;


--
-- Name: FUNCTION date_dist(date, date); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.date_dist(date, date) TO admin;
GRANT ALL ON FUNCTION public.date_dist(date, date) TO yg_reader_group;


--
-- Name: FUNCTION enforce_maintenance_constraints(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.enforce_maintenance_constraints() TO admin;
GRANT ALL ON FUNCTION public.enforce_maintenance_constraints() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_access_missing(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_access_missing() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_access_missing() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_acquisition_missing(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_acquisition_missing() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_acquisition_missing() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_infrastructure_groundwater(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_infrastructure_groundwater() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_infrastructure_groundwater() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_infrastructure_hydromet(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_infrastructure_hydromet() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_infrastructure_hydromet() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_infrastructure_missing(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_infrastructure_missing() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_infrastructure_missing() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_owners_operators_missing(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_owners_operators_missing() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_owners_operators_missing() TO yg_reader_group;


--
-- Name: FUNCTION fill_locations_metadata_transmission_missing(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.fill_locations_metadata_transmission_missing() TO admin;
GRANT ALL ON FUNCTION public.fill_locations_metadata_transmission_missing() TO yg_reader_group;


--
-- Name: FUNCTION float4_dist(real, real); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.float4_dist(real, real) TO admin;
GRANT ALL ON FUNCTION public.float4_dist(real, real) TO yg_reader_group;


--
-- Name: FUNCTION float8_dist(double precision, double precision); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.float8_dist(double precision, double precision) TO admin;
GRANT ALL ON FUNCTION public.float8_dist(double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bit_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bit_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bit_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bit_consistent(internal, bit, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bit_consistent(internal, bit, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bit_consistent(internal, bit, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bit_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bit_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bit_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bit_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bit_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bit_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bit_same(public.gbtreekey_var, public.gbtreekey_var, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bit_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bit_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bit_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bit_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bit_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_consistent(internal, boolean, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_consistent(internal, boolean, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_consistent(internal, boolean, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_same(public.gbtreekey2, public.gbtreekey2, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_same(public.gbtreekey2, public.gbtreekey2, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_same(public.gbtreekey2, public.gbtreekey2, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bool_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bool_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bool_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bpchar_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bpchar_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bpchar_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bpchar_consistent(internal, character, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bpchar_consistent(internal, character, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bpchar_consistent(internal, character, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bytea_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bytea_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bytea_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bytea_consistent(internal, bytea, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bytea_consistent(internal, bytea, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bytea_consistent(internal, bytea, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bytea_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bytea_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bytea_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bytea_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bytea_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bytea_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bytea_same(public.gbtreekey_var, public.gbtreekey_var, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bytea_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bytea_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_bytea_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_bytea_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_bytea_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_consistent(internal, money, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_consistent(internal, money, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_consistent(internal, money, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_distance(internal, money, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_distance(internal, money, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_distance(internal, money, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_cash_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_cash_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_cash_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_consistent(internal, date, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_consistent(internal, date, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_consistent(internal, date, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_distance(internal, date, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_distance(internal, date, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_distance(internal, date, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_same(public.gbtreekey8, public.gbtreekey8, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_same(public.gbtreekey8, public.gbtreekey8, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_same(public.gbtreekey8, public.gbtreekey8, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_date_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_date_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_date_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_decompress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_decompress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_decompress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_consistent(internal, anyenum, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_consistent(internal, anyenum, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_consistent(internal, anyenum, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_same(public.gbtreekey8, public.gbtreekey8, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_same(public.gbtreekey8, public.gbtreekey8, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_same(public.gbtreekey8, public.gbtreekey8, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_enum_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_enum_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_enum_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_consistent(internal, real, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_consistent(internal, real, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_consistent(internal, real, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_distance(internal, real, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_distance(internal, real, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_distance(internal, real, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_same(public.gbtreekey8, public.gbtreekey8, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_same(public.gbtreekey8, public.gbtreekey8, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_same(public.gbtreekey8, public.gbtreekey8, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float4_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float4_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float4_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_consistent(internal, double precision, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_consistent(internal, double precision, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_consistent(internal, double precision, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_distance(internal, double precision, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_distance(internal, double precision, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_distance(internal, double precision, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_float8_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_float8_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_float8_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_inet_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_inet_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_inet_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_inet_consistent(internal, inet, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_inet_consistent(internal, inet, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_inet_consistent(internal, inet, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_inet_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_inet_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_inet_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_inet_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_inet_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_inet_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_inet_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_inet_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_inet_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_inet_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_inet_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_inet_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_consistent(internal, smallint, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_consistent(internal, smallint, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_consistent(internal, smallint, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_distance(internal, smallint, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_distance(internal, smallint, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_distance(internal, smallint, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_same(public.gbtreekey4, public.gbtreekey4, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_same(public.gbtreekey4, public.gbtreekey4, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_same(public.gbtreekey4, public.gbtreekey4, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int2_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int2_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int2_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_consistent(internal, integer, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_consistent(internal, integer, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_consistent(internal, integer, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_distance(internal, integer, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_distance(internal, integer, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_distance(internal, integer, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_same(public.gbtreekey8, public.gbtreekey8, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_same(public.gbtreekey8, public.gbtreekey8, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_same(public.gbtreekey8, public.gbtreekey8, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int4_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int4_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int4_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_consistent(internal, bigint, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_consistent(internal, bigint, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_consistent(internal, bigint, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_distance(internal, bigint, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_distance(internal, bigint, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_distance(internal, bigint, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_int8_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_int8_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_int8_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_consistent(internal, interval, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_consistent(internal, interval, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_consistent(internal, interval, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_decompress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_decompress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_decompress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_distance(internal, interval, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_distance(internal, interval, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_distance(internal, interval, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_same(public.gbtreekey32, public.gbtreekey32, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_same(public.gbtreekey32, public.gbtreekey32, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_same(public.gbtreekey32, public.gbtreekey32, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_intv_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_intv_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_intv_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_consistent(internal, macaddr8, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_consistent(internal, macaddr8, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_consistent(internal, macaddr8, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad8_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad8_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad8_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_consistent(internal, macaddr, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_consistent(internal, macaddr, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_consistent(internal, macaddr, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_macad_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_macad_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_macad_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_numeric_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_numeric_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_numeric_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_numeric_consistent(internal, numeric, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_numeric_consistent(internal, numeric, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_numeric_consistent(internal, numeric, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_numeric_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_numeric_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_numeric_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_numeric_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_numeric_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_numeric_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_numeric_same(public.gbtreekey_var, public.gbtreekey_var, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_numeric_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_numeric_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_numeric_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_numeric_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_numeric_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_consistent(internal, oid, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_consistent(internal, oid, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_consistent(internal, oid, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_distance(internal, oid, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_distance(internal, oid, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_distance(internal, oid, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_same(public.gbtreekey8, public.gbtreekey8, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_same(public.gbtreekey8, public.gbtreekey8, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_same(public.gbtreekey8, public.gbtreekey8, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_oid_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_oid_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_oid_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_text_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_text_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_text_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_text_consistent(internal, text, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_text_consistent(internal, text, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_text_consistent(internal, text, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_text_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_text_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_text_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_text_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_text_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_text_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_text_same(public.gbtreekey_var, public.gbtreekey_var, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_text_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_text_same(public.gbtreekey_var, public.gbtreekey_var, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_text_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_text_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_text_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_consistent(internal, time without time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_consistent(internal, time without time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_consistent(internal, time without time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_distance(internal, time without time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_distance(internal, time without time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_distance(internal, time without time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_time_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_time_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_time_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_timetz_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_timetz_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_timetz_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_timetz_consistent(internal, time with time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_timetz_consistent(internal, time with time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_timetz_consistent(internal, time with time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_consistent(internal, timestamp without time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_consistent(internal, timestamp without time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_consistent(internal, timestamp without time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_distance(internal, timestamp without time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_distance(internal, timestamp without time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_distance(internal, timestamp without time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_same(public.gbtreekey16, public.gbtreekey16, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_same(public.gbtreekey16, public.gbtreekey16, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_same(public.gbtreekey16, public.gbtreekey16, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_ts_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_ts_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_ts_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_tstz_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_tstz_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_tstz_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_tstz_consistent(internal, timestamp with time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_tstz_consistent(internal, timestamp with time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_tstz_consistent(internal, timestamp with time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_tstz_distance(internal, timestamp with time zone, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_tstz_distance(internal, timestamp with time zone, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_tstz_distance(internal, timestamp with time zone, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_compress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_compress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_consistent(internal, uuid, smallint, oid, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_consistent(internal, uuid, smallint, oid, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_consistent(internal, uuid, smallint, oid, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_penalty(internal, internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_picksplit(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_same(public.gbtreekey32, public.gbtreekey32, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_same(public.gbtreekey32, public.gbtreekey32, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_same(public.gbtreekey32, public.gbtreekey32, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_uuid_union(internal, internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_uuid_union(internal, internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_uuid_union(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_var_decompress(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_var_decompress(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_var_decompress(internal) TO yg_reader_group;


--
-- Name: FUNCTION gbt_var_fetch(internal); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.gbt_var_fetch(internal) TO admin;
GRANT ALL ON FUNCTION public.gbt_var_fetch(internal) TO yg_reader_group;


--
-- Name: FUNCTION get_csw_layer(); Type: ACL; Schema: public; Owner: admin
--

GRANT ALL ON FUNCTION public.get_csw_layer() TO yg_reader_group;


--
-- Name: FUNCTION get_shareable_groups_for(_rel regclass, _privs text[]); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.get_shareable_groups_for(_rel regclass, _privs text[]) TO admin;
GRANT ALL ON FUNCTION public.get_shareable_groups_for(_rel regclass, _privs text[]) TO yg_reader_group;


--
-- Name: FUNCTION get_shareable_principals_for(_rel regclass, _privs text[], _always_include text[]); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.get_shareable_principals_for(_rel regclass, _privs text[], _always_include text[]) TO admin;
GRANT ALL ON FUNCTION public.get_shareable_principals_for(_rel regclass, _privs text[], _always_include text[]) TO yg_reader_group;


--
-- Name: FUNCTION int2_dist(smallint, smallint); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.int2_dist(smallint, smallint) TO admin;
GRANT ALL ON FUNCTION public.int2_dist(smallint, smallint) TO yg_reader_group;


--
-- Name: FUNCTION int4_dist(integer, integer); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.int4_dist(integer, integer) TO admin;
GRANT ALL ON FUNCTION public.int4_dist(integer, integer) TO yg_reader_group;


--
-- Name: FUNCTION int8_dist(bigint, bigint); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.int8_dist(bigint, bigint) TO admin;
GRANT ALL ON FUNCTION public.int8_dist(bigint, bigint) TO yg_reader_group;


--
-- Name: FUNCTION interval_dist(interval, interval); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.interval_dist(interval, interval) TO admin;
GRANT ALL ON FUNCTION public.interval_dist(interval, interval) TO yg_reader_group;


--
-- Name: FUNCTION oid_dist(oid, oid); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.oid_dist(oid, oid) TO admin;
GRANT ALL ON FUNCTION public.oid_dist(oid, oid) TO yg_reader_group;


--
-- Name: FUNCTION time_dist(time without time zone, time without time zone); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.time_dist(time without time zone, time without time zone) TO admin;
GRANT ALL ON FUNCTION public.time_dist(time without time zone, time without time zone) TO yg_reader_group;


--
-- Name: FUNCTION ts_dist(timestamp without time zone, timestamp without time zone); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.ts_dist(timestamp without time zone, timestamp without time zone) TO admin;
GRANT ALL ON FUNCTION public.ts_dist(timestamp without time zone, timestamp without time zone) TO yg_reader_group;


--
-- Name: FUNCTION tstz_dist(timestamp with time zone, timestamp with time zone); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.tstz_dist(timestamp with time zone, timestamp with time zone) TO admin;
GRANT ALL ON FUNCTION public.tstz_dist(timestamp with time zone, timestamp with time zone) TO yg_reader_group;


--
-- Name: FUNCTION update_created_modified(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.update_created_modified() TO admin;
GRANT ALL ON FUNCTION public.update_created_modified() TO yg_reader_group;


--
-- Name: FUNCTION update_modified(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.update_modified() TO admin;
GRANT ALL ON FUNCTION public.update_modified() TO yg_reader_group;


--
-- Name: FUNCTION update_updated(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.update_updated() TO admin;
GRANT ALL ON FUNCTION public.update_updated() TO yg_reader_group;


--
-- Name: FUNCTION user_in_group(group_name text); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.user_in_group(group_name text) TO admin;
GRANT ALL ON FUNCTION public.user_in_group(group_name text) TO yg_reader_group;


--
-- Name: FUNCTION user_modified(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.user_modified() TO admin;
GRANT ALL ON FUNCTION public.user_modified() TO yg_reader_group;


--
-- Name: FUNCTION validate_documents_array(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.validate_documents_array() TO admin;
GRANT ALL ON FUNCTION public.validate_documents_array() TO yg_reader_group;


--
-- Name: FUNCTION validate_share_with(); Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON FUNCTION public.validate_share_with() TO admin;
GRANT ALL ON FUNCTION public.validate_share_with() TO yg_reader_group;


--
-- Name: FUNCTION __st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.__st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.__st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _add_overview_constraint(ovschema name, ovtable name, ovcolumn name, refschema name, reftable name, refcolumn name, factor integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_overview_constraint(ovschema name, ovtable name, ovcolumn name, refschema name, reftable name, refcolumn name, factor integer) TO admin;
GRANT ALL ON FUNCTION spatial._add_overview_constraint(ovschema name, ovtable name, ovcolumn name, refschema name, reftable name, refcolumn name, factor integer) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint(cn name, sql text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint(cn name, sql text) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint(cn name, sql text) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_alignment(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_alignment(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_alignment(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_blocksize(rastschema name, rasttable name, rastcolumn name, axis text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_blocksize(rastschema name, rasttable name, rastcolumn name, axis text) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_blocksize(rastschema name, rasttable name, rastcolumn name, axis text) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_coverage_tile(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_coverage_tile(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_coverage_tile(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_extent(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_extent(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_extent(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_nodata_values(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_nodata_values(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_nodata_values(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_num_bands(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_num_bands(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_num_bands(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_out_db(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_out_db(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_out_db(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_pixel_types(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_pixel_types(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_pixel_types(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_scale(rastschema name, rasttable name, rastcolumn name, axis character); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_scale(rastschema name, rasttable name, rastcolumn name, axis character) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_scale(rastschema name, rasttable name, rastcolumn name, axis character) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_spatially_unique(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_spatially_unique(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_spatially_unique(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _add_raster_constraint_srid(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._add_raster_constraint_srid(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._add_raster_constraint_srid(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_overview_constraint(ovschema name, ovtable name, ovcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_overview_constraint(ovschema name, ovtable name, ovcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_overview_constraint(ovschema name, ovtable name, ovcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint(rastschema name, rasttable name, cn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint(rastschema name, rasttable name, cn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint(rastschema name, rasttable name, cn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_alignment(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_alignment(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_alignment(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_blocksize(rastschema name, rasttable name, rastcolumn name, axis text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_blocksize(rastschema name, rasttable name, rastcolumn name, axis text) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_blocksize(rastschema name, rasttable name, rastcolumn name, axis text) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_coverage_tile(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_coverage_tile(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_coverage_tile(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_extent(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_extent(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_extent(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_nodata_values(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_nodata_values(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_nodata_values(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_num_bands(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_num_bands(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_num_bands(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_out_db(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_out_db(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_out_db(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_pixel_types(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_pixel_types(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_pixel_types(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_regular_blocking(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_regular_blocking(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_regular_blocking(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_scale(rastschema name, rasttable name, rastcolumn name, axis character); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_scale(rastschema name, rasttable name, rastcolumn name, axis character) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_scale(rastschema name, rasttable name, rastcolumn name, axis character) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_spatially_unique(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_spatially_unique(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_spatially_unique(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _drop_raster_constraint_srid(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._drop_raster_constraint_srid(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._drop_raster_constraint_srid(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _overview_constraint(ov spatial.raster, factor integer, refschema name, reftable name, refcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._overview_constraint(ov spatial.raster, factor integer, refschema name, reftable name, refcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._overview_constraint(ov spatial.raster, factor integer, refschema name, reftable name, refcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _overview_constraint_info(ovschema name, ovtable name, ovcolumn name, OUT refschema name, OUT reftable name, OUT refcolumn name, OUT factor integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._overview_constraint_info(ovschema name, ovtable name, ovcolumn name, OUT refschema name, OUT reftable name, OUT refcolumn name, OUT factor integer) TO admin;
GRANT ALL ON FUNCTION spatial._overview_constraint_info(ovschema name, ovtable name, ovcolumn name, OUT refschema name, OUT reftable name, OUT refcolumn name, OUT factor integer) TO yg_reader_group;


--
-- Name: FUNCTION _postgis_deprecate(oldname text, newname text, version text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_deprecate(oldname text, newname text, version text) TO admin;
GRANT ALL ON FUNCTION spatial._postgis_deprecate(oldname text, newname text, version text) TO yg_reader_group;


--
-- Name: FUNCTION _postgis_index_extent(tbl regclass, col text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_index_extent(tbl regclass, col text) TO admin;
GRANT ALL ON FUNCTION spatial._postgis_index_extent(tbl regclass, col text) TO yg_reader_group;


--
-- Name: FUNCTION _postgis_join_selectivity(regclass, text, regclass, text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_join_selectivity(regclass, text, regclass, text, text) TO admin;
GRANT ALL ON FUNCTION spatial._postgis_join_selectivity(regclass, text, regclass, text, text) TO yg_reader_group;


--
-- Name: FUNCTION _postgis_pgsql_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_pgsql_version() TO admin;
GRANT ALL ON FUNCTION spatial._postgis_pgsql_version() TO yg_reader_group;


--
-- Name: FUNCTION _postgis_scripts_pgsql_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_scripts_pgsql_version() TO admin;
GRANT ALL ON FUNCTION spatial._postgis_scripts_pgsql_version() TO yg_reader_group;


--
-- Name: FUNCTION _postgis_selectivity(tbl regclass, att_name text, geom spatial.geometry, mode text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_selectivity(tbl regclass, att_name text, geom spatial.geometry, mode text) TO admin;
GRANT ALL ON FUNCTION spatial._postgis_selectivity(tbl regclass, att_name text, geom spatial.geometry, mode text) TO yg_reader_group;


--
-- Name: FUNCTION _postgis_stats(tbl regclass, att_name text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._postgis_stats(tbl regclass, att_name text, text) TO admin;
GRANT ALL ON FUNCTION spatial._postgis_stats(tbl regclass, att_name text, text) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_alignment(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_alignment(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_alignment(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_blocksize(rastschema name, rasttable name, rastcolumn name, axis text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_blocksize(rastschema name, rasttable name, rastcolumn name, axis text) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_blocksize(rastschema name, rasttable name, rastcolumn name, axis text) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_coverage_tile(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_coverage_tile(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_coverage_tile(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_extent(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_extent(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_extent(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_index(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_index(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_index(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_nodata_values(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_nodata_values(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_nodata_values(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_num_bands(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_num_bands(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_num_bands(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_out_db(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_out_db(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_out_db(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_pixel_types(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_pixel_types(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_pixel_types(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_regular_blocking(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_regular_blocking(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_regular_blocking(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_scale(rastschema name, rasttable name, rastcolumn name, axis character); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_scale(rastschema name, rasttable name, rastcolumn name, axis character) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_scale(rastschema name, rasttable name, rastcolumn name, axis character) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_spatially_unique(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_spatially_unique(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_spatially_unique(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_info_srid(rastschema name, rasttable name, rastcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_info_srid(rastschema name, rasttable name, rastcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_info_srid(rastschema name, rasttable name, rastcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_nodata_values(rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_nodata_values(rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_nodata_values(rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_out_db(rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_out_db(rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_out_db(rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION _raster_constraint_pixel_types(rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._raster_constraint_pixel_types(rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial._raster_constraint_pixel_types(rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION _st_3ddfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_3ddfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_3ddfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_3ddwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_3ddwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_3ddwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_3dintersects(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_3dintersects(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_3dintersects(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_asgml(integer, spatial.geometry, integer, integer, text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_asgml(integer, spatial.geometry, integer, integer, text, text) TO admin;
GRANT ALL ON FUNCTION spatial._st_asgml(integer, spatial.geometry, integer, integer, text, text) TO yg_reader_group;


--
-- Name: FUNCTION _st_aspect4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_aspect4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_aspect4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, width integer, height integer, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, width integer, height integer, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, width integer, height integer, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_asx3d(integer, spatial.geometry, integer, integer, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_asx3d(integer, spatial.geometry, integer, integer, text) TO admin;
GRANT ALL ON FUNCTION spatial._st_asx3d(integer, spatial.geometry, integer, integer, text) TO yg_reader_group;


--
-- Name: FUNCTION _st_bestsrid(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_bestsrid(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_bestsrid(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_bestsrid(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_bestsrid(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_bestsrid(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_clip(rast spatial.raster, nband integer[], geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_clip(rast spatial.raster, nband integer[], geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_clip(rast spatial.raster, nband integer[], geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_colormap(rast spatial.raster, nband integer, colormap text, method text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_colormap(rast spatial.raster, nband integer, colormap text, method text) TO admin;
GRANT ALL ON FUNCTION spatial._st_colormap(rast spatial.raster, nband integer, colormap text, method text) TO yg_reader_group;


--
-- Name: FUNCTION _st_contains(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_contains(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_contains(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_contains(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_contains(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_contains(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_containsproperly(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_containsproperly(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_containsproperly(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_containsproperly(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_containsproperly(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_containsproperly(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_convertarray4ma(value double precision[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_convertarray4ma(value double precision[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_convertarray4ma(value double precision[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_count(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_count(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_count(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_countagg_finalfn(agg spatial.agg_count); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_countagg_finalfn(agg spatial.agg_count) TO admin;
GRANT ALL ON FUNCTION spatial._st_countagg_finalfn(agg spatial.agg_count) TO yg_reader_group;


--
-- Name: FUNCTION _st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_countagg_transfn(agg spatial.agg_count, rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_coveredby(geog1 spatial.geography, geog2 spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_coveredby(geog1 spatial.geography, geog2 spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_coveredby(geog1 spatial.geography, geog2 spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_coveredby(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_coveredby(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_coveredby(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_coveredby(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_coveredby(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_coveredby(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_covers(geog1 spatial.geography, geog2 spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_covers(geog1 spatial.geography, geog2 spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_covers(geog1 spatial.geography, geog2 spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_covers(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_covers(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_covers(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_covers(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_covers(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_covers(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_crosses(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_crosses(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_crosses(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_dfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_dfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_dfullywithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dfullywithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_dfullywithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_distancetree(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_distancetree(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_distancetree(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_distancetree(spatial.geography, spatial.geography, double precision, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_distancetree(spatial.geography, spatial.geography, double precision, boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_distancetree(spatial.geography, spatial.geography, double precision, boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_distanceuncached(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_distanceuncached(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_distanceuncached(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_distanceuncached(spatial.geography, spatial.geography, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_distanceuncached(spatial.geography, spatial.geography, boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_distanceuncached(spatial.geography, spatial.geography, boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_distanceuncached(spatial.geography, spatial.geography, double precision, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_distanceuncached(spatial.geography, spatial.geography, double precision, boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_distanceuncached(spatial.geography, spatial.geography, double precision, boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_dwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_dwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_dwithin(geog1 spatial.geography, geog2 spatial.geography, tolerance double precision, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dwithin(geog1 spatial.geography, geog2 spatial.geography, tolerance double precision, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_dwithin(geog1 spatial.geography, geog2 spatial.geography, tolerance double precision, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_dwithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dwithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_dwithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_dwithinuncached(spatial.geography, spatial.geography, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dwithinuncached(spatial.geography, spatial.geography, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_dwithinuncached(spatial.geography, spatial.geography, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_dwithinuncached(spatial.geography, spatial.geography, double precision, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_dwithinuncached(spatial.geography, spatial.geography, double precision, boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_dwithinuncached(spatial.geography, spatial.geography, double precision, boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_equals(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_equals(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_equals(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_expand(spatial.geography, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_expand(spatial.geography, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_expand(spatial.geography, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_gdalwarp(rast spatial.raster, algorithm text, maxerr double precision, srid integer, scalex double precision, scaley double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, width integer, height integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_gdalwarp(rast spatial.raster, algorithm text, maxerr double precision, srid integer, scalex double precision, scaley double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, width integer, height integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_gdalwarp(rast spatial.raster, algorithm text, maxerr double precision, srid integer, scalex double precision, scaley double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, width integer, height integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_geomfromgml(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_geomfromgml(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_geomfromgml(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_grayscale4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_grayscale4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_grayscale4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_hillshade4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_hillshade4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_hillshade4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, width double precision[], "right" boolean, min double precision, max double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, width double precision[], "right" boolean, min double precision, max double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, width double precision[], "right" boolean, min double precision, max double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_intersects(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_intersects(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_intersects(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_intersects(geom spatial.geometry, rast spatial.raster, nband integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_intersects(geom spatial.geometry, rast spatial.raster, nband integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_intersects(geom spatial.geometry, rast spatial.raster, nband integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_intersects(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_intersects(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_intersects(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_linecrossingdirection(line1 spatial.geometry, line2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_linecrossingdirection(line1 spatial.geometry, line2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_linecrossingdirection(line1 spatial.geometry, line2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_longestline(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_longestline(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_longestline(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_mapalgebra(rastbandargset spatial.rastbandarg[], expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_mapalgebra(rastbandargset spatial.rastbandarg[], expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_mapalgebra(rastbandargset spatial.rastbandarg[], expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_mapalgebra(rastbandargset spatial.rastbandarg[], callbackfunc regprocedure, pixeltype text, distancex integer, distancey integer, extenttype text, customextent spatial.raster, mask double precision[], weighted boolean, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_mapalgebra(rastbandargset spatial.rastbandarg[], callbackfunc regprocedure, pixeltype text, distancex integer, distancey integer, extenttype text, customextent spatial.raster, mask double precision[], weighted boolean, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_mapalgebra(rastbandargset spatial.rastbandarg[], callbackfunc regprocedure, pixeltype text, distancex integer, distancey integer, extenttype text, customextent spatial.raster, mask double precision[], weighted boolean, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_maxdistance(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_maxdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_maxdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_neighborhood(rast spatial.raster, band integer, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_neighborhood(rast spatial.raster, band integer, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_neighborhood(rast spatial.raster, band integer, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_orderingequals(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_orderingequals(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_orderingequals(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_overlaps(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_overlaps(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_overlaps(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_overlaps(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_overlaps(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_overlaps(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_pixelascentroids(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_pixelascentroids(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_pixelascentroids(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_pixelaspolygons(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_pixelaspolygons(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_pixelaspolygons(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_pointoutside(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_pointoutside(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial._st_pointoutside(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION _st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_rastertoworldcoord(rast spatial.raster, columnx integer, rowy integer, OUT longitude double precision, OUT latitude double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_rastertoworldcoord(rast spatial.raster, columnx integer, rowy integer, OUT longitude double precision, OUT latitude double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_rastertoworldcoord(rast spatial.raster, columnx integer, rowy integer, OUT longitude double precision, OUT latitude double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_reclass(rast spatial.raster, VARIADIC reclassargset spatial.reclassarg[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_reclass(rast spatial.raster, VARIADIC reclassargset spatial.reclassarg[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_reclass(rast spatial.raster, VARIADIC reclassargset spatial.reclassarg[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_roughness4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_roughness4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_roughness4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_samealignment_finalfn(agg spatial.agg_samealignment); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_samealignment_finalfn(agg spatial.agg_samealignment) TO admin;
GRANT ALL ON FUNCTION spatial._st_samealignment_finalfn(agg spatial.agg_samealignment) TO yg_reader_group;


--
-- Name: FUNCTION _st_samealignment_transfn(agg spatial.agg_samealignment, rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_samealignment_transfn(agg spatial.agg_samealignment, rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial._st_samealignment_transfn(agg spatial.agg_samealignment, rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION _st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], noset boolean[], hasnosetvalue boolean, nosetvalue double precision, keepnodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], noset boolean[], hasnosetvalue boolean, nosetvalue double precision, keepnodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], noset boolean[], hasnosetvalue boolean, nosetvalue double precision, keepnodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_slope4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_slope4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_slope4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_sortablehash(geom spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_sortablehash(geom spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_sortablehash(geom spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_summarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_summarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_summarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_summarystats_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_summarystats_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial._st_summarystats_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION _st_summarystats_transfn(internal, spatial.raster, boolean, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_summarystats_transfn(internal, spatial.raster, boolean, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_summarystats_transfn(internal, spatial.raster, boolean, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_summarystats_transfn(internal, spatial.raster, integer, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_summarystats_transfn(internal, spatial.raster, integer, boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_summarystats_transfn(internal, spatial.raster, integer, boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_summarystats_transfn(internal, spatial.raster, integer, boolean, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_summarystats_transfn(internal, spatial.raster, integer, boolean, double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_summarystats_transfn(internal, spatial.raster, integer, boolean, double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_tile(rast spatial.raster, width integer, height integer, nband integer[], padwithnodata boolean, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_tile(rast spatial.raster, width integer, height integer, nband integer[], padwithnodata boolean, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_tile(rast spatial.raster, width integer, height integer, nband integer[], padwithnodata boolean, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_touches(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_touches(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_touches(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_touches(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_touches(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_touches(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_tpi4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_tpi4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_tpi4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_tri4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_tri4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_tri4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_union_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_union_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial._st_union_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION _st_union_transfn(internal, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION _st_union_transfn(internal, spatial.raster, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_union_transfn(internal, spatial.raster, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, text) TO admin;
GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, text) TO yg_reader_group;


--
-- Name: FUNCTION _st_union_transfn(internal, spatial.raster, spatial.unionarg[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, spatial.unionarg[]) TO admin;
GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, spatial.unionarg[]) TO yg_reader_group;


--
-- Name: FUNCTION _st_union_transfn(internal, spatial.raster, integer, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, integer, text) TO admin;
GRANT ALL ON FUNCTION spatial._st_union_transfn(internal, spatial.raster, integer, text) TO yg_reader_group;


--
-- Name: FUNCTION _st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial._st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION _st_voronoi(g1 spatial.geometry, clip spatial.geometry, tolerance double precision, return_polygons boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_voronoi(g1 spatial.geometry, clip spatial.geometry, tolerance double precision, return_polygons boolean) TO admin;
GRANT ALL ON FUNCTION spatial._st_voronoi(g1 spatial.geometry, clip spatial.geometry, tolerance double precision, return_polygons boolean) TO yg_reader_group;


--
-- Name: FUNCTION _st_within(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_within(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial._st_within(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION _st_within(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_within(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_within(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION _st_worldtorastercoord(rast spatial.raster, longitude double precision, latitude double precision, OUT columnx integer, OUT rowy integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._st_worldtorastercoord(rast spatial.raster, longitude double precision, latitude double precision, OUT columnx integer, OUT rowy integer) TO admin;
GRANT ALL ON FUNCTION spatial._st_worldtorastercoord(rast spatial.raster, longitude double precision, latitude double precision, OUT columnx integer, OUT rowy integer) TO yg_reader_group;


--
-- Name: FUNCTION _updaterastersrid(schema_name name, table_name name, column_name name, new_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial._updaterastersrid(schema_name name, table_name name, column_name name, new_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial._updaterastersrid(schema_name name, table_name name, column_name name, new_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION addgeometrycolumn(table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addgeometrycolumn(table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean) TO admin;
GRANT ALL ON FUNCTION spatial.addgeometrycolumn(table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean) TO yg_reader_group;


--
-- Name: FUNCTION addgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean) TO admin;
GRANT ALL ON FUNCTION spatial.addgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying, new_srid integer, new_type character varying, new_dim integer, use_typmod boolean) TO yg_reader_group;


--
-- Name: FUNCTION addgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer, new_type character varying, new_dim integer, use_typmod boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer, new_type character varying, new_dim integer, use_typmod boolean) TO admin;
GRANT ALL ON FUNCTION spatial.addgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer, new_type character varying, new_dim integer, use_typmod boolean) TO yg_reader_group;


--
-- Name: FUNCTION addoverviewconstraints(ovtable name, ovcolumn name, reftable name, refcolumn name, ovfactor integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addoverviewconstraints(ovtable name, ovcolumn name, reftable name, refcolumn name, ovfactor integer) TO admin;
GRANT ALL ON FUNCTION spatial.addoverviewconstraints(ovtable name, ovcolumn name, reftable name, refcolumn name, ovfactor integer) TO yg_reader_group;


--
-- Name: FUNCTION addoverviewconstraints(ovschema name, ovtable name, ovcolumn name, refschema name, reftable name, refcolumn name, ovfactor integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addoverviewconstraints(ovschema name, ovtable name, ovcolumn name, refschema name, reftable name, refcolumn name, ovfactor integer) TO admin;
GRANT ALL ON FUNCTION spatial.addoverviewconstraints(ovschema name, ovtable name, ovcolumn name, refschema name, reftable name, refcolumn name, ovfactor integer) TO yg_reader_group;


--
-- Name: FUNCTION addrasterconstraints(rasttable name, rastcolumn name, VARIADIC constraints text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addrasterconstraints(rasttable name, rastcolumn name, VARIADIC constraints text[]) TO admin;
GRANT ALL ON FUNCTION spatial.addrasterconstraints(rasttable name, rastcolumn name, VARIADIC constraints text[]) TO yg_reader_group;


--
-- Name: FUNCTION addrasterconstraints(rastschema name, rasttable name, rastcolumn name, VARIADIC constraints text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addrasterconstraints(rastschema name, rasttable name, rastcolumn name, VARIADIC constraints text[]) TO admin;
GRANT ALL ON FUNCTION spatial.addrasterconstraints(rastschema name, rasttable name, rastcolumn name, VARIADIC constraints text[]) TO yg_reader_group;


--
-- Name: FUNCTION addrasterconstraints(rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addrasterconstraints(rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO admin;
GRANT ALL ON FUNCTION spatial.addrasterconstraints(rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO yg_reader_group;


--
-- Name: FUNCTION addrasterconstraints(rastschema name, rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.addrasterconstraints(rastschema name, rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO admin;
GRANT ALL ON FUNCTION spatial.addrasterconstraints(rastschema name, rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO yg_reader_group;


--
-- Name: FUNCTION box3dtobox(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.box3dtobox(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.box3dtobox(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION contains_2d(spatial.box2df, spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.contains_2d(spatial.box2df, spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.contains_2d(spatial.box2df, spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION contains_2d(spatial.box2df, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.contains_2d(spatial.box2df, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.contains_2d(spatial.box2df, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION contains_2d(spatial.geometry, spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.contains_2d(spatial.geometry, spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.contains_2d(spatial.geometry, spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION dropgeometrycolumn(table_name character varying, column_name character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropgeometrycolumn(table_name character varying, column_name character varying) TO admin;
GRANT ALL ON FUNCTION spatial.dropgeometrycolumn(table_name character varying, column_name character varying) TO yg_reader_group;


--
-- Name: FUNCTION dropgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying) TO admin;
GRANT ALL ON FUNCTION spatial.dropgeometrycolumn(schema_name character varying, table_name character varying, column_name character varying) TO yg_reader_group;


--
-- Name: FUNCTION dropgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying) TO admin;
GRANT ALL ON FUNCTION spatial.dropgeometrycolumn(catalog_name character varying, schema_name character varying, table_name character varying, column_name character varying) TO yg_reader_group;


--
-- Name: FUNCTION dropgeometrytable(table_name character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropgeometrytable(table_name character varying) TO admin;
GRANT ALL ON FUNCTION spatial.dropgeometrytable(table_name character varying) TO yg_reader_group;


--
-- Name: FUNCTION dropgeometrytable(schema_name character varying, table_name character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropgeometrytable(schema_name character varying, table_name character varying) TO admin;
GRANT ALL ON FUNCTION spatial.dropgeometrytable(schema_name character varying, table_name character varying) TO yg_reader_group;


--
-- Name: FUNCTION dropgeometrytable(catalog_name character varying, schema_name character varying, table_name character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropgeometrytable(catalog_name character varying, schema_name character varying, table_name character varying) TO admin;
GRANT ALL ON FUNCTION spatial.dropgeometrytable(catalog_name character varying, schema_name character varying, table_name character varying) TO yg_reader_group;


--
-- Name: FUNCTION dropoverviewconstraints(ovtable name, ovcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropoverviewconstraints(ovtable name, ovcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial.dropoverviewconstraints(ovtable name, ovcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION dropoverviewconstraints(ovschema name, ovtable name, ovcolumn name); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.dropoverviewconstraints(ovschema name, ovtable name, ovcolumn name) TO admin;
GRANT ALL ON FUNCTION spatial.dropoverviewconstraints(ovschema name, ovtable name, ovcolumn name) TO yg_reader_group;


--
-- Name: FUNCTION droprasterconstraints(rasttable name, rastcolumn name, VARIADIC constraints text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.droprasterconstraints(rasttable name, rastcolumn name, VARIADIC constraints text[]) TO admin;
GRANT ALL ON FUNCTION spatial.droprasterconstraints(rasttable name, rastcolumn name, VARIADIC constraints text[]) TO yg_reader_group;


--
-- Name: FUNCTION droprasterconstraints(rastschema name, rasttable name, rastcolumn name, VARIADIC constraints text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.droprasterconstraints(rastschema name, rasttable name, rastcolumn name, VARIADIC constraints text[]) TO admin;
GRANT ALL ON FUNCTION spatial.droprasterconstraints(rastschema name, rasttable name, rastcolumn name, VARIADIC constraints text[]) TO yg_reader_group;


--
-- Name: FUNCTION droprasterconstraints(rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.droprasterconstraints(rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO admin;
GRANT ALL ON FUNCTION spatial.droprasterconstraints(rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO yg_reader_group;


--
-- Name: FUNCTION droprasterconstraints(rastschema name, rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.droprasterconstraints(rastschema name, rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO admin;
GRANT ALL ON FUNCTION spatial.droprasterconstraints(rastschema name, rasttable name, rastcolumn name, srid boolean, scale_x boolean, scale_y boolean, blocksize_x boolean, blocksize_y boolean, same_alignment boolean, regular_blocking boolean, num_bands boolean, pixel_types boolean, nodata_values boolean, out_db boolean, extent boolean) TO yg_reader_group;


--
-- Name: FUNCTION equals(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.equals(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.equals(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION find_srid(character varying, character varying, character varying); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.find_srid(character varying, character varying, character varying) TO admin;
GRANT ALL ON FUNCTION spatial.find_srid(character varying, character varying, character varying) TO yg_reader_group;


--
-- Name: FUNCTION geog_brin_inclusion_add_value(internal, internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geog_brin_inclusion_add_value(internal, internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geog_brin_inclusion_add_value(internal, internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_cmp(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_cmp(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_cmp(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_distance_knn(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_distance_knn(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_distance_knn(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_eq(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_eq(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_eq(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_ge(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_ge(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_ge(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_compress(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_compress(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_compress(internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_consistent(internal, spatial.geography, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_consistent(internal, spatial.geography, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_consistent(internal, spatial.geography, integer) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_decompress(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_decompress(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_decompress(internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_distance(internal, spatial.geography, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_distance(internal, spatial.geography, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_distance(internal, spatial.geography, integer) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_penalty(internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_penalty(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_penalty(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_picksplit(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_picksplit(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_picksplit(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_same(spatial.box2d, spatial.box2d, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_same(spatial.box2d, spatial.box2d, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_same(spatial.box2d, spatial.box2d, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_gist_union(bytea, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gist_union(bytea, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gist_union(bytea, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_gt(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_gt(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_gt(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_le(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_le(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_le(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_lt(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_lt(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_lt(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_overlaps(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_overlaps(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geography_overlaps(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geography_spgist_choose_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_spgist_choose_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_spgist_choose_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_spgist_compress_nd(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_spgist_compress_nd(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_spgist_compress_nd(internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_spgist_config_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_spgist_config_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_spgist_config_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_spgist_inner_consistent_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_spgist_inner_consistent_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_spgist_inner_consistent_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_spgist_leaf_consistent_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_spgist_leaf_consistent_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_spgist_leaf_consistent_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geography_spgist_picksplit_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geography_spgist_picksplit_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geography_spgist_picksplit_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geom2d_brin_inclusion_add_value(internal, internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geom2d_brin_inclusion_add_value(internal, internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geom2d_brin_inclusion_add_value(internal, internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geom3d_brin_inclusion_add_value(internal, internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geom3d_brin_inclusion_add_value(internal, internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geom3d_brin_inclusion_add_value(internal, internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geom4d_brin_inclusion_add_value(internal, internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geom4d_brin_inclusion_add_value(internal, internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geom4d_brin_inclusion_add_value(internal, internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_above(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_above(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_above(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_below(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_below(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_below(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_cmp(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_cmp(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_cmp(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_contained_3d(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_contained_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_contained_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_contained_by_raster(spatial.geometry, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_contained_by_raster(spatial.geometry, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_contained_by_raster(spatial.geometry, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION geometry_contains(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_contains(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_contains(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_contains_3d(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_contains_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_contains_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_contains_nd(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_contains_nd(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_contains_nd(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_distance_box(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_distance_box(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_distance_box(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_distance_centroid(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_distance_centroid(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_distance_centroid(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_distance_centroid_nd(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_distance_centroid_nd(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_distance_centroid_nd(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_distance_cpa(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_distance_cpa(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_distance_cpa(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_eq(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_eq(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_eq(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_ge(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_ge(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_ge(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_compress_2d(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_compress_2d(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_compress_2d(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_compress_nd(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_compress_nd(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_compress_nd(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_consistent_2d(internal, spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_consistent_2d(internal, spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_consistent_2d(internal, spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_consistent_nd(internal, spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_consistent_nd(internal, spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_consistent_nd(internal, spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_decompress_2d(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_decompress_2d(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_decompress_2d(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_decompress_nd(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_decompress_nd(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_decompress_nd(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_distance_2d(internal, spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_distance_2d(internal, spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_distance_2d(internal, spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_distance_nd(internal, spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_distance_nd(internal, spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_distance_nd(internal, spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_penalty_2d(internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_penalty_2d(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_penalty_2d(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_penalty_nd(internal, internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_penalty_nd(internal, internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_penalty_nd(internal, internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_picksplit_2d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_picksplit_2d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_picksplit_2d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_picksplit_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_picksplit_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_picksplit_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_same_2d(geom1 spatial.geometry, geom2 spatial.geometry, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_same_2d(geom1 spatial.geometry, geom2 spatial.geometry, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_same_2d(geom1 spatial.geometry, geom2 spatial.geometry, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_same_nd(spatial.geometry, spatial.geometry, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_same_nd(spatial.geometry, spatial.geometry, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_same_nd(spatial.geometry, spatial.geometry, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_sortsupport_2d(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_sortsupport_2d(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_sortsupport_2d(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_union_2d(bytea, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_union_2d(bytea, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_union_2d(bytea, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gist_union_nd(bytea, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gist_union_nd(bytea, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gist_union_nd(bytea, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_gt(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_gt(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_gt(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_hash(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_hash(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_hash(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_le(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_le(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_le(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_left(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_left(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_left(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_lt(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_lt(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_lt(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_neq(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_neq(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_neq(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overabove(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overabove(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overabove(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overbelow(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overbelow(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overbelow(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overlaps(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overlaps(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overlaps(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overlaps_3d(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overlaps_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overlaps_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overlaps_nd(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overlaps_nd(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overlaps_nd(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overleft(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overleft(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overleft(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_overright(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_overright(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_overright(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_raster_contain(spatial.geometry, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_raster_contain(spatial.geometry, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_raster_contain(spatial.geometry, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION geometry_raster_overlap(spatial.geometry, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_raster_overlap(spatial.geometry, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_raster_overlap(spatial.geometry, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION geometry_right(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_right(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_right(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_same(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_same(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_same(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_same_3d(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_same_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_same_3d(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_same_nd(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_same_nd(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_same_nd(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_sortsupport(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_sortsupport(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_sortsupport(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_choose_2d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_choose_2d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_choose_2d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_choose_3d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_choose_3d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_choose_3d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_choose_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_choose_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_choose_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_compress_2d(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_compress_2d(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_compress_2d(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_compress_3d(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_compress_3d(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_compress_3d(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_compress_nd(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_compress_nd(internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_compress_nd(internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_config_2d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_config_2d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_config_2d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_config_3d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_config_3d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_config_3d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_config_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_config_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_config_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_inner_consistent_2d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_inner_consistent_2d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_inner_consistent_2d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_inner_consistent_3d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_inner_consistent_3d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_inner_consistent_3d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_inner_consistent_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_inner_consistent_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_inner_consistent_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_leaf_consistent_2d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_leaf_consistent_2d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_leaf_consistent_2d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_leaf_consistent_3d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_leaf_consistent_3d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_leaf_consistent_3d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_leaf_consistent_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_leaf_consistent_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_leaf_consistent_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_picksplit_2d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_picksplit_2d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_picksplit_2d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_picksplit_3d(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_picksplit_3d(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_picksplit_3d(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_spgist_picksplit_nd(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_spgist_picksplit_nd(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_spgist_picksplit_nd(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION geometry_within(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_within(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_within(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometry_within_nd(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometry_within_nd(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometry_within_nd(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geometrytype(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometrytype(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.geometrytype(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION geometrytype(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geometrytype(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.geometrytype(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION geomfromewkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geomfromewkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.geomfromewkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION geomfromewkt(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.geomfromewkt(text) TO admin;
GRANT ALL ON FUNCTION spatial.geomfromewkt(text) TO yg_reader_group;


--
-- Name: FUNCTION get_proj4_from_srid(integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.get_proj4_from_srid(integer) TO admin;
GRANT ALL ON FUNCTION spatial.get_proj4_from_srid(integer) TO yg_reader_group;


--
-- Name: FUNCTION gserialized_gist_joinsel_2d(internal, oid, internal, smallint); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.gserialized_gist_joinsel_2d(internal, oid, internal, smallint) TO admin;
GRANT ALL ON FUNCTION spatial.gserialized_gist_joinsel_2d(internal, oid, internal, smallint) TO yg_reader_group;


--
-- Name: FUNCTION gserialized_gist_joinsel_nd(internal, oid, internal, smallint); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.gserialized_gist_joinsel_nd(internal, oid, internal, smallint) TO admin;
GRANT ALL ON FUNCTION spatial.gserialized_gist_joinsel_nd(internal, oid, internal, smallint) TO yg_reader_group;


--
-- Name: FUNCTION gserialized_gist_sel_2d(internal, oid, internal, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.gserialized_gist_sel_2d(internal, oid, internal, integer) TO admin;
GRANT ALL ON FUNCTION spatial.gserialized_gist_sel_2d(internal, oid, internal, integer) TO yg_reader_group;


--
-- Name: FUNCTION gserialized_gist_sel_nd(internal, oid, internal, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.gserialized_gist_sel_nd(internal, oid, internal, integer) TO admin;
GRANT ALL ON FUNCTION spatial.gserialized_gist_sel_nd(internal, oid, internal, integer) TO yg_reader_group;


--
-- Name: FUNCTION is_contained_2d(spatial.box2df, spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.is_contained_2d(spatial.box2df, spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.is_contained_2d(spatial.box2df, spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION is_contained_2d(spatial.box2df, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.is_contained_2d(spatial.box2df, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.is_contained_2d(spatial.box2df, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION is_contained_2d(spatial.geometry, spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.is_contained_2d(spatial.geometry, spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.is_contained_2d(spatial.geometry, spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_2d(spatial.box2df, spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_2d(spatial.box2df, spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_2d(spatial.box2df, spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_2d(spatial.box2df, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_2d(spatial.box2df, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_2d(spatial.box2df, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_2d(spatial.geometry, spatial.box2df); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_2d(spatial.geometry, spatial.box2df) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_2d(spatial.geometry, spatial.box2df) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_geog(spatial.geography, spatial.gidx); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_geog(spatial.geography, spatial.gidx) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_geog(spatial.geography, spatial.gidx) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_geog(spatial.gidx, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_geog(spatial.gidx, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_geog(spatial.gidx, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_geog(spatial.gidx, spatial.gidx); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_geog(spatial.gidx, spatial.gidx) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_geog(spatial.gidx, spatial.gidx) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_nd(spatial.geometry, spatial.gidx); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_nd(spatial.geometry, spatial.gidx) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_nd(spatial.geometry, spatial.gidx) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_nd(spatial.gidx, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_nd(spatial.gidx, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_nd(spatial.gidx, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION overlaps_nd(spatial.gidx, spatial.gidx); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.overlaps_nd(spatial.gidx, spatial.gidx) TO admin;
GRANT ALL ON FUNCTION spatial.overlaps_nd(spatial.gidx, spatial.gidx) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asflatgeobuf_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asflatgeobuf_transfn(internal, anyelement); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_transfn(internal, anyelement) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_transfn(internal, anyelement) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asflatgeobuf_transfn(internal, anyelement, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_transfn(internal, anyelement, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_transfn(internal, anyelement, boolean) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asflatgeobuf_transfn(internal, anyelement, boolean, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_transfn(internal, anyelement, boolean, text) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asflatgeobuf_transfn(internal, anyelement, boolean, text) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asgeobuf_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asgeobuf_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asgeobuf_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asgeobuf_transfn(internal, anyelement); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asgeobuf_transfn(internal, anyelement) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asgeobuf_transfn(internal, anyelement) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asgeobuf_transfn(internal, anyelement, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asgeobuf_transfn(internal, anyelement, text) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asgeobuf_transfn(internal, anyelement, text) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_combinefn(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_combinefn(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_combinefn(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_deserialfn(bytea, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_deserialfn(bytea, internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_deserialfn(bytea, internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_serialfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_serialfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_serialfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_transfn(internal, anyelement); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_transfn(internal, anyelement, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_transfn(internal, anyelement, text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text, integer) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_transfn(internal, anyelement, text, integer, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text, integer, text) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text, integer, text) TO yg_reader_group;


--
-- Name: FUNCTION pgis_asmvt_transfn(internal, anyelement, text, integer, text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text, integer, text, text) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_asmvt_transfn(internal, anyelement, text, integer, text, text) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_accum_transfn(internal, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_accum_transfn(internal, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_accum_transfn(internal, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_accum_transfn(internal, spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_accum_transfn(internal, spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_accum_transfn(internal, spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_accum_transfn(internal, spatial.geometry, double precision, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_accum_transfn(internal, spatial.geometry, double precision, integer) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_accum_transfn(internal, spatial.geometry, double precision, integer) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_clusterintersecting_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_clusterintersecting_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_clusterintersecting_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_clusterwithin_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_clusterwithin_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_clusterwithin_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_collect_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_collect_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_collect_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_coverageunion_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_coverageunion_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_coverageunion_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_makeline_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_makeline_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_makeline_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_polygonize_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_polygonize_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_polygonize_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_union_parallel_combinefn(internal, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_combinefn(internal, internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_combinefn(internal, internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_union_parallel_deserialfn(bytea, internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_deserialfn(bytea, internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_deserialfn(bytea, internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_union_parallel_finalfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_finalfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_finalfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_union_parallel_serialfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_serialfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_serialfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_union_parallel_transfn(internal, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_transfn(internal, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_transfn(internal, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION pgis_geometry_union_parallel_transfn(internal, spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_transfn(internal, spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.pgis_geometry_union_parallel_transfn(internal, spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION populate_geometry_columns(use_typmod boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.populate_geometry_columns(use_typmod boolean) TO admin;
GRANT ALL ON FUNCTION spatial.populate_geometry_columns(use_typmod boolean) TO yg_reader_group;


--
-- Name: FUNCTION populate_geometry_columns(tbl_oid oid, use_typmod boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.populate_geometry_columns(tbl_oid oid, use_typmod boolean) TO admin;
GRANT ALL ON FUNCTION spatial.populate_geometry_columns(tbl_oid oid, use_typmod boolean) TO yg_reader_group;


--
-- Name: FUNCTION postgis_addbbox(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_addbbox(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_addbbox(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION postgis_cache_bbox(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_cache_bbox() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_cache_bbox() TO yg_reader_group;


--
-- Name: FUNCTION postgis_constraint_dims(geomschema text, geomtable text, geomcolumn text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_constraint_dims(geomschema text, geomtable text, geomcolumn text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_constraint_dims(geomschema text, geomtable text, geomcolumn text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_constraint_srid(geomschema text, geomtable text, geomcolumn text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_constraint_srid(geomschema text, geomtable text, geomcolumn text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_constraint_srid(geomschema text, geomtable text, geomcolumn text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_constraint_type(geomschema text, geomtable text, geomcolumn text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_constraint_type(geomschema text, geomtable text, geomcolumn text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_constraint_type(geomschema text, geomtable text, geomcolumn text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_dropbbox(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_dropbbox(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_dropbbox(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION postgis_extensions_upgrade(target_version text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_extensions_upgrade(target_version text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_extensions_upgrade(target_version text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_full_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_full_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_full_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_gdal_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_gdal_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_gdal_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_geos_compiled_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_geos_compiled_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_geos_compiled_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_geos_noop(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_geos_noop(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_geos_noop(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION postgis_geos_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_geos_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_geos_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_getbbox(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_getbbox(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_getbbox(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION postgis_hasbbox(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_hasbbox(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_hasbbox(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION postgis_index_supportfn(internal); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_index_supportfn(internal) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_index_supportfn(internal) TO yg_reader_group;


--
-- Name: FUNCTION postgis_lib_build_date(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_lib_build_date() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_lib_build_date() TO yg_reader_group;


--
-- Name: FUNCTION postgis_lib_revision(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_lib_revision() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_lib_revision() TO yg_reader_group;


--
-- Name: FUNCTION postgis_lib_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_lib_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_lib_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_libjson_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_libjson_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_libjson_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_liblwgeom_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_liblwgeom_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_liblwgeom_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_libprotobuf_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_libprotobuf_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_libprotobuf_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_libxml_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_libxml_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_libxml_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_noop(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_noop(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_noop(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION postgis_noop(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_noop(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_noop(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION postgis_proj_compiled_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_proj_compiled_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_proj_compiled_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_proj_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_proj_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_proj_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_raster_lib_build_date(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_raster_lib_build_date() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_raster_lib_build_date() TO yg_reader_group;


--
-- Name: FUNCTION postgis_raster_lib_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_raster_lib_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_raster_lib_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_raster_scripts_installed(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_raster_scripts_installed() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_raster_scripts_installed() TO yg_reader_group;


--
-- Name: FUNCTION postgis_scripts_build_date(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_scripts_build_date() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_scripts_build_date() TO yg_reader_group;


--
-- Name: FUNCTION postgis_scripts_installed(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_scripts_installed() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_scripts_installed() TO yg_reader_group;


--
-- Name: FUNCTION postgis_scripts_released(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_scripts_released() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_scripts_released() TO yg_reader_group;


--
-- Name: FUNCTION postgis_srs(auth_name text, auth_srid text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_srs(auth_name text, auth_srid text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_srs(auth_name text, auth_srid text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_srs_all(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_srs_all() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_srs_all() TO yg_reader_group;


--
-- Name: FUNCTION postgis_srs_codes(auth_name text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_srs_codes(auth_name text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_srs_codes(auth_name text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_srs_search(bounds spatial.geometry, authname text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_srs_search(bounds spatial.geometry, authname text) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_srs_search(bounds spatial.geometry, authname text) TO yg_reader_group;


--
-- Name: FUNCTION postgis_svn_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_svn_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_svn_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_transform_geometry(geom spatial.geometry, text, text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_transform_geometry(geom spatial.geometry, text, text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_transform_geometry(geom spatial.geometry, text, text, integer) TO yg_reader_group;


--
-- Name: FUNCTION postgis_transform_pipeline_geometry(geom spatial.geometry, pipeline text, forward boolean, to_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_transform_pipeline_geometry(geom spatial.geometry, pipeline text, forward boolean, to_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_transform_pipeline_geometry(geom spatial.geometry, pipeline text, forward boolean, to_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION postgis_type_name(geomname character varying, coord_dimension integer, use_new_name boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_type_name(geomname character varying, coord_dimension integer, use_new_name boolean) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_type_name(geomname character varying, coord_dimension integer, use_new_name boolean) TO yg_reader_group;


--
-- Name: FUNCTION postgis_typmod_dims(integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_typmod_dims(integer) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_typmod_dims(integer) TO yg_reader_group;


--
-- Name: FUNCTION postgis_typmod_srid(integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_typmod_srid(integer) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_typmod_srid(integer) TO yg_reader_group;


--
-- Name: FUNCTION postgis_typmod_type(integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_typmod_type(integer) TO admin;
GRANT ALL ON FUNCTION spatial.postgis_typmod_type(integer) TO yg_reader_group;


--
-- Name: FUNCTION postgis_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_version() TO yg_reader_group;


--
-- Name: FUNCTION postgis_wagyu_version(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.postgis_wagyu_version() TO admin;
GRANT ALL ON FUNCTION spatial.postgis_wagyu_version() TO yg_reader_group;


--
-- Name: FUNCTION raster_above(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_above(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_above(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_below(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_below(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_below(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_contain(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_contain(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_contain(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_contained(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_contained(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_contained(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_contained_by_geometry(spatial.raster, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_contained_by_geometry(spatial.raster, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.raster_contained_by_geometry(spatial.raster, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION raster_eq(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_eq(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_eq(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_geometry_contain(spatial.raster, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_geometry_contain(spatial.raster, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.raster_geometry_contain(spatial.raster, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION raster_geometry_overlap(spatial.raster, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_geometry_overlap(spatial.raster, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.raster_geometry_overlap(spatial.raster, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION raster_hash(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_hash(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_hash(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_left(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_left(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_left(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_overabove(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_overabove(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_overabove(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_overbelow(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_overbelow(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_overbelow(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_overlap(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_overlap(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_overlap(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_overleft(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_overleft(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_overleft(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_overright(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_overright(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_overright(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_right(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_right(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_right(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION raster_same(spatial.raster, spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.raster_same(spatial.raster, spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.raster_same(spatial.raster, spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_3dclosestpoint(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dclosestpoint(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dclosestpoint(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3ddfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3ddfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_3ddfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_3ddistance(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3ddistance(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3ddistance(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3ddwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3ddwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_3ddwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_3dintersects(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dintersects(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dintersects(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3dlength(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dlength(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dlength(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3dlineinterpolatepoint(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dlineinterpolatepoint(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dlineinterpolatepoint(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_3dlongestline(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dlongestline(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dlongestline(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3dmakebox(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dmakebox(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dmakebox(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3dmaxdistance(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dmaxdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dmaxdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3dperimeter(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dperimeter(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dperimeter(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_3dshortestline(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dshortestline(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dshortestline(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(rast spatial.raster, addbandargset spatial.addbandarg[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, addbandargset spatial.addbandarg[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, addbandargset spatial.addbandarg[]) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(rast spatial.raster, pixeltype text, initialvalue double precision, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, pixeltype text, initialvalue double precision, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, pixeltype text, initialvalue double precision, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(torast spatial.raster, fromrasts spatial.raster[], fromband integer, torastindex integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(torast spatial.raster, fromrasts spatial.raster[], fromband integer, torastindex integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(torast spatial.raster, fromrasts spatial.raster[], fromband integer, torastindex integer) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(torast spatial.raster, fromrast spatial.raster, fromband integer, torastindex integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(torast spatial.raster, fromrast spatial.raster, fromband integer, torastindex integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(torast spatial.raster, fromrast spatial.raster, fromband integer, torastindex integer) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(rast spatial.raster, index integer, outdbfile text, outdbindex integer[], nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, index integer, outdbfile text, outdbindex integer[], nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, index integer, outdbfile text, outdbindex integer[], nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(rast spatial.raster, index integer, pixeltype text, initialvalue double precision, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, index integer, pixeltype text, initialvalue double precision, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, index integer, pixeltype text, initialvalue double precision, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_addband(rast spatial.raster, outdbfile text, outdbindex integer[], index integer, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, outdbfile text, outdbindex integer[], index integer, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_addband(rast spatial.raster, outdbfile text, outdbindex integer[], index integer, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_addmeasure(spatial.geometry, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addmeasure(spatial.geometry, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_addmeasure(spatial.geometry, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_addpoint(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addpoint(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_addpoint(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_addpoint(geom1 spatial.geometry, geom2 spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_addpoint(geom1 spatial.geometry, geom2 spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_addpoint(geom1 spatial.geometry, geom2 spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_affine(spatial.geometry, double precision, double precision, double precision, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_affine(spatial.geometry, double precision, double precision, double precision, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_affine(spatial.geometry, double precision, double precision, double precision, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_affine(spatial.geometry, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_affine(spatial.geometry, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_affine(spatial.geometry, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_angle(line1 spatial.geometry, line2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_angle(line1 spatial.geometry, line2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_angle(line1 spatial.geometry, line2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_angle(pt1 spatial.geometry, pt2 spatial.geometry, pt3 spatial.geometry, pt4 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_angle(pt1 spatial.geometry, pt2 spatial.geometry, pt3 spatial.geometry, pt4 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_angle(pt1 spatial.geometry, pt2 spatial.geometry, pt3 spatial.geometry, pt4 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_approxcount(rast spatial.raster, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxcount(rast spatial.raster, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxcount(rast spatial.raster, nband integer, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, nband integer, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, nband integer, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxcount(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxcount(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxhistogram(rast spatial.raster, sample_percent double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, sample_percent double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, sample_percent double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxhistogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, sample_percent double precision, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxhistogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxhistogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, exclude_nodata_value boolean, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, exclude_nodata_value boolean, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, exclude_nodata_value boolean, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, sample_percent double precision, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, sample_percent double precision, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, sample_percent double precision, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, nband integer, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, nband integer, sample_percent double precision, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, sample_percent double precision, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, sample_percent double precision, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxquantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxquantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxsummarystats(rast spatial.raster, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxsummarystats(rast spatial.raster, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxsummarystats(rast spatial.raster, nband integer, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, nband integer, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, nband integer, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_approxsummarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_approxsummarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean, sample_percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_area(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_area(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_area(text) TO yg_reader_group;


--
-- Name: FUNCTION st_area(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_area(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_area(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_area(geog spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_area(geog spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_area(geog spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_area2d(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_area2d(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_area2d(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_asbinary(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_asbinary(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_asbinary(spatial.geography, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geography, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geography, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asbinary(spatial.geometry, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geometry, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.geometry, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asbinary(spatial.raster, outasin boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.raster, outasin boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asbinary(spatial.raster, outasin boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asencodedpolyline(geom spatial.geometry, nprecision integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asencodedpolyline(geom spatial.geometry, nprecision integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asencodedpolyline(geom spatial.geometry, nprecision integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkb(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkb(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkb(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkb(spatial.geometry, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkb(spatial.geometry, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkb(spatial.geometry, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkt(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkt(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkt(text) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkt(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkt(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkt(spatial.geography, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geography, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geography, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asewkt(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asewkt(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asgdalraster(rast spatial.raster, format text, options text[], srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgdalraster(rast spatial.raster, format text, options text[], srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgdalraster(rast spatial.raster, format text, options text[], srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asgeojson(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgeojson(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgeojson(text) TO yg_reader_group;


--
-- Name: FUNCTION st_asgeojson(geog spatial.geography, maxdecimaldigits integer, options integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgeojson(geog spatial.geography, maxdecimaldigits integer, options integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgeojson(geog spatial.geography, maxdecimaldigits integer, options integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asgeojson(geom spatial.geometry, maxdecimaldigits integer, options integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgeojson(geom spatial.geometry, maxdecimaldigits integer, options integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgeojson(geom spatial.geometry, maxdecimaldigits integer, options integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asgeojson(r record, geom_column text, maxdecimaldigits integer, pretty_bool boolean, id_column text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgeojson(r record, geom_column text, maxdecimaldigits integer, pretty_bool boolean, id_column text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgeojson(r record, geom_column text, maxdecimaldigits integer, pretty_bool boolean, id_column text) TO yg_reader_group;


--
-- Name: FUNCTION st_asgml(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgml(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgml(text) TO yg_reader_group;


--
-- Name: FUNCTION st_asgml(geom spatial.geometry, maxdecimaldigits integer, options integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgml(geom spatial.geometry, maxdecimaldigits integer, options integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgml(geom spatial.geometry, maxdecimaldigits integer, options integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asgml(geog spatial.geography, maxdecimaldigits integer, options integer, nprefix text, id text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgml(geog spatial.geography, maxdecimaldigits integer, options integer, nprefix text, id text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgml(geog spatial.geography, maxdecimaldigits integer, options integer, nprefix text, id text) TO yg_reader_group;


--
-- Name: FUNCTION st_asgml(version integer, geog spatial.geography, maxdecimaldigits integer, options integer, nprefix text, id text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgml(version integer, geog spatial.geography, maxdecimaldigits integer, options integer, nprefix text, id text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgml(version integer, geog spatial.geography, maxdecimaldigits integer, options integer, nprefix text, id text) TO yg_reader_group;


--
-- Name: FUNCTION st_asgml(version integer, geom spatial.geometry, maxdecimaldigits integer, options integer, nprefix text, id text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgml(version integer, geom spatial.geometry, maxdecimaldigits integer, options integer, nprefix text, id text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgml(version integer, geom spatial.geometry, maxdecimaldigits integer, options integer, nprefix text, id text) TO yg_reader_group;


--
-- Name: FUNCTION st_ashexewkb(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ashexewkb(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_ashexewkb(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_ashexewkb(spatial.geometry, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ashexewkb(spatial.geometry, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_ashexewkb(spatial.geometry, text) TO yg_reader_group;


--
-- Name: FUNCTION st_ashexwkb(spatial.raster, outasin boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ashexwkb(spatial.raster, outasin boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_ashexwkb(spatial.raster, outasin boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asjpeg(rast spatial.raster, options text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, options text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, options text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_asjpeg(rast spatial.raster, nbands integer[], options text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nbands integer[], options text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nbands integer[], options text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_asjpeg(rast spatial.raster, nbands integer[], quality integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nbands integer[], quality integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nbands integer[], quality integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asjpeg(rast spatial.raster, nband integer, options text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nband integer, options text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nband integer, options text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_asjpeg(rast spatial.raster, nband integer, quality integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nband integer, quality integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asjpeg(rast spatial.raster, nband integer, quality integer) TO yg_reader_group;


--
-- Name: FUNCTION st_askml(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_askml(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_askml(text) TO yg_reader_group;


--
-- Name: FUNCTION st_askml(geog spatial.geography, maxdecimaldigits integer, nprefix text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_askml(geog spatial.geography, maxdecimaldigits integer, nprefix text) TO admin;
GRANT ALL ON FUNCTION spatial.st_askml(geog spatial.geography, maxdecimaldigits integer, nprefix text) TO yg_reader_group;


--
-- Name: FUNCTION st_askml(geom spatial.geometry, maxdecimaldigits integer, nprefix text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_askml(geom spatial.geometry, maxdecimaldigits integer, nprefix text) TO admin;
GRANT ALL ON FUNCTION spatial.st_askml(geom spatial.geometry, maxdecimaldigits integer, nprefix text) TO yg_reader_group;


--
-- Name: FUNCTION st_aslatlontext(geom spatial.geometry, tmpl text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aslatlontext(geom spatial.geometry, tmpl text) TO admin;
GRANT ALL ON FUNCTION spatial.st_aslatlontext(geom spatial.geometry, tmpl text) TO yg_reader_group;


--
-- Name: FUNCTION st_asmarc21(geom spatial.geometry, format text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmarc21(geom spatial.geometry, format text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmarc21(geom spatial.geometry, format text) TO yg_reader_group;


--
-- Name: FUNCTION st_asmvtgeom(geom spatial.geometry, bounds spatial.box2d, extent integer, buffer integer, clip_geom boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmvtgeom(geom spatial.geometry, bounds spatial.box2d, extent integer, buffer integer, clip_geom boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmvtgeom(geom spatial.geometry, bounds spatial.box2d, extent integer, buffer integer, clip_geom boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_aspect(rast spatial.raster, nband integer, pixeltype text, units text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspect(rast spatial.raster, nband integer, pixeltype text, units text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspect(rast spatial.raster, nband integer, pixeltype text, units text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_aspect(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, units text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspect(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, units text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspect(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, units text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_aspng(rast spatial.raster, options text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, options text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, options text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_aspng(rast spatial.raster, nbands integer[], options text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nbands integer[], options text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nbands integer[], options text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_aspng(rast spatial.raster, nbands integer[], compression integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nbands integer[], compression integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nbands integer[], compression integer) TO yg_reader_group;


--
-- Name: FUNCTION st_aspng(rast spatial.raster, nband integer, options text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nband integer, options text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nband integer, options text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_aspng(rast spatial.raster, nband integer, compression integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nband integer, compression integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_aspng(rast spatial.raster, nband integer, compression integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, ref spatial.raster, pixeltype text[], value double precision[], nodataval double precision[], touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, ref spatial.raster, pixeltype text[], value double precision[], nodataval double precision[], touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, ref spatial.raster, pixeltype text[], value double precision[], nodataval double precision[], touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, ref spatial.raster, pixeltype text, value double precision, nodataval double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, ref spatial.raster, pixeltype text, value double precision, nodataval double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, ref spatial.raster, pixeltype text, value double precision, nodataval double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, gridx double precision, gridy double precision, pixeltype text[], value double precision[], nodataval double precision[], skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, gridx double precision, gridy double precision, pixeltype text[], value double precision[], nodataval double precision[], skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, gridx double precision, gridy double precision, pixeltype text[], value double precision[], nodataval double precision[], skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, gridx double precision, gridy double precision, pixeltype text, value double precision, nodataval double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, gridx double precision, gridy double precision, pixeltype text, value double precision, nodataval double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, gridx double precision, gridy double precision, pixeltype text, value double precision, nodataval double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, pixeltype text, value double precision, nodataval double precision, upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, pixeltype text, value double precision, nodataval double precision, upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, scalex double precision, scaley double precision, pixeltype text, value double precision, nodataval double precision, upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, width integer, height integer, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, pixeltype text[], value double precision[], nodataval double precision[], upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, width integer, height integer, gridx double precision, gridy double precision, pixeltype text[], value double precision[], nodataval double precision[], skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, gridx double precision, gridy double precision, pixeltype text[], value double precision[], nodataval double precision[], skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, gridx double precision, gridy double precision, pixeltype text[], value double precision[], nodataval double precision[], skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, width integer, height integer, gridx double precision, gridy double precision, pixeltype text, value double precision, nodataval double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, gridx double precision, gridy double precision, pixeltype text, value double precision, nodataval double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, gridx double precision, gridy double precision, pixeltype text, value double precision, nodataval double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asraster(geom spatial.geometry, width integer, height integer, pixeltype text, value double precision, nodataval double precision, upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, pixeltype text, value double precision, nodataval double precision, upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asraster(geom spatial.geometry, width integer, height integer, pixeltype text, value double precision, nodataval double precision, upperleftx double precision, upperlefty double precision, skewx double precision, skewy double precision, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_assvg(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_assvg(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_assvg(text) TO yg_reader_group;


--
-- Name: FUNCTION st_assvg(geog spatial.geography, rel integer, maxdecimaldigits integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_assvg(geog spatial.geography, rel integer, maxdecimaldigits integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_assvg(geog spatial.geography, rel integer, maxdecimaldigits integer) TO yg_reader_group;


--
-- Name: FUNCTION st_assvg(geom spatial.geometry, rel integer, maxdecimaldigits integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_assvg(geom spatial.geometry, rel integer, maxdecimaldigits integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_assvg(geom spatial.geometry, rel integer, maxdecimaldigits integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_astext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_astext(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astext(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_astext(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_astext(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astext(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_astext(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_astext(spatial.geography, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astext(spatial.geography, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_astext(spatial.geography, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astext(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astext(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_astext(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astiff(rast spatial.raster, options text[], srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, options text[], srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, options text[], srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astiff(rast spatial.raster, compression text, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, compression text, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, compression text, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astiff(rast spatial.raster, nbands integer[], options text[], srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, nbands integer[], options text[], srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, nbands integer[], options text[], srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astiff(rast spatial.raster, nbands integer[], compression text, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, nbands integer[], compression text, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_astiff(rast spatial.raster, nbands integer[], compression text, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_astwkb(geom spatial.geometry, prec integer, prec_z integer, prec_m integer, with_sizes boolean, with_boxes boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astwkb(geom spatial.geometry, prec integer, prec_z integer, prec_m integer, with_sizes boolean, with_boxes boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_astwkb(geom spatial.geometry, prec integer, prec_z integer, prec_m integer, with_sizes boolean, with_boxes boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_astwkb(geom spatial.geometry[], ids bigint[], prec integer, prec_z integer, prec_m integer, with_sizes boolean, with_boxes boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_astwkb(geom spatial.geometry[], ids bigint[], prec integer, prec_z integer, prec_m integer, with_sizes boolean, with_boxes boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_astwkb(geom spatial.geometry[], ids bigint[], prec integer, prec_z integer, prec_m integer, with_sizes boolean, with_boxes boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_aswkb(spatial.raster, outasin boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_aswkb(spatial.raster, outasin boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_aswkb(spatial.raster, outasin boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asx3d(geom spatial.geometry, maxdecimaldigits integer, options integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asx3d(geom spatial.geometry, maxdecimaldigits integer, options integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asx3d(geom spatial.geometry, maxdecimaldigits integer, options integer) TO yg_reader_group;


--
-- Name: FUNCTION st_azimuth(geog1 spatial.geography, geog2 spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_azimuth(geog1 spatial.geography, geog2 spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_azimuth(geog1 spatial.geography, geog2 spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_azimuth(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_azimuth(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_azimuth(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_band(rast spatial.raster, nbands integer[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_band(rast spatial.raster, nbands integer[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_band(rast spatial.raster, nbands integer[]) TO yg_reader_group;


--
-- Name: FUNCTION st_band(rast spatial.raster, nband integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_band(rast spatial.raster, nband integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_band(rast spatial.raster, nband integer) TO yg_reader_group;


--
-- Name: FUNCTION st_band(rast spatial.raster, nbands text, delimiter character); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_band(rast spatial.raster, nbands text, delimiter character) TO admin;
GRANT ALL ON FUNCTION spatial.st_band(rast spatial.raster, nbands text, delimiter character) TO yg_reader_group;


--
-- Name: FUNCTION st_bandfilesize(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandfilesize(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandfilesize(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bandfiletimestamp(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandfiletimestamp(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandfiletimestamp(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bandisnodata(rast spatial.raster, forcechecking boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandisnodata(rast spatial.raster, forcechecking boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandisnodata(rast spatial.raster, forcechecking boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_bandisnodata(rast spatial.raster, band integer, forcechecking boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandisnodata(rast spatial.raster, band integer, forcechecking boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandisnodata(rast spatial.raster, band integer, forcechecking boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_bandmetadata(rast spatial.raster, band integer[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandmetadata(rast spatial.raster, band integer[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandmetadata(rast spatial.raster, band integer[]) TO yg_reader_group;


--
-- Name: FUNCTION st_bandmetadata(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandmetadata(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandmetadata(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bandnodatavalue(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandnodatavalue(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandnodatavalue(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bandpath(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandpath(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandpath(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bandpixeltype(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bandpixeltype(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bandpixeltype(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bdmpolyfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bdmpolyfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bdmpolyfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_bdpolyfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_bdpolyfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_bdpolyfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_boundary(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_boundary(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_boundary(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_boundingdiagonal(geom spatial.geometry, fits boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_boundingdiagonal(geom spatial.geometry, fits boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_boundingdiagonal(geom spatial.geometry, fits boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_box2dfromgeohash(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_box2dfromgeohash(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_box2dfromgeohash(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(text, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(text, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(text, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(spatial.geography, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(spatial.geography, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(spatial.geography, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(text, double precision, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(text, double precision, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(text, double precision, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(text, double precision, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(text, double precision, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(text, double precision, text) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(spatial.geography, double precision, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(spatial.geography, double precision, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(spatial.geography, double precision, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(spatial.geography, double precision, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(spatial.geography, double precision, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(spatial.geography, double precision, text) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(geom spatial.geometry, radius double precision, quadsegs integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(geom spatial.geometry, radius double precision, quadsegs integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(geom spatial.geometry, radius double precision, quadsegs integer) TO yg_reader_group;


--
-- Name: FUNCTION st_buffer(geom spatial.geometry, radius double precision, options text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buffer(geom spatial.geometry, radius double precision, options text) TO admin;
GRANT ALL ON FUNCTION spatial.st_buffer(geom spatial.geometry, radius double precision, options text) TO yg_reader_group;


--
-- Name: FUNCTION st_buildarea(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_buildarea(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_buildarea(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_centroid(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_centroid(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_centroid(text) TO yg_reader_group;


--
-- Name: FUNCTION st_centroid(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_centroid(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_centroid(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_centroid(spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_centroid(spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_centroid(spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_chaikinsmoothing(spatial.geometry, integer, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_chaikinsmoothing(spatial.geometry, integer, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_chaikinsmoothing(spatial.geometry, integer, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_cleangeometry(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_cleangeometry(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_cleangeometry(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_clip(rast spatial.raster, geom spatial.geometry, crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, geom spatial.geometry, crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, geom spatial.geometry, crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_clip(rast spatial.raster, nband integer, geom spatial.geometry, crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, nband integer, geom spatial.geometry, crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, nband integer, geom spatial.geometry, crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_clip(rast spatial.raster, geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_clip(rast spatial.raster, geom spatial.geometry, nodataval double precision, crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, geom spatial.geometry, nodataval double precision, crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, geom spatial.geometry, nodataval double precision, crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_clip(rast spatial.raster, nband integer[], geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, nband integer[], geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, nband integer[], geom spatial.geometry, nodataval double precision[], crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_clip(rast spatial.raster, nband integer, geom spatial.geometry, nodataval double precision, crop boolean, touched boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, nband integer, geom spatial.geometry, nodataval double precision, crop boolean, touched boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_clip(rast spatial.raster, nband integer, geom spatial.geometry, nodataval double precision, crop boolean, touched boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_clipbybox2d(geom spatial.geometry, box spatial.box2d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clipbybox2d(geom spatial.geometry, box spatial.box2d) TO admin;
GRANT ALL ON FUNCTION spatial.st_clipbybox2d(geom spatial.geometry, box spatial.box2d) TO yg_reader_group;


--
-- Name: FUNCTION st_closestpoint(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_closestpoint(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_closestpoint(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_closestpoint(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_closestpoint(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_closestpoint(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_closestpoint(spatial.geography, spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_closestpoint(spatial.geography, spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_closestpoint(spatial.geography, spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_closestpointofapproach(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_closestpointofapproach(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_closestpointofapproach(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterdbscan(spatial.geometry, eps double precision, minpoints integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterdbscan(spatial.geometry, eps double precision, minpoints integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterdbscan(spatial.geometry, eps double precision, minpoints integer) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterintersecting(spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterintersecting(spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterintersecting(spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterintersectingwin(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterintersectingwin(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterintersectingwin(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterkmeans(geom spatial.geometry, k integer, max_radius double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterkmeans(geom spatial.geometry, k integer, max_radius double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterkmeans(geom spatial.geometry, k integer, max_radius double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterwithin(spatial.geometry[], double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterwithin(spatial.geometry[], double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterwithin(spatial.geometry[], double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterwithinwin(spatial.geometry, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterwithinwin(spatial.geometry, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterwithinwin(spatial.geometry, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_collect(spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_collect(spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_collect(spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_collect(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_collect(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_collect(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_collectionextract(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_collectionextract(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_collectionextract(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_collectionextract(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_collectionextract(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_collectionextract(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_collectionhomogenize(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_collectionhomogenize(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_collectionhomogenize(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_colormap(rast spatial.raster, colormap text, method text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_colormap(rast spatial.raster, colormap text, method text) TO admin;
GRANT ALL ON FUNCTION spatial.st_colormap(rast spatial.raster, colormap text, method text) TO yg_reader_group;


--
-- Name: FUNCTION st_colormap(rast spatial.raster, nband integer, colormap text, method text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_colormap(rast spatial.raster, nband integer, colormap text, method text) TO admin;
GRANT ALL ON FUNCTION spatial.st_colormap(rast spatial.raster, nband integer, colormap text, method text) TO yg_reader_group;


--
-- Name: FUNCTION st_combinebbox(spatial.box2d, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_combinebbox(spatial.box2d, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_combinebbox(spatial.box2d, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_combinebbox(spatial.box3d, spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_combinebbox(spatial.box3d, spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_combinebbox(spatial.box3d, spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION st_combinebbox(spatial.box3d, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_combinebbox(spatial.box3d, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_combinebbox(spatial.box3d, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_concavehull(param_geom spatial.geometry, param_pctconvex double precision, param_allow_holes boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_concavehull(param_geom spatial.geometry, param_pctconvex double precision, param_allow_holes boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_concavehull(param_geom spatial.geometry, param_pctconvex double precision, param_allow_holes boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_contains(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_contains(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_contains(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_contains(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_contains(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_contains(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_contains(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_contains(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_contains(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_containsproperly(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_containsproperly(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_containsproperly(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_containsproperly(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_containsproperly(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_containsproperly(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_containsproperly(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_containsproperly(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_containsproperly(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_contour(rast spatial.raster, bandnumber integer, level_interval double precision, level_base double precision, fixed_levels double precision[], polygonize boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_contour(rast spatial.raster, bandnumber integer, level_interval double precision, level_base double precision, fixed_levels double precision[], polygonize boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_contour(rast spatial.raster, bandnumber integer, level_interval double precision, level_base double precision, fixed_levels double precision[], polygonize boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_convexhull(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_convexhull(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_convexhull(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_coorddim(geometry spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coorddim(geometry spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_coorddim(geometry spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_count(rast spatial.raster, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_count(rast spatial.raster, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_count(rast spatial.raster, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_count(rast spatial.raster, nband integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_count(rast spatial.raster, nband integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_count(rast spatial.raster, nband integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_coverageinvalidedges(geom spatial.geometry, tolerance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coverageinvalidedges(geom spatial.geometry, tolerance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_coverageinvalidedges(geom spatial.geometry, tolerance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_coveragesimplify(geom spatial.geometry, tolerance double precision, simplifyboundary boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coveragesimplify(geom spatial.geometry, tolerance double precision, simplifyboundary boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_coveragesimplify(geom spatial.geometry, tolerance double precision, simplifyboundary boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_coverageunion(spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coverageunion(spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_coverageunion(spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_coveredby(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coveredby(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_coveredby(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_coveredby(geog1 spatial.geography, geog2 spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coveredby(geog1 spatial.geography, geog2 spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_coveredby(geog1 spatial.geography, geog2 spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_coveredby(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coveredby(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_coveredby(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_coveredby(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coveredby(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_coveredby(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_coveredby(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coveredby(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_coveredby(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_covers(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_covers(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_covers(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_covers(geog1 spatial.geography, geog2 spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_covers(geog1 spatial.geography, geog2 spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_covers(geog1 spatial.geography, geog2 spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_covers(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_covers(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_covers(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_covers(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_covers(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_covers(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_covers(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_covers(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_covers(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_cpawithin(spatial.geometry, spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_cpawithin(spatial.geometry, spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_cpawithin(spatial.geometry, spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_createoverview(tab regclass, col name, factor integer, algo text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_createoverview(tab regclass, col name, factor integer, algo text) TO admin;
GRANT ALL ON FUNCTION spatial.st_createoverview(tab regclass, col name, factor integer, algo text) TO yg_reader_group;


--
-- Name: FUNCTION st_crosses(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_crosses(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_crosses(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_curven(geometry spatial.geometry, i integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_curven(geometry spatial.geometry, i integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_curven(geometry spatial.geometry, i integer) TO yg_reader_group;


--
-- Name: FUNCTION st_curvetoline(geom spatial.geometry, tol double precision, toltype integer, flags integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_curvetoline(geom spatial.geometry, tol double precision, toltype integer, flags integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_curvetoline(geom spatial.geometry, tol double precision, toltype integer, flags integer) TO yg_reader_group;


--
-- Name: FUNCTION st_delaunaytriangles(g1 spatial.geometry, tolerance double precision, flags integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_delaunaytriangles(g1 spatial.geometry, tolerance double precision, flags integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_delaunaytriangles(g1 spatial.geometry, tolerance double precision, flags integer) TO yg_reader_group;


--
-- Name: FUNCTION st_dfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dfullywithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_dfullywithin(rast1 spatial.raster, rast2 spatial.raster, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dfullywithin(rast1 spatial.raster, rast2 spatial.raster, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dfullywithin(rast1 spatial.raster, rast2 spatial.raster, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_dfullywithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dfullywithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dfullywithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_difference(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_difference(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_difference(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_dimension(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dimension(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_dimension(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_disjoint(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_disjoint(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_disjoint(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_disjoint(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_disjoint(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_disjoint(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_disjoint(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_disjoint(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_disjoint(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_distance(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distance(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_distance(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_distance(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distance(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_distance(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_distance(geog1 spatial.geography, geog2 spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distance(geog1 spatial.geography, geog2 spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_distance(geog1 spatial.geography, geog2 spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_distancecpa(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distancecpa(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_distancecpa(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_distancesphere(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distancesphere(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_distancesphere(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_distancesphere(geom1 spatial.geometry, geom2 spatial.geometry, radius double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distancesphere(geom1 spatial.geometry, geom2 spatial.geometry, radius double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_distancesphere(geom1 spatial.geometry, geom2 spatial.geometry, radius double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_distancespheroid(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distancespheroid(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_distancespheroid(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_distancespheroid(geom1 spatial.geometry, geom2 spatial.geometry, spatial.spheroid); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distancespheroid(geom1 spatial.geometry, geom2 spatial.geometry, spatial.spheroid) TO admin;
GRANT ALL ON FUNCTION spatial.st_distancespheroid(geom1 spatial.geometry, geom2 spatial.geometry, spatial.spheroid) TO yg_reader_group;


--
-- Name: FUNCTION st_distinct4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distinct4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_distinct4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_distinct4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_distinct4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_distinct4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_dump(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dump(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_dump(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_dumpaspolygons(rast spatial.raster, band integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dumpaspolygons(rast spatial.raster, band integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_dumpaspolygons(rast spatial.raster, band integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_dumppoints(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dumppoints(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_dumppoints(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_dumprings(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dumprings(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_dumprings(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_dumpsegments(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dumpsegments(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_dumpsegments(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_dumpvalues(rast spatial.raster, nband integer[], exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dumpvalues(rast spatial.raster, nband integer[], exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_dumpvalues(rast spatial.raster, nband integer[], exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_dumpvalues(rast spatial.raster, nband integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dumpvalues(rast spatial.raster, nband integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_dumpvalues(rast spatial.raster, nband integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_dwithin(text, text, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dwithin(text, text, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dwithin(text, text, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_dwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dwithin(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_dwithin(rast1 spatial.raster, rast2 spatial.raster, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dwithin(rast1 spatial.raster, rast2 spatial.raster, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dwithin(rast1 spatial.raster, rast2 spatial.raster, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_dwithin(geog1 spatial.geography, geog2 spatial.geography, tolerance double precision, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dwithin(geog1 spatial.geography, geog2 spatial.geography, tolerance double precision, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_dwithin(geog1 spatial.geography, geog2 spatial.geography, tolerance double precision, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_dwithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_dwithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_dwithin(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_endpoint(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_endpoint(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_endpoint(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_envelope(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_envelope(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_envelope(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_envelope(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_envelope(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_envelope(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_equals(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_equals(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_equals(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_estimatedextent(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_estimatedextent(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_estimatedextent(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_estimatedextent(text, text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_estimatedextent(text, text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_estimatedextent(text, text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_estimatedextent(text, text, text, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_estimatedextent(text, text, text, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_estimatedextent(text, text, text, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_expand(spatial.box2d, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_expand(spatial.box2d, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_expand(spatial.box2d, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_expand(spatial.box3d, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_expand(spatial.box3d, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_expand(spatial.box3d, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_expand(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_expand(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_expand(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_expand(box spatial.box2d, dx double precision, dy double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_expand(box spatial.box2d, dx double precision, dy double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_expand(box spatial.box2d, dx double precision, dy double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_expand(box spatial.box3d, dx double precision, dy double precision, dz double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_expand(box spatial.box3d, dx double precision, dy double precision, dz double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_expand(box spatial.box3d, dx double precision, dy double precision, dz double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_expand(geom spatial.geometry, dx double precision, dy double precision, dz double precision, dm double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_expand(geom spatial.geometry, dx double precision, dy double precision, dz double precision, dm double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_expand(geom spatial.geometry, dx double precision, dy double precision, dz double precision, dm double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_exteriorring(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_exteriorring(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_exteriorring(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_filterbym(spatial.geometry, double precision, double precision, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_filterbym(spatial.geometry, double precision, double precision, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_filterbym(spatial.geometry, double precision, double precision, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_findextent(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_findextent(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_findextent(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_findextent(text, text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_findextent(text, text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_findextent(text, text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_flipcoordinates(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_flipcoordinates(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_flipcoordinates(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_force2d(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_force2d(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_force2d(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_force3d(geom spatial.geometry, zvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_force3d(geom spatial.geometry, zvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_force3d(geom spatial.geometry, zvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_force3dm(geom spatial.geometry, mvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_force3dm(geom spatial.geometry, mvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_force3dm(geom spatial.geometry, mvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_force3dz(geom spatial.geometry, zvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_force3dz(geom spatial.geometry, zvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_force3dz(geom spatial.geometry, zvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_force4d(geom spatial.geometry, zvalue double precision, mvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_force4d(geom spatial.geometry, zvalue double precision, mvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_force4d(geom spatial.geometry, zvalue double precision, mvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_forcecollection(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcecollection(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcecollection(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_forcecurve(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcecurve(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcecurve(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_forcepolygonccw(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcepolygonccw(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcepolygonccw(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_forcepolygoncw(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcepolygoncw(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcepolygoncw(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_forcerhr(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcerhr(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcerhr(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_forcesfs(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcesfs(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcesfs(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_forcesfs(spatial.geometry, version text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_forcesfs(spatial.geometry, version text) TO admin;
GRANT ALL ON FUNCTION spatial.st_forcesfs(spatial.geometry, version text) TO yg_reader_group;


--
-- Name: FUNCTION st_frechetdistance(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_frechetdistance(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_frechetdistance(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_fromflatgeobuf(anyelement, bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_fromflatgeobuf(anyelement, bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_fromflatgeobuf(anyelement, bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_fromflatgeobuftotable(text, text, bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_fromflatgeobuftotable(text, text, bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_fromflatgeobuftotable(text, text, bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_fromgdalraster(gdaldata bytea, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_fromgdalraster(gdaldata bytea, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_fromgdalraster(gdaldata bytea, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_gdaldrivers(OUT idx integer, OUT short_name text, OUT long_name text, OUT can_read boolean, OUT can_write boolean, OUT create_options text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_gdaldrivers(OUT idx integer, OUT short_name text, OUT long_name text, OUT can_read boolean, OUT can_write boolean, OUT create_options text) TO admin;
GRANT ALL ON FUNCTION spatial.st_gdaldrivers(OUT idx integer, OUT short_name text, OUT long_name text, OUT can_read boolean, OUT can_write boolean, OUT create_options text) TO yg_reader_group;


--
-- Name: FUNCTION st_generatepoints(area spatial.geometry, npoints integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_generatepoints(area spatial.geometry, npoints integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_generatepoints(area spatial.geometry, npoints integer) TO yg_reader_group;


--
-- Name: FUNCTION st_generatepoints(area spatial.geometry, npoints integer, seed integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_generatepoints(area spatial.geometry, npoints integer, seed integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_generatepoints(area spatial.geometry, npoints integer, seed integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geogfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geogfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geogfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geogfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geogfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_geogfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_geographyfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geographyfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geographyfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geohash(geog spatial.geography, maxchars integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geohash(geog spatial.geography, maxchars integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geohash(geog spatial.geography, maxchars integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geohash(geom spatial.geometry, maxchars integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geohash(geom spatial.geometry, maxchars integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geohash(geom spatial.geometry, maxchars integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geomcollfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomcollfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomcollfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomcollfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomcollfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomcollfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geomcollfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomcollfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomcollfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_geomcollfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomcollfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomcollfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geometricmedian(g spatial.geometry, tolerance double precision, max_iter integer, fail_if_not_converged boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geometricmedian(g spatial.geometry, tolerance double precision, max_iter integer, fail_if_not_converged boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_geometricmedian(g spatial.geometry, tolerance double precision, max_iter integer, fail_if_not_converged boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_geometryfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geometryfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geometryfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geometryfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geometryfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geometryfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geometryn(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geometryn(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geometryn(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geometrytype(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geometrytype(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_geometrytype(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromewkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromewkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromewkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromewkt(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromewkt(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromewkt(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromgeohash(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromgeohash(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromgeohash(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromgeojson(json); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromgeojson(json) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromgeojson(json) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromgeojson(jsonb); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromgeojson(jsonb) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromgeojson(jsonb) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromgeojson(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromgeojson(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromgeojson(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromgml(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromgml(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromgml(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromgml(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromgml(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromgml(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromkml(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromkml(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromkml(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfrommarc21(marc21xml text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfrommarc21(marc21xml text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfrommarc21(marc21xml text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromtwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromtwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromtwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_geomfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geomfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_geomfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_georeference(rast spatial.raster, format text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_georeference(rast spatial.raster, format text) TO admin;
GRANT ALL ON FUNCTION spatial.st_georeference(rast spatial.raster, format text) TO yg_reader_group;


--
-- Name: FUNCTION st_geotransform(spatial.raster, OUT imag double precision, OUT jmag double precision, OUT theta_i double precision, OUT theta_ij double precision, OUT xoffset double precision, OUT yoffset double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_geotransform(spatial.raster, OUT imag double precision, OUT jmag double precision, OUT theta_i double precision, OUT theta_ij double precision, OUT xoffset double precision, OUT yoffset double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_geotransform(spatial.raster, OUT imag double precision, OUT jmag double precision, OUT theta_i double precision, OUT theta_ij double precision, OUT xoffset double precision, OUT yoffset double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_gmltosql(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_gmltosql(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_gmltosql(text) TO yg_reader_group;


--
-- Name: FUNCTION st_gmltosql(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_gmltosql(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_gmltosql(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_grayscale(rastbandargset spatial.rastbandarg[], extenttype text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_grayscale(rastbandargset spatial.rastbandarg[], extenttype text) TO admin;
GRANT ALL ON FUNCTION spatial.st_grayscale(rastbandargset spatial.rastbandarg[], extenttype text) TO yg_reader_group;


--
-- Name: FUNCTION st_grayscale(rast spatial.raster, redband integer, greenband integer, blueband integer, extenttype text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_grayscale(rast spatial.raster, redband integer, greenband integer, blueband integer, extenttype text) TO admin;
GRANT ALL ON FUNCTION spatial.st_grayscale(rast spatial.raster, redband integer, greenband integer, blueband integer, extenttype text) TO yg_reader_group;


--
-- Name: FUNCTION st_hasarc(geometry spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hasarc(geometry spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_hasarc(geometry spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_hasm(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hasm(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_hasm(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_hasnoband(rast spatial.raster, nband integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hasnoband(rast spatial.raster, nband integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_hasnoband(rast spatial.raster, nband integer) TO yg_reader_group;


--
-- Name: FUNCTION st_hasz(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hasz(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_hasz(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_hausdorffdistance(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hausdorffdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_hausdorffdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_hausdorffdistance(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hausdorffdistance(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_hausdorffdistance(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_height(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_height(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_height(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_hexagon(size double precision, cell_i integer, cell_j integer, origin spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hexagon(size double precision, cell_i integer, cell_j integer, origin spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_hexagon(size double precision, cell_i integer, cell_j integer, origin spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_hexagongrid(size double precision, bounds spatial.geometry, OUT geom spatial.geometry, OUT i integer, OUT j integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hexagongrid(size double precision, bounds spatial.geometry, OUT geom spatial.geometry, OUT i integer, OUT j integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_hexagongrid(size double precision, bounds spatial.geometry, OUT geom spatial.geometry, OUT i integer, OUT j integer) TO yg_reader_group;


--
-- Name: FUNCTION st_hillshade(rast spatial.raster, nband integer, pixeltype text, azimuth double precision, altitude double precision, max_bright double precision, scale double precision, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hillshade(rast spatial.raster, nband integer, pixeltype text, azimuth double precision, altitude double precision, max_bright double precision, scale double precision, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_hillshade(rast spatial.raster, nband integer, pixeltype text, azimuth double precision, altitude double precision, max_bright double precision, scale double precision, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_hillshade(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, azimuth double precision, altitude double precision, max_bright double precision, scale double precision, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_hillshade(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, azimuth double precision, altitude double precision, max_bright double precision, scale double precision, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_hillshade(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, azimuth double precision, altitude double precision, max_bright double precision, scale double precision, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_histogram(rast spatial.raster, nband integer, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, bins integer, "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_histogram(rast spatial.raster, nband integer, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_histogram(rast spatial.raster, nband integer, exclude_nodata_value boolean, bins integer, width double precision[], "right" boolean, OUT min double precision, OUT max double precision, OUT count bigint, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_interiorringn(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_interiorringn(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_interiorringn(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_interpolatepoint(line spatial.geometry, point spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_interpolatepoint(line spatial.geometry, point spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_interpolatepoint(line spatial.geometry, point spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_interpolateraster(geom spatial.geometry, options text, rast spatial.raster, bandnumber integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_interpolateraster(geom spatial.geometry, options text, rast spatial.raster, bandnumber integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_interpolateraster(geom spatial.geometry, options text, rast spatial.raster, bandnumber integer) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(spatial.geography, spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(spatial.geography, spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(spatial.geography, spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast spatial.raster, geomin spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast spatial.raster, geomin spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast spatial.raster, geomin spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(geomin spatial.geometry, rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(geomin spatial.geometry, rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(geomin spatial.geometry, rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast spatial.raster, band integer, geomin spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast spatial.raster, band integer, geomin spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast spatial.raster, band integer, geomin spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, rast2 spatial.raster, nodataval double precision[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, nodataval double precision[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, nodataval double precision[]) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, rast2 spatial.raster, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, rast2 spatial.raster, returnband text, nodataval double precision[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, returnband text, nodataval double precision[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, returnband text, nodataval double precision[]) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, rast2 spatial.raster, returnband text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, returnband text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, rast2 spatial.raster, returnband text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, nodataval double precision[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, nodataval double precision[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, nodataval double precision[]) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, returnband text, nodataval double precision[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, returnband text, nodataval double precision[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, returnband text, nodataval double precision[]) TO yg_reader_group;


--
-- Name: FUNCTION st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, returnband text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, returnband text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersection(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, returnband text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(geog1 spatial.geography, geog2 spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(geog1 spatial.geography, geog2 spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(geog1 spatial.geography, geog2 spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(geom spatial.geometry, rast spatial.raster, nband integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(geom spatial.geometry, rast spatial.raster, nband integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(geom spatial.geometry, rast spatial.raster, nband integer) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(rast spatial.raster, nband integer, geom spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(rast spatial.raster, nband integer, geom spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(rast spatial.raster, nband integer, geom spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(rast spatial.raster, geom spatial.geometry, nband integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(rast spatial.raster, geom spatial.geometry, nband integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(rast spatial.raster, geom spatial.geometry, nband integer) TO yg_reader_group;


--
-- Name: FUNCTION st_intersects(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_intersects(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_intersects(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_invdistweight4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_invdistweight4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_invdistweight4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_inversetransformpipeline(geom spatial.geometry, pipeline text, to_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_inversetransformpipeline(geom spatial.geometry, pipeline text, to_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_inversetransformpipeline(geom spatial.geometry, pipeline text, to_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_isclosed(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isclosed(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_isclosed(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_iscollection(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_iscollection(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_iscollection(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_iscoveragetile(rast spatial.raster, coverage spatial.raster, tilewidth integer, tileheight integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_iscoveragetile(rast spatial.raster, coverage spatial.raster, tilewidth integer, tileheight integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_iscoveragetile(rast spatial.raster, coverage spatial.raster, tilewidth integer, tileheight integer) TO yg_reader_group;


--
-- Name: FUNCTION st_isempty(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isempty(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_isempty(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_isempty(rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isempty(rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_isempty(rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_ispolygonccw(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ispolygonccw(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_ispolygonccw(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_ispolygoncw(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ispolygoncw(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_ispolygoncw(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_isring(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isring(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_isring(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_issimple(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_issimple(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_issimple(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_isvalid(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isvalid(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_isvalid(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_isvalid(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isvalid(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_isvalid(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_isvaliddetail(geom spatial.geometry, flags integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isvaliddetail(geom spatial.geometry, flags integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_isvaliddetail(geom spatial.geometry, flags integer) TO yg_reader_group;


--
-- Name: FUNCTION st_isvalidreason(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isvalidreason(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_isvalidreason(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_isvalidreason(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isvalidreason(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_isvalidreason(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_isvalidtrajectory(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_isvalidtrajectory(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_isvalidtrajectory(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_largestemptycircle(geom spatial.geometry, tolerance double precision, boundary spatial.geometry, OUT center spatial.geometry, OUT nearest spatial.geometry, OUT radius double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_largestemptycircle(geom spatial.geometry, tolerance double precision, boundary spatial.geometry, OUT center spatial.geometry, OUT nearest spatial.geometry, OUT radius double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_largestemptycircle(geom spatial.geometry, tolerance double precision, boundary spatial.geometry, OUT center spatial.geometry, OUT nearest spatial.geometry, OUT radius double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_length(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_length(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_length(text) TO yg_reader_group;


--
-- Name: FUNCTION st_length(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_length(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_length(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_length(geog spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_length(geog spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_length(geog spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_length2d(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_length2d(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_length2d(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_length2dspheroid(spatial.geometry, spatial.spheroid); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_length2dspheroid(spatial.geometry, spatial.spheroid) TO admin;
GRANT ALL ON FUNCTION spatial.st_length2dspheroid(spatial.geometry, spatial.spheroid) TO yg_reader_group;


--
-- Name: FUNCTION st_lengthspheroid(spatial.geometry, spatial.spheroid); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lengthspheroid(spatial.geometry, spatial.spheroid) TO admin;
GRANT ALL ON FUNCTION spatial.st_lengthspheroid(spatial.geometry, spatial.spheroid) TO yg_reader_group;


--
-- Name: FUNCTION st_letters(letters text, font json); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_letters(letters text, font json) TO admin;
GRANT ALL ON FUNCTION spatial.st_letters(letters text, font json) TO yg_reader_group;


--
-- Name: FUNCTION st_linecrossingdirection(line1 spatial.geometry, line2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linecrossingdirection(line1 spatial.geometry, line2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_linecrossingdirection(line1 spatial.geometry, line2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_lineextend(geom spatial.geometry, distance_forward double precision, distance_backward double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineextend(geom spatial.geometry, distance_forward double precision, distance_backward double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineextend(geom spatial.geometry, distance_forward double precision, distance_backward double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_linefromencodedpolyline(txtin text, nprecision integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linefromencodedpolyline(txtin text, nprecision integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_linefromencodedpolyline(txtin text, nprecision integer) TO yg_reader_group;


--
-- Name: FUNCTION st_linefrommultipoint(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linefrommultipoint(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_linefrommultipoint(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_linefromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linefromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_linefromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_linefromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linefromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_linefromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_linefromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linefromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_linefromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_linefromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linefromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_linefromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_lineinterpolatepoint(text, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoint(text, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoint(text, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_lineinterpolatepoint(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoint(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoint(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_lineinterpolatepoint(spatial.geography, double precision, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoint(spatial.geography, double precision, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoint(spatial.geography, double precision, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_lineinterpolatepoints(text, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoints(text, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoints(text, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_lineinterpolatepoints(spatial.geometry, double precision, repeat boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoints(spatial.geometry, double precision, repeat boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoints(spatial.geometry, double precision, repeat boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_lineinterpolatepoints(spatial.geography, double precision, use_spheroid boolean, repeat boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoints(spatial.geography, double precision, use_spheroid boolean, repeat boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_lineinterpolatepoints(spatial.geography, double precision, use_spheroid boolean, repeat boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_linelocatepoint(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linelocatepoint(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_linelocatepoint(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_linelocatepoint(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linelocatepoint(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_linelocatepoint(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_linelocatepoint(spatial.geography, spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linelocatepoint(spatial.geography, spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_linelocatepoint(spatial.geography, spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_linemerge(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linemerge(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_linemerge(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_linemerge(spatial.geometry, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linemerge(spatial.geometry, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_linemerge(spatial.geometry, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_linestringfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linestringfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_linestringfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_linestringfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linestringfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_linestringfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_linesubstring(text, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linesubstring(text, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_linesubstring(text, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_linesubstring(spatial.geography, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linesubstring(spatial.geography, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_linesubstring(spatial.geography, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_linesubstring(spatial.geometry, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linesubstring(spatial.geometry, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_linesubstring(spatial.geometry, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_linetocurve(geometry spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_linetocurve(geometry spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_linetocurve(geometry spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_locatealong(geometry spatial.geometry, measure double precision, leftrightoffset double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_locatealong(geometry spatial.geometry, measure double precision, leftrightoffset double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_locatealong(geometry spatial.geometry, measure double precision, leftrightoffset double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_locatebetween(geometry spatial.geometry, frommeasure double precision, tomeasure double precision, leftrightoffset double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_locatebetween(geometry spatial.geometry, frommeasure double precision, tomeasure double precision, leftrightoffset double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_locatebetween(geometry spatial.geometry, frommeasure double precision, tomeasure double precision, leftrightoffset double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_locatebetweenelevations(geometry spatial.geometry, fromelevation double precision, toelevation double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_locatebetweenelevations(geometry spatial.geometry, fromelevation double precision, toelevation double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_locatebetweenelevations(geometry spatial.geometry, fromelevation double precision, toelevation double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_longestline(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_longestline(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_longestline(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_m(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_m(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_m(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_makebox2d(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makebox2d(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_makebox2d(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_makeemptycoverage(tilewidth integer, tileheight integer, width integer, height integer, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeemptycoverage(tilewidth integer, tileheight integer, width integer, height integer, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeemptycoverage(tilewidth integer, tileheight integer, width integer, height integer, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_makeemptyraster(rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeemptyraster(rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeemptyraster(rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_makeemptyraster(width integer, height integer, upperleftx double precision, upperlefty double precision, pixelsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeemptyraster(width integer, height integer, upperleftx double precision, upperlefty double precision, pixelsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeemptyraster(width integer, height integer, upperleftx double precision, upperlefty double precision, pixelsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_makeemptyraster(width integer, height integer, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeemptyraster(width integer, height integer, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeemptyraster(width integer, height integer, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_makeenvelope(double precision, double precision, double precision, double precision, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeenvelope(double precision, double precision, double precision, double precision, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeenvelope(double precision, double precision, double precision, double precision, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_makeline(spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeline(spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeline(spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_makeline(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeline(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeline(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_makepoint(double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makepoint(double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_makepoint(double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_makepoint(double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makepoint(double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_makepoint(double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_makepoint(double precision, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makepoint(double precision, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_makepoint(double precision, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_makepointm(double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makepointm(double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_makepointm(double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_makepolygon(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makepolygon(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_makepolygon(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_makepolygon(spatial.geometry, spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makepolygon(spatial.geometry, spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_makepolygon(spatial.geometry, spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_makevalid(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makevalid(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_makevalid(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_makevalid(geom spatial.geometry, params text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makevalid(geom spatial.geometry, params text) TO admin;
GRANT ALL ON FUNCTION spatial.st_makevalid(geom spatial.geometry, params text) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast spatial.raster, pixeltype text, expression text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, pixeltype text, expression text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, pixeltype text, expression text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast spatial.raster, nband integer, pixeltype text, expression text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer, pixeltype text, expression text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer, pixeltype text, expression text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rastbandargset spatial.rastbandarg[], callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rastbandargset spatial.rastbandarg[], callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rastbandargset spatial.rastbandarg[], callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast1 spatial.raster, rast2 spatial.raster, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast1 spatial.raster, rast2 spatial.raster, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast1 spatial.raster, rast2 spatial.raster, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast spatial.raster, nband integer[], callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer[], callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer[], callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast spatial.raster, nband integer, callbackfunc regprocedure, mask double precision[], weighted boolean, pixeltype text, extenttype text, customextent spatial.raster, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer, callbackfunc regprocedure, mask double precision[], weighted boolean, pixeltype text, extenttype text, customextent spatial.raster, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer, callbackfunc regprocedure, mask double precision[], weighted boolean, pixeltype text, extenttype text, customextent spatial.raster, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast spatial.raster, nband integer, callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer, callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast spatial.raster, nband integer, callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebra(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebra(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer, callbackfunc regprocedure, pixeltype text, extenttype text, customextent spatial.raster, distancex integer, distancey integer, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebraexpr(rast spatial.raster, pixeltype text, expression text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast spatial.raster, pixeltype text, expression text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast spatial.raster, pixeltype text, expression text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebraexpr(rast spatial.raster, band integer, pixeltype text, expression text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast spatial.raster, band integer, pixeltype text, expression text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast spatial.raster, band integer, pixeltype text, expression text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebraexpr(rast1 spatial.raster, rast2 spatial.raster, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast1 spatial.raster, rast2 spatial.raster, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast1 spatial.raster, rast2 spatial.raster, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebraexpr(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebraexpr(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, expression text, pixeltype text, extenttype text, nodata1expr text, nodata2expr text, nodatanodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, onerastuserfunc regprocedure); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, onerastuserfunc regprocedure) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, onerastuserfunc regprocedure) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, band integer, onerastuserfunc regprocedure); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, onerastuserfunc regprocedure) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, onerastuserfunc regprocedure) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, onerastuserfunc regprocedure, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, onerastuserfunc regprocedure, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, onerastuserfunc regprocedure, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, pixeltype text, onerastuserfunc regprocedure); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, pixeltype text, onerastuserfunc regprocedure) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, pixeltype text, onerastuserfunc regprocedure) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, band integer, onerastuserfunc regprocedure, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, onerastuserfunc regprocedure, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, onerastuserfunc regprocedure, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, band integer, pixeltype text, onerastuserfunc regprocedure); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, pixeltype text, onerastuserfunc regprocedure) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, pixeltype text, onerastuserfunc regprocedure) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, pixeltype text, onerastuserfunc regprocedure, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, pixeltype text, onerastuserfunc regprocedure, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, pixeltype text, onerastuserfunc regprocedure, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast spatial.raster, band integer, pixeltype text, onerastuserfunc regprocedure, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, pixeltype text, onerastuserfunc regprocedure, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast spatial.raster, band integer, pixeltype text, onerastuserfunc regprocedure, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast1 spatial.raster, rast2 spatial.raster, tworastuserfunc regprocedure, pixeltype text, extenttype text, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast1 spatial.raster, rast2 spatial.raster, tworastuserfunc regprocedure, pixeltype text, extenttype text, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast1 spatial.raster, rast2 spatial.raster, tworastuserfunc regprocedure, pixeltype text, extenttype text, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafct(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, tworastuserfunc regprocedure, pixeltype text, extenttype text, VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, tworastuserfunc regprocedure, pixeltype text, extenttype text, VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafct(rast1 spatial.raster, band1 integer, rast2 spatial.raster, band2 integer, tworastuserfunc regprocedure, pixeltype text, extenttype text, VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mapalgebrafctngb(rast spatial.raster, band integer, pixeltype text, ngbwidth integer, ngbheight integer, onerastngbuserfunc regprocedure, nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mapalgebrafctngb(rast spatial.raster, band integer, pixeltype text, ngbwidth integer, ngbheight integer, onerastngbuserfunc regprocedure, nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mapalgebrafctngb(rast spatial.raster, band integer, pixeltype text, ngbwidth integer, ngbheight integer, onerastngbuserfunc regprocedure, nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_max4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_max4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_max4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_max4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_max4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_max4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_maxdistance(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_maxdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_maxdistance(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_maximuminscribedcircle(spatial.geometry, OUT center spatial.geometry, OUT nearest spatial.geometry, OUT radius double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_maximuminscribedcircle(spatial.geometry, OUT center spatial.geometry, OUT nearest spatial.geometry, OUT radius double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_maximuminscribedcircle(spatial.geometry, OUT center spatial.geometry, OUT nearest spatial.geometry, OUT radius double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_mean4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mean4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mean4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_mean4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mean4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mean4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_memsize(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_memsize(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_memsize(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_memsize(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_memsize(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_memsize(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_metadata(rast spatial.raster, OUT upperleftx double precision, OUT upperlefty double precision, OUT width integer, OUT height integer, OUT scalex double precision, OUT scaley double precision, OUT skewx double precision, OUT skewy double precision, OUT srid integer, OUT numbands integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_metadata(rast spatial.raster, OUT upperleftx double precision, OUT upperlefty double precision, OUT width integer, OUT height integer, OUT scalex double precision, OUT scaley double precision, OUT skewx double precision, OUT skewy double precision, OUT srid integer, OUT numbands integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_metadata(rast spatial.raster, OUT upperleftx double precision, OUT upperlefty double precision, OUT width integer, OUT height integer, OUT scalex double precision, OUT scaley double precision, OUT skewx double precision, OUT skewy double precision, OUT srid integer, OUT numbands integer) TO yg_reader_group;


--
-- Name: FUNCTION st_min4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_min4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_min4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_min4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_min4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_min4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_minconvexhull(rast spatial.raster, nband integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_minconvexhull(rast spatial.raster, nband integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_minconvexhull(rast spatial.raster, nband integer) TO yg_reader_group;


--
-- Name: FUNCTION st_mindist4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mindist4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_mindist4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_minimumboundingcircle(inputgeom spatial.geometry, segs_per_quarter integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_minimumboundingcircle(inputgeom spatial.geometry, segs_per_quarter integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_minimumboundingcircle(inputgeom spatial.geometry, segs_per_quarter integer) TO yg_reader_group;


--
-- Name: FUNCTION st_minimumboundingradius(spatial.geometry, OUT center spatial.geometry, OUT radius double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_minimumboundingradius(spatial.geometry, OUT center spatial.geometry, OUT radius double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_minimumboundingradius(spatial.geometry, OUT center spatial.geometry, OUT radius double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_minimumclearance(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_minimumclearance(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_minimumclearance(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_minimumclearanceline(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_minimumclearanceline(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_minimumclearanceline(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_minpossiblevalue(pixeltype text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_minpossiblevalue(pixeltype text) TO admin;
GRANT ALL ON FUNCTION spatial.st_minpossiblevalue(pixeltype text) TO yg_reader_group;


--
-- Name: FUNCTION st_mlinefromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mlinefromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_mlinefromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_mlinefromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mlinefromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_mlinefromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_mlinefromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mlinefromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_mlinefromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_mlinefromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mlinefromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_mlinefromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_mpointfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpointfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpointfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_mpointfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpointfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpointfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_mpointfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpointfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpointfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_mpointfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpointfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpointfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_mpolyfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpolyfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpolyfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_mpolyfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpolyfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpolyfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_mpolyfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpolyfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpolyfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_mpolyfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_mpolyfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_mpolyfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_multi(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multi(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_multi(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_multilinefromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multilinefromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_multilinefromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_multilinestringfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multilinestringfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_multilinestringfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_multilinestringfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multilinestringfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_multilinestringfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_multipointfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipointfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipointfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_multipointfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipointfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipointfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_multipointfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipointfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipointfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_multipolyfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipolyfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipolyfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_multipolyfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipolyfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipolyfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_multipolygonfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipolygonfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipolygonfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_multipolygonfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_multipolygonfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_multipolygonfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_ndims(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ndims(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_ndims(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_nearestvalue(rast spatial.raster, pt spatial.geometry, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, pt spatial.geometry, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, pt spatial.geometry, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_nearestvalue(rast spatial.raster, columnx integer, rowy integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, columnx integer, rowy integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, columnx integer, rowy integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_nearestvalue(rast spatial.raster, band integer, pt spatial.geometry, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, band integer, pt spatial.geometry, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, band integer, pt spatial.geometry, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_nearestvalue(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_nearestvalue(rast spatial.raster, band integer, columnx integer, rowy integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_neighborhood(rast spatial.raster, pt spatial.geometry, distancex integer, distancey integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, pt spatial.geometry, distancex integer, distancey integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, pt spatial.geometry, distancex integer, distancey integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_neighborhood(rast spatial.raster, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_neighborhood(rast spatial.raster, band integer, pt spatial.geometry, distancex integer, distancey integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, band integer, pt spatial.geometry, distancex integer, distancey integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, band integer, pt spatial.geometry, distancex integer, distancey integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_neighborhood(rast spatial.raster, band integer, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, band integer, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_neighborhood(rast spatial.raster, band integer, columnx integer, rowy integer, distancex integer, distancey integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_node(g spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_node(g spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_node(g spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_normalize(geom spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_normalize(geom spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_normalize(geom spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_notsamealignmentreason(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_notsamealignmentreason(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_notsamealignmentreason(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_npoints(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_npoints(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_npoints(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_nrings(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_nrings(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_nrings(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_numbands(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numbands(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_numbands(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_numcurves(geometry spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numcurves(geometry spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_numcurves(geometry spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_numgeometries(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numgeometries(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_numgeometries(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_numinteriorring(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numinteriorring(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_numinteriorring(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_numinteriorrings(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numinteriorrings(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_numinteriorrings(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_numpatches(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numpatches(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_numpatches(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_numpoints(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_numpoints(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_numpoints(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_offsetcurve(line spatial.geometry, distance double precision, params text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_offsetcurve(line spatial.geometry, distance double precision, params text) TO admin;
GRANT ALL ON FUNCTION spatial.st_offsetcurve(line spatial.geometry, distance double precision, params text) TO yg_reader_group;


--
-- Name: FUNCTION st_orderingequals(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_orderingequals(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_orderingequals(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_orientedenvelope(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_orientedenvelope(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_orientedenvelope(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_overlaps(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_overlaps(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_overlaps(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_overlaps(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_overlaps(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_overlaps(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_overlaps(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_overlaps(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_overlaps(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_patchn(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_patchn(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_patchn(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_perimeter(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_perimeter(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_perimeter(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_perimeter(geog spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_perimeter(geog spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_perimeter(geog spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_perimeter2d(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_perimeter2d(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_perimeter2d(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelascentroid(rast spatial.raster, x integer, y integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelascentroid(rast spatial.raster, x integer, y integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelascentroid(rast spatial.raster, x integer, y integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelascentroids(rast spatial.raster, band integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelascentroids(rast spatial.raster, band integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelascentroids(rast spatial.raster, band integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelaspoint(rast spatial.raster, x integer, y integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelaspoint(rast spatial.raster, x integer, y integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelaspoint(rast spatial.raster, x integer, y integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelaspoints(rast spatial.raster, band integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelaspoints(rast spatial.raster, band integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelaspoints(rast spatial.raster, band integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelaspolygon(rast spatial.raster, x integer, y integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelaspolygon(rast spatial.raster, x integer, y integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelaspolygon(rast spatial.raster, x integer, y integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelaspolygons(rast spatial.raster, band integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelaspolygons(rast spatial.raster, band integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelaspolygons(rast spatial.raster, band integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelheight(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelheight(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelheight(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelofvalue(rast spatial.raster, search double precision[], exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, search double precision[], exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, search double precision[], exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelofvalue(rast spatial.raster, search double precision, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, search double precision, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, search double precision, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelofvalue(rast spatial.raster, nband integer, search double precision[], exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, nband integer, search double precision[], exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, nband integer, search double precision[], exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelofvalue(rast spatial.raster, nband integer, search double precision, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, nband integer, search double precision, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelofvalue(rast spatial.raster, nband integer, search double precision, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_pixelwidth(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pixelwidth(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_pixelwidth(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_point(double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_point(double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_point(double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_point(double precision, double precision, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_point(double precision, double precision, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_point(double precision, double precision, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointfromgeohash(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointfromgeohash(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointfromgeohash(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_pointfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_pointfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointinsidecircle(spatial.geometry, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointinsidecircle(spatial.geometry, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointinsidecircle(spatial.geometry, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_pointm(xcoordinate double precision, ycoordinate double precision, mcoordinate double precision, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointm(xcoordinate double precision, ycoordinate double precision, mcoordinate double precision, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointm(xcoordinate double precision, ycoordinate double precision, mcoordinate double precision, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointn(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointn(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointn(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointonsurface(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointonsurface(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointonsurface(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_points(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_points(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_points(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_pointz(xcoordinate double precision, ycoordinate double precision, zcoordinate double precision, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointz(xcoordinate double precision, ycoordinate double precision, zcoordinate double precision, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointz(xcoordinate double precision, ycoordinate double precision, zcoordinate double precision, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_pointzm(xcoordinate double precision, ycoordinate double precision, zcoordinate double precision, mcoordinate double precision, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_pointzm(xcoordinate double precision, ycoordinate double precision, zcoordinate double precision, mcoordinate double precision, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_pointzm(xcoordinate double precision, ycoordinate double precision, zcoordinate double precision, mcoordinate double precision, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polyfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polyfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_polyfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_polyfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polyfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_polyfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polyfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polyfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_polyfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_polyfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polyfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_polyfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polygon(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygon(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygon(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polygon(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygon(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygon(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polygonfromtext(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygonfromtext(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygonfromtext(text) TO yg_reader_group;


--
-- Name: FUNCTION st_polygonfromtext(text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygonfromtext(text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygonfromtext(text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polygonfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygonfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygonfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_polygonfromwkb(bytea, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygonfromwkb(bytea, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygonfromwkb(bytea, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_polygonize(spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygonize(spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygonize(spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_project(geog spatial.geography, distance double precision, azimuth double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_project(geog spatial.geography, distance double precision, azimuth double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_project(geog spatial.geography, distance double precision, azimuth double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_project(geog_from spatial.geography, geog_to spatial.geography, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_project(geog_from spatial.geography, geog_to spatial.geography, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_project(geog_from spatial.geography, geog_to spatial.geography, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_project(geom1 spatial.geometry, distance double precision, azimuth double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_project(geom1 spatial.geometry, distance double precision, azimuth double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_project(geom1 spatial.geometry, distance double precision, azimuth double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_project(geom1 spatial.geometry, geom2 spatial.geometry, distance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_project(geom1 spatial.geometry, geom2 spatial.geometry, distance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_project(geom1 spatial.geometry, geom2 spatial.geometry, distance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, exclude_nodata_value boolean, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, exclude_nodata_value boolean, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, exclude_nodata_value boolean, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, nband integer, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, nband integer, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, quantiles double precision[], OUT quantile double precision, OUT value double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, quantiles double precision[], OUT quantile double precision, OUT value double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, quantile double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, quantile double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantile(rast spatial.raster, nband integer, exclude_nodata_value boolean, quantile double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_quantizecoordinates(g spatial.geometry, prec_x integer, prec_y integer, prec_z integer, prec_m integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_quantizecoordinates(g spatial.geometry, prec_x integer, prec_y integer, prec_z integer, prec_m integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_quantizecoordinates(g spatial.geometry, prec_x integer, prec_y integer, prec_z integer, prec_m integer) TO yg_reader_group;


--
-- Name: FUNCTION st_range4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_range4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_range4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_range4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_range4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_range4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_rastertoworldcoord(rast spatial.raster, columnx integer, rowy integer, OUT longitude double precision, OUT latitude double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastertoworldcoord(rast spatial.raster, columnx integer, rowy integer, OUT longitude double precision, OUT latitude double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastertoworldcoord(rast spatial.raster, columnx integer, rowy integer, OUT longitude double precision, OUT latitude double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rastertoworldcoordx(rast spatial.raster, xr integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordx(rast spatial.raster, xr integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordx(rast spatial.raster, xr integer) TO yg_reader_group;


--
-- Name: FUNCTION st_rastertoworldcoordx(rast spatial.raster, xr integer, yr integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordx(rast spatial.raster, xr integer, yr integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordx(rast spatial.raster, xr integer, yr integer) TO yg_reader_group;


--
-- Name: FUNCTION st_rastertoworldcoordy(rast spatial.raster, yr integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordy(rast spatial.raster, yr integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordy(rast spatial.raster, yr integer) TO yg_reader_group;


--
-- Name: FUNCTION st_rastertoworldcoordy(rast spatial.raster, xr integer, yr integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordy(rast spatial.raster, xr integer, yr integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastertoworldcoordy(rast spatial.raster, xr integer, yr integer) TO yg_reader_group;


--
-- Name: FUNCTION st_rastfromhexwkb(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastfromhexwkb(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastfromhexwkb(text) TO yg_reader_group;


--
-- Name: FUNCTION st_rastfromwkb(bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rastfromwkb(bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_rastfromwkb(bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_reclass(rast spatial.raster, VARIADIC reclassargset spatial.reclassarg[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reclass(rast spatial.raster, VARIADIC reclassargset spatial.reclassarg[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_reclass(rast spatial.raster, VARIADIC reclassargset spatial.reclassarg[]) TO yg_reader_group;


--
-- Name: FUNCTION st_reclass(rast spatial.raster, reclassexpr text, pixeltype text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reclass(rast spatial.raster, reclassexpr text, pixeltype text) TO admin;
GRANT ALL ON FUNCTION spatial.st_reclass(rast spatial.raster, reclassexpr text, pixeltype text) TO yg_reader_group;


--
-- Name: FUNCTION st_reclass(rast spatial.raster, nband integer, reclassexpr text, pixeltype text, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reclass(rast spatial.raster, nband integer, reclassexpr text, pixeltype text, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_reclass(rast spatial.raster, nband integer, reclassexpr text, pixeltype text, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_reduceprecision(geom spatial.geometry, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reduceprecision(geom spatial.geometry, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_reduceprecision(geom spatial.geometry, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_relate(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_relate(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_relate(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_relate(geom1 spatial.geometry, geom2 spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_relate(geom1 spatial.geometry, geom2 spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_relate(geom1 spatial.geometry, geom2 spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_relate(geom1 spatial.geometry, geom2 spatial.geometry, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_relate(geom1 spatial.geometry, geom2 spatial.geometry, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_relate(geom1 spatial.geometry, geom2 spatial.geometry, text) TO yg_reader_group;


--
-- Name: FUNCTION st_relatematch(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_relatematch(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_relatematch(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_removeirrelevantpointsforview(spatial.geometry, spatial.box2d, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_removeirrelevantpointsforview(spatial.geometry, spatial.box2d, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_removeirrelevantpointsforview(spatial.geometry, spatial.box2d, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_removepoint(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_removepoint(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_removepoint(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_removerepeatedpoints(geom spatial.geometry, tolerance double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_removerepeatedpoints(geom spatial.geometry, tolerance double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_removerepeatedpoints(geom spatial.geometry, tolerance double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_removesmallparts(spatial.geometry, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_removesmallparts(spatial.geometry, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_removesmallparts(spatial.geometry, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_resample(rast spatial.raster, ref spatial.raster, usescale boolean, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, ref spatial.raster, usescale boolean, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, ref spatial.raster, usescale boolean, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_resample(rast spatial.raster, ref spatial.raster, algorithm text, maxerr double precision, usescale boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, ref spatial.raster, algorithm text, maxerr double precision, usescale boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, ref spatial.raster, algorithm text, maxerr double precision, usescale boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_resample(rast spatial.raster, scalex double precision, scaley double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, scalex double precision, scaley double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, scalex double precision, scaley double precision, gridx double precision, gridy double precision, skewx double precision, skewy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_resample(rast spatial.raster, width integer, height integer, gridx double precision, gridy double precision, skewx double precision, skewy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, width integer, height integer, gridx double precision, gridy double precision, skewx double precision, skewy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_resample(rast spatial.raster, width integer, height integer, gridx double precision, gridy double precision, skewx double precision, skewy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rescale(rast spatial.raster, scalexy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rescale(rast spatial.raster, scalexy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rescale(rast spatial.raster, scalexy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rescale(rast spatial.raster, scalex double precision, scaley double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rescale(rast spatial.raster, scalex double precision, scaley double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rescale(rast spatial.raster, scalex double precision, scaley double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_resize(rast spatial.raster, percentwidth double precision, percentheight double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resize(rast spatial.raster, percentwidth double precision, percentheight double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_resize(rast spatial.raster, percentwidth double precision, percentheight double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_resize(rast spatial.raster, width integer, height integer, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resize(rast spatial.raster, width integer, height integer, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_resize(rast spatial.raster, width integer, height integer, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_resize(rast spatial.raster, width text, height text, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_resize(rast spatial.raster, width text, height text, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_resize(rast spatial.raster, width text, height text, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_reskew(rast spatial.raster, skewxy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reskew(rast spatial.raster, skewxy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_reskew(rast spatial.raster, skewxy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_reskew(rast spatial.raster, skewx double precision, skewy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reskew(rast spatial.raster, skewx double precision, skewy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_reskew(rast spatial.raster, skewx double precision, skewy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_retile(tab regclass, col name, ext spatial.geometry, sfx double precision, sfy double precision, tw integer, th integer, algo text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_retile(tab regclass, col name, ext spatial.geometry, sfx double precision, sfy double precision, tw integer, th integer, algo text) TO admin;
GRANT ALL ON FUNCTION spatial.st_retile(tab regclass, col name, ext spatial.geometry, sfx double precision, sfy double precision, tw integer, th integer, algo text) TO yg_reader_group;


--
-- Name: FUNCTION st_reverse(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_reverse(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_reverse(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_rotate(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotate(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotate(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rotate(spatial.geometry, double precision, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotate(spatial.geometry, double precision, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotate(spatial.geometry, double precision, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_rotate(spatial.geometry, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotate(spatial.geometry, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotate(spatial.geometry, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rotatex(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotatex(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotatex(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rotatey(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotatey(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotatey(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rotatez(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotatez(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotatez(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_rotation(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_rotation(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_rotation(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_roughness(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_roughness(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_roughness(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_roughness(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_roughness(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_roughness(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_samealignment(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_samealignment(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_samealignment(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_samealignment(ulx1 double precision, uly1 double precision, scalex1 double precision, scaley1 double precision, skewx1 double precision, skewy1 double precision, ulx2 double precision, uly2 double precision, scalex2 double precision, scaley2 double precision, skewx2 double precision, skewy2 double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_samealignment(ulx1 double precision, uly1 double precision, scalex1 double precision, scaley1 double precision, skewx1 double precision, skewy1 double precision, ulx2 double precision, uly2 double precision, scalex2 double precision, scaley2 double precision, skewx2 double precision, skewy2 double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_samealignment(ulx1 double precision, uly1 double precision, scalex1 double precision, scaley1 double precision, skewx1 double precision, skewy1 double precision, ulx2 double precision, uly2 double precision, scalex2 double precision, scaley2 double precision, skewx2 double precision, skewy2 double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_scale(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_scale(spatial.geometry, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_scale(spatial.geometry, spatial.geometry, origin spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, spatial.geometry, origin spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, spatial.geometry, origin spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_scale(spatial.geometry, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_scale(spatial.geometry, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_scalex(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scalex(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_scalex(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_scaley(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scaley(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_scaley(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_scroll(spatial.geometry, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_scroll(spatial.geometry, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_scroll(spatial.geometry, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_segmentize(geog spatial.geography, max_segment_length double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_segmentize(geog spatial.geography, max_segment_length double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_segmentize(geog spatial.geography, max_segment_length double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_segmentize(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_segmentize(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_segmentize(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setbandindex(rast spatial.raster, band integer, outdbindex integer, force boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setbandindex(rast spatial.raster, band integer, outdbindex integer, force boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setbandindex(rast spatial.raster, band integer, outdbindex integer, force boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setbandisnodata(rast spatial.raster, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setbandisnodata(rast spatial.raster, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_setbandisnodata(rast spatial.raster, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_setbandnodatavalue(rast spatial.raster, nodatavalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setbandnodatavalue(rast spatial.raster, nodatavalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setbandnodatavalue(rast spatial.raster, nodatavalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setbandnodatavalue(rast spatial.raster, band integer, nodatavalue double precision, forcechecking boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setbandnodatavalue(rast spatial.raster, band integer, nodatavalue double precision, forcechecking boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setbandnodatavalue(rast spatial.raster, band integer, nodatavalue double precision, forcechecking boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setbandpath(rast spatial.raster, band integer, outdbpath text, outdbindex integer, force boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setbandpath(rast spatial.raster, band integer, outdbpath text, outdbindex integer, force boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setbandpath(rast spatial.raster, band integer, outdbpath text, outdbindex integer, force boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_seteffectivearea(spatial.geometry, double precision, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_seteffectivearea(spatial.geometry, double precision, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_seteffectivearea(spatial.geometry, double precision, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_setgeoreference(rast spatial.raster, georef text, format text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setgeoreference(rast spatial.raster, georef text, format text) TO admin;
GRANT ALL ON FUNCTION spatial.st_setgeoreference(rast spatial.raster, georef text, format text) TO yg_reader_group;


--
-- Name: FUNCTION st_setgeoreference(rast spatial.raster, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setgeoreference(rast spatial.raster, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setgeoreference(rast spatial.raster, upperleftx double precision, upperlefty double precision, scalex double precision, scaley double precision, skewx double precision, skewy double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setgeotransform(rast spatial.raster, imag double precision, jmag double precision, theta_i double precision, theta_ij double precision, xoffset double precision, yoffset double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setgeotransform(rast spatial.raster, imag double precision, jmag double precision, theta_i double precision, theta_ij double precision, xoffset double precision, yoffset double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setgeotransform(rast spatial.raster, imag double precision, jmag double precision, theta_i double precision, theta_ij double precision, xoffset double precision, yoffset double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setm(rast spatial.raster, geom spatial.geometry, resample text, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setm(rast spatial.raster, geom spatial.geometry, resample text, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_setm(rast spatial.raster, geom spatial.geometry, resample text, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_setpoint(spatial.geometry, integer, spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setpoint(spatial.geometry, integer, spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_setpoint(spatial.geometry, integer, spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_setrotation(rast spatial.raster, rotation double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setrotation(rast spatial.raster, rotation double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setrotation(rast spatial.raster, rotation double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setscale(rast spatial.raster, scale double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setscale(rast spatial.raster, scale double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setscale(rast spatial.raster, scale double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setscale(rast spatial.raster, scalex double precision, scaley double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setscale(rast spatial.raster, scalex double precision, scaley double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setscale(rast spatial.raster, scalex double precision, scaley double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setskew(rast spatial.raster, skew double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setskew(rast spatial.raster, skew double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setskew(rast spatial.raster, skew double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setskew(rast spatial.raster, skewx double precision, skewy double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setskew(rast spatial.raster, skewx double precision, skewy double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setskew(rast spatial.raster, skewx double precision, skewy double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setsrid(geog spatial.geography, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setsrid(geog spatial.geography, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_setsrid(geog spatial.geography, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_setsrid(geom spatial.geometry, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setsrid(geom spatial.geometry, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_setsrid(geom spatial.geometry, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_setsrid(rast spatial.raster, srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setsrid(rast spatial.raster, srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_setsrid(rast spatial.raster, srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_setupperleft(rast spatial.raster, upperleftx double precision, upperlefty double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setupperleft(rast spatial.raster, upperleftx double precision, upperlefty double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setupperleft(rast spatial.raster, upperleftx double precision, upperlefty double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalue(rast spatial.raster, geom spatial.geometry, newvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, geom spatial.geometry, newvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, geom spatial.geometry, newvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalue(rast spatial.raster, x integer, y integer, newvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, x integer, y integer, newvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, x integer, y integer, newvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalue(rast spatial.raster, nband integer, geom spatial.geometry, newvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, nband integer, geom spatial.geometry, newvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, nband integer, geom spatial.geometry, newvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalue(rast spatial.raster, band integer, x integer, y integer, newvalue double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, band integer, x integer, y integer, newvalue double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalue(rast spatial.raster, band integer, x integer, y integer, newvalue double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalues(rast spatial.raster, nband integer, geomvalset spatial.geomval[], keepnodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, geomvalset spatial.geomval[], keepnodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, geomvalset spatial.geomval[], keepnodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], noset boolean[], keepnodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], noset boolean[], keepnodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], noset boolean[], keepnodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], nosetvalue double precision, keepnodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], nosetvalue double precision, keepnodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, x integer, y integer, newvalueset double precision[], nosetvalue double precision, keepnodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalues(rast spatial.raster, x integer, y integer, width integer, height integer, newvalue double precision, keepnodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, x integer, y integer, width integer, height integer, newvalue double precision, keepnodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, x integer, y integer, width integer, height integer, newvalue double precision, keepnodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setvalues(rast spatial.raster, nband integer, x integer, y integer, width integer, height integer, newvalue double precision, keepnodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, x integer, y integer, width integer, height integer, newvalue double precision, keepnodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_setvalues(rast spatial.raster, nband integer, x integer, y integer, width integer, height integer, newvalue double precision, keepnodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_setz(rast spatial.raster, geom spatial.geometry, resample text, band integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_setz(rast spatial.raster, geom spatial.geometry, resample text, band integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_setz(rast spatial.raster, geom spatial.geometry, resample text, band integer) TO yg_reader_group;


--
-- Name: FUNCTION st_sharedpaths(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_sharedpaths(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_sharedpaths(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_shiftlongitude(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_shiftlongitude(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_shiftlongitude(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_shortestline(text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_shortestline(text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_shortestline(text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_shortestline(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_shortestline(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_shortestline(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_shortestline(spatial.geography, spatial.geography, use_spheroid boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_shortestline(spatial.geography, spatial.geography, use_spheroid boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_shortestline(spatial.geography, spatial.geography, use_spheroid boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_simplify(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_simplify(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_simplify(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_simplify(spatial.geometry, double precision, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_simplify(spatial.geometry, double precision, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_simplify(spatial.geometry, double precision, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_simplifypolygonhull(geom spatial.geometry, vertex_fraction double precision, is_outer boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_simplifypolygonhull(geom spatial.geometry, vertex_fraction double precision, is_outer boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_simplifypolygonhull(geom spatial.geometry, vertex_fraction double precision, is_outer boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_simplifypreservetopology(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_simplifypreservetopology(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_simplifypreservetopology(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_simplifyvw(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_simplifyvw(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_simplifyvw(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_skewx(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_skewx(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_skewx(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_skewy(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_skewy(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_skewy(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_slope(rast spatial.raster, nband integer, pixeltype text, units text, scale double precision, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_slope(rast spatial.raster, nband integer, pixeltype text, units text, scale double precision, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_slope(rast spatial.raster, nband integer, pixeltype text, units text, scale double precision, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_slope(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, units text, scale double precision, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_slope(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, units text, scale double precision, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_slope(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, units text, scale double precision, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_snap(geom1 spatial.geometry, geom2 spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snap(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snap(geom1 spatial.geometry, geom2 spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(spatial.geometry, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(spatial.geometry, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(spatial.geometry, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(spatial.geometry, double precision, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(spatial.geometry, double precision, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(spatial.geometry, double precision, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(geom1 spatial.geometry, geom2 spatial.geometry, double precision, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(geom1 spatial.geometry, geom2 spatial.geometry, double precision, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(geom1 spatial.geometry, geom2 spatial.geometry, double precision, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, scalexy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, scalexy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, scalexy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, scalex double precision, scaley double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, scalex double precision, scaley double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, scalex double precision, scaley double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, algorithm text, maxerr double precision, scalex double precision, scaley double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, algorithm text, maxerr double precision, scalex double precision, scaley double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_snaptogrid(rast spatial.raster, gridx double precision, gridy double precision, algorithm text, maxerr double precision, scalex double precision, scaley double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_split(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_split(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_split(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_square(size double precision, cell_i integer, cell_j integer, origin spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_square(size double precision, cell_i integer, cell_j integer, origin spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_square(size double precision, cell_i integer, cell_j integer, origin spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_squaregrid(size double precision, bounds spatial.geometry, OUT geom spatial.geometry, OUT i integer, OUT j integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_squaregrid(size double precision, bounds spatial.geometry, OUT geom spatial.geometry, OUT i integer, OUT j integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_squaregrid(size double precision, bounds spatial.geometry, OUT geom spatial.geometry, OUT i integer, OUT j integer) TO yg_reader_group;


--
-- Name: FUNCTION st_srid(geog spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_srid(geog spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_srid(geog spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_srid(geom spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_srid(geom spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_srid(geom spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_srid(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_srid(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_srid(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_startpoint(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_startpoint(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_startpoint(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_stddev4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_stddev4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_stddev4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_stddev4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_stddev4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_stddev4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_subdivide(geom spatial.geometry, maxvertices integer, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_subdivide(geom spatial.geometry, maxvertices integer, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_subdivide(geom spatial.geometry, maxvertices integer, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_sum4ma(value double precision[], pos integer[], VARIADIC userargs text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_sum4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_sum4ma(value double precision[], pos integer[], VARIADIC userargs text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_sum4ma(matrix double precision[], nodatamode text, VARIADIC args text[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_sum4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_sum4ma(matrix double precision[], nodatamode text, VARIADIC args text[]) TO yg_reader_group;


--
-- Name: FUNCTION st_summary(spatial.geography); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summary(spatial.geography) TO admin;
GRANT ALL ON FUNCTION spatial.st_summary(spatial.geography) TO yg_reader_group;


--
-- Name: FUNCTION st_summary(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summary(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_summary(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_summary(rast spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summary(rast spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_summary(rast spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_summarystats(rast spatial.raster, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summarystats(rast spatial.raster, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_summarystats(rast spatial.raster, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_summarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_summarystats(rast spatial.raster, nband integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_swapordinates(geom spatial.geometry, ords cstring); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_swapordinates(geom spatial.geometry, ords cstring) TO admin;
GRANT ALL ON FUNCTION spatial.st_swapordinates(geom spatial.geometry, ords cstring) TO yg_reader_group;


--
-- Name: FUNCTION st_symdifference(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_symdifference(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_symdifference(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_symmetricdifference(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_symmetricdifference(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_symmetricdifference(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_tile(rast spatial.raster, width integer, height integer, padwithnodata boolean, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tile(rast spatial.raster, width integer, height integer, padwithnodata boolean, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_tile(rast spatial.raster, width integer, height integer, padwithnodata boolean, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_tile(rast spatial.raster, nband integer[], width integer, height integer, padwithnodata boolean, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tile(rast spatial.raster, nband integer[], width integer, height integer, padwithnodata boolean, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_tile(rast spatial.raster, nband integer[], width integer, height integer, padwithnodata boolean, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_tile(rast spatial.raster, nband integer, width integer, height integer, padwithnodata boolean, nodataval double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tile(rast spatial.raster, nband integer, width integer, height integer, padwithnodata boolean, nodataval double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_tile(rast spatial.raster, nband integer, width integer, height integer, padwithnodata boolean, nodataval double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_tileenvelope(zoom integer, x integer, y integer, bounds spatial.geometry, margin double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tileenvelope(zoom integer, x integer, y integer, bounds spatial.geometry, margin double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_tileenvelope(zoom integer, x integer, y integer, bounds spatial.geometry, margin double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_touches(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_touches(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_touches(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_touches(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_touches(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_touches(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_touches(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_touches(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_touches(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_tpi(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tpi(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_tpi(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_tpi(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tpi(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_tpi(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(spatial.geometry, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(spatial.geometry, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(spatial.geometry, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(geom spatial.geometry, to_proj text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(geom spatial.geometry, to_proj text) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(geom spatial.geometry, to_proj text) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(geom spatial.geometry, from_proj text, to_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(geom spatial.geometry, from_proj text, to_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(geom spatial.geometry, from_proj text, to_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(geom spatial.geometry, from_proj text, to_proj text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(geom spatial.geometry, from_proj text, to_proj text) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(geom spatial.geometry, from_proj text, to_proj text) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(rast spatial.raster, alignto spatial.raster, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, alignto spatial.raster, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, alignto spatial.raster, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(rast spatial.raster, srid integer, scalexy double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, srid integer, scalexy double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, srid integer, scalexy double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(rast spatial.raster, srid integer, scalex double precision, scaley double precision, algorithm text, maxerr double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, srid integer, scalex double precision, scaley double precision, algorithm text, maxerr double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, srid integer, scalex double precision, scaley double precision, algorithm text, maxerr double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_transform(rast spatial.raster, srid integer, algorithm text, maxerr double precision, scalex double precision, scaley double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, srid integer, algorithm text, maxerr double precision, scalex double precision, scaley double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_transform(rast spatial.raster, srid integer, algorithm text, maxerr double precision, scalex double precision, scaley double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_transformpipeline(geom spatial.geometry, pipeline text, to_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transformpipeline(geom spatial.geometry, pipeline text, to_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_transformpipeline(geom spatial.geometry, pipeline text, to_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_translate(spatial.geometry, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_translate(spatial.geometry, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_translate(spatial.geometry, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_translate(spatial.geometry, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_translate(spatial.geometry, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_translate(spatial.geometry, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_transscale(spatial.geometry, double precision, double precision, double precision, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_transscale(spatial.geometry, double precision, double precision, double precision, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_transscale(spatial.geometry, double precision, double precision, double precision, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_tri(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tri(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_tri(rast spatial.raster, nband integer, pixeltype text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_tri(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_tri(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_tri(rast spatial.raster, nband integer, customextent spatial.raster, pixeltype text, interpolate_nodata boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_triangulatepolygon(g1 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_triangulatepolygon(g1 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_triangulatepolygon(g1 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_unaryunion(spatial.geometry, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_unaryunion(spatial.geometry, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_unaryunion(spatial.geometry, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.geometry[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.geometry[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.geometry[]) TO yg_reader_group;


--
-- Name: FUNCTION st_union(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_union(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(geom1 spatial.geometry, geom2 spatial.geometry, gridsize double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_upperleftx(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_upperleftx(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_upperleftx(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_upperlefty(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_upperlefty(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_upperlefty(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_value(rast spatial.raster, pt spatial.geometry, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, pt spatial.geometry, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, pt spatial.geometry, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_value(rast spatial.raster, x integer, y integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, x integer, y integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, x integer, y integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_value(rast spatial.raster, band integer, x integer, y integer, exclude_nodata_value boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, band integer, x integer, y integer, exclude_nodata_value boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, band integer, x integer, y integer, exclude_nodata_value boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_value(rast spatial.raster, band integer, pt spatial.geometry, exclude_nodata_value boolean, resample text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, band integer, pt spatial.geometry, exclude_nodata_value boolean, resample text) TO admin;
GRANT ALL ON FUNCTION spatial.st_value(rast spatial.raster, band integer, pt spatial.geometry, exclude_nodata_value boolean, resample text) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rast spatial.raster, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rast spatial.raster, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rastertable text, rastercolumn text, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rastertable text, rastercolumn text, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rast spatial.raster, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rast spatial.raster, nband integer, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rastertable text, rastercolumn text, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rastertable text, rastercolumn text, nband integer, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT count integer) TO yg_reader_group;


--
-- Name: FUNCTION st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuecount(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rast spatial.raster, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rast spatial.raster, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rastertable text, rastercolumn text, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rastertable text, rastercolumn text, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rast spatial.raster, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rast spatial.raster, nband integer, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rastertable text, rastercolumn text, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rastertable text, rastercolumn text, nband integer, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rast spatial.raster, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalues double precision[], roundto double precision, OUT value double precision, OUT percent double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_valuepercent(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_valuepercent(rastertable text, rastercolumn text, nband integer, exclude_nodata_value boolean, searchvalue double precision, roundto double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_voronoilines(g1 spatial.geometry, tolerance double precision, extend_to spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_voronoilines(g1 spatial.geometry, tolerance double precision, extend_to spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_voronoilines(g1 spatial.geometry, tolerance double precision, extend_to spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_voronoipolygons(g1 spatial.geometry, tolerance double precision, extend_to spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_voronoipolygons(g1 spatial.geometry, tolerance double precision, extend_to spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_voronoipolygons(g1 spatial.geometry, tolerance double precision, extend_to spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_width(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_width(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_width(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_within(geom1 spatial.geometry, geom2 spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_within(geom1 spatial.geometry, geom2 spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_within(geom1 spatial.geometry, geom2 spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_within(rast1 spatial.raster, rast2 spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_within(rast1 spatial.raster, rast2 spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_within(rast1 spatial.raster, rast2 spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_within(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_within(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_within(rast1 spatial.raster, nband1 integer, rast2 spatial.raster, nband2 integer) TO yg_reader_group;


--
-- Name: FUNCTION st_wkbtosql(wkb bytea); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_wkbtosql(wkb bytea) TO admin;
GRANT ALL ON FUNCTION spatial.st_wkbtosql(wkb bytea) TO yg_reader_group;


--
-- Name: FUNCTION st_wkttosql(text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_wkttosql(text) TO admin;
GRANT ALL ON FUNCTION spatial.st_wkttosql(text) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoord(rast spatial.raster, pt spatial.geometry, OUT columnx integer, OUT rowy integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoord(rast spatial.raster, pt spatial.geometry, OUT columnx integer, OUT rowy integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoord(rast spatial.raster, pt spatial.geometry, OUT columnx integer, OUT rowy integer) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoord(rast spatial.raster, longitude double precision, latitude double precision, OUT columnx integer, OUT rowy integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoord(rast spatial.raster, longitude double precision, latitude double precision, OUT columnx integer, OUT rowy integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoord(rast spatial.raster, longitude double precision, latitude double precision, OUT columnx integer, OUT rowy integer) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoordx(rast spatial.raster, xw double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoordx(rast spatial.raster, xw double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoordx(rast spatial.raster, xw double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoordx(rast spatial.raster, pt spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoordx(rast spatial.raster, pt spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoordx(rast spatial.raster, pt spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoordx(rast spatial.raster, xw double precision, yw double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoordx(rast spatial.raster, xw double precision, yw double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoordx(rast spatial.raster, xw double precision, yw double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoordy(rast spatial.raster, yw double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoordy(rast spatial.raster, yw double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoordy(rast spatial.raster, yw double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoordy(rast spatial.raster, pt spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoordy(rast spatial.raster, pt spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoordy(rast spatial.raster, pt spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_worldtorastercoordy(rast spatial.raster, xw double precision, yw double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_worldtorastercoordy(rast spatial.raster, xw double precision, yw double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_worldtorastercoordy(rast spatial.raster, xw double precision, yw double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_wrapx(geom spatial.geometry, wrap double precision, move double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_wrapx(geom spatial.geometry, wrap double precision, move double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_wrapx(geom spatial.geometry, wrap double precision, move double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_x(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_x(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_x(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_xmax(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_xmax(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_xmax(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION st_xmin(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_xmin(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_xmin(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION st_y(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_y(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_y(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_ymax(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ymax(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_ymax(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION st_ymin(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_ymin(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_ymin(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION st_z(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_z(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_z(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_zmax(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_zmax(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_zmax(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION st_zmflag(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_zmflag(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_zmflag(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_zmin(spatial.box3d); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_zmin(spatial.box3d) TO admin;
GRANT ALL ON FUNCTION spatial.st_zmin(spatial.box3d) TO yg_reader_group;


--
-- Name: FUNCTION update_geom_type(); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.update_geom_type() TO admin;
GRANT ALL ON FUNCTION spatial.update_geom_type() TO yg_reader_group;


--
-- Name: FUNCTION updategeometrysrid(character varying, character varying, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.updategeometrysrid(character varying, character varying, integer) TO admin;
GRANT ALL ON FUNCTION spatial.updategeometrysrid(character varying, character varying, integer) TO yg_reader_group;


--
-- Name: FUNCTION updategeometrysrid(character varying, character varying, character varying, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.updategeometrysrid(character varying, character varying, character varying, integer) TO admin;
GRANT ALL ON FUNCTION spatial.updategeometrysrid(character varying, character varying, character varying, integer) TO yg_reader_group;


--
-- Name: FUNCTION updategeometrysrid(catalogn_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.updategeometrysrid(catalogn_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer) TO admin;
GRANT ALL ON FUNCTION spatial.updategeometrysrid(catalogn_name character varying, schema_name character varying, table_name character varying, column_name character varying, new_srid_in integer) TO yg_reader_group;


--
-- Name: FUNCTION updaterastersrid(table_name name, column_name name, new_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.updaterastersrid(table_name name, column_name name, new_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.updaterastersrid(table_name name, column_name name, new_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION updaterastersrid(schema_name name, table_name name, column_name name, new_srid integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.updaterastersrid(schema_name name, table_name name, column_name name, new_srid integer) TO admin;
GRANT ALL ON FUNCTION spatial.updaterastersrid(schema_name name, table_name name, column_name name, new_srid integer) TO yg_reader_group;


--
-- Name: FUNCTION st_3dextent(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_3dextent(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_3dextent(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_asflatgeobuf(anyelement); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asflatgeobuf(anyelement) TO admin;
GRANT ALL ON FUNCTION spatial.st_asflatgeobuf(anyelement) TO yg_reader_group;


--
-- Name: FUNCTION st_asflatgeobuf(anyelement, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asflatgeobuf(anyelement, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_asflatgeobuf(anyelement, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_asflatgeobuf(anyelement, boolean, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asflatgeobuf(anyelement, boolean, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asflatgeobuf(anyelement, boolean, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asgeobuf(anyelement); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgeobuf(anyelement) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgeobuf(anyelement) TO yg_reader_group;


--
-- Name: FUNCTION st_asgeobuf(anyelement, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asgeobuf(anyelement, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asgeobuf(anyelement, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asmvt(anyelement); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement) TO yg_reader_group;


--
-- Name: FUNCTION st_asmvt(anyelement, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asmvt(anyelement, text, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_asmvt(anyelement, text, integer, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text, integer, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text, integer, text) TO yg_reader_group;


--
-- Name: FUNCTION st_asmvt(anyelement, text, integer, text, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text, integer, text, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_asmvt(anyelement, text, integer, text, text) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterintersecting(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterintersecting(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterintersecting(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_clusterwithin(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_clusterwithin(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_clusterwithin(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_collect(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_collect(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_collect(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_countagg(spatial.raster, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_countagg(spatial.raster, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_countagg(spatial.raster, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_countagg(spatial.raster, integer, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_countagg(spatial.raster, integer, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_countagg(spatial.raster, integer, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_countagg(spatial.raster, integer, boolean, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_countagg(spatial.raster, integer, boolean, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_countagg(spatial.raster, integer, boolean, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_coverageunion(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_coverageunion(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_coverageunion(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_extent(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_extent(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_extent(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_makeline(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_makeline(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_makeline(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_memcollect(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_memcollect(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_memcollect(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_memunion(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_memunion(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_memunion(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_polygonize(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_polygonize(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_polygonize(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_samealignment(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_samealignment(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_samealignment(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_summarystatsagg(spatial.raster, boolean, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summarystatsagg(spatial.raster, boolean, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_summarystatsagg(spatial.raster, boolean, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_summarystatsagg(spatial.raster, integer, boolean); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summarystatsagg(spatial.raster, integer, boolean) TO admin;
GRANT ALL ON FUNCTION spatial.st_summarystatsagg(spatial.raster, integer, boolean) TO yg_reader_group;


--
-- Name: FUNCTION st_summarystatsagg(spatial.raster, integer, boolean, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_summarystatsagg(spatial.raster, integer, boolean, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_summarystatsagg(spatial.raster, integer, boolean, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.geometry); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.geometry) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.geometry) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.raster); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.raster) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.raster) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.geometry, double precision); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.geometry, double precision) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.geometry, double precision) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.raster, integer); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, integer) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, integer) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.raster, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, text) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.raster, spatial.unionarg[]); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, spatial.unionarg[]) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, spatial.unionarg[]) TO yg_reader_group;


--
-- Name: FUNCTION st_union(spatial.raster, integer, text); Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, integer, text) TO admin;
GRANT ALL ON FUNCTION spatial.st_union(spatial.raster, integer, text) TO yg_reader_group;


--
-- Name: TABLE feedback; Type: ACL; Schema: application; Owner: admin
--

GRANT SELECT,INSERT ON TABLE application.feedback TO PUBLIC;


--
-- Name: TABLE images; Type: ACL; Schema: application; Owner: admin
--

GRANT SELECT ON TABLE application.images TO PUBLIC;


--
-- Name: TABLE page_content; Type: ACL; Schema: application; Owner: admin
--

GRANT SELECT ON TABLE application.page_content TO PUBLIC;


--
-- Name: TABLE text; Type: ACL; Schema: application; Owner: admin
--

GRANT SELECT ON TABLE application.text TO PUBLIC;


--
-- Name: TABLE borehole_well_purposes; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.borehole_well_purposes TO admin;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.borehole_well_purposes TO yg_editor_group;


--
-- Name: SEQUENCE borehole_well_purposes_borehole_well_purpose_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.borehole_well_purposes_borehole_well_purpose_id_seq TO admin;


--
-- Name: TABLE boreholes; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.boreholes TO admin;
GRANT SELECT ON TABLE boreholes.boreholes TO PUBLIC;
GRANT SELECT ON TABLE boreholes.boreholes TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.boreholes TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.boreholes TO tkc_group;


--
-- Name: SEQUENCE boreholes_borehole_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.boreholes_borehole_id_seq TO admin;


--
-- Name: TABLE boreholes_documents; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.boreholes_documents TO admin;
GRANT SELECT ON TABLE boreholes.boreholes_documents TO PUBLIC;
GRANT SELECT ON TABLE boreholes.boreholes_documents TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.boreholes_documents TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.boreholes_documents TO tkc_group;


--
-- Name: TABLE boreholes_no_coords; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.boreholes_no_coords TO admin;
GRANT SELECT ON TABLE boreholes.boreholes_no_coords TO PUBLIC;
GRANT SELECT ON TABLE boreholes.boreholes_no_coords TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.boreholes_no_coords TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.boreholes_no_coords TO tkc_group;


--
-- Name: SEQUENCE boreholes_no_coords_borehole_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.boreholes_no_coords_borehole_id_seq TO admin;


--
-- Name: TABLE boreholes_no_coords_documents; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.boreholes_no_coords_documents TO admin;
GRANT SELECT ON TABLE boreholes.boreholes_no_coords_documents TO PUBLIC;
GRANT SELECT ON TABLE boreholes.boreholes_no_coords_documents TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.boreholes_no_coords_documents TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.boreholes_no_coords_documents TO tkc_group;


--
-- Name: TABLE casing_materials; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.casing_materials TO admin;
GRANT SELECT ON TABLE boreholes.casing_materials TO PUBLIC;
GRANT SELECT ON TABLE boreholes.casing_materials TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.casing_materials TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.casing_materials TO tkc_group;


--
-- Name: SEQUENCE casing_materials_casing_material_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.casing_materials_casing_material_id_seq TO admin;


--
-- Name: TABLE drillers; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.drillers TO admin;
GRANT SELECT ON TABLE boreholes.drillers TO PUBLIC;
GRANT SELECT ON TABLE boreholes.drillers TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.drillers TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.drillers TO tkc_group;


--
-- Name: SEQUENCE drillers_driller_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.drillers_driller_id_seq TO admin;


--
-- Name: TABLE geology; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.geology TO admin;
GRANT SELECT ON TABLE boreholes.geology TO PUBLIC;
GRANT SELECT ON TABLE boreholes.geology TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.geology TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.geology TO tkc_group;


--
-- Name: SEQUENCE geology_geo_record_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.geology_geo_record_id_seq TO admin;


--
-- Name: TABLE permafrost; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.permafrost TO admin;
GRANT SELECT ON TABLE boreholes.permafrost TO PUBLIC;
GRANT SELECT ON TABLE boreholes.permafrost TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.permafrost TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.permafrost TO tkc_group;


--
-- Name: SEQUENCE permafrost_permafrost_record_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.permafrost_permafrost_record_id_seq TO admin;


--
-- Name: TABLE wells; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON TABLE boreholes.wells TO admin;
GRANT SELECT ON TABLE boreholes.wells TO PUBLIC;
GRANT SELECT ON TABLE boreholes.wells TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE boreholes.wells TO yg_editor_group;
GRANT SELECT ON TABLE boreholes.wells TO tkc_group;


--
-- Name: SEQUENCE wells_well_id_seq; Type: ACL; Schema: boreholes; Owner: postgres
--

GRANT ALL ON SEQUENCE boreholes.wells_well_id_seq TO admin;


--
-- Name: TABLE aggregation_types; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.aggregation_types TO public_reader;
GRANT ALL ON TABLE continuous.aggregation_types TO admin;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.aggregation_types TO yg_editor_group;
GRANT SELECT ON TABLE continuous.aggregation_types TO tkc_group;
GRANT ALL ON TABLE continuous.aggregation_types TO tkc_editor;


--
-- Name: SEQUENCE aggregation_types_aggregation_type_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.aggregation_types_aggregation_type_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.aggregation_types_aggregation_type_id_seq TO admin;


--
-- Name: TABLE approvals; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.approvals TO public_reader;
GRANT ALL ON TABLE continuous.approvals TO admin;
GRANT SELECT ON TABLE continuous.approvals TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.approvals TO yg_editor_group;
GRANT SELECT ON TABLE continuous.approvals TO tkc_group;
GRANT ALL ON TABLE continuous.approvals TO tkc_editor;


--
-- Name: SEQUENCE approvals_approval_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.approvals_approval_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.approvals_approval_id_seq TO admin;


--
-- Name: TABLE contributors; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.contributors TO public_reader;
GRANT ALL ON TABLE continuous.contributors TO admin;
GRANT SELECT ON TABLE continuous.contributors TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.contributors TO yg_editor_group;
GRANT SELECT ON TABLE continuous.contributors TO tkc_group;
GRANT ALL ON TABLE continuous.contributors TO tkc_editor;


--
-- Name: SEQUENCE contributors_contributor_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.contributors_contributor_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.contributors_contributor_id_seq TO admin;


--
-- Name: TABLE correction_types; Type: ACL; Schema: continuous; Owner: admin
--

GRANT SELECT ON TABLE continuous.correction_types TO public_reader;
GRANT SELECT ON TABLE continuous.correction_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.correction_types TO yg_editor_group;
GRANT SELECT ON TABLE continuous.correction_types TO tkc_group;
GRANT ALL ON TABLE continuous.correction_types TO tkc_editor;
GRANT ALL ON TABLE continuous.correction_types TO PUBLIC;


--
-- Name: SEQUENCE correction_types_correction_type_id_seq; Type: ACL; Schema: continuous; Owner: admin
--

GRANT SELECT ON SEQUENCE continuous.correction_types_correction_type_id_seq TO yg_reader_group;


--
-- Name: TABLE corrections; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.corrections TO public_reader;
GRANT ALL ON TABLE continuous.corrections TO admin;
GRANT SELECT ON TABLE continuous.corrections TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.corrections TO yg_editor_group;
GRANT SELECT ON TABLE continuous.corrections TO tkc_group;
GRANT ALL ON TABLE continuous.corrections TO tkc_editor;


--
-- Name: SEQUENCE corrections_correction_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.corrections_correction_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.corrections_correction_id_seq TO admin;


--
-- Name: TABLE extrema; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.extrema TO public_reader;
GRANT ALL ON TABLE continuous.extrema TO admin;
GRANT SELECT ON TABLE continuous.extrema TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.extrema TO yg_editor_group;
GRANT SELECT ON TABLE continuous.extrema TO tkc_group;
GRANT ALL ON TABLE continuous.extrema TO tkc_editor;


--
-- Name: TABLE forecasts; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.forecasts TO public_reader;
GRANT ALL ON TABLE continuous.forecasts TO admin;
GRANT SELECT ON TABLE continuous.forecasts TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.forecasts TO yg_editor_group;
GRANT SELECT ON TABLE continuous.forecasts TO tkc_group;
GRANT ALL ON TABLE continuous.forecasts TO tkc_editor;


--
-- Name: TABLE grades; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.grades TO public_reader;
GRANT ALL ON TABLE continuous.grades TO admin;
GRANT SELECT ON TABLE continuous.grades TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.grades TO yg_editor_group;
GRANT SELECT ON TABLE continuous.grades TO tkc_group;
GRANT ALL ON TABLE continuous.grades TO tkc_editor;


--
-- Name: SEQUENCE grades_grade_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.grades_grade_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.grades_grade_id_seq TO admin;


--
-- Name: TABLE measurements_calculated_daily; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON TABLE continuous.measurements_calculated_daily TO admin;
GRANT SELECT ON TABLE continuous.measurements_calculated_daily TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.measurements_calculated_daily TO yg_editor_group;
GRANT SELECT ON TABLE continuous.measurements_calculated_daily TO tkc_group;
GRANT ALL ON TABLE continuous.measurements_calculated_daily TO tkc_editor;


--
-- Name: TABLE timeseries; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.timeseries TO public_reader;
GRANT ALL ON TABLE continuous.timeseries TO admin;
GRANT SELECT ON TABLE continuous.timeseries TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.timeseries TO yg_editor_group;
GRANT SELECT ON TABLE continuous.timeseries TO tkc_group;
GRANT ALL ON TABLE continuous.timeseries TO tkc_editor;


--
-- Name: TABLE measurements_calculated_daily_corrected; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.measurements_calculated_daily_corrected TO public_reader;
GRANT ALL ON TABLE continuous.measurements_calculated_daily_corrected TO admin;
GRANT SELECT ON TABLE continuous.measurements_calculated_daily_corrected TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.measurements_calculated_daily_corrected TO yg_editor_group;
GRANT SELECT ON TABLE continuous.measurements_calculated_daily_corrected TO tkc_group;
GRANT ALL ON TABLE continuous.measurements_calculated_daily_corrected TO tkc_editor;


--
-- Name: TABLE measurements_continuous; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT ALL ON TABLE continuous.measurements_continuous TO admin;
GRANT SELECT ON TABLE continuous.measurements_continuous TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.measurements_continuous TO yg_editor_group;
GRANT SELECT ON TABLE continuous.measurements_continuous TO tkc_group;
GRANT ALL ON TABLE continuous.measurements_continuous TO tkc_editor;


--
-- Name: TABLE measurements_continuous_corrected; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.measurements_continuous_corrected TO public_reader;
GRANT ALL ON TABLE continuous.measurements_continuous_corrected TO admin;
GRANT SELECT ON TABLE continuous.measurements_continuous_corrected TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.measurements_continuous_corrected TO yg_editor_group;
GRANT SELECT ON TABLE continuous.measurements_continuous_corrected TO tkc_group;
GRANT ALL ON TABLE continuous.measurements_continuous_corrected TO tkc_editor;


--
-- Name: TABLE measurements_hourly_corrected; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.measurements_hourly_corrected TO public_reader;
GRANT ALL ON TABLE continuous.measurements_hourly_corrected TO admin;
GRANT SELECT ON TABLE continuous.measurements_hourly_corrected TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.measurements_hourly_corrected TO yg_editor_group;
GRANT SELECT ON TABLE continuous.measurements_hourly_corrected TO tkc_group;
GRANT ALL ON TABLE continuous.measurements_hourly_corrected TO tkc_editor;


--
-- Name: TABLE owners; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.owners TO public_reader;
GRANT ALL ON TABLE continuous.owners TO admin;
GRANT SELECT ON TABLE continuous.owners TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.owners TO yg_editor_group;
GRANT SELECT ON TABLE continuous.owners TO tkc_group;
GRANT ALL ON TABLE continuous.owners TO tkc_editor;


--
-- Name: SEQUENCE owners_owner_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.owners_owner_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.owners_owner_id_seq TO admin;


--
-- Name: TABLE qualifiers; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.qualifiers TO public_reader;
GRANT ALL ON TABLE continuous.qualifiers TO admin;
GRANT SELECT ON TABLE continuous.qualifiers TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.qualifiers TO yg_editor_group;
GRANT SELECT ON TABLE continuous.qualifiers TO tkc_group;
GRANT ALL ON TABLE continuous.qualifiers TO tkc_editor;


--
-- Name: SEQUENCE qualifiers_qualifier_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.qualifiers_qualifier_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.qualifiers_qualifier_id_seq TO admin;


--
-- Name: TABLE rating_curve_points; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.rating_curve_points TO public_reader;
GRANT ALL ON TABLE continuous.rating_curve_points TO admin;
GRANT SELECT ON TABLE continuous.rating_curve_points TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.rating_curve_points TO yg_editor_group;
GRANT SELECT ON TABLE continuous.rating_curve_points TO tkc_group;
GRANT ALL ON TABLE continuous.rating_curve_points TO tkc_editor;


--
-- Name: SEQUENCE rating_curve_points_curve_point_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.rating_curve_points_curve_point_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.rating_curve_points_curve_point_id_seq TO admin;


--
-- Name: TABLE rating_curve_shifts; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.rating_curve_shifts TO public_reader;
GRANT ALL ON TABLE continuous.rating_curve_shifts TO admin;
GRANT SELECT ON TABLE continuous.rating_curve_shifts TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.rating_curve_shifts TO yg_editor_group;
GRANT SELECT ON TABLE continuous.rating_curve_shifts TO tkc_group;
GRANT ALL ON TABLE continuous.rating_curve_shifts TO tkc_editor;


--
-- Name: SEQUENCE rating_curve_shifts_curve_shift_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.rating_curve_shifts_curve_shift_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.rating_curve_shifts_curve_shift_id_seq TO admin;


--
-- Name: TABLE rating_curves; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.rating_curves TO public_reader;
GRANT ALL ON TABLE continuous.rating_curves TO admin;
GRANT SELECT ON TABLE continuous.rating_curves TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.rating_curves TO yg_editor_group;
GRANT SELECT ON TABLE continuous.rating_curves TO tkc_group;
GRANT ALL ON TABLE continuous.rating_curves TO tkc_editor;


--
-- Name: SEQUENCE rating_curves_curve_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.rating_curves_curve_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.rating_curves_curve_id_seq TO admin;


--
-- Name: TABLE thresholds; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON TABLE continuous.thresholds TO public_reader;
GRANT ALL ON TABLE continuous.thresholds TO admin;
GRANT SELECT ON TABLE continuous.thresholds TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.thresholds TO yg_editor_group;
GRANT SELECT ON TABLE continuous.thresholds TO tkc_group;
GRANT ALL ON TABLE continuous.thresholds TO tkc_editor;


--
-- Name: TABLE locations; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations TO public_reader;
GRANT ALL ON TABLE public.locations TO admin;
GRANT SELECT ON TABLE public.locations TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations TO yg_editor_group;
GRANT SELECT ON TABLE public.locations TO tkc_group;
GRANT ALL ON TABLE public.locations TO tkc_editor;


--
-- Name: TABLE locations_z; Type: ACL; Schema: public; Owner: admin
--

GRANT SELECT ON TABLE public.locations_z TO PUBLIC;


--
-- Name: TABLE media_types; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.media_types TO public_reader;
GRANT ALL ON TABLE public.media_types TO admin;
GRANT SELECT ON TABLE public.media_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.media_types TO yg_editor_group;
GRANT SELECT ON TABLE public.media_types TO tkc_group;
GRANT ALL ON TABLE public.media_types TO tkc_editor;


--
-- Name: TABLE parameters; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.parameters TO public_reader;
GRANT ALL ON TABLE public.parameters TO admin;
GRANT SELECT ON TABLE public.parameters TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.parameters TO yg_editor_group;
GRANT SELECT ON TABLE public.parameters TO tkc_group;
GRANT ALL ON TABLE public.parameters TO tkc_editor;


--
-- Name: TABLE timeseries_metadata_en; Type: ACL; Schema: continuous; Owner: admin
--

GRANT SELECT ON TABLE continuous.timeseries_metadata_en TO public_reader;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.timeseries_metadata_en TO yg_editor_group;
GRANT SELECT ON TABLE continuous.timeseries_metadata_en TO PUBLIC;


--
-- Name: TABLE timeseries_metadata_fr; Type: ACL; Schema: continuous; Owner: admin
--

GRANT SELECT ON TABLE continuous.timeseries_metadata_fr TO public_reader;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE continuous.timeseries_metadata_fr TO yg_editor_group;
GRANT SELECT ON TABLE continuous.timeseries_metadata_fr TO PUBLIC;


--
-- Name: SEQUENCE timeseries_timeseries_id_seq; Type: ACL; Schema: continuous; Owner: postgres
--

GRANT SELECT ON SEQUENCE continuous.timeseries_timeseries_id_seq TO public_reader;
GRANT ALL ON SEQUENCE continuous.timeseries_timeseries_id_seq TO admin;


--
-- Name: TABLE protocols_methods; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.protocols_methods TO public_reader;
GRANT ALL ON TABLE discrete.protocols_methods TO admin;
GRANT SELECT ON TABLE discrete.protocols_methods TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.protocols_methods TO yg_editor_group;
GRANT SELECT ON TABLE discrete.protocols_methods TO tkc_group;
GRANT ALL ON TABLE discrete.protocols_methods TO tkc_editor;


--
-- Name: SEQUENCE analysis_protocols_protocol_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.analysis_protocols_protocol_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.analysis_protocols_protocol_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.analysis_protocols_protocol_id_seq TO yg_reader_group;


--
-- Name: TABLE collection_methods; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.collection_methods TO public_reader;
GRANT ALL ON TABLE discrete.collection_methods TO admin;
GRANT SELECT ON TABLE discrete.collection_methods TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.collection_methods TO yg_editor_group;
GRANT SELECT ON TABLE discrete.collection_methods TO tkc_group;
GRANT ALL ON TABLE discrete.collection_methods TO tkc_editor;


--
-- Name: SEQUENCE collection_methods_collection_method_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.collection_methods_collection_method_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.collection_methods_collection_method_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.collection_methods_collection_method_id_seq TO yg_reader_group;


--
-- Name: TABLE guideline_values; Type: ACL; Schema: discrete; Owner: admin
--

GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.guideline_values TO yg_editor_group;
GRANT SELECT ON TABLE discrete.guideline_values TO tkc_group;
GRANT ALL ON TABLE discrete.guideline_values TO tkc_editor;


--
-- Name: TABLE guidelines; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.guidelines TO public_reader;
GRANT ALL ON TABLE discrete.guidelines TO admin;
GRANT SELECT ON TABLE discrete.guidelines TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.guidelines TO yg_editor_group;


--
-- Name: SEQUENCE guidelines_guideline_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.guidelines_guideline_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.guidelines_guideline_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.guidelines_guideline_id_seq TO yg_reader_group;


--
-- Name: TABLE laboratories; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.laboratories TO public_reader;
GRANT ALL ON TABLE discrete.laboratories TO admin;
GRANT SELECT ON TABLE discrete.laboratories TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.laboratories TO yg_editor_group;
GRANT SELECT ON TABLE discrete.laboratories TO tkc_group;
GRANT ALL ON TABLE discrete.laboratories TO tkc_editor;


--
-- Name: SEQUENCE laboratories_lab_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.laboratories_lab_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.laboratories_lab_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.laboratories_lab_id_seq TO yg_reader_group;


--
-- Name: TABLE result_conditions; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.result_conditions TO public_reader;
GRANT ALL ON TABLE discrete.result_conditions TO admin;
GRANT SELECT ON TABLE discrete.result_conditions TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.result_conditions TO yg_editor_group;
GRANT SELECT ON TABLE discrete.result_conditions TO tkc_group;
GRANT ALL ON TABLE discrete.result_conditions TO tkc_editor;


--
-- Name: SEQUENCE result_conditions_result_condition_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.result_conditions_result_condition_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.result_conditions_result_condition_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.result_conditions_result_condition_id_seq TO yg_reader_group;


--
-- Name: TABLE result_speciations; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.result_speciations TO public_reader;
GRANT ALL ON TABLE discrete.result_speciations TO admin;
GRANT SELECT ON TABLE discrete.result_speciations TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.result_speciations TO yg_editor_group;
GRANT SELECT ON TABLE discrete.result_speciations TO tkc_group;
GRANT ALL ON TABLE discrete.result_speciations TO tkc_editor;


--
-- Name: SEQUENCE result_speciations_result_speciation_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.result_speciations_result_speciation_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.result_speciations_result_speciation_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.result_speciations_result_speciation_id_seq TO yg_reader_group;


--
-- Name: TABLE result_types; Type: ACL; Schema: discrete; Owner: admin
--

GRANT SELECT ON TABLE discrete.result_types TO public_reader;
GRANT SELECT ON TABLE discrete.result_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.result_types TO yg_editor_group;
GRANT SELECT ON TABLE discrete.result_types TO tkc_group;
GRANT ALL ON TABLE discrete.result_types TO tkc_editor;


--
-- Name: TABLE result_value_types; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.result_value_types TO public_reader;
GRANT ALL ON TABLE discrete.result_value_types TO admin;
GRANT SELECT ON TABLE discrete.result_value_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.result_value_types TO yg_editor_group;
GRANT SELECT ON TABLE discrete.result_value_types TO tkc_group;
GRANT ALL ON TABLE discrete.result_value_types TO tkc_editor;


--
-- Name: SEQUENCE result_value_types_result_value_type_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.result_value_types_result_value_type_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.result_value_types_result_value_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.result_value_types_result_value_type_id_seq TO yg_reader_group;


--
-- Name: TABLE results; Type: ACL; Schema: discrete; Owner: admin
--

GRANT SELECT ON TABLE discrete.results TO public_reader;
GRANT SELECT ON TABLE discrete.results TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.results TO yg_editor_group;
GRANT SELECT ON TABLE discrete.results TO tkc_group;
GRANT ALL ON TABLE discrete.results TO tkc_editor;


--
-- Name: TABLE sample_fractions; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.sample_fractions TO public_reader;
GRANT ALL ON TABLE discrete.sample_fractions TO admin;
GRANT SELECT ON TABLE discrete.sample_fractions TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.sample_fractions TO yg_editor_group;
GRANT SELECT ON TABLE discrete.sample_fractions TO tkc_group;
GRANT ALL ON TABLE discrete.sample_fractions TO tkc_editor;


--
-- Name: SEQUENCE sample_fractions_sample_fraction_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.sample_fractions_sample_fraction_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.sample_fractions_sample_fraction_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.sample_fractions_sample_fraction_id_seq TO yg_reader_group;


--
-- Name: TABLE sample_series; Type: ACL; Schema: discrete; Owner: admin
--

GRANT SELECT ON TABLE discrete.sample_series TO public_reader;
GRANT SELECT ON TABLE discrete.sample_series TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.sample_series TO yg_editor_group;
GRANT SELECT ON TABLE discrete.sample_series TO tkc_group;
GRANT ALL ON TABLE discrete.sample_series TO tkc_editor;


--
-- Name: TABLE sample_types; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON TABLE discrete.sample_types TO public_reader;
GRANT ALL ON TABLE discrete.sample_types TO admin;
GRANT SELECT ON TABLE discrete.sample_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.sample_types TO yg_editor_group;
GRANT SELECT ON TABLE discrete.sample_types TO tkc_group;
GRANT ALL ON TABLE discrete.sample_types TO tkc_editor;


--
-- Name: SEQUENCE sample_types_sample_type_id_seq; Type: ACL; Schema: discrete; Owner: postgres
--

GRANT SELECT ON SEQUENCE discrete.sample_types_sample_type_id_seq TO public_reader;
GRANT ALL ON SEQUENCE discrete.sample_types_sample_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE discrete.sample_types_sample_type_id_seq TO yg_reader_group;


--
-- Name: TABLE samples; Type: ACL; Schema: discrete; Owner: admin
--

GRANT SELECT ON TABLE discrete.samples TO public_reader;
GRANT SELECT ON TABLE discrete.samples TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE discrete.samples TO yg_editor_group;
GRANT SELECT ON TABLE discrete.samples TO tkc_group;
GRANT ALL ON TABLE discrete.samples TO tkc_editor;


--
-- Name: TABLE field_visit_images; Type: ACL; Schema: field; Owner: postgres
--

GRANT ALL ON TABLE field.field_visit_images TO admin;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_images TO public_reader;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_images TO tkc_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_images TO yg_editor_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_images TO yg_reader_group;


--
-- Name: TABLE field_visit_instruments; Type: ACL; Schema: field; Owner: postgres
--

GRANT ALL ON TABLE field.field_visit_instruments TO admin;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_instruments TO public_reader;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_instruments TO tkc_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_instruments TO yg_editor_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visit_instruments TO yg_reader_group;


--
-- Name: TABLE field_visits; Type: ACL; Schema: field; Owner: postgres
--

GRANT ALL ON TABLE field.field_visits TO admin;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visits TO public_reader;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visits TO tkc_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visits TO yg_editor_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE field.field_visits TO yg_reader_group;


--
-- Name: SEQUENCE field_visits_field_visit_id_seq; Type: ACL; Schema: field; Owner: postgres
--

GRANT ALL ON SEQUENCE field.field_visits_field_visit_id_seq TO admin;


--
-- Name: TABLE document_types; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON TABLE files.document_types TO public_reader;
GRANT ALL ON TABLE files.document_types TO admin;
GRANT SELECT ON TABLE files.document_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE files.document_types TO yg_editor_group;
GRANT SELECT ON TABLE files.document_types TO tkc_group;


--
-- Name: SEQUENCE document_types_document_type_id_seq; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON SEQUENCE files.document_types_document_type_id_seq TO public_reader;
GRANT ALL ON SEQUENCE files.document_types_document_type_id_seq TO admin;


--
-- Name: TABLE documents; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON TABLE files.documents TO public_reader;
GRANT ALL ON TABLE files.documents TO admin;
GRANT SELECT ON TABLE files.documents TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE files.documents TO yg_editor_group;
GRANT SELECT ON TABLE files.documents TO tkc_group;


--
-- Name: SEQUENCE documents_document_id_seq; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON SEQUENCE files.documents_document_id_seq TO public_reader;
GRANT ALL ON SEQUENCE files.documents_document_id_seq TO admin;


--
-- Name: TABLE documents_spatial; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON TABLE files.documents_spatial TO public_reader;
GRANT ALL ON TABLE files.documents_spatial TO admin;
GRANT SELECT ON TABLE files.documents_spatial TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE files.documents_spatial TO yg_editor_group;
GRANT SELECT ON TABLE files.documents_spatial TO tkc_group;


--
-- Name: TABLE image_series; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON TABLE files.image_series TO public_reader;
GRANT ALL ON TABLE files.image_series TO admin;
GRANT SELECT ON TABLE files.image_series TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE files.image_series TO yg_editor_group;
GRANT SELECT ON TABLE files.image_series TO tkc_group;


--
-- Name: TABLE image_types; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON TABLE files.image_types TO public_reader;
GRANT ALL ON TABLE files.image_types TO admin;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE files.image_types TO yg_editor_group;
GRANT SELECT ON TABLE files.image_types TO tkc_group;


--
-- Name: SEQUENCE image_types_image_type_id_seq; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON SEQUENCE files.image_types_image_type_id_seq TO public_reader;
GRANT ALL ON SEQUENCE files.image_types_image_type_id_seq TO admin;


--
-- Name: TABLE images; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON TABLE files.images TO public_reader;
GRANT ALL ON TABLE files.images TO admin;
GRANT SELECT ON TABLE files.images TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE files.images TO yg_editor_group;
GRANT SELECT ON TABLE files.images TO tkc_group;


--
-- Name: SEQUENCE images_image_id_seq; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON SEQUENCE files.images_image_id_seq TO public_reader;
GRANT ALL ON SEQUENCE files.images_image_id_seq TO admin;


--
-- Name: SEQUENCE images_index_img_meta_id_seq; Type: ACL; Schema: files; Owner: postgres
--

GRANT SELECT ON SEQUENCE files.images_index_img_meta_id_seq TO public_reader;
GRANT ALL ON SEQUENCE files.images_index_img_meta_id_seq TO admin;


--
-- Name: TABLE internal_status; Type: ACL; Schema: information; Owner: postgres
--

GRANT ALL ON TABLE information.internal_status TO admin;
GRANT SELECT ON TABLE information.internal_status TO yg_reader_group;


--
-- Name: TABLE version_info; Type: ACL; Schema: information; Owner: postgres
--

GRANT ALL ON TABLE information.version_info TO admin;
GRANT SELECT ON TABLE information.version_info TO yg_reader_group;
GRANT SELECT ON TABLE information.version_info TO public_reader;


--
-- Name: SEQUENCE version_info_id_seq; Type: ACL; Schema: information; Owner: postgres
--

GRANT ALL ON SEQUENCE information.version_info_id_seq TO admin;
GRANT SELECT ON SEQUENCE information.version_info_id_seq TO yg_reader_group;


--
-- Name: TABLE array_maintenance_changes; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.array_maintenance_changes TO public_reader;
GRANT ALL ON TABLE instruments.array_maintenance_changes TO admin;
GRANT SELECT ON TABLE instruments.array_maintenance_changes TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.array_maintenance_changes TO yg_editor_group;
GRANT SELECT ON TABLE instruments.array_maintenance_changes TO tkc_group;
GRANT ALL ON TABLE instruments.array_maintenance_changes TO tkc_editor;


--
-- Name: SEQUENCE array_maintenance_changes_event_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.array_maintenance_changes_event_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.array_maintenance_changes_event_id_seq TO yg_reader_group;


--
-- Name: TABLE calibrate_depth; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_depth TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_depth TO admin;
GRANT SELECT ON TABLE instruments.calibrate_depth TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_depth TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_depth TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_depth TO tkc_editor;


--
-- Name: TABLE calibrate_dissolved_oxygen; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_dissolved_oxygen TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_dissolved_oxygen TO admin;
GRANT SELECT ON TABLE instruments.calibrate_dissolved_oxygen TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_dissolved_oxygen TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_dissolved_oxygen TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_dissolved_oxygen TO tkc_editor;


--
-- Name: TABLE calibrate_orp; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_orp TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_orp TO admin;
GRANT SELECT ON TABLE instruments.calibrate_orp TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_orp TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_orp TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_orp TO tkc_editor;


--
-- Name: TABLE calibrate_ph; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_ph TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_ph TO admin;
GRANT SELECT ON TABLE instruments.calibrate_ph TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_ph TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_ph TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_ph TO tkc_editor;


--
-- Name: TABLE calibrate_specific_conductance; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_specific_conductance TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_specific_conductance TO admin;
GRANT SELECT ON TABLE instruments.calibrate_specific_conductance TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_specific_conductance TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_specific_conductance TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_specific_conductance TO tkc_editor;


--
-- Name: TABLE calibrate_temperature; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_temperature TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_temperature TO admin;
GRANT SELECT ON TABLE instruments.calibrate_temperature TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_temperature TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_temperature TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_temperature TO tkc_editor;


--
-- Name: TABLE calibrate_turbidity; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrate_turbidity TO public_reader;
GRANT ALL ON TABLE instruments.calibrate_turbidity TO admin;
GRANT SELECT ON TABLE instruments.calibrate_turbidity TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrate_turbidity TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrate_turbidity TO tkc_group;
GRANT ALL ON TABLE instruments.calibrate_turbidity TO tkc_editor;


--
-- Name: TABLE calibrations; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.calibrations TO public_reader;
GRANT ALL ON TABLE instruments.calibrations TO admin;
GRANT SELECT ON TABLE instruments.calibrations TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.calibrations TO yg_editor_group;
GRANT SELECT ON TABLE instruments.calibrations TO tkc_group;
GRANT ALL ON TABLE instruments.calibrations TO tkc_editor;


--
-- Name: SEQUENCE calibrations_calibration_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.calibrations_calibration_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.calibrations_calibration_id_seq TO yg_reader_group;


--
-- Name: TABLE instrument_maintenance; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.instrument_maintenance TO public_reader;
GRANT ALL ON TABLE instruments.instrument_maintenance TO admin;
GRANT SELECT ON TABLE instruments.instrument_maintenance TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.instrument_maintenance TO yg_editor_group;
GRANT SELECT ON TABLE instruments.instrument_maintenance TO tkc_group;
GRANT ALL ON TABLE instruments.instrument_maintenance TO tkc_editor;


--
-- Name: SEQUENCE instrument_maintenance_event_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.instrument_maintenance_event_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.instrument_maintenance_event_id_seq TO yg_reader_group;


--
-- Name: TABLE instrument_make; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.instrument_make TO public_reader;
GRANT ALL ON TABLE instruments.instrument_make TO admin;
GRANT SELECT ON TABLE instruments.instrument_make TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.instrument_make TO yg_editor_group;
GRANT SELECT ON TABLE instruments.instrument_make TO tkc_group;
GRANT ALL ON TABLE instruments.instrument_make TO tkc_editor;


--
-- Name: SEQUENCE instrument_make_make_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.instrument_make_make_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.instrument_make_make_id_seq TO yg_reader_group;


--
-- Name: TABLE instrument_model; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.instrument_model TO public_reader;
GRANT ALL ON TABLE instruments.instrument_model TO admin;
GRANT SELECT ON TABLE instruments.instrument_model TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.instrument_model TO yg_editor_group;
GRANT SELECT ON TABLE instruments.instrument_model TO tkc_group;
GRANT ALL ON TABLE instruments.instrument_model TO tkc_editor;


--
-- Name: SEQUENCE instrument_model_model_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.instrument_model_model_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.instrument_model_model_id_seq TO yg_reader_group;


--
-- Name: TABLE instrument_type; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.instrument_type TO public_reader;
GRANT ALL ON TABLE instruments.instrument_type TO admin;
GRANT SELECT ON TABLE instruments.instrument_type TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.instrument_type TO yg_editor_group;
GRANT SELECT ON TABLE instruments.instrument_type TO tkc_group;
GRANT ALL ON TABLE instruments.instrument_type TO tkc_editor;


--
-- Name: SEQUENCE instrument_type_type_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.instrument_type_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.instrument_type_type_id_seq TO yg_reader_group;


--
-- Name: TABLE instruments; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.instruments TO public_reader;
GRANT ALL ON TABLE instruments.instruments TO admin;
GRANT SELECT ON TABLE instruments.instruments TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.instruments TO yg_editor_group;
GRANT SELECT ON TABLE instruments.instruments TO tkc_group;
GRANT ALL ON TABLE instruments.instruments TO tkc_editor;


--
-- Name: SEQUENCE instruments_instrument_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.instruments_instrument_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.instruments_instrument_id_seq TO yg_reader_group;


--
-- Name: TABLE observers; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.observers TO public_reader;
GRANT ALL ON TABLE instruments.observers TO admin;
GRANT SELECT ON TABLE instruments.observers TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.observers TO yg_editor_group;
GRANT SELECT ON TABLE instruments.observers TO tkc_group;
GRANT ALL ON TABLE instruments.observers TO tkc_editor;


--
-- Name: SEQUENCE observers_observer_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.observers_observer_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.observers_observer_id_seq TO yg_reader_group;


--
-- Name: TABLE sensor_types; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.sensor_types TO public_reader;
GRANT ALL ON TABLE instruments.sensor_types TO admin;
GRANT SELECT ON TABLE instruments.sensor_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.sensor_types TO yg_editor_group;
GRANT SELECT ON TABLE instruments.sensor_types TO tkc_group;
GRANT ALL ON TABLE instruments.sensor_types TO tkc_editor;


--
-- Name: SEQUENCE sensor_types_sensor_type_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.sensor_types_sensor_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.sensor_types_sensor_type_id_seq TO yg_reader_group;


--
-- Name: TABLE sensors; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT SELECT ON TABLE instruments.sensors TO public_reader;
GRANT ALL ON TABLE instruments.sensors TO admin;
GRANT SELECT ON TABLE instruments.sensors TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE instruments.sensors TO yg_editor_group;
GRANT SELECT ON TABLE instruments.sensors TO tkc_group;
GRANT ALL ON TABLE instruments.sensors TO tkc_editor;


--
-- Name: SEQUENCE sensors_sensor_id_seq; Type: ACL; Schema: instruments; Owner: postgres
--

GRANT ALL ON SEQUENCE instruments.sensors_sensor_id_seq TO admin;
GRANT SELECT ON SEQUENCE instruments.sensors_sensor_id_seq TO yg_reader_group;


--
-- Name: TABLE approval_types; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.approval_types TO public_reader;
GRANT ALL ON TABLE public.approval_types TO admin;
GRANT SELECT ON TABLE public.approval_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.approval_types TO yg_editor_group;
GRANT SELECT ON TABLE public.approval_types TO tkc_group;
GRANT ALL ON TABLE public.approval_types TO tkc_editor;


--
-- Name: SEQUENCE approval_types_approval_type_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.approval_types_approval_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.approval_types_approval_type_id_seq TO yg_reader_group;


--
-- Name: TABLE datum_conversions; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.datum_conversions TO public_reader;
GRANT ALL ON TABLE public.datum_conversions TO admin;
GRANT SELECT ON TABLE public.datum_conversions TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.datum_conversions TO yg_editor_group;
GRANT SELECT ON TABLE public.datum_conversions TO tkc_group;
GRANT ALL ON TABLE public.datum_conversions TO tkc_editor;


--
-- Name: SEQUENCE datum_conversions_conversion_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.datum_conversions_conversion_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.datum_conversions_conversion_id_seq TO yg_reader_group;


--
-- Name: TABLE datum_list; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.datum_list TO public_reader;
GRANT ALL ON TABLE public.datum_list TO admin;
GRANT SELECT ON TABLE public.datum_list TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.datum_list TO yg_editor_group;
GRANT SELECT ON TABLE public.datum_list TO tkc_group;
GRANT ALL ON TABLE public.datum_list TO tkc_editor;


--
-- Name: TABLE grade_types; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.grade_types TO public_reader;
GRANT ALL ON TABLE public.grade_types TO admin;
GRANT SELECT ON TABLE public.grade_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.grade_types TO yg_editor_group;
GRANT SELECT ON TABLE public.grade_types TO tkc_group;
GRANT ALL ON TABLE public.grade_types TO tkc_editor;


--
-- Name: SEQUENCE grade_types_grade_type_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.grade_types_grade_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.grade_types_grade_type_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_networks; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_networks TO public_reader;
GRANT ALL ON TABLE public.locations_networks TO admin;
GRANT SELECT ON TABLE public.locations_networks TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_networks TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_networks TO tkc_group;
GRANT ALL ON TABLE public.locations_networks TO tkc_editor;


--
-- Name: TABLE locations_projects; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_projects TO public_reader;
GRANT ALL ON TABLE public.locations_projects TO admin;
GRANT SELECT ON TABLE public.locations_projects TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_projects TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_projects TO tkc_group;
GRANT ALL ON TABLE public.locations_projects TO tkc_editor;


--
-- Name: TABLE networks; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.networks TO public_reader;
GRANT ALL ON TABLE public.networks TO admin;
GRANT SELECT ON TABLE public.networks TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.networks TO yg_editor_group;
GRANT SELECT ON TABLE public.networks TO tkc_group;
GRANT ALL ON TABLE public.networks TO tkc_editor;


--
-- Name: TABLE projects; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.projects TO public_reader;
GRANT ALL ON TABLE public.projects TO admin;
GRANT SELECT ON TABLE public.projects TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.projects TO yg_editor_group;
GRANT SELECT ON TABLE public.projects TO tkc_group;
GRANT ALL ON TABLE public.projects TO tkc_editor;


--
-- Name: TABLE location_metadata_en; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.location_metadata_en TO public_reader;
GRANT ALL ON TABLE public.location_metadata_en TO admin;
GRANT SELECT ON TABLE public.location_metadata_en TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.location_metadata_en TO yg_editor_group;
GRANT SELECT ON TABLE public.location_metadata_en TO tkc_group;
GRANT ALL ON TABLE public.location_metadata_en TO tkc_editor;


--
-- Name: TABLE location_metadata_fr; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.location_metadata_fr TO public_reader;
GRANT ALL ON TABLE public.location_metadata_fr TO admin;
GRANT SELECT ON TABLE public.location_metadata_fr TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.location_metadata_fr TO yg_editor_group;
GRANT SELECT ON TABLE public.location_metadata_fr TO tkc_group;
GRANT ALL ON TABLE public.location_metadata_fr TO tkc_editor;


--
-- Name: TABLE location_types; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.location_types TO public_reader;
GRANT ALL ON TABLE public.location_types TO admin;
GRANT SELECT ON TABLE public.location_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.location_types TO yg_editor_group;
GRANT SELECT ON TABLE public.location_types TO tkc_group;
GRANT ALL ON TABLE public.location_types TO tkc_editor;


--
-- Name: SEQUENCE location_types_type_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.location_types_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.location_types_type_id_seq TO yg_reader_group;


--
-- Name: SEQUENCE locations_location_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_location_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_location_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_metadata_access; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_access TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_access TO admin;
GRANT SELECT ON TABLE public.locations_metadata_access TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_access TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_access TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_access TO tkc_editor;


--
-- Name: SEQUENCE locations_metadata_access_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_access_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_access_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_metadata_infrastructure; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_infrastructure TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_infrastructure TO admin;
GRANT SELECT ON TABLE public.locations_metadata_infrastructure TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_infrastructure TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_infrastructure TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_infrastructure TO tkc_editor;


--
-- Name: TABLE locations_metadata_infrastructure_groundwater; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_infrastructure_groundwater TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_infrastructure_groundwater TO admin;
GRANT SELECT ON TABLE public.locations_metadata_infrastructure_groundwater TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_infrastructure_groundwater TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_infrastructure_groundwater TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_infrastructure_groundwater TO tkc_editor;


--
-- Name: SEQUENCE locations_metadata_infrastructure_groundwater_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_infrastructure_groundwater_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_infrastructure_groundwater_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_metadata_infrastructure_hydromet; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_infrastructure_hydromet TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_infrastructure_hydromet TO admin;
GRANT SELECT ON TABLE public.locations_metadata_infrastructure_hydromet TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_infrastructure_hydromet TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_infrastructure_hydromet TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_infrastructure_hydromet TO tkc_editor;


--
-- Name: SEQUENCE locations_metadata_infrastructure_hydromet_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_infrastructure_hydromet_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_infrastructure_hydromet_id_seq TO yg_reader_group;


--
-- Name: SEQUENCE locations_metadata_infrastructure_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_infrastructure_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_infrastructure_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_metadata_instruments; Type: ACL; Schema: public; Owner: admin
--

GRANT SELECT ON TABLE public.locations_metadata_instruments TO public_reader;
GRANT SELECT ON TABLE public.locations_metadata_instruments TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_instruments TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_instruments TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_instruments TO tkc_editor;


--
-- Name: TABLE locations_metadata_maintenance; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_maintenance TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_maintenance TO admin;
GRANT SELECT ON TABLE public.locations_metadata_maintenance TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_maintenance TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_maintenance TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_maintenance TO tkc_editor;


--
-- Name: SEQUENCE locations_metadata_maintenance_location_maintenance_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_maintenance_location_maintenance_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_maintenance_location_maintenance_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_metadata_owners_operators; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_owners_operators TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_owners_operators TO admin;
GRANT SELECT ON TABLE public.locations_metadata_owners_operators TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_owners_operators TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_owners_operators TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_owners_operators TO tkc_editor;


--
-- Name: SEQUENCE locations_metadata_owners_operators_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_owners_operators_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_owners_operators_id_seq TO yg_reader_group;


--
-- Name: TABLE locations_metadata_xsections; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.locations_metadata_xsections TO public_reader;
GRANT ALL ON TABLE public.locations_metadata_xsections TO admin;
GRANT SELECT ON TABLE public.locations_metadata_xsections TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.locations_metadata_xsections TO yg_editor_group;
GRANT SELECT ON TABLE public.locations_metadata_xsections TO tkc_group;
GRANT ALL ON TABLE public.locations_metadata_xsections TO tkc_editor;


--
-- Name: SEQUENCE locations_metadata_xsections_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.locations_metadata_xsections_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.locations_metadata_xsections_id_seq TO yg_reader_group;


--
-- Name: TABLE network_project_types; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.network_project_types TO public_reader;
GRANT ALL ON TABLE public.network_project_types TO admin;
GRANT SELECT ON TABLE public.network_project_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.network_project_types TO yg_editor_group;
GRANT SELECT ON TABLE public.network_project_types TO tkc_group;
GRANT ALL ON TABLE public.network_project_types TO tkc_editor;


--
-- Name: SEQUENCE network_project_types_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.network_project_types_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.network_project_types_id_seq TO yg_reader_group;


--
-- Name: SEQUENCE networks_network_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.networks_network_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.networks_network_id_seq TO yg_reader_group;


--
-- Name: TABLE organizations; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.organizations TO public_reader;
GRANT ALL ON TABLE public.organizations TO admin;
GRANT SELECT ON TABLE public.organizations TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.organizations TO yg_editor_group;
GRANT SELECT ON TABLE public.organizations TO tkc_group;
GRANT ALL ON TABLE public.organizations TO tkc_editor;


--
-- Name: SEQUENCE owners_contributors_owner_contributor_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.owners_contributors_owner_contributor_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.owners_contributors_owner_contributor_id_seq TO yg_reader_group;


--
-- Name: SEQUENCE param_type_param_type_code_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.param_type_param_type_code_seq TO admin;
GRANT SELECT ON SEQUENCE public.param_type_param_type_code_seq TO yg_reader_group;


--
-- Name: TABLE parameter_groups; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.parameter_groups TO public_reader;
GRANT ALL ON TABLE public.parameter_groups TO admin;
GRANT SELECT ON TABLE public.parameter_groups TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.parameter_groups TO yg_editor_group;
GRANT SELECT ON TABLE public.parameter_groups TO tkc_group;
GRANT ALL ON TABLE public.parameter_groups TO tkc_editor;


--
-- Name: SEQUENCE parameter_groups_group_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.parameter_groups_group_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.parameter_groups_group_id_seq TO yg_reader_group;


--
-- Name: TABLE parameter_relationships; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.parameter_relationships TO public_reader;
GRANT ALL ON TABLE public.parameter_relationships TO admin;
GRANT SELECT ON TABLE public.parameter_relationships TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.parameter_relationships TO yg_editor_group;
GRANT SELECT ON TABLE public.parameter_relationships TO tkc_group;
GRANT ALL ON TABLE public.parameter_relationships TO tkc_editor;


--
-- Name: SEQUENCE parameter_relationships_relationship_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.parameter_relationships_relationship_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.parameter_relationships_relationship_id_seq TO yg_reader_group;


--
-- Name: TABLE parameter_sub_groups; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.parameter_sub_groups TO public_reader;
GRANT ALL ON TABLE public.parameter_sub_groups TO admin;
GRANT SELECT ON TABLE public.parameter_sub_groups TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.parameter_sub_groups TO yg_editor_group;
GRANT SELECT ON TABLE public.parameter_sub_groups TO tkc_group;
GRANT ALL ON TABLE public.parameter_sub_groups TO tkc_editor;


--
-- Name: SEQUENCE parameter_sub_groups_sub_group_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.parameter_sub_groups_sub_group_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.parameter_sub_groups_sub_group_id_seq TO yg_reader_group;


--
-- Name: SEQUENCE parameters_param_code_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.parameters_param_code_seq TO admin;
GRANT SELECT ON SEQUENCE public.parameters_param_code_seq TO yg_reader_group;


--
-- Name: SEQUENCE projects_project_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.projects_project_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.projects_project_id_seq TO yg_reader_group;


--
-- Name: TABLE qualifier_types; Type: ACL; Schema: public; Owner: postgres
--

GRANT SELECT ON TABLE public.qualifier_types TO public_reader;
GRANT ALL ON TABLE public.qualifier_types TO admin;
GRANT SELECT ON TABLE public.qualifier_types TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.qualifier_types TO yg_editor_group;
GRANT SELECT ON TABLE public.qualifier_types TO tkc_group;
GRANT ALL ON TABLE public.qualifier_types TO tkc_editor;


--
-- Name: SEQUENCE qualifier_types_qualifier_type_id_seq; Type: ACL; Schema: public; Owner: postgres
--

GRANT ALL ON SEQUENCE public.qualifier_types_qualifier_type_id_seq TO admin;
GRANT SELECT ON SEQUENCE public.qualifier_types_qualifier_type_id_seq TO yg_reader_group;


--
-- Name: TABLE sub_locations; Type: ACL; Schema: public; Owner: admin
--

GRANT SELECT ON TABLE public.sub_locations TO public_reader;
GRANT SELECT ON TABLE public.sub_locations TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.sub_locations TO yg_editor_group;
GRANT SELECT ON TABLE public.sub_locations TO tkc_group;
GRANT ALL ON TABLE public.sub_locations TO tkc_editor;


--
-- Name: TABLE geography_columns; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.geography_columns TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.geography_columns TO yg_editor_group;
GRANT SELECT ON TABLE spatial.geography_columns TO tkc_group;


--
-- Name: TABLE geometry_columns; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.geometry_columns TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.geometry_columns TO yg_editor_group;
GRANT SELECT ON TABLE spatial.geometry_columns TO tkc_group;


--
-- Name: TABLE raster_columns; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.raster_columns TO public_reader;
GRANT ALL ON TABLE spatial.raster_columns TO admin;
GRANT SELECT ON TABLE spatial.raster_columns TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.raster_columns TO yg_editor_group;
GRANT SELECT ON TABLE spatial.raster_columns TO tkc_group;


--
-- Name: TABLE raster_overviews; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.raster_overviews TO public_reader;
GRANT ALL ON TABLE spatial.raster_overviews TO admin;
GRANT SELECT ON TABLE spatial.raster_overviews TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.raster_overviews TO yg_editor_group;
GRANT SELECT ON TABLE spatial.raster_overviews TO tkc_group;


--
-- Name: TABLE raster_series_index; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.raster_series_index TO public_reader;
GRANT ALL ON TABLE spatial.raster_series_index TO admin;
GRANT SELECT ON TABLE spatial.raster_series_index TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.raster_series_index TO yg_editor_group;
GRANT SELECT ON TABLE spatial.raster_series_index TO tkc_group;


--
-- Name: SEQUENCE raster_series_index_raster_series_id_seq; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON SEQUENCE spatial.raster_series_index_raster_series_id_seq TO admin;
GRANT SELECT ON SEQUENCE spatial.raster_series_index_raster_series_id_seq TO yg_reader_group;


--
-- Name: TABLE rasters; Type: ACL; Schema: spatial; Owner: admin
--

GRANT SELECT ON TABLE spatial.rasters TO public_reader;
GRANT SELECT ON TABLE spatial.rasters TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.rasters TO yg_editor_group;
GRANT SELECT ON TABLE spatial.rasters TO tkc_group;


--
-- Name: TABLE rasters_reference; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.rasters_reference TO public_reader;
GRANT ALL ON TABLE spatial.rasters_reference TO admin;
GRANT SELECT ON TABLE spatial.rasters_reference TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.rasters_reference TO yg_editor_group;
GRANT SELECT ON TABLE spatial.rasters_reference TO tkc_group;


--
-- Name: SEQUENCE rasters_reference_reference_id_seq; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON SEQUENCE spatial.rasters_reference_reference_id_seq TO admin;
GRANT SELECT ON SEQUENCE spatial.rasters_reference_reference_id_seq TO yg_reader_group;


--
-- Name: TABLE spatial_ref_sys; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.spatial_ref_sys TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.spatial_ref_sys TO yg_editor_group;
GRANT SELECT ON TABLE spatial.spatial_ref_sys TO tkc_group;


--
-- Name: TABLE vectors; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT SELECT ON TABLE spatial.vectors TO public_reader;
GRANT ALL ON TABLE spatial.vectors TO admin;
GRANT SELECT ON TABLE spatial.vectors TO yg_reader_group;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE spatial.vectors TO yg_editor_group;
GRANT SELECT ON TABLE spatial.vectors TO tkc_group;


--
-- Name: SEQUENCE vectors_geom_id_seq; Type: ACL; Schema: spatial; Owner: postgres
--

GRANT ALL ON SEQUENCE spatial.vectors_geom_id_seq TO admin;
GRANT SELECT ON SEQUENCE spatial.vectors_geom_id_seq TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: application; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA application GRANT SELECT ON TABLES TO PUBLIC;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: application; Owner: admin
--

ALTER DEFAULT PRIVILEGES FOR ROLE admin IN SCHEMA application GRANT SELECT ON TABLES TO PUBLIC;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: boreholes; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA boreholes GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: continuous; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous GRANT SELECT ON SEQUENCES TO public_reader;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: continuous; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous GRANT ALL ON FUNCTIONS TO public_reader;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: continuous; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous GRANT SELECT ON TABLES TO public_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: discrete; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT SELECT ON SEQUENCES TO public_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT SELECT ON SEQUENCES TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: discrete; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT ALL ON FUNCTIONS TO public_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT ALL ON FUNCTIONS TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: discrete; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT SELECT ON TABLES TO public_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT SELECT ON TABLES TO yg_reader_group;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: files; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files GRANT SELECT ON SEQUENCES TO public_reader;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: files; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files GRANT SELECT ON TABLES TO public_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: information; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information GRANT SELECT ON SEQUENCES TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: information; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information GRANT ALL ON FUNCTIONS TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: information; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information GRANT SELECT ON TABLES TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: instruments; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments GRANT SELECT ON SEQUENCES TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: instruments; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments GRANT ALL ON FUNCTIONS TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: instruments; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments GRANT SELECT ON TABLES TO yg_reader_group;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT ON SEQUENCES TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON FUNCTIONS TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT ON TABLES TO public_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT ON TABLES TO yg_reader_group;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: spatial; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial GRANT SELECT ON SEQUENCES TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: spatial; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial GRANT ALL ON FUNCTIONS TO yg_reader_group;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: spatial; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial GRANT SELECT ON TABLES TO yg_reader_group;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO yg_editor_group;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: -; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL ON SEQUENCES TO admin;


--
-- Name: DEFAULT PRIVILEGES FOR FUNCTIONS; Type: DEFAULT ACL; Schema: -; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL ON FUNCTIONS TO admin;


--
-- Name: DEFAULT PRIVILEGES FOR SCHEMAS; Type: DEFAULT ACL; Schema: -; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL ON SCHEMAS TO admin;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: -; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL ON TABLES TO admin;


--
-- PostgreSQL database dump complete
--

