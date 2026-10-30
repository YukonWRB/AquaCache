-- Installed by migrate_result_chemistry_dimensions() inside the patch transaction.
-- Catalogues describe analytical meaning. Result provenance remains in the
-- existing laboratory, protocol, import-run, and calculation relationships.

COMMENT ON TABLE discrete.reporting_bases IS
  'Numeric reporting expressions independent of units, chemical form, and analytical target. Conversion requires the analytical target. Retire entries instead of deleting historical references.';
COMMENT ON TABLE discrete.result_speciations IS
  'Chemical forms and explicit chemical-form scopes. A missing form does not mean all species. Do not put titration endpoints or reporting expressions here.';
COMMENT ON TABLE discrete.result_targets IS
  'Analyzed material or chemical pool, including isotope carrier materials and free/WAD/SAD/total cyanide pools. Preparation/recovery fractions such as acid soluble, extractable, and total recoverable belong in sample_fractions. This is analytical identity, not data provenance.';
COMMENT ON TABLE discrete.sample_fractions IS
  'The analyzed or prepared portion of a sample, including physical separation and operational recovery fractions such as dissolved, total, acid soluble, extractable, and total recoverable. It does not encode reporting basis, chemical species, isotope carrier material, or cyanide analytical pool.';

-- An ID must keep the same scientific meaning across all historical results.
-- Active status and translations remain editable; a new definition gets a new
-- entry, and the old entry can be retired without breaking its references.
CREATE FUNCTION discrete.guard_chemistry_catalogue_definition() RETURNS trigger
LANGUAGE plpgsql SET search_path = pg_catalog AS $function$
BEGIN
  IF (to_jsonb(NEW) - ARRAY[TG_ARGV[0] || '_fr', 'description_fr', 'active',
        'modified', 'modified_by', 'created', 'created_by']) IS DISTINCT FROM
     (to_jsonb(OLD) - ARRAY[TG_ARGV[0] || '_fr', 'description_fr', 'active',
        'modified', 'modified_by', 'created', 'created_by']) THEN
    RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE =
      'Analytical definitions are immutable. Create a new entry and retire the old entry.';
  END IF;
  RETURN NEW;
END;
$function$;
ALTER FUNCTION discrete.guard_chemistry_catalogue_definition() OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.guard_chemistry_catalogue_definition() FROM PUBLIC;
CREATE TRIGGER immutable_analytical_definition BEFORE UPDATE ON discrete.reporting_bases
FOR EACH ROW EXECUTE FUNCTION discrete.guard_chemistry_catalogue_definition('reporting_basis');
CREATE TRIGGER immutable_analytical_definition BEFORE UPDATE ON discrete.result_speciations
FOR EACH ROW EXECUTE FUNCTION discrete.guard_chemistry_catalogue_definition('result_speciation');
CREATE TRIGGER immutable_analytical_definition BEFORE UPDATE ON discrete.result_targets
FOR EACH ROW EXECUTE FUNCTION discrete.guard_chemistry_catalogue_definition('result_target');

-- These matching fields have the same meaning in results, mappings, guideline
-- applicability, and guideline inputs. An unspecified chemical identity matches
-- only an equally unspecified result. Never assume a missing form means all forms.
CREATE FUNCTION discrete.result_matches_chemistry(in_result_id INTEGER, chemistry JSONB)
RETURNS BOOLEAN LANGUAGE sql STABLE
SET search_path = pg_catalog, discrete, public
AS $function$
  SELECT COALESCE((
    SELECT
      (NOT p.reporting_basis OR chemistry->>'reporting_basis_id' IS NOT NULL)
      AND (NOT p.result_speciation OR chemistry->>'result_speciation_id' IS NOT NULL)
      AND (NOT p.result_target OR chemistry->>'result_target_id' IS NOT NULL)
      AND (NOT p.endpoint OR chemistry->>'endpoint_mode' IS NOT NULL)
      AND r.matrix_state_id = COALESCE((chemistry->>'matrix_state_id')::INTEGER,
        public.get_unique_parameter_matrix_state_id(r.parameter_id))
      AND (NOT chemistry ? 'sample_fraction_id' OR r.sample_fraction_id IS NOT DISTINCT FROM
        (chemistry->>'sample_fraction_id')::INTEGER)
      AND r.reporting_basis_id IS NOT DISTINCT FROM (chemistry->>'reporting_basis_id')::INTEGER
      AND r.result_speciation_id IS NOT DISTINCT FROM (chemistry->>'result_speciation_id')::INTEGER
      AND r.result_target_id IS NOT DISTINCT FROM (chemistry->>'result_target_id')::INTEGER
      AND r.endpoint_mode IS NOT DISTINCT FROM chemistry->>'endpoint_mode'
      AND r.endpoint_ph IS NOT DISTINCT FROM (chemistry->>'endpoint_ph')::NUMERIC
      AND r.analysis_conditions = COALESCE(chemistry->'analysis_conditions', '{}'::JSONB)
      AND NOT EXISTS (SELECT 1 FROM unnest(discrete.required_analysis_condition_keys(r.parameter_id, r.matrix_state_id)) AS needed(key)
        WHERE chemistry->'analysis_conditions'->key IS NULL OR
              chemistry->'analysis_conditions'->key = 'null'::JSONB)
    FROM discrete.results r JOIN public.parameters p USING (parameter_id)
    WHERE r.result_id = in_result_id
  ), FALSE);
$function$;
ALTER FUNCTION discrete.result_matches_chemistry(INTEGER, JSONB) OWNER TO admin;
GRANT EXECUTE ON FUNCTION discrete.result_matches_chemistry(INTEGER, JSONB) TO PUBLIC;

CREATE OR REPLACE FUNCTION discrete.enforce_result_speciation()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER
SET search_path = pg_catalog, public, discrete
AS $function$
DECLARE
  requirement RECORD;
  changed BOOLEAN;
  condition_key TEXT;
BEGIN
  SELECT p.* INTO requirement FROM public.parameters p
    WHERE p.parameter_id = NEW.parameter_id FOR SHARE;
  IF NOT FOUND THEN RETURN NEW; END IF;
  NEW.analysis_conditions := discrete.normalize_analysis_conditions(NEW.analysis_conditions);
  -- Historical unknowns remain unknown. Unrelated edits may preserve their
  -- analytical identity; inserts and analytical edits must satisfy requirements.
  changed := TG_OP = 'INSERT' OR
    ROW(NEW.parameter_id, NEW.matrix_state_id, NEW.sample_fraction_id,
        NEW.reporting_basis_id, NEW.result_speciation_id, NEW.result_target_id,
        NEW.endpoint_mode, NEW.endpoint_ph, NEW.analysis_conditions, NEW.protocol_method)
    IS DISTINCT FROM
    ROW(OLD.parameter_id, OLD.matrix_state_id, OLD.sample_fraction_id,
        OLD.reporting_basis_id, OLD.result_speciation_id, OLD.result_target_id,
        OLD.endpoint_mode, OLD.endpoint_ph, OLD.analysis_conditions, OLD.protocol_method);
  IF changed THEN
    IF requirement.sample_fraction AND NEW.sample_fraction_id IS NULL THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Sample fraction is required for the selected parameter.';
    END IF;
    IF requirement.reporting_basis AND NEW.reporting_basis_id IS NULL THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Reporting basis is required for the selected parameter.';
    END IF;
    IF requirement.result_speciation AND NEW.result_speciation_id IS NULL THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Chemical speciation is required for the selected parameter.';
    END IF;
    IF requirement.result_target AND NEW.result_target_id IS NULL THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Analytical target is required for the selected parameter.';
    END IF;
    IF requirement.endpoint AND NEW.endpoint_mode IS NULL THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Endpoint definition is required for the selected parameter.';
    END IF;
    FOREACH condition_key IN ARRAY discrete.required_analysis_condition_keys(NEW.parameter_id, NEW.matrix_state_id) LOOP
      IF NOT (NEW.analysis_conditions ? condition_key) OR NEW.analysis_conditions->condition_key = 'null'::JSONB THEN
        RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Required analysis condition: ' || condition_key;
      END IF;
    END LOOP;
  END IF;
  IF NEW.reporting_basis_id IS NOT NULL AND
     (TG_OP = 'INSERT' OR NEW.reporting_basis_id IS DISTINCT FROM OLD.reporting_basis_id) AND
     NOT EXISTS (SELECT 1 FROM discrete.reporting_bases WHERE reporting_basis_id = NEW.reporting_basis_id AND active) THEN
    RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Choose an active reporting basis.';
  END IF;
  IF NEW.result_speciation_id IS NOT NULL AND
     (TG_OP = 'INSERT' OR NEW.result_speciation_id IS DISTINCT FROM OLD.result_speciation_id) AND
     NOT EXISTS (SELECT 1 FROM discrete.result_speciations WHERE result_speciation_id = NEW.result_speciation_id AND active) THEN
    RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Choose an active chemical speciation.';
  END IF;
  IF NEW.result_target_id IS NOT NULL AND
     (TG_OP = 'INSERT' OR NEW.result_target_id IS DISTINCT FROM OLD.result_target_id) AND
     NOT EXISTS (SELECT 1 FROM discrete.result_targets WHERE result_target_id = NEW.result_target_id AND active) THEN
    RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Choose an active analytical target.';
  END IF;
  RETURN NEW;
END;
$function$;
ALTER FUNCTION discrete.enforce_result_speciation() OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.enforce_result_speciation() FROM PUBLIC;

-- Retain the existing name so the historical trigger stays attached throughout
-- the representation migration. Recreate the trigger with all typed fields.
DROP TRIGGER IF EXISTS trg_enforce_result_speciation ON discrete.results;
CREATE TRIGGER trg_enforce_result_speciation BEFORE INSERT OR UPDATE ON discrete.results
FOR EACH ROW EXECUTE FUNCTION discrete.enforce_result_speciation();
-- The shared guard above also validates fraction requiredness and allows
-- unrelated edits to historical unknowns. The old unconditional fraction
-- trigger would otherwise reject those edits a second time.
DROP TRIGGER IF EXISTS trg_enforce_sample_fraction ON discrete.results;

-- Share the requiredness definition between row edits and whole-set publication.
CREATE FUNCTION discrete.import_parameter_mapping_error(mapping_row discrete.import_parameter_mappings)
RETURNS TEXT LANGUAGE plpgsql STABLE SECURITY DEFINER
SET search_path = pg_catalog, public, discrete
AS $function$
DECLARE
  requirement RECORD;
  missing_dimension TEXT;
  condition_key TEXT;
BEGIN
  SELECT p.* INTO requirement FROM public.parameters p
    WHERE p.parameter_id = mapping_row.parameter_id;
  IF NOT FOUND THEN RETURN 'Parameter'; END IF;
  missing_dimension := CASE
    WHEN requirement.sample_fraction AND mapping_row.sample_fraction_id IS NULL THEN 'Sample fraction'
    WHEN requirement.reporting_basis AND mapping_row.reporting_basis_id IS NULL THEN 'Reporting basis'
    WHEN requirement.result_speciation AND mapping_row.result_speciation_id IS NULL THEN 'Chemical speciation'
    WHEN requirement.result_target AND mapping_row.result_target_id IS NULL THEN 'Analytical target'
    WHEN requirement.endpoint AND mapping_row.endpoint_mode IS NULL THEN 'Endpoint definition'
  END;
  IF mapping_row.matrix_state_id IS NULL AND public.get_unique_parameter_matrix_state_id(mapping_row.parameter_id) IS NULL
    AND requirement.analysis_conditions_required_keys_by_matrix <> '{}'::JSONB THEN missing_dimension := 'Matrix state'; END IF;
  FOREACH condition_key IN ARRAY discrete.required_analysis_condition_keys(mapping_row.parameter_id, mapping_row.matrix_state_id) LOOP
    IF NOT (mapping_row.analysis_conditions ? condition_key) OR mapping_row.analysis_conditions->condition_key = 'null'::JSONB THEN
      missing_dimension := 'Analysis condition ' || condition_key;
      EXIT;
    END IF;
  END LOOP;
  IF mapping_row.active AND (
    (mapping_row.reporting_basis_id IS NOT NULL AND NOT EXISTS (SELECT 1 FROM discrete.reporting_bases WHERE reporting_basis_id = mapping_row.reporting_basis_id AND active)) OR
    (mapping_row.result_speciation_id IS NOT NULL AND NOT EXISTS (SELECT 1 FROM discrete.result_speciations WHERE result_speciation_id = mapping_row.result_speciation_id AND active)) OR
    (mapping_row.result_target_id IS NOT NULL AND NOT EXISTS (SELECT 1 FROM discrete.result_targets WHERE result_target_id = mapping_row.result_target_id AND active))) THEN
    missing_dimension := COALESCE(missing_dimension, 'Active chemistry catalogue selection');
  END IF;
  RETURN missing_dimension;
END;
$function$;
ALTER FUNCTION discrete.import_parameter_mapping_error(discrete.import_parameter_mappings) OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.import_parameter_mapping_error(discrete.import_parameter_mappings) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION discrete.import_parameter_mapping_error(discrete.import_parameter_mappings) TO PUBLIC;

CREATE OR REPLACE FUNCTION discrete.enforce_import_parameter_mapping_requirements()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER
SET search_path = pg_catalog, public, discrete
AS $function$
DECLARE missing_dimension TEXT;
BEGIN
  PERFORM 1 FROM public.parameters WHERE parameter_id = NEW.parameter_id FOR SHARE;
  -- Unassigned targets may be staged for manual mapping. Publication rejects
  -- them when active; foreign keys still reject a nonexistent non-NULL ID.
  IF NOT FOUND THEN RETURN NEW; END IF;
  NEW.analysis_conditions := discrete.normalize_analysis_conditions(NEW.analysis_conditions);
  missing_dimension := discrete.import_parameter_mapping_error(NEW);
  IF missing_dimension IS NOT NULL THEN
    -- Creating a draft copies immutable historical evidence. Permit exactly
    -- unchanged legacy rows, never new or altered incomplete active mappings.
    IF TG_OP = 'INSERT' AND EXISTS (
      SELECT 1 FROM discrete.import_mapping_sets current_set
      JOIN discrete.import_mapping_sets previous_set
        ON previous_set.import_source_id = current_set.import_source_id
       AND previous_set.import_profile_id IS NOT DISTINCT FROM current_set.import_profile_id
       AND previous_set.status IN ('published', 'retired') AND previous_set.version < current_set.version
      JOIN discrete.import_parameter_mappings previous_mapping
        ON previous_mapping.import_mapping_set_id = previous_set.import_mapping_set_id
      WHERE current_set.import_mapping_set_id = NEW.import_mapping_set_id AND current_set.status = 'draft'
        AND (to_jsonb(previous_mapping) - ARRAY['import_mapping_id', 'import_mapping_set_id',
             'created', 'modified', 'created_by', 'modified_by', 'analysis_conditions_key']) =
            (to_jsonb(NEW) - ARRAY['import_mapping_id', 'import_mapping_set_id',
             'created', 'modified', 'created_by', 'modified_by', 'analysis_conditions_key'])
    ) THEN RETURN NEW; END IF;
    IF TG_OP = 'UPDATE' AND NEW.active IS FALSE AND
      (to_jsonb(NEW) - ARRAY['active', 'modified', 'modified_by', 'analysis_conditions_key']) =
      (to_jsonb(OLD) - ARRAY['active', 'modified', 'modified_by', 'analysis_conditions_key']) THEN RETURN NEW; END IF;
    RAISE EXCEPTION USING ERRCODE = '23514',
      MESSAGE = missing_dimension || ' is required for the selected parameter.';
  END IF;
  RETURN NEW;
END;
$function$;
ALTER FUNCTION discrete.enforce_import_parameter_mapping_requirements() OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.enforce_import_parameter_mapping_requirements() FROM PUBLIC;
DROP TRIGGER IF EXISTS import_parameter_mapping_required_descriptors ON discrete.import_parameter_mappings;
CREATE TRIGGER import_parameter_mapping_required_descriptors
BEFORE INSERT OR UPDATE ON discrete.import_parameter_mappings
FOR EACH ROW EXECUTE FUNCTION discrete.enforce_import_parameter_mapping_requirements();

CREATE FUNCTION discrete.assert_import_mapping_set_complete(in_mapping_set_id INTEGER)
RETURNS VOID LANGUAGE plpgsql SECURITY DEFINER
SET search_path = pg_catalog, public, discrete
AS $function$
DECLARE errors TEXT;
BEGIN
  -- Serialize publication with child edits and parameter-policy changes.
  PERFORM 1 FROM discrete.import_mapping_sets
    WHERE import_mapping_set_id = in_mapping_set_id FOR UPDATE;
  PERFORM 1 FROM public.parameters p WHERE p.parameter_id IN (
    SELECT m.parameter_id FROM discrete.import_parameter_mappings m
    WHERE m.import_mapping_set_id = in_mapping_set_id AND m.active)
    ORDER BY p.parameter_id FOR SHARE;
  SELECT string_agg(format('%s (%s: %s)', m.import_mapping_id,
    COALESCE(p.param_name, 'unassigned parameter'), problem.message), '; ' ORDER BY m.import_mapping_id)
  INTO errors FROM discrete.import_parameter_mappings m
  LEFT JOIN public.parameters p USING (parameter_id)
  CROSS JOIN LATERAL (SELECT discrete.import_parameter_mapping_error(m) AS message) problem
  WHERE m.import_mapping_set_id = in_mapping_set_id AND m.active AND problem.message IS NOT NULL;
  IF errors IS NOT NULL THEN
    RAISE EXCEPTION USING ERRCODE = '23514', CONSTRAINT = 'published_mapping_set_complete',
      MESSAGE = 'Cannot publish incomplete active import mappings: ' || errors;
  END IF;
END;
$function$;
ALTER FUNCTION discrete.assert_import_mapping_set_complete(INTEGER) OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.assert_import_mapping_set_complete(INTEGER) FROM PUBLIC;
GRANT EXECUTE ON FUNCTION discrete.assert_import_mapping_set_complete(INTEGER) TO admin;

CREATE OR REPLACE FUNCTION discrete.guard_import_mapping_set()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, discrete
AS $function$
BEGIN
  IF TG_OP = 'INSERT' THEN
    IF NEW.status <> 'draft' THEN RAISE EXCEPTION 'New mapping sets must begin as drafts.'; END IF;
    RETURN NEW;
  END IF;
  IF TG_OP = 'DELETE' THEN
    IF OLD.status <> 'draft' THEN RAISE EXCEPTION 'Published or retired mapping sets cannot be deleted.'; END IF;
    RETURN OLD;
  END IF;
  IF ROW(NEW.import_source_id, NEW.import_profile_id, NEW.version) IS DISTINCT FROM
     ROW(OLD.import_source_id, OLD.import_profile_id, OLD.version) THEN
    RAISE EXCEPTION 'A mapping set scope and version cannot be changed.';
  END IF;
  IF NOT ((OLD.status = 'draft' AND NEW.status = 'published') OR
          (OLD.status = 'published' AND NEW.status = 'retired')) THEN
    RAISE EXCEPTION 'Allowed mapping-set transitions are draft to published and published to retired.';
  END IF;
  IF NEW.status = 'published' THEN
    PERFORM discrete.assert_import_mapping_set_complete(NEW.import_mapping_set_id);
  END IF;
  RETURN NEW;
END;
$function$;
ALTER FUNCTION discrete.guard_import_mapping_set() OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.guard_import_mapping_set() FROM PUBLIC;

CREATE OR REPLACE FUNCTION discrete.guard_import_mapping_row()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER SET search_path = pg_catalog, discrete
AS $function$
DECLARE old_status TEXT; new_status TEXT; old_set INTEGER; new_set INTEGER;
BEGIN
  IF TG_OP IN ('UPDATE','DELETE') THEN old_set := OLD.import_mapping_set_id; END IF;
  IF TG_OP IN ('INSERT','UPDATE') THEN new_set := NEW.import_mapping_set_id; END IF;
  -- Publication and child edits inspect status while holding conflicting locks.
  PERFORM 1 FROM discrete.import_mapping_sets WHERE import_mapping_set_id IN (old_set, new_set)
    ORDER BY import_mapping_set_id FOR SHARE;
  IF old_set IS NOT NULL THEN
    SELECT status INTO old_status FROM discrete.import_mapping_sets WHERE import_mapping_set_id = old_set;
    IF old_status IS DISTINCT FROM 'draft' THEN RAISE EXCEPTION 'Mapping rows can only be changed in a draft mapping set.'; END IF;
  END IF;
  IF new_set IS NOT NULL THEN
    SELECT status INTO new_status FROM discrete.import_mapping_sets WHERE import_mapping_set_id = new_set;
    IF new_status IS DISTINCT FROM 'draft' THEN RAISE EXCEPTION 'Mapping rows can only be written to a draft mapping set.'; END IF;
  END IF;
  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END;
$function$;
ALTER FUNCTION discrete.guard_import_mapping_row() OWNER TO admin;
REVOKE ALL ON FUNCTION discrete.guard_import_mapping_row() FROM PUBLIC;

CREATE OR REPLACE FUNCTION public.enforce_parameter_import_mapping_requirements()
RETURNS trigger LANGUAGE plpgsql SECURITY DEFINER
SET search_path = pg_catalog, public, discrete
AS $function$
DECLARE
  dimension TEXT;
  dimensions TEXT[] := ARRAY['sample_fraction', 'reporting_basis', 'result_speciation', 'result_target', 'endpoint'];
  column_name TEXT;
  missing_mapping BOOLEAN;
  condition_key TEXT;
BEGIN
  FOREACH dimension IN ARRAY dimensions LOOP
    IF COALESCE((to_jsonb(NEW)->>dimension)::BOOLEAN, FALSE) AND
       NOT COALESCE((to_jsonb(OLD)->>dimension)::BOOLEAN, FALSE) THEN
      column_name := CASE WHEN dimension = 'endpoint' THEN 'endpoint_mode' ELSE dimension || '_id' END;
      EXECUTE format('SELECT EXISTS (
        SELECT 1 FROM discrete.import_parameter_mappings m
        JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
        WHERE m.parameter_id = $1 AND m.active AND s.status IN (''draft'', ''published'') AND m.%I IS NULL)', column_name)
        INTO missing_mapping USING NEW.parameter_id;
      IF missing_mapping THEN
        RAISE EXCEPTION USING ERRCODE = '23514',
          MESSAGE = 'Cannot require ' || dimension || ' while an active import mapping omits it.';
      END IF;
    END IF;
  END LOOP;
  FOREACH condition_key IN ARRAY NEW.analysis_conditions_required_keys LOOP
    IF condition_key IS NULL OR btrim(condition_key) = '' THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Required analysis condition keys cannot be empty.';
    END IF;
    IF NOT condition_key = ANY(OLD.analysis_conditions_required_keys) AND EXISTS (
      SELECT 1 FROM discrete.import_parameter_mappings m
      JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
      WHERE m.parameter_id = NEW.parameter_id AND m.active AND s.status IN ('draft', 'published')
        AND (NOT m.analysis_conditions ? condition_key OR m.analysis_conditions->condition_key = 'null'::JSONB)) THEN
      RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Cannot require analysis condition ' || condition_key || ' while an active mapping omits it.';
    END IF;
  END LOOP;
  IF NEW.analysis_conditions_required_keys_by_matrix IS DISTINCT FROM OLD.analysis_conditions_required_keys_by_matrix AND EXISTS (
    SELECT 1 FROM discrete.import_parameter_mappings m JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
    LEFT JOIN public.matrix_states ms ON ms.matrix_state_id = COALESCE(m.matrix_state_id,
      public.get_unique_parameter_matrix_state_id(NEW.parameter_id))
    WHERE m.parameter_id = NEW.parameter_id AND m.active AND s.status IN ('draft', 'published') AND (
      (ms.matrix_state_id IS NULL AND NEW.analysis_conditions_required_keys_by_matrix <> '{}'::JSONB) OR
      EXISTS (SELECT 1 FROM jsonb_array_elements_text(COALESCE(
        NEW.analysis_conditions_required_keys_by_matrix->ms.matrix_state_code, '[]'::JSONB)) AS needed(key)
        WHERE NOT m.analysis_conditions ? key OR m.analysis_conditions->key = 'null'::JSONB))) THEN
    RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'Active import mappings must satisfy analysis conditions for their matrix state.';
  END IF;
  RETURN NEW;
END;
$function$;
ALTER FUNCTION public.enforce_parameter_import_mapping_requirements() OWNER TO admin;
REVOKE ALL ON FUNCTION public.enforce_parameter_import_mapping_requirements() FROM PUBLIC;
DROP TRIGGER IF EXISTS parameters_import_mapping_required_descriptors ON public.parameters;
CREATE TRIGGER parameters_import_mapping_required_descriptors
BEFORE UPDATE OF sample_fraction, reporting_basis, result_speciation, result_target, endpoint, analysis_conditions_required_keys, analysis_conditions_required_keys_by_matrix
ON public.parameters FOR EACH ROW
EXECUTE FUNCTION public.enforce_parameter_import_mapping_requirements();

COMMENT ON COLUMN public.parameters.reporting_basis IS 'Whether new or analytically changed results require a numeric reporting expression.';
COMMENT ON COLUMN public.parameters.result_speciation IS 'Whether new or analytically changed results require an explicit chemical form or chemical-form scope.';
COMMENT ON COLUMN public.parameters.result_target IS 'Whether new or analytically changed results require an analyzed material or operational chemical pool.';
COMMENT ON COLUMN public.parameters.endpoint IS 'Whether new or analytically changed results require a recorded endpoint determination. A fixed endpoint additionally requires endpoint_ph.';
COMMENT ON COLUMN public.parameters.analysis_conditions_required_keys IS
  'Analysis conditions required for new or analytically changed results. Keys describe numeric interpretation, such as mass_basis, gas_basis, reference_temperature_kelvin, reference_pressure_kpa, or isotope_reference_standard. Do not use source/laboratory provenance here.';
COMMENT ON COLUMN public.parameters.analysis_conditions_required_keys_by_matrix IS
  'Additional required analysis conditions by matrix-state code. For example {"solid":["mass_basis"],"gas":["gas_basis"]} can require solid/gas context without making it mandatory for liquid results. Values are arrays of condition keys; global required keys are also applied.';

-- This helper returns mg/L as CaCO3 for liquid samples. It never treats another
-- reporting expression, endpoint, solid-mass basis, or gas reference condition
-- as compatible merely because the parameter name is hardness/calcium/magnesium.
CREATE FUNCTION criteria.get_sample_hardness(in_sample_id INTEGER, in_result_type_preference INTEGER[], in_sample_fraction_id INTEGER)
RETURNS NUMERIC LANGUAGE sql STABLE SET search_path = pg_catalog, public, discrete
AS $function$
  WITH eligible AS (
    SELECT r.*, lower(p.param_name) AS parameter, lower(sf.sample_fraction) AS fraction,
      row_number() OVER (PARTITION BY p.parameter_id, r.sample_fraction_id ORDER BY
        COALESCE(array_position(in_result_type_preference, r.result_type), 1), r.result_id) AS choice,
      count(*) OVER (PARTITION BY p.parameter_id, r.sample_fraction_id,
        COALESCE(array_position(in_result_type_preference, r.result_type), 1)) AS alternatives
    FROM discrete.results r
    JOIN public.parameters p USING (parameter_id)
    JOIN public.matrix_states ms USING (matrix_state_id)
    JOIN public.units u ON u.unit_id = p.units_liquid
    JOIN discrete.sample_fractions sf USING (sample_fraction_id)
    LEFT JOIN discrete.reporting_bases b USING (reporting_basis_id)
    WHERE r.sample_id = in_sample_id AND r.result IS NOT NULL AND r.result_condition IS NULL
      AND ms.matrix_state_code = 'liquid' AND lower(u.unit_name) = 'mg/l'
      AND lower(sf.sample_fraction) IN ('dissolved', 'total')
      AND (in_sample_fraction_id IS NULL OR r.sample_fraction_id = in_sample_fraction_id)
      AND (cardinality(in_result_type_preference) IS NULL OR cardinality(in_result_type_preference) = 0
           OR r.result_type = ANY(in_result_type_preference))
      AND r.result_speciation_id IS NULL AND r.result_target_id IS NULL
      AND r.endpoint_mode IS NULL AND r.analysis_conditions = '{}'::JSONB
      AND (
        (lower(p.param_name) = 'hardness' AND b.reporting_basis_code = 'as_caco3') OR
        (lower(p.param_name) = 'calcium' AND (b.reporting_basis_code = 'as_ca' OR
           (r.reporting_basis_id IS NULL AND NOT p.reporting_basis))) OR
        (lower(p.param_name) = 'magnesium' AND (b.reporting_basis_code = 'as_mg' OR
           (r.reporting_basis_id IS NULL AND NOT p.reporting_basis)))
      )
  ), selected AS (
    SELECT * FROM eligible WHERE choice = 1 AND alternatives = 1
  ), values AS (
    SELECT fraction,
      max(result) FILTER (WHERE parameter = 'calcium') AS calcium,
      max(result) FILTER (WHERE parameter = 'magnesium') AS magnesium,
      max(result) FILTER (WHERE parameter = 'hardness') AS hardness
    FROM selected GROUP BY fraction
  ), candidates AS (
    SELECT fraction, CASE WHEN calcium > 0 AND magnesium > 0
      THEN 2.497 * calcium + 4.118 * magnesium ELSE hardness END AS hardness
    FROM values
  )
  SELECT hardness FROM candidates WHERE hardness > 0
  ORDER BY CASE fraction WHEN 'dissolved' THEN 1 ELSE 2 END LIMIT 1;
$function$;
CREATE OR REPLACE FUNCTION criteria.get_sample_hardness(in_sample_id INTEGER, in_result_type_preference INTEGER[])
RETURNS NUMERIC LANGUAGE sql STABLE SET search_path = pg_catalog, criteria
AS $function$ SELECT criteria.get_sample_hardness(in_sample_id, in_result_type_preference, NULL::INTEGER) $function$;
CREATE OR REPLACE FUNCTION criteria.get_sample_hardness(in_sample_id INTEGER)
RETURNS NUMERIC LANGUAGE sql STABLE SET search_path = pg_catalog, criteria
AS $function$ SELECT criteria.get_sample_hardness(in_sample_id, NULL::INTEGER[]) $function$;
ALTER FUNCTION criteria.get_sample_hardness(INTEGER, INTEGER[]) OWNER TO admin;
ALTER FUNCTION criteria.get_sample_hardness(INTEGER, INTEGER[], INTEGER) OWNER TO admin;
ALTER FUNCTION criteria.get_sample_hardness(INTEGER) OWNER TO admin;
