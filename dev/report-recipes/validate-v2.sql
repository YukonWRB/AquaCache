-- Structural validation is shared with the published JSON Schema. These checks
-- add identity and reference invariants which JSON Schema cannot express.
CREATE FUNCTION application.validate_report_recipe_v2(recipe JSONB, schema_doc JSONB)
RETURNS VOID
LANGUAGE plpgsql
SET search_path = pg_catalog, application
AS $function$
DECLARE
  part TEXT;
  item JSONB;
  param JSONB;
  input_id TEXT;
  available TEXT[] := ARRAY[]::TEXT[];
  expected_type TEXT;
BEGIN
  IF NOT application.report_json_matches(recipe, schema_doc, schema_doc) THEN
    RAISE EXCEPTION 'Recipe does not match aquacache-report version 2.'
      USING ERRCODE = '23514';
  END IF;
  FOREACH part IN ARRAY ARRAY['inputs', 'datasets', 'steps', 'criteria', 'sections', 'annotations'] LOOP
    IF EXISTS (
      SELECT 1 FROM jsonb_array_elements(recipe -> part) e
      GROUP BY e ->> 'id' HAVING count(*) > 1
    ) THEN
      RAISE EXCEPTION 'Duplicate id in recipe %.', part USING ERRCODE = '23514';
    END IF;
  END LOOP;
  FOR param IN SELECT * FROM jsonb_array_elements(recipe -> 'inputs') LOOP
    IF param ? 'default' AND NOT application.report_json_matches(
      param -> 'default', schema_doc #> ARRAY['$defs', 'value_' || (param ->> 'type')], schema_doc
    ) THEN
      RAISE EXCEPTION 'Invalid default for input %.', param ->> 'id' USING ERRCODE = '23514';
    END IF;
  END LOOP;
  FOR item IN SELECT * FROM jsonb_array_elements(recipe -> 'datasets') LOOP
    available := array_append(available, item ->> 'id');
    IF item #>> '{selection,mode}' = 'input' THEN
      SELECT e INTO param FROM jsonb_array_elements(recipe -> 'inputs') e
      WHERE e ->> 'id' = item #>> '{selection,input}';
      expected_type := CASE WHEN item #>> '{selection,input_field}' = 'locations'
        THEN 'string_array' ELSE 'integer_array' END;
      IF NOT FOUND OR NOT (item -> 'selection' ? 'input_field')
         OR param ->> 'type' <> expected_type THEN
        RAISE EXCEPTION 'Dataset % requires a declared selection input of the appropriate array type.', item ->> 'id'
          USING ERRCODE = '23514';
      END IF;
    ELSIF item -> 'selection' ? 'input' OR item -> 'selection' ? 'input_field' THEN
      RAISE EXCEPTION 'Selection input binding requires mode input.' USING ERRCODE = '23514';
    END IF;
    IF item #>> '{selection,mode}' = 'explicit' AND NOT (
      item -> 'selection' ?| ARRAY['locations', 'series_ids', 'sample_ids', 'licence_ids']
    ) THEN
      RAISE EXCEPTION 'Explicit selection requires identifiers.' USING ERRCODE = '23514';
    END IF;
    IF item #>> '{selection,mode}' <> 'explicit' AND (
      item -> 'selection' ?| ARRAY['locations', 'series_ids', 'sample_ids', 'licence_ids']
    ) THEN
      RAISE EXCEPTION 'Literal identifiers require explicit selection mode.' USING ERRCODE = '23514';
    END IF;
    IF item -> 'period' ? 'anchor' AND
      (item #>> '{period,start_offset_seconds}')::BIGINT >=
      (item #>> '{period,end_offset_seconds}')::BIGINT THEN
      RAISE EXCEPTION 'Period start must precede end.' USING ERRCODE = '23514';
    END IF;
    FOR input_id IN
      SELECT p.value ->> 'input' FROM jsonb_each(item -> 'period') p
      WHERE jsonb_typeof(p.value) = 'object'
    LOOP
      IF NOT EXISTS (SELECT 1 FROM jsonb_array_elements(recipe -> 'inputs') e
        WHERE e ->> 'id' = input_id AND e ->> 'type' = 'datetime') THEN
        RAISE EXCEPTION 'Period input % must be a declared datetime.', input_id USING ERRCODE = '23514';
      END IF;
    END LOOP;
  END LOOP;
  -- Array order is execution order: no dangling references, forward references,
  -- cycles, or dataset/step identity collisions.
  FOR item IN SELECT * FROM jsonb_array_elements(recipe -> 'steps') LOOP
    IF item ->> 'id' = ANY(available) OR EXISTS (
      SELECT 1 FROM jsonb_array_elements_text(item -> 'inputs') e
      WHERE NOT (e = ANY(available))
    ) THEN
      RAISE EXCEPTION 'Step % has a duplicate id or unavailable input.', item ->> 'id'
        USING ERRCODE = '23514';
    END IF;
    IF item #>> '{operation,code}' = 'aquacache.conditions' AND item #>> '{operation,version}' = '1'
       AND (item #>> '{options,baseline_start_year}')::INTEGER > (item #>> '{options,baseline_end_year}')::INTEGER THEN
      RAISE EXCEPTION 'Historical baseline years are reversed.' USING ERRCODE = '23514';
    END IF;
    IF item #>> '{operation,code}' = 'aquacache.exceedances' AND item #>> '{operation,version}' = '1' AND EXISTS (
      SELECT 1 FROM jsonb_array_elements_text(item #> '{options,criteria}') c
      WHERE NOT EXISTS (SELECT 1 FROM jsonb_array_elements(recipe -> 'criteria') e WHERE e ->> 'id' = c)
    ) THEN
      RAISE EXCEPTION 'Exceedance step references an unknown criterion.' USING ERRCODE = '23514';
    END IF;
    available := array_append(available, item ->> 'id');
  END LOOP;
  FOR item IN SELECT * FROM jsonb_array_elements(recipe -> 'criteria') LOOP
    IF item #>> '{provider,code}' = 'aquacache.criteria' AND item #>> '{provider,version}' = '1' THEN
      FOR input_id IN SELECT value FROM jsonb_each_text(item -> 'selection') WHERE key IN ('guideline_input', 'licence_input') LOOP
        IF NOT EXISTS (SELECT 1 FROM jsonb_array_elements(recipe -> 'inputs') e
          WHERE e ->> 'id' = input_id AND e ->> 'type' = 'integer_array') THEN
          RAISE EXCEPTION 'Criterion input % must be a declared integer array.', input_id USING ERRCODE = '23514';
        END IF;
      END LOOP;
    END IF;
  END LOOP;
  FOR item IN SELECT * FROM jsonb_array_elements(recipe -> 'sections') LOOP
    IF EXISTS (SELECT 1 FROM jsonb_array_elements_text(item -> 'datasets') e
      WHERE NOT (e = ANY(available))) THEN
      RAISE EXCEPTION 'Section % references unavailable data.', item ->> 'id'
        USING ERRCODE = '23514';
    END IF;
  END LOOP;
  FOR item IN SELECT * FROM jsonb_array_elements(recipe -> 'annotations') LOOP
    IF (item ->> 'scope' = 'report' AND item ? 'section') OR
       (item ->> 'scope' <> 'report' AND NOT (item ? 'section')) OR
       (item ? 'section' AND NOT EXISTS (
         SELECT 1 FROM jsonb_array_elements(recipe -> 'sections') e
         WHERE e ->> 'id' = item ->> 'section'
       )) THEN
      RAISE EXCEPTION 'Annotation % has an invalid section scope.', item ->> 'id'
        USING ERRCODE = '23514';
    END IF;
  END LOOP;
END;
$function$;
