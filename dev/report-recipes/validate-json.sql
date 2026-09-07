-- Internal validator for the deliberately bounded JSON Schema vocabulary used
-- by schema-v2.json. Not a general JSON Schema implementation. Pure, recursive,
-- no database reads, remote reference resolution, or evaluation of recipe code.
CREATE FUNCTION application.report_json_matches(v JSONB, s JSONB, root JSONB)
RETURNS BOOLEAN
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE
SET search_path = pg_catalog, application
AS $function$
DECLARE
  entry RECORD;
  child JSONB;
  kind TEXT;
BEGIN
  IF v IS NULL OR s IS NULL THEN RETURN FALSE; END IF;
  IF s = 'true'::JSONB THEN RETURN TRUE; END IF;
  IF s = 'false'::JSONB THEN RETURN FALSE; END IF;
  IF jsonb_typeof(s) <> 'object' THEN RETURN FALSE; END IF;
  -- Fail closed if a later schema adds a keyword this implementation lacks.
  IF EXISTS (
    SELECT 1 FROM jsonb_object_keys(s) AS k(name)
    WHERE name NOT IN ('$schema', '$id', '$defs', '$ref', 'title', 'description',
      'type', 'const', 'enum', 'anyOf', 'allOf', 'if', 'then', 'required', 'properties',
      'additionalProperties', 'items', 'minItems', 'uniqueItems',
      'minLength', 'pattern', 'minimum', 'maximum')
  ) THEN RETURN FALSE; END IF;
  IF s ? '$ref' THEN
    IF s ->> '$ref' NOT LIKE '#/$defs/%' THEN RETURN FALSE; END IF;
    IF NOT application.report_json_matches(
      v, root #> ARRAY['$defs', substr(s ->> '$ref', 9)], root
    ) THEN RETURN FALSE; END IF;
  END IF;
  kind := jsonb_typeof(v);
  IF s ? 'type' THEN
    IF s ->> 'type' = 'integer' THEN
      IF kind <> 'number' THEN RETURN FALSE; END IF;
      IF (v::TEXT)::NUMERIC <> trunc((v::TEXT)::NUMERIC) THEN RETURN FALSE; END IF;
    ELSIF kind IS DISTINCT FROM s ->> 'type' THEN RETURN FALSE;
    END IF;
  END IF;
  IF s ? 'const' AND v IS DISTINCT FROM s -> 'const' THEN RETURN FALSE; END IF;
  IF s ? 'enum' AND NOT EXISTS (
    SELECT 1 FROM jsonb_array_elements(s -> 'enum') e WHERE e = v
  ) THEN RETURN FALSE; END IF;
  IF s ? 'anyOf' AND NOT EXISTS (
    SELECT 1 FROM jsonb_array_elements(s -> 'anyOf') a
    WHERE application.report_json_matches(v, a, root)
  ) THEN RETURN FALSE; END IF;
  IF s ? 'allOf' AND EXISTS (
    SELECT 1 FROM jsonb_array_elements(s -> 'allOf') a
    WHERE NOT application.report_json_matches(v, a, root)
  ) THEN RETURN FALSE; END IF;
  IF s ? 'if' AND application.report_json_matches(v, s -> 'if', root)
     AND s ? 'then' AND NOT application.report_json_matches(v, s -> 'then', root)
  THEN RETURN FALSE; END IF;
  IF kind = 'object' THEN
    IF EXISTS (
      SELECT 1 FROM jsonb_array_elements_text(s -> 'required') r
      WHERE NOT (v ? r)
    ) THEN RETURN FALSE; END IF;
    FOR entry IN SELECT * FROM jsonb_each(v) LOOP
      IF (s -> 'properties') ? entry.key THEN
        IF NOT application.report_json_matches(
          entry.value, s #> ARRAY['properties', entry.key], root
        ) THEN RETURN FALSE; END IF;
      ELSIF s ? 'additionalProperties' THEN
        IF NOT application.report_json_matches(
          entry.value, s -> 'additionalProperties', root
        ) THEN RETURN FALSE; END IF;
      END IF;
    END LOOP;
  ELSIF kind = 'array' THEN
    IF jsonb_array_length(v) < COALESCE((s ->> 'minItems')::INTEGER, 0)
    THEN RETURN FALSE; END IF;
    IF s ->> 'uniqueItems' = 'true' AND (
      SELECT count(*) <> count(DISTINCT e) FROM jsonb_array_elements(v) e
    ) THEN RETURN FALSE; END IF;
    IF s ? 'items' THEN
      FOR child IN SELECT * FROM jsonb_array_elements(v) LOOP
        IF NOT application.report_json_matches(child, s -> 'items', root)
        THEN RETURN FALSE; END IF;
      END LOOP;
    END IF;
  ELSIF kind = 'string' THEN
    IF length(v #>> '{}') < COALESCE((s ->> 'minLength')::INTEGER, 0)
       OR (s ? 'pattern' AND (v #>> '{}') !~ (s ->> 'pattern'))
    THEN RETURN FALSE; END IF;
  ELSIF kind = 'number' THEN
    IF (s ? 'minimum' AND (v::TEXT)::NUMERIC < (s ->> 'minimum')::NUMERIC)
       OR (s ? 'maximum' AND (v::TEXT)::NUMERIC > (s ->> 'maximum')::NUMERIC)
    THEN RETURN FALSE; END IF;
  END IF;
  RETURN TRUE;
END;
$function$;
