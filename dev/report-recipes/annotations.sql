-- Commentary is authored after generation as well as during it. It therefore
-- has its own append-only history, independent of the execution state machine.
CREATE TABLE application.report_run_annotations (
  report_run_annotation_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  report_run_id INTEGER NOT NULL REFERENCES application.report_runs(report_run_id),
  annotation_key TEXT NOT NULL,
  entity_key TEXT NOT NULL DEFAULT '',
  revision INTEGER NOT NULL,
  body TEXT NOT NULL,
  created_by TEXT NOT NULL DEFAULT CURRENT_USER,
  created TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
  UNIQUE (report_run_id, annotation_key, entity_key, revision),
  CHECK (annotation_key = btrim(annotation_key) AND annotation_key <> ''),
  CHECK (entity_key = btrim(entity_key)),
  CHECK (revision > 0)
);
ALTER TABLE application.report_run_annotations OWNER TO admin;
COMMENT ON TABLE application.report_run_annotations IS
  'Append-only human commentary. annotation_key references the recipe annotation id; entity_key is a stable source-qualified location/sample identifier, or empty for report/section notes. Empty body clears a note. Rendered outputs retain the exact annotation IDs used in their run manifest.';

CREATE FUNCTION application.validate_report_run_annotation()
RETURNS TRIGGER LANGUAGE plpgsql
SET search_path = pg_catalog, application
AS $function$
DECLARE
  recipe_doc JSONB;
  definition JSONB;
  next_revision INTEGER;
BEGIN
  -- Serialize appends for a run; updates to run state use the same row lock.
  SELECT r.recipe INTO recipe_doc
  FROM application.report_runs run
  JOIN application.report_recipe_revisions r USING (report_recipe_revision_id)
  WHERE run.report_run_id = NEW.report_run_id
    AND application.can_manage_report_role(run.requested_by_role)
  FOR UPDATE OF run;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Run is unavailable for annotation.' USING ERRCODE = '42501';
  END IF;
  SELECT e INTO definition FROM jsonb_array_elements(recipe_doc -> 'annotations') e
  WHERE e ->> 'id' = NEW.annotation_key;
  IF NOT FOUND OR
    (definition ->> 'scope' = 'entity' AND NEW.entity_key = '') OR
    (definition ->> 'scope' <> 'entity' AND NEW.entity_key <> '') THEN
    RAISE EXCEPTION 'Annotation key or entity scope is invalid.' USING ERRCODE = '23514';
  END IF;
  SELECT COALESCE(max(revision), 0) + 1 INTO next_revision
  FROM application.report_run_annotations
  WHERE report_run_id = NEW.report_run_id
    AND annotation_key = NEW.annotation_key AND entity_key = NEW.entity_key;
  IF NEW.revision IS NOT NULL AND NEW.revision <> next_revision THEN
    RAISE EXCEPTION 'Expected annotation revision %.', next_revision USING ERRCODE = '23514';
  END IF;
  NEW.revision := next_revision;
  NEW.created_by := CURRENT_USER;
  NEW.created := clock_timestamp();
  RETURN NEW;
END;
$function$;
ALTER FUNCTION application.validate_report_run_annotation() OWNER TO admin;
REVOKE ALL ON FUNCTION application.validate_report_run_annotation() FROM PUBLIC;
CREATE TRIGGER validate_report_run_annotation_trigger
BEFORE INSERT ON application.report_run_annotations
FOR EACH ROW EXECUTE FUNCTION application.validate_report_run_annotation();
CREATE TRIGGER prevent_report_run_annotation_change_trigger
BEFORE UPDATE OR DELETE ON application.report_run_annotations
FOR EACH ROW EXECUTE FUNCTION application.prevent_report_history_change();
CREATE TRIGGER audit_report_run_annotations_trigger
AFTER INSERT OR UPDATE OR DELETE ON application.report_run_annotations
FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func();
INSERT INTO audit.table_registry
  (schema_name, table_name, capture_mode, rationale, history_started_at, updated_at)
VALUES ('application', 'report_run_annotations', 'generic_insert_update_delete',
  'Versioned human commentary independent of immutable report execution history.',
  clock_timestamp(), clock_timestamp());
ALTER TABLE application.report_run_annotations ENABLE ROW LEVEL SECURITY;
ALTER TABLE application.report_run_annotations FORCE ROW LEVEL SECURITY;
CREATE POLICY report_run_annotations_select ON application.report_run_annotations
FOR SELECT USING (EXISTS (
  SELECT 1 FROM application.report_runs r
  WHERE r.report_run_id = report_run_annotations.report_run_id
));
CREATE POLICY report_run_annotations_insert ON application.report_run_annotations
FOR INSERT WITH CHECK (EXISTS (
  SELECT 1 FROM application.report_runs r
  WHERE r.report_run_id = report_run_annotations.report_run_id
    AND application.can_manage_report_role(r.requested_by_role)
));
REVOKE ALL ON application.report_run_annotations FROM PUBLIC;
GRANT SELECT, INSERT ON application.report_run_annotations TO PUBLIC;
REVOKE ALL ON SEQUENCE application.report_run_annotations_report_run_annotation_id_seq FROM PUBLIC;
GRANT USAGE ON SEQUENCE application.report_run_annotations_report_run_annotation_id_seq TO PUBLIC;

