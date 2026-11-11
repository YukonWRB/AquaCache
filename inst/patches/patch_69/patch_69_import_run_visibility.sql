-- Configuration snapshots may contain identifiers for every workbook sample.
-- Reading a public result must not disclose the rest of a restricted workbook.
ALTER TABLE discrete.import_runs OWNER TO admin;
ALTER TABLE discrete.import_runs ENABLE ROW LEVEL SECURITY;

DROP POLICY IF EXISTS import_run_read ON discrete.import_runs;
CREATE POLICY import_run_read ON discrete.import_runs FOR SELECT
USING (
  (source_document_id IS NOT NULL AND EXISTS (
    SELECT 1 FROM files.documents d WHERE d.document_id = source_document_id
  ))
  OR (source_document_id IS NULL AND created_by = CURRENT_USER)
);

DROP POLICY IF EXISTS import_run_insert ON discrete.import_runs;
CREATE POLICY import_run_insert ON discrete.import_runs FOR INSERT
WITH CHECK (
  created_by = CURRENT_USER
  AND (source_document_id IS NULL OR EXISTS (
    SELECT 1 FROM files.documents d WHERE d.document_id = source_document_id
  ))
);

DROP POLICY IF EXISTS import_run_update ON discrete.import_runs;
CREATE POLICY import_run_update ON discrete.import_runs FOR UPDATE
USING (created_by = CURRENT_USER)
WITH CHECK (created_by = CURRENT_USER);

COMMENT ON TABLE discrete.import_runs IS
'One import operation with an immutable configuration snapshot and archived source-document reference. Workbook snapshots are readable only when the source document is readable; documentless runs are private to their creator and administrators.';
