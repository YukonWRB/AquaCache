# Install database-maintained metadata for raster series.
#
# This patch-local helper is used by patch 59 and by the one-time dev
# remediation script after patch 59 has already been applied.
install_raster_series_metadata_triggers <- function(con) {
  required <- DBI::dbGetQuery(
    con,
    "SELECT
       to_regclass('spatial.raster_series_index') IS NOT NULL AS has_series,
       to_regclass('spatial.rasters_reference') IS NOT NULL AS has_references"
  )
  if (!all(unlist(required[1, ], use.names = FALSE))) {
    stop(
      "Raster series metadata triggers require spatial.raster_series_index and spatial.rasters_reference."
    )
  }

  statements <- c(
    "ALTER TABLE spatial.raster_series_index
       ALTER COLUMN start_datetime DROP NOT NULL,
       ALTER COLUMN end_datetime DROP NOT NULL,
       ALTER COLUMN last_new_raster DROP NOT NULL",
    "COMMENT ON COLUMN spatial.raster_series_index.start_datetime IS
       'Earliest valid_from in spatial.rasters_reference for this series; maintained by database triggers.'",
    "COMMENT ON COLUMN spatial.raster_series_index.end_datetime IS
       'Latest valid_to in spatial.rasters_reference for this series; maintained by database triggers.'",
    "COMMENT ON COLUMN spatial.raster_series_index.last_issue IS
       'Latest issued datetime in spatial.rasters_reference for this series; maintained by database triggers.'",
    "COMMENT ON COLUMN spatial.raster_series_index.last_new_raster IS
       'Database time of the most recent raster reference insert for this series; maintained by database triggers.'",
    "CREATE OR REPLACE FUNCTION spatial.refresh_raster_series_metadata(
       p_raster_series_ids INTEGER[]
     )
     RETURNS void
     LANGUAGE sql
     AS $function$
       WITH ids AS (
         SELECT DISTINCT x.raster_series_id
         FROM unnest(p_raster_series_ids) AS x(raster_series_id)
         WHERE x.raster_series_id IS NOT NULL
       ),
       metadata AS (
         SELECT
           ids.raster_series_id,
           MIN(rr.valid_from) AS start_datetime,
           MAX(rr.valid_to) AS end_datetime,
           MAX(rr.issued) AS last_issue
         FROM ids
         LEFT JOIN spatial.rasters_reference rr
           ON rr.raster_series_id = ids.raster_series_id
         GROUP BY ids.raster_series_id
       )
       UPDATE spatial.raster_series_index rsi
       SET
         start_datetime = metadata.start_datetime,
         end_datetime = metadata.end_datetime,
         last_issue = metadata.last_issue
       FROM metadata
       WHERE rsi.raster_series_id = metadata.raster_series_id
         AND (
           rsi.start_datetime IS DISTINCT FROM metadata.start_datetime OR
           rsi.end_datetime IS DISTINCT FROM metadata.end_datetime OR
           rsi.last_issue IS DISTINCT FROM metadata.last_issue
         );
     $function$",
    "COMMENT ON FUNCTION spatial.refresh_raster_series_metadata(INTEGER[]) IS
       'Recomputes start_datetime, end_datetime, and last_issue exactly for the supplied raster series from spatial.rasters_reference.'",
    "CREATE OR REPLACE FUNCTION spatial.refresh_raster_series_metadata_on_insert()
     RETURNS trigger
     LANGUAGE plpgsql
     AS $function$
     BEGIN
       WITH inserted_metadata AS (
         SELECT
           nr.raster_series_id,
           MIN(nr.valid_from) AS start_datetime,
           MAX(nr.valid_to) AS end_datetime,
           MAX(nr.issued) AS last_issue
         FROM new_rows nr
         WHERE nr.raster_series_id IS NOT NULL
         GROUP BY nr.raster_series_id
       )
       UPDATE spatial.raster_series_index rsi
       SET
         start_datetime = CASE
           WHEN rsi.start_datetime IS NULL THEN inserted_metadata.start_datetime
           WHEN inserted_metadata.start_datetime IS NULL THEN rsi.start_datetime
           ELSE LEAST(rsi.start_datetime, inserted_metadata.start_datetime)
         END,
         end_datetime = CASE
           WHEN rsi.end_datetime IS NULL THEN inserted_metadata.end_datetime
           WHEN inserted_metadata.end_datetime IS NULL THEN rsi.end_datetime
           ELSE GREATEST(rsi.end_datetime, inserted_metadata.end_datetime)
         END,
         last_issue = CASE
           WHEN rsi.last_issue IS NULL THEN inserted_metadata.last_issue
           WHEN inserted_metadata.last_issue IS NULL THEN rsi.last_issue
           ELSE GREATEST(rsi.last_issue, inserted_metadata.last_issue)
         END,
         last_new_raster = CURRENT_TIMESTAMP
       FROM inserted_metadata
       WHERE rsi.raster_series_id = inserted_metadata.raster_series_id;

       RETURN NULL;
     END;
     $function$",
    "COMMENT ON FUNCTION spatial.refresh_raster_series_metadata_on_insert() IS
       'After-insert statement trigger function that incrementally extends raster series metadata and records last_new_raster.'",
    "CREATE OR REPLACE FUNCTION spatial.refresh_raster_series_metadata_on_delete()
     RETURNS trigger
     LANGUAGE plpgsql
     AS $function$
     DECLARE
       v_raster_series_ids INTEGER[];
     BEGIN
       SELECT ARRAY_AGG(DISTINCT old_row.raster_series_id)
       INTO v_raster_series_ids
       FROM old_rows old_row
       WHERE old_row.raster_series_id IS NOT NULL;

       IF array_length(v_raster_series_ids, 1) IS NOT NULL THEN
         PERFORM spatial.refresh_raster_series_metadata(v_raster_series_ids);
       END IF;

       RETURN NULL;
     END;
     $function$",
    "COMMENT ON FUNCTION spatial.refresh_raster_series_metadata_on_delete() IS
       'After-delete statement trigger function that recomputes raster series bounds and latest issue for affected series.'",
    "CREATE OR REPLACE FUNCTION spatial.refresh_raster_series_metadata_on_update()
     RETURNS trigger
     LANGUAGE plpgsql
     AS $function$
     DECLARE
       v_raster_series_ids INTEGER[];
     BEGIN
       SELECT ARRAY_AGG(DISTINCT changed.raster_series_id)
       INTO v_raster_series_ids
       FROM (
         SELECT old_row.raster_series_id
         FROM old_rows old_row
         JOIN new_rows new_row USING (reference_id)
         WHERE
           old_row.raster_series_id IS DISTINCT FROM new_row.raster_series_id OR
           old_row.valid_from IS DISTINCT FROM new_row.valid_from OR
           old_row.valid_to IS DISTINCT FROM new_row.valid_to OR
           old_row.issued IS DISTINCT FROM new_row.issued
         UNION
         SELECT new_row.raster_series_id
         FROM old_rows old_row
         JOIN new_rows new_row USING (reference_id)
         WHERE
           old_row.raster_series_id IS DISTINCT FROM new_row.raster_series_id OR
           old_row.valid_from IS DISTINCT FROM new_row.valid_from OR
           old_row.valid_to IS DISTINCT FROM new_row.valid_to OR
           old_row.issued IS DISTINCT FROM new_row.issued
       ) changed
       WHERE changed.raster_series_id IS NOT NULL;

       IF array_length(v_raster_series_ids, 1) IS NOT NULL THEN
         PERFORM spatial.refresh_raster_series_metadata(v_raster_series_ids);
       END IF;

       RETURN NULL;
     END;
     $function$",
    "COMMENT ON FUNCTION spatial.refresh_raster_series_metadata_on_update() IS
       'After-update statement trigger function that recomputes raster series metadata when a reference changes series or datetime metadata.'",
    "DROP TRIGGER IF EXISTS refresh_raster_series_metadata_on_insert_tr
       ON spatial.rasters_reference",
    "CREATE TRIGGER refresh_raster_series_metadata_on_insert_tr
       AFTER INSERT ON spatial.rasters_reference
       REFERENCING NEW TABLE AS new_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION spatial.refresh_raster_series_metadata_on_insert()",
    "DROP TRIGGER IF EXISTS refresh_raster_series_metadata_on_delete_tr
       ON spatial.rasters_reference",
    "CREATE TRIGGER refresh_raster_series_metadata_on_delete_tr
       AFTER DELETE ON spatial.rasters_reference
       REFERENCING OLD TABLE AS old_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION spatial.refresh_raster_series_metadata_on_delete()",
    "DROP TRIGGER IF EXISTS refresh_raster_series_metadata_on_update_tr
       ON spatial.rasters_reference",
    "CREATE TRIGGER refresh_raster_series_metadata_on_update_tr
       AFTER UPDATE ON spatial.rasters_reference
       REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
       FOR EACH STATEMENT
       EXECUTE FUNCTION spatial.refresh_raster_series_metadata_on_update()",
    "COMMENT ON TRIGGER refresh_raster_series_metadata_on_insert_tr
       ON spatial.rasters_reference IS
       'Maintains raster series metadata after raster reference inserts.'",
    "COMMENT ON TRIGGER refresh_raster_series_metadata_on_delete_tr
       ON spatial.rasters_reference IS
       'Maintains raster series metadata after raster reference deletes.'",
    "COMMENT ON TRIGGER refresh_raster_series_metadata_on_update_tr
       ON spatial.rasters_reference IS
       'Maintains raster series metadata after raster reference updates.'"
  )

  for (statement in statements) {
    DBI::dbExecute(con, statement)
  }

  DBI::dbGetQuery(
    con,
    "SELECT spatial.refresh_raster_series_metadata(
       ARRAY(
         SELECT raster_series_id
         FROM spatial.raster_series_index
       )
     )"
  )

  verification <- DBI::dbGetQuery(
    con,
    "WITH expected AS (
       SELECT
         rsi.raster_series_id,
         MIN(rr.valid_from) AS start_datetime,
         MAX(rr.valid_to) AS end_datetime,
         MAX(rr.issued) AS last_issue
       FROM spatial.raster_series_index rsi
       LEFT JOIN spatial.rasters_reference rr USING (raster_series_id)
       GROUP BY rsi.raster_series_id
     ),
     metadata_columns AS (
       SELECT
         COUNT(*) FILTER (WHERE attnotnull) = 0 AS metadata_is_nullable
       FROM pg_attribute
       WHERE attrelid = 'spatial.raster_series_index'::regclass
         AND attname IN (
           'start_datetime',
           'end_datetime',
           'last_new_raster'
         )
     ),
     installed_triggers AS (
       SELECT COUNT(*) = 3 AS has_triggers
       FROM pg_trigger
       WHERE tgrelid = 'spatial.rasters_reference'::regclass
         AND NOT tgisinternal
         AND tgname IN (
           'refresh_raster_series_metadata_on_insert_tr',
           'refresh_raster_series_metadata_on_delete_tr',
           'refresh_raster_series_metadata_on_update_tr'
         )
     )
     SELECT
       to_regprocedure(
         'spatial.refresh_raster_series_metadata(integer[])'
       ) IS NOT NULL AS has_refresh_function,
       metadata_columns.metadata_is_nullable,
       installed_triggers.has_triggers,
       NOT EXISTS (
         SELECT 1
         FROM spatial.raster_series_index rsi
         JOIN expected USING (raster_series_id)
         WHERE
           rsi.start_datetime IS DISTINCT FROM expected.start_datetime OR
           rsi.end_datetime IS DISTINCT FROM expected.end_datetime OR
           rsi.last_issue IS DISTINCT FROM expected.last_issue
       ) AS metadata_matches_references
     FROM metadata_columns
     CROSS JOIN installed_triggers"
  )

  if (!all(unlist(verification[1, ], use.names = FALSE))) {
    stop("Raster series metadata trigger installation verification failed.")
  }

  invisible(verification)
}
