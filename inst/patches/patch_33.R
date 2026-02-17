# Patch 33

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 33. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    # Drop the hourly view, don't re-create
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.measurements_hourly_corrected;"
    )

    # Drop unused functions
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS public.check_location_exists();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS public.user_in_group(text);"
    )

    # pin apply_corrections to invoker context explicitly.
    #
    # The view `continuous.measurements_continuous_corrected` does not join
    # `continuous.timeseries` directly; it reads from `continuous.measurements_continuous`
    # and computes corrected values with this function. The critical protection is now
    # enforced by direct RLS on the source measurement table (below), but this explicit
    # security mode keeps helper execution aligned with caller privileges.
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.apply_corrections(integer, timestamp with time zone, numeric) SECURITY INVOKER;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.apply_corrections(integer, timestamp with time zone, numeric)
       SET search_path = continuous, pg_catalog;"
    )

    # -------------------------------------------------------------------------
    # 2) Ensure continuous views run with invoker privileges
    # -------------------------------------------------------------------------
    # Why this matters:
    # - These views are intended to expose only rows linked to visible timeseries rows.
    # - If views execute with owner privileges, they can bypass caller-scoped RLS checks.
    #
    # Fix:
    # - Force SECURITY INVOKER semantics on each view.
    # - Keep security_barrier enabled to reduce planner predicate-pushdown surprises.
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.measurements_calculated_daily_corrected
       SET (security_invoker = true, security_barrier = true);"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.measurements_continuous_corrected
       SET (security_invoker = true, security_barrier = true);"
    )

    # SECURITY INVOKER views require callers to have permissions on all
    # referenced relations. Grant SELECT broadly and rely on RLS to enforce
    # per-row visibility from share_with.
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.measurements_calculated_daily TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.measurements_continuous TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.corrections TO PUBLIC;"
    )

    # -------------------------------------------------------------------------
    # 3) Add direct RLS protection to source measurement tables
    # -------------------------------------------------------------------------
    # Why this matters:
    # - Even with secured views, users can still query base measurement tables directly
    #   when they have SELECT grants.
    # - Those base tables do not contain share_with columns, so policies must resolve
    #   visibility through continuous.timeseries.
    #
    # Fix:
    # - Enable RLS on both source tables.
    # - Use a policy that permits access only when linked timeseries rows are visible to
    #   the caller via share_with (public_reader or one of caller memberships).

    # Drop possible FKs on measurements_xxx tables that could interfere with RLS policy creation. New policies are created later down that treat these tables a 'children' of 'timeseries'
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_continuous ENABLE ROW LEVEL SECURITY;"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS measurements_continuous_rls ON continuous.measurements_continuous;"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS rls ON continuous.measurements_continuous;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.measurements_calculated_daily ENABLE ROW LEVEL SECURITY;"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS measurements_calculated_daily_rls ON continuous.measurements_calculated_daily;"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS rls ON continuous.measurements_calculated_daily;"
    )

    # -------------------------------------------------------------------------
    # 4) Cascade visibility from parent tables via foreign keys
    # -------------------------------------------------------------------------
    # Goal requested by maintainers:
    #   1) if a location is hidden, rows in tables that reference that location
    #      should also be hidden;
    #   2) if a timeseries is hidden, rows in tables that reference that timeseries
    #      should also be hidden;
    #   3) same cascading visibility behavior for references to boreholes.boreholes,
    #      boreholes.wells, and files.documents.
    #
    # Implementation details:
    # - Discover single-column FKs pointing at any parent table with share_with
    #   that should govern dependent visibility (locations, timeseries, boreholes,
    #   wells, and documents).
    # - Build one restrictive SELECT policy per child table that ANDs all relevant
    #   parent-visibility checks.
    # - Enable RLS on those child tables.
    # - If a table has no permissive SELECT policy yet, add a neutral permissive
    #   policy (USING true) so the restrictive policy can still be evaluated.
    DBI::dbExecute(
      con,
      "DO $$
       DECLARE
         rec RECORD;
         has_permissive_select BOOLEAN;
       BEGIN
         FOR rec IN
           WITH parent_catalog AS (
             SELECT 'public.locations'::regclass AS parent_relid, 'location_id'::text AS parent_key, 'location'::text AS parent_type
             UNION ALL
             SELECT 'continuous.timeseries'::regclass, 'timeseries_id', 'timeseries'
             UNION ALL
             SELECT 'boreholes.boreholes'::regclass, 'borehole_id', 'borehole'
             UNION ALL
             SELECT 'discrete.samples'::regclass, 'sample_id', 'sample'
           ),
           parent_fks AS (
             SELECT
               n.nspname AS child_schema,
               c.relname AS child_table,
               a.attname AS child_column,
               pc.parent_type
             FROM pg_constraint con
             JOIN parent_catalog pc
               ON pc.parent_relid = con.confrelid
             JOIN pg_class c
               ON c.oid = con.conrelid
             JOIN pg_namespace n
               ON n.oid = c.relnamespace
             JOIN pg_attribute a
               ON a.attrelid = con.conrelid
              AND a.attnum = con.conkey[1]
             JOIN pg_attribute pa
               ON pa.attrelid = con.confrelid
              AND pa.attnum = con.confkey[1]
             WHERE con.contype = 'f'
               AND array_length(con.conkey, 1) = 1
               AND array_length(con.confkey, 1) = 1
               AND pa.attname = pc.parent_key
               AND NOT (n.nspname = 'public' AND c.relname = 'locations')
               AND NOT (n.nspname = 'discrete' AND c.relname = 'samples')
               AND NOT (n.nspname = 'boreholes' AND c.relname = 'boreholes')
           )
           SELECT
             child_schema,
             child_table,
             string_agg(
               CASE parent_type
                 WHEN 'location' THEN
                   format(
                     'EXISTS (
                      SELECT 1 FROM public.locations l 
                      WHERE l.location_id = %I.%I
                      AND (
                        l.share_with @> ARRAY[''public_reader''::text]
                        OR EXISTS (
                          SELECT 1
                          FROM unnest(l.share_with) AS r(role_name)
                          WHERE pg_has_role(current_user, r.role_name, ''member'')
                        )
                      )
                     )',
                     child_table, child_column
                   )
                 WHEN 'timeseries' THEN
                   format(
                     'EXISTS (
                      SELECT 1 FROM continuous.timeseries ts 
                      WHERE ts.timeseries_id = %I.%I
                      AND (
                        ts.share_with @> ARRAY[''public_reader''::text]
                        OR EXISTS (
                          SELECT 1
                          FROM unnest(ts.share_with) AS r(role_name)
                          WHERE pg_has_role(current_user, r.role_name, ''member'')
                        )
                       )
                     )',
                     child_table, child_column
                   )
                 WHEN 'borehole' THEN
                   format(
                     'EXISTS (
                      SELECT 1 FROM boreholes.boreholes b 
                      WHERE b.borehole_id = %I.%I
                      AND (
                        b.share_with @> ARRAY[''public_reader''::text]
                        OR EXISTS (
                          SELECT 1
                          FROM unnest(b.share_with) AS r(role_name)
                          WHERE pg_has_role(current_user, r.role_name, ''member'')
                        )
                      )
                     )',
                     child_table, child_column
                   )
                  WHEN 'sample' THEN
                   format(
                     'EXISTS (
                      SELECT 1 FROM discrete.samples s 
                      WHERE s.sample_id = %I.%I
                      AND (
                        s.share_with @> ARRAY[''public_reader''::text]
                        OR EXISTS (
                          SELECT 1
                          FROM unnest(s.share_with) AS r(role_name)
                          WHERE pg_has_role(current_user, r.role_name, ''member'')
                        )
                      )
                     )',
                     child_table, child_column
                   )
               END,
               ' AND '
               ORDER BY parent_type, child_column
             ) AS using_sql
           FROM parent_fks
           GROUP BY child_schema, child_table
         LOOP
           EXECUTE format(
             'ALTER TABLE %I.%I ENABLE ROW LEVEL SECURITY',
             rec.child_schema,
             rec.child_table
           );

           EXECUTE format(
             'DROP POLICY IF EXISTS parent_visibility_restrict ON %I.%I',
             rec.child_schema,
             rec.child_table
           );
           EXECUTE format(
             'CREATE POLICY parent_visibility_restrict ON %I.%I AS RESTRICTIVE FOR SELECT USING (%s)',
             rec.child_schema,
             rec.child_table,
             rec.using_sql
           );

           SELECT EXISTS (
             SELECT 1
             FROM pg_policy p
             JOIN pg_class c
               ON c.oid = p.polrelid
             JOIN pg_namespace n
               ON n.oid = c.relnamespace
             WHERE n.nspname = rec.child_schema
               AND c.relname = rec.child_table
               AND p.polpermissive IS TRUE
               AND p.polcmd IN ('r', '*')
           )
           INTO has_permissive_select;

           IF NOT has_permissive_select THEN
             EXECUTE format(
               'DROP POLICY IF EXISTS parent_visibility_allow_select ON %I.%I',
               rec.child_schema,
               rec.child_table
             );
             EXECUTE format(
               'CREATE POLICY parent_visibility_allow_select ON %I.%I FOR SELECT USING (true)',
               rec.child_schema,
               rec.child_table
             );
           END IF;
         END LOOP;
       END
       $$;"
    )

    DBI::dbExecute(
      con,
      "ALTER ROLE
    public_reader
    RESET
    logged_in_user.group_id"
    )

    DBI::dbExecute(
      con,
      "ALTER ROLE
    public_reader
    RESET
    logged_in_user.username"
    )

    # Replace any remaining policy references to current_user_roles() with direct pg_has_role(...) checks on share_with, then verify no dependency remains.
    DBI::dbExecute(
      con,
      "DO $$
       DECLARE
         rec RECORD;
       BEGIN
         FOR rec IN
           SELECT
             n.nspname AS table_schema,
             c.relname AS table_name,
             p.polname AS policy_name,
             p.polwithcheck IS NOT NULL AS has_with_check
           FROM pg_policy p
           JOIN pg_class c
             ON c.oid = p.polrelid
           JOIN pg_namespace n
             ON n.oid = c.relnamespace
           JOIN pg_attribute a
             ON a.attrelid = c.oid
            AND a.attname = 'share_with'
            AND a.attnum > 0
            AND NOT a.attisdropped
           WHERE (
             pg_get_expr(p.polqual, p.polrelid) ILIKE '%current_user_roles%'
             OR COALESCE(pg_get_expr(p.polwithcheck, p.polrelid), '') ILIKE '%current_user_roles%'
           )
         LOOP
           EXECUTE format(
             'ALTER POLICY %I ON %I.%I USING (share_with @> ARRAY[''public_reader''::text] OR EXISTS (SELECT 1 FROM unnest(share_with) AS s(role_name) WHERE pg_has_role(current_user, s.role_name, ''member'')));',
             rec.policy_name,
             rec.table_schema,
             rec.table_name
           );

           IF rec.has_with_check THEN
             EXECUTE format(
               'ALTER POLICY %I ON %I.%I WITH CHECK (share_with @> ARRAY[''public_reader''::text] OR EXISTS (SELECT 1 FROM unnest(share_with) AS s(role_name) WHERE pg_has_role(current_user, s.role_name, ''member'')));',
               rec.policy_name,
               rec.table_schema,
               rec.table_name
             );
           END IF;
         END LOOP;
       END
       $$;"
    )

    remaining <- DBI::dbGetQuery(
      con,
      "
    SELECT count(*)
    FROM pg_depend d
    JOIN pg_proc p ON p.oid = d.refobjid
    WHERE p.proname = 'current_user_roles'
      AND p.pronamespace = 'public'::regnamespace;
    "
    )
    if (remaining$count > 0) {
      stop(
        "Cannot drop current_user_roles function because there are still dependencies. Please investigate and remove those dependencies before applying this patch."
      )
    }

    # Now finally, drop current_user_roles
    DBI::dbExecute(con, "DROP FUNCTION IF EXISTS public.current_user_roles();")

    # Move timeseries_data_sharing_agreements to the continuous schema
    DBI::dbExecute(
      con,
      "ALTER TABLE timeseries_data_sharing_agreements SET SCHEMA continuous;"
    )

    # Now modify the raster tables to improve usability in future ################
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.rasters_reference
       ADD COLUMN IF NOT EXISTS cell_size_x_deg numeric;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.rasters_reference
       ADD COLUMN IF NOT EXISTS cell_size_y_deg numeric;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN spatial.rasters_reference.cell_size_x_deg IS 'Pixel width (degrees) for the raster tiles associated with this reference_id.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN spatial.rasters_reference.cell_size_y_deg IS 'Pixel height (degrees) for the raster tiles associated with this reference_id.';"
    )

    message(
      "Populating cell size for existing rasters in the rasters_reference table. This can take a while if you have many rasters, please be patient!"
    )
    DBI::dbExecute(
      con,
      "
      UPDATE spatial.rasters_reference rr
      SET cell_size_x_deg = s.cell_size_x_deg,
          cell_size_y_deg = s.cell_size_y_deg
      FROM (
        SELECT
          reference_id,
          MIN(ABS(ST_ScaleX(rast))) AS cell_size_x_deg,
          MIN(ABS(ST_ScaleY(rast))) AS cell_size_y_deg
        FROM spatial.rasters
        WHERE reference_id IS NOT NULL
        GROUP BY reference_id
      ) s
      WHERE rr.reference_id = s.reference_id;
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE FUNCTION spatial.sync_rr_cell_size_deg_ins()
      RETURNS trigger
      LANGUAGE plpgsql
      AS $$
      BEGIN
        UPDATE spatial.rasters_reference rr
        SET cell_size_x_deg = s.cell_size_x_deg,
            cell_size_y_deg = s.cell_size_y_deg
        FROM (
          SELECT
            r.reference_id,
            MIN(ABS(ST_ScaleX(r.rast))) AS cell_size_x_deg,
            MIN(ABS(ST_ScaleY(r.rast))) AS cell_size_y_deg
          FROM spatial.rasters r
          WHERE r.reference_id IN (
            SELECT DISTINCT nr.reference_id
            FROM new_rows nr
            WHERE nr.reference_id IS NOT NULL
          )
          GROUP BY r.reference_id
        ) s
        WHERE rr.reference_id = s.reference_id;

        RETURN NULL;
      END;
      $$;
      "
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_sync_rr_cell_size_ins ON spatial.rasters;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE TRIGGER trg_sync_rr_cell_size_ins
      AFTER INSERT ON spatial.rasters
      REFERENCING NEW TABLE AS new_rows
      FOR EACH STATEMENT
      EXECUTE FUNCTION spatial.sync_rr_cell_size_deg_ins();
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE FUNCTION spatial.sync_rr_cell_size_deg_upd()
      RETURNS trigger
      LANGUAGE plpgsql
      AS $$
      BEGIN
        UPDATE spatial.rasters_reference rr
        SET cell_size_x_deg = s.cell_size_x_deg,
            cell_size_y_deg = s.cell_size_y_deg
        FROM (
          SELECT
            r.reference_id,
            MIN(ABS(ST_ScaleX(r.rast))) AS cell_size_x_deg,
            MIN(ABS(ST_ScaleY(r.rast))) AS cell_size_y_deg
          FROM spatial.rasters r
          WHERE r.reference_id IN (
            SELECT DISTINCT reference_id
            FROM (
              SELECT nr.reference_id FROM new_rows nr
              UNION
              SELECT orr.reference_id FROM old_rows orr
            ) x
            WHERE reference_id IS NOT NULL
          )
          GROUP BY r.reference_id
        ) s
        WHERE rr.reference_id = s.reference_id;

        RETURN NULL;
      END;
      $$;
      "
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_sync_rr_cell_size_upd ON spatial.rasters;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE TRIGGER trg_sync_rr_cell_size_upd
      AFTER UPDATE ON spatial.rasters
      REFERENCING NEW TABLE AS new_rows OLD TABLE AS old_rows
      FOR EACH STATEMENT
      EXECUTE FUNCTION spatial.sync_rr_cell_size_deg_upd();
      "
    )

    # Add a new approval type for automatically screened data #####
    DBI::dbExecute(
      con,
      "INSERT INTO public.approval_types (approval_type_code, approval_type_description, approval_type_description_fr, color_code)
       VALUES ('S', 'auto screened, no human review', 'auto filtré, pas de révision humaine', '#FFA500');"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '33' WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion("AquaCache")),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")

    message("Patch 33 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 33 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
