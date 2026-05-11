# Patch 45: role deletion helpers
#
# Adds administrative helper functions used by YGwater's user-management module
# to inspect share_with references, clean those references, and drop roles only
# after share_with references have been cleared. This replaces the earlier patch
# 31 helper definitions with versions that scan all ordinary/partitioned tables
# containing a text[] share_with column, not only RLS-enabled tables.

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 45: adding safer role deletion helpers. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

message("Starting transaction...")
active <- dbTransBegin(con)

tryCatch(
  {
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.role_share_with_references(
        role_to_delete text
      )
      RETURNS TABLE(
        table_schema text,
        table_name text,
        matched_rows integer,
        only_role_rows integer
      )
      LANGUAGE plpgsql
      STABLE
      AS $$
      DECLARE
        rec record;
      BEGIN
        IF role_to_delete IS NULL OR btrim(role_to_delete) = '' THEN
          RAISE EXCEPTION 'role_to_delete must be a non-empty role name.';
        END IF;

        FOR rec IN
          SELECT n.nspname AS schema_name, c.relname AS relation_name
          FROM pg_class c
          JOIN pg_namespace n ON n.oid = c.relnamespace
          JOIN pg_attribute a ON a.attrelid = c.oid
          WHERE c.relkind IN ('r', 'p')
            AND n.nspname !~ '^pg_'
            AND n.nspname <> 'information_schema'
            AND a.attname = 'share_with'
            AND a.atttypid = 'text[]'::regtype
            AND NOT a.attisdropped
          ORDER BY n.nspname, c.relname
        LOOP
          EXECUTE format(
            'SELECT
               count(*) FILTER (
                 WHERE share_with IS NOT NULL
                   AND $1 = ANY(share_with)
               )::integer,
               count(*) FILTER (
                 WHERE share_with IS NOT NULL
                   AND $1 = ANY(share_with)
                   AND NOT EXISTS (
                     SELECT 1
                     FROM unnest(share_with) AS sw(role_name)
                     WHERE sw.role_name IS DISTINCT FROM $1
                   )
               )::integer
             FROM %I.%I',
            rec.schema_name, rec.relation_name
          )
          INTO matched_rows, only_role_rows
          USING role_to_delete;

          table_schema := rec.schema_name;
          table_name := rec.relation_name;
          RETURN NEXT;
        END LOOP;
      END;
      $$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION public.role_share_with_references(text) IS
       'Lists row counts for every table whose text[] share_with column references the supplied database role. only_role_rows counts rows that would become unshared if the role were removed without replacement.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.cleanup_share_with_role(
        role_to_delete text,
        replacement_role text DEFAULT NULL,
        dry_run boolean DEFAULT true
      )
      RETURNS TABLE(
        table_schema text,
        table_name text,
        only_role_rows integer,
        matched_rows integer,
        updated_rows integer
      )
      LANGUAGE plpgsql
      AS $$
      DECLARE
        rec record;
      BEGIN
        IF role_to_delete IS NULL OR btrim(role_to_delete) = '' THEN
          RAISE EXCEPTION 'role_to_delete must be a non-empty role name.';
        END IF;

        IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = role_to_delete) THEN
          RAISE EXCEPTION 'Role % does not exist.', role_to_delete;
        END IF;

        replacement_role := NULLIF(btrim(replacement_role), '');

        IF replacement_role IS NOT NULL THEN
          IF replacement_role = role_to_delete THEN
            RAISE EXCEPTION 'Replacement role must differ from the role being deleted.';
          END IF;

          IF NOT EXISTS (
            SELECT 1
            FROM pg_roles r
            WHERE r.rolname = replacement_role
              AND (
                r.rolname = 'public_reader'
                OR (
                  r.rolcanlogin = false
                  AND r.rolname <> 'public'
                  AND r.rolname !~ '^pg_'
                )
              )
          ) THEN
            RAISE EXCEPTION 'Replacement role % is not a valid share_with role.', replacement_role;
          END IF;
        END IF;

        IF NOT dry_run AND replacement_role IS NULL THEN
          SELECT COALESCE(sum(ref.only_role_rows), 0)::integer
          INTO only_role_rows
          FROM public.role_share_with_references(role_to_delete) ref;

          IF only_role_rows > 0 THEN
            RAISE EXCEPTION
              'Role % is the sole share_with entry in % rows - replacement required.',
              role_to_delete, only_role_rows;
          END IF;
        END IF;

        FOR rec IN
          SELECT ref.table_schema, ref.table_name, ref.matched_rows, ref.only_role_rows
          FROM public.role_share_with_references(role_to_delete) ref
        LOOP
          updated_rows := 0;

          IF NOT dry_run AND rec.matched_rows > 0 THEN
            IF replacement_role IS NULL THEN
              EXECUTE format(
                'UPDATE %I.%I
                 SET share_with = ARRAY(
                   SELECT sw.role_name
                   FROM unnest(array_remove(share_with, $1)) AS sw(role_name)
                   WHERE sw.role_name IS NOT NULL
                 )::text[]
                 WHERE share_with IS NOT NULL
                   AND $1 = ANY(share_with)',
                rec.table_schema, rec.table_name
              )
              USING role_to_delete;
            ELSE
              EXECUTE format(
                'UPDATE %I.%I
                 SET share_with = ARRAY(
                   SELECT DISTINCT sw.role_name
                   FROM unnest(array_replace(share_with, $1, $2)) AS sw(role_name)
                   WHERE sw.role_name IS NOT NULL
                   ORDER BY sw.role_name
                 )::text[]
                 WHERE share_with IS NOT NULL
                   AND $1 = ANY(share_with)',
                rec.table_schema, rec.table_name
              )
              USING role_to_delete, replacement_role;
            END IF;

            GET DIAGNOSTICS updated_rows = ROW_COUNT;
          END IF;

          table_schema := rec.table_schema;
          table_name := rec.table_name;
          matched_rows := rec.matched_rows;
          only_role_rows := rec.only_role_rows;
          RETURN NEXT;
        END LOOP;
      END;
      $$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION public.cleanup_share_with_role(text, text, boolean) IS
       'Dry-runs or applies cleanup of text[] share_with references before deleting a database role. Without a replacement role, rows that would be left with no share_with entries block execution. With a replacement role, the old role is replaced everywhere and duplicates are removed.'"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS public.drop_role_if_unused(text)"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.drop_role_if_unused(
        role_to_delete text,
        reassign_owned_to text DEFAULT NULL,
        drop_owned boolean DEFAULT false
      )
      RETURNS TABLE(
        dropped boolean,
        remaining_references bigint,
        message text
      )
      LANGUAGE plpgsql
      AS $$
      DECLARE
        ref_count bigint;
        parent_role record;
        child_role record;
      BEGIN
        IF role_to_delete IS NULL OR btrim(role_to_delete) = '' THEN
          RAISE EXCEPTION 'role_to_delete must be a non-empty role name.';
        END IF;

        role_to_delete := btrim(role_to_delete);
        reassign_owned_to := NULLIF(btrim(reassign_owned_to), '');

        IF role_to_delete IN ('postgres', 'admin', 'public_reader', 'public')
           OR role_to_delete ~ '^pg_' THEN
          dropped := false;
          remaining_references := 0;
          message := format('Role %s is protected and was not dropped.', role_to_delete);
          RETURN NEXT;
          RETURN;
        END IF;

        IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = role_to_delete) THEN
          dropped := false;
          remaining_references := 0;
          message := format('Role %s does not exist.', role_to_delete);
          RETURN NEXT;
          RETURN;
        END IF;

        IF reassign_owned_to = role_to_delete THEN
          RAISE EXCEPTION 'Reassign-owned target role must differ from the role being deleted.';
        END IF;

        IF reassign_owned_to IS NOT NULL AND NOT EXISTS (
          SELECT 1 FROM pg_roles WHERE rolname = reassign_owned_to
        ) THEN
          RAISE EXCEPTION 'Reassign-owned target role % does not exist.', reassign_owned_to;
        END IF;

        SELECT COALESCE(sum(ref.matched_rows), 0)::bigint
        INTO ref_count
        FROM public.role_share_with_references(role_to_delete) ref;

        IF ref_count > 0 THEN
          dropped := false;
          remaining_references := ref_count;
          message := format(
            'Role %s is still referenced in share_with (%s total row references). Clean those references before dropping the role.',
            role_to_delete, ref_count
          );
          RETURN NEXT;
          RETURN;
        END IF;

        BEGIN
          FOR child_role IN
            SELECT child.rolname
            FROM pg_auth_members am
            JOIN pg_roles parent ON parent.oid = am.roleid
            JOIN pg_roles child ON child.oid = am.member
            WHERE parent.rolname = role_to_delete
          LOOP
            EXECUTE format('REVOKE %I FROM %I CASCADE', role_to_delete, child_role.rolname);
          END LOOP;

          FOR parent_role IN
            SELECT parent.rolname
            FROM pg_auth_members am
            JOIN pg_roles parent ON parent.oid = am.roleid
            JOIN pg_roles child ON child.oid = am.member
            WHERE child.rolname = role_to_delete
          LOOP
            EXECUTE format('REVOKE %I FROM %I CASCADE', parent_role.rolname, role_to_delete);
          END LOOP;

          IF reassign_owned_to IS NOT NULL THEN
            EXECUTE format(
              'REASSIGN OWNED BY %I TO %I',
              role_to_delete,
              reassign_owned_to
            );
          END IF;

          IF drop_owned THEN
            EXECUTE format('DROP OWNED BY %I', role_to_delete);
          END IF;

          EXECUTE format('DROP ROLE %I', role_to_delete);
          dropped := true;
          remaining_references := 0;
          message := format('Role %s dropped.', role_to_delete);
        EXCEPTION
          WHEN dependent_objects_still_exist OR insufficient_privilege OR object_in_use OR others THEN
            dropped := false;
            remaining_references := 0;
            message := format('Role %s has no share_with references, but DROP ROLE failed: %s', role_to_delete, SQLERRM);
        END;

        RETURN NEXT;
      END;
      $$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION public.drop_role_if_unused(text, text, boolean) IS
       'Drops a database role only after share_with references have been removed. Optionally reassigns owned objects and drops privileges in the current database before attempting DROP ROLE.'"
    )

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure('public.role_share_with_references(text)') IS NOT NULL AS has_reference_function,
         to_regprocedure('public.cleanup_share_with_role(text,text,boolean)') IS NOT NULL AS has_cleanup_function,
         to_regprocedure('public.drop_role_if_unused(text,text,boolean)') IS NOT NULL AS has_drop_function,
         to_regprocedure('public.drop_role_if_unused(text)') IS NULL AS old_drop_removed"
    )

    if (
      !isTRUE(verification$has_reference_function[[1]]) ||
        !isTRUE(verification$has_cleanup_function[[1]]) ||
        !isTRUE(verification$has_drop_function[[1]]) ||
        !isTRUE(verification$old_drop_removed[[1]])
    ) {
      stop(
        "Patch 45 verification failed: expected role deletion helpers were not installed."
      )
    }

    # Expand location_metadata views to include location types
    # Drop view and recreate in English
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS public.location_metadata_en;"
    )
    # Drop view and recreate in French
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS public.location_metadata_fr;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW public.location_metadata_en WITH (security_invoker='true', security_barrier='true') AS
        SELECT 
          loc.location_id,
          loc.name,
          loc.alias AS alias,
          loc.location_code,
          lt.type AS location_type,
          loc.latitude,
          loc.longitude,
          dc.conversion_m AS elevation,
          dl.datum_name_en AS datum,
          loc.note,
          array_agg(DISTINCT proj.name) AS projects,
          array_agg(DISTINCT net.name) AS networks,
          COALESCE(
            jsonb_agg(
              DISTINCT jsonb_build_object(
                'language_code', lng.language_id,
                'language_name_en', lng.language_name_en,
                'name', ln.name
              )
            ) FILTER (WHERE ln.location_id IS NOT NULL),
            '[]'::jsonb
          ) AS fn_names
        FROM ((((((public.locations loc
          LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
          LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
          LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
          LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
          LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
          LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
          LEFT JOIN public.location_names ln ON loc.location_id = ln.location_id
          LEFT JOIN public.languages lng ON ln.language_id = lng.language_id
          LEFT JOIN public.location_types lt ON loc.location_type = lt.type_id
        GROUP BY loc.location_id, loc.location_code, lt.type, loc.name, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_en;"
    )

    DBI::dbExecute(
      con,
      "ALTER VIEW public.location_metadata_en OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE VIEW public.location_metadata_fr WITH (security_invoker='true', security_barrier='true') AS
        SELECT loc.location_id,
        loc.name_fr AS nom,
            loc.alias AS alias,
            loc.location_code AS code_de_site,
            lt.type_fr AS type_de_site,
            loc.latitude,
            loc.longitude,
            dc.conversion_m AS altitude,
            dl.datum_name_fr AS datum,
            loc.note,
            array_agg(DISTINCT proj.name_fr) AS projets,
            array_agg(DISTINCT net.name_fr) AS réseaux,
            COALESCE(
              jsonb_agg(
                DISTINCT jsonb_build_object(
                  'language_id', lng.language_id,
                  'language_name_fr', lng.language_name_fr,
                  'name', ln.name
                )
              ) FILTER (WHERE ln.location_id IS NOT NULL),
              '[]'::jsonb
            ) AS noms_premières_nations
          FROM ((((((public.locations loc
            LEFT JOIN public.locations_projects loc_proj ON ((loc.location_id = loc_proj.location_id)))
            LEFT JOIN public.projects proj ON ((loc_proj.project_id = proj.project_id)))
            LEFT JOIN public.locations_networks loc_net ON ((loc.location_id = loc_net.location_id)))
            LEFT JOIN public.networks net ON ((loc_net.network_id = net.network_id)))
            LEFT JOIN public.datum_conversions dc ON (((loc.location_id = dc.location_id) AND (dc.current = true))))
            LEFT JOIN public.datum_list dl ON ((dc.datum_id_to = dl.datum_id)))
            LEFT JOIN public.location_names ln ON loc.location_id = ln.location_id
            LEFT JOIN public.languages lng ON ln.language_id = lng.language_id
            LEFT JOIN public.location_types lt ON loc.location_type = lt.type_id
          GROUP BY loc.location_id, loc.location_code, lt.type_fr, loc.name_fr, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_fr;"
    )

    DBI::dbExecute(
      con,
      "ALTER VIEW public.location_metadata_fr OWNER TO admin;"
    )

    # Grant select on views to public role
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE public.location_metadata_en TO public;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE public.location_metadata_fr TO public;"
    )

    # Same thing for continuous.timeseries_medata views

    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.timeseries_metadata_en;"
    )
    DBI::dbExecute(
      con,
      "DROP VIEW IF EXISTS continuous.timeseries_metadata_fr;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW continuous.timeseries_metadata_en
    WITH(security_invoker=true,security_barrier=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name AS location_name,
        lt.type AS location_type,
        loc.alias AS alias_name,
        lz.z_meters AS depth_height_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS location_elevation,
        array_agg(DISTINCT proj.name) AS projects,
        array_agg(DISTINCT net.name) AS networks,
        mtypes.media_type,
        params.param_name AS parameter_name,
        get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id) AS units,
        at.aggregation_type,
        ts.record_rate AS recording_rate,
        ts.sensor_priority,
        ts.start_datetime,
        ts.end_datetime,
        ts.note,
        ts.timeseries_type AS timeseries_type_code,
        tt.timeseries_type_name AS timeseries_type,
        tt.description AS timeseries_type_description,
        ts.last_new_data
       FROM timeseries ts
         JOIN locations loc ON ts.location_id = loc.location_id
         LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
         LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
         LEFT JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
         LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
         LEFT JOIN locations_projects loc_proj ON loc.location_id = loc_proj.location_id
         LEFT JOIN projects proj ON loc_proj.project_id = proj.project_id
         LEFT JOIN locations_networks loc_net ON loc.location_id = loc_net.location_id
         LEFT JOIN networks net ON loc_net.network_id = net.network_id
         LEFT JOIN datum_conversions dc ON loc.location_id = dc.location_id AND dc.current = true
         LEFT JOIN timeseries_types tt ON ts.timeseries_type = tt.timeseries_type
         LEFT JOIN location_types lt ON loc.location_type = lt.type_id
      GROUP BY ts.timeseries_id, loc.location_id, lt.type, mtypes.media_type, params.param_name, (get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id)), at.aggregation_type, lz.z_meters, dc.conversion_m, tt.timeseries_type_name, tt.description;"
    )

    DBI::dbExecute(
      con,
      'CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
    WITH(security_invoker=true,security_barrier=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name_fr AS nom_endroit,
        lt.type_fr AS type_endroit,
        loc.alias AS nom_alias,
        lz.z_meters AS profondeur_hauteur_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS "élévation_endroit",
        array_agg(DISTINCT proj.name_fr) AS projets,
        array_agg(DISTINCT net.name_fr) AS "réseaux",
        mtypes.media_type_fr AS "type_de_média",
        params.param_name_fr AS "nom_paramètre",
        get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id) AS "unités",
        ag.aggregation_type_fr AS "type_agrégation",
        ts.record_rate AS "fréquence_enregistrement",
        ts.sensor_priority AS "priorité_capteur",
        ts.start_datetime AS "début",
        ts.end_datetime AS fin,
        ts.note,
        ts.timeseries_type AS code_type_serie_temporelle,
        tt.timeseries_type_name_fr AS type_serie_temporelle,
        tt.description_fr AS description_type_serie_temporelle,
        ts.last_new_data AS dernier_nouvelles_donnees
       FROM timeseries ts
         JOIN locations loc ON ts.location_id = loc.location_id
         LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
         LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
         LEFT JOIN aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id
         LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
         LEFT JOIN locations_projects loc_proj ON loc.location_id = loc_proj.location_id
         LEFT JOIN projects proj ON loc_proj.project_id = proj.project_id
         LEFT JOIN locations_networks loc_net ON loc.location_id = loc_net.location_id
         LEFT JOIN networks net ON loc_net.network_id = net.network_id
         LEFT JOIN datum_conversions dc ON loc.location_id = dc.location_id AND dc.current = true
         LEFT JOIN timeseries_types tt ON ts.timeseries_type = tt.timeseries_type
          LEFT JOIN location_types lt ON loc.location_type = lt.type_id
      GROUP BY ts.timeseries_id, loc.location_id, lt.type_fr, mtypes.media_type_fr, params.param_name_fr, (get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id)), ag.aggregation_type_fr, lz.z_meters, dc.conversion_m, tt.timeseries_type_name_fr, tt.description_fr;'
    )

    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.timeseries_metadata_en OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.timeseries_metadata_fr OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_metadata_en TO public;"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_metadata_fr TO public;"
    )

    # If we got here, everything worked and we can commit the transaction

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '45'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    DBI::dbExecute(con, "COMMIT;")
    message(
      "Patch 45 applied successfully. Role deletion helpers are installed."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    stop(
      "Patch 45 failed and the database has been rolled back to its previous state. ",
      e$message
    )
  }
)
