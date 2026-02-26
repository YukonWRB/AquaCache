# Patch 34

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 34. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # Add tables in schema 'application' to track API use and use of Shiny applications

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.api_requests (
            id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            session_start TIMESTAMPTZ NOT NULL DEFAULT NOW(),
            session_end TIMESTAMPTZ,
            endpoint TEXT NOT NULL, -- the API endpoint that was hit
            parameters JSONB, -- the parameters provided with the API request, stored as JSON
            user_id TEXT, -- The user's authentication name, if provided
            user_ip INET,
            status_code INTEGER,
            success BOOLEAN DEFAULT FALSE, -- gets flipped to TRUE upon request completion if the request was successful
            response_time_ms INTEGER GENERATED ALWAYS AS (
              CASE
                WHEN session_end IS NOT NULL
                THEN (EXTRACT(EPOCH FROM (session_end - session_start)) * 1000)::integer
                ELSE NULL
              END
            ) STORED -- the time it took to process the request, in milliseconds
          );"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.shiny_app_usage (
            id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            app_name TEXT NOT NULL,
            session_start TIMESTAMPTZ NOT NULL DEFAULT NOW(),
            session_end TIMESTAMPTZ,
            user_id TEXT, -- The user's login name
            user_ip INET,
            login_to INTEGER REFERENCES shiny_app_usage(id) DEFERRABLE INITIALLY DEFERRED, -- self reference to track sessions, NULL for logouts without a login record
            error_message TEXT -- If the session ended with an error, the error message is stored here
          );"
    )

    # Make owners 'admin'
    DBI::dbExecute(
      con,
      "ALTER TABLE application.api_requests OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE application.shiny_app_usage OWNER TO admin;"
    )

    # Grant permissions on the new tables to everyone (read, write, update)
    DBI::dbExecute(
      con,
      "REVOKE ALL ON application.shiny_app_usage FROM PUBLIC;"
    )

    DBI::dbExecute(
      con,
      "GRANT INSERT ON application.shiny_app_usage TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT UPDATE (session_end, login_to, error_message)
      ON application.shiny_app_usage
      TO PUBLIC;"
    )
    # Technically not necessary because the apps use RETURNING id and select isn't needed for that
    DBI::dbExecute(
      con,
      "GRANT SELECT (id)
      ON application.shiny_app_usage
      TO PUBLIC;"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON application.api_requests FROM PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT INSERT ON application.api_requests TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT UPDATE (session_end, status_code, success)
      ON application.api_requests
      TO PUBLIC;"
    )

    message("Tracking tables created successfully.")

    # Modify the 'timeseries' view tables to include the networks and projects that the timeseries's location belongs to.
    message("Adding additional information to the timeseries_metadata views...")
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
    WITH(security_invoker=true)
    AS SELECT 
        ts.timeseries_id,
        loc.location_id,
        loc.name AS location_name,
        lz.z_meters AS depth_height_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS location_elevation,
        array_agg(DISTINCT proj.name) AS projects,
        array_agg(DISTINCT net.name) AS networks,
        mtypes.media_type,
        params.param_name AS parameter_name,
        params.unit_default AS units,
        at.aggregation_type,
        ts.record_rate AS recording_rate,
        ts.start_datetime,
        ts.end_datetime,
        ts.note
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
      GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type, params.param_name, params.unit_default, at.aggregation_type, lz.z_meters, dc.conversion_m;"
    )

    DBI::dbExecute(
      con,
      '
    CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
    WITH(security_invoker=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name_fr AS nom_endroit,
        lz.z_meters AS profondeur_hauteur_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS "élévation_endroit",
        array_agg(DISTINCT proj.name_fr) AS projets,
        array_agg(DISTINCT net.name_fr) AS "réseaux",
        mtypes.media_type_fr AS "type_de_média",
        params.param_name_fr AS "nom_paramètre",
        params.unit_default AS "unités",
        ag.aggregation_type_fr AS "type_agrégation",
        ts.record_rate AS "fréquence_enregistrement",
        ts.start_datetime AS "début",
        ts.end_datetime AS fin,
        ts.note
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
      GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type_fr, params.param_name_fr, params.unit_default, ag.aggregation_type_fr, lz.z_meters, dc.conversion_m;'
    )

    # columns public.timeseries.timeseries_data_sharing_agreement_id and spatial.raster_types were created as SERIAL PRIMARY KEY - need to change to GENERATED ALWAYS AS IDENTITY
    DBI::dbExecute(
      con,
      "
      DO $$
      DECLARE
        r record;
        nextval bigint;
        seq_schema text;
        seq_name   text;
        old_seq_tmp text;
      BEGIN
        FOR r IN
          SELECT
            n.nspname  AS sch,
            c.relname  AS tbl,
            a.attname  AS col,
            pg_get_serial_sequence(format('%I.%I', n.nspname, c.relname), a.attname) AS seq
          FROM pg_attribute a
          JOIN pg_class     c ON c.oid = a.attrelid AND c.relkind = 'r'
          JOIN pg_namespace n ON n.oid = c.relnamespace
          JOIN pg_attrdef   d ON d.adrelid = a.attrelid AND d.adnum = a.attnum
          WHERE a.attnum > 0
            AND NOT a.attisdropped
            -- has nextval() default => SERIAL-like
            AND pg_get_expr(d.adbin, d.adrelid) LIKE 'nextval(%'
            -- (optional) limit to PK columns only
            AND EXISTS (
              SELECT 1 FROM pg_index i
              WHERE i.indrelid = c.oid AND i.indisprimary
                AND a.attnum = ANY(i.indkey)
            )
        LOOP
          -- Compute a safe next value before we drop/rename
          EXECUTE format(
            'SELECT GREATEST(
              (SELECT COALESCE(MAX(%1$I),0)+1 FROM %2$I.%3$I),
              (SELECT last_value + CASE WHEN is_called THEN 1 ELSE 0 END FROM %4$s)
            )',
            r.col, r.sch, r.tbl, r.seq
          ) INTO nextval;

          -- Parse schema and name of the existing serial sequence
          seq_schema := split_part(r.seq, '.', 1);
          seq_name   := split_part(r.seq, '.', 2);

          -- Rename the old serial sequence to free the original name
        old_seq_tmp := '_old_sequence_';
          EXECUTE format('ALTER SEQUENCE %s RENAME TO %I', r.seq, old_seq_tmp);

          -- Drop the DEFAULT and add IDENTITY with the original sequence name
          EXECUTE format('ALTER TABLE %I.%I ALTER COLUMN %I DROP DEFAULT', r.sch, r.tbl, r.col);
          EXECUTE format(
            'ALTER TABLE %I.%I ALTER COLUMN %I ADD GENERATED BY DEFAULT AS IDENTITY (SEQUENCE NAME %s)',
            r.sch, r.tbl, r.col, format('%I.%I', seq_schema, seq_name)
          );

          -- Set the identity start to the computed next value
          EXECUTE format('ALTER TABLE %I.%I ALTER COLUMN %I RESTART WITH %s', r.sch, r.tbl, r.col, nextval);

          -- Old serial sequence is no longer used; drop it
          EXECUTE format('DROP SEQUENCE %I.%I', seq_schema, old_seq_tmp);
        END LOOP;
      END
      $$ LANGUAGE plpgsql;
      "
    )

    # rename public.languages.language_code to language_id if it's not already done
    check <- DBI::dbGetQuery(
      con,
      "SELECT column_name FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'languages' AND column_name = 'language_id';"
    )
    if (nrow(check) == 0) {
      # Deal with views
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
        "ALTER TABLE public.languages RENAME COLUMN language_code TO language_id;"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE public.location_names RENAME COLUMN language_code TO language_id;"
      )

      DBI::dbExecute(
        con,
        "CREATE OR REPLACE VIEW public.location_metadata_en WITH (security_invoker='true') AS
        SELECT 
          loc.location_id,
          loc.name,
          loc.alias AS alias,
          loc.location_code,
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
        GROUP BY loc.location_id, loc.location_code, loc.name, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_en;"
      )

      DBI::dbExecute(
        con,
        "ALTER VIEW public.location_metadata_en OWNER TO admin;"
      )

      DBI::dbExecute(
        con,
        "CREATE VIEW public.location_metadata_fr WITH (security_invoker='true') AS
        SELECT loc.location_id,
        loc.name_fr AS nom,
            loc.alias AS alias,
            loc.location_code AS code_de_site,
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
          GROUP BY loc.location_id, loc.location_code, loc.name_fr, loc.latitude, loc.longitude, loc.note, dc.conversion_m, dl.datum_name_fr;"
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
    }

    # Redo of RLS policies introduced in patch 33 because they did not address anything besides select and hid visibility if a parent record column was NULL
    DBI::dbExecute(
      con,
      "DO $$
       DECLARE
         rec RECORD;
         has_permissive_select BOOLEAN;
         has_permissive_insert BOOLEAN;
         has_permissive_update BOOLEAN;
         has_permissive_delete BOOLEAN;
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
               AND con.conrelid <> con.confrelid          -- exclude self-FKs
               --AND NOT (n.nspname = 'public' AND c.relname = 'locations')
               --AND NOT (n.nspname = 'discrete' AND c.relname = 'samples')
               --AND NOT (n.nspname = 'boreholes' AND c.relname = 'boreholes')
           )
           SELECT
             child_schema,
             child_table,
             string_agg(
               CASE parent_type
                 WHEN 'location' THEN
                   format(
                     '(%I.%I IS NULL OR EXISTS (
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
    ))',
                     child_table, child_column, child_table, child_column
                   )
                 WHEN 'timeseries' THEN
                   format(
                     '(%I.%I IS NULL OR EXISTS (
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
    ))',
                     child_table, child_column, child_table, child_column
                   )
                 WHEN 'borehole' THEN
                   format(
                     '(%I.%I IS NULL OR EXISTS (
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
    ))',
                     child_table, child_column, child_table, child_column
                   )
                 WHEN 'sample' THEN
                   format(
                     '(%I.%I IS NULL OR EXISTS (
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
    ))',
                     child_table, child_column, child_table, child_column
                   )
               END,
               ' AND '
               ORDER BY parent_type, child_column
             ) AS using_sql
           FROM parent_fks
           GROUP BY child_schema, child_table
         LOOP
         BEGIN
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

           EXECUTE format(
            'DROP POLICY IF EXISTS parent_visibility_restrict_insert ON %I.%I',
            rec.child_schema,
            rec.child_table
          );
          EXECUTE format(
            'CREATE POLICY parent_visibility_restrict_insert ON %I.%I AS RESTRICTIVE FOR INSERT WITH CHECK (%s)',
            rec.child_schema,
            rec.child_table,
            rec.using_sql
          );
          EXECUTE format(
            'DROP POLICY IF EXISTS parent_visibility_restrict_update ON %I.%I',
            rec.child_schema,
            rec.child_table
          );
          EXECUTE format(
            'CREATE POLICY parent_visibility_restrict_update ON %I.%I AS RESTRICTIVE FOR UPDATE USING (%s) WITH CHECK (%s)',
            rec.child_schema,
            rec.child_table,
            rec.using_sql,
            rec.using_sql
          );
          EXECUTE format(
            'DROP POLICY IF EXISTS parent_visibility_restrict_delete ON %I.%I',
            rec.child_schema,
            rec.child_table
          );
          EXECUTE format(
            'CREATE POLICY parent_visibility_restrict_delete ON %I.%I AS RESTRICTIVE FOR DELETE USING (%s)',
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
              AND p.polcmd IN ('a', '*')
          )
          INTO has_permissive_insert;

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
              AND p.polcmd IN ('w', '*')
          )
          INTO has_permissive_update;

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
              AND p.polcmd IN ('d', '*')
          )
          INTO has_permissive_delete;

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

           IF NOT has_permissive_insert THEN
             EXECUTE format(
               'DROP POLICY IF EXISTS parent_visibility_allow_insert ON %I.%I',
               rec.child_schema,
               rec.child_table
             );
             EXECUTE format(
               'CREATE POLICY parent_visibility_allow_insert ON %I.%I FOR INSERT WITH CHECK (true)',
               rec.child_schema,
               rec.child_table
             );
           END IF;

           IF NOT has_permissive_update THEN
             EXECUTE format(
               'DROP POLICY IF EXISTS parent_visibility_allow_update ON %I.%I',
               rec.child_schema,
               rec.child_table
             );
             EXECUTE format(
               'CREATE POLICY parent_visibility_allow_update ON %I.%I FOR UPDATE USING (true) WITH CHECK (true)',
               rec.child_schema,
               rec.child_table
             );
           END IF;

           IF NOT has_permissive_delete THEN
             EXECUTE format(
               'DROP POLICY IF EXISTS parent_visibility_allow_delete ON %I.%I',
               rec.child_schema,
               rec.child_table
             );
             EXECUTE format(
               'CREATE POLICY parent_visibility_allow_delete ON %I.%I FOR DELETE USING (true)',
               rec.child_schema,
               rec.child_table
             );
           END IF;

           EXCEPTION WHEN OTHERS THEN
            RAISE WARNING 'Failed on %.%: %', rec.child_schema, rec.child_table, SQLERRM;
          END;
         END LOOP;
       END
       $$;"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '34' WHERE item = 'Last patch number';"
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

    message("Patch 34 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 34 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
