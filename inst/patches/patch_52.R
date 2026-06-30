# Patch 52: guideline publisher and series name uniqueness

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 52: enforcing unique guideline publisher and series names, adding sample document links, guarding used parameter units, and adding water use licence tables."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

active <- FALSE
active <- dbTransBegin(con)

patch_package_version <- tryCatch(
  as.character(packageVersion("AquaCache")),
  error = function(e) {
    if (file.exists("DESCRIPTION")) {
      desc <- read.dcf("DESCRIPTION")
      if ("Version" %in% colnames(desc)) {
        return(desc[1, "Version"])
      }
    }
    NA_character_
  }
)

tryCatch(
  {
    check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('criteria.guideline_publishers') IS NOT NULL AS has_publishers,
         to_regclass('criteria.guideline_series') IS NOT NULL AS has_series,
         to_regclass('criteria.guidelines') IS NOT NULL AS has_guidelines,
         to_regclass('criteria.guideline_jurisdictions') IS NOT NULL AS has_guideline_jurisdictions,
         to_regclass('discrete.samples') IS NOT NULL AS has_samples,
         to_regclass('files.documents') IS NOT NULL AS has_documents,
         to_regclass('public.locations') IS NOT NULL AS has_locations,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )

    if (
      !isTRUE(check$has_publishers[[1]]) ||
        !isTRUE(check$has_series[[1]]) ||
        !isTRUE(check$has_guidelines[[1]]) ||
        !isTRUE(check$has_guideline_jurisdictions[[1]]) ||
        !isTRUE(check$has_samples[[1]]) ||
        !isTRUE(check$has_documents[[1]]) ||
        !isTRUE(check$has_locations[[1]]) ||
        !isTRUE(check$has_version_info[[1]])
    ) {
      stop(
        "Patch 52 requires criteria.guideline_publishers, criteria.guideline_series, criteria.guidelines, criteria.guideline_jurisdictions, discrete.samples, files.documents, public.locations, and information.version_info to already exist."
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE criteria.guideline_publishers
       SET publisher_name = btrim(publisher_name)
       WHERE publisher_name <> btrim(publisher_name);"
    )

    DBI::dbExecute(
      con,
      "UPDATE criteria.guideline_series
       SET series_name = btrim(series_name)
       WHERE series_name <> btrim(series_name);"
    )

    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_publishers_name_lwr_key
       ON criteria.guideline_publishers (lower(btrim(publisher_name)));"
    )

    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS guideline_series_name_lwr_key
       ON criteria.guideline_series (lower(btrim(series_name)));"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON INDEX criteria.guideline_publishers_name_lwr_key IS
       'Enforces case-insensitive uniqueness of guideline publisher names after trimming whitespace.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON INDEX criteria.guideline_series_name_lwr_key IS
       'Enforces case-insensitive uniqueness of guideline series names after trimming whitespace.';"
    )

    # Add a guard function to prevent changing parameter units if results exist for that matrix state
    DBI::dbExecute(
      con,
      "
    CREATE OR REPLACE FUNCTION public.parameter_matrix_state_has_results(
      p_parameter_id integer,
      p_matrix_state_code text
    )
    RETURNS boolean AS $$
      SELECT
        EXISTS (
          SELECT 1
          FROM public.matrix_states AS ms
          JOIN discrete.results AS r
            ON r.matrix_state_id = ms.matrix_state_id
          WHERE r.parameter_id = p_parameter_id
            AND ms.matrix_state_code = p_matrix_state_code
        )
        OR EXISTS (
          SELECT 1
          FROM public.matrix_states AS ms
          JOIN continuous.timeseries AS ts
            ON ts.matrix_state_id = ms.matrix_state_id
          WHERE ts.parameter_id = p_parameter_id
            AND ms.matrix_state_code = p_matrix_state_code
            AND EXISTS (
              SELECT 1
              FROM continuous.measurements_continuous AS mc
              WHERE mc.timeseries_id = ts.timeseries_id
            )
        );
    $$ LANGUAGE sql STABLE;
    "
    )

    DBI::dbExecute(
      con,
      "
    COMMENT ON FUNCTION public.parameter_matrix_state_has_results(integer, text)
    IS 'Returns true when a parameter has discrete results or continuous measurements for the requested matrix state.';
    "
    )

    DBI::dbExecute(
      con,
      "
    CREATE OR REPLACE FUNCTION public.prevent_used_parameter_unit_update()
    RETURNS trigger AS $$
    BEGIN
      IF OLD.units_liquid IS NOT NULL
          AND OLD.units_liquid IS DISTINCT FROM NEW.units_liquid
          AND public.parameter_matrix_state_has_results(
            OLD.parameter_id,
            'liquid'
          ) THEN
        RAISE EXCEPTION
          'Cannot change liquid unit for parameter_id %. Existing liquid results or continuous measurements use the assigned unit. If you truly need to change the units, you should disable this trigger (DROP TRIGGER prevent_used_parameter_unit_update ON public.parameters;), make the change, UPDATE ALL VALUES IN THE DATABASE USING THIS PARAMETER, and then re-enable the trigger.',
          OLD.parameter_id
          USING ERRCODE = 'check_violation';
      END IF;
    
      IF OLD.units_solid IS NOT NULL
          AND OLD.units_solid IS DISTINCT FROM NEW.units_solid
          AND public.parameter_matrix_state_has_results(
            OLD.parameter_id,
            'solid'
          ) THEN
        RAISE EXCEPTION
          'Cannot change solid unit for parameter_id %. Existing solid results or continuous measurements use the assigned unit. If you truly need to change the units, you should disable this trigger (DROP TRIGGER prevent_used_parameter_unit_update ON public.parameters;), make the change, UPDATE ALL VALUES IN THE DATABASE USING THIS PARAMETER, and then re-enable the trigger.',
          OLD.parameter_id
          USING ERRCODE = 'check_violation';
      END IF;
    
      IF OLD.units_gas IS NOT NULL
          AND OLD.units_gas IS DISTINCT FROM NEW.units_gas
          AND public.parameter_matrix_state_has_results(
            OLD.parameter_id,
            'gas'
          ) THEN
        RAISE EXCEPTION
          'Cannot change gas unit for parameter_id %. Existing gas results or continuous measurements use the assigned unit. If you truly need to change the units, you should disable this trigger (DROP TRIGGER prevent_used_parameter_unit_update ON public.parameters;), make the change, UPDATE ALL VALUES IN THE DATABASE USING THIS PARAMETER, and then re-enable the trigger.',
          OLD.parameter_id
          USING ERRCODE = 'check_violation';
      END IF;
    
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    "
    )

    DBI::dbExecute(
      con,
      "
    COMMENT ON FUNCTION public.prevent_used_parameter_unit_update()
    IS 'Prevents changing assigned parameter unit columns after results exist for the corresponding matrix state.';
    "
    )

    DBI::dbExecute(
      con,
      "
    DROP TRIGGER IF EXISTS prevent_used_parameter_unit_update ON public.parameters;
    "
    )

    DBI::dbExecute(
      con,
      "
    CREATE TRIGGER prevent_used_parameter_unit_update
    BEFORE UPDATE OF units_liquid, units_solid, units_gas
    ON public.parameters
    FOR EACH ROW
    EXECUTE FUNCTION public.prevent_used_parameter_unit_update();
    "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS discrete.sample_documents (
        sample_id integer NOT NULL
          REFERENCES discrete.samples(sample_id)
          ON UPDATE CASCADE
          ON DELETE CASCADE,
        document_id integer NOT NULL
          REFERENCES files.documents(document_id)
          ON UPDATE CASCADE
          ON DELETE CASCADE,
        document_role text NOT NULL DEFAULT 'supporting',
        note text,
        created_by text NOT NULL DEFAULT current_user,
        modified_by text,
        created timestamp with time zone NOT NULL DEFAULT current_timestamp,
        modified timestamp with time zone,
        PRIMARY KEY (sample_id, document_id),
        CONSTRAINT sample_documents_document_role_not_blank
          CHECK (btrim(document_role) <> '')
      );
      "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_documents
       DROP COLUMN IF EXISTS link_source;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_documents OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON TABLE discrete.sample_documents IS
      'Associates discrete samples with supporting records in files.documents. This is the canonical sample-document relationship table.';
      "
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON COLUMN discrete.sample_documents.document_role IS
      'Short business role for the linked document, such as supporting, lab_submission, COA, field_note, or image.';
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS sample_documents_document_id_idx
      ON discrete.sample_documents(document_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS sample_documents_role_idx
      ON discrete.sample_documents(document_role);
      "
    )

    sample_documents_legacy_column <- DBI::dbGetQuery(
      con,
      "
      SELECT EXISTS (
        SELECT 1
        FROM information_schema.columns
        WHERE table_schema = 'discrete'
          AND table_name = 'samples'
          AND column_name = 'documents'
      ) AS exists;
      "
    )

    if (isTRUE(sample_documents_legacy_column$exists[[1]])) {
      invalid_sample_documents <- DBI::dbGetQuery(
        con,
        "
        SELECT count(*)::integer AS n
        FROM discrete.samples AS s
        CROSS JOIN LATERAL unnest(s.documents) AS sample_doc(document_id)
        LEFT JOIN files.documents AS d
          ON d.document_id = sample_doc.document_id
        WHERE s.documents IS NOT NULL
          AND (
            sample_doc.document_id IS NULL
            OR d.document_id IS NULL
          );
        "
      )

      if (invalid_sample_documents$n[[1]] > 0L) {
        stop(
          "Cannot migrate discrete.samples.documents. At least one sample document array contains a NULL or a document_id that is not present in files.documents."
        )
      }

      sample_metadata_views <- DBI::dbGetQuery(
        con,
        "
        SELECT
          n.nspname AS schema_name,
          c.relname AS view_name,
          pg_get_viewdef(c.oid, true) AS view_definition
        FROM pg_class c
        JOIN pg_namespace n
          ON n.oid = c.relnamespace
        WHERE n.nspname = 'discrete'
          AND c.relname IN (
            'samples_metadata_en',
            'samples_metadata_fr',
            'results_metadata_en',
            'results_metadata_fr'
          )
          AND c.relkind = 'v';
        "
      )

      if (nrow(sample_metadata_views) > 0L) {
        sample_metadata_views$view_definition <- gsub(
          "\\n[[:space:]]*s\\.documents,[[:space:]]*",
          "\n",
          sample_metadata_views$view_definition
        )

        DBI::dbExecute(
          con,
          "DROP VIEW IF EXISTS
             discrete.results_metadata_en,
             discrete.results_metadata_fr;"
        )
        DBI::dbExecute(
          con,
          "DROP VIEW IF EXISTS
             discrete.samples_metadata_en,
             discrete.samples_metadata_fr;"
        )
      }

      DBI::dbExecute(
        con,
        "
        INSERT INTO discrete.sample_documents (
          sample_id,
          document_id,
          document_role,
          note,
          created_by,
          created
        )
        SELECT
          s.sample_id,
          sample_doc.document_id,
          'supporting',
          'Migrated from discrete.samples.documents by patch 52.',
          current_user,
          COALESCE(s.created, current_timestamp)
        FROM discrete.samples AS s
        CROSS JOIN LATERAL unnest(s.documents) AS sample_doc(document_id)
        WHERE s.documents IS NOT NULL
        ON CONFLICT (sample_id, document_id) DO NOTHING;
        "
      )

      DBI::dbExecute(
        con,
        "DROP TRIGGER IF EXISTS validate_documents_trigger
         ON discrete.samples;"
      )

      DBI::dbExecute(
        con,
        "DROP TRIGGER IF EXISTS sync_sample_documents_from_array
         ON discrete.samples;"
      )

      DBI::dbExecute(
        con,
        "DROP FUNCTION IF EXISTS discrete.sync_sample_documents_from_array();"
      )

      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.samples
         DROP COLUMN documents;"
      )

      if (nrow(sample_metadata_views) > 0L) {
        recreate_order <- c(
          "samples_metadata_en",
          "samples_metadata_fr",
          "results_metadata_en",
          "results_metadata_fr"
        )

        for (view_name in recreate_order) {
          view_row <- sample_metadata_views[
            sample_metadata_views$view_name == view_name,
            ,
            drop = FALSE
          ]
          if (!nrow(view_row)) {
            next
          }

          DBI::dbExecute(
            con,
            sprintf(
              "CREATE VIEW discrete.%s
               WITH (security_invoker = true, security_barrier = true)
               AS
               %s",
              view_name,
              view_row$view_definition[[1]]
            )
          )
        }

        DBI::dbExecute(
          con,
          "COMMENT ON VIEW discrete.samples_metadata_en IS
           'English-language view that flattens key discrete sample metadata into a reader-friendly dataset with location, media, method, type, quality, and organization names.'"
        )
        DBI::dbExecute(
          con,
          "COMMENT ON VIEW discrete.samples_metadata_fr IS
           'French-language view that flattens key discrete sample metadata into a reader-friendly dataset with location, media, method, type, quality, and organization names.'"
        )
        DBI::dbExecute(
          con,
          "COMMENT ON VIEW discrete.results_metadata_en IS
           'English-language view that exposes discrete analytical results with sample context and reader-friendly parameter, unit, fraction, speciation, condition, method, and laboratory names.'"
        )
        DBI::dbExecute(
          con,
          "COMMENT ON VIEW discrete.results_metadata_fr IS
           'French-language view that exposes discrete analytical results with sample context and reader-friendly parameter, unit, fraction, speciation, condition, method, and laboratory names.'"
        )

        existing_roles <- DBI::dbGetQuery(
          con,
          "SELECT rolname FROM pg_roles"
        )$rolname
        view_select_roles <- intersect(
          c(
            "admin",
            "public_reader",
            "yg_reader_group",
            "yg_reader",
            "yg_editor_group",
            "yg_editor",
            "cyfn_editor_group",
            "cyfn_editor",
            "tkc_group",
            "tkc_editor"
          ),
          existing_roles
        )

        for (role_name in view_select_roles) {
          for (view_name in recreate_order) {
            DBI::dbExecute(
              con,
              paste0(
                "GRANT SELECT ON TABLE discrete.",
                view_name,
                " TO ",
                DBI::dbQuoteIdentifier(con, role_name),
                ";"
              )
            )
          }
        }
      }
    }

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS sync_sample_documents_from_array
       ON discrete.samples;"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS discrete.sync_sample_documents_from_array();"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS public.validate_documents_array();"
    )

    DBI::dbExecute(
      con,
      "
      ALTER TABLE discrete.sample_documents ENABLE ROW LEVEL SECURITY;
      "
    )

    DBI::dbExecute(
      con,
      "
      DROP POLICY IF EXISTS sample_documents_parent_sample_access
      ON discrete.sample_documents;
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE POLICY sample_documents_parent_sample_access
      ON discrete.sample_documents
      FOR ALL
      USING (
        EXISTS (
          SELECT 1
          FROM discrete.samples AS s
          WHERE s.sample_id = sample_documents.sample_id
        )
      )
      WITH CHECK (
        EXISTS (
          SELECT 1
          FROM discrete.samples AS s
          WHERE s.sample_id = sample_documents.sample_id
        )
      );
      "
    )

    sample_document_roles <- DBI::dbGetQuery(
      con,
      "SELECT public.get_shareable_principals_for('discrete.samples') AS role_name;"
    )

    for (role_name in sample_document_roles$role_name) {
      DBI::dbExecute(
        con,
        paste0(
          "GRANT SELECT, INSERT, UPDATE, DELETE ON discrete.sample_documents TO ",
          DBI::dbQuoteIdentifier(con, role_name),
          ";"
        )
      )
    }

    # Water use licence catalogue and link tables ###########################
    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS criteria.licence_types (
        licence_type_id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        licence_type_name_en text NOT NULL,
        licence_type_name_fr text NOT NULL,
        description_en text,
        description_fr text,
        active boolean NOT NULL DEFAULT true,
        sort_order integer NOT NULL DEFAULT 100,
        created_by text NOT NULL DEFAULT current_user,
        modified_by text,
        created timestamp with time zone NOT NULL DEFAULT current_timestamp,
        modified timestamp with time zone,
        CONSTRAINT licence_types_name_en_not_blank
          CHECK (btrim(licence_type_name_en) <> ''),
        CONSTRAINT licence_types_name_fr_not_blank
          CHECK (btrim(licence_type_name_fr) <> '')
      );
      "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.licence_types OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE UNIQUE INDEX IF NOT EXISTS licence_types_name_en_lwr_key
      ON criteria.licence_types (lower(btrim(licence_type_name_en)));
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE UNIQUE INDEX IF NOT EXISTS licence_types_name_fr_lwr_key
      ON criteria.licence_types (lower(btrim(licence_type_name_fr)));
      "
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON TABLE criteria.licence_types IS
      'Controlled water use licence categories used to classify licences associated with AquaCache locations.';
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS criteria.licence_authorities (
        licence_authority_id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        jurisdiction_id integer
          REFERENCES criteria.guideline_jurisdictions(jurisdiction_id)
          ON UPDATE CASCADE
          ON DELETE RESTRICT,
        authority_name_en text NOT NULL,
        authority_name_fr text NOT NULL,
        description_en text,
        description_fr text,
        active boolean NOT NULL DEFAULT true,
        sort_order integer NOT NULL DEFAULT 100,
        created_by text NOT NULL DEFAULT current_user,
        modified_by text,
        created timestamp with time zone NOT NULL DEFAULT current_timestamp,
        modified timestamp with time zone,
        CONSTRAINT licence_authorities_name_en_not_blank
          CHECK (btrim(authority_name_en) <> ''),
        CONSTRAINT licence_authorities_name_fr_not_blank
          CHECK (btrim(authority_name_fr) <> '')
      );
      "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.licence_authorities OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE UNIQUE INDEX IF NOT EXISTS licence_authorities_name_en_lwr_key
      ON criteria.licence_authorities (lower(btrim(authority_name_en)));
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE UNIQUE INDEX IF NOT EXISTS licence_authorities_name_fr_lwr_key
      ON criteria.licence_authorities (lower(btrim(authority_name_fr)));
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licence_authorities_jurisdiction_id_idx
      ON criteria.licence_authorities(jurisdiction_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON TABLE criteria.licence_authorities IS
      'Organizations that issue, regulate, or enforce water use licences or related authorizations.';
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS criteria.licences (
        licence_id integer PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
        licence_type_id integer NOT NULL
          REFERENCES criteria.licence_types(licence_type_id)
          ON UPDATE CASCADE
          ON DELETE RESTRICT,
        licence_number text NOT NULL,
        licence_name text,
        licence_holder text,
        issued_date date,
        expiry_date date,
        issuing_authority_id integer
          REFERENCES criteria.licence_authorities(licence_authority_id)
          ON UPDATE CASCADE
          ON DELETE RESTRICT,
        enforcement_authority_id integer
          REFERENCES criteria.licence_authorities(licence_authority_id)
          ON UPDATE CASCADE
          ON DELETE RESTRICT,
        licence_url text,
        note text,
        active boolean NOT NULL DEFAULT true,
        created_by text NOT NULL DEFAULT current_user,
        modified_by text,
        created timestamp with time zone NOT NULL DEFAULT current_timestamp,
        modified timestamp with time zone,
        CONSTRAINT licences_licence_number_not_blank
          CHECK (btrim(licence_number) <> ''),
        CONSTRAINT licences_expiry_after_issued
          CHECK (
            issued_date IS NULL
            OR expiry_date IS NULL
            OR expiry_date >= issued_date
          )
      );
      "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.licences OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE UNIQUE INDEX IF NOT EXISTS licences_licence_number_lwr_key
      ON criteria.licences (lower(btrim(licence_number)));
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licences_licence_type_id_idx
      ON criteria.licences(licence_type_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licences_issued_date_idx
      ON criteria.licences(issued_date);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licences_expiry_date_idx
      ON criteria.licences(expiry_date);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licences_issuing_authority_id_idx
      ON criteria.licences(issuing_authority_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licences_enforcement_authority_id_idx
      ON criteria.licences(enforcement_authority_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON TABLE criteria.licences IS
      'Water use licences or related authorizations that may be associated with AquaCache locations. AquaCache stores licence metadata and external links, but is not the licensing authority.';
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS criteria.licence_documents (
        licence_id integer NOT NULL
          REFERENCES criteria.licences(licence_id)
          ON UPDATE CASCADE
          ON DELETE CASCADE,
        document_id integer NOT NULL
          REFERENCES files.documents(document_id)
          ON UPDATE CASCADE
          ON DELETE CASCADE,
        document_role text NOT NULL DEFAULT 'licence',
        note text,
        created_by text NOT NULL DEFAULT current_user,
        modified_by text,
        created timestamp with time zone NOT NULL DEFAULT current_timestamp,
        modified timestamp with time zone,
        PRIMARY KEY (licence_id, document_id),
        CONSTRAINT licence_documents_document_role_not_blank
          CHECK (btrim(document_role) <> '')
      );
      "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.licence_documents OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licence_documents_document_id_idx
      ON criteria.licence_documents(document_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS licence_documents_document_role_idx
      ON criteria.licence_documents(document_role);
      "
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON TABLE criteria.licence_documents IS
      'Optional relationship between water use licences and records in files.documents.';
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE TABLE IF NOT EXISTS criteria.location_licences (
        location_id integer NOT NULL
          REFERENCES public.locations(location_id)
          ON UPDATE CASCADE
          ON DELETE CASCADE,
        licence_id integer NOT NULL
          REFERENCES criteria.licences(licence_id)
          ON UPDATE CASCADE
          ON DELETE CASCADE,
        relationship_role text NOT NULL DEFAULT 'associated',
        note text,
        created_by text NOT NULL DEFAULT current_user,
        modified_by text,
        created timestamp with time zone NOT NULL DEFAULT current_timestamp,
        modified timestamp with time zone,
        PRIMARY KEY (location_id, licence_id),
        CONSTRAINT location_licences_relationship_role_not_blank
          CHECK (btrim(relationship_role) <> '')
      );
      "
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE criteria.location_licences OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS location_licences_licence_id_idx
      ON criteria.location_licences(licence_id);
      "
    )

    DBI::dbExecute(
      con,
      "
      CREATE INDEX IF NOT EXISTS location_licences_relationship_role_idx
      ON criteria.location_licences(relationship_role);
      "
    )

    DBI::dbExecute(
      con,
      "
      COMMENT ON TABLE criteria.location_licences IS
      'Many-to-many relationship between AquaCache locations and water use licences or related authorizations.';
      "
    )

    DBI::dbExecute(
      con,
      "
      INSERT INTO criteria.licence_types (
        licence_type_name_en,
        licence_type_name_fr,
        description_en,
        description_fr,
        sort_order
      )
      VALUES
        (
          'placer mining',
          'exploitation minière des placers',
          'Water use licence for placer mining activities.',
          'Permis d''utilisation de l''eau pour les activités d''exploitation minière des placers.',
          10
        ),
        (
          'conservation',
          'conservation',
          'Water use licence for conservation activities.',
          'Permis d''utilisation de l''eau pour les activités de conservation.',
          20
        ),
        (
          'hydroelectric generation',
          'production hydroélectrique',
          'Water use licence for hydroelectric generation activities.',
          'Permis d''utilisation de l''eau pour les activités de production hydroélectrique.',
          30
        ),
        (
          'municipal',
          'municipal',
          'Water use licence for municipal activities.',
          'Permis d''utilisation de l''eau pour les activités municipales.',
          40
        ),
        (
          'miscellaneous',
          'divers',
          'Water use licence for miscellaneous activities.',
          'Permis d''utilisation de l''eau pour les activités diverses.',
          50
        ),
        (
          'quartz (hard rock) mining',
          'exploitation minière quartz (roche dure)',
          'Water use licence for quartz (hard rock) mining activities.',
          'Permis d''utilisation de l''eau pour les activités d''exploitation minière quartz (roche dure).',
          60
        ),
        (
          'recreational',
          'récréatif',
          'Water use licence for recreational activities.',
          'Permis d''utilisation de l''eau pour les activités récréatives.',
          70
        ),
        (
          'industrial',
          'industriel',
          'Water use licence for industrial activities.',
          'Permis d''utilisation de l''eau pour les activités industrielles.',
          80
        ),
        (
          'agricultural',
          'agricole',
          'Water use licence for agricultural activities.',
          'Permis d''utilisation de l''eau pour les activités agricoles.',
          90
        )
      ON CONFLICT DO NOTHING;
      "
    )

    DBI::dbExecute(
      con,
      "
      INSERT INTO criteria.licence_authorities (
        jurisdiction_id,
        authority_name_en,
        authority_name_fr,
        sort_order
      )
      SELECT
        gj.jurisdiction_id,
        src.authority_name_en,
        src.authority_name_fr,
        src.sort_order
      FROM (
        VALUES
          (
            'YT',
            'Yukon Water Board',
            'Office des eaux du Yukon',
            10
          ),
          (
            'YT',
            'Yukon Department of Environment, Standards and Approvals',
            'Ministère de l''Environnement du Yukon, Normes et approbations',
            20
          ),
          (
            'YT',
            'Yukon Department of Energy, Mines, and Resources Compliance, Monitoring and Inspections',
            'Ministère de l''Énergie, des Mines et des Ressources du Yukon, Conformité, surveillance et inspections',
            30
          ),
          (
            'YT',
            'Yukon Department of Environment, Environmental Compliance and Inspections',
            'Ministère de l''Environnement du Yukon, Conformité et inspections environnementales',
            40
          ),
          (
            'CA',
            'Department of Fisheries and Oceans (Canada)',
            'Ministère des Pêches et des Océans (Canada)',
            50
          )
      ) src(jurisdiction_code, authority_name_en, authority_name_fr, sort_order)
      LEFT JOIN criteria.guideline_jurisdictions gj
        ON gj.jurisdiction_code = src.jurisdiction_code
      ON CONFLICT DO NOTHING;
      "
    )

    for (table_name in c(
      "licence_types",
      "licence_authorities",
      "licences",
      "licence_documents",
      "location_licences"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_user_modified
           ON criteria.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON criteria.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified();",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_update_modified
           ON criteria.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON criteria.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified();",
          table_name,
          table_name
        )
      )
    }

    licence_tables <- c(
      "criteria.licence_types",
      "criteria.licence_authorities",
      "criteria.licences",
      "criteria.licence_documents",
      "criteria.location_licences"
    )
    guideline_definition_tables <- c(
      "criteria.guideline_comparison_operators",
      "criteria.guideline_value_algorithms",
      "criteria.guideline_jurisdictions",
      "criteria.guideline_jurisdiction_levels",
      "criteria.guideline_protection_goals",
      "criteria.guideline_exposure_durations",
      "criteria.guideline_averaging_periods",
      "criteria.guideline_publishers",
      "criteria.guideline_series",
      "criteria.guidelines",
      "criteria.guidelines_media_types",
      "criteria.guidelines_fractions",
      "criteria.guideline_value_rules",
      "criteria.guideline_rule_inputs",
      "criteria.guideline_rule_coefficients",
      "criteria.guideline_narrative_values",
      "criteria.guideline_locations"
    )
    add_guidelines_dml_tables <- c(
      "criteria.guidelines",
      "criteria.guideline_value_rules",
      "criteria.guideline_rule_inputs",
      "criteria.guideline_rule_coefficients",
      "criteria.guideline_narrative_values",
      "criteria.guidelines_fractions",
      "criteria.guidelines_media_types",
      "criteria.guideline_locations"
    )
    add_guidelines_lookup_write_tables <- c(
      "criteria.guideline_publishers",
      "criteria.guideline_series",
      "criteria.guideline_jurisdictions",
      "criteria.guideline_protection_goals",
      "criteria.guideline_exposure_durations",
      "criteria.guideline_averaging_periods"
    )

    q_literal <- function(x) {
      as.character(DBI::dbQuoteString(con, x))
    }
    q_role <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }
    sql_text_array <- function(x) {
      if (length(x) == 0L) {
        return("ARRAY[]::text[]")
      }

      sprintf(
        "ARRAY[%s]::text[]",
        paste(vapply(x, q_literal, character(1)), collapse = ", ")
      )
    }

    grant_schema_usage <- function(schema_name, roles) {
      if (length(roles) == 0L) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT USAGE ON SCHEMA %s TO %s;",
            schema_name,
            q_role(role_name)
          )
        )
      }

      invisible(NULL)
    }

    grant_table_privileges <- function(table_names, privileges, roles) {
      if (length(roles) == 0L) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON TABLE %s TO %s;",
            privileges,
            paste(table_names, collapse = ", "),
            q_role(role_name)
          )
        )
      }

      invisible(NULL)
    }

    grant_sequence_privileges <- function(sequence_names, privileges, roles) {
      sequence_names <- sequence_names[!is.na(sequence_names)]
      if (length(sequence_names) == 0L || length(roles) == 0L) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON SEQUENCE %s TO %s;",
            privileges,
            paste(sequence_names, collapse = ", "),
            q_role(role_name)
          )
        )
      }

      invisible(NULL)
    }

    existing_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname

    get_roles_with_reference_privileges <- function(
      reference_table,
      privileges,
      always_include = character()
    ) {
      roles <- DBI::dbGetQuery(
        con,
        sprintf(
          "
          SELECT x.role_name
          FROM (
            SELECT r.rolname AS role_name
            FROM pg_roles AS r
            WHERE r.rolcanlogin = false
              AND r.rolname NOT IN (
                'public',
                'pg_read_all_data',
                'pg_write_all_data'
              )
              AND NOT EXISTS (
                SELECT 1
                FROM unnest(%s) AS p(privilege_name)
                WHERE NOT has_table_privilege(
                  r.oid,
                  %s::regclass,
                  p.privilege_name
                )
              )

            UNION

            SELECT r.rolname AS role_name
            FROM pg_roles AS r
            WHERE r.rolname = ANY(%s)
          ) AS x
          ORDER BY x.role_name;",
          sql_text_array(privileges),
          q_literal(reference_table),
          sql_text_array(always_include)
        )
      )$role_name

      intersect(unique(roles), existing_roles)
    }

    # Guideline definitions and licence metadata are public catalogue data.
    # Give every DB role read access, then mirror explicit reader grants from
    # existing criteria tables so deployments with non-PUBLIC reader groups
    # remain consistent.
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA criteria TO PUBLIC;")
    DBI::dbExecute(
      con,
      sprintf(
        "GRANT SELECT ON TABLE %s TO PUBLIC;",
        paste(c(guideline_definition_tables, licence_tables), collapse = ", ")
      )
    )

    criteria_reader_roles <- unique(c(
      get_roles_with_reference_privileges(
        "criteria.guidelines",
        "SELECT",
        "public_reader"
      ),
      get_roles_with_reference_privileges(
        "criteria.guideline_publishers",
        "SELECT"
      )
    ))
    grant_schema_usage("criteria", criteria_reader_roles)
    grant_table_privileges(
      c(guideline_definition_tables, licence_tables),
      "SELECT",
      criteria_reader_roles
    )

    # Editor roles vary by deployment. Reuse existing edit access on related
    # location, document, sample, and guideline tables instead of assuming a
    # fixed set of Yukon or partner group names.
    criteria_editor_roles <- unique(c(
      get_roles_with_reference_privileges(
        "public.locations",
        c("INSERT", "UPDATE", "DELETE"),
        "admin"
      ),
      get_roles_with_reference_privileges(
        "files.documents",
        c("INSERT", "UPDATE", "DELETE")
      ),
      get_roles_with_reference_privileges(
        "discrete.samples",
        c("INSERT", "UPDATE", "DELETE")
      ),
      get_roles_with_reference_privileges(
        "criteria.guidelines",
        c("INSERT", "UPDATE", "DELETE")
      )
    ))
    criteria_editor_roles <- setdiff(
      criteria_editor_roles,
      c("public", "public_reader")
    )

    grant_schema_usage("criteria", criteria_editor_roles)
    grant_table_privileges(
      add_guidelines_dml_tables,
      "SELECT, INSERT, UPDATE, DELETE",
      criteria_editor_roles
    )
    grant_table_privileges(
      add_guidelines_lookup_write_tables,
      "SELECT, INSERT, UPDATE",
      criteria_editor_roles
    )
    grant_table_privileges(
      licence_tables,
      "SELECT, INSERT, UPDATE, DELETE",
      criteria_editor_roles
    )

    criteria_write_tables <- c(
      add_guidelines_dml_tables,
      add_guidelines_lookup_write_tables,
      licence_tables
    )
    criteria_identity_sequences <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        SELECT pg_get_serial_sequence(
                 format('%%I.%%I', table_schema, table_name),
                 column_name
               ) AS sequence_name
        FROM information_schema.columns
        WHERE table_schema = 'criteria'
          AND table_name IN (%s)
          AND is_identity = 'YES'
        ORDER BY table_name, ordinal_position;",
        paste(
          vapply(
            sub("^criteria\\.", "", criteria_write_tables),
            q_literal,
            character(1)
          ),
          collapse = ", "
        )
      )
    )$sequence_name
    grant_sequence_privileges(
      criteria_identity_sequences,
      "USAGE, SELECT",
      criteria_editor_roles
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '52'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        patch_package_version,
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    DBI::dbExecute(con, "COMMIT;")
    active <- FALSE

    message("Patch 52 applied successfully.")
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
