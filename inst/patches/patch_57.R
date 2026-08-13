# Patch 57 adds sample groups so quality-control samples can apply to a set of
# samples rather than one location or one linked sample. It also permits
# locationless blank and control samples while retaining location requirements
# for environmental samples. It also documents route-level configuration fields
# used by registered transmission adapters so clients can render guided inputs.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 57: adding sample groups, location-optional QC samples, and updating transmission-adapter route-field documentation. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.samples') IS NOT NULL AS has_samples,
         to_regclass('discrete.sample_types') IS NOT NULL AS has_sample_types,
         to_regclass('discrete.samples_metadata_en') IS NOT NULL AS has_samples_metadata_en,
         to_regclass('discrete.samples_metadata_fr') IS NOT NULL AS has_samples_metadata_fr,
         to_regclass('public.organizations') IS NOT NULL AS has_organizations,
         to_regclass('public.source_adapter_capabilities') IS NOT NULL AS has_source_adapter_capabilities,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info,
         to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
         to_regprocedure('public.user_modified()') IS NOT NULL AS has_user_modified,
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_update_modified,
         to_regprocedure('public.validate_share_with()') IS NOT NULL AS has_validate_share_with,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 57 requires the discrete sample tables and metadata views, organization and version tables, sharing validation, and audit infrastructure created by earlier patches."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "56") {
      stop("Patch 57 must be applied to a database at Patch 56.")
    }

    nesdis_capability <- DBI::dbGetQuery(
      con,
      "SELECT source_fx
       FROM public.source_adapter_capabilities
       WHERE source_fx = 'downloadNESDIS'
         AND data_domain = 'continuous'"
    )
    if (nrow(nesdis_capability) != 1L) {
      stop(
        "Patch 57 requires the continuous downloadNESDIS capability created by Patch 56."
      )
    }

    message("Documenting downloadNESDIS route configuration fields...")
    DBI::dbExecute(
      con,
      "UPDATE public.source_adapter_capabilities
       SET ui_config = jsonb_set(
         ui_config,
         '{route_config_fields}',
         '[
           {
             \"name\": \"timestamp_floor_seconds\",
             \"path\": [\"parser_config\", \"timestamp_floor_seconds\"],
             \"label\": \"Observation-time alignment interval, seconds (optional)\",
             \"help\": \"Use this only when the message body contains no observation timestamp and observations belong on fixed clock boundaries. AquaCache rounds the GOES/LRGS header time down to this interval. For example, 3600 stores a message received at 22:09:33 as an observation at 22:00:00. Leave this blank when the payload supplies observation times or when the header time is the intended measurement time.\",
             \"placeholder\": \"For example, 3600 for hourly observations\",
             \"value_type\": \"integer\",
             \"control\": \"numeric\",
             \"required\": false,
             \"minimum\": 1,
             \"step\": 1,
             \"message_formats\": [\"BLM\"]
           }
         ]'::jsonb,
         true
       )
       WHERE source_fx = 'downloadNESDIS'
         AND data_domain = 'continuous'"
    )

    target_objects <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.sample_groups') IS NOT NULL AS has_sample_groups,
         to_regclass('discrete.sample_group_members') IS NOT NULL AS has_sample_group_members,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'sample_types'
             AND column_name IN (
               'requires_location',
               'requires_sample_group'
             )
         ) AS has_sample_type_flags"
    )
    if (any(unlist(target_objects[1, ], use.names = FALSE))) {
      stop(
        "Patch 57 target tables or columns already exist. Investigate the partial migration before applying this patch."
      )
    }

    sample_unique_constraint <- DBI::dbGetQuery(
      con,
      "SELECT oid
       FROM pg_constraint
       WHERE conrelid = 'discrete.samples'::regclass
         AND conname = 'samples_location_id_sub_location_id_media_id_z_datetime_sam_key'
         AND contype = 'u'"
    )
    if (nrow(sample_unique_constraint) != 1L) {
      stop(
        "Patch 57 requires the Patch 43 sample uniqueness constraint before it can make that rule location-specific."
      )
    }

    grouped_qc_types <- c(
      "QC-sample-field blank",
      "QC-sample-lab blank",
      "QC-sample-post-preservative blank",
      "QC-sample-pre-preservative blank",
      "QC-sample-reference sample",
      "QC-sample-trip blank",
      "QC-negative control",
      "sample-negative control",
      "sample-positive control"
    )
    grouped_qc_type_sql <- paste(
      vapply(
        grouped_qc_types,
        function(x) as.character(DBI::dbQuoteString(con, x)),
        character(1)
      ),
      collapse = ", "
    )
    existing_sample_types <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT sample_type
         FROM discrete.sample_types
         WHERE sample_type IN (%s)",
        grouped_qc_type_sql
      )
    )$sample_type
    missing_sample_types <- setdiff(grouped_qc_types, existing_sample_types)
    if (length(missing_sample_types)) {
      stop(
        "Patch 57 requires these sample types from earlier patches: ",
        paste(missing_sample_types, collapse = ", ")
      )
    }

    existing_grouped_qc <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT
           s.sample_id,
           st.sample_type
         FROM discrete.samples s
         JOIN discrete.sample_types st
           ON st.sample_type_id = s.sample_type
         WHERE st.sample_type IN (%s)
         ORDER BY s.sample_id
         LIMIT 20",
        grouped_qc_type_sql
      )
    )
    if (nrow(existing_grouped_qc)) {
      stop(
        "Patch 57 cannot infer sample-group membership for existing blank or control samples. Assign their intended groups in a data migration before applying this patch. Example sample_id(s): ",
        paste(existing_grouped_qc$sample_id, collapse = ", ")
      )
    }

    message("Adding sample-type context requirements...")
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_types
       ADD COLUMN requires_location BOOLEAN NOT NULL DEFAULT TRUE,
       ADD COLUMN requires_sample_group BOOLEAN NOT NULL DEFAULT FALSE"
    )
    DBI::dbExecute(
      con,
      sprintf(
        "UPDATE discrete.sample_types
         SET requires_location = FALSE,
             requires_sample_group = TRUE
         WHERE sample_type IN (%s)",
        grouped_qc_type_sql
      )
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_types.requires_location IS
       'When true, samples of this type must identify a monitoring location. Blank, reference, and control sample types may set this false.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_types.requires_sample_group IS
       'When true, every sample of this type must belong to at least one discrete.sample_group. This is required for blanks and controls whose meaning depends on a trip, field event, shipment, or laboratory batch.'"
    )

    message("Creating sample group tables...")
    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.sample_groups (
         sample_group_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
           (SEQUENCE NAME discrete.sample_group_id_seq),
         group_type TEXT NOT NULL CHECK (group_type IN (
           'field_event',
           'trip',
           'cooler',
           'shipment',
           'lab_batch',
           'qc_set',
           'other'
         )),
         group_code TEXT,
         group_name TEXT,
         start_datetime TIMESTAMPTZ,
         end_datetime TIMESTAMPTZ,
         owner INTEGER NOT NULL
           REFERENCES public.organizations(organization_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         contributor INTEGER
           REFERENCES public.organizations(organization_id)
           ON UPDATE CASCADE ON DELETE SET NULL,
         metadata JSONB NOT NULL DEFAULT '{}'::JSONB,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         share_with TEXT[] NOT NULL DEFAULT ARRAY['public_reader']::TEXT[],
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMPTZ,
         modified_by TEXT,
         CONSTRAINT sample_groups_identifier_present CHECK (
           NULLIF(btrim(group_code), '') IS NOT NULL
           OR NULLIF(btrim(group_name), '') IS NOT NULL
         ),
         CONSTRAINT sample_groups_group_code_not_blank CHECK (
           group_code IS NULL OR btrim(group_code) <> ''
         ),
         CONSTRAINT sample_groups_group_name_not_blank CHECK (
           group_name IS NULL OR btrim(group_name) <> ''
         ),
         CONSTRAINT sample_groups_period_valid CHECK (
           end_datetime IS NULL
           OR start_datetime IS NULL
           OR end_datetime >= start_datetime
         ),
         CONSTRAINT sample_groups_metadata_object CHECK (
           jsonb_typeof(metadata) = 'object'
         ),
         CONSTRAINT sample_groups_share_with_not_empty CHECK (
           cardinality(share_with) > 0
         )
       )"
    )
    DBI::dbExecute(con, "ALTER TABLE discrete.sample_groups OWNER TO admin")
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX sample_groups_owner_type_code_lwr_key
       ON discrete.sample_groups (
         owner,
         group_type,
         lower(btrim(group_code))
       )
       WHERE group_code IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX sample_groups_start_datetime_idx
       ON discrete.sample_groups (start_datetime, sample_group_id)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX sample_groups_share_with_gin_idx
       ON discrete.sample_groups USING GIN (share_with)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.sample_group_members (
         sample_group_member_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
           (SEQUENCE NAME discrete.sample_group_member_id_seq),
         sample_group_id INTEGER NOT NULL
           REFERENCES discrete.sample_groups(sample_group_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         sample_id INTEGER NOT NULL
           REFERENCES discrete.samples(sample_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         sequence_in_group INTEGER CHECK (
           sequence_in_group IS NULL OR sequence_in_group > 0
         ),
         note TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified TIMESTAMPTZ,
         modified_by TEXT,
         CONSTRAINT sample_group_members_group_sample_key
           UNIQUE (sample_group_id, sample_id)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_group_members OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX sample_group_members_sample_idx
       ON discrete.sample_group_members (sample_id, sample_group_id)"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_groups IS
       'Groups discrete samples that share a field event, trip, cooler, shipment, laboratory batch, or other quality-control context. A group can span multiple monitoring locations.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_groups.group_code IS
       'Owner-scoped external or operational identifier for the trip, shipment, batch, or other group.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_group_members IS
       'Many-to-many membership relating samples, including routine samples and blanks, to their shared sample-group context.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_group_members.sequence_in_group IS
       'Optional collection, handling, or processing order within the group.'"
    )

    message("Adding sample context constraints...")
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       ALTER COLUMN location_id DROP NOT NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       DROP CONSTRAINT samples_location_id_sub_location_id_media_id_z_datetime_sam_key"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX samples_location_context_unique_idx
       ON discrete.samples (
         location_id,
         sub_location_id,
         media_id,
         z,
         datetime,
         sample_type,
         collection_method
       ) NULLS NOT DISTINCT
       WHERE location_id IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       ADD CONSTRAINT samples_sublocation_requires_location CHECK (
         location_id IS NOT NULL OR sub_location_id IS NULL
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX samples_locationless_datetime_sample_id_idx
       ON discrete.samples (datetime, sample_id)
       WHERE location_id IS NULL"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.location_id IS
       'Monitoring location for an environmental sample. May be NULL only when the sample type does not require a location; every locationless sample must belong to a sample group.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.linked_with IS
       'Legacy one-to-one sample relationship. New trip, field-event, batch, blank, and control relationships use discrete.sample_groups and discrete.sample_group_members.'"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.enforce_sample_context_requirements()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, discrete, public
       AS $function$
       DECLARE
         type_requires_location BOOLEAN;
         type_label TEXT;
       BEGIN
         SELECT
           st.requires_location,
           st.sample_type
         INTO
           type_requires_location,
           type_label
         FROM discrete.sample_types st
         WHERE st.sample_type_id = NEW.sample_type;

         IF type_requires_location IS TRUE AND NEW.location_id IS NULL THEN
           RAISE EXCEPTION
             'Sample type % requires discrete.samples.location_id.',
             COALESCE(type_label, NEW.sample_type::TEXT)
             USING ERRCODE = '23514';
         END IF;

         IF NEW.location_id IS NULL AND NEW.sub_location_id IS NOT NULL THEN
           RAISE EXCEPTION
             'discrete.samples.sub_location_id cannot be set when location_id is NULL.'
             USING ERRCODE = '23514';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_sample_context_requirements()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION discrete.enforce_sample_context_requirements()
       FROM PUBLIC"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.enforce_sample_group_membership()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, discrete, public
       AS $function$
       DECLARE
         check_sample_ids INTEGER[];
         orphan_sample_id INTEGER;
       BEGIN
         IF TG_TABLE_NAME = 'sample_group_members' THEN
           check_sample_ids := ARRAY[OLD.sample_id];
           IF TG_OP = 'UPDATE' THEN
             check_sample_ids := check_sample_ids || NEW.sample_id;
           END IF;
         ELSE
           check_sample_ids := ARRAY[NEW.sample_id];
         END IF;

         SELECT s.sample_id
         INTO orphan_sample_id
         FROM discrete.samples s
         JOIN discrete.sample_types st
           ON st.sample_type_id = s.sample_type
         WHERE s.sample_id = ANY(check_sample_ids)
           AND (
             s.location_id IS NULL
             OR st.requires_sample_group
           )
           AND NOT EXISTS (
             SELECT 1
             FROM discrete.sample_group_members sgm
             WHERE sgm.sample_id = s.sample_id
           )
         LIMIT 1;

         IF orphan_sample_id IS NOT NULL THEN
           RAISE EXCEPTION
             'Sample_id % must belong to at least one discrete.sample_group.',
             orphan_sample_id
             USING ERRCODE = '23514';
         END IF;

         RETURN NULL;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_sample_group_membership()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION discrete.enforce_sample_group_membership()
       FROM PUBLIC"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.enforce_sample_type_context_requirements()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, discrete, public
       AS $function$
       DECLARE
         invalid_sample_id INTEGER;
       BEGIN
         IF NEW.requires_location THEN
           SELECT s.sample_id
           INTO invalid_sample_id
           FROM discrete.samples s
           WHERE s.sample_type = NEW.sample_type_id
             AND s.location_id IS NULL
           LIMIT 1;

           IF invalid_sample_id IS NOT NULL THEN
             RAISE EXCEPTION
               'Sample type % cannot require a location while sample_id % is locationless.',
               NEW.sample_type,
               invalid_sample_id
               USING ERRCODE = '23514';
           END IF;
         END IF;

         IF NEW.requires_sample_group THEN
           SELECT s.sample_id
           INTO invalid_sample_id
           FROM discrete.samples s
           WHERE s.sample_type = NEW.sample_type_id
             AND NOT EXISTS (
               SELECT 1
               FROM discrete.sample_group_members sgm
               WHERE sgm.sample_id = s.sample_id
             )
           LIMIT 1;

           IF invalid_sample_id IS NOT NULL THEN
             RAISE EXCEPTION
               'Sample type % cannot require sample groups while sample_id % has no group.',
               NEW.sample_type,
               invalid_sample_id
               USING ERRCODE = '23514';
           END IF;
         END IF;

         RETURN NULL;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_sample_type_context_requirements()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION discrete.enforce_sample_type_context_requirements()
       FROM PUBLIC"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER samples_context_requirements
       BEFORE INSERT OR UPDATE OF location_id, sub_location_id, sample_type
       ON discrete.samples
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_sample_context_requirements()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER samples_group_membership_required
       AFTER INSERT OR UPDATE OF location_id, sample_type
       ON discrete.samples
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_sample_group_membership()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER sample_group_members_group_membership_required
       AFTER DELETE OR UPDATE OF sample_id
       ON discrete.sample_group_members
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_sample_group_membership()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER sample_types_context_requirements
       AFTER UPDATE OF requires_location, requires_sample_group
       ON discrete.sample_types
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_sample_type_context_requirements()"
    )

    message("Adding sharing, auditing, and access controls...")
    for (table_name in c("sample_groups", "sample_group_members")) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER audit_%s_trigger
           AFTER INSERT OR UPDATE OR DELETE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()",
          table_name,
          table_name
        )
      )
    }
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_share_with_trigger
       BEFORE INSERT OR UPDATE OF share_with ON discrete.sample_groups
       FOR EACH ROW EXECUTE FUNCTION public.validate_share_with()"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO audit.table_registry (
         schema_name,
         table_name,
         capture_mode,
         rationale,
         history_started_at,
         updated_at
       ) VALUES
         (
           'discrete',
           'sample_groups',
           'generic_insert_update_delete',
           'Sample-group definitions affect the interpretation of blanks, controls, and associated environmental samples.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'sample_group_members',
           'generic_insert_update_delete',
           'Sample-group membership affects the interpretation of blanks, controls, and associated environmental samples.',
           clock_timestamp(),
           clock_timestamp()
         )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_groups ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY sample_groups_share_with_access
       ON discrete.sample_groups
       FOR ALL
       USING (
         share_with @> ARRAY['public_reader']::TEXT[]
         OR EXISTS (
           SELECT 1
           FROM unnest(share_with) role(role_name)
           WHERE pg_has_role(current_user, role.role_name, 'member')
         )
       )
       WITH CHECK (
         share_with @> ARRAY['public_reader']::TEXT[]
         OR EXISTS (
           SELECT 1
           FROM unnest(share_with) role(role_name)
           WHERE pg_has_role(current_user, role.role_name, 'member')
         )
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_group_members ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY sample_group_members_parent_access
       ON discrete.sample_group_members
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM discrete.sample_groups sg
           WHERE sg.sample_group_id = sample_group_members.sample_group_id
         )
         AND EXISTS (
           SELECT 1
           FROM discrete.samples s
           WHERE s.sample_id = sample_group_members.sample_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM discrete.sample_groups sg
           WHERE sg.sample_group_id = sample_group_members.sample_group_id
         )
         AND EXISTS (
           SELECT 1
           FROM discrete.samples s
           WHERE s.sample_id = sample_group_members.sample_id
         )
       )"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE
         discrete.sample_groups,
         discrete.sample_group_members
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON SEQUENCE
         discrete.sample_group_id_seq,
         discrete.sample_group_member_id_seq
       FROM PUBLIC"
    )

    sample_role_privileges <- DBI::dbGetQuery(
      con,
      "SELECT
         grantee,
         string_agg(
           privilege_type,
           ', ' ORDER BY privilege_type
         ) AS privileges
       FROM information_schema.role_table_grants
       WHERE table_schema = 'discrete'
         AND table_name = 'samples'
         AND privilege_type IN ('SELECT', 'INSERT', 'UPDATE', 'DELETE')
       GROUP BY grantee
       ORDER BY grantee"
    )
    for (i in seq_len(nrow(sample_role_privileges))) {
      privileges <- strsplit(
        sample_role_privileges$privileges[[i]],
        ", ",
        fixed = TRUE
      )[[1]]
      role_name <- as.character(
        DBI::dbQuoteIdentifier(con, sample_role_privileges$grantee[[i]])
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT %s ON TABLE
             discrete.sample_groups,
             discrete.sample_group_members
           TO %s",
          paste(privileges, collapse = ", "),
          role_name
        )
      )
      if ("INSERT" %in% privileges) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
               discrete.sample_group_id_seq,
               discrete.sample_group_member_id_seq
             TO %s",
            role_name
          )
        )
      }
    }

    message("Updating sample metadata views for locationless samples...")
    location_join_pattern <- paste0(
      "(?i)\\bJOIN[[:space:]]+(?:public\\.)?locations[[:space:]]+loc",
      "[[:space:]]+ON[[:space:]]+s\\.location_id[[:space:]]*=",
      "[[:space:]]*loc\\.location_id"
    )
    for (view_name in c("samples_metadata_en", "samples_metadata_fr")) {
      view_definition <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_viewdef($1::regclass, true) AS definition",
        params = list(paste0("discrete.", view_name))
      )$definition[[1]]
      join_matches <- gregexpr(
        location_join_pattern,
        view_definition,
        perl = TRUE
      )[[1]]
      if (identical(join_matches, -1L) || length(join_matches) != 1L) {
        stop(
          "Patch 57 expected exactly one inner location join in discrete.",
          view_name,
          "."
        )
      }

      updated_definition <- sub(
        location_join_pattern,
        "LEFT JOIN public.locations loc ON s.location_id = loc.location_id",
        view_definition,
        perl = TRUE
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE OR REPLACE VIEW discrete.%s
           WITH (security_invoker = true, security_barrier = true)
           AS
           %s",
          view_name,
          updated_definition
        )
      )
    }
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_en IS
       'English-language view that flattens key discrete sample metadata. Location columns are NULL for valid locationless blanks and controls.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_fr IS
       'French-language view that flattens key discrete sample metadata. Location columns are NULL for valid locationless blanks and controls.'"
    )

    message("Verifying sample group schema...")
    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'sample_types'
             AND column_name = 'requires_location'
             AND is_nullable = 'NO'
         ) AS has_requires_location,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'sample_types'
             AND column_name = 'requires_sample_group'
             AND is_nullable = 'NO'
         ) AS has_requires_sample_group,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'samples'
             AND column_name = 'location_id'
             AND is_nullable = 'YES'
         ) AS samples_location_nullable,
         to_regclass('discrete.sample_groups') IS NOT NULL AS has_sample_groups,
         to_regclass('discrete.sample_group_members') IS NOT NULL AS has_sample_group_members,
         to_regprocedure('discrete.enforce_sample_context_requirements()') IS NOT NULL AS has_context_guard,
         to_regprocedure('discrete.enforce_sample_group_membership()') IS NOT NULL AS has_group_guard,
         EXISTS (
           SELECT 1
           FROM pg_indexes
           WHERE schemaname = 'discrete'
             AND tablename = 'samples'
             AND indexname = 'samples_location_context_unique_idx'
             AND indexdef ILIKE '%WHERE (location_id IS NOT NULL)%'
         ) AS has_located_sample_unique_index,
         EXISTS (
           SELECT 1
           FROM pg_policies
           WHERE schemaname = 'discrete'
             AND tablename = 'sample_groups'
             AND policyname = 'sample_groups_share_with_access'
         ) AS has_sample_groups_policy,
         EXISTS (
           SELECT 1
           FROM audit.table_registry
           WHERE schema_name = 'discrete'
             AND table_name = 'sample_groups'
             AND capture_mode = 'generic_insert_update_delete'
         ) AS sample_groups_audited,
         EXISTS (
           SELECT 1
           FROM audit.table_registry
           WHERE schema_name = 'discrete'
             AND table_name = 'sample_group_members'
             AND capture_mode = 'generic_insert_update_delete'
         ) AS sample_group_members_audited,
         pg_get_viewdef('discrete.samples_metadata_en'::regclass, true)
           ILIKE '%LEFT JOIN locations loc ON s.location_id = loc.location_id%'
           AS samples_metadata_en_keeps_locationless,
         pg_get_viewdef('discrete.samples_metadata_fr'::regclass, true)
           ILIKE '%LEFT JOIN locations loc ON s.location_id = loc.location_id%'
           AS samples_metadata_fr_keeps_locationless,
         EXISTS (
           SELECT 1
           FROM public.source_adapter_capabilities sac
           WHERE sac.source_fx = 'downloadNESDIS'
             AND sac.data_domain = 'continuous'
             AND jsonb_path_exists(
               sac.ui_config,
               '$.route_config_fields[*] ? (@.name == \"timestamp_floor_seconds\")'
             )
         ) AS has_nesdis_timestamp_floor_ui_field"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop(
        "Patch 57 verification failed: ",
        paste(
          names(verification)[!unlist(verification[1, ], use.names = FALSE)],
          collapse = ", "
        )
      )
    }

    qc_type_verification <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT sample_type
         FROM discrete.sample_types
         WHERE sample_type IN (%s)
           AND (
             requires_location
             OR NOT requires_sample_group
           )",
        grouped_qc_type_sql
      )
    )
    if (nrow(qc_type_verification)) {
      stop(
        "Patch 57 failed to configure context rules for: ",
        paste(qc_type_verification$sample_type, collapse = ", ")
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '57'
       WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(packageVersion("AquaCache")))
    )

    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 57 applied successfully. Blank and control samples can be locationless, sample groups provide their required many-to-many QC context, and downloadNESDIS route timing fields are documented for clients."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
