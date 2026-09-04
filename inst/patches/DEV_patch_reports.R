# DEV_patch_reports.R
#
# Work-in-progress reporting patch intended to follow Patch 60. This file is
# deliberately outside the numbered patch sequence and does not update
# information.version_info. Rename and number it only after Patch 60 and the
# reporting contract are finalized.
#
# The construct lives in the shared application schema but is client-neutral.
# Report recipes are JSONB documents, not serialized R objects, SQL text, or
# executable code. A versioned schema document describes the portable contract
# so YGwater, Python, or another application can validate and render the same
# recipe. Relational tables retain identity, ownership, sharing, immutable
# revision history, execution provenance, and output locations.
#
# Set DEV_patch_reports_dry_run <- TRUE before sourcing to build and verify the
# complete schema inside a transaction and then roll it back.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on the development reporting patch: adding portable, versioned report recipes and execution provenance. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

dry_run <- exists("DEV_patch_reports_dry_run", inherits = TRUE) &&
  isTRUE(get("DEV_patch_reports_dry_run", inherits = TRUE))

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regnamespace('application') IS NOT NULL AS has_application_schema,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info,
         to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
         to_regclass('files.documents') IS NOT NULL AS has_documents,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function,
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_modified_function,
         to_regprocedure('public.user_modified()') IS NOT NULL AS has_user_function,
         to_regprocedure('public.validate_share_with()') IS NOT NULL AS has_share_validation,
         EXISTS (
           SELECT 1 FROM pg_roles WHERE rolname = 'admin'
         ) AS has_admin_role"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "The development reporting patch requires the shared application schema, version table, document store, audit framework, timestamp/user/share triggers, and admin role created by earlier patches."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "60") {
      stop(
        "DEV_patch_reports.R must be applied after Patch 60. It deliberately leaves the recorded patch number at 60."
      )
    }

    reporting_targets_exist <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('application.report_recipe_schemas') IS NOT NULL
           OR to_regclass('application.report_recipes') IS NOT NULL
           OR to_regclass('application.report_recipe_revisions') IS NOT NULL
           OR to_regclass('application.report_runs') IS NOT NULL
           OR to_regclass('application.report_run_outputs') IS NOT NULL
           OR to_regprocedure(
             'application.can_manage_report_role(text)'
           ) IS NOT NULL
           OR to_regprocedure(
             'application.can_view_report(text,text[])'
           ) IS NOT NULL AS available"
    )$available[[1]]
    if (isTRUE(reporting_targets_exist)) {
      stop(
        "One or more application reporting objects already exist. Investigate the existing or partially applied development reporting patch before continuing."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE application.report_recipe_schemas (
         report_recipe_schema_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY,
         schema_code TEXT NOT NULL,
         schema_version INTEGER NOT NULL,
         schema_name TEXT NOT NULL,
         description TEXT,
         schema_document JSONB NOT NULL,
         status TEXT NOT NULL DEFAULT 'draft',
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT report_recipe_schemas_identity_key
           UNIQUE (schema_code, schema_version),
         CONSTRAINT report_recipe_schemas_code_not_blank
           CHECK (schema_code = btrim(schema_code) AND schema_code <> ''),
         CONSTRAINT report_recipe_schemas_version_positive
           CHECK (schema_version > 0),
         CONSTRAINT report_recipe_schemas_name_not_blank
           CHECK (schema_name = btrim(schema_name) AND schema_name <> ''),
         CONSTRAINT report_recipe_schemas_document_object
           CHECK (jsonb_typeof(schema_document) = 'object'),
         CONSTRAINT report_recipe_schemas_status_check
           CHECK (status IN ('draft', 'active', 'deprecated', 'retired'))
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE application.report_recipes (
         report_recipe_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY,
         recipe_name TEXT NOT NULL,
         description TEXT,
         owner_role TEXT NOT NULL DEFAULT CURRENT_USER,
         share_with TEXT[] DEFAULT ARRAY['public_reader']::TEXT[],
         tags TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],
         current_revision INTEGER,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT report_recipes_name_not_blank
           CHECK (recipe_name = btrim(recipe_name) AND recipe_name <> ''),
         CONSTRAINT report_recipes_owner_not_blank
           CHECK (owner_role = btrim(owner_role) AND owner_role <> ''),
         CONSTRAINT report_recipes_current_revision_positive
           CHECK (current_revision IS NULL OR current_revision > 0),
         CONSTRAINT report_recipes_tags_no_nulls
           CHECK (array_position(tags, NULL) IS NULL)
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE application.report_recipe_revisions (
         report_recipe_revision_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY,
         report_recipe_id INTEGER NOT NULL
           REFERENCES application.report_recipes(report_recipe_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         revision INTEGER NOT NULL,
         report_recipe_schema_id INTEGER NOT NULL
           REFERENCES application.report_recipe_schemas(report_recipe_schema_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         recipe JSONB NOT NULL,
         recipe_md5 TEXT GENERATED ALWAYS AS (md5(recipe::TEXT)) STORED,
         change_note TEXT,
         authored_by_client TEXT,
         authored_by_client_version TEXT,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         CONSTRAINT report_recipe_revisions_identity_key
           UNIQUE (report_recipe_id, revision),
         CONSTRAINT report_recipe_revisions_revision_positive
           CHECK (revision > 0),
         CONSTRAINT report_recipe_revisions_recipe_object
           CHECK (jsonb_typeof(recipe) = 'object'),
         CONSTRAINT report_recipe_revisions_client_not_blank
           CHECK (
             authored_by_client IS NULL
             OR btrim(authored_by_client) <> ''
           ),
         CONSTRAINT report_recipe_revisions_client_version_not_blank
           CHECK (
             authored_by_client_version IS NULL
             OR btrim(authored_by_client_version) <> ''
           )
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_recipes
       ADD CONSTRAINT report_recipes_current_revision_fk
       FOREIGN KEY (report_recipe_id, current_revision)
       REFERENCES application.report_recipe_revisions(
         report_recipe_id,
         revision
       )
       ON UPDATE RESTRICT ON DELETE RESTRICT
       DEFERRABLE INITIALLY IMMEDIATE"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE application.report_runs (
         report_run_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         report_recipe_revision_id INTEGER NOT NULL
           REFERENCES application.report_recipe_revisions(
             report_recipe_revision_id
           )
           ON UPDATE CASCADE ON DELETE RESTRICT,
         requested_by_role TEXT NOT NULL DEFAULT CURRENT_USER,
         status TEXT NOT NULL DEFAULT 'queued',
         runtime_arguments JSONB NOT NULL DEFAULT '{}'::JSONB,
         resolved_inputs JSONB NOT NULL DEFAULT '{}'::JSONB,
         executor JSONB NOT NULL DEFAULT '{}'::JSONB,
         database_patch TEXT NOT NULL,
         status_message TEXT,
         requested_at TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
         started_at TIMESTAMPTZ,
         completed_at TIMESTAMPTZ,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT report_runs_status_check CHECK (
           status IN ('queued', 'running', 'succeeded', 'failed', 'cancelled')
         ),
         CONSTRAINT report_runs_runtime_arguments_object
           CHECK (jsonb_typeof(runtime_arguments) = 'object'),
         CONSTRAINT report_runs_resolved_inputs_object
           CHECK (jsonb_typeof(resolved_inputs) = 'object'),
         CONSTRAINT report_runs_executor_object
           CHECK (jsonb_typeof(executor) = 'object'),
         CONSTRAINT report_runs_role_not_blank
           CHECK (
             requested_by_role = btrim(requested_by_role)
             AND requested_by_role <> ''
           ),
         CONSTRAINT report_runs_status_message_not_blank
           CHECK (
             status_message IS NULL OR btrim(status_message) <> ''
           ),
         CONSTRAINT report_runs_lifecycle_check CHECK (
           (
             status = 'queued'
             AND started_at IS NULL
             AND completed_at IS NULL
           )
           OR (
             status = 'running'
             AND started_at IS NOT NULL
             AND completed_at IS NULL
           )
           OR (
             status = 'succeeded'
             AND started_at IS NOT NULL
             AND completed_at IS NOT NULL
             AND completed_at >= started_at
           )
           OR (
             status = 'failed'
             AND completed_at IS NOT NULL
             AND (started_at IS NULL OR completed_at >= started_at)
             AND status_message IS NOT NULL
             AND btrim(status_message) <> ''
           )
           OR (
             status = 'cancelled'
             AND completed_at IS NOT NULL
             AND (started_at IS NULL OR completed_at >= started_at)
           )
         )
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE application.report_run_outputs (
         report_run_output_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY,
         report_run_id INTEGER NOT NULL
           REFERENCES application.report_runs(report_run_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         output_name TEXT NOT NULL,
         output_format TEXT NOT NULL,
         media_type TEXT,
         storage_kind TEXT NOT NULL,
         document_id INTEGER
           REFERENCES files.documents(document_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         external_uri TEXT,
         checksum_algorithm TEXT,
         checksum TEXT,
         size_bytes BIGINT,
         expires_at TIMESTAMPTZ,
         metadata JSONB NOT NULL DEFAULT '{}'::JSONB,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         CONSTRAINT report_run_outputs_name_key
           UNIQUE (report_run_id, output_name),
         CONSTRAINT report_run_outputs_name_not_blank
           CHECK (output_name = btrim(output_name) AND output_name <> ''),
         CONSTRAINT report_run_outputs_format_not_blank
           CHECK (output_format = btrim(output_format) AND output_format <> ''),
         CONSTRAINT report_run_outputs_storage_kind_check
           CHECK (storage_kind IN ('files_document', 'external_uri', 'ephemeral')),
         CONSTRAINT report_run_outputs_locator_check CHECK (
           (
             storage_kind = 'files_document'
             AND document_id IS NOT NULL
             AND external_uri IS NULL
           )
           OR (
             storage_kind = 'external_uri'
             AND document_id IS NULL
             AND external_uri IS NOT NULL
             AND btrim(external_uri) <> ''
           )
           OR (
             storage_kind = 'ephemeral'
             AND document_id IS NULL
             AND external_uri IS NULL
           )
         ),
         CONSTRAINT report_run_outputs_checksum_pair_check CHECK (
           (checksum_algorithm IS NULL AND checksum IS NULL)
           OR (
             checksum_algorithm IS NOT NULL
             AND btrim(checksum_algorithm) <> ''
             AND checksum IS NOT NULL
             AND btrim(checksum) <> ''
           )
         ),
         CONSTRAINT report_run_outputs_size_nonnegative
           CHECK (size_bytes IS NULL OR size_bytes >= 0),
         CONSTRAINT report_run_outputs_metadata_object
           CHECK (jsonb_typeof(metadata) = 'object')
       )"
    )

    for (table_name in c(
      "report_recipe_schemas",
      "report_recipes",
      "report_recipe_revisions",
      "report_runs",
      "report_run_outputs"
    )) {
      DBI::dbExecute(
        con,
        sprintf("ALTER TABLE application.%s OWNER TO admin", table_name)
      )
    }

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.report_recipe_schemas IS
       'Versioned, client-neutral validation contracts for JSONB report recipes. schema_document uses JSON Schema where practical; database triggers enforce the core AquaCache contract without requiring a PostgreSQL extension.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.report_recipe_schemas.schema_document IS
       'A portable schema document consumed by report-authoring and rendering clients. It contains data only and must never be treated as executable code.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.report_recipes IS
       'Stable identity, ownership, sharing, and lifecycle metadata for saved report recipes. The actual recipe is stored in immutable revisions.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.report_recipes.owner_role IS
       'Database login or group role that manages the recipe. public_reader and PostgreSQL system roles cannot own recipes.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.report_recipes.share_with IS
       'Database group roles allowed to read the recipe. Recipes default to public_reader; NULL means private to owner_role. Recipe visibility never grants access to underlying data.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.report_recipe_revisions IS
       'Immutable JSONB snapshots of report intent. Revision numbers are allocated and published by database triggers under a row lock.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.report_recipe_revisions.recipe IS
       'Declarative report configuration. Renderers must validate it against its schema and must not evaluate recipe values as R, Python, shell, template, or SQL code.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.report_recipe_revisions.recipe_md5 IS
       'Deterministic digest of PostgreSQL canonical JSONB text for change detection. It is not a security signature.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.report_runs IS
       'Private execution history for saved recipe revisions. runtime_arguments records caller choices; resolved_inputs records exact IDs and date ranges selected at run time; executor records the producing application and software versions.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.report_runs.database_patch IS
       'AquaCache patch level captured automatically when the run row is inserted.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.report_run_outputs IS
       'Immutable metadata for zero or more artifacts produced by a report run. Content remains in files.documents, external storage, or an explicitly ephemeral client download; report bytes are not duplicated here.'"
    )

    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX report_recipes_owner_name_key
       ON application.report_recipes (owner_role, lower(recipe_name))"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_recipes_share_with_gin_idx
       ON application.report_recipes USING GIN (share_with)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_recipes_tags_gin_idx
       ON application.report_recipes USING GIN (tags)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_recipe_revisions_schema_idx
       ON application.report_recipe_revisions (report_recipe_schema_id)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_recipe_revisions_recipe_gin_idx
       ON application.report_recipe_revisions USING GIN (recipe jsonb_path_ops)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_runs_revision_requested_idx
       ON application.report_runs (
         report_recipe_revision_id,
         requested_at DESC
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_runs_requester_requested_idx
       ON application.report_runs (requested_by_role, requested_at DESC)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_runs_open_status_idx
       ON application.report_runs (status, requested_at)
       WHERE status IN ('queued', 'running')"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX report_run_outputs_document_idx
       ON application.report_run_outputs (document_id)
       WHERE document_id IS NOT NULL"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.can_manage_report_role(p_owner_role TEXT)
       RETURNS BOOLEAN
       LANGUAGE sql
       STABLE
       PARALLEL SAFE
       SET search_path = pg_catalog, application
       AS $function$
         SELECT
           current_user = 'admin'
           OR pg_has_role(current_user, 'admin', 'member')
           OR current_user = p_owner_role
           OR pg_has_role(current_user, p_owner_role, 'member')
       $function$"
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.can_view_report(
         p_owner_role TEXT,
         p_share_with TEXT[]
       )
       RETURNS BOOLEAN
       LANGUAGE sql
       STABLE
       PARALLEL SAFE
       SET search_path = pg_catalog, application
       AS $function$
         SELECT
           application.can_manage_report_role(p_owner_role)
           OR COALESCE(
             p_share_with @> ARRAY['public_reader']::TEXT[],
             FALSE
           )
           OR EXISTS (
             SELECT 1
             FROM unnest(COALESCE(p_share_with, ARRAY[]::TEXT[])) role(role_name)
             WHERE pg_has_role(current_user, role.role_name, 'member')
           )
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.validate_report_recipe_schema()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       BEGIN
         IF TG_OP = 'UPDATE' AND (
           NEW.report_recipe_schema_id IS DISTINCT FROM
             OLD.report_recipe_schema_id
           OR NEW.schema_code IS DISTINCT FROM OLD.schema_code
           OR NEW.schema_version IS DISTINCT FROM OLD.schema_version
           OR NEW.schema_document IS DISTINCT FROM OLD.schema_document
           OR NEW.created_by IS DISTINCT FROM OLD.created_by
           OR NEW.created IS DISTINCT FROM OLD.created
         ) THEN
           RAISE EXCEPTION
             'Published report schema identity and content are immutable; insert a new schema_version instead.'
             USING ERRCODE = '23514';
         END IF;

         IF NOT (NEW.schema_document ? '$schema')
            OR NOT (NEW.schema_document ? 'type')
            OR NEW.schema_document ->> 'type' <> 'object'
         THEN
           RAISE EXCEPTION
             'schema_document must identify its JSON Schema dialect and describe an object.'
             USING ERRCODE = '23514';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.validate_report_recipe()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       BEGIN
         NEW.owner_role := btrim(NEW.owner_role);

         IF TG_OP = 'UPDATE' AND (
           NEW.report_recipe_id IS DISTINCT FROM OLD.report_recipe_id
           OR NEW.created_by IS DISTINCT FROM OLD.created_by
           OR NEW.created IS DISTINCT FROM OLD.created
         ) THEN
           RAISE EXCEPTION
             'Report recipe identity and creation provenance are immutable.'
             USING ERRCODE = '23514';
         END IF;

         IF NEW.owner_role IN ('public', 'public_reader')
            OR NEW.owner_role ~ '^pg_'
            OR NOT EXISTS (
              SELECT 1 FROM pg_roles WHERE rolname = NEW.owner_role
            )
         THEN
           RAISE EXCEPTION
             'owner_role % is not an eligible database login or group role.',
             NEW.owner_role
             USING ERRCODE = '23514';
         END IF;

         IF TG_OP = 'INSERT' AND NEW.current_revision IS NOT NULL THEN
           RAISE EXCEPTION
             'A new report recipe must be inserted without current_revision; insert its first revision next.'
             USING ERRCODE = '23514';
         END IF;

         IF TG_OP = 'UPDATE'
            AND NEW.current_revision IS DISTINCT FROM OLD.current_revision
            AND (
              NEW.current_revision IS NULL
              OR NEW.current_revision <> COALESCE(OLD.current_revision, 0) + 1
              OR NOT EXISTS (
                SELECT 1
                FROM application.report_recipe_revisions revision
                WHERE revision.report_recipe_id = NEW.report_recipe_id
                  AND revision.revision = NEW.current_revision
              )
            )
         THEN
           RAISE EXCEPTION
             'current_revision can advance only to the next existing immutable revision.'
             USING ERRCODE = '23514';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.validate_report_recipe_revision()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       DECLARE
         current_number INTEGER;
         expected_number INTEGER;
         selected_schema_code TEXT;
         selected_schema_version INTEGER;
         invalid_dataset JSONB;
         invalid_section JSONB;
       BEGIN
         SELECT recipe.current_revision
         INTO current_number
         FROM application.report_recipes recipe
         WHERE recipe.report_recipe_id = NEW.report_recipe_id
           AND application.can_manage_report_role(recipe.owner_role)
         FOR UPDATE;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Report recipe % does not exist or is not manageable by the current role.',
             NEW.report_recipe_id
             USING ERRCODE = '42501';
         END IF;

         expected_number := COALESCE(current_number, 0) + 1;
         IF NEW.revision IS NULL THEN
           NEW.revision := expected_number;
         ELSIF NEW.revision <> expected_number THEN
           RAISE EXCEPTION
             'Expected revision % for report recipe %, received %.',
             expected_number,
             NEW.report_recipe_id,
             NEW.revision
             USING ERRCODE = '23514';
         END IF;

         SELECT schema_definition.schema_code,
                schema_definition.schema_version
         INTO selected_schema_code, selected_schema_version
         FROM application.report_recipe_schemas schema_definition
         WHERE schema_definition.report_recipe_schema_id =
           NEW.report_recipe_schema_id
           AND schema_definition.status = 'active';

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Report recipe schema % is not available for new revisions.',
             NEW.report_recipe_schema_id
             USING ERRCODE = '23514';
         END IF;

         IF selected_schema_code = 'aquacache-report'
            AND selected_schema_version = 1
         THEN
           IF NOT (NEW.recipe ? 'datasets')
              OR jsonb_typeof(NEW.recipe -> 'datasets') <> 'array'
           THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires a non-empty datasets array.'
               USING ERRCODE = '23514';
           END IF;
           IF jsonb_array_length(NEW.recipe -> 'datasets') = 0 THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires a non-empty datasets array.'
               USING ERRCODE = '23514';
           END IF;

           IF NOT (NEW.recipe ? 'sections')
              OR jsonb_typeof(NEW.recipe -> 'sections') <> 'array'
           THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires a non-empty sections array.'
               USING ERRCODE = '23514';
           END IF;
           IF jsonb_array_length(NEW.recipe -> 'sections') = 0 THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires a non-empty sections array.'
               USING ERRCODE = '23514';
           END IF;

           IF NOT (NEW.recipe ? 'output')
              OR jsonb_typeof(NEW.recipe -> 'output') <> 'object'
           THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires output.formats as a non-empty array of strings.'
               USING ERRCODE = '23514';
           END IF;
           IF NOT ((NEW.recipe -> 'output') ? 'formats')
              OR jsonb_typeof(NEW.recipe #> '{output,formats}') <> 'array'
           THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires output.formats as a non-empty array of strings.'
               USING ERRCODE = '23514';
           END IF;
           IF jsonb_array_length(NEW.recipe #> '{output,formats}') = 0
              OR EXISTS (
                SELECT 1
                FROM jsonb_array_elements(
                  NEW.recipe #> '{output,formats}'
                ) AS formats(format_value)
                WHERE jsonb_typeof(format_value) <> 'string'
                   OR btrim(format_value #>> '{}') = ''
              )
           THEN
             RAISE EXCEPTION
               'aquacache-report version 1 requires output.formats as a non-empty array of strings.'
               USING ERRCODE = '23514';
           END IF;

           SELECT dataset
           INTO invalid_dataset
           FROM jsonb_array_elements(
             NEW.recipe -> 'datasets'
           ) AS datasets(dataset)
           WHERE jsonb_typeof(dataset) <> 'object'
              OR NOT (dataset ? 'id')
              OR jsonb_typeof(dataset -> 'id') <> 'string'
              OR btrim(dataset ->> 'id') = ''
              OR NOT (dataset ? 'source')
              OR dataset ->> 'source' NOT IN ('discrete', 'continuous')
              OR NOT (dataset ? 'selection')
              OR jsonb_typeof(dataset -> 'selection') <> 'object'
           LIMIT 1;
           IF invalid_dataset IS NOT NULL THEN
             RAISE EXCEPTION
               'Each dataset requires a non-empty string id, source discrete or continuous, and an object selection.'
               USING ERRCODE = '23514';
           END IF;

           IF EXISTS (
             SELECT 1
             FROM jsonb_array_elements(
               NEW.recipe -> 'datasets'
             ) AS datasets(dataset)
             GROUP BY dataset ->> 'id'
             HAVING count(*) > 1
           ) THEN
             RAISE EXCEPTION
               'Dataset ids must be unique within a report recipe.'
               USING ERRCODE = '23514';
           END IF;

           SELECT section
           INTO invalid_section
           FROM jsonb_array_elements(
             NEW.recipe -> 'sections'
           ) AS sections(section)
           WHERE jsonb_typeof(section) <> 'object'
              OR NOT (section ? 'type')
              OR jsonb_typeof(section -> 'type') <> 'string'
              OR btrim(section ->> 'type') = ''
              OR (
                section ? 'dataset'
                AND (
                  jsonb_typeof(section -> 'dataset') <> 'string'
                  OR NOT EXISTS (
                    SELECT 1
                    FROM jsonb_array_elements(
                      NEW.recipe -> 'datasets'
                    ) AS datasets(dataset)
                    WHERE dataset ->> 'id' = section ->> 'dataset'
                  )
                )
              )
           LIMIT 1;
           IF invalid_section IS NOT NULL THEN
             RAISE EXCEPTION
               'Each section requires a non-empty type; an optional dataset must reference a declared dataset id.'
               USING ERRCODE = '23514';
           END IF;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.publish_report_recipe_revision()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       BEGIN
         UPDATE application.report_recipes
         SET current_revision = NEW.revision
         WHERE report_recipe_id = NEW.report_recipe_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Unable to publish revision % for report recipe %.',
             NEW.revision,
             NEW.report_recipe_id;
         END IF;

         RETURN NULL;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.prevent_report_history_change()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       BEGIN
         RAISE EXCEPTION
           'Rows in %.% are immutable; preserve history by inserting a new row.',
           TG_TABLE_SCHEMA,
           TG_TABLE_NAME
           USING ERRCODE = '55000';
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.set_report_created_by()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       BEGIN
         NEW.created_by := CURRENT_USER;
         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.validate_report_run()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application, information
       AS $function$
       DECLARE
         old_status TEXT;
       BEGIN
         NEW.requested_by_role := btrim(NEW.requested_by_role);

         IF NOT EXISTS (
           SELECT 1 FROM pg_roles WHERE rolname = NEW.requested_by_role
         ) THEN
           RAISE EXCEPTION
             'requested_by_role % does not exist.',
             NEW.requested_by_role
             USING ERRCODE = '23514';
         END IF;

         IF TG_OP = 'INSERT' THEN
           SELECT version
           INTO NEW.database_patch
           FROM information.version_info
           WHERE item = 'Last patch number';
         ELSE
           IF NEW.report_recipe_revision_id IS DISTINCT FROM
                OLD.report_recipe_revision_id
              OR NEW.requested_by_role IS DISTINCT FROM OLD.requested_by_role
              OR NEW.runtime_arguments IS DISTINCT FROM OLD.runtime_arguments
              OR NEW.database_patch IS DISTINCT FROM OLD.database_patch
              OR NEW.requested_at IS DISTINCT FROM OLD.requested_at
              OR NEW.created_by IS DISTINCT FROM OLD.created_by
              OR NEW.created IS DISTINCT FROM OLD.created
           THEN
             RAISE EXCEPTION
               'A report run revision, requester, runtime arguments, database patch, and request time are immutable.'
               USING ERRCODE = '23514';
           END IF;

           old_status := OLD.status;
           IF old_status IN ('succeeded', 'failed', 'cancelled') THEN
             RAISE EXCEPTION
               'Completed report runs are immutable.'
               USING ERRCODE = '55000';
           END IF;

           IF NEW.status <> old_status AND NOT (
             (old_status = 'queued' AND NEW.status IN (
               'running', 'failed', 'cancelled'
             ))
             OR (old_status = 'running' AND NEW.status IN (
               'succeeded', 'failed', 'cancelled'
             ))
           ) THEN
             RAISE EXCEPTION
               'Invalid report run status transition from % to %.',
               old_status,
               NEW.status
               USING ERRCODE = '23514';
           END IF;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE FUNCTION application.validate_report_run_output()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SET search_path = pg_catalog, application
       AS $function$
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM application.report_runs run
           WHERE run.report_run_id = NEW.report_run_id
             AND run.status IN ('running', 'succeeded')
             AND application.can_manage_report_role(run.requested_by_role)
         ) THEN
           RAISE EXCEPTION
             'Report run % is not manageable or is not running/succeeded.',
             NEW.report_run_id
             USING ERRCODE = '23514';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_report_recipe_schema_trigger
       BEFORE INSERT OR UPDATE ON application.report_recipe_schemas
       FOR EACH ROW
       EXECUTE FUNCTION application.validate_report_recipe_schema()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER prevent_report_recipe_schema_delete_trigger
       BEFORE DELETE ON application.report_recipe_schemas
       FOR EACH ROW
       EXECUTE FUNCTION application.prevent_report_history_change()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_report_recipe_trigger
       BEFORE INSERT OR UPDATE ON application.report_recipes
       FOR EACH ROW
       EXECUTE FUNCTION application.validate_report_recipe()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_share_with_trigger
       BEFORE INSERT OR UPDATE OF share_with ON application.report_recipes
       FOR EACH ROW
       EXECUTE FUNCTION public.validate_share_with()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_report_recipe_revision_trigger
       BEFORE INSERT ON application.report_recipe_revisions
       FOR EACH ROW
       EXECUTE FUNCTION application.validate_report_recipe_revision()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER publish_report_recipe_revision_trigger
       AFTER INSERT ON application.report_recipe_revisions
       FOR EACH ROW
       EXECUTE FUNCTION application.publish_report_recipe_revision()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER prevent_report_recipe_revision_change_trigger
       BEFORE UPDATE OR DELETE ON application.report_recipe_revisions
       FOR EACH ROW
       EXECUTE FUNCTION application.prevent_report_history_change()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_report_run_trigger
       BEFORE INSERT OR UPDATE ON application.report_runs
       FOR EACH ROW
       EXECUTE FUNCTION application.validate_report_run()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER prevent_report_run_output_change_trigger
       BEFORE UPDATE OR DELETE ON application.report_run_outputs
       FOR EACH ROW
       EXECUTE FUNCTION application.prevent_report_history_change()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER validate_report_run_output_trigger
       BEFORE INSERT ON application.report_run_outputs
       FOR EACH ROW
       EXECUTE FUNCTION application.validate_report_run_output()"
    )

    for (table_name in c(
      "report_recipe_schemas",
      "report_recipes",
      "report_recipe_revisions",
      "report_runs",
      "report_run_outputs"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER set_%s_created_by
           BEFORE INSERT ON application.%s
           FOR EACH ROW EXECUTE FUNCTION application.set_report_created_by()",
          table_name,
          table_name
        )
      )
    }

    for (table_name in c(
      "report_recipe_schemas",
      "report_recipes",
      "report_runs"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER update_%s_modified
           BEFORE UPDATE ON application.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER update_%s_modified_by
           BEFORE UPDATE ON application.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_name,
          table_name
        )
      )
    }

    for (table_name in c(
      "report_recipe_schemas",
      "report_recipes",
      "report_recipe_revisions",
      "report_runs",
      "report_run_outputs"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER audit_%s_trigger
           AFTER INSERT OR UPDATE OR DELETE ON application.%s
           FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()",
          table_name,
          table_name
        )
      )
    }

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
           'application',
           'report_recipe_schemas',
           'generic_insert_update_delete',
           'Recipe schema status and versioned validation contracts determine how saved recipes are interpreted.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'application',
           'report_recipes',
           'generic_insert_update_delete',
           'Recipe ownership, sharing, publication, and lifecycle metadata control report access and interpretation.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'application',
           'report_recipe_revisions',
           'generic_insert_update_delete',
           'Immutable recipe revisions define reproducible report intent.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'application',
           'report_runs',
           'generic_insert_update_delete',
           'Report run state, resolved inputs, and executor metadata provide operational and reproducibility provenance.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'application',
           'report_run_outputs',
           'generic_insert_update_delete',
           'Report output metadata links execution history to durable or ephemeral artifacts.',
           clock_timestamp(),
           clock_timestamp()
         )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_recipes ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_recipes FORCE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_recipes_select
       ON application.report_recipes
       FOR SELECT
       USING (
         application.can_view_report(owner_role, share_with)
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_recipes_insert
       ON application.report_recipes
       FOR INSERT
       WITH CHECK (
         application.can_manage_report_role(owner_role)
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_recipes_update
       ON application.report_recipes
       FOR UPDATE
       USING (
         application.can_manage_report_role(owner_role)
       )
       WITH CHECK (
         application.can_manage_report_role(owner_role)
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_recipe_revisions
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_recipe_revisions
       FORCE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_recipe_revisions_select
       ON application.report_recipe_revisions
       FOR SELECT
       USING (
         EXISTS (
           SELECT 1
           FROM application.report_recipes recipe
           WHERE recipe.report_recipe_id =
             report_recipe_revisions.report_recipe_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_recipe_revisions_insert
       ON application.report_recipe_revisions
       FOR INSERT
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM application.report_recipes recipe
           WHERE recipe.report_recipe_id =
             report_recipe_revisions.report_recipe_id
             AND application.can_manage_report_role(recipe.owner_role)
         )
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_runs ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_runs FORCE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_runs_select
       ON application.report_runs
       FOR SELECT
       USING (
         application.can_manage_report_role(requested_by_role)
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_runs_insert
       ON application.report_runs
       FOR INSERT
       WITH CHECK (
         application.can_manage_report_role(requested_by_role)
         AND EXISTS (
           SELECT 1
           FROM application.report_recipe_revisions revision
           WHERE revision.report_recipe_revision_id =
             report_runs.report_recipe_revision_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_runs_update
       ON application.report_runs
       FOR UPDATE
       USING (
         application.can_manage_report_role(requested_by_role)
       )
       WITH CHECK (
         application.can_manage_report_role(requested_by_role)
       )"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_run_outputs ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE application.report_run_outputs FORCE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_run_outputs_select
       ON application.report_run_outputs
       FOR SELECT
       USING (
         EXISTS (
           SELECT 1
           FROM application.report_runs run
           WHERE run.report_run_id = report_run_outputs.report_run_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY report_run_outputs_insert
       ON application.report_run_outputs
       FOR INSERT
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM application.report_runs run
           WHERE run.report_run_id = report_run_outputs.report_run_id
             AND application.can_manage_report_role(run.requested_by_role)
         )
       )"
    )

    function_signatures <- c(
      "can_manage_report_role(text)",
      "can_view_report(text,text[])",
      "validate_report_recipe_schema()",
      "validate_report_recipe()",
      "validate_report_recipe_revision()",
      "publish_report_recipe_revision()",
      "prevent_report_history_change()",
      "set_report_created_by()",
      "validate_report_run()",
      "validate_report_run_output()"
    )
    for (function_signature in function_signatures) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER FUNCTION application.%s OWNER TO admin",
          function_signature
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "REVOKE ALL ON FUNCTION application.%s FROM PUBLIC",
          function_signature
        )
      )
    }
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION
         application.can_manage_report_role(TEXT),
         application.can_view_report(TEXT, TEXT[])
       TO PUBLIC"
    )

    DBI::dbExecute(
      con,
      "GRANT USAGE ON SCHEMA application TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE
         application.report_recipe_schemas,
         application.report_recipes,
         application.report_recipe_revisions,
         application.report_runs,
         application.report_run_outputs
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON SEQUENCE
         application.report_recipe_schemas_report_recipe_schema_id_seq,
         application.report_recipes_report_recipe_id_seq,
         application.report_recipe_revisions_report_recipe_revision_id_seq,
         application.report_runs_report_run_id_seq,
         application.report_run_outputs_report_run_output_id_seq
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON application.report_recipe_schemas TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT, INSERT, UPDATE
       ON application.report_recipes TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT, INSERT
       ON application.report_recipe_revisions TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT, INSERT, UPDATE
       ON application.report_runs TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT, INSERT
       ON application.report_run_outputs TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "GRANT USAGE, SELECT ON SEQUENCE
         application.report_recipes_report_recipe_id_seq,
         application.report_recipe_revisions_report_recipe_revision_id_seq,
         application.report_runs_report_run_id_seq,
         application.report_run_outputs_report_run_output_id_seq
       TO PUBLIC"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO application.report_recipe_schemas (
         schema_code,
         schema_version,
         schema_name,
         description,
         schema_document,
         status
       ) VALUES (
         'aquacache-report',
         1,
         'AquaCache report recipe',
         'Portable core contract for reports combining discrete and continuous AquaCache data.',
         '{
           \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",
           \"$id\": \"urn:aquacache:report-recipe:1\",
           \"title\": \"AquaCache report recipe\",
           \"type\": \"object\",
           \"required\": [\"datasets\", \"sections\", \"output\"],
           \"properties\": {
             \"metadata\": {\"type\": \"object\"},
             \"inputs\": {\"type\": \"object\"},
             \"datasets\": {
               \"type\": \"array\",
               \"minItems\": 1,
               \"items\": {
                 \"type\": \"object\",
                 \"required\": [\"id\", \"source\", \"selection\"],
                 \"properties\": {
                   \"id\": {\"type\": \"string\", \"minLength\": 1},
                   \"source\": {
                     \"type\": \"string\",
                     \"enum\": [\"discrete\", \"continuous\"]
                   },
                   \"selection\": {\"type\": \"object\"},
                   \"filters\": {\"type\": \"object\"},
                   \"transformations\": {\"type\": \"array\"}
                 },
                 \"additionalProperties\": true
               }
             },
             \"sections\": {
               \"type\": \"array\",
               \"minItems\": 1,
               \"items\": {
                 \"type\": \"object\",
                 \"required\": [\"type\"],
                 \"properties\": {
                   \"type\": {\"type\": \"string\", \"minLength\": 1},
                   \"dataset\": {\"type\": \"string\", \"minLength\": 1},
                   \"options\": {\"type\": \"object\"}
                 },
                 \"additionalProperties\": true
               }
             },
             \"output\": {
               \"type\": \"object\",
               \"required\": [\"formats\"],
               \"properties\": {
                 \"formats\": {
                   \"type\": \"array\",
                   \"minItems\": 1,
                   \"items\": {\"type\": \"string\", \"minLength\": 1}
                 },
                 \"template\": {\"type\": \"object\"},
                 \"renderer\": {\"type\": \"object\"}
               },
               \"additionalProperties\": true
             }
           },
           \"additionalProperties\": true
         }'::JSONB,
         'active'
       )"
    )

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         (
           SELECT count(*)
           FROM information_schema.tables
           WHERE table_schema = 'application'
             AND table_name IN (
               'report_recipe_schemas',
               'report_recipes',
               'report_recipe_revisions',
               'report_runs',
               'report_run_outputs'
             )
         ) = 5 AS all_reporting_tables_available,
         (
           SELECT count(*)
           FROM information_schema.columns
           WHERE table_schema = 'application'
             AND data_type = 'jsonb'
             AND (table_name, column_name) IN (
               ('report_recipe_schemas', 'schema_document'),
               ('report_recipe_revisions', 'recipe'),
               ('report_runs', 'runtime_arguments'),
               ('report_runs', 'resolved_inputs'),
               ('report_runs', 'executor'),
               ('report_run_outputs', 'metadata')
             )
         ) = 6 AS all_structured_documents_use_jsonb,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'application'
             AND table_name = 'report_recipes'
             AND column_name = 'share_with'
             AND column_default LIKE '%public_reader%'
         ) AS recipes_are_public_by_default,
         (
           SELECT count(*)
           FROM information_schema.role_table_grants
           WHERE grantee = 'PUBLIC'
             AND table_schema = 'application'
             AND table_name IN (
               'report_recipe_schemas',
               'report_recipes',
               'report_recipe_revisions',
               'report_runs',
               'report_run_outputs'
             )
             AND privilege_type = 'SELECT'
         ) = 5 AS all_reporting_tables_selectable_by_public,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'application'
             AND table_name IN (
               'report_recipe_schemas',
               'report_recipes',
               'report_recipe_revisions',
               'report_runs',
               'report_run_outputs'
             )
             AND data_type = 'bytea'
         ) AS reporting_tables_have_no_blobs,
         (
           SELECT count(*)
           FROM application.report_recipe_schemas
           WHERE schema_code = 'aquacache-report'
             AND schema_version = 1
             AND status = 'active'
             AND schema_document ->> 'type' = 'object'
         ) = 1 AS portable_recipe_schema_available,
         (
           SELECT count(*)
           FROM pg_class relation
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'application'
             AND relation.relname IN (
               'report_recipes',
               'report_recipe_revisions',
               'report_runs',
               'report_run_outputs'
             )
             AND relation.relrowsecurity
             AND relation.relforcerowsecurity
         ) = 4 AS all_user_tables_force_rls,
         (
           SELECT count(*)
           FROM pg_policies
           WHERE schemaname = 'application'
             AND policyname IN (
               'report_recipes_select',
               'report_recipes_insert',
               'report_recipes_update',
               'report_recipe_revisions_select',
               'report_recipe_revisions_insert',
               'report_runs_select',
               'report_runs_insert',
               'report_runs_update',
               'report_run_outputs_select',
               'report_run_outputs_insert'
             )
         ) = 10 AS all_reporting_policies_available,
         (
           SELECT count(*)
           FROM audit.table_registry
           WHERE schema_name = 'application'
             AND table_name IN (
               'report_recipe_schemas',
               'report_recipes',
               'report_recipe_revisions',
               'report_runs',
               'report_run_outputs'
             )
             AND capture_mode = 'generic_insert_update_delete'
         ) = 5 AS all_reporting_tables_registered_for_audit,
         (
           SELECT count(*)
           FROM pg_trigger trigger_definition
           JOIN pg_class relation
             ON relation.oid = trigger_definition.tgrelid
           JOIN pg_namespace namespace
             ON namespace.oid = relation.relnamespace
           WHERE namespace.nspname = 'application'
             AND relation.relname IN (
               'report_recipe_schemas',
               'report_recipes',
               'report_recipe_revisions',
               'report_runs',
               'report_run_outputs'
             )
             AND trigger_definition.tgname LIKE 'audit_%_trigger'
             AND NOT trigger_definition.tgisinternal
         ) = 5 AS all_reporting_audit_triggers_available,
         to_regprocedure(
           'application.can_manage_report_role(text)'
         ) IS NOT NULL AS has_manage_role_function,
         to_regprocedure(
           'application.can_view_report(text,text[])'
         ) IS NOT NULL AS has_view_function,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conname = 'report_recipes_current_revision_fk'
             AND conrelid = 'application.report_recipes'::REGCLASS
             AND condeferrable
         ) AS current_revision_fk_is_deferrable,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.role_table_grants
           WHERE grantee = 'PUBLIC'
             AND table_schema = 'application'
             AND table_name = 'report_recipe_revisions'
             AND privilege_type = 'UPDATE'
         ) AS revisions_not_publicly_updateable,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.role_table_grants
           WHERE grantee = 'PUBLIC'
             AND table_schema = 'application'
             AND table_name = 'report_run_outputs'
             AND privilege_type = 'UPDATE'
         ) AS outputs_not_publicly_updateable,
         (
           SELECT version
           FROM information.version_info
           WHERE item = 'Last patch number'
         ) = '60' AS official_patch_number_unchanged"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      failed_verification <- names(verification)[
        !vapply(verification[1, ], isTRUE, logical(1))
      ]
      stop(
        "Development application-reporting verification failed: ",
        paste(failed_verification, collapse = ", "),
        "."
      )
    }

    if (dry_run) {
      DBI::dbExecute(con, "ROLLBACK")
      active <- FALSE
      message(
        "DEV_patch_reports.R dry run completed successfully and was rolled back. The recorded database patch remains 60."
      )
    } else {
      DBI::dbExecute(con, "COMMIT")
      active <- FALSE
      message(
        "DEV_patch_reports.R applied successfully. Portable report recipes, immutable revisions, private run provenance, and output references are ready. The recorded database patch remains 60 because this development patch is not yet numbered."
      )
    }
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
