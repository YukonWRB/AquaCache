# DEV_patch_blanks.R
#
# Work-in-progress development patch for discrete QC samples that are not tied
# to a single location, such as trip blanks and some field blanks.
#
# This file intentionally lives outside the numbered patch sequence and does
# not update information.version_info. Set DEV_patch_blanks_dry_run <- TRUE
# before sourcing to exercise the patch inside a transaction and roll it back.

if (!exists("con") || !DBI::dbIsValid(con)) {
  stop(
    "Create a valid database connection named `con` before sourcing this patch."
  )
}

if (!exists("dbTransCheck", mode = "function") ||
    !exists("dbTransBegin", mode = "function")) {
  stop(
    "Load AquaCache before sourcing this patch so dbTransCheck() and dbTransBegin() are available."
  )
}

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (!check$session_user %in% c("postgres", "admin")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work."
  )
}

message(
  "Working on DEV_patch_blanks: adding database support for locationless discrete QC blank samples."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

dry_run <- exists("DEV_patch_blanks_dry_run", inherits = TRUE) &&
  isTRUE(get("DEV_patch_blanks_dry_run", inherits = TRUE))

message("Starting transaction...")
active <- FALSE
active <- dbTransBegin(con)

tryCatch(
  {
    DBI::dbExecute(
      con,
      "SET LOCAL search_path = discrete, public, pg_catalog"
    )

    missing_relations <- DBI::dbGetQuery(
      con,
      "WITH required(relation_name) AS (
         VALUES
           ('discrete.samples'),
           ('discrete.results'),
           ('discrete.sample_types'),
           ('discrete.samples_metadata_en'),
           ('discrete.samples_metadata_fr'),
           ('public.locations'),
           ('public.sub_locations'),
           ('public.media_types'),
           ('public.organizations')
       )
       SELECT relation_name
       FROM required
       WHERE to_regclass(relation_name) IS NULL
       ORDER BY relation_name"
    )

    if (nrow(missing_relations) > 0L) {
      stop(
        "This patch requires the following relations to already exist: ",
        paste(missing_relations$relation_name, collapse = ", ")
      )
    }

    missing_functions <- DBI::dbGetQuery(
      con,
      "WITH required(function_name, function_signature) AS (
         VALUES
           ('public.user_modified()', 'public.user_modified()'),
           ('public.update_modified()', 'public.update_modified()'),
           (
             'public.get_shareable_principals_for(regclass, text[], text[])',
             'public.get_shareable_principals_for(regclass, text[], text[])'
           )
       )
       SELECT function_name
       FROM required
       WHERE to_regprocedure(function_signature) IS NULL
       ORDER BY function_name"
    )

    if (nrow(missing_functions) > 0L) {
      stop(
        "This patch requires the following functions to already exist: ",
        paste(missing_functions$function_name, collapse = ", ")
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.current_user_roles()
       RETURNS text[]
       LANGUAGE sql
       STABLE
       SECURITY DEFINER
       SET search_path = pg_temp, pg_catalog
       AS $function$
         SELECT COALESCE(array_agg(r.rolname), ARRAY[]::text[])
         FROM pg_roles r
         WHERE pg_has_role(current_user, r.oid, 'member');
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.current_user_roles()
       OWNER TO postgres;"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION public.current_user_roles() FROM PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION public.current_user_roles() TO PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION public.current_user_roles() IS
       'Returns database role names for which current_user has membership; used by row-level security policies on share_with tables.';"
    )

    q_role <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    grant_table_privileges <- function(table_names, privileges, roles) {
      if (length(roles) == 0L) {
        return(invisible(NULL))
      }

      for (table_name in table_names) {
        for (role_name in roles) {
          DBI::dbExecute(
            con,
            sprintf(
              "GRANT %s ON %s TO %s;",
              privileges,
              table_name,
              q_role(role_name)
            )
          )
        }
      }

      invisible(NULL)
    }

    grant_sequence_privileges <- function(sequence_names, privileges, roles) {
      sequence_names <- sequence_names[!is.na(sequence_names)]

      if (length(sequence_names) == 0L || length(roles) == 0L) {
        return(invisible(NULL))
      }

      for (sequence_name in sequence_names) {
        for (role_name in roles) {
          DBI::dbExecute(
            con,
            sprintf(
              "GRANT %s ON SEQUENCE %s TO %s;",
              privileges,
              sequence_name,
              q_role(role_name)
            )
          )
        }
      }

      invisible(NULL)
    }

    message("Adding sample type location requirements...")
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_types
       ADD COLUMN IF NOT EXISTS requires_location BOOLEAN;"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.sample_types
       SET requires_location = TRUE
       WHERE requires_location IS NULL;"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.sample_types
       SET requires_location = FALSE
       WHERE sample_type IN (
         'QC-sample-field blank',
         'QC-sample-trip blank',
         'QC-sample-lab blank',
         'QC-sample-post-preservative blank',
         'QC-sample-pre-preservative blank',
         'QC-negative control'
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_types
       ALTER COLUMN requires_location SET DEFAULT TRUE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_types
       ALTER COLUMN requires_location SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_types.requires_location IS
       'When true, samples of this type must have discrete.samples.location_id. QC blank/control sample types can set this false so a sample may represent a trip, cooler, shipment, lab batch, or QC group rather than one monitoring location.';"
    )

    message("Creating sample group tables...")
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.sample_groups (
         sample_group_id INTEGER PRIMARY KEY
           GENERATED BY DEFAULT AS IDENTITY,
         group_type TEXT NOT NULL
           CHECK (group_type IN (
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
         group_datetime TIMESTAMP WITH TIME ZONE,
         start_datetime TIMESTAMP WITH TIME ZONE,
         end_datetime TIMESTAMP WITH TIME ZONE,
         location_id INTEGER
           REFERENCES public.locations(location_id)
           ON UPDATE CASCADE
           ON DELETE SET NULL,
         sub_location_id INTEGER
           REFERENCES public.sub_locations(sub_location_id)
           ON UPDATE CASCADE
           ON DELETE SET NULL,
         owner INTEGER
           REFERENCES public.organizations(organization_id)
           ON UPDATE CASCADE
           ON DELETE SET NULL,
         contributor INTEGER
           REFERENCES public.organizations(organization_id)
           ON UPDATE CASCADE
           ON DELETE SET NULL,
         metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         share_with TEXT[] NOT NULL DEFAULT ARRAY['public_reader']::text[],
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified TIMESTAMP WITH TIME ZONE,
         modified_by TEXT,
         CONSTRAINT sample_groups_code_not_blank
           CHECK (group_code IS NULL OR btrim(group_code) <> ''),
         CONSTRAINT sample_groups_name_not_blank
           CHECK (group_name IS NULL OR btrim(group_name) <> ''),
         CONSTRAINT sample_groups_metadata_object
           CHECK (jsonb_typeof(metadata) = 'object'),
         CONSTRAINT sample_groups_period_valid
           CHECK (end_datetime IS NULL OR start_datetime IS NULL OR end_datetime >= start_datetime),
         CONSTRAINT sample_groups_sublocation_requires_location
           CHECK (location_id IS NOT NULL OR sub_location_id IS NULL)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_groups OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_groups IS
       'Groups discrete samples that belong to the same field event, trip, cooler, shipment, lab batch, or QC set. This is the canonical relationship layer for trip blanks, field blanks, replicates, and other QC samples that may apply to more than one environmental sample.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_groups.location_id IS
       'Optional location context for a location-specific field event or QC group. Leave NULL for groups such as trips, coolers, shipments, or lab batches that are not tied to one monitoring location.';"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS sample_groups_type_code_lwr_key
       ON discrete.sample_groups (group_type, lower(btrim(group_code)))
       WHERE group_code IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS sample_groups_group_datetime_idx
       ON discrete.sample_groups (group_datetime, sample_group_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS sample_groups_location_idx
       ON discrete.sample_groups (location_id, group_datetime, sample_group_id)
       WHERE location_id IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS sample_groups_share_with_gin_idx
       ON discrete.sample_groups USING GIN (share_with);"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS discrete.sample_group_members (
         sample_group_member_id INTEGER PRIMARY KEY
           GENERATED BY DEFAULT AS IDENTITY,
         sample_group_id INTEGER NOT NULL
           REFERENCES discrete.sample_groups(sample_group_id)
           ON UPDATE CASCADE
           ON DELETE CASCADE,
         sample_id INTEGER NOT NULL
           REFERENCES discrete.samples(sample_id)
           ON UPDATE CASCADE
           ON DELETE CASCADE,
         member_role TEXT NOT NULL DEFAULT 'related_sample'
           CHECK (member_role IN (
             'routine_sample',
             'primary_sample',
             'related_sample',
             'field_replicate',
             'field_duplicate',
             'trip_blank',
             'field_blank',
             'lab_blank',
             'equipment_blank',
             'method_blank',
             'negative_control',
             'other_qc',
             'other'
           )),
         related_sample_id INTEGER
           REFERENCES discrete.samples(sample_id)
           ON UPDATE CASCADE
           ON DELETE SET NULL,
         sequence_in_group INTEGER
           CHECK (sequence_in_group IS NULL OR sequence_in_group > 0),
         note TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified TIMESTAMP WITH TIME ZONE,
         modified_by TEXT,
         CONSTRAINT sample_group_members_unique
           UNIQUE (sample_group_id, sample_id),
         CONSTRAINT sample_group_members_related_sample_not_self
           CHECK (related_sample_id IS NULL OR related_sample_id <> sample_id)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_group_members OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_group_members IS
       'Many-to-many membership table linking discrete samples to sample_groups with a role such as routine_sample, trip_blank, field_blank, or field_replicate.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_group_members.related_sample_id IS
       'Optional direct relationship to a specific sample inside or outside the group, used when a member such as a replicate or duplicate should identify a primary sample. Trip and field blanks that apply to the whole group should normally leave this NULL.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS sample_group_members_sample_idx
       ON discrete.sample_group_members (sample_id, sample_group_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS sample_group_members_group_role_idx
       ON discrete.sample_group_members (sample_group_id, member_role, sample_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS sample_group_members_related_sample_idx
       ON discrete.sample_group_members (related_sample_id)
       WHERE related_sample_id IS NOT NULL;"
    )

    message("Adding sample location and group membership guards...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.enforce_sample_location_requirement()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY INVOKER
       SET search_path = pg_catalog, discrete, public
       AS $function$
       DECLARE
         type_requires_location BOOLEAN;
         type_label TEXT;
       BEGIN
         SELECT
           COALESCE(st.requires_location, TRUE),
           st.sample_type
         INTO
           type_requires_location,
           type_label
         FROM discrete.sample_types AS st
         WHERE st.sample_type_id = NEW.sample_type;

         IF type_requires_location IS TRUE AND NEW.location_id IS NULL THEN
           RAISE EXCEPTION
             'Sample type % requires location_id on discrete.samples.',
             COALESCE(type_label, NEW.sample_type::text)
             USING ERRCODE = '23514';
         END IF;

         IF NEW.location_id IS NULL AND NEW.sub_location_id IS NOT NULL THEN
           RAISE EXCEPTION
             'discrete.samples.sub_location_id cannot be set when location_id is NULL.'
             USING ERRCODE = '23514';
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_sample_location_requirement()
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION discrete.enforce_sample_location_requirement() IS
       'Trigger guard that keeps location_id mandatory for sample types whose discrete.sample_types.requires_location flag is true.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.enforce_locationless_sample_group_membership()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY INVOKER
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
         FROM discrete.samples AS s
         JOIN discrete.sample_types AS st
           ON st.sample_type_id = s.sample_type
         WHERE s.sample_id = ANY(check_sample_ids)
           AND s.location_id IS NULL
           AND COALESCE(st.requires_location, TRUE) IS FALSE
           AND NOT EXISTS (
             SELECT 1
             FROM discrete.sample_group_members AS sgm
             WHERE sgm.sample_id = s.sample_id
           )
         LIMIT 1;

         IF orphan_sample_id IS NOT NULL THEN
           RAISE EXCEPTION
             'Locationless sample_id % must belong to at least one discrete.sample_group.',
             orphan_sample_id
             USING ERRCODE = '23514';
         END IF;

         RETURN NULL;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_locationless_sample_group_membership()
       OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION discrete.enforce_locationless_sample_group_membership() IS
       'Deferred trigger guard requiring any locationless location-optional sample to be linked through discrete.sample_group_members by commit time.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.enforce_sample_type_location_requirement()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY INVOKER
       SET search_path = pg_catalog, discrete, public
       AS $function$
       BEGIN
         IF NEW.requires_location IS TRUE AND EXISTS (
           SELECT 1
           FROM discrete.samples AS s
           WHERE s.sample_type = NEW.sample_type_id
             AND s.location_id IS NULL
         ) THEN
           RAISE EXCEPTION
             'Cannot require locations for sample type % while locationless samples of that type exist.',
             NEW.sample_type
             USING ERRCODE = '23514';
         END IF;

         RETURN NULL;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_sample_type_location_requirement()
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       ALTER COLUMN location_id DROP NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.location_id IS
       'Monitoring location for environmental samples. May be NULL only for sample types whose discrete.sample_types.requires_location is false; locationless QC samples must be linked through discrete.sample_group_members.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.linked_with IS
       'Legacy one-to-one sample link. Use discrete.sample_groups and discrete.sample_group_members for new QC relationships, especially blanks that apply to more than one sample.';"
    )
    DBI::dbExecute(
      con,
      "DO $do$
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conname = 'samples_sublocation_requires_location'
             AND conrelid = 'discrete.samples'::regclass
         ) THEN
           ALTER TABLE discrete.samples
             ADD CONSTRAINT samples_sublocation_requires_location
             CHECK (location_id IS NOT NULL OR sub_location_id IS NULL);
         END IF;
       END;
       $do$;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS samples_locationless_datetime_sample_id_idx
       ON discrete.samples (datetime, sample_type, sample_id)
       WHERE location_id IS NULL;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS samples_location_requirement
       ON discrete.samples;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER samples_location_requirement
       BEFORE INSERT OR UPDATE OF location_id, sub_location_id, sample_type
       ON discrete.samples
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_sample_location_requirement();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS samples_locationless_group_membership
       ON discrete.samples;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER samples_locationless_group_membership
       AFTER INSERT OR UPDATE OF location_id, sample_type
       ON discrete.samples
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_locationless_sample_group_membership();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS sample_group_members_locationless_group_membership
       ON discrete.sample_group_members;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER sample_group_members_locationless_group_membership
       AFTER DELETE OR UPDATE OF sample_id
       ON discrete.sample_group_members
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_locationless_sample_group_membership();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS sample_types_location_requirement_valid
       ON discrete.sample_types;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER sample_types_location_requirement_valid
       AFTER UPDATE OF requires_location
       ON discrete.sample_types
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_sample_type_location_requirement();"
    )

    for (table_name in c("sample_groups", "sample_group_members")) {
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_user_modified
           ON discrete.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified();",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s_update_modified
           ON discrete.%s;",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON discrete.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified();",
          table_name,
          table_name
        )
      )
    }

    message("Adding RLS policies and grants for sample group tables...")
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_groups ENABLE ROW LEVEL SECURITY;"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS sample_groups_share_with_access
       ON discrete.sample_groups;"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY sample_groups_share_with_access
       ON discrete.sample_groups
       FOR ALL
       USING (
         share_with @> ARRAY['public_reader']::text[]
         OR share_with && public.current_user_roles()
       )
       WITH CHECK (
         share_with @> ARRAY['public_reader']::text[]
         OR share_with && public.current_user_roles()
       );"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_group_members ENABLE ROW LEVEL SECURITY;"
    )
    DBI::dbExecute(
      con,
      "DROP POLICY IF EXISTS sample_group_members_parent_access
       ON discrete.sample_group_members;"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY sample_group_members_parent_access
       ON discrete.sample_group_members
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM discrete.sample_groups AS sg
           WHERE sg.sample_group_id = sample_group_members.sample_group_id
         )
         OR EXISTS (
           SELECT 1
           FROM discrete.samples AS s
           WHERE s.sample_id = sample_group_members.sample_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM discrete.sample_groups AS sg
           WHERE sg.sample_group_id = sample_group_members.sample_group_id
         )
         OR EXISTS (
           SELECT 1
           FROM discrete.samples AS s
           WHERE s.sample_id = sample_group_members.sample_id
         )
       );"
    )

    sample_group_roles <- DBI::dbGetQuery(
      con,
      "SELECT public.get_shareable_principals_for('discrete.samples') AS role_name;"
    )$role_name
    sample_group_roles <- unique(c("admin", sample_group_roles))
    sample_group_roles <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT rolname
         FROM pg_roles
         WHERE rolname IN (%s)
         ORDER BY rolname;",
        paste(
          vapply(
            sample_group_roles,
            function(x) as.character(DBI::dbQuoteString(con, x)),
            character(1)
          ),
          collapse = ", "
        )
      )
    )$rolname

    grant_table_privileges(
      c("discrete.sample_groups", "discrete.sample_group_members"),
      "SELECT, INSERT, UPDATE, DELETE",
      sample_group_roles
    )

    sample_group_sequences <- DBI::dbGetQuery(
      con,
      "SELECT pg_get_serial_sequence('discrete.sample_groups', 'sample_group_id') AS sequence_name
       UNION ALL
       SELECT pg_get_serial_sequence('discrete.sample_group_members', 'sample_group_member_id') AS sequence_name;"
    )$sequence_name

    grant_sequence_privileges(
      sample_group_sequences,
      "USAGE, SELECT",
      sample_group_roles
    )

    message("Updating sample metadata views to retain locationless samples...")
    update_sample_metadata_view <- function(view_name) {
      view_def <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_viewdef($1::regclass, true) AS view_definition;",
        params = list(paste0("discrete.", view_name))
      )$view_definition[[1]]

      updated_view_def <- sub(
        "\n     JOIN locations loc ON s.location_id = loc.location_id",
        "\n     LEFT JOIN locations loc ON s.location_id = loc.location_id",
        view_def,
        fixed = TRUE
      )

      if (identical(updated_view_def, view_def) &&
          !grepl(
            "\n     LEFT JOIN locations loc ON s.location_id = loc.location_id",
            view_def,
            fixed = TRUE
          )) {
        stop(
          "Could not find the expected locations join in discrete.",
          view_name,
          "."
        )
      }

      DBI::dbExecute(
        con,
        sprintf(
          "CREATE OR REPLACE VIEW discrete.%s
           WITH (security_invoker = true, security_barrier = true)
           AS
           %s",
          view_name,
          updated_view_def
        )
      )
    }

    update_sample_metadata_view("samples_metadata_en")
    update_sample_metadata_view("samples_metadata_fr")

    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_en IS
       'English-language view that flattens key discrete sample metadata into a reader-friendly dataset with optional location, media, method, type, quality, organization, and QC grouping context. Locationless rows are valid only for location-optional sample types such as trip blanks.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_fr IS
       'French-language view that flattens key discrete sample metadata into a reader-friendly dataset with optional location, media, method, type, quality, organization, and QC grouping context. Locationless rows are valid only for location-optional sample types such as trip blanks.';"
    )

    message("Verifying blank sample schema changes...")
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
             AND table_name = 'samples'
             AND column_name = 'location_id'
             AND is_nullable = 'YES'
         ) AS samples_location_nullable,
         to_regclass('discrete.sample_groups') IS NOT NULL AS has_sample_groups,
         to_regclass('discrete.sample_group_members') IS NOT NULL AS has_sample_group_members,
         to_regprocedure('discrete.enforce_sample_location_requirement()') IS NOT NULL AS has_location_guard,
         to_regprocedure('discrete.enforce_locationless_sample_group_membership()') IS NOT NULL AS has_group_guard,
         EXISTS (
           SELECT 1
           FROM pg_trigger
           WHERE tgname = 'samples_location_requirement'
             AND tgrelid = 'discrete.samples'::regclass
             AND NOT tgisinternal
         ) AS has_sample_location_trigger,
         EXISTS (
           SELECT 1
           FROM pg_trigger
           WHERE tgname = 'samples_locationless_group_membership'
             AND tgrelid = 'discrete.samples'::regclass
             AND NOT tgisinternal
         ) AS has_sample_group_trigger;"
    )

    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop(
        "Blank sample schema verification failed: ",
        paste(
          names(verification)[!unlist(verification[1, ], use.names = FALSE)],
          collapse = ", "
        )
      )
    }

    if (dry_run) {
      DBI::dbExecute(con, "ROLLBACK;")
      active <- FALSE
      message("DEV_patch_blanks dry run completed successfully and was rolled back.")
    } else {
      DBI::dbExecute(con, "COMMIT;")
      active <- FALSE
      message("DEV_patch_blanks applied successfully.")
    }
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    }
    stop(e)
  }
)
