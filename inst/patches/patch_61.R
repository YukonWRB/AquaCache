# Patch 61: versioned discrete import mappings and provenance

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

# Tests may set patch61_commit <- FALSE before sourcing this file. In that
# mode the complete patch remains visible in the caller's open transaction so
# both interactive and automated workflows can be exercised before rollback.
patch61_commit <- if (exists("patch61_commit", inherits = FALSE)) {
  isTRUE(get("patch61_commit", inherits = FALSE))
} else {
  TRUE
}

message(
  "Working on patch 61: versioning discrete import mappings and provenance."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    prerequisites <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.import_sources') IS NOT NULL AS has_sources,
         to_regclass('discrete.import_profiles') IS NOT NULL AS has_profiles,
         to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS has_parameters,
         to_regclass('discrete.import_qualifier_mappings') IS NOT NULL AS has_legacy_result_flags,
         to_regclass('discrete.sample_group_types') IS NOT NULL AS has_sample_group_types,
         to_regclass('discrete.sample_groups') IS NOT NULL AS has_sample_groups,
         to_regclass('discrete.sample_group_members') IS NOT NULL AS has_sample_group_members,
         to_regclass('discrete.samples_metadata_en') IS NOT NULL AS has_samples_metadata_en,
         to_regclass('discrete.samples_metadata_fr') IS NOT NULL AS has_samples_metadata_fr,
         to_regclass('discrete.results_metadata_en') IS NOT NULL AS has_results_metadata_en,
         to_regclass('discrete.results_metadata_fr') IS NOT NULL AS has_results_metadata_fr,
         to_regclass('public.locations') IS NOT NULL AS has_locations,
         to_regclass('public.sub_locations') IS NOT NULL AS has_sub_locations,
         to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
         to_regprocedure('public.user_modified()') IS NOT NULL AS has_user_modified,
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_update_modified,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function,
         (
           SELECT version = '60'
           FROM information.version_info
           WHERE item = 'Last patch number'
         ) AS is_patch_60"
    )
    if (!all(unlist(prerequisites[1, ], use.names = FALSE))) {
      failed <- names(prerequisites)[
        !vapply(prerequisites[1, ], isTRUE, logical(1))
      ]
      stop(
        "Patch 61 requires a complete Patch 60 database. Missing prerequisites: ",
        paste(failed, collapse = ", "),
        "."
      )
    }

    quote_grantee <- function(x) {
      if (identical(x, "PUBLIC")) {
        return("PUBLIC")
      }
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    # linked_with is still exposed by the sample metadata views, and the result
    # metadata views depend on those sample views. Preserve all four definitions
    # and their access contracts before replacing the legacy relationship.
    metadata_view_names <- c(
      "samples_metadata_en",
      "samples_metadata_fr",
      "results_metadata_en",
      "results_metadata_fr"
    )
    metadata_view_definitions <- stats::setNames(
      vapply(
        metadata_view_names,
        function(view_name) {
          DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT pg_get_viewdef('discrete.%s'::regclass, true) AS definition",
              view_name
            )
          )$definition[[1]]
        },
        character(1)
      ),
      metadata_view_names
    )
    metadata_view_properties <- DBI::dbGetQuery(
      con,
      "SELECT
         relation.relname AS view_name,
         pg_get_userbyid(relation.relowner) AS owner_name,
         obj_description(relation.oid, 'pg_class') AS view_comment
       FROM pg_class relation
       JOIN pg_namespace namespace ON namespace.oid = relation.relnamespace
       WHERE namespace.nspname = 'discrete'
         AND relation.relname IN (
           'samples_metadata_en',
           'samples_metadata_fr',
           'results_metadata_en',
           'results_metadata_fr'
         )"
    )
    metadata_view_privileges <- DBI::dbGetQuery(
      con,
      "SELECT table_name, grantee, privilege_type
       FROM information_schema.role_table_grants
       WHERE table_schema = 'discrete'
         AND table_name IN (
           'samples_metadata_en',
           'samples_metadata_fr',
           'results_metadata_en',
           'results_metadata_fr'
         )
       ORDER BY table_name, grantee, privilege_type"
    )

    # PostgreSQL preserves view output names when base columns are renamed.
    # Transform both the base references and public output names explicitly,
    # while removing linked_sample_id from the two sample views.
    metadata_view_definitions <- gsub(
      "sample_import_source_id",
      "sample_external_sample_id",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_definitions <- gsub(
      "sample_import_source",
      "sample_source_adapter_function",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_definitions <- gsub(
      "sm.import_source_id",
      "sm.external_sample_id",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_definitions <- gsub(
      "sm.import_source",
      "sm.source_adapter_function",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_definitions <- gsub(
      "s.import_source_id",
      "s.external_sample_id",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_definitions <- gsub(
      "s.import_source",
      "s.source_adapter_function",
      metadata_view_definitions,
      fixed = TRUE
    )
    metadata_view_definitions <- sub(
      "(?m)^[[:space:]]*s\\.linked_with AS linked_sample_id,[[:space:]]*\\r?\\n",
      "",
      metadata_view_definitions,
      perl = TRUE
    )
    transformed_views_valid <- vapply(
      metadata_view_definitions,
      function(view_definition) {
        !grepl("linked_with", view_definition, fixed = TRUE) &&
          !grepl("linked_sample_id", view_definition, fixed = TRUE) &&
          !grepl("s.import_source", view_definition, fixed = TRUE) &&
          !grepl("sm.import_source", view_definition, fixed = TRUE) &&
          !grepl("sample_import_source", view_definition, fixed = TRUE)
      },
      logical(1)
    )
    if (!all(transformed_views_valid)) {
      stop(
        "Patch 61 could not safely remove linked_with or rename source identity fields in metadata views: ",
        paste(
          names(transformed_views_valid)[!transformed_views_valid],
          collapse = ", "
        ),
        "."
      )
    }

    # Patch 49 called these source-system codes "qualifiers", but AquaCache
    # reserves qualifier for normalized quality metadata in qualifier_types,
    # continuous.qualifiers, and discrete.sample_qualifiers. Rename the import
    # construct before reshaping it into versioned mapping sets.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_qualifier_mappings
       RENAME TO import_result_flag_mappings"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME COLUMN import_qualifier_mapping_id
         TO import_result_flag_mapping_id"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME COLUMN qualifier_column TO source_flag_column"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME COLUMN qualifier_value TO source_flag_value"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME COLUMN result_condition TO result_condition_id"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME CONSTRAINT import_qualifier_mappings_pkey
         TO import_result_flag_mappings_pkey"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME CONSTRAINT import_qualifier_mappings_result_condition_fkey
         TO import_result_flag_mappings_result_condition_id_fkey"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME CONSTRAINT import_qualifier_mappings_result_condition_value_source_check
         TO import_result_flag_mappings_condition_value_source_check"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       RENAME CONSTRAINT import_qualifier_mappings_result_action_check
         TO import_result_flag_mappings_result_action_check"
    )
    DBI::dbExecute(
      con,
      "ALTER SEQUENCE discrete.import_qualifier_mappings_import_qualifier_mapping_id_seq
       RENAME TO import_result_flag_mappings_import_result_flag_mapping_id_seq"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_profiles
       SET column_map = (column_map - 'result_qualifier') ||
         CASE
           WHEN column_map ? 'result_flag' THEN '{}'::jsonb
           ELSE jsonb_build_object(
             'result_flag',
             column_map -> 'result_qualifier'
           )
         END
       WHERE column_map ? 'result_qualifier'"
    )

    # Replace the legacy one-way sample pointer with an explicit replicate-set
    # group. Connected components preserve stars, chains, and cycles without
    # silently discarding any relationship recorded by linked_with.
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.sample_group_types (
         group_type,
         group_type_name,
         group_type_name_fr,
         description,
         description_fr,
         sort_order
       ) VALUES (
         'replicate_set',
         'Replicate set',
         'Ensemble de réplicats',
         'Samples that are duplicates or replicates and may be evaluated or averaged together.',
         'Échantillons en double ou réplicats pouvant être évalués ou moyennés ensemble.',
         (
           SELECT COALESCE(max(sort_order), 0) + 1
           FROM discrete.sample_group_types
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE TEMP TABLE patch61_linked_sample_components
         ON COMMIT DROP
       AS
       WITH RECURSIVE
       edges AS (
         SELECT sample_id AS from_id, linked_with AS to_id
         FROM discrete.samples
         WHERE linked_with IS NOT NULL
         UNION ALL
         SELECT linked_with AS from_id, sample_id AS to_id
         FROM discrete.samples
         WHERE linked_with IS NOT NULL
       ),
       nodes AS (
         SELECT from_id AS sample_id FROM edges
         UNION
         SELECT to_id AS sample_id FROM edges
       ),
       reachable(origin_id, sample_id) AS (
         SELECT sample_id, sample_id
         FROM nodes
         UNION
         SELECT reachable.origin_id, edges.to_id
         FROM reachable
         JOIN edges ON edges.from_id = reachable.sample_id
       )
       SELECT sample_id, min(origin_id)::integer AS component_id
       FROM reachable
       GROUP BY sample_id"
    )
    DBI::dbExecute(
      con,
      "CREATE TEMP TABLE patch61_linked_group_map (
         component_id INTEGER PRIMARY KEY,
         sample_group_id INTEGER NOT NULL UNIQUE
       ) ON COMMIT DROP"
    )
    DBI::dbExecute(
      con,
      "WITH component_details AS (
         SELECT
           component.component_id,
           anchor.owner,
           anchor.contributor,
           (
             SELECT min(member.datetime)
             FROM patch61_linked_sample_components member_component
             JOIN discrete.samples member
               ON member.sample_id = member_component.sample_id
             WHERE member_component.component_id = component.component_id
           ) AS start_datetime,
           (
             SELECT max(member.datetime)
             FROM patch61_linked_sample_components member_component
             JOIN discrete.samples member
               ON member.sample_id = member_component.sample_id
             WHERE member_component.component_id = component.component_id
           ) AS end_datetime,
           (
             SELECT array_agg(DISTINCT access_role ORDER BY access_role)
             FROM patch61_linked_sample_components member_component
             JOIN discrete.samples member
               ON member.sample_id = member_component.sample_id
             CROSS JOIN LATERAL unnest(member.share_with) access_role
             WHERE member_component.component_id = component.component_id
           ) AS share_with,
           (
             SELECT jsonb_agg(
               jsonb_build_object(
                 'sample_id', linked_sample.sample_id,
                 'linked_with', linked_sample.linked_with
               ) ORDER BY linked_sample.sample_id
             )
             FROM patch61_linked_sample_components member_component
             JOIN discrete.samples linked_sample
               ON linked_sample.sample_id = member_component.sample_id
             WHERE member_component.component_id = component.component_id
               AND linked_sample.linked_with IS NOT NULL
           ) AS legacy_links
         FROM (
           SELECT DISTINCT component_id
           FROM patch61_linked_sample_components
         ) component
         JOIN discrete.samples anchor
           ON anchor.sample_id = component.component_id
       ),
       inserted AS (
         INSERT INTO discrete.sample_groups (
           group_type,
           group_code,
           group_name,
           start_datetime,
           end_datetime,
           owner,
           contributor,
           metadata,
           note,
           share_with
         )
         SELECT
           'replicate_set',
           'patch61-legacy-linked-with-' || component_id,
           'Migrated linked sample set ' || component_id,
           start_datetime,
           end_datetime,
           owner,
           contributor,
           jsonb_build_object(
             'migrated_from', 'discrete.samples.linked_with',
             'legacy_links', legacy_links
           ),
           'Created by Patch 61 while replacing discrete.samples.linked_with.',
           share_with
         FROM component_details
         RETURNING sample_group_id, owner, group_code
       )
       INSERT INTO patch61_linked_group_map (component_id, sample_group_id)
       SELECT component.component_id, inserted.sample_group_id
       FROM component_details component
       JOIN inserted
         ON inserted.owner = component.owner
        AND inserted.group_code =
          'patch61-legacy-linked-with-' || component.component_id"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.sample_group_members (
         sample_group_id,
         sample_id,
         sequence_in_group,
         note
       )
       SELECT
         group_map.sample_group_id,
         component.sample_id,
         row_number() OVER (
           PARTITION BY component.component_id
           ORDER BY component.sample_id
         )::integer,
         'Migrated from discrete.samples.linked_with by Patch 61.'
       FROM patch61_linked_sample_components component
       JOIN patch61_linked_group_map group_map USING (component_id)"
    )

    DBI::dbExecute(
      con,
      "DROP VIEW discrete.results_metadata_en,
         discrete.results_metadata_fr,
         discrete.samples_metadata_en,
         discrete.samples_metadata_fr"
    )

    # Keep the adapter function, mapping source, and external sample identifier
    # as separate concepts. External identity remains on the sample because one
    # AquaCache sample has one authoritative source identity.
    duplicate_sample_identity <- DBI::dbGetQuery(
      con,
      "SELECT
         import_source,
         import_source_id,
         count(*) AS duplicate_count
       FROM discrete.samples
       WHERE import_source IS NOT NULL
         AND import_source_id IS NOT NULL
         AND location_id IS NULL
       GROUP BY import_source, import_source_id
       HAVING count(*) > 1
       LIMIT 1"
    )
    if (nrow(duplicate_sample_identity)) {
      stop(
        "Patch 61 found duplicate external sample identity '",
        duplicate_sample_identity$import_source[[1]],
        "' / '",
        duplicate_sample_identity$import_source_id[[1]],
        "' among locationless samples. Resolve the duplicate before applying the patch."
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       DROP CONSTRAINT samples_import_source_identity_complete"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX discrete.samples_import_source_identity_key"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       RENAME COLUMN import_source TO source_adapter_function"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       RENAME COLUMN import_source_id TO external_sample_id"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       ADD COLUMN import_source_id INTEGER
         REFERENCES discrete.import_sources(import_source_id)
         ON UPDATE CASCADE
         ON DELETE RESTRICT,
       ADD CONSTRAINT samples_source_adapter_not_blank CHECK (
         source_adapter_function IS NULL
         OR NULLIF(btrim(source_adapter_function), '') IS NOT NULL
       ),
       ADD CONSTRAINT samples_external_id_not_blank CHECK (
         external_sample_id IS NULL
         OR NULLIF(btrim(external_sample_id), '') IS NOT NULL
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples DROP COLUMN linked_with"
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.guard_sample_external_identity()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF TG_OP = 'UPDATE'
            AND NEW.source_adapter_function IS NOT DISTINCT FROM OLD.source_adapter_function
            AND NEW.external_sample_id IS NOT DISTINCT FROM OLD.external_sample_id THEN
           RETURN NEW;
         END IF;

         IF (NEW.source_adapter_function IS NULL) <>
            (NEW.external_sample_id IS NULL) THEN
           RAISE EXCEPTION
             'source_adapter_function and external_sample_id must be supplied together for new or reassigned sample identities';
         END IF;
         RETURN NEW;
       END
       $function$"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION discrete.guard_sample_external_identity()
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guard_sample_external_identity
       BEFORE INSERT OR UPDATE OF source_adapter_function, external_sample_id
       ON discrete.samples
       FOR EACH ROW EXECUTE FUNCTION discrete.guard_sample_external_identity()"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX samples_external_identity_key
       ON discrete.samples (
         source_adapter_function,
         external_sample_id
       ) NULLS NOT DISTINCT
       WHERE source_adapter_function IS NOT NULL
         AND external_sample_id IS NOT NULL
         AND location_id IS NULL"
    )

    for (view_name in metadata_view_names) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE VIEW discrete.%s
           WITH (security_invoker = true, security_barrier = true)
           AS
           %s",
          view_name,
          metadata_view_definitions[[view_name]]
        )
      )
      view_property <- metadata_view_properties[
        metadata_view_properties$view_name == view_name,
        ,
        drop = FALSE
      ]
      DBI::dbExecute(
        con,
        paste0(
          "ALTER VIEW discrete.",
          as.character(DBI::dbQuoteIdentifier(con, view_name)),
          " OWNER TO ",
          as.character(DBI::dbQuoteIdentifier(
            con,
            view_property$owner_name[[1]]
          ))
        )
      )
      if (!is.na(view_property$view_comment[[1]])) {
        DBI::dbExecute(
          con,
          paste0(
            "COMMENT ON VIEW discrete.",
            as.character(DBI::dbQuoteIdentifier(con, view_name)),
            " IS ",
            as.character(DBI::dbQuoteString(
              con,
              view_property$view_comment[[1]]
            ))
          )
        )
      }
    }
    for (i in seq_len(nrow(metadata_view_privileges))) {
      DBI::dbExecute(
        con,
        paste0(
          "GRANT ",
          metadata_view_privileges$privilege_type[[i]],
          " ON TABLE discrete.",
          as.character(DBI::dbQuoteIdentifier(
            con,
            metadata_view_privileges$table_name[[i]]
          )),
          " TO ",
          quote_grantee(metadata_view_privileges$grantee[[i]])
        )
      )
    }

    # A source/profile pair is the common scope key. Mapping-set versions make
    # a complete collection of typed mappings publishable and reproducible.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_profiles
       ADD CONSTRAINT import_profiles_source_profile_unique
       UNIQUE (import_source_id, import_profile_id)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.import_mapping_sets (
         import_mapping_set_id INTEGER PRIMARY KEY
           GENERATED BY DEFAULT AS IDENTITY,
         import_source_id INTEGER NOT NULL,
         import_profile_id INTEGER,
         version INTEGER NOT NULL CHECK (version > 0),
         status TEXT NOT NULL DEFAULT 'draft'
           CHECK (status IN ('draft', 'published', 'retired')),
         published_at TIMESTAMP WITH TIME ZONE,
         retired_at TIMESTAMP WITH TIME ZONE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMP WITH TIME ZONE,
         CONSTRAINT import_mapping_sets_source_fkey
           FOREIGN KEY (import_source_id)
           REFERENCES discrete.import_sources(import_source_id)
           ON UPDATE CASCADE ON DELETE RESTRICT,
         CONSTRAINT import_mapping_sets_profile_source_fkey
           FOREIGN KEY (import_source_id, import_profile_id)
           REFERENCES discrete.import_profiles (
             import_source_id,
             import_profile_id
           )
           ON UPDATE CASCADE ON DELETE RESTRICT,
         CONSTRAINT import_mapping_sets_scope_version_unique
           UNIQUE NULLS NOT DISTINCT (
             import_source_id,
             import_profile_id,
             version
           ),
         CONSTRAINT import_mapping_sets_lifecycle_valid CHECK (
           (status = 'draft' AND published_at IS NULL AND retired_at IS NULL)
           OR (status = 'published' AND published_at IS NOT NULL AND retired_at IS NULL)
           OR (status = 'retired' AND published_at IS NOT NULL AND retired_at IS NOT NULL)
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_mapping_sets OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX import_mapping_sets_one_published_scope
       ON discrete.import_mapping_sets (
         import_source_id,
         import_profile_id
       ) NULLS NOT DISTINCT
       WHERE status = 'published'"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.import_mapping_sets (
         import_source_id,
         import_profile_id,
         version,
         status,
         published_at,
         note
       )
       SELECT import_source_id, NULL, 1, 'published', clock_timestamp(),
              'Migrated source-wide mappings from Patch 60'
       FROM discrete.import_sources"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.import_mapping_sets (
         import_source_id,
         import_profile_id,
         version,
         status,
         published_at,
         note
       )
       SELECT import_source_id, import_profile_id, 1, 'published',
              clock_timestamp(), 'Migrated profile mappings from Patch 60'
       FROM discrete.import_profiles"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
       ADD COLUMN import_mapping_set_id INTEGER"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_parameter_mappings mapping
       SET import_mapping_set_id = mapping_set.import_mapping_set_id
       FROM discrete.import_mapping_sets mapping_set
       WHERE mapping_set.import_source_id = mapping.import_source_id
         AND mapping_set.import_profile_id IS NULL
         AND mapping_set.status = 'published'"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
       ALTER COLUMN import_mapping_set_id SET NOT NULL,
       DROP CONSTRAINT import_parameter_mappings_unique,
       DROP CONSTRAINT import_parameter_mappings_import_source_id_fkey,
       DROP COLUMN import_source_id,
       ADD CONSTRAINT import_parameter_mappings_set_fkey
         FOREIGN KEY (import_mapping_set_id)
         REFERENCES discrete.import_mapping_sets(import_mapping_set_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD CONSTRAINT import_parameter_mappings_unique
         UNIQUE (import_mapping_set_id, source_match)"
    )

    invalid_result_flag_scope <- DBI::dbGetQuery(
      con,
      "SELECT count(*) AS invalid_count
       FROM discrete.import_result_flag_mappings mapping
       JOIN discrete.import_profiles profile
         ON profile.import_profile_id = mapping.import_profile_id
       WHERE mapping.import_source_id <> profile.import_source_id"
    )$invalid_count[[1]]
    if (invalid_result_flag_scope > 0L) {
      stop(
        "Patch 61 found ",
        invalid_result_flag_scope,
        " result-flag mapping(s) whose profile belongs to a different source."
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       ADD COLUMN import_mapping_set_id INTEGER"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_result_flag_mappings mapping
       SET import_mapping_set_id = mapping_set.import_mapping_set_id
       FROM discrete.import_mapping_sets mapping_set
       WHERE mapping_set.import_source_id = mapping.import_source_id
         AND mapping_set.import_profile_id IS NOT DISTINCT FROM mapping.import_profile_id
         AND mapping_set.status = 'published'"
    )
    result_flag_scope_fks <- DBI::dbGetQuery(
      con,
      "SELECT conname
       FROM pg_constraint
       WHERE conrelid = 'discrete.import_result_flag_mappings'::regclass
         AND contype = 'f'
         AND confrelid IN (
           'discrete.import_sources'::regclass,
           'discrete.import_profiles'::regclass
         )"
    )$conname
    for (constraint_name in result_flag_scope_fks) {
      DBI::dbExecute(
        con,
        paste0(
          "ALTER TABLE discrete.import_result_flag_mappings DROP CONSTRAINT ",
          as.character(DBI::dbQuoteIdentifier(con, constraint_name))
        )
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_result_flag_mappings
       ALTER COLUMN import_mapping_set_id SET NOT NULL,
       DROP CONSTRAINT import_qualifier_mappings_unique,
       DROP COLUMN import_source_id,
       DROP COLUMN import_profile_id,
       ADD CONSTRAINT import_result_flag_mappings_set_fkey
         FOREIGN KEY (import_mapping_set_id)
         REFERENCES discrete.import_mapping_sets(import_mapping_set_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD CONSTRAINT import_result_flag_mappings_unique
         UNIQUE NULLS NOT DISTINCT (
           import_mapping_set_id,
           source_flag_column,
           source_flag_value
         )"
    )

    # Typed defaults share a profile's exact one-to-one lifecycle. Keeping
    # them on import_profiles avoids an unnecessary extension table while
    # retaining database-enforced catalogue references.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_profiles
       ADD COLUMN media_id INTEGER REFERENCES public.media_types(media_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN collection_method_id INTEGER
         REFERENCES discrete.collection_methods(collection_method_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN sample_type_id INTEGER
         REFERENCES discrete.sample_types(sample_type_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN owner_organization_id INTEGER
         REFERENCES public.organizations(organization_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN contributor_organization_id INTEGER
         REFERENCES public.organizations(organization_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN laboratory_id INTEGER
         REFERENCES discrete.laboratories(lab_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN result_type_id INTEGER
         REFERENCES discrete.result_types(result_type_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN matrix_state_id INTEGER
         REFERENCES public.matrix_states(matrix_state_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN result_value_type_id INTEGER
         REFERENCES discrete.result_value_types(result_value_type_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN grade_type_id INTEGER
         REFERENCES public.grade_types(grade_type_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN approval_type_id INTEGER
         REFERENCES public.approval_types(approval_type_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD COLUMN sample_no_source_update BOOLEAN NOT NULL DEFAULT FALSE,
       ADD COLUMN result_no_source_update BOOLEAN NOT NULL DEFAULT FALSE"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_profiles
       SET media_id = NULLIF(defaults ->> 'media_id', '')::integer,
           collection_method_id = NULLIF(defaults ->> 'collection_method', '')::integer,
           sample_type_id = NULLIF(defaults ->> 'sample_type', '')::integer,
           owner_organization_id = NULLIF(defaults ->> 'owner', '')::integer,
           contributor_organization_id = NULLIF(defaults ->> 'contributor', '')::integer,
           laboratory_id = NULLIF(defaults ->> 'laboratory', '')::integer,
           result_type_id = NULLIF(defaults ->> 'result_type', '')::integer,
           matrix_state_id = NULLIF(defaults ->> 'matrix_state_id', '')::integer,
           result_value_type_id = NULLIF(defaults ->> 'result_value_type', '')::integer,
           grade_type_id = NULLIF(defaults ->> 'grade_type_id', '')::integer,
           approval_type_id = NULLIF(defaults ->> 'approval_type_id', '')::integer,
           sample_no_source_update = COALESCE(
             (defaults ->> 'sample_no_source_update')::boolean,
             FALSE
           ),
           result_no_source_update = COALESCE(
             (defaults ->> 'result_no_source_update')::boolean,
             FALSE
           )"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_profiles
       SET defaults = defaults - ARRAY[
         'media_id', 'collection_method', 'sample_type', 'owner',
         'contributor', 'laboratory', 'result_type', 'matrix_state_id',
         'result_value_type', 'grade_type_id', 'approval_type_id',
         'sample_no_source_update', 'result_no_source_update'
       ]::text[]"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.import_location_mappings (
         import_location_mapping_id INTEGER PRIMARY KEY
           GENERATED BY DEFAULT AS IDENTITY,
         import_mapping_set_id INTEGER NOT NULL
           REFERENCES discrete.import_mapping_sets(import_mapping_set_id)
           ON UPDATE CASCADE
           ON DELETE RESTRICT,
         source_location_code TEXT NOT NULL,
         source_location_name TEXT,
         source_location_key TEXT GENERATED ALWAYS AS (
           lower(btrim(source_location_code))
         ) STORED,
         location_id INTEGER NOT NULL
           REFERENCES public.locations(location_id)
           ON UPDATE CASCADE
           ON DELETE RESTRICT,
         sub_location_id INTEGER,
         priority INTEGER NOT NULL DEFAULT 100,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMP WITH TIME ZONE,
         CONSTRAINT import_location_mappings_source_code_not_blank
           CHECK (btrim(source_location_code) <> ''),
         CONSTRAINT import_location_mappings_sub_location_fkey
           FOREIGN KEY (location_id, sub_location_id)
           REFERENCES public.sub_locations (location_id, sub_location_id)
           ON UPDATE CASCADE
           ON DELETE RESTRICT,
         CONSTRAINT import_location_mappings_unique
           UNIQUE (import_mapping_set_id, source_location_key)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_location_mappings OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX import_location_mappings_set_idx
       ON discrete.import_location_mappings (import_mapping_set_id)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX import_location_mappings_location_idx
       ON discrete.import_location_mappings (location_id, sub_location_id)"
    )

    # Generalize the optional upload log so automated source adapters and
    # interactive applications can record the same provenance contract.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_upload_sessions
       RENAME TO import_runs"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_runs
       RENAME COLUMN import_upload_session_id TO import_run_id"
    )
    DBI::dbExecute(
      con,
      "ALTER SEQUENCE discrete.import_upload_sessions_import_upload_session_id_seq
       RENAME TO import_runs_import_run_id_seq"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_runs
       ALTER COLUMN import_profile_id DROP NOT NULL,
       ADD COLUMN import_source_id INTEGER,
       ADD COLUMN source_mapping_set_id INTEGER,
       ADD COLUMN profile_mapping_set_id INTEGER,
       ADD COLUMN source_adapter_function TEXT,
       ADD COLUMN adapter_version TEXT,
       ADD COLUMN source_uri TEXT,
       ADD COLUMN started_at TIMESTAMP WITH TIME ZONE
         NOT NULL DEFAULT CURRENT_TIMESTAMP,
       ADD COLUMN completed_at TIMESTAMP WITH TIME ZONE,
       ADD CONSTRAINT import_runs_source_fkey
         FOREIGN KEY (import_source_id)
         REFERENCES discrete.import_sources(import_source_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD CONSTRAINT import_runs_source_mapping_set_fkey
         FOREIGN KEY (source_mapping_set_id)
         REFERENCES discrete.import_mapping_sets(import_mapping_set_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD CONSTRAINT import_runs_profile_mapping_set_fkey
         FOREIGN KEY (profile_mapping_set_id)
         REFERENCES discrete.import_mapping_sets(import_mapping_set_id)
         ON UPDATE CASCADE ON DELETE RESTRICT"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_runs run
       SET import_source_id = profile.import_source_id,
           source_mapping_set_id = source_set.import_mapping_set_id,
           profile_mapping_set_id = profile_set.import_mapping_set_id,
           completed_at = CASE
             WHEN run.status IN ('committed', 'failed', 'cancelled')
             THEN COALESCE(run.committed_at, run.modified, run.created)
             ELSE NULL
           END
       FROM discrete.import_profiles profile
       JOIN discrete.import_mapping_sets source_set
         ON source_set.import_source_id = profile.import_source_id
        AND source_set.import_profile_id IS NULL
        AND source_set.status = 'published'
       LEFT JOIN discrete.import_mapping_sets profile_set
         ON profile_set.import_source_id = profile.import_source_id
        AND profile_set.import_profile_id = profile.import_profile_id
        AND profile_set.status = 'published'
       WHERE run.import_profile_id = profile.import_profile_id"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_runs
       ALTER COLUMN import_source_id SET NOT NULL,
       ALTER COLUMN source_mapping_set_id SET NOT NULL,
       DROP CONSTRAINT import_upload_sessions_import_profile_id_fkey,
       ADD CONSTRAINT import_runs_profile_fkey
         FOREIGN KEY (import_profile_id)
         REFERENCES discrete.import_profiles(import_profile_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       ADD CONSTRAINT import_runs_adapter_not_blank CHECK (
         source_adapter_function IS NULL
         OR btrim(source_adapter_function) <> ''
       ),
       ADD CONSTRAINT import_runs_completion_valid CHECK (
         completed_at IS NULL OR completed_at >= started_at
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_upload_rows
       RENAME TO import_run_rows"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_run_rows
       RENAME COLUMN import_upload_row_id TO import_run_row_id"
    )
    DBI::dbExecute(
      con,
      "ALTER SEQUENCE discrete.import_upload_rows_import_upload_row_id_seq
       RENAME TO import_run_rows_import_run_row_id_seq"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_run_rows
       RENAME COLUMN import_upload_session_id TO import_run_id"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX import_runs_source_started_idx
       ON discrete.import_runs (import_source_id, started_at DESC)"
    )

    # Published and retired revisions are historical evidence. All child-row
    # edits must occur in a draft that is then published as one unit.
    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.guard_import_mapping_row()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $$
       DECLARE
         old_status text;
         new_status text;
       BEGIN
         IF TG_OP IN ('UPDATE', 'DELETE') THEN
           SELECT status INTO old_status
           FROM discrete.import_mapping_sets
           WHERE import_mapping_set_id = OLD.import_mapping_set_id;
           IF old_status IS DISTINCT FROM 'draft' THEN
             RAISE EXCEPTION
               'Mapping rows can only be changed in a draft mapping set.';
           END IF;
         END IF;
         IF TG_OP IN ('INSERT', 'UPDATE') THEN
           SELECT status INTO new_status
           FROM discrete.import_mapping_sets
           WHERE import_mapping_set_id = NEW.import_mapping_set_id;
           IF new_status IS DISTINCT FROM 'draft' THEN
             RAISE EXCEPTION
               'Mapping rows can only be written to a draft mapping set.';
           END IF;
         END IF;
         IF TG_OP = 'DELETE' THEN
           RETURN OLD;
         END IF;
         RETURN NEW;
       END
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION discrete.guard_import_mapping_set()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF TG_OP = 'INSERT' THEN
           IF NEW.status <> 'draft' THEN
             RAISE EXCEPTION
               'New mapping sets must begin as drafts.';
           END IF;
           RETURN NEW;
         END IF;
         IF TG_OP = 'DELETE' THEN
           IF OLD.status <> 'draft' THEN
             RAISE EXCEPTION
               'Published or retired mapping sets cannot be deleted.';
           END IF;
           RETURN OLD;
         END IF;
         IF NEW.import_source_id IS DISTINCT FROM OLD.import_source_id
            OR NEW.import_profile_id IS DISTINCT FROM OLD.import_profile_id
            OR NEW.version IS DISTINCT FROM OLD.version THEN
           RAISE EXCEPTION
             'A mapping set scope and version cannot be changed.';
         END IF;
         IF NOT (
           (OLD.status = 'draft' AND NEW.status = 'published')
           OR (OLD.status = 'published' AND NEW.status = 'retired')
         ) THEN
           RAISE EXCEPTION
             'Allowed mapping-set transitions are draft to published and published to retired.';
         END IF;
         RETURN NEW;
       END
       $$"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION discrete.guard_import_mapping_row() FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION discrete.guard_import_mapping_set() FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER guard_import_mapping_set
       BEFORE INSERT OR UPDATE OR DELETE
       ON discrete.import_mapping_sets
       FOR EACH ROW EXECUTE FUNCTION discrete.guard_import_mapping_set()"
    )
    for (table_name in c(
      "import_parameter_mappings",
      "import_result_flag_mappings",
      "import_location_mappings"
    )) {
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER guard_import_mapping_row BEFORE INSERT OR UPDATE OR DELETE ON discrete.",
          table_name,
          " FOR EACH ROW EXECUTE FUNCTION discrete.guard_import_mapping_row()"
        )
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON discrete.import_location_mappings
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON discrete.import_location_mappings
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    for (table_name in "import_mapping_sets") {
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER trg_user_audit BEFORE UPDATE ON discrete.",
          table_name,
          " FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER update_modify_time BEFORE UPDATE ON discrete.",
          table_name,
          " FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
        )
      )
    }
    for (renamed_trigger in c(
      "audit_import_upload_sessions_trigger",
      "audit_import_upload_rows_trigger",
      "audit_import_qualifier_mappings_trigger"
    )) {
      renamed_table <- switch(
        renamed_trigger,
        audit_import_upload_sessions_trigger = "import_runs",
        audit_import_upload_rows_trigger = "import_run_rows",
        audit_import_qualifier_mappings_trigger = "import_result_flag_mappings"
      )
      DBI::dbExecute(
        con,
        paste0(
          "DROP TRIGGER IF EXISTS ",
          renamed_trigger,
          " ON discrete.",
          renamed_table
        )
      )
    }
    for (table_name in c(
      "import_sources",
      "import_profiles",
      "import_mapping_sets",
      "import_parameter_mappings",
      "import_result_flag_mappings",
      "import_location_mappings",
      "import_runs",
      "import_run_rows"
    )) {
      trigger_name <- paste0("audit_", table_name, "_trigger")
      DBI::dbExecute(
        con,
        paste0(
          "DROP TRIGGER IF EXISTS ",
          trigger_name,
          " ON discrete.",
          table_name
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TRIGGER ",
          trigger_name,
          " AFTER INSERT OR UPDATE OR DELETE ON discrete.",
          table_name,
          " FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
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
           'discrete',
           'import_sources',
           'generic_insert_update_delete',
           'Import source definitions determine the external provenance and namespace for every mapping.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_profiles',
           'generic_insert_update_delete',
           'Import profiles determine how external files are parsed and normalized.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_mapping_sets',
           'generic_insert_update_delete',
           'Mapping-set versions make effective import transformations reproducible.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_parameter_mappings',
           'generic_insert_update_delete',
           'Parameter mappings determine the meaning and numeric conversion of imported results.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_result_flag_mappings',
           'generic_insert_update_delete',
           'Source result-flag mappings determine result conditions and import actions.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_location_mappings',
           'generic_insert_update_delete',
           'External location-code mappings determine where imported samples are stored.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_runs',
           'generic_insert_update_delete',
           'Import runs record automated and interactive ingestion provenance and outcomes.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'discrete',
           'import_run_rows',
           'generic_insert_update_delete',
           'Import run rows preserve optional row-level normalization and validation evidence.',
           clock_timestamp(),
           clock_timestamp()
         )
       ON CONFLICT (schema_name, table_name) DO UPDATE
       SET capture_mode = EXCLUDED.capture_mode,
           rationale = EXCLUDED.rationale,
           updated_at = clock_timestamp()"
    )

    DBI::dbExecute(
      con,
      "DELETE FROM audit.table_registry
       WHERE schema_name = 'discrete'
         AND table_name IN (
           'import_profile_defaults',
           'import_qualifier_mappings',
           'import_upload_sessions',
           'import_upload_rows'
         )"
    )
    comment_table <- function(table_name, description) {
      DBI::dbExecute(
        con,
        paste0(
          "COMMENT ON TABLE discrete.",
          as.character(DBI::dbQuoteIdentifier(con, table_name)),
          " IS ",
          as.character(DBI::dbQuoteString(con, description))
        )
      )
    }
    comment_columns <- function(table_name, comments) {
      for (column_name in names(comments)) {
        DBI::dbExecute(
          con,
          paste0(
            "COMMENT ON COLUMN discrete.",
            as.character(DBI::dbQuoteIdentifier(con, table_name)),
            ".",
            as.character(DBI::dbQuoteIdentifier(con, column_name)),
            " IS ",
            as.character(DBI::dbQuoteString(con, comments[[column_name]]))
          )
        )
      }
    }
    audit_column_comments <- c(
      created_by = "Database role that created the row.",
      modified_by = "Database role that most recently modified the row.",
      created = "Timestamp when the row was created.",
      modified = "Timestamp when the row was most recently modified."
    )

    comment_table(
      "import_sources",
      "Stable namespaces for external data providers or formats. Both interactive file profiles and automated source adapters use a source to locate mappings and record provenance."
    )
    comment_columns("import_sources", c(
      import_source_id = "Surrogate identifier for the import source namespace.",
      source_code = "Stable machine-readable source code used by import APIs and adapter keys.",
      source_name = "Human-readable source name shown in applications.",
      source_description = "Optional explanation of the provider, system, or format represented by this source.",
      active = "Whether the source is available for new imports."
    , audit_column_comments))

    comment_table(
      "import_profiles",
      "Parsing and normalization configuration for one source file layout. Profiles are primarily used by interactive or batch file uploads; automated adapters may use source-wide mappings without a profile."
    )
    comment_columns("import_profiles", c(
      import_profile_id = "Surrogate identifier for the import profile.",
      import_source_id = "Source namespace whose mappings and provenance apply to this profile.",
      profile_code = "Stable machine-readable profile code unique within the source.",
      profile_name = "Human-readable profile name shown to users.",
      profile_description = "Optional explanation of the external file layout and intended use.",
      file_type = "Expected source file extension or delimited-text type.",
      parser_type = "Logical source layout: long, wide, or mixed.",
      sheet_strategy = "Rule used to select workbook sheets.",
      sheet_name = "Preferred workbook sheet name when the sheet strategy uses a name.",
      sheet_index = "One-based fallback workbook sheet position.",
      header_row = "One-based row containing source column names.",
      units_row = "Optional one-based row containing source result units.",
      parameter_row = "Optional one-based row containing source parameter labels in a wide layout.",
      data_start_row = "One-based first row containing source records.",
      datetime_origin = "Rule used to interpret date/time values, including Excel serial-date systems.",
      timezone = "IANA or database-recognized time zone used for source datetimes without an offset.",
      column_map = "Declarative JSON object mapping canonical import fields to source columns.",
      wide_config = "Declarative JSON object describing wide-layout result columns and related parser settings.",
      defaults = "Parser-specific JSON defaults not represented by the typed catalogue columns on this table.",
      sample_identity = "Ordered JSON array of normalized sample fields used to group source result rows into samples.",
      result_identity = "Ordered JSON array of normalized fields used to distinguish results within a sample.",
      validation_rules = "Declarative JSON object containing profile-specific validation settings.",
      media_id = "Default AquaCache medium assigned when the source does not provide one.",
      collection_method_id = "Default AquaCache sample collection method.",
      sample_type_id = "Default AquaCache sample type.",
      owner_organization_id = "Default organization that owns imported samples.",
      contributor_organization_id = "Default organization credited as the sample contributor.",
      laboratory_id = "Default laboratory assigned to imported results.",
      result_type_id = "Default result type assigned to imported results.",
      matrix_state_id = "Default physical matrix state assigned to imported results.",
      result_value_type_id = "Default result value type assigned to imported results.",
      grade_type_id = "Default data grade assigned when the source does not supply one.",
      approval_type_id = "Default approval state assigned when the source does not supply one.",
      sample_no_source_update = "Default source-update protection flag for imported samples.",
      result_no_source_update = "Default source-update protection flag for imported results.",
      active = "Whether the profile can be selected for new imports.",
      note = "Administrative note about the profile."
    , audit_column_comments))

    comment_table(
      "import_mapping_sets",
      "Versioned snapshots of the parameter, source result-flag, and location mappings for one import source or one source/profile override. New imports use the one published snapshot for each applicable scope, with profile mappings taking precedence over source-wide mappings. Import runs retain the exact source and profile mapping-set IDs they used, so transformations remain reproducible and auditable after mappings change. Published and retired snapshots cannot be edited; changes are made in a new draft and published together."
    )
    comment_columns("import_mapping_sets", c(
      import_mapping_set_id = "Surrogate identifier for this immutable mapping snapshot.",
      import_source_id = "Source namespace governed by the mapping snapshot.",
      import_profile_id = "Optional profile override scope; NULL denotes source-wide mappings.",
      version = "Monotonically increasing version within the source or source/profile scope.",
      status = "Lifecycle state: draft, published, or retired.",
      published_at = "Timestamp when the draft became the active published mapping snapshot.",
      retired_at = "Timestamp when a published snapshot was superseded.",
      note = "Administrative explanation of the mapping revision."
    , audit_column_comments))

    comment_table(
      "import_parameter_mappings",
      "Maps source parameter descriptors to typed AquaCache result metadata and numeric transformations within one mapping-set version."
    )
    comment_columns("import_parameter_mappings", c(
      import_mapping_id = "Surrogate identifier for the parameter mapping row.",
      import_mapping_set_id = "Versioned source or profile mapping snapshot containing this row.",
      source_match = "Non-empty JSON object of source field names and exact values that identify a result parameter.",
      parameter_id = "AquaCache parameter produced by the mapping; NULL can intentionally mark an ignored source parameter.",
      result_type = "AquaCache result type produced by the mapping.",
      sample_fraction_id = "Optional total, dissolved, filtered, or other sample fraction produced by the mapping.",
      result_value_type = "Optional typed interpretation of the result value.",
      result_speciation_id = "Optional chemical speciation produced by the mapping.",
      matrix_state_id = "Optional physical matrix state produced by the mapping.",
      conversion = "Multiplier applied to the parsed source numeric value.",
      result_offset = "Offset added after applying the conversion multiplier.",
      priority = "Ordering weight used after profile scope and match specificity; smaller values are considered first.",
      active = "Whether the mapping participates in resolution.",
      note = "Administrative explanation or provenance for the mapping."
    , audit_column_comments))

    comment_table(
      "import_result_flag_mappings",
      "Maps source-specific result flags or codes to AquaCache result conditions or import actions within one mapping-set version. This import syntax is distinct from normalized AquaCache qualifiers in public.qualifier_types, continuous.qualifiers, and discrete.sample_qualifiers. Universal numeric syntax such as leading less-than or greater-than signs may remain adapter logic so an adapter does not require seeded mappings."
    )
    comment_columns("import_result_flag_mappings", c(
      import_result_flag_mapping_id = "Surrogate identifier for the source result-flag mapping row.",
      import_mapping_set_id = "Versioned source or profile mapping snapshot containing this row.",
      source_flag_column = "Optional source column name that narrows where the result flag or code applies.",
      source_flag_value = "Exact external result flag or code to match, compared case-insensitively by the import API.",
      result_condition_id = "Optional AquaCache result condition produced by the source flag.",
      result_condition_value_source = "Source for the condition threshold: none, result, a detection-limit field, or a literal.",
      result_condition_value_literal = "Threshold already expressed in the normalized target unit, used when result_condition_value_source is literal.",
      result_action = "Action requested after matching: keep, null, skip, reject, or record only as a note.",
      note_template = "Optional human-readable note template for the normalized result.",
      priority = "Ordering weight used after profile scope and column specificity; smaller values are considered first.",
      active = "Whether the source result-flag mapping participates in resolution.",
      note = "Administrative explanation or provenance for the mapping."
    , audit_column_comments))

    comment_table(
      "import_location_mappings",
      "Maps external sample-location codes to typed AquaCache locations within one mapping-set version. This is needed when a single file can contain samples from multiple locations; automated adapters with a preselected sample-series location may omit it."
    )
    comment_columns("import_location_mappings", c(
      import_location_mapping_id = "Surrogate identifier for the location mapping row.",
      import_mapping_set_id = "Versioned source or profile mapping snapshot containing this row.",
      source_location_code = "Stable location or station code supplied by the external source.",
      source_location_name = "Optional human-readable location name supplied by the external source.",
      source_location_key = "Normalized, case-insensitive source location code used for uniqueness and matching.",
      location_id = "AquaCache location produced by the mapping.",
      sub_location_id = "Optional AquaCache sub-location produced by the mapping.",
      priority = "Ordering weight reserved for location resolution; smaller values are considered first.",
      active = "Whether the location mapping participates in resolution.",
      note = "Administrative explanation or provenance for the mapping."
    , audit_column_comments))

    comment_table(
      "import_runs",
      "One provenance record per attempted interactive file upload or automated source-adapter ingestion. The run pins the exact mapping snapshots used and stores outcome summaries without requiring row-level logging."
    )
    comment_columns("import_runs", c(
      import_run_id = "Surrogate identifier for the import attempt.",
      import_source_id = "Source namespace used for mappings and provenance.",
      import_profile_id = "Optional file profile used by the run; NULL is valid for automated adapters.",
      source_mapping_set_id = "Exact published source-wide mapping snapshot used by the run.",
      profile_mapping_set_id = "Exact published profile override snapshot used by the run, if any.",
      source_adapter_function = "Registered AquaCache function or application adapter that performed the import.",
      adapter_version = "Optional package, adapter, or application version that performed the import.",
      source_uri = "Optional URL, path, dataset key, or other stable source locator.",
      source_file_name = "Original source file name for file-based imports.",
      source_file_hash = "Optional content hash used to identify or deduplicate the source file.",
      source_file_size = "Optional source file size in bytes.",
      status = "Run lifecycle or terminal outcome.",
      summary = "JSON object of source, sample, result, or other run counts.",
      validation_summary = "JSON object summarizing warnings, errors, and validation outcomes.",
      started_at = "Timestamp when ingestion began.",
      completed_at = "Timestamp when ingestion reached a terminal state.",
      committed_at = "Timestamp when a successful run committed its normalized data.",
      note = "Administrative or failure note for the run."
    , audit_column_comments))

    comment_table(
      "import_run_rows",
      "Optional one-to-many row or result evidence for an import run. Interactive validation can retain source and normalized records; high-volume automated adapters may store only the run summary."
    )
    comment_columns("import_run_rows", c(
      import_run_row_id = "Surrogate identifier for the retained row-level evidence.",
      import_run_id = "Import run that produced or evaluated this row.",
      sheet_name = "Optional workbook sheet or equivalent source partition.",
      source_row_number = "Optional one-based row number in the source partition.",
      result_index = "One-based result position when a source row produces multiple results.",
      source_record = "JSON object preserving the relevant as-sourced fields.",
      normalized_sample = "JSON object containing the normalized sample fields considered by the import.",
      normalized_result = "JSON object containing the normalized result fields considered by the import.",
      validation_status = "Row-level validation or commit status.",
      validation_messages = "JSON array of row-level validation messages.",
      sample_id = "Committed AquaCache sample, when one was created or matched.",
      result_id = "Committed AquaCache result, when one was created or matched."
    , audit_column_comments))

    comment_columns("samples", c(
      source_adapter_function = "Registered AquaCache adapter function that supplied this sample.",
      external_sample_id = "Stable sample identifier assigned by the external source.",
      import_source_id = "Optional typed reference to the import mapping source whose configuration normalized this sample."
    ))

    sample_privileges <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT
         CASE
           WHEN acl.grantee = 0 THEN 'PUBLIC'
           ELSE pg_get_userbyid(acl.grantee)
         END AS grantee,
         upper(acl.privilege_type) AS privilege_type
       FROM pg_class relation
       JOIN pg_namespace namespace
         ON namespace.oid = relation.relnamespace
       CROSS JOIN LATERAL aclexplode(
         COALESCE(
           relation.relacl,
           acldefault('r', relation.relowner)
         )
       ) acl
       WHERE namespace.nspname = 'discrete'
         AND relation.relname = 'samples'
         AND upper(acl.privilege_type) IN ('SELECT', 'INSERT', 'UPDATE')"
    )
    mapping_tables <- c(
      "import_sources",
      "import_profiles",
      "import_mapping_sets",
      "import_parameter_mappings",
      "import_result_flag_mappings",
      "import_location_mappings",
      "import_runs",
      "import_run_rows"
    )
    for (i in seq_len(nrow(sample_privileges))) {
      for (table_name in mapping_tables) {
        DBI::dbExecute(
          con,
          paste0(
            "GRANT ",
            sample_privileges$privilege_type[[i]],
            " ON TABLE discrete.",
            table_name,
            " TO ",
            quote_grantee(sample_privileges$grantee[[i]])
          )
        )
      }
    }
    sample_sequence_grantees <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT
         CASE
           WHEN acl.grantee = 0 THEN 'PUBLIC'
           ELSE pg_get_userbyid(acl.grantee)
         END AS grantee
       FROM pg_class sequence
       JOIN pg_namespace namespace
         ON namespace.oid = sequence.relnamespace
       CROSS JOIN LATERAL aclexplode(
         COALESCE(
           sequence.relacl,
           acldefault('S', sequence.relowner)
         )
       ) acl
       WHERE namespace.nspname = 'discrete'
         AND sequence.relname = 'samples_sample_id_seq'
         AND upper(acl.privilege_type) = 'USAGE'"
    )$grantee
    for (role_name in sample_sequence_grantees) {
      for (sequence_name in c(
        "import_sources_import_source_id_seq",
        "import_profiles_import_profile_id_seq",
        "import_mapping_sets_import_mapping_set_id_seq",
        "import_parameter_mappings_import_mapping_id_seq",
        "import_result_flag_mappings_import_result_flag_mapping_id_seq",
        "import_location_mappings_import_location_mapping_id_seq",
        "import_runs_import_run_id_seq",
        "import_run_rows_import_run_row_id_seq"
      )) {
        if (
          isTRUE(DBI::dbGetQuery(
            con,
            "SELECT to_regclass($1) IS NOT NULL AS available",
            params = list(paste0("discrete.", sequence_name))
          )$available[[1]])
        ) {
          DBI::dbExecute(
            con,
            paste0(
              "GRANT USAGE, SELECT ON SEQUENCE discrete.",
              sequence_name,
              " TO ",
              quote_grantee(role_name)
            )
          )
        }
      }
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '61'
       WHERE item = 'Last patch number'"
    )
    patch_package_version <- as.character(packageVersion("AquaCache"))
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(patch_package_version)
    )

    final_verification <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'import_parameter_mappings'
             AND column_name = 'import_mapping_set_id'
         ) AS parameter_mapping_set_exists,
         (
           SELECT count(*)
           FROM pg_constraint
           WHERE conname IN (
             'import_parameter_mappings_set_fkey',
             'import_result_flag_mappings_set_fkey',
             'import_location_mappings_import_mapping_set_id_fkey'
           )
         ) = 3 AS mapping_set_constraints_exist,
         (
           SELECT count(*) = 4
           FROM pg_trigger
           WHERE NOT tgisinternal
             AND tgname IN (
               'guard_import_mapping_set',
               'guard_import_mapping_row'
             )
             AND tgrelid IN (
               'discrete.import_mapping_sets'::regclass,
               'discrete.import_parameter_mappings'::regclass,
               'discrete.import_result_flag_mappings'::regclass,
               'discrete.import_location_mappings'::regclass
             )
         ) AS mapping_revision_guards_exist,
         to_regclass('discrete.import_mapping_sets') IS NOT NULL
           AS mapping_sets_exist,
         to_regclass('discrete.import_result_flag_mappings') IS NOT NULL
           AS result_flag_mappings_exist,
         to_regclass('discrete.import_qualifier_mappings') IS NULL
           AS ambiguous_import_qualifier_name_removed,
         (
           SELECT count(*) = 4
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'import_result_flag_mappings'
             AND column_name IN (
               'import_result_flag_mapping_id',
               'source_flag_column',
               'source_flag_value',
               'result_condition_id'
             )
         ) AS result_flag_columns_renamed,
         to_regclass('discrete.import_profile_defaults') IS NULL
           AS redundant_profile_defaults_removed,
         (
           SELECT count(*) = 13
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'import_profiles'
             AND column_name IN (
               'media_id', 'collection_method_id', 'sample_type_id',
               'owner_organization_id', 'contributor_organization_id',
               'laboratory_id', 'result_type_id', 'matrix_state_id',
               'result_value_type_id', 'grade_type_id', 'approval_type_id',
               'sample_no_source_update', 'result_no_source_update'
             )
         ) AS typed_profile_defaults_exist,
         to_regclass('discrete.import_location_mappings') IS NOT NULL
           AS location_mappings_exist,
         to_regclass('discrete.import_runs') IS NOT NULL
           AS import_runs_exist,
         to_regclass('discrete.import_run_rows') IS NOT NULL
           AS import_run_rows_exist,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns column_info
           LEFT JOIN pg_namespace namespace
             ON namespace.nspname = column_info.table_schema
           LEFT JOIN pg_class relation
             ON relation.relnamespace = namespace.oid
            AND relation.relname = column_info.table_name
           LEFT JOIN pg_attribute attribute
             ON attribute.attrelid = relation.oid
            AND attribute.attname = column_info.column_name
            AND attribute.attnum > 0
            AND NOT attribute.attisdropped
           LEFT JOIN pg_description description
             ON description.objoid = relation.oid
            AND description.objsubid = attribute.attnum
           WHERE column_info.table_schema = 'discrete'
             AND column_info.table_name IN (
               'import_sources',
               'import_profiles',
               'import_mapping_sets',
               'import_parameter_mappings',
               'import_result_flag_mappings',
               'import_location_mappings',
               'import_runs',
               'import_run_rows'
             )
             AND NULLIF(btrim(description.description), '') IS NULL
         ) AS import_columns_documented,
         to_regclass('discrete.import_upload_sessions') IS NULL
           AS old_upload_sessions_removed,
         to_regclass('discrete.import_upload_rows') IS NULL
           AS old_upload_rows_removed,
         (
           SELECT count(*) = 3
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'samples'
             AND column_name IN (
               'source_adapter_function',
               'external_sample_id',
               'import_source_id'
             )
         ) AS sample_identity_columns_exist,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'samples'
             AND column_name = 'linked_with'
         ) AS legacy_linked_with_removed,
         EXISTS (
           SELECT 1
           FROM discrete.sample_group_types
           WHERE group_type = 'replicate_set'
             AND active
         ) AS replicate_set_type_exists,
         NOT EXISTS (
           SELECT 1
           FROM patch61_linked_sample_components component
           LEFT JOIN patch61_linked_group_map group_map USING (component_id)
           LEFT JOIN discrete.sample_group_members member
             ON member.sample_group_id = group_map.sample_group_id
            AND member.sample_id = component.sample_id
           WHERE group_map.sample_group_id IS NULL
              OR member.sample_group_member_id IS NULL
         ) AS legacy_sample_links_migrated,
         EXISTS (
           SELECT 1
           FROM pg_trigger
           WHERE tgrelid = 'discrete.samples'::regclass
             AND tgname = 'guard_sample_external_identity'
             AND NOT tgisinternal
         ) AS sample_identity_guard_exists,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name IN (
               'samples_metadata_en',
               'samples_metadata_fr',
               'results_metadata_en',
               'results_metadata_fr'
             )
             AND (
               (
                 table_name IN ('samples_metadata_en', 'samples_metadata_fr')
                 AND column_name IN ('import_source', 'import_source_id')
               )
               OR (
                 table_name IN ('results_metadata_en', 'results_metadata_fr')
                 AND column_name IN (
                   'sample_import_source',
                   'sample_import_source_id'
                 )
               )
             )
         ) AS metadata_view_identity_names_current,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name IN (
               'samples_metadata_en',
               'samples_metadata_fr',
               'results_metadata_en',
               'results_metadata_fr'
             )
             AND column_name LIKE '%linked_sample_id%'
         ) AS metadata_view_legacy_links_removed,
         NOT EXISTS (
           SELECT 1
           FROM discrete.import_sources source
           LEFT JOIN discrete.import_mapping_sets mapping_set
             ON mapping_set.import_source_id = source.import_source_id
            AND mapping_set.import_profile_id IS NULL
            AND mapping_set.status = 'published'
           WHERE mapping_set.import_mapping_set_id IS NULL
         ) AS every_source_has_published_set,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'discrete.import_location_mappings'::regclass
             AND conname = 'import_location_mappings_sub_location_fkey'
         ) AS location_sub_location_constraint_exists,
         (
           SELECT version = '61'
           FROM information.version_info
           WHERE item = 'Last patch number'
         ) AS patch_number_is_current,
         (
           SELECT version = $1
           FROM information.version_info
           WHERE item = 'AquaCache R package used for last patch'
         ) AS patch_package_version_is_current",
      params = list(patch_package_version)
    )
    if (!all(unlist(final_verification[1, ], use.names = FALSE))) {
      failed <- names(final_verification)[
        !vapply(final_verification[1, ], isTRUE, logical(1))
      ]
      stop(
        "Patch 61 final verification failed: ",
        paste(failed, collapse = ", "),
        "."
      )
    }

    if (isTRUE(patch61_commit)) {
      DBI::dbExecute(con, "COMMIT")
      active <- FALSE
      message(
        "Patch 61 applied successfully. Discrete imports now use versioned mapping sets, typed profile defaults, generalized run provenance, explicit source identities, and replicate-set sample groups instead of linked_with."
      )
    } else {
      message(
        "Patch 61 staged successfully in the current transaction. Run workflow checks now, then explicitly ROLLBACK or COMMIT."
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
