# Patch 63 keeps one editable draft mapping set per source/profile scope.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 63: enforcing one editable import-mapping draft per source/profile scope. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before running this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.import_sources') IS NOT NULL AS has_sources,
         to_regclass('discrete.import_profiles') IS NOT NULL AS has_profiles,
         to_regclass('discrete.import_mapping_sets') IS NOT NULL AS has_mapping_sets,
         to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS has_parameter_mappings,
         to_regclass('discrete.import_result_flag_mappings') IS NOT NULL AS has_flag_mappings,
         to_regclass('discrete.import_location_mappings') IS NOT NULL AS has_location_mappings,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 63 requires Patch 61 import sources, profiles, mapping sets, all three mapping tables, and version metadata."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "62") {
      stop("Patch 63 must be applied to a database at Patch 62.")
    }

    duplicate_drafts <- DBI::dbGetQuery(
      con,
      "SELECT count(*) AS duplicate_scope_count
       FROM (
         SELECT import_source_id, import_profile_id
         FROM discrete.import_mapping_sets
         WHERE status = 'draft'
         GROUP BY import_source_id, import_profile_id
         HAVING count(*) > 1
       ) duplicate_scopes"
    )$duplicate_scope_count[[1]]
    if (duplicate_drafts > 0L) {
      stop(
        "Patch 63 found multiple draft mapping sets for one or more source/profile scopes. Resolve those drafts before applying this patch."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX import_mapping_sets_one_draft_scope
       ON discrete.import_mapping_sets
       (import_source_id, import_profile_id) NULLS NOT DISTINCT
       WHERE status = 'draft'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON INDEX discrete.import_mapping_sets_one_draft_scope IS
       'Allows repeated parameter, location, and result-flag edits to accumulate in one draft per source/profile scope before publication.'"
    )

    verified <- DBI::dbGetQuery(
      con,
      "SELECT EXISTS (
         SELECT 1
         FROM pg_index index_info
         JOIN pg_class index_class
           ON index_class.oid = index_info.indexrelid
         JOIN pg_namespace index_schema
           ON index_schema.oid = index_class.relnamespace
         WHERE index_schema.nspname = 'discrete'
           AND index_class.relname = 'import_mapping_sets_one_draft_scope'
           AND index_info.indisunique
           AND pg_get_expr(index_info.indpred, index_info.indrelid) LIKE '%draft%'
       ) AS has_one_draft_index"
    )$has_one_draft_index[[1]]
    if (!isTRUE(verified)) {
      stop("Patch 63 verification failed.")
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '63'
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
      "Patch 63 applied successfully. Each source/profile scope can now retain one editable draft while parameter, location, and result-flag changes are accumulated."
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
