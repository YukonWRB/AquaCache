# DEV PATCH PLACEHOLDER
#
# This file is intentionally named DEV_patch_44.R, not patch_44.R, so
# AquaConnect's patch discovery should not run it automatically. Rename a
# finalized version to patch_44.R only after replacing the examples below with
# the real patch changes and testing them against a representative database.
#
# Goal for future patches:
# Avoid hard-coded role or group names in GRANT/REVOKE statements. Different
# database instances may not have the same login roles, group roles, or
# application roles, so a portable patch should derive privileges from existing
# objects in the target database and only apply grants to roles that exist there.
#
# Recommended pattern:
#
# 1. Pick a source object whose access model already matches the new object.
#    Examples:
#    - For a new discrete.samples_* view, copy SELECT grants from
#      discrete.samples or another existing user-facing samples view.
#    - For a new discrete.results_* view, copy SELECT grants from
#      discrete.results or another existing user-facing results view.
#    - For a replacement function, capture EXECUTE grants before dropping or
#      replacing the function, then replay those grants after CREATE OR REPLACE.
#    - For a new sibling function, copy EXECUTE grants from the closest existing
#      function with the same expected audience.
#
# 2. Do not write role names directly in the patch unless the role is guaranteed
#    by PostgreSQL itself, such as PUBLIC. Even then, preserve PUBLIC only when
#    the source object already grants to PUBLIC or when the patch intentionally
#    changes the access model.
#
# 3. Recheck role existence immediately before granting. A role discovered from
#    source object ACLs can still be absent on another database instance, and a
#    patch should skip missing roles rather than fail halfway through.
#
# 4. Quote object and role identifiers with DBI::dbQuoteIdentifier(). Function
#    signatures used in GRANT EXECUTE ON FUNCTION statements cannot be parameterized
#    as identifiers, so keep those signatures as trusted constants inside the
#    patch, not as user-supplied values.
#
# 5. Run the patch in a transaction. Create/replace objects first, replay derived
#    grants second, then update information.version_info last.
#
#
# Relation grants template
# ------------------------
# Use this shape for views/tables. Leave it as patch-local helper code so future
# patches stay self-contained.
#
# quote_role <- function(con, role_name) {
#   if (toupper(role_name) == "PUBLIC") {
#     return("PUBLIC")
#   }
#   as.character(DBI::dbQuoteIdentifier(con, role_name))
# }
#
# relation_name <- function(con, schema_name, relation_name) {
#   paste(
#     as.character(DBI::dbQuoteIdentifier(con, schema_name)),
#     as.character(DBI::dbQuoteIdentifier(con, relation_name)),
#     sep = "."
#   )
# }
#
# existing_roles <- function(con) {
#   DBI::dbGetQuery(con, "SELECT rolname FROM pg_catalog.pg_roles")$rolname
# }
#
# copy_relation_grants <- function(con, source_schema, source_relation,
#                                  target_schema, target_relation,
#                                  privileges = "SELECT") {
#   grants <- DBI::dbGetQuery(
#     con,
#     "SELECT grantee, privilege_type
#      FROM information_schema.role_table_grants
#      WHERE table_schema = $1
#        AND table_name = $2
#        AND privilege_type = ANY($3)
#      ORDER BY grantee, privilege_type",
#     params = list(source_schema, source_relation, privileges)
#   )
#
#   if (!nrow(grants)) {
#     return(invisible(FALSE))
#   }
#
#   roles <- existing_roles(con)
#   target <- relation_name(con, target_schema, target_relation)
#
#   for (i in seq_len(nrow(grants))) {
#     grantee <- grants$grantee[i]
#     if (toupper(grantee) != "PUBLIC" && !grantee %in% roles) {
#       next
#     }
#
#     sql <- sprintf(
#       "GRANT %s ON TABLE %s TO %s",
#       grants$privilege_type[i],
#       target,
#       quote_role(con, grantee)
#     )
#     DBI::dbExecute(con, sql)
#   }
#
#   invisible(TRUE)
# }
#
# Example use after creating new metadata views:
#
# copy_relation_grants(
#   con,
#   source_schema = "discrete",
#   source_relation = "samples",
#   target_schema = "discrete",
#   target_relation = "samples_metadata_en"
# )
#
# copy_relation_grants(
#   con,
#   source_schema = "discrete",
#   source_relation = "results",
#   target_schema = "discrete",
#   target_relation = "results_metadata_en"
# )
#
#
# Function grants template
# ------------------------
# Capture function EXECUTE grants before DROP FUNCTION or CREATE OR REPLACE if
# there is any chance the ACL could be reset. PostgreSQL function identity should
# include the argument types, for example:
#
# source_signature <- "continuous.measurements_continuous_corrected(integer[], timestamp without time zone, timestamp without time zone)"
#
# get_function_execute_grants <- function(con, function_signature) {
#   DBI::dbGetQuery(
#     con,
#     "SELECT
#        CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE grantee_role.rolname END AS grantee,
#        acl.is_grantable
#      FROM pg_catalog.pg_proc p
#      CROSS JOIN LATERAL pg_catalog.aclexplode(
#        COALESCE(p.proacl, pg_catalog.acldefault('f', p.proowner))
#      ) acl
#      LEFT JOIN pg_catalog.pg_roles grantee_role
#        ON grantee_role.oid = acl.grantee
#      WHERE p.oid = $1::regprocedure
#        AND acl.privilege_type = 'EXECUTE'
#      ORDER BY grantee",
#     params = list(function_signature)
#   )
# }
#
# apply_function_execute_grants <- function(con, target_signature, grants) {
#   if (!nrow(grants)) {
#     return(invisible(FALSE))
#   }
#
#   roles <- existing_roles(con)
#
#   for (i in seq_len(nrow(grants))) {
#     grantee <- grants$grantee[i]
#     if (toupper(grantee) != "PUBLIC" && !grantee %in% roles) {
#       next
#     }
#
#     sql <- sprintf(
#       "GRANT EXECUTE ON FUNCTION %s TO %s%s",
#       target_signature,
#       quote_role(con, grantee),
#       if (isTRUE(grants$is_grantable[i])) " WITH GRANT OPTION" else ""
#     )
#     DBI::dbExecute(con, sql)
#   }
#
#   invisible(TRUE)
# }
#
# Example replacement flow:
#
# corrected_grants <- get_function_execute_grants(con, source_signature)
# DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION ...")
# apply_function_execute_grants(con, source_signature, corrected_grants)
#
#
# Verification queries
# --------------------
# After patching, check grants through PostgreSQL catalogs instead of assuming a
# role list. These queries show the effective user-facing ACLs without requiring
# any instance-specific group names in the patch.
#
# DBI::dbGetQuery(
#   con,
#   "SELECT grantee, privilege_type
#    FROM information_schema.role_table_grants
#    WHERE table_schema = 'discrete'
#      AND table_name IN (
#        'samples_metadata_en',
#        'samples_metadata_fr',
#        'results_metadata_en',
#        'results_metadata_fr'
#      )
#    ORDER BY table_name, grantee, privilege_type"
# )
#
# DBI::dbGetQuery(
#   con,
#   "SELECT p.oid::regprocedure::text AS function_name,
#           CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE r.rolname END AS grantee,
#           acl.privilege_type,
#           acl.is_grantable
#    FROM pg_catalog.pg_proc p
#    JOIN pg_catalog.pg_namespace n
#      ON n.oid = p.pronamespace
#    CROSS JOIN LATERAL pg_catalog.aclexplode(
#      COALESCE(p.proacl, pg_catalog.acldefault('f', p.proowner))
#    ) acl
#    LEFT JOIN pg_catalog.pg_roles r
#      ON r.oid = acl.grantee
#    WHERE n.nspname IN ('continuous', 'audit')
#      AND p.proname IN (
#        'measurements_continuous_corrected',
#        'measurements_continuous_corrected_at',
#        'measurements_continuous_as_of'
#      )
#    ORDER BY function_name, grantee"
# )
