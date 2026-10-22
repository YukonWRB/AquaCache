# Patch 66: instance-safe parameter catalogue consolidation after typed Patch 65.
# Records unreported EQWin acidity chemistry explicitly as unknown, without
# changing source matches or replacing a reporting basis already identified.
# Like earlier patches, source this file with an existing connection named con.
# It owns one transaction, commits on success, and rolls back the entire patch
# on error. It neither opens a connection nor selects a database implicitly.
# Catalogue decisions are listed in patch_66/parameters.tsv and
# patch_66/catalogue_review.tsv; the helper documents the unresolved mapping rule.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop("Patch 66 requires a postgres maintenance connection.")
}
if (dbTransCheck(con)) {
  stop("A transaction is already in progress. Commit or roll it back first.")
}
patch66_path <- system.file("patches", "patch_66", package = "AquaCache")
if (!nzchar(patch66_path)) {
  stop("Missing installed Patch 66 migration files.")
}
patch66_helpers <- new.env(parent = asNamespace("AquaCache"))
sys.source(file.path(patch66_path, "cleanup.R"), envir = patch66_helpers)
message(
  "Working on patch 66: consolidating reviewed parameter aliases and analytical variants."
)
active <- dbTransBegin(con)
tryCatch(
  {
    DBI::dbExecute(con, "SET LOCAL lock_timeout = '30s'")
    DBI::dbExecute(con, "LOCK TABLE information.version_info IN EXCLUSIVE MODE")
    revision <- DBI::dbGetQuery(
      con,
      "SELECT version FROM information.version_info WHERE item = 'Last patch number'"
    )
    if (nrow(revision) != 1L || revision$version[[1]] != "65") {
      stop("Patch 66 requires exactly revision 65.")
    }
    # Patch 65 migration-only objects have no forward-facing application
    # consumer. Keep this cleanup in its own transactional section; unexpected
    # dependencies abort the patch because these drops intentionally use no CASCADE.
    message("Removing Patch 65 migration-only chemistry history.")
    DBI::dbExecute(
      con,
      "DROP TABLE IF EXISTS information.patch65_result_descriptor_crosswalk"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS information.guard_chemistry_migration_history()"
    )
    patch66_report <- patch66_helpers$patch66_parameter_cleanup(
      con,
      file.path(patch66_path, "parameters.tsv")
    )
    message("Adding declared matrix states to legacy guideline sample lookups.")
    patch66_helpers$patch66_guideline_matrix_state_fix(con)
    message("Removing ECCC archive end dates from current aggregation methods.")
    patch66_eccc_methods <- patch66_helpers$patch66_eccc_applicability_fix(con)
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.analysis_conditions IS
     'JSONB object of analytical conditions that affect interpretation of the numeric result, such as mass or gas basis and reference temperature or pressure. An empty object means no conditions are recorded. The database normalizes conditions, validates common fields, and enforces parameter- and matrix-specific required keys. Additional named keys are currently permitted, so their names and meanings must be coordinated across database clients. Keep laboratory, protocol, and source provenance in their dedicated fields. This object contributes to result identity through analysis_conditions_key.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.analysis_conditions_key IS
     'Database-generated SHA-256 digest of normalized analysis_conditions. This is a compact condition identity component in the results_analytical_identity unique constraint, not an identifier for the complete result row. Do not supply or edit this value directly. Changes to the normalization or digest functions require a controlled migration to recompute stored values and recheck result uniqueness.'"
    )
    DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '66' WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
     WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(utils::packageVersion("AquaCache")))
    )
    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 66 applied successfully. Parameter references were preserved and unreported EQWin acidity metadata was marked unknown; ",
      nrow(patch66_eccc_methods),
      " current ECCC applicability range(s) were corrected."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back Patch 66...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
