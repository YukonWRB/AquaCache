# Patch 66: instance-safe parameter catalogue consolidation after typed Patch 65.
# Like earlier patches, source this file with an existing connection named con.
# It owns one transaction, commits on success, and rolls back the entire patch
# on error. It neither opens a connection nor selects a database implicitly.
# See patch_66/REVIEW.md for catalogue decisions and verification boundaries.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop("Patch 66 requires a postgres maintenance connection.")
}
if (AquaCache:::dbTransCheck(con)) {
  stop("A transaction is already in progress. Commit or roll it back first.")
}
patch66_path <- system.file("patches", "patch_66", package = "AquaCache")
if (!nzchar(patch66_path)) stop("Missing installed Patch 66 migration files.")
patch66_helpers <- new.env(parent = asNamespace("AquaCache"))
sys.source(file.path(patch66_path, "cleanup.R"), envir = patch66_helpers)
message("Working on patch 66: consolidating reviewed parameter aliases and analytical variants.")
active <- AquaCache:::dbTransBegin(con)
tryCatch({
  DBI::dbExecute(con, "SET LOCAL lock_timeout = '30s'")
  DBI::dbExecute(con, "LOCK TABLE information.version_info IN EXCLUSIVE MODE")
  revision <- DBI::dbGetQuery(con,
    "SELECT version FROM information.version_info WHERE item = 'Last patch number'")
  if (nrow(revision) != 1L || revision$version[[1]] != "65") {
    stop("Patch 66 requires exactly revision 65.")
  }
  patch66_report <- patch66_helpers$patch66_parameter_cleanup(con,
    file.path(patch66_path, "parameters.tsv"))
  DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
  DBI::dbExecute(con,
    "UPDATE information.version_info SET version = '66' WHERE item = 'Last patch number'")
  DBI::dbExecute(con,
    "UPDATE information.version_info SET version = $1
     WHERE item = 'AquaCache R package used for last patch'",
    params = list(as.character(utils::packageVersion("AquaCache"))))
  DBI::dbExecute(con, "COMMIT")
  active <- FALSE
  message("Patch 66 applied successfully. Parameter references and analytical identities were preserved.")
}, error = function(e) {
  if (isTRUE(active)) {
    message("Error detected. Rolling back Patch 66...")
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
  }
  stop(e)
})
