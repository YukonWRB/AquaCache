# Reapply only Patch 66's parameter cleanup after Patch 66 has already been
# applied. The default is a rollback-only review run; set commit = TRUE only
# after reviewing the generated report and this script.

# This was run after the SARU database import added some duplicated parameters that patch_66 theoretically already dealt with.

patch66_reapply_parameter_cleanup <- function(
  con,
  patch66_path,
  commit = FALSE
) {
  if (!is.logical(commit) || length(commit) != 1L || is.na(commit)) {
    stop("commit must be TRUE or FALSE.")
  }
  if (
    !is.character(patch66_path) ||
      length(patch66_path) != 1L ||
      is.na(patch66_path) ||
      !dir.exists(patch66_path)
  ) {
    stop("patch66_path must identify the local Patch 66 directory.")
  }
  if (AquaCache:::dbTransCheck(con)) {
    stop(
      "Use a connection with no active transaction; this script owns its transaction."
    )
  }

  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS database,
       host(inet_server_addr()) AS host,
       current_user AS user_name,
       (SELECT version FROM information.version_info
         WHERE item = 'Last patch number') AS patch"
  )
  if (
    nrow(identity) != 1L ||
      identity$database[[1]] != "aquacache" ||
      identity$host[[1]] != "199.247.132.26" ||
      !identity$user_name[[1]] %in% c("postgres", "admin") ||
      identity$patch[[1]] != "66"
  ) {
    stop(
      "This reapplication script requires prod AquaCache at Patch 66 and a maintenance role."
    )
  }

  cleanup_path <- file.path(patch66_path, "cleanup.R")
  plan_path <- file.path(patch66_path, "parameters.tsv")
  if (!file.exists(cleanup_path) || !file.exists(plan_path)) {
    stop("Patch 66 cleanup.R or parameters.tsv is missing.")
  }
  helpers <- new.env(parent = asNamespace("AquaCache"))
  sys.source(cleanup_path, envir = helpers)

  if (!isTRUE(AquaCache::dbTransBegin(con))) {
    stop("Could not start the Patch 66 cleanup transaction.")
  }
  transaction <- new.env(parent = emptyenv())
  transaction$active <- TRUE
  on.exit(
    if (isTRUE(transaction$active)) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    },
    add = TRUE
  )

  tryCatch(
    {
      report <- helpers$patch66_parameter_cleanup(
        con,
        plan_path,
        # The SARU workbook preparation applied the EQWin mapping conversion
        # before insertion. cleanup.R verifies the complete SARU batch marker,
        # no aggregate components, and current published/draft mapping factors.
        preconverted_result_sources = "benzo[b,j]fluoranthene"
      )
      message("Patch 66 parameter-cleanup report:")
      print(report)
      DBI::dbExecute(con, "SET CONSTRAINTS ALL IMMEDIATE")
      triggers <- DBI::dbGetQuery(
        con,
        "SELECT c.relname AS table_name, t.tgname, t.tgenabled::TEXT AS state
         FROM pg_trigger t
         JOIN pg_class c ON c.oid = t.tgrelid
         WHERE NOT t.tgisinternal
           AND ((c.relname = 'parameters'
               AND t.tgname = 'prevent_used_parameter_unit_update')
             OR (c.relname = 'import_parameter_mappings'
               AND t.tgname = 'guard_import_mapping_row'))"
      )
      if (
        nrow(triggers) != 2L ||
          any(triggers$state != "O")
      ) {
        stop("A parameter-unit or import-mapping guard was not restored.")
      }

      if (commit) {
        DBI::dbExecute(con, "COMMIT")
        transaction$active <- FALSE
        message("Patch 66 parameter cleanup committed to prod AquaCache.")
        return(invisible(report))
      }

      DBI::dbExecute(con, "ROLLBACK")
      transaction$active <- FALSE
      message(
        "Rollback-only review completed; no production changes were retained."
      )
      invisible(report)
    },
    error = function(e) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
      transaction$active <- FALSE
      stop(e)
    }
  )
}


patch66_reapply_parameter_cleanup(
  con,
  system.file("patches", "patch_66", package = "AquaCache"),
  commit = TRUE
)
