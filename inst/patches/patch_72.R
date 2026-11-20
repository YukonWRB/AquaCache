# Patch 72: assign protocols in parameter mappings and standardize legacy names.
if (DBI::dbGetQuery(con, "SELECT SESSION_USER")$session_user != "postgres") {
  stop("Patch 72 requires a postgres connection.")
}
if (dbTransCheck(con)) {
  stop("Finish the current transaction before applying Patch 72.")
}
patch72_helpers <- new.env(parent = asNamespace("AquaCache"))
for (file in c("column_names.R", "routine_updates.R")) {
  sys.source(
    system.file(
      "patches",
      "patch_72",
      file,
      package = "AquaCache",
      mustWork = TRUE
    ),
    envir = patch72_helpers
  )
}
changed <- DBI::dbWithTransaction(con, {
  last_patch <- DBI::dbGetQuery(
    con,
    "SELECT version FROM information.version_info WHERE item = 'Last patch number' FOR UPDATE"
  )$version
  if (length(last_patch) != 1L || !last_patch %in% c("71", "72")) {
    stop(
      "Patch 72 requires a database at Patch 71."
    )
  }
  if (last_patch == "71") {
    patch72_helpers$patch72_rename_columns(con)
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
         ADD COLUMN protocol_method_id INTEGER REFERENCES discrete.protocols_methods
         ON UPDATE CASCADE ON DELETE SET NULL"
    )
    patch72_helpers$patch72_update_routines(con)
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.import_parameter_mappings.protocol_method_id IS
       'Optional protocol or analytical method assigned to results by this mapping.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.protocol_method_id IS
       'References discrete.protocols_methods(protocol_method_id).'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.depth_height_m IS
       'Signed vertical offset of the sample in metres relative to the local ground or water surface, or from top of well casing if applicable. Positive above the surface, negative below it, zero at the surface, and NULL when unknown or unspecified.'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '72' WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
        WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(utils::packageVersion("AquaCache")))
    )
  }
  patch72_helpers$patch72_rename_import_profile_fields(con)
  last_patch == "71"
})
if (changed) {
  message(
    "Patch 72 applied: methods and standardized column names are available."
  )
} else {
  message("Patch 72 is already applied.")
}
