# Patch 71: configurable source-wide workbook analyte exclusions and project-specific location mappings.
patch_number <- 71L
if (DBI::dbGetQuery(con, "SELECT SESSION_USER")$session_user != "postgres") {
  stop("Patch 71 requires a postgres connection.")
}
if (dbTransCheck(con)) {
  stop("Finish the current transaction before applying Patch 71.")
}
last_patch <- DBI::dbGetQuery(
  con,
  "SELECT version FROM information.version_info WHERE item = 'Last patch number'"
)$version
if (length(last_patch) != 1L || last_patch != "70") {
  stop("Patch 71 must be applied to a database at Patch 70.")
}
active <- dbTransBegin(con)
tryCatch(
  {
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_sources
       ADD COLUMN excluded_analytes TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],
       ADD CONSTRAINT import_sources_excluded_analytes_no_nulls
         CHECK (array_position(excluded_analytes, NULL) IS NULL)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.import_sources.excluded_analytes IS
       'Complete source analyte names excluded from all workbook formats before result conversion and parameter mapping. Matching ignores case and surrounding spaces; an empty array excludes nothing.'"
    )
    # Preserve the legacy ALS filtration exclusions as editable source data if they exist.
    # Other sources deliberately start with no exclusions.
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_sources
        SET excluded_analytes = ARRAY[
          'Dissolved mercury filtration location',
          'Dissolved metals filtration location'
        ]::TEXT[]
      WHERE lower(btrim(source_code)) = 'als'"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_location_mappings
       ADD COLUMN project_id INTEGER REFERENCES public.projects(project_id)
         ON UPDATE CASCADE ON DELETE RESTRICT,
       DROP CONSTRAINT import_location_mappings_unique,
       ADD CONSTRAINT import_location_mappings_unique
         UNIQUE NULLS NOT DISTINCT
         (import_source_id, import_profile_id, source_location_key, project_id)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.import_location_mappings.project_id IS
       'Optional project scope for an external source location name. NULL is a project-free fallback. The same name may map to different targets in different projects.'"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1 WHERE item = 'Last patch number'",
      params = list(as.character(patch_number))
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
      "Patch 71 applied: source-wide workbook analyte exclusions are configurable."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
