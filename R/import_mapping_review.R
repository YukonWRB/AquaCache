#' Review saved import mappings across sources and workbook formats
#'
#' @description
#' Returns current published and draft parameter, location, and result-flag
#' mappings, including sources that have no workbook format. Completeness is
#' calculated with the database validator used by [publishImportMappings()].
#' No definitions are changed. Retired revisions are available for inspection.
#'
#' @param con AquaCache connection to a database at Patch 65 or later.
#' @param source_code Optional source code. `NULL` includes all sources.
#' @param include_history Include retired mapping-set revisions.
#' @return A data.frame containing source and profile scope, revision status,
#'   matching criteria, target label, activity, validation issue, audit metadata,
#'   and a JSON definition without generated IDs or audit metadata. `profile_code`
#'   is `NA` for source-wide mappings used by package download functions.
#' @export
getImportMappingReview <- function(con, source_code = NULL, include_history = FALSE) {
  if (!is.logical(include_history) || length(include_history) != 1L || is.na(include_history)) {
    stop("'include_history' must be TRUE or FALSE.")
  }
  if (!is.null(source_code) && (!is.character(source_code) || length(source_code) != 1L ||
      is.na(source_code) || !nzchar(trimws(source_code)))) {
    stop("'source_code' must be NULL or a non-empty source code.")
  }
  DBI::dbGetQuery(con,
    "WITH scopes AS (
       SELECT ms.import_mapping_set_id, ms.import_source_id, ms.import_profile_id,
              ms.version, ms.status, src.source_code, src.source_name,
              src.active AS source_active, p.profile_code, p.profile_name,
              p.active AS profile_active
       FROM discrete.import_mapping_sets ms
       JOIN discrete.import_sources src USING (import_source_id)
       LEFT JOIN discrete.import_profiles p USING (import_profile_id)
       WHERE ($1::BOOLEAN OR ms.status <> 'retired')
         AND ($2::TEXT IS NULL OR src.source_code = $2)
     ), mappings AS (
       SELECT s.*, 'parameter'::TEXT AS mapping_type, m.import_mapping_id,
              m.source_match::TEXT AS source_match, p.param_name AS target,
              m.active, discrete.import_parameter_mapping_error(m) AS issue,
              m.note, m.created, m.created_by, m.modified, m.modified_by,
              (to_jsonb(m) - ARRAY['import_mapping_id','import_mapping_set_id',
                'created','created_by','modified','modified_by'])::TEXT AS definition
       FROM scopes s JOIN discrete.import_parameter_mappings m USING (import_mapping_set_id)
       LEFT JOIN public.parameters p USING (parameter_id)
       UNION ALL
       SELECT s.*, 'location'::TEXT, m.import_location_mapping_id,
              jsonb_build_object('source_location_code',m.source_location_code)::TEXT,
              concat_ws(' / ',l.name,sl.sub_location_name), m.active,
              CASE WHEN m.location_id IS NULL THEN 'Choose an AquaCache location.' END,
              m.note, m.created, m.created_by, m.modified, m.modified_by,
              (to_jsonb(m) - ARRAY['import_location_mapping_id','import_mapping_set_id',
                'created','created_by','modified','modified_by'])::TEXT
       FROM scopes s JOIN discrete.import_location_mappings m USING (import_mapping_set_id)
       LEFT JOIN public.locations l USING (location_id)
       LEFT JOIN public.sub_locations sl USING (sub_location_id)
       UNION ALL
       SELECT s.*, 'result_flag'::TEXT, m.import_result_flag_mapping_id,
              jsonb_build_object('source_flag_column',m.source_flag_column,
                'source_flag_value',m.source_flag_value)::TEXT,
              concat_ws(' / ',m.result_action,c.result_condition),m.active,NULL::TEXT,
              m.note, m.created, m.created_by, m.modified, m.modified_by,
              (to_jsonb(m) - ARRAY['import_result_flag_mapping_id','import_mapping_set_id',
                'created','created_by','modified','modified_by'])::TEXT
       FROM scopes s JOIN discrete.import_result_flag_mappings m USING (import_mapping_set_id)
       LEFT JOIN discrete.result_conditions c USING (result_condition_id)
     )
     SELECT * FROM mappings ORDER BY source_code,profile_code NULLS FIRST,
       version DESC,mapping_type,source_match,import_mapping_id",
    params = list(include_history, if (is.null(source_code)) NA_character_ else trimws(source_code)))
}
