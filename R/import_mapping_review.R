
#' Review current import mappings
#'
#' Returns one current row per directly scoped mapping. Historical audit events
#' remain available in the audit tables; the legacy revision graph is not part
#' of the current editor.
#' @param con AquaCache database connection.
#' @param source_code Optional source-code filter.
#' @param include_history Legacy argument retained for call compatibility.
#' @return A data.frame of current mapping definitions and validation state.
#' @export
getImportMappingReview <- function(con, source_code = NULL, include_history = FALSE) {
  if (!is.logical(include_history) || length(include_history) != 1L || is.na(include_history)) {
    stop("'include_history' must be TRUE or FALSE.")
  }
  if (!is.null(source_code) && (!is.character(source_code) || length(source_code) != 1L || is.na(source_code) || !nzchar(trimws(source_code)))) {
    stop("'source_code' must be NULL or a non-empty source code.")
  }
  params <- list(if (is.null(source_code)) NA_character_ else trimws(source_code))
  DBI::dbGetQuery(con,
    "WITH mappings AS (
       SELECT s.source_code,s.source_name,s.import_source_id,s.active AS source_active,
         p.profile_code,p.profile_name,p.import_profile_id,p.active AS profile_active,
         'parameter'::TEXT AS mapping_type,m.import_mapping_id,
         NULL::INTEGER AS import_location_mapping_id,
         NULL::INTEGER AS import_result_flag_mapping_id,
         m.source_match::TEXT AS source_match,
         a.param_name::TEXT AS target,m.active,
         discrete.import_parameter_mapping_error(m) AS issue,m.note,
         m.created,m.created_by,m.modified,m.modified_by,
         (to_jsonb(m)-ARRAY['import_mapping_id','import_source_id','import_profile_id',
           'created','created_by','modified','modified_by'])::TEXT AS definition
       FROM discrete.import_parameter_mappings m
       JOIN discrete.import_sources s USING(import_source_id)
       LEFT JOIN discrete.import_profiles p USING(import_profile_id)
       LEFT JOIN public.parameters a USING(parameter_id)
       UNION ALL
       SELECT s.source_code,s.source_name,s.import_source_id,s.active,p.profile_code,p.profile_name,p.import_profile_id,p.active,
         'location'::TEXT,m.import_location_mapping_id,m.import_location_mapping_id,
         NULL::INTEGER,
         jsonb_build_object('source_location_code',m.source_location_code)::TEXT,
         concat_ws(' / ',l.name,sl.sub_location_name)::TEXT,m.active,
         CASE WHEN m.location_id IS NULL THEN 'Choose an AquaCache location.' END::TEXT,
         m.note,m.created,m.created_by,m.modified,m.modified_by,
         (to_jsonb(m)-ARRAY['import_location_mapping_id','import_source_id','import_profile_id',
           'created','created_by','modified','modified_by'])::TEXT
       FROM discrete.import_location_mappings m
       JOIN discrete.import_sources s USING(import_source_id)
       LEFT JOIN discrete.import_profiles p USING(import_profile_id)
       LEFT JOIN public.locations l USING(location_id)
       LEFT JOIN public.sub_locations sl USING(sub_location_id)
       UNION ALL
       SELECT s.source_code,s.source_name,s.import_source_id,s.active,p.profile_code,p.profile_name,p.import_profile_id,p.active,
         'result_flag'::TEXT,m.import_result_flag_mapping_id,NULL::INTEGER,
         m.import_result_flag_mapping_id,
         jsonb_build_object('source_flag_column',m.source_flag_column,
           'source_flag_value',m.source_flag_value)::TEXT,
         concat_ws(' / ',m.result_action,c.result_condition)::TEXT,m.active,NULL::TEXT,
         m.note,m.created,m.created_by,m.modified,m.modified_by,
         (to_jsonb(m)-ARRAY['import_result_flag_mapping_id','import_source_id','import_profile_id',
           'created','created_by','modified','modified_by'])::TEXT
       FROM discrete.import_result_flag_mappings m
       JOIN discrete.import_sources s USING(import_source_id)
       LEFT JOIN discrete.import_profiles p USING(import_profile_id)
       LEFT JOIN discrete.result_conditions c USING(result_condition_id)
     )
     SELECT *, (profile_code IS NOT NULL) AS profile_specific
     FROM mappings
     WHERE $1::TEXT IS NULL OR source_code=$1
     ORDER BY source_code,profile_code NULLS FIRST,mapping_type,source_match,
       COALESCE(import_mapping_id,import_location_mapping_id,import_result_flag_mapping_id)",
    params = params
  )
}
