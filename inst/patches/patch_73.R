# Patch 73: restore parameter unit and chemistry checks for direct mappings.
if (DBI::dbGetQuery(con, "SELECT SESSION_USER")$session_user != "postgres") {
  stop("Patch 73 requires a postgres connection.")
}
if (dbTransCheck(con)) {
  stop("Finish the current transaction before applying Patch 73.")
}

changed <- DBI::dbWithTransaction(con, {
  last_patch <- DBI::dbGetQuery(
    con,
    "SELECT version FROM information.version_info
     WHERE item = 'Last patch number' FOR UPDATE"
  )$version
  if (length(last_patch) != 1L || !last_patch %in% c("72", "73")) {
    stop("Patch 73 requires a database at Patch 72.")
  }
  if (last_patch == "72") {
    # Include inactive mappings in the unit check, since they can be used again.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.parameter_matrix_state_has_results(
         p_parameter_id INTEGER,
         p_matrix_state_code TEXT
       ) RETURNS BOOLEAN
       LANGUAGE SQL STABLE
       SET search_path = pg_catalog, public, discrete, continuous, criteria, spatial
       AS $function$
         SELECT EXISTS (
           SELECT 1
           FROM discrete.results r
           JOIN public.matrix_states ms USING (matrix_state_id)
           WHERE r.parameter_id = p_parameter_id AND ms.matrix_state_code = p_matrix_state_code
         ) OR EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           JOIN public.matrix_states ms USING (matrix_state_id)
           WHERE t.parameter_id = p_parameter_id AND ms.matrix_state_code = p_matrix_state_code
         ) OR EXISTS (
           SELECT 1
           FROM criteria.guidelines g
           JOIN public.matrix_states ms USING (matrix_state_id)
           WHERE g.parameter_id = p_parameter_id AND ms.matrix_state_code = p_matrix_state_code
         ) OR EXISTS (
           SELECT 1
           FROM criteria.guideline_rule_inputs i
           JOIN public.matrix_states ms USING (matrix_state_id)
           WHERE i.parameter_id = p_parameter_id AND ms.matrix_state_code = p_matrix_state_code
         ) OR EXISTS (
           SELECT 1
           FROM discrete.import_parameter_mappings m
           JOIN public.matrix_states ms USING (matrix_state_id)
           WHERE m.parameter_id = p_parameter_id AND ms.matrix_state_code = p_matrix_state_code
         ) OR EXISTS (
           SELECT 1 FROM continuous.rating_curves c
           WHERE p_parameter_id IN (c.input_parameter_id, c.output_parameter_id)
         ) OR EXISTS (
           SELECT 1 FROM public.locations_metadata_instrument_connection_signals s
           WHERE s.parameter_id = p_parameter_id
         ) OR EXISTS (
           SELECT 1 FROM spatial.raster_series_index s
           WHERE s.parameter_id = p_parameter_id
         ) OR EXISTS (
           SELECT 1 FROM spatial.rasters_reference r
           WHERE r.parameter_id = p_parameter_id
         );
       $function$"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.enforce_parameter_import_mapping_requirements()
       RETURNS TRIGGER
       LANGUAGE plpgsql SECURITY DEFINER
       SET search_path = pg_catalog, public, discrete
       AS $function$
       DECLARE
         dimension TEXT;
         column_name TEXT;
         missing_mapping BOOLEAN;
         condition_key TEXT;
       BEGIN
         FOREACH dimension IN ARRAY ARRAY[
           'sample_fraction', 'reporting_basis', 'result_speciation', 'result_target', 'endpoint'
         ] LOOP
           IF COALESCE((to_jsonb(NEW)->>dimension)::BOOLEAN, FALSE)
              AND NOT COALESCE((to_jsonb(OLD)->>dimension)::BOOLEAN, FALSE) THEN
             column_name := CASE WHEN dimension = 'endpoint' THEN 'endpoint_mode'
                                 ELSE dimension || '_id' END;
             EXECUTE format(
               'SELECT EXISTS (
                  SELECT 1 FROM discrete.import_parameter_mappings m
                  WHERE m.parameter_id = $1 AND m.active AND m.%I IS NULL
                )', column_name
             ) INTO missing_mapping USING NEW.parameter_id;
             IF missing_mapping THEN
               RAISE EXCEPTION USING ERRCODE = '23514',
                 MESSAGE = 'Cannot require ' || dimension || ' while an active import mapping omits it.';
             END IF;
           END IF;
         END LOOP;

         FOREACH condition_key IN ARRAY NEW.analysis_conditions_required_keys LOOP
           IF condition_key IS NULL OR btrim(condition_key) = '' THEN
             RAISE EXCEPTION USING ERRCODE = '23514',
               MESSAGE = 'Required analysis condition keys cannot be empty.';
           END IF;
           IF NOT condition_key = ANY(OLD.analysis_conditions_required_keys) AND EXISTS (
             SELECT 1 FROM discrete.import_parameter_mappings m
             WHERE m.parameter_id = NEW.parameter_id AND m.active
               AND (NOT m.analysis_conditions ? condition_key
                    OR m.analysis_conditions->condition_key = 'null'::JSONB)
           ) THEN
             RAISE EXCEPTION USING ERRCODE = '23514',
               MESSAGE = 'Cannot require analysis condition ' || condition_key || ' while an active mapping omits it.';
           END IF;
         END LOOP;

         IF NEW.analysis_conditions_required_keys_by_matrix
              IS DISTINCT FROM OLD.analysis_conditions_required_keys_by_matrix AND EXISTS (
           SELECT 1 FROM discrete.import_parameter_mappings m
           LEFT JOIN public.matrix_states ms ON ms.matrix_state_id = COALESCE(
             m.matrix_state_id, public.get_unique_parameter_matrix_state_id(NEW.parameter_id)
           )
           WHERE m.parameter_id = NEW.parameter_id AND m.active AND (
             (ms.matrix_state_id IS NULL AND NEW.analysis_conditions_required_keys_by_matrix <> '{}'::JSONB)
             OR EXISTS (
               SELECT 1 FROM jsonb_array_elements_text(COALESCE(
                 NEW.analysis_conditions_required_keys_by_matrix->ms.matrix_state_code, '[]'::JSONB
               )) AS needed(key)
               WHERE NOT m.analysis_conditions ? key OR m.analysis_conditions->key = 'null'::JSONB
             )
           )
         ) THEN
           RAISE EXCEPTION USING ERRCODE = '23514',
             MESSAGE = 'Active import mappings must satisfy analysis conditions for their matrix state.';
         END IF;
         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '73' WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(utils::packageVersion("AquaCache")))
    )
  }
  last_patch == "72"
})

if (changed) {
  message(
    "Patch 73 applied: parameter unit and chemistry checks use direct mappings."
  )
} else {
  message("Patch 73 is already applied.")
}
