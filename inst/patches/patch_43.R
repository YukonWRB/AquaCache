# Patch 43: discrete metadata views and lean continuous measurement access
#
# Adds English and French user-facing discrete sample/result views. Also removes
# created/modified row timestamps from corrected continuous measurement access
# and narrows audit.measurements_continuous_as_of() directly, without retaining
# compatibility helper functions.

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 43: adding discrete metadata views and lean continuous measurement access. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

message("Starting transaction...")
active <- dbTransBegin(con)

tryCatch(
  {
    missing_relations <- DBI::dbGetQuery(
      con,
      "WITH required(relation_name) AS (
         VALUES
           ('audit.measurements_continuous_log'),
           ('continuous.measurements_continuous'),
           ('continuous.timeseries'),
           ('discrete.samples'),
           ('discrete.results'),
           ('discrete.collection_methods'),
           ('discrete.laboratories'),
           ('discrete.protocols_methods'),
           ('discrete.result_conditions'),
           ('discrete.result_speciations'),
           ('discrete.result_types'),
           ('discrete.result_value_types'),
           ('discrete.sample_fractions'),
           ('discrete.sample_types'),
           ('public.approval_types'),
           ('public.datum_conversions'),
           ('public.grade_types'),
           ('public.locations'),
           ('public.locations_networks'),
           ('public.locations_projects'),
           ('public.matrix_states'),
           ('public.media_types'),
           ('public.networks'),
           ('public.organizations'),
           ('public.parameters'),
           ('public.projects'),
           ('public.qualifier_types'),
           ('public.sub_locations')
       )
       SELECT relation_name
       FROM required
       WHERE to_regclass(relation_name) IS NULL
       ORDER BY relation_name"
    )

    if (nrow(missing_relations) > 0) {
      stop(
        "This patch requires the following relations to already exist: ",
        paste(missing_relations$relation_name, collapse = ", ")
      )
    }

    function_check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure('public.get_parameter_unit_name(integer, integer)') IS NOT NULL AS has_get_parameter_unit_name,
         to_regprocedure('continuous.apply_corrections(integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections,
         to_regprocedure('continuous.apply_corrections_at(timestamp with time zone, integer, timestamp with time zone, numeric)') IS NOT NULL AS has_apply_corrections_at,
         to_regprocedure('continuous.resolve_compound_timeseries_raw_window(integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_resolve_compound,
         to_regprocedure('continuous.resolve_compound_timeseries_raw_window_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, integer[])') IS NOT NULL AS has_resolve_compound_at"
    )

    if (
      !isTRUE(function_check$has_get_parameter_unit_name[[1]]) ||
        !isTRUE(function_check$has_apply_corrections[[1]]) ||
        !isTRUE(function_check$has_apply_corrections_at[[1]]) ||
        !isTRUE(function_check$has_resolve_compound[[1]]) ||
        !isTRUE(function_check$has_resolve_compound_at[[1]])
    ) {
      stop(
        "This patch requires parameter-unit, correction, and compound-resolution functions from earlier patches to already exist."
      )
    }

    existing_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname

    write_roles <- intersect(
      c("admin", "tkc_editor", "yg_editor_group", "cyfn_editor_group"),
      existing_roles
    )

    view_select_roles <- intersect(
      c(
        "admin",
        "public_reader",
        "yg_reader_group",
        "yg_reader",
        "yg_editor_group",
        "yg_editor",
        "cyfn_editor_group",
        "cyfn_editor",
        "tkc_group",
        "tkc_editor"
      ),
      existing_roles
    )

    historical_function_roles <- intersect(
      c("yg_editor_group", "yg_editor", "yg_reader_group", "yg_reader"),
      existing_roles
    )

    q_ident <- function(x) {
      as.character(DBI::dbQuoteIdentifier(con, x))
    }

    grant_table_privileges <- function(object_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON TABLE %s TO %s",
            privileges,
            object_name,
            q_ident(role_name)
          )
        )
      }
    }

    grant_function_privileges <- function(function_name, privileges, roles) {
      if (length(roles) == 0) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON FUNCTION %s TO %s",
            privileges,
            function_name,
            q_ident(role_name)
          )
        )
      }
    }

    message("Creating discrete.samples_metadata_en...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.samples_metadata_en
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT
         s.sample_id,
         s.location_id,
         loc.location_code,
         loc.name AS location_name,
         loc.alias AS alias_name,
         loc.latitude,
         loc.longitude,
         dc.conversion_m AS location_elevation,
         loc_projects.projects,
         loc_networks.networks,
         s.sub_location_id,
         sub_loc.sub_location_name,
         sub_loc.latitude AS sub_location_latitude,
         sub_loc.longitude AS sub_location_longitude,
         s.media_id,
         mt.media_type,
         s.z AS depth_height_m,
         s.datetime,
         s.target_datetime,
         s.collection_method AS collection_method_id,
         cm.collection_method,
         s.sample_type AS sample_type_id,
         st.sample_type,
         s.linked_with AS linked_sample_id,
         s.sample_volume_ml,
         s.purge_volume_l,
         s.purge_time_min,
         s.flow_rate_l_min,
         s.wave_hgt_m,
         s.sample_grade AS sample_grade_id,
         gt.grade_type_code AS sample_grade_code,
         gt.grade_type_description AS sample_grade_description,
         s.sample_approval AS sample_approval_id,
         at.approval_type_code AS sample_approval_code,
         at.approval_type_description AS sample_approval_description,
         s.sample_qualifier AS sample_qualifier_id,
         qt.qualifier_type_code AS sample_qualifier_code,
         qt.qualifier_type_description AS sample_qualifier_description,
         s.owner AS owner_id,
         owner_org.name AS owner_name,
         s.contributor AS contributor_id,
         contributor_org.name AS contributor_name,
         s.comissioning_org AS commissioning_org_id,
         commissioning_org.name AS commissioning_org_name,
         s.sampling_org AS sampling_org_id,
         sampling_org.name AS sampling_org_name,
         s.field_visit_id,
         s.data_sharing_agreement_id,
         s.documents,
         s.import_source,
         s.import_source_id,
         s.no_update,
         s.note,
         s.share_with,
         s.private_expiry,
         s.created,
         s.created_by,
         s.modified,
         s.modified_by
       FROM discrete.samples s
       JOIN public.locations loc
         ON s.location_id = loc.location_id
       LEFT JOIN public.sub_locations sub_loc
         ON s.sub_location_id = sub_loc.sub_location_id
       LEFT JOIN public.media_types mt
         ON s.media_id = mt.media_id
       LEFT JOIN discrete.collection_methods cm
         ON s.collection_method = cm.collection_method_id
       LEFT JOIN discrete.sample_types st
         ON s.sample_type = st.sample_type_id
       LEFT JOIN public.grade_types gt
         ON s.sample_grade = gt.grade_type_id
       LEFT JOIN public.approval_types at
         ON s.sample_approval = at.approval_type_id
       LEFT JOIN public.qualifier_types qt
         ON s.sample_qualifier = qt.qualifier_type_id
       LEFT JOIN public.organizations owner_org
         ON s.owner = owner_org.organization_id
       LEFT JOIN public.organizations contributor_org
         ON s.contributor = contributor_org.organization_id
       LEFT JOIN public.organizations commissioning_org
         ON s.comissioning_org = commissioning_org.organization_id
       LEFT JOIN public.organizations sampling_org
         ON s.sampling_org = sampling_org.organization_id
       LEFT JOIN LATERAL (
         SELECT dc2.conversion_m
         FROM public.datum_conversions dc2
         WHERE dc2.location_id = s.location_id
           AND dc2.current IS TRUE
         ORDER BY dc2.conversion_id
         LIMIT 1
       ) dc
         ON TRUE
       LEFT JOIN LATERAL (
         SELECT array_agg(DISTINCT proj.name ORDER BY proj.name) AS projects
         FROM public.locations_projects loc_proj
         JOIN public.projects proj
           ON loc_proj.project_id = proj.project_id
         WHERE loc_proj.location_id = s.location_id
       ) loc_projects
         ON TRUE
       LEFT JOIN LATERAL (
         SELECT array_agg(DISTINCT net.name ORDER BY net.name) AS networks
         FROM public.locations_networks loc_net
         JOIN public.networks net
           ON loc_net.network_id = net.network_id
         WHERE loc_net.location_id = s.location_id
       ) loc_networks
         ON TRUE"
    )

    message("Creating discrete.samples_metadata_fr...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.samples_metadata_fr
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT
         s.sample_id,
         s.location_id,
         loc.location_code,
         COALESCE(loc.name_fr, loc.name) AS nom_endroit,
         loc.alias AS nom_alias,
         loc.latitude,
         loc.longitude,
         dc.conversion_m AS \"élévation_endroit\",
         loc_projects.projets,
         loc_networks.\"réseaux\",
         s.sub_location_id,
         COALESCE(
           sub_loc.sub_location_name_fr,
           sub_loc.sub_location_name
         ) AS nom_sous_endroit,
         sub_loc.latitude AS latitude_sous_endroit,
         sub_loc.longitude AS longitude_sous_endroit,
         s.media_id,
         COALESCE(mt.media_type_fr, mt.media_type) AS \"type_de_média\",
         s.z AS profondeur_hauteur_m,
         s.datetime,
         s.target_datetime AS datetime_cible,
         s.collection_method AS collection_method_id,
         cm.collection_method AS \"méthode_collecte\",
         s.sample_type AS sample_type_id,
         COALESCE(st.sample_type_fr, st.sample_type) AS \"type_échantillon\",
         s.linked_with AS linked_sample_id,
         s.sample_volume_ml AS \"volume_échantillon_ml\",
         s.purge_volume_l AS volume_purge_l,
         s.purge_time_min AS \"durée_purge_min\",
         s.flow_rate_l_min AS \"débit_l_min\",
         s.wave_hgt_m AS hauteur_vague_m,
         s.sample_grade AS sample_grade_id,
         gt.grade_type_code AS \"code_grade_échantillon\",
         gt.grade_type_description_fr AS \"description_grade_échantillon\",
         s.sample_approval AS sample_approval_id,
         at.approval_type_code AS \"code_approbation_échantillon\",
         at.approval_type_description_fr AS \"description_approbation_échantillon\",
         s.sample_qualifier AS sample_qualifier_id,
         qt.qualifier_type_code AS \"code_qualificatif_échantillon\",
         qt.qualifier_type_description_fr AS \"description_qualificatif_échantillon\",
         s.owner AS owner_id,
         COALESCE(owner_org.name_fr, owner_org.name) AS \"propriétaire\",
         s.contributor AS contributor_id,
         COALESCE(contributor_org.name_fr, contributor_org.name) AS contributeur,
         s.comissioning_org AS commissioning_org_id,
         COALESCE(
           commissioning_org.name_fr,
           commissioning_org.name
         ) AS organisme_mise_service,
         s.sampling_org AS sampling_org_id,
         COALESCE(sampling_org.name_fr, sampling_org.name) AS \"organisme_échantillonnage\",
         s.field_visit_id,
         s.data_sharing_agreement_id,
         s.documents,
         s.import_source,
         s.import_source_id,
         s.no_update,
         s.note,
         s.share_with,
         s.private_expiry,
         s.created,
         s.created_by,
         s.modified,
         s.modified_by
       FROM discrete.samples s
       JOIN public.locations loc
         ON s.location_id = loc.location_id
       LEFT JOIN public.sub_locations sub_loc
         ON s.sub_location_id = sub_loc.sub_location_id
       LEFT JOIN public.media_types mt
         ON s.media_id = mt.media_id
       LEFT JOIN discrete.collection_methods cm
         ON s.collection_method = cm.collection_method_id
       LEFT JOIN discrete.sample_types st
         ON s.sample_type = st.sample_type_id
       LEFT JOIN public.grade_types gt
         ON s.sample_grade = gt.grade_type_id
       LEFT JOIN public.approval_types at
         ON s.sample_approval = at.approval_type_id
       LEFT JOIN public.qualifier_types qt
         ON s.sample_qualifier = qt.qualifier_type_id
       LEFT JOIN public.organizations owner_org
         ON s.owner = owner_org.organization_id
       LEFT JOIN public.organizations contributor_org
         ON s.contributor = contributor_org.organization_id
       LEFT JOIN public.organizations commissioning_org
         ON s.comissioning_org = commissioning_org.organization_id
       LEFT JOIN public.organizations sampling_org
         ON s.sampling_org = sampling_org.organization_id
       LEFT JOIN LATERAL (
         SELECT dc2.conversion_m
         FROM public.datum_conversions dc2
         WHERE dc2.location_id = s.location_id
           AND dc2.current IS TRUE
         ORDER BY dc2.conversion_id
         LIMIT 1
       ) dc
         ON TRUE
       LEFT JOIN LATERAL (
         SELECT array_agg(
           DISTINCT COALESCE(proj.name_fr, proj.name)
           ORDER BY COALESCE(proj.name_fr, proj.name)
         ) AS projets
         FROM public.locations_projects loc_proj
         JOIN public.projects proj
           ON loc_proj.project_id = proj.project_id
         WHERE loc_proj.location_id = s.location_id
       ) loc_projects
         ON TRUE
       LEFT JOIN LATERAL (
         SELECT array_agg(
           DISTINCT COALESCE(net.name_fr, net.name)
           ORDER BY COALESCE(net.name_fr, net.name)
         ) AS \"réseaux\"
         FROM public.locations_networks loc_net
         JOIN public.networks net
           ON loc_net.network_id = net.network_id
         WHERE loc_net.location_id = s.location_id
       ) loc_networks
         ON TRUE"
    )

    message("Creating discrete.results_metadata_en...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.results_metadata_en
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT
         r.result_id,
         r.sample_id,
         sm.location_id,
         sm.location_code,
         sm.location_name,
         sm.alias_name,
         sm.latitude,
         sm.longitude,
         sm.location_elevation,
         sm.projects,
         sm.networks,
         sm.sub_location_id,
         sm.sub_location_name,
         sm.sub_location_latitude,
         sm.sub_location_longitude,
         sm.media_id,
         sm.media_type,
         sm.depth_height_m,
         sm.datetime,
         sm.target_datetime,
         sm.collection_method_id,
         sm.collection_method,
         sm.sample_type_id,
         sm.sample_type,
         sm.sample_grade_id,
         sm.sample_grade_code,
         sm.sample_grade_description,
         sm.sample_approval_id,
         sm.sample_approval_code,
         sm.sample_approval_description,
         sm.sample_qualifier_id,
         sm.sample_qualifier_code,
         sm.sample_qualifier_description,
         sm.owner_id AS sample_owner_id,
         sm.owner_name AS sample_owner_name,
         sm.contributor_id AS sample_contributor_id,
         sm.contributor_name AS sample_contributor_name,
         sm.import_source AS sample_import_source,
         sm.import_source_id AS sample_import_source_id,
         sm.note AS sample_note,
         r.parameter_id,
         p.param_name AS parameter_name,
         p.cas_number,
         r.matrix_state_id,
         ms.matrix_state_code,
         ms.matrix_state_name,
         public.get_parameter_unit_name(
           r.parameter_id,
           r.matrix_state_id
         ) AS units,
         r.sample_fraction_id,
         sf.sample_fraction,
         r.result_type AS result_type_id,
         rt.result_type,
         r.result,
         r.result_condition AS result_condition_id,
         rc.result_condition,
         r.result_condition_value,
         r.result_value_type AS result_value_type_id,
         rvt.result_value_type,
         r.result_speciation_id,
         rs.result_speciation,
         r.protocol_method AS protocol_method_id,
         pm.protocol_name AS protocol_method,
         pm.protocol_description,
         pm.url AS protocol_url,
         r.laboratory AS lab_id,
         lab.lab_name,
         r.analysis_datetime,
         sm.no_update AS sample_no_update,
         r.no_update AS result_no_update,
         sm.share_with AS sample_share_with,
         r.share_with AS result_share_with,
         sm.private_expiry AS sample_private_expiry,
         r.private_expiry AS result_private_expiry,
         r.created,
         r.created_by,
         r.modified,
         r.modified_by
       FROM discrete.results r
       JOIN discrete.samples_metadata_en sm
         ON r.sample_id = sm.sample_id
       LEFT JOIN public.parameters p
         ON r.parameter_id = p.parameter_id
       LEFT JOIN public.matrix_states ms
         ON r.matrix_state_id = ms.matrix_state_id
       LEFT JOIN discrete.sample_fractions sf
         ON r.sample_fraction_id = sf.sample_fraction_id
       LEFT JOIN discrete.result_types rt
         ON r.result_type = rt.result_type_id
       LEFT JOIN discrete.result_conditions rc
         ON r.result_condition = rc.result_condition_id
       LEFT JOIN discrete.result_value_types rvt
         ON r.result_value_type = rvt.result_value_type_id
       LEFT JOIN discrete.result_speciations rs
         ON r.result_speciation_id = rs.result_speciation_id
       LEFT JOIN discrete.protocols_methods pm
         ON r.protocol_method = pm.protocol_id
       LEFT JOIN discrete.laboratories lab
         ON r.laboratory = lab.lab_id"
    )

    message("Creating discrete.results_metadata_fr...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW discrete.results_metadata_fr
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT
         r.result_id,
         r.sample_id,
         sm.location_id,
         sm.location_code,
         sm.nom_endroit,
         sm.nom_alias,
         sm.latitude,
         sm.longitude,
         sm.\"élévation_endroit\",
         sm.projets,
         sm.\"réseaux\",
         sm.sub_location_id,
         sm.nom_sous_endroit,
         sm.latitude_sous_endroit,
         sm.longitude_sous_endroit,
         sm.media_id,
         sm.\"type_de_média\",
         sm.profondeur_hauteur_m,
         sm.datetime,
         sm.datetime_cible,
         sm.collection_method_id,
         sm.\"méthode_collecte\",
         sm.sample_type_id,
         sm.\"type_échantillon\",
         sm.sample_grade_id,
         sm.\"code_grade_échantillon\",
         sm.\"description_grade_échantillon\",
         sm.sample_approval_id,
         sm.\"code_approbation_échantillon\",
         sm.\"description_approbation_échantillon\",
         sm.sample_qualifier_id,
         sm.\"code_qualificatif_échantillon\",
         sm.\"description_qualificatif_échantillon\",
         sm.owner_id AS sample_owner_id,
         sm.\"propriétaire\" AS \"propriétaire_échantillon\",
         sm.contributor_id AS sample_contributor_id,
         sm.contributeur AS \"contributeur_échantillon\",
         sm.import_source AS source_importation_échantillon,
         sm.import_source_id AS id_source_importation_échantillon,
         sm.note AS note_échantillon,
         r.parameter_id,
         COALESCE(p.param_name_fr, p.param_name) AS \"nom_paramètre\",
         p.cas_number,
         r.matrix_state_id,
         ms.matrix_state_code,
         COALESCE(
           ms.matrix_state_name_fr,
           ms.matrix_state_name
         ) AS \"état_matrice\",
         public.get_parameter_unit_name(
           r.parameter_id,
           r.matrix_state_id
         ) AS \"unités\",
         r.sample_fraction_id,
         sf.sample_fraction AS \"fraction_échantillon\",
         r.result_type AS result_type_id,
         rt.result_type AS \"type_résultat\",
         r.result AS \"résultat\",
         r.result_condition AS result_condition_id,
         rc.result_condition AS \"condition_résultat\",
         r.result_condition_value AS \"valeur_condition_résultat\",
         r.result_value_type AS result_value_type_id,
         rvt.result_value_type AS \"type_valeur_résultat\",
         r.result_speciation_id,
         rs.result_speciation AS \"spéciation_résultat\",
         r.protocol_method AS protocol_method_id,
         pm.protocol_name AS \"méthode_protocole\",
         pm.protocol_description AS \"description_protocole\",
         pm.url AS url_protocole,
         r.laboratory AS lab_id,
         lab.lab_name AS nom_laboratoire,
         r.analysis_datetime AS datetime_analyse,
         sm.no_update AS no_update_échantillon,
         r.no_update AS no_update_résultat,
         sm.share_with AS share_with_échantillon,
         r.share_with AS share_with_résultat,
         sm.private_expiry AS private_expiry_échantillon,
         r.private_expiry AS private_expiry_résultat,
         r.created,
         r.created_by,
         r.modified,
         r.modified_by
       FROM discrete.results r
       JOIN discrete.samples_metadata_fr sm
         ON r.sample_id = sm.sample_id
       LEFT JOIN public.parameters p
         ON r.parameter_id = p.parameter_id
       LEFT JOIN public.matrix_states ms
         ON r.matrix_state_id = ms.matrix_state_id
       LEFT JOIN discrete.sample_fractions sf
         ON r.sample_fraction_id = sf.sample_fraction_id
       LEFT JOIN discrete.result_types rt
         ON r.result_type = rt.result_type_id
       LEFT JOIN discrete.result_conditions rc
         ON r.result_condition = rc.result_condition_id
       LEFT JOIN discrete.result_value_types rvt
         ON r.result_value_type = rvt.result_value_type_id
       LEFT JOIN discrete.result_speciations rs
         ON r.result_speciation_id = rs.result_speciation_id
       LEFT JOIN discrete.protocols_methods pm
         ON r.protocol_method = pm.protocol_id
       LEFT JOIN discrete.laboratories lab
         ON r.laboratory = lab.lab_id"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_en IS
       'English-language view that flattens key discrete sample metadata into a reader-friendly dataset with location, media, method, type, quality, and organization names.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.samples_metadata_fr IS
       'French-language view that flattens key discrete sample metadata into a reader-friendly dataset with location, media, method, type, quality, and organization names.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_metadata_en IS
       'English-language view that exposes discrete analytical results with sample context and reader-friendly parameter, unit, fraction, speciation, condition, method, and laboratory names.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW discrete.results_metadata_fr IS
       'French-language view that exposes discrete analytical results with sample context and reader-friendly parameter, unit, fraction, speciation, condition, method, and laboratory names.'"
    )

    view_names <- c(
      "discrete.samples_metadata_en",
      "discrete.samples_metadata_fr",
      "discrete.results_metadata_en",
      "discrete.results_metadata_fr"
    )
    for (view_name in view_names) {
      grant_table_privileges(view_name, "SELECT", view_select_roles)
    }

    message("Narrowing continuous point-in-time measurement history function...")
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS audit.measurements_continuous_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.measurements_continuous_as_of(
         p_as_of TIMESTAMPTZ,
         p_timeseries_ids INTEGER[] DEFAULT NULL,
         p_start_datetime TIMESTAMPTZ DEFAULT NULL,
         p_end_datetime TIMESTAMPTZ DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       BEGIN
         RETURN QUERY EXECUTE
         $sql$
           WITH candidate_row_ids AS (
             SELECT mc.measurement_row_id
             FROM continuous.measurements_continuous mc
             WHERE ($1 IS NULL OR mc.timeseries_id = ANY($1))
               AND ($2 IS NULL OR mc.datetime >= $2)
               AND ($3 IS NULL OR mc.datetime <= $3)
             UNION
             SELECT log.measurement_row_id
             FROM audit.measurements_continuous_log log
             WHERE log.measurement_row_id IS NOT NULL
               AND log.action_timestamp > $4
               AND (
                 $1 IS NULL OR
                   (log.original_data ->> 'timeseries_id')::integer = ANY($1)
               )
               AND (
                 $2 IS NULL OR
                   (log.original_data ->> 'datetime')::timestamptz >= $2
               )
               AND (
                 $3 IS NULL OR
                   (log.original_data ->> 'datetime')::timestamptz <= $3
               )
           ),
           current_rows AS (
             SELECT
               mc.measurement_row_id,
               mc.timeseries_id,
               mc.datetime,
               mc.value,
               mc.period,
               mc.imputed,
               mc.created AS row_created
             FROM continuous.measurements_continuous mc
             JOIN candidate_row_ids ids
               ON ids.measurement_row_id = mc.measurement_row_id
           ),
           future_changes AS (
             SELECT DISTINCT ON (log.measurement_row_id)
               log.measurement_row_id,
               (log.original_data ->> 'timeseries_id')::integer AS timeseries_id,
               (log.original_data ->> 'datetime')::timestamptz AS datetime,
               (log.original_data ->> 'value')::numeric AS value,
               (log.original_data ->> 'period')::interval AS period,
               (log.original_data ->> 'imputed')::boolean AS imputed,
               NULLIF(log.original_data ->> 'created', '')::timestamptz AS row_created
             FROM audit.measurements_continuous_log log
             JOIN candidate_row_ids ids
               ON ids.measurement_row_id = log.measurement_row_id
             WHERE log.measurement_row_id IS NOT NULL
               AND log.action_timestamp > $4
             ORDER BY
               log.measurement_row_id,
               log.action_timestamp ASC,
               log.log_id ASC
           ),
           snapshot_rows AS (
             SELECT
               COALESCE(f.measurement_row_id, c.measurement_row_id) AS measurement_row_id,
               CASE
                 WHEN f.measurement_row_id IS NOT NULL THEN f.timeseries_id
                 ELSE c.timeseries_id
               END AS timeseries_id,
               CASE
                 WHEN f.measurement_row_id IS NOT NULL THEN f.datetime
                 ELSE c.datetime
               END AS datetime,
               CASE
                 WHEN f.measurement_row_id IS NOT NULL THEN f.value
                 ELSE c.value
               END AS value,
               CASE
                 WHEN f.measurement_row_id IS NOT NULL THEN f.period
                 ELSE c.period
               END AS period,
               CASE
                 WHEN f.measurement_row_id IS NOT NULL THEN f.imputed
                 ELSE c.imputed
               END AS imputed,
               CASE
                 WHEN f.measurement_row_id IS NOT NULL THEN f.row_created
                 ELSE c.row_created
               END AS row_created
             FROM current_rows c
             FULL OUTER JOIN future_changes f
               ON c.measurement_row_id = f.measurement_row_id
           )
           SELECT
             s.timeseries_id,
             s.datetime,
             s.value,
             s.period,
             s.imputed
           FROM snapshot_rows s
           WHERE s.row_created <= $4
             AND ($1 IS NULL OR s.timeseries_id = ANY($1))
             AND ($2 IS NULL OR s.datetime >= $2)
             AND ($3 IS NULL OR s.datetime <= $3)
           ORDER BY s.timeseries_id, s.datetime
         $sql$
         USING p_timeseries_ids, p_start_datetime, p_end_datetime, p_as_of;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.measurements_continuous_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ
       ) IS
       'Point-in-time access function for continuous measurement values. It reconstructs measurement rows from current data plus audit.measurements_continuous_log without returning row-created or row-modified metadata.';"
    )

    message("Recreating corrected continuous measurement functions...")
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_internal_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         p_path INTEGER[] DEFAULT ARRAY[]::INTEGER[]
       )
       RETURNS TABLE (
         datetime TIMESTAMP WITH TIME ZONE,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           SELECT
             mc.datetime,
             continuous.apply_corrections_at(
               p_as_of,
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed
           FROM audit.measurements_continuous_as_of(
             p_as_of,
             ARRAY[p_timeseries_id],
             p_from,
             p_to
           ) mc;

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           src.datetime,
           continuous.apply_corrections_at(
             p_as_of,
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed
         FROM continuous.resolve_compound_timeseries_raw_window_at(
           p_as_of,
           p_timeseries_id,
           p_from,
           p_to,
           p_path
         ) src;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS continuous.measurements_continuous_corrected(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected(
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       SECURITY INVOKER
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           SELECT
             mc.timeseries_id,
             mc.datetime,
             mc.value AS value_raw,
             continuous.apply_corrections(
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed
           FROM continuous.measurements_continuous mc
           WHERE mc.timeseries_id = p_timeseries_id
             AND (p_from IS NULL OR mc.datetime >= p_from)
             AND (p_to IS NULL OR mc.datetime <= p_to);

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           p_timeseries_id,
           src.datetime,
           src.value_raw,
           continuous.apply_corrections(
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed
         FROM continuous.resolve_compound_timeseries_raw_window(
           p_timeseries_id,
           p_from,
           p_to,
           ARRAY[]::INTEGER[]
         ) src;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS continuous.measurements_continuous_corrected_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       )"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.measurements_continuous_corrected_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         v_type TEXT;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_type = 'basic' THEN
           RETURN QUERY
           SELECT
             mc.timeseries_id,
             mc.datetime,
             mc.value AS value_raw,
             continuous.apply_corrections_at(
               p_as_of,
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed
           FROM audit.measurements_continuous_as_of(
             p_as_of,
             ARRAY[p_timeseries_id],
             p_from,
             p_to
           ) mc;

           RETURN;
         END IF;

         RETURN QUERY
         SELECT
           p_timeseries_id,
           src.datetime,
           src.value_raw,
           continuous.apply_corrections_at(
             p_as_of,
             p_timeseries_id,
             src.datetime,
             src.value_raw
           ) AS value_corrected,
           src.period,
           src.imputed
         FROM continuous.resolve_compound_timeseries_raw_window_at(
           p_as_of,
           p_timeseries_id,
           p_from,
           p_to,
           ARRAY[]::INTEGER[]
         ) src;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS audit.measurements_continuous_values_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ
       )"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) IS
       'Returns raw and corrected values for one basic or compound timeseries over a requested datetime window. Row-created and row-modified timestamps are intentionally omitted for lower tuple width.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) IS
       'Point-in-time corrected-measurement access function for one basic or compound timeseries over a requested datetime window. Row-created and row-modified timestamps are intentionally omitted for lower tuple width.';"
    )

    DBI::dbExecute(
      con,
      "GRANT EXECUTE ON FUNCTION continuous.measurements_continuous_corrected(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) TO PUBLIC"
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected(INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE)",
      "EXECUTE",
      write_roles
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION audit.measurements_continuous_as_of(
         TIMESTAMPTZ,
         INTEGER[],
         TIMESTAMPTZ,
         TIMESTAMPTZ
       ) FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.measurements_continuous_corrected_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE
       ) FROM PUBLIC"
    )

    grant_function_privileges(
      "audit.measurements_continuous_as_of(TIMESTAMPTZ, INTEGER[], TIMESTAMPTZ, TIMESTAMPTZ)",
      "EXECUTE",
      historical_function_roles
    )
    grant_function_privileges(
      "continuous.measurements_continuous_corrected_at(TIMESTAMPTZ, INTEGER, TIMESTAMP WITH TIME ZONE, TIMESTAMP WITH TIME ZONE)",
      "EXECUTE",
      historical_function_roles
    )

    message("Checking patched views and function signatures...")
    result_check <- DBI::dbGetQuery(
      con,
      "SELECT
         pg_get_function_result(
           'audit.measurements_continuous_as_of(timestamp with time zone, integer[], timestamp with time zone, timestamp with time zone)'::regprocedure
         ) ILIKE '%created%' AS as_of_has_created,
         pg_get_function_result(
           'audit.measurements_continuous_as_of(timestamp with time zone, integer[], timestamp with time zone, timestamp with time zone)'::regprocedure
         ) ILIKE '%modified%' AS as_of_has_modified,
         pg_get_function_result(
           'continuous.measurements_continuous_corrected(integer, timestamp with time zone, timestamp with time zone)'::regprocedure
         ) ILIKE '%created%' AS corrected_has_created,
         pg_get_function_result(
           'continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone)'::regprocedure
         ) ILIKE '%modified%' AS corrected_at_has_modified,
         to_regprocedure(
           'audit.measurements_continuous_values_as_of(timestamp with time zone, integer[], timestamp with time zone, timestamp with time zone)'
         ) IS NOT NULL AS has_values_as_of"
    )

    if (
      isTRUE(result_check$as_of_has_created[[1]]) ||
        isTRUE(result_check$as_of_has_modified[[1]]) ||
        isTRUE(result_check$corrected_has_created[[1]]) ||
        isTRUE(result_check$corrected_at_has_modified[[1]]) ||
        isTRUE(result_check$has_values_as_of[[1]])
    ) {
      stop(
        "Patch 43 verification failed: old timestamp fields or compatibility helper functions remain."
      )
    }

    if ("public_reader" %in% existing_roles) {
      DBI::dbExecute(con, "SET LOCAL ROLE public_reader")

      leak_checks <- DBI::dbGetQuery(
        con,
        "SELECT check_name, leak_count
         FROM (
           SELECT
             'discrete.samples_metadata_en' AS check_name,
             COUNT(*)::integer AS leak_count
           FROM discrete.samples_metadata_en v
           LEFT JOIN discrete.samples s
             ON s.sample_id = v.sample_id
           WHERE s.sample_id IS NULL

           UNION ALL

           SELECT
             'discrete.samples_metadata_fr' AS check_name,
             COUNT(*)::integer AS leak_count
           FROM discrete.samples_metadata_fr v
           LEFT JOIN discrete.samples s
             ON s.sample_id = v.sample_id
           WHERE s.sample_id IS NULL

           UNION ALL

           SELECT
             'discrete.results_metadata_en result' AS check_name,
             COUNT(*)::integer AS leak_count
           FROM discrete.results_metadata_en v
           LEFT JOIN discrete.results r
             ON r.result_id = v.result_id
           WHERE r.result_id IS NULL

           UNION ALL

           SELECT
             'discrete.results_metadata_fr result' AS check_name,
             COUNT(*)::integer AS leak_count
           FROM discrete.results_metadata_fr v
           LEFT JOIN discrete.results r
             ON r.result_id = v.result_id
           WHERE r.result_id IS NULL

           UNION ALL

           SELECT
             'discrete.results_metadata_en sample' AS check_name,
             COUNT(*)::integer AS leak_count
           FROM discrete.results_metadata_en v
           LEFT JOIN discrete.samples s
             ON s.sample_id = v.sample_id
           WHERE s.sample_id IS NULL

           UNION ALL

           SELECT
             'discrete.results_metadata_fr sample' AS check_name,
             COUNT(*)::integer AS leak_count
           FROM discrete.results_metadata_fr v
           LEFT JOIN discrete.samples s
             ON s.sample_id = v.sample_id
           WHERE s.sample_id IS NULL
         ) x
         WHERE leak_count > 0"
      )

      DBI::dbExecute(con, "RESET ROLE")

      if (nrow(leak_checks) > 0) {
        stop(
          "Patch 43 visibility check failed: ",
          paste(
            paste0(leak_checks$check_name, "=", leak_checks$leak_count),
            collapse = ", "
          )
        )
      }
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '43'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    DBI::dbExecute(con, "COMMIT;")
    message(
      "Patch 43 applied successfully. Discrete sample/result metadata views are available in English and French, and corrected continuous measurement functions now omit row timestamp metadata."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    stop(
      "Patch 43 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
