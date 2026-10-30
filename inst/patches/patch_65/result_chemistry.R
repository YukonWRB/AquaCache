#' Split the Patch 65 staging descriptor catalogue into analytical dimensions
#'
#' @description Internal, transactional Patch 65 migration. The existing patch
#' first preserves legacy parameter/fraction meanings in a staging descriptor
#' catalogue. This migration then separates numeric reporting expressions,
#' chemical forms, and analytical targets without changing result values or
#' import mapping identities. Unknown labels stop the patch for review.
#' @param con AquaCache connection with an active patch transaction.
#' @return Invisibly, the descriptor-to-dimension crosswalk.
#' @keywords internal
migrate_result_chemistry_dimensions <- function(con) {
  if (!isTRUE(dbTransCheck(con))) {
    stop("The chemistry migration requires an active patch transaction.")
  }
  descriptors <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT result_descriptor_id, result_descriptor
     FROM discrete.result_descriptors ORDER BY result_descriptor_id"))
  descriptors[, `:=`(
    reporting_basis = NA_character_, result_speciation = NA_character_,
    result_target = NA_character_, basis_kind = NA_character_,
    target_kind = NA_character_
  )]

  # This is an explicit vocabulary crosswalk, not a chemical parser. An 'as'
  # prefix alone does not establish a mass basis (for example 'as total metals').
  mass_expressions <- c("B", "Ca", "Cl", "CN", "CO3", "DPA", "F", "H",
    "H2S", "HCO3", "LAS", "Mg", "N", "Na", "NaCL", "NH3", "NH4",
    "NO2", "NO3", "O2", "OH", "P", "PO4", "Ra", "Ra226", "S", "Se",
    "Si", "SiO2", "SO3", "SO4", "U3O8", "As", "arsenic")
  targets <- c("of CH4" = "Methane", "of CO2" = "Carbon dioxide",
    "of H2O" = "Water", "of Inorganic C" = "Inorganic carbon",
    "of NH4" = "Ammonium", "of NO3" = "Nitrate",
    "of Organic C" = "Organic carbon", "of Organic N" = "Organic nitrogen",
    "of PO4" = "Phosphate", "of SO4" = "Sulfate", "of S" = "Sulfur",
    "as CO3 & OH" = "Carbonate and hydroxide",
    "as dissolved Ca & Mg" = "Dissolved calcium and magnesium",
    "as dissolved Ca, Mg, Fe, Sr, & Mn" = "Dissolved calcium, magnesium, iron, strontium and manganese",
    "as dissolved metals" = "Dissolved metals",
    "as total Ca & Mg" = "Calcium and magnesium, whole sample",
    "as total Ca, Mg, Fe, Sr, & Mn" = "Calcium, magnesium, iron, strontium and manganese, whole sample",
    "as total metals" = "Metals, whole sample")
  # These identify distinct cyanide pools. Preparation/recovery fractions such
  # as acid soluble, extractable, and total recoverable stay in sample_fractions.
  operational_targets <- c("free", "weak acid dissociable", "strong acid dissociable")
  arsenic_forms <- c("as arsenic (all states)" = "Arsenic, all species",
    "as arsenate (As-V)" = "Arsenic(V)", "as arsenite (As-III)" = "Arsenic(III)",
    "as arsenate" = "Arsenic(V)", "as arsenite" = "Arsenic(III)",
    "inorganic" = "Inorganic arsenic")
  for (i in seq_len(nrow(descriptors))) {
    # The staging fraction migration may have composed an operational target
    # and a reporting expression. They become independent foreign keys here.
    terms <- trimws(strsplit(descriptors$result_descriptor[[i]], " / ", fixed = TRUE)[[1]])
    for (term in terms) {
      if (term %in% paste("as", mass_expressions)) {
        value <- if (term == "as arsenic") "as As" else term
        if (!is.na(descriptors$reporting_basis[[i]])) stop("Multiple reporting bases: ", term)
        descriptors[i, `:=`(reporting_basis = value, basis_kind = "chemical_entity")]
      } else if (term == "as CaCO3") {
        if (!is.na(descriptors$reporting_basis[[i]])) stop("Multiple reporting bases: ", term)
        descriptors[i, `:=`(reporting_basis = term, basis_kind = "chemical_entity")]
      } else if (term %in% c("as NTU", "as FNU")) {
        descriptors[i, `:=`(reporting_basis = term, basis_kind = "instrument_response")]
      } else if (term %in% names(arsenic_forms)) {
        descriptors[i, `:=`(result_speciation = unname(arsenic_forms[[term]]),
          reporting_basis = "as As", basis_kind = "chemical_entity")]
      } else if (term %in% names(targets)) {
        if (!is.na(descriptors$result_target[[i]])) stop("Multiple analytical targets: ", term)
        descriptors[i, `:=`(result_target = unname(targets[[term]]), target_kind = "material_pool")]
      } else if (term %in% operational_targets) {
        if (!is.na(descriptors$result_target[[i]])) stop("Multiple analytical targets: ", term)
        descriptors[i, `:=`(result_target = term, target_kind = "operational_pool")]
      } else {
        stop("Unclassified legacy result descriptor; review before migrating: ", term)
      }
    }
  }

  catalogues <- list(
    reporting_bases = c(id = "reporting_basis_id", label = "reporting_basis", kind = "basis_kind"),
    result_speciations = c(id = "result_speciation_id", label = "result_speciation"),
    result_targets = c(id = "result_target_id", label = "result_target", kind = "target_kind")
  )
  catalogue_grants <- DBI::dbGetQuery(con,
    "SELECT grantee, privilege_type, is_grantable FROM information_schema.role_table_grants
     WHERE table_schema = 'discrete' AND table_name = 'result_descriptors'
       AND grantee NOT IN ('PUBLIC', 'admin', 'postgres')")
  for (table in names(catalogues)) {
    fields <- catalogues[[table]]
    id <- fields[["id"]]
    label <- fields[["label"]]
    kind_sql <- if ("kind" %in% names(fields)) sprintf(
      ", %s TEXT NOT NULL CHECK (btrim(%s) <> '')", fields[["kind"]], fields[["kind"]]) else ""
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE discrete.%s (
         %s INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
         %s_code TEXT NOT NULL UNIQUE CHECK (%s_code ~ '^[a-z][a-z0-9_]*$'),
         %s TEXT NOT NULL UNIQUE CHECK (btrim(%s) <> ''),
         %s_fr TEXT, description TEXT NOT NULL CHECK (btrim(description) <> ''),
         description_fr TEXT, active BOOLEAN NOT NULL DEFAULT TRUE,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER, modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ%s
       )", table, id, label, label, label, label, label, kind_sql))
    DBI::dbExecute(con, sprintf("ALTER TABLE discrete.%s OWNER TO admin", table))
    DBI::dbExecute(con, sprintf("REVOKE ALL ON discrete.%s FROM PUBLIC", table))
    DBI::dbExecute(con, sprintf("GRANT SELECT ON discrete.%s TO public_reader", table))
    for (j in seq_len(nrow(catalogue_grants))) {
      grantee <- as.character(DBI::dbQuoteIdentifier(con, catalogue_grants$grantee[[j]]))
      privilege <- catalogue_grants$privilege_type[[j]]
      DBI::dbExecute(con, sprintf("GRANT %s ON discrete.%s TO %s%s", privilege, table,
        grantee, if (catalogue_grants$is_grantable[[j]] == "YES") " WITH GRANT OPTION" else ""))
      if (privilege == "INSERT") {
        sequence <- DBI::dbGetQuery(con, "SELECT pg_get_serial_sequence($1, $2) AS name",
          params = list(paste0("discrete.", table), id))$name[[1]]
        DBI::dbExecute(con, sprintf("GRANT USAGE, SELECT ON SEQUENCE %s TO %s", sequence, grantee))
      }
    }
    for (trigger in c("trg_user_audit", "trg_modified_audit", "audit_chemistry_catalogue")) {
      event <- if (trigger == "audit_chemistry_catalogue") "AFTER INSERT OR UPDATE OR DELETE" else "BEFORE UPDATE"
      function_name <- switch(trigger, trg_user_audit = "public.user_modified()",
        trg_modified_audit = "public.update_modified()", audit_chemistry_catalogue = "audit.if_modified_func()")
      DBI::dbExecute(con, sprintf("CREATE TRIGGER %s %s ON discrete.%s FOR EACH ROW EXECUTE FUNCTION %s",
        trigger, event, table, function_name))
    }
    value_columns <- c(label, if ("kind" %in% names(fields)) fields[["kind"]])
    values <- unique(descriptors[!is.na(get(label)), value_columns, with = FALSE])
    for (i in seq_len(nrow(values))) {
      value <- values[[label]][[i]]
      code <- tolower(gsub("[^A-Za-z0-9]+", "_", value))
      code <- sub("_$", "", code)
      kind <- if ("kind" %in% names(fields)) values[[fields[["kind"]]]][[i]] else NULL
      description <- switch(table,
        reporting_bases = paste0("Numeric reporting expression ", value,
          ". The physical unit supplies the quantity type. Whether this is a substance concentration or an equivalent expression depends on the measurand. Conversion requires that meaning; no universal conversion is implied."),
        result_speciations = paste0("Chemical form or defined chemical-form scope: ", value, "."),
        result_targets = paste0("Analyzed material or operationally defined pool: ", value,
          ". Consult the recorded analytical method for preparation and recovery."))
      sql <- sprintf("INSERT INTO discrete.%s (%s_code, %s, description%s)
                      VALUES ($1, $2, $3%s) RETURNING %s", table, label, label,
        if (is.null(kind)) "" else paste0(", ", fields[["kind"]]),
        if (is.null(kind)) "" else ", $4", id)
      new_id <- DBI::dbGetQuery(con, sql, params = c(list(code, value, description),
        if (!is.null(kind)) list(kind)))[[id]][[1]]
      descriptors[get(label) == value, (id) := as.integer(new_id)]
    }
    if (!id %in% names(descriptors)) descriptors[, (id) := NA_integer_]
  }
  # New total-cyanide mappings need an explicit analytical pool independently
  # of their total/dissolved/prepared sample fraction. Never infer this target
  # from an unfiltered fraction; only explicit provider analyte labels qualify.
  DBI::dbExecute(con,
    "INSERT INTO discrete.result_targets
       (result_target_code, result_target, target_kind, description)
     VALUES ('total_cyanide', 'total cyanide', 'operational_pool',
       'Cyanide recovered by a total-cyanide determination. The recorded method defines recovery; sample fraction records preparation independently.')")
  # Leave a reviewed migration crosswalk inside this transaction only. Import
  # source payloads and mapping-set identities remain historical evidence.
  # Do not use dbWriteTable/dbAppendTable here: RPostgres may open and commit
  # its own transaction when the enclosing patch used SQL BEGIN. Explicit
  # INSERTs remain inside the patch's transaction and cannot commit it.
  DBI::dbExecute(con,
    "CREATE TEMP TABLE patch65_chemistry_crosswalk (
       result_descriptor_id INTEGER PRIMARY KEY, reporting_basis_id INTEGER,
       result_speciation_id INTEGER, result_target_id INTEGER) ON COMMIT DROP")
  for (i in seq_len(nrow(descriptors))) {
    DBI::dbExecute(con,
      "INSERT INTO patch65_chemistry_crosswalk VALUES ($1, $2, $3, $4)",
      params = unname(as.list(descriptors[i, .(result_descriptor_id, reporting_basis_id,
        result_speciation_id, result_target_id)])))
  }
  # Raw import/audit payloads may retain pre-migration descriptor IDs. Keep an
  # immutable interpretation dictionary so those snapshots remain explainable
  # after the generic lookup is removed. This is migration history, not a fourth
  # chemistry dimension or a selectable catalogue for new results.
  DBI::dbExecute(con,
    "CREATE TABLE information.patch65_result_descriptor_crosswalk (
       legacy_catalogue TEXT NOT NULL CHECK (legacy_catalogue IN
         ('result_speciations', 'sample_fractions', 'patch65_descriptors')),
       legacy_id INTEGER NOT NULL, legacy_label TEXT NOT NULL,
       sample_fraction_id INTEGER REFERENCES discrete.sample_fractions ON DELETE RESTRICT,
       reporting_basis_id INTEGER REFERENCES discrete.reporting_bases ON DELETE RESTRICT,
       result_speciation_id INTEGER REFERENCES discrete.result_speciations ON DELETE RESTRICT,
       result_target_id INTEGER REFERENCES discrete.result_targets ON DELETE RESTRICT,
       PRIMARY KEY (legacy_catalogue, legacy_id))")
  DBI::dbExecute(con,
    "INSERT INTO information.patch65_result_descriptor_crosswalk
     SELECT CASE WHEN old.legacy_id IS NULL THEN 'patch65_descriptors'
         ELSE 'result_speciations' END, x.result_descriptor_id, d.result_descriptor, NULL::INTEGER,
       x.reporting_basis_id, x.result_speciation_id, x.result_target_id
     FROM patch65_chemistry_crosswalk x JOIN discrete.result_descriptors d USING (result_descriptor_id)
     LEFT JOIN patch65_legacy_result_speciations old
       ON old.legacy_id = x.result_descriptor_id")
  DBI::dbExecute(con,
    "INSERT INTO information.patch65_result_descriptor_crosswalk
     SELECT 'sample_fractions', old.sample_fraction_id, old.sample_fraction,
       retained.sample_fraction_id, NULL::INTEGER,
       CASE WHEN lower(btrim(old.sample_fraction)) = 'inorganic' THEN form.result_speciation_id END,
       CASE WHEN lower(btrim(old.sample_fraction)) IN
         ('free', 'weak acid dissociable', 'strong acid dissociable') THEN target.result_target_id END
     FROM patch65_legacy_sample_fractions old
     LEFT JOIN discrete.sample_fractions retained ON lower(btrim(retained.sample_fraction)) =
       CASE WHEN lower(btrim(old.sample_fraction)) = 'unfiltered' THEN 'total'
         ELSE lower(btrim(old.sample_fraction)) END
     LEFT JOIN discrete.result_speciations form ON form.result_speciation_code = 'inorganic_arsenic'
     LEFT JOIN discrete.result_targets target ON lower(btrim(target.result_target)) = lower(btrim(old.sample_fraction))")
  DBI::dbExecute(con,
    "ALTER TABLE information.patch65_result_descriptor_crosswalk OWNER TO admin;
     REVOKE ALL ON information.patch65_result_descriptor_crosswalk FROM PUBLIC;
     GRANT SELECT ON information.patch65_result_descriptor_crosswalk TO public_reader;
     COMMENT ON TABLE information.patch65_result_descriptor_crosswalk IS
       'Immutable Patch 65 ID and label dictionary for the original result_speciations and sample_fractions catalogues, plus descriptors created during staging. legacy_catalogue disambiguates overlapping IDs. Preserves old raw import and audit payload interpretation. Unknown physical fractions are not inferred from cyanide pools.';
     CREATE FUNCTION information.guard_chemistry_migration_history() RETURNS trigger
     LANGUAGE plpgsql SET search_path = pg_catalog AS $function$
     BEGIN RAISE EXCEPTION 'Chemistry migration interpretation records are immutable.'; END;
     $function$;
     ALTER FUNCTION information.guard_chemistry_migration_history() OWNER TO admin;
     REVOKE ALL ON FUNCTION information.guard_chemistry_migration_history() FROM PUBLIC;
     CREATE TRIGGER immutable_migration_history BEFORE UPDATE OR DELETE OR TRUNCATE
       ON information.patch65_result_descriptor_crosswalk FOR EACH STATEMENT
       EXECUTE FUNCTION information.guard_chemistry_migration_history()", immediate = TRUE)
  references <- DBI::dbGetQuery(con,
    "SELECT table_schema, table_name FROM information_schema.columns
     WHERE column_name = 'result_descriptor_id'
       AND table_schema NOT IN ('pg_catalog', 'information_schema')
       AND (table_schema, table_name) IN (
         SELECT table_schema, table_name FROM information_schema.tables
         WHERE table_type = 'BASE TABLE')
       AND NOT (table_schema = 'discrete' AND table_name = 'result_descriptors')")
  supported <- c("discrete.results", "discrete.import_parameter_mappings",
    "criteria.guidelines", "criteria.guideline_rule_inputs", "public.parameter_descriptor_suggestions")
  unknown <- setdiff(paste(references$table_schema, references$table_name, sep = "."), supported)
  if (length(unknown)) stop("Unreviewed descriptor references: ", paste(unknown, collapse = ", "))

  # Capture dependent view definitions and ownership before any column changes.
  views <- DBI::dbGetQuery(con,
    "SELECT c.oid, n.nspname AS schema_name, c.relname AS view_name,
            pg_get_viewdef(c.oid, TRUE) AS definition, pg_get_userbyid(c.relowner) AS owner,
            obj_description(c.oid, 'pg_class') AS comment, array_to_json(c.reloptions)::TEXT AS options
     FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE c.relkind = 'v' AND n.nspname = 'discrete'
       AND c.relname IN ('results_metadata_en', 'results_metadata_fr',
                         'result_aggregation_summary', 'stale_result_aggregations')")
  views <- views[match(intersect(c("stale_result_aggregations", "result_aggregation_summary",
    "results_metadata_en", "results_metadata_fr"), views$view_name), views$view_name), ]
  grants <- DBI::dbGetQuery(con,
    "SELECT table_schema, table_name, grantee, privilege_type, is_grantable
     FROM information_schema.table_privileges WHERE table_schema = 'discrete'
       AND table_name IN ('results_metadata_en', 'results_metadata_fr',
                         'result_aggregation_summary', 'stale_result_aggregations')")
  column_grants <- DBI::dbGetQuery(con,
    "SELECT table_name, column_name, grantee, privilege_type, is_grantable
     FROM information_schema.column_privileges WHERE table_schema = 'discrete'
       AND table_name IN ('results_metadata_en', 'results_metadata_fr',
                         'result_aggregation_summary', 'stale_result_aggregations')")
  for (i in seq_len(nrow(views))) DBI::dbExecute(con, sprintf("DROP VIEW %s.%s",
    DBI::dbQuoteIdentifier(con, views$schema_name[[i]]), DBI::dbQuoteIdentifier(con, views$view_name[[i]])))

  # Requiredness belongs to public.parameters. The old requirement becomes a
  # reporting-basis requirement except for the explicitly reviewed isotope
  # parameters below, where it describes analyzed material. Advisory suggestion
  # rows and the presence/absence of historical results never establish policy.
  DBI::dbExecute(con,
    "ALTER TABLE public.parameters ADD COLUMN reporting_basis BOOLEAN NOT NULL DEFAULT FALSE,
       ADD COLUMN result_speciation BOOLEAN NOT NULL DEFAULT FALSE,
       ADD COLUMN result_target BOOLEAN NOT NULL DEFAULT FALSE,
       ADD COLUMN endpoint BOOLEAN NOT NULL DEFAULT FALSE,
       ADD COLUMN analysis_conditions_required_keys TEXT[] NOT NULL DEFAULT '{}'::TEXT[],
       ADD COLUMN analysis_conditions_required_keys_by_matrix JSONB NOT NULL DEFAULT '{}'::JSONB")
  DBI::dbExecute(con,
    "CREATE FUNCTION discrete.valid_condition_requirements(value JSONB) RETURNS BOOLEAN
     LANGUAGE plpgsql IMMUTABLE SET search_path = pg_catalog AS $function$
     DECLARE entry RECORD;
     BEGIN
       IF jsonb_typeof(value) <> 'object' THEN RETURN FALSE; END IF;
       FOR entry IN SELECT * FROM jsonb_each(value) LOOP
         IF entry.key NOT IN ('liquid', 'solid', 'gas', 'not_applicable') OR jsonb_typeof(entry.value) <> 'array' THEN RETURN FALSE; END IF;
         IF EXISTS (SELECT 1 FROM jsonb_array_elements(entry.value) AS item
           WHERE jsonb_typeof(item) <> 'string' OR btrim(item #>> '{}') = '') THEN RETURN FALSE; END IF;
       END LOOP;
       RETURN TRUE;
     END;
     $function$;
     ALTER FUNCTION discrete.valid_condition_requirements(JSONB) OWNER TO admin;
     ALTER TABLE public.parameters ADD CONSTRAINT parameters_conditions_by_matrix
       CHECK (discrete.valid_condition_requirements(analysis_conditions_required_keys_by_matrix));
     CREATE FUNCTION discrete.valid_condition_key_array(value TEXT[]) RETURNS BOOLEAN
     LANGUAGE sql IMMUTABLE SET search_path = pg_catalog AS $function$
       SELECT NOT EXISTS (SELECT 1 FROM unnest(value) AS k(key) WHERE key IS NULL OR btrim(key) = '')
     $function$;
     ALTER FUNCTION discrete.valid_condition_key_array(TEXT[]) OWNER TO admin;
     ALTER TABLE public.parameters ADD CONSTRAINT parameters_condition_keys
       CHECK (discrete.valid_condition_key_array(analysis_conditions_required_keys));
     CREATE FUNCTION discrete.required_analysis_condition_keys(in_parameter_id INTEGER, in_matrix_state_id INTEGER)
     RETURNS TEXT[] LANGUAGE sql STABLE SET search_path = pg_catalog, public AS $function$
       SELECT ARRAY(SELECT DISTINCT key FROM (
         SELECT unnest(p.analysis_conditions_required_keys) AS key
         UNION ALL SELECT jsonb_array_elements_text(COALESCE(
           p.analysis_conditions_required_keys_by_matrix->ms.matrix_state_code, '[]'::JSONB))
       ) required ORDER BY key)
       FROM public.parameters p LEFT JOIN public.matrix_states ms ON ms.matrix_state_id =
         COALESCE(in_matrix_state_id, public.get_unique_parameter_matrix_state_id(p.parameter_id))
       WHERE p.parameter_id = in_parameter_id;
     $function$;
     ALTER FUNCTION discrete.required_analysis_condition_keys(INTEGER, INTEGER) OWNER TO admin", immediate = TRUE)
  # Normalize numeric spelling (1 and 1.0) before generating a compact indexed
  # identity. Conditions remain stored as JSONB, independently of provenance.
  DBI::dbExecute(con,
    "CREATE FUNCTION discrete.normalize_analysis_conditions(value JSONB)
     RETURNS JSONB LANGUAGE plpgsql IMMUTABLE SET search_path = pg_catalog, discrete
     AS $function$
     BEGIN
       CASE jsonb_typeof(value)
         WHEN 'object' THEN RETURN COALESCE((SELECT jsonb_object_agg(key,
           discrete.normalize_analysis_conditions(item)) FROM jsonb_each(value) AS t(key,item)), '{}'::jsonb);
         WHEN 'array' THEN RETURN COALESCE((SELECT jsonb_agg(discrete.normalize_analysis_conditions(item)
           ORDER BY ordinal) FROM jsonb_array_elements(value) WITH ORDINALITY AS t(item,ordinal)), '[]'::jsonb);
         WHEN 'number' THEN RETURN to_jsonb(trim_scale(value::text::numeric));
         ELSE RETURN value;
       END CASE;
     END;
     $function$")
  DBI::dbExecute(con,
    "CREATE FUNCTION discrete.analysis_conditions_key(value JSONB) RETURNS TEXT
     LANGUAGE sql IMMUTABLE SET search_path = pg_catalog, discrete
     AS $function$ SELECT encode(sha256(convert_to(discrete.normalize_analysis_conditions(value)::TEXT, 'UTF8')), 'hex') $function$")
  DBI::dbExecute(con,
    "CREATE FUNCTION discrete.valid_analysis_conditions(value JSONB) RETURNS BOOLEAN
     LANGUAGE sql IMMUTABLE SET search_path = pg_catalog
     AS $function$
       SELECT COALESCE(jsonb_typeof(value) = 'object'
         AND NOT EXISTS (SELECT 1 FROM jsonb_object_keys(value) AS k(key) WHERE btrim(key) = '')
         AND (NOT value ? 'mass_basis' OR value->>'mass_basis' IN ('dry', 'wet', 'ash', 'other'))
         AND (NOT value ? 'gas_basis' OR value->>'gas_basis' IN ('dry', 'wet', 'other'))
         AND NOT EXISTS (
           SELECT 1 FROM unnest(ARRAY['reference_temperature_kelvin', 'reference_pressure_kpa']) AS k(key)
           WHERE value ? key AND CASE WHEN jsonb_typeof(value->key) = 'number'
             THEN (value->>key)::numeric <= 0 ELSE TRUE END), FALSE)
     $function$")
  for (signature in c("normalize_analysis_conditions(JSONB)", "analysis_conditions_key(JSONB)",
                      "valid_analysis_conditions(JSONB)")) {
    DBI::dbExecute(con, paste0("ALTER FUNCTION discrete.", signature, " OWNER TO admin"))
  }
  DBI::dbExecute(con,
    "ALTER TABLE discrete.import_parameter_mappings DISABLE TRIGGER guard_import_mapping_row")
  # Disable only the affected semantic guards; ordinary audit and RLS remain.
  triggers <- DBI::dbGetQuery(con,
    "SELECT n.nspname AS schema_name, c.relname AS table_name, t.tgname
     FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace
     JOIN pg_proc p ON p.oid = t.tgfoid
     WHERE NOT t.tgisinternal AND (p.prosrc ILIKE '%result_descriptor%')")
  for (i in seq_len(nrow(triggers))) DBI::dbExecute(con, sprintf("ALTER TABLE %s.%s DISABLE TRIGGER %s",
    DBI::dbQuoteIdentifier(con, triggers$schema_name[[i]]),
    DBI::dbQuoteIdentifier(con, triggers$table_name[[i]]), DBI::dbQuoteIdentifier(con, triggers$tgname[[i]])))

  for (i in seq_len(nrow(references))) {
    schema <- references$table_schema[[i]]
    table <- references$table_name[[i]]
    if (table == "parameter_descriptor_suggestions") next
    relation <- as.character(DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table)))
    DBI::dbExecute(con, sprintf(
      "ALTER TABLE %s ADD COLUMN reporting_basis_id INTEGER REFERENCES discrete.reporting_bases ON DELETE RESTRICT,
         ADD COLUMN result_speciation_id INTEGER REFERENCES discrete.result_speciations ON DELETE RESTRICT,
         ADD COLUMN result_target_id INTEGER REFERENCES discrete.result_targets ON DELETE RESTRICT,
         ADD COLUMN endpoint_mode TEXT,
         ADD COLUMN endpoint_ph NUMERIC,
         ADD COLUMN analysis_conditions JSONB NOT NULL DEFAULT '{}'::JSONB,
         ADD COLUMN analysis_conditions_key TEXT GENERATED ALWAYS AS
           (discrete.analysis_conditions_key(analysis_conditions)) STORED,
         ADD CONSTRAINT %s CHECK (discrete.valid_analysis_conditions(analysis_conditions)),
         ADD CONSTRAINT %s CHECK (
           (endpoint_mode IS NULL AND endpoint_ph IS NULL) OR
           (endpoint_mode IS NOT NULL AND endpoint_mode IN ('fixed', 'inflection', 'gran', 'other') AND
             (endpoint_mode <> 'fixed' OR endpoint_ph IS NOT NULL))
         ),
         ADD CONSTRAINT %s CHECK (endpoint_ph IS NULL OR endpoint_ph NOT IN
           ('NaN'::numeric, 'Infinity'::numeric, '-Infinity'::numeric))", relation,
      DBI::dbQuoteIdentifier(con, paste0(table, "_analysis_conditions_object")),
      DBI::dbQuoteIdentifier(con, paste0(table, "_endpoint_definition")),
      DBI::dbQuoteIdentifier(con, paste0(table, "_endpoint_finite"))))
    DBI::dbExecute(con, sprintf(
      "UPDATE %s row SET reporting_basis_id = x.reporting_basis_id,
         result_speciation_id = x.result_speciation_id, result_target_id = x.result_target_id
       FROM patch65_chemistry_crosswalk x WHERE row.result_descriptor_id = x.result_descriptor_id", relation))
  }
  # Repair only a reporting expression explicitly present in the provider
  # mapping. The name acidity/alkalinity alone does not establish a basis or
  # endpoint. Preserve populated dimensions and immutable mapping identities.
  DBI::dbExecute(con,
    "UPDATE discrete.import_parameter_mappings m SET reporting_basis_id = basis.reporting_basis_id
     FROM public.parameters p, discrete.reporting_bases basis
     WHERE m.parameter_id = p.parameter_id AND m.reporting_basis_id IS NULL
       AND basis.reporting_basis_code = 'as_caco3'
       AND lower(btrim(p.param_name)) IN ('acidity', 'alkalinity', 'hardness')
       AND (
         lower(COALESCE(m.source_match->>'input_unit', '')) IN ('mgcaco3/l', 'mg/l as caco3') OR
         lower(COALESCE(m.source_match->>'ParamCode', '')) LIKE '%(ascaco3)%' OR
         lower(COALESCE(m.source_match->>'input_param', '')) ~ '(^|[^a-z0-9])caco3([^a-z0-9]|$)'
       )")
  DBI::dbExecute(con,
    "UPDATE discrete.import_parameter_mappings m SET result_target_id = target.result_target_id
     FROM public.parameters p, discrete.result_targets target
     WHERE m.parameter_id = p.parameter_id AND m.result_target_id IS NULL
       AND lower(btrim(p.param_name)) = 'cyanide' AND target.result_target_code = 'total_cyanide'
       AND (upper(btrim(COALESCE(m.source_match->>'input_param', ''))) = 'CYANIDE TOTAL'
         OR upper(btrim(COALESCE(m.source_match->>'ParamCode', ''))) = 'CN-T')")
  # Profile identity arrays and static parser defaults live outside SQL column
  # dependencies. Translate their legacy IDs through the same explicit map.
  # A provider column carrying a mixed descriptor needs a reviewed parser; do
  # not silently reinterpret that single source column as a reporting basis.
  profiles <- DBI::dbGetQuery(con,
    "SELECT import_profile_id, profile_code, column_map, wide_config, defaults, result_identity
     FROM discrete.import_profiles")
  chemistry_fields <- c("reporting_basis_id", "result_speciation_id", "result_target_id",
    "endpoint_mode", "endpoint_ph", "analysis_conditions")
  fraction_crosswalk <- data.table::as.data.table(DBI::dbGetQuery(con,
    "SELECT legacy_id, legacy_label, sample_fraction_id, result_speciation_id, result_target_id
     FROM information.patch65_result_descriptor_crosswalk WHERE legacy_catalogue='sample_fractions'"))
  translate_defaults <- function(value, profile_code) {
    if (!is.list(value)) return(value)
    if (is.null(names(value))) return(lapply(value, translate_defaults, profile_code = profile_code))
    old_fields <- intersect(names(value), c("result_speciation_id", "result_descriptor_id",
      "result_speciation", "result_descriptor"))
    if (length(old_fields) > 1L) stop("Review conflicting legacy chemistry defaults in profile ", profile_code)
    if (length(old_fields)) {
      old <- value[[old_fields[[1]]]]
      row <- NULL
      if (!is.null(old)) {
        if (length(old) != 1L || is.list(old)) stop("Review non-scalar chemistry defaults in profile ", profile_code)
        id <- suppressWarnings(as.numeric(old))
        missing <- is.na(old) || !nzchar(trimws(as.character(old))) || (!is.na(id) && id == 0)
        if (!missing) {
          row <- if (!is.na(id) && is.finite(id) && id == trunc(id))
            descriptors[result_descriptor_id == id] else
            descriptors[tolower(trimws(result_descriptor)) == tolower(trimws(as.character(old)))]
          if (nrow(row) != 1L) stop("Review unknown legacy chemistry default in profile ", profile_code)
        }
      }
      value[old_fields] <- NULL
      for (field in chemistry_fields[1:3]) {
        replacement <- if (is.null(row) || is.na(row[[field]][[1]])) NULL else row[[field]][[1]]
        if (field %in% names(value) && !identical(value[[field]], replacement)) {
          stop("Review conflicting typed chemistry default in profile ", profile_code)
        }
        value[field] <- list(replacement)
      }
    }
    fraction_fields <- intersect(names(value), c("sample_fraction_id", "sample_fraction"))
    if (length(fraction_fields) > 1L) stop("Review conflicting sample-fraction defaults in profile ", profile_code)
    if (length(fraction_fields)) {
      old <- value[[fraction_fields[[1]]]]
      if (!is.null(old) && length(old) == 1L && !is.list(old) && !is.na(old) && nzchar(trimws(as.character(old)))) {
        id <- suppressWarnings(as.numeric(old))
        row <- if (!is.na(id) && is.finite(id) && id == trunc(id)) fraction_crosswalk[legacy_id == id] else
          fraction_crosswalk[tolower(trimws(legacy_label)) == tolower(trimws(as.character(old)))]
        if (nrow(row) != 1L) stop("Review unknown sample-fraction default in profile ", profile_code)
        value[fraction_fields] <- NULL
        value["sample_fraction_id"] <- list(if (is.na(row$sample_fraction_id[[1]])) NULL else row$sample_fraction_id[[1]])
        for (field in c("result_speciation_id", "result_target_id")) {
          if (!is.na(row[[field]][[1]])) {
            if (!is.null(value[[field]]) && value[[field]] != row[[field]][[1]]) {
              stop("Review conflicting fraction/chemistry defaults in profile ", profile_code)
            }
            value[[field]] <- row[[field]][[1]]
          }
        }
      } else if (!is.null(old) && (length(old) != 1L || is.list(old))) {
        stop("Review non-scalar sample-fraction default in profile ", profile_code)
      }
    }
    lapply(value, translate_defaults, profile_code = profile_code)
  }
  for (i in seq_len(nrow(profiles))) {
    profile_code <- profiles$profile_code[[i]]
    column_map <- jsonlite::fromJSON(profiles$column_map[[i]], simplifyVector = FALSE)
    if (any(names(column_map) %in% c("result_speciation_id", "result_descriptor_id", "result_speciation", "result_descriptor"))) {
      stop("Review the legacy descriptor source column in import profile ", profile_code)
    }
    defaults <- translate_defaults(jsonlite::fromJSON(profiles$defaults[[i]], simplifyVector = FALSE), profile_code)
    wide <- translate_defaults(jsonlite::fromJSON(profiles$wide_config[[i]], simplifyVector = FALSE), profile_code)
    identity <- unlist(jsonlite::fromJSON(profiles$result_identity[[i]], simplifyVector = FALSE), use.names = FALSE)
    identity <- setdiff(identity, c("result_speciation_id", "result_descriptor_id", "result_speciation", "result_descriptor"))
    identity <- unique(c(identity, chemistry_fields))
    DBI::dbExecute(con,
      "UPDATE discrete.import_profiles SET defaults = $1::JSONB, wide_config = $2::JSONB,
         result_identity = $3::JSONB WHERE import_profile_id = $4",
      params = list(if (length(defaults)) jsonlite::toJSON(defaults, auto_unbox = TRUE, null = "null") else "{}",
        if (length(wide)) jsonlite::toJSON(wide, auto_unbox = TRUE, null = "null") else "{}",
        jsonlite::toJSON(identity, auto_unbox = FALSE), profiles$import_profile_id[[i]]))
  }
  DBI::dbExecute(con,
    "UPDATE public.parameters SET reporting_basis = result_descriptor")
  DBI::dbExecute(con,
    "UPDATE public.parameters SET reporting_basis = FALSE, result_target = result_descriptor
     WHERE lower(btrim(param_name)) IN (
       'carbon-13', 'carbon-13/carbon-12 ratio', 'delta carbon-13/carbon-12',
       'nitrogen-15', 'nitrogen-15/nitrogen-14 ratio', 'delta nitrogen-15/nitrogen-14',
       'oxygen-17', 'oxygen-17/oxygen-16 ratio', 'delta oxygen-17/oxygen-16',
       'oxygen-18', 'oxygen-18/oxygen-16 ratio', 'delta oxygen-18/oxygen-16',
       'sulfur-34', 'sulfur-34/sulfur-32 ratio', 'delta sulfur-34/sulfur-32',
       'deuterium, delta', 'deuterium/hydrogen ratio', 'tritium',
       'uranium-234/uranium-238 ratio', 'uranium-238')")
  # Arsenic consolidation establishes an explicit chemical-form scope even
  # on a seed cluster with no arsenic results or source mappings.
  DBI::dbExecute(con,
    "UPDATE public.parameters SET reporting_basis = TRUE, result_speciation = TRUE
     WHERE lower(btrim(param_name)) = 'arsenic'")
  DBI::dbExecute(con,
    "UPDATE public.parameters SET result_target = TRUE
     WHERE lower(btrim(param_name)) = 'cyanide'")
  DBI::dbExecute(con,
    "UPDATE public.parameters SET reporting_basis = TRUE, endpoint = TRUE
     WHERE lower(btrim(param_name)) IN ('acidity', 'alkalinity')")
  orphan_requirements <- DBI::dbGetQuery(con,
    "SELECT param_name FROM public.parameters WHERE result_descriptor
       AND NOT (reporting_basis OR result_speciation OR result_target)")
  if (nrow(orphan_requirements)) stop("Required descriptor has no classified dimension: ",
    paste(orphan_requirements$param_name, collapse = ", "))

  # Three small, typed advisory relations retain the established suggestion
  # contract. Missing suggestions never make an otherwise valid value invalid.
  for (table in names(catalogues)) {
    fields <- catalogues[[table]]
    suggestion <- switch(table, reporting_bases = "parameter_reporting_basis_suggestions",
      result_speciations = "parameter_speciation_suggestions", result_targets = "parameter_target_suggestions")
    id <- fields[["id"]]
    DBI::dbExecute(con, sprintf(
      "CREATE TABLE public.%s (parameter_id INTEGER NOT NULL REFERENCES public.parameters ON DELETE CASCADE,
        %s INTEGER NOT NULL REFERENCES discrete.%s ON DELETE CASCADE,
        display_order INTEGER NOT NULL DEFAULT 100 CHECK (display_order > 0),
        PRIMARY KEY (parameter_id, %s))", suggestion, id, table, id))
    DBI::dbExecute(con, sprintf(
      "INSERT INTO public.%s SELECT s.parameter_id, x.%s, min(s.display_order)
       FROM public.parameter_descriptor_suggestions s
       JOIN patch65_chemistry_crosswalk x USING (result_descriptor_id)
       WHERE x.%s IS NOT NULL GROUP BY s.parameter_id, x.%s", suggestion, id, id, id))
    DBI::dbExecute(con, sprintf("ALTER TABLE public.%s OWNER TO admin", suggestion))
    DBI::dbExecute(con, sprintf("GRANT SELECT ON public.%s TO public_reader", suggestion))
    DBI::dbExecute(con, sprintf("COMMENT ON TABLE public.%s IS
      'Advisory choice ordering only. Missing rows do not prohibit a chemistry dimension.'", suggestion))
  }

  chemistry_sql <- system.file("patches", "patch_65", "chemistry.sql", package = "AquaCache")
  if (!nzchar(chemistry_sql)) stop("The Patch 65 chemistry SQL is missing from AquaCache.")
  DBI::dbExecute(con, paste(readLines(chemistry_sql, warn = FALSE), collapse = "\n"), immediate = TRUE)

  # Rebuild stored bodies without altering published function signatures.
  # Their old fourth 'speciation' argument historically meant reporting basis;
  # the public sample accessor is replaced with a typed signature below.
  functions <- DBI::dbGetQuery(con,
    "SELECT p.oid::regprocedure::text AS signature, p.proname,
            p.prosrc, pg_get_functiondef(p.oid) AS definition
     FROM pg_proc p JOIN pg_namespace n ON n.oid = p.pronamespace
     WHERE p.prokind = 'f' AND n.nspname NOT IN ('pg_catalog', 'information_schema')
       AND p.prosrc ILIKE '%result_descriptor%'")
  for (i in seq_len(nrow(functions))) {
    source <- functions$prosrc[[i]]
    rewritten <- gsub("result_descriptors", "reporting_bases", source, fixed = TRUE)
    rewritten <- gsub("result_descriptor", "reporting_basis", rewritten, fixed = TRUE)
    if (functions$proname[[i]] == "guideline_get_input_value") {
      if (!grepl("r.parameter_id = input_row.parameter_id", rewritten, fixed = TRUE)) stop("Review guideline input matcher before migrating.")
      rewritten <- gsub("r.parameter_id = input_row.parameter_id",
        "r.parameter_id = input_row.parameter_id AND discrete.result_matches_chemistry(r.result_id, to_jsonb(input_row))",
        rewritten, fixed = TRUE)
      hardness_call <- "hardness_value := criteria.get_sample_hardness("
      if (!grepl(hardness_call, rewritten, fixed = TRUE)) stop("Review the hardness input helper before migrating.")
      arguments <- "hardness_value := criteria[.]get_sample_hardness\\(\\s*in_sample_id,\\s*input_row[.]result_type_preference\\s*\\)"
      if (!grepl(arguments, rewritten, perl = TRUE)) stop("Review the hardness helper arguments before migrating.")
      rewritten <- sub(arguments,
        "hardness_value := criteria.get_sample_hardness(in_sample_id, input_row.result_type_preference, input_row.sample_fraction_id)",
        rewritten, perl = TRUE)
      rewritten <- sub(hardness_call,
        "IF (input_row.parameter_id IS NOT NULL AND NOT EXISTS (
            SELECT 1 FROM public.parameters WHERE parameter_id = input_row.parameter_id AND lower(param_name) = 'hardness'))
          OR (input_row.matrix_state_id IS NOT NULL AND NOT EXISTS (
            SELECT 1 FROM public.matrix_states WHERE matrix_state_id = input_row.matrix_state_id AND matrix_state_code = 'liquid'))
          OR (input_row.reporting_basis_id IS NOT NULL AND NOT EXISTS (
            SELECT 1 FROM discrete.reporting_bases WHERE reporting_basis_id = input_row.reporting_basis_id AND reporting_basis_code = 'as_caco3'))
          OR (input_row.sample_fraction_id IS NOT NULL AND NOT EXISTS (
            SELECT 1 FROM discrete.sample_fractions WHERE sample_fraction_id = input_row.sample_fraction_id
              AND lower(btrim(sample_fraction)) IN ('dissolved', 'total')))
          OR input_row.result_speciation_id IS NOT NULL OR input_row.result_target_id IS NOT NULL
          OR input_row.endpoint_mode IS NOT NULL OR input_row.analysis_conditions <> '{}'::JSONB THEN
          RETURN QUERY SELECT input_row.input_id, input_row.input_code, NULL::NUMERIC, NULL::INTEGER,
            'incompatible_hardness_expression'::TEXT,
            'The hardness helper returns liquid-sample mg/L as CaCO3 with no additional chemical scope or reference conditions.'::TEXT;
          RETURN;
        END IF;
        hardness_value := criteria.get_sample_hardness(", rewritten, fixed = TRUE)
    }
    if (functions$proname[[i]] == "applicable_guideline_rules_for_result") {
      if (!grepl("g.parameter_id = rr.parameter_id", rewritten, fixed = TRUE)) stop("Review guideline applicability matcher before migrating.")
      rewritten <- sub("g.parameter_id = rr.parameter_id",
        "g.parameter_id = rr.parameter_id AND discrete.result_matches_chemistry(rr.result_id, to_jsonb(g))",
        rewritten, fixed = TRUE)
    }
    DBI::dbExecute(con, sub(source, rewritten, functions$definition[[i]], fixed = TRUE))
  }

  DBI::dbExecute(con, "DROP FUNCTION discrete.get_sample_val(INTEGER, INTEGER, INTEGER, INTEGER)")
  DBI::dbExecute(con,
    "CREATE FUNCTION discrete.get_sample_val(
       sample_id INTEGER, parameter_id INTEGER, sample_fraction_id INTEGER DEFAULT NULL,
       reporting_basis_id INTEGER DEFAULT NULL, result_speciation_id INTEGER DEFAULT NULL,
       result_target_id INTEGER DEFAULT NULL, endpoint_mode TEXT DEFAULT NULL,
       endpoint_ph NUMERIC DEFAULT NULL, analysis_conditions JSONB DEFAULT '{}'::JSONB,
       matrix_state_id INTEGER DEFAULT NULL)
     RETURNS NUMERIC LANGUAGE sql STABLE SET search_path = pg_catalog, discrete, public
     AS $function$
       SELECT CASE WHEN count(*) = 1 THEN max(r.result) ELSE NULL::NUMERIC END
       FROM discrete.results r WHERE r.sample_id = $1 AND r.parameter_id = $2
         AND r.sample_fraction_id IS NOT DISTINCT FROM $3
         AND r.reporting_basis_id IS NOT DISTINCT FROM $4
         AND r.result_speciation_id IS NOT DISTINCT FROM $5
         AND r.result_target_id IS NOT DISTINCT FROM $6
         AND r.endpoint_mode IS NOT DISTINCT FROM $7
         AND r.endpoint_ph IS NOT DISTINCT FROM $8
         AND r.analysis_conditions = discrete.normalize_analysis_conditions($9)
         AND discrete.result_matches_chemistry(r.result_id, jsonb_build_object(
           'matrix_state_id', $10, 'sample_fraction_id', $3,
           'reporting_basis_id', $4, 'result_speciation_id', $5,
           'result_target_id', $6, 'endpoint_mode', $7, 'endpoint_ph', $8,
           'analysis_conditions', $9))
     $function$")
  DBI::dbExecute(con,
    "ALTER FUNCTION discrete.get_sample_val(INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, TEXT, NUMERIC, JSONB, INTEGER) OWNER TO admin")
  DBI::dbExecute(con,
    "GRANT EXECUTE ON FUNCTION discrete.get_sample_val(INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, TEXT, NUMERIC, JSONB, INTEGER) TO PUBLIC")


  # Stored scalar formulae contain identifiers outside pg_proc and must move
  # through the same crosswalk as declarative guideline inputs. Do not guess
  # dynamic or nested legacy helper arguments: stop for a reviewed conversion.
  rules <- DBI::dbGetQuery(con,
    "SELECT rule_id, formula_sql FROM criteria.guideline_value_rules
     WHERE formula_sql ~* 'discrete[.]get_sample_val[[:space:]]*[(]'")
  for (i in seq_len(nrow(rules))) {
    sql <- rules$formula_sql[[i]]
    matches <- gregexpr("(?i)\\bdiscrete\\.get_sample_val\\s*\\([^()]*\\)", sql, perl = TRUE)
    calls <- regmatches(sql, matches)[[1]]
    if (!length(calls)) stop("Review nested sample accessor in guideline rule ", rules$rule_id[[i]])
    revised <- vapply(calls, function(call) {
      args <- strsplit(sub("(?s)^.*?\\((.*)\\)$", "\\1", call, perl = TRUE), ",", fixed = TRUE)[[1]]
      args <- trimws(args)
      if (length(args) < 2L || length(args) > 4L) stop("Review sample accessor argument count in rule ", rules$rule_id[[i]])
      legacy_names <- c("sample_id", "parameter_id", "sample_fraction_id", "result_speciation_id")
      values <- stats::setNames(c(NA_character_, NA_character_, "NULL::INTEGER", "NULL::INTEGER"), legacy_names)
      assigned <- character()
      named_seen <- FALSE
      for (j in seq_along(args)) {
        if (grepl("^[a-z_]+\\s*(?::=|=>)", args[[j]], perl = TRUE, ignore.case = TRUE)) {
          named_seen <- TRUE
          name <- tolower(sub("^([a-z_]+)\\s*(?::=|=>).*$", "\\1", args[[j]], perl = TRUE, ignore.case = TRUE))
          if (name == "result_descriptor_id") name <- "result_speciation_id"
          value <- sub("^[a-z_]+\\s*(?::=|=>)\\s*", "", args[[j]], perl = TRUE, ignore.case = TRUE)
        } else {
          if (named_seen) stop("Positional argument follows a named argument in rule ", rules$rule_id[[i]])
          name <- legacy_names[[j]]
          value <- args[[j]]
        }
        if (!name %in% legacy_names || name %in% assigned) stop("Review duplicate or unknown accessor argument in rule ", rules$rule_id[[i]])
        values[[name]] <- value
        assigned <- c(assigned, name)
      }
      if (anyNA(values)) stop("Missing sample or parameter accessor argument in rule ", rules$rule_id[[i]])
      args <- unname(values)
      legacy_id <- toupper(sub("::[[:space:]]*(INT|INTEGER)[[:space:]]*$", "", args[[4]], ignore.case = TRUE))
      if (!grepl("^(NULL|[0-9]+)$", legacy_id)) stop("Review dynamic legacy chemistry reference in rule ", rules$rule_id[[i]])
      row <- if (legacy_id == "NULL") NULL else descriptors[result_descriptor_id == as.integer(legacy_id)]
      if (!is.null(row) && nrow(row) != 1L) stop("Unknown legacy chemistry ID in rule ", rules$rule_id[[i]])
      ids <- if (is.null(row)) rep("NULL::INTEGER", 3) else vapply(
        c("reporting_basis_id", "result_speciation_id", "result_target_id"),
        function(field) if (is.na(row[[field]][[1]])) "NULL::INTEGER" else paste0(row[[field]][[1]], "::INTEGER"), character(1))
      legacy_fraction <- toupper(sub("::[[:space:]]*(INT|INTEGER)[[:space:]]*$", "", args[[3]], ignore.case = TRUE))
      if (!grepl("^(NULL|[0-9]+)$", legacy_fraction)) stop("Review dynamic legacy sample fraction in rule ", rules$rule_id[[i]])
      if (legacy_fraction != "NULL") {
        fraction <- fraction_crosswalk[legacy_id == as.integer(legacy_fraction)]
        if (nrow(fraction) != 1L) stop("Unknown legacy sample-fraction ID in rule ", rules$rule_id[[i]])
        args[[3]] <- if (is.na(fraction$sample_fraction_id[[1]])) "NULL::INTEGER" else
          paste0(fraction$sample_fraction_id[[1]], "::INTEGER")
        for (j in 2:3) {
          field <- c("result_speciation_id", "result_target_id")[[j - 1L]]
          if (!is.na(fraction[[field]][[1]])) {
            replacement <- paste0(fraction[[field]][[1]], "::INTEGER")
            if (ids[[j]] != "NULL::INTEGER" && ids[[j]] != replacement) {
              stop("Conflicting legacy fraction/chemistry in rule ", rules$rule_id[[i]])
            }
            ids[[j]] <- replacement
          }
        }
      }
      sprintf("discrete.get_sample_val(sample_id := %s, parameter_id := %s, sample_fraction_id := %s,
        reporting_basis_id := %s, result_speciation_id := %s, result_target_id := %s)",
        args[[1]], args[[2]], args[[3]], ids[[1]], ids[[2]], ids[[3]])
    }, character(1))
    regmatches(sql, matches) <- list(revised)
    DBI::dbExecute(con, "UPDATE criteria.guideline_value_rules SET formula_sql = $1 WHERE rule_id = $2",
      params = list(sql, rules$rule_id[[i]]))
  }

  # New result identity preserves each analytical dimension and endpoint.
  # PostgreSQL refuses any collision rather than retaining just one row.
  key <- DBI::dbGetQuery(con,
    "SELECT conname FROM pg_constraint WHERE conrelid = 'discrete.results'::regclass
       AND contype = 'u' AND pg_get_constraintdef(oid) LIKE '%result_descriptor_id%'")
  for (constraint in key$conname) DBI::dbExecute(con, sprintf(
    "ALTER TABLE discrete.results DROP CONSTRAINT %s", DBI::dbQuoteIdentifier(con, constraint)))
  DBI::dbExecute(con,
    "ALTER TABLE discrete.results ADD CONSTRAINT results_analytical_identity
     UNIQUE NULLS NOT DISTINCT (sample_id, result_type, parameter_id, matrix_state_id,
       sample_fraction_id, result_value_type, reporting_basis_id, result_speciation_id,
       result_target_id, endpoint_mode, endpoint_ph, analysis_conditions_key,
       protocol_method, laboratory, analysis_datetime)")

  for (i in rev(seq_len(nrow(views)))) {
    view <- views[i, ]
    definition <- gsub("result_descriptors", "reporting_bases", view$definition, fixed = TRUE)
    definition <- gsub("result_descriptor", "reporting_basis", definition, fixed = TRUE)
    if (view$view_name == "results_metadata_fr") {
      definition <- sub('rs[.]reporting_basis AS "[^"]+"',
        'COALESCE(rs.reporting_basis_fr, rs.reporting_basis) AS base_expression_resultat', definition)
    }
    if (view$view_name != "stale_result_aggregations") {
      definition <- sprintf(
        "SELECT legacy.*, r.result_speciation_id, spec.result_speciation,
           r.result_target_id, target.result_target, r.endpoint_mode, r.endpoint_ph,
           r.analysis_conditions, r.analysis_conditions_key
         FROM (%s) legacy JOIN discrete.results r USING (result_id)
         LEFT JOIN discrete.result_speciations spec USING (result_speciation_id)
         LEFT JOIN discrete.result_targets target USING (result_target_id)", sub(";[[:space:]]*$", "", definition))
    }
    view_id <- as.character(DBI::dbQuoteIdentifier(con,
      DBI::Id(schema = view$schema_name[[1]], table = view$view_name[[1]])))
    options <- if (is.na(view$options[[1]])) "" else paste0(" WITH (",
      paste(jsonlite::fromJSON(view$options[[1]]), collapse = ", "), ")")
    DBI::dbExecute(con, paste0("CREATE VIEW ", view_id, options, " AS ", definition))
    DBI::dbExecute(con, paste0("ALTER VIEW ", view_id, " OWNER TO ", DBI::dbQuoteIdentifier(con, view$owner[[1]])))
    if (!is.na(view$comment[[1]])) DBI::dbExecute(con, paste0("COMMENT ON VIEW ", view_id, " IS ", DBI::dbQuoteString(con, view$comment[[1]])))
    for (j in which(grants$table_name == view$view_name[[1]])) {
      grantee <- if (grants$grantee[[j]] == "PUBLIC") "PUBLIC" else
        as.character(DBI::dbQuoteIdentifier(con, grants$grantee[[j]]))
      DBI::dbExecute(con, sprintf("GRANT %s ON %s TO %s%s", grants$privilege_type[[j]],
        view_id, grantee, if (grants$is_grantable[[j]] == "YES") " WITH GRANT OPTION" else ""))
    }
    for (j in which(column_grants$table_name == view$view_name[[1]])) {
      covered <- grants$table_name == column_grants$table_name[[j]] &
        grants$grantee == column_grants$grantee[[j]] &
        grants$privilege_type == column_grants$privilege_type[[j]] &
        (grants$is_grantable == "YES" | column_grants$is_grantable[[j]] == "NO")
      if (any(covered)) next
      column <- gsub("result_descriptor", "reporting_basis", column_grants$column_name[[j]], fixed = TRUE)
      if (view$view_name == "results_metadata_fr" &&
          grepl("(?i)sp.*ciation.*r.*sultat|descripteur.*r.*sultat", column, perl = TRUE)) column <- "base_expression_resultat"
      grantee <- if (column_grants$grantee[[j]] == "PUBLIC") "PUBLIC" else
        as.character(DBI::dbQuoteIdentifier(con, column_grants$grantee[[j]]))
      DBI::dbExecute(con, sprintf("GRANT %s (%s) ON %s TO %s%s", column_grants$privilege_type[[j]],
        DBI::dbQuoteIdentifier(con, column), view_id, grantee,
        if (column_grants$is_grantable[[j]] == "YES") " WITH GRANT OPTION" else ""))
    }
  }
  for (i in seq_len(nrow(references))) {
    if (references$table_name[[i]] == "parameter_descriptor_suggestions") next
    relation <- as.character(DBI::dbQuoteIdentifier(con,
      DBI::Id(schema = references$table_schema[[i]], table = references$table_name[[i]])))
    DBI::dbExecute(con, paste0("ALTER TABLE ", relation, " DROP COLUMN result_descriptor_id"))
  }
  DBI::dbExecute(con, "DROP TABLE public.parameter_descriptor_suggestions")
  DBI::dbExecute(con, "ALTER TABLE public.parameters DROP COLUMN result_descriptor")
  DBI::dbExecute(con, "DROP TABLE discrete.result_descriptors")
  for (i in seq_len(nrow(triggers))) DBI::dbExecute(con, sprintf("ALTER TABLE %s.%s ENABLE TRIGGER %s",
    DBI::dbQuoteIdentifier(con, triggers$schema_name[[i]]), DBI::dbQuoteIdentifier(con, triggers$table_name[[i]]),
    DBI::dbQuoteIdentifier(con, triggers$tgname[[i]])))
  DBI::dbExecute(con, "ALTER TABLE discrete.import_parameter_mappings ENABLE TRIGGER guard_import_mapping_row")
  DBI::dbExecute(con, "DROP TABLE patch65_chemistry_crosswalk")
  invisible(descriptors)
}
