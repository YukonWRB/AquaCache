# One-time Patch 66 helpers. No connection, transaction or version management.
# The shared package helper migrates every relational parameter reference,
# coalesces advisory links/group membership, and rejects result-key collisions.

patch66_unit_definition <- function(label, solid = FALSE) {
  key <- tolower(gsub("[[:space:]]", "", label))
  key <- gsub("\u00b5|\u03bc", "u", key)
  liquid <- c("mg/l" = 1, "ug/l" = .001, "ng/l" = .000001)
  mass <- c("mg/kg" = 1, "ug/kg" = .001, "ng/kg" = .000001, "%" = 10000)
  other <- c("cu" = 1, "tcu" = 1, "m" = 1, "mbtoc" = 1)
  scale <- c(if (solid) mass else liquid, other)
  if (length(key) != 1L || is.na(key) || !key %in% names(scale)) stop("Unreviewed unit: ", label)
  list(kind = if (key %in% names(other)) {
    if (key %in% c("cu", "tcu")) "true_colour" else "length"
  } else if (solid) "mass_fraction" else "mass_concentration", scale = unname(scale[[key]]))
}

patch66_parameter_cleanup <- function(con, plan_path) {
  if (!AquaCache:::dbTransCheck(con)) stop("Patch 66 cleanup requires an active transaction.")
  plan <- data.table::fread(plan_path, sep = "\t", quote = "", na.strings = "", encoding = "UTF-8")
  if (anyNA(plan$source) || anyNA(plan$target) || anyDuplicated(tolower(trimws(plan$source)))) {
    stop("Invalid or repeated source in Patch 66 manifest.")
  }
  if (any(!vapply(plan$conditions[!is.na(plan$conditions)], jsonlite::validate, logical(1)))) {
    stop("Invalid analytical-condition JSON in Patch 66 manifest.")
  }
  columns <- DBI::dbGetQuery(con,
    "SELECT table_schema, table_name, column_name FROM information_schema.columns
     WHERE table_schema IN ('public','discrete','criteria')")
  for (table in c("reporting_bases", "result_speciations", "result_targets")) {
    if (!any(columns$table_schema == "discrete" & columns$table_name == table)) {
      stop("Patch 66 requires the final typed Patch 65 schema: ", table)
    }
  }
  unit_fields <- c("units_liquid", "units_solid", "units_gas", "units_na")
  requirements <- c("sample_fraction", "reporting_basis", "result_speciation", "result_target", "endpoint")
  has_mappings <- any(columns$table_schema == "discrete" & columns$table_name == "import_parameter_mappings")
  DBI::dbExecute(con, "LOCK TABLE public.parameters IN SHARE ROW EXCLUSIVE MODE")
  units <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM public.units"))
  matrices <- DBI::dbGetQuery(con, "SELECT * FROM public.matrix_states")
  refs <- DBI::dbGetQuery(con,
    "SELECT DISTINCT n.nspname AS schema_name, c.relname AS table_name,
       a.attname AS column_name, cardinality(f.conkey) AS key_size
     FROM pg_constraint f JOIN pg_class c ON c.oid = f.conrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace
     JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(f.conkey)
     WHERE f.contype = 'f' AND f.confrelid = 'public.parameters'::regclass")
  if (any(refs$key_size != 1L)) stop("Review composite parameter foreign keys before cleanup.")
  # Include standard result/mapping tables even on instances lacking an FK.
  for (table in c("results", if (has_mappings) "import_parameter_mappings")) {
    if (!any(refs$schema_name == "discrete" & refs$table_name == table & refs$column_name == "parameter_id")) {
      refs <- rbind(refs, data.frame(schema_name = "discrete", table_name = table,
        column_name = "parameter_id", key_size = 1L))
    }
  }
  # Prevent concurrent inserts while interpreting a legacy definition. Lock in
  # deterministic order; do not disable audit, identity, unit or FK checks.
  ref_tables <- unique(refs[c("schema_name", "table_name")])
  ref_tables <- ref_tables[order(ref_tables$schema_name, ref_tables$table_name), ]
  for (j in seq_len(nrow(ref_tables))) DBI::dbExecute(con, sprintf(
    "LOCK TABLE %s IN SHARE ROW EXCLUSIVE MODE", DBI::dbQuoteIdentifier(con,
      DBI::Id(schema = ref_tables$schema_name[[j]], table = ref_tables$table_name[[j]]))))
  guard <- if (has_mappings) DBI::dbGetQuery(con,
    "SELECT tgenabled::TEXT AS state FROM pg_trigger
     WHERE tgrelid = 'discrete.import_parameter_mappings'::regclass
       AND tgname = 'guard_import_mapping_row' AND NOT tgisinternal") else NULL
  if (has_mappings && (nrow(guard) != 1L || guard$state[[1]] != "O")) stop("Mapping guard must be enabled.")
  # Relocating an old result must not invent unknown reporting bases/fractions
  # merely to satisfy a new target's insertion policy. Suspend only these two
  # requiredness guards during the reviewed migration. Existing typed fields,
  # numerical values and source payloads are retained; current policy is restored.
  policy <- DBI::dbGetQuery(con,
    "SELECT n.nspname AS schema_name, c.relname AS table_name, t.tgname, t.tgenabled::TEXT AS state
     FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace JOIN pg_proc p ON p.oid = t.tgfoid
     WHERE NOT t.tgisinternal AND n.nspname = 'discrete'
       AND ((c.relname='results' AND p.proname='enforce_result_speciation')
         OR (c.relname='import_parameter_mappings' AND p.proname='enforce_import_parameter_mapping_requirements'))")
  if (nrow(policy) != 1L + as.integer(has_mappings) || any(!policy$state %in% c("O", "A", "R"))) {
    stop("Unexpected Patch 65 chemistry policy triggers.")
  }
  for (j in seq_len(nrow(policy))) DBI::dbExecute(con, sprintf(
    "ALTER TABLE %s DISABLE TRIGGER %s", DBI::dbQuoteIdentifier(con,
      DBI::Id(schema = policy$schema_name[[j]], table = policy$table_name[[j]])),
    DBI::dbQuoteIdentifier(con, policy$tgname[[j]])))
  plan[, `:=`(action = "source absent; skipped", mappings_corrected = 0L)]
  for (i in seq_len(nrow(plan))) {
    # Refresh after each operation: canonical targets may themselves be renamed.
    p <- data.table::as.data.table(DBI::dbGetQuery(con, "SELECT * FROM public.parameters"))
    source <- p[tolower(trimws(param_name)) == tolower(trimws(plan$source[[i]]))]
    target <- p[tolower(trimws(param_name)) == tolower(trimws(plan$target[[i]]))]
    if (nrow(source) > 1L || nrow(target) > 1L) stop("Ambiguous parameter name: ", plan$source[[i]])
    if (!nrow(source)) next
    source_id <- source$parameter_id[[1]]
    destination <- if (nrow(target)) target else source
    target_id <- destination$parameter_id[[1]]
    merge <- source_id != target_id
    # Optional identity guard for a generic label carrying a specific CAS.
    if (!is.na(plan$expected_cas[[i]]) && (is.na(source$cas_number[[1]]) ||
      trimws(source$cas_number[[1]]) != plan$expected_cas[[i]])) {
      stop("Source CAS needs review: ", plan$source[[i]])
    }
    if (plan$mode[[i]] == "rename" && !merge) {
      DBI::dbExecute(con, "UPDATE public.parameters SET param_name=$1 WHERE parameter_id=$2",
        params=list(plan$target[[i]], source_id))
      plan$action[[i]] <- "renamed"
      message(plan$source[[i]], " -> ", plan$target[[i]], ": renamed")
      next
    }
    if (merge) {
      incompatible <- DBI::dbGetQuery(con,
        "SELECT NOT (s.analysis_conditions_required_keys <@ t.analysis_conditions_required_keys)
           OR (s.analysis_conditions_required_keys_by_matrix <> '{}'::JSONB
             AND s.analysis_conditions_required_keys_by_matrix <> t.analysis_conditions_required_keys_by_matrix) AS incompatible
         FROM public.parameters s CROSS JOIN public.parameters t
         WHERE s.parameter_id=$1 AND t.parameter_id=$2", params=list(source_id,target_id))$incompatible[[1]]
      if (incompatible) stop("Review differing condition requirements before merging ", plan$source[[i]])
    }
    # Canonical sediment analytes gain a solid unit only if one is absent.
    # Percent and mg/kg are alternative expressions of mass fraction, not new
    # analytes. Unknown dry/wet mass basis stays unknown in analysis_conditions.
    if (plan$mode[[i]] == "solid" && merge && is.na(destination$units_solid[[1]])) {
      destination$units_solid <- source$units_solid
      DBI::dbExecute(con, "UPDATE public.parameters SET units_solid=$1 WHERE parameter_id=$2",
        params = list(destination$units_solid[[1]], target_id))
    }
    matrix_override <- if (plan$mode[[i]] == "toc") {
      id <- matrices$matrix_state_id[matrices$matrix_state_code == "not_applicable"]
      if (length(id) != 1L) stop("Missing not_applicable matrix.")
      id
    } else NULL
    # Compare ONLY populated source matrices. A target may support additional
    # matrices without changing the meaning of any existing source number.
    numeric_change <- FALSE
    for (field in unit_fields) {
      old_id <- source[[field]][[1]]
      new_id <- if (!is.null(matrix_override)) destination$units_na[[1]] else destination[[field]][[1]]
      if (is.na(old_id)) next
      if (is.na(new_id)) stop("Target lacks source matrix unit: ", plan$source[[i]])
      if (old_id == new_id) next
      old <- patch66_unit_definition(units[unit_id == old_id, unit_name], field == "units_solid")
      new <- patch66_unit_definition(units[unit_id == new_id, unit_name], field == "units_solid")
      if (old$kind != new$kind) stop("Incompatible units: ", plan$source[[i]])
      numeric_change <- numeric_change || old$scale != new$scale
    }
    if (numeric_change) for (j in seq_len(nrow(refs))) {
      ref <- refs[j, ]
      metadata <- ref$schema_name == "public" && ref$table_name %in% c("parameter_relationships",
        "parameter_reporting_basis_suggestions", "parameter_speciation_suggestions", "parameter_target_suggestions")
      if (metadata || (ref$schema_name == "discrete" && ref$table_name == "import_parameter_mappings")) next
      used <- DBI::dbGetQuery(con, sprintf("SELECT EXISTS (SELECT 1 FROM %s WHERE %s=$1) AS used",
        DBI::dbQuoteIdentifier(con, DBI::Id(schema = ref$schema_name, table = ref$table_name)),
        DBI::dbQuoteIdentifier(con, ref$column_name)), params = list(source_id))$used[[1]]
      if (used) stop("Separate numeric migration required for ", plan$source[[i]], " in ", ref$table_name)
    }
    mappings <- if (has_mappings) DBI::dbGetQuery(con,
      "SELECT import_mapping_id, source_match->>'input_unit' AS input_unit,
         matrix_state_id, conversion, result_offset FROM discrete.import_parameter_mappings WHERE parameter_id=$1",
      params = list(source_id)) else data.frame()
    # Normalize factors only where a source's populated unit changes scale.
    # Accept either source-correct or already-target-correct factors, never
    # multiply an already-converted factor a second time. Custom factors stop.
    if (numeric_change && nrow(mappings)) {
      for (j in seq_len(nrow(mappings))) {
        m <- mappings[j, ]
        matrix_code <- matrices$matrix_state_code[matrices$matrix_state_id == m$matrix_state_id]
        if (length(matrix_code) != 1L || is.na(matrix_code)) stop("Explicit mapping matrix needed: ", m$import_mapping_id)
        field <- switch(matrix_code, liquid="units_liquid", solid="units_solid", gas="units_gas", not_applicable="units_na")
        old <- patch66_unit_definition(units[unit_id == source[[field]][[1]], unit_name], matrix_code == "solid")
        new <- patch66_unit_definition(units[unit_id == destination[[field]][[1]], unit_name], matrix_code == "solid")
        input <- patch66_unit_definition(m$input_unit, matrix_code == "solid")
        if (input$kind != old$kind || old$kind != new$kind) stop("Incompatible mapping units.")
        expected <- input$scale / new$scale
        previous <- input$scale / old$scale
        actual <- if (is.na(m$conversion)) 1 else m$conversion
        offset <- if (is.na(m$result_offset)) 0 else m$result_offset
        if (!is.finite(actual) || !is.finite(offset) || offset != 0 ||
          !any(abs(actual-c(expected,previous)) <= 1e-12*pmax(abs(c(expected,previous)), .Machine$double.eps))) {
          stop("Unreviewed conversion/offset for mapping ", m$import_mapping_id)
        }
        if (actual != expected) {
          DBI::dbExecute(con, "ALTER TABLE discrete.import_parameter_mappings DISABLE TRIGGER guard_import_mapping_row")
          DBI::dbExecute(con, "UPDATE discrete.import_parameter_mappings SET conversion=$1 WHERE import_mapping_id=$2",
            params = list(expected, m$import_mapping_id))
          DBI::dbExecute(con, "ALTER TABLE discrete.import_parameter_mappings ENABLE TRIGGER guard_import_mapping_row")
          plan$mappings_corrected[[i]] <- plan$mappings_corrected[[i]] + 1L
        }
      }
    }
    if (merge) DBI::dbExecute(con,
      "UPDATE public.parameters SET units_liquid=$1,units_solid=$2,units_gas=$3,units_na=$4 WHERE parameter_id=$5",
      params = c(unname(as.list(destination[, ..unit_fields])), list(source_id)))

    chemistry <- list()
    # A formerly single-matrix source may join a multi-matrix target. Retain
    # its previously implicit matrix before expanding the unit definitions.
    populated <- which(!vapply(source[, ..unit_fields], function(value) is.na(value[[1]]), logical(1)))
    if (merge && is.null(matrix_override) && length(populated)==1L) {
      code <- c("liquid","solid","gas","not_applicable")[[populated]]
      id <- matrices$matrix_state_id[matrices$matrix_state_code==code]
      if (length(id)!=1L) stop("Source matrix did not resolve uniquely.")
      chemistry$matrix_state_id <- id
    }
    if (!is.na(plan$speciation_code[[i]])) {
      code <- plan$speciation_code[[i]]
      found <- DBI::dbGetQuery(con,
        "SELECT result_speciation_id, result_speciation, active FROM discrete.result_speciations WHERE result_speciation_code=$1",
        params = list(code))
      if (!nrow(found)) found <- DBI::dbGetQuery(con,
        "INSERT INTO discrete.result_speciations (result_speciation_code,result_speciation,description)
         VALUES ($1,$2,$3) RETURNING result_speciation_id,result_speciation,active",
        params = list(code, plan$speciation_label[[i]], paste0("Chemical form: ", plan$speciation_label[[i]], ".")))
      if (nrow(found) != 1L || !isTRUE(found$active[[1]]) || found$result_speciation[[1]] != plan$speciation_label[[i]]) {
        stop("Conflicting/inactive chemical-form definition: ", code)
      }
      chemistry$result_speciation_id <- found$result_speciation_id[[1]]
    }
    if (!is.na(plan$endpoint_ph[[i]])) {
      chemistry$endpoint_mode <- "fixed"
      chemistry$endpoint_ph <- plan$endpoint_ph[[i]]
    }
    if (!is.null(matrix_override)) chemistry$matrix_state_id <- matrix_override
    # 'Total nitrogen/organic carbon' describes the analyte's chemical pool,
    # not necessarily an unfiltered/whole-sample physical preparation. Preserve
    # any recorded sample fraction and leave missing fractions unknown.
    if (!is.na(plan$conditions[[i]])) chemistry$analysis_conditions <- plan$conditions[[i]]
    # Apply reviewed qualifiers to every analytical reference before merging.
    # SQL predicates reject contradictory populated metadata; missing historical
    # reporting bases and mass bases are never guessed from a chemical name.
    for (table in c("discrete.results", if (has_mappings) "discrete.import_parameter_mappings",
      "criteria.guidelines", "criteria.guideline_rule_inputs")) {
      parts <- strsplit(table, ".", fixed=TRUE)[[1]]
      fields <- columns$column_name[columns$table_schema == parts[[1]] & columns$table_name == parts[[2]]]
      additions <- chemistry[intersect(names(chemistry), fields)]
      if (!length(additions)) next
      assignments <- character()
      for (field in names(additions)) {
        value <- additions[[field]]
        if (field == "analysis_conditions") {
          conflict_sql <- "analysis_conditions @> $2::JSONB IS NOT TRUE AND EXISTS
            (SELECT 1 FROM jsonb_each($2::JSONB) e WHERE analysis_conditions ? e.key
             AND analysis_conditions->e.key IS DISTINCT FROM e.value)"
        } else if (field == "matrix_state_id" && !is.null(matrix_override)) {
          conflict_sql <- "FALSE" # explicit reference-point/matrix correction
        } else conflict_sql <- sprintf("%s IS NOT NULL AND %s IS DISTINCT FROM $2", field, field)
        conflict <- DBI::dbGetQuery(con, sprintf(
          "SELECT EXISTS (SELECT 1 FROM %s WHERE parameter_id=$1 AND (%s)) AS conflict", table, conflict_sql),
          params = if (conflict_sql == "FALSE") list(source_id) else list(source_id, value))$conflict[[1]]
        if (conflict) stop("Conflicting ", field, " for ", plan$source[[i]], " in ", table)
        assignments <- c(assignments, sprintf("%s=%s", field, if (field == "analysis_conditions") {
          sprintf("COALESCE(analysis_conditions,'{}'::JSONB) || $%s::JSONB", length(assignments)+2L)
        } else sprintf("$%s", length(assignments)+2L)))
      }
      if (table == "discrete.import_parameter_mappings") DBI::dbExecute(con,
        "ALTER TABLE discrete.import_parameter_mappings DISABLE TRIGGER guard_import_mapping_row")
      DBI::dbExecute(con, sprintf("UPDATE %s SET %s WHERE parameter_id=$1", table, paste(assignments, collapse=",")),
        params = c(list(source_id), unname(additions)))
      if (table == "discrete.import_parameter_mappings") DBI::dbExecute(con,
        "ALTER TABLE discrete.import_parameter_mappings ENABLE TRIGGER guard_import_mapping_row")
    }
    # No policy requirement is weakened: combine global flags after references
    # move. Target condition requirements are retained; incompatible source
    # requirements were rejected above. Legacy unknowns remain readable.
    AquaCache:::revise_parameter_catalogue_entry(con, source_name=source$param_name[[1]],
      target_name=plan$target[[i]], delete_source=merge)
    if (length(chemistry$result_speciation_id) && any(columns$table_schema=="public" &
      columns$table_name=="parameter_speciation_suggestions")) {
      DBI::dbExecute(con,
        "INSERT INTO public.parameter_speciation_suggestions (parameter_id,result_speciation_id,display_order)
         VALUES ($1,$2,1) ON CONFLICT (parameter_id,result_speciation_id) DO NOTHING",
        params=list(target_id,chemistry$result_speciation_id))
    }
    if (merge) {
      flags <- vapply(requirements, function(field) isTRUE(source[[field]][[1]]) || isTRUE(destination[[field]][[1]]), logical(1))
      for (field in names(flags)[flags]) {
        if (!isTRUE(destination[[field]][[1]])) DBI::dbExecute(con, sprintf(
          "UPDATE public.parameters SET %s=TRUE WHERE parameter_id=$1", field), params=list(target_id))
      }
    }
    plan$action[[i]] <- if (merge) "merged" else if (source$param_name[[1]] != plan$target[[i]]) "renamed" else "qualified"
    message(plan$source[[i]], " -> ", plan$target[[i]], ": ", plan$action[[i]])
  }
  for (j in seq_len(nrow(policy))) DBI::dbExecute(con, sprintf(
    "ALTER TABLE %s %s TRIGGER %s", DBI::dbQuoteIdentifier(con,
      DBI::Id(schema=policy$schema_name[[j]],table=policy$table_name[[j]])),
    switch(policy$state[[j]], O="ENABLE", A="ENABLE ALWAYS", R="ENABLE REPLICA"),
    DBI::dbQuoteIdentifier(con,policy$tgname[[j]])))
  duplicates <- DBI::dbGetQuery(con,
    "SELECT lower(regexp_replace(btrim(param_name),'[[:space:]]+',' ','g')) AS name,
       count(*) FROM public.parameters GROUP BY 1 HAVING count(*)>1")
  if (nrow(duplicates)) stop("Unresolved duplicate parameter names: ", paste(duplicates$name,collapse=", "))
  if (has_mappings) {
    state <- DBI::dbGetQuery(con,
      "SELECT tgenabled::TEXT AS state FROM pg_trigger WHERE
       tgrelid='discrete.import_parameter_mappings'::regclass AND tgname='guard_import_mapping_row'")
    if (nrow(state)!=1L || state$state[[1]]!="O") stop("Mapping guard was not restored.")
  }
  print(plan[, .(source,target,action,mappings_corrected)])
  invisible(plan)
}
