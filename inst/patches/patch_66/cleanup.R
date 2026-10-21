# One-time Patch 66 helpers. No connection, transaction or version management.
# The shared package helper migrates every relational parameter reference,
# coalesces advisory links/group membership, and rejects result-key collisions.
# Complete the reviewed EQWin acidity mappings with explicit unresolved
# chemistry states. Other incomplete mappings still stop the catalogue change.

patch66_ensure_unknown_reporting_basis <- function(con) {
  DBI::dbExecute(
    con,
    "LOCK TABLE discrete.reporting_bases IN SHARE ROW EXCLUSIVE MODE"
  )
  row <- DBI::dbGetQuery(
    con,
    "SELECT reporting_basis_id, reporting_basis_code, reporting_basis,
       basis_kind, active
     FROM discrete.reporting_bases
     WHERE reporting_basis_code = 'unknown'
        OR lower(btrim(reporting_basis)) = 'unknown / not reported'"
  )
  if (nrow(row) > 1L) {
    stop("Ambiguous unknown reporting-basis entry.")
  }
  if (!nrow(row)) {
    DBI::dbGetQuery(
      con,
      "INSERT INTO discrete.reporting_bases
         (reporting_basis_code, reporting_basis, basis_kind, description)
       VALUES ('unknown', 'unknown / not reported', 'unresolved',
         'The source did not report a numeric reporting expression. This explicit unresolved state has no conversion semantics and is not equivalent to any known reporting basis.')
       RETURNING reporting_basis_id"
    )
    row <- DBI::dbGetQuery(
      con,
      "SELECT reporting_basis_id, reporting_basis_code, reporting_basis,
         basis_kind, active
       FROM discrete.reporting_bases WHERE reporting_basis_code = 'unknown'"
    )
  }
  if (
    nrow(row) != 1L ||
      row$reporting_basis_code[[1]] != "unknown" ||
      row$reporting_basis[[1]] != "unknown / not reported" ||
      row$basis_kind[[1]] != "unresolved"
  ) {
    stop("The reporting-basis code 'unknown' already has a different meaning.")
  }
  if (!isTRUE(row$active[[1]])) {
    DBI::dbExecute(
      con,
      "UPDATE discrete.reporting_bases SET active = TRUE
       WHERE reporting_basis_id = $1",
      params = list(row$reporting_basis_id[[1]])
    )
  }
  DBI::dbExecute(
    con,
    "COMMENT ON TABLE discrete.reporting_bases IS
     'Numeric reporting expressions independent of units, chemical form, and analytical target. The unknown / not reported entry is an explicit unresolved state with no conversion semantics; retire entries instead of deleting historical references.'"
  )
  as.integer(row$reporting_basis_id[[1]])
}

patch66_allow_unknown_endpoint <- function(con, has_mappings = TRUE) {
  checks <- DBI::dbGetQuery(
    con,
    "SELECT n.nspname AS schema_name, c.relname AS table_name,
       constraint_row.conname AS constraint_name,
       pg_get_constraintdef(constraint_row.oid) AS definition
     FROM pg_constraint constraint_row
     JOIN pg_class c ON c.oid = constraint_row.conrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace
     WHERE constraint_row.contype = 'c'
       AND n.nspname IN ('discrete', 'criteria', 'public')
       AND constraint_row.conname = c.relname || '_endpoint_definition'
       AND EXISTS (SELECT 1 FROM information_schema.columns column_row
         WHERE column_row.table_schema = n.nspname AND column_row.table_name = c.relname
           AND column_row.column_name = 'endpoint_mode')
       AND EXISTS (SELECT 1 FROM information_schema.columns column_row
         WHERE column_row.table_schema = n.nspname AND column_row.table_name = c.relname
           AND column_row.column_name = 'endpoint_ph')
     ORDER BY n.nspname, c.relname"
  )
  required <- c(
    "discrete.results",
    if (isTRUE(has_mappings)) "discrete.import_parameter_mappings"
  )
  found <- paste(checks$schema_name, checks$table_name, sep = ".")
  if (!all(required %in% found)) {
    stop(
      "Patch 66 requires Patch 65 endpoint constraints on results and import mappings."
    )
  }
  for (i in seq_len(nrow(checks))) {
    definition <- checks$definition[[i]]
    if (grepl("'unknown'", definition, fixed = TRUE)) {
      next
    }
    expected_tokens <- c(
      "endpoint_mode",
      "endpoint_ph",
      "fixed",
      "inflection",
      "gran",
      "other"
    )
    if (
      !all(vapply(
        expected_tokens,
        grepl,
        logical(1),
        x = definition,
        fixed = TRUE
      ))
    ) {
      stop(
        "Unreviewed endpoint constraint on ",
        checks$schema_name[[i]],
        ".",
        checks$table_name[[i]],
        "."
      )
    }
    relation <- as.character(DBI::dbQuoteIdentifier(
      con,
      DBI::Id(schema = checks$schema_name[[i]], table = checks$table_name[[i]])
    ))
    constraint <- as.character(DBI::dbQuoteIdentifier(
      con,
      checks$constraint_name[[i]]
    ))
    DBI::dbExecute(
      con,
      sprintf("ALTER TABLE %s DROP CONSTRAINT %s", relation, constraint)
    )
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s ADD CONSTRAINT %s CHECK (
         (endpoint_mode IS NULL AND endpoint_ph IS NULL) OR
         (endpoint_mode IS NOT NULL
           AND endpoint_mode IN ('fixed', 'inflection', 'gran', 'other', 'unknown')
           AND (endpoint_mode <> 'fixed' OR endpoint_ph IS NOT NULL)
           AND (endpoint_mode <> 'unknown' OR endpoint_ph IS NULL)
         )
         )",
        relation,
        constraint
      )
    )
  }
  invisible(nrow(checks))
}

patch66_complete_eqwin_acidity_mappings <- function(
  con,
  unknown_reporting_basis_id
) {
  candidates <- DBI::dbGetQuery(
    con,
    "SELECT m.import_mapping_id, m.source_match->>'ParamCode' AS param_code,
       m.reporting_basis_id, basis.reporting_basis_code, m.endpoint_mode
     FROM discrete.import_parameter_mappings m
     JOIN discrete.import_mapping_sets mapping_set USING (import_mapping_set_id)
     JOIN discrete.import_sources import_source USING (import_source_id)
     JOIN public.parameters p USING (parameter_id)
     LEFT JOIN discrete.reporting_bases basis USING (reporting_basis_id)
     WHERE import_source.source_code = 'EQWin'
       AND lower(btrim(p.param_name)) = 'acidity'
       AND m.source_match->>'input_unit' = 'mg/L'
       AND m.source_match->>'ParamCode' IN ('Acidity', 'Acidity(asCaCO3)')
       AND (m.endpoint_mode IS NULL OR
         (m.source_match->>'ParamCode' = 'Acidity' AND m.reporting_basis_id IS NULL))
     ORDER BY m.import_mapping_id"
  )
  if (!nrow(candidates)) {
    message(
      "No reviewed incomplete EQWin acidity mappings require unknown metadata."
    )
    return(invisible(integer()))
  }
  if (
    any(
      candidates$param_code == "Acidity(asCaCO3)" &
        (is.na(candidates$reporting_basis_id) |
          is.na(candidates$reporting_basis_code) |
          candidates$reporting_basis_code != "as_caco3")
    )
  ) {
    stop(
      "EQWin Acidity(asCaCO3) mapping must retain its explicit CaCO3 reporting basis."
    )
  }
  unknown_dimensions <- character()
  if (anyNA(candidates$endpoint_mode)) {
    unknown_dimensions <- c(unknown_dimensions, "endpoint")
  }
  if (
    any(
      candidates$param_code == "Acidity" & is.na(candidates$reporting_basis_id)
    )
  ) {
    unknown_dimensions <- c(unknown_dimensions, "reporting basis")
  }
  guard <- DBI::dbGetQuery(
    con,
    "SELECT tgenabled::TEXT AS state FROM pg_trigger
     WHERE tgrelid = 'discrete.import_parameter_mappings'::regclass
       AND tgname = 'guard_import_mapping_row' AND NOT tgisinternal"
  )
  if (nrow(guard) != 1L || guard$state[[1]] != "O") {
    stop(
      "Mapping immutability guard must be enabled before the reviewed EQWin correction."
    )
  }
  DBI::dbExecute(
    con,
    "ALTER TABLE discrete.import_parameter_mappings DISABLE TRIGGER guard_import_mapping_row"
  )
  updated <- tryCatch(
    DBI::dbGetQuery(
      con,
      "UPDATE discrete.import_parameter_mappings m
     SET reporting_basis_id = CASE
           WHEN m.source_match->>'ParamCode' = 'Acidity'
             THEN COALESCE(m.reporting_basis_id, $1)
           ELSE m.reporting_basis_id END,
         endpoint_mode = COALESCE(m.endpoint_mode, 'unknown')
     FROM discrete.import_mapping_sets mapping_set,
          discrete.import_sources import_source, public.parameters p
     WHERE m.import_mapping_set_id = mapping_set.import_mapping_set_id
       AND mapping_set.import_source_id = import_source.import_source_id
       AND m.parameter_id = p.parameter_id
       AND import_source.source_code = 'EQWin'
       AND lower(btrim(p.param_name)) = 'acidity'
       AND m.source_match->>'input_unit' = 'mg/L'
       AND m.source_match->>'ParamCode' IN ('Acidity', 'Acidity(asCaCO3)')
       AND (m.endpoint_mode IS NULL OR
         (m.source_match->>'ParamCode' = 'Acidity' AND m.reporting_basis_id IS NULL))
     RETURNING m.import_mapping_id",
      params = list(as.integer(unknown_reporting_basis_id))
    ),
    finally = try(
      DBI::dbExecute(
        con,
        "ALTER TABLE discrete.import_parameter_mappings ENABLE TRIGGER guard_import_mapping_row"
      ),
      silent = TRUE
    )
  )
  unresolved <- DBI::dbGetQuery(
    con,
    "SELECT m.import_mapping_id, discrete.import_parameter_mapping_error(m) AS missing
     FROM discrete.import_parameter_mappings m
     JOIN discrete.import_mapping_sets mapping_set USING (import_mapping_set_id)
     JOIN discrete.import_sources import_source USING (import_source_id)
     JOIN public.parameters p USING (parameter_id)
     WHERE import_source.source_code = 'EQWin'
       AND lower(btrim(p.param_name)) = 'acidity'
       AND m.source_match->>'input_unit' = 'mg/L'
       AND m.source_match->>'ParamCode' IN ('Acidity', 'Acidity(asCaCO3)')
       AND discrete.import_parameter_mapping_error(m) IS NOT NULL
     ORDER BY m.import_mapping_id"
  )
  if (nrow(unresolved)) {
    stop(
      "Reviewed EQWin acidity mappings remain incomplete: ",
      paste(
        sprintf("%s (%s)", unresolved$import_mapping_id, unresolved$missing),
        collapse = "; "
      )
    )
  }
  message(
    "Recorded unknown ",
    paste(unknown_dimensions, collapse = " and "),
    " for ",
    nrow(updated),
    " EQWin acidity mapping(s). Mapping IDs and source-match criteria were preserved."
  )
  invisible(updated$import_mapping_id)
}

patch66_unit_definition <- function(label, solid = FALSE) {
  key <- tolower(gsub("[[:space:]]", "", label))
  key <- gsub("\u00b5|\u03bc", "u", key)
  liquid <- c("mg/l" = 1, "ug/l" = .001, "ng/l" = .000001)
  mass <- c("mg/kg" = 1, "ug/kg" = .001, "ng/kg" = .000001, "%" = 10000)
  other <- c("cu" = 1, "tcu" = 1, "m" = 1, "mbtoc" = 1)
  scale <- c(if (solid) mass else liquid, other)
  if (length(key) != 1L || is.na(key) || !key %in% names(scale)) {
    stop("Unreviewed unit: ", label)
  }
  list(
    kind = if (key %in% names(other)) {
      if (key %in% c("cu", "tcu")) "true_colour" else "length"
    } else if (solid) {
      "mass_fraction"
    } else {
      "mass_concentration"
    },
    scale = unname(scale[[key]])
  )
}

patch66_parameter_cleanup <- function(con, plan_path) {
  if (!AquaCache:::dbTransCheck(con)) {
    stop("Patch 66 cleanup requires an active transaction.")
  }
  plan <- data.table::fread(
    plan_path,
    sep = "\t",
    quote = "",
    na.strings = "",
    encoding = "UTF-8"
  )
  if (
    anyNA(plan$source) ||
      anyNA(plan$target) ||
      anyDuplicated(tolower(trimws(plan$source)))
  ) {
    stop("Invalid or repeated source in Patch 66 manifest.")
  }
  if (
    any(
      !vapply(
        plan$conditions[!is.na(plan$conditions)],
        jsonlite::validate,
        logical(1)
      )
    )
  ) {
    stop("Invalid analytical-condition JSON in Patch 66 manifest.")
  }
  columns <- DBI::dbGetQuery(
    con,
    "SELECT table_schema, table_name, column_name FROM information_schema.columns
     WHERE table_schema IN ('public','discrete','criteria')"
  )
  for (table in c("reporting_bases", "result_speciations", "result_targets")) {
    if (
      !any(columns$table_schema == "discrete" & columns$table_name == table)
    ) {
      stop("Patch 66 requires the final typed Patch 65 schema: ", table)
    }
  }
  unit_fields <- c("units_liquid", "units_solid", "units_gas", "units_na")
  requirements <- c(
    "sample_fraction",
    "reporting_basis",
    "result_speciation",
    "result_target",
    "endpoint"
  )
  has_mappings <- any(
    columns$table_schema == "discrete" &
      columns$table_name == "import_parameter_mappings"
  )
  DBI::dbExecute(
    con,
    "LOCK TABLE public.parameters IN SHARE ROW EXCLUSIVE MODE"
  )
  units <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT * FROM public.units"
  ))
  matrices <- DBI::dbGetQuery(con, "SELECT * FROM public.matrix_states")
  refs <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT n.nspname AS schema_name, c.relname AS table_name,
       a.attname AS column_name, cardinality(f.conkey) AS key_size
     FROM pg_constraint f JOIN pg_class c ON c.oid = f.conrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace
     JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(f.conkey)
     WHERE f.contype = 'f' AND f.confrelid = 'public.parameters'::regclass"
  )
  if (any(refs$key_size != 1L)) {
    stop("Review composite parameter foreign keys before cleanup.")
  }
  # Include standard result/mapping tables even on instances lacking an FK.
  for (table in c("results", if (has_mappings) "import_parameter_mappings")) {
    if (
      !any(
        refs$schema_name == "discrete" &
          refs$table_name == table &
          refs$column_name == "parameter_id"
      )
    ) {
      refs <- rbind(
        refs,
        data.frame(
          schema_name = "discrete",
          table_name = table,
          column_name = "parameter_id",
          key_size = 1L
        )
      )
    }
  }
  # Prevent concurrent inserts while interpreting a legacy definition. Lock in
  # deterministic order; do not disable audit, identity, unit or FK checks.
  ref_tables <- unique(refs[c("schema_name", "table_name")])
  ref_tables <- ref_tables[
    order(ref_tables$schema_name, ref_tables$table_name),
  ]
  for (j in seq_len(nrow(ref_tables))) {
    DBI::dbExecute(
      con,
      sprintf(
        "LOCK TABLE %s IN SHARE ROW EXCLUSIVE MODE",
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(
            schema = ref_tables$schema_name[[j]],
            table = ref_tables$table_name[[j]]
          )
        )
      )
    )
  }
  guard <- if (has_mappings) {
    DBI::dbGetQuery(
      con,
      "SELECT tgenabled::TEXT AS state FROM pg_trigger
     WHERE tgrelid = 'discrete.import_parameter_mappings'::regclass
       AND tgname = 'guard_import_mapping_row' AND NOT tgisinternal"
    )
  } else {
    NULL
  }
  if (has_mappings && (nrow(guard) != 1L || guard$state[[1]] != "O")) {
    stop("Mapping guard must be enabled.")
  }
  patch66_allow_unknown_endpoint(con, has_mappings = has_mappings)
  unknown_basis_id <- patch66_ensure_unknown_reporting_basis(con)
  if (has_mappings) {
    patch66_complete_eqwin_acidity_mappings(con, unknown_basis_id)
  }
  if (has_mappings) {
    source_names <- unique(tolower(trimws(plan$source)))
    source_names_sql <- paste(
      DBI::dbQuoteString(con, source_names),
      collapse = ", "
    )
    incomplete <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT p.param_name,
         string_agg(m.import_mapping_id::TEXT || ' (' || mapping_error.dimension || ')',
           '; ' ORDER BY m.import_mapping_id) AS mapping_details
       FROM discrete.import_parameter_mappings m
       JOIN public.parameters p USING (parameter_id)
       CROSS JOIN LATERAL (
         SELECT discrete.import_parameter_mapping_error(m) AS dimension
       ) AS mapping_error
       WHERE lower(trim(p.param_name)) IN (",
        source_names_sql,
        ")
         AND mapping_error.dimension IS NOT NULL
       GROUP BY p.parameter_id, p.param_name
       ORDER BY p.param_name"
      )
    )
    if (nrow(incomplete)) {
      details <- paste0(
        incomplete$param_name,
        " [",
        incomplete$mapping_details,
        "]"
      )
      stop(
        "Patch 66 found incomplete import mappings beyond the reviewed EQWin ",
        "acidity exceptions. Review these manifest source(s) first: ",
        paste(details, collapse = "; ")
      )
    }
  }
  # Relocating an old result must not invent unknown reporting bases/fractions
  # merely to satisfy a new target's insertion policy. Suspend only these two
  # requiredness guards during the reviewed migration. Existing typed fields,
  # numerical values and source payloads are retained; current policy is restored.
  policy <- DBI::dbGetQuery(
    con,
    "SELECT n.nspname AS schema_name, c.relname AS table_name, t.tgname, t.tgenabled::TEXT AS state
     FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid
     JOIN pg_namespace n ON n.oid = c.relnamespace JOIN pg_proc p ON p.oid = t.tgfoid
     WHERE NOT t.tgisinternal AND n.nspname = 'discrete'
       AND ((c.relname='results' AND p.proname='enforce_result_speciation')
         OR (c.relname='import_parameter_mappings' AND p.proname='enforce_import_parameter_mapping_requirements'))"
  )
  if (
    nrow(policy) != 1L + as.integer(has_mappings) ||
      any(!policy$state %in% c("O", "A", "R"))
  ) {
    stop("Unexpected Patch 65 chemistry policy triggers.")
  }
  for (j in seq_len(nrow(policy))) {
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s DISABLE TRIGGER %s",
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(
            schema = policy$schema_name[[j]],
            table = policy$table_name[[j]]
          )
        ),
        DBI::dbQuoteIdentifier(con, policy$tgname[[j]])
      )
    )
  }
  plan[, `:=`(action = "source absent; skipped", mappings_corrected = 0L)]
  for (i in seq_len(nrow(plan))) {
    # Refresh after each operation: canonical targets may themselves be renamed.
    p <- data.table::as.data.table(DBI::dbGetQuery(
      con,
      "SELECT * FROM public.parameters"
    ))
    source <- p[
      tolower(trimws(param_name)) == tolower(trimws(plan$source[[i]]))
    ]
    target <- p[
      tolower(trimws(param_name)) == tolower(trimws(plan$target[[i]]))
    ]
    if (nrow(source) > 1L || nrow(target) > 1L) {
      stop("Ambiguous parameter name: ", plan$source[[i]])
    }
    if (!nrow(source)) {
      next
    }
    source_id <- source$parameter_id[[1]]
    destination <- if (nrow(target)) target else source
    target_id <- destination$parameter_id[[1]]
    merge <- source_id != target_id
    # Optional identity guard for a generic label carrying a specific CAS.
    if (
      !is.na(plan$expected_cas[[i]]) &&
        (is.na(source$cas_number[[1]]) ||
          trimws(source$cas_number[[1]]) != plan$expected_cas[[i]])
    ) {
      stop("Source CAS needs review: ", plan$source[[i]])
    }
    if (plan$mode[[i]] == "rename" && !merge) {
      DBI::dbExecute(
        con,
        "UPDATE public.parameters SET param_name=$1 WHERE parameter_id=$2",
        params = list(plan$target[[i]], source_id)
      )
      plan$action[[i]] <- "renamed"
      message(plan$source[[i]], " -> ", plan$target[[i]], ": renamed")
      next
    }
    if (merge) {
      incompatible <- DBI::dbGetQuery(
        con,
        "SELECT NOT (s.analysis_conditions_required_keys <@ t.analysis_conditions_required_keys)
           OR (s.analysis_conditions_required_keys_by_matrix <> '{}'::JSONB
             AND s.analysis_conditions_required_keys_by_matrix <> t.analysis_conditions_required_keys_by_matrix) AS incompatible
         FROM public.parameters s CROSS JOIN public.parameters t
         WHERE s.parameter_id=$1 AND t.parameter_id=$2",
        params = list(source_id, target_id)
      )$incompatible[[1]]
      if (incompatible) {
        stop(
          "Review differing condition requirements before merging ",
          plan$source[[i]]
        )
      }
    }
    # Canonical sediment analytes gain a solid unit only if one is absent.
    # Percent and mg/kg are alternative expressions of mass fraction, not new
    # analytes. Unknown dry/wet mass basis stays unknown in analysis_conditions.
    if (
      plan$mode[[i]] == "solid" && merge && is.na(destination$units_solid[[1]])
    ) {
      destination$units_solid <- source$units_solid
      DBI::dbExecute(
        con,
        "UPDATE public.parameters SET units_solid=$1 WHERE parameter_id=$2",
        params = list(destination$units_solid[[1]], target_id)
      )
    }
    matrix_override <- if (plan$mode[[i]] == "toc") {
      id <- matrices$matrix_state_id[
        matrices$matrix_state_code == "not_applicable"
      ]
      if (length(id) != 1L) {
        stop("Missing not_applicable matrix.")
      }
      id
    } else {
      NULL
    }
    # Compare ONLY populated source matrices. A target may support additional
    # matrices without changing the meaning of any existing source number.
    numeric_change <- FALSE
    for (field in unit_fields) {
      old_id <- source[[field]][[1]]
      new_id <- if (!is.null(matrix_override)) {
        destination$units_na[[1]]
      } else {
        destination[[field]][[1]]
      }
      if (is.na(old_id)) {
        next
      }
      if (is.na(new_id)) {
        stop("Target lacks source matrix unit: ", plan$source[[i]])
      }
      if (old_id == new_id) {
        next
      }
      old <- patch66_unit_definition(
        units[unit_id == old_id, unit_name],
        field == "units_solid"
      )
      new <- patch66_unit_definition(
        units[unit_id == new_id, unit_name],
        field == "units_solid"
      )
      if (old$kind != new$kind) {
        stop("Incompatible units: ", plan$source[[i]])
      }
      numeric_change <- numeric_change || old$scale != new$scale
    }
    if (numeric_change) {
      for (j in seq_len(nrow(refs))) {
        ref <- refs[j, ]
        metadata <- ref$schema_name == "public" &&
          ref$table_name %in%
            c(
              "parameter_relationships",
              "parameter_reporting_basis_suggestions",
              "parameter_speciation_suggestions",
              "parameter_target_suggestions"
            )
        if (
          metadata ||
            (ref$schema_name == "discrete" &&
              ref$table_name == "import_parameter_mappings")
        ) {
          next
        }
        used <- DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT EXISTS (SELECT 1 FROM %s WHERE %s=$1) AS used",
            DBI::dbQuoteIdentifier(
              con,
              DBI::Id(schema = ref$schema_name, table = ref$table_name)
            ),
            DBI::dbQuoteIdentifier(con, ref$column_name)
          ),
          params = list(source_id)
        )$used[[1]]
        if (used) {
          stop(
            "Separate numeric migration required for ",
            plan$source[[i]],
            " in ",
            ref$table_name
          )
        }
      }
    }
    mappings <- if (has_mappings) {
      DBI::dbGetQuery(
        con,
        "SELECT import_mapping_id, source_match->>'input_unit' AS input_unit,
         matrix_state_id, conversion, result_offset FROM discrete.import_parameter_mappings WHERE parameter_id=$1",
        params = list(source_id)
      )
    } else {
      data.frame()
    }
    # Normalize factors only where a source's populated unit changes scale.
    # Accept either source-correct or already-target-correct factors, never
    # multiply an already-converted factor a second time. Custom factors stop.
    if (numeric_change && nrow(mappings)) {
      for (j in seq_len(nrow(mappings))) {
        m <- mappings[j, ]
        matrix_code <- matrices$matrix_state_code[
          matrices$matrix_state_id == m$matrix_state_id
        ]
        if (length(matrix_code) != 1L || is.na(matrix_code)) {
          stop("Explicit mapping matrix needed: ", m$import_mapping_id)
        }
        field <- switch(
          matrix_code,
          liquid = "units_liquid",
          solid = "units_solid",
          gas = "units_gas",
          not_applicable = "units_na"
        )
        old <- patch66_unit_definition(
          units[unit_id == source[[field]][[1]], unit_name],
          matrix_code == "solid"
        )
        new <- patch66_unit_definition(
          units[unit_id == destination[[field]][[1]], unit_name],
          matrix_code == "solid"
        )
        input <- patch66_unit_definition(m$input_unit, matrix_code == "solid")
        if (input$kind != old$kind || old$kind != new$kind) {
          stop("Incompatible mapping units.")
        }
        expected <- input$scale / new$scale
        previous <- input$scale / old$scale
        actual <- if (is.na(m$conversion)) 1 else m$conversion
        offset <- if (is.na(m$result_offset)) 0 else m$result_offset
        if (
          !is.finite(actual) ||
            !is.finite(offset) ||
            offset != 0 ||
            !any(
              abs(actual - c(expected, previous)) <=
                1e-12 * pmax(abs(c(expected, previous)), .Machine$double.eps)
            )
        ) {
          stop("Unreviewed conversion/offset for mapping ", m$import_mapping_id)
        }
        if (actual != expected) {
          DBI::dbExecute(
            con,
            "ALTER TABLE discrete.import_parameter_mappings DISABLE TRIGGER guard_import_mapping_row"
          )
          DBI::dbExecute(
            con,
            "UPDATE discrete.import_parameter_mappings SET conversion=$1 WHERE import_mapping_id=$2",
            params = list(expected, m$import_mapping_id)
          )
          DBI::dbExecute(
            con,
            "ALTER TABLE discrete.import_parameter_mappings ENABLE TRIGGER guard_import_mapping_row"
          )
          plan$mappings_corrected[[i]] <- plan$mappings_corrected[[i]] + 1L
        }
      }
    }
    if (merge) {
      DBI::dbExecute(
        con,
        "UPDATE public.parameters SET units_liquid=$1,units_solid=$2,units_gas=$3,units_na=$4 WHERE parameter_id=$5",
        params = c(
          unname(as.list(destination[, ..unit_fields])),
          list(source_id)
        )
      )
    }

    chemistry <- list()
    # A formerly single-matrix source may join a multi-matrix target. Retain
    # its previously implicit matrix before expanding the unit definitions.
    populated <- which(
      !vapply(
        source[, ..unit_fields],
        function(value) is.na(value[[1]]),
        logical(1)
      )
    )
    if (merge && is.null(matrix_override) && length(populated) == 1L) {
      code <- c("liquid", "solid", "gas", "not_applicable")[[populated]]
      id <- matrices$matrix_state_id[matrices$matrix_state_code == code]
      if (length(id) != 1L) {
        stop("Source matrix did not resolve uniquely.")
      }
      chemistry$matrix_state_id <- id
    }
    if (!is.na(plan$speciation_code[[i]])) {
      code <- plan$speciation_code[[i]]
      found <- DBI::dbGetQuery(
        con,
        "SELECT result_speciation_id, result_speciation, active FROM discrete.result_speciations WHERE result_speciation_code=$1",
        params = list(code)
      )
      if (!nrow(found)) {
        found <- DBI::dbGetQuery(
          con,
          "INSERT INTO discrete.result_speciations (result_speciation_code,result_speciation,description)
         VALUES ($1,$2,$3) RETURNING result_speciation_id,result_speciation,active",
          params = list(
            code,
            plan$speciation_label[[i]],
            paste0("Chemical form: ", plan$speciation_label[[i]], ".")
          )
        )
      }
      if (
        nrow(found) != 1L ||
          !isTRUE(found$active[[1]]) ||
          found$result_speciation[[1]] != plan$speciation_label[[i]]
      ) {
        stop("Conflicting/inactive chemical-form definition: ", code)
      }
      chemistry$result_speciation_id <- found$result_speciation_id[[1]]
    }
    if (!is.na(plan$endpoint_ph[[i]])) {
      chemistry$endpoint_mode <- "fixed"
      chemistry$endpoint_ph <- plan$endpoint_ph[[i]]
    }
    if (!is.null(matrix_override)) {
      chemistry$matrix_state_id <- matrix_override
    }
    # 'Total nitrogen/organic carbon' describes the analyte's chemical pool,
    # not necessarily an unfiltered/whole-sample physical preparation. Preserve
    # any recorded sample fraction and leave missing fractions unknown.
    if (!is.na(plan$conditions[[i]])) {
      chemistry$analysis_conditions <- plan$conditions[[i]]
    }
    # Apply reviewed qualifiers to every analytical reference before merging.
    # SQL predicates reject contradictory populated metadata; missing historical
    # reporting bases and mass bases are never guessed from a chemical name.
    for (table in c(
      "discrete.results",
      if (has_mappings) "discrete.import_parameter_mappings",
      "criteria.guidelines",
      "criteria.guideline_rule_inputs"
    )) {
      parts <- strsplit(table, ".", fixed = TRUE)[[1]]
      fields <- columns$column_name[
        columns$table_schema == parts[[1]] & columns$table_name == parts[[2]]
      ]
      additions <- chemistry[intersect(names(chemistry), fields)]
      if (!length(additions)) {
        next
      }
      assignments <- character()
      for (field in names(additions)) {
        value <- additions[[field]]
        if (field == "analysis_conditions") {
          conflict_sql <- "analysis_conditions @> $2::JSONB IS NOT TRUE AND EXISTS
            (SELECT 1 FROM jsonb_each($2::JSONB) e WHERE analysis_conditions ? e.key
             AND analysis_conditions->e.key IS DISTINCT FROM e.value)"
        } else if (field == "matrix_state_id" && !is.null(matrix_override)) {
          conflict_sql <- "FALSE" # explicit reference-point/matrix correction
        } else {
          conflict_sql <- sprintf(
            "%s IS NOT NULL AND %s IS DISTINCT FROM $2",
            field,
            field
          )
        }
        conflict <- DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT EXISTS (SELECT 1 FROM %s WHERE parameter_id=$1 AND (%s)) AS conflict",
            table,
            conflict_sql
          ),
          params = if (conflict_sql == "FALSE") {
            list(source_id)
          } else {
            list(source_id, value)
          }
        )$conflict[[1]]
        if (conflict) {
          stop("Conflicting ", field, " for ", plan$source[[i]], " in ", table)
        }
        assignments <- c(
          assignments,
          sprintf(
            "%s=%s",
            field,
            if (field == "analysis_conditions") {
              sprintf(
                "COALESCE(analysis_conditions,'{}'::JSONB) || $%s::JSONB",
                length(assignments) + 2L
              )
            } else {
              sprintf("$%s", length(assignments) + 2L)
            }
          )
        )
      }
      if (table == "discrete.import_parameter_mappings") {
        DBI::dbExecute(
          con,
          "ALTER TABLE discrete.import_parameter_mappings DISABLE TRIGGER guard_import_mapping_row"
        )
      }
      DBI::dbExecute(
        con,
        sprintf(
          "UPDATE %s SET %s WHERE parameter_id=$1",
          table,
          paste(assignments, collapse = ",")
        ),
        params = c(list(source_id), unname(additions))
      )
      if (table == "discrete.import_parameter_mappings") {
        DBI::dbExecute(
          con,
          "ALTER TABLE discrete.import_parameter_mappings ENABLE TRIGGER guard_import_mapping_row"
        )
      }
    }
    # No policy requirement is weakened: combine global flags after references
    # move. Target condition requirements are retained; incompatible source
    # requirements were rejected above. Legacy unknowns remain readable.
    AquaCache:::revise_parameter_catalogue_entry(
      con,
      source_name = source$param_name[[1]],
      target_name = plan$target[[i]],
      delete_source = merge
    )
    if (
      length(chemistry$result_speciation_id) &&
        any(
          columns$table_schema == "public" &
            columns$table_name == "parameter_speciation_suggestions"
        )
    ) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.parameter_speciation_suggestions (parameter_id,result_speciation_id,display_order)
         VALUES ($1,$2,1) ON CONFLICT (parameter_id,result_speciation_id) DO NOTHING",
        params = list(target_id, chemistry$result_speciation_id)
      )
    }
    if (merge) {
      flags <- vapply(
        requirements,
        function(field) {
          isTRUE(source[[field]][[1]]) || isTRUE(destination[[field]][[1]])
        },
        logical(1)
      )
      for (field in names(flags)[flags]) {
        if (!isTRUE(destination[[field]][[1]])) {
          DBI::dbExecute(
            con,
            sprintf(
              "UPDATE public.parameters SET %s=TRUE WHERE parameter_id=$1",
              field
            ),
            params = list(target_id)
          )
        }
      }
    }
    plan$action[[i]] <- if (merge) {
      "merged"
    } else if (source$param_name[[1]] != plan$target[[i]]) {
      "renamed"
    } else {
      "qualified"
    }
    message(plan$source[[i]], " -> ", plan$target[[i]], ": ", plan$action[[i]])
  }
  for (j in seq_len(nrow(policy))) {
    DBI::dbExecute(
      con,
      sprintf(
        "ALTER TABLE %s %s TRIGGER %s",
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(
            schema = policy$schema_name[[j]],
            table = policy$table_name[[j]]
          )
        ),
        switch(
          policy$state[[j]],
          O = "ENABLE",
          A = "ENABLE ALWAYS",
          R = "ENABLE REPLICA"
        ),
        DBI::dbQuoteIdentifier(con, policy$tgname[[j]])
      )
    )
  }
  # Match the catalogue audit: ignore case and non-alphanumeric separators.
  duplicates <- DBI::dbGetQuery(
    con,
    "SELECT regexp_replace(lower(param_name), '[^a-z0-9]', '', 'g') AS name,
       count(*) FROM public.parameters GROUP BY 1 HAVING count(*)>1"
  )
  if (nrow(duplicates)) {
    stop(
      "Unresolved duplicate parameter names: ",
      paste(duplicates$name, collapse = ", ")
    )
  }
  if (has_mappings) {
    state <- DBI::dbGetQuery(
      con,
      "SELECT tgenabled::TEXT AS state FROM pg_trigger WHERE
       tgrelid='discrete.import_parameter_mappings'::regclass AND tgname='guard_import_mapping_row'"
    )
    if (nrow(state) != 1L || state$state[[1]] != "O") {
      stop("Mapping guard was not restored.")
    }
  }
  print(plan[, .(source, target, action, mappings_corrected)])
  invisible(plan)
}

patch66_guideline_matrix_state_fix <- function(con) {
  if (!AquaCache:::dbTransCheck(con)) {
    stop("Patch 66 guideline cleanup requires an active transaction.")
  }
  candidates <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT r.rule_id, r.formula_sql
     FROM criteria.guideline_value_rules r
     JOIN criteria.guideline_rule_inputs i USING (rule_id)
     WHERE r.algorithm_code = 'sql_scalar'
       AND r.formula_sql ~* 'discrete[.]get_sample_val[[:space:]]*[(]'
       AND COALESCE(i.input_source, 'sample_result') = 'sample_result'
       AND i.parameter_id IS NOT NULL AND i.matrix_state_id IS NOT NULL
     ORDER BY r.rule_id"
  )
  updated <- 0L
  for (j in seq_len(nrow(candidates))) {
    rule_id <- candidates$rule_id[[j]]
    original <- candidates$formula_sql[[j]]
    inputs <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT parameter_id, matrix_state_id
       FROM criteria.guideline_rule_inputs
       WHERE rule_id = $1 AND COALESCE(input_source, 'sample_result') = 'sample_result'
         AND parameter_id IS NOT NULL AND matrix_state_id IS NOT NULL
       ORDER BY parameter_id, matrix_state_id",
      params = list(rule_id)
    )
    hits <- gregexpr(
      "(?is)discrete[.]get_sample_val\\s*\\([^()]*\\)",
      original,
      perl = TRUE
    )
    calls <- regmatches(original, hits)[[1]]
    if (!length(calls)) {
      stop("Could not safely parse guideline SQL accessors for rule ", rule_id)
    }
    formula <- original
    for (call in calls) {
      param_match <- regmatches(
        call,
        regexec(
          "(?i)\\bparameter_id\\s*(?::=|=>)\\s*([0-9]+)(?:\\s*::\\s*(?:INT|INTEGER))?\\b",
          call,
          perl = TRUE
        )
      )[[1]]
      if (length(param_match) < 2L) {
        stop(
          "Could not resolve a literal parameter_id in guideline rule ",
          rule_id
        )
      }
      parameter_id <- as.integer(param_match[[2]])
      input <- inputs[inputs$parameter_id == parameter_id, , drop = FALSE]
      if (!nrow(input)) {
        next
      }
      if (nrow(input) != 1L) {
        stop(
          "Ambiguous matrix-state input for parameter ",
          parameter_id,
          " in guideline rule ",
          rule_id
        )
      }
      matrix_state_id <- as.integer(input$matrix_state_id[[1]])
      state_present <- grepl(
        "(?i)\\bmatrix_state_id\\s*(?::=|=>)",
        call,
        perl = TRUE
      )
      if (state_present) {
        state_match <- regmatches(
          call,
          regexec(
            "(?i)\\bmatrix_state_id\\s*(?::=|=>)\\s*([0-9]+)(?:\\s*::\\s*(?:INT|INTEGER))?\\b",
            call,
            perl = TRUE
          )
        )[[1]]
        if (
          length(state_match) < 2L ||
            as.integer(state_match[[2]]) != matrix_state_id
        ) {
          stop(
            "Conflicting matrix_state_id in guideline rule ",
            rule_id,
            " for parameter ",
            parameter_id
          )
        }
        next
      }
      revised <- sub(
        "\\s*\\)$",
        paste0(",\n      matrix_state_id := ", matrix_state_id, "::INTEGER)"),
        call,
        perl = TRUE
      )
      if (identical(revised, call)) {
        stop("Could not add matrix_state_id to guideline rule ", rule_id)
      }
      formula <- sub(call, revised, formula, fixed = TRUE)
      updated <- updated + 1L
      message(
        "Guideline rule ",
        rule_id,
        ": added declared matrix state ",
        matrix_state_id,
        " to parameter ",
        parameter_id,
        "."
      )
    }
    if (!identical(formula, original)) {
      changed <- DBI::dbExecute(
        con,
        "UPDATE criteria.guideline_value_rules
         SET formula_sql = $1
         WHERE rule_id = $2 AND formula_sql IS NOT DISTINCT FROM $3",
        params = list(formula, rule_id, original)
      )
      if (changed != 1L) {
        stop(
          "Guideline rule changed during Patch 66; refusing formula update for rule ",
          rule_id
        )
      }
    }
  }
  message(
    "Updated ",
    updated,
    " guideline sample lookup(s) with declared matrix states."
  )
  invisible(updated)
}

# Patch 65 bounded ECCC method dates to the observed archive years in
# weathercan::stations(). Those archive bounds describe available records, not
# when a calculation recipe is valid. Open the current ECCC recipes while
# retaining real daily-method changes and any explicitly versioned successors.
patch66_eccc_applicability_fix <- function(con) {
  if (!AquaCache:::dbTransCheck(con)) {
    stop("Patch 66 ECCC applicability cleanup requires an active transaction.")
  }
  tables <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('continuous.timeseries_aggregation_methods') IS NOT NULL
         AS methods,
       to_regclass('continuous.timeseries_aggregation_method_steps') IS NOT NULL
         AS steps"
  )
  if (!isTRUE(tables$methods[[1]]) || !isTRUE(tables$steps[[1]])) {
    stop(
      "Patch 66 ECCC applicability cleanup requires the Patch 65 aggregation schema."
    )
  }
  DBI::dbExecute(
    con,
    "LOCK TABLE continuous.timeseries_aggregation_methods,
      continuous.timeseries_aggregation_method_steps
     IN SHARE ROW EXCLUSIVE MODE"
  )
  updated <- DBI::dbGetQuery(
    con,
    "WITH candidates AS (
       SELECT method.aggregation_method_id, method.timeseries_id
       FROM continuous.timeseries_aggregation_methods method
       WHERE method.source_name = 'Environment and Climate Change Canada'
         AND method.applicable_until < 'infinity'::timestamptz
         AND (
           (
             EXISTS (
               SELECT 1
               FROM continuous.timeseries_aggregation_method_steps step
               WHERE step.aggregation_method_id = method.aggregation_method_id
                 AND step.calculation_parameters->>'daily_period' =
                   'From 1961-07-01 ECCC date'
             )
             AND EXTRACT(MONTH FROM method.applicable_until AT TIME ZONE 'UTC') = 1
             AND EXTRACT(DAY FROM method.applicable_until AT TIME ZONE 'UTC') = 2
             AND (method.applicable_until AT TIME ZONE 'UTC')::time = TIME '06:00:00'
           )
           OR (
             EXISTS (
               SELECT 1
               FROM continuous.timeseries_aggregation_method_steps step
               WHERE step.aggregation_method_id = method.aggregation_method_id
                 AND step.calculation_parameters->>'source_interval' = 'hour'
                 AND step.calculation_parameters->>'source_parameter' IN (
                   'wind_spd', 'wind_dir', 'precip_amt', 'wind_gust',
                   'spd_max_gust'
                 )
             )
             AND EXTRACT(MONTH FROM method.applicable_until AT TIME ZONE 'UTC') = 1
             AND EXTRACT(DAY FROM method.applicable_until AT TIME ZONE 'UTC') = 1
             AND (method.applicable_until AT TIME ZONE 'UTC')::time = TIME '00:00:00'
           )
         )
         AND NOT EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_methods successor
           WHERE successor.timeseries_id = method.timeseries_id
             AND successor.applicable_from >= method.applicable_until
         )
     )
     UPDATE continuous.timeseries_aggregation_methods method
     SET applicable_until = 'infinity'::timestamptz
     FROM candidates
     WHERE method.aggregation_method_id = candidates.aggregation_method_id
       AND method.applicable_until < 'infinity'::timestamptz
     RETURNING method.aggregation_method_id, method.timeseries_id"
  )
  if (nrow(updated)) {
    message(
      "Opened ",
      nrow(updated),
      " current ECCC aggregation method(s): ",
      paste(updated$aggregation_method_id, collapse = ", "),
      "."
    )
  } else {
    message(
      "No archive-limited current ECCC aggregation methods required correction."
    )
  }
  invisible(updated)
}
