# patch_65.R: repair Patch 64 parameter updates and field-result metadata,
# separate reporting bases, chemical forms, targets, and numeric conditions,
# preserve explicit endpoints and legacy interpretation, add time-ranged continuous
# aggregation methods, bound ECCC recipes using station archive metadata,
# correct ECCC wind types and periods, and rename the
# timeseries default-show field.

# Keep the one-time chemistry conversion beside this patch. Reusable parameter
# maintenance tools remain in the package namespace.
patch65_helpers <- new.env(parent = asNamespace("AquaCache"))
helper_path <- system.file(
  "patches",
  "patch_65",
  "result_chemistry.R",
  package = "AquaCache"
)
if (!nzchar(helper_path)) {
  stop("Missing Patch 65 chemistry migration helper.")
}
sys.source(helper_path, envir = patch65_helpers)

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on DEV patch 65: revising parameter references; separating reporting bases, chemical forms, analytical targets, endpoints, and analysis conditions; migrating sample fractions; preventing laboratory metadata on field results; correcting ECCC wind aggregation types and windows; adding time-ranged aggregation methods; correcting measurement periods; and renaming the timeseries default-show field. Changes are made within one transaction and errors roll back the patch."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before running this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.import_parameter_mappings') IS NOT NULL
           AS has_parameter_mappings,
         to_regclass('discrete.import_mapping_sets') IS NOT NULL
           AS has_mapping_sets,
         to_regclass('discrete.results') IS NOT NULL AS has_results,
         to_regclass('discrete.result_speciations') IS NOT NULL
           AS has_result_speciations,
         to_regclass('discrete.result_types') IS NOT NULL AS has_result_types,
         to_regclass('public.parameters') IS NOT NULL AS has_parameters,
         to_regclass('information.version_info') IS NOT NULL
           AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "DEV patch 65 requires the Patch 64 mapping tables, discrete results and speciations, result types, public.parameters, and information.version_info."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "64") {
      stop("DEV patch 65 must be applied to a database at Patch 64.")
    }

    # Retain the original ID dictionaries before renaming or deleting anything.
    # Old source payloads refer to both catalogues, not only to descriptors.
    DBI::dbExecute(
      con,
      "CREATE TEMP TABLE patch65_legacy_sample_fractions ON COMMIT DROP AS
       SELECT sample_fraction_id, sample_fraction FROM discrete.sample_fractions"
    )
    DBI::dbExecute(
      con,
      "CREATE TEMP TABLE patch65_legacy_result_speciations ON COMMIT DROP AS
       SELECT result_speciation_id AS legacy_id, result_speciation AS legacy_label
       FROM discrete.result_speciations"
    )

    # The old lookup contains several kinds of result descriptor. Rename the
    # lookup and its referring columns without changing IDs. Then move the
    # operational cyanide fractions out of sample_fractions. When both the old
    # sample fraction and old descriptor were populated, preserve both labels
    # in one descriptor (for example, "weak acid dissociable / as CN").
    descriptor_table_state <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.result_speciations') IS NOT NULL
           AS has_legacy_table,
         to_regclass('discrete.result_descriptors') IS NULL
           AS descriptor_table_name_available,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'parameters'
             AND column_name = 'result_speciation'
         ) AS has_legacy_parameter_requirement,
         NOT EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'parameters'
             AND column_name = 'result_descriptor'
         ) AS descriptor_requirement_name_available"
    )
    if (!all(unlist(descriptor_table_state[1, ], use.names = FALSE))) {
      stop(
        "DEV patch 65 found an unexpected result descriptor schema. It expects the Patch 64 result_speciations table and parameters.result_speciation column, with no result_descriptors/result_descriptor objects yet."
      )
    }

    legacy_descriptor_columns <- DBI::dbGetQuery(
      con,
      "SELECT relation_namespace.nspname AS table_schema,
              relation.relname AS table_name,
              attribute.attname AS column_name
       FROM pg_attribute attribute
       JOIN pg_class relation ON relation.oid = attribute.attrelid
       JOIN pg_namespace relation_namespace
         ON relation_namespace.oid = relation.relnamespace
       WHERE attribute.attnum > 0
         AND NOT attribute.attisdropped
         AND relation.relkind IN ('r', 'p')
         AND relation_namespace.nspname NOT IN ('pg_catalog', 'information_schema')
         AND attribute.attname IN ('result_speciation_id', 'result_speciation')
         AND NOT (
           relation_namespace.nspname = 'discrete'
           AND relation.relname = 'result_speciations'
         )
       ORDER BY relation_namespace.nspname, relation.relname, attribute.attname"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_speciations
       RENAME TO result_descriptors"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_descriptors
       RENAME COLUMN result_speciation_id TO result_descriptor_id"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.result_descriptors
       RENAME COLUMN result_speciation TO result_descriptor"
    )

    # Rename every base-table reference to the old key/value name. This also
    # covers guideline filters and cluster-specific child tables that retain
    # a foreign key to the descriptor lookup.
    for (i in seq_len(nrow(legacy_descriptor_columns))) {
      reference <- legacy_descriptor_columns[i, , drop = FALSE]
      old_column <- reference$column_name[[1]]
      new_column <- if (old_column == "result_speciation_id") {
        "result_descriptor_id"
      } else {
        "result_descriptor"
      }
      table_id <- DBI::Id(
        schema = reference$table_schema[[1]],
        table = reference$table_name[[1]]
      )
      table_sql <- as.character(DBI::dbQuoteIdentifier(con, table_id))
      old_exists <- DBI::dbGetQuery(
        con,
        "SELECT EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = $1 AND table_name = $2 AND column_name = $3
         ) AS found",
        params = list(
          reference$table_schema[[1]],
          reference$table_name[[1]],
          old_column
        )
      )$found[[1]]
      new_exists <- DBI::dbGetQuery(
        con,
        "SELECT EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = $1 AND table_name = $2 AND column_name = $3
         ) AS found",
        params = list(
          reference$table_schema[[1]],
          reference$table_name[[1]],
          new_column
        )
      )$found[[1]]
      if (isTRUE(old_exists) && isTRUE(new_exists)) {
        stop(
          "Both legacy and new result descriptor columns exist on ",
          reference$table_schema[[1]],
          ".",
          reference$table_name[[1]],
          "."
        )
      }
      if (isTRUE(old_exists)) {
        DBI::dbExecute(
          con,
          paste0(
            "ALTER TABLE ",
            table_sql,
            " RENAME COLUMN ",
            as.character(DBI::dbQuoteIdentifier(con, old_column)),
            " TO ",
            as.character(DBI::dbQuoteIdentifier(con, new_column))
          )
        )
      }
    }

    # PostgreSQL tracks view dependencies, but view output names are part of
    # the view interface. Rename any legacy output columns as well.
    legacy_descriptor_views <- DBI::dbGetQuery(
      con,
      "SELECT relation_namespace.nspname AS table_schema,
              relation.relname AS table_name,
              relation.relkind,
              attribute.attname AS column_name
       FROM pg_attribute attribute
       JOIN pg_class relation ON relation.oid = attribute.attrelid
       JOIN pg_namespace relation_namespace
         ON relation_namespace.oid = relation.relnamespace
       WHERE attribute.attnum > 0
         AND NOT attribute.attisdropped
         AND relation.relkind IN ('v', 'm')
         AND relation_namespace.nspname NOT IN ('pg_catalog', 'information_schema')
         AND attribute.attname IN ('result_speciation_id', 'result_speciation')
       ORDER BY relation_namespace.nspname, relation.relname, attribute.attnum"
    )
    for (i in seq_len(nrow(legacy_descriptor_views))) {
      view <- legacy_descriptor_views[i, , drop = FALSE]
      old_column <- view$column_name[[1]]
      new_column <- if (old_column == "result_speciation_id") {
        "result_descriptor_id"
      } else {
        "result_descriptor"
      }
      old_exists <- DBI::dbGetQuery(
        con,
        "SELECT EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = $1 AND table_name = $2 AND column_name = $3
         ) AS found",
        params = list(view$table_schema[[1]], view$table_name[[1]], old_column)
      )$found[[1]]
      new_exists <- DBI::dbGetQuery(
        con,
        "SELECT EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = $1 AND table_name = $2 AND column_name = $3
         ) AS found",
        params = list(view$table_schema[[1]], view$table_name[[1]], new_column)
      )$found[[1]]
      if (isTRUE(old_exists) && isTRUE(new_exists)) {
        stop(
          "Both legacy and new result descriptor outputs exist on ",
          view$table_schema[[1]],
          ".",
          view$table_name[[1]],
          "."
        )
      }
      if (isTRUE(old_exists)) {
        view_id <- DBI::dbQuoteIdentifier(
          con,
          DBI::Id(schema = view$table_schema[[1]], table = view$table_name[[1]])
        )
        view_kind <- if (view$relkind[[1]] == "m") {
          "MATERIALIZED VIEW"
        } else {
          "VIEW"
        }
        DBI::dbExecute(
          con,
          paste0(
            "ALTER ",
            view_kind,
            " ",
            as.character(view_id),
            " RENAME COLUMN ",
            as.character(DBI::dbQuoteIdentifier(con, old_column)),
            " TO ",
            as.character(DBI::dbQuoteIdentifier(con, new_column))
          )
        )
      }
    }

    # Stored SQL and PL/pgSQL bodies are not rewritten when a referenced
    # column is renamed. Update those bodies while preserving each function's
    # signature and return-column names.
    legacy_descriptor_functions <- DBI::dbGetQuery(
      con,
      "SELECT p.prosrc AS function_source,
              pg_get_functiondef(p.oid) AS function_definition
       FROM pg_proc p
       JOIN pg_namespace n ON n.oid = p.pronamespace
       WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
         AND p.prosrc ILIKE '%result_speciation%'"
    )
    for (i in seq_len(nrow(legacy_descriptor_functions))) {
      function_source <- legacy_descriptor_functions$function_source[[i]]
      rewritten_source <- gsub(
        "result_speciations",
        "result_descriptors",
        function_source,
        fixed = TRUE
      )
      rewritten_source <- gsub(
        "result_speciation_id",
        "result_descriptor_id",
        rewritten_source,
        fixed = TRUE
      )
      rewritten_source <- gsub(
        "result_speciation",
        "result_descriptor",
        rewritten_source,
        fixed = TRUE
      )
      if (identical(function_source, rewritten_source)) {
        next
      }
      function_definition <-
        legacy_descriptor_functions$function_definition[[i]]
      body_match <- regexpr(function_source, function_definition, fixed = TRUE)
      if (body_match[[1]] < 1L) {
        stop("Could not locate a stored function body for descriptor rename.")
      }
      body_start <- as.integer(body_match[[1]])
      body_length <- as.integer(attr(body_match, "match.length"))
      function_definition <- paste0(
        substr(function_definition, 1L, body_start - 1L),
        rewritten_source,
        substr(
          function_definition,
          body_start + body_length,
          nchar(function_definition)
        )
      )
      DBI::dbExecute(con, function_definition)
    }

    stale_descriptor_functions <- DBI::dbGetQuery(
      con,
      "SELECT n.nspname AS schema_name, p.proname AS function_name
       FROM pg_proc p
       JOIN pg_namespace n ON n.oid = p.pronamespace
       WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
         AND p.prosrc ILIKE '%result_speciation%'"
    )
    if (nrow(stale_descriptor_functions) > 0L) {
      stop("Stored database functions still reference result_speciation.")
    }

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.result_descriptors IS
       'Single-valued result descriptors, including chemical forms, operational analytical fractions, reporting expressions, and isotope targets. A result can carry one descriptor.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.result_descriptor_id IS
       'Optional descriptor qualifying the result, such as an analytical fraction or reporting expression.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.parameters.result_descriptor IS
       'Whether a descriptor is required for results of this parameter when the parameter-level requirement applies.'"
    )

    # Move the operational cyanide fractions to result descriptors. For other
    # fractions scheduled for deletion, preserve their descriptor on any
    # existing result, mapping, or guideline input that still references them.
    explicitly_moved_sample_fraction_names <- c(
      "weak acid dissociable",
      "strong acid dissociable",
      "free"
    )
    sample_fraction_names_to_delete <- unique(c(
      explicitly_moved_sample_fraction_names,
      "inorganic",
      "unfiltered"
    ))
    sample_fraction_names_to_delete_sql <- paste(
      as.character(DBI::dbQuoteString(con, sample_fraction_names_to_delete)),
      collapse = ", "
    )
    sample_fraction_references <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT key_column.table_schema,
              key_column.table_name,
              key_column.column_name
       FROM information_schema.key_column_usage key_column
       JOIN information_schema.table_constraints table_constraint
         ON table_constraint.constraint_catalog = key_column.constraint_catalog
        AND table_constraint.constraint_schema = key_column.constraint_schema
        AND table_constraint.constraint_name = key_column.constraint_name
        AND table_constraint.table_schema = key_column.table_schema
        AND table_constraint.table_name = key_column.table_name
       JOIN information_schema.constraint_column_usage referenced_column
         ON referenced_column.constraint_catalog = key_column.constraint_catalog
        AND referenced_column.constraint_schema = key_column.constraint_schema
        AND referenced_column.constraint_name = key_column.constraint_name
       WHERE table_constraint.constraint_type = 'FOREIGN KEY'
         AND referenced_column.table_schema = 'discrete'
         AND referenced_column.table_name = 'sample_fractions'
         AND referenced_column.column_name = 'sample_fraction_id'
       ORDER BY key_column.table_schema, key_column.table_name,
                key_column.column_name"
    )
    supported_sample_fraction_references <- c(
      "discrete.results.sample_fraction_id",
      "discrete.import_parameter_mappings.sample_fraction_id",
      "criteria.guideline_rule_inputs.sample_fraction_id"
    )
    supported_migration_tables <- list(
      list(schema = "discrete", table = "results"),
      list(schema = "discrete", table = "import_parameter_mappings"),
      list(schema = "criteria", table = "guideline_rule_inputs")
    )
    # Fractions describe the analyzed/prepared portion of a sample. Only the
    # cyanide pools and arsenic form move into the staging descriptor lookup.
    # Unfiltered is handled independently below as a total sample fraction;
    # neither the fraction nor a manufacturer establishes a cyanide target.
    sample_fraction_descriptor_label_sql <- function(
      source_table,
      fraction_alias = "fraction",
      row_alias = "source_row"
    ) {
      paste0(fraction_alias, ".sample_fraction")
    }
    sample_fraction_descriptor_joins_sql <- function(source_table, row_alias) {
      if (
        source_table$schema == "discrete" &&
          source_table$table %in% c("results", "import_parameter_mappings")
      ) {
        paste0(
          " LEFT JOIN public.parameters parameter",
          " ON parameter.parameter_id = ",
          row_alias,
          ".parameter_id"
        )
      } else {
        ""
      }
    }
    unsupported_sample_fraction_references <- character()
    referenced_sample_fraction_names <- character()
    for (i in seq_len(nrow(sample_fraction_references))) {
      reference <- sample_fraction_references[i, , drop = FALSE]
      reference_sql <- as.character(
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(
            schema = reference$table_schema[[1]],
            table = reference$table_name[[1]]
          )
        )
      )
      column_sql <- as.character(
        DBI::dbQuoteIdentifier(con, reference$column_name[[1]])
      )
      reference_key <- paste(
        reference$table_schema[[1]],
        reference$table_name[[1]],
        reference$column_name[[1]],
        sep = "."
      )
      reference_counts <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT count(*) AS reference_count
           FROM ",
          reference_sql,
          " child
           JOIN discrete.sample_fractions fraction
             ON fraction.sample_fraction_id = child.",
          column_sql,
          " WHERE lower(btrim(fraction.sample_fraction)) IN (",
          sample_fraction_names_to_delete_sql,
          ")"
        )
      )
      if (
        reference_counts$reference_count[[1]] > 0L &&
          !reference_key %in% supported_sample_fraction_references
      ) {
        unsupported_sample_fraction_references <- c(
          unsupported_sample_fraction_references,
          paste0(
            reference_key,
            " (",
            reference_counts$reference_count[[1]],
            ")"
          )
        )
      }
      if (
        reference_counts$reference_count[[1]] > 0L &&
          reference_key %in% supported_sample_fraction_references
      ) {
        referenced_sample_fraction_names <- c(
          referenced_sample_fraction_names,
          tolower(
            DBI::dbGetQuery(
              con,
              paste0(
                "SELECT DISTINCT fraction.sample_fraction
               FROM ",
                reference_sql,
                " child
               JOIN discrete.sample_fractions fraction
                 ON fraction.sample_fraction_id = child.",
                column_sql,
                " WHERE lower(btrim(fraction.sample_fraction)) IN (",
                sample_fraction_names_to_delete_sql,
                ")"
              )
            )$sample_fraction
          )
        )
      }
    }
    if (length(unsupported_sample_fraction_references) > 0L) {
      stop(
        "Cannot migrate or delete sample fractions referenced by unsupported tables: ",
        paste(unsupported_sample_fraction_references, collapse = ", "),
        ". No rows were changed."
      )
    }
    moved_sample_fraction_names <- unique(c(
      explicitly_moved_sample_fraction_names,
      setdiff(referenced_sample_fraction_names, "unfiltered")
    ))
    moved_sample_fraction_sql <- paste(
      as.character(DBI::dbQuoteString(con, moved_sample_fraction_names)),
      collapse = ", "
    )
    moved_descriptor_rows <- list()
    for (source_table in supported_migration_tables) {
      table_exists <- DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1) IS NOT NULL AS found",
        params = list(paste(source_table$schema, source_table$table, sep = "."))
      )$found[[1]]
      if (!isTRUE(table_exists)) {
        next
      }
      source_columns <- DBI::dbGetQuery(
        con,
        "SELECT column_name
         FROM information_schema.columns
         WHERE table_schema = $1 AND table_name = $2",
        params = list(source_table$schema, source_table$table)
      )$column_name
      if (
        !all(
          c("sample_fraction_id", "result_descriptor_id") %in% source_columns
        )
      ) {
        stop(
          "The table ",
          source_table$schema,
          ".",
          source_table$table,
          " cannot preserve both its sample fraction and result descriptor."
        )
      }
      source_sql <- as.character(
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(schema = source_table$schema, table = source_table$table)
        )
      )
      fraction_label_sql <- sample_fraction_descriptor_label_sql(source_table)
      parameter_join_sql <- sample_fraction_descriptor_joins_sql(
        source_table,
        "source_row"
      )
      moved_descriptor_rows[[length(moved_descriptor_rows) + 1L]] <-
        DBI::dbGetQuery(
          con,
          paste0(
            "SELECT DISTINCT ",
            fraction_label_sql,
            " AS sample_fraction,
                    descriptor.result_descriptor
             FROM ",
            source_sql,
            " source_row
             JOIN discrete.sample_fractions fraction
               ON fraction.sample_fraction_id = source_row.sample_fraction_id
             ",
            parameter_join_sql,
            "
             LEFT JOIN discrete.result_descriptors descriptor
               ON descriptor.result_descriptor_id =
                    source_row.result_descriptor_id
             WHERE lower(btrim(fraction.sample_fraction)) IN (",
            moved_sample_fraction_sql,
            ")"
          )
        )
    }
    moved_descriptor_rows <- Filter(
      function(rows) nrow(rows) > 0L,
      moved_descriptor_rows
    )
    sample_fraction_labels <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT sample_fraction
         FROM discrete.sample_fractions
         WHERE lower(btrim(sample_fraction)) IN (",
        paste(
          as.character(DBI::dbQuoteString(
            con,
            explicitly_moved_sample_fraction_names
          )),
          collapse = ", "
        ),
        ")"
      )
    )$sample_fraction
    descriptor_labels_to_add <- sample_fraction_labels
    if (length(moved_descriptor_rows) > 0L) {
      moved_descriptor_rows <- do.call(rbind, moved_descriptor_rows)
      for (i in seq_len(nrow(moved_descriptor_rows))) {
        fraction_label <- moved_descriptor_rows$sample_fraction[[i]]
        existing_label <- moved_descriptor_rows$result_descriptor[[i]]
        combined_label <- if (
          is.na(existing_label) || !nzchar(trimws(existing_label))
        ) {
          fraction_label
        } else if (
          tolower(trimws(existing_label)) == tolower(trimws(fraction_label))
        ) {
          existing_label
        } else {
          paste(fraction_label, existing_label, sep = " / ")
        }
        descriptor_labels_to_add <- c(descriptor_labels_to_add, combined_label)
      }
    }
    descriptor_labels_to_add <- unique(descriptor_labels_to_add)
    for (descriptor_label in descriptor_labels_to_add) {
      DBI::dbExecute(
        con,
        "INSERT INTO discrete.result_descriptors (result_descriptor)
         SELECT $1::text
         WHERE NOT EXISTS (
           SELECT 1 FROM discrete.result_descriptors
           WHERE lower(btrim(result_descriptor)) = lower(btrim($1::text))
         )
         ON CONFLICT DO NOTHING",
        params = list(descriptor_label)
      )
    }

    # A row's result identity includes its physical fraction and descriptor.
    # Detect any key collision before updating so no result is silently lost.
    result_fraction_label_sql <- sample_fraction_descriptor_label_sql(
      list(schema = "discrete", table = "results"),
      fraction_alias = "fraction",
      row_alias = "result"
    )
    result_parameter_join_sql <- sample_fraction_descriptor_joins_sql(
      list(schema = "discrete", table = "results"),
      "result"
    )
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TEMP TABLE patch65_result_descriptor_keys ON COMMIT DROP AS
         SELECT result.result_id,
                result.sample_id,
                result.result_type,
                result.parameter_id,
                result.matrix_state_id,
                CASE
                  WHEN lower(btrim(fraction.sample_fraction)) IN (",
        moved_sample_fraction_sql,
        ") THEN NULL::INTEGER
                  ELSE result.sample_fraction_id
                END AS next_sample_fraction_id,
                result.result_value_type,
                CASE
                  WHEN lower(btrim(fraction.sample_fraction)) IN (",
        moved_sample_fraction_sql,
        ") THEN (
                    SELECT min(target_descriptor.result_descriptor_id)
                    FROM discrete.result_descriptors target_descriptor
                    WHERE lower(btrim(target_descriptor.result_descriptor)) =
                      lower(btrim(
                        CASE
                          WHEN current_descriptor.result_descriptor_id IS NULL
                            THEN ",
        result_fraction_label_sql,
        "
                          WHEN lower(btrim(current_descriptor.result_descriptor)) =
                               lower(btrim(",
        result_fraction_label_sql,
        "))
                            THEN current_descriptor.result_descriptor
                          ELSE ",
        result_fraction_label_sql,
        " || ' / ' ||
                               current_descriptor.result_descriptor
                        END
                      ))
                  )
                  ELSE result.result_descriptor_id
                END AS next_result_descriptor_id,
                result.protocol_method,
                result.laboratory,
                result.analysis_datetime
         FROM discrete.results result
         LEFT JOIN discrete.sample_fractions fraction
           ON fraction.sample_fraction_id = result.sample_fraction_id
         LEFT JOIN discrete.result_descriptors current_descriptor
           ON current_descriptor.result_descriptor_id =
                result.result_descriptor_id",
        result_parameter_join_sql
      )
    )
    unresolved_descriptor_rows <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT count(*) AS n
         FROM patch65_result_descriptor_keys migrated
         JOIN discrete.results result USING (result_id)
         JOIN discrete.sample_fractions fraction
           ON fraction.sample_fraction_id = result.sample_fraction_id
         WHERE lower(btrim(fraction.sample_fraction)) IN (",
        moved_sample_fraction_sql,
        ")
           AND migrated.next_result_descriptor_id IS NULL"
      )
    )$n[[1]]
    if (unresolved_descriptor_rows > 0L) {
      stop("At least one result could not be assigned a migrated descriptor.")
    }
    duplicate_result_keys <- DBI::dbGetQuery(
      con,
      "SELECT sample_id, parameter_id, count(*) AS result_count
       FROM patch65_result_descriptor_keys
       GROUP BY sample_id, result_type, parameter_id, matrix_state_id,
                next_sample_fraction_id, result_value_type,
                next_result_descriptor_id, protocol_method, laboratory,
                analysis_datetime
       HAVING count(*) > 1
       ORDER BY sample_id, parameter_id
       LIMIT 10"
    )
    if (nrow(duplicate_result_keys) > 0L) {
      stop(
        "Moving sample fractions would collide with existing result identities. Review sample/parameter pairs: ",
        paste(
          paste0(
            duplicate_result_keys$sample_id,
            "/",
            duplicate_result_keys$parameter_id
          ),
          collapse = ", "
        )
      )
    }

    # Preserve parameter-level requirements. Old fraction guards cannot express
    # a historical cyanide pool whose physical fraction was never recorded.
    # The typed chemistry guard installed below permits unchanged historical
    # unknowns while enforcing those requirements on new analytical identities.
    patch65_fraction_guards <- DBI::dbGetQuery(
      con,
      "SELECT n.nspname AS schema_name, c.relname AS table_name, t.tgname
       FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid
       JOIN pg_namespace n ON n.oid = c.relnamespace
       JOIN pg_proc p ON p.oid = t.tgfoid
       WHERE NOT t.tgisinternal AND t.tgenabled = 'O'
         AND p.proname IN ('enforce_sample_fraction',
           'enforce_import_parameter_mapping_requirements')
         AND t.tgrelid IN ('discrete.results'::regclass,
           'discrete.import_parameter_mappings'::regclass)"
    )
    for (i in seq_len(nrow(patch65_fraction_guards))) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s.%s DISABLE TRIGGER %s",
          DBI::dbQuoteIdentifier(con, patch65_fraction_guards$schema_name[[i]]),
          DBI::dbQuoteIdentifier(con, patch65_fraction_guards$table_name[[i]]),
          DBI::dbQuoteIdentifier(con, patch65_fraction_guards$tgname[[i]])
        )
      )
    }
    mapping_guard <- DBI::dbGetQuery(
      con,
      "SELECT tgenabled
       FROM pg_trigger
       WHERE tgrelid = 'discrete.import_parameter_mappings'::regclass
         AND tgname = 'guard_import_mapping_row'
         AND NOT tgisinternal"
    )
    if (nrow(mapping_guard) != 1L || mapping_guard$tgenabled[[1]] != "O") {
      stop(
        "The import mapping immutability guard is missing or not enabled; descriptor migration stopped."
      )
    }
    # This is a representation migration: retain every mapping row and mapping
    # set ID while replacing an obsolete fraction reference with its equivalent
    # descriptor. Update historical snapshots only for this schema conversion,
    # then restore the guard before the patch continues.
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
       DISABLE TRIGGER guard_import_mapping_row"
    )

    for (source_table in supported_migration_tables) {
      table_exists <- DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1) IS NOT NULL AS found",
        params = list(paste(source_table$schema, source_table$table, sep = "."))
      )$found[[1]]
      if (!isTRUE(table_exists)) {
        next
      }
      table_sql <- as.character(
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(schema = source_table$schema, table = source_table$table)
        )
      )
      # Retain the physical fraction and existing descriptor independently.
      # A uniqueness/FK conflict aborts the transaction rather than coalescing
      # historically distinct results. No cyanide target is inferred here.
      DBI::dbExecute(
        con,
        paste0(
          "UPDATE ",
          table_sql,
          " target SET sample_fraction_id = (
           SELECT sample_fraction_id FROM discrete.sample_fractions
           WHERE lower(btrim(sample_fraction)) = 'total')
         WHERE sample_fraction_id IN (
           SELECT sample_fraction_id FROM discrete.sample_fractions
           WHERE lower(btrim(sample_fraction)) = 'unfiltered')"
        )
      )
      fraction_label_sql <- sample_fraction_descriptor_label_sql(
        source_table,
        fraction_alias = "fraction",
        row_alias = "target"
      )
      parameter_join_sql <- sample_fraction_descriptor_joins_sql(
        source_table,
        "target"
      )
      rows_moved <- DBI::dbExecute(
        con,
        paste0(
          "UPDATE ",
          table_sql,
          " target
           SET result_descriptor_id = (
             SELECT min(next_descriptor.result_descriptor_id)
             FROM discrete.sample_fractions fraction
             LEFT JOIN discrete.result_descriptors old_descriptor
               ON old_descriptor.result_descriptor_id =
                    target.result_descriptor_id
             ",
          parameter_join_sql,
          "
             JOIN discrete.result_descriptors next_descriptor
               ON lower(btrim(next_descriptor.result_descriptor)) =
                 lower(btrim(
                   CASE
                     WHEN old_descriptor.result_descriptor_id IS NULL
                       THEN ",
          fraction_label_sql,
          "
                     WHEN lower(btrim(old_descriptor.result_descriptor)) =
                          lower(btrim(",
          fraction_label_sql,
          "))
                       THEN old_descriptor.result_descriptor
                     ELSE ",
          fraction_label_sql,
          " || ' / ' ||
                          old_descriptor.result_descriptor
                   END
                 ))
             WHERE fraction.sample_fraction_id = target.sample_fraction_id
               AND lower(btrim(fraction.sample_fraction)) IN (",
          moved_sample_fraction_sql,
          ")
           ),
           sample_fraction_id = NULL
           WHERE sample_fraction_id IN (
             SELECT sample_fraction_id
             FROM discrete.sample_fractions
             WHERE lower(btrim(sample_fraction)) IN (",
          moved_sample_fraction_sql,
          ")
           )"
        )
      )
      message(
        "Moved ",
        rows_moved,
        " ",
        source_table$schema,
        ".",
        source_table$table,
        " row(s) to result descriptors."
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.import_parameter_mappings
       ENABLE TRIGGER guard_import_mapping_row"
    )

    # All referenced terms were migrated from supported tables; the scan above
    # stopped first if any other table held a reference.
    DBI::dbExecute(
      con,
      paste0(
        "DELETE FROM discrete.sample_fractions
         WHERE lower(btrim(sample_fraction)) IN (",
        sample_fraction_names_to_delete_sql,
        ")"
      )
    )

    remaining_sample_fractions <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT sample_fraction
         FROM discrete.sample_fractions
         WHERE lower(btrim(sample_fraction)) IN (",
        sample_fraction_names_to_delete_sql,
        ")"
      )
    )
    if (nrow(remaining_sample_fractions) > 0L) {
      stop(
        "One or more obsolete sample fraction entries remain after migration."
      )
    }

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       RENAME COLUMN publicly_visible TO default_show"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.default_show IS
       'Controls whether this timeseries appears in default public timeseries listings and views. It does not control whether users are allowed to access the timeseries.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.enforce_parameter_import_mapping_requirements()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, discrete
       AS $function$
       BEGIN
         IF NEW.sample_fraction IS TRUE
            AND OLD.sample_fraction IS DISTINCT FROM TRUE
            AND EXISTS (
             SELECT 1
             FROM discrete.import_parameter_mappings mapping
             JOIN discrete.import_mapping_sets mapping_set
               USING (import_mapping_set_id)
             WHERE mapping.parameter_id = NEW.parameter_id
               AND mapping.active IS TRUE
               AND mapping_set.status IN ('draft', 'published')
               AND mapping.sample_fraction_id IS NULL
            ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'parameters_import_mapping_required_descriptors',
             MESSAGE = 'Cannot require a sample fraction while an import mapping for this parameter has no sample fraction.';
         END IF;

         IF NEW.result_descriptor IS TRUE
            AND OLD.result_descriptor IS DISTINCT FROM TRUE
            AND EXISTS (
              SELECT 1
              FROM discrete.import_parameter_mappings mapping
              JOIN discrete.import_mapping_sets mapping_set
                USING (import_mapping_set_id)
              WHERE mapping.parameter_id = NEW.parameter_id
                AND mapping.active IS TRUE
                AND mapping_set.status IN ('draft', 'published')
                AND mapping.result_descriptor_id IS NULL
            ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'parameters_import_mapping_required_descriptors',
             MESSAGE = 'Cannot require a descriptor while an import mapping for this parameter has no descriptor.';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    result_columns <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'results'
             AND column_name = 'result_type'
         ) AS has_result_type,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'results'
             AND column_name = 'laboratory'
         ) AS has_laboratory,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'results'
             AND column_name = 'lab_report_no'
         ) AS has_lab_report_no,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'discrete'
             AND table_name = 'results'
             AND column_name = 'lab_sample_no'
         ) AS has_lab_sample_no"
    )
    if (!all(unlist(result_columns[1, ], use.names = FALSE))) {
      stop(
        "DEV patch 65 requires discrete.results result_type, laboratory, lab_report_no, and lab_sample_no columns."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.enforce_field_results_without_laboratory()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, discrete
       AS $function$
       DECLARE
         result_type_name text;
       BEGIN
         SELECT lower(btrim(result_type))
           INTO result_type_name
           FROM discrete.result_types
          WHERE result_type_id = NEW.result_type;

         IF result_type_name = 'field'
            AND (
              NEW.laboratory IS NOT NULL
              OR NULLIF(btrim(NEW.lab_report_no), '') IS NOT NULL
              OR NULLIF(btrim(NEW.lab_sample_no), '') IS NOT NULL
            ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'results_field_without_laboratory_metadata',
             MESSAGE = 'Field results cannot include laboratory metadata.';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS results_field_without_laboratory_metadata
       ON discrete.results"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER results_field_without_laboratory_metadata
       BEFORE INSERT OR UPDATE OF result_type, laboratory, lab_report_no, lab_sample_no
       ON discrete.results
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_field_results_without_laboratory()"
    )

    parameter_function <- DBI::dbGetQuery(
      con,
      "SELECT prosrc
       FROM pg_proc
       WHERE oid = to_regprocedure(
         'public.enforce_parameter_import_mapping_requirements()'
       )"
    )
    if (
      nrow(parameter_function) != 1L ||
        !grepl(
          "OLD.sample_fraction IS DISTINCT FROM TRUE",
          parameter_function$prosrc[[1]],
          fixed = TRUE
        ) ||
        !grepl(
          "OLD.result_descriptor IS DISTINCT FROM TRUE",
          parameter_function$prosrc[[1]],
          fixed = TRUE
        )
    ) {
      stop("DEV patch 65 parameter-trigger function verification failed.")
    }

    verified <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure(
           'discrete.enforce_field_results_without_laboratory()'
         ) IS NOT NULL AS function_exists,
         EXISTS (
           SELECT 1
           FROM pg_trigger
           WHERE tgrelid = 'discrete.results'::regclass
             AND tgname = 'results_field_without_laboratory_metadata'
             AND NOT tgisinternal
         ) AS trigger_exists"
    )
    if (!all(unlist(verified[1, ], use.names = FALSE))) {
      stop("DEV patch 65 verification failed.")
    }

    existing_field_metadata <- DBI::dbGetQuery(
      con,
      "SELECT count(*) AS invalid_count
       FROM discrete.results result
       JOIN discrete.result_types result_type
         ON result_type.result_type_id = result.result_type
       WHERE lower(btrim(result_type.result_type)) = 'field'
         AND (
           result.laboratory IS NOT NULL
           OR NULLIF(btrim(result.lab_report_no), '') IS NOT NULL
           OR NULLIF(btrim(result.lab_sample_no), '') IS NOT NULL
         )"
    )$invalid_count[[1]]
    if (existing_field_metadata > 0L) {
      message(
        "DEV patch 65 preserved ",
        existing_field_metadata,
        " existing field result(s) with laboratory metadata. Review those rows; new or modified field results cannot include laboratory metadata."
      )
    }

    # Reviewed parameter catalogue migrations also rewrite historical mapping
    # snapshots and result references. The helper requires this patch's active
    # transaction and preserves distinct sources unless deletion is requested.
    catalogue_revisions <- list(
      list(
        source_name = "well depth",
        target_name = "water level below top of casing",
        source_description = "Well Depth in Meter below TOC",
        param_name_fr = "niveau d'eau sous le haut du tubage",
        description = "Water level measured below the top of the well casing.",
        description_fr = "Niveau d'eau mesuré sous le haut du tubage du puits.",
        plot_default_y_orientation = "inverted",
        result_descriptor = FALSE,
        delete_source = TRUE
      ),
      list(
        source_name = "turbidity",
        target_name = "turbidity",
        param_name_fr = "turbidité",
        result_descriptor = TRUE,
        mapping_descriptor = "as NTU",
        mapping_descriptor_by = c(NTU = "as NTU", FNU = "as FNU"),
        preserve_existing_descriptor = TRUE
      ),
      list(
        source_name = "Turbidity - FNU",
        target_name = "turbidity",
        mapping_descriptor = "as FNU",
        delete_source = TRUE
      ),
      list(
        source_name = "arsenic",
        target_name = "arsenic",
        param_name_fr = "arsenic"
      ),
      list(
        source_name = "As-V",
        target_name = "arsenic",
        mapping_descriptor = "as arsenate (As-V)",
        delete_source = TRUE
      ),
      list(
        source_name = "As-III",
        target_name = "arsenic",
        mapping_descriptor = "as arsenite (As-III)",
        delete_source = TRUE
      ),
      list(
        source_name = "arsenite",
        target_name = "arsenic",
        mapping_descriptor = "as arsenite (As-III)",
        delete_source = TRUE
      )
    )
    if (
      DBI::dbGetQuery(
        con,
        "SELECT count(*)::INTEGER AS n FROM discrete.sample_fractions
       WHERE lower(btrim(sample_fraction)) = 'total'"
      )$n[[1]] !=
        1L
    ) {
      stop("Patch 65 requires exactly one total sample fraction.")
    }
    for (revision in catalogue_revisions) {
      source_exists <- DBI::dbGetQuery(
        con,
        "SELECT EXISTS (SELECT 1 FROM public.parameters
         WHERE lower(btrim(param_name)) = lower(btrim($1))) AS found",
        params = list(revision$source_name)
      )$found[[1]]
      if (!source_exists) {
        next
      }
      # A missing source is a normal cluster difference. Ambiguous names,
      # incompatible definitions, and migration conflicts must roll back.
      do.call(
        AquaCache:::revise_parameter_catalogue_entry,
        c(list(con = con), revision)
      )
    }
    # Keep known parameter/descriptor pairings as ordered UI suggestions.
    # This relation is advisory: unlisted descriptors remain selectable and
    # the parameter result_descriptor flag continues to control requiredness.
    DBI::dbExecute(
      con,
      "CREATE TABLE public.parameter_descriptor_suggestions (
         parameter_id INTEGER NOT NULL
           REFERENCES public.parameters(parameter_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         result_descriptor_id INTEGER NOT NULL
           REFERENCES discrete.result_descriptors(result_descriptor_id)
           ON UPDATE CASCADE ON DELETE CASCADE,
         display_order INTEGER NOT NULL DEFAULT 100
           CHECK (display_order > 0),
         PRIMARY KEY (parameter_id, result_descriptor_id)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameter_descriptor_suggestions OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE public.parameter_descriptor_suggestions
       TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.parameter_descriptor_suggestions IS
       'Advisory parameter-to-descriptor associations for ordering UI choices. Missing rows mean no suggestion is recorded; they do not make an unlisted descriptor invalid.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.parameter_descriptor_suggestions.display_order IS
       'Suggested display position; lower positive values appear first. This order does not enforce compatibility.'"
    )

    # Common nutrient reporting alternatives follow EPA WQX guidance. Resolve
    # parameter names at runtime rather than assuming IDs are shared across
    # clusters; absent parameter names simply contribute no seed rows.
    DBI::dbExecute(
      con,
      "WITH seed_candidates (
         param_name, result_descriptor, seed_order
       ) AS (
         VALUES
           ('alkalinity'::text, 'as CaCO3'::text, 1::integer),
           ('alkalinity'::text, 'as HCO3'::text, 2::integer),
           ('alkalinity'::text, 'as CO3'::text, 3::integer),
           ('alkalinity'::text, 'as CO3 & OH'::text, 4::integer),
           ('ammonia (NH3)'::text, 'as N'::text, 1::integer),
           ('ammonia (NH3)'::text, 'as NH3'::text, 2::integer),
           ('ammonium (NH4+)'::text, 'as N'::text, 1::integer),
           ('ammonium (NH4+)'::text, 'as NH4'::text, 2::integer),
           ('ammonia (NH3) and ammonium (NH4+)'::text, 'as N'::text, 1::integer),
           ('arsenic'::text, 'as arsenic'::text, 1::integer),
           ('arsenic'::text, 'as arsenate'::text, 2::integer),
           ('arsenic'::text, 'as arsenite'::text, 3::integer),
           ('boron'::text, 'as B'::text, 1::integer),
           ('calcium'::text, 'as Ca'::text, 1::integer),
           ('calcium carbonate'::text, 'as CaCO3'::text, 1::integer),
           ('carbon-13'::text, 'of Inorganic C'::text, 1::integer),
           ('carbon-13'::text, 'of Organic C'::text, 2::integer),
           ('carbon-13'::text, 'of CO2'::text, 3::integer),
           ('carbon-13/carbon-12 ratio'::text, 'of Inorganic C'::text, 1::integer),
           ('carbon-13/carbon-12 ratio'::text, 'of Organic C'::text, 2::integer),
           ('carbon-13/carbon-12 ratio'::text, 'of CO2'::text, 3::integer),
           ('chloride'::text, 'as Cl'::text, 1::integer),
           ('cyanide'::text, 'as CN'::text, 1::integer),
           ('cyanide + thiocyanate'::text, 'as CN'::text, 1::integer),
           ('delta carbon-13/carbon-12'::text, 'of Inorganic C'::text, 1::integer),
           ('delta carbon-13/carbon-12'::text, 'of Organic C'::text, 2::integer),
           ('delta carbon-13/carbon-12'::text, 'of CO2'::text, 3::integer),
           ('delta nitrogen-15/nitrogen-14'::text, 'of Organic N'::text, 1::integer),
           ('delta nitrogen-15/nitrogen-14'::text, 'of NO3'::text, 2::integer),
           ('delta nitrogen-15/nitrogen-14'::text, 'of NH4'::text, 3::integer),
           ('delta oxygen-17/oxygen-16'::text, 'of H2O'::text, 1::integer),
           ('delta oxygen-18/oxygen-16'::text, 'of H2O'::text, 1::integer),
           ('delta sulfur-34/sulfur-32'::text, 'of S'::text, 1::integer),
           ('delta sulfur-34/sulfur-32'::text, 'of SO4'::text, 2::integer),
           ('delta sulfur-34/sulfur-32'::text, 'of H2S'::text, 3::integer),
           ('deuterium, delta'::text, 'of H2O'::text, 1::integer),
           ('deuterium/hydrogen ratio'::text, 'of H2O'::text, 1::integer),
           ('dissolved metals'::text, 'as dissolved metals'::text, 1::integer),
           ('dissolved oxygen'::text, 'as O2'::text, 1::integer),
           ('oxygen, dissolved'::text, 'as O2'::text, 1::integer),
           ('biochemical oxygen demand'::text, 'as O2'::text, 1::integer),
           ('chemical oxygen demand'::text, 'as O2'::text, 1::integer),
           ('carbonaceous biochemical oxygen demand'::text, 'as O2'::text, 1::integer),
           ('fluoride'::text, 'as F'::text, 1::integer),
           ('hardness'::text, 'as CaCO3'::text, 1::integer),
           ('hydrogen sulfide'::text, 'as H2S'::text, 1::integer),
           ('hydrogen sulfide'::text, 'as S'::text, 2::integer),
           ('magnesium'::text, 'as Mg'::text, 1::integer),
           ('nitrate'::text, 'as N'::text, 1::integer),
           ('nitrate'::text, 'as NO3'::text, 2::integer),
           ('nitrite'::text, 'as N'::text, 1::integer),
           ('nitrite'::text, 'as NO2'::text, 2::integer),
           ('nitrogen inorganic (ammonia, nitrate, nitrite)'::text, 'as N'::text, 1::integer),
           ('nitrogen inorganic (nitrate and nitrite)'::text, 'as N'::text, 1::integer),
           ('nitrogen, Kjeldahl'::text, 'as N'::text, 1::integer),
           ('nitrogen, organic'::text, 'as N'::text, 1::integer),
           ('nitrogen, undefined'::text, 'as N'::text, 1::integer),
           ('nitrogen, total'::text, 'as N'::text, 1::integer),
           ('nitrogen total'::text, 'as N'::text, 1::integer),
           ('nitrogen, total, in sediment_%'::text, 'as N'::text, 1::integer),
           ('total nitrogen, mixed forms'::text, 'as N'::text, 1::integer),
           ('nitrogen-15'::text, 'of Organic N'::text, 1::integer),
           ('nitrogen-15'::text, 'of NO3'::text, 2::integer),
           ('nitrogen-15'::text, 'of NH4'::text, 3::integer),
           ('nitrogen-15/nitrogen-14 ratio'::text, 'of Organic N'::text, 1::integer),
           ('nitrogen-15/nitrogen-14 ratio'::text, 'of NO3'::text, 2::integer),
           ('nitrogen-15/nitrogen-14 ratio'::text, 'of NH4'::text, 3::integer),
           ('orthophosphate'::text, 'as P'::text, 1::integer),
           ('orthophosphate'::text, 'as PO4'::text, 2::integer),
           ('organic phosphorus'::text, 'as P'::text, 1::integer),
           ('phosphorus, hydrolyzable'::text, 'as P'::text, 1::integer),
           ('phosphorus, elemental'::text, 'as P'::text, 1::integer),
           ('total phosphorus, mixed forms'::text, 'as P'::text, 1::integer),
           ('total phosphorus, mixed forms'::text, 'as PO4'::text, 2::integer),
           ('soluble reactive phosphorus (SRP)'::text, 'as P'::text, 1::integer),
           ('soluble reactive phosphorus (SRP)'::text, 'as PO4'::text, 2::integer),
           ('salinity'::text, 'as NaCL'::text, 1::integer),
           ('selenium'::text, 'as Se'::text, 1::integer),
           ('selenate'::text, 'as Se'::text, 1::integer),
           ('selenite'::text, 'as Se'::text, 1::integer),
           ('selenocyanate'::text, 'as Se'::text, 1::integer),
           ('selenomethionine'::text, 'as Se'::text, 1::integer),
           ('selenosulfate'::text, 'as Se'::text, 1::integer),
           ('silica'::text, 'as SiO2'::text, 1::integer),
           ('silica'::text, 'as Si'::text, 2::integer),
           ('silica, reactive'::text, 'as SiO2'::text, 1::integer),
           ('silica, reactive'::text, 'as Si'::text, 2::integer),
           ('silicate'::text, 'as SiO2'::text, 1::integer),
           ('silicate'::text, 'as Si'::text, 2::integer),
           ('sodium'::text, 'as Na'::text, 1::integer),
           ('sulfate'::text, 'as SO4'::text, 1::integer),
           ('sulfate'::text, 'as S'::text, 2::integer),
           ('sulfide'::text, 'as H2S'::text, 1::integer),
           ('sulfide'::text, 'as S'::text, 2::integer),
           ('sulfite'::text, 'as SO3'::text, 1::integer),
           ('sulfite'::text, 'as S'::text, 2::integer),
           ('sulfur'::text, 'as S'::text, 1::integer),
           ('sulfur total'::text, 'as S'::text, 1::integer),
           ('sulfur-34'::text, 'of S'::text, 1::integer),
           ('sulfur-34'::text, 'of SO4'::text, 2::integer),
           ('sulfur-34'::text, 'of H2S'::text, 3::integer),
           ('sulfur-34/sulfur-32 ratio'::text, 'of S'::text, 1::integer),
           ('sulfur-34/sulfur-32 ratio'::text, 'of SO4'::text, 2::integer),
           ('sulfur-34/sulfur-32 ratio'::text, 'of H2S'::text, 3::integer),
           ('thiosulfate'::text, 'as S'::text, 1::integer),
           ('total metals'::text, 'as total metals'::text, 1::integer),
           ('tritium'::text, 'of H2O'::text, 1::integer),
           ('turbidity'::text, 'as NTU'::text, 1::integer),
           ('turbidity'::text, 'as FNU'::text, 2::integer),
           ('oxygen-18'::text, 'of H2O'::text, 1::integer),
           ('oxygen-18/oxygen-16 ratio'::text, 'of H2O'::text, 1::integer)
       ),
       candidate_evidence AS (
         SELECT result.parameter_id,
                result.result_descriptor_id,
                count(*)::bigint AS result_count,
                0::bigint AS mapping_count,
                NULL::integer AS seed_order
         FROM discrete.results result
         WHERE result.result_descriptor_id IS NOT NULL
         GROUP BY result.parameter_id, result.result_descriptor_id

         UNION ALL

         SELECT mapping.parameter_id,
                mapping.result_descriptor_id,
                0::bigint AS result_count,
                count(*)::bigint AS mapping_count,
                NULL::integer AS seed_order
         FROM discrete.import_parameter_mappings mapping
         JOIN discrete.import_mapping_sets mapping_set
           USING (import_mapping_set_id)
         WHERE mapping.active IS TRUE
           AND mapping_set.status = 'published'
           AND mapping.result_descriptor_id IS NOT NULL
         GROUP BY mapping.parameter_id, mapping.result_descriptor_id

         UNION ALL

         SELECT parameter.parameter_id,
                descriptor.result_descriptor_id,
                0::bigint AS result_count,
                0::bigint AS mapping_count,
                seed.seed_order
         FROM seed_candidates seed
         JOIN public.parameters parameter
           ON lower(btrim(parameter.param_name)) = lower(seed.param_name)
         JOIN discrete.result_descriptors descriptor
           ON descriptor.result_descriptor = seed.result_descriptor
       ),
       candidate_totals AS (
         SELECT parameter_id,
                result_descriptor_id,
                sum(result_count)::bigint AS result_count,
                sum(mapping_count)::bigint AS mapping_count,
                min(seed_order) AS seed_order
         FROM candidate_evidence
         GROUP BY parameter_id, result_descriptor_id
       ),
       ranked_candidates AS (
         SELECT parameter_id,
                result_descriptor_id,
                row_number() OVER (
                  PARTITION BY parameter_id
                  ORDER BY seed_order NULLS LAST,
                           result_count DESC,
                           mapping_count DESC,
                           result_descriptor_id
                )::integer AS display_order
         FROM candidate_totals
       )
       INSERT INTO public.parameter_descriptor_suggestions (
         parameter_id, result_descriptor_id, display_order
       )
       SELECT parameter_id, result_descriptor_id, display_order
       FROM ranked_candidates
       ORDER BY parameter_id, display_order
       ON CONFLICT (parameter_id, result_descriptor_id)
       DO UPDATE SET display_order = EXCLUDED.display_order"
    )

    suggestions_created <- DBI::dbGetQuery(
      con,
      "SELECT count(*) AS suggestion_count
       FROM public.parameter_descriptor_suggestions"
    )$suggestion_count[[1]]
    message(
      "DEV patch 65 recorded ",
      suggestions_created,
      " advisory parameter/descriptor suggestion(s)."
    )

    # The preceding steps preserve legacy labels in a transaction-local staging
    # catalogue. Finish the representation migration with independent reporting
    # bases, chemical speciations, and analytical targets before committing.
    # Offer an explicit all-species scope without assigning it to historical
    # arsenic results whose analytical scope was never recorded.
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.result_descriptors (result_descriptor)
       SELECT 'as arsenic (all states)' WHERE NOT EXISTS (
         SELECT 1 FROM discrete.result_descriptors
         WHERE lower(btrim(result_descriptor)) = 'as arsenic (all states)')"
    )
    patch65_helpers$migrate_result_chemistry_dimensions(con)

    # Catalogue policy may expose incomplete historical mapping definitions.
    # Keep their identities and report the exact review work; an unknown
    # endpoint or chemical scope must never be replaced by a guessed default.
    mapping_requirements_to_review <- DBI::dbGetQuery(
      con,
      "SELECT m.import_mapping_id, p.param_name, array_to_string(array_remove(ARRAY[
         CASE WHEN p.sample_fraction AND m.sample_fraction_id IS NULL THEN 'sample_fraction' END,
         CASE WHEN p.reporting_basis AND m.reporting_basis_id IS NULL THEN 'reporting_basis' END,
         CASE WHEN p.result_speciation AND m.result_speciation_id IS NULL THEN 'result_speciation' END,
         CASE WHEN p.result_target AND m.result_target_id IS NULL THEN 'result_target' END,
         CASE WHEN p.endpoint AND m.endpoint_mode IS NULL THEN 'endpoint' END
       ], NULL), ', ') AS missing_fields
       FROM discrete.import_parameter_mappings m
       JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
       JOIN public.parameters p USING (parameter_id)
       WHERE m.active AND s.status = 'published' AND (
         (p.sample_fraction AND m.sample_fraction_id IS NULL) OR
         (p.reporting_basis AND m.reporting_basis_id IS NULL) OR
         (p.result_speciation AND m.result_speciation_id IS NULL) OR
         (p.result_target AND m.result_target_id IS NULL) OR
         (p.endpoint AND m.endpoint_mode IS NULL)) ORDER BY p.param_name, m.import_mapping_id"
    )
    if (nrow(mapping_requirements_to_review)) {
      message(
        "Published mappings whose result rows will be skipped until analytical review: ",
        paste(
          sprintf(
            "%s (%s: %s)",
            mapping_requirements_to_review$import_mapping_id,
            mapping_requirements_to_review$param_name,
            mapping_requirements_to_review$missing_fields
          ),
          collapse = "; "
        )
      )
    }

    # These IDs are used by the field visit parameter group below.
    DBI::dbExecute(
      con,
      "UPDATE public.units
       SET unit_name = 'NTU/FNU'
       WHERE unit_name = 'NTU'"
    )

    # Create a parameter group for field visit parameters, which will be used by the AquaField application.

    field_group_name <- "field"
    field_parameter_names <- c(
      "pH",
      "specific conductance",
      "conductivity",
      "oxidation reduction potential (ORP)",
      "turbidity",
      "temperature, water",
      "temperature, air",
      "water flow",
      "snow depth",
      "snow water equivalent",
      "water level below top of casing",
      "water level below ground surface",
      "water level",
      "hardness"
    )

    field_group <- DBI::dbGetQuery(
      con,
      "SELECT group_id
   FROM public.parameter_groups
   WHERE group_name = $1::text
   FOR UPDATE",
      params = list(field_group_name)
    )

    if (nrow(field_group) > 1L) {
      stop("More than one parameter group named 'field' exists.")
    }

    if (nrow(field_group) == 0L) {
      field_group <- DBI::dbGetQuery(
        con,
        "INSERT INTO public.parameter_groups
       (group_name, group_name_fr, description, description_fr)
     VALUES ($1::text, $2::text, $3::text, $4::text)
     RETURNING group_id",
        params = list(
          "field",
          "terrain",
          "Parameters which are commonly recorded during field visits.",
          "Paramètres habituellement mesurés pendant les visites sur le terrain."
        )
      )
    }

    field_group_id <- as.integer(field_group$group_id[[1]])

    placeholders <- paste0(
      "$",
      seq_along(field_parameter_names),
      "::text",
      collapse = ", "
    )
    field_parameters <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT parameter_id, param_name
     FROM public.parameters
     WHERE param_name IN (",
        placeholders,
        ")
     ORDER BY param_name"
      ),
      params = as.list(field_parameter_names)
    )

    missing <- setdiff(field_parameter_names, field_parameters$param_name)
    if (
      length(missing) > 0L ||
        anyDuplicated(field_parameters$param_name) ||
        nrow(field_parameters) != length(field_parameter_names)
    ) {
      stop(
        "Field parameters are missing or duplicated: ",
        paste(missing, collapse = ", ")
      )
    }

    for (parameter_id in field_parameters$parameter_id) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.parameter_relationships (parameter_id, group_id)
     SELECT $1::integer, $2::integer
     WHERE NOT EXISTS (
       SELECT 1
       FROM public.parameter_relationships
       WHERE parameter_id = $1::integer
         AND group_id = $2::integer
     )",
        params = list(as.integer(parameter_id), field_group_id)
      )
    }

    # Continuous aggregation methods ###############################
    # `measurements_continuous.period` cannot describe the calculation
    # behind an output (or multiple calculation stages). Keep that legacy
    # per-observation column during the package migration, and store the
    # source methodology separately as a time-ranged, ordered recipe.
    continuous_objects <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.measurements_continuous') IS NOT NULL
           AS has_measurements,
         to_regclass('continuous.aggregation_types') IS NOT NULL
           AS has_aggregation_types,
         to_regclass('continuous.timeseries_source_adapters') IS NOT NULL
           AS has_source_adapters,
         to_regclass('audit.table_registry') IS NOT NULL AS has_audit_registry,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL
           AS has_audit_function,
         to_regprocedure('public.update_modified()') IS NOT NULL
           AS has_modified_function,
         to_regprocedure('public.user_modified()') IS NOT NULL
           AS has_modified_by_function,
         to_regprocedure('continuous.refresh_calculated_daily_from_ranges()') IS NOT NULL
           AS has_daily_range_refresh,
         EXISTS (
           SELECT 1 FROM pg_extension WHERE extname = 'btree_gist'
         ) AS has_btree_gist"
    )
    if (!all(unlist(continuous_objects[1, ], use.names = FALSE))) {
      stop(
        "DEV patch 65 aggregation-method changes require the continuous timeseries, measurements, aggregation types, source adapters, btree_gist, and standard audit/modified objects."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_aggregation_methods (
         aggregation_method_id BIGINT
           GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         applicable_from TIMESTAMPTZ NOT NULL
           DEFAULT '-infinity'::TIMESTAMPTZ,
         applicable_until TIMESTAMPTZ NOT NULL
           DEFAULT 'infinity'::TIMESTAMPTZ,
          source_name TEXT,
          source_reference TEXT,
          note TEXT,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
          CONSTRAINT timeseries_aggregation_method_period_valid
           CHECK (applicable_from < applicable_until),
         CONSTRAINT timeseries_aggregation_method_no_overlap
           EXCLUDE USING gist (
             timeseries_id WITH =,
             tstzrange(applicable_from, applicable_until, '[)') WITH &&
           )
           DEFERRABLE INITIALLY IMMEDIATE
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_aggregation_methods OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_aggregation_methods
       TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_aggregation_methods
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_timeseries_visibility
       ON continuous.timeseries_aggregation_methods
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries ts
           WHERE ts.timeseries_id =
             timeseries_aggregation_methods.timeseries_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries ts
           WHERE ts.timeseries_id =
             timeseries_aggregation_methods.timeseries_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_aggregation_methods IS
       'Versioned calculation methodology for a continuous timeseries, represented by its calculation operations, input relationships, and provenance fields. Applicability is half-open [applicable_from, applicable_until); non-overlapping rows allow methodology changes while preserving the timeseries final aggregation type. Insert the method, operations, and input relationships in one transaction.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_methods.applicable_until IS
       'Exclusive end of applicability. Use infinity for a method that remains current.'"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_aggregation_method_steps (
         step_id BIGINT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
         aggregation_method_id BIGINT NOT NULL
           REFERENCES continuous.timeseries_aggregation_methods(
             aggregation_method_id
           ) ON DELETE CASCADE ON UPDATE CASCADE,
         stage_number SMALLINT NOT NULL,
         operation_type TEXT NOT NULL DEFAULT 'window_aggregate'
           CHECK (operation_type IN ('window_aggregate', 'arithmetic_mean')),
         aggregation_type_id INTEGER NOT NULL
           REFERENCES continuous.aggregation_types(aggregation_type_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         window_start_offset INTERVAL,
         window_end_offset INTERVAL,
         evaluation_interval INTERVAL,
         evaluation_anchor TIMESTAMP WITHOUT TIME ZONE,
         evaluation_timezone TEXT,
         expected_input_interval INTERVAL,
         input_weighting TEXT NOT NULL DEFAULT 'equal'
           CHECK (input_weighting IN ('equal', 'duration_weighted')),
         minimum_input_count INTEGER,
         minimum_coverage NUMERIC(5, 4),
         missing_data_policy TEXT NOT NULL DEFAULT 'ignore'
           CHECK (missing_data_policy IN (
             'ignore', 'propagate', 'require_complete'
           )),
         calculation_parameters JSONB NOT NULL DEFAULT '{}'::JSONB,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT timeseries_aggregation_method_step_method_id_unique
           UNIQUE (aggregation_method_id, step_id),
         CONSTRAINT timeseries_aggregation_method_stage_number_positive
           CHECK (stage_number > 0),
         CONSTRAINT timeseries_aggregation_method_operation_fields_valid
           CHECK (
             (
               operation_type = 'window_aggregate'
               AND window_start_offset IS NOT NULL
               AND window_end_offset IS NOT NULL
               AND window_start_offset < window_end_offset
             )
             OR (
               operation_type = 'arithmetic_mean'
               AND window_start_offset IS NULL
               AND window_end_offset IS NULL
               AND evaluation_interval IS NULL
               AND evaluation_anchor IS NULL
               AND evaluation_timezone IS NULL
               AND expected_input_interval IS NULL
               AND input_weighting = 'equal'
               AND minimum_coverage IS NULL
             )
           ),
         CONSTRAINT timeseries_aggregation_method_evaluation_valid
           CHECK (
             (evaluation_interval IS NULL AND evaluation_anchor IS NULL)
             OR (
               evaluation_interval IS NOT NULL
               AND evaluation_interval > INTERVAL '0'
               AND evaluation_anchor IS NOT NULL
             )
           ),
         CONSTRAINT timeseries_aggregation_method_input_interval_valid
           CHECK (
             expected_input_interval IS NULL
             OR expected_input_interval > INTERVAL '0'
           ),
         CONSTRAINT timeseries_aggregation_method_minimum_count_valid
           CHECK (minimum_input_count IS NULL OR minimum_input_count > 0),
         CONSTRAINT timeseries_aggregation_method_coverage_valid
           CHECK (
             minimum_coverage IS NULL
             OR minimum_coverage BETWEEN 0 AND 1
           ),
         CONSTRAINT timeseries_aggregation_method_complete_policy_valid
           CHECK (
             missing_data_policy <> 'require_complete'
             OR expected_input_interval IS NOT NULL
           ),
         CONSTRAINT timeseries_aggregation_method_parameters_object
           CHECK (jsonb_typeof(calculation_parameters) = 'object'),
          CONSTRAINT timeseries_aggregation_method_timezone_not_blank
            CHECK (
              evaluation_timezone IS NULL
              OR btrim(evaluation_timezone) <> ''
            ),
         CONSTRAINT timeseries_aggregation_method_calendar_timezone
           CHECK (
             operation_type <> 'window_aggregate'
             OR
             evaluation_timezone IS NOT NULL
             OR (
                EXTRACT(YEAR FROM COALESCE(
                  evaluation_interval, INTERVAL '0'
                )) = 0
                AND EXTRACT(MONTH FROM COALESCE(
                  evaluation_interval, INTERVAL '0'
                )) = 0
               AND EXTRACT(DAY FROM COALESCE(
                  evaluation_interval, INTERVAL '0'
                )) = 0
                AND EXTRACT(YEAR FROM COALESCE(
                  window_start_offset, INTERVAL '0'
                )) = 0
                AND EXTRACT(MONTH FROM COALESCE(
                  window_start_offset, INTERVAL '0'
                )) = 0
                AND EXTRACT(DAY FROM COALESCE(
                  window_start_offset, INTERVAL '0'
                )) = 0
                AND EXTRACT(YEAR FROM COALESCE(
                  window_end_offset, INTERVAL '0'
                )) = 0
                AND EXTRACT(MONTH FROM COALESCE(
                  window_end_offset, INTERVAL '0'
                )) = 0
                AND EXTRACT(DAY FROM COALESCE(
                  window_end_offset, INTERVAL '0'
                )) = 0
              )
            )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_aggregation_method_steps OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_aggregation_method_steps
       TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_aggregation_method_steps
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_aggregation_method_visibility
       ON continuous.timeseries_aggregation_method_steps
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_methods method
           JOIN continuous.timeseries ts
             ON ts.timeseries_id = method.timeseries_id
           WHERE method.aggregation_method_id =
             timeseries_aggregation_method_steps.aggregation_method_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_methods method
           JOIN continuous.timeseries ts
             ON ts.timeseries_id = method.timeseries_id
           WHERE method.aggregation_method_id =
             timeseries_aggregation_method_steps.aggregation_method_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.aggregation_type_id IS
       'Aggregation type produced by this operation. The single terminal operation must match continuous.timeseries.aggregation_type_id.'"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_aggregation_method_step_inputs (
         aggregation_method_id BIGINT NOT NULL,
         step_id BIGINT NOT NULL,
         input_number SMALLINT NOT NULL,
         input_step_id BIGINT NOT NULL,
         component_weight NUMERIC(12, 8) NOT NULL DEFAULT 1,
         created_by TEXT NOT NULL DEFAULT CURRENT_USER,
         modified_by TEXT,
         created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT timeseries_aggregation_method_step_inputs_pk
           PRIMARY KEY (aggregation_method_id, step_id, input_number),
         CONSTRAINT timeseries_aggregation_method_step_inputs_unique
           UNIQUE (aggregation_method_id, step_id, input_step_id),
         CONSTRAINT timeseries_aggregation_method_step_inputs_step_fk
           FOREIGN KEY (aggregation_method_id, step_id)
           REFERENCES continuous.timeseries_aggregation_method_steps(
             aggregation_method_id, step_id
           ) ON DELETE CASCADE ON UPDATE CASCADE,
         CONSTRAINT timeseries_aggregation_method_step_inputs_input_fk
           FOREIGN KEY (aggregation_method_id, input_step_id)
           REFERENCES continuous.timeseries_aggregation_method_steps(
             aggregation_method_id, step_id
           ) ON DELETE CASCADE ON UPDATE CASCADE,
         CONSTRAINT timeseries_aggregation_method_step_inputs_number_positive
           CHECK (input_number > 0),
         CONSTRAINT timeseries_aggregation_method_step_inputs_weight_positive
           CHECK (component_weight > 0 AND component_weight <> 'NaN'::NUMERIC),
         CONSTRAINT timeseries_aggregation_method_step_inputs_distinct
           CHECK (step_id <> input_step_id)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_aggregation_method_step_inputs
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE continuous.timeseries_aggregation_method_step_inputs
       TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_aggregation_method_step_inputs
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_aggregation_method_visibility
       ON continuous.timeseries_aggregation_method_step_inputs
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_methods method
           JOIN continuous.timeseries ts
             ON ts.timeseries_id = method.timeseries_id
           WHERE method.aggregation_method_id =
             timeseries_aggregation_method_step_inputs.aggregation_method_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_methods method
           JOIN continuous.timeseries ts
             ON ts.timeseries_id = method.timeseries_id
           WHERE method.aggregation_method_id =
             timeseries_aggregation_method_step_inputs.aggregation_method_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_aggregation_method_step_inputs IS
       'Directed input relationships between operations in one aggregation method. Operations without input links consume the method source values; input links let downstream operations consume prior results. arithmetic_mean returns the weighted arithmetic mean of its inputs, using component_weight values (equal weights of 1 produce (min + max) / 2).'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_aggregation_method_steps IS
       'Calculation operations for a timeseries aggregation method. Multiple operations may share a stage and consume the original method input. Rows in later stages consume the explicitly linked outputs in timeseries_aggregation_method_step_inputs. Window aggregation operations record their half-open input window, optional output cadence and anchor, timezone for local calendar schedules, expected input cadence, weighting, coverage, and missing-data rules. Arithmetic-mean operations combine two or more linked operation outputs. A daily meteorological mean can therefore use parallel daily maximum and minimum operations followed by an arithmetic mean; separate method versions represent historical changes to the daily windows.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.window_start_offset IS
       'Inclusive offset from the step evaluation timestamp. For example, -2 minutes with a window_end_offset of 0 describes the two minutes before the timestamp. Day, month, and year components are calendar offsets in evaluation_timezone and therefore require that timezone; time-only offsets are elapsed durations.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.evaluation_interval IS
       'Optional repetition interval for scheduled outputs. Calendar units such as 1 day repeat in local calendar time when evaluation_timezone is supplied; time-only units represent elapsed durations. When present, evaluation_anchor fixes the cadence phase; NULL represents event-driven or otherwise irregular evaluations.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.evaluation_anchor IS
       'Local wall-clock timestamp that fixes the output cadence phase. Interpret it in evaluation_timezone when supplied, or in UTC when evaluation_timezone is NULL.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.evaluation_timezone IS
       'IANA timezone used to interpret local wall-clock anchors and calendar-based intervals or window offsets. It is required whenever an evaluation interval or window offset contains day, month, or year components. Local calendar schedules account for daylight-saving transitions.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.expected_input_interval IS
       'Expected spacing of values entering this operation, either from the original method input or linked operation outputs.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.input_weighting IS
       'equal gives each input value equal influence; duration_weighted weights each input by the duration it represents.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.minimum_coverage IS
       'Optional required fraction of the configured input window covered by valid inputs, from 0 through 1.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_aggregation_method_steps.calculation_parameters IS
       'Optional operator-specific JSON object for parameters that are not structural inputs or windows, such as implementation options for a percentile algorithm.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.validate_timeseries_aggregation_method()
       RETURNS trigger
       LANGUAGE plpgsql
       SET search_path = pg_catalog, continuous
       AS $function$
       DECLARE
         v_method_id BIGINT;
         v_method_ids BIGINT[];
         v_timeseries_id INTEGER;
         v_timeseries_aggregation_type_id INTEGER;
         v_operation_count INTEGER;
         v_stage_count INTEGER;
         v_last_stage_number SMALLINT;
         v_terminal_count INTEGER;
         v_final_aggregation_type_id INTEGER;
       BEGIN
         IF TG_OP = 'INSERT' THEN
           v_method_ids := ARRAY[NEW.aggregation_method_id];
         ELSIF TG_OP = 'DELETE' THEN
           v_method_ids := ARRAY[OLD.aggregation_method_id];
         ELSE
           v_method_ids := ARRAY[OLD.aggregation_method_id,
             NEW.aggregation_method_id];
         END IF;
         FOR v_method_id IN SELECT DISTINCT id FROM unnest(v_method_ids) id ORDER BY id LOOP

         SELECT timeseries_id
           INTO v_timeseries_id
           FROM continuous.timeseries_aggregation_methods
          WHERE aggregation_method_id = v_method_id
          FOR UPDATE;
         IF NOT FOUND THEN
           CONTINUE;
         END IF;

         IF EXISTS (
           SELECT 1 FROM continuous.timeseries_aggregation_method_steps step
           JOIN continuous.aggregation_types type USING (aggregation_type_id)
           WHERE step.aggregation_method_id = v_method_id AND (
             (step.operation_type = 'arithmetic_mean'
               AND type.aggregation_type NOT IN ('mean', '(min+max)/2'))
             OR (step.operation_type = 'window_aggregate'
               AND type.aggregation_type IN ('instantaneous', '(min+max)/2'))
           )
         ) THEN
           RAISE EXCEPTION USING ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_operation_type_valid',
             MESSAGE = 'The aggregation type must describe the calculation operation.';
         END IF;

         IF EXISTS (
           SELECT 1 FROM continuous.timeseries_aggregation_method_steps step
           JOIN continuous.aggregation_types type USING (aggregation_type_id)
           LEFT JOIN continuous.timeseries_aggregation_method_step_inputs input
             ON input.aggregation_method_id = step.aggregation_method_id
             AND input.step_id = step.step_id
           LEFT JOIN continuous.timeseries_aggregation_method_steps source
             ON source.aggregation_method_id = input.aggregation_method_id
             AND source.step_id = input.input_step_id
           LEFT JOIN continuous.aggregation_types source_type
             ON source_type.aggregation_type_id = source.aggregation_type_id
           WHERE step.aggregation_method_id = v_method_id
             AND type.aggregation_type = '(min+max)/2'
           GROUP BY step.step_id
           HAVING count(input.input_step_id) <> 2
             OR count(DISTINCT source_type.aggregation_type) <> 2
             OR count(*) FILTER (WHERE source_type.aggregation_type IN ('minimum', 'maximum')) <> 2
             OR min(input.component_weight) <> max(input.component_weight)
             OR bool_or(step.missing_data_policy <> 'propagate')
         ) THEN
           RAISE EXCEPTION USING ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_extrema_mean_valid',
             MESSAGE = '(min+max)/2 requires one minimum and one maximum with equal weights and propagated missing data.';
         END IF;

         SELECT count(*)::INTEGER,
                count(DISTINCT stage_number)::INTEGER,
                max(stage_number)
           INTO v_operation_count, v_stage_count, v_last_stage_number
           FROM continuous.timeseries_aggregation_method_steps
          WHERE aggregation_method_id = v_method_id;
         IF v_operation_count = 0 THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_requires_steps',
             MESSAGE = 'An aggregation method must contain at least one calculation step.';
         END IF;
         IF v_stage_count <> v_last_stage_number THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_stages_contiguous',
             MESSAGE = 'Aggregation method stages must be contiguous from 1.';
         END IF;

         IF EXISTS (
           SELECT 1
             FROM continuous.timeseries_aggregation_method_steps operation
             LEFT JOIN LATERAL (
               SELECT count(*)::INTEGER AS input_count
                 FROM continuous.timeseries_aggregation_method_step_inputs input
                WHERE input.aggregation_method_id =
                  operation.aggregation_method_id
                  AND input.step_id = operation.step_id
             ) inputs ON TRUE
            WHERE operation.aggregation_method_id = v_method_id
              AND operation.operation_type = 'arithmetic_mean'
             AND inputs.input_count < 2
         ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_mean_requires_inputs',
             MESSAGE = 'An arithmetic-mean operation must consume at least two prior operation outputs.';
         END IF;

         IF EXISTS (
           SELECT 1
             FROM continuous.timeseries_aggregation_method_steps operation
            WHERE operation.aggregation_method_id = v_method_id
              AND operation.stage_number > 1
              AND NOT EXISTS (
                SELECT 1
                  FROM continuous.timeseries_aggregation_method_step_inputs input
                 WHERE input.aggregation_method_id =
                   operation.aggregation_method_id
                   AND input.step_id = operation.step_id
              )
         ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_later_stage_requires_input',
             MESSAGE = 'Operations after stage 1 must consume at least one prior operation output.';
         END IF;

         IF EXISTS (
           SELECT 1
             FROM continuous.timeseries_aggregation_method_step_inputs input
             JOIN continuous.timeseries_aggregation_method_steps operation
               ON operation.aggregation_method_id =
                    input.aggregation_method_id
              AND operation.step_id = input.step_id
             JOIN continuous.timeseries_aggregation_method_steps source
               ON source.aggregation_method_id =
                    input.aggregation_method_id
              AND source.step_id = input.input_step_id
            WHERE input.aggregation_method_id = v_method_id
              AND source.stage_number >= operation.stage_number
         ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_input_stage_order',
             MESSAGE = 'An operation may only consume outputs from an earlier stage.';
         END IF;

         IF EXISTS (
           SELECT 1
             FROM continuous.timeseries_aggregation_method_step_inputs input
            WHERE input.aggregation_method_id = v_method_id
            GROUP BY input.step_id
           HAVING min(input.input_number) <> 1
               OR max(input.input_number) <> count(*)
         ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_input_numbers_contiguous',
             MESSAGE = 'Operation input numbers must be contiguous from 1.';
         END IF;

         -- UTC is a known valid timezone used by all ECCC seeds. Expanding
         -- pg_timezone_names for every deferred step/edge event is expensive;
         -- consult the server catalogue only when another name is present.
         IF EXISTS (
           SELECT 1 FROM continuous.timeseries_aggregation_method_steps step
           WHERE step.aggregation_method_id = v_method_id
             AND step.evaluation_timezone IS NOT NULL
             AND step.evaluation_timezone <> 'UTC'
         ) THEN
         IF EXISTS (
           SELECT 1
             FROM continuous.timeseries_aggregation_method_steps step
            WHERE step.aggregation_method_id = v_method_id
              AND step.evaluation_timezone IS NOT NULL
              AND step.evaluation_timezone <> 'UTC'
              AND NOT EXISTS (
                SELECT 1
                  FROM pg_catalog.pg_timezone_names tz
                 WHERE tz.name = step.evaluation_timezone
              )
         ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_timezone_valid',
             MESSAGE = 'An evaluation_timezone must be a recognized PostgreSQL/IANA timezone name.';
         END IF;
         END IF;

         SELECT count(*)::INTEGER, max(operation.aggregation_type_id)
           INTO v_terminal_count, v_final_aggregation_type_id
           FROM continuous.timeseries_aggregation_method_steps operation
          WHERE operation.aggregation_method_id = v_method_id
            AND NOT EXISTS (
              SELECT 1
                FROM continuous.timeseries_aggregation_method_step_inputs input
               WHERE input.aggregation_method_id =
                 operation.aggregation_method_id
                 AND input.input_step_id = operation.step_id
            );
         IF v_terminal_count <> 1 THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_single_output',
             MESSAGE = 'An aggregation method must have exactly one terminal output operation.';
         END IF;
         SELECT aggregation_type_id
           INTO v_timeseries_aggregation_type_id
           FROM continuous.timeseries
          WHERE timeseries_id = v_timeseries_id
          FOR UPDATE;
         IF v_final_aggregation_type_id IS DISTINCT FROM
            v_timeseries_aggregation_type_id THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_final_type_matches_series',
             MESSAGE = 'The final aggregation step must match the timeseries aggregation_type_id.';
         END IF;

         END LOOP;
         RETURN NULL;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.validate_timeseries_aggregation_method()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.validate_timeseries_aggregation_method()
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.validate_timeseries_aggregation_type()
       RETURNS trigger
       LANGUAGE plpgsql
       SET search_path = pg_catalog, continuous
       AS $function$
       DECLARE
         v_current_type INTEGER;
       BEGIN
         IF NEW.aggregation_type_id IS NOT DISTINCT FROM
            OLD.aggregation_type_id THEN
           RETURN NULL;
        END IF;

         -- Deferred checks must inspect the final row, not an intermediate
         -- NEW value from an earlier update in the same transaction.
         SELECT aggregation_type_id INTO v_current_type
         FROM continuous.timeseries WHERE timeseries_id = NEW.timeseries_id;

         IF EXISTS (
           SELECT 1
             FROM continuous.timeseries_aggregation_methods method
             JOIN continuous.timeseries_aggregation_method_steps final_step
               ON final_step.aggregation_method_id =
                 method.aggregation_method_id
            WHERE method.timeseries_id = NEW.timeseries_id
              AND NOT EXISTS (
                SELECT 1
                  FROM continuous.timeseries_aggregation_method_step_inputs input
                 WHERE input.aggregation_method_id =
                   final_step.aggregation_method_id
                   AND input.input_step_id = final_step.step_id
              )
              AND final_step.aggregation_type_id IS DISTINCT FROM
                v_current_type
         ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'timeseries_aggregation_method_final_type_matches_series',
             MESSAGE = 'The timeseries aggregation_type_id must match the final step of every aggregation method.';
         END IF;

         RETURN NULL;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.validate_timeseries_aggregation_type()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "REVOKE ALL ON FUNCTION continuous.validate_timeseries_aggregation_type()
       FROM PUBLIC"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_timeseries_aggregation_type
       AFTER UPDATE OF aggregation_type_id ON continuous.timeseries
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_timeseries_aggregation_type()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_timeseries_aggregation_method
       AFTER INSERT OR UPDATE ON continuous.timeseries_aggregation_methods
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_timeseries_aggregation_method()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_timeseries_aggregation_method_steps
       AFTER INSERT OR UPDATE OR DELETE
       ON continuous.timeseries_aggregation_method_steps
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_timeseries_aggregation_method()"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER validate_timeseries_aggregation_method_step_inputs
       AFTER INSERT OR UPDATE OR DELETE
       ON continuous.timeseries_aggregation_method_step_inputs
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.validate_timeseries_aggregation_method()"
    )

    for (table_name in c(
      "timeseries_aggregation_methods",
      "timeseries_aggregation_method_steps",
      "timeseries_aggregation_method_step_inputs"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER update_%s_modified
           BEFORE UPDATE ON continuous.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER update_%s_modified_by
           BEFORE UPDATE ON continuous.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER audit_%s_trigger
           AFTER INSERT OR UPDATE OR DELETE ON continuous.%s
           FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()",
          table_name,
          table_name
        )
      )
    }
    DBI::dbExecute(
      con,
      "INSERT INTO audit.table_registry (
         schema_name,
         table_name,
         capture_mode,
         rationale,
         history_started_at,
         updated_at
       ) VALUES
         (
           'continuous',
           'timeseries_aggregation_methods',
           'generic_insert_update_delete',
           'Time-ranged aggregation-method assignments determine how continuous measurements are interpreted.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'timeseries_aggregation_method_steps',
           'generic_insert_update_delete',
           'Aggregation operations determine the statistics and temporal windows that produce continuous values.',
           clock_timestamp(),
           clock_timestamp()
         ),
         (
           'continuous',
           'timeseries_aggregation_method_step_inputs',
           'generic_insert_update_delete',
           'Input relationships record dependencies between aggregation operations.',
           clock_timestamp(),
           clock_timestamp()
         )
       ON CONFLICT (schema_name, table_name) DO UPDATE
       SET capture_mode = EXCLUDED.capture_mode,
           rationale = EXCLUDED.rationale,
           updated_at = EXCLUDED.updated_at"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.measurements_continuous.period IS
       'Per-observation measurement duration. This records the support period for an individual value and remains used by continuous-data import, synchronization, imputation, and daily HYDAT updates. It is distinct from the time-ranged calculation method in continuous.timeseries_aggregation_methods and continuous.timeseries_aggregation_method_steps.'"
    )

    # ECCC wind observations are averaged values, not instantaneous values.
    # Infer their aggregation type from active fetch arguments so this works
    # across clusters whose timeseries IDs differ. Correct only series with
    # one unambiguous active ECCC fetch signature.
    wind_sources <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT
         ts.timeseries_id,
         ts.aggregation_type_id,
         adapter.source_fx_args ->> 'location' AS station_id,
         adapter.source_fx_args ->> 'parameter' AS source_parameter,
         adapter.source_fx_args ->> 'interval' AS source_interval
       FROM continuous.timeseries_source_adapters adapter
       JOIN continuous.timeseries ts
         ON ts.timeseries_id = adapter.timeseries_id
       WHERE adapter.source_fx = 'downloadECCCwx'
         AND adapter.active IS TRUE
         AND jsonb_typeof(adapter.source_fx_args) = 'object'
         AND NOT EXISTS (
           SELECT 1 FROM continuous.timeseries_source_adapters other
           WHERE other.timeseries_id = ts.timeseries_id AND other.active
             AND other.source_fx IS DISTINCT FROM 'downloadECCCwx')"
    )
    if (nrow(wind_sources) > 0L) {
      wind_sources <- unique(wind_sources)
      signatures_per_series <- vapply(
        split(wind_sources, wind_sources$timeseries_id),
        nrow,
        integer(1)
      )
      unambiguous_wind_sources <- wind_sources[
        wind_sources$timeseries_id %in%
          as.integer(names(signatures_per_series[
            signatures_per_series == 1L
          ])) &
          wind_sources$source_parameter %in%
            c("wind_spd", "wind_dir", "wind_gust", "spd_max_gust"),
      ]
      if (nrow(unambiguous_wind_sources) > 0L) {
        unambiguous_wind_sources$expected_aggregation <- ifelse(
          unambiguous_wind_sources$source_parameter %in%
            c("wind_spd", "wind_dir"),
          "mean",
          "maximum"
        )
        required_wind_types <- unique(
          unambiguous_wind_sources$expected_aggregation
        )
        wind_aggregation_types <- DBI::dbGetQuery(
          con,
          "SELECT aggregation_type, aggregation_type_id
           FROM continuous.aggregation_types
           WHERE aggregation_type IN ('mean', 'maximum')"
        )
        if (
          !all(required_wind_types %in% wind_aggregation_types$aggregation_type)
        ) {
          stop(
            "DEV patch 65 requires the aggregation types used by active ECCC wind fetches: ",
            paste(required_wind_types, collapse = ", ")
          )
        }
        unambiguous_wind_sources$expected_aggregation_type_id <- as.integer(
          wind_aggregation_types$aggregation_type_id[
            match(
              unambiguous_wind_sources$expected_aggregation,
              wind_aggregation_types$aggregation_type
            )
          ]
        )
        wind_type_changes <- unambiguous_wind_sources[
          unambiguous_wind_sources$aggregation_type_id !=
            unambiguous_wind_sources$expected_aggregation_type_id,
        ]
        if (nrow(wind_type_changes) > 0L) {
          for (row_number in seq_len(nrow(wind_type_changes))) {
            DBI::dbExecute(
              con,
              "UPDATE continuous.timeseries
               SET aggregation_type_id = $1
               WHERE timeseries_id = $2",
              params = list(
                wind_type_changes$expected_aggregation_type_id[[row_number]],
                wind_type_changes$timeseries_id[[row_number]]
              )
            )
          }
          message(
            "Corrected ",
            nrow(wind_type_changes),
            " ECCC wind timeseries aggregation type(s) from active fetch arguments."
          )
        }
      }
    }

    # Seed ECCC methods from each instance's active downloadECCCwx arguments.
    # No timeseries IDs are assumed: deployed clusters have different station
    # and parameter selections. Daily cutovers below are expressed against the
    # timestamps stored by downloadECCCwx (ECCC date + 30 hours, in UTC).
    eccc_sources <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT
         ts.timeseries_id,
         ts.aggregation_type_id,
         aggregation.aggregation_type,
         adapter.source_fx_args ->> 'location' AS station_id,
         adapter.source_fx_args ->> 'parameter' AS parameter,
         adapter.source_fx_args ->> 'interval' AS interval
       FROM continuous.timeseries_source_adapters adapter
       JOIN continuous.timeseries ts
         ON ts.timeseries_id = adapter.timeseries_id
       JOIN continuous.aggregation_types aggregation
         ON aggregation.aggregation_type_id = ts.aggregation_type_id
       WHERE adapter.source_fx = 'downloadECCCwx'
         AND adapter.active IS TRUE
         AND jsonb_typeof(adapter.source_fx_args) = 'object'
         AND NOT EXISTS (
           SELECT 1 FROM continuous.timeseries_source_adapters other
           WHERE other.timeseries_id = ts.timeseries_id AND other.active
             AND other.source_fx IS DISTINCT FROM 'downloadECCCwx')"
    )

    if (nrow(eccc_sources) > 0L) {
      component_aggregation_types <- DBI::dbGetQuery(
        con,
        "SELECT aggregation_type, aggregation_type_id
         FROM continuous.aggregation_types
         WHERE aggregation_type IN ('minimum', 'maximum')"
      )
      if (
        !all(
          c("minimum", "maximum") %in%
            component_aggregation_types$aggregation_type
        )
      ) {
        stop(
          "DEV patch 65 requires 'minimum' and 'maximum' aggregation types to model ECCC daily mean temperature."
        )
      }
      minimum_aggregation_type_id <- as.integer(
        component_aggregation_types$aggregation_type_id[
          match("minimum", component_aggregation_types$aggregation_type)
        ]
      )
      maximum_aggregation_type_id <- as.integer(
        component_aggregation_types$aggregation_type_id[
          match("maximum", component_aggregation_types$aggregation_type)
        ]
      )

      # Duplicate identical active adapters are harmless; conflicting fetch
      # signatures for one series cannot be assigned one unambiguous method.
      eccc_sources <- unique(eccc_sources)
      signatures_per_series <- vapply(
        split(eccc_sources, eccc_sources$timeseries_id),
        nrow,
        integer(1)
      )
      ambiguous_series <- as.integer(
        names(signatures_per_series[signatures_per_series > 1L])
      )
      if (length(ambiguous_series) > 0L) {
        message(
          "Skipping ECCC aggregation settings for timeseries with multiple active fetch signatures: ",
          paste(ambiguous_series, collapse = ", ")
        )
      }
      incomplete_series <- unique(
        eccc_sources$timeseries_id[
          is.na(eccc_sources$station_id) |
            !nzchar(eccc_sources$station_id) |
            is.na(eccc_sources$parameter) |
            is.na(eccc_sources$interval)
        ]
      )
      if (length(incomplete_series) > 0L) {
        message(
          "ECCC timeseries with incomplete location/parameter/interval fetch arguments were left unassigned: ",
          paste(incomplete_series, collapse = ", ")
        )
      }
      eccc_sources <- eccc_sources[
        !eccc_sources$timeseries_id %in% ambiguous_series &
          !is.na(eccc_sources$station_id) &
          nzchar(eccc_sources$station_id) &
          !is.na(eccc_sources$parameter) &
          !is.na(eccc_sources$interval),
      ]

      # Match ECCC's numeric station_id against the weathercan station
      # catalogue. stations_search() searches by station name or coordinates,
      # so it is not an exact lookup for source adapter location IDs. Its
      # TC_id is an identifier, not proof of METAR/SYNOP reporting or its
      # historical averaging window. Preserve it as provenance only. Wind
      # methods require a separately reviewed report type and applicability
      # period, so they remain unassigned rather than inferring 2/10 minutes.
      station_catalog <- weathercan::stations()
      if (
        !all(
          c(
            "station_id",
            "station_name",
            "TC_id",
            "interval",
            "start",
            "end"
          ) %in%
            names(station_catalog)
        )
      ) {
        stop(
          "DEV patch 65 requires weathercan station IDs, names, interval, record years, and TC_id metadata."
        )
      }
      station_catalog$station_id_key <- trimws(
        as.character(suppressWarnings(
          as.numeric(as.character(station_catalog$station_id))
        ))
      )
      station_catalog$TC_id <- trimws(as.character(station_catalog$TC_id))
      station_catalog$TC_id[
        is.na(station_catalog$TC_id) | !nzchar(station_catalog$TC_id)
      ] <- NA_character_
      station_catalog$station_name <- trimws(
        as.character(station_catalog$station_name)
      )
      eccc_sources$station_id_key <- trimws(
        as.character(suppressWarnings(as.numeric(eccc_sources$station_id)))
      )
      station_ids <- unique(eccc_sources$station_id_key)
      station_metadata <- do.call(
        rbind,
        lapply(station_ids, function(station_id) {
          station_rows <- station_catalog[
            !is.na(station_catalog$station_id_key) &
              station_catalog$station_id_key == station_id,
            ,
            drop = FALSE
          ]
          station_tc_ids <- unique(
            station_rows$TC_id[
              !is.na(station_rows$TC_id) & nzchar(station_rows$TC_id)
            ]
          )
          station_names <- unique(
            station_rows$station_name[
              !is.na(station_rows$station_name) &
                nzchar(station_rows$station_name)
            ]
          )
          report_type <- if (nrow(station_rows) == 0L) {
            "unclassified"
          } else if (length(station_tc_ids) > 1L) {
            "ambiguous"
          } else {
            "unclassified"
          }
          data.frame(
            station_id = station_id,
            station_name = paste(station_names, collapse = " / "),
            station_tc_id = if (length(station_tc_ids) == 1L) {
              station_tc_ids[[1]]
            } else {
              NA_character_
            },
            wind_report_type = report_type,
            stringsAsFactors = FALSE
          )
        })
      )
      station_metadata_index <- match(
        eccc_sources$station_id_key,
        station_metadata$station_id
      )
      eccc_sources$station_name <- station_metadata$station_name[
        station_metadata_index
      ]
      eccc_sources$station_tc_id <- station_metadata$station_tc_id[
        station_metadata_index
      ]
      eccc_sources$wind_report_type <- station_metadata$wind_report_type[
        station_metadata_index
      ]
      message(
        "Classified ECCC station metadata from weathercan: ",
        sum(station_metadata$wind_report_type == "aviation"),
        " verified aviation station(s), ",
        sum(station_metadata$wind_report_type == "synoptic"),
        " verified synoptic station(s), and ",
        sum(!station_metadata$wind_report_type %in% c("aviation", "synoptic")),
        " station record(s) requiring wind-method review; TC_id alone does not classify a report."
      )

      # The fixed historical daily boundaries in ECCC's guidance apply to
      # stations with hourly observations. Use the same ECCC station ID to
      # establish that fact; daily-only sites require their station report to
      # determine the local observation day and are reported for manual review.
      hourly_stations <- unique(
        eccc_sources$station_id[eccc_sources$interval == "hour"]
      )
      hourly_archive <- station_catalog[
        as.character(station_catalog$interval) == "hour" &
          is.finite(station_catalog$start) &
          is.finite(station_catalog$end),
        ,
        drop = FALSE
      ]
      hourly_stations <- intersect(
        hourly_stations,
        hourly_archive$station_id_key
      )
      unmatched_daily_stations <- unique(
        eccc_sources$station_id[
          eccc_sources$interval == "day" &
            !eccc_sources$station_id %in% hourly_stations
        ]
      )
      if (length(unmatched_daily_stations) > 0L) {
        message(
          "ECCC daily station IDs without an active hourly ECCC series need station-specific day-boundary review: ",
          paste(unmatched_daily_stations, collapse = ", ")
        )
      }

      add_eccc_aggregation_method <- function(
        series,
        applicable_from,
        applicable_until,
        window_start,
        window_end,
        evaluation_interval,
        evaluation_anchor,
        expected_input_interval,
        calculation_parameters,
        source_reference,
        note,
        operations = NULL,
        operation_inputs = list()
      ) {
        method_id <- DBI::dbGetQuery(
          con,
          "INSERT INTO continuous.timeseries_aggregation_methods (
             timeseries_id,
             applicable_from,
             applicable_until,
             source_name,
             source_reference,
             note
           ) VALUES (
             $1, $2::timestamptz, $3::timestamptz,
             'Environment and Climate Change Canada', $4, $5
           )
           RETURNING aggregation_method_id",
          params = list(
            as.integer(series$timeseries_id),
            applicable_from,
            applicable_until,
            source_reference,
            note
          )
        )$aggregation_method_id[[1]]
        if (is.null(operations)) {
          operations <- list(list(
            stage_number = 1L,
            operation_type = "window_aggregate",
            aggregation_type_id = as.integer(series$aggregation_type_id),
            window_start = window_start,
            window_end = window_end,
            evaluation_interval = evaluation_interval,
            evaluation_anchor = evaluation_anchor,
            evaluation_timezone = "UTC",
            expected_input_interval = expected_input_interval,
            calculation_parameters = calculation_parameters
          ))
        }

        operation_ids <- vector("list", length(operations))
        for (operation_index in seq_along(operations)) {
          operation <- operations[[operation_index]]
          # RPostgres binds SQL NULL as a scalar NA, not a zero-length R NULL.
          # Arithmetic operations omit all window/cadence fields by design.
          for (field in c(
            "window_start",
            "window_end",
            "evaluation_interval",
            "evaluation_anchor",
            "evaluation_timezone",
            "expected_input_interval"
          )) {
            if (is.null(operation[[field]])) operation[[field]] <- NA_character_
          }
          operation_parameters <- operation$calculation_parameters
          if (is.null(operation_parameters)) {
            operation_parameters_json <- "{}"
          } else {
            operation_parameters_json <- as.character(jsonlite::toJSON(
              operation_parameters,
              auto_unbox = TRUE,
              null = "null"
            ))
          }
          operation_ids[[operation_index]] <- DBI::dbGetQuery(
            con,
            "INSERT INTO continuous.timeseries_aggregation_method_steps (
               aggregation_method_id,
               stage_number,
               operation_type,
               aggregation_type_id,
               window_start_offset,
               window_end_offset,
               evaluation_interval,
               evaluation_anchor,
               evaluation_timezone,
               expected_input_interval,
               calculation_parameters,
               missing_data_policy
             ) VALUES (
               $1, $2, $3, $4, $5::interval, $6::interval,
               $7::interval, $8::timestamp, $9, $10::interval, $11::jsonb, $12
             )
             RETURNING step_id",
            params = list(
              as.numeric(method_id),
              as.integer(operation$stage_number),
              operation$operation_type,
              as.integer(operation$aggregation_type_id),
              operation$window_start,
              operation$window_end,
              operation$evaluation_interval,
              operation$evaluation_anchor,
              operation$evaluation_timezone,
              operation$expected_input_interval,
              operation_parameters_json,
              if (operation$operation_type == "arithmetic_mean") {
                "propagate"
              } else {
                "ignore"
              }
            )
          )$step_id[[1]]
        }

        for (input in operation_inputs) {
          component_weight <- input$component_weight
          if (is.null(component_weight)) {
            component_weight <- 1
          }
          DBI::dbExecute(
            con,
            "INSERT INTO continuous.timeseries_aggregation_method_step_inputs (
               aggregation_method_id,
               step_id,
               input_number,
               input_step_id,
               component_weight
             ) VALUES ($1, $2, $3, $4, $5)",
            params = list(
              as.numeric(method_id),
              as.numeric(operation_ids[[input$operation_index]]),
              as.integer(input$input_number),
              as.numeric(operation_ids[[input$input_operation_index]]),
              as.numeric(component_weight)
            )
          )
        }
      }

      daily_periods <- list(
        list(
          from = "-infinity",
          until = "1957-06-02 06:00:00+00",
          precip_start = "-17 hours -30 minutes",
          precip_end = "6 hours 30 minutes",
          min_start = "-29 hours -30 minutes",
          min_end = "-5 hours -30 minutes",
          label = "Before 1957-06-01 ECCC date"
        ),
        list(
          from = "1957-06-02 06:00:00+00",
          until = "1961-07-02 06:00:00+00",
          precip_start = "-18 hours",
          precip_end = "6 hours",
          min_start = "-30 hours",
          min_end = "-6 hours",
          label = "1957-06-01 through 1961-06-30 ECCC dates"
        ),
        list(
          from = "1961-07-02 06:00:00+00",
          until = "infinity",
          precip_start = "-24 hours",
          precip_end = "0 seconds",
          min_start = "-24 hours",
          min_end = "0 seconds",
          label = "From 1961-07-01 ECCC date"
        )
      )

      created_eccc_methods <- 0L
      unsupported_eccc <- character()
      for (row_number in seq_len(nrow(eccc_sources))) {
        series <- eccc_sources[row_number, , drop = FALSE]
        parameter <- series$parameter[[1]]
        interval <- series$interval[[1]]
        aggregation <- series$aggregation_type[[1]]
        wind_report_type <- series$wind_report_type[[1]]
        methods <- list()

        # The provider catalogue bounds historical applicability. A currently
        # active adapter does not prove this station had hourly observations
        # before its hourly archive began. Daily rules use the overlap of the
        # daily and hourly record years, expressed in our stored UTC timestamps.
        archive_rows <- station_catalog[
          station_catalog$station_id_key == series$station_id_key[[1]] &
            as.character(station_catalog$interval) == interval &
            is.finite(station_catalog$start) &
            is.finite(station_catalog$end),
          ,
          drop = FALSE
        ]
        if (!nrow(archive_rows)) {
          unsupported_eccc <- c(
            unsupported_eccc,
            sprintf(
              "%s/%s: no verified %s archive years",
              series$timeseries_id,
              series$station_id,
              interval
            )
          )
          next
        }
        first_year <- min(archive_rows$start)
        last_year <- max(archive_rows$end)
        if (interval == "day") {
          hourly_rows <- hourly_archive[
            hourly_archive$station_id_key == series$station_id_key[[1]],
            ,
            drop = FALSE
          ]
          if (!nrow(hourly_rows)) {
            unsupported_eccc <- c(
              unsupported_eccc,
              sprintf(
                "%s/%s: daily observation-day history needs review",
                series$timeseries_id,
                series$station_id
              )
            )
            next
          }
          first_year <- max(first_year, min(hourly_rows$start))
          last_year <- min(last_year, max(hourly_rows$end))
        }
        if (first_year > last_year) {
          next
        }
        archive_from <- as.POSIXct(
          sprintf(
            "%d-01-%s",
            as.integer(first_year),
            if (interval == "day") "02 06:00:00" else "01 00:00:00"
          ),
          tz = "UTC"
        )
        archive_until <- if (
          last_year >= as.integer(format(Sys.Date(), "%Y"))
        ) {
          Inf
        } else {
          as.numeric(as.POSIXct(
            sprintf(
              "%d-01-%s",
              as.integer(last_year) + 1L,
              if (interval == "day") "02 06:00:00" else "01 00:00:00"
            ),
            tz = "UTC"
          ))
        }

        # Instantaneous values have no aggregation window to record.
        if (aggregation == "instantaneous") {
          next
        }

        if (
          interval == "hour" &&
            parameter %in% c("wind_spd", "wind_dir") &&
            aggregation == "mean" &&
            wind_report_type %in% c("aviation", "synoptic")
        ) {
          is_aviation <- wind_report_type == "aviation"
          wind_window <- if (is_aviation) "-2 minutes" else "-10 minutes"
          wind_report_interval <- if (is_aviation) "1 hour" else "6 hours"
          station_parameters <- list(
            source_parameter = parameter,
            source_interval = interval,
            station_id = series$station_id[[1]],
            station_name = series$station_name[[1]],
            station_TC_id = series$station_tc_id[[1]],
            report_type = wind_report_type,
            station_metadata_source = "weathercan::stations() ECCC station catalogue"
          )
          if (parameter == "wind_dir") {
            station_parameters$statistic <-
              "ECCC-reported mean wind direction; preserve as a vector mean rather than averaging degree values arithmetically"
            station_parameters$source_value_transform <-
              "downloadECCCwx converts ECCC tens of degrees to degrees"
            station_parameters$calm_direction_value <- "0 degrees"
          }
          methods <- list(list(
            from = "-infinity",
            until = "infinity",
            start = wind_window,
            end = "0 seconds",
            evaluation_interval = wind_report_interval,
            evaluation_anchor = "1970-01-01 00:00:00",
            expected_input_interval = wind_report_interval,
            parameters = station_parameters,
            reference = "https://climate.weather.gc.ca/glossary_e.html
https://climate.weather.gc.ca/doc/Technical_Documentation.pdf
https://www.canada.ca/en/environment-climate-change/services/climate-change/canadian-centre-climate-services/display-download/technical-documentation-hourly-data.html
https://www.canada.ca/en/environment-climate-change/services/weather-manuals-documentation/manobs-surface-observations.html",
            note = if (is_aviation) {
              paste0(
                "weathercan station metadata identifies this airport observing site by TC_id ",
                series$station_tc_id[[1]],
                ". ECCC hourly aviation wind observations use a two-minute mean ending at the observation time."
              )
            } else {
              paste0(
                "weathercan station metadata has no TC_id for this station, so its hourly observations are treated as synoptic. ECCC synoptic reports occur at 00, 06, 12, and 18 UTC and use a ten-minute wind mean ending at the observation time."
              )
            }
          ))
        } else if (
          interval == "hour" &&
            parameter %in% c("wind_spd", "wind_dir") &&
            aggregation == "mean"
        ) {
          # Unclassified/conflicting station metadata cannot safely distinguish
          # the two-minute aviation from ten-minute synoptic wind window.
          methods <- list()
        } else if (
          interval == "hour" &&
            parameter == "precip_amt" &&
            aggregation == "sum"
        ) {
          methods <- list(list(
            from = "-infinity",
            until = "infinity",
            start = "0 seconds",
            end = "1 hour",
            evaluation_interval = "1 hour",
            evaluation_anchor = "1970-01-01 00:00:00",
            expected_input_interval = "1 hour",
            parameters = list(
              source_parameter = parameter,
              source_interval = interval,
              timestamp_semantics = "ECCC hourly precipitation amount for the hour beginning at the timestamp."
            ),
            reference = "https://climate.weather.gc.ca/doc/Technical_Documentation.pdf",
            note = "ECCC hourly precipitation amount covers the hour beginning at the reported UTC timestamp."
          ))
        } else if (
          interval == "hour" &&
            parameter %in% c("wind_gust", "spd_max_gust") &&
            aggregation == "maximum"
        ) {
          methods <- list(list(
            from = "-infinity",
            until = "infinity",
            start = "-10 minutes",
            end = "0 seconds",
            evaluation_interval = "1 hour",
            evaluation_anchor = "1970-01-01 00:00:00",
            expected_input_interval = "1 hour",
            parameters = list(
              source_parameter = parameter,
              source_interval = interval,
              statistic = "Maximum gust observed during the 10 minutes before the observation time; gust speed is a peak value measured over a short sample, up to 5 seconds."
            ),
            reference = "https://www.canada.ca/en/environment-climate-change/services/weather-manuals-documentation/manobs-surface-observations.html
https://www.canada.ca/en/environment-climate-change/services/weather-general-tools-resources/how-we-use-observation-site/understanding-current-conditions-on-website.html",
            note = "ECCC hourly gust is the maximum gust from the 10 minutes preceding the observation. Only used when this hourly parameter is actually present in the configured downloadECCCwx source."
          ))
        } else if (
          interval == "day" &&
            series$station_id[[1]] %in% hourly_stations &&
            parameter %in%
              c(
                "mean_temp",
                "min_temp",
                "max_temp",
                "total_precip",
                "total_rain",
                "total_snow"
              )
        ) {
          is_mean_temp <- parameter == "mean_temp" &&
            aggregation %in% c("mean", "(min+max)/2")
          is_precipitation <- parameter %in%
            c(
              "total_precip",
              "total_rain",
              "total_snow"
            ) &&
            aggregation == "sum"
          is_minimum <- parameter == "min_temp" &&
            aggregation == "minimum"
          is_maximum <- parameter == "max_temp" &&
            aggregation == "maximum"
          if (is_mean_temp || is_precipitation || is_minimum || is_maximum) {
            for (period in daily_periods) {
              operations <- NULL
              operation_inputs <- list()
              if (is_mean_temp) {
                operations <- list(
                  list(
                    stage_number = 1L,
                    operation_type = "window_aggregate",
                    aggregation_type_id = minimum_aggregation_type_id,
                    window_start = period$min_start,
                    window_end = period$min_end,
                    evaluation_interval = "1 day",
                    evaluation_anchor = "1970-01-02 06:00:00",
                    evaluation_timezone = "UTC",
                    expected_input_interval = NULL,
                    calculation_parameters = list(
                      source_parameter = "min_temp",
                      source_interval = interval,
                      daily_period = period$label
                    )
                  ),
                  list(
                    stage_number = 1L,
                    operation_type = "window_aggregate",
                    aggregation_type_id = maximum_aggregation_type_id,
                    window_start = period$precip_start,
                    window_end = period$precip_end,
                    evaluation_interval = "1 day",
                    evaluation_anchor = "1970-01-02 06:00:00",
                    evaluation_timezone = "UTC",
                    expected_input_interval = NULL,
                    calculation_parameters = list(
                      source_parameter = "max_temp",
                      source_interval = interval,
                      daily_period = period$label
                    )
                  ),
                  list(
                    stage_number = 2L,
                    operation_type = "arithmetic_mean",
                    aggregation_type_id = as.integer(
                      series$aggregation_type_id
                    ),
                    calculation_parameters = list(
                      formula = "(minimum + maximum) / 2",
                      daily_period = period$label
                    )
                  )
                )
                operation_inputs <- list(
                  list(
                    operation_index = 3L,
                    input_operation_index = 1L,
                    input_number = 1L,
                    component_weight = 1
                  ),
                  list(
                    operation_index = 3L,
                    input_operation_index = 2L,
                    input_number = 2L,
                    component_weight = 1
                  )
                )
                window_start <- NULL
                window_end <- NULL
                component_parameters <- list()
              } else if (is_minimum) {
                component_parameters <- list(
                  source_parameter = parameter,
                  source_interval = interval,
                  daily_period = period$label,
                  timestamp_basis = "downloadECCCwx stores the source date at 06:00 UTC on the following day."
                )
                window_start <- period$min_start
                window_end <- period$min_end
              } else {
                component_parameters <- list(
                  source_parameter = parameter,
                  source_interval = interval,
                  daily_period = period$label,
                  includes = if (is_maximum) {
                    "air temperature observations"
                  } else if (parameter == "total_precip") {
                    "rain and water-equivalent snowfall"
                  } else if (parameter == "total_rain") {
                    "rainfall"
                  } else {
                    "fresh snowfall depth"
                  },
                  timestamp_basis = "downloadECCCwx stores the source date at 06:00 UTC on the following day."
                )
                window_start <- period$precip_start
                window_end <- period$precip_end
              }
              methods[[length(methods) + 1L]] <- list(
                from = period$from,
                until = period$until,
                start = window_start,
                end = window_end,
                evaluation_interval = "1 day",
                evaluation_anchor = "1970-01-02 06:00:00",
                expected_input_interval = NULL,
                parameters = component_parameters,
                operations = operations,
                operation_inputs = operation_inputs,
                reference = "https://www.canada.ca/en/environment-climate-change/services/climate-change/canadian-centre-climate-services/display-download/technical-documentation-daily-data.html
https://climate.weather.gc.ca/glossary_e.html",
                note = paste0(
                  if (is_mean_temp) {
                    "ECCC daily mean temperature is the arithmetic mean of daily minimum and maximum temperature. "
                  } else {
                    ""
                  },
                  "ECCC daily value for ",
                  period$label,
                  "; applicability boundaries are shifted to match downloadECCCwx's next-day 06:00 UTC stored timestamp."
                )
              )
            }
          }
        }

        if (length(methods) == 0L) {
          review_reason <- if (
            interval == "day" &&
              parameter == "spd_max_gust" &&
              aggregation == "maximum"
          ) {
            "daily gust reporting-day boundary needs station-history review"
          } else if (
            interval == "hour" &&
              parameter %in% c("wind_spd", "wind_dir") &&
              aggregation == "mean" &&
              !wind_report_type %in% c("aviation", "synoptic")
          ) {
            paste0(
              "weathercan station metadata is ",
              wind_report_type,
              "; aviation/synoptic wind window was left unassigned"
            )
          } else {
            "no verified rule for this fetch signature and aggregation type"
          }
          unsupported_eccc <- c(
            unsupported_eccc,
            paste(
              series$timeseries_id[[1]],
              series$station_id[[1]],
              parameter,
              interval,
              aggregation,
              review_reason,
              sep = "/"
            )
          )
          next
        }
        for (method in methods) {
          from_epoch <- max(
            as.numeric(archive_from),
            if (method$from == "-infinity") {
              -Inf
            } else {
              as.numeric(as.POSIXct(method$from, tz = "UTC"))
            }
          )
          until_epoch <- min(
            archive_until,
            if (method$until == "infinity") {
              Inf
            } else {
              as.numeric(as.POSIXct(method$until, tz = "UTC"))
            }
          )
          if (from_epoch >= until_epoch) {
            next
          }
          method$from <- format(
            as.POSIXct(from_epoch, origin = "1970-01-01", tz = "UTC"),
            "%Y-%m-%d %H:%M:%S%z"
          )
          method$until <- if (is.infinite(until_epoch)) {
            "infinity"
          } else {
            format(
              as.POSIXct(until_epoch, origin = "1970-01-01", tz = "UTC"),
              "%Y-%m-%d %H:%M:%S%z"
            )
          }
          add_eccc_aggregation_method(
            series = series,
            applicable_from = method$from,
            applicable_until = method$until,
            window_start = method$start,
            window_end = method$end,
            evaluation_interval = method$evaluation_interval,
            evaluation_anchor = method$evaluation_anchor,
            expected_input_interval = method$expected_input_interval,
            calculation_parameters = method$parameters,
            source_reference = method$reference,
            note = method$note,
            operations = method$operations,
            operation_inputs = method$operation_inputs
          )
          created_eccc_methods <- created_eccc_methods + 1L
        }
      }
      rm(add_eccc_aggregation_method)
      if (length(unsupported_eccc) > 0L) {
        message(
          "ECCC fetch signatures without a verified aggregation rule were left unassigned: ",
          paste(unsupported_eccc, collapse = ", ")
        )
      }
      message(
        "Added ",
        created_eccc_methods,
        " ECCC aggregation-method version(s) from active downloadECCCwx fetch arguments."
      )
    } else {
      message(
        "No active downloadECCCwx source adapters were found; no ECCC aggregation methods were seeded."
      )
    }

    # Back-correct measurement periods from the method recipes. For a single
    # window operation, period is the window duration. For a multi-stage recipe
    # such as daily (min + max) / 2, use the common duration of its leaf windows.
    # Evaluation intervals describe output cadence and are not support periods.
    DBI::dbExecute(
      con,
      "CREATE TEMP TABLE ac_patch65_measurement_periods ON COMMIT DROP AS
       WITH terminal_steps AS (
         SELECT
           step.aggregation_method_id,
           step.operation_type,
           step.window_start_offset,
           step.window_end_offset
         FROM continuous.timeseries_aggregation_method_steps step
         WHERE NOT EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_method_step_inputs input
           WHERE input.aggregation_method_id = step.aggregation_method_id
             AND input.input_step_id = step.step_id
         )
       ),
       leaf_periods AS (
         SELECT
           step.aggregation_method_id,
           count(*) FILTER (WHERE leaf_period.period IS NULL)
             AS unresolved_period_count,
           count(DISTINCT leaf_period.period) AS period_count,
           min(leaf_period.period) AS period
         FROM continuous.timeseries_aggregation_method_steps step
         CROSS JOIN LATERAL (
           SELECT CASE
             WHEN step.window_start_offset IS NOT NULL
              AND step.window_end_offset IS NOT NULL
             THEN step.window_end_offset - step.window_start_offset
           END AS period
         ) leaf_period
         WHERE NOT EXISTS (
           SELECT 1
           FROM continuous.timeseries_aggregation_method_step_inputs input
           WHERE input.aggregation_method_id = step.aggregation_method_id
             AND input.step_id = step.step_id
         )
         GROUP BY step.aggregation_method_id
       ),
       resolved_periods AS (
         SELECT
           method.timeseries_id,
           method.applicable_from,
           method.applicable_until,
           CASE
             WHEN terminal.operation_type = 'window_aggregate'
              AND terminal.window_start_offset IS NOT NULL
              AND terminal.window_end_offset IS NOT NULL
             THEN terminal.window_end_offset - terminal.window_start_offset
             WHEN terminal.operation_type = 'arithmetic_mean'
              AND leaves.unresolved_period_count = 0
              AND leaves.period_count = 1
             THEN leaves.period
           END AS period
         FROM continuous.timeseries_aggregation_methods method
         LEFT JOIN terminal_steps terminal
           ON terminal.aggregation_method_id = method.aggregation_method_id
         LEFT JOIN leaf_periods leaves
           ON leaves.aggregation_method_id = method.aggregation_method_id
       )
       SELECT timeseries_id, applicable_from, applicable_until, period
       FROM resolved_periods
       WHERE period > INTERVAL '0'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX ON ac_patch65_measurement_periods (
         timeseries_id,
         applicable_from,
         applicable_until
       )"
    )
    DBI::dbExecute(con, "ANALYZE ac_patch65_measurement_periods")
    period_timeseries <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT timeseries_id
       FROM ac_patch65_measurement_periods
       ORDER BY timeseries_id"
    )
    if (nrow(period_timeseries) > 0L) {
      # Period changes cannot alter series IDs or datetime bounds. Suppress the
      # corresponding no-op metadata trigger and ordinary audit/stamp triggers.
      # Capture changed ranges with UPDATE RETURNING, then use the existing
      # daily refresh helper once, including downstream compound timeseries.
      # This avoids expensive old/new transition-table joins during backfill.
      period_backfill_triggers <- DBI::dbGetQuery(
        con,
        "SELECT trig.tgname, trig.tgenabled::TEXT AS enabled
         FROM pg_trigger trig
         JOIN pg_proc trigger_function
           ON trigger_function.oid = trig.tgfoid
         JOIN pg_namespace function_namespace
           ON function_namespace.oid = trigger_function.pronamespace
         WHERE trig.tgrelid =
           'continuous.measurements_continuous'::regclass
           AND NOT trig.tgisinternal
           AND trig.tgenabled <> 'D'
           AND (
             (
               function_namespace.nspname = 'audit'
               AND trigger_function.proname =
                 'log_measurements_continuous_change'
             )
             OR trigger_function.proname IN (
               'user_modified',
               'update_modified'
             )
             OR (function_namespace.nspname = 'continuous'
               AND trigger_function.proname = 'refresh_basic_metadata_on_measurements_update')
           )
         ORDER BY trig.tgname"
      )
      for (trigger_name in period_backfill_triggers$tgname) {
        DBI::dbExecute(
          con,
          paste0(
            "ALTER TABLE continuous.measurements_continuous DISABLE TRIGGER ",
            as.character(DBI::dbQuoteIdentifier(con, trigger_name))
          )
        )
      }

      previous_daily_refresh <- DBI::dbGetQuery(
        con,
        "SELECT current_setting('continuous.skip_daily_refresh', TRUE) AS value"
      )$value[[1]]
      DBI::dbExecute(con, "SET LOCAL continuous.skip_daily_refresh = 'on'")
      DBI::dbExecute(
        con,
        "CREATE TEMP TABLE calculated_daily_trigger_ranges (
           timeseries_id INTEGER, start_dt TIMESTAMPTZ, end_dt TIMESTAMPTZ
         ) ON COMMIT DROP"
      )

      message(
        "Back-correcting measurement periods for ",
        nrow(period_timeseries),
        " timeseries using their applicable aggregation methods."
      )
      corrected_period_rows <- 0
      for (series_index in seq_len(nrow(period_timeseries))) {
        corrected_period_rows <- corrected_period_rows +
          as.numeric(
            DBI::dbGetQuery(
              con,
              "WITH changed AS (
             UPDATE continuous.measurements_continuous measurement
             SET period = assignment.period
             FROM ac_patch65_measurement_periods assignment
             WHERE assignment.timeseries_id = $1
               AND measurement.timeseries_id = assignment.timeseries_id
               AND measurement.datetime >= assignment.applicable_from
               AND measurement.datetime < assignment.applicable_until
               AND measurement.period IS DISTINCT FROM assignment.period
             RETURNING measurement.timeseries_id, measurement.datetime
             ), ranges AS (
               INSERT INTO calculated_daily_trigger_ranges
               SELECT timeseries_id, min(datetime), max(datetime)
               FROM changed GROUP BY timeseries_id RETURNING timeseries_id
             )
             SELECT count(*)::DOUBLE PRECISION AS n FROM changed",
              params = list(period_timeseries$timeseries_id[[series_index]])
            )$n[[1]]
          )
        if (series_index %% 25L == 0L) {
          message(
            "Processed aggregation-period backfill for ",
            series_index,
            " of ",
            nrow(period_timeseries),
            " timeseries."
          )
        }
      }
      DBI::dbGetQuery(
        con,
        "SELECT continuous.refresh_calculated_daily_from_ranges()"
      )
      DBI::dbExecute(con, "DROP TABLE calculated_daily_trigger_ranges")
      DBI::dbGetQuery(
        con,
        "SELECT set_config('continuous.skip_daily_refresh', $1, TRUE)",
        params = list(
          if (is.na(previous_daily_refresh)) "off" else previous_daily_refresh
        )
      )
      for (trigger_index in seq_len(nrow(period_backfill_triggers))) {
        trigger_name <- period_backfill_triggers$tgname[[trigger_index]]
        enable_mode <- switch(
          period_backfill_triggers$enabled[[trigger_index]],
          A = "ENABLE ALWAYS",
          R = "ENABLE REPLICA",
          O = "ENABLE"
        )
        DBI::dbExecute(
          con,
          paste0(
            "ALTER TABLE continuous.measurements_continuous ",
            enable_mode,
            " TRIGGER ",
            as.character(DBI::dbQuoteIdentifier(con, trigger_name))
          )
        )
      }
      message(
        "Back-corrected ",
        corrected_period_rows,
        " measurement period value(s); no measurement audit rows were recorded for this metadata-only correction."
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '65'
       WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(packageVersion("AquaCache")))
    )

    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "DEV patch 65 applied successfully. Discrete chemistry now has independent reporting bases, chemical forms, analytical targets, endpoints, and analysis conditions, with preserved legacy interpretation and requiredness. Parameter references, ECCC wind aggregation types and windows, measurement periods, continuous aggregation metadata, and the timeseries default-show field were revised; new or modified field results cannot include laboratory metadata."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
