# Patch 40

# Initial checks #################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  paste(
    "Working on Patch 40. Missing schema, relation, function, and column",
    "comments are being added within a transaction so the database can roll",
    "back cleanly if anything fails."
  )
)

message("Starting transaction...")

check <- dbTransCheck(con)
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    `%||%` <- function(x, y) {
      if (
        is.null(x) ||
          length(x) == 0 ||
          all(is.na(x)) ||
          all(trimws(as.character(x)) == "")
      ) {
        y
      } else {
        x
      }
    }

    trim_or_na <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x <- trimws(x)
      x[x == ""] <- NA_character_
      x
    }

    is_blank <- function(x) {
      length(x) == 0 || all(is.na(x)) || all(trimws(as.character(x)) == "")
    }

    relation_key <- function(schema_name, object_name) {
      paste(schema_name, object_name, sep = ".")
    }

    schema_list_sql <- function(target_schemas) {
      sprintf(
        "SELECT unnest(ARRAY[%s]::text[]) AS schema_name",
        paste(sprintf("'%s'", target_schemas), collapse = ", ")
      )
    }

    discover_documented_schemas <- function(con) {
      DBI::dbGetQuery(
        con,
        "
        WITH relation_schemas AS (
          SELECT DISTINCT n.nspname AS schema_name
          FROM pg_class c
          JOIN pg_namespace n
            ON n.oid = c.relnamespace
          WHERE c.relkind IN ('r', 'p', 'v', 'm')
            AND n.nspname !~ '^pg_'
            AND n.nspname <> 'information_schema'
            AND c.relname !~ '^pg_'
            AND NOT EXISTS (
              SELECT 1
              FROM pg_depend dep
              JOIN pg_extension ext
                ON ext.oid = dep.refobjid
              WHERE dep.classid = 'pg_class'::regclass
                AND dep.objid = c.oid
                AND dep.refclassid = 'pg_extension'::regclass
                AND dep.deptype = 'e'
            )
        ),
        routine_schemas AS (
          SELECT DISTINCT n.nspname AS schema_name
          FROM pg_proc p
          JOIN pg_namespace n
            ON n.oid = p.pronamespace
          WHERE p.prokind IN ('f', 'p')
            AND n.nspname !~ '^pg_'
            AND n.nspname <> 'information_schema'
            AND p.proname !~ '^pg_'
            AND NOT EXISTS (
              SELECT 1
              FROM pg_depend dep
              JOIN pg_extension ext
                ON ext.oid = dep.refobjid
              WHERE dep.classid = 'pg_proc'::regclass
                AND dep.objid = p.oid
                AND dep.refclassid = 'pg_extension'::regclass
                AND dep.deptype = 'e'
            )
        )
        SELECT
          n.oid AS schema_oid,
          n.nspname AS schema_name,
          obj_description(n.oid, 'pg_namespace') AS comment
        FROM pg_namespace n
        WHERE n.nspname IN (
          SELECT schema_name FROM relation_schemas
          UNION
          SELECT schema_name FROM routine_schemas
        )
        ORDER BY n.nspname;
        "
      )
    }

    quote_literal_sql <- function(x) {
      as.character(DBI::dbQuoteLiteral(con, x))
    }

    quote_identifier_sql <- function(name) {
      as.character(DBI::dbQuoteIdentifier(con, name))
    }

    quote_relation_sql <- function(schema_name, object_name) {
      as.character(
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(schema = schema_name, table = object_name)
        )
      )
    }

    build_comment_on_schema <- function(schema_name, text) {
      paste0(
        "COMMENT ON SCHEMA ",
        quote_identifier_sql(schema_name),
        " IS ",
        quote_literal_sql(text),
        ";"
      )
    }

    build_comment_on_relation <- function(
      kind_sql,
      schema_name,
      object_name,
      text
    ) {
      paste0(
        "COMMENT ON ",
        kind_sql,
        " ",
        quote_relation_sql(schema_name, object_name),
        " IS ",
        quote_literal_sql(text),
        ";"
      )
    }

    build_comment_on_column <- function(
      schema_name,
      object_name,
      column_name,
      text
    ) {
      paste0(
        "COMMENT ON COLUMN ",
        quote_relation_sql(schema_name, object_name),
        ".",
        quote_identifier_sql(column_name),
        " IS ",
        quote_literal_sql(text),
        ";"
      )
    }

    build_comment_on_routine <- function(
      schema_name,
      function_name,
      identity_args,
      prokind,
      text
    ) {
      routine_kind <- if (identical(prokind, "p")) "PROCEDURE" else "FUNCTION"
      paste0(
        "COMMENT ON ",
        routine_kind,
        " ",
        quote_identifier_sql(schema_name),
        ".",
        quote_identifier_sql(function_name),
        "(",
        identity_args %||% "",
        ") IS ",
        quote_literal_sql(text),
        ";"
      )
    }

    blank_mask <- function(x) {
      is.na(trim_or_na(x))
    }

    get_lookup_df <- function(lookup, key, empty_df) {
      out <- lookup[[key]]
      if (is.null(out)) empty_df else out
    }

    execute_statement_batches <- function(
      statements,
      label,
      chunk_size = 100L
    ) {
      if (length(statements) == 0L) {
        return(invisible(0L))
      }

      n_batches <- ceiling(length(statements) / chunk_size)
      message(
        sprintf(
          "Applying %d %s comment statement(s) in %d batch(es)...",
          length(statements),
          label,
          n_batches
        )
      )

      for (start in seq.int(1L, length(statements), by = chunk_size)) {
        end <- min(start + chunk_size - 1L, length(statements))
        DBI::dbExecute(
          con,
          paste(statements[start:end], collapse = "\n"),
          immediate = TRUE
        )
      }

      invisible(length(statements))
    }

    schema_docs <- c(
      application = "Application-managed content, notifications, and usage telemetry for client applications.",
      audit = "Audit logging and point-in-time reconstruction helpers for tracking metadata and measurement edits.",
      boreholes = "Groundwater and borehole inventory tables, including borehole construction, geology, permafrost, and linked documents.",
      continuous = "Automated or manual entry time-series definitions, observations, corrections, quality flags, forecasts, and other continuous-data business rules.",
      discrete = "Manual and laboratory sample workflows, analytical results, guideline lookups, and sampling metadata.",
      field = "Field visit records and the assets or instruments associated with a visit.",
      files = "Documents, images, image-series metadata, and related file-classification tables.",
      information = "Internal database status and version-tracking tables used by AquaCache itself.",
      instruments = "Instrument inventory, maintenance, calibration, communications, and deployment-related reference data.",
      public = "Shared core reference tables such as locations, organizations, parameters, units, permissions, and normalized location metadata.",
      spatial = "Raster and vector storage plus indexing tables for spatial series and derived spatial metadata."
    )

    table_docs <- c(
      "application.api_requests" = "Captures application or API request history, likely for monitoring, diagnostics, throttling, or troubleshooting.",
      "application.feedback" = "Stores end-user feedback submitted through an application interface, usually as a lightweight content or support workflow table.",
      "application.images" = "Stores application-managed image assets that can be placed into CMS-style content blocks such as pages or announcements.",
      "application.shiny_app_usage" = "Stores one record per Shiny application session or usage episode so higher-volume event rows can roll up to a parent usage record.",
      "boreholes.borehole_well_purposes" = "Lookup table of valid purpose categories for wells associated with boreholes.",
      "boreholes.boreholes" = "Primary borehole inventory for boreholes that have accepted coordinates and can be mapped directly.",
      "boreholes.boreholes_documents" = "Associative table that links mapped boreholes to file records, so reports or logs can be attached to a borehole.",
      "boreholes.boreholes_no_coords" = "Secondary borehole inventory for boreholes that are known but do not yet have usable coordinates.",
      "boreholes.boreholes_no_coords_documents" = "Associative table linking non-spatial borehole records to supporting documents.",
      "boreholes.casing_materials" = "Reference table of valid casing materials used when describing well or borehole construction.",
      "boreholes.drillers" = "Reference table of drillers or drilling companies associated with borehole construction records.",
      "boreholes.geology" = "Stores geology or lithology intervals observed within a borehole, usually by depth range.",
      "boreholes.permafrost" = "Stores permafrost intervals or classifications recorded for a borehole, typically by depth range.",
      "boreholes.wells" = "Stores well-specific metadata connected to a borehole, such as construction, use, or operational attributes.",
      "continuous.aggregation_types" = "Reference table defining how a time-series value represents time, such as instantaneous, mean, sum, minimum, or maximum.",
      "continuous.approvals" = "Time-bounded approval history for a time series, letting the approved status change over date ranges.",
      "continuous.contributors" = "Time-bounded record of contributing organizations or parties for each time series.",
      "continuous.grades" = "Time-bounded grade history for each time series so quality grades can change over time.",
      "continuous.owners" = "Time-bounded ownership history for each time series.",
      "continuous.qualifiers" = "Time-bounded qualifier history for each time series, such as conditions or context flags.",
      "continuous.rating_curves" = "Stores rating-curve headers or versions used to convert one measurement domain into another, typically stage to discharge.",
      "continuous.rating_curve_shifts" = "Stores temporal shifts or offsets that modify a rating curve for a given period.",
      "continuous.rating_curve_points" = "Stores the individual points that make up a rating curve or a rating-curve segment.",
      "discrete.collection_methods" = "Reference table of field collection methods used when taking discrete samples.",
      "discrete.guideline_publishers" = "Reference table of organizations that publish water-quality or sample-result guidelines.",
      "discrete.guideline_series" = "Reference table grouping guidelines into a named publication series or edition.",
      "discrete.guidelines" = "Stores published guideline thresholds used to interpret discrete analytical results.",
      "discrete.laboratories" = "Reference table of laboratories that analyze discrete samples.",
      "discrete.protocols_methods" = "Reference table of protocols or analytical methods used in discrete sampling and laboratory work.",
      "discrete.result_conditions" = "Reference table of result-condition qualifiers such as below detection, estimated, or otherwise condition-coded values.",
      "discrete.result_speciations" = "Reference table for chemical or analytical speciation categories used on results.",
      "discrete.result_types" = "Reference table classifying what a result value represents, such as observed, duplicate, blank, or derived values.",
      "discrete.result_value_types" = "Reference table defining the form of the stored result value, such as numeric, textual, or categorical.",
      "discrete.results" = "Stores one analytical result per sample, parameter, and related analytical qualifiers.",
      "discrete.sample_fractions" = "Reference table of sample fractions such as dissolved, total, filtered, or unfiltered.",
      "discrete.sample_types" = "Reference table of sample types or sampling programs.",
      "discrete.samples" = "Stores the sampled event itself: where, when, how, and under what media a discrete sample was collected.",
      "files.document_types" = "Reference table defining the types of documents that can be attached to AquaCache records.",
      "files.documents_spatial" = "Associative table linking documents to spatial features or spatial records.",
      "files.image_types" = "Reference table defining the types of images stored in the files schema.",
      "information.version_info" = "Stores internal database version and patch-tracking values used by AquaCache maintenance routines.",
      "instruments.calibrations" = "Stores one parent calibration event per instrument or sensor calibration activity.",
      "instruments.instrument_maintenance" = "Stores maintenance events performed on an instrument or instrument assembly.",
      "instruments.instrument_make" = "Reference table of instrument manufacturers or makes.",
      "instruments.instrument_model" = "Reference table of instrument models, usually linked to a make and type.",
      "instruments.instrument_type" = "Reference table of high-level instrument categories such as logger, sensor, modem, or controller.",
      "instruments.instruments" = "Master inventory of physical instruments deployed, maintained, or tracked by AquaCache.",
      "instruments.observers" = "Reference table of people who perform field observations, calibrations, or maintenance tasks.",
      "instruments.sensor_types" = "Reference table of sensor categories used to classify sensors.",
      "instruments.sensors" = "Inventory of sensors, often linked to a parent instrument, model, or deployment metadata.",
      "instruments.suppliers" = "Reference table of suppliers or vendors for instruments and parts.",
      "public.ac_append_rls" = "Internal helper table supporting row-level-security-aware append operations from application code or helper functions.",
      "public.approval_types" = "Reference table of approval categories used by continuous and related quality workflows.",
      "public.grade_types" = "Reference table of grade categories used to classify data quality or record status.",
      "public.location_types" = "Reference table defining the types of locations stored in AquaCache, such as station, site, or feature categories.",
      "public.locations" = "Master location table for physical monitoring sites, stations, or other spatially meaningful assets.",
      "public.locations_metadata_instruments" = "Stores time-bounded instrument deployment metadata for a location.",
      "public.locations_metadata_maintenance" = "Stores maintenance history or maintenance metadata linked to a location and its instrumentation.",
      "public.locations_networks" = "Associative table connecting locations to monitoring networks.",
      "public.locations_projects" = "Associative table connecting locations to projects or initiatives.",
      "public.media_types" = "Reference table defining media such as water, air, snow, or soil and their default matrix-state behavior.",
      "public.network_project_types" = "Reference table classifying how a network or project relationship should be interpreted.",
      "public.networks" = "Reference table of monitoring networks, programs, or organizational groupings of locations.",
      "public.organization_data_sharing_agreements" = "Associative table linking organizations to data-sharing agreement documents.",
      "public.organizations" = "Reference table of organizations that own, contribute to, operate, or otherwise relate to AquaCache assets.",
      "public.parameter_groups" = "Reference table for high-level parameter groupings.",
      "public.parameter_relationships" = "Stores explicit relationships between parameters, such as derived or paired parameter mappings.",
      "public.parameter_sub_groups" = "Reference table for more specific parameter subgroupings beneath a broader parameter group.",
      "public.projects" = "Reference table of projects, initiatives, or work programs linked to locations or networks.",
      "public.qualifier_types" = "Reference table of qualifier categories used in time-series workflows.",
      "public.sub_locations" = "Child location dimension used to distinguish sub-sites or measurement points beneath a main location.",
      "public.units" = "Reference table of measurement units used across parameters and results.",
      "spatial.raster_types" = "Reference table classifying raster products, such as observed, forecast, or derived raster families.",
      "spatial.spatial_ref_sys" = "Reference table of spatial reference systems. This is PostGIS-managed rather than AquaCache-specific business data."
    )

    view_docs <- c(
      "continuous.measurements_calculated_daily_corrected" = "Convenience view that exposes stored daily values after applying currently effective corrections and security filtering.",
      "continuous.measurements_continuous_corrected" = "Convenience view that exposes raw continuous measurements alongside correction-adjusted values visible to the current caller.",
      "continuous.timeseries_metadata_en" = "English-language view that flattens key time-series metadata into a reader-friendly dataset for applications and exports.",
      "continuous.timeseries_metadata_fr" = "French-language view that flattens key time-series metadata into a reader-friendly dataset for applications and exports.",
      "public.location_metadata_en" = "English-language flattened location metadata view assembled from the normalized location tables.",
      "public.location_metadata_fr" = "French-language flattened location metadata view assembled from the normalized location tables."
    )

    function_docs <- c(
      "audit.jsonb_changed_fields" = "Compares two JSONB row snapshots and returns only the fields whose values changed.",
      "audit.log_measurements_calculated_daily_change" = "Trigger function that writes daily measurement changes into the dedicated audit log table.",
      "audit.log_measurements_continuous_change" = "Trigger function that writes continuous measurement changes into the dedicated audit log table.",
      "continuous.delete_old_forecasts" = "Maintenance routine that removes forecast rows that are no longer meant to remain in the database.",
      "continuous.trunc_hour_utc" = "Helper function that truncates a timestamp to the start of its UTC hour, mainly to support indexing and grouping.",
      "public.default_matrix_state_id_from_media" = "Helper that returns the default matrix-state identifier associated with a given media type.",
      "public.drop_role_if_unused" = "Administrative helper that drops a database role only if it no longer has references in share-with style columns.",
      "public.get_parameter_unit_id" = "Returns the unit identifier that should be used for a parameter under a given matrix state.",
      "public.get_parameter_unit_name" = "Returns the display name of the unit resolved for a parameter under a given matrix state.",
      "public.get_shareable_principals_for" = "Lists roles that are eligible to receive shared access for a relation and privilege set.",
      "public.get_unique_parameter_matrix_state_id" = "Returns the unique matrix-state identifier for a parameter when exactly one valid mapping exists.",
      "public.parameter_has_unit_for_matrix_state" = "Checks whether a parameter has a valid unit mapping for a given matrix state.",
      "public.resolve_matrix_state_id" = "Resolves the correct matrix-state identifier from media, parameter, and optionally an explicit matrix-state value.",
      "public.update_modified" = "Trigger helper that stamps the modified timestamp when a row is updated.",
      "public.user_modified" = "Trigger helper that records which database role last modified the row.",
      "public.validate_documents_array" = "Validates the structure or contents of a document-id array before the row is written.",
      "public.validate_share_with" = "Validates share-with role arrays so row-level sharing only references acceptable roles.",
      "spatial.update_geom_type" = "Trigger helper that derives or refreshes the stored geometry-type label when a vector geometry changes."
    )

    infer_schema_description <- function(
      schema_name,
      comment,
      table_count,
      view_count,
      function_count
    ) {
      if (!is_blank(comment)) {
        return(trim_or_na(comment)[1])
      }

      manual <- unname(schema_docs[schema_name])
      if (!is.na(manual)) {
        return(manual)
      }

      parts <- c()
      if (table_count > 0) {
        parts <- c(
          parts,
          sprintf("%d table%s", table_count, if (table_count == 1) "" else "s")
        )
      }
      if (view_count > 0) {
        parts <- c(
          parts,
          sprintf("%d view%s", view_count, if (view_count == 1) "" else "s")
        )
      }
      if (function_count > 0) {
        parts <- c(
          parts,
          sprintf(
            "%d function%s",
            function_count,
            if (function_count == 1) "" else "s"
          )
        )
      }

      if (length(parts) == 0) {
        return(
          "User-defined schema discovered from the live catalog. Add a COMMENT ON SCHEMA statement to document its purpose."
        )
      }

      sprintf(
        "User-defined schema discovered from the live catalog, currently containing %s. Add a COMMENT ON SCHEMA statement to document its purpose more precisely.",
        paste(parts, collapse = ", ")
      )
    }

    infer_table_comment <- function(
      schema_name,
      table_name,
      comment,
      outgoing_fks,
      incoming_fks,
      columns_df
    ) {
      if (!is_blank(comment)) {
        return(trim_or_na(comment)[1])
      }

      full_name <- relation_key(schema_name, table_name)
      manual <- unname(table_docs[full_name])
      if (!is.na(manual)) {
        return(manual)
      }

      if (
        schema_name == "instruments" && startsWith(table_name, "calibrate_")
      ) {
        sensor_name <- gsub("^calibrate_", "", table_name)
        sensor_name <- gsub("_", " ", sensor_name)
        return(sprintf(
          "Stores calibration details specific to %s measurements. Rows normally hang off a parent record in instruments.calibrations.",
          sensor_name
        ))
      }

      if (
        schema_name == "public" && startsWith(table_name, "locations_metadata_")
      ) {
        suffix <- gsub("^locations_metadata_", "", table_name)
        suffix <- gsub("_", " ", suffix)
        return(sprintf(
          "Normalized child table for location metadata focused on %s. It extends the core location record without overloading public.locations.",
          suffix
        ))
      }

      if (schema_name == "files" && grepl("_series$", table_name)) {
        noun <- gsub("_series$", "", table_name)
        noun <- gsub("_", " ", noun)
        return(sprintf(
          "Series-level metadata table for grouped %s records, allowing many related files to be managed as one logical set.",
          noun
        ))
      }

      fk_count <- nrow(outgoing_fks)
      incoming_count <- nrow(incoming_fks)
      id_like <- grepl("_id$", columns_df$column_name)
      audit_like <- columns_df$column_name %in%
        c(
          "created",
          "modified",
          "created_by",
          "modified_by",
          "note",
          "active",
          "share_with",
          "private_expiry"
        )
      business_columns <- sum(!(id_like | audit_like))

      if (fk_count >= 2 && business_columns <= 2) {
        parents <- unique(relation_key(
          outgoing_fks$referenced_schema,
          outgoing_fks$referenced_table
        ))
        return(sprintf(
          "Association table that links %s and carries minimal additional business attributes beyond the relationship itself.",
          paste(parents, collapse = ", ")
        ))
      }

      if (
        grepl(
          "(types|groups|states|methods|publishers|series|suppliers|observers)$",
          table_name
        )
      ) {
        pretty <- gsub("_", " ", table_name)
        return(sprintf(
          "Reference table that standardizes the allowed values for %s across the database.",
          pretty
        ))
      }

      relation_hint <- if (fk_count > 0) {
        sprintf(
          " It links outward to %d table(s) and is referenced by %d table(s).",
          fk_count,
          incoming_count
        )
      } else {
        ""
      }

      sprintf(
        "Stores %s records within the %s schema.%s",
        gsub("_", " ", table_name),
        schema_name,
        relation_hint
      )
    }

    infer_view_comment <- function(
      schema_name,
      view_name,
      comment,
      dependencies
    ) {
      if (!is_blank(comment)) {
        return(trim_or_na(comment)[1])
      }

      full_name <- relation_key(schema_name, view_name)
      manual <- unname(view_docs[full_name])
      if (!is.na(manual)) {
        return(manual)
      }

      sprintf(
        "Derived relation that packages underlying tables into a reusable read model. The definition currently depends on %d upstream relation(s).",
        nrow(dependencies)
      )
    }

    make_relation_regex <- function(schema_name, object_name) {
      schema_name <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", schema_name)
      object_name <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", object_name)
      paste0(
        "(?i)(?:\"",
        schema_name,
        "\"|",
        schema_name,
        ")\\s*\\.\\s*(?:\"",
        object_name,
        "\"|",
        object_name,
        ")"
      )
    }

    detect_relation_mentions <- function(definition, relation_catalog) {
      hits <- relation_catalog$vkey[vapply(
        relation_catalog$regex,
        function(pattern) grepl(pattern, definition, perl = TRUE),
        logical(1)
      )]
      unique(hits)
    }

    infer_function_comment <- function(
      schema_name,
      function_name,
      comment,
      returns,
      used_by_triggers,
      relation_mentions
    ) {
      if (!is_blank(comment)) {
        return(trim_or_na(comment)[1])
      }

      full_name <- relation_key(schema_name, function_name)
      manual <- unname(function_docs[full_name])
      if (!is.na(manual)) {
        return(manual)
      }

      if (identical(trimws(tolower(returns)), "trigger")) {
        if (nrow(used_by_triggers) > 0) {
          tables <- unique(relation_key(
            used_by_triggers$schema_name,
            used_by_triggers$table_name
          ))
          return(sprintf(
            "Trigger function used by %d trigger(s) across %d table(s): %s.",
            nrow(used_by_triggers),
            length(tables),
            paste(utils::head(tables, 5), collapse = ", ")
          ))
        }

        return(
          "Trigger function that enforces a business rule or maintains derived values when rows are inserted, updated, or deleted."
        )
      }

      if (startsWith(function_name, "check_")) {
        return(
          "Validation routine that checks a business rule and normally raises an error when invalid data would be written."
        )
      }

      if (startsWith(function_name, "fill_")) {
        return(
          "Helper routine that fills related metadata records or missing defaults needed elsewhere in the schema."
        )
      }

      if (startsWith(function_name, "enforce_")) {
        return(
          "Business-rule routine that enforces a consistency requirement before data is accepted."
        )
      }

      if (startsWith(function_name, "prevent_")) {
        return(
          "Guard routine that blocks a write when it would create an invalid overlap or forbidden state."
        )
      }

      if (startsWith(function_name, "get_")) {
        return(
          "Lookup or derivation helper that returns a value or small result set needed by application logic or constraints."
        )
      }

      if (length(relation_mentions) > 0) {
        return(sprintf(
          "Custom %s routine that appears to work with %d documented relation(s): %s.",
          if (grepl("^table\\(", tolower(returns))) {
            "table-returning"
          } else {
            "database"
          },
          length(relation_mentions),
          paste(utils::head(relation_mentions, 5), collapse = ", ")
        ))
      }

      "Custom database routine defined inside the AquaCache schemas. Review the signature and SQL body for exact behavior."
    }

    infer_column_comment <- function(column_name, comment, fk_rows, data_type) {
      if (!is_blank(comment)) {
        return(trim_or_na(comment)[1])
      }

      if (nrow(fk_rows) == 1 && identical(fk_rows$column_list, column_name)) {
        return(sprintf(
          "References %s.%s(%s).",
          fk_rows$referenced_schema,
          fk_rows$referenced_table,
          fk_rows$referenced_columns %||% "key column"
        ))
      }

      manual <- switch(
        column_name,
        created = "Timestamp when the row was created.",
        modified = "Timestamp when the row was last updated.",
        created_by = "Database role or application actor recorded as the row creator.",
        modified_by = "Database role or application actor recorded as the last modifier.",
        active = "Boolean flag indicating whether the record should currently be treated as active.",
        note = "Free-text note field for context that does not fit a more structured column.",
        description = "Human-readable description or explanatory text.",
        share_with = "Array of database roles allowed to see or use the row under the sharing model.",
        private_expiry = "Date after which a temporary privacy restriction is expected to lapse.",
        tags = "Tag array or tag-like collection used for lightweight classification and search.",
        start_datetime = "Start of the time span represented by the row.",
        end_datetime = "End of the time span represented by the row.",
        start_dt = "Start of the time span represented by the row.",
        end_dt = "End of the time span represented by the row.",
        datetime = "Timestamp associated with the observation, event, or record.",
        date = "Date associated with the observation, event, or record.",
        id = "Surrogate identifier for the row.",
        NULL
      )

      if (!is.null(manual)) {
        return(manual)
      }

      if (grepl("_id$", column_name)) {
        stem <- gsub("_id$", "", column_name)
        stem <- gsub("_", " ", stem)
        return(sprintf("Identifier field for the related %s record.", stem))
      }

      if (
        grepl("^is_|_flag$", column_name) || identical(data_type, "boolean")
      ) {
        return("Boolean flag column used to capture a yes or no style state.")
      }

      NA_character_
    }

    check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regnamespace('information') IS NOT NULL AS has_information_schema,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info;"
    )
    if (
      !isTRUE(check$has_information_schema[[1]]) ||
        !isTRUE(check$has_version_info[[1]])
    ) {
      stop(
        "Patch 40 requires information.version_info to already exist."
      )
    }

    documented_schemas <- discover_documented_schemas(con)
    target_schemas <- documented_schemas$schema_name
    if (length(target_schemas) == 0) {
      stop(
        "Patch 40 did not find any non-system AquaCache schemas to document."
      )
    }
    schema_sql <- schema_list_sql(target_schemas)

    tables <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          c.oid AS object_oid,
          n.nspname AS schema_name,
          c.relname AS object_name,
          obj_description(c.oid, 'pg_class') AS comment
        FROM pg_class c
        JOIN pg_namespace n
          ON n.oid = c.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = n.nspname
        WHERE c.relkind IN ('r', 'p')
          AND c.relname !~ '^pg_'
          AND NOT EXISTS (
            SELECT 1
            FROM pg_depend dep
            JOIN pg_extension ext
              ON ext.oid = dep.refobjid
            WHERE dep.classid = 'pg_class'::regclass
              AND dep.objid = c.oid
              AND dep.refclassid = 'pg_extension'::regclass
              AND dep.deptype = 'e'
          )
        ORDER BY n.nspname, c.relname;
        ",
        schema_sql
      )
    )
    tables$key <- relation_key(tables$schema_name, tables$object_name)

    views <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          c.oid AS object_oid,
          n.nspname AS schema_name,
          c.relname AS object_name,
          c.relkind,
          obj_description(c.oid, 'pg_class') AS comment
        FROM pg_class c
        JOIN pg_namespace n
          ON n.oid = c.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = n.nspname
        WHERE c.relkind IN ('v', 'm')
          AND c.relname !~ '^pg_'
          AND NOT EXISTS (
            SELECT 1
            FROM pg_depend dep
            JOIN pg_extension ext
              ON ext.oid = dep.refobjid
            WHERE dep.classid = 'pg_class'::regclass
              AND dep.objid = c.oid
              AND dep.refclassid = 'pg_extension'::regclass
              AND dep.deptype = 'e'
          )
        ORDER BY n.nspname, c.relname;
        ",
        schema_sql
      )
    )
    views$key <- relation_key(views$schema_name, views$object_name)

    columns <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          c.oid AS object_oid,
          n.nspname AS schema_name,
          c.relname AS object_name,
          c.relkind,
          a.attnum AS ordinal_position,
          a.attname AS column_name,
          format_type(a.atttypid, a.atttypmod) AS data_type,
          col_description(c.oid, a.attnum) AS comment
        FROM pg_class c
        JOIN pg_namespace n
          ON n.oid = c.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = n.nspname
        JOIN pg_attribute a
          ON a.attrelid = c.oid
         AND a.attnum > 0
         AND NOT a.attisdropped
        WHERE c.relkind IN ('r', 'p', 'v', 'm')
          AND c.relname !~ '^pg_'
          AND NOT EXISTS (
            SELECT 1
            FROM pg_depend dep
            JOIN pg_extension ext
              ON ext.oid = dep.refobjid
            WHERE dep.classid = 'pg_class'::regclass
              AND dep.objid = c.oid
              AND dep.refclassid = 'pg_extension'::regclass
              AND dep.deptype = 'e'
          )
        ORDER BY n.nspname, c.relname, a.attnum;
        ",
        schema_sql
      )
    )
    columns$key <- relation_key(columns$schema_name, columns$object_name)

    constraints <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          con.oid AS constraint_oid,
          src_ns.nspname AS schema_name,
          src.relname AS table_name,
          con.conname,
          con.contype,
          string_agg(src_att.attname, ', ' ORDER BY src_key.ord) AS column_list,
          ref_ns.nspname AS referenced_schema,
          ref_cls.relname AS referenced_table,
          string_agg(ref_att.attname, ', ' ORDER BY ref_key.ord) AS referenced_columns
        FROM pg_constraint con
        JOIN pg_class src
          ON src.oid = con.conrelid
        JOIN pg_namespace src_ns
          ON src_ns.oid = src.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = src_ns.nspname
        LEFT JOIN LATERAL unnest(COALESCE(con.conkey, ARRAY[]::smallint[]))
          WITH ORDINALITY AS src_key(attnum, ord)
          ON true
        LEFT JOIN pg_attribute src_att
          ON src_att.attrelid = src.oid
         AND src_att.attnum = src_key.attnum
        LEFT JOIN pg_class ref_cls
          ON ref_cls.oid = con.confrelid
        LEFT JOIN pg_namespace ref_ns
          ON ref_ns.oid = ref_cls.relnamespace
        LEFT JOIN LATERAL unnest(COALESCE(con.confkey, ARRAY[]::smallint[]))
          WITH ORDINALITY AS ref_key(attnum, ord)
          ON ref_key.ord = src_key.ord
        LEFT JOIN pg_attribute ref_att
          ON ref_att.attrelid = ref_cls.oid
         AND ref_att.attnum = ref_key.attnum
        WHERE con.contype IN ('p', 'u', 'f', 'c', 'x')
        GROUP BY
          con.oid,
          src_ns.nspname,
          src.relname,
          con.conname,
          con.contype,
          ref_ns.nspname,
          ref_cls.relname
        ORDER BY src_ns.nspname, src.relname, con.contype, con.conname;
        ",
        schema_sql
      )
    )
    constraints$key <- relation_key(
      constraints$schema_name,
      constraints$table_name
    )

    triggers <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          trg.oid AS trigger_oid,
          ns.nspname AS schema_name,
          tbl.relname AS table_name,
          trg.tgname AS trigger_name,
          fn.oid AS function_oid,
          fn_ns.nspname AS function_schema,
          fn.proname AS function_name,
          pg_get_function_identity_arguments(fn.oid) AS function_identity_args
        FROM pg_trigger trg
        JOIN pg_class tbl
          ON tbl.oid = trg.tgrelid
        JOIN pg_namespace ns
          ON ns.oid = tbl.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = ns.nspname
        JOIN pg_proc fn
          ON fn.oid = trg.tgfoid
        JOIN pg_namespace fn_ns
          ON fn_ns.oid = fn.pronamespace
        WHERE tbl.relkind IN ('r', 'p')
          AND NOT trg.tgisinternal
        ORDER BY ns.nspname, tbl.relname, trg.tgname;
        ",
        schema_sql
      )
    )

    view_dependencies <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT DISTINCT
          src_ns.nspname AS view_schema,
          src_cls.relname AS view_name,
          dep_ns.nspname AS dependency_schema,
          dep_cls.relname AS dependency_name,
          dep_cls.relkind AS dependency_kind
        FROM pg_rewrite rw
        JOIN pg_class src_cls
          ON src_cls.oid = rw.ev_class
        JOIN pg_namespace src_ns
          ON src_ns.oid = src_cls.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = src_ns.nspname
        JOIN pg_depend dep
          ON dep.objid = rw.oid
        JOIN pg_class dep_cls
          ON dep_cls.oid = dep.refobjid
        JOIN pg_namespace dep_ns
          ON dep_ns.oid = dep_cls.relnamespace
        WHERE src_cls.relkind IN ('v', 'm')
          AND dep_cls.relkind IN ('r', 'p', 'v', 'm')
          AND dep_cls.oid <> src_cls.oid
        ORDER BY src_ns.nspname, src_cls.relname, dep_ns.nspname, dep_cls.relname;
        ",
        schema_sql
      )
    )
    view_dependencies$key <- relation_key(
      view_dependencies$view_schema,
      view_dependencies$view_name
    )

    functions <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          p.oid AS function_oid,
          n.nspname AS schema_name,
          p.proname AS function_name,
          p.prokind,
          pg_get_function_identity_arguments(p.oid) AS identity_args,
          pg_get_function_result(p.oid) AS returns,
          obj_description(p.oid, 'pg_proc') AS comment,
          CASE
            WHEN obj_description(p.oid, 'pg_proc') IS NULL
              THEN pg_get_functiondef(p.oid)
            ELSE NULL
          END AS definition
        FROM pg_proc p
        JOIN pg_namespace n
          ON n.oid = p.pronamespace
        JOIN target_schemas ts
          ON ts.schema_name = n.nspname
        WHERE p.prokind IN ('f', 'p')
          AND p.proname !~ '^pg_'
          AND NOT EXISTS (
            SELECT 1
            FROM pg_depend dep
            JOIN pg_extension ext
              ON ext.oid = dep.refobjid
            WHERE dep.classid = 'pg_proc'::regclass
              AND dep.objid = p.oid
              AND dep.refclassid = 'pg_extension'::regclass
              AND dep.deptype = 'e'
          )
        ORDER BY n.nspname, p.proname, pg_get_function_identity_arguments(p.oid);
        ",
        schema_sql
      )
    )

    schema_counts <- data.frame(
      schema_name = target_schemas,
      schema_comment = documented_schemas$comment[
        match(target_schemas, documented_schemas$schema_name)
      ],
      table_count = vapply(
        target_schemas,
        function(s) sum(tables$schema_name == s),
        integer(1)
      ),
      view_count = vapply(
        target_schemas,
        function(s) sum(views$schema_name == s),
        integer(1)
      ),
      function_count = vapply(
        target_schemas,
        function(s) sum(functions$schema_name == s),
        integer(1)
      ),
      stringsAsFactors = FALSE
    )

    missing_schemas <- schema_counts[
      blank_mask(schema_counts$schema_comment),
      ,
      drop = FALSE
    ]
    missing_tables <- tables[blank_mask(tables$comment), , drop = FALSE]
    missing_views <- views[blank_mask(views$comment), , drop = FALSE]
    missing_functions <- functions[
      blank_mask(functions$comment),
      ,
      drop = FALSE
    ]
    missing_columns <- columns[blank_mask(columns$comment), , drop = FALSE]

    message(
      sprintf(
        paste(
          "Patch 40 found %d schema comments, %d table comments,",
          "%d view comments, %d function comments, and %d column comments",
          "still missing."
        ),
        nrow(missing_schemas),
        nrow(missing_tables),
        nrow(missing_views),
        nrow(missing_functions),
        nrow(missing_columns)
      )
    )

    fk_constraints <- constraints[constraints$contype == "f", , drop = FALSE]
    table_columns <- columns[columns$relkind %in% c("r", "p"), , drop = FALSE]
    single_column_fks <- fk_constraints[
      !is.na(fk_constraints$column_list) &
        !grepl(",", fk_constraints$column_list, fixed = TRUE),
      ,
      drop = FALSE
    ]

    outgoing_fks_by_key <- if (nrow(fk_constraints) > 0) {
      split(fk_constraints, fk_constraints$key)
    } else {
      list()
    }
    incoming_fks_by_relation <- if (nrow(fk_constraints) > 0) {
      split(
        fk_constraints,
        relation_key(
          fk_constraints$referenced_schema,
          fk_constraints$referenced_table
        )
      )
    } else {
      list()
    }
    columns_by_key <- if (nrow(table_columns) > 0) {
      split(table_columns, table_columns$key)
    } else {
      list()
    }
    view_dependencies_by_key <- if (nrow(view_dependencies) > 0) {
      split(view_dependencies, view_dependencies$key)
    } else {
      list()
    }
    triggers_by_routine <- if (nrow(triggers) > 0) {
      split(
        triggers,
        paste(
          triggers$function_schema,
          triggers$function_name,
          triggers$function_identity_args,
          sep = "\r"
        )
      )
    } else {
      list()
    }
    column_fks_by_key <- if (nrow(single_column_fks) > 0) {
      split(
        single_column_fks,
        paste(single_column_fks$key, single_column_fks$column_list, sep = "\r")
      )
    } else {
      list()
    }

    relation_catalog <- data.frame(
      schema_name = character(),
      object_name = character(),
      vkey = character(),
      regex = character(),
      stringsAsFactors = FALSE
    )
    if (nrow(missing_functions) > 0) {
      relation_catalog <- rbind(
        data.frame(
          schema_name = tables$schema_name,
          object_name = tables$object_name,
          vkey = tables$key,
          stringsAsFactors = FALSE
        ),
        data.frame(
          schema_name = views$schema_name,
          object_name = views$object_name,
          vkey = views$key,
          stringsAsFactors = FALSE
        )
      )
      relation_catalog$regex <- mapply(
        make_relation_regex,
        relation_catalog$schema_name,
        relation_catalog$object_name,
        USE.NAMES = FALSE
      )
    }

    schema_statements <- character()
    for (i in seq_len(nrow(missing_schemas))) {
      row <- missing_schemas[i, , drop = FALSE]
      text <- infer_schema_description(
        schema_name = row$schema_name[1],
        comment = row$schema_comment[1],
        table_count = row$table_count[1],
        view_count = row$view_count[1],
        function_count = row$function_count[1]
      )
      if (!is_blank(text)) {
        schema_statements <- c(
          schema_statements,
          build_comment_on_schema(row$schema_name[1], text)
        )
      }
    }
    schema_updates <- length(schema_statements)
    execute_statement_batches(schema_statements, "schema")

    fk_empty <- fk_constraints[0, , drop = FALSE]
    table_columns_empty <- table_columns[0, , drop = FALSE]
    view_dependencies_empty <- view_dependencies[0, , drop = FALSE]
    triggers_empty <- triggers[0, , drop = FALSE]

    table_statements <- character()
    for (i in seq_len(nrow(missing_tables))) {
      row <- missing_tables[i, , drop = FALSE]
      key <- row$key[1]
      text <- infer_table_comment(
        schema_name = row$schema_name[1],
        table_name = row$object_name[1],
        comment = row$comment[1],
        outgoing_fks = get_lookup_df(outgoing_fks_by_key, key, fk_empty),
        incoming_fks = get_lookup_df(
          incoming_fks_by_relation,
          relation_key(row$schema_name[1], row$object_name[1]),
          fk_empty
        ),
        columns_df = get_lookup_df(columns_by_key, key, table_columns_empty)
      )
      if (!is_blank(text)) {
        table_statements <- c(
          table_statements,
          build_comment_on_relation(
            "TABLE",
            row$schema_name[1],
            row$object_name[1],
            text
          )
        )
      }
    }
    table_updates <- length(table_statements)
    execute_statement_batches(table_statements, "table")

    view_statements <- character()
    for (i in seq_len(nrow(missing_views))) {
      row <- missing_views[i, , drop = FALSE]
      text <- infer_view_comment(
        schema_name = row$schema_name[1],
        view_name = row$object_name[1],
        comment = row$comment[1],
        dependencies = get_lookup_df(
          view_dependencies_by_key,
          row$key[1],
          view_dependencies_empty
        )
      )
      if (!is_blank(text)) {
        relation_kind <- if (identical(row$relkind[1], "m")) {
          "MATERIALIZED VIEW"
        } else {
          "VIEW"
        }
        view_statements <- c(
          view_statements,
          build_comment_on_relation(
            relation_kind,
            row$schema_name[1],
            row$object_name[1],
            text
          )
        )
      }
    }
    view_updates <- length(view_statements)
    execute_statement_batches(view_statements, "view")

    function_statements <- character()
    for (i in seq_len(nrow(missing_functions))) {
      row <- missing_functions[i, , drop = FALSE]
      relation_mentions <- detect_relation_mentions(
        row$definition[1] %||% "",
        relation_catalog
      )
      relation_mentions <- setdiff(
        relation_mentions,
        relation_key(row$schema_name[1], row$function_name[1])
      )
      text <- infer_function_comment(
        schema_name = row$schema_name[1],
        function_name = row$function_name[1],
        comment = row$comment[1],
        returns = row$returns[1],
        used_by_triggers = get_lookup_df(
          triggers_by_routine,
          paste(
            row$schema_name[1],
            row$function_name[1],
            row$identity_args[1],
            sep = "\r"
          ),
          triggers_empty
        ),
        relation_mentions = relation_mentions
      )
      if (!is_blank(text)) {
        function_statements <- c(
          function_statements,
          build_comment_on_routine(
            schema_name = row$schema_name[1],
            function_name = row$function_name[1],
            identity_args = row$identity_args[1],
            prokind = row$prokind[1],
            text = text
          )
        )
      }
    }
    function_updates <- length(function_statements)
    execute_statement_batches(function_statements, "function")

    column_statements <- character()
    for (i in seq_len(nrow(missing_columns))) {
      row <- missing_columns[i, , drop = FALSE]
      text <- infer_column_comment(
        column_name = row$column_name[1],
        comment = row$comment[1],
        fk_rows = get_lookup_df(
          column_fks_by_key,
          paste(row$key[1], row$column_name[1], sep = "\r"),
          fk_empty
        ),
        data_type = row$data_type[1]
      )
      if (!is_blank(text)) {
        column_statements <- c(
          column_statements,
          build_comment_on_column(
            schema_name = row$schema_name[1],
            object_name = row$object_name[1],
            column_name = row$column_name[1],
            text = text
          )
        )
      }
    }
    column_updates <- length(column_statements)
    execute_statement_batches(column_statements, "column")

    message(
      sprintf(
        paste(
          "Patch 40 documentation updates:",
          "%d schema comments, %d table comments, %d view comments,",
          "%d function comments, %d column comments."
        ),
        schema_updates,
        table_updates,
        view_updates,
        function_updates,
        column_updates
      )
    )

    message("Updating calibration tables for standards checks...")

    constraint_exists <- function(con, schema, table, constraint_name) {
      DBI::dbGetQuery(
        con,
        "SELECT EXISTS (
       SELECT 1
       FROM information_schema.table_constraints
       WHERE table_schema = $1
         AND table_name = $2
         AND constraint_name = $3
     ) AS present",
        params = list(schema, table, constraint_name)
      )$present[[1]]
    }

    add_check_only_column <- function(con, table_name) {
      DBI::dbExecute(
        con,
        paste0(
          "ALTER TABLE instruments.",
          table_name,
          " ADD COLUMN IF NOT EXISTS check_only boolean NOT NULL DEFAULT FALSE"
        )
      )
      DBI::dbExecute(
        con,
        paste0(
          "COMMENT ON COLUMN instruments.",
          table_name,
          ".check_only IS ",
          DBI::dbQuoteString(
            con,
            paste(
              "TRUE when the row records a check against standards/references only and no as-left (calibrated)",
              "values were entered."
            )
          )
        )
      )
    }

    add_check_only_column(con, "calibrate_ph")
    add_check_only_column(con, "calibrate_orp")
    add_check_only_column(con, "calibrate_specific_conductance")
    add_check_only_column(con, "calibrate_turbidity")
    add_check_only_column(con, "calibrate_dissolved_oxygen")

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_ph
       ALTER COLUMN ph1_post_val DROP NOT NULL,
       ALTER COLUMN ph2_post_val DROP NOT NULL,
       ALTER COLUMN ph3_post_val DROP NOT NULL"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_orp
       ALTER COLUMN orp_post_mv DROP NOT NULL"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_specific_conductance
       ALTER COLUMN spc1_post DROP NOT NULL,
       ALTER COLUMN spc2_post DROP NOT NULL,
       ALTER COLUMN spc3_post DROP NOT NULL"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_turbidity
       ALTER COLUMN turb1_post DROP NOT NULL,
       ALTER COLUMN turb2_post DROP NOT NULL"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_dissolved_oxygen
       ALTER COLUMN baro_press_post DROP NOT NULL,
       ALTER COLUMN do_post_mgl DROP NOT NULL"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_specific_conductance
       DROP CONSTRAINT IF EXISTS calibrate_specific_conductance_point_count_chk"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_specific_conductance
       ADD CONSTRAINT calibrate_specific_conductance_point_count_chk CHECK (
         calibration_points >= 1
         AND calibration_points <= 3
         AND spc1_std IS NOT NULL
         AND spc1_pre IS NOT NULL
         AND (
           (check_only = TRUE AND spc1_post IS NULL)
           OR
           (check_only = FALSE AND spc1_post IS NOT NULL)
         )
         AND (
           (
             calibration_points = 1
             AND spc2_std IS NULL
             AND spc2_pre IS NULL
             AND spc2_post IS NULL
             AND spc3_std IS NULL
             AND spc3_pre IS NULL
             AND spc3_post IS NULL
           )
           OR
           (
             calibration_points = 2
             AND spc2_std IS NOT NULL
             AND spc2_pre IS NOT NULL
             AND (
               (check_only = TRUE AND spc2_post IS NULL)
               OR
               (check_only = FALSE AND spc2_post IS NOT NULL)
             )
             AND spc3_std IS NULL
             AND spc3_pre IS NULL
             AND spc3_post IS NULL
           )
           OR
           (
             calibration_points = 3
             AND spc2_std IS NOT NULL
             AND spc2_pre IS NOT NULL
             AND (
               (check_only = TRUE AND spc2_post IS NULL)
               OR
               (check_only = FALSE AND spc2_post IS NOT NULL)
             )
             AND spc3_std IS NOT NULL
             AND spc3_pre IS NOT NULL
             AND (
               (check_only = TRUE AND spc3_post IS NULL)
               OR
               (check_only = FALSE AND spc3_post IS NOT NULL)
             )
           )
         )
        )"
    )

    if (
      constraint_exists(
        con,
        "instruments",
        "calibrate_ph",
        "calibrate_ph_check_only_chk"
      )
    ) {
      DBI::dbExecute(
        con,
        "ALTER TABLE instruments.calibrate_ph
         DROP CONSTRAINT calibrate_ph_check_only_chk"
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_ph
       ADD CONSTRAINT calibrate_ph_check_only_chk CHECK (
         (
           check_only = TRUE
           AND ph1_post_val IS NULL
           AND ph2_post_val IS NULL
           AND ph3_post_val IS NULL
         )
         OR
         (
           check_only = FALSE
           AND ph1_post_val IS NOT NULL
           AND ph2_post_val IS NOT NULL
           AND ph3_post_val IS NOT NULL
         )
       )"
    )

    if (
      constraint_exists(
        con,
        "instruments",
        "calibrate_orp",
        "calibrate_orp_check_only_chk"
      )
    ) {
      DBI::dbExecute(
        con,
        "ALTER TABLE instruments.calibrate_orp
         DROP CONSTRAINT calibrate_orp_check_only_chk"
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_orp
       ADD CONSTRAINT calibrate_orp_check_only_chk CHECK (
         (check_only = TRUE AND orp_post_mv IS NULL)
         OR
         (check_only = FALSE AND orp_post_mv IS NOT NULL)
       )"
    )

    if (
      constraint_exists(
        con,
        "instruments",
        "calibrate_turbidity",
        "calibrate_turbidity_check_only_chk"
      )
    ) {
      DBI::dbExecute(
        con,
        "ALTER TABLE instruments.calibrate_turbidity
         DROP CONSTRAINT calibrate_turbidity_check_only_chk"
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_turbidity
       ADD CONSTRAINT calibrate_turbidity_check_only_chk CHECK (
         (
           check_only = TRUE
           AND turb1_post IS NULL
           AND turb2_post IS NULL
         )
         OR
         (
           check_only = FALSE
           AND turb1_post IS NOT NULL
           AND turb2_post IS NOT NULL
         )
       )"
    )

    if (
      constraint_exists(
        con,
        "instruments",
        "calibrate_dissolved_oxygen",
        "calibrate_dissolved_oxygen_check_only_chk"
      )
    ) {
      DBI::dbExecute(
        con,
        "ALTER TABLE instruments.calibrate_dissolved_oxygen
         DROP CONSTRAINT calibrate_dissolved_oxygen_check_only_chk"
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.calibrate_dissolved_oxygen
       ADD CONSTRAINT calibrate_dissolved_oxygen_check_only_chk CHECK (
         (
           check_only = TRUE
           AND baro_press_post IS NULL
           AND do_post_mgl IS NULL
         )
         OR
         (
           check_only = FALSE
           AND baro_press_post IS NOT NULL
           AND do_post_mgl IS NOT NULL
         )
       )"
    )

    # Fix for stalling raster writes caused by immediate FK locking on spatial.rasters_reference.

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_sync_rr_cell_size_upd ON spatial.rasters;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_sync_rr_cell_size_ins ON spatial.rasters;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.rasters_reference DROP COLUMN IF EXISTS cell_size_x_deg CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.rasters_reference DROP COLUMN IF EXISTS cell_size_y_deg CASCADE;"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS spatial.sync_rr_cell_size_deg_apply(integer[]);"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS spatial.sync_rr_cell_size_deg_ins();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS spatial.sync_rr_cell_size_deg_upd();"
    )

    #     DBI::dbExecute(
    #       con,
    #       "CREATE OR REPLACE FUNCTION spatial.sync_rr_cell_size_deg_apply(ref_ids integer[])
    # RETURNS void
    # LANGUAGE plpgsql
    # AS $$
    # BEGIN
    #   IF COALESCE(array_length(ref_ids, 1), 0) = 0 THEN
    #     RETURN;
    #   END IF;

    #   UPDATE spatial.rasters_reference rr
    #   SET cell_size_x_deg = s.cell_size_x_deg,
    #       cell_size_y_deg = s.cell_size_y_deg
    #   FROM (
    #     SELECT
    #       r.reference_id,
    #       MIN(ABS(ST_ScaleX(r.rast))) AS cell_size_x_deg,
    #       MIN(ABS(ST_ScaleY(r.rast))) AS cell_size_y_deg
    #     FROM spatial.rasters r
    #     WHERE r.reference_id = ANY(ref_ids)
    #     GROUP BY r.reference_id
    #   ) s
    #   WHERE rr.reference_id = s.reference_id;
    # END;
    # $$;
    # "
    #     )

    #     DBI::dbExecute(
    #       con,
    #       "CREATE OR REPLACE FUNCTION spatial.sync_rr_cell_size_deg_ins()
    # RETURNS trigger
    # LANGUAGE plpgsql
    # AS $$
    # DECLARE
    #   ref_ids integer[];
    # BEGIN
    #   SELECT array_agg(DISTINCT nr.reference_id)
    #   INTO ref_ids
    #   FROM new_rows nr
    #   WHERE nr.reference_id IS NOT NULL;

    #   IF COALESCE(array_length(ref_ids, 1), 0) = 0 THEN
    #     RETURN NULL;
    #   END IF;

    #   PERFORM spatial.sync_rr_cell_size_deg_apply(ref_ids);
    #   RETURN NULL;
    # END;
    # $$;
    # "
    #     )

    #     DBI::dbExecute(
    #       con,
    #       "CREATE OR REPLACE FUNCTION spatial.sync_rr_cell_size_deg_upd()
    # RETURNS trigger
    # LANGUAGE plpgsql
    # AS $$
    # DECLARE
    #   ref_ids integer[];
    # BEGIN
    #   SELECT array_agg(DISTINCT x.reference_id)
    #   INTO ref_ids
    #   FROM (
    #     SELECT nr.reference_id AS reference_id FROM new_rows nr
    #     UNION
    #     SELECT orr.reference_id AS reference_id FROM old_rows orr
    #   ) x
    #   WHERE x.reference_id IS NOT NULL;

    #   IF COALESCE(array_length(ref_ids, 1), 0) = 0 THEN
    #     RETURN NULL;
    #   END IF;

    #   PERFORM spatial.sync_rr_cell_size_deg_apply(ref_ids);
    #   RETURN NULL;
    # END;
    # $$;
    # "
    #     )

    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.rasters
  ALTER CONSTRAINT fk_reference_id DEFERRABLE INITIALLY DEFERRED;"
    )

    # Wrap things up and commit
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '40'
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

    message("Patch 40 applied successfully, transaction closed.")
  },
  error = function(e) {
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 40 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
