#' Generate a human-readable HTML reference for the AquaCache database
#'
#' @description
#' Generates a large, navigable HTML document describing the AquaCache schema
#' for analysts, developers, and other readers who need more context than a raw
#' database dump provides. The output includes schema overviews, tables, views,
#' functions, keys, foreign-key relationships, triggers, indexes, and column
#' glossaries. Existing database comments are used whenever available, and
#' inferred descriptions are clearly marked.
#'
#' If no connection is supplied, the function creates a temporary connection
#' with [AquaConnect()] and closes it automatically when generation finishes.
#'
#' @param con A DBI connection to the AquaCache database. If `NULL`, the
#'   function attempts to create a temporary connection from environment
#'   variables.
#' @param output_file File path for the generated HTML document.
#' @param schemas Optional character vector of schema names to document. The
#'   default `NULL` documents the standard AquaCache schemas.
#' @param check_visibility Logical. If `TRUE`, compare the visible object counts
#'   of the supplied connection against an admin session when possible and warn
#'   if the connection appears to be missing documented objects.
#' @param open Logical. If `TRUE`, open the generated HTML file in the default
#'   browser after it is written.
#'
#' @return Invisibly returns the normalized output path.
#' @export
generateACDatabaseReference <- function(
  con = NULL,
  output_file = "aquacache_database_reference.html",
  schemas = NULL,
  check_visibility = TRUE,
  open = interactive()
) {
  target_schemas <- c(
    "application",
    "audit",
    "boreholes",
    "continuous",
    "discrete",
    "field",
    "files",
    "information",
    "instruments",
    "public",
    "spatial"
  )

  schema_descriptions <- c(
    application = "Application-managed content, notifications, and usage telemetry for client applications such as YGwater.",
    audit = "Audit logging and point-in-time reconstruction helpers for tracking metadata and measurement edits.",
    boreholes = "Groundwater and borehole inventory tables, including borehole construction, geology, permafrost, and linked documents.",
    continuous = "Automated time-series definitions, observations, corrections, quality flags, forecasts, and other continuous-data business rules.",
    discrete = "Manual and laboratory sample workflows, analytical results, guideline lookups, and sampling metadata.",
    field = "Field visit records and the assets or instruments associated with a visit.",
    files = "Documents, images, image-series metadata, and related file-classification tables.",
    information = "Internal database status and version-tracking tables used by AquaCache itself.",
    instruments = "Instrument inventory, maintenance, calibration, communications, and deployment-related reference data.",
    public = "Shared core reference tables such as locations, organizations, parameters, units, permissions, and normalized location metadata.",
    spatial = "Raster and vector storage plus indexing tables for spatial series and derived spatial metadata."
  )

  schema_palette <- list(
    application = c(accent = "#0b7285", tint = "#e6f7fa"),
    audit = c(accent = "#8b3d66", tint = "#f8e8f0"),
    boreholes = c(accent = "#8a5a00", tint = "#fbf1df"),
    continuous = c(accent = "#007c91", tint = "#e3f7fb"),
    discrete = c(accent = "#2b6cb0", tint = "#e8f0fb"),
    field = c(accent = "#5c6b33", tint = "#eef3e3"),
    files = c(accent = "#8c4b1f", tint = "#f9ece3"),
    information = c(accent = "#495057", tint = "#edf0f2"),
    instruments = c(accent = "#5f3dc4", tint = "#eee8ff"),
    public = c(accent = "#2b8a3e", tint = "#e7f6ea"),
    spatial = c(accent = "#0c8599", tint = "#e4f7fa")
  )

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

  escape_html <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
  }

  slugify <- function(...) {
    x <- paste(..., collapse = "-")
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "-", x)
    x <- gsub("(^-+|-+$)", "", x)
    x
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

  connect_catalog <- function() {
    con <- AquaConnect(silent = TRUE)
    suppressWarnings(
      try(
        DBI::dbExecute(
          con,
          "SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY"
        ),
        silent = TRUE
      )
    )
    con
  }

  fetch_catalog <- function(con, target_schemas = target_schemas) {
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
          obj_description(c.oid, 'pg_class') AS comment,
          pg_get_viewdef(c.oid, true) AS definition
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
          NOT a.attnotnull AS nullable,
          pg_get_expr(ad.adbin, ad.adrelid) AS column_default,
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
        LEFT JOIN pg_attrdef ad
          ON ad.adrelid = c.oid
         AND ad.adnum = a.attnum
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
          string_agg(ref_att.attname, ', ' ORDER BY ref_key.ord) AS referenced_columns,
          pg_get_constraintdef(con.oid, true) AS definition
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

    indexes <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s)
        SELECT
          ns.nspname AS schema_name,
          tbl.relname AS table_name,
          idx_cls.relname AS index_name,
          idx.indisunique AS is_unique,
          idx.indisprimary AS is_primary,
          con.conname AS backing_constraint,
          pg_get_indexdef(idx.indexrelid, 0, true) AS definition
        FROM pg_index idx
        JOIN pg_class tbl
          ON tbl.oid = idx.indrelid
        JOIN pg_namespace ns
          ON ns.oid = tbl.relnamespace
        JOIN target_schemas ts
          ON ts.schema_name = ns.nspname
        JOIN pg_class idx_cls
          ON idx_cls.oid = idx.indexrelid
        LEFT JOIN pg_constraint con
          ON con.conindid = idx.indexrelid
         AND con.conrelid = tbl.oid
         AND con.contype IN ('p', 'u', 'x')
        WHERE tbl.relkind IN ('r', 'p')
        ORDER BY ns.nspname, tbl.relname, idx_cls.relname;
        ",
        schema_sql
      )
    )
    indexes$key <- relation_key(indexes$schema_name, indexes$table_name)

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
          trg.tgenabled AS enabled_code,
          fn.oid AS function_oid,
          fn_ns.nspname AS function_schema,
          fn.proname AS function_name,
          pg_get_function_identity_arguments(fn.oid) AS function_identity_args,
          obj_description(fn.oid, 'pg_proc') AS function_comment,
          pg_get_triggerdef(trg.oid, true) AS definition
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
    triggers$key <- relation_key(triggers$schema_name, triggers$table_name)

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
          pg_get_function_arguments(p.oid) AS arguments,
          pg_get_function_result(p.oid) AS returns,
          l.lanname AS language,
          CASE p.provolatile
            WHEN 'i' THEN 'immutable'
            WHEN 's' THEN 'stable'
            ELSE 'volatile'
          END AS volatility,
          p.prosecdef AS security_definer,
          obj_description(p.oid, 'pg_proc') AS comment,
          pg_get_functiondef(p.oid) AS definition
        FROM pg_proc p
        JOIN pg_namespace n
          ON n.oid = p.pronamespace
        JOIN target_schemas ts
          ON ts.schema_name = n.nspname
        JOIN pg_language l
          ON l.oid = p.prolang
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

    version_info <- tryCatch(
      DBI::dbGetQuery(
        con,
        "
        SELECT item, version
        FROM information.version_info
        ORDER BY item;
        "
      ),
      error = function(...) {
        data.frame(
          item = character(),
          version = character(),
          stringsAsFactors = FALSE
        )
      }
    )

    list(
      target_schemas = target_schemas,
      tables = tables,
      views = views,
      columns = columns,
      constraints = constraints,
      indexes = indexes,
      triggers = triggers,
      view_dependencies = view_dependencies,
      functions = functions,
      version_info = version_info
    )
  }

  infer_table_purpose <- function(
    schema_name,
    table_name,
    comment,
    outgoing_fks,
    incoming_fks,
    columns_df
  ) {
    if (!is_blank(comment)) {
      return(list(text = trim_or_na(comment)[1], source = "Database comment"))
    }

    full_name <- relation_key(schema_name, table_name)
    manual <- switch(
      full_name,
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
      "spatial.spatial_ref_sys" = "Reference table of spatial reference systems. This is typically PostGIS-managed rather than AquaCache-specific business data.",
      NULL
    )

    if (!is.null(manual)) {
      return(list(text = manual, source = "Inferred from object name"))
    }

    if (schema_name == "instruments" && startsWith(table_name, "calibrate_")) {
      sensor_name <- gsub("^calibrate_", "", table_name)
      sensor_name <- gsub("_", " ", sensor_name)
      return(list(
        text = sprintf(
          "Stores calibration details specific to %s measurements. Rows normally hang off a parent record in instruments.calibrations.",
          sensor_name
        ),
        source = "Inferred from object name"
      ))
    }

    if (
      schema_name == "public" && startsWith(table_name, "locations_metadata_")
    ) {
      suffix <- gsub("^locations_metadata_", "", table_name)
      suffix <- gsub("_", " ", suffix)
      return(list(
        text = sprintf(
          "Normalized child table for location metadata focused on %s. It extends the core location record without overloading public.locations.",
          suffix
        ),
        source = "Inferred from object family"
      ))
    }

    if (schema_name == "files" && grepl("_series$", table_name)) {
      noun <- gsub("_series$", "", table_name)
      noun <- gsub("_", " ", noun)
      return(list(
        text = sprintf(
          "Series-level metadata table for grouped %s records, allowing many related files to be managed as one logical set.",
          noun
        ),
        source = "Inferred from object name"
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
      return(list(
        text = sprintf(
          "Association table that links %s and carries minimal additional business attributes beyond the relationship itself.",
          paste(parents, collapse = ", ")
        ),
        source = "Inferred from relationships"
      ))
    }

    if (
      grepl(
        "(types|groups|states|methods|publishers|series|suppliers|observers)$",
        table_name
      )
    ) {
      pretty <- gsub("_", " ", table_name)
      return(list(
        text = sprintf(
          "Reference table that standardizes the allowed values for %s across the database.",
          pretty
        ),
        source = "Inferred from object name"
      ))
    }

    pretty_name <- gsub("_", " ", table_name)
    relation_hint <- if (fk_count > 0) {
      sprintf(
        " It links outward to %d table(s) and is referenced by %d table(s).",
        fk_count,
        incoming_count
      )
    } else {
      ""
    }

    list(
      text = sprintf(
        "Stores %s records within the %s schema.%s",
        pretty_name,
        schema_name,
        relation_hint
      ),
      source = "Inferred from schema and name"
    )
  }

  infer_view_purpose <- function(
    schema_name,
    view_name,
    comment,
    dependencies
  ) {
    if (!is_blank(comment)) {
      return(list(text = trim_or_na(comment)[1], source = "Database comment"))
    }

    full_name <- relation_key(schema_name, view_name)
    manual <- switch(
      full_name,
      "continuous.measurements_calculated_daily_corrected" = "Convenience view that exposes stored daily values after applying currently effective corrections and security filtering.",
      "continuous.measurements_continuous_corrected" = "Convenience view that exposes raw continuous measurements alongside correction-adjusted values visible to the current caller.",
      "continuous.timeseries_metadata_en" = "English-language view that flattens key time-series metadata into a reader-friendly dataset for applications and exports.",
      "continuous.timeseries_metadata_fr" = "French-language view that flattens key time-series metadata into a reader-friendly dataset for applications and exports.",
      "public.location_metadata_en" = "English-language flattened location metadata view assembled from the normalized location tables.",
      "public.location_metadata_fr" = "French-language flattened location metadata view assembled from the normalized location tables.",
      NULL
    )

    if (!is.null(manual)) {
      return(list(text = manual, source = "Inferred from object name"))
    }

    dep_count <- nrow(dependencies)
    list(
      text = sprintf(
        "Derived relation that packages underlying tables into a reusable read model. The definition currently depends on %d upstream relation(s).",
        dep_count
      ),
      source = "Inferred from dependencies"
    )
  }

  infer_function_purpose <- function(
    schema_name,
    function_name,
    comment,
    returns,
    used_by_triggers,
    relation_mentions
  ) {
    if (!is_blank(comment)) {
      return(list(text = trim_or_na(comment)[1], source = "Database comment"))
    }

    full_name <- relation_key(schema_name, function_name)
    manual <- switch(
      full_name,
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
      "spatial.update_geom_type" = "Trigger helper that derives or refreshes the stored geometry-type label when a vector geometry changes.",
      NULL
    )

    if (!is.null(manual)) {
      return(list(text = manual, source = "Inferred from object name"))
    }

    if (identical(trimws(tolower(returns)), "trigger")) {
      if (nrow(used_by_triggers) > 0) {
        tables <- unique(relation_key(
          used_by_triggers$schema_name,
          used_by_triggers$table_name
        ))
        return(list(
          text = sprintf(
            "Trigger function used by %d trigger(s) across %d table(s): %s.",
            nrow(used_by_triggers),
            length(tables),
            paste(utils::head(tables, 5), collapse = ", ")
          ),
          source = "Inferred from trigger usage"
        ))
      }

      return(list(
        text = "Trigger function that enforces a business rule or maintains derived values when rows are inserted, updated, or deleted.",
        source = "Inferred from return type"
      ))
    }

    if (startsWith(function_name, "check_")) {
      return(list(
        text = "Validation routine that checks a business rule and normally raises an error when invalid data would be written.",
        source = "Inferred from object name"
      ))
    }

    if (startsWith(function_name, "fill_")) {
      return(list(
        text = "Helper routine that fills related metadata records or missing defaults needed elsewhere in the schema.",
        source = "Inferred from object name"
      ))
    }

    if (startsWith(function_name, "enforce_")) {
      return(list(
        text = "Business-rule routine that enforces a consistency requirement before data is accepted.",
        source = "Inferred from object name"
      ))
    }

    if (startsWith(function_name, "prevent_")) {
      return(list(
        text = "Guard routine that blocks a write when it would create an invalid overlap or forbidden state.",
        source = "Inferred from object name"
      ))
    }

    if (startsWith(function_name, "get_")) {
      return(list(
        text = "Lookup or derivation helper that returns a value or small result set needed by application logic or constraints.",
        source = "Inferred from object name"
      ))
    }

    if (length(relation_mentions) > 0) {
      return(list(
        text = sprintf(
          "Custom %s routine that appears to work with %d documented relation(s): %s.",
          if (grepl("^table\\(", tolower(returns))) {
            "table-returning"
          } else {
            "database"
          },
          length(relation_mentions),
          paste(utils::head(relation_mentions, 5), collapse = ", ")
        ),
        source = "Inferred from definition"
      ))
    }

    list(
      text = "Custom database routine defined inside the AquaCache schemas. Review the signature and SQL body below for exact behavior.",
      source = "Inferred from catalog metadata"
    )
  }

  infer_column_meaning <- function(column_name, comment, fk_rows, data_type) {
    if (!is_blank(comment)) {
      return(list(text = trim_or_na(comment)[1], source = "Database comment"))
    }

    if (nrow(fk_rows) == 1 && identical(fk_rows$column_list, column_name)) {
      return(list(
        text = sprintf(
          "References %s.%s(%s).",
          fk_rows$referenced_schema,
          fk_rows$referenced_table,
          fk_rows$referenced_columns %||% "key column"
        ),
        source = "Inferred from foreign key"
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
      return(list(text = manual, source = "Inferred from column name"))
    }

    if (grepl("_id$", column_name)) {
      stem <- gsub("_id$", "", column_name)
      stem <- gsub("_", " ", stem)
      return(list(
        text = sprintf("Identifier field for the related %s record.", stem),
        source = "Inferred from column name"
      ))
    }

    if (grepl("^is_|_flag$", column_name) || identical(data_type, "boolean")) {
      return(list(
        text = "Boolean flag column used to capture a yes or no style state.",
        source = "Inferred from data type"
      ))
    }

    list(text = NA_character_, source = NA_character_)
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

  badge <- function(text, class_name = "default") {
    sprintf(
      "<span class=\"badge badge-%s\">%s</span>",
      class_name,
      escape_html(text)
    )
  }

  link_relation <- function(schema_name, object_name, anchor_map) {
    key <- relation_key(schema_name, object_name)
    label <- escape_html(key)
    href <- anchor_map[[key]]
    if (is.null(href)) {
      return(label)
    }
    sprintf("<a href=\"#%s\">%s</a>", href, label)
  }

  format_multiline_sql <- function(text) {
    sprintf("<pre><code>%s</code></pre>", escape_html(text %||% ""))
  }

  format_text_block <- function(text, source) {
    source_badge <- switch(
      source %||% "Inferred",
      "Database comment" = badge("Documented in DB", "good"),
      "Inferred from object name" = badge("Inferred", "warn"),
      "Inferred from object family" = badge("Inferred", "warn"),
      "Inferred from relationships" = badge("Inferred", "warn"),
      "Inferred from schema and name" = badge("Inferred", "warn"),
      "Inferred from dependencies" = badge("Inferred", "warn"),
      "Inferred from definition" = badge("Inferred", "warn"),
      "Inferred from trigger usage" = badge("Inferred", "warn"),
      "Inferred from catalog metadata" = badge("Inferred", "warn"),
      "Inferred from column name" = badge("Inferred", "warn"),
      "Inferred from foreign key" = badge("Inferred", "warn"),
      "Inferred from data type" = badge("Inferred", "warn"),
      badge("Inferred", "warn")
    )
    sprintf(
      "<div class=\"purpose-block\">%s<p>%s</p></div>",
      source_badge,
      escape_html(text %||% "No descriptive text available.")
    )
  }

  format_definition_list <- function(items) {
    if (length(items) == 0) {
      return("<p class=\"muted\">None.</p>")
    }

    rows <- vapply(
      names(items),
      function(name) {
        sprintf("<dt>%s</dt><dd>%s</dd>", escape_html(name), items[[name]])
      },
      character(1)
    )
    sprintf("<dl class=\"kv-list\">%s</dl>", paste(rows, collapse = ""))
  }

  format_simple_table <- function(
    df,
    col_labels = names(df),
    classes = "data-table"
  ) {
    if (nrow(df) == 0) {
      return("<p class=\"muted\">None.</p>")
    }

    head_html <- paste(
      sprintf("<th>%s</th>", escape_html(col_labels)),
      collapse = ""
    )

    body_rows <- apply(df, 1, function(row) {
      cells <- paste(sprintf("<td>%s</td>", row), collapse = "")
      sprintf("<tr>%s</tr>", cells)
    })

    sprintf(
      "<table class=\"%s\"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>",
      classes,
      head_html,
      paste(body_rows, collapse = "")
    )
  }

  enabled_label <- function(code) {
    switch(
      as.character(code),
      O = "Enabled",
      D = "Disabled",
      R = "Enabled for replica",
      A = "Enabled always",
      "Unknown"
    )
  }

  kind_label <- function(relkind) {
    switch(
      relkind,
      r = "Table",
      p = "Partitioned table",
      v = "View",
      m = "Materialized view",
      "Relation"
    )
  }

  catalog_visibility_summary <- function(con, target_schemas = target_schemas) {
    schema_sql <- schema_list_sql(target_schemas)

    DBI::dbGetQuery(
      con,
      sprintf(
        "
        WITH target_schemas AS (%s),
        relation_counts AS (
          SELECT
            n.nspname AS schema_name,
            count(*) FILTER (WHERE c.relkind IN ('r', 'p')) AS tables,
            count(*) FILTER (WHERE c.relkind IN ('v', 'm')) AS views
          FROM pg_class c
          JOIN pg_namespace n
            ON n.oid = c.relnamespace
          JOIN target_schemas ts
            ON ts.schema_name = n.nspname
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
          GROUP BY n.nspname
        ),
        routine_counts AS (
          SELECT
            n.nspname AS schema_name,
            count(*) AS routines
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
          GROUP BY n.nspname
        )
        SELECT
          ts.schema_name,
          COALESCE(rc.tables, 0) AS tables,
          COALESCE(rc.views, 0) AS views,
          COALESCE(fc.routines, 0) AS routines
        FROM target_schemas ts
        LEFT JOIN relation_counts rc
          ON rc.schema_name = ts.schema_name
        LEFT JOIN routine_counts fc
          ON fc.schema_name = ts.schema_name
        ORDER BY ts.schema_name;
        ",
        schema_sql
      )
    )
  }

  check_catalog_visibility <- function(con, target_schemas = target_schemas) {
    current_summary <- catalog_visibility_summary(
      con,
      target_schemas = target_schemas
    )
    current_user <- DBI::dbGetQuery(
      con,
      "SELECT current_user AS current_user;"
    )[1, 1]

    missing_schemas <- current_summary$schema_name[
      current_summary$tables == 0 &
        current_summary$views == 0 &
        current_summary$routines == 0
    ]
    if (length(missing_schemas) > 0) {
      warning(
        "The supplied connection cannot see any documented objects in the following schema(s): ",
        paste(missing_schemas, collapse = ", "),
        ". The generated reference may be incomplete.",
        call. = FALSE
      )
    }

    admin_user <- Sys.getenv("aquacacheAdminUser")
    admin_pass <- Sys.getenv("aquacacheAdminPass")
    if (
      !nzchar(admin_user) ||
        !nzchar(admin_pass) ||
        identical(current_user, admin_user)
    ) {
      return(invisible(current_summary))
    }

    admin_con <- tryCatch(
      DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = Sys.getenv("aquacacheName"),
        host = Sys.getenv("aquacacheHost"),
        port = Sys.getenv("aquacachePort"),
        user = admin_user,
        password = admin_pass
      ),
      error = function(...) NULL
    )

    if (is.null(admin_con)) {
      warning(
        "Could not compare the supplied connection against an admin session, so completeness could not be fully verified.",
        call. = FALSE
      )
      return(invisible(current_summary))
    }

    on.exit(DBI::dbDisconnect(admin_con), add = TRUE)
    admin_summary <- catalog_visibility_summary(
      admin_con,
      target_schemas = target_schemas
    )
    comparison <- merge(
      admin_summary,
      current_summary,
      by = "schema_name",
      suffixes = c("_admin", "_current"),
      all = TRUE
    )

    problems <- character()
    for (i in seq_len(nrow(comparison))) {
      row <- comparison[i, , drop = FALSE]
      gaps <- c()
      for (col in c("tables", "views", "routines")) {
        admin_n <- row[[paste0(col, "_admin")]]
        current_n <- row[[paste0(col, "_current")]]
        if (!is.na(admin_n) && !is.na(current_n) && current_n < admin_n) {
          gaps <- c(gaps, sprintf("%s %s/%s", col, current_n, admin_n))
        }
      }
      if (length(gaps) > 0) {
        problems <- c(
          problems,
          sprintf("%s (%s)", row$schema_name, paste(gaps, collapse = ", "))
        )
      }
    }

    if (length(problems) > 0) {
      warning(
        "The supplied connection appears to see fewer objects than an admin session for: ",
        paste(problems, collapse = "; "),
        ". The generated reference may omit some database objects.",
        call. = FALSE
      )
    }

    invisible(current_summary)
  }

  build_html <- function(
    catalog,
    output_path,
    selected_schemas = catalog$target_schemas %||% target_schemas
  ) {
    tables <- catalog$tables
    views <- catalog$views
    columns <- catalog$columns
    constraints <- catalog$constraints
    indexes <- catalog$indexes
    triggers <- catalog$triggers
    view_dependencies <- catalog$view_dependencies
    functions <- catalog$functions

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

    anchor_map <- list()
    for (i in seq_len(nrow(tables))) {
      anchor_map[[tables$key[i]]] <- slugify(
        "table",
        tables$schema_name[i],
        tables$object_name[i]
      )
    }
    for (i in seq_len(nrow(views))) {
      anchor_map[[views$key[i]]] <- slugify(
        "view",
        views$schema_name[i],
        views$object_name[i]
      )
    }

    functions$anchor <- vapply(
      functions$function_oid,
      function(x) slugify("function", x),
      character(1)
    )

    schema_counts <- data.frame(
      schema_name = selected_schemas,
      schema_description = unname(schema_descriptions[selected_schemas]),
      table_count = vapply(
        selected_schemas,
        function(s) sum(tables$schema_name == s),
        integer(1)
      ),
      view_count = vapply(
        selected_schemas,
        function(s) sum(views$schema_name == s),
        integer(1)
      ),
      function_count = vapply(
        selected_schemas,
        function(s) sum(functions$schema_name == s),
        integer(1)
      ),
      stringsAsFactors = FALSE
    )

    object_totals <- list(
      tables = nrow(tables),
      views = nrow(views),
      functions = nrow(functions),
      foreign_keys = sum(constraints$contype == "f"),
      triggers = nrow(triggers),
      indexes = nrow(indexes)
    )

    generation_stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

    version_summary <- if (nrow(catalog$version_info) > 0) {
      paste(
        sprintf(
          "<li><strong>%s:</strong> %s</li>",
          escape_html(catalog$version_info$item),
          escape_html(catalog$version_info$version)
        ),
        collapse = ""
      )
    } else {
      "<li>Version information table was not available during generation.</li>"
    }

    preview_text <- function(text) {
      out <- trim_or_na(text)[1] %||% ""
      if (nchar(out) > 220) {
        paste0(substr(out, 1, 217), "...")
      } else {
        out
      }
    }

    schema_class_name <- function(schema_name) {
      paste0("schema-theme-", gsub("[^a-z0-9]+", "-", tolower(schema_name)))
    }

    make_group <- function(title, cards, class_name, count, open = TRUE) {
      if (length(cards) == 0) {
        return("")
      }

      sprintf(
        "
        <details class=\"schema-group %s\"%s>
          <summary class=\"schema-group-summary\">
            <span>%s</span>
            <span class=\"badge badge-default\">%d object%s</span>
          </summary>
          <div class=\"schema-group-body\">%s</div>
        </details>
        ",
        class_name,
        if (isTRUE(open)) " open" else "",
        escape_html(title),
        count,
        if (count == 1) "" else "s",
        paste(cards, collapse = "")
      )
    }

    cards_html <- character()

    for (schema_name in selected_schemas) {
      table_rows <- tables[tables$schema_name == schema_name, , drop = FALSE]
      view_rows <- views[views$schema_name == schema_name, , drop = FALSE]
      function_rows <- functions[
        functions$schema_name == schema_name,
        ,
        drop = FALSE
      ]

      table_cards <- character()
      view_cards <- character()
      function_cards <- character()

      for (i in seq_len(nrow(table_rows))) {
        row <- table_rows[i, , drop = FALSE]
        key <- row$key[1]
        table_columns <- columns[
          columns$key == key & columns$relkind %in% c("r", "p"),
          ,
          drop = FALSE
        ]
        table_constraints <- constraints[constraints$key == key, , drop = FALSE]
        outgoing_fks <- table_constraints[
          table_constraints$contype == "f",
          ,
          drop = FALSE
        ]
        incoming_fks <- constraints[
          constraints$contype == "f" &
            constraints$referenced_schema == row$schema_name[1] &
            constraints$referenced_table == row$object_name[1],
          ,
          drop = FALSE
        ]
        pk_rows <- table_constraints[
          table_constraints$contype == "p",
          ,
          drop = FALSE
        ]
        unique_rows <- table_constraints[
          table_constraints$contype == "u",
          ,
          drop = FALSE
        ]
        check_rows <- table_constraints[
          table_constraints$contype %in% c("c", "x"),
          ,
          drop = FALSE
        ]
        table_indexes <- indexes[indexes$key == key, , drop = FALSE]
        table_triggers <- triggers[triggers$key == key, , drop = FALSE]

        purpose <- infer_table_purpose(
          schema_name = row$schema_name[1],
          table_name = row$object_name[1],
          comment = row$comment[1],
          outgoing_fks = outgoing_fks,
          incoming_fks = incoming_fks,
          columns_df = table_columns
        )

        pk_html <- if (nrow(pk_rows) > 0) {
          paste(
            sprintf(
              "<li><strong>%s</strong>: %s</li>",
              escape_html(pk_rows$conname),
              escape_html(pk_rows$definition)
            ),
            collapse = ""
          )
        } else {
          "<li>No declared primary key.</li>"
        }

        unique_html <- if (nrow(unique_rows) > 0) {
          paste(
            sprintf(
              "<li><strong>%s</strong>: %s</li>",
              escape_html(unique_rows$conname),
              escape_html(unique_rows$definition)
            ),
            collapse = ""
          )
        } else {
          "<li>No standalone unique constraint was found beyond the primary key.</li>"
        }

        outgoing_html <- if (nrow(outgoing_fks) > 0) {
          paste(
            apply(outgoing_fks, 1, function(fk) {
              sprintf(
                "<li><strong>%s</strong>: %s -> %s</li>",
                escape_html(fk[["conname"]]),
                escape_html(fk[["column_list"]] %||% ""),
                link_relation(
                  fk[["referenced_schema"]],
                  fk[["referenced_table"]],
                  anchor_map
                )
              )
            }),
            collapse = ""
          )
        } else {
          "<li>No outgoing foreign keys.</li>"
        }

        incoming_html <- if (nrow(incoming_fks) > 0) {
          paste(
            apply(incoming_fks, 1, function(fk) {
              sprintf(
                "<li><strong>%s</strong>: referenced by %s via %s</li>",
                escape_html(fk[["conname"]]),
                link_relation(
                  fk[["schema_name"]],
                  fk[["table_name"]],
                  anchor_map
                ),
                escape_html(fk[["column_list"]] %||% "")
              )
            }),
            collapse = ""
          )
        } else {
          "<li>No other documented table currently references this table with a foreign key.</li>"
        }

        trigger_html <- if (nrow(table_triggers) > 0) {
          paste(
            apply(table_triggers, 1, function(trg) {
              function_label <- sprintf(
                "%s.%s(%s)",
                trg[["function_schema"]],
                trg[["function_name"]],
                trg[["function_identity_args"]]
              )
              sprintf(
                "<li><strong>%s</strong> %s<br><span class=\"muted\">Calls %s. %s</span></li>",
                escape_html(trg[["trigger_name"]]),
                badge(enabled_label(trg[["enabled_code"]]), "default"),
                escape_html(function_label),
                escape_html(
                  trim_or_na(trg[["function_comment"]])[1] %||%
                    "No function comment."
                )
              )
            }),
            collapse = ""
          )
        } else {
          "<li>No user-defined triggers found.</li>"
        }

        index_html <- if (nrow(table_indexes) > 0) {
          paste(
            apply(table_indexes, 1, function(idx) {
              parts <- c()
              if (isTRUE(as.logical(idx[["is_primary"]]))) {
                parts <- c(parts, "Primary")
              }
              if (isTRUE(as.logical(idx[["is_unique"]]))) {
                parts <- c(parts, "Unique")
              }
              if (!is_blank(idx[["backing_constraint"]])) {
                parts <- c(
                  parts,
                  sprintf("backs %s", idx[["backing_constraint"]])
                )
              }
              label <- if (length(parts) > 0) {
                paste(parts, collapse = ", ")
              } else {
                "Index"
              }
              sprintf(
                "<li><strong>%s</strong> %s<br><span class=\"muted\">%s</span></li>",
                escape_html(idx[["index_name"]]),
                badge(label, "default"),
                escape_html(idx[["definition"]])
              )
            }),
            collapse = ""
          )
        } else {
          "<li>No indexes found.</li>"
        }

        rule_html <- if (nrow(check_rows) > 0) {
          paste(
            sprintf(
              "<li><strong>%s</strong>: %s</li>",
              escape_html(check_rows$conname),
              escape_html(check_rows$definition)
            ),
            collapse = ""
          )
        } else {
          "<li>No check or exclusion constraints recorded.</li>"
        }

        column_rows <- character()
        for (j in seq_len(nrow(table_columns))) {
          col <- table_columns[j, , drop = FALSE]
          col_name <- col$column_name[1]
          fk_for_column <- outgoing_fks[
            outgoing_fks$column_list == col_name,
            ,
            drop = FALSE
          ]
          meaning <- infer_column_meaning(
            column_name = col_name,
            comment = col$comment[1],
            fk_rows = fk_for_column,
            data_type = col$data_type[1]
          )

          key_badges <- character()
          if (
            nrow(pk_rows) > 0 &&
              grepl(
                paste0(
                  "(^|, )",
                  gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", col_name),
                  "($|, )"
                ),
                pk_rows$column_list[1]
              )
          ) {
            key_badges <- c(key_badges, badge("PK", "good"))
          }
          if (
            nrow(unique_rows) > 0 &&
              any(vapply(
                unique_rows$column_list,
                function(x) {
                  grepl(
                    paste0(
                      "(^|, )",
                      gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", col_name),
                      "($|, )"
                    ),
                    x
                  )
                },
                logical(1)
              ))
          ) {
            key_badges <- c(key_badges, badge("UQ", "default"))
          }
          if (nrow(fk_for_column) > 0) {
            key_badges <- c(key_badges, badge("FK", "warn"))
          }

          meaning_html <- if (!is_blank(meaning$text)) {
            note_badge <- if (
              !is_blank(meaning$source) && meaning$source != "Database comment"
            ) {
              badge("Inferred", "warn")
            } else {
              ""
            }
            paste0(
              note_badge,
              if (nzchar(note_badge)) " " else "",
              escape_html(meaning$text)
            )
          } else {
            "<span class=\"muted\">No column-level description found.</span>"
          }

          column_rows <- c(
            column_rows,
            sprintf(
              "<tr><td>%s</td><td><code>%s</code></td><td><code>%s</code></td><td>%s</td><td><code>%s</code></td><td>%s</td><td>%s</td></tr>",
              escape_html(col$ordinal_position[1]),
              escape_html(col_name),
              escape_html(col$data_type[1]),
              if (isTRUE(col$nullable[1])) "Yes" else "No",
              escape_html(trim_or_na(col$column_default[1])[1] %||% ""),
              if (length(key_badges) > 0) {
                paste(key_badges, collapse = " ")
              } else {
                "<span class=\"muted\">None</span>"
              },
              meaning_html
            )
          )
        }

        search_text <- paste(
          row$schema_name[1],
          row$object_name[1],
          purpose$text,
          paste(table_columns$column_name, collapse = " "),
          paste(table_constraints$conname, collapse = " "),
          collapse = " "
        )

        table_cards <- c(
          table_cards,
          sprintf(
            "
            <section class=\"object-card\" id=\"%s\" data-search=\"%s\" data-kind=\"table\">
              <div class=\"object-header\">
                <div>
                  <p class=\"eyebrow\">%s</p>
                  <h3>%s</h3>
                </div>
                <div class=\"object-badges\">%s %s %s %s</div>
              </div>
              %s
              <div class=\"object-grid\">
                <div>
                  <h4>Primary Key</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>Unique Keys</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>Links To</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>Linked From</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>Business Rules</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>Triggers</h4>
                  <ul>%s</ul>
                </div>
                <div class=\"span-2\">
                  <h4>Indexes</h4>
                  <ul>%s</ul>
                </div>
              </div>
              <h4>Columns</h4>
              <table class=\"data-table\">
                <thead>
                  <tr>
                    <th>#</th>
                    <th>Column</th>
                    <th>Type</th>
                    <th>Nullable</th>
                    <th>Default</th>
                    <th>Key Role</th>
                    <th>Meaning</th>
                  </tr>
                </thead>
                <tbody>%s</tbody>
              </table>
            </section>
            ",
            anchor_map[[key]],
            escape_html(search_text),
            escape_html(kind_label("r")),
            escape_html(key),
            badge(sprintf("%d columns", nrow(table_columns)), "default"),
            badge(
              sprintf(
                "%d outgoing link%s",
                nrow(outgoing_fks),
                if (nrow(outgoing_fks) == 1) "" else "s"
              ),
              "default"
            ),
            badge(
              sprintf(
                "%d incoming link%s",
                nrow(incoming_fks),
                if (nrow(incoming_fks) == 1) "" else "s"
              ),
              "default"
            ),
            badge(
              sprintf(
                "%d trigger%s",
                nrow(table_triggers),
                if (nrow(table_triggers) == 1) "" else "s"
              ),
              "default"
            ),
            format_text_block(purpose$text, purpose$source),
            pk_html,
            unique_html,
            outgoing_html,
            incoming_html,
            rule_html,
            trigger_html,
            index_html,
            paste(column_rows, collapse = "")
          )
        )
      }

      for (i in seq_len(nrow(view_rows))) {
        row <- view_rows[i, , drop = FALSE]
        key <- row$key[1]
        view_columns <- columns[
          columns$key == key & columns$relkind %in% c("v", "m"),
          ,
          drop = FALSE
        ]
        deps <- view_dependencies[view_dependencies$key == key, , drop = FALSE]
        purpose <- infer_view_purpose(
          schema_name = row$schema_name[1],
          view_name = row$object_name[1],
          comment = row$comment[1],
          dependencies = deps
        )

        dep_html <- if (nrow(deps) > 0) {
          paste(
            apply(deps, 1, function(dep) {
              sprintf(
                "<li>%s %s</li>",
                badge(kind_label(dep[["dependency_kind"]]), "default"),
                link_relation(
                  dep[["dependency_schema"]],
                  dep[["dependency_name"]],
                  anchor_map
                )
              )
            }),
            collapse = ""
          )
        } else {
          "<li>No relation dependencies were discovered.</li>"
        }

        column_rows <- character()
        for (j in seq_len(nrow(view_columns))) {
          col <- view_columns[j, , drop = FALSE]
          meaning <- infer_column_meaning(
            col$column_name[1],
            col$comment[1],
            fk_rows = data.frame(),
            data_type = col$data_type[1]
          )
          meaning_html <- if (!is_blank(meaning$text)) {
            note_badge <- if (
              !is_blank(meaning$source) && meaning$source != "Database comment"
            ) {
              badge("Inferred", "warn")
            } else {
              ""
            }
            paste0(
              note_badge,
              if (nzchar(note_badge)) " " else "",
              escape_html(meaning$text)
            )
          } else {
            "<span class=\"muted\">No column-level description found.</span>"
          }
          column_rows <- c(
            column_rows,
            sprintf(
              "<tr><td>%s</td><td><code>%s</code></td><td><code>%s</code></td><td>%s</td></tr>",
              escape_html(col$ordinal_position[1]),
              escape_html(col$column_name[1]),
              escape_html(col$data_type[1]),
              meaning_html
            )
          )
        }

        search_text <- paste(
          row$schema_name[1],
          row$object_name[1],
          purpose$text,
          paste(view_columns$column_name, collapse = " "),
          paste(
            relation_key(deps$dependency_schema, deps$dependency_name),
            collapse = " "
          ),
          collapse = " "
        )

        view_cards <- c(
          view_cards,
          sprintf(
            "
            <section class=\"object-card\" id=\"%s\" data-search=\"%s\" data-kind=\"view\">
              <div class=\"object-header\">
                <div>
                  <p class=\"eyebrow\">%s</p>
                  <h3>%s</h3>
                </div>
                <div class=\"object-badges\">%s %s</div>
              </div>
              %s
              <div class=\"object-grid\">
                <div>
                  <h4>Depends On</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>SQL Definition</h4>
                  <details>
                    <summary>Show view SQL</summary>
                    %s
                  </details>
                </div>
              </div>
              <h4>Columns</h4>
              <table class=\"data-table\">
                <thead>
                  <tr>
                    <th>#</th>
                    <th>Column</th>
                    <th>Type</th>
                    <th>Meaning</th>
                  </tr>
                </thead>
                <tbody>%s</tbody>
              </table>
            </section>
            ",
            anchor_map[[key]],
            escape_html(search_text),
            escape_html(kind_label(row$relkind[1])),
            escape_html(key),
            badge(sprintf("%d columns", nrow(view_columns)), "default"),
            badge(
              sprintf(
                "%d upstream relation%s",
                nrow(deps),
                if (nrow(deps) == 1) "" else "s"
              ),
              "default"
            ),
            format_text_block(purpose$text, purpose$source),
            dep_html,
            format_multiline_sql(row$definition[1]),
            paste(column_rows, collapse = "")
          )
        )
      }

      for (i in seq_len(nrow(function_rows))) {
        row <- function_rows[i, , drop = FALSE]
        used_by_triggers <- triggers[
          triggers$function_oid == row$function_oid[1],
          ,
          drop = FALSE
        ]
        relation_mentions <- detect_relation_mentions(
          row$definition[1],
          relation_catalog
        )
        purpose <- infer_function_purpose(
          schema_name = row$schema_name[1],
          function_name = row$function_name[1],
          comment = row$comment[1],
          returns = row$returns[1],
          used_by_triggers = used_by_triggers,
          relation_mentions = relation_mentions
        )

        trigger_use_html <- if (nrow(used_by_triggers) > 0) {
          paste(
            apply(used_by_triggers, 1, function(trg) {
              sprintf(
                "<li><strong>%s</strong> on %s</li>",
                escape_html(trg[["trigger_name"]]),
                link_relation(
                  trg[["schema_name"]],
                  trg[["table_name"]],
                  anchor_map
                )
              )
            }),
            collapse = ""
          )
        } else {
          "<li>This function is not currently wired to a trigger in the documented schemas.</li>"
        }

        relation_html <- if (length(relation_mentions) > 0) {
          paste(
            sprintf(
              "<li>%s</li>",
              vapply(
                strsplit(relation_mentions, "\\.", fixed = FALSE),
                function(parts) {
                  link_relation(parts[1], parts[2], anchor_map)
                },
                character(1)
              )
            ),
            collapse = ""
          )
        } else {
          "<li>No schema-qualified relation references were detected in the function body. Some routines rely on trigger context, dynamic SQL, or helper functions instead.</li>"
        }

        signature <- sprintf(
          "%s.%s(%s)",
          row$schema_name[1],
          row$function_name[1],
          row$arguments[1]
        )

        kind_text <- if (row$prokind[1] == "p") "Procedure" else "Function"
        security_text <- if (isTRUE(row$security_definer[1])) {
          "Runs with definer privileges"
        } else {
          "Runs with invoker privileges"
        }

        search_text <- paste(
          row$schema_name[1],
          row$function_name[1],
          row$identity_args[1],
          row$returns[1],
          purpose$text,
          paste(relation_mentions, collapse = " "),
          collapse = " "
        )

        function_cards <- c(
          function_cards,
          sprintf(
            "
            <section class=\"object-card\" id=\"%s\" data-search=\"%s\" data-kind=\"function\">
              <div class=\"object-header\">
                <div>
                  <p class=\"eyebrow\">%s</p>
                  <h3>%s</h3>
                </div>
                <div class=\"object-badges\">%s %s %s</div>
              </div>
              %s
              %s
              <div class=\"object-grid\">
                <div>
                  <h4>Used By Triggers</h4>
                  <ul>%s</ul>
                </div>
                <div>
                  <h4>Relations Mentioned In Definition</h4>
                  <ul>%s</ul>
                </div>
              </div>
              <details>
                <summary>Show function definition</summary>
                %s
              </details>
            </section>
            ",
            row$anchor[1],
            escape_html(search_text),
            escape_html(kind_text),
            escape_html(signature),
            badge(sprintf("Returns: %s", row$returns[1]), "default"),
            badge(sprintf("Language: %s", row$language[1]), "default"),
            badge(sprintf("Volatility: %s", row$volatility[1]), "default"),
            format_text_block(purpose$text, purpose$source),
            format_definition_list(list(
              Signature = sprintf("<code>%s</code>", escape_html(signature)),
              Identity_arguments = sprintf(
                "<code>%s</code>",
                escape_html(row$identity_args[1] %||% "")
              ),
              Returns = sprintf(
                "<code>%s</code>",
                escape_html(row$returns[1] %||% "")
              ),
              Security = escape_html(security_text)
            )),
            trigger_use_html,
            relation_html,
            format_multiline_sql(row$definition[1])
          )
        )
      }

      schema_body <- paste(
        make_group(
          "Tables",
          table_cards,
          "schema-group-tables",
          length(table_cards),
          open = TRUE
        ),
        make_group(
          "Views",
          view_cards,
          "schema-group-views",
          length(view_cards),
          open = TRUE
        ),
        make_group(
          "Functions",
          function_cards,
          "schema-group-functions",
          length(function_cards),
          open = FALSE
        ),
        collapse = ""
      )

      cards_html <- c(
        cards_html,
        sprintf(
          "
          <details class=\"schema-block %s\" id=\"schema-%s\" open data-schema=\"%s\">
            <summary class=\"schema-summary\">
              <div class=\"schema-header\">
                <div>
                  <p class=\"eyebrow\">Schema</p>
                  <h2>%s</h2>
                </div>
                <div class=\"object-badges\">%s %s %s</div>
              </div>
            </summary>
            <div class=\"schema-body\">
              <p class=\"schema-description\">%s</p>
              %s
            </div>
          </details>
          ",
          schema_class_name(schema_name),
          escape_html(schema_name),
          escape_html(schema_name),
          escape_html(schema_name),
          badge(
            sprintf(
              "%d tables",
              schema_counts$table_count[
                schema_counts$schema_name == schema_name
              ]
            ),
            "default"
          ),
          badge(
            sprintf(
              "%d views",
              schema_counts$view_count[schema_counts$schema_name == schema_name]
            ),
            "default"
          ),
          badge(
            sprintf(
              "%d functions",
              schema_counts$function_count[
                schema_counts$schema_name == schema_name
              ]
            ),
            "default"
          ),
          escape_html(schema_descriptions[[schema_name]] %||% ""),
          schema_body
        )
      )
    }

    schema_overview_df <- data.frame(
      Schema = escape_html(schema_counts$schema_name),
      Description = escape_html(schema_counts$schema_description),
      Tables = escape_html(schema_counts$table_count),
      Views = escape_html(schema_counts$view_count),
      Functions = escape_html(schema_counts$function_count),
      stringsAsFactors = FALSE
    )

    summary_cards <- paste(
      c(
        sprintf(
          "<div class=\"summary-card\"><span class=\"summary-label\">Tables</span><strong>%s</strong></div>",
          object_totals$tables
        ),
        sprintf(
          "<div class=\"summary-card\"><span class=\"summary-label\">Views</span><strong>%s</strong></div>",
          object_totals$views
        ),
        sprintf(
          "<div class=\"summary-card\"><span class=\"summary-label\">Functions</span><strong>%s</strong></div>",
          object_totals$functions
        ),
        sprintf(
          "<div class=\"summary-card\"><span class=\"summary-label\">Foreign Keys</span><strong>%s</strong></div>",
          object_totals$foreign_keys
        ),
        sprintf(
          "<div class=\"summary-card\"><span class=\"summary-label\">Triggers</span><strong>%s</strong></div>",
          object_totals$triggers
        ),
        sprintf(
          "<div class=\"summary-card\"><span class=\"summary-label\">Indexes</span><strong>%s</strong></div>",
          object_totals$indexes
        )
      ),
      collapse = ""
    )

    schema_nav <- paste(
      sprintf(
        "<a class=\"schema-chip %s\" href=\"#schema-%s\">%s</a>",
        vapply(selected_schemas, schema_class_name, character(1)),
        escape_html(selected_schemas),
        escape_html(selected_schemas)
      ),
      collapse = ""
    )

    toc_items <- paste(
      sprintf(
        "<li><a href=\"#schema-%s\"><strong>%s</strong><span>%d tables, %d views, %d functions</span></a></li>",
        escape_html(schema_counts$schema_name),
        escape_html(schema_counts$schema_name),
        schema_counts$table_count,
        schema_counts$view_count,
        schema_counts$function_count
      ),
      collapse = ""
    )

    schema_theme_css <- paste(
      vapply(
        selected_schemas,
        function(schema_name) {
          colors <- schema_palette[[schema_name]] %||%
            c(accent = "#007c91", tint = "#e3f7fb")
          sprintf(
            ".%s{--schema-accent:%s;--schema-tint:%s}.schema-chip.%s{background:%s;border-color:%s;color:%s}",
            schema_class_name(schema_name),
            colors[["accent"]],
            colors[["tint"]],
            schema_class_name(schema_name),
            colors[["tint"]],
            colors[["accent"]],
            colors[["accent"]]
          )
        },
        character(1)
      ),
      collapse = "\n"
    )

    page_css <- paste(
      c(
        ":root{--bg:#f4fbfd;--panel:#ffffff;--panel-alt:#eef7f9;--ink:#13232d;--muted:#5b7380;--line:#d7e5ea;--accent:#007c91;--accent-deep:#0d5561;--warm:#d27a00;--good:#1b7f5a;--shadow:0 12px 32px rgba(10,52,63,0.08)}",
        "*{box-sizing:border-box}",
        "html{scroll-behavior:smooth}",
        "body{margin:0;font-family:\"Segoe UI\",\"Helvetica Neue\",Arial,sans-serif;color:var(--ink);background:radial-gradient(circle at top left, rgba(0,124,145,0.18), transparent 28%),radial-gradient(circle at top right, rgba(210,122,0,0.12), transparent 24%),linear-gradient(180deg,#fbfeff 0%,var(--bg) 100%);line-height:1.55}",
        "a{color:var(--accent-deep)}",
        "code,pre{font-family:Consolas,\"Courier New\",monospace}",
        ".page{width:min(1600px,calc(100% - 48px));margin:0 auto;padding:32px 0 72px}",
        ".hero{background:linear-gradient(135deg,rgba(0,124,145,0.95),rgba(13,85,97,0.95));color:#fff;border-radius:28px;padding:36px 36px 28px;box-shadow:var(--shadow);position:relative;overflow:hidden}",
        ".hero::after{content:\"\";position:absolute;inset:auto -8% -25% auto;width:320px;height:320px;background:radial-gradient(circle,rgba(255,255,255,0.24),rgba(255,255,255,0) 70%);pointer-events:none}",
        ".eyebrow{margin:0 0 10px;letter-spacing:0.14em;text-transform:uppercase;font-size:0.76rem;font-weight:700;color:rgba(255,255,255,0.72)}",
        ".hero h1{margin:0;font-size:clamp(2.2rem,5vw,4rem);line-height:1}",
        ".hero p{max-width:920px;font-size:1.05rem;margin:16px 0 0;color:rgba(255,255,255,0.9)}",
        ".hero ul{margin:18px 0 0;padding-left:20px;color:rgba(255,255,255,0.9)}",
        ".top-controls{margin:24px 0;display:grid;grid-template-columns:1.2fr 1fr;gap:20px;align-items:start}",
        ".panel{background:var(--panel);border:1px solid var(--line);border-radius:22px;padding:22px;box-shadow:var(--shadow)}",
        ".panel h2,.schema-block h2,.object-card h3,.object-card h4{margin-top:0}",
        ".summary-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:14px;margin-top:18px}",
        ".summary-card{background:linear-gradient(180deg,#ffffff,#f5fbfc);border:1px solid var(--line);border-radius:18px;padding:16px}",
        ".summary-card strong{display:block;font-size:1.85rem;margin-top:6px}",
        ".summary-label{display:block;color:var(--muted);font-size:0.85rem;text-transform:uppercase;letter-spacing:0.08em}",
        ".search-box{width:100%;padding:14px 16px;border-radius:14px;border:1px solid var(--line);font-size:1rem}",
        ".search-meta{display:flex;justify-content:space-between;gap:14px;align-items:center;margin-top:12px;flex-wrap:wrap}",
        ".button-row{display:flex;gap:10px;flex-wrap:wrap}",
        ".ghost-button{background:#fff;border:1px solid var(--line);border-radius:999px;padding:8px 12px;font-weight:600;color:var(--accent-deep);cursor:pointer}",
        ".schema-chip-row{display:flex;flex-wrap:wrap;gap:10px;margin-top:16px}",
        ".schema-chip{display:inline-flex;align-items:center;padding:8px 12px;border-radius:999px;background:var(--panel-alt);text-decoration:none;border:1px solid var(--line);color:var(--accent-deep);font-weight:600}",
        ".content-layout{display:grid;grid-template-columns:300px minmax(0,1fr);gap:24px;align-items:start;margin-top:24px}",
        ".toc-panel{position:sticky;top:16px;padding:0;background:rgba(255,255,255,0.94);backdrop-filter:blur(6px);overflow:hidden;max-height:calc(100vh - 32px)}",
        ".toc-panel-summary{list-style:none;cursor:pointer;padding:18px 22px;display:flex;justify-content:space-between;align-items:center;gap:12px;font-weight:700;background:rgba(255,255,255,0.96)}",
        ".toc-panel-summary::-webkit-details-marker{display:none}",
        ".toc-panel-summary::after{content:\"Hide\";font-size:0.85rem;color:var(--muted);font-weight:600}",
        ".toc-panel:not([open]) .toc-panel-summary::after{content:\"Show\"}",
        ".toc-panel-body{padding:0 22px 22px;border-top:1px solid var(--line);overflow:auto;max-height:calc(100vh - 110px);scrollbar-gutter:stable}",
        ".toc-panel:not([open]) .toc-panel-body{display:none}",
        ".toc-panel h2{margin:0}",
        ".toc-list{list-style:none;padding:0 4px 4px 0;margin:16px 0 0}",
        ".toc-list li+li{margin-top:10px}",
        ".toc-list a{display:block;padding:12px 14px;border-radius:16px;background:#f7fbfc;border:1px solid var(--line);text-decoration:none}",
        ".toc-list strong{display:block;color:var(--ink)}",
        ".toc-list span{display:block;color:var(--muted);font-size:0.9rem;margin-top:2px}",
        ".main-content{min-width:0}",
        ".schema-block{margin:0 0 24px;border-radius:26px;background:linear-gradient(180deg,var(--schema-tint),rgba(255,255,255,0.98) 180px);border:1px solid var(--schema-accent);box-shadow:var(--shadow);overflow:hidden}",
        ".schema-summary{list-style:none;cursor:pointer;padding:18px 22px;background:linear-gradient(180deg,var(--schema-tint),rgba(255,255,255,0.92));position:sticky;top:0;z-index:4;border-bottom:1px solid rgba(0,0,0,0.04)}",
        ".schema-summary::-webkit-details-marker,.schema-group-summary::-webkit-details-marker{display:none}",
        ".schema-body{padding:0 22px 22px}",
        ".schema-header,.object-header{display:flex;justify-content:space-between;gap:16px;align-items:start}",
        ".schema-description{color:var(--muted);max-width:980px;margin:12px 0 0}",
        ".schema-group{margin-top:18px;background:rgba(255,255,255,0.78);border:1px solid rgba(19,35,45,0.08);border-radius:20px;overflow:hidden}",
        ".schema-group-summary{list-style:none;cursor:pointer;padding:14px 18px;display:flex;justify-content:space-between;align-items:center;background:rgba(255,255,255,0.78);font-weight:700}",
        ".schema-group-body{padding:0 18px 18px}",
        ".object-card{border:1px solid rgba(19,35,45,0.08);border-radius:22px;padding:22px;margin-top:18px;background:linear-gradient(180deg,#ffffff,#fbfeff)}",
        ".object-card h3{margin-bottom:8px;word-break:break-word}",
        ".object-card h4{margin-bottom:8px;font-size:1rem}",
        ".object-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:18px;margin-top:18px}",
        ".span-2{grid-column:span 2}",
        ".object-badges{display:flex;flex-wrap:wrap;gap:8px;justify-content:end}",
        ".badge{display:inline-flex;align-items:center;padding:4px 10px;border-radius:999px;font-size:0.8rem;font-weight:700;letter-spacing:0.02em;border:1px solid transparent;white-space:nowrap}",
        ".badge-default{background:#edf6f8;color:var(--accent-deep);border-color:#cfe4ea}",
        ".badge-good{background:#e7f6ee;color:var(--good);border-color:#cfe9dc}",
        ".badge-warn{background:#fff4e5;color:#8f5600;border-color:#f1d2a3}",
        ".purpose-block{margin-top:10px;padding:14px 16px;background:var(--panel-alt);border-radius:16px;border:1px solid var(--line)}",
        ".purpose-block p{margin:10px 0 0}",
        ".data-table{width:100%;border-collapse:collapse;margin-top:10px;font-size:0.94rem}",
        ".data-table th,.data-table td{padding:10px 12px;border-bottom:1px solid var(--line);vertical-align:top;text-align:left}",
        ".data-table thead th{background:#f4fafc;position:sticky;top:0;z-index:1}",
        ".muted{color:var(--muted)}",
        ".kv-list{display:grid;grid-template-columns:220px 1fr;gap:10px 16px;margin:18px 0}",
        ".kv-list dt{font-weight:700;color:var(--muted)}",
        ".kv-list dd{margin:0}",
        ".object-card details{margin-top:14px;background:#f8fcfd;border-radius:16px;border:1px solid var(--line);padding:12px 14px}",
        "summary{cursor:pointer;font-weight:700}",
        "pre{margin:14px 0 0;padding:16px;border-radius:14px;background:#13232d;color:#ecf7fb;overflow:auto}",
        "footer{margin-top:32px;color:var(--muted);text-align:center}",
        "@media (max-width:1180px){.top-controls,.content-layout{grid-template-columns:1fr}.toc-panel{position:static;max-height:none}.toc-panel-body{max-height:none}}",
        "@media (max-width:860px){.page{width:min(100% - 28px,1600px)}.schema-header,.object-header,.object-badges,.search-meta{display:block}.object-grid{grid-template-columns:1fr}.span-2{grid-column:auto}.kv-list{grid-template-columns:1fr}}",
        schema_theme_css
      ),
      collapse = "\n"
    )

    page_js <- paste(
      c(
        "const searchBox=document.getElementById('doc-search');",
        "const statusEl=document.getElementById('search-status');",
        "const objectCards=Array.from(document.querySelectorAll('.object-card'));",
        "const groupBlocks=Array.from(document.querySelectorAll('.schema-group'));",
        "const schemaBlocks=Array.from(document.querySelectorAll('.schema-block'));",
        "const expandAll=document.getElementById('expand-all');",
        "const collapseAll=document.getElementById('collapse-all');",
        "function updateSearch(){",
        "const query=searchBox.value.trim().toLowerCase();",
        "let visibleCount=0;",
        "objectCards.forEach(card=>{const haystack=((card.dataset.search||'')+' '+card.textContent).toLowerCase();const show=!query||haystack.includes(query);card.style.display=show?'':'none';if(show){visibleCount+=1;}});",
        "groupBlocks.forEach(group=>{const visible=Array.from(group.querySelectorAll('.object-card')).some(card=>card.style.display!=='none');group.style.display=visible?'':'none';if(query&&visible){group.open=true;}});",
        "schemaBlocks.forEach(block=>{const visible=Array.from(block.querySelectorAll('.object-card')).some(card=>card.style.display!=='none');block.style.display=visible?'':'none';if(query&&visible){block.open=true;}});",
        "statusEl.textContent=query?`Showing ${visibleCount} matching objects below.`:`Showing all ${objectCards.length} documented objects below.`;",
        "}",
        "searchBox.addEventListener('input',updateSearch);",
        "expandAll.addEventListener('click',()=>{document.querySelectorAll('.schema-block,.schema-group').forEach(el=>{el.open=true;});});",
        "collapseAll.addEventListener('click',()=>{document.querySelectorAll('.schema-group,.schema-block').forEach(el=>{el.open=false;});});",
        "updateSearch();"
      ),
      collapse = ""
    )

    html <- sprintf(
      "<!DOCTYPE html>
  <html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <title>AquaCache Database Reference</title>
    <style>%s</style>
  </head>
  <body>
    <div class=\"page\">
      <section class=\"hero\">
        <p class=\"eyebrow\">AquaCache Schema Reference</p>
        <h1>Human-Readable Database Documentation</h1>
        <p>This reference is designed for readers who know some SQL concepts but do not live in the database every day. It focuses on the practical questions that usually slow people down: what each table is for, what uniquely identifies a row, which other tables it depends on, which tables depend on it, and what custom views or functions already exist.</p>
        <ul>
          <li>Generated from the live AquaCache PostgreSQL catalog using admin visibility in a read-only session.</li>
          <li>Emphasizes database comments when they exist and clearly labels inferred explanations when the schema itself is silent.</li>
          <li>Includes tables, views, functions, foreign keys, unique keys, triggers, and index notes.</li>
        </ul>
      </section>
      <section class=\"top-controls\">
        <div class=\"panel\">
          <h2>At A Glance</h2>
          <p class=\"muted\">Generated on %s. The document was built from the current live schema and is intended as a practical reference rather than a formal data dictionary export.</p>
          <div class=\"summary-grid\">%s</div>
          <h3>Version Tracking</h3>
          <ul>%s</ul>
        </div>
        <div class=\"panel\">
          <h2>How To Read This Document</h2>
          <p>Use the search box to filter the sections below in place by schema, object name, column name, or a word from the description. The page does not jump to a first match; instead it hides non-matching objects and leaves the matching ones visible. Schema sections and object-type groups can also be collapsed to make long browsing easier.</p>
          <input id=\"doc-search\" class=\"search-box\" type=\"search\" placeholder=\"Search tables, columns, views, functions, or descriptions\">
          <div class=\"search-meta\">
            <div>
              <p class=\"muted\">The search box filters the documentation below as you type.</p>
              <p id=\"search-status\" class=\"muted\"></p>
            </div>
            <div class=\"button-row\">
              <button type=\"button\" class=\"ghost-button\" id=\"expand-all\">Expand All</button>
              <button type=\"button\" class=\"ghost-button\" id=\"collapse-all\">Collapse All</button>
            </div>
          </div>
          <div class=\"schema-chip-row\">%s</div>
        </div>
      </section>
      <div class=\"content-layout\">
        <details class=\"panel toc-panel\" open>
          <summary class=\"toc-panel-summary\">
            <h2>Schema TOC</h2>
          </summary>
          <div class=\"toc-panel-body\">
            <p class=\"muted\">Jump to a schema section. This panel can be collapsed, and the list inside it scrolls independently when there are more schemas than fit on screen.</p>
            <ul class=\"toc-list\">%s</ul>
          </div>
        </details>
        <main class=\"main-content\">
          <section class=\"panel\">
            <h2>Schema Overview</h2>
            <p class=\"muted\">AquaCache is organized into topic-oriented schemas. The <code>public</code> schema carries the core reference and location model; most other schemas hang off it.</p>
            %s
          </section>
          %s
        </main>
      </div>
      <footer>
        <p>Output file: %s</p>
      </footer>
    </div>
    <script>%s</script>
  </body>
  </html>",
      page_css,
      escape_html(generation_stamp),
      summary_cards,
      version_summary,
      schema_nav,
      toc_items,
      format_simple_table(schema_overview_df),
      paste(cards_html, collapse = ""),
      escape_html(normalizePath(output_path, winslash = "/", mustWork = FALSE)),
      page_js
    )

    html
  }

  selected_schemas <- unique(as.character(schemas %||% target_schemas))
  if (length(selected_schemas) == 0 || any(trimws(selected_schemas) == "")) {
    stop("`schemas` must contain at least one non-empty schema name.")
  }

  if (is.null(con)) {
    con <- connect_catalog()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  if (!DBI::dbIsValid(con)) {
    stop("`con` must be a valid DBI connection.")
  }

  if (isTRUE(check_visibility)) {
    check_catalog_visibility(con, target_schemas = selected_schemas)
  }

  catalog <- fetch_catalog(con, target_schemas = selected_schemas)
  html <- build_html(
    catalog,
    output_path = output_file,
    selected_schemas = selected_schemas
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, output_file, useBytes = TRUE)

  out <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  if (isTRUE(open)) {
    utils::browseURL(out)
  }

  invisible(out)
}
