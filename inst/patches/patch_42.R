# Patch 42: restore invoker-scoped RLS for timeseries metadata
#
# Patch 41 recreated continuous.timeseries_metadata_en and
# continuous.timeseries_metadata_fr without security_invoker=true. Because those
# views are owned by a privileged role, public_reader could see metadata for
# timeseries rows hidden by continuous.timeseries RLS.
#
# Keep this patch deliberately narrow. Earlier parent-visibility patches had to
# avoid recursive RLS policy graphs, so this patch does not rediscover and
# recreate policies for every FK. It only:
# - restores invoker execution on the affected views;
# - adds explicit parent visibility policies for the new compound timeseries
#   tables from patch 41;
# - adds explicit parent visibility policies for a small set of dependent
#   tables found without existing parent policies during the localhost audit.

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 42: tightening invoker-scoped visibility for timeseries metadata and selected dependent tables. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
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
    check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.timeseries_metadata_en') IS NOT NULL AS has_timeseries_metadata_en,
         to_regclass('continuous.timeseries_metadata_fr') IS NOT NULL AS has_timeseries_metadata_fr,
         EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'public_reader') AS has_public_reader"
    )

    if (
      !isTRUE(check$has_timeseries[[1]]) ||
        !isTRUE(check$has_timeseries_metadata_en[[1]]) ||
        !isTRUE(check$has_timeseries_metadata_fr[[1]]) ||
        !isTRUE(check$has_public_reader[[1]])
    ) {
      stop(
        "This patch requires continuous.timeseries, continuous.timeseries_metadata_en, continuous.timeseries_metadata_fr, and role public_reader to already exist."
      )
    }

    q_table <- function(schema_name, table_name) {
      paste(
        as.character(DBI::dbQuoteIdentifier(con, schema_name)),
        as.character(DBI::dbQuoteIdentifier(con, table_name)),
        sep = "."
      )
    }

    rel_exists <- function(schema_name, table_name) {
      qualified_name <- paste(schema_name, table_name, sep = ".")
      res <- DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1) IS NOT NULL AS exists",
        params = list(qualified_name)
      )
      isTRUE(res$exists[[1]])
    }

    apply_parent_visibility_policy <- function(
      schema_name,
      table_name,
      using_sql
    ) {
      qual <- q_table(schema_name, table_name)

      DBI::dbExecute(
        con,
        sprintf("ALTER TABLE %s ENABLE ROW LEVEL SECURITY", qual)
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_restrict ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_restrict ON %s AS RESTRICTIVE FOR SELECT USING (%s)",
          qual,
          using_sql
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_restrict_insert ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_restrict_insert ON %s AS RESTRICTIVE FOR INSERT WITH CHECK (%s)",
          qual,
          using_sql
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_restrict_update ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_restrict_update ON %s AS RESTRICTIVE FOR UPDATE USING (%s) WITH CHECK (%s)",
          qual,
          using_sql,
          using_sql
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_restrict_delete ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_restrict_delete ON %s AS RESTRICTIVE FOR DELETE USING (%s)",
          qual,
          using_sql
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_allow_select ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_allow_select ON %s FOR SELECT USING (true)",
          qual
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_allow_insert ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_allow_insert ON %s FOR INSERT WITH CHECK (true)",
          qual
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_allow_update ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_allow_update ON %s FOR UPDATE USING (true) WITH CHECK (true)",
          qual
        )
      )

      DBI::dbExecute(
        con,
        sprintf(
          "DROP POLICY IF EXISTS parent_visibility_allow_delete ON %s",
          qual
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE POLICY parent_visibility_allow_delete ON %s FOR DELETE USING (true)",
          qual
        )
      )
    }

    visible_timeseries_sql <- function(child_table, child_column) {
      sprintf(
        "%1$s IS NULL OR EXISTS (
           SELECT 1
           FROM continuous.timeseries ts
           WHERE ts.timeseries_id = %2$s.%1$s
         )",
        child_column,
        child_table
      )
    }

    message(
      "Setting continuous timeseries metadata views to SECURITY INVOKER..."
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.timeseries_metadata_en
       SET (security_invoker = true, security_barrier = true)"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.timeseries_metadata_fr
       SET (security_invoker = true, security_barrier = true)"
    )

    if (rel_exists("continuous", "timeseries_compound_dependencies")) {
      DBI::dbExecute(
        con,
        "ALTER VIEW continuous.timeseries_compound_dependencies
         SET (security_invoker = true, security_barrier = true)"
      )
    }

    message(
      "Adding explicit parent visibility policies for selected dependent tables..."
    )
    if (rel_exists("continuous", "timeseries_compounds")) {
      apply_parent_visibility_policy(
        "continuous",
        "timeseries_compounds",
        visible_timeseries_sql("timeseries_compounds", "timeseries_id")
      )
    }

    if (rel_exists("continuous", "timeseries_compound_members")) {
      apply_parent_visibility_policy(
        "continuous",
        "timeseries_compound_members",
        paste(
          "(",
          visible_timeseries_sql(
            "timeseries_compound_members",
            "timeseries_id"
          ),
          ") AND (",
          visible_timeseries_sql(
            "timeseries_compound_members",
            "member_timeseries_id"
          ),
          ")",
          sep = ""
        )
      )
    }

    if (
      rel_exists(
        "public",
        "locations_metadata_instrument_connection_signals"
      )
    ) {
      apply_parent_visibility_policy(
        "public",
        "locations_metadata_instrument_connection_signals",
        visible_timeseries_sql(
          "locations_metadata_instrument_connection_signals",
          "timeseries_id"
        )
      )
    }

    if (rel_exists("field", "field_visit_images")) {
      apply_parent_visibility_policy(
        "field",
        "field_visit_images",
        "(
          field_visit_id IS NULL
          OR EXISTS (
            SELECT 1
            FROM field.field_visits fv
            WHERE fv.field_visit_id = field_visit_images.field_visit_id
          )
        ) AND (
          image_id IS NULL
          OR EXISTS (
            SELECT 1
            FROM files.images img
            WHERE img.image_id = field_visit_images.image_id
          )
        )"
      )
    }

    if (rel_exists("field", "field_visit_instruments")) {
      apply_parent_visibility_policy(
        "field",
        "field_visit_instruments",
        "field_visit_id IS NULL
         OR EXISTS (
           SELECT 1
           FROM field.field_visits fv
           WHERE fv.field_visit_id = field_visit_instruments.field_visit_id
         )"
      )
    }

    if (rel_exists("boreholes", "boreholes_no_coords_documents")) {
      apply_parent_visibility_policy(
        "boreholes",
        "boreholes_no_coords_documents",
        "document_id IS NULL
         OR EXISTS (
           SELECT 1
           FROM files.documents d
           WHERE d.document_id = boreholes_no_coords_documents.document_id
         )"
      )
    }

    if (rel_exists("files", "documents_spatial")) {
      apply_parent_visibility_policy(
        "files",
        "documents_spatial",
        "document_id IS NULL
         OR EXISTS (
           SELECT 1
           FROM files.documents d
           WHERE d.document_id = documents_spatial.document_id
         )"
      )
    }

    message("Checking public_reader visibility through patched relations...")
    DBI::dbExecute(con, "SET LOCAL ROLE public_reader;")

    leak_checks <- DBI::dbGetQuery(
      con,
      "SELECT check_name, leak_count
       FROM (
         SELECT
           'continuous.timeseries_metadata_en' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_metadata_en v
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = v.timeseries_id
         WHERE ts.timeseries_id IS NULL

         UNION ALL

         SELECT
           'continuous.timeseries_metadata_fr' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_metadata_fr v
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = v.timeseries_id
         WHERE ts.timeseries_id IS NULL

         UNION ALL

         SELECT
           'continuous.timeseries_compound_dependencies compound' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_compound_dependencies d
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = d.compound_timeseries_id
         WHERE ts.timeseries_id IS NULL

         UNION ALL

         SELECT
           'continuous.timeseries_compound_dependencies member' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_compound_dependencies d
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = d.member_timeseries_id
         WHERE ts.timeseries_id IS NULL

         UNION ALL

         SELECT
           'continuous.timeseries_compounds' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_compounds c
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = c.timeseries_id
         WHERE ts.timeseries_id IS NULL

         UNION ALL

         SELECT
           'continuous.timeseries_compound_members compound' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_compound_members m
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = m.timeseries_id
         WHERE ts.timeseries_id IS NULL

         UNION ALL

         SELECT
           'continuous.timeseries_compound_members member' AS check_name,
           COUNT(*)::integer AS leak_count
         FROM continuous.timeseries_compound_members m
         LEFT JOIN continuous.timeseries ts
           ON ts.timeseries_id = m.member_timeseries_id
         WHERE ts.timeseries_id IS NULL
       ) x
       WHERE leak_count > 0"
    )

    recursion_checks <- DBI::dbGetQuery(
      con,
      "SELECT 'continuous.timeseries_metadata_en' AS check_name, COUNT(*)::integer AS row_count
       FROM continuous.timeseries_metadata_en
       UNION ALL
       SELECT 'continuous.timeseries_metadata_fr', COUNT(*)::integer
       FROM continuous.timeseries_metadata_fr
       UNION ALL
       SELECT 'continuous.timeseries_compounds', COUNT(*)::integer
       FROM continuous.timeseries_compounds
       UNION ALL
       SELECT 'continuous.timeseries_compound_members', COUNT(*)::integer
       FROM continuous.timeseries_compound_members"
    )

    DBI::dbExecute(con, "RESET ROLE;")

    if (nrow(leak_checks) > 0) {
      stop(
        paste(
          "Patch 42 visibility check failed:",
          paste(
            paste0(leak_checks$check_name, "=", leak_checks$leak_count),
            collapse = ", "
          )
        )
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '42'
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
      "Patch 42 applied successfully. Timeseries metadata views now execute as the caller and selected dependent tables have explicit parent-visibility RLS policies."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    stop(
      "Patch 42 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
