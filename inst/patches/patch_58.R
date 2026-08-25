# Patch 58 makes source identity complete and unique for imported locationless
# discrete samples, which have no location context available for matching. It
# also moves sample-group types into a queryable, bilingual reference table.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 58: enforcing complete, unique source identities for locationless discrete samples and creating the sample-group type catalogue. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.samples') IS NOT NULL AS has_samples,
         to_regclass('discrete.sample_groups') IS NOT NULL AS has_sample_groups,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 58 requires the discrete sample and sample-group tables and information.version_info created by earlier patches."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "57") {
      stop("Patch 58 must be applied to a database at Patch 57.")
    }

    target_objects <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass(
           'discrete.sample_group_types'
         ) IS NOT NULL AS has_group_type_table,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'discrete.samples'::regclass
             AND conname = 'samples_import_source_identity_complete'
         ) AS has_identity_constraint,
         to_regclass(
           'discrete.samples_import_source_identity_key'
         ) IS NOT NULL AS has_identity_index"
    )
    if (any(unlist(target_objects[1, ], use.names = FALSE))) {
      stop(
        "Patch 58 target constraint or index already exists. Investigate the partial migration before applying this patch."
      )
    }

    old_group_type_check <- DBI::dbGetQuery(
      con,
      "SELECT oid
       FROM pg_constraint
       WHERE conrelid = 'discrete.sample_groups'::regclass
         AND conname = 'sample_groups_group_type_check'
         AND contype = 'c'"
    )
    if (nrow(old_group_type_check) != 1L) {
      stop(
        "Patch 58 requires the sample_groups_group_type_check constraint created by Patch 57."
      )
    }

    invalid_identity <- DBI::dbGetQuery(
      con,
      "SELECT sample_id
       FROM discrete.samples
       WHERE location_id IS NULL
         AND NOT (
         (import_source IS NULL AND import_source_id IS NULL)
         OR (
           NULLIF(btrim(import_source), '') IS NOT NULL
           AND NULLIF(btrim(import_source_id), '') IS NOT NULL
         )
         )
       ORDER BY sample_id"
    )
    if (nrow(invalid_identity) > 0L) {
      stop(
        "Patch 58 found locationless discrete samples with incomplete or blank source identity: ",
        paste(invalid_identity$sample_id, collapse = ", "),
        ". Set both import_source and import_source_id, or set both to NULL, before applying this patch."
      )
    }

    duplicate_identity <- DBI::dbGetQuery(
      con,
      "SELECT
         import_source,
         import_source_id,
         array_agg(sample_id ORDER BY sample_id) AS sample_ids
       FROM discrete.samples
       WHERE location_id IS NULL
         AND import_source IS NOT NULL
         AND import_source_id IS NOT NULL
       GROUP BY import_source, import_source_id
       HAVING count(*) > 1
       ORDER BY import_source, import_source_id"
    )
    if (nrow(duplicate_identity) > 0L) {
      duplicate_text <- paste0(
        duplicate_identity$import_source,
        "/",
        duplicate_identity$import_source_id,
        " -> ",
        vapply(
          duplicate_identity$sample_ids,
          function(x) paste(x, collapse = ","),
          character(1)
        )
      )
      stop(
        "Patch 58 found duplicate locationless sample source identities: ",
        paste(duplicate_text, collapse = "; "),
        ". Resolve each duplicate explicitly before applying this patch."
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.sample_group_types (
         group_type TEXT PRIMARY KEY,
         group_type_name TEXT NOT NULL UNIQUE,
         group_type_name_fr TEXT NOT NULL UNIQUE,
         description TEXT NOT NULL,
         description_fr TEXT NOT NULL,
         sort_order SMALLINT NOT NULL UNIQUE CHECK (sort_order > 0),
         active BOOLEAN NOT NULL DEFAULT TRUE
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_group_types OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.sample_group_types (
         group_type,
         group_type_name,
         group_type_name_fr,
         description,
         description_fr,
         sort_order
       ) VALUES
         (
           'field_event',
           'Field event',
           'Événement sur le terrain',
           'Samples collected or prepared as part of one field event.',
           'Échantillons prélevés ou préparés dans le cadre d''un même événement sur le terrain.',
           1
         ),
         (
           'trip',
           'Trip',
           'Sortie sur le terrain',
           'Samples associated with one sampling trip.',
           'Échantillons associés à une même sortie sur le terrain.',
           2
         ),
         (
           'cooler',
           'Cooler',
           'Glacière',
           'Samples transported or stored in one cooler.',
           'Échantillons transportés ou entreposés dans une même glacière.',
           3
         ),
         (
           'shipment',
           'Shipment',
           'Expédition',
           'Samples included in one shipment.',
           'Échantillons compris dans une même expédition.',
           4
         ),
         (
           'lab_batch',
           'Laboratory batch',
           'Lot de laboratoire',
           'Samples prepared or analyzed in one laboratory batch.',
           'Échantillons préparés ou analysés dans un même lot de laboratoire.',
           5
         ),
         (
           'qc_set',
           'Quality-control set',
           'Ensemble de contrôle de la qualité',
           'Samples related through one quality-control assessment.',
           'Échantillons liés par une même évaluation de contrôle de la qualité.',
           6
         ),
         (
           'other',
           'Other',
           'Autre',
           'Another explicitly identified sample-group context.',
           'Autre contexte de regroupement d''échantillons explicitement identifié.',
           7
         )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_group_types IS
       'Governed bilingual catalogue of operational contexts used to group discrete samples. Applications should list active rows ordered by sort_order.'"
    )
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE discrete.sample_group_types TO PUBLIC"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_groups
       DROP CONSTRAINT sample_groups_group_type_check,
       ADD CONSTRAINT sample_groups_group_type_fkey
         FOREIGN KEY (group_type)
         REFERENCES discrete.sample_group_types(group_type)
         ON UPDATE CASCADE
         ON DELETE RESTRICT"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.samples
       ADD CONSTRAINT samples_import_source_identity_complete CHECK (
         location_id IS NOT NULL
         OR (
           (import_source IS NULL AND import_source_id IS NULL)
           OR (
             NULLIF(btrim(import_source), '') IS NOT NULL
             AND NULLIF(btrim(import_source_id), '') IS NOT NULL
           )
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX samples_import_source_identity_key
       ON discrete.samples (import_source, import_source_id)
       WHERE location_id IS NULL
         AND import_source IS NOT NULL
         AND import_source_id IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.import_source IS
       'Registered source-adapter function that supplied the sample. For a locationless sample, it must be paired with a nonblank import_source_id and the pair uniquely identifies the sample.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.samples.import_source_id IS
       'Stable source-specific sample identifier. For a locationless sample, it must be paired with a nonblank import_source and is unique within that source. Located samples may reuse a source ID at different locations.'"
    )

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass(
           'discrete.sample_group_types'
         ) IS NOT NULL AS has_group_type_table,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'discrete.sample_groups'::regclass
             AND conname = 'sample_groups_group_type_fkey'
             AND contype = 'f'
         ) AS has_group_type_fkey,
         (
           SELECT count(*) = 7 AND count(*) FILTER (WHERE active) = 7
           FROM discrete.sample_group_types
         ) AS has_seed_group_types,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'discrete.samples'::regclass
             AND conname = 'samples_import_source_identity_complete'
             AND contype = 'c'
         ) AS has_identity_constraint,
         to_regclass(
           'discrete.samples_import_source_identity_key'
         ) IS NOT NULL AS has_identity_index"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop("Patch 58 verification failed.")
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '58'
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
      "Patch 58 applied successfully. Imported locationless samples now have complete, unique source identities, and sample-group types are available from a governed bilingual catalogue."
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
