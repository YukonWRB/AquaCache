# Patch 4.
# Creates new table 'qualifier' to hold information such as 'ice affected', 'estimated', 'draw-down', etc. for data quality control.

# Initial checks #################
# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(
  con,
  "SELECT has_database_privilege(current_user, 'CREATE') AS can_create"
)

if (!check$can_create) {
  stop(
    "You do not have the necessary privileges to create a new schema in this database."
  )
}

message(
  "Working on Patch 4. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    # Delete columns for 'owner' and 'contributor' from table measurements_discrete
    DBI::dbExecute(
      con,
      "ALTER TABLE measurements_discrete DROP COLUMN owner CASCADE"
    ) # Cascades to view table 'measurements_discrete_corrected'
    DBI::dbExecute(
      con,
      "ALTER TABLE measurements_discrete DROP COLUMN contributor CASCADE"
    ) # Cascades to view table 'measurements_discrete_corrected'

    # Add a sample type to table sample_types to allow differentiation between lab protocols/methods. Permits a single sample to be split at one lab between two methods.
    df <- data.frame(sample_type = "QC-sample-lab protocol split")
    DBI::dbAppendTable(con, "sample_types", df)

    # Change table 'measurements_discrete'; add column for 'analysis_datetime' (timestamp with timezone), nullable
    DBI::dbExecute(
      con,
      "ALTER TABLE measurements_discrete ADD COLUMN analysis_datetime TIMESTAMP WITH TIME ZONE"
    )
    # Drop the old unique key and recreate to include analysis_datetime
    DBI::dbExecute(
      con,
      "ALTER TABLE measurements_discrete DROP CONSTRAINT measurements_discrete_unique"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE measurements_discrete ADD CONSTRAINT measurements_discrete_unique UNIQUE NULLS NOT DISTINCT (timeseries_id, datetime, sample_type, collection_method, sample_fraction, result_speciation, result_value_type, analysis_datetime)"
    )

    # Move all the 'discrete' related tables and views to a new schema
    # Tables are measurements_discrete, sample_types, result_value_types, result_conditions, collection_methods, sample_fractions, laboratories, result_speciation, analysis_protocols
    # View are measurements_discrete_corrected
    DBI::dbExecute(con, "CREATE SCHEMA measurements_discrete;")
    DBI::dbExecute(
      con,
      "ALTER TABLE measurements_discrete SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE sample_types SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE result_value_types SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE result_conditions SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE collection_methods SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE sample_fractions SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE laboratories SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE result_speciations SET SCHEMA measurements_discrete;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE analysis_protocols SET SCHEMA measurements_discrete;"
    )

    # Move table 'internal_status' from public schema to 'information'
    DBI::dbExecute(con, "ALTER TABLE internal_status SET SCHEMA information;")

    # Re-create the view table 'measurements_discrete_corrected' without the 'owner' and 'contributor' columns
    DBI::dbExecute(
      con,
      "
                 CREATE OR REPLACE VIEW measurements_discrete.measurements_discrete_corrected
AS SELECT md.timeseries_id,
    md.target_datetime,
    md.datetime,
    md.value AS value_raw,
    ac.corrected_value AS value_corrected,
    md.result_value_type,
    md.result_condition,
    md.result_condition_value,
    md.sample_type,
    md.collection_method,
    md.sample_fraction,
    md.result_speciation,
    md.lab,
    md.protocol,
    md.note
   FROM measurements_discrete md
     LEFT JOIN LATERAL ( SELECT apply_corrections(md.timeseries_id, md.datetime, md.value) AS corrected_value) ac ON true
  WHERE md.value IS NOT NULL AND ac.corrected_value IS NOT NULL OR md.value IS NULL;
                 "
    )

    # Create new schema 'spatial'
    DBI::dbExecute(con, "CREATE SCHEMA spatial;")
    # Move spatial tables: spatial_ref_sys, rasters, rasters_reference, vectors
    DBI::dbExecute(con, "ALTER TABLE spatial_ref_sys SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE rasters SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE rasters_reference SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE raster_series_index SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE vectors SET SCHEMA spatial;")
    # And the views 'geography_columns', 'geometry_columns', 'raster_columns', 'raster_overviews'
    DBI::dbExecute(con, "ALTER TABLE geography_columns SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE geometry_columns SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE raster_columns SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE raster_overviews SET SCHEMA spatial;")

    # Alter the default search path to include the new schema 'measurements_discrete' and 'spatial'
    curr_db_name <- DBI::dbGetQuery(
      con,
      "SELECT current_database() AS db_name"
    )[1, 1]
    DBI::dbExecute(
      con,
      paste0(
        'ALTER DATABASE "',
        curr_db_name,
        '" SET search_path TO public, measurements_discrete, instruments, spatial, information;'
      )
    )

    # Grand select privileges on the new schema to the ac_editor and hydromet_read roles
    try({
      DBI::dbExecute(
        con,
        "GRANT USAGE ON SCHEMA measurements_discrete TO ac_editor, hydromet_read;"
      )
      DBI::dbExecute(
        con,
        "GRANT SELECT ON ALL TABLES IN SCHEMA measurements_discrete TO ac_editor, hydromet_read;"
      )
    })
    try({
      DBI::dbExecute(
        con,
        "GRANT USAGE ON SCHEMA spatial TO ac_editor, hydromet_read;"
      )
      DBI::dbExecute(
        con,
        "GRANT SELECT ON ALL TABLES IN SCHEMA spatial TO ac_editor, hydromet_read;"
      )
    })

    # modify existing column 'timeseries.owner': make a NOT NULL field, with FK to owners_contributors.organization_id
    DBI::dbExecute(
      con,
      "ALTER TABLE timeseries ALTER COLUMN owner SET NOT NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE timeseries ADD CONSTRAINT fk_owner FOREIGN KEY (owner) REFERENCES owners_contributors(organization_id)"
    )

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '4' WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion("AquaCache")),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")
    attr(con, "active_transaction") <- FALSE

    message(
      "Patch 4 applied successfully: table measurements_discrete modified. Columns owner, contributor removed as these data are held in stand-alone tables. Any new discrete data brought in to the database will use the adjust_ family of functions to add optional owner, contributor, qualifier, grade, approval data in stand-alone tables."
    )

    warning(
      "New schemas were created and tables were moved into these. You may need to close and recreate this connection to find these tables without specifying the schema each time."
    )
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 4 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
