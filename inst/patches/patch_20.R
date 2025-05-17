# Patch 20

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 20. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

message("This patch modifies the column 'period_type' of table 'timeseries'. This column gets renamed aggregation_type_id, and it will now refer to a table.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Drop the check constraint on the column period_type
  DBI::dbExecute(con, "ALTER TABLE timeseries DROP CONSTRAINT IF EXISTS timeseries_period_type_check;")
  # Rename column period_type to aggregation_type_id
  DBI::dbExecute(con, "ALTER TABLE timeseries RENAME COLUMN period_type TO aggregation_type_id;")

  # Create the new table for aggregation types
  DBI::dbExecute(con, "CREATE TABLE continuous.aggregation_types (
    aggregation_type_id SERIAL PRIMARY KEY,
    aggregation_type TEXT NOT NULL UNIQUE,
    aggregation_type_fr TEXT NOT NULL UNIQUE,
    description TEXT,
    description_fr TEXT
  );")
  
  # Insert the aggregation types into the new table
  tbl <- data.frame(
    aggregation_type = c("instantaneous", "mean", "median", "min", "max", "(min+max)/2", "sum"),
    aggregation_type_fr = c("instantané", "moyenne", "médiane", "minimum", "maximum", "(min+max)/2", "sum"),
    description = c("Instantaneous value, no period assigned.", 
                    "Mean value over the period.",
                    "Median value over the period.",
                    "Minimum value over the period.",
                    "Maximum value over the period.",
                    "Average of the minimum and maximum values over the period. (Meteorological daily mean)",
                    "Sum of the values over the period."),
    description_fr = c("Valeur instantanée, pas de période assignée.",
                       "Valeur moyenne sur la période.",
                       "Valeur médiane sur la période.",
                       "Valeur minimale sur la période.",
                       "Valeur maximale sur la période.",
                       "Moyenne des valeurs minimales et maximales sur la période. (Moyenne quotidienne météorologique)",
                       "Somme des valeurs sur la période.")
  )
  DBI::dbAppendTable(con, "aggregation_types", tbl)
  
  # Update the timeseries table to use the new aggregation_type_id
  # Get the aggregation_type_id for each aggregation_type in the timeseries table
  aggregation_types <- DBI::dbGetQuery(con, "SELECT aggregation_type_id, aggregation_type FROM continuous.aggregation_types;")
  for (i in 1:nrow(aggregation_types)) {
    DBI::dbExecute(con, paste0("UPDATE timeseries SET aggregation_type_id = ", aggregation_types$aggregation_type_id[i], " WHERE aggregation_type_id = '", aggregation_types$aggregation_type[i], "';"))
  }
  
  # Drop the timeseries_metadata_en and timeseries_metadata_fr views as they depend on this column; recreate them later
  DBI::dbExecute(con, "DROP VIEW IF EXISTS timeseries_metadata_en;")
  DBI::dbExecute(con, "DROP VIEW IF EXISTS timeseries_metadata_fr;")
  
  # Now there should only be numbers in the text column in table timeseries, convert it to integer
  DBI::dbExecute(con, "ALTER TABLE timeseries ALTER COLUMN aggregation_type_id TYPE INTEGER USING aggregation_type_id::integer;")
  DBI::dbExecute(con, "ALTER TABLE timeseries ALTER COLUMN aggregation_type_id SET NOT NULL;")
  DBI::dbExecute(con, "ALTER TABLE timeseries ADD CONSTRAINT fk_aggregation_type FOREIGN KEY (aggregation_type_id) REFERENCES aggregation_types(aggregation_type_id);")
  
  # Rename the 'min' and 'max' of table aggregation_types to 'minimum' and 'maximum'
  DBI::dbExecute(con, "UPDATE aggregation_types SET aggregation_type = 'minimum' WHERE aggregation_type = 'min';")
  DBI::dbExecute(con, "UPDATE aggregation_types SET aggregation_type = 'maximum' WHERE aggregation_type = 'max';")
  
  
  DBI::dbExecute(con, "
  CREATE OR REPLACE VIEW continuous.timeseries_metadata_en
  WITH(security_invoker=true)
  AS SELECT ts.timeseries_id,
  mtypes.media_type,
  params.param_name AS parameter_name,
  params.unit_default AS default_units,
  params.unit_solid AS units_solid_medium,
  at.aggregation_type,
  ts.record_rate AS recording_rate,
  ts.start_datetime,
  ts.end_datetime,
  ts.note,
  loc.location_id,
  loc.location AS location_code,
  loc.name AS location_name,
  loc.latitude,
  loc.longitude
  FROM timeseries ts
  JOIN locations loc ON ts.location_id = loc.location_id
  LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
  LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
  LEFT JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
  ORDER BY ts.timeseries_id;
  ")
  DBI::dbExecute(con, '
  ALTER TABLE continuous.timeseries_metadata_en OWNER TO "admin";
  ')
  DBI::dbExecute(con, '
  GRANT ALL ON TABLE continuous.timeseries_metadata_en TO "admin";
    ')
  DBI::dbExecute(con, '
  GRANT SELECT ON TABLE continuous.timeseries_metadata_en TO public_reader;
    ')
  DBI::dbExecute(con, '
  GRANT SELECT ON TABLE continuous.timeseries_metadata_en TO discrete_editor;
    ')
  DBI::dbExecute(con, '
  GRANT DELETE, INSERT, SELECT, UPDATE ON TABLE continuous.timeseries_metadata_en TO continuous_editor;
    ')
  DBI::dbExecute(con, '
  GRANT SELECT ON TABLE continuous.timeseries_metadata_en TO yg_reader;
  ')
  
  DBI::dbExecute(con, '
  CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
  WITH(security_invoker=true)
  AS SELECT ts.timeseries_id,
  mtypes.media_type_fr AS "type_de_média",
  params.param_name_fr AS "nom_paramètre",
  params.unit_default AS "unités_par_défaut",
  params.unit_solid AS "unités_media_solide",
  ag.aggregation_type_fr AS "type_agrégation",
  ts.record_rate AS "fréquence_enregistrement",
  ts.start_datetime AS "début",
  ts.end_datetime AS fin,
  ts.note,
  loc.location_id,
  loc.location AS location_code,
  loc.name_fr AS nom_endroit,
  loc.latitude,
  loc.longitude
  FROM timeseries ts
  JOIN locations loc ON ts.location_id = loc.location_id
  LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
  LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
  LEFT JOIN aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id
  ORDER BY ts.timeseries_id;
  ')
  
  DBI::dbExecute(con, '
  ALTER TABLE continuous.timeseries_metadata_fr OWNER TO "admin";
  ')
  DBI::dbExecute(con, '
  GRANT ALL ON TABLE continuous.timeseries_metadata_fr TO "admin";
    ')
  DBI::dbExecute(con, '
  GRANT SELECT ON TABLE continuous.timeseries_metadata_fr TO public_reader;
    ')
  DBI::dbExecute(con, '
  GRANT SELECT ON TABLE continuous.timeseries_metadata_fr TO discrete_editor;
    ')
  DBI::dbExecute(con, '
  GRANT DELETE, INSERT, SELECT, UPDATE ON TABLE continuous.timeseries_metadata_fr TO continuous_editor;
    ')
  DBI::dbExecute(con, '
  GRANT SELECT ON TABLE continuous.timeseries_metadata_fr TO yg_reader;
  ')
  
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '20' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  message("Patch 20 applied successfully.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 20 failed and the DB has been rolled back to its earlier state. ", e$message)
})
