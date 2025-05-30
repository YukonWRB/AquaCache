# Patch 21

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 21. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

message("This patch updates the timeseries_medatada views to give a more complete picture of the timeseries table and associated data.")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # First change the owner of table 'rasters' to 'admin' if it's still 'posgres'
  DBI::dbExecute(con, "ALTER TABLE rasters OWNER TO admin;")
  
  # Drop the views
  DBI::dbExecute(con, "DROP VIEW IF EXISTS continuous.timeseries_metadata_en;")
  DBI::dbExecute(con, "DROP VIEW IF EXISTS continuous.timeseries_metadata_fr;")
  
  # Now update the English version
  DBI::dbExecute(con, "
  CREATE VIEW continuous.timeseries_metadata_en
  WITH(security_invoker=true)
  AS SELECT 
  ts.timeseries_id,
  loc.location_id,
  loc.name AS location_name,
  ts.z AS depth_height_m,
  mtypes.media_type,
  params.param_name AS parameter_name,
  params.unit_default AS units,
  at.aggregation_type,
  ts.record_rate AS recording_rate,
  ts.start_datetime,
  ts.end_datetime,
  ts.note
  FROM timeseries ts
  JOIN locations loc ON ts.location_id = loc.location_id
  LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
  LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
  LEFT JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
  ORDER BY ts.timeseries_id;
  ")
  
  # Now update the French version
  DBI::dbExecute(con, '
  CREATE VIEW continuous.timeseries_metadata_fr
  WITH(security_invoker=true)
  AS SELECT 
  ts.timeseries_id,
      loc.location_id,
    loc.name_fr AS nom_endroit,
    ts.z AS "profondeur_hauteur_m",
    mtypes.media_type_fr AS "type_de_média",
    params.param_name_fr AS "nom_paramètre",
    params.unit_default AS "unités",
    ag.aggregation_type_fr AS "type_agrégation",
    ts.record_rate AS "fréquence_enregistrement",
    ts.start_datetime AS "début",
    ts.end_datetime AS fin,
    ts.note
   FROM timeseries ts
     JOIN locations loc ON ts.location_id = loc.location_id
     LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
     LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
     LEFT JOIN aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id
  ORDER BY ts.timeseries_id;
  ')
  
  # Improve performance of measurements_corrected_hourly view by dropping an ORDER BY clause
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW continuous.measurements_hourly_corrected
AS SELECT timeseries_id,
    date_trunc('hour'::text, datetime) AS datetime,
    avg(value_raw) AS value_raw,
    avg(value_corrected) AS value_corrected,
    bool_or(imputed) AS imputed
   FROM measurements_continuous_corrected mcc
  GROUP BY timeseries_id, (date_trunc('hour'::text, datetime));
")
  
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW continuous.measurements_continuous_corrected AS
    SELECT mc.timeseries_id,
           mc.datetime,
           mc.value AS value_raw,
           COALESCE(ac.value_corrected, mc.value) AS value_corrected,
           mc.period,
           mc.imputed
      FROM continuous.measurements_continuous mc
      JOIN continuous.timeseries ts ON mc.timeseries_id = ts.timeseries_id
      LEFT JOIN LATERAL (
            SELECT continuous.apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
              FROM continuous.corrections c
             WHERE c.timeseries_id = mc.timeseries_id
               AND mc.datetime <@ tstzrange(c.start_dt, c.end_dt)
      ) ac ON TRUE
     WHERE 'public_reader'::text = ANY (ts.share_with)
        OR EXISTS (
            SELECT 1
              FROM unnest(ts.share_with) role(role)
             WHERE pg_has_role(CURRENT_USER, (role.role)::name, 'MEMBER'::text)
               AND ((role.role)::name IN ( SELECT pg_roles.rolname FROM pg_roles))
           );")
  
  
  # Add a few indices for performance
  DBI::dbExecute(con, "CREATE INDEX idx_corrections_timeseries_range_gist ON continuous.corrections USING gist (timeseries_id, tstzrange(start_dt, end_dt));")
  
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION continuous.trunc_hour_utc(ts timestamptz)
RETURNS timestamptz
LANGUAGE sql IMMUTABLE AS
$$ SELECT date_trunc('hour', ts AT TIME ZONE 'UTC') AT TIME ZONE 'UTC'; $$;")
DBI::dbExecute(con, "CREATE INDEX measurements_continuous_hour_idx
    ON continuous.measurements_continuous
        USING btree (timeseries_id, continuous.trunc_hour_utc(datetime));")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '21' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  message("Patch 21 applied successfully.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 21 failed and the DB has been rolled back to its earlier state. ", e$message)
})
