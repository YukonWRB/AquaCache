# Patch 34

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 34. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    # DRop + re-create timeseries view with more information
    DBI::dbExecute(con, "DROP VIEW IF EXISTS timeseries_metadata_en;")
    DBI::dbExecute(con, "DROP VIEW IF EXISTS timeseries_metadata_fr;")

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW continuous.timeseries_metadata_en
    WITH(security_invoker=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name AS location_name,
        loc.alias AS alias_name,
        lz.z_meters AS depth_height_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS location_elevation,
        array_agg(DISTINCT proj.name) AS projects,
        array_agg(DISTINCT net.name) AS networks,
        mtypes.media_type,
        params.param_name AS parameter_name,
        params.unit_default AS units,
        at.aggregation_type,
        ts.record_rate AS recording_rate,
        ts.sensor_priority,
        ts.start_datetime,
        ts.end_datetime,
        ts.note
       FROM timeseries ts
         JOIN locations loc ON ts.location_id = loc.location_id
         LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
         LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
         LEFT JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
         LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
         LEFT JOIN locations_projects loc_proj ON loc.location_id = loc_proj.location_id
         LEFT JOIN projects proj ON loc_proj.project_id = proj.project_id
         LEFT JOIN locations_networks loc_net ON loc.location_id = loc_net.location_id
         LEFT JOIN networks net ON loc_net.network_id = net.network_id
         LEFT JOIN datum_conversions dc ON loc.location_id = dc.location_id AND dc.current = true
      GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type, params.param_name, params.unit_default, at.aggregation_type, lz.z_meters, dc.conversion_m;"
    )

    DBI::dbExecute(
      con,
      '
    CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
    WITH(security_invoker=true)
    AS SELECT ts.timeseries_id,
        loc.location_id,
        loc.name_fr AS nom_endroit,
        loc.alias AS nom_alias,
        lz.z_meters AS profondeur_hauteur_m,
        loc.latitude,
        loc.longitude,
        dc.conversion_m AS "élévation_endroit",
        array_agg(DISTINCT proj.name_fr) AS projets,
        array_agg(DISTINCT net.name_fr) AS "réseaux",
        mtypes.media_type_fr AS "type_de_média",
        params.param_name_fr AS "nom_paramètre",
        params.unit_default AS "unités",
        ag.aggregation_type_fr AS "type_agrégation",
        ts.record_rate AS "fréquence_enregistrement",
        ts.sensor_priority AS "priorité_capteur",
        ts.start_datetime AS "début",
        ts.end_datetime AS fin,
        ts.note
      FROM timeseries ts
        JOIN locations loc ON ts.location_id = loc.location_id
        LEFT JOIN parameters params ON ts.parameter_id = params.parameter_id
        LEFT JOIN media_types mtypes ON ts.media_id = mtypes.media_id
        LEFT JOIN aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id
        LEFT JOIN locations_z lz ON ts.z_id = lz.z_id
        LEFT JOIN locations_projects loc_proj ON loc.location_id = loc_proj.location_id
        LEFT JOIN projects proj ON loc_proj.project_id = proj.project_id
        LEFT JOIN locations_networks loc_net ON loc.location_id = loc_net.location_id
        LEFT JOIN networks net ON loc_net.network_id = net.network_id
        LEFT JOIN datum_conversions dc ON loc.location_id = dc.location_id AND dc.current = true
      GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type_fr, params.param_name_fr, params.unit_default, ag.aggregation_type_fr, lz.z_meters, dc.conversion_m;'
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '34' WHERE item = 'Last patch number';"
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

    message("Patch 34 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 34 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
