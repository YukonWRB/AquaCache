# Patch 10
# Properly relocate postgis extension

# Initial checks #################
# Ensure the user is postgres OR admin as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user %in% c("postgres", "admin")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work."
  )
}

message(
  "Working on Patch 10. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    # Some postgis tables had previously been moved to 'spatial', but not the rest of the extension. Move these again to be with the rest of the extension, then move the extension and all associated tables back to 'spatial'. Hopefully this cures the database restore issues.

    # Step 1: Move spatial tables: spatial_ref_sys, rasters, rasters_reference, vectors
    DBI::dbExecute(con, "ALTER TABLE spatial_ref_sys SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE rasters SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE rasters_reference SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE raster_series_index SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE vectors SET SCHEMA public;")
    # And the views 'geography_columns', 'geometry_columns', 'raster_columns', 'raster_overviews'
    DBI::dbExecute(con, "ALTER TABLE geography_columns SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE geometry_columns SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE raster_columns SET SCHEMA public;")
    DBI::dbExecute(con, "ALTER TABLE raster_overviews SET SCHEMA public;")

    # Step 2: Make the PostGIS extension relocatable
    DBI::dbExecute(
      con,
      "UPDATE pg_extension SET extrelocatable = TRUE WHERE extname = 'postgis';"
    )

    # Step 3: Relocate the PostGIS extension
    DBI::dbExecute(con, "ALTER EXTENSION postgis SET SCHEMA spatial")

    # Step 4: Relocate the PostGIS raster extension if installed
    raster_installed <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) FROM pg_extension WHERE extname = 'postgis_raster';"
    )
    if (raster_installed[1, 1] > 0) {
      DBI::dbExecute(
        con,
        "UPDATE pg_extension SET extrelocatable = TRUE WHERE extname = 'postgis_raster';"
      )
      DBI::dbExecute(con, "ALTER EXTENSION postgis_raster SET SCHEMA spatial")
    }

    # Step 5: Perform a controlled upgrade
    DBI::dbExecute(con, "ALTER EXTENSION postgis UPDATE TO 'ANY';")
    DBI::dbExecute(con, "ALTER EXTENSION postgis_raster UPDATE TO 'ANY';")

    # Step 6: Verify relocation
    result <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT extname, nspname AS schema_name FROM pg_extension
       JOIN pg_namespace ON pg_extension.extnamespace = pg_namespace.oid
       WHERE extname IN ('postgis', 'postgis_raster');"
      )
    )

    if (all(result$schema_name == "spatial")) {
      DBI::dbExecute(con, "ROLLBACK;")
      stop("Looks like the move wasn't successful, rolling back.")
    }

    # Step 7: Force update so that everything is a-ok
    DBI::dbExecute(con, "ALTER EXTENSION postgis UPDATE;")

    DBI::dbExecute(con, "ALTER TABLE rasters SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE rasters_reference SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE raster_series_index SET SCHEMA spatial;")
    DBI::dbExecute(con, "ALTER TABLE vectors SET SCHEMA spatial;")

    # Step 8: Make the PostGIS extension non-relocatable again
    DBI::dbExecute(
      con,
      "UPDATE pg_extension SET extrelocatable = FALSE WHERE extname = 'postgis';"
    )

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '10' WHERE item = 'Last patch number';"
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
      "Patch 10 applied successfully: fixed postgis relocation to spatial schema."
    )
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 10 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
