# Patch 30

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 30. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # modify sample_series fkeys so that they cascade on delete and update
    message(
      "Modifying foreign key constraints on discrete.sample_series to cascade on delete and update."
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_sub_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_sub_location_id_fkey FOREIGN KEY (sub_location_id) REFERENCES public.sub_locations(sub_location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_default_owner_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_default_owner_fkey FOREIGN KEY (default_owner) REFERENCES public.organizations(organization_id) ON DELETE SET NULL ON UPDATE SET NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series DROP CONSTRAINT sample_series_default_contributor_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series ADD CONSTRAINT sample_series_default_contributor_fkey FOREIGN KEY (default_contributor) REFERENCES public.organizations(organization_id) ON DELETE SET NULL ON UPDATE SET NULL;"
    )

    # Do same for images
    message(
      "Modifying foreign key constraints on files.images to cascade on delete and update."
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images DROP CONSTRAINT images_image_type_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images ADD CONSTRAINT images_image_type_fkey FOREIGN KEY (image_type) REFERENCES files.image_types(image_type_id) ON DELETE SET NULL ON UPDATE SET NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images DROP CONSTRAINT images_location_id_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.images ADD CONSTRAINT images_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.locations(location_id) ON DELETE CASCADE ON UPDATE CASCADE;"
    )

    # Drop a few unused function and triggers
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS documents_spatial_after_delete ON files.documents_spatial;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS documents_spatial_after_insert ON files.documents_spatial;"
    )

    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_document_flags_after_insert();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_document_flags_after_delete();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_line_flag();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_location_flag();"
    )
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS files.update_polygon_flag();"
    )

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '30' WHERE item = 'Last patch number';"
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

    message("Patch 30 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 30 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
