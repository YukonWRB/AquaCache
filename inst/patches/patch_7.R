# Patch 7.
# Fix function remove_group_id_from_share_with

# Initial checks #################
# Ensure the user is admin or postgres as this patch creates new users
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (!check$session_user %in% c("postgres")) {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 7. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE

tryCatch({
  
  DBI::dbExecute(con, "
                 CREATE OR REPLACE FUNCTION public.remove_group_id_from_share_with()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
BEGIN
    -- Update the share_with array to remove the deleted group_id for each table
    EXECUTE 'UPDATE locations SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE timeseries SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_continuous SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_discrete SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_calculated_daily SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE images SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE images_index SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE documents SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    RETURN OLD;
END;
$function$
;
")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '7' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  
  message("Patch 7 applied successfully: functions referencing old table calculated_daily have been removed.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 7 failed and the DB has been rolled back to its earlier state. ", e$message)
})

