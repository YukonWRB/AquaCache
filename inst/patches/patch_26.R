# Patch 26

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 26. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop("A transaction is already in progress. Please commit or rollback the current transaction before applying this patch.")
}
active <- dbTransBegin(con)

tryCatch({
  
  message("Creating infrastructure to handle water quality guideline values...")
  
  # Add a function in discrete schema to calculate hardness, since this is a recurring need for guidelines
  DBI::dbExecute(con, "
                CREATE OR REPLACE FUNCTION discrete.get_sample_hardness(in_sample_id INTEGER)
                RETURNS NUMERIC
                LANGUAGE sql
                STABLE
                SET search_path = discrete, public
                AS $$
                WITH vals AS (
                  SELECT
                    MAX(result) FILTER (WHERE parameter_id = 1061 AND sample_fraction_id = 5)  AS ca_d,
                    MAX(result) FILTER (WHERE parameter_id = 1103 AND sample_fraction_id = 5)  AS mg_d,
                    MAX(result) FILTER (WHERE parameter_id = 100  AND sample_fraction_id = 5  AND result_speciation_id = 3) AS hard_d,
                    MAX(result) FILTER (WHERE parameter_id = 1061 AND sample_fraction_id = 19) AS ca_t,
                    MAX(result) FILTER (WHERE parameter_id = 1103 AND sample_fraction_id = 19) AS mg_t,
                    MAX(result) FILTER (WHERE parameter_id = 100  AND sample_fraction_id = 19 AND result_speciation_id = 3) AS hard_t
                  FROM discrete.results
                  WHERE sample_id = $1   -- use $1 placeholder, not in_sample_id
                )
                SELECT CASE
                  WHEN ca_d IS NOT NULL AND mg_d IS NOT NULL AND ca_d > 0 AND mg_d > 0 
                    THEN 2.497*ca_d + 4.118*mg_d
                  WHEN hard_d IS NOT NULL AND hard_d > 0 
                    THEN hard_d
                  WHEN ca_t IS NOT NULL AND mg_t IS NOT NULL AND ca_t > 0 AND mg_t > 0 
                    THEN 2.497*ca_t + 4.118*mg_t
                  WHEN hard_t IS NOT NULL AND hard_t > 0 
                    THEN hard_t
                END
                FROM vals;
                $$;
")
  DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION discrete.get_sample_hardness(int) TO PUBLIC;")
  
  DBI::dbExecute(con, "
                 CREATE OR REPLACE FUNCTION discrete.get_sample_val(
                  sample_id int,
                  parameter_id int,
                  sample_fraction_id INT DEFAULT NULL,
                  result_speciation_id INT DEFAULT NULL
                ) RETURNS numeric
                LANGUAGE sql 
                STABLE 
                SET search_path = discrete, public AS $$
                SELECT MAX(result)::numeric
                FROM discrete.results
                WHERE sample_id = $1
                  AND parameter_id = $2
                  AND sample_fraction_id IS NOT DISTINCT FROM $3 -- allows match on NULL
                  AND result_speciation_id IS NOT DISTINCT FROM $4; -- allows match on NULL
                $$;
")
  DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION discrete.get_sample_val(int, int, int, int) TO PUBLIC;")
  
  
  # Remove the visibility_public column from several tables as it is no longer needed
  DBI::dbExecute(con, "ALTER TABLE image_series DROP COLUMN IF EXISTS visibility_public;")
  DBI::dbExecute(con, "ALTER TABLE locations DROP COLUMN IF EXISTS visibility_public;")
  
  # Find all columns 'share_with' in tables timeseries, locations, and images tables (text array) and change the entry for 'yg_reader' to 'yg_reader_group'
  DBI::dbExecute(con, "UPDATE timeseries SET share_with = array_replace(share_with, 'yg_reader', 'yg_reader_group') WHERE share_with IS NOT NULL AND 'yg_reader' = ANY(share_with);")
  DBI::dbExecute(con, "UPDATE images SET share_with = array_replace(share_with, 'yg_reader', 'yg_reader_group') WHERE share_with IS NOT NULL AND 'yg_reader' = ANY(share_with);")
  DBI::dbExecute(con, "UPDATE locations SET share_with = array_replace(share_with, 'yg_reader', 'yg_reader_group') WHERE share_with IS NOT NULL AND 'yg_reader' = ANY(share_with);")
  
  
  # Rename columns img_meta_id to img_series_id in images and image_series tables
  DBI::dbExecute(con, "ALTER TABLE images RENAME COLUMN img_meta_id TO img_series_id;")
  DBI::dbExecute(con, "ALTER TABLE image_series RENAME COLUMN img_meta_id TO img_series_id;")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '26' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  DBI::dbExecute(con, "
  CREATE OR REPLACE FUNCTION files.enforce_share_with_restriction()
  RETURNS trigger
  LANGUAGE plpgsql
  AS $function$
    BEGIN
  -- Skip check if img_series_id is NULL
  IF NEW.img_series_id IS NULL THEN
  RETURN NEW;
  END IF;
  
  -- Check if images_index.share_with is NOT '{public_reader}'
  IF NOT ('public_reader' = ANY(NEW.share_with)) THEN
  -- Retrieve the corresponding share_with from images_index
  PERFORM 1
  FROM images_index
  WHERE img_series_id = NEW.img_series_id
  AND NOT ('public_reader' = ANY(share_with));
  
  -- If images_index.share_with is NOT {'public_reader'}, raise an exception
  IF FOUND THEN
  RAISE EXCEPTION 'images_index entry for img_series_id % has a restrictive share_with, images.share_with cannot be {public_reader}', NEW.img_series_id;
  END IF;
  END IF;
  RETURN NEW;
  END;
  $function$
    ;
  ")
  
  
  # Add missing triggers to enforce share_with restrictions on boreholes schema tables
  DBI::dbExecute(con, "
  create or replace trigger validate_share_with_trigger before
  insert
  or
  update
  on
  boreholes.boreholes for each row execute function validate_share_with()
  ")
  DBI::dbExecute(con, "
  create or replace trigger validate_share_with_trigger before
  insert
  or
  update
  on
  boreholes.wells for each row execute function validate_share_with()
  ")
  
  
  # Updaet get_sharable_principals_for so that pg_roles don't show up
  DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION public.get_shareable_principals_for(
  _rel regclass,
  _privs text[] DEFAULT ARRAY['SELECT'::text],
  _always_include text[] DEFAULT ARRAY['public_reader'::text, 'admin'::text]
)
RETURNS TABLE(role_name text)
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path TO 'pg_temp','pg_catalog'
AS $function$
  SELECT x.role_name
  FROM (
    SELECT r.rolname AS role_name
    FROM pg_roles r
    WHERE r.rolcanlogin = false
      AND r.rolname <> 'public'
      AND r.rolname <> 'pg_read_all_data'
      AND r.rolname NOT LIKE 'pg\\_%'      -- hide system roles
      AND r.rolname NOT LIKE 'rds\\_%'     -- (optional) hide AWS RDS roles
      AND EXISTS (
        SELECT 1 FROM unnest(_privs) p
        WHERE has_table_privilege(r.oid, _rel, p)
      )

    UNION

    SELECT r.rolname
    FROM pg_roles r
    WHERE r.rolname = ANY(_always_include)
  ) AS x
  ORDER BY
    CASE WHEN x.role_name = ANY(_always_include) THEN 0 ELSE 1 END,
    x.role_name;
$function$;
")
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  
  message("Patch 26 applied successfully.")
  
}, error = function(e) {
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  stop("Patch 26 failed and the DB has been rolled back to its earlier state. ", e$message)
})
