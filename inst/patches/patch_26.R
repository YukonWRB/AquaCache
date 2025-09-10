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
  
  # Find all columns 'share_with' in tables timeseries and images tables (text array) and change the entry for 'yg_reader' to 'yg_reader_group'
  DBI::dbExecute(con, "UPDATE timeseries SET share_with = array_replace(share_with, 'yg_reader', 'yg_reader_group') WHERE share_with IS NOT NULL AND 'yg_reader' = ANY(share_with);")
  DBI::dbExecute(con, "UPDATE images SET share_with = array_replace(share_with, 'yg_reader', 'yg_reader_group') WHERE share_with IS NOT NULL AND 'yg_reader' = ANY(share_with);")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '26' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  
  message("Patch 26 applied successfully.")
  
}, error = function(e) {
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  stop("Patch 26 failed and the DB has been rolled back to its earlier state. ", e$message)
})
