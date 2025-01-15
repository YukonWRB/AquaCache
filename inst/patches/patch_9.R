# Patch 9.
# Rejig users and prepare DB for use with timescaleDB compression

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 9. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  
  # Rename existing role
  DBI::dbExecute(con, 'ALTER ROLE ac_reader RENAME TO public_reader;')
  
  # Create new role
  if (nrow(DBI::dbGetQuery(con, "SELECT 1 FROM pg_roles WHERE rolname = 'yg'")) == 1) {
    message("'yg' role already exists. Skipping creation.")
  } else {
    DBI::dbExecute(con, 'CREATE ROLE yg;')
  }
  
  # Grant privileges
schemas <- c("public", "information", "spatial", "measurements_discrete", "instruments")
for (schema in schemas) {
  DBI::dbExecute(con, paste0("GRANT USAGE ON SCHEMA ", schema, " TO yg;"))
  DBI::dbExecute(con, paste0("GRANT SELECT ON ALL TABLES IN SCHEMA ", schema, " TO yg;"))
  DBI::dbExecute(con, paste0("GRANT SELECT ON ALL SEQUENCES IN SCHEMA ", schema, " TO yg;"))
  DBI::dbExecute(con, paste0("GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA ", schema, " TO yg;"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES IN SCHEMA ", schema, " GRANT SELECT ON TABLES TO yg;"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES IN SCHEMA ", schema, " GRANT SELECT ON SEQUENCES TO yg;"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES IN SCHEMA ", schema, " GRANT EXECUTE ON FUNCTIONS TO yg;"))
}

# Drop views measurements_continuous_corrected, measurements_continuous_hourly, measurements_continuous_corrected_hourly
DBI::dbExecute(con, "DROP VIEW measurements_continuous_corrected CASCADE;") 
DBI::dbExecute(con, "DROP VIEW measurements_continuous_hourly;")


# Convert numeric arrays to text arrays for tables that will keep this column
DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION convert_share_with(arr int[])
RETURNS text[] AS $$
BEGIN
    RETURN ARRAY(
        SELECT CASE
            WHEN x = 1 THEN 'public_reader'
            WHEN x = 2 THEN 'yg'
        END
        FROM unnest(arr) AS x
    );
END;
$$ LANGUAGE plpgsql IMMUTABLE;
")

tbl_names <- c("locations", "timeseries", "measurements_discrete", "documents", "images", "images_index")

# Drop columns 'share_with' from tables that won't keep them, and drop triggers that check share_with, and remove RLS as well
DBI::dbExecute(con, "ALTER TABLE measurements_continuous DROP COLUMN share_with CASCADE;") # Should cascade to the RLS policy
DBI::dbExecute(con, "DROP TRIGGER IF EXISTS validate_share_with_trigger_measurements_continuous ON measurements_continuous;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DROP COLUMN share_with CASCADE;") # Should cascade to the RLS policy
DBI::dbExecute(con, "DROP TRIGGER IF EXISTS validate_share_with_trigger_measurements_calculated_daily ON measurements_calculated_daily;")
DBI::dbExecute(con, "DROP TRIGGER IF EXISTS validate_share_with_trigger_calculated_daily ON measurements_calculated_daily;")
DBI::dbExecute(con, "ALTER TABLE measurements_continuous DISABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_continuous NO FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily DISABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_calculated_daily NO FORCE ROW LEVEL SECURITY;")


# Create a helper function to check if a user is in a group:
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION user_in_group(group_name text)
                    RETURNS boolean AS $$
                      SELECT pg_has_role(current_user, group_name, 'MEMBER');
                    $$ LANGUAGE sql STABLE;
")

for (i in tbl_names) {
  DBI::dbExecute(con, paste0("DROP TRIGGER IF EXISTS validate_share_with_trigger ON ", i, ";"))
  DBI::dbExecute(con, paste0("DROP TRIGGER IF EXISTS validate_share_with_trigger_", i, " ON ", i, ";"))
  DBI::dbExecute(con, paste0("DROP INDEX IF EXISTS ", i, "_share_with_gin_idx;"))
  policy_name <- DBI::dbGetQuery(con, paste0("SELECT policyname FROM pg_policies WHERE tablename = '", i, "';"))$policyname
  if (length(policy_name) > 0) {
    DBI::dbExecute(con, paste0("DROP POLICY ", policy_name, " ON ", i, ";"))
  }
  DBI::dbExecute(con, paste0(
    "ALTER TABLE ", i, 
    " ALTER COLUMN share_with TYPE text[] ",
    "USING convert_share_with(share_with);"
  ))
  DBI::dbExecute(con, paste0("CREATE INDEX ", i, "_share_with_gin_idx ON ", i, " USING GIN (share_with);"))
  DBI::dbExecute(con, paste0("CREATE POLICY rls ON ", i, "
                              FOR ALL
                              USING (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              )
                              WITH CHECK (
                                ('public_reader' = ANY(share_with))
                                OR EXISTS (
                                  SELECT 1
                                  FROM unnest(share_with) g
                                  WHERE user_in_group(g)
                                )
                              );
                 "))
}

DBI::dbExecute(con, "DROP FUNCTION convert_share_with(int[]);")

# Create validate_share_with trigger function
DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION public.validate_share_with()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM unnest(NEW.share_with) AS group_name
        WHERE NOT EXISTS (
            SELECT 1
            FROM pg_roles
            WHERE rolname = group_name
        )
    ) THEN
        RAISE EXCEPTION 'Invalid group_name in share_with array';
    END IF;
    IF 'public_reader' = ANY(NEW.share_with) AND array_length(NEW.share_with, 1) > 1 THEN
        RAISE EXCEPTION 'If public_reader is present in the share_with array, it must be the only value';
    END IF;
    RETURN NEW;
END;
$function$;
")

# These tables also have a default of 1 in the share_with column. This default value needs to become 'public_reader' instead.
for (i in tbl_names) {
  DBI::dbExecute(con, paste0("ALTER TABLE ", i, " ALTER COLUMN share_with SET DEFAULT '{public_reader}';"))
}

# Add triggers
for (i in tbl_names) {
  DBI::dbExecute(con, paste0("CREATE TRIGGER validate_share_with_trigger
BEFORE INSERT OR UPDATE ON ", i, "
FOR EACH ROW
EXECUTE FUNCTION public.validate_share_with();"))
}


# Create an index on corrections to speed things up
DBI::dbExecute(con, "CREATE INDEX idx_corrections_timeseries_date_range
ON corrections (timeseries_id, start_dt, end_dt);
")


# Prepare DB for use with timescaleDB, if so desired later.
# Instead of using RLS on these tables, they will be accessible to the 'public' user via views and the RLS columns and policies will be removed completely.

# Create new views
# View measurements_continuous_corrected with RLS enforcement and speed optimization
DBI::dbExecute(con, "CREATE OR REPLACE VIEW public.measurements_continuous_corrected AS
                      SELECT mc.timeseries_id,
                             mc.datetime,
                             mc.value AS value_raw,
                             COALESCE(ac.value_corrected, mc.value) AS value_corrected
                             mc.period,
                             mc.imputed
                      FROM measurements_continuous mc
                      JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id
                      LEFT JOIN LATERAL (
                        SELECT apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
                        WHERE EXISTS (
                            SELECT 1 FROM corrections c
                            WHERE c.timeseries_id = mc.timeseries_id
                              AND c.start_dt <= mc.datetime
                              AND c.end_dt >= mc.datetime
                        )
                      ) ac ON TRUE
                      WHERE (
                          'public_reader' = ANY (ts.share_with)
                          OR EXISTS (
                              SELECT 1
                              FROM unnest(ts.share_with) role(role)
                              WHERE pg_has_role(CURRENT_USER, role.role::name, 'MEMBER')
                                AND role.role::name IN (SELECT rolname FROM pg_roles)
                          )
                      )
;")

# View measurements_hourly_corrected with RLS enforcement
DBI::dbExecute(con, "CREATE OR REPLACE VIEW public.measurements_hourly_corrected AS
SELECT 
    mcc.timeseries_id,
    date_trunc('hour', mcc.datetime) AS datetime,
    avg(mcc.value_raw) AS value_raw,
    avg(mcc.value_corrected) AS value_corrected,
    bool_or(mcc.imputed) AS imputed
FROM 
    measurements_continuous_corrected mcc
GROUP BY 
    mcc.timeseries_id, date_trunc('hour', mcc.datetime)
ORDER BY 
    date_trunc('hour', mcc.datetime);")

# View measurements_calculated_daily_corrected with RLS enforcement
DBI::dbExecute(con, "CREATE OR REPLACE VIEW public.measurements_calculated_daily_corrected AS
SELECT 
    mcd.timeseries_id,
    mcd.date,
    mcd.value,
    mcd.imputed,
    mcd.percent_historic_range,
    mcd.max,
    mcd.min,
    mcd.q90,
    mcd.q75,
    mcd.q50,
    mcd.q25,
    mcd.q10,
    mcd.mean,
    mcd.doy_count
FROM 
    measurements_calculated_daily mcd
JOIN 
    timeseries ts ON mcd.timeseries_id = ts.timeseries_id
WHERE 
    (
        'public_reader' = ANY(ts.share_with)
        OR EXISTS (
           SELECT 1
           FROM unnest(ts.share_with) role(role)
           WHERE pg_has_role(CURRENT_USER, role.role::name, 'MEMBER')
           AND role.role::name IN (SELECT rolname FROM pg_roles)
      )
    )
;")


# Now make a function/trigger to warn future admins that granting privileges on the tables now protected using RLS is discouraged, and state why.
# This is not working as expected, so it's commented out for now.
# DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION warn_on_public_grant()
# RETURNS event_trigger
# LANGUAGE plpgsql
# AS $$
#   DECLARE
# obj_name text;
# BEGIN
# -- Extract the table being granted privileges
# SELECT objid::regclass::text INTO obj_name
# FROM pg_event_trigger_ddl_commands()
# WHERE command_tag = 'GRANT';
# 
# -- Check if the table is sensitive
# IF obj_name IN ('measurements_continuous', 'measurements_calculated_daily') THEN
# RAISE WARNING 'Direct GRANT SELECT on % is discouraged. RLS is not enabled on these tables so as to enable use of timescaleDB compression feature. Use views to enforce RLS.', obj_name;
# END IF;
# END;
# $$;
# ")
# 
# DBI::dbExecute(con, "CREATE EVENT TRIGGER warn_on_public_grant
# ON ddl_command_start
# WHEN TAG IN ('GRANT')
# EXECUTE FUNCTION warn_on_public_grant();")

# Revoke the 'public_reader' user's ability to view tables measurements_continuous and measurements_calculated_daily
DBI::dbExecute(con, "REVOKE ALL ON measurements_continuous FROM public_reader;")
DBI::dbExecute(con, "REVOKE ALL ON measurements_calculated_daily FROM public_reader;")

# Drop the tables 'users' and user_groups
DBI::dbExecute(con, "DROP TABLE users")
DBI::dbExecute(con, "DROP TABLE user_groups")

# Update/modify function enforce_share_with_restriction that is used to check table 'images' for share_with values congruent with table images_index
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION public.enforce_share_with_restriction()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
              BEGIN
                  -- Skip check if img_meta_id is NULL
                  IF NEW.img_meta_id IS NULL THEN
                      RETURN NEW;
                  END IF;
                  
                  -- Check if images_index.share_with is NOT '{public_reader}'
                  IF NOT ('public_reader' = ANY(NEW.share_with)) THEN
                      -- Retrieve the corresponding share_with from images_index
                      PERFORM 1
                      FROM images_index
                      WHERE img_meta_id = NEW.img_meta_id
                        AND NOT ('public_reader' = ANY(share_with));
                        
                      -- If images_index.share_with is NOT {'public_reader'}, raise an exception
                      IF FOUND THEN
                          RAISE EXCEPTION 'images_index entry for img_meta_id % has a restrictive share_with, images.share_with cannot be {public_reader}', NEW.img_meta_id;
                      END IF;
                  END IF;
                  RETURN NEW;
              END;
              $function$
;")


# Now it should be possible for the admin to install timescaleDB and enable it with compression


# Create helper function so that all app users can see all roles which can read the 'locations' table, allowing them to set the share_with field when creating new fields
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION get_roles_with_select_on_locations()
RETURNS TABLE(role_name text) AS $$
BEGIN
  RETURN QUERY
  SELECT grantee::text
  FROM information_schema.role_table_grants
  WHERE table_name = 'locations'
    AND privilege_type = 'SELECT'
    AND grantee::text NOT IN (
      SELECT rolname
      FROM pg_roles
      WHERE rolbypassrls = TRUE
    );
END;
$$ LANGUAGE plpgsql SECURITY DEFINER;

")

DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION get_roles_with_select_on_locations() TO public;")


# Update the version_info table
DBI::dbExecute(con, "UPDATE information.version_info SET version = '9' WHERE item = 'Last patch number';")
DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))

# Commit the transaction
DBI::dbExecute(con, "COMMIT;")
attr(con, "active_transaction") <- FALSE


message("Patch 9 applied successfully: rejigged users and prepared DB for use with timescaleDB. \n  \n  Now you can set up timescaleDB and enable compression on tables 'measurements_continuous' and 'measurements_calculated_daily' if you wish, and view performance is improved regardless.")

}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 9 failed and the DB has been rolled back to its earlier state. ", e$message)
})
