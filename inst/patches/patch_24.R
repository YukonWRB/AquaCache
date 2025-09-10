# Patch 24

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 24. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop("A transaction is already in progress. Please commit or rollback the current transaction before applying this patch.")
}
active <- dbTransBegin(con)

tryCatch({
  
  # Better way to get current user roles in Postgres for RLS (will replace function user_in_group())
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION public.current_user_roles()
      RETURNS text[]
      LANGUAGE sql
      STABLE
      SECURITY DEFINER
      SET search_path = pg_temp, pg_catalog AS
      $$
        SELECT coalesce(array_agg(r.rolname), ARRAY[]::text[])
      FROM pg_roles r
      WHERE pg_has_role(current_user, r.oid, 'member');
      $$;
      ")
  
  DBI::dbExecute(con, "REVOKE ALL ON FUNCTION current_user_roles() FROM PUBLIC;")
  DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION current_user_roles() TO PUBLIC;")
  
  
  # Find all tables across all schemas that have a column named 'share_with':
  tbls <- DBI::dbGetQuery(con, "
SELECT t.table_schema, t.table_name
FROM information_schema.tables t
JOIN information_schema.columns c
  ON c.table_schema=t.table_schema AND c.table_name=t.table_name
WHERE t.table_type='BASE TABLE'
  AND c.column_name='share_with'
  AND t.table_schema NOT IN ('pg_catalog','information_schema')
ORDER BY 1,2;
")
  
  # Now iterate over each table and add the new RLS policy:
  for (i in seq_len(nrow(tbls))) {
    sch <- tbls$table_schema[i]
    tab <- tbls$table_name[i]
    qual <- paste0(DBI::dbQuoteIdentifier(con, sch), ".", DBI::dbQuoteIdentifier(con, tab))
    
    # Enable RLS
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ENABLE ROW LEVEL SECURITY;", qual))
    
    # Drop existing SELECT policies on this table (explicitly)
    pols <- DBI::dbGetQuery(con, paste0("SELECT * FROM pg_policies WHERE schemaname = '", sch, "' AND tablename = '", tab, "'"))$policyname
    for (p in pols) {
      DBI::dbExecute(con, sprintf("DROP POLICY %s ON %s;", DBI::dbQuoteIdentifier(con, p), qual))
    }
    
    # Create the new SELECT policy
    DBI::dbExecute(con, sprintf("
    CREATE POLICY rls ON %s
    USING (
      share_with @> ARRAY['public_reader']
      OR share_with && public.current_user_roles()
    );", qual))
  }
  
  
  # Now deal with the views that reference these tables.
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW continuous.measurements_calculated_daily_corrected
WITH (security_barrier = true) AS
SELECT mcd.timeseries_id,
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
FROM measurements_calculated_daily mcd
JOIN timeseries ts USING (timeseries_id);  -- RLS on timeseries filters visibility even if not selected
")
  
  DBI::dbExecute(con, "CREATE OR REPLACE VIEW continuous.measurements_continuous_corrected
WITH (security_barrier = true)
AS
SELECT
  mc.timeseries_id,
  mc.datetime,
  mc.value AS value_raw,
  COALESCE(ac.value_corrected, mc.value) AS value_corrected,
  mc.period,
  mc.imputed
FROM measurements_continuous mc
JOIN timeseries ts USING (timeseries_id)   -- RLS on timeseries filters visibility even if not selected
LEFT JOIN LATERAL (
  -- Only runs when a window exists; LIMIT 1 avoids extra work
  SELECT continuous.apply_corrections(mc.timeseries_id, mc.datetime, mc.value) AS value_corrected
  FROM corrections c
  WHERE c.timeseries_id = mc.timeseries_id
    AND mc.datetime <@ tstzrange(c.start_dt, c.end_dt)
  LIMIT 1
) ac ON true;
")
  
  
  
  # Create new security definer function to check which groups have permissions on any table
  DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION public.get_shareable_principals_for(
  _rel            regclass,                        -- e.g. 'public.locations'
  _privs          text[]  DEFAULT ARRAY['SELECT'], -- SELECT/INSERT/UPDATE/DELETE/...
  _always_include text[]  DEFAULT ARRAY['public_reader', 'admin']
)
RETURNS TABLE(role_name text)
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = pg_temp, pg_catalog AS
$$
  SELECT x.role_name
  FROM (
    -- NOLOGIN groups that have ANY of the requested privileges on _rel
    SELECT r.rolname AS role_name
    FROM pg_roles r
    WHERE r.rolcanlogin = false
      AND r.rolname <> 'public'
      AND r.rolname <> 'pg_read_all_data'
      AND EXISTS (
        SELECT 1 FROM unnest(_privs) p
        WHERE has_table_privilege(r.oid, _rel, p)
      )

    UNION

    -- Always-include roles (e.g., sentinel 'public_reader'), if they exist
    SELECT r.rolname AS role_name
    FROM pg_roles r
    WHERE r.rolname = ANY(_always_include)
  ) AS x
  ORDER BY
    CASE WHEN x.role_name = ANY(_always_include) THEN 0 ELSE 1 END,
    x.role_name;
$$;
")

DBI::dbExecute(con, "REVOKE ALL ON FUNCTION public.get_shareable_principals_for(regclass, text[], text[]) FROM PUBLIC;")
DBI::dbExecute(con, "GRANT EXECUTE ON FUNCTION public.get_shareable_principals_for(regclass, text[], text[]) TO PUBLIC;")

# Delete the old function:
DBI::dbExecute(con, "DROP FUNCTION IF EXISTS public.get_roles_with_select_on_locations();")



# Replace the function that ensures share_with always has valid roles with one that ensures that only groups or share_with are valid:
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION public.validate_share_with()
RETURNS trigger
LANGUAGE plpgsql
AS $$
  DECLARE
bad text;
BEGIN
-- Canonicalize: de-duplicate & sort (optional but handy)
NEW.share_with :=
  (SELECT array_agg(DISTINCT g ORDER BY g)
   FROM unnest(NEW.share_with) AS u(g));

-- 'public_reader' must be the only value if present
IF 'public_reader' = ANY(NEW.share_with)
AND array_length(NEW.share_with, 1) > 1
THEN
RAISE EXCEPTION 'If public_reader is present, it must be the only value';
END IF;

-- Find any entry that is NOT an allowed group or 'public_reader'
SELECT g
INTO bad
FROM unnest(NEW.share_with) AS u(g)
WHERE NOT EXISTS (
  SELECT 1
  FROM pg_roles r
  WHERE r.rolname = g
  AND (
    r.rolname = 'public_reader'           -- sentinel
    OR (r.rolcanlogin = false             -- groups only
        AND r.rolname <> 'public'         -- exclude pseudo-role
        AND r.rolname !~ '^pg_')   -- exclude built-ins
  )
)
LIMIT 1;

IF FOUND THEN
RAISE EXCEPTION 'Invalid value in share_with: % (allowed: group roles or public_reader)', bad;
END IF;

RETURN NEW;
END;
$$;
")

# Now adjust the roles if they exist
yg_reader_exists <- DBI::dbGetQuery(con, "SELECT 1 FROM pg_roles WHERE rolname = 'yg_reader';")

if (nrow(yg_reader_exists) > 0) {
  # yg_reader should be nobypassrls as that's not inherited by users anyways.
  DBI::dbExecute(con, "ALTER ROLE yg_reader NOBYPASSRLS;")
  # rename yg_reader to yg_reader_group
  DBI::dbExecute(con, "ALTER ROLE yg_reader RENAME TO yg_reader_group;")
  
  # grant select on schema boreholes and all tables to yg_reader_group
  DBI::dbExecute(con, "GRANT USAGE ON SCHEMA boreholes TO yg_reader_group;")
  DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA boreholes TO yg_reader_group;")
}

# delete continuous_editor and discrete_editor roles
roles <- c("continuous_editor", "discrete_editor")
for (i in roles) {
  
  DBI::dbExecute(con, paste0("REVOKE ALL ON SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ", i, ";"))
  DBI::dbExecute(con, paste0("REVOKE ALL ON ALL TABLES IN SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ", i, ";"))
  DBI::dbExecute(con, paste0("REVOKE ALL ON ALL SEQUENCES IN SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ", i, ";"))
  DBI::dbExecute(con, paste0("REVOKE ALL ON ALL FUNCTIONS IN SCHEMA public, continuous, discrete, spatial, files, instruments, information FROM ", i, ";"))
  
  # Revoke default privileges for i
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous REVOKE ALL ON TABLES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete REVOKE ALL ON TABLES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial REVOKE ALL ON TABLES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files REVOKE ALL ON TABLES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments REVOKE ALL ON TABLES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information REVOKE ALL ON TABLES FROM ", i, ";"))
  
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA continuous REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA discrete REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA spatial REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA files REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA instruments REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  DBI::dbExecute(con, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA information REVOKE ALL ON SEQUENCES FROM ", i, ";"))
  
  # Reassign ownership
  DBI::dbExecute(con, paste0("REASSIGN OWNED BY ", i, " TO postgres;"))
  
  # Drop all privileges
  DBI::dbExecute(con, paste0("DROP OWNED BY ", i, ";"))
  
  # Terminate active connections (if necessary)
  DBI::dbExecute(con, paste0("
SELECT pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.usename = '", i, "'
  AND pid <> pg_backend_pid();
"))
  
  # connect to snow database and drop privileges on public.circuits table
  try({
    dets <-  DBI::dbGetQuery(con, "SELECT inet_server_addr() AS ip, inet_server_port() AS port")
    snowcon <- snowConnect(username = "postgres", host = as.character(dets$ip), port = as.numeric(dets$port), silent = TRUE)
    
    DBI::dbExecute(snowcon, paste0("REVOKE ALL ON TABLE public.circuits FROM ", i, ";"))
    DBI::dbExecute(snowcon, paste0("ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES FROM ", i, ";"))
    DBI::dbExecute(snowcon, paste0("REASSIGN OWNED BY ", i, " TO postgres;"))
    DBI::dbExecute(snowcon, paste0("DROP OWNED BY ", i, ";"))
    
    DBI::dbDisconnect(snowcon)
  })
  
  # Drop the role
  DBI::dbExecute(con, paste0("DROP ROLE ", i, ";"))
}


# Create a new group called 'yg_editor' for yg staff
DBI::dbExecute(con, "
  CREATE ROLE yg_editor_group
    NOLOGIN
    NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION;
")

# Give yg_editor_group edit select privileges on all schemas
# Find the schemas first (but not those starting with pg_ or information_schema)
schemas <- DBI::dbGetQuery(con, "
SELECT schema_name
FROM information_schema.schemata 
WHERE schema_name NOT LIKE 'pg_%' 
  AND schema_name <> 'information_schema'
  AND schema_name <> 'application'
  AND schema_name <> 'information';")

# Now iterate over each schema and grant privileges to yg_editor_group
for (i in seq_len(nrow(schemas))) {
  schema <- schemas$schema_name[i]
  sql <- sprintf("GRANT USAGE ON SCHEMA %s TO yg_editor_group;", DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, sql)
  
  sql <- sprintf("GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA %s TO yg_editor_group;", DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, sql)
  
  sql <- sprintf("ALTER DEFAULT PRIVILEGES IN SCHEMA %s GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO yg_editor_group;", DBI::dbQuoteIdentifier(con, schema))
  DBI::dbExecute(con, sql)
}


# Update the version_info table
DBI::dbExecute(con, "UPDATE information.version_info SET version = '24' WHERE item = 'Last patch number';")
DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))


# Commit the transaction
DBI::dbExecute(con, "COMMIT;")

message("Patch 24 applied successfully.")

}, error = function(e) {
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  stop("Patch 24 failed and the DB has been rolled back to its earlier state. ", e$message)
})
