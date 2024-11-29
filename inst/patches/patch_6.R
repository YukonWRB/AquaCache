# Patch 6.
# Creates new table 'network_types' ....

# Initial checks #################
# Ensure the user is postgres as this patch creates new users
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 6. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Create new users so that postgres stops being used for everything. Read-only user is hydromet_read, change that to ac_reader.

  # Admin user creation script
  message("Creating new users so that postgres stops being used. ENTER YOUR DESIRED PASSWORD BELOW.")
  
  role_exists <- DBI::dbGetQuery(con, "SELECT 1 FROM pg_roles WHERE rolname = 'admin';")
  
  # 1. Create the admin role
  if (nrow(role_exists) == 0) {
    password <- readline("Password for the admin user: ")
    DBI::dbExecute(con, paste0("CREATE ROLE admin WITH LOGIN PASSWORD '", password, "';"))
  }

  
  # 2. Grant the admin role privileges to connect to the database
  current_db <- DBI::dbGetQuery(con, "SELECT current_database();")[[1]]
  DBI::dbExecute(con, paste0("GRANT CONNECT ON DATABASE ", DBI::dbQuoteIdentifier(con, current_db), " TO admin;"))

  # 3. Grant the admin role privileges to manage schemas
  # Existing schemas
  schemas <- DBI::dbGetQuery(con, "
      SELECT schema_name
      FROM information_schema.schemata
      WHERE schema_name NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (schema_name in schemas$schema_name) {
    DBI::dbExecute(con, paste0("GRANT USAGE, CREATE ON SCHEMA ", DBI::dbQuoteIdentifier(con, schema_name), " TO admin;"))
  }
  
  # Future schemas (default privileges)
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT USAGE, CREATE ON SCHEMAS TO admin;")
  
  # 4. Grant privileges for tables, sequences, and functions
  
  # Existing objects
  tables <- DBI::dbGetQuery(con, "
      SELECT schemaname, tablename
      FROM pg_tables
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(tables))) {
    schema <- tables$schemaname[row]
    table <- tables$tablename[row]
    DBI::dbExecute(con, paste0("GRANT ALL PRIVILEGES ON TABLE ", DBI::dbQuoteIdentifier(con, schema), ".", DBI::dbQuoteIdentifier(con, table), " TO admin;"))
  }
  
  sequences <- DBI::dbGetQuery(con, "
      SELECT schemaname, sequencename
      FROM pg_sequences
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(sequences))) {
    schema <- sequences$schemaname[row]
    sequence <- sequences$sequencename[row]
    DBI::dbExecute(con, paste0("GRANT ALL PRIVILEGES ON SEQUENCE ", DBI::dbQuoteIdentifier(con, schema), ".", DBI::dbQuoteIdentifier(con, sequence), " TO admin;"))
  }
  
  # Future objects (default privileges)
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON TABLES TO admin;")
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON SEQUENCES TO admin;")
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT EXECUTE ON FUNCTIONS TO admin;")
  
  # 5. Grant role management privileges (optional)
  # Allows admin to manage roles but not create/drop databases
  DBI::dbExecute(con, "ALTER ROLE admin CREATEROLE;")
  
  # 6. Remove database creation privilege from admin
  DBI::dbExecute(con, "ALTER ROLE admin NOCREATEDB;")
  
  # 7. Bypass RLS for this user
  DBI::dbExecute(con, "ALTER ROLE admin BYPASSRLS;")
  
  message("Admin user setup completed for database ", DBI::dbGetQuery(con, "SELECT current_database();")[[1]])

  # Modify the user 'hydromet_read' to 'ac_reader'
  message("Modifying 'hydromet_read' user to 'ac_reader'...")
  DBI::dbExecute(con, "ALTER ROLE hydromet_read RENAME TO ac_reader;")
  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '6' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  
  message("Patch 6 applied successfully: created new users. Now stop using postgres for everything, switch your 'postgres' entries in the .Renviron file to 'admin'!")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 6 failed and the DB has been rolled back to its earlier state. ", e$message)
})


# Now apply the changes to the snow database if it exists

# Check if the snow database exists
snow <- DBI::dbGetQuery(con, "SELECT datname FROM pg_database WHERE datname = 'snow';")

if (nrow(snow) == 0) {
  warning("No 'snow' database found. Skipping updates.")
  return(invisible())
}

# Begin a transaction
message("SnowDB exists, making changes to it...")
tryCatch({
  message("Now attempting to make 'admin' user get the same privileges on the snow database as the aquacache, if it exists..")
  host <- DBI::dbGetQuery(con, "SELECT inet_server_addr() AS host_address, inet_server_port() AS port;")
  snowCon <- snowConnect(username = "postgres", host = host$host_address, port = host$port, silent = TRUE)
  
  message("Starting transaction...")
  DBI::dbExecute(snowCon, "BEGIN;")
  attr(snowCon, "active_transaction") <- TRUE
  
  on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
  
  # 1. Grant the admin role privileges to connect to the database
  current_db <- DBI::dbGetQuery(snowCon, "SELECT current_database();")[[1]]
  DBI::dbExecute(snowCon, paste0("GRANT CONNECT ON DATABASE ", DBI::dbQuoteIdentifier(snowCon, current_db), " TO admin;"))
  
  # 2. Grant the admin role privileges to manage schemas
  # Existing schemas
  schemas <- DBI::dbGetQuery(snowCon, "
      SELECT schema_name
      FROM information_schema.schemata
      WHERE schema_name NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (schema_name in schemas$schema_name) {
    DBI::dbExecute(snowCon, paste0("GRANT USAGE, CREATE ON SCHEMA ", DBI::dbQuoteIdentifier(snowCon, schema_name), " TO admin;"))
  }
  
  # Future schemas (default privileges)
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT USAGE, CREATE ON SCHEMAS TO admin;")
  
  # 3. Grant privileges for tables, sequences, and functions
  
  # Existing objects
  tables <- DBI::dbGetQuery(snowCon, "
      SELECT schemaname, tablename
      FROM pg_tables
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(tables))) {
    schema <- tables$schemaname[row]
    table <- tables$tablename[row]
    DBI::dbExecute(snowCon, paste0("GRANT ALL PRIVILEGES ON TABLE ", DBI::dbQuoteIdentifier(snowCon, schema), ".", DBI::dbQuoteIdentifier(snowCon, table), " TO admin;"))
  }
  
  sequences <- DBI::dbGetQuery(snowCon, "
      SELECT schemaname, sequencename
      FROM pg_sequences
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(sequences))) {
    schema <- sequences$schemaname[row]
    sequence <- sequences$sequencename[row]
    DBI::dbExecute(snowCon, paste0("GRANT ALL PRIVILEGES ON SEQUENCE ", DBI::dbQuoteIdentifier(snowCon, schema), ".", DBI::dbQuoteIdentifier(snowCon, sequence), " TO admin;"))
  }
  
  # Future objects (default privileges)
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON TABLES TO admin;")
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON SEQUENCES TO admin;")
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT EXECUTE ON FUNCTIONS TO admin;")
  
  # 4. Grant role management privileges
  DBI::dbExecute(snowCon, "ALTER ROLE admin CREATEROLE;")
  
  # 5. Remove database creation privilege from admin
  DBI::dbExecute(snowCon, "ALTER ROLE admin NOCREATEDB;")
  
  message("Admin user setup completed for database ", DBI::dbGetQuery(snowCon, "SELECT current_database();")[[1]])
  
  
  # Check if 'snow_read' exists as a user. If not, create it
  snow_read_exists <- DBI::dbGetQuery(snowCon, "SELECT 1 FROM pg_roles WHERE rolname = 'snow_read';")
  
  # Create the snow reader role
  if (nrow(snow_read_exists) == 0) {
    DBI::dbExecute(con, "CREATE ROLE snow_reader WITH LOGIN PASSWORD 'snow';")
    message("Created the 'snow_reader' account with default password of 'snow'")
  }
  
  # Now modify some things for user 'snow_read'. Rename it to 'snow_reader' first:
  message("Modifying 'snow_read' user to 'snow_reader'...")
  DBI::dbExecute(snowCon, "ALTER ROLE snow_read RENAME TO snow_reader;")
  # Ensure this role by has privileges on all 'snow' tables as they are now
  message("Granting 'snow_reader' select privileges on all tables in 'snow'...")
  tables <- DBI::dbGetQuery(snowCon, "
      SELECT schemaname, tablename
      FROM pg_tables
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(tables))) {
    schema <- tables$schemaname[row]
    table <- tables$tablename[row]
    DBI::dbExecute(snowCon, paste0("GRANT SELECT ON TABLE ", DBI::dbQuoteIdentifier(snowCon, schema), ".", DBI::dbQuoteIdentifier(snowCon, table), " TO snow_reader;"))
  }
  
  # Commit the transaction
  DBI::dbExecute(snowCon, "COMMIT;")
  attr(snowCon, "active_transaction") <- FALSE
  
  message("Complete changes to snow")
  
}, error = function(e) {
  # Rollback the transaction
  DBI::dbExecute(snowCon, "ROLLBACK;")
  attr(snowCon, "active_transaction") <<- FALSE
  stop("Snow database updates failed and were rolled back to an earlier state. ", e$message)
})
