# Patch 8.
# Creates new table 'network_types' ....

# Initial checks #################
# Ensure the user is postgres as this patch creates new users
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop("You do not have the necessary privileges for this patch. Connect as postgres user to make this work.")
}

message("Working on Patch 8. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?).")

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch({
  
  # Grant privileges for tables, sequences, functions, triggers, and views
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
  
  functions <- DBI::dbGetQuery(con, "
    SELECT 
        n.nspname AS schema_name,                     -- Schema name
        p.proname AS function_name,                  -- Function name
        pg_get_function_identity_arguments(p.oid) AS argument_types -- Argument types
    FROM pg_proc p
    JOIN pg_namespace n ON p.pronamespace = n.oid   -- Join to get schema name
    WHERE n.nspname NOT IN ('pg_catalog', 'information_schema'); -- Exclude system schemas
")
  # Grant EXECUTE privilege on each function
  for (i in seq_len(nrow(functions))) {
    schema <- functions$schema_name[i]
    function_name <- functions$function_name[i]
    argument_types <- functions$argument_types[i] # Include argument types
    query <- paste0("GRANT EXECUTE ON FUNCTION ", 
                    DBI::dbQuoteIdentifier(con, schema), ".", 
                    DBI::dbQuoteIdentifier(con, function_name), "(", 
                    argument_types, ") TO admin;")
    DBI::dbExecute(con, query)
  }
  
  
  
  # Query all tables
  tables <- DBI::dbGetQuery(con, "
    SELECT schemaname, tablename
    FROM pg_tables
    WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
")
  
  # Grant TRIGGER privilege on each table
  for (i in seq_len(nrow(tables))) {
    schema <- tables$schemaname[i]
    table <- tables$tablename[i]
    query <- paste0("GRANT TRIGGER ON TABLE ", 
                    DBI::dbQuoteIdentifier(con, schema), ".", 
                    DBI::dbQuoteIdentifier(con, table), " TO admin;")
    DBI::dbExecute(con, query)
  }
  
  
  views <- DBI::dbGetQuery(con, "
      SELECT schemaname AS schema_name, viewname AS view_name
      FROM pg_views
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(views))) {
    schema <- views$schema_name[row]
    view <- views$view_name[row]
    query <- paste0(
      "GRANT ALL PRIVILEGES ON ", 
      DBI::dbQuoteIdentifier(con, schema), ".", 
      DBI::dbQuoteIdentifier(con, view), 
      " TO admin;"
    )
    DBI::dbExecute(con, query)
  }
  
  # Future objects (default privileges)
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON TABLES TO admin;")
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON SEQUENCES TO admin;")
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT EXECUTE ON FUNCTIONS TO admin;")
  DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT TRIGGER ON TABLES TO admin;")

  
  # Update the version_info table
  DBI::dbExecute(con, "UPDATE information.version_info SET version = '8' WHERE item = 'Last patch number';")
  DBI::dbExecute(con, paste0("UPDATE information.version_info SET version = '", as.character(packageVersion("AquaCache")), "' WHERE item = 'AquaCache R package used for last patch';"))
  
  # Commit the transaction
  DBI::dbExecute(con, "COMMIT;")
  attr(con, "active_transaction") <- FALSE
  
  
  message("Patch 8 applied successfully: admin now has access to all views, functions, and triggers and defaults to have access to all in future.")
  
}, error = function(e) {
  
  # Rollback the transaction
  DBI::dbExecute(con, "ROLLBACK;")
  attr(con, "active_transaction") <<- FALSE
  stop("Patch 8 failed and the DB has been rolled back to its earlier state. ", e$message)
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
  
  # Grant privileges for tables, sequences, functions, triggers, and views
  
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
  
  functions <- DBI::dbGetQuery(snowCon, "
    SELECT 
        n.nspname AS schema_name,                     -- Schema name
        p.proname AS function_name,                  -- Function name
        pg_get_function_identity_arguments(p.oid) AS argument_types -- Argument types
    FROM pg_proc p
    JOIN pg_namespace n ON p.pronamespace = n.oid   -- Join to get schema name
    WHERE n.nspname NOT IN ('pg_catalog', 'information_schema'); -- Exclude system schemas
")
  # Grant EXECUTE privilege on each function
  for (i in seq_len(nrow(functions))) {
    schema <- functions$schema_name[i]
    function_name <- functions$function_name[i]
    argument_types <- functions$argument_types[i] # Include argument types
    query <- paste0("GRANT EXECUTE ON FUNCTION ", 
                    DBI::dbQuoteIdentifier(snowCon, schema), ".", 
                    DBI::dbQuoteIdentifier(snowCon, function_name), "(", 
                    argument_types, ") TO admin;")
    DBI::dbExecute(snowCon, query)
  }
  
  
  # Query all tables
  tables <- DBI::dbGetQuery(snowCon, "
    SELECT schemaname, tablename
    FROM pg_tables
    WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
")
  
  # Grant TRIGGER privilege on each table
  for (i in seq_len(nrow(tables))) {
    schema <- tables$schemaname[i]
    table <- tables$tablename[i]
    query <- paste0("GRANT TRIGGER ON TABLE ", 
                    DBI::dbQuoteIdentifier(snowCon, schema), ".", 
                    DBI::dbQuoteIdentifier(snowCon, table), " TO admin;")
    DBI::dbExecute(snowCon, query)
  }
  
  
  views <- DBI::dbGetQuery(snowCon, "
      SELECT schemaname, viewname
      FROM pg_views
      WHERE schemaname NOT IN ('pg_catalog', 'information_schema', 'pg_toast');
  ")
  for (row in seq_len(nrow(views))) {
    schema <- views$schemaname[row]
    view <- views$viewname[row]
    DBI::dbExecute(snowCon, paste0("GRANT ALL PRIVILEGES ON ", DBI::dbQuoteIdentifier(snowCon, schema), ".", DBI::dbQuoteIdentifier(snowCon, view), " TO admin;"))
  }
  
  # Future objects (default privileges)
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON TABLES TO admin;")
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT ALL PRIVILEGES ON SEQUENCES TO admin;")
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT EXECUTE ON FUNCTIONS TO admin;")
  DBI::dbExecute(snowCon, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres GRANT TRIGGER ON TABLES TO admin;")
  
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

