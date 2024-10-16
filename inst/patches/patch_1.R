# First patch. This creates a new schema and table.

# Ensure the user has CREATE privileges on the database and can therefore create a new schema
check <- DBI::dbGetQuery(con, "SELECT has_database_privilege(current_user, 'CREATE') AS can_create")

if (!check$can_create) {
  stop("You do not have the necessary privileges to create a new schema in this database.")
}

# create new schema called 'information'
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS information")

# Alter the default schema search path
DBI::dbExecute(con, 'ALTER DATABASE "AquaCache" SET search_path TO public, instruments, information;')
# Set search_path for the current session
DBI::dbExecute(con, 'SET search_path TO public, instruments, information')


# Create a new table in the new schema
DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS information.version_info (
  id SERIAL PRIMARY KEY,
  item TEXT,
  version TEXT,
  created_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
)")

# Function update_created_modified already exists so use it
# Create trigger
DBI::dbExecute(con, "CREATE TRIGGER update_version_info_created_modified
    BEFORE INSERT OR UPDATE ON information.version_info
    FOR EACH ROW
    EXECUTE FUNCTION update_created_modified();
  ")

tbl <- data.frame(item = c("AquaCache R package used for last patch", "Last patch number"), version = c(as.character(packageVersion("AquaCache")), "1"))

DBI::dbWriteTable(con, "version_info", tbl, append = TRUE, row.names = FALSE)


# Check that the table has two rows and is in the right schema
check <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM information.version_info")

if (nrow(check) != 2) {
  stop("The table 'version_info' was not created successfully or is not in the right schema.")
}

# Give user a message
message("Patch 1 applied successfully: created new schema 'information' and table 'version_info'.")
