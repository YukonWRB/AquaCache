# Tables for use with YGwater application

# Create new schema 'application' in the database
DBI::dbExecute(con, "CREATE SCHEMA application")
DBI::dbExecute(con, "COMMENT ON SCHEMA application IS 'Schema to hold application-related data, such as text and images which need frequent updates, metrics such as number of viewers, plots generated, etc.';")

# modify the seach path to include the new schema
DBI::dbExecute(con, "ALTER DATABASE aquacache SET search_path TO public, continuous, discrete, spatial, files, instruments, information, application;")

# Grant usage to all
DBI::dbExecute(con, "GRANT USAGE ON SCHEMA application TO PUBLIC;")
DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA application GRANT SELECT ON TABLES TO PUBLIC;")


# Create table to hold text for the application
DBI::dbExecute(con, "CREATE TABLE application.text (
                id TEXT NOT NULL PRIMARY KEY,
                text_en TEXT NOT NULL,
                text_fr TEXT,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                updated TIMESTAMP WITH TIME ZONE
                );")
# Comment on the table
DBI::dbExecute(con, "COMMENT ON TABLE application.text IS 'Table to hold frequently changed text for the application, such as news, descriptions, etc. Text which is more static is instead stored in the R package files.';")
DBI::dbExecute(con, "COMMENT ON COLUMN application.text.id IS 'Unique identifier for the text; this is referenced in the application to select the correct entry.';")

# Create table to hold images for the application
DBI::dbExecute(con, "CREATE TABLE application.images (
                id TEXT NOT NULL PRIMARY KEY,
                image BYTEA NOT NULL,
                format TEXT NOT NULL,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                updated TIMESTAMP WITH TIME ZONE
                );")



