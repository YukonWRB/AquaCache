# Tables for use with YGwater application

# Create new schema 'application' in the database
DBI::dbExecute(con, "CREATE SCHEMA application")
DBI::dbExecute(con, "COMMENT ON SCHEMA application IS 'Schema to hold application-related data, such as text and images which need frequent updates, metrics such as number of viewers, plots generated, etc.';")

# modify the seach path to include the new schema
DBI::dbExecute(con, "ALTER DATABASE aquacache SET search_path TO public, continuous, discrete, spatial, files, instruments, information, application;")

# Grant usage to all
DBI::dbExecute(con, "GRANT USAGE ON SCHEMA application TO PUBLIC;")
DBI::dbExecute(con, "ALTER DEFAULT PRIVILEGES IN SCHEMA application GRANT SELECT ON TABLES TO PUBLIC;")


# Create table to hold text for the application
DBI::dbExecute(con, "CREATE TABLE application.text (
                id TEXT NOT NULL PRIMARY KEY,
                text_en TEXT NOT NULL,
                text_fr TEXT,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                modified TIMESTAMP WITH TIME ZONE
                );")
# Comment on the table
DBI::dbExecute(con, "COMMENT ON TABLE application.text IS 'Table to hold frequently changed text for the application, such as news, descriptions, etc. Text which is more static is instead stored in the R package files.';")
DBI::dbExecute(con, "COMMENT ON COLUMN application.text.id IS 'Unique identifier for the text; this is referenced in the application to select the correct entry.';")
DBI::dbExecute(con, "CREATE TRIGGER update_text_modified BEFORE UPDATE ON application.text FOR EACH ROW EXECUTE FUNCTION public.update_modified()")

# Create table to hold images for the application
DBI::dbExecute(con, "CREATE TABLE application.images (
                id TEXT NOT NULL PRIMARY KEY,
                image BYTEA NOT NULL,
                format TEXT NOT NULL,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                modified TIMESTAMP WITH TIME ZONE
                );")

DBI::dbExecute(con, "create trigger update_image_modified before update on application.images for each row execute function public.update_modified()")

# Create a table to order the images and text for each page
DBI::dbExecute(con, "CREATE TABLE application.page_content (
                page TEXT NOT NULL,
                position INTEGER NOT NULL,
                content_type TEXT NOT NULL CHECK (content_type IN ('text', 'image')),
                content_id TEXT NOT NULL
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                modified TIMESTAMP WITH TIME ZONE
                );")
DBI::dbExecute(con, "COMMENT ON TABLE application.page_content IS 'Table to hold the content for each page, with the position of each element. This is used to order the text and images on each page.';")
DBI::dbExecute(con, "COMMENT ON COLUMN application.page_content.page IS 'The page to which the content belongs.';")
DBI::dbExecute(con, "COMMENT ON COLUMN application.page_content.position IS 'The position of the content on the page.';")
DBI::dbExecute(con, "COMMENT ON COLUMN application.page_content.content_type IS 'The type of content, either text or image.';")
DBI::dbExecute(con, "COMMENT ON COLUMN application.page_content.content_id IS 'The id of the content, either the text id or the image id.';")
DBI::dbExecute(con, "CREATE TRIGGER update_page_content_modified BEFORE UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.update_modified()")

DBI::dbExecute(con, "GRANT SELECT ON ALL TABLES IN SCHEMA application TO PUBLIC;")

# Enforce referential integrity of the page_content table
DBI::dbExecute(con, "
               CREATE OR REPLACE FUNCTION check_page_content_integrity() 
RETURNS trigger AS $$
BEGIN
  IF NEW.content_type = 'text' THEN
    IF NOT EXISTS (
      SELECT 1 FROM application.text WHERE id = NEW.content_id
    ) THEN
      RAISE EXCEPTION 'Text id % does not exist', NEW.content_id;
    END IF;
  ELSIF NEW.content_type = 'image' THEN
    IF NOT EXISTS (
      SELECT 1 FROM application.images WHERE id = NEW.content_id
    ) THEN
      RAISE EXCEPTION 'Image id % does not exist', NEW.content_id;
    END IF;
  ELSE
    RAISE EXCEPTION 'Unknown content type: %', NEW.content_type;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
")

DBI::dbExecute(con, "
CREATE TRIGGER trg_page_content_integrity
BEFORE INSERT OR UPDATE ON application.page_content
FOR EACH ROW 
EXECUTE FUNCTION check_page_content_integrity();
")
