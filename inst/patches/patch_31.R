# Patch 31

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 31. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    message(
      "Creating functions to enable the deletion of roles from share_with columns, removal of these roles from database roles with checks on references within tables, and new application-specific tables."
    )
    # Function to delete or re-assign roles from share_with columns
    DBI::dbExecute(
      con,
      'DROP FUNCTION IF EXISTS public.cleanup_share_with_role'
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.cleanup_share_with_role(
      role_to_delete     text,
      replacement_role   text DEFAULT NULL,
      dry_run            boolean DEFAULT true
    )
    RETURNS TABLE(
      table_schema     text,
      table_name       text,
      only_role_rows   integer,
      matched_rows     integer,
      updated_rows     integer
    )
    LANGUAGE plpgsql
    AS $$
    DECLARE
      rec record;
    BEGIN
      IF role_to_delete IS NULL OR btrim(role_to_delete) = '' THEN
        RAISE EXCEPTION 'role_to_delete must be a non-empty role name.';
      END IF;
    
      -- If a replacement role is provided, validate it is an allowed share_with role.
      IF replacement_role IS NOT NULL THEN
        IF replacement_role = role_to_delete THEN
          RAISE EXCEPTION 'Replacement role must differ from the role being deleted.';
        END IF;
    
        IF NOT EXISTS (
          SELECT 1
          FROM pg_roles r
          WHERE r.rolname = replacement_role
            AND (
              r.rolname = 'public_reader'
              OR (r.rolcanlogin = false AND r.rolname <> 'public' AND r.rolname !~ '^pg_')
            )
        ) THEN
          RAISE EXCEPTION 'Replacement role % is not a valid share_with role.', replacement_role;
        END IF;
      END IF;
    
      -- If actually executing and no replacement is provided, block if any row has share_with = {role_to_delete}.
      IF NOT dry_run AND replacement_role IS NULL THEN
        FOR rec IN
          SELECT n.nspname AS schema_name, c.relname AS table_name
          FROM pg_class c
          JOIN pg_namespace n ON n.oid = c.relnamespace
          JOIN pg_attribute a ON a.attrelid = c.oid
          WHERE c.relkind = 'r'
            AND c.relrowsecurity
            AND a.attname = 'share_with'
            AND NOT a.attisdropped
        LOOP
          EXECUTE format(
            'SELECT count(*)
             FROM %I.%I
             WHERE share_with IS NOT NULL
               AND $1 = ANY(share_with)
               AND array_length(share_with, 1) = 1',
            rec.schema_name, rec.table_name
          )
          INTO only_role_rows
          USING role_to_delete;
    
          IF only_role_rows > 0 THEN
            RAISE EXCEPTION
              'Role % is the sole share_with entry in %.% (% rows) - replacement required.',
              role_to_delete, rec.schema_name, rec.table_name, only_role_rows;
          END IF;
        END LOOP;
      END IF;
    
      -- Main loop: compute counts and optionally update.
      FOR rec IN
        SELECT n.nspname AS schema_name, c.relname AS table_name
        FROM pg_class c
        JOIN pg_namespace n ON n.oid = c.relnamespace
        JOIN pg_attribute a ON a.attrelid = c.oid
        WHERE c.relkind = 'r'
          AND c.relrowsecurity
          AND a.attname = 'share_with'
          AND NOT a.attisdropped
      LOOP
        -- Count rows where role appears; count sole-entry rows.
        EXECUTE format(
          'SELECT
             count(*) FILTER (WHERE share_with IS NOT NULL AND $1 = ANY(share_with)),
             count(*) FILTER (WHERE share_with IS NOT NULL AND $1 = ANY(share_with) AND array_length(share_with,1)=1)
           FROM %I.%I',
          rec.schema_name, rec.table_name
        )
        INTO matched_rows, only_role_rows
        USING role_to_delete;
    
        updated_rows := 0;
    
        IF NOT dry_run AND matched_rows > 0 THEN
          IF replacement_role IS NULL THEN
            -- Safe remove-only path. (We already guarded sole-entry rows above.)
            EXECUTE format(
              'UPDATE %I.%I
               SET share_with = array_remove(share_with, $1)
               WHERE share_with IS NOT NULL
                 AND $1 = ANY(share_with)',
              rec.schema_name, rec.table_name
            )
            USING role_to_delete;
    
          ELSE
            -- Replace if it was the only entry; otherwise remove.
            EXECUTE format(
              'UPDATE %I.%I
               SET share_with = CASE
                 WHEN array_length(share_with, 1) = 1 AND $1 = ANY(share_with)
                   THEN ARRAY[$2]::text[]
                 ELSE array_remove(share_with, $1)
               END
               WHERE share_with IS NOT NULL
                 AND $1 = ANY(share_with)',
              rec.schema_name, rec.table_name
            )
            USING role_to_delete, replacement_role;
          END IF;
    
          GET DIAGNOSTICS updated_rows = ROW_COUNT;
        END IF;
    
        table_schema := rec.schema_name;
        table_name := rec.table_name;
        RETURN NEXT;
      END LOOP;
    
    END;
    $$;
    "
    )

    # Function to delete roles while checking if they're still used
    DBI::dbExecute(con, "DROP FUNCTION IF EXISTS public.drop_role_if_unused")
    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE FUNCTION public.drop_role_if_unused(
        role_to_delete                 text
      )
      RETURNS TABLE(
        dropped                boolean,
        remaining_references   bigint,
        message                text
      )
      LANGUAGE plpgsql
      AS $$
      DECLARE
        rec record;
        ref_count bigint;
        total_refs bigint := 0;
      BEGIN
        IF role_to_delete IS NULL OR btrim(role_to_delete) = '' THEN
          RAISE EXCEPTION 'role_to_delete must be a non-empty role name.';
        END IF;
      
        IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = role_to_delete) THEN
          dropped := false;
          remaining_references := 0;
          message := format('Role %s does not exist.', role_to_delete);
          RETURN NEXT;
          RETURN;
        END IF;
      
        -- Count references across all RLS tables with share_with column.
        FOR rec IN
          SELECT n.nspname AS schema_name, c.relname AS table_name
          FROM pg_class c
          JOIN pg_namespace n ON n.oid = c.relnamespace
          JOIN pg_attribute a ON a.attrelid = c.oid
          WHERE c.relkind = 'r'
            AND c.relrowsecurity
            AND a.attname = 'share_with'
            AND NOT a.attisdropped
        LOOP
          EXECUTE format(
            'SELECT count(*)
             FROM %I.%I
             WHERE share_with IS NOT NULL
               AND $1 = ANY(share_with)',
            rec.schema_name, rec.table_name
          )
          INTO ref_count
          USING role_to_delete;
      
          total_refs := total_refs + ref_count;
        END LOOP;
      
        IF total_refs > 0 THEN
          dropped := false;
          remaining_references := total_refs;
          message := format(
            'Role %s is still referenced in share_with (%s total row references). Not dropped.',
            role_to_delete, total_refs
          );
          RETURN NEXT;
          RETURN;
        END IF;
      
        -- Safe to drop the role (as far as share_with references are concerned).
        -- Note: DROP ROLE can still fail if the role owns objects, has privileges, or memberships.
        BEGIN
          EXECUTE format('DROP ROLE %I', role_to_delete);
          dropped := true;
          remaining_references := 0;
          message := format('Role %s dropped (no remaining share_with references).', role_to_delete);
        EXCEPTION
          WHEN dependent_objects_still_exist OR insufficient_privilege OR object_in_use OR others THEN
            dropped := false;
            remaining_references := 0;
            message := format(
              'Role %s has no share_with references, but DROP ROLE failed: %s',
              role_to_delete, SQLERRM
            );
        END;
      
        RETURN NEXT;
      END;
      $$;      
    "
    )

    # Checks for existing application schema and tables
    # Create new schema 'application' in the database
    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS application")
    DBI::dbExecute(
      con,
      "COMMENT ON SCHEMA application IS 'Schema to hold application-related data, such as text and images which need frequent updates, metrics such as number of viewers, plots generated, etc.';"
    )

    # modify the seach path to include the new schema
    DBI::dbExecute(
      con,
      "ALTER DATABASE aquacache SET search_path TO public, continuous, discrete, spatial, files, instruments, information, application;"
    )

    # Grant usage to all
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA application TO PUBLIC;")
    DBI::dbExecute(
      con,
      "ALTER DEFAULT PRIVILEGES IN SCHEMA application GRANT SELECT ON TABLES TO PUBLIC;"
    )

    # Create table to hold text for the application
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.text (
                id TEXT NOT NULL PRIMARY KEY,
                text_en TEXT NOT NULL,
                text_fr TEXT,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                modified TIMESTAMP WITH TIME ZONE
                );"
    )
    # Comment on the table
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.text IS 'Table to hold frequently changed text for the application, such as news, descriptions, etc. Text which is more static is instead stored in the R package files.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.text.id IS 'Unique identifier for the text; this is referenced in the application to select the correct entry.';"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE TRIGGER update_text_modified BEFORE UPDATE ON application.text FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )

    # Create table to hold images for the application
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.images (
                id TEXT NOT NULL PRIMARY KEY,
                image BYTEA NOT NULL,
                format TEXT NOT NULL,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                modified TIMESTAMP WITH TIME ZONE
                );"
    )

    DBI::dbExecute(
      con,
      "create OR REPLACE trigger update_image_modified before update on application.images for each row execute function public.update_modified()"
    )

    # Create a table to order the images and text for each page
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.page_content (
                page TEXT NOT NULL,
                position INTEGER NOT NULL,
                content_type TEXT NOT NULL CHECK (content_type IN ('text', 'image')),
                content_id TEXT NOT NULL,
                created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
                modified TIMESTAMP WITH TIME ZONE
                );"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.page_content IS 'Table to hold the content for each page, with the position of each element. This is used to order the text and images on each page.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.page_content.page IS 'The page to which the content belongs.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.page_content.position IS 'The position of the content on the page.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.page_content.content_type IS 'The type of content, either text or image.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.page_content.content_id IS 'The id of the content, either the text id or the image id.';"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE TRIGGER update_page_content_modified BEFORE UPDATE ON application.page_content FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS application.notifications (
      notification_id INTEGER GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY,
      active BOOLEAN NOT NULL DEFAULT TRUE,
      target_module TEXT[] NOT NULL,
      message JSONB NOT NULL
      );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX ON application.notifications USING GIN (target_module);"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE application.notifications IS 'Holds notification text for the YGwater application or any other application which interfaces with this database and requires the display of notifications without opening application code.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN application.notifications.message IS 'The message to display in the application. Use JSONB formatting to match the language to the chose message in a way that will be understood by the application logic. For example, the YGwater app language codes are English, French, etc.'"
    )

    DBI::dbExecute(
      con,
      "GRANT SELECT ON ALL TABLES IN SCHEMA application TO PUBLIC;"
    )

    # Enforce referential integrity of the page_content table
    DBI::dbExecute(
      con,
      "
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
        "
    )

    # Drop and re-create trigger

    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE TRIGGER trg_page_content_integrity
      BEFORE INSERT OR UPDATE ON application.page_content
      FOR EACH ROW 
      EXECUTE FUNCTION check_page_content_integrity();
      "
    )

    # Addition of application table for notifications

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '31' WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion("AquaCache")),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")

    message("Patch 31 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 31 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
