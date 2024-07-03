# user and user group tables #################
DBI::dbExecute(con, "CREATE TABLE if not exists user_groups (
                 group_id SERIAL PRIMARY KEY,
                 group_name TEXT UNIQUE NOT NULL,
                 group_description TEXT NOT NULL);")

groups <- data.frame(group_name = c("public"),
                     group_description = c("Default group for all public users."))

DBI::dbAppendTable(con, "user_groups", groups)

# Create function and trigger to make sure that the public group cannot be deleted
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION prevent_delete_public_group()
RETURNS TRIGGER AS $$
BEGIN
    IF OLD.group_id = 1 THEN
        RAISE EXCEPTION 'Cannot delete the public group (group_id 1)';
    END IF;
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;
")
DBI::dbExecute(con, "CREATE TRIGGER prevent_delete_public_group_trigger
BEFORE DELETE ON user_groups
FOR EACH ROW
EXECUTE FUNCTION prevent_delete_public_group();
")



# Create users table
DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS users (
                 user_id SERIAL PRIMARY KEY,
                 username TEXT UNIQUE NOT NULL,
                 email TEXT UNIQUE NOT NULL,
                 group_id INTEGER NOT NULL,
                 password_hash TEXT NOT NULL,
                 password_salt TEXT NOT NULL,
                 algorithm TEXT NOT NULL DEFAULT 'sha256',
                 FOREIGN KEY (group_id) REFERENCES user_groups(group_id)
);")

# owners_contributors table #################
DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS owners_contributors (
                 owner_contributor_id SERIAL PRIMARY KEY,
                 name TEXT UNIQUE NOT NULL,
                 contact_name TEXT,
                 phone TEXT,
                 email TEXT,
                 note TEXT
               );")


# Add visibility_public columns to locations and images_index tables
DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN visibility_public TEXT NOT NULL CHECK(visibility_public IN ('exact', 'region', 'jitter')) DEFAULT 'exact'")
DBI::dbExecute(con, "ALTER TABLE images_index ADD COLUMN visibility_public TEXT NOT NULL CHECK(visibility_public IN ('exact', 'region', 'jitter')) DEFAULT 'exact'")



# Add share_with column to tables locations, timeseries, measurements_continuous, measurements_discrete, images, images_index, and documents
DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE timeseries ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE measurements_continuous ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE calculated_daily ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE images_index ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")
DBI::dbExecute(con, "ALTER TABLE documents ADD COLUMN share_with INTEGER[] NOT NULL DEFAULT '{1}'")



# Enforce referential integrity for share_with column
# Create function to remove a deleted group_id from all protected tables
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION remove_group_id_from_share_with()
RETURNS TRIGGER AS $$
BEGIN
    -- Update the share_with array to remove the deleted group_id for each table
    EXECUTE 'UPDATE locations SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE timeseries SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_continuous SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE measurements_discrete SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE calculated_daily SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE images SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE images_index SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    EXECUTE 'UPDATE documents SET share_with = array_remove(share_with, $1) WHERE $1 = ANY(share_with)' USING OLD.group_id;
    RETURN OLD;
END;
$$ LANGUAGE plpgsql;
")
# Create trigger to run function above when a group_id is deleted
DBI::dbExecute(con, "CREATE TRIGGER remove_group_id_trigger
BEFORE DELETE ON user_groups
FOR EACH ROW
EXECUTE FUNCTION remove_group_id_from_share_with();
")


# Create function to validate share_with array
DBI::dbExecute(con, "CREATE OR REPLACE FUNCTION validate_share_with()
RETURNS TRIGGER AS $$
BEGIN
    -- Check if any group_id in the NEW.share_with array is not present in user_groups table
    IF EXISTS (
        SELECT 1
        FROM unnest(NEW.share_with) AS group_id
        WHERE group_id NOT IN (SELECT group_id FROM user_groups)
    ) THEN
        RAISE EXCEPTION 'Invalid group_id in share_with array';
    END IF;
    
    -- Check if 1 (public group) is in the share_with array
    IF 1 = ANY(NEW.share_with) AND array_length(NEW.share_with, 1) > 1 THEN
        RAISE EXCEPTION 'If group_id 1 (public) is present in the share_with array, it must be the only value';
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

")

# Create triggers to run function above when share_with array is updated on each table
DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_locations
BEFORE INSERT OR UPDATE ON locations
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_timeseries
BEFORE INSERT OR UPDATE ON timeseries
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_measurements_continuous
BEFORE INSERT OR UPDATE ON measurements_continuous
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_measurements_discrete
BEFORE INSERT OR UPDATE ON measurements_discrete
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_calculated_daily
BEFORE INSERT OR UPDATE ON calculated_daily
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_images
BEFORE INSERT OR UPDATE ON images
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_images_index
BEFORE INSERT OR UPDATE ON images_index
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")

DBI::dbExecute(con, "CREATE TRIGGER validate_share_with_trigger_documents
BEFORE INSERT OR UPDATE ON documents
FOR EACH ROW
EXECUTE FUNCTION validate_share_with();")



# Add owner and contributor columns to tables locations, timeseries, measurements_continuous, measurements_discrete, calculated_daily, images, images_index, and documents
DBI::dbExecute(con, "ALTER TABLE locations ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE timeseries ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE measurements_continuous ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")
DBI::dbExecute(con, "ALTER TABLE measurements_continuous ADD COLUMN contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")
DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD COLUMN contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE calculated_daily ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")
DBI::dbExecute(con, "ALTER TABLE calculated_daily ADD COLUMN contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")
DBI::dbExecute(con, "ALTER TABLE images ADD COLUMN contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE images_index ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")

DBI::dbExecute(con, "ALTER TABLE documents ADD COLUMN owner INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")
DBI::dbExecute(con, "ALTER TABLE documents ADD COLUMN contributor INTEGER DEFAULT NULL REFERENCES owners_contributors (owner_contributor_id) ON DELETE SET NULL ON UPDATE CASCADE")


# Add no_update column to measurements_continuous and measurements_discrete and calculated_daily tables
DBI::dbExecute(con, "ALTER TABLE measurements_continuous ADD COLUMN no_update BOOLEAN NOT NULL DEFAULT FALSE")
DBI::dbExecute(con, "ALTER TABLE measurements_discrete ADD COLUMN no_update BOOLEAN NOT NULL DEFAULT FALSE")
DBI::dbExecute(con, "ALTER TABLE calculated_daily ADD COLUMN no_update BOOLEAN NOT NULL DEFAULT FALSE")



# Enable row level security on tables locations, timeseries, measurements_continuous, measurements_discrete, calculated_daily, images, images_index, and documents
DBI::dbExecute(con, "ALTER TABLE locations ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE timeseries ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_continuous ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_discrete ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE calculated_daily ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE images ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE images_index ENABLE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE documents ENABLE ROW LEVEL SECURITY;")

# Create policy for each table
DBI::dbExecute(con, "CREATE POLICY location_policy ON locations
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY timeseries_policy ON timeseries
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY measurements_continuous_policy ON measurements_continuous
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY measurements_discrete_policy ON measurements_discrete
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY calculated_daily_policy ON calculated_daily
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY images_policy ON images
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY images_index_policy ON images_index
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")
DBI::dbExecute(con, "CREATE POLICY documents_policy ON documents
USING (
  EXISTS (
    SELECT 1
    FROM unnest(share_with) AS unnested_group_id
    JOIN users u ON unnested_group_id = u.group_id
    WHERE u.username = current_setting('logged_in_user.username', true)
  )
);")


# -- Force RLS to be applied
DBI::dbExecute(con, "ALTER TABLE locations FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE timeseries FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_continuous FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE measurements_discrete FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE calculated_daily FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE images FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE images_index FORCE ROW LEVEL SECURITY;")
DBI::dbExecute(con, "ALTER TABLE documents FORCE ROW LEVEL SECURITY;")
