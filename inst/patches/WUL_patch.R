DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS criteria.licence_types (
    licence_type_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY ,
    type_name_en TEXT NOT NULL,
    type_name_fr TEXT NOT NULL,
    created_by TEXT DEFAULT CURRENT_USER NOT NULL,
    modified_by TEXT,
    created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified TIMESTAMPTZ
    )
    "
)

DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS criteria.licences (
    licence_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    licence_type_id INTEGER NOT NULL REFERENCES licence_types(licence_type_id),
    licence_number TEXT NOT NULL,
    issued_date DATE,
    expiry_date DATE,
    issuing_jurisdiction INTEGER REFERENCES criteria.jurisdictions(jurisdiction_id), -- see discrete data schema
    issuing_authority INTEGER REFERENCES ????, -- see discrete data schema
    enforcement_authority INTEGER REFERENCES ????,
    licence_url TEXT,
    created_by TEXT DEFAULT CURRENT_USER NOT NULL,
    modified_by TEXT,
    created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
    modified TIMESTAMPTZ
)
"
)

DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS criteria.licences_documents (
?ID column?
licence_id INTEGER NOT NULL REFERENCES criteria.licences(licence_id),
document_id INTEGER NOT NULL REFERENCES criteria.documents(document_id),
created_by TEXT DEFAULT CURRENT_USER NOT NULL,
modified_by TEXT,
created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
modified TIMESTAMPTZ,
PRIMARY KEY (licence_id, document_id)
)"
)

DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS criteria.locations_licences (
?id column?
location_id INTEGER NOT NULL REFERENCES locations(location_id),
licence_id INTEGER NOT NULL REFERENCES criteria.licences(licence_id),
created_by TEXT DEFAULT CURRENT_USER NOT NULL,
modified_by TEXT,
created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
modified TIMESTAMPTZ,
PRIMARY KEY (location_id, licence_id)
)"
)

table_names <- c(
    "licence_types",
    "licences",
    "licences_documents",
    "locations_licences"
)

for (i in 1:length(table_names)) {
    table_name <- table_names[i]
    DBI::dbExecute(
        con,
        sprintf(
            "DROP TRIGGER IF EXISTS criteria.%s_user_modified
         ON criteria.%s;",
            table_name,
            table_name
        )
    )
    DBI::dbExecute(
        con,
        sprintf(
            "CREATE TRIGGER criteria.%s_user_modified
         BEFORE UPDATE ON criteria.%s
         FOR EACH ROW EXECUTE FUNCTION public.user_modified();",
            table_name,
            table_name
        )
    )
    DBI::dbExecute(
        con,
        sprintf(
            "DROP TRIGGER IF EXISTS criteria.%s_update_modified
         ON criteria.%s;",
            table_name,
            table_name
        )
    )
    DBI::dbExecute(
        con,
        sprintf(
            "CREATE TRIGGER criteria.%s_update_modified
         BEFORE UPDATE ON criteria.%s
         FOR EACH ROW EXECUTE FUNCTION public.update_modified();",
            table_name,
            table_name
        )
    )
}
