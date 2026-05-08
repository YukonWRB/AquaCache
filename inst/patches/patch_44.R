# Patch 44: resampled corrected continuous measurement access, instrument maintenance cleanup, and deployment-timeseries links
#
# Replaces continuous.measurements_continuous_corrected() and
# continuous.measurements_continuous_corrected_at() with signatures that include
# statistic and resample_seconds arguments. The exact-data path remains a direct
# query; resampled paths aggregate in PostgreSQL and return explicit empty bins.
#
# Also separates due-maintenance state from completed instrument maintenance
# events and adds normalized instrument sensor event tables. The legacy
# instruments.array_maintenance_changes table is migrated into normalized tables
# and dropped. Instrument deployment to timeseries links are normalized so one
# deployment can be associated with more than one timeseries.

# Initial checks ##############################################################
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 44: adding resampled corrected continuous measurement functions, cleaning up instrument maintenance schema, and normalizing instrument deployment-timeseries links. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

message("Starting transaction...")
active <- dbTransBegin(con)

tryCatch(
  {
    required_functions <- DBI::dbGetQuery(
      con,
      "SELECT missing_function
       FROM (
         VALUES
           ('continuous.measurements_continuous_corrected(integer, timestamp with time zone, timestamp with time zone)'),
           ('continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone)')
       ) required(missing_function)
       WHERE to_regprocedure(missing_function) IS NULL"
    )

    if (nrow(required_functions) > 0) {
      stop(
        "This patch requires the following functions to already exist: ",
        paste(required_functions$missing_function, collapse = ", ")
      )
    }

    required_relations <- DBI::dbGetQuery(
      con,
      "SELECT missing_relation
       FROM (
         VALUES
           ('instruments.instruments'),
           ('instruments.instrument_maintenance'),
           ('instruments.array_maintenance_changes'),
           ('instruments.observers'),
           ('instruments.sensors'),
           ('public.locations_metadata_instruments'),
           ('continuous.timeseries')
       ) required(missing_relation)
       WHERE to_regclass(missing_relation) IS NULL"
    )

    if (nrow(required_relations) > 0) {
      stop(
        "This patch requires the following relations to already exist: ",
        paste(required_relations$missing_relation, collapse = ", ")
      )
    }

    q_role <- function(role_name) {
      if (toupper(role_name) == "PUBLIC") {
        return("PUBLIC")
      }
      as.character(DBI::dbQuoteIdentifier(con, role_name))
    }

    existing_roles <- function() {
      DBI::dbGetQuery(con, "SELECT rolname FROM pg_catalog.pg_roles")$rolname
    }

    get_function_execute_grants <- function(function_signature) {
      DBI::dbGetQuery(
        con,
        "SELECT
           CASE WHEN acl.grantee = 0 THEN 'PUBLIC' ELSE grantee_role.rolname END AS grantee,
           acl.is_grantable
         FROM pg_catalog.pg_proc p
         CROSS JOIN LATERAL pg_catalog.aclexplode(
           COALESCE(p.proacl, pg_catalog.acldefault('f', p.proowner))
         ) acl
         LEFT JOIN pg_catalog.pg_roles grantee_role
           ON grantee_role.oid = acl.grantee
         WHERE p.oid = $1::regprocedure
           AND acl.privilege_type = 'EXECUTE'
         ORDER BY grantee",
        params = list(function_signature)
      )
    }

    apply_function_execute_grants <- function(function_signature, grants) {
      if (!nrow(grants)) {
        return(invisible(FALSE))
      }

      roles <- existing_roles()

      for (i in seq_len(nrow(grants))) {
        grantee <- grants$grantee[i]
        if (toupper(grantee) != "PUBLIC" && !grantee %in% roles) {
          next
        }

        DBI::dbExecute(
          con,
          sprintf(
            "GRANT EXECUTE ON FUNCTION %s TO %s%s",
            function_signature,
            q_role(grantee),
            if (isTRUE(grants$is_grantable[i])) " WITH GRANT OPTION" else ""
          )
        )
      }

      invisible(TRUE)
    }

    q_ident <- function(identifier) {
      as.character(DBI::dbQuoteIdentifier(con, identifier))
    }

    q_relation <- function(relation_name) {
      parts <- strsplit(relation_name, ".", fixed = TRUE)[[1]]
      paste(vapply(parts, q_ident, character(1)), collapse = ".")
    }

    q_ident_list <- function(identifiers) {
      paste(vapply(identifiers, q_ident, character(1)), collapse = ", ")
    }

    relation_exists <- function(relation_name) {
      DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1) IS NOT NULL AS exists",
        params = list(relation_name)
      )$exists[[1]]
    }

    column_exists <- function(schema_name, table_name, column_name) {
      DBI::dbGetQuery(
        con,
        "SELECT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = $1
             AND table_name = $2
             AND column_name = $3
         ) AS exists",
        params = list(schema_name, table_name, column_name)
      )$exists[[1]]
    }

    replace_fk_constraint <- function(
      table_name,
      constraint_name,
      columns,
      referenced_table,
      referenced_columns,
      on_delete = "RESTRICT",
      on_update = "CASCADE"
    ) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s DROP CONSTRAINT IF EXISTS %s",
          q_relation(table_name),
          q_ident(constraint_name)
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          paste(
            "ALTER TABLE %s ADD CONSTRAINT %s",
            "FOREIGN KEY (%s) REFERENCES %s (%s)",
            "ON UPDATE %s ON DELETE %s"
          ),
          q_relation(table_name),
          q_ident(constraint_name),
          q_ident_list(columns),
          q_relation(referenced_table),
          q_ident_list(referenced_columns),
          on_update,
          on_delete
        )
      )

      invisible(NULL)
    }

    grant_table_privileges <- function(relation_name, privileges, roles) {
      if (!length(roles)) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT %s ON TABLE %s TO %s",
            privileges,
            q_relation(relation_name),
            q_role(role_name)
          )
        )
      }

      invisible(NULL)
    }

    grant_sequence_privileges <- function(relation_name, column_name, roles) {
      if (!length(roles)) {
        return(invisible(NULL))
      }

      sequence_name <- DBI::dbGetQuery(
        con,
        "SELECT pg_get_serial_sequence($1, $2) AS sequence_name",
        params = list(relation_name, column_name)
      )$sequence_name[[1]]

      if (is.na(sequence_name) || !nzchar(sequence_name)) {
        return(invisible(NULL))
      }

      for (role_name in roles) {
        DBI::dbExecute(
          con,
          sprintf(
            "GRANT USAGE, SELECT ON SEQUENCE %s TO %s",
            q_relation(sequence_name),
            q_role(role_name)
          )
        )
      }

      invisible(NULL)
    }

    instrument_read_roles <- intersect(
      c("public_reader", "yg_reader_group", "tkc_group"),
      existing_roles()
    )
    instrument_write_roles <- intersect(
      c("yg_editor_group"),
      existing_roles()
    )
    instrument_admin_roles <- intersect(
      c("admin", "tkc_editor"),
      existing_roles()
    )

    current_due_count <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::integer AS n
       FROM instruments.instrument_maintenance
       WHERE date_maintenance_due IS NOT NULL"
    )$n[[1]]

    legacy_sensor_event_count <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::integer AS n
       FROM instruments.array_maintenance_changes"
    )$n[[1]]

    legacy_sensor_slot_count <- DBI::dbGetQuery(
      con,
      "SELECT count(*)::integer AS n
       FROM instruments.array_maintenance_changes amc
       CROSS JOIN LATERAL (
         VALUES
           (amc.sensor1_id, amc.sensor1_notes),
           (amc.sensor2_id, amc.sensor2_notes),
           (amc.sensor3_id, amc.sensor3_notes),
           (amc.sensor4_id, amc.sensor4_notes),
           (amc.sensor5_id, amc.sensor5_notes),
           (amc.sensor6_id, amc.sensor6_notes),
           (amc.sensor7_id, amc.sensor7_notes),
           (amc.sensor8_id, amc.sensor8_notes)
       ) slot(sensor_id, note)
       WHERE slot.sensor_id IS NOT NULL
          OR NULLIF(btrim(slot.note), '') IS NOT NULL"
    )$n[[1]]

    message("Hardening instrument history linkages...")
    replace_fk_constraint(
      "instruments.instruments",
      "fk_instrument_owner",
      "owner",
      "public.organizations",
      "organization_id"
    )
    replace_fk_constraint(
      "instruments.instruments",
      "instruments_observer_fkey",
      "observer",
      "instruments.observers",
      "observer_id"
    )
    replace_fk_constraint(
      "instruments.instruments",
      "instruments_make_fkey",
      "make",
      "instruments.instrument_make",
      "make_id"
    )
    replace_fk_constraint(
      "instruments.instruments",
      "instruments_model_fkey",
      "model",
      "instruments.instrument_model",
      "model_id"
    )
    replace_fk_constraint(
      "instruments.instruments",
      "instruments_type_fkey",
      "type",
      "instruments.instrument_type",
      "type_id"
    )
    replace_fk_constraint(
      "instruments.sensors",
      "sensors_sensor_type_fkey",
      "sensor_type",
      "instruments.sensor_types",
      "sensor_type_id"
    )
    replace_fk_constraint(
      "instruments.calibrations",
      "calibrations_observer_fkey",
      "observer",
      "instruments.observers",
      "observer_id"
    )
    replace_fk_constraint(
      "instruments.calibrations",
      "calibrations_id_sensor_holder_fkey",
      "id_sensor_holder",
      "instruments.instruments",
      "instrument_id"
    )
    replace_fk_constraint(
      "instruments.calibrations",
      "calibrations_id_handheld_meter_fkey",
      "id_handheld_meter",
      "instruments.instruments",
      "instrument_id"
    )
    replace_fk_constraint(
      "instruments.instrument_maintenance",
      "instrument_maintenance_instrument_id_fkey",
      "instrument_id",
      "instruments.instruments",
      "instrument_id"
    )
    replace_fk_constraint(
      "instruments.instrument_maintenance",
      "instrument_maintenance_observer_fkey",
      "observer",
      "instruments.observers",
      "observer_id"
    )
    replace_fk_constraint(
      "public.locations_metadata_instruments",
      "fk_loc_meta_instrument",
      "instrument_id",
      "instruments.instruments",
      "instrument_id"
    )
    replace_fk_constraint(
      "field.field_visit_instruments",
      "field_visit_instruments_instrument_id_fkey",
      "instrument_id",
      "instruments.instruments",
      "instrument_id"
    )

    if (relation_exists("public.locations_metadata_xsections")) {
      if (
        column_exists("public", "locations_metadata_xsections", "instrument") &&
          !column_exists(
            "public",
            "locations_metadata_xsections",
            "instrument_id"
          )
      ) {
        DBI::dbExecute(
          con,
          "ALTER TABLE public.locations_metadata_xsections
           RENAME COLUMN instrument TO instrument_id"
        )
      }

      DBI::dbExecute(
        con,
        "ALTER TABLE public.locations_metadata_xsections
         DROP CONSTRAINT IF EXISTS locations_metadata_xsections_instrument_fkey"
      )
      replace_fk_constraint(
        "public.locations_metadata_xsections",
        "locations_metadata_xsections_instrument_id_fkey",
        "instrument_id",
        "instruments.instruments",
        "instrument_id"
      )
    }

    message("Adding deployment overlap constraints...")
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
       DROP CONSTRAINT IF EXISTS no_overlap_instrument_deployment"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
       ADD CONSTRAINT no_overlap_instrument_deployment
       EXCLUDE USING gist (
         instrument_id WITH =,
         tstzrange(
           start_datetime,
           COALESCE(end_datetime, 'infinity'::timestamp with time zone),
           '[)'::text
         ) WITH &&
       )
       DEFERRABLE"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instruments
       DROP CONSTRAINT IF EXISTS no_overlap_timeseries_deployment"
    )

    message("Normalizing deployment-timeseries associations...")
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.locations_metadata_instrument_timeseries (
         deployment_timeseries_id INTEGER
           PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
         metadata_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_instruments(metadata_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMP WITH TIME ZONE,
         CONSTRAINT locations_metadata_instrument_timeseries_unique
           UNIQUE (metadata_id, timeseries_id),
         CONSTRAINT locations_metadata_instrument_timeseries_note_chk
           CHECK (note IS NULL OR btrim(note) <> '')
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS
         locations_metadata_instrument_timeseries_tsid_idx
       ON public.locations_metadata_instrument_timeseries
       (timeseries_id, metadata_id)"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_user_audit
       ON public.locations_metadata_instrument_timeseries"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON public.locations_metadata_instrument_timeseries
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_modify_time
       ON public.locations_metadata_instrument_timeseries"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON public.locations_metadata_instrument_timeseries
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_instrument_timeseries IS
       'Many-to-many association between instrument deployment metadata records and continuous timeseries. This table supports deployments where one deployed instrument populates more than one timeseries.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_timeseries.metadata_id IS
       'Instrument deployment metadata record in public.locations_metadata_instruments.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_timeseries.timeseries_id IS
       'Continuous timeseries associated with the instrument deployment.'"
    )
    if (
      column_exists("public", "locations_metadata_instruments", "timeseries_id")
    ) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.locations_metadata_instrument_timeseries (
           metadata_id,
           timeseries_id,
           created_by,
           modified_by,
           created,
           modified
         )
         SELECT
           metadata_id,
           timeseries_id,
           created_by,
           modified_by,
           created,
           modified
         FROM public.locations_metadata_instruments
         WHERE timeseries_id IS NOT NULL
         ON CONFLICT (metadata_id, timeseries_id) DO NOTHING"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE public.locations_metadata_instruments
         DROP COLUMN timeseries_id"
      )
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION
         public.check_instrument_connection_signal_timeseries()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       DECLARE
         connection_row RECORD;
         instrument_row RECORD;
         timeseries_row RECORD;
       BEGIN
         IF NEW.timeseries_id IS NULL THEN
           RETURN NEW;
         END IF;

         SELECT *
         INTO connection_row
         FROM public.locations_metadata_instrument_connections
         WHERE connection_id = NEW.connection_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Connection % does not exist.', NEW.connection_id;
         END IF;

         SELECT *
         INTO instrument_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = connection_row.instrument_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Instrument deployment % for connection % does not exist.',
             connection_row.instrument_metadata_id,
             NEW.connection_id;
         END IF;

         SELECT timeseries_id, parameter_id, location_id, sub_location_id, z_id
         INTO timeseries_row
         FROM continuous.timeseries
         WHERE timeseries_id = NEW.timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Timeseries % does not exist.', NEW.timeseries_id;
         END IF;

         IF timeseries_row.location_id <> instrument_row.location_id OR
            timeseries_row.sub_location_id IS DISTINCT FROM
              instrument_row.sub_location_id THEN
           RAISE EXCEPTION
             'Timeseries % must belong to the same location/sub-location as the communicating instrument deployment %.',
             NEW.timeseries_id,
             connection_row.instrument_metadata_id;
         END IF;

         IF instrument_row.z_id IS NOT NULL AND
            timeseries_row.z_id IS DISTINCT FROM instrument_row.z_id THEN
           RAISE EXCEPTION
             'Timeseries % z_id must match the communicating instrument deployment z_id when the deployment z_id is populated.',
             NEW.timeseries_id;
         END IF;

         IF NEW.parameter_id IS NOT NULL AND
            timeseries_row.parameter_id IS DISTINCT FROM NEW.parameter_id THEN
           RAISE EXCEPTION
             'Signal parameter_id % does not match continuous.timeseries.parameter_id % for timeseries %.',
             NEW.parameter_id,
             timeseries_row.parameter_id,
             NEW.timeseries_id;
         END IF;

         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connection_signals s
           JOIN public.locations_metadata_instrument_connections c
             ON c.connection_id = s.connection_id
           WHERE s.connection_signal_id <> NEW.connection_signal_id
             AND s.timeseries_id = NEW.timeseries_id
             AND connection_row.start_datetime <
               COALESCE(c.end_datetime, 'infinity'::timestamptz)
             AND COALESCE(connection_row.end_datetime, 'infinity'::timestamptz) >
               c.start_datetime
         ) THEN
           RAISE EXCEPTION
             'Timeseries % is already linked to another overlapping instrument connection signal.',
             NEW.timeseries_id;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION
         public.check_locations_metadata_instruments_acquisition_dependents()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           JOIN public.locations_metadata_instruments i
             ON i.metadata_id = c.instrument_metadata_id
           JOIN public.locations_metadata_instruments l
             ON l.metadata_id = c.logger_metadata_id
           JOIN instruments.communication_protocols cp
             ON cp.protocol_id = c.protocol_id
           JOIN instruments.communication_protocol_families cpf
             ON cpf.protocol_family_id = cp.protocol_family_id
           LEFT JOIN instruments.instruments li
             ON li.instrument_id = l.instrument_id
           WHERE (c.instrument_metadata_id = NEW.metadata_id OR
                  c.logger_metadata_id = NEW.metadata_id)
             AND (
               i.instrument_id IS NULL OR
               l.instrument_id IS NULL OR
               li.can_be_logger IS DISTINCT FROM TRUE OR
               (cpf.family_code = 'internal' AND
                  c.instrument_metadata_id <> c.logger_metadata_id) OR
               (cpf.family_code <> 'internal' AND
                  c.instrument_metadata_id = c.logger_metadata_id) OR
               i.location_id <> l.location_id OR
               c.start_datetime < i.start_datetime OR
               c.start_datetime < l.start_datetime OR
               COALESCE(c.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(i.end_datetime, 'infinity'::timestamptz) OR
               COALESCE(c.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(l.end_datetime, 'infinity'::timestamptz)
             )
         ) THEN
           RAISE EXCEPTION
             'Updating instrument deployment % would invalidate existing instrument/logger connection metadata.',
             NEW.metadata_id;
         END IF;

         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           JOIN public.locations_metadata_instrument_connection_signals s
             ON s.connection_id = c.connection_id
           JOIN continuous.timeseries ts
             ON ts.timeseries_id = s.timeseries_id
           WHERE c.instrument_metadata_id = NEW.metadata_id
             AND (
               ts.location_id <> NEW.location_id OR
               ts.sub_location_id IS DISTINCT FROM NEW.sub_location_id OR
               (NEW.z_id IS NOT NULL AND
                  ts.z_id IS DISTINCT FROM NEW.z_id) OR
               (s.parameter_id IS NOT NULL AND
                  ts.parameter_id IS DISTINCT FROM s.parameter_id)
             )
         ) THEN
           RAISE EXCEPTION
             'Updating instrument deployment % would invalidate existing signal-to-timeseries mappings.',
             NEW.metadata_id;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION
         public.check_locations_metadata_instrument_timeseries_overlap()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_timeseries lmit_a
           INNER JOIN public.locations_metadata_instruments lmi_a
             ON lmi_a.metadata_id = lmit_a.metadata_id
           INNER JOIN public.locations_metadata_instrument_timeseries lmit_b
             ON lmit_b.timeseries_id = lmit_a.timeseries_id
            AND lmit_b.metadata_id <> lmit_a.metadata_id
           INNER JOIN public.locations_metadata_instruments lmi_b
             ON lmi_b.metadata_id = lmit_b.metadata_id
           WHERE tstzrange(
               lmi_a.start_datetime,
               COALESCE(
                 lmi_a.end_datetime,
                 'infinity'::timestamp with time zone
               ),
               '[)'::text
             ) && tstzrange(
               lmi_b.start_datetime,
               COALESCE(
                 lmi_b.end_datetime,
                 'infinity'::timestamp with time zone
               ),
               '[)'::text
             )
         ) THEN
           RAISE EXCEPTION
             'A timeseries cannot be associated with overlapping instrument deployments';
         END IF;

         RETURN NULL;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS
         check_locations_metadata_instrument_timeseries_overlap
       ON public.locations_metadata_instrument_timeseries"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER
         check_locations_metadata_instrument_timeseries_overlap
       AFTER INSERT OR UPDATE ON public.locations_metadata_instrument_timeseries
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION
         public.check_locations_metadata_instrument_timeseries_overlap()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS
         check_locations_metadata_instrument_timeseries_period_overlap
       ON public.locations_metadata_instruments"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER
         check_locations_metadata_instrument_timeseries_period_overlap
       AFTER UPDATE OF start_datetime, end_datetime
       ON public.locations_metadata_instruments
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION
         public.check_locations_metadata_instrument_timeseries_overlap()"
    )
    grant_table_privileges(
      "public.locations_metadata_instrument_timeseries",
      "SELECT",
      instrument_read_roles
    )
    grant_table_privileges(
      "public.locations_metadata_instrument_timeseries",
      "SELECT, INSERT, UPDATE, DELETE",
      union(instrument_write_roles, instrument_admin_roles)
    )
    grant_sequence_privileges(
      "public.locations_metadata_instrument_timeseries",
      "deployment_timeseries_id",
      union(instrument_write_roles, instrument_admin_roles)
    )

    message("Separating instrument maintenance due state from event history...")
    DBI::dbExecute(
      con,
      "CREATE SEQUENCE IF NOT EXISTS
         instruments.instrument_maintenance_due_maintenance_due_id_seq"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.instrument_maintenance_due (
         maintenance_due_id INTEGER PRIMARY KEY
           DEFAULT nextval('instruments.instrument_maintenance_due_maintenance_due_id_seq'::regclass),
         instrument_id INTEGER NOT NULL
           REFERENCES instruments.instruments(instrument_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         observer INTEGER NOT NULL
           REFERENCES instruments.observers(observer_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
         date_maintenance_due DATE NOT NULL,
         maintenance_due_note TEXT NOT NULL,
         completed_event_id INTEGER,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMP WITH TIME ZONE,
         CONSTRAINT instrument_maintenance_due_note_not_blank
           CHECK (btrim(maintenance_due_note) <> '')
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_maintenance_due
       ADD COLUMN IF NOT EXISTS maintenance_due_id INTEGER"
    )
    DBI::dbExecute(
      con,
      "ALTER SEQUENCE
         instruments.instrument_maintenance_due_maintenance_due_id_seq
       OWNED BY instruments.instrument_maintenance_due.maintenance_due_id"
    )
    DBI::dbExecute(
      con,
      "UPDATE instruments.instrument_maintenance_due
       SET maintenance_due_id = nextval('instruments.instrument_maintenance_due_maintenance_due_id_seq'::regclass)
       WHERE maintenance_due_id IS NULL"
    )
    DBI::dbExecute(
      con,
      "SELECT setval(
         'instruments.instrument_maintenance_due_maintenance_due_id_seq'::regclass,
         COALESCE((
           SELECT max(maintenance_due_id)
           FROM instruments.instrument_maintenance_due
         ), 1),
         (
           SELECT max(maintenance_due_id) IS NOT NULL
           FROM instruments.instrument_maintenance_due
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_maintenance_due
       ALTER COLUMN maintenance_due_id
       SET DEFAULT nextval('instruments.instrument_maintenance_due_maintenance_due_id_seq'::regclass)"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_maintenance_due
       ALTER COLUMN maintenance_due_id SET NOT NULL"
    )
    DBI::dbExecute(
      con,
      "DO $$
       DECLARE pk_cols text[];
       BEGIN
         SELECT array_agg(a.attname ORDER BY a.attnum)
         INTO pk_cols
         FROM pg_constraint c
         JOIN unnest(c.conkey) AS k(attnum) ON TRUE
         JOIN pg_attribute a
           ON a.attrelid = c.conrelid
          AND a.attnum = k.attnum
         WHERE c.conrelid = 'instruments.instrument_maintenance_due'::regclass
           AND c.contype = 'p';

         IF pk_cols IS NOT NULL
            AND pk_cols <> ARRAY['maintenance_due_id'] THEN
           ALTER TABLE instruments.instrument_maintenance_due
           DROP CONSTRAINT instrument_maintenance_due_pkey;
         END IF;
       END $$"
    )
    DBI::dbExecute(
      con,
      "DO $$
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'instruments.instrument_maintenance_due'::regclass
             AND contype = 'p'
         ) THEN
           ALTER TABLE instruments.instrument_maintenance_due
           ADD CONSTRAINT instrument_maintenance_due_pkey
           PRIMARY KEY (maintenance_due_id);
         END IF;
       END $$"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_maintenance_due
       ADD COLUMN IF NOT EXISTS completed_event_id INTEGER"
    )
    replace_fk_constraint(
      "instruments.instrument_maintenance_due",
      "instrument_maintenance_due_completed_event_id_fkey",
      "completed_event_id",
      "instruments.instrument_maintenance",
      "event_id",
      on_delete = "SET NULL"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_user_audit
       ON instruments.instrument_maintenance_due"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON instruments.instrument_maintenance_due
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_modify_time
       ON instruments.instrument_maintenance_due"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON instruments.instrument_maintenance_due
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.instrument_maintenance_due IS
       'Scheduled maintenance due items for instruments. Due items remain open until linked to a completed maintenance event.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.instrument_maintenance_due.maintenance_due_id IS
       'Unique identifier for one scheduled instrument maintenance due item.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.instrument_maintenance_due.obs_datetime IS
       'Date and time when the due-maintenance item was observed or scheduled.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.instrument_maintenance_due.completed_event_id IS
       'Completed maintenance event that satisfied this due item. NULL means still open.'"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.instrument_maintenance_due (
         instrument_id,
         observer,
         obs_datetime,
         date_maintenance_due,
         maintenance_due_note,
         created_by,
         modified_by,
         created,
         modified
       )
       SELECT
         im.instrument_id,
         im.observer,
         im.obs_datetime,
         im.date_maintenance_due,
         im.note,
         im.created_by,
         im.modified_by,
         im.created,
         im.modified
       FROM instruments.instrument_maintenance AS im
       WHERE im.date_maintenance_due IS NOT NULL
         AND NOT EXISTS (
           SELECT 1
           FROM instruments.instrument_maintenance_due AS due
           WHERE due.instrument_id = im.instrument_id
             AND due.obs_datetime = im.obs_datetime
             AND due.date_maintenance_due = im.date_maintenance_due
             AND due.maintenance_due_note IS NOT DISTINCT FROM im.note
         )
       ORDER BY
         im.instrument_id,
         im.date_maintenance_due,
         im.obs_datetime DESC,
         im.event_id DESC"
    )

    DBI::dbExecute(
      con,
      "DELETE FROM instruments.instrument_maintenance
       WHERE date_maintenance_due IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_maintenance
       DROP COLUMN IF EXISTS date_maintenance_due"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS instrument_maintenance_instrument_datetime_idx
       ON instruments.instrument_maintenance
       (instrument_id, obs_datetime DESC, event_id DESC)"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS instruments.instrument_maintenance_due_date_idx"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS instrument_maintenance_due_active_idx
       ON instruments.instrument_maintenance_due
       (instrument_id, date_maintenance_due, maintenance_due_id)
       WHERE completed_event_id IS NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS instrument_maintenance_due_completed_event_idx
       ON instruments.instrument_maintenance_due
       (completed_event_id)
       WHERE completed_event_id IS NOT NULL"
    )

    message("Creating normalized instrument sensor maintenance tables...")
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.instrument_sensor_events (
         event_id INTEGER PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
         instrument_id INTEGER NOT NULL
           REFERENCES instruments.instruments(instrument_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         observer INTEGER NOT NULL
           REFERENCES instruments.observers(observer_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
         event_note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMP WITH TIME ZONE
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.instrument_sensor_event_slots (
         event_id INTEGER NOT NULL
           REFERENCES instruments.instrument_sensor_events(event_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         slot_number INTEGER NOT NULL,
         sensor_id INTEGER
           REFERENCES instruments.sensors(sensor_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMP WITH TIME ZONE,
         PRIMARY KEY (event_id, slot_number),
         CONSTRAINT instrument_sensor_event_slots_slot_number_chk
           CHECK (slot_number > 0),
         CONSTRAINT instrument_sensor_event_slots_note_not_blank_chk
           CHECK (note IS NULL OR btrim(note) <> ''),
         CONSTRAINT instrument_sensor_event_slots_value_chk
           CHECK (
             sensor_id IS NOT NULL OR
               NULLIF(btrim(note), '') IS NOT NULL
           )
       )"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_user_audit
       ON instruments.instrument_sensor_events"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON instruments.instrument_sensor_events
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_modify_time
       ON instruments.instrument_sensor_events"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON instruments.instrument_sensor_events
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_user_audit
       ON instruments.instrument_sensor_event_slots"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON instruments.instrument_sensor_event_slots
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_modify_time
       ON instruments.instrument_sensor_event_slots"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON instruments.instrument_sensor_event_slots
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.instrument_sensor_events IS
       'Header table for changes or maintenance observations on replaceable instrument sensor slots.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.instrument_sensor_event_slots IS
       'One row per sensor slot state recorded during an instrument sensor maintenance event.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.instrument_sensor_event_slots.slot_number IS
       'Ordinal sensor slot number on the parent instrument.'"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.instrument_sensor_events (
         event_id,
         instrument_id,
         observer,
         obs_datetime,
         created_by,
         modified_by,
         created,
         modified
       )
       SELECT
         event_id,
         instrument_id,
         observer,
         obs_datetime,
         created_by,
         modified_by,
         created,
         modified
       FROM instruments.array_maintenance_changes
       ON CONFLICT (event_id) DO UPDATE
       SET instrument_id = EXCLUDED.instrument_id,
           observer = EXCLUDED.observer,
           obs_datetime = EXCLUDED.obs_datetime,
           modified_by = CURRENT_USER,
           modified = CURRENT_TIMESTAMP"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO instruments.instrument_sensor_event_slots (
         event_id,
         slot_number,
         sensor_id,
         note,
         created_by,
         modified_by,
         created,
         modified
       )
       SELECT
         amc.event_id,
         slot.slot_number,
         slot.sensor_id,
         NULLIF(btrim(slot.note), ''),
         amc.created_by,
         amc.modified_by,
         amc.created,
         amc.modified
       FROM instruments.array_maintenance_changes amc
       CROSS JOIN LATERAL (
         VALUES
           (1, amc.sensor1_id, amc.sensor1_notes),
           (2, amc.sensor2_id, amc.sensor2_notes),
           (3, amc.sensor3_id, amc.sensor3_notes),
           (4, amc.sensor4_id, amc.sensor4_notes),
           (5, amc.sensor5_id, amc.sensor5_notes),
           (6, amc.sensor6_id, amc.sensor6_notes),
           (7, amc.sensor7_id, amc.sensor7_notes),
         (8, amc.sensor8_id, amc.sensor8_notes)
       ) slot(slot_number, sensor_id, note)
       WHERE slot.sensor_id IS NOT NULL
          OR NULLIF(btrim(slot.note), '') IS NOT NULL
       ON CONFLICT (event_id, slot_number) DO UPDATE
       SET sensor_id = EXCLUDED.sensor_id,
           note = EXCLUDED.note,
           modified_by = CURRENT_USER,
           modified = CURRENT_TIMESTAMP"
    )
    DBI::dbExecute(
      con,
      "SELECT setval(
         pg_get_serial_sequence(
           'instruments.instrument_sensor_events',
           'event_id'
         ),
         GREATEST(
           COALESCE(
             (
               SELECT max(event_id)
               FROM instruments.instrument_sensor_events
             ),
             1
           ),
           1
         ),
         EXISTS (
           SELECT 1
           FROM instruments.instrument_sensor_events
         )
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS instrument_sensor_events_instrument_datetime_idx
       ON instruments.instrument_sensor_events
       (instrument_id, obs_datetime DESC, event_id DESC)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS instrument_sensor_event_slots_sensor_idx
       ON instruments.instrument_sensor_event_slots
       (sensor_id)
       WHERE sensor_id IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW instruments.instrument_sensor_current
       WITH (security_invoker = true, security_barrier = true)
       AS
       SELECT DISTINCT ON (events.instrument_id, slots.slot_number)
         events.instrument_id,
         slots.slot_number,
         slots.sensor_id,
         sensors.sensor_serial,
         sensors.sensor_type,
         slots.note,
         events.event_id,
         events.observer,
         events.obs_datetime
       FROM instruments.instrument_sensor_events events
       JOIN instruments.instrument_sensor_event_slots slots
         ON events.event_id = slots.event_id
       LEFT JOIN instruments.sensors sensors
         ON slots.sensor_id = sensors.sensor_id
       ORDER BY
         events.instrument_id,
         slots.slot_number,
         events.obs_datetime DESC,
         events.event_id DESC"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON VIEW instruments.instrument_sensor_current IS
       'Latest recorded sensor assignment for each instrument sensor slot.'"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION instruments.assert_sensor_current_assignment_is_unique(
         p_sensor_id INTEGER
       )
       RETURNS VOID
       LANGUAGE plpgsql
       AS $$
       DECLARE
         assignment_count INTEGER;
         assignment_summary TEXT;
         sensor_label TEXT;
       BEGIN
         IF p_sensor_id IS NULL THEN
           RETURN;
         END IF;

         SELECT sensor_serial
         INTO sensor_label
         FROM instruments.sensors
         WHERE sensor_id = p_sensor_id;

         IF upper(btrim(COALESCE(sensor_label, ''))) = 'BLANK' THEN
           RETURN;
         END IF;

         SELECT count(*),
                string_agg(
                  format(
                    'instrument %s slot %s',
                    instrument_id,
                    slot_number
                  ),
                  ', '
                  ORDER BY instrument_id, slot_number
                )
         INTO assignment_count, assignment_summary
         FROM instruments.instrument_sensor_current
         WHERE sensor_id = p_sensor_id;

         IF assignment_count > 1 THEN
           RAISE EXCEPTION
             'Sensor % is currently assigned to multiple instrument slots: %',
             p_sensor_id,
             assignment_summary
             USING
               ERRCODE = '23505',
               HINT = 'Record a removal event for the old slot before assigning the sensor to a new instrument slot.';
         END IF;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION instruments.check_sensor_current_assignment_slot()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         PERFORM instruments.assert_sensor_current_assignment_is_unique(NEW.sensor_id);
         IF TG_OP = 'UPDATE' THEN
           PERFORM instruments.assert_sensor_current_assignment_is_unique(OLD.sensor_id);
         END IF;
         RETURN NEW;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION instruments.check_sensor_current_assignment_event()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         slot_sensor_id INTEGER;
       BEGIN
         FOR slot_sensor_id IN
           SELECT DISTINCT sensor_id
           FROM instruments.instrument_sensor_event_slots
           WHERE event_id IN (NEW.event_id, OLD.event_id)
             AND sensor_id IS NOT NULL
         LOOP
           PERFORM instruments.assert_sensor_current_assignment_is_unique(
             slot_sensor_id
           );
         END LOOP;
         RETURN NEW;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_sensor_current_assignment_slot
       ON instruments.instrument_sensor_event_slots"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_sensor_current_assignment_slot
       AFTER INSERT OR UPDATE OF sensor_id, slot_number
       ON instruments.instrument_sensor_event_slots
       FOR EACH ROW
       EXECUTE FUNCTION instruments.check_sensor_current_assignment_slot()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_sensor_current_assignment_event
       ON instruments.instrument_sensor_events"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_sensor_current_assignment_event
       AFTER UPDATE OF instrument_id, obs_datetime
       ON instruments.instrument_sensor_events
       FOR EACH ROW
       EXECUTE FUNCTION instruments.check_sensor_current_assignment_event()"
    )

    new_instrument_tables <- c(
      "instruments.instrument_maintenance_due",
      "instruments.instrument_sensor_events",
      "instruments.instrument_sensor_event_slots"
    )
    for (relation_name in new_instrument_tables) {
      grant_table_privileges(relation_name, "SELECT", instrument_read_roles)
      grant_table_privileges(
        relation_name,
        "SELECT, INSERT, UPDATE, DELETE",
        instrument_write_roles
      )
      grant_table_privileges(
        relation_name,
        "ALL PRIVILEGES",
        instrument_admin_roles
      )
    }
    grant_table_privileges(
      "instruments.instrument_sensor_current",
      "SELECT",
      unique(c(
        instrument_read_roles,
        instrument_write_roles,
        instrument_admin_roles
      ))
    )
    grant_sequence_privileges(
      "instruments.instrument_maintenance_due",
      "maintenance_due_id",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )
    grant_sequence_privileges(
      "instruments.instrument_sensor_events",
      "event_id",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )

    message("Checking instrument maintenance schema cleanup...")
    maintenance_schema_check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('instruments.instrument_maintenance_due') IS NOT NULL
           AS has_due_table,
         to_regclass('instruments.instrument_sensor_events') IS NOT NULL
           AS has_sensor_events,
         to_regclass('instruments.instrument_sensor_event_slots') IS NOT NULL
           AS has_sensor_event_slots,
         to_regclass('instruments.instrument_sensor_current') IS NOT NULL
           AS has_sensor_current_view,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'instruments'
             AND table_name = 'instrument_maintenance'
             AND column_name = 'date_maintenance_due'
         ) AS maintenance_due_removed,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'instruments'
             AND table_name = 'instrument_maintenance_due'
             AND column_name = 'maintenance_due_id'
         ) AS has_due_item_id,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'instruments'
             AND table_name = 'instrument_maintenance_due'
             AND column_name = 'completed_event_id'
         ) AS has_due_completion_link,
         (
           SELECT count(*)::integer
           FROM instruments.instrument_maintenance_due
         ) = $1 AS due_count_matches,
         (
           SELECT count(*)::integer
           FROM instruments.instrument_sensor_events
         ) >= $2 AS sensor_event_count_matches,
         (
           SELECT count(*)::integer
           FROM instruments.instrument_sensor_event_slots
         ) >= $3 AS sensor_slot_count_matches",
      params = list(
        current_due_count,
        legacy_sensor_event_count,
        legacy_sensor_slot_count
      )
    )

    if (!all(unlist(maintenance_schema_check[1, ]))) {
      stop(
        "Patch 44 verification failed: instrument maintenance schema cleanup did not produce the expected tables, migrated row counts, or column removal."
      )
    }

    # Drop old table 'array_maintenance_changes' if the new tables were created and data was migrated
    if (
      maintenance_schema_check$has_due_table &&
        maintenance_schema_check$has_sensor_events &&
        maintenance_schema_check$has_sensor_event_slots &&
        maintenance_schema_check$has_sensor_current_view
    ) {
      DBI::dbExecute(
        con,
        "DROP TABLE IF EXISTS instruments.array_maintenance_changes"
      )
    }

    message("Checking hardened instrument linkages...")
    instrument_hardening_check <- DBI::dbGetQuery(
      con,
      "WITH expected_restrict_fk(conname) AS (
         VALUES
           ('fk_instrument_owner'),
           ('instruments_observer_fkey'),
           ('instruments_make_fkey'),
           ('instruments_model_fkey'),
           ('instruments_type_fkey'),
           ('sensors_sensor_type_fkey'),
           ('calibrations_observer_fkey'),
           ('calibrations_id_sensor_holder_fkey'),
           ('calibrations_id_handheld_meter_fkey'),
           ('instrument_maintenance_instrument_id_fkey'),
           ('instrument_maintenance_observer_fkey'),
           ('instrument_maintenance_due_instrument_id_fkey'),
           ('instrument_maintenance_due_observer_fkey'),
           ('instrument_sensor_events_instrument_id_fkey'),
           ('instrument_sensor_events_observer_fkey'),
           ('instrument_sensor_event_slots_sensor_id_fkey'),
           ('fk_loc_meta_instrument'),
           ('field_visit_instruments_instrument_id_fkey'),
           ('locations_metadata_xsections_instrument_id_fkey')
       )
       SELECT
         to_regclass('instruments.array_maintenance_changes') IS NULL
           AS old_array_table_removed,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'public.locations_metadata_instruments'::regclass
             AND conname = 'no_overlap_instrument_deployment'
             AND contype = 'x'
         ) AS has_instrument_deployment_overlap_constraint,
          NOT EXISTS (
            SELECT 1
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name = 'locations_metadata_instruments'
              AND column_name = 'timeseries_id'
          ) AS legacy_deployment_timeseries_column_removed,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'locations_metadata_xsections'
             AND column_name = 'instrument'
         ) AS xsection_old_instrument_name_removed,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'locations_metadata_xsections'
             AND column_name = 'instrument_id'
         ) AS xsection_instrument_id_exists,
         (
           SELECT count(DISTINCT c.conname)
           FROM pg_constraint c
           JOIN expected_restrict_fk e
             ON c.conname = e.conname
           WHERE c.contype = 'f'
             AND c.confdeltype = 'r'
         ) = (
           SELECT count(*)
           FROM expected_restrict_fk
         ) AS expected_foreign_keys_are_restrict,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid =
             'instruments.instrument_sensor_event_slots'::regclass
             AND conname = 'instrument_sensor_event_slots_note_not_blank_chk'
             AND contype = 'c'
         ) AS has_sensor_slot_blank_note_check,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid =
             'instruments.instrument_sensor_event_slots'::regclass
             AND conname = 'instrument_sensor_event_slots_value_chk'
             AND contype = 'c'
         ) AS has_sensor_slot_value_check,
         EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid =
             'instruments.instrument_maintenance_due'::regclass
             AND conname = 'instrument_maintenance_due_completed_event_id_fkey'
             AND contype = 'f'
             AND confdeltype = 'n'
         ) AS has_due_completion_fk"
    )

    if (!all(unlist(instrument_hardening_check[1, ]))) {
      stop(
        "Patch 44 verification failed: instrument linkage hardening did not produce the expected constraints, column rename, or table removal."
      )
    }

    # Normalize instruments.sensors
    # Make new table of sensor makes and pre-populate with the values found in instruments.sensors.sensor_make AND the existing instruments.instrument_make table, matching on the text value of the make, ignoring case and whitespace, and preferring matches to existing instrument makes when available
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.sensor_makes (
         make_id INTEGER PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
         make TEXT UNIQUE NOT NULL,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMP WITH TIME ZONE
       )"
    )
    # triggers on modified cols
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_user_audit
       ON instruments.sensor_makes"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON instruments.sensor_makes
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_modify_time
       ON instruments.sensor_makes"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON instruments.sensor_makes
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    # Insert data
    DBI::dbExecute(
      con,
      "INSERT INTO instruments.sensor_makes (make, description, created_by)
       SELECT DISTINCT sensor_make, NULL, created_by
       FROM instruments.sensors
       WHERE sensor_make IS NOT NULL
         AND sensor_make <> ''
       ON CONFLICT (make) DO NOTHING"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO instruments.sensor_makes (make, description, created_by)
       SELECT DISTINCT make, description, created_by
       FROM instruments.instrument_make
        WHERE make IS NOT NULL
          AND make <> ''
        ON CONFLICT (make) DO NOTHING"
    )
    # There will be an entry for 'Ott' and one for 'OTT', delete the one for 'Ott'
    DBI::dbExecute(
      con,
      "DELETE FROM instruments.sensor_makes
       WHERE make = 'Ott'"
    )
    # switch out the text strings in instruments.sensors.sensor_make to the corresponding make_id from instruments.sensor_makes, matching on the text value of the make, ignoring case and whitespace
    DBI::dbExecute(
      con,
      "UPDATE instruments.sensors s
       SET sensor_make = sm.make_id::text
       FROM instruments.sensor_makes sm
       WHERE lower(btrim(s.sensor_make)) = lower(btrim(sm.make))
         AND s.sensor_make IS NOT NULL
         AND sm.make_id IS NOT NULL"
    )
    # Now alter the column to be an integer fk to instruments.instrument_make.make_id
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.sensors
       ALTER COLUMN sensor_make TYPE INTEGER
       USING sensor_make::INTEGER"
    )
    replace_fk_constraint(
      "instruments.sensors",
      "sensors_sensor_make_fkey",
      "sensor_make",
      "instruments.sensor_makes",
      "make_id",
      on_delete = "RESTRICT"
    )

    # Grant privileges on new sensor_makes table
    grant_table_privileges(
      "instruments.sensor_makes",
      "SELECT",
      unique(c(
        instrument_read_roles,
        instrument_write_roles,
        instrument_admin_roles
      ))
    )
    grant_table_privileges(
      "instruments.sensor_makes",
      "INSERT, UPDATE, DELETE",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )
    grant_sequence_privileges(
      "instruments.sensor_makes",
      "make_id",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )

    # Create new table 'instruments.sensor_models' and pre-populate with the values found in instruments.sensors.sensor_model
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.sensor_models (
         model_id INTEGER PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
         model TEXT UNIQUE NOT NULL,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMP WITH TIME ZONE
       )"
    )

    # Add triggers for modification audit on instruments.sensor_models
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS trg_user_audit
       ON instruments.sensor_models"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER trg_user_audit
       BEFORE UPDATE ON instruments.sensor_models
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS update_modify_time
       ON instruments.sensor_models"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER update_modify_time
       BEFORE UPDATE ON instruments.sensor_models
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )

    # Now populate the table
    DBI::dbExecute(
      con,
      "INSERT INTO instruments.sensor_models (model, description, created_by)
       SELECT DISTINCT sensor_model, NULL, CURRENT_USER
       FROM instruments.sensors
       WHERE sensor_model IS NOT NULL
         AND sensor_model <> ''"
    )
    # switch out the text strings in instruments.sensors.sensor_model to the corresponding model_id from instruments.sensor_models, matching on the text value of the model, ignoring case and whitespace
    DBI::dbExecute(
      con,
      "UPDATE instruments.sensors s
       SET sensor_model = sm.model_id::text
       FROM instruments.sensor_models sm
       WHERE lower(btrim(s.sensor_model)) = lower(btrim(sm.model))
         AND s.sensor_model IS NOT NULL
         AND sm.model_id IS NOT NULL"
    )
    # Convert the column to integer
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.sensors
       ALTER COLUMN sensor_model TYPE INTEGER
       USING sensor_model::INTEGER"
    )
    # Now alter the column to be an integer fk to instruments.sensor_models.model_id
    replace_fk_constraint(
      "instruments.sensors",
      "sensors_sensor_model_fkey",
      "sensor_model",
      "instruments.sensor_models",
      "model_id",
      on_delete = "RESTRICT"
    )
    # Privileges on new sensor_models table
    grant_table_privileges(
      "instruments.sensor_models",
      "SELECT",
      unique(c(
        instrument_read_roles,
        instrument_write_roles,
        instrument_admin_roles
      ))
    )
    grant_table_privileges(
      "instruments.sensor_models",
      "INSERT, UPDATE, DELETE",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )
    grant_sequence_privileges(
      "instruments.sensor_models",
      "model_id",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )
    grant_sequence_privileges(
      "instruments.sensor_types",
      "sensor_type_id",
      unique(c(instrument_write_roles, instrument_admin_roles))
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.sensors
       ADD COLUMN IF NOT EXISTS owner INTEGER"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.sensors
       ADD COLUMN IF NOT EXISTS supplier_id INTEGER"
    )
    replace_fk_constraint(
      "instruments.sensors",
      "sensors_owner_fkey",
      "owner",
      "public.organizations",
      "organization_id",
      on_delete = "RESTRICT"
    )
    replace_fk_constraint(
      "instruments.sensors",
      "sensors_supplier_id_fkey",
      "supplier_id",
      "instruments.suppliers",
      "supplier_id",
      on_delete = "RESTRICT"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.sensors.owner IS
       'Organization that owns the sensor.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.sensors.supplier_id IS
       'Supplier from which the sensor was purchased, when known.'"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS sensor_makes_make_normalized_uidx
       ON instruments.sensor_makes (lower(btrim(make)))"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS sensor_models_model_normalized_uidx
       ON instruments.sensor_models (lower(btrim(model)))"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS sensor_types_sensor_type_normalized_uidx
       ON instruments.sensor_types (lower(btrim(sensor_type)))"
    )

    # Rename some tables to match conventions in DB
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_type RENAME TO instrument_types"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_make RENAME TO instrument_makes"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instrument_model RENAME TO instrument_models"
    )

    source_current_signature <- "continuous.measurements_continuous_corrected(integer, timestamp with time zone, timestamp with time zone)"
    source_at_signature <- "continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone)"
    target_current_signature <- "continuous.measurements_continuous_corrected(integer, timestamp with time zone, timestamp with time zone, text, integer)"
    target_at_signature <- "continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, text, integer)"

    current_grants <- get_function_execute_grants(source_current_signature)
    at_grants <- get_function_execute_grants(source_at_signature)

    message("Dropping old corrected measurement function signatures...")
    DBI::dbExecute(con, paste0("DROP FUNCTION ", source_current_signature))
    DBI::dbExecute(con, paste0("DROP FUNCTION ", source_at_signature))

    message(
      "Creating corrected measurement function with resampling arguments..."
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION continuous.measurements_continuous_corrected(
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         statistic TEXT DEFAULT 'actual',
         resample_seconds INTEGER DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       SECURITY INVOKER
       AS $function$
       DECLARE
         v_type TEXT;
         v_statistic TEXT := lower(statistic);
         v_step INTERVAL;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_statistic IS NULL THEN
           v_statistic := 'actual';
         END IF;

         IF v_statistic NOT IN ('actual', 'min', 'max', 'mean', 'median') THEN
           RAISE EXCEPTION 'statistic must be one of actual, min, max, mean, median';
         END IF;

         IF v_statistic = 'actual' THEN
           IF resample_seconds IS NOT NULL THEN
             RAISE EXCEPTION 'resample_seconds can only be used when statistic is min, max, mean, or median';
           END IF;

           IF v_type = 'basic' THEN
             RETURN QUERY
             SELECT
               mc.timeseries_id,
               mc.datetime,
               mc.value AS value_raw,
               continuous.apply_corrections(
                 mc.timeseries_id,
                 mc.datetime,
                 mc.value
               ) AS value_corrected,
               mc.period,
               mc.imputed
             FROM continuous.measurements_continuous mc
             WHERE mc.timeseries_id = p_timeseries_id
               AND (p_from IS NULL OR mc.datetime >= p_from)
               AND (p_to IS NULL OR mc.datetime <= p_to);

             RETURN;
           END IF;

           RETURN QUERY
           SELECT
             p_timeseries_id,
             src.datetime,
             src.value_raw,
             continuous.apply_corrections(
               p_timeseries_id,
               src.datetime,
               src.value_raw
             ) AS value_corrected,
             src.period,
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window(
             p_timeseries_id,
             p_from,
             p_to,
             ARRAY[]::INTEGER[]
           ) src;

           RETURN;
         END IF;

         IF resample_seconds IS NULL OR resample_seconds <= 0 THEN
           RAISE EXCEPTION 'resample_seconds must be a positive integer when statistic is min, max, mean, or median';
         END IF;

         IF p_from IS NULL OR p_to IS NULL THEN
           RAISE EXCEPTION 'p_from and p_to are required when statistic is min, max, mean, or median';
         END IF;

         IF p_to < p_from THEN
           RAISE EXCEPTION 'p_to must be greater than or equal to p_from';
         END IF;

         v_step := make_interval(secs => resample_seconds);

         IF v_type = 'basic' THEN
           RETURN QUERY
           WITH binned_measurements AS (
             SELECT
               p_from + make_interval(
                 secs => (
                   floor(
                     extract(epoch FROM (mc.datetime - p_from)) /
                       resample_seconds
                   ) * resample_seconds
                 )::double precision
               ) AS datetime,
               mc.value AS value_raw,
               continuous.apply_corrections(
                 mc.timeseries_id,
                 mc.datetime,
                 mc.value
               ) AS value_corrected,
               mc.imputed
             FROM continuous.measurements_continuous mc
             WHERE mc.timeseries_id = p_timeseries_id
               AND mc.datetime >= p_from
               AND mc.datetime <= p_to
           ),
           grouped_measurements AS MATERIALIZED (
             SELECT
               binned_measurements.datetime,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_raw)
                 WHEN 'max' THEN max(binned_measurements.value_raw)
                 WHEN 'mean' THEN avg(binned_measurements.value_raw)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_raw::double precision
                   )
                 )::numeric
               END AS value_raw,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_corrected)
                 WHEN 'max' THEN max(binned_measurements.value_corrected)
                 WHEN 'mean' THEN avg(binned_measurements.value_corrected)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_corrected::double precision
                   )
                 )::numeric
               END AS value_corrected,
               bool_or(binned_measurements.imputed) AS imputed
             FROM binned_measurements
             GROUP BY binned_measurements.datetime
           ),
           bins AS (
             SELECT generate_series(p_from, p_to, v_step) AS datetime
           )
           SELECT
             p_timeseries_id AS timeseries_id,
             bins.datetime,
             grouped_measurements.value_raw,
             grouped_measurements.value_corrected,
             v_step AS period,
             grouped_measurements.imputed
           FROM bins
           LEFT JOIN grouped_measurements
             ON grouped_measurements.datetime = bins.datetime
           ORDER BY bins.datetime;

           RETURN;
         END IF;

         RETURN QUERY
         WITH binned_measurements AS (
           SELECT
             p_from + make_interval(
               secs => (
                 floor(
                   extract(epoch FROM (src.datetime - p_from)) /
                     resample_seconds
                 ) * resample_seconds
               )::double precision
             ) AS datetime,
             src.value_raw,
             continuous.apply_corrections(
               p_timeseries_id,
               src.datetime,
               src.value_raw
             ) AS value_corrected,
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window(
             p_timeseries_id,
             p_from,
             p_to,
             ARRAY[]::INTEGER[]
           ) src
         ),
         grouped_measurements AS MATERIALIZED (
           SELECT
             binned_measurements.datetime,
             CASE v_statistic
               WHEN 'min' THEN min(binned_measurements.value_raw)
               WHEN 'max' THEN max(binned_measurements.value_raw)
               WHEN 'mean' THEN avg(binned_measurements.value_raw)
               WHEN 'median' THEN (
                 percentile_cont(0.5) WITHIN GROUP (
                   ORDER BY binned_measurements.value_raw::double precision
                 )
               )::numeric
             END AS value_raw,
             CASE v_statistic
               WHEN 'min' THEN min(binned_measurements.value_corrected)
               WHEN 'max' THEN max(binned_measurements.value_corrected)
               WHEN 'mean' THEN avg(binned_measurements.value_corrected)
               WHEN 'median' THEN (
                 percentile_cont(0.5) WITHIN GROUP (
                   ORDER BY binned_measurements.value_corrected::double precision
                 )
               )::numeric
             END AS value_corrected,
             bool_or(binned_measurements.imputed) AS imputed
           FROM binned_measurements
           GROUP BY binned_measurements.datetime
         ),
         bins AS (
           SELECT generate_series(p_from, p_to, v_step) AS datetime
         )
         SELECT
           p_timeseries_id AS timeseries_id,
           bins.datetime,
           grouped_measurements.value_raw,
           grouped_measurements.value_corrected,
           v_step AS period,
           grouped_measurements.imputed
         FROM bins
         LEFT JOIN grouped_measurements
           ON grouped_measurements.datetime = bins.datetime
         ORDER BY bins.datetime;
       END;
       $function$"
    )

    message(
      "Creating point-in-time corrected measurement function with resampling arguments..."
    )
    DBI::dbExecute(
      con,
      "CREATE FUNCTION continuous.measurements_continuous_corrected_at(
         p_as_of TIMESTAMPTZ,
         p_timeseries_id INTEGER,
         p_from TIMESTAMP WITH TIME ZONE,
         p_to TIMESTAMP WITH TIME ZONE,
         statistic TEXT DEFAULT 'actual',
         resample_seconds INTEGER DEFAULT NULL
       )
       RETURNS TABLE (
         timeseries_id INTEGER,
         datetime TIMESTAMP WITH TIME ZONE,
         value_raw NUMERIC,
         value_corrected NUMERIC,
         period INTERVAL,
         imputed BOOLEAN
       )
       LANGUAGE plpgsql
       STABLE
       SECURITY INVOKER
       SET search_path = pg_catalog, public, continuous, audit
       AS $function$
       DECLARE
         v_type TEXT;
         v_statistic TEXT := lower(statistic);
         v_step INTERVAL;
       BEGIN
         SELECT t.timeseries_type
         INTO v_type
         FROM continuous.timeseries t
         WHERE t.timeseries_id = p_timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'timeseries % not found', p_timeseries_id;
         END IF;

         IF v_statistic IS NULL THEN
           v_statistic := 'actual';
         END IF;

         IF v_statistic NOT IN ('actual', 'min', 'max', 'mean', 'median') THEN
           RAISE EXCEPTION 'statistic must be one of actual, min, max, mean, median';
         END IF;

         IF v_statistic = 'actual' THEN
           IF resample_seconds IS NOT NULL THEN
             RAISE EXCEPTION 'resample_seconds can only be used when statistic is min, max, mean, or median';
           END IF;

           IF v_type = 'basic' THEN
             RETURN QUERY
             SELECT
               mc.timeseries_id,
               mc.datetime,
               mc.value AS value_raw,
               continuous.apply_corrections_at(
                 p_as_of,
                 mc.timeseries_id,
                 mc.datetime,
                 mc.value
               ) AS value_corrected,
               mc.period,
               mc.imputed
             FROM audit.measurements_continuous_as_of(
               p_as_of,
               ARRAY[p_timeseries_id],
               p_from,
               p_to
             ) mc;

             RETURN;
           END IF;

           RETURN QUERY
           SELECT
             p_timeseries_id,
             src.datetime,
             src.value_raw,
             continuous.apply_corrections_at(
               p_as_of,
               p_timeseries_id,
               src.datetime,
               src.value_raw
             ) AS value_corrected,
             src.period,
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window_at(
             p_as_of,
             p_timeseries_id,
             p_from,
             p_to,
             ARRAY[]::INTEGER[]
           ) src;

           RETURN;
         END IF;

         IF resample_seconds IS NULL OR resample_seconds <= 0 THEN
           RAISE EXCEPTION 'resample_seconds must be a positive integer when statistic is min, max, mean, or median';
         END IF;

         IF p_from IS NULL OR p_to IS NULL THEN
           RAISE EXCEPTION 'p_from and p_to are required when statistic is min, max, mean, or median';
         END IF;

         IF p_to < p_from THEN
           RAISE EXCEPTION 'p_to must be greater than or equal to p_from';
         END IF;

         v_step := make_interval(secs => resample_seconds);

         IF v_type = 'basic' THEN
           RETURN QUERY
           WITH binned_measurements AS (
             SELECT
               p_from + make_interval(
                 secs => (
                   floor(
                     extract(epoch FROM (mc.datetime - p_from)) /
                       resample_seconds
                   ) * resample_seconds
                 )::double precision
               ) AS datetime,
               mc.value AS value_raw,
               continuous.apply_corrections_at(
                 p_as_of,
                 mc.timeseries_id,
                 mc.datetime,
                 mc.value
               ) AS value_corrected,
               mc.imputed
             FROM audit.measurements_continuous_as_of(
               p_as_of,
               ARRAY[p_timeseries_id],
               p_from,
               p_to
             ) mc
           ),
           grouped_measurements AS MATERIALIZED (
             SELECT
               binned_measurements.datetime,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_raw)
                 WHEN 'max' THEN max(binned_measurements.value_raw)
                 WHEN 'mean' THEN avg(binned_measurements.value_raw)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_raw::double precision
                   )
                 )::numeric
               END AS value_raw,
               CASE v_statistic
                 WHEN 'min' THEN min(binned_measurements.value_corrected)
                 WHEN 'max' THEN max(binned_measurements.value_corrected)
                 WHEN 'mean' THEN avg(binned_measurements.value_corrected)
                 WHEN 'median' THEN (
                   percentile_cont(0.5) WITHIN GROUP (
                     ORDER BY binned_measurements.value_corrected::double precision
                   )
                 )::numeric
               END AS value_corrected,
               bool_or(binned_measurements.imputed) AS imputed
             FROM binned_measurements
             GROUP BY binned_measurements.datetime
           ),
           bins AS (
             SELECT generate_series(p_from, p_to, v_step) AS datetime
           )
           SELECT
             p_timeseries_id AS timeseries_id,
             bins.datetime,
             grouped_measurements.value_raw,
             grouped_measurements.value_corrected,
             v_step AS period,
             grouped_measurements.imputed
           FROM bins
           LEFT JOIN grouped_measurements
             ON grouped_measurements.datetime = bins.datetime
           ORDER BY bins.datetime;

           RETURN;
         END IF;

         RETURN QUERY
         WITH binned_measurements AS (
           SELECT
             p_from + make_interval(
               secs => (
                 floor(
                   extract(epoch FROM (src.datetime - p_from)) /
                     resample_seconds
                 ) * resample_seconds
               )::double precision
             ) AS datetime,
             src.value_raw,
             continuous.apply_corrections_at(
               p_as_of,
               p_timeseries_id,
               src.datetime,
               src.value_raw
             ) AS value_corrected,
             src.imputed
           FROM continuous.resolve_compound_timeseries_raw_window_at(
             p_as_of,
             p_timeseries_id,
             p_from,
             p_to,
             ARRAY[]::INTEGER[]
           ) src
         ),
         grouped_measurements AS MATERIALIZED (
           SELECT
             binned_measurements.datetime,
             CASE v_statistic
               WHEN 'min' THEN min(binned_measurements.value_raw)
               WHEN 'max' THEN max(binned_measurements.value_raw)
               WHEN 'mean' THEN avg(binned_measurements.value_raw)
               WHEN 'median' THEN (
                 percentile_cont(0.5) WITHIN GROUP (
                   ORDER BY binned_measurements.value_raw::double precision
                 )
               )::numeric
             END AS value_raw,
             CASE v_statistic
               WHEN 'min' THEN min(binned_measurements.value_corrected)
               WHEN 'max' THEN max(binned_measurements.value_corrected)
               WHEN 'mean' THEN avg(binned_measurements.value_corrected)
               WHEN 'median' THEN (
                 percentile_cont(0.5) WITHIN GROUP (
                   ORDER BY binned_measurements.value_corrected::double precision
                 )
               )::numeric
             END AS value_corrected,
             bool_or(binned_measurements.imputed) AS imputed
           FROM binned_measurements
           GROUP BY binned_measurements.datetime
         ),
         bins AS (
           SELECT generate_series(p_from, p_to, v_step) AS datetime
         )
         SELECT
           p_timeseries_id AS timeseries_id,
           bins.datetime,
           grouped_measurements.value_raw,
           grouped_measurements.value_corrected,
           v_step AS period,
           grouped_measurements.imputed
         FROM bins
         LEFT JOIN grouped_measurements
           ON grouped_measurements.datetime = bins.datetime
         ORDER BY bins.datetime;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected(
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE,
         TEXT,
         INTEGER
       ) IS
       'Returns exact or fixed-width resampled raw and corrected values for one basic or compound timeseries. Statistic must be actual, min, max, mean, or median. For min/max/mean/median, p_from and p_to must be non-NULL and resample_seconds controls the bin width; bins with no source values return NULL values.';"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.measurements_continuous_corrected_at(
         TIMESTAMPTZ,
         INTEGER,
         TIMESTAMP WITH TIME ZONE,
         TIMESTAMP WITH TIME ZONE,
         TEXT,
         INTEGER
       ) IS
       'Point-in-time exact or fixed-width resampled corrected-measurement access for one basic or compound timeseries. Statistic must be actual, min, max, mean, or median. For min/max/mean/median, p_from and p_to must be non-NULL and resample_seconds controls the bin width; bins with no source values return NULL values.';"
    )

    DBI::dbExecute(
      con,
      paste0(
        "REVOKE ALL ON FUNCTION ",
        target_current_signature,
        " FROM PUBLIC"
      )
    )
    DBI::dbExecute(
      con,
      paste0("REVOKE ALL ON FUNCTION ", target_at_signature, " FROM PUBLIC")
    )

    apply_function_execute_grants(target_current_signature, current_grants)
    apply_function_execute_grants(target_at_signature, at_grants)

    message("Checking resampled function signatures...")
    result_check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regprocedure(
           'continuous.measurements_continuous_corrected(integer, timestamp with time zone, timestamp with time zone, text, integer)'
         ) IS NOT NULL AS has_current_resample,
         to_regprocedure(
           'continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone, text, integer)'
         ) IS NOT NULL AS has_at_resample,
         to_regprocedure(
           'continuous.measurements_continuous_corrected(integer, timestamp with time zone, timestamp with time zone)'
         ) IS NULL AS old_current_removed,
         to_regprocedure(
           'continuous.measurements_continuous_corrected_at(timestamp with time zone, integer, timestamp with time zone, timestamp with time zone)'
         ) IS NULL AS old_at_removed"
    )

    if (
      !isTRUE(result_check$has_current_resample[[1]]) ||
        !isTRUE(result_check$has_at_resample[[1]]) ||
        !isTRUE(result_check$old_current_removed[[1]]) ||
        !isTRUE(result_check$old_at_removed[[1]])
    ) {
      stop(
        "Patch 44 verification failed: corrected-measurement functions were not replaced with the expected signatures."
      )
    }

    message("Checking normalized deployment-timeseries associations...")
    deployment_timeseries_check <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass(
           'public.locations_metadata_instrument_timeseries'
         ) IS NOT NULL AS has_association_table,
         to_regprocedure(
           'public.check_locations_metadata_instrument_timeseries_overlap()'
         ) IS NOT NULL AS has_overlap_function,
         to_regprocedure(
           'public.check_instrument_connection_signal_timeseries()'
         ) IS NOT NULL AS has_signal_timeseries_function,
         to_regprocedure(
           'public.check_locations_metadata_instruments_acquisition_dependents()'
         ) IS NOT NULL AS has_acquisition_dependent_function,
         pg_get_functiondef(
           'public.check_instrument_connection_signal_timeseries()'::regprocedure
         ) NOT ILIKE '%instrument_row.timeseries_id%'
           AS signal_timeseries_function_normalized,
         pg_get_functiondef(
           'public.check_locations_metadata_instruments_acquisition_dependents()'::regprocedure
         ) NOT ILIKE '%NEW.timeseries_id%'
           AS acquisition_dependent_function_normalized,
         NOT EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'locations_metadata_instruments'
             AND column_name = 'timeseries_id'
         ) AS legacy_deployment_timeseries_column_removed,
         NOT EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_timeseries lmit_a
           INNER JOIN public.locations_metadata_instruments lmi_a
             ON lmi_a.metadata_id = lmit_a.metadata_id
           INNER JOIN public.locations_metadata_instrument_timeseries lmit_b
             ON lmit_b.timeseries_id = lmit_a.timeseries_id
            AND lmit_b.metadata_id <> lmit_a.metadata_id
           INNER JOIN public.locations_metadata_instruments lmi_b
             ON lmi_b.metadata_id = lmit_b.metadata_id
           WHERE tstzrange(
               lmi_a.start_datetime,
               COALESCE(
                 lmi_a.end_datetime,
                 'infinity'::timestamp with time zone
               ),
               '[)'::text
             ) && tstzrange(
               lmi_b.start_datetime,
               COALESCE(
                 lmi_b.end_datetime,
                 'infinity'::timestamp with time zone
               ),
               '[)'::text
             )
         ) AS no_overlapping_timeseries_links"
    )

    if (
      !isTRUE(deployment_timeseries_check$has_association_table[[1]]) ||
        !isTRUE(deployment_timeseries_check$has_overlap_function[[1]]) ||
        !isTRUE(
          deployment_timeseries_check$has_signal_timeseries_function[[1]]
        ) ||
        !isTRUE(
          deployment_timeseries_check$has_acquisition_dependent_function[[1]]
        ) ||
        !isTRUE(
          deployment_timeseries_check$signal_timeseries_function_normalized[[1]]
        ) ||
        !isTRUE(
          deployment_timeseries_check$acquisition_dependent_function_normalized[[
            1
          ]]
        ) ||
        !isTRUE(
          deployment_timeseries_check$legacy_deployment_timeseries_column_removed[[
            1
          ]]
        ) ||
        !isTRUE(
          deployment_timeseries_check$no_overlapping_timeseries_links[[1]]
        )
    ) {
      stop(
        "Patch 44 verification failed: normalized deployment-timeseries associations were not created correctly."
      )
    }

    # Now let's commit!
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '44'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    DBI::dbExecute(con, "COMMIT;")
    message(
      "Patch 44 applied successfully. continuous.measurements_continuous_corrected() and continuous.measurements_continuous_corrected_at() now include statistic and resample_seconds arguments, instrument maintenance due state and sensor events now have normalized tables, and instrument deployment-timeseries links are now normalized."
    )
  },
  error = function(e) {
    message("Error detected. Rolling back transaction...")
    try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    stop(
      "Patch 44 failed and the database has been rolled back to its previous state. ",
      e$message
    )
  }
)
