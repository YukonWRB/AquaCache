# Patch 37

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 37. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # Basic sanity checks: this combined patch extends the patch 36 deployment
    # metadata schema and assumes the patch 36-era column names already exist.
    check <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "to_regclass('public.locations_metadata_instruments') IS NOT NULL AS has_lmi,",
        "to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,",
        "to_regclass('public.parameters') IS NOT NULL AS has_parameters,",
        "to_regclass('public.units') IS NOT NULL AS has_units,",
        "to_regclass('instruments.instruments') IS NOT NULL AS has_instruments,",
        "to_regclass('instruments.instrument_type') IS NOT NULL AS has_instrument_type,",
        "EXISTS (",
        "  SELECT 1",
        "  FROM information_schema.columns",
        "  WHERE table_schema = 'public'",
        "    AND table_name = 'locations_metadata_instruments'",
        "    AND column_name = 'timeseries_id'",
        ") AS has_lmi_timeseries"
      )
    )

    if (
      !isTRUE(check$has_lmi[[1]]) ||
        !isTRUE(check$has_timeseries[[1]]) ||
        !isTRUE(check$has_parameters[[1]]) ||
        !isTRUE(check$has_units[[1]]) ||
        !isTRUE(check$has_instruments[[1]]) ||
        !isTRUE(check$has_instrument_type[[1]]) ||
        !isTRUE(check$has_lmi_timeseries[[1]])
    ) {
      stop(
        paste(
          "Combined patch 37 requires public.locations_metadata_instruments",
          "(including locations_metadata_instruments.timeseries_id),",
          "continuous.timeseries, public.parameters, public.units,",
          "instruments.instruments, and instruments.instrument_type",
          "to already exist."
        )
      )
    }

    # Instrument capability flags ###########################################
    # This is a first-pass capability model used to distinguish deployments
    # that can legitimately act as dataloggers from ordinary sensors.
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
       ADD COLUMN IF NOT EXISTS can_be_logger BOOLEAN;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.instruments.can_be_logger IS
       'Whether the instrument can act as a datalogger or acquisition controller in instrument/logger connection metadata. Populated with a first-pass heuristic in patch 37 and intended for manual review.';"
    )

    logger_types <- DBI::dbGetQuery(
      con,
      "SELECT type_id, type FROM instruments.instrument_type;"
    )
    for (i in unique(logger_types$type_id)) {
      type_name <- logger_types$type[logger_types$type_id == i]
      if (
        grepl(
          "logger|data\\s*logger|datalogger|rtu|plc|controller|gateway|station",
          type_name,
          ignore.case = TRUE
        )
      ) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE instruments.instruments",
            " SET can_be_logger = TRUE",
            " WHERE type = ",
            i,
            " AND can_be_logger IS NULL;"
          )
        )
      } else {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE instruments.instruments",
            " SET can_be_logger = FALSE",
            " WHERE type = ",
            i,
            " AND can_be_logger IS NULL;"
          )
        )
      }
    }
    DBI::dbExecute(
      con,
      "UPDATE instruments.instruments
       SET can_be_logger = FALSE
       WHERE can_be_logger IS NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
       ALTER COLUMN can_be_logger SET DEFAULT FALSE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
       ALTER COLUMN can_be_logger SET NOT NULL;"
    )

    # Communication protocol lookup #########################################
    # This lookup is intentionally protocol-agnostic so that SDI-12, analog,
    # pulse, and serial protocols can all use the same connection model.
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.communication_protocol_families (
         protocol_family_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         family_code TEXT NOT NULL,
         family_name TEXT NOT NULL,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT communication_protocol_families_code_key UNIQUE (family_code),
         CONSTRAINT communication_protocol_families_name_key UNIQUE (family_name)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.communication_protocol_families OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.communication_protocol_families IS
       'Lookup of broad communication protocol families used to group protocol definitions and drive UI behaviour.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocol_families.family_code IS
       'Stable short code for the protocol family, e.g. digital_bus, serial, analog.';"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS communication_protocol_families_user_modified
       ON instruments.communication_protocol_families;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS communication_protocol_families_update_modified
       ON instruments.communication_protocol_families;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER communication_protocol_families_user_modified
       BEFORE UPDATE ON instruments.communication_protocol_families
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER communication_protocol_families_update_modified
       BEFORE UPDATE ON instruments.communication_protocol_families
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.communication_protocol_families
         (family_code, family_name, description)
       VALUES
         ('digital_bus', 'Digital bus',
          'Addressed or bus-based digital communication between one datalogger and one or more instruments.'),
         ('serial', 'Serial',
          'Point-to-point or multi-drop serial communication, typically RS-232 or RS-485 based.'),
         ('network', 'Network / IP',
          'Packet-based or socket-based communication over Ethernet, Wi-Fi, cellular IP, or similar network transports.'),
         ('analog', 'Analog',
          'Voltage or current signals measured by logger analog inputs.'),
         ('pulse', 'Pulse / counter',
          'Pulse, tipping-bucket, switch closure, or counter-based inputs.'),
         ('internal', 'Internal / system',
          'Values produced by the datalogger internally rather than by an external communicating instrument.'),
         ('other', 'Other / custom',
          'Catch-all family for uncommon or not-yet-modelled communication approaches.')
       ON CONFLICT (family_code) DO UPDATE
       SET family_name = EXCLUDED.family_name,
           description = EXCLUDED.description;"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.communication_protocols (
         protocol_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         protocol_code TEXT NOT NULL,
         protocol_name TEXT NOT NULL,
         protocol_family_id INTEGER NOT NULL
           REFERENCES instruments.communication_protocol_families(protocol_family_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         uses_logger_port BOOLEAN NOT NULL DEFAULT TRUE,
         uses_device_address BOOLEAN NOT NULL DEFAULT FALSE,
         uses_signal_index BOOLEAN NOT NULL DEFAULT FALSE,
         uses_poll_command BOOLEAN NOT NULL DEFAULT FALSE,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT communication_protocols_code_key UNIQUE (protocol_code),
         CONSTRAINT communication_protocols_name_key UNIQUE (protocol_name)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.communication_protocols OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.communication_protocols IS
       'Lookup of communication protocols used between deployed instruments and deployed dataloggers. Designed to support SDI-12 and non-SDI protocols with one model.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocols.protocol_code IS
       'Stable short code for the protocol, e.g. SDI12, MODBUS_RTU, ANALOG_VOLTAGE.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocols.protocol_family_id IS
       'Foreign key to instruments.communication_protocol_families for broad protocol grouping and UI hints.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocols.uses_logger_port IS
       'Whether the protocol typically needs a logger-side port, channel, or terminal label.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocols.uses_device_address IS
       'Whether the protocol typically uses a device address on a shared bus, e.g. SDI-12 or Modbus RTU.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocols.uses_signal_index IS
       'Whether the protocol commonly returns indexed values within one connection, e.g. SDI-12 parameter numbers or Modbus registers.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.communication_protocols.uses_poll_command IS
       'Whether the protocol commonly uses an explicit command string, e.g. SDI-12 M! or custom serial queries.';"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS communication_protocols_user_modified
       ON instruments.communication_protocols;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS communication_protocols_update_modified
       ON instruments.communication_protocols;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER communication_protocols_user_modified
       BEFORE UPDATE ON instruments.communication_protocols
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER communication_protocols_update_modified
       BEFORE UPDATE ON instruments.communication_protocols
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.communication_protocols
         (protocol_code, protocol_name, protocol_family_id, uses_logger_port,
          uses_device_address, uses_signal_index, uses_poll_command,
          description)
       SELECT
         v.protocol_code,
         v.protocol_name,
         pf.protocol_family_id,
         v.uses_logger_port,
         v.uses_device_address,
         v.uses_signal_index,
         v.uses_poll_command,
         v.description
       FROM (
         VALUES
           ('SDI12', 'SDI-12', 'digital_bus', TRUE, TRUE, TRUE, TRUE,
            'Shared addressed serial bus between a datalogger and smart sensors.'),
           ('MODBUS_RTU', 'Modbus RTU', 'serial', TRUE, TRUE, TRUE, FALSE,
            'Addressed RS-485 serial protocol; registers or coils typically mapped per signal.'),
           ('MODBUS_TCP', 'Modbus TCP', 'network', TRUE, TRUE, TRUE, FALSE,
            'Modbus over TCP/IP; use logger_port for the local interface, address_scope for the remote host or endpoint namespace, device_address for the Unit ID when used, and protocol_config for transport details such as byte order or timeouts.'),
           ('RS232_ASCII', 'RS-232 ASCII', 'serial', TRUE, FALSE, FALSE, TRUE,
            'Point-to-point serial ASCII communication using logger-side commands or parser settings.'),
           ('RS485_ASCII', 'RS-485 ASCII', 'serial', TRUE, FALSE, FALSE, TRUE,
            'Multi-drop or point-to-point RS-485 ASCII communication without formal register addressing.'),
           ('ANALOG_VOLTAGE', 'Analog voltage', 'analog', TRUE, FALSE, FALSE, FALSE,
            'Single-ended or differential voltage input measured by the datalogger.'),
           ('ANALOG_CURRENT', 'Analog current (4-20 mA)', 'analog', TRUE, FALSE, FALSE, FALSE,
            'Current-loop sensor measured through a logger analog/current input.'),
           ('PULSE', 'Pulse / counter', 'pulse', TRUE, FALSE, FALSE, FALSE,
            'Pulse, tipping-bucket, switch closure, or other counted-event input.'),
           ('SYSTEM_INTERNAL', 'Logger internal/system', 'internal', TRUE, FALSE, FALSE, FALSE,
            'Value computed or measured internally by the datalogger, e.g. battery voltage or message size.'),
           ('OTHER', 'Other / custom', 'other', TRUE, FALSE, FALSE, FALSE,
            'Catch-all protocol for unusual or not-yet-modelled logger communication patterns.')
       ) AS v(
         protocol_code,
         protocol_name,
         family_code,
         uses_logger_port,
         uses_device_address,
         uses_signal_index,
         uses_poll_command,
         description
       )
       JOIN instruments.communication_protocol_families pf
         ON pf.family_code = v.family_code
       ON CONFLICT (protocol_code) DO UPDATE
       SET protocol_name = EXCLUDED.protocol_name,
           protocol_family_id = EXCLUDED.protocol_family_id,
           uses_logger_port = EXCLUDED.uses_logger_port,
           uses_device_address = EXCLUDED.uses_device_address,
           uses_signal_index = EXCLUDED.uses_signal_index,
           uses_poll_command = EXCLUDED.uses_poll_command,
           description = EXCLUDED.description;"
    )

    # Instrument-to-logger connection metadata ##############################
    # This layer is deliberately about communication between a deployed
    # instrument and a deployed logger. Future logger-to-transmitter telemetry
    # (e.g. GOES) should be handled as its own temporal layer rather than
    # overloaded onto this table.
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.locations_metadata_instrument_connections (
         connection_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         instrument_metadata_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_instruments(metadata_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         logger_metadata_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_instruments(metadata_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         protocol_id INTEGER NOT NULL
           REFERENCES instruments.communication_protocols(protocol_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         logger_port TEXT,
         address_scope TEXT,
         device_address TEXT,
         protocol_config JSONB NOT NULL DEFAULT '{}'::jsonb,
         note TEXT,
         start_datetime TIMESTAMPTZ NOT NULL,
         end_datetime TIMESTAMPTZ,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT locations_metadata_instrument_connections_period_valid
           CHECK (end_datetime IS NULL OR start_datetime < end_datetime)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instrument_connections OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instrument_connections
       DROP CONSTRAINT IF EXISTS locations_metadata_instrument_connections_not_self;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_instrument_connections IS
       'Temporal metadata describing how a deployed instrument communicates with a deployed datalogger. Supports SDI-12, Modbus, analog, pulse, serial, and IP-based protocols with one protocol lookup.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.instrument_metadata_id IS
       'Deployed instrument record from public.locations_metadata_instruments for the communicating instrument or sensor.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.logger_metadata_id IS
       'Deployed instrument record from public.locations_metadata_instruments for the receiving datalogger.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.logger_port IS
       'Logger-side port, terminal, channel, bus label, or local interface identifier. This can hold values such as SDI-12 ports, analog input labels, COM ports, RS-485 buses, system channels, or a network interface label.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.address_scope IS
       'Optional namespace for device_address when the address is only unique within part of the connection path. Examples include a Modbus TCP host:port, a bridged serial segment, or another endpoint label. Leave NULL when logger_port alone defines the address scope.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.device_address IS
       'Protocol-specific address on the logger-side bus or within address_scope. For SDI-12 this is typically the one-character sensor address; for Modbus this is typically the slave or Unit ID; for non-addressed protocols leave NULL.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.protocol_config IS
       'Protocol-specific settings stored as JSONB, e.g. SDI-12 settings, analog excitation notes, Modbus serial settings, byte order, timeout tuning, or parser configuration.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connections.note IS
       'Free-text notes about wiring, maintenance concerns, or logger programming assumptions for the connection.';"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_instrument_connections_instrument_idx
       ON public.locations_metadata_instrument_connections (instrument_metadata_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_instrument_connections_logger_idx
       ON public.locations_metadata_instrument_connections (logger_metadata_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_instrument_connections_protocol_idx
       ON public.locations_metadata_instrument_connections (protocol_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_instrument_connections_range_idx
       ON public.locations_metadata_instrument_connections
       (instrument_metadata_id, logger_metadata_id, start_datetime, end_datetime);"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_instrument_connections_user_modified
       ON public.locations_metadata_instrument_connections;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_instrument_connections_update_modified
       ON public.locations_metadata_instrument_connections;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_instrument_connections_user_modified
       BEFORE UPDATE ON public.locations_metadata_instrument_connections
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_instrument_connections_update_modified
       BEFORE UPDATE ON public.locations_metadata_instrument_connections
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_instrument_connection_bounds()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         instrument_row RECORD;
         logger_row RECORD;
         protocol_row RECORD;
         logger_can_be_logger BOOLEAN;
       BEGIN
         SELECT metadata_id, location_id, sub_location_id, instrument_id,
                start_datetime, end_datetime
         INTO instrument_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = NEW.instrument_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Instrument deployment % does not exist.',
             NEW.instrument_metadata_id;
         END IF;

         SELECT metadata_id, location_id, sub_location_id, instrument_id,
                start_datetime, end_datetime
         INTO logger_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = NEW.logger_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Logger deployment % does not exist.',
             NEW.logger_metadata_id;
         END IF;

         SELECT cp.protocol_id, cpf.family_code
         INTO protocol_row
         FROM instruments.communication_protocols cp
         JOIN instruments.communication_protocol_families cpf
           ON cpf.protocol_family_id = cp.protocol_family_id
         WHERE cp.protocol_id = NEW.protocol_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Protocol % does not exist.', NEW.protocol_id;
         END IF;

         IF instrument_row.instrument_id IS NULL THEN
           RAISE EXCEPTION
             'Instrument deployment % must reference a deployed instrument.',
             NEW.instrument_metadata_id;
         END IF;

         IF logger_row.instrument_id IS NULL THEN
           RAISE EXCEPTION
             'Logger deployment % must reference a deployed instrument.',
             NEW.logger_metadata_id;
         END IF;

         SELECT can_be_logger
         INTO logger_can_be_logger
         FROM instruments.instruments
         WHERE instrument_id = logger_row.instrument_id;

         IF NOT FOUND OR logger_can_be_logger IS DISTINCT FROM TRUE THEN
           RAISE EXCEPTION
             'Logger deployment % must reference an instrument marked can_be_logger = TRUE.',
             NEW.logger_metadata_id;
         END IF;

         IF protocol_row.family_code = 'internal' THEN
           IF NEW.instrument_metadata_id <> NEW.logger_metadata_id THEN
             RAISE EXCEPTION
               'Internal/system protocols must use the same deployment for instrument_metadata_id and logger_metadata_id.';
           END IF;
         ELSIF NEW.instrument_metadata_id = NEW.logger_metadata_id THEN
           RAISE EXCEPTION
             'Only internal/system protocols may use the same deployment for instrument_metadata_id and logger_metadata_id.';
         END IF;

         IF instrument_row.location_id <> logger_row.location_id THEN
           RAISE EXCEPTION
             'Instrument deployment % and logger deployment % must belong to the same location.',
             NEW.instrument_metadata_id,
             NEW.logger_metadata_id;
         END IF;

         IF NEW.start_datetime < instrument_row.start_datetime OR
            NEW.start_datetime < logger_row.start_datetime THEN
           RAISE EXCEPTION
             'Connection start_datetime must not be earlier than either deployment start.';
         END IF;

         IF COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
            COALESCE(instrument_row.end_datetime, 'infinity'::timestamptz) THEN
           RAISE EXCEPTION
             'Connection end_datetime must not extend beyond the instrument deployment period.';
         END IF;

         IF COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
            COALESCE(logger_row.end_datetime, 'infinity'::timestamptz) THEN
           RAISE EXCEPTION
             'Connection end_datetime must not extend beyond the logger deployment period.';
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_instrument_connection_overlap()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           WHERE c.connection_id <> NEW.connection_id
             AND c.instrument_metadata_id = NEW.instrument_metadata_id
             AND c.logger_metadata_id = NEW.logger_metadata_id
             AND c.protocol_id = NEW.protocol_id
             AND COALESCE(c.logger_port, '') = COALESCE(NEW.logger_port, '')
             AND COALESCE(c.address_scope, '') = COALESCE(NEW.address_scope, '')
             AND COALESCE(c.device_address, '') = COALESCE(NEW.device_address, '')
             AND NEW.start_datetime <
               COALESCE(c.end_datetime, 'infinity'::timestamptz)
             AND COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
               c.start_datetime
         ) THEN
           RAISE EXCEPTION
             'Duplicate overlapping instrument/logger connection detected for instrument deployment %, logger deployment %, protocol %, port %, address scope %, address %.',
             NEW.instrument_metadata_id,
             NEW.logger_metadata_id,
             NEW.protocol_id,
             COALESCE(NEW.logger_port, '(none)'),
             COALESCE(NEW.address_scope, '(none)'),
             COALESCE(NEW.device_address, '(none)');
         END IF;

         IF NEW.device_address IS NOT NULL AND EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           WHERE c.connection_id <> NEW.connection_id
             AND c.logger_metadata_id = NEW.logger_metadata_id
             AND c.protocol_id = NEW.protocol_id
             AND COALESCE(c.logger_port, '') = COALESCE(NEW.logger_port, '')
             AND COALESCE(c.address_scope, '') = COALESCE(NEW.address_scope, '')
             AND c.device_address = NEW.device_address
             AND NEW.start_datetime <
               COALESCE(c.end_datetime, 'infinity'::timestamptz)
             AND COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
               c.start_datetime
         ) THEN
           RAISE EXCEPTION
             'Address % is already in use on logger deployment %, protocol %, port %, and address scope % during the requested time range.',
             NEW.device_address,
             NEW.logger_metadata_id,
             NEW.protocol_id,
             COALESCE(NEW.logger_port, '(none)'),
             COALESCE(NEW.address_scope, '(none)');
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_instrument_connection_bounds
       ON public.locations_metadata_instrument_connections;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_instrument_connection_overlap
       ON public.locations_metadata_instrument_connections;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_instrument_connection_bounds
       AFTER INSERT OR UPDATE ON public.locations_metadata_instrument_connections
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_instrument_connection_bounds();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_instrument_connection_overlap
       AFTER INSERT OR UPDATE ON public.locations_metadata_instrument_connections
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_instrument_connection_overlap();"
    )

    # Per-signal mapping ####################################################
    # A single SDI-12 connection can yield several values, and a single
    # instrument can also expose different outputs on separate logger ports.
    # This child table handles per-value mapping without assuming one physical
    # instrument equals one timeseries.
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instruments.timeseries_id IS
       'Legacy single-timeseries link retained for compatibility. Must be NULL when a deployment maps to multiple distinct signal-level timeseries in public.locations_metadata_instrument_connection_signals; signal-level mappings are the authoritative model from patch 37 onward.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.locations_metadata_instrument_connection_signals (
         connection_signal_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         connection_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_instrument_connections(connection_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         signal_name TEXT NOT NULL,
         parameter_id INTEGER
           REFERENCES public.parameters(parameter_id)
           ON DELETE SET NULL ON UPDATE CASCADE,
         signal_order INTEGER,
         logger_input_label TEXT,
         protocol_signal_ref TEXT,
         acquisition_command TEXT,
         timeseries_id INTEGER
           REFERENCES continuous.timeseries(timeseries_id)
           ON DELETE SET NULL ON UPDATE CASCADE,
         signal_units INTEGER
           REFERENCES public.units(unit_id)
           ON DELETE SET NULL ON UPDATE CASCADE,
         scale_multiplier NUMERIC NOT NULL DEFAULT 1,
         scale_offset NUMERIC NOT NULL DEFAULT 0,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT locations_metadata_instrument_connection_signals_order_valid
           CHECK (signal_order IS NULL OR signal_order > 0)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_instrument_connection_signals OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_instrument_connection_signals IS
       'Per-signal mapping for an instrument/logger connection. Supports SDI-12 parameter numbers, Modbus registers, analog channels, and optional links to continuous.timeseries.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.signal_name IS
       'Human-readable name for the signal or value returned over the connection.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.parameter_id IS
       'Optional parameter lookup when the signal should be known before a timeseries exists.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.signal_order IS
       'Signal position within the connection response when ordering matters, e.g. SDI-12 parameter number or register order.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.logger_input_label IS
       'Logger-specific label for the mapped value, e.g. Analog 1, D+,D-, system channel, or parser field name.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.protocol_signal_ref IS
       'Protocol-specific reference for the signal, e.g. SDI-12 parameter index, Modbus register or coil reference, parser field, or pulse channel.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.acquisition_command IS
       'Protocol command or read profile used to obtain the value when relevant, e.g. SDI-12 M!, M1!, Modbus function/read block details, or a custom serial query.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.signal_units IS
       'Units for the raw/transmitted signal before any downstream correction or conversion.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.scale_multiplier IS
       'Optional multiplier applied to the raw signal before storage or interpretation.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_instrument_connection_signals.scale_offset IS
       'Optional offset applied to the raw signal after multiplication.';"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_instrument_connection_signals_connection_idx
       ON public.locations_metadata_instrument_connection_signals (connection_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_instrument_connection_signals_timeseries_idx
       ON public.locations_metadata_instrument_connection_signals (timeseries_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS locations_metadata_instrument_connection_signals_connection_order_key
       ON public.locations_metadata_instrument_connection_signals (connection_id, signal_order)
       WHERE signal_order IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS locations_metadata_instrument_connection_signals_connection_timeseries_key
       ON public.locations_metadata_instrument_connection_signals (connection_id, timeseries_id)
       WHERE timeseries_id IS NOT NULL;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_instrument_connection_signals_user_modified
       ON public.locations_metadata_instrument_connection_signals;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_instrument_connection_signals_update_modified
       ON public.locations_metadata_instrument_connection_signals;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_instrument_connection_signals_user_modified
       BEFORE UPDATE ON public.locations_metadata_instrument_connection_signals
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_instrument_connection_signals_update_modified
       BEFORE UPDATE ON public.locations_metadata_instrument_connection_signals
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_instrument_connection_signal_timeseries()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
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
            timeseries_row.sub_location_id IS DISTINCT FROM instrument_row.sub_location_id THEN
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

         IF instrument_row.timeseries_id IS NOT NULL AND
            instrument_row.timeseries_id IS DISTINCT FROM NEW.timeseries_id THEN
           RAISE EXCEPTION
             'Instrument deployment % has legacy timeseries_id % but signal mapping references timeseries %.',
             connection_row.instrument_metadata_id,
             instrument_row.timeseries_id,
             NEW.timeseries_id;
         END IF;

         IF instrument_row.timeseries_id IS NOT NULL AND EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           JOIN public.locations_metadata_instrument_connection_signals s
             ON s.connection_id = c.connection_id
           WHERE c.instrument_metadata_id = connection_row.instrument_metadata_id
             AND s.timeseries_id IS NOT NULL
           GROUP BY c.instrument_metadata_id
           HAVING COUNT(DISTINCT s.timeseries_id) > 1
         ) THEN
           RAISE EXCEPTION
             'Instrument deployment % uses multiple distinct signal-level timeseries and therefore cannot retain locations_metadata_instruments.timeseries_id.',
             connection_row.instrument_metadata_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_instrument_connection_signal_timeseries
       ON public.locations_metadata_instrument_connection_signals;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_instrument_connection_signal_timeseries
       AFTER INSERT OR UPDATE ON public.locations_metadata_instrument_connection_signals
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_instrument_connection_signal_timeseries();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_locations_metadata_instruments_acquisition_dependents()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
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

         IF NEW.timeseries_id IS NOT NULL AND EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           JOIN public.locations_metadata_instrument_connection_signals s
             ON s.connection_id = c.connection_id
           WHERE c.instrument_metadata_id = NEW.metadata_id
             AND s.timeseries_id IS NOT NULL
             AND s.timeseries_id <> NEW.timeseries_id
         ) THEN
           RAISE EXCEPTION
             'Instrument deployment % has legacy timeseries_id % that conflicts with existing signal-level mappings.',
             NEW.metadata_id,
             NEW.timeseries_id;
         END IF;

         IF NEW.timeseries_id IS NOT NULL AND EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           JOIN public.locations_metadata_instrument_connection_signals s
             ON s.connection_id = c.connection_id
           WHERE c.instrument_metadata_id = NEW.metadata_id
             AND s.timeseries_id IS NOT NULL
           GROUP BY c.instrument_metadata_id
           HAVING COUNT(DISTINCT s.timeseries_id) > 1
         ) THEN
           RAISE EXCEPTION
             'Instrument deployment % uses multiple distinct signal-level timeseries and therefore cannot retain locations_metadata_instruments.timeseries_id.',
             NEW.metadata_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_timeseries_dependents()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connection_signals s
           JOIN public.locations_metadata_instrument_connections c
             ON c.connection_id = s.connection_id
           JOIN public.locations_metadata_instruments i
             ON i.metadata_id = c.instrument_metadata_id
           WHERE s.timeseries_id = NEW.timeseries_id
             AND (
               NEW.location_id <> i.location_id OR
               NEW.sub_location_id IS DISTINCT FROM i.sub_location_id OR
               (i.z_id IS NOT NULL AND
                  NEW.z_id IS DISTINCT FROM i.z_id) OR
               (s.parameter_id IS NOT NULL AND
                  NEW.parameter_id IS DISTINCT FROM s.parameter_id)
             )
         ) THEN
           RAISE EXCEPTION
             'Updating timeseries % would invalidate existing instrument connection signal metadata.',
             NEW.timeseries_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_instrument_logger_capability()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF NEW.can_be_logger IS DISTINCT FROM TRUE AND EXISTS (
           SELECT 1
           FROM public.locations_metadata_instruments lmi
           JOIN public.locations_metadata_instrument_connections c
             ON c.logger_metadata_id = lmi.metadata_id
           WHERE lmi.instrument_id = NEW.instrument_id
         ) THEN
           RAISE EXCEPTION
             'Instrument % is already in use as a logger and cannot have can_be_logger set to FALSE.',
             NEW.instrument_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_communication_protocol_dependents()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_instrument_connections c
           JOIN instruments.communication_protocols cp
             ON cp.protocol_id = c.protocol_id
           JOIN instruments.communication_protocol_families cpf
             ON cpf.protocol_family_id = cp.protocol_family_id
           WHERE (cpf.family_code = 'internal' AND
                  c.instrument_metadata_id <> c.logger_metadata_id)
              OR (cpf.family_code <> 'internal' AND
                  c.instrument_metadata_id = c.logger_metadata_id)
         ) THEN
           RAISE EXCEPTION
             'Updating communication protocol metadata would invalidate existing instrument/logger connections.';
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_locations_metadata_instruments_acquisition_dependents
       ON public.locations_metadata_instruments;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_timeseries_dependents
       ON continuous.timeseries;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_instrument_logger_capability
       ON instruments.instruments;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_communication_protocol_dependents
       ON instruments.communication_protocols;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_communication_protocol_family_dependents
       ON instruments.communication_protocol_families;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_locations_metadata_instruments_acquisition_dependents
       AFTER UPDATE
       ON public.locations_metadata_instruments
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_locations_metadata_instruments_acquisition_dependents();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_timeseries_dependents
       AFTER UPDATE
       ON continuous.timeseries
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_timeseries_dependents();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_instrument_logger_capability
       AFTER UPDATE
       ON instruments.instruments
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_instrument_logger_capability();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_communication_protocol_dependents
       AFTER UPDATE
       ON instruments.communication_protocols
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_communication_protocol_dependents();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_communication_protocol_family_dependents
       AFTER UPDATE
       ON instruments.communication_protocol_families
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_communication_protocol_dependents();"
    )

    # Instrument capability flags ###########################################
    # Transmission components include accessories such as antennas, modems,
    # batteries, and solar panels, so they need a separate capability flag.
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
       ADD COLUMN IF NOT EXISTS can_be_telemetry_component BOOLEAN;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.instruments.can_be_telemetry_component IS
       'Whether the instrument can participate in a telemetry setup as an attached component such as a transmitter, antenna, modem, battery, or related telemetry accessory. Populated with a first-pass heuristic in patch 38 and intended for manual review.';"
    )

    telemetry_component_types <- DBI::dbGetQuery(
      con,
      "SELECT type_id, type FROM instruments.instrument_type;"
    )
    for (i in unique(telemetry_component_types$type_id)) {
      type_name <- telemetry_component_types$type[
        telemetry_component_types$type_id == i
      ]
      if (
        grepl(
          paste(
            "logger|data\\s*logger|datalogger|gateway|transmitter|antenna",
            "modem|terminal|sim|battery|solar|panel|controller|regulator",
            "radio",
            sep = "|"
          ),
          type_name,
          ignore.case = TRUE
        )
      ) {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE instruments.instruments",
            " SET can_be_telemetry_component = TRUE",
            " WHERE type = ",
            i,
            " AND can_be_telemetry_component IS NULL;"
          )
        )
      } else {
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE instruments.instruments",
            " SET can_be_telemetry_component = FALSE",
            " WHERE type = ",
            i,
            " AND can_be_telemetry_component IS NULL;"
          )
        )
      }
    }
    DBI::dbExecute(
      con,
      "UPDATE instruments.instruments
       SET can_be_telemetry_component = FALSE
       WHERE can_be_telemetry_component IS NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
       ALTER COLUMN can_be_telemetry_component SET DEFAULT FALSE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.instruments
       ALTER COLUMN can_be_telemetry_component SET NOT NULL;"
    )

    # Transmission method lookups ##########################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.transmission_method_families (
         transmission_method_family_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         family_code TEXT NOT NULL,
         family_name TEXT NOT NULL,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT transmission_method_families_code_key UNIQUE (family_code),
         CONSTRAINT transmission_method_families_name_key UNIQUE (family_name)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.transmission_method_families OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.transmission_method_families IS
       'Lookup of broad telemetry families used when data leaves a deployed logger, e.g. satellite, cellular, or network-based transmission.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_method_families.family_code IS
       'Stable short code for the transmission family, e.g. satellite, cellular, or network.';"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS transmission_method_families_user_modified
       ON instruments.transmission_method_families;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS transmission_method_families_update_modified
       ON instruments.transmission_method_families;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER transmission_method_families_user_modified
       BEFORE UPDATE ON instruments.transmission_method_families
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER transmission_method_families_update_modified
       BEFORE UPDATE ON instruments.transmission_method_families
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.transmission_method_families
         (family_code, family_name, description)
       VALUES
         ('satellite', 'Satellite',
          'Satellite telemetry such as GOES DCS, Iridium, or Inmarsat.'),
         ('cellular', 'Cellular',
          'Telemetry using cellular carriers for IP, SMS, or related services.'),
         ('radio', 'Radio',
          'Licensed or unlicensed terrestrial radio telemetry.'),
         ('network', 'Network / IP',
          'Telemetry over Ethernet, Wi-Fi, or similar IP-based network links.'),
         ('manual', 'Manual / local retrieval',
          'Manual site visits, local direct downloads, or removable-media retrieval.'),
         ('other', 'Other / custom',
          'Catch-all family for transmission setups that do not fit the current model cleanly.')
       ON CONFLICT (family_code) DO UPDATE
       SET family_name = EXCLUDED.family_name,
           description = EXCLUDED.description;"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.transmission_methods (
         transmission_method_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         method_code TEXT NOT NULL,
         method_name TEXT NOT NULL,
         transmission_method_family_id INTEGER NOT NULL
           REFERENCES instruments.transmission_method_families(transmission_method_family_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         uses_provider BOOLEAN NOT NULL DEFAULT FALSE,
         uses_platform_identifier BOOLEAN NOT NULL DEFAULT FALSE,
         uses_route_schedule BOOLEAN NOT NULL DEFAULT FALSE,
         uses_payload_format BOOLEAN NOT NULL DEFAULT FALSE,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT transmission_methods_code_key UNIQUE (method_code),
         CONSTRAINT transmission_methods_name_key UNIQUE (method_name)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.transmission_methods OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.transmission_methods IS
       'Lookup of specific telemetry methods used to move logger data outward, e.g. GOES DCS, cellular IP, or Iridium SBD.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_methods.method_code IS
       'Stable short code for the transmission method, e.g. GOES_DCS or CELLULAR_IP.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_methods.uses_provider IS
       'Whether the method usually needs a named provider or carrier, e.g. NESDIS, Iridium, or a cellular carrier.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_methods.uses_platform_identifier IS
       'Whether the method usually needs a platform-facing identifier such as a NESDIS ID, IMEI, ICCID, phone number, or terminal identifier.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_methods.uses_route_schedule IS
       'Whether the method usually has explicit route-level scheduling metadata such as first transmit time, interval, or window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_methods.uses_payload_format IS
       'Whether the method usually has an explicit route-level payload or message format such as SHEF, CSV, JSON, or a binary format.';"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS transmission_methods_user_modified
       ON instruments.transmission_methods;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS transmission_methods_update_modified
       ON instruments.transmission_methods;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER transmission_methods_user_modified
       BEFORE UPDATE ON instruments.transmission_methods
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER transmission_methods_update_modified
       BEFORE UPDATE ON instruments.transmission_methods
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.transmission_methods
         (method_code, method_name, transmission_method_family_id, uses_provider,
          uses_platform_identifier, uses_route_schedule, uses_payload_format,
          description)
       SELECT
         v.method_code,
         v.method_name,
         tmf.transmission_method_family_id,
         v.uses_provider,
         v.uses_platform_identifier,
         v.uses_route_schedule,
         v.uses_payload_format,
         v.description
       FROM (
         VALUES
           ('GOES_DCS', 'GOES DCS', 'satellite', TRUE, TRUE, TRUE, TRUE,
            'GOES Data Collection System transmissions routed through NESDIS or a comparable geostationary service.'),
           ('IRIDIUM_SBD', 'Iridium SBD', 'satellite', TRUE, TRUE, TRUE, TRUE,
            'Iridium Short Burst Data telemetry.'),
           ('IRIDIUM_IP', 'Iridium IP', 'satellite', TRUE, TRUE, TRUE, TRUE,
            'Iridium IP or direct IP telemetry over an Iridium terminal.'),
           ('INMARSAT', 'Inmarsat', 'satellite', TRUE, TRUE, TRUE, TRUE,
            'Inmarsat or related commercial satellite telemetry services.'),
           ('CELLULAR_IP', 'Cellular IP', 'cellular', TRUE, TRUE, TRUE, TRUE,
            'Cellular modem or logger telemetry over IP services such as HTTPS, MQTT, FTP, or VPN-backed links.'),
           ('CELLULAR_SMS', 'Cellular SMS', 'cellular', TRUE, TRUE, TRUE, TRUE,
            'Cellular short-message telemetry or alarm delivery.'),
           ('TERRESTRIAL_RADIO', 'Terrestrial radio', 'radio', TRUE, TRUE, TRUE, TRUE,
            'Licensed or unlicensed terrestrial radio telemetry.'),
           ('ETHERNET_PUSH', 'Ethernet push/pull', 'network', TRUE, FALSE, TRUE, TRUE,
            'Wired network telemetry using IP-based services.'),
           ('WIFI_PUSH', 'Wi-Fi push/pull', 'network', TRUE, FALSE, TRUE, TRUE,
            'Wi-Fi telemetry using IP-based services.'),
           ('MANUAL_DOWNLOAD', 'Manual / local download', 'manual', FALSE, FALSE, FALSE, FALSE,
            'Manual site visits, direct cable downloads, or removable-media retrieval.'),
           ('OTHER', 'Other / custom', 'other', FALSE, FALSE, FALSE, FALSE,
            'Catch-all method for unusual or not-yet-modelled telemetry setups.')
       ) AS v(
         method_code,
         method_name,
         family_code,
         uses_provider,
         uses_platform_identifier,
         uses_route_schedule,
         uses_payload_format,
         description
       )
       JOIN instruments.transmission_method_families tmf
         ON tmf.family_code = v.family_code
       ON CONFLICT (method_code) DO UPDATE
       SET method_name = EXCLUDED.method_name,
           transmission_method_family_id = EXCLUDED.transmission_method_family_id,
           uses_provider = EXCLUDED.uses_provider,
           uses_platform_identifier = EXCLUDED.uses_platform_identifier,
           uses_route_schedule = EXCLUDED.uses_route_schedule,
           uses_payload_format = EXCLUDED.uses_payload_format,
           description = EXCLUDED.description;"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS instruments.transmission_component_roles (
         transmission_component_role_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         role_code TEXT NOT NULL,
         role_name TEXT NOT NULL,
         description TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT transmission_component_roles_code_key UNIQUE (role_code),
         CONSTRAINT transmission_component_roles_name_key UNIQUE (role_name)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE instruments.transmission_component_roles OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE instruments.transmission_component_roles IS
       'Lookup of roles that a deployed instrument can play inside a telemetry setup, e.g. transmitter, antenna, or battery.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN instruments.transmission_component_roles.role_code IS
       'Stable short code for the telemetry component role, e.g. transmitter or primary_antenna.';"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS transmission_component_roles_user_modified
       ON instruments.transmission_component_roles;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS transmission_component_roles_update_modified
       ON instruments.transmission_component_roles;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER transmission_component_roles_user_modified
       BEFORE UPDATE ON instruments.transmission_component_roles
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER transmission_component_roles_update_modified
       BEFORE UPDATE ON instruments.transmission_component_roles
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "INSERT INTO instruments.transmission_component_roles
         (role_code, role_name, description)
       VALUES
         ('transmitter', 'Transmitter',
          'Dedicated transmitter, terminal, or logger-transmitter device used to send data off site.'),
         ('primary_antenna', 'Primary antenna',
          'Primary antenna used to radiate or receive the telemetry link.'),
         ('positioning_antenna', 'Positioning antenna',
          'GPS, GNSS, or similar positioning/time-reference antenna used by the telemetry setup.'),
         ('modem', 'Modem / terminal',
          'Dedicated modem, terminal, or network interface module that supports the telemetry path.'),
         ('sim', 'SIM / subscriber module',
          'SIM card or similar subscription-bound module associated with a telemetry service.'),
         ('battery', 'Battery',
          'Battery dedicated to the telemetry setup or telemetry enclosure.'),
         ('solar_panel', 'Solar panel',
          'Solar panel or charging source supporting telemetry equipment.'),
         ('power_controller', 'Power controller',
          'Charge controller, DC regulator, or related telemetry power interface.'),
         ('other', 'Other / custom',
          'Catch-all role for telemetry components not yet modelled explicitly.')
       ON CONFLICT (role_code) DO UPDATE
       SET role_name = EXCLUDED.role_name,
           description = EXCLUDED.description;"
    )

    # Transmission setup metadata ##########################################
    # One row represents a time-bounded telemetry setup for a deployed logger.
    # Route-specific scheduling and endpoint metadata lives in a child table;
    # attached transmission hardware lives in a separate component table.
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.locations_metadata_transmission_setups (
         transmission_setup_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         logger_metadata_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_instruments(metadata_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         transmission_method_id INTEGER NOT NULL
           REFERENCES instruments.transmission_methods(transmission_method_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         provider_name TEXT,
         platform_identifier TEXT,
         transmission_config JSONB NOT NULL DEFAULT '{}'::jsonb,
         note TEXT,
         start_datetime TIMESTAMPTZ NOT NULL,
         end_datetime TIMESTAMPTZ,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT locations_metadata_transmission_setups_period_valid
           CHECK (end_datetime IS NULL OR start_datetime < end_datetime)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_transmission_setups OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_transmission_setups IS
       'Temporal metadata describing how a deployed datalogger sends data outward via GOES, cellular, radio, network, or manual retrieval methods. Attached telemetry hardware and route-level schedules live in child tables.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.logger_metadata_id IS
       'Deployed logger record from public.locations_metadata_instruments for the logger that originates the telemetry stream.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.provider_name IS
       'Optional carrier or provider name such as NESDIS, Iridium, Telus, Bell, or another telemetry service provider.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.platform_identifier IS
       'Provider-facing identifier for the telemetry platform, e.g. NESDIS ID, IMEI, ICCID, phone number, terminal identifier, or radio callsign. This generalizes the older transmission_code concept.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.transmission_config IS
       'Method-wide settings stored as JSONB, e.g. APN, modem profile, satellite service plan, security settings, or bearer-level notes that apply across all routes in the setup.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.note IS
       'Free-text notes about the telemetry setup, service changes, provider transitions, or operational assumptions.';"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_setups_logger_idx
       ON public.locations_metadata_transmission_setups (logger_metadata_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_setups_method_idx
       ON public.locations_metadata_transmission_setups (transmission_method_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_setups_range_idx
       ON public.locations_metadata_transmission_setups
       (logger_metadata_id, start_datetime, end_datetime);"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_transmission_setups_user_modified
       ON public.locations_metadata_transmission_setups;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_transmission_setups_update_modified
       ON public.locations_metadata_transmission_setups;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_transmission_setups_user_modified
       BEFORE UPDATE ON public.locations_metadata_transmission_setups
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_transmission_setups_update_modified
       BEFORE UPDATE ON public.locations_metadata_transmission_setups
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_transmission_setup_bounds()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         logger_row RECORD;
         logger_can_be_logger BOOLEAN;
       BEGIN
         SELECT metadata_id, location_id, instrument_id, start_datetime,
                end_datetime
         INTO logger_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = NEW.logger_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Logger deployment % does not exist.',
             NEW.logger_metadata_id;
         END IF;

         IF logger_row.instrument_id IS NULL THEN
           RAISE EXCEPTION
             'Logger deployment % must reference a deployed instrument.',
             NEW.logger_metadata_id;
         END IF;

         SELECT can_be_logger
         INTO logger_can_be_logger
         FROM instruments.instruments
         WHERE instrument_id = logger_row.instrument_id;

         IF NOT FOUND OR logger_can_be_logger IS DISTINCT FROM TRUE THEN
           RAISE EXCEPTION
             'Logger deployment % must reference an instrument marked can_be_logger = TRUE.',
             NEW.logger_metadata_id;
         END IF;

         IF NEW.start_datetime < logger_row.start_datetime THEN
           RAISE EXCEPTION
             'Transmission setup start_datetime must not be earlier than the logger deployment start.';
         END IF;

         IF COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
            COALESCE(logger_row.end_datetime, 'infinity'::timestamptz) THEN
           RAISE EXCEPTION
             'Transmission setup end_datetime must not extend beyond the logger deployment period.';
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_transmission_setup_overlap()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_transmission_setups s
           WHERE s.transmission_setup_id <> NEW.transmission_setup_id
             AND s.logger_metadata_id = NEW.logger_metadata_id
             AND s.transmission_method_id = NEW.transmission_method_id
             AND COALESCE(s.provider_name, '') = COALESCE(NEW.provider_name, '')
             AND COALESCE(s.platform_identifier, '') =
               COALESCE(NEW.platform_identifier, '')
             AND NEW.start_datetime <
               COALESCE(s.end_datetime, 'infinity'::timestamptz)
             AND COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
               s.start_datetime
         ) THEN
           RAISE EXCEPTION
             'Duplicate overlapping telemetry setup detected for logger deployment %, method %, provider %, and platform identifier %.',
             NEW.logger_metadata_id,
             NEW.transmission_method_id,
             COALESCE(NEW.provider_name, '(none)'),
             COALESCE(NEW.platform_identifier, '(none)');
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_transmission_setup_bounds
       ON public.locations_metadata_transmission_setups;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_transmission_setup_overlap
       ON public.locations_metadata_transmission_setups;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_transmission_setup_bounds
       AFTER INSERT OR UPDATE ON public.locations_metadata_transmission_setups
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_transmission_setup_bounds();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_transmission_setup_overlap
       AFTER INSERT OR UPDATE ON public.locations_metadata_transmission_setups
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_transmission_setup_overlap();"
    )

    # Route-level transmission parameters ##################################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.locations_metadata_transmission_routes (
         transmission_route_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         transmission_setup_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_transmission_setups(transmission_setup_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         route_name TEXT NOT NULL,
         endpoint_identifier TEXT,
         message_format TEXT,
         schedule_reference_time_utc TIME,
         transmit_interval_seconds INTEGER,
         transmit_window_seconds INTEGER,
         payload_size_bytes INTEGER,
         route_config JSONB NOT NULL DEFAULT '{}'::jsonb,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT locations_metadata_transmission_routes_interval_valid
           CHECK (transmit_interval_seconds IS NULL OR transmit_interval_seconds > 0),
         CONSTRAINT locations_metadata_transmission_routes_window_valid
           CHECK (transmit_window_seconds IS NULL OR transmit_window_seconds >= 0),
         CONSTRAINT locations_metadata_transmission_routes_payload_valid
           CHECK (payload_size_bytes IS NULL OR payload_size_bytes > 0)
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_transmission_routes OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_transmission_routes IS
       'Route-level transmission parameters attached to a telemetry setup. One setup can have multiple named routes such as a GOES schedule, a cellular API push, or an alarm SMS path.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.route_name IS
       'Human-readable name for the route, e.g. Primary GOES schedule, AQTS API push, or Alarm SMS.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.endpoint_identifier IS
       'External endpoint or route identifier such as a GOES channel label, host name, URL, MQTT topic, phone number, email address, or file-drop target.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.message_format IS
       'Payload or message format used on this route, e.g. SHEF, CSV, JSON, XML, proprietary binary, or similar.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.schedule_reference_time_utc IS
       'Optional UTC time-of-day anchor for the route schedule, e.g. the first daily GOES transmission time. Leave NULL when the route has no fixed anchor or when schedule details only exist in route_config.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.transmit_interval_seconds IS
       'Optional repeat interval for the route in seconds.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.transmit_window_seconds IS
       'Optional transmission window or slot duration in seconds, e.g. a GOES DCS time window.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.payload_size_bytes IS
       'Optional expected payload size in bytes when that matters for the route or service plan.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_routes.route_config IS
       'Route-specific settings stored as JSONB, e.g. GOES channel and baud, HTTPS host/port/path, MQTT broker settings, SMS options, retry policy, or authentication metadata.';"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_routes_setup_idx
       ON public.locations_metadata_transmission_routes (transmission_setup_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_routes_endpoint_idx
       ON public.locations_metadata_transmission_routes (endpoint_identifier);"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS locations_metadata_transmission_routes_setup_route_name_key
       ON public.locations_metadata_transmission_routes
       (transmission_setup_id, route_name);"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_transmission_routes_user_modified
       ON public.locations_metadata_transmission_routes;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_transmission_routes_update_modified
       ON public.locations_metadata_transmission_routes;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_transmission_routes_user_modified
       BEFORE UPDATE ON public.locations_metadata_transmission_routes
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_transmission_routes_update_modified
       BEFORE UPDATE ON public.locations_metadata_transmission_routes
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    # Telemetry hardware attached to a setup ###############################
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.locations_metadata_transmission_components (
         transmission_component_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         transmission_setup_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_transmission_setups(transmission_setup_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         component_metadata_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_instruments(metadata_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         transmission_component_role_id INTEGER NOT NULL
           REFERENCES instruments.transmission_component_roles(transmission_component_role_id)
           ON DELETE RESTRICT ON UPDATE CASCADE,
         is_primary BOOLEAN NOT NULL DEFAULT FALSE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_transmission_components OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_transmission_components IS
       'Deployed instruments or accessories attached to a telemetry setup, e.g. a separate transmitter, GOES antenna, GPS antenna, battery, or solar panel.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_components.component_metadata_id IS
       'Deployed instrument record from public.locations_metadata_instruments for the component participating in the telemetry setup.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_components.transmission_component_role_id IS
       'Role played by the deployed component inside the telemetry setup, e.g. transmitter, primary antenna, or battery.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_components.is_primary IS
       'Whether this component is the primary component for its role within the telemetry setup.';"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_components_setup_idx
       ON public.locations_metadata_transmission_components (transmission_setup_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_components_component_idx
       ON public.locations_metadata_transmission_components (component_metadata_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS locations_metadata_transmission_components_role_idx
       ON public.locations_metadata_transmission_components (transmission_component_role_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS locations_metadata_transmission_components_setup_component_role_key
       ON public.locations_metadata_transmission_components
       (transmission_setup_id, component_metadata_id, transmission_component_role_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX IF NOT EXISTS locations_metadata_transmission_components_setup_primary_role_key
       ON public.locations_metadata_transmission_components
       (transmission_setup_id, transmission_component_role_id)
       WHERE is_primary;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_transmission_components_user_modified
       ON public.locations_metadata_transmission_components;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS locations_metadata_transmission_components_update_modified
       ON public.locations_metadata_transmission_components;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_transmission_components_user_modified
       BEFORE UPDATE ON public.locations_metadata_transmission_components
       FOR EACH ROW EXECUTE FUNCTION public.user_modified();"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER locations_metadata_transmission_components_update_modified
       BEFORE UPDATE ON public.locations_metadata_transmission_components
       FOR EACH ROW EXECUTE FUNCTION public.update_modified();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_transmission_component_bounds()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         setup_row RECORD;
         logger_row RECORD;
         component_row RECORD;
         logger_can_be_logger BOOLEAN;
         component_can_participate BOOLEAN;
       BEGIN
         SELECT transmission_setup_id, logger_metadata_id, start_datetime, end_datetime
         INTO setup_row
         FROM public.locations_metadata_transmission_setups
         WHERE transmission_setup_id = NEW.transmission_setup_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Transmission setup % does not exist.',
             NEW.transmission_setup_id;
         END IF;

         SELECT metadata_id, location_id, instrument_id
         INTO logger_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = setup_row.logger_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Logger deployment % for transmission setup % does not exist.',
             setup_row.logger_metadata_id,
             NEW.transmission_setup_id;
         END IF;

         IF logger_row.instrument_id IS NULL THEN
           RAISE EXCEPTION
             'Logger deployment % must reference a deployed instrument.',
             setup_row.logger_metadata_id;
         END IF;

         SELECT can_be_logger
         INTO logger_can_be_logger
         FROM instruments.instruments
         WHERE instrument_id = logger_row.instrument_id;

         IF NOT FOUND OR logger_can_be_logger IS DISTINCT FROM TRUE THEN
           RAISE EXCEPTION
             'Logger deployment % must reference an instrument marked can_be_logger = TRUE.',
             setup_row.logger_metadata_id;
         END IF;

         SELECT metadata_id, location_id, instrument_id, start_datetime, end_datetime
         INTO component_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = NEW.component_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Transmission component deployment % does not exist.',
             NEW.component_metadata_id;
         END IF;

         IF component_row.instrument_id IS NULL THEN
           RAISE EXCEPTION
             'Transmission component deployment % must reference a deployed instrument.',
             NEW.component_metadata_id;
         END IF;

         SELECT (can_be_telemetry_component OR can_be_logger)
         INTO component_can_participate
         FROM instruments.instruments
         WHERE instrument_id = component_row.instrument_id;

         IF NOT FOUND OR component_can_participate IS DISTINCT FROM TRUE THEN
           RAISE EXCEPTION
             'Transmission component deployment % must reference an instrument marked can_be_telemetry_component = TRUE or can_be_logger = TRUE.',
             NEW.component_metadata_id;
         END IF;

         IF component_row.location_id <> logger_row.location_id THEN
           RAISE EXCEPTION
             'Transmission component deployment % must belong to the same location as the logger deployment %.',
             NEW.component_metadata_id,
             setup_row.logger_metadata_id;
         END IF;

         IF setup_row.start_datetime < component_row.start_datetime THEN
           RAISE EXCEPTION
             'Telemetry component deployment % must start on or before the telemetry setup start_datetime.',
             NEW.component_metadata_id;
         END IF;

         IF COALESCE(setup_row.end_datetime, 'infinity'::timestamptz) >
            COALESCE(component_row.end_datetime, 'infinity'::timestamptz) THEN
           RAISE EXCEPTION
             'Telemetry component deployment % must remain active for the full telemetry setup period.',
             NEW.component_metadata_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_transmission_setup_component_bounds()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         logger_row RECORD;
         logger_can_be_logger BOOLEAN;
       BEGIN
         SELECT metadata_id, location_id, instrument_id
         INTO logger_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = NEW.logger_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Logger deployment % does not exist.',
             NEW.logger_metadata_id;
         END IF;

         IF logger_row.instrument_id IS NULL THEN
           RAISE EXCEPTION
             'Logger deployment % must reference a deployed instrument.',
             NEW.logger_metadata_id;
         END IF;

         SELECT can_be_logger
         INTO logger_can_be_logger
         FROM instruments.instruments
         WHERE instrument_id = logger_row.instrument_id;

         IF NOT FOUND OR logger_can_be_logger IS DISTINCT FROM TRUE THEN
           RAISE EXCEPTION
             'Logger deployment % must reference an instrument marked can_be_logger = TRUE.',
             NEW.logger_metadata_id;
         END IF;

         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_transmission_components tc
           JOIN public.locations_metadata_instruments c
             ON c.metadata_id = tc.component_metadata_id
           LEFT JOIN instruments.instruments ci
             ON ci.instrument_id = c.instrument_id
           WHERE tc.transmission_setup_id = NEW.transmission_setup_id
             AND (
               c.instrument_id IS NULL OR
               (
                 COALESCE(ci.can_be_telemetry_component, FALSE) IS DISTINCT FROM TRUE
                 AND COALESCE(ci.can_be_logger, FALSE) IS DISTINCT FROM TRUE
               ) OR
               c.location_id <> logger_row.location_id OR
               NEW.start_datetime < c.start_datetime OR
               COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(c.end_datetime, 'infinity'::timestamptz)
             )
         ) THEN
           RAISE EXCEPTION
             'Existing telemetry components do not span the full period of transmission setup %.',
             NEW.transmission_setup_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_transmission_component_bounds
       ON public.locations_metadata_transmission_components;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_transmission_setup_component_bounds
       ON public.locations_metadata_transmission_setups;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_transmission_component_bounds
       AFTER INSERT OR UPDATE ON public.locations_metadata_transmission_components
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_transmission_component_bounds();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_transmission_setup_component_bounds
       AFTER INSERT OR UPDATE ON public.locations_metadata_transmission_setups
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_transmission_setup_component_bounds();"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_locations_metadata_instruments_transmission_dependents()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_transmission_setups s
           JOIN public.locations_metadata_instruments l
             ON l.metadata_id = s.logger_metadata_id
           LEFT JOIN instruments.instruments li
             ON li.instrument_id = l.instrument_id
           WHERE s.logger_metadata_id = NEW.metadata_id
             AND (
               l.instrument_id IS NULL OR
               li.can_be_logger IS DISTINCT FROM TRUE OR
               s.start_datetime < l.start_datetime OR
               COALESCE(s.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(l.end_datetime, 'infinity'::timestamptz)
             )
         ) THEN
           RAISE EXCEPTION
             'Updating instrument deployment % would invalidate existing transmission setup metadata.',
             NEW.metadata_id;
         END IF;

         IF EXISTS (
           SELECT 1
           FROM public.locations_metadata_transmission_components tc
           JOIN public.locations_metadata_transmission_setups s
             ON s.transmission_setup_id = tc.transmission_setup_id
           JOIN public.locations_metadata_instruments l
             ON l.metadata_id = s.logger_metadata_id
           JOIN public.locations_metadata_instruments c
             ON c.metadata_id = tc.component_metadata_id
           LEFT JOIN instruments.instruments li
             ON li.instrument_id = l.instrument_id
           LEFT JOIN instruments.instruments ci
             ON ci.instrument_id = c.instrument_id
           WHERE (s.logger_metadata_id = NEW.metadata_id OR
                  tc.component_metadata_id = NEW.metadata_id)
             AND (
               l.instrument_id IS NULL OR
               li.can_be_logger IS DISTINCT FROM TRUE OR
               c.instrument_id IS NULL OR
               (
                 COALESCE(ci.can_be_telemetry_component, FALSE) IS DISTINCT FROM TRUE
                 AND COALESCE(ci.can_be_logger, FALSE) IS DISTINCT FROM TRUE
               ) OR
               c.location_id <> l.location_id OR
               s.start_datetime < l.start_datetime OR
               COALESCE(s.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(l.end_datetime, 'infinity'::timestamptz) OR
               s.start_datetime < c.start_datetime OR
               COALESCE(s.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(c.end_datetime, 'infinity'::timestamptz)
             )
         ) THEN
           RAISE EXCEPTION
             'Updating instrument deployment % would invalidate existing transmission component metadata.',
             NEW.metadata_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_instrument_transmission_capabilities()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
         IF NEW.can_be_logger IS DISTINCT FROM TRUE AND EXISTS (
           SELECT 1
           FROM public.locations_metadata_instruments lmi
           JOIN public.locations_metadata_transmission_setups s
             ON s.logger_metadata_id = lmi.metadata_id
           WHERE lmi.instrument_id = NEW.instrument_id
         ) THEN
           RAISE EXCEPTION
             'Instrument % is already in use as a transmission logger and cannot have can_be_logger set to FALSE.',
             NEW.instrument_id;
         END IF;

         IF NEW.can_be_logger IS DISTINCT FROM TRUE AND
            NEW.can_be_telemetry_component IS DISTINCT FROM TRUE AND EXISTS (
              SELECT 1
              FROM public.locations_metadata_instruments lmi
              JOIN public.locations_metadata_transmission_components tc
                ON tc.component_metadata_id = lmi.metadata_id
              WHERE lmi.instrument_id = NEW.instrument_id
            ) THEN
           RAISE EXCEPTION
             'Instrument % is already in use as a transmission component and cannot have both can_be_logger and can_be_telemetry_component set to FALSE.',
             NEW.instrument_id;
         END IF;

         RETURN NEW;
       END;
       $$;"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_locations_metadata_instruments_transmission_dependents
       ON public.locations_metadata_instruments;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_instrument_transmission_capabilities
       ON instruments.instruments;"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_locations_metadata_instruments_transmission_dependents
       AFTER UPDATE
       ON public.locations_metadata_instruments
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_locations_metadata_instruments_transmission_dependents();"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_instrument_transmission_capabilities
       AFTER UPDATE
       ON instruments.instruments
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION public.check_instrument_transmission_capabilities();"
    )

    # Clean up some legacy columns called 'updated', which duplicate column 'modified' and cause confusion and bloat. These are all not necessary.
    # Find all tables where there's a column called 'updated' AND a column called 'modified'
    updated <- DBI::dbGetQuery(
      con,
      "SELECT table_schema, table_name
                             FROM information_schema.columns
                             WHERE column_name = 'updated';"
    )
    modified <- DBI::dbGetQuery(
      con,
      "SELECT table_schema, table_name
                             FROM information_schema.columns
                             WHERE column_name = 'modified';"
    )
    modified <- modified[modified$table_name %in% updated$table_name, ]

    # Delete the 'updated' columns from the offending tables
    for (i in seq_len(nrow(modified))) {
      schema <- modified$table_schema[i]
      table <- modified$table_name[i]
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s.%s DROP COLUMN IF EXISTS updated;",
          schema,
          table
        )
      )
    }

    # Drop function 'public.update_updated'
    DBI::dbExecute(
      con,
      "DROP FUNCTION IF EXISTS public.update_updated() CASCADE;"
    )

    # Add audit tables for key data ###########
    # Two-part approach:
    # 1) a generic audit table for moderate-volume metadata tables, and
    # 2) specific audit tables for continuous measurement tables so lookups can stay keyed on timeseries/time rather than generic JSON filters.

    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS audit;")
    DBI::dbExecute(con, "ALTER SCHEMA audit OWNER TO admin;")
    DBI::dbExecute(con, "REVOKE ALL ON SCHEMA audit FROM PUBLIC;")

    audit_roles <- DBI::dbGetQuery(con, "SELECT rolname FROM pg_roles;")$rolname # Used later on to target role-specific permissions
    audit_editor_roles <- intersect(
      c("yg_editor_group", "yg_editor"),
      audit_roles
    )
    audit_reader_roles <- intersect(
      c("yg_reader_group", "yg_reader"),
      audit_roles
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit.general_log (
         log_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         schema_name TEXT NOT NULL,
         table_name TEXT NOT NULL,
         user_name TEXT NOT NULL,
         actor_user TEXT,
         application_name TEXT,
         action TEXT NOT NULL CHECK (action IN ('UPDATE', 'DELETE')),
         action_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
         original_data JSONB NOT NULL,
         new_data JSONB,
         changed_fields JSONB,
         transaction_id BIGINT NOT NULL DEFAULT txid_current()
       );"
    )
    DBI::dbExecute(con, "ALTER TABLE audit.general_log OWNER TO admin;")

    # Constraint to enforce that new_data and changed_fields are only populated for UPDATE actions, and are NULL for DELETE actions.
    DBI::dbExecute(
      con,
      "
      DO $$
      BEGIN
        IF NOT EXISTS (
          SELECT 1
          FROM pg_constraint
          WHERE conname = 'general_log_payload_consistency'
            AND conrelid = 'audit.general_log'::regclass
        ) THEN
          ALTER TABLE audit.general_log
            ADD CONSTRAINT general_log_payload_consistency
            CHECK (
              (action = 'DELETE' AND new_data IS NULL AND changed_fields IS NULL)
              OR
              (action = 'UPDATE' AND new_data IS NOT NULL AND changed_fields IS NOT NULL)
            );
        END IF;
      END
      $$;
      "
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_datetime_idx
       ON audit.general_log (action_timestamp DESC);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_schema_table_idx
       ON audit.general_log (schema_name, table_name);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_action_idx
       ON audit.general_log (action);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_user_idx
       ON audit.general_log (user_name);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_actor_idx
       ON audit.general_log (actor_user);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS general_log_changed_fields_idx
       ON audit.general_log USING GIN (changed_fields);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.general_log IS
       'General-purpose audit log for metadata and moderate-volume tables. Stores old and new rows plus a changed_fields diff for UPDATE actions.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.user_name IS
       'Effective PostgreSQL role executing the data change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.actor_user IS
       'Application-level actor when supplied via aquacache.audit_user; otherwise falls back to session_user.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.application_name IS
       'Current PostgreSQL application_name at the time of change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.original_data IS
       'JSONB payload capturing the row state before the change.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.new_data IS
       'JSONB payload capturing the row state after the change. NULL for DELETE actions.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN audit.general_log.changed_fields IS
       'JSONB payload containing only columns whose values changed on UPDATE. NULL for DELETE actions.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit.measurements_continuous_log (
         log_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         timeseries_id INTEGER NOT NULL,
         measurement_datetime TIMESTAMPTZ NOT NULL,
         user_name TEXT NOT NULL,
         actor_user TEXT,
         application_name TEXT,
         action TEXT NOT NULL CHECK (action IN ('UPDATE', 'DELETE')),
         action_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
         original_data JSONB NOT NULL,
         new_data JSONB,
         changed_fields JSONB,
         transaction_id BIGINT NOT NULL DEFAULT txid_current()
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.measurements_continuous_log OWNER TO admin;"
    )

    # Constraint to enforce that new_data and changed_fields are only populated for UPDATE actions, and are NULL for DELETE actions.
    DBI::dbExecute(
      con,
      "
      DO $$
      BEGIN
        IF NOT EXISTS (
          SELECT 1
          FROM pg_constraint
          WHERE conname = 'measurements_continuous_log_payload_consistency'
            AND conrelid = 'audit.measurements_continuous_log'::regclass
        ) THEN
          ALTER TABLE audit.measurements_continuous_log
            ADD CONSTRAINT measurements_continuous_log_payload_consistency
            CHECK (
              (action = 'DELETE' AND new_data IS NULL AND changed_fields IS NULL)
              OR
              (action = 'UPDATE' AND new_data IS NOT NULL AND changed_fields IS NOT NULL)
            );
        END IF;
      END
      $$;
      "
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS measurements_continuous_log_ts_dt_idx
       ON audit.measurements_continuous_log (timeseries_id, measurement_datetime DESC);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS measurements_continuous_log_action_timestamp_idx
       ON audit.measurements_continuous_log (action_timestamp DESC);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS measurements_continuous_log_changed_fields_idx
       ON audit.measurements_continuous_log USING GIN (changed_fields);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.measurements_continuous_log IS
       'Audit log for continuous.measurements_continuous keyed by timeseries_id and datetime for efficient review of point-level edits and deletions.';"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS audit.measurements_calculated_daily_log (
         log_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         timeseries_id INTEGER NOT NULL,
         measurement_date DATE NOT NULL,
         user_name TEXT NOT NULL,
         actor_user TEXT,
         application_name TEXT,
         action TEXT NOT NULL CHECK (action IN ('UPDATE', 'DELETE')),
         action_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
         original_data JSONB NOT NULL,
         new_data JSONB,
         changed_fields JSONB,
         transaction_id BIGINT NOT NULL DEFAULT txid_current()
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE audit.measurements_calculated_daily_log OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "
      DO $$
      BEGIN
        IF NOT EXISTS (
          SELECT 1
          FROM pg_constraint
          WHERE conname = 'measurements_calculated_daily_log_payload_consistency'
            AND conrelid = 'audit.measurements_calculated_daily_log'::regclass
        ) THEN
          ALTER TABLE audit.measurements_calculated_daily_log
            ADD CONSTRAINT measurements_calculated_daily_log_payload_consistency
            CHECK (
              (action = 'DELETE' AND new_data IS NULL AND changed_fields IS NULL)
              OR
              (action = 'UPDATE' AND new_data IS NOT NULL AND changed_fields IS NOT NULL)
            );
        END IF;
      END
      $$;
      "
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS measurements_calculated_daily_log_ts_date_idx
       ON audit.measurements_calculated_daily_log (timeseries_id, measurement_date DESC);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS measurements_calculated_daily_log_action_timestamp_idx
       ON audit.measurements_calculated_daily_log (action_timestamp DESC);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS measurements_calculated_daily_log_changed_fields_idx
       ON audit.measurements_calculated_daily_log USING GIN (changed_fields);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE audit.measurements_calculated_daily_log IS
       'Audit log for continuous.measurements_calculated_daily keyed by timeseries_id and date for efficient review of daily-value edits and deletions.';"
    )

    DBI::dbExecute(con, "REVOKE ALL ON ALL TABLES IN SCHEMA audit FROM PUBLIC;")
    DBI::dbExecute(
      con,
      "REVOKE ALL ON ALL SEQUENCES IN SCHEMA audit FROM PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER DEFAULT PRIVILEGES IN SCHEMA audit
       REVOKE ALL ON TABLES FROM PUBLIC;"
    )
    DBI::dbExecute(
      con,
      "ALTER DEFAULT PRIVILEGES IN SCHEMA audit
       REVOKE ALL ON SEQUENCES FROM PUBLIC;"
    )

    for (role_name in audit_editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf("GRANT USAGE ON SCHEMA audit TO %s;", quoted_role)
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON ALL TABLES IN SCHEMA audit TO %s;",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER DEFAULT PRIVILEGES IN SCHEMA audit
           GRANT SELECT ON TABLES TO %s;",
          quoted_role
        )
      )
    }
    for (role_name in audit_reader_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf("GRANT USAGE ON SCHEMA audit TO %s;", quoted_role)
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON ALL TABLES IN SCHEMA audit TO %s;",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER DEFAULT PRIVILEGES IN SCHEMA audit
           GRANT SELECT ON TABLES TO %s;",
          quoted_role
        )
      )
    }

    # Now make audit functions and apply triggers to all tables we want tracked
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.jsonb_changed_fields(
         old_row JSONB,
         new_row JSONB
       )
       RETURNS JSONB
       LANGUAGE sql
       IMMUTABLE
       AS $function$
         SELECT COALESCE(
           jsonb_object_agg(n.key, n.value),
           '{}'::jsonb
         )
         FROM jsonb_each(new_row) AS n(key, value)
         WHERE (old_row -> n.key) IS DISTINCT FROM n.value;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.jsonb_changed_fields(JSONB, JSONB) OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.if_modified_func()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := current_user::TEXT;
         v_actor_user TEXT := COALESCE(
           NULLIF(current_setting('aquacache.audit_user', true), ''),
           session_user::TEXT
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_old_data JSONB;
         v_new_data JSONB;
         v_changed_fields JSONB;
       BEGIN
         IF TG_OP = 'DELETE' THEN
           v_old_data := to_jsonb(OLD)
             - 'created' - 'modified' - 'created_by' - 'modified_by';

           INSERT INTO audit.general_log (
             schema_name,
             table_name,
             user_name,
             actor_user,
             application_name,
             action,
             original_data,
             new_data,
             changed_fields,
             action_timestamp,
             transaction_id
           ) VALUES (
             TG_TABLE_SCHEMA,
             TG_TABLE_NAME,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             v_old_data,
             NULL,
             NULL,
             CURRENT_TIMESTAMP,
             txid_current()
           );

           RETURN OLD;
         END IF;

         v_old_data := to_jsonb(OLD)
           - 'created' - 'modified' - 'created_by' - 'modified_by';
         v_new_data := to_jsonb(NEW)
           - 'created' - 'modified' - 'created_by' - 'modified_by';

         -- Ignore automatically maintained sync timestamps on timeseries
         IF TG_TABLE_SCHEMA = 'continuous' AND TG_TABLE_NAME = 'timeseries' THEN
           v_old_data := v_old_data
             - 'last_new_data' - 'last_synchronize' - 'last_daily_calculation' - 'end_datetime' - 'start_datetime' - 'last_synchronize' - 'last_new_data' - 'last_daily_calculation';
           v_new_data := v_new_data
             - 'last_new_data' - 'last_synchronize' - 'last_daily_calculation' - 'end_datetime' - 'start_datetime' - 'last_synchronize' - 'last_new_data' - 'last_daily_calculation';
         END IF;

         v_changed_fields := audit.jsonb_changed_fields(v_old_data, v_new_data);

         IF v_changed_fields = '{}'::jsonb THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.general_log (
           schema_name,
           table_name,
           user_name,
           actor_user,
           application_name,
           action,
           original_data,
           new_data,
           changed_fields,
           action_timestamp,
           transaction_id
         ) VALUES (
           TG_TABLE_SCHEMA,
           TG_TABLE_NAME,
           v_user_name,
           v_actor_user,
           v_application_name,
           TG_OP,
           v_old_data,
           v_new_data,
           v_changed_fields,
           CURRENT_TIMESTAMP,
           txid_current()
         );

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.if_modified_func() OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION audit.if_modified_func() IS
       'Generic audit trigger for moderate-volume tables. Captures UPDATE and DELETE actions into audit.general_log.';"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.log_measurements_continuous_change()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := current_user::TEXT;
         v_actor_user TEXT := COALESCE(
           NULLIF(current_setting('aquacache.audit_user', true), ''),
           session_user::TEXT
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_timeseries_id INTEGER;
         v_measurement_datetime TIMESTAMPTZ;
         v_old_data JSONB;
         v_new_data JSONB;
         v_changed_fields JSONB;
       BEGIN
         IF TG_OP = 'DELETE' THEN
           v_timeseries_id := OLD.timeseries_id;
           v_measurement_datetime := OLD.datetime;
           INSERT INTO audit.measurements_continuous_log (
             timeseries_id,
             measurement_datetime,
             user_name,
             actor_user,
             application_name,
             action,
             action_timestamp,
             original_data,
             new_data,
             changed_fields,
             transaction_id
           ) VALUES (
             v_timeseries_id,
             v_measurement_datetime,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             CURRENT_TIMESTAMP,
             to_jsonb(OLD),
             NULL,
             NULL,
             txid_current()
           );

           RETURN OLD;
         END IF;

         v_old_data := to_jsonb(OLD);
         v_new_data := to_jsonb(NEW);
         v_timeseries_id := NEW.timeseries_id;
         v_measurement_datetime := NEW.datetime;
         v_changed_fields := audit.jsonb_changed_fields(v_old_data, v_new_data);

         IF v_changed_fields = '{}'::jsonb THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.measurements_continuous_log (
           timeseries_id,
           measurement_datetime,
           user_name,
           actor_user,
           application_name,
           action,
           action_timestamp,
           original_data,
           new_data,
           changed_fields,
           transaction_id
         ) VALUES (
           v_timeseries_id,
           v_measurement_datetime,
           v_user_name,
           v_actor_user,
           v_application_name,
           TG_OP,
           CURRENT_TIMESTAMP,
           v_old_data,
           v_new_data,
           v_changed_fields,
           txid_current()
         );

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.log_measurements_continuous_change() OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION audit.log_measurements_calculated_daily_change()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, audit
       AS $function$
       DECLARE
         v_user_name TEXT := current_user::TEXT;
         v_actor_user TEXT := COALESCE(
           NULLIF(current_setting('aquacache.audit_user', true), ''),
           session_user::TEXT
         );
         v_application_name TEXT := NULLIF(
           current_setting('application_name', true),
           ''
         );
         v_timeseries_id INTEGER;
         v_measurement_date DATE;
         v_old_data JSONB;
         v_new_data JSONB;
         v_changed_fields JSONB;
       BEGIN
         IF TG_OP = 'DELETE' THEN
           v_timeseries_id := OLD.timeseries_id;
           v_measurement_date := OLD.date;
           INSERT INTO audit.measurements_calculated_daily_log (
             timeseries_id,
             measurement_date,
             user_name,
             actor_user,
             application_name,
             action,
             action_timestamp,
             original_data,
             new_data,
             changed_fields,
             transaction_id
           ) VALUES (
             v_timeseries_id,
             v_measurement_date,
             v_user_name,
             v_actor_user,
             v_application_name,
             TG_OP,
             CURRENT_TIMESTAMP,
             to_jsonb(OLD),
             NULL,
             NULL,
             txid_current()
           );

           RETURN OLD;
         END IF;

         v_old_data := to_jsonb(OLD);
         v_new_data := to_jsonb(NEW);
         v_timeseries_id := NEW.timeseries_id;
         v_measurement_date := NEW.date;
         v_changed_fields := audit.jsonb_changed_fields(v_old_data, v_new_data);

         IF v_changed_fields = '{}'::jsonb THEN
           RETURN NEW;
         END IF;

         INSERT INTO audit.measurements_calculated_daily_log (
           timeseries_id,
           measurement_date,
           user_name,
           actor_user,
           application_name,
           action,
           action_timestamp,
           original_data,
           new_data,
           changed_fields,
           transaction_id
         ) VALUES (
           v_timeseries_id,
           v_measurement_date,
           v_user_name,
           v_actor_user,
           v_application_name,
           TG_OP,
           CURRENT_TIMESTAMP,
           v_old_data,
           v_new_data,
           v_changed_fields,
           txid_current()
         );

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION audit.log_measurements_calculated_daily_change() OWNER TO admin;"
    )

    # Attach triggers to tables
    general_audit_tables <- data.frame(
      schema = c(
        "boreholes",
        "boreholes",
        "boreholes",
        "boreholes",
        "boreholes",
        "boreholes",
        "boreholes",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "continuous",
        "discrete",
        "discrete",
        "discrete",
        "discrete",
        "field",
        "field",
        "field",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "instruments",
        "public",
        "public",
        "public",
        "public",
        "public",
        "public",
        "public",
        "public",
        "public",
        "public",
        "public"
      ),
      table = c(
        "boreholes",
        "wells",
        "permafrost",
        "geology",
        "boreholes_documents",
        "casing_materials",
        "borehole_well_purposes",
        "timeseries",
        "corrections",
        "thresholds",
        "extrema",
        "forecasts",
        "timeseries_data_sharing_agreements",
        "samples",
        "results",
        "guidelines",
        "sample_series",
        "field_visit_images",
        "field_visit_instruments",
        "field_visits",
        "instruments",
        "calibrations",
        "calibrate_orp",
        "calibrate_ph",
        "calibrate_specific_conductance",
        "calibrate_temperature",
        "calibrate_turbidity",
        "instrument_maintenance",
        "sensors",
        "array_maintenance_changes",
        "communication_protocol_families",
        "communication_protocols",
        "transmission_method_families",
        "transmission_methods",
        "transmission_component_roles",
        "locations",
        "parameters",
        "datum_conversions",
        "locations_z",
        "sub_locations",
        "locations_metadata_instruments",
        "locations_metadata_instrument_connections",
        "locations_metadata_instrument_connection_signals",
        "locations_metadata_transmission_setups",
        "locations_metadata_transmission_routes",
        "locations_metadata_transmission_components"
      ),
      stringsAsFactors = FALSE
    )

    for (i in seq_len(nrow(general_audit_tables))) {
      schema_name <- general_audit_tables$schema[i]
      table_name <- general_audit_tables$table[i]
      full_table_name <- paste0(schema_name, ".", table_name)
      trigger_name <- paste0("audit_", table_name, "_trigger")

      exists_check <- DBI::dbGetQuery(
        con,
        "SELECT to_regclass($1) IS NOT NULL AS exists;",
        params = list(full_table_name)
      )
      if (!isTRUE(exists_check$exists[[1]])) {
        stop(
          "Audit target table does not exist: ",
          full_table_name,
          ". Please review patch_37 audit target list."
        )
      }

      # Drop existing trigger if it exists
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER IF EXISTS %s ON %s.%s;",
          trigger_name,
          schema_name,
          table_name
        )
      )

      # Create new trigger
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s
           AFTER UPDATE OR DELETE ON %s.%s
           FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func();",
          trigger_name,
          schema_name,
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS audit_measurements_continuous_trigger
       ON continuous.measurements_continuous;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_measurements_continuous_trigger
       AFTER UPDATE OR DELETE ON continuous.measurements_continuous
       FOR EACH ROW
       EXECUTE FUNCTION audit.log_measurements_continuous_change();"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS audit_measurements_calculated_daily_trigger
       ON continuous.measurements_calculated_daily;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_measurements_calculated_daily_trigger
       AFTER UPDATE OR DELETE ON continuous.measurements_calculated_daily
       FOR EACH ROW
       EXECUTE FUNCTION audit.log_measurements_calculated_daily_change();"
    )

    # Correct overly broad correction_types grants from patch 27 and ensure
    # editor roles can use the backing sequence when inserting new rows.
    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE continuous.correction_types FROM PUBLIC;"
    )
    correction_types_reader_roles <- intersect(
      c("public_reader", "yg_reader_group", "yg_reader"),
      audit_roles
    )
    for (role_name in correction_types_reader_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON TABLE continuous.correction_types TO %s;",
          quoted_role
        )
      )
    }

    correction_types_editor_roles <- intersect(
      c("yg_editor_group", "yg_editor"),
      audit_roles
    )
    for (role_name in correction_types_editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE continuous.correction_types TO %s;",
          quoted_role
        )
      )
    }

    DBI::dbExecute(
      con,
      "
      DO $$
      DECLARE
        correction_seq REGCLASS;
      BEGIN
        SELECT pg_get_serial_sequence(
          'continuous.correction_types',
          'correction_type_id'
        )::regclass
        INTO correction_seq;

        IF correction_seq IS NOT NULL THEN
          EXECUTE format(
            'REVOKE ALL ON SEQUENCE %s FROM PUBLIC;',
            correction_seq
          );
        END IF;
      END
      $$;
      "
    )
    for (role_name in correction_types_editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "
          DO $$
          DECLARE
            correction_seq REGCLASS;
          BEGIN
            SELECT pg_get_serial_sequence(
              'continuous.correction_types',
              'correction_type_id'
            )::regclass
            INTO correction_seq;

            IF correction_seq IS NOT NULL THEN
              EXECUTE format(
                'GRANT USAGE, SELECT, UPDATE ON SEQUENCE %%s TO %s;',
                correction_seq
              );
            END IF;
          END
          $$;
          ",
          quoted_role
        )
      )
    }

    # Wrap things up ########################################################
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '37'
       WHERE item = 'Last patch number';"
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

    message("Patch 37 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 37 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
