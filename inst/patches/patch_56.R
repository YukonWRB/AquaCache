# Patch 56 adds a source-adapter registry, provider-neutral transmission
# mappings, operational history, precise continuous-timeseries datetime
# metadata, borehole and well approval status, and governed well-construction
# catalogues.
# The first adapter using these objects retrieves GOES DCS transmissions from
# NESDIS/LRGS, but the schema also supports other providers and transports.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 56: adding the cross-domain source-adapter registry, provider-neutral transmission mappings, import-run history, precise continuous-timeseries datetime metadata, borehole and well approval status, and governed well-construction details. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before applying this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('public.locations_metadata_transmission_routes') IS NOT NULL AS has_routes,
         to_regclass('public.locations_metadata_transmission_setups') IS NOT NULL AS has_setups,
         to_regclass('public.locations_metadata_instruments') IS NOT NULL AS has_instrument_metadata,
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('discrete.sample_series') IS NOT NULL AS has_sample_series,
         to_regclass('files.image_series') IS NOT NULL AS has_image_series,
         to_regclass('spatial.raster_series_index') IS NOT NULL AS has_raster_series,
         to_regclass('instruments.transmission_methods') IS NOT NULL AS has_methods,
         to_regclass('public.approval_types') IS NOT NULL AS has_approval_types,
         to_regclass('boreholes.boreholes') IS NOT NULL AS has_boreholes,
         to_regclass('boreholes.wells') IS NOT NULL AS has_wells,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info,
         to_regprocedure('public.user_modified()') IS NOT NULL AS has_user_modified,
         to_regprocedure('public.update_modified()') IS NOT NULL AS has_update_modified,
         to_regprocedure('audit.if_modified_func()') IS NOT NULL AS has_audit_function"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 56 requires the transmission metadata, continuous, discrete, ",
        "image, raster, approval-type, borehole, well, audit, and version objects created by earlier patches."
      )
    }

    not_reviewed <- DBI::dbGetQuery(
      con,
      "SELECT approval_type_id
       FROM public.approval_types
       WHERE approval_type_code = 'N'"
    )
    if (
      nrow(not_reviewed) != 1L ||
        is.na(not_reviewed$approval_type_id[[1]])
    ) {
      stop(
        "Patch 56 requires exactly one public.approval_types row with code ",
        "'N' (Not reviewed)."
      )
    }
    not_reviewed_id <- as.integer(not_reviewed$approval_type_id[[1]])

    # Transmission setups belong directly to a location. A deployed logger is
    # useful metadata when known, but it must not be required to configure a
    # provider platform or ingest route.
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_transmission_setups
       ADD COLUMN location_id INTEGER"
    )
    DBI::dbExecute(
      con,
      "UPDATE public.locations_metadata_transmission_setups s
       SET location_id = lmi.location_id
       FROM public.locations_metadata_instruments lmi
       WHERE lmi.metadata_id = s.logger_metadata_id"
    )
    missing_setup_locations <- DBI::dbGetQuery(
      con,
      "SELECT transmission_setup_id
       FROM public.locations_metadata_transmission_setups
       WHERE location_id IS NULL"
    )
    if (nrow(missing_setup_locations) > 0L) {
      stop(
        "Patch 56 could not determine a location for transmission setup(s): ",
        paste(missing_setup_locations$transmission_setup_id, collapse = ", ")
      )
    }
    overlapping_setups <- DBI::dbGetQuery(
      con,
      "SELECT
         earlier.transmission_setup_id AS earlier_setup_id,
         later.transmission_setup_id AS later_setup_id
       FROM public.locations_metadata_transmission_setups earlier
       JOIN public.locations_metadata_transmission_setups later
         ON earlier.transmission_setup_id < later.transmission_setup_id
        AND earlier.location_id = later.location_id
        AND earlier.transmission_method_id = later.transmission_method_id
        AND COALESCE(earlier.provider_name, '') =
          COALESCE(later.provider_name, '')
        AND COALESCE(earlier.platform_identifier, '') =
          COALESCE(later.platform_identifier, '')
        AND earlier.start_datetime <
          COALESCE(later.end_datetime, 'infinity'::timestamptz)
        AND COALESCE(earlier.end_datetime, 'infinity'::timestamptz) >
          later.start_datetime"
    )
    if (nrow(overlapping_setups) > 0L) {
      conflicts <- paste0(
        overlapping_setups$earlier_setup_id,
        "/",
        overlapping_setups$later_setup_id
      )
      stop(
        "Patch 56 found overlapping transmission setups for the same ",
        "location, method, provider, and platform: ",
        paste(conflicts, collapse = ", ")
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE public.locations_metadata_transmission_setups
       DROP CONSTRAINT IF EXISTS
         locations_metadata_transmission_setups_logger_metadata_id_fkey,
       ALTER COLUMN logger_metadata_id DROP NOT NULL,
       ALTER COLUMN location_id SET NOT NULL,
       ADD CONSTRAINT locations_metadata_transmission_setups_location_fkey
         FOREIGN KEY (location_id)
         REFERENCES public.locations(location_id)
         ON DELETE CASCADE ON UPDATE CASCADE,
       ADD CONSTRAINT locations_metadata_transmission_setups_logger_fkey
         FOREIGN KEY (logger_metadata_id)
         REFERENCES public.locations_metadata_instruments(metadata_id)
         ON DELETE SET NULL ON UPDATE CASCADE"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.locations_metadata_transmission_setups IS
       'Temporal metadata describing how a location sends data outward through a provider platform. A deployed logger may be linked when known; attached telemetry hardware and route-level schedules live in child tables.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.location_id IS
       'Location that owns this telemetry setup and its routes. This direct relationship remains required when the originating logger deployment is unknown.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.locations_metadata_transmission_setups.logger_metadata_id IS
       'Optional deployed logger that originates the telemetry stream. When supplied, it must belong to location_id and span the setup period.'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX locations_metadata_transmission_setups_location_range_idx
       ON public.locations_metadata_transmission_setups
       (location_id, start_datetime, end_datetime)"
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
         IF NEW.logger_metadata_id IS NULL THEN
           RETURN NEW;
         END IF;

         SELECT metadata_id, location_id, instrument_id, start_datetime,
                end_datetime
         INTO logger_row
         FROM public.locations_metadata_instruments
         WHERE metadata_id = NEW.logger_metadata_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Logger deployment % does not exist.',
             NEW.logger_metadata_id;
         END IF;

         IF logger_row.location_id <> NEW.location_id THEN
           RAISE EXCEPTION
             'Logger deployment % belongs to location %, but transmission setup % belongs to location %.',
             NEW.logger_metadata_id,
             logger_row.location_id,
             NEW.transmission_setup_id,
             NEW.location_id;
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
       $$"
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
             AND s.location_id = NEW.location_id
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
             'Duplicate overlapping telemetry setup detected for location %, method %, provider %, and platform identifier %.',
             NEW.location_id,
             NEW.transmission_method_id,
             COALESCE(NEW.provider_name, '(none)'),
             COALESCE(NEW.platform_identifier, '(none)');
         END IF;

         RETURN NEW;
       END;
       $$"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_transmission_component_bounds()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         setup_row RECORD;
         component_row RECORD;
         component_can_participate BOOLEAN;
       BEGIN
         SELECT transmission_setup_id, location_id, start_datetime, end_datetime
         INTO setup_row
         FROM public.locations_metadata_transmission_setups
         WHERE transmission_setup_id = NEW.transmission_setup_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION 'Transmission setup % does not exist.',
             NEW.transmission_setup_id;
         END IF;

         SELECT metadata_id, location_id, instrument_id, start_datetime,
                end_datetime
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

         IF component_row.location_id <> setup_row.location_id THEN
           RAISE EXCEPTION
             'Transmission component deployment % belongs to location %, but transmission setup % belongs to location %.',
             NEW.component_metadata_id,
             component_row.location_id,
             NEW.transmission_setup_id,
             setup_row.location_id;
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
       $$"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.check_transmission_setup_component_bounds()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       BEGIN
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
               c.location_id <> NEW.location_id OR
               NEW.start_datetime < c.start_datetime OR
               COALESCE(NEW.end_datetime, 'infinity'::timestamptz) >
                 COALESCE(c.end_datetime, 'infinity'::timestamptz)
             )
         ) THEN
           RAISE EXCEPTION
             'Existing telemetry components do not match the location or full period of transmission setup %.',
             NEW.transmission_setup_id;
         END IF;

         RETURN NEW;
       END;
       $$"
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
               l.location_id <> s.location_id OR
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
           JOIN public.locations_metadata_instruments c
             ON c.metadata_id = tc.component_metadata_id
           LEFT JOIN public.locations_metadata_instruments l
             ON l.metadata_id = s.logger_metadata_id
           LEFT JOIN instruments.instruments li
             ON li.instrument_id = l.instrument_id
           LEFT JOIN instruments.instruments ci
             ON ci.instrument_id = c.instrument_id
           WHERE (s.logger_metadata_id = NEW.metadata_id OR
                  tc.component_metadata_id = NEW.metadata_id)
             AND (
               (
                 s.logger_metadata_id IS NOT NULL AND (
                   l.instrument_id IS NULL OR
                   li.can_be_logger IS DISTINCT FROM TRUE OR
                   l.location_id <> s.location_id OR
                   s.start_datetime < l.start_datetime OR
                   COALESCE(s.end_datetime, 'infinity'::timestamptz) >
                     COALESCE(l.end_datetime, 'infinity'::timestamptz)
                 )
               ) OR
               c.instrument_id IS NULL OR
               (
                 COALESCE(ci.can_be_telemetry_component, FALSE) IS DISTINCT FROM TRUE
                 AND COALESCE(ci.can_be_logger, FALSE) IS DISTINCT FROM TRUE
               ) OR
               c.location_id <> s.location_id OR
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
       $$"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE public.source_adapter_capabilities (
         source_fx TEXT NOT NULL,
         data_domain TEXT NOT NULL,
         adapter_kind TEXT NOT NULL DEFAULT 'standard',
         requires_transmission_mapping BOOLEAN NOT NULL DEFAULT FALSE,
         inject_timeseries_id BOOLEAN NOT NULL DEFAULT FALSE,
         parallel_group_strategy TEXT NOT NULL DEFAULT 'timeseries',
         parallel_group_args TEXT[] NOT NULL DEFAULT ARRAY[]::text[],
         allow_empty_initial_fetch BOOLEAN NOT NULL DEFAULT FALSE,
         transmission_method_codes TEXT[] NOT NULL DEFAULT ARRAY[]::text[],
         argument_schema JSONB NOT NULL DEFAULT
           '{\"schema_version\":1,\"arguments\":[]}'::jsonb,
         ui_config JSONB NOT NULL DEFAULT '{}'::jsonb,
         enabled BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT source_adapter_capabilities_pkey
           PRIMARY KEY (source_fx, data_domain),
         CONSTRAINT source_adapter_capabilities_source_not_blank
           CHECK (btrim(source_fx) <> ''),
         CONSTRAINT source_adapter_capabilities_domain_valid
           CHECK (
             data_domain IN ('continuous', 'discrete', 'image', 'raster')
           ),
         CONSTRAINT source_adapter_capabilities_kind_valid
           CHECK (adapter_kind IN ('standard', 'transmission')),
         CONSTRAINT source_adapter_capabilities_transmission_domain_valid
           CHECK (
             adapter_kind <> 'transmission'
             OR data_domain = 'continuous'
           ),
         CONSTRAINT source_adapter_capabilities_group_strategy_valid
           CHECK (
             parallel_group_strategy IN (
               'timeseries',
               'source_args',
               'transmission_platform'
             )
           ),
         CONSTRAINT source_adapter_capabilities_group_args_valid
           CHECK (
             parallel_group_strategy = 'source_args'
             OR cardinality(parallel_group_args) = 0
           ),
         CONSTRAINT source_adapter_capabilities_method_codes_valid
           CHECK (
             adapter_kind = 'transmission'
             OR cardinality(transmission_method_codes) = 0
           ),
         CONSTRAINT source_adapter_capabilities_argument_schema_object
           CHECK (
             jsonb_typeof(argument_schema) = 'object'
             AND argument_schema @> '{\"schema_version\":1}'::jsonb
             AND jsonb_typeof(argument_schema -> 'arguments') = 'array'
           ),
         CONSTRAINT source_adapter_capabilities_ui_config_object
           CHECK (jsonb_typeof(ui_config) = 'object')
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.source_adapter_capabilities OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.source_adapter_capabilities IS
       'Authoritative provider-neutral registry of enabled AquaCache source adapters by data domain and how they are configured, grouped, and invoked. AquaCache import workflows and clients such as YGwater use the same rows so new providers do not require function-name-specific UI or execution branches.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.source_adapter_capabilities.data_domain IS
       'AquaCache data domain in which the adapter may be assigned: continuous, discrete, image, or raster. One source function may be registered in more than one domain.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.source_adapter_capabilities.transmission_method_codes IS
       'Stable instruments.transmission_methods.method_code values that may be selected for this adapter. An empty array means the adapter does not restrict transmission methods.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.source_adapter_capabilities.argument_schema IS
       'Versioned catalogue of adapter arguments, their source classification, value types, validation rules, and generic client controls. AquaCache validates this document against the registered R function signature.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.source_adapter_capabilities.ui_config IS
       'Optional presentation defaults and labels for generic clients. Operational route and provider settings belong on transmission setup or route records, not in this registry.'"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO public.source_adapter_capabilities (
         source_fx,
         data_domain,
         adapter_kind,
         requires_transmission_mapping,
         inject_timeseries_id,
         parallel_group_strategy,
         parallel_group_args,
         allow_empty_initial_fetch,
         transmission_method_codes,
         ui_config,
         note
       ) VALUES
       (
         'downloadAquarius',
         'continuous',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves continuous observations from an Aquarius server.'
       ),
       (
         'downloadECCCwx',
         'continuous',
         'standard',
         FALSE,
         FALSE,
         'source_args',
         ARRAY['location', 'interval'],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Shares the weathercan cache for a station and interval.'
       ),
       (
         'downloadECCCwxMinute',
         'continuous',
         'standard',
         FALSE,
         FALSE,
         'source_args',
         ARRAY['location'],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Groups minute weather observations by station so one worker reuses its database connection.'
       ),
       (
         'downloadNESDIS',
         'continuous',
         'transmission',
         TRUE,
         TRUE,
         'transmission_platform',
         ARRAY[]::text[],
         TRUE,
         ARRAY['GOES_DCS'],
         '{\"provider_name\":\"NESDIS\",\"source_field_label\":\"Payload field\"}'::jsonb,
         'Retrieves GOES DCS payloads and maps one provider field to each AquaCache timeseries.'
       ),
       (
         'downloadNWIS',
         'continuous',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves continuous observations from the USGS NWIS service.'
       ),
       (
         'downloadRWIS',
         'continuous',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves continuous observations from the RWIS service.'
       ),
       (
         'downloadWSC',
         'continuous',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves continuous observations from Water Survey of Canada.'
       ),
       (
         'downloadECCCwq',
         'discrete',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves discrete water-quality samples and results from ECCC.'
       ),
       (
         'downloadEQWin',
         'discrete',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves discrete samples and results from an EQWin database.'
       ),
       (
         'downloadSnowCourse',
         'discrete',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves snow-course observations from the configured snow database.'
       ),
       (
         'downloadNupointImages',
         'image',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves images from a NuPoint camera service.'
       ),
       (
         'downloadWSCImages',
         'image',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves hydrometric camera images from Water Survey of Canada.'
       ),
       (
         'downloadCaLDAS',
         'raster',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves CaLDAS reanalysis rasters.'
       ),
       (
         'downloadERA5',
         'raster',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves ERA5 reanalysis rasters.'
       ),
       (
         'downloadHRDPA',
         'raster',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves HRDPA precipitation-analysis rasters.'
       ),
       (
         'downloadHRDPS',
         'raster',
         'standard',
         FALSE,
         FALSE,
         'timeseries',
         ARRAY[]::text[],
         FALSE,
         ARRAY[]::text[],
         '{}'::jsonb,
         'Retrieves HRDPS forecast rasters.'
       )"
    )

    adapter_argument_catalog <- local({
      user <- function(
        name,
        label,
        help,
        value_type = "character",
        control = "text",
        required = FALSE,
        default = NULL,
        choices = NULL,
        minimum = NULL,
        maximum = NULL,
        step = NULL,
        advanced = FALSE
      ) {
        sourceAdapterArgument(
          name = name,
          source = "user",
          help = help,
          label = label,
          value_type = value_type,
          control = control,
          required = required,
          default = default,
          choices = choices,
          minimum = minimum,
          maximum = maximum,
          step = step,
          advanced = advanced
        )
      }
      managed <- function(name, source, help) {
        sourceAdapterArgument(name = name, source = source, help = help)
      }
      schema <- function(...) list(...)

      list(
        continuous = list(
          downloadAquarius = schema(
            user(
              "location",
              "Aquarius location identifier",
              "Identifier of the location in the Aquarius server.",
              required = TRUE
            ),
            user(
              "parameter",
              "Aquarius parameter identifier",
              "Identifier of the parameter in the Aquarius server.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            user(
              "difference",
              "Calculate differences",
              "Convert cumulative observations to increments. Takes cumulative values, such as standpipe readings, and returns the difference between consecutive values rather than the values themselves.",
              value_type = "logical",
              control = "checkbox",
              default = FALSE
            ),
            user(
              "reset_drop",
              "Reset-drop threshold",
              "Drop used to identify a cumulative counter reset. Only used if 'Calculate differences' is TRUE.",
              value_type = "numeric",
              control = "numeric",
              default = 20,
              minimum = 0,
              advanced = TRUE
            ),
            user(
              "min_pos",
              "Minimum positive increment",
              "Smallest positive increment retained. Increments below this threshold are treated as noise and discarded. Only used if 'Calculate differences' is TRUE.",
              value_type = "numeric",
              control = "numeric",
              default = 0,
              minimum = 0,
              advanced = TRUE
            ),
            user(
              "max_gap",
              "Maximum differencing gap",
              "Maximum allowed gap in number of data points above which an increment is not recorded. Only used if 'Calculate differences' is TRUE.",
              value_type = "numeric",
              control = "numeric",
              default = 0,
              minimum = 0,
              advanced = TRUE
            ),
            managed(
              "login",
              "environment",
              "The function default reads AQUSER and AQPASS from the R environment."
            ),
            managed(
              "server",
              "environment",
              "The function default reads AQSERVER from the R environment."
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            )
          ),
          downloadECCCwx = schema(
            user(
              "location",
              "ECCC station ID",
              "ECCC Station ID used by weathercan. This is NOT the Nav Canada ID, the WMO ID, or the Climate ID. See R function weathercan::stations() for help finding the correct ID.",
              required = TRUE
            ),
            user(
              "parameter",
              "Weathercan column",
              "The target column for this timeseries as returned by weathercan::weather_dl(). Column names vary with interval.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            user(
              "interval",
              "Observation interval",
              "Interval supplied to weathercan::weather_dl().",
              control = "select",
              required = TRUE,
              choices = c("hour", "day", "month")
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            )
          ),
          downloadECCCwxMinute = schema(
            user(
              "location",
              "ECCC climate identifier",
              "A four-letter station code used by the SWOB realtime API, such as 'CVXY'",
              required = TRUE
            ),
            user(
              "parameter",
              "Parameter name",
              "The SWOB element name to extract such as \"temp\", \"wind_spd\", \"wind_dir\", \"wind_gust\", \"stn_press\", or \"dew_point\". See https://api.weather.gc.ca/collections/swob-realtime/queryables?f=html for the full list.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            )
          ),
          downloadNESDIS = schema(
            managed(
              "transmission_route_id",
              "runtime",
              "YGwater derives this from the selected transmission mapping and stores it with the source assignment. Direct calls may omit it when exactly one effective route is mapped to the timeseries."
            ),
            managed(
              "timeseries_id",
              "runtime",
              "AquaCache injects the current timeseries ID from the import queue."
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            ),
            managed(
              "client_path",
              "environment",
              "AquaCache resolves the OpenDCS launcher from NESDIS_LRGS_CLIENT, PATH, or DCSTOOL_HOME when a path is not supplied explicitly."
            ),
            managed(
              "username",
              "environment",
              "The function default reads NESDIS_LRGS_USER."
            ),
            managed(
              "password",
              "environment",
              "The function default reads NESDIS_LRGS_PASSWORD."
            ),
            managed(
              "servers",
              "runtime",
              "Resolved from the selected transmission route configuration."
            ),
            managed(
              "port",
              "runtime",
              "Resolved from the selected transmission route configuration."
            ),
            managed(
              "overwrite",
              "internal",
              "Reserved for direct calls and testing; YGwater does not pass it."
            ),
            managed(
              "write",
              "internal",
              "AquaCache imports use the function default write behaviour."
            ),
            managed(
              "raw_messages",
              "internal",
              "Reserved for parser tests and direct calls."
            ),
            managed(
              "payload_reference",
              "internal",
              "Managed by the transmission import workflow."
            ),
            managed(
              "parser",
              "internal",
              "Reserved for parser injection during testing or custom direct calls."
            ),
            managed(
              "cache",
              "internal",
              "AquaCache uses the adapter's default shared-cache behaviour."
            )
          ),
          downloadNWIS = schema(
            user(
              "location",
              "USGS/NWIS site identifier",
              "USGS/NWIS site identifier.",
              required = TRUE
            ),
            user(
              "parameter",
              "USGS/NWIS parameter code",
              "USGS/NWIS parameter code, e.g. 65 for instantaneous gauge level, 60 for mean daily flow, 61 for instantaneous flow (though beware, these two might be flipped), 10 for water temperature. See https://waterdata.usgs.gov/code-dictionary for a list of valid codes.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            user(
              "modifiedSince",
              "Modified-since filter",
              "Optional NWIS modified-since value for advanced retrieval control.",
              advanced = TRUE
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            )
          ),
          downloadRWIS = schema(
            user(
              "location",
              "RWIS location",
              "RWIS station or location identifier.",
              required = TRUE
            ),
            user(
              "parameter",
              "RWIS parameter",
              "RWIS parameter identifier.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            ),
            managed(
              "rwis",
              "internal",
              "Reserved for supplying pre-fetched RWIS data during direct calls or tests."
            )
          ),
          downloadWSC = schema(
            user(
              "location",
              "WSC station number",
              "Water Survey of Canada station number such as '09EA004'.",
              required = TRUE
            ),
            user(
              "parameter",
              "WSC parameter",
              "Water Survey of Canada parameter identifier. 47 for discharge primary (sensor derived), 46 for level, 5 for water temperature, 4 for equipment temperature, 1 for air temperature. See the full list using R function tidyhydat::param_id().",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored measurement."
            ),
            managed(
              "end_datetime",
              "runtime",
              "YGwater omits this argument so the function default uses the current time."
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            )
          )
        ),
        discrete = list(
          downloadECCCwq = schema(
            user(
              "location",
              "ECCC monitoring location",
              "Monitoring-location identifier used in the ECCC dataset.",
              required = TRUE
            ),
            user(
              "file",
              "ECCC data file or URL",
              "Local file path or URL for the ECCC water-quality export.",
              required = TRUE
            ),
            user(
              "key",
              "Import mapping key",
              "Database import-mapping source code or legacy mapping filename.",
              default = "downloadECCCeq1.csv",
              advanced = TRUE
            ),
            user(
              "tz",
              "Source timezone",
              "Timezone used for source datetimes.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the sample-series synchronization start."
            ),
            managed(
              "end_datetime",
              "runtime",
              "AquaCache passes the sample-series synchronization end or current time."
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            ),
            user(
              "warn_unmapped",
              "Warn about unmapped results",
              "Show warnings for source results without import mappings.",
              value_type = "logical",
              control = "checkbox",
              default = FALSE,
              advanced = TRUE
            )
          ),
          downloadEQWin = schema(
            user(
              "location",
              "EQWin station",
              "Station identifier in the EQWin database.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the sample-series synchronization start."
            ),
            managed(
              "end_datetime",
              "runtime",
              "AquaCache passes the sample-series synchronization end or current time."
            ),
            user(
              "EQpath",
              "EQWin database path",
              "Path to the EQWin Microsoft Access database.",
              required = TRUE
            ),
            user(
              "key",
              "Import mapping source",
              "Database import-mapping source code or legacy mapping key.",
              default = "EQWin",
              advanced = TRUE
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            ),
            managed(
              "EQCon",
              "runtime",
              "AquaCache opens and caches the EQWin connection using EQpath."
            ),
            user(
              "EQsource_id",
              "EQWin source identifier",
              "Optional stable source identifier used for import provenance.",
              advanced = TRUE
            ),
            user(
              "tz",
              "EQWin timezone",
              "Timezone used for EQWin datetimes.",
              default = "MST"
            ),
            user(
              "unknown_time_local",
              "Default local sample time",
              "Time assigned when the source contains a date without a time.",
              default = "12:00:00",
              advanced = TRUE
            ),
            user(
              "media_id",
              "Default media ID",
              "Fallback AquaCache media ID.",
              value_type = "integer",
              control = "numeric",
              minimum = 1,
              advanced = TRUE
            ),
            user(
              "collection_method",
              "Default collection method ID",
              "Fallback collection-method ID.",
              value_type = "integer",
              control = "numeric",
              minimum = 1,
              advanced = TRUE
            ),
            user(
              "sample_type",
              "Default sample type ID",
              "Fallback sample-type ID.",
              value_type = "integer",
              control = "numeric",
              minimum = 1,
              advanced = TRUE
            )
          ),
          downloadSnowCourse = schema(
            user(
              "location",
              "Snow-course location",
              "Location identifier in the snow database.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the sample-series synchronization start."
            ),
            managed(
              "end_datetime",
              "runtime",
              "AquaCache passes the sample-series synchronization end or current time."
            ),
            user(
              "old_loc",
              "Legacy snow location",
              "Optional legacy location identifier.",
              advanced = TRUE
            ),
            user(
              "adjust_start",
              "Start-date adjustment",
              "Optional adjustment passed to the snow-course adapter.",
              value_type = "numeric",
              control = "numeric",
              advanced = TRUE
            ),
            user(
              "adjust_end",
              "End-date adjustment",
              "Optional adjustment passed to the snow-course adapter.",
              value_type = "numeric",
              control = "numeric",
              advanced = TRUE
            ),
            user(
              "share_with",
              "Default sharing role",
              "Role assigned to imported snow-course data.",
              default = "yg_reader_group",
              advanced = TRUE
            ),
            managed(
              "con",
              "runtime",
              "AquaCache passes the active database connection."
            ),
            managed(
              "snowCon",
              "runtime",
              "AquaCache opens or reuses the configured snow-database connection."
            )
          )
        ),
        image = list(
          downloadNupointImages = schema(
            user(
              "location",
              "NuPoint camera identifier",
              "Camera or location identifier used by NuPoint.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored image."
            ),
            managed(
              "username",
              "environment",
              "The function default reads nupointUser from the R environment."
            ),
            managed(
              "password",
              "environment",
              "The function default reads nupointPass from the R environment."
            ),
            managed(
              "url",
              "environment",
              "The function default reads nupointServer from the R environment."
            ),
            managed(
              "port",
              "environment",
              "The function default reads nupointPort from the R environment."
            ),
            managed(
              "folder",
              "environment",
              "The function default reads nupointFolder from the R environment."
            ),
            managed(
              "save_path",
              "internal",
              "AquaCache uses the adapter-managed temporary download directory."
            ),
            managed(
              "delete",
              "internal",
              "AquaCache uses the function default cleanup behaviour."
            )
          ),
          downloadWSCImages = schema(
            user(
              "location",
              "WSC camera location",
              "Camera folder or station identifier in the WSC image service.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the instant after the latest stored image."
            ),
            managed(
              "username",
              "environment",
              "The function default reads ECCCUSER from the R environment."
            ),
            managed(
              "password",
              "environment",
              "The function default reads ECCCPASS from the R environment."
            ),
            managed(
              "url",
              "environment",
              "The function uses its configured WSC image-service URL default."
            ),
            managed(
              "save_path",
              "internal",
              "AquaCache uses the adapter-managed temporary download directory."
            )
          )
        ),
        raster = list(
          downloadCaLDAS = schema(
            user(
              "parameter",
              "CaLDAS parameter",
              "CaLDAS source parameter name.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the next raster datetime to retrieve."
            ),
            user(
              "clip",
              "Province or territory code",
              "Optional two-character clipping code."
            ),
            user(
              "hrs",
              "Hours to retrieve",
              "UTC analysis hours to download.",
              value_type = "numeric_vector",
              control = "multiselect",
              default = 0,
              choices = c(0, 3, 6, 9, 12, 15, 18, 21)
            )
          ),
          downloadERA5 = schema(
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the next raster datetime to retrieve."
            ),
            managed(
              "end_datetime",
              "runtime",
              "AquaCache passes an explicit end when requested; otherwise the function default is used."
            ),
            user(
              "clip",
              "Province or territory code",
              "Two-character clipping code.",
              default = "YT"
            ),
            user(
              "param",
              "ERA5 parameter",
              "ERA5 source parameter name.",
              control = "select",
              required = TRUE,
              choices = "APCP_Sfc"
            ),
            user(
              "user",
              "ECMWF user",
              "ECMWF authentication user. This value is persisted in source_fx_args.",
              control = "password",
              required = TRUE,
              advanced = TRUE
            ),
            user(
              "key",
              "ECMWF API key",
              "ECMWF authentication key. This value is persisted in source_fx_args.",
              control = "password",
              required = TRUE,
              advanced = TRUE
            ),
            user(
              "hrs",
              "Hours to retrieve",
              "UTC hours to download.",
              value_type = "numeric_vector",
              control = "multiselect",
              default = 0,
              choices = 0:23
            ),
            user(
              "batch",
              "Use batch request",
              "Use one batch request instead of sequential downloads.",
              value_type = "logical",
              control = "checkbox",
              default = TRUE
            )
          ),
          downloadHRDPA = schema(
            user(
              "parameter",
              "HRDPA parameter",
              "HRDPA source parameter name.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the next raster datetime to retrieve."
            ),
            user(
              "clip",
              "Province or territory code",
              "Optional two-character clipping code."
            )
          ),
          downloadHRDPS = schema(
            user(
              "parameter",
              "HRDPS parameter",
              "HRDPS source parameter name.",
              required = TRUE
            ),
            managed(
              "start_datetime",
              "runtime",
              "AquaCache passes the next raster datetime to retrieve."
            ),
            user(
              "clip",
              "Province or territory code",
              "Optional two-character clipping code."
            )
          )
        )
      )
    })
    for (data_domain in names(adapter_argument_catalog)) {
      domain_catalog <- adapter_argument_catalog[[data_domain]]
      for (source_fx in names(domain_catalog)) {
        registerSourceAdapterArguments(
          con = con,
          source_fx = source_fx,
          data_domain = data_domain,
          arguments = domain_catalog[[source_fx]],
          check_function = TRUE
        )
      }
    }

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_source_adapters (
         timeseries_source_adapter_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
             (SEQUENCE NAME continuous.timeseries_source_adapter_id_seq),
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id) ON DELETE CASCADE,
         data_domain TEXT GENERATED ALWAYS AS ('continuous'::text) STORED,
         source_fx TEXT NOT NULL,
         source_fx_args JSONB,
         fetch_priority SMALLINT,
         synchronize_priority SMALLINT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT timeseries_source_adapters_capability_fkey
           FOREIGN KEY (source_fx, data_domain)
           REFERENCES public.source_adapter_capabilities(source_fx, data_domain),
         CONSTRAINT timeseries_source_adapters_source_not_blank
           CHECK (btrim(source_fx) <> ''),
         CONSTRAINT timeseries_source_adapters_args_object
           CHECK (
             source_fx_args IS NULL
             OR jsonb_typeof(source_fx_args) = 'object'
           ),
         CONSTRAINT timeseries_source_adapters_fetch_priority_valid
           CHECK (fetch_priority IS NULL OR fetch_priority > 0),
         CONSTRAINT timeseries_source_adapters_sync_priority_valid
           CHECK (
             synchronize_priority IS NULL
             OR synchronize_priority > 0
           ),
         CONSTRAINT timeseries_source_adapters_has_purpose
           CHECK (
             fetch_priority IS NOT NULL
             OR synchronize_priority IS NOT NULL
           )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_source_adapters OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX timeseries_source_adapters_active_fetch_priority_key
       ON continuous.timeseries_source_adapters
         (timeseries_id, fetch_priority)
       WHERE active AND fetch_priority IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX timeseries_source_adapters_active_sync_priority_key
       ON continuous.timeseries_source_adapters
         (timeseries_id, synchronize_priority)
       WHERE active AND synchronize_priority IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX timeseries_source_adapters_source_idx
       ON continuous.timeseries_source_adapters (source_fx)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE discrete.sample_series_source_adapters (
         sample_series_source_adapter_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
             (SEQUENCE NAME discrete.sample_series_source_adapter_id_seq),
         sample_series_id INTEGER NOT NULL
           REFERENCES discrete.sample_series(sample_series_id) ON DELETE CASCADE,
         data_domain TEXT GENERATED ALWAYS AS ('discrete'::text) STORED,
         source_fx TEXT NOT NULL,
         source_fx_args JSONB,
         fetch_priority SMALLINT,
         synchronize_priority SMALLINT,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT sample_series_source_adapters_capability_fkey
           FOREIGN KEY (source_fx, data_domain)
           REFERENCES public.source_adapter_capabilities(source_fx, data_domain),
         CONSTRAINT sample_series_source_adapters_source_not_blank
           CHECK (btrim(source_fx) <> ''),
         CONSTRAINT sample_series_source_adapters_args_object
           CHECK (
             source_fx_args IS NULL
             OR jsonb_typeof(source_fx_args) = 'object'
           ),
         CONSTRAINT sample_series_source_adapters_fetch_priority_valid
           CHECK (fetch_priority IS NULL OR fetch_priority > 0),
         CONSTRAINT sample_series_source_adapters_sync_priority_valid
           CHECK (
             synchronize_priority IS NULL
             OR synchronize_priority > 0
           ),
         CONSTRAINT sample_series_source_adapters_has_purpose
           CHECK (
             fetch_priority IS NOT NULL
             OR synchronize_priority IS NOT NULL
           )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series_source_adapters OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX sample_series_source_adapters_active_fetch_priority_key
       ON discrete.sample_series_source_adapters
         (sample_series_id, fetch_priority)
       WHERE active AND fetch_priority IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX sample_series_source_adapters_active_sync_priority_key
       ON discrete.sample_series_source_adapters
         (sample_series_id, synchronize_priority)
       WHERE active AND synchronize_priority IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX sample_series_source_adapters_source_idx
       ON discrete.sample_series_source_adapters (source_fx)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE files.image_series_source_adapters (
         image_series_source_adapter_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
             (SEQUENCE NAME files.image_series_source_adapter_id_seq),
         img_series_id INTEGER NOT NULL
           REFERENCES files.image_series(img_series_id) ON DELETE CASCADE,
         data_domain TEXT GENERATED ALWAYS AS ('image'::text) STORED,
         source_fx TEXT NOT NULL,
         source_fx_args JSONB,
         fetch_priority SMALLINT NOT NULL,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT image_series_source_adapters_capability_fkey
           FOREIGN KEY (source_fx, data_domain)
           REFERENCES public.source_adapter_capabilities(source_fx, data_domain),
         CONSTRAINT image_series_source_adapters_source_not_blank
           CHECK (btrim(source_fx) <> ''),
         CONSTRAINT image_series_source_adapters_args_object
           CHECK (
             source_fx_args IS NULL
             OR jsonb_typeof(source_fx_args) = 'object'
           ),
         CONSTRAINT image_series_source_adapters_fetch_priority_valid
           CHECK (fetch_priority > 0)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.image_series_source_adapters OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX image_series_source_adapters_active_fetch_priority_key
       ON files.image_series_source_adapters (img_series_id, fetch_priority)
       WHERE active"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX image_series_source_adapters_source_idx
       ON files.image_series_source_adapters (source_fx)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE spatial.raster_series_source_adapters (
         raster_series_source_adapter_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
             (SEQUENCE NAME spatial.raster_series_source_adapter_id_seq),
         raster_series_id INTEGER NOT NULL
           REFERENCES spatial.raster_series_index(raster_series_id)
           ON DELETE CASCADE,
         data_domain TEXT GENERATED ALWAYS AS ('raster'::text) STORED,
         source_fx TEXT NOT NULL,
         source_fx_args JSONB,
         fetch_priority SMALLINT NOT NULL,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT raster_series_source_adapters_capability_fkey
           FOREIGN KEY (source_fx, data_domain)
           REFERENCES public.source_adapter_capabilities(source_fx, data_domain),
         CONSTRAINT raster_series_source_adapters_source_not_blank
           CHECK (btrim(source_fx) <> ''),
         CONSTRAINT raster_series_source_adapters_args_object
           CHECK (
             source_fx_args IS NULL
             OR jsonb_typeof(source_fx_args) = 'object'
           ),
         CONSTRAINT raster_series_source_adapters_fetch_priority_valid
           CHECK (fetch_priority > 0)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.raster_series_source_adapters OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX raster_series_source_adapters_active_fetch_priority_key
       ON spatial.raster_series_source_adapters
         (raster_series_id, fetch_priority)
       WHERE active"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX raster_series_source_adapters_source_idx
       ON spatial.raster_series_source_adapters (source_fx)"
    )

    missing_assignments <- DBI::dbGetQuery(
      con,
      "SELECT 'continuous' AS data_domain, t.source_fx
       FROM continuous.timeseries t
       LEFT JOIN public.source_adapter_capabilities sac
         ON sac.source_fx = t.source_fx
        AND sac.data_domain = 'continuous'
       WHERE t.source_fx IS NOT NULL
         AND t.source_fx != 'downloadSynthetic'
         AND sac.source_fx IS NULL
       UNION
       SELECT 'discrete' AS data_domain, ss.source_fx
       FROM discrete.sample_series ss
       LEFT JOIN public.source_adapter_capabilities sac
         ON sac.source_fx = ss.source_fx
        AND sac.data_domain = 'discrete'
       WHERE ss.source_fx IS NOT NULL
         AND ss.source_fx != 'downloadSyntheticDiscrete'
         AND sac.source_fx IS NULL
       UNION
       SELECT 'image' AS data_domain, i.source_fx
       FROM files.image_series i
       LEFT JOIN public.source_adapter_capabilities sac
         ON sac.source_fx = i.source_fx
        AND sac.data_domain = 'image'
       WHERE i.source_fx IS NOT NULL
         AND sac.source_fx IS NULL
       UNION
       SELECT 'raster' AS data_domain, r.source_fx
       FROM spatial.raster_series_index r
       LEFT JOIN public.source_adapter_capabilities sac
         ON sac.source_fx = r.source_fx
        AND sac.data_domain = 'raster'
       WHERE r.source_fx IS NOT NULL
         AND sac.source_fx IS NULL
       ORDER BY data_domain, source_fx"
    )
    if (nrow(missing_assignments) > 0L) {
      stop(
        "Patch 56 cannot migrate source adapters that are absent from ",
        "public.source_adapter_capabilities: ",
        paste(
          paste0(
            missing_assignments$data_domain,
            "/",
            missing_assignments$source_fx
          ),
          collapse = ", "
        ),
        ". Register them in patch_56.R before applying the patch."
      )
    }

    DBI::dbExecute(
      con,
      "INSERT INTO continuous.timeseries_source_adapters (
         timeseries_id,
         source_fx,
         source_fx_args,
         fetch_priority,
         synchronize_priority,
         active,
         note
       )
       SELECT
         timeseries_id,
         source_fx,
         CASE
          WHEN jsonb_typeof(source_fx_args) = 'array'
            THEN source_fx_args -> 0
          ELSE source_fx_args
         END,
         1,
         1,
         TRUE,
         'Migrated from continuous.timeseries by Patch 56.'
       FROM continuous.timeseries
       WHERE source_fx IS NOT NULL
       AND source_fx != 'downloadSynthetic'"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.sample_series_source_adapters (
         sample_series_id,
         source_fx,
         source_fx_args,
         fetch_priority,
         synchronize_priority,
         active,
         note
       )
       SELECT
         sample_series_id,
         source_fx,
         CASE
          WHEN jsonb_typeof(source_fx_args) = 'array'
            THEN source_fx_args -> 0
          ELSE source_fx_args
         END,
         1,
         1,
         TRUE,
         'Migrated from discrete.sample_series by Patch 56.'
       FROM discrete.sample_series
       WHERE source_fx IS NOT NULL
       AND source_fx != 'downloadSyntheticDiscrete'"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO files.image_series_source_adapters (
         img_series_id, 
         source_fx, 
         source_fx_args, 
         fetch_priority,
         active, 
         note
       )
       SELECT 
       img_series_id, 
       source_fx, 
       CASE
         WHEN jsonb_typeof(source_fx_args) = 'array'
           THEN source_fx_args -> 0
         ELSE source_fx_args
       END,
       1,
       TRUE,
       'Migrated from files.image_series by Patch 56.'
       FROM files.image_series
       WHERE source_fx IS NOT NULL"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO spatial.raster_series_source_adapters (
         raster_series_id, 
         source_fx, 
         source_fx_args, 
         fetch_priority,
         active, note
       )
       SELECT 
       raster_series_id, 
       source_fx, 
       CASE
         WHEN jsonb_typeof(source_fx_args) = 'array'
           THEN source_fx_args -> 0
         ELSE source_fx_args
       END, 
       1, 
       TRUE,
       'Migrated from spatial.raster_series_index by Patch 56.'
       FROM spatial.raster_series_index
       WHERE source_fx IS NOT NULL"
    )

    # Patch 53 normalized non-basic timeseries through the legacy source
    # columns. Replace that enforcement before dropping the columns, and make
    # source assignments themselves basic-timeseries-only.
    DBI::dbExecute(
      con,
      "DROP TRIGGER normalize_nonbasic_timeseries_metadata_tr
       ON continuous.timeseries"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       DROP CONSTRAINT timeseries_nonbasic_no_remote_sync_ck"
    )
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.normalize_nonbasic_timeseries_metadata()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NEW.timeseries_type <> 'basic' THEN
           NEW.active := TRUE;
           NEW.sync_remote := FALSE;
           NEW.default_owner := NULL;
           NEW.default_data_sharing_agreement_id := NULL;

           IF TG_OP = 'UPDATE' THEN
             DELETE FROM continuous.timeseries_source_adapters
             WHERE timeseries_id = NEW.timeseries_id;
           END IF;

           IF NEW.timeseries_type = 'compound' THEN
             NEW.record_rate :=
               continuous.fastest_compound_member_record_rate(NEW.timeseries_id);
           ELSE
             NEW.record_rate := NULL;
           END IF;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.normalize_nonbasic_timeseries_metadata()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER normalize_nonbasic_timeseries_metadata_tr
       BEFORE INSERT OR UPDATE OF
         timeseries_type,
         record_rate,
         default_owner,
         default_data_sharing_agreement_id,
         active,
         sync_remote
       ON continuous.timeseries
       FOR EACH ROW
       EXECUTE FUNCTION continuous.normalize_nonbasic_timeseries_metadata()"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ADD CONSTRAINT timeseries_nonbasic_no_remote_sync_ck
       CHECK (
         timeseries_type = 'basic'
         OR (active IS TRUE AND sync_remote IS FALSE)
       )"
    )

    # Calculated-daily rows are a derived cache, not source observations. Patch
    # 41 allowed their date-only values to update timeseries datetime bounds by
    # converting each date to midnight UTC. For a series whose first source
    # observation occurred later that day, the daily-cache insert therefore
    # replaced the precise timestamp with 00:00:00. Keep bounds tied to the
    # source measurements and remove the cache-to-metadata feedback path.
    message(
      "Fixing issue with timeseries start datetime metadata where the start defaulted to the day's start rather than the actual data start. Recalculation can take a while."
    )
    for (trigger_name in c(
      "refresh_basic_metadata_on_daily_measurements_insert_tr",
      "refresh_basic_metadata_on_daily_measurements_update_tr",
      "refresh_basic_metadata_on_daily_measurements_delete_tr"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "DROP TRIGGER %s ON continuous.measurements_calculated_daily",
          trigger_name
        )
      )
    }
    for (function_name in c(
      "refresh_basic_metadata_on_daily_measurements_insert",
      "refresh_basic_metadata_on_daily_measurements_update",
      "refresh_basic_metadata_on_daily_measurements_delete"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "DROP FUNCTION continuous.%s()",
          function_name
        )
      )
    }
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.refresh_basic_timeseries_datetime_bounds(
         p_timeseries_ids INTEGER[]
       )
       RETURNS void
       LANGUAGE sql
       AS $function$
         WITH ids AS (
           SELECT DISTINCT x.timeseries_id
           FROM unnest(p_timeseries_ids) AS x(timeseries_id)
           WHERE x.timeseries_id IS NOT NULL
         ),
         bounds AS (
           SELECT
             ids.timeseries_id,
             MIN(mc.datetime) AS start_datetime,
             MAX(mc.datetime) AS end_datetime
           FROM ids
           LEFT JOIN continuous.measurements_continuous mc
             ON mc.timeseries_id = ids.timeseries_id
           GROUP BY ids.timeseries_id
         )
         UPDATE continuous.timeseries t
         SET
           start_datetime = b.start_datetime,
           end_datetime = b.end_datetime
         FROM bounds b
         WHERE t.timeseries_id = b.timeseries_id
           AND t.timeseries_type = 'basic'
           AND (
             t.start_datetime IS DISTINCT FROM b.start_datetime OR
             t.end_datetime IS DISTINCT FROM b.end_datetime
           );
       $function$"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON FUNCTION continuous.refresh_basic_timeseries_datetime_bounds(INTEGER[]) IS
       'Recomputes start_datetime and end_datetime exactly for the supplied basic continuous timeseries_ids from source rows in measurements_continuous. The derived measurements_calculated_daily cache does not define source-data bounds.'"
    )
    DBI::dbExecute(
      con,
      "SELECT continuous.refresh_basic_timeseries_datetime_bounds(
         ARRAY(
           SELECT timeseries_id
           FROM continuous.timeseries
           WHERE timeseries_type = 'basic'
         )
       )"
    )

    datetime_bound_errors <- DBI::dbGetQuery(
      con,
      "SELECT
         t.timeseries_id,
         t.start_datetime,
         MIN(mc.datetime) AS expected_start_datetime,
         t.end_datetime,
         MAX(mc.datetime) AS expected_end_datetime
       FROM continuous.timeseries t
       LEFT JOIN continuous.measurements_continuous mc
         ON mc.timeseries_id = t.timeseries_id
       WHERE t.timeseries_type = 'basic'
       GROUP BY
         t.timeseries_id,
         t.start_datetime,
         t.end_datetime
       HAVING
         t.start_datetime IS DISTINCT FROM MIN(mc.datetime)
         OR t.end_datetime IS DISTINCT FROM MAX(mc.datetime)"
    )
    if (nrow(datetime_bound_errors) > 0L) {
      stop(
        "Patch 56 could not align continuous.timeseries datetime bounds with ",
        "the source continuous measurements for timeseries_id(s): ",
        paste(datetime_bound_errors$timeseries_id, collapse = ", ")
      )
    }
    DBI::dbExecute(
      con,
      "CREATE FUNCTION continuous.check_basic_timeseries_source_adapter()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           WHERE t.timeseries_id = NEW.timeseries_id
             AND t.timeseries_type = 'basic'
         ) THEN
           RAISE EXCEPTION
             'Source adapters can only be assigned to basic timeseries; timeseries % is not basic.',
             NEW.timeseries_id;
         END IF;
         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.check_basic_timeseries_source_adapter()
       OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_basic_timeseries_source_adapter
       AFTER INSERT OR UPDATE ON continuous.timeseries_source_adapters
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.check_basic_timeseries_source_adapter()"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       DROP COLUMN source_fx,
       DROP COLUMN source_fx_args"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series
       DROP COLUMN source_fx,
       DROP COLUMN source_fx_args"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.image_series
       DROP COLUMN source_fx,
       DROP COLUMN source_fx_args"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.raster_series_index
       DROP COLUMN source_fx,
       DROP COLUMN source_fx_args"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_source_adapters IS
       'Source-adapter assignments for continuous timeseries. The lowest active non-null fetch or synchronize priority selects the adapter for that operation; higher priorities are retained as explicit standby choices.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE discrete.sample_series_source_adapters IS
       'Source-adapter assignments for discrete sample series. The lowest active non-null fetch or synchronize priority selects the adapter for that operation; higher priorities are retained as explicit standby choices.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_source_adapters.active IS
       'Whether this adapter assignment may be selected. Inactive assignments and their configuration are retained but ignored by automated workflows.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.sample_series_source_adapters.active IS
       'Whether this adapter assignment may be selected. Inactive assignments and their configuration are retained but ignored by automated workflows.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE files.image_series_source_adapters IS
       'Source-adapter assignments for image series. The lowest active fetch priority selects the adapter; inactive assignments retain their configuration.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE spatial.raster_series_source_adapters IS
       'Source-adapter assignments for raster series. The lowest active fetch priority selects the adapter; inactive assignments retain their configuration.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.transmission_timeseries_mappings (
         transmission_mapping_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         transmission_route_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_transmission_routes(transmission_route_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         source_field TEXT NOT NULL,
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         value_multiplier NUMERIC NOT NULL DEFAULT 1,
         value_offset NUMERIC NOT NULL DEFAULT 0,
         missing_values JSONB NOT NULL DEFAULT '[]'::jsonb,
         mapping_config JSONB NOT NULL DEFAULT '{}'::jsonb,
         enabled BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT transmission_timeseries_mappings_source_not_blank
           CHECK (btrim(source_field) <> ''),
         CONSTRAINT transmission_timeseries_mappings_multiplier_nonzero
           CHECK (value_multiplier <> 0),
         CONSTRAINT transmission_timeseries_mappings_missing_values_array
           CHECK (jsonb_typeof(missing_values) = 'array'),
         CONSTRAINT transmission_timeseries_mappings_config_object
           CHECK (jsonb_typeof(mapping_config) = 'object'),
         CONSTRAINT transmission_timeseries_mappings_route_source_key
           UNIQUE (transmission_route_id, source_field),
         CONSTRAINT transmission_timeseries_mappings_route_timeseries_key
           UNIQUE (transmission_route_id, timeseries_id)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.transmission_timeseries_mappings OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.transmission_timeseries_mappings IS
       'Maps a provider or transport source field to one AquaCache basic continuous timeseries. Route-specific mappings allow one shared payload to feed multiple locations and can be reused by GOES, Iridium, cellular, network, and file-based import adapters.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_timeseries_mappings.source_field IS
       'Exact provider or payload field name, including underscore-delimited fields, JSON paths, CSV headings, or protocol metadata names.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_timeseries_mappings.missing_values IS
       'JSON array of source values that must be treated as missing before multiplier and offset conversion.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.transmission_timeseries_mappings.mapping_config IS
       'Adapter-specific mapping settings such as extraction paths, units, quality-code handling, or future transformation rules.'"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX transmission_timeseries_mappings_timeseries_idx
       ON continuous.transmission_timeseries_mappings (timeseries_id)"
    )

    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.transmission_import_runs (
         transmission_import_run_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         transmission_route_id INTEGER NOT NULL
           REFERENCES public.locations_metadata_transmission_routes(transmission_route_id)
           ON DELETE CASCADE ON UPDATE CASCADE,
         query_since TIMESTAMPTZ NOT NULL,
         query_until TIMESTAMPTZ NOT NULL,
         importer TEXT NOT NULL,
         source_server TEXT,
         status TEXT NOT NULL,
         payload_bytes BIGINT NOT NULL DEFAULT 0,
         transmissions_received INTEGER NOT NULL DEFAULT 0,
         measurements_parsed INTEGER NOT NULL DEFAULT 0,
         measurements_inserted INTEGER NOT NULL DEFAULT 0,
         last_message_datetime TIMESTAMPTZ,
         payload_reference TEXT,
         source_metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
         error_message TEXT,
         started TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
         completed TIMESTAMPTZ,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT transmission_import_runs_period_valid
           CHECK (query_since < query_until),
         CONSTRAINT transmission_import_runs_importer_not_blank
           CHECK (btrim(importer) <> ''),
         CONSTRAINT transmission_import_runs_status_valid
           CHECK (status IN ('running', 'success', 'no_data', 'failed')),
         CONSTRAINT transmission_import_runs_metadata_object
           CHECK (jsonb_typeof(source_metadata) = 'object'),
         CONSTRAINT transmission_import_runs_counts_valid
           CHECK (
             payload_bytes >= 0
             AND transmissions_received >= 0
             AND measurements_parsed >= 0
             AND measurements_inserted >= 0
           ),
         CONSTRAINT transmission_import_runs_completion_valid
           CHECK (
             (status = 'running' AND completed IS NULL)
             OR (status <> 'running' AND completed IS NOT NULL)
           )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.transmission_import_runs OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.transmission_import_runs IS
       'Provider-neutral operational history for transmission imports. Successful and no-data query windows provide a durable retrieval cursor for each route, while importer and source metadata identify the adapter used.'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX transmission_import_runs_route_completed_idx
       ON continuous.transmission_import_runs
       (transmission_route_id, completed DESC)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX transmission_import_runs_cursor_idx
       ON continuous.transmission_import_runs
       (transmission_route_id, query_until DESC)
       WHERE status IN ('success', 'no_data')"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION continuous.check_transmission_timeseries_mapping()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $$
       DECLARE
         route_location_id INTEGER;
         target_location_id INTEGER;
         target_timeseries_type TEXT;
       BEGIN
         SELECT s.location_id
         INTO route_location_id
         FROM public.locations_metadata_transmission_routes r
         JOIN public.locations_metadata_transmission_setups s
           ON s.transmission_setup_id = r.transmission_setup_id
         WHERE r.transmission_route_id = NEW.transmission_route_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Transmission route % does not exist.',
             NEW.transmission_route_id;
         END IF;

         SELECT location_id, timeseries_type
         INTO target_location_id, target_timeseries_type
         FROM continuous.timeseries
         WHERE timeseries_id = NEW.timeseries_id;

         IF NOT FOUND THEN
           RAISE EXCEPTION
             'Continuous timeseries % does not exist.',
             NEW.timeseries_id;
         END IF;

         IF target_timeseries_type <> 'basic' THEN
           RAISE EXCEPTION
             'Transmission mappings can only target basic timeseries; timeseries % has type %.',
             NEW.timeseries_id,
             target_timeseries_type;
         END IF;

         IF target_location_id <> route_location_id THEN
           RAISE EXCEPTION
             'Transmission mapping timeseries % belongs to location %, but route % belongs to location %.',
             NEW.timeseries_id,
             target_location_id,
             NEW.transmission_route_id,
             route_location_id;
         END IF;

         RETURN NEW;
       END;
       $$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION continuous.check_transmission_timeseries_mapping() OWNER TO admin"
    )

    DBI::dbExecute(
      con,
      "CREATE CONSTRAINT TRIGGER check_transmission_timeseries_mapping
       AFTER INSERT OR UPDATE ON continuous.transmission_timeseries_mappings
       DEFERRABLE INITIALLY DEFERRED
       FOR EACH ROW
       EXECUTE FUNCTION continuous.check_transmission_timeseries_mapping()"
    )

    for (table_name in c(
      "timeseries_source_adapters",
      "transmission_timeseries_mappings",
      "transmission_import_runs"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON continuous.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON continuous.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_name,
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TRIGGER sample_series_source_adapters_user_modified
       BEFORE UPDATE ON discrete.sample_series_source_adapters
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER sample_series_source_adapters_update_modified
       BEFORE UPDATE ON discrete.sample_series_source_adapters
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    for (table_spec in list(
      c(schema = "files", table = "image_series_source_adapters"),
      c(schema = "spatial", table = "raster_series_source_adapters")
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified
           BEFORE UPDATE ON %s.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_spec[["table"]],
          table_spec[["schema"]],
          table_spec[["table"]]
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified
           BEFORE UPDATE ON %s.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_spec[["table"]],
          table_spec[["schema"]],
          table_spec[["table"]]
        )
      )
    }

    DBI::dbExecute(
      con,
      "CREATE TRIGGER source_adapter_capabilities_user_modified
       BEFORE UPDATE ON public.source_adapter_capabilities
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER source_adapter_capabilities_update_modified
       BEFORE UPDATE ON public.source_adapter_capabilities
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_source_adapter_capabilities_trigger
       AFTER UPDATE OR DELETE ON public.source_adapter_capabilities
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_transmission_timeseries_mappings_trigger
       AFTER UPDATE OR DELETE ON continuous.transmission_timeseries_mappings
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_timeseries_source_adapters_trigger
       AFTER UPDATE OR DELETE ON continuous.timeseries_source_adapters
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_sample_series_source_adapters_trigger
       AFTER UPDATE OR DELETE ON discrete.sample_series_source_adapters
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_image_series_source_adapters_trigger
       AFTER UPDATE OR DELETE ON files.image_series_source_adapters
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_raster_series_source_adapters_trigger
       AFTER UPDATE OR DELETE ON spatial.raster_series_source_adapters
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_source_adapters
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_timeseries_visibility
       ON continuous.timeseries_source_adapters
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           WHERE t.timeseries_id =
             timeseries_source_adapters.timeseries_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           WHERE t.timeseries_id =
             timeseries_source_adapters.timeseries_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.sample_series_source_adapters
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_sample_series_visibility
       ON discrete.sample_series_source_adapters
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM discrete.sample_series ss
           WHERE ss.sample_series_id =
             sample_series_source_adapters.sample_series_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM discrete.sample_series ss
           WHERE ss.sample_series_id =
             sample_series_source_adapters.sample_series_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE files.image_series_source_adapters
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_image_series_visibility
       ON files.image_series_source_adapters
       FOR ALL
       USING (
         EXISTS (
           SELECT 1 FROM files.image_series i
           WHERE i.img_series_id = image_series_source_adapters.img_series_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1 FROM files.image_series i
           WHERE i.img_series_id = image_series_source_adapters.img_series_id
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.raster_series_source_adapters
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_raster_series_visibility
       ON spatial.raster_series_source_adapters
       FOR ALL
       USING (
         EXISTS (
           SELECT 1 FROM spatial.raster_series_index r
           WHERE r.raster_series_id =
             raster_series_source_adapters.raster_series_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1 FROM spatial.raster_series_index r
           WHERE r.raster_series_id =
             raster_series_source_adapters.raster_series_id
         )
       )"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE
         public.source_adapter_capabilities,
         continuous.timeseries_source_adapters,
         discrete.sample_series_source_adapters,
         files.image_series_source_adapters,
         spatial.raster_series_source_adapters,
         continuous.transmission_timeseries_mappings,
         continuous.transmission_import_runs
       FROM PUBLIC"
    )

    database_roles <- DBI::dbGetQuery(
      con,
      "SELECT rolname FROM pg_roles"
    )$rolname
    reader_roles <- intersect(
      c("yg_reader_group", "yg_reader"),
      database_roles
    )
    editor_roles <- intersect(
      c("yg_editor_group", "yg_editor"),
      database_roles
    )

    for (role_name in reader_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON TABLE
             public.source_adapter_capabilities,
             continuous.timeseries_source_adapters,
             discrete.sample_series_source_adapters,
             files.image_series_source_adapters,
             spatial.raster_series_source_adapters,
             continuous.transmission_timeseries_mappings,
             continuous.transmission_import_runs
           TO %s",
          quoted_role
        )
      )
    }
    for (role_name in editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON TABLE public.source_adapter_capabilities TO %s",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE
             continuous.timeseries_source_adapters,
             discrete.sample_series_source_adapters,
             files.image_series_source_adapters,
             spatial.raster_series_source_adapters,
             continuous.transmission_timeseries_mappings,
             continuous.transmission_import_runs
           TO %s",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
             continuous.timeseries_source_adapter_id_seq,
             discrete.sample_series_source_adapter_id_seq,
             files.image_series_source_adapter_id_seq,
             spatial.raster_series_source_adapter_id_seq,
             continuous.transmission_timeseries_mappings_transmission_mapping_id_seq,
             continuous.transmission_import_runs_transmission_import_run_id_seq
           TO %s",
          quoted_role
        )
      )
    }

    if ("continuous_editor" %in% database_roles) {
      DBI::dbExecute(
        con,
        "GRANT SELECT ON TABLE public.source_adapter_capabilities
         TO continuous_editor"
      )
      DBI::dbExecute(
        con,
        "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE
           continuous.timeseries_source_adapters,
           continuous.transmission_timeseries_mappings,
           continuous.transmission_import_runs
         TO continuous_editor"
      )
      DBI::dbExecute(
        con,
        "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
           continuous.timeseries_source_adapter_id_seq,
           continuous.transmission_timeseries_mappings_transmission_mapping_id_seq,
           continuous.transmission_import_runs_transmission_import_run_id_seq
         TO continuous_editor"
      )
    }
    if ("discrete_editor" %in% database_roles) {
      DBI::dbExecute(
        con,
        "GRANT SELECT ON TABLE public.source_adapter_capabilities
         TO discrete_editor"
      )
      DBI::dbExecute(
        con,
        "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE
           discrete.sample_series_source_adapters
         TO discrete_editor"
      )
      DBI::dbExecute(
        con,
        "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
           discrete.sample_series_source_adapter_id_seq
         TO discrete_editor"
      )
    }

    # Add record-level approval status to boreholes and wells. Existing table
    # privileges apply automatically to added columns, preserving the PUBLIC
    # and role-based access already established for these base tables.
    for (table_name in c("boreholes", "wells")) {
      qualified_table <- paste0("boreholes.", table_name)
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s
           ADD COLUMN IF NOT EXISTS approval_type_id INTEGER",
          qualified_table
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "UPDATE %s
           SET approval_type_id = $1",
          qualified_table
        ),
        params = list(not_reviewed_id)
      )
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s
           ALTER COLUMN approval_type_id SET NOT NULL,
           ALTER COLUMN approval_type_id SET DEFAULT %d",
          qualified_table,
          not_reviewed_id
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "DO $body$
           BEGIN
             IF NOT EXISTS (
               SELECT 1
               FROM pg_constraint
               WHERE conrelid = '%s'::regclass
                 AND conname = '%s_approval_type_id_fkey'
             ) THEN
               ALTER TABLE %s
               ADD CONSTRAINT %s_approval_type_id_fkey
               FOREIGN KEY (approval_type_id)
               REFERENCES public.approval_types(approval_type_id)
               ON UPDATE CASCADE
               ON DELETE RESTRICT;
             END IF;
           END
           $body$",
          qualified_table,
          table_name,
          qualified_table,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "COMMENT ON COLUMN %s.approval_type_id IS
           'Current review and approval level from public.approval_types. New records default to Not reviewed (N).'",
          qualified_table
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE INDEX IF NOT EXISTS %s_approval_type_id_idx
           ON %s (approval_type_id)",
          table_name,
          qualified_table
        )
      )
    }

    # Wells are independent records beneath a borehole and therefore need
    # their own names when more than one well occupies the same borehole.
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.wells
       ADD COLUMN IF NOT EXISTS well_name TEXT"
    )
    DBI::dbExecute(
      con,
      "WITH numbered_wells AS (
         SELECT w.well_id,
                COALESCE(
                  NULLIF(BTRIM(b.borehole_name), ''),
                  'Borehole ' || b.borehole_id
                ) AS base_name,
                ROW_NUMBER() OVER (
                  PARTITION BY w.borehole_id
                  ORDER BY w.well_id
                ) AS well_number,
                COUNT(*) OVER (PARTITION BY w.borehole_id) AS well_count
         FROM boreholes.wells w
         INNER JOIN boreholes.boreholes b USING (borehole_id)
       )
       UPDATE boreholes.wells w
       SET well_name = CASE
         WHEN numbered_wells.well_count = 1 THEN numbered_wells.base_name
         ELSE numbered_wells.base_name || ' ' || numbered_wells.well_number
       END
       FROM numbered_wells
       WHERE w.well_id = numbered_wells.well_id
         AND NULLIF(BTRIM(w.well_name), '') IS NULL"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.wells
       ALTER COLUMN well_name SET NOT NULL"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.well_name IS
       'Name of this well. It may match its borehole for a one-to-one relationship but must identify the individual well when a borehole contains multiple wells.'"
    )
    DBI::dbExecute(
      con,
      "DO $body$
       BEGIN
         IF NOT EXISTS (
           SELECT 1
           FROM pg_constraint
           WHERE conrelid = 'boreholes.wells'::regclass
             AND conname = 'wells_borehole_well_name_key'
         ) THEN
           ALTER TABLE boreholes.wells
           ADD CONSTRAINT wells_borehole_well_name_key
           UNIQUE (borehole_id, well_name);
         END IF;
       END
       $body$"
    )

    # Add a controlled catalogue for borehole drilling methods.
    DBI::dbExecute(
      con,
      "CREATE TABLE boreholes.drill_methods (
         drill_method_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         method_name TEXT NOT NULL UNIQUE,
         method_name_fr TEXT NOT NULL UNIQUE,
         description TEXT,
         description_fr TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       )"
    )
    DBI::dbExecute(con, "ALTER TABLE boreholes.drill_methods OWNER TO admin")
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE boreholes.drill_methods IS
       'Catalogue of drilling methods used to create boreholes. This table is referenced by boreholes.boreholes.drill_method.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.drill_methods.method_name IS
       'Canonical English drilling-method name.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.drill_methods.method_name_fr IS
       'Canonical French drilling-method name.'"
    )
    new_values_en <- c(
      "Excavation",
      "Jetting",
      "Driven point",
      "Direct push",
      "Rotary - air",
      "Rotary - mud",
      "Rotary - water",
      "Reverse circulation - air",
      "Sonic",
      "Auger",
      "Cable tool",
      "Diamond core",
      "Unknown"
    )
    new_methods <- data.frame(
      method_name = new_values_en,
      method_name_fr = c(
        "Excavation",
        "Forage par jet d'eau",
        "Enfoncement d'une pointe filtrante",
        "Enfoncement direct",
        "Forage rotatif à l'air",
        "Forage rotatif à la boue",
        "Forage rotatif à l'eau",
        "Forage à circulation inverse à l'air",
        "Forage sonique",
        "Forage à la tarière",
        "Forage au câble",
        "Carottage au diamant",
        "Méthode inconnue"
      ),
      description = c(
        "Manual or mechanical removal of material to construct a shallow, typically large-diameter well or borehole.",
        "Use of a pressurized water jet to loosen unconsolidated material, with returning water carrying cuttings to the surface.",
        "Installation of a screened well point and attached casing by driving them directly into unconsolidated sediment.",
        "Advancement of a tool string into unconsolidated material using static force or percussion, generally without rotary excavation.",
        "Rotary drilling in which compressed air carries cuttings to the surface through the annular space around the drill string.",
        "Rotary drilling in which circulating drilling mud carries cuttings to the surface and supports the borehole walls.",
        "Rotary drilling in which circulating water carries cuttings to the surface, without the deliberate use of drilling mud.",
        "Rotary drilling in which compressed air carries cuttings to the surface through an inner tube or dual-wall drill string.",
        "Drilling using high-frequency vibration, commonly combined with rotation, to advance casing or a core barrel through the formation.",
        "Drilling with a rotating helical auger that cuts the formation and conveys material to the surface.",
        "Percussion drilling in which a heavy bit is repeatedly raised and dropped on a cable, with loosened material periodically removed from the borehole.",
        "Rotary coring using a diamond-set bit to cut and recover a cylindrical sample of bedrock.",
        "The drilling method was not recorded or cannot be determined from the available information."
      ),
      description_fr = c(
        "Enlèvement manuel ou mécanique des matériaux pour construire un puits ou un forage peu profond et généralement de grand diamètre.",
        "Utilisation d'un jet d'eau sous pression pour désagréger les matériaux meubles, l'eau de retour remontant les déblais à la surface.",
        "Installation d'une pointe filtrante et de son tubage par enfoncement direct dans des sédiments non consolidés.",
        "Enfoncement d'un train de tiges dans des matériaux non consolidés par poussée statique ou percussion, généralement sans excavation rotative.",
        "Forage rotatif dans lequel de l'air comprimé remonte les déblais à la surface par l'espace annulaire entourant le train de tiges.",
        "Forage rotatif dans lequel une boue de forage en circulation remonte les déblais à la surface et soutient les parois du forage.",
        "Forage rotatif dans lequel de l'eau en circulation remonte les déblais à la surface, sans utilisation délibérée de boue de forage.",
        "Forage rotatif dans lequel de l'air comprimé remonte les déblais à la surface par un tube intérieur ou un train de tiges à double paroi.",
        "Forage utilisant des vibrations à haute fréquence, généralement combinées à une rotation, pour faire avancer un tubage ou un carottier dans la formation.",
        "Forage à l'aide d'une tarière hélicoïdale rotative qui coupe la formation et remonte les matériaux à la surface.",
        "Forage par percussion dans lequel un trépan lourd est soulevé et relâché à répétition au moyen d'un câble, les matériaux désagrégés étant retirés périodiquement du forage.",
        "Carottage rotatif utilisant un trépan diamanté pour découper et récupérer un échantillon cylindrique de roche.",
        "La méthode de forage n'a pas été consignée ou ne peut pas être déterminée à partir des renseignements disponibles."
      )
    )
    # Insert or update the new catalogue values, but do not delete any existing values.
    for (i in seq_len(nrow(new_methods))) {
      DBI::dbExecute(
        con,
        "INSERT INTO boreholes.drill_methods (method_name, method_name_fr, description, description_fr)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (method_name) DO UPDATE
         SET method_name_fr = EXCLUDED.method_name_fr,
             description = EXCLUDED.description,
             description_fr = EXCLUDED.description_fr",
        params = list(
          new_methods$method_name[i],
          new_methods$method_name_fr[i],
          new_methods$description[i],
          new_methods$description_fr[i]
        )
      )
    }

    existing_drill_methods <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT b.drill_method
       FROM boreholes.boreholes b
       LEFT JOIN boreholes.drill_methods dm
         ON dm.method_name = b.drill_method
       WHERE b.drill_method IS NOT NULL
         AND dm.drill_method_id IS NULL
       ORDER BY b.drill_method"
    )
    if (nrow(existing_drill_methods) > 0L) {
      stop(
        "Cannot convert boreholes.boreholes.drill_method because these existing values are not in the new catalogue: ",
        paste(existing_drill_methods$drill_method, collapse = ", "),
        ". Update Patch 56's catalogue or clean those values before applying the patch."
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE boreholes.boreholes b
       SET drill_method = dm.drill_method_id::text
       FROM boreholes.drill_methods dm
       WHERE b.drill_method = dm.method_name"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes
       ALTER COLUMN drill_method TYPE INTEGER USING drill_method::INTEGER,
       ADD CONSTRAINT boreholes_drill_method_fkey
         FOREIGN KEY (drill_method)
         REFERENCES boreholes.drill_methods(drill_method_id)
         ON UPDATE CASCADE
         ON DELETE SET NULL"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.boreholes.drill_method IS
       'Drilling method from boreholes.drill_methods.'"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX boreholes_drill_method_idx
       ON boreholes.boreholes (drill_method)"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER drill_methods_user_modified_trigger
       BEFORE UPDATE ON boreholes.drill_methods
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER drill_methods_update_modified_trigger
       BEFORE UPDATE ON boreholes.drill_methods
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_drill_methods_trigger
       AFTER UPDATE OR DELETE ON boreholes.drill_methods
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )

    # Borehole reference data is readable throughout the boreholes schema.
    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE boreholes.drill_methods TO PUBLIC"
    )
    for (role_name in editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT, INSERT, UPDATE, DELETE
           ON TABLE boreholes.drill_methods TO %s",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT USAGE, SELECT, UPDATE
           ON SEQUENCE boreholes.drill_methods_drill_method_id_seq TO %s",
          quoted_role
        )
      )
    }

    # Add governed catalogues and construction fields for well seals and screens.
    DBI::dbExecute(
      con,
      "CREATE TABLE boreholes.seal_materials (
         seal_material_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         material_name TEXT NOT NULL UNIQUE,
         material_name_fr TEXT NOT NULL UNIQUE,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE boreholes.screen_materials (
         screen_material_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         material_name TEXT NOT NULL UNIQUE,
         material_name_fr TEXT NOT NULL UNIQUE,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       )"
    )
    DBI::dbExecute(
      con,
      "CREATE TABLE boreholes.screen_types (
         screen_type_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
         type_name TEXT NOT NULL UNIQUE,
         type_name_fr TEXT NOT NULL UNIQUE,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ
       )"
    )

    for (table_name in c(
      "seal_materials",
      "screen_materials",
      "screen_types"
    )) {
      DBI::dbExecute(
        con,
        sprintf("ALTER TABLE boreholes.%s OWNER TO admin", table_name)
      )
    }
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE boreholes.seal_materials IS
       'Controlled catalogue of materials used to construct annular seals in wells.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE boreholes.screen_materials IS
       'Controlled catalogue of materials used for well screens and intake assemblies.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE boreholes.screen_types IS
       'Controlled catalogue of well-screen and open-intake construction types.'"
    )

    seal_materials <- data.frame(
      material_name = c(
        "Bentonite",
        "Bentonite chips or pellets",
        "Bentonite grout",
        "Cement grout",
        "Cement-bentonite grout",
        "Concrete",
        "Drill cuttings",
        "Native material",
        "Sand",
        "Gravel",
        "Other",
        "Unknown"
      ),
      material_name_fr = c(
        "Bentonite",
        "Copeaux ou granules de bentonite",
        "Coulis de bentonite",
        "Coulis de ciment",
        "Coulis ciment-bentonite",
        "Béton",
        "Déblais de forage",
        "Matériau naturel sur place",
        "Sable",
        "Gravier",
        "Autre",
        "Inconnu"
      )
    )
    DBI::dbAppendTable(
      con,
      DBI::Id(schema = "boreholes", table = "seal_materials"),
      seal_materials
    )

    screen_materials <- data.frame(
      material_name = c(
        "Stainless steel",
        "Steel",
        "Galvanized steel",
        "PVC",
        "ABS",
        "Plastic (unspecified)",
        "Fibreglass",
        "Brass or bronze",
        "Other",
        "Unknown"
      ),
      material_name_fr = c(
        "Acier inoxydable",
        "Acier",
        "Acier galvanisé",
        "PVC",
        "ABS",
        "Plastique (non précisé)",
        "Fibre de verre",
        "Laiton ou bronze",
        "Autre",
        "Inconnu"
      )
    )
    DBI::dbAppendTable(
      con,
      DBI::Id(schema = "boreholes", table = "screen_materials"),
      screen_materials
    )

    screen_types <- data.frame(
      type_name = c(
        "Continuous wire-wrap",
        "Louvered",
        "Bridge-slot",
        "Slotted",
        "Perforated",
        "Porous",
        "Well point",
        "Open hole",
        "Open bottom",
        "Other",
        "Unknown"
      ),
      type_name_fr = c(
        "À fil enroulé continu",
        "À persiennes",
        "À fentes en pont",
        "À fentes",
        "Perforé",
        "Poreux",
        "Pointe filtrante",
        "Trou ouvert",
        "Fond ouvert",
        "Autre",
        "Inconnu"
      )
    )
    DBI::dbAppendTable(
      con,
      DBI::Id(schema = "boreholes", table = "screen_types"),
      screen_types
    )

    # Correct legacy intervals whose endpoints were stored in reverse order
    # before enforcing the construction-depth invariants.
    DBI::dbExecute(
      con,
      "UPDATE boreholes.wells
       SET screen_top_depth_m = screen_bottom_depth_m,
           screen_bottom_depth_m = screen_top_depth_m
       WHERE screen_top_depth_m IS NOT NULL
         AND screen_bottom_depth_m IS NOT NULL
         AND screen_bottom_depth_m < screen_top_depth_m"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.wells
       ADD COLUMN seal_material_id INTEGER
         REFERENCES boreholes.seal_materials(seal_material_id)
         ON UPDATE CASCADE ON DELETE SET NULL,
       ADD COLUMN seal_diameter_mm NUMERIC,
       ADD COLUMN seal_depth_from_m NUMERIC,
       ADD COLUMN seal_depth_to_m NUMERIC,
       ADD COLUMN screen_material_id INTEGER
         REFERENCES boreholes.screen_materials(screen_material_id)
         ON UPDATE CASCADE ON DELETE SET NULL,
       ADD COLUMN screen_type_id INTEGER
         REFERENCES boreholes.screen_types(screen_type_id)
         ON UPDATE CASCADE ON DELETE SET NULL,
       ADD CONSTRAINT wells_seal_diameter_positive
         CHECK (seal_diameter_mm IS NULL OR seal_diameter_mm > 0),
       ADD CONSTRAINT wells_seal_depth_from_nonnegative
         CHECK (seal_depth_from_m IS NULL OR seal_depth_from_m >= 0),
       ADD CONSTRAINT wells_seal_depth_to_nonnegative
         CHECK (seal_depth_to_m IS NULL OR seal_depth_to_m >= 0),
       ADD CONSTRAINT wells_seal_interval_valid
         CHECK (
           seal_depth_from_m IS NULL
           OR seal_depth_to_m IS NULL
           OR seal_depth_to_m >= seal_depth_from_m
         ),
       ADD CONSTRAINT wells_screen_top_depth_nonnegative
         CHECK (screen_top_depth_m IS NULL OR screen_top_depth_m >= 0),
       ADD CONSTRAINT wells_screen_bottom_depth_nonnegative
         CHECK (screen_bottom_depth_m IS NULL OR screen_bottom_depth_m >= 0),
       ADD CONSTRAINT wells_screen_interval_valid
         CHECK (
           screen_top_depth_m IS NULL
           OR screen_bottom_depth_m IS NULL
           OR screen_bottom_depth_m >= screen_top_depth_m
         )"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.seal_material_id IS
       'Material used for the annular seal, from boreholes.seal_materials.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.seal_diameter_mm IS
       'Outside diameter of the annular seal in millimetres.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.seal_depth_from_m IS
       'Depth to the top of the annular seal in metres below ground surface.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.seal_depth_to_m IS
       'Depth to the bottom of the annular seal in metres below ground surface.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.screen_material_id IS
       'Well-screen material, from boreholes.screen_materials.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.wells.screen_type_id IS
       'Well-screen or open-intake construction type, from boreholes.screen_types.'"
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX wells_seal_material_idx
       ON boreholes.wells (seal_material_id)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX wells_screen_material_idx
       ON boreholes.wells (screen_material_id)"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX wells_screen_type_idx
       ON boreholes.wells (screen_type_id)"
    )

    for (table_name in c(
      "seal_materials",
      "screen_materials",
      "screen_types"
    )) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_user_modified_trigger
           BEFORE UPDATE ON boreholes.%s
           FOR EACH ROW EXECUTE FUNCTION public.user_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER %s_update_modified_trigger
           BEFORE UPDATE ON boreholes.%s
           FOR EACH ROW EXECUTE FUNCTION public.update_modified()",
          table_name,
          table_name
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE TRIGGER audit_%s_trigger
           AFTER UPDATE OR DELETE ON boreholes.%s
           FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()",
          table_name,
          table_name
        )
      )
    }

    DBI::dbExecute(
      con,
      "GRANT SELECT ON TABLE
         boreholes.seal_materials,
         boreholes.screen_materials,
         boreholes.screen_types
       TO PUBLIC"
    )
    for (role_name in editor_roles) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE
             boreholes.seal_materials,
             boreholes.screen_materials,
             boreholes.screen_types
           TO %s",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
             boreholes.seal_materials_seal_material_id_seq,
             boreholes.screen_materials_screen_material_id_seq,
             boreholes.screen_types_screen_type_id_seq
           TO %s",
          quoted_role
        )
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '56'
       WHERE item = 'Last patch number'"
    )
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = $1
       WHERE item = 'AquaCache R package used for last patch'",
      params = list(as.character(packageVersion("AquaCache")))
    )

    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "Patch 56 applied successfully. Source adapters are now registered by data domain, transmission imports retain durable mappings and history, continuous-timeseries datetime bounds retain precise source timestamps, boreholes and wells have record-level approval status, and wells have individual names plus governed seal and screen construction details."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
