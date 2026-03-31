# Patch 39

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 39. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
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
    # Basic sanity checks ###################################################
    check <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "to_regclass('public.parameters') IS NOT NULL AS has_parameters,",
        "to_regclass('public.units') IS NOT NULL AS has_units,",
        "to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,",
        "to_regclass('continuous.measurements_calculated_daily') IS NOT NULL AS has_measurements_calculated_daily,",
        "to_regclass('continuous.owners') IS NOT NULL AS has_owners,",
        "to_regclass('discrete.samples') IS NOT NULL AS has_samples,",
        "to_regclass('discrete.results') IS NOT NULL AS has_results,",
        "to_regclass('discrete.guidelines') IS NOT NULL AS has_guidelines,",
        "to_regclass('discrete.guidelines_media_types') IS NOT NULL AS has_guidelines_media_types,",
        "to_regclass('public.locations') IS NOT NULL AS has_locations,",
        "to_regclass('public.location_types') IS NOT NULL AS has_location_types,",
        "to_regclass('public.sub_locations') IS NOT NULL AS has_sub_locations,",
        "to_regclass('public.organizations') IS NOT NULL AS has_organizations,",
        "to_regclass('public.media_types') IS NOT NULL AS has_media_types,",
        "to_regclass('public.locations_z') IS NOT NULL AS has_locations_z,",
        "to_regclass('public.locations_projects') IS NOT NULL AS has_locations_projects,",
        "to_regclass('public.projects') IS NOT NULL AS has_projects,",
        "to_regclass('public.locations_networks') IS NOT NULL AS has_locations_networks,",
        "to_regclass('public.networks') IS NOT NULL AS has_networks,",
        "to_regclass('public.locations_metadata_instruments') IS NOT NULL AS has_lmi,",
        "to_regclass('public.datum_conversions') IS NOT NULL AS has_datum_conversions,",
        "to_regclass('public.datum_list') IS NOT NULL AS has_datum_list,",
        "to_regclass('spatial.vectors') IS NOT NULL AS has_vectors,",
        "to_regclass('spatial.raster_series_index') IS NOT NULL AS has_raster_series_index,",
        "to_regclass('spatial.rasters_reference') IS NOT NULL AS has_rasters_reference,",
        "to_regclass('continuous.aggregation_types') IS NOT NULL AS has_aggregation_types"
      )
    )

    if (
      !isTRUE(check$has_parameters[[1]]) ||
        !isTRUE(check$has_units[[1]]) ||
        !isTRUE(check$has_timeseries[[1]]) ||
        !isTRUE(check$has_measurements_calculated_daily[[1]]) ||
        !isTRUE(check$has_owners[[1]]) ||
        !isTRUE(check$has_samples[[1]]) ||
        !isTRUE(check$has_results[[1]]) ||
        !isTRUE(check$has_guidelines[[1]]) ||
        !isTRUE(check$has_guidelines_media_types[[1]]) ||
        !isTRUE(check$has_locations[[1]]) ||
        !isTRUE(check$has_location_types[[1]]) ||
        !isTRUE(check$has_sub_locations[[1]]) ||
        !isTRUE(check$has_organizations[[1]]) ||
        !isTRUE(check$has_media_types[[1]]) ||
        !isTRUE(check$has_locations_z[[1]]) ||
        !isTRUE(check$has_locations_projects[[1]]) ||
        !isTRUE(check$has_projects[[1]]) ||
        !isTRUE(check$has_locations_networks[[1]]) ||
        !isTRUE(check$has_networks[[1]]) ||
        !isTRUE(check$has_lmi[[1]]) ||
        !isTRUE(check$has_datum_conversions[[1]]) ||
        !isTRUE(check$has_datum_list[[1]]) ||
        !isTRUE(check$has_vectors[[1]]) ||
        !isTRUE(check$has_raster_series_index[[1]]) ||
        !isTRUE(check$has_rasters_reference[[1]]) ||
        !isTRUE(check$has_aggregation_types[[1]])
    ) {
      stop(
        paste(
          "Patch 39 requires public.parameters, public.units,",
          "continuous.timeseries, continuous.measurements_calculated_daily,",
          "continuous.owners, discrete.samples, discrete.results,",
          "discrete.guidelines, discrete.guidelines_media_types,",
          "public.locations, public.location_types, public.sub_locations,",
          "public.organizations, public.media_types,",
          "public.locations_z, public.locations_projects, public.projects,",
          "public.locations_networks, public.networks,",
          "public.locations_metadata_instruments, public.datum_conversions,",
          "public.datum_list, spatial.vectors,",
          "spatial.raster_series_index, spatial.rasters_reference, and",
          "continuous.aggregation_types to already exist."
        )
      )
    }

    legacy_cols <- DBI::dbGetQuery(
      con,
      "SELECT column_name
       FROM information_schema.columns
       WHERE table_schema = 'public'
         AND table_name = 'parameters'
         AND column_name IN ('units_liquid_text', 'units_solid_text', 'units_gas_text')
       ORDER BY column_name;"
    )
    if (nrow(legacy_cols) > 0) {
      stop(
        paste(
          "Patch 39 found legacy unit text columns already present in",
          "public.parameters (",
          paste(legacy_cols$column_name, collapse = ", "),
          "). Review the existing schema state before applying this patch."
        )
      )
    }

    unit_col_info <- DBI::dbGetQuery(
      con,
      "SELECT column_name, data_type
       FROM information_schema.columns
       WHERE table_schema = 'public'
         AND table_name = 'parameters'
         AND column_name IN ('unit_default', 'unit_solid', 'units_liquid', 'units_solid', 'units_gas')
       ORDER BY column_name;"
    )

    has_unit_default <- "unit_default" %in% unit_col_info$column_name
    has_unit_solid_legacy <- "unit_solid" %in% unit_col_info$column_name
    has_units_liquid <- "units_liquid" %in% unit_col_info$column_name
    has_units_solid <- "units_solid" %in% unit_col_info$column_name
    has_units_gas <- "units_gas" %in% unit_col_info$column_name

    if (
      (has_unit_default && has_units_liquid) ||
        (has_unit_solid_legacy && has_units_solid)
    ) {
      stop(
        paste(
          "Patch 39 found both legacy and target unit columns in",
          "public.parameters. Review the existing schema state before",
          "applying this patch."
        )
      )
    }

    if (!has_unit_default && !has_units_liquid) {
      stop(
        "Patch 39 requires either public.parameters.unit_default or public.parameters.units_liquid to exist."
      )
    }
    if (!has_unit_solid_legacy && !has_units_solid) {
      stop(
        "Patch 39 requires either public.parameters.unit_solid or public.parameters.units_solid to exist."
      )
    }

    liquid_col <- if (has_units_liquid) "units_liquid" else "unit_default"
    solid_col <- if (has_units_solid) "units_solid" else "unit_solid"

    unit_col_types <- setNames(
      unit_col_info$data_type,
      unit_col_info$column_name
    )[
      c(liquid_col, solid_col, if (has_units_gas) "units_gas" else NULL)
    ]
    text_types <- c("text", "character varying", "character")
    integer_types <- c("smallint", "integer", "bigint")
    unit_cols_are_text <- all(unit_col_types %in% text_types)
    unit_cols_are_integer <- all(unit_col_types %in% integer_types)

    if (!unit_cols_are_text && !unit_cols_are_integer) {
      stop(
        paste(
          "Patch 39 expected the parameter unit columns to all be",
          "text-like columns or all be integer foreign keys. Found:",
          paste(
            paste(names(unit_col_types), unit_col_types, sep = "="),
            collapse = ", "
          )
        )
      )
    }

    if (unit_cols_are_text) {
      # Insert any parameter units that were added after patch 36 so the
      # column migration can map every non-empty text value to public.units.
      existing_parameter_units <- c(
        DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT DISTINCT NULLIF(BTRIM(%s), '') AS unit_name
             FROM public.parameters;",
            liquid_col
          )
        )$unit_name,
        DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT DISTINCT NULLIF(BTRIM(%s), '') AS unit_name
             FROM public.parameters;",
            solid_col
          )
        )$unit_name
      )
      if (has_units_gas) {
        existing_parameter_units <- c(
          existing_parameter_units,
          DBI::dbGetQuery(
            con,
            "SELECT DISTINCT NULLIF(BTRIM(units_gas), '') AS unit_name
             FROM public.parameters;"
          )$unit_name
        )
      }
      all_units <- sort(unique(existing_parameter_units))
      all_units <- all_units[!is.na(all_units)]

      if (length(all_units) > 0) {
        for (unit in all_units) {
          DBI::dbExecute(
            con,
            "INSERT INTO public.units (unit_name)
             VALUES ($1)
             ON CONFLICT (unit_name) DO NOTHING;",
            params = list(unit)
          )
        }
      }

      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE public.parameters
           RENAME COLUMN %s TO units_liquid_text;",
          liquid_col
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE public.parameters
           RENAME COLUMN %s TO units_solid_text;",
          solid_col
        )
      )
      if (has_units_gas) {
        DBI::dbExecute(
          con,
          "ALTER TABLE public.parameters
           RENAME COLUMN units_gas TO units_gas_text;"
        )
      }
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         ADD COLUMN units_liquid INTEGER;"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         ADD COLUMN units_solid INTEGER;"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         ADD COLUMN units_gas INTEGER;"
      )

      DBI::dbExecute(
        con,
        "UPDATE public.parameters p
         SET units_liquid = u.unit_id
         FROM public.units u
         WHERE p.units_liquid IS NULL
           AND u.unit_name = NULLIF(BTRIM(p.units_liquid_text), '');"
      )
      DBI::dbExecute(
        con,
        "UPDATE public.parameters p
         SET units_solid = u.unit_id
         FROM public.units u
         WHERE p.units_solid IS NULL
           AND u.unit_name = NULLIF(BTRIM(p.units_solid_text), '');"
      )
      if (has_units_gas) {
        DBI::dbExecute(
          con,
          "UPDATE public.parameters p
           SET units_gas = u.unit_id
           FROM public.units u
           WHERE p.units_gas IS NULL
             AND u.unit_name = NULLIF(BTRIM(p.units_gas_text), '');"
        )
      }

      missing_default_units <- DBI::dbGetQuery(
        con,
        "SELECT parameter_id, param_name, units_liquid_text
         FROM public.parameters
         WHERE NULLIF(BTRIM(units_liquid_text), '') IS NOT NULL
           AND units_liquid IS NULL
         ORDER BY parameter_id;"
      )
      if (nrow(missing_default_units) > 0) {
        stop(
          paste(
            "Patch 39 could not map one or more parameter units_liquid",
            "values into public.units. Example parameters:",
            paste(head(missing_default_units$param_name, 5), collapse = ", ")
          )
        )
      }

      missing_solid_units <- DBI::dbGetQuery(
        con,
        "SELECT parameter_id, param_name, units_solid_text
         FROM public.parameters
         WHERE NULLIF(BTRIM(units_solid_text), '') IS NOT NULL
           AND units_solid IS NULL
         ORDER BY parameter_id;"
      )
      if (nrow(missing_solid_units) > 0) {
        stop(
          paste(
            "Patch 39 could not map one or more parameter units_solid",
            "values into public.units. Example parameters:",
            paste(head(missing_solid_units$param_name, 5), collapse = ", ")
          )
        )
      }

      if (has_units_gas) {
        missing_gas_units <- DBI::dbGetQuery(
          con,
          "SELECT parameter_id, param_name, units_gas_text
           FROM public.parameters
           WHERE NULLIF(BTRIM(units_gas_text), '') IS NOT NULL
             AND units_gas IS NULL
           ORDER BY parameter_id;"
        )
        if (nrow(missing_gas_units) > 0) {
          stop(
            paste(
              "Patch 39 could not map one or more parameter units_gas",
              "values into public.units. Example parameters:",
              paste(head(missing_gas_units$param_name, 5), collapse = ", ")
            )
          )
        }
      }
    } else {
      message(
        paste(
          "Patch 39 detected integer unit columns in public.parameters;",
          "skipping the text-to-foreign-key migration step and",
          "re-applying dependent schema objects."
        )
      )

      if (has_unit_default) {
        DBI::dbExecute(
          con,
          "ALTER TABLE public.parameters
           RENAME COLUMN unit_default TO units_liquid;"
        )
      }
      if (has_unit_solid_legacy) {
        DBI::dbExecute(
          con,
          "ALTER TABLE public.parameters
           RENAME COLUMN unit_solid TO units_solid;"
        )
      }
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         ADD COLUMN IF NOT EXISTS units_gas INTEGER;"
      )
    }

    # Re-apply constraints, indexes, and comments on the normalized columns.
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_unit_default_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_unit_solid_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_units_liquid_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_units_solid_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_units_gas_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       ADD CONSTRAINT parameters_units_liquid_fkey
       FOREIGN KEY (units_liquid)
       REFERENCES public.units(unit_id)
       ON DELETE SET NULL
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       ADD CONSTRAINT parameters_units_solid_fkey
       FOREIGN KEY (units_solid)
       REFERENCES public.units(unit_id)
       ON DELETE SET NULL
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       ADD CONSTRAINT parameters_units_gas_fkey
       FOREIGN KEY (units_gas)
       REFERENCES public.units(unit_id)
       ON DELETE SET NULL
       ON UPDATE CASCADE;"
    )

    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS public.parameters_unit_default_idx;"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS public.parameters_unit_solid_idx;"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS public.parameters_units_liquid_idx;"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS public.parameters_units_solid_idx;"
    )
    DBI::dbExecute(
      con,
      "DROP INDEX IF EXISTS public.parameters_units_gas_idx;"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX parameters_units_liquid_idx
       ON public.parameters (units_liquid);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX parameters_units_solid_idx
       ON public.parameters (units_solid);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX parameters_units_gas_idx
       ON public.parameters (units_gas);"
    )

    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.parameters.units_liquid IS
       'Foreign key to public.units for the units used when this parameter is measured in a liquid matrix.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.parameters.units_solid IS
       'Foreign key to public.units for the units used when this parameter is measured in a solid matrix.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.parameters.units_gas IS
       'Foreign key to public.units for the units used when this parameter is measured in a gas matrix.';"
    )

    # Matrix states, media defaults, and explicit parameter state usage #####
    DBI::dbExecute(
      con,
      "CREATE TABLE IF NOT EXISTS public.matrix_states (
         matrix_state_id INTEGER PRIMARY KEY GENERATED BY DEFAULT AS IDENTITY,
         matrix_state_code TEXT NOT NULL UNIQUE,
         matrix_state_name TEXT NOT NULL UNIQUE,
         matrix_state_name_fr TEXT NOT NULL UNIQUE,
         description TEXT
       );"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.matrix_states OWNER TO admin;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.matrix_states IS
       'Lookup of physical matrix states used to resolve parameter units consistently across continuous, discrete, and raster data.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.matrix_states.matrix_state_code IS
       'Stable code for the matrix state, e.g. liquid, solid, or gas.';"
    )
    DBI::dbExecute(
      con,
      "INSERT INTO public.matrix_states
         (matrix_state_code, matrix_state_name, matrix_state_name_fr, description)
       VALUES
         ('liquid', 'Liquid', 'Liquide',
          'Measurement is in a liquid matrix or solution.'),
         ('solid', 'Solid', 'Solide',
          'Measurement is in a solid matrix or particulate material.'),
         ('gas', 'Gas', 'Gaz',
          'Measurement is in a gaseous matrix.')
       ON CONFLICT (matrix_state_code) DO UPDATE
       SET matrix_state_name = EXCLUDED.matrix_state_name,
           matrix_state_name_fr = EXCLUDED.matrix_state_name_fr,
           description = EXCLUDED.description;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE public.media_types
       ADD COLUMN IF NOT EXISTS default_matrix_state_id INTEGER
       REFERENCES public.matrix_states(matrix_state_id)
       ON DELETE RESTRICT
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.media_types.default_matrix_state_id IS
       'Default physical matrix state associated with this media type, used to resolve parameter units when no explicit override is provided.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS media_types_default_matrix_state_idx
       ON public.media_types (default_matrix_state_id);"
    )
    DBI::dbExecute(
      con,
      "WITH media_guess AS (
         SELECT
           mt.media_id,
           LOWER(CONCAT_WS(' ', mt.media_type, mt.media_type_fr)) AS media_label,
           CASE
             WHEN LOWER(CONCAT_WS(' ', mt.media_type, mt.media_type_fr))
               ~ '(air|gas|gazeux|atmos|vapo|smoke|emission)' THEN 'gas'
             WHEN LOWER(CONCAT_WS(' ', mt.media_type, mt.media_type_fr))
               ~ '(water|eau|liquid|effluent|groundwater|ground water|surface water|stormwater|storm water|wastewater|waste water|drinking water|solution|brine|porewater|pore water|leachate|runoff|seep)' THEN 'liquid'
             WHEN LOWER(CONCAT_WS(' ', mt.media_type, mt.media_type_fr))
               ~ '(snow|neige|ice|glace|soil|sediment|sédiment|solid|solide|sludge|rock|ash|dust|particulate|precipitate|biota|vegetation|moss|lichen|tissue|core|peat|tissue)' THEN 'solid'
             ELSE NULL
           END AS matrix_state_code
         FROM public.media_types mt
       )
       UPDATE public.media_types mt
       SET default_matrix_state_id = ms.matrix_state_id
       FROM media_guess mg
       JOIN public.matrix_states ms
         ON ms.matrix_state_code = mg.matrix_state_code
       WHERE mt.media_id = mg.media_id
         AND mt.default_matrix_state_id IS NULL
         AND mg.matrix_state_code IS NOT NULL;"
    )

    # Fix one issue with 'sediment, surface water', which should actually be 'solid'
    DBI::dbExecute(
      con,
      "UPDATE public.media_types
       SET default_matrix_state_id = (
         SELECT matrix_state_id
         FROM public.matrix_states
         WHERE matrix_state_code = 'solid'
       )
       WHERE media_type = 'sediment, surface water';"
    )

    unresolved_media_states <- DBI::dbGetQuery(
      con,
      "SELECT media_id, media_type
       FROM public.media_types
       WHERE default_matrix_state_id IS NULL
       ORDER BY media_id;"
    )
    if (nrow(unresolved_media_states) > 0) {
      stop(
        paste(
          "Patch 39 could not assign default_matrix_state_id for one or more",
          "media types. Example media types:",
          paste(head(unresolved_media_states$media_type, 5), collapse = ", ")
        )
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE public.media_types
       ALTER COLUMN default_matrix_state_id SET NOT NULL;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.default_matrix_state_id_from_media(
         p_media_id INTEGER
       )
       RETURNS INTEGER
       LANGUAGE sql
       STABLE
       AS $function$
         SELECT mt.default_matrix_state_id
         FROM public.media_types mt
         WHERE mt.media_id = p_media_id;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.default_matrix_state_id_from_media(INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.get_parameter_unit_id(
         p_parameter_id INTEGER,
         p_matrix_state_id INTEGER
       )
       RETURNS INTEGER
       LANGUAGE sql
       STABLE
       AS $function$
         SELECT CASE ms.matrix_state_code
           WHEN 'liquid' THEN p.units_liquid
           WHEN 'solid' THEN p.units_solid
           WHEN 'gas' THEN p.units_gas
           ELSE NULL
         END
         FROM public.parameters p
         JOIN public.matrix_states ms
           ON ms.matrix_state_id = p_matrix_state_id
         WHERE p.parameter_id = p_parameter_id;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.get_parameter_unit_id(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.get_parameter_unit_name(
         p_parameter_id INTEGER,
         p_matrix_state_id INTEGER
       )
       RETURNS TEXT
       LANGUAGE sql
       STABLE
       AS $function$
         SELECT u.unit_name
         FROM public.units u
         WHERE u.unit_id = public.get_parameter_unit_id(
           p_parameter_id,
           p_matrix_state_id
         );
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.get_parameter_unit_name(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.parameter_has_unit_for_matrix_state(
         p_parameter_id INTEGER,
         p_matrix_state_id INTEGER
       )
       RETURNS BOOLEAN
       LANGUAGE sql
       STABLE
       AS $function$
         SELECT public.get_parameter_unit_id(
           p_parameter_id,
           p_matrix_state_id
         ) IS NOT NULL;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.parameter_has_unit_for_matrix_state(INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.get_unique_parameter_matrix_state_id(
         p_parameter_id INTEGER
       )
       RETURNS INTEGER
       LANGUAGE sql
       STABLE
       AS $function$
         WITH available_states AS (
           SELECT ms.matrix_state_id
           FROM public.parameters p
           CROSS JOIN LATERAL (
             VALUES
               ('liquid', p.units_liquid),
               ('solid', p.units_solid),
               ('gas', p.units_gas)
           ) available(matrix_state_code, unit_id)
           JOIN public.matrix_states ms
             ON ms.matrix_state_code = available.matrix_state_code
           WHERE p.parameter_id = p_parameter_id
             AND available.unit_id IS NOT NULL
         )
         SELECT CASE
           WHEN COUNT(*) = 1 THEN MIN(matrix_state_id)
           ELSE NULL
         END
         FROM available_states;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.get_unique_parameter_matrix_state_id(INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.resolve_matrix_state_id(
         p_media_id INTEGER,
         p_parameter_id INTEGER,
         p_matrix_state_id INTEGER DEFAULT NULL
       )
       RETURNS INTEGER
       LANGUAGE plpgsql
       STABLE
       AS $function$
       DECLARE
         v_default_matrix_state_id INTEGER;
         v_unique_matrix_state_id INTEGER;
       BEGIN
         IF p_matrix_state_id IS NOT NULL THEN
           RETURN p_matrix_state_id;
         END IF;

         IF p_media_id IS NOT NULL THEN
           v_default_matrix_state_id :=
             public.default_matrix_state_id_from_media(p_media_id);
         END IF;

         IF p_parameter_id IS NULL THEN
           RETURN v_default_matrix_state_id;
         END IF;

         IF v_default_matrix_state_id IS NOT NULL
            AND COALESCE(
              public.parameter_has_unit_for_matrix_state(
                p_parameter_id,
                v_default_matrix_state_id
              ),
              FALSE
            ) THEN
           RETURN v_default_matrix_state_id;
         END IF;

         v_unique_matrix_state_id :=
           public.get_unique_parameter_matrix_state_id(p_parameter_id);

         RETURN v_unique_matrix_state_id;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.resolve_matrix_state_id(INTEGER, INTEGER, INTEGER)
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.set_matrix_state_from_media()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NEW.matrix_state_id IS NULL THEN
           NEW.matrix_state_id := public.resolve_matrix_state_id(
             NEW.media_id,
             NEW.parameter_id,
             NEW.matrix_state_id
           );
         END IF;

         IF NEW.parameter_id IS NOT NULL AND NEW.matrix_state_id IS NULL THEN
           RAISE EXCEPTION
             'matrix_state_id could not be resolved for parameter_id % and media_id %.',
             NEW.parameter_id,
             NEW.media_id;
         ELSIF NEW.media_id IS NOT NULL AND NEW.matrix_state_id IS NULL THEN
           RAISE EXCEPTION
             'matrix_state_id could not be resolved from media_id %.',
             NEW.media_id;
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.set_matrix_state_from_media() OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.enforce_parameter_unit_for_matrix_state()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NEW.parameter_id IS NOT NULL AND NEW.matrix_state_id IS NULL THEN
           RAISE EXCEPTION
             'matrix_state_id must be populated when parameter_id % is set.',
             NEW.parameter_id;
         END IF;

         IF NEW.parameter_id IS NOT NULL
            AND COALESCE(
              public.parameter_has_unit_for_matrix_state(
                NEW.parameter_id,
                NEW.matrix_state_id
              ),
              FALSE
            ) IS NOT TRUE THEN
           RAISE EXCEPTION
             'Parameter % does not have units defined for matrix_state_id %.',
             NEW.parameter_id,
             NEW.matrix_state_id;
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.enforce_parameter_unit_for_matrix_state()
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.set_result_matrix_state_default()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NEW.matrix_state_id IS NULL THEN
           SELECT public.resolve_matrix_state_id(
             s.media_id,
             NEW.parameter_id,
             NEW.matrix_state_id
           )
           INTO NEW.matrix_state_id
           FROM discrete.samples s
           WHERE s.sample_id = NEW.sample_id;
         END IF;

         IF NEW.matrix_state_id IS NULL THEN
           RAISE EXCEPTION
             'matrix_state_id could not be resolved from sample_id %.',
             NEW.sample_id;
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.set_result_matrix_state_default()
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.enforce_guideline_matrix_state()
       RETURNS TRIGGER
       LANGUAGE plpgsql
       AS $function$
       BEGIN
         IF NEW.matrix_state_id IS NULL THEN
           RAISE EXCEPTION
             'matrix_state_id must be populated for discrete.guidelines.';
         END IF;

         RETURN NEW;
       END;
       $function$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION discrete.enforce_guideline_matrix_state()
       OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ADD COLUMN IF NOT EXISTS matrix_state_id INTEGER
       REFERENCES public.matrix_states(matrix_state_id)
       ON DELETE RESTRICT
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries.matrix_state_id IS
       'Explicit matrix state used to resolve parameter units for the timeseries. Defaults from public.media_types when not supplied.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS timeseries_matrix_state_idx
       ON continuous.timeseries (matrix_state_id);"
    )
    DBI::dbExecute(
      con,
      "UPDATE continuous.timeseries t
       SET matrix_state_id = public.resolve_matrix_state_id(
         t.media_id,
         t.parameter_id,
         t.matrix_state_id
       )
       WHERE t.matrix_state_id IS NULL;"
    )
    unresolved_timeseries_states <- DBI::dbGetQuery(
      con,
      "SELECT timeseries_id, parameter_id, media_id
       FROM continuous.timeseries
       WHERE matrix_state_id IS NULL
       ORDER BY timeseries_id;"
    )
    if (nrow(unresolved_timeseries_states) > 0) {
      stop(
        paste(
          "Patch 39 could not resolve matrix_state_id for one or more",
          "continuous.timeseries rows. Example timeseries_id values:",
          paste(
            head(unresolved_timeseries_states$timeseries_id, 5),
            collapse = ", "
          )
        )
      )
    }
    timeseries_missing_units <- DBI::dbGetQuery(
      con,
      "SELECT timeseries_id, parameter_id, matrix_state_id
       FROM continuous.timeseries
       WHERE public.get_parameter_unit_id(parameter_id, matrix_state_id) IS NULL
       ORDER BY timeseries_id;"
    )
    if (nrow(timeseries_missing_units) > 0) {
      stop(
        paste(
          "Patch 39 found one or more continuous.timeseries rows whose",
          "parameter does not have units for the assigned matrix state.",
          "Example timeseries_id values:",
          paste(
            head(timeseries_missing_units$timeseries_id, 5),
            collapse = ", "
          )
        )
      )
    }
    DBI::dbExecute(
      con,
      "SET CONSTRAINTS ALL IMMEDIATE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ALTER COLUMN matrix_state_id SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       DROP CONSTRAINT IF EXISTS timeseries_unique;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries
       ADD CONSTRAINT timeseries_unique UNIQUE NULLS NOT DISTINCT (
         location_id,
         parameter_id,
         aggregation_type_id,
         media_id,
         matrix_state_id,
         record_rate,
         z_id,
         sensor_priority,
         sub_location_id
       );"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS set_timeseries_matrix_state_default
       ON continuous.timeseries;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS a_set_timeseries_matrix_state_default
       ON continuous.timeseries;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER a_set_timeseries_matrix_state_default
       BEFORE INSERT OR UPDATE OF media_id, matrix_state_id
       ON continuous.timeseries
       FOR EACH ROW
       EXECUTE FUNCTION public.set_matrix_state_from_media();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_timeseries_matrix_state_units
       ON continuous.timeseries;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS z_check_timeseries_matrix_state_units
       ON continuous.timeseries;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER z_check_timeseries_matrix_state_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id, media_id
       ON continuous.timeseries
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )
    DBI::dbExecute(
      con,
      "SET CONSTRAINTS ALL DEFERRED;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
       ADD COLUMN IF NOT EXISTS matrix_state_id INTEGER
       REFERENCES public.matrix_states(matrix_state_id)
       ON DELETE RESTRICT
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.results.matrix_state_id IS
       'Explicit matrix state used to resolve parameter units for the result. Defaults from the parent sample media when not supplied.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS results_matrix_state_idx
       ON discrete.results (matrix_state_id);"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.results r
       SET matrix_state_id = public.resolve_matrix_state_id(
         s.media_id,
         r.parameter_id,
         r.matrix_state_id
       )
       FROM discrete.samples s
       WHERE r.sample_id = s.sample_id
          AND r.matrix_state_id IS NULL;"
    )
    unresolved_result_states <- DBI::dbGetQuery(
      con,
      "SELECT result_id, sample_id, parameter_id
       FROM discrete.results
       WHERE matrix_state_id IS NULL
       ORDER BY result_id;"
    )
    if (nrow(unresolved_result_states) > 0) {
      stop(
        paste(
          "Patch 39 could not resolve matrix_state_id for one or more",
          "discrete.results rows. Example result_id values:",
          paste(head(unresolved_result_states$result_id, 5), collapse = ", ")
        )
      )
    }
    results_missing_units <- DBI::dbGetQuery(
      con,
      "SELECT result_id, parameter_id, matrix_state_id
       FROM discrete.results
       WHERE public.get_parameter_unit_id(parameter_id, matrix_state_id) IS NULL
       ORDER BY result_id;"
    )
    if (nrow(results_missing_units) > 0) {
      stop(
        paste(
          "Patch 39 found one or more discrete.results rows whose",
          "parameter does not have units for the assigned matrix state.",
          "Example result_id values:",
          paste(head(results_missing_units$result_id, 5), collapse = ", ")
        )
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
       ALTER COLUMN matrix_state_id SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
       DROP CONSTRAINT IF EXISTS sampleid_type_parameter_fraction_result_value_key;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.results
       ADD CONSTRAINT sampleid_type_parameter_fraction_result_value_key
       UNIQUE NULLS NOT DISTINCT (
         sample_id,
         result_type,
         parameter_id,
         matrix_state_id,
         sample_fraction_id,
         result_value_type,
         result_speciation_id,
         protocol_method,
         laboratory,
         analysis_datetime
       );"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS set_results_matrix_state_default
       ON discrete.results;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS a_set_results_matrix_state_default
       ON discrete.results;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER a_set_results_matrix_state_default
       BEFORE INSERT OR UPDATE OF sample_id, matrix_state_id
       ON discrete.results
       FOR EACH ROW
       EXECUTE FUNCTION discrete.set_result_matrix_state_default();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_results_matrix_state_units
       ON discrete.results;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS z_check_results_matrix_state_units
       ON discrete.results;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER z_check_results_matrix_state_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id, sample_id
       ON discrete.results
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       ADD COLUMN IF NOT EXISTS matrix_state_id INTEGER
       REFERENCES public.matrix_states(matrix_state_id)
       ON DELETE RESTRICT
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN discrete.guidelines.matrix_state_id IS
       'Explicit matrix state used to resolve the units and applicability of the guideline.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS guidelines_matrix_state_idx
       ON discrete.guidelines (matrix_state_id);"
    )
    DBI::dbExecute(
      con,
      "UPDATE discrete.guidelines g
       SET matrix_state_id = public.get_unique_parameter_matrix_state_id(
         g.parameter_id
       )
       WHERE g.matrix_state_id IS NULL
         AND public.get_unique_parameter_matrix_state_id(g.parameter_id) IS NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "WITH guideline_state_guess AS (
         SELECT
           gm.guideline_id,
            MIN(mt.default_matrix_state_id) AS matrix_state_id,
           COUNT(DISTINCT mt.default_matrix_state_id) AS state_count,
           COUNT(*) FILTER (WHERE mt.default_matrix_state_id IS NULL) AS null_state_count
         FROM discrete.guidelines_media_types gm
         LEFT JOIN public.media_types mt
           ON mt.media_id = gm.media_id
         GROUP BY gm.guideline_id
       )
       UPDATE discrete.guidelines g
       SET matrix_state_id = COALESCE(
         public.get_unique_parameter_matrix_state_id(g.parameter_id),
         CASE
           WHEN gsg.state_count = 1
             AND gsg.null_state_count = 0
             AND public.parameter_has_unit_for_matrix_state(
               g.parameter_id,
               gsg.matrix_state_id
             )
             THEN gsg.matrix_state_id
           ELSE NULL
         END
       )
       FROM guideline_state_guess gsg
       WHERE g.guideline_id = gsg.guideline_id
         AND g.matrix_state_id IS NULL;"
    )
    unresolved_guideline_states <- DBI::dbGetQuery(
      con,
      "SELECT guideline_id, guideline_name
       FROM discrete.guidelines
       WHERE matrix_state_id IS NULL
       ORDER BY guideline_id;"
    )
    if (nrow(unresolved_guideline_states) > 0) {
      stop(
        paste(
          "Patch 39 could not resolve matrix_state_id for one or more",
          "discrete.guidelines rows. Example guideline_id values:",
          paste(
            head(unresolved_guideline_states$guideline_id, 5),
            collapse = ", "
          )
        )
      )
    }
    guidelines_missing_units <- DBI::dbGetQuery(
      con,
      "SELECT guideline_id, parameter_id, matrix_state_id
       FROM discrete.guidelines
       WHERE public.get_parameter_unit_id(parameter_id, matrix_state_id) IS NULL
       ORDER BY guideline_id;"
    )
    if (nrow(guidelines_missing_units) > 0) {
      stop(
        paste(
          "Patch 39 found one or more discrete.guidelines rows whose",
          "parameter does not have units for the assigned matrix state.",
          "Example guideline_id values:",
          paste(head(guidelines_missing_units$guideline_id, 5), collapse = ", ")
        )
      )
    }
    DBI::dbExecute(
      con,
      "ALTER TABLE discrete.guidelines
       ALTER COLUMN matrix_state_id SET NOT NULL;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guidelines_matrix_state
       ON discrete.guidelines;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_guidelines_matrix_state
       BEFORE INSERT OR UPDATE OF matrix_state_id
       ON discrete.guidelines
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_guideline_matrix_state();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guidelines_matrix_state_units
       ON discrete.guidelines;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER check_guidelines_matrix_state_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id
       ON discrete.guidelines
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_guideline_media_matrix_state
       ON discrete.guidelines_media_types;"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.raster_series_index
       ADD COLUMN IF NOT EXISTS matrix_state_id INTEGER
       REFERENCES public.matrix_states(matrix_state_id)
       ON DELETE RESTRICT
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE spatial.rasters_reference
       ADD COLUMN IF NOT EXISTS matrix_state_id INTEGER
       REFERENCES public.matrix_states(matrix_state_id)
       ON DELETE RESTRICT
       ON UPDATE CASCADE;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN spatial.raster_series_index.matrix_state_id IS
       'Explicit matrix state used to resolve parameter units for the raster series. Defaults from public.media_types when not supplied.';"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN spatial.rasters_reference.matrix_state_id IS
       'Explicit matrix state used to resolve parameter units for the raster. Defaults from public.media_types when not supplied.';"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS raster_series_index_matrix_state_idx
       ON spatial.raster_series_index (matrix_state_id);"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS rasters_reference_matrix_state_idx
       ON spatial.rasters_reference (matrix_state_id);"
    )
    DBI::dbExecute(
      con,
      "UPDATE spatial.raster_series_index rsi
       SET matrix_state_id = public.resolve_matrix_state_id(
         rsi.media_id,
         rsi.parameter_id,
         rsi.matrix_state_id
       )
       WHERE rsi.matrix_state_id IS NULL;"
    )
    DBI::dbExecute(
      con,
      "UPDATE spatial.rasters_reference rr
       SET matrix_state_id = public.resolve_matrix_state_id(
         rr.media_id,
         rr.parameter_id,
         rr.matrix_state_id
       )
       WHERE rr.matrix_state_id IS NULL;"
    )
    unresolved_raster_states <- DBI::dbGetQuery(
      con,
      "SELECT source_table, row_id
       FROM (
         SELECT 'spatial.raster_series_index' AS source_table,
                raster_series_id AS row_id
         FROM spatial.raster_series_index
         WHERE parameter_id IS NOT NULL
           AND matrix_state_id IS NULL
         UNION ALL
         SELECT 'spatial.rasters_reference' AS source_table,
                reference_id AS row_id
         FROM spatial.rasters_reference
         WHERE parameter_id IS NOT NULL
           AND matrix_state_id IS NULL
       ) q
       ORDER BY source_table, row_id;"
    )
    if (nrow(unresolved_raster_states) > 0) {
      stop(
        paste(
          "Patch 39 could not resolve matrix_state_id for one or more",
          "spatial raster rows with parameter_id populated. Example rows:",
          paste(
            head(
              paste(
                unresolved_raster_states$source_table,
                unresolved_raster_states$row_id,
                sep = ":"
              ),
              5
            ),
            collapse = ", "
          )
        )
      )
    }
    raster_missing_units <- DBI::dbGetQuery(
      con,
      "SELECT source_table, row_id
       FROM (
         SELECT 'spatial.raster_series_index' AS source_table,
                raster_series_id AS row_id
         FROM spatial.raster_series_index
         WHERE parameter_id IS NOT NULL
           AND public.get_parameter_unit_id(parameter_id, matrix_state_id) IS NULL
         UNION ALL
         SELECT 'spatial.rasters_reference' AS source_table,
                reference_id AS row_id
         FROM spatial.rasters_reference
         WHERE parameter_id IS NOT NULL
           AND public.get_parameter_unit_id(parameter_id, matrix_state_id) IS NULL
       ) q
       ORDER BY source_table, row_id;"
    )
    if (nrow(raster_missing_units) > 0) {
      stop(
        paste(
          "Patch 39 found one or more spatial raster rows whose parameter",
          "does not have units for the assigned matrix state. Example rows:",
          paste(
            head(
              paste(
                raster_missing_units$source_table,
                raster_missing_units$row_id,
                sep = ":"
              ),
              5
            ),
            collapse = ", "
          )
        )
      )
    }
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS set_raster_series_matrix_state_default
       ON spatial.raster_series_index;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS a_set_raster_series_matrix_state_default
       ON spatial.raster_series_index;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER a_set_raster_series_matrix_state_default
       BEFORE INSERT OR UPDATE OF media_id, matrix_state_id
       ON spatial.raster_series_index
       FOR EACH ROW
       EXECUTE FUNCTION public.set_matrix_state_from_media();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_raster_series_matrix_state_units
       ON spatial.raster_series_index;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS z_check_raster_series_matrix_state_units
       ON spatial.raster_series_index;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER z_check_raster_series_matrix_state_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id, media_id
       ON spatial.raster_series_index
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS set_rasters_reference_matrix_state_default
       ON spatial.rasters_reference;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS a_set_rasters_reference_matrix_state_default
       ON spatial.rasters_reference;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER a_set_rasters_reference_matrix_state_default
       BEFORE INSERT OR UPDATE OF media_id, matrix_state_id
       ON spatial.rasters_reference
       FOR EACH ROW
       EXECUTE FUNCTION public.set_matrix_state_from_media();"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS check_rasters_reference_matrix_state_units
       ON spatial.rasters_reference;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS z_check_rasters_reference_matrix_state_units
       ON spatial.rasters_reference;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER z_check_rasters_reference_matrix_state_units
       BEFORE INSERT OR UPDATE OF parameter_id, matrix_state_id, media_id
       ON spatial.rasters_reference
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_unit_for_matrix_state();"
    )

    # Replace schema objects that previously selected the parameter unit text
    # directly from public.parameters.
    DBI::dbExecute(
      con,
      "CREATE OR REPLACE VIEW continuous.timeseries_metadata_en
       WITH(security_invoker=true)
       AS SELECT ts.timeseries_id,
           loc.location_id,
           loc.name AS location_name,
           loc.alias AS alias_name,
           lz.z_meters AS depth_height_m,
           loc.latitude,
           loc.longitude,
           dc.conversion_m AS location_elevation,
            array_agg(DISTINCT proj.name) AS projects,
            array_agg(DISTINCT net.name) AS networks,
            mtypes.media_type,
            params.param_name AS parameter_name,
            public.get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id) AS units,
            at.aggregation_type,
            ts.record_rate AS recording_rate,
            ts.sensor_priority,
           ts.start_datetime,
           ts.end_datetime,
           ts.note
           FROM continuous.timeseries ts
             JOIN public.locations loc ON ts.location_id = loc.location_id
             LEFT JOIN public.parameters params ON ts.parameter_id = params.parameter_id
             LEFT JOIN public.media_types mtypes ON ts.media_id = mtypes.media_id
             LEFT JOIN continuous.aggregation_types at
               ON ts.aggregation_type_id = at.aggregation_type_id
            LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id
            LEFT JOIN public.locations_projects loc_proj
              ON loc.location_id = loc_proj.location_id
            LEFT JOIN public.projects proj ON loc_proj.project_id = proj.project_id
            LEFT JOIN public.locations_networks loc_net
              ON loc.location_id = loc_net.location_id
            LEFT JOIN public.networks net ON loc_net.network_id = net.network_id
             LEFT JOIN public.datum_conversions dc
               ON loc.location_id = dc.location_id
              AND dc.current = true
          GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type,
            params.param_name,
            public.get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id),
            at.aggregation_type,
            lz.z_meters, dc.conversion_m;"
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.timeseries_metadata_en OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      'CREATE OR REPLACE VIEW continuous.timeseries_metadata_fr
       WITH(security_invoker=true)
       AS SELECT ts.timeseries_id,
           loc.location_id,
           loc.name_fr AS nom_endroit,
           loc.alias AS nom_alias,
           lz.z_meters AS profondeur_hauteur_m,
           loc.latitude,
           loc.longitude,
           dc.conversion_m AS "élévation_endroit",
           array_agg(DISTINCT proj.name_fr) AS projets,
            array_agg(DISTINCT net.name_fr) AS "réseaux",
            mtypes.media_type_fr AS "type_de_média",
            params.param_name_fr AS "nom_paramètre",
            public.get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id) AS "unités",
            ag.aggregation_type_fr AS "type_agrégation",
            ts.record_rate AS "fréquence_enregistrement",
            ts.sensor_priority AS "priorité_capteur",
           ts.start_datetime AS "début",
           ts.end_datetime AS fin,
           ts.note
           FROM continuous.timeseries ts
             JOIN public.locations loc ON ts.location_id = loc.location_id
             LEFT JOIN public.parameters params ON ts.parameter_id = params.parameter_id
             LEFT JOIN public.media_types mtypes ON ts.media_id = mtypes.media_id
             LEFT JOIN continuous.aggregation_types ag
               ON ts.aggregation_type_id = ag.aggregation_type_id
            LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id
            LEFT JOIN public.locations_projects loc_proj
              ON loc.location_id = loc_proj.location_id
            LEFT JOIN public.projects proj ON loc_proj.project_id = proj.project_id
            LEFT JOIN public.locations_networks loc_net
              ON loc.location_id = loc_net.location_id
            LEFT JOIN public.networks net ON loc_net.network_id = net.network_id
             LEFT JOIN public.datum_conversions dc
               ON loc.location_id = dc.location_id
              AND dc.current = true
          GROUP BY ts.timeseries_id, loc.location_id, mtypes.media_type_fr,
            params.param_name_fr,
            public.get_parameter_unit_name(ts.parameter_id, ts.matrix_state_id),
            ag.aggregation_type_fr, lz.z_meters, dc.conversion_m;'
    )
    DBI::dbExecute(
      con,
      "ALTER VIEW continuous.timeseries_metadata_fr OWNER TO admin;"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.get_csw_layer()
       RETURNS TABLE(
         location text,
         station_name text,
         station_name_fr text,
         latitude numeric,
         longitude numeric,
         type text,
         owner_name text,
         owner_name_fr text,
         timeseries_id integer,
         parameter_id integer,
         param_name text,
         param_name_fr text,
         param_units text,
         date date,
         value numeric,
         percent_historic_range numeric,
         mean numeric,
         min numeric,
         max numeric,
         doy_count integer,
         drainage_area_km2 numeric,
         datum_name_en text,
         datum_name_fr text
       )
       LANGUAGE sql
       AS $$
       WITH core AS (
         SELECT
           l.location_id,
           l.location_code,
           l.name,
           l.name_fr,
           l.latitude,
           l.longitude,
           lt.type,
           t.timeseries_id,
            t.parameter_id,
            t.sub_location_id,
            p.param_name,
            p.param_name_fr,
            public.get_parameter_unit_name(
              t.parameter_id,
              t.matrix_state_id
            ) AS param_units,
            mcd.date,
            mcd.value,
            mcd.percent_historic_range,
           mcd.mean,
           mcd.min,
           mcd.max,
           mcd.doy_count
         FROM public.locations l
         JOIN public.location_types lt
           ON l.location_type = lt.type_id
          JOIN continuous.timeseries t
            ON t.location_id = l.location_id
          JOIN public.parameters p
            ON p.parameter_id = t.parameter_id
          JOIN continuous.measurements_calculated_daily mcd
            ON mcd.timeseries_id = t.timeseries_id
         WHERE
           lt.type_id IN (1, 2, 16)
           AND l.jurisdictional_relevance IS TRUE
           AND p.parameter_id IN (1150, 1165, 21, 1220)
           AND mcd.date >= NOW() - INTERVAL '30 days'
       ),
       core_locs AS (
         SELECT DISTINCT
           c.location_id,
           c.location_code
         FROM core c
       ),
       loc_owner AS (
         SELECT DISTINCT ON (t2.location_id)
           t2.location_id,
           org.name AS owner_name,
           org.name_fr AS owner_name_fr
         FROM continuous.timeseries t2
         JOIN core_locs cl
           ON cl.location_id = t2.location_id
         JOIN continuous.owners o
           ON o.timeseries_id = t2.timeseries_id
         JOIN public.organizations org
           ON org.organization_id = o.organization_id
         ORDER BY
           t2.location_id,
           o.start_dt DESC,
           o.end_dt DESC,
           o.owner_id DESC
       ),
       datum_current AS (
         SELECT DISTINCT ON (dc.location_id)
           dc.location_id,
           dc.conversion_m,
           dl.datum_name_en,
           dl.datum_name_fr
         FROM public.datum_conversions dc
         JOIN core_locs cl
           ON cl.location_id = dc.location_id
         LEFT JOIN public.datum_list dl
           ON dl.datum_id = dc.datum_id_to
         WHERE dc.current IS TRUE
         ORDER BY
           dc.location_id,
           dc.modified DESC NULLS LAST,
           dc.conversion_id DESC
       ),
       drainage AS (
         SELECT
           v.feature_name,
           (ST_Area(v.geom::geography) / 1000000)::numeric AS drainage_area_km2
         FROM spatial.vectors v
         JOIN core_locs cl
           ON cl.location_code = v.feature_name
         WHERE v.layer_name = 'Drainage basins'
       )
       SELECT
         c.location_code AS location,
         CASE
           WHEN sl.sub_location_name IS NOT NULL
             THEN CONCAT(c.name, ' - ', sl.sub_location_name)
           ELSE c.name
         END AS station_name,
         CASE
           WHEN sl.sub_location_name_fr IS NOT NULL
             THEN CONCAT(c.name_fr, ' - ', sl.sub_location_name_fr)
           ELSE c.name_fr
         END AS station_name_fr,
         c.latitude,
         c.longitude,
         c.type,
         lo.owner_name,
         lo.owner_name_fr,
         c.timeseries_id,
         c.parameter_id,
         c.param_name,
         c.param_name_fr,
         c.param_units,
         c.date,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.value + COALESCE(dc.conversion_m, 0)
           ELSE c.value
         END AS value,
         c.percent_historic_range,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.mean + COALESCE(dc.conversion_m, 0)
           ELSE c.mean
         END AS mean,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.min + COALESCE(dc.conversion_m, 0)
           ELSE c.min
         END AS min,
         CASE
           WHEN c.param_name = 'water level'
             THEN c.max + COALESCE(dc.conversion_m, 0)
           ELSE c.max
         END AS max,
         c.doy_count,
         dr.drainage_area_km2,
         CASE
           WHEN c.param_name = 'water level'
             THEN dc.datum_name_en
           ELSE NULL
         END AS datum_name_en,
         CASE
           WHEN c.param_name = 'water level'
             THEN dc.datum_name_fr
           ELSE NULL
         END AS datum_name_fr
       FROM core c
       LEFT JOIN public.sub_locations sl
         ON sl.sub_location_id = c.sub_location_id
       LEFT JOIN loc_owner lo
         ON lo.location_id = c.location_id
       LEFT JOIN datum_current dc
         ON dc.location_id = c.location_id
       LEFT JOIN drainage dr
         ON dr.feature_name = c.location_code
       ORDER BY c.location_code, c.param_name, c.date;
       $$;"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.get_csw_layer() OWNER TO admin;"
    )

    # Backfill any record_units values that were still NULL using the new FK
    # columns on public.parameters.
    DBI::dbExecute(
      con,
      "UPDATE public.locations_metadata_instruments lmi
       SET record_units = public.get_parameter_unit_id(
         ts.parameter_id,
         ts.matrix_state_id
       )
       FROM continuous.timeseries ts
       WHERE lmi.timeseries_id = ts.timeseries_id
          AND lmi.record_units IS NULL
          AND public.get_parameter_unit_id(
            ts.parameter_id,
            ts.matrix_state_id
          ) IS NOT NULL;"
    )

    # Remove the deprecated free-text columns now that the units table is the
    # authoritative source for parameter units.
    if (unit_cols_are_text) {
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         DROP COLUMN IF EXISTS units_liquid_text;"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         DROP COLUMN IF EXISTS units_solid_text;"
      )
      DBI::dbExecute(
        con,
        "ALTER TABLE public.parameters
         DROP COLUMN IF EXISTS units_gas_text;"
      )
    }

    # Modify the calibrations table to use 1, 2, or 3 point calibration.
    table_sql <- as.character(
      DBI::dbQuoteIdentifier(
        con,
        DBI::Id(
          schema = "instruments",
          table = "calibrate_specific_conductance"
        )
      )
    )

    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ADD COLUMN IF NOT EXISTS calibration_points integer"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ADD COLUMN IF NOT EXISTS spc3_std numeric"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ADD COLUMN IF NOT EXISTS spc3_pre numeric"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ADD COLUMN IF NOT EXISTS spc3_post numeric"
      )
    )

    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ALTER COLUMN spc2_std DROP NOT NULL"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ALTER COLUMN spc2_pre DROP NOT NULL"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ALTER COLUMN spc2_post DROP NOT NULL"
      )
    )

    DBI::dbExecute(
      con,
      paste0(
        "UPDATE ",
        table_sql,
        " SET calibration_points = CASE ",
        "WHEN spc3_std IS NOT NULL OR spc3_pre IS NOT NULL OR spc3_post IS NOT NULL THEN 3 ",
        "WHEN spc2_std IS NOT NULL OR spc2_pre IS NOT NULL OR spc2_post IS NOT NULL THEN 2 ",
        "ELSE 1 END ",
        "WHERE calibration_points IS NULL"
      )
    )

    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ALTER COLUMN calibration_points SET DEFAULT 2"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ALTER COLUMN calibration_points SET NOT NULL"
      )
    )

    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " DROP CONSTRAINT IF EXISTS ",
        "calibrate_specific_conductance_point_count_chk"
      )
    )
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE ",
        table_sql,
        " ADD CONSTRAINT calibrate_specific_conductance_point_count_chk CHECK (",
        "  calibration_points BETWEEN 1 AND 3",
        "  AND spc1_std IS NOT NULL",
        "  AND spc1_pre IS NOT NULL",
        "  AND spc1_post IS NOT NULL",
        "  AND (",
        "    (",
        "      calibration_points = 1",
        "      AND spc2_std IS NULL",
        "      AND spc2_pre IS NULL",
        "      AND spc2_post IS NULL",
        "      AND spc3_std IS NULL",
        "      AND spc3_pre IS NULL",
        "      AND spc3_post IS NULL",
        "    )",
        "    OR (",
        "      calibration_points = 2",
        "      AND spc2_std IS NOT NULL",
        "      AND spc2_pre IS NOT NULL",
        "      AND spc2_post IS NOT NULL",
        "      AND spc3_std IS NULL",
        "      AND spc3_pre IS NULL",
        "      AND spc3_post IS NULL",
        "    )",
        "    OR (",
        "      calibration_points = 3",
        "      AND spc2_std IS NOT NULL",
        "      AND spc2_pre IS NOT NULL",
        "      AND spc2_post IS NOT NULL",
        "      AND spc3_std IS NOT NULL",
        "      AND spc3_pre IS NOT NULL",
        "      AND spc3_post IS NOT NULL",
        "    )",
        "  )",
        ")"
      )
    )

    # Collapse consecutive same-value qualifying rows into continuous periods before purging the audit tables, so the cleanup itself does not leave a large volume of noise in the logs.
    # The adjust_* functions have been modified to stop producing new rows unecessarily.
    collapse_same_value_segments <- function(table_sql, id_col, value_col) {
      collapse_sql <- sprintf(
        paste(
          "WITH sequenced AS (",
          "  SELECT",
          "    %1$s AS row_id,",
          "    timeseries_id,",
          "    %2$s AS value_id,",
          "    start_dt,",
          "    end_dt,",
          "    ROW_NUMBER() OVER (",
          "      PARTITION BY timeseries_id",
          "      ORDER BY start_dt, end_dt, %1$s",
          "    ) AS rn,",
          "    LAG(%2$s) OVER (",
          "      PARTITION BY timeseries_id",
          "      ORDER BY start_dt, end_dt, %1$s",
          "    ) AS prev_value_id",
          "  FROM %3$s",
          "),",
          "ordered AS (",
          "  SELECT",
          "    row_id,",
          "    timeseries_id,",
          "    start_dt,",
          "    end_dt,",
          "    SUM(",
          "      CASE WHEN rn = 1 OR prev_value_id IS DISTINCT FROM value_id THEN 1 ELSE 0 END",
          "    ) OVER (",
          "      PARTITION BY timeseries_id",
          "      ORDER BY start_dt, end_dt, row_id",
          "      ROWS UNBOUNDED PRECEDING",
          "    ) AS grp",
          "  FROM sequenced",
          "),",
          "group_bounds AS (",
          "  SELECT",
          "    timeseries_id,",
          "    grp,",
          "    MAX(end_dt) AS grp_end,",
          "    COUNT(*) AS grp_rows",
          "  FROM ordered",
          "  GROUP BY timeseries_id, grp",
          "  HAVING COUNT(*) > 1",
          "),",
          "keepers AS (",
          "  SELECT DISTINCT ON (o.timeseries_id, o.grp)",
          "    o.row_id,",
          "    o.timeseries_id,",
          "    o.grp",
          "  FROM ordered o",
          "  JOIN group_bounds gb",
          "    ON gb.timeseries_id = o.timeseries_id",
          "   AND gb.grp = o.grp",
          "  ORDER BY o.timeseries_id, o.grp, o.start_dt, o.end_dt, o.row_id",
          "),",
          "updated AS (",
          "  UPDATE %3$s t",
          "  SET",
          "    end_dt = gb.grp_end,",
          "    modified = CURRENT_TIMESTAMP,",
          "    modified_by = SESSION_USER",
          "  FROM keepers k",
          "  JOIN group_bounds gb",
          "    ON gb.timeseries_id = k.timeseries_id",
          "   AND gb.grp = k.grp",
          "  WHERE t.%1$s = k.row_id",
          "    AND t.end_dt IS DISTINCT FROM gb.grp_end",
          "  RETURNING 1",
          "),",
          "deleted AS (",
          "  DELETE FROM %3$s t",
          "  USING ordered o",
          "  JOIN keepers k",
          "    ON k.timeseries_id = o.timeseries_id",
          "   AND k.grp = o.grp",
          "  WHERE t.%1$s = o.row_id",
          "    AND t.%1$s <> k.row_id",
          "  RETURNING 1",
          ")",
          "SELECT",
          "  COALESCE((SELECT COUNT(*) FROM updated), 0) AS rows_extended,",
          "  COALESCE((SELECT COUNT(*) FROM deleted), 0) AS rows_deleted;",
          sep = "\n"
        ),
        id_col,
        value_col,
        table_sql
      )

      result <- DBI::dbGetQuery(con, collapse_sql)
      message(
        sprintf(
          paste(
            "Collapsed consecutive same-value rows in %s:",
            "%s rows extended, %s rows deleted."
          ),
          table_sql,
          result$rows_extended[[1]],
          result$rows_deleted[[1]]
        )
      )
    }

    message(
      "Collapsing consecutive same-value segments in the continuous schema..."
    )
    collapse_same_value_segments(
      table_sql = "continuous.grades",
      id_col = "grade_id",
      value_col = "grade_type_id"
    )
    collapse_same_value_segments(
      table_sql = "continuous.approvals",
      id_col = "approval_id",
      value_col = "approval_type_id"
    )
    collapse_same_value_segments(
      table_sql = "continuous.qualifiers",
      id_col = "qualifier_id",
      value_col = "qualifier_type_id"
    )
    collapse_same_value_segments(
      table_sql = "continuous.owners",
      id_col = "owner_id",
      value_col = "organization_id"
    )
    collapse_same_value_segments(
      table_sql = "continuous.contributors",
      id_col = "contributor_id",
      value_col = "organization_id"
    )

    # Wipe the audit tables since the data changes in this patch will cause a large number of audit records to be generated that aren't meaningful to retain.
    DBI::dbExecute(con, "DELETE FROM audit.measurements_calculated_daily_log")
    DBI::dbExecute(con, "DELETE FROM audit.measurements_continuous_log")
    DBI::dbExecute(con, "DELETE FROM audit.general_log")

    # Wrap things up ########################################################
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '39'
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

    message("Patch 39 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 39 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
