# Patch 62 adds an exclusive parameter unit for matrix-independent results.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 62: adding the not-applicable matrix state and exclusive parameter units. Changes are being made within a transaction, so an error will roll back the database."
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
         to_regclass('public.parameters') IS NOT NULL AS has_parameters,
         to_regclass('public.units') IS NOT NULL AS has_units,
         to_regclass('public.matrix_states') IS NOT NULL AS has_matrix_states,
         to_regclass('discrete.results') IS NOT NULL AS has_results,
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.measurements_continuous') IS NOT NULL AS has_measurements,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info,
         to_regprocedure(
           'public.get_parameter_unit_id(integer,integer)'
         ) IS NOT NULL AS has_unit_resolver,
         to_regprocedure(
           'public.get_unique_parameter_matrix_state_id(integer)'
         ) IS NOT NULL AS has_unique_state_resolver,
         to_regprocedure(
           'public.parameter_matrix_state_has_results(integer,text)'
         ) IS NOT NULL AS has_unit_usage_guard"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 62 requires parameter, unit, matrix-state, discrete-result, continuous-timeseries, unit-resolution, used-unit guard, and version schemas from earlier patches."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "61") {
      stop("Patch 62 must be applied to a database at Patch 61.")
    }

    DBI::dbExecute(
      con,
      "INSERT INTO public.matrix_states
         (matrix_state_code, matrix_state_name, matrix_state_name_fr, description)
       VALUES
         ('not_applicable', 'Not applicable', 'Sans objet',
          'The measured parameter does not describe a liquid, solid, or gas matrix.')
       ON CONFLICT (matrix_state_code) DO UPDATE
       SET matrix_state_name = EXCLUDED.matrix_state_name,
           matrix_state_name_fr = EXCLUDED.matrix_state_name_fr,
           description = EXCLUDED.description;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       ADD COLUMN IF NOT EXISTS units_na INTEGER;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_units_na_fkey;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       ADD CONSTRAINT parameters_units_na_fkey
       FOREIGN KEY (units_na)
       REFERENCES public.units(unit_id)
       ON UPDATE CASCADE
       ON DELETE RESTRICT;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       DROP CONSTRAINT IF EXISTS parameters_units_na_exclusive_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.parameters
       ADD CONSTRAINT parameters_units_na_exclusive_check
       CHECK (
         units_na IS NULL
         OR (units_liquid IS NULL AND units_solid IS NULL AND units_gas IS NULL)
       );"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX IF NOT EXISTS parameters_units_na_idx
       ON public.parameters (units_na);"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.parameters.units_na IS
       'Foreign key to public.units for a parameter with no applicable liquid, solid, or gas matrix state. When set, the other parameter unit columns must be NULL.';"
    )

    # The Patch 39 row-validation triggers run on the following updates. Let
    # them resolve the new state through the existing liquid unit until the
    # well-depth unit is moved to units_na below.
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
           WHEN 'not_applicable' THEN COALESCE(p.units_na, p.units_liquid)
           ELSE NULL
         END
         FROM public.parameters p
         JOIN public.matrix_states ms
           ON ms.matrix_state_id = p_matrix_state_id
         WHERE p.parameter_id = p_parameter_id;
       $function$;"
    )

    # Well depth was historically assigned a liquid unit. Reclassify its
    # stored rows while the transitional resolver still recognizes its unit.
    DBI::dbExecute(
      con,
      "DO $migration$
       DECLARE
         v_parameter_id integer;
         v_matrix_state_id integer;
       BEGIN
         SELECT matrix_state_id
         INTO v_matrix_state_id
         FROM public.matrix_states
         WHERE matrix_state_code = 'not_applicable';

         FOR v_parameter_id IN
           SELECT parameter_id
           FROM public.parameters
           WHERE lower(btrim(param_name)) = 'well depth'
         LOOP
           UPDATE discrete.results
           SET matrix_state_id = v_matrix_state_id
           WHERE parameter_id = v_parameter_id
             AND matrix_state_id IS DISTINCT FROM v_matrix_state_id;

           UPDATE continuous.timeseries
           SET matrix_state_id = v_matrix_state_id
           WHERE parameter_id = v_parameter_id
             AND matrix_state_id IS DISTINCT FROM v_matrix_state_id;

           UPDATE public.parameters
           SET units_na = units_liquid,
               units_liquid = NULL
           WHERE parameter_id = v_parameter_id
             AND units_liquid IS NOT NULL
             AND units_na IS NULL
             AND units_solid IS NULL
             AND units_gas IS NULL;
         END LOOP;
       END;
       $migration$;"
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
           WHEN 'not_applicable' THEN p.units_na
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
               ('gas', p.units_gas),
               ('not_applicable', p.units_na)
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
      "CREATE OR REPLACE FUNCTION public.prevent_used_parameter_unit_update()
       RETURNS trigger AS $$
       DECLARE
         unit_fields text[] := ARRAY[
           'liquid', 'solid', 'gas', 'not_applicable'
         ];
         unit_columns text[] := ARRAY[
           'units_liquid', 'units_solid', 'units_gas', 'units_na'
         ];
         i integer;
       BEGIN
         FOR i IN 1..array_length(unit_fields, 1) LOOP
           IF (to_jsonb(OLD) ->> unit_columns[i]) IS NOT NULL
              AND (to_jsonb(OLD) -> unit_columns[i]) IS DISTINCT FROM
                  (to_jsonb(NEW) -> unit_columns[i])
              AND public.parameter_matrix_state_has_results(
                OLD.parameter_id,
                unit_fields[i]
              ) THEN
             RAISE EXCEPTION
               'Cannot change % for parameter_id %. Existing results or continuous measurements use the assigned unit.',
               unit_columns[i],
               OLD.parameter_id
               USING ERRCODE = 'check_violation';
           END IF;
         END LOOP;
         RETURN NEW;
       END;
       $$ LANGUAGE plpgsql;"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS prevent_used_parameter_unit_update
       ON public.parameters;"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER prevent_used_parameter_unit_update
       BEFORE UPDATE OF units_liquid, units_solid, units_gas, units_na
       ON public.parameters
       FOR EACH ROW
       EXECUTE FUNCTION public.prevent_used_parameter_unit_update();"
    )

    verification <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1 FROM public.matrix_states
           WHERE matrix_state_code = 'not_applicable'
             AND matrix_state_name = 'Not applicable'
             AND matrix_state_name_fr = 'Sans objet'
         ) AS has_matrix_state,
         EXISTS (
           SELECT 1 FROM information_schema.columns
           WHERE table_schema = 'public' AND table_name = 'parameters'
             AND column_name = 'units_na' AND data_type = 'integer'
         ) AS has_units_na,
         EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conrelid = 'public.parameters'::regclass
             AND conname = 'parameters_units_na_fkey' AND contype = 'f'
         ) AS has_units_na_fkey,
         EXISTS (
           SELECT 1 FROM pg_constraint
           WHERE conrelid = 'public.parameters'::regclass
             AND conname = 'parameters_units_na_exclusive_check' AND contype = 'c'
         ) AS has_exclusive_check,
         EXISTS (
           SELECT 1 FROM pg_trigger
           WHERE tgrelid = 'public.parameters'::regclass
             AND tgname = 'prevent_used_parameter_unit_update'
             AND NOT tgisinternal
         ) AS has_unit_guard,
         NOT EXISTS (
           SELECT 1 FROM public.parameters
           WHERE lower(btrim(param_name)) = 'well depth'
             AND units_liquid IS NOT NULL
         ) OR EXISTS (
           SELECT 1
           FROM public.parameters p
           WHERE lower(btrim(p.param_name)) = 'well depth'
             AND p.units_na IS NOT NULL
             AND p.units_liquid IS NULL
         ) AS well_depth_reclassified"
    )
    if (!all(unlist(verification[1, ], use.names = FALSE))) {
      stop("Patch 62 verification failed.")
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '62'
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
      "Patch 62 applied successfully. Matrix-independent parameter units are supported and well depth now uses the not-applicable state."
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
