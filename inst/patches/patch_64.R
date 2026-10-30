# Patch 64: enforce parameter-specific descriptors on import mappings.

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (!identical(check$session_user[[1]], "postgres")) {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 64: requiring sample fractions and speciations on import mappings when their AquaCache parameter requires them. Changes are being made within a transaction, so an error will roll back the database."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback it before running this patch."
  )
}

active <- dbTransBegin(con)
tryCatch(
  {
    required <- DBI::dbGetQuery(
      con,
      "SELECT
         to_regclass('discrete.import_parameter_mappings') IS NOT NULL
           AS has_parameter_mappings,
         to_regclass('public.parameters') IS NOT NULL AS has_parameters,
         to_regclass('information.version_info') IS NOT NULL
           AS has_version_info,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'parameters'
             AND column_name = 'sample_fraction'
         ) AS has_sample_fraction_requirement,
         EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'parameters'
             AND column_name = 'result_speciation'
         ) AS has_speciation_requirement"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "Patch 64 requires Patch 61 import parameter mappings and the public.parameters sample_fraction and result_speciation columns."
      )
    }

    last_patch <- DBI::dbGetQuery(
      con,
      "SELECT version
       FROM information.version_info
       WHERE item = 'Last patch number'"
    )$version
    if (length(last_patch) != 1L || last_patch != "63") {
      stop("Patch 64 must be applied to a database at Patch 63.")
    }

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION discrete.enforce_import_parameter_mapping_requirements()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, discrete
       AS $function$
       DECLARE
         requires_sample_fraction BOOLEAN;
         requires_result_speciation BOOLEAN;
         missing_required_sample_fraction BOOLEAN;
         missing_required_speciation BOOLEAN;
       BEGIN
         IF NEW.parameter_id IS NULL THEN
           RETURN NEW;
         END IF;

         SELECT parameter.sample_fraction, parameter.result_speciation
           INTO requires_sample_fraction, requires_result_speciation
           FROM public.parameters AS parameter
          WHERE parameter.parameter_id = NEW.parameter_id
          FOR UPDATE;

         -- Leave missing parameter IDs to the mapping table's foreign key.
         IF NOT FOUND THEN
           RETURN NEW;
         END IF;

         missing_required_sample_fraction :=
           requires_sample_fraction IS TRUE
           AND NEW.sample_fraction_id IS NULL;
         missing_required_speciation :=
           requires_result_speciation IS TRUE
           AND NEW.result_speciation_id IS NULL;

         IF missing_required_sample_fraction OR missing_required_speciation THEN
           -- Draft creation copies the previous published snapshot first.
           -- Keep an unchanged legacy row so its owner can repair or disable it.
           IF TG_OP = 'INSERT' AND EXISTS (
             SELECT 1
             FROM discrete.import_mapping_sets AS current_set
             JOIN discrete.import_mapping_sets AS previous_set
               ON previous_set.import_source_id = current_set.import_source_id
              AND previous_set.import_profile_id
                    IS NOT DISTINCT FROM current_set.import_profile_id
              AND previous_set.status IN ('published', 'retired')
              AND previous_set.version < current_set.version
             JOIN discrete.import_parameter_mappings AS previous_mapping
               ON previous_mapping.import_mapping_set_id =
                    previous_set.import_mapping_set_id
             WHERE current_set.import_mapping_set_id =
                     NEW.import_mapping_set_id
               AND current_set.status = 'draft'
               AND previous_mapping.source_match = NEW.source_match
               AND previous_mapping.parameter_id
                     IS NOT DISTINCT FROM NEW.parameter_id
               AND previous_mapping.result_type
                     IS NOT DISTINCT FROM NEW.result_type
               AND previous_mapping.sample_fraction_id
                     IS NOT DISTINCT FROM NEW.sample_fraction_id
               AND previous_mapping.result_value_type
                     IS NOT DISTINCT FROM NEW.result_value_type
               AND previous_mapping.result_speciation_id
                     IS NOT DISTINCT FROM NEW.result_speciation_id
               AND previous_mapping.matrix_state_id
                     IS NOT DISTINCT FROM NEW.matrix_state_id
               AND previous_mapping.conversion
                     IS NOT DISTINCT FROM NEW.conversion
               AND previous_mapping.result_offset
                     IS NOT DISTINCT FROM NEW.result_offset
               AND previous_mapping.priority
                     IS NOT DISTINCT FROM NEW.priority
               AND previous_mapping.active
                     IS NOT DISTINCT FROM NEW.active
               AND previous_mapping.note IS NOT DISTINCT FROM NEW.note
           ) THEN
             RETURN NEW;
           END IF;

           IF TG_OP = 'UPDATE' THEN
             IF NEW.active IS FALSE
                AND NEW.parameter_id IS NOT DISTINCT FROM OLD.parameter_id
                AND NEW.sample_fraction_id
                      IS NOT DISTINCT FROM OLD.sample_fraction_id
                AND NEW.result_speciation_id
                      IS NOT DISTINCT FROM OLD.result_speciation_id THEN
               RETURN NEW;
             END IF;
           END IF;

           IF missing_required_sample_fraction THEN
             RAISE EXCEPTION USING
               ERRCODE = '23514',
               CONSTRAINT = 'import_parameter_mappings_required_descriptors',
               MESSAGE = 'Sample fraction is required for the selected parameter. This is enforced by the sample_fraction boolean column of table public.parameters.';
           ELSE
             RAISE EXCEPTION USING
               ERRCODE = '23514',
               CONSTRAINT = 'import_parameter_mappings_required_descriptors',
               MESSAGE = 'Speciation is required for the selected parameter This is enforced by the result_speciation boolean column of table public.parameters.';
           END IF;
         END IF;

         RETURN NEW;
       END;
       $function$"
    )
    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS import_parameter_mapping_required_descriptors
       ON discrete.import_parameter_mappings"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER import_parameter_mapping_required_descriptors
       BEFORE INSERT OR UPDATE
       ON discrete.import_parameter_mappings
       FOR EACH ROW
       EXECUTE FUNCTION discrete.enforce_import_parameter_mapping_requirements()"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.enforce_parameter_import_mapping_requirements()
       RETURNS trigger
       LANGUAGE plpgsql
       SECURITY DEFINER
       SET search_path = pg_catalog, public, discrete
       AS $function$
       BEGIN
         IF NEW.sample_fraction IS TRUE
            AND OLD.sample_fraction IS DISTINCT FROM TRUE
            AND EXISTS (
             SELECT 1
             FROM discrete.import_parameter_mappings mapping
             JOIN discrete.import_mapping_sets mapping_set
               USING (import_mapping_set_id)
             WHERE mapping.parameter_id = NEW.parameter_id
               AND mapping.active IS TRUE
               AND mapping_set.status IN ('draft', 'published')
               AND mapping.sample_fraction_id IS NULL
            ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'parameters_import_mapping_required_descriptors',
             MESSAGE = 'Cannot require a sample fraction while an import mapping for this parameter has no sample fraction.';
         END IF;

         IF NEW.result_speciation IS TRUE
            AND OLD.result_speciation IS DISTINCT FROM TRUE
            AND EXISTS (
              SELECT 1
              FROM discrete.import_parameter_mappings mapping
              JOIN discrete.import_mapping_sets mapping_set
                USING (import_mapping_set_id)
              WHERE mapping.parameter_id = NEW.parameter_id
                AND mapping.active IS TRUE
                AND mapping_set.status IN ('draft', 'published')
                AND mapping.result_speciation_id IS NULL
            ) THEN
           RAISE EXCEPTION USING
             ERRCODE = '23514',
             CONSTRAINT = 'parameters_import_mapping_required_descriptors',
             MESSAGE = 'Cannot require a speciation while an import mapping for this parameter has no speciation.';
         END IF;

         RETURN NEW;
       END;
       $function$"
    )

    DBI::dbExecute(
      con,
      "DROP TRIGGER IF EXISTS parameters_import_mapping_required_descriptors
       ON public.parameters"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER parameters_import_mapping_required_descriptors
       BEFORE UPDATE OF sample_fraction, result_speciation
       ON public.parameters
       FOR EACH ROW
       EXECUTE FUNCTION public.enforce_parameter_import_mapping_requirements()"
    )

    verified <- DBI::dbGetQuery(
      con,
      "SELECT
         EXISTS (
           SELECT 1
           FROM pg_trigger
           WHERE tgrelid = 'discrete.import_parameter_mappings'::regclass
             AND tgname = 'import_parameter_mapping_required_descriptors'
             AND NOT tgisinternal
         ) AS mapping_trigger_exists,
         EXISTS (
           SELECT 1
           FROM pg_trigger
           WHERE tgrelid = 'public.parameters'::regclass
             AND tgname = 'parameters_import_mapping_required_descriptors'
             AND NOT tgisinternal
         ) AS parameter_trigger_exists"
    )
    if (!all(unlist(verified[1, ], use.names = FALSE))) {
      stop("Patch 64 verification failed.")
    }

    legacy_invalid_count <- DBI::dbGetQuery(
      con,
      "SELECT count(*) AS invalid_count
       FROM discrete.import_parameter_mappings mapping
       JOIN discrete.import_mapping_sets mapping_set
         USING (import_mapping_set_id)
       JOIN public.parameters parameter
         ON parameter.parameter_id = mapping.parameter_id
       WHERE mapping.active IS TRUE
         AND mapping_set.status IN ('draft', 'published')
         AND ((parameter.sample_fraction IS TRUE
               AND mapping.sample_fraction_id IS NULL)
           OR (parameter.result_speciation IS TRUE
               AND mapping.result_speciation_id IS NULL))"
    )$invalid_count[[1]]
    if (legacy_invalid_count > 0L) {
      message(
        "Patch 64 preserved ",
        legacy_invalid_count,
        " existing active mapping(s) with missing required descriptors. Repair or deactivate those mappings; new or changed invalid mappings are blocked."
      )
    }

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '64'
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
      "Patch 64 applied successfully. Import parameter mappings now require descriptors mandated by public.parameters, and those requirements cannot be changed while invalid mappings exist."
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
