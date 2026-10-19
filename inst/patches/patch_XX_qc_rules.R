# Patch XX adds post-import quality-control rules for continuous timeseries:
# a rule catalogue (public.qc_rule_capabilities) and per-timeseries rule
# assignments (continuous.timeseries_qc_rules), mirroring the source-adapter
# pair from patch 56. gradeContinuous() applies the assigned rules and writes
# continuous.qualifiers. Also adds the qualifier types the rules emit (CT,
# SSM, PSM) where missing.
#
# NUMBERING: this file is deliberately named patch_XX_qc_rules.R so that
# AquaConnect() (pattern ^patch_[0-9]+\.R) does not pick it up. When merged,
# rename it to the next patch number and set patch_number below. Until then
# it can be sourced by hand for testing; it then skips version_info.
patch_number <- NA_integer_

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on the QC rules patch: adding the QC rule catalogue, per-timeseries rule assignments, and the CT, SSM and PSM qualifier types. Changes are being made within a transaction, so an error will roll back the database."
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
         to_regclass('continuous.timeseries') IS NOT NULL AS has_timeseries,
         to_regclass('continuous.qualifiers') IS NOT NULL AS has_qualifiers,
         to_regclass('public.qualifier_types') IS NOT NULL AS has_qualifier_types,
         to_regclass('public.source_adapter_capabilities') IS NOT NULL AS has_adapters,
         to_regprocedure('continuous.measurements_continuous_corrected(integer,timestamp with time zone,timestamp with time zone,text,integer)') IS NOT NULL AS has_corrected_fx,
         to_regclass('information.version_info') IS NOT NULL AS has_version_info"
    )
    if (!all(unlist(required[1, ], use.names = FALSE))) {
      stop(
        "The QC rules patch requires continuous timeseries and qualifiers, qualifier types, the patch 56 source-adapter registry, the patch 53 corrected-measurements function, and version metadata."
      )
    }
    if (!is.na(patch_number)) {
      last_patch <- DBI::dbGetQuery(
        con,
        "SELECT version
         FROM information.version_info
         WHERE item = 'Last patch number'"
      )$version
      if (
        length(last_patch) != 1L ||
          last_patch != as.character(patch_number - 1L)
      ) {
        stop(
          "Patch ", patch_number, " must be applied to a database at Patch ",
          patch_number - 1L, "."
        )
      }
    }

    # --- Qualifier types the rules emit -------------------------------------
    # DO NOTHING on conflict: databases that already created these codes
    # (NWT ECC did, for its legacy flags) keep their rows and ids. French
    # descriptions and colours are then filled only where still NULL.
    new_types <- data.frame(
      code = c("CT", "SSM", "PSM"),
      description = c(
        "Too cold for rain: rain recorded while air temperature was below freezing",
        "Likely false tipping bucket tips from snowmelt",
        "Potential false tipping bucket tips from snowmelt"
      ),
      description_fr = c(
        "Trop froid pour de la pluie : pluie enregistrée alors que la température de l'air était sous le point de congélation",
        "Basculements probablement erronés de l'auget causés par la fonte de la neige",
        "Basculements possiblement erronés de l'auget causés par la fonte de la neige"
      ),
      color_code = c("#5dade2", "#8e44ad", "#c39bd3")
    )
    for (i in seq_len(nrow(new_types))) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.qualifier_types
           (qualifier_type_code, qualifier_type_description,
            qualifier_type_description_fr, color_code)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (qualifier_type_code) DO NOTHING",
        params = list(
          new_types$code[i],
          new_types$description[i],
          new_types$description_fr[i],
          new_types$color_code[i]
        )
      )
      DBI::dbExecute(
        con,
        "UPDATE public.qualifier_types
         SET qualifier_type_description_fr =
               COALESCE(qualifier_type_description_fr, $2),
             color_code = COALESCE(color_code, $3)
         WHERE qualifier_type_code = $1",
        params = list(
          new_types$code[i],
          new_types$description_fr[i],
          new_types$color_code[i]
        )
      )
    }

    # --- Rule catalogue -----------------------------------------------------
    DBI::dbExecute(
      con,
      "CREATE TABLE public.qc_rule_capabilities (
         rule_fx TEXT PRIMARY KEY,
         description TEXT NOT NULL,
         companion_roles TEXT[] NOT NULL DEFAULT ARRAY[]::text[],
         emits TEXT[] NOT NULL,
         argument_schema JSONB NOT NULL DEFAULT
           '{\"schema_version\":1,\"arguments\":[]}'::jsonb,
         enabled BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT qc_rule_capabilities_fx_not_blank
           CHECK (btrim(rule_fx) <> ''),
         CONSTRAINT qc_rule_capabilities_emits_not_empty
           CHECK (cardinality(emits) > 0),
         CONSTRAINT qc_rule_capabilities_argument_schema_object CHECK (
           jsonb_typeof(argument_schema) = 'object'
           AND argument_schema @> '{\"schema_version\":1}'::jsonb
           AND jsonb_typeof(argument_schema -> 'arguments') = 'array'
         )
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE public.qc_rule_capabilities OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE public.qc_rule_capabilities IS
       'Catalogue of post-import quality-control rules for continuous timeseries. Each rule_fx is implemented in R and registered in AquaCache''s qc_rule_registry; gradeContinuous() applies the rules assigned in continuous.timeseries_qc_rules and writes continuous.qualifiers.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.qc_rule_capabilities.companion_roles IS
       'Other series the rule reads, by role (e.g. air_temp, wind_speed). Resolved per timeseries from rule_args.companions, or automatically from the same location''s series of the role''s parameter with the lowest sensor_priority.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.qc_rule_capabilities.emits IS
       'Qualifier type codes the rule can write. Inside its graded window, gradeContinuous() may replace unprotected qualifiers of exactly these codes, its baseline (UNS), and UNK.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.qc_rule_capabilities.argument_schema IS
       'Rule arguments with defaults, in the same versioned shape as source_adapter_capabilities.argument_schema. Defaults are overridden per assignment by timeseries_qc_rules.rule_args.'"
    )

    arg <- function(name, type, default, description = NULL) {
      out <- list(name = name, type = type, default = default)
      if (!is.null(description)) {
        out$description <- description
      }
      out
    }
    schema_json <- function(...) {
      as.character(jsonlite::toJSON(
        list(schema_version = 1L, arguments = list(...)),
        auto_unbox = TRUE
      ))
    }
    rules <- list(
      list(
        rule_fx = "qc_rain_cold",
        description = "Rain recorded while air temperature is below a threshold.",
        companion_roles = "{air_temp}",
        emits = "{CT}",
        argument_schema = schema_json(
          arg("max_air_temp", "number", -0.1, "Rain above 0 is flagged when air temperature is below this (deg C)."),
          arg("qualifier", "string", "CT")
        )
      ),
      list(
        rule_fx = "qc_stuck_sensor",
        description = "Rolling standard deviation below a minimum: a stuck or frozen sensor. Optionally splits stuck zero readings by air temperature (wind speed).",
        companion_roles = "{air_temp}",
        emits = "{ERROR,SUS}",
        argument_schema = schema_json(
          arg("width", "integer", 5L, "Readings in the centred window."),
          arg("min_sd", "number", 0.001, "Standard deviation below which readings are stuck, e.g. 0.001 for wind speed, 0.1 for wind direction."),
          arg("qualifier", "string", "ERROR"),
          arg("zero_split", "boolean", FALSE, "Wind speed: a stuck 0 is zero_qualifier below zero_max_air_temp or with no air temperature (frozen propeller or a calm cold night), and not flagged at or above it (calm is real)."),
          arg("zero_max_air_temp", "number", 0),
          arg("zero_qualifier", "string", "SUS")
        )
      ),
      list(
        rule_fx = "qc_dir_calm",
        description = "Wind direction recorded while wind speed is at or below a threshold.",
        companion_roles = "{wind_speed}",
        emits = "{ERROR}",
        argument_schema = schema_json(
          arg("max_wind_speed", "number", 0),
          arg("qualifier", "string", "ERROR")
        )
      ),
      list(
        rule_fx = "qc_snowmelt_tips",
        description = "Possible false tipping-bucket tips from snowmelt: rain on the first days of the year above freezing, or while warm after a cold spell.",
        companion_roles = "{air_temp}",
        emits = "{PSM}",
        argument_schema = schema_json(
          arg("first_warm_days", "integer", 2L, "Days flagged from the first day of the calendar year with air temperature above 0."),
          arg("cold_window", "integer", 720L, "Readings in the rolling mean of air temperature (720 = 30 days of hourly data)."),
          arg("cold_align", "string", "trailing", "trailing = the past cold_window readings, so data are flagged as they arrive; centred = the legacy behaviour (zoo::rollmean default)."),
          arg("qualifier", "string", "PSM")
        )
      )
    )
    for (r in rules) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.qc_rule_capabilities
           (rule_fx, description, companion_roles, emits, argument_schema)
         VALUES ($1, $2, $3::text[], $4::text[], $5::jsonb)",
        params = list(
          r$rule_fx,
          r$description,
          r$companion_roles,
          r$emits,
          r$argument_schema
        )
      )
    }

    # --- Assignments --------------------------------------------------------
    DBI::dbExecute(
      con,
      "CREATE TABLE continuous.timeseries_qc_rules (
         timeseries_qc_rule_id INTEGER PRIMARY KEY
           GENERATED ALWAYS AS IDENTITY
             (SEQUENCE NAME continuous.timeseries_qc_rule_id_seq),
         timeseries_id INTEGER NOT NULL
           REFERENCES continuous.timeseries(timeseries_id) ON DELETE CASCADE,
         rule_fx TEXT NOT NULL
           REFERENCES public.qc_rule_capabilities(rule_fx),
         rule_args JSONB,
         run_order SMALLINT NOT NULL DEFAULT 1,
         apply_from TIMESTAMPTZ,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT timeseries_qc_rules_args_object CHECK (
           rule_args IS NULL OR jsonb_typeof(rule_args) = 'object'
         ),
         CONSTRAINT timeseries_qc_rules_run_order_valid
           CHECK (run_order > 0)
       )"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_qc_rules OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE UNIQUE INDEX timeseries_qc_rules_active_order_key
       ON continuous.timeseries_qc_rules (timeseries_id, run_order)
       WHERE active"
    )
    DBI::dbExecute(
      con,
      "CREATE INDEX timeseries_qc_rules_rule_idx
       ON continuous.timeseries_qc_rules (rule_fx)"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON TABLE continuous.timeseries_qc_rules IS
       'Assigns quality-control rules from public.qc_rule_capabilities to continuous timeseries. gradeContinuous() runs a series'' active rules in run_order after each import. A rule may be assigned to one series more than once with different arguments.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_qc_rules.rule_args IS
       'Arguments overriding the catalogue defaults, as a JSON object. companions: {\"air_temp\": <timeseries_id>} names a companion series explicitly instead of resolving it from the location.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_qc_rules.apply_from IS
       'gradeContinuous() never writes qualifiers before the latest apply_from among a series'' active rules, even when regrading the whole record. Protects flags the rules cannot reproduce, e.g. those loaded from a legacy QC system. NULL = no cutoff.'"
    )

    DBI::dbExecute(
      con,
      "CREATE TRIGGER qc_rule_capabilities_user_modified
       BEFORE UPDATE ON public.qc_rule_capabilities
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER qc_rule_capabilities_update_modified
       BEFORE UPDATE ON public.qc_rule_capabilities
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER timeseries_qc_rules_user_modified
       BEFORE UPDATE ON continuous.timeseries_qc_rules
       FOR EACH ROW EXECUTE FUNCTION public.user_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER timeseries_qc_rules_update_modified
       BEFORE UPDATE ON continuous.timeseries_qc_rules
       FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_qc_rule_capabilities_trigger
       AFTER UPDATE OR DELETE ON public.qc_rule_capabilities
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER audit_timeseries_qc_rules_trigger
       AFTER UPDATE OR DELETE ON continuous.timeseries_qc_rules
       FOR EACH ROW EXECUTE FUNCTION audit.if_modified_func()"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE continuous.timeseries_qc_rules
       ENABLE ROW LEVEL SECURITY"
    )
    DBI::dbExecute(
      con,
      "CREATE POLICY parent_timeseries_visibility
       ON continuous.timeseries_qc_rules
       FOR ALL
       USING (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           WHERE t.timeseries_id = timeseries_qc_rules.timeseries_id
         )
       )
       WITH CHECK (
         EXISTS (
           SELECT 1
           FROM continuous.timeseries t
           WHERE t.timeseries_id = timeseries_qc_rules.timeseries_id
         )
       )"
    )

    DBI::dbExecute(
      con,
      "REVOKE ALL ON TABLE
         public.qc_rule_capabilities,
         continuous.timeseries_qc_rules
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
      c("yg_editor_group", "yg_editor", "continuous_editor"),
      database_roles
    )
    for (role_name in c(reader_roles, editor_roles)) {
      quoted_role <- as.character(DBI::dbQuoteIdentifier(con, role_name))
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT SELECT ON TABLE
             public.qc_rule_capabilities,
             continuous.timeseries_qc_rules
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
          "GRANT INSERT, UPDATE, DELETE ON TABLE
             continuous.timeseries_qc_rules
           TO %s",
          quoted_role
        )
      )
      DBI::dbExecute(
        con,
        sprintf(
          "GRANT USAGE, SELECT, UPDATE ON SEQUENCE
             continuous.timeseries_qc_rule_id_seq
           TO %s",
          quoted_role
        )
      )
    }

    # --- Verify -------------------------------------------------------------
    verified <- DBI::dbGetQuery(
      con,
      "SELECT
         (SELECT count(*) FROM public.qc_rule_capabilities) AS n_rules,
         (SELECT count(*) FROM public.qualifier_types
          WHERE qualifier_type_code IN ('CT', 'SSM', 'PSM', 'UNS', 'UNK',
                                        'ERROR', 'SUS')) AS n_types,
         to_regclass('continuous.timeseries_qc_rules') IS NOT NULL
           AS has_assignments"
    )
    if (
      verified$n_rules != 4 ||
        verified$n_types != 7 ||
        !isTRUE(verified$has_assignments)
    ) {
      stop(
        "QC rules patch verification failed: expected 4 rules, the qualifier types CT, SSM, PSM, UNS, UNK, ERROR and SUS, and continuous.timeseries_qc_rules."
      )
    }

    if (!is.na(patch_number)) {
      DBI::dbExecute(
        con,
        "UPDATE information.version_info SET version = $1
         WHERE item = 'Last patch number'",
        params = list(as.character(patch_number))
      )
      DBI::dbExecute(
        con,
        "UPDATE information.version_info SET version = $1
         WHERE item = 'AquaCache R package used for last patch'",
        params = list(as.character(packageVersion("AquaCache")))
      )
    } else {
      message(
        "patch_number is not set (unmerged draft): information.version_info was not updated."
      )
    }

    DBI::dbExecute(con, "COMMIT")
    active <- FALSE
    message(
      "QC rules patch applied successfully. Assign rules to timeseries in continuous.timeseries_qc_rules; gradeContinuous() applies them after each import."
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
