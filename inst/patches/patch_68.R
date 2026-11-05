# Patch 68 adds post-import quality-control rules for continuous timeseries:
# a rule catalogue (public.qc_rule_capabilities) and per-timeseries rule
# assignments (continuous.timeseries_qc_rules), mirroring the source-adapter
# pair from patch 56. gradeContinuous() applies saved assignments and writes
# continuous.qualifiers; it never creates assignments during imports. Also
# adds the qualifier types the rules emit (CT, SSM, PSM) where missing and
# offers an explicit choice of rules to assign and backfill during this patch.
#
# Patch 68 follows patch 67, which adds parameter unit migration and revised
# continuous extrema tables. AquaConnect() applies patches in numeric order.
patch_number <- 68L

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
      "Patch ",
      patch_number,
      " must be applied to a database at Patch ",
      patch_number - 1L,
      "."
    )
  }
}

if (interactive()) {
  message(
    "Choose which QC rules to assign automatically. Enter 0 for none, or a comma-separated selection from 1 to 4:\n",
    "0 = do not create any automatic assignments (they can  be created later)\n",
    "1 = qc_rain_cold (rain below the registered air-temperature threshold)\n",
    "2 = qc_stuck_sensor (applies to all active continuous parameters. Beware that this might not be advisable for many timeseries, such as precipitation, that routinely report identical values (no precip) for a long time. We recommend applying this per-timeseries or per-parameter later)\n",
    "3 = qc_dir_calm (wind direction during calm wind)\n",
    "4 = qc_snowmelt_tips (possible snowmelt tipping-bucket readings)\n\n",
    "For example, enter 1,3,4 to select those three rules. No rule arguments are requested: selected assignments use populateTimeseriesQCRules()' automatic policy defaults (including its recording-rate-based snowmelt window). Assignments apply to the full timeseries record (-Inf to Inf). This patch will backfill all currently available readings; future imports will use the saved assignments."
  )
  selection <- trimws(readline(
    prompt = "Enter 0 or one or more rule numbers (1,2,3,4): "
  ))
  selected_numbers <- suppressWarnings(as.integer(trimws(
    strsplit(selection, ",", fixed = TRUE)[[1L]]
  )))
  if (
    !length(selected_numbers) ||
      anyNA(selected_numbers) ||
      any(!selected_numbers %in% 0:4) ||
      (0L %in% selected_numbers && length(selected_numbers) != 1L)
  ) {
    stop(
      "Invalid QC rule selection. Enter 0 alone, or a comma-separated selection of 1, 2, 3 and/or 4."
    )
  }
  selected_numbers <- unique(selected_numbers)
} else {
  selected_numbers <- 0
}

active <- dbTransBegin(con)
tryCatch(
  {
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
         rule_fx TEXT NOT NULL,
         rule_version INTEGER NOT NULL DEFAULT 1,
         description TEXT NOT NULL,
         emits TEXT[] NOT NULL,
         argument_schema JSONB NOT NULL DEFAULT
           '{\"schema_version\":2,\"arguments\":[],\"companions\":[]}'::jsonb,
         enabled BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
         modified TIMESTAMPTZ,
         CONSTRAINT qc_rule_capabilities_pkey
           PRIMARY KEY (rule_fx, rule_version),
         CONSTRAINT qc_rule_capabilities_fx_not_blank
           CHECK (btrim(rule_fx) <> ''),
         CONSTRAINT qc_rule_capabilities_version_valid
           CHECK (rule_version > 0),
         CONSTRAINT qc_rule_capabilities_emits_not_empty
           CHECK (cardinality(emits) > 0),
         CONSTRAINT qc_rule_capabilities_argument_schema_object CHECK (
           jsonb_typeof(argument_schema) = 'object'
           AND argument_schema @> '{\"schema_version\":2}'::jsonb
           AND jsonb_typeof(argument_schema -> 'arguments') = 'array'
           AND jsonb_typeof(argument_schema -> 'companions') = 'array'
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
      "COMMENT ON COLUMN public.qc_rule_capabilities.rule_version IS
       'Immutable implementation and default-argument version. Publish changed grading logic or defaults under a new version so existing assignments keep their pinned semantics.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.qc_rule_capabilities.emits IS
       'Qualifier type codes the rule can write. Inside its graded window, gradeContinuous() may replace unprotected qualifiers of exactly these codes, its baseline (UNS), and UNK.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN public.qc_rule_capabilities.argument_schema IS
       'Versioned argument and companion metadata for schema-driven AquaLink forms. Argument descriptors follow the source-adapter control contract. Defaults are copied to assignments when saved.'"
    )

    arg <- function(
      name,
      source,
      label,
      help,
      default,
      value_type = NULL,
      control = NULL,
      required = FALSE,
      choices = NULL,
      minimum = NULL,
      maximum = NULL,
      step = NULL,
      advanced = FALSE,
      units = NULL,
      visible_when = NULL
    ) {
      out <- list(
        name = name,
        source = source,
        help = help,
        default = default,
        advanced = advanced
      )
      if (identical(source, "user")) {
        out$label <- label
        out$value_type <- value_type
        out$control <- control
        out$required <- required
      }
      optional <- list(
        choices = choices,
        minimum = minimum,
        maximum = maximum,
        step = step,
        units = units,
        visible_when = visible_when
      )
      out <- c(out, optional[!vapply(optional, is.null, logical(1))])
      out
    }
    companion <- function(role, label, parameter_name, help, when = NULL) {
      out <- list(
        role = role,
        label = label,
        parameter_name = parameter_name,
        selection_strategy = "same_location_lowest_sensor_priority",
        allow_explicit_override = TRUE,
        missing_behavior = "warn_and_use_na",
        help = help
      )
      if (!is.null(when)) {
        out$when <- when
      }
      out
    }
    schema_json <- function(arguments, companions = list()) {
      as.character(jsonlite::toJSON(
        list(
          schema_version = 2L,
          arguments = arguments,
          companions = companions
        ),
        auto_unbox = TRUE,
        null = "null",
        digits = NA
      ))
    }
    rules <- list(
      list(
        rule_fx = "qc_rain_cold",
        rule_version = 1L,
        description = "Rain recorded while air temperature is below a threshold.",
        emits = "{CT}",
        argument_schema = schema_json(
          list(
            arg(
              "max_air_temp",
              "user",
              "Cold threshold",
              "Rain above 0 is flagged when air temperature is below this threshold.",
              -0.1,
              value_type = "numeric",
              control = "numeric",
              required = TRUE,
              step = 0.1,
              units = "\u00b0C"
            ),
            arg(
              "qualifier",
              "internal",
              "Output qualifier",
              "Qualifier written when the cold-rain rule matches.",
              "CT"
            )
          ),
          list(companion(
            "air_temp",
            "Air temperature",
            "temperature, air",
            "Automatically uses the lowest-priority-number air-temperature series at this location; an explicit series may be selected."
          ))
        )
      ),
      list(
        rule_fx = "qc_stuck_sensor",
        rule_version = 1L,
        description = "Rolling standard deviation below a minimum: a stuck or frozen sensor. Optionally splits stuck zero readings by air temperature (wind speed).",
        emits = "{ERROR,SUS}",
        argument_schema = schema_json(
          list(
            arg(
              "width",
              "user",
              "Window width",
              "Readings in the centred rolling window.",
              5L,
              value_type = "integer",
              control = "numeric",
              required = TRUE,
              minimum = 2,
              step = 1,
              units = "readings"
            ),
            arg(
              "min_sd",
              "user",
              "Maximum standard deviation",
              "A rolling standard deviation below this value is treated as a stuck sensor.",
              0.001,
              value_type = "numeric",
              control = "numeric",
              required = TRUE,
              minimum = 0,
              step = 0.001
            ),
            arg(
              "qualifier",
              "internal",
              "Stuck qualifier",
              "Qualifier written for a non-zero flatline.",
              "ERROR"
            ),
            arg(
              "zero_split",
              "user",
              "Split stuck zero readings",
              "When enabled, use air temperature to distinguish cold stuck zeros from warm calm conditions.",
              FALSE,
              value_type = "logical",
              control = "checkbox",
              required = TRUE
            ),
            arg(
              "zero_max_air_temp",
              "user",
              "Cold threshold for zero readings",
              "A stuck zero below this air temperature is classified as SUS.",
              0,
              value_type = "numeric",
              control = "numeric",
              required = TRUE,
              step = 0.1,
              units = "\u00b0C",
              visible_when = list(argument = "zero_split", equals = TRUE)
            ),
            arg(
              "zero_qualifier",
              "internal",
              "Cold-zero qualifier",
              "Qualifier written for a stuck zero below the air-temperature threshold.",
              "SUS"
            )
          ),
          list(companion(
            "air_temp",
            "Air temperature",
            "temperature, air",
            "Used only when zero-splitting is enabled; if unavailable, the rule treats temperature as missing.",
            when = list(argument = "zero_split", equals = TRUE)
          ))
        )
      ),
      list(
        rule_fx = "qc_dir_calm",
        rule_version = 1L,
        description = "Wind direction recorded while wind speed is at or below a threshold.",
        emits = "{ERROR}",
        argument_schema = schema_json(
          list(
            arg(
              "max_wind_speed",
              "user",
              "Calm wind threshold",
              "Wind direction is flagged when wind speed is at or below this threshold.",
              0,
              value_type = "numeric",
              control = "numeric",
              required = TRUE,
              minimum = 0,
              step = 0.1,
              units = "m/s"
            ),
            arg(
              "qualifier",
              "internal",
              "Output qualifier",
              "Qualifier written when wind direction is recorded during calm conditions.",
              "ERROR"
            )
          ),
          list(companion(
            "wind_speed",
            "Wind speed",
            "velocity, wind",
            "Automatically uses the lowest-priority-number wind-speed series at this location; an explicit series may be selected."
          ))
        )
      ),
      list(
        rule_fx = "qc_snowmelt_tips",
        rule_version = 1L,
        description = "Possible false tipping-bucket tips from snowmelt: rain on the first days of the year above freezing, or while warm after a cold spell.",
        emits = "{PSM}",
        argument_schema = schema_json(
          list(
            arg(
              "first_warm_days",
              "user",
              "Early warm-period length",
              "Number of days from the first above-freezing day of the local calendar year during which precipitation is flagged.",
              2L,
              value_type = "integer",
              control = "numeric",
              required = TRUE,
              minimum = 0,
              step = 1,
              units = "days"
            ),
            arg(
              "cold_window",
              "user",
              "Cold-period window",
              "Number of readings used to calculate the rolling air-temperature mean.",
              720L,
              value_type = "integer",
              control = "numeric",
              required = TRUE,
              minimum = 1,
              step = 1,
              units = "readings"
            ),
            arg(
              "cold_align",
              "user",
              "Window alignment",
              "Trailing applies as data arrive; centred reproduces the legacy look-ahead behavior.",
              "trailing",
              value_type = "character",
              control = "select",
              required = TRUE,
              choices = c("trailing", "centred")
            ),
            arg(
              "qualifier",
              "internal",
              "Output qualifier",
              "Qualifier written when the snowmelt rule matches.",
              "PSM"
            )
          ),
          list(companion(
            "air_temp",
            "Air temperature",
            "temperature, air",
            "Used for the local warm-period and preceding-cold-period checks."
          ))
        )
      )
    )
    for (r in rules) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.qc_rule_capabilities
           (rule_fx, rule_version, description, emits, argument_schema)
         VALUES ($1, $2, $3, $4::text[], $5::jsonb)",
        params = list(
          r$rule_fx,
          r$rule_version,
          r$description,
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
         rule_fx TEXT NOT NULL,
         rule_version INTEGER NOT NULL DEFAULT 1,
         rule_args JSONB NOT NULL,
         companion_timeseries JSONB NOT NULL DEFAULT '{}'::jsonb,
         run_order SMALLINT NOT NULL DEFAULT 1,
         active BOOLEAN NOT NULL DEFAULT TRUE,
         note TEXT,
         created_by TEXT DEFAULT CURRENT_USER NOT NULL,
         modified_by TEXT,
         created TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
         modified TIMESTAMPTZ,
         CONSTRAINT timeseries_qc_rules_capability_fkey
           FOREIGN KEY (rule_fx, rule_version)
             REFERENCES public.qc_rule_capabilities(rule_fx, rule_version),
         CONSTRAINT timeseries_qc_rules_args_object CHECK (
           jsonb_typeof(rule_args) = 'object'
         ),
         CONSTRAINT timeseries_qc_rules_companions_object CHECK (
           jsonb_typeof(companion_timeseries) = 'object'
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
      "COMMENT ON COLUMN continuous.timeseries_qc_rules.rule_version IS
       'Pins the assignment to the implementation/default version selected from public.qc_rule_capabilities.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_qc_rules.rule_args IS
       'Effective argument values including defaults, materialized when the assignment is saved.'"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN continuous.timeseries_qc_rules.companion_timeseries IS
       'Explicit companion timeseries_id overrides by role; absent roles use the catalogue automatic same-location resolution strategy.'"
    )

    DBI::dbExecute(
      con,
      "CREATE OR REPLACE FUNCTION public.qc_rule_version_immutable()
       RETURNS trigger
       LANGUAGE plpgsql
       AS $qc_rule_version_immutable$
       BEGIN
         IF NEW.rule_fx IS DISTINCT FROM OLD.rule_fx
           OR NEW.rule_version IS DISTINCT FROM OLD.rule_version
           OR NEW.emits IS DISTINCT FROM OLD.emits
           OR NEW.argument_schema IS DISTINCT FROM OLD.argument_schema
         THEN
           RAISE EXCEPTION
             'QC rule semantics are versioned; create a new rule_version instead.'
             USING ERRCODE = 'check_violation';
         END IF;
         RETURN NEW;
       END;
       $qc_rule_version_immutable$"
    )
    DBI::dbExecute(
      con,
      "ALTER FUNCTION public.qc_rule_version_immutable() OWNER TO admin"
    )
    DBI::dbExecute(
      con,
      "CREATE TRIGGER qc_rule_capabilities_version_immutable
       BEFORE UPDATE ON public.qc_rule_capabilities
       FOR EACH ROW EXECUTE FUNCTION public.qc_rule_version_immutable()"
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

    # --- Optional automatic assignments ------------------------------------
    if (identical(selected_numbers, 0L)) {
      message(
        "No QC rules will be automatically assigned. You can create assignments later with populateTimeseriesQCRules() or setTimeseriesQCRules()."
      )
    } else {
      qc_rule_options <- c(
        "qc_rain_cold",
        "qc_stuck_sensor",
        "qc_dir_calm",
        "qc_snowmelt_tips"
      )
      selected_rule_fx <- qc_rule_options[selected_numbers]
      automatic_assignments <- populateTimeseriesQCRules(
        con,
        dry_run = FALSE,
        rule_fx = selected_rule_fx,
        backfill = TRUE
      )
      if (!nrow(automatic_assignments)) {
        message("No eligible timeseries matched the selected rules.")
      } else {
        created <- sum(automatic_assignments$action == "created")
        already_configured <- sum(
          automatic_assignments$action == "already_configured"
        )
        skipped <- sum(
          !automatic_assignments$action %in%
            c(
              "created",
              "already_configured"
            )
        )
        message(
          "QC assignment setup complete: ",
          created,
          " created, ",
          already_configured,
          " already configured, ",
          skipped,
          " skipped (for example, no matching primary or companion series)."
        )
      }
      message(
        "To create or backfill assignments after this patch, call populateTimeseriesQCRules() explicitly."
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
      "QC rules patch applied successfully. QC assignments are never created automatically during imports; use populateTimeseriesQCRules() for explicit bulk assignment and backfill, or setTimeseriesQCRules() for custom assignments. gradeContinuous() applies active saved assignments to new data."
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
