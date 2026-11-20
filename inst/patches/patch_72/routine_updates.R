# Recreate SQL and PL/pgSQL routines whose textual bodies refer to renamed
# columns. PostgreSQL updates parsed dependencies, but not these bodies.
patch72_replace_routines <- function(con, signatures, replacements) {
  for (signature in signatures) {
    original <- DBI::dbGetQuery(
      con,
      "SELECT pg_get_functiondef(to_regprocedure($1)) AS definition",
      params = list(signature)
    )$definition[[1L]]
    if (is.na(original)) {
      stop("Patch 72 could not find routine ", signature, ".", call. = FALSE)
    }
    updated <- original
    for (needle in names(replacements)) {
      updated <- gsub(needle, replacements[[needle]], updated, perl = TRUE)
    }
    if (!identical(original, updated)) {
      DBI::dbExecute(con, updated)
    }
  }
}

patch72_update_routines <- function(con) {
  update <- function(signatures, replacements) {
    patch72_replace_routines(con, signatures, replacements)
  }
  interval_names <- c(
    "\\bstart_dt\\b" = "start_datetime",
    "\\bend_dt\\b" = "end_datetime"
  )
  update(
    c(
      "audit.if_modified_func()",
      "continuous.apply_corrections(integer,timestamptz,numeric)",
      "continuous.check_special_qualifier_overlap()",
      "continuous.measurements_continuous_corrected(integer,timestamptz,timestamptz,text,integer)",
      "continuous.measurements_continuous_corrected_basic_simple(integer,timestamptz,timestamptz)",
      "continuous.refresh_calculated_daily(integer,date,date)",
      "continuous.resolve_compound_timeseries_raw_window(integer,timestamptz,timestamptz,integer[])",
      "continuous.trim_continuous_timeseries_tail(integer)"
    ),
    interval_names
  )
  update(
    "public.check_timeseries_data_sharing_agreements_overlap()",
    c(
      "public\\.timeseries_data_sharing_agreements" = "continuous.timeseries_data_sharing_agreements",
      interval_names
    )
  )
  update(
    "public.get_csw_layer()",
    c(
      interval_names,
      "\\bdc\\.datum_id_to\\b" = "dc.to_datum_id"
    )
  )

  # Transition relations use the renamed physical fields. Keep the shared
  # temporary range table's columns for the existing measurement triggers.
  update(
    as.vector(outer(
      c("correction", "grade"),
      c("insert", "update", "delete"),
      function(table, event) {
        paste0(
          "continuous.refresh_calculated_daily_",
          table,
          "_",
          event,
          "_trg()"
        )
      }
    )),
    c(
      interval_names,
      "(?m)^(\\s*)start_datetime timestamptz," = "\\1start_dt timestamptz,",
      "(?m)^(\\s*)end_datetime timestamptz$" = "\\1end_dt timestamptz"
    )
  )

  # Keep as-of function output names and read both generations of audit JSON.
  update(
    "audit.continuous_qc_rows_as_of(timestamptz,integer[],timestamptz,timestamptz,text[])",
    c(
      "q\\.start_dt," = "q.start_datetime AS start_dt,",
      "q\\.end_dt," = "q.end_datetime AS end_dt,",
      "g\\.original_data ->> 'start_dt'" = "COALESCE(g.original_data ->> 'start_datetime', g.original_data ->> 'start_dt')",
      "g\\.original_data ->> 'end_dt'" = "COALESCE(g.original_data ->> 'end_datetime', g.original_data ->> 'end_dt')"
    )
  )
  update(
    "audit.corrections_as_of(timestamptz,integer[])",
    c(
      "r\\.start_dt," = "COALESCE(r.start_datetime, r.start_dt),",
      "r\\.end_dt," = "COALESCE(r.end_datetime, r.end_dt),",
      "start_dt TIMESTAMPTZ," = "start_dt TIMESTAMPTZ, start_datetime TIMESTAMPTZ,",
      "end_dt TIMESTAMPTZ," = "end_dt TIMESTAMPTZ, end_datetime TIMESTAMPTZ,"
    )
  )

  update(
    c(
      "criteria.applicable_guideline_rules_for_result(integer,date,boolean,boolean)",
      "criteria.get_sample_hardness(integer,integer[],integer)",
      "criteria.guideline_get_input_value(integer,integer)",
      "discrete.convert_result_aggregation_to_direct(integer,text,numeric,integer,numeric,text)",
      "discrete.refresh_result_aggregations(integer[])"
    ),
    c(
      "\\br\\.result_condition\\b" = "r.result_condition_id",
      "\\br\\.result_type\\b" = "r.result_type_id",
      "\\bp\\.units_liquid\\b" = "p.liquid_unit_id",
      "\\bresult_condition = (v_condition|NULL)" = "result_condition_id = \\1"
    )
  )
  update(
    "discrete.calculate_result_aggregation(integer)",
    c(
      "\\brc\\.result_condition\\b" = "rc.result_condition_id"
    )
  )
  update(
    c(
      "discrete.refresh_result_aggregation(integer)",
      "discrete.validate_result_aggregation()"
    ),
    c(
      "result_condition = NULL" = "result_condition_id = NULL",
      "result_condition IS NOT NULL" = "result_condition_id IS NOT NULL",
      "SELECT result, result_condition," = "SELECT result, result_condition_id,"
    )
  )
  update(
    "discrete.enforce_field_results_without_laboratory()",
    c(
      "\\bNEW\\.result_type\\b" = "NEW.result_type_id",
      "\\bNEW\\.laboratory\\b" = "NEW.lab_id"
    )
  )
  update(
    "discrete.enforce_result_speciation()",
    c(
      "\\b(NEW|OLD)\\.protocol_method\\b" = "\\1.protocol_method_id"
    )
  )
  update(
    "discrete.enforce_sample_context_requirements()",
    c(
      "\\bNEW\\.sample_type\\b" = "NEW.sample_type_id"
    )
  )
  update(
    c(
      "discrete.enforce_sample_group_membership()",
      "discrete.enforce_sample_type_context_requirements()"
    ),
    c(
      "\\bs\\.sample_type\\b" = "s.sample_type_id"
    )
  )
  update(
    "files.check_data_sharing_agreement()",
    c(
      "\\bd\\.type\\b" = "d.document_type_id"
    )
  )
  update(
    "files.check_default_data_sharing_agreement()",
    c(
      "\\bdt\\.type\\b" = "dt.document_type_en"
    )
  )

  units <- patch72_column_names[["public.parameters"]]
  unit_references <- setNames(
    paste0("\\1.", unname(units)),
    paste0("\\b(p|parameter|NEW)\\.", names(units), "\\b")
  )
  update(
    c(
      "public.get_parameter_unit_id(integer,integer)",
      "public.get_unique_parameter_matrix_state_id(integer)"
    ),
    unit_references
  )
  update(
    c(
      "public.begin_parameter_unit_change(integer,jsonb,jsonb,jsonb)",
      "public.prevent_used_parameter_unit_update()"
    ),
    setNames(unname(units), paste0("\\b", names(units), "\\b"))
  )
}
