# The names are part of the migration, independent of the package's runtime
# aliases. Leave earlier migrations and public view output names intact.
patch72_column_names <- list(
  "boreholes.boreholes" = c(
    comissioned_by = "commissioned_by",
    drilled_by = "driller_id",
    drill_method = "drill_method_id"
  ),
  "boreholes.boreholes_no_coords" = c(comissioned_by = "commissioned_by"),
  "boreholes.wells" = c(casing_material = "casing_material_id"),
  "files.documents" = c(type = "document_type_id"),
  "files.images" = c(image_type = "image_type_id"),
  "instruments.calibrations" = c(
    id_sensor_holder = "sensor_holder_instrument_id",
    id_handheld_meter = "handheld_meter_instrument_id",
    observer = "observer_id"
  ),
  "instruments.instruments" = c(
    make = "instrument_make_id",
    model = "instrument_model_id",
    type = "instrument_type_id"
  ),
  "public.locations_metadata_instruments" = c(
    record_units = "record_unit_id",
    record_statistic = "record_aggregation_type_id"
  ),
  "public.locations_metadata_instrument_connection_signals" = c(
    signal_units = "signal_unit_id"
  ),
  "public.datum_conversions" = c(
    datum_id_from = "from_datum_id",
    datum_id_to = "to_datum_id"
  ),
  "public.parameters" = c(
    units_liquid = "liquid_unit_id",
    units_solid = "solid_unit_id",
    units_gas = "gas_unit_id",
    units_na = "not_applicable_unit_id"
  ),
  "discrete.samples" = c(
    z = "depth_height_m",
    comissioning_org = "commissioning_org_id",
    sampling_org = "sampling_org_id",
    wave_hgt_m = "wave_height_m",
    collection_method = "collection_method_id",
    sample_type = "sample_type_id",
    sample_grade = "grade_type_id",
    sample_approval = "approval_type_id"
  ),
  "discrete.results" = c(
    result_type = "result_type_id",
    result_condition = "result_condition_id",
    result_value_type = "result_value_type_id",
    laboratory = "lab_id",
    protocol_method = "protocol_method_id"
  ),
  "discrete.result_components" = c(result_condition = "result_condition_id"),
  "discrete.import_parameter_mappings" = c(
    result_type = "result_type_id",
    result_value_type = "result_value_type_id"
  ),
  "discrete.protocols_methods" = c(protocol_id = "protocol_method_id")
)
for (table in c(
  "approvals",
  "contributors",
  "corrections",
  "grades",
  "notes",
  "owners",
  "qualifiers",
  "timeseries_data_sharing_agreements"
)) {
  patch72_column_names[[paste0("continuous.", table)]] <- c(
    start_dt = "start_datetime",
    end_dt = "end_datetime"
  )
}

patch72_rename_columns <- function(con) {
  for (table in names(patch72_column_names)) {
    parts <- strsplit(table, ".", fixed = TRUE)[[1L]]
    id <- DBI::Id(schema = parts[[1L]], table = parts[[2L]])
    map <- patch72_column_names[[table]]
    quoted_table <- DBI::dbQuoteIdentifier(con, id)
    for (old in names(map)) {
      DBI::dbExecute(
        con,
        sprintf(
          "ALTER TABLE %s RENAME COLUMN %s TO %s",
          quoted_table,
          DBI::dbQuoteIdentifier(con, old),
          DBI::dbQuoteIdentifier(con, map[[old]])
        )
      )
    }
  }
}

patch72_rename_import_profile_fields <- function(con) {
  fields <- c(
    sample_identity = "discrete.samples",
    result_identity = "discrete.results"
  )
  for (column in names(fields)) {
    replacements <- patch72_column_names[[fields[[column]]]]
    DBI::dbExecute(
      con,
      sprintf(
        "UPDATE discrete.import_profiles p
         SET %1$s = updated.identity
         FROM (
           SELECT import_profile_id,
                  jsonb_agg(COALESCE($1::jsonb -> value, to_jsonb(value))
                            ORDER BY position) AS identity
           FROM discrete.import_profiles
           CROSS JOIN LATERAL jsonb_array_elements_text(%1$s)
             WITH ORDINALITY AS fields(value, position)
           GROUP BY import_profile_id
         ) updated
         WHERE updated.import_profile_id = p.import_profile_id
           AND p.%1$s IS DISTINCT FROM updated.identity",
        DBI::dbQuoteIdentifier(con, column)
      ),
      params = list(jsonlite::toJSON(as.list(replacements), auto_unbox = TRUE))
    )
  }
  fields <- c(
    "collection_method",
    "sample_type",
    "laboratory",
    "result_type",
    "result_value_type",
    "protocol_method"
  )
  defaults <- c(
    patch72_column_names[["discrete.samples"]],
    patch72_column_names[["discrete.results"]]
  )[fields]
  for (old in names(defaults)) {
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_profiles
       SET defaults = (defaults - $1::text) ||
         CASE WHEN defaults ? $2::text THEN '{}'::jsonb
           ELSE jsonb_build_object($2::text, defaults -> $1::text)
         END
       WHERE defaults ? $1::text",
      params = list(old, defaults[[old]])
    )
  }
}
