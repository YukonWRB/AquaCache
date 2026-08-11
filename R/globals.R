#globals for downloadAquarius
utils::globalVariables(c("timeseries")) #This is not fixable otherwise because it comes from a sourced file which lives in /inst

# data.table and foreach non-standard evaluation used by transmission imports.
utils::globalVariables(c(
  ".",
  "datetime",
  "i",
  "is_missing_source",
  "last_query_until",
  "max_days",
  "overlap_minutes",
  "platform_identifier",
  "query_since",
  "query_until",
  "raw_value",
  "route_config_list",
  "source_field",
  "start_datetime_setup",
  "timeseries_id",
  "transmission_mapping_id",
  "transmission_order",
  "transmission_route_id",
  "value",
  "value_multiplier",
  "value_offset"
))
