#' Create a small SQLite copy of the AquaCache database
#'
#' This helper connects to a running AquaCache PostgreSQL database and writes a small subset of records to a SQLite file.  It is intended for package tests or examples where a local database is required but only a few locations are needed.
#'
#' Only the specified locations are kept from the source database.  Continuous data is copied from `continuous.timeseries` and `continuous.measurements_continuous` while discrete data is copied from `discrete.samples` and `discrete.results`.  Lookup tables such as `grade_types`, `approval_types`, `qualifier_types`, `sample_types`, and other discrete metadata tables are also copied in full.  Continuous measurement views (`measurements_*_corrected`) are materialized as regular tables.
#'
#' @param con An open connection to the AquaCache PostgreSQL database.  Use [AquaConnect()] to create this connection.
#' @param file Path to the SQLite file to create.
#' @param continuous_locations Character vector of location codes to retain in continuous tables.  Defaults to the first location found in the database when `NULL`.
#' @param discrete_locations Character vector of location codes to retain in discrete tables.  Defaults to `continuous_locations` when `NULL`.
#' @param overwrite Logical. Overwrite `file` if it already exists.
#'
#' @return The path to the created SQLite file.
#' 
#' @examples
#' \dontrun{
#' pg <- AquaConnect()
#' create_aquacache_sqlite(pg, "aquacache_test.sqlite",
#'   continuous_locations = "09EA004",
#'   discrete_locations = "09AA004")
#' DBI::dbDisconnect(pg)
#' }
#' @export

create_aquacache_sqlite <- function(con, file,
                                    continuous_locations = NULL,
                                    discrete_locations = NULL,
                                    overwrite = TRUE) {
  if (!inherits(con, "DBIConnection")) {
    stop("`con` must be a DBI connection, e.g. created with AquaConnect().")
  }
  
  if (is.null(continuous_locations) && is.null(discrete_locations)) {
    locs <- DBI::dbGetQuery(con,
                            "SELECT location FROM public.locations ORDER BY location LIMIT 2")$location
    continuous_locations <- locs
    discrete_locations <- locs
  }
  
  if (is.null(discrete_locations)) {
    discrete_locations <- continuous_locations
  }
  
  if (overwrite && file.exists(file)) unlink(file)
  
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), file)
  
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  
  # reference tables that do not depend on location
  ref_tables <- c(
    "public.parameters",
    "public.parameter_groups",
    "public.parameter_sub_groups",
    "public.parameter_relationships",
    "public.media_types",
    "public.grade_types",
    "public.approval_types",
    "public.qualifier_types",
    "public.datum_list",
    "discrete.sample_types",
    "discrete.collection_methods",
    "discrete.sample_fractions",
    "discrete.result_speciations",
    "continuous.aggregation_types",
    "continuous.grades",
    "continuous.approvals",
    "continuous.qualifiers"
  )
  
  for (tbl in ref_tables) {
    if (DBI::dbExistsTable(con, tbl)) {
      data <- DBI::dbReadTable(con, tbl)
      DBI::dbWriteTable(sqlite, sub(".*\\.", "", tbl), data)
    }
  }
  
  loc_query <- paste(DBI::dbQuoteString(con, unique(c(continuous_locations,
                                                      discrete_locations))),
                     collapse = ",")
  locations <- DBI::dbGetQuery(con,
                               sprintf("SELECT * FROM public.locations WHERE location IN (%s)", loc_query))
  DBI::dbWriteTable(sqlite, "locations", locations)
  
  if (DBI::dbExistsTable(con, "public.networks")) {
    networks <- DBI::dbReadTable(con, "public.networks")
    DBI::dbWriteTable(sqlite, "networks", networks)
  }
  if (DBI::dbExistsTable(con, "public.locations_networks")) {
    ln <- DBI::dbGetQuery(con,
                          sprintf("SELECT * FROM public.locations_networks WHERE location_id IN (%s)",
                                  paste(locations$location_id, collapse = ",")))
    DBI::dbWriteTable(sqlite, "locations_networks", ln)
  }
  
  loc_ids <- paste(locations$location_id, collapse = ",")
  
  # datum conversions (applies to discrete or continuous)
  dc <- DBI::dbGetQuery(con,
                          sprintf("SELECT * FROM public.datum_conversions WHERE location_id IN (%s)",
                                  loc_ids))
  DBI::dbWriteTable(sqlite, "datum_conversions", dc)
  
  # Continuous data --------------------------------------------------------
  if (length(continuous_locations) > 0) {
    ts <- DBI::dbGetQuery(con,
                          sprintf("SELECT * FROM continuous.timeseries WHERE location_id IN (%s)",
                                  loc_ids))
    if (nrow(ts) > 0) {
      DBI::dbWriteTable(sqlite, "timeseries", ts)
      
      ts_ids <- paste(ts$timeseries_id, collapse = ",")
      mc <- DBI::dbGetQuery(con,
                            sprintf("SELECT * FROM continuous.measurements_continuous WHERE timeseries_id IN (%s)",
                                    ts_ids))
      DBI::dbWriteTable(sqlite, "measurements_continuous", mc)
      
      md <- DBI::dbGetQuery(con,
                            sprintf("SELECT * FROM continuous.measurements_calculated_daily WHERE timeseries_id IN (%s)",
                                    ts_ids))
      DBI::dbWriteTable(sqlite, "measurements_calculated_daily", md)
      
      corr <- DBI::dbGetQuery(con,
                              sprintf("SELECT * FROM continuous.corrections WHERE timeseries_id IN (%s)",
                                      ts_ids))
      DBI::dbWriteTable(sqlite, "corrections", corr)
      
      mcc <- DBI::dbGetQuery(con,
                             sprintf("SELECT * FROM continuous.measurements_continuous_corrected WHERE timeseries_id IN (%s)",
                                     ts_ids))
      DBI::dbWriteTable(sqlite, "measurements_continuous_corrected", mcc)
      
      mhc <- DBI::dbGetQuery(con,
                             sprintf("SELECT * FROM continuous.measurements_hourly_corrected WHERE timeseries_id IN (%s)",
                                     ts_ids))
      DBI::dbWriteTable(sqlite, "measurements_hourly_corrected", mhc)
      
      mdc <- DBI::dbGetQuery(con,
                             sprintf("SELECT * FROM continuous.measurements_calculated_daily_corrected WHERE timeseries_id IN (%s)",
                                     ts_ids))
      DBI::dbWriteTable(sqlite, "measurements_calculated_daily_corrected", mdc)
    } else {
      warning("No continuous timeseries found for the specified locations.")
    }
  }
  
  # Discrete data ---------------------------------------------------------
  if (length(discrete_locations) > 0) {
    
    ss <- DBI::dbGetQuery(con, sprintf("SELECT * FROM discrete.sample_series WHERE location_id IN (%s)",
                          loc_ids))
    DBI::dbWriteTable(sqlite, "sample_series", ss)
    
    samples <- DBI::dbGetQuery(con,
                               sprintf("SELECT * FROM discrete.samples WHERE location_id IN (%s)",
                                       loc_ids))
    if (nrow(samples) > 0) {
      DBI::dbWriteTable(sqlite, "samples", samples)
      sample_ids <- paste(samples$sample_id, collapse = ",")
      results <- DBI::dbGetQuery(con,
                                 sprintf("SELECT * FROM discrete.results WHERE sample_id IN (%s)",
                                         sample_ids))
      DBI::dbWriteTable(sqlite, "results", results)
    } else {
      warning("No discrete samples found for the specified locations.")
    }
  }
  
  return(file)
}
