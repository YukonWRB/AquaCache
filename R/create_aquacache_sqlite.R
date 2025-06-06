#' Create a small SQLite copy of the AquaCache database
#'
#' This helper connects to a running AquaCache PostgreSQL database and writes a small subset of records to a SQLite file.  It is intended for package tests or examples where a local database is required but only a few locations are needed.
#'
#' Only the specified locations are kept from the source database.  Continuous data is copied from `continuous.timeseries` and `continuous.measurements_continuous` while discrete data is copied from `discrete.samples` and `discrete.results`.  Lookup tables such as `grade_types`, `approval_types`, `qualifier_types`, `sample_types`, and other discrete metadata tables are also copied in full.  Continuous measurement views (`measurements_*_corrected`) are materialized as regular tables.
#'
#' @param file Path to the SQLite file to create; leave as 'choose' to prompt the user to select a directory to save the file in.
#' @param continuous_locations Character vector of location codes to retain in continuous tables.  Defaults to the first location found in the database when `NULL`.
#' @param discrete_locations Character vector of location codes to retain in discrete tables.  Defaults to `continuous_locations` when `NULL`.
#' @param start_datetime The start datetime for the data to be copied.  If `NULL`, data is copied from the beginning of records to `end_datetime` for the affected locations.
#' @param end_datetime The end datetime for the data to be copied.  If `NULL`, all data is copied from `start_datetime` to the end of records for the affected locations.
#' @param overwrite Logical. Overwrite `file` if it already exists.
#' @param con An open connection to the AquaCache PostgreSQL database.  Use [AquaConnect()] to create this connection.
#'
#' @return The path to the created SQLite file.
#' 
#' @examples
#' \dontrun{
#' pg <- AquaConnect()
#' create_aquacache_sqlite("aquacache_test.sqlite",
#'   continuous_locations = "09EA004",
#'   discrete_locations = "09AA004")
#' DBI::dbDisconnect(pg)
#' }
#' @export

create_aquacache_sqlite <- function(file = "choose",
                                    continuous_locations = NULL,
                                    discrete_locations = NULL,
                                    overwrite = TRUE,
                                    start_datetime = NULL,
                                    end_datetime = NULL,
                                    con = NULL) {
  
  
  if (file == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want this report saved.")
    file <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
    file <- file.path(file, "aquacache.sqlite")
  }
  
  if (is.null(start_datetime)) {
    start_datetime <- "1800-01-01 00:00"
  }
  
  if (is.null(end_datetime)) {
    end_datetime <- Sys.time()
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  if (is.null(continuous_locations)) {
    continuous_locations <- DBI::dbGetQuery(con,
                                            "SELECT DISTINCT(location_id) FROM timeseries ORDER BY location_id LIMIT 2")$location_id
  } 
  if (is.null(discrete_locations)) {
    discrete_locations <- DBI::dbGetQuery(con,
                                          "SELECT DISTINCT(location_id) FROM samples ORDER BY location_id LIMIT 2")$location_id
  }
  
  if (overwrite && file.exists(file)) unlink(file)
  
  sqlite <- DBI::dbConnect(RSQLite::SQLite(), file)
  
  on.exit(DBI::dbDisconnect(sqlite), add = TRUE)
  
  # reference tables that do not depend on location
  ref_tables <- c(
    "parameters",
    "parameter_groups",
    "parameter_sub_groups",
    "parameter_relationships",
    "media_types",
    "grade_types",
    "approval_types",
    "qualifier_types",
    "datum_list",
    "sample_types",
    "collection_methods",
    "sample_fractions",
    "result_speciations",
    "aggregation_types"
  )
  
  for (tbl in ref_tables) {
    if (DBI::dbExistsTable(con, tbl)) {
      data <- DBI::dbReadTable(con, tbl)
      DBI::dbWriteTable(sqlite, sub(".*\\.", "", tbl), data)
    }
  }
  
  locations <- unique(c(continuous_locations, discrete_locations))
  
  locs <- DBI::dbGetQuery(con,
                          sprintf("SELECT * FROM locations WHERE location_id IN (%s)", paste(locations, collapse = ", ")))
  DBI::dbWriteTable(sqlite, "locations", locs)
  
  networks <- DBI::dbReadTable(con, "networks")
  DBI::dbWriteTable(sqlite, "networks", networks)
  
  ln <- DBI::dbGetQuery(con,
                        sprintf("SELECT * FROM locations_networks WHERE location_id IN (%s)",
                                paste(locations, collapse = ",")))
  DBI::dbWriteTable(sqlite, "locations_networks", ln)
  
  # datum conversions (applies to discrete or continuous)
  dc <- DBI::dbGetQuery(con,
                        sprintf("SELECT * FROM datum_conversions WHERE location_id IN (%s)",
                                paste(locations, collapse = ",")))
  DBI::dbWriteTable(sqlite, "datum_conversions", dc)
  
  # Continuous data --------------------------------------------------------
  if (length(continuous_locations) > 0) {
    ts <- DBI::dbGetQuery(con,
                          sprintf("SELECT * FROM timeseries WHERE location_id IN (%s)",
                                  paste(continuous_locations, collapse = ",")))
    if (nrow(ts) > 0) {
      DBI::dbWriteTable(sqlite, "timeseries", ts)
      
      g <- DBI::dbGetQuery(con, paste0("SELECT * FROM grades WHERE timeseries_id IN (",
                                       paste(ts$timeseries_id, collapse = ","), ") AND start_dt >= '", start_datetime, "' OR end_dt <= '", end_datetime, "'"))
      DBI::dbWriteTable(sqlite, "grades", g)
      
      a <- DBI::dbGetQuery(con, paste0("SELECT * FROM approvals WHERE timeseries_id IN (",
                                       paste(ts$timeseries_id, collapse = ","), ") AND start_dt >= '", start_datetime, "' OR end_dt <= '", end_datetime, "'"))
      DBI::dbWriteTable(sqlite, "approvals", a)
      
      q <- DBI::dbGetQuery(con, paste0("SELECT * FROM qualifiers WHERE timeseries_id IN (",
                                       paste(ts$timeseries_id, collapse = ","), ") AND start_dt >= '", start_datetime, "' OR end_dt <= '", end_datetime, "'"))
      DBI::dbWriteTable(sqlite, "qualifiers", q)
      
      
      ts_ids <- paste(ts$timeseries_id, collapse = ",")
      mc <- DBI::dbGetQuery(con,
                            sprintf("SELECT * FROM measurements_continuous WHERE timeseries_id IN (%s) AND datetime >= '%s' AND datetime <= '%s'",
                                    ts_ids, start_datetime, end_datetime))
      DBI::dbWriteTable(sqlite, "measurements_continuous", mc)
      
      md <- DBI::dbGetQuery(con,
                            sprintf("SELECT * FROM measurements_calculated_daily WHERE timeseries_id IN (%s) AND date >= '%s' AND date <= '%s'",
                                    ts_ids, start_datetime, end_datetime))
      DBI::dbWriteTable(sqlite, "measurements_calculated_daily", md)
      
      corr <- DBI::dbGetQuery(con,
                              sprintf("SELECT * FROM corrections WHERE timeseries_id IN (%s)",
                                      ts_ids))
      DBI::dbWriteTable(sqlite, "corrections", corr)
      
      mcc <- DBI::dbGetQuery(con,
                             sprintf("SELECT * FROM measurements_continuous_corrected WHERE timeseries_id IN (%s) AND datetime >= '%s' AND datetime <= '%s'",
                                     ts_ids, start_datetime, end_datetime))
      DBI::dbWriteTable(sqlite, "measurements_continuous_corrected", mcc)
      
      mhc <- DBI::dbGetQuery(con,
                             sprintf("SELECT * FROM measurements_hourly_corrected WHERE timeseries_id IN (%s) AND datetime >= '%s' AND datetime <= '%s'",
                                     ts_ids, start_datetime, end_datetime))
      DBI::dbWriteTable(sqlite, "measurements_hourly_corrected", mhc)
      
      mdc <- DBI::dbGetQuery(con,
                             sprintf("SELECT * FROM measurements_calculated_daily_corrected WHERE timeseries_id IN (%s) AND date >= '%s' AND date <= '%s'",
                                     ts_ids, start_datetime, end_datetime))
      DBI::dbWriteTable(sqlite, "measurements_calculated_daily_corrected", mdc)
    } else {
      warning("No continuous timeseries found for the specified locations.")
    }
  }
  
  # Discrete data ---------------------------------------------------------
  if (length(discrete_locations) > 0) {
    
    ss <- DBI::dbGetQuery(con, sprintf("SELECT * FROM sample_series WHERE location_id IN (%s)",
                                       paste(discrete_locations, collapse = ",")))
    DBI::dbWriteTable(sqlite, "sample_series", ss)
    
    samples <- DBI::dbGetQuery(con,
                               sprintf("SELECT * FROM samples WHERE location_id IN (%s) AND datetime >= '%s' AND datetime <= '%s'",
                                       paste(discrete_locations, collapse = ","), start_datetime, end_datetime))
    if (nrow(samples) > 0) {
      DBI::dbWriteTable(sqlite, "samples", samples)
      sample_ids <- paste(samples$sample_id, collapse = ",")
      results <- DBI::dbGetQuery(con,
                                 sprintf("SELECT * FROM results WHERE sample_id IN (%s)",
                                         sample_ids))
      DBI::dbWriteTable(sqlite, "results", results)
    } else {
      warning("No discrete samples found for the specified locations.")
    }
  }
  return(file)
}
