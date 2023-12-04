load_all()
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("54198"),
                            parameter = c("dly max air temp", "dly min air temp", "dly mean air temp", "dly tot precip", "dly tot rain", "dly tot snow", "hly tot precip", "air temp"),
                            unit = c("C", "C", "C", "mm", "mm", "cm", "mm", "C"),
                            category = "continuous",
                            period_type = c("max", "min", "(min+max)/2", "sum", "sum", "sum", "sum", "instantaneous"),
                            param_type = "meteorological",
                            start_datetime = "2023-11-01",
                            operator = "ECCC",
                            network = "ECCC met",
                            public = TRUE,
                            source_fx = "getRealtimeECCCwx",
                            source_fx_args = c("{interval = 'day'}", "{interval = 'day'}", "{interval = 'day'}", "{interval = 'day'}", "{interval = 'day'}", "{interval = 'day'}", "{interval = 'hour'}", "{interval = 'hour'}"),
                            note = "Current measurement location is Dawson airport, but timeseries includes measurements taken in town and at other locations near the airport. Historical measurements adjusted using overlap to match current location")

locations_df <- data.frame(location = c("54198"),
                           name = c("Watson Lake"),
                           latitude = c(60.12),
                           longitude = c(-128.82),
                           datum_id_from = c(10),
                           datum_id_to = c(110),
                           conversion_m = c(687.3),
                           current = c(TRUE),
                           note = "Timeseries associated with this location may be compound timeseries from multiple locations in the same region. Refer to the timeseries-specific note for details if applicable.")
addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = locations_df)

# Deleting from various tables
# locs <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE ")
DBI::dbExecute(con, "DELETE FROM timeseries WHERE timeseries_id IN ()")
DBI::dbExecute(con, "DELETE FROM measurements_continuous WHERE timeseries_id IN (385,386,387,388,389,390,391,392)")
DBI::dbExecute(con, "DELETE FROM calculated_daily WHERE timeseries_id IN (385,386,387,388,389,390,391,392)")
# DBI::dbExecute(con, "DELETE FROM locations WHERE location = '30MA005'")
# DBI::dbExecute(con, "DELETE FROM datum_conversions WHERE location = '30MA005'")



# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")
