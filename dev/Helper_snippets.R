load_all()
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("15356000", "15348000", "15041200", "15024800", "15129120", "15453500"),
                            parameter = c("flow"),
                            unit = c("m3/s"),
                            category = "continuous",
                            period_type = "instantaneous",
                            param_type = "hydrometric",
                            start_datetime = "1970-01-01 00:00:00",
                            operator = "USGS",
                            network = "USGS",
                            public = TRUE,
                            source_fx = "getRealtimeNWIS",
                            source_fx_args = NA)

locations_df <- data.frame(location = c("15356000", "15348000", "15041200", "15024800", "15129120", "15453500"),
                           name = c("Yukon River at Eagle", "Fortymile River Near Steele Creek", "Taku River Near Juneau", "Stikine River Near Wrangell", "Alsek River at Dry Bay Near Yakutat", "Yukon River Near Stevens Village"),
                           latitude = c(64.789168, 64.3088552, 58.53829828, 56.70772188, 59.1928056, 65.8751013),
                           longitude = c(-141.2000892, -141.4045168, -133.7017432, -132.1319653, -138.3339167, -149.7203487),
                           datum_id_from = c(10),
                           datum_id_to = c(63, 63, 63, 63, 63, 1000),
                           conversion_m = c(259.08, 365.76, 15.24, 7.62, 15.24, 74.31024),
                           current = c(TRUE))
addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = locations_df)

# Deleting from various tables
# locs <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE ")
# DBI::dbExecute(con, "DELETE FROM timeseries WHERE location IN (52)")
DBI::dbExecute(con, "DELETE FROM measurements_continuous WHERE timeseries_id IN (363,364,365,366)")
DBI::dbExecute(con, "DELETE FROM calculated_daily WHERE timeseries_id IN (363,364,365,366)")
# DBI::dbExecute(con, "DELETE FROM locations WHERE location = '30MA005'")
# DBI::dbExecute(con, "DELETE FROM datum_conversions WHERE location = '30MA005'")



# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")
