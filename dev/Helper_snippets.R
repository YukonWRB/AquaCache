load_all()
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("15356000"),
                            parameter = c("flow"),
                            unit = c("m3/s"),
                            category = c("continuous"),
                            period_type = "instantaneous",
                            param_type = "hydrometric",
                            start_datetime = "1970-01-01 00:00:00",
                            operator = "USGS",
                            network = "USGS",
                            public = TRUE,
                            param_type = "hydrometric",
                            source_fx = "getRealtimeNWIS",
                            source_fx_args = NA)

locations_df <- data.frame(location = c("15356000"),
                           name = c("Yukon River at Eagle"),
                           latitude = c(64.789168),
                           longitude = c(-141.2000892),
                           datum_id_from = c(10),
                           datum_id_to = c(110),
                           conversion_m = c(259.08),
                           current = c(TRUE))
addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = locations_df)

# Deleting from various tables
DBI::dbExecute(con, "DELETE FROM timeseries WHERE timeseries_id IN (52)")
DBI::dbExecute(con, "DELETE FROM measurements_continuous WHERE timeseries_id IN (45,47)")
DBI::dbExecute(con, "DELETE FROM calculated_daily WHERE timeseries_id IN (54)")
DBI::dbExecute(con, "DELETE FROM locations WHERE location = '30MA005'")
DBI::dbExecute(con, "DELETE FROM datum_conversions WHERE location = '30MA005'")



# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")
