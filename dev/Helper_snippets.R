load_all()
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("15356000"),
                            parameter = c("flow"),
                            unit = c("m3/s"),
                            category = c("continuous"),
                            type = "instantaneous",
                            start_datetime = "1970-01-01 00:00:00",
                            operator = "USGS",
                            network = "USGS",
                            public = TRUE,
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
add_timeseries(timeseries_df = timeseries_df, locations_df = locations_df)

# Deleting from various tables
DBI::dbExecute(con, "DELETE FROM timeseries WHERE timeseries_id IN (52)")
DBI::dbExecute(con, "DELETE FROM measurements_continuous WHERE timeseries_id IN (45,47)")
DBI::dbExecute(con, "DELETE FROM calculated_daily WHERE timeseries_id IN (54)")
DBI::dbExecute(con, "DELETE FROM locations WHERE location = '30MA005'")
DBI::dbExecute(con, "DELETE FROM datum_conversions WHERE location = '30MA005'")



# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")


#Porting existing realtime WSC data from old sqlite DB to new postgres DB
#1. Find the unique location,parameter pairs. Add these to the DB *without* pulling in new data.
#2. Add the realtime data. Ensure column names are ok, and that dt column befores posixct!
#3. Calculate stats for all newly added locations.
#4. Add drainage basins for WSC stations
#5. Auto-calculate drainage basins for other stations (requires HRDEM or CDEM + adapted code)
