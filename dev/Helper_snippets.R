load_all()
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("YOWN-0101"),
                            parameter = c("iron dissolved", "iron total"),
                            unit = c("mg/l"),
                            category = "discrete",
                            period_type = c("instantaneous"),
                            param_type = "ground water chemical",
                            record_rate = NA,
                            start_datetime = "1980-01-01",
                            operator = "WRB",
                            network = "YOWN",
                            public = TRUE,
                            public_delay = c("P1D", NA),
                            source_fx = "downloadEQWin",
                            source_fx_args = NA,
                            note = NA)

locations_df <- data.frame(location = c("YOWN-0101"),
                           name = c("Wolf Creek Well"),
                           latitude = c(60.606923),
                           longitude = c(-134.962745),
                           datum_id_from = c(10),
                           datum_id_to = c(110),
                           conversion_m = c(749),
                           current = c(TRUE),
                           note = NA)

settings_df <- data.frame(source_fx = "downloadEQWin",
                          parameter = c("iron dissolved", "iron total"),
                          period_type = "instantaneous",
                          record_rate = NA,
                          remote_param_name = c("F-D", "F-T"))

addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = locations_df, settings_df = settings_df)

# Deleting from various tables
# locs <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE ")
# DBI::dbExecute(con, "DELETE FROM timeseries WHERE timeseries_id IN ()")
# DBI::dbExecute(con, "DELETE FROM measurements_continuous WHERE timeseries_id IN (385,386,387,388,389,390,391,392)")
# DBI::dbExecute(con, "DELETE FROM calculated_daily WHERE timeseries_id IN (385,386,387,388,389,390,391,392)")
#



# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")
