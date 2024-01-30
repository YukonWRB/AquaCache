load_all()
con <- hydrometConnect()
locations <- DBI::dbGetQuery(con, "SELECT location FROM locations WHERE operator = 'WSC'")[,1]

# Adding timeseries
timeseries_df <- data.frame(location = locations,
                            parameter = "air temp",
                            unit = "°C",
                            category = "continuous",
                            period_type = c("instantaneous"),
                            param_type = "surface water physical",
                            record_rate = "< 1 day",
                            start_datetime = "1980-01-01",
                            operator = "WSC",
                            network = "Canada Yukon Hydrometric Network",
                            public = TRUE,
                            public_delay = NA,
                            source_fx = "downloadWSC",
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




# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")

