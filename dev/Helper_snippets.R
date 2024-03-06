load_all()
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = "09AA-M3",
                            parameter = c("total precip", "air temp"),
                            unit = c("mm", "°C"),
                            category = "continuous",
                            period_type = c("instantaneous"),
                            param_type = "meteorological",
                            record_rate = "< 1 day",
                            start_datetime = "1980-01-01",
                            public = TRUE,
                            public_delay = NA,
                            source_fx = "downloadAquarius",
                            source_fx_args = NA,
                            note = NA)

locations_df <- data.frame(location = c("09AA-M3"),
                           name = c("Montana Mountain Meteorological"),
                           latitude = c(60.133850),
                           longitude = c(-134.719450),
                           network = "Snow Survey Network",
                           project = NA,
                           datum_id_from = c(10),
                           datum_id_to = c(110),
                           conversion_m = c(1026),
                           current = c(TRUE),
                           note = NA, 
                           contact = NA)

settings_df <- data.frame(source_fx = "downloadAquarius",
                          parameter = c("total precip", "air temp"),
                          period_type = "instantaneous",
                          record_rate = "< 1 day",
                          remote_param_name = c("Precip Total.Corrected", "Air Temp.Corrected"))

addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = locations_df)




# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('hydromet'));")

