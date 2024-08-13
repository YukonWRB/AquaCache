load_all()
con <- AquaConnect()

# Adding timeseries
timeseries_df <- data.frame(location = "09DC007",
                            parameter = c("water level", "discharge, river/stream"),
                            category = "continuous",
                            period_type = c("instantaneous"),
                            media_type = "surface water",
                            record_rate = "< 1 day",
                            start_datetime = "1950-01-01",
                            public = TRUE,
                            public_delay = NA,
                            source_fx = "downloadWSC",
                            source_fx_args = NA,
                            note = NA)

locations_df <- data.frame(location = c("09DC007"),
                           name = c("Mayo River at Highway No. 11"),
                           name_fr = c("Rivière Mayo à la route 11"),
                           latitude = c(63.605556),
                           longitude = c(-135.9),
                           network = "Canada Yukon Hydrometric Network",
                           project = NA,
                           datum_id_from = c(10),
                           datum_id_to = c(10),
                           conversion_m = 0,
                           current = c(TRUE),
                           note = NA, 
                           contact = NA)

settings_df <- data.frame(source_fx = "downloadAquarius",
                          parameter = c("total precip", "air temp"),
                          period_type = "instantaneous",
                          record_rate = "< 1 day",
                          remote_param_name = c("Precip Total.Corrected", "Air Temp.Corrected"))

addACTimeseries(timeseries_df = timeseries_df)




# Check the DB size:
DBI::dbGetQuery(con, "select pg_size_pretty(pg_database_size('AquaCache'));")

