
####- Remove from calculated_daily, measurements_continuous and timeseries -####

con <- HydroMetDB::hydrometConnect()

for (i in 385:481){
  DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id  = ", i, ";"))
  DBI::dbExecute(con, paste0("DELETE FROM calculated_daily WHERE timeseries_id  = ", i, ";"))
  DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id  = ", i, ";"))
}

# *timeseries_id are changed manually in locations table.

#### ---------------------- Add timeseries ---------------------------------####
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("26894"),
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
                            note = "Timeseries includes measurements from multiple  locations. Historical measurements adjusted using overlap to match current location")

addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = NULL)


####------------------- Explore locations data -----------------------------####

##### Whitehorse

test <- chooseWeather(location=NULL,
                      coords=c(60.7197, -135.0523),
                      dist=10,
                      interval="day",
                      start=1950,
                      end=2023,
                      variable=c("mean_temp"),
                      return_data=TRUE)

test <- weathercan::stations_search(
  coords = c(60.7197, -135.0523),
  dist = 5,
  interval = "day"
)


#### Dawson

test <- weathercan::stations_search(
  coords = c(64.0639, -139.4333),
  dist = 20,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(64.0639, -139.4333),
                      dist=16,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1897,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)


#### Faro

test <- chooseWeather(location=NULL,
                      coords=c(62.2316, -133.3561),
                      dist=10,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1897,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)

weathercan::stations_search(
  coords = c(62.2316, -133.3561),
  dist = 15,
  interval = "day"
)

#### Watson Lake

weathercan::stations_search(
  coords = c(60.0630, -128.7162),
  dist = 15,
  interval = "day"
)


test <- chooseWeather(location=NULL,
                      coords=c(60.0630, -128.7162),
                      dist=10,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1970,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####


#### Haines Junction

weathercan::stations_search(
  coords = c(60.7525, -137.5102),
  dist = 15,
  interval = "day"
)


test <- chooseWeather(location=NULL,
                      coords=c(60.7525, -137.5102),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1944,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####


#### Beaver Creek

weathercan::stations_search(
  coords = c(62.3838, -140.8753),
  dist = 15,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(62.3838, -140.8753),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1944,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####


#### Teslin

weathercan::stations_search(
  coords = c(60.1660, -132.7247),
  dist = 15,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(60.1660, -132.7247),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1944,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####


#### Carmacks

test <- weathercan::stations_search(
  coords = c(62.0877, -136.2920),
  dist = 15,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(62.0877, -136.2920),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1963,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####


combined_data <- combineWeather(stations=list("", ""), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, ""), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

#### Mayo

weathercan::stations_search(
  coords = c(63.5942, -135.8966),
  dist = 15,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(63.5942, -135.8966),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1944,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####

combined_data <- combineWeather(stations=list("", ""), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, ""), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

#### Old Crow

weathercan::stations_search(
  coords = c(67.5696, -139.8288),
  dist = 15,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(67.5696, -139.8288),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1944,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####

combined_data <- combineWeather(stations=list("", ""), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, ""), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)



####------------------- Get locations data -----------------------------####

# Stitch data --> timeseries_id | datetime | value

# Need function that will:
# - Take a list of dataframes and their associated timseries_id
# - append these to the measurements_continuous table.
add_measurements_continuous <- function(combined_data, timeseries_ids) {
  for (t in 1:length(timeseries_id)) {
    # pull dataframne from list of dataframes
    tab <- combined_data[[t]]
    # Add timeseries column
    tab$timeseries_id <- timeseries_id[t]
    # Set date as datetime
    # Rename columns
    # Add to db
  }
}

#### Whitehorse

combined_data <- combineWeather(stations=list("2101310", "2101300"), start='1940-01-01', end='2023-12-03', variables=c("mean_temp", "total_precip", "total_rain", "total_snow", "min_temp", "max_temp"), months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101400"), start='1940-01-01', end='2023-12-03', variables=c("mean_temp", "total_precip", "total_rain", "total_snow", "min_temp", "max_temp"), months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101415"), start='1940-01-01', end='2023-12-03', variables=c("mean_temp", "total_precip", "total_rain", "total_snow", "min_temp", "max_temp"), months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101303"), start='1940-01-01', end='2023-12-03', variables=c("mean_temp", "total_precip", "total_rain", "total_snow", "min_temp", "max_temp"), months=NULL)

Whitehorse <- combined_data
# Remove until 1942-03-31
Whitehorse <- Whitehorse[Whitehorse$date >= "1942-04-01",]
# Add timeseries_id
Whitehorse$timeseries_id <-



#### Dawson

combined_data <- combineWeather(stations=list("2100LRP", "2100402"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100407"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100400"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101062"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100398"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

Dawson <- combined_data

#### Faro

# Mean_temp
combined_data <- combineWeather(stations=list("2100518", "2100527"), start='1970-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100517"), start='1970-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100516"), start='1970-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

Faro <- combined_data

# Total_precip is spotty

#### Watson Lake

combined_data <- combineWeather(stations=list("2101204", "2101200"), start='1938-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101201"), start='1938-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101202"), start='1938-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101222"), start='1938-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

#### Haines Junction

combined_data <- combineWeather(stations=list("2100630", "2100631"), start='1944-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

HainesJct <- combined_data

#### Beaver Creek

combined_data <- combineWeather(stations=list("2100155", "2100160"), start='1968-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100161"), start='1968-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

BeaverCreek <- combined_data

#### Teslin

combined_data <- combineWeather(stations=list("2101102", "2101100"), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101099"), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101106"), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

Teslin <- combined_data
