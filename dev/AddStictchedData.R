plot_ts <- function(tables){

  # Run for loop over all tables in list
  for (t in 1:length(tables)){
    tab <- tables[[t]]
    # Recognize date or datetime as date
    colnames(tab)[grep('date', colnames(tab), ignore.case = TRUE)] <- 'date'
    tab$date <- as.Date(tab$date)
    # Remove NAs
    tab <- tab[!is.na(tab$value),]
    # Set as time series object
    tab <- xts::as.xts(tab[, c("date", "value")])
    # Bind tables together
    if (t>1) {
      table <- cbind(table, tab, by="date")
    } else {table <- tab}
  }

  plot <- dygraphs::dygraph(table)
  return(plot)

}


#### ---------------------- Add timeseries ---------------------------------####
con <- hydrometConnect()

# Adding timeseries
timeseries_df <- data.frame(location = c("8964"),
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
                      start=1951,
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
                      start=1924,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####

#### Ross River

weathercan::stations_search(
  coords = c(61.9791, -132.4485),
  dist = 15,
  interval = "day"
)

test <- chooseWeather(location=NULL,
                      coords=c(61.9791, -132.4485),
                      dist=15,
                      interval="day",
                      months=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12),
                      start=1961,
                      end=2023,
                      variable=c("mean_temp", "total_precip"),
                      return_data=TRUE)####


####------------------- Get locations data -----------------------------####

# Stitch data --> timeseries_id | datetime | value

#### Whitehorse
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Whitehorse/"

whitehorse <- YGwater::combineWeather(stations = list("2101310", "2101300", "2101400", "2101415", "2101303"), start='1940-01-01', end='2023-11-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

## Fix up Whitehorse temp for csv
whitehorse_temp <- whitehorse[whitehorse$variable=="mean_temp",]
# Remove empty dates at start
whitehorse_temp <- whitehorse_temp[whitehorse_temp$date >= "1942-04-01",]

## Pull in snow bulletin csv
whitehorse_temp_sb <- read.csv(paste0(folder, "whitehorse_temp_sb.csv"))
# Put into long format
whitehorse_temp_sb <- whitehorse_temp_sb %>%
  tidyr::pivot_longer(cols=names(whitehorse_temp_sb[, 2:length(whitehorse_temp_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
whitehorse_temp_sb$date <- paste0(sub("X", "", whitehorse_temp_sb$year), sub("2020", "", whitehorse_temp_sb$date))
whitehorse_temp_sb$date <- as.Date(whitehorse_temp_sb$date, "%Y-%m-%d")
# remove year
whitehorse_temp_sb <- whitehorse_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
plot_ts(whitehorse_temp, whitehorse_temp_sb)

# Combine the two. Where there is overlap, take snow bulletin
whitehorse_temp_all <- dplyr::left_join(whitehorse_temp, whitehorse_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)
# Add other columns
whitehorse_temp_all$timeseries_id <- 484
whitehorse_temp_all$imputed <- FALSE
whitehorse_temp_all$period <- 'P1D'
# remove NAs
whitehorse_temp_all <- whitehorse_temp_all[!is.na(whitehorse_temp_all$value),]
# fix date to datetime
whitehorse_temp_all$datetime <- as.POSIXct(paste(whitehorse_temp_all$date, "12:00:00"),
                                       format = "%Y-%m-%d %H:%M:%S")
whitehorse_temp_all <- whitehorse_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", whitehorse_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(whitehorse_temp_all$datetime), "' WHERE timeseries_id = 484"))



#### Dawson
# Dawson is a combination of data pulled from ECCC and data pulled from the snow bulletin spreadsheets that has been QAQC'd
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Dawson/"

dawson <- YGwater::combineWeather(stations = list("2100LRP", "2100402", "2100407", "2100400", "2101062", "2100398"),
                                start= '1897-01-01',
                                end='2023-11-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

## Fix up Dawson temp for csv
  dawson_temp <- dawson[dawson$variable=="mean_temp",]
  # Remove empty dates at start
  dawson_temp <- dawson_temp[dawson_temp$date >= "1897-07-22",]

## Pull in snow bulletin csv
  dawson_temp_sb <- read.csv(paste0(folder, "Dawson_temp_sb.csv"))
  # Put into wide format
  dawson_temp_sb <- dawson_temp_sb %>%
    tidyr::pivot_longer(cols=names(dawson_temp_sb[, 2:length(dawson_temp_sb)]),
                        names_to='year',
                        values_to = 'value')
  # Fix date
  dawson_temp_sb$date <- paste0(sub("X", "", dawson_temp_sb$year), sub("2023", "", dawson_temp_sb$date))
  dawson_temp_sb$date <- as.Date(dawson_temp_sb$date, "%Y-%m-%d")
  # remove year
  dawson_temp_sb <- dawson_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
  plot_ts(dawson_temp, dawson_temp_sb)

  # Combine the two. Where there is overlap, take snow bulletin
  dawson_temp_all <- dplyr::left_join(dawson_temp, dawson_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
    dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
    dplyr::select(date, value)
  # Add other columns
  dawson_temp_all$timeseries_id <- 492
  dawson_temp_all$imputed <- FALSE
  dawson_temp_all$period <- 'P1D'
  # remove NAs
  dawson_temp_all <- dawson_temp_all[!is.na(dawson_temp_all$value),]
  # fix date to datetime
  dawson_temp_all$datetime <- as.POSIXct(paste(dawson_temp_all$date, "12:00:00"),
                                         format = "%Y-%m-%d %H:%M:%S")
  dawson_temp_all <- dawson_temp_all[, c("value", "timeseries_id", "datetime", "imputed")]
  # Add to db
  con <- hydrometConnect()
  DBI::dbAppendTable(con, "measurements_continuous", dawson_temp_all)

# update timeseries start date
  DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(dawson_temp_all$datetime), "' WHERE timeseries_id = 492"))


#### Faro

folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Faro/"
# Mean_temp
faro <- combineWeather(stations=list("2100518", "2100527", "2100517", "2100516", "2100515"), start='1966-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

## Fix up faro temp for csv
faro_temp <- faro[faro$variable=="mean_temp",]
# Remove empty dates at start
faro_temp <- faro_temp[faro_temp$date >= "1966-04-01",]

## Pull in snow bulletin csv
faro_temp_sb <- read.csv(paste0(folder, "Faro_temp_sb.csv"))
# Put into long format
faro_temp_sb <- faro_temp_sb %>%
  tidyr::pivot_longer(cols=names(faro_temp_sb[, 2:length(faro_temp_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
faro_temp_sb$date <- paste0(sub("X", "", faro_temp_sb$year), sub("2020", "", faro_temp_sb$date))
faro_temp_sb$date <- as.Date(faro_temp_sb$date, "%Y-%m-%d")
# remove year
faro_temp_sb <- faro_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
plot_ts(faro_temp, faro_temp_sb)

# Remove JAN 1992 to Dec 1993 of snow bulletin because it's garbage
faro_temp_sb <- faro_temp_sb[!(format(faro_temp_sb$date,"%Y")==1992 | format(faro_temp_sb$date,"%Y")==1993), ]
# Combine the two. Where there is overlap, take snow bulletin
faro_temp_all <- dplyr::left_join(faro_temp, faro_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)
# Add other columns
faro_temp_all$timeseries_id <- 500
faro_temp_all$imputed <- FALSE
faro_temp_all$period <- 'P1D'
# remove NAs
faro_temp_all <- faro_temp_all[!is.na(faro_temp_all$value),]
# fix date to datetime
faro_temp_all$datetime <- as.POSIXct(paste(faro_temp_all$date, "12:00:00"),
                                           format = "%Y-%m-%d %H:%M:%S")
faro_temp_all <- faro_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", faro_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(faro_temp_all$datetime), "' WHERE timeseries_id = 500"))


#### Watson Lake

watsonlake <- combineWeather(stations=list("2101204", "2101200", "2101201", "2101202", "2101222"), start='1938-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

## Fix up watsonlake temp for csv
watsonlake_temp <- watsonlake[watsonlake$variable=="mean_temp",]
# Remove empty dates at start
watsonlake_temp <- watsonlake_temp[watsonlake_temp$date >= "1938-10-01",]

watsonlake_temp_all <- watsonlake_temp
# Add other columns
watsonlake_temp_all$timeseries_id <- 508
watsonlake_temp_all$imputed <- FALSE
watsonlake_temp_all$period <- 'P1D'
# remove NAs
watsonlake_temp_all <- watsonlake_temp_all[!is.na(watsonlake_temp_all$value),]
# fix date to datetime
watsonlake_temp_all$datetime <- as.POSIXct(paste(watsonlake_temp_all$date, "12:00:00"),
                                     format = "%Y-%m-%d %H:%M:%S")
watsonlake_temp_all <- watsonlake_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# One last chgeck
plot_ts(watsonlake_temp_all)
# Add to db
con <- hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", watsonlake_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(watsonlake_temp_all$datetime), "' WHERE timeseries_id = 508"))


#### Haines Junction

hainesjunction <- combineWeather(stations=list("2100630", "2100631"), start='1944-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

## Fix up hainesjunction temp for csv
# Remove empty dates at start
hainesjunction_temp <- hainesjunction_temp[hainesjunction_temp$date >= "1944-10-07",]

hainesjunction_temp_all <- hainesjunction_temp
# Add other columns
hainesjunction_temp_all$timeseries_id <- 516
hainesjunction_temp_all$imputed <- FALSE
hainesjunction_temp_all$period <- 'P1D'
# remove NAs
hainesjunction_temp_all <- hainesjunction_temp_all[!is.na(hainesjunction_temp_all$value),]
# fix date to datetime
hainesjunction_temp_all$datetime <- as.POSIXct(paste(hainesjunction_temp_all$date, "12:00:00"),
                                           format = "%Y-%m-%d %H:%M:%S")
hainesjunction_temp_all <- hainesjunction_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# One last chgeck
plot_ts(hainesjunction_temp_all)
# Add to db
con <- HydroMetDB::hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", hainesjunction_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(hainesjunction_temp_all$datetime), "' WHERE timeseries_id = 516"))


#### Beaver Creek

beavercreek <- combineWeather(stations=list("2100155", "2100160", "2100161"), start='1968-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

## Fix up beavercreek temp for csv
# Remove empty dates at start
beavercreek_temp <- beavercreek[beavercreek$date >= "1968-11-25",]

beavercreek_temp_all <- beavercreek_temp
# Add other columns
beavercreek_temp_all$timeseries_id <- 524
beavercreek_temp_all$imputed <- FALSE
beavercreek_temp_all$period <- 'P1D'
# remove NAs
beavercreek_temp_all <- beavercreek_temp_all[!is.na(beavercreek_temp_all$value),]
# fix date to datetime
beavercreek_temp_all$datetime <- as.POSIXct(paste(beavercreek_temp_all$date, "12:00:00"),
                                               format = "%Y-%m-%d %H:%M:%S")
beavercreek_temp_all <- beavercreek_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# One last chgeck
plot_ts(beavercreek_temp_all)
# Add to db
con <- HydroMetDB::hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", beavercreek_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(beavercreek_temp_all$datetime), "' WHERE timeseries_id = 524"))


#### Teslin

teslin <- combineWeather(stations=list("2101102", "2101100", "2101099", "2101106"), start='1943-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

## Fix up teslin temp for csv
# Remove empty dates at start
teslin_temp <- teslin[teslin$date >= "1943-10-01",]

# Add other columns
teslin_temp$timeseries_id <- 532
teslin_temp$imputed <- FALSE
teslin_temp$period <- 'P1D'
# remove NAs
teslin_temp <- teslin_temp[!is.na(teslin_temp$value),]
# fix date to datetime
teslin_temp$datetime <- as.POSIXct(paste(teslin_temp$date, "12:00:00"),
                                            format = "%Y-%m-%d %H:%M:%S")
teslin_temp <- teslin_temp[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# One last chgeck
plot_ts(teslin_temp)
# Add to db
con <- HydroMetDB::hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", teslin_temp)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(teslin_temp$datetime), "' WHERE timeseries_id = 532"))



#### Old Crow

folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/OldCrow/"

oldcrow <- YGwater::combineWeather(stations = list("2100805", "2100800", "2100807"), start='1951-01-01', end='2023-12-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

## Fix up oldcrow temp for csv
oldcrow_temp <- oldcrow[oldcrow$variable=="mean_temp",]
# Remove empty dates at start
oldcrow_temp <- oldcrow_temp[oldcrow_temp$date >= "1951-09-18",]

## Pull in snow bulletin csv
oldcrow_temp_sb <- read.csv(paste0(folder, "oldcrow_temp_sb.csv"))
# Put into long format
oldcrow_temp_sb <- oldcrow_temp_sb %>%
  tidyr::pivot_longer(cols=names(oldcrow_temp_sb[, 2:length(oldcrow_temp_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
oldcrow_temp_sb$date <- paste0(sub("X", "", oldcrow_temp_sb$year), sub("2020", "", oldcrow_temp_sb$date))
oldcrow_temp_sb$date <- as.Date(oldcrow_temp_sb$date, "%Y-%m-%d")
# remove year
oldcrow_temp_sb <- oldcrow_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
plot_ts(oldcrow_temp, oldcrow_temp_sb)

# Combine the two. Where there is overlap, take my stitched climate
oldcrow_temp_all <- dplyr::left_join(oldcrow_temp, oldcrow_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df1, value_df2)) %>%
  dplyr::select(date, value)
# Add other columns
oldcrow_temp_all$timeseries_id <- 556
oldcrow_temp_all$imputed <- FALSE
oldcrow_temp_all$period <- 'P1D'
# remove NAs
oldcrow_temp_all <- oldcrow_temp_all[!is.na(oldcrow_temp_all$value),]
# fix date to datetime
oldcrow_temp_all$datetime <- as.POSIXct(paste(oldcrow_temp_all$date, "12:00:00"),
                                        format = "%Y-%m-%d %H:%M:%S")
oldcrow_temp_all <- oldcrow_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Plot last time
plot_ts(oldcrow_temp_all)
# Add to db
con <- HydroMetDB::hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", oldcrow_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(oldcrow_temp_all$datetime), "' WHERE timeseries_id = 556"))


#### Carmacks

folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Carmacks/"

carmacks <- YGwater::combineWeather(stations = list("2100301", "2100300"), start='1963-01-01', end='2023-12-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

## Fix up carmacks temp for csv
carmacks_temp <- carmacks[carmacks$variable=="mean_temp",]
# Remove empty dates at start
carmacks_temp <- carmacks_temp[carmacks_temp$date >= "1963-08-28",]

## Pull in snow bulletin csv
carmacks_temp_sb <- read.csv(paste0(folder, "carmacks_temp_sb.csv"))
# Put into long format
carmacks_temp_sb <- carmacks_temp_sb %>%
  tidyr::pivot_longer(cols=names(carmacks_temp_sb[, 2:length(carmacks_temp_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
carmacks_temp_sb$date <- paste0(sub("X", "", carmacks_temp_sb$year), sub("2020", "", carmacks_temp_sb$date))
carmacks_temp_sb$date <- as.Date(carmacks_temp_sb$date, "%Y-%m-%d")
# remove year
carmacks_temp_sb <- carmacks_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
plot_ts(carmacks_temp, carmacks_temp_sb)

# Combine the two. Where there is overlap, take snow bulletin
carmacks_temp_all <- dplyr::left_join(carmacks_temp, carmacks_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df1, value_df2)) %>%
  dplyr::select(date, value)
# Add other columns
carmacks_temp_all$timeseries_id <- 540
carmacks_temp_all$imputed <- FALSE
carmacks_temp_all$period <- 'P1D'
# remove NAs
carmacks_temp_all <- carmacks_temp_all[!is.na(carmacks_temp_all$value),]
# fix date to datetime
carmacks_temp_all$datetime <- as.POSIXct(paste(carmacks_temp_all$date, "12:00:00"),
                                           format = "%Y-%m-%d %H:%M:%S")
carmacks_temp_all <- carmacks_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", carmacks_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(carmacks_temp_all$datetime), "' WHERE timeseries_id = 540"))


#### Mayo

folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Mayo/"

mayo <- YGwater::combineWeather(stations = list("2100701", "2100702", "2100700"), start='1924-01-01', end='2023-12-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

## Fix up mayo temp for csv
mayo_temp <- mayo[mayo$variable=="mean_temp",]
# Remove empty dates at start
mayo_temp <- mayo_temp[mayo_temp$date >= "1925-01-12",]

## Pull in snow bulletin csv
mayo_temp_sb <- read.csv(paste0(folder, "mayo_temp_sb.csv"))
# Put into long format
mayo_temp_sb <- mayo_temp_sb %>%
  tidyr::pivot_longer(cols=names(mayo_temp_sb[, 2:length(mayo_temp_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
mayo_temp_sb$date <- paste0(sub("X", "", mayo_temp_sb$year), sub("2020", "", mayo_temp_sb$date))
mayo_temp_sb$date <- as.Date(mayo_temp_sb$date, "%Y-%m-%d")
# remove year
mayo_temp_sb <- mayo_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
plot_ts(mayo_temp, mayo_temp_sb)

# Combine the two. Where there is overlap, take snow bulletin
mayo_temp_all <- dplyr::left_join(mayo_temp, mayo_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df1, value_df2)) %>%
  dplyr::select(date, value)
# Add other columns
mayo_temp_all$timeseries_id <- 548
mayo_temp_all$imputed <- FALSE
mayo_temp_all$period <- 'P1D'
# remove NAs
mayo_temp_all <- mayo_temp_all[!is.na(mayo_temp_all$value),]
# fix date to datetime
mayo_temp_all$datetime <- as.POSIXct(paste(mayo_temp_all$date, "12:00:00"),
                                         format = "%Y-%m-%d %H:%M:%S")
mayo_temp_all <- mayo_temp_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", mayo_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(mayo_temp_all$datetime), "' WHERE timeseries_id = 548"))







######--- OTHER ---
# # # Check for hourly data on missing days?
dawson_mean_temp <- dawson[dawson$variable=="mean_temp", ]
for (d in test[is.na(test$value), ]$date) {
  d <- as.Date(d)
  # Subset to row of interest
  rw <- test[test$date == d,]
  # Get station before this empty one
  stn <- test %>%
    dplyr::filter(date <= d, !is.na(value)) %>%
    dplyr::slice_max(order_by = date) %>%
    dplyr::pull(station)

  # Get hourly data for that day
  test2 <- tryCatch({ getWeather(station = stn, start = d, end = d,
                                 tzone = "UTC",
                                 interval = "hour",
                                 save_path = NULL)
  }, error = function(e) {test2 <<- NULL})

  if (is.null(test2)) {
    next
  }

  # Calculate mean from min and max
  avg <- (max(test2$temp) - min(test2$temp)) / 2
  # Replace NA with avg
  test[test$date==d,]$value <- avg
  test[test$date==d,]$station <- stn
}




## Fix up Dawson temp for csv
dawson_temp <- dawson[dawson$variable=="mean_temp",]
# Remove empty dates at start
dawson_temp <- dawson_temp[dawson_temp$date >= "1897-07-22",]
# Create month-day column and year column
dawson_temp$monthday <- format(dawson_temp$date, "%m-%d")
dawson_temp$year <- format(dawson_temp$date, "%Y")
dawson_temp <- dawson_temp[, c("value", "monthday", "year")]
# Transform to wide format
dawson_temp <- dawson_temp %>% tidyr::pivot_wider(names_from = year, values_from = value)
# Order by date
dawson_temp <- dawson_temp[order(dawson_temp$monthday),]
# Write into csv
write.csv(dawson_temp, paste0(folder,"Dawson_temp_auto.csv"))

## Pull in csv
dawson_temp_all <- read.csv(paste0(folder, "Dawson_temp_all.csv"))
# Put into wide format
dawson_temp_all <- dawson_temp_all %>%
  tidyr::pivot_longer(cols=names(dawson_temp_all[, 2:length(dawson_temp_all)]),
                      names_to='year',
                      values_to = 'value')
# Fix date column

precip <- dawson[dawson$variable=="total_precip",]

precip <- precip %>% mutate()
group_by()


dawson <- YGwater::combineWeather(stations = list("2100LRP", "2100402", "2100407", "2100400", "2101062", "2100398"),
                                  start= '1897-01-01',
                                  end='2023-11-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

dawson <- dawson[dawson$date >= "1897-07-22",]

dawson_mean_temp <- dawson[dawson$variable == "mean_temp",]

sum(is.na(dawson[dawson$variable=="mean_temp",]$value))

# Get monthly aggregates of precip since 1980
dawson_agg <- dawson %>%
  dplyr::mutate(yearmonth=format(date, "%Y-%m")) %>%
  dplyr::group_by(yearmonth, variable) %>%
  dplyr::summarise(sum = sum(value))

dawson_agg <- dawson %>%
  dplyr::mutate(date=format(date, "%Y-01-01")) %>%
  dplyr::group_by(date, variable) %>%
  dplyr::summarise(value = mean(value), na.rm=TRUE)

plot_ts(dawson_agg[dawson_agg$variable=="mean_temp",])


combined_data <- combineWeather(stations=list("2100LRP", "2100402"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100407"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100400"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101062"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100398"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

Dawson <- combined_data
