
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

#### Whitehorse
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Whitehorse/"

whitehorse <- YGwater::combineWeather(stations = list("2101310", "2101300", "2101400", "2101415", "2101303"), start='1940-01-01', end='2023-11-01', variables=c("mean_temp", "total_precip", "min_temp", "max_temp"), months=NULL)

## Fix up Whitehorse temp for csv
whitehorse_temp <- whitehorse[whitehorse$variable=="mean_temp",]
# Remove empty dates at start
whitehorse_temp <- whitehorse_temp[whitehorse_temp$date >= "1942-04-01",]

## Pull in snow bulletin csv
whitehorse_temp_sb <- read.csv(paste0(folder, "whitehorse_temp_sb.csv"))
# Put into wide format
whitehorse_temp_sb <- whitehorse_temp_sb %>%
  tidyr::pivot_longer(cols=names(whitehorse_temp_sb[, 2:length(whitehorse_temp_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
whitehorse_temp_sb$date <- paste0(sub("X", "", whitehorse_temp_sb$year), sub("2023", "", whitehorse_temp_sb$date))
whitehorse_temp_sb$date <- as.Date(whitehorse_temp_sb$date, "%Y-%m-%d")
# remove year
whitehorse_temp_sb <- whitehorse_temp_sb[, c("date", "value")]

## Compare wsc to snow bulletin mean temps
plot_ts <- function(table1, table2=NULL){
  if (is.null(table2)) {
    table1$date <- as.Date(table1$date)
    table1 <- table1[!is.na(table1$value),]
    table1 <- xts::as.xts(table1[, c("date", "value")])
    plot <- dygraphs::dygraph(table1)
  } else {
    table1$date <- as.Date(table1$date)
    table1 <- table1[!is.na(table1$value),]
    table1 <- xts::as.xts(table1[, c("date", "value")])
    table2$date <- as.Date(table2$date)
    table2 <- table2[!is.na(table2$value),]
    table2 <- xts::as.xts(table2[, c("date", "value")])
    table <- cbind(table1, table2, by="date")
    plot <- dygraphs::dygraph(table)
  }
  return(plot)
}

plot_ts(whitehorse_temp, whitehorse_temp_sb)

# Combine the two. Where there is overlap, take snow bulletin
whitehorse_temp_all <- dplyr::left_join(whitehorse_temp, whitehorse_temp_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)
# Add other columns
whitehorse_temp_all$timeseries_id <- 0000
whitehorse_temp_all$imputed <- FALSE
whitehorse_temp_all$period <- 'P1D'
# remove NAs
whitehorse_temp_all <- whitehorse_temp_all[!is.na(whitehorse_temp_all$value),]
# fix date to datetime
whitehorse_temp_all$datetime <- as.POSIXct(paste(whitehorse_temp_all$date, "12:00:00"),
                                       format = "%Y-%m-%d %H:%M:%S")
whitehorse_temp_all <- whitehorse_temp_all[, c("value", "timeseries_id", "datetime", "imputed")]
# Add to db
con <- hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", whitehorse_temp_all)

# update timeseries start date
DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(whitehorse_temp_all$datetime), "' WHERE timeseries_id = 492"))



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
  plot_ts <- function(table1, table2=NULL){
    if (is.null(table2)) {
      table1$date <- as.Date(table1$date)
      table1 <- table1[!is.na(table1$value),]
      table1 <- xts::as.xts(table1[, c("date", "value")])
      plot <- dygraphs::dygraph(table1)
    } else {
      table1$date <- as.Date(table1$date)
      table1 <- table1[!is.na(table1$value),]
      table1 <- xts::as.xts(table1[, c("date", "value")])
      table2$date <- as.Date(table2$date)
      table2 <- table2[!is.na(table2$value),]
      table2 <- xts::as.xts(table2[, c("date", "value")])
      table <- cbind(table1, table2, by="date")
      plot <- dygraphs::dygraph(table)
    }
    return(plot)
  }

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

plot_ts <- function(table){
  table$date <- as.Date(table$date)
  table <- xts::as.xts(table[, c("date", "value")])
  plot <- dygraphs::dygraph(table)
  return(plot)
}

plot_ts(dawson_agg[dawson_agg$variable=="mean_temp",])


combined_data <- combineWeather(stations=list("2100LRP", "2100402"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100407"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100400"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2101062"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)
combined_data <- combineWeather(stations=list(combined_data, "2100398"), start='1897-01-01', end='2023-12-03', variable="mean_temp", months=NULL)

Dawson <- combined_data
