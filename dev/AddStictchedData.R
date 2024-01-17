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
timeseries_df <- data.frame(location = c("48168"),
                            parameter = c("mly tot precip"),
                            unit = c("mm"),
                            category = "continuous",
                            period_type = c("sum"),
                            param_type = "meteorological",
                            start_datetime = "?",
                            operator = "ECCC",
                            network = "ECCC met",
                            public = TRUE,
                            source_fx = NULL,
                            source_fx_args = c("{interval = 'month'}"),
                            note = "Timeseries includes measurements from multiple stations. Since Oct. 2007, the monthly precip is taken from the QAQC'd snow bulletin data where available.")

addHydrometTimeseries(timeseries_df = timeseries_df, locations_df = NULL)


####------------------- Explore locations data -----------------------------####

##### Whitehorse

test <- chooseWeather(location=NULL,
                      coords=c(60.7197, -135.0523),
                      dist=10,
                      interval="day",
                      start=1900,
                      end=2023,
                      variable=c("total_precip"),
                      return_data=TRUE)

test <- weathercan::stations_search(
  coords = c(60.7197, -135.0523),
  dist = 10,
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


####-------------- Create and import monthly precip data -------------------####
# Daily air temp data is a combination of the data used for the snow bulletin and stitched together stations. When stations are stitched together, a bias is calculated for the overlap of the two.

#### Whitehorse
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Whitehorse/"

whitehorse <- YGwater::combineWeather(stations = list("2101310", "2101300", "2101400", "2101415", "2101303", "2100907", "2101290"), start='1940-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up Whitehorse precip
whitehorse_precip <- whitehorse[whitehorse$variable=="total_precip",]
# Remove empty dates at start and NAs
whitehorse_precip <- whitehorse_precip[whitehorse_precip$date >= "1940-09-20",]
whitehorse_precip <- whitehorse_precip[!(is.na(whitehorse_precip$value)),]
# Calculate mean monthly values
whitehorse_precip$yearmonth <- format(whitehorse_precip$date, "%Y-%m")
whitehorse_precip$year <- format(whitehorse_precip$date, "%Y")
whitehorse_precip <- whitehorse_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date))
# Remove yearmonth with more than 1 day missing?
whitehorse_precip <- whitehorse_precip[format(whitehorse_precip$date, "%m") %in% c("02") & whitehorse_precip$count >= 28 |
                                         format(whitehorse_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & whitehorse_precip$count >= 30 |
                                         format(whitehorse_precip$date, "%m") %in% c("04","06","09","11") & whitehorse_precip$count >= 29, ]

## Pull in snow bulletin csv
whitehorse_precip_sb <- read.csv(paste0(folder, "whitehorse_precip_sb.csv"))
# Put into long format
whitehorse_precip_sb <- whitehorse_precip_sb %>%
  tidyr::pivot_longer(cols=names(whitehorse_precip_sb[, 2:length(whitehorse_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
whitehorse_precip_sb$month <- sprintf("%02d", match(whitehorse_precip_sb$X, month.name))
whitehorse_precip_sb$year <- paste0(sub("X", "", whitehorse_precip_sb$year))
whitehorse_precip_sb$yearmonth <- paste0(whitehorse_precip_sb$year, "-", whitehorse_precip_sb$month)
# Add date
whitehorse_precip_sb$date <- as.Date(paste0(whitehorse_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(whitehorse_precip, whitehorse_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
whitehorse_precip_sb <- whitehorse_precip_sb[whitehorse_precip_sb$date >= "2007-10-01", ]
# Where there is overlap, take snow bulletin
whitehorse_precip_all <- dplyr::left_join(whitehorse_precip, whitehorse_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
whitehorse_precip_all$date <- lubridate::ceiling_date(whitehorse_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
whitehorse_precip_all$timeseries_id <- 663
whitehorse_precip_all$imputed <- FALSE
whitehorse_precip_all$period <- "P1M"

# fix date to datetime
whitehorse_precip_all$datetime <- as.POSIXct(paste(whitehorse_precip_all$date, "23:59:59"),
                                             format = "%Y-%m-%d %H:%M:%S", tz="MST")
whitehorse_precip_all <- whitehorse_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
test <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 663")
DBI::dbSendQuery(con, "DELETE FROM measurements_continuous WHERE timeseries_id = 663")
DBI::dbAppendTable(con, "measurements_continuous", whitehorse_precip_all)


#### Dawson
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Dawson/"

dawson <- YGwater::combineWeather(stations = list("2100LRP", "2100402", "2100400", "2100164", "2101062", "2101070"), start='1897-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up dawson precip
dawson_precip <- dawson[dawson$variable=="total_precip",]
# Remove empty dates at start and NAs
dawson_precip <- dawson_precip[dawson_precip$date >= "1897-09-01",]
dawson_precip <- dawson_precip[!(is.na(dawson_precip$value)),]
# Calculate mean monthly values
dawson_precip$yearmonth <- format(dawson_precip$date, "%Y-%m")
dawson_precip$year <- format(dawson_precip$date, "%Y")
dawson_precip <- dawson_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date, na.rm=TRUE))
# Remove yearmonth with more than 1 day missing?
dawson_precip <- dawson_precip[format(dawson_precip$date, "%m") %in% c("02") & dawson_precip$count >= 28 |
                                 format(dawson_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & dawson_precip$count >= 30 |
                                 format(dawson_precip$date, "%m") %in% c("04","06","09","11") & dawson_precip$count >= 29, ]


## Pull in snow bulletin csv
dawson_precip_sb <- read.csv(paste0(folder, "dawson_precip_sb.csv"))
# Put into long format
dawson_precip_sb <- dawson_precip_sb %>%
  tidyr::pivot_longer(cols=names(dawson_precip_sb[, 2:length(dawson_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
dawson_precip_sb$month <- sprintf("%02d", match(dawson_precip_sb$X, month.name))
dawson_precip_sb$year <- paste0(sub("X", "", dawson_precip_sb$year))
dawson_precip_sb$yearmonth <- paste0(dawson_precip_sb$year, "-", dawson_precip_sb$month)
# Add date
dawson_precip_sb$date <- as.Date(paste0(dawson_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(dawson_precip, dawson_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
dawson_precip_sb <- dawson_precip_sb[dawson_precip_sb$date >= "2007-10-01", ]
# Where there is overlap, take snow bulletin
dawson_precip_all <- dplyr::left_join(dawson_precip, dawson_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
dawson_precip_all$date <- lubridate::ceiling_date(dawson_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
dawson_precip_all$timeseries_id <- 664
dawson_precip_all$imputed <- FALSE
dawson_precip_all$period <- "P1M"
# remove NAs
dawson_precip_all <- dawson_precip_all[!is.na(dawson_precip_all$value),]
# fix date to datetime
dawson_precip_all$datetime <- as.POSIXct(paste(dawson_precip_all$date, "23:59:59"),
                                         format = "%Y-%m-%d %H:%M:%S")
dawson_precip_all <- dawson_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
test <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 664")
DBI::dbSendQuery(con, "DELETE FROM measurements_continuous WHERE timeseries_id = 664")
DBI::dbAppendTable(con, "measurements_continuous", dawson_precip_all)

# update timeseries start date
# DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(dawson_precip_all$datetime), "' WHERE timeseries_id = 484"))


#### Teslin
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Teslin/"

teslin <- YGwater::combineWeather(stations = list("2101102", "2101100", "2101099"), start='1943-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up teslin precip
teslin_precip <- teslin[teslin$variable=="total_precip",]
# Remove empty dates at start and NAs
teslin_precip <- teslin_precip[teslin_precip$date >= "1943-10-01",]
teslin_precip <- teslin_precip[!(is.na(teslin_precip$value)),]
# Calculate mean monthly values
teslin_precip$yearmonth <- format(teslin_precip$date, "%Y-%m")
teslin_precip$year <- format(teslin_precip$date, "%Y")
teslin_precip <- teslin_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date, na.rm=TRUE))
# Remove yearmonth with more than 1 day missing?
teslin_precip <- teslin_precip[format(teslin_precip$date, "%m") %in% c("02") & teslin_precip$count >= 27 |
                                 format(teslin_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & teslin_precip$count >= 29 |
                                 format(teslin_precip$date, "%m") %in% c("04","06","09","11") & teslin_precip$count >= 28, ]


## Pull in snow bulletin csv
teslin_precip_sb <- read.csv(paste0(folder, "teslin_precip_sb.csv"))
# Put into long format
teslin_precip_sb <- teslin_precip_sb %>%
  tidyr::pivot_longer(cols=names(teslin_precip_sb[, 2:length(teslin_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
teslin_precip_sb$month <- sprintf("%02d", match(teslin_precip_sb$X, month.name))
teslin_precip_sb$year <- paste0(sub("X", "", teslin_precip_sb$year))
teslin_precip_sb$yearmonth <- paste0(teslin_precip_sb$year, "-", teslin_precip_sb$month)
# Add date
teslin_precip_sb$date <- as.Date(paste0(teslin_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(teslin_precip, teslin_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
teslin_precip_sb <- teslin_precip_sb[teslin_precip_sb$date >= "2008-10-01", ]
# Where there is overlap, take snow bulletin
teslin_precip_all <- dplyr::left_join(teslin_precip, teslin_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
teslin_precip_all$date <- lubridate::ceiling_date(teslin_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
teslin_precip_all$timeseries_id <- 665
teslin_precip_all$imputed <- FALSE
teslin_precip_all$period <- "P1M"
# remove NAs
teslin_precip_all <- teslin_precip_all[!is.na(teslin_precip_all$value),]
# fix date to datetime
teslin_precip_all$datetime <- as.POSIXct(paste(teslin_precip_all$date, "23:59:59"),
                                         format = "%Y-%m-%d %H:%M:%S")
teslin_precip_all <- teslin_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
test <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 665")
DBI::dbSendQuery(con, "DELETE FROM measurements_continuous WHERE timeseries_id = 665")
DBI::dbAppendTable(con, "measurements_continuous", teslin_precip_all)


#### Carmacks
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Carmacks/"

carmacks <- YGwater::combineWeather(stations = list("2100301", "2100300"), start='1963-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up carmacks precip
carmacks_precip <- carmacks[carmacks$variable=="total_precip",]
# Remove empty dates at start and NAs
carmacks_precip <- carmacks_precip[carmacks_precip$date >= "1963-08-26",]
carmacks_precip <- carmacks_precip[!(is.na(carmacks_precip$value)),]
# Calculate mean monthly values
carmacks_precip$yearmonth <- format(carmacks_precip$date, "%Y-%m")
carmacks_precip$year <- format(carmacks_precip$date, "%Y")
carmacks_precip <- carmacks_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date, na.rm=TRUE))
# Remove yearmonth with more than 1 day missing?
carmacks_precip <- carmacks_precip[format(carmacks_precip$date, "%m") %in% c("02") & carmacks_precip$count >= 27 |
                                     format(carmacks_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & carmacks_precip$count >= 29 |
                                     format(carmacks_precip$date, "%m") %in% c("04","06","09","11") & carmacks_precip$count >= 28, ]


## Pull in snow bulletin csv
carmacks_precip_sb <- read.csv(paste0(folder, "carmacks_precip_sb.csv"))
# Put into long format
carmacks_precip_sb <- carmacks_precip_sb %>%
  tidyr::pivot_longer(cols=names(carmacks_precip_sb[, 2:length(carmacks_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
carmacks_precip_sb$month <- sprintf("%02d", match(carmacks_precip_sb$X, month.name))
carmacks_precip_sb$year <- paste0(sub("X", "", carmacks_precip_sb$year))
carmacks_precip_sb$yearmonth <- paste0(carmacks_precip_sb$year, "-", carmacks_precip_sb$month)
# Add date
carmacks_precip_sb$date <- as.Date(paste0(carmacks_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(carmacks_precip, carmacks_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
carmacks_precip_sb <- carmacks_precip_sb[carmacks_precip_sb$date >= "2012-10-01", ]
# Where there is overlap, take snow bulletin
carmacks_precip_all <- dplyr::left_join(carmacks_precip, carmacks_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
carmacks_precip_all$date <- lubridate::ceiling_date(carmacks_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
carmacks_precip_all$timeseries_id <- 666
carmacks_precip_all$imputed <- FALSE
carmacks_precip_all$period <- "P1M"
# remove NAs
carmacks_precip_all <- carmacks_precip_all[!is.na(carmacks_precip_all$value),]
# fix date to datetime
carmacks_precip_all$datetime <- as.POSIXct(paste(carmacks_precip_all$date, "23:59:59"),
                                           format = "%Y-%m-%d %H:%M:%S")
carmacks_precip_all <- carmacks_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
test <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 666")
#DBI::dbSendQuery(con, "DELETE FROM measurements_continuous WHERE timeseries_id = 666")
DBI::dbAppendTable(con, "measurements_continuous", carmacks_precip_all)


#### Watson Lake
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/WatsonLake/"

watsonlake <- YGwater::combineWeather(stations = list("2101204", "2101201", "2101200", "2101222"), start='1938-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up watsonlake precip
watsonlake_precip <- watsonlake[watsonlake$variable=="total_precip",]
# Remove empty dates at start and NAs
watsonlake_precip <- watsonlake_precip[watsonlake_precip$date >= "1938-10-01",]
watsonlake_precip <- watsonlake_precip[!(is.na(watsonlake_precip$value)),]
# Calculate mean monthly values
watsonlake_precip$yearmonth <- format(watsonlake_precip$date, "%Y-%m")
watsonlake_precip$year <- format(watsonlake_precip$date, "%Y")
watsonlake_precip <- watsonlake_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date, na.rm=TRUE))
# Remove yearmonth with more than 1 day missing?
watsonlake_precip <- watsonlake_precip[format(watsonlake_precip$date, "%m") %in% c("02") & watsonlake_precip$count >= 27 |
                                         format(watsonlake_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & watsonlake_precip$count >= 29 |
                                         format(watsonlake_precip$date, "%m") %in% c("04","06","09","11") & watsonlake_precip$count >= 28, ]


## Pull in snow bulletin csv
watsonlake_precip_sb <- read.csv(paste0(folder, "watsonlake_precip_sb.csv"))
# Put into long format
watsonlake_precip_sb <- watsonlake_precip_sb %>%
  tidyr::pivot_longer(cols=names(watsonlake_precip_sb[, 2:length(watsonlake_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
watsonlake_precip_sb$month <- sprintf("%02d", match(watsonlake_precip_sb$X, month.name))
watsonlake_precip_sb$year <- paste0(sub("X", "", watsonlake_precip_sb$year))
watsonlake_precip_sb$yearmonth <- paste0(watsonlake_precip_sb$year, "-", watsonlake_precip_sb$month)
# Add date
watsonlake_precip_sb$date <- as.Date(paste0(watsonlake_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(watsonlake_precip, watsonlake_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
watsonlake_precip_sb <- watsonlake_precip_sb[watsonlake_precip_sb$date >= "2007-10-01", ]
# Where there is overlap, take snow bulletin
watsonlake_precip_all <- dplyr::left_join(watsonlake_precip, watsonlake_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
watsonlake_precip_all$date <- lubridate::ceiling_date(watsonlake_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
watsonlake_precip_all$timeseries_id <- 667
watsonlake_precip_all$imputed <- FALSE
watsonlake_precip_all$period <- "P1M"
# remove NAs
watsonlake_precip_all <- watsonlake_precip_all[!is.na(watsonlake_precip_all$value),]
# fix date to datetime
watsonlake_precip_all$datetime <- as.POSIXct(paste(watsonlake_precip_all$date, "23:59:59"),
                                             format = "%Y-%m-%d %H:%M:%S")
watsonlake_precip_all <- watsonlake_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
test <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 667")
#DBI::dbSendQuery(con, "DELETE FROM measurements_continuous WHERE timeseries_id = 667")
DBI::dbAppendTable(con, "measurements_continuous", watsonlake_precip_all)


#### Mayo
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/Mayo/"

mayo <- YGwater::combineWeather(stations = list("2100701", "2100700"), start='1924-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up mayo precip
mayo_precip <- mayo[mayo$variable=="total_precip",]
# Remove empty dates at start and NAs
mayo_precip <- mayo_precip[mayo_precip$date >= "1925-06-01",]
mayo_precip <- mayo_precip[!(is.na(mayo_precip$value)),]
# Calculate mean monthly values
mayo_precip$yearmonth <- format(mayo_precip$date, "%Y-%m")
mayo_precip$year <- format(mayo_precip$date, "%Y")
mayo_precip <- mayo_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date, na.rm=TRUE))
# Remove yearmonth with more than 1 day missing?
mayo_precip <- mayo_precip[format(mayo_precip$date, "%m") %in% c("02") & mayo_precip$count >= 27 |
                             format(mayo_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & mayo_precip$count >= 29 |
                             format(mayo_precip$date, "%m") %in% c("04","06","09","11") & mayo_precip$count >= 28, ]


## Pull in snow bulletin csv
mayo_precip_sb <- read.csv(paste0(folder, "mayo_precip_sb.csv"))
# Put into long format
mayo_precip_sb <- mayo_precip_sb %>%
  tidyr::pivot_longer(cols=names(mayo_precip_sb[, 2:length(mayo_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
mayo_precip_sb$month <- sprintf("%02d", match(mayo_precip_sb$X, month.name))
mayo_precip_sb$year <- paste0(sub("X", "", mayo_precip_sb$year))
mayo_precip_sb$yearmonth <- paste0(mayo_precip_sb$year, "-", mayo_precip_sb$month)
# Add date
mayo_precip_sb$date <- as.Date(paste0(mayo_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(mayo_precip, mayo_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
mayo_precip_sb <- mayo_precip_sb[mayo_precip_sb$date >= "2007-10-01", ]
# Where there is overlap, take snow bulletin
mayo_precip_all <- dplyr::left_join(mayo_precip, mayo_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
mayo_precip_all$date <- lubridate::ceiling_date(mayo_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
mayo_precip_all$timeseries_id <- 668
mayo_precip_all$imputed <- FALSE
mayo_precip_all$period <- "P1M"
# remove NAs
mayo_precip_all <- mayo_precip_all[!is.na(mayo_precip_all$value),]
# fix date to datetime
mayo_precip_all$datetime <- as.POSIXct(paste(mayo_precip_all$date, "23:59:59"),
                                       format = "%Y-%m-%d %H:%M:%S")
mayo_precip_all <- mayo_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
test <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 668")
#DBI::dbSendQuery(con, "DELETE FROM measurements_continuous WHERE timeseries_id = 668")
DBI::dbAppendTable(con, "measurements_continuous", mayo_precip_all)


#### Old Crow
folder <- "H:/estewart/SnowBulletin/AddingStitchedClimateData/OldCrow/"

oldcrow <- YGwater::combineWeather(stations = list("2100805", "2100800"), start='1951-01-01', end='2023-11-01', variables=c("total_precip"), months=NULL)

## Fix up oldcrow precip
oldcrow_precip <- oldcrow[oldcrow$variable=="total_precip",]
# Remove empty dates at start and NAs
oldcrow_precip <- oldcrow_precip[oldcrow_precip$date >= "1951-09-16",]
oldcrow_precip <- oldcrow_precip[!(is.na(oldcrow_precip$value)),]
# Calculate mean monthly values
oldcrow_precip$yearmonth <- format(oldcrow_precip$date, "%Y-%m")
oldcrow_precip$year <- format(oldcrow_precip$date, "%Y")
oldcrow_precip <- oldcrow_precip %>%
  dplyr::group_by(yearmonth) %>%
  dplyr::summarise(value = sum(value, na.rm=TRUE),
                   count = dplyr::n(),
                   date = min(date, na.rm=TRUE))
# Remove yearmonth with more than 1 day missing?
oldcrow_precip <- oldcrow_precip[format(oldcrow_precip$date, "%m") %in% c("02") & oldcrow_precip$count >= 26 |
                                   format(oldcrow_precip$date, "%m") %in% c("01","03","05","07","08","10","12") & oldcrow_precip$count >= 27 |
                                   format(oldcrow_precip$date, "%m") %in% c("04","06","09","11") & oldcrow_precip$count >= 27, ]


## Pull in snow bulletin csv
oldcrow_precip_sb <- read.csv(paste0(folder, "oldcrow_precip_sb.csv"))
# Put into long format
oldcrow_precip_sb <- oldcrow_precip_sb %>%
  tidyr::pivot_longer(cols=names(oldcrow_precip_sb[, 2:length(oldcrow_precip_sb)]),
                      names_to='year',
                      values_to = 'value')
# Fix date
oldcrow_precip_sb$month <- sprintf("%02d", match(oldcrow_precip_sb$X, month.name))
oldcrow_precip_sb$year <- paste0(sub("X", "", oldcrow_precip_sb$year))
oldcrow_precip_sb$yearmonth <- paste0(oldcrow_precip_sb$year, "-", oldcrow_precip_sb$month)
# Add date
oldcrow_precip_sb$date <- as.Date(paste0(oldcrow_precip_sb$yearmonth, "-01"), "%Y-%m-%d")

## Compare wsc to snow bulletin mean temps
plot_ts(list(oldcrow_precip, oldcrow_precip_sb))

# Combine the two.
# Only keep snow bulletin Oct 2007 onwards
oldcrow_precip_sb <- oldcrow_precip_sb[oldcrow_precip_sb$date >= "2016-10-01", ]
# Where there is overlap, take snow bulletin
oldcrow_precip_all <- dplyr::left_join(oldcrow_precip, oldcrow_precip_sb, by = "date", suffix = c("_df1", "_df2")) %>%
  dplyr::mutate(value = dplyr::coalesce(value_df2, value_df1)) %>%
  dplyr::select(date, value)

# Change date to last day, minute, second of month
oldcrow_precip_all$date <- lubridate::ceiling_date(oldcrow_precip_all$date + lubridate::month(1), "month") - lubridate::days(1)

# Add other columns
oldcrow_precip_all$timeseries_id <- 671
oldcrow_precip_all$imputed <- FALSE
oldcrow_precip_all$period <- "P1M"
# remove NAs
oldcrow_precip_all <- oldcrow_precip_all[!is.na(oldcrow_precip_all$value),]
# fix date to datetime
oldcrow_precip_all$datetime <- as.POSIXct(paste(oldcrow_precip_all$date, "23:59:59"),
                                          format = "%Y-%m-%d %H:%M:%S")
oldcrow_precip_all <- oldcrow_precip_all[, c("value", "timeseries_id", "datetime", "imputed", "period")]
# Add to db
con <- HydroMetDB::hydrometConnect()
DBI::dbAppendTable(con, "measurements_continuous", oldcrow_precip_all)



####------------- Create and import daily mean air temp data ---------------####

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
