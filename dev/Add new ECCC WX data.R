stns <- as.data.frame(weathercan::stations())
unique(stns$prov)

stns <- stns[
  stns$prov %in% c("AB", "BC", "YT", "NT") & stns$lat > 59 & stns$lon < -125,
]

stns <- stns[!is.na(stns$start) & !is.na(stns$end), ]
stns <- stns[stns$end >= 2024, ]
View(stns)

names <- c(
  "Pelly Ranch Fort Selkirk",
  "Rock River",
  "Otter Falls NCPC",
  "Margaret Lake",
  "Komakuk Beach",
  "Ivvavik Nat. Park",
  "Herschel Island",
  "Burwash Airport"
)

# Pelly Ranch Fort Selkirk
# Current fetch is 54601, dailies, hourlies. Historical record is 1586, dailies
# No overlap between records
old <- weathercan::weather_dl(
  station_ids = 1586,
  interval = "day",
  time_disp = "UTC"
)
new <- weathercan::weather_dl(
  station_ids = 54601,
  interval = "day",
  time_disp = "UTC"
)
data <- rbind(old, new)
data <- data[order(data$date), ]

data <- data[, c(
  "date",
  "qual",
  "mean_temp",
  "min_temp",
  "max_temp",
  "snow_grnd",
  "total_precip",
  "total_rain",
  "total_snow"
)]
data$datetime <- as.POSIXct(data$date, tz = "UTC")
# Drop cols with all NA
data <- data[, colSums(is.na(data)) < nrow(data)]

addACTimeseries(
  con = con,
  df = NULL,
  location = "54601",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 54601, parameter: mean_temp, interval: day",
    "location: 54601, parameter: min_temp, interval: day",
    "location: 54601, parameter: max_temp, interval: day",
    "location: 54601, parameter: snow_grnd, interval: day",
    "location: 54601, parameter: total_precip, interval: day",
    "location: 54601, parameter: total_rain, interval: day",
    "location: 54601, parameter: total_snow, interval: day"
  ),
  data = list(
    data.frame(datetime = data$datetime, value = data$mean_temp),
    data.frame(datetime = data$datetime, value = data$min_temp),
    data.frame(datetime = data$datetime, value = data$max_temp),
    data.frame(datetime = data$datetime, value = data$snow_grnd),
    data.frame(datetime = data$datetime, value = data$total_precip),
    data.frame(datetime = data$datetime, value = data$total_rain),
    data.frame(datetime = data$datetime, value = data$total_snow)
  )
)

# Hourly data
addACTimeseries(
  con = con,
  location = "54601",
  parameter = c(1250, 34, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "sum",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 54601, parameter: temp, interval: hour",
    "location: 54601, parameter: precip_amt, interval: hour",
    "location: 54601, parameter: wind_spd, interval: hour",
    "location: 54601, parameter: wind_dir, interval: hour"
  )
)

# Rock River
# Current fetch is 8979, dailies, hourlies. No overlap
# Try to add all timeseries direct, they'll fail if the data is not there
# Daily data first
addACTimeseries(
  con = con,
  location = "8979",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 8979, parameter: mean_temp, interval: day",
    "location: 8979, parameter: min_temp, interval: day",
    "location: 8979, parameter: max_temp, interval: day",
    "location: 8979, parameter: snow_grnd, interval: day",
    "location: 8979, parameter: total_precip, interval: day",
    "location: 8979, parameter: total_rain, interval: day",
    "location: 8979, parameter: total_snow, interval: day"
  )
)
# Hourly data
addACTimeseries(
  con = con,
  location = "8979",
  parameter = c(1250, 34, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "sum",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 8979, parameter: temp, interval: hour",
    "location: 8979, parameter: precip_amt, interval: hour",
    "location: 8979, parameter: wind_spd, interval: hour",
    "location: 8979, parameter: wind_dir, interval: hour"
  )
)

# Otter Falls NCPC
# 1583, dailies only
addACTimeseries(
  con = con,
  location = "1583",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 1583, parameter: mean_temp, interval: day",
    "location: 1583, parameter: min_temp, interval: day",
    "location: 1583, parameter: max_temp, interval: day",
    "location: 1583, parameter: snow_grnd, interval: day",
    "location: 1583, parameter: total_precip, interval: day",
    "location: 1583, parameter: total_rain, interval: day",
    "location: 1583, parameter: total_snow, interval: day"
  )
)

# Margaret Lake
# 27296, dailies, hourlies
addACTimeseries(
  con = con,
  location = "27296",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 27296, parameter: mean_temp, interval: day",
    "location: 27296, parameter: min_temp, interval: day",
    "location: 27296, parameter: max_temp, interval: day",
    "location: 27296, parameter: snow_grnd, interval: day",
    "location: 27296, parameter: total_precip, interval: day",
    "location: 27296, parameter: total_rain, interval: day",
    "location: 27296, parameter: total_snow, interval: day"
  )
)
# Hourly data
addACTimeseries(
  con = con,
  location = "27296",
  parameter = c(1250, 34, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "sum",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 27296, parameter: temp, interval: hour",
    "location: 27296, parameter: precip_amt, interval: hour",
    "location: 27296, parameter: wind_spd, interval: hour",
    "location: 27296, parameter: wind_dir, interval: hour"
  )
)

# Komakuk Beach
# Current fetch is 10822, historic fetch is 1568. dailies/hourlies. No overlap.
old <- weathercan::weather_dl(
  station_ids = 1568,
  interval = "day",
  time_disp = "UTC"
)
new <- weathercan::weather_dl(
  station_ids = 10822,
  interval = "day",
  time_disp = "UTC"
)
data <- rbind(old, new)
data <- data[order(data$date), ]

data <- data[, c(
  "date",
  "mean_temp",
  "min_temp",
  "max_temp",
  "snow_grnd",
  "total_precip",
  "total_rain",
  "total_snow"
)]
data$datetime <- as.POSIXct(data$date, tz = "UTC")
# Drop cols with all NA
data <- data[, colSums(is.na(data)) < nrow(data)]

addACTimeseries(
  con = con,
  df = NULL,
  location = "10822",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 10822, parameter: mean_temp, interval: day",
    "location: 10822, parameter: min_temp, interval: day",
    "location: 10822, parameter: max_temp, interval: day",
    "location: 10822, parameter: snow_grnd, interval: day",
    "location: 10822, parameter: total_precip, interval: day",
    "location: 10822, parameter: total_rain, interval: day",
    "location: 10822, parameter: total_snow, interval: day"
  ),
  data = list(
    data.frame(datetime = data$datetime, value = data$mean_temp),
    data.frame(datetime = data$datetime, value = data$min_temp),
    data.frame(datetime = data$datetime, value = data$max_temp),
    data.frame(datetime = data$datetime, value = data$snow_grnd),
    data.frame(datetime = data$datetime, value = data$total_precip),
    data.frame(datetime = data$datetime, value = data$total_rain),
    data.frame(datetime = data$datetime, value = data$total_snow)
  )
)

# Hourly data
old <- weathercan::weather_dl(
  station_ids = 1568,
  interval = "hour",
  time_disp = "UTC"
)
new <- weathercan::weather_dl(
  station_ids = 10822,
  interval = "hour",
  time_disp = "UTC"
)
data <- rbind(old, new)
data <- data[order(data$time), ]

data <- data[, c(
  "time",
  "precip_amt",
  "temp",
  "wind_spd",
  "wind_dir"
)]
data$datetime <- data$time
# Drop cols with all NA
data <- data[, colSums(is.na(data)) < nrow(data)]


# Hourly data
addACTimeseries(
  con = con,
  location = "10822",
  parameter = c(1250, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 10822, parameter: temp, interval: hour",
    "location: 10822, parameter: wind_spd, interval: hour",
    "location: 10822, parameter: wind_dir, interval: hour"
  ),
  data = list(
    data.frame(datetime = data$datetime, value = data$temp),
    data.frame(datetime = data$datetime, value = data$wind_spd),
    data.frame(datetime = data$datetime, value = data$wind_dir)
  )
)

# Ivavik Nat. Park
# 26869, dailies, hourlies
addACTimeseries(
  con = con,
  location = "26869",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 26869, parameter: mean_temp, interval: day",
    "location: 26869, parameter: min_temp, interval: day",
    "location: 26869, parameter: max_temp, interval: day",
    "location: 26869, parameter: snow_grnd, interval: day",
    "location: 26869, parameter: total_precip, interval: day",
    "location: 26869, parameter: total_rain, interval: day",
    "location: 26869, parameter: total_snow, interval: day"
  )
)
# Hourly data
addACTimeseries(
  con = con,
  location = "26869",
  parameter = c(1250, 34, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "sum",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 26869, parameter: temp, interval: hour",
    "location: 26869, parameter: precip_amt, interval: hour",
    "location: 26869, parameter: wind_spd, interval: hour",
    "location: 26869, parameter: wind_dir, interval: hour"
  )
)

# Herschel Island
# 1560, dailies, hourlies
# Past dailies at 1559, no overlap
old <- weathercan::weather_dl(
  station_ids = 1559,
  interval = "day",
  time_disp = "UTC"
)
new <- weathercan::weather_dl(
  station_ids = 1560,
  interval = "day",
  time_disp = "UTC"
)
data <- rbind(old, new)
data <- data[order(data$date), ]

data <- data[, c(
  "date",
  "mean_temp",
  "min_temp",
  "max_temp",
  "snow_grnd",
  "total_precip",
  "total_rain",
  "total_snow"
)]
data$datetime <- as.POSIXct(data$date, tz = "UTC")
# Drop cols with all NA
data <- data[, colSums(is.na(data)) < nrow(data)]
names(data)

addACTimeseries(
  con = con,
  location = "1560",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 1560, parameter: mean_temp, interval: day",
    "location: 1560, parameter: min_temp, interval: day",
    "location: 1560, parameter: max_temp, interval: day",
    "location: 1560, parameter: snow_grnd, interval: day",
    "location: 1560, parameter: total_precip, interval: day",
    "location: 1560, parameter: total_rain, interval: day",
    "location: 1560, parameter: total_snow, interval: day"
  ),
  data = list(
    data.frame(datetime = data$datetime, value = data$mean_temp),
    data.frame(datetime = data$datetime, value = data$min_temp),
    data.frame(datetime = data$datetime, value = data$max_temp),
    data.frame(datetime = data$datetime, value = data$snow_grnd),
    data.frame(datetime = data$datetime, value = data$total_precip),
    data.frame(datetime = data$datetime, value = data$total_rain),
    data.frame(datetime = data$datetime, value = data$total_snow)
  )
)
# Hourly data
addACTimeseries(
  con = con,
  location = "1560",
  parameter = c(1250, 34, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "sum",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 1560, parameter: temp, interval: hour",
    "location: 1560, parameter: precip_amt, interval: hour",
    "location: 1560, parameter: wind_spd, interval: hour",
    "location: 1560, parameter: wind_dir, interval: hour"
  )
)

# Burwash Airport
# Current fetch is 49650, historic fetch is 1525. Dailies, hourlies.
# Stitched data from 1525 and 49650 with overlap
old <- weathercan::weather_dl(
  station_ids = 1525,
  interval = "day",
  time_disp = "UTC"
)
new <- weathercan::weather_dl(
  station_ids = 49650,
  interval = "day",
  time_disp = "UTC"
)
# For each parameter, calculate the average offset during the overlapping period and adjust old data accordingly
overlap_old <- old[old$date > min(new$date), c("date", "mean_temp")]
overlap_new <- new[new$date %in% overlap_old$date, c("date", "mean_temp")]
mean_temp_overlap <- mean(
  overlap_new$mean_temp - overlap_old$mean_temp,
  na.rm = TRUE
)

overlap_old <- old[old$date > min(new$date), c("date", "min_temp")]
overlap_new <- new[new$date %in% overlap_old$date, c("date", "min_temp")]
min_temp_overlap <- mean(
  overlap_new$min_temp - overlap_old$min_temp,
  na.rm = TRUE
)

overlap_old <- old[old$date > min(new$date), c("date", "max_temp")]
overlap_new <- new[new$date %in% overlap_old$date, c("date", "max_temp")]
max_temp_overlap <- mean(
  overlap_new$max_temp - overlap_old$max_temp,
  na.rm = TRUE
)

old <- old[old$date < min(new$date), ]
old$mean_temp <- old$mean_temp + mean_temp_overlap
old$min_temp <- old$min_temp + min_temp_overlap
old$max_temp <- old$max_temp + max_temp_overlap

data <- rbind(old, new)

data <- data[order(data$date), ]

data <- data[, c(
  "date",
  "mean_temp",
  "min_temp",
  "max_temp",
  "snow_grnd",
  "total_precip",
  "total_rain",
  "total_snow"
)]
data$datetime <- as.POSIXct(data$date, tz = "UTC")
# Drop cols with all NA
data <- data[, colSums(is.na(data)) < nrow(data)]
names(data)

addACTimeseries(
  con = con,
  location = "49650",
  parameter = c(1250, 1250, 1250, 1220, 34, 35, 1221),
  media = 7,
  aggregation_type = c(
    "(min+max)/2",
    "minimum",
    "maximum",
    "instantaneous",
    "sum",
    "sum",
    "sum"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 day",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 49650, parameter: mean_temp, interval: day",
    "location: 49650, parameter: min_temp, interval: day",
    "location: 49650, parameter: max_temp, interval: day",
    "location: 49650, parameter: snow_grnd, interval: day",
    "location: 49650, parameter: total_precip, interval: day",
    "location: 49650, parameter: total_rain, interval: day",
    "location: 49650, parameter: total_snow, interval: day"
  ),
  data = list(
    data.frame(datetime = data$datetime, value = data$mean_temp),
    data.frame(datetime = data$datetime, value = data$min_temp),
    data.frame(datetime = data$datetime, value = data$max_temp),
    data.frame(datetime = data$datetime, value = data$snow_grnd),
    data.frame(datetime = data$datetime, value = data$total_precip),
    data.frame(datetime = data$datetime, value = data$total_rain),
    data.frame(datetime = data$datetime, value = data$total_snow)
  )
)

# Hourly data
old <- weathercan::weather_dl(
  station_ids = 1525,
  interval = "hour",
  time_disp = "UTC"
)
new <- weathercan::weather_dl(
  station_ids = 49650,
  interval = "hour",
  time_disp = "UTC"
)

overlap_old <- old[old$time > min(new$time), c("time", "temp")]
overlap_new <- new[new$time %in% overlap_old$time, c("time", "temp")]
temp_overlap <- mean(
  overlap_new$temp - overlap_old$temp,
  na.rm = TRUE
)

old <- old[old$time < min(new$time), ]
old$temp <- old$temp + temp_overlap
data <- rbind(old, new)
data <- data[order(data$time), ]
data <- data[, c(
  "time",
  "precip_amt",
  "temp",
  "wind_spd",
  "wind_dir"
)]
data$datetime <- data$time
# Drop cols with all NA
data <- data[, colSums(is.na(data)) < nrow(data)]
names(data)
# Hourly data
addACTimeseries(
  con = con,
  location = "49650",
  parameter = c(1250, 34, 1154, 1202),
  media = 7,
  aggregation_type = c(
    "instantaneous",
    "sum",
    "instantaneous",
    "instantaneous"
  ),
  start_datetime = "1850-01-01 00:00",
  record_rate = "1 hour",
  owner = 3,
  source_fx = "downloadECCCwx",
  source_fx_args = c(
    "location: 49650, parameter: temp, interval: hour",
    "location: 49650, parameter: precip_amt, interval: hour",
    "location: 49650, parameter: wind_spd, interval: hour",
    "location: 49650, parameter: wind_dir, interval: hour"
  ),
  data = list(
    data.frame(datetime = data$datetime, value = data$temp),
    data.frame(datetime = data$datetime, value = data$precip_amt),
    data.frame(datetime = data$datetime, value = data$wind_spd),
    data.frame(datetime = data$datetime, value = data$wind_dir)
  )
)


# Sadly, ECCC's wind speed data was imported in km/h but the DB parameter is in m/s. Find all wind speed data (parameter_id = 1154) with source_fx = downloadECCCwx and convert from km/h to m/s (import function will be fixed as soon as this data is in the DB).

DBI::dbExecute(
  con,
  "UPDATE timeseries SET active = FALSE WHERE source_fx = 'downloadECCCwx'"
)

ts <- DBI::dbGetQuery(
  con,
  "SELECT timeseries_id FROM timeseries WHERE parameter_id = 1154 AND source_fx = 'downloadECCCwx';"
)
DBI::dbExecute(con, "BEGIN;")
DBI::dbExecute(
  con,
  paste0(
    "UPDATE measurements_continuous SET value = value / 3.6 WHERE timeseries_id IN ",
    "(",
    paste(ts$timeseries_id, collapse = ", "),
    ");"
  )
)
DBI::dbExecute(con, "COMMIT;")

# Similar problem for wind direction: ECCC reports in degrees/10, DB is in degrees. Let's multiply by 10.
ts_dir <- DBI::dbGetQuery(
  con,
  "SELECT timeseries_id FROM timeseries WHERE parameter_id = 1202 AND source_fx = 'downloadECCCwx';"
)
DBI::dbExecute(con, "BEGIN;")
DBI::dbExecute(
  con,
  paste0(
    "UPDATE measurements_continuous SET value = value * 10 WHERE timeseries_id IN ",
    "(",
    paste(ts_dir$timeseries_id, collapse = ", "),
    ");"
  )
)
DBI::dbExecute(con, "COMMIT;")

# Some of the imported daily data has an incorrect time stamp
# Observations building daily values end at 6 UTC on following day (so values reported on the 24th include hours 07 to 23 on the 24th plus 00 to 06 on the 25th)
# The import function already does this:
# data <- data.frame(
# datetime = as.POSIXct(dl$date, tz = "UTC") + 30 * 60 * 60,

# We need to single out data points from timeseries with import function downloadECCCwx and record_rate = '1 day' and add 6 hours to the timestamp ONLY FOR DATA POINTS WHERE THE TIME IS 00:00:00
ts_daily <- DBI::dbGetQuery(
  con,
  "SELECT timeseries_id FROM timeseries WHERE source_fx = 'downloadECCCwx' AND record_rate = '1 day';"
)
DBI::dbExecute(con, "BEGIN;")
for (ts_id in ts_daily$timeseries_id) {
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE measurements_continuous SET datetime = datetime + INTERVAL '6 hours' WHERE timeseries_id = ",
      ts_id,
      " AND EXTRACT(HOUR FROM datetime) = 0 AND EXTRACT(MINUTE FROM datetime) = 0 AND EXTRACT(SECOND FROM datetime) = 0;"
    )
  )
}

# For each of these timeseries, we also need to update the start_datetime in the timeseries table
for (ts_id in ts_daily$timeseries_id) {
  min_datetime <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT MIN(datetime) AS min_datetime FROM measurements_continuous WHERE timeseries_id = ",
      ts_id,
      ";"
    )
  )$min_datetime
  DBI::dbExecute(
    con,
    paste0(
      "UPDATE timeseries SET start_datetime = '",
      min_datetime,
      "' WHERE timeseries_id = ",
      ts_id,
      ";"
    )
  )
}

# Turn the timeseries back on
# ! Ensure AquaCache package is updated on the virtual machine!!!
DBI::dbExecute(
  con,
  "UPDATE timeseries SET active = TRUE WHERE source_fx = 'downloadECCCwx' AND end_datetime > '2024-01-01';"
)
