# Adding RWIS stations to database and timeseries for their available parameters

rwis <- RWISConnect()
con <- AquaConnect(host = "199.247.132.26")
stns <- DBI::dbGetQuery(
  rwis,
  "SELECT id, name, abbreviation, owner_agency, latitude, longitude, elevation FROM stations_station WHERE owner_agency NOT IN ('Yukon Water Resources Branch', 'Nav Canada', 'British Columbia Ministry of Environment', 'Department of National Defense', 'Meteorological Service of Canada') AND name NOT IN ('WFM Portable 3', 'Helmut', 'Castle', 'Boya Lake')"
)

# For each station, ensure that there is at least some data in measurements_measurement. If not, remove the row.
stns$remove <- FALSE
for (i in seq_len(nrow(stns))) {
  stn_id <- stns$id[i]
  meas_check <- DBI::dbGetQuery(
    rwis,
    paste0(
      "SELECT * FROM measurements_measurement WHERE station_id = ",
      stn_id,
      " LIMIT 1"
    )
  )
  if (nrow(meas_check) == 0) {
    stns[i, "remove"] <- TRUE
  }
}
stns <- stns[stns$remove == FALSE, ]


# Replace any negative elevation values with NA
stns$elevation[stns$elevation <= 0] <- NA
# Remove stations with -999 in either latitude or longitude
stns <- stns[stns$latitude != -999 & stns$longitude != -999, ]


# Map the stations on a leaflet map to visually check locations
library(leaflet)
leaflet(data = stns) %>%
  addTiles() %>%
  addCircleMarkers(~longitude, ~latitude, popup = ~name)

# Add the new owners to the database
owners <- data.frame(
  name = c(
    "Yukon Government Department of Public Works",
    "Yukon Government Wildland Fire Management",
    "Yukon Government Department of Environment (Parks)",
    "British Columnbia Wildfire Service",
    "Yukon Government Department of Energy, Mines, Resources",
    "Parks Canada",
    "Avalanche Canada"
  ),
  name_fr = c(
    "Gouvernement du Yukon, ministère de la voirie et des traveaux publics",
    "Gouvernement du Yukon, gestion des feux de forêt",
    "Gouvernement du Yukon, ministère de l'Environnement (parcs)",
    "Service des incendies de forêt de la Colombie-Britannique",
    "Gouvernement du Yukon, ministère de l'Énergie, des Mines et des Ressources",
    "Parcs Canada",
    "Avalanche Canada"
  )
)

DBI::dbAppendTable(con, "organizations", owners)

networks <- data.frame(
  name = c(
    "Yukon Widland Fire Management",
    "Parks Canada",
    "Avalanche Canada",
    "British Columbia Wildfire Service"
  ),
  description = c(
    "Monitoring of weather conditions to support wildfire management in Yukon.",
    "Weather monitoring stations operated by Parks Canada.",
    "Weather stations operated by Avalanche Canada for avalanche forecasting and safety.",
    "Weather monitoring stations operated by the British Columbia Wildfire Service."
  ),
  name_fr = c(
    "Surveillance des conditions météorologiques pour la gestion des feux de forêt au Yukon.",
    "Stations météorologiques exploités par Parcs Canada.",
    "Stations météorologiques exploitées par Avalanche Canada pour la prévision et la sécurité des avalanches.",
    "Stations météorologiques exploitées par le Service des feux de forêt de la Colombie-Britannique."
  ),
  type = 1
)
DBI::dbAppendTable(con, "networks", networks)

# Export stns to csv and fill in values for import manually
write.csv(
  stns,
  file = "dev/RWIS_stations_to_add.csv",
  row.names = FALSE
)

# Fill it in...

# Load the csv again
stns_to_add <- data.table::fread(
  "dev/RWIS_stations_to_add_utf8.csv",
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)
stns_to_add <- stns_to_add[stns_to_add$include, ]
add <- stns_to_add[, c(
  "name",
  "name_fr",
  "latitude",
  "longitude",
  "share_with",
  "network",
  "owner",
  "datum_id_from",
  "datum_id_to",
  "elevation_m",
  "location_type",
  "abbreviation"
)]
add$data_sharing_agreement_id <- NA
add$project <- NA
add$contact <- NA
add$current <- TRUE
add$note <- NA
add$location <- add$abbreviation
add$abbreviation <- NULL
add$conversion_m <- add$elevation_m
add$elevation_m <- NULL

addACLocation(add, con = con)

params_map <- stats::setNames(
  c("ta", "pcp1", "pcp6", "pcp12", "pcp24", "rn1", "sn1", "ws", "wd"),
  c(
    "temperature, air",
    "precipitation, total",
    "precipitation, total",
    "precipitation, total",
    "precipitation, total",
    "precipitation, rain",
    "precipitation, snow",
    "velocity, wind",
    "wind direction (direction from, expressed 0-360 deg)"
  )
)

# For each location in 'add', use addACTimeseries to add timeseries for each parameter in params_map. If it fails for all, delete the location from the DB
for (i in 1:nrow(add)) {
  row <- add[i, ]
  for (j in 1:length(params_map)) {
    param_rwis <- unname(params_map[j])
    param_ac <- names(params_map)[j]
    param_id_ac <- DBI::dbGetQuery(
      con,
      "SELECT parameter_id FROM parameters WHERE param_name = $1",
      params = list(param_ac)
    )[1, 1]
    args <- paste0("location:", row$location, ", parameter:", param_rwis)
    addACTimeseries(
      start_datetime = "1900-01-01 00:00",
      location = row$location,
      parameter = as.numeric(param_id_ac),
      media = 7,
      record_rate = '1 hour',
      share_with = "public_reader",
      owner = as.numeric(row$owner),
      source_fx = "downloadRWIS",
      source_fx_args = args,
      aggregation_type = if (grepl("precipitation", param_ac)) {
        "sum"
      } else {
        "instantaneous"
      },
      con = con
    )
  }
}

# Manually check if any locations have no timeseries, and delete them from the DB
# If there are timeseries, iterate over them to figure out their nominal record_rate and update the timeseries table accordingly
add$delete <- TRUE
for (i in 1:nrow(add)) {
  row <- add[i, ]
  ts_check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM timeseries WHERE location = $1",
    params = list(row$location)
  )
  if (nrow(ts_check) == 0) {
    # Delete location
    # DBI::dbExecute(
    #   con,
    #   "DELETE FROM locations WHERE location = $1",
    #   params = list(row$location)
    # )
  } else {
    for (j in 1:nrow(ts_check)) {
      ts_row <- ts_check[j, ]
      meas_times <- DBI::dbGetQuery(
        con,
        "SELECT datetime FROM measurements_continuous WHERE timeseries_id = $1 ORDER BY datetime ASC",
        params = list(ts_row$timeseries_id)
      )
      if (nrow(meas_times) > 0) {
        add$delete[i] <- FALSE
        # Now determine the nominal record rate
        periods <- calculate_period(
          meas_times,
          timeseries_id = ts_row$timeseries_id,
          con = con
        )$period
        # Count the most common period
        nominal_period <- names(sort(table(periods), decreasing = TRUE)[1])
        # Update the timeseries table
        DBI::dbExecute(
          con,
          "UPDATE timeseries SET record_rate = $1 WHERE timeseries_id = $2",
          params = list(nominal_period, ts_row$timeseries_id)
        )
      }
    }
  }
}
# Take a peek
delete <- add[add$delete, ]
delete
for (i in 1:nrow(delete)) {
  row <- delete[i, ]
  DBI::dbExecute(
    con,
    "DELETE FROM locations WHERE location = $1",
    params = list(row$location)
  )
}
