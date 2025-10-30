# Adding new stations for ECCC WQ data
# Add a new network
df <- data.frame(
  name = "National Long-Term Water Quality Monitoring Network",
  description = "Operated by Environment and Climate Change Canada and partner organizations. A network of monitoring stations across Canada that collect long-term water quality data to assess trends and changes in aquatic ecosystems.",
  name_fr = "Réseau national de surveillance à long terme de la qualité de l'eau",
  description_fr = "Exploité par Environnement et Changement climatique Canada et des organisations partenaires. Un réseau de stations de surveillance à travers le Canada qui recueillent des données à long terme sur la qualité de l'eau pour évaluer les tendances et les changements dans les écosystèmes aquatiques.",
  type = 1
)

DBI::dbAppendTable(con, "networks", df)

net <- DBI::dbGetQuery(
  con,
  "SELECT network_id FROM networks WHERE name = 'National Long-Term Water Quality Monitoring Network'"
)[1, 1]

owner <- DBI::dbGetQuery(
  con,
  "SELECT organization_id FROM organizations WHERE name = 'Environment and Climate Change Canada'"
)[1, 1]

df <- data.frame(
  location = c(
    "YT09AB0006",
    "YT09AB0008",
    "YT09DD0008",
    "YT09DD0014",
    "YT09FB0003",
    "YT09FC0002",
    "YT09BC0021"
  ),
  name = c(
    "Yukon River Below Marsh Lake",
    "Yukon River Above Takhini River",
    "South McQuesten River Below Flat Creek",
    "Haggart Creek Above South McQuesten River",
    "Porcupine River Above Old Crow River",
    "Old Crow River at the Mouth",
    "Rose Creek Above Anvil Creek"
  ),
  name_fr = c(
    "Fleuve Yukon au barrage du lac Marsh",
    "Fleuve Yukon en amont de la rivière Takhini",
    "Rivière South McQuesten en aval du ruisseau Flat",
    "Ruisseau Haggart en amont de la rivière South McQuesten",
    "Rivière Porcupine en amont de la rivière Old Crow",
    "Rivière Old Crow à l'embouchure",
    "Ruisseau Rose en amont du reuisseau Anvil"
  ),
  latitude = c(
    60.57874,
    60.83909,
    63.9141,
    63.93388,
    67.5797,
    67.58065,
    62.3751
  ),
  longitude = c(
    -134.68671,
    -135.18103,
    -135.7231,
    -136.03763,
    -139.7819,
    -139.80127,
    -133.544
  ),
  share_with = "public_reader",
  owner = owner,
  data_sharing_agreement_id = NA,
  note = c(
    "Sampled upstream of the Lewes dam structure",
    "Approx 200m upstream of the Takhini River confluence",
    NA,
    "Slight change in exact monitoring location with no expected impact on data. Old site coordinates: 63.908, -136.03548.",
    "Approx 600m upstream of the Old Crow river confluence",
    "Approx 250m upstream of confluence with Porcupine river, on right bank.",
    "Approx 14km upstream of Anvil Creek"
  ),
  contact = NA,
  datum_id_from = 10,
  datum_id_to = 10,
  conversion_m = 0,
  current = TRUE,
  network = net,
  project = NA,
  location_type = 1
)

addACLocation(df, con = con)

# Pull the location ids for the new stations from table 'locations'
locs <- DBI::dbGetQuery(
  con,
  "SELECT location_id, location, name FROM locations WHERE location IN ('YT09AB0006', 'YT09AB0008', 'YT09DD0008', 'YT09DD0014', 'YT09FB0003', 'YT09FC0002', 'YT09BC0021', '08AA003', '08AB001', '09EA003', '09EB003', '09FB002', '10AA001', '10MA002', '10MD001') ORDER BY location"
)

locs$args = c(
  "location: YT08AA0010, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fpacific-coastal-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Pacific-Coastal-Cote-Pacifique-2000-present.csv",
  "location: YT08AB0009, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fpacific-coastal-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Pacific-Coastal-Cote-Pacifique-2000-present.csv",
  "location: YT09EA0001, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09EB0005, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09FB0002, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT10AA0001, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Flower-mackenzie-river-basin-long-term-water-quality-monitoring-data-canada-s-north%2FWater-Qual-Eau-Mackenzie-2000-present.csv",
  "location: YT10MA0011, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Flower-mackenzie-river-basin-long-term-water-quality-monitoring-data-canada-s-north%2FWater-Qual-Eau-Mackenzie-2000-present.csv",
  "location: YT10MD0001, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Flower-mackenzie-river-basin-long-term-water-quality-monitoring-data-canada-s-north%2FWater-Qual-Eau-Mackenzie-2000-present.csv",
  "location: YT09AB0006, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09AB0008, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09BC0021, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09DD0008, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09DD0014, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09FB0003, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv",
  "location: YT09FC0002, key: downloadECCCeq1.csv, tz: MST, file: https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fnational-long-term-water-quality-monitoring-data%2Fyukon-river-basin-long-term-water-quality-monitoring-data%2FWater-Qual-Eau-Yukon-2000-present.csv"
)

# For each location, add a sample_series entry and fetch results
for (i in 1:nrow(locs)) {
  row <- locs[i, ]
  # args <- row$args
  # # split into "argument1: value1" etc.
  # args <- strsplit(args, ",\\s*")[[1]]
  # 
  # # split only on first colon
  # keys <- sub(":.*", "", args)
  # vals <- sub("^[^:]+:\\s*", "", args)
  # 
  # # build named list
  # args <- stats::setNames(as.list(vals), keys)
  # 
  # # convert to JSON
  # args <- jsonlite::toJSON(args, auto_unbox = TRUE)
  # 
  # # Build a df and add to sample_series
  # df <- data.frame(
  #   location_id = row$location_id,
  #   default_owner = owner,
  #   default_contributor = owner,
  #   active = TRUE,
  #   source_fx = 'downloadECCCwq',
  #   source_fx_args = args
  # )
  # DBI::dbAppendTable(con, "sample_series", df)

  # Fetch the sample_series_id
  ss_id <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT sample_series_id FROM sample_series WHERE location_id = ",
      row$location_id,
      " AND source_fx = 'downloadECCCwq'"
    )
  )[1, 1]

  getNewDiscrete(con = con, sample_series_id = ss_id)
}
