readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
pkgload::load_all(".", quiet = TRUE)
con <- AquaConnect(name = "testdb", silent = TRUE, check = FALSE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

assignments <- DBI::dbGetQuery(
  con,
  "SELECT
     tsa.timeseries_id,
     tsa.source_fx,
     tsa.fetch_priority,
     tsa.synchronize_priority,
     tsa.active,
     tsa.source_fx_args::text,
     MAX(m.datetime) AS last_datetime
   FROM continuous.timeseries_source_adapters tsa
   LEFT JOIN continuous.measurements_continuous m
     ON m.timeseries_id = tsa.timeseries_id
   WHERE tsa.active
     AND tsa.source_fx <> 'downloadWSC'
   GROUP BY
     tsa.timeseries_id,
     tsa.source_fx,
     tsa.fetch_priority,
     tsa.synchronize_priority,
     tsa.active,
     tsa.source_fx_args
   ORDER BY tsa.source_fx, tsa.timeseries_id
   LIMIT 40"
)
print(assignments, row.names = FALSE)

adapter_capabilities <- getSourceAdapterCapabilities(
  con = con,
  data_domain = "continuous"
)
cl <- parallel::makeCluster(1L)
on.exit(parallel::stopCluster(cl), add = TRUE)
parallel::clusterExport(cl, "adapter_capabilities", envir = environment())
worker_state <- parallel::clusterEvalQ(cl, {
  matches <- adapter_capabilities$source_fx == "downloadAquarius"
  implicit_subset <- adapter_capabilities[which(matches)]
  explicit_subset <- adapter_capabilities[which(matches), , drop = FALSE]
  list(
    class = class(adapter_capabilities),
    implicit_rows = nrow(implicit_subset),
    implicit_columns = names(implicit_subset),
    explicit_rows = nrow(explicit_subset),
    explicit_source_fx = explicit_subset$source_fx
  )
})
print(worker_state)

dev_con <- AquaConnect(
  name = "aquacache",
  host = "10.250.12.154",
  silent = TRUE,
  check = FALSE
)
on.exit(DBI::dbDisconnect(dev_con), add = TRUE)
dev_assignments <- DBI::dbGetQuery(
  dev_con,
  "SELECT
     tsa.timeseries_id,
     tsa.source_fx,
     tsa.source_fx_args::text,
     tsa.fetch_priority,
     tsa.synchronize_priority
   FROM continuous.timeseries_source_adapters tsa
   WHERE tsa.active
     AND tsa.source_fx IN ('downloadAquarius', 'downloadECCCwx', 'downloadRWIS')
   ORDER BY tsa.source_fx, tsa.timeseries_id
   LIMIT 20"
)
print(dev_assignments, row.names = FALSE)

test_candidates <- DBI::dbGetQuery(
  con,
  "SELECT
     t.timeseries_id,
     t.timeseries_type,
     t.active,
     at.aggregation_type,
     MAX(m.datetime) AS last_datetime
   FROM continuous.timeseries t
   JOIN continuous.aggregation_types at
     ON at.aggregation_type_id = t.aggregation_type_id
   LEFT JOIN continuous.measurements_continuous m
     ON m.timeseries_id = t.timeseries_id
   WHERE t.timeseries_id IN (20, 21)
   GROUP BY t.timeseries_id, t.timeseries_type, t.active, at.aggregation_type
   ORDER BY t.timeseries_id"
)
print(test_candidates, row.names = FALSE)
