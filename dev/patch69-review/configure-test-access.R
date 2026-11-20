# Match restore_seed_db's database-local reader grants without changing any
# existing cluster login, password or membership.
readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
pkgload::load_all(".", quiet = TRUE)
con <- AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
DBI::dbExecute(con, aquacache_public_reader_grants_sql(con,
  excluded_tables = c("continuous.measurements_continuous", "application.api_requests",
    "application.feedback", "application.images", "application.notifications"),
  excluded_schemas = "audit"))
DBI::dbDisconnect(con)
