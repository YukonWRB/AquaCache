# Reproduce the migration from the checked-in Patch 68 fixture. Only testdb is
# replaced; existing cluster role passwords are preserved.
readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
Sys.setenv(PGPASSWORD = Sys.getenv("aquacacheAdminPass"))
args <- c("-h", "10.250.12.154", "-p", Sys.getenv("aquacachePort", "5432"),
          "-U", Sys.getenv("aquacacheAdminUser"))
backup_dir <- normalizePath("dev/patch69-review", winslash = "/")
input <- gzfile("tests/testthat/fixtures/test_db.sql.gz")
sql <- readLines(input, warn = FALSE)
close(input)
sql <- gsub("EXECUTE 'ALTER ROLE public_reader LOGIN PASSWORD ''aquacache''';",
            "PERFORM 1; -- Preserve the existing cluster role password.", sql, fixed = TRUE)
stopifnot(!any(grepl("ALTER ROLE|CREATE DATABASE|\\\\connect", sql)))
path <- file.path(backup_dir, "patch68-fixture.sql")
writeLines(sql, path, useBytes = TRUE)
con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "postgres", host = "10.250.12.154",
  port = Sys.getenv("aquacachePort", "5432"), user = Sys.getenv("aquacacheAdminUser"),
  password = Sys.getenv("aquacacheAdminPass"))
exists <- DBI::dbGetQuery(con,
  "SELECT 1 FROM pg_database WHERE datname='testdb_patch69_review_backup'")
if (!nrow(exists)) {
  DBI::dbExecute(con, "ALTER DATABASE testdb ALLOW_CONNECTIONS false")
  tryCatch({
    DBI::dbGetQuery(con,
      "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname='testdb'")
    DBI::dbExecute(con, "CREATE DATABASE testdb_patch69_review_backup TEMPLATE testdb")
  }, finally = DBI::dbExecute(con, "ALTER DATABASE testdb ALLOW_CONNECTIONS true"))
}
DBI::dbExecute(con, "DROP DATABASE testdb WITH (FORCE)")
DBI::dbExecute(con, "CREATE DATABASE testdb")
DBI::dbDisconnect(con)
status <- system2("C:/rtools45/x86_64-w64-mingw32.static.posix/bin/psql.exe",
  c(args, "-d", "testdb", "-v", "ON_ERROR_STOP=1", "-q", "-f", shQuote(path)),
  stdout = "dev/patch69-review/restore.log", stderr = "dev/patch69-review/restore-errors.log")
stopifnot(status == 0L)
cat("Restored testdb at Patch 68; preserved pre-review backup.\n")
source("dev/patch69-review/configure-test-access.R")
