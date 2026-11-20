# Run after reset-testdb.R. All database work is restricted to disposable testdb.
source("dev/patch69-review/seed-patch68.R")
source("dev/patch69-review/migrate.R")
Sys.setenv(AQUACACHE_TEST_PATCH69 = "true")
source("dev/patch69-review/run-tests.R")
