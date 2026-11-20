readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
Sys.setenv(aquacacheName = "testdb", aquacacheHost = "10.250.12.154")
results <- testthat::test_local(".", filter = "import-mapping|missing-import|discrete-import-results|direct-import",
  reporter = "summary", stop_on_failure = TRUE)
report <- as.data.frame(results)
stopifnot(all(report$failed == 0L), !any(report$error))
