# If on CI, set environment variables which would otherwise be found in the user's .Renviron file.
# We're working with a micro postgres database here installed on the CI environment; see the .github/workflows/R-CMD-check.yaml file.
# This DB is created with AquaCache::create_test_db() and default parameters, expect for the username which uses 'postgres'
if (Sys.getenv("CI") == "true") {
  Sys.setenv(
    aquacacheName = "testdb",
    aquacacheHost = "localhost",
    aquacachePort = "5432",
    aquacacheUser = "runner",
    aquacachePass = "runner",
    aquacacheAdminUser = "runner",
    aquacacheAdminPass = "runner",
    AQUSER = "readonly",
    AQPASS = "WaterIsLife",
    AQSERVER = "https://yukon.aquaticinformatics.net/AQUARIUS"
  )
  message("Running on CI, setting environment accordingly.")
}

set.seed(123) # Set seed for reproducibility in tests


# Check for Postgres credentials; skip tests if not available
skip_if_no_postgres <- function() {
  testthat::skip_if_not_installed("RPostgres")
  required <- c(
    "aquacacheName",
    "aquacacheHost",
    "aquacachePort",
    "aquacacheAdminUser",
    "aquacacheAdminPass"
  )
  missing <- required[Sys.getenv(required, unset = "") == ""]
  if (length(missing) > 0) {
    testthat::skip(
      paste(
        "Postgres test database credentials not available:",
        paste(missing, collapse = ", ")
      )
    )
  }
}
