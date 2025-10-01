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
