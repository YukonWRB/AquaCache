path <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/database/hydro.sqlite"

path_test <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/database/test/hydro-test.sqlite"


con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                       dbname = "hydromet",
                       host = "localhost",
                       port = 5432,
                       user = "postgres",
                       password = "SnowFa11ing")
