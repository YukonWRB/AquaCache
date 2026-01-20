library(AquaCache)

config <- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHost"),
    dbPort = Sys.getenv("aquacachePort"),
    dbUser = Sys.getenv("aquacacheAdminUser"),
    dbPass = Sys.getenv("aquacacheAdminPass")
)

con <- AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    user = config$dbUser,
    pass = config$dbPass
)


imputeMissing(
    tsid = 513,
    radius = 10,
    start = as.Date("2015-01-01"),
    end = as.Date("2017-01-01"),
    con = con
)
