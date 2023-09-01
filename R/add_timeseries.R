#' Add timeseries to database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one or multiple timeseries to the database by adding entries to the timeseries table. The locations table and matching measurement tables will be populated during the next run of [hydro_update_daily()]. If specifying level or flow, incorporation will first be attempted for a WSC location. Failure triggers an attempt to add a station from Aquarius.
#'
#' If [hydro_update_daily()] fails to find a corresponding timeseries anywhere, the offending timeseries will be marked as "FAILED" in the timeseries table in the "category" column. Reset the "category" field to try again.
#'
#'
#' @param path The path to the local database, with extension.
#' @param location The location identifier, exactly as per the WSC or as written in Aquarius. Case sensitive. Specify as a character vector of length 1 or more.
#' @param parameter The parameter for this location, matched 1:1 to the other vectors. If at all possible match existing parameters, such as "level", "flow", "SWE", "snow depth", or "distance", or "temperature".
#' @param unit The measurement unit for this new location, matched 1:1 to the other vectors. If at all possible try to match an existing unit such as "m" for level or distance, "m3/s" for flow volumes, "mm" for mm SWE or precipitation, "cm" for snow depth.
#' @param category Continuous or discrete, matched to the other vectors 1:1.
#' @param network The name of the network. Again, try to match existing network names.
#'
#' @return One or more new entries are created in the table 'locations.'
#' @export
#' @seealso [WRBtools::DB_browse_ts()] to see what's in the timeseries table already and match parameters, units, category, types, networks.

add_timeseries <- function(path, location, parameter, unit, category, network){

  hydro <- WRBtools::hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(hydro))

  add <- data.frame(location = location, parameter = parameter, unit = unit, category = category, network = network)

  DBI::dbAppendTable(hydro, "timeseries", add)

}
