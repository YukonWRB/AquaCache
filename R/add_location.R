#' Add location(s) to database
#'
#' This function facilitates the addition of one or multiple stations to the database by adding entries to the locations table. The locations table and matching measurement tables will be populated during the next run of hydro_update_daily. If specifying level or flow, incorporation will first be attempted for a WSC location. Failure triggers an attempt to add a station from Aquarius.
#'
#' @param path The path to the local database, with extension.
#' @param location The location identifier, exactly as per the WSC or as written in Aquarius. Case sensitive. Specify as a character vector of length 1 or more.
#' @param type The type of data for this location, matched 1:1 to the locations vector. One of "level", "flow", "SWE", "depth" (for snow depth) or "distance". The later is typically used for distances from bridge girders to water surface, but could be used for other distances.
#'
#' @return One or more new entries are created in the table locations.
#' @export
#'

add_location <- function(path, location, type){

  hydro <- WRBtools::hydroConnect(path = path)
  on.exit(DBI::dbDisconnect(hydro))

  add <- data.frame(location = location, data_type = type)

  DBI::dbAppendTable(hydro, "locations", add)

}
