#' Add location(s) to database
#'
#' This function facilitates the addition of one or multiple stations to the database by adding entries to the locations table. The locations table and matching measurement tables will be populated during the next run of hydro_update_daily. If specifying level or flow, incorporation will first be attempted for a WSC location. Failure triggers an attempt to add a station from Aquarius.
#'
#' @param path The path to the local database, with extension.
#' @param location The location identifier, exactly as per the WSC or as written in Aquarius. Case sensitive. Specify as a character vector of length 1 or more.
#' @param parameter The parameter for this location, matched 1:1 to the locations vector. One of "level", "flow", "SWE", "snow depth", or "distance". The later is typically used for distances from bridge girders to water surface, but could be used for other distances.
#' @param type Continuous or discrete, matched to the locations and parameter vectors 1:1.
#'
#' @return One or more new entries are created in the table locations.
#' @export
#'

add_location <- function(path, location, parameter, type){

  hydro <- WRBtools::hydroConnect(path = path)
  on.exit(DBI::dbDisconnect(hydro))

  add <- data.frame(location = location, parameter = parameter, type = type)

  DBI::dbAppendTable(hydro, "locations", add)

}
