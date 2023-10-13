#' Add images to the DB
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param path The path to the local hydro SQLite database, with extension. Passed to [YGWater::hydroConnect()].
#' @param locations The locations you wish to have updated as a character vector. Defaults to "all", though the meaning of all is dependent on the parameter 'aquarius'. Will search for all timeseries with the specified location codes, so level + flow, snow depth + SWE, etc.
#'
#' @return Updated entries in the hydro database.
#' @import tidyhydat.ws
#' @export
#'
