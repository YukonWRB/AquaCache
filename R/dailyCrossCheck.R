#' Cross-check daily and realtime tables for consistency
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Checks that there is a corresponding entry in table "daily" for every day in table "realtime" for every location specified in `locations`.
#'
#' @param path The path to the local hydro SQLite database, with extension. Passed to [WRBtools::hydroConnect()].
#' @param locations The locations you wish to have updated as a character vector. Defaults to "all", though the meaning of all is dependent on the parameter 'aquarius'. Will search for all timeseries with the specified location codes, so level + flow, snow depth + SWE, etc.
#'
#' @return Updated entries in the hydro database.
#' @import tidyhydat.ws
#' @export
#'

dailyCrossCheck <- function(path, locations = "all")
{

  hydro <- WRBtools::hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(hydro))

  if (locations == "all"){
    all_timeseries <- DBI::dbGetQuery(hydro, "SELECT * FROM timeseries WHERE type = 'continuous'")
  } else {
    all_timeseries <- DBI::dbGetQuery(hydro, paste0("SELECT * FROM timeseries WHERE type = 'continuous' AND location IN ('", paste(locations, collapse = "', '"), "')"))
  }

  for (i in 1:nrow(all_timeseries)){
    loc <- all_timeseries$location[i]
    parameter <- all_timeseries$parameter[i]
    tryCatch({
      hydro <- WRBtools::hydroConnect(path = path, silent = TRUE)
      realtime <- DBI::dbGetQuery(hydro, paste0("SELECT MIN(datetime_UTC) AS datetime_UTC FROM realtime WHERE location = '", loc, "' AND parameter = '", parameter, "' GROUP BY DATE(datetime_UTC);"))$datetime_UTC
      #NOTE: the line below may not be needed with postgres or SQL server since they have proper time formats.
      realtime_days <- lubridate::date(realtime)
      daily_days <- DBI::dbGetQuery(hydro, paste0("SELECT date FROM daily WHERE location = '", loc, "' AND parameter = '", parameter, "';"))$date
      DBI::dbDisconnect(hydro)

      first_missing <- NULL
      done <- FALSE
      index <- 1
      while (!done){
        if (index == length(realtime_days)){
          done <- TRUE
        }
        if (!(realtime_days[index] %in% daily_days)){
          first_missing <- realtime_days[index]
          done <- TRUE
        } else {
          index <- index + 1
        }
      }
      if (!is.null(first_missing)){
        calculate_stats(timeseries = data.frame("location" = loc, "parameter" = parameter), start_recalc = first_missing, path = path)
        print(paste0("Found missing entries for location ", loc, " and parameter ", parameter))
      }
    }, error = function(e) {
      print(paste0("Failed on location ", loc, " and parameter ", parameter))
    })
  }

}
