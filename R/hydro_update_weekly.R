#' Weekly update of hydro database
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved"
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param WSC_range The starting date from which to pull real-time WSC data from the web and replace in the local database. Default is max possible days.
#' @param aquarius_range Should only unapproved (locked) data be replaced, or all available data? Select from "all" or "unapproved". Default is "unapproved".
#'
#' @return Updated entries in the hydro database.
#' @export
#'

hydro_weekly_update <- function(path, WSC_range = Sys.Date()-577, aquarius_range = "unapproved") {

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  if (WSC_range < Sys.Date()-577) {WSC_range <- Sys.Date()-577}

  #Two parts: part 1 deals with WSC data and downloads + replaces the previous 18 months. Part 2 deals with Aquarius data and, by default, downloads and replaces only the data that was not labelled as "approved" in the previous update.

  #1. First deal with WSC data
  new_realtime <- list(flow = list(), level = list())
  library(tidyhydat.ws) #necessary because internal data is not properly specified
  for (i in WSCstns){
    token <- tidyhydat.ws::token_ws(username = Sys.getenv("WS_USRNM"), password = Sys.getenv("WS_PWD"))
    try(new_realtime$flow[[i]] <- tidyhydat.ws::realtime_ws(i, 47, start_date = WSC_range, end_date = Sys.Date(), token = token))
    try(new_realtime$level[[i]] <- tidyhydat.ws::realtime_ws(i, 46, start_date = WSC_range, end_date = Sys.Date(), token = token))
  }

  #keep only what's necessary from the raw download
  for (i in names(new_realtime$flow)){
    new_realtime$flow[[i]] <- new_realtime$flow[[i]][,c(2,4,1)]
    names(new_realtime$flow[[i]]) <- c("datetime_UTC", "flow", "location")
  }
  for (i in names(new_realtime$level)){
    new_realtime$level[[i]] <- new_realtime$level[[i]][,c(2,4,1)]
    names(new_realtime$level[[i]]) <- c("datetime_UTC", "level", "location")
  }

  new_flow_rt <- do.call("rbind", new_realtime$flow)
  rownames(new_flow_rt) <- NULL
  new_level_rt <- do.call("rbind", new_realtime$level)
  rownames(new_level_rt) <- NULL

  #Delete the entries and then append the new data into the database. This has the added bonus of adding new rows as well as replacing existing rows
  delete_bracket <- as.numeric(c(min(new_flow_rt$datetime_UTC), max(new_flow_rt$datetime_UTC)))
  DBI::dbExecute(hydro, paste0("DELETE FROM WSC_flow_realtime WHERE datetime_UTC BETWEEN ", delete_bracket[1], " AND ", delete_bracket[2])) #NOTE: SQL BETWEEN is inclusive
  DBI::dbAppendTable(hydro, "WSC_flow_realtime", new_flow_rt)

  delete_bracket <- as.numeric(c(min(new_level_rt$datetime_UTC), max(new_level_rt$datetime_UTC)))
  DBI::dbExecute(hydro, paste0("DELETE FROM WSC_level_realtime WHERE datetime_UTC BETWEEN ", delete_bracket[1], " AND ", delete_bracket[2])) #NOTE: SQL BETWEEN is inclusive
  DBI::dbAppendTable(hydro, "WSC_level_realtime", new_level_rt)
}

