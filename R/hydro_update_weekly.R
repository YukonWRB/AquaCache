#' Weekly update of hydro database
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved". Daily means and statistics are recalculated for any potentially affected days in the daily tables, except for daily means provided in HYDAT historical tables.
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param WSC_range The starting date from which to pull real-time WSC data from the web and replace in the local database. Default is max possible days.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next six parameters. FALSE will only populate with WSC data.
#' @param aquarius_range Should only unapproved (locked) data be replaced, or all available data? Select from "all" or "unapproved". Default is "unapproved".
#' @param stage The name of the stage (level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge (flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same parameter and label.
#' @param distance The name of the distance timeseries as it appears in Aquarius if it exists, in the form Parameter.Label. All stations must have the same parameter and label. Usually used for distance from bridge girders to water surface.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return Updated entries in the hydro database.
#' @import tidyhydat.ws
#' @export
#'


hydro_update_weekly <- function(path, WSC_range = Sys.Date()-577, aquarius = TRUE, aquarius_range = "unapproved", stage = "Stage.Corrected", discharge = "Discharge.Master", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", distance = "Distance.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS")
{

  if (!(aquarius_range %in% c("all", "unapproved"))){
    stop("The parameter aquarius_range must be either 'all' or 'unapproved'")
  }

  if (aquarius){
    if (is.null(Sys.getenv("AQPASS"))){
      stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
    }
    if (is.null(Sys.getenv("AQUSER"))){
      stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
    }
  }
  if (is.null(Sys.getenv("WS_USRNM"))){
    stop("Your WSC user name must be available in the .Renviron file in the form WS_USRNM='yourusername'")
  }
  if (is.null(Sys.getenv("WS_PWD"))){
    stop("Your WSC password must be available in the .Renviron file in the form WS_PWD='yourpassword'")
  }

  hydro <- WRBtools::hydroConnect(path = path)
  on.exit(DBI::dbDisconnect(hydro))

  locations <- DBI::dbGetQuery(hydro, "SELECT * FROM locations WHERE name IS NOT 'FAILED' AND parameter IN ('level', 'flow', 'distance', 'SWE', 'snow depth') AND type = 'continuous'")
  for (i in 1:nrow(locations)){
    loc <- locations$location[i]
    parameter <- locations$parameter[i]
    operator <- locations$operator[i]
    units <- DBI::dbGetQuery(hydro, paste0("SELECT DISTINCT units FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]

    tryCatch({
      if (operator == "WRB" & aquarius){
        #need to figure out a way to seamlessly incorporate new parameters.
        ts_name <- if(parameter == "SWE") SWE else if (parameter=="snow depth") depth else if (parameter == "level") stage else if (parameter == "flow") discharge else if (parameter == "distance") distance
        if (aquarius_range == "unapproved"){
          first_unapproved <- DBI::dbGetQuery(hydro, paste0("SELECT MIN(datetime_UTC) FROM realtime WHERE parameter = '", parameter, "' AND location = '", locations$location[i], "' AND NOT approval = 'approved'"))[1,]
          data <- WRBtools::aq_download(loc_id = locations$location[i], ts_name = ts_name, start = first_unapproved, server = server)
        } else if (aquarius_range == "all"){
          data <- WRBtools::aq_download(loc_id = locations$location[i], ts_name = ts_name, server = server)
        }
        ts <- data.frame("location" = locations$location[i], "parameter" = parameter, "datetime_UTC" = as.character(data$timeseries$timestamp_UTC), "value" = data$timeseries$value, "units" = units, "grade" = data$timeseries$grade_description, "approval" = data$timeseries$approval_description)

      } else if (operator == "WSC"){
        token <- suppressMessages(tidyhydat.ws::token_ws())
        data <- suppressMessages(tidyhydat.ws::realtime_ws(locations$location[i], if (parameter == "flow") 47 else if (parameter == "level") 46, start_date = WSC_range,  token = token))
        data <- data[,c(2,4,1)]
        names(data) <- c("datetime_UTC", "value", "location")
        data$parameter <- parameter
        data$datetime_UTC <- as.character(data$datetime_UTC)
        data$approval <- "preliminary"
        data$units <- units
        ts <- data
      }

      if (nrow(ts) > 0){
        #TODO: the current delete + append locks the database for far too long. Should look at replacing only rows where necessary instead. Could be done by pulling the data, comparing, and using UPDATE.
        DBI::dbExecute(hydro, paste0("DELETE FROM realtime WHERE parameter = '", parameter, "' AND location = '", loc, "' AND datetime_UTC BETWEEN '", min(ts$datetime_UTC), "' AND '", max(ts$datetime_UTC), "'"))
        DBI::dbAppendTable(hydro, paste0(table_name, "_realtime"), ts)
        #make the new entry into table locations
        DBI::dbExecute(hydro, paste0("UPDATE locations SET end_datetime_UTC = '", as.character(max(ts$datetime_UTC)),"' WHERE location = '", locations$location[i], "' AND parameter = '", parameter, "' AND type = 'continuous'"))

        #Recalculate daily means and statistics
        calculate_stats(locations = data.frame("location" = loc,
                                               "parameter" = parameter),
                        path = path)

        units <- DBI::dbGetQuery(hydro, paste0("SELECT DISTINCT units FROM daily WHERE parameter = '", parameter, "' AND location = '", loc, "'"))[1,]
      }
    }, error = function(e) {
      print(paste0("Failed on location ", locations$location[i], " and parameter ", locations$parameter[i]))
    }
    )
  }

    DBI::dbExecute(hydro, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_update_weekly'"))

} #End of function
