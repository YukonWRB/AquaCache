#' Weekly update of hydro database
#'
#' The weekly update function pulls and replaces a large tranche of Water Survey of Canada and Aquarius data to ensure incorporation of any edits. By default, all real-time water survey data currently available online is replaced, as is any Aquarius data not previously labelled as "approved"
#'
#' @param path The path to the local hydro SQLite database, with extension.
#' @param WSC_range The starting date from which to pull real-time WSC data from the web and replace in the local database. Default is max possible days.
#' @param aquarius TRUE if you are fetching data from Aquarius, in which case you should also check the next six parameters. FALSE will only populate with WSC data.
#' @param aquarius_range Should only unapproved (locked) data be replaced, or all available data? Select from "all" or "unapproved". Default is "unapproved".
#' @param stage The name of the stage(level) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param discharge The name of the discharge(flow) timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names. !This DOES NOT apply to WSC stations mirrored in Aquarius.
#' @param SWE The name of the snow water equivalent timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param depth The name of the snow depth timeseries as it appears in Aquarius, if it exists, in the form Parameter.Label. All stations must have the same names.
#' @param server The URL to your Aquarius server, if needed. Note that your credentials must be in your .Renviron profile: see ?WRBtools::aq_download.
#'
#' @return Updated entries in the hydro database.
#' @export
#'

hydro_update_weekly <- function(path, WSC_range = Sys.Date()-577, aquarius = TRUE, aquarius_range = "unapproved", stage = "Stage.Publish", discharge = "Discharge.Publish", SWE = "SWE.Corrected", depth = "Snow Depth.TempCompensated.Corrected", server = "https://yukon.aquaticinformatics.net/AQUARIUS")
{

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

  library(tidyhydat.ws)
  detach("package:tidyhydat.ws", unload = TRUE)

  hydro <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(hydro))

  #Update WSC data
  #Update Aquarius data
  tryCatch({
    aq_flow <- DBI::dbGetQuery(hydro, "SELECT *
                               FROM WRB_flow_realtime
                               WHERE NOT approval = 'approved' AND MAX(datetime_UTC)
                               GROUP BY location")
    for (i in aq_flow){

    }
    aq_level <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_level_realtime")
    for (i in aq_level){

    }
    aq_depth <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_snow_pillow_depth_realtime")
    for (i in aq_depth){

    }
    aq_SWE <- DBI::dbGetQuery(hydro, "SELECT * FROM WRB_snow_pillow_SWE_realtime")
    for (i in aq_SWE){

    }
  }, error = function(e) {
    warning("New information from Aquarius may not have been properly downloaded. Try again later.")
  }
  )
  #first find out the earliest date not labelled as "approved", then download from that to now.
  #VACUUM the database periodically after everything is up to date


} #End of function
