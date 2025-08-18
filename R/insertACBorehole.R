#' Add a borehole record to the aquacache database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of a borehole record to the database. If the borehole
#' is also a well, additional well-specific information can be provided. The function can also
#' associate a document with the borehole and handle permafrost information if present.
#'
#' @param con A connection to the database. Default NULL uses AquaConnect() and closes the connection afterwards.
#' @param path Path to a document file to attach to the borehole record.
#' @param well_name Name of the borehole/well. Required.
#' @param latitude The latitude coordinate of the borehole location. Required.
#' @param longitude The longitude coordinate of the borehole location. Required.
#' @param location_source Source of the location information (e.g., "GPS", "Survey").
#' @param surveyed_ground_level_elevation Ground elevation from survey in meters.
#' @param purpose_of_well Purpose of the well (e.g., "Monitoring", "Production").
#' @param depth_to_bedrock Depth to bedrock in meters.
#' @param permafrost_present Logical indicating if permafrost is present. Default is FALSE.
#' @param permafrost_top_depth Depth to the top of permafrost in meters, if present.
#' @param permafrost_bottom_depth Depth to the bottom of permafrost in meters, if present.
#' @param date_drilled Date when the borehole was drilled.
#' @param casing_outside_diameter Outside diameter of the casing in centimeters.
#' @param is_well Logical indicating if the borehole is also a well. Default is FALSE.
#' @param well_depth Total depth of the well in meters.
#' @param top_of_screen Depth to the top of the well screen in meters.
#' @param bottom_of_screen Depth to the bottom of the well screen in meters.
#' @param well_head_stick_up Height of the well head above ground in meters.
#' @param static_water_level Static water level measured from the top of the well in meters.
#' @param estimated_yield Estimated yield of the well in liters per minute.
#' @param ground_elev_m Ground elevation in meters.
#' @param notes Additional notes about the borehole/well.
#' @param share_with Which user groups should the record be shared with. Default is "yg_reader".
#' @param drilled_by Company or individual who drilled the borehole.
#' @param drill_method Method used for drilling.
#'
#' @return The borehole_id of the newly inserted record.
#' @export
#'
#' @examples
#' \dontrun{
#' insertACBorehole(
#'   well_name = "Test Well",
#'   latitude = 60.7212,
#'   longitude = -135.0568,
#'   ...
#' )
#' }

insertACBorehole <- function(
  con = NULL,
  path = NULL,
  well_name = NULL,
  latitude = NULL,
  longitude = NULL,
  location_source = NULL,
  surveyed_ground_level_elevation = NULL,
  purpose_of_well = NULL,
  depth_to_bedrock = NULL,
  permafrost_present = FALSE,
  permafrost_top_depth = NULL,
  permafrost_bottom_depth = NULL,
  date_drilled = NULL,
  casing_outside_diameter = NULL,
  is_well = FALSE,
  well_depth = NULL,
  top_of_screen = NULL,
  bottom_of_screen = NULL,
  well_head_stick_up = NULL,
  static_water_level = NULL,
  estimated_yield = NULL,
  ground_elev_m = NULL,
  notes = NULL,
  share_with = "yg_reader",
  drilled_by = NULL,
  drill_method = NULL
) {
  # Insert the new borehole data into the database
  
  # Establish database connection if not provided
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  } else {
    con <- con
  }
  
  # Set timezone to UTC for consistency
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  # Validate 'share_with' parameter type
  if (!inherits(share_with, "character")) {
    stop("The 'share_with' parameter must be a character vector with one element ",
         "per share with group.")
  }
  
  # Validate required inputs
  if (is.null(well_name) || !is.character(well_name)) {
    stop("'well_name' must be a non-NULL character.")
  }
  if (!is.null(location_source) && !is.character(location_source)) {
    stop("'location_source' must be character if provided.")
  }
  if (!is.null(purpose_of_well) && !is.character(purpose_of_well)) {
    stop("'purpose_of_well' must be character if provided.")
  }
  
  # Validate numeric fields
  numeric_fields <- c(
    "depth_to_bedrock", "casing_outside_diameter", "well_depth", 
    "top_of_screen", "bottom_of_screen", "well_head_stick_up", 
    "static_water_level", "estimated_yield", "ground_elev_m",
    "latitude", "longitude", "surveyed_ground_level_elevation"
  )
  for (field in numeric_fields) {
    value <- get(field)
    if (!is.null(value) && !is.numeric(value)) {
      stop(paste0("'", field, "' must be numeric if provided."))
    }
  }
  
  # Validate DB connection object
  if (!is.null(con) && !inherits(con, "DBIConnection")) {
    stop("'con' must be a DBIConnection object if provided.")
  }
  
  # Construct SQL query for borehole insertion
  query <- paste0(
    "INSERT INTO boreholes (share_with, latitude, longitude, borehole_name,",
    "location_source, ground_elevation_m, depth_m, drilled_by, ",
    "drill_method, completion_date, notes) VALUES (",
    "'{", paste(share_with, collapse = ","), "}', ",
    latitude, ", ",
    longitude, ", ",
    ifelse(is.null(well_name), "NULL", paste0("'", well_name, "'")), ", ",
    ifelse(is.null(location_source), "NULL", 
           paste0("'", location_source, "'")), ", ",
    ifelse(is.null(ground_elev_m), "NULL", ground_elev_m), ", ",
    ifelse(is.null(well_depth), "NULL", well_depth), ", ",
    ifelse(is.null(drilled_by), "NULL", paste0("'", drilled_by, "'")), ", ",
    ifelse(is.null(drill_method), "NULL", 
           paste0("'", drill_method, "'")), ", ",
    ifelse(is.null(date_drilled), "NULL", 
           paste0("'", date_drilled, "'")), ", ",
    ifelse(is.null(notes), "NULL", paste0("'", notes, "'"))
    , ") RETURNING borehole_id;"
  )
  # Execute borehole insertion and retrieve new borehole_id
  borehole_id <- DBI::dbGetQuery(con, query)
  
  # If permafrost is present, insert permafrost record
  if (permafrost_present) {
    query <- paste0(
      "INSERT INTO permafrost (borehole_id, depth_from_m, ",
      "depth_to_m) VALUES (",
      "'", borehole_id, "', ",
      ifelse(is.na(permafrost_top_depth), "NULL", permafrost_top_depth), ", ",
      ifelse(is.na(permafrost_bottom_depth), "NULL", permafrost_bottom_depth),
      ")"
    )
    DBI::dbExecute(con, query)
  }
  

  # If borehole is a well, insert well-specific data
  if (is_well) {
    query <- paste0(
      "INSERT INTO well (borehole_id, casing_outside_diameter, well_depth, ",
      "top_of_screen, bottom_of_screen, well_head_stick_up, ",
      "static_water_level, estimated_yield) VALUES (",
      "'", borehole_id, "', ",
      ifelse(is.null(casing_outside_diameter), "NULL", 
             casing_outside_diameter), ", ",
      ifelse(is.null(well_depth), "NULL", well_depth), ", ",
      ifelse(is.null(top_of_screen), "NULL", top_of_screen), ", ",
      ifelse(is.null(bottom_of_screen), "NULL", bottom_of_screen), ", ",
      ifelse(is.null(well_head_stick_up), "NULL", well_head_stick_up), ", ",
      ifelse(is.null(static_water_level), "NULL", static_water_level), ", ",
      ifelse(is.null(estimated_yield), "NULL", estimated_yield),
      ")"
    )
    DBI::dbExecute(con, query)
  }
  
  # Determine document type based on is_well flag
  document_type <- if(is_well) "well log" else "borehole log"
  
  # Insert document metadata using insertACDocument
  insertACDocument(
    path = path,
    type = document_type,
    name = paste0("Document for ", well_name),
    description = paste0(document_type, " for ", well_name),
    tags = unlist(strsplit(document_type, " "))
  )
  
  # Return the new borehole_id
  return(borehole_id)
}
