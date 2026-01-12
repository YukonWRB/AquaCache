#' Add a borehole record to the aquacache database
#'
#'@description
#'
#' This function facilitates the addition of a borehole record to the database. If the borehole
#' is also a well, additional well-specific information can be provided. The function can also
#' associate a document with the borehole and handle permafrost information if present.
#'
#' @param con A connection to the database. Default NULL uses AquaConnect() and closes the connection afterwards.
#' @param path Path to a document/file to attach to the borehole record. If NULL, no document is attached.
#' @param well_name Name of the borehole/well. Required.
#' @param latitude The latitude coordinate of the borehole location. Required.
#' @param longitude The longitude coordinate of the borehole location. Required.
#' @param location_source Source of the location information (e.g., "GPS", "Survey").
#' @param surveyed_ground_elev Ground elevation from survey in meters.
#' @param purpose_of_borehole Purpose of the borehole as integer matching the database's borehole_well_purpose column.
#' @param purpose_borehole_inferred Logical indicating if the purpose of the borehole is inferred (TRUE) or explicit in documentation (FALSE). Default is FALSE.
#' @param depth_to_bedrock Depth to bedrock in meters.
#' @param permafrost_present Logical indicating if permafrost is present. Default is FALSE.
#' @param permafrost_top Depth to the top of permafrost in meters, if present.
#' @param permafrost_bot Depth to the bottom of permafrost in meters, if present.
#' @param date_drilled Date when the borehole was drilled.
#' @param casing_od Outside diameter of the casing in milimeters
#' @param is_well Logical indicating if the borehole is also a well. Default is FALSE.
#' @param well_depth Total depth of the well in meters.
#' @param top_of_screen Depth to the top of the well screen in meters.
#' @param bottom_of_screen Depth to the bottom of the well screen in meters.
#' @param well_head_stick_up Height of the well head above ground in meters.
#' @param static_water_level Static water level measured from the top of the well in meters.
#' @param estimated_yield Estimated yield of the well in liters per minute.
#' @param ground_elev_m Ground elevation in meters.
#' @param notes_borehole Additional notes about the borehole.
#' @param notes_well Additional notes about the well.
#' @param share_with_borehole A character vector of the user group(s) with which to share the borehole, one element per group. Default is "public_reader".
#' @param drilled_by Company or individual who drilled the borehole.
#' @param drill_method Method used for drilling.
#' @param purpose_of_well Purpose of the borehole as integer matching the database's borehole_well_purpose column. Default is `purpose_of_borehole`.
#' @param purpose_well_inferred Logical indicating if the purpose of the borehole is inferred (TRUE) or explicit in documentation (FALSE). Default is `purpose_borehole_inferred`.
#' @param share_with_well A character vector of the user group(s) with which to share the well, one elemtn per group. Default is `share_with_borehole`.
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
  surveyed_ground_elev = NULL,
  purpose_of_borehole = NULL,
  purpose_borehole_inferred = FALSE,
  depth_to_bedrock = NULL,
  permafrost_present = FALSE,
  permafrost_top = NULL,
  permafrost_bot = NULL,
  date_drilled = NULL,
  casing_od = NULL,
  is_well = FALSE,
  well_depth = NULL,
  top_of_screen = NULL,
  bottom_of_screen = NULL,
  well_head_stick_up = NULL,
  static_water_level = NULL,
  estimated_yield = NULL,
  ground_elev_m = NULL,
  notes_borehole = NULL,
  notes_well = NULL,
  share_with_borehole = "public_reader",
  drilled_by = NULL,
  drill_method = NULL,
  purpose_of_well = purpose_of_borehole,
  purpose_well_inferred = purpose_borehole_inferred,
  share_with_well = share_with_borehole
) {
  # Establish database connection if not provided
  if (is.null(con)) {
    print("insertACBorehole: Connection was NULL, creating new connection")
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  # Set timezone to UTC for consistency
  DBI::dbExecute(con, "SET timezone = 'UTC'")

  # Validate 'share_with' parameter type
  if (!inherits(share_with_borehole, "character")) {
    stop(
      "The 'share_with_borehole' parameter must be a character vector with one element per share with group."
    )
  }
  if (!inherits(share_with_well, "character")) {
    stop(
      "The 'share_with_well' parameter must be a character vector with one element per share with group."
    )
  }

  # Validate required inputs
  if (is.null(well_name) || !is.character(well_name)) {
    stop("'well_name' must be a non-NULL character.")
  }
  if (!is.null(location_source) && !is.character(location_source)) {
    stop("'location_source' must be character if provided.")
  }

  # Validate purpose_of_borehole and purpose_of_well if provided
  if (!is.null(purpose_of_borehole)) {
    # Check if purpose of borehole exists in the database
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT borehole_well_purpose_id FROM borehole_well_purposes WHERE borehole_well_purpose_id = ",
        purpose_of_borehole,
        ";"
      )
    )[1, 1]
    if (is.na(exists)) {
      stop(
        "The specified 'purpose_of_borehole' does not exist in the database."
      )
    }
  }
  if (!is.null(purpose_of_well)) {
    # Check if purpose of borehole exists in the database
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT borehole_well_purpose_id FROM borehole_well_purposes WHERE borehole_well_purpose_id = ",
        purpose_of_well,
        ";"
      )
    )[1, 1]
    if (is.na(exists)) {
      stop("The specified 'purpose_of_well' does not exist in the database.")
    }
  }
  # Validate inferred purpose flags
  if (
    !is.logical(purpose_borehole_inferred) ||
      length(purpose_borehole_inferred) != 1
  ) {
    stop(
      "'purpose_borehole_inferred' must be a single logical value (TRUE or FALSE)."
    )
  }
  if (
    !is.logical(purpose_well_inferred) || length(purpose_well_inferred) != 1
  ) {
    stop(
      "'purpose_well_inferred' must be a single logical value (TRUE or FALSE)."
    )
  }

  # Validate numeric fields
  numeric_fields <- c(
    "depth_to_bedrock",
    "casing_od",
    "well_depth",
    "top_of_screen",
    "bottom_of_screen",
    "well_head_stick_up",
    "static_water_level",
    "estimated_yield",
    "ground_elev_m",
    "latitude",
    "longitude",
    "surveyed_ground_elev",
    "permafrost_top",
    "permafrost_bot"
  )
  for (field in numeric_fields) {
    value <- get(field)
    if (!is.null(value) && !is.numeric(value)) {
      stop(paste0("'", field, "' must be numeric if provided."))
    }
  }

  # Construct SQL query for borehole insertion
  query <- paste0(
    "INSERT INTO boreholes (share_with, latitude, longitude, borehole_name,",
    "location_source, ground_elevation_m, depth_m, depth_to_bedrock_m, drilled_by, ",
    "drill_method, completion_date, notes, borehole_purpose_id, inferred_purpose) VALUES (",
    "'{",
    paste(share_with_borehole, collapse = ","),
    "}', ",
    latitude,
    ", ",
    longitude,
    ", ",
    ifelse(is.null(well_name), "NULL", paste0("'", well_name, "'")),
    ", ",
    ifelse(is.null(location_source), "NULL", paste0("'", location_source, "'")),
    ", ",
    ifelse(is.null(ground_elev_m), "NULL", ground_elev_m),
    ", ",
    ifelse(is.null(well_depth), "NULL", well_depth),
    ", ",
    ifelse(is.null(depth_to_bedrock), "NULL", depth_to_bedrock),
    ", ",
    ifelse(is.null(drilled_by), "NULL", paste0("'", drilled_by, "'")),
    ", ",
    ifelse(is.null(drill_method), "NULL", paste0("'", drill_method, "'")),
    ", ",
    ifelse(is.null(date_drilled), "NULL", paste0("'", date_drilled, "'")),
    ", ",
    ifelse(is.null(notes_borehole), "NULL", paste0("'", notes_borehole, "'")),
    ", ",
    ifelse(
      is.null(purpose_of_borehole),
      "NULL",
      paste0("'", purpose_of_borehole, "'")
    ),
    ", ",
    purpose_borehole_inferred,
    ") RETURNING borehole_id;"
  )
  # Execute borehole insertion and retrieve new borehole_id
  borehole_id <- DBI::dbGetQuery(con, query)[1, 1]

  # If permafrost is present, insert permafrost record
  if (permafrost_present) {
    query <- paste0(
      "INSERT INTO permafrost (borehole_id, depth_from_m, ",
      "depth_to_m) VALUES (",
      "'",
      borehole_id,
      "', ",
      ifelse(is.na(permafrost_top), "NULL", permafrost_top),
      ", ",
      ifelse(is.na(permafrost_bot), "NULL", permafrost_bot),
      ")"
    )
    DBI::dbExecute(con, query)
  }

  # If borehole is a well, insert well-specific data
  if (is_well) {
    query <- paste0(
      "INSERT INTO wells (borehole_id, casing_diameter_mm, ",
      "screen_top_depth_m, screen_bottom_depth_m, stick_up_height_m, ",
      "static_water_level_m, estimated_yield_lps, well_purpose_id, inferred_purpose, notes, share_with) VALUES (",
      "",
      borehole_id,
      ", ",
      ifelse(is.null(casing_od), "NULL", casing_od),
      ", ",
      ifelse(is.null(top_of_screen), "NULL", top_of_screen),
      ", ",
      ifelse(is.null(bottom_of_screen), "NULL", bottom_of_screen),
      ", ",
      ifelse(is.null(well_head_stick_up), "NULL", well_head_stick_up),
      ", ",
      ifelse(is.null(static_water_level), "NULL", static_water_level),
      ", ",
      ifelse(is.null(estimated_yield), "NULL", estimated_yield),
      ", ",
      ifelse(
        is.null(purpose_of_well),
        "NULL",
        paste0("'", purpose_of_well, "'")
      ),
      ", ",
      purpose_well_inferred,
      ", ",
      ifelse(is.null(notes_well), "NULL", paste0("'", notes_well, "'")),
      ", ",
      "'{",
      paste(share_with_well, collapse = ","),
      "}'",
      ")"
    )
    DBI::dbExecute(con, query)
  }

  # Insert document metadata using insertACDocument
  if (!is.null(path)) {
    # Determine document type based on is_well flag
    document_type <- if (is_well) "well log" else "borehole log"
    res <- insertACDocument(
      con = con,
      path = path,
      type = document_type,
      name = paste0("Document for borehole/well", well_name),
      description = paste0(document_type, " for borehole/well ", well_name),
      tags = unlist(strsplit(document_type, " "))
    )
    # use res$new_document_id to link document to borehole
    DBI::dbExecute(
      con,
      "INSERT INTO boreholes.boreholes_documents (borehole_id, document_id) VALUES ($1, $2);",
      params = list(borehole_id, res$new_document_id)
    )
  }
  # Return the new borehole_id
  return(borehole_id)
}
