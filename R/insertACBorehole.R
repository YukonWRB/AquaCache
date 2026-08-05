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
#' @param location_id Optional location ID if the borehole is associated with a predefined location.
#' @param latitude The latitude coordinate of the borehole location. Required.
#' @param longitude The longitude coordinate of the borehole location. Required.
#' @param location_source Source of the location information (e.g., "GPS", "Survey").
#' @param surveyed_ground_elev Ground elevation from survey in meters.
#' @param purpose_of_borehole Purpose of the borehole as integer matching the database's borehole_well_purpose column.
#' @param purpose_borehole_inferred Logical indicating if the purpose of the borehole is inferred (TRUE) or explicit in documentation (FALSE). Default is FALSE.
#' @param bedrock_reached Logical indicating if bedrock was reached during drilling. Default is NULL (unknown).
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
#' @param drill_method Drilling-method ID from `boreholes.drill_methods`, or an
#'   English or French method name already present in that table. Text matching
#'   is case-insensitive and ignores surrounding whitespace.
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
  location_id = NULL,
  latitude = NULL,
  longitude = NULL,
  location_source = NULL,
  surveyed_ground_elev = NULL,
  purpose_of_borehole = NULL,
  purpose_borehole_inferred = FALSE,
  bedrock_reached = NULL,
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

  # Resolve either a catalogue key or an existing English/French method name.
  if (!is.null(drill_method)) {
    if (is.numeric(drill_method)) {
      if (
        length(drill_method) != 1L ||
          is.na(drill_method) ||
          !is.finite(drill_method) ||
          drill_method <= 0 ||
          drill_method != floor(drill_method) ||
          drill_method > .Machine$integer.max
      ) {
        stop("'drill_method' must be one positive integer ID or one method name.")
      }
      drill_method <- as.integer(drill_method)
      drill_method_match <- DBI::dbGetQuery(
        con,
        "SELECT drill_method_id
         FROM boreholes.drill_methods
         WHERE drill_method_id = $1",
        params = list(drill_method)
      )
    } else if (is.character(drill_method) && length(drill_method) == 1L) {
      drill_method <- trimws(drill_method)
      if (is.na(drill_method) || !nzchar(drill_method)) {
        stop("'drill_method' cannot be blank when provided.")
      }
      drill_method_match <- DBI::dbGetQuery(
        con,
        "SELECT drill_method_id
         FROM boreholes.drill_methods
         WHERE lower(method_name) = lower($1)
            OR lower(method_name_fr) = lower($1)",
        params = list(drill_method)
      )
    } else {
      stop("'drill_method' must be one positive integer ID or one method name.")
    }

    if (nrow(drill_method_match) != 1L) {
      stop(
        "The specified 'drill_method' does not match exactly one entry in boreholes.drill_methods."
      )
    }
    drill_method <- drill_method_match$drill_method_id[[1]]
  }

  # Validate location_id if provided
  if (!is.null(location_id)) {
    # Check if location_id exists in the database
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM public.locations WHERE location_id = $1;",
      params = list(location_id)
    )[1, 1]
    if (is.na(exists)) {
      stop("The specified 'location_id' does not exist in the database.")
    }
  }

  # Validate latitude and longitude
  if (is.null(latitude) || !is.numeric(latitude)) {
    stop("'latitude' must be a non-NULL numeric value.")
  }
  if (is.null(longitude) || !is.numeric(longitude)) {
    stop("'longitude' must be a non-NULL numeric value.")
  }

  # Validate permafrost parameters
  if (!is.logical(permafrost_present) || length(permafrost_present) != 1) {
    stop("'permafrost_present' must be a single logical value (TRUE or FALSE).")
  }
  if (permafrost_present) {
    if (is.null(permafrost_top) || !is.numeric(permafrost_top)) {
      stop(
        "'permafrost_top' must be provided as a numeric value when 'permafrost_present' is TRUE."
      )
    }
    if (is.null(permafrost_bot) || !is.numeric(permafrost_bot)) {
      stop(
        "'permafrost_bot' must be provided as a numeric value when 'permafrost_present' is TRUE."
      )
    }
    if (permafrost_bot <= permafrost_top) {
      stop("'permafrost_bot' must be greater than 'permafrost_top'.")
    }
  }

  # Validate purpose_of_borehole and purpose_of_well if provided
  if (!is.null(purpose_of_borehole)) {
    # Check if purpose of borehole exists in the database
    exists <- DBI::dbGetQuery(
      con,
      "SELECT borehole_well_purpose_id
       FROM boreholes.borehole_well_purposes
       WHERE borehole_well_purpose_id = $1;",
      params = list(purpose_of_borehole)
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
      "SELECT borehole_well_purpose_id
       FROM boreholes.borehole_well_purposes
       WHERE borehole_well_purpose_id = $1;",
      params = list(purpose_of_well)
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

  # Validate bedrock_reached and depth_to_bedrock
  if (!is.null(depth_to_bedrock) && !is.numeric(depth_to_bedrock)) {
    stop("'depth_to_bedrock' must be numeric if provided.")
  }
  if (!is.null(bedrock_reached) && !is.logical(bedrock_reached)) {
    stop("'bedrock_reached' must be logical if provided.")
  }
  if (
    !is.null(bedrock_reached) && bedrock_reached && is.null(depth_to_bedrock)
  ) {
    stop(
      "'depth_to_bedrock' must be provided if 'bedrock_reached' is TRUE."
    )
  }
  if (
    !is.null(bedrock_reached) && !bedrock_reached && !is.null(depth_to_bedrock)
  ) {
    stop(
      "'depth_to_bedrock' should not be provided if 'bedrock_reached' is FALSE."
    )
  }
  if (
    !is.null(bedrock_reached) &&
      is.null(depth_to_bedrock) &&
      !is.null(depth_to_bedrock)
  ) {
    stop(
      "'depth_to_bedrock' should not be provided if 'bedrock_reached' is NULL."
    )
  }
  if (!is.null(depth_to_bedrock) && depth_to_bedrock < 0) {
    stop("'depth_to_bedrock' must be a non-negative numeric value.")
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

  # Bound parameters preserve quotes and other special characters in text fields.
  query <-
    "INSERT INTO boreholes.boreholes (
      share_with,
      location_id,
      latitude,
      longitude,
      borehole_name,
      location_source,
      ground_elevation_m,
      depth_m,
      bedrock_reached,
      depth_to_bedrock_m,
      drilled_by,
      drill_method,
      completion_date,
      notes,
      borehole_purpose_id,
      inferred_purpose)
    VALUES (
      $1::text[], $2, $3, $4, $5, $6, $7, $8,
      $9, $10, $11, $12, $13, $14, $15, $16
    )
    RETURNING borehole_id;"

  borehole_params <- list(
    paste0("{", paste(share_with_borehole, collapse = ","), "}"),
    if (is.null(location_id)) NA_integer_ else location_id,
    latitude,
    longitude,
    well_name,
    if (is.null(location_source)) NA_character_ else location_source,
    if (is.null(ground_elev_m)) NA_real_ else ground_elev_m,
    if (is.null(well_depth)) NA_real_ else well_depth,
    if (is.null(bedrock_reached)) NA else bedrock_reached,
    if (is.null(depth_to_bedrock)) NA_real_ else depth_to_bedrock,
    if (is.null(drilled_by)) NA_character_ else drilled_by,
    if (is.null(drill_method)) NA_integer_ else drill_method,
    if (is.null(date_drilled)) NA_character_ else date_drilled,
    if (is.null(notes_borehole)) NA_character_ else notes_borehole,
    if (is.null(purpose_of_borehole)) NA_integer_ else purpose_of_borehole,
    purpose_borehole_inferred
  )
  # Execute borehole insertion and retrieve new borehole_id
  borehole_id <- DBI::dbGetQuery(con, query, params = borehole_params)[1, 1]

  # If permafrost is present, insert permafrost record
  if (permafrost_present) {
    DBI::dbExecute(
      con,
      "INSERT INTO boreholes.permafrost (
        borehole_id,
        depth_from_m,
        depth_to_m)
      VALUES ($1, $2, $3)",
      params = list(borehole_id, permafrost_top, permafrost_bot)
    )
  }

  # If borehole is a well, insert well-specific data
  if (is_well) {
    DBI::dbExecute(
      con,
      "INSERT INTO boreholes.wells (
        borehole_id,
        casing_diameter_mm,
        screen_top_depth_m,
        screen_bottom_depth_m,
        stick_up_height_m,
        static_water_level_m,
        estimated_yield_lps,
        well_purpose_id,
        inferred_purpose,
        notes,
        share_with)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11::text[])",
      params = list(
        borehole_id,
        if (is.null(casing_od)) NA_real_ else casing_od,
        if (is.null(top_of_screen)) NA_real_ else top_of_screen,
        if (is.null(bottom_of_screen)) NA_real_ else bottom_of_screen,
        if (is.null(well_head_stick_up)) NA_real_ else well_head_stick_up,
        if (is.null(static_water_level)) NA_real_ else static_water_level,
        if (is.null(estimated_yield)) NA_real_ else estimated_yield,
        if (is.null(purpose_of_well)) NA_integer_ else purpose_of_well,
        purpose_well_inferred,
        if (is.null(notes_well)) NA_character_ else notes_well,
        paste0("{", paste(share_with_well, collapse = ","), "}")
      )
    )
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
