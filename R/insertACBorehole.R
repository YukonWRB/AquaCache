#' Add a borehole record to the aquacache database
#'
#'@description
#'
#' This function facilitates the addition of a borehole record to the database. If the borehole
#' is also a well, additional well-specific information can be provided. The function can also
#' associate a document with the borehole and handle permafrost information if present.
#' Supply one `well_name` per well. Other well-specific arguments may contain
#' either one value, which is recycled, or one value per well.
#'
#' @param con A connection to the database. Default NULL uses AquaConnect() and closes the connection afterwards.
#' @param path Path to a document/file to attach to the borehole record. If NULL, no document is attached.
#' @param document_name Optional name for the attached document. When omitted,
#'   the name defaults to `"Document for borehole/well <borehole_name>"`.
#' @param well_name Character vector containing one name per well when
#'   `is_well = TRUE`. For backward compatibility, the first value is also the
#'   default `borehole_name`.
#' @param borehole_name Name of the borehole. Defaults to the first element of
#'   `well_name`, preserving the historical one-to-one borehole/well naming
#'   behaviour.
#' @param location_id Optional location ID if the borehole is associated with a predefined location.
#' @param latitude The latitude coordinate of the borehole location. Required.
#' @param longitude The longitude coordinate of the borehole location. Required.
#' @param location_source Source of the location information (e.g., "GPS", "Survey").
#' @param surveyed_ground_elev Ground elevation from survey in meters.
#' @param purpose_of_borehole Purpose of the borehole as integer matching the database's borehole_well_purpose column.
#' @param purpose_borehole_inferred Logical indicating if the purpose of the borehole is inferred (TRUE) or explicit in documentation (FALSE). Default is FALSE.
#' @param bedrock_reached Logical indicating if bedrock was reached during drilling. Default is NULL (unknown).
#' @param depth_to_bedrock Depth to bedrock in meters. `NULL` or the character
#'   value `"Unknown"` records an unknown depth.
#' @param permafrost_present Logical indicating if permafrost is present. Default is FALSE.
#' @param permafrost_top Depth to the top of permafrost in meters, if present.
#' @param permafrost_bot Depth to the bottom of permafrost in meters, if present.
#' @param date_drilled Date when the borehole was drilled.
#' @param casing_od Outside diameter of each well casing in millimeters. A
#'   scalar is recycled across wells.
#' @param is_well Logical indicating if the borehole is also a well. Default is FALSE.
#' @param well_depth Total depth of the well in meters.
#' @param top_of_screen Depth to the top of each well screen in meters. A
#'   scalar is recycled across wells.
#' @param bottom_of_screen Depth to the bottom of each well screen in meters. A
#'   scalar is recycled across wells.
#' @param seal_material Seal-material ID from `boreholes.seal_materials`, or an
#'   English or French material name already present in that table. A scalar is
#'   recycled across wells.
#' @param seal_diameter_mm Outside diameter of the annular seal in millimeters.
#' @param seal_depth_from Depth to the top of the seal in meters below ground.
#' @param seal_depth_to Depth to the bottom of the seal in meters below ground.
#' @param screen_material Screen-material ID from
#'   `boreholes.screen_materials`, or an English or French material name
#'   already present in that table. A scalar is recycled across wells.
#' @param screen_type Screen-type ID from `boreholes.screen_types`, or an
#'   English or French type name already present in that table. A scalar is
#'   recycled across wells.
#' @param well_head_stick_up Height of each well head above ground in meters.
#' @param static_water_level Static water level measured from the top of each
#'   well in meters.
#' @param estimated_yield Estimated yield of each well in liters per second.
#' @param ground_elev_m Ground elevation in meters.
#' @param notes_borehole Additional notes about the borehole.
#' @param notes_well Additional notes for each well.
#' @param share_with_borehole A character vector of the user group(s) with which to share the borehole, one element per group. Default is "public_reader".
#' @param drilled_by Company or individual who drilled the borehole.
#' @param drill_method Drilling-method ID from `boreholes.drill_methods`, or an
#'   English or French method name already present in that table. Text matching
#'   is case-insensitive and ignores surrounding whitespace.
#' @param purpose_of_well Purpose of each well as an integer matching the
#'   database's borehole_well_purpose column. A scalar is recycled across wells.
#' @param purpose_well_inferred Logical vector indicating whether each well
#'   purpose is inferred. A scalar is recycled across wells.
#' @param share_with_well A character vector of groups applied to every well,
#'   or a list containing one character vector of groups per well.
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
  share_with_well = share_with_borehole,
  seal_material = NULL,
  seal_diameter_mm = NULL,
  seal_depth_from = NULL,
  seal_depth_to = NULL,
  screen_material = NULL,
  screen_type = NULL,
  borehole_name = well_name[1],
  document_name = NULL
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
  if (!is.character(share_with_well) && !is.list(share_with_well)) {
    stop(
      "The 'share_with_well' parameter must be a character vector applied to every well or a list of character vectors with one element per well."
    )
  }

  # Validate required inputs
  if (
    is.null(borehole_name) ||
      !is.character(borehole_name) ||
      length(borehole_name) != 1L ||
      is.na(borehole_name) ||
      !nzchar(trimws(borehole_name))
  ) {
    stop("'borehole_name' must be one non-blank character value.")
  }
  if (!is.null(path)) {
    if (is.null(document_name)) {
      document_name <- paste0("Document for borehole/well ", borehole_name)
    } else if (
      !is.character(document_name) ||
        length(document_name) != 1L ||
        is.na(document_name) ||
        !nzchar(trimws(document_name))
    ) {
      stop("'document_name' must be one non-empty character value if provided.")
    } else {
      document_name <- trimws(document_name)
    }
  }
  if (!is.logical(is_well) || length(is_well) != 1L || is.na(is_well)) {
    stop("'is_well' must be one non-missing logical value.")
  }
  if (
    isTRUE(is_well) &&
      (is.null(well_name) || !is.character(well_name) || !length(well_name))
  ) {
    stop("'well_name' must contain one name per well when 'is_well' is TRUE.")
  }
  if (!is.null(location_source) && !is.character(location_source)) {
    stop("'location_source' must be character if provided.")
  }

  resolve_catalogue_value <- function(
    value,
    table,
    id_column,
    name_column,
    name_fr_column,
    argument
  ) {
    if (is.null(value)) {
      return(NULL)
    }

    if (is.numeric(value)) {
      if (
        length(value) != 1L ||
          is.na(value) ||
          !is.finite(value) ||
          value <= 0 ||
          value != floor(value) ||
          value > .Machine$integer.max
      ) {
        stop(
          sprintf(
            "'%s' must be one positive integer ID or one catalogue name.",
            argument
          )
        )
      }
      value <- as.integer(value)
      match <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT %s FROM boreholes.%s WHERE %s = $1",
          id_column,
          table,
          id_column
        ),
        params = list(value)
      )
    } else if (is.character(value) && length(value) == 1L) {
      value <- trimws(value)
      if (is.na(value) || !nzchar(value)) {
        stop(sprintf("'%s' cannot be blank when provided.", argument))
      }
      match <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT %s
             FROM boreholes.%s
            WHERE lower(%s) = lower($1)
               OR lower(%s) = lower($1)",
          id_column,
          table,
          name_column,
          name_fr_column
        ),
        params = list(value)
      )
    } else {
      stop(
        sprintf(
          "'%s' must be one positive integer ID or one catalogue name.",
          argument
        )
      )
    }

    if (nrow(match) != 1L) {
      stop(
        sprintf(
          "The specified '%s' does not match exactly one entry in boreholes.%s.",
          argument,
          table
        )
      )
    }

    as.integer(match[[id_column]][[1]])
  }

  well_count <- if (isTRUE(is_well)) length(well_name) else 0L
  if (well_count) {
    well_name <- trimws(well_name)
    if (anyNA(well_name) || any(!nzchar(well_name))) {
      stop("'well_name' must contain one non-blank name per well.")
    }
    if (anyDuplicated(tolower(well_name))) {
      stop("'well_name' values must be unique within a borehole.")
    }
  } else {
    well_name <- character()
  }

  normalize_well_vector <- function(value, argument, type, missing_value) {
    if (!well_count) {
      return(rep(missing_value, 0L))
    }
    if (is.null(value)) {
      return(rep(missing_value, well_count))
    }

    valid_type <- switch(
      type,
      numeric = is.numeric(value),
      character = is.character(value),
      logical = is.logical(value)
    )
    if (!valid_type || !(length(value) %in% c(1L, well_count))) {
      stop(
        sprintf(
          "'%s' must be %s with length one or the number of wells (%d).",
          argument,
          type,
          well_count
        )
      )
    }
    rep(value, length.out = well_count)
  }

  normalize_catalogue_values <- function(
    value,
    table,
    id_column,
    name_column,
    name_fr_column,
    argument
  ) {
    if (!well_count) {
      return(integer())
    }
    if (is.null(value)) {
      return(rep(NA_integer_, well_count))
    }
    if (!(is.numeric(value) || is.character(value)) ||
        !(length(value) %in% c(1L, well_count))) {
      stop(
        sprintf(
          "'%s' must contain one catalogue value or one value per well (%d).",
          argument,
          well_count
        )
      )
    }
    value <- rep(value, length.out = well_count)
    vapply(
      value,
      function(item) {
        if (is.na(item)) {
          return(NA_integer_)
        }
        resolve_catalogue_value(
          item,
          table = table,
          id_column = id_column,
          name_column = name_column,
          name_fr_column = name_fr_column,
          argument = argument
        )
      },
      integer(1)
    )
  }

  drill_method <- resolve_catalogue_value(
    drill_method,
    table = "drill_methods",
    id_column = "drill_method_id",
    name_column = "method_name",
    name_fr_column = "method_name_fr",
    argument = "drill_method"
  )
  seal_material <- normalize_catalogue_values(
    seal_material,
    table = "seal_materials",
    id_column = "seal_material_id",
    name_column = "material_name",
    name_fr_column = "material_name_fr",
    argument = "seal_material"
  )
  screen_material <- normalize_catalogue_values(
    screen_material,
    table = "screen_materials",
    id_column = "screen_material_id",
    name_column = "material_name",
    name_fr_column = "material_name_fr",
    argument = "screen_material"
  )
  screen_type <- normalize_catalogue_values(
    screen_type,
    table = "screen_types",
    id_column = "screen_type_id",
    name_column = "type_name",
    name_fr_column = "type_name_fr",
    argument = "screen_type"
  )

  casing_od <- normalize_well_vector(
    casing_od, "casing_od", "numeric", NA_real_
  )
  top_of_screen <- normalize_well_vector(
    top_of_screen, "top_of_screen", "numeric", NA_real_
  )
  bottom_of_screen <- normalize_well_vector(
    bottom_of_screen, "bottom_of_screen", "numeric", NA_real_
  )
  well_head_stick_up <- normalize_well_vector(
    well_head_stick_up, "well_head_stick_up", "numeric", NA_real_
  )
  static_water_level <- normalize_well_vector(
    static_water_level, "static_water_level", "numeric", NA_real_
  )
  estimated_yield <- normalize_well_vector(
    estimated_yield, "estimated_yield", "numeric", NA_real_
  )
  purpose_of_well <- normalize_well_vector(
    purpose_of_well, "purpose_of_well", "numeric", NA_integer_
  )
  purpose_well_inferred <- normalize_well_vector(
    purpose_well_inferred,
    "purpose_well_inferred",
    "logical",
    FALSE
  )
  notes_well <- normalize_well_vector(
    notes_well, "notes_well", "character", NA_character_
  )
  seal_diameter_mm <- normalize_well_vector(
    seal_diameter_mm, "seal_diameter_mm", "numeric", NA_real_
  )
  seal_depth_from <- normalize_well_vector(
    seal_depth_from, "seal_depth_from", "numeric", NA_real_
  )
  seal_depth_to <- normalize_well_vector(
    seal_depth_to, "seal_depth_to", "numeric", NA_real_
  )

  if (!well_count) {
    share_with_well <- list()
  } else if (is.character(share_with_well)) {
    share_with_well <- rep(list(share_with_well), well_count)
  } else {
    if (!(length(share_with_well) %in% c(1L, well_count))) {
      stop(
        sprintf(
          "'share_with_well' must contain one group vector or one per well (%d).",
          well_count
        )
      )
    }
    share_with_well <- rep(share_with_well, length.out = well_count)
    if (any(!vapply(share_with_well, is.character, logical(1)))) {
      stop("Every 'share_with_well' list element must be a character vector.")
    }
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
  for (well_purpose in unique(purpose_of_well[!is.na(purpose_of_well)])) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT borehole_well_purpose_id
       FROM boreholes.borehole_well_purposes
       WHERE borehole_well_purpose_id = $1;",
      params = list(well_purpose)
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
  if (well_count && anyNA(purpose_well_inferred)) {
    stop(
      "'purpose_well_inferred' cannot contain missing values."
    )
  }

  # Validate bedrock_reached and depth_to_bedrock
  if (
    is.character(depth_to_bedrock) &&
      length(depth_to_bedrock) == 1L &&
      (
        is.na(depth_to_bedrock) ||
          identical(tolower(trimws(depth_to_bedrock)), "unknown")
      )
  ) {
    depth_to_bedrock <- NULL
  }
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
    is.null(bedrock_reached) &&
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
    "permafrost_bot",
    "seal_diameter_mm",
    "seal_depth_from",
    "seal_depth_to"
  )
  for (field in numeric_fields) {
    value <- get(field)
    if (!is.null(value) && !is.numeric(value)) {
      stop(paste0("'", field, "' must be numeric if provided."))
    }
  }

  nonnegative_fields <- c(
    "top_of_screen",
    "bottom_of_screen",
    "seal_depth_from",
    "seal_depth_to"
  )
  for (field in nonnegative_fields) {
    value <- get(field)
    if (!is.null(value) && any(value < 0, na.rm = TRUE)) {
      stop(paste0("'", field, "' must be non-negative if provided."))
    }
  }
  if (!is.null(seal_diameter_mm) && any(seal_diameter_mm <= 0, na.rm = TRUE)) {
    stop("'seal_diameter_mm' must be greater than zero if provided.")
  }
  if (
    any(
      !is.na(top_of_screen) &
        !is.na(bottom_of_screen) &
        bottom_of_screen < top_of_screen
    )
  ) {
    stop("'bottom_of_screen' must be greater than or equal to 'top_of_screen'.")
  }
  if (
    any(
      !is.na(seal_depth_from) &
        !is.na(seal_depth_to) &
        seal_depth_to < seal_depth_from
    )
  ) {
    stop("'seal_depth_to' must be greater than or equal to 'seal_depth_from'.")
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
    borehole_name,
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
  if (well_count) {
    well_query <-
      "INSERT INTO boreholes.wells (
        borehole_id,
        well_name,
        casing_diameter_mm,
        screen_top_depth_m,
        screen_bottom_depth_m,
        stick_up_height_m,
        static_water_level_m,
        estimated_yield_lps,
        well_purpose_id,
        inferred_purpose,
        notes,
        share_with,
        seal_material_id,
        seal_diameter_mm,
        seal_depth_from_m,
        seal_depth_to_m,
        screen_material_id,
        screen_type_id)
      VALUES (
        $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12::text[],
        $13, $14, $15, $16, $17, $18
      )"

    for (well_index in seq_len(well_count)) {
      DBI::dbExecute(
        con,
        well_query,
        params = list(
          borehole_id,
          well_name[[well_index]],
          casing_od[[well_index]],
          top_of_screen[[well_index]],
          bottom_of_screen[[well_index]],
          well_head_stick_up[[well_index]],
          static_water_level[[well_index]],
          estimated_yield[[well_index]],
          purpose_of_well[[well_index]],
          purpose_well_inferred[[well_index]],
          notes_well[[well_index]],
          paste0(
            "{",
            paste(share_with_well[[well_index]], collapse = ","),
            "}"
          ),
          seal_material[[well_index]],
          seal_diameter_mm[[well_index]],
          seal_depth_from[[well_index]],
          seal_depth_to[[well_index]],
          screen_material[[well_index]],
          screen_type[[well_index]]
        )
      )
    }
  }

  # Insert document metadata using insertACDocument
  if (!is.null(path)) {
    # Determine document type based on is_well flag
    document_type <- if (is_well) "well log" else "borehole log"
    res <- insertACDocument(
      con = con,
      path = path,
      type = document_type,
      name = document_name,
      description = paste0(document_type, " for borehole/well ", borehole_name),
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
