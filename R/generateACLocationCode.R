#' Generate location codes from National Hydro Network polygons
#'
#' Generates location codes for new locations based on the National Hydro Network
#' drainage basins polygons, downloading the layer if it is missing from the
#' database. Location codes use the first two digits and two or three letters of
#' the polygon name, the location type suffix, and a numeric sequence.
#'
#' @param latitude Numeric vector of latitude values in decimal degrees.
#' @param longitude Numeric vector of longitude values in decimal degrees.
#' @param location_type Numeric vector of location type IDs from the
#'   `location_types` table.
#' @param con A connection to the aquacache database. Default uses
#'   [AquaConnect()]. If left `NULL` the function will attempt to connect to the
#'   database and automatically disconnect afterwards.
#' @param ask Logical. If `TRUE` and running interactively, prompt before
#'   downloading National Hydro Network basins when missing. If `FALSE` or
#'   non-interactive, download automatically.
#'
#' @return A character vector of generated location codes.
#' @export
generateACLocationCode <- function(
  latitude,
  longitude,
  location_type,
  con = NULL,
  ask = TRUE
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)

  lengths <- c(length(latitude), length(longitude), length(location_type))
  if (!all(lengths == lengths[1])) {
    stop("Latitude, longitude, and location_type must be the same length.")
  }

  check <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS count
    FROM spatial.vectors
    WHERE layer_name = 'National Hydro Network - Basins';
    "
  )[1, 1]

  if (check < 1338) {
    message(
      "National Hydro Network - Basins layer not found or incomplete in ",
      "spatial.vectors. Downloading the layer is required to generate location ",
      "codes."
    )

    do_download <- !ask || !interactive()
    if (ask && interactive()) {
      ans <- readline(prompt = "Download and load NHN Basins now? (y/n): ")
      do_download <- tolower(ans) == "y"
    }

    if (!do_download) {
      stop(
        "National Hydro Network basins are required to generate location codes."
      )
    }

    message("Downloading and loading NHN basins...")
    vect <- terra::vect(
      "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_decoupage.gpkg.zip"
    )
    vect <- vect[, c("validity_date", "dataset_name", "edition", "version")]
    vect$description <- paste0(
      "Edition: ",
      vect$edition,
      ", Version: ",
      vect$version
    )

    insertACVector(
      geom = vect,
      layer_name = "National Hydro Network - Basins",
      feature_name_col = "dataset_name",
      description_col = "description",
      con = con,
      overwrite = FALSE,
      ask = FALSE
    )
  }

  location_types <- DBI::dbGetQuery(
    con,
    "SELECT type_id, type_suffix FROM location_types;"
  )

  extract_suffix <- function(code) {
    if (length(code) == 0) {
      return(NA_integer_)
    }
    result <- suppressWarnings(as.integer(sub("^.*?(\\d+)$", "\\1", code)))
    result[is.na(code)] <- NA_integer_
    result
  }

  generated_codes <- character(length(latitude))

  for (i in seq_along(latitude)) {
    poly <- DBI::dbGetQuery(
      con,
      "
      WITH p AS (
        SELECT ST_Transform(ST_SetSRID(ST_MakePoint($1,$2), 4326), ST_SRID(geom)) AS pt
        FROM spatial.vectors
        WHERE layer_name = 'National Hydro Network - Basins'
        LIMIT 1
      )
      SELECT feature_name
      FROM spatial.vectors v, p
      WHERE v.layer_name = 'National Hydro Network - Basins'
        AND ST_Intersects(v.geom, p.pt)
      ORDER BY ST_Area(v.geom::geography) ASC
      LIMIT 1;
      ",
      params = list(longitude[i], latitude[i])
    )[1, 1]

    if (is.na(poly)) {
      stop(
        "Unable to find a National Hydro Network basin for latitude ",
        latitude[i],
        " and longitude ",
        longitude[i],
        "."
      )
    }

    code <- sub("^([0-9]{2})([A-Za-z]{2,3}).*$", "\\1\\2", poly)

    type_suffix <- location_types[
      location_types$type_id == location_type[i],
      "type_suffix"
    ]

    if (length(type_suffix) == 0 || is.na(type_suffix)) {
      type_suffix <- "OT"
      warning("Location type suffix is NA. Using 'OT' (other) as the suffix.")
    }

    prefix <- paste0(code, "-", type_suffix)

    existing_code <- DBI::dbGetQuery(
      con,
      "
      SELECT MAX(location_code) AS max_code
      FROM public.locations
      WHERE location_code LIKE $1
      ",
      params = list(paste0(prefix, "%"))
    )[1, 1]

    existing_suffix <- extract_suffix(existing_code)
    generated_suffix <- extract_suffix(
      generated_codes[grepl(paste0("^", prefix, "-"), generated_codes)]
    )

    suffix_max <- c(existing_suffix, generated_suffix)
    if (all(is.na(suffix_max))) {
      next_suffix <- 1
    } else {
      next_suffix <- max(suffix_max, na.rm = TRUE) + 1
    }

    generated_codes[i] <- paste0(prefix, "-", sprintf("%05d", next_suffix))
  }

  generated_codes
}
