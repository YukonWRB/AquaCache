#' Get elevation at a geographic coordinate
#'
#' Queries one or more public elevation services and returns the first
#' available elevation.
#'
#' The default source order is:
#'
#' * `"arcticdem"`: ArcticDEM 2 m mosaic from the Polar Geospatial Center. Prefer this when available for highest accuracy north of the US/Canada border, though be aware that it is a DSM and not a DTM.
#' * `"usgs"`: USGS 3DEP Elevation Point Query Service. United States and Canada, including Alaska. Locations in the US most often benefit from LIDAR and a cell size of 1 meter, 5 meters in parts of Alaska.
#' * `"cdem"`: Canadian Digital Elevation Model.
#' * `"cdsm"`: Canadian Digital Surface Model. Not available above 60 degrees of latitude, but in that case you should prefer the ArcticDEM over the cdem; this is just a fall-back.
#'
#' No API keys are required for the default sources.
#'
#' @param lat Numeric scalar. Latitude in decimal degrees (WGS84).
#' @param lon Numeric scalar. Longitude in decimal degrees (WGS84).
#' @param source Character vector giving the elevation sources to try, in
#'   order. Valid values are `"usgs"`, `"arcticdem"`, `"cdem"`, and `"cdsm"`.
#' @param details Logical. If `FALSE`, return only the elevation in metres.
#'   If `TRUE`, return a list containing elevation and source metadata.
#'
#' @return
#' If `details = FALSE`, a numeric scalar giving elevation in metres, or
#' `NA_real_` if none of the requested sources return a value.
#'
#' If `details = TRUE`, a list containing `elevation`, `source`,
#' `resolution`, and `vertical_datum`.
#'
#' @export
get_elevation <- function(
  lat,
  lon,
  source = c("arcticdem", "usgs", "cdem", "cdsm"),
  details = TRUE
) {
  lat <- .validate_coordinate(lat, "lat", -90, 90)
  lon <- .validate_coordinate(lon, "lon", -180, 180)

  valid_sources <- c("arcticdem", "usgs", "cdem", "cdsm")

  unknown <- setdiff(source, valid_sources)

  if (length(unknown)) {
    stop(
      "Unknown elevation source(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  providers <- list(
    arcticdem = .elevation_arcticdem,
    usgs = .elevation_usgs,
    cdem = .elevation_geogratis,
    cdsm = .elevation_geogratis
  )

  for (src in source) {
    result <- tryCatch(
      providers[[src]](
        lat = lat,
        lon = lon,
        source = src
      ),
      error = function(e) NULL
    )

    if (
      !is.null(result) &&
        length(result$elevation) == 1L &&
        is.finite(result$elevation)
    ) {
      if (isTRUE(details)) {
        return(result)
      }

      return(result$elevation)
    }
  }

  if (isTRUE(details)) {
    return(list(
      elevation = NA_real_,
      source = NA_character_,
      resolution = NA_real_,
      vertical_datum = NA_character_
    ))
  }

  NA_real_
}


.validate_coordinate <- function(x, name, min, max) {
  if (length(x) != 1L) {
    stop(
      "'",
      name,
      "' must be a single value.",
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.numeric(x))

  if (!is.finite(value)) {
    stop(
      "'",
      name,
      "' could not be coerced to a finite numeric value.",
      call. = FALSE
    )
  }

  if (value < min || value > max) {
    stop(
      "'",
      name,
      "' must be between ",
      min,
      " and ",
      max,
      ".",
      call. = FALSE
    )
  }

  value
}

.elevation_usgs <- function(lat, lon, source = NULL) {
  response <- httr2::request(
    "https://epqs.nationalmap.gov/v1/json"
  ) |>
    httr2::req_url_query(
      x = lon,
      y = lat,
      wkid = 4326,
      units = "Meters",
      includeDate = "false"
    ) |>
    httr2::req_timeout(seconds = 10) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  elevation <- suppressWarnings(
    as.numeric(response$value)
  )

  if (
    length(elevation) != 1L ||
      !is.finite(elevation) ||
      elevation <= -999999
  ) {
    return(NULL)
  }

  resolution <- suppressWarnings(
    as.numeric(response$resolution)
  )

  if (!length(resolution) || !is.finite(resolution)) {
    resolution <- NA_real_
    resolution_units <- NA_character_
  } else if (resolution < 0.01) {
    resolution_units <- "degrees"
  } else {
    resolution_units <- "m"
  }

  list(
    elevation = elevation,
    elevation_units = "m",
    source = "USGS 3DEP",
    resolution_x = resolution,
    resolution_y = resolution,
    resolution_units = resolution_units,
    vertical_datum = "NAVD88"
  )
}

.elevation_arcticdem <- function(lat, lon, source = NULL) {
  geometry <- sprintf(
    paste0(
      '{"x":%.10f,"y":%.10f,',
      '"spatialReference":{"wkid":4326}}'
    ),
    lon,
    lat
  )

  response <- httr2::request(
    paste0(
      "https://di-pgc.img.arcgis.com/arcgis/rest/services/",
      "arcticdem_latest/ImageServer/identify"
    )
  ) |>
    httr2::req_url_query(
      geometry = geometry,
      geometryType = "esriGeometryPoint",
      returnGeometry = "false",
      renderingRule = '{"rasterFunction":"Height Orthometric"}',
      f = "json"
    ) |>
    httr2::req_timeout(seconds = 10) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  elevation <- suppressWarnings(
    as.numeric(response$value)
  )

  if (
    length(elevation) != 1L ||
      !is.finite(elevation)
  ) {
    return(NULL)
  }

  list(
    elevation = elevation,
    elevation_units = "m",
    source = "ArcticDEM",
    resolution_x = 2,
    resolution_y = 2,
    resolution_units = "m",
    vertical_datum = "EGM2008"
  )
}


.elevation_geogratis <- function(
  lat,
  lon,
  source = c("cdsm", "cdem")
) {
  source <- match.arg(source)

  response <- httr2::request(
    paste0(
      "https://geogratis.gc.ca/services/elevation/",
      source,
      "/altitude"
    )
  ) |>
    httr2::req_url_query(
      lat = lat,
      lon = lon
    ) |>
    httr2::req_timeout(seconds = 10) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  elevation <- suppressWarnings(
    as.numeric(response$altitude)
  )

  if (
    length(elevation) != 1L ||
      !is.finite(elevation)
  ) {
    return(NULL)
  }

  if (source == "cdsm") {
    resolution_x <- 0.75
    resolution_y <- 0.75
  } else {
    # CDEM longitude resolution varies by latitude.
    resolution_x <- if (lat < 68) {
      0.75
    } else if (lat < 80) {
      1.5
    } else {
      3
    }

    # Latitude resolution is constant.
    resolution_y <- 0.75
  }

  list(
    elevation = elevation,
    elevation_units = "m",
    source = toupper(source),
    resolution_x = resolution_x,
    resolution_y = resolution_y,
    resolution_units = "arcsec",
    vertical_datum = "CGVD28:2010"
  )
}
