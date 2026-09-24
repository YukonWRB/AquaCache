raster_clip_normalize <- function(clip) {
  if (is.null(clip)) {
    return(list(type = "none", value = NULL))
  }

  if (is.character(clip)) {
    if (length(clip) == 0L || anyNA(clip) || any(nchar(clip) != 2L)) {
      stop(
        "Character parameter 'clip' must contain two-letter province or ",
        "territory abbreviations."
      )
    }
    return(list(type = "provinces", value = clip))
  }

  if (inherits(clip, "SpatExtent")) {
    area <- unname(c(clip$ymax, clip$xmin, clip$ymin, clip$xmax))
  } else {
    # A named JSON object decodes as a list, while a JSON array decodes as a
    # numeric vector. Accept both representations used by source_fx_args.
    if (
      is.list(clip) &&
        length(clip) == 4L &&
        all(vapply(
          clip,
          function(x) is.numeric(x) && length(x) == 1L,
          logical(1)
        ))
    ) {
      clip <- unlist(clip, use.names = TRUE)
    }
    if (!is.numeric(clip)) {
      stop(
        "Parameter 'clip' must be province abbreviations, a numeric bounding ",
        "box, a named bounding-box list, a terra SpatExtent, or NULL."
      )
    }
    if (length(clip) != 4L) {
      stop("Bounding-box parameter 'clip' must contain exactly four coordinates.")
    }

    clip_names <- names(clip)
    if (is.null(clip_names) || all(!nzchar(clip_names))) {
      area <- unname(as.numeric(clip))
    } else {
      if (anyNA(clip_names)) {
        stop("Names on numeric parameter 'clip' cannot be missing.")
      }
      clip_names <- tolower(trimws(clip_names))
      directional_names <- setequal(
        clip_names,
        c("north", "west", "south", "east")
      )
      extent_names <- setequal(
        clip_names,
        c("xmin", "xmax", "ymin", "ymax")
      )
      if (
        length(unique(clip_names)) != 4L ||
          (!directional_names && !extent_names)
      ) {
        stop(
          "Named bounding-box parameter 'clip' must use north, west, south, ",
          "east or xmin, xmax, ymin, ymax."
        )
      }
      if (directional_names) {
        area <- unname(clip[match(
          c("north", "west", "south", "east"),
          clip_names
        )])
      } else {
        area <- unname(clip[match(
          c("ymax", "xmin", "ymin", "xmax"),
          clip_names
        )])
      }
    }
  }

  area <- unname(as.numeric(area))
  if (any(!is.finite(area))) {
    stop("Bounding-box coordinates in 'clip' must be finite numbers.")
  }
  if (any(area[c(1L, 3L)] < -90 | area[c(1L, 3L)] > 90)) {
    stop("Bounding-box latitudes in 'clip' must be between -90 and 90.")
  }
  if (any(area[c(2L, 4L)] < -180 | area[c(2L, 4L)] > 180)) {
    stop("Bounding-box longitudes in 'clip' must be between -180 and 180.")
  }
  if (area[1L] <= area[3L]) {
    stop("Bounding-box north must be greater than south in 'clip'.")
  }
  if (area[4L] <= area[2L]) {
    stop("Bounding-box east must be greater than west in 'clip'.")
  }

  list(type = "bbox", value = area)
}

raster_clip_spatial <- function(clip_spec, provinces = prov_buff) {
  if (identical(clip_spec$type, "none")) {
    return(NULL)
  }
  if (identical(clip_spec$type, "bbox")) {
    area <- clip_spec$value
    return(terra::ext(area[c(2L, 4L, 3L, 1L)]))
  }

  valid_codes <- unique(provinces$PREABBR)
  if (!all(clip_spec$value %in% valid_codes)) {
    stop(sprintf(
      "Some values in 'clip' are not valid province abbreviations. Valid values are: %s",
      paste(valid_codes, collapse = ", ")
    ))
  }
  provinces[provinces$PREABBR %in% clip_spec$value, ]
}
