#' Get CaLDAS rasters
#'
#' @param parameter The parameter for which to get new rasters, such as 'AirTemp_AGL-1.5m' or 'SnowDepth_Sfc' or 'SnowWaterEquiv_Sfc'.
#' @param start_datetime The datetime from which to start looking for new rasters. Coerced to POSIXct, timezone UTC. At present, CaLDAS rasters are only available for 30 days in the past, so earlier start datetimes will be adjusted to 30 days ago for speed.
#' @param clip The area with which to clip the rasters. Supply one or more
#'   two-letter province or territory abbreviations as per the [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8)
#'   to use their combined 300 km buffered boundary. Alternatively, supply an
#'   unnamed numeric EPSG:4326 vector in `north`, `west`, `south`, `east` order, a named
#'   numeric vector or named list using either those names or `xmin`, `xmax`,
#'   `ymin`, and `ymax`, or a [terra::SpatExtent]. JSON arrays and named JSON
#'   objects passed through `source_fx_args` are supported. Set to `NULL` for
#'   no clip.
#' @param hrs CaLDAS data is provided every 3 hours. Give a vector of hours from 0, 3, 6, 9, 12, 15, 18, 21 to specify which rasters to bring in from start_datetime to end_datetime. Default is for 0 hours only, so each day at 00:00 UTC.

#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export
#'

downloadCaLDAS <- function(parameter, start_datetime, clip = NULL, hrs = c(0)) {
  clip_spec <- raster_clip_normalize(clip)

  if (!inherits(start_datetime, "POSIXct")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else {
    attr(start_datetime, "tzone") <- "UTC"
  }

  if (start_datetime < (Sys.time() - 30 * 24 * 60 * 60)) {
    message(
      "CaLDAS rasters are only available for the past 30 days; adjusting start_datetime accordingly."
    )
    start_datetime <- Sys.time() - 30 * 24 * 60 * 60
  }

  # 'hrs' might have been passed in a a character vector like 0,6,12,18. Separate on the commas so it can be made a numeric vector
  if (inherits(hrs, "character")) {
    hrs <- strsplit(hrs, ",")[[1]]
  }

  # Check that 'hrs' is a numeric vector of integers between 0 and 23
  if (!inherits(hrs, "numeric")) {
    hrs <- as.numeric(hrs)
  }
  if (
    length(hrs) == 0L ||
      anyNA(hrs) ||
      !all(hrs %in% c(0, 3, 6, 9, 12, 15, 18, 21))
  ) {
    stop(
      "Parameter 'hrs' must be a numeric vector of integers in c(0, 3, 6, 9, 12, 15, 18, 21)."
    )
  }

  # Check if there already exists a temporary file with the required interval, location, start_datetime, and end_datetime.
  # Listings are already filtered by parameter, hour, and requested date range,
  # so cache each request separately. A shared cache can silently return files
  # for an earlier call with different criteria.
  parameter_key <- paste(sprintf("%02x", utf8ToInt(parameter)), collapse = "")
  hours_key <- paste(sort(unique(hrs)), collapse = "-")
  start_date_key <- format(as.Date(start_datetime, tz = "UTC"), "%Y%m%d")
  cache_dir <- file.path(
    tempdir(),
    "downloadCaLDAS",
    paste(parameter_key, hours_key, start_date_key, sep = "_")
  )
  saved_files <- list.files(cache_dir)
  if (length(saved_files) == 0) {
    file_exists <- FALSE
  } else {
    saved_files <- data.frame(
      file = saved_files,
      datetime = as.POSIXct(
        saved_files,
        format = "%Y%m%d%H%M.rds",
        tz = "UTC"
      )
    )
    ok <- saved_files[
      !is.na(saved_files$datetime) &
        saved_files$datetime > Sys.time() - 10 * 60,
    ]
    if (nrow(ok) > 0) {
      target_file <- ok[
        order(ok$datetime, decreasing = TRUE),
      ][1, ]
      available <- readRDS(file.path(cache_dir, target_file$file))
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
  }
  # If there is no file that matches necessary use, download and save for later
  if (!file_exists) {
    seq_days <- seq.Date(
      as.Date(start_datetime),
      as.Date(.POSIXct(Sys.time(), tz = "UTC")),
      by = "day"
    )
    available <- data.frame()
    for (i in 1:length(seq_days)) {
      day <- seq_days[i]
      for (j in hrs) {
        # j is hour, but it it's single digit is needs to be padded with a 0
        if (nchar(as.character(j)) == 1) {
          j <- paste0("0", j)
        } else {
          j <- as.character(j)
        }
        tryCatch(
          {
            files <- suppressWarnings(rvest::session(paste0(
              "https://dd.weather.gc.ca/",
              gsub("-", "", day),
              "/WXO-DD/model_nsrps-caldas/2.5km/",
              j,
              "/"
            ))) |>
              rvest::html_elements(xpath = '//*[contains(@href, ".nc")]') |>
              rvest::html_attr("href")
            files <- files[grep(parameter, files)] # only retain the files we're interested in
            tmp <- data.frame(
              file = files,
              datetime = as.POSIXct(
                substr(files, 1, 11),
                format = "%Y%m%dT%H",
                tz = "UTC"
              ),
              prelim = FALSE,
              path = paste0(
                "https://dd.weather.gc.ca/",
                gsub("-", "", day),
                "/WXO-DD/model_nsrps-caldas/2.5km/",
                j,
                "/",
                files
              )
            )
            available <- rbind(available, tmp)
          },
          error = function(e) {
            message(paste0(
              "No CaLDAS raster available for ",
              day,
              " at ",
              j,
              " UTC"
            ))
          }
        )
      }
    }

    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    name <- format(Sys.time(), "%Y%m%d%H%M", tz = "UTC")
    saveRDS(available, file.path(cache_dir, paste0(name, ".rds")))
  }

  # Now filter by start_datetime
  available <- available[available$datetime >= start_datetime, ]
  available <- available[order(available$datetime), ]
  duplicates <- duplicated(available$datetime, fromLast = TRUE) |
    duplicated(available$datetime)
  available <- available[!(available$prelim & duplicates) | !duplicates, ]

  clip <- raster_clip_spatial(clip_spec)

  if (nrow(available) > 0) {
    message("downloadCaLDAS: new rasters available. Downloading...")
    files <- list()
    clipped <- FALSE
    native_clip <- NULL
    for (i in 1:nrow(available)) {
      file <- list()
      download_url <- available$path[i]
      tmp <- tempfile(fileext = ".nc")
      curl::curl_download(
        url = download_url,
        destfile = tmp,
        quiet = TRUE
      )
      rast <- terra::rast(tmp)[[1]]
      file[["units"]] <- terra::units(rast) # Units is fetched now because the clip operation seems to remove them.
      if (!clipped) {
        if (identical(clip_spec$type, "bbox")) {
          # Project a densified WGS84 boundary to the rotated CaLDAS grid.
          # Projecting the full national raster first can make GDAL choose an
          # incorrect output extent for this rotated-pole NetCDF.
          area <- clip_spec$value
          boundary <- rbind(
            cbind(seq(area[2L], area[4L], length.out = 101L), area[1L]),
            cbind(seq(area[2L], area[4L], length.out = 101L), area[3L]),
            cbind(area[2L], seq(area[3L], area[1L], length.out = 101L)),
            cbind(area[4L], seq(area[3L], area[1L], length.out = 101L))
          )
          native_boundary <- terra::project(
            boundary,
            "epsg:4326",
            terra::crs(rast)
          )
          native_clip <- terra::ext(
            range(native_boundary[, 1L]),
            range(native_boundary[, 2L])
          )
        } else if (identical(clip_spec$type, "provinces")) {
          native_clip <- terra::project(clip, rast)
        }
        clipped <- TRUE
      }
      if (!is.null(clip)) {
        rast <- terra::crop(rast, native_clip)
        if (identical(clip_spec$type, "provinces")) {
          rast <- terra::mask(rast, native_clip) # Makes NA values beyond the boundary of clip
          rast <- terra::trim(rast) # Trims the NA values
        }
      }
      if (is.null(clip)) {
        # The rotated grid spans enough of the globe that projecting it in one
        # operation can produce an incorrect target extent. Project four
        # smaller regions and merge them in WGS84 instead.
        source_extent <- as.vector(terra::ext(rast))
        x_mid <- mean(source_extent[1:2])
        y_mid <- mean(source_extent[3:4])
        tile_extents <- list(
          terra::ext(source_extent[1L], x_mid, source_extent[3L], y_mid),
          terra::ext(x_mid, source_extent[2L], source_extent[3L], y_mid),
          terra::ext(source_extent[1L], x_mid, y_mid, source_extent[4L]),
          terra::ext(x_mid, source_extent[2L], y_mid, source_extent[4L])
        )
        projected_tiles <- lapply(
          tile_extents,
          function(x) terra::project(terra::crop(rast, x), "epsg:4326")
        )
        rast <- suppressWarnings(do.call(terra::merge, projected_tiles))
      } else {
        rast <- terra::project(rast, "epsg:4326") # Project to WGS84 (EPSG:4326)
      }
      if (identical(clip_spec$type, "bbox")) {
        rast <- terra::crop(rast, clip)
      }
      file[["rast"]] <- rast
      # Check which parameter we're dealing with: 6h or 24h, based on if 'parameter' contains 6h or 24h
      file[["valid_from"]] <- available[i, "datetime"] - 60 * 60 * 3 # CaLDAS rasters are every 3 hours
      file[["valid_to"]] <- available[i, "datetime"]
      file[["source"]] <- download_url
      file[["model"]] <- "CaLDAS"
      files[[i]] <- file
    }
    files[["forecast"]] <- FALSE
    message("downloadCaLDAS: finished downloading new rasters.")
  } else {
    message("downloadCaLDAS: no new rasters found.")
    files <- NULL
  }
  return(files)
}
