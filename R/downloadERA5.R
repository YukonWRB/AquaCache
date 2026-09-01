#' Get ERA5 rasters
#'
#' @description Interfaces with the ecmwfr package to download ERA5-Land reanalysis data from the ECMWF Copernicus Climate Data Store (CDS). The function downloads the data in netCDF format and returns a list of rasters with associated metadata. ERA5-Land forecast accumulations are converted to one-hour values by subtracting the preceding forecast step; required predecessor hours are requested automatically. Data are downloaded sequentially starting from the earliest requested timestamp. If a download fails, the function returns the rasters that were successfully downloaded up to that point. Rasters are returned in the geographic coordinate reference system (EPSG:4326).
#'
#' @param param The ERA5-Land variable name used by the CDS API, such as
#'   `"total_precipitation"` or `"snow_depth"`.
#' @param start_datetime The datetime from which to start looking for new rasters. This date does not need to align with the first day of a month. Required timestamps are grouped by month and hour for efficient CDS requests. Specify as POSIXct or something coercible to POSIXct; coercion will be done to UTC time zone.
#' @param end_datetime The datetime until which to look for new rasters. If NULL, the current datetime is used. Specify as POSIXct or something coercible to POSIXct; coercion will be done to UTC time zone.
#' @param clip The two-letter abbreviation(s) as per the [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip ERA5-Land rasters. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#' @param user A label for the ECMWF credentials, retained for compatibility with
#'   `ecmwfr`. Current CDS personal access tokens do not require a username.
#' @param key The ECMWF CDS personal access token. By default, this is read from
#'   the `ecmwfr`-standard `ecmwfr_PAT` environment variable. A token passed
#'   explicitly is made available to `ecmwfr` only for the duration of this
#'   call, bypassing platform-specific keyrings.
#' @param hrs ERA5 data is provided in hourly chunks. Specify a vector of hours from 0 to 23 specifying the hourly rasters to bring in from start_datetime to end_datetime. Default is for 0 hours only, or each day at 00:00 UTC.
#' @param batch Should a batch request be used or should downloads be sequential? Batch request can be much quicker but will fail if any of the requests fail. Sequential runs each request one by one from the earliest possible raster so that if one fails rasters are returned up to the last successful download.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export
#'

downloadERA5 <- function(
  start_datetime,
  end_datetime = .POSIXct(Sys.time(), tz = "UTC"),
  clip = NULL,
  param,
  user = "ecmwfr",
  key = Sys.getenv("ecmwfr_PAT"),
  hrs = c(0),
  batch = TRUE
) {
  # Checks and conversions for datetimes
  if (!inherits(start_datetime, "POSIXct")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else {
    attr(start_datetime, "tzone") <- "UTC"
  }

  if (!inherits(end_datetime, "POSIXct")) {
    end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
  } else {
    attr(end_datetime, "tzone") <- "UTC"
  }

  # ERA5-Land data are only available up to five days prior to the current time
  max_end <- as.POSIXct(
    as.Date(Sys.time() - 5 * 24 * 60 * 60, tz = "UTC"),
    tz = "UTC"
  )
  if (end_datetime > max_end) {
    end_datetime <- max_end
  }
  if (start_datetime > max_end) {
    return(list()) # No data can be returned
  }
  if (start_datetime > end_datetime) {
    stop("Parameter 'start_datetime' must be before 'end_datetime'.")
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
    !is.numeric(hrs) ||
      any(hrs < 0) ||
      any(hrs > 23) ||
      any(!is.finite(hrs)) ||
      any(hrs %% 1 != 0)
  ) {
    stop(
      "Parameter 'hrs' must be a numeric vector of integers between 0 and 23."
    )
  }

  if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
    stop(
      "No ECMWF API token was supplied. Set 'ecmwfr_PAT' or pass 'key' ",
      "explicitly."
    )
  }

  # ecmwfr checks ecmwfr_PAT before consulting keyring. Setting it temporarily
  # avoids repeated keyring backend selection warnings on headless Linux while
  # preserving an existing process-level value after this function returns.
  old_ecmwfr_pat <- Sys.getenv("ecmwfr_PAT", unset = NA_character_)
  on.exit({
    if (is.na(old_ecmwfr_pat)) {
      Sys.unsetenv("ecmwfr_PAT")
    } else {
      Sys.setenv(ecmwfr_PAT = old_ecmwfr_pat)
    }
  }, add = TRUE)
  Sys.setenv(ecmwfr_PAT = key)

  # Get that param is valid and fetch short form
  scrape_era5_land_metadata <- function(
    url = "https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation"
  ) {
    page <- rvest::read_html(url)
    tables <- rvest::html_table(page, fill = TRUE)
    # Optionally, assign names to tables based on their captions or order
    names(tables) <- paste0("table", seq_along(tables))
    return(tables)
  }

  # check parameter 'clip'
  if (!is.null(clip)) {
    if (!inherits(clip, "character")) {
      stop("Parameter clip must be a character vector of 2 characters.")
    } else if (any(nchar(clip) != 2)) {
      stop("Parameter clip must be a character vector of 2 characters.")
    }
  }

  area <- NULL
  if (!is.null(clip)) {
    prov_buff <- terra::vect(system.file(
      "extdata/prov_buffers/Provinces_buffered_300km.shp",
      package = "YGwater"
    ))
    prov_buff <- terra::project(prov_buff, "epsg:4326")

    # make sure clip is in the province shapefile
    if (!all(clip %in% prov_buff$PREABBR)) {
      stop(sprintf(
        "Some values in 'clip' are not valid province abbreviations. Valid values are: %s",
        paste(unique(prov_buff$PREABBR), collapse = ", ")
      ))
    }

    # This is package data living as shapefile in inst/extdata, loaded using file data_load.R
    clip <- prov_buff[prov_buff$PREABBR %in% clip, ]

    # get the extent of the clip polygon
    area <- terra::ext(clip)
    area <- c(area$ymax, area$xmin, area$ymin, area$xmax)
  }

  # Load the metadata for ERA5-Land parameters
  tables <- scrape_era5_land_metadata()
  param_md <- suppressMessages(dplyr::bind_rows(tables))

  # Check if the parameter is in the metadata
  if (!(param %in% param_md$`Variable name in CDS`)) {
    stop(sprintf(
      "Parameter '%s' not found in metadata. Be sure to use the 'Variable name in CDS', which can be found at 'https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation'",
      param
    ))
  }
  # Remove rows with NA in 'Variable name in CDS'
  param_md <- param_md[!is.na(param_md$`Variable name in CDS`), ]
  param_md <- param_md[param_md$`Variable name in CDS` == param, ]
  # Get the short name for the parameter
  param_short <- param_md$shortName

  # ERA5-Land forecast accumulations run from 00 UTC through forecast steps
  # 01--24. Hourly values therefore require the preceding forecast step,
  # except at 01 UTC where the step-1 value is already the one-hour total.
  # Variable classification follows the ECMWF ERA5-Land data documentation.
  accumulated_params <- c(
    "surface_runoff",
    "sub_surface_runoff",
    "snow_evaporation",
    "snowmelt",
    "snowfall",
    "surface_sensible_heat_flux",
    "surface_latent_heat_flux",
    "surface_solar_radiation_downwards",
    "surface_thermal_radiation_downwards",
    "surface_net_solar_radiation",
    "surface_net_thermal_radiation",
    "total_evaporation",
    "runoff",
    "total_precipitation",
    "evaporation_from_the_top_of_canopy",
    "evaporation_from_bare_soil",
    "evaporation_from_open_water_surfaces_excluding_oceans",
    "evaporation_from_vegetation_transpiration",
    "potential_evaporation"
  )
  is_accumulated <- param %in% accumulated_params

  output_days <- seq.Date(
    as.Date(start_datetime),
    as.Date(end_datetime),
    by = "day"
  )
  output_seconds <- as.vector(outer(
    as.numeric(as.POSIXct(output_days, tz = "UTC")),
    hrs * 60 * 60,
    `+`
  ))
  output_seconds <- sort(unique(output_seconds[
    output_seconds >= as.numeric(start_datetime) &
      output_seconds <= as.numeric(end_datetime)
  ]))
  if (length(output_seconds) == 0) return(list())
  output_datetimes <- .POSIXct(output_seconds, tz = "UTC")

  required_datetimes <- output_datetimes
  if (is_accumulated) {
    output_hours <- as.integer(format(output_datetimes, "%H", tz = "UTC"))
    required_datetimes <- c(
      required_datetimes,
      output_datetimes[output_hours != 1L] - 60 * 60
    )
  }
  required_datetimes <- sort(unique(required_datetimes))

  # Create a temporary directory to store the downloaded data, from which we will create rasters to upload to AC
  data_dir <- file.path(tempdir(), "downloadERA5")
  data_dir <- normalizePath(data_dir, mustWork = FALSE)
  # Clean up the directory in case it has leftover files from previous runs
  unlink(data_dir, recursive = TRUE, force = TRUE)
  suppressWarnings(dir.create(data_dir))

  # NOT USED because the files need to live on to the getNewRasters function
  # on.exit({
  #   # Clean up the temporary directory on exit
  #   if (dir.exists(data_dir)) {
  #     unlink(data_dir, recursive = TRUE, force = TRUE)
  #   }
  # }, add = TRUE)

  # Group required timestamps by month and hour. This keeps predecessor-only
  # dates out of unrelated requests while retaining efficient multi-day calls.
  requests <- list()
  request_groups <- split(
    required_datetimes,
    format(required_datetimes, "%Y%m%H", tz = "UTC")
  )
  for (group_datetimes in request_groups) {
    group_dates <- sort(unique(as.Date(group_datetimes, tz = "UTC")))
    run_id <- cumsum(c(TRUE, diff(as.integer(group_dates)) > 1L))

    for (id in unique(run_id)) {
      run_dates <- group_dates[run_id == id]
      hour <- format(group_datetimes[1], "%H", tz = "UTC")
      if (length(run_dates) == 1L) {
        date_value <- format(run_dates, "%Y-%m-%d")
        name <- paste0(
          "ERA5_",
          param_short,
          "_",
          format(run_dates, "%Y%m%d"),
          hour
        )
      } else {
        date_value <- paste(
          format(min(run_dates), "%Y-%m-%d"),
          format(max(run_dates), "%Y-%m-%d"),
          sep = "/"
        )
        name <- paste0(
          "ERA5_",
          param_short,
          "_",
          format(min(run_dates), "%Y%m%d"),
          hour,
          "_to_",
          format(max(run_dates), "%Y%m%d"),
          hour
        )
      }

      request <- list(
        dataset_short_name = "reanalysis-era5-land",
        product_type = "reanalysis",
        variable = param,
        date = date_value,
        time = paste0(hour, ":00"),
        data_format = "netcdf",
        download_format = "unarchived",
        target = paste0(name, ".nc")
      )
      if (!is.null(area)) request$area <- area
      requests[[length(requests) + 1L]] <- request
    }
  }

  # Download the data using the Copernicus API
  if (batch) {
    message(
      "downloading ERA5 rasters using batch request... please be patient."
    )
    workers <- min(length(requests), 10L)
    download_files <- suppressMessages(
      ecmwfr::wf_request_batch(
        request_list = requests, # the requests we built above
        path = data_dir,
        user = user,
        workers = workers,
        retry = 5
      )
    )
  } else {
    # Download the data using the Copernicus API sequentially so that a failure does not discard already downloaded rasters
    message("downloading ERA5 rasters sequentially... please be patient.")
    download_files <- character()
    downloaded_requests <- list()
    download_failed <- FALSE

    # If interactive, show progress bar
    num_requests <- length(requests)
    if (interactive()) {
      pb <- utils::txtProgressBar(min = 0, max = num_requests, style = 3)
      on.exit(close(pb), add = TRUE)
    }
    for (ii in 1:num_requests) {
      req <- requests[[ii]]
      if (interactive()) {
        utils::setTxtProgressBar(pb, ii)
      }
      tryCatch(
        {
          # use invisible and capture.output so that the progress bar from wf_request is suppressed
          invisible(utils::capture.output(
            zf <- suppressMessages(
              ecmwfr::wf_request(
                request = req, # Individual request
                path = data_dir,
                user = user,
                transfer = TRUE,
                retry = 30
              )
            )
          ))
          download_files <- c(download_files, zf)
          downloaded_requests[[length(downloaded_requests) + 1]] <- req
        },
        error = function(e) {
          message(sprintf(
            "Failed to download request '%s': %s",
            req$target,
            e$message
          ))
          download_failed <<- TRUE
        }
      )
      if (download_failed) break
    }

    requests <- downloaded_requests

    if (download_failed) {
      message(
        "Download incomplete due to an error. Returning available rasters only."
      )
    }
  }

  if (length(download_files) == 0) {
    stop("No data was downloaded. Please check your parameters and try again.")
  }

  # Current requests ask CDS for unarchived NetCDF files. Keep a defensive
  # fallback because CDS can still return a zip archive for some data layouts.
  for (download_file in download_files) {
    if (tolower(tools::file_ext(download_file)) != "zip") next

    archive_contents <- utils::unzip(download_file, list = TRUE)
    nc_files <- archive_contents$Name[grepl("\\.nc$", archive_contents$Name)]
    if (length(nc_files) != 1) {
      stop(sprintf(
        "Expected one NetCDF file in '%s', found %d.",
        basename(download_file),
        length(nc_files)
      ))
    }

    extract_dir <- tempfile("era5-unzip-", tmpdir = data_dir)
    dir.create(extract_dir)
    utils::unzip(download_file, files = nc_files, exdir = extract_dir)
    nc_target <- paste0(tools::file_path_sans_ext(download_file), ".nc")
    copied <- file.copy(
      file.path(extract_dir, nc_files),
      nc_target,
      overwrite = TRUE
    )
    if (!copied) {
      stop(sprintf("Could not extract '%s'.", basename(download_file)))
    }
    unlink(extract_dir, recursive = TRUE, force = TRUE)
    file.remove(download_file)
  }

  raw_rasters <- list()
  for (request in requests) {
    # Create a string representation of the request for logging
    url <- paste(
      names(request),
      as.character(request),
      sep = ": ",
      collapse = "; "
    )
    model <- request$dataset_short_name

    if ("date" %in% names(request)) {
      date_range <- strsplit(request$date, "/")[[1]]
      if (length(date_range) == 1L) {
        request_dates <- as.Date(date_range)
      } else {
        request_dates <- seq.Date(
          as.Date(date_range[1]),
          as.Date(date_range[2]),
          by = "day"
        )
      }
    } else {
      request_dates <- as.Date(paste(
        request$year,
        request$month,
        request$day,
        sep = "-"
      ))
    }
    hour_val <- as.numeric(substr(request$time, 1, 2))
    request_datetimes <- as.POSIXct(request_dates, tz = "UTC") +
      hour_val * 60 * 60

    filename <- file.path(data_dir, request$target)
    rasters <- terra::rast(filename)
    terra::crs(rasters) <- "EPSG:4326"
    if (terra::nlyr(rasters) != length(request_datetimes)) {
      stop(sprintf(
        "Expected %d raster layer(s) in '%s', found %d.",
        length(request_datetimes),
        basename(filename),
        terra::nlyr(rasters)
      ))
    }

    for (ii in seq_along(request_datetimes)) {
      datetime_key <- format(
        request_datetimes[ii],
        "%Y%m%d%H",
        tz = "UTC"
      )
      raw_rasters[[datetime_key]] <- list(
        rast = rasters[[ii]],
        model = model,
        url = url,
        units = terra::units(rasters[[ii]])
      )
    }
  }

  files <- list()
  for (ii in seq_along(output_datetimes)) {
    datetime_ii <- output_datetimes[ii]
    datetime_key <- format(datetime_ii, "%Y%m%d%H", tz = "UTC")
    current <- raw_rasters[[datetime_key]]
    if (is.null(current)) next

    raster <- current$rast
    if (is_accumulated) {
      hour_val <- as.integer(format(datetime_ii, "%H", tz = "UTC"))
      if (hour_val != 1L) {
        previous_key <- format(
          datetime_ii - 60 * 60,
          "%Y%m%d%H",
          tz = "UTC"
        )
        previous <- raw_rasters[[previous_key]]
        if (is.null(previous)) {
          message(sprintf(
            "Cannot de-accumulate '%s' at %s: preceding forecast step is missing.",
            param,
            format(datetime_ii, "%Y-%m-%d %H:%M UTC", tz = "UTC")
          ))
          next
        }
        raster <- raster - previous$rast
      }
    }

    files[[length(files) + 1L]] <- list(
      rast = raster,
      valid_from = datetime_ii - 60 * 60,
      valid_to = datetime_ii,
      flag = NA,
      source = "ECMWF API",
      model = current$model,
      url = current$url,
      units = current$units,
      issued = datetime_ii + 5 * 60 * 60 * 24
    )
  }

  files[["forecast"]] <- FALSE
  return(files)
}
