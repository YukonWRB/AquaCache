#' Get ERA5 rasters
#'
#' @description Interfaces with the ecmwfr package to download ERA5-Land
#'   reanalysis data from the ECMWF Copernicus Climate Data Store (CDS). The
#'   function downloads NetCDF data and returns rasters with associated
#'   metadata. ERA5-Land forecast accumulations are converted to one-hour
#'   values by subtracting the preceding forecast step; required predecessor
#'   hours are requested automatically. Requests are grouped by month and hour
#'   and may be submitted in parallel. Transient failures are retried, and an
#'   incomplete run returns only its longest complete chronological prefix.
#'   Rasters are returned in the geographic coordinate reference system
#'   (EPSG:4326).
#'
#' @param param The ERA5-Land variable name used by the CDS API, such as
#'   `"total_precipitation"` or `"snow_depth"`.
#' @param start_datetime The datetime from which to start looking for new rasters. This date does not need to align with the first day of a month. Required timestamps are grouped by month and hour for efficient CDS requests. Specify as POSIXct or something coercible to POSIXct; coercion will be done to UTC time zone.
#' @param end_datetime The datetime until which to look for new rasters. If NULL, the current datetime is used. Specify as POSIXct or something coercible to POSIXct; coercion will be done to UTC time zone.
#' @param clip The area to download. Supply one or more two-letter province or
#'   territory abbreviations as per the [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8)
#'   to use their combined 300 km buffered extent. Alternatively, supply an
#'   unnamed numeric EPSG:4326 vector in CDS order (`north`, `west`, `south`, `east`), a
#'   named numeric vector or named list using either those names or `xmin`,
#'   `xmax`, `ymin`, and `ymax`, or a [terra::SpatExtent]. JSON arrays and named
#'   JSON objects passed through `source_fx_args` are supported. Set to `NULL`
#'   for no clip.
#' @param user A label for the ECMWF credentials, retained for compatibility with
#'   `ecmwfr`. Current CDS personal access tokens do not require a username.
#' @param key The ECMWF CDS personal access token. By default, this is read from
#'   the `ecmwfr`-standard `ecmwfr_PAT` environment variable. A token passed
#'   explicitly is made available to `ecmwfr` only for the duration of this
#'   call, bypassing platform-specific keyrings.
#' @param hrs ERA5 data is provided in hourly chunks. Specify a vector of hours
#'   from 0 to 23 specifying the hourly rasters to bring in from
#'   `start_datetime` to `end_datetime`. The default is 0 hours, or each day at
#'   00:00 UTC. Character forms such as `"0,6,12,18"`, `"c(0,6,12,18)"`, and
#'   `"c(0:23)"` are also accepted without evaluating R code.
#' @param batch Should requests be submitted in parallel with
#'   [ecmwfr::wf_request_batch()] or downloaded sequentially? Failed or timed
#'   out batch requests are retried sequentially. Both modes return only the
#'   longest complete chronological prefix, so later successful downloads can
#'   never advance a raster series past missing data.
#' @param max_attempts Maximum number of attempts for each request after a batch
#'   failure or when downloading sequentially.
#' @param retry_delay Initial delay in seconds between request attempts. The
#'   delay doubles after each failure, up to five minutes.
#' @param request_timeout Maximum number of seconds to wait for a request during
#'   each attempt.
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
  batch = TRUE,
  max_attempts = 5L,
  retry_delay = 30,
  request_timeout = 3600
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

  # Source-adapter arguments created before typed JSON controls may contain a
  # compact character representation. Parse only comma-separated numbers and
  # integer ranges; never evaluate database-provided text as R code.
  if (inherits(hrs, "character")) {
    if (length(hrs) != 1L || is.na(hrs)) {
      stop("Character parameter 'hrs' must contain one value.")
    }
    hrs_text <- trimws(hrs)
    hrs_text <- sub("^c\\((.*)\\)$", "\\1", hrs_text)
    hrs_tokens <- trimws(strsplit(hrs_text, ",", fixed = TRUE)[[1]])
    hrs <- unlist(lapply(hrs_tokens, function(token) {
      if (grepl("^[0-9]+\\s*:\\s*[0-9]+$", token)) {
        bounds <- as.integer(strsplit(token, ":", fixed = TRUE)[[1]])
        return(seq.int(bounds[1], bounds[2]))
      }
      suppressWarnings(as.numeric(token))
    }), use.names = FALSE)
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
  hrs <- sort(unique(hrs))

  if (is.character(batch) && length(batch) == 1L && !is.na(batch)) {
    batch <- switch(
      tolower(trimws(batch)),
      "true" = TRUE,
      "false" = FALSE,
      stop("Character parameter 'batch' must be 'TRUE' or 'FALSE'.")
    )
  }
  if (!is.logical(batch) || length(batch) != 1L || is.na(batch)) {
    stop("Parameter 'batch' must be TRUE or FALSE.")
  }
  if (
    length(max_attempts) != 1L ||
      is.na(max_attempts) ||
      !is.numeric(max_attempts) ||
      max_attempts < 1 ||
      max_attempts %% 1 != 0
  ) {
    stop("Parameter 'max_attempts' must be a positive integer.")
  }
  max_attempts <- as.integer(max_attempts)
  if (
    length(retry_delay) != 1L ||
      is.na(retry_delay) ||
      !is.numeric(retry_delay) ||
      retry_delay < 0
  ) {
    stop("Parameter 'retry_delay' must be a non-negative number.")
  }
  if (
    length(request_timeout) != 1L ||
      is.na(request_timeout) ||
      !is.numeric(request_timeout) ||
      request_timeout <= 0
  ) {
    stop("Parameter 'request_timeout' must be a positive number.")
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
  on.exit(
    {
      if (is.na(old_ecmwfr_pat)) {
        Sys.unsetenv("ecmwfr_PAT")
      } else {
        Sys.setenv(ecmwfr_PAT = old_ecmwfr_pat)
      }
    },
    add = TRUE
  )
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

  clip_spec <- raster_clip_normalize(clip)
  area <- clip_spec$value
  if (identical(clip_spec$type, "provinces")) {
    prov_buff <- terra::vect(system.file(
      "extdata/prov_buffers/Provinces_buffered_300km.shp",
      package = "YGwater"
    ))
    prov_buff <- terra::project(prov_buff, "epsg:4326")
    clip <- raster_clip_spatial(clip_spec, prov_buff)

    # get the extent of the clip polygon
    area <- terra::ext(clip)
    area <- unname(c(area$ymax, area$xmin, area$ymin, area$xmax))
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
  if (length(output_seconds) == 0) {
    return(list())
  }
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

  # Clipped instantaneous variables can request every selected hour for a
  # month in one CDS job. Unclipped requests stay split by hour to avoid very
  # large global files. Accumulated variables also stay split so automatic
  # predecessor timestamps do not expand into a date/hour cross-product.
  requests <- list()
  request_group_format <- if (is_accumulated || is.null(area)) {
    "%Y%m%H"
  } else {
    "%Y%m"
  }
  request_groups <- split(
    required_datetimes,
    format(required_datetimes, request_group_format, tz = "UTC")
  )
  for (group_datetimes in request_groups) {
    group_dates <- sort(unique(as.Date(group_datetimes, tz = "UTC")))
    group_hours <- sort(unique(as.integer(format(
      group_datetimes,
      "%H",
      tz = "UTC"
    ))))
    run_id <- cumsum(c(TRUE, diff(as.integer(group_dates)) > 1L))

    for (id in unique(run_id)) {
      run_dates <- group_dates[run_id == id]
      run_seconds <- sort(as.vector(outer(
        as.numeric(as.POSIXct(run_dates, tz = "UTC")),
        group_hours * 60 * 60,
        `+`
      )))
      run_datetimes <- .POSIXct(run_seconds, tz = "UTC")
      if (length(run_datetimes) == 1L) {
        name <- paste0(
          "ERA5_",
          param_short,
          "_",
          format(run_datetimes, "%Y%m%d%H", tz = "UTC")
        )
      } else {
        name <- paste0(
          "ERA5_",
          param_short,
          "_",
          format(min(run_datetimes), "%Y%m%d%H", tz = "UTC"),
          "_to_",
          format(max(run_datetimes), "%Y%m%d%H", tz = "UTC")
        )
      }

      request <- list(
        dataset_short_name = "reanalysis-era5-land",
        product_type = "reanalysis",
        variable = param,
        year = format(run_dates[1L], "%Y"),
        month = format(run_dates[1L], "%m"),
        day = format(run_dates, "%d"),
        time = sprintf("%02d:00", group_hours),
        data_format = "netcdf",
        download_format = "unarchived",
        target = paste0(name, ".nc")
      )
      if (!is.null(area)) {
        request$area <- area
      }
      requests[[length(requests) + 1L]] <- request
    }
  }

  find_download_file <- function(request, returned_files = character()) {
    request_stem <- tools::file_path_sans_ext(basename(request$target))
    disk_files <- list.files(data_dir, full.names = TRUE)
    candidates <- unique(c(returned_files, disk_files))
    candidates <- candidates[
      !is.na(candidates) & nzchar(candidates) &
        tools::file_path_sans_ext(basename(candidates)) == request_stem &
        tolower(tools::file_ext(candidates)) %in% c("nc", "zip")
    ]
    if (length(candidates) == 0L) {
      return(NA_character_)
    }
    candidates[1L]
  }

  permanent_error <- function(message) {
    grepl(
      paste(
        "not auth",
        "forbidden",
        "licen[cs]e",
        "terms and conditions",
        "request.*not valid",
        "invalid request",
        "no data is available",
        "maximum request size",
        "request.*too large",
        sep = "|"
      ),
      message,
      ignore.case = TRUE
    )
  }

  download_request <- function(request) {
    job <- NULL
    last_error <- "request did not complete"
    attempts_used <- 0L
    for (attempt in seq_len(max_attempts)) {
      attempts_used <- attempt
      result <- tryCatch(
        {
          if (is.null(job)) {
            job <- suppressMessages(ecmwfr::wf_request(
              request = request,
              path = data_dir,
              user = user,
              transfer = FALSE,
              retry = 5,
              verbose = FALSE
            ))
          }

          # Permit simple character-returning mocks and remain defensive if a
          # future ecmwfr version returns a completed path at submission.
          if (is.character(job)) {
            return(job[1L])
          }

          suppressMessages(job$transfer(time_out = request_timeout))
          if (isTRUE(job$is_success())) {
            path <- job$get_file()
            try(suppressMessages(job$delete()), silent = TRUE)
            return(path)
          }
          stop("request timed out before a file became available")
        },
        error = identity
      )

      if (is.character(result)) {
        return(result)
      }
      last_error <- conditionMessage(result)
      if (permanent_error(last_error)) {
        break
      }

      if (!is.null(job) && !is.character(job)) {
        status <- tryCatch(job$get_status(), error = function(e) NA_character_)
        if (status %in% c("failed", "deleted")) {
          try(suppressMessages(job$delete()), silent = TRUE)
          job <- NULL
        }
      }

      if (attempt < max_attempts) {
        delay <- min(300, retry_delay * 2^(attempt - 1L))
        if (delay > 0) {
          Sys.sleep(delay + stats::runif(1L, 0, min(5, delay * 0.1)))
        }
        message(
          "Retrying ERA5 request '",
          request$target,
          "' (attempt ",
          attempt + 1L,
          " of ",
          max_attempts,
          ")."
        )
      }
    }

    message(
      "Failed to download request '",
      request$target,
      "' after ",
      attempts_used,
      " attempt",
      if (attempts_used == 1L) "" else "s",
      ": ",
      gsub("[[:space:]]+", " ", last_error)
    )
    NA_character_
  }

  # Download the data using the Copernicus API. A failed batch may already
  # have written valid files, so discover those files and retry missing
  # requests in order until one is exhausted rather than discarding completed
  # work.
  download_files <- character()
  if (batch) {
    message(
      "downloading ERA5 rasters using batch request... please be patient."
    )
    workers <- min(length(requests), 10L)
    batch_result <- tryCatch(
      suppressMessages(ecmwfr::wf_request_batch(
        request_list = requests,
        path = data_dir,
        user = user,
        workers = workers,
        time_out = request_timeout,
        retry = 5
      )),
      error = identity
    )
    if (inherits(batch_result, "error")) {
      message(
        "ERA5 batch request ended early: ",
        gsub("[[:space:]]+", " ", conditionMessage(batch_result)),
        ". Retrying the first missing request sequentially."
      )
    } else {
      download_files <- as.character(batch_result)
    }
  } else {
    message("downloading ERA5 rasters sequentially... please be patient.")
  }

  resolved_files <- vapply(
    requests,
    find_download_file,
    character(1),
    returned_files = download_files
  )
  missing_requests <- which(is.na(resolved_files))
  if (length(missing_requests) > 0L) {
    for (ii in missing_requests) {
      downloaded <- download_request(requests[[ii]])
      if (is.na(downloaded)) {
        break
      }
      download_files <- c(download_files, downloaded)
      resolved_files[ii] <- find_download_file(
        requests[[ii]],
        returned_files = download_files
      )
    }
  }

  downloaded_requests <- !is.na(resolved_files)
  requests <- requests[downloaded_requests]
  download_files <- resolved_files[downloaded_requests]

  if (length(download_files) == 0L) {
    stop("No data was downloaded. Please check your parameters and try again.")
  }

  # Current requests ask CDS for unarchived NetCDF files. Keep a defensive
  # fallback because CDS can still return a zip archive for some data layouts.
  for (download_file in download_files) {
    if (tolower(tools::file_ext(download_file)) != "zip") {
      next
    }

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
    hour_values <- as.numeric(substr(request$time, 1, 2))
    request_seconds <- sort(as.vector(outer(
      as.numeric(as.POSIXct(request_dates, tz = "UTC")),
      hour_values * 60 * 60,
      `+`
    )))
    request_datetimes <- .POSIXct(request_seconds, tz = "UTC")

    filename <- file.path(data_dir, request$target)
    rasters <- terra::rast(filename)
    terra::crs(rasters) <- "EPSG:4326"

    # Prefer the NetCDF time coordinate when terra exposes it. CDS can omit a
    # genuinely unavailable timestamp while still returning the other layers;
    # preserve those layers and let the chronological-prefix check below stop
    # before the gap.
    raster_datetimes <- tryCatch(
      terra::time(rasters),
      error = function(e) NULL
    )
    if (
      length(raster_datetimes) == terra::nlyr(rasters) &&
        all(!is.na(raster_datetimes))
    ) {
      if (inherits(raster_datetimes, "Date")) {
        raster_datetimes <- as.POSIXct(raster_datetimes, tz = "UTC")
      } else if (!inherits(raster_datetimes, "POSIXct")) {
        raster_datetimes <- as.POSIXct(
          raster_datetimes,
          origin = "1970-01-01",
          tz = "UTC"
        )
      } else {
        attr(raster_datetimes, "tzone") <- "UTC"
      }
      if (anyDuplicated(as.numeric(raster_datetimes))) {
        stop(sprintf(
          "NetCDF file '%s' contains duplicate timestamps.",
          basename(filename)
        ))
      }
      unexpected <- !as.numeric(raster_datetimes) %in%
        as.numeric(request_datetimes)
      if (any(unexpected)) {
        stop(sprintf(
          "NetCDF timestamps in '%s' do not match the request.",
          basename(filename)
        ))
      }
      layer_order <- order(raster_datetimes)
      rasters <- rasters[[layer_order]]
      request_datetimes <- raster_datetimes[layer_order]
    } else if (terra::nlyr(rasters) != length(request_datetimes)) {
      stop(sprintf(
        paste0(
          "Expected %d raster layer(s) in '%s', found %d and no usable ",
          "time coordinate."
        ),
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
  first_unavailable <- NULL
  for (ii in seq_along(output_datetimes)) {
    datetime_ii <- output_datetimes[ii]
    datetime_key <- format(datetime_ii, "%Y%m%d%H", tz = "UTC")
    current <- raw_rasters[[datetime_key]]
    if (is.null(current)) {
      first_unavailable <- datetime_ii
      break
    }

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
          first_unavailable <- datetime_ii
          break
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

  if (!is.null(first_unavailable)) {
    if (length(files) == 0L) {
      message(
        "No complete chronological ERA5 prefix was available; the first ",
        "missing timestamp is ",
        format(first_unavailable, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
        "."
      )
      return(list())
    }
    message(
      "ERA5 download is incomplete. Returning ",
      length(files),
      " raster",
      if (length(files) == 1L) "" else "s",
      " through ",
      format(files[[length(files)]]$valid_to, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      "; the first unavailable timestamp is ",
      format(first_unavailable, "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      "."
    )
  }

  files[["forecast"]] <- FALSE
  return(files)
}
