#' Get ERA5 rasters
#'
#' @param param The parameter for which to get new rasters. Currently only "APCP_Sfc" is supported.
#' @param start_datetime The datetime from which to start looking for new rasters
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#' @param user The username to use for ECMWF authentication.
#' @param key The key to use for EMCWF authentication.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export
#'

downloadERA5 <- function(start_datetime, clip = "YT", param, user, key) {

  
  ecmwfr::wf_set_key(key = key, user = user)
  
  # Get that param is valid and fetch short form
  scrape_era5_land_metadata <- function(url = "https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation") {
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
    } else if (nchar(clip) != 2) {
      stop("Parameter clip must be a character vector of 2 characters.")
    }
  }

  prov_buff <- terra::vect("inst/extdata/prov_buffers/Provinces_buffered_300km.shp")
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

  # Load the metadata for ERA5-Land parameters
  tables <- scrape_era5_land_metadata()
  param_md <- dplyr::bind_rows(tables)

  # Check if the parameter is in the metadata
  if (!(param %in% param_md$`Variable name in CDS`)) {
    stop(sprintf("Parameter '%s' not found in metadata. Be sure to use the 'Variable name in CDS', which can be found at 'https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation'", param))
  }
  # Remove rows with NA in 'Variable name in CDS'
  param_md <- param_md[!is.na(param_md$`Variable name in CDS`), ]
  param_md <- param_md[param_md$`Variable name in CDS` == param, ]
  # Get the short name for the parameter
  param_short <- param_md$shortName

  # for the entire date range, create a monthly array of dates to download data one month at a time
  # for the remainder (current month), we create a second, daily date array
  # downloading one day at a time is relatively slow, so we try to download as much as possible in one go
  seq_months <- seq.Date(as.Date(start_datetime), as.Date(Sys.time() - 5 * 24 * 60 * 60, tz = "UTC"), by = "month")
  seq_days <- seq.Date(tail(seq_months, 1), as.Date(Sys.time() - 5 * 24 * 60 * 60, tz = "UTC"), by = "day")

  # Create a list to hold the requests
  requests <- list()

  # Create a temperary directory to store the downloaded data, from which we will create rasters to upload to AC
  data_dir <- file.path(tempdir(), "downloadERA5")
  data_dir <- normalizePath(data_dir, mustWork = FALSE)
  dir.create(data_dir)
  on.exit({
    # Clean up the temporary directory
    if (dir.exists(data_dir)) {
      unlink(data_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  # create requests for monthly data (don't include the last month, which is handled separately)
  for (ii in seq_along(seq_months)[-length(seq_months)]) {
    from_datetime <- seq_months[ii]

    from_year <- sprintf("%d", as.numeric(format(from_datetime, "%Y")))
    from_month <- sprintf("%02d", as.numeric(format(from_datetime, "%m")))
    from_day <- sprintf("%02d", as.numeric(format(from_datetime, "%d")))
    to_datetime <- seq_months[ii + 1] - 1
    to_year <- sprintf("%d", as.numeric(format(to_datetime, "%Y")))
    to_month <- sprintf("%02d", as.numeric(format(to_datetime, "%m")))
    to_day <- sprintf("%02d", as.numeric(format(to_datetime, "%d")))
    hour <- sprintf("%02d", as.numeric(format(from_datetime, "%H")))
    name <- paste0("ERA5_", param_short, "_", from_year, from_month, from_day, hour, "_to_", to_year, to_month, to_day, hour)

    request <- list(
      dataset_short_name = "reanalysis-era5-land",
      product_type = "reanalysis",
      variable = param,
      date = paste0(from_year, "-", from_month, "-", from_day, "/", to_year, "-", to_month, "-", to_day),
      time = paste0(hour, ":00"),
      format = "netcdf",
      area = area,
      target = name
    )
    requests[[length(requests) + 1]] <- request
  }

  # add requests for daily data (the last month)
  # we use the last month, which is the current month minus 5 days (latency of ERA5-Land data)
  for (ii in seq_along(seq_days)) {
    from_datetime <- seq_days[ii]

    from_year <- sprintf("%d", as.numeric(format(from_datetime, "%Y")))
    from_month <- sprintf("%02d", as.numeric(format(from_datetime, "%m")))
    from_day <- sprintf("%02d", as.numeric(format(from_datetime, "%d")))

    hour <- sprintf("%02d", as.numeric(format(from_datetime, "%H")))
    name <- paste0("ERA5_", param_short, "_", from_year, from_month, from_day, hour)

    request <- list(
      dataset_short_name = "reanalysis-era5-land",
      product_type = "reanalysis",
      variable = param,
      year = sprintf("%d", as.numeric(format(from_datetime, "%Y"))),
      month = sprintf("%02d", as.numeric(format(from_datetime, "%m"))),
      day = sprintf("%02d", as.numeric(format(from_datetime, "%d"))),
      time = paste0(hour, ":00"),
      format = "netcdf",
      area = area,
      target = name
    )
    requests[[length(requests) + 1]] <- request
  }

  # Download the data using the Copernicus API
  # Note: You need to have the Copernicus API credentials set up in your .Renviron file
  # or use the `wfr::set_key()` function to set them in your R session

  ecmwfr::wf_request_batch(
    request = requests,  # the request
    path = data_dir,
    user = user,
    workers = 10
  )

  # extract the downloaded zip files, rename the .nc files, and delete the zip files
  zip_files <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)
  for (zip_file in zip_files) {
    unzip(zip_file, exdir = data_dir)
    base_filename <- sub("\\.zip$", "", basename(zip_file))
    nc_file <- file.path(data_dir, "data_0.nc")
    if (file.exists(nc_file)) {
      file.rename(nc_file, file.path(data_dir, paste0(base_filename, ".nc")))
    }
    # Delete the zip file after processing
    file.remove(zip_file)
  }

  files <- list()
  jj <- 1
  for (request in requests) {

    name <- request$target
    # Create a string representation of the request for logging
    url = paste(names(request), as.character(request), sep = ": ", collapse = "; ")
    model <- request$dataset_short_name

    if ("date" %in% names(request)){
      is_timeseries <- TRUE
    } else {
      is_timeseries <- FALSE
    }

    # if the request is for >1 timestamp, it's a timeseries
    # loop through the raster bands and store each timestamp as an entry in 'files'

    if (is_timeseries) {

      # multi-day requests have a date range, here we parse the date range and iterate over each day
      date_range <- strsplit(request$date, "/")[[1]]
      from_date <- date_range[1]
      to_date <- date_range[2]
      seq_dates <- seq.Date(as.Date(from_date), as.Date(to_date), by = "day")

      # load the nc raster
      filename <- file.path(data_dir, paste0(request$target, ".nc"))
      rasters <- terra::rast(filename)

      for (ii in seq_along(seq_dates)) {
        datetime_ii <- as.POSIXct(seq_dates[ii], tz = "UTC")
        file <- list()
        file[["rast"]] <- rasters[[ii]]
        file[["valid_from"]] <- datetime_ii
        file[["valid_to"]] <- datetime_ii + 60*60
        file[["flag"]] <- "None"
        file[["source"]] <- "CDS"
        file[["model"]] <- model
        file[["url"]] <- url

        files[[jj]] <- file
        jj <- jj + 1
      }

    } else {

      # For single day requests, we use year, month, and day
      from_date <- as.POSIXct(paste0(request$year, "-", request$month, "-", request$day), tz = "UTC")

      # load the raster
      filename <- file.path(data_dir, paste0(request$target, ".nc"))
      raster <- terra::rast(filename)

      file <- list()
      file[["rast"]] <- raster
      file[["valid_from"]] <- from_date
      file[["valid_to"]] <- from_date + 60 * 60
      file[["flag"]] <- "None"
      file[["source"]] <- "CDS"
      file[["model"]] <- model
      file[["url"]] <- url

      files[[jj]] <- file
      jj <- jj + 1

    }
  }

  files[["forecast"]] <- FALSE
  return(files)
}
