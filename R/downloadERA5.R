#' Get ERA5 rasters
#'
#' @param param The parameter for which to get new rasters. Currently only "APCP_Sfc" is supported.
#' @param start_datetime The datetime from which to start looking for new rasters.
#' @param end_datetime The datetime until which to look for new rasters. If NULL, the current datetime is used.
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#' @param user The username to use for ECMWF authentication.
#' @param key The key to use for EMCWF authentication.
#' @param hrs ERA5 data is provided in hourly chunks. Specify a vector of hours from 0 to 23 specifying the hourly rasters to bring in from start_datetime to end_datetime. Default is for 0 hours only, so each day at midnight/00:00 UTC.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export
#'

downloadERA5 <- function(start_datetime, end_datetime = .POSIXct(Sys.time(), tz = "UTC") - 60*60*24*5, clip = "YT", param, user, key, hrs = c(0)) {
  
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
  max_end <- as.POSIXct(Sys.time() - 5 * 24 * 60 * 60, tz = "UTC")
  if (end_datetime > max_end) {
    end_datetime <- max_end
  }
  if (start_datetime > end_datetime) {
    stop("Parameter 'start_datetime' must be before 'end_datetime'.")
  }
  
  # 'hrs' might have been passed in a a character vector like 0,6,12,18. Separate on the commas so it can be made a numeric vector
  if (inherits(hrs, "character")) {
    hrs <- strsplit(hrs, ",")[[1]]
  }
  
  # Check that 'hrs' is a numeric vector of integers between 0 and 23
  if (!inherits(hrs, "numeric")) hrs <- as.numeric(hrs)
  if (!is.numeric(hrs) || any(hrs < 0) || any(hrs > 23) || any(!is.finite(hrs)) || any(hrs %% 1 != 0)) {
    stop("Parameter 'hrs' must be a numeric vector of integers between 0 and 23.")
  }
  
  suppressMessages(ecmwfr::wf_set_key(key = key, user = user))
  
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
  param_md <- suppressMessages(dplyr::bind_rows(tables))
  
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
  seq_months <- seq.Date(as.Date(start_datetime), as.Date(end_datetime), by = "month")
  seq_days <- seq.Date(tail(seq_months, 1), as.Date(end_datetime), by = "day")
  
  # Create a list to hold the requests
  requests <- list()
  
  # Create a temperary directory to store the downloaded data, from which we will create rasters to upload to AC
  data_dir <- file.path(tempdir(), "downloadERA5")
  data_dir <- normalizePath(data_dir, mustWork = FALSE)
  # Clean up the directory in case it has leftover files from previous runs
  unlink(data_dir, recursive = TRUE, force = TRUE)
  suppressWarnings(dir.create(data_dir))
  
  # NOT USED because the files need to live on to the getNewRasters function
  # on.exit({
  #   # Clean up the temporary directory on exit also
  #   if (dir.exists(data_dir)) {
  #     unlink(data_dir, recursive = TRUE, force = TRUE)
  #   }
  # }, add = TRUE)
  
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
    
    for (hh in hrs) {
      hour <- sprintf("%02d", hh)
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
  }
  
  # add requests for daily data (the last month)
  # we use the last month, which is the current month minus 5 days (latency of ERA5-Land data)
  for (ii in seq_along(seq_days)) {
    from_datetime <- seq_days[ii]
    
    from_year <- sprintf("%d", as.numeric(format(from_datetime, "%Y")))
    from_month <- sprintf("%02d", as.numeric(format(from_datetime, "%m")))
    from_day <- sprintf("%02d", as.numeric(format(from_datetime, "%d")))
    
    for (hh in hrs) {
      hour <- sprintf("%02d", hh)
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
  }
  
  # Download the data using the Copernicus API
  message("downloading ERA5 rasters... this could take a while.")
  workers <- max(length(requests), 10)
  zip_files <- suppressMessages(
    ecmwfr::wf_request_batch(
      request = requests,  # the request
      path = data_dir,
      user = user,
      workers = workers,
      retry = 30
    )
  )
  message("ERA 5 download completed.")
  if (length(zip_files) == 0) {
    stop("No data was downloaded. Please check your parameters and try again.")
  }
  
  # extract the downloaded zip files, rename the .nc files, and delete the zip files
  # zip_files <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)
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
    
    # if the request is for > 1 timestamp, it's a timeseries
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
      hour_val <- as.numeric(substr(request$time, 1, 2))
      
      for (ii in seq_along(seq_dates)) {
        datetime_ii <- as.POSIXct(seq_dates[ii], tz = "UTC") + hour_val * 60* 60
        file <- list()
        file[["rast"]] <- rasters[[ii]]
        file[["valid_from"]] <- datetime_ii
        file[["valid_to"]] <- datetime_ii + 60 * 60
        file[["flag"]] <- NA
        file[["source"]] <- "ECMWF download"
        file[["model"]] <- model
        file[["url"]] <- url
        file[["units"]] <- terra::units(rasters[[ii]])
        file[["issued"]] <- as.POSIXct(as.numeric(gsub("sd_valid_time=", "", names(rasters[[ii]])))) + 5 * 60 * 60 * 24
        
        if (datetime_ii >= start_datetime && datetime_ii <= end_datetime) {
          files[[jj]] <- file
          jj <- jj + 1
        }
      }
    } else {
      
      # For single day requests, we use year, month, and day
      time_hour <- as.numeric(substr(request$time, 1, 2))
      from_date <- as.POSIXct(paste0(request$year, "-", request$month, "-", request$day), tz = "UTC") + time_hour * 60 * 60
      
      # load the raster
      filename <- file.path(data_dir, paste0(request$target, ".nc"))
      raster <- terra::rast(filename)
      
      file <- list()
      file[["rast"]] <- raster
      file[["valid_from"]] <- from_date
      file[["valid_to"]] <- from_date + 60 * 60
      file[["flag"]] <- NA
      file[["source"]] <- "ECMWF API"
      file[["model"]] <- model
      file[["url"]] <- url
      file[["units"]] <- terra::units(raster)
      file[["issued"]] <- as.POSIXct(as.numeric(gsub("sd_valid_time=", "", names(raster))), tz = "UTC")
      
      if (from_date >= start_datetime && from_date <= end_datetime) {
        files[[jj]] <- file
        jj <- jj + 1
      }
    }
  }
  
  files[["forecast"]] <- FALSE
  return(files)
}
