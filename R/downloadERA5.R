#' Get ERA5 rasters
#' 
#' @description Interfaces with the ecmwfr package to download ERA5-Land reanalysis data from the ECMWF Copernicus Climate Data Store (CDS). The function downloads the data in netCDF format and returns a list of rasters with associated metadata. Data are downloaded sequentially starting from the earliest requested timestamp. If a download fails, the function returns the rasters that were successfully downloaded up to that point. Rasters are returned in the geographic coordinate reference system (EPSG:4326).
#'
#' @param param The parameter for which to get new rasters. Currently only "APCP_Sfc" is supported.
#' @param start_datetime The datetime from which to start looking for new rasters. This date does not need to align with the first day of a month. Portions of the first and last months are downloaded day by day while any complete months in between are requested in a single call .Specify as POSIXct or something coercible to POSIXct; coercion will be done with to UTC time zone.
#' @param end_datetime The datetime until which to look for new rasters. If NULL, the current datetime is used. Specify as POSIXct or something coercible to POSIXct; coercion will be done with to UTC time zone.
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#' @param user The username to use for ECMWF authentication.
#' @param key The key to use for EMCWF authentication.
#' @param hrs ERA5 data is provided in hourly chunks. Specify a vector of hours from 0 to 23 specifying the hourly rasters to bring in from start_datetime to end_datetime. Default is for 0 hours only, so each day at midnight/00:00 UTC.
#' @param batch Should a batch request be used or should downloads be sequential? Batch request can be much quicker but will fail if any of the requests fail. Sequential runs each request one by one from the earliest possible raster so that if one fails rasters are returned up to the last successful download.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export
#'

downloadERA5 <- function(start_datetime, end_datetime = .POSIXct(Sys.time(), tz = "UTC"), clip = "YT", param, user, key, hrs = c(0), batch = TRUE) {
  
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
  
  prov_buff <- terra::vect(system.file("extdata/prov_buffers/Provinces_buffered_300km.shp", package = "YGwater"))
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
  
  # Build download requests a month at a time.  Full months are requested in a single call while partial months are requested day by day so that only the necessary days are downloaded.
  # Create a list to hold the requests
  requests <- list()
  
  # helper to compute the first day of the next month
  next_month <- function(x) seq.Date(as.Date(x), by = "month", length.out = 2)[2]
  
  current_day <- as.Date(start_datetime)
  end_day <- as.Date(end_datetime)
  
  while (current_day <= end_day) {
    month_start <- as.Date(format(current_day, "%Y-%m-01"))
    month_end <- next_month(month_start) - 1
    
    range_start <- current_day
    range_end <- min(end_day, month_end)
    
    if (range_start == month_start && range_end == month_end) {
      # request the entire month in a single call
      for (hh in hrs) {
        hour <- sprintf("%02d", hh)
        name <- paste0(
          "ERA5_", param_short, "_",
          format(month_start, "%Y%m%d"), hour,
          "_to_",
          format(month_end, "%Y%m%d"), hour
        )
        
        request <- list(
          dataset_short_name = "reanalysis-era5-land",
          product_type = "reanalysis",
          variable = param,
          date = paste0(
            format(month_start, "%Y-%m-%d"), "/",
            format(month_end, "%Y-%m-%d")
          ),
          time = paste0(hour, ":00"),
          format = "netcdf",
          area = area,
          target = name
        )
        requests[[length(requests) + 1]] <- request
      }
    } else {
      # partial month - request day by day
      seq_days <- seq.Date(range_start, range_end, by = "day")
      for (i in 1:length(seq_days)) {
        dd <- seq_days[[i]]
        for (hh in hrs) {
          hour <- sprintf("%02d", hh)
          name <- paste0(
            "ERA5_", param_short, "_",
            format(dd, "%Y%m%d"), hour
          )
          
          request <- list(
            dataset_short_name = "reanalysis-era5-land",
            product_type = "reanalysis",
            variable = param,
            year = format(dd, "%Y"),
            month = format(dd, "%m"),
            day = format(dd, "%d"),
            time = paste0(hour, ":00"),
            format = "netcdf",
            area = area,
            target = name
          )
          requests[[length(requests) + 1]] <- request
        }
      }
    }
    current_day <- range_end + 1
  }
  
  # Download the data using the Copernicus API
  if (batch) {
    message("downloading ERA5 rasters using batch request... please be patient.")
    workers <- max(length(requests), 10)
    zip_files <- suppressMessages(
      ecmwfr::wf_request_batch(
        request_list = requests,  # the requests we built above
        path = data_dir,
        user = user,
        workers = workers,
        retry = 30
      )
    )
  } else {
    # Download the data using the Copernicus API sequentially so that a failure does not discard already downloaded rasters
    message("downloading ERA5 rasters sequentially... please be patient.")
    zip_files <- c()
    downloaded_requests <- list()
    download_failed <- FALSE

    # If interactive, show progress bar
    num_requests <- length(requests)
    if (interactive()) {
      pb <- utils::txtProgressBar(min = 0, max = num_requests, style = 3)
      on.exit(utils::close(pb), add = TRUE)
    }
    for (ii in num_requests) {
      req <- requests[[ii]]
      if (interactive()) {
        utils::setTxtProgressBar(pb, ii)
      }
      tryCatch({
        # use invisible and capture.output so that the progress bar from wf_request is suppressed
        invisible(capture.output(
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
        zip_files <- c(zip_files, zf)
        downloaded_requests[[length(downloaded_requests) + 1]] <- req
      }, error = function(e) {
        message(sprintf("Failed to download request '%s': %s", req$target, e$message))
        download_failed <<- TRUE
      })
      if (download_failed) break
    }

    requests <- downloaded_requests
    
    if (download_failed) message("Download incomplete due to an error. Returning available rasters only.")
  }
  
  
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
      terra::crs(rasters) <- "EPSG:4326"
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
      file[["issued"]] <- NA
      
      if (from_date >= start_datetime && from_date <= end_datetime) {
        files[[jj]] <- file
        jj <- jj + 1
      }
    }
  }
  
  files[["forecast"]] <- FALSE
  return(files)
}
