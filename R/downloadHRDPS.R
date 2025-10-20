#' Get HRDPS rasters
#'
#' @param parameter The parameter to fetch.
#' @param start_datetime Not a true start_datetime in the sense used in fetching other data, but rather the datetime of the last issued forecast in the database. This is compared to what's on the remote and, if different, the new forecast is fetched.
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export

downloadHRDPS <- function(
    parameter,
    start_datetime,
    clip = NULL
) {
  # check parameter 'clip'
  if (!is.null(clip)) {
    if (!inherits(clip, "character")) {
      stop("Parameter clip must be a character vector of 2 characters.")
    } else if (nchar(clip) != 2) {
      stop("Parameter clip must be a character vector of 2 characters.")
    }
  }
  if (!inherits(start_datetime, "POSIXct")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  } else {
    attr(start_datetime, "tzone") <- "UTC"
  }
  
  # Check if a new issue is available.
  curr.dt <- .POSIXct(Sys.time(), tz = "UTC")
  curr.date <- gsub("-", "", substr(curr.dt, 1, 10))
  check_url <- paste0("https://dd.weather.gc.ca/", curr.date, "/WXO-DD/model_hrdps/continental/2.5km/")
  
  # First check directories in current date url. Limit to elements of 00, 06, 12, 18
  links_now <- tryCatch({
    res <-  rvest::session(check_url) |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
    res <- grep("^(00|06|12|18)/$", res, value = TRUE)
    
    # Ensure each directory has all 48 hours of forecast available
    valid_links <- c()
    for (link in res) {
      timestep_links <- rvest::session(paste0(check_url, link)) |>
        rvest::html_elements("a") |>
        rvest::html_attr("href")
      timestep_links <- timestep_links[grep("^[0-9]{3}", timestep_links)]
      if (length(timestep_links) >= 48) {
        valid_links <- c(valid_links, link)
      }
    }
    if (is.null(valid_links)) {
      valid_links <- character(0)
    }
    valid_links
  }, error = function(e) {
    character(0)
  })
  # If nothing found, check past date url
  if (length(links_now) == 0) {
    # check past date
    curr.date <- gsub("-", "", substr(curr.dt - 60*60*24, 1, 10))
    check_url <- paste0("https://dd.weather.gc.ca/", curr.date, "/WXO-DD/model_hrdps/continental/2.5km/")
    links_now <- tryCatch({
      res <-  rvest::session(check_url) |>
        rvest::html_elements("a") |>
        rvest::html_attr("href")
      res <- grep("^(00|06|12|18)/$", res, value = TRUE)
      
      # Ensure each directory has all 48 hours of forecast available
      valid_links <- c()
      for (link in res) {
        timestep_links <- rvest::session(paste0(check_url, link)) |>
          rvest::html_elements("a") |>
          rvest::html_attr("href")
        timestep_links <- timestep_links[grep("^[0-9]{3}", timestep_links)]
        if (length(timestep_links) >= 48) {
          valid_links <- c(valid_links, link)
        }
      }
      if (is.null(valid_links)) {
        valid_links <- character(0)
      }
      valid_links
    }, error = function(e) {
      character(0)
    })
  }
  if (length(links_now) == 0) {
    stop("downloadHRDPS: Could not find any forecast directories on the remote server.")
  }
  
  # Compare start_datetime, which is actually the datetime of the last issued forecast in the database, to the latest issue available on the remote
  
  latest_issue <- data.frame(links = paste0(check_url, links_now),
                             issue = as.POSIXct(paste0(curr.date, substr(links_now, 1, 2), ":00:00"), format = "%Y%m%d%H:%M:%S", tz = "UTC")
  )
  if (start_datetime >= max(latest_issue$issue)) {
    message("downloadHRDPS: There is no new forecast available yet.")
    return(NULL)
  } else {
    message("downloadHRDPS: New forecast available. Downloading...")
    latest_issue <- latest_issue[latest_issue$issue == max(latest_issue$issue), ]
    
    base <- paste0(check_url, substr(latest_issue$links, nchar(check_url) + 1, nchar(latest_issue$links)))
    # Add links for each hour of forecast, with form 000 to 048
    hour_links <- rvest::session(base) |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
    hour_links <- hour_links[grep("^[0-9]{3}/$", hour_links
    )]
    grib_link <- substr(latest_issue$issue, 1, 13) |>
      gsub("-", "", x = _) |>
      gsub(" ", "T", x = _)
    # If for hour 00, will be missing the T00, so add it
    if (nchar(grib_link) == 8) {
      grib_link <- paste0(grib_link, "T00")
    }
    all_links <- paste0(base, hour_links, grib_link, "Z_MSC_HRDPS_", parameter, "_RLatLon0.0225_PT", gsub("/", "", hour_links), "H.grib2")
    
    # Make clip polygon
    if (!is.null(clip)) {
      clip <- prov_buff[prov_buff$PREABBR %in% clip, ] # This is package data living as shapefile in inst/extdata, loaded using file data_load.R
      if (nrow(clip) == 0) {
        clip <- NULL
      }
    }
    
    files <- list()
    clipped <- FALSE
    zero_time <- as.POSIXct(grib_link, format = "%Y%m%dT%H", tz = "UTC")
    for (i in 1:length(all_links)) {
      # Wrap in try because not all parameters have a 000 issue
      file <- list()
      link <- all_links[i]
      # Quick check to ensure the file exists (it will not for 0 hour forecasts of some parameters)
      resp <- httr::HEAD(link, httr::timeout(10))
      if (httr::http_error(resp)) next
      
      rast <- terra::rast(link)[[1]]
      file[["units"]] <- terra::units(rast) # Units is fetched now because the clip operation seems to remove them.
      rast <- terra::project(rast, "epsg:4326") # Project to WGS84 (EPSG:4326)
      if (!clipped) {
        if (!is.null(clip)) {
          clip <- terra::project(clip, rast) # project clip vector to crs of the raster
        }
        clipped <- TRUE # So that project doesn't happen after the first iteration
      }
      if (!is.null(clip)) {
        rast <- terra::mask(rast, clip) # Makes NA values beyond the boundary of clip
        rast <- terra::trim(rast) # Trims the NA values
      }
      file[["rast"]] <- rast
      file[["valid_from"]] <- zero_time + (i-1) * 3600
      file[["valid_to"]] <- zero_time + i * 3600
      file[["flag"]] <- NA
      file[["source"]] <- link
      file[["issued"]] <- latest_issue$issue
      file[["model"]] <- "HRDPS"
      files[[i]] <- file
    }
    if (length(files) == 0) {
      message("downloadHRDPS: No new rasters were found for the specified parameter.")
      return(NULL)
    }
    files[["forecast"]] <- TRUE
    files[["issued"]] <- latest_issue$issue
    message("downloadHRDPS: finished downloading new rasters.")
    
    return(files)
  }
}
