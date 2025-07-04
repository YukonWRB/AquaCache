#' Get HRDPS rasters
#'
#' @param parameter The parameter to fetch. 
#' @param start_datetime Not a true start_datetime in the sense used in fetching other data, but rather the datetime of the last issued forecast in the database. This is compared to what's on the remote and, if different, the new forecast is fetched.
#' @param url The url from which to get new rasters.
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export

downloadHRDPS <- function(parameter, start_datetime, url = "https://dd.weather.gc.ca/model_hrdps/continental/2.5km/", clip = NULL) {
  
  # check parameter 'clip'
  if (!is.null(clip)) {
    if (!inherits(clip, "character")) {
      stop("Parameter clip must be a character vector of 2 characters.")
    } else if (nchar(clip) != 2) {
      stop("Parameter clip must be a character vector of 2 characters.")
    }
  }
  
  # Check if there already exists a temporary file with the required interval, location, start_datetime, and end_datetime.
  saved_files <- list.files(paste0(tempdir(), "/downloadHRDPS"))
  if (length(saved_files) == 0) {
    file_exists <- FALSE
  } else {
    saved_files <- data.frame(file = saved_files,
                              datetime = as.POSIXct(saved_files, format = "%Y%m%d%H%M.rds"))
    ok <- saved_files[saved_files$datetime > Sys.time() - 10*60, ]
    if (nrow(ok) > 0) {
      target_file <- saved_files[order(saved_files$datetime, decreasing = TRUE) , ][1,]
      available <- readRDS(paste0(tempdir(), "/downloadHRDPS/",target_file$file))
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
  }
  # If there is no file that matches necessary use, download links and save for later
  if (!file_exists) {
    available <- data.frame()
    for (i in c("00", "06", "12", "18")) {
      links <- rvest::session(paste0(url, i)) |>
        rvest::html_elements("a") |>
        rvest::html_attr("href")
      links <- links[grep("^[0-9]{3}", links)]
      links <- gsub("/", "", links)
      # At this point we have **n** links, one for each hour of the forecast. Within each of these links we have **m** links, one for each parameter.
      tmp_available <- data.frame()
      for (j in 1:length(links)) {
        timestep_links <- rvest::session(paste0(url, i, "/", links[j])) |>
          rvest::html_elements("a") |>
          rvest::html_attr("href")
        timestep_links <- timestep_links[grep("^[0-9]{8}T[0-9]{2}Z", timestep_links)]
        if (length(timestep_links) > 0) {
          tmp <- data.frame(link = paste0(url, i, "/", links[j], "/", timestep_links),
                            issue = as.POSIXct(substr(timestep_links, 1, 11), format = "%Y%m%dT%H", tz = "UTC"),
                            valid = as.POSIXct(substr(timestep_links, 1, 11), format = "%Y%m%dT%H", tz = "UTC") + as.numeric(links[j]) * 60 * 60,
                            parameter = sub(".*HRDPS[-_](.*?)_RLatLon.*", "\\1", timestep_links))
          tmp_available <- rbind(tmp_available, tmp)
        }
      }
      available <- rbind(available, tmp_available)
    }
    suppressWarnings(dir.create(paste0(tempdir(), "/downloadHRDPS")))
    name <- gsub(" ", "", Sys.time())
    name <- gsub("-", "", name)
    name <- substr(gsub(":", "", name), 1,12)
    saveRDS(available, paste0(tempdir(), "/downloadHRDPS/", name, ".rds"))
  }
  
  #Now filter by parameter and latest issue with all data available
  available <- available[available$parameter == parameter, ]
  available <- if (nrow(available[available$issue == max(available$issue), ]) == 48) available[available$issue == max(available$issue), ] else available[available$issue == max(available$issue) - 60*60*6, ]
  
  
  if (start_datetime >= unique(available$issue)) {
    message("downloadHRDPS: There is no new forecast available yet.")
    return()
  } else {
    message("downloadHRDPS: New forecast available. Downloading...")
  }
  
  #Make clip polygon
  extent <- paste(clip, collapse = "_")
  if (!is.null(clip)) {
    clip <- prov_buff[prov_buff$PREABBR %in% clip, ] #This is package data living as shapefile in inst/extdata, loaded using file data_load.R
    if (nrow(clip) == 0) {
      clip <- NULL
    }
  }
  
  if (nrow(available) > 0) {
    files <- list()
    clipped <- FALSE
    for (i in 1:nrow(available)) {
      file <- list()
      download_url <- available[i, "link"[]]
      rast <- terra::rast(download_url)[[1]]
      rast <- terra::project(rast, "epsg:4326") #Project to WGS84 (EPSG:4326)
      file[["units"]] <- terra::units(rast) #Units is fetched now because the clip operation seems to remove them.
      if (!clipped) { #Only project and clip once
        if (!is.null(clip)) {
          clip <- terra::project(clip, rast) #project clip vector to crs of the raster
        }
        clipped <- TRUE #So that project doesn't happen after the first iteration
      }
      if (!is.null(clip)) {
        rast <- terra::mask(rast, clip) #Makes NA values beyond the boundary of clip
        rast <- terra::trim(rast) #Trims the NA values
      }
      file[["rast"]] <- rast
      file[["valid_from"]] <- available[i, "valid"] - 60*60*1
      file[["valid_to"]] <- available[i, "valid"]
      file[["issued"]] <- available[i, "issue"]
      file[["source"]] <- download_url
      file[["model"]] <- "HRDPS"
      files[[i]] <- file
    }
    files[["forecast"]] <- TRUE
    files[["issued"]] <- max(available$issue)
  } else {
    files <- NULL
  }
  
  message("downloadHRDPS: finished downloading new rasters, returning.")
  return(files)
}
