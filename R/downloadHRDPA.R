#' Get HRDPA rasters
#'
#' @param start_datetime The datetime from which to start looking for new rasters
#' @param url The url from which to get new rasters.
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip.
#'
#' @return A list of lists, where each element consists of the target raster as well as associated attributes.
#' @export
#'

downloadHRDPA <- function(start_datetime, url = "https://dd.weather.gc.ca/model_hrdpa/2.5km/", clip = NULL) {

  # check parameter 'clip'
  if (!is.null(clip)){
    if (!inherits(clip, "character")){
      stop("Parameter clip must be a character vector of 2 characters.")
    } else if (nchar(clip) != 2){
      stop("Parameter clip must be a character vector of 2 characters.")
    }
  }

  # Check if there already exists a temporary file with the required interval, location, start_datetime, and end_datetime.
  saved_files <- list.files(paste0(tempdir(), "/downloadHRDPA"))
  if (length(saved_files) == 0){
    file_exists <- FALSE
  } else {
    saved_files <- data.frame(file = saved_files,
                              datetime = as.POSIXct(saved_files, format = "%Y%m%d%H%M.rds"))
    ok <- saved_files[saved_files$datetime > Sys.time()+10*60 , ]
    if (nrow(ok) > 0){
      target_file <- saved_files[order(saved_files$datetime, decreasing = TRUE) , ][1,]
      available <- readRDS(paste0(tempdir(), "/downloadHRDPA/", target_file$file))
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
  }
  # If there is no file that matches necessary use, download and save for later
  if (!file_exists){
    available <- data.frame()
    for (i in c("00", "06", "12", "18")){
      links <- rvest::session(paste0(url, i)) |>
        rvest::html_elements("a") |>
        rvest::html_attr("href")
      links <- links[grep("^[0-9]{8}T[0-9]{2}Z", links)]
      links <- links[grep("Accum6h", links)] #only retain the links we're interested in
      tmp <- data.frame(link = links,
                                    datetime = as.POSIXct(substr(links, 1, 11), format = "%Y%m%dT%H", tz = "UTC"),
                                    prelim = FALSE)
      tmp$link <- paste0(i, "/", links)
      available <- rbind(available, tmp)
    }
    available[grep("Prelim", available$link), "prelim"] <- TRUE
    suppressWarnings(dir.create(paste0(tempdir(), "/downloadHRDPA")))
    name <- gsub(" ", "", Sys.time())
    name <- gsub("-", "", name)
    name <- substr(gsub(":", "", name), 1,12)
    saveRDS(available, paste0(tempdir(), "/downloadHRDPA/", name, ".rds"))
  }

  #Now filter by start_datetime, discard prelim rasters if non-prelim exists
  available <- available[available$datetime >= start_datetime , ]
  available <- available[order(available$datetime), ]
  duplicates <- duplicated(available$datetime, fromLast = TRUE) | duplicated(available$datetime)
  available <- available[!(available$prelim & duplicates) | !duplicates, ]

  #Make clip polygon
  extent <- paste(clip, collapse="_")
  if (!is.null(clip)){
    clip <- prov_buff[prov_buff$PREABBR %in% clip, ] #This is package data living as shapefile in inst/extdata, loaded using file data_load.R
    if (nrow(clip) == 0){
      clip <- NULL
    }
  }

  if (nrow(available) > 0){
    files <- list()
    clipped <- FALSE
    for (i in 1:nrow(available)){
      file <- list()
      download_url <- paste0(url, "/", available[i, "link"[]])
      rast <- terra::rast(download_url)[[1]]
      if (clipped == FALSE){
        if (!is.null(clip)){
          clip <- terra::project(clip, rast) #project clip vector to crs of the raster
        }
        clipped <- TRUE #So that project doesn't happen after the first iteration
      }
      if (!is.null(clip)){
        rast <- terra::mask(rast, clip) #Makes NA values beyond the boundary of clip
        rast <- terra::trim(rast) #Trims the NA values
      }
      file[["rast"]] <- rast
      file[["valid_from"]] <- available[i, "datetime"]
      file[["valid_to"]] <- available[i, "datetime"] + 60*60*6
      file[["description"]] <- if (available[i, "prelim"]) "PRELIMINARY" else NA
      file[["units"]] <- terra::units(rast)
      file[["source"]] <- download_url
      file[["model"]] <- "HRDPA"
      files[[i]] <- file
    }
  } else {
    files <- NULL
  }

  return(files)
}
