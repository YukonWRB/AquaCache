#' Get images from the WSC
#'
#' Fetches auto images from the WSC. Default URL is for the Yukon, adjust for other cameras/locations. Intended to be called by function [getNewImages()]
#'
#' @param location The location for which to get images.
#' @param start_datetime The earliest datetime to start pulling images from.
#' @param username Username to use for password-protected login.
#' @param password Password to use for protected login.
#' @param url The URL from which to get new images
#' @param save_path Optional; path in which to save the image.
#'
#' @return A list object of type 'response' containing the image (object$content) and metadata regarding the object and how/when it was fetched.
#' @export
#'

downloadWSCImages <- function(location, start_datetime, username = Sys.getenv("ECCCUSER"), password = Sys.getenv("ECCCPASS"), url = "https://collaboration.cmc.ec.gc.ca/cmc/hydrometric_additionalData/FieldData/YT/", save_path = NULL) {

  # Check if there already exists a temporary file with the required interval, location, start_datetime, and end_datetime.
  saved_files <- list.files(paste0(tempdir(), "/downloadWSCImages"))

  if (length(saved_files) == 0) {
    file_exists <- FALSE
  } else {
    saved_files <- data.frame(file = saved_files,
                              datetime = as.POSIXct(saved_files, format = "%Y%m%d%H%M.rds"), tz = "UTC")
    ok <- saved_files[saved_files$datetime > Sys.time() - 10*60 , ]
    if (nrow(ok) > 0) {
      target_file <- saved_files[order(saved_files$datetime, decreasing = TRUE) , ][1,]
      tbl <- readRDS(paste0(tempdir(), "/downloadWSCImages/", target_file$file))
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
  }
  # If there is no file that matches necessary use, download and save for later
  if (!file_exists) {
    links <- rvest::session(url, httr::authenticate(username, password)) |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")

    links <- links[grep("^[0-9]{2}[A-Za-z]{2}[0-9]{3}", links)]
    tbl <- data.frame(link = links,
                      location = sub("^([0-9]{2}[A-Za-z]{2}[0-9]{3}).*", "\\1", links),
                      datetime = as.POSIXct(sub(".*_(\\d{8}T\\d{6}Z).*", "\\1", links), format = "%Y%m%dT%H%M%SZ", tz = "UTC"))
    suppressWarnings(dir.create(paste0(tempdir(), "/downloadWSCImages")))
    name <- gsub(" ", "", Sys.time())
    name <- gsub("-", "", name)
    name <- substr(gsub(":", "", name), 1,12)
    saveRDS(tbl, paste0(tempdir(), "/downloadWSCImages/", name, ".rds"))
  }

  tbl <- tbl[tbl$location == location & tbl$datetime >= start_datetime , ]

  if (nrow(tbl) > 0) {
    files <- list()
    for (i in 1:nrow(tbl)) {
      download_url <- paste0(url, "/", tbl[i, "link"[]])
      file <- httr::GET(download_url, config = httr::authenticate(username, password))
      file$timestamp <- tbl[i, "datetime"]
      files[[tbl[i, "link"]]] <- file
    }
  } else {
    files <- NULL
  }

  return(files)
}
