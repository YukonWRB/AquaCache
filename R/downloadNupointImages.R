#' Get images from NuPoint SFTP server
#'
#' Fetches images from a NuPoint SFTP server. Default URL/IP address is Yukon-specific. To fetch from the WSC instead see [downloadWSCImages()]. Intended to be called by function [getNewImages()].
#'
#' @param location The location for which to get images.
#' @param start_datetime The earliest datetime to start pulling images from.
#' @param username Username to use for password-protected login.
#' @param password Password to use for protected login.
#' @param url The URL or IP address from which to get new images.
#' @param port The port on which to connect to the SFTP server.
#' @param folder The folder in which to look for new images.
#' @param save_path Optional; path in which to save the image.
#' @param delete Should the files be deleted from the SFTP site once they've been successfully been fetched?
#'
#' @return A list object of type 'response' containing the image (object$content) and metadata regarding the object and how/when it was fetched.
#' @export
#'

downloadNupointImages <- function(location, start_datetime, username = Sys.getenv("nupointUser"), password = Sys.getenv("nupointPass"), url = Sys.getenv("nupointServer"), port = Sys.getenv("nupointPort"), folder = Sys.getenv("nupointFolder"), save_path = NULL, delete = TRUE) {
  
  if (!inherits(start_datetime, "POSIXct")) {
    stop("Parameter start_datetime must be a POSIXct.")
  }
  
  # for variables left as "Sys.getenv(xxxx)", check that these exist
  if (is.null(Sys.getenv("nupointUser")) | is.null(Sys.getenv("nupointPass")) | is.null(Sys.getenv("nupointServer")) | is.null(Sys.getenv("nupointPort")) | is.null(Sys.getenv("nupointFolder")) ) {
    stop("One or more of the necessary environment variables are missing. Please ensure that nupointUser, nupointPass, nupointServer, nupointPort, and nupointFolder are set in your .Renviron file OR specify them yourself in the function call.")
  }
  
  # Create connection setup to nupoint
  nupoint <- sftp::sftp_connect(
    server = url,
    folder = folder,
    username = username,
    password = password,
    port = port,
    timeout = 120)
  
  # Check if there already exists a temporary file with the required interval, location, start_datetime, and end_datetime.
  saved_files <- list.files(paste0(tempdir(), "/downloadNupointImages"))
  
  if (length(saved_files) == 0) {
    file_exists <- FALSE
  } else {
    saved_files <- data.frame(file = saved_files,
                              datetime = as.POSIXct(basename(saved_files), format = "%Y%m%d%H%M", tz = "UTC"))
    ok <- saved_files[saved_files$datetime > .POSIXct(Sys.time(), tz = "UTC") - 2*60 , ]
    if (nrow(ok) > 0) {
      target_file <- saved_files[order(saved_files$datetime, decreasing = TRUE) , ][1,]
      tbl <- readRDS(paste0(tempdir(), "/downloadNupointImages/", target_file$file))
      file_exists <- TRUE
    } else {
      file_exists <- FALSE
    }
  }
  # If there is no file that matches necessary use, download and save for later
  if (!file_exists) {
    links <- sftp::sftp_listfiles(nupoint, verbose = FALSE)$name
    tbl <-  data.frame(link = links,
                         datetime = as.POSIXct(sub(".*_(\\d{14}).*", "\\1", links), format = "%Y%m%d%H%M%S", tz = "UTC"),
                         location = sub("^(.*)_\\d{14}.*$", "\\1", links))
    
    suppressWarnings(dir.create(paste0(tempdir(), "/downloadNupointImages")))
    name <- gsub(" ", "", .POSIXct(Sys.time(), tz = "UTC"))
    name <- gsub("-", "", name)
    name <- substr(gsub(":", "", name), 1,12)
    saveRDS(tbl, paste0(tempdir(), "/downloadNupointImages/", name, ".rds"))
  }
  
  tbl <- tbl[tbl$location == location & tbl$datetime >= start_datetime , ]
  
  if (nrow(tbl) > 0) {
    files <- data.frame()
    for (i in 1:nrow(tbl)) {
      file <- tbl[i, "link"]
      sftp::sftp_download(file, tofolder = paste0(tempdir(), "/downloadNupointImages"), sftp_connection = nupoint, verbose = FALSE)
      files[i, "file"] <- paste0(tempdir(), "/downloadNupointImages/", file)
      files[i, "datetime"] <- tbl[i, "datetime"]
      files[i, "location"] <- tbl[i, "location"]
      if (delete) {
        sftp::sftp_delete(file, sftp_connection = nupoint, verbose = FALSE)
      }
    }
  } else {
    files <- NULL
  }
  
  return(files)
}
