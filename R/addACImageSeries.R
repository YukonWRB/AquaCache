#' Add new image series (auto-fetched)
#'
#' Use this function to add a new image series, i.e. a continually updating time-series of images that can be accessed from the web or internal server location on a regular basis.
#'
#' @param location The location or location_id associated with the image series. Pass a location code as text and a location_id as a numeric.
#' @param start_datetime The datetime (as POSIXct) from which to look for images
#' @param source_fx The function to use for fetching new images. Must be an existing function in this package.
#' @param source_fx_args Additional arguments to pass to the function, in the form "\{param1 = arg1\}, \{param2 = 'arg2'\}". Each parameter = value pair needs to be enclosed in curly brackets, which might be missing here. Do not deviate from this format!
#' @param share_with Which user groups should the image series be shared with? Default is 'public_reader', the public group.
#' @param visibility_public How should the image location be publicly visible? Options are 'exact', 'region', 'jitter'. 
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. If left NULL, a connection will be attempted using AquaConnect() and closed afterwards.
#'
#' @return TRUE if successful, and a new entry in the database with images fetched.
#' @export
#'

addACImageSeries <- function(location, start_datetime, source_fx, source_fx_args = NA, share_with = "public_reader", visibility_public = 'exact', con = NULL) {
  # function will add entry to image_series, then trigger getNewImages from the user-specified start_datetime

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (inherits(location, "character")) {
      location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location,  "';"))[1,1]
      if (is.na(location_id)) {
        stop("The location you specified does not exist. Reminder that you should specify the location code (text) or location_id (numeric), not the name.")
      }
  } else if (inherits(location, "numeric")) {
    location_id <- location
    check <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location_id = ", location_id))[1,1]
    if (is.na(check)) {
      stop("The location_id you specified does not exist.")
    }
  } else {
    stop("Parameter 'location' must be either a character or numeric vector of length 1.")
  }

  if (!visibility_public %in% c('exact', 'region', 'jitter')) {
    stop("The 'visibility_public' parameter must be either 'exact', 'region', or 'jitter'.")
  }
  if (!inherits(share_with, "character")) {
    stop("The 'share_with' parameter must be a character vector.")
  }
  
  exists <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM image_series WHERE location_id = ", location_id, " AND img_type = 'auto'"))[1,1]
  if (!is.na(exists)) {
    stop("There is already an entry for that location or location_id and for images of type 'auto' in the image_series table.")
  }
  insert <- data.frame(location_id = location_id,
                       img_type = "auto",
                       first_img = start_datetime,
                       last_img = start_datetime,
                       source_fx = source_fx,
                       source_fx_args = source_fx_args,
                       share_with = paste0("{", paste(share_with, collapse = ", "), "}"),
                       visibility_public = visibility_public,
                       active = TRUE,
                       description = "Image series automatically taken from a web or server location.")

  DBI::dbAppendTable(con, "image_series", insert)
  res <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM image_series WHERE location_id = ", location_id, " AND img_type = 'auto'"))[1,1]
  added <- getNewImages(image_meta_ids = res, con = con)
  if (length(added) == 0) {
    warning("Failed to find or add new images. The new entry to table image_series has been deleted.")
    DBI::dbExecute(con, paste0("DELETE FROM image_series WHERE img_meta_id = ", res, ";"))
  } else {
    first_new <- DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM images WHERE img_meta_id = ", res, ";"))[1,1]
    DBI::dbExecute(con, paste0("UPDATE image_series SET first_img = '", first_new, "' WHERE img_meta_id = ", res, ";"))
    message("Added new image series for location_id ", location_id, " and type 'auto'. The new img_meta_id is ", res, ".")
  }
}
