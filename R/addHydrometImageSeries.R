#' Add new image series (auto-fetched)
#'
#' Use this function to add a new image series, i.e. a continually updating time-series of images that can be accessed from the web or internal server location on a regular basis.
#'
#' @param location The location or location_id associated with the image series. Pass a location code as text and a location_id as a numeric.
#' @param start_datetime The datetime (as POSIXct) from which to look for images
#' @param source_fx The function to use for fetching new images. Must be an existing function in this package.
#' @param source_fx_args Additional arguments to pass to the function, in the form "{param1 = arg1}, {param2 = 'arg2'}". Each parameter = value pair needs to be enclosed in curly brackets, which might be missing here. Do not deviate from this format!
#' @param public Should the images be publicly visible?
#' @param public_delay A period in ISO 8601 format by which to delay public visibility of images.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return TRUE if successful, and a new entry in the database with images fetched.
#' @export
#'

addHydrometImageSeries <- function(location, start_datetime, source_fx, source_fx_args = NA, public = TRUE, public_delay = NA, con = hydrometConnect(silent = TRUE)) {
  #function will add entry to images_index, then trigger getNewImages from the user-specified start_datetime

  on.exit(DBI::dbDisconnect(con))

  if (inherits(location, "character")){
      location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location,  "';"))[1,1]
      if (is.na(location_id)){
        stop("The location you specified does not exist. Reminder that you should specify the location code or location_id, not the name.")
      }
  } else if (inherits(location, "numeric")) {
    location_id <- location
    check <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location_id = ", location_id))[1,1]
    if (is.na(check)){
      stop("The location_id you specified does not exist.")
    }
  } else {
    stop("Parameter 'location' must be either a character or numeric vector of length 1.")
  }


  exists <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location_id = '", location_id, "' AND img_type = 'auto'"))[1,1]
  if (!is.na(exists)){
    stop("There is already an entry for that location or location_id and for images of type 'auto' in the images_index table.")
  }
  insert <- data.frame(location_id = location_id,
                       img_type = "auto",
                       public = public,
                       first_img = start_datetime,
                       last_img = start_datetime,
                       public_delay = public_delay,
                       source_fx = source_fx,
                       source_fx_args = source_fx_args)

  DBI::dbAppendTable(con, "images_index", insert)
  res <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location_id = '", location_id, "' AND img_type = 'auto'"))[1,1]
  added <- getNewImages(image_meta_ids = res, con = con)
  if (length(added) == 0){
    warning("Failed to find or add new images. The new entry to table images_index has been deleted.")
    DBI::dbExecute(con, paste0("DELETE FROM images_index WHERE img_meta_id = ", res, ";"))
  } else {
    first_new <- DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM images WHERE img_meta_id = ", res, ";"))[1,1]
    DBI::dbExecute(con, paste0("UPDATE images_index SET first_img = '", first_new, "' WHERE img_meta_id = ", res, ";"))
    message("Added new image series for location_id ", location_id, " and type 'auto'. The new img_meta_id is ", res, ".")
  }
}
