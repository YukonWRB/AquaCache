#' Add images to aquacache database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' Images auto-generated on a regular basis should be updated in the database using function [getNewImages()] instead of this function. For one-off images, read on.
#'
#' This function facilitates the addition of one image at a time to the database in the 'images' table. Each image must be linked to a specific location_id from the locations table. Adding an image directly to the database is not possible since the file must be converted to a binary object before loading. See [YGwater::getImage()] to get the image out again.
#'
#' @param object Valid path including extension to the image to upload, or an object of class 'response' such as that provided by [downloadWSCImages()].
#' @param img_meta_id The img_meta_id, from the table images_index, corresponding to the image location and type. Set NULL if there is no img_meta_id yet.
#' @param datetime The datetime the image was taken at, as a POSIXct object or something that can be coerced to one. If not POSIXct timezone is assumed to be UTC.
#' @param fetch_datetime The datetime the image was retrieved (optional). If not POSIXct timezone is assumed to be UTC.
#' @param description A description of the image. Pass as text.
#' @param owner The owner of the image. Pass as text.
#' @param contributor The contributor of the image. Pass as text.
#' @param share_with Which user groups should the image be shared with. Default is 'public_reader', the public group. Pass as a numeric vector.
#' @param location If no img_meta_id exists yet: the location or location_id with which to associate the document (must be in the database). Pass a location code as text and a location_id as a numeric. If img_meta_id is specified, this parameter is ignored.
#' @param image_type If no img_meta_id exists yet: the type of image: 'auto', or 'manual'. Pass as text.
#' @param con A connection to the database. Default NULL uses AquaConnect() and close the connection afterwards.
#'
#' @return TRUE if an image was properly added to the database.
#' @export

insertACImage <- function(object, img_meta_id, datetime, fetch_datetime = NULL, description = NULL, owner = NULL, contributor = NULL, share_with = "public_reader", location = NULL, image_type = NULL, con = NULL) {

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  
  # TODO: Modify this function to work with a file and NO img_meta_id OR an object AND an img_meta_id, working with either programmatic ingests or manual uploads.
  
  #Checks
  if (length(location) > 1) {
    stop("You can only specify one location at a time.")
  }
  if (nchar(description) < 5) {
    stop("Minimum character length for 'description' is 5. Try harder.")
  }
  if (!is.null(img_meta_id)) {
    img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE img_meta_id = ", img_meta_id))[1,1]
    if (is.na(img_meta_id)) {
      stop("The img_meta_id you specified does not exist. Try again. If you need to create a new entry see the help file.")
    }
  } else { # See if need to create the img_meta_id and corresponding fields
    if (is.null(location) | is.null(image_type)) {
      stop("Parameter 'img_meta_id' was set to NULL, but 'location' and/or 'image_type' is also NULL. Refer to the help file to fix the problem.")
    }
    if (!inherits(image_type, "character")) {
      stop("The parameter image_type should be a character vector of 1")
    }
    if (length(location) != 1) {
      stop("The parameter location should be a character vector of 1 OR a numeric vector of 1 (see help file).")
    }
    if (inherits(location, "character")) {
      location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location,  "';"))[1,1]
      if (is.na(location_id)) {
        stop("The location you specified does not exist. Reminder that you should specify the location code or location_id, not the location name.")
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
    if (!inherits(share_with, "character")) {
      stop("The 'share_with' parameter must be a character vector.")
    }
    if (!inherits(owner, "character")) {
      stop("The 'owner' parameter must be a character vector.")
    }
    if (!inherits(contributor, "character")) {
      stop("The 'contributor' parameter must be a character vector.")
    }

    #See if the id exists first
    img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location_id = '", location_id, "' AND img_type = '", image_type, "'"))[1,1]
    if (is.na(img_meta_id)) { #Create the img_meta_id
      message("It looks like this is the first image of type ", image_type, " entered for location_id ", location_id, ". Creating an entry in table images_index. This series of images will be set to share_with = 'public_reader' and visibility_public = 'exact'")
      DBI::dbExecute(con, paste0("INSERT INTO images_index (location_id, img_type, first_img) VALUES ('", location_id, "', '", image_type, "', '", Sys.time(), "');"))
      img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location_id = '", location_id, "' AND img_type = '", image_type, "'"))[1,1]
    } else {
      warning("There is alreay an entry corresponding to this location_id and image_type. Using the img_meta_id ", img_meta_id, ".")
    }
  }

  if (inherits(datetime, "character")) {
    as.POSIXct(datetime, tz = "UTC")
  }
  if (!inherits(datetime, "POSIXct")) {
    stop("Datetime must be a POSIXct object or something that can be coerced to one.")
  }
  datetime <- lubridate::floor_date(datetime, "second")
  
  if (!is.null(fetch_datetime)) {
    if (inherits(fetch_datetime, "character")) {
      as.POSIXct(fetch_datetime, tz = "UTC")
    }
    if (!inherits(fetch_datetime, "POSIXct")) {
      stop("fetch_datetime must be a POSIXct object or something that can be coerced to one.")
    }
  }

  #get the extension, and also the file itself as RAW
  if (inherits(object, "response")) {
    extension <- tools::file_ext(object$url)
    file <- object$content
  } else if (inherits(object, "character")) {
    extension <- tools::file_ext(object)
    file <- hexView::readRaw(object)$fileRaw
  }

  #Add to the database and update tables
  exist_img <- DBI::dbGetQuery(con, paste0("SELECT datetime FROM images WHERE datetime = '", datetime, "' AND img_meta_id = ", img_meta_id, ";"))[1,1]
  update <- FALSE
  if (!is.na(exist_img)) {
    warning("There is already an image in the database for this img_meta_id and datetime ", datetime, ". Updating the image, keeping the same img_id.")
    update <- TRUE
  }
  
  if (update) {
    DBI::dbExecute(con, paste0("UPDATE images SET ", if (!is.null(fetch_datetime)) paste0("fetch_datetime = '", fetch_datetime, "', "), if (!is.null(description)) paste0("description = '", description, "', "), if (!is.null(owner)) paste0("owner = '", owner, "', "), if (!is.null(contributor)) paste0("contributor = '", contributor, "', "), "share_with = '{", paste(share_with, collapse = ","), "}', format = '", extension, "', file = '\\x", paste0(file, collapse = ""), "' WHERE img_meta_id = ", img_meta_id, " AND datetime = '", datetime, "';"))
  } else {
    DBI::dbExecute(con, paste0("INSERT INTO images (img_meta_id, datetime, fetch_datetime, description, share_with, format, file) VALUES ('", img_meta_id, "', '", datetime, "', '", fetch_datetime, "', '", description, "', '{",  paste(share_with, collapse = ","), "}', '", extension, "', '\\x", paste0(file, collapse = ""), "');"))
    if (!is.null(owner)) {
      DBI::dbExecute(con, paste0("UPDATE images SET owner = '", owner, "' WHERE img_meta_id = ", img_meta_id, " AND datetime = '", datetime, "';"))
    }
    if (!is.null(contributor)) {
      DBI::dbExecute(con, paste0("UPDATE images SET contributor = '", contributor, "' WHERE img_meta_id = ", img_meta_id, " AND datetime = '", datetime, "';"))
    }
    if (!is.null(fetch_datetime)) {
      DBI::dbExecute(con, paste0("UPDATE images SET fetch_datetime = '", fetch_datetime, "' WHERE img_meta_id = ", img_meta_id, " AND datetime = '", datetime, "';"))
    }
  }
  
  img_times <- DBI::dbGetQuery(con, paste0("SELECT first_img, last_img FROM images_index WHERE img_meta_id = ", img_meta_id, ";"))

  if (img_times[1,1] > datetime) {
    DBI::dbExecute(con, paste0("UPDATE images_index SET first_img = '", datetime, "' WHERE img_meta_id = ", img_meta_id, ";"))
  }
  if (img_times[1,2] < datetime) {
    DBI::dbExecute(con, paste0("UPDATE images_index SET last_img = '", datetime, "' WHERE img_meta_id = ", img_meta_id, ";"))
  }
    DBI::dbExecute(con, paste0("UPDATE images_index SET last_new_img = '", as.POSIXct(Sys.time(),tz = "UTC"), "' WHERE img_meta_id = ", img_meta_id, ";"))
  return(TRUE)
}

