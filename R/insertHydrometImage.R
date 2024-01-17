#' Add images to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' Images auto-generated on a regular basis should be updated in the database using function [getNewImages()] instead of this function. For one-off images, read on.
#'
#' This function facilitates the addition of one image at a time to the database in the 'images' table. Each image must be linked to a specific location. Adding an image directly to the database is not possible, since the file must be converted to a binary object before loading. See [fetchImage()] to get the image out again.
#'
#' @param object Valid path including extension to the document to upload, or an object of class 'response' such as that provided by [downloadWSCImages()].
#' @param img_meta_id The img_meta_id, from the table images_index, corresponding to the image location and type. Set NULL if there is no img_meta_id yet.
#' @param datetime The datetime the image was taken at, as a POSIXct object or something that can be coerced to one. If not POSIXct timezone is assumed to be UTC.
#' @param fetch_datetime The datetime the image was retrieved (optional).
#' @param location If no img_meta_id exists yet: the location with which to associate the document (must be in the database).
#' @param image_type If no img_meta_id exists yet: the type of image: 'auto', or 'manual'.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return TRUE if an image was properly added to the database.
#' @export

insertHydrometImage <- function(object, img_meta_id, datetime, fetch_datetime = NULL, location = NULL, image_type = NULL, con = hydrometConnect()){

  #Checks
  if (length(location) > 1){
    stop("You can only specify one location at a time.")
  }
  if (!is.null(img_meta_id)){
    img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE img_meta_id = ", img_meta_id))[1,1]
    if (is.na(img_meta_id)){
      stop("The img_meta_id you specified does not exist. Try again. If you need to create a new entry see the help file.")
    }
  } else { # See if need to create the img_meta_id and corresponding fields
    if (is.null(location) | is.null(image_type)){
      stop("Parameter 'img_meta_id' was set to NULL, but 'location' and/or 'image_type' is also NULL. Refer to the help file to fix the problem.")
    }
    if (!inherits(image_type, "character")){
      stop("The parameter image_type should be a character vector of 1")
    }
    if (!inherits(location, "character")){
      stop("The parameter location should be a character vector of 1")
    }
    #See if the id exists first
    img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location = '", location, "' AND img_type = '", image_type, "'"))[1,1]
    if (is.na(img_meta_id)){ #Create the img_meta_id
      loc_exists <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location = '", location, "';"))
      if (nrow(loc_exists) == 0){
        stop("The location you specified does not exist in the database. Check your spelling, or add it if necessary.")
      }
      message("It looks like this is the first image of type ", image_type, " entered for location ", location, ". Creating an entry in table images_index. This series of images will be set to 'public' visibility with no delay.")
      DBI::dbExecute(con, paste0("INSERT INTO images_index (location, img_type, public, first_img) VALUES ('", location, "', '", image_type, "', 'TRUE'", Sys.time(), "');"))
      img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location = '", location, "' AND img_type = '", image_type, "'"))[1,1]
    } else {
      warning("There is alreay an entry corresponding to this location and image_type. Using the img_meta_id ", img_meta_id, ".")
    }
  }

  if (inherits(datetime, "character")){
    as.POSIXct(datetime, tz = "UTC")
  }
  if (!inherits(datetime, "POSIXct")){
    stop("Datetime must be a POSIXct object or something that can be coerced to one.")
  }
  if (!is.null(fetch_datetime)){
    if (inherits(fetch_datetime, "character")){
      as.POSIXct(fetch_datetime, tz = "UTC")
    }
    if (!inherits(fetch_datetime, "POSIXct")){
      stop("fetch_datetime must be a POSIXct object or something that can be coerced to one.")
    }
  }

  #get the extension, and also the file itself as RAW
  if (inherits(object, "response")){
    extension <- tools::file_ext(object$url)
    file <- object$content
  } else if (inherits(object, "character")){
    extension <- tools::file_ext(object)
    file <- hexView::readRaw(object)$fileRaw
  }

  #Add to the database and update tables
  exist_img <- DBI::dbGetQuery(con, paste0("SELECT datetime FROM images WHERE datetime = '", datetime, "' AND img_meta_id = ", img_meta_id, ";"))[1,1]
  if (!is.na(exist_img)){
    warning("The is already an image in the database for this img_meta_id and datetime ", datetime, ". Deleting the old image and inserting the new one.")
    DBI::dbExecute(con, paste0("DELETE FROM images WHERE datetime = '", datetime, "' and img_meta_id = ", img_meta_id, ";"))
  }
  if (!is.null(fetch_datetime)){
    DBI::dbExecute(con, paste0("INSERT INTO images (img_meta_id, datetime, fetch_datetime, format, file) VALUES ('", img_meta_id, "', '", datetime, "', '", fetch_datetime, "', '", extension, "', '\\x", paste0(file, collapse = ""), "');"))
  } else {
    DBI::dbExecute(con, paste0("INSERT INTO images (img_meta_id, datetime, format, file) VALUES ('", img_meta_id, "', '", datetime, "', '", extension, "', '\\x", paste0(file, collapse = ""), "');"))
  }
  img_times <- DBI::dbGetQuery(con, paste0("SELECT first_img, last_img FROM images_index WHERE img_meta_id = ", img_meta_id, ";"))

  if (img_times[1,1] < datetime){
    DBI::dbExecute(con, paste0("UPDATE images_index SET last_img = '", datetime, "' WHERE img_meta_id = ", img_meta_id, ";"))
  }
  if (img_times[1,2] > datetime) {
    DBI::dbExecute(con, paste0("UPDATE images_index SET first_img = '", datetime, "' WHERE img_meta_id = ", img_meta_id, ";"))
  }
    DBI::dbExecute(con, paste0("UPDATE images_index SET last_new_img = '", as.POSIXct(Sys.time(),tz = "UTC"), "' WHERE img_meta_id = ", img_meta_id, ";"))
  return(TRUE)
}

