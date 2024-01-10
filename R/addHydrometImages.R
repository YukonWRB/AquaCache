#' Add images to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one image at a time to the database in the 'images' table. Each image must be linked to a specific location. Adding an image directly to the database is not possible, since the file must be converted to a binary object before loading. See [fetchDocument()] to get the image out again.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param path Valid path including extension to the document to upload.
#' @param location The location with which to associate the document (must be in the database).
#' @param image_type The type of image: 'auto', or 'manual'.
#' @param datetime The datetime the image was taken at, as a POSIXct object or something that can be coerced to one. If not POSIXct timezone is assumed to be UTC.
#' @param fetch_datetime The datetime the image was retrieved (optional).
#' @param con A connection to the database.
#'
#' @return TRUE if an image was properly added to the database.
#' @export

addHydrometImages <- function(path, location, image_type, datetime, fetch_datetime = NULL, con = hydrometConnect(silent=TRUE)){

  #Checks
  if (length(path) > 1){
    stop("You can only specify one path at a time.")
  }
  if (!inherits(image_type, "character")){
    stop("The image_type should be a character vector of 1")
  }
  if (length(location) > 1){
    stop("You can only specify one location at a time.")
  }
  loc_exists <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location = '", location, "';"))
  if (nrow(loc_exists) == 0){
    stop("The location you specified does not exist in the database. Check your spelling.")
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

  extension <- tools::file_ext(path)
  file <- hexView::readRaw(path)$fileRaw

  if (!is.null(fetch_datetime)){

  } else {
    DBI::dbExecute(con, paste0("INSERT INTO images (location, datetime, format, image_type, file) VALUES ('", location, "', '", datetime, "', '", extension, "', ", image_type, "', '\\x", paste0(file, collapse = ""), "');"))
  }
 return(TRUE)
}

