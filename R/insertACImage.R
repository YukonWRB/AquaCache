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
#' @param datetime The datetime the image was taken at, as a POSIXct object or something that can be coerced to one. If not POSIXct timezone is assumed to be UTC.
#' @param image_type The image_type_id from table images_types corresponding to the image type. Pass as numeric, must match an entry in table 'image_types'
#' @param fetch_datetime The datetime the image was retrieved (optional). If not POSIXct timezone is assumed to be UTC.
#' @param img_meta_id The img_meta_id, from the table image_series, corresponding to the image location and type. Set NULL if the image is not linked to an image series - in that case you'll need to specify, at minimum, a latitude and longitude for the image OR associate it with a location.
#' @param location The location_id with which to associate the document (must be in the database). Pass a location_id as a numeric. If img_meta_id is specified, the location already associated is used. This parameter can also be left NULL if latitude and longitude are specified.
#' @param latitude If no img_meta_id exists yet AND not specifying a location: the latitude of the image. Pass as numeric.
#' @param longitude If no img_meta_id exists yet AND not specifying a location: the longitude of the image. Pass as numeric.
#' @param description A description of the image. Pass as text, can be left NULL; consider also using the `tags` parameter.
#' @param tags Tags to associate with the image. Pass as a character vector, one element per tag.
#' @param owner The owner of the image, matching the organization_id of table 'organizations'. Can be left NULL.
#' @param contributor The contributor of the image, matching the organization_id of table 'organizations'. Pass as text, can be left NULL.
#' @param share_with Which user groups should the image be shared with. Default is 'public_reader', the public group. Pass as a character vector, one element per group.
#' @param azimuth_true Optional: the true azimuth of the image (direction the image was taken in. Pass as numeric.
#' @param elevation_agl Optional: the elevation above ground level of the image in METERS. Pass as numeric.
#' @param elevation_msl Optional: the elevation above mean sea level of the image in METERS. Pass as numeric.
#' @param con A connection to the database. Default NULL uses AquaConnect() and close the connection afterwards.
#'
#' @return TRUE if an image was properly added to the database.
#' @export

insertACImage <- function(object, datetime, image_type, fetch_datetime = NULL, img_meta_id = NULL, description = NULL, tags = NULL,  owner = NULL, contributor = NULL, share_with = "public_reader", location = NULL, latitude = NULL, longitude = NULL, azimuth_true = NULL, elevation_agl = NULL, elevation_msl = NULL, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  #Checks
  if (!inherits(share_with, "character")) {
    stop("The 'share_with' parameter must be a character vector with one element per share with group.")
  }
  
  if (!is.null(owner)) {
    # Make sure it exists in the database table 'organizations'
    owner <- DBI::dbGetQuery(con, paste0("SELECT organization_id FROM organizations WHERE organization_id = ", owner))[1,1]
    if (is.na(owner)) {
      stop("The owner you specified does not exist. Try again.")
    }
  }
  
  if (!is.null(contributor)) {
    # Make sure it exists in the database table 'organizations'
    contributor <- DBI::dbGetQuery(con, paste0("SELECT organization_id FROM organizations WHERE organization_id = ", contributor))[1,1]
    if (is.na(contributor)) {
      stop("The contributor you specified does not exist. Try again.")
    }
  }
  
  if (!is.null(description)) {
    if (!inherits(description, "character")) {
      stop("The 'description' parameter must be a character vector.")
    }
    if (nchar(description) < 5) {
      stop("Minimum character length for 'description' is 5. Try harder.")
    }
  }
  if (!is.null(tags)) {
    if (!inherits(tags, "character")) {
      stop("The 'tags' parameter must be a character vector.")
    }
  }
  # Make sure it exists in the database table 'image_types'
  image_type <- DBI::dbGetQuery(con, paste0("SELECT image_type_id FROM image_types WHERE image_type_id = ", image_type))[1,1]
  if (is.na(image_type)) {
    stop("The image_type you specified does not exist. Try again.")
  }
  
  if (!is.null(latitude)) {
    if (!is.numeric(latitude)) {
      stop("The 'latitude' parameter must be a numeric value.")
    }
  }
  
  if (!is.null(longitude)) {
    if (!is.numeric(longitude)) {
      stop("The 'longitude' parameter must be a numeric value.")
    }
  }
  
  if (!is.null(azimuth_true)) {
    if (!is.numeric(azimuth_true)) {
      stop("The 'azimuth_true' parameter must be a numeric value.")
    }
  }
  
  if (!is.null(elevation_agl)) {
    if (!is.numeric(elevation_agl)) {
      stop("The 'elevation_agl' parameter must be a numeric value.")
    }
  }
  
  if (!is.null(elevation_msl)) {
    if (!is.numeric(elevation_msl)) {
      stop("The 'elevation_msl' parameter must be a numeric value.")
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
  
  meta_id <- FALSE
  if (!is.null(img_meta_id)) {
    img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM image_series WHERE img_meta_id = ", img_meta_id))[1,1]
    if (is.na(img_meta_id)) {
      stop("The img_meta_id you specified does not exist. Try again. If you need to create a new entry see the help file.")
    } else {
      location <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM image_series WHERE img_meta_id = ", img_meta_id))[1,1]
      latitude <- DBI::dbGetQuery(con, paste0("SELECT latitude FROM locations WHERE location_id = ", location))[1,1]
      longitude <- DBI::dbGetQuery(con, paste0("SELECT longitude FROM locations WHERE location_id = ", location))[1,1]
      elevation_msl <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", location, " AND current IS TRUE"))[1,1]
      if (is.na(elevation_msl)) {
        elevation_msl <- NULL
      }
      if (is.na(location)) {
        stop("The img_meta_id you specified does not have a location_id associated with it. Try again.")
      }
    }
    meta_id <- TRUE
    
  } else { # No image_meta_id, so we need to make sure a location is associated OR that a lat/long is provided
    
    if (is.null(location)) {
      if (is.null(latitude) | is.null(longitude)) {
        stop("You didn't specify an img_meta_id. Parameter 'location' was set to NULL, but you didn't specify a latitude and/or longitude. Refer to the help file to fix the problem.")
      }
    } else {  # Make sure the location_id exists in the database
      if (length(location) > 1) {
        stop("You can only specify one location at a time.")
      }
      location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location_id = ", location))[1,1]
      if (is.na(location_id)) {
        stop("The location_id you specified does not exist.")
      } else {
        latitude <- DBI::dbGetQuery(con, paste0("SELECT latitude FROM locations WHERE location_id = ", location))[1,1]
        longitude <- DBI::dbGetQuery(con, paste0("SELECT longitude FROM locations WHERE location_id = ", location))[1,1]
        if (is.null(elevation_msl)) {
          elevation_msl <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", location, " AND current IS TRUE"))[1,1]
        }
      }
    }
  }
  
  
  # get the extension, and also the file itself as RAW #####
  if (inherits(object, "response")) {
    extension <- tolower(tools::file_ext(object$url))
    file <- object$content
  } else if (inherits(object, "character")) {
    extension <- tolower(tools::file_ext(object))
    # Make sure the extension is some sort of image
    if (!extension %in% c("jpg", "jpeg", "png", "gif", "bmp", "tiff")) {
      stop("The file extension is not recognized as an image. Please provide a valid image file.")
    }
    file <- hexView::readRaw(object)$fileRaw
  }
  
  
  
  # Add the image in a transaction
  tryCatch({
    activeTrans <- dbTransBegin(con) # returns TRUE if a transaction is not already in progress and was set up, otherwise commit will happen in the original calling function.
    
    if (meta_id) { # There's an associated meta_id
      exist_img <- DBI::dbGetQuery(con, paste0("SELECT datetime FROM images WHERE datetime = '", datetime, "' AND img_meta_id = ", img_meta_id, ";"))[1,1]
      update <- FALSE
      if (!is.na(exist_img)) {
        warning("There is already an image in the database for this img_meta_id and datetime ", datetime, ". Updating the image, keeping the same img_id.")
        update <- TRUE
      }
      if (update) {
        new_id <- DBI::dbGetQuery(con, paste0("SELECT image_id FROM images WHERE datetime = '", datetime, "' AND img_meta_id = ", img_meta_id, ";"))[1,1]
        DBI::dbExecute(con, paste0("UPDATE images SET ", if (!is.null(fetch_datetime)) paste0("fetch_datetime = '", fetch_datetime, "', "), if (!is.null(description)) paste0("description = '", description, "', "), if (!is.null(owner)) paste0("owner = '", owner, "', "), if (!is.null(contributor)) paste0("contributor = '", contributor, "', "), "share_with = '{", paste(share_with, collapse = ","), "}', format = '", extension, "', file = '\\x", paste0(file, collapse = ""), "' WHERE image_id = ", new_id, ";"))
      } else {
        new_id <- DBI::dbGetQuery(con, paste0("INSERT INTO images (img_meta_id, datetime, fetch_datetime, description, share_with, location_id, latitude, longitude, format, file, image_type) VALUES ('", img_meta_id, "', '", datetime, "', '", fetch_datetime, "', '", description, "', '{",  paste(share_with, collapse = ","), "}', ", location, ", ", latitude, ", ", longitude, ", '", extension, "', '\\x", paste0(file, collapse = ""), "',", image_type, ") RETURNING image_id;"))
        if (!is.null(owner)) {
          DBI::dbExecute(con, paste0("UPDATE images SET owner = ", owner, " WHERE image_id = ", new_id, ";"))
        }
        if (!is.null(contributor)) {
          DBI::dbExecute(con, paste0("UPDATE images SET contributor = ", contributor, " WHERE image_id = ", new_id, ";"))
        }
        if (!is.null(fetch_datetime)) {
          DBI::dbExecute(con, paste0("UPDATE images SET fetch_datetime = '", fetch_datetime, "' WHERE image_id = ", new_id, ";"))
        }
      }
      if (!is.null(tags)) {
        DBI::dbExecute(con, paste0("UPDATE images SET tags = '{", paste(tags, collapse = ","), "}' WHERE image_id = ", new_id, ";"))
      }
      
      if (!is.null(azimuth_true)) {
        DBI::dbExecute(con, paste0("UPDATE images SET azimuth_true = ", azimuth_true, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(elevation_agl)) {
        DBI::dbExecute(con, paste0("UPDATE images SET elevation_agl_m = ", elevation_agl, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(elevation_msl)) {
        DBI::dbExecute(con, paste0("UPDATE images SET elevation_msl_m = ", elevation_msl, " WHERE image_id = ", new_id, ";"))
      }
      
      img_times <- DBI::dbGetQuery(con, paste0("SELECT first_img, last_img FROM image_series WHERE img_meta_id = ", img_meta_id, ";"))
      
      if (img_times[1,1] > datetime) {
        DBI::dbExecute(con, paste0("UPDATE image_series SET first_img = '", datetime, "' WHERE img_meta_id = ", img_meta_id, ";"))
      }
      if (img_times[1,2] < datetime) {
        DBI::dbExecute(con, paste0("UPDATE image_series SET last_img = '", datetime, "' WHERE img_meta_id = ", img_meta_id, ";"))
      }
      DBI::dbExecute(con, paste0("UPDATE image_series SET last_new_img = '", as.POSIXct(Sys.time(),tz = "UTC"), "' WHERE img_meta_id = ", img_meta_id, ";"))
      
      
    } else {  # Not working with an img_meta_id
      
      # No update for this one, just insert
      new_id <- DBI::dbGetQuery(con, paste0("INSERT INTO images (datetime, share_with, latitude, longitude, format, file, image_type) VALUES ('", datetime, "', '{", paste(share_with, collapse = ","), "}', ", latitude, ", ", longitude, ", '", extension, "', '\\x", paste0(file, collapse = ""), "', ", image_type, ") RETURNING image_id;"))
      if (!is.null(description)) {
        DBI::dbExecute(con, paste0("UPDATE images SET description = '", description, "' WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(owner)) {
        DBI::dbExecute(con, paste0("UPDATE images SET owner = ", owner, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(contributor)) {
        DBI::dbExecute(con, paste0("UPDATE images SET contributor = ", contributor, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(fetch_datetime)) {
        DBI::dbExecute(con, paste0("UPDATE images SET fetch_datetime = '", fetch_datetime, "' WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(tags)) {
        DBI::dbExecute(con, paste0("UPDATE images SET tags = '{", paste(tags, collapse = ","), "}' WHERE image_id = ", new_id, ";"))
      }
      
      if (!is.null(azimuth_true)) {
        DBI::dbExecute(con, paste0("UPDATE images SET azimuth_true = ", azimuth_true, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(elevation_agl)) {
        DBI::dbExecute(con, paste0("UPDATE images SET elevation_agl_m = ", elevation_agl, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(elevation_msl)) {
        DBI::dbExecute(con, paste0("UPDATE images SET elevation_msl_m = ", elevation_msl, " WHERE image_id = ", new_id, ";"))
      }
      if (!is.null(location)) {
        DBI::dbExecute(con, paste0("UPDATE images SET location_id = ", location, " WHERE image_id = ", new_id, ";"))
      }
    }
    
    if (activeTrans) {
      DBI::dbExecute(con, "COMMIT;")
    }
    return(TRUE)
  }, error = function(e) {
    if (activeTrans) {
      DBI::dbExecute(con, "ROLLBACK")
    }
    stop("An error occurred while inserting the image: ", e$message)
  })
}

