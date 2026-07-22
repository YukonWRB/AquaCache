#' Add images to aquacache database
#'
#'@description
#'
#' Images auto-generated on a regular basis should be updated in the database using function [getNewImages()] instead of this function. For one-off images, read on.
#'
#' This function facilitates the addition of one image at a time to the database in the 'images' table. Each image must be linked to a specific location_id from the locations table. Adding an image directly to the database is not possible since the file must be converted to a binary object before loading. See [YGwater::getImage()] to get the image out again.
#'
#' @param object Valid path including extension to the image to upload, or an object of class 'response' such as that provided by [downloadWSCImages()].
#' @param datetime The datetime the image was taken at, as a POSIXct object or something that can be coerced to one. If not POSIXct timezone is assumed to be UTC.
#' @param image_type The image_type_id from table images_types corresponding to the image type. Pass as numeric, must match an entry in table 'image_types'
#' @param fetch_datetime The datetime the image was retrieved (optional). If not POSIXct timezone is assumed to be UTC.
#' @param img_series_id The img_series_id, from the table image_series, corresponding to the image location and type. Set NULL if the image is not linked to an image series - in that case you'll need to specify, at minimum, a latitude and longitude for the image OR associate it with a location.
#' @param location The location_id with which to associate the document (must be in the database). Pass a location_id as a numeric. If img_series_id is specified, the location already associated is used. This parameter can also be left NULL if latitude and longitude are specified.
#' @param latitude If no img_series_id exists yet AND not specifying a location: the latitude of the image. Pass as numeric.
#' @param longitude If no img_series_id exists yet AND not specifying a location: the longitude of the image. Pass as numeric.
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

insertACImage <- function(
  object,
  datetime,
  image_type,
  fetch_datetime = NULL,
  img_series_id = NULL,
  description = NULL,
  tags = NULL,
  owner = NULL,
  contributor = NULL,
  share_with = "public_reader",
  location = NULL,
  latitude = NULL,
  longitude = NULL,
  azimuth_true = NULL,
  elevation_agl = NULL,
  elevation_msl = NULL,
  con = NULL
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  #Checks
  if (!inherits(share_with, "character")) {
    stop(
      "The 'share_with' parameter must be a character vector with one element per share with group."
    )
  }

  if (!is.null(owner)) {
    # Make sure it exists in the database table 'organizations'
    owner <- DBI::dbGetQuery(
      con,
      "SELECT organization_id FROM public.organizations WHERE organization_id = $1",
      params = list(owner)
    )[1, 1]
    if (is.na(owner)) {
      stop("The owner you specified does not exist. Try again.")
    }
  }

  if (!is.null(contributor)) {
    # Make sure it exists in the database table 'organizations'
    contributor <- DBI::dbGetQuery(
      con,
      "SELECT organization_id FROM public.organizations WHERE organization_id = $1",
      params = list(contributor)
    )[1, 1]
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
  image_type <- DBI::dbGetQuery(
    con,
    "SELECT image_type_id FROM files.image_types WHERE image_type_id = $1",
    params = list(image_type)
  )[1, 1]
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
    datetime <- as.POSIXct(datetime, tz = "UTC")
  }
  if (!inherits(datetime, "POSIXct")) {
    stop(
      "Datetime must be a POSIXct object or something that can be coerced to one."
    )
  }
  datetime <- lubridate::floor_date(datetime, "second")

  if (!is.null(fetch_datetime)) {
    if (inherits(fetch_datetime, "character")) {
      fetch_datetime <- as.POSIXct(fetch_datetime, tz = "UTC")
    }
    if (!inherits(fetch_datetime, "POSIXct")) {
      stop(
        "fetch_datetime must be a POSIXct object or something that can be coerced to one."
      )
    }
  }

  series_id <- FALSE
  if (!is.null(img_series_id)) {
    series <- DBI::dbGetQuery(
      con,
      "SELECT s.img_series_id, s.location_id, l.latitude, l.longitude,
              dc.conversion_m
       FROM files.image_series s
       LEFT JOIN public.locations l ON l.location_id = s.location_id
       LEFT JOIN public.datum_conversions dc
         ON dc.location_id = s.location_id AND dc.current IS TRUE
       WHERE s.img_series_id = $1",
      params = list(img_series_id)
    )
    if (!nrow(series)) {
      stop(
        "The img_series_id you specified does not exist. Try again. If you need to create a new entry see the help file."
      )
    } else {
      img_series_id <- series$img_series_id[[1]]
      location <- series$location_id[[1]]
      latitude <- series$latitude[[1]]
      longitude <- series$longitude[[1]]
      elevation_msl <- series$conversion_m[[1]]
      if (is.na(elevation_msl)) {
        elevation_msl <- NULL
      }
      if (is.na(location)) {
        stop(
          "The img_series_id you specified does not have a location_id associated with it. Try again."
        )
      }
    }
    series_id <- TRUE
  } else {
    # No image_series_id, so we need to make sure a location is associated OR that a lat/long is provided

    if (is.null(location)) {
      if (is.null(latitude) | is.null(longitude)) {
        stop(
          "You didn't specify an img_series_id. Parameter 'location' was set to NULL, but you didn't specify a latitude and/or longitude. Refer to the help file to fix the problem."
        )
      }
    } else {
      # Make sure the location_id exists in the database
      if (length(location) > 1) {
        stop("You can only specify one location at a time.")
      }
      location_id <- DBI::dbGetQuery(
        con,
        "SELECT l.location_id, l.latitude, l.longitude, dc.conversion_m
         FROM public.locations l
         LEFT JOIN public.datum_conversions dc
           ON dc.location_id = l.location_id AND dc.current IS TRUE
         WHERE l.location_id = $1",
        params = list(location)
      )
      if (!nrow(location_id)) {
        stop("The location_id you specified does not exist.")
      } else {
        latitude <- location_id$latitude[[1]]
        longitude <- location_id$longitude[[1]]
        if (is.null(elevation_msl)) {
          elevation_msl <- location_id$conversion_m[[1]]
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
      stop(
        "The file extension is not recognized as an image. Please provide a valid image file."
      )
    }
    file <- readBin(object, "raw", n = file.size(object))
  }

  file_hex <- paste0(file, collapse = "")
  share_with_json <- jsonlite::toJSON(
    as.character(share_with),
    auto_unbox = FALSE
  )
  tags_json <- jsonlite::toJSON(tags, auto_unbox = FALSE, null = "null")
  fetch_datetime_db <- if (is.null(fetch_datetime)) {
    as.POSIXct(NA, tz = "UTC")
  } else {
    fetch_datetime
  }
  description_db <- if (is.null(description)) NA_character_ else description
  owner_db <- if (is.null(owner)) NA_integer_ else as.integer(owner)
  contributor_db <- if (is.null(contributor)) {
    NA_integer_
  } else {
    as.integer(contributor)
  }
  series_db <- if (series_id) as.integer(img_series_id) else NA_integer_
  location_db <- if (is.null(location)) NA_integer_ else as.integer(location)
  azimuth_db <- if (is.null(azimuth_true)) NA_real_ else azimuth_true
  elevation_agl_db <- if (is.null(elevation_agl)) NA_real_ else elevation_agl
  elevation_msl_db <- if (is.null(elevation_msl)) NA_real_ else elevation_msl

  activeTrans <- FALSE
  tryCatch(
    {
      # dbTransBegin returns TRUE only when this function opened the transaction.
      activeTrans <- dbTransBegin(con)

      existing_id <- integer()
      if (series_id) {
        existing_id <- DBI::dbGetQuery(
          con,
          "SELECT image_id
           FROM files.images
           WHERE datetime = $1 AND img_series_id = $2",
          params = list(datetime, img_series_id)
        )$image_id
      }

      if (length(existing_id)) {
        warning(
          "There is already an image in the database for this img_series_id and datetime ",
          datetime,
          ". Updating the image, keeping the same img_id."
        )
        DBI::dbGetQuery(
          con,
          "UPDATE files.images
           SET fetch_datetime = COALESCE($1, fetch_datetime),
               description = COALESCE($2, description),
               owner = COALESCE($3, owner),
               contributor = COALESCE($4, contributor),
               share_with = ARRAY(
                 SELECT jsonb_array_elements_text($5::jsonb)
               ),
               format = $6,
               file = decode($7, 'hex'),
               tags = CASE WHEN $8::jsonb = 'null'::jsonb THEN tags
                           ELSE ARRAY(
                             SELECT jsonb_array_elements_text($8::jsonb)
                           ) END,
               azimuth_true = COALESCE($9, azimuth_true),
               elevation_agl_m = COALESCE($10, elevation_agl_m),
               elevation_msl_m = COALESCE($11, elevation_msl_m)
           WHERE image_id = $12
           RETURNING image_id",
          params = list(
            fetch_datetime_db,
            description_db,
            owner_db,
            contributor_db,
            share_with_json,
            extension,
            file_hex,
            tags_json,
            azimuth_db,
            elevation_agl_db,
            elevation_msl_db,
            existing_id[[1]]
          )
        )
      } else {
        DBI::dbGetQuery(
          con,
          "INSERT INTO files.images (
             img_series_id, datetime, fetch_datetime, description, share_with,
             location_id, latitude, longitude, format, file, image_type,
             owner, contributor, tags, azimuth_true, elevation_agl_m,
             elevation_msl_m
           )
           VALUES (
             $1, $2, $3, $4,
             ARRAY(SELECT jsonb_array_elements_text($5::jsonb)),
             $6, $7, $8, $9, decode($10, 'hex'), $11, $12, $13,
             CASE WHEN $14::jsonb = 'null'::jsonb THEN NULL
                  ELSE ARRAY(
                    SELECT jsonb_array_elements_text($14::jsonb)
                  ) END,
             $15, $16, $17
           )
           ON CONFLICT (file_hash) DO UPDATE SET
             img_series_id = COALESCE(EXCLUDED.img_series_id, files.images.img_series_id),
             datetime = EXCLUDED.datetime,
             fetch_datetime = COALESCE(EXCLUDED.fetch_datetime, files.images.fetch_datetime),
             description = COALESCE(EXCLUDED.description, files.images.description),
             share_with = EXCLUDED.share_with,
             location_id = COALESCE(EXCLUDED.location_id, files.images.location_id),
             latitude = EXCLUDED.latitude,
             longitude = EXCLUDED.longitude,
             format = EXCLUDED.format,
             image_type = EXCLUDED.image_type,
             owner = COALESCE(EXCLUDED.owner, files.images.owner),
             contributor = COALESCE(EXCLUDED.contributor, files.images.contributor),
             tags = COALESCE(EXCLUDED.tags, files.images.tags),
             azimuth_true = COALESCE(EXCLUDED.azimuth_true, files.images.azimuth_true),
             elevation_agl_m = COALESCE(EXCLUDED.elevation_agl_m, files.images.elevation_agl_m),
             elevation_msl_m = COALESCE(EXCLUDED.elevation_msl_m, files.images.elevation_msl_m)
           RETURNING image_id",
          params = list(
            series_db,
            datetime,
            fetch_datetime_db,
            description_db,
            share_with_json,
            location_db,
            latitude,
            longitude,
            extension,
            file_hex,
            image_type,
            owner_db,
            contributor_db,
            tags_json,
            azimuth_db,
            elevation_agl_db,
            elevation_msl_db
          )
        )
      }

      if (series_id) {
        DBI::dbExecute(
          con,
          "UPDATE files.image_series
           SET first_img = LEAST(COALESCE(first_img, $1), $1),
               last_img = GREATEST(COALESCE(last_img, $1), $1),
               last_new_img = NOW()
           WHERE img_series_id = $2",
          params = list(datetime, img_series_id)
        )
      }

      if (activeTrans) {
        DBI::dbExecute(con, "COMMIT;")
      }
      TRUE
    },
    error = function(e) {
      if (activeTrans) {
        DBI::dbExecute(con, "ROLLBACK")
      }
      stop("An error occurred while inserting the image: ", e$message)
    }
  )
}
