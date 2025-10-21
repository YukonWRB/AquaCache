#' Add new image series (auto-fetched)
#'
#' Use this function to add a new image series, i.e. a continually updating time-series of images that can be accessed from the web or internal server location on a regular basis.
#'
#' @details
#' #' Additional arguments to pass to the function specified in `source_fx` go in argument `source_fx_args` and will be converted to JSON format. It's therefore necessary to pass this argument in as a single length character vector in the style "argument1: value1, argument2: value2".
#'
#' @param location The location or location_id associated with the image series. Pass a location code as text and a location_id as a numeric.
#' @param start_datetime The datetime (as POSIXct) from which to look for images
#' @param source_fx The function to use for fetching new images. Must be an existing function in this package.
#' @param source_fx_args Arguments to pass to the function(s) specified in parameter 'source_fx'. See details.
#' @param share_with A *character* vector of the user group(s) with which to share the timeseries, Default is 'public_reader'. Pass multiple groups as a single string, e.g. "public_reader, YG"
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. If left NULL, a connection will be attempted using AquaConnect() and closed afterwards.
#'
#' @return TRUE if successful, and a new entry in the database with images fetched.
#' @export
#'

addACImageSeries <- function(
  location,
  start_datetime,
  source_fx,
  source_fx_args = NA,
  share_with = "public_reader",
  con = NULL
) {
  # function will add entry to image_series, then trigger getNewImages from the user-specified start_datetime

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (inherits(location, "character")) {
    location_id <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT location_id FROM locations WHERE location = '",
        location,
        "';"
      )
    )[1, 1]
    if (is.na(location_id)) {
      stop(
        "The location you specified does not exist. Reminder that you should specify the location code (text) or location_id (numeric), not the name."
      )
    }
  } else if (inherits(location, "numeric")) {
    location_id <- location
    check <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT location_id FROM locations WHERE location_id = ",
        location_id
      )
    )[1, 1]
    if (is.na(check)) {
      stop("The location_id you specified does not exist.")
    }
  } else {
    stop(
      "Parameter 'location' must be either a character or numeric vector of length 1."
    )
  }

  if (!inherits(share_with, "character")) {
    stop("The 'share_with' parameter must be a character vector.")
  }

  exists <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT img_series_id FROM image_series WHERE location_id = ",
      location_id,
      ";"
    )
  )[1, 1]
  if (!is.na(exists)) {
    stop(
      "There is already an entry for that location or location_id and for images of type 'auto' in the image_series table."
    )
  }

  args <- source_fx_args
  # split into "argument1: value1" etc.
  args <- strsplit(args, ",\\s*")[[1]]

  # split each pair on ":" and trim whitespace
  args <- strsplit(args, ":\\s*")

  # build a named list: names = keys, values = values
  args <- stats::setNames(
    lapply(args, function(x) x[2]),
    sapply(args, function(x) x[1])
  )
  # convert to JSON
  args <- jsonlite::toJSON(args, auto_unbox = TRUE)

  insert <- data.frame(
    location_id = location_id,
    first_img = start_datetime,
    last_img = start_datetime,
    source_fx = source_fx,
    source_fx_args = args,
    share_with = paste0("{", paste(share_with, collapse = ","), "}"),
    active = TRUE,
    description = "Image series automatically taken from a web or server location."
  )

  DBI::dbAppendTable(con, "image_series", insert)
  res <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT img_series_id FROM image_series WHERE location_id = ",
      location_id,
      ";"
    )
  )[1, 1]
  added <- getNewImages(image_series_ids = res, con = con)
  if (length(added) == 0) {
    warning(
      "Failed to find or add new images. The new entry to table image_series has been deleted."
    )
    DBI::dbExecute(
      con,
      paste0("DELETE FROM image_series WHERE img_series_id = ", res, ";")
    )
  } else {
    first_new <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT MIN(datetime) FROM images WHERE img_series_id = ",
        res,
        ";"
      )
    )[1, 1]
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE image_series SET first_img = '",
        first_new,
        "' WHERE img_series_id = ",
        res,
        ";"
      )
    )
    message(
      "Added new image series for location_id ",
      location_id,
      " and type 'auto'. The new img_series_id is ",
      res,
      "."
    )
  }
}
