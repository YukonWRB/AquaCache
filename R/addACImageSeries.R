#' Add new image series (auto-fetched)
#'
#' Use this function to add a new image series, i.e. a continually updating time-series of images that can be accessed from the web or internal server location on a regular basis.
#'
#' @details
#' #' Additional arguments to pass to the function specified in `source_fx` go in argument `source_fx_args` and will be converted to JSON format. It's therefore necessary to pass this argument in as a single length character vector in the style "argument1: value1, argument2: value2".
#'
#' @param location_id The location_id associated with the image series.
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
  location_id,
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

  # Confirm the location_id exists, tell the user the location 'name' that corresponds
  loc_check <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT name FROM locations WHERE location_id = ",
      location_id,
      ";"
    )
  )
  if (nrow(loc_check) == 0) {
    stop("The specified location_id does not exist in the locations table.")
  } else {
    message(
      "Adding image series for location '",
      loc_check$name[1],
      "' (location_id ",
      location_id,
      ")."
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

  # split only on first colon
  keys <- sub(":.*", "", args)
  vals <- sub("^[^:]+:\\s*", "", args)

  # build named list
  args <- stats::setNames(as.list(vals), keys)

  # convert to JSON
  args <- jsonlite::toJSON(args, auto_unbox = TRUE)

  # Insert the new entry into the image_series table
  res <- DBI::dbGetQuery(
    con,
    "INSERT INTO image_series (location_id, first_img, last_img, source_fx, source_fx_args, share_with, active, description) VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING img_series_id;",
    params = list(
      location_id,
      start_datetime,
      start_datetime,
      source_fx,
      args,
      paste0("{", paste(share_with, collapse = ","), "}"),
      TRUE,
      "Image series automatically taken from a web or server location."
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
