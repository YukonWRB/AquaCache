#' Add new image series (auto-fetched)
#'
#' Use this function to add a new image series, i.e. a continually updating time-series of images that can be accessed from the web or internal server location on a regular basis.
#'
#' @details Source functions and arguments are stored as ordered assignments. The active assignment with the lowest `fetch_priority` is used by [getNewImages()].
#'
#' @param location_id The AquaCache location_id associated with the image series.
#' @param start_datetime The datetime (as POSIXct) from which to look for images
#' @param source_adapters An assignment data frame with columns `source_fx`, optional JSON or named-list `source_fx_args`, `fetch_priority`, `active`,  and optional `note`. Functions must be enabled for the image domain in `public.source_adapter_capabilities`.
#' @param share_with A *character* vector of the user group(s) with which to share the timeseries, default is 'public_reader'. Pass multiple groups as a single string, e.g. "public_reader, YG"
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. If left NULL, a connection will be attempted using AquaConnect() and closed afterwards.
#'
#' @return TRUE if successful, and a new entry in the database with images fetched.
#' @export
#'
#' @examples
#' \dontrun{
#' image_sources <- data.frame(
#'   source_fx = c("downloadWSCImages", "downloadNupointImages"),
#'   source_fx_args = I(list(
#'     list(location = "09AA001"),
#'     list(location = "camera-01")
#'   )),
#'   fetch_priority = c(1L, 2L),
#'   active = c(TRUE, FALSE),
#'   note = c(
#'     "Primary WSC image feed.",
#'     "Configured backup, currently inactive."
#'   )
#' )
#'
#' addACImageSeries(
#'   location_id = 123L,
#'   start_datetime = as.POSIXct("2025-01-01", tz = "UTC"),
#'   source_adapters = image_sources
#' )
#' }

addACImageSeries <- function(
  location_id,
  start_datetime,
  source_adapters,
  share_with = "public_reader",
  con = NULL
) {
  # function will add entry to image_series, then trigger getNewImages from the user-specified start_datetime

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  source_adapters <- source_adapter_assignments_normalize(
    assignments = source_adapters,
    con = con,
    data_domain = "image"
  )
  if (!nrow(source_adapters)) {
    stop("At least one image source-adapter assignment is required.")
  }

  # Confirm the location_id exists, tell the user the location 'name' that corresponds
  loc_check <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT name FROM public.locations WHERE location_id = ",
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
      "SELECT img_series_id FROM files.image_series WHERE location_id = ",
      location_id,
      ";"
    )
  )[1, 1]
  if (!is.na(exists)) {
    stop(
      "There is already an entry for that location or location_id and for images of type 'auto' in the image_series table."
    )
  }

  DBI::dbBegin(con)
  res <- tryCatch(
    {
      id <- DBI::dbGetQuery(
        con,
        "INSERT INTO files.image_series (
           location_id, first_img, last_img, share_with, active, description
         ) VALUES ($1, $2, $3, $4, $5, $6)
         RETURNING img_series_id;",
        params = list(
          location_id,
          start_datetime,
          start_datetime,
          paste0("{", paste(share_with, collapse = ","), "}"),
          TRUE,
          "Image series automatically taken from a web or server location."
        )
      )[1, 1]
      source_adapter_assignments_insert(
        con = con,
        data_domain = "image",
        series_id = id,
        assignments = source_adapters
      )
      DBI::dbCommit(con)
      id
    },
    error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    }
  )
  added <- getNewImages(image_series_ids = res, con = con)
  if (length(added) == 0) {
    warning(
      "Failed to find or add new images. The new entry to table image_series has been deleted."
    )
    DBI::dbExecute(
      con,
      paste0("DELETE FROM files.image_series WHERE img_series_id = ", res, ";")
    )
  } else {
    first_new <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT MIN(datetime) FROM files.images WHERE img_series_id = ",
        res,
        ";"
      )
    )[1, 1]
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE files.image_series SET first_img = '",
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
