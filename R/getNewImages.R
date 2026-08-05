#' Get new images
#'
#' @description
#'
#' Retrieves images using the active assignment with the lowest fetch priority
#' in `files.image_series_source_adapters` for each image series.
#' Every source function must have an enabled image-domain entry in
#' `public.source_adapter_capabilities`.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes `start_datetime`, defaulting to the instant after the
#' last image. Additional parameters come from the selected assignment's
#' `source_fx_args` JSON object.
#'
#' If you are a developer, note that download or source functions MUST be registered in AquaCache using function [registerSourceAdapterArguments()], and that this operation would normally be completed using the 'patch' system. See patch_56.R for examples.
#'
#' @param image_series_ids A vector of image_series_id's. Default 'all' fetches all ids in the table.
#' @param con A connection to the database. Leaving NULL will create a connection and close it automatically.
#' @param active Sets behavior for import of new images for image series. If set to 'default', the column 'active' in the image_series table will determine whether to get new images or not. If set to 'all', all image series will be fetched regardless of the 'active' column.
#' @export
#'

getNewImages <- function(
  image_series_ids = "all",
  con = NULL,
  active = 'default'
) {
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  image_series_select_sql <-
    "SELECT i.img_series_id, i.last_img, source.source_fx,
            source.source_fx_args, source.fetch_priority,
            i.active, i.location_id
     FROM files.image_series i
     LEFT JOIN LATERAL (
       SELECT isa.source_fx, isa.source_fx_args, isa.fetch_priority
       FROM files.image_series_source_adapters isa
       WHERE isa.img_series_id = i.img_series_id
         AND isa.active
       ORDER BY isa.fetch_priority, isa.image_series_source_adapter_id
       LIMIT 1
     ) source ON TRUE"

  # Create table of series_ids
  if (image_series_ids[1] == "all") {
    series_ids <- DBI::dbGetQuery(
      con,
      image_series_select_sql
    )
  } else {
    series_ids <- DBI::dbGetQuery(
      con,
      paste0(
        image_series_select_sql,
        " WHERE i.img_series_id IN ('",
        paste(image_series_ids, collapse = "', '"),
        "');"
      )
    )
    if (length(image_series_ids) != nrow(series_ids)) {
      warning(
        "At least one requested image_series_id could not be found."
      )
    }
  }
  if (nrow(series_ids) == 0) {
    stop("No image_series_ids could be found matching your criteria.")
  }

  if (active == 'default') {
    series_ids <- series_ids[series_ids$active, ]
  }
  if (nrow(series_ids) == 0) {
    stop("No active image_series_ids could be found matching your criteria.")
  }

  missing_source <- is.na(series_ids$source_fx)
  if (any(missing_source)) {
    warning(
      "The following image series have no active source-adapter assignment ",
      "and will be ignored: ",
      paste(series_ids$img_series_id[missing_source], collapse = ", "),
      "."
    )
    series_ids <- series_ids[!missing_source, , drop = FALSE]
  }
  if (nrow(series_ids) == 0L) {
    stop("No image series has an active source-adapter assignment.")
  }

  registered_source_fx <- getSourceAdapterCapabilities(
    con = con,
    data_domain = "image"
  )$source_fx
  unregistered_source_fx <- setdiff(
    unique(series_ids$source_fx),
    registered_source_fx
  )
  if (length(unregistered_source_fx) > 0L) {
    stop(
      "getNewImages: Every source_fx must have an enabled entry in ",
      "public.source_adapter_capabilities for the image domain. ",
      "Missing or disabled: ",
      paste(unregistered_source_fx, collapse = ", "),
      "."
    )
  }

  message("Fetching new images with getNewImages...")

  image_type <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT image_type_id FROM files.image_types WHERE image_type = 'Automated camera';"
    )
  )[1, 1]

  if (is.na(image_type)) {
    stop(
      "getNewImages: Could not find image type 'Automated camera' in the database table 'image_type'."
    )
  }

  count <- 0 #counter for number of successful new pulls
  image_count <- 0
  success <- character(0)
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(series_ids), style = 3)
  }
  for (i in seq_len(nrow(series_ids))) {
    id <- series_ids[i, "img_series_id"]
    location_id <- series_ids[i, "location_id"]
    next_instant <- series_ids[i, "last_img"] + 1 #one second after the last image
    source_fx <- series_ids[i, "source_fx"]
    source_fx_args <- series_ids[i, "source_fx_args"]

    tryCatch(
      {
        args_list <- list(start_datetime = next_instant)
        if (!is.na(source_fx_args)) {
          #add some arguments if they are specified
          args <- source_adapter_args_decode(source_fx_args)
          args_list <- c(args_list, args)
        }
        imgs <- do.call(source_fx, args_list) # Get the data using the args_list
        if (is.null(imgs)) {
          next
        }

        # Here, the output should be either of class "list", as results from downloadWSCImages, or data.frame, as results from downloadNupointImages.
        if (inherits(imgs, "list")) {
          if (length(imgs) == 0) {
            next
          }
          for (j in seq_along(imgs)) {
            img <- imgs[[j]]
            # Get the image_type_id from the image_types table corresponding to 'Auto'
            insertACImage(
              object = img,
              img_series_id = id,
              datetime = img$timestamp,
              fetch_datetime = .POSIXct(Sys.time(), tz = "UTC"),
              con = con,
              description = "Auto-fetched.",
              image_type = image_type,
              tags = "auto",
              location = location_id
            ) # update to the last_img and last_new_img datetime is already being done by insertACImage
            image_count <- image_count + 1
          }
        } else if (inherits(imgs, "data.frame")) {
          if (nrow(imgs) == 0) {
            next
          }
          for (j in seq_len(nrow(imgs))) {
            insertACImage(
              object = imgs[j, "file"],
              img_series_id = id,
              datetime = imgs[j, "datetime"],
              fetch_datetime = .POSIXct(Sys.time(), tz = "UTC"),
              con = con,
              description = "Auto-fetched.",
              image_type = image_type,
              tags = 'auto',
              location = location_id
            ) # update to the last_img and last_new_img datetime is already being done by insertACImage
            image_count <- image_count + 1
          }
        } else {
          next
        }
        count <- count + 1
        success <- c(success, id)
      },
      error = function(e) {
        warning(
          "getNewImages: Failed to get new images or to append new images for img_series_id ",
          id,
          "."
        )
      }
    )

    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
  } # End of for loop

  if (interactive()) {
    close(pb)
  }

  message(count, " out of ", nrow(series_ids), " img_series_ids were updated.")
  message(image_count, " images were added in total.")

  try(
    # In a try in case the user doesn't have update permissions on internal_status
    {
      DBI::dbExecute(
        con,
        "UPDATE information.internal_status SET value = NOW() WHERE event = 'lastet_new_images';"
      )
    },
    silent = TRUE
  )
  return(success)
}
