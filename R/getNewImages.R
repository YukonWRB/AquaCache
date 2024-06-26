#' Get new images
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new data corresponding to entries in the table "images_index". As with the timeseries table, fetching new data depends on the function listed in the source_fx column of the relevant table and optionally on parameters in column source_fx_args. Refer to [addHydrometTimeseries()] for a description of how to formulate these arguments.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location referenced by the column 'location_id', start_datetime defaults to the instant after the last point already existing in the DB. Additional parameters can be passed using the "source_fx_args" column in the "timeseries" table.
#'
#' @param image_meta_ids A vector of image_meta_id's. Default 'all' fetches all ids where img_type = 'auto'.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param active Sets behavior for import of new images for image series. If set to 'default', the column 'active' in the images_index table will determine whether to get new images or not. If set to 'all', all image series will be fetched regardless of the 'active' column.
#' @export
#'

getNewImages <- function(image_meta_ids = "all", con = hydrometConnect(silent = TRUE), active = 'default') {

  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  # Create table of meta_ids
  if (image_meta_ids[1] == "all") {
    meta_ids <- DBI::dbGetQuery(con, "SELECT i.img_meta_id, i.last_img, i.source_fx, i.source_fx_args, l.location, i.active FROM images_index i JOIN locations l ON i.location_id = l.location_id WHERE i.img_type = 'auto' AND i.source_fx IS NOT NULL;")
  } else {
    meta_ids <- DBI::dbGetQuery(con, paste0("SELECT i.img_meta_id, i.last_img, i.source_fx, i.source_fx_args, l.location, i.active FROM images_index i JOIN locations l ON i.location_id = l.location_id WHERE i.img_type = 'auto' AND i.source_fx IS NOT NULL AND img_meta_id IN ('", paste(image_meta_ids, collapse = "', '"), "');"))
    if (length(image_meta_ids) != nrow(meta_ids)) {
      warning("At least one of the image_meta_ids you called for cannot be found in the database or has no function specified in column source_fx of table images_index.")
    }
  }
  
  if (active == 'default') {
    meta_ids <- meta_ids[meta_ids$active == TRUE, ]
  }

  count <- 0 #counter for number of successful new pulls
  image_count <- 0
  success <- character(0)

  for (i in 1:nrow(meta_ids)) {
    id <- meta_ids[i, "img_meta_id"]
    location <- meta_ids[i, "location"]
    next_instant <- meta_ids[i, "last_img"] + 1 #one second after the last image
    source_fx <- meta_ids[i, "source_fx"]
    source_fx_args <- meta_ids[i, "source_fx_args"]

    tryCatch({
      args_list <- list(location = location, start_datetime = next_instant)
      if (!is.na(source_fx_args)) { #add some arguments if they are specified
        args <- strsplit(source_fx_args, "\\},\\s*\\{")
        pairs <- lapply(args, function(pair) {
          gsub("[{}]", "", pair)
        })
        pairs <- lapply(pairs, function(pair) {
          gsub("\"", "", pair)
        })
        pairs <- lapply(pairs, function(pair) {
          gsub("'", "", pair)
        })
        pairs <- strsplit(unlist(pairs), "=")
        pairs <- lapply(pairs, function(pair) {
          trimws(pair)
        })
        for (j in 1:length(pairs)) {
          args_list[[pairs[[j]][1]]] <- pairs[[j]][[2]]
        }
      }
      imgs <- do.call(source_fx, args_list) #Get the data using the args_list
      if (is.null(imgs)) {
        next
      }
      
      # Here, the output should be either of class "response", as results from downloadWSCImages, or data.frame, as results from downloadNupointImages.
      if (inherits(imgs, "response")) {
        for (j in 1:length(imgs)) {
          img <- imgs[[j]]
          insertHydrometImage(object = img, img_meta_id = id, datetime = img$timestamp, fetch_datetime = .POSIXct(Sys.time(), tz = "UTC"), con = con, description = "Auto-fetched.")  # update to the last_img and last_new_img datetime is already being done by insertHydrometImage
          image_count <- image_count + 1
        }
      } else if (inherits(imgs, "data.frame")) {
        for (j in 1:nrow(imgs)) {
          insertHydrometImage(object = imgs[j, "file"], img_meta_id = id, datetime = imgs[j, "datetime"], fetch_datetime = .POSIXct(Sys.time(), tz = "UTC"), con = con, description = "Auto-fetched.")  # update to the last_img and last_new_img datetime is already being done by insertHydrometImage
          image_count <- image_count + 1
        }
      } else {
        stop("Can't work with an object of class ", class(img), ". This likely occured because the output of the image fetch function used for img_meta_id ", id, " is not yielding the expected output.")
      }
      
      count <- count + 1
      success <- c(success, id)
    }, error = function(e) {
      warning("getNewImages: Failed to get new images or to append new images for img_meta_id ", id, ".")
    })
  }
  message(count, " out of ", nrow(meta_ids), " img_meta_ids were updated.")
  message(image_count, " images were added in total.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_images'"))
  return(success)
}
