#' Get new images
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new data corresponding to entries in the table "image_series". As with the timeseries table, fetching new data depends on the function listed in the source_fx column of the relevant table and optionally on parameters in column source_fx_args. Refer to [addACTimeseries()] for a description of how to formulate these arguments.
#'
#' ## Default arguments passed to 'source_fx' functions:
#' This function passes default arguments to the "source_fx" function: 'location' gets the location referenced by the column 'location_id', start_datetime defaults to the instant after the last point already existing in the DB. Additional parameters can be passed using the "source_fx_args" column in the "timeseries" table.
#'
#' @param image_series_ids A vector of image_series_id's. Default 'all' fetches all ids in the table.
#' @param con A connection to the database. Leaving NULL will create a connection and close it automatically.
#' @param active Sets behavior for import of new images for image series. If set to 'default', the column 'active' in the image_series table will determine whether to get new images or not. If set to 'all', all image series will be fetched regardless of the 'active' column.
#' @export
#'

getNewImages <- function(image_series_ids = "all", con = NULL, active = 'default') {
  
  if (!active %in% c('default', 'all')) {
    stop("Parameter 'active' must be either 'default' or 'all'.")
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  # Create table of series_ids
  if (image_series_ids[1] == "all") {
    series_ids <- DBI::dbGetQuery(con, "SELECT i.img_series_id, i.last_img, i.source_fx, i.source_fx_args, i.active, i.location_id FROM image_series i WHERE i.source_fx IS NOT NULL;")
  } else {
    series_ids <- DBI::dbGetQuery(con, paste0("SELECT i.img_series_id, i.last_img, i.source_fx, i.source_fx_args, i.active, i.location_id FROM image_series i WHERE i.source_fx IS NOT NULL AND img_series_id IN ('", paste(image_series_ids, collapse = "', '"), "');"))
    if (length(image_series_ids) != nrow(series_ids)) {
      warning("At least one of the image_series_ids you called for cannot be found in the database or has no function specified in column source_fx of table image_series.")
    }
  }
  if (nrow(series_ids) == 0) {
    stop("No image_series_ids could be found matching your criteria.")
  }
  
  if (active == 'default') {
    series_ids <- series_ids[series_ids$active, ]
  }
  
  message("Fetching new images with getNewImages...")
  
  image_type <- DBI::dbGetQuery(con, paste0("SELECT image_type_id FROM image_types WHERE image_type = 'Automated camera';"))[1, 1]
  
  if (is.na(image_type)) {
    stop("getNewImages: Could not find image type 'Automated camera' in the database table 'image_type'.")
  }
  
  count <- 0 #counter for number of successful new pulls
  image_count <- 0
  success <- character(0)
  if (interactive()) {
    pb <- utils::txtProgressBar(min = 0, max = nrow(series_ids), style = 3)
  }
  for (i in 1:nrow(series_ids)) {
    id <- series_ids[i, "img_series_id"]
    location_id <- series_ids[i, "location_id"]
    next_instant <- series_ids[i, "last_img"] + 1 #one second after the last image
    source_fx <- series_ids[i, "source_fx"]
    source_fx_args <- series_ids[i, "source_fx_args"]
    
    tryCatch({
      args_list <- list(start_datetime = next_instant)
      if (!is.na(source_fx_args)) { #add some arguments if they are specified
        args <- jsonlite::fromJSON(source_fx_args)
        args_list <- c(args_list, lapply(args, as.character))
      }
      imgs <- do.call(source_fx, args_list) # Get the data using the args_list
      if (is.null(imgs)) {
        next
      }
      
      # Here, the output should be either of class "list", as results from downloadWSCImages, or data.frame, as results from downloadNupointImages.
      if (inherits(imgs, "list")) {
        for (j in 1:length(imgs)) {
          img <- imgs[[j]]
          # Get the image_type_id from the image_types table corresponding to 'Auto'
          insertACImage(object = img, img_series_id = id, datetime = img$timestamp, fetch_datetime = .POSIXct(Sys.time(), tz = "UTC"), con = con, description = "Auto-fetched.", image_type = image_type, tags = "auto", location = location_id)  # update to the last_img and last_new_img datetime is already being done by insertACImage
          image_count <- image_count + 1
        }
      } else if (inherits(imgs, "data.frame")) {
        for (j in 1:nrow(imgs)) {
          insertACImage(object = imgs[j, "file"], img_series_id = id, datetime = imgs[j, "datetime"], fetch_datetime = .POSIXct(Sys.time(), tz = "UTC"), con = con, description = "Auto-fetched.", image_type = image_type, tags = 'auto', location = location_id)  # update to the last_img and last_new_img datetime is already being done by insertACImage
          image_count <- image_count + 1
        }
      } else {
        next
      }
      count <- count + 1
      success <- c(success, id)
    }, error = function(e) {
      warning("getNewImages: Failed to get new images or to append new images for img_series_id ", id, ".")
    })    
    
    if (interactive()) {
      utils::setTxtProgressBar(pb, i)
    }
    
  } # End of for loop
  
  if (interactive()) {
    close(pb)
  }
  
  message(count, " out of ", nrow(series_ids), " img_series_ids were updated.")
  message(image_count, " images were added in total.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_images'"))
  return(success)
}
