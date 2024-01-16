#' Get new rasters
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves new data corresponding to entries in the table "raster_series_index". You can add a new raster series with [addHydrometRasterSeries()]. As with the timeseries and images table, fetching new data depends on the function listed in the source_fx column of the relevant table and optionally on parameters in column source_fx_args. Refer to [addHydrometTimeseries()] for a description of how to formulate these arguments.
#'
#' @param raster_series_ids A vector of raster_series_id's. Default 'all' fetches all ids in the raster_series_index table.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @export
#'

getNewRasters <- function(raster_series_ids = "all", con = hydrometConnect(silent=TRUE)) {


  # Create table of meta_ids
  if (raster_series_ids[1] == "all"){
    meta_ids <- DBI::dbGetQuery(con, "SELECT raster_series_id, end_datetime, source_fx, source_fx_args FROM raster_series_index WHERE source_fx IS NOT NULL;")
  } else {
    meta_ids <- DBI::dbGetQuery(con, paste0("SELECT raster_series_id, end_datetime, source_fx, source_fx_args FROM raster_series_index WHERE raster_series_id IN ('", paste(raster_series_ids, collapse = "', '"), "') AND source_fx IS NOT NULL;"))
    if (length(raster_series_ids) != nrow(meta_ids)){
      warning("At least one of the raster_series_ids you called for cannot be found in the database or has no function specified in column source_fx of table images_index.")
    }
  }

  count <- 0 #counter for number of successful new pulls
  raster_count <- 0
  success <- character(0)

  for (i in 1:nrow(meta_ids)){
    id <- meta_ids[i, "raster_series_id"]
    next_instant <- meta_ids[i, "end_datetime"] + 1 #one second after the last raster end_datetime
    source_fx <- meta_ids[i, "source_fx"]
    source_fx_args <- meta_ids[i, "source_fx_args"]

    tryCatch({
      args_list <- list(start_datetime = next_instant)
      if (!is.na(source_fx_args)){ #add some arguments if they are specified
        args <- strsplit(source_fx_args, "\\},\\s*\\{")
        pairs <- lapply(args, function(pair){
          gsub("[{}]", "", pair)
        })
        pairs <- lapply(pairs, function(pair){
          gsub("\"", "", pair)
        })
        pairs <- lapply(pairs, function(pair){
          gsub("'", "", pair)
        })
        pairs <- strsplit(unlist(pairs), "=")
        pairs <- lapply(pairs, function(pair){
          trimws(pair)
        })
        for (j in 1:length(pairs)){
          args_list[[pairs[[j]][1]]] <- pairs[[j]][[2]]
        }
      }
      rasters <- do.call(source_fx, args_list) #Get the data using the args_list

      for (j in 1:length(rasters)){
        rast <- rasters[[j]]
        #TODO fix this
        # insertModelRaster <- function(con, raster, model, valid_from, valid_to, issued, units = NULL, source = NULL, bit.depth = NULL, blocks = NULL)
        insertModelRaster(raster = rast, raster_series_id = id, datetime = rast$timestamp, fetch_datetime = Sys.time(), con = con)
        raster_count <- raster_count + 1
      }
      count <- count + 1
      success <- c(success, id)
    }, error = function(e){
      warning("getNewRasters: Failed to get new rasters or to append new rasters for raster_series_id ", id, ".")
    })
  }
  message(count, " out of ", nrow(meta_ids), " raster_series_id's were updated.")
  message(raster_count, " were added in total.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_rasters'"))
  return(success)
}
