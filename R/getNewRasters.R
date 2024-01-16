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

# raster <- terra::rast("https://dd.weather.gc.ca/model_hrdpa/2.5km/06/20231217T06Z_MSC_HRDPA_APCP-Accum6h_Sfc_RLatLon0.0225_PT0H.grib2")
# raster <- raster[[1]]

getNewRasters <- function(raster_series_ids = "all", con = hydrometConnect(silent=TRUE)) {

  # Create table of meta_ids
  if (raster_series_ids[1] == "all"){
    meta_ids <- DBI::dbGetQuery(con, "SELECT raster_series_id, end_datetime, source_fx, source_fx_args FROM raster_series_index WHERE source_fx IS NOT NULL;")
  } else {
    meta_ids <- DBI::dbGetQuery(con, paste0("SELECT raster_series_id, end_datetime, source_fx, source_fx_args FROM raster_series_index WHERE raster_series_id IN ('", paste(raster_series_ids, collapse = "', '"), "') AND source_fx IS NOT NULL;"))
    if (length(raster_series_ids) != nrow(meta_ids)){
      warning("At least one of the raster_series_ids you called for cannot be found in the database or has no function specified in column source_fx of table raster_series_index")
    }
  }

  count <- 0 #counter for number of successful new pulls
  raster_count <- 0
  success <- character(0)

  for (i in 1:nrow(meta_ids)){
    id <- meta_ids[i, "raster_series_id"]
    source_fx <- meta_ids[i, "source_fx"]
    source_fx_args <- meta_ids[i, "source_fx_args"]
    if (source_fx == "getNewHRDPA"){
      prelim <- DBI::dbGetQuery(con, paste0("SELECT min(valid_from) FROM rasters_reference WHERE description = 'PRELIMINARY' AND valid_from > '", meta_ids[i, "end_datetime"] - 60*60*24*30, "';"))[1,1] #searches for rasters labelled 'prelim' within the last 30 days. If exists, try to replace it and later rasters (there shouldn't be any later ones)
      if (!is.na(prelim)){
        next_instant <- prelim - 1
      } else {
        next_instant <- meta_ids[i, "end_datetime"] + 1
      }
    } else {
      next_instant <- meta_ids[i, "end_datetime"] + 1 #one second after the last raster end_datetime, unless the description field lists PRELIMINARY
    }

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

      if (!is.null(rasters)){
        for (j in 1:length(rasters)){
          rast <- rasters[[j]]
          valid_from <- rast[["valid_from"]]
          valid_to <- rast[["valid_to"]]
          source <- rast[["source"]]
          description <- rast[["description"]]
          units <- rast[["units"]]
          model <- rast[["model"]]
          rast <- rast[["rast"]]
          #Check if the raster already exists. If it does but description is PRELIMINARY AND the new one is not, delete the prelim one and replace.
          exists <- DBI::dbGetQuery(con, paste0("SELECT reference_id FROM rasters_reference WHERE valid_from = '", valid_from, "' AND raster_series_id = ", id, " AND description = 'PRELIMINARY';"))[1,1]
          if (!is.na(exists) & description != "PRELIMINARY"){
            DBI::dbExecute(con, paste0("DELETE FROM rasters_reference WHERE reference_id = ", exists, ";")) #This should cascade to the rasters table
          }
          insertModelRaster(raster = rast, raster_series_id = id, valid_from = valid_from, valid_to = valid_to, description = description, source = source, units = units, model = model, con = con)
          DBI::dbExecute(con, paste0("UPDATE raster_series_index SET last_new_raster = '", .POSIXct(Sys.time(), tz = "UTC"), "' WHERE raster_series_id = ", id, ";"))
          DBI::dbExecute(con, paste0("UPDATE raster_series_index SET end_datetime = '", valid_to, "' WHERE raster_series_id = ", id, ";"))

          raster_count <- raster_count + 1
        }
        count <- count + 1
        success <- c(success, id)
      }

    }, error = function(e){
      warning("getNewRasters: Failed to get new rasters or to append new rasters for raster_series_id ", id, ".")
    })
  }
  message(count, " out of ", nrow(meta_ids), " raster_series_id's were updated.")
  message(raster_count, " were added in total.")
  DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_rasters'"))
  return(success)
}
