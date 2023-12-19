#' Add a model-output raster file to the database
#'
#' @description
#' Use this function to add a raster file output by a model (forecast or reanalysis) to the database. Ensures that database constraints are met. If you need to replace or delete a vector for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first.
#'
#' You can fetch the rasters using [rpostgis::pgGetRast()]
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param raster The raster object to add to the database, as a [terra::rast()] object, as a file path, or as a valid URL. Can be multi-band. Band names will be taken directly from this raster.
#' @param model The model which generated the raster.
#' @param valid_from Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param valid_to Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param issued The issued datetime of the raster. Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param units The units associated with each band, as a character vector. If left NULL function will attempt to retrieve units from the raster metadata. Otherwise must be a vector of 1 or of length equal to the number of raster bands.
#' @param source The source from which this raster was retrieve (optional but recommended).
#' @param bit.depth The bit depth of the raster. 32-bit float is '32BF', 32-bit unsigned integer is '32BUI', 32-bit signed integer is '32BSI'. Default to NULL which will parse the data to determine which 32-bit flavor to choose. You **must** specify if your data is greater than 32 bit.
#' @param blocks The number of blocks in which to break the raster apart. If NULL blocks will be automatically determined, as per [rpostgis::pgWriteRast()]. Blocks (tiles) can be useful to speed up queries that only need a small subset of the raster, but lengthen write time.
#'
#' @return The reference_id of the newly appended raster.
#' @export


#This function will need to call writeRaster (the rpostgis knock-off) to add the raster, then make an SQL call to add data to the raster_reference table. Then, another function should be created that fetches rasters based on the values in the reference table, calling pgGetRaster.

add_model_raster <- function(con, raster, model, valid_from, valid_to, issued, units = NULL, source = NULL, bit.depth = NULL, blocks = NULL)
{


  # Checking valid_from parameter
  tryCatch({
    if (inherits(valid_from, "character") & nchar(valid_from) > 10){ #Does not necessarily default to 0 hour.
      valid_from <- as.POSIXct(valid_from, tz = "UTC")
    } else if (inherits(valid_from, "POSIXct")){
      attr(valid_from, "tzone") <- "UTC"
    } else if (inherits(valid_from, "Date") | (inherits(valid_from, "character") & nchar(valid_from) == 10)){ #defaults to 0 hour
      valid_from <- as.POSIXct(valid_from, tz = "UTC")
    } else {
      stop("Parameter valid_from could not be coerced to POSIXct.")
    }
  }, error = function(e){
    stop("Failed to convert parameter valid_from to POSIXct.")
  })

  # Checking valid_to parameter
  tryCatch({
    if (inherits(valid_to, "character") & nchar(valid_to) > 10){ #Does not necessarily default to 0 hour.
      valid_to <- as.POSIXct(valid_to, tz = "UTC")
    } else if (inherits(valid_to, "POSIXct")){
      attr(valid_to, "tzone") <- "UTC"
    } else if (inherits(valid_to, "Date") | (inherits(valid_to, "character") & nchar(valid_to) == 10)){ #defaults to very end of day
      valid_to <- as.POSIXct(valid_to, tz = "UTC")
      valid_to <- valid_to + 60*60*23.9999
    } else {
      stop("Parameter valid_to could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter valid_to to POSIXct.")
  })

  # Checking issued parameter
  tryCatch({
    if (inherits(issued, "character") & nchar(issued) > 10){ #Does not necessarily default to 0 hour.
      issued <- as.POSIXct(issued, tz = "UTC")
    } else if (inherits(issued, "POSIXct")){
      attr(issued, "tzone") <- "UTC"
    } else if (inherits(issued, "Date") | (inherits(issued, "character") & nchar(issued) == 10)){ #defaults to very end of day
      issued <- as.POSIXct(issued, tz = "UTC")
      issued <- issued + 60*60*23.9999
    } else {
      stop("Parameter issued could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter issued to POSIXct.")
  })



  if(!("rasters_model_outputs" %in% DBI::dbListTables(con))) {
    DBI::dbExecute(con, "CREATE TABLE rasters_model_outputs (
                   reference_id SERIAL PRIMARY KEY,
                   model TEXT NOT NULL,
                   band_names TEXT NOT NULL,
                   units TEXT,
                   valid_from TIMESTAMP WITH TIME ZONE NOT NULL,
                   valid_to TIMESTAMP WITH TIME ZONE NOT NULL,
                   issued TIMESTAMP WITH TIME ZONE NOT NULL,
                   source TEXT);")
  }

  #Make sure that if units are provided that there's either one or 1 per band
  if (!is.null(units)){
    if (!inherits(units, "character")){
      stop("Parameter units must be specified as a character vector.")
    }
    if (length(units) > 1){
      if (length(units) != length(names(raster))){
        stop("The parameter units was provided, but there isn't exactly one element or one element per band in the raster.")
      }
      units <- paste(units, collapse = ", ")
    }
  } else {
    units <- paste(terra::units(raster), collapse = ", ")
  }

  # Attempt to write the raster to file
  res <- writeRaster(con = con, raster = raster, rast_table = "rasters", bit.depth = bit.depth, blocks = blocks,
              constraints = TRUE)

  if (res$status){
    # band names
    bnds <- DBI::dbQuoteString(con, paste0("{{",paste(names(raster),collapse = "},{"),"}}"))
    entry <- data.frame("model" = model,
                        "band_names" = bnds,
                        "units" = units,
                        "valid_from" = valid_from,
                        "valid_to" = valid_to,
                        "issued" = issued,
                        "source" = if (is.null(source)) NA else source)
    DBI::dbAppendTable(con, "rasters_model_outputs", entry)
    new_id <- DBI::dbGetQuery(con, "SELECT max(reference_id) FROM rasters_model_outputs")[1,1]
    DBI::dbExecute(con, paste0("UPDATE rasters SET reference_id = ", new_id, " WHERE rid IN (", paste(res$appended_rids, collapse = ","), ");"))
    return (new_id)
  }
}
