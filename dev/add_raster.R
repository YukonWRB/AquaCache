#' Add a raster file to the database
#'
#' Use this function to add a raster file such as a DEM or precipitation raster to the database. Ensures that database constraints are met. If you need to replace or delete a vector for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first or using [prpostgis::pgWriteGeom()] with the necessary parameters.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param raster The raster object to add to the database, as a [terra::rast()] object.
#' @param parameter The parameter which this raster represents, such as "elevation" or "precip_total".
#' @param units The units applicable for the raster values, such as "mm" or "kg/m^2".
#' @param description An optional (and highly recommended) description of the raster. If left NULL the raster name embedded in the file name will attempt to be retrieved from the source.
#' @param valid_from For rasters that have a valid from and valid to time, such as forecast climate parameters. Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param valid_to For rasters that have a valid from and valid to time, such as forecast climate parameters. Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param issued The issued datetime of the raster. Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param source The source from which this raster was retrieved.
#' @param epsg Normally the function will get the epsg code directly from the raster file, but files sometimes do not have this information. If you get an error about this try setting the epsg code here as a numeric vector.
#'
#' @return Nothing, but the vector should be added to the DB if no error.
#' @export
#'

add_raster <- function(con, raster, parameter, units, description = NULL, valid_from = NULL, valid_to = NULL, issued = NULL, source = NULL, epsg = NULL){

  #testing parameters
  rast <- terra::rast("https://dd.weather.gc.ca/analysis/precip/hrdpa/grib2/polar_stereographic/06/CMC_HRDPA_APCP-006-0100cutoff_SFC_0_ps2.5km_2023100700_000.grib2")
  parameter <- "precip_total"
  units <- "kg/m^2"
  description <- NULL
  valid_from = as.POSIXct("2023-06-01 12:00:00", tz = "UTC")
  valid_to = as.POSIXct("2023-06-01 18:00:00", tz = "UTC")
  valid_from = as.POSIXct("2023-06-01 12:00:00", tz = "UTC")

  if (is.null(description)){
    try(description <- names(rast)[1])
  }

  # Convert times if not null
  if (!is.null(valid_from)){
    if (!inherits(valid_from, "POSIXct")){
      valid_from <- as.POSIXct(valid_from, tz = "UTC")
    }
  }
  if (!is.null(valid_to)){
    if (!inherits(valid_to, "POSIXct")){
      valid_to <- as.POSIXct(valid_to, tz = "UTC")
    }
  }
  if (!is.null(issued)){
    if (!inherits(issued, "POSIXct")){
      issued <- as.POSIXct(issued, tz = "UTC")
    }
  }

  #Check for an embedded authority:code, assign if none
  crs <- terra::crs(rast, describe = TRUE)
  if (is.na(crs$authority) | is.na(crs$code)){
    if (is.null(epsg)){
      stop("There is no epsg code that could be read from the raster file, and you didn't specify one with the parameter 'epsg'. Please specify one or fix the file.")
    } else {
      crs <- epsg
    }
  } else {
    crs <- crs$code
  }

  # Prepare the SQL insert statement with CRS information
  insert_sql <- paste0("INSERT INTO rasters (parameter, description, units, valid_from, valid_to, issued, source, bands, raster) VALUES ('", parameter, "', '", description, "', '", units, "', '", valid_from, "', '", valid_to, "', '", issued, "', '", source, "', ", bnds, ",
                       ST_SetSRID(
                       ST_SetValues(
                       ST_AddBand(
                       ST_MakeEmptyRaster(", dim(rast)[2], ",", dim(rast)[1], ",", terra::ext(rast)[1], ",", terra::ext(rast)[4], ",", terra::res(rast)[1], ",", -terra::res(rast)[2], ", 0, 0, ", crs, "))));")

}

