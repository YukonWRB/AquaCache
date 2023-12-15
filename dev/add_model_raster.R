#' Add a model-output raster file to the database
#'
#' Use this function to add a raster file output by a model (forecast or reanalysis) to the database. Ensures that database constraints are met. If you need to replace or delete a vector for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first or using [prpostgis::pgWriteGeom()] with the necessary parameters.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param raster The raster object to add to the database, as a [terra::rast()] object, as a file path, or as a valid URL. Can be multi-band.
#' @param model The model which generated the raster.
#' @param valid_from Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param valid_to Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param issued The issued datetime of the raster. Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param source The source from which this raster was retrieve (optional but recommended).
#' @param bit.depth The bit depth of the raster. 32-bit float is '32BF', 32-bit unsigned integer is '32BUI', 32-bit signed integer is '32BSI'. Default to NULL which will parse the data to determine which 32-bit flavor to choose. You **must** specify if your data is greater than 64 bit.
#'
#' @return The rid of the newly appended raster.
#' @export

add_model_raster <- function(con, raster, model, valid_from, valid_to, issued, source = NULL, bit.depth = NULL){

  #testing parameters
  raster <- terra::rast("https://dd.weather.gc.ca/model_hrdpa/2.5km/00/20231123T00Z_MSC_HRDPA_APCP-Accum6h_Sfc_RLatLon0.0225_PT0H.grib2")
  description <- NULL
  valid_from = as.POSIXct("2023-06-01 12:00:00", tz = "UTC")
  valid_to = as.POSIXct("2023-06-01 18:00:00", tz = "UTC")
  valid_from = as.POSIXct("2023-06-01 12:00:00", tz = "UTC")


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

  #Find the corresponding SRID in the database, create a new one if it doesn't exist
  srid <- 0
  try(srid <- suppressMessages(rpostgis::pgSRID(con, sf::st_crs(terra::crs(raster)), create.srid = TRUE)),
      silent = TRUE)
  # Warning about no CRS
  if (length(srid) == 1) {
    if (srid == 0) warning("The raster has no CRS specified.")
  }

  #Get the necessary data for the bands
  n.bands <- terra::nlyr(raster)
  bands_info <- list()
  for (i in 1:n.bands){
    bands_info[[i]] <- list(parameter = names(raster)[i], units = terra::units(raster)[1])
  }
  jsonb <- jsonlite::toJSON(bands_info, auto_unbox = TRUE)

  #Get information about the raster
  dims <- dim(raster)
  ex <- terra::ext(raster)
  res <- terra::res(raster)
  # figure out bit depth
  if (is.null(bit.depth)) {
    if (is.integer(terra::values(r1))) {
      if (min(terra::values(r1), na.rm = TRUE) >= 0) {
        bit.depth <- "32BUI"
      } else {
        bit.depth <- "32BSI"
      }
    } else {
      bit.depth <- "32BF"
    }
  }
  bit.depth <- DBI::dbQuoteString(conn, bit.depth)
  ndval <- -99999

  # Write the raster to file #############
  # Drop constraints first (they are re-added after)
  try(dbExecute(conn, paste0("SELECT DropRasterConstraints('", namef[1], "','", namef[2], "','rast',",
                             paste(rep("TRUE", 12), collapse = ","),");")))

  # 1. Start with the empty raster
  empty_rast <- paste0(dims[2], ", ", dims[1], ", ", ex[1], ", ", ex[4], ", ", res[1], ", ", res[2], ", 0, 0, ", srid[1], ")")

  add_empty <- paste0("INSERT INTO rasters_model_outputs(model, valid_from, valid_to, issued, source, bands, rast) VALUES ('", model, "', '", valid_from, "', '", valid_to, "', '", issued, "', '", source, "', '", jsonb, "', ST_MakeEmptyRaster(", empty_rast, "));")
  DBI::dbExecute(con, add_empty)
  new_rid <- DBI::dbGetQuery(con, "SELECT max(rid) FROM rasters_model_outputs")

  # 2. Add the bands (empty for now)
  bndargs <- paste0("ROW(",1:length(names(raster)),",",bit.depth,"::text,0,", ndval,")")
  if (res[1] != res[2]) s2g <- paste0(", ", res[1], ", ", -res[2]) else s2g <- NULL #Check if x and y resolution is equal or not to form string
  add_bands <- paste0("UPDATE rasters_model_outputs SET rast = ST_SnapToGrid(ST_AddBand(rast,ARRAY[",
                      paste(bndargs,collapse = ","),"]::addbandarg[]), ", ex[1], "," , ex[4] , s2g, ") ",
                      "where rid = ",
                      new_rid, ";")
  DBI::dbExecute(con, add_bands)

  # 3. Populate the bands with actual values
  for (i in 1:length(names(raster))){
    matrix <- terra::as.matrix(raster[[i]], wide = TRUE)
    matrix[is.na(matrix)] <- ndval
    write_raster <- paste(apply(matrix, 1, FUN = function(x) {
      paste0("[", paste(x, collapse = ","), "]")
    }), collapse = ",")
    tmp.query <- paste0("UPDATE rasters_model_outputs SET rast = ST_SetValues(rast,", i,", 1, 1, ARRAY[",
                        write_raster, "]::double precision[][]) where rid = ", new_rid, ";")
    dbExecute(con, tmp.query)
  }

  # add raster constraints back in
    dbExecute(con, "SELECT AddRasterConstraints('rasters_model_outputs'::name, 'rast'::name);")

    test <- rpostgis::pgGetRast(con, "rasters_model_outputs")
    test <- DBI::dbGetQuery(con, "SELECT rast FROM rasters_model_outputs WHERE rid = 1;")
    test <- as.matrix(test$rast)
    terra::rast(test)
}
