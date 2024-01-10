#' Add a raster file to the database
#'
#' @description
#' Use this function to add a raster file to the database that wasn't created by a model (use [addModelRaster()] for that). Ensures that database constraints are met. If you need to replace or delete a raster for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first, remembering to delete the matching entries in the rasters and rasters_model_output tables.
#'
#' Depending on size, rasters might be broken up into many tiles. Because of this and the database's spatial capabilities, it's possible to only fetch the tiles you need using [rpostgis::pgGetRast()]. You'll have to specify which reference_id to use as a clause; find the right one in the 'rasters_reference' table. Look at the parameter `boundary` to specify a limited spatial extent, and at `bands` to only fetch certain bands. The rasters themselves live in the 'rasters' table, but the reference id in in the 'rasters_reference' table.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param raster The raster object to add to the database, as a [terra::rast()] object, as a file path, or as a valid URL. Can be multi-band. Band names will be taken directly from this raster.
#' @param description A succint description for the raster.
#' @param units The units associated with each band, as a character vector. If left NULL function will attempt to retrieve units from the raster metadata. Otherwise if specified must be a vector of 1 or of length equal to the number of raster bands.
#' @param source The source from which this raster was retrieved (optional but recommended).
#' @param bit.depth The bit depth of the raster. 32-bit float is '32BF', 32-bit unsigned integer is '32BUI', 32-bit signed integer is '32BSI'. Default to NULL which will parse the data to determine which 32-bit flavor to choose. You **must** specify if your data is greater than 32 bit.
#' @param blocks The number of blocks in which to break the raster apart. If NULL blocks will be automatically determined, as per [rpostgis::pgWriteRast()]. Blocks (tiles) can be useful to speed up queries that only need a small subset of the raster, but lengthen write time.
#'
#' @return The reference_id of the newly appended raster.
#' @export

addRaster <- function(con, raster, description, units = NULL, source = NULL, bit.depth = NULL, blocks = NULL)
{

  if(!("rasters_reference" %in% DBI::dbListTables(con))) {
    message("rasters_reference does not already exist. Creating it.")
    version <- DBI::dbGetQuery(con, "SELECT version()")
    if (grepl("PostgreSQL", version$version)){
      DBI::dbExecute(con, "CREATE TABLE rasters_reference (
                   reference_id SERIAL PRIMARY KEY,
                   type TEXT CHECK(type IN ('model', 'other')),
                   model TEXT,
                   description TEXT,
                   band_names TEXT NOT NULL,
                   units TEXT,
                   valid_from TIMESTAMP WITH TIME ZONE,
                   valid_to TIMESTAMP WITH TIME ZONE,
                   issued TIMESTAMP WITH TIME ZONE,
                   source TEXT,
                   CONSTRAINT check_model_constraints
                     CHECK (
                     (type = 'model' AND model IS NOT NULL AND valid_from IS NOT NULL AND valid_to IS NOT NULL AND issued IS NOT NULL) OR
                     (type = 'other' AND description IS NOT NULL)
                     )
                     );")
    } else if (grepl("Microsoft", version$version)) {
      DBI::dbExecute(con, "CREATE TABLE rasters_reference (
                   reference_id INT IDENTITY(1,1) PRIMARY KEY,
                   type VARCHAR(MAX) CHECK(type IN ('model', 'other')),
                   model VARCHAR(MAX),
                   description (VARCHAR(MAX),
                   band_names VARCHAR(MAX) NOT NULL,
                   units VARCHAR(MAX),
                   valid_from TIMESTAMP WITH TIME ZONE,
                   valid_to TIMESTAMP WITH TIME ZONE,
                   issued TIMESTAMP WITH TIME ZONE,
                   source VARCHAR(MAX),
                   CONSTRAINT check_model_constraints
                     CHECK (
                     (type = 'model' AND model IS NOT NULL AND valid_from IS NOT NULL AND valid_to IS NOT NULL AND issued IS NOT NULL) OR
                     (type = 'other' AND description IS NOT NULL)
                     )
                     );")
    } else {
      stop("This script is designed to work with either postgreSQL or SQL server databases.")
    }
    add_constraints <- TRUE
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
    entry <- data.frame("type" = "other",
                        "band_names" = bnds,
                        "units" = units,
                        "description" = description,
                        "source" = if (is.null(source)) NA else source)
    DBI::dbAppendTable(con, "rasters_reference", entry)
    new_id <- DBI::dbGetQuery(con, "SELECT max(reference_id) FROM rasters_reference")[1,1]
    DBI::dbExecute(con, paste0("UPDATE rasters SET reference_id = ", new_id, " WHERE rid IN (", paste(res$appended_rids, collapse = ","), ");"))

    if (add_constraints){
      DBI::dbExecute(con, "ALTER TABLE rasters ADD CONSTRAINT fk_reference_id FOREIGN KEY (reference_id) REFERENCES rasters_reference(reference_id) ON DELETE CASCADE ON UPDATE CASCADE")
    }

    return (new_id)
  }
}
