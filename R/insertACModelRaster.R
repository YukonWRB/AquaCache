#' Add a model-output raster file to the database
#'
#' @description
#' To create a new raster series use function [addACRasterSeries()]. Use this function to add a raster file output by a model (forecast or reanalysis) to the aquacache database. Ensures that database constraints are met. If you need to replace or delete a raster for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first. If the raster is not created by a model use function [insertACRaster()] instead.
#'
#' Depending on size, rasters might be broken up into many tiles. Because of this and the database's spatial capabilities, it's possible to only fetch the tiles you need using [rpostgis::pgGetRast()]. You'll have to specify which reference_id to use as a clause; find the right one in the 'rasters_reference' table. Look at the parameter `boundary` to specify a limited spatial extent, and at `bands` to only fetch certain bands. The rasters themselves live in the 'rasters' table, but the reference id in in the 'rasters_reference' table.
#'
#' @param con A connection to the database. Leave NULL to use the package default connection settings and have the connection automatically closed afterwards.
#' @param raster The raster object to add to the database, as a [terra::rast()] object, as a file path, or as a valid URL. Can be multi-band. Band names will be taken directly from this raster.
#' @param raster_series_id The raster_series_id for the model, matching an entry in the raster_series_index table.
#' @param valid_from Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param valid_to Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param description An optional description for the raster.
#' @param flag An optional flag for the raster, perhaps used for overwriting preliminary rasters later on.
#' @param issued The issued datetime of the raster. Must be a .POSIXct object or character vector that can be coerced to one. Character vectors will be converted assuming a UTC offset of 0.
#' @param units The units associated with each band, as a character vector. If left NULL function will attempt to retrieve units from the raster metadata. Otherwise if specified must be a vector of 1 or of length equal to the number of raster bands.
#' @param model The model which created the raster.
#' @param source The source from which this raster was retrieve (optional but recommended).
#' @param bit.depth The bit depth of the raster. 32-bit float is '32BF', 32-bit unsigned integer is '32BUI', 32-bit signed integer is '32BSI'. Default to NULL which will parse the data to determine which 32-bit flavor to choose. You **must** specify if your data is greater than 32 bit.
#' @param blocks The number of blocks in which to break the raster apart. If NULL blocks will be automatically determined, as per [rpostgis::pgWriteRast()]. Blocks (tiles) can be useful to speed up queries that only need a small subset of the raster, but lengthen write time.
#'
#' @return The reference_id of the newly appended raster.
#' @export

insertACModelRaster <- function(con = NULL, raster, raster_series_id, valid_from, valid_to, description = NA, flag = NA, issued = NA, units = NULL, model = NA, source = NA, bit.depth = NULL, blocks = NULL)
{
  
  # Parameter check in case they're passed as NULL by other functions
  if (is.null(description)) {
    description <- NA
  }
  if (is.null(flag)) {
    flag <- NA
  }
  if (is.null(issued)) {
    issued <- NA
  }
  if (is.null(source)) {
    source <- NA
  }
  if (is.null(model)) {
    model <- NA
  }

  # Checking valid_from parameter
  tryCatch({
    if (inherits(valid_from, "character") & nchar(valid_from) > 10) { #Does not necessarily default to 0 hour.
      valid_from <- as.POSIXct(valid_from, tz = "UTC")
    } else if (inherits(valid_from, "POSIXct")) {
      attr(valid_from, "tzone") <- "UTC"
    } else if (inherits(valid_from, "Date") | (inherits(valid_from, "character") & nchar(valid_from) == 10)) { #defaults to 0 hour
      valid_from <- as.POSIXct(valid_from, tz = "UTC")
    } else {
      stop("Parameter valid_from could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter valid_from to POSIXct.")
  })

  # Checking valid_to parameter
  tryCatch({
    if (inherits(valid_to, "character") & nchar(valid_to) > 10) { #Does not necessarily default to 0 hour.
      valid_to <- as.POSIXct(valid_to, tz = "UTC")
    } else if (inherits(valid_to, "POSIXct")) {
      attr(valid_to, "tzone") <- "UTC"
    } else if (inherits(valid_to, "Date") | (inherits(valid_to, "character") & nchar(valid_to) == 10)) { #defaults to very end of day
      valid_to <- as.POSIXct(valid_to, tz = "UTC")
      valid_to <- valid_to + 60*60*23.9999
    } else {
      stop("Parameter valid_to could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter valid_to to POSIXct.")
  })

  # Checking issued parameter
  if (!is.na(issued)) {
    tryCatch({
      if (inherits(issued, "character") & nchar(issued) > 10) { #Does not necessarily default to 0 hour.
        issued <- as.POSIXct(issued, tz = "UTC")
      } else if (inherits(issued, "POSIXct")) {
        attr(issued, "tzone") <- "UTC"
      } else if (inherits(issued, "Date") | (inherits(issued, "character") & nchar(issued) == 10)) { #defaults to very end of day
        issued <- as.POSIXct(issued, tz = "UTC")
        issued <- issued + 60*60*23.9999
      } else {
        stop("Parameter issued could not be coerced to POSIXct.")
      }
    }, error = function(e) {
      stop("Failed to convert parameter issued to POSIXct.")
    })
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  if (!("rasters_reference" %in% DBI::dbListTables(con))) {
    message("table rasters_reference does not already exist. Creating it.")
    version <- DBI::dbGetQuery(con, "SELECT version()")
    if (grepl("PostgreSQL", version$version)) {
      DBI::dbExecute(con, "CREATE TABLE rasters_reference (
                   reference_id SERIAL PRIMARY KEY,
                   raster_series_id INTEGER,
                   type TEXT CHECK(type IN ('model', 'other')),
                   model TEXT,
                   description TEXT,
                   flag TEXT,
                   band_names TEXT NOT NULL,
                   units TEXT,
                   valid_from TIMESTAMP WITH TIME ZONE,
                   valid_to TIMESTAMP WITH TIME ZONE,
                   issued TIMESTAMP WITH TIME ZONE,
                   source TEXT,
                   UNIQUE NULLS NOT DISTINCT (raster_series_id, flag, valid_from, valid_to, issued),
                   CONSTRAINT check_model_constraints
                     CHECK (
                     (type = 'model' AND valid_from IS NOT NULL AND valid_to IS NOT NULL) OR
                     (type = 'other' AND description IS NOT NULL)
                     )
                     );")
    } else if (grepl("Microsoft", version$version)) {
      DBI::dbExecute(con, "CREATE TABLE rasters_reference (
                   reference_id INT IDENTITY(1,1) PRIMARY KEY,
                   raster_series_id INTEGER,
                   type VARCHAR(MAX) CHECK(type IN ('model', 'other')),
                   model VARCHAR(MAX),
                   description VARCHAR(MAX),
                   flag VARCHAR(MAX),
                   band_names VARCHAR(MAX) NOT NULL,
                   units VARCHAR(MAX),
                   valid_from TIMESTAMP WITH TIME ZONE,
                   valid_to TIMESTAMP WITH TIME ZONE,
                   issued TIMESTAMP WITH TIME ZONE,
                   source VARCHAR(MAX),
                   UNIQUE NULLS NOT DISTINCT (raster_series_id, flag, valid_from, valid_to, issued),
                   CONSTRAINT check_model_constraints
                     CHECK (
                     (type = 'model' AND valid_from IS NOT NULL AND valid_to IS NOT NULL) OR
                     (type = 'other' AND description IS NOT NULL)
                     )
                     );")
    } else {
      stop("This script is designed to work with either postgreSQL or SQL server databases.")
    }
    add_constraints <- TRUE
  } else {
    add_constraints <- FALSE
  }

  #Make sure that if units are provided that there's either one total or 1 per band
  if (!is.null(units)) {
    if (!inherits(units, "character")) {
      stop("Parameter units must be specified as a character vector.")
    }
    if (length(units) > 1) {
      if (length(units) != length(names(raster))) {
        stop("The parameter units was provided, but there isn't exactly one element or one element per band in the raster.")
      }
      units <- paste(units, collapse = ", ")
    }
  } else {
    units <- paste(terra::units(raster), collapse = ", ")
  }

  # Attempt to write the raster to the database
  res <- writeRaster(con = con, raster = raster, rast_table = c("spatial","rasters"), bit.depth = bit.depth, blocks = blocks,
              constraints = TRUE)

  if (res$status) {
    # band names
    bnds <- DBI::dbQuoteString(con, paste0("{{",paste(names(raster),collapse = "},{"),"}}"))
    entry <- data.frame("raster_series_id" = raster_series_id,
                        "type" = "model",
                        "model" = model,
                        "band_names" = bnds,
                        "units" = units,
                        "valid_from" = valid_from,
                        "valid_to" = valid_to,
                        "issued" = issued,
                        "source" = source,
                        "description" = description,
                        "flag" = flag)
    DBI::dbAppendTable(con, "rasters_reference", entry)
    new_id <- DBI::dbGetQuery(con, "SELECT max(reference_id) FROM rasters_reference")[1,1]
    
    DBI::dbExecute(con, paste0("UPDATE rasters SET reference_id = ", new_id, " WHERE rid IN (", paste(res$appended_rids, collapse = ","), ");"))

    if (add_constraints) {
      DBI::dbExecute(con, "ALTER TABLE rasters ADD CONSTRAINT fk_reference_id FOREIGN KEY (reference_id) REFERENCES rasters_reference(reference_id) ON DELETE CASCADE ON UPDATE CASCADE")
      DBI::dbExecute(con, "ALTER TABLE rasters_reference ADD CONSTRAINT fk_raster_series_id FOREIGN KEY (raster_series_id) REFERENCES raster_series_index(raster_series_id) ON DELETE CASCADE ON UPDATE CASCADE")
      DBI::dbExecute(con, "COMMENT ON TABLE public.rasters_reference IS 'References rasters in the rasters table, since the later might have rasters broken up in multiple tiles. This table has one reference_id per raster, which may be linked to multiple entries in table rasters.'")
      DBI::dbExecute(con, "COMMENT ON COLUMN public.rasters_reference.flag IS 'Used to flag rasters that require further review or that need to be deleted after a certain period. Reanalysis products in particular can have preliminary issues, in which case PRELIMINARY would be entered here.'")
      DBI::dbExecute(con, "COMMENT ON COLUMN public.rasters_reference.reference_id IS 'Used to identify one or more raster tiles (large rasters may be broken up in tiles for performance) in the table rasters.'")
      DBI::dbExecute(con, "COMMENT ON COLUMN public.rasters_reference.raster_series_id IS 'Identifies a time-series of rasters, the details of which are stored in table raster_series_index.'")
      DBI::dbExecute(con, "COMMENT ON COLUMN public.rasters_reference.model IS 'If the raster is generated from a model such as a climate model enter the name here. This is more useful for one-off rasters, as model timeseries will also list the model in table raster_series_index.'")
    }

    return(new_id)
  }
}
