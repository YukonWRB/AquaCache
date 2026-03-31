#' Add a raster file to the database
#'
#' @description
#' Use this function to add a raster file to the database that wasn't created by a model (use [insertACModelRaster()] for that). Ensures that database constraints are met. If you need to replace or delete a raster for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first, remembering to delete the matching entries in the rasters and rasters_model_output tables.
#'
#' Depending on size, rasters might be broken up into many tiles. Because of this and the database's spatial capabilities, it's possible to only fetch the tiles you need using [rpostgis::pgGetRast()]. You'll have to specify which reference_id to use as a clause; find the right one in the 'rasters_reference' table. Look at the parameter `boundary` to specify a limited spatial extent, and at `bands` to only fetch certain bands.
#'
#' A note about raster organization: the rasters themselves live in the 'rasters' table, but the reference id in in the 'rasters_reference' table. This is in contrast to some PostGIS implementations where rasters are stored either in a single table - one row per raster - or in separate tables for each raster. The first approach doesn't allow for tiling, which is a big deal on large rasters, and the second approach makes the management of rasters difficult, especially with our approach of auto-fetching raster series. The AquaCache approach takes the good from both methods: rasters are stored in a single table, but the reference_id allows for multiple rows to be linked to the same raster, which allows for tiling. The rasters_reference table also allows us to store metadata about the rasters, such as the model they came from, their valid time range, and so on.
#'
#' @param con A connection to the database. Leave NULL to use the package default connection settings and have the connection closed automatically afterwards.
#' @param raster The raster object to add to the database, as a [terra::rast()] object, as a file path, or as a valid URL. Can be multi-band. Band names will be taken directly from this raster.
#' @param description A succinct description for the raster.
#' @param flag An optional flag for the raster, perhaps used for overwriting preliminary rasters later on.
#' @param units The units associated with each band, as a character vector. If left NULL function will attempt to retrieve units from the raster metadata. Otherwise if specified must be a vector of 1 or of length equal to the number of raster bands.
#' @param source The source from which this raster was retrieved (optional but recommended).
#' @param bit.depth The bit depth of the raster. 32-bit float is '32BF', 32-bit unsigned integer is '32BUI', 32-bit signed integer is '32BSI'. Default to NULL which will parse the data to determine which 32-bit flavor to choose. You **must** specify if your data is greater than 32 bit.
#' @param blocks The number of blocks in which to break the raster apart. If NULL blocks will be automatically determined, as per [rpostgis::pgWriteRast()]. Blocks (tiles) can be useful to speed up queries that only need a small subset of the raster, but lengthen write time.
#'
#' @return The reference_id of the newly appended raster.
#' @export

# parameter saved for later:
# @param pyramid_levels The number of pyramid levels to create for the raster. If NULL, no pyramids will be created. Pyramids can speed up queries that only need a small subset of the raster, but lengthen write time and increase storage requirements. Function

insertACRaster <- function(
  con = NULL,
  raster,
  description,
  flag = NA,
  units = NULL,
  source = NULL,
  bit.depth = NULL,
  blocks = NULL
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  if (!("rasters_reference" %in% DBI::dbListTables(con))) {
    stop(
      "spatial.rasters_reference does not exist in this database. Apply the AquaCache schema before calling insertACRaster()."
    )
  }

  if (inherits(raster, "character")) {
    raster <- terra::rast(raster)
  }

  #Make sure that if units are provided that there's either 1 or 1 per band
  if (!is.null(units)) {
    if (!inherits(units, "character")) {
      stop("Parameter units must be specified as a character vector.")
    }
    if (length(units) > 1) {
      if (length(units) != length(names(raster))) {
        stop(
          "The parameter units was provided, but there isn't exactly one element or one element per band in the raster."
        )
      }
      units <- paste(units, collapse = ", ")
    }
  } else {
    units <- paste(terra::units(raster), collapse = ", ")
  }

  # Attempt to write the raster to the database
  res <- writeRaster(
    con = con,
    raster = raster,
    rast_table = c("spatial", "rasters"),
    bit.depth = bit.depth,
    blocks = blocks,
    constraints = TRUE
  )

  if (res$status) {
    other_type_id <- DBI::dbGetQuery(
      con,
      "SELECT raster_type_id
       FROM spatial.raster_types
       WHERE raster_type_name = 'other';"
    )[1, 1]
    if (is.na(other_type_id)) {
      stop("Could not resolve raster_type_id for raster type 'other'.")
    }

    # band names
    bnds <- DBI::dbQuoteString(
      con,
      paste0("{{", paste(names(raster), collapse = "},{"), "}}")
    )
    new_id <- DBI::dbGetQuery(
      con,
      "INSERT INTO spatial.rasters_reference (raster_type_id, band_names, units, description, source) VALUES ($1, $2, $3, $4, $5) RETURNING reference_id;",
      params = list(
        other_type_id,
        bnds,
        units,
        description,
        if (is.null(source)) NA else source
      )
    )[1, 1]
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE rasters SET reference_id = ",
        new_id,
        " WHERE rid IN (",
        paste(res$appended_rids, collapse = ","),
        ");"
      )
    )

    return(new_id)
  } else {
    stop(paste0("Failed to write raster to database. Error: ", res$message))
  }
}
