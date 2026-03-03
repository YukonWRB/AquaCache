#' Create raster pyramids for an existing raster reference
#'
#' @description
#' Creates down-sampled raster products directly in the database by resampling
#' existing raster tiles in the \code{spatial.rasters} table. Each pyramid level
#' is stored as a new entry in \code{spatial.rasters_reference} with its own
#' \code{reference_id} and updated cell-size metadata (in meters). Resampling
#' uses the native raster units, while the stored cell-size metadata is reported
#' in meters. The source raster data never leaves the database.
#'
#' @param con A connection to the database.
#' @param reference_id The \code{reference_id} in \code{spatial.rasters_reference} to use as the source raster.
#' @param factors A numeric or integer vector of integer multiples of the existing cell size to generate (e.g., \code{c(2, 4, 8)}).
#' @param method Optional resampling method passed through to PostGIS function ST_Resample. If NULL, the default PostGIS resampling behavior (nearest neighbor) is used. See [PostGIS documentation](https://postgis.net/docs/RT_ST_Resample.html) for details.
#'
#' @return A data.frame mapping each factor to the newly created
#'   \code{reference_id}.
#' @export

createRasterPyramids <- function(
  con,
  reference_id,
  factors = c(2, 4, 8),
  method = NULL
) {
  # validate factors - should be integers only and greater than 1
  if (
    !(is.numeric(factors) || is.integer(factors)) ||
      any(factors <= 1) ||
      any(factors != as.integer(factors))
  ) {
    stop("Parameter factors must be numeric values greater than 1.")
  }

  factors <- sort(unique(as.integer(factors)))

  meta <- DBI::dbGetQuery(
    con,
    "SELECT * FROM spatial.rasters_reference WHERE reference_id = $1",
    params = list(as.integer(reference_id))
  )

  if (nrow(meta) == 0) {
    stop("No rasters_reference entry found for the supplied reference_id.")
  }

  sizes_deg <- DBI::dbGetQuery(
    con,
    "SELECT 
    MIN(ABS(spatial.ST_PixelWidth(rast))) AS cell_size_x_deg,
    MIN(ABS(spatial.ST_PixelHeight(rast))) AS cell_size_y_deg
    FROM spatial.rasters 
    WHERE reference_id = $1",
    params = list(as.integer(reference_id))
  )

  sizes_m <- DBI::dbGetQuery(
    con,
    "SELECT 
    MIN(ABS(spatial.ST_PixelWidth(spatial.ST_Transform(rast, 3857)))) AS cell_size_x_m,
    MIN(ABS(spatial.ST_PixelHeight(spatial.ST_Transform(rast, 3857)))) AS cell_size_y_m
    FROM spatial.rasters 
    WHERE reference_id = $1",
    params = list(as.integer(reference_id))
  )

  if (is.na(meta$cell_size_x[1]) || is.na(meta$cell_size_y[1])) {
    meta$cell_size_x[1] <- sizes_m$cell_size_x_m[1]
    meta$cell_size_y[1] <- sizes_m$cell_size_y_m[1]
  }

  if (is.na(meta$cell_size_x[1]) || is.na(meta$cell_size_y[1])) {
    stop("Unable to determine cell size for the source raster.")
  }

  if (
    is.na(sizes_deg$cell_size_x_deg[1]) || is.na(sizes_deg$cell_size_y_deg[1])
  ) {
    stop("Unable to determine source raster cell size in degrees.")
  }

  results <- data.frame(
    factor = factors,
    reference_id = NA_integer_
  )

  for (i in seq_along(factors)) {
    factor <- factors[i]
    new_desc <- meta$description[1]
    if (!is.na(new_desc)) {
      new_desc <- paste0(new_desc, sprintf("%s", factor))
    }

    insert_reference <- paste0(
      "INSERT INTO spatial.rasters_reference 
        (raster_series_id, 
        raster_type_id, 
        model, 
        description, 
        band_names, 
        units,
        valid_from, 
        valid_to, 
        issued, 
        source, 
        flag, 
        parameter_id, 
        aggregation_type_id, 
        media_id, 
        z_value, 
        z_units, 
        cell_size_x, 
        cell_size_y) 
      SELECT raster_series_id, raster_type_id, model, ",
      if (is.na(new_desc)) "NULL" else DBI::dbQuoteString(con, new_desc),
      ", band_names, units, valid_from, valid_to, issued, source, flag, ",
      "parameter_id, aggregation_type_id, media_id, z_value, z_units, ",
      meta$cell_size_x[1] * factor,
      ", ",
      meta$cell_size_y[1] * factor,
      " FROM spatial.rasters_reference 
      WHERE reference_id = ",
      as.integer(reference_id),
      " RETURNING reference_id;"
    )

    new_id <- DBI::dbGetQuery(con, insert_reference)[1, 1]
    results$reference_id[i] <- new_id

    resample_expr <- paste0(
      "spatial.ST_Resample(rast, ",
      sizes_deg$cell_size_x_deg[1] * factor,
      ", ",
      sizes_deg$cell_size_y_deg[1] * factor,
      if (is.null(method)) {
        ""
      } else {
        paste0(", 0, 0, ", DBI::dbQuoteString(con, method))
      },
      ")"
    )

    insert_tiles <- paste0(
      "INSERT INTO spatial.rasters 
        (reference_id, r_class, r_proj4, rast) ",
      "SELECT ",
      new_id,
      ", r_class, r_proj4, ",
      resample_expr,
      " FROM spatial.rasters WHERE reference_id = ",
      as.integer(reference_id),
      ";"
    )

    DBI::dbExecute(con, insert_tiles)
  }

  results
}
