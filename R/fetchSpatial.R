#' Retrieve raster files from the database
#'
#' Placeholder function, not yet complete.
#'
#' @return An error message telling the user what to do instead.
#' @export
#'

fetchRaster <- function() {
  stop("This function isn't finished yet. Use rpostgis::pgGetRast() to extract a raster instead.")
}

#' Retrieve vector files from the database
#'
#' This function formulates an SQL query to retrieve points, lines, or polygons from the database and returns them as a terra object. At minimum, only one of `geom_id`, `layer_name`, `feature_name`, or `geom_type` are required, though if the combination you enter results in more than one geom_type you will get a descriptive error.
#'
#' @param geom_id A numeric vector of geom_ids from the 'vectors' table.
#' @param layer_name A character vector (one or more elements) specifying the target layer_name from the 'vectors' table.
#' @param feature_name A character vector (one or more elements) specifying the target feature_name from the 'vectors' table.
#' @param geom_type One of c('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon').
#' @param return_cols The names of columns to return.
#' @param table The target table in the database (as character string). If not under the public schema, use format c("schema", "table").
#' @param geom_col The name of the database table column in which to insert the geometry object.
#' @param con A connection to the target database.
#'
#' @return If successful, a terra object. If unsuccessful because the query targets more than 1 geometry types, a table showing you the result of the query.
#' @export
#'

fetchVector <- function(geom_id = NULL, layer_name = NULL, feature_name = NULL, geom_type = NULL, return_cols = c("geom_id", "geom_type", "layer_name", "feature_name", "description"), table = "vectors", geom_col = "geom", con = hydrometConnect()) {

  if (is.null(geom_id) & is.null(layer_name) & is.null(feature_name) & is.null(geom_type)) {
    stop("You need to specify at least one of the NULL parameters.")
  }
  if (!is.null(geom_type)){
    if (length(geom_type) > 1){
      stop("You can only select one geom_type at a time.")
    } else if (!(geom_type %in% c('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon'))) {
      stop("Parameter geom_type is not one of the possible choices. Refer to the help file.")
    }
  }


  #build the query
  query <- paste0("SELECT geom_id, layer_name, feature_name, geom_type FROM ", table, " WHERE")
  if (!is.null(geom_id)) {
    query <- paste0(query,  " geom_id IN (", paste(geom_id, collapse = ", "), ") AND")
  }
  if (!is.null(layer_name)) {
    query <- paste0(query,  " layer_name IN ('", paste(layer_name, collapse = "', '"), "') AND")
  }
  if (!is.null(feature_name)) {
    query <- paste0(query,  " feature_name IN ('", paste(feature_name, collapse = "', '"), "') AND")
  }
  if (!is.null(geom_type)) {
    query <- paste0(query,  " geom_type IN ('", paste(geom_type, collapse = "', '"), "') AND")
  }
  query <- gsub("\\s+AND$", "", query)
  tbl <- DBI::dbGetQuery(con, query)


  #Check if query resulted in multiple geom_types
  if (length(unique(tbl$geom_type)) > 1){
    return(tbl)
    stop("Your query resulted in more than one geometry type: ", paste(unique(tbl$geom_type), collapse = ", AND "), " were returned. Refer to the returned table and refine your search")
  }

  return <- rpostgis::pgGetGeom(con, query = paste0("SELECT  ", paste(return_cols, collapse = ", "), ", ", geom_col, " AS geom FROM ", table, " WHERE geom_id IN (", paste(tbl$geom_id, collapse = ", "), ");"))
  return <- terra::vect(return)

  return(return)
}
