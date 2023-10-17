#' Add a vector file to the database
#'
#' Use this function to add a vector file such as a drainage polygon, provincial buffer, waterbodies, etc to the database. Ensures that database constraints are met. If you need to replace or delete a vector for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first or using [rpostgis::pgWriteGeom()] with the necessary parameters.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param geom The geometry object to add to the database, as a [terra::vect()] object. Conversion will automatically be made to epsg:4269, NAD83 lat/long decimal degrees.
#' @param table The referenced table in the database (as character string), must be under the public schema.
#'
#' @return Nothing, but the vector should be added to the DB if no error is thrown.
#' @export
#'

add_vector <- function(con, table, geom){

  if (!inherits(geom, "SpatVector")){
    stop("This function requires a {terra} SpatVector object.")
  }

  #re-project if necessary
  if (!(terra::same.crs(geom, "epsg:4269"))){
    geom <- terra::project(geom, "epsg:4269")
  }

  # Check that the geom attribute table has the right column names and that they are not filled with NAs, then add to the DB
  if ("polygon_type" %in% names(geom) & "description" %in% names(geom) & "name" %in% names(geom)){
    if (all(geom$polygon_type %in% c('drainage_basin', 'buffer', 'waterbody', 'prov_terr', 'country', 'other'))) { #ensure DB constraints are met
      if (!any(is.na(geom$description))){
        if (!any(is.na(geom$name))){
          rpostgis::pgWriteGeom(con, name = table , data.obj = geom, geom = "geom")
        } else {
          stop("At least one of your values for the attribute/column 'name' is NA. Please add a descriptive name.")
        }
      } else {
        stop("At least one of your values for the attribute/colunmn 'description' is NA. Please add a valid description.")
      }
    } else {
      stop("At least one of your values for the attribute/column 'polygon_type' is invalid. Valid entries are in c('drainage_basin', 'buffer', 'waterbody', 'prov_terr', 'country', 'other').")
    }
  } else {
    stop("Your SpatVector object is lacking at least one of the mandatory fields 'polygon_type', 'name', or 'description'. Note that 'polygon_type' must be one of c('drainage_basin', 'buffer', 'waterbody', 'prov_terr', 'country', 'other').")
  }
}
