#' Add a vector file to the database
#'
#'@description
#' `r lifecycle::badge("stable")`
#'
#' Use this function to add a vector file that is not a drainage basin to the database (see function [insertHydrometBasin()] for those, since they get their own reference table). Ensures that database constraints are met. If you need to replace or delete a vector for any reason you'll have to use SQL (perhaps via R using the DBI package) to delete it first or using [rpostgis::pgWriteGeom()] with the necessary parameters.
#'
#' ## Extracting from the database
#' Use function [rpostgis::pgGetGeom()] to retrieve the geometry from the database, specifying in the argument 'clauses' "WHERE geom_id = xx".
#'
#' ## Attribute tables:
#' The attribute table of the object will be discarded in order to work with the existing database column names and to enable many to many relationships. If you want vector files with multiple attributes with attribute tables please use another method, such as saving a .gpkg of the vector file and uploading it to the 'documents' table using [insertHydrometDocument()].
#'
#' @param geom The geometry object to add to the database, as a [terra::vect()] object. Conversion will automatically be made to epsg:4269, NAD83 lat/long decimal degrees. Can be points, lines, or polygons.
#' @param layer_name The name to give to the vector layer.
#' @param name A short but descriptive name to give to the geom attribute. Leave NULL if specified with parameter `name_col.` This parameter only works for geoms with a single attribute (row).
#' @param description Optional but highly recommended long-form description of the geometry attribute. Leave NULL is specifying a `description_col` instead.
#' @param name_col The name of the column containing names to give to the geom attributes. Each attribute (row) will be entered to the database using the string in this column. Leave NULL if specified with parameter `name`.
#' @param description_col The name of the column containing descriptions associated with the geometry attributes. Each attribute (row) will be entered to the database using the string in this column. Leave NULL if specified with parameter `description`.
#' @param table The referenced table in the database (as character string). If not under the public schema, use format c("schema", "table").
#' @param geom_col_name The name of the column in which to insert the geometry object.
#' @param overwrite If a row already exists for the combination of layer_name, name,  and geometry type (point, line, or polygon), should it be overwritten?
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return TRUE if the vector was added.
#'
#'
# geom <- terra::vect("G:\\water\\Common_GW_SW\\Data\\database\\polygons\\watersheds\\all_basins.shp")

insertHydrometVector <- function(geom, layer_name, name = NULL, description = NULL, name_col = NULL, description_col = NULL, table = "points_lines_polygons", geom_col_name = "geom", overwrite = FALSE, con = hydrometConnect()){

  on.exit(DBI::dbDisconnect(con))

  if (!inherits(geom, "SpatVector")){
    stop("This function requires a {terra} SpatVector object.")
  }

  if (is.null(name) & is.null(name_col)){
    stop("You need to specify either a name or name_col.")
  }

  if (!is.null(name_col) & !is.null(name)){
    stop("You specified a name as well as a name_col. Choose only one. 'name' only works for single row files, 'name_col' works for multiple rows.")
  }

  if (!is.null(name_col)){
    if (!(name_col %in% names(geom))){
      stop("You specified a non-existent column for the name_col.")
    }
  }
  if (!is.null(name) & nrow(geom) > 1){
    stop("You specified the parameter 'name' but this only works for vector files with one attribute. Please review the help file.")
  }

  if (!is.null(name_col)){
    for (i in 1:nrow(geom)){
      sub.geom <- geom[i, 0]
      sub.name <- geom[i, name_col]

    }
  }


  geom <- geom[1,0] #drop the attribute table
  geom$name<- name
  geom$description <- description
  #re-project if necessary
  if (!(terra::same.crs(geom, "epsg:4269"))){
    geom <- terra::project(geom, "epsg:4269")
  }

  # new_geomtype <- terra::geomtype(geom)
  # db_geomtype <- if (new_geomtype == "polygons")
  # # 'ST_Point'::text, 'ST_MultiPoint'::text, 'ST_LineString'::text, 'ST_MultiLineString'::text, 'ST_Polygon'::text, 'ST_MultiPolygon'::text]
  # DBI::dbGetQuery()
  # if (overwrite){
  #   DBI::db
  # } else {
  #
  # }

  if (!terra::is.valid(geom)){
    geom <- terra::makeValid(geom)
    message("geom object had invalid geometry, fix attempted using terra::makeValid().")
  }

  rpostgis::pgWriteGeom(con, name = table , data.obj = geom, geom = geom_col_name, partial.match = TRUE)


return(TRUE)
}
