#' Add a vector file to the database
#'
#'@description
#' `r lifecycle::badge("stable")`
#'
#' Use this function to add a vector file to the database. Ensures that database constraints are met and permits spatial queries. If you need to delete (not overwrite) a vector for any reason you'll have to use SQL (perhaps via R using the DBI package).
#'
#' ## Extracting from the database
#' Use function [fetchVector()] to retrieve a point, line, or polygon from the database.
#'
#' ## Attribute tables:
#' The attribute table of the object will be discarded except for the columns specified in parameters `feature_name_col` and `description_col` to work with the existing database column names and to enable many to many relationships within the database. If you want vector files with attribute tables please use another method, such as saving a .gpkg of the vector file and uploading it to the 'documents' table using [insertHydrometDocument()]. Note however that this precludes using the object's spatial attributes within the database!
#'
#' @param geom The geometry object to add to the database, as a [terra::vect()] object. Conversion will automatically be made to epsg:4269, NAD83 lat/long decimal degrees. Can be points, lines, or polygons with one or more features. Multi-feature geoms will be split up into individual database entries.
#' @param layer_name The name to give to the vector layer.
#' @param feature_name A short but descriptive name to give to the geom feature. Leave NULL if specified with parameter `feature_name_col` instead. This parameter only works for geoms with a single feature (row).
#' @param description Optional but highly recommended long-form description of the geometry feature. Leave NULL is specifying a `description_col` instead.
#' @param feature_name_col The name of the column with names to give to the geom features. Each feature (row in the attribute table) will be entered to the database using the string in this column. Leave NULL if specified with parameter `feature_name`.
#' @param description_col The name of the column containing descriptions associated with each feature. Each feature (row in the attribute table) will be entered to the database using the string in this column. Leave NULL if specified with parameter `description` instead.
#' @param table The target table in the database (as character string). If not under the public schema, use format c("schema", "table").
#' @param geom_col The name of the database table column in which to insert the geometry object.
#' @param overwrite If a row already exists for the combination of layer_name, name,  and geometry type (point, line, or polygon), should it be overwritten?
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return A boolean vector, one element per feature.Messages will also be printed to the console.
#'

insertHydrometVector <- function(geom, layer_name, feature_name = NULL, description = NULL, feature_name_col = NULL, description_col = NULL, table = "vectors", geom_col = "geom", overwrite = FALSE, con = hydrometConnect()){

  on.exit(DBI::dbDisconnect(con))

  if (!inherits(geom, "SpatVector")){
    stop("This function requires a {terra} SpatVector object.")
  }

  if (is.null(feature_name) & is.null(feature_name_col)){
    stop("You need to specify either a 'feature_name' or 'feature_name_col.'")
  }

  if (!is.null(feature_name_col) & !is.null(feature_name)){
    stop("You specified a 'feature_name' as well as a 'feature_name_col.' Choose only one. 'feature_name' only works for objects with one feature, 'feature_name_col' works for multiple features.")
  }

  if (!is.null(feature_name_col)){
    if (!(feature_name_col %in% names(geom))){
      stop("You specified a non-existent column for 'feature_name_col.'")
    }
  }
  if (!is.null(feature_name) & nrow(geom) > 1){
    stop("You specified the parameter 'feature_name' but this only works for vector files with one feature. Please review the help file.")
  }

  #re-project if necessary
  if (!(terra::same.crs(geom, "epsg:4269"))){
    geom <- terra::project(geom, "epsg:4269")
  }
  tbl <- as.data.frame(geom)

  success <- vector()
    for (i in 1:nrow(geom)){
      tryCatch({
        sub.geom <- geom[i, 0] #Drop attribute table for the feature
        sub.geom$layer_name <- layer_name
        if (nrow(geom) == 1 & !is.null(feature_name)){
          feat_name <- feature_name
          sub.geom$feature_name <- feat_name
        } else {
          feat_name <- tbl[i, feature_name_col]
          sub.geom$feature_name <- feat_name
        }
        if (!is.null(description)){
          desc <- description
          sub.geom$description <- desc
        } else if (!is.null(description_col)){
          desc <- tbl[i, description_col]
          sub.geom$description <- desc
        }

        if (!terra::is.valid(sub.geom)){
          sub.geom <- terra::makeValid(sub.geom)
          message("geom object had invalid geometry, fix attempted using terra::makeValid().")
        }

        new_geomtype <- terra::geomtype(sub.geom)
        db_geomtype <- if (new_geomtype == "polygons") 'ST_Polygon' else if (new_geomtype == "points") 'ST_Point' else if (new_geomtype == "lines")'ST_LineString'
        if (overwrite){
          exist <- DBI::dbGetQuery(con, paste0("SELECT layer_name, feature_name, geom_type, geom_id FROM vectors WHERE layer_name = '", layer_name, "' AND feature_name = '", feat_name, "' AND geom_type = '", db_geomtype, "';"))
          if (nrow(exist) == 1){
            message("Updating entry for geom_type = ", db_geomtype, ", layer_name = ", layer_name, ", feature_name = ", feat_name, ".")
            sub.geom$geom_id <- exist[1, "geom_id"]
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = table , data.obj = sub.geom, geom = geom_col, partial.match = TRUE, upsert.using = "geom_id"))
            DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
          } else if (nrow(exist) == 0){
            message("There is no existing database entry for this mix of geom_type = ", db_geomtype, ", layer_name = ", layer_name, ", feature_name = ", feat_name, ". Writing it without overwrite.")
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = table , data.obj = sub.geom, geom = geom_col, partial.match = TRUE))
            DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
          } else {
            warning("Failed to overwrite existing feature: there seems to be two entries in the database for this mix of geom_type = ", db_geomtype, ", layer_name = ", layer_name, ", feature_name = ", feat_name, ".")
            success[[i]] <- FALSE
          }
        } else { #overwrite if FALSE
          exist <- DBI::dbGetQuery(con, paste0("SELECT layer_name, feature_name, geom_type, geom_id FROM vectors WHERE layer_name = '", layer_name, "' AND feature_name = '", feat_name, "' AND geom_type = '", db_geomtype, "';"))
          if (nrow(exist) != 0){
            warning("Not writing geom_type = ", db_geomtype, ", layer_name = ", layer_name, ", feature_name = ", feat_name, ". There is already an entry matching this but parameter overwrite is FALSE.")
            success[[i]] <- FALSE
          } else {
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = table , data.obj = sub.geom, geom = geom_col, partial.match = TRUE))
            DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
          }
        }
       }, error = function(e){
         if (!success[[i]]){
           warning("Failed to write feature ", i, ": error occured when calling rpostgis::pgWriteGeom to write to the DB.")
         } else {
           warning("Unknown error occured while writing feature ", i, ".")
         }
      })
    }

return(success)
}
