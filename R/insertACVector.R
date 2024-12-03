#' Add a vector file to the database
#'
#'@description
#' `r lifecycle::badge("stable")`
#'
#' Use this function to add a vector file to the database. Ensures that database constraints are met and permits spatial queries. If you need to delete (not overwrite) a vector for any reason you'll have to use SQL (perhaps via R using the DBI package).
#'
#' ## Extracting from the database
#' Use function [YGwater::getVector()] to retrieve a point, line, or polygon from the database.
#'
#' ## Attribute tables:
#' The attribute table of the object will be discarded except for the columns specified in parameters `feature_name_col` and `description_col` to work with the existing database column names and to enable many to many relationships within the database. If you want vector files with attribute tables please use another method, such as saving a .gpkg of the vector file and uploading it to the 'documents' table using [insertACDocument()]. Note however that this precludes using the object's spatial attributes within the database!
#'
#' @param geom The geometry object to add to the database, as a [terra::vect()] object or as a file path to a shapefile, geopackage or something else that terra::vect() van use. Conversion will automatically be made to epsg:4269, NAD83 lat/long decimal degrees. Can be points, lines, or polygons with one or more features. Multi-feature geoms will be split up into individual database entries.
#' @param layer_name The name to give to the vector layer, which defines which layer_name it gets assigned to in the database. This should always be an existing layer_name unless you have a good reason to create a new one.
#' @param feature_name A short but descriptive name to give to the geom feature. Leave NULL if specified with parameter `feature_name_col` instead. This parameter only works for geoms with a single feature (row).
#' @param description Optional but highly recommended long-form description of the geometry feature. Leave NULL is specifying a `description_col` instead.
#' @param feature_name_col The name of the column with names to give to the geom features. Each feature (row in the attribute table) will be entered to the database using the string in this column. Leave NULL if specified with parameter `feature_name`.
#' @param description_col The name of the column containing descriptions associated with each feature. Each feature (row in the attribute table) will be entered to the database using the string in this column. Leave NULL if specified with parameter `description` instead.
#' @param table The target table in the database (as character string). See 'schema' if not under the 'spatial' schema.
#' @param schema The schema in which the target 'table' is located. Default is 'spatial'. Note that this is NOT the default for [rpostgis::pgWriteGeom()].
#' @param geom_col The name of the database table column in which to insert the geometry object.
#' @param overwrite If a row already exists for the combination of layer_name, name,  and geometry type (point, line, or polygon), should it be overwritten?
#' @param con A connection to the database. Default NULL will use the utility function [AquaConnect()] and disconnect afterwards.
#'
#' @return A boolean vector, one element per feature.Messages will also be printed to the console.
#' @export

insertACVector <- function(geom, layer_name, feature_name = NULL, description = NULL, feature_name_col = NULL, description_col = NULL, table = "vectors", schema = "spatial", geom_col = "geom", overwrite = FALSE, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  if (inherits(con, "Pool")) {
    con <- pool::localCheckout(con)  # Automatically returned when the function exits
  }
  
  exist_layer_names <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT layer_name FROM ", schema, ".", table, ";"))
  
  if (!layer_name %in% exist_layer_names$layer_name) {
    message("The layer_name you specified does not exist yet. Are you sure you want to create it? The current entries are:\n", paste(exist_layer_names$layer_name, collapse = "\n"))
    commit <- readline(prompt = writeLines(paste("\n1: Definitely not",
                                                 "\n2: Maybe?",
                                                 "\n3: Yes, I want to create it."
    )))
    commit <- as.numeric(commit)
    if (commit != 3) {
      stop("Allright, come back when you're ready.")
    }
  }

  if (inherits(geom, "character")) {
    geom <- terra::vect(geom)
  } else if (!inherits(geom, "SpatVector")) {
    stop("The 'geom' parameter must be a file path to a vector file or a terra::vect() object.")
  }

  if (is.null(feature_name) & is.null(feature_name_col)) {
    stop("You need to specify either a 'feature_name' or 'feature_name_col.'")
  }

  if (!is.null(feature_name_col) & !is.null(feature_name)) {
    stop("You specified a 'feature_name' as well as a 'feature_name_col.' Choose only one. 'feature_name' only works for objects with one feature, 'feature_name_col' works for multiple features.")
  }

  if (!is.null(feature_name_col)) {
    if (!(feature_name_col %in% names(geom))) {
      stop("You specified a non-existent column for 'feature_name_col.'")
    }
    # Aggregate on the feature_name_col
    geom <- terra::aggregate(geom, by = feature_name_col)
  }
  if (!is.null(feature_name) & nrow(geom) > 1) {
    stop("You specified the parameter 'feature_name' but this only works for vector files with one feature. Please review the help file.")
  }

  #re-project if necessary
  if (!(terra::same.crs(geom, "epsg:4269"))) {
    geom <- terra::project(geom, "epsg:4269")
  }
  tbl <- as.data.frame(geom)

  success <- vector()
    for (i in 1:nrow(geom)) {
      tryCatch({
        sub.geom <- geom[i, 0] #Drop attribute table for the feature
        sub.geom$layer_name <- layer_name
        if (nrow(geom) == 1 & !is.null(feature_name)) {
          feat_name <- feature_name
          sub.geom$feature_name <- feat_name
        } else {
          feat_name <- tbl[i, feature_name_col]
          sub.geom$feature_name <- feat_name
        }
        if (!is.null(description)) {
          desc <- description
          sub.geom$description <- desc
        } else if (!is.null(description_col)) {
          desc <- tbl[i, description_col]
          sub.geom$description <- desc
        }

        if (!terra::is.valid(sub.geom)) {
          sub.geom <- terra::makeValid(sub.geom)
          message("geom object had invalid geometry, fix attempted using terra::makeValid().")
        }

        if (overwrite) {
          exist <- DBI::dbGetQuery(con, paste0("SELECT layer_name, feature_name, geom_type, geom_id FROM ", schema, ".", table, " WHERE layer_name = '", layer_name, "' AND feature_name = '", feat_name, "';"))
          if (nrow(exist) == 1) {
            message("Updating entry for layer_name = ", layer_name, ", feature_name = ", feat_name, ".")
            sub.geom$geom_id <- exist[1, "geom_id"]
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = c(schema, table) , data.obj = sub.geom, geom = geom_col, partial.match = TRUE, upsert.using = "geom_id"))
            DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
          } else if (nrow(exist) == 0) {
            message("There is no existing database entry for this mix of layer_name = ", layer_name, ", feature_name = ", feat_name, ". Writing it without overwrite.")
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = c(schema, table) , data.obj = sub.geom, geom = geom_col, partial.match = TRUE))
            DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
          }
        } else { #overwrite if FALSE
          exist <- DBI::dbGetQuery(con, paste0("SELECT layer_name, feature_name, geom_type, geom_id FROM ,", schema, ".", table, ", WHERE layer_name = '", layer_name, "' AND feature_name = '", feat_name, "';"))
          if (nrow(exist) != 0) {
            message("There is already an entry for layer_name = ", layer_name, " and feature_name = ", feat_name, " but you didn't ask to overwrite it. Would you like to aggregate the database feature with the new one?")
            agg <- readline(prompt = writeLines(paste("\n1: Yes",
                                                      "\n2: No way!"
            )))
            agg <- as.numeric(agg)
            if (agg != 1) {
              warning("Not writing layer_name = ", layer_name, ", feature_name = ", feat_name, ". There is already an entry matching this but parameter overwrite is FALSE.")
              success[[i]] <- FALSE
            } else {
              message("Seeing if I can aggregate the layers together and update the existing vector entry...")
              sub.geom <- terra::aggregate(rbind(exist, sub.geom), by = "feature_name")
              sub.geom$geom_id <- exist[1, "geom_id"]
              success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = c(schema, table) , data.obj = sub.geom, geom = geom_col, partial.match = TRUE, upsert.using = "geom_id"))
              DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
              message("Succeeded in adding to the existing vector!")
            }
          } else {
            success[[i]] <- suppressMessages(rpostgis::pgWriteGeom(con, name = c(schema, table) , data.obj = sub.geom, geom = geom_col, partial.match = TRUE))
            DBI::dbExecute(con, paste0("UPDATE internal_status SET value = '", .POSIXct(Sys.time(), "UTC"), "' WHERE event = 'last_new_vectors'"))
          }
        }
       }, error = function(e) {
         if (!success[[i]]) {
           warning("Failed to write feature ", i, ": error occured when calling rpostgis::pgWriteGeom to write to the DB.")
         } else {
           warning("Unknown error occured while writing feature ", i, ".")
         }
      })
    }

return(success)
}
