#' Add document to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one document at a time to the database in the 'documents' table. Each document must be linked to a specific location. Adding a document directly to the database is not possible, since the file must be converted to a binary object before loading. See [fetchDocument()] to get a document out again.
#'
#' ## Locations, lines, and polygons
#' Any document can be associated with locations (points), lines, polygons, or any combination thereof. For example, to associate a document with location '09EA004', line 'Klondike River Near Dempster Bridge', and polygon '09EA004', you would specify locations = '09EA004', lines = 'Klondike River Near Dempster Bridge', polygons = '09EA004'.
#'
#' @param path Valid path including extension to the document to upload.
#' @param locations The location(s) with which to associate the document (must be in the database). Leave NULL if associating instead with lines and/or polygons.
#' @param lines The line(s) with which to associate the document (must be in the database). Leave NULL if associating instead with locations and/or polygons.
#' @param polygons The polygons with which to associate the document (must be in the database). Leave NULL if associating instead with lines and/or locations.
#' @param description A text description of what the document is. Please be descriptive!
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return TRUE if a document was properly added to the database.
#' @export

insertHydrometDocument <- function(path, locations = NULL, lines = NULL, polygons = NULL, description, con = hydrometConnect()){

  #Checks
  if (length(path) > 1){
    stop("You can only specify one path at a time.")
  }
  if (!inherits(location, "character")){
    stop("The location code should be a character vector of 1")
  }
  if (length(location) > 1){
    stop("You can only specify one location at a time.")
  }
  loc_exists <- DBI::dbGetQuery(con, paste0("SELECT location FROM locations WHERE location = '", location, "';"))
  if (nrow(loc_exists) == 0){
    stop("The location you specified does not exist in the database. Check your spelling.")
  }
  if (!inherits(description, "character")){
    stop("Your description must be a character vector.")
  }
  if (length(description) > 1){
    stop("You can only enter the description as a character vector of length 1.")
  }
  if (nchar(description) < 5) {
    stop("Minimum character length for 'description' is 5. Try harder.")
  }

  extension <- tools::file_ext(path)
  file <- hexView::readRaw(path)$fileRaw

  DBI::dbExecute(con, paste0("INSERT INTO documents (location, description, format, document) VALUES ('", location, "', '", description, "', '", extension, "', '\\x", paste0(file, collapse = ""), "');"))

  return(TRUE)
}

