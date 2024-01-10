#' Add document to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one document at a time to the database in the 'documents' table. Each document must be linked to a specific location. Adding a document directly to the database is not possible, since the file must be converted to a binary object before loading. See [fetchDocument()] to get a document out again.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param path Valid path including extension to the document to upload.
#' @param location The location with which to associate the document (must be in the database).
#' @param description A text description of what the document is. Please be descriptive!
#' @param con A connection to the database.
#'
#' @return TRUE if a document was properly added to the database.
#' @export

addHydrometDocument <- function(path, location, description, con = hydrometConnect(silent=TRUE)){

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

