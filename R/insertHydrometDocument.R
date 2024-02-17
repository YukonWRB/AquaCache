#' Add document to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one document at a time to the database in the 'documents' table. Each document must be linked to a specific location. Adding a document directly to the database is not possible, since the file must be converted to a binary object before loading. See [fetchDocument()] to get a document out again.
#'
#' ## Locations, lines, and polygons
#' Any document can be associated with locations (points), lines, polygons, or any combination thereof. Please reference the table 'vectors' to give the correct geom_id(s) for your desired geoms.
#'
#' @param path Valid path including extension to the document to upload.
#' @param name A concise but descriptive name to give the document.
#' @param type Type of document. Must be one of 'thesis', 'report', 'well log', 'conference paper', 'poster', 'journal article', 'map', 'graph', 'protocol', 'grading scheme', 'metadata', 'other'.
#' @param description A text description of what the document is. Please be detailed!
#' @param authors Document author(s) if known. Specify multiple authors as individual elements of a character vector, such as c("author 1", "author 2").
#' @param publish_date The date of publication, as a Date object.
#' @param url An optional url (could also be a DOI) for the document.
#' @param geoms The geom_id(s) with which to associate the document (must be in the database table 'vectors'). Leave NULL for a document with no spatial context.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return TRUE if a document was properly added to the database.
#' @export

insertHydrometDocument <- function(path, name, type, description, authors = NULL, publish_date = NULL, url = NULL, geoms = NULL, con = hydrometConnect()) {

  #Checks
  if (length(path) > 1) {
    stop("You can only specify one path at a time.")
  }
  if (!inherits(description, "character")) {
    stop("Your description must be a character vector.")
  }
  if (length(description) > 1) {
    stop("You can only enter the description as a character vector of length 1.")
  }
  if (nchar(description) < 5) {
    stop("Minimum character length for 'description' is 5. Try harder.")
  }
  type <- tolower(type)
  if (!(type %in% c('thesis', 'report', 'well log', 'conference paper', 'poster', 'journal article', 'map', 'graph', 'protocol', 'grading scheme', 'metadata', 'other'))) {
    stop("Your specified document type is invalid. Refer to the help file.")
  }
  #Check that the name doesn't already exist
  name_check <- DBI::dbGetQuery(con, paste0("SELECT name FROM documents WHERE name = '", name, "';"))
  if (nrow(name_check) != 0) {
    stop("There is already a document with this name in the database.")
  }

  if (!is.null(geoms)) {
    #Check to make sure the geom_ids exist, report back to the user what actually got associated.
    exist_geoms <- DBI::dbGetQuery(con, paste0("SELECT geom_id, geom_type, layer_name, feature_name, description FROM vectors WHERE geom_id IN (", paste(geoms, collapse = ", "), ")"))
    if (nrow(exist_geoms) == 0) {
      stop("None of the geom_ids you specified for parameter geoms can be found in the table vectors. Try again.")
    }
    if (nrow(exist_geoms != length(geoms))) {
      warning("At least one of the geom_ids you specified for parameter geoms could not be found in table vectors.")
    }
  }
  if (!is.null(publish_date)) {
    if (!inherits(publish_date, "Date")) {
      stop("publish_date must be a Date object.")
    }
  }

  extension <- tools::file_ext(path)
  file <- hexView::readRaw(path)$fileRaw

  DBI::dbExecute(con, paste0("INSERT INTO documents (name, document_type, description, format, document) VALUES ('", name, "', '", type, "', '", description, "', '", extension, "', '\\x", paste0(file, collapse = ""), "');"))
  id <- DBI::dbGetQuery(con, paste0("SELECT document_id FROM documents WHERE name = '", name, "';"))

  if (!is.null(authors)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET authors = '{", paste(authors, collapse = ", "), "}' WHERE document_id = ", id, ";"))
  }
  if (!is.null(url)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET url = '", url, "' WHERE document_id = ", id, ";"))
  }
  if (!is.null(publish_date)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET publish_date = '", publish_date, "' WHERE document_id = ", id, ";"))
  }

  if (!is.null(geoms)) {
    docs_spat <- data.frame("document_id" = id,
                            "geom_id" = exist_geoms$geom_id)

    DBI::dbAppendTable(con, "documents_spatial", docs_spat)

    return(list("success" = TRUE, "new_document_id" =  id, "associated_geoms" = exist_geoms))
  }
  return(list("success" = TRUE, "new_document_id" =  id))
}

