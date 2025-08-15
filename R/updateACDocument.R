#' Update existing document in aquacache database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the updating of one document at a time to the database in the 'documents' table. Adding a document directly to the database is not possible, since the file must be converted to a binary object before loading. See [YGwater::getDocument()] to get a document out again.
#'
#' ## Locations, lines, and polygons
#' Any document can be associated with locations (points), lines, polygons, or any combination thereof. Please reference the table 'vectors' to give the correct geom_id(s) for your desired geoms.
#'
#' @param id The document_id of the document to update, from the document_id column of the documents table.
#' @param new_path Valid path including extension to the document to upload.
#' @param new_name A concise but descriptive name to give the document.
#' @param new_type Type of document, which must exist in the database already. Currently one of 'thesis', 'report', 'well log', 'conference paper', 'poster', 'journal article', 'map', 'graph', 'protocol', 'metadata', 'audit'.
#' @param new_description A text description of what the document is. Please be detailed!
#' @param new_authors Document author(s) if known. Specify multiple authors as individual elements of a character vector, such as c("author 1", "author 2").
#' @param new_publish_date The date of publication, as a Date object.
#' @param new_url An optional url (could also be a DOI) for the document.
#' @param new_user_groups New user groups to associate with the document. Leave NULL for no change.
#' @param new_geoms The geom_id(s) with which to associate the document (must be in the database table 'vectors'). Leave NULL for a document with no spatial context. Will overwrite any existing associations.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' @param check Logical, whether to double check that we're working with the right document before proceeding. Default is TRUE
#'
#' @return TRUE if a document was properly added to the database.
#' @export

updateHydrometDocument <- function(id, new_path = NULL, new_name = NULL, new_type = NULL, new_description = NULL, new_authors = NULL, new_publish_date = NULL, new_url = NULL, new_user_groups = NULL, new_geoms = NULL, con = AquaConnect(), check = TRUE) {
  
  #Checks
  id_exists <- DBI::dbGetQuery(con, paste0("SELECT d.name, t.document_type_en AS type, d.authors, d.url, d.publish_date, d.description FROM documents AS d LEFT JOIN document_types as t ON d.type = t.document_type_id WHERE document_id = ", id))
  if (nrow(id_exists) == 0) {
    stop("The document_id you specified does not exist in the database.")
  } else {
    if (check) {
      # Give a message to the user about what is already in the database and ask them to confirm they want to update it.
      message <- paste0("The document you are about to update has the following attributes:\n  Name: ", id_exists$name, "\n  Type: ", id_exists$type, "\n  Authors: ", id_exists$authors, "\n  URL: ", id_exists$url, "\n  Publish Date: ", id_exists$publish_date, "\n  Description: ", id_exists$description, "\n\n Do you wish to continue?  Enter Y or N.")
      confirm <- readline(prompt = writeLines(message))
      confirm <- tolower(confirm)
      
      if (!(confirm %in% c("y", "n"))) {
        while (!(confirm %in% c("y", "n"))) {
          confirm <- readline(prompt =
                                writeLines(paste("\nThat isn't an acceptable choice. Try again."
                                )))
          confirm <- tolower(confirm)
        }
      }
      if (confirm == "n") {
        stop("Aborting.")
      }
    }
  }
    
  if (!is.null(new_path)) {
    if (length(new_path) > 1) {
      stop("You can only specify one path at a time.")
    }
  }
  
  if (!is.null(new_description)) {
    if (!inherits(new_description, "character")) {
      stop("Your description must be a character vector.")
    }
    if (length(new_description) > 1) {
      stop("You can only enter the description as a character vector of length 1.")
    }
    if (nchar(new_description) < 5) {
      stop("Minimum character length for 'description' is 5. Try harder.")
    }
  }

  if (!is.null(new_type)) {
    new_type <- tolower(new_type)
    db_types <- DBI::dbGetQuery(con, "SELECT document_type_id, document_type_en FROM document_types;")
    if (!(new_type %in% db_types$document_type_en)) {
      stop("Your specified document type is not in the DB. This is what I see in there now:\n  ", paste(db_types$document_type_en, collapse = "\n  "))
    }
  }

  if (!is.null(new_name)) {
    #Check that the name doesn't already exist
    name_check <- DBI::dbGetQuery(con, paste0("SELECT name FROM documents WHERE name = '", new_name, "';"))
    if (nrow(name_check) != 0) {
      stop("There is already a document with this name in the database.")
    }
  }
  
  if (!is.null(new_geoms)) {
    #Check to make sure the geom_ids exist, report back to the user what actually got associated.
    exist_geoms <- DBI::dbGetQuery(con, paste0("SELECT geom_id, geom_type, layer_name, feature_name, description FROM vectors WHERE geom_id IN (", paste(new_geoms, collapse = ", "), ")"))
    if (nrow(exist_geoms) == 0) {
      stop("None of the geom_ids you specified for parameter geoms can be found in the table vectors. Try again.")
    }
    if (nrow(exist_geoms != length(new_geoms))) {
      warning("At least one of the geom_ids you specified for parameter geoms could not be found in table vectors.")
    }
  }
  
  if (!is.null(new_publish_date)) {
    if (!inherits(new_publish_date, "Date")) {
      stop("publish_date must be a Date object.")
    }
  }
  
  if (!is.null(new_path)) {
    extension <- tools::file_ext(new_path)
    file <- readBin(new_path, "raw", n = file.size(new_path))
    DBI::dbExecute(con, paste0("UPDATE documents SET document = '\\x", paste0(file, collapse = ""), "' WHERE document_id = ", id, ";"))
  }
  
  if (!is.null(new_name)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET name = '", new_name, "' WHERE document_id = ", id, ";"))
  }

  if (!is.null(new_type)) {
    assigned_type <- db_types$document_type_id[db_types$document_type_en == new_type]
    DBI::dbExecute(con, paste0("UPDATE documents SET type = ", assigned_type, " WHERE document_id = ", id, ";"))
  }

  if (!is.null(new_authors)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET authors = '{", paste(new_authors, collapse = ", "), "}' WHERE document_id = ", id, ";"))
  }
  if (!is.null(new_url)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET url = '", new_url, "' WHERE document_id = ", id, ";"))
  }
  if (!is.null(new_publish_date)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET publish_date = '", new_publish_date, "' WHERE document_id = ", id, ";"))
  }
  
  if (!is.null(new_user_groups)) {
    DBI::dbExecute(con, paste0("UPDATE documents SET user_groups = '{", paste(new_user_groups, collapse = ", "), "}' WHERE document_id = ", id, ";"))
  }
  
  if (!is.null(new_geoms)) {
    docs_spat <- data.frame("document_id" = id,
                            "geom_id" = exist_geoms$geom_id)
    DBI::dbExecute(con, "DELETE FROM documents_spatial WHERE document_id = ", id, ";")
    DBI::dbAppendTable(con, "documents_spatial", docs_spat)
  }
  message("Document updated successfully.")
}

