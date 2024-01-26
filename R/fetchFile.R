#' Extract image from postgreSQL DB
#'
#' @description
#' Extracts an image stored in the hydromet database as BYTEA type. For extracting documents see [fetchDocument()], and for extracting rasters to R (which automatically sets the CRS) see [rpostgis::pgGetRast()]. Calls function internal package function fetchFile.
#'
#' @param id The ID number from column 'image_id' of table 'images'.
#' @param con A connection to the database.
#' @param save_dir A directory in which to write the file.
#' @param save_name The name to give the file, *without* extension.
#'
#' @return A data.frame containing the row(s) identified by the specified ID. One column will contain the binary object. If asked for, the document only will be saved to file.
#' @export
#'

fetchImage <- function(id, con = hydrometConnect(silent = TRUE), save_dir = NULL, save_name = NULL) {
  fetchFile(id = id, id_col = "image_id", ext = "format", table = "images", con = con, save_dir = save_dir, save_name = save_name)
}

#' Extract document from postgreSQL DB
#'
#' @description
#' Extracts a document stored in the hydromet database as BYTEA type. For extracting images see [fetchImage()], and for extracting rasters to R (which automatically sets the CRS) see [rpostgis::pgGetRast()]. Calls function internal package function fetchFile.
#'
#'
#' @param id The ID number from column 'document_id' of table 'documents'.
#' @param con A connection to the database.
#' @param save_dir A directory in which to write the file.
#' @param save_name The name to give the file, *without* extension.
#'
#' @return A data.frame containing the row(s) identified by the specified ID. One column will contain the binary object. If asked for, the document only will be saved to file.
#' @export
#'

fetchDocument <- function(id, con = hydrometConnect(silent = TRUE), save_dir = NULL, save_name = NULL) {
  fetchFile(id = id, id_col = "document_id", ext = "format", table = "documents", con = con, save_dir = save_dir, save_name = save_name)
}


#' Extract document stored as BYTEA from postgreSQL DB
#'
#'@description
#' *NOTE* Unless you need flexibility, use functions [fetchDocument()] or [fetchImage()].
#'
#' Extracts a document/file stored in a postgreSQL database as BYTEA type. Should be flexible enough to work with most schemas, but was designed around the 'documents' and 'images' table created with this package and with files uploaded to the database with [insertHydrometDocument()] or [insertHydrometImage()]. For extracting rasters to R (which automatically sets the CRS) see [rpostgis::pgGetRast()].
#'
#' @param id The unique ID from the column specified in `id_col` with which to identify the individual record containing the binary object.
#' @param id_col The column in which to look for the `id`
#' @param ext The column in which to look for the file extension, or a file extension (must be preceeded by a period). Only used if save_path is not NULL
#' @param table The table to look in for the document.
#' @param con A connection to the database.
#' @param save_dir A directory in which to write the file.
#' @param save_name The name to give the file, *without* extension (specify the extension in `ext`).
#'
#' @return A data.frame containing the row(s) identified by the specified ID.
#' @export

fetchFile <- function(id, id_col, ext, table, con = hydrometConnect(silent = TRUE), save_dir = NULL, save_name = NULL) {

  on.exit(DBI::dbDisconnect(con))

  res <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table, " WHERE ", id_col, " = '", id, "';"))
  if (nrow(res) > 1){
    warning("The id you specified returned more than one record. If you specified a save directory and name it will be ignored. You can save the results yourself using function writeBin().")
    save_dir <- NULL
  }

  if (!is.null(save_dir)){
    if (!dir.exists(save_dir)){
      stop("The directory you pointed to does not exist.")
    }
    save_dir <- sub("/$", "", save_dir)
    if (!is.null(save_name)){
      if (startsWith(ext, ".")) {
        name <- paste0(save_dir, "/", save_name, ext)
      } else {
        format <- res[[ext]]
        if (length(format) != 1){
          stop("Attempting to get the file extension from the database did not yield anything. Perhaps you are specifying the wrong column name?")
        }
        name <- paste0(save_dir, "/", save_name, ".", format)
      }
      #find blob data type column
      vect <- logical(0)
      for (i in 1:ncol(res)){
        if ("blob" %in% class(res[[i]])) {
          vect <- c(vect, TRUE)
        } else {
          vect <- c(vect, FALSE)
        }
      }
      if (sum(vect) > 1){
        stop("There is more than one column of type 'blob' in the output. Returning the data.frame without writing to disk.")
        return(res)
      } else if (sum(vect) < 1){
        stop("There is no column of type 'blob' in the output. Returning the data.frame without writing to disk.")
        return(res)
      }
      document <- res[[which(vect)]][[1]]
      writeBin(document, name)
    }
  }

  return(res)
}
