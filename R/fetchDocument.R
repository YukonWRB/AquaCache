#' Extract document stored as BYTEA from postgreSQL DB
#'
#' Extracts a document/file stored in a postgreSQL database as BYTEA type. Should be flexible enough to work with most schemas, but was designed around the 'documents' table created with this package and with files uploaded to the database with [addHydrometDocument()].
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

fetchDocument <- function(id, id_col = "document_id", ext = "format", table = "documents", con = hydrometConnect(silent = TRUE), save_dir = NULL, save_name = NULL) {


  res <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", table, " WHERE document_id = '", id, "';"))
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
