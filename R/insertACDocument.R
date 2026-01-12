#' Add document to aquacache database
#'
#'@description
#'
#' This function facilitates the addition of one document at a time to the database in the 'documents' table. Each document must be linked to a specific location. Adding a document directly to the database is not possible, since the file must be converted to a binary object before loading. See [YGwater::getDocument()] to get a document out again.
#'
#' ## Locations, lines, and polygons
#' Any document can be associated with locations (points), lines, polygons, or any combination thereof. Please reference the table 'vectors' to give the correct geom_id(s) for your desired geoms.
#'
#' @param path Valid path including extension to the document to upload. Set to 'choose' to open a file dialog.
#' @param name A concise but descriptive name to give the document.
#' @param type Type of document, which must exist in the database already. Currently one of 'thesis', 'report', 'well log', 'conference paper', 'poster', 'journal article', 'map', 'graph', 'protocol', 'metadata', 'audit'.
#' @param description A text description of what the document is. Please be detailed!
#' @param tags Tags to associate with the document. Specify multiple tags as individual elements of a character vector, such as c("tag 1", "tag 2").
#' @param authors Document author(s) if known. Specify multiple authors as individual elements of a character vector, such as c("author 1", "author 2").
#' @param publish_date The date of publication, as a Date object.
#' @param url An optional url (could also be a DOI) for the document.
#' @param share_with User groups with which to share the document. Default 'public_reader' is the public group. See the table 'user_groups' for more information.
#' @param geoms The geom_id(s) with which to associate the document (must be in the database table 'vectors'). Leave NULL for a document with no spatial context.
#' @param con A connection to the database. Leave NULL to create a new connection using AquaConnect() and have it closed automatically.
#'
#' @return A list with success = TRUE, the new document_id, and any associated geoms.
#' @export

insertACDocument <- function(
  path,
  name,
  type,
  description,
  tags = NULL,
  authors = NULL,
  publish_date = NULL,
  url = NULL,
  share_with = "public_reader",
  geoms = NULL,
  con = NULL
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  DBI::dbExecute(con, "SET timezone = 'UTC'")

  #Checks
  if (length(path) > 1) {
    stop("You can only specify one path at a time.")
  }
  if (!inherits(description, "character")) {
    stop("Your description must be a character vector.")
  }
  if (length(description) > 1) {
    stop(
      "You can only enter the description as a character vector of length 1."
    )
  }
  if (!is.null(tags)) {
    if (!is.character(tags)) {
      stop("Tags must be a character vector.")
    }
  }
  if (nchar(description) < 5) {
    stop("Minimum character length for 'description' is 5. Try harder.")
  }
  type <- tolower(type)
  db_types <- DBI::dbGetQuery(
    con,
    "SELECT document_type_id, document_type_en FROM files.document_types;"
  )
  if (!(type %in% db_types$document_type_en)) {
    stop(
      "Your specified document type is not in the DB. This is what I see in there now:\n  ",
      paste(db_types$document_type_en, collapse = "\n  ")
    )
  }
  #Check that the name doesn't already exist
  name_check <- DBI::dbGetQuery(
    con,
    paste0("SELECT name FROM files.documents WHERE name = '", name, "';")
  )
  if (nrow(name_check) != 0) {
    stop("There is already a document with this name in the database.")
  }

  if (!is.null(geoms)) {
    #Check to make sure the geom_ids exist, report back to the user what actually got associated.
    exist_geoms <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT geom_id, geom_type, layer_name, feature_name, description FROM spatial.vectors WHERE geom_id IN (",
        paste(geoms, collapse = ", "),
        ")"
      )
    )
    if (nrow(exist_geoms) == 0) {
      stop(
        "None of the geom_ids you specified for parameter geoms can be found in the table vectors. Try again."
      )
    }
    if (nrow(exist_geoms != length(geoms))) {
      warning(
        "At least one of the geom_ids you specified for parameter geoms could not be found in table vectors."
      )
    }
  }
  if (!is.null(publish_date)) {
    if (!inherits(publish_date, "Date")) {
      stop("publish_date must be a Date object.")
    }
  }

  if (path == "choose") {
    path <- rstudioapi::selectFile(
      caption = "Select a document to upload",
      filter = list("All files" = c("*.*"))
    )
  } else if (!file.exists(path)) {
    stop(
      "The file you specified does not exist. Please check the path and try again."
    )
  }
  extension <- tools::file_ext(path)
  file <- readBin(path, what = "raw", n = file.info(path)$size)

  assigned_type <- db_types$document_type_id[db_types$document_type_en == type]
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO files.documents (name, type, description, format, document, share_with) VALUES ('",
      name,
      "', '",
      assigned_type,
      "', '",
      description,
      "', '",
      extension,
      "', '\\x",
      paste0(file, collapse = ""),
      "', ARRAY[",
      paste(sprintf("'%s'", share_with), collapse = ","),
      "]) ON CONFLICT (file_hash) DO NOTHING "
    )
  )

  # Get the document_id of the newly added (or existing) document
  id <- DBI::dbGetQuery(
    con,
    "SELECT MAX(document_id) FROM files.documents WHERE name = $1",
    params = list(name)
  )[1, 1]

  if (!is.null(authors)) {
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE files.documents SET authors = '{",
        paste(authors, collapse = ", "),
        "}' WHERE document_id = ",
        id,
        ";"
      )
    )
  }
  if (!is.null(url)) {
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE files.documents SET url = '",
        url,
        "' WHERE document_id = ",
        id,
        ";"
      )
    )
  }
  if (!is.null(publish_date)) {
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE files.documents SET publish_date = '",
        publish_date,
        "' WHERE document_id = ",
        id,
        ";"
      )
    )
  }
  if (!is.null(tags)) {
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE files.documents SET tags = '{",
        paste(tags, collapse = ", "),
        "}' WHERE document_id = ",
        id,
        ";"
      )
    )
  }

  if (!is.null(geoms)) {
    docs_spat <- data.frame("document_id" = id, "geom_id" = exist_geoms$geom_id)

    DBI::dbAppendTable(con, "documents_spatial", docs_spat)

    return(list(
      "success" = TRUE,
      "new_document_id" = id,
      "associated_geoms" = exist_geoms
    ))
  }
  return(list("success" = TRUE, "new_document_id" = id))
}
