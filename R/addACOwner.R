#' Add a new owner or contributor to the aquacache
#' 
#' @param name The name of the new owner/contributor or organization
#' @param name_fr The French name of the new owner/contributor or organization (optional).
#' @param contact_name The name of the contact person for the new owner/contributor or organization (optional).
#' @param phone The phone number of the contact person for the new owner/contributor or organization (optional). Specify as a character string or numeric.
#' @param email The email address of the contact person for the new owner/contributor or organization (optional).
#' @param note Any notes about the new owner/contributor or organization (optional).
#' 
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#'
#' @return The owner_contributor_id of the new user group, plus a new entry to the database.
#' @export


addACOwner <- function(name, name_fr = NA, contact_name = NA, phone = NA, email = NA, note = NA, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  # Check that name and any other optional parameters are character vectors of length 1
  if (!inherits(name, "character") | length(name) != 1) {
    stop("Parameter 'name' must be a character vector of length 1.")
  }
  if (!is.na(name_fr) & (!inherits(name_fr, "character") | length(name_fr) != 1)) {
    stop("Parameter 'name_fr' must be a character vector of length 1.")
  }
  if (!is.na(contact_name) & (!inherits(contact_name, "character") | length(contact_name) != 1)) {
    stop("Parameter 'contact_name' must be a character vector of length 1.")
  }
  if (!is.na(phone) & (!inherits(phone, "character") & !inherits(phone, "numeric") | length(phone) != 1)) {
    stop("Parameter 'phone' must be a character or numeric vector of length 1.")
  }
  if (!is.na(email) & (!inherits(email, "character") | length(email) != 1)) {
    stop("Parameter 'email' must be a character vector of length 1.")
  }
  if (!is.na(note) & (!inherits(note, "character") | length(note) != 1)) {
    stop("Parameter 'note' must be a character vector of length 1.")
  }
  
  # Check if the owner/contributor already exists (all lower case)
  exists <- DBI::dbGetQuery(con, paste0("SELECT owner_contributor_id FROM owners_contributors_operators WHERE LOWER(name) = '", tolower(name), "';"))[1,1]
  if (!is.na(exists)) {
    stop("There is already an entry with that name.")
  }
  
  # Insert the new owner/contributor group
  insert <- data.frame(name = name,
                       name_fr = name_fr,
                       contact_name = contact_name,
                       phone = phone,
                       email = email,
                       note = note)
  
  DBI::dbAppendTable(con, "owners_contributors_operators", insert)
  
  # Retrieve the new owner_contributor_id and return
  new_id <- DBI::dbGetQuery(con, paste0("SELECT owner_contributor_id FROM owners_contributors_operators WHERE name = '", name, "';"))[1,1]
  message("Added new owner/contributor with name ", name, ". New owner_contributor_id is: ", new_id)
}
