#' Add a new user group to the aquacache
#' 
#' @param name The name of the new user group
#' @param description A description of the new user group
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. NULL will create a connection and close it afterwards, otherwise it's up to you to close it after.
#'
#' @return The group_id of the new user group, plus a new entry to the database.
#' @export
#'

addACUserGroup <- function(name, description, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  # Check that name and description are char vectors of length 1
  if (!is.character(name) | length(name) != 1) {
    stop("Parameter name must be a character vector of length 1.")
  }
  if (!is.character(description) | length(description) != 1) {
    stop("Parameter description must be a character vector of length 1.")
  }
  
  # Check if the user group already exists
  exists <- DBI::dbGetQuery(con, paste0("SELECT group_id FROM user_groups WHERE group_name = '", name, "';"))[1,1]
  if (!is.na(exists)) {
    stop("There is already a user group with that name.")
  }
  
  # Insert the new user group
  insert <- data.frame(group_name = name,
                       group_description = description)
  
  DBI::dbAppendTable(con, "user_groups", insert)
  
  # Retrieve the new group_id and return
  new_id <- DBI::dbGetQuery(con, paste0("SELECT group_id FROM user_groups WHERE group_name = '", name, "';"))[1,1]
  message("Added new user group with name ", name, ". New group_id is: ", new_id)
}
