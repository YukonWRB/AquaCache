#' Create a new user in the aquacache database
#' 
#' The aquacache database uses row-level security to restrict record access to users based on their group_id.
#' Users are stored in the 'users' table in the aquacache database. The password is hashed using the specified algorithm and a random salt.
#'
#' @param username The username for the new user, all lowercase.
#' @param password The password for the new user.
#' @param group 
#' @param con A connection to the aquacache database. You need to connect using a user that has the CREATEROLE privilege.
#'
#' @return The new user is added to the database.
#' @export
#'
addACUser <- function(username, password, group, con) {
  
  # Check that the connection is done by a user with CREATEROLE privilege
  

}

