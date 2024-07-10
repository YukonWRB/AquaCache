#' Validate a user's credentials against those in the database.
#'
#' @param username The username for the user to validate.
#' @param password The password for the user to validate.
#' @param con A connection to the AquaCache database. Default uses [AquaConnect()].
#'
#' @return TRUE if the user's credentials are valid, FALSE otherwise.
#' @export
#'

validateACUser <- function(username, password, con = AquaConnect()) {
  # Retrieve the user's information from the database
  user_info <- DBI::dbGetQuery(con, "SELECT password_hash, password_salt, algorithm FROM users WHERE username = $1", params = list(username))
  
  if (nrow(user_info) == 0) {
    return(FALSE) # User not found
  }
  
  # Extract the stored hash, salt, and algorithm
  stored_hash <- user_info$password_hash
  salt <- user_info$password_salt
  algorithm <- user_info$algorithm
  
  # Hash the provided password using the same algorithm and salt
  password_hash <- digest::digest(paste0(password, salt), algo = algorithm, serialize = FALSE)
  
  # Compare the hashes
  return(password_hash == stored_hash)
}
