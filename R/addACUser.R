#' Create a new user in the AquaCache database
#' 
#' The AquaCache database uses row-level security to restrict record access to users based on their group_id.
#' Users are stored in the 'users' table in the AquaCache database. The password is hashed using the specified algorithm and a random salt.
#'
#' @param username The username for the new user.
#' @param password The password for the new user.
#' @param email The email address for the new user.
#' @param group_ids The group_ids for the new user. Default 1 is the 'public' group. To add a group, use [addACUserGroup()].
#' @param con A connection to the AquaCache database. Default uses [AquaConnect()].
#' @param algorithm The hashing algorithm to use. Default is "sha256". Refer to [digest::digest()] for available algorithms.
#'
#' @return The new user is added to the database.
#' @export
#'
addACUser <- function(username, password, email, group_ids = c(1), con = AquaConnect(silent = TRUE), algorithm = "sha256") {
  
  # Confirm that group_ids is a numeric vector and that all values are in the user_groups table
  if (!is.numeric(group_ids)) {
    stop("Parameter 'group_ids' must be a numeric vector.")
  }
  if (length(group_ids) == 0) {
    stop("Parameter 'group_ids' must contain at least one group_id.")
  }
  
  # Check that the group_ids exist in the user_groups table
  group_check <- DBI::dbGetQuery(con, "SELECT group_id FROM user_groups;")
  if (any(!group_ids %in% group_check$group_id)) {
    stop("One or more group_ids do not exist in the user_groups table.")
  }
  
  tryCatch({
    # Generate a random salt
    salt <- paste0(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
    
    # Create the password hash using the salt
    password_hash <- digest::digest(paste0(password, salt), algo = algorithm, serialize = FALSE)
    
    # Insert the new user into the database
    # DBI::dbExecute(con, "INSERT INTO users (username, user_groups, password_hash, password_salt, email, algorithm) VALUES ($1, $2, $3, $4, $5, $6)",
                   # params = list(username, group_ids, password_hash, salt, email, algorithm))
    DBI::dbExecute(con, paste0("INSERT INTO users (username, user_groups, password_hash, password_salt, email, algorithm) VALUES ('", username, "', array[", group_ids, "], '", password_hash, "', '", salt, "', '", email, "', '", algorithm, "');"))
    
    message("User '", username, "' created successfully.")
  }, error = function(e) {
    stop("Error creating user: ", e$message)
  })

}

