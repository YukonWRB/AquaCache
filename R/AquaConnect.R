#' Connect to the AquaCache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the hydrology database. A nearly identical function exists in package YGwater, but this one by default uses admin privileges while the other uses read-only privileges. Database superusers will have access to all database records, but other users will be asked to provide their username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#' 
#' See Details for more information for developers.
#' 
#' @details
#' - To facilitate development, a dev database can be connected to by setting the dev parameter to TRUE. This will append "_dev" to the database name.
#' - An attribute is added to the connection object to track if a transaction is active. This can be used by functions to determine if a transaction is already open, in which case functions can forgo opening a new transaction and instead use the existing one.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form AquaCacheHost:"hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form AquaCachePort:"1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form AquaCacheAdminUser:"username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form AquaCacheAdminPass:"password".
#' @param silent TRUE suppresses messages except for errors.
#' @param dev TRUE appends "_dev" to the database name.
#'
#' @return A connection to the database.
#'
#' @export

AquaConnect <- function(name = "AquaCache", host = Sys.getenv("AquaCacheHost"), port = Sys.getenv("AquaCachePort"), username = Sys.getenv("AquaCacheAdminUser"), password = Sys.getenv("AquaCacheAdminPass"), silent = FALSE, dev = FALSE){

  if (dev) {
    name <- paste0(name, "_dev")
  }
  tryCatch({
    con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)

    if (!DBI::dbGetQuery(con, "SELECT rolsuper FROM pg_roles WHERE rolname = current_user;")[1,1]) {
      if (interactive()) {
        # Prompt the user to enter their username and password
        message("You are not connecting to the database as a supersuer. Please enter your username and password to view records other than the 'public' ones, or enter nothing to log in as public.")
        username <- readline("Username: ")
        if (nchar(username) > 0) {
          password <- readline("Password: ")
          res <- validateACUser(username, password, con)
          if (res) {
            DBI::dbExecute(con, paste0("SET logged_in_user.username = '", username, "';"))
            message("You are now logged in as '", username, "'.")
          } else {
            DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
            message("Username or password failed. You are now logged in as 'public'.")
          }
        } else {
          DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
          message("You are now logged in as 'public'.")
        }
      } else {
        DBI::dbExecute(con, "SET logged_in_user.username = 'public';")
        message("You are now logged in as 'public'. If you need to change this either connect using an interactive session or use superuser credentials.")
      }
    }

    
    # Check for patches to apply ####################
    # See if table information.version_info exists, else set last_patch to 0
    if (!DBI::dbExistsTable(con, DBI::SQL('"information"."version_info"'))) {
      last_patch <- 0
    } else {
      # Get last patch applied from DB
      last_patch <- DBI::dbGetQuery(con, "SELECT version FROM information.version_info WHERE item  = 'Last patch number'")
      if (nrow(last_patch) == 0) {
        last_patch <- 0
      } else {
        last_patch <- as.numeric(last_patch$version)
      }
    }
    # Get last patch available from package. These are in inst/patches folder and named "patch_X.R"
    patch_files <- list.files(system.file("patches", package = "AquaCache"), pattern = "patch_[0-9]+\\.R", full.names = FALSE)
    last_patch_file <- max(as.numeric(gsub("patch_|.R", "", patch_files)))
    
    
    if (last_patch < last_patch_file) {
      
      # Check if the user has write permissions to the database tables
      privileges <- DBI::dbGetQuery(con, "SELECT table_schema, table_name, privilege_type FROM information_schema.role_table_grants WHERE grantee = current_user AND privilege_type IN ('INSERT', 'UPDATE', 'DELETE') AND table_schema NOT IN ('pg_catalog', 'information_schema');")
      if (nrow(privileges) == 0) {
        warning("You do not have write permissions to the database tables. Please contact your database administrator to apply patches. Queries may not work as expected until patches are applied.")
      } else {
        message("There are patches available to apply to the database. Do you want to apply them now? We HIGHLY recomment doing so before running any functions from this package. \n 1 = apply patches now \n 2 = exit without applying patches  \n")
        choice <- readline(prompt = "Enter 1 or 2: ")
        
        if (choice == 1) {
          
          message("It is HIGHLY recommended that your database is backed up. Take the time to do this now or make sure your automatic workflow actually did its job. \n Hit enter to continue.")
          readline(prompt = "")
          
          # Apply patches in order
          tryCatch({
            for (i in (last_patch + 1):last_patch_file) {
              source(system.file("patches", paste0("patch_", i, ".R"), package = "AquaCache"), local = TRUE, echo = TRUE)
            }
            message("Patches applied successfully.")
          }, error = function(e) {
            stop("Patches not applied. An error occurred in patch ", i, " : ", e$message)
          })
        } else if (choice == 2) {
          warning("Patches not applied. Please apply patches before running any functions from this package by running AquaPatchCheck().")
        } else {
          warning("Invalid choice. Patches not applied. Please apply patches before running any functions from this package by running AquaPatchCheck().")
        }
      }
    }  # Else do nothing
    
    # Add a new attribute to the connection object to track if a transaction is active
    attr(con, "active_transaction") <- FALSE
    
    return(con)
  }, error = function(e) {
    stop("Connection failed. Error message: ", e$message)
  })
}
