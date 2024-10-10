#' Check for patches to apply to the AquaCache database
#' 
#' @description
#' The functions in this package are dependent on a consistent database schema. However, sometimes changes need to be made to the database schema either for the sake of data organization in the DB or to facilitate or allow new functionality via this package. This function checks for patches that need to be applied to the database to ensure that the schema is up-to-date before running any functions that depend on it.
#' 
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()].
#' 
#' @return A message indicating whether or not patches need to be applied.
#' @export
#'

AquaPatchCheck <- function(con) {
  
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
    }
    
    message("There are patches available to apply to the database. Do you want to apply them now? We HIGHLY recomment doing so before running any functions from this package. \n 1 = apply patches now \n 2 = exit without applying patches")
    choice <- readline(prompt = "Enter 1 or 2: ")
    
    if (choice == 1) {
      # Apply patches in order
      tryCatch({
        for (i in (last_patch + 1):last_patch_file) {
          source(system.file("patches", paste0("patch_", i, ".R"), package = "AquaCache"))
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
  }  # Else do nothing
}
