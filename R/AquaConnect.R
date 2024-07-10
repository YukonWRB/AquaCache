#' Connect to the hydromet (dev) database
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by function [AquaConnect()]. Still exported by this package as old database 'hydromet' is now a development environment.
#'
#' This function exists to facilitate connecting to the hydrology database. A nearly identical function exists in package YGwater, but this one by default uses admin privileges while the other uses read-only privileges. Database superusers will have access to all database records, but other users will be asked to provide their username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form hydrometHost:"hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form hydrometPort:"1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form hydrometAdminUser:"username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form hydrometAdminPass:"password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

hydrometConnect <- function(name = "hydromet", host = Sys.getenv("hydrometHost"), port = Sys.getenv("hydrometPort"), username = Sys.getenv("hydrometAdminUser"), password = Sys.getenv("hydrometAdminPass"), silent = FALSE){
  
  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    
    if (!DBI::dbGetQuery(hydro, "SELECT rolsuper FROM pg_roles WHERE rolname = current_user;")[1,1]) {
      if (interactive()) {
        # Prompt the user to enter their username and password
        message("You are not connecting to the database as a supersuer. Please enter your username and password to view records other than the 'public' ones, or enter nothing to log in as public.")
        username <- readline("Username: ")
        if (nchar(username) > 0) {
          password <- readline("Password: ")
          res <- validateACUser(username, password, hydro)
          if (res) {
            DBI::dbExecute(hydro, paste0("SET logged_in_user.username = '", username, "';"))
            message("You are now logged in as '", username, "'.")
          } else {
            DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
            message("Username or password failed. You are now logged in as 'public'.")
          }
        } else {
          DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
          message("You are now logged in as 'public'.")
        }
      } else {
        DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
        message("You are now logged in as 'public'. If you need to change this either connect using an interactive session or use superuser credentials.")
      }
    } 
    
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    
    return(hydro)
  }, error = function(e) {
    stop("Connection failed.")
  })
}




#' Connect to the AquaCache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the hydrology database. A nearly identical function exists in package YGwater, but this one by default uses admin privileges while the other uses read-only privileges. Database superusers will have access to all database records, but other users will be asked to provide their username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form AquaCacheHost:"hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form AquaCachePort:"1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form AquaCacheAdminUser:"username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form AquaCacheAdminPass:"password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

AquaConnect <- function(name = "AquaCache", host = Sys.getenv("AquaCacheHost"), port = Sys.getenv("AquaCachePort"), username = Sys.getenv("AquaCacheAdminUser"), password = Sys.getenv("AquaCacheAdminPass"), silent = FALSE){

  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)

    if (!DBI::dbGetQuery(hydro, "SELECT rolsuper FROM pg_roles WHERE rolname = current_user;")[1,1]) {
      if (interactive()) {
        # Prompt the user to enter their username and password
        message("You are not connecting to the database as a supersuer. Please enter your username and password to view records other than the 'public' ones, or enter nothing to log in as public.")
        username <- readline("Username: ")
        if (nchar(username) > 0) {
          password <- readline("Password: ")
          res <- validateACUser(username, password, hydro)
          if (res) {
            DBI::dbExecute(hydro, paste0("SET logged_in_user.username = '", username, "';"))
            message("You are now logged in as '", username, "'.")
          } else {
            DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
            message("Username or password failed. You are now logged in as 'public'.")
          }
        } else {
          DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
          message("You are now logged in as 'public'.")
        }
      } else {
        DBI::dbExecute(hydro, "SET logged_in_user.username = 'public';")
        message("You are now logged in as 'public'. If you need to change this either connect using an interactive session or use superuser credentials.")
      }
    } 
    
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    
    return(hydro)
  }, error = function(e) {
    stop("Connection failed.")
  })
}
