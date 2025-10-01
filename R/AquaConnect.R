#' Connect to the aquacache database
#'
#' @description
#'
#' This function exists to facilitate connecting to the hydrology database. A nearly identical function exists in package YGwater, but this one by default uses admin privileges while the other uses read-only privileges. Database superusers will have access to all database records, but other users will be asked to provide their username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#'
#' See Details for more information for developers.
#'
#' @details
#' - To facilitate development, a dev database can be connected to by setting the dev parameter to TRUE. This will append "_dev" to the database name.
#' - An attribute is added to the connection object to track if a transaction is active. This can be used by functions to determine if a transaction is already open, in which case functions can forgo opening a new transaction and instead use the existing one.
#'
#' @param name Database name. By default searches the .Renviron file for parameter=value pair of form aquacacheName="name". If you want to connect to a different database, pass the name of that database here.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost:"hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form aquacachePort:"1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminUser:"username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminPass:"password".
#' @param silent TRUE suppresses messages except for errors and login messages.
#'
#' @return A connection to the database.
#'
#' @export

AquaConnect <- function(
  name = Sys.getenv("aquacacheName"),
  host = Sys.getenv("aquacacheHost"),
  port = Sys.getenv("aquacachePort"),
  username = Sys.getenv("aquacacheAdminUser"),
  password = Sys.getenv("aquacacheAdminPass"),
  silent = FALSE
) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        drv = RPostgres::Postgres(),
        dbname = name,
        host = host,
        port = port,
        user = username,
        password = password
      )

      # Explicitly set the timezone to UTC as all functions in this package work with UTC timezones
      DBI::dbExecute(con, "SET timezone = 'UTC'")

      user <- DBI::dbGetQuery(con, "SELECT current_user;")
      if (!user[1, 1] %in% c("postgres", "admin")) {
        message(
          "You are not connecting to the database as a supersuer or admin. Many functions in this package require at least the 'admin' privileges."
        )
      }
    },
    error = function(e) {
      stop("Connection failed. Error message: ", e$message)
    }
  )

  # Check for patches to apply ####################
  if (user[1, 1] %in% c("postgres", "admin")) {
    # See if table information.version_info exists, else set last_patch to 0
    if (!DBI::dbExistsTable(con, DBI::SQL('"information"."version_info"'))) {
      last_patch <- 0
    } else {
      # Get last patch applied from DB
      last_patch <- DBI::dbGetQuery(
        con,
        "SELECT version FROM information.version_info WHERE item  = 'Last patch number'"
      )
      if (nrow(last_patch) == 0) {
        last_patch <- 0
      } else {
        last_patch <- as.numeric(last_patch$version)
      }
    }
    # Get last patch available from package. These are in inst/patches folder and named "patch_X.R"
    patch_files <- list.files(
      system.file("patches", package = "AquaCache"),
      pattern = "^patch_[0-9]+\\.R",
      full.names = FALSE
    )
    last_patch_file <- max(as.numeric(gsub("patch_|.R", "", patch_files)))

    if (last_patch < last_patch_file) {
      # Check if the user is 'postgres' or 'admin', either of which should work for most patches
      user <- DBI::dbGetQuery(con, "SELECT current_user;")
      if (!user[1, 1] %in% c("postgres", "admin")) {
        warning(
          "You are not connecting as 'admin' or 'postgres' user. Please contact your database administrator to apply patches. Queries may not work as expected until patches are applied."
        )
      } else {
        message(
          "There are patches available to apply to the database. Do you want to apply them now? We HIGHLY recomment doing so before running any functions from this package. \n 1 = apply patches now \n 2 = continue without applying patches  \n"
        )
        choice <- readline(prompt = "Enter 1 or 2: ")

        if (choice == 1) {
          message(
            "It is HIGHLY recommended that your database is backed up. Take the time to do this now or make sure your automatic workflow actually did its job. \n Hit enter to continue."
          )
          readline(prompt = "")

          # Apply patches in order
          tryCatch(
            {
              for (patch in (last_patch + 1):last_patch_file) {
                source(
                  system.file(
                    "patches",
                    paste0("patch_", patch, ".R"),
                    package = "AquaCache"
                  ),
                  local = TRUE
                )
              }
              message("Patches applied successfully.\n")
            },
            error = function(e) {
              stop(
                "Patches not applied. An error occurred in patch ",
                patch,
                " : ",
                e$message,
                "\n"
              )
            }
          )
        } else if (choice == 2) {
          warning(
            "Patches not applied. Please apply patches before running any functions from this package.\n"
          )
        } else {
          warning(
            "Invalid choice. Patches not applied. Please apply patches before running any functions from this package.\n"
          )
        }
      }
    }
  } else {
    warning(
      "You are not connecting as 'admin' or 'postgres' user so no checks for applicable patches could be done.\n"
    )
  }

  if (!silent) {
    message(
      "Connected to the aquacache database with the timezone set to UTC.\n"
    )
    message("Remember to disconnect using DBI::dbDisconnect when finished!")
  }

  return(con)
}
