#' Dump database schema and data to a file
#'
#' This function uses the `pg_dump` and `pg_dumpall` utilities to create a dump of the schema and global objects in a PostgreSQL database. The schema dump is saved to an SQL file in the specified output path. You must call this function from a machine with the PostgreSQL utilities installed.
#'
#' @seealso [create_test_db()]
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter:value pair of form aquacachePort="1234".
#' @param username Username. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminUser="username".
#' @param password Password. By default searches the .Renviron file for parameter:value pair of form aquacacheAdminPass="password".
#' @param outpath The path to the folder/location where the schema dump will be saved. If "choose", the user will be prompted to select a folder. Separate files will be created for the outputs of pg_dumpall and pg_dump.
#' @param pg_dump The path to the pg_dump utility. By default (NULL), the function searches the PATH for the utility, but this might not always work.
#' @param pg_dumpall The path to the pg_dumpall utility. By default (NULL), the function searches the PATH for the utility, but this might not always work.
#' @param data Should the contents of the target database be included in the dump? Default is FALSE.
#'
#' @returns An SQL file containing the schema definition saved to the 'outfile' path.
#' @export
#'

db_dump <- function(
  name = "aquacache",
  host = Sys.getenv("aquacacheHost"),
  port = Sys.getenv("aquacachePort"),
  username = Sys.getenv("aquacacheAdminUser"),
  password = Sys.getenv("aquacacheAdminPass"),
  outpath = "choose",
  pg_dump = NULL,
  pg_dumpall = NULL,
  data = FALSE
) {
  # Check or prompt for output path
  if (outpath == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want this report saved.")
    outpath <- rstudioapi::selectDirectory(
      caption = "Select Save Folder",
      path = file.path(Sys.getenv("USERPROFILE"), "Desktop")
    )
  }

  # Validate the `pg_dump` utility
  if (is.null(pg_dump)) {
    pg_dump <- Sys.which("pg_dump")
  }
  if (!file.exists(pg_dump)) {
    stop("The specified pg_dump utility does not exist or could not be found.")
  }

  # Validate the `pg_dumpall` utility
  if (is.null(pg_dumpall)) {
    pg_dumpall <- Sys.which("pg_dumpall")
  }
  if (!file.exists(pg_dumpall)) {
    stop(
      "The specified pg_dumpall utility does not exist or could not be found."
    )
  }

  Sys.setenv(PGPASSWORD = password) # Pass the password securely

  # Create the pg_dumpall command to dump global objects
  global_outfile <- file.path(outpath, "globals_dump.sql")
  dumpall_command <- sprintf(
    '"%s" -U %s -h %s -p %s --roles-only -f "%s"',
    pg_dumpall,
    username,
    host,
    port,
    global_outfile
  )

  # Execute the pg_dumpall command
  result_dumpall <- system(dumpall_command, ignore.stderr = FALSE)
  if (result_dumpall != 0) {
    stop(
      "Error occurred while running pg_dumpall. Check your database connection and credentials."
    )
  }
  message(sprintf("Global roles and privileges saved to: %s", global_outfile))

  # Create the pg_dump command for schema/data
  schema_outfile <- file.path(outpath, paste0(name, "_schema_dump.sql"))
  dump_command <- sprintf(
    '"%s" -U %s -h %s -p %s %s -f "%s" %s',
    pg_dump,
    username,
    host,
    port,
    if (data) "" else "-s", # Add schema-only flag if data is FALSE
    schema_outfile,
    name
  )

  # Execute the pg_dump command
  result_dump <- system(dump_command, ignore.stderr = FALSE)
  if (result_dump != 0) {
    stop(
      "Error occurred while running pg_dump. Check your database connection and credentials."
    )
  }
  message(sprintf("Database schema and data saved to: %s", schema_outfile))

  # Clear the password environment variable
  Sys.unsetenv("PGPASSWORD")

  # Return paths of the output files
  return(list(
    globals_file = global_outfile,
    schema_file = schema_outfile
  ))
}
