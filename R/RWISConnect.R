#' Connect to the RWIS database
#'
#' @description
#'
#' Establishes a connection to the RWIS (Road Weather Information System) PostgreSQL database. Only works from within YG networks.
#'
#' @param name Database name.
#' @param host Database host address.
#' @param port Connection port.
#' @param username Username. Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password.
#' @return A connection to the database
#'
#' @seealso [AquaConnect()] for establishing a connection to the WRB's hydrometric database.
#'
#' @export
#'

RWISConnect <- function(
  name = Sys.getenv("rwisName", "rwdm"),
  host = Sys.getenv("rwisHost", "rwis.gov.yk.ca"),
  port = Sys.getenv("rwisPort", "5432"),
  username = Sys.getenv("rwisUser"),
  password = Sys.getenv("rwisPass")
) {
  # Check that each parameter is a non-empty string
  params <- list(
    name = name,
    host = host,
    port = port,
    username = username,
    password = password
  )
  for (param_name in names(params)) {
    param_value <- params[[param_name]]
    if (
      !is.character(param_value) ||
        length(param_value) != 1 ||
        nchar(param_value) == 0
    ) {
      stop(paste(
        "RWISConnect: parameter",
        param_name,
        "must be a non-empty string. By default, RWISConnect() reads connection parameters from environment variables."
      ))
    }
  }

  RWIS <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = params$name,
    host = params$host,
    port = params$port,
    user = params$username,
    password = params$password
  )

  if (!DBI::dbIsValid(RWIS)) {
    stop("Connection failed.")
  } else {
    return(RWIS)
  }
}
