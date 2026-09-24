# Run from the AquaCache checkout with an output JSON path as the first argument.
# Capture the actual patch statements, including loops and companion SQL. No
# database connection is opened. The JS test executes them in disposable PGlite.
output <- commandArgs(trailingOnly = TRUE)[[1L]]
statements <- list()
verification_sql <- NULL
testthat::with_mocked_bindings(
  {
    patch_env <- new.env(parent = globalenv())
    patch_env$con <- NULL
    patch_env$dbTransCheck <- function(con) FALSE
    patch_env$dbTransBegin <- function(con) TRUE
    suppressMessages(source("inst/patches/DEV_patch_reports.R", local = patch_env))
  },
  dbExecute = function(conn, statement, params = NULL, ...) {
    statements[[length(statements) + 1L]] <<- list(sql = statement, params = params)
    0L
  },
  dbGetQuery = function(conn, statement, ...) {
    if (statement == "SELECT SESSION_USER") return(data.frame(session_user = "postgres"))
    if (grepl("AS has_application_schema", statement, fixed = TRUE)) return(data.frame(ok = TRUE))
    if (grepl("SELECT version", statement, fixed = TRUE) &&
        !grepl("all_reporting_tables", statement, fixed = TRUE)) return(data.frame(version = "60"))
    if (grepl("AS available", statement, fixed = TRUE)) return(data.frame(available = FALSE))
    verification_sql <<- statement
    data.frame(ok = TRUE)
  },
  .package = "DBI"
)
jsonlite::write_json(list(statements = statements, verification = verification_sql),
  output, auto_unbox = TRUE, null = "null", pretty = TRUE)
cat("Captured", length(statements), "statements without a database connection.\n")

