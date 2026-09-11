# Update existing EQWin import parameter mappings to match by ParamCode.
#
# This script preserves the current AquaCache target mapping fields. It only
# rewrites discrete.import_parameter_mappings.source_match for EQWin rows from
# the old ParamDesc-based shape to:
#   {"ParamCode": "<EQWin ParamCode>", "input_unit": "<EQWin Units>"}
#
# Dry run:
#   Rscript inst/scripts/update_EQWin_import_mappings_paramcode.R
#
# Apply to dev aquacache:
#   Rscript inst/scripts/update_EQWin_import_mappings_paramcode.R --apply

suppressPackageStartupMessages({
  library(data.table)
})

args <- commandArgs(trailingOnly = TRUE)
has_arg <- function(flag) flag %in% args
arg_value <- function(flag, default) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit) == 0L) {
    return(default)
  }
  sub(paste0("^", flag, "="), "", hit[[length(hit)]])
}
is_blank <- function(x) {
  is.null(x) || length(x) == 0L || is.na(x[[1]]) || !nzchar(trimws(x[[1]]))
}
scalar_chr <- function(x) {
  if (is_blank(x)) {
    return("")
  }
  as.character(x[[1]])
}

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
repo_root <- if (length(script_file) == 1L) {
  normalizePath(file.path(dirname(script_file), "..", ".."), winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

renviron <- "C:/Users/gtdelapl/Documents/.Renviron"
if (file.exists(renviron)) {
  readRenviron(renviron)
}

apply_changes <- has_arg("--apply")
include_inactive <- has_arg("--include-inactive")
db_name <- arg_value("--db-name", "aquacache")
db_host <- arg_value("--db-host", "10.250.12.154")
db_port <- arg_value("--db-port", Sys.getenv("aquacachePort", "5432"))
source_code <- arg_value("--source-code", "EQWin")
access_path <- arg_value("--access-path", "X:/EQWin/WaterResources.mdb")

if (apply_changes && !(identical(db_name, "aquacache") && identical(db_host, "10.250.12.154"))) {
  stop(
    "Refusing to write outside dev aquacache. ",
    "Pass --db-name=aquacache --db-host=10.250.12.154, or edit the script deliberately."
  )
}
if (!file.exists(access_path)) {
  stop("EQWin Access database not found: ", access_path)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required to load the local AquaCache checkout.")
}

pkgload::load_all(repo_root, quiet = TRUE)

con <- AquaConnect(
  name = db_name,
  host = db_host,
  port = db_port,
  silent = TRUE,
  check = FALSE
)
on.exit(DBI::dbDisconnect(con), add = TRUE)

eq_con <- AccessConnect(access_path, silent = TRUE)
if (is.null(eq_con) || !isTRUE(tryCatch(DBI::dbIsValid(eq_con), error = function(e) FALSE))) {
  stop("Could not open a valid EQWin Access connection for: ", access_path)
}
on.exit(DBI::dbDisconnect(eq_con), add = TRUE)

eqparams <- as.data.table(DBI::dbGetQuery(
  eq_con,
  "SELECT ParamCode, ParamDesc, Units
   FROM eqparams"
))
eqparams[, ParamCode := fifelse(is.na(ParamCode), "", as.character(ParamCode))]
eqparams[, ParamDesc := fifelse(is.na(ParamDesc), "", as.character(ParamDesc))]
eqparams[, Units := fifelse(is.na(Units), "", as.character(Units))]

source <- DBI::dbGetQuery(
  con,
  "SELECT import_source_id
   FROM discrete.import_sources
   WHERE source_code = $1;",
  params = list(source_code)
)
if (nrow(source) == 0L) {
  stop("No import source found for source_code '", source_code, "'.")
}
source_id <- source$import_source_id[[1]]

active_sql <- if (include_inactive) "" else "AND active IS TRUE"
mappings <- as.data.table(DBI::dbGetQuery(
  con,
  sprintf(
    "SELECT import_mapping_id,
            active,
            source_match::text AS source_match
     FROM discrete.import_parameter_mappings
     WHERE import_source_id = $1
       %s
     ORDER BY import_mapping_id;",
    active_sql
  ),
  params = list(source_id)
))
if (nrow(mappings) == 0L) {
  stop("No import parameter mappings found for source_code '", source_code, "'.")
}

mappings[, source_match_values := lapply(source_match, jsonlite::fromJSON)]
mappings[, old_ParamDesc := vapply(source_match_values, function(x) scalar_chr(x$ParamDesc), character(1))]
mappings[, old_input_param := vapply(source_match_values, function(x) scalar_chr(x$input_param), character(1))]
mappings[, old_ParamCode := vapply(source_match_values, function(x) scalar_chr(x$ParamCode), character(1))]
mappings[, old_input_unit := vapply(source_match_values, function(x) scalar_chr(x$input_unit), character(1))]

mappings[, ParamCode := fifelse(nzchar(old_ParamCode), old_ParamCode, old_input_param)]
needs_lookup <- !nzchar(mappings$ParamCode) & nzchar(mappings$old_ParamDesc)
if (any(needs_lookup)) {
  for (i in which(needs_lookup)) {
    candidates <- eqparams[
      ParamDesc == mappings$old_ParamDesc[[i]] &
        Units == mappings$old_input_unit[[i]],
      unique(ParamCode)
    ]
    if (length(candidates) == 1L) {
      mappings$ParamCode[[i]] <- candidates
    }
  }
}

unable <- mappings[!nzchar(mappings$ParamCode)]
if (nrow(unable) > 0L) {
  print(unable[, .(import_mapping_id, source_match)])
  stop("Could not determine ParamCode for ", nrow(unable), " mapping rows.")
}

catalog_codes <- unique(eqparams$ParamCode)
missing_from_catalog <- mappings[!mappings$ParamCode %chin% catalog_codes]
if (nrow(missing_from_catalog) > 0L) {
  warning(
    nrow(missing_from_catalog),
    " mapping rows use ParamCode values not found in ",
    access_path,
    ". Keeping those codes from the existing database mapping."
  )
}

mappings[, new_source_match := vapply(
  seq_len(.N),
  function(i) {
    jsonlite::toJSON(
      list(
        ParamCode = mappings$ParamCode[[i]],
        input_unit = mappings$old_input_unit[[i]]
      ),
      auto_unbox = TRUE,
      null = "null"
    )
  },
  character(1)
)]

duplicates <- mappings[, .N, by = new_source_match][N > 1L]
if (nrow(duplicates) > 0L) {
  problem_rows <- mappings[
    mappings$new_source_match %chin% duplicates$new_source_match,
    .(import_mapping_id, active, source_match, new_source_match)
  ]
  print(problem_rows)
  stop(
    "Rewriting would create duplicate source_match values. ",
    "Resolve these rows manually before applying."
  )
}

updates <- mappings[mappings$source_match != mappings$new_source_match]
cat("Database: ", db_name, " at ", db_host, "\n", sep = "")
cat("EQWin source: ", source_code, " (import_source_id ", source_id, ")\n", sep = "")
cat("Rows inspected: ", nrow(mappings), "\n", sep = "")
cat("Rows to update: ", nrow(updates), "\n", sep = "")
cat("Rows already using ParamCode shape: ", nrow(mappings) - nrow(updates), "\n", sep = "")
if (!include_inactive) {
  cat("Scope: active mappings only. Pass --include-inactive to inspect inactive rows too.\n")
}

if (nrow(updates) > 0L) {
  print(head(
    updates[, .(import_mapping_id, source_match, new_source_match)],
    10L
  ))
}

if (!apply_changes) {
  cat("Dry run only. Re-run with --apply to update source_match JSON.\n")
  quit(save = "no", status = 0L)
}

active_trans <- dbTransBegin(con)
tryCatch(
  {
    for (i in seq_len(nrow(updates))) {
      DBI::dbExecute(
        con,
        "UPDATE discrete.import_parameter_mappings
         SET source_match = $1::jsonb
         WHERE import_mapping_id = $2;",
        params = list(
          updates$new_source_match[[i]],
          updates$import_mapping_id[[i]]
        )
      )
    }
    if (active_trans) {
      DBI::dbExecute(con, "COMMIT;")
    }
  },
  error = function(e) {
    if (active_trans) {
      try(DBI::dbExecute(con, "ROLLBACK;"), silent = TRUE)
    }
    stop(e)
  }
)

summary <- DBI::dbGetQuery(
  con,
  "SELECT count(*) AS active_rows,
          count(*) FILTER (WHERE source_match ? 'ParamDesc') AS active_paramdesc_rows,
          count(*) FILTER (WHERE source_match ? 'ParamCode') AS active_paramcode_rows
   FROM discrete.import_parameter_mappings
   WHERE import_source_id = $1
     AND active IS TRUE;",
  params = list(source_id)
)
print(summary)
cat("EQWin import mappings updated.\n")
