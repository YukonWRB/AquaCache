suppressPackageStartupMessages({
  library(data.table)
})

# Seed AquaCache with the EQWin parameters, units, result speciations, and
# optional import-parameter mapping needed by downloadEQWin(). After applying
# changes it also writes inst/import_keys/EQWin.csv, a finalized fallback key
# that downloadEQWin() can use when the database mapping is unavailable.
#
# Test database:
# & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' inst\scripts\seed_EQWin_import_lookups.R --db=testdb --apply --upload-mapping
#
# Non-test database, including production, requires the explicit safety flag:
# & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' inst\scripts\seed_EQWin_import_lookups.R --db=aquacache --allow-non-testdb --apply --upload-mapping

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) {
    return(default)
  }
  sub(paste0("^", name, "="), "", hit[[length(hit)]])
}

arg_flag <- function(name) {
  name %in% args || identical(tolower(arg_value(name, "false")), "true")
}

read_mapping_input <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xlsm", "xls")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Reading Excel keys requires the openxlsx package.")
    }
    return(as.data.table(openxlsx::read.xlsx(path)))
  }
  fread(path, encoding = "UTF-8")
}

as_key_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  x <- trimws(tolower(as.character(x)))
  x[x %in% c("true", "t", "1", "yes", "y")] <- "TRUE"
  x[x %in% c("false", "f", "0", "no", "n")] <- "FALSE"
  as.logical(x)
}

is_present <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

first_existing_path <- function(paths) {
  paths <- paths[is_present(paths)]
  hit <- paths[file.exists(paths)]
  if (!length(hit)) {
    return(NA_character_)
  }
  normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
}

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (file.exists("C:/Users/gtdelapl/Documents/.Renviron")) {
  readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
}
default_key <- first_existing_path(c(
  file.path(repo_root, "inst", "import_keys", "DEV_EQWin_import_keys_finished.csv"),
  file.path(repo_root, "outputs", "eqwin_key_finished", "DEV_EQWin_import_keys_finished.xlsx")
))
if (is.na(default_key)) {
  stop(
    "Could not find a finished EQWin key. Provide --key=/path/to/DEV_EQWin_import_keys_finished.csv."
  )
}

key_path <- normalizePath(arg_value("--key", default_key), winslash = "/", mustWork = TRUE)
target_db <- arg_value("--db", Sys.getenv("EQWIN_TARGET_DB", "testdb"))
target_host <- arg_value(
  "--host",
  if (identical(target_db, Sys.getenv("aquacacheTestName", "testdb"))) {
    Sys.getenv("aquacacheTestHost", "10.250.12.154")
  } else {
    Sys.getenv("aquacacheHost", "10.250.12.154")
  }
)
target_port <- arg_value(
  "--port",
  if (identical(target_db, Sys.getenv("aquacacheTestName", "testdb"))) {
    Sys.getenv("aquacacheTestPort", Sys.getenv("aquacachePort", "5432"))
  } else {
    Sys.getenv("aquacachePort", "5432")
  }
)
target_user <- arg_value(
  "--user",
  if (identical(target_db, Sys.getenv("aquacacheTestName", "testdb"))) {
    Sys.getenv("aquacacheTestUser", Sys.getenv("aquacacheAdminUser"))
  } else {
    Sys.getenv("aquacacheAdminUser")
  }
)
target_pass <- arg_value(
  "--password",
  if (identical(target_db, Sys.getenv("aquacacheTestName", "testdb"))) {
    Sys.getenv("aquacacheTestPass", Sys.getenv("aquacacheAdminPass"))
  } else {
    Sys.getenv("aquacacheAdminPass")
  }
)
output_dir <- normalizePath(
  arg_value(
    "--output-dir",
    file.path(tempdir(), "AquaCache_eqwin_seed", target_db)
  ),
  winslash = "/",
  mustWork = FALSE
)
package_key_arg <- arg_value(
  "--package-key",
  file.path(repo_root, "inst", "import_keys", "EQWin.csv")
)
package_key_path <- if (tolower(package_key_arg) %in% c("", "none", "false", "no")) {
  NA_character_
} else {
  normalizePath(package_key_arg, winslash = "/", mustWork = FALSE)
}
apply_changes <- arg_flag("--apply")
allow_non_testdb <- arg_flag("--allow-non-testdb")
upload_mapping <- arg_flag("--upload-mapping")
update_existing_cas <- arg_flag("--update-existing-cas")
source_code <- arg_value("--source-code", "EQWin")
source_name <- arg_value("--source-name", "EQWin completed import key")

if (file.exists(file.path(repo_root, "DESCRIPTION"))) {
  suppressPackageStartupMessages(pkgload::load_all(repo_root, quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(AquaCache))
}

if (!identical(target_db, "testdb") && !allow_non_testdb) {
  stop(
    "Refusing to seed EQWin lookups outside testdb without --allow-non-testdb. ",
    "Target database was: ",
    target_db
  )
}
if (!apply_changes) {
  message("Dry run only. Re-run with --apply to modify ", target_db, ".")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
completed_key_path <- file.path(output_dir, paste0("DEV_EQWin_import_keys_", target_db, "_completed.csv"))
summary_path <- file.path(output_dir, paste0("seed_EQWin_import_lookups_", target_db, "_summary.csv"))
unit_review_path <- file.path(output_dir, paste0("seed_EQWin_import_lookups_", target_db, "_unit_review.csv"))
cas_review_path <- file.path(output_dir, paste0("seed_EQWin_import_lookups_", target_db, "_cas_review.csv"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = target_db,
  host = target_host,
  port = target_port,
  user = target_user,
  password = target_pass
)
on.exit(DBI::dbDisconnect(con), add = TRUE)
DBI::dbExecute(con, "SET timezone = 'UTC'")
db_identity <- DBI::dbGetQuery(
  con,
  "SELECT current_database() AS db, inet_server_addr()::text AS host, current_user AS username"
)
if (!identical(db_identity$db[[1]], target_db)) {
  stop("Connected to unexpected database: ", db_identity$db[[1]])
}

key <- read_mapping_input(key_path)
key <- as.data.table(key)
if (!("ignore" %in% names(key))) {
  key[, ignore := FALSE]
}
key[, ignore_bool := as_key_logical(ignore)]
key[is.na(ignore_bool), ignore_bool := FALSE]
active_key <- key[ignore_bool == FALSE]

matrix_unit_column <- function(matrix_state_id) {
  fifelse(
    as.integer(matrix_state_id) == 1L,
    "units_liquid",
    fifelse(as.integer(matrix_state_id) == 2L, "units_solid", "units_gas")
  )
}

lookup_units <- function() {
  as.data.table(DBI::dbGetQuery(con, "SELECT unit_id, unit_name FROM public.units"))
}

resolve_units <- function(unit_names) {
  units <- lookup_units()
  ids <- units$unit_id[match(unit_names, units$unit_name)]
  if (anyNA(ids[is_present(unit_names)])) {
    stop("Missing units: ", paste(unique(unit_names[is.na(ids) & is_present(unit_names)]), collapse = ", "))
  }
  ids
}

insert_missing_units <- function(unit_names) {
  unit_names <- sort(unique(unit_names[is_present(unit_names)]))
  missing <- setdiff(unit_names, lookup_units()$unit_name)
  if (!length(missing)) {
    return(0L)
  }
  if (apply_changes) {
    for (unit_name in missing) {
      DBI::dbExecute(
        con,
        "INSERT INTO public.units (unit_name)
         VALUES ($1)
         ON CONFLICT (unit_name) DO NOTHING",
        params = list(unit_name)
      )
    }
  }
  length(missing)
}

lookup_speciations <- function() {
  as.data.table(DBI::dbGetQuery(
    con,
    "SELECT result_speciation_id, result_speciation
     FROM discrete.result_speciations"
  ))
}

insert_missing_speciations <- function(speciation_names) {
  speciation_names <- sort(unique(speciation_names[is_present(speciation_names)]))
  missing <- setdiff(speciation_names, lookup_speciations()$result_speciation)
  if (!length(missing)) {
    return(0L)
  }
  if (apply_changes) {
    for (speciation in missing) {
      DBI::dbExecute(
        con,
        "INSERT INTO discrete.result_speciations (result_speciation)
         VALUES ($1)
         ON CONFLICT (result_speciation) DO NOTHING",
        params = list(speciation)
      )
    }
  }
  length(missing)
}

new_param_rows <- unique(
  active_key[
    mapping_status == "requires_new_parameter" & is_present(NEW_param_needed),
    .(
      param_name = NEW_param_needed,
      description = ParamDesc,
      cas_number,
      result_speciation = is_present(NEW_speciation_needed) |
        is_present(result_speciation_id) |
        is_present(result_speciation_AC),
      sample_fraction = is_present(sample_fraction) | is_present(sample_fraction_AC),
      matrix_state_id = as.integer(matrix_state),
      unit_name = input_unit
    )
  ],
  by = "param_name"
)

speciation_needed <- active_key[
  is_present(NEW_speciation_needed),
  unique(trimws(as.character(NEW_speciation_needed)))
]
units_needed <- unique(c(active_key$input_unit))

apply_seed <- function() {
  units_inserted <- insert_missing_units(units_needed)
  speciations_inserted <- insert_missing_speciations(speciation_needed)

  if (!apply_changes) {
    return(list(
      units_inserted = units_inserted,
      speciations_inserted = speciations_inserted,
      parameters_inserted = nrow(new_param_rows),
      existing_parameter_empty_units_updated = NA_integer_,
      existing_parameter_speciation_flags_updated = NA_integer_,
      existing_parameter_fraction_flags_updated = NA_integer_,
      existing_parameter_cas_updated = NA_integer_
    ))
  }

  new_param_rows[, unit_id := resolve_units(unit_name)]
  new_param_rows[, unit_col := matrix_unit_column(matrix_state_id)]
  existing_params <- as.data.table(DBI::dbGetQuery(
    con,
    "SELECT parameter_id, param_name FROM public.parameters"
  ))
  insert_rows <- new_param_rows[!(param_name %chin% existing_params$param_name)]
  for (i in seq_len(nrow(insert_rows))) {
    units_liquid <- if (identical(insert_rows$unit_col[[i]], "units_liquid")) insert_rows$unit_id[[i]] else NA_integer_
    units_solid <- if (identical(insert_rows$unit_col[[i]], "units_solid")) insert_rows$unit_id[[i]] else NA_integer_
    units_gas <- if (identical(insert_rows$unit_col[[i]], "units_gas")) insert_rows$unit_id[[i]] else NA_integer_
    DBI::dbExecute(
      con,
      "INSERT INTO public.parameters (
         param_name, description, cas_number, result_speciation,
         sample_fraction, plot_default_y_orientation,
         units_liquid, units_solid, units_gas
       ) VALUES (
         $1, $2, $3, $4, $5, 'normal', $6, $7, $8
       )
       ON CONFLICT (param_name) DO NOTHING",
      params = list(
        insert_rows$param_name[[i]],
        insert_rows$description[[i]],
        insert_rows$cas_number[[i]],
        insert_rows$result_speciation[[i]],
        insert_rows$sample_fraction[[i]],
        units_liquid,
        units_solid,
        units_gas
      )
    )
  }

  params <- as.data.table(DBI::dbGetQuery(
    con,
    "SELECT parameter_id, param_name, result_speciation, sample_fraction,
            cas_number, units_liquid, units_solid, units_gas
     FROM public.parameters"
  ))
  key_param_names <- fifelse(
    active_key$mapping_status == "requires_new_parameter" & is_present(active_key$NEW_param_needed),
    as.character(active_key$NEW_param_needed),
    NA_character_
  )
  key_param_ids <- suppressWarnings(as.integer(active_key$parameter_id))
  key_param_ids[is.na(key_param_ids)] <- params$parameter_id[match(key_param_names[is.na(key_param_ids)], params$param_name)]
  flag_rows <- data.table(
    parameter_id = key_param_ids,
    needs_speciation = is_present(active_key$NEW_speciation_needed) |
      is_present(active_key$result_speciation_id) |
      is_present(active_key$result_speciation_AC),
    needs_fraction = is_present(active_key$sample_fraction) | is_present(active_key$sample_fraction_AC)
  )
  spec_flag_ids <- unique(flag_rows[!is.na(parameter_id) & needs_speciation == TRUE, parameter_id])
  frac_flag_ids <- unique(flag_rows[!is.na(parameter_id) & needs_fraction == TRUE, parameter_id])
  spec_flags_updated <- 0L
  frac_flags_updated <- 0L
  if (length(spec_flag_ids)) {
    spec_flags_updated <- DBI::dbExecute(
      con,
      paste0(
        "UPDATE public.parameters
         SET result_speciation = TRUE,
             modified_by = CURRENT_USER,
             modified = CURRENT_TIMESTAMP
         WHERE parameter_id IN (",
        paste(spec_flag_ids, collapse = ","),
        ")
           AND result_speciation IS DISTINCT FROM TRUE"
      )
    )
  }
  if (length(frac_flag_ids)) {
    frac_flags_updated <- DBI::dbExecute(
      con,
      paste0(
        "UPDATE public.parameters
         SET sample_fraction = TRUE,
             modified_by = CURRENT_USER,
             modified = CURRENT_TIMESTAMP
         WHERE parameter_id IN (",
        paste(frac_flag_ids, collapse = ","),
        ")
           AND sample_fraction IS DISTINCT FROM TRUE"
      )
    )
  }

  unit_rows <- active_key[is_present(parameter_id) & is_present(matrix_state) & is_present(input_unit)]
  unit_rows[, unit_col := matrix_unit_column(as.integer(matrix_state))]
  unit_rows[, unit_id := resolve_units(input_unit)]
  unit_rows <- unique(unit_rows[, .(
    parameter_id = as.integer(parameter_id),
    unit_col,
    eqwin_unit = input_unit,
    eqwin_unit_id = unit_id
  )])
  unit_review <- vector("list", nrow(unit_rows))
  empty_units_updated <- 0L
  for (i in seq_len(nrow(unit_rows))) {
    unit_col <- unit_rows$unit_col[[i]]
    current <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT p.parameter_id, p.param_name, p.%s AS current_unit_id,
                u.unit_name AS current_unit_name
         FROM public.parameters p
         LEFT JOIN public.units u ON p.%s = u.unit_id
         WHERE p.parameter_id = $1",
        unit_col,
        unit_col
      ),
      params = list(unit_rows$parameter_id[[i]])
    )
    if (!nrow(current)) {
      next
    }
    action <- if (is.na(current$current_unit_id[[1]])) {
      "updated_empty_unit"
    } else if (identical(as.integer(current$current_unit_id[[1]]), as.integer(unit_rows$eqwin_unit_id[[i]]))) {
      "already_matched"
    } else {
      "left_existing_non_empty_unit_for_review"
    }
    unit_review[[i]] <- data.table(
      parameter_id = unit_rows$parameter_id[[i]],
      param_name = current$param_name[[1]],
      unit_column = unit_col,
      eqwin_unit = unit_rows$eqwin_unit[[i]],
      eqwin_unit_id = unit_rows$eqwin_unit_id[[i]],
      current_unit_name = current$current_unit_name[[1]],
      action = action
    )
    if (identical(action, "updated_empty_unit")) {
      DBI::dbExecute(
        con,
        sprintf(
          "UPDATE public.parameters
           SET %s = $1, modified_by = CURRENT_USER, modified = CURRENT_TIMESTAMP
           WHERE parameter_id = $2 AND %s IS NULL",
          unit_col,
          unit_col
        ),
        params = list(unit_rows$eqwin_unit_id[[i]], unit_rows$parameter_id[[i]])
      )
      empty_units_updated <- empty_units_updated + 1L
    }
  }
  unit_review <- rbindlist(unit_review, fill = TRUE)
  fwrite(unit_review, unit_review_path)

  cas_rows <- active_key[
    FLAG_AC_params_edit == TRUE & is_present(parameter_id) & is_present(cas_number),
    .(
      parameter_id = as.integer(parameter_id),
      input_param,
      ParamDesc,
      proposed_cas_number = cas_number,
      note = FLAG_notes_combined
    )
  ]
  cas_review <- merge(
    cas_rows,
    params[, .(parameter_id, param_name, current_cas_number = cas_number)],
    by = "parameter_id",
    all.x = TRUE
  )
  cas_review[, action := fifelse(
    is.na(current_cas_number) | current_cas_number != proposed_cas_number,
    if (update_existing_cas) "updated" else "left_for_review",
    "already_matched"
  )]
  cas_updated <- 0L
  if (update_existing_cas && nrow(cas_review[action == "updated"])) {
    for (i in seq_len(nrow(cas_review[action == "updated"]))) {
      row <- cas_review[action == "updated"][i]
      DBI::dbExecute(
        con,
        "UPDATE public.parameters
         SET cas_number = $1,
             modified_by = CURRENT_USER,
             modified = CURRENT_TIMESTAMP
         WHERE parameter_id = $2",
        params = list(row$proposed_cas_number[[1]], row$parameter_id[[1]])
      )
      cas_updated <- cas_updated + 1L
    }
  }
  fwrite(cas_review, cas_review_path)

  list(
    units_inserted = units_inserted,
    speciations_inserted = speciations_inserted,
    parameters_inserted = nrow(insert_rows),
    existing_parameter_empty_units_updated = empty_units_updated,
    existing_parameter_speciation_flags_updated = spec_flags_updated,
    existing_parameter_fraction_flags_updated = frac_flags_updated,
    existing_parameter_cas_updated = cas_updated
  )
}

seed_summary <- DBI::dbWithTransaction(con, apply_seed())

params <- as.data.table(DBI::dbGetQuery(
  con,
  "SELECT parameter_id, param_name FROM public.parameters"
))
specs <- lookup_speciations()

completed_key <- copy(key)
new_param_names <- completed_key[
  mapping_status == "requires_new_parameter" & is_present(NEW_param_needed),
  as.character(NEW_param_needed)
]
new_param_ids <- params$parameter_id[match(new_param_names, params$param_name)]
completed_key[
  mapping_status == "requires_new_parameter" & is_present(NEW_param_needed),
  parameter_id := new_param_ids
]
new_spec_names <- completed_key[
  is_present(NEW_speciation_needed),
  as.character(NEW_speciation_needed)
]
new_spec_ids <- specs$result_speciation_id[match(new_spec_names, specs$result_speciation)]
completed_key[
  is_present(NEW_speciation_needed),
  result_speciation_AC := new_spec_ids
]
completed_key[
  is_present(NEW_speciation_needed),
  result_speciation_id := new_spec_ids
]

unresolved_parameters <- completed_key[
  ignore_bool == FALSE & !is_present(parameter_id)
]
if (nrow(unresolved_parameters)) {
  stop(
    "Finalized EQWin key still has ",
    nrow(unresolved_parameters),
    " active row(s) without parameter_id. First unresolved input_param: ",
    unresolved_parameters$input_param[[1]]
  )
}

completed_key[, ignore_bool := NULL]
fwrite(completed_key, completed_key_path)

final_mapping_key_path <- completed_key_path
if (!is.na(package_key_path)) {
  if (apply_changes) {
    dir.create(dirname(package_key_path), recursive = TRUE, showWarnings = FALSE)
    fwrite(completed_key, package_key_path)
    final_mapping_key_path <- package_key_path
  } else {
    message(
      "Dry run only. Skipping finalized package key write to ",
      package_key_path,
      "."
    )
  }
}

uploaded_rows <- NA_integer_
if (upload_mapping) {
  if (!apply_changes) {
    stop("--upload-mapping requires --apply.")
  }
  existing_source <- DBI::dbGetQuery(
    con,
    "SELECT import_source_id
     FROM discrete.import_sources
     WHERE source_code = $1",
    params = list(source_code)
  )
  if (nrow(existing_source)) {
    DBI::dbExecute(
      con,
      "UPDATE discrete.import_parameter_mappings
       SET active = FALSE
       WHERE import_source_id = $1",
      params = list(existing_source$import_source_id[[1]])
    )
  }
  target_columns <- list(
    parameter = c("parameter_id", "parameter", "param_name"),
    result_type = c("result_type", "result_type_id"),
    sample_fraction = c("sample_fraction_id", "sample_fraction_AC", "sample_fraction"),
    result_value_type = c("result_value_type", "result_value_type_id"),
    result_speciation = c("result_speciation_id", "result_speciation_AC", "result_speciation"),
    matrix_state = c("matrix_state_id", "matrix_state")
  )
  resolved <- upsertImportParameterMappings(
    con = con,
    source_code = source_code,
    source_name = source_name,
    source_description = paste("Completed EQWin import key loaded from", basename(final_mapping_key_path)),
    mappings = final_mapping_key_path,
    match_columns = c("input_param", "ParamDesc", "input_unit"),
    target_columns = target_columns
  )
  uploaded_rows <- nrow(resolved)
}

summary <- data.table(
  metric = c(
    "database",
    "host",
    "username",
    "apply_changes",
    "key_path",
    "completed_key_path",
    "package_key_path",
    "final_mapping_key_path",
    "units_inserted",
    "result_speciations_inserted",
    "parameters_inserted",
    "existing_parameter_empty_units_updated",
    "existing_parameter_speciation_flags_updated",
    "existing_parameter_fraction_flags_updated",
    "existing_parameter_cas_updated",
    "unit_review_path",
    "cas_review_path",
    "mapping_source_code",
    "uploaded_mapping_rows"
  ),
  value = as.character(c(
    db_identity$db[[1]],
    db_identity$host[[1]],
    db_identity$username[[1]],
    apply_changes,
    key_path,
    completed_key_path,
    package_key_path,
    final_mapping_key_path,
    seed_summary$units_inserted,
    seed_summary$speciations_inserted,
    seed_summary$parameters_inserted,
    seed_summary$existing_parameter_empty_units_updated,
    seed_summary$existing_parameter_speciation_flags_updated,
    seed_summary$existing_parameter_fraction_flags_updated,
    seed_summary$existing_parameter_cas_updated,
    unit_review_path,
    cas_review_path,
    source_code,
    uploaded_rows
  ))
)
fwrite(summary, summary_path)
print(summary)
