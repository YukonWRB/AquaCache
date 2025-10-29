# Patch 28

# Initial checks #################
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 28. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

# Begin a transaction
message("Starting transaction...")

check <- dbTransCheck(con) # Check if a transaction is already in progress
if (check) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}
active <- dbTransBegin(con)

tryCatch(
  {
    message(
      "Modifying parameters table: simplifying pH parameter; updating units for coliform parameters; renaming nitrogen parameters; adding total nitrogen and palladium parameters..."
    )

    # Delete ph field, rename ph, lab to ph
    # Find the parameter_id for ph, field
    id <- DBI::dbGetQuery(
      con,
      "SELECT parameter_id FROM parameters WHERE param_name = 'ph, field';"
    )$parameter_id
    # Delete the reference in parameter_relationships
    if (length(id) > 0) {
      DBI::dbExecute(
        con,
        paste0(
          "DELETE FROM parameter_relationships WHERE parameter_id = ",
          id,
          ";"
        )
      )
    }
    DBI::dbExecute(
      con,
      "DELETE FROM parameters WHERE param_name = 'ph, field';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET param_name = 'ph' WHERE param_name = 'ph, lab';"
    )

    # Update the unit for fecal coliform
    DBI::dbExecute(
      con,
      "UPDATE parameters SET unit_default = 'CFU/100 mL' WHERE param_name = 'fecal coliform';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET unit_default = 'CFU/100 mL' WHERE param_name = 'total coliform';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET unit_default = 'CFU/100 mL' WHERE param_name = 'escherichia coli';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET unit_default = 'CFU/100 mL' WHERE param_name = 'coliphage';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET unit_default = 'ratio' WHERE param_name = 'coliform/streptococcus ratio, fecal';"
    )

    # rename nitrogen related parameters
    DBI::dbExecute(
      con,
      "UPDATE parameters SET param_name = 'nitrogen, organic' WHERE param_name = 'organic nitrogen';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET param_name = 'nitrogen inorganic (ammonia, nitrate and nitrite)' WHERE param_name = 'inorganic nitrogen (ammonia, nitrate and nitrite)';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET param_name = 'nitrogen inorganic (nitrate and nitrite)' WHERE param_name = 'inorganic nitrogen (nitrate and nitrite)';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET param_name = 'nitrogen, kjeldahl' WHERE param_name = 'kjeldahl nitrogen';"
    )
    DBI::dbExecute(
      con,
      "UPDATE parameters SET param_name = 'nitrogen, undefined' WHERE param_name = 'total nitrogen, mixed forms';"
    )

    # Add parameter for total nitrogen
    DBI::dbExecute(
      con,
      "INSERT INTO parameters (param_name, unit_default, result_speciation, sample_fraction, plot_default_y_orientation) VALUES ('nitrogen, total', 'mg/L', FALSE, TRUE, 'normal');"
    )

    # Add palladium if not exist
    exist <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM parameters WHERE param_name = 'palladium';"
    )$n
    if (exist == 0) {
      DBI::dbExecute(
        con,
        "INSERT INTO parameters (param_name, unit_default, result_speciation, sample_fraction, plot_default_y_orientation) VALUES ('palladium', 'mg/L', FALSE, TRUE, 'normal');"
      )
    }

    # Wrap things up ##################
    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '28' WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion("AquaCache")),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )
    # Commit the transaction
    DBI::dbExecute(con, "COMMIT;")

    message("Patch 28 applied successfully, transaction closed.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    stop(
      "Patch 28 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
