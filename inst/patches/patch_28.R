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
    # Check parameter_id for palladium
    pd_id <- DBI::dbGetQuery(
      con,
      "SELECT parameter_id FROM parameters WHERE param_name = 'palladium';"
    )$parameter_id

    if (pd_id != 1263) {
      warning(
        "Palladium parameter_id is not 1263 as expected. Please check the downloadECCCeq1.csv key integrity."
      )
    }

    # Update ECCC's lab name
    exist <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM laboratories WHERE lab_name = 'Environment Canada';"
    )$n

    if (exist == 1) {
      DBI::dbExecute(
        con,
        "UPDATE laboratories SET lab_name = 'Environment and Climate Change Canada' WHERE lab_name = 'Environment Canada';"
      )
    } else {
      DBI::dbExecute(
        con,
        "INSERT INTO laboratories (lab_name) VALUES ('Environment and Climate Change Canada');"
      )
    }

    # Drop and update unique constraint on 'results' to include lab/field in the key
    # Find the name of the existing unique constraint
    constraint_name <- DBI::dbGetQuery(
      con,
      "SELECT conname FROM pg_constraint WHERE conrelid = 'discrete.results'::regclass AND contype = 'u';"
    )$conname
    # Drop the existing unique constraint
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE discrete.results DROP CONSTRAINT ",
        constraint_name,
        ";"
      )
    )
    DBI::dbExecute(
      con,
      "
    ALTER TABLE discrete.results ADD CONSTRAINT sampleid_type_parameter_fraction_result_value_key UNIQUE NULLS NOT DISTINCT (sample_id, result_type, parameter_id, sample_fraction_id, result_value_type, result_speciation_id, protocol_method, laboratory, analysis_datetime)
    "
    )

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
