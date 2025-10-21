# Patch 15

# Initial checks #################
# # Ensure the user is postgres OR admin as this patch requires it
# check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")
#
# if (!check$session_user %in% c("postgres", "admin")) {
#   stop("You do not have the necessary privileges for this patch. Connect as postgres or admin user to make this work.")
# }
# Ensure the user is postgres as this patch requires it
check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on Patch 15. Changes are being made within a transaction, so if something goes wrong, the database will be rolled back to its previous state (but you have a backup, right?)."
)

message(
  "This patch modifies adds color codes to grade, approvals, and qualifiers so that these can be used when plotting data."
)

# Begin a transaction
message("Starting transaction...")
DBI::dbExecute(con, "BEGIN;")
attr(con, "active_transaction") <- TRUE
tryCatch(
  {
    # Modify the grade_types table
    DBI::dbExecute(
      con,
      "ALTER TABLE grade_types ADD COLUMN color_code VARCHAR(9);"
    )
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#228B22' WHERE grade_type_code = 'A';"
    ) # Excellent
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#ffd700' WHERE grade_type_code = 'B';"
    ) # Good
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#ffaaaa' WHERE grade_type_code = 'C';"
    ) # Fair
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#8b5338' WHERE grade_type_code = 'D';"
    ) # Poor
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#FF0000' WHERE grade_type_code = 'F';"
    ) # Unusable
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#808080' WHERE grade_type_code = 'UNK' OR grade_type_code = 'UNS';"
    ) # Unknown or unspecified
    DBI::dbExecute(
      con,
      "UPDATE grade_types SET color_code = '#471a1a' WHERE grade_type_code = 'MISS';"
    ) # Missing

    # Modify the approval_types table
    DBI::dbExecute(
      con,
      "ALTER TABLE approval_types ADD COLUMN color_code VARCHAR(9);"
    )
    DBI::dbExecute(
      con,
      "UPDATE approval_types SET color_code = '#228B22' WHERE approval_type_code = 'A';"
    ) # Approved
    DBI::dbExecute(
      con,
      "UPDATE approval_types SET color_code = '#ffd700' WHERE approval_type_code = 'C';"
    ) # Ready for review
    DBI::dbExecute(
      con,
      "UPDATE approval_types SET color_code = '#00ffff' WHERE approval_type_code = 'R';"
    ) # Reviewed, pending approval
    DBI::dbExecute(
      con,
      "UPDATE approval_types SET color_code = '#0080c0' WHERE approval_type_code = 'N';"
    ) # Not reviewed
    DBI::dbExecute(
      con,
      "UPDATE approval_types SET color_code = '#808080' WHERE approval_type_code = 'UNK' OR approval_type_code = 'UNS';"
    ) # Unknown or unspecified

    # Add a row for 'reviewed, requires revision'
    DBI::dbExecute(
      con,
      "INSERT INTO approval_types (approval_type_code, approval_type_description, approval_type_description_fr, color_code) VALUES ('RR', 'Reviewed, requires revision', 'Examiné, révision requise, ', '#FF0000');"
    )

    # Modify the qualifier_types table (many will just be gray)
    DBI::dbExecute(
      con,
      "ALTER TABLE qualifier_types ADD COLUMN color_code VARCHAR(9);"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#80ffff' WHERE qualifier_type_code = 'ICE';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#80ffff' WHERE qualifier_type_code = 'ICE-EST';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#e9dec7' WHERE qualifier_type_code = 'DRY';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#e9dec7' WHERE qualifier_type_code = 'OOW';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#FF0000' WHERE qualifier_type_code = 'SUS';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#ffaaaa' WHERE qualifier_type_code = 'EST';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#ef1fad' WHERE qualifier_type_code = 'DD';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#004080' WHERE qualifier_type_code = 'REL';"
    )
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#60c0c8' WHERE qualifier_type_code = 'INT';"
    )
    # Make everything else gray
    DBI::dbExecute(
      con,
      "UPDATE qualifier_types SET color_code = '#808080' WHERE qualifier_type_code NOT IN ('ICE', 'ICE-EST', 'DRY', 'OOW', 'SUS', 'EST', 'DD', 'REL', 'INT');"
    )

    # Drop old (no longer used) image_type column from table image_series
    # Remove the unique constraint on img_type and location_id first
    DBI::dbExecute(
      con,
      "ALTER TABLE image_series DROP CONSTRAINT IF EXISTS unique_location_img_type;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE image_series DROP COLUMN IF EXISTS img_type;"
    )
    # Add a new unique constraint on location_id
    DBI::dbExecute(
      con,
      "ALTER TABLE image_series ADD CONSTRAINT unique_location UNIQUE (location_id);"
    )

    # Update the version_info table
    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '15' WHERE item = 'Last patch number';"
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
    attr(con, "active_transaction") <- FALSE

    message("Patch 15 applied successfully.")
  },
  error = function(e) {
    # Rollback the transaction
    DBI::dbExecute(con, "ROLLBACK;")
    attr(con, "active_transaction") <<- FALSE
    stop(
      "Patch 15 failed and the DB has been rolled back to its earlier state. ",
      e$message
    )
  }
)
