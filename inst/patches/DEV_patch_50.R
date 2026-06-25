# Patch 50: database-backed source import parameter mappings

check <- DBI::dbGetQuery(con, "SELECT SESSION_USER")

if (check$session_user != "postgres") {
  stop(
    "You do not have the necessary privileges for this patch. Connect as postgres user to make this work."
  )
}

message(
  "Working on patch 50: adding flexible, database-backed import parameter mapping tables."
)

if (dbTransCheck(con)) {
  stop(
    "A transaction is already in progress. Please commit or rollback the current transaction before applying this patch."
  )
}

active <- FALSE
active <- dbTransBegin(con)

tryCatch(
  {
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes ADD COLUMN IF NOT EXISTS bedrock_reached BOOLEAN DEFAULT NULL;"
    )
    DBI::dbExecute(
      con,
      "COMMENT ON COLUMN boreholes.boreholes.bedrock_reached IS 'TRUE if bedrock was reached, FALSE if not, NULL if unknown';"
    )

    # Make it so that if bedrock_reached is TRUE, then depth_to_bedrock_m must be non-null, and if bedrock_reached is FALSE or NULL, then depth_to_bedrock_m must be NULL.

    # Find rows that violate the constraint for troubleshooting
    bad <- DBI::dbGetQuery(
      con,
      "SELECT * FROM boreholes.boreholes
       WHERE (bedrock_reached = TRUE AND depth_to_bedrock_m IS NULL)
          OR (bedrock_reached IN (FALSE, NULL) AND depth_to_bedrock_m IS NOT NULL);"
    )

    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes DROP CONSTRAINT IF EXISTS bedrock_depth_check;"
    )
    DBI::dbExecute(
      con,
      "ALTER TABLE boreholes.boreholes
       ADD CONSTRAINT bedrock_depth_check
       CHECK (
         (bedrock_reached = TRUE AND depth_to_bedrock_m IS NOT NULL) OR
         (bedrock_reached = FALSE OR bedrock_reached IS NULL AND depth_to_bedrock_m IS NULL)
       );"
    )

    DBI::dbExecute(
      con,
      "UPDATE information.version_info SET version = '50'
       WHERE item = 'Last patch number';"
    )
    DBI::dbExecute(
      con,
      paste0(
        "UPDATE information.version_info SET version = '",
        as.character(packageVersion('AquaCache')),
        "' WHERE item = 'AquaCache R package used for last patch';"
      )
    )

    DBI::dbExecute(con, "COMMIT;")
    active <- FALSE

    message(
      "Patch 50 applied successfully."
    )
  },
  error = function(e) {
    if (isTRUE(active)) {
      message("Error detected. Rolling back active transaction...")
      try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    }
    stop(e)
  }
)
