con <- AquaCache::snowConnect()
DBI::dbDisconnect(con)

#### Add column to snow database surveys table ####

  # Add column method
  DBI::dbSendQuery(con, "ALTER TABLE surveys ADD COLUMN method TEXT")

  # Set method of those already in db (average)
  DBI::dbSendQuery(con, "UPDATE surveys SET method = 'average'")

  # Add comment to new column
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.method IS 'The method used for collecting the survey. Options are standard, bulk and average. The average option indicates that depth and SWE values represent an average of multiple samples. All entries prior to 2024 snow season are calculated averages'")

  # Remove average column from measurements table
  DBI::dbExecute(con, "ALTER TABLE measurements DROP COLUMN average")

  # Add ice_notes column
  DBI::dbSendQuery(con, "ALTER TABLE surveys ADD COLUMN ice_notes TEXT")

  # Add comment to new column
  DBI::dbExecute(con, "COMMENT ON COLUMN public.surveys.ice_notes IS 'Notes specific to the description of ice layers within the snow pack or on ground surface below snow.'")

#### Add column to snow database maintenance table ####

  # Add column date_completed
  DBI::dbSendQuery(con, "ALTER TABLE maintenance ADD COLUMN date_completed DATE")

  # Add constraint so that DATE is NOT NULL when completed is TRUE
  DBI::dbSendQuery(con, "ALTER TABLE maintenance ADD CONSTRAINT if_completed_then_date_is_not_null
                  CHECK (
                  (completed = FALSE AND date_completed IS NULL) OR
                  (completed = TRUE AND date_completed IS NOT NULL))
")
  DBI::dbSendQuery(con, "ALTER TABLE maintenance DROP CONSTRAINT if_completed_then_date_is_not_null
")

  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.date_completed IS 'The date on which the maintenance was completed. When completed = TRUE, date_completed must be not NULL. When completed = FALSE, date_completed must be NULL'")

  # Add a partial unique index so that when completed = FALSE, location and maintenance must be unique
  DBI::dbSendQuery(con, "CREATE UNIQUE INDEX unique_location_maintenance ON maintenance (location, maintenance) WHERE completed = FALSE")

  DBI::dbExecute(con, "COMMENT ON COLUMN public.maintenance.location IS 'The snow course for which this maintenance is linked to. A foreign key reffering to location from locations table. Location and maintenance but be a unique combination when completed = FALSE.'")

#### Set predefined set of value options ####

  DBI::dbSendQuery(con, "ALTER TABLE surveys ADD CONSTRAINT method_check CHECK (method IN ('average', 'bulk', 'standard'))")

#### Testing readSnowTemplate (adding and deleting) ####
 # All tests are done where sample_datetime is 2024 and later
  # Remove measurements 2024 and later
  surv_id <- DBI::dbGetQuery(con, "SELECT DISTINCT survey_id FROM measurements WHERE sample_datetime > '2024-01-01 12:00:00'")$survey_id
  DBI::dbExecute(con, paste0("DELETE FROM measurements WHERE survey_id IN ('", paste0(surv_id, collapse = "', '"), "')") )
  # Remove survey with survey_id from previous removal
  DBI::dbGetQuery(con, paste0("SELECT * FROM surveys WHERE survey_id IN ('", paste(surv_id, collapse = "', '"), "')") )
  DBI::dbExecute(con, paste0("DELETE FROM surveys WHERE survey_id IN ('", paste(surv_id, collapse = "', '"), "')") )



