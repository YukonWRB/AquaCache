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


# Update surveys constraint
  con <- HydroMetDB::snowConnect()
  DBI::dbExecute(con, "ALTER TABLE surveys DROP CONSTRAINT method_check")
  
  DBI::dbExecute(con, "ALTER TABLE surveys ADD CONSTRAINT method_check CHECK (method IN ('average', 'bulk', 'standard', 'no sample'))")
  
##### Adding snow pillow/scale data to db ####
  ## Pull in table I created
  table <- read.csv("//Env-fs/env-data/corp/water/Hydrology/11_SnowMet_Network/01_Automated_Stations/51_Data_Exports/Discrete SD & SWE for Snow DB import/All.csv")

  surveys <- table
  #### Make survey table
  ## Set survey_date as date
  surveys$survey_date2 <- as.Date(surveys$survey_date)
    # Function to calculate closest first of the month
    library(lubridate)
    find_closest_first <- function(date) {
    # Get the first day of the current month
    first_current_month <- floor_date(date, "month")
    
    # Get the first day of the next month
    first_next_month <- ceiling_date(date, "month")
    
    # Calculate which is closer to the survey_date
    if (date - first_current_month <= first_next_month - date) {
      return(first_current_month)
    } else {
      return(first_next_month)
    }
  }
  # Using sapply to apply the function to each date in the survey_date column
    surveys$target_date <- as.Date(unlist(lapply(surveys$survey_date2, find_closest_first)))
  ## Subset to columns of interest
  surveys <- surveys[, c("location", "target_date", "survey_date", "method")]
  # remove duplicates
  surveys <- surveys[!duplicated(surveys),]
  
  #### Make measurements table
  measurements <- table
  # Get swe and depth into same row (widen)
  measurements <- measurements %>% 
    tidyr::pivot_wider(
      names_from = parameter,
      values_from = value
    )
  # Add estimate_flag and exclude_flag
  measurements$estimate_flag <- FALSE
  measurements$exclude_flag <- FALSE
  # Keep columns of interest and rename
  measurements <- measurements[, c("survey_date", "")]
  
  
    
