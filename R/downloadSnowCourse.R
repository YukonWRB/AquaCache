#' Bring snow course data into the aquacache database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Brings in pared-down snow course data to the database. Can automatically calculate an offset value where locations have operated in parallel in anticipation of replacing the old location with a nearby new one, updating the calculation with each new data point (see parameter old_loc).
#'
#' @param location The location code associated with the snow course.
#' @param sub_location The sub-location code associated with the snow course (leave NULL if not applicable).
#' @param start_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If date, time will default to 00:00 to capture whole day.
#' @param end_datetime Specify as class Date, POSIXct OR as character string which can be interpreted as POSIXct. If character, UTC offset of 0 will be assigned, otherwise conversion to UTC 0 will be performed on POSIXct class input. If Date, time will default to 23:59:59 to capture whole day.
#' @param old_loc In some cases the measurement location has moved slightly over the years, but not enough for the new location to be distinct from the old location. In this case you can specify the old location name which will be searched for in the snow database. If found, the timeseries from the old location will be treated as if they are the new location. An offset will be calculated whenever possible putting the old location in-line with the new location. New location data takes precedence when both were measured.
#' @param adjust_start The start date or datetime to use for the adjustment of the old location data. If NULL, the start date of the new location will be used. To have no adjustment, set adjust_start and adjust_end to the same date/datetime
#' @param adjust_end The end date or datetime to use for the adjustment of the old location data. If NULL, the end date of the new location will be used.
#' @param con A connection to the aquacache database, only used if an offset is calculated for an old_loc. If not provided, a connection will be attempted using AquaConnect().
#' @param snowCon A connection to the snow database.
#'
#' @return A data.frame object with the requested data. If there are no new data points the data.frame will have 0 rows.
#' @export


downloadSnowCourse <- function(location, sub_location = NULL, start_datetime, end_datetime = Sys.time(), old_loc = NULL, adjust_start = NULL, adjust_end = NULL, con = NULL, snowCon = snowConnect())
{
  
  # Check parameters and set defaults ########################################
  # Checking start_datetime parameter
  tryCatch({
    if (inherits(start_datetime, "character") & nchar(start_datetime) > 10) { #Does not necessarily default to 0 hour.
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else if (inherits(start_datetime, "POSIXct")) {
      attr(start_datetime, "tzone") <- "UTC"
    } else if (inherits(start_datetime, "Date") | (inherits(start_datetime, "character") & nchar(start_datetime) == 10)) { #defaults to 0 hour
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
    } else {
      stop("Parameter start_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter start_datetime to POSIXct.")
  })
  
  # Checking end_datetime parameter
  tryCatch({
    if (inherits(end_datetime, "character") & nchar(end_datetime) > 10) { #Does not necessarily default to 0 hour.
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
    } else if (inherits(end_datetime, "POSIXct")) {
      attr(end_datetime, "tzone") <- "UTC"
    } else if (inherits(end_datetime, "Date") | (inherits(end_datetime, "character") & nchar(end_datetime) == 10)) { #defaults to very end of day
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")
      end_datetime <- end_datetime + 60*60*23.9999
    } else {
      stop("Parameter end_datetime could not be coerced to POSIXct.")
    }
  }, error = function(e) {
    stop("Failed to convert parameter end_datetime to POSIXct.")
  })
  
  start_date <- as.Date(start_datetime)
  end_date <- as.Date(end_datetime)
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  DBI::dbExecute(con, "SET timezone = 'UTC'")
  
  swe_paramid <- DBI::dbGetQuery(con, "SELECT parameter_id FROM parameters WHERE param_name = 'snow water equivalent';")[1,1]
  depth_paramid <- DBI::dbGetQuery(con, "SELECT parameter_id FROM parameters WHERE param_name = 'snow depth';")[1,1]
  media_id <- DBI::dbGetQuery(con, "SELECT media_id FROM media_types WHERE media_type = 'atmospheric'")[1,1]
  sample_type <- DBI::dbGetQuery(con, "SELECT sample_type_id FROM sample_types WHERE LOWER(sample_type) = 'sample-field msr/obs - no lab results expected'")[1,1]
  sample_owner <- DBI::dbGetQuery(con, "SELECT sample_type_id FROM sample_types WHERE LOWER(sample_type) = 'sample-field msr/obs - no lab results expected'")[1,1]
  sample_contributor <- DBI::dbGetQuery(con, "SELECT organization_id FROM organizations WHERE LOWER(name) = 'yukon department of environment, water resources branch';")[1,1]
  sample_collect_method <- DBI::dbGetQuery(con, "SELECT collection_method_id FROM collection_methods WHERE LOWER(collection_method) = 'observation'")[1,1]
  estimated_result <- DBI::dbGetQuery(con, "SELECT result_value_type_id FROM result_value_types WHERE LOWER(result_value_type) = 'estimated'")[1,1]
  actual_result <- DBI::dbGetQuery(con, "SELECT result_value_type_id FROM result_value_types WHERE LOWER(result_value_type) = 'actual'")[1,1]
  protocol_method <- DBI::dbGetQuery(con, "SELECT protocol_id FROM protocols_methods WHERE LOWER(protocol_name) = 'bc snow survey sampling guide'")[1,1]
  
  location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  
  # See if we need to adjust the old location data ############################  
  adjust <- FALSE # Flags if an adjustment is needed from old location data
  if (!is.null(old_loc)) {
    # Check for surveys at the old location matching up with the requested time range (if we're just adding new data and there are no old location new measurements, there won't be an update of the offset)
    old_surveys <- DBI::dbGetQuery(snowCon, paste0("SELECT survey_id FROM surveys WHERE location = '", old_loc, "' AND survey_date < '", end_date, "' AND survey_date > '", start_date, "';"))
    
    if (nrow(old_surveys) > 0) { # If TRUE, get all old site survey measurements
      # At this point, we have to go and get all old measurements as they may all need adjustment
      old_surveys <- DBI::dbGetQuery(snowCon, paste0("SELECT survey_id, survey_date, target_date, notes FROM surveys WHERE location = '", old_loc, "' AND survey_date < '", end_date, "';"))
      old_surveys$survey_date <- as.POSIXct(old_surveys$survey_date, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
      old_surveys$target_date <- as.POSIXct(old_surveys$target_date, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
      # Some survey notes are the text 'NA', so we need to convert them to actual NA
      old_surveys$notes[old_surveys$notes == "NA"] <- NA
      # Get the old location data points
      old_meas <- data.frame()
      for (i in 1:nrow(old_surveys)) {
        #Get the measurements for each survey
        meas <- DBI::dbGetQuery(snowCon, paste0("SELECT swe, depth FROM measurements WHERE survey_id = ", old_surveys$survey_id[i], " AND exclude_flag IS FALSE AND (swe IS NOT NULL OR depth IS NOT NULL);"))
        if (nrow(meas) > 0) {
          meas <- data.frame(datetime = old_surveys$survey_date[i],
                             target_datetime = old_surveys$target_date[i],
                             parameter_id = c(swe_paramid, depth_paramid),
                             survey_id = old_surveys$survey_id[i],
                             result = c(mean(meas$swe, na.rm = TRUE), mean(meas$depth, na.rm = TRUE))
          )
          old_meas <- rbind(old_meas, meas)
        }
      }
      if (nrow(old_meas) > 0) {
        adjust <- TRUE
      }
    }
  }
  
  
  # adjust old data if needed ################################
  # At this point if adjust is TRUE it means that there are new overlapping data points (if just appending new data) or that we're synchronizing (going back in time and refreshing)
  if (adjust) {
    # Get all overlapping surveys from the new location
    query <- paste0("SELECT survey_id, survey_date, target_date FROM surveys WHERE location = '", location, "' AND survey_date IN ('", paste(old_surveys$survey_date, collapse = "', '"), "');")
    adj_surveys <- DBI::dbGetQuery(snowCon, query)
    
    # Further limit results based on adjust_start and adjust_end
    if (!is.null(adjust_start)) {
      adj_surveys <- adj_surveys[adj_surveys$survey_date >= adjust_start, ]
    }
    if (!is.null(adjust_end)) {
      adj_surveys <- adj_surveys[adj_surveys$survey_date <= adjust_end, ]
    }

    if (nrow(adj_surveys) > 0) {
      adj_surveys$survey_date <- as.POSIXct(adj_surveys$survey_date, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
      adj_surveys$target_date <- as.POSIXct(adj_surveys$target_date, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
      
      # Get the adjust data points
      adj_meas <- data.frame()
      for (i in 1:nrow(adj_surveys)) {
        #Get the measurements for each survey
        meas <- DBI::dbGetQuery(snowCon, paste0("SELECT swe, depth FROM measurements WHERE survey_id = ", adj_surveys$survey_id[i], " AND exclude_flag IS FALSE AND (swe IS NOT NULL OR depth IS NOT NULL);"))
        if (nrow(meas) > 0) {
          meas <- data.frame(datetime = adj_surveys$survey_date[i],
                             target_datetime = adj_surveys$target_date[i],
                             parameter_id = c(swe_paramid, depth_paramid),
                             result = c(mean(meas$swe, na.rm = TRUE), mean(meas$depth, na.rm = TRUE))
          )
          adj_meas <- rbind(adj_meas, meas)
        }
      }
      common_datetimes <- as.POSIXct(intersect(adj_meas$datetime, old_meas$datetime), tz = "UTC")
      # Calculate the offset as a percentage of the new data, apply to the old data
      # Means of meas are calculated, because the snow DB can have multiple measurements for the sample but these are not transfered over to the aquacache
      offset_swe <- mean(mean(adj_meas$result[adj_meas$datetime %in% common_datetimes & adj_meas$parameter_id == swe_paramid], na.rm = TRUE) / mean(old_meas$result[old_meas$datetime %in% common_datetimes & old_meas$parameter_id == swe_paramid], na.rm = TRUE), na.rm = TRUE)
      offset_depth <- mean(mean(adj_meas$result[adj_meas$datetime %in% common_datetimes & adj_meas$parameter_id == depth_paramid], na.rm = TRUE) / mean(old_meas$result[old_meas$datetime %in% common_datetimes & old_meas$parameter_id == depth_paramid], na.rm = TRUE), na.rm = TRUE)
      # Apply offset to old data
      old_meas[old_meas$parameter_id == swe_paramid, "result"] <- old_meas[old_meas$parameter_id == swe_paramid, "result"] * offset_swe
      old_meas[old_meas$parameter_id == depth_paramid, "result"] <- old_meas[old_meas$parameter_id == depth_paramid, "result"] * offset_depth
      # Discard old data that overlaps with new data
      old_meas <- old_meas[!(old_meas$datetime %in% adj_meas$datetime),]
      # Add in the old_surveys$survey_id by matching on old_meas$datetime = old_surveys$survey_date
      old_meas <- merge(old_meas, old_surveys, by.x = "datetime", by.y = "survey_date")
      
      # adjust the old data in 'result' table of database with the new values, keyed by sample_id
      for (j in unique(old_meas$datetime)) {
        j <- as.POSIXct(j, tz = "UTC")
        # Find the sample_id by matching on old_meas$survey_date[j] = samples.datetime
        adj_sample_id <- DBI::dbGetQuery(con, paste0("SELECT sample_id FROM samples WHERE location_id = '", location_id, "' AND datetime = '", j, "';"))[1,1]
        if (is.na(adj_sample_id)) { # Create a new survey if it doesn't exist (maybe this is the first time the data comes in for that station, or old data has been added)
          df <- data.frame(location_id = location_id,
                           datetime = j,
                           target_datetime = old_meas[old_meas$datetime == j, "target_datetime"][1], # Bring it to noon local time
                           import_source_id = old_meas[old_meas$datetime == j, "survey_id"][1],
                           sample_type = sample_type,
                           owner = sample_owner,
                           contributor = sample_contributor,
                           collection_method = sample_collect_method,
                           media_id = media_id,
                           import_source = "downloadSnowCourse")
          DBI::dbAppendTable(con, "samples", df)
          
          # Fetch the new id
          adj_sample_id <- DBI::dbGetQuery(con, paste0("SELECT sample_id FROM samples WHERE location_id = '", location_id, "' AND datetime = '", j, "';"))[1,1]
        } else { # Update the existing survey and results
          DBI::dbExecute(con, paste0("UPDATE discrete.samples SET note = '", paste0("Sample passed through from a nearby station to form a composite timeseries. Actual sample location = ", old_loc, ", with calculated offsets applied of ", round(offset_swe, 4), " for SWE and ", round(offset_depth, 4), " for depth applied to this sample."), "', import_source_id = '", old_meas[old_meas$datetime == j, "survey_id"][1], "', import_source = 'downloadSnowCourse' WHERE sample_id = ", adj_sample_id, ";"))
          # SWE
          DBI::dbExecute(con, paste0(
            "WITH upd AS (
               UPDATE discrete.results
               SET result = ", old_meas[old_meas$parameter_id == swe_paramid & old_meas$datetime == j, "result"], ",
                   result_type = 1,
                   result_value_type = ", actual_result, ",
                   protocol_method = ", protocol_method, "
               WHERE parameter_id = ", swe_paramid, "
                 AND sample_id = ", adj_sample_id, "
               RETURNING *
             )
             INSERT INTO discrete.results (parameter_id, sample_id, result, result_type, result_value_type, protocol_method)
             SELECT ", swe_paramid, ", ", 
                       adj_sample_id, ", ",
                       old_meas[old_meas$parameter_id == swe_paramid & old_meas$datetime == j, "result"],
                       ", 1, ",
                      actual_result, ", ", 
                      protocol_method, "
             WHERE NOT EXISTS (SELECT 1 FROM upd);"
          ))
          # Depth
          DBI::dbExecute(con, paste0(
            "WITH upd AS (
               UPDATE discrete.results
               SET result = ", old_meas[old_meas$parameter_id == depth_paramid & old_meas$datetime == j, "result"], ",
                   result_type = 1,
                   result_value_type = ", actual_result, ",
                   protocol_method = ", protocol_method, "
               WHERE parameter_id = ", depth_paramid, "
                 AND sample_id = ", adj_sample_id, "
               RETURNING *
             )
             INSERT INTO discrete.results (parameter_id, sample_id, result, result_type, result_value_type, protocol_method)
             SELECT ", depth_paramid, ", ", 
                      adj_sample_id, ", ",
                      old_meas[old_meas$parameter_id == depth_paramid & old_meas$datetime == j, "result"], 
                      ", 1, ",
                      actual_result, ", ", 
                      protocol_method, "
             WHERE NOT EXISTS (SELECT 1 FROM upd);"
          ))
        }
      }
      # Update the sample_series table of aquacache DB with the offset values
      DBI::dbExecute(con, paste0("UPDATE sample_series SET note = 'Compound sample series incorporating measurements from ", old_loc, ". SWE measurements at the old location adjusted using a multiplier of ", round(offset_swe, 4), ", depth with a multiplier of ", round(offset_depth, 4), " calculated from ", length(common_datetimes), " data points. New location measurements take precedence over old for overlap period.' WHERE location_id = '", location_id, "' AND source_fx = 'downloadSnowCourse';"))
      
    } else { # No adjustment is necessary, but the old location data points might still need to be added
      
      for (j in unique(old_meas$datetime)) {
        j <- as.POSIXct(j, tz = "UTC")
        # Find the sample_id by matching on old_meas$survey_date[j] = samples.datetime
        adj_sample_id <- DBI::dbGetQuery(con, paste0("SELECT sample_id FROM samples WHERE location_id = '", location_id, "' AND datetime = '", j, "';"))[1,1]
        if (is.na(adj_sample_id)) { # Create a new survey if it doesn't exist (maybe this is the first time the data comes in for that station, or old data has been added)
          df <- data.frame(location_id = location_id,
                           datetime = j,
                           target_datetime = old_meas[old_meas$datetime == j, "target_datetime"][1], # Bring it to noon local time
                           import_source_id = old_meas[old_meas$datetime == j, "survey_id"][1],
                           sample_type = sample_type,
                           owner = sample_owner,
                           contributor = sample_contributor,
                           collection_method = sample_collect_method,
                           media_id = media_id,
                           import_source = "downloadSnowCourse")
          DBI::dbAppendTable(con, "samples", df)
          
          # Fetch the new id
          adj_sample_id <- DBI::dbGetQuery(con, paste0("SELECT sample_id FROM samples WHERE location_id = '", location_id, "' AND datetime = '", j, "';"))[1,1]
        } else { # Update the existing survey and results
          DBI::dbExecute(con, paste0("UPDATE discrete.samples SET note = '", paste0("Sample passed through from a nearby station to form a composite timeseries. Actual sample location = ", old_loc, ", with no calculated offset applied to this sample."), "', import_source_id = '", old_meas[old_meas$datetime == j, "survey_id"][1], "', import_source = 'downloadSnowCourse' WHERE sample_id = ", adj_sample_id, ";"))
          # SWE
          DBI::dbExecute(con, paste0(
            "WITH upd AS (
               UPDATE discrete.results
               SET result = ", old_meas[old_meas$parameter_id == swe_paramid & old_meas$datetime == j, "result"], ",
                   result_type = 1,
                   result_value_type = ", actual_result, ",
                   protocol_method = ", protocol_method, "
               WHERE parameter_id = ", swe_paramid, "
                 AND sample_id = ", adj_sample_id, "
               RETURNING *
             )
             INSERT INTO discrete.results (parameter_id, sample_id, result, result_type, result_value_type, protocol_method)
             SELECT ", swe_paramid, ", ", 
            adj_sample_id, ", ",
            old_meas[old_meas$parameter_id == swe_paramid & old_meas$datetime == j, "result"],
            ", 1, ",
            actual_result, ", ", 
            protocol_method, "
             WHERE NOT EXISTS (SELECT 1 FROM upd);"
          ))
          # Depth
          DBI::dbExecute(con, paste0(
            "WITH upd AS (
               UPDATE discrete.results
               SET result = ", old_meas[old_meas$parameter_id == depth_paramid & old_meas$datetime == j, "result"], ",
                   result_type = 1,
                   result_value_type = ", actual_result, ",
                   protocol_method = ", protocol_method, "
               WHERE parameter_id = ", depth_paramid, "
                 AND sample_id = ", adj_sample_id, "
               RETURNING *
             )
             INSERT INTO discrete.results (parameter_id, sample_id, result, result_type, result_value_type, protocol_method)
             SELECT ", depth_paramid, ", ", 
            adj_sample_id, ", ",
            old_meas[old_meas$parameter_id == depth_paramid & old_meas$datetime == j, "result"], 
            ", 1, ",
            actual_result, ", ", 
            protocol_method, "
             WHERE NOT EXISTS (SELECT 1 FROM upd);"
          ))
        }
      }
    }
  } # End of adjustment block
    
    
    # Now, finally, get the new data and return it ############################################
    new_surveys <- DBI::dbGetQuery(snowCon, paste0("SELECT survey_id AS import_source_id, location, target_date AS target_datetime, survey_date AS datetime, notes AS note FROM surveys WHERE location = '", location, "' AND survey_date > '", start_date, "';"))
  
  # Some notes are the text 'NA', so we need to convert them to actual NA
  new_surveys$note[new_surveys$note == "NA"] <- NA
    
    if (nrow(new_surveys) == 0) {
      return(list())
    }
    new_surveys$target_datetime <- as.POSIXct(new_surveys$target_datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    new_surveys$datetime <- as.POSIXct(new_surveys$datetime, tz = "UTC") + 68400 # Add 19 hours to get to noon MST (but still in UTC as that's easier to pass to the DB)
    
    ls <- list()
    for (i in 1:nrow(new_surveys)) {
      sample <- new_surveys[i,]
      #Get the measurements for each survey
      meas <- DBI::dbGetQuery(snowCon, paste0("SELECT survey_id, estimate_flag, swe, depth FROM measurements WHERE survey_id = ", new_surveys$import_source_id[i], " AND exclude_flag IS FALSE AND (swe IS NOT NULL OR depth IS NOT NULL);"))
      
      if (nrow(meas) > 0) {
        meas <- data.frame(parameter_id = c(swe_paramid, depth_paramid),
                           result = c(mean(meas$swe, na.rm = TRUE), mean(meas$depth, na.rm = TRUE)),  # Means because the snow DB might return multiple measurements for the sample
                           result_value_type = any(meas$estimate_flag),
                           result_type = 1) # 1 = field observation
        
        # Change estimated or actual values to the database values
        meas$result_value_type[meas$result_value_type] <- estimated_result
        meas$result_value_type[!meas$result_value_type] <- actual_result
        meas$protocol_method <- protocol_method
      } else {
        meas <- data.frame()
      }
      sample$sample_type <- sample_type
      sample$owner <- sample_owner
      sample$contributor <- sample_contributor
      sample$collection_method <- sample_collect_method
      sample$media_id <- media_id
      
      ls[[i]] <- list(sample = sample,
                      results = meas)
    }
    
  return(ls)
}
