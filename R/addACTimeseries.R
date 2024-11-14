#' Add timeseries to AquaCache database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one or multiple timeseries to the database by adding entries to the timeseries and settings tables. See related function [addACLocation()] for adding a location to which timeseries must be attached). To add an image series see [addACImageSeries()], for raster series see [addACRasterSeries()]. For one-off images use [insertACImage()] and for rasters [insertACRaster()]. For documents use [insertACDocument()].
#'
#' You will be prompted to add locations that don't exist yet if any fall into this category.
#' 
#' @details
#' You can add the new timeseries by directly editing the database, but this function ensures that database constraints are respected and will immediately seek to populate the measurements and calculated tables with new information for each timeseries. For Water Survey of Canada data, this function will also seek out level and flow data from the HYDAT database, downloading or checking for updates to it before use.
#' 
#' If specifying a data.frame for argument 'data', different criteria applies depending on if the timeseries is categorized as continuous or discrete.
#' For continuous data:
#' The data.frame must contain a 'datetime' (POSIXct) OR 'date' (date) column. If specifying 'date' then the data is entered directly to the 'measurements_calculated_daily' table with no entry to measurements_continuous. 'value' (numeric) is also required, and optionally 'owner', 'contributor', 'share_with', 'approval', 'grade', 'qualifier'. Function [addNewContinuous()] will be called to add this data to the database. If source_fx is also specified it will be called to fetch more recent data than that in this data.frame.
#' For discrete data:
#' This is not supported yet.
#'
#' Additional arguments to pass to the function specified in source_fx should take the form of "\{param1 = arg1\}, \{param2 = 'arg2'\}". The data fetch function will separate out the parameter:argument pairs based on them being within curly brackets. Do not deviate from this format!
#'
#' @param df A data.frame containing at least one row and the following columns: start_datetime, location, z, parameter, media, sensor_priority, category, period_type, record_rate, share_with, owner, source_Fx, source_fx_args, note. If this parameter is provided, all other parameters save for `data` must be NA or left as their default values. See notes for the other parameters for more information on each column of df.
#' @param data An optional list of data.frames of length nrow(df) or length(location) containing the data to add to the database. This parameter depends on the `category` of the timeseries being created; see details. If adding multiple timeseries and not all of them need data, include NA elements in the list in the correct locations.
#' @param start_datetime A character or posixct vector of datetimes from which to look for new data, if source_fx is specified. Will be coerced to posixct with a time zone of UTC if not posixct.
#' @param location A numeric vector corresponding to column 'location' of table 'locations'.
#' @param z A numeric vector of elevations in meters for the timeseries observations. This allows for differentiation of things like wind speeds at different heights. Leave as NA if not specified.
#' @param parameter A numeric vector corresponding to column 'parameter_id' of table 'parameters'.
#' @param media A numeric vector corresponding to column 'media_id' of table 'media_types'.
#' @param sensor_priority A numeric vector assigning priority order to assign to this timeseries, default 1. This can allow for storage of multiple identical timeseries taken by different sensors for redundancy.
#' @param category A character vector comprised of either 'discrete' or 'continuous'; classifies the data as either category.
#' @param period_type A character vector describing the measurement type; one of 'instantaneous' (immediate sensor value), 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'.
#' @param record_rate A broad categorization of the rate at which recording takes place, only used for 'continuous' category data.. Select from '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'; set to NA for discrete timeseries.
#' @param share_with A *character* vector of the user group(s) with which to share the timeseries, selected from column 'group_id' of table 'user_groups'. Default is 1. Pass multiple groups as a single string, e.g. "1, 2, 3" or strings, e.g. c("1, 2, 3", "2, 4").
#' @param owner A numeric vector of the owner(s) of the timeseries(s). This can be different from the location owner!
#' @param source_fx The function to use for fetching data to append to the timeseries automatically. If specified, must be one of the 'downloadXXX' functions in this R package.
#' @param source_fx_args Additional arguments to pass to the function(s) specified in parameter 'source_fx'.
#' @param note Text notes to append to the timeseries.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. Leave NULL to use the package default connection and have it closed afterwards automatically.
#'
#' @return One or more new entries are created in the table 'timeseries'
#' @export
#' 
#' @examples
#' \dontrun{
#' #Make a data.frame to pass to the function:
#'   df <- data.frame(start_datetime = "2015-01-01 00:00",
#' location = "09AA-M3",
#' z = c(NA, 3),
#' parameter = c(34, 1154),
#' media = 7,
#' sensor_priority = 1,
#' category = "continuous",
#' period_type = c("sum", "mean"),
#' record_rate = "< 1 day",
#' share_with = "1",
#' owner = 2,
#' source_fx = "downloadAquarius",
#' source_fx_args = NA,
#' note = c("Total precipitation from standpipe, reset every year in the fall.",
#' "Hourly average of wind speeds recorded every minute")
#' )
#' #Add the timeseries using the data.frame
#' addACTimeseries(df)
#' }

addACTimeseries <- function(df = NULL, data = NULL, start_datetime = NA, location = NA, z = NA, parameter = NA, media = NA, sensor_priority = 1, category = NA, period_type = 'instantaneous', record_rate = NA, share_with = "1", owner = NA, source_fx = NA, source_fx_args = NA, note = NA, con = NULL) {
  
  # Testing parameters
  # df <- data.frame(start_datetime = NA,
  #                  location = c("YEC-ML", "YEC-MRM", "YEC-MRW", "YEC-WL"),
  #                  z = NA,
  #                  parameter = c(1165, 1150, 1150, 1165),
  #                  media = 1,
  #                  sensor_priority = 1,
  #                  category = "continuous",
  #                  period_type = c("instantaneous"),
  #                  record_rate = "< 1 day",
  #                  share_with = "2",
  #                  owner = 5,
  #                  source_fx = NA,
  #                  source_fx_args = NA,
  #                  note = NA
  #                  )
  # start_datetime <- NA
  # location <- NA
  # z <- NA
  # parameter <- NA
  # media <- NA
  # sensor_priority <- 1
  # category <- NA
  # period_type <- 'instantaneous'
  # record_rate <- NA
  # share_with <- "1"
  # owner <- NA
  # source_fx <- NA
  # source_fx_args <- NA
  # note <- NA
  
  
  # df = NULL
  # data = NULL
  # start_datetime = "1940-01-01"
  # location = "10ED001"
  # parameter = c(1150, 1165)
  # z = NA
  # media = 1
  # sensor_priority = 1
  # category = 'continuous'
  # period_type = 'instantaneous'
  # record_rate = '< 1 day'
  # share_with = "1"
  # owner = 1
  # source_fx = "downloadWSC"
  # source_fx_args = NA
  # note = NA
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  if (!is.null(data)) {
    if (!inherits(data, "list")) {
      stop("The 'data' parameter must be a list of data.frames if it is provided.")
    }
  }
  
  if (!is.null(df)) {
    # Check that a few of the other parameters are NA
    if (!all(is.na(c(location, start_datetime, z, parameter, media, category, record_rate, owner, source_fx, source_fx_args, note)))) {
      stop("You cannot provide a data.frame and other parameters at the same time.")
    }
    # Check that the data.frame is not empty
    if (nrow(df) == 0) {
      stop("The data.frame provided is empty.")
    }
    
    # Check that there is a column name for each function parameter that is not 'df'
    if (!all(c("start_datetime", "location", "z", "parameter", "media", "sensor_priority", "category", "period_type", "record_rate", "share_with", "owner", "source_fx", "source_fx_args", "note") %in% colnames(df))) {
      stop("The data.frame provided does not contain all the necessary columns.")
    }
    
    # Assign each column of the data.frame to the corresponding function parameter
    start_datetime <- df$start_datetime
    location <- df$location
    z <- df$z
    parameter <- df$parameter
    media <- df$media
    sensor_priority <- df$sensor_priority
    category <- df$category
    period_type <- df$period_type
    record_rate <- df$record_rate
    share_with <- df$share_with
    owner <- df$owner
    source_fx <- df$source_fx
    source_fx_args <- df$source_fx_args
    note <- df$note
  } 
  
  
  # Check on arguments
  
  # Find the longest argument, then make sure all are either NA, length 1, or the same length.
  length <- max(length(start_datetime), length(location), length(z), length(parameter), length(media), length(sensor_priority), length(category), length(period_type), length(record_rate), length(owner), length(source_fx), length(source_fx_args), length(note))
  
  if (!inherits(start_datetime, "POSIXct")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  }
  if (length(start_datetime) == 1 && length > 1) {
    start_datetime <- rep(start_datetime, length)
  }
  if (!is.null(data) & length(data) != length) {
    stop("The 'data' parameter must be a list of data.frames of the same length as the other parameters.")
  }
  
  if (length(share_with) != 1 && length(share_with) != length) {
    stop("share_with must be a single value or a vector of the same length as the other parameters. Please check the function documentation.")
  }

  if (any(is.na(location))) {
    stop("location cannot contain NA values")
  } else {
    if (!inherits(location, "character")) {
      stop("location must be a character vector")
    }
    if (length(location) == 1 && length > 1) {
      location <- rep(location, length)
    }
    #Check that every location in 'location' already exists
    new_locs <- NULL
    exist_locs <- DBI::dbGetQuery(con, "SELECT location FROM locations")[,1]
    if (!all(location %in% exist_locs)) {
      new_locs <- location[!(location %in% exist_locs)]
      stop("Not all of the locations in your timeseries_df are already in the database. Please add the following location(s) first using addACLocation(): ", paste(new_locs, collapse = ", "), ", or use one of the existing locations.")
    }
  }
  
  if (any(!is.na(z))) {
    if (!inherits(z, "numeric")) {
      stop("z must be a numeric vector or left as NA")
    }
  }
  if (length(z) == 1 && length > 1) {
    z <- rep(z, length)
  }
  
  
  if (any(is.na(parameter))) {
    stop("parameter cannot contain NA values")
  } else {
    if (!inherits(parameter, "numeric")) {
      stop("parameter must be a numeric vector")
    }
    if (length(parameter) == 1 && length > 1) {
      parameter <- rep(parameter, length)
    }
    db_param <- DBI::dbGetQuery(con, paste0("SELECT parameter_id FROM parameters WHERE parameter_id IN (", paste(unique(parameter), collapse = ", "), ");"))
    
    if (nrow(db_param) < length(unique(parameter))) {
      stop("At least one of the parameter_ids you specified does not exist in the database.")
    }
  }
  
  if (any(is.na(media))) {
    stop("media cannot contain NA values")
  } else {
    if (!inherits(media, "numeric")) {
      stop("media must be a numeric vector")
    }
    if (length(media) == 1 && length > 1) {
      media <- rep(media, length)
    }
    db_media <- DBI::dbGetQuery(con, paste0("SELECT media_id FROM media_types WHERE media_id IN (", paste(unique(media), collapse = ", "), ");"))
    if (nrow(db_media) < length(unique(media))) {
      stop("At least one of the media_ids you specified does not exist in the database.")
    }
  }
  
  if (any(is.na(sensor_priority))) {
    if (!inherits(sensor_priority, "numeric")) {
      stop("sensor_priority must be a numeric vector")
    }
    sensor_priority[is.na(sensor_priority)] <- 1
  } else if (!inherits(sensor_priority, "numeric")) {
    stop("sensor_priority must be a numeric vector")
  }
  if (length(sensor_priority) == 1 && length > 1) {
    sensor_priority <- rep(sensor_priority, length)
  }
  
  if (any(is.na(category))) {
    stop("category cannot contain NA values")
  } else {
    if (!(all(category %in% c("discrete", "continuous")))) {
      stop("category must only be discrete or continuous")
    }
    if (length(category) == 1 && length > 1) {
      category <- rep(category, length)
    }
  }
  
  if (any(is.na(period_type))) {
    stop("period_type cannot contain NA values")
  } else {
    if (!all(period_type %in% c('instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'))) {
      stop("period_type must be one of 'instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'")
    }
    if (length(period_type) == 1 && length > 1) {
      period_type <- rep(period_type, length)
    }
  }
  
  
  if (any(is.na(record_rate))) {
    stop("record_rate cannot contain NA values")
  } else {
    if (length(record_rate) == 1 && length > 1) {
      record_rate <- rep(record_rate, length)
    }
  }
  
  if ('discrete' %in% category) {
    # check that elements of vector 'category' that are 'discrete' have corresponding NA in 'record_rate'
    if (!all(is.na(record_rate[category == 'discrete']))) {
      stop("record_rate must be NA for discrete timeseries.")
    }
  }
  if ('continuous' %in% category) {
    # check that elements of vector 'category' that are 'continuous' have corresponding non-NA in 'record_rate'
    if (!all(!is.na(record_rate[category == 'continuous']))) {
      stop("record_rate must be specified for continuous timeseries. ")
    }
    # Check that record rate elements that are not NA are in the correct format
    rec_rate_no_na <- record_rate[!is.na(record_rate)]
    if (!all(rec_rate_no_na %in% c('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))) {
      stop("record_rate must be one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year', except for discrete timeseries.")
    }
  }
  
  if (any(is.na(share_with))) {
    if (!inherits(share_with, "character")) {
      stop("share_with must be a character vector (yes, even though it's numbers. Check the function documentation.).")
    }
    share_with[is.na(share_with)] <- 1
  } else if (!inherits(share_with, "character")) {
    stop("share_with must be a character vector (yes, even though it's numbers. Check the function documentation.).")
  }
  if (length(share_with) == 1 && length > 1) {
    share_with <- rep(share_with, length)
  }
  db_share <- DBI::dbGetQuery(con, paste0("SELECT group_id FROM user_groups WHERE group_id IN (", paste(unique(share_with), collapse = ", "), ");"))
  if (nrow(db_share) < length(unique(share_with))) {
    stop("At least one of the share_with groups ids you specified does not exist in the database.")
  }
  
  if (any(is.na(owner))) {
    stop("owner cannot be NA")
  } else if (!inherits(owner, "numeric")) {
    stop("owner must be a numeric vector")
  }
  if (length(owner) == 1 && length > 1) {
    owner <- rep(owner, length)
  }
  db_owner <- DBI::dbGetQuery(con, paste0("SELECT owner_contributor_id FROM owners_contributors WHERE owner_contributor_id IN (", paste(unique(owner), collapse = ", "), ");"))
  if (nrow(db_owner) < length(unique(owner))) {
    stop("At least one of the owners you specified does not exist in the database.")
  }
  
  if (any(is.na(source_fx))) {
    warning("At least one of the source_fx you entered is NA. This will not allow for automatic fetching of new data. The timeseries will be added to the database, but you will need to manually add data unless you also specified a data.frame with this.")
  } 
  source_fx_check <- source_fx[!is.na(source_fx)]
  if (length(source_fx_check) > 0) {
    if (!all(source_fx_check %in% ls("package:AquaCache"))) {
      stop("At least one of the source_fx strings you entered does not exist in the AquaCache package.")
    }
  }
  if (length(source_fx) == 1 && length > 1) {
    source_fx <- rep(source_fx, length)
  }
  
  if (length(source_fx_args) == 1 && length > 1) {
    stop("source_fx_args must be a vector of the same length as the other parameters OR left NA; you cannot leave it as length 1 as this function presumes that arguments are particular to single timeseries and won't replicate to length of other vectors.")
  }
  if (!any(is.na(source_fx_args))) {
    if (!all(grepl("\\{.*?\\}", source_fx_args))) {
      stop("source_fx_args must be in the form of '{param1 = arg1}, {param2 = arg2}', with each parameter:argument pair enclosed in curly brackets.")
    }
  }
  
  if (length(note) == 1 && length > 1) {
    stop("note must be a character vector of the same length as the other parameters OR left NA; you cannot leave it as length 1 as this function presumes that notes are particular to single timeseries and won't replicate to length of other vectors.")
  }
  if (!any(is.na(note))) {
    if (!inherits(note, "character")) {
      stop("note must be a character vector or left NA.")
    }
  }

  # Check that the proper entries exist in the settings database table. Make entry to DB if necessary/possible ################
  for (i in 1:length(location)) {
    sfx <- source_fx[i]
    if (is.na(sfx)) {
      next
    }
    rec_rate <- record_rate[i]
    param <- parameter[i]
    p_type <- period_type[i]
    
    if (is.na(rec_rate)) {
      setting <- DBI::dbGetQuery(con, paste0("SELECT * FROM settings WHERE source_fx = '", sfx, "' AND record_rate IS NULL AND parameter_id = ", param, " AND period_type = '", p_type, "';"))
    } else {
      setting <- DBI::dbGetQuery(con, paste0("SELECT * FROM settings WHERE source_fx = '", sfx, "' AND record_rate = '", rec_rate, "' AND parameter_id = ", param, " AND period_type = '", p_type, "';"))
    }
    if (nrow(setting) == 0) {
      stop("There is no existing entry in the 'settings' table for source_fx ", sfx, ", parameter ", param, ", and period_type ", p_type, ". Please add a corresponding entry to the database table 'settings' to address this and try again. I need to know how to fetch the data for this timeseries!!!")
    }
  }
  
  
  #Add the timeseries #######################################################################################################

  for (i in 1:length(location)) {
    loc_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location[i], "';"))
    tryCatch({
      add <- data.frame(location = location[i],
                        location_id = loc_id,
                        z = z[i],
                        parameter_id = parameter[i],
                        media_id = media[i],
                        sensor_priority = sensor_priority[i],
                        category = category[i],
                        period_type = period_type[i],
                        record_rate = record_rate[i],
                        share_with = paste0("{", paste(share_with[i], collapse = ", "), "}"),
                        owner = owner[i],
                        source_fx = source_fx[i],
                        source_fx_args = source_fx_args[i],
                        note = note[i],
                        end_datetime = if (is.na(source_fx[i])) NA else start_datetime[i] - 1)
      
      tryCatch({
        DBI::dbAppendTable(con, "timeseries", add) #This is in the tryCatch because the timeseries might already have been added by update_hydat, which searches for level + flow for each location, or by a failed attempt at adding earlier on.
        message("Added a new entry to the timeseries table for location ", add$location, ", parameter ", add$parameter_id, ", category ", add$category, ", media_type ", add$media_id, ", and period_type ", add$period_type, ".")
        new_tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", add$location, "' AND parameter_id = ", add$parameter_id, " AND category = '", add$category, "' AND period_type = '", add$period_type, "' AND record_rate = '", add$record_rate, "';"))[1,1]
      }, error = function(e) {
        message("It looks like the timeseries for for location ", add$location, ", parameter ", add$parameter_id, ", category ", add$category, ", media_type ", add$media_id, ", and period_type ", add$period_type, " has already been added. This likely happened because this function already called function update_hydat on a flow or level timeseries of the Water Survey of Canada and automatically looked for the corresponding level/flow timeseries, or because of an earlier failed attempt to add the timeseries Don't worry, I'm still checking for data.")
        new_tsid <<- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", add$location, "' AND parameter_id = ", add$parameter_id, " AND category = '", add$category, "' AND period_type = '", add$period_type, "' AND record_rate = '", add$record_rate, "';"))[1,1]
        # Modify the end_datetime in the DB to be one second before the start_datetime
        DBI::dbExecute(con, paste0("UPDATE timeseries SET end_datetime = '", add$end_datetime, "' WHERE timeseries_id = ", new_tsid, ";"))
      }
      )
      
      if (!is.null(data)) {
        if (!is.na(data[i])) {
          if ('date' %in% colnames(data[[i]])) {
            addNewContinuous(tsid = new_tsid, df = data[[i]], target = 'daily', con = con)
            calculate_stats(timeseries_id = new_tsid, con = con, start_recalc = min(data[[i]]$date))
            DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = (SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ", new_tsid, ") WHERE timeseries_id = ", new_tsid, ";"))
          } else {
            addNewContinuous(tsid = new_tsid, df = data[[i]], target = 'continuous', con = con)
            calculate_stats(timeseries_id = new_tsid, con = con, start_recalc = min(data[[i]]$datetime))
            DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = (SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", new_tsid, ") WHERE timeseries_id = ", new_tsid, ";"))
          }
        }
      }

      
      if (!is.na(source_fx[i])) {
        param_name <- DBI::dbGetQuery(con, paste0("SELECT param_name FROM parameters WHERE parameter_id = ", add$parameter_id, ";"))[1,1]
        
        # Call the relevant 'get' functions to bring in data
        if (add$category == "continuous") {
          remove_after_hydat <- FALSE
          tryCatch({
            DBI::dbExecute(con, paste0("DELETE FROM measurements_continuous WHERE timeseries_id = ", new_tsid, " AND datetime >= '", add$end_datetime, "';"))
            getNewContinuous(con = con, timeseries_id = new_tsid)
            new_start <- DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", new_tsid, ";"))
            DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", new_start$min, "' WHERE timeseries_id = ", new_tsid, ";"))
          }, error = function(e) {
            message("Failed to add new continuous data for location ", add$location, " and parameter ", add$parameter_id, ".")
            if ((add$source_fx == "downloadWSC") & param_name %in% c("water level", "discharge, river/stream")) {
              message("Attempting to add historical data from HYDAT database")
              remove_after_hydat <<- TRUE
            } else {
              DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
              message("Deleted the timeseries entry for location ", add$location, " and parameter ", add$parameter_id, ".")
            }
          })
          
          # Now conditionally check for HYDAT data
          if ((add$source_fx == "downloadWSC") & param_name %in% c("water level", "discharge, river/stream")) {
            message("Adding historical data from HYDAT database")
            suppressMessages(update_hydat(timeseries_id = new_tsid, force_update = TRUE))
            if (remove_after_hydat) {
              # see if anything exists in table measurements_calculated_daily for this timeseries_id. If not, delete the timeseries.
              exist <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM measurements_calculated_daily WHERE timeseries_id = ", new_tsid, ";"))
              if (nrow(exist) == 0) {
                DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
                message("Deleted the timeseries entry for location ", add$location, " and parameter ", add$parameter_id, " as no realtime or daily means data could be found.")
              }
            }
          }
          tryCatch({
            if (add$record_rate %in% c('1 day', '< 1 day')) {
              calculate_stats(timeseries_id = new_tsid, con = con, start_recalc = NULL)
              message("Success! Calculated daily means and statistics for ", add$location, " and parameter ", param_name, ".")
            } else {
              message("Not calculating daily statistics for ", add$location, " and parameter ", param_name, " as recording rate is greater than 1 day.")
            }
          }, error = function(e) {
            message("Unable to calculate daily means and statistics for ", add$location, " and parameter ", param_name, " with message ", e$message, ".")
          }, warning = function(e) {
            message("May have failed to calculate daily means and statistics for ", add$location, " and parameter ", param_name, ".")
          })
        } else { #Add the non-continuous data
          tryCatch({
            DBI::dbExecute(con, paste0("DELETE FROM measurements_discrete WHERE timeseries_id = ", new_tsid, " AND datetime >= '", add$end_datetime, "';"))
            getNewDiscrete(con = con, timeseries_id = new_tsid)
            new_start <- DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_discrete WHERE timeseries_id = ", new_tsid, ";"))
            DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", new_start$min, "' WHERE timeseries_id = ", new_tsid, ";"))
          }, error = function(e) {
            message("Failed to add new discrete data for location ", add$location, " and parameter ", add$parameter_id, ".")
            DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
            message("Deleted the timeseries entry for location ", add$location, " and parameter ", add$parameter_id, ".")
          })
          
        } #End of loop adding discrete data
      } else {
        message("You didn't specify a source_fx. No data was added to the measurements_continuous or measurements_discrete table, so make sure you go and add that data ASAP. If you made a mistake delete the timeseries from the timeseries table and restart. The timeseries ID for this new entry is ", new_tsid)
      }
    }, error = function(e) {
      warning("Failed to add new data for location ", add$location, " and parameter ", add$parameter_id, ". Returned error: ", e$message)
    })
    
  } #End of loop iterating over each new  timeseries entry
  
  
}

