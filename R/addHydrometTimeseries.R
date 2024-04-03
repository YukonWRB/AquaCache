#' Add timeseries to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one or multiple timeseries to the database by adding entries to the timeseries, locations, and datum_conversions tables. See related function [addHydrometTimeseriesTemplate()] for help in formatting the data.frames to pass to `timeseries_df` and `locations_df`. To add an image series see [addHydrometImageSeries()], for raster series see [addHydrometRasterSeries()]. For one-off images use [insertHydrometImage()] and for rasters [insertHydrometRaster()]. For documents use [insertHydrometDocument()].
#'
#' @details
#' You can also add the new timeseries by directly editing the database, but this function ensures that database constraints are respected and will immediately seek to populate the measurements and calculated tables with new information for each timeseries.
#'
#' Additional arguments to pass to the function specified in source_fx should take the form of "\{param1 = arg1\}, \{param2 = 'arg2'\}". The data fetch function will separate out the parameter:argument pairs based on them being within curly brackets.
#'
#' @param timeseries_df A data.frame containing the information necessary to add the timeseries (see details for template).
#' @param locations_df A data.frame containing spatial information related to the individual locations specified in timeseries_df. Only necessary if you are specifying a location code that is NOT already in the database. Function returns an error if you didn't specify a spatial_df when it is necessary. See details for template.
#' @param settings_df A data.frame containing new entries for the 'settings' table. Only necessary if you are asking for a new combination of source_fx, parameter, period_type, and record_rate. Function will double check that required entries exist.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return One or more new entries are created in the table 'timeseries'
#' @export
#' @seealso [addHydrometTimeseriesTemplate()] to see templates for timesries_df and locations_df.

addHydrometTimeseries <- function(timeseries_df, locations_df = NULL, settings_df = NULL, con = hydrometConnect()) {

  
  #TODO: add a way to pass new parameters using params_df
  
  #Check the names of timeseries_df and locations_df, if it's not null
  if (!all(c("location", "parameter", "unit", "category", "period_type", "record_rate", "param_type", "public", "public_delay", "source_fx", "source_fx_args", "start_datetime", "note") %in% names(timeseries_df))) {
    stop("It looks like you're either missing columns in timeseries_df or that you have a typo. Please review that you have columns named c('location', 'parameter', 'unit', 'category', 'period_type', 'record_rate', 'param_type', 'start_datetime', 'public', 'public_delay', source_fx', 'source_fx_args', 'note'). Use NA to indicate a column with no applicable value.")
  }
  
  if (!is.null(locations_df)) {
    if (!all(c("location", "name", "name_fr", "latitude", "longitude", "datum_id_from", "datum_id_to", "conversion_m", "current", "note", "contact", "network", "project") %in% names(locations_df))) {
      stop("It looks like you're either missing columns in locations_df or that you have a typo. Please review that you have columns named c('location', 'name', 'name_fr', 'latitude', 'longitude', 'datum_id_from', 'datum_id_to', 'conversion_m', 'current', 'note', 'contact', 'network', 'project'). Use NA to indicate a column with no applicable value.")
    }
  }
  
  #Check that every location in the timeseries_df already exists; if they don't, check they've been specified in locations_df
  new_locs <- NULL
  exist_locs <- DBI::dbGetQuery(con, "SELECT location FROM locations")[,1]
  if (!all(unique(timeseries_df$location) %in% exist_locs)) {
    if (is.null(locations_df)) {
      missing <- timeseries_df$location[!(timeseries_df$location %in% exist_locs)]
      stop("You didn't specify a locations_df, but not all of the locations in your timeseries_df are already in the database. Either double-check your timeseries_df or give me a locations_df from which to add the missing location(s) ", paste(missing, collapse = ", "), ".")
    } else {
      # Check that locations in locations_df match those in timeseries_df
      if (!all(unique(locations_df$location) %in% unique(timeseries_df$location))) {
        stop("The locations in your locations_df don't match those in your timeseries_df. Please double-check that they match.")
      }
      new_locs <- unique(timeseries_df$location)[!(unique(timeseries_df$location) %in% exist_locs)]
    }
  }


  #modify some columns
  timeseries_df$parameter <- tolower(timeseries_df$parameter)
  timeseries_df$category <- tolower(timeseries_df$category)
  timeseries_df$period_type <- tolower(timeseries_df$period_type)
  timeseries_df$param_type <- tolower(timeseries_df$param_type)
  timeseries_df$record_rate <- as.character(timeseries_df$record_rate)

  # Check that cols in the timeseries_df meets constraints
  if (!all(timeseries_df$category %in% c("discrete", "continuous"))) {
    stop("One of the rows in your timeseries_df has a disallowed value: column 'category' can only be one of 'discrete' or 'continuous'.")
  }
  if (!all(timeseries_df$param_type %in% c('surface water', 'ground water', 'waste water', 'waste water effluent', 'seep', 'drinking water', 'meteorological'))) {
    stop("One of the rows in your timeseries_df has a disallowed value: column 'param_type' can only be one of 'surface water', 'ground water', 'waste water', 'waste water effluent', 'seep', 'drinking water', 'meteorological'.")
  }
  if (!all(timeseries_df$period_type %in% c('instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'))) {
    stop("One of the rows in your timeseries_df has a disallowed value: column 'period_type' can only be one of 'instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'.")
  }
  #Check column public_delay can be converted to a period
  check <- lubridate::period(timeseries_df[!is.na(timeseries_df$public_delay), "public_delay"])
  if (NA %in% check) {
    stop("At least one of your entries to timeseries_df in column 'public_delay' is not NA and cannot be coerced to a duration Use ISO8601 format only, or NA for no entry.")
  }

  sub.discrete <- timeseries_df[timeseries_df$category == "discrete", ]
  if (!all(is.na(sub.discrete$record_rate))) {
    stop("One of the rows in your timeseries_df has a disallowed value: column 'record_rate' can only be NULL where column 'category' is 'discrete'.")
  }
  sub.cont <- timeseries_df[timeseries_df$category == "continuous", ]
  if (!all(sub.cont$record_rate %in% c('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))) {
    stop("One of the rows in your timeseries_df has a disallowed value: column 'record_rate' can only be one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year' where column 'category' is 'continuous'.")
  }

  if (!is.null(settings_df)) {
    if (!all(names(settings_df) %in% c("source_fx", "parameter", "period_type", "record_rate", "remote_param_name"))) {
      stop("You provided a data.frame for 'settings_df' with incorrect names. Refer to function addHydrometTimeseriesTemplate or to this function's help.")
    }
    #modify some col names
    settings_df$parameter <- tolower(settings_df$parameter)
    settings_df$period_type <- tolower(settings_df$period_type)
    settings_df$record_rate <- as.character(settings_df$record_rate) #to align with what comes out of the DB
  }
  
  if (!is.null(locations_df)) {
    # Check that the networks already exists. If not, stop and alert user to add it first. #########################
    new_networks <- unique(locations_df$network)
    exist_networks <- DBI::dbGetQuery(con, "SELECT name FROM networks")[,1]
    if (!all(new_networks %in% exist_networks)) {
      missing <- new_networks[!(new_networks %in% exist_networks)]
      stop("Not all of the networks in your locations_df are already in the database. Please add the following network(s) first: ", paste(missing, collapse = ", "), ", or use one of the existing networks: ", paste(exist_networks$name, collapse = ", "), ".")
    }
    # Check that the projects already exists. If not, stop and alert user to add it first. #########################
    new_projects <- unique(locations_df$project)
    if (!is.na(new_projects)) {
      exist_projects <- DBI::dbGetQuery(con, "SELECT name FROM projects")[,1]
      if (!all(new_projects %in% exist_projects)) {
        missing <- new_projects[!(new_projects %in% exist_projects)]
        stop("Not all of the projects in your locations_df are already in the database. Please add the following project(s) first: ", paste(missing, collapse = ", "), ", or use one of the existing projects: ", paste(exist_projects$name, collapse = ", "), ".")
      }
    }
  }
  
  

  # Check that the proper entries exist in the settings table. Make entry to DB if necessary/possible ################
  for (i in 1:nrow(timeseries_df)) {
    source_fx <- timeseries_df[i, "source_fx"]
    record_rate <- timeseries_df[i, "record_rate"]
    parameter <- timeseries_df[i, "parameter"]
    param_code <- DBI::dbGetQuery(con, paste0("SELECT param_code FROM parameters WHERE param_name = '", parameter, "';"))[,1]
    period_type <- timeseries_df[i, "period_type"]

    if (is.na(record_rate)) {
      setting <- DBI::dbGetQuery(con, paste0("SELECT * FROM settings WHERE source_fx = '", source_fx, "' AND record_rate IS NULL AND parameter = ", param_code, " AND period_type = '", period_type, "';"))
    } else {
      setting <- DBI::dbGetQuery(con, paste0("SELECT * FROM settings WHERE source_fx = '", source_fx, "' AND record_rate = '", record_rate, "' AND parameter = ", param_code, " AND period_type = '", period_type, "';"))
    }
    if (!is.null(settings_df)) { # A df was provided with settings info
      if (nrow(setting) == 1) { # There already is a setting in the DB for that combination, so do nothing
      } else if (nrow(setting) == 0) { # A df was provided, and there is no existing entry for this combination. Make a new entry if possible
        if (is.na(record_rate)) {
          sub.settings_df <- settings_df[settings_df$source_fx == source_fx & is.na(settings_df$record_rate) & settings_df$parameter == parameter & settings_df$period_type == period_type, ]
        } else {
          sub.settings_df <- settings_df[settings_df$source_fx == source_fx & settings_df$record_rate == record_rate & settings_df$parameter == parameter & settings_df$period_type == period_type, ]
        }
        if (row.names(sub.settings_df) == "NA") {
          sub.settings_df <- data.frame()
        }
        if (nrow(sub.settings_df) == 1) {
          # Check if the parameter exists in the parameters table already
          if (nrow(DBI::dbGetQuery(con, paste0("SELECT * FROM parameters WHERE param_name = '", parameter, "';"))) == 0) {
            stop("The parameter '", parameter, "' does not exist in the parameters table. Please add it first using the function parameter param_df.")
          } else {
            # Get the param_code for the parameter and sub it in to sub.settings_df
            param_code <- DBI::dbGetQuery(con, paste0("SELECT param_code FROM parameters WHERE param_name = '", parameter, "';"))[,1]
            sub.settings_df$parameter <- param_code
          }
          DBI::dbAppendTable(con, "settings", sub.settings_df)
        } else if (nrow(sub.settings_df) > 1) {
          DBI::dbAppendTable(con, "settings", sub.settings_df[1, ])
        } else {
          stop("There is no entry in the 'settings' database table corresponding to row ", i, " of timeseries_df, and no valid entry in settings_df to use for adding the required settings. Please review your input parameters.")
        }
      }
    } else { #no df was provided
      if (nrow(setting) == 0) {
        stop("There is no existing entry in the 'settings' table for row ", i, " of the supplied timeseries_df. Please provide a data.frame for the settings_df parameter to address this.")
      }
    }
  }

  #Add the location, networks, projects, and datums info ###################################################################
  if (!is.null(new_locs)) {
    for (i in new_locs) {
      DBI::dbWithTransaction(
        con,
        {
          #vectors table first
          point <- data.frame("feature_name" = locations_df[locations_df$location == i, "location"],
                              "description" = locations_df[locations_df$location == i, "name"],
                              "latitude" = locations_df[locations_df$location == i, "latitude"],
                              "longitude" = locations_df[locations_df$location == i, "longitude"])
          point <- terra::vect(point, geom = c("longitude", "latitude"), crs = "epsg:4269")

          insertHydrometVector(point, "Locations", feature_name_col = "feature_name", description_col = "description")
          geom_id <- DBI::dbGetQuery(con, paste0("SELECT geom_id FROM vectors WHERE layer_name = 'Locations' AND feature_name = '", locations_df[locations_df$location == i, "location"], "';"))


          #locations table second
          location <- data.frame(location = unique(locations_df[locations_df$location == i, "location"]),
                                 name = unique(locations_df[locations_df$location == i, "name"]),
                                 name_fr = unique(locations_df[locations_df$location == i, "name_fr"]),
                                 latitude = unique(locations_df[locations_df$location == i, "latitude"]),
                                 longitude = unique(locations_df[locations_df$location == i, "longitude"]),
                                 note = locations_df[locations_df$location == i, "note"],
                                 contact = locations_df[locations_df$location == i, "contact"],
                                 geom_id = geom_id)
          DBI::dbAppendTable(con, "locations", location)
          
          location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", unique(locations_df[locations_df$location == i, "location"]), "';"))[1,1]
          
          # networks table third
          if (!is.na(locations_df[locations_df$location == i, "network"])) {
            network_id <- DBI::dbGetQuery(con, paste0("SELECT network_id FROM networks WHERE name = '", unique(locations_df[locations_df$location == i, "network"]), "';"))[1,1]
            tbl <- data.frame(location_id = location_id, network_id = network_id)
            DBI::dbAppendTable(con, "locations_networks", tbl)
          }
          
          # projects table fourth
          if (!is.na(locations_df[locations_df$location == i, "project"])) {
            project_id <- DBI::dbGetQuery(con, paste0("SELECT project_id FROM projects WHERE name = '", unique(locations_df[locations_df$location == i, "project"]), "';"))[1,1]
            tbl <- data.frame(location_id = location_id, project_id = project_id)
            DBI::dbAppendTable(con, "locations_projects", tbl)
          }
          

          location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", unique(locations_df[locations_df$location == i, "location"]), "';"))[1,1]

          #datums table fifth
          datum <- data.frame(location_id = location_id,
                              datum_id_from = locations_df[locations_df$location == i, "datum_id_from"],
                              datum_id_to = locations_df[locations_df$location == i, "datum_id_to"],
                              conversion_m = locations_df[locations_df$location == i, "conversion_m"],
                              current = locations_df[locations_df$location == i, "current"])
          DBI::dbAppendTable(con, "datum_conversions", datum)
          message("Added a new entry to the locations table for location ", i, ".")
        }
      ) #End of dbWithTransaction
    }
  }


  #Add the timeseries #######################################################################################################
  timeseries_df$location_id <- NA
  failed_rows <- c()
  for (i in 1:nrow(timeseries_df)) {
    timeseries_df[i, "location_id"] <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", timeseries_df[i, "location"], "';"))
    tryCatch({
      add <- timeseries_df[i, -which(names(timeseries_df) == "start_datetime")]
      
      loc <- add$location
      param_code <- DBI::dbGetQuery(con, paste0("SELECT param_code FROM parameters WHERE param_name = '", add$parameter, "';"))[,1]
      source_fx <- add$source_fx
      period_type <- add$period_type
      record_rate <- add$record_rate
      source_fx_args <- add$source_fx_args
      param_name <- add$parameter
      add$parameter <- param_code
      tryCatch({
        DBI::dbAppendTable(con, "timeseries", add) #This is in the tryCatch because the timeseries might already have been added by update_hydat, which searches for level + flow for each location, or by a failed attempt at adding earlier on.
        message("Added a new entry to the timeseries table for location ", add$location, ", parameter ", param_name, ", category ", add$category, ", param_type ", add$param_type, ", and period_type ", add$period_type, ".")
      }, error = function(e) {
        message("It looks like the timeseries has already been added. This likely happened because this function already called function update_hydat on a flow or level timeseries of the Water Survey of Canada and this function automatically looked for the corresponding level/flow timeseries, or because of an earlier failed attempt to add the timeseries.")
      })
      
      new_tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", add$location, "' AND parameter = ", param_code, " AND category = '", add$category, "' AND period_type = '", add$period_type, "' AND record_rate = '", add$record_rate, "';"))[1,1]

      if (is.na(record_rate)) {
        param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = ", param_code, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate IS NULL;"))[1,1]
      } else {
        param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = ", param_code, " AND source_fx = '", source_fx, "' AND period_type = '", period_type, "' AND record_rate = '", record_rate, "';"))[1,1]
      }

      if (!is.na(add$source_fx)) {
        args_list <- list(location = loc, param_code = param_code, start_datetime = timeseries_df[i, "start_datetime"])
        if (!is.na(source_fx_args)) {#add some arguments if they are specified
          args <- strsplit(source_fx_args, "\\},\\s*\\{")
          pairs <- lapply(args, function(pair) {
            gsub("[{}]", "", pair)
          })
          pairs <- lapply(pairs, function(pair) {
            gsub("\"", "", pair)
          })
          pairs <- lapply(pairs, function(pair) {
            gsub("'", "", pair)
          })
          pairs <- strsplit(unlist(pairs), "=")
          pairs <- lapply(pairs, function(pair) {
            trimws(pair)
          })
          for (j in 1:length(pairs)) {
            args_list[[pairs[[j]][1]]] <- pairs[[j]][[2]]
          }
        }

        ts <- do.call(source_fx, args_list) #Get the data using the args_list
        ts <- ts[!is.na(ts$value) , ]

        if (add$category == "continuous") {
          if (nrow(ts) > 0) {
            if (period_type == "instantaneous") { #Period is always 0 for instantaneous data
              ts$period <- "00:00:00"
            } else if ((period_type != "instantaneous") & !("period" %in% names(ts))) { #period_types of mean, median, min, max should all have a period
              ts <- ts[order(ts$datetime) ,] #Sort ascending
              diffs <- as.numeric(diff(ts$datetime), units = "hours")
              smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
              # Initialize variables to track changes
              consecutive_count <- 0
              changes <- data.frame()
              last_diff <- 0
              for (j in 1:length(smoothed_diffs)) {
                if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] < 25 && smoothed_diffs[j] != last_diff) { # Check if smoothed interval is less than threshold, which is set to more than a whole day (greatest interval possible is 24 hours) as well as not the same as the last recorded diff
                  consecutive_count <- consecutive_count + 1
                  if (consecutive_count == 3) { # At three consecutive new measurements it's starting to look like a pattern
                    last_diff <- smoothed_diffs[j]
                    change <- data.frame(datetime = ts$datetime[j - 3],
                                         period = last_diff)
                    changes <- rbind(changes, change)
                    consecutive_count <- 0
                  }
                } else {
                  consecutive_count <- 0
                }
              }
              # Calculate the duration in days, hours, minutes, and seconds and assign to the right location in ts
              ts$period <- NA
              if (nrow(changes) > 0) {
                for (j in 1:nrow(changes)) {
                  days <- floor(changes$period[j] / 24)
                  remaining_hours <- changes$period[j] %% 24
                  minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
                  seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
                  ts[ts$datetime == changes$datetime[j], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
                }
                #carry non-na's forward and backwards, if applicable
                ts$period <- zoo::na.locf(zoo::na.locf(ts$period, na.rm = FALSE), fromLast = TRUE)

              } else { #In this case there were too few measurements to conclusively determine a period, so set to NA. They can be calculated once more data gets added.
                ts$period <- NA
              }
            } else { #Check to make sure that the supplied period can actually be coerced to a period
              check <- lubridate::period(unique(ts$period))
              if (NA %in% check) { #Can't be coerced, so set to NA
                ts$period <- NA
              }
            }
            ts$timeseries_id <- new_tsid
            ts$imputed <- FALSE

            tryCatch({
              DBI::dbAppendTable(con, "measurements_continuous", ts)
              DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(ts$datetime), "', end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", new_tsid, ";"))
              message("Success! Added new realtime data for ", add$location, " and parameter ", param_name, ".")
            }, error = function(e) {
              warning("Unable to add new values to the measurements_continuous table for row ", i, ". It looks like there is already data there for this location/parameter/period_type/categeory combination.")
            })
            if ((add$source_fx == "downloadWSC") & param_name %in% c("water level", "water flow")) {
              message("Adding historical data from HYDAT database")
              suppressMessages(update_hydat(timeseries_id = new_tsid, force_update = TRUE))
            }
            tryCatch({
              if (add$record_rate %in% c('1 day', '< 1 day')) {
                calculate_stats(timeseries_id = new_tsid, con = con, start_recalc = NULL)
              message("Success! Calculated daily means and statistics for ", add$location, " and parameter ", param_name, ".")
              } else {
                message("Not calculating daily statistics for ", add$location, " and parameter ", param_name, " as recording rate is greater than 1 day.")
              }
            }, error = function(e) {
              message("Unable to calculate daily means and statistics for ", add$location, " and parameter ", param_name, ".")
            }, warning = function(e) {
              message("May have failed to calculate daily means and statistics for ", add$location, " and parameter ", param_name, ".")
            })
          } else { #There is no data to associated with this timeseries. Delete it and see if the location should also be deleted.
            failed_rows <- c(failed_rows, i)
            DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
            loc_necessary <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", loc, "'"))
            # see if the location is referenced by other rows in timeseries_df that are not also excluded by failed_rows. If so, don't delete the location.
            sub.tsdf <- timeseries_df[-failed_rows,]
            loc_elsewhere <- nrow(sub.tsdf[sub.tsdf$location == loc,])
            if (nrow(loc_necessary) == 0 && loc_elsewhere == 0) {
              DBI::dbExecute(con, paste0("DELETE FROM locations WHERE location = '", loc, "';"))
            }
            message("There was no data found for row ", i, " using the source_fx you specified. The corresponding timeseries_id has been deleted from the timeseries table, while the location was deleted if not referenced by other timeseries in the database.")
            next
          }
        } else { #Add the non-continuous data
          if (nrow(ts) > 0) {
            ts$timeseries_id <- new_tsid
            tryCatch({
              DBI::dbAppendTable(con, "measurements_discrete", ts)
              DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(ts$datetime), "', end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", new_tsid, ";"))
              message("Success! Added new discrete data for ", add$location, " and parameter ", param_name, ".")
            }, error = function(e) {
              message("Unable to add new values to the measurements_discrete table for row ", i, ". It looks like there is already data there for this location/parameter/period_type/categeory combination.")
            })
          } else { #There is no data to associate with this timeseries. Delete it and see if the location should also be deleted.
            failed_rows <- c(failed_rows, i)
            DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
            loc_necessary <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", loc, "'"))
            # see if the location is referenced by other rows in timeseries_df that are not also excluded by failed_rows. If so, don't delete the location.
            sub.tsdf <- timeseries_df[-failed_rows,]
            loc_elsewhere <- nrow(sub.tsdf[sub.tsdf$location == loc,])
            if (nrow(loc_necessary) == 0 && loc_elsewhere == 0) {
              DBI::dbExecute(con, paste0("DELETE FROM locations WHERE location = '", loc, "';"))
            }
            message("There was no data found for row ", i, " using the source_fx you specified. The corresponding timeseries_id has been deleted from the timeseries table, while the location was deleted if not referenced by other timeseries in the database.")
            next
          }
        } #End of loop adding discrete data
      } else {
        message("You didn't specify a source_fx. No data was added to the measurements_continuous or measurements_discrete table, so make sure you go and add that data ASAP. If you made a mistake delete the timeseries from the timeseries table and restart. The timeseries ID for this new entry is ", new_tsid)
      }
    }, error = function(e) {
      warning("Failed to add new data for row number ", i, " in the provided timeseries_df data.frame.")
    })
  
    } #End of loop iterating over each new  timeseries entry


}

