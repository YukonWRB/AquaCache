#' Add timeseries to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one or multiple timeseries to the database by adding entries to the timeseries, locations, and datum_conversions tables. See related function [addHydrometTimeseriesTemplate()] for help in formatting the data.frames to pass to `timeseries_df` and `locations_df`.
#'
#' @details
#' You can also add the new timeseries by directly editing the database, but this function ensures that database constraints are respected and will immediately seek to populate the measurements and calculated tables with new information for each timeseries.
#'
#' Additional arguments to pass to the function specified in source_fx should take the form of "{{param1 = arg1}}, {{param2 = 'arg2'}}". The data fetch function will separate out the parameter:argument pairs based on them being within curly brackets.
#'
#' @param timeseries_df A data.frame containing the information necessary to add the timeseries (see details for template).
#' @param locations_df A data.frame containing spatial information related to the individual locations specified in timeseries_df. Only necessary if you are specifying a location code that is NOT already in the database. Function returns an error if you didn't specify a spatial_df when it is necessary. See details for template.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#'
#' @return One or more new entries are created in the table 'timeseries'
#' @export
#' @seealso [addHydrometTimeseriesTemplate()] to see templates for timesries_df and locations_df.

addHydrometTimeseries <- function(timeseries_df, locations_df = NULL, con = hydrometConnect()){

  #Check that every location in the timeseries_df already exists; if they don't, check they've been specified in locations_df
  new_locs <- NULL
  exist_locs <- DBI::dbGetQuery(con, "SELECT location FROM locations")
  if (!all(unique(timeseries_df$location) %in% exist_locs$location)) {
    if (is.null(locations_df)){
      stop("You didn't specify a locations_df, but not all of the locations in your timeseries_df are already in the database. Either double-check your timeseries_df or give me a locations_df from which to add the missing location(s) ", paste(unique(timeseries_df$location)[!(unique(timeseries_df$location) %in% exist_locs)], collapse = ", "), ".")
    } else {
      new_locs <- unique(timeseries_df$location)[!(unique(timeseries_df$location) %in% exist_locs)]
    }
  }

  #Check the names of timeseries_df and locations_df, if it's not null
  if (!all(c("location", "parameter", "unit", "category", "period_type", "param_type", "operator", "network", "public", "source_fx", "source_fx_args", "start_datetime", "note") %in% names(timeseries_df))){
    stop("It looks like you're either missing columns in timeseries_df or that you have a typo. Please review that you have columns named c('location', 'parameter', 'unit', 'category', 'period_type', 'param_type', 'start_datetime', 'operator', 'network', 'public', 'source_fx', 'source_fx_args', 'note'). Use NA to indicate a column with no applicable value.")
  }

  if (!is.null(locations_df)){
    if (!all(c("location", "name", "latitude", "longitude", "datum_id_from", "datum_id_to", "conversion_m", "current", "note") %in% names(locations_df))){
      stop("It looks like you're either missing columns in locations_df or that you have a typo. Please review that you have columns named c('location', 'name', 'latitude', 'longitude', 'datum_id_from', 'datum_id_to', 'conversion_m', 'current', 'note'). Use NA to indicate a column with no applicable value.")
    }
  }


  #Add the location info ###########
  if (!is.null(new_locs)){
    for (i in new_locs){
      DBI::dbWithTransaction(
        con,
        {
          #locations table first
          location <- data.frame(location = unique(locations_df[locations_df$location == i, "location"]),
                                 name = unique(locations_df[locations_df$location == i, "name"]),
                                 latitude = unique(locations_df[locations_df$location == i, "latitude"]),
                                 longitude = unique(locations_df[locations_df$location == i, "longitude"]),
                                 note = locations_df[locations_df$location == i, "note"])
          DBI::dbAppendTable(con, "locations", location)

          DBI::dbExecute(con, "UPDATE locations SET point = ST_SetSRID(ST_MakePoint(longitude, latitude), 4269) WHERE point IS NULL;")


          #now datums table
          datum <- data.frame(location = locations_df[locations_df$location == i, "location"],
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


  #Add the timeseries ########
  for (i in 1:nrow(timeseries_df)){
    tryCatch({
      add <- timeseries_df[i, -which(names(timeseries_df) == "start_datetime")]
      start_datetime <- timeseries_df[i, "start_datetime"]
      tryCatch({
        DBI::dbAppendTable(con, "timeseries", add) #This is in the tryCatch because the timeseries might already have been added by update_hydat, which searches for level + flow for each location, or by a failed attempt at adding earlier on.
        message("Added a new entry to the timeseries table for location ", add$location, ", parameter ", add$parameter, ", category ", add$category, ", param_type ", add$param_type, ", and period_type ", add$period_type, ".")
      }, error = function (e) {
        message("It looks like the timeseries has already been added. This likely happened because this function already called function update_hydat on a flow or level timeseries of the Water Survey of Canada and this function automatically looked for the corresponding level/flow timeseries, or because of an earlier failed attempt to add the timeseries.")
      })
      new_tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", add$location, "' AND parameter = '", add$parameter, "' AND category = '", add$category, "' AND period_type = '", add$period_type, "';"))[1,1]
      loc <- add$location
      parameter <- add$parameter
      source_fx <- add$source_fx
      source_fx_args <- add$source_fx_args
      param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = '", parameter, "' AND source_fx = '", source_fx, "';"))[1,1]
      period_type <- add$period_type

      if (!is.na(add$source_fx)){
        args_list <- list(location = loc, param_code = param_code, start_datetime = timeseries_df[i, "start_datetime"])
        if (!is.na(source_fx_args)){#add some arguments if they are specified
          args <- strsplit(source_fx_args, "\\},\\s*\\{")
          pairs <- lapply(args, function(pair){
            gsub("[{}]", "", pair)
          })
          pairs <- lapply(pairs, function(pair){
            gsub("\"", "", pair)
          })
          pairs <- lapply(pairs, function(pair){
            gsub("'", "", pair)
          })
          pairs <- strsplit(unlist(pairs), "=")
          pairs <- lapply(pairs, function(pair){
            trimws(pair)
          })
          for (j in 1:length(pairs)){
            args_list[[pairs[[j]][1]]] <- pairs[[j]][[2]]
          }
        }

        ts <- do.call(source_fx, args_list) #Get the data using the args_list
        ts <- ts[!is.na(ts$value) , ]

        if (add$category == "continuous"){
          if (nrow(ts) > 0){
            if (period_type == "instantaneous"){ #Period is always 0 for instantaneous data
              ts$period <- "00:00:00"
              no_period <- data.frame() # Created here for use later
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
                    change <- data.frame(datetime = ts$datetime[j-3],
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
              if (nrow(changes) > 0){
                for (j in 1:nrow(changes)){
                  days <- floor(changes$period[j] / 24)
                  remaining_hours <- changes$period[j] %% 24
                  minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
                  seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
                  ts[ts$datetime == changes$datetime[j], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
                }
                #carry non-na's forward and backwards, if applicable
                ts$period <- zoo::na.locf(zoo::na.locf(ts$period, na.rm = FALSE), fromLast=TRUE)

              } else { #In this case there were too few measurements to conclusively determine a period, so set to NA. They can be calculated once more data gets added.
                ts$period <- NA
              }
            } else { #Check to make sure that the supplied period can actually be coerced to a period
              check <- lubridate::period(unique(ts$period))
              if (NA %in% check){ #Can't be coerced, so set to NA
                ts$period <- NA
              }
            }
            ts$timeseries_id <- new_tsid
            ts$imputed <- FALSE

            tryCatch({
              DBI::dbAppendTable(con, "measurements_continuous", ts)
              DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(ts$datetime), "', end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", new_tsid, ";"))
              message("Success! Added new realtime data for ", add$location, " and parameter ", add$parameter, ".")
            }, error = function(e) {
              warning("Unable to add new values to the measurements_continuous table for row ", i, ". It looks like there is already data there for this location/parameter/period_type/categeory combination.")
            })
            tryCatch({
              calculate_stats(timeseries_id = new_tsid)
              message("Success! Calculated daily means and statistics for ", add$location, " and parameter ", add$parameter, ".")
            }, error = function(e){
              warning("Unable to calculate daily means and statistics for ", add$location, " and parameter ", add$parameter, ".")
            })
          } else { #There is no data to associate with this timeseries. Delete it and see if the location should also be deleted.
            DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
            loc_necessary <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", loc, "'"))
            if (nrow(loc_necessary) == 0){
              DBI::dbExecute(con, paste0("DELETE FROM locations WHERE timeseries_id = ", new_tsid, ";"))
            }
            warning("There was no data found for row ", i, " using the source_fx you specified. The corresponding timeseries_id has been deleted from the timeseries table, while the location was deleted if not referenced by other timeseries in the database.")
          }
          if (add$operator == "WSC"){
            suppressMessages(update_hydat(timeseries_id = new_tsid, force_update = TRUE))
          }
        } else { #Add the non-continuous data
          if (nrow(ts) > 0){
            ts$timeseries_id <- new_tsid
            tryCatch({
              DBI::dbAppendTable(con, "measurements_discrete", ts)
              DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", min(ts$datetime), "', end_datetime = '", max(ts$datetime),"', last_new_data = '", .POSIXct(Sys.time(), "UTC"), "' WHERE timeseries_id = ", new_tsid, ";"))
              message("Success! Added new discrete data for ", add$location, " and parameter ", add$parameter, ".")
            }, error = function(e) {
              warning("Unable to add new values to the measurements_discrete table for row ", i, ". It looks like there is already data there for this location/parameter/period_type/categeory combination.")
            })
          } else { #There is no data to associate with this timeseries. Delete it and see if the location should also be deleted.
            DBI::dbExecute(con, paste0("DELETE FROM timeseries WHERE timeseries_id = ", new_tsid, ";"))
            loc_necessary <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", loc, "'"))
            if (nrow(loc_necessary) == 0){
              DBI::dbExecute(con, paste0("DELETE FROM locations WHERE timeseries_id = ", new_tsid, ";"))
            }
            warning("There was no data found for row ", i, " using the source_fx you specified. The corresponding timeseries_id has been deleted from the timeseries table, while the location was deleted if not referenced by other timeseries in the database.")
          }
        } #End of loop adding discrete data
      } else {
        warning("You didn't specify a source_fx. No data was added to the measurements_continuous or measurements_discrete table, so make sure you go and add that data ASAP. If you made a mistake delete the timeseries from the timeseries table and restart. The timeseries ID for this new entry is ", new_tsid)
      }
    }, error = function(e) {
      warning("Failed to add new data for row number ", i, " in the provided timeseries_df data.frame.")
    })
  } #End of loop iterating over each new  timeseries entry


  #TODO: calculate or find polygons for any locations that have flow or level. Modify function getWatersheds.

}

