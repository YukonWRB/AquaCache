#' Add timeseries to hydrometric database
#'
#'@description
#'`r lifecycle::badge("stable")`
#'
#' This function facilitates the addition of one or multiple timeseries to the database by adding entries to the timeseries, locations, and datum_conversions tables. See related function [addHydrometTemplate()] for help in formatting the data.frames to pass to `timeseries_df` and `locations_df`.
#'
#' @details
#' You can also add the new timeseries by directly editing the database, but this function ensures that database constraints are respected and will immediately seek to populate the measurements and calculated tables with new information for each timeseries.
#'
#' Additional arguments to pass to the function specified in source_fx should take the form of "{param1 = arg1}, {param2 = 'arg2'}". The data fetch function will separate out the parameter:argument pairs based on them being within curly brackets.
#'
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [hydrometConnect()].
#' @param timeseries_df A data.frame containing the information necessary to add the timeseries (see details for template).
#' @param locations_df A data.frame containing spatial information related to the individual locations specified in timeseries_df. Only necessary if you are specifying a location code that is NOT already in the database. Function returns an error if you didn't specify a spatial_df when it is necessary. See details for template.
#'
#' @return One or more new entries are created in the table 'timeseries'
#' @export
#' @seealso [addHydrometTemplate()] to see templates for timesries_df and locations_df.

addHydrometTimeseries <- function(con = hydrometConnect(silent=TRUE), timeseries_df, locations_df = NULL){

  #Check that every location in the timeseries_df already exists; if they don't, check they've been specified in locations_df
  new_locs <- NULL
  exist_locs <- DBI::dbGetQuery(con, "SELECT location FROM locations")
  if (!all(unique(timeseries_df$location) %in% exist_locs)) {
    if (is.null(locations_df)){
      stop("You didn't specify a locations_df, but not all of the locations in your timeseries_df are already in the database. Either double-check your timeseries_df or give me a locations_df from which to add the missing location(s) ", paste(unique(timeseries_df$location)[!(unique(timeseries_df$location) %in% exist_locs)], collapse = ", "), ".")
    } else {
      new_locs <- unique(timeseries_df$location)[!(unique(timeseries_df$location) %in% exist_locs)]
    }
  }

  #Check the names of timeseries_df and locations_df, if it's not null
  if (!all(c("location", "parameter", "unit", "category", "period_type", "param_type", "operator", "network", "public", "source_fx", "source_fx_args", "start_datetime") %in% names(timeseries_df))){
    stop("It looks like you're either missing columns in timeseries_df or that you have a typo. Please review that you have columns named c('location', 'parameter', 'unit', 'category', 'period_type', 'param_type', 'start_datetime', 'operator', 'network', 'public', 'source_fx', 'source_fx_args'). Use NA to indicate a column with no applicable value.")
  }

  if (!is.null(locations_df)){
    if (!all(c("location", "name", "latitude", "longitude", "datum_id_from", "datum_id_to", "conversion_m", "current") %in% names(locations_df))){
      stop("It looks like you're either missing columns in locations_df or that you have a typo. Please review that you have columns named c('location', 'name', 'latitude', 'longitude', 'datum_id_from', 'datum_id_to', 'conversion_m', 'current'). Use NA to indicate a column with no applicable value.")
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
                                 longitude = unique(locations_df[locations_df$location == i, "longitude"]))
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
          DBI::dbAppendTable(con, "timeseries", add) #This is in the tryCatch because the timeseries might already have been added by update_hydat, which searches for level + flow for each location.
          message("Added a new entry to the timeseries table for location ", add$location, " and parameter ", add$parameter, ".")
        }, error = function (e) {
          message("It looks like the timeseries has already been added. This likely happened because this function already called function update_hydat on a flow or level timeseries of the Water Survey of Canada, and this function automatically looked for the corresponding level/flow timeseries.")
        })
        new_tsid <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", add$location, "' AND parameter = '", add$parameter, "' AND category = '", add$category, "' AND period_type = '", add$period_type, "';"))[1,1]
        loc <- add$location
        parameter <- add$parameter
        source_fx <- add$source_fx
        source_fx_args <- add$source_fx_args
        param_code <- DBI::dbGetQuery(con, paste0("SELECT remote_param_name FROM settings WHERE parameter = '", parameter, "' AND source_fx = '", source_fx, "';"))[1,1]

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

        if (add$category == "continuous"){
          if (nrow(ts) > 0){
            #TODO: right now the period is not being calculated here. Does it need to, or does calculate_stats or synchronizeContinuous do it already?
            ts$period <- "00:00:00"
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
          }
          if (add$operator == "WSC"){
            suppressMessages(update_hydat(timeseries_id = new_tsid, force_update = TRUE))
          }
        } else { #Add the non-continuous data
          warning("This function is not yet set up to deal with 'discrete' category data!!! Skipping any such timeseries in the provided data.frame.")

          #TODO: Figure out how to handle non-continuous data!!!
          #
          # if (nrow(ts) > 0){
          #   # At this point ts should contain the data you want to insert into measurements_discrete. If any coercion is necessary (col names or other) it should have been done by the source_fx function. No need for calculations, periodicity sorting out, or anything necessary on the continuous timeseries.
          #   # Remember to update the start_datetime, end_datetime, last_new_data in the timeseries table
          #
          # } else {
          #   warning("Failed to retrieve any data from ", add$location[i], " and parameter ", add$parameter[i], "using function ", add$source_fx[i], ".")
          # }

        }
    }, error = function(e) {
      warning("Failed to add new data for row number ", i, " in the provided timeseries_df data.frame.")
    })
  } #End of loop iterating over each new  timeseries entry


    #TODO: calculate or find polygons for any locations that have flow or level. Modify function getWatersheds.

}
