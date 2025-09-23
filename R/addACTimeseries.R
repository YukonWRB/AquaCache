#' Add timeseries to aquacache database
#'
#'@description
#'
#' This function facilitates the addition of one or multiple timeseries to the database by adding entries to the timeseries and settings tables. See related function [addACLocation()] for adding a location to which timeseries must be attached. To add an image series see [addACImageSeries()], for raster series see [addACRasterSeries()]. For one-off images use [insertACImage()] and for rasters [insertACRaster()]. For documents use [insertACDocument()].
#'
#' You will be prompted to add locations that don't exist yet if any fall into this category.
#'
#' @details
#' You can add the new timeseries by directly editing the database, but this function ensures that database constraints are respected and will immediately seek to populate the measurements and calculated tables with new information for each timeseries. For Water Survey of Canada data, this function will also seek out level and flow data from the HYDAT database, downloading or checking for updates to it before use.
#'
#' If specifying a data.frame for argument `data`, different criteria applies depending on if the timeseries is categorized as continuous or discrete.
#' For continuous data:
#' The data.frame must contain a 'datetime' (POSIXct) OR 'date' (date) column. If specifying 'date' then the data is entered directly to the 'measurements_calculated_daily' table with no entry to measurements_continuous. 'value' (numeric) is also required, and optionally 'owner', 'contributor', 'share_with', 'approval', 'grade', 'qualifier'. Function [addNewContinuous()] will be called to add this data to the database. If source_fx is also specified it will be called to fetch more recent data than that in this data.frame.
#' For discrete data:
#' This is not supported yet.
#'
#' Additional arguments to pass to the function specified in `source_fx` go in argument `source_fx_args` (or a column with same name in 'df') and will be converted to JSON format. It's therefore necessary to pass this argument in as a single length character vector in the style "argument1: value1, argument2: value2".
#'
#' @param df A data.frame containing at least one row and the following columns: start_datetime, location, z, parameter, media, sensor_priority, aggregation_type, record_rate, share_with, owner, source_fx, source_fx_args, note. If this parameter is provided, all other parameters save for `data` must be NA or left as their default values. See notes for the other parameters for more information on each column of df.
#' @param data An optional list of data.frames of length nrow(df) or length(location) containing the data to add to the database. If adding multiple timeseries and not all of them need data, include NA elements in the list in the correct locations.
#' @param start_datetime A character or posixct vector of datetimes from which to look for new data, if source_fx is specified. Will be coerced to posixct with a time zone of UTC if not posixct.
#' @param location A character vector corresponding to column 'location' of table 'locations' OR a numeric vector corresponding to column 'location_id' of table 'locations'.
#' @param sub_location A character vector corresponding to column 'sub_location_id' of table 'sub_locations'. This is optional and can be left as NA if not specified. It is used to differentiate between multiple timeseries at the same location, e.g. different standpipes or wells.
#' @param z A numeric vector of elevations in meters for the timeseries observations. This allows for differentiation of things like wind speeds at different heights. Leave as NA if not specified.
#' @param parameter A numeric vector corresponding to column 'parameter_id' of table 'parameters'.
#' @param media A numeric vector corresponding to column 'media_id' of table 'media_types'.
#' @param sensor_priority A numeric vector assigning priority order to assign to this timeseries, default 1. This can allow for storage of multiple identical timeseries taken by different sensors for redundancy.
#' @param aggregation_type A character vector describing the measurement type; one of 'instantaneous' (immediate sensor value), 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'.
#' @param record_rate A broad categorization of the rate at which recording takes place. Select from a number fo minutes or hours ('5 minutes', '1 hour'), '1 day', '1 week', '4 weeks', '1 month', '1 year'; set to NA for discrete timeseries.
#' @param share_with A *character* vector of the user group(s) with which to share the timeseries, Default is 'public_reader'. Pass multiple groups as a single string, e.g. "public_reader, YG" or multiple such strings if specifying multiple timeseries in one go.
#' @param owner A numeric vector of the owner(s) of the timeseries(s). This can be different from the location owner!
#' @param source_fx The function to use for fetching data to append to the timeseries automatically. If specified, must be one of the 'downloadXXX' functions in this R package.
#' @param source_fx_args Arguments to pass to the function(s) specified in parameter 'source_fx'. See details.
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
#' aggregation_type = c("sum", "mean"),
#' record_rate = "1 hour",
#' share_with = "public_reader",
#' owner = 2,
#' source_fx = "downloadAquarius",
#' source_fx_args = NA,
#' note = c("Total precipitation from standpipe, reset every year in the fall.",
#' "Hourly average of wind speeds recorded every minute")
#' )
#' #Add the timeseries using the data.frame
#' addACTimeseries(df)
#' }

addACTimeseries <- function(
  df = NULL,
  data = NULL,
  start_datetime = NA,
  location = NA,
  sub_location = NA,
  z = NA,
  parameter = NA,
  media = NA,
  sensor_priority = 1,
  aggregation_type = 'instantaneous',
  record_rate = NA,
  share_with = "public_reader",
  owner = NA,
  source_fx = NA,
  source_fx_args = NA,
  note = NA,
  con = NULL
) {
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (!is.null(data)) {
    if (!inherits(data, "list")) {
      stop(
        "The 'data' parameter must be a list of data.frames if it is provided."
      )
    }
  }

  if (!is.null(df)) {
    # Check that a few of the other parameters are NA
    if (
      !all(is.na(c(
        location,
        start_datetime,
        z,
        parameter,
        media,
        record_rate,
        owner,
        source_fx,
        source_fx_args,
        note
      )))
    ) {
      stop(
        "You cannot provide a data.frame and other parameters at the same time."
      )
    }
    # Check that the data.frame is not empty
    if (nrow(df) == 0) {
      stop("The data.frame provided is empty.")
    }

    # Check that there is a column name for each function parameter that is not 'df'
    if (
      !all(
        c(
          "start_datetime",
          "location",
          "z",
          "parameter",
          "media",
          "sensor_priority",
          "aggregation_type",
          "record_rate",
          "share_with",
          "owner",
          "source_fx",
          "source_fx_args",
          "note"
        ) %in%
          colnames(df)
      )
    ) {
      stop(
        "The data.frame provided does not contain all the necessary columns."
      )
    }

    # Assign each column of the data.frame to the corresponding function parameter
    start_datetime <- df$start_datetime
    location <- df$location
    sub_location <- df$sub_location
    z <- df$z
    parameter <- df$parameter
    media <- df$media
    sensor_priority <- df$sensor_priority
    aggregation_type <- df$aggregation_type
    record_rate <- df$record_rate
    share_with <- df$share_with
    owner <- df$owner
    source_fx <- df$source_fx
    source_fx_args <- df$source_fx_args
    note <- df$note
  }

  # Check on arguments

  # Find the longest argument, then make sure all are either NA, length 1, or the same length.
  maxlength <- max(
    length(start_datetime),
    length(location),
    length(z),
    length(parameter),
    length(media),
    length(sensor_priority),
    length(aggregation_type),
    length(record_rate),
    length(owner),
    length(source_fx),
    length(source_fx_args),
    length(note)
  )

  if (!inherits(start_datetime, "POSIXct")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  }
  if (length(start_datetime) == 1 && maxlength > 1) {
    start_datetime <- rep(start_datetime, length)
  }
  if (!is.null(data) & length(data) != maxlength) {
    stop(
      "The 'data' parameter must be a list of data.frames of the same length as the other parameters."
    )
  }

  if (length(share_with) != 1 && length(share_with) != maxlength) {
    stop(
      "share_with must be a single value or a vector of the same length as the other parameters. Please check the function documentation."
    )
  }

  if (any(is.na(location))) {
    stop("location cannot contain NA values")
  } else {
    if (length(location) == 1 && maxlength > 1) {
      location <- rep(location, length)
    }

    #Check that every location in 'location' already exists
    new_locs <- NULL
    if (inherits(location, "numeric")) {
      exist_locs <- DBI::dbGetQuery(con, "SELECT location_id FROM locations")[,
        1
      ]
      new_locs <- location[!(location %in% exist_locs)]
    } else if (inherits(location, "character")) {
      exist_locs <- DBI::dbGetQuery(con, "SELECT location FROM locations")[, 1]
      new_locs <- location[!(location %in% exist_locs)]
    }
    if (!all(location %in% exist_locs)) {
      stop(
        "Not all of the locations in your timeseries_df are already in the database. Please add the following location(s) first using addACLocation() or the add location Shiny module: ",
        paste(new_locs, collapse = ", "),
        ", or use one of the existing locations."
      )
    }
  }

  # Check that every sub_location in 'sub_location' already exists, if specified
  if (!is.na(sub_location)) {
    db_sub_loc <- DBI::dbGetQuery(
      con,
      "SELECT sub_location_id FROM sub_locations;"
    )[, 1]
    if (!all(sub_location %in% db_sub_loc)) {
      stop(
        "At least one of the sub_location_ids you specified does not exist in the database. Please add it first using the add sub-location Shiny module."
      )
    }
  }

  if (any(!is.na(z))) {
    if (!inherits(z, "numeric")) {
      stop("z must be a numeric vector or left as NA")
    }
  }
  if (length(z) == 1 && maxlength > 1) {
    z <- rep(z, maxlength)
  }

  if (any(is.na(parameter))) {
    stop("parameter cannot contain NA values")
  } else {
    if (!inherits(parameter, "numeric")) {
      stop("parameter must be a numeric vector")
    }
    if (length(parameter) == 1 && maxlength > 1) {
      parameter <- rep(parameter, maxlength)
    }
    db_param <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT parameter_id FROM parameters WHERE parameter_id IN (",
        paste(unique(parameter), collapse = ", "),
        ");"
      )
    )

    if (nrow(db_param) < length(unique(parameter))) {
      stop(
        "At least one of the parameter_ids you specified does not exist in the database."
      )
    }
  }

  if (any(is.na(media))) {
    stop("media cannot contain NA values")
  } else {
    if (!inherits(media, "numeric")) {
      stop("media must be a numeric vector")
    }
    if (length(media) == 1 && maxlength > 1) {
      media <- rep(media, maxlength)
    }
    db_media <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT media_id FROM media_types WHERE media_id IN (",
        paste(unique(media), collapse = ", "),
        ");"
      )
    )
    if (nrow(db_media) < length(unique(media))) {
      stop(
        "At least one of the media_ids you specified does not exist in the database."
      )
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
  if (length(sensor_priority) == 1 && maxlength > 1) {
    sensor_priority <- rep(sensor_priority, maxlength)
  }

  if (any(is.na(aggregation_type))) {
    stop("aggregation_type cannot contain NA values")
  } else {
    if (
      !all(
        aggregation_type %in%
          c(
            'instantaneous',
            'sum',
            'mean',
            'median',
            'min',
            'max',
            '(min+max)/2'
          )
      )
    ) {
      stop(
        "aggregation_type must be one of 'instantaneous', 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'"
      )
    }
    if (length(aggregation_type) == 1 && maxlength > 1) {
      aggregation_type <- rep(aggregation_type, maxlength)
    }
  }

  if (any(is.na(record_rate))) {
    stop("record_rate cannot contain NA values")
  } else {
    if (length(record_rate) == 1 && maxlength > 1) {
      record_rate <- rep(record_rate, maxlength)
    }
  }

  # Check that record rate elements that are not NA are in the correct format
  rec_rate_no_na <- record_rate[!is.na(record_rate)]
  # USe lubridate::period() to check if the record rate is in the correct format
  for (i in 1:length(rec_rate_no_na)) {
    if (!lubridate::is.period(lubridate::period(rec_rate_no_na[i]))) {
      stop(
        "record_rate must be a character vector of the form '1 hour', '5 minutes', '1 day', '1 week', '4 weeks', '1 month', '1 year'"
      )
    }
  }

  if (any(is.na(share_with))) {
    if (!inherits(share_with, "character")) {
      stop("share_with must be a character vector.")
    }
    share_with[is.na(share_with)] <- "public_reader"
  } else if (!inherits(share_with, "character")) {
    stop("share_with must be a character vector.")
  }
  if (length(share_with) == 1 && maxlength > 1) {
    share_with <- rep(share_with, maxlength)
  }

  if (any(is.na(owner))) {
    stop("owner cannot be NA")
  } else if (!inherits(owner, "numeric")) {
    stop("owner must be a numeric vector")
  }
  if (length(owner) == 1 && maxlength > 1) {
    owner <- rep(owner, maxlength)
  }
  db_owner <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT organization_id FROM organizations WHERE organization_id IN (",
      paste(unique(owner), collapse = ", "),
      ");"
    )
  )
  if (nrow(db_owner) < length(unique(owner))) {
    stop(
      "At least one of the owners you specified does not exist in the database."
    )
  }

  if (any(is.na(source_fx))) {
    warning(
      "At least one of the source_fx you entered is NA. This will not allow for automatic fetching of new data. The timeseries will be added to the database, but you will need to manually add data unless you also specified a data.frame with this."
    )
  }
  source_fx_check <- source_fx[!is.na(source_fx)]
  if (length(source_fx_check) > 0) {
    if (!all(source_fx_check %in% ls(getNamespace("AquaCache")))) {
      stop(
        "At least one of the source_fx strings you entered does not exist in the AquaCache package."
      )
    }
  }
  if (length(source_fx) == 1 && maxlength > 1) {
    source_fx <- rep(source_fx, maxlength)
  }

  if (length(source_fx_args) == 1 && maxlength > 1) {
    stop(
      "source_fx_args must be a vector of the same length as the other parameters OR left NA; you cannot leave it as length 1 as this function presumes that arguments are particular to single timeseries and won't replicate to length of other vectors."
    )
  }

  if (length(note) == 1 && maxlength > 1) {
    stop(
      "note must be a character vector of the same length as the other parameters OR left NA; you cannot leave it as length 1 as this function presumes that notes are particular to single timeseries and won't replicate to length of other vectors."
    )
  }
  if (!any(is.na(note))) {
    if (!inherits(note, "character")) {
      stop("note must be a character vector or left NA.")
    }
  }

  #Add the timeseries #######################################################################################################

  for (i in 1:length(location)) {
    loc_code <- location[i]
    if (inherits(loc_code, "character")) {
      # Get the location_id from the database
      loc_id <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT location_id FROM locations WHERE location = '",
          loc_code,
          "';"
        )
      )[1, 1]
    } else {
      loc_id <- loc_code
      loc_code <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT location FROM locations WHERE location_id = ",
          loc_id,
          ";"
        )
      )[1, 1]
    }
    tryCatch(
      {
        args <- source_fx_args[i]
        # split into "argument1: value1" etc.
        args <- strsplit(args, ",\\s*")[[1]]

        # split each pair on ":" and trim whitespace
        args <- strsplit(args, ":\\s*")

        # build a named list: names = keys, values = values
        args <- stats::setNames(
          lapply(args, function(x) x[2]),
          sapply(args, function(x) x[1])
        )
        # convert to JSON
        args <- jsonlite::toJSON(args, auto_unbox = TRUE)

        aggregation_type_id <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT aggregation_type_id FROM aggregation_types WHERE aggregation_type = '",
            aggregation_type[i],
            "';"
          )
        )[1, 1]

        zi <- z[i]
        # If not NA, create a new entry in public.locations_z
        if (!is.na(zi)) {
          z_df <- data.frame(
            location_id = loc_id,
            z_meters = zi,
            sub_location_id = sub_location[i]
          )
          try({
            # This may fail if the z value already exists for this location/sub_location combo
            DBI::dbAppendTable(con, "locations_z", z_df)
          })
          zi <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT location_z_id FROM locations_z WHERE location_id = ",
              loc_id,
              " AND z_meters = ",
              zi,
              " AND sub_location_id ",
              if (is.na(sub_location[i])) {
                "IS NULL"
              } else {
                paste0("= '", sub_location[i], "'")
              },
              ";"
            )
          )[1, 1]
        }

        add <- data.frame(
          location = loc_code,
          sub_location_id = sub_location[i],
          location_id = loc_id,
          z = zi,
          parameter_id = parameter[i],
          media_id = media[i],
          sensor_priority = sensor_priority[i],
          aggregation_type_id = aggregation_type_id,
          record_rate = record_rate[i],
          share_with = paste0("{", paste(share_with[i], collapse = ", "), "}"),
          default_owner = owner[i],
          source_fx = source_fx[i],
          source_fx_args = args,
          note = note[i],
          end_datetime = if (is.na(source_fx[i])) NA else start_datetime[i] - 1
        )

        tryCatch(
          {
            DBI::dbAppendTable(con, "timeseries", add) #This is in the tryCatch because the timeseries might already have been added by update_hydat, which searches for level + flow for each location, or by a failed attempt at adding earlier on.
            message(
              "Added a new entry to the timeseries table for location ",
              add$location,
              ", parameter ",
              add$parameter_id,
              ", media_type ",
              add$media_id,
              ", and aggregation_type_id ",
              add$aggregation_type_id,
              "."
            )
            new_tsid <- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT timeseries_id FROM timeseries WHERE location = '",
                add$location,
                "' AND parameter_id = ",
                add$parameter_id,
                " AND aggregation_type_id = '",
                add$aggregation_type_id,
                "' AND record_rate = '",
                add$record_rate,
                "';"
              )
            )[1, 1]
          },
          error = function(e) {
            message(
              "It looks like the timeseries for for location ",
              add$location,
              ", parameter ",
              add$parameter_id,
              ", media_type ",
              add$media_id,
              ", and aggregation_type_id ",
              add$aggregation_type_id,
              " has already been added. This likely happened because this function already called function update_hydat on a flow or level timeseries of the Water Survey of Canada and automatically looked for the corresponding level/flow timeseries, or because of an earlier failed attempt to add the timeseries Don't worry, I'm still checking for data."
            )
            new_tsid <<- DBI::dbGetQuery(
              con,
              paste0(
                "SELECT timeseries_id FROM timeseries WHERE location = '",
                add$location,
                "' AND parameter_id = ",
                add$parameter_id,
                " AND aggregation_type_id = '",
                add$aggregation_type_id,
                "' AND record_rate = '",
                add$record_rate,
                "';"
              )
            )[1, 1]
            # Modify the end_datetime in the DB to be one second before the start_datetime
            DBI::dbExecute(
              con,
              paste0(
                "UPDATE timeseries SET end_datetime = '",
                add$end_datetime,
                "' WHERE timeseries_id = ",
                new_tsid,
                ";"
              )
            )
          }
        )

        if (!is.null(data)) {
          if (!is.na(data[i])) {
            if ('date' %in% colnames(data[[i]])) {
              addNewContinuous(
                tsid = new_tsid,
                df = data[[i]],
                target = 'daily',
                con = con
              )
              calculate_stats(
                timeseries_id = new_tsid,
                con = con,
                start_recalc = min(data[[i]]$date)
              )
              DBI::dbExecute(
                con,
                paste0(
                  "UPDATE timeseries SET start_datetime = (SELECT MIN(date) FROM measurements_calculated_daily WHERE timeseries_id = ",
                  new_tsid,
                  ") WHERE timeseries_id = ",
                  new_tsid,
                  ";"
                )
              )
            } else {
              addNewContinuous(
                tsid = new_tsid,
                df = data[[i]],
                target = 'realtime',
                con = con
              )
              calculate_stats(
                timeseries_id = new_tsid,
                con = con,
                start_recalc = min(data[[i]]$datetime)
              )
              DBI::dbExecute(
                con,
                paste0(
                  "UPDATE timeseries SET start_datetime = (SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ",
                  new_tsid,
                  ") WHERE timeseries_id = ",
                  new_tsid,
                  ";"
                )
              )
            }
          }
        }

        if (!is.na(source_fx[i])) {
          param_name <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT param_name FROM parameters WHERE parameter_id = ",
              add$parameter_id,
              ";"
            )
          )[1, 1]

          # Call the relevant 'get' functions to bring in data
          remove_after_hydat <- FALSE
          tryCatch(
            {
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM measurements_continuous WHERE timeseries_id = ",
                  new_tsid,
                  " AND datetime >= '",
                  add$end_datetime,
                  "';"
                )
              )
              getNewContinuous(con = con, timeseries_id = new_tsid)
              new_start <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ",
                  new_tsid,
                  ";"
                )
              )
              DBI::dbExecute(
                con,
                paste0(
                  "UPDATE timeseries SET start_datetime = '",
                  new_start$min,
                  "' WHERE timeseries_id = ",
                  new_tsid,
                  ";"
                )
              )
            },
            error = function(e) {
              message(
                "Failed to add new continuous data for location ",
                add$location,
                " and parameter ",
                add$parameter_id,
                "."
              )
              if (
                (add$source_fx == "downloadWSC") &
                  param_name %in% c("water level", "water flow")
              ) {
                message("Attempting to add historical data from HYDAT database")
                remove_after_hydat <<- TRUE
              } else {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM timeseries WHERE timeseries_id = ",
                    new_tsid,
                    ";"
                  )
                )
                message(
                  "Deleted the timeseries entry for location ",
                  add$location,
                  " and parameter ",
                  add$parameter_id,
                  "."
                )
              }
            }
          )

          # Now conditionally check for HYDAT data
          if (
            (add$source_fx == "downloadWSC") &
              param_name %in% c("water level", "water flow")
          ) {
            message("Adding historical data from HYDAT database")
            suppressMessages(update_hydat(
              con = con,
              timeseries_id = new_tsid,
              force_update = TRUE
            ))
            if (remove_after_hydat) {
              # see if anything exists in table measurements_calculated_daily for this timeseries_id. If not, delete the timeseries.
              exist <- DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT timeseries_id FROM measurements_calculated_daily WHERE timeseries_id = ",
                  new_tsid,
                  ";"
                )
              )
              if (nrow(exist) == 0) {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM timeseries WHERE timeseries_id = ",
                    new_tsid,
                    ";"
                  )
                )
                message(
                  "Deleted the timeseries entry for location ",
                  add$location,
                  " and parameter ",
                  add$parameter_id,
                  " as no realtime or daily means data could be found."
                )
              }
            }
          }
          tryCatch(
            {
              if (
                lubridate::period(add$record_rate) <= lubridate::period("1 day")
              ) {
                calculate_stats(
                  timeseries_id = new_tsid,
                  con = con,
                  start_recalc = NULL
                )
                message(
                  "Success! Calculated daily means and statistics for ",
                  add$location,
                  " and parameter ",
                  param_name,
                  "."
                )
              } else {
                message(
                  "Not calculating daily statistics for ",
                  add$location,
                  " and parameter ",
                  param_name,
                  " as recording rate is greater than 1 day."
                )
              }
            },
            error = function(e) {
              message(
                "Unable to calculate daily means and statistics for ",
                add$location,
                " and parameter ",
                param_name,
                " with message ",
                e$message,
                "."
              )
            },
            warning = function(e) {
              message(
                "May have failed to calculate daily means and statistics for ",
                add$location,
                " and parameter ",
                param_name,
                "."
              )
            }
          )
        } else {
          message(
            "You didn't specify a source_fx. No data was added to the measurements_continuous or measurements_discrete table, so make sure you go and add that data ASAP. If you made a mistake delete the timeseries from the timeseries table and restart. The timeseries ID for this new entry is ",
            new_tsid
          )
        }
      },
      error = function(e) {
        warning(
          "Failed to add new data for location ",
          add$location,
          " and parameter ",
          add$parameter_id,
          ". Returned error: ",
          e$message
        )
      }
    )
  } #End of loop iterating over each new  timeseries entry
}
