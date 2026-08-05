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
#' The data.frame must contain a 'datetime' (POSIXct) OR 'date' (date) column. If specifying 'date' then the data is entered to `measurements_continuous` as one-day-period rows and database triggers maintain `measurements_calculated_daily`. 'value' (numeric) is also required, and optionally 'owner', 'contributor', 'share_with', 'approval', 'grade', 'qualifier'. Function [addNewContinuous()] will be called to add this data to the database. If a fetch source adapter is also specified it will be called to fetch more recent data than that in this data.frame.
#'
#' Source adapters are supplied in `source_adapters`. Each assignment may be used for fetching, synchronization, or both, and may be retained while temporarily inactive. The highest active priority (lowest number) is selected for each operation.
#'
#' @param df A data.frame containing at least one row and the following columns: start_datetime, location, z, parameter, media, sensor_priority, aggregation_type, record_rate, share_with, owner, note. An optional list-column named `source_adapters` may contain one assignment data.frame per new timeseries. If this parameter is provided, all other parameters save for `data` must be left at their defaults.
#' @param data An optional list of data.frames of length nrow(df) or length(location) containing the data to add to the database. If adding multiple timeseries and not all of them need data, include NA elements in the list in the correct locations.
#' @param start_datetime A character or POSIXct vector of datetimes from which
#'   to look for new data when an active fetch assignment is specified. Values
#'   are coerced to POSIXct in UTC.
#' @param location A character vector corresponding to locations.location_code (preferred), locations.alias (legacy and nullable), or locations.name OR a numeric vector corresponding to locations.location_id.
#' @param sub_location A numeric vector corresponding to column 'sub_location_id' of table 'sub_locations'. This is optional and can be left as NA if not specified. It is used to differentiate between multiple timeseries at the same location, e.g. different standpipes or wells.
#' @param z A numeric vector of elevations in meters for the timeseries observations. This allows for differentiation of things like wind speeds at different heights. Leave as NA if not specified.
#' @param parameter A numeric vector corresponding to column 'parameter_id' of table 'parameters'.
#' @param media A numeric vector corresponding to column 'media_id' of table 'media_types'.
#' @param matrix_state_id An optional numeric vector corresponding to column 'matrix_state_id' of table 'matrix_states'. Leave as NA to let the database resolve the matrix state from the media and parameter defaults.
#' @param sensor_priority A numeric vector assigning priority order to assign to this timeseries, default 1. This can allow for storage of multiple identical timeseries taken by different sensors for redundancy.
#' @param aggregation_type A character vector describing the measurement type; one of 'instantaneous' (immediate sensor value), 'sum', 'mean', 'median', 'min', 'max', '(min+max)/2'.
#' @param record_rate A broad categorization of the rate at which recording takes place. Select from a number fo minutes or hours ('5 minutes', '1 hour'), '1 day', '1 week', '4 weeks', '1 month', '1 year'.
#' @param share_with A *character* vector of the user group(s) with which to share the timeseries, Default is 'public_reader'. Pass multiple groups as a single string, e.g. "public_reader, YG" or multiple such strings if specifying multiple timeseries in one go.
#' @param owner A numeric vector of the owner(s) of the timeseries(s). This can be different from the location owner!
#' @param source_adapters `NULL`, one assignment data.frame when adding one
#'   timeseries, or a list containing one assignment data.frame (or `NULL`) per
#'   new timeseries. Columns are `source_fx`, optional JSON or named-list
#'   `source_fx_args`, `fetch_priority`, `synchronize_priority`, `active`, and
#'   `note`. Every function must be registered for the continuous domain.
#' @param note Text notes to append to the timeseries.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. Leave NULL to use the package default connection and have it closed afterwards automatically.
#'
#' @return One or more new entries are created in the table 'timeseries'
#' @export
#'
#' @examples
#' \dontrun{
#' # Each timeseries receives its own source-adapter assignment data frame.
#' precipitation_sources <- data.frame(
#'   source_fx = c("downloadAquarius", "downloadRWIS"),
#'   source_fx_args = I(list(
#'     list(
#'       location = "09AA-M3",
#'       parameter = "Precip Total",
#'       difference = TRUE
#'     ),
#'     list(location = "RWIS_STATION", parameter = "Precipitation")
#'   )),
#'   fetch_priority = c(1L, 2L),
#'   synchronize_priority = c(2L, 1L),
#'   active = c(TRUE, TRUE),
#'   note = c(
#'     "Preferred source for routine fetching.",
#'     "Preferred source for full synchronization."
#'   )
#' )
#'
#' wind_sources <- data.frame(
#'   source_fx = "downloadAquarius",
#'   source_fx_args = I(list(list(
#'     location = "09AA-M3",
#'     parameter = "Wind Speed"
#'   ))),
#'   fetch_priority = 1L,
#'   synchronize_priority = 1L,
#'   active = TRUE
#' )
#'
#' # Add the assignment data frames as a list-column, one per timeseries.
#' df <- data.frame(
#'   start_datetime = "2015-01-01 00:00",
#'   location = "09AA-M3",
#'   z = c(NA, 3),
#'   parameter = c(34, 1154),
#'   media = 7,
#'   sensor_priority = 1,
#'   aggregation_type = c("sum", "mean"),
#'   record_rate = "1 hour",
#'   share_with = "public_reader",
#'   owner = 2,
#'   note = c(
#'     "Total precipitation from standpipe, reset every fall.",
#'     "Hourly average of wind speeds recorded every minute."
#'   )
#' )
#' df$source_adapters <- list(precipitation_sources, wind_sources)
#'
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
  matrix_state_id = NA,
  sensor_priority = 1,
  aggregation_type = 'instantaneous',
  record_rate = NA,
  share_with = "public_reader",
  owner = NA,
  source_adapters = NULL,
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
        sub_location,
        z,
        parameter,
        media,
        matrix_state_id,
        record_rate,
        owner,
        note
      ))) ||
        !is.null(source_adapters)
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
    sub_location <- if ("sub_location" %in% colnames(df)) {
      df$sub_location
    } else {
      rep(NA_integer_, nrow(df))
    }
    z <- df$z
    parameter <- df$parameter
    media <- df$media
    matrix_state_id <- if ("matrix_state_id" %in% colnames(df)) {
      df$matrix_state_id
    } else {
      rep(NA_integer_, nrow(df))
    }
    sensor_priority <- df$sensor_priority
    aggregation_type <- df$aggregation_type
    record_rate <- df$record_rate
    share_with <- df$share_with
    owner <- df$owner
    source_adapters <- if ("source_adapters" %in% names(df)) {
      df$source_adapters
    } else {
      rep(list(NULL), nrow(df))
    }
    note <- df$note
  }

  # Check on arguments

  # Find the longest argument, then make sure all are either NA, length 1, or the same length.
  maxlength <- max(
    length(start_datetime),
    length(location),
    length(sub_location),
    length(z),
    length(parameter),
    length(media),
    length(matrix_state_id),
    length(sensor_priority),
    length(aggregation_type),
    length(record_rate),
    length(owner),
    length(note)
  )

  if (any(is.na(start_datetime))) {
    stop("start_datetime cannot contain NA values")
  }
  if (!inherits(start_datetime, "POSIXct")) {
    start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
  }
  if (length(start_datetime) == 1 && maxlength > 1) {
    start_datetime <- rep(start_datetime, maxlength)
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
      location <- rep(location, maxlength)
    }

    # Check that every location in 'location' already exists
    new_locs <- NULL
    loc_tbl <- NULL
    if (inherits(location, "numeric")) {
      exist_locs <- DBI::dbGetQuery(
        con,
        "SELECT location_id FROM public.locations"
      )[,
        1
      ]
      new_locs <- location[!(location %in% exist_locs)]
    } else if (inherits(location, "character")) {
      loc_tbl <- DBI::dbGetQuery(
        con,
        "SELECT location_id, location_code, alias, name FROM public.locations"
      )
      exist_locs <- tolower(unique(c(
        loc_tbl$location_code,
        loc_tbl$alias,
        loc_tbl$name
      )))
      exist_locs <- exist_locs[!is.na(exist_locs)]
      new_locs <- location[!(tolower(location) %in% exist_locs)]
    }
    if (length(new_locs) > 0) {
      stop(
        "Not all of the locations in your timeseries_df are already in the database. Please add the following location(s) first using addACLocation() or the add location Shiny module: ",
        paste(new_locs, collapse = ", "),
        ", or use one of the existing locations."
      )
    }
  }

  # Check that every sub_location in 'sub_location' already exists, if specified
  if (length(sub_location) == 1 && maxlength > 1) {
    sub_location <- rep(sub_location, maxlength)
  }
  if (any(!is.na(sub_location))) {
    if (
      !inherits(sub_location, "numeric") && !inherits(sub_location, "integer")
    ) {
      stop("sub_location must be a numeric or integer vector or left as NA.")
    }
    db_sub_loc <- DBI::dbGetQuery(
      con,
      "SELECT sub_location_id FROM public.sub_locations;"
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
        "SELECT parameter_id FROM public.parameters WHERE parameter_id IN (",
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
        "SELECT media_id FROM public.media_types WHERE media_id IN (",
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

  if (any(!is.na(matrix_state_id))) {
    if (
      !inherits(matrix_state_id, "numeric") &&
        !inherits(matrix_state_id, "integer")
    ) {
      stop("matrix_state_id must be a numeric or integer vector or left as NA.")
    }
  }
  if (length(matrix_state_id) == 1 && maxlength > 1) {
    matrix_state_id <- rep(matrix_state_id, maxlength)
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
            'minimum',
            'maximum',
            '(min+max)/2'
          )
      )
    ) {
      stop(
        "aggregation_type must be one of 'instantaneous', 'sum', 'mean', 'median', 'minimum', 'maximum', '(min+max)/2'"
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
  } else if (!inherits(owner, "numeric") & !inherits(owner, "integer")) {
    stop("owner must be a numeric or integer vector")
  }
  if (length(owner) == 1 && maxlength > 1) {
    owner <- rep(owner, maxlength)
  }
  db_owner <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT organization_id FROM public.organizations WHERE organization_id IN (",
      paste(unique(owner), collapse = ", "),
      ");"
    )
  )
  if (nrow(db_owner) < length(unique(owner))) {
    stop(
      "At least one of the owners you specified does not exist in the database."
    )
  }

  source_adapters_by_series <- if (is.null(source_adapters)) {
    rep(list(NULL), maxlength)
  } else if (inherits(source_adapters, "data.frame")) {
    if (maxlength != 1L) {
      stop(
        "When adding multiple timeseries, source_adapters must be a list ",
        "with one assignment data.frame or NULL per timeseries."
      )
    }
    list(source_adapters)
  } else if (is.list(source_adapters) && length(source_adapters) == maxlength) {
    source_adapters
  } else {
    stop(
      "source_adapters must be NULL, one assignment data.frame for one ",
      "timeseries, or a list with one element per timeseries."
    )
  }
  source_adapters_by_series <- lapply(
    source_adapters_by_series,
    source_adapter_assignments_normalize,
    con = con,
    data_domain = "continuous"
  )

  if (!any(is.na(note))) {
    if (!inherits(note, "character")) {
      stop("note must be a character vector or left NA.")
    }
    if (length(note) == 1 && maxlength > 1) {
      stop(
        "note must be a character vector of the same length as the other parameters OR left NA; you cannot leave it as length 1 as this function presumes that notes are particular to single timeseries and won't replicate to length of other vectors."
      )
    }
  }

  #Add the timeseries #######################################################################################################

  for (i in 1:length(location)) {
    loc_id <- location[i]
    loc_label <- as.character(loc_id)
    if (inherits(loc_id, "character")) {
      # Get the location_id from the database
      loc_name <- tolower(loc_id)
      if (is.null(loc_tbl)) {
        loc_tbl <- DBI::dbGetQuery(
          con,
          "SELECT location_id, location_code, alias, name FROM public.locations"
        )
      }
      loc_match <- loc_tbl[
        tolower(loc_tbl$location_code) == loc_name,
      ]
      if (nrow(loc_match) == 0) {
        loc_match <- loc_tbl[tolower(loc_tbl$alias) == loc_name, ]
      }
      if (nrow(loc_match) == 0) {
        loc_match <- loc_tbl[tolower(loc_tbl$name) == loc_name, ]
      }
      if (nrow(loc_match) == 0) {
        stop("Unable to find a location matching ", loc_id, ".")
      }
      loc_id <- loc_match$location_id[1]
      loc_label <- loc_match$location_code[1]
      if (is.na(loc_label) || loc_label == "") {
        loc_label <- loc_match$name[1]
      }
    } else if (inherits(loc_id, "numeric")) {
      loc_info <- DBI::dbGetQuery(
        con,
        "SELECT location_code, name FROM public.locations WHERE location_id = $1;",
        params = list(loc_id)
      )
      if (nrow(loc_info) == 1) {
        loc_label <- loc_info$location_code[1]
        if (is.na(loc_label) || loc_label == "") {
          loc_label <- loc_info$name[1]
        }
      }
    }
    tryCatch(
      {
        source_assignments <- source_adapters_by_series[[i]]
        fetch_assignments <- source_assignments[
          source_assignments$active &
            !is.na(source_assignments$fetch_priority),
          ,
          drop = FALSE
        ]
        if (nrow(fetch_assignments) > 0L) {
          fetch_assignments <- fetch_assignments[
            order(fetch_assignments$fetch_priority),
            ,
            drop = FALSE
          ]
          fetch_source_fx <- fetch_assignments$source_fx[[1L]]
        } else {
          fetch_source_fx <- NA_character_
        }

        aggregation_type_id <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT aggregation_type_id FROM continuous.aggregation_types WHERE aggregation_type = '",
            aggregation_type[i],
            "';"
          )
        )[1, 1]

        resolved_matrix_state_id <- resolve_parameter_matrix_state(
          con = con,
          media_id = media[i],
          parameter_id = parameter[i],
          matrix_state_id = matrix_state_id[i]
        )
        if (is.na(resolved_matrix_state_id)) {
          stop(
            "Could not resolve matrix_state_id for location ",
            loc_label,
            ", parameter ",
            parameter[i],
            ", and media ",
            media[i],
            "."
          )
        }

        zi <- z[i]
        # If not NA, create a new entry in public.locations_z
        if (!is.na(zi)) {
          try({
            # This may fail if the z value already exists for this location/sub_location combo
            DBI::dbExecute(
              con,
              "INSERT INTO public.locations_z (location_id, z_meters, sub_location_id) VALUES ($1, $2, $3) ON CONFLICT DO NOTHING;",
              params = list(loc_id, zi, sub_location[i])
            )
          })
          zi <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT z_id FROM public.locations_z WHERE location_id = ",
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
          sub_location_id = sub_location[i],
          location_id = loc_id,
          z_id = zi,
          parameter_id = parameter[i],
          media_id = media[i],
          matrix_state_id = resolved_matrix_state_id,
          sensor_priority = sensor_priority[i],
          aggregation_type_id = aggregation_type_id,
          record_rate = record_rate[i],
          share_with = paste0("{", paste(share_with[i], collapse = ", "), "}"),
          default_owner = owner[i],
          note = note[i],
          end_datetime = if (is.na(fetch_source_fx)) {
            NA
          } else {
            start_datetime[i] - 1
          }
        )

        new_timeseries_created <- FALSE
        tryCatch(
          {
            new_tsid <- DBI::dbGetQuery(
              con,
              "INSERT INTO continuous.timeseries (location_id, sub_location_id, z_id, parameter_id, media_id, matrix_state_id, sensor_priority, aggregation_type_id, record_rate, share_with, default_owner, note) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10::text[], $11, $12) RETURNING timeseries_id;",
              params = list(
                add$location_id,
                add$sub_location_id,
                add$z_id,
                add$parameter_id,
                add$media_id,
                add$matrix_state_id,
                add$sensor_priority,
                add$aggregation_type_id,
                add$record_rate,
                add$share_with,
                add$default_owner,
                add$note
              )
            )[1, 1]
            new_timeseries_created <- TRUE

            message(
              "Added a new entry to the timeseries table for location ",
              loc_label,
              ", parameter ",
              add$parameter_id,
              ", media_type ",
              add$media_id,
              ", and aggregation_type_id ",
              add$aggregation_type_id,
              "."
            )
          },
          error = function(e) {
            message(
              "It looks like the timeseries for for location ",
              loc_label,
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
              paste(
                "SELECT timeseries_id",
                "FROM continuous.timeseries",
                "WHERE location_id = $1",
                "  AND parameter_id = $2",
                "  AND aggregation_type_id = $3",
                "  AND media_id = $4",
                "  AND matrix_state_id IS NOT DISTINCT FROM $5",
                "  AND record_rate = $6",
                "  AND z_id IS NOT DISTINCT FROM $7",
                "  AND sensor_priority = $8",
                "  AND sub_location_id IS NOT DISTINCT FROM $9;"
              ),
              params = list(
                add$location_id,
                add$parameter_id,
                add$aggregation_type_id,
                add$media_id,
                add$matrix_state_id,
                add$record_rate,
                add$z_id,
                add$sensor_priority,
                add$sub_location_id
              )
            )[1, 1]
            if (is.na(new_tsid)) {
              stop(conditionMessage(e))
            }
          }
        )

        if (new_timeseries_created && nrow(source_assignments) > 0L) {
          source_adapter_assignments_insert(
            con = con,
            data_domain = "continuous",
            series_id = new_tsid,
            assignments = source_assignments
          )
        }

        if (!is.null(data)) {
          x <- data[[i]]
          if (!is.data.frame(x)) {
            stop(
              "The element of the 'data' list corresponding to timeseries ",
              i,
              " is not a data.frame."
            )
          } else if (nrow(x) == 0) {
            stop(
              "The element of the 'data' list corresponding to timeseries ",
              i,
              " is an empty data.frame."
            )
          } else {
            # Ensure that the data.frame has the necessary columns
            if (!all(c("datetime", "value") %in% colnames(data[[i]]))) {
              stop(
                "The element of the 'data' list corresponding to timeseries ",
                i,
                " does not contain the necessary columns 'datetime' and 'value'."
              )
            }

            addNewContinuous(
              tsid = new_tsid,
              df = data[[i]],
              con = con
            ) # Calculates stats within the function
          }
        }

        if (!is.na(fetch_source_fx)) {
          param_name <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT param_name FROM public.parameters WHERE parameter_id = ",
              add$parameter_id,
              ";"
            )
          )[1, 1]

          # Call the relevant 'get' functions to bring in new data
          remove_after_hydat <- FALSE
          tryCatch(
            {
              # Wipe any potential data that is after what was added
              DBI::dbExecute(
                con,
                paste0(
                  "DELETE FROM continuous.measurements_continuous WHERE timeseries_id = ",
                  new_tsid,
                  " AND datetime >= '",
                  add$end_datetime,
                  "';"
                )
              )
              getNewContinuous(
                con = con,
                timeseries_id = new_tsid,
                stats = TRUE
              )
            },
            error = function(e) {
              message(
                "Failed to add new continuous data for location ",
                loc_label,
                " and parameter ",
                add$parameter_id,
                "."
              )
              if (
                (fetch_source_fx == "downloadWSC") &
                  param_name %in% c("water level", "water flow")
              ) {
                message("Attempting to add historical data from HYDAT database")
                remove_after_hydat <<- TRUE
              } else {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM continuous.timeseries WHERE timeseries_id = ",
                    new_tsid,
                    ";"
                  )
                )
                message(
                  "Deleted the timeseries entry for location ",
                  loc_label,
                  " and parameter ",
                  add$parameter_id,
                  "."
                )
              }
            }
          )

          # Now conditionally check for HYDAT data
          if (
            (fetch_source_fx == "downloadWSC") &
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
                  "SELECT timeseries_id FROM continuous.measurements_calculated_daily WHERE timeseries_id = ",
                  new_tsid,
                  ";"
                )
              )
              if (nrow(exist) == 0) {
                DBI::dbExecute(
                  con,
                  paste0(
                    "DELETE FROM continuous.timeseries WHERE timeseries_id = ",
                    new_tsid,
                    ";"
                  )
                )
                message(
                  "Deleted the timeseries entry for location ",
                  loc_label,
                  " and parameter ",
                  add$parameter_id,
                  " as no realtime or daily means data could be found."
                )
              }
            }
          }
          if (lubridate::period(add$record_rate) > lubridate::period("1 day")) {
            message(
              "Not calculating daily statistics for ",
              loc_label,
              " and parameter ",
              param_name,
              " as recording rate is greater than 1 day."
            )
          }
        } else {
          message(
            "You didn't specify an active fetch source adapter. No data was added to the measurements_continuous table, so make sure you add that data manually. The timeseries ID for this new entry is ",
            new_tsid
          )
        }
      },
      error = function(e) {
        warning(
          "Failed to add new data for location ",
          loc_label,
          " and parameter ",
          add$parameter_id,
          ". Returned error: ",
          e$message
        )
      }
    )
  } #End of loop iterating over each new  timeseries entry
}
