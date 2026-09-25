#' Add location to aquacache
#'
#' Adds a new location to the aquacache 'locations' table. You can pass a data.frame with the necessary columns, or provide each parameter separately. Extensive checks are performed to ensure that the location does not already exist, and that all necessary parameters are provided and are valid.
#'
#' @param df A data.frame containing the location fields. `datum_id_from`,
#'   `datum_id_to`, `conversion_m`, and `current` are optional. When
#'   `conversion_m` is missing, [get_elevation()] estimates the elevation from
#'   `latitude` and `longitude` and the matching datum IDs are populated
#'   automatically. If this parameter is provided, all other parameters except
#'   for `con` must be left as their default values.
#' @param name A character vector of the location name(s).
#' @param name_fr A character vector of the location name(s) in French. You're highly encouraged to populate this field, but if left blank (or the corresponding column in `df` is missing or empty) it will be populated with 'Traduction requise!'.
#' @param alias A character vector of the location alias(es). This is optional, leave NA if not needed.
#' @param location_code A character vector of the location code(s). Note that in most cases this should be left NULL to auto-generate, or created when adding a new location using the YGwater Shiny application!
#' @param latitude A numeric vector of the latitude(s) as decimal degrees.
#' @param longitude A numeric vector of the longitude(s) as decimal degrees.
#' @param share_with A character vector of the user group(s) with which to share the location(s), separated by a comma. Default public group is "public_reader".
#' @param location_type A numeric vector of the location type(s) id(s) from table 'location_types'.
#' @param note A character vector of notes for the location(s) (optional).
#' @param contact A character vector of the contact(s) for the location(s) (optional).
#' @param datum_id_from A numeric vector of the datum ID(s) from which the
#'   location(s) are measured, from `public.datum_list`. When `conversion_m` is
#'   omitted, this is set to the Assumed Datum ID.
#' @param datum_id_to A numeric vector of the datum ID(s) to which the
#'   location(s) are measured, from `public.datum_list`. When `conversion_m` is
#'   omitted, this is matched to the vertical datum returned by
#'   [get_elevation()]. A missing datum is added to `public.datum_list`.
#' @param conversion_m A numeric vector of conversion factors from
#'   `datum_id_from` to `datum_id_to`. Missing values are estimated with
#'   [get_elevation()].
#' @param current A logical vector indicating whether the conversion factor(s)
#'   are current. Missing values default to `TRUE`.
#' @param network A numeric vector of the network(s) to which the location(s) belong.
#' @param project A numeric vector of the project(s) to which the location(s) belong.
#' @param con A connection to the aquacache database. Default uses [AquaConnect()]. If left NULL the function will attempt to connect to the database and automatically disconnect afterwards.
#'
#' @return Invisibly, a data.frame describing the new locations and their
#'   datum conversions.
#' @export

addACLocation <- function(
  df = NULL,
  name = NA,
  name_fr = NA,
  alias = NA,
  location_code = NA,
  latitude = NA,
  longitude = NA,
  share_with = NA,
  location_type = NA,
  note = NA,
  contact = NA,
  datum_id_from = NA,
  datum_id_to = NA,
  conversion_m = NA,
  current = NA,
  network = NA,
  project = NA,
  con = NULL
) {
  # df = NULL
  # name = "Test location2"
  # name_fr = "Endroit test2"
  # alias = "TL2"
  # location_code = "TL002"
  # latitude = 66.60114
  # longitude = -138.85132
  # share_with = 'public_reader'
  # location_type = 15
  # datum_id_from = 10
  # datum_id_to = 35
  # conversion_m = 440
  # current = TRUE
  # network = 4
  # project = NA
  # con = con
  # note = NA
  # contact = NA

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (!is.null(df)) {
    # Check that all other parameters are NA
    if (
      !all(is.na(c(
        name,
        name_fr,
        alias,
        location_code,
        latitude,
        longitude,
        share_with,
        location_type,
        note,
        contact,
        datum_id_from,
        datum_id_to,
        conversion_m,
        current
      )))
    ) {
      stop(
        "You cannot provide a data.frame and other parameters at the same time."
      )
    }

    # Check that all required columns are present. Datum fields are optional so
    # callers can request an elevation lookup by omitting them.
    required_columns <- c(
      "name",
      "alias",
      "location_code",
      "latitude",
      "longitude",
      "share_with",
      "location_type",
      "note",
      "contact",
      "network",
      "project"
    )
    if (!all(required_columns %in% colnames(df))) {
      missing <- setdiff(required_columns, colnames(df))
      stop(
        "The data.frame provided does not contain all the necessary columns: missing column(s) ",
        paste(missing, collapse = ", "),
        "."
      )
    }
    # Check that the data.frame is not empty
    if (nrow(df) == 0) {
      stop("The data.frame provided is empty.")
    }
    if (!'name_fr' %in% names(df)) {
      df$name_fr <- NA
      message(
        "You did not provide a column for 'name_fr'. The corresponding database column will be populated with 'Traduction requise!'"
      )
    }
    for (column in c(
      "datum_id_from",
      "datum_id_to",
      "conversion_m",
      "current"
    )) {
      if (!column %in% names(df)) {
        df[[column]] <- NA
      }
    }
    # Assign each column of the data.frame to the corresponding function parameter
    name <- df$name
    name_fr <- df$name_fr
    alias <- df$alias
    location_code <- df$location_code
    latitude <- df$latitude
    longitude <- df$longitude
    share_with <- df$share_with
    location_type <- df$location_type
    note <- df$note
    contact <- df$contact
    datum_id_from <- as.numeric(df$datum_id_from)
    datum_id_to <- as.numeric(df$datum_id_to)
    conversion_m <- as.numeric(df$conversion_m)
    current <- df$current
    network <- df$network
    project <- df$project
  }

  name_fr[is.na(name_fr)] <- 'Traduction requise!'
  name_fr[!nzchar(name_fr)] <- 'Traduction requise!'

  # Convert lat/long to numeric, which will result in NAs if the user provided invalid values
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  datum_id_from <- as.numeric(datum_id_from)
  datum_id_to <- as.numeric(datum_id_to)
  conversion_m <- as.numeric(conversion_m)

  # Begin checks ############################
  lengths <- c(
    length(name),
    length(name_fr),
    length(alias),
    length(location_code),
    length(latitude),
    length(longitude),
    length(share_with),
    length(location_type),
    length(note),
    length(contact),
    length(datum_id_from),
    length(datum_id_to),
    length(conversion_m),
    length(current),
    length(network),
    length(project)
  )

  # Check that the length of each parameter vector is equal, if not stop
  if (!all(lengths == lengths[1])) {
    stop("All parameters must be the same length.")
  }

  # Some parameters can be NA, in which case they get default values.

  missing_code <- is.na(location_code) | trimws(location_code) == ""
  if (any(missing_code)) {
    location_code[missing_code] <- generateACLocationCode(
      latitude = latitude[missing_code],
      longitude = longitude[missing_code],
      location_type = location_type[missing_code],
      con = con
    )
  }
  if (any(is.na(location_code) | trimws(location_code) == "")) {
    stop(
      "location_code cannot contain NA values. It is typically auto-generated by the YGwater Shiny application; provide a valid value if adding locations directly."
    )
  }

  if (any(is.na(share_with))) {
    share_with[is.na(share_with)] <- "public_reader"
  }
  # Check that latitudes and longitudes are decimal degrees, not dms
  if (any(is.na(latitude)) | any(is.na(longitude))) {
    stop(
      "Latitude and longitude must be provided and must be in decimal degrees, not degrees, minutes, seconds."
    )
  }
  if (any(latitude > 90) | any(latitude < -90)) {
    stop("At least one of your latitude entries appears to be invalid.")
  }
  if (any(longitude > 180) | any(longitude < -180)) {
    stop("At least one of your longitude entries appears to be invalid.")
  }

  automatic_elevation <- is.na(conversion_m)
  elevation_details <- vector("list", length(conversion_m))
  if ("elevation_details" %in% names(df)) {
    supplied_details <- df$elevation_details
    if (
      !is.list(supplied_details) ||
        length(supplied_details) != length(conversion_m)
    ) {
      stop(
        "The optional elevation_details column must contain one get_elevation() result per location."
      )
    }
    for (i in which(automatic_elevation)) {
      details <- supplied_details[[i]]
      if (
        is.null(details) ||
          length(details$elevation) != 1L ||
          !is.finite(details$elevation) ||
          length(details$vertical_datum) != 1L ||
          is.na(details$vertical_datum) ||
          !nzchar(trimws(details$vertical_datum))
      ) {
        stop(
          "Supplied elevation_details must include a finite elevation and vertical datum."
        )
      }
      conversion_m[i] <- details$elevation
      elevation_details[[i]] <- details
      datum_id_from[i] <- NA_real_
      datum_id_to[i] <- NA_real_
    }
  }
  if (any(automatic_elevation)) {
    for (i in which(
      automatic_elevation & vapply(elevation_details, is.null, logical(1))
    )) {
      details <- get_elevation(
        lat = latitude[i],
        lon = longitude[i],
        details = TRUE
      )
      if (
        !is.finite(details$elevation) ||
          is.na(details$vertical_datum) ||
          !nzchar(trimws(details$vertical_datum))
      ) {
        stop(
          "No elevation with a known vertical datum could be found for ",
          name[i],
          " (",
          latitude[i],
          ", ",
          longitude[i],
          "). Supply conversion_m, datum_id_from, and datum_id_to manually."
        )
      }
      conversion_m[i] <- details$elevation
      elevation_details[[i]] <- details
      # Elevations returned by get_elevation() describe ground height above an
      # absolute vertical datum, so any caller-supplied datum IDs are replaced.
      datum_id_from[i] <- NA_real_
      datum_id_to[i] <- NA_real_
    }
  }

  manual_elevation <- !automatic_elevation
  datum_id_from[manual_elevation & is.na(datum_id_from)] <- 10
  datum_id_to[manual_elevation & is.na(datum_id_to)] <- 10
  current[is.na(current)] <- TRUE

  # Check that the location code does not already exist
  for (i in location_code) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM public.locations WHERE LOWER(location_code) = $1;",
      params = list(tolower(i))
    )[1, 1]
    if (!is.na(exists)) {
      stop("There is already a location with the code ", i, ".")
    }
  }

  # Check that the location name does not already exist. name_fr CAN already exist, useful if users input locations without a known French name.
  for (i in name) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM public.locations WHERE LOWER(name) = $1;",
      params = list(tolower(i))
    )[1, 1]
    if (!is.na(exists)) {
      stop("There is already a location with the name ", i, ".")
    }
  }

  # Check that there is no location with the same latitude and longitude
  for (i in 1:length(latitude)) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM public.locations WHERE latitude = $1 AND longitude = $2;",
      params = list(latitude[i], longitude[i])
    )[1, 1]
    if (!is.na(exists)) {
      stop(
        "There is already a location with that latitude ",
        latitude[i],
        " and longitude ",
        longitude[i],
        " in the locations table."
      )
    }
  }

  # Check that network and project exist in the 'networks' and 'projects' tables (if not NA)
  if (any(!is.na(network))) {
    network_sub <- network[!is.na(network)]
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT network_id FROM public.networks WHERE network_id IN (",
        paste(network_sub, collapse = ", "),
        ");"
      )
    )
    if (nrow(exists) != length(unique(network_sub))) {
      stop("At least one of the network IDs you specified does not exist.")
    }
  }
  if (any(!is.na(project))) {
    project_sub <- project[!is.na(project)]
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT project_id FROM public.projects WHERE project_id IN (",
        paste(project_sub, collapse = ", "),
        ");"
      )
    )
    if (nrow(exists) != length(unique(project_sub))) {
      stop("At least one of the project IDs you specified does not exist.")
    }
  }

  # Check that datum_id_from and datum_id_to exist in the 'datum_list' table
  unique_datums <- unique(stats::na.omit(c(datum_id_from, datum_id_to)))
  if (length(unique_datums) > 1) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT datum_id FROM public.datum_list WHERE datum_id IN (",
        paste(unique_datums, collapse = ", "),
        ");"
      )
    )
  } else if (length(unique_datums) == 1) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT datum_id FROM public.datum_list WHERE datum_id = $1;",
      params = list(unique_datums)
    )
  } else {
    exists <- data.frame(datum_id = integer())
  }
  if (length(unique_datums) != nrow(exists)) {
    stop("At least one of the datum IDs you specified does not exist.")
  }

  # Check that location_type exists in the 'location_types' table
  unique_location_types <- unique(location_type)
  if (length(unique_location_types) > 1) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT type_id FROM public.location_types WHERE type_id IN (",
        paste(unique_location_types, collapse = ", "),
        ");"
      )
    )
  } else {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT type_id FROM public.location_types WHERE type_id = $1;",
      params = list(unique_location_types)
    )
  }
  if (length(unique_location_types) != nrow(exists)) {
    stop("At least one of the location type IDs you specified does not exist.")
  }

  added <- vector("list", length(location_code))
  for (i in seq_along(location_code)) {
    tryCatch(
      {
        active <- dbTransBegin(con)

        vertical_datum <- NA_character_
        elevation_source <- NA_character_
        if (automatic_elevation[i]) {
          datum_id_from[i] <- .match_or_create_location_datum(
            con = con,
            datum_name = "ASSUMED DATUM",
            create = FALSE
          )
          vertical_datum <- elevation_details[[i]]$vertical_datum
          datum_id_to[i] <- .match_or_create_location_datum(
            con = con,
            datum_name = vertical_datum,
            create = TRUE
          )
          elevation_source <- elevation_details[[i]]$source
        }

        # Add the location to the 'locations' table ############################
        location_id <- DBI::dbGetQuery(
          con,
          "INSERT INTO public.locations (location_code, name, name_fr, alias, latitude, longitude, share_with, location_type, note, contact) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10) RETURNING location_id;",
          params = list(
            location_code[i],
            name[i],
            name_fr[i],
            alias[i],
            latitude[i],
            longitude[i],
            paste0("{", share_with[i], "}"),
            location_type[i],
            note[i],
            contact[i]
          )
        )[1, 1]

        # Add the location's datum information to the 'datums' table ############################
        DBI::dbExecute(
          con,
          "INSERT INTO public.datum_conversions (location_id, datum_id_from, datum_id_to, conversion_m, current) VALUES ($1, $2, $3, $4, $5);",
          params = list(
            location_id,
            datum_id_from[i],
            datum_id_to[i],
            conversion_m[i],
            current[i]
          )
        )

        # Add entries to the project and network tables ############################
        if (!is.na(network[i])) {
          DBI::dbExecute(
            con,
            "INSERT INTO public.locations_networks (location_id, network_id) VALUES ($1, $2);",
            params = list(
              location_id,
              network[i]
            )
          )
        }

        if (!is.na(project[i])) {
          DBI::dbExecute(
            con,
            "INSERT INTO public.locations_projects (location_id, project_id) VALUES ($1, $2);",
            params = list(
              location_id,
              project[i]
            )
          )
        }

        message(
          "Added a new entry to the locations table for location ",
          name[i],
          "."
        )

        added[[i]] <- data.frame(
          location_id = as.integer(location_id),
          location_code = location_code[i],
          name = name[i],
          elevation_m = conversion_m[i],
          datum_id_from = as.integer(datum_id_from[i]),
          datum_id_to = as.integer(datum_id_to[i]),
          vertical_datum = vertical_datum,
          elevation_source = elevation_source,
          elevation_estimated = automatic_elevation[i],
          stringsAsFactors = FALSE
        )

        if (active) {
          DBI::dbExecute(con, "COMMIT;")
        }
      },
      error = function(e) {
        if (active) {
          DBI::dbExecute(con, "ROLLBACK;")
        }
        stop("Error adding location ", name[i], ": ", e$message)
      }
    ) # end tryCatch
  } # end for loop

  invisible(do.call(rbind, added))
}


.match_or_create_location_datum <- function(
  con,
  datum_name,
  create = TRUE
) {
  datums <- DBI::dbGetQuery(
    con,
    "SELECT datum_id, datum_name_en FROM public.datum_list ORDER BY datum_id"
  )

  match_datum <- function(datums, datum_name) {
    normalize <- function(x) {
      toupper(gsub("[^[:alnum:]]", "", trimws(x)))
    }
    target <- normalize(datum_name)
    exact <- which(normalize(datums$datum_name_en) == target)
    if (length(exact)) {
      return(datums$datum_id[exact[1]])
    }

    # CDEM/CDSM elevations are estimates in CGVD28. AquaCache's established
    # datum for derived elevations is CGVD28 (approximate), rather than one of
    # the year-specific or assumed CGVD28 entries inherited from HYDAT.
    if (identical(target, "CGVD28")) {
      approximate <- which(
        normalize(datums$datum_name_en) == "CGVD28APPROXIMATE"
      )
      if (length(approximate)) {
        return(datums$datum_id[approximate[1]])
      }
    }

    NA_integer_
  }

  datum_id <- match_datum(datums, datum_name)
  if (!is.na(datum_id)) {
    return(as.integer(datum_id))
  }
  if (!isTRUE(create)) {
    stop("Required datum '", datum_name, "' is missing from public.datum_list.")
  }

  # datum_list predates identity columns. Lock it while assigning MAX + 1 so
  # concurrent location imports cannot choose the same datum_id.
  DBI::dbExecute(
    con,
    "LOCK TABLE public.datum_list IN SHARE ROW EXCLUSIVE MODE"
  )
  datums <- DBI::dbGetQuery(
    con,
    "SELECT datum_id, datum_name_en FROM public.datum_list ORDER BY datum_id"
  )
  datum_id <- match_datum(datums, datum_name)
  if (!is.na(datum_id)) {
    return(as.integer(datum_id))
  }

  DBI::dbGetQuery(
    con,
    "INSERT INTO public.datum_list (datum_id, datum_name_en, datum_name_fr)
     SELECT COALESCE(MAX(datum_id), 0) + 1, $1, $1
     FROM public.datum_list
     RETURNING datum_id",
    params = list(datum_name)
  )$datum_id[[1]]
}
