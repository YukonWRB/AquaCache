#' Add location to aquacache
#'
#' Adds a new location to the aquacache 'locations' table. You can pass a data.frame with the necessary columns, or provide each parameter separately. Extensive checks are performed to ensure that the location does not already exist, and that all necessary parameters are provided and are valid.
#'
#' @param df A data.frame containing the following columns: location, name, name_fr, latitude, longitude, share_with, owner, data_sharing_agreement_id, location_type, note, contact, datum_id_from, datum_id_to, conversion_m, current, network, project. If this parameter is provided, all other parameters except for `con` must be left as their default values.
#' @param location A character vector of the location code(s).
#' @param name A character vector of the location name(s).
#' @param name_fr A character vector of the location name(s) in French.
#' @param latitude A numeric vector of the latitude(s) as decimal degrees.
#' @param longitude A numeric vector of the longitude(s) as decimal degrees.
#' @param share_with A character vector of the user group(s) with which to share the location(s), separated by a comma. Default public group is "public_reader".
#' @param owner A numeric vector of the owner(s) of the location(s).
#' @param data_sharing_agreement_id A numeric vector of the data sharing agreement(s) for the location(s) from column 'document_id' of the 'documents' table.
#' @param location_type A numeric vector of the location type(s) id(s) from table 'location_types'.
#' @param note A character vector of notes for the location(s) (optional).
#' @param contact A character vector of the contact(s) for the location(s) (optional).
#' @param datum_id_from A numeric vector of the datum ID(s) from which the location(s) are measured, from table 'datum_list'.
#' @param datum_id_to A numeric vector of the datum ID(s) to which the location(s) are measured, from table 'datum_list'.
#' @param conversion_m A numeric vector of the conversion factor(s) from the datum_id_from to the datum_id_to.
#' @param current A logical vector of whether the conversion factor(s) are current.
#' @param network A numeric vector of the network(s) to which the location(s) belong.
#' @param project A numeric vector of the project(s) to which the location(s) belong.
#' @param con A connection to the aquacache database. Default uses [AquaConnect()]. If left NULL the function will attempt to connect to the database and automatically disconnect afterwards.
#'
#' @return Success/error messages and new entries added to the database.
#' @export

addACLocation <- function(
  df = NULL,
  location = NA,
  name = NA,
  name_fr = NA,
  latitude = NA,
  longitude = NA,
  share_with = NA,
  owner = NA,
  data_sharing_agreement_id = NA,
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
  # location = "TEST02"
  # name = "Test location2"
  # name_fr = "Endroit test2"
  # latitude = 66.60114
  # longitude = -138.85132
  # share_with = 'public_reader'
  # owner = 2
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
  # data_sharing_agreement_id = NA

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  if (!is.null(df)) {
    # Check that all other parameters are NA
    if (
      !all(is.na(c(
        location,
        name,
        name_fr,
        latitude,
        longitude,
        share_with,
        owner,
        data_sharing_agreement_id,
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

    # Check that there is a column name for each function parameter that is not 'df'
    if (
      !all(
        c(
          "location",
          "name",
          "name_fr",
          "latitude",
          "longitude",
          "share_with",
          "owner",
          "data_sharing_agreement_id",
          "location_type",
          "note",
          "contact",
          "datum_id_from",
          "datum_id_to",
          "conversion_m",
          "current"
        ) %in%
          colnames(df)
      )
    ) {
      missing <- setdiff(
        c(
          "location",
          "name",
          "name_fr",
          "latitude",
          "longitude",
          "share_with",
          "owner",
          "data_sharing_agreement_id",
          "location_type",
          "note",
          "contact",
          "datum_id_from",
          "datum_id_to",
          "conversion_m",
          "current"
        ),
        colnames(df)
      )
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
    # Assign each column of the data.frame to the corresponding function parameter
    location <- df$location
    name <- df$name
    name_fr <- df$name_fr
    latitude <- df$latitude
    longitude <- df$longitude
    share_with <- df$share_with
    owner <- df$owner
    data_sharing_agreement_id <- df$data_sharing_agreement_id
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

  # Convert lat/long to numeric, which will result in NAs if the user provided invalid values
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)

  # Begin checks ############################
  lengths <- c(
    length(location),
    length(name),
    length(name_fr),
    length(latitude),
    length(longitude),
    length(share_with),
    length(owner),
    length(data_sharing_agreement_id),
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

  # Some parameters can be NA, in which case they get default values
  if (any(is.na(share_with))) {
    share_with[is.na(share_with)] <- "public_reader"
  }
  if (any(is.na(datum_id_from))) {
    datum_id_from[is.na(datum_id_from)] <- 10
  }
  if (any(is.na(datum_id_to))) {
    datum_id_to[is.na(datum_id_to)] <- 10
  }
  if (any(is.na(conversion_m))) {
    conversion_m[is.na(conversion_m)] <- 0
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

  # Check that the location code does not already exist
  for (i in location) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT location_id FROM locations WHERE LOWER(location) = '",
        tolower(i),
        "';"
      )
    )[1, 1]
    if (!is.na(exists)) {
      stop("There is already a location with the code ", i, ".")
    }
  }

  # Check that the location name (name_fr later) does not already exist
  for (i in name) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT location_id FROM locations WHERE LOWER(name) = '",
        tolower(i),
        "';"
      )
    )[1, 1]
    if (!is.na(exists)) {
      stop("There is already a location with the name ", i, ".")
    }
  }
  for (i in name_fr) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM locations WHERE LOWER(name_fr) = $1;",
      params = list(tolower(i))
    )[1, 1]
    if (!is.na(exists)) {
      stop("There is already a location with the French name ", i, ".")
    }
  }

  # Check that there is no location with the same latitude and longitude
  for (i in 1:length(latitude)) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT location_id FROM locations WHERE latitude = ",
        latitude[i],
        " AND longitude = ",
        longitude[i],
        ";"
      )
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
        "SELECT network_id FROM networks WHERE network_id IN (",
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
        "SELECT project_id FROM projects WHERE project_id IN (",
        paste(project_sub, collapse = ", "),
        ");"
      )
    )
    if (nrow(exists) != length(unique(project_sub))) {
      stop("At least one of the project IDs you specified does not exist.")
    }
  }

  # Check that datum_id_from and datum_id_to exist in the 'datum_list' table
  unique_datums <- unique(c(datum_id_from, datum_id_to))
  if (length(unique_datums) > 1) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT datum_id FROM datum_list WHERE datum_id IN (",
        paste(unique_datums, collapse = ", "),
        ");"
      )
    )
  } else {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT datum_id FROM datum_list WHERE datum_id = ",
        unique_datums,
        ";"
      )
    )
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
        "SELECT type_id FROM location_types WHERE type_id IN (",
        paste(unique_location_types, collapse = ", "),
        ");"
      )
    )
  } else {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT type_id FROM location_types WHERE type_id = ",
        unique_location_types,
        ";"
      )
    )
  }
  if (length(unique_location_types) != nrow(exists)) {
    stop("At least one of the location type IDs you specified does not exist.")
  }

  # Check that owner exists in the 'organizations' table
  unique_owners <- unique(owner)
  if (length(unique_owners) > 1) {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT organization_id FROM organizations WHERE organization_id IN (",
        paste(unique_owners, collapse = ", "),
        ");"
      )
    )
  } else {
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT organization_id FROM organizations WHERE organization_id = ",
        unique_owners,
        ";"
      )
    )
  }
  if (length(unique_owners) != nrow(exists)) {
    stop(
      "At least one of the owner IDs you specified does not exist. You can add it with function addACOrg()."
    )
  }

  # Check that data_sharing_agreement_id exists in the 'documents' table
  unique_data_sharing_agreements <- unique(data_sharing_agreement_id)
  if (!is.na(unique_data_sharing_agreements)) {
    if (length(unique_data_sharing_agreements) > 1) {
      exists <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT document_id FROM documents WHERE document_id IN (",
          paste(unique_data_sharing_agreements, collapse = ", "),
          ");"
        )
      )
    } else {
      exists <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT document_id FROM documents WHERE document_id = ",
          unique_data_sharing_agreements,
          ";"
        )
      )
    }
    if (length(unique_data_sharing_agreements) != nrow(exists)) {
      stop(
        "At least one of the data sharing agreement IDs you specified does not exist."
      )
    }
  }

  active <- dbTransBegin(con)
  tryCatch(
    {
      for (i in 1:length(location)) {
        # Add the location to the 'vectors' table ############################
        # Check if there's already a point with the exact same name
        exists <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT geom_id, ST_Y(geom) AS latitude, ST_X(geom) AS longitude FROM spatial.vectors WHERE layer_name = 'Locations' AND LOWER(feature_name) = '",
            tolower(location[i]),
            "';"
          )
        )

        if (nrow(exists) == 0) {
          # Add the point to the table
          point <- data.frame(
            "feature_name" = location[i],
            "description" = name[i],
            "latitude" = latitude[i],
            "longitude" = longitude[i]
          )
          point <- terra::vect(
            point,
            geom = c("longitude", "latitude"),
            crs = "epsg:4269"
          )
          insertACVector(
            geom = point,
            layer_name = "Locations",
            feature_name_col = "feature_name",
            description_col = "description",
            con = con
          )
          geom_id <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT geom_id FROM spatial.vectors WHERE layer_name = 'Locations' AND feature_name = '",
              location[i],
              "';"
            )
          )[1, 1]
        } else {
          geom_id <- exists$geom_id
          message(
            "Location ",
            location[i],
            " already exists in the 'vectors' table."
          )
        }

        # Add the location to the 'locations' table ############################
        location_id <- DBI::dbGetQuery(
          con,
          "INSERT INTO locations (location, name, name_fr, latitude, longitude, share_with, data_sharing_agreement_id, location_type, note, contact, geom_id) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11) RETURNING location_id;",
          params = list(
            location[i],
            name[i],
            name_fr[i],
            latitude[i],
            longitude[i],
            paste0("{", paste(share_with, collapse = ", "), "}"),
            data_sharing_agreement_id[i],
            location_type[i],
            note[i],
            contact[i],
            geom_id
          )
        )[1, 1]

        # Deal with ownership information ############################
        DBI::dbExecute(
          con,
          "INSERT INTO locations_metadata_owners_operators (location_id, owner, operator, start_datetime) VALUES ($1, $2, $3, $4);",
          params = list(
            location_id,
            owner[i],
            owner[i],
            "1970-01-01"
          )
        )

        # Add the location's datum information to the 'datums' table ############################
        DBI::dbExecute(
          con,
          "INSERT INTO datum_conversions (location_id, datum_id_from, datum_id_to, conversion_m, current) VALUES ($1, $2, $3, $4, $5);",
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
            "INSERT INTO locations_networks (location_id, network_id) VALUES ($1, $2);",
            params = list(
              location_id,
              network[i]
            )
          )
        }

        if (!is.na(project[i])) {
          DBI::dbExecute(
            con,
            "INSERT INTO locations_projects (location_id, project_id) VALUES ($1, $2);",
            params = list(
              location_id,
              project[i]
            )
          )
        }

        message(
          "Added a new entry to the locations table for location ",
          location[i],
          "."
        )
      }
      if (active) {
        DBI::dbExecute(con, "COMMIT;")
      }
    },
    error = function(e) {
      if (active) {
        DBI::dbExecute(con, "ROLLBACK;")
      }
      stop("Error adding location. No changes were made. Error: ", e$message)
    }
  )
}
