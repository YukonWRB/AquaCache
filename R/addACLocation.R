#' Add location to AquaCache
#' 
#' Adds a new location to the AquaCache 'locations' table. You can pass a data.frame with the necessary columns, or provide each parameter separately. Extensive checks are performed to ensure that the location does not already exist, and that all necessary parameters are provided and are valid.
#'
#' @param df A data.frame containing the following columns: location, name, name_fr, latitude, longitude, visibility_public, share_with, owner, data_sharing_agreement_id, location_type, note, contact, datum_id_from, datum_id_to, conversion_m, current, network, project. If this parameter is provided, all other parameters must be NA or left as their default values.
#' @param location A character vector of the location code(s).
#' @param name A character vector of the location name(s).
#' @param name_fr A character vector of the location name(s) in French.
#' @param latitude A numeric vector of the latitude(s).
#' @param longitude A numeric vector of the longitude(s).
#' @param visibility_public A character vector of the visibility of the location(s). Default is 'exact'.
#' @param share_with A numeric vector of the user group(s) with which to share the location(s). Default is 1.
#' @param owner A numeric vector of the owner(s) of the location(s).
#' @param data_sharing_agreement_id A numeric vector of the data sharing agreement(s) for the location(s).
#' @param location_type A numeric vector of the location type(s).
#' @param note A character vector of notes for the location(s).
#' @param contact A character vector of the contact(s) for the location(s).
#' @param datum_id_from A numeric vector of the datum ID(s) from which the location(s) are measured.
#' @param datum_id_to A numeric vector of the datum ID(s) to which the location(s) are measured.
#' @param conversion_m A numeric vector of the conversion factor(s) from the datum_id_from to the datum_id_to.
#' @param current A logical vector of whether the conversion factor(s) are current.
#' @param network A numeric vector of the network(s) to which the location(s) belong.
#' @param project A numeric vector of the project(s) to which the location(s) belong.
#' @param con A connection to the AquaCache database. Default uses [AquaConnect()]. If left NULL the function will attempt to connect to the database and automatically disconnect afterwards.
#'
#' @return Success/error messages and new entries added to the database.
#' @export

addACLocation <- function(df = NULL, location = NA, name = NA, name_fr = NA, latitude = NA, longitude = NA, visibility_public = NA, share_with = 1, owner = NA, data_sharing_agreement_id = NA, location_type = NA, note = NA, contact = NA, datum_id_from = NA, datum_id_to = NA, conversion_m = NA, current = NA, network = NA, project = NA, con = NULL) {
  
   
  # df <- data.frame(location = "Yukon_Abv_YDA",
  #                  name = "Yukon River Above Dawson",
  #                  name_fr = "Riviere Yukon en amont de Dawson",
  #                  latitude = 63.99564,
  #                  longitude = -139.65884,
  #                  visibility_public = "exact",
  #                  share_with = 1,
  #                  owner = NA,
  #                  data_sharing_agreement_id = NA,
  #                  location_type = 1,
  #                  datum_id_from = 10,
  #                  datum_id_to = 10,
  #                  conversion_m = 0,
  #                  current = TRUE,
  #                  network = 1,
  #                  project = NA,
  #                  note = "NuPoint camera maintained by WSC, no flow/level monitoring.",
  #                  contact = NA)
  # 
  # location <- NA
  # name <- NA
  # name_fr <- NA
  # latitude <- NA
  # longitude <- NA
  # visibility_public <- NA
  # share_with <- NA
  # owner <- NA
  # data_sharing_agreement_id <- NA
  # location_type <- NA
  # note <- NA
  # contact <- NA
  # datum_id_from <- NA
  # datum_id_to <- NA
  # conversion_m <- NA
  # current <- NA
  # network <- NA
  # project <- NA
  # 
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  if (!is.null(df)) {
    # Check that all other parameters are NA
    if (!all(is.na(c(location, name, name_fr, latitude, longitude, visibility_public, share_with, owner, data_sharing_agreement_id, location_type, note, contact, datum_id_from, datum_id_to, conversion_m, current)))) {
      stop("You cannot provide a data.frame and other parameters at the same time.")
    }
    
    # Check that there is a column name for each function parameter that is not 'df'
    if (!all(c("location", "name", "name_fr", "latitude", "longitude", "visibility_public", "share_with", "owner", "data_sharing_agreement_id", "location_type", "note", "contact", "datum_id_from", "datum_id_to", "conversion_m", "current") %in% colnames(df))) {
      stop("The data.frame provided does not contain all the necessary columns.")
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
    visibility_public <- df$visibility_public
    share_with <- df$share_with
    owner <- df$owner
    data_sharing_agreement_id <- df$data_sharing_agreement_id
    location_type <- df$location_type
    note <- df$note
    contact <- df$contact
    datum_id_from <- df$datum_id_from
    datum_id_to <- df$datum_id_to
    conversion_m <- df$conversion_m
    current <- df$current
    network <- df$network
    project <- df$project
  }
  
  
  # Begin checks ############################
  lengths <- c(length(location), length(name), length(name_fr), length(latitude), length(longitude), length(visibility_public), length(share_with), length(owner), length(data_sharing_agreement_id), length(location_type), length(note), length(contact), length(datum_id_from), length(datum_id_to), length(conversion_m), length(current), length(network), length(project))
  
  # Check that the length of each parameter vector is equal, if not stop
  if (!all(lengths == lengths[1])) {
    stop("All parameters must be the same length.")
  }
  
  # Some parameters can be NA, in which case they get default values
  if (any(is.na(share_with))) {
    share_with[is.na(share_with)] <- 1
  }
  if (any(is.na(visibility_public))) {
    visibility_public[is.na(visibility_public)] <- "exact"
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
  
  # Check that the location does not already exist
  exists <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  
  if (!is.na(exists)) {
    stop("There is already a location with that name.")
  }
  
  # Check that there is no location with the same latitude and longitude
  exists <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE latitude = ", latitude, " AND longitude = ", longitude, ";"))[1,1]
  if (!is.na(exists)) {
    stop("There is already a location with that latitude and longitude.")
  }

  
  # Check that network and project exist in the 'networks' and 'projects' tables (if not NA)
  if (any(!is.na(network))) {
    network_sub <- network[!is.na(network)]
    if (length(network_sub > 1)) {
      exists <- DBI::dbGetQuery(con, paste0("SELECT network_id FROM networks WHERE network_id IN (", paste(network_sub, collapse = ", "), ");"))
      if (is.na(exists)) {
        stop("At least one of the network IDs you specified does not exist.")
      }
    } else {
      exists <- DBI::dbGetQuery(con, paste0("SELECT network_id FROM networks WHERE network_id = ", network, ";"))[1,1]
      if (is.na(exists)) {
        stop("The network_id you specified does not exist.")
      }
    }
  }
  if (any(!is.na(project))) {
    project_sub <- project[!is.na(project)]
    if (length(project_sub > 1)) {
      exists <- DBI::dbGetQuery(con, paste0("SELECT project_id FROM projects WHERE project_id IN (", paste(project_sub, collapse = ", "), ");"))
      if (is.na(exists)) {
        stop("At least one of the project IDs you specified does not exist.")
      }
    } else {
      exists <- DBI::dbGetQuery(con, paste0("SELECT project_id FROM projects WHERE project_id = ", project, ";"))[1,1]
      if (is.na(exists)) {
        stop("The project_id you specified does not exist.")
      }
    }
  }
  
  # Check that datum_id_from and datum_id_to exist in the 'datum_list' table
  unique_datums <- unique(c(datum_id_from, datum_id_to))
  if (length(unique_datums) > 1) {
      exists <- DBI::dbGetQuery(con, paste0("SELECT datum_id FROM datum_list WHERE datum_id IN (", paste(unique_datums, collapse = ", "), ");"))
  } else {
    exists <- DBI::dbGetQuery(con, paste0("SELECT datum_id FROM datum_list WHERE datum_id = ", unique_datums, ";"))[1,1]
  }
  if (length(unique_datums) != nrow(exists)) {
    stop("At least one of the datum IDs you specified does not exist.")
  }
  
  # Check that location_type exists in the 'location_types' table
  unique_location_types <- unique(location_type)
  if (length(unique_location_types) > 1) {
    exists <- DBI::dbGetQuery(con, paste0("SELECT location_type_id FROM location_types WHERE location_type_id IN (", paste(unique_location_types, collapse = ", "), ");"))
  } else {
    exists <- DBI::dbGetQuery(con, paste0("SELECT location_type_id FROM location_types WHERE location_type_id = ", unique_location_types, ";"))[1,1]
  }
  if (length(unique_location_types) != nrow(exists)) {
    stop("At least one of the location type IDs you specified does not exist.")
  }
  
  # Check that owner exists in the 'owners_contributors' table
  unique_owners <- unique(owner)
  if (length(unique_owners) > 1) {
    exists <- DBI::dbGetQuery(con, paste0("SELECT owner_id FROM owners_contributors WHERE owner_id IN (", paste(unique_owners, collapse = ", "), ");"))
  } else {
    exists <- DBI::dbGetQuery(con, paste0("SELECT owner_id FROM owners_contributors WHERE owner_id = ", unique_owners, ";"))[1,1]
  }
  if (length(unique_owners) != nrow(exists)) {
    stop("At least one of the owner IDs you specified does not exist.")
  }
  
  # Check that data_sharing_agreement_id exists in the 'data_sharing_agreements' table
  unique_data_sharing_agreements <- unique(data_sharing_agreement_id)
  if (length(unique_data_sharing_agreements) > 1) {
    exists <- DBI::dbGetQuery(con, paste0("SELECT data_sharing_agreement_id FROM data_sharing_agreements WHERE data_sharing_agreement_id IN (", paste(unique_data_sharing_agreements, collapse = ", "), ");"))
  } else {
    exists <- DBI::dbGetQuery(con, paste0("SELECT data_sharing_agreement_id FROM data_sharing_agreements WHERE data_sharing_agreement_id = ", unique_data_sharing_agreements, ";"))[1,1]
  }
  if (length(unique_data_sharing_agreements) != nrow(exists)) {
    stop("At least one of the data sharing agreement IDs you specified does not exist.")
  }
  
  
  for (i in 1:length(location)) {
    # Add the location to the 'vectors' table ############################
    # Check if there's already a point at this location
    exists <- DBI::dbGetQuery(con, paste0("SELECT geom_id, ST_Y(geom) AS latitude, ST_X(geom) AS longitude FROM vectors WHERE layer_name = 'Locations' AND feature_name = '", location[i], "';"))
    
    if (nrow(exists) == 0) { # Add the point to the table
      point <- data.frame("feature_name" = location[i],
                          "description" = name[i],
                          "latitude" = latitude[i],
                          "longitude" = longitude[i])
      point <- terra::vect(point, geom = c("longitude", "latitude"), crs = "epsg:4269")
      insertACVector(point, "Locations", feature_name_col = "feature_name", description_col = "description")
      geom_id <- DBI::dbGetQuery(con, paste0("SELECT geom_id FROM vectors WHERE layer_name = 'Locations' AND feature_name = '", location[i], "';"))[1,1]
      
    } else {
      geom_id <- exists$geom_id
      message("Location ", location[i], " already exists in the 'vectors' table.")
    }
    
    # Add the location to the 'locations' table ############################
    location <- data.frame(location = location[i],
                           name = name[i],
                           name_fr = name_fr[i],
                           latitude = latitude[i],
                           longitude = longitude[i],
                           visibility_public = visibility_public[i],
                           share_with = paste0("{", share_with[i], "}"),
                           owner = owner[i],
                           data_sharing_agreement_id = data_sharing_agreement_id[i],
                           location_type = location_type[i],
                           note = note[i],
                           contact = contact[i],
                           geom_id = geom_id)
    DBI::dbAppendTable(con, "locations", location)
    
    # Get the location_id of the new location
    location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location[i], "';"))[1,1]
    
    # Add the location's datum information to the 'datums' table ############################
    datum <- data.frame(location_id = location_id,
                        datum_id_from = datum_id_from[i],
                        datum_id_to = datum_id_to[i],
                        conversion_m = conversion_m[i],
                        current = current[i])
    DBI::dbAppendTable(con, "datum_conversions", datum)

    
    # Add entries to the project and network tables ############################
    if (!is.na(network[i])) {
      tbl <- data.frame(location_id = location_id, network_id = network[i])
      DBI::dbAppendTable(con, "locations_networks", tbl)
    }
    
    if (!is.na(project[i])) {
      tbl <- data.frame(location_id = location_id, project_id = project[i])
      DBI::dbAppendTable(con, "locations_projects", tbl)
    }

    message("Added a new entry to the locations table for location ", location[i], ".")
  } 
}
