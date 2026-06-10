#' Add location to aquacache
#'
#' Adds a new location to the aquacache 'locations' table. You can pass a data.frame with the necessary columns, or provide each parameter separately. Extensive checks are performed to ensure that the location does not already exist, and that all necessary parameters are provided and are valid.
#'
#' @param df A data.frame containing the following columns: name, name_fr, alias, location_code, latitude, longitude, share_with, location_type, note, contact, datum_id_from, datum_id_to, conversion_m, current, network, project. Optional columns are exact_share_with, public_geom_type, min_offset_m, max_offset_m, public_accuracy_m, mask_method, and mask_seed. If this parameter is provided, all other parameters except for `con` must be left as their default values.
#' @param name A character vector of the location name(s).
#' @param name_fr A character vector of the location name(s) in French.
#' @param alias A character vector of the location alias(es). This is optional, leave NA if not needed.
#' @param location_code A character vector of the location code(s). Note that in most cases this should be auto-generated when by adding a new location using the YGwater Shiny application!
#' @param latitude A numeric vector of the latitude(s) as decimal degrees.
#' @param longitude A numeric vector of the longitude(s) as decimal degrees.
#' @param share_with A character vector of the user group(s) with which to share the location(s), separated by a comma. Default public group is "public_reader".
#' @param exact_share_with A character vector of the user group(s) allowed to see exact coordinates in public-safe views, separated by a comma. Defaults to `share_with` when the database has the `locations.exact_share_with` column.
#' @param public_geom_type Public geometry to create for the location when the database has `location_public_geometries`. One of "exact_point" or "masked_point". Defaults to "exact_point".
#' @param min_offset_m Minimum masking offset in metres for `public_geom_type = "masked_point"`.
#' @param max_offset_m Maximum masking offset in metres for `public_geom_type = "masked_point"`.
#' @param public_accuracy_m Public geometry accuracy in metres. Defaults to 0 for exact points and `max_offset_m` for masked points.
#' @param mask_method Optional public geometry method label. Defaults to "exact" or "stable_toroid".
#' @param mask_seed Optional deterministic seed for masked points. Defaults to `location_code`.
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
  name = NA,
  name_fr = NA,
  alias = NA,
  location_code = NA,
  latitude = NA,
  longitude = NA,
  share_with = NA,
  exact_share_with = NA,
  public_geom_type = "exact_point",
  min_offset_m = NA,
  max_offset_m = NA,
  public_accuracy_m = NA,
  mask_method = NA,
  mask_seed = NA,
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

    # Check that there is a column name for each function parameter that is not 'df'
    if (
      !all(
        c(
          "name",
          "name_fr",
          "alias",
          "location_code",
          "latitude",
          "longitude",
          "share_with",
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
          "name",
          "name_fr",
          "alias",
          "location_code",
          "latitude",
          "longitude",
          "share_with",
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
    name <- df$name
    name_fr <- df$name_fr
    alias <- df$alias
    location_code <- df$location_code
    latitude <- df$latitude
    longitude <- df$longitude
    share_with <- df$share_with
    if ("exact_share_with" %in% names(df)) {
      exact_share_with <- df$exact_share_with
    }
    if ("public_geom_type" %in% names(df)) {
      public_geom_type <- df$public_geom_type
    }
    if ("min_offset_m" %in% names(df)) {
      min_offset_m <- df$min_offset_m
    }
    if ("max_offset_m" %in% names(df)) {
      max_offset_m <- df$max_offset_m
    }
    if ("public_accuracy_m" %in% names(df)) {
      public_accuracy_m <- df$public_accuracy_m
    }
    if ("mask_method" %in% names(df)) {
      mask_method <- df$mask_method
    }
    if ("mask_seed" %in% names(df)) {
      mask_seed <- df$mask_seed
    }
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
  min_offset_m <- as.numeric(min_offset_m)
  max_offset_m <- as.numeric(max_offset_m)
  public_accuracy_m <- as.numeric(public_accuracy_m)

  recycle_arg <- function(x, n, arg_name) {
    if (length(x) == n) {
      return(x)
    }
    if (length(x) == 1L) {
      return(rep(x, n))
    }
    stop(arg_name, " must be length 1 or the same length as name/location_code.")
  }

  n_locations <- length(name)
  public_geom_type <- recycle_arg(public_geom_type, n_locations, "public_geom_type")
  exact_share_with <- recycle_arg(exact_share_with, n_locations, "exact_share_with")
  min_offset_m <- recycle_arg(min_offset_m, n_locations, "min_offset_m")
  max_offset_m <- recycle_arg(max_offset_m, n_locations, "max_offset_m")
  public_accuracy_m <- recycle_arg(public_accuracy_m, n_locations, "public_accuracy_m")
  mask_method <- recycle_arg(mask_method, n_locations, "mask_method")
  mask_seed <- recycle_arg(mask_seed, n_locations, "mask_seed")

  # Begin checks ############################
  lengths <- c(
    length(name),
    length(name_fr),
    length(alias),
    length(location_code),
    length(latitude),
    length(longitude),
    length(share_with),
    length(exact_share_with),
    length(public_geom_type),
    length(min_offset_m),
    length(max_offset_m),
    length(public_accuracy_m),
    length(mask_method),
    length(mask_seed),
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
  if (any(is.na(exact_share_with))) {
    exact_share_with[is.na(exact_share_with)] <- share_with[is.na(exact_share_with)]
  }
  if (any(is.na(public_geom_type) | trimws(public_geom_type) == "")) {
    public_geom_type[is.na(public_geom_type) | trimws(public_geom_type) == ""] <- "exact_point"
  }
  public_geom_type <- trimws(public_geom_type)
  if (!all(public_geom_type %in% c("exact_point", "masked_point"))) {
    stop("public_geom_type must be one of 'exact_point' or 'masked_point'.")
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
  masked <- public_geom_type == "masked_point"
  if (any(masked)) {
    if (any(is.na(min_offset_m[masked])) || any(is.na(max_offset_m[masked]))) {
      stop("min_offset_m and max_offset_m must be provided for masked public geometry.")
    }
    if (any(min_offset_m[masked] < 0) || any(max_offset_m[masked] < 0)) {
      stop("min_offset_m and max_offset_m must be non-negative.")
    }
    if (any(max_offset_m[masked] < min_offset_m[masked])) {
      stop("max_offset_m must be greater than or equal to min_offset_m.")
    }
  }

  role_array_literal <- function(x) {
    roles <- trimws(unlist(strsplit(as.character(x), ","), use.names = FALSE))
    roles <- roles[nzchar(roles)]
    if (length(roles) == 0L) {
      stop("Role arrays cannot be empty.")
    }
    paste0("{", paste(roles, collapse = ","), "}")
  }

  patch_48_support <- DBI::dbGetQuery(
    con,
    "SELECT
       EXISTS (
         SELECT 1
         FROM information_schema.columns
         WHERE table_schema = 'public'
           AND table_name = 'locations'
           AND column_name = 'exact_share_with'
       ) AS has_exact_share_with,
       to_regclass('public.location_public_geometries') IS NOT NULL
         AS has_public_geometries,
       to_regprocedure(
         'public.location_masked_point(numeric,numeric,numeric,numeric,text)'
       ) IS NOT NULL AS has_masked_point"
  )
  has_exact_share_with <- isTRUE(patch_48_support$has_exact_share_with[[1]])
  has_public_geometries <- isTRUE(patch_48_support$has_public_geometries[[1]])
  has_masked_point <- isTRUE(patch_48_support$has_masked_point[[1]])
  if (any(masked) && !has_masked_point) {
    stop("Masked public geometry requires public.location_masked_point().")
  }

  # Check that the location code does not already exist
  for (i in location_code) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM locations WHERE LOWER(location_code) = $1;",
      params = list(tolower(i))
    )[1, 1]
    if (!is.na(exists)) {
      stop("There is already a location with the code ", i, ".")
    }
  }

  # Check that the location name (name_fr later) does not already exist
  for (i in name) {
    exists <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM locations WHERE LOWER(name) = $1;",
      params = list(tolower(i))
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
      "SELECT location_id FROM locations WHERE latitude = $1 AND longitude = $2;",
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
      "SELECT datum_id FROM datum_list WHERE datum_id = $1;",
      params = list(unique_datums)
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
      "SELECT type_id FROM location_types WHERE type_id = $1;",
      params = list(unique_location_types)
    )
  }
  if (length(unique_location_types) != nrow(exists)) {
    stop("At least one of the location type IDs you specified does not exist.")
  }

  for (i in 1:length(location_code)) {
    tryCatch(
      {
        active <- dbTransBegin(con)

        # Add the location to the 'locations' table ############################
        insert_columns <- c(
          "location_code",
          "name",
          "name_fr",
          "alias",
          "latitude",
          "longitude",
          "share_with",
          "location_type",
          "note",
          "contact"
        )
        insert_params <- list(
            location_code[i],
            name[i],
            name_fr[i],
            alias[i],
            latitude[i],
            longitude[i],
            role_array_literal(share_with[i]),
            location_type[i],
            note[i],
            contact[i]
        )
        if (has_exact_share_with) {
          insert_columns <- c(insert_columns, "exact_share_with")
          insert_params <- c(
            insert_params,
            list(role_array_literal(exact_share_with[i]))
          )
        }
        location_id <- DBI::dbGetQuery(
          con,
          sprintf(
            "INSERT INTO locations (%s) VALUES (%s) RETURNING location_id;",
            paste(insert_columns, collapse = ", "),
            paste0("$", seq_along(insert_columns), collapse = ", ")
          ),
          params = insert_params
        )[1, 1]

        if (has_public_geometries) {
          public_accuracy <- public_accuracy_m[i]
          if (is.na(public_accuracy)) {
            public_accuracy <- if (public_geom_type[i] == "exact_point") {
              0
            } else {
              max_offset_m[i]
            }
          }
          public_mask_method <- mask_method[i]
          if (is.na(public_mask_method) || trimws(public_mask_method) == "") {
            public_mask_method <- if (public_geom_type[i] == "exact_point") {
              "exact"
            } else {
              "stable_toroid"
            }
          }
          public_mask_seed <- mask_seed[i]
          if (is.na(public_mask_seed) || trimws(public_mask_seed) == "") {
            public_mask_seed <- location_code[i]
          }

          DBI::dbExecute(
            con,
            "INSERT INTO public.location_public_geometries (
               location_id,
               public_geom_type,
               public_geom,
               min_offset_m,
               max_offset_m,
               public_accuracy_m,
               mask_method,
               active
             )
             VALUES (
               $1,
               $2,
               CASE
                 WHEN $2 = 'exact_point' THEN
                   ST_SetSRID(
                     ST_MakePoint($3::double precision, $4::double precision),
                     4326
                   )
                 WHEN $2 = 'masked_point' THEN
                   public.location_masked_point(
                     $4::numeric,
                     $3::numeric,
                     $5::numeric,
                     $6::numeric,
                     $7::text
                   )
               END,
               $5,
               $6,
               $8,
               $9,
               TRUE
             );",
            params = list(
              location_id,
              public_geom_type[i],
              longitude[i],
              latitude[i],
              if (is.na(min_offset_m[i])) NA_real_ else min_offset_m[i],
              if (is.na(max_offset_m[i])) NA_real_ else max_offset_m[i],
              public_mask_seed,
              public_accuracy,
              public_mask_method
            )
          )
        }

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
          name[i],
          "."
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
}
