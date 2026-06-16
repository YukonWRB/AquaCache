#' Load National Hydro Network (NHN) data into AquaCache
#' @description
#' This function loads the National Hydro Network (NHN) geospatial data into the AquaCache database. The NHN is a comprehensive dataset of Canada's surface water features, including rivers, lakes, and basins.
#'
#' @param con A connection to the database, created using the utility function [AquaConnect()] or [DBI::dbConnect()]. If left NULL, a connection will be attempted using AquaConnect() and its default parameters and closed afterwards.
#' @param targets A character vector specifying which NHN layers to load. Valid options include "basins", "junctions", "flowpaths", "littoral", "banks", "watercourses", "waterbodies", and "islands". By default, all layers will be loaded. Note that the "junctions", "flowpaths", "littoral", and "banks" layers are all in the same Geopackage file, so they will be downloaded together if any of them are included in the targets. The same applies to the "watercourses", "waterbodies", and "islands" layers, which are also in the same Geopackage file. Consider adding these together to minimize the number of large downloads.
#' @param clip Optional but highly recommended unless storage space is no concern. The top-level NHN regions to retain. Valid options range from '01' to '11' and correspond to the major hydrological regions of Canada. If NULL (default), all regions will be loaded. Note that this doesn't affect the download size, as the entire dataset will still be downloaded, but it will limit the data that is loaded into the database.
#' @param bbox Optional but also recommended, as the 'clip' parameter uses very broad regions; for example, '10' includes all waters draining to the Arctic ocean. Specify a bounding box as a numeric vector of the form c(xmin, xmax, ymin, ymax) in the coordinate reference system of the NHN data (EPSG:4617). Can be used with or without the `clip` parameter. If provided, only features that intersect with this bounding box will be loaded into the database. Note that this doesn't affect the download size as the entire dataset will still be downloaded, but it will limit the data that is loaded into the database.
#' @param hnet_gpkg Optional path to an already-downloaded `rhn_nhn_hnet.gpkg` file. If supplied, the flowpath, littoral, bank, and hydro junction layers are read from this file instead of downloading the national archive again.
#' @param hhyd_gpkg Optional path to an already-downloaded `rhn_nhn_hhyd.gpkg` file. If supplied, the watercourse, waterbody, and island layers are read from this file instead of downloading the national archive again.
#' @param overwrite A logical value indicating whether to overwrite existing data in the database. Default is FALSE, which will not overwrite existing data and will fail if there is a conflict. If TRUE, existing data for the specified layers will be replaced.
#' @param table The name of the database table to which the data will be added. Default is "vectors" for a standard AquaCache setup.
#' @param schema The name of the database schema to which the data will be added. Default is "spatial" for a standard AquaCache setup.
#' @param geom_col The name of the geometry column in the database. Default is "geom" for a standard AquaCache setup.
#' @return A character vector of the names of the layers that were successfully loaded into the database.
#'
#' @export

load_nhn <- function(
  con = NULL,
  targets = c(
    "basins",
    "junctions",
    "flowpaths",
    "littoral",
    "banks",
    "watercourses",
    "waterbodies",
    "islands"
  ),
  clip = NULL,
  bbox = NULL,
  hnet_gpkg = NULL,
  hhyd_gpkg = NULL,
  overwrite = FALSE,
  table = "vectors",
  schema = "spatial",
  geom_col = "geom"
) {
  # Validate the targets parameter
  valid_targets <- c(
    "basins",
    "junctions",
    "flowpaths",
    "littoral",
    "banks",
    "watercourses",
    "waterbodies",
    "islands"
  )
  if (!all(targets %in% valid_targets)) {
    stop(
      "Invalid targets specified. Valid options are: ",
      paste(valid_targets, collapse = ", ")
    )
  }

  # Validate the clip parameter  valid_clips <- sprintf("%02d", 1:11)
  if (!is.null(clip)) {
    valid_clips <- sprintf("%02d", 1:11)
    if (!all(clip %in% valid_clips)) {
      stop(
        "Invalid clip specified. Valid options are: ",
        paste(valid_clips, collapse = ", ")
      )
    }
  }

  if (!is.null(bbox)) {
    # Make the spatExtent object now to validate the bbox.
    bbox <- terra::ext(bbox)

    bbox_filter <- function(x, bbox) {
      x[terra::is.related(x, bbox, "intersects"), ]
    }
  }

  nhn_layer_query <- function(layer, columns, clip) {
    query <- paste0(
      "SELECT ",
      paste(columns, collapse = ", "),
      " FROM ",
      layer
    )
    if (!is.null(clip)) {
      query <- paste0(
        query,
        " WHERE substr(dataset_name, 1, 2) IN ('",
        paste(clip, collapse = "','"),
        "')"
      )
    }
    query
  }

  read_nhn_layer <- function(gpkg_file, layer, columns, clip) {
    terra::vect(
      gpkg_file,
      layer = layer,
      query = nhn_layer_query(layer, columns, clip)
    )
  }

  get_nhn_gpkg <- function(local_file, pattern, url, free_gb, label) {
    if (!is.null(local_file)) {
      if (!file.exists(local_file)) {
        stop("The supplied ", label, " file does not exist: ", local_file)
      }
      return(local_file)
    }

    free <- get_free_space_gb()
    if (free < free_gb) {
      stop(
        "Insufficient disk space to download and extract NHN ",
        label,
        " data. Approximately ",
        free_gb,
        " GB of free space is required. Current free space: ",
        round(free, 2),
        " GB."
      )
    }

    tempfile <- tempfile(fileext = ".gpkg")
    message(
      "Downloading NHN ",
      label,
      ". This is a very large file and will take a long time."
    )
    curl::curl_download(url, destfile = tempfile, quiet = TRUE)
    message(
      "Download complete. Extracting data from the downloaded file. This may also take a while."
    )
    utils::unzip(tempfile, exdir = tempdir())
    unlink(tempfile)

    unzipped_file <- list.files(
      tempdir(),
      pattern = pattern,
      full.names = TRUE
    )
    if (!length(unzipped_file)) {
      stop("Could not find ", pattern, " after extracting the NHN archive.")
    }
    unzipped_file[[1]]
  }

  if (is.null(con)) {
    con <- AquaConnect()
    on.exit(DBI::dbDisconnect(con))
  }

  if ("basins" %in% targets) {
    message(
      "Downloading NHN basin polygons. Approximately 300 MB, this may take a few minutes."
    )
    # Fetch the file. terra::vect will auto-unzip. This is a different workflow than the others because there is only one layer in the gpkg.

    basins <- terra::vect(
      "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_decoupage.gpkg.zip"
    )
    message("Basin download complete. Loading data into AquaCache database.")

    # Use clip parameter to filter on the first two characters of 'dataset_name', which correspond to the top-level NHN region.
    if (!is.null(clip)) {
      basins <- basins[substr(basins$dataset_name, 1, 2) %in% clip, ]
    }

    # Apply bbox filter if provided
    if (!is.null(bbox)) {
      basins <- bbox_filter(basins, bbox)
    }

    basins <- basins[, c("validity_date", "dataset_name", "edition", "version")]

    basins$description <- paste0(
      "Edition: ",
      basins$edition,
      ", Version: ",
      basins$version
    )

    insertACVector(
      geom = basins,
      layer_name = "National Hydro Network - Basins",
      feature_name_col = "dataset_name",
      description_col = "description",
      con = con,
      overwrite = overwrite,
      ask = FALSE,
      table = table,
      schema = schema,
      geom_col = geom_col
    )
  }

  # The junction, flowpath, littoral, and bank layers are all in the same gpkg,
  # so we can download it once and then load the relevant layers.
  if (any(c("junctions", "flowpaths", "littoral", "banks") %in% targets)) {
    hnet_file <- get_nhn_gpkg(
      local_file = hnet_gpkg,
      pattern = "rhn_nhn_hnet.gpkg$",
      url = "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_hnet.gpkg.zip",
      free_gb = 100,
      label = "flowpath/littoral/banks"
    )
    message(
      "NHN hnet file found/downloaded."
    )

    # Load the nhn_hnet_Hydro_Junction_0 layer first, as it is relevant to other layers.
    junctions <- read_nhn_layer(
      hnet_file,
      "nhn_hnet_Hydro_Junction_0",
      c("geom", "validity_date", "dataset_name", "nid", "junction_type"),
      clip
    )
    message(
      "NHN hnet file loaded."
    )

    # apply bbox filter if provided
    if (!is.null(bbox)) {
      junctions <- bbox_filter(junctions, bbox)
    }

    # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
    junctions$feature_name <- paste0(
      junctions$dataset_name,
      "_",
      junctions$nid
    )

    message(
      "Loading hydro junction data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
    )
    # Load to AquaCache
    insertACVector(
      geom = junctions,
      layer_name = "National Hydro Network - Hydro Junctions",
      feature_name_col = "feature_name",
      description_col = NULL,
      description = "Hydro junctions from the National Hydro Network, used in the NHN dataset to represent confluences, divergences, and other changes in the flow network.",
      con = con,
      overwrite = overwrite,
      ask = FALSE,
      table = table,
      schema = schema,
      geom_col = geom_col
    )

    if ("flowpaths" %in% targets) {
      message(
        "Loading flowpath data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      flows <- read_nhn_layer(
        hnet_file,
        "nhn_hnet_Network_Linear_Flow_1",
        c(
          "geom",
          "validity_date",
          "dataset_name",
          "nid",
          "from_junction",
          "to_junction",
          "flow_direction",
          "level_priority"
        ),
        clip
      )

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        flows <- bbox_filter(flows, bbox)
      }

      # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
      flows$feature_name <- paste0(
        flows$dataset_name,
        "_",
        flows$nid
      )

      # Load to AquaCache
      insertACVector(
        geom = flows,
        layer_name = "National Hydro Network - Flowpaths",
        feature_name_col = "feature_name",
        description_col = NULL,
        description = "Water flowpaths from the National Hydro Network for use in basin delineation. Continuous through waterbodies.",
        con = con,
        overwrite = overwrite,
        ask = FALSE,
        table = table,
        schema = schema,
        geom_col = geom_col
      )
    }

    if ("littoral" %in% targets) {
      message(
        "Loading littoral (coastline) data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      littoral <- read_nhn_layer(
        hnet_file,
        "nhn_hnet_Littoral_1",
        c(
          "geom",
          "validity_date",
          "dataset_name",
          "nid",
          "from_junction",
          "to_junction"
        ),
        clip
      )

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        littoral <- bbox_filter(littoral, bbox)
      }

      # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
      littoral$feature_name <- paste0(
        littoral$dataset_name,
        "_",
        littoral$nid
      )

      # Load to AquaCache
      insertACVector(
        geom = littoral,
        layer_name = "National Hydro Network - Littoral",
        feature_name_col = "feature_name",
        description_col = NULL,
        description = "Littoral features (coastline) from the National Hydro Network",
        con = con,
        overwrite = overwrite,
        ask = FALSE,
        table = table,
        schema = schema,
        geom_col = geom_col
      )
    }

    if ("banks" %in% targets) {
      message(
        "Loading bank data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      banks <- read_nhn_layer(
        hnet_file,
        "nhn_hnet_Bank_1",
        c(
          "geom",
          "validity_date",
          "dataset_name",
          "nid",
          "from_junction",
          "to_junction",
          "waterbody_nid"
        ),
        clip
      )

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        banks <- bbox_filter(banks, bbox)
      }

      # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
      banks$feature_name <- paste0(
        banks$dataset_name,
        "_",
        banks$nid
      )

      # Load to AquaCache
      insertACVector(
        geom = banks,
        layer_name = "National Hydro Network - Banks",
        feature_name_col = "feature_name",
        description_col = NULL,
        description = "Stream/lake bank features from the National Hydro Network",
        con = con,
        overwrite = overwrite,
        ask = FALSE,
        table = table,
        schema = schema,
        geom_col = geom_col
      )
    }
  }

  # Watercourses, waterbodies, and islands are all in a gpkg also
  if (any(c("watercourses", "waterbodies", "islands") %in% targets)) {
    hhyd_file <- get_nhn_gpkg(
      local_file = hhyd_gpkg,
      pattern = "rhn_nhn_hhyd.gpkg$",
      url = "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_hhyd.gpkg.zip",
      free_gb = 50,
      label = "water features"
    )
    message("NHN hhyd file ready. Loading data into AquaCache database.")

    # contents <- gpkg::gpkg_contents(gpkg) # Lists the layers in the Geopackage
    # We're looking for 'nhn_hhyd_S_L_Watercourse_1', 'nhn_hhyd_Waterbody_2', and 'nhn_hhyd_Island_2'

    if ("watercourses" %in% targets) {
      message(
        "Loading watercourse data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      watercourses <- read_nhn_layer(
        hhyd_file,
        "nhn_hhyd_S_L_Watercourse_1",
        c("geom", "validity_date", "dataset_name", "nid"),
        clip
      )

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        watercourses <- bbox_filter(watercourses, bbox)
      }

      # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
      watercourses$feature_name <- paste0(
        watercourses$dataset_name,
        "_",
        watercourses$nid
      )

      # Load to AquaCache
      insertACVector(
        geom = watercourses,
        layer_name = "National Hydro Network - Watercourses",
        feature_name_col = "feature_name",
        description_col = NULL,
        description = "Water courses from the National Hydro Network. Not continuous through waterbodies.",
        con = con,
        overwrite = overwrite,
        ask = FALSE,
        table = table,
        schema = schema,
        geom_col = geom_col
      )
    }

    if ("waterbodies" %in% targets) {
      message(
        "Loading waterbody data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      waterbodies <- read_nhn_layer(
        hhyd_file,
        "nhn_hhyd_Waterbody_2",
        c("geom", "validity_date", "dataset_name", "nid"),
        clip
      )

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        waterbodies <- bbox_filter(waterbodies, bbox)
      }

      # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
      waterbodies$feature_name <- paste0(
        waterbodies$dataset_name,
        "_",
        waterbodies$nid
      )

      # Load to AquaCache
      insertACVector(
        geom = waterbodies,
        layer_name = "National Hydro Network - Waterbodies",
        feature_name_col = "feature_name",
        description_col = NULL,
        description = "Waterbodies from the National Hydro Network.",
        con = con,
        overwrite = overwrite,
        ask = FALSE,
        table = table,
        schema = schema,
        geom_col = geom_col
      )
    }

    if ("islands" %in% targets) {
      message(
        "Loading island data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      islands <- read_nhn_layer(
        hhyd_file,
        "nhn_hhyd_Island_2",
        c("geom", "validity_date", "dataset_name", "nid"),
        clip
      )

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        islands <- bbox_filter(islands, bbox)
      }

      # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
      islands$feature_name <- paste0(
        islands$dataset_name,
        "_",
        islands$nid
      )

      # Load to AquaCache
      insertACVector(
        geom = islands,
        layer_name = "National Hydro Network - Islands",
        feature_name_col = "feature_name",
        description_col = NULL,
        description = "Island features from the National Hydro Network.",
        con = con,
        overwrite = overwrite,
        ask = FALSE,
        table = table,
        schema = schema,
        geom_col = geom_col
      )
    }
  }

  return(targets)
}
