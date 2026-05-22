#' Load National Hydro Network (NHN) data into AquaCache
#' @description
#' This function loads the National Hydro Network (NHN) geospatial data into the AquaCache database. The NHN is a comprehensive dataset of Canada's surface water features, including rivers, lakes, and basins.
#'
#' @param con A connection to the database, created using the utility function [AquaConnect()] or [DBI::dbConnect()]. If left NULL, a connection will be attempted using AquaConnect() and its default parameters and closed afterwards.
#' @param targets A character vector specifying which NHN layers to load. Valid options include "basins", "flowpaths", "littoral", "banks", "watercourses", "waterbodies", and "islands". By default, all layers will be loaded. Note that the "flowpaths", "littoral", and "banks" layers are all in the same Geopackage file, so they will be downloaded together if any of them are included in the targets. The same applies to the "watercourses", "waterbodies", and "islands" layers, which are also in the same Geopackage file. Consider adding these together to minimize the number of large downloads.
#' @param clip Optional but highly recommended unless storage space is no concern. The top-level NHN regions to retain. Valid options range from '01' to '11' and correspond to the major hydrological regions of Canada. If NULL (default), all regions will be loaded. Note that this doesn't affect the download size, as the entire dataset will still be downloaded, but it will limit the data that is loaded into the database.
#' @param bbox Optional but also recommended, as the 'clip' parameter uses very broad regions; for example, '10' includes all waters draining to the Arctic ocean. Specify a bounding box as a numeric vector of the form c(xmin, xmax, ymin, ymax) in the coordinate reference system of the NHN data (EPSG:4617). Can be used with or without the `clip` parameter. If provided, only features that intersect with this bounding box will be loaded into the database. Note that this doesn't affect the download size as the entire dataset will still be downloaded, but it will limit the data that is loaded into the database.
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
    "flowpaths",
    "littoral",
    "banks",
    "watercourses",
    "waterbodies",
    "islands"
  ),
  clip = NULL,
  bbox = NULL,
  overwrite = FALSE,
  table = "vectors",
  schema = "spatial",
  geom_col = "geom"
) {
  # Validate the targets parameter
  valid_targets <- c(
    "basins",
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
      basins <- basins[terra::intersect(basins, bbox), ]
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

  # The flowpaths, littoral, and banks layers are all in the same gpkg, so we can download it once and then load the relevant layers based on the targets specified.
  if (any(c("flowpaths", "littoral", "banks") %in% targets)) {
    # Check that there is sufficient disk space to download and extract the file, approx 100 GB.
    free <- get_free_space_gb()
    if (free < 100) {
      stop(
        "Insufficient disk space to download and extract NHN flowpath data. Approximately 100 GB of free space is required. Current free space: ",
        round(free, 2),
        " GB."
      )
    }

    # Check that 'gpkg' is available
    rlang::check_installed(
      "gpkg",
      reason = "to extract layers from Geopackage files"
    )

    tempfile <- tempfile(fileext = ".gpkg")
    message(
      "Downloading NHN flowpath/littoral/banks. Approximately 27 GB to download, this **will** take a long time!"
    )
    download.file(
      "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_hnet.gpkg.zip",
      destfile = tempfile
    )
    message(
      "Download complete. Extracting data from the downloaded file. This may also take a while."
    )
    unzip(tempfile, exdir = tempdir())
    # Delete the tempfile after unzipping to free up space, as the unzipped file will be used from the temp directory.
    unlink(tempfile)

    unzipped_file <- list.files(
      tempdir(),
      pattern = "rhn_nhn_hnet.gpkg$",
      full.names = TRUE
    )
    message(
      "Extraction complete. Loading hydro junction data into AquaCache database (necessary for other layers). Please be patient, even if it looks like your R session is frozen."
    )

    gpkg <- gpkg::geopackage(unzipped_file)

    # Load the nhn_hnet_Hydro_Junction_0 layer first, as it is relevant to other layers.
    if (is.null(clip)) {
      junctions <- gpkg::gpkg_vect(
        gpkg,
        "nhn_hnet_Hydro_Junction_0",
        query = "SELECT geom, validity_date, dataset_name, nid, junction_type FROM nhn_hnet_Hydro_Junction_0"
      )
    } else {
      junctions <- gpkg::gpkg_vect(
        gpkg,
        "nhn_hnet_Hydro_Junction_0",
        query = paste0(
          "SELECT geom, validity_date, dataset_name, nid, junction_type FROM nhn_hnet_Hydro_Junction_0 WHERE substr(dataset_name, 1, 2) IN ('",
          paste(clip, collapse = "','"),
          "')"
        )
      )
    }

    # apply bbox filter if provided
    if (!is.null(bbox)) {
      junctions <- junctions[terra::intersect(junctions, bbox), ]
    }

    # Unique key in AquaCache is on layer_name, feature_name, geom_type. We'll create a new column.
    junctions$feature_name <- paste0(
      junctions$dataset_name,
      "_",
      junctions$nid
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
      if (is.null(clip)) {
        flows <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hnet_Network_Linear_Flow_1",
          query = "SELECT geom, validity_date, dataset_name, nid, from_junction, to_junction, flow_direction, level_priority FROM nhn_hnet_Network_Linear_Flow_1"
        )
      } else {
        # Use clip
        flows <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hnet_Network_Linear_Flow_1",
          query = paste0(
            "SELECT geom, validity_date, dataset_name, nid, from_junction, to_junction, flow_direction, level_priority FROM nhn_hnet_Network_Linear_Flow_1 WHERE substr(dataset_name, 1, 2) IN ('",
            paste(clip, collapse = "','"),
            "')"
          )
        )
      }

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        flows <- flows[terra::intersect(flows, bbox), ]
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
      if (is.null(clip)) {
        littoral <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hnet_Littoral_1",
          query = "SELECT geom, validity_date, dataset_name, nid, from_junction, to_junction FROM nhn_hnet_Littoral_1"
        )
      } else {
        littoral <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hnet_Littoral_1",
          query = paste0(
            "SELECT geom, validity_date, dataset_name, nid, from_junction, to_junction FROM nhn_hnet_Littoral_1 WHERE substr(dataset_name, 1, 2) IN ('",
            paste(clip, collapse = "','"),
            "')"
          )
        )
      }

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        littoral <- littoral[terra::intersect(littoral, bbox), ]
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
      if (is.null(clip)) {
        banks <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hnet_Bank_1",
          query = "SELECT geom, validity_date, dataset_name, nid, from_junction, to_junction, waterbody_nid FROM nhn_hnet_Bank_1"
        )
      } else {
        banks <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hnet_Bank_1",
          query = paste0(
            "SELECT geom, validity_date, dataset_name, nid, from_junction, to_junction, waterbody_nid FROM nhn_hnet_Bank_1 WHERE substr(dataset_name, 1, 2) IN ('",
            paste(clip, collapse = "','"),
            "')"
          )
        )
      }

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        banks <- banks[terra::intersect(banks, bbox), ]
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
    # Check that there is sufficient disk space to download and extract the file, approx 50 GB.
    free <- get_free_space_gb()
    if (free < 50) {
      stop(
        "Insufficient disk space to download and extract NHN water features data. Approximately 50 GB of free space is required. Current free space: ",
        round(free, 2),
        " GB."
      )
    }

    # Check that 'gpkg' is available
    rlang::check_installed(
      "gpkg",
      reason = "to extract layers from Geopackage files"
    )

    tempfile <- tempfile(fileext = ".gpkg")
    message(
      "Downloading NHN flowpath/littoral/banks. Approximately 27 GB to download, this **will** take a long time!"
    )
    download.file(
      "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/gpkg_en/CA/rhn_nhn_hhyd.gpkg.zip",
      destfile = tempfile
    )
    message(
      "Download complete. Extracting data from the downloaded file. This may also take a while."
    )
    unzip(tempfile, exdir = tempdir())
    # Delete the tempfile after unzipping to free up space, as the unzipped file will be used from the temp directory.
    unlink(tempfile)

    unzipped_file <- list.files(
      tempdir(),
      pattern = "rhn_nhn_hhyd.gpkg$",
      full.names = TRUE
    )
    message("Extraction complete. Loading data into AquaCache database.")

    gpkg <- gpkg::geopackage(unzipped_file)

    # contents <- gpkg::gpkg_contents(gpkg) # Lists the layers in the Geopackage
    # We're looking for 'nhn_hhyd_S_L_Watercourse_1', 'nhn_hhyd_Waterbody_2', and 'nhn_hhyd_Island_2'

    if ("watercourses" %in% targets) {
      message(
        "Loading watercourse data into AquaCache database. Please be patient, even if it looks like your R session is frozen."
      )
      if (is.null(clip)) {
        watercourses <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hhyd_S_L_Watercourse_1",
          query = "SELECT geom, validity_date, dataset_name, nid FROM nhn_hhyd_S_L_Watercourse_1"
        )
      } else {
        # Use clip
        watercourses <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hhyd_S_L_Watercourse_1",
          query = paste0(
            "SELECT geom, validity_date, dataset_name, nid FROM nhn_hhyd_S_L_Watercourse_1 WHERE substr(dataset_name, 1, 2) IN ('",
            paste(clip, collapse = "','"),
            "')"
          )
        )
      }

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        watercourses <- watercourses[terra::intersect(watercourses, bbox), ]
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
      if (is.null(clip)) {
        waterbodies <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hhyd_Waterbody_2",
          query = "SELECT geom, validity_date, dataset_name, nid FROM nhn_hhyd_Waterbody_2"
        )
      } else {
        waterbodies <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hhyd_Waterbody_2",
          query = paste0(
            "SELECT geom, validity_date, dataset_name, nid FROM nhn_hhyd_Waterbody_2 WHERE substr(dataset_name, 1, 2) IN ('",
            paste(clip, collapse = "','"),
            "')"
          )
        )
      }

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        waterbodies <- waterbodies[terra::intersect(waterbodies, bbox), ]
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
      if (is.null(clip)) {
        islands <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hhyd_Island_2",
          query = "SELECT geom, validity_date, dataset_name, nid FROM nhn_hhyd_Island_2"
        )
      } else {
        islands <- gpkg::gpkg_vect(
          gpkg,
          "nhn_hhyd_Island_2",
          query = paste0(
            "SELECT geom, validity_date, dataset_name, nid FROM nhn_hhyd_Island_2 WHERE substr(dataset_name, 1, 2) IN ('",
            paste(clip, collapse = "','"),
            "')"
          )
        )
      }

      # apply bbox filter if provided
      if (!is.null(bbox)) {
        islands <- islands[terra::intersect(islands, bbox), ]
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
